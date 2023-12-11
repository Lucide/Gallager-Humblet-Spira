-module(ghs).

-behaviour(events).
-export([on_process_state/5, on_link_state/6, on_message/9]).

% -include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([main/1, start/0]).

%% MACROS
-define(LOG_FILENAME, "logs/log.txt").
-define(GRAPH_FILENAME, "json/graph.json").
-define(EVENTS_FILENAME, "json/events.json").
-define(LATENCY, 0).
-define(Expected_replies(Node),
    length(Node#node.children) + erlang:min(length(Node#node.undecided), 1)
).

% STRUCTS
-record(edge, {dst :: pid(), src :: pid(), weight :: non_neg_integer()}).
-record(node, {
    id :: pid(),
    parent = none :: #edge{} | none,
    children = [] :: [#edge{}],
    rejected = [] :: [#edge{}],
    undecided = [] :: [#edge{}],
    minimax_routing_table = #{} :: #{pid() => #edge{}}
}).
-record(component, {core :: pid(), level = 0 :: non_neg_integer()}).
-record(candidate, {source_id :: pid(), edge :: #edge{}}).
-record(state, {
    phase :: searching | found,
    replies = 0 :: non_neg_integer(),
    candidate = none :: #candidate{} | none,
    selected = false :: boolean(),
    supervisor :: pid(),
    tree = none :: map() | none
}).

%% escript entry point
main(Args) ->
    {ok, #{verbose := Verbose}, _, _} =
        argparse:parse(Args, #{
            arguments => [
                #{
                    name => verbose,
                    short => $v,
                    long => "verbose",
                    type => boolean,
                    default => false
                }
            ]
        }),
    logger:set_module_level(?MODULE, debug),
    Config = #{
        config => #{
            file => ?LOG_FILENAME,
            % prevent flushing (delete)
            flush_qlen => 100000,
            % disable drop mode
            drop_mode_qlen => 100000,
            % disable burst detection
            burst_limit_enable => false
        },
        level =>
            if
                Verbose -> debug;
                true -> notice
            end,
        modes => [write],
        formatter => {logger_formatter, #{template => [pid, " ", msg, "\n"]}}
    },
    logger:add_handler(to_file_handler, logger_std_h, Config),
    logger:set_handler_config(default, level, notice),

    start(),

    logger_std_h:filesync(to_file_handler).

%% algorithm entry point (can be executed from shell)
start() ->
    events:init(?MODULE),
    latency:init(),
    {_, EventsFile} = file:open(?EVENTS_FILENAME, [write, delayed_write]),
    register(events_file, EventsFile),
    ok = file:truncate(whereis(events_file)),

    Supervisor = self(),

    % load graph, spawn a process for each node, and stores node-pid bijections
    Graph = datagraph:load(?GRAPH_FILENAME),
    Nodes = datagraph:get_list_of_nodes(Graph),
    Node_To_Pid = lists:foldl(
        fun(V, Acc) ->
            Pid = spawn(fun() -> node_start(Supervisor) end),
            maps:put(V, Pid, Acc)
        end,
        maps:new(),
        Nodes
    ),
    Pid_To_Node = lists:foldl(
        fun(V, Acc) ->
            Pid = maps:get(V, Node_To_Pid),
            maps:put(Pid, V, Acc)
        end,
        maps:new(),
        Nodes
    ),

    % log the possible locations of events (locations are now process ids, converted from node ids)
    % locations are annotated with positions and weights
    lists:foreach(
        fun({V, [X, Y]}) ->
            save_node(maps:get(V, Node_To_Pid), X, Y)
        end,
        datagraph:get_list_of_datanodes(Graph)
    ),
    lists:foreach(
        fun({V1, V2, Weight}) ->
            save_link(maps:get(V1, Node_To_Pid), maps:get(V2, Node_To_Pid), Weight)
        end,
        datagraph:get_list_of_dataedges(Graph)
    ),

    % inform node processes about their neighbours, which then start executing the algorithm
    lists:foreach(
        fun(V1) ->
            Pid = maps:get(V1, Node_To_Pid),
            Adjs = lists:map(
                fun({V2, Weight}) ->
                    #edge{src = Pid, dst = maps:get(V2, Node_To_Pid), weight = Weight}
                end,
                datagraph:get_list_of_dataadjs(V1, Graph)
            ),
            Pid ! {change_adjs, Adjs}
        end,
        Nodes
    ),

    % collect and serialize trees
    Trees = supervise(length(maps:keys(Pid_To_Node))),
    JsonTrees = jsone:encode(
        lists:map(fun(Tree) -> datagraph:export(pid_tree_to_id_tree(Tree, Pid_To_Node)) end, Trees),
        [{float_format, [{decimals, 16}, compact]}, {indent, 2}]
    ),
    io:fwrite("~s~n", [JsonTrees]),

    latency:stop(),
    events:stop(),
    ok = file:write(events_file, "\n]"),
    ok = file:close(EventsFile).

%% RESERVED

supervise(N) ->
    supervise(N, []).

supervise(N, Trees) when N > 0 ->
    receive
        {done} ->
            supervise(N - 1, Trees);
        {Tree} ->
            supervise(N, [Tree | Trees])
    end;
supervise(0, Trees) ->
    Trees.

root_action(Node, #state{tree = none} = State, Component) ->
    broadcast(Node, State, Component);
root_action(_Node, State, _Component) ->
    State#state.supervisor ! {State#state.tree}.

node_end(Supervisor) ->
    Supervisor ! {done}.

node_start(Supervisor) ->
    receive
        {change_adjs, Adjs} ->
            SortedAdjs = lists:sort(fun compare_edge/2, Adjs)
    end,
    % events:process_state({core, 0}),
    search(
        #node{id = self(), undecided = SortedAdjs},
        #state{supervisor = Supervisor, phase = searching},
        #component{level = 0, core = self()}
    ).

node_loop(Node, State, Component) ->
    % update visualization (core node, parent link, selected children, rejected edges)
    Core = Component#component.core,
    case self() of
        Core -> events:process_state({core, Component#component.level});
        _ -> events:process_state({normal, 0})
    end,
    ParentLink = Node#node.parent,
    case ParentLink of
        none ->
            ok;
        _ ->
            events:link_state(ParentLink#edge.src, ParentLink#edge.dst, accepted)
    end,
    lists:foreach(
        fun(Link) ->
            case Link of
                ParentLink -> ok;
                _ -> events:link_state(Link#edge.src, Link#edge.dst, deleted)
            end
        end,
        Node#node.children
    ),
    lists:foreach(
        fun(Link) ->
            case Link of
                ParentLink -> ok;
                _ -> events:link_state(Link#edge.src, Link#edge.dst, rejected)
            end
        end,
        Node#node.rejected
    ),
    events:tick(),

    % message consumption
    receive
        {{test, Source_Id, Source_Component}, _} = Msg when
            Component#component.level >= Source_Component#component.level
        ->
            events:received_annotated_msg(Msg),
            test(Node, State, Component, Source_Id, Source_Component);
        {accept, _} = Msg ->
            events:received_annotated_msg(Msg),
            Candidate = #candidate{source_id = Node#node.id, edge = hd(Node#node.undecided)},
            report(
                Node,
                State#state{
                    replies = State#state.replies + 1,
                    candidate =
                        min(fun compare_candidate/2, State#state.candidate, Candidate)
                },
                Component
            );
        {reject, _} = Msg ->
            events:received_annotated_msg(Msg),
            search(
                Node#node{
                    undecided = tl(Node#node.undecided),
                    rejected = [hd(Node#node.undecided) | Node#node.rejected]
                },
                State,
                Component
            );
        {{report, Candidate}, _} = Msg ->
            events:received_annotated_msg(Msg),
            report(
                Node,
                State#state{
                    replies = State#state.replies + 1,
                    candidate =
                        min(fun compare_candidate/2, State#state.candidate, Candidate)
                },
                Component
            );
        {notify, _} = Msg ->
            events:received_annotated_msg(Msg),
            notify(Node, State, Component);
        {{merge, Source_Id, Source_Level}, _} = Msg when Component#component.level > Source_Level ->
            events:received_annotated_msg(Msg),
            merge(Node, State, Component, Source_Id, Source_Level);
        {{merge, Source_Id, Source_Level}, _} = Msg when
            Component#component.level == Source_Level andalso
                State#state.selected andalso
                State#state.candidate#candidate.edge#edge.dst == Source_Id
        ->
            events:received_annotated_msg(Msg),
            merge(Node, State, Component, Source_Id, Source_Level);
        {{update, New_Component, Phase}, _} = Msg ->
            events:received_annotated_msg(Msg),
            update(Node, State#state{phase = Phase}, New_Component);
        {broadcast, _} = Msg ->
            events:received_annotated_msg(Msg),
            broadcast(Node, State, Component);
        {{convergecast, Tree, Minimax_Routing_Table}, _} = Msg ->
            events:received_annotated_msg(Msg),
            Routing_Table = maps:merge(
                Node#node.minimax_routing_table, Minimax_Routing_Table
            ),
            New_Tree = maps:merge(State#state.tree, Tree),
            convergecast(
                Node#node{
                    minimax_routing_table = Routing_Table
                },
                State#state{
                    replies = State#state.replies + 1,
                    tree = New_Tree
                },
                Component
            );
        {{route, Dst, Content}, _} = Msg when Dst == Node#node.id ->
            events:received_annotated_msg(Msg),
            io:fwrite(standard_io, "got long distance ~p", [Content]),
            node_loop(Node, State, Component);
        {{route, Dst, _}, _} = Msg ->
            events:received_annotated_msg(Msg),
            Next_Hop_Edge = maps:get(Dst, Node#node.minimax_routing_table, Node#node.parent),
            send_tick(Next_Hop_Edge#edge.dst, Msg),
            node_loop(Node, State, Component)
    end.

test(Node, State, Component, Source_Id, #component{core = Source_Core}) ->
    case Component#component.core of
        Source_Core ->
            send_tick(Source_Id, reject);
        _ ->
            send_tick(Source_Id, accept)
    end,
    node_loop(Node, State, Component).

search(#node{undecided = [Edge | _]} = Node, State, Component) ->
    send_tick(Edge#edge.dst, {test, Node#node.id, Component}),
    node_loop(Node, State, Component);
search(Node, #state{replies = Replies} = State, Component) when
    Replies == ?Expected_replies(Node)
->
    report(Node, State, Component);
search(Node, State, Component) ->
    node_loop(Node, State, Component).

report(#node{parent = none} = Node, #state{candidate = none} = State, Component) when
    State#state.replies == ?Expected_replies(Node)
->
    root_action(Node, State, Component);
report(#node{parent = none} = Node, State, Component) when
    State#state.replies == ?Expected_replies(Node)
->
    notify(Node, State#state{phase = found}, Component);
report(Node, State, Component) when State#state.replies == ?Expected_replies(Node) ->
    Reported_Candidate =
        case State#state.candidate of
            none ->
                none;
            Candidate ->
                Candidate#candidate{source_id = Node#node.id}
        end,
    send_tick(Node#node.parent#edge.dst, {report, Reported_Candidate}),
    node_loop(Node, State#state{phase = found}, Component);
report(Node, State, Component) ->
    node_loop(Node, State, Component).

notify(Node, #state{candidate = Candidate} = State, Component) when
    Candidate#candidate.source_id == Node#node.id
->
    send_tick(Candidate#candidate.edge#edge.dst, {merge, Node#node.id, Component#component.level}),
    % modified to avoid testing again the core edge
    node_loop(
        Node#node{undecided = tl(Node#node.undecided)}, State#state{selected = true}, Component
    );
notify(Node, #state{candidate = Candidate} = State, Component) ->
    {[Source_Edge], Children} =
        lists:partition(
            fun(Edge) -> Edge#edge.dst == Candidate#candidate.source_id end,
            Node#node.children
        ),
    send_tick(Source_Edge#edge.dst, notify),
    node_loop(
        Node#node{
            parent = Source_Edge,
            children = list(Node#node.parent) ++ Children
        },
        State,
        Component
    ).

merge(Node, State, Component, Source_Id, Source_Level) when
    Component#component.level > Source_Level
->
    % Source_Edge is not a candidate when Component.level > Source_Level
    {value, Source_Edge} =
        lists:search(fun(Edge) -> Edge#edge.dst == Source_Id end, Node#node.undecided),
    send_tick(Source_Id, {update, Component, State#state.phase}),
    node_loop(
        Node#node{children = [Source_Edge | Node#node.children]},
        State,
        Component
    );
merge(Node, State, Component, Source_Id, _Source_Level) ->
    New_Component =
        Component#component{
            core = max(Node#node.id, Source_Id),
            level = Component#component.level + 1
        },
    send_tick(Source_Id, {update, New_Component, searching}),
    node_loop(Node, State, Component).

update(Node, #state{selected = false} = State, Component) ->
    lists:map(
        fun(#edge{dst = Edge_Dst}) ->
            send_tick(Edge_Dst, {update, Component, State#state.phase})
        end,
        Node#node.children
    ),
    New_State =
        State#state{
            replies = 0,
            candidate = none,
            selected = false
        },
    case New_State#state.phase of
        searching ->
            search(Node, New_State, Component);
        _ ->
            node_loop(Node, New_State, Component)
    end;
update(Node, #state{candidate = Candidate} = State, Component) ->
    New_Children = list(Node#node.parent) ++ Node#node.children,
    lists:map(
        fun(#edge{dst = Edge_Dst}) ->
            send_tick(Edge_Dst, {update, Component, State#state.phase})
        end,
        New_Children
    ),
    New_Node =
        if
            Component#component.core == Node#node.id ->
                Node#node{
                    parent = none,
                    children = [Candidate#candidate.edge | New_Children]
                };
            true ->
                Node#node{
                    parent = Candidate#candidate.edge,
                    children = New_Children
                }
        end,
    New_State =
        State#state{
            replies = 0,
            candidate = none,
            selected = false
        },
    case New_State#state.phase of
        searching ->
            search(New_Node, New_State, Component);
        _ ->
            node_loop(New_Node, New_State, Component)
    end.

broadcast(#node{children = []} = Node, State, Component) ->
    convergecast(
        Node,
        State#state{
            replies = 0,
            tree = datagraph:add_node(Node#node.id, datagraph:new())
        },
        Component
    );
broadcast(Node, State, Component) ->
    lists:foreach(
        fun(#edge{dst = Edge_Dst}) ->
            send_tick(Edge_Dst, broadcast)
        end,
        Node#node.children
    ),
    node_loop(
        Node,
        State#state{
            replies = 0,
            tree = datagraph:add_node(Node#node.id, datagraph:new())
        },
        Component
    ).

% compute MST weight
% calculate minimax routing tables
convergecast(#node{parent = none} = Node, State, Component) when
    State#state.replies == ?Expected_replies(Node)
->
    New_State = State,
    root_action(Node, New_State, Component),
    node_end(State#state.supervisor),
    node_loop(Node, New_State, Component);
convergecast(Node, State, Component) when State#state.replies == ?Expected_replies(Node) ->
    Tree = lists:foldl(
        fun(Child, Tree) ->
            datagraph:add_directed_edge(Child#edge.dst, Child#edge.src, Child#edge.weight, Tree)
        end,
        State#state.tree,
        Node#node.children
    ),
    New_State = State#state{tree = Tree},
    Parent_Tree = datagraph:add_directed_edge(
        Node#node.parent#edge.src,
        Node#node.parent#edge.dst,
        Node#node.parent#edge.weight,
        datagraph:add_node(Node#node.parent#edge.dst, Tree)
    ),
    Rev_Parent_Edge = reverse_edge(Node#node.parent),
    Parent_Routing_Table = maps:merge(
        maps:from_keys(maps:keys(Node#node.minimax_routing_table), Rev_Parent_Edge),
        #{Node#node.id => Rev_Parent_Edge}
    ),
    send_tick(Node#node.parent#edge.dst, {convergecast, Parent_Tree, Parent_Routing_Table}),
    node_end(State#state.supervisor),
    node_loop(Node, New_State, Component);
convergecast(Node, State, Component) ->
    node_loop(Node, State, Component).

% UTILS

reverse_edge(#edge{dst = Dst, src = Src} = Edge) ->
    Edge#edge{dst = Src, src = Dst}.

list(none) ->
    [];
list(A) ->
    [A].

min(Fun, A, B) ->
    case Fun(A, B) of
        true ->
            A;
        false ->
            B
    end.

compare_edge(_, none) ->
    true;
compare_edge(none, _) ->
    false;
compare_edge(A_Edge, B_Edge) ->
    {
        A_Edge#edge.weight,
        min(A_Edge#edge.src, A_Edge#edge.dst),
        max(A_Edge#edge.src, A_Edge#edge.dst)
    } =<
        {
            B_Edge#edge.weight,
            min(B_Edge#edge.src, B_Edge#edge.dst),
            max(B_Edge#edge.src, B_Edge#edge.dst)
        }.

compare_candidate(_, none) ->
    true;
compare_candidate(none, _) ->
    false;
compare_candidate(#candidate{edge = A_Edge}, #candidate{edge = B_Edge}) ->
    compare_edge(A_Edge, B_Edge).

pid_tree_to_id_tree(Tree, Pid_To_Node) ->
    Id_Tree_Only_Nodes = lists:foldl(
        fun({V, Data}, Id_Tree) -> datagraph:add_node(maps:get(V, Pid_To_Node), Data, Id_Tree) end,
        datagraph:new(),
        datagraph:get_list_of_datanodes(Tree)
    ),
    lists:foldl(
        fun({V1, V2, EdgeData}, Id_Tree) ->
            datagraph:add_directed_edge(
                maps:get(V1, Pid_To_Node), maps:get(V2, Pid_To_Node), EdgeData, Id_Tree
            )
        end,
        Id_Tree_Only_Nodes,
        datagraph:get_list_of_dataedges(Tree)
    ).
% modification of erlang:send function
send(To, Msg) ->
    LatencySendFun = fun(To2, Msg2) -> latency:send(To2, Msg2, ?LATENCY) end,
    events:send(To, Msg, LatencySendFun).

send_tick(To, Msg) ->
    events:tick(),
    send(To, Msg).

% function for logging events
save_event(Event) when is_map(Event) ->
    JsonString = binary_to_list(jsone:encode(Event)),
    {ok, Position} = file:position(whereis(events_file), cur),
    String =
        case Position of
            0 -> "[\n" ++ JsonString;
            _ -> ",\n" ++ JsonString
        end,
    ok = file:write(events_file, String).

% sanitize pids and timestamps as json strings
pid_to_json(Pid) ->
    erlang:list_to_binary(erlang:pid_to_list(Pid)).

timestamp_to_json(Time) ->
    L = maps:to_list(Time),
    L2 = lists:map(fun({Key, Value}) -> {pid_to_json(Key), Value} end, L),
    maps:from_list(L2).

% node/link initialization
save_node(At, X, Y) when is_pid(At) andalso is_number(X) andalso is_number(Y) ->
    Event = #{at => At, x => X, y => Y},
    save_event(Event).

save_link(From, To, Weight) when is_pid(From) andalso is_pid(To) andalso is_number(Weight) ->
    Event = #{from => From, to => To, weight => Weight},
    save_event(Event).

% callback functions for process state, link state, and messages
on_process_state(Pid, OldState, NewState, _Time, VClock) ->
    case NewState of
        OldState ->
            ok;
        {Tag, Level} ->
            % Event = #{tag => Tag, at => Pid, level => Level, from_time => Time, to_time => Time + ?LATENCY},
            Event = #{tag => Tag, at => Pid, level => Level, time => timestamp_to_json(VClock)},
            save_event(Event)
    end.

on_link_state(From, To, OldState, NewState, _Time, VClock) ->
    case NewState of
        OldState ->
            ok;
        Tag ->
            % Event = #{tag => Tag, from => From, to => To, from_time => Time, to_time => Time + ?LATENCY},
            Event = #{tag => Tag, from => From, to => To, time => timestamp_to_json(VClock)},
            save_event(Event)
    end.

on_message(From, To, Msg, _FromState, _ToState, _FromTime, _ToTime, FromVClock, ToVClock) ->
    if
        is_tuple(Msg) ->
            Tag = element(1, Msg);
        is_atom(Msg) ->
            Tag = Msg
    end,
    % Event = #{tag => Tag, from => From, to => To, from_time => FromTime, to_time => ToTime},
    Event = #{
        tag => Tag,
        from => From,
        to => To,
        from_time => timestamp_to_json(FromVClock),
        to_time => timestamp_to_json(ToVClock)
    },
    save_event(Event).
