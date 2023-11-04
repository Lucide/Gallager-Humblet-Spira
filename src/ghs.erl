-module(ghs).

-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/logger.hrl").

%% API exports
-export([main/1]).

-define(Log_path, "logs/log.txt").
-define(Expected_replies(Node),
    length(Node#node.children) + erlang:min(length(Node#node.undecided), 1)
).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, #{n := N, verbose := Verbose}, _, _} =
        argparse:parse(Args, #{
            arguments => [
                #{name => n, type => integer},
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
        config => #{file => ?Log_path},
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
    ?LOG_DEBUG("N: ~p", [N]),
    demo(N),
    logger_std_h:filesync(to_file_handler),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

-record(edge, {dst :: pid(), src :: pid(), weight :: non_neg_integer()}).
-record(node, {
    id :: pid(),
    parent = none :: #edge{} | none,
    children = [] :: [#edge{}],
    rejected = [] :: [#edge{}],
    undecided = [] :: [#edge{}]
}).
-record(component, {core :: pid(), level = 0 :: non_neg_integer()}).
-record(candidate, {source_id :: pid(), edge :: #edge{}}).
-record(state, {
    phase :: searching | found,
    replies = 0 :: non_neg_integer(),
    candidate = none :: #candidate{} | none,
    selected = false :: boolean(),
    supervisor :: pid(),
    representative = none :: nonempty_string() | none,
    sum = 0 :: non_neg_integer()
}).

demo(N) when N > 0 ->
    Supervisor = self(),
    Graph_Nodes = [spawn(fun() -> node_behaviour(Supervisor) end) || _ <- lists:seq(1, N)],

    Graph = graph:random_kronecker_graph(Graph_Nodes),
    lists:foreach(
        fun(Graph_Node) ->
            Graph_Node !
                {change_adjs, [
                    #edge{
                        src = Graph_Node,
                        dst = Dst,
                        weight = Weight
                    }
                 || {Dst, Weight} <- graph:get_adjs(Graph_Node, Graph)
                ]}
        end,
        Graph_Nodes
    ),

    lists:foreach(fun(Graph_Node) -> Graph_Node ! {start} end, Graph_Nodes),
    supervisor(Graph).

supervisor(Graph) ->
    supervisor(Graph, length(graph:get_nodes(Graph)), []).
supervisor(Graph, 0, Components) ->
    supervisor_action(Graph, Components),
    lists:map(fun(Id) -> Id ! {die} end, graph:get_nodes(Graph));
supervisor(Graph, N, Components) ->
    receive
        {component, Component} ->
            supervisor(Graph, N, [Component | Components]);
        {done} ->
            ?LOG_DEBUG("N=~p", [N - 1]),
            supervisor(Graph, N - 1, Components)
    end.

supervisor_action(Graph, Components) ->
    String = lists:foldl(
        fun({Representative, Sum}, Acc) ->
            case Acc of
                "" -> Sep = "";
                _ -> Sep = ", "
            end,
            lists:flatten([
                Acc,
                io_lib:format("~s[\"~s\", ~w]", [Sep, Representative, Sum])
            ])
        end,
        "",
        Components
    ),
    graph:fwrite_partial_graph(standard_io, Graph),
    io:fwrite(standard_io, "\"components\": [~s]}", [String]).
root_action(Node, #state{representative = none} = State, Component) ->
    broadcast(Node, State, Component);
root_action(_Node, State, _Component) ->
    State#state.supervisor ! {component, {State#state.representative, State#state.sum}}.
done_action(Supervisor) ->
    ?LOG_DEBUG("done to ~w", [Supervisor]),
    Supervisor ! {done}.

node_behaviour(Supervisor) ->
    node_behaviour(Supervisor, []).

node_behaviour(Supervisor, Edges) ->
    receive
        {change_adjs, New_Edges} ->
            node_behaviour(Supervisor, lists:sort(fun compare_edge/2, New_Edges));
        {die} ->
            ok;
        {start} ->
            search(
                #node{id = self(), undecided = Edges},
                #state{supervisor = Supervisor, phase = searching},
                #component{level = 0, core = self()}
            )
    end.

node_behaviour(Node, State, Component) ->
    % ?LOG_DEBUG(
    %     "-~w~n ~w exp_replies=~w~n ~w",
    %     [Node, State, ?Expected_replies(Node), Component]
    % ),
    receive
        {test, Source_Id, Source_Component} when
            Component#component.level >= Source_Component#component.level
        ->
            ?LOG_DEBUG("test from ~w, ~w", [Source_Id, Source_Component]),
            test(Node, State, Component, Source_Id, Source_Component);
        {accept} ->
            ?LOG_DEBUG("got accept", []),
            ?assertEqual(
                searching,
                State#state.phase,
                io_lib:format("accept received in ~p phase", [State#state.phase])
            ),
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
        {reject} ->
            ?LOG_DEBUG("got reject", []),
            ?assertEqual(
                searching,
                State#state.phase,
                lists:flatten(
                    io_lib:format("reject received in ~p phase", [State#state.phase])
                )
            ),
            ?assert(length(Node#node.undecided) > 0, io_lib:format("reject without undecided", [])),
            search(
                Node#node{
                    undecided = tl(Node#node.undecided),
                    rejected = [hd(Node#node.undecided) | Node#node.rejected]
                },
                State,
                Component
            );
        {report, Candidate} ->
            ?LOG_DEBUG("got report, ~w", [Candidate]),
            ?assertEqual(
                searching,
                State#state.phase,
                io_lib:format("report received in ~p phase", [State#state.phase])
            ),
            report(
                Node,
                State#state{
                    replies = State#state.replies + 1,
                    candidate =
                        min(fun compare_candidate/2, State#state.candidate, Candidate)
                },
                Component
            );
        {notify} ->
            ?LOG_DEBUG("got notify", []),
            ?assertEqual(
                found,
                State#state.phase,
                io_lib:format("notify received in ~p phase", [State#state.phase])
            ),
            notify(Node, State, Component);
        {merge, Source_Id, Source_Level} when Component#component.level > Source_Level ->
            ?LOG_DEBUG("merge (quick) from ~w, ~w", [Source_Id, Source_Level]),
            merge(Node, State, Component, Source_Id, Source_Level);
        {merge, Source_Id, Source_Level} when
            Component#component.level == Source_Level andalso
                State#state.selected andalso
                State#state.candidate#candidate.edge#edge.dst ==
                    Source_Id
        ->
            ?LOG_DEBUG("merge from ~w, ~w", [Source_Id, Source_Level]),
            ?assertEqual(
                found,
                State#state.phase,
                io_lib:format("merge (slow) received in ~p phase", [State#state.phase])
            ),
            merge(Node, State, Component, Source_Id, Source_Level);
        {update, New_Component, Phase} ->
            ?LOG_DEBUG("got update, ~w ~w", [New_Component, Phase]),
            ?assertEqual(
                found,
                State#state.phase,
                io_lib:format("update received in ~p phase", [State#state.phase])
            ),
            update(Node, State#state{phase = Phase}, New_Component);
        {broadcast} ->
            broadcast(Node, State, Component);
        {sum, Source_Representative, Source_Sum} ->
            sum(
                Node,
                State#state{
                    replies = State#state.replies + 1,
                    representative = max(State#state.representative, Source_Representative),
                    sum = State#state.sum + Source_Sum
                },
                Component
            );
        {die} ->
            ?LOG_DEBUG("dies", []),
            ok
    end.

test(Node, State, Component, Source_Id, #component{core = Source_Core}) ->
    case Component#component.core of
        Source_Core ->
            ?LOG_DEBUG("reject to ~w", [Source_Id]),
            Source_Id ! {reject};
        _ ->
            ?LOG_DEBUG("accept to ~w", [Source_Id]),
            Source_Id ! {accept}
    end,
    node_behaviour(Node, State, Component).

search(#node{undecided = [Edge | _]} = Node, State, Component) ->
    ?LOG_DEBUG("test to ~w, ~w", [Edge#edge.dst, Component]),
    Edge#edge.dst ! {test, Node#node.id, Component},
    node_behaviour(Node, State, Component);
search(Node, #state{replies = Replies} = State, Component) when
    Replies == ?Expected_replies(Node)
->
    report(Node, State, Component);
search(Node, State, Component) ->
    node_behaviour(Node, State, Component).

report(#node{parent = none} = Node, #state{candidate = none} = State, Component) when
    State#state.replies == ?Expected_replies(Node)
->
    root_action(Node, State, Component);
report(#node{parent = none} = Node, State, Component) when
    State#state.replies == ?Expected_replies(Node)
->
    notify(Node, State#state{phase = found}, Component);
report(Node, State, Component) when
    State#state.replies == ?Expected_replies(Node)
->
    Reported_Candidate =
        case State#state.candidate of
            none ->
                none;
            Candidate ->
                Candidate#candidate{source_id = Node#node.id}
        end,
    ?LOG_DEBUG(
        "report to ~w, ~w",
        [Node#node.parent#edge.dst, Reported_Candidate]
    ),
    Node#node.parent#edge.dst ! {report, Reported_Candidate},
    node_behaviour(Node, State#state{phase = found}, Component);
report(Node, State, Component) ->
    node_behaviour(Node, State, Component).

notify(Node, #state{candidate = Candidate} = State, Component) when
    Candidate#candidate.source_id == Node#node.id
->
    ?LOG_DEBUG(
        "merge to ~w, ~w",
        [Candidate#candidate.edge#edge.dst, Component#component.level]
    ),
    Candidate#candidate.edge#edge.dst ! {merge, Node#node.id, Component#component.level},
    node_behaviour(Node, State#state{selected = true}, Component);
notify(Node, #state{candidate = Candidate} = State, Component) ->
    {[Source_Edge], Children} =
        lists:partition(
            fun(Edge) -> Edge#edge.dst == Candidate#candidate.source_id end,
            Node#node.children
        ),
    ?LOG_DEBUG("notify to ~w", [Source_Edge#edge.dst]),
    Source_Edge#edge.dst ! {notify},
    node_behaviour(
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
    ?LOG_DEBUG("update to ~w, ~w ~w", [Source_Id, State#state.phase, Component]),
    Source_Id ! {update, Component, State#state.phase},
    node_behaviour(
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
    ?LOG_DEBUG("update to ~w, ~w ~w", [Source_Id, searching, New_Component]),
    Source_Id ! {update, New_Component, searching},
    node_behaviour(Node, State, Component).

update(Node, #state{selected = false} = State, Component) ->
    lists:map(
        fun(#edge{dst = Edge_Dst}) ->
            ?LOG_DEBUG(
                "update to ~w, ~w ~w",
                [Edge_Dst, State#state.phase, Component]
            ),
            Edge_Dst ! {update, Component, State#state.phase}
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
            node_behaviour(Node, New_State, Component)
    end;
update(Node, #state{candidate = Candidate} = State, Component) ->
    New_Children = list(Node#node.parent) ++ Node#node.children,
    lists:map(
        fun(#edge{dst = Edge_Dst}) ->
            ?LOG_DEBUG(
                "update to ~w, ~w ~w",
                [Edge_Dst, State#state.phase, Component]
            ),
            Edge_Dst ! {update, Component, State#state.phase}
        end,
        New_Children
    ),
    New_Node =
        if
            Component#component.core == Node#node.id ->
                Node#node{parent = none, children = [Candidate#candidate.edge | New_Children]};
            true ->
                Node#node{parent = Candidate#candidate.edge, children = New_Children}
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
            node_behaviour(New_Node, New_State, Component)
    end.

broadcast(#node{children = []} = Node, State, Component) ->
    sum(
        Node,
        State#state{replies = 0, representative = pid_to_list(Node#node.id)},
        Component
    );
broadcast(Node, State, Component) ->
    lists:foreach(fun(#edge{dst = Edge_Dst}) -> Edge_Dst ! {broadcast} end, Node#node.children),
    node_behaviour(
        Node, State#state{replies = 0, representative = pid_to_list(Node#node.id)}, Component
    ).

sum(#node{parent = none} = Node, State, Component) when
    State#state.replies == ?Expected_replies(Node)
->
    root_action(Node, State, Component),
    done_action(State#state.supervisor),
    node_behaviour(Node, State, Component);
sum(Node, State, Component) when State#state.replies == ?Expected_replies(Node) ->
    Node#node.parent#edge.dst !
        {sum, State#state.representative, State#state.sum + Node#node.parent#edge.weight},
    done_action(State#state.supervisor),
    node_behaviour(Node, State, Component);
sum(Node, State, Component) ->
    node_behaviour(Node, State, Component).

% UTILS

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
