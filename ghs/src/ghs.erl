-module(ghs).
-include_lib("stdlib/include/assert.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    demo(5),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

-record(edge, {
    id :: pid(),
    weight :: non_neg_integer()
}).

-record(node, {
    id :: pid(),
    parent = none :: #edge{} | none,
    children = [] :: list(#edge{}),
    rejected = [] :: list(#edge{}),
    undecided = [] :: list(#edge{})
}).

-record(component, {
    core :: pid(),
    level = 0 :: non_neg_integer()
}).

-record(state, {
    phase :: searching | found,
    search_replies = 0 :: non_neg_integer(),
    candidate = {none, none} :: {pid(), #edge{}} | none
}).

demo(N) when is_integer(N), N > 0 ->
    % spawn N processes and store their IDs
    Graph_Nodes = [spawn(fun node_behaviour/0) || lists:seq(1, N)],

    % connect the nodes and
    % let them know their edges
    Graph = graph:random_kronecker_graph(Graph_Nodes),
    lists:foreach(
        fun(Graph_Node) ->
            Graph_Node ! {change_adjs, graph:get_adjs(Graph_Node, Graph)}
        end,
        Graph_Nodes
    ),

    % show the graph
    spawn(fun() -> graph:show_graph(Graph) end),

    % initiate wave, wait for response and print size of network component
    lists:foreach(
        % todo rename to start
        fun(Graph_Node) -> Graph_Node ! {'begin'} end,
        Graph_Nodes
    ),

    % TODO receive alg completion

    % kill the nodes
    lists:foreach(
        fun(Graph_Node) ->
            Graph_Node ! die
        end,
        Graph_Nodes
    ),
    ok.

node_behaviour() ->
    node_behaviour([]).
node_behaviour(Edges) ->
    receive
        {change_adjs, New_Edges} ->
            node_behaviour(
                lists:sort(
                    compare_edge, New_Edges
                )
            );
        die ->
            ok;
        {'begin'} ->
            % todo component
            node_behaviour(
                #node{id = self(), undecided = Edges}, #state{phase = undecided}, #component{
                    level = 0, core = self()
                }
            )
    end.
node_behaviour(
    Node,
    #state{phase = undecided} = State,
    Component
) ->
    search(Node, State, Component);
node_behaviour(
    Node,
    % #state{phase = Phase, search_replies = Search_Replies, candidate = Candidate} = State,
    State,
    Component
) ->
    receive
        {test, Id, {level = Level, core = Core}} when
            Component#component.level >= Level
        ->
            case Component#component.core of
                Core -> Id ! {reject};
                _ -> Id ! {accept}
            end;
        {accept} ->
            ?assertEqual(
                searching,
                State#state.phase,
                io_lib:format("accept received in ~w phase", [State#state.phase])
            ),
            Best_Undecided = hd(Node#node.undecided),
            report(
                Node,
                State#state{
                    search_replies = State#state.search_replies + 1,
                    candidate = min(
                        fun compare_edge/2, State#state.candidate, Best_Undecided
                    )
                },
                Component
            );
        {reject} ->
            ?assertEqual(
                searching,
                State#state.phase,
                io_lib:format("reject received in ~w phase", [State#state.phase])
            ),
            search(
                Node#node{
                    undecided = tl(Node#node.undecided),
                    rejected = hd(Node#node.undecided) ++ Node#node.rejected
                },
                State,
                Component
            );
        {report, Sender_Id, Candidate} ->
            ?assertEqual(
                searching,
                State#state.phase,
                io_lib:format("report received in ~w phase", [State#state.phase])
            ),
            report(
                Node,
                State#state{
                    search_replies = State#state.search_replies + 1,
                    candidate = min(
                        fun compare_edge/2, State#state.candidate, Candidate
                    )
                },
                Component
            );
        {update, Component, searching} ->
            search(
                Node,
                State#state{
                    phase = searching,
                    search_replies = 0,
                    candidate = none
                },
                Component
            )
    end.
% HELPERS

-define(Expected_search_replies(Node),
    (length(Node#node.children) + erlang:min(length(Node#node.undecided), 1))
).
search(#node{undecided = [Edge | _]} = Node, State, Component) ->
    Edge ! {test, Component},
    node_behaviour(
        Node,
        State,
        Component
    );
search(
    Node, {search_replies = Search_Replies} = State, Component
) when Search_Replies == ?Expected_search_replies(Node) ->
    report(Node, State, Component);
search(Node, State, Component) ->
    node_behaviour(
        Node,
        State,
        Component
    ).
report(#node{parent = none}, {candidate = none}, _) ->
    ok;
report(#node{parent = none} = Node, {candidate = Candidate} = _State, _Component) ->
    case lists:search(fun() -> true end, Node#node.undecided) of
        {value, Candidate} -> ok;
        false -> ok
    end;
report(
    #node{parent = {Parent_Id, _}} = Node, {search_replies = Search_Replies} = State, Component
) when
    Search_Replies == ?Expected_search_replies(Node)
->
    Parent_Id ! {report, Node#node.id, State#state.candidate},
    node_behaviour(
        Node,
        State#state{
            phase = found
        },
        Component
    );
report(Node, State, Component) ->
    node_behaviour(
        Node,
        State,
        Component
    ).

%% min(Fun, [A | Tail]) ->
lists:foldl(
    fun(E, Acc) ->
        min(Fun, E, Acc)
    end,
    A,
    Tail
).

min(Fun, A, B) ->
    case Fun(A, B) of
        true -> A;
        false -> B
    end.

compare_edge(_, none) ->
    true;
compare_edge(none, _) ->
    false;
compare_edge({_, A_Weight}, {_, B_Weight}) ->
    A_Weight =< B_Weight.
