-module(graph).
-export([
    new/0,
    add_node/2,
    add_nodes/2,
    add_directed_edge/4,
    add_edge/4,
    add_directed_edges/4,
    get_nodes/1,
    get_adjs/2,
    get_edges/1,
    get_adj_matrix/1,
    remove_node/2,
    random_kronecker_graph/1, random_kronecker_graph/2,
    json_format/2,
    show_graph/1, show_graph/2
]).

% EXAMPLES OF USE:
% todo
% c(graph)                   			 compile (function show_graph requires python3 with json, networkx, and matplotlib modules)
%
% graph:new()                			 return the empty graph
% graph:add_node(V, G)      			 add node V to graph G
% graph:add_nodes(L, G)      		     add all nodes of list L to graph G
%
% graph:add_directed_edges(L1, L2, G)    add directed edges from all nodes of L1 to all nodes of L2
% graph:add_edges(L1, L2, G)			 add undirected edges between all nodes of L1 and all nodes of L2
%
% graph:get_edges(G)                     return all directed edges (pairs of nodes) in a graph (undirected edges are listed in both directions)
% graph:get_adjs(V, G)                   get list of vertices adjacent to a given one
%
% graph:bipartite(L1, L2)                construct a bipartite graph with two lists of nodes
% graph:clique(L)                        construct a clique from a list of nodes
% graph:random_avg_degree(L, D)          construct a random undirected graph from a list of nodes, with average degree D

new() ->
    dict:new().

add_node(V, G) ->
    dict:store(V, [], G).

add_nodes(L, G) ->
    lists:foldl(
        fun(V, Acc) ->
            add_node(V, Acc)
        end,
        G,
        L
    ).

add_directed_edge(V1, V2, Weight, G) ->
    dict:append(V1, {V2, Weight}, G).

add_edge(V1, V2, Weight, G) ->
    if
        V1 == V2 ->
            add_directed_edge(V1, V2, Weight, G);
        true ->
            add_directed_edge(V1, V2, Weight, add_directed_edge(V2, V1, Weight, G))
    end.

add_directed_edges(L1, L2, Weight, G) ->
    lists:foldl(
        fun(V1, Acc1) ->
            lists:foldl(
                fun(V2, Acc2) ->
                    add_directed_edge(V1, V2, Weight, Acc2)
                end,
                Acc1,
                L2
            )
        end,
        G,
        L1
    ).

get_nodes(G) ->
    dict:fetch_keys(G).

get_adjs(V, G) ->
    dict:fetch(V, G).

get_edges(G) ->
    Nodes = get_nodes(G),
    GroupedEdges = lists:map(
        fun(V1) ->
            Adjs = get_adjs(V1, G),
            lists:map(fun({V2, Weight}) -> {V1, V2, Weight} end, Adjs)
        end,
        Nodes
    ),
    lists:flatten(GroupedEdges).

get_adj_matrix(G) ->
    Nodes = get_nodes(G),
    [
        [
            case maps:find(R, G) of
                {ok, {C, Weight}} -> Weight;
                _ -> 0
            end
         || C <- Nodes
        ]
     || R <- Nodes
    ].

remove_node(V, G) ->
    dict:erase(V, G).

random_kronecker_graph(L) -> random_kronecker_graph(L, [[0.99, 0.54], [0.54, 0.13]]).
random_kronecker_graph(L, M) ->
    LogIterations = lists:seq(1, ceil(math:log2(length(L)))),
    P = lists:foldl(
        fun(_, Acc) ->
            kronecker_product(M, Acc)
        end,
        [[1]],
        LogIterations
    ),
    lists:foldl(
        fun(I, Acc1) ->
            V1 = lists:nth(I, L),
            lists:foldl(
                fun(J, Acc2) ->
                    V2 = lists:nth(J, L),
                    EdgeProbability = lists:nth(J, lists:nth(I, P)),
                    RandReal = rand:uniform_real(),
                    if
                        RandReal < (1 - EdgeProbability) * (1 - EdgeProbability) ->
                            Acc2;
                        true ->
                            add_edge(V1, V2, rand:uniform(100), Acc2)
                    end
                end,
                Acc1,
                lists:seq(I + 1, length(L))
            )
        end,
        add_nodes(L, new()),
        lists:seq(1, length(L))
    ).

json_format(NodeToJsonFun, G) ->
    Nodes = lists:foldl(
        fun(V, Acc) ->
            case Acc of
                "" -> Sep = "";
                _ -> Sep = ", "
            end,
            lists:flatten([Acc, Sep, NodeToJsonFun(V)])
        end,
        "",
        get_nodes(G)
    ),
    Edges = lists:foldl(
        fun({V1, V2, Weight}, Acc) ->
            case Acc of
                "" -> Sep = "";
                _ -> Sep = ", "
            end,
            lists:flatten([
                Acc,
                Sep,
                "{" ++ "\"weight\": " ++ Weight ++ ", " ++ "[",
                NodeToJsonFun(V1),
                ", ",
                NodeToJsonFun(V2),
                "]"
            ])
        end,
        "",
        get_edges(G)
    ),
    "\"nodes\": [" ++ Nodes ++ "],\n\"edges\": [" ++ Edges ++ "]".

show_graph(G) -> show_graph(fun(V) -> lists:flatten(io_lib:format("\"~w\"", [V])) end, G).
show_graph(NodeToJsonFun, G) ->
    {ok, File} = file:open("../logs/show_graph.json", [write]),
    io:format(File, "{~s}\n", [json_format(NodeToJsonFun, G)]),
    os:cmd("python3 ../utils/show_graph.py ../logs/show_graph.json"),
    ok.

% RESERVED

kronecker_product(M1, M2) ->
    lists:foldl(
        fun(X, Acc) ->
            XM2 = lists:map(
                fun(Y) ->
                    kronecker_subproduct(X, Y)
                end,
                M2
            ),
            lists:append(Acc, XM2)
        end,
        [],
        M1
    ).

kronecker_subproduct(L1, L2) ->
    lists:foldl(
        fun(X, Acc) ->
            XL2 = lists:map(
                fun(Y) ->
                    X * Y
                end,
                L2
            ),
            lists:append(Acc, XL2)
        end,
        [],
        L1
    ).
