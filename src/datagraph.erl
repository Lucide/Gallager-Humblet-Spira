% module to handle graphs with possible data on nodes and edges
% (e.g. pairs of coordinates on nodes and/or weights on edges)
%
% EXAMPLES OF USE:
%
% datagraph:load("graph.json")
% datagraph:add_node(new_node, coordinates, G)
% datagraph:add_edge(node1, node2, weight, G)

-module(datagraph).
-export([
    new/0,
    load/1,
    save/2,
    add_node/2, add_node/3,
    add_nodes/2,
    add_directed_edge/3, add_directed_edge/4,
    add_directed_edges/2,
    add_edge/3, add_edge/4,
    add_edges/2,
    get_node_data/2,
    get_edge_data/3,
    get_list_of_nodes/1,
    get_list_of_datanodes/1,
    get_list_of_adjs/2,
    get_list_of_dataadjs/2,
    get_list_of_edges/1,
    get_list_of_dataedges/1
]).

new() ->
    maps:new().

load(Filename) ->
    {ok, JsonString} = file:read_file(Filename),
    JsonData = jsone:decode(JsonString),

    G0 = new(),
    G1 = lists:foldl(
        fun(Elem, Acc) ->
            case Acc of
                {badformat, Elem} ->
                    Acc;
                _ ->
                    case jsondata_to_node(Elem) of
                        undefined -> {badformat, Elem};
                        {V, undefined} -> add_node(V, Acc);
                        {V, Data} -> add_node(V, Data, Acc)
                    end
            end
        end,
        G0,
        maps:get(<<"nodes">>, JsonData)
    ),
    G2 = lists:foldl(
        fun(Elem, Acc) ->
            case Acc of
                {badformat, Elem} ->
                    Acc;
                _ ->
                    case jsondata_to_edge(Elem) of
                        undefined -> {badformat, Elem};
                        {V1, V2, undefined} -> add_edge(V1, V2, Acc);
                        {V1, V2, Data} -> add_edge(V1, V2, Data, Acc)
                    end
            end
        end,
        G1,
        maps:get(<<"edges">>, JsonData)
    ),
    G2.

save(G, Filename) ->
    Nodes = get_list_of_datanodes(G),
    Edges = get_list_of_dataedges(G),

    JsonData = #{nodes => Nodes, edges => Edges},
    JsonString = jsone:encode(JsonData),

    {ok, FileHandle} = file:open(Filename, [write, binary]),
    ok = file:write(FileHandle, JsonString),
    ok = file:close(FileHandle).

add_node(V, G) ->
    add_node(V, undefined, G).

add_node(V, Data, G) ->
    maps:put(V, {Data, maps:new()}, G).

add_nodes(L, G) when is_list(L) ->
    lists:foldl(fun(V, Acc) -> add_node(V, Acc) end, G, L);
add_nodes(M, G) when is_map(M) ->
    map:fold(fun(V, Data, Acc) -> add_node(V, Data, Acc) end, G, M).

add_directed_edge(V1, V2, G) ->
    add_directed_edge(V1, V2, undefined, G).

add_directed_edge(V1, V2, EdgeData, G) ->
    {V1Data, V1Adjs} = maps:get(V1, G),
    NewAdjs = maps:put(V2, EdgeData, V1Adjs),
    maps:put(V1, {V1Data, NewAdjs}, G).

add_directed_edges(L, G) when is_list(L) ->
    lists:foldl(fun({V1, V2}, Acc) -> add_directed_edge(V1, V2, Acc) end, G, L);
add_directed_edges(M, G) when is_map(M) ->
    maps:fold(fun({V1, V2}, Data, Acc) -> add_directed_edge(V1, V2, Data, Acc) end, G, M).

add_edge(V1, V2, G) ->
    add_edge(V1, V2, undefined, G).

add_edge(V1, V2, Data, G) ->
    if
        V1 == V2 ->
            add_directed_edge(V1, V2, Data, G);
        true ->
            add_directed_edge(V1, V2, Data, add_directed_edge(V2, V1, Data, G))
    end.

add_edges(L, G) when is_list(L) ->
    lists:foldl(fun({V1, V2}, Acc) -> add_edge(V1, V2, Acc) end, G, L);
add_edges(M, G) when is_map(M) ->
    maps:fold(fun({V1, V2}, Data, Acc) -> add_edge(V1, V2, Data, Acc) end, G, M).

get_node_data(V, G) ->
    case maps:get(V, G) of
        {badkey, _} ->
            {badnode, V};
        {Data, _Adjs} ->
            Data
    end.

get_edge_data(V1, V2, G) ->
    case maps:get(V1, G) of
        {badkey, _} ->
            {badnode, V1};
        {_Data, Adjs} ->
            case maps:get(V2, Adjs) of
                {badkey, _} ->
                    {badnode, V2};
                Data ->
                    Data
            end
    end.

get_list_of_nodes(G) ->
    maps:keys(G).

get_list_of_datanodes(G) ->
    NodesAsList = maps:to_list(G),
    lists:map(fun({V, {Data, _Adjs}}) -> {V, Data} end, NodesAsList).

get_list_of_adjs(V, G) ->
    case maps:get(V, G) of
        {badkey, _} ->
            {badnode, V};
        {_Data, Adjs} ->
            maps:keys(Adjs)
    end.

get_list_of_dataadjs(V, G) ->
    case maps:get(V, G) of
        {badkey, _} ->
            {badnode, V};
        {_Data, Adjs} ->
            maps:to_list(Adjs)
    end.

get_list_of_edges(G) ->
    maps:fold(
        fun(V1, {_Data, Adjs}, Acc) ->
            Edges = lists:map(fun(V2) -> {V1, V2} end, maps:keys(Adjs)),
            Acc ++ Edges
        end,
        [],
        G
    ).

get_list_of_dataedges(G) ->
    maps:fold(
        fun(V1, {_Data, Adjs}, Acc1) ->
            maps:fold(fun(V2, EdgeData, Acc2) -> [{V1, V2, EdgeData} | Acc2] end, Acc1, Adjs)
        end,
        [],
        G
    ).

% RESERVED

jsondata_to_node(JsonData) ->
    case JsonData of
        V when is_number(V) orelse is_binary(V) -> {V, undefined};
        [V] ->
            {V, undefined};
        [V, Data] ->
            {V, Data};
        [V | Data] ->
            {V, list_to_tuple(Data)};
        M when is_map(M) ->
            case {maps:get(<<"id">>, M, undefined), maps:get(<<"data">>, M, undefined)} of
                {undefined, _} -> undefined;
                {V, undefined} -> {V, undefined};
                {V, Data} -> {V, Data}
            end;
        {PropList} ->
            case
                {proplists:get_value(<<"id">>, PropList), proplists:get_value(<<"data">>, PropList)}
            of
                {undefined, _} -> undefined;
                {V, undefined} -> {V, undefined};
                {V, Data} -> {V, Data}
            end;
        _ ->
            undefined
    end.

jsondata_to_edge(JsonData) ->
    case JsonData of
        [V1, V2] ->
            {V1, V2, undefined};
        [V1, V2, Data] ->
            {V1, V2, Data};
        [V1 | [V2 | Data]] ->
            {V1, V2, list_to_tuple(Data)};
        M when is_map(M) ->
            case
                {
                    maps:get(<<"src">>, M, undefined),
                    maps:get(<<"dst">>, M, undefined),
                    maps:get(<<"data">>, M, undefined)
                }
            of
                {undefined, _, _} -> undefined;
                {_, undefined, _} -> undefined;
                {V1, V2, Data} -> {V1, V2, Data}
            end;
        {PropList} ->
            case
                {
                    proplists:get_value(<<"src">>, PropList),
                    proplists:get_value(<<"dst">>, PropList),
                    proplists:get_value(<<"data">>, PropList)
                }
            of
                {undefined, _, _} -> undefined;
                {_, undefined, _} -> undefined;
                {V1, V2, Data} -> {V1, V2, Data}
            end;
        _ ->
            undefined
    end.
