-module(latency).
-export([init/0, stop/0, send/3, send/4, send/5, default_in_fun/4, default_out_fun/6]).

% EXAMPLES OF USE:
%
% c(latency)                   			        	compile
%
% latency:init()                                	initialise a table of routers that simulate delays
% latency:stop()                                	destroys the table of routers
%
% latency:send(To, Msg, Latency)                	like "To ! Msg", but simulates a latency in milliseconds
%
% latency:send(To, Msg, Latency, InFun, OutFun)		as above, but use InFun(From, To, Router, AnnotatedMsg) instead of erlang:send(To, Msg)
% 													to forward annotated message to Router, and then when message is ready to be received,
% 													call OutFun(From, To, Router, Msg, SendTime, ReceiveTime) to put the mesasge into the
% 													target mailbox, e.g. using erlang:send(To, Msg)
% 													(e.g. this function can also be used to log the send-receive times)

init() ->
    try
        ets:new(routers, [set, named_table, public, {read_concurrency, true}])
    catch
        error:badarg -> ok
    end,
    ok.

stop() ->
    try
        Entries = ets:tab2list(routers),
        lists:foreach(
            fun({_, Pid}) ->
                exit(Pid, kill)
            end,
            Entries
        ),
        ets:delete(routers)
    catch
        error:badarg -> ok
    end,
    ok.

send(To, Msg, Latency) ->
    send(To, Msg, Latency, fun default_in_fun/4, fun default_out_fun/6).

send(To, Msg, InFun, OutFun) ->
    send(To, Msg, 100, InFun, OutFun).

send(To, Msg, Latency, InFun, OutFun) ->
    From = self(),
    Router = find_or_spawn_router(From, To),
    AnnotatedMsg =
        {Msg, erlang:monotonic_time(), erlang:convert_time_unit(Latency, millisecond, native),
            OutFun},
    InFun(From, To, Router, AnnotatedMsg).

default_in_fun(_From, _To, Router, AnnotatedMsg) ->
    erlang:send(Router, AnnotatedMsg).

default_out_fun(_From, To, _Router, Msg, _SendTime, _ReceiveTime) ->
    erlang:send(To, Msg).

% RESERVED

router_loop(From, To) ->
    receive
        {Msg, SendTime, ExpectedDelay, OutFun} ->
            ActualDelay = erlang:monotonic_time() - SendTime,
            if
                ActualDelay < ExpectedDelay ->
                    timer:sleep(
                        erlang:convert_time_unit(ExpectedDelay - ActualDelay, native, millisecond)
                    );
                true ->
                    ok
            end,
            ReceiveTime = erlang:monotonic_time(),
            Router = self(),
            OutFun(From, To, Router, Msg, SendTime, ReceiveTime),
            router_loop(From, To)
    end.

find_or_spawn_router(From, To) ->
    case ets:lookup(routers, {From, To}) of
        [] ->
            Router = spawn_link(fun() -> router_loop(From, To) end),
            ets:insert(routers, {{From, To}, Router});
        [{_, Router}] ->
            ok
    end,
    Router.
