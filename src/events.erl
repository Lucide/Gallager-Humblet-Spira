-module(events).
-export([
    init/1,
    stop/0,
    process_state/1, process_state/2,
    tick/0, tick/1,
    link_state/2, link_state/3,
    send/2, send/3,
    received_annotated_msg/1
]).

% callback functions that will notify changes in process state, link state, or message reception
-callback on_process_state(
    Pid :: pid(), OldState :: term(), NewState :: term(), Time :: term(), VClock :: term()
) -> ok.
-callback on_link_state(
    From :: pid(),
    To :: pid(),
    OldState :: term(),
    NewState :: term(),
    Time :: term(),
    VClock :: term()
) -> ok.
-callback on_message(
    From :: pid(),
    To :: pid(),
    Msg :: term(),
    FromState :: term(),
    ToState :: term(),
    FromTime :: term(),
    ToTime :: term(),
    FromVClock :: term(),
    ToVClock :: term()
) -> ok.

% EXAMPLES OF USE:
%
% c(events)                   			              compile
%
% events:init(?MODULE)                                initialize a table to store states and physical/logical timestamps for processes and links
% 													  the module ?MODULE needs to implement the callback functions on_state_update and on_message
% 													  (e.g. to log information about events)
% events:stop()                                       destroy the table and stop the service
%
% events:process_state(Pid, NewState)                 to be called for every potential change in a state of a process
% events:link_state(From, To, NewState)               to be called for every potential change in a state of a link
%
% events:send(To, Msg)                       	      like erlang:send, but also includes an annotation in the message
% events:send(To, Msg, SendFun)              	      as above, but uses SendFun(To, Msg) instead of erlang:send(To, Msg)
%
% events:received_annotated_msg(Msg)                  to be called every time an annotated message is consumed at the receiver process

init(Module) ->
    ets:new(events_info, [
        set, named_table, public, {read_concurrency, true}, {write_concurrency, true}
    ]),
    Server = spawn_link(fun() -> loop(Module) end),
    register(events_callback_server, Server),
    ok.

stop() ->
    events_callback_server ! {stop, self()},
    receive
        ack -> ok
    end,
    ets:delete(events_info),
    ok.

process_state(NewState) ->
    process_state(self(), NewState).

process_state(Pid, NewState) ->
    {OldState, _OldTime, OldVClock} = get_info(Pid),
    NewTime = erlang:monotonic_time(),
    NewVClock = vectorclock:increment(Pid, OldVClock),
    put_info(Pid, {NewState, NewTime, NewVClock}),
    events_callback_server ! {callback_process_state, Pid, OldState, NewState, NewTime, NewVClock},
    ok.

tick() -> tick(self()).

tick(Pid) ->
    {State, _OldTime, OldVClock} = get_info(Pid),
    NewTime = erlang:monotonic_time(),
    NewVClock = vectorclock:increment(Pid, OldVClock),
    put_info(Pid, {State, NewTime, NewVClock}),
    ok.

link_state(To, NewState) ->
    link_state(self(), To, NewState).

link_state(From, To, NewState) ->
    {OldState, _OldTime, OldVClock} = get_info({From, To}),
    NewTime = erlang:monotonic_time(),
    {_, _, VClock1} = get_info(From),
    {_, _, VClock2} = get_info(To),
    NewVClock = vectorclock:max(OldVClock, vectorclock:max(VClock1, VClock2)),
    put_info({From, To}, {NewState, NewTime, NewVClock}),
    events_callback_server !
        {callback_link_state, From, To, OldState, NewState, NewTime, NewVClock}.

send(To, Msg) ->
    send(To, Msg, fun erlang:send/2).

send(To, Msg, SendFun) ->
    SendFun(To, {Msg, create_annotation(self(), To)}).

% Msg must be a tuple whose last element is an annotation
received_annotated_msg(Msg) ->
    {ActualMsg, Annotation} = divide_last_element(Msg),
    {From, FromState, FromTime, FromVClock} = Annotation,

    To = self(),
    {ToState, _OldToTime, OldToVClock} = get_info(To),
    NewToTime = erlang:monotonic_time(),
    NewToVClock = vectorclock:max(OldToVClock, FromVClock),
    put_info(To, {ToState, NewToTime, NewToVClock}),

    events_callback_server !
        {callback_message, From, To, ActualMsg, FromState, ToState, FromTime, NewToTime, FromVClock,
            NewToVClock},
    ok.

% RESERVED

get_info(Location) ->
    case ets:lookup(events_info, Location) of
        [{_, Info}] -> Info;
        _ -> {undefined, undefined, vectorclock:new_clock()}
    end.

put_info(Location, Info) ->
    true = ets:insert(events_info, {Location, Info}),
    ok.

create_annotation(From, _To) ->
    {FromState, FromTime, FromVClock} = get_info(From),
    put_info(From, {FromState, FromTime, FromVClock}),
    {From, FromState, FromTime, FromVClock}.

divide_last_element({Msg, Annotation}) ->
    {Msg, Annotation};
divide_last_element(T) when is_tuple(T) andalso tuple_size(T) > 2 ->
    {L, Annotation} = divide_last_element(tuple_to_list(T)),
    {list_to_tuple(L), Annotation};
divide_last_element(T) when is_tuple(T) ->
    {L, Annotation} = divide_last_element(tuple_to_list(T)),
    {list_to_tuple(L), Annotation};
divide_last_element([H]) ->
    {[], H};
divide_last_element([H | T]) ->
    {L, Annotation} = divide_last_element(T),
    {[H | L], Annotation}.

loop(Module) ->
    receive
        {callback_process_state, Pid, OldState, NewState, Time, VClock} ->
            Module:on_process_state(Pid, OldState, NewState, Time, VClock),
            loop(Module);
        {callback_link_state, From, To, OldState, NewState, Time, VClock} ->
            Module:on_link_state(From, To, OldState, NewState, Time, VClock),
            loop(Module);
        {callback_message, From, To, Msg, FromState, ToState, FromTime, ToTime, FromVClock,
            ToVClock} ->
            Module:on_message(
                From, To, Msg, FromState, ToState, FromTime, ToTime, FromVClock, ToVClock
            ),
            loop(Module);
        {stop, From} ->
            From ! ack
    end.
