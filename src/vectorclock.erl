-module(vectorclock).
-export([
    new_clock/0,
    new_aux/0,
    increment/2,
    max/2,
    below_or_equal/2,
    update_on_clock_change/4,
    update_on_send/4,
    compress/1, compress/3,
    decompress/1
]).

% This module provides basic manipulation primitives for vector clocks.
% Data must be maintained locally at each node, and contains:
% 1) the actual values of the vector clock, stored in a map/associative array (one potential key for each node)
% 2) some auxiliary data structures, meant to represent:
%    - last-update values, i.e. for each key i, the owner's entry of vector clock since last update of entry i
%    - last-sent values, i.e. for each key i, the owner's entry of vector clock since a message was last sent to i
% The auxiliary data structures are used to optimize the annotation on the messages
% (only a portion of the vector clock is annotated on a message, depending on last-update and last-sent values,
%  the owner of the clock, and the node that is meant to receive the message).
% Basic functions for encoding and decoding message annotations are provided.
%
% EXAMPLES OF USE:
%
% c(vectorclock)                 		                                compile
%
% vectorclock:new_clock()                                               initializes a new vector clock
% vectorclock:new_aux()                                                 initializes the auxiliary data structure
%
% vectorclock:increment(Key, Clock)                                     increment an entry of a vector clock (used before a send)
% vectorclock:max(Clock1, Clock2)                                       take the component-wise max of two vector clocks (used after a receive)
% vectorclock:below_or_equal(Clock1, Clock2)                            check component-wise order between two clocks
%
% vectorclock:update_on_clock_change(Aux, OldClock, NewClock, Owner)    update auxiliary data based on clock differences and owner
% vectorclock:update_on_send(Aux, NewClock, Owner, Target)              update auxiliary data based on current clock, owner, and target of a send instruction
% vectorclock:compress(Clock)                                           returns a list that represents the entire vector clock, without optimizations
% vectorclock:compress(Clock, Aux, Target)                              returns a list that represents an optimized portion of the clock
% vectorclock:decompress(List)                                          returns the vector clock compressed as a list

new_clock() -> maps:new().
new_aux() -> {maps:new(), maps:new()}.

increment(Key, Clock) ->
    maps:update_with(Key, fun(X) -> X + 1 end, 1, Clock).

max(Clock1, Clock2) ->
    maps:merge_with(
        fun(_Key, Value1, Value2) ->
            erlang:max(Value1, Value2)
        end,
        Clock1,
        Clock2
    ).

below_or_equal(Clock1, Clock2) ->
    maps:fold(
        fun(Key, Value1, Acc) ->
            case maps:find(Key, Clock2) of
                {ok, Value2} -> ok;
                _ -> Value2 = 0
            end,
            Acc and (Value1 =< Value2)
        end,
        true,
        Clock1
    ).

update_on_clock_change({AuxU, AuxS}, OldClock, NewClock, Owner) ->
    % get value of owner's entry of new vector clock
    case maps:find(Owner, NewClock) of
        {ok, OwnerValue} -> ok;
        _ -> OwnerValue = 0
    end,
    % compute keys of updated entries
    % (assume keys in OldClock also occur in NewClock)
    AffectedKeys = maps:fold(
        fun(Key, NewValue, Acc) ->
            case maps:find(Key, OldClock) of
                {ok, NewValue} ->
                    Acc;
                _ ->
                    lists:append(Acc, [Key])
            end
        end,
        [],
        NewClock
    ),
    % compute last-update values
    NewAuxU = lists:foldl(
        fun(Key, Acc) ->
            maps:put(Key, OwnerValue, Acc)
        end,
        AuxU,
        AffectedKeys
    ),
    {NewAuxU, AuxS}.

update_on_send({AuxU, AuxS}, NewClock, Owner, Target) ->
    % get value of owner's entry of vector clock
    case maps:find(Owner, NewClock) of
        {ok, OwnerValue} -> ok;
        _ -> OwnerValue = 0
    end,
    % compute last-sent values
    NewAuxS = maps:put(Target, OwnerValue, AuxS),
    {AuxU, NewAuxS}.

compress(Clock) ->
    maps:to_list(Clock).

compress(Clock, {AuxU, AuxS}, Target) ->
    % compute last-sent value corresponding to Target
    case maps:find(Target, AuxS) of
        {ok, TargetLastSent} -> ok;
        _Else -> TargetLastSent = 0
    end,
    % an entry with a given key is selected for the annotation
    % if value TargetLastSent is strictly below the last-update value with that key
    maps:fold(
        fun(Key, ClockValue, Acc) ->
            case maps:find(Key, AuxU) of
                {ok, UValue} when TargetLastSent < UValue ->
                    lists:append(Acc, [{Key, ClockValue}]);
                _ ->
                    Acc
            end
        end,
        [],
        Clock
    ).

decompress(List) ->
    maps:from_list(List).
