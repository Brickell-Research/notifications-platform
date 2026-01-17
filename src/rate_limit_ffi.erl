-module(rate_limit_ffi).
-export([create_ets_table/0, current_time_seconds/0, get_request_count/3, record_request/3]).

create_ets_table() ->
    try
        Table = ets:new(rate_limit_table, [bag, public, {read_concurrency, true}]),
        {ok, Table}
    catch
        _:_ -> {error, nil}
    end.

current_time_seconds() ->
    erlang:system_time(second).

get_request_count(Table, Key, Cutoff) ->
    % Get all entries for this key
    Entries = ets:lookup(Table, Key),
    % Count entries with timestamp > cutoff
    Count = lists:foldl(fun({_, Timestamp}, Acc) ->
        case Timestamp > Cutoff of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, Entries),
    % Clean up old entries while we're at it
    ets:select_delete(Table, [{{Key, '$1'}, [{'=<', '$1', Cutoff}], [true]}]),
    Count.

record_request(Table, Key, Timestamp) ->
    ets:insert(Table, {Key, Timestamp}),
    nil.
