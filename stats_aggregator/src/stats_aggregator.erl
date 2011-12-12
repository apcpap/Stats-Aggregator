-module(stats_aggregator).

-include_lib("eunit/include/eunit.hrl").

-export([
         add_event/2,
         add_event/3,
         get_events/3,
         populate/3,
         get_subcategories/1
         ]).

%%%===================================================================
%%% API
%%%===================================================================

add_event(CategoryList, Count) ->
    TimeList = sa_helper:time_to_list(calendar:local_time()),
    add_event(CategoryList, TimeList, Count).

add_event(CategoryList, EventTimeList_, Count) ->
    EventTimeList = sa_helper:pad_int_list_r(EventTimeList_,6,1),
    LocalTime_s = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    Timeouts = [ LocalTime_s - T || T <- sa_config:get_event_timeouts_s() ],
    lists:foldl(
        fun(Cat, CatList) ->
            lists:foldl( 
                fun(N, TimeList) ->
                    NTimeList =
                        case N =:= 0 of
                            true  -> TimeList;
                            false -> TimeList ++ [lists:nth(N, EventTimeList)]
                        end,
                    EventTime_s = calendar:datetime_to_gregorian_seconds(sa_helper:list_to_time(NTimeList)),
                    case (N =:= 0) orelse (EventTime_s >= lists:nth(N, Timeouts)) of
                        true -> gen_server:cast(singleton_event_store, {add_event, CatList, NTimeList, Count});
                        false -> skipped
                    end,
                    NTimeList
                end, [], [0,1,2,3,4,5,6]),
            CatList ++ [Cat]
        end, [], CategoryList ++ [dummy]),
    ok.

get_subcategories(CatList) ->
    sa_event_store:get_subcategories(CatList).

get_events(CategoryList, StartTimeList, EndTimeList) ->
    Start_s = calendar:datetime_to_gregorian_seconds(sa_helper:list_to_time(StartTimeList)),
    End_s   = calendar:datetime_to_gregorian_seconds(sa_helper:list_to_time(EndTimeList)),
    Interval_s = End_s - Start_s,
    MaxDataPoints = 100,
    {Granularity, SampleInterval_s} = lists:foldl(
        fun(I, {G, SampleInterval_s}) ->
            case Interval_s > MaxDataPoints * SampleInterval_s of
                true -> {G - 1, I};
                _    -> {G, SampleInterval_s}
            end
        end, {6, 1}, [60, 60*60, 60*60*24, 60*60*24*31, 60*60*24*365, 60*60*24*365*100]),
    io:format("Using granularity ~p~n", [Granularity]),
    EventsRaw = sa_event_store:get_events(CategoryList, StartTimeList, EndTimeList, Granularity),

    StartTimeSub_s = calendar:datetime_to_gregorian_seconds(sa_helper:list_to_time(lists:sublist(StartTimeList, Granularity))),
    EndTimeSub_s = calendar:datetime_to_gregorian_seconds(sa_helper:list_to_time(lists:sublist(EndTimeList, Granularity))),
    Events = fill_events(EventsRaw, [], StartTimeSub_s, EndTimeSub_s, SampleInterval_s, Granularity),
    {Events, Granularity, SampleInterval_s}.

fill_events(Events, ResultEvents, Time_s, EndTime_s, Interval_s, Granularity) ->
    case Time_s =< EndTime_s of
        false -> ResultEvents;
        true ->
            CurTimeList = lists:sublist(sa_helper:time_to_list(calendar:gregorian_seconds_to_datetime(Time_s)), Granularity),
%            io:format("Testing ~p~n", [CurTimeList]),
            case Events of
                [{CurTimeList, Count} | R] ->
%                    io:format("Found ~p~n", [TimeList]),
                    fill_events(R, [{CurTimeList, Count} | ResultEvents], Time_s + Interval_s, EndTime_s, Interval_s, Granularity);
                _ ->
                    fill_events(Events, [{CurTimeList, 0} | ResultEvents], Time_s + Interval_s, EndTime_s, Interval_s, Granularity)
            end
    end.

populate(Cats, PopSpan_s, CntLeft) ->
    populate(Cats, PopSpan_s, CntLeft, CntLeft).

populate(Cats, PopSpan_s, CntLeft, CntTotal) ->
    case (CntLeft rem 1000) of
        0 -> io:format("Populated ~p/~p~n", [CntTotal - CntLeft, CntTotal]);
        _ -> ok
    end,
    if
        CntLeft =< 0    -> ok;
        true            ->
            LocalTime = calendar:local_time(),
            LocalTime_s = calendar:datetime_to_gregorian_seconds(LocalTime),
            EventTime_s = LocalTime_s - random:uniform(PopSpan_s),
            EventTime = calendar:gregorian_seconds_to_datetime(EventTime_s),
            EventTimeList = sa_helper:time_to_list(EventTime),
            GenList = lists:nth(random:uniform(length(Cats)), Cats),
%            io:format("GenList:~p~n", [GenList]),
            EventCatList = lists:foldl(fun(E,A) ->
                                          ECat = lists:nth(random:uniform(length(E)), E),
                                          A ++ [ECat]
                                       end, [], GenList),
%            EventCatList = lists:nth(random:uniform(length(Cats)), Cats),
            add_event(EventCatList, EventTimeList, 1),
            populate(Cats, PopSpan_s, CntLeft - 1, CntTotal)
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

eventflow_test() ->
    sa_event_store:start_link(),

    Cats = [
            ["web", "page_request"],
            ["web", "page_404"],
            ["resource", "alloc"],
            ["resource", "free"]
           ],
    populate(Cats, 24 * 60 * 60, 1000),

    L = get_events(["web","page_404"], [2010], [2012]),
    io:format(user, "Result: ~p~n", [L]),
%    sa_event_store:print(),
    ok.