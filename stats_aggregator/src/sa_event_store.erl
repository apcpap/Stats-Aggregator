-module(sa_event_store).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-export([
         print/0,
         start_link/0,
         get_events/4,
         get_subcategories/1
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {uid_counter = 0
               }).

-define(TABLE_ID, ?MODULE).
-define(TABLE_BY_CAT, sa_event_store_tbl_cat).
-define(TABLE_BY_TIME, sa_event_store_tbl_time).
-define(CLEANUP_INTERVAL_MS, 5 * 1000).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, singleton_event_store}, ?MODULE, [], []).

print() ->
    gen_server:call(singleton_event_store, {print}).

get_events(CategoryList, StartTimeList, EndTimeList, Granularity) ->
    {ok, L} = gen_server:call(singleton_event_store, {get_events, CategoryList, StartTimeList, EndTimeList, Granularity}),
    L.

get_subcategories(CatList) ->
    {ok, L} = gen_server:call(singleton_event_store, {get_subcategories, CatList}),
    L.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    State = #state{
                  },
    ets:new(?TABLE_BY_CAT, [public, named_table, ordered_set]),
    ets:new(?TABLE_BY_TIME, [public, named_table, ordered_set]),
    timer:send_after(?CLEANUP_INTERVAL_MS, self(), {cleanup}),

    % insert some dummy data
    Cats = [
            [
                ["apps"],
                ["app1", "app2", "app3"],
                ["access", "alloc", "free", "failure"]
            ],
            [
                ["backend"],
                ["info", "warning", "error"],
                ["event0", "event1", "event2", "event3"]
            ],
            [
                ["monitor"],
                ["access", "invalidaccess"],
                ["user1", "user2", "user3"]
            ]
           ],
    stats_aggregator:populate(Cats, 24 * 60 * 60, 10000),
%    stats_aggregator:populate(Cats, 60 * 60, 100),

    {ok, State}.


handle_call({get_events, CatList, TimeListStart, TimeListEnd, Granularity}, _From, State) ->
    StartListSub = sa_helper:fill_time_list(lists:sublist(TimeListStart, Granularity), 0),
    EndListSub = sa_helper:fill_time_list(lists:sublist(TimeListEnd, Granularity), 2),
    FirstKey = {CatList, StartListSub},
%    FirstKey = {CatList, sa_helper:fill_time_list(TimeListStart, 0)},
    MatchingEventsRev = gather_entries(CatList, StartListSub, EndListSub, TimeListStart, TimeListEnd, Granularity, [], FirstKey),
    MatchingEvents = lists:reverse(MatchingEventsRev),
    {reply, {ok, MatchingEvents}, State};

handle_call({get_subcategories, CatList}, _From, State) ->
    FirstKey = {sa_helper:fill_time_list([], 2), []},
    MatchingCats = gather_subcategories(CatList, FirstKey, []),
    {reply, {ok, MatchingCats}, State};


handle_call({print}, _From, State) ->
    L1 = ets:tab2list(?TABLE_BY_CAT),
    io:format(user, "TableByCat: ~p~n", [L1]),
    L2 = ets:tab2list(?TABLE_BY_TIME),
    io:format(user, "TableByTime: ~p~n", [L2]),
    io:format(user, "Table Size: ~p ~p~n", [length(L1), length(L2)]),
    {reply, {ok}, State};

handle_call(_, _From, State) ->
    {reply, {ok}, State}.

handle_cast({add_event, CatList, TimeList_, Count}, State) ->
    TimeList = sa_helper:fill_time_list(TimeList_),
    KeyByCat = {CatList, TimeList},
    KeyByTime = {TimeList, CatList},
    case ets:lookup(?TABLE_BY_TIME, KeyByTime) of
        [{KeyByTime, PrevCount}]   ->
            ets:insert(?TABLE_BY_TIME, {KeyByTime, Count + PrevCount}),
            ets:insert(?TABLE_BY_CAT,  {KeyByCat , Count + PrevCount}),
            {noreply, State};
        [] ->
            ets:insert(?TABLE_BY_TIME, {KeyByTime, Count}),
            ets:insert(?TABLE_BY_CAT,  {KeyByCat , Count}),
            {noreply, State}
    end;

handle_cast({remove_event, CatList, TimeList_, Count}, State) ->
    TimeList = sa_helper:fill_time_list(TimeList_),
    KeyByCat = {CatList, TimeList},
    KeyByTime = {TimeList, CatList},
    case ets:lookup(?TABLE_BY_TIME, KeyByTime) of
        [{KeyByTime, PrevCount}]   ->
            if
                PrevCount > Count   ->
                    ets:insert(?TABLE_BY_TIME, {KeyByTime, PrevCount - Count}),
                    ets:insert(?TABLE_BY_CAT,  {KeyByCat , PrevCount - Count}),
                    {noreply, State};
                PrevCount =:= Count ->
                    ets:delete(?TABLE_BY_TIME, KeyByTime),
                    ets:delete(?TABLE_BY_CAT , KeyByCat),
                    {noreply, State};
                true ->
                    throw({error, "cannot decrease count of events"})
            end;
        [] ->
            throw({error, "Event does not exist"})
    end;

handle_cast(_, State) ->
    {noreply, State}.

handle_info({cleanup}, State) ->
%    io:format(user, "Cleanup~n", []),
    Timeouts = sa_config:get_event_timeouts_s(),
    LocalTime = calendar:local_time(),
    LocalTime_s = calendar:datetime_to_gregorian_seconds(LocalTime),
    clean_up(LocalTime_s, Timeouts, 6),
    timer:send_after(?CLEANUP_INTERVAL_MS, self(), {cleanup}),
    {noreply, State};

handle_info(timeout, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


gather_subcategories(CatList, Key, List) ->
    NewList =
        case ets:lookup(?TABLE_BY_TIME, Key) of
            [{{_, ECatList}, Count}] ->
                case sa_helper:list_has_prefix(CatList, ECatList) andalso
                     (length(ECatList) =:= length(CatList) + 1)
                    of
                    true  -> [{ECatList, Count} | List];
                    false -> List
                end;
            [] ->
                List
        end,
    case ets:prev(?TABLE_BY_TIME, Key) of
        '$end_of_table' ->
            NewList;
        NextKey ->
            gather_subcategories(CatList, NextKey, NewList)
    end.


clean_up(LocalTime_s, Timeouts, Granularity) ->
    if
        Granularity > 0 ->
            Timeout_s = lists:nth(Granularity, Timeouts),
            EndTime_s = LocalTime_s - Timeout_s,
            EndTime = calendar:gregorian_seconds_to_datetime(EndTime_s),
            EndTimeList = sa_helper:time_to_list(EndTime),
            KeyTimeList = lists:sublist(EndTimeList, Granularity),
            FirstKey = {sa_helper:fill_time_list(KeyTimeList, 2), ""},
            clean_entries(EndTimeList, Granularity, FirstKey),
            % clean next granularity
            clean_up(LocalTime_s, Timeouts, Granularity - 1);
        true -> ok
    end.

clean_entries(EndTimeList, Granularity, Key) ->
%    io:format(user, "Checking Key ~p for gran ~p list ~p ~n", [Key, Granularity, EndTimeList]),
    RangeEnd =
        case ets:lookup(?TABLE_BY_TIME, Key) of
            [{{ETimeList_, ECatList}, _}] ->
                ETimeList = sa_helper:unfill_time_list(ETimeList_),
%                io:format(user, "Comparing ~p agains ~p~n", [ETimeList, EndTimeList]),
                case (length(ETimeList) =:= Granularity) andalso
                    sa_helper:is_intlist_leq(ETimeList, EndTimeList) of
                        true    ->
%                            io:format(user, "Deleting ~p~n", [Key]),
                            ets:delete(?TABLE_BY_TIME, {ETimeList_, ECatList}),
                            ets:delete(?TABLE_BY_CAT , {ECatList  , ETimeList_}),
                            false;
                        false ->
                            true
                end;
            [] ->
                false
        end,
%    io:format(user, "Proceeding~n", []),
    case ets:prev(?TABLE_BY_TIME, Key) of
        '$end_of_table' ->
            ok;
        NextKey ->
            if
                RangeEnd =:= true   -> ok;
                true                -> clean_entries(EndTimeList, Granularity, NextKey)
            end
    end.


gather_entries(CatList, StartListSub, EndListSub, StartList, EndList, Granularity, ResultList, Key) ->
%    io:format(user, "checking key ~p ~p ~p ~p~n", [Key, TimeListStart, TimeListEnd, Granularity]),
    LookupResult =
        case ets:lookup(?TABLE_BY_CAT, Key) of
            [{{ECatList, ETimeListSub}, Count}] ->
                ETimeList = sa_helper:unfill_time_list(ETimeListSub),
%                io:format(user,"Testing ~p ~p ~p ~p~n", [ECatList, ETimeList, StartListSub, EndListSub]),
                case (length(ETimeList) =:= Granularity) andalso
                    (CatList =:= ECatList) andalso
                    sa_helper:is_intlist_inbetween(ETimeListSub, StartListSub, EndListSub) of
                        true    ->
%                            io:format("Key is in range ~p ~p ~p ~n", [ETimeList, StartList, EndList]),
                            case sa_helper:is_intlist_inbetween(ETimeList, StartList, EndList) of
                                true -> [{ETimeList, Count} | ResultList];
                                _    -> ResultList
                            end;
                        false   ->
%                            io:format("Key is out of range"),
                            end_of_range
                end;
            _ ->
                ResultList
        end,
    case LookupResult of
        end_of_range ->
%            io:format("End of range~n",[]),
            ResultList;
        NewResultList ->
            case ets:next(?TABLE_BY_CAT, Key) of
                '$end_of_table' ->
%                    io:format("End of table~n",[]),
                    NewResultList;
                NextKey ->
                    gather_entries(CatList, StartListSub, EndListSub, StartList, EndList, Granularity, NewResultList, NextKey)
            end
    end.


%%%===================================================================
%%% Tests
%%%===================================================================

start_test() ->
    {ok, Pid} = start_link(),
    gen_server:cast(Pid, {add_event, ["t0", "s0"], [2011,11,14], 1}),
    gen_server:cast(Pid, {add_event, ["t0", "s0"], [2011,11,15], 1}),
    {ok, [_, _]} = gen_server:call(Pid, {get_events, ["t0", "s0"], [2011,0,0,0,0,0], [2012,0,0,0,0,0], 3}),
    {ok, []} = gen_server:call(Pid, {get_events, ["t0", "s0"], [2011,0,0,0,0,0], [2011,0,0,0,0,0], 3}),


    gen_server:cast(Pid, {add_event, ["t0", "s0"], [2011,11,14], 1}),
    {ok, [_, _]} = gen_server:call(Pid, {get_events, ["t0", "s0"], [2011,0,0,0,0,0], [2012,0,0,0,0,0],3}),
    gen_server:cast(Pid, {remove_event, ["t0", "s0"], [2011,11,14], 1}),
    {ok, [_, _]} = gen_server:call(Pid, {get_events, ["t0", "s0"], [2011,0,0,0,0,0], [2012,0,0,0,0,0],3}),
    gen_server:cast(Pid, {remove_event, ["t0", "s0"], [2011,11,14], 1}),
    {ok, [_]} = gen_server:call(Pid, {get_events, ["t0", "s0"], [2011,0,0,0,0,0], [2012,0,0,0,0,0],3}),

    ok.

