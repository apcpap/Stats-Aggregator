-module(sa_webserver).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([
         start_link/1,
         start_link/0,
         get_count/0,
         stop/0,
         extract_param/2
         ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8001).
-define(SEPARATOR_CHAR, $!).

-record(state, {port, lsock, request_count = 0}).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% @spec start_link() -> {ok, Pid}
%% @doc Calls `start_link(Port)' using the default port.
start_link() ->
    start_link(?DEFAULT_PORT).

%%--------------------------------------------------------------------
%% @doc Fetches the number of requests made to this server.
%% @spec get_count() -> {ok, Count}
%% where
%%  Count = integer()
%% @end
%%--------------------------------------------------------------------
get_count() ->
    gen_server:call(?SERVER, get_count).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp_closed, Socket}, #state{lsock = LSock} = State) ->
    io:format("TCP connection closed~n"),
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State};
handle_info({tcp, Socket, RawData}, State) ->
    io:format("TCP data received~n"),
    try
        do_call(Socket, RawData)
    catch
        _Class:Err ->
            io:format("Error in call: ~p~n", [Err])
    end,
    gen_tcp:close(Socket),
    {ok, _Sock} = gen_tcp:accept(State#state.lsock),
    RequestCount = State#state.request_count,
    {noreply, State#state{request_count = RequestCount + 1}};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_call(Socket, RawData) ->
    io:format("Got Raw Data :->~p<-~n", [RawData]),
    % TODO: unescape the category name !!!
    User        = extract_param(RawData, "user"),
    Password    = extract_param(RawData, "password"),
    CallBack    = extract_param(RawData, "jsoncallback"),
    Action      = extract_param(RawData, "action"),
    ResponseJSON =
        case Action of
            "getstats" ->
                Category = sa_helper:remove_empty_items(re:split([?SEPARATOR_CHAR] ++ extract_param(RawData, "category"), [?SEPARATOR_CHAR], [{return, list}])),
                true = sa_auth:is_authorized(User, Password, Category),
                stats_aggregator:add_event(["monitor", "statsqueries", User], 1),
                StartTime = sa_helper:stringlist_to_intlist(sa_helper:remove_empty_items(
                                re:split([?SEPARATOR_CHAR] ++ extract_param(RawData, "starttime"), [?SEPARATOR_CHAR], [{return, list}])
                           )),
                EndTime = sa_helper:stringlist_to_intlist(sa_helper:remove_empty_items(
                                re:split([?SEPARATOR_CHAR] ++ extract_param(RawData, "endtime"), [?SEPARATOR_CHAR], [{return, list}])
                           )),
                get_events_json(Category,StartTime,EndTime);
            "getcategories" ->
                Category = sa_helper:remove_empty_items(re:split([?SEPARATOR_CHAR] ++ extract_param(RawData, "category"), [?SEPARATOR_CHAR], [{return, list}])),
                true = sa_auth:is_authorized(User, Password, Category),
                L = stats_aggregator:get_subcategories(Category),
                JSONItems = ["{" ++ str_to_json("cat", escape_list_to_string(CatList)) ++ ", " ++ str_to_json("count", integer_to_list(Count)) ++ "}"
                             || {CatList, Count} <- L],
                "{" ++ list_to_json_array("items", JSONItems) ++ "}";
            "getauthcategories" ->
                AllowedCats = sa_auth:get_allowed_categories(User, Password),
                EscapedAuthCats = [ escape_list_to_string(E) || E <- AllowedCats],
                JSONCats = [ "{" ++ str_to_json("cat", E) ++ "}" || E <- EscapedAuthCats],
                "{" ++ list_to_json_array("allowedcats", JSONCats) ++ "}";
            _ ->
                "{}"
        end,
    Response = string_to_http_response(CallBack ++ "(" ++ ResponseJSON ++ ")"),
    gen_tcp:send(Socket, Response),
    io:format("Sent Answer:~n~p~n",[Response]).

get_events_json(CategoryList, StartTimeList, EndTimeList) ->
    {L, Granularity, SampleInterval_s} = stats_aggregator:get_events(CategoryList, StartTimeList, EndTimeList),
    Converter =
        fun({TimeList, Count}) ->
            TList = sa_helper:time_to_list(sa_helper:list_to_time(TimeList)),
            Time = lists:foldl(fun(E,A) -> A ++ integer_to_list(E) ++ " " end, "", TList),
            "{" ++ list_to_json_group([str_to_json("time", Time),
                                       str_to_json("count", integer_to_list(Count))]) ++ "}"
        end,
    "{" ++ list_to_json_group([str_to_json("resolution_s", integer_to_list(SampleInterval_s)),
                               str_to_json("granularity", integer_to_list(Granularity)),
                               list_to_json_array("items", [ Converter(X) || X <- L])]) ++ "}".

string_to_http_response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
      io_lib:fwrite(
         "HTTP/1.1 200 OK\r\nContent-Type: application/x-javascript; charset=utf-8\r\nContent-Length: ~p\r\n\r\n~s",
         [size(B), B])).


extract_param(RawData, ParamName) ->
%   HEADER = re:replace(RawData, "\r\n$", "", [{return, list}]),

    % TODO: the following regexp matching is suspected to be crappy, but works for now
    {match, [ParamVal]} =
        re:run(RawData,
               "(.*)[\?](.*)" ++ ParamName ++ "=(.*)[\ \&](.*)",
                   [{capture, [3], list}, ungreedy]),
    ParamVal.


list_to_json_group(StringList) ->
    {_, ItemString} = lists:foldl(
        fun(E, {N, S}) ->
            Item = E,
            if
                N > 0 -> {N + 1, S ++ ", " ++ Item};
                true  -> {N + 1, S ++ Item}
            end
        end, {0, ""}, StringList),
    ItemString.

list_to_json_array(Name, StringList) ->
    Name ++ ": [" ++ list_to_json_group(StringList) ++ "]".

escape_list_to_string(List) ->
    lists:foldl(
        fun(E,A) ->
            case A of
                [] -> A ++ E;
                _  -> A ++ [?SEPARATOR_CHAR] ++ E
            end
        end, "", List).


str_to_json(Name, Str) ->
    Name ++ ": \"" ++ Str ++ "\"".

%%%===================================================================
%%% Tests
%%%===================================================================
