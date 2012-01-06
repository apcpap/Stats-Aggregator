-module(sa_webserver).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([
         extract_param/2,
         get_events_json/3,
         str_to_json/2,
         escape_list_to_string/1,
         list_to_json_array/2
         ]).

-define(SEPARATOR_CHAR, $!).

%%%===================================================================
%%% API
%%%===================================================================

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
