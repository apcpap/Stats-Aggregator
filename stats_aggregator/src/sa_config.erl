-module(sa_config).

-export([
         get_event_timeouts_s/0
        ]).

%%%===================================================================
%%% API
%%%===================================================================

get_event_timeouts_s() ->
    [
        100*365*24*60*60, % keeptime for granularity "year"
        365*24*60*60,     % keeptime for granularity "months"
        30*24*60*60,      % keeptime for granularity "days"
        7*24*60*60,       % keeptime for granularity "hours"
        24*60*60,         % keeptime for granularity "minutes"
        20             % keeptime for granularity "seconds"
    ].
