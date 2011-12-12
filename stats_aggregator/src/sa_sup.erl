%% Supervisor for the buckets
-module(sa_sup).

-behaviour(supervisor).

-export([
            start_link/0
        ]).

-export([init/1]).

-define(SERVER,?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Children = [ 
                 {event_store, {sa_event_store, start_link, []},
                  permanent, 2000, worker, [sa_event_store]},
                 {webserver, {sa_webserver, start_link, [8001]},
                  permanent, 2000, worker, [sa_webserver]}
               ],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

