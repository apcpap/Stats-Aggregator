-module(sa_server_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	WebmachineSup = {sa_webmachine_sup, {sa_webmachine_sup, start_link, []},
						permanent, 2000, supervisor, [sa_webmachine_sup]},
	Children = [WebmachineSup,
                    {event_store, {sa_event_store, start_link, []},
                      permanent, 2000, worker, [sa_event_store]},
                    {webserver, {sa_webserver, start_link, [8001]},
                      permanent, 2000, worker, [sa_webserver]}
                   ],
	RestartStrategy = {one_for_one, 10, 10},
	{ok, {RestartStrategy, Children}}.