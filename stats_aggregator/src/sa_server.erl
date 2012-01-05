-module (sa_server).
-export ([start/0, stop/0]).

start() -> application:start(stats_aggregator).

stop() ->
	application:stop(webmachine),
	application:stop(stats_aggregator).
