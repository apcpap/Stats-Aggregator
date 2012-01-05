-module(stats_aggregator_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export ([start/0]).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) -> start().

start() ->
%    configure(),
    sa_helper:load_config("C:/twofloats/Repositories/StatsAggregator/stats_aggregator/priv/app.config"),
    application:start(sasl),
    application:start(crypto),
    application:start(inets),
    application:start(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, webmachine_logger),
    application:start(webmachine),
    sa_server_sup:start_link().

stop(_State) ->
    ok.

% when CONFIG is specified, we are loading the config file manually
%configure() ->
%	case os:getenv("CONFIG") of
%		false ->
%			nothing;
%		FileName ->
%			love_hate_utils:load_config(FileName)
%	end.