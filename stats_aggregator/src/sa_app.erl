%%%-------------------------------------------------------------------
%%% Created : 1. November 2011
%%%-------------------------------------------------------------------
-module(sa_app).

-behaviour(application).

-export([
	 start/2,
	 stop/1
        ]).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    case sa_sup:start_link() of
	{ok, Pid} -> {ok, Pid};
	Other     -> {error, Other}
    end.


%%--------------------------------------------------------------------
%% Func: stop/1
%% Returns: any 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

