-module(sa_webmachine_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, upgrade/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    {ok, Ip} = application:get_env(ip),
    {ok, Port} = application:get_env(port),
    {ok, DispatchFile} = application:get_env(webmachine_dispatcher),
    {ok, Dispatch} = file:consult(DispatchFile),
    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
    Processes = [Web],
    {ok, { {one_for_one, 10, 10}, Processes} }.
