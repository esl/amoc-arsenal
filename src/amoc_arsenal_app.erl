%%%-------------------------------------------------------------------
%% @doc amoc_arsenal public API
%% @end
%%%-------------------------------------------------------------------

-module(amoc_arsenal_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    amoc_logging:start(),
    HasMetrics = amoc_metrics:start(),
    {ok, _} = amoc_api:start(HasMetrics),
    amoc_arsenal_sup:start_link().

stop(_State) ->
    amoc_api:stop().

%% internal functions
