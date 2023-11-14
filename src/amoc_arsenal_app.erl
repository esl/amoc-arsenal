%%%-------------------------------------------------------------------
%% @doc amoc_arsenal public API
%% @end
%%%-------------------------------------------------------------------

-module(amoc_arsenal_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    amoc_api:start(),
    amoc_metrics:start(),
    amoc_arsenal_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
