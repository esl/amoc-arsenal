-define(DUMMY_SCENARIO_MODULE(Name), <<"
-module(", (atom_to_binary(Name, utf8))/binary, ").
-moduledoc \"\"\"
some edoc
\"\"\".
-behaviour(amoc_scenario).

-required_variable(#{name => some_parameter, description => \"some parameter\"}).

-export([start/1]).
-export([init/0]).

-spec init() -> ok.
init() ->
    ok = amoc_metrics:init(counters, ", (atom_to_binary(Name, utf8))/binary, "),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(Id) ->
    ok = amoc_metrics:update_counter(", (atom_to_binary(Name, utf8))/binary, ", 1),
    amoc_user:stop().

%% generated at", ?FILE, ":", (integer_to_binary(?LINE))/binary>>).
