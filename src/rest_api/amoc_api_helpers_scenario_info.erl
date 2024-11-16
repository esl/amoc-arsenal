%%==============================================================================
%% Copyright 2024 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api_helpers_scenario_info).

-include_lib("kernel/include/eep48.hrl").

%% API
-export([is_loaded/1,
         scenario_settings/1,
         scenario_params/1,
         get_documentation/1]).

-spec get_documentation(module()) -> binary().
get_documentation(Scenario) ->
    case code:get_doc(Scenario) of
        {error, _} ->
            ScenarioName = atom_to_binary(Scenario, utf8),
            <<"cannot extract documentation for ", ScenarioName/binary>>;
        {ok, #docs_v1{module_doc = ModuleDoc}} ->
            Doc = maps:get(<<"en">>, ModuleDoc),
            iolist_to_binary(Doc)
    end.

-spec scenario_settings(module()) -> #{atom() => binary()}.
scenario_settings(Scenario) ->
    {ok, ConfigMap} = amoc_config_scenario:get_default_configuration(Scenario),
    F = fun(_Name, #{value := Value}) ->
            format(Value)
        end,
    maps:map(F, ConfigMap).

-spec scenario_params(module()) -> #{atom() => map()}.
scenario_params(Scenario) ->
    {ok, ConfigMap} = amoc_config_scenario:get_default_configuration(Scenario),
    F = fun(_Name, Param) ->
            format_param(Param)
        end,
    maps:map(F, ConfigMap).

format_param(ParamMap) ->
    F = fun(Item, Value, NewMap) ->
            case add_item(Item) of
                {true, FormattedItem} -> NewMap#{FormattedItem => format(Value)};
                false -> NewMap
            end
        end,
    maps:fold(F, #{}, ParamMap).

add_item(mod)   -> {true, module};
add_item(value) -> {true, default_value};
add_item(I) ->
    RequiredItems = [description, verification_fn, update_fn],
    case lists:member(I, RequiredItems) of
        false -> false;
        true -> {true, I}
    end.

-spec format(any()) -> binary().
format(Value) ->
    amoc_config_parser:format(Value, binary).

-spec is_loaded(binary()) -> {true, amoc:scenario()} | false.
is_loaded(ScenarioName) ->
    case to_existing_atom(ScenarioName) of
        {true, Scenario} ->
            case amoc_code_server:does_scenario_exist(Scenario) of
                true -> {true, Scenario};
                false -> false
            end;
        false ->
            false
    end.

-spec to_existing_atom(binary()) -> {true, atom()} | false.
to_existing_atom(ScenarioName) ->
    try
        {true, binary_to_existing_atom(ScenarioName, utf8)}
    catch
        error:badarg -> false
    end.
