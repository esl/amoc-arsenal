%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api_helpers_execution).

%% Backward-compatible interarrival parameter that
%% attempts to simulate per-node interarrival.
%% But, unlike the original behavior, adding a new node
%% does not change the rate for new users.
-required_variable(#{name => interarrival, default_value => 50,
                     verification => {?MODULE, positive_integer, 1},
                     description => "a delay between creating the processes for two "
                                    "consecutive users (ms, def: 50ms)",
                     update => {?MODULE, update_interarrival, 2}}).

%% API
-export([start/1, stop/0, add_users/1, remove_users/1, update_settings/1]).

-export([positive_integer/1, update_interarrival/2]).

-type body() :: #{binary() => any()}.
-type ret_value() :: {ok, any()} | {error, any()}.

-spec positive_integer(any()) -> boolean().
positive_integer(Interarrival) ->
    is_integer(Interarrival) andalso Interarrival > 0.

update_interarrival(interarrival, NewValue) ->
    update_interarrival(NewValue).

-spec start(body()) -> ret_value().
start(#{<<"scenario">> := ScenarioName} = Body) ->
    case amoc_api_helpers_scenario_info:is_loaded(ScenarioName) of
        {true, Scenario} ->
            Users = maps:get(<<"users">>, Body, 0),
            SettingsMap = maps:get(<<"settings">>, Body, #{}),
            case read_settings(SettingsMap) of
                {ok, Settings} ->
                    amoc_dist:do(Scenario, 0, Settings),
                    Interarrival = amoc_config:get(interarrival),
                    update_interarrival(Interarrival),
                    amoc_dist:add(Users);
                {error, _} = Err -> Err
            end;
        false ->
            {error, no_such_scenario}
    end;
start(_) ->
    {error, invalid_body}.

-spec stop() -> ret_value().
stop() ->
    amoc_dist:stop().

-spec add_users(body()) -> ret_value().
add_users(#{<<"users">> := Users, <<"nodes">> := Nodes}) ->
    amoc_dist:add(Users, read_nodes(Nodes));
add_users(#{<<"users">> := Users}) ->
    amoc_dist:add(Users);
add_users(_) ->
    {error, invalid_body}.

-spec remove_users(body()) -> ret_value().
remove_users(#{<<"users">> := Users, <<"nodes">> := Nodes}) ->
    amoc_dist:remove(Users, false, read_nodes(Nodes));
remove_users(#{<<"users">> := Users}) ->
    amoc_dist:remove(Users, false);
remove_users(_) ->
    {error, invalid_body}.

-spec update_settings(body()) -> ret_value().
update_settings(#{<<"settings">> := SettingsMap} = Body) ->
    case read_settings(SettingsMap) of
        {ok, Settings} ->
            case Body of
                #{<<"nodes">> := Nodes} ->
                    amoc_dist:update_settings(Settings, read_nodes(Nodes));
                _ ->
                    amoc_dist:update_settings(Settings)
            end;
        {error, _} = Err -> Err
    end;
update_settings(_) ->
    {error, invalid_body}.

read_settings(SettingsMap) ->
    try
        {ok, [read_kv(K, V) || {K, V} <- maps:to_list(SettingsMap)]}
    catch
        throw:{invalid_value, _, _, _} = Err ->
            {error, Err}
    end.

read_kv(K, V) ->
    Key = binary_to_atom(K, utf8),
    case amoc_config_parser:parse_value(V) of
        {ok, Value} -> {Key, Value};
        {error, E} ->
            throw({invalid_value, K, V, E})
    end.

read_nodes(NodeList) ->
    [binary_to_atom(Node, utf8) || Node <- NodeList].

update_interarrival(Interarrival) ->
    Interval = 60000,
    SlaveNodes = amoc_cluster:slave_nodes(),
    NumberOfSlaveNodes = length(SlaveNodes),
    MasterNode = amoc_cluster:master_node(),
    Rate = round(Interval * NumberOfSlaveNodes / Interarrival),
    Settings = [{user_rate, Rate}],
    {ok, _} = amoc_dist:update_settings(Settings, [MasterNode]).
