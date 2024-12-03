%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api).

-export([start/1, stop/0]).

-spec start(boolean()) -> {ok, pid()} | {error, any()}.
start(HasMetrics) ->
    amoc_api_logic_handler:set_validator_state(),
    Port = amoc_config_env:get(api_port, 4000),
    TransportOpts = #{socket_opts => [{ip, {0, 0, 0, 0}}, {port, Port}]},
    ProtocolOpts =
        case HasMetrics of
            true ->
                #{metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
                  stream_handlers => [cowboy_metrics_h]};
            false ->
                #{}
        end,
    amoc_rest_server:start(
        openapi_http_server,
        #{
            transport => tcp,
            transport_opts => TransportOpts,
            protocol_opts => ProtocolOpts,
            logic_handler => amoc_api_logic_handler
        }
    ).

-spec stop() -> ok | {error, not_found}.
stop() ->
    cowboy:stop_listener(openapi_http_server).
