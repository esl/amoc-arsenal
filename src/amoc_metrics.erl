-module(amoc_metrics).

-behaviour(prometheus_collector).

-export([start/0, init/2]).
-export([update_counter/1, update_counter/2, update_gauge/2, update_time/2]).
-export([deregister_cleanup/1, collect_mf/2]).

-include_lib("kernel/include/logger.hrl").

-type simple_name() :: atom() | [atom()].
-type name() :: simple_name() | {strict, simple_name()}.
-type type() :: counters | gauge | times | histogram | summary.

-export_type([name/0]).

%% ===================================================================
%% API
%% ===================================================================

-spec start() -> boolean().
start() ->
    HasMetrics = maybe_add_exporter(),
    maybe_init_predefined_metrics(),
    HasMetrics.

-spec init(type(), name()) -> ok.
init(counters, Name) ->
    prometheus_counter:new([{name, Name}]);
init(gauge, Name) ->
    prometheus_gauge:new([{name, Name}]);
init(times, Name) ->
    prometheus_summary:new([{name, Name}]);
init(summary, Name) ->
    prometheus_summary:new([{name, Name}]);
init(histogram, Name) ->
    prometheus_histogram:new([{name, Name}]).

-spec update_counter(name()) -> ok.
update_counter(Name) ->
    prometheus_counter:inc(Name).

-spec update_counter(name(), integer()) -> ok.
update_counter(Name, Value) ->
    prometheus_counter:inc(Name, Value).

-spec update_gauge(name(), integer()) -> ok.
update_gauge(Name, Value) ->
    prometheus_gauge:set(Name, Value).

-spec update_time(name(), integer()) -> ok.
update_time(Name, Value) ->
    prometheus_summary:observe(Name, Value).

-spec collect_mf(prometheus_registry:registry(), prometheus_collector:collect_mf_callback()) -> ok.
collect_mf(_Registry, Callback) ->
    Data = amoc_users_sup:count_no_of_users(),
    Mf = prometheus_model_helpers:create_mf(
           amoc_users_size, "Number of AMOC users running", gauge, Data),
    Callback(Mf),
    ok.

-spec deregister_cleanup(prometheus_registry:registry()) -> ok.
deregister_cleanup(_Registry) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec maybe_add_exporter() -> boolean().
maybe_add_exporter() ->
    case {amoc_config_env:get(prometheus_port, 9090),
          amoc_config_env:get(prometheus_ip, {0, 0, 0, 0})} of
        {Port, IpTuple}
          when is_integer(Port), is_tuple(IpTuple), 4 =:= tuple_size(IpTuple) ->
            Routes = [{'_', [{"/metrics/[:registry]", prometheus_cowboy2_handler, []}]}],
            Dispatch = cowboy_router:compile(Routes),
            ProtocolOpts = #{env => #{dispatch => Dispatch}},
            TransportOpts = #{socket_opts => [{port, Port}, {ip, IpTuple}]},
            cowboy:start_clear(prometheus_exporter, TransportOpts, ProtocolOpts),
            true;
        {Port, Ip} ->
            ?LOG_INFO(#{what => no_prometheus_backend_enabled, port => Port, ip => Ip}),
            false
    end.

maybe_init_predefined_metrics() ->
    {ok, App} = application:get_application(?MODULE),
    Preconfigured = application:get_env(App, predefined_metrics, []),
    [init(Type, Name) || {Type, Name} <- lists:flatten(Preconfigured)].
