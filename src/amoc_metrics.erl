-module(amoc_metrics).

-behaviour(prometheus_collector).

-export([start/0, start_predefined_metrics/1, init/2]).
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
    maybe_add_exporter().

-spec start_predefined_metrics(atom()) -> any().
start_predefined_metrics(App) ->
    Preconfigured = application:get_env(App, predefined_metrics, []),
    [init(Type, Name) || {Type, Name} <- lists:flatten(Preconfigured)].

-spec init(type(), name()) -> ok.
init(counters, Name) ->
    prometheus_counter:new([{name, Name}]);
init(gauge, Name) ->
    prometheus_gauge:new([{name, Name}]);
init(summary, Name) ->
    prometheus_summary:new([{name, Name}]);
init(Type, Name) when histogram =:= Type; times =:= Type ->
    prometheus_histogram:new([{name, Name}, {buckets, histogram_buckets()}]).

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
    case {get_ip_address(), amoc_config_env:get(prometheus_port, 9090)} of
        {IpTuple, Port}
          when is_tuple(IpTuple), is_integer(Port) ->
            Routes = [{'_', [{"/metrics/[:registry]", prometheus_cowboy2_handler, []}]}],
            Dispatch = cowboy_router:compile(Routes),
            ProtocolOpts = #{env => #{dispatch => Dispatch}},
            TransportOpts = #{socket_opts => [{port, Port}, {ip, IpTuple}]},
            cowboy:start_clear(prometheus_exporter, TransportOpts, ProtocolOpts),
            true;
        {Ip, Port} ->
            ?LOG_INFO(#{what => no_prometheus_backend_enabled, ip => Ip, port => Port}),
            false
    end.

get_ip_address() ->
    case amoc_config_env:get(prometheus_ip, {0, 0, 0, 0}) of
        IpTuple when is_tuple(IpTuple)
                     andalso 4 =:= tuple_size(IpTuple)
                     orelse 8 =:= tuple_size(IpTuple) ->
            IpTuple;
        IpAddr when is_list(IpAddr) ->
            {ok, IpTuple} = inet:parse_address(IpAddr),
            IpTuple;
        _ ->
            undefined
    end.

-spec histogram_buckets() -> [integer()].
histogram_buckets() ->
    histogram_buckets([], 1 bsl 30). % ~1.07 * 10^9

histogram_buckets(AccBuckets, Val) when Val > 0 ->
    histogram_buckets([Val | AccBuckets], Val bsr 1);
histogram_buckets(AccBuckets, _Val) ->
    AccBuckets.
