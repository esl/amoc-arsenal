-module(amoc_logging).

-include_lib("kernel/include/logger.hrl").

-export([start/0,
         handle_cluster_event/4,
         handle_log_event/4]).

start() ->
    ok = telemetry:attach_many(
           <<"amoc-arsenal-logging-cluster">>,
           [
            [amoc, cluster, connect_nodes],
            [amoc, cluster, nodedown],
            [amoc, cluster, master_node_down]
           ],
           fun ?MODULE:handle_cluster_event/4, []),
    ok = telemetry:attach_many(
           <<"amoc-arsenal-logging-pure-log-events">>,
           [
            [amoc, config, get],
            [amoc, config, verify],
            [amoc, config, env],
            [amoc, throttle, process]
           ],
           fun ?MODULE:handle_log_event/4, []).

handle_cluster_event([amoc, cluster, connect_nodes], _, #{nodes := Nodes, state := State}, _) ->
    ?LOG(info, #{msg => <<"connecting to nodes">>, node => node(), nodes => Nodes, state => State});
handle_cluster_event([amoc, cluster, nodedown], _, #{nodes := [Node]}, _) ->
    ?LOG(error, #{msg => <<"node is down">>, node => Node});
handle_cluster_event([amoc, cluster, master_node_down], _, #{nodes := [Master]}, _) ->
    ?LOG(error, #{msg => <<"Master node is down. Halting.">>, node => Master}).

handle_log_event([amoc | _], _, #{log_level := Level} = Metadata, _) ->
    ?LOG(Level, Metadata).
