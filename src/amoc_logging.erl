-module(amoc_logging).

-include_lib("kernel/include/logger.hrl").

-export([start/0, handle_event/4]).

start() ->
    ok = telemetry:attach_many(
           <<"amoc_html_reporter-scenario">>,
           [
            [amoc, config, get],
            [amoc, config, verify],
            [amoc, config, env],
            [amoc, cluster, connect_nodes],
            [amoc, cluster, nodedown],
            [amoc, cluster, master_node_down],
            [amoc, throttle, process]
           ],
           fun ?MODULE:handle_event/4, []).

handle_event([amoc, config, _], _, #{log_level := Level} = Metadata, _) ->
    ?LOG(Level, Metadata);

handle_event([amoc, cluster, connect_nodes], _, #{nodes := Nodes, state := State}, _) ->
    ?LOG(info, #{msg => <<"connecting to nodes">>, node => node(), nodes => Nodes, state => State});

handle_event([amoc, cluster, nodedown], #{count := 1}, #{nodes := [Node]}, _) ->
    ?LOG(error, #{msg => <<"node is down">>, node => Node});

handle_event([amoc, cluster, master_node_down], #{count := 1}, #{nodes := [Master]}, _) ->
    ?LOG(error, #{msg => <<"Master node is down. Halting.">>, node => Master});

handle_event([amoc, throttle, process],
             #{msg := Msg, process := Pid},
             #{printable_state := PrintableState, name := Name},
             _) ->
    ?LOG(debug, #{what => throttle_process_internal_event, name => Name,
                  msg => Msg, pid => Pid, state => PrintableState}).
