#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

#############################
## amoc REST API functions ##
#############################

function update_settings() {
    local port="$(amoc_container_port "$1")" # $1 - where to send request
    local settings="$2" # $2 - settings
    shift 2
    local nodes="$(node_list "$@")"
    local json_body="{ ${nodes} \"settings\": ${settings} }"
    echo "json_body = '$json_body'" >&2
    curl -X PATCH --header 'Content-Type: application/json' --header 'Accept: application/json' \
         -s  -w " %{http_code}" -d "$json_body" "http://localhost:${port}/execution/update_settings"
}

echo "checking status of the nodes"
original_settings=( '"interarrival":"30"'
                    '"test":"<<\\"test_value\\">>"'
                    '"nodes":"\['"'"'amoc_arsenal@amoc-master'"'"'\]"' )
for service in "amoc-worker-"{"1","2","3"}; do
    get_status "${service}" | contains "${original_settings[@]}"
done

echo "update settings on the nodes"
update_settings amoc-master '{"interarrival": "40"}' \
                "amoc_arsenal@amoc-master" | contains 200
## amoc_arsenal@amoc-worker-2 fails, but amoc_arsenal@amoc-master is updated
update_settings amoc-master '{"interarrival": "50", "nodes": "[]"}' \
                "amoc_arsenal@"{"amoc-master","amoc-worker-2"} \
    | contains 500 "changing_global_parameters_on_a_slave_node"
update_settings amoc-master '{"test": "<<\"new_value\">>"}' | contains 200
update_settings amoc-worker-3 '{"test": "<<\"another_value\">>"}' \
    | contains 500 '"error":"not_a_master"'
## amoc_arsenal@amoc-worker-4 is incorrect node, other nodes
## should stay unpatched because the node list is incorrect
update_settings amoc-master '{"test": "<<\"another_value\">>"}' \
                "amoc_arsenal@"{"amoc-master","amoc-worker-2","amoc-worker-4"} \
    | contains 500 "error" 'bad_nodes' "amoc_arsenal@amoc-worker-4"

echo "checking status of the nodes"
new_settings=( '"interarrival":"50"'
               '"test":"<<\\"new_value\\">>"'
               '"nodes":"\['"'"'amoc_arsenal@amoc-master'"'"'\]"' )
get_status amoc-worker-1 | contains "${new_settings[@]}"
get_status amoc-worker-2 | contains "${new_settings[@]}"
get_status amoc-worker-3 | contains "${new_settings[@]}"
get_status amoc-master | contains '"interarrival":"50"' \
                                    '"nodes":"\[\]"' \
                                    '"test":"<<\\"new_value\\">>"'

echo "restore settings on the nodes"
update_settings amoc-master '{"interarrival":"30"}' "amoc_arsenal@amoc-master" | contains 200
update_settings amoc-master '{"test": "<<\"test_value\">>"}' | contains 200
update_settings amoc-master '{"nodes":"['"'"'amoc_arsenal@amoc-master'"'"']"}' | contains 200

echo "checking status of the nodes"
for service in "amoc-worker-"{"1","2","3"}; do
    get_status "${service}" | contains "${original_settings[@]}"
done
