#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

#############################
## amoc REST API functions ##
#############################

function add_users() {
    local port="$(amoc_container_port "$1")" # $1 - where to send request
    local n="$2" # $2 - number of users to add
    shift 2
    local nodes="$(node_list "$@")"
    local json_body="{ ${nodes} \"users\": $n}"
    curl -X PATCH --header 'Content-Type: application/json' --header 'Accept: application/json' \
         -s  -w " %{http_code}" -d "$json_body" "http://localhost:${port}/execution/add_users"
}

function remove_users() {
    local port="$(amoc_container_port "$1")" # $1 - where to send request
    local n="$2" # $2 - number of users to add
    shift 2
    local nodes="$(node_list "$@")"
    local json_body="{ ${nodes} \"users\": $n}"
    curl -X PATCH --header 'Content-Type: application/json' --header 'Accept: application/json' \
         -s  -w " %{http_code}" -d "$json_body" "http://localhost:${port}/execution/remove_users"
}


echo "adding users to nodes"
add_users amoc-master 6 "amoc_arsenal@amoc-worker-3" | contains 200
add_users amoc-master 2 "amoc_arsenal@"{"amoc-worker-1","amoc-worker-2"} | contains 200
add_users amoc-master 15 | contains 200 ## add 5 users on each node
add_users amoc-worker-3 15 | contains 500 '"error":"not_a_master"'

echo "checking status of the nodes"
sleep 1 ## 1 second is enough to start 11
get_status amoc-master | contains '"amoc_status":"up"' \
                                  '"status":"disabled"'
worker_status=( '"amoc_status":"up"'
                '"status":"running"'
                '"scenario":"dummy_scenario"'
                '"test":"<<\\"test_value\\">>"'
                '"interarrival":"30"' )
get_status amoc-worker-1 | contains "${worker_status[@]}" '"number_of_users":11'
get_status amoc-worker-2 | contains "${worker_status[@]}" '"number_of_users":11'
get_status amoc-worker-3 | contains "${worker_status[@]}" '"number_of_users":11'

echo "removing users from nodes"
 ## remove all the users from amoc-worker-3
remove_users amoc-master 11 "amoc_arsenal@amoc-worker-3" | contains 200
remove_users amoc-master 2 "amoc_arsenal@"{"amoc-worker-1","amoc-worker-2"} | contains 200
sleep 3 ## two seconds is a shutdown period for a forced users removal.
        ## so we must wait before removing the next chunk of users,
        ## otherwise the same users can be selected for removal twice.
remove_users amoc-master 15 | contains 200 ## remove 5 users from each node
remove_users amoc-worker-3 15 | contains 500 '"error":"not_a_master"'

echo "checking status of the nodes"
sleep 3 ## 2 seconds is a shutdown period for a forced users removal.
get_status amoc-master | contains '"amoc_status":"up"' \
                                  '"status":"disabled"'
get_status amoc-worker-1 | contains "${worker_status[@]}" '"number_of_users":5'
get_status amoc-worker-2 | contains "${worker_status[@]}" '"number_of_users":5'
get_status amoc-worker-3 | contains "${worker_status[@]}" '"number_of_users":0'
