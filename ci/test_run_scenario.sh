#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

#############################
## amoc REST API functions ##
#############################
run_scenario() {
    local port="$(amoc_container_port "$1")"
    local json_body='{ "scenario": "'"$2"'", "users": '"$3"' , "settings" : { "test" : "<<\"test_value\">>" } }'
    curl -X PATCH --header 'Content-Type: application/json' --header 'Accept: application/json' \
         -s  -w "%{http_code}" -o /dev/null -d "$json_body" "http://localhost:${port}/execution/start"
}

echo "executing dummy_scenario"
run_scenario amoc-master dummy_scenario 10 | contains 200

echo "checking status of the nodes"
get_status amoc-master | contains '"amoc_status":"up"' \
                                  '"status":"disabled"'
sleep 1 ## 1 second is enought to start 5 users with 50ms interarrival timeout
worker_status=( '"amoc_status":"up"'
                '"status":"running"'
                '"scenario":"dummy_scenario"'
                '"number_of_users":5'
                '"test":"<<\\"test_value\\">>"'
                '"interarrival":"50"' )
get_status amoc-worker-1 | contains "${worker_status[@]}"
get_status amoc-worker-2 | contains "${worker_status[@]}"
