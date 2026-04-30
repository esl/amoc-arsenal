#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

#############################
## amoc REST API functions ##
#############################
update_settings() {
    local port="$(amoc_container_port "$1")"
    local json_body='{ "settings" : { "interarrival" : "50" } }'
    curl -X PATCH --header 'Content-Type: application/json' \
         --header 'Accept: application/json' \
         -s  -w "%{http_code}" -o /dev/null -d "$json_body" \
         "http://localhost:${port}/execution/update_settings"
}

echo "update_settings"
update_settings amoc-master | contains 200

status=( '"user_rate":"3600"'
         '"interarrival":"50"' )
get_status amoc-master | contains "${status[@]}"
get_status amoc-worker-1 | contains "${status[@]}"
get_status amoc-worker-2 | contains "${status[@]}"
get_status amoc-worker-3 | contains "${status[@]}"
