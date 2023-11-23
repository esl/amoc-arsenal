#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

echo "checking that clustering is done properly"
node_list=( '"amoc_arsenal@amoc-'{"master","worker-1","worker-2"}'":"up"' )
get_nodes amoc-master | contains "${node_list[@]}"
get_nodes amoc-worker-1 | contains "${node_list[@]}"
get_nodes amoc-worker-2 | contains "${node_list[@]}"

echo "checking status of the nodes"
get_status amoc-master | contains '"amoc_status":"up"' '"status":"idle"'
get_status amoc-worker-1 | contains '"amoc_status":"up"' '"status":"idle"'
get_status amoc-worker-2 | contains '"amoc_status":"up"' '"status":"idle"'

echo "checking that metrics are reported"
wait_for_metrics "amoc-"{"master","worker-1","worker-2"}
