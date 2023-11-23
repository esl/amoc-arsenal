#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

docker_compose up --wait --wait-timeout 100 amoc-worker-3

echo "checking that clustering is done properly"
node_list=( '"amoc_arsenal@amoc-'{"master","worker-1","worker-2","worker-3"}'":"up"' )
get_nodes amoc-master | contains "${node_list[@]}"
get_nodes amoc-worker-1 | contains "${node_list[@]}"
get_nodes amoc-worker-2 | contains "${node_list[@]}"
get_nodes amoc-worker-3 | contains "${node_list[@]}"

echo "checking status of the nodes"
worker_status=( '"amoc_status":"up"'
                '"status":"running"'
                '"scenario":"dummy_scenario"'
                '"test":"<<\\"test_value\\">>"'
                '"interarrival":"50"' )
get_status amoc-worker-1 | contains "${worker_status[@]}" '"number_of_users":5'
get_status amoc-worker-2 | contains "${worker_status[@]}" '"number_of_users":5'
sleep 1 ## wait one second to ensure that noumber of users is indeed 0
get_status amoc-worker-3 | contains "${worker_status[@]}" '"number_of_users":0'

echo "checking that metrics are reported"
wait_for_metrics "amoc-"{"master","worker-1","worker-2","worker-3"}
