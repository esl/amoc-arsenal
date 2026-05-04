#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

function run() {
    echo "$@"
    $@
}

run ./ci/build_docker_image.sh
run ./ci/start_demo_cluster.sh
run ./ci/test_amoc_cluster.sh
run ./ci/test_distribute_scenario.sh
run ./ci/test_run_scenario.sh
run ./ci/test_add_new_node.sh
run ./ci/test_add_and_remove_users.sh
run ./ci/test_update_settings.sh
# run ./ci/stop_demo_cluster.sh
