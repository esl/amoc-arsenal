#!/bin/bash

###############################
## general purpose functions ##
###############################
function enable_strict_mode() {
    # the below settings are based on:
    #    http://redsymbol.net/articles/unofficial-bash-strict-mode/
    set -euo pipefail
    IFS=$'\n\t'
}

function contains() {
    local output="$(cat -)"
    local ret= acc=0
    for pattern in "$@"; do
        ret="$(echo "$output" | grep -q -e "$pattern"; echo "$?")"
        if [ "$ret" -ne "0" ]; then
            [ "$(($acc))" -eq "0" ] && {
                echo "contains FAILED"
                echo "output: '${output}'"; }
            echo "pattern is missing: '${pattern}'"
        fi >&2
        acc+="+${ret}"
    done
    test "$(($acc))" "-eq" "0"
}

function retry() {
    local n="$1" m="0"
    local output=
    shift 1
    echo "waiting for '$@'"
    until echo -n "." && output="$("$@" 2>&1)"; do
        [ "$n" -gt "$m" ] || {
            echo -e "\nfailed after '$m' retries\nlast output"
            echo "${output}"
            return 1; }
        sleep 1
        m="$(( m + 1 ))"
    done
    echo -e "\nsuccess after '$m' retries";
}

######################
## docker functions ##
######################
function amoc_container_port() {
    local service="$1"
    case "$service" in
       amoc-master)
         echo 4000 ;;
       amoc-worker-[0-9])
         echo "400${service#amoc-worker-}" ;;
       *)
         return 1 ;;
    esac
}

function docker_compose() {
    local compose_file="${git_root}/ci/docker-compose.yml"
    docker compose -p "amoc-demo-cluster" -f "$compose_file" "$@"
}

#############################
## amoc REST API functions ##
#############################
function get_nodes() {
    local servise="$1"
    port="$(amoc_container_port "$servise")"
    curl -s -X GET "http://localhost:${port}/nodes" -H  "accept: application/json"
}

function get_status() {
    local servise="$1"
    local port="$(amoc_container_port "$servise")"
    curl -s -X GET "http://localhost:${port}/status" -H  "accept: application/json"
}

#################################
## graphite REST API functions ##
#################################
function metrics_reported() {
    curl -s "http://localhost:8080/metrics/find?query=*" | contains "$@"
}

function wait_for_metrics() {
     retry 60 metrics_reported "$@"
}

######################
## common variables ##
######################
git_root="$(git rev-parse --show-toplevel)"
