#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

docker_compose up --wait --wait-timeout 100 amoc-{master,worker-1,worker-2} prometheus grafana

## configure default grafana datasource
json=( '{'
       '"name": "prometheus",'
       '"access": "proxy",'
       '"type": "prometheus",'
       '"url": "http://prometheus:9090",'
       '"isDefault": true'
       '}' )

curl 'http://admin:admin@localhost:3000/api/datasources' -X POST \
     -H 'Content-Type: application/json;charset=UTF-8' \
     -d "${json[*]}" -w "\n%{response_code}\n"
