## Metrics

`amoc_metrics` allow to configure a Prometheus exporter using the following environment variables:

* `prometheus_port` - prometheus port:
    * default value - `9090`
    * example: `AMOC_PROMETHEUS_PORT='9090'`

* `prometheus_ip` - prometheus IP:
    * default value - `"127.0.0.1"`
    * example: `AMOC_PROMETHEUS_IP='"127.0.0.1"'`

Note that they are parsed as erlang terms and so the double-quotes inside the single-quotes are
necessary.

In order to initialise some preconfigured metrics,
other applications can declare the `predefined_metrics`
environment variable (in their own `*.app.src` file):
```erl
{predefined_metrics, [{gauge, some_metric}, {histogram, another_metric}]}
```
