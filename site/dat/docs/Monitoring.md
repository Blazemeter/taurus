# Resource Monitoring Service

A frequent task for tests is to monitor target server's health. Monitoring service is built
to collect data from those remote servers. At this time the following sources are supported:
 - local health stats, enabled by default
 - [ServerAgent](http://jmeter-plugins.org/wiki/PerfMonAgent/) — technology that is used by JMeter users for long time and
 - [Graphite](https://graphite.readthedocs.org/en/latest/)

## Local Monitoring Stats

This service collects local health stats from computer running Taurus. It is enabled by default.

Following metrics are collected locally: 
- `cpu` - total CPU usage %
- `mem` - total RAM usage %
- `bytes-sent`/`bytes-recv` - network transfer rate 
- `disk-read`/`disk-write` - disk I/O rate
- `disk-space` - % disk space used for artifacts storage
- `engine-loop` - Taurus "check loop" utilization, values higher than 1.0 means you should increase `settings.check-interval`
- `conn-all` - quantity of network connections

If you want to use only your metrics, please look into 
[merging rules](https://gettaurus.org/docs/ConfigSyntax/#Multiple-Files-Merging-Rules). For example, if you want to see
only specific metrics, use `~` like in the example below. You can also define, whether or not you need logs for local
monitoring via `logging` option.

```yaml
services:
- module: monitoring
  ~local:
  - interval: 20s   # polling interval
    logging: True # local monitoring logs will be saved to "local_monitoring_logs.csv" in the artifacts dir
    metrics:
    - cpu
    - disk-space
    - engine-loop
```

## Sidebar Widget

Once you have resource monitoring enabled, you'll be presented with small sidebar widget that
informs you on latest data from monitoring agents:

[](monitoring-widget.png)

The widget will possibly not display all the metrics for the long list, that's the limitation of
screen height :)

## ServerAgent

[ServerAgent](http://jmeter-plugins.org/wiki/PerfMonAgent/) is a small Java application that
collects server health stats and makes them accessible through network connection. To use it,
you need to install and launch ServerAgent on each of your target servers and then specify
[metrics](http://jmeter-plugins.org/wiki/PerfMonMetrics/) to collect under `services` item.
You can also define, whether or not you need logs for ServerAgent via `logging` option.
For example: 
```yaml
services:
- module: monitoring
  server-agent:
  - address: 192.168.0.1:4444
    label: target-server  # if you specify label, it will be used in reports instead of ip:port
    interval: 3s    # polling interval
    logging: True # those logs will be saved to "SAlogs_192.168.0.1_4444.csv" in the artifacts dir
    metrics:
    - cpu
    - disks
    - memory

```

## Graphite 

Graphite data source uses graphite The Render URL API to receive metrics.
In this example you can see usage optional server `label`, `timeout` for graphite answers, `interval`
between requests and interesting graphite data range definition with parameters `from`/`until`. You can also define, 
whether or not you need logs for Graphite monitoring via `logging` option.
```yaml
services:
- module: monitoring
  graphite:
  - address: 192.168.0.38
    interval: 5s
    from: 100s
    until: 1s
    timeout: 2s
    logging: True # those logs will be saved to "Graphitelogs_192.168.0.38.csv" in the artifacts dir
    metrics:
    - store.memUsage
    - test.param1
  - address: local_serv:2222
    label: test_serv
    metrics:
    - production.hardware.cpuUsage
    - groupByNode(myserv_comp_org.cpu.?.cpu.*.value, 4, 'avg')
```
