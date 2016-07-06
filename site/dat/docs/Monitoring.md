# Resource Monitoring Service

A frequest task for tests is to monitor target server's health. Monitoring service is built to collect data from those remote servers. At this time followind sources are supported:
 - local health stats, enabled by default
 - [ServerAgent](http://jmeter-plugins.org/wiki/PerfMonAgent/) - technology that is used by JMeter users for long time and
 - [Graphite](https://graphite.readthedocs.org/en/latest/).
Also you can use `local` monitoring (enabled by default) for check tester system state.

### Local Monitoring Stats

Following metrics are collected locally: 
- `cpu` - total CPU usage %
- `mem` - total RAM usage %
- `bytes-sent`/`bytes-recv` - network transfer rate 
- `disk-read`/`disk-write` - disk I/O rate
- `disk-space` - % disk space used for artifacts storage
- `engine-loop` - Taurus "check loop" utilization, values higher than 1.0 means you should increase `settings.check-interval`

```yaml
---
services:
- module: monitoring
  local:
  - metrics:
    - cpu
    - disk-space
    - engine-loop
```
  
### Using ServerAgent To Collect Monitoring Stats From Server
 
Shortly, you need to unzip and launch small Java server on each of your target servers and then specify [metrics](http://jmeter-plugins.org/wiki/PerfMonMetrics/) to collect under `services` item. For example: 
```yaml
---
services:
- module: monitoring
  server-agent:
  - address: 127.0.0.1:4444
    metrics:
    - cpu
    - disks
    - memory

```

### Graphite 

Graphite data source uses graphite The Render URL API to receive metrics.
In this example you can see usage optional server `label`, `timeout` for graphite answers, `interval` between requests and interesting graphite data range definition with parameters `from`/`until`.
```yaml
---
services:
- module: monitoring
  graphite:
  - address: 192.168.0.38
    interval: 5s
    from: 100s
    until: 1s
    timeout: 2s
    metrics:
    - store.memUsage
    - test.param1
  - address: local_serv:2222
    label: test_serv
    metrics:
    - production.hardware.cpuUsage
    - groupByNode(myserv_comp_org.cpu.?.cpu.*.value, 4, 'avg')
``` 

### Sidebar Widget

Once you have resource monitoring enabled, you'll be presented with small sidebar widget that informs you on latest data from monitoring agents:

[](monitoring-widget.png)

The widget will possibly not display all the metrics for the long list, that's the limitation of screen height :)

