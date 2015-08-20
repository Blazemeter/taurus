# Services Subsystem

When you need to perform some actions before test starts, after test starts, or in parallel with running test, you should use services subsystem in Taurus. To configure services, use `services` top-level section of config with list of service module configs:

```yaml
---
services:
  - shellexec:
      prepare: ls -la .
```

## Shell Executor Service Module

...