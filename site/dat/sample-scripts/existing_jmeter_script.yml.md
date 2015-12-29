```yaml
---
execution:
- iterations: 50
  concurrency: 10
  scenario: 
    script: my-existing.jmx
    
services:
- module: fail-criteria
  criterias:
  - "avg-rt>150ms for 10s, continue as failed"
  - "fail>50% for 10s, stop as failed"
```