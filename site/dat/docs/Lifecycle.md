# Taurus Lifecycle

Stages:
 - Prepare
 - Startup
 - Check
 - Shutdown
 - Post-process

## Prepare stage
Do everything what we need to start tests, no matter how long it will take us.

Common tasks:
 - Create artifacts directory
 - Parse configs
 - Instantiate objects
 - Check if any additional software must be downloaded and installed

Mirror stage: Post-process

## Startup stage
Start test executors ASAP. Long/heavy tasks are not allowed here.

Mirror stage: Shutdown

## Check stage
Check if executors/modules have finished

Common tasks:
 - Check executor/module 
 - Update widgets (if any)
 
## Shutdown stage
Shutdown executors/modules

Mirror stage: Startup

## Post-process
Process/save results, do long post-test analysis

Mirror stage: Prepare