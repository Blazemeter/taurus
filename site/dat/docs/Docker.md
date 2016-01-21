# Docker Image

Taurus has [Docker image](https://hub.docker.com/r/undera/bzt/) that allows you to run tool as container.

To use it, create a directory, for example `/tmp/my-test`, put all YAML configs and additional files like JMXses there, then start Docker like this:

```bash
sudo docker run --rm -v /tmp/my-test:/bzt-configs:ro undera/bzt
```

Make note that `/tmp/my-test` were passed in `-v` Docker option, it's crucial. Also note that you have to use `.yml` as config file extension. Here's [what happens](https://github.com/Blazemeter/taurus/blob/master/Dockerfile) inside container:
 1. Directory `/tmp/my-test` is mounted as `/bzt-configs`
 1. Current directory changed to `/bzt-configs`
 1. Taurus is started with following command: `bzt /bzt-configs/*.yml`


## Accessing Taurus Artifacts
If you want to receive Taurus artifacts from container, just mount some directory as `/tmp/artifacts` and files will get there. Following example gives you artifacts in `/tmp/my-run-artifacts` directory.

```bash
sudo docker run --rm -v /tmp:/bzt-configs:ro -v /tmp/my-run-artifacts:/tmp/artifacts:rw undera/bzt
```