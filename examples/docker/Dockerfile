FROM python

RUN pip install bzt && bzt --help

RUN echo "settings:\n  default-executor: apiritif" > /root/.bzt-rc

WORKDIR /tmp
ENTRYPOINT ["bzt"]
