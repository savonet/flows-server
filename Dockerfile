FROM ocaml/opam

USER opam

COPY . /tmp/flows-server

WORKDIR /tmp/flows-server

RUN opam pin -n .

RUN opam list --short --recursive --external --required-by=liquidsoap-flows-server > /tmp/deps

USER root

RUN \
    cat /tmp/deps | xargs apt-get install -y --no-install-recommends geoip-bin && \
    apt-get -y autoclean && apt-get -y clean

USER opam

RUN opam install -y liquidsoap-flows-server
