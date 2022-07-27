FROM ocaml/opam

USER opam

WORKDIR $HOME/flows-server

COPY liquidsoap-flows-server.opam $HOME/flows-server/liquidsoap-flows-server.opam

RUN opam pin -n .

RUN opam list --short --recursive --external --required-by=liquidsoap-flows-server > /tmp/deps

USER root

RUN \
    cat /tmp/deps | xargs apt-get install -y --no-install-recommends geoip-bin && \
    apt-get -y autoclean && apt-get -y clean

USER opam

RUN opam list --short --required-by=liquidsoap-flows-server | xargs opam install -y

COPY . $HOME/flows-server

RUN opam install -y liquidsoap-flows-server
