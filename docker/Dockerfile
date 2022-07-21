FROM debian:stable-slim

RUN apt-get -y update

RUN apt-get -y install build-essential git ocaml-nox dune opam pkg-config

WORKDIR /home

RUN opam init -y --disable-sandboxing

# RUN opam switch -y create 4.14.0

RUN apt-get -y install libgmp-dev geoip-bin

RUN opam install -y cohttp-lwt-unix ppx_yojson_conv ppx_stable sha yojson

RUN git clone https://github.com/savonet/flows-server.git

RUN cd flows-server/src && wget https://mailfud.org/geoip-legacy/GeoIPCity.dat.gz && gunzip GeoIPCity.dat.gz

RUN cd flows-server && eval $(opam env) && make run
