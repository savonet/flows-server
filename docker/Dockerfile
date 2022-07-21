FROM debian:stable-slim

RUN apt-get -y update

RUN apt-get -y install build-essential git ocaml-nox dune opam geoip-bin

WORKDIR /home

RUN opam init -y --disable-sandboxing

RUN opam switch -y create 4.14.0

RUN apt-get -y install pkg-config

RUN opam install -y cohttp-lwt-unix ppx_yojson_conv ppx_stable sha yojson

RUN git clone https://github.com/savonet/flows-server.git

RUN cd src && wget https://mailfud.org/geoip-legacy/GeoIPCity.dat.gz && gunzip GeoIPCity.dat.gz

RUN eval $(opam env) && make run
