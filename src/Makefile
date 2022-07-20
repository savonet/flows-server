all: run

run:
	# @if [ ! -e GeoLite2-City.mmdb ]; then wget https://github.com/P3TERX/GeoLite.mmdb/raw/download/GeoLite2-City.mmdb; fi
	@if [ ! -e GeoIPCity.dat ]; then wget "https://github.com/maxmind/geoip-api-php/blob/main/tests/data/GeoIPCity.dat?raw=true" -O GeoIPCity.dat; fi
	@dune exec ./flows.exe

build:
	@dune build
