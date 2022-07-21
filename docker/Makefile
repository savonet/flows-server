all:

build:
	docker build -t flows-server --network=host .

start:
	docker run -d \
	  -p 127.0.0.1:15000:8080/tcp \
	  --storage-opt size=2G --memory=2G --cpus=1 \
	  --log-driver=journald \
	  --restart=always \
	  --name=flows-server \
	  flows-server

stop:
	-docker stop flows-server
	-docker rm flows-server

restart: stop start

test: build
	-docker rm flows-server
	docker run -ti --rm \
	  -p 127.0.0.1:8080:8080 \
	  --name=flows-server \
	  flows-server
