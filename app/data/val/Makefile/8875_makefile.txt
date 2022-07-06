CONTAINER_NAME=heartbleed

IMAGE_NAME=hackhuntsville/heartbleed
IMAGE_VERSION=latest

build:
	docker build -t $(IMAGE_NAME):$(IMAGE_VERSION) .
	docker build -t $(IMAGE_NAME):latest .

enter:
	sudo nsenter --target `docker inspect --format {{.State.Pid}} $(CONTAINER_NAME)` --mount --uts --ipc --net --pid

run:
	docker run -d --name $(CONTAINER_NAME) \
		-p 80:80 \
		-p 443:443 \
		-v /var/docker/heartbleed/etc/nginx/sites-enabled:/etc/nginx/sites-enabled \
		-v /var/docker/heartbleed/etc/nginx/ssl:/etc/nginx/ssl \
		-v /var/docker/heartbleed/var/log/nginx:/var/log/nginx \
		-v /var/docker/heartbleed/usr/share/nginx/www:/usr/share/nginx/www \
		$(IMAGE_NAME):$(IMAGE_VERSION)

deploy:
	docker push $(IMAGE_NAME):$(IMAGE_VERSION)
	docker push $(IMAGE_NAME):latest	
