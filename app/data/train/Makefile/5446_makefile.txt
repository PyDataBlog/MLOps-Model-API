VERSION0=0.0
VERSION=$(VERSION0)

all: latest

v0:
	docker build --tag silex/sqlite:$(VERSION0) ./$(VERSION0)/

latest: v0
	docker build --tag silex/sqlite:latest $(VERSION)/
