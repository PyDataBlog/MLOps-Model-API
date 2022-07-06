
web-graph-image:
	python -m grpc_tools.protoc -Iproto --python_out=./web_graph\
		--grpc_python_out=./web_graph/ proto/web_graph.proto
	docker build --tag guilhermesft/dwarf-web-graph-builder:latest \
		--file web_graph/Dockerfile web_graph
scraper-image:
	python -m grpc_tools.protoc -Iproto --python_out=./scraper\
		--grpc_python_out=./scraper/ proto/web_graph.proto
	docker build --tag guilhermesft/dwarf-scraper:latest --file scraper/Dockerfile scraper
