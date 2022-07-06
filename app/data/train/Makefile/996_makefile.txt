all: hadoop kafka spark elasticsearch
	npm install
elasticsearch: elasticsearch-6.1.1.tar.gz
	mkdir elasticsearch;
	tar -vxf elasticsearch-6.1.1.tar.gz -C ./elasticsearch;
elasticsearch-6.1.1.tar.gz: 
	wget https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-6.1.1.tar.gz
spark-2.2.1-bin-hadoop2.7.tgz:
	wget http://mirrors.shuosc.org/apache/spark/spark-2.2.1/spark-2.2.1-bin-hadoop2.7.tgz
spark: spark-2.2.1-bin-hadoop2.7.tgz
	mkdir spark;
	tar -vxf spark-2.2.1-bin-hadoop2.7.tgz -C ./spark;
kafka_2.11-1.0.0.tgz:
	wget http://mirrors.shuosc.org/apache/kafka/1.0.0/kafka_2.11-1.0.0.tgz
kafka: kafka_2.11-1.0.0.tgz
	mkdir kafka;
	tar -vxf kafka_2.11-1.0.0.tgz -C ./kafka;
hadoop-2.7.5.tar.gz:
	wget -c -t 0 -O hadoop-2.7.5.tar.gz http://mirrors.shuosc.org/apache/hadoop/common/hadoop-2.7.5/hadoop-2.7.5.tar.gz
hadoop: hadoop-2.7.5.tar.gz
	mkdir hadoop;
	tar -vxf hadoop-2.7.5.tar.gz -C ./hadoop;
clean:
	rm -rf node_modules
	rm -rf hadoop
	rm -rf hadoop-2.7.5.tar.gz
	rm -rf kafka
	rm -rf kafka_2.11-1.0.0.tgz
	rm -rf spark
	rm -rf spark-2.2.1-bin-hadoop2.7.tgz
	rm -rf elasticsearch
	rm -rf elasticsearch-6.1.1.tar.gz
format: clean
	find . -name "*.js" -exec js-beautify -r {} \;	 
	find . -name "*.html" -exec html-beautify -r {} \;	 
	find . -name "*.css" -exec css-beautify -r {} \;	 
