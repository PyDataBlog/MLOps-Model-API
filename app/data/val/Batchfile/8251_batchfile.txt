POST /oauth2/token HTTP/1.1Host: account.api.here.com Authorization: OAuth [oauth1.0signature header]Content-Type: application/x-www-form-urlencodedgrant_type=client_credentials

Automotive host: cc.account.api.here.com

aws emr add-steps --cluster-id j-2AXXXXXXGAPLF --steps Type=CUSTOM_JAR,Name="Spark Program",Jar="command-runner.jar",ActionOnFailure=CONTINUE,Args=[spark-example,SparkPi,10]

spark-submit --deploy-mode cluster --master yarn --conf spark.driver.userClassPathFirst=true --conf spark.executor.userClassPathFirst=true --conf spark.executor.extraJavaOptions=-Dlog4j.configuration=file:///mnt/log4j.properties --conf spark.driver.extraJavaOptions=-Dlog4j.configuration=file:///mnt/log4j.properties --class com.here.acs.mlp.tsr.TSRApplication s3://here-tsr-eu-west-1/deploy/cit/mlp/1.2.2-20170303-167/jars/tsr-jar-with-dependencies.jar -b here-tsr-eu-west-1 -k deploy/cit/mlp/1.2.2-20170303-167/conf/tsr_test.yaml -run_extract false


aws emr add-steps --cluster-id j-2AXXXXXXGAPLF --steps Type=Spark,Name="Spark Program",ActionOnFailure=CONTINUE,Args=[--class,org.apache.spark.examples.SparkPi,/usr/lib/spark/lib/spark-examples.jar,10]



start="$(date  --date="2 days ago" +%Y-%m-%d)"
end="$(date  --date="1 day ago" +%Y-%m-%d)"
today="$(date  --date="today" +%Y-%m-%d)"

destination_path="s3://here-tsr-eu-west-1/deploy/cit/mlp/sdip-extracts/"$today

curl -v -H "Content-Type: application/json" -X POST -d '{"sourcePath":"s3://here-be4das-cit-sda/cit-jEsB8EAZdycNXYvqS7u3","destinationPath":"'$destination_path'","roleName":"arn:aws:iam::712144602929:role/sdip-extract-here-internal","format":"protobuf","filters":[{"type":"dateRange","field":"receivedDay","start":"'$start'","end":"'$end'"}]}' "http://extract.sdip.cit.olp.api.here.com/sdip/v2/extractService/extract?app_id=WVRa0toPBWz2TE71bf5N&app_code=_F7RfSFUQKTR4SvOpOPuJg"