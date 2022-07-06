docker exec test rm -f /tmp/jenkins-cli.jar
docker exec test wget -O /tmp/jenkins-cli.jar http://localhost:8080/jnlpJars/jenkins-cli.jar
docker exec test java -jar /tmp/jenkins-cli.jar -s http://localhost:8080 install-plugin git
docker exec test java -jar /tmp/jenkins-cli.jar -s http://localhost:8080 install-plugin sonar -restart
