	mvn archetype:generate ^
	  -DarchetypeGroupId=org.codehaus.mojo.archetypes ^
	  -DarchetypeArtifactId=pom-root ^
	  -DarchetypeVersion=1.1 ^
	  -DgroupId=net.eost.example.osgi ^
	  -DartifactId=jdbc ^
	  -DinteractiveMode=false ^
	  -Dversion=1.0-SNAPSHOT