%~d0
 cd %~dp0
 java -Xms256M -Xmx1024M -cp ../lib/advancedPersistentLookupLib-1.0.jar;../lib/commons-collections-3.2.jar;../lib/dom4j-1.6.1.jar;../lib/jboss-serialization.jar;../lib/log4j-1.2.15.jar;../lib/mysql-connector-java-5.1.30-bin.jar;../lib/trove.jar;../lib/systemRoutines.jar;../lib/userRoutines.jar;.;passageannee_0_1.jar; projetdemo.passageannee_0_1.passageannee --context=Default %* 