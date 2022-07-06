# techtalk-java9-example
Examples of my TechTalk on Java 9 Modules

## Prerequsistes
* [JDK 9 Early Access with Project Jigsaw](https://jdk9.java.net/jigsaw/) (build 145 is used for the examples)
* Optionally a bash shell to run the build commands

## Example 1: Modules
This example consist of a java package containing a ```LeftPad``` (leftpad) utilty class 
and another package containing a simple application using that utility (application).

The simplest form of Java 9 module descriptors are added to these traditional Java packages: 
the leftpad module exports the package containing the utility class and the application module 
declares that it requires the leftpad module.

*1. Compile without modules and run with classic classpath*
```
$ cd modules
$ javac -d target $(find ./src -name '[A-Z]*.java')
$ java -cp target  com.github.pellaton.java9.application.Main
```
  
*2. Compile as Java 9 modules and run with module path*
```
$ cd modules
$ javac --module-source-path src -d target $(find . -name '*.java')
$ java --module-path target -m application/com.github.pellaton.java9.application.Main
```

##Example 2: Servcies
In this example, the ```LeftPad``` utility class was changed to be a service having an interface 
in an API package and an implementation in a separate package. The module only exports the
API package therefore hiding the implementation class from other modules. To facilitate the
use of the PadLeft service in other modules, the service is exported.

The application module was changed to lookup the PadLeft service of the other module using 
the ```ServiceLocator```.  

*Compile as Java 9 modules and run with module path*
```
$ cd services
$ javac --module-source-path src -d target $(find . -name '*.java')
$ java --module-path target -m application/com.github.pellaton.java9.application.Main
```
