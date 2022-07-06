docker-images
===

Dockerfiles for own use.

mikeneck/jdk8
===

Jdk8 image with minimal spec.

### Base image

* [ubuntu:15.10](https://hub.docker.com/_/ubuntu-debootstrap/)

### installed packages

* `software-properties-common`
* `oracle-java8-installer`
* `zip`
* `unzip`
* `curl`

### added cert files

* cert of startssl
  * root - level4

mikeneck/gradle
===

Gradle with minimal spec.

### Using Java version

* jdk8u60

### Base image

* [mikeneck/jdk8](https://hub.docker.com/r/mikeneck/jdk8/)

### Gradle versions

* Latest
  * Gradle 2.7
* mikeneck/gradle:2.7
  * Gradle 2.7

### User/Group

* group
  * build
* user
  * gradle

### Environmental value

* `GRADLE_HOME`
  * `/home/gradle/gradle`
* `PATH`
  * `$PATH:$GRADLE_HOME/bin`

mikeneck/bazel
===

Bazel with minimal spec.

### Base image

* [mikeneck/jdk8](https://hub.docker.com/r/mikeneck/jdk8/)

### Java version

* jdk8u60

### Bazel versions

* Latest
  * Bazel 0.1.0
* 0.1.0
  * Bazel 0.1.0

### User/Group

* group
  * build
* user
  * bazel
