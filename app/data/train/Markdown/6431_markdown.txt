# TED-XML Model (in development)

*for Simple XML framework*

[![Release](https://jitpack.io/v/juzraai/ted-xml-model.svg)](https://jitpack.io/#juzraai/ted-xml-model) [![Build Status](https://travis-ci.org/juzraai/ted-xml-model.svg?branch=master)](https://travis-ci.org/juzraai/ted-xml-model)


## About

I was searching for open data to find some **challenge** I can solve using [Kotlin](https://kotlinlang.org/) and I found out that [TED (Tenders European Daily)](http://ted.europa.eu/) has an [archive of notices](http://data.europa.eu/euodp/en/data/dataset/ted-1) in XML format.

Since I had developed [Red Flags](https://github.com/petabyte-research/redflags), I'm a bit interested in this topic so I decided to create something related.

Today I'd change a lot of things in Red Flags' implementation, and one of these is the input: using these XMLs would make **less harm** to TED's servers.

This repository contains a "POKO" (POJO - in Kotlin 🙃) model annotated with [Simple XML framework](http://simple.sourceforge.net/)'s annotations, so you can **read/parse a TED-XML file with a single line** into an OO model.

**Why not JAXB?** Simple: because the XJC generator dies from R2.0.8 version of TED-XML schema and I couldn't make it work.

Of course, the structure of the XML and therefore this model **may need some transformation** if you want to work with the data, but this model provides you a kickstart.

I'm also planning to create a model on TED's CODIF data and other resources that one may need in projects related to public procurements.



## Features

* Provides you a one-liner to parse a TED-XML (see below)
* Written in Kotlin - runs on JVM, fully Java compatible
* Maven project - you can import it easily (see below)
* It also imports Simple XML framework
* There are no `null` values in the model (and if you are using Kotlin, there will never be!) - you can safely write getter chains
* *(Planning to be)* Compatible with both TED-XML schema versions being used currently (R2.0.9 and R2.0.8, see table below)

Note that this model is **only for parsing TED XMLs** and NOT for validating or generating them.



## Progress / Compatibility

| Section               | R2.0.9 | R2.0.8 |
|:----------------------|:------:|:------:|
| `TECHNICAL_SECTION`   |   ✔   |       |
| `LINKS_SECTION`       |   ✔   |   ✔   |
| `SENDER`              |   ✔   |   ✔   |
| `CODED_DATA_SECTION`  |   ✔   |       |
| `TRANSLATION_SECTION` |   ✔   |       |
| `FORM_SECTION`        |       |        |



## Usage

The easiest way to use **ted-xml-model** is adding it as dependency from [JitPack.io](https://jitpack.io/#juzraai/ted-xml-model). Follow the link to get information on how to do this, click on the green "Get it" button besides the latest version.

**Calling from Kotlin:**

```kotlin
val tedExport = TedXmlModel.parse(/* File or InputStream or resource name */)
```

**Calling from Java:**

```java
TedExport tedExport = TedXmlModel.parse(/* File or InputStream or resource name */);
```



## How to contribute

* If you find a bug, please open an issue
* If you have time, you can write unit tests for each element, for both schema versions
