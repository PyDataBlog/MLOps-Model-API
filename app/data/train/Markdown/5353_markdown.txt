GoJS
====

GoJS is a project to produce a compiler which can produce efficient and light Javascript from Go code, or at least a subset of it, using Go’s toolchain to document and check the integrity of code.

###Why?

As a full-stack developer, I missed Go’s expressiveness, its descriptiveness and its lightness when writing Javascript code. I also believe in producing technologies that make it just as easy, if not easier to produce fast and light code to send to users. Closure compiler goes some of the way, but it does not at all change the syntax of the language, leaving the developer to write extensive comment blocks to indicate information about the nature of the code - and even then, a developer may be penalised in compilation efficiency as a result of writing code in a way that makes more sense to them.

####Doesn't Dart solve some of these problems?

Dart seemed a very interesting language, but the Javascript it generated was not efficient enough and its features not extensive enough to justify writing in it over Javascript.

###How?

GoJS aims to produce heavily annotated Javascript code for Closure Compiler - all the information it requires is available inherently in Go’s syntax.

####How are you going to use the Go toolchain? Surely `document` does not exist in Go?

Go requires explicit definitions of values in order for `go build` to function, to solve this, a go program `extern` is used to parse and convert Closure Compiler’s extern files into Go packages using the `text/template` package, carrying with it all the documentation that is in the JSDoc.

A note is made of how the functions, variables and constants in the package are transformed into Javascript code, which is stored as a comment in the package code.

This means that to use GoJS, the packages that correspond to the externs that you need to use are imported, the developer writes Go in the way he is used to, can convert the code to annotated Javascript using GoJS, and when production code is needed, Closure Compiler can be used on the Javascript.
