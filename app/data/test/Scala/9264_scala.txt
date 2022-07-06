package com.blackboxsociety

import sbt._
import Keys._

object BlackBoxPlugin extends Plugin {

  val blackBoxSettings = Seq(
    resolvers += Resolver.url(
      "bintray-sbt-plugin-releases",
      url("http://dl.bintray.com/content/sbt/sbt-plugin-releases")
    )(Resolver.ivyStylePatterns),
    resolvers += "softprops-maven" at "http://dl.bintray.com/content/softprops/maven",
    resolvers += "Black Box Society Repository" at "http://dl.bintray.com/blackboxsociety/releases",
    libraryDependencies += "com.blackboxsociety" %% "blackbox-http" % "0.2.0"
  )

  val blackBoxPlugins = Seq(
    addSbtPlugin("io.spray" % "sbt-revolver" % "0.7.1"),
    addSbtPlugin("me.lessis" % "less-sbt" % "0.2.2"),
    addSbtPlugin("org.scala-sbt" % "sbt-closure" % "0.1.4"),
    addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.11.2")
  )

}
