import sbt._

object Dependencies {
  lazy val appDependencies = Seq(
    "com.typesafe.slick"      %% "slick"            % "3.2.1",
    "com.typesafe.akka"       %% "akka-http"        % "10.0.9",
    "joda-time"               % "joda-time"         % "2.9.9"
  )


  lazy val testDependencies = Seq(
    "org.scalatest"           %% "scalatest"        % "3.0.1" % Test
  )
}
