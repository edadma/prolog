name := "prolog"

version := "0.2"

scalaVersion := "2.12.8"

//crossScalaVersions := Seq( "2.11.11" )

scalacOptions ++= Seq( "-deprecation", "-feature", "-unchecked", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

organization := "xyz.hyperreal"

//resolvers += Resolver.sonatypeRepo( "snapshots" )

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.5" % "test",
	"org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

libraryDependencies ++= Seq(
//	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
//	"org.scala-lang.modules" %% "scala-xml" % "1.0.6"
//	"org.scala-lang.modules" %% "scala-swing" % "2.0.3"
)

//libraryDependencies ++= Seq(
//  "com.typesafe" % "config" % "1.3.3"
//)

libraryDependencies ++= Seq(
  "jline" % "jline" % "2.14.6"
)

libraryDependencies ++= {
	val akkaV = "2.5.12"
	Seq(
//		"com.typesafe.akka" %% "akka-actor"    % akkaV
//		"com.typesafe.akka" %% "akka-remote"   % akkaV,
//		"com.typesafe.akka" %% "akka-testkit"  % akkaV    % "test",
//		"org.specs2"        %% "specs2-core"   % "2.3.11" % "test"
	)
}

//libraryDependencies ++= {
//	val akka_http = "10.1.1"
//	Seq(
//		"com.typesafe.akka" %% "akka-http-core"       % akka_http,
//		"com.typesafe.akka" %% "akka-http"            % akka_http,
//		"com.typesafe.akka" %% "akka-http-testkit"    % akka_http,
//		"com.typesafe.akka" %% "akka-http-spray-json" % akka_http,
//		"com.typesafe.akka" %% "akka-http-jackson"    % akka_http
//	)
//}

libraryDependencies ++= Seq(
  "xyz.hyperreal" %% "pattern-matcher" % "0.2.13",
  "xyz.hyperreal" %% "lia" % "0.22.2",
  "xyz.hyperreal" %% "args" % "0.1"
)

coverageExcludedPackages := ".*Main"

mainClass in (Compile, run) := Some( "xyz.hyperreal." + name.value.replace('-', '_') + ".Main" )

mainClass in assembly := Some( "xyz.hyperreal." + name.value.replace('-', '_') + ".Main" )

assemblyJarName in assembly := name.value + "-" + version.value + ".jar"

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))

homepage := Some(url("https://github.com/edadma/" + name.value))

pomExtra :=
  <scm>
    <url>git@github.com:edadma/{name.value}.git</url>
    <connection>scm:git:git@github.com:edadma/{name.value}.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>https://github.com/edadma</url>
    </developer>
  </developers>
