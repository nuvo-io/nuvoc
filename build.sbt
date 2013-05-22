import AssemblyKeys._ // put this at the top of the file

assemblySettings

name		:= "nuvoc"

version		:= "0.1.1-SNAPSHOT"

organization 	:= "io.nuvo"

homepage :=  Some(new java.net.URL("http://nuvo.io/nuvoc.html"))

scalaVersion 	:= "2.10.1"

seq(githubRepoSettings: _*)

localRepo := Path.userHome / "github" / "repo"

githubRepo := "git@github.com:nuvo-io/mvn-repo.git"


libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

//libraryDependencies += "org.specs2" % "specs2_2.10.1" % "1.12.3"

//libraryDependencies += "org.slf4j" % "slf4j-api" % "1.6.4"

// libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.6.4"

//libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.1"

//libraryDependencies += "org.scalatest" % "scalatest_2.10.0" % "1.8-B1"

autoCompilerPlugins := true

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-optimise"


scalacOptions += "-language:postfixOps"

