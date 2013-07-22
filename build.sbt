import AssemblyKeys._ // put this at the top of the file

assemblySettings

name		:= "nuvoc"

version		:= "0.1.2"

organization 	:= "io.nuvo"

homepage :=  Some(new java.net.URL("http://nuvo.io/nuvoc.html"))

scalaVersion 	:= "2.10.2"

seq(githubRepoSettings: _*)

localRepo := Path.userHome / "github" / "repo"

githubRepo := "git@github.com:nuvo-io/mvn-repo.git"


libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.2"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

autoCompilerPlugins := true

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-optimise"


scalacOptions += "-language:postfixOps"


proguardSettings

ProguardKeys.options in Proguard ++= Seq("-dontnote", "-dontwarn", "-ignorewarnings")

ProguardKeys.options in Proguard += ProguardOptions.keepMain("nuvo.compiler.NuvoC")
