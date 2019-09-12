coverageExcludedPackages := ""
publishMavenStyle := true
credentials += Credentials(Path.userHome / ".sbt" / ".credentials")
publishTo := Some("Sonatype Nexus" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
name := "RIDE Compiler"
normalizedName := "lang"
description := "The RIDE smart contract language compiler"
homepage := Some(url("https://docs.acrylplatform.com/en/technical-details/acryl-contracts-language-description/maven-compiler-package.html"))
developers := List(Developer("petermz", "Peter Zhelezniakov", "peterz@rambler.ru", url("https://acrylplatform.com")))
libraryDependencies ++=
  Seq(
    "org.scala-js"                      %% "scalajs-stubs" % "1.0.0-RC1" % Provided,
    "com.github.spullara.mustache.java" % "compiler"       % "0.9.5"
  )
