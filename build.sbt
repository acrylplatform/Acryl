/* IDEA notes
 * May require to delete .idea and re-import with all checkboxes
 * Worksheets may not work: https://youtrack.jetbrains.com/issue/SCL-6726
 * To work with worksheets, make sure:
   1. You've selected the appropriate project
   2. You've checked "Make project before run"
 */

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import Hashes.mk
import org.apache.commons.codec.binary.Hex
import sbt.Keys._
import sbt._
import sbt.io.Using
import sbt.internal.inc.ReflectUtilities
import sbtcrossproject.CrossPlugin.autoImport.crossProject

lazy val common = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .disablePlugins(ProtocPlugin)
  .settings(
    libraryDependencies ++= Dependencies.common.value,
    coverageExcludedPackages := ""
  )

lazy val commonJS  = common.js
lazy val commonJVM = common.jvm

lazy val versionSourceTask = (path: String) =>
  Def.task {
    // WARNING!!!
    // Please, update the fallback version every major and minor releases.
    // This version is used then building from sources without Git repository
    // In case of not updating the version nodes build from headless sources will fail to connect to newer versions
    val FallbackVersion = (1, 1, 2)

    val versionFile      = sourceManaged.value / "com" / "acrylplatform" / "Version.scala"
    val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r
    val (major, minor, patch) = version.value match {
      case versionExtractor(ma, mi, pa) => (ma.toInt, mi.toInt, pa.toInt)
      case _                            => FallbackVersion
    }
    IO.write(
      versionFile,
      s"""package $path
       |
       |object Version {
       |  val VersionString = "${version.value}"
       |  val VersionTuple = ($major, $minor, $patch)
       |}
       |""".stripMargin
    )
    Seq(versionFile)
}

lazy val versionSourceSetting = (path: String) => inConfig(Compile)(Seq(sourceGenerators += versionSourceTask(path)))

lazy val lang =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .disablePlugins(ProtocPlugin)
    .dependsOn(common % "compile;test->test")
    .settings(
      coverageExcludedPackages := ".*",
      test in assembly := {},
      libraryDependencies ++= Dependencies.lang.value ++ Dependencies.test,
      resolvers += Resolver.bintrayIvyRepo("portable-scala", "sbt-plugins"),
      resolvers += Resolver.sbtPluginRepo("releases")
      // Compile / scalafmt / sourceDirectories += file("shared").getAbsoluteFile / "src" / "main" / "scala" // This doesn't work too
    )

lazy val langJS  = lang.js.settings(versionSourceSetting("com.acrylplatform.lang"))
lazy val langJVM = lang.jvm

lazy val node = project
  .dependsOn(
    commonJVM % "compile;test->test",
    langJVM   % "compile;test->test"
  )
  .settings(versionSourceSetting("com.acrylplatform"))

lazy val `grpc-server` = project
  .dependsOn(node % "compile;test->test;runtime->provided")

lazy val `node-it` = project.dependsOn(node)

lazy val `node-generator` = project.dependsOn(node, `node-it` % "compile->test")

lazy val benchmark = project
  .dependsOn(
    node    % "compile;test->test",
    langJVM % "compile;test->test"
  )

lazy val it = project
  .settings(
    description := "Hack for near future to support builds in TeamCity for old and new branches both",
    Test / test := Def
      .sequential(
        root / packageAll,
        `node-it` / Docker / docker,
        `node-it` / Test / test
      )
      .value
  )

lazy val root = (project in file("."))
  .aggregate(
    commonJS,
    commonJVM,
    langJS,
    langJVM,
    node,
    `node-it`,
    `node-generator`,
    benchmark
  )

inScope(Global)(
  Seq(
    scalaVersion := "2.12.8",
    organization := "com.acrylplatform",
    organizationName := "Acryl Platform",
    organizationHomepage := Some(url("https://acrylplatform.com")),
    scmInfo := Some(ScmInfo(url("https://github.com/acrylplatform/Acryl"), "git@github.com:acrylplatform/Acryl.git", None)),
    licenses := Seq(("MIT", url("https://github.com/acrylplatform/Acryl/blob/master/LICENSE"))),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:postfixOps",
      "-Ywarn-unused:-implicits",
      "-Xlint",
      "-Ypartial-unification",
      "-opt:l:inline",
      "-opt-inline-from:**"
    ),
    crossPaths := false,
    scalafmtOnCompile := false,
    dependencyOverrides ++= Dependencies.enforcedVersions.value,
    cancelable := true,
    logBuffered := false,
    coverageExcludedPackages := ".*",
    parallelExecution := false,
    testListeners := Seq.empty, // Fix for doubled test reports
    /* http://www.scalatest.org/user_guide/using_the_runner
     * o - select the standard output reporter
     * I - show reminder of failed and canceled tests without stack traces
     * D - show all durations
     * O - drop InfoProvided events
     * F - show full stack traces
     * u - select the JUnit XML reporter with output directory
     */
    testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports"),
    testOptions += Tests.Setup(_ => sys.props("sbt-testing") = "true"),
    concurrentRestrictions := {
      val threadNumber = Option(System.getenv("SBT_THREAD_NUMBER")).fold(1)(_.toInt)
      Seq(Tags.limit(Tags.ForkedTestGroup, threadNumber))
    },
    network := Network(sys.props.get("network"))
  ))

// ThisBuild options
git.useGitDescribe := true
git.uncommittedSignifier := Some("DIRTY")

// root project settings
// https://stackoverflow.com/a/48592704/4050580
def allProjects: List[ProjectReference] = ReflectUtilities.allVals[Project](this).values.toList map { p =>
  p: ProjectReference
}

lazy val cleanAll = taskKey[Unit]("Clean all projects")
cleanAll := clean.all(ScopeFilter(inProjects(allProjects: _*), inConfigurations(Compile))).value

lazy val packageAll = taskKey[Unit]("Package all artifacts")
packageAll := Def
  .sequential(
    root / cleanAll,
    Def.task {
      val artifacts = Seq(
        (node / assembly).value,
        (node / Debian / packageBin).value,
        (`grpc-server` / Universal / packageZipTarball).value,
        (`grpc-server` / Debian / packageBin).value
      )

      val destDir = ((root / target).value / "release").toPath
      Files.createDirectories(destDir)

      val hashes = artifacts.map { file =>
        val xs = Using.fileInputStream(file)(mk("SHA-256", _))
        file.toPath.getFileName.toString -> Hex.encodeHexString(xs)
      }

      // takeWhile to sort network builds
      val sortedHashes = hashes
        .sortBy { case (fileName, _) => (fileName.takeWhile(x => !x.isDigit).length, fileName) }

      val content =
        s"""## SHA256 Checksums
           |
           |```
           |${sortedHashes.map { case (fileName, hash) => s"$hash $fileName" }.mkString("\n")}
           |```
           |""".stripMargin

      val releaseNotesFile = destDir.resolve("checksums.md").toFile
      IO.write(releaseNotesFile, content, StandardCharsets.UTF_8)

      artifacts
        .map(_.toPath)
        .foreach { source =>
          Files.move(source, destDir.resolve(source.getFileName))
        }
    }
  )
  .value

lazy val checkPRRaw = taskKey[Unit]("Build a project and run unit tests")
checkPRRaw := {
  try {
    cleanAll.value // Hack to run clean before all tasks
  } finally {
    test.all(ScopeFilter(inProjects(commonJVM, langJVM, node), inConfigurations(Test))).value
    (commonJS / Compile / fastOptJS).value
    (langJS / Compile / fastOptJS).value
    compile.all(ScopeFilter(inProjects(`node-generator`, benchmark), inConfigurations(Test))).value
  }
}

def checkPR: Command = Command.command("checkPR") { state =>
  val updatedState = Project
    .extract(state)
    .appendWithoutSession(Seq(Global / scalacOptions ++= Seq("-Xfatal-warnings", "-Ywarn-unused:-imports")), state)
  Project.extract(updatedState).runTask(root / checkPRRaw, updatedState)
  state
}

commands += checkPR
