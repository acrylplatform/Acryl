import AcrylDockerKeys._

enablePlugins(AcrylDockerPlugin, ItTestPlugin)

description := "NODE integration tests"
libraryDependencies ++= Dependencies.itTest

inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.acrylplatform/node-it")),
    exposedPorts := Set(6863, 6869), // NetworkApi, RestApi
    additionalFiles ++= (LocalProject("node") / Universal / stage).value +: Seq(
      (Test / resourceDirectory).value / "template.conf",
      (Test / sourceDirectory).value / "container" / "start-acryl.sh"
    )
  ))

lazy val printTests = taskKey[Unit]("Print all available Test's")

printTests := {
  val logger: Logger = ConsoleLogger()
  val tests = (definedTests in Test).value
  tests.zipWithIndex foreach { case (t, i) =>
    logger.info(s"[$i] ${t.name}")
  }
}