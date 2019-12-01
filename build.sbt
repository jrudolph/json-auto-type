val scalaV = "2.13.1"
val akkaV = "2.6.0"
val akkaHttpV = "10.1.10"
val sprayJsonV = "1.3.5"
val upickleV = "0.8.0"
val utestV = "0.7.1"
val scalaJsDomV = "0.9.7"
val scalaTestV = "3.0.8"

lazy val root =
  project.in(file("."))
    .aggregate(logic, frontend, web)

lazy val logic =
  project.in(file("logic"))
    .settings(commonSettings: _*)
    .settings(
      libraryDependencies ++= Seq(
        "io.spray" %% "spray-json" % sprayJsonV,
        "org.scalatest" %% "scalatest" % scalaTestV % "test",
      ),
    )

// Scala-Js frontend
lazy val frontend =
  project.in(file("frontend"))
    .enablePlugins(ScalaJSPlugin)
    .settings(commonSettings: _*)
    .settings(
      scalaJSUseMainModuleInitializer := true,
      testFrameworks += new TestFramework("utest.runner.Framework"),
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value, // needed not to fail in frontend/updateClassifiers
        "org.scala-js" %%% "scalajs-dom" % scalaJsDomV,
        "com.lihaoyi" %%% "utest" % utestV % "test",
      )
    )

// Akka Http based backend
lazy val web =
  project.in(file("web"))
    .dependsOn(logic)
    .enablePlugins(SbtTwirl, BuildInfoPlugin, ParadoxMaterialThemePlugin)
    .settings(commonSettings: _*)
    .settings(
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-stream" % akkaV,
        "com.typesafe.akka" %% "akka-http" % akkaHttpV,
        "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpV,
        "org.scalatest" %% "scalatest" % scalaTestV % "test",
      ),
      resourceGenerators in Compile += Def.task {
        val f1 = (fastOptJS in Compile in frontend).value.data
        Seq(f1, new File(f1.getPath+".map"))
      }.taskValue,
      watchSources ++= (watchSources in frontend).value,

      buildInfoPackage := "example.akkawschat",
      buildInfoKeys ++= Seq(
        "longProjectName" -> "Example Project"
      ),
      paradoxMaterialTheme in Compile := {
        ParadoxMaterialTheme()
          // choose from https://jonas.github.io/paradox-material-theme/getting-started.html#changing-the-color-palette
          .withColor("light-green", "amber")
          // choose from https://jonas.github.io/paradox-material-theme/getting-started.html#adding-a-logo
          .withLogoIcon("cloud")
          .withCopyright("Copyleft © Johannes Rudolph")
          .withRepository(uri("https://github.com/jrudolph/xyz"))
          .withSocial(
            uri("https://github.com/jrudolph"),
            uri("https://twitter.com/virtualvoid")
          )
      },
      paradoxProperties ++= Map(
        "github.base_url" -> (paradoxMaterialTheme in Compile).value.properties.getOrElse("repo", "")
      ),
    )

def commonSettings = Seq(
  scalaVersion := scalaV,
  scalacOptions ++= Seq("-deprecation", "-feature", "-encoding", "utf8", "-Ywarn-dead-code", "-unchecked", "-Xlint")
)