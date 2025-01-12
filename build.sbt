
val toolkitV = "0.2.0"
val toolkit = "org.scala-lang" %% "toolkit" % toolkitV
val toolkitTest = "org.scala-lang" %% "toolkit-test" % toolkitV

ThisBuild / scalaVersion := "3.3.3"
libraryDependencies += toolkit
libraryDependencies += (toolkitTest % Test)

lazy val root = project
    .in(file("."))
    .settings(
        name := "Malatanghulu",
        libraryDependencies ++= Seq(
            "io.circe" %% "circe-core" % "0.14.3",
            "io.circe" %% "circe-generic" % "0.14.3",
            "io.circe" %% "circe-parser" % "0.14.3",
            ("org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0")
            .cross(CrossVersion.for3Use2_13),
        )
    )