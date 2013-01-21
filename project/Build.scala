import sbt._
import Keys._


object FPInScalaBuild extends Build with Dependencies {

  override lazy val settings =
    super.settings ++ Seq(
      scalaVersion := "2.10.0",
      scalacOptions ++= Seq(
        "-deprecation",
        "-feature",
        "-language:higherKinds",
        "-language:implicitConversions",
        "-language:postfixOps",
        "-optimize",
        "-Xlint"
      )
    )

  lazy val root =
    Project(
      id = "fpinscala",
      base = file(".")
    ) aggregate (exercises, answers)

  lazy val exercises =
    Project(id = "exercises", base = file("exercises"))

  lazy val answers =
    Project(
      id = "answers",
      base = file("answers")
    ) settings (
      libraryDependencies += scalaz
    )

}


trait Dependencies {

  val scalaz = "org.scalaz" %% "scalaz-core" % "7.0.0-M7"

}
