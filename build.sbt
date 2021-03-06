scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies ++= Seq(
  "javax.mail" % "javax.mail-api" % "1.5.1",
  "javax.mail" % "mail" % "1.5.0-b01"
)

libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-lang3" % "3.1",
    "commons-io" % "commons-io" % "2.4"
)
