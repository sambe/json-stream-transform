organization := "com.github.sambe"

name := "json-stream-transform"

version := "0.1"

scalaVersion := "2.10.3"

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.4.0"

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.4.0"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.9" % "test"

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/sambe/json-stream-transform</url>
    <licenses>
      <license>
        <name>LGPL-3.0</name>
        <url>http://opensource.org/licenses/LGPL-3.0</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:sambe/json-stream-transform.git</url>
      <connection>scm:git:git@github.com:sambe/json-stream-transform.git</connection>
    </scm>
    <developers>
      <developer>
        <id>sambe</id>
        <name>Samuel Berner</name>
      </developer>
    </developers>)