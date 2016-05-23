organization := "ai.colosseum"

name := "colosseum-backend"

version := "0.1"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.10.6", "2.11.8")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.3.1",
  "org.specs2" %% "specs2" % "2.3.13",
  "com.typesafe.play" %% "play-ws" % "2.4.3"
)

unmanagedSourceDirectories in Compile <+= (sourceDirectory in Compile, scalaBinaryVersion){
  (sourceDir, version) => sourceDir / (if (version.startsWith("2.10")) "scala_2.10" else "scala_2.11")
}

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/ryanpbrewster/ai-colosseum</url>
  <licenses>
    <license>
      <name>MIT</name>
      <url>http://opensource.org/licenses/MIT</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:ryanpbrewster/ai-colosseum.git</url>
    <connection>scm:git:git@github.com:ryanpbrewster/ai-colosseum.git</connection>
  </scm>
  <developers>
    <developer>
      <id>ryanpbrewster</id>
      <name>Ryan Brewster</name>
      <url>https://github.com/ryanpbrewster</url>
    </developer>
  </developers>)
