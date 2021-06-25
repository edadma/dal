name := "dal"

version := "0.2.0"

versionScheme := Some("semver-spec")

scalaVersion := "2.13.6"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

organization := "xyz.hyperreal"

githubOwner := "edadma"

githubRepository := "dal"

resolvers += Resolver.githubPackages("edadma", "numbers")

enablePlugins(ScalaJSPlugin)

//scalaJSUseMainModuleInitializer := true

Test / scalaJSUseMainModuleInitializer := false

Test / scalaJSUseTestModuleInitializer := true

jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()

mainClass := Some( "xyz.hyperreal." + name.value.replace('-', '_') + ".Main" )

libraryDependencies ++= Seq(
  "org.scalatest" %%% "scalatest" % "3.2.5" % "test"
)

libraryDependencies ++= Seq(
  "xyz.hyperreal" %%% "numbers" % "0.1.2"
)

licenses := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))

homepage := Some(url("https://github.com/edadma/" + name.value))

publishMavenStyle := true

Test / publishArtifact := false

pomIncludeRepository := { _ => false }

pomExtra :=
  <scm>
    <url>git@github.com:edadma/{name.value}.git</url>
    <connection>scm:git:git@github.com:edadma/{name.value}.git</connection>
  </scm>
    <developers>
      <developer>
        <id>edadma</id>
        <name>Edward A. Maxedon, Sr.</name>
        <url>https://github.com/edadma</url>
      </developer>
    </developers>


//ThisBuild / scalaVersion := "2.13.2"
//
//lazy val root = project.in(file(".")).
//  aggregate(dal.js, dal.jvm).
//  settings(
//    publish := {},
//    publishLocal := {},
//    scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" ),
//
//    libraryDependencies ++= Seq(
//      "org.scalatest" %%% "scalatest" % "3.1.1" % "test"
//    ),
//
//    libraryDependencies ++= Seq(
//      "xyz.hyperreal" %%% "numbers" % "0.7.2"
//    ),
//  )
//
//lazy val dal = crossProject(JSPlatform, JVMPlatform).in(file(".")).
//  settings(
//    name := "dal",
//
//    version := "0.1.9",
//
//    organization := "xyz.hyperreal",
//
//    resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven",
//
//    mainClass in (Compile, run) := Some( "xyz.hyperreal." + name.value.replace('-', '_') + ".Main" ),
//
//    licenses := Seq("ISC" -> url("https://opensource.org/licenses/ISC")),
//
//    homepage := Some(url("https://github.com/edadma/" + name.value)),
//
//    publishMavenStyle := true,
//
//    publishArtifact in Test := false,
//
//    pomIncludeRepository := { _ => false },
//
//    pomExtra :=
//      <scm>
//        <url>git@github.com:edadma/{name.value}.git</url>
//        <connection>scm:git:git@github.com:edadma/{name.value}.git</connection>
//      </scm>
//      <developers>
//        <developer>
//          <id>edadma</id>
//          <name>Edward A. Maxedon, Sr.</name>
//          <url>https://github.com/edadma</url>
//        </developer>
//      </developers>
//  ).
//  jvmSettings(
//
//    libraryDependencies ++= Seq(
//      "org.scalatest" %% "scalatest" % "3.1.1" % "test"
//    ),
//
//    libraryDependencies ++= Seq(
//      "xyz.hyperreal" %% "numbers" % "0.7.2"
//    ),
//  ).
//  jsSettings(
//  )
