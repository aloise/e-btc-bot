name := "e-btc-bot"

version := "1.0"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
	 "com.typesafe" % "config" % "1.+",
	 "com.github.tototoshi" %% "scala-csv" % "1.0.0-SNAPSHOT",
	  "com.typesafe.play" %% "play-json" % "2.2.+",
	 "io.spray" % "spray-client" % "1.2-RC4",
	 "commons-codec" % "commons-codec" % "1.8",
	 "com.typesafe.akka" % "akka-actor_2.10" % "2.2.+"
//	 "io.spray" %%  "spray-json" % "1.+"
)	 

resolvers ++= Seq(
	 "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
	 "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
	 "repo.codahale.com" at "https://github.com/cphylabs/jerkson",
	 "spray repo" at "http://repo.spray.io"
)	 