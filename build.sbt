val swing = "org.scala-lang" % "scala-swing" % "2.10+"

lazy val root = (project in file(".")).
settings {
	name := "Chess"
	libraryDependencies += swing
	scalacOptions += "-deprecation"
}

