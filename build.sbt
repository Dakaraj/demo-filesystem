name := "demo-filesystem"

version := "0.1"

scalaVersion := "2.12.10"

lazy val filesystem = (project in file("filesystem"))
	.settings(
		name := "Filesystem"
	)
