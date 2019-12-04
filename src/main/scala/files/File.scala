package files

import filesystem.FilesystemException

class File(override val parentPath: String,
           override val name: String,
           val contents: String) extends DirEntry(parentPath, name) {

	override def asDirectory: Directory = throw new FilesystemException("A file cannot be converted to a directory!")

	override def asFile: File = this

	override def getType: String = "f"

	override def isDirectory: Boolean = false

	override def isFile: Boolean = true

	def setContent(newContents: String): File = new File(parentPath, name, newContents)

	def appendContent(newContents: String): File = setContent(contents + "\n" + newContents)
}

object File {
	def empty(parentPath: String, name: String): File = {
		new File(parentPath, name, "")
	}
}
