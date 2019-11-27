package files

abstract class DirEntry(val parentPath: String, val name: String) {
	def path: String = {
		val optSep = if (parentPath.equals(Directory.ROOT_PATH)) "" else Directory.SEPARATOR
		parentPath + optSep + name
	}

	def asDirectory: Directory

	def asFile: File

	def getType: String

	def isDirectory: Boolean

	def isFile: Boolean
}
