package commands

import files.Directory
import filesystem.State

class Echo(args: List[String]) extends Command {
	override def apply(state: State): State = {
		if (args.isEmpty) state
		else if (args.length == 1) state.setMessage(args.head)
		else {
			val operator = args(args.length - 2)
			val filename = args.last
			val contents = createContent(args, args.length-2)

			if (operator.equals(">>"))  doEcho(state, contents, filename, append = true)
			else if (operator.equals(">")) doEcho(state, contents, filename, append = false)
			else state.setMessage(createContent(args, args.length))
		}
	}

	def getRootAfterEcho(currentDir: Directory, path: List[String], contents: String, append: Boolean): Directory = {

	}

	def doEcho(state: State, contents: String, filename: String, append: Boolean): State = {
		if (filename.contains(Directory.SEPARATOR)) state.setMessage("echo does not support separators in filenames")
		else {
			val newRoot: Directory = ???

			if (newRoot == state.root) state.setMessage(filename + ": no such file")
			else {
				State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
			}
		}
	}

	def createContent(args: List[String], topIdx: Int): String = {
		args.mkString(" ")
	}
}
