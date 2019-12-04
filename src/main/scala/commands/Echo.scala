package commands

import files.{DirEntry, Directory, File}
import filesystem.State

import scala.annotation.tailrec

class Echo(args: List[String]) extends Command {
	override def apply(state: State): State = {
		if (args.isEmpty) state
		else if (args.length == 1) state.setMessage(args.head)
		else {
			val operator = args(args.length - 2)
			val filename = args.last
			val contents = createContent(args, args.length-2)

			if (operator.equals(">>")) doEcho(state, contents, filename, append = true)
			else if (operator.equals(">")) doEcho(state, contents, filename, append = false)
			else state.setMessage(createContent(args, args.length))
		}
	}

	def getRootAfterEcho(currentDir: Directory, path: List[String], contents: String, append: Boolean): Directory = {
		/*
			if path is empty - fail (return current dir)
			else if no more things to explore (path tail is empty)
				find the file to create/add content to
				if file is not found, create file
				else if it's actually a directory then fail
				else
					replace or append content to file
					replace entry with a new file
			else
				find next directory to navigate
				call self recursively

				if recursive call failed - fail
				else replace entry with a new directory after the recursive call
		 */
		if (path.isEmpty) currentDir
		else if (path.tail.isEmpty) {
			val dirEntry: DirEntry = currentDir.findEntry(path.head)

			if (dirEntry == null) currentDir.addEntry(new File(currentDir.path, path.head, contents))
			else if (dirEntry.isDirectory) currentDir
			else {
				if (append) currentDir.replaceEntry(path.head, dirEntry.asFile.appendContent(contents))
				else currentDir.replaceEntry(path.head, dirEntry.asFile.setContent(contents))
			}
		} else {
			val nextDir = currentDir.findEntry(path.head).asDirectory
			val newNextDir = getRootAfterEcho(nextDir, path.tail, contents, append)

			if (newNextDir == nextDir) currentDir
			else currentDir.replaceEntry(path.head, newNextDir)
		}
	}

	def doEcho(state: State, contents: String, filename: String, append: Boolean): State = {
		if (filename.contains(Directory.SEPARATOR)) state.setMessage("echo does not support separators in filenames")
		else {
			val newRoot: Directory = getRootAfterEcho(
				state.root, state.wd.getAllFoldersInPath :+ filename, contents, append
			)

			if (newRoot == state.root) state.setMessage(filename + ": no such file")
			else {
				State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
			}
		}
	}

	def createContent(args: List[String], topIdx: Int): String = {
		@tailrec
		def createContentHelper(args: List[String], content: StringBuilder, curIdx: Int): String = {
			if (args.tail.isEmpty) content.mkString
			else if (curIdx == topIdx) content.mkString
			else createContentHelper(
				args.tail, content.append(args.head).append(if (args.tail.isEmpty) "" else " "), curIdx+1
			)
		}

		createContentHelper(args, new StringBuilder, 0)
	}
}
