package commands

import files.{DirEntry, Directory}
import filesystem.State

import scala.annotation.tailrec

class Cd(dir: String) extends Command {
	override def apply(state: State): State = {
		// 1. Find root
		val root = state.root
		val wd = state.wd

		// 2. Find the absolute path of the directory I want to cd to
		val absPath = if (dir.startsWith(Directory.SEPARATOR)) dir
		else if (wd.isRoot) wd.path + dir
		else wd.path + Directory.SEPARATOR + dir

		// 3. Find the directory to cd to, given the path
		val destination = doFindEntry(root, absPath)

		// 4. Change the state given the new directory
		if (destination == null || !destination.isDirectory)
			state.setMessage(dir + ": no such directory")
		else State(root, destination.asDirectory)
	}

	def doFindEntry(root: Directory, path: String): DirEntry = {
		@tailrec
		def findEntryHelper(currentDir: Directory, path: List[String]): DirEntry = {
			if (path.isEmpty || path.head.isEmpty) currentDir
			else if (path.tail.isEmpty) currentDir.findEntry(path.head)
			else {
				val nextDir = currentDir.findEntry(path.head)

				if (nextDir == null || !nextDir.isDirectory) null
				else findEntryHelper(nextDir.asDirectory, path.tail)
			}
		}

		@tailrec
		def collapseRelativeTokens(path: List[String], result: List[String]): List[String] = {
			if (path.isEmpty) result
			else if (path.head.equals(".")) collapseRelativeTokens(path.tail, result)
			else if (path.head.equals("..")) {
				if (result.isEmpty) null
				else collapseRelativeTokens(path.tail, result.init)
			} else collapseRelativeTokens(path.tail, result :+ path.head)
		}

		// 1. tokens
		val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList

		// 1.5 eliminate or collapse relative tokens
		val newTokens: List[String] = collapseRelativeTokens(tokens, List[String]())

		// 2. navigate to correct entry
		if (newTokens == null) null
		else findEntryHelper(root, newTokens)
	}
}
