package commands

import files.Directory
import filesystem.State

class Rm(name: String) extends Command {
	override def apply(state: State): State = {
		// 1. Get wd
		val wd = state.wd

		// 2. Get the abs path
		val absPath = if (name.startsWith(Directory.SEPARATOR)) name
		else if (wd.isRoot) wd.path + name
		else wd.path + Directory.SEPARATOR + name

		// 3. Do some checks
		// TODO: Implement check whether dir to delete is not a parent of current directory
		if (absPath.equals(Directory.ROOT_PATH)) state.setMessage("Removing root folder is not allowed (yet)!")
//		else if (wd.path.contains(absPath)) state.setMessage("Cannot delete current directory or it's parent")
		else {
			doRm(state, absPath)
		}
	}

	def doRm(state: State, path: String): State = {
		// 4. Find the entry to remove
		// 5. Update structure
		//		@tailrec
		def rmHelper(currentDir: Directory, path: List[String]): Directory = {
			if (path.isEmpty) currentDir
			else if (path.tail.isEmpty) currentDir.removeEntry(path.head)
			else {
				val nextDir = currentDir.findEntry(path.head)
				if (!nextDir.isDirectory) currentDir
				else {
					val newNextDir = rmHelper(nextDir.asDirectory, path.tail)
					if (newNextDir == nextDir) currentDir
					else currentDir.replaceEntry(path.head, newNextDir)
				}
			}
		}

		val tokens = path.substring(1).split(Directory.SEPARATOR).toList
		val newRoot: Directory = rmHelper(state.root, tokens)

		if (newRoot == state.root) state.setMessage(path + ": no such file or directory")
		else State(newRoot, newRoot.findDescendant(state.wd.path.substring(1)))
	}
}
