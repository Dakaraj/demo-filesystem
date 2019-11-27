package commands

import java.util.regex.Pattern

import files.{DirEntry, Directory}
import filesystem.State

abstract class CreateEntry(name: String) extends Command {
	val pattern: Pattern = Pattern.compile("\\w+", Pattern.UNICODE_CHARACTER_CLASS)

	override def apply(state: State): State = {
		val wd = state.wd
		if (wd.hasEntry(name)) {
			state.setMessage("Entry '" + name + "' already exists!")
		} else if (name.contains(Directory.SEPARATOR)) {
			state.setMessage(name + " must not contain separators!")
		} else if (checkIllegal(name)) {
			state.setMessage(name + " contains illegal symbols!")
		} else {
			doCreateEntry(state, name)
		}
	}

	private def checkIllegal(name: String): Boolean = {
		!pattern.matcher(name).matches()
	}

	def doCreateEntry(state: State, name: String): State = {
		def updateStructure(currentDir: Directory, path: List[String], newEntry: DirEntry): Directory = {
			if (path.isEmpty) currentDir.addEntry(newEntry)
			else {
				val oldEntry = currentDir.findEntry(path.head).asDirectory
				currentDir.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
			}
		}

		val wd = state.wd

		// 1. all the directories in the full path
		val allDirsInPath = wd.getAllFoldersInPath

		// 2. create new entry in the working directory
		val newEntry: DirEntry = doCreateSpecificEntry(state)

		// 3. update the whole directory structure starting from the root
		// (the directory structure is IMMUTABLE)
		val newRoot = updateStructure(state.root, allDirsInPath, newEntry)

		// 4. find new working directory INSTANCE given wd's path in the NEW directory structure
		val newWd = newRoot.findDescendant(allDirsInPath)

		State(newRoot, newWd)
	}

	def doCreateSpecificEntry(state: State): DirEntry
}
