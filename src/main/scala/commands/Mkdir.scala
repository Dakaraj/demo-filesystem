package commands

import files.{DirEntry, Directory}
import filesystem.State

class Mkdir(name: String) extends CreateEntry(name) {
	override def doCreateSpecificEntry(state: State): DirEntry = {
		Directory.empty(state.wd.path, name)
	}
}
