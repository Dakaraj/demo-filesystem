package filesystem

import files.Directory

class State(val root: Directory, val wd: Directory, val output: String) {

}

object State {
	val SHELL_TOKEN = "$ "
}
