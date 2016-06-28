package airport


import airport.service.FileReader


object Main {

  def main(args: Array[String]): Unit = {
    val data = new FileReader()
    data.loadFiles
    val ui = new ConsoleUi(data)
    ui.menu()
  }

}
