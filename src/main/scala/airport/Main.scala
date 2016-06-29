package airport

import airport.service.{DataHandler, FileReader}


object Main {

  def main(args: Array[String]): Unit = {
    val fileReader = new FileReader()
    fileReader.loadFiles
    val data = new DataHandler(
      fileReader.countriesByCode.toMap,
      fileReader.countriesByName.toMap,
      fileReader.airportsByCCode.toMap,
      fileReader.runwaysByAIdent.toMap)
    val ui = new ConsoleUi(data)
    ui.menu()
  }

}
