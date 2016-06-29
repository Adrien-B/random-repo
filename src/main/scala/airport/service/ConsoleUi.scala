package airport

import airport.model.{Country, CountryCode}
import airport.service.{DataHandler, FileReader}

import scala.io.StdIn


class ConsoleUi(data: DataHandler) {

  def queryMenu() : Unit = {
    val nameOrCC = StdIn.readLine("Please enter a country code or a country name: ")
    data.countriesForNameOrCC(nameOrCC) match {
      case List(country) => queryDisplay(country)
      case Nil => println(s"$nameOrCC is not a known country code or name")
        menu()
      case list => println(s"$nameOrCC is not a known country code or name")
        println("howover you may want to search for one of those name:")
        list.foreach{item => println(s" $item")}
        queryMenu()
    }
  }

  def queryDisplay(country: Country) = {
    import data._
    airportsByCCode.get(country.code) match {
      case None => println("this country has no airport")
      case Some(list) =>
        list.foreach{ airport =>
          println(s"${airport.ident.ident}: ${airport.name} a ${airport.typ} located in ${country.name}")
          runwaysByAIdent.get(airport.ident) match {
            case None => println("  |-no runway informations for this airport")
            case Some(runways) =>
              println(s"  |-This airport has ${runways.size} runway(s), we have those informations: ")
              runways.foreach{ runway =>
              runway.lighted.collect{
                  case true => println("  |  |-a lighted runway")
                  case false => println("  |  |-a runway that is not lighted")
                }
              runway.surface.foreach{s => s"  |  |-runway of $s"}
              runway.length.foreach{l => println(s"  |  |-$l feet long")}
              runway.width.foreach{w => println(s"  |  |-$w feet large")}
              println("  | ")
            }
          }
        }
    }
  }

  def report() = {
    println("")
    println("countries with the highest number of airports")
    data.maxAirportCountries.foreach{ case (countryEith, nbAirport) =>
      println(s"  ${cname(countryEith)}, has ${nbAirport} airports")
    }

    println("")
    println("countries with the lowest number of airports")
    data.minAirportCountries.foreach{ case (countryEith, nbAirport) =>
      println(s"  ${cname(countryEith)}, has ${nbAirport} airport(s)")
    }

    println("")
    println("countries runway surafce type:")
    data.countryAndRunwaySurface.foreach{ case (countryEith, surfaces) =>
      println(s"  ${cname(countryEith)}: ${surfaces.mkString(", ")}")
    }
  }

  def menu(): Unit = {
    Seq("","What would you like to do","1) Query","2) Report").foreach(println(_))
    val input = StdIn.readLine("""Type "1", "2" or "exit": """)
    input match {
      case "1" => queryMenu()
      case "2" => report()
      case "exit" => ()
      case unknown =>
        println(s"$unknown is neither 1,2 or exit")
        menu()
    }
  }

  private def cname(countryEith: Either[Country, CountryCode]) = {
    countryEith match {
      case Left(country) => country.name
      case Right(cc) => s"unknow country of code ${cc.code}"
    }
  }

}
