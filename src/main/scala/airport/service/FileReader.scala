package airport.service

import better.files.File
import airport.model._

import scala.collection.mutable.{HashMap, PriorityQueue}
import scala.util.Try

class FileReader {

  val countriesByCode = new HashMap[CountryCode, Country]
  val countriesByName = new HashMap[String, Country]
  val airportsByCCode = new HashMap[CountryCode, List[Airport]]
  val runwaysByAIdent = new HashMap[AirportIdent, List[Runway]]

  private lazy val countryAndAirportSorted: List[(Either[Country, CountryCode], List[Airport])] = airportsByCCode
    .toList
    .map{case (cc, airports) => (countriesByCode.get(cc).toLeft(cc), airports)}
    .sortBy(_._2.size)

  def minAirportCountries = countryAndAirportSorted.take(10)
    .map{ case (country, airports) => (country, airports.size)}

  def maxAirportCountries = countryAndAirportSorted.takeRight(10)
    .map{ case (country, airports) => (country, airports.size)}
    .reverse

  def countryAndRunwaySurface = {
    countryAndAirportSorted.map{ case (country, airports) =>
      val surfaces = airports.flatMap{airport =>
        runwaysByAIdent.get(airport.ident)
          .map(_.flatMap(_.surface))
          .getOrElse(List())
      }.distinct
      (country, surfaces)
    }
  }

  def loadFiles() = {
    processLines("resources/countries.csv",insertCountry(_))
    processLines("resources/airports.csv",insertAirport(_))
    processLines("resources/runways.csv",insertRunway(_))
    countryAndAirportSorted
    ()
  }

  private def processLines[A](fileName: String, process: Seq[String] => Unit) = {
    File(fileName).lineIterator
      .drop(1)
      .foreach { line =>
        val trimLine = line.split("(,)(?=(?:[^\"]|\"[^\"]*\")*$)") //regexp to match coma out of quotes
          .map(_.replaceAll("\"", "").trim)
        process(trimLine)
      }
  }

  private[service] def insertCountry(line: Seq[String]) = {
    line.toList match {
      case _ :: CountryCode(code) :: name :: tail =>
        val country = Country(code, name)
        countriesByName += (name -> country)
        countriesByCode += (code -> country)
      case error =>
        println(s"${error.mkString(", ")} is not a valid country")
    }
  }

  private[service] def insertAirport(line: Seq[String]) = {
    line.toList match {
      case _ :: ident :: typ :: name :: lat :: long :: elevationFT :: continent :: CountryCode(code) :: tail =>
        val airports = Airport(code, AirportIdent(ident), name, typ) :: airportsByCCode.getOrElse(code, List.empty)
        airportsByCCode += (code -> airports)
      case error =>
        println(line(8))
        println(s"${error.mkString(", ")} is not a valid airport")
    }
  }

  private[service] def insertRunway(line: Seq[String]) = {
    Try{
      val ident = new AirportIdent(line(2))
      val runway = Runway(
        ident,
        stringOpt(line(3)).map(_.toInt),
        stringOpt(line(4)).map(_.toInt),
        stringOpt(line(5)),
        stringOpt(line(6)).map(_ == "1")
        )
      val runaways = runway :: runwaysByAIdent.getOrElse(ident, List.empty)
      runwaysByAIdent += (ident -> runaways)
    }.getOrElse{
      println(s"${line.mkString(", ")} is not a valid runaway")
    }
  }



  private def stringOpt(str: String) = {
    Option(str).filterNot(_.isEmpty)
  }
}