package airport.service

import airport.model._
import better.files.File
import org.slf4j.LoggerFactory

import scala.collection.mutable.HashMap
import scala.util.Try

class FileReader {

  private val logger = LoggerFactory.getLogger(classOf[FileReader])

  val countriesByCode = new HashMap[CountryCode, Country]
  val countriesByName = new HashMap[String, Country]
  val airportsByCCode = new HashMap[CountryCode, List[Airport]]
  val runwaysByAIdent = new HashMap[AirportIdent, List[Runway]]

  def loadFiles() = {
    logger.info("load countries")
    processLines("resources/countries.csv",insertCountry(_))
    logger.info("load airports")
    processLines("resources/airports.csv",insertAirport(_))
    logger.info("load runways")
    processLines("resources/runways.csv",insertRunway(_))
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
        logger.error(s"${error.mkString(", ")} is not a valid country")
    }
  }

  private[service] def insertAirport(line: Seq[String]) = {
    line.toList match {
      case _ :: ident :: typ :: name :: lat :: long :: elevationFT :: continent :: CountryCode(code) :: tail =>
        val airports = Airport(code, AirportIdent(ident), name, typ) :: airportsByCCode.getOrElse(code, List.empty)
        airportsByCCode += (code -> airports)
      case error =>
        logger.error(s"${error.mkString(", ")} is not a valid airport")
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
      logger.error(s"${line.mkString(", ")} is not a valid runaway")
    }
  }



  private def stringOpt(str: String) = {
    Option(str).filterNot(_.isEmpty)
  }
}
