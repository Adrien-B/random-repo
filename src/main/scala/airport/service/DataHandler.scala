package airport.service

import airport.model.CountryCode.isCountryCode
import airport.model._

class DataHandler(val countriesByCode: Map[CountryCode, Country],
                  val countriesByName: Map[String, Country],
                  val airportsByCCode: Map[CountryCode, List[Airport]],
                  val runwaysByAIdent: Map[AirportIdent, List[Runway]]) {


  private val countriesByApproxName = countriesByName.map{case (name, country) => (approxStr(name) -> country)}
  private val approxNameAndCountries = countriesByApproxName.toList

  private val countryAndAirportSorted: List[(Either[Country, CountryCode], List[Airport])] = airportsByCCode
    .toList
    .map{case (cc, airports) => (countriesByCode.get(cc).toLeft(cc), airports)}
    .sortBy(_._2.size)

  def minAirportCountries = {
    val minSize = countryAndAirportSorted.head._2.size
    countryAndAirportSorted.takeWhile(_._2.size == minSize)
      .map{ case (country, airports) => (country, minSize)}
  }

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

  def countriesForNameOrCC(nameOrCC: String): List[Country] = {
    lazy val approxName = approxStr(nameOrCC)
    nameOrCC match {
      case CountryCode(cc) if countriesByCode.get(cc).isDefined => countriesByCode.get(cc).toList
      case name if (countriesByName.get(name).isDefined) => countriesByName.get(name).toList
      case name if isCountryCode(name.toUpperCase()) =>
        CountryCode.unapply(name.toUpperCase).flatMap{cc => countriesByCode.get(cc)}.toList
      case notCodeNotName if countriesByApproxName.get(approxName).isDefined =>
        countriesByApproxName.get(approxName).toList
      case notCodeNotName => approxNameAndCountries.collect{case (name, country) if name.contains(approxName) => country}
    }
  }

  private def approxStr(str: String) = {
    str.toLowerCase().trim.replace(" ","")
  }
}
