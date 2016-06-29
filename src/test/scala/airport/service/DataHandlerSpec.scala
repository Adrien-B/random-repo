package airport.service

import airport.model.CountryCode.TestFactory.richString
import airport.model._
import org.scalatest.{MustMatchers, WordSpec}

class DataHandlerSpec extends WordSpec with MustMatchers {

  private val data = new FileReader()
  data.loadFiles()
  private val sut = new DataHandler(
    data.countriesByCode.toMap,
    data.countriesByName.toMap,
    data.airportsByCCode.toMap,
    data.runwaysByAIdent.toMap)


  "DataHandler" must {

    "load file and extract minAirportList" in {
      sut.minAirportCountries must equal(List(
        (Left(Country("JE".cc,"Jersey")),1),
        (Left(Country("GI".cc,"Gibraltar")),1),
        (Left(Country("NF".cc,"Norfolk Island")),1),
        (Left(Country("MO".cc,"Macau")),1),
        (Left(Country("ZZ".cc,"Unknown or unassigned country")),1),
        (Left(Country("BL".cc,"Saint Barthélemy")),1),
        (Left(Country("CW".cc,"Curaçao")),1),
        (Left(Country("AD".cc,"Andorra")),1),
        (Left(Country("SH".cc,"Saint Helena")),1),
        (Left(Country("CC".cc,"Cocos (Keeling) Islands")),1),
        (Left(Country("GM".cc,"Gambia")),1),
        (Left(Country("NU".cc,"Niue")),1),
        (Left(Country("SX".cc,"Sint Maarten")),1),
        (Left(Country("MC".cc,"Monaco")),1),
        (Left(Country("CX".cc,"Christmas Island")),1),
        (Left(Country("LI".cc,"Liechtenstein")),1),
        (Left(Country("MQ".cc,"Martinique")),1),
        (Left(Country("NR".cc,"Nauru")),1),
        (Left(Country("YT".cc,"Mayotte")),1),
        (Left(Country("AI".cc,"Anguilla")),1),
        (Left(Country("IO".cc,"British Indian Ocean Territory")),1),
        (Left(Country("AW".cc,"Aruba")),1),
        (Left(Country("VA".cc,"Vatican City")),1),
        (Left(Country("TV".cc,"Tuvalu")),1)
      ))
    }

    "load file and extract maxAirportList" in {
      sut.maxAirportCountries must equal(List(
        (Left(Country("US".cc, "United States")), 21501),
        (Left(Country("BR".cc, "Brazil")), 3839),
        (Left(Country("CA".cc, "Canada")), 2454),
        (Left(Country("AU".cc, "Australia")), 1908),
        (Left(Country("RU".cc, "Russia")), 920),
        (Left(Country("FR".cc, "France")), 789),
        (Left(Country("AR".cc, "Argentina")), 713),
        (Left(Country("DE".cc, "Germany")), 703),
        (Left(Country("CO".cc, "Colombia")), 700),
        (Left(Country("VE".cc, "Venezuela")), 592)
      ))
    }

    "load file and extrat surface per country" in {
      sut.countryAndRunwaySurface(50) must equal(
        Left(Country("BQ".cc, "Caribbean Netherlands")), List("ASP")
      )
      sut.countryAndRunwaySurface(100) must equal(
        Left(Country("TN".cc,"Tunisia")),List("ASP", "CON", "PEM")
      )
    }

    "match Vietnam code, name, traditional locale name… with Vietnam" in {
      val vietnam = List(Country("VN".cc, "Vietnam"))
      sut.countriesForNameOrCC("VN") must equal(vietnam)
      sut.countriesForNameOrCC("vn") must equal(vietnam)
      sut.countriesForNameOrCC("Vietnam") must equal(vietnam)
      sut.countriesForNameOrCC("vietnam") must equal(vietnam)
      sut.countriesForNameOrCC("Viet Nam") must equal(vietnam)
      sut.countriesForNameOrCC("viet") must equal(vietnam)
    }

    "match zimb with Zimbabwe" in {
      val Zimbabwe = List(Country("ZW".cc,"Zimbabwe"))
      sut.countriesForNameOrCC("zimb") must equal(Zimbabwe)
    }
  }
}
