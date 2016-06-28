package airport.service

import airport.model._
import org.scalatest.{MustMatchers, WordSpec}
import airport.model.CountryCode.TestFactory.richString

class FileReaderSpec extends WordSpec with MustMatchers {

  "FileReader" must {

    trait Fixture {
      val sut = new FileReader()
    }

    "load valid runaway" in new Fixture {
      sut.insertRunway("259670,31876,ZUNZ,9843,,UNK,0,0,05,,,9644,,,23,,,9671".split(","))
      sut.insertRunway("245733,19223,KAAA,3999,75,ASP,1,0,03,40.1533,-89.34,593,35.3,,21,40.1623,-89.3317,594,215.3,".split(","))
      sut.insertRunway("245734,19223,KAAA,3003,135,GRS,0,0,14,40.1633,-89.3371,597,144.4,307,32,40.1566,-89.3308,597,324.4,".split(","))

      sut.runwaysByAIdent must equal(Map(
        AirportIdent("KAAA") -> List(Runway(AirportIdent("KAAA"), Some(3003), Some(135), Some("GRS"), Some(false)), Runway(AirportIdent("KAAA"), Some(3999), Some(75), Some("ASP"), Some(true))),
        AirportIdent("ZUNZ") -> List(Runway(AirportIdent("ZUNZ"), Some(9843), None, Some("UNK"), Some(false)))
      ))
    }

    "load valid airport" in new Fixture {
      sut.insertAirport("6523,00A,heliport,Total Rf Heliport,40.07080078125,-74.93360137939453,11,NA,US,US-PA,Bensalem,no,00A,,00A,,,".split(","))
      sut.insertAirport("6546,00MD,small_airport,Slater Field,38.75709915161133,-75.75379943847656,45,NA,US,US-MD,Federalsburg,no,00MD,,00MD,,,".split(","))
      sut.insertAirport("42993,RU-0051,small_airport,Nikolsk Airport,59.496665954589844,45.518333435058594,551,EU,RU,RU-VLG,Nikolsk,no,,,,,http://en.wikipedia.org/wiki/Nikolsk_Airport,Nikolsk South Airport".split(","))

      val usCC = "US".cc
      sut.airportsByCCode must equal(Map(
        "RU".cc -> List(Airport("RU".cc, AirportIdent("RU-0051"), "Nikolsk Airport", "small_airport")),
        usCC -> List(Airport(usCC, AirportIdent("00MD"), "Slater Field", "small_airport"), Airport(usCC, AirportIdent("00A"), "Total Rf Heliport", "heliport"))
      ))
    }

    "load valid country" in new Fixture {
      sut.insertCountry("302672,AD,Andorra,EU,http://en.wikipedia.org/wiki/Andorra".split(","))

      sut.countriesByCode must equal(Map("AD".cc -> Country("AD".cc, "Andorra")))
      sut.countriesByName must equal(Map("Andorra" -> Country("AD".cc, "Andorra")))
    }

    "load file and extract minAirportList" in new Fixture {
      sut.loadFiles()
      sut.minAirportCountries must equal(List(
        (Left(Country("CX".cc, "Christmas Island")), 1),
        (Left(Country("AW".cc, "Aruba")), 1),
        (Left(Country("NF".cc, "Norfolk Island")), 1),
        (Left(Country("SX".cc, "Sint Maarten")), 1),
        (Left(Country("GM".cc, "Gambia")), 1),
        (Left(Country("YT".cc, "Mayotte")), 1),
        (Left(Country("CW".cc, "Curaçao")), 1),
        (Left(Country("MC".cc, "Monaco")), 1),
        (Left(Country("BL".cc, "Saint Barthélemy")), 1),
        (Left(Country("TV".cc, "Tuvalu")), 1)
      ))
    }

    "load file and extract maxAirportList" in new Fixture {
      sut.loadFiles()
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

    "load file and extrat surface per country" in new Fixture {
      sut.loadFiles()
      sut.countryAndRunwaySurface(50) must equal(
        Left(Country("BM".cc, "Bermuda")), List("ASP")
      )
      sut.countryAndRunwaySurface(100) must equal(
        Left(Country("TN".cc,"Tunisia")),List("ASP", "CON", "PEM")
      )
    }
  }
}
