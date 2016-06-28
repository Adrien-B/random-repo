package airport.model

case class Runway(airportIdent: AirportIdent,
                  length: Option[Int],
                  width: Option[Int],
                  surface: Option[String],
                  lighted: Option[Boolean])
