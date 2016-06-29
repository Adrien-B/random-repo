package airport.model

class CountryCode private(val code: String) extends AnyVal{
  override def toString = code
}

object CountryCode{


  def unapply(possibleCode: String): Option[CountryCode] = {
    if ((possibleCode.length == 2) && possibleCode.forall(_.isUpper))
      Some(new CountryCode(possibleCode))
    else
      None
  }

  def isCountryCode(possibleCC: String) = unapply(possibleCC).isDefined

  object TestFactory{

    implicit class richString(s: String){
      def cc = CountryCode.unapply(s).get
    }
  }
}