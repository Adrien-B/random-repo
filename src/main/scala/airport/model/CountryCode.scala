package airport.model

class CountryCode private(val code: String) extends AnyVal

object CountryCode{


  def unapply(possibleCode: String): Option[CountryCode] = {
    if ((possibleCode.length == 2) && possibleCode.forall(_.isUpper))
      Some(new CountryCode(possibleCode))
    else
      None
  }


  object TestFactory{

    implicit class richString(s: String){
      def cc = CountryCode.unapply(s).get
    }
  }
}