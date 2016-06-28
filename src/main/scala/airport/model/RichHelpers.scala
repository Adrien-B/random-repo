package airport.model

import cats.data.OptionT

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

/**
  * Created by bruce on 07/06/2016.
  */


object RichHelpers {

  type futureOpt[A] = OptionT[Future,A]

  implicit class richFutureOpt[A](optF: OptionT[Future,A]) {
    def computedValue = Await.result(optF.value, 1 second )
  }

  implicit class richFuture[A](f: Future[A]) {
    def computedValue = Await.result(f, 1 second )
  }
}
