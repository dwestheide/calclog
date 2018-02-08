package calclog

import java.time.Duration

import org.scalacheck.{Arbitrary, Gen}

trait Arbitraries {
  implicit val arbitraryDuration: Arbitrary[Duration] = Arbitrary(Gen.posNum[Long].map(Duration.ofNanos))
}

object Arbitraries extends Arbitraries
