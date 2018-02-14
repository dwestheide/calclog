package calclog.examples.datetime

import java.time.Duration

object DateTimeExample extends App {

  import calclog.datetime._
  import fractions.bigdecimal._
  import calclog.Implicits._
  import calclog.ValueFormatter.Implicits._

  val listeningTime = Duration.ofMillis(50000) ~ "listening time"
  val x = 5 ~ "x"
  val fraction = listeningTime / listeningTime

}
