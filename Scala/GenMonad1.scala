import org.scalacheck.Gen._

object GenMonad1 {
	def main(args: Array[String]) = {
		val g = for {
			s <- alphaStr
			c <- oneOf(s)
		} yield (s, c)
		for (_ <- 1 to 10) g.sample.map(println _)
	}
}
