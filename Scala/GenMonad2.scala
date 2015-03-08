import org.scalacheck.Gen._

object GenMonad2 {
	def main(args: Array[String]) = {
		val g = alphaStr flatMap (s => oneOf(s) map (c => (s, c)))
		for (_ <- 1 to 10) g.sample.map(println)
	}
}
