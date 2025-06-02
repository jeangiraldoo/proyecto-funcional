import Oraculo ._
import Common ._

package object ReconstCadenas {
  val alfabeto = Seq('a', 'c', 'g', 't')
  
  def reconstruirCadenaIngenua(n: Int, oraculo: Oraculo): Seq[Char] = {
    def generateAll(length: Int): Seq[Seq[Char]] = {
      if (length == 0) Seq(Seq())
      else for {
        shorter <- generateAll(length - 1)
        letter <- alfabeto
      } yield shorter :+ letter
    }
    var result: Option[Seq[Char]] = None
    for {
      len <- 1 to n
      candidate <- generateAll(len)
      if result.isEmpty && oraculo(candidate) && candidate.length == n
    } result = Some(candidate)

    result.getOrElse(Seq())
  }

  def reconstruirCadenaIngenuaPar(n: Int, oraculo: Oraculo): Seq[Char] = {
    def generateAll(length: Int): Seq[Seq[Char]] = {
      if (length == 0) Seq(Seq())
      else for {
        shorter <- generateAll(length - 1)
        letter <- alfabeto
      } yield shorter :+ letter
    }

    var result: Option[Seq[Char]] = None

    for {
      len <- 1 to n
      if result.isEmpty
    } {
      val candidates = generateAll(len)
      val tasks = candidates.map(c => task(oraculo(c)))

      for ((c, t) <- candidates.zip(tasks)
           if result.isEmpty && c.length == n && t.join()) {
        result = Some(c)
      }
    }

    result.getOrElse(Seq())
  }
}
