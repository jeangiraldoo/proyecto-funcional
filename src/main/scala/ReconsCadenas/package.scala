import Oraculo ._
import Common ._

package object ReconsCadenas {
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

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    require(n > 0 && (n & (n - 1)) == 0, "n debe ser una potencia de 2")
    
    def productoCartesiano(sc1: Seq[Seq[Char]], sc2: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      for {
        s1 <- sc1
        s2 <- sc2
      } yield s1 ++ s2
    }

    
    def buscarCadena(sc: Seq[Seq[Char]], k: Int): Seq[Char] = {
      if (k > n) {
        throw new IllegalStateException("No se encontró la cadena")
      } else {
        val scK = if (k == 1) {
          alfabeto.map(c => Seq(c))
        } else {
          productoCartesiano(sc, sc)
        }
        
        val candidatasValidas = scK.filter(o)
        
        candidatasValidas.find(_.length == n) match {
          case Some(cadenaEncontrada) => cadenaEncontrada
          case None =>
            buscarCadena(candidatasValidas, k * 2)
        }
      }
    }

    
    val sc1 = alfabeto.map(c => Seq(c)).filter(o)
    buscarCadena(sc1, 2)
  }
}
