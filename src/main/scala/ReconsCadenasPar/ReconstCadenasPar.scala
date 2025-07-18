import Oraculo._
import Common._
import scala.collection.parallel.CollectionConverters._

package object ReconstCadenasPar {

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    require(n > 0 && (n & (n - 1)) == 0)

    def buscarCadena(sc: Seq[Seq[Char]], k: Int): Seq[Char] = {
      val scK = if (sc.length >= umbral) {
        val mitad = sc.length / 2
        val (sc1, sc2) = sc.splitAt(mitad)

        val (parte1, parte2) = parallel(
          for {
            s1 <- sc1
            s2 <- sc
          } yield s1 ++ s2,
          for {
            s1 <- sc2
            s2 <- sc
          } yield s1 ++ s2
        )

        parte1 ++ parte2
      } else {
        for {
          s1 <- sc
          s2 <- sc
        } yield s1 ++ s2
      }

      val candidatasValidas = if (scK.length >= umbral) {
        scK.par.filter(o).seq
      } else {
        scK.filter(o)
      }


      if (k == n) {
        candidatasValidas.find(cadena => cadena.length == n && o(cadena)).get
      } else {
        buscarCadena(candidatasValidas, k * 2)
      }
    }


    val sc1 = alfabeto.map(c => Seq(c)).filter(o)

    if (n == 1) {
      sc1.head
    } else {
      buscarCadena(sc1, 2)
    }
  }
}