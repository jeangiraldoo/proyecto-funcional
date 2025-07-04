import Oraculo._
import Common._

package object ReconstCadenasPar {

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    require(n > 0 && (n & (n - 1)) == 0, "n debe ser una potencia de 2")

    def productoCartesiano(sc1: Seq[Seq[Char]], sc2: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (sc1.size * sc2.size >= umbral) {
        val tareas = for (s1 <- sc1) yield task {
          for (s2 <- sc2) yield s1 ++ s2
        }
        tareas.flatMap(_.join())
      } else {
        for {
          s1 <- sc1
          s2 <- sc2
        } yield s1 ++ s2
      }
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

        
        val candidatasValidas = if (scK.size >= umbral) {
          val numTareas = 4  
          val tamañoGrupo = (scK.size + numTareas - 1) / numTareas
          val grupos = scK.grouped(tamañoGrupo).toSeq

          val tareas = grupos.map(grupo => task {
            grupo.filter(o)
          })

          tareas.flatMap(_.join())
        } else {
          scK.filter(o)
        }

        candidatasValidas.find(_.length == n) match {
          case Some(cadenaEncontrada) => cadenaEncontrada
          case None => buscarCadena(candidatasValidas, k * 2)
        }
      }
    }

    val sc1 = alfabeto.map(c => Seq(c)).filter(o)
    buscarCadena(sc1, 2)
  }
}