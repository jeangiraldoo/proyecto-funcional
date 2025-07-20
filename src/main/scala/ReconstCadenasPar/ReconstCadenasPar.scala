import Oraculo._
import Common._
import scala.collection.parallel.CollectionConverters._

package object ReconstCadenasPar {
  val alfabeto = Seq('a', 'c', 'g', 't')
  def reconstruirCadenaIngenuoPar(n: Int, oraculo: Oraculo): Seq[Char] = {
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

  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    // Genera nuevas combinaciones de cadenas concatenando pares del conjunto actual.
    // Solo conserva aquellas cuyas subcadenas de tamaño 'tamanoVentana' están todas en el conjunto original.
    def generarCadenasValidasPorVentana(conjuntoActual: Set[String], tamanoVentana: Int): Set[String] = {
      val combinacionesValidas = for {
        cadena1 <- conjuntoActual.par
        cadena2 <- conjuntoActual.par
        cadenaCombinada = cadena1 + cadena2
        if cadenaCombinada.sliding(tamanoVentana).forall(conjuntoActual.contains)
      } yield cadenaCombinada

      combinacionesValidas.seq.toSet
    }

    // Construye recursivamente la cadena completa válida utilizando el oráculo.
    @scala.annotation.tailrec
    def construirRecursivamente(conjuntoCandidatas: Set[String], longitudActual: Int): String = {
      if (longitudActual >= n) {
        conjuntoCandidatas.find(cadena => cadena.length == n && o(cadena.toSeq)).getOrElse("")
      } else {
        val nuevasCadenas = generarCadenasValidasPorVentana(conjuntoCandidatas, longitudActual)
        val candidatasEvaluadas = nuevasCadenas.par.filter(cadena => o(cadena.toSeq))
        construirRecursivamente(candidatasEvaluadas.seq.toSet, longitudActual * 2)
      }
    }

    // Punto de partida: letras individuales del alfabeto como cadenas
    val conjuntoInicial: Set[String] = alfabeto.map(_.toString).toSet

    // Iniciar construcción recursiva con longitud 1
    construirRecursivamente(conjuntoInicial, 1).toList
  }
}