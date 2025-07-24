import Oraculo ._
import Common ._
import ArbolSufijos._

package object ReconstCadenas {
  val alfabeto = Seq('a', 'c', 'g', 't')

  def reconstruirCadenaIngenuo(n: Int, oraculo: Oraculo): Seq[Char] = {
    def loop(currentLevelSequences: List[Seq[Char]]): Seq[Char] = {
      if (currentLevelSequences.isEmpty) return Seq.empty

      val valid_sequence = currentLevelSequences.filter(oraculo)

      val target_length_sequence = valid_sequence.find(_.length == n)
      target_length_sequence match {
        case Some(solution) => solution
        case None =>
          val nextLevelSequences = for {
            seq <- valid_sequence
            ch <- alfabeto
          } yield seq :+ ch

          loop(nextLevelSequences)
      }
    }
    loop(List(Seq.empty))
  }

  def reconstruirCadenaMejorado(n: Int, oracle: Oraculo): Seq[Char] = {
    @annotation.tailrec
    def loop(stack: Seq[Seq[Char]]): Seq[Char] = stack match {
      case Seq() => Seq.empty
      case current +: rest =>
        if (current.length == n) current
        else {
          val next = alfabeto.view
            .map(c => current :+ c)
            .filter(oracle)
            .toSeq
          loop(next ++ rest)
        }
    }

    loop(Seq(Seq.empty))
  }

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    require(n > 0 && (n & (n - 1)) == 0)

    def buscarCadena(sc: Seq[Seq[Char]], k: Int): Seq[Char] = {
      val scK = for {
        s1 <- sc
        s2 <- sc
      } yield s1 ++ s2

      val candidatasValidas = scK.filter(o)

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

    def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    def combinarYFiltrar(cadenasValidas: Set[String], tamanoSubcadena: Int): Set[String] = {
      for {
        primera <- cadenasValidas
        segunda <- cadenasValidas
        combinada = primera + segunda
        if combinada.sliding(tamanoSubcadena).forall(cadenasValidas.contains)
      } yield combinada
    }

    def construirCadena(candidatas: Set[String], tamanoActual: Int): String = {
      if (tamanoActual >= n) {
        candidatas.find(cadena =>
          cadena.length == n && o(cadena.toSeq)
        ).getOrElse("")
      } else {
        val combinadas = combinarYFiltrar(candidatas, tamanoActual)
        val candidatasValidas = combinadas.filter(cadena => o(cadena.toSeq))
        construirCadena(candidatasValidas, tamanoActual * 2)
      }
    }

    val subcadenasInicialesValidas: Set[String] = alfabeto.flatMap(letra => {
      val letraComoCadena = letra.toString
      if (o(letraComoCadena.toSeq)) Some(letraComoCadena) else None
    }).toSet
    construirCadena(subcadenasInicialesValidas, 1).toList
}

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    require(n > 0 && (n & (n - 1)) == 0)

    @annotation.tailrec
    def buscarCadena(cadenasValidasActuales: List[Seq[Char]], current_k: Int): Seq[Char] = {
      if (current_k == n) {
        // Buscar directamente en las cadenas válidas una que tenga longitud n
        cadenasValidasActuales
          .find(cadena => cadena.length == n && o(cadena))
          .getOrElse(Seq.empty)
      } else {
        // Combinar todas las cadenas válidas actuales entre sí
        val combinadas = for {
          s1 <- cadenasValidasActuales
          s2 <- cadenasValidasActuales
          combinada = s1 ++ s2
          if esCombinacionValida(combinada, current_k, cadenasValidasActuales)
        } yield combinada

        // Filtrar solo las combinaciones que el oráculo considera válidas
        val candidatosValidos = combinadas
          .filter(o)
          .distinct

        buscarCadena(candidatosValidos, current_k * 2)
      }
    }

    def esCombinacionValida(combinada: Seq[Char], k: Int, cadenasValidas: List[Seq[Char]]): Boolean = {
      // Verificar que la longitud sea correcta y que todas las subcadenas de longitud k estén en la lista válida
      combinada.length == k * 2 &&
      combinada.sliding(k).forall(subcadena => cadenasValidas.contains(subcadena))
    }

    // Obtener las cadenas iniciales de longitud 1 que son válidas según el oráculo
    val sc1 = alfabeto
      .map(Seq(_))
      .filter(o)
      .toList

    if (n == 1) sc1.headOption.getOrElse(Seq.empty)
    else buscarCadena(sc1, 2)
}
}