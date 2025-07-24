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
        if (current.length == n) current.reverse
        else {
          val next = alfabeto.view
            .map(c => c +: current)
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

    // Función recursiva principal que busca la cadena objetivo
    def buscarCadena(currentTrie: Trie, current_k: Int): Seq[Char] = {
      if (current_k == n) {
        // Caso base: extraer cadenas de longitud n y verificar con el oráculo
        obtenerCadenasDeLongitudN(currentTrie, Seq.empty, n)
          .find(o) // Función de alto orden: find con predicado
          .getOrElse(Seq.empty)
      } else {
        // Caso recursivo: generar combinaciones y continuar
        val cadenasValidasActuales = extraerCadenasDelTrie(currentTrie, current_k)
        
        // Uso de for-comprehension (syntactic sugar para flatMap y map)
        val combinadas = for {
          s1 <- cadenasValidasActuales.toSeq
          s2 <- cadenasValidasActuales.toSeq
          combinada = s1 ++ s2
          if esCombinacionValida(combinada, current_k, currentTrie)
        } yield combinada

        // Aplicación de funciones de alto orden en pipeline
        val nextTrie = combinadas
          .filter(o) // Filtrar con oráculo
          .toList
          .pipe(ArbolDeSufijos.arbolDeSufijos) // Construir nuevo trie

        buscarCadena(nextTrie, current_k * 2) // Llamada recursiva
      }
    }

    // Función auxiliar para verificar si una combinación es válida
    def esCombinacionValida(combinada: Seq[Char], k: Int, trie: Trie): Boolean = {
      combinada.length == k * 2 &&
      combinada.sliding(k).forall(ArbolDeSufijos.pertenece(_, trie))
    }

    // Inicialización usando funciones de alto orden
    val sc1 = alfabeto
      .map(Seq(_)) // map: transformar cada char en Seq[Char]
      .filter(o)   // filter: aplicar oráculo

    val trieInicial = ArbolDeSufijos.arbolDeSufijos(sc1.toList)

    // Pattern matching implícito en la expresión condicional
    if (n == 1) sc1.headOption.getOrElse(Seq.empty)
    else buscarCadena(trieInicial, 2)
  }

  // Función auxiliar que encapsula la extracción de cadenas del trie
  private def extraerCadenasDelTrie(trie: Trie, longitud: Int): Set[Seq[Char]] = {
    def extraerRecursivo(t: Trie, currentPath: Seq[Char]): Set[Seq[Char]] = {
      t match {
        case Nodo(charNode, marcadaNode, hijos) =>
          val newPath = if (charNode == ' ') currentPath else currentPath :+ charNode
          val resultadosHijos = hijos.flatMap(extraerRecursivo(_, newPath)).toSet
          val resultadoActual = Option.when(
            newPath.length == longitud && marcadaNode && charNode != ' '
          )(newPath).toSet
          resultadosHijos ++ resultadoActual
          
        case Hoja(charLeaf, marcadaLeaf) =>
          val newPath = currentPath :+ charLeaf
          Option.when(newPath.length == longitud && marcadaLeaf)(newPath).toSet
      }
    }
    extraerRecursivo(trie, Seq.empty)
  }

  private def obtenerCadenasDeLongitudN(t: Trie, currentPath: Seq[Char], targetLength: Int): List[Seq[Char]] = {
    t match {
      case Nodo(charNode, marcadaNode, hijos) =>
        val newPath = if (charNode == ' ') currentPath else currentPath :+ charNode
        val fromChildren = hijos.flatMap(h => obtenerCadenasDeLongitudN(h, newPath, targetLength))
        val currentResult = if (newPath.length == targetLength && marcadaNode && charNode != ' ') List(newPath) else List.empty
        currentResult ++ fromChildren
      case Hoja(charLeaf, marcadaLeaf) =>
        val newPath = currentPath :+ charLeaf
        if (newPath.length == targetLength && marcadaLeaf) {
          List(newPath)
        } else {
          List.empty
        }
    }
  }
}
