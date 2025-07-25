import Oraculo._
import Common._
import scala.collection.parallel.CollectionConverters._
import ArbolSufijos._


package object ReconstCadenasPar {
  val alfabeto = Seq('a', 'c', 'g', 't')

  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, oraculo: Oraculo): Seq[Char] = {
    def expand(seqs: List[Seq[Char]]): List[Seq[Char]] =
      for {
        seq <- seqs
        ch <- alfabeto
      } yield seq :+ ch

    def parallelFilter(seqs: List[Seq[Char]]): List[Seq[Char]] = {
      if (seqs.length >= umbral) {
        val quarter = seqs.length / 4
        val (q1, r1) = seqs.splitAt(quarter)
        val (q2, r2) = r1.splitAt(quarter)
        val (q3, q4) = r2.splitAt(quarter)
        val (v1, v2, v3, v4) = parallel(
          q1.filter(oraculo),
          q2.filter(oraculo),
          q3.filter(oraculo),
          q4.filter(oraculo)
        )
        v1 ++ v2 ++ v3 ++ v4
      } else {
        seqs.filter(oraculo)
      }
    }

    def parallelExpand(seqs: List[Seq[Char]]): List[Seq[Char]] = {
      if (seqs.length >= umbral) {
        val quarter = seqs.length / 4
        val (q1, r1) = seqs.splitAt(quarter)
        val (q2, r2) = r1.splitAt(quarter)
        val (q3, q4) = r2.splitAt(quarter)
        val (x1, x2, x3, x4) = parallel(
          expand(q1),
          expand(q2),
          expand(q3),
          expand(q4)
        )
        x1 ++ x2 ++ x3 ++ x4
      } else {
        expand(seqs)
      }
    }

    def loop(currentLevelSequences: List[Seq[Char]]): Seq[Char] = {
      if (currentLevelSequences.isEmpty) return Seq.empty

      val validSequences = parallelFilter(currentLevelSequences)

      validSequences.find(_.length == n) match {
        case Some(solution) => solution
        case None =>
          val nextLevelSequences = parallelExpand(validSequences)
          loop(nextLevelSequences)
      }
    }

    loop(List(Seq.empty))
  }

  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, oracle: Oraculo): Seq[Char] = {
    def loop(stack: Seq[Seq[Char]]): Seq[Char] = stack match {
      case Seq() => Seq.empty
      case current +: rest =>
        if (current.length == n) current
        else {
          val expanded = alfabeto.map(c => current :+ c)

          val filtered: Seq[Seq[Char]] =
            if (expanded.length >= umbral) {
              val size = expanded.length / 4
              val (q1, r1) = expanded.splitAt(size)
              val (q2, r2) = r1.splitAt(size)
              val (q3, q4) = r2.splitAt(size)

              val (f1, f2, f3, f4) = parallel(
                q1.filter(oracle),
                q2.filter(oracle),
                q3.filter(oracle),
                q4.filter(oracle)
              )
              f1 ++ f2 ++ f3 ++ f4
            } else {
              expanded.filter(oracle)
            }

          loop(filtered ++ rest)
        }
    }

    loop(Seq(Seq.empty))
  }

  def reconstruirCadenaMejoradoPar1(umbral: Int)(n: Int, oracle: Oraculo): Seq[Char] = {
    @annotation.tailrec
    def loop(stack: Seq[Seq[Char]]): Seq[Char] = stack match {
      case Seq() => Seq.empty
      case current +: rest =>
        if (current.length == n) current
        else {
          val expanded = alfabeto.map(c => current :+ c)

          val filtered: Seq[Seq[Char]] =
            if (expanded.size >= umbral) expanded.par.filter(oracle).toList
            else expanded.filter(oracle)

          loop(filtered ++ rest)
        }
    }

    loop(Seq(Seq.empty))
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

  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    require(n > 0 && (n & (n - 1)) == 0, "n debe ser potencia de 2 y mayor que 0")

    @tailrec
    def reconstruirRec(sc: Seq[Seq[Char]], k: Int): Seq[Char] = {
      if (k > n) {
        Seq.empty
      } else {
        val tree = arbolDeSufijos(sc)
        val useParallel = sc.size > umbral

        val combinaciones = for {
          s1 <- sc
          s2 <- sc
        } yield s1 ++ s2

        val filtradas = if (useParallel) {
          combinaciones.par.filter(comb =>
            comb.sliding(k / 2).forall(sub => pertenece(sub.toSeq, tree))
          ).seq
        } else {
          combinaciones.filter(comb =>
            comb.sliding(k / 2).forall(sub => pertenece(sub.toSeq, tree))
          )
        }

        val validas = if (useParallel) {
          filtradas.par.filter(o).seq
        } else {
          filtradas.filter(o)
        }

        validas.find(_.length == n) match {
          case Some(sol) => sol
          case None => reconstruirRec(validas, k * 2)
        }
      }
    }

    val sc = alfabeto.map(c => Seq(c)).filter(o)
    if (n == 1) sc.head else reconstruirRec(sc, 2)
  }




}