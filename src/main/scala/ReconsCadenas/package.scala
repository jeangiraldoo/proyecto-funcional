import Oraculo ._
import Common ._

package object ReconsCadenas {
  val alfabeto = Seq('a', 'c', 'g', 't')

  def reconstruirCadenaIngenuo(n: Int, oraculo: Oraculo): Seq[Char] = {
    def generateAll(length: Int): Seq[Seq[Char]] = {
      if (length == 0) Seq(Seq())
      else for {
        shorter <- generateAll(length - 1)
        letter <- alfabeto
      } yield shorter :+ letter
    }

    (1 to n)
      .flatMap(generateAll)
      .find(candidate => oraculo(candidate) && candidate.length == n)
      .getOrElse(Seq())
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

  
}
