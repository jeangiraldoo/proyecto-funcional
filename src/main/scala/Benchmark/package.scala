import org.scalameter.*
import ReconstCadenas.*
import Oraculo.*

package object Benchmark {
  
  type algoritmoCadena = (Int,Oraculo) => Seq[Char]
  type algoitmoCadenaPar = (Int) => (Int,Oraculo) => Seq[Char]
  
  def compararAlgoritmosPar(a1: algoritmoCadena, a2: algoitmoCadenaPar)
                        (umbral: Int)(m1: Int, m2: Oraculo): (Double, Double, Double) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 1),
      KeyValue(Key.exec.maxWarmupRuns -> 2),
      KeyValue(Key.verbose -> false)
    ) withWarmer (new Warmer.Default) measure (a1(m1, m2))

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 1),
      KeyValue(Key.exec.maxWarmupRuns -> 2),
      KeyValue(Key.verbose -> false)
    ) withWarmer (new Warmer.Default) measure (a2(umbral)(m1, m2))

    val speedUp = timeA1.value / timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }

  def compararAlgoritmosSec(a1: algoritmoCadena, a2: algoritmoCadena)
                           (m1: Int, m2: Oraculo): (Double, Double, Double) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 30),
      KeyValue(Key.verbose -> false)
    ) withWarmer (new Warmer.Default) measure (a1(m1, m2))

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 30),
      KeyValue(Key.verbose -> false)
    ) withWarmer (new Warmer.Default) measure (a2(m1, m2))

    val speedUp = timeA1.value / timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }
  
  
}
