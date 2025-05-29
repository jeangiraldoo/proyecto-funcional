import Oraculo._
import ReconstCadenas._

val seq = List('a', 'c', 'c', 'a')
val oraculo = crearOraculo(1)(seq)

reconstruirCadenaIngenua(4, oraculo)
