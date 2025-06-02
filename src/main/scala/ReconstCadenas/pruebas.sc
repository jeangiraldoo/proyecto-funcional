import Oraculo._
import ReconstCadenas._

val seq = List('g', 'c', 'g', 'a')
val oraculo = crearOraculo(1)(seq)

reconstruirCadenaIngenua(4, oraculo)
reconstruirCadenaIngenuaPar(4, oraculo)
