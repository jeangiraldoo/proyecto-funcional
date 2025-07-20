import Oraculo._
import ReconsCadenas._

val seq = List('g', 'c', 'g', 'a')
val oraculo = crearOraculo(1)(seq)

reconstruirCadenaIngenuo(4, oraculo)
reconstruirCadenaMejorado(4, oraculo)
