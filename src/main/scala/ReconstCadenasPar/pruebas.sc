import Oraculo.*
import ReconstCadenasPar._

val seq = List('g', 'c', 'g', 'a')
val oraculo = crearOraculo(1)(seq)

reconstruirCadenaIngenuoPar(4, oraculo)
reconstruirCadenaTurboPar(2)(4, oraculo)

