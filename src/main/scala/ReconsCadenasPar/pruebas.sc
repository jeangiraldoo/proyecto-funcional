import Oraculo.*
import ReconstCadenasPar.reconstruirCadenaTurboPar

val seq = List('g', 'c', 'g', 'a')
val oraculo = crearOraculo(1)(seq)

reconstruirCadenaTurboPar(2)(4, oraculo)

