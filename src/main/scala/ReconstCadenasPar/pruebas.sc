import Oraculo.*
import ReconstCadenasPar._
import ReconstCadenas._

val base = List('g', 'c', 'g', 'a')
val seq = List.fill(1024 / base.length)(base).flatten
val oraculo = crearOraculo(1)(seq)

reconstruirCadenaMejorado(1024, oraculo)
reconstruirCadenaMejoradoPar(2)(1024, oraculo)
//reconstruirCadenaIngenuoPar3(512, oraculo)

//reconstruirCadenaTurboPar(2)(4, oraculo)

