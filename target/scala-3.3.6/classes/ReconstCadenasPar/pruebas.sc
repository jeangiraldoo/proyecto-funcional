import Oraculo.*
import ReconstCadenas.*
import ReconstCadenasPar.*
import org.scalameter.*
import scala.util.Random
import Benchmark.*

val random = new Random()
val alfabeto = Seq('a', 'c', 'g', 't')

//METODO DE COMPARACION DE ALGORITMOS

//METODO PARA GENERAR CADENAS

def generarSecuencia(n: Int): Seq[Char] = {
  val longitud = math.pow(2, n).toInt
  Seq.fill(longitud)(alfabeto(Random.nextInt(alfabeto.length)))
}

//METODO PARA GENERAR LA PRUEBA

def generarPrueba(MIN:Int,MAX:Int,nPrueba: Int, algorSec:(Int,Oraculo) => Seq[Char],
                  algorPar:Int=>(Int,Oraculo) => Seq[Char], umbral:Int ) : Seq[String] = {
  val reportes = for {
    l <- MIN to MAX
    n <- 1 to nPrueba
    secuencia = generarSecuencia(l)
    o = crearOraculo(1)(secuencia)
    resultado = compararAlgoritmosPar(
      algorSec,
      algorPar
    )(umbral)(secuencia.length, o)
  } yield s"Cadenas ${secuencia.length} | Prueba $n | Resultado: $resultado"

  reportes
}

/*
* Generar Prueba recibe 6 parametros:
* 1. longitud minima de los caracteres que se van a probar
* 2. longitud Maxima de los caracteres que se van a probar
* 3. numero de pruebas maximas que se van a hacer, de 1 hasta el valor ingresado
* 4. algoritmo secuencial a comparar
* 5. algoritmo paralelo a comparar
* 6. umbral del algoritmo secuencial
* */

val generarPruebas:Seq[String] = generarPrueba(2,10,2,reconstruirCadenaTurboMejorada,reconstruirCadenaTurboMejoradaPar,0)


generarPruebas.foreach(println)