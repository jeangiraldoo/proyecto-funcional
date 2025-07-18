import Oraculo._
import ReconstCadenas._
import scala.util.Random

val random = new Random()

def secAlAzar(long:Int, s:Seq[Char]): Seq[Char] = {
  //Crea una secuencia de long caracteres del alfabeto,
  // escogidos de forma aleatoria, terminando en s
  if (s.length==long) s
  else {
    val indiceAzar=random.nextInt(4)
    secAlAzar(long,alfabeto(indiceAzar)+:s)
  }
}
val costoOraculo = 1

val sec1=Seq('a', 'c', 'c', 'a')
val sec2 = Seq('a', 'c', 'g', 'c', 'a')
val sec3=secAlAzar(10,Seq())

val or_1=crearOraculo(costoOraculo)(sec1)
val or_2=crearOraculo(costoOraculo)(sec2)
val or_3=crearOraculo(costoOraculo)(sec3)

reconstruirCadenaIngenuo(sec1.length, or_1)
reconstruirCadenaIngenuo(sec2.length, or_2)
reconstruirCadenaIngenuo(sec3.length, or_3)

def secsCortasParaPruebas(n:Int):Seq[Seq[Char]] = for {
  i <- 1 to n
  s = secAlAzar(i,Seq())
} yield s

def secsLargasParaPruebas(n:Int):Seq[Seq[Char]] = for {
  i <- 1 to n
  s = secAlAzar(math.pow(2,i).toInt,Seq())
} yield s

def pruebasIngenuo (ss:Seq[Seq[Char]]) = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s,reconstruirCadenaIngenuo(s.length,o))

def pruebasTurbo (ss:Seq[Seq[Char]]) = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s.length, s,reconstruirCadenaTurbo(s.length,o))

// Secuencias para pruebas
val ss1_10=secsCortasParaPruebas(10)
val ss1_16=secsCortasParaPruebas(16)
val ss2_1024 = secsLargasParaPruebas(10)
val ss2_2048 = secsLargasParaPruebas(11)
val ss2_4096 = secsLargasParaPruebas(12)
val s1_8 = ss1_10(7)
val s2_8 = ss1_16(7)
val s1_10 = ss1_10.reverse(0)
val s1_16 = ss1_16(15)
val s1_32 = ss2_1024(4)
val s2_32 = ss2_2048(4)
val s3_32 = ss2_4096(4)
val s1_64 = ss2_1024(5)
val s2_64 = ss2_2048(5)
val s3_64 = ss2_4096(5)
val s1_128 = ss2_1024(6)
val s2_128 = ss2_2048(6)
val s1_256 = ss2_1024(7)
val s2_256 = ss2_2048(7)
val s1_512 = ss2_1024(8)
val s1_1024 = ss2_1024(9)

// Pruebas funcionales

// secuencias de longitud 8
println("=== Pruebas longitud 8 ===")
reconstruirCadenaIngenuo(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaIngenuo(s2_8.length, crearOraculo(costoOraculo)(s2_8))
reconstruirCadenaTurbo(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaTurbo(s2_8.length, crearOraculo(costoOraculo)(s2_8))

// secuencias de longitud 16
println("\n=== Pruebas longitud 16 ===")
reconstruirCadenaTurbo(s1_16.length, crearOraculo(costoOraculo)(s1_16))

// secuencias de longitud 32
println("\n=== Pruebas longitud 32 ===")
reconstruirCadenaTurbo(s1_32.length, crearOraculo(costoOraculo)(s1_32))
reconstruirCadenaTurbo(s2_32.length, crearOraculo(costoOraculo)(s2_32))
reconstruirCadenaTurbo(s3_32.length, crearOraculo(costoOraculo)(s3_32))

// secuencias de longitud 64
println("\n=== Pruebas longitud 64 ===")
reconstruirCadenaTurbo(s1_64.length, crearOraculo(costoOraculo)(s1_64))
reconstruirCadenaTurbo(s2_64.length, crearOraculo(costoOraculo)(s2_64))
reconstruirCadenaTurbo(s3_64.length, crearOraculo(costoOraculo)(s3_64))

// secuencias de longitud 128
println("\n=== Pruebas longitud 128 ===")
reconstruirCadenaTurbo(s1_128.length, crearOraculo(costoOraculo)(s1_128))
reconstruirCadenaTurbo(s2_128.length, crearOraculo(costoOraculo)(s2_128))

// secuencias de longitud 256
println("\n=== Pruebas longitud 256 ===")
reconstruirCadenaTurbo(s1_256.length, crearOraculo(costoOraculo)(s1_256))
reconstruirCadenaTurbo(s2_256.length, crearOraculo(costoOraculo)(s2_256))

// secuencias de longitud 512
println("\n=== Pruebas longitud 512 ===")
reconstruirCadenaTurbo(s1_512.length, crearOraculo(costoOraculo)(s1_512))

// secuencias de longitud 1024
println("\n=== Pruebas longitud 1024 ===")
reconstruirCadenaTurbo(s1_1024.length, crearOraculo(costoOraculo)(s1_1024))

// Pruebas por lotes
println("\n=== Pruebas por lotes - Ingenuo ===")
pruebasIngenuo(ss1_10)
pruebasIngenuo(ss1_16.slice(0,10))
// con n=13 casi no puede, con n= 14, no pudo

println("\n=== Pruebas por lotes - Turbo ===")
pruebasTurbo(ss1_10)
pruebasTurbo(ss1_16)
pruebasTurbo(ss2_1024.slice(0,10))