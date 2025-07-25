import Oraculo.*
import ReconstCadenas.*
import ReconstCadenasPar.*
import ArbolSufijos.*
import scala.util.Random
import scala.concurrent.{Future, TimeoutException}
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

object BenchmarkTurboAcelerada {
  val random = new Random()
  val alfabetoLocal = Seq('a', 'c', 'g', 't')

  // Generar cadenas aleatorias de longitud potencia de 2
  def generarSecuencia(exp: Int): Seq[Char] = {
    val longitud = math.pow(2, exp).toInt
    Seq.fill(longitud)(alfabetoLocal(random.nextInt(alfabetoLocal.length)))
  }

  // Función para medir tiempo de ejecución
  def medirTiempo[T](codigo: => T): (T, Long) = {
    val inicio = System.currentTimeMillis()
    val resultado = codigo
    val fin = System.currentTimeMillis()
    (resultado, fin - inicio)
  }

  // Función segura para ejecutar algoritmos con timeout
  def ejecutarConTimeout[T](timeout: Long)(codigo: => T): Either[String, T] = {
    try {
      val future = Future {
        codigo
      }

      val result = scala.concurrent.Await.result(future, timeout.seconds)
      Right(result)
    } catch {
      case _: TimeoutException => Left("TIMEOUT")
      case e: Exception => Left(s"ERROR: ${e.getMessage}")
    }
  }

  // Comparar algoritmos con manejo de errores
  def compararTurboAcelerada(exp: Int, umbral: Int, timeout: Long = 30): String = {
    val secuencia = generarSecuencia(exp)
    val oraculo = crearOraculo(1)(secuencia)
    val n = secuencia.length

    println(s"  Probando secuencia: ${secuencia.take(10).mkString("")}${if(n > 10) "..." else ""} (n=$n)")

    // Ejecutar versión secuencial
    val resultadoSec = ejecutarConTimeout(timeout) {
      medirTiempo(reconstruirCadenaTurboAcelerada(n, oraculo))
    }

    // Ejecutar versión paralela
    val resultadoPar = ejecutarConTimeout(timeout) {
      medirTiempo(reconstruirCadenaTurboAceleradaPar(umbral)(n, oraculo))
    }

    (resultadoSec, resultadoPar) match {
      case (Right((resSec, tiempoSec)), Right((resPar, tiempoPar))) =>
        val correcto = resSec == resPar && resSec == secuencia
        val estadoCorreccion = if (correcto) "✓" else "✗"
        val aceleracion = if (tiempoPar > 0) tiempoSec.toDouble / tiempoPar.toDouble else 0.0
        val eficiencia = if (aceleracion > 1.2) "MEJORA" else if (aceleracion < 0.8) "PEOR" else "SIMILAR"

        f"n=$n | Sec: ${tiempoSec}ms | Par: ${tiempoPar}ms | Aceleración: ${aceleracion}%.2fx | $eficiencia | $estadoCorreccion"

      case (Left(errorSec), Right((_, tiempoPar))) =>
        f"n=$n | Sec: $errorSec | Par: ${tiempoPar}ms | SOLO_PARALELO_FUNCIONA"

      case (Right((_, tiempoSec)), Left(errorPar)) =>
        f"n=$n | Sec: ${tiempoSec}ms | Par: $errorPar | SOLO_SECUENCIAL_FUNCIONA"

      case (Left(errorSec), Left(errorPar)) =>
        f"n=$n | Sec: $errorSec | Par: $errorPar | AMBOS_FALLARON"
    }
  }

  // Generar múltiples pruebas
  def generarPruebas(minExp: Int, maxExp: Int, numPruebas: Int, umbral: Int): Seq[String] = {
    for {
      exp <- minExp to maxExp
      prueba <- 1 to numPruebas
    } yield {
      println(s"Ejecutando prueba $prueba para 2^$exp caracteres (umbral=$umbral)...")
      compararTurboAcelerada(exp, umbral, timeout = 15) // 15 segundos timeout
    }
  }

  // Encontrar umbral óptimo
  def encontrarUmbralOptimo(exp: Int, umbrales: Seq[Int], numPruebas: Int): Unit = {
    println(f"\n=== Análisis de umbrales para 2^$exp = ${math.pow(2, exp).toInt} caracteres ===")

    val resultados = for (umbral <- umbrales) yield {
      println(s"\n--- Probando umbral: $umbral ---")
      val pruebas = (1 to numPruebas).map { _ =>
        compararTurboAcelerada(exp, umbral, timeout = 10)
      }

      pruebas.foreach(println)

      // Calcular estadísticas
      val aceleraciones = pruebas.flatMap { linea =>
        val regex = """Aceleración: ([\d.]+)x""".r
        regex.findFirstMatchIn(linea).map(_.group(1).toDouble)
      }

      val promedio = if (aceleraciones.nonEmpty) aceleraciones.sum / aceleraciones.length else 0.0
      println(f"Promedio aceleración: ${promedio}%.2fx")

      (umbral, promedio)
    }

    val mejorUmbral = resultados.maxBy(_._2)
    println(f"\n🏆 MEJOR UMBRAL: ${mejorUmbral._1} con aceleración promedio de ${mejorUmbral._2}%.2fx")
  }

  // Ejecutar benchmark completo
  def ejecutarBenchmark(): Unit = {
    println("=== 🚀 BENCHMARK TURBO ACELERADA vs TURBO ACELERADA PARALELA ===")
    println("Algoritmo: Usa árboles de sufijos para optimizar el filtrado de combinaciones")

    try {
      // 1. Pruebas básicas (muy pequeñas para verificar funcionamiento)
      println("\n1️⃣ Pruebas básicas (verificación de funcionamiento)")
      val basicas = generarPruebas(2, 3, 2, 2) // 4 y 8 caracteres
      basicas.foreach(println)

      // 2. Análisis de umbrales
      println("\n2️⃣ Análisis de umbrales óptimos")
      encontrarUmbralOptimo(3, Seq(1, 2, 4), 2) // 8 caracteres

      // 3. Pruebas de rendimiento
      println("\n3️⃣ Pruebas de rendimiento")
      val rendimiento = generarPruebas(3, 4, 3, 2) // 8 y 16 caracteres
      rendimiento.foreach(println)

      // 4. Pruebas de escalabilidad (cuidadosas)
      println("\n4️⃣ Pruebas de escalabilidad (CUIDADO: puede ser lento)")
      val escalabilidad = generarPruebas(4, 5, 1, 4) // 16 y 32 caracteres
      escalabilidad.foreach(println)

      println("\n=== 📊 RESUMEN DE INTERPRETACIÓN ===")
      println("✓ = Ambos algoritmos devuelven la solución correcta")
      println("✗ = Hay diferencias en las soluciones (ERROR)")
      println("MEJORA = Aceleración > 1.2x (paralelismo beneficioso)")
      println("SIMILAR = Aceleración 0.8x - 1.2x (rendimiento comparable)")
      println("PEOR = Aceleración < 0.8x (overhead del paralelismo)")
      println("TIMEOUT = El algoritmo tardó más de lo esperado")

    } catch {
      case e: Exception =>
        println(s"Error durante el benchmark: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  // Función para análisis individual detallado
  def analizarCaso(exp: Int, umbral: Int): Unit = {
    println(f"\n=== 🔍 ANÁLISIS DETALLADO: 2^$exp = ${math.pow(2, exp).toInt} caracteres ===")

    val secuencia = generarSecuencia(exp)
    val oraculo = crearOraculo(1)(secuencia)
    val n = secuencia.length

    println(f"Secuencia objetivo: ${secuencia.mkString("")}")
    println(f"Longitud: $n caracteres")
    println(f"Umbral de paralelización: $umbral")
    println(f"Es potencia de 2: ${(n & (n - 1)) == 0}")

    println("\n⏱️ Midiendo rendimiento...")
    val resultado = compararTurboAcelerada(exp, umbral, timeout = 20)
    println(resultado)
  }

  // Función auxiliar para pruebas rápidas
  def pruebaRapida(): Unit = {
    println("=== 🏃‍♂️ PRUEBA RÁPIDA ===")
    println("Ejecutando una prueba simple para verificar que todo funciona...")

    try {
      val resultado = compararTurboAcelerada(2, 2, timeout = 10) // 4 caracteres
      println(s"Resultado: $resultado")

      if (resultado.contains("✓")) {
        println("✅ ¡Prueba exitosa! El benchmark está funcionando correctamente.")
      } else {
        println("⚠️ Hay problemas. Revisa la implementación de los algoritmos.")
      }
    } catch {
      case e: Exception =>
        println(s"❌ Error en la prueba rápida: ${e.getMessage}")
        println("Verifica que todos los paquetes estén importados correctamente.")
    }
  }
}

// VERSIÓN SEGURA PARA EJECUTAR
println("=== Iniciando benchmark para Turbo Acelerada ===")

// Primero ejecuta una prueba rápida
BenchmarkTurboAcelerada.pruebaRapida()

// Luego el benchmark completo (descomenta si la prueba rápida funciona)
// BenchmarkTurboAcelerada.ejecutarBenchmark()

// Para análisis específicos:
// BenchmarkTurboAcelerada.analizarCaso(3, 2)  // 8 caracteres
// BenchmarkTurboAcelerada.encontrarUmbralOptimo(4, Seq(2, 4, 8), 2)  // 16 caracteres

println("\n=== 💡 INSTRUCCIONES ===")
println("1. Primero ejecuta la 'pruebaRapida()' para verificar que todo funciona")
println("2. Si la prueba rápida es exitosa, descomenta 'ejecutarBenchmark()' para el análisis completo")
println("3. Usa 'analizarCaso()' para análisis detallados de casos específicos")
println("4. Usa 'encontrarUmbralOptimo()' para encontrar el mejor umbral para un tamaño dado")