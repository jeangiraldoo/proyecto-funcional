// ArbolSufijos/package.scala
package object ArbolSufijos {

  // Definición de las clases Trie, Nodo y Hoja tal como se proporciona en el enunciado.
  abstract class Trie
  case class Nodo(car: Char, marcada: Boolean, hijos: List[Trie]) extends Trie
  case class Hoja(car: Char, marcada: Boolean) extends Trie

  // Función para obtener la raíz de un Trie.
  def raiz(t: Trie): Char = {
    t match {
      case Nodo(c, _, _) => c
      case Hoja(c, _) => c
    }
  }

  // Función para obtener las cabezas (caracteres de los hijos directos) de un Trie.
  def cabezas(t: Trie): Seq[Char] = {
    t match {
      case Nodo(_, _, lt) => lt.map(t => raiz(t))
      case Hoja(c, _) => Seq[Char](c)
    }
  }

  /**
   * Verifica si una secuencia `s` pertenece al Trie `t`. [cite: 71]
   *
   * @param s La secuencia (cadena) a buscar.
   * @param t El Trie donde buscar.
   * @return `true` si `s` está en el Trie y el nodo final está marcado como una palabra, `false` en caso contrario. [cite: 71]
   * Utiliza recursión y reconocimiento de patrones.
   */
  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    (s.headOption, t) match {
      case (Some(charBuscado), Nodo(carNodo, marcadaNodo, hijos)) =>
        // Si el caracter del nodo actual coincide con el caracter buscado.
        if (charBuscado == carNodo) {
          if (s.tail.isEmpty) {
            // Si es el último caracter de 's', verificamos si el nodo actual está marcado.
            marcadaNodo
          } else {
            // Buscamos el hijo que corresponde al siguiente caracter de 's'.
            hijos.find(hijo => raiz(hijo) == s.tail.headOption.getOrElse('\u0000')) match {
              case Some(hijoCoincidente) => pertenece(s.tail, hijoCoincidente)
              case None => false // No se encontró el camino para el resto de la secuencia.
            }
          }
        } else {
          false // El caracter no coincide con el nodo actual.
        }
      case (Some(charBuscado), Hoja(carHoja, marcadaHoja)) =>
        // Si es una Hoja, solo coincide si 's' es exactamente este caracter y está marcada.
        charBuscado == carHoja && s.tail.isEmpty && marcadaHoja
      case (None, Nodo(_, marcadaNodo, _)) =>
        // Si la secuencia 's' es vacía, solo es "parte" del Trie si el nodo actual está marcado.
        marcadaNodo
      case (None, Hoja(_, marcadaHoja)) =>
        // Si la secuencia 's' es vacía, solo es "parte" del Trie si la hoja actual está marcada.
        marcadaHoja
      case _ => false // Cualquier otro caso (e.g., Trie vacío cuando se espera un nodo).
    }
  }

  /**
   * Adiciona una secuencia `s` a un Trie `t` y devuelve el Trie resultante. [cite: 71]
   *
   * @param s La secuencia (cadena) a adicionar.
   * @param t El Trie original.
   * @return Un nuevo Trie con la secuencia `s` adicionada. [cite: 71]
   * Utiliza recursión y reconocimiento de patrones.
   */
  def adicionar(s: Seq[Char], t: Trie): Trie = {
    s.headOption match {
      case Some(charToAdd) =>
        t match {
          case Nodo(carNodo, marcadaNodo, hijos) =>
            if (carNodo == charToAdd) { // Si el caracter del nodo coincide con el actual de la secuencia
              val (hijosCoincidentes, otrosHijos) = hijos.partition(hijo => raiz(hijo) == s.tail.headOption.getOrElse('\u0000'))
              hijosCoincidentes.headOption match {
                case Some(hijoExistente) =>
                  // Si el hijo para el siguiente caracter ya existe, se actualiza recursivamente.
                  Nodo(carNodo, marcadaNodo, adicionar(s.tail, hijoExistente) :: otrosHijos)
                case None =>
                  // Si el hijo para el siguiente caracter no existe, se crea un nuevo camino.
                  Nodo(carNodo, marcadaNodo, construirTrieDesdeSecuencia(s.tail) :: hijos)
              }
            } else if (carNodo == ' ' && s.length == 1) { // Caso especial para la raíz vacía que se convierte en hoja
              Hoja(charToAdd, true)
            }
            else { // Si el caracter de la raíz no coincide, se asume que 't' es una raíz vacía
              // que debe ser actualizada para el primer caracter de 's'.
              // O se añade el camino completo si 't' ya tiene otros caminos y 'charToAdd' es un nuevo inicio.
              val (matchingChild, otherChildren) = hijos.partition(h => raiz(h) == charToAdd)
              matchingChild.headOption match {
                case Some(child) =>
                  Nodo(carNodo, marcadaNodo, adicionar(s.tail, child) :: otherChildren)
                case None =>
                  Nodo(carNodo, marcadaNodo, construirTrieDesdeSecuencia(s) :: hijos)
              }
            }

          case Hoja(carHoja, marcadaHoja) =>
            if (charToAdd == carHoja) {
              if (s.tail.isEmpty) Hoja(carHoja, true) // Si la secuencia termina aquí, solo se marca la hoja.
              else {
                // Si la secuencia continúa, la Hoja se convierte en un Nodo para añadir hijos.
                Nodo(carHoja, marcadaHoja, List(construirTrieDesdeSecuencia(s.tail)))
              }
            } else {
              // Si la hoja no coincide, esto no debería ocurrir en una inserción normal de sufijos
              // a menos que la `adicionar` se llame con un trie que no es raíz y la cadena no es prefijo.
              // Para el contexto del árbol de sufijos, asumimos que estamos extendiendo un prefijo existente o añadiendo desde la raíz.
              throw new IllegalArgumentException(s"No se puede adicionar '$s' a una Hoja con caracter diferente '$carHoja'.")
            }
        }
      case None =>
        // Si la secuencia a adicionar es vacía, el nodo actual se marca como una palabra completa.
        t match {
          case Nodo(car, _, hijos) => Nodo(car, true, hijos)
          case Hoja(car, _) => Hoja(car, true)
        }
    }
  }

  // Función auxiliar para construir un Trie a partir de una secuencia dada.
  // Utiliza recursión.
  private def construirTrieDesdeSecuencia(s: Seq[Char]): Trie = {
    s.headOption match {
      case Some(c) =>
        if (s.tail.isEmpty) Hoja(c, true) // Si es el último caracter, es una Hoja y está marcada.
        else Nodo(c, false, List(construirTrieDesdeSecuencia(s.tail))) // Es un Nodo y el resto de la secuencia es un hijo.
      case None =>
        // Esto no debería ser llamado con una secuencia vacía para construir un Trie.
        throw new IllegalArgumentException("No se puede construir un Trie desde una secuencia vacía.")
    }
  }

  /**
   * Construye el árbol de sufijos para una secuencia de secuencias `ss`. [cite: 72]
   *
   * @param ss La secuencia de secuencias (cadenas) para las cuales se construirá el árbol de sufijos. [cite: 72]
   * @return El Trie que representa el árbol de sufijos de todas las secuencias en `ss`. [cite: 72]
   * Utiliza funciones de alto orden (`foldLeft`, `map`) e iteradores (`until`).
   */
  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    // La raíz de un árbol de sufijos es a menudo un nodo vacío que representa el prefijo nulo.
    // Usamos un Nodo con un caracter que no se espera en el alfabeto (e.g., ' ').
    val trieInicial: Trie = Nodo(' ', false, List())

    // Usa foldLeft para procesar cada secuencia en 'ss'.
    // Para cada secuencia, genera todos sus sufijos y los adiciona al Trie acumulado.
    ss.foldLeft(trieInicial) { (accTrie, currentSeq) =>
      // Genera todos los sufijos de la `currentSeq`.
      // Uso de iterador `until` y función de alto orden `map`.
      val sufijos = (0 until currentSeq.length).map(i => currentSeq.drop(i))

      // Usa foldLeft para adicionar cada sufijo generado al Trie.
      sufijos.foldLeft(accTrie) { (innerAccTrie, sufijo) =>
        adicionar(sufijo, innerAccTrie) // Adiciona el sufijo usando la función `adicionar`.
      }
    }
  }
}