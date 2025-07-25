package object ArbolSufijos {
  abstract class Trie
  case class Nodo(car: Char, marcada: Boolean, hijos: List[Trie]) extends Trie
  case class Hoja(car: Char, marcada: Boolean) extends Trie

  def raiz(t: Trie): Char = t match {
    case Nodo(c, _, _) => c
    case Hoja(c, _)    => c
  }
  def cabezas(t: Trie): Seq[Char] = t match {
    case Nodo(_, _, xs) => xs.map(raiz)
    case Hoja(c, _)     => Seq(c)
  }

  def pertenece(s: Seq[Char], t: Trie): Boolean = (s, t) match {
    case (Nil, nodo: Nodo) => nodo.marcada
    case (Nil, hoja: Hoja) => hoja.marcada
    case (c +: cs, Nodo(_, _, hijos)) =>
      hijos.find(n => raiz(n) == c) match {
        case Some(child) => pertenece(cs, child)
        case None        => false
      }
    case (c +: cs, Hoja(_, _)) => false
  }

  def adicionar(s: Seq[Char], t: Trie): Trie = (s, t) match {
    case (Nil, Nodo(c, _, hijos)) => Nodo(c, true, hijos)
    case (Nil, Hoja(c, _))        => Hoja(c, true)
    case (c +: cs, Nodo(ch, marca, hijos)) =>
      // revisar si existe un hijo con caracter c
      val (antes, resto) = hijos.span(n => raiz(n) != c)
      val actualizado = resto match {
        case h :: tail => adicionar(cs, h) :: tail
        case Nil if cs.isEmpty => Hoja(c, true) :: Nil
        case Nil => Nodo(c, false, List()) match {
          case nuevo => adicionar(cs, nuevo) :: Nil
        }
      }
      Nodo(ch, marca, antes ++ actualizado)
    case (c +: cs, hoja @ Hoja(ch, marca)) =>
      // transformar Hoja en Nodo
      val nuevoHijos: List[Trie] = adicionar(cs, Hoja(c, marcada = cs.isEmpty)) :: Nil
      Nodo(ch, marca, nuevoHijos)
  }

  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    // raíz vacía
    val emptyRoot: Trie = Nodo(' ', false, List())
    ss.foldLeft(emptyRoot) { (root, seq) =>
      // por cada sufijo de seq
      seq.indices.foldLeft(root) { (r, i) =>
        val suffix = seq.drop(i)
        adicionar(suffix, r)
      }
    }
  }
}