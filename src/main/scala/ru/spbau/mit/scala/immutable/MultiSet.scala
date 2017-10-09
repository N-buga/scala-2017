package ru.spbau.mit.scala.immutable

trait MultiSet[+A] {
  def filter(predicate: A => Boolean): MultiSet[A]
  def map[B] (mapFunction: A => B): MultiSet[B]
  def flatMap[B] (flatMapFunction: A => MultiSet[B]): MultiSet[B]
//  def flatMap[B] (flatMapFucntion: A => Traversable[B]): MultiSet[B]
  def foreach[U](f: A => U): Unit
  def find[B >: A](element: B): Option[B]

  def |[B >: A](another: MultiSet[B]): MultiSet[B]
  def &[B >: A](another: MultiSet[B]): MultiSet[B]
  def *(value: Int): MultiSet[A]
  def +[B >: A](element: B): MultiSet[B]

  def apply[B >: A](element: B): Int
  def remove[B >: A](element: B): (Int, MultiSet[B])

  def depth: Int
  def size: Int
}

private[immutable] case class NotEmptyNode[A](element: A, count: Int = 1, next: MultiSet[A] = EmptyNode) extends MultiSet[A] {
  override def filter(predicate: (A) => Boolean): MultiSet[A] = {
    val restFilter = next.filter(predicate)
    if (predicate(element)) {
      NotEmptyNode(element, count, restFilter)
    } else {
      restFilter
    }
  }

  override def map[B](mapFunction: (A) => B): MultiSet[B] =
    NotEmptyNode(mapFunction(element), count, next.map(mapFunction))

  override def flatMap[B](flatMapFunction: (A) => MultiSet[B]): MultiSet[B] = {
    val flatRes = flatMapFunction(element) * count
    flatRes | next.flatMap(flatMapFunction)
  }

//  override def flatMap[B](flatMapFunction: (A) => Traversable[B]): MultiSet[B] = {
//      val flatRes = MultiSet(flatMapFunction(element).toSeq: _*) * count
//      flatRes | next.flatMap(flatMapFunction)
//  }

  override def foreach[U](f: (A) => U): Unit = {
    for (_ <- 1 to count)
      f(element)
    next.foreach(f)
  }

  override def find[B >: A](elementToFind: B): Option[B] = {
    if (elementToFind == element) {
      Some(element)
    } else {
      next.find(elementToFind)
    }
  }

  override def |[B >: A](another: MultiSet[B]): MultiSet[B] = another.remove(element) match {
    case (0, _) => NotEmptyNode(element, count, next | another)
    case (num, restAnother) => NotEmptyNode(element, num + count, next | restAnother)
  }

  override def &[B >: A](another: MultiSet[B]): MultiSet[B] = {
    val restAnd = next & another
    another(element) match {
      case 0 => restAnd
      case anotherCount => NotEmptyNode(element, count.min(anotherCount), restAnd)
    }
  }

  override def *(value: Int): MultiSet[A] = {
    NotEmptyNode(element, count * value, next * value)
  }

  override def +[B >: A](element: B): MultiSet[B] = {
    NotEmptyNode(element) | this
  }

  override def apply[B >: A](element: B): Int = {
    if (this.element == element) {
      count
    } else {
      next.apply(element)
    }
  }

  override def remove[B >: A](element: B): (Int, MultiSet[B]) = {
    if (this.element == element) {
      (count, next)
    } else {
      val (countEl, newNext) = next.remove(element)
      (countEl, NotEmptyNode(this.element, count, newNext))
    }
  }

  override def depth: Int = next.depth + 1

  override def size: Int = count + next.size
}

private[immutable] case object EmptyNode extends MultiSet[Nothing] {
  override def filter(predicate: (Nothing) => Boolean): MultiSet[Nothing] = EmptyNode

  override def map[B](mapFunction: (Nothing) => B): MultiSet[B] = EmptyNode

  override def flatMap[B](flatMapFunction: (Nothing) => MultiSet[B]): MultiSet[B] = EmptyNode

//  override def flatMap[B](flatMapFucntion: (Nothing) => Iterable[B]): MultiSet[B] = EmptyNode

  override def foreach[U](f: (Nothing) => U): Unit = {}

  override def find[B >: Nothing](element: B): Option[B] = None

  override def |[B >: Nothing](another: MultiSet[B]): MultiSet[B] = another

  override def &[B >: Nothing](another: MultiSet[B]): MultiSet[B] = EmptyNode

  override def *(value: Int): MultiSet[Nothing] = EmptyNode

  override def +[B >: Nothing](element: B): MultiSet[B] = NotEmptyNode(element)

  override def apply[B >: Nothing](element: B): Int = 0

  override def remove[B >: Nothing](element: B): (Int, MultiSet[B]) = (0, EmptyNode)

  override def depth: Int = 0

  override def size: Int = 0
}

object MultiSet {
  def apply[A](elements: A*): MultiSet[A] = {
    elements.foldRight(EmptyNode: MultiSet[A])((value, multiSet) => NotEmptyNode(value) | multiSet)
  }

  def unapplySeq[A](arg: MultiSet[A]): Option[Seq[A]] = arg match {
    case EmptyNode => None
    case NotEmptyNode(element, count, next) => Some(Seq.fill(count)(element) ++ unapplySeq(next).getOrElse(Seq.empty))
  }
}