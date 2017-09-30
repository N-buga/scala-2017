package ru.spbau.mit

trait ImmutableMultiSet[+A] extends {
  def filter(predicate: A => Boolean): ImmutableMultiSet[A]
  def map[B] (mapFunction: A => B): ImmutableMultiSet[B]
  def flatMap[B] (flatMapFunction: A => ImmutableMultiSet[B]): ImmutableMultiSet[B]
  def foreach(consumer: A => Unit): Unit
  def find[B >: A](element: B): Option[B]

  def |[B >: A](another: ImmutableMultiSet[B]): ImmutableMultiSet[B]
  def &[B >: A](another: ImmutableMultiSet[B]): ImmutableMultiSet[B]
  def *(value: Int): ImmutableMultiSet[A]
  def +[B >: A](element: B): ImmutableMultiSet[B]

  def apply[B >: A](element: B): Int
  def remove[B >: A](element: B): (Int, ImmutableMultiSet[B])

  def depth: Int
  def size: Int
}

case class NoEmptyNode[+A](element: A, count: Int, next: ImmutableMultiSet[A]) extends ImmutableMultiSet[A] {
  override def filter(predicate: (A) => Boolean): ImmutableMultiSet[A] = {
    if (predicate(element)) {
      NoEmptyNode(element, count, next.filter(predicate))
    } else {
      next.filter(predicate)
    }
  }

  override def map[B](mapFunction: (A) => B): ImmutableMultiSet[B] = {
    NoEmptyNode(mapFunction(element), count, next.map(mapFunction))
  }

  override def flatMap[B](flatMapFunction: (A) => ImmutableMultiSet[B]): ImmutableMultiSet[B] = {
    var flatRes = flatMapFunction(element) * count
    flatRes | next.flatMap(flatMapFunction)
  }

  override def foreach(consumer: (A) => Unit): Unit = {
    for (i <- Range(0, count))
      consumer(element)
    next.foreach(consumer)
  }

  override def find[B >: A](elementToFind: B): Option[B] = {
    if (elementToFind == element) {
      Some(element)
    } else {
      next.find(elementToFind)
    }
  }

  override def |[B >: A](another: ImmutableMultiSet[B]): ImmutableMultiSet[B] = another.remove(element) match {
    case (0, _) => NoEmptyNode(element, count, next | another)
    case (num, restAnother) => NoEmptyNode(element, num + count, next | restAnother)
  }

  override def &[B >: A](another: ImmutableMultiSet[B]): ImmutableMultiSet[B] = {
    val countAnotherMultisetEl = another(element)
    if (countAnotherMultisetEl != 0) {
      NoEmptyNode(element, count.min(countAnotherMultisetEl), next&another)
    } else {
      next&another
    }
  }

  override def *(value: Int): ImmutableMultiSet[A] = {
    NoEmptyNode(element, count*value, next*value)
  }

  override def +[B >: A](element: B): ImmutableMultiSet[B] = {
    NoEmptyNode(element, 1, EmptyNode) | this
  }

  override def apply[B >: A](element: B): Int = {
    if (this.element == element) {
      count
    } else {
      next.apply(element)
    }
  }

  override def remove[B >: A](element: B): (Int, ImmutableMultiSet[B]) = {
    if (this.element == element) {
      (count, next)
    } else {
      val (countEl, new_next) = next.remove(element)
      (countEl, NoEmptyNode(this.element, count, new_next))
    }
  }

  override def depth: Int = next.depth + 1

  override def size: Int = count + next.size
}

case object EmptyNode extends ImmutableMultiSet[Nothing] {
  override def filter(predicate: (Nothing) => Boolean): ImmutableMultiSet[Nothing] = EmptyNode

  override def map[B](mapFunction: (Nothing) => B): ImmutableMultiSet[B] = EmptyNode

  override def flatMap[B](flatMapFunction: (Nothing) => ImmutableMultiSet[B]): ImmutableMultiSet[B] = EmptyNode

  override def foreach(consumer: (Nothing) => Unit): Unit = {}

  override def find[B >: Nothing](element: B): Option[B] = None

  override def |[B >: Nothing](another: ImmutableMultiSet[B]): ImmutableMultiSet[B] = another

  override def &[B >: Nothing](another: ImmutableMultiSet[B]): ImmutableMultiSet[B] = EmptyNode

  override def *(value: Int): ImmutableMultiSet[Nothing] = EmptyNode

  override def +[B >: Nothing](element: B): ImmutableMultiSet[B] = NoEmptyNode(element, 1, EmptyNode)

  override def apply[B >: Nothing](element: B): Int = 0

  override def remove[B >: Nothing](element: B): (Int, ImmutableMultiSet[B]) = (0, EmptyNode)

  override def depth: Int = 0

  override def size: Int = 0
}

object ImmutableMultiSet {
  def apply[A](elements: A*): ImmutableMultiSet[A] = {
    elements.foldRight[ImmutableMultiSet[A]](EmptyNode)((value, multiSet) => NoEmptyNode(value, 1, EmptyNode) | multiSet)
  }

  def unapplySeq[A](arg: ImmutableMultiSet[A]): Option[Seq[A]] = arg match {
    case EmptyNode => None
    case NoEmptyNode(el, count, next) => Some(Seq.fill(count)(el) ++ unapplySeq(next).getOrElse(Seq.empty))
  }
}