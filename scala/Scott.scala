object Scott {

  trait STuple[+A, +B] {
    def apply[C]: ((A, B) => C) => C
  }

  def toTuple[A, B](tuple: STuple[A, B]): (A, B) =
    tuple[(A, B)].apply((a: A, b: B) => (a, b))


  def fromTuple[A, B](tuple: (A, B)): STuple[A, B] = new STuple[A, B] {
    override def apply[C]: ((A, B) => C) => C = (f: (A, B) => C) => f(tuple._1, tuple._2)
  }

  def fst[A, B](tuple: STuple[A, B]): A =
    tuple[A].apply((a: A, _: B) => a)

  def snd[B](tuple: STuple[_, B]): B =
    tuple[B].apply((_, b: B) => b)

  def swap[A, B](tuple: STuple[A, B]): STuple[B, A] =
    fromTuple(toTuple(tuple).swap)

  def curry[A, B, C](f: STuple[A, B] => C): A => B => C =
    (a: A) => (b: B) => f(new STuple[A, B] {
      override def apply[C]: ((A, B) => C) => C =
        (g: (A, B) => C) => g(a, b)
    })

  def uncurry[A, B, C](f: A => B => C): STuple[A, B] => C =
    (tuple: STuple[A, B]) => tuple[C]((a: A, b: B) => f(a)(b))

  trait SOption[+A] {
    def apply[B]: (=> B, A => B) => B
  }

  def toOption[A](option: SOption[A]): Option[A] =
    option[Option[A]].apply(Option.empty[A], Some(_))


  def fromOption[A](option: Option[A]): SOption[A] = new SOption[A] {
    override def apply[B]: (=> B, A => B) => B =
      (ifEmpty, f) => option.fold(ifEmpty)(f)
  }

  def isSome(option: SOption[_]): Boolean =
    option[Boolean].apply(false, _ => true)

  def isNone(option: SOption[_]): Boolean =
    option[Boolean].apply(true, _ => false)


  trait SEither[+A, +B] {
    def apply[C]: (A => C, B => C) => C
  }

  def toEither[A, B](either: SEither[A, B]): Either[A, B] =
    either[Either[A, B]].apply(Left[A, B], Right[A, B])

  def fromEither[A, B](either: Either[A, B]): SEither[A, B] =
    new SEither[A, B] {
      override def apply[C]: (A => C, B => C) => C =
        (left: A => C, right: B => C) => either.fold(left, right)
    }

  def isLeft[A](either: SEither[A, _]): Boolean =
    either[Boolean].apply(_ => true, _ => false)

  def isRight[A](either: SEither[A, _]): Boolean =
    either[Boolean].apply(_ => false, _ => true)

  trait SList[+A] {
    def apply[B]: (=> B, (A, SList[A]) => B) => B
  }

  def catOptions[A](list: SList[SOption[A]]): SList[A] =
    list[SList[A]](nil[A], (h: SOption[A], t: SList[SOption[A]]) => {
      h[SList[A]](catOptions(t), cons(_, catOptions(t)))
    })

  def nil[A]: SList[A] = new SList[A] {
    def apply[B]: (=> B, (A, SList[A]) => B) => B =
      (b, _: (A, SList[A]) => B) => b
  }

  def toList[A](list: SList[A]): List[A] =
    list[List[A]](List.empty[A], (h: A, t: SList[A]) => h :: toList(t))

  def fromList[A](list: List[A]): SList[A] = list match {
    case Nil => nil[A]
    case h :: t => cons(h, fromList(t))
  }

  def cons[A](head: A, list: SList[A]): SList[A] =
    new SList[A] {
      override def apply[B]: (=> B, (A, SList[A]) => B) => B =
        (_, f: (A, SList[A]) => B) => f(head, list)
    }

  def concat[A](left: SList[A], right: SList[A]): SList[A] =
    left[SList[A]](right, (lh: A, lt: SList[A]) => {
      cons[A](lh, concat(lt, right))
    })

  def empty(list: SList[_]): Boolean =
    list[Boolean](true, (_, _) => false)

  def length(list: SList[_]): Int =
    list[Int](0, (_, t) => 1 + length(t))

  def map[A, B](f: A => B, list: SList[A]): SList[B] =
    list[SList[B]](nil[B], (h, t) => cons(f(h), map(f, t)))

  def zip[A, B](listA: SList[A], listB: SList[B]): SList[STuple[A, B]] =
    listA[SList[STuple[A, B]]](nil[STuple[A, B]],
      (headA, tailA) => listB[SList[STuple[A, B]]](nil[STuple[A, B]],
        (headB, tailB) => cons(fromTuple(headA, headB), zip(tailA, tailB))))

  def foldLeft[A, B](f: (B, A) => B, z: B, list: SList[A]): B =
    list[B](z, (h: A, t: SList[A]) => foldLeft(f, f(z, h), t))

  def foldRight[A, B](f: (A, B) => B, z: B, list: SList[A]): B =
    list[B](z, (h: A, t: SList[A]) => f(h, foldRight(f, z, t)))

  def take[A](n: Int, list: SList[A]): SList[A] =
    n match {
      case 0 => nil[A]
      case _ => list[SList[A]](nil[A], (h: A, t: SList[A]) => cons(h, take(n - 1, t)))
    }


  def partition[A, B](list: SList[SEither[A, B]]): STuple[SList[A], SList[B]] = {
    fromTuple(foldRight[SEither[A, B], (SList[A], SList[B])]({
      case (t, (l, r)) => t.apply(h => (cons(h, l), r), h => (l, cons(h, r)))
    }, (nil, nil), list))
  }
}
