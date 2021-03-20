package u03

import u02.Optionals._

object Lists {

  // A generic linkedlist
  sealed trait List[E]

  // a companion object (i.e., module) for List
  object List {
    case class Cons[E](head: E, tail: List[E]) extends List[E]
    case class Nil[E]() extends List[E]

    def sum(l: List[Int]): Int = l match {
      case Cons(h, t) => h + sum(t)
      case _ => 0
    }

    def append[A](l1: List[A], l2: List[A]): List[A] = (l1, l2) match {
      case (Cons(h, t), l2) => Cons(h, append(t, l2))
      case _ => l2
    }

    def map[A,B](l: List[A])(mapper: A=>B): List[B] = l match {
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()
    }

    def filter[A](l1: List[A])(pred: A=>Boolean): List[A] = l1 match {
      case Cons(h,t) if (pred(h)) => Cons(h, filter(t)(pred))
      case Cons(_,t) => filter(t)(pred)
      case Nil() => Nil()
    }

    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Cons(_,t) if n>0 => drop(t, n-1)
      case Cons(h,t) => Cons(h,t)
      case Nil() => Nil()
    }

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
      case Cons(h,t) => append(f(h), flatMap(t)(f))//Cons(f(h), flatMap(t)(f))
      case Nil() => Nil()
    }

    def mapFlat[A,B](l: List[A])(mapper: A=>B): List[B] = l match {
      case Cons(h,t) => append(Cons(mapper(h),Nil()), mapFlat(t)(mapper))
      case Nil() => Nil()
    }

    def mapFlat2[A,B](l: List[A])(mapper: A=>B): List[B] = l match {
      case Cons(_,_) =>  flatMap(l)(x => Cons[B](mapper(x), Nil()))
      case Nil() => Nil()
    }

    def filterFlat[A](l1: List[A])(pred: A=>Boolean): List[A] = l1 match {
      case Cons(h,t) if (pred(h)) =>  flatMap(l1)(x => Cons(x, Nil()))
      //case Cons(h,t) if (pred(h)) => append(Cons(h,Nil()), filterFlat(t)(pred))
      case Cons(_,t) => filterFlat(t)(pred)
      case Nil() => Nil()
    }

    def max(l: List[Int]): Option[Int] = {
      def _max(l2: List[Int], n: Int, n_times: Int): Option[Int] = l2 match {
        case Nil() if n_times == 0 => Option.None()
        case Nil() => Option.Some(n)
        case Cons(h,t) => _max(filter(t)(x => x>h), h, n_times + 1)
      }
      _max(l, 0, 0)
    }
  }
}

object ListsMain extends App {
  import Lists._

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60
  import List._
  println(append(Cons(5, Nil()), l)) // 5,10,20,30
  println(filter[Int](l)(_ >=20)) // 20,30
}