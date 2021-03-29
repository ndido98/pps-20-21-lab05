package u05lab.code

import scala.annotation.tailrec
import scala.language.postfixOps // silence warnings

sealed trait List[A] {

    def head: Option[A]

    def tail: Option[List[A]]

    def append(list: List[A]): List[A]

    def foreach(consumer: (A) => Unit): Unit

    def get(pos: Int): Option[A]

    def filter(predicate: (A) => Boolean): List[A]

    def map[B](fun: (A) => B): List[B]

    def toSeq: Seq[A]

    def foldLeft[B](acc: B)(f: (B, A) => B): B

    def foldRight[B](acc: B)(f: (A, B) => B): B

    def flatMap[B](f: A => List[B]): List[B]

    def reverse(): List[A]

    def zipRight: List[(A, Int)]

    def partition(pred: A => Boolean): (List[A], List[A])

    def span(pred: A => Boolean): (List[A], List[A])

    def reduce(op: (A, A) => A): A

    def takeRight(n: Int): List[A]

    def collect[B](op: PartialFunction[A, B]): List[B]

    // right-associative construction: 10 :: 20 :: 30 :: Nil()
    def ::(head: A): List[A] = Cons(head, this)
}

// defining concrete implementations based on the same template

case class Cons[A](_head: A, _tail: List[A])
    extends ListImplementation[A]

case class Nil[A]()
    extends ListImplementation[A]

// enabling pattern matching on ::

object :: {
    def unapply[A](l: List[A]): Option[(A, List[A])] = l match {
        case Cons(h, t) => Some((h, t))
        case _ => None
    }
}

// List algorithms
trait ListImplementation[A] extends List[A] {

    override def head: Option[A] = this match {
        case h :: _ => Some(h)
        case _ => None
    }

    override def tail: Option[List[A]] = this match {
        case _ :: t => Some(t)
        case _ => None
    }

    override def append(list: List[A]): List[A] = this match {
        case h :: t => h :: (t append list)
        case _ => list
    }

    override def foreach(consumer: A => Unit): Unit = this match {
        case h :: t =>
            consumer(h)
            t foreach consumer
        case _ => None
    }

    override def get(pos: Int): Option[A] = this match {
        case h :: _ if pos == 0 => Some(h)
        case _ :: t if pos > 0 => t get (pos - 1)
        case _ => None
    }

    override def filter(predicate: A => Boolean): List[A] = this match {
        case h :: t if predicate(h) => h :: (t filter predicate)
        case _ :: t => t filter predicate
        case _ => Nil()
    }

    override def map[B](fun: A => B): List[B] = this match {
        case h :: t => fun(h) :: (t map fun)
        case _ => Nil()
    }

    override def toSeq: Seq[A] = this match {
        case h :: t => h +: t.toSeq // using method '+:' in Seq..
        case _ => Seq()
    }

    override def foldLeft[B](acc: B)(f: (B, A) => B): B = this match {
        case Cons(h, t) => t.foldLeft(f(acc, h))(f)
        case Nil() => acc
    }

    override def foldRight[B](acc: B)(f: (A, B) => B): B =
        this.reverse().foldLeft(acc)((acc, elem) => f(elem, acc))

    override def reverse(): List[A] =
        this.foldLeft(Nil[A]().asInstanceOf[List[A]])((acc, elem) => Cons(elem, acc))

    override def flatMap[B](f: A => List[B]): List[B] = this match {
        case Cons(h, t) => f(h).append(t.flatMap(f))
        case Nil() => Nil()
    }

    override def zipRight: List[(A, Int)] = {
        var k = 0
        map(e => {
            val mapped = (e, k)
            k += 1
            mapped
        })
    }

    override def partition(pred: A => Boolean): (List[A], List[A]) = (filter(pred), filter(!pred(_)))

    override def span(pred: A => Boolean): (List[A], List[A]) = {
        def _prefixLength(list: List[A]): Integer = list match {
            case h :: t if pred(h) => 1 + _prefixLength(t)
            case _ => 0
        }
        val cut = _prefixLength(this)
        val zipped = this.zipRight
        this match {
            case Cons(_, _) => (zipped.filter(e => e._2 < cut).map(e => e._1), zipped.filter(e => e._2 >= cut).map(e => e._1))
            case Nil() => (List.nil, List.nil)
        }
    }

    /**
      *
      * @throws UnsupportedOperationException if the list is empty
      */
    override def reduce(op: (A, A) => A): A = this match {
        case Cons(h, t) => {
            var result = h
            for (elem <- t) {
                result = op(result, elem)
            }
            result
        }
        case Nil() => throw new UnsupportedOperationException()
    }

    override def takeRight(n: Int): List[A] = {
        @tailrec
        def _takeRight(list: List[A], elems: Int): List[A] = list match {
            case Cons(_, _) if elems == 0 => list
            case Cons(_, t) if elems > 0 => _takeRight(t, elems - 1)
            case _ => Nil()
        }
        def _length(list: List[A]): Int = foldLeft(0)((acc, _) => acc + 1)
        _takeRight(this, _length(this) - n)
    }

    override def collect[B](op: PartialFunction[A, B]): List[B] = this filter op.isDefinedAt map op
}

// Factories
object List {

    // Smart constructors
    def nil[A]: List[A] = Nil()

    def cons[A](h: A, t: List[A]): List[A] = Cons(h, t)

    def apply[A](elems: A*): List[A] = {
        var list: List[A] = Nil()
        for (i <- elems.length - 1 to 0 by -1) list = elems(i) :: list
        list
    }

    def of[A](elem: A, n: Int): List[A] =
        if (n == 0) Nil() else elem :: of(elem, n - 1)
}
