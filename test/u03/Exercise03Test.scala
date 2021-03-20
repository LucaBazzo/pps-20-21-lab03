package u03

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import u03.Lists.List._
import u03.Streams.Stream

class Exercise03Test {

  val s = Stream.take(Stream.iterate(0)( _ +1))(10)

  @Test def testDrop(): Unit ={
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil ())))),Stream.toList(Stream.drop(s)(6)))
    assertEquals(Cons(9, Nil ()),Stream.toList(Stream.drop(s)(9)))
    assertEquals(Nil(),Stream.toList(Stream.drop(s)(100)))
  }

  @Test def testCostant(): Unit ={
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))),
                  Stream.toList(Stream.take(Stream.constant("x"))(5)))
    assertEquals(Nil(), Stream.toList(Stream.take(Stream.constant("x"))(0)))

    assertEquals(Cons(18, Cons(18, Cons(18, Nil()))),
      Stream.toList(Stream.take(Stream.constant(18))(3)))
  }

  @Test def testFibonacci(): Unit ={
    val fibs: Stream[Int] = Stream.map(Stream.iterate((0,1))(x => (x._2, x._1 + x._2)))(x => x._1)
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))),
      Stream.toList(Stream.take(fibs)(8)))
    assertEquals(Cons(0, Cons(1, Nil())), Stream.toList(Stream.take(fibs)(2)))
  }
}
