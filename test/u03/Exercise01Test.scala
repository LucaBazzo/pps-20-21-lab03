package u03

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import u03.Lists.List._
import u02.Optionals._


class Exercise01Test {

  val lst = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testDrop(): Unit ={
    assertEquals(Cons(20, Cons(30, Nil())), drop(lst, 1)) // Cons (20 , Cons (30 , Nil ()))
    assertEquals(Cons(30, Nil()), drop(lst, 2)) // Cons (30 , Nil ())
    assertEquals(Nil(), drop(lst, 5)) // Nil ()
  }

  @Test def testFlatMap(): Unit = {
    assertEquals(Cons(11, Cons(21, Cons(31, Nil ()))), flatMap(lst)(v => Cons(v + 1, Nil()))) // Cons (11 , Cons (21 , Cons (31 , Nil ())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))),
                        flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil()))))
    // Cons (11 , Cons (12 , Cons (21 , Cons (22 , Cons (31 , Cons (32 , Nil ()))))))
  }

  @Test def testMapFlat(): Unit = {
    assertEquals(Cons(11,Cons(21,Cons(31,Nil()))), mapFlat(lst)(v => v + 1))
    assertEquals(Cons("10",Cons("20",Cons("30",Nil()))), mapFlat(lst)(v => v.toString()))
  }

  @Test def testMapFlat2(): Unit = {
    assertEquals(Cons(11,Cons(21,Cons(31,Nil()))), mapFlat2(lst)(v => v + 1))
    assertEquals(Cons("10",Cons("20",Cons("30",Nil()))), mapFlat2(lst)(v => v.toString()))
  }

  @Test def testFilterFlat(): Unit = {
    assertEquals(Cons(10,Cons(20,Nil())), filterFlat(lst)( _ < 21 ))
    assertEquals(Cons(10,Nil()), filterFlat(lst)( _ == 10 ))
  }



  @Test def testMax(): Unit ={
    assertEquals(Option.Some(25), max(Cons(10, Cons(25, Cons(20, Nil()))))) // Some (25)
    assertEquals(Option.None(), max(Nil())) // None ()
    assertEquals(Option.Some(50), max(Cons(50, Cons(25, Cons(20, Nil())))))
    assertEquals(Option.Some(-2), max(Cons(-2, Nil())))
  }

}
