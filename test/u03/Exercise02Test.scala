package u03

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import u02.Modules._
import u02.Modules.Person._
import u03.Lists.List._


class Exercise02Test {

  val lst:Lists.List[Person] = Cons(Student("mario", 2015), Cons(Teacher("luigi", "sistemi"),
    Cons(Teacher("wario", "architettura"), Nil())))

  @Test def testCourses(): Unit = {
    assertEquals(Cons("sistemi", Cons("architettura", Nil())), courses(lst))
    assertEquals(Nil(), courses(Nil()))
  }
}
