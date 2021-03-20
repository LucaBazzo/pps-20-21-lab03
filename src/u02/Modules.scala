package u02

import u02.Modules.Person.get_type
import u03.Lists.List._
import u03.Lists._

object Modules extends App {

  // An ADT: type + module
  sealed trait Person
  object Person {

    case class Student(name: String, year: Int) extends Person

    case class Teacher(name: String, course: String) extends Person

    def name(p: Person): String = p match {
      case Student(n, _) => n
      case Teacher(n, _) => n
    }

    def get_course(p: Person): String = p match {
      case Student(_, _) => ""
      case Teacher(_, n) => n
    }

    def get_type(p: Person):String = p match {
      case Student(_,_) => "Student"
      case Teacher(_,_) => "Teacher"
    }

    def courses(l: List[Person]): List[String] = l match {
      case Cons(_,_) => map(filter(l)(x => get_type(x) == "Teacher"))(y => get_course(y))
      case Nil() => Nil()
    }

  }

  println(Person.name(Person.Student("mario",2015)))
  println(get_type(Person.Student("mario",2015)) == "Student")

  import Person._
  println(name(Student("mario",2015)))

  val lst:List[Person] = Cons(Student("mario", 2015), Cons(Teacher("luigi", "sistemi"),
    Cons(Teacher("wario", "architettura"), Nil())))

  print(filter(lst)(x => get_type(x) == "Teacher"))

}
