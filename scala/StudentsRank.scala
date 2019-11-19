/*
You're going on a trip with some students and it's up to you to keep track of how much money each Student has. A student is defined like this:

case class Student(name: String, fives: Int, tens: Int, twenties: Int)
As you can tell, each Student has some fives, tens, and twenties. Your job is to return the name of the student with the most money.
 If every student has the same amount, then return "all".

Notes:

Each student will have a unique name
There will always be a clear winner: either one person has the most, or everyone has the same amount
If there is only one student, then that student has the most money
 */
object StudentsRank {

  case class Student(name: String, fives: Int, tens: Int, twenties: Int)

  def mostMoney(students: List[Student]): String = {
    val r = students.map(s => (s.name, s.fives * 5 + s.tens * 10 + s.twenties * 20)).sortBy(-_._2).take(2)
    if (r.length == 1 || r.head._2 > r.last._2) r.head._1
    else "all"
  }
}