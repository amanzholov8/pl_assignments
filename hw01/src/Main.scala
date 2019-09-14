import Util._

object Main extends Homework01 {
  /* TODO Implement 10 missing functions */
  // 1. Define the function "dollar2won", which consumes an integer number of dollars
  // and produces the won equivalent. Use the won/dollar conversion rate
  // of 1100 won per dollar.
  def dollar2won(dollar: Int): Int = {
  	if (dollar<0) error("dollar cannot be negative")
  	1100 * dollar
  }
  // 2. Write the function "volumeOfCuboid", which consumes three integer numbers
  // denoting lengths of three sides and produces the volume of the cuboid.
  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = {
  	if (a<=0 || b<=0 || c<=0) error("Input cannot be negative")
  	a * b * c
  }

  // 3. Write the function "isEven", which consumes an integer number and returns
  // whether the number is even.
  def isEven(num: Int): Boolean = if (num%2 == 0) true else false   

  // 4. Write the function "isOdd", which consumes an integer number and returns
  // whether the number is odd.
  def isOdd(num: Int): Boolean = if (num%2==1) true else false

  // 5. Write the function "gcd", which consumes two integer numbers and returns
  // the greatest common divisor of them.
  def gcd(a: Int, b: Int): Int = {
  	if (a==0 && b==0) error("Undefined")
  	if (b==0) a else gcd(b, a%b)
  }

  // 6. Write the function "lcm", which consumes two integer numbers and returns
  // the least common multiple of them.
  def lcm(a: Int, b: Int): Int = {
  	if (gcd(a, b) == 0) 0 else a*b/gcd(a, b)
  }

  // You have a type "COURSE", which is either "CS320", "CS311", or "CS330".
  // "CS320" has two members: "quiz" for a number of quizzes and "homework"
  // for a number of programming assignments. CS311 has one member: "homework"
  // which is a number too. CS330 has two members: "projects" for a number of
  // projects and "homework" for a number of programming assignments.
  

  // 7. Define the function numOfHomework, which consumes a course and produces
  // the number of programming assignments for the given course.
  def numOfHomework(course: COURSE): Int = course match  {
    case CS320(quiz, homework) => if (homework<0 || quiz<0) error("assignments cannot be negative") else homework
    case CS311(homework) => if (homework<0) error("assignments cannot be negative") else homework
    case CS330(projects, homework) => if (homework<0 || projects<0) error("assignments cannot be negative") else homework
  }

  // 8. Define the function hasProjects, which consumes a course and produces
  // true only when the given course is CS330 with more than or equal to
  // two projects, otherwise produces false.
  def hasProjects(course: COURSE): Boolean = course match {
    case CS320(quiz, homework) => if (homework<0 || quiz<0) error("assignments cannot be negative") else false
    case CS311(homework) => if (homework<0) error("assignments cannot be negative") else false
    case CS330(projects, homework) => if (homework<0 || projects<0) error("assignments cannot be negative") else if (projects>=2) true else false
  }

  // 9. Define the function namePets, which consumes a list of pets and produces
  // a corresponding list of pets with names; it names all occurrences of "dog"
  // with "happy", "cat" with "smart", "pig" with "pinky", and keeps the other pets
  // as unnamed. For example,
  //
  //   namePets(List("dog", "tiger", "cat")) == List("happy", "tiger", "smart")
  //
  def namePets(pets: List[String]): List[String] = {
  	val res = pets.map((name: String) => {
  		if (name == "dog") "happy"
  		else if (name == "cat") "smart"
  		else if (name == "pig") "pinky"
  		else name
  	})
  	res
  }

  // 10. Generalize namePets to the function giveName. The new function consumes
  // two strings, called old and new. It produces a function that gets a list of
  // strings and replaces all occurrences of old by new in the list. For example,
  //
  //   val nameBears: List[String] => List[String] = giveName("bear", "pooh")
  //   nameBears(List("pig", "cat", "bear")) = List("pig", "cat", "pooh")
  //
  def giveName(oldName: String, newName: String): List[String] => List[String] = {
  	val output = (names: List[String]) => {
  		val res = names.map((name: String) => {
  			if (name == oldName) newName
  			else  name
  		})
  		res
  	}
  	output
  }

  def ownTests(): Unit = {
    /* TODO Write your own tests */
    test(dollar2won(10), 11000)
    test(dollar2won(1000), 1100000)
    testExc(dollar2won(-10), "negative")

    test(volumeOfCuboid(1, 2, 3), 6)
    test(volumeOfCuboid(4, 7, 6), 168)
    testExc(volumeOfCuboid(-1, 2, 3), "negative")
    testExc(volumeOfCuboid(1, -2, 3), "negative")
    testExc(volumeOfCuboid(1, 2, -3), "negative")

    test(isEven(8), true)
    test(isEven(7), false)

    test(isOdd(7), true)
    test(isOdd(8), false)

    test(gcd(45, 54), 9)
    test(gcd(0, 13), 13)
    test(gcd(13, 0), 13)
    testExc(gcd(0, 0), "Undefined")

    test(lcm(6, 8), 24)
    test(lcm(0,8), 0)
    test(lcm(8,0), 0)

    test(numOfHomework(CS320(3, 2)), 2)
    test(numOfHomework(CS311(3)), 3)
    test(numOfHomework(CS330(3, 0)), 0)
    testExc(numOfHomework(CS320(3, -2)), "negative")
    testExc(numOfHomework(CS311(-1)), "negative")
    testExc(numOfHomework(CS330(-3, 0)), "negative")
    testExc(numOfHomework(CS320(-3, 2)), "negative")
    testExc(numOfHomework(CS330(0, -2)), "negative")

    test(hasProjects(CS330(3, 0)), true)
    test(hasProjects(CS320(1, 1)), false)
    test(hasProjects(CS311(4)), false)
    test(hasProjects(CS330(1, 2)), false)
    testExc(hasProjects(CS330(-3, 0)), "negative")
    testExc(hasProjects(CS330(0, -3)), "negative")
    testExc(hasProjects(CS311(-2)), "negative")
    testExc(hasProjects(CS320(-1, 2)), "negative")
    testExc(hasProjects(CS320(3, -1)), "negative")

    test(namePets(List("dog", "tiger", "cat")), List("happy", "tiger", "smart"))
    test(namePets(List("pig", "dog", "cat")), List("pinky", "happy", "smart"))
   	test(namePets(List("bear", "tiger", "wolf")), List("bear", "tiger", "wolf"))

    test(giveName("bear", "pooh")(List("pig", "cat", "bear")), List("pig", "cat", "pooh"))
    test(giveName("pig", "nyam-nyam")(List("pig", "cat", "bear")), List("nyam-nyam", "cat", "bear"))
  }
}
