import Util._

object Main extends Homework02 {
  
   type Env = Map[String, List[Int]]
   
   // applies a binary numeric function on all combinations of integers from
   // the two input lists, and return the list of all of the results
   def binOp(
     op: (Int, Int) => Int,
     ls: List[Int],
     rs: List[Int]
   ): List[Int] = ls match {
     case Nil => Nil
     case l :: rest =>
       def f(r: Int): Int = op(l, r) // TODO complete this function
       rs.map(f) ++ binOp(op, rest, rs)
   }

   def lookup(id: String, env: Env): List[Int] = env.get(id) match{
      case Some(v) => v
      case None => error(s"free identifier: $id")
   }

   def interp(e: MUWAE, env: Env): List[Int]= e match{
      case Num(nums) => nums
      case Add(left, right) => binOp(_+_, interp(left, env), interp(right, env))
      case Sub(left, right) => binOp(_-_, interp(left, env), interp(right, env))
      case With(x, expr, body) => interp(body, env + (x -> interp(expr, env)))
      case Id(x) => lookup(x, env)
      case Min(l, m ,r) => {
        val a = interp(l, env)
        val b = interp(m, env)
        val c = interp(r, env)
        if (a.length != 1 || b.length != 1 || c.length != 1) error(s"incorrect input")
        if (a.head <= b.head && a.head<= c.head) a
        else if (b.head <= a.head && b.head <= c.head) b
        else c
      }
      case Max(l, m, r) => {
        val a = interp(l, env)
        val b = interp(m, env)
        val c = interp(r, env)
        if (a.length != 1 || b.length != 1 || c.length != 1) error(s"incorrect input")
        if (a.head >= b.head && a.head >= c.head) a
        else if (b.head >= a.head && b.head >= c.head) b
        else c
      }
   }

  // Evaluate a MUWAE program contained in a string
  def run(str: String): List[Int] = {
      interp(MUWAE(str), Map())
  }

  // Write your own tests
  def ownTests: Unit = {
     test(run("{+ 3 7}"), List(10))
     test(run("{- 10 {3 5}}"), List(7, 5))
     test(run("{with {x {+ 5 5}} {+ x x}}"), List(20))
     test(run("{+ {1 2} {3 4}}"), List(4, 5, 5, 6))
     test(run("{- {+ {1 2} {3 4}} {1 2}}"), List(3, 2, 4, 3, 4, 3, 5, 4))
     test(run("{- {10 2 1} {3 2}}"), List(7, 8, -1, 0, -2, -1))
     test(run("{with {x {1 2}} {+ x {4 3}}}"), List(5, 4, 6, 5))
     test(run("{with {x 9} {+ x {with {x 3} x}}}"), List(12))
     test(run("{with {x 100} {+ x {with {y 3} x}}}"), List(200))
     test(run("{with {x 5} {+ x {with {x 3} 10}}}"), List(15))
     test(run("{with {x {7 5}} {+ x x}}"), List(14, 12, 12, 10))
     test(run("{with {x {1 2}} {+ x {4 3}}}"), List(5, 4, 6, 5))
     test(run("{with {x 2} {- {+ x x} x}}"), List(2))
     test(run("{+ {muwae-min 3 5 7} {muwae-min 10 100 1000}}"), List(13))
     test(run("{+ {muwae-min 9 3 7} {muwae-max 6 2 20}}"), List(23))
     test(run("{with {x 10} {muwae-max x 2 3}}"), List(10))
     test(run("{with {x 20} {with {y 5} {with {z {10 20}} {+ z {muwae-max {+ x y} 0 12}}}}}"), List(35, 45))
     test(run("{with {x 20} {with {y 5} {with {z {10 20}} {+ z {muwae-min {+ x y} 0 12}}}}}"), List(10, 20))
     test(run("{with {x {muwae-min 3 9 5}} {with {y {- x 3}} y}}"), List(0))
     test(run("{with {x {muwae-max 2 3 5}} {muwae-min x 7 6}}"), List(5))
     test(run("{with {x {muwae-max 9 7 10}} {muwae-max 8 x {+ 1 x}}}"), List(11))
     test(run("{- {muwae-min 6 4 5} {muwae-max 2 3 4}}"), List(0))
     test(run("{with {x {+ 7 2}} {muwae-min x 7 0}}"), List(0))
     test(run("{+ {muwae-min 9 3 7} {muwae-max 6 2 20}}"), List(23))
     test(run("{with {x {13}} {muwae-min x 1 12}}"), List(1))
     test(run("{with {x {muwae-min 2 1 3}} {+ x x}}"), List(2))
     test(run("{with {a 10} {with {b 19} {with {c 2} {muwae-min a b c}}}}"), List(2))
     test(run("{with {x 3} {muwae-max 3 4 {+ x x}}}"), List(6))
     test(run("{with {a 10} {with {b 19} {with {c 2} {muwae-max a b c}}}}"), List(19))
     test(run("{with {x {muwae-min 2 5 4}} {+ x x}}"), List(4))
     test(run("{with {x {muwae-max 2 5 4}} {+ x x}}"), List(10))
     test(run("{with {x {- 11 3}} {muwae-max x {+ x x} {- x x}}}"), List(16))
     test(run("{with {x {- 11 3}} {muwae-min x {+ x x} {- x x}}}"), List(0))
     test(run("{muwae-min {+ 4 4} {with {x 5} {+ x {with {x 3} 10}}} 3}"), List(3))
     test(run("{muwae-max {+ 4 4} {with {x 5} {+ x {with {x 3} 10}}} 3}"), List(15))
     test(run("{with {x {13}} {muwae-min x 1 12}}"), List(1))
     test(run("{with {x {10} } {muwae-max x 2 3}}"), List(10))
     test(run("{with {x {muwae-min 2 1 3}} {+ x x}}"), List(2))
     test(run("{with {x {muwae-max 2 1 3}} {+ x x}}"), List(6))
     test(run("{with {x 2} {muwae-min x 3 10}}"), List(2))
     test(run("{with {x 2} {muwae-max x 3 10}}"), List(10))
     test(run("{muwae-min {+ 4 4} 2 3} "), List(2))
     test(run("{muwae-max {+ 4 4} 2 3} "), List(8))
     test(run("{with {x 10} {muwae-min x 2 3}}"), List(2))
     test(run("{with {x 10} {muwae-max x 2 3}}"), List(10))
     test(run("{with {x {10}} {muwae-max x 2 3}}"), List(10))
     test(run("{muwae-min {+ 3 4} 5 6}"), List(5))
     test(run("{muwae-max {+ 3 4} 5 6}"), List(7))
     test(run("{with {x {10}} {muwae-min x {3} {5}}}"), List(3))
     test(run("{with {x {10}} {muwae-max x {3} {5}}}"), List(10))
     test(run("{muwae-min {3} 4 5}"), List(3))
     test(run("{muwae-max {3} 4 {5}}"), List(5))
     test(run("{+ {10 100 1000 10000} {muwae-min {- 3 4} 5 6}}"), List(9, 99, 999, 9999))

     testExc(run("{muwae-min 3 {2 4} 5}"), "incorrect")
     testExc(run("{muwae-max 3 {2 4} 5}"), "incorrect")
     testExc(run("{+ x 10}"), "free identifier")
  }
}