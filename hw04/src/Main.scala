import Util._

object Main extends Homework04 {
	trait BFAEValue
	case class NumV(n: Int) extends BFAEValue
	case class CloV(param: String, body: BFAE, env: Env) extends BFAEValue
	case class BoxV(addr: Addr) extends BFAEValue
	case class RecV(rec: Map[String, Addr], env: Env) extends BFAEValue
	//case class BufV() extends BFAEValue

	type Env = Map[String, BFAEValue]
	type Addr = Int
	type Sto = Map[Addr, BFAEValue]

	def numVAdd(left: BFAEValue, right: BFAEValue): BFAEValue = (left, right) match{
		case (NumV(n), NumV(m)) => NumV(n + m)
		case _ => error(s"not a number")
	}

	def numVSub(left: BFAEValue, right: BFAEValue): BFAEValue = (left, right) match{
		case (NumV(n), NumV(m)) => NumV(n - m)
		case _ => error(s"not a number")
	}

	def lookup(x: String, env: Env): BFAEValue = env.get(x) match{
		case Some(v) => v
		case None => error(s"no such field: $x")
	}

	def malloc(sto: Sto): Addr = {
		sto.foldLeft(0) {
		case (max, (addr, _)) => math.max(max, addr)
		} + 1
	}

	def storeLookup(addr: Addr, sto: Sto): BFAEValue = sto.get(addr) match{
		case Some(v) => v
		case _ => error(s"no such address: $addr")
	}

	def interp(bfae: BFAE, env: Env, sto: Sto): (BFAEValue, Sto) = bfae match {
		case Num(n) => (NumV(n), sto)
		case Add(l, r) => 
			val (lv, ls) = interp(l, env, sto)
			val (rv, rs) = interp(r, env, ls)
			(numVAdd(lv, rv), rs)
		case Sub(l, r) => 
			val (lv, ls) = interp(l, env, sto)
			val (rv, rs) = interp(r, env, ls)
			(numVSub(lv, rv), rs)
		case Id(x) => (lookup(x, env), sto)
		case Fun(params, b) => (CloV(params, b, env), sto)
		case App(fun, arg) => 
			val (fv, fs) = interp(fun, env, sto)
			val (av, as) = interp(arg, env, fs)
			fv match {
				case CloV(x, b, fenv) => interp(b, fenv + (x -> av), as)
				case _ => error(s"not a closure: $fv")
			}
		case NewBox(expr) =>
			val (v, s) = interp(expr, env, sto)
			val addr = malloc(s)
			(BoxV(addr), s + (addr -> v))
		case SetBox(box, expr) => 
			val (bv, bs) = interp(box, env, sto)
			bv match {
				case BoxV(addr) =>
					val (v, s) = interp(expr, env, bs)
					(v, s + (addr -> v))
				case _ => error(s"not a box: $bv")
			}
		case OpenBox(box) => 
			val (bv, bs) = interp(box, env, sto)
			bv match {
				case BoxV(addr) => (storeLookup(addr, bs), bs)
				case _ => error(s"not a box: $bv")
			}
		case Seqn(l, rl) =>
			val (lv, ls) = interp(l, env, sto)
			rl match {
				case Nil => (lv, ls)
				case lf::rest => 
					interp(Seqn(lf, rest), env, ls)
			}
		case Rec(fields) => 
			val recf: Map[String, Addr] = Map()
			val st: Sto = sto
			fields.foreach {
				case (str, bf) =>
					val (rv, rs) = interp(bf, env, st)
					val addr = malloc(rs)
					val recf: Map[String, Addr] = recf + (str -> addr)
					val st: Sto = rs + (addr -> rv)
			}
			(RecV(recf, env), sto)
			//(RecV(fields, env), sto)
	// 	case Get(rec, field) => 
	// 		val (rv, rs) = interp(rec, env ,sto) 
	// 		rv match {
	// 			case RecV(fields, renv) => fields.get(field) match {
	// 				case Some(v) => interp(v, renv, rs)
	// 				case _ => error(s"no such field: $field")
	// 			}
	// 			case _ => error(s"not a record: $rv")
	// 		}
	// 	case Set(rec, field, expr) => 
	// 		val (rv, rs) = interp(rec, env, sto)
	// 		rv match {
	// 			case RecV(fields, renv) => fields.get(field) match {
	// 				case Some(v) => 
	// 					val (ov, os) = interp(v, renv, rs)
	// 					val (vs, s) = interp(expr, renv, os)
						
	// 					(vs, s)
	// 				case _ => error(s"no such field: $field")
	// 			}
	// 			case _ => error(s"not a record: $rv")
	// 		}
	 }

	// Evaluate a BFAE program contained in a string
  	def run(str: String): String = {
  		val (res, sto) = interp(BFAE(str), Map(), Map())
  		res match {
  			case NumV(n) => s"$n"
    		case CloV(ps, b, e) => s"function"
    		case BoxV(addr) => s"box"
    		case RecV(r, e) => s"record"
  		}
  	}

	// Write your own tests
  	def ownTests: Unit = {
  		test(run("""{{fun {b} {seqn {setbox b {+ 2 {openbox b}}}
                               {setbox b {+ 3 {openbox b}}}
                               {setbox b {+ 4 {openbox b}}}
                               {openbox b}}}
                {newbox 1}}"""), "10")
  		test(run("{+ 20 10}"), "30")

  		test(run("{seqn 1 2}"), "2")
    	test(run("{{fun {b} {openbox b}} {newbox 10}}"), "10")
    	test(run("{{fun {b} {seqn {setbox b 12} {openbox b}}} {newbox 10}}"), "12")
    	test(run("{{fun {b} {seqn {setbox b 12} {openbox b}}} {newbox 10}}"), "12")
    	test(run("{{fun {b} {openbox b}} {seqn {newbox 9} {newbox 10}}}"), "10")
    	test(run("{{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}"), "9")
    	test(run("{{fun {b} {seqn {setbox b 2} {openbox b}}} {newbox 1}}"), "2")
    	test(run("{{fun {b} {seqn {setbox b {+ 2 {openbox b}}} {setbox b {+ 3 {openbox b}}} {setbox b {+ 4 {openbox b}}} {openbox b}}} {newbox 1}}"), "10")
    	//test(run("{{fun {r} {get r x}} {rec {x 1}}}"), "1")
    	test(run("{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}"), "5")
    	//test(run("{{{{{fun {g} {fun {s} {fun {r1} {fun {r2} {+ {get r1 b} {seqn {{s r1} {g r2}} {+ {seqn {{s r2} {g r1}} {get r1 b}} {get r2 b}}}}}}}} {fun {r} {get r a}}} {fun {r} {fun {v} {set r b v}}}} {rec {a 0} {b 2}}} {rec {a 3} {b 4}}}"), "5")
    	test(run("{fun {x} x}"), "function")
    	test(run("{newbox 1}"), "box")
    	test(run("{rec}"), "record")

  	}

}