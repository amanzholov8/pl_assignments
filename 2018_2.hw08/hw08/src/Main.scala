import Util._

object Main extends Homework08 {
	trait KXCFAEValue
	case class NumV(n: Int) extends KXCFAEValue
	case class CloV(params: List[String], body: KXCFAE, env: Env) extends KXCFAEValue
	case class ContV(proc: Cont) extends KXCFAEValue
	case object Exception extends KXCFAEValue

	type Env = Map[String, KXCFAEValue]
	type Cont = KXCFAEValue => KXCFAEValue

	def numVAdd(left: KXCFAEValue, right: KXCFAEValue): KXCFAEValue = (left, right) match {
		case (NumV(n), NumV(m)) => NumV(n + m)
		case (Exception, _) => Exception
		case (_, Exception) => Exception
		case _ => error(s"not both numbers")
	}

	def numVSub(left: KXCFAEValue, right: KXCFAEValue): KXCFAEValue = (left, right) match {
		case (NumV(n), NumV(m)) => NumV(n - m)
		case (Exception, _) => Exception
		case (_, Exception) => Exception
		case _ => error(s"not both numbers")
	}

	def lookup(x: String, env: Env): KXCFAEValue = env.get(x) match {
		case Some(v) => v 
		case None => error(s"free identifier: $x")
	}

	def mapping(params: List[String], interpreted: List[KXCFAEValue], fenv: Env): Env = params match {
		case Nil => fenv
		case l::rest =>
			mapping(rest, interpreted.tail, fenv +  (l -> interpreted.head))
    }

    def appfun(k: Cont, args: List[KXCFAE], fv: KXCFAEValue, env: Env, avs: List[KXCFAEValue]): KXCFAEValue = args match {
    	case Nil => fv match {
    		case CloV(ps, b, fenv) => 
    			interp(b, fenv, k)
    		case Exception => k(Exception)
			case v => error("not a closure: $v")
    	}
    	case l::Nil => interp(l, env, av => 
    		if (av == Exception) k(Exception)
    		else {
	    		fv match{
		    		case CloV(ps, b, fenv) => 
		    			if (ps.length != avs.length + 1) error(s"wrong arity")
						interp(b, mapping(ps, (av::avs).reverse, fenv), k)
					case ContV(kv) => kv(av)
					case Exception => k(Exception)
					case v => error("not a closure: $v")
	    			}
	    	}) 
    	case l::rest => interp(l, env, av => 
    				if (av == Exception) k(Exception)
    				else appfun(k, rest, fv, env, av::avs))
    }

	def interp(kxcfae: KXCFAE, env: Env, k: Cont): KXCFAEValue = kxcfae match {
		case Num(n) => k(NumV(n))
		case Add(l, r) => 
			interp(l, env, lv => 
				if (lv == Exception) k(Exception)
				else
					interp(r, env, rv => k(numVAdd(lv, rv))))
		case Sub(l, r) => 
			interp(l, env, lv => 
				if (lv == Exception) k(Exception)
				else
					interp(r, env, rv => k(numVSub(lv, rv))))
		case Id(x) => k(lookup(x, env))
		case Fun(ps, b) => k(CloV(ps, b, env))
		case App(f, as) =>
			interp(f, env, fv => appfun(k, as, fv, env, Nil))
		case If0(c, e1, e2) => interp(c, env, cv => cv match {
			case NumV(0) => interp(e1, env, k)
			case Exception => k(Exception)
			case _ => interp(e2, env, k)
		})
		case Withcc(x, b) => 
				interp(b, env + (x -> ContV(k)), k)
		case Try(trye, catche) => interp(trye, env, tv => tv match {
			case Exception => interp(catche, env, k)
			case v => k(v)
		})
		case Throw => k(Exception)
	}

	// Evaluate a KXCFAE program contained in a string
  	def run(str: String): String = { 
  		val res = interp(KXCFAE(str), Map(), x => x)
  		res match {
  			case NumV(n) => s"$n"
    		case CloV(ps, b, e) => s"function"
    		case ContV(_) => s"continuation"
    		case Exception => error(s"no enclosing try-catch.")
  		}
	}

  	// Write your own tests
  	def ownTests: Unit = {
  	    // multiple arguments [5]
	   test(run("{{fun {x y} {- y x}} 10 12}"), "2")
	   test(run("{fun {} 12}"), "function")
	   test(run("{fun {x} {fun {} x}}"), "function")
	   test(run("{{{fun {x} {fun {} x}} 13}}"), "13")
	   test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")

	   testExc(run("{throw}"), "no enclosing try-catch.")

	   // Exceptions [35]
	   test(run("{+ {withcc k {k 5}} 4}"), "9")
	   test(run("{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} 1 {+ y {g g {- y 1}}}}} 10}"), "55") // recursive function
	   test(run("{withcc done {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {done 100} {+ y {g g {- y 1}}}}} 10}}"), "100") // exit from recursive function using continuation
	   test(run("{withcc k {- 0 {k 100}}}"), "100")
	   test(run("{withcc k {k {- 0 100}}}"), "-100")
	   test(run("{withcc k {k {+ 100 11}}}"), "111")
	   test(run("{{fun {a b c} {- {+ {withcc k {+ {k 100} a}} b} c}} 100 200 300}"), "0")
	   test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
	   test(run("{{withcc esc {{fun {x y} {fun {z} {+ z y}}} 1 {withcc k {esc k}}}} 10}"), "20")
	   test(run("{try {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {+ y {g g {- y 1}}}}} 10} catch 110}"), "110") // exit from recursive function using try-catch
	   
	   test(run("{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch y}}} 10}"), "54") // equal? for multiple recursive try-catch
	   test(run("{withcc done {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch {done y}}}} 10}}"), "2")
	   test(run("{try {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch {throw}}}} 10} catch 20110464}"), "20110464") // recursive try-catch throwing ("1")
	   
	   test(run("{try {{fun {x y z} {a b c}} 1 2 {throw}} catch 0}"), "0")
	   test(run("{{fun {f} {try {f 3} catch 8}} {fun {x} {throw}}}"), "8")
	   test(run("{try {- 0 {withcc k {+ 3 {k {throw}}}}} catch 89}"), "89")
	   test(run("{try {+ 3 {withcc k {+ 1000 {k {throw}}}}} catch 11}"), "11")
	   test(run("{{fun {x y z} {try {+ 1 {+ x {throw}}} catch {+ y z}}} 1 2 3}"), "5")
	   test(run("{+ {try {- 10 {throw}} catch 3} 10}"), "13")
	   test(run("{try {if0 0 {throw} {+ 1 2}} catch {if0 10 1 {try {throw} catch 54}}}"), "54")
	   test(run("{try {withcc a {+ 1 {withcc b {throw}}}} catch 10}"), "10")
	   test(run("{try {- 0 {throw}} catch 5}"), "5")
	   test(run("{try {if0 {throw} 3 4} catch 5}"), "5")

	   test(run("{try {{fun {x y} {try x catch y}} {throw} 0} catch -1}"), "-1")
	   test(run("{try {try {throw} catch {throw}} catch 9}"), "9")
	   test(run("{{withcc esc {try {{withcc k {esc k}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}"), "8")
	   test(run("{{withcc esc {try {{withcc k {try {esc k} catch {fun {x} {fun {y} 9}}}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}"), "8")
	   test(run("{withcc foo {{fun {x y} {y x}} {+ 2 3} {withcc bar {+ 1 {bar foo}}}}}"), "5")
	   test(run("{try {withcc zzz {{fun {x y z w} {+ {+ x y} {+ z w}}} 1 2 {zzz 10} {throw}}} catch 42}"), "10")

	   test(run("{try {withcc zzz {{fun {x y z w} {+ {+ x y} {+ z w}}} 1 2 {throw} {zzz 10}}} catch 42}"), "42")
	   test(run("{try {withcc zzz {{fun {x y z w} {+ {w {+ x y}} {+ {throw} z}}} 1 2 3 zzz}} catch 42}"), "3")
	   
	   test(run("{withcc esc {try {+ {throw} {esc 3}} catch 4}}"), "4")
	   test(run("{withcc esc {{fun {x y} {+ {+ x 3} y}} {withcc k {try {k {esc {throw}}} catch {k 5}}} 7}}"), "15")
	   test(run("{try {withcc x {+ {x 1} {throw}}} catch 0}"), "1")
	   test(run("{+ 12 {withcc k {+ 1 {k {{fun {} 7}}}}}}"), "19")

	   // multiple arguments [6]
	   test(run("{+ 999 {withcc done {{fun {f x} {f f x done}} {fun {g y z} {if0 {- y 1} {z 100} {+ y {g g {- y 1} z}}}} 10}}}"), "1099")
	   test(run("{+ 999 {withcc done {{fun {f x} {f f x {fun {x} {if0 x {done {- 0 999}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 100} {+ y {g g {- y 1} z}}}} 10}}}"), "11053")
	   test(run("{+ 999 {withcc done {{fun {f x} {f f x {fun {x} {if0 x {done {- 0 999}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {+ y {g g {- y 1} z}}}} 10}}}"), "0")
	   test(run("{withcc done {{fun {f x} {f f x {fun {x} {if0 x {fun {y} {fun {x} {+ x y}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 3}}"), "64")
	   test(run("{{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 3}} 5}"), "continuation")
	   test(run("{{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {fun {x} 42}}}}}"), "42")

	   // Exceptions [4]
	   test(run("{try {{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} {throw}}}}} {fun {g y z} {if0 {- y 1} {z 1} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {fun {x} 42}}}}} catch 4242}"), "4242")
	   test(run("{withcc esc {{try {withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} {throw}}}}} {fun {g y z} {if0 {- y 1} {z 1} {{g g {- y 1} z} 32}}} 4}} catch esc} 33}}"), "33")
	   test(run("{try {try {throw} catch {try {throw} catch {try {throw} catch {+ {withcc k {try {throw} catch {k 0}}} {throw}}}}} catch 0}"), "0")
	   test(run("{try {{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {throw}}}}} catch 4242}"), "4242")

  	}
}