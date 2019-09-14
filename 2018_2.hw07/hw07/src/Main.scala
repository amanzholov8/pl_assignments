import Util._

object Main extends Homework07 {
	trait CORELValue
	case class NumV(n: Int) extends CORELValue
	case class CloV(param: String, body: COREL, var env: Env) extends CORELValue
	case class BoolV(bool: Boolean) extends CORELValue
	case class VariantV(name: String, value: CORELValue) extends CORELValue
	case class ConstructorV(name: String) extends CORELValue

	case class TypeEnv(
		vars : Map[String, Type] = Map(),
		tbinds: Map[String, Map[String, Type]] = Map(),
		alphas: Set[String] = Set()
		) {
			def addVar(x: String, t: Type): TypeEnv = 
				copy(vars = vars + (x -> t))
			def addTBind(x: String, cs: Map[String, Type]): TypeEnv = 
				copy(tbinds = tbinds + (x -> cs))
			def addAlpha(x:String): TypeEnv = 
				copy(alphas = alphas + x)
		}

	type Env = Map[String, CORELValue]

	def change(b1: Type, for1: String, for2: String): Type = b1 match {
		case NumT => NumT
		case BoolT => BoolT
		case IdT(x) => if (x==for1) IdT(for2) else if (x==for2) IdT(for1) else IdT(x)
		case ArrowT(l, r) => ArrowT(change(l, for1, for2), change(r, for1, for2))
		case PolyT(n, b) => 
			if (n==for1) PolyT(for2, change(b, for1, for2))
			else if (n==for2) PolyT(for1, change(b, for1, for2))
			else PolyT(n, change(b, for1, for2))
	}

	def same(left: Type, right: Type): Boolean = (left, right) match {
			case (NumT, NumT) => true
			case (BoolT, BoolT) => true
			case (IdT(s1), IdT(s2)) => s1==s2
			case (ArrowT(p1, r1), ArrowT(p2, r2)) =>
				same(p1, p2) && same(r1, r2)
			case (PolyT(for1, b1), PolyT(for2, b2)) =>
				same(change(b1, for1, for2), b2)
			case _ => false
		}

	def notype(msg: Any): Nothing = error(s"no type: $msg")

	def mustSame(left: Type, right: Type): Type = 
		if (same(left, right)) left
		else notype(s"$left is not equal to $right")
	
	def validType(ty: Type, tyEnv: TypeEnv): Type = ty match {
		case NumT => ty
		case BoolT => ty
		case ArrowT(p, r) =>
			ArrowT(validType(p, tyEnv), validType(r, tyEnv))
		case IdT(x) =>
			if (tyEnv.tbinds.contains(x)) ty
			else if (tyEnv.alphas.contains(x)) ty
			else notype(s"$x is a free type")
		case PolyT(forall, b) => validType(b, tyEnv.addAlpha(forall))
    }

    def tyEnvExt(name: String, 
    			tyEnvT: TypeEnv, 
    			consKeys: List[String], 
    			consVals: List[Type]): TypeEnv = consKeys match{
    	
    	case Nil => tyEnvT
    	case l::rest => 
    		tyEnvExt(name, tyEnvT.addVar(l, 
    			ArrowT(consVals.head, IdT(name))), rest, consVals.tail) 

    }

    def typeCheckCases(cs: Map[String, Type], 
    					tyEnv: TypeEnv, 
    					cKeys: List[String], 
    					cVals: List[(String, COREL)]): Type = cKeys match {
    	case Nil => error(s"empty")
    	case l::Nil => typeCheckFun(cVals.head._2, 
    						tyEnv.addVar(cVals.head._1, cs.getOrElse(l,
							notype(s"$l is free"))))
    	case l::rest =>  
    		val rft = typeCheckFun(cVals.head._2, 
    					tyEnv.addVar(cVals.head._1, cs.getOrElse(l,
						notype(s"$l is free"))))
    		mustSame(rft, typeCheckCases(cs, tyEnv, rest, cVals.tail))
    }

	def typeCheckFun(corel: COREL, tyEnv: TypeEnv): Type = corel match {
		case Num(_) => NumT
		case Bool(_) => BoolT
		case Add(l, r) => 
			mustSame(typeCheckFun(l, tyEnv), NumT)
			mustSame(typeCheckFun(r, tyEnv), NumT)
			NumT
		case Sub(l, r) =>
			mustSame(typeCheckFun(l, tyEnv), NumT)
			mustSame(typeCheckFun(r, tyEnv), NumT)
			NumT
		case Equ(l, r) =>
			mustSame(typeCheckFun(l, tyEnv), NumT)
			mustSame(typeCheckFun(r, tyEnv), NumT)
			BoolT
		case Id(x) => tyEnv.vars.getOrElse(x, notype(s"$x is a free identifier"))
		case Fun(x, t, b) => 
			validType(t, tyEnv)
			ArrowT(t, typeCheckFun(b, tyEnv.addVar(x, t)))
		case App(f, a) => 
			val funT = typeCheckFun(f, tyEnv)
			val argT = typeCheckFun(a, tyEnv)
			funT match {
				case ArrowT(param, result) if same(argT, param) => result
				case  _ => notype(s"apply $argT to $funT")
			}
		case IfThenElse(if0, e1, e2) => 
			mustSame(typeCheckFun(if0, tyEnv), BoolT)
			mustSame(typeCheckFun(e1, tyEnv), typeCheckFun(e2, tyEnv))
		case Rec(f, ft, x, xt, b) => ft match {
			case ArrowT(pt, rt) => 
				mustSame(pt, xt)
				mustSame(rt, typeCheckFun(b, (tyEnv.addVar(f, ft)).addVar(x, xt)))
				ft
			case _ =>  notype(s"$ft is not an arrow type")
		}
		case WithType(name, cons, b) =>
			val tyEnvT = tyEnv.addTBind(name, cons)
			val consKeys = cons.keys.toList
			val consVals = cons.values.toList
			val tyEnvV = tyEnvExt(name, tyEnvT, consKeys, consVals)
			cons.foreach{case (str: String, ty: Type) => validType(ty, tyEnvT)}
			typeCheckFun(b, tyEnvV)
		case Cases(name, disE, cases) =>
			val cs = tyEnv.tbinds.getOrElse(name, notype(s"$name is a free type"))
			mustSame(typeCheckFun(disE, tyEnv), IdT(name))
			val cKeys = cases.keys.toList
			val cVals = cases.values.toList
			typeCheckCases(cs, tyEnv, cKeys, cVals)
		case TFun(name, e) => 
			PolyT(name, typeCheckFun(e, tyEnv.addAlpha(name)))
		case TApp(b, ty) => typeCheckFun(b, tyEnv) match {
			case PolyT(name, body) => 
				//tyEnv.addVar(name, ty)
				changeAlpha(body, name, ty)
			case _ => error(s"not polyT: $b")
		}
	}

	def changeAlpha(body: Type, name: String, ty: Type): Type = body match {
		case ArrowT(l, r) => l match {
			case PolyT(n1, b1) => ArrowT(l, r)
			case _ => ArrowT(changeAlpha(l, name, ty), changeAlpha(r, name, ty))
		}
		case NumT => NumT
		case BoolT => BoolT
		case IdT(x) => if (x==name) ty else IdT(x)
		case PolyT(n1, b1) => PolyT(n1, changeAlpha(b1, name, ty))
	}

	def numVAdd(left: CORELValue, right: CORELValue): CORELValue = (left, right) match{
		case (NumV(n), NumV(m)) => NumV(n + m)
		case _ => error(s"not both numbers")
	}

	def numVSub(left: CORELValue, right: CORELValue): CORELValue = (left, right) match{
		case (NumV(n), NumV(m)) => NumV(n - m)
		case _ => error(s"not both numbers")
	}

	def isEqual(left: CORELValue, right: CORELValue): CORELValue = (left, right) match{
		case (NumV(n), NumV(m)) => BoolV(n==m)
		case _ => error(s"not both numbers")
	}

	def lookup(x: String, env: Env): CORELValue = env.get(x) match{
		case Some(v) => v
		case None => error(s"free identifier: $x")
	}

	def envExtension(arr: List[String], env: Env): Env = arr match {
		case Nil => env
		case str::rest =>
			envExtension(rest, env + (str -> ConstructorV(str)))
	}

	def varInterp(name: String,
				av: CORELValue,
				cKeys: List[String], 
				cVals: List[(String, COREL)], 
				env: Env): CORELValue = cKeys match {
		case Nil => error(s"$name is a free constructor")
		case l::rest => 
			if (l==name) interp(cVals.head._2, env + (cVals.head._1 -> av))
			else varInterp(name, av, rest, cVals.tail, env)
	}

	def interp(corel: COREL, env: Env): CORELValue = corel match {
		case Num(n) => NumV(n)
		case Bool(b) => BoolV(b)
		case Add(l, r) => numVAdd(interp(l, env), interp(r, env))
		case Sub(l, r) => numVSub(interp(l, env), interp(r, env))
		case Equ(l, r) => isEqual(interp(l, env), interp(r, env))
		case Id(x) => lookup(x, env)
		case Fun(param, _, body) => CloV(param, body, env)
		case App(funcE, argE) => interp(funcE, env) match {
			case CloV(param, body, fenv) => 
				interp(body, fenv+(param -> interp(argE, env)))
			case ConstructorV(name) => VariantV(name, interp(argE, env))
			case v => error(s"not a closure: $v")
		}
		case IfThenElse(if0, e1, e2) => interp(if0, env) match {
			case BoolV(true) => interp(e1, env)
			case _ => interp(e2, env)
		} 
		case Rec(name, _, param, _, body) => 
		 	val cloV = CloV(param, body, env)
		 	cloV.env = env + (name -> cloV)
		 	cloV
		case WithType(name, cons, body) =>
			val consArr = cons.keys.toList
		 	interp(body, envExtension(consArr, env))

		case Cases(name, disp, cases) => interp(disp, env) match {
			case VariantV(name, av) => 
				val cKeys = cases.keys.toList
				val cVals = cases.values.toList
				varInterp(name, av, cKeys, cVals, env)
			case v => error(s"not a variant: $v")

		}
		case TFun(_, e) => interp(e, env)
		case TApp(b, _) => interp(b, env)
	}


	def typeCheck(str: String): Type = {
		typeCheckFun(COREL(str), TypeEnv())
	}

	// Evaluate an expression contained in a string
    def run(str: String): String = {
    	val res = interp(COREL(str), Map())
    	res match {
    		case NumV(n) => s"$n"
    		case CloV(ps, b, e) => s"function"
    		case BoolV(b) => s"$b"
    		case VariantV(_, _) => s"variant"
    		case ConstructorV(_) => s"constructor"
     	}
    }

	def ownTests: Unit = {
		test(run("{+ 7 8}"), "15")
		test(run("{{fun {x: num} {{fun {f: {num -> num}} {+ {f 1} {{fun {x: num} {f 2}} 3}}} {fun {y: num} {+ x y}}}} 0}"), "3")

		test(typeCheck("{tyfun {a} 3}"), Type("{^ a num}"))
	  test(typeCheck("{tyfun {a} {tyfun {b} 3}}"), Type("{^ a {^ b num}}"))
	  test(typeCheck("{tyfun {a} {fun {x: a} x}}"), Type("{^ a {a -> a}}"))
	  test(typeCheck("{tyfun {a} {tyfun {b} {fun {x: {^ a {^ b a}}} x}}}"), Type("{^ a {^ b {{^ a {^ b a}} -> {^ a {^ b a}}}}}"))
	  test(typeCheck("{@ {tyfun {a} {tyfun {b} {fun {x: {^ a {^ b a}}} x}}} num}"), Type("{^ b {{^ a {^ b a}} -> {^ a {^ b a}}}}"))
	  test(typeCheck("{fun {x: {^ a a}} x}"), Type("{{^ a a} -> {^ a a}}"))
	  testExc(typeCheck("{fun {x: {^ a {a -> b}}} x}"), "free")
	  testExc(typeCheck("{tyfun {a} {fun {x: b} x}}"), "free")
	  testExc(typeCheck("{@ {tyfun {a} {fun {x: b} x}} num}"), "free")
	  testExc(typeCheck("{tyfun {a} {fun {x: a} {tyfun {b} {fun {y: b} {+ x y}}}}}"), "no")
	  test(typeCheck("{tyfun {a} 3}"), Type("{^ a num}"))
	  test(typeCheck("{tyfun {a} {tyfun {b} 3}}"), Type("{^ a {^ b num}}"))
	  test(typeCheck("{tyfun {a} {fun {x: a} x}}"), Type("{^ a {a -> a}}"))
	  test(typeCheck("{@ {tyfun {a} {fun {x: a} x}} {^ b {b -> b}}}"), Type("{{^ b {b -> b}} -> {^ b {b -> b}}}"))
	  test(typeCheck("{tyfun {a} {tyfun {b} 3}}"), Type("{^ a {^ b num}}"))
	  test(typeCheck("{tyfun {a} {fun {x: a} x}}"), Type("{^ a {a -> a}}"))
	  test(typeCheck("{tyfun {a} {tyfun {b} {fun {x: a} x}}}"), Type("{^ a {^ b {a -> a}}}"))
	  test(typeCheck("{if true {tyfun {a} {fun {x: a} x}} {tyfun {b} {fun {y: b} y}}}"), Type("{^ a {a -> a}}"))
	  test(typeCheck("{if true {tyfun {b} {fun {y: b} y}} {tyfun {a} {fun {x: a} x}}}"), Type("{^ b {b -> b}}"))
	  test(typeCheck("{if {= 8 8} {tyfun {a} {tyfun {b} {fun {x: a} x}}} {tyfun {b} {tyfun {a} {fun {x: b} x}}}}"), Type("{^ a {^ b {a -> a}}}"))
	  test(typeCheck("{tyfun {a} {fun {x: a} {tyfun {b} {fun {y: a} {if true x y}}}}}"), Type("{^ a {a -> {^ b {a -> a}}}}"))
	  test(typeCheck("{tyfun {a} {fun {a: {num -> num}} {fun {x: a} x}}}"), Type("{^ a {{num -> num} -> {a -> a}}}"))
	  test(typeCheck("{fun {a: {num -> num}} {tyfun {a} {fun {x: a} x}}}"), Type("{{num -> num} -> {^ a {a -> a}}}"))
	  test(typeCheck("{@ {tyfun {a} {fun {x: {^ a {a -> a}}} x}} num}"), Type("{{^ a {a -> a}} -> {^ a {a -> a}}}"))
	  test(typeCheck("{@ {tyfun {a} {fun {x: a} 5}} num}"), Type("{num -> num}"))
	  test(typeCheck("{if {= 8 10} {tyfun {a} {tyfun {b} {fun {x: a} {fun {y: b} y}}}} {tyfun {b} {tyfun {a} {fun {x: b} {fun {y: a} y}}}}}"), Type("{^ a {^ b {a -> {b -> b}}}}"))
	  test(typeCheck("{@ {tyfun {a} {fun {a: a} {{fun {x: {^ a {a -> a}}} {{@ x num} 10}} {tyfun {b} {fun {b: b} b}}}}} {num -> num}}"), Type("{{num -> num} -> num}"))
	  test(typeCheck("{@ {tyfun {a} {fun {a: a} {{fun {x: {^ a {a -> a}}} {{@ x num} 10}} {tyfun {b} {fun {b: b} b}}}}} num}"), Type("{num -> num}"))
	  test(typeCheck("{@ {tyfun {a} {fun {a: a} {{fun {x: {^ a {a -> a}}} {{@ x num} 10}} {tyfun {a} {fun {a: a} a}}}}} num}"), Type("{num -> num}"))
	  test(typeCheck("{tyfun {a} 3}"), Type("{^ a num}"))
	  test(typeCheck("{tyfun {a} {tyfun {b} 3}}"), Type("{^ a {^ b num}}"))
	  test(typeCheck("{tyfun {a} {fun {x: a} x}}"), Type("{^ a {a -> a}}"))
	  test(typeCheck("{if true {tyfun {a} {fun {x: a} x}} {tyfun {b} {fun {y: b} y}}}"), Type("{^ a {a -> a}}"))
	  test(typeCheck("{if true {tyfun {b} {fun {y: b} y}} {tyfun {a} {fun {x: a} x}}}"), Type("{^ b {b -> b}}"))
	  test(typeCheck("{if true {tyfun {a} {tyfun {b} {fun {x: a} x}}} {tyfun {b} {tyfun {a} {fun {x: b} x}}}}"), Type("{^ a {^ b {a -> a}}}"))
	  test(typeCheck("{tyfun {a} {fun {x: a} {tyfun {b} {fun {y: a} {if true x y}}}}}"), Type("{^ a {a -> {^ b {a -> a}}}}"))
	  test(typeCheck("{fun {x: {^ a a}} x}"), Type("{{^ a a} -> {^ a a}}"))
	  test(typeCheck("{@ {tyfun {a} {tyfun {b} {fun {x: {^ a {^ b a}}} x}}} num}"), Type("{^ b {{^ a {^ b a}} -> {^ a {^ b a}}}}"))
	  test(typeCheck("{{@ {@ {tyfun {a} {tyfun {b} {fun {x: a} x}}} num} num} 10}"), Type("num"))
	  test(typeCheck("{withtype {foo {a num} {b num}} {cases foo {a 3} {a {n} {+ n 3}} {b {n} {+ n 4}}}}"), Type("num"))

	  // interpretor
	  test(run("{{{@ {tyfun {a} {fun {f: a} f}} {num -> num}} {fun {x: num} x}} 10}"), "10")
	  test(run("{@ {tyfun {a} {fun {f: a} f}} {num -> num}}"), "function")
	  test(run("{@ {@ {tyfun {a} {tyfun {b} 3}} num} num}"), "3")
	  test(run("{tyfun {a} {fun {x: b} x}}"), "function")
	  test(run("{{fun {x: num} {{fun {f: {num -> num}} {+ {f 1} {{fun {x: num} {f 2}} 3}}} {fun {y: num} {+ x y}}}} 0}"), "3")
	  test(run("{@ {tyfun {a} {fun {x: a} x}} num}"), "function")
	  test(run("{tyfun {a} {tyfun {b} 3}}"), "3")
	  test(run("{{{@ {tyfun {a} {fun {f: a} f}} {num  -> num}} {fun {x: num} x}} 10}"), "10")
	  test(run("{@ {tyfun {a} {fun {f: a} f}} {num -> num}}"), "function")
	  test(run("{@ {@ {tyfun {a} {tyfun {b} 3}} num} num}"), "3")
	  test(run("{@ {tyfun {a} {fun {f: a} f}} {num -> num}}"), "function")
	  test(run("{{@ {if true {tyfun {a} {fun {x: a} x}} {tyfun {b} {fun {x: b} b}}} x} 30}"), "30")
	  test(run("{{fun {x: {^ a {a -> a}}} {{@ x num} 10}} {tyfun {b} {fun {y: b} y}}}"), "10")
	  test(run("{@ {tyfun {a} {fun {x: a} 5}} num}"), "function")
	  test(run("{@ {tyfun {a} {fun {x: {^ a {a -> a}}} x}} num}"), "function")
	  test(run("{{{@ {@ {tyfun {a} {tyfun {b} {fun {x: {a -> b}} x}}} num} num} {fun {x: num} {+ 5 x}}} 3}"), "8")
	  test(run("{{{@ {@ {tyfun {a} {tyfun {a} {fun {x: {a -> a}} x}}} num} num} {fun {x: num} {+ 5 x}}} 3}"), "8")
	  test(run("{@ {@ {tyfun {a} {tyfun {b} 3}} num} num}"), "3")
	  test(run("{{@ {@ {tyfun {a} {tyfun {b} {fun {x: a} x}}} num} num} 10}"), "10")
	  test(run("{with {f: {num -> num} {recfun {f: {num -> num} x: num} {if {= x 0} 0 {+ {f {- x 1}} x}}}} {f 10}}"), "55")

	}
}