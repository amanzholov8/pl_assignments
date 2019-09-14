test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
test(run("{with {g {fun {r} {access r c}}} {g {record {a 0} {c 12} {b 7}}}}"), "12")
test(run("{access {record {r {record {z 0}}}} r}"), "record")
test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
test(run("{with {f {fun {a b} {+ a b}}} {with {g {fun {x} {- x 5}}} {with {x {f 2 5}} {g x}}}}"), "2")
test(run("{with {f {fun {x y} {+ x y}}} {f 1 2}}"), "3")
test(run("{with {f {fun {} 5}} {+ {f} {f}}}"), "10")
test(run("{with {h {fun {x y z w} {+ x w}}} {h 1 4 5 6}}"), "7")
test(run("{with {f {fun {} 4}} {with {g {fun {x} {+ x x}}} {with {x 10} {- {+ x {f}} {g 4}}}}}"), "6")
test(run("{record {a 10} {b {+ 1 2}}}"), "record")
test(run("{access {record {r {record {z 0}}}} r}"), "record")
test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
test(run("{with {x 3} {with {y 5} {access {record {a x} {b y}} a}}}"), "3")
test(run("{with {f {fun {a b} {+ {access a a} b}}} {with {g {fun {x} {+ 5 x}}} {with {x {f {record {a 10} {b 5}} 2}} {g x}}}}"), "17")
test(run("{with {f {fun {a b c d e} {record {a a} {b b} {c c} {d d} {e e}}}} {access {f 1 2 3 4 5} c}}"), "3")
test(run("{with {f {fun {a b c} {record {a a} {b b} {c c}}}} {access {f 1 2 3} b}}"), "2")
test(run("{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}} {access {f 1 2 3} y}}"), "2")
test(run("{with {f {fun {x} {+ 5 x}}} {f {access {access {record {a {record {a 10} {b {- 5 2}}}} {b {access {record {x 50}} x}}} a} b}}}"), "8")
test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
test(run("{access {record {r {record {z 0}}}} r}"), "record")
test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
test(run("{record {a 10}}"), "record")
test(run("{access {record {a 10}} a}"), "10")
test(run("{access {record {a {+ 1 2}}} a}"), "3")
test(run("{fun {x} x}"), "function")
test(run("{access {record {a {record {b 10}}}} a}"), "record")
test(run("{access {access {record {a {record {a 10} {b 20}}}} a} a}"), "10")
test(run("{+ {access {record {a 10}} a} {access {record {a 20}} a}}"), "30")
test(run("{access {record {a 10}} a}"), "10")
test(run("{access {record {a {- 2 1}}} a}"), "1")
test(run("{access {record {a {record {b 10}}}} a}"), "record")
test(run("{access {access {record {a {record {a 10}}}} a} a}"), "10")
test(run("{access {access {record {a {record {a 10} {b 20}}}} a} a}"), "10")
test(run("{access {access {record {a {record {a 10} {b 20}}}} a} b}"), "20")
test(run("{access {record {r {record {z 0}}}} r}"), "record")
test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
test(run("{with {y {record {x 1} {y 2} {z 3}}} {access y y}}"), "2")
test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
test(run("{with {g {fun {r} {access r c}}} {g {record {a 0} {c 12} {b 7}}}}"), "12")
test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
testExc(run("{with {f {fun {} f}} {f}}"), "")
test(run("{{fun {f} {with {f {access f f}} {with {f {fun {} f}} {f}}}} {record {f 10}}}"), "10")
testExc(run("{{fun {f} {with {f {access f x}} {with {f {fun {} f}} {f}}}} {record {f 10}}}"), "no such field")
testExc(run("{{fun {a b c} {with {f {access b c}} {with {f {fun {} f}} {f}}}} {record {a 10}} {record {b 20}} {record {c 30}}}"), "no such field")
test(run("{{fun {a b c} {with {f {access c c}} {with {f {fun {} f}} {f}}}} {record {a 10}} {record {b 20}} {record {c 30}}}"), "30")
testExc(run("{{fun {a b c} {with {f {access b c}} {with {f {fun {} f}} {f}}}} {record {a 10}} {record {b 20}}}"), "wrong arity")
test(run("{{fun {x y z} {with {x {fun {x} {x z}}} {with {f {fun {x} x}} {with {z y} {z f x}}}}} {fun {x y} {x y}} {fun {x y} {y x}} 42}"), "42")
testExc(run("{{fun {x y z} {with {x {fun {x} {x z}}} {with {f {fun {x} x}} {with {z y} {z x x}}}}} {fun {x y} {x y}} {fun {x y} {y x}} 42}"), "")
testExc(run("{{fun {x y z} {with {x {fun {x} {x z}}} {y {with {z y} {z z x}} {{fun {x} y} x}}}} {fun {x y} {x y}} {fun {x y} {y x}} 42}"), "wrong arity")
test(run("{access {{fun {x y z} {with {x {fun {x} {x {record {a x} {x x} {y y} {z {+ {access z b} 40}}}}}} {with {f {fun {x} x}} {with {z y} {z f x}}}}} {fun {x y} {x y}} {fun {x y} {y x}} {record {a 1} {b 2} {c 3}}} z}"), "42")
