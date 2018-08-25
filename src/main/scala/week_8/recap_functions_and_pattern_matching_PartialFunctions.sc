val f1: Function[String, String] = {
  case "ping" => "pong"
}
val f2: String => String = {
  case "ping" => "pong"
}

f1("ping")
f2("ping")
// f1("abc") MatchError

//////////

val pf1: PartialFunction[String, String] = {
  case "ping" => "pong"
}

if (pf1.isDefinedAt("ping")) pf1("ping")
if (pf1.isDefinedAt("abc")) pf1("abc")

//////////
//////////
//////////

def tryCall(g: PartialFunction[List[Int], String])(list: List[Int]): Unit = {
  if (g.isDefinedAt(list)) print(g(list))
}

//////////

val g1: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: y :: rest => "two"
}
val g1TryCaller: Function[List[Int], Unit] = tryCall(g1)
g1TryCaller(List.empty)
g1TryCaller(List(1))
g1TryCaller(List(1, 2))
g1TryCaller(List(1, 2, 3))

//////////

val g2: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: rest => rest match {
    case Nil => "two"
  }
}
val g2TryCaller: Function[List[Int], Unit] = tryCall(g2)
g2TryCaller(List.empty)
g2TryCaller(List(1))
//g2TryCaller(List(1, 2)) MatchError
//g2TryCaller(List(1, 2, 3)) MatchError

//////////
