package example

import scala.util.parsing.combinator.* 

enum Expr:
    case Fun(param: BigInt, body: Expr)
    case App(fun: Expr, param: Expr)
    case Val(count: BigInt)

    def str : String = this match
        case Fun(p, b)  => "[λ" + ('a'+p-1).charValue + ".(" + b.str + ")]"
        case App(f, p)  => "{"+f.str + " ("+p.str+")}"
        case Val(c)     => ('a'+c-1).charValue + ""
    

type Env = Map[BigInt, Value]

enum Value:
    case CloV(param: BigInt, body: Expr, env: ()=>Env)
    case NumV(v: BigInt)
    case IdV()

    def str : String = this match
        case CloV(p, b, e) => s"${p}->${b}"
        case _ => "<NOPE>" 
    

object Program extends Parser.From(Parser.program)
object Expr extends Parser.From(Parser.expr)
object Parser extends RegexParsers with PackratParsers{
    type P[+T] = PackratParser[T]
    class From[T](p: Parser[T]) {
        def apply(s: String): T = parseAll(p, s) match {
            case Success(result, next) => result
            case Failure(msg, next) => throw Exception("Parser failure:" + msg)
            case Error(msg, next) =>  throw Exception("Parser error: " + msg)
        };
    }

    private lazy val num: P[BigInt] = s"-?[0-9]+".r ^^ BigInt.apply

    lazy val program: P[List[Expr]] = rep1sep(expr,"\n")

    lazy val expr: P[Expr] =
        import Expr.*
        lazy val e1: P[Expr] =  e2 ~ rep("!" ~> e1 <~ "?") ^^ {case f ~ p => p.foldLeft(f)(App.apply)} 
        lazy val e2: P[Expr] = (
            ("후" ~> rep("루")) ~ e1 <~ "~" ^^ {case c ~ e =>Fun(c.length, e)} |
            "탕" ~> rep("탕") ^^ {list=>Val(list.length+1)}
        )
        e1
}
