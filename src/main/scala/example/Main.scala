package example

import example.Interpreter.interp
import example.Interpreter.parseEncoding
import cats.instances.float

@main def main(args: String*): Unit =
  //println(Expr("후루루 후루 탕탕!탕?~!0?~!후루루루 탕탕탕~?").str)
  //println(interp(Expr("후루루후루탕탕!탕?~!0?~!후루루루탕탕탕~?"), Map()))

  //println(interp(Expr("후루루 후루 탕탕!탕?~~!1?!2?"), Map()))
 
  val res = interp(Expr(commonlib(
    """
    탕탕탕탕탕 ! 탕탕탕 ? ! 탕탕탕탕탕탕탕탕탕탕 !탕탕!탕탕!탕??? !탕탕!탕?? ? ! 탕탕!탕? ?
    """
  )), ()=>Map())
  //println(fact.str)
  //println(s"${fact}")
  println(s"${res.str}")
  println(parseEncoding(res))

def error(msg: String): Nothing = throw Exception(msg)

object Interpreter {
  import Expr.* 
  import Value.* 

  def process(expr: List[Expr]) : List[Value] = expr.map(e=>interp(e, ()=>Map()))

  def interp(expr: Expr, env: ()=>Env) : Value = expr match {
    case App(f, e) => interp(f,env) match {
      case CloV(p, b, fenv) => interp(b, ()=> fenv() + (p->interp(e, env)))
      case NumV(n) => interp(e, env) match {
        case NumV(v) => NumV(v+1)
        case IdV() => NumV(n+1)
        //case CloV(p, b, fenv) => interp(b, fenv + (p->interp(e, env))) 
        case _ => error(s"${e.str}: not computable env: ${env}")
      } 
      case v => error(s"${v}: not a function")
    }
    case Fun(param, body) => CloV(param, body, env)
    case Val(count) => env().getOrElse(count, error(s"$count: unbound identifier"));
  }

  def parseEncoding(n: Value): BigInt = n match
    case CloV(p, body, fenv) => {
      interp(body, () => fenv() + (p->NumV(0))) match
      case CloV(p, b, fenv) => println(s"${p}->${b.str}"); println(s"${fenv()}"); 
        interp(b, () => fenv() + (p->IdV())) match
          case NumV(n) => n
          case IdV() => 0
          case x => error(s"${b.str}: not a encoding")      
      //case NumV(n) => n
      //case Fun(_, body) => functionRep(body, 0)
      case _ => error(s"${body.str}: not a encoding")
    }
    case _ => error(s"${n}: not a CloV")

  def functionRep(n: Expr, c: BigInt) : BigInt = n match
    case App(_, p) => functionRep(p, c+1)
    case Val(count) => c
    case _ => error(s"${n.str}: not a encoding")
  
}

def commonlib(body:String) : String = """
후루루루루루루루루루루루루루루루루루루루루 후루 후루루 후루루루 후루루루루 후루루루루루 후루루루루루루 후루루루루루루루 후루루루루루루루루 후루루루루루루루루루루 후루루루루루루루루루

"""
+ // Body
body
+ // Body
"""

~
!
후루루루루루루루루루 후루루루루루루루루루루루 탕탕탕탕탕탕탕탕탕!탕탕탕탕탕탕탕탕탕탕탕!탕탕탕탕탕탕탕탕탕탕탕??~
!
후루루루루루루루루루루루 
탕탕탕탕탕탕탕탕탕!탕탕탕탕탕탕탕탕탕탕탕!탕탕탕탕탕탕탕탕탕탕탕??~
?
~
? 
~~
!

후루루루루루루루루 후루루루루루루루루루 후루루루루루루루루루루 탕탕탕탕탕탕탕탕 ! 
후루루루루루루루루루루루루 
  후루루루루루루루루루루루루루 
  탕탕탕탕탕탕탕탕탕탕탕탕탕 ! 탕탕탕탕탕탕탕탕탕탕탕탕 ! 탕탕탕탕탕탕탕탕탕 ? ?
  ~
~
?
!후루루루루루루루루루루루루루루루 탕탕탕탕탕탕탕탕탕탕~? !후루루루루루루루루루루루루루루루루 탕탕탕탕탕탕탕탕탕탕탕탕탕탕탕탕~?
~ ~ ~
?
!
후루루루루루루루루루루루루 
후루루루루루루루루루루루루루
탕탕탕탕탕탕탕탕탕탕탕탕
!
탕탕탕탕탕탕!탕탕탕탕탕탕탕탕탕탕탕탕탕?
?
!탕?
~
~
?
~~
!후루루루루루루 후루루루루루루루 탕탕탕탕탕탕!탕탕?!탕탕탕탕탕탕탕?~~?
!후루루루루루루 탕탕탕탕탕탕!후루루루루루루루 탕탕탕탕~?!탕탕탕?~?
~~~~~~
!후루 후루루 탕!탕탕?~~?
!후루 후루루 탕탕~~?
!후루 후루루 후루루루 탕탕!탕!탕탕?!탕탕탕??~~~?
!후루 후루루 탕~~?
!후루 후루루 탕탕~~?
!후루 후루루 후루루루 탕!탕탕?!탕탕탕?~~~?    
    """

     val succ4 = Expr("후루루루 후루루 후루 탕탕!탕탕탕!탕탕?!탕??~~~!후루루 후루 탕탕!탕?~~?")
  val zerosuccbind = Expr(
    """후루 후루루 후루루루 후루루루루 탕탕탕탕 !탕탕 !탕탕 !탕??? !탕탕 !탕??~
    !후루루루루 후루루루루루 탕탕탕탕!탕탕탕!탕탕탕탕탕??!후루루루루루루루 후루루루루루루 탕탕탕탕탕탕~~?~~?
    ~
    !후루루루루 후루루루 탕탕탕탕!탕탕?!탕탕탕?~~?
    ~~
    !후루루 후루 탕탕!탕?~~?
    !후루루루 후루루 후루 탕탕!탕탕탕!탕탕?!탕??~~~?
    """)
  val expr = Expr("후루루 후루 탕탕!탕?~~")

  val factYGn = """
  탕탕탕탕탕탕탕탕탕
!
후루루루루루루루루루루루 
후루루루루루루루루루루루루
탕탕탕탕탕! 탕탕탕탕탕탕탕 !탕탕탕탕탕탕탕탕탕탕탕탕??
!
탕탕!탕?
?
!
탕탕탕탕탕탕탕탕탕탕
!
탕탕탕탕탕탕탕탕탕탕탕탕
?
!
탕탕탕탕탕탕탕탕탕탕탕
!
탕탕탕탕탕탕탕탕
!
탕탕탕탕탕탕탕탕탕탕탕탕
?
?
?

?
~
~
?
!

탕탕!탕?

?

  """

  val fact = 
    """
후루 후루루 후루루루 후루루루루 후루루루루루 후루루루루루루 후루루루루루루루 후루루루루루루루루 후루루루루루루루루루 후루루루루루루루루루루
탕탕탕탕탕탕탕탕탕
!
후루루루루루루루루루루루 
후루루루루루루루루루루루루
탕탕탕탕탕!탕탕탕탕탕탕탕!탕탕탕탕탕탕탕탕탕탕탕탕??
!
탕탕!탕?
?
!
탕탕탕탕탕탕탕탕탕탕
!
탕탕탕탕탕탕탕탕탕탕탕탕
?
!
탕탕탕탕탕탕탕탕탕탕탕
!
탕탕탕탕탕탕탕탕
!
탕탕탕탕탕탕탕탕탕탕탕탕
?
?
?

?
~
~
?
!
탕탕!탕탕!탕탕!탕탕!탕탕!탕?????
?
~
!
후루루루루루루루루루 후루루루루루루루루루루 탕탕탕탕탕탕탕탕탕!탕탕탕탕탕탕탕탕탕탕!탕탕탕탕탕탕탕탕탕탕??~~
!
후루루루루루루루루루루 
탕탕탕탕탕탕탕탕탕!탕탕탕탕탕탕탕탕탕탕!탕탕탕탕탕탕탕탕탕탕??
~
?
? 
~~
!
후루루루루루루루루 탕탕탕탕탕탕탕탕!후루루루루루루루루루 후루루루루루루루루루루 탕탕탕탕탕탕탕!탕탕탕탕탕탕탕탕탕!탕탕!탕??!탕탕탕탕탕탕탕탕탕탕? 
!탕탕탕탕탕탕!탕탕탕탕탕탕탕탕탕!탕탕탕탕탕탕탕탕탕탕??
!탕탕!탕?? 
?
?~~?
!후루루루루루루루루루루루 탕~?
!탕? 
~
?
!
후루루루루루루루루루루루루 
후루루루루루루루루루루루루루
탕탕탕탕탕탕탕탕탕탕탕탕
!
탕탕탕탕탕탕!탕탕탕탕탕탕탕탕탕탕탕탕탕?!탕?
?
~
~
?
~~
!후루루루루루루 후루루루루루루루 탕탕탕탕탕탕!탕탕?!탕탕탕탕탕탕탕?~~?
!후루루루루루루 탕탕탕탕탕탕!후루루루루루루루 탕탕탕탕~?!탕탕탕?~?
~~~~~
!후루 후루루 탕탕~~?
!후루 후루루 후루루루 탕탕!탕!탕탕?!탕탕탕??~~~?
!후루 후루루 탕~~?
!후루 후루루 탕탕~~?
!후루 후루루 후루루루 탕!탕탕?!탕탕탕?~~~?    
    """