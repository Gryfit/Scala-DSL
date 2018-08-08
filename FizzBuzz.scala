sealed trait Cmd

case object Skip extends Cmd
case object Halt extends Cmd
case class Print (x: String) extends Cmd


class FizzBuzz {
  type Program = List[Cmd]
  type Cont = Program => Program

  def interp(p: Program): String = p match {
    case List() => ""
    case Print(x)::xs => x + interp(xs)
    case Skip::xs => interp(xs)
    case Halt::xs => ""
  }

  def base(n: Int): Cont = (x: Program) => x ++ List(Print(n.toString))

  def fizz(n: Int): Cont = n%3 match {
    case 0 => (x: Program) => List(Print("fizz")) ++ x ++ List(Halt)
    case _ => identity
  }

  def buzz(n: Int): Cont = n%5 match {
    case 0 => (x: Program) => List(Print("buzz")) ++ x ++ List(Halt)
    case _ => identity
  }

  def fb(n: Int): Program = (base(n) compose fizz(n) compose buzz(n))(List(Skip))

  def fizzbuzz(n: Int): String = interp(fb(n))
}
object FizzBuzz extends App{
  val fbinstance = new FizzBuzz()
  for (x <- 1 to 100) {
    println(fbinstance.fizzbuzz(x))
  }
}
