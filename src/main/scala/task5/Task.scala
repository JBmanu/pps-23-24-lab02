package task5

object Task:

  val isPositiveEqualStyle = (x: Int) =>
    x match
      case x if x >= 0 => "Positive"
      case _ => "Negative"

  val isPositiveDoublePointStyle: Int => String =
    _ match
      case n if n >= 0 => "Positive"
      case _ => "Negative"

  val isPositiveVerboseStyle: Int => String = (x: Int) =>
    x match
      case x if x >= 0 => "Positive"
      case x => "Negative"

  val isPositiveCompatStyle: Int => String = {
    case x if x >= 0 => "Positive"
    case x => "Negative"
  }

  def isPositiveMethod(i: Int): String =
    i match
      case n if n >= 0 => "Positive"
      case _ => "Negative"

  val isEmpty: String => Boolean = _ == ""
  val neg: (String => Boolean) => (String => Boolean) = p => s => !p(s)

  def genericNeg[A](p: A => Boolean): A => Boolean = (a: A) => !p(a)


  val notCurriedFun: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z

  def notCurriedMethod(x: Int, y: Int, z: Int): Boolean = (x <= y) && (y == z)

  val curriedFun: (Int => Int => Int => Boolean) = x => y => z => x <= y && y == z

  def curriedMethod(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z

  val compositeFun: (Int => Int, Int => Int) => (Int => Int) = (f, g) => (x) => f(g(x))
  
  def genCompositeFun[A](f: A => A, g: A => A): A => A = x => f(g(x))