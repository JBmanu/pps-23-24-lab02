package task5

import task5.Shape.{Circle, Rectangle, Square}

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

  val gcd: (Int, Int) => Int = (a, b) =>
    (a, b) match
      case _ if b == 0 => a
      case _ if a > b => gcd(b, a % b)

object Shape
  enum Shape:
    case Rectangle(b: Double, h: Double)
    case Circle(r: Double)
    case Square(l: Double)

  def perimeter(s: Shape): Double =
    s match
      case Rectangle(b, h) => (2 * b) + (2 * h)
      case Circle(r) => 2 * Math.PI * r
      case Square(l) => l * 4

  def scaling(s: Shape, a: Double): Shape =
    s match
      case Rectangle(b, h) => Rectangle(a * b, a * h)
      case Circle(r) => Circle(a * r)
      case Square(l) => Square(a * l)


object OptionalExtension:
  import task5.Optionals.Optional
  import task5.Optionals.Optional.*
  
  def map[A, B](o: Optional[A])(f: A => B): Optional[B] =
    o match
      case Maybe(v) => Maybe(f(v))
      case Empty() => Empty()
      
      
