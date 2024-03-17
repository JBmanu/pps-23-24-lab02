package task5

import org.junit.*

import task5.Shape.{Circle, Rectangle, Square}
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}

object Task2a:
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
      case _ => "Negative"

  val isPositiveCompatStyle: Int => String = {
    case x if x >= 0 => "Positive"
    case _ => "Negative"
  }

  def isPositiveMethod(i: Int): String =
    i match
      case n if n >= 0 => "Positive"
      case _ => "Negative"

  val isEmpty: String => Boolean = _ == ""
  val neg: (String => Boolean) => (String => Boolean) = p => s => !p(s)

  def genericNeg[A](p: A => Boolean): A => Boolean = (a: A) => !p(a)

  class Task2aTest:
    @Test def positiveNumberWithLambda(): Unit =
      assertEquals("Positive", isPositiveEqualStyle(0))
      assertEquals("Negative", isPositiveDoublePointStyle(-1))
      assertEquals("Negative", isPositiveVerboseStyle(-1))
      assertEquals("Positive", isPositiveCompatStyle(0))
      assertEquals("Positive", isPositiveMethod(0))

    @Test def negFunction(): Unit =
      assertTrue(isEmpty(""))
      assertFalse(neg(isEmpty)(""))
      assertTrue(neg(isEmpty)("hello"))

    @Test def negGenericFunction(): Unit =
      val isPositive: Int => Boolean = _ >= 0
      val v = -1

      assertFalse(genericNeg(isEmpty)(""))
      assertTrue(genericNeg(isPositive)(v))


object Task2b:
  val notCurriedFun: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z

  def notCurriedMethod(x: Int, y: Int, z: Int): Boolean = (x <= y) && (y == z)

  val curriedFun: (Int => Int => Int => Boolean) = x => y => z => x <= y && y == z

  def curriedMethod(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z

  val compositeFun: (Int => Int, Int => Int) => (Int => Int) = (f, g) => (x) => f(g(x))
  def genCompositeFun[A](f: A => A, g: A => A): A => A = x => f(g(x))

  class Task2bTest:
    @Test def relationFunction(): Unit =
      val x = 5
      val y = 6
      val z = 6

      assertTrue(notCurriedFun(x, y, z))
      assertFalse(notCurriedFun(z, y, x))
      assertTrue(notCurriedMethod(x, y, z))
      assertFalse(notCurriedMethod(z, y, x))
      assertTrue(curriedFun(x)(y)(z))
      assertFalse(curriedFun(z)(y)(x))
      assertTrue(curriedMethod(x)(y)(z))
      assertFalse(curriedMethod(z)(y)(x))

  @Test def compositeFunction(): Unit =
    val x = 5
    assertEquals(9, compositeFun(_ - 1, _ * 2)(x))

    val f: Int => Int = x => x - 1
    assertEquals(9, genCompositeFun(f, _ * 2)(x))


object Task3
  val gcd: (Int, Int) => Int = (a, b) =>
    (a, b) match
      case _ if b == 0 => a
      case _ if a > b => gcd(b, a % b)

  class Task3Test:
    @Test def greatestCommonDivisor(): Unit =
      assertEquals(4, gcd(12, 8))
      assertEquals(7, gcd(14, 7))
      assertEquals(1, gcd(12, 7))

object Task4Shape
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

  class Task4ShapeTest:
    @Test def perimeterShape(): Unit =
      val rectangle = Rectangle(4d, 2d)
      val circle = Circle(3d)
      val square = Square(4d)

      assertEquals(12.0d, perimeter(rectangle), 0.0d)
      assertEquals(18.85d, perimeter(circle), 0.1d)
      assertEquals(16.0d, perimeter(square), 0.0d)

    @Test def scalingShape(): Unit =
      val rectangle = Rectangle(4d, 2d)
      val circle = Circle(3d)
      val square = Square(4d)
      val alpha = 2

      assertEquals(Rectangle(4 * alpha, 2 * alpha), scaling(rectangle, alpha))
      assertEquals(Circle(3 * alpha), scaling(circle, alpha))
      assertEquals(Square(4 * alpha), scaling(square, alpha))

object Task5OptionalExtension:
  import task5.Optionals.Optional
  import task5.Optionals.Optional.*

  def map[A, B](o: Optional[A])(f: A => B): Optional[B] =
    o match
      case Maybe(v) => Maybe(f(v))
      case _ => Empty()

  def filter[A](o: Optional[A])(f: A => Boolean): Optional[A] =
    o match
      case Maybe(v) if f(v) => o
      case _ => Empty()

  class Task5OptionalExtensionTest:
    @Test def optionalMap(): Unit =
      val x = Maybe(6)
      val f: Int => Boolean = _ > 5
      assertEquals(Maybe(true), map(x)(f))

    @Test def optionalFilter(): Unit =
      val v = Maybe(6)
      val f: Int => Boolean = _ > 5
      assertEquals(Maybe(6), filter(v)(f))
