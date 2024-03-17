package task5

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test

class TaskTest:

  import Task.*

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

    assertFalse(genericNeg(isEmpty)(""))
    assertTrue(genericNeg(isPositive)(-1))

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

  @Test def greatestCommonDivisor(): Unit =
    assertEquals(4, gcd(12, 8))
    assertEquals(7, gcd(14, 7))
    assertEquals(1, gcd(12, 7))

  @Test def perimeterShape(): Unit =
    import task5.Shape.*

    val rectangle = Rectangle(4d, 2d)
    val circle = Circle(3d)
    val square = Square(4d)

    assertEquals(12.0d, perimeter(rectangle), 0.0d)
    assertEquals(18.85d, perimeter(circle), 0.1d)
    assertEquals(16.0d, perimeter(square), 0.0d)

  @Test def scalingShape(): Unit =
    import task5.Shape.*

    val rectangle = Rectangle(4d, 2d)
    val circle = Circle(3d)
    val square = Square(4d)
    val alpha = 2

    assertEquals(Rectangle(4 * alpha, 2 * alpha), scaling(rectangle, alpha))
    assertEquals(Circle(3 * alpha), scaling(circle, alpha))
    assertEquals(Square(4 * alpha), scaling(square, alpha))

  @Test def optionalMap(): Unit =
    import task5.OptionalExtension.map
    import task5.Optionals.Optional.*
    
    assertEquals(Maybe(true), map(Maybe(6))(_ > 5))

  @Test def optionalFilter(): Unit =
    import task5.OptionalExtension.filter
    import task5.Optionals.Optional.*

    assertEquals(Maybe(6), filter(Maybe(6))(_ > 5))


