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


