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
    
    
  
