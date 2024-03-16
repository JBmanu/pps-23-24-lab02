package task5

import org.junit.Assert.assertEquals
import org.junit.Test


class TaskTest:

  import Task.*

  @Test def positiveNumberWithLambda(): Unit =
    assertEquals("Positive", isPositive(0))
    assertEquals("Negative", isPositive1(-1))
    assertEquals("Negative", isPositive2(-1))
    assertEquals("Positive", isPositive3(0))
    assertEquals("Positive", isPositive4(0))
