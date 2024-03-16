package task5

object Task:

  val isPositive = (x: Int) =>
    x match
      case x if x >= 0 => "Positive"
      case _ => "Negative"

  val isPositive1: Int => String =
    _ match
      case n if n >= 0 => "Positive"
      case _ => "Negative"

  val isPositive2: Int => String = (x: Int) =>
    x match
      case x if x >= 0 => "Positive"
      case x => "Negative"

  val isPositive3: Int => String = {
    case x if x >= 0 => "Positive"
    case x => "Negative"
  }

  def isPositive4(i: Int): String =
    i match
      case n if n >= 0 => "Positive"
      case _ => "Negative"

