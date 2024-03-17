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

