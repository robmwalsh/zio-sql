trait Capabilities[-A]    {
  def show(a: A): Unit
}
trait DefaultCapabilities {
  implicit case object NoString extends Capabilities[String] {
    override def show(a: String): Unit = println(s"what's a string?")
  }
  implicit case object NoInt    extends Capabilities[Int]    {
    override def show(a: Int): Unit = println(s"what's an int?")
  }
}
trait Literate[-A] extends Capabilities[A]
object Literate    extends DefaultCapabilities {
  implicit case object LiterateString extends Literate[String] {
    override def show(a: String): Unit = println(s"the string is: $a")
  }
}
trait Numerate[-A] extends Capabilities[A] with DefaultCapabilities
object Numerate    extends DefaultCapabilities {
  implicit case object NumerateInt extends Numerate[Int] {
    override def show(a: Int): Unit = println(s"the int is: $a")
  }
}

trait Person {
  type Lower[-A] <: Capabilities[A]
  def read[A](a: A)(implicit capabilities: Lower[A]) = capabilities.show(a)
}

object InnumerateIlliteratePerson extends Person {
  type Lower[-A] = Capabilities[A]
}
object LiteratePerson             extends Person {
  type Lower[-A] = Literate[A]
}
object NumeratePerson             extends Person {
  type Lower[-A] = Numerate[A]
}
object LiterateNumeratePerson     extends Person {
  type Lower[-A] = Literate[A] with Numerate[A]
}

InnumerateIlliteratePerson.read(42)
InnumerateIlliteratePerson.read("hello")
LiteratePerson.read(42)
LiteratePerson.read("hello")
NumeratePerson.read(42)
NumeratePerson.read("hello")
LiterateNumeratePerson.read(42)
LiterateNumeratePerson.read("hello")
