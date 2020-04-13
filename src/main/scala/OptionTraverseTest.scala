object OptionTraverseTest extends App{
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight(Some(List()):Option[List[B]]){ (e, acc) =>
      for {
        a <- acc;
        b <- f(e)
      } yield b::a
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)

  val myList = List(1, 2, 3)
  println(traverse(myList)(Some(_)))
  val fun = (x: Int) => if (x%2==1) Some(x) else None
  println(traverse(myList)(fun))
  val myList2 = List(Option(1),Option(2),Option(3))
  println(sequence(myList2))
}
