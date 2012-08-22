object Foo {
  object X { def unapply(x : Int)(y: Option[Int] = None): Option[Tuple2[Int, Int]] = Some((2,2)) }
  42 match { case _ X _ => () }
}
