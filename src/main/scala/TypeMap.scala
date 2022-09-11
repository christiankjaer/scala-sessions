object TypeMap {

  type Member[Ts <: Tuple, Key <: String] = Ts match {
    case (Key, x) *: _ => x
    case _ *: rest     => Member[rest, Key]
  }

}
