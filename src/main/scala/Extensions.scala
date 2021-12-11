extension[A, B] (x: (A, B))
  def applyLeft[C](f: A => C): (C, B) = (f(x._1), x._2)
  def applyRight[C](f: B => C): (A, C) = (x._1, f(x._2))

extension (i: Int)
  def clampLower(l: Int): Int = if i < l then l else i
