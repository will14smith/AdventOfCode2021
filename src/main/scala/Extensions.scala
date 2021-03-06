extension[A, B] (x: (A, B))
  def applyLeft[C](f: A => C): (C, B) = (f(x._1), x._2)
  def applyRight[C](f: B => C): (A, C) = (x._1, f(x._2))

extension (i: Int)
  def clampLower(l: Int): Int = if i < l then l else i
  def clampUpper(h: Int): Int = if i > h then h else i
  def between(incLower: Int, incHigher: Int): Boolean = incLower <= i && i <= incHigher

extension (i: Long)
  def clampLower(l: Long): Long = if i < l then l else i
  def clampUpper(h: Long): Long = if i > h then h else i
  def between(incLower: Long, incHigher: Long): Boolean = incLower <= i && i <= incHigher

extension (x: (Int, Int))
  def min = if x._1 < x._2 then x._1 else x._2
  def max = if x._1 > x._2 then x._1 else x._2

extension[A] (i : IterableOnce[A])
  def freq: Map[A, Long] = i.iterator.toList.groupMapReduce(identity)(_ => 1L)(_ + _)

extension[A] (f: Map[A, Long])
  def freqFlatMap[B](fn: A => IterableOnce[B]): Map[B, Long] = f.iterator.flatMap(s => fn(s._1).iterator.map((_, s._2))).toList.groupMapReduce(_._1)(_._2)(_ + _)