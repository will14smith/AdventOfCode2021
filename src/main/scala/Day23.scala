import scala.collection.mutable

object Day23 extends Day[Unit, Int, Int] {
  def parse(input: String): Unit = { }

  def part1(a: Unit): Int = {
    val nodes = List(
      Node(0, Set(1), None),
      Node(1, Set(0, 2), None),
      Node(2, Set(1, 3, 11), None),
      Node(3, Set(2, 4), None),
      Node(4, Set(3, 5, 13), None),
      Node(5, Set(4, 6), None),
      Node(6, Set(5, 7, 15), None),
      Node(7, Set(6, 8), None),
      Node(8, Set(7, 9, 17), None),
      Node(9, Set(8, 10), None),
      Node(10, Set(9), None),
      Node(11, Set(2, 12), Some('A')),
      Node(12, Set(11), Some('A')),
      Node(13, Set(4, 14), Some('B')),
      Node(14, Set(13), Some('B')),
      Node(15, Set(6, 16), Some('C')),
      Node(16, Set(15), Some('C')),
      Node(17, Set(8, 18), Some('D')),
      Node(18, Set(17), Some('D')),
    )

    val amphipods = List(
      Amphipod('D', 11),
      Amphipod('B', 12),
      Amphipod('D', 13),
      Amphipod('A', 14),
      Amphipod('C', 15),
      Amphipod('A', 16),
      Amphipod('B', 17),
      Amphipod('C', 18),
    )

    solve(nodes, amphipods)
  }

  def part2(a: Unit): Int = {
    val nodes = List(
      Node(0, Set(1), None),
      Node(1, Set(0, 2), None),
      Node(2, Set(1, 3, 11), None),
      Node(3, Set(2, 4), None),
      Node(4, Set(3, 5, 15), None),
      Node(5, Set(4, 6), None),
      Node(6, Set(5, 7, 19), None),
      Node(7, Set(6, 8), None),
      Node(8, Set(7, 9, 23), None),
      Node(9, Set(8, 10), None),
      Node(10, Set(9), None),
      Node(11, Set(2, 12), Some('A')),
      Node(12, Set(11, 13), Some('A')),
      Node(13, Set(12, 14), Some('A')),
      Node(14, Set(13), Some('A')),
      Node(15, Set(4, 16), Some('B')),
      Node(16, Set(15, 17), Some('B')),
      Node(17, Set(16, 18), Some('B')),
      Node(18, Set(17), Some('B')),
      Node(19, Set(6, 20), Some('C')),
      Node(20, Set(19, 21), Some('C')),
      Node(21, Set(20, 22), Some('C')),
      Node(22, Set(21), Some('C')),
      Node(23, Set(8, 24), Some('D')),
      Node(24, Set(23, 25), Some('D')),
      Node(25, Set(24, 26), Some('D')),
      Node(26, Set(25), Some('D')),
    )

    val amphipods = List(
      Amphipod('D', 11), //
      Amphipod('D', 12),
      Amphipod('D', 13),
      Amphipod('B', 14), //
      Amphipod('D', 15), //
      Amphipod('C', 16),
      Amphipod('B', 17),
      Amphipod('A', 18), //
      Amphipod('C', 19), //
      Amphipod('B', 20),
      Amphipod('A', 21),
      Amphipod('A', 22), //
      Amphipod('B', 23), //
      Amphipod('A', 24),
      Amphipod('C', 25),
      Amphipod('C', 26), //
    )

    solve(nodes, amphipods)
  }

  def solve(nodes: List[Node], amphipods: List[Amphipod]): Int = {
    val data = Model(nodes.toArray, amphipods.map(n => (n.position, n)).toMap)

    val allPaths = paths(data)
    val fromRooms = allPaths.zipWithIndex.map(x => {
      if !data.isCorridor(x._2) then x._1.filter(p => data.isStoppableCorridor(p.last)) else List()
    })
    val fromCorridors = allPaths.zipWithIndex.map(x => {
      if data.isStoppableCorridor(x._2) then x._1.filter(p => !data.isCorridor(p.last)).groupMapReduce(p => { data.nodes(p.last).target.get })(p => List(p))(_ ++ _) else Map()
    })
    val ps = Paths(fromRooms, fromCorridors)

    min(ps, data)
  }

  def min(paths: Paths, data: Model): Int = {
    val q = new mutable.PriorityQueue[(Model, Int)]()(Ordering.by[(Model, Int), Int](_._2))
    val checked = new mutable.HashMap[Model, Int]

    q.enqueue((data, 0))

    var min = Int.MaxValue

    while(q.nonEmpty) {
      val (current, score) = q.dequeue()

      val best = checked.getOrElse(current, Int.MaxValue)

      if(score < best) {
        checked.update(current, score)

        if (isDone(current)) {
          min = Math.min(min, score)
        } else {
          val nextStates = next(paths, current).map(_.applyRight(_ + score))
          q.addAll(nextStates)
        }
      }
    }

    min
  }

  def paths(data: Model): Array[List[Path]] = data.nodes.map(n => pathsFrom(data, n._1))
  def pathsFrom(data: Model, start: Int): List[Path] = {
    var queue = data.nodes(start).neighbours.map(Array(_)).toList
    var visited = Set(start)
    var paths = List.empty[Path]

    while(queue.nonEmpty) {
      val path = queue.head
      queue = queue.tail

      if(!visited.contains(path.last)) {
        paths = path :: paths

        queue = queue ++ data.nodes(path.last).neighbours.map(path.appended)

        visited = visited + path.last
      }
    }

    paths
  }

  def next(paths: Paths, data: Model): Iterable[(Model, Int)] = data.amphipods.values.flatMap(a => next(paths, data, a))
  def next(paths: Paths, data: Model, amphipod: Amphipod): List[(Model, Int)] = {
    val roomIsOk = !hasOthersInRoom(data, amphipod)
    if(isDone(data, amphipod) && roomIsOk) {
      return List()
    }

    val availablePaths = if data.isCorridor(amphipod.position) then {
      if(!roomIsOk) {
        List()
      } else {
        paths.fromCorridor(amphipod.position)(amphipod.id)
          .filter(isPathFree(data, _))
          .maxByOption(_.length)
          .toList
      }
    }
    else {
      paths.fromRoom(amphipod.position).filter(isPathFree(data, _))
    }

    availablePaths.map(p => (data.move(amphipod.position, p.last, amphipod.id), amphipod.cost * p.length))
  }

  def isDone(data: Model): Boolean = data.amphipods.forall(a => isDone(data, a._2))
  def isDone(data: Model, amphipod: Amphipod): Boolean = data.nodes(amphipod.position).target.getOrElse(' ') == amphipod.id

  def hasOthersInRoom(data: Model, amphipod: Amphipod): Boolean = {
    data.nodes.exists(n => if n.target.contains(amphipod.id) then {
      val a = data.amphipods.get(n.position)
      if a.isEmpty then false else a.get.id != amphipod.id
    } else false)
  }

  def isPathFree(model: Day23.Model, path: Day23.Path): Boolean = path.forall(model.isEmpty)

  type Path = Array[Int]
  case class Paths(fromRoom: Array[List[Path]], fromCorridor: Array[Map[Char, List[Path]]])

  case class Node(position: Int, neighbours: Set[Int], target: Option[Char]) {
    val isCorridor: Boolean = target.isEmpty
  }
  case class Amphipod(id: Char, position: Int) {
    def cost: Int = id match {
      case 'A' => 1
      case 'B' => 10
      case 'C' => 100
      case 'D' => 1000
    }
  }
  case class Model(nodes: Array[Node], amphipods: Map[Int, Amphipod]) {
    def move(oldPos: Int, newPos: Int, id: Char): Model = Model(nodes, amphipods.removed(oldPos).updated(newPos, Amphipod(id, newPos)))

    def isEmpty(position: Int): Boolean = !amphipods.contains(position)
    def isCorridor(position: Int): Boolean = nodes(position).isCorridor
    def isStoppableCorridor(position: Int): Boolean = nodes(position).isCorridor && nodes(position).neighbours.forall(isCorridor)
  }
}
