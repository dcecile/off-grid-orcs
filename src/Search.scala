package offGridOrcs

import scala.util.Random
import scala.collection.mutable

object Search {
  type Path = List[Vec2]
  type Frontier = Seq[Path]

  def findPathToTrees(orc: Orc, world: World, stockpilePosition: Vec2): Option[Path] = {
    val frontiers = findFrontiers(orc.position, world).take(32)
    val solutionsOption = pickSolutionFrontier(frontiers, world)
    solutionsOption.map(pickBestSolution(stockpilePosition))
  }

  def pickBestSolution(stockpilePosition: Vec2)(solutionFrontier: Frontier): Path = {
    val bestSolution = solutionFrontier.minBy(solution =>
      (stockpilePosition - solution.head).gridLength)
    bestSolution.reverse.tail
  }

  def pickSolutionFrontier(frontiers: Stream[Frontier], world: World): Option[Frontier] = {
    frontiers
      .map(pickSolutionPaths(world))
      .find(_.nonEmpty)
  }

  def pickSolutionPaths(world: World)(frontier: Frontier): Frontier = {
    frontier.filter(path =>
      world(path.head).structure match {
        case Tile.Trees(_) => true
        case _ => false
      })
  }

  def findFrontiers(startPosition: Vec2, world: World): Stream[Frontier] = {
    val visited = mutable.Set(startPosition)

    def loopNewPath(newPath: Path, tile: Tile): Option[Path] = {
      tile match {
        case Tile(_, _, Some(_), _, _, _, _) =>
          // Obstructed by orc
          None
        case Tile(_, _, _, Some(_), _, _, _) =>
          // Obstructed by demon
          None
        case Tile(_, _, _, _, Some(_), _, _) =>
          // Obstructed by building
          None
        case _ =>
          // Found an unobstructed path
          Some(newPath)
      }
    }

    def loopPath(currentPath: Path): Frontier = {
      val currentPosition = currentPath.head
      val directions = Random.shuffle(Vec2.FourDirections)
      directions
        .map(currentPosition + _)
        .filter(world.isPositionValid(_))
        .filter(!visited.contains(_))
        .map({newPosition =>
          visited += newPosition
          (newPosition :: currentPath, world(newPosition))
        })
        .map(Function.tupled(loopNewPath))
        .flatten
    }

    def loop(currentFrontier: Frontier): Frontier = {
      currentFrontier.flatMap(loopPath)
    }

    val initialFrontier = Seq(startPosition :: Nil)
    Stream.iterate(initialFrontier)(loop)
      .takeWhile(_.nonEmpty)
  }
}
