import java.io.File

data class HexVector(val x: Int, val y: Int) {
  operator fun plus(v: HexVector) = HexVector(this.x + v.x, this.y + v.y)
}

fun inputDirectionToVector(direction: String): HexVector {
  return when (direction) {
    "e"  -> HexVector(2, 0)
    "w"  -> HexVector(-2, 0)
    "ne" -> HexVector(1, 1)
    "nw" -> HexVector(-1, 1)
    "se" -> HexVector(1, -1)
    "sw" -> HexVector(-1, -1)
    else -> throw IllegalArgumentException("Invalid direction")
  }
}

fun parseLineToDirections(line: String): List<String> {
  val directions = mutableListOf<String>();
  var pos = 0
  while (pos < line.length) {
    val direction = when (line[pos]) {
      'e'  -> "e"
      'w'  -> "w"
      'n'  -> "n" + line[pos + 1]
      's'  -> "s" + line[pos + 1]
      else -> throw IllegalArgumentException("Invalid line")
    }
    directions.add(direction)
    pos += direction.length
  }
  return directions
}

fun getAdjacentTiles(tile: HexVector): List<HexVector> {
  val adjacentTiles = mutableListOf<HexVector>();
  adjacentTiles.add(tile + HexVector(2, 0))
  adjacentTiles.add(tile + HexVector(-2, 0))
  adjacentTiles.add(tile + HexVector(1, 1))
  adjacentTiles.add(tile + HexVector(-1, 1))
  adjacentTiles.add(tile + HexVector(1, -1))
  adjacentTiles.add(tile + HexVector(-1, -1))
  return adjacentTiles
}

fun countAdjacentBlackTiles(blackTiles: Set<HexVector>, tile: HexVector): Int {
  return getAdjacentTiles(tile).filter { blackTiles.contains (it) }.size
}

fun main() {
  // Part 1
  val blackTiles: MutableSet<HexVector> = HashSet()
  File("input.txt").forEachLine { 
    val tile = parseLineToDirections(it)
      .map { inputDirectionToVector(it) }
      .reduce { a, b -> a + b }

    if (blackTiles.contains(tile)) blackTiles.remove(tile) else blackTiles.add(tile)
  }
  println(blackTiles.size)

  // Part 2
  repeat(100) {
    val whiteTilesToFlip = blackTiles
      .fold(listOf<HexVector>()) { tiles, tile -> tiles + getAdjacentTiles(tile) }
      .filter { !blackTiles.contains(it) }
      .filter { countAdjacentBlackTiles(blackTiles, it) == 2 }
    
    val blackTilesToFlip = blackTiles
      .filter { countAdjacentBlackTiles(blackTiles, it) !in listOf(1, 2) }

    blackTiles.removeAll(blackTilesToFlip)
    blackTiles.addAll(whiteTilesToFlip)
  }
  println(blackTiles.size)
}

main()
