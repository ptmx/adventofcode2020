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
  var directions = mutableListOf<String>();
  var pos = 0
  while (pos < line.length) {
    val direction = when (line[pos]) {
      'e'  -> "e"
      'w'  -> "w"
      'n'  -> "n" + line[pos + 1]
      's'  -> "s" + line[pos + 1]
      else -> throw IllegalArgumentException("Invalid direction")
    }
    directions.add(direction)
    pos += direction.length
  }
  return directions
}

fun main() {
  val blackTiles: MutableSet<HexVector> = HashSet()
  File("input.txt").forEachLine { 
    val tile = parseLineToDirections(it)
      .map { inputDirectionToVector(it) }
      .reduce { a, b -> a + b }

    if (blackTiles.contains(tile)) blackTiles.remove(tile) else blackTiles.add(tile)
  }
  println(blackTiles.size)
}

main()
