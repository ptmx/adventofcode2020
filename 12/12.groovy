String[] lines = new File('input.txt').text.split('\n')

enum UpdateType {
  DIRECTION,
  POSITION
}

def solve(String[] lines, Integer initialDirX, Integer initialDirY, UpdateType updateType) {
  x = 0
  y = 0
  (dirX, dirY) = [initialDirX, initialDirY]
  lines.each {
    String direction = it.substring(0, 1)
    Integer value = Integer.parseInt(it.substring(1))

    if (direction == 'F') {
      x = x + (dirX * value)
      y = y + (dirY * value)
    } else if (direction == 'E') {
      if (updateType == UpdateType.DIRECTION) dirX = dirX + value
      if (updateType == UpdateType.POSITION) x = x + value
    } else if (direction == 'W') {
      if (updateType == UpdateType.DIRECTION) dirX = dirX - value
      if (updateType == UpdateType.POSITION) x = x - value
    } else if (direction == 'N') {
      if (updateType == UpdateType.DIRECTION) dirY = dirY + value
      if (updateType == UpdateType.POSITION) y = y + value
    } else if (direction == 'S') {
      if (updateType == UpdateType.DIRECTION) dirY = dirY - value
      if (updateType == UpdateType.POSITION) y = y - value
    } else {
      if (
        (direction == 'L' && value == 90) ||
        (direction == 'R' && value == 270)
      ) {
        (dirX, dirY) = [-dirY, dirX]
      } else if (
        (direction == 'L' && value == 270) ||
        (direction == 'R' && value == 90)
      ) {
        (dirX, dirY) = [dirY, -dirX]
      } else {
        (dirX, dirY) = [-dirX, -dirY]
      }
    }
  }

  return Math.abs(x) + Math.abs(y)
}

println solve(lines, 1, 0, UpdateType.POSITION)
println solve(lines, 10, 1, UpdateType.DIRECTION)
