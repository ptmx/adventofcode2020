import std.algorithm, std.conv, std.range, std.stdio, std.typecons;

enum CycleCount = 6;

class Point {
  int x;
  int y;
  int z;
  int w;

  this(int x, int y, int z, int w) {
    this.x = x;
    this.y = y;
    this.z = z;
    this.w = w;
  }

  override bool opEquals(Object o) {
    auto rhs = to!(const Point)(o);
    return this.x == rhs.x && this.y == rhs.y && this.z == rhs.z && this.w == rhs.w;
  }

  string toKey() {
    return [this.x, this.y, this.z, this.w]
      .map!(coord => to!string(coord))
      .join(",");
  }

  static Point fromKey(string key) {
    string[] splitKey = key.split(",");
    string xKey = splitKey[0];
    string yKey = splitKey[1];
    string zKey = splitKey[2];
    string wKey = splitKey[3];
    return new Point(to!int(xKey), to!int(yKey), to!int(zKey), to!int(wKey));
  }
}

void main()
{
  bool[string] grid = initializeGrid();
  foreach(_; 0..CycleCount) runCycle(grid);
  writeln(grid.length);
}

bool[string] initializeGrid() {
  bool[string] grid;
  foreach (xIndex, line; File("input.txt").byLine().enumerate(0)) {
    foreach (yIndex, character; line.enumerate(0)) {
      if (character == '#') {
        string key = new Point(xIndex, yIndex, 0, 0).toKey();
        grid[key] = true;
      }
    }
  }
  return grid;
}

void runCycle(bool[string] grid) {
  Point[] activeOrAdjacentPoints = getActiveOrAdjacentPoints(grid);
  Point[] pointsToFlip = getPointsToFlip(grid, activeOrAdjacentPoints);
  flipPoints(grid, pointsToFlip);
}

Point[] getActiveOrAdjacentPoints(bool[string] grid) {
  Point[] allPoints = [];
  Point[] activePoints = grid.keys().map!(key => Point.fromKey(key)).array;
  allPoints ~= activePoints;
  foreach (point; activePoints) {
    allPoints ~= getAdjacentPoints(point);
  }
  multiSort!("a.x < b.x", "a.y < b.y", "a.z < b.z", "a.w < b.w", SwapStrategy.unstable)(allPoints);
  return uniq(allPoints).array;
}

Point[] getAdjacentPoints(Point point) {
  Point[] adjacentPoints;
  const int x = point.x;
  const int y = point.y;
  const int z = point.z;
  const int w = point.w;
  foreach (xi; x-1..x+2) {
    foreach (yi; y-1..y+2) {
      foreach (zi; z-1..z+2) {
        foreach (wi; w-1..w+2) {
          if (xi != x || yi != y || zi != z || wi != w) {
            adjacentPoints ~= new Point(xi, yi, zi, wi);
          }
        }
      }
    }
  }
  return adjacentPoints;
}

Point[] getPointsToFlip(bool[string] grid, Point[] points) {
  return points.filter!((point) {
    const bool active = isActive(grid, point);
    const int adjacentCount = countActiveAdjacentPoints(grid, point);
    return active ? (adjacentCount != 2 && adjacentCount != 3) : (adjacentCount == 3);
  }).array;
}

int countActiveAdjacentPoints(bool[string] grid, Point point) {
  auto adjacentPoints = getAdjacentPoints(point);
  return sum(adjacentPoints.map!(point => (point.toKey() in grid) ? 1 : 0));
}

void flipPoints(bool[string] grid, Point[] points) {
  foreach (point; points) {
    if (isActive(grid, point)) {
      grid.remove(point.toKey());
    } else {
      grid[point.toKey()] = true;
    }
  }
}

bool isActive(bool[string] grid, Point point) {
  return !!(point.toKey() in grid);
}
