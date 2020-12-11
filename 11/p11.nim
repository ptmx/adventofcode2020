import sequtils
import strutils

const DIRECTIONS = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

proc gridHeight(grid: seq[string]): int = grid.len()
proc gridWidth(grid: seq[string]): int = grid[0].len()

proc get(grid: seq[string], x: int, y: int): char =
  return grid[x][y] 

proc set(grid: var seq[string], x: int, y: int, value: char) =
  grid[x][y] = value

proc isInBounds(grid: seq[string], x: int, y: int): bool =
  let height = gridHeight(grid)
  let width = gridWidth(grid)

  return x >= 0 and y >= 0 and x < height and y < width

proc countAdjacentOccupiedSeats(grid: seq[string], x: int, y: int, maxSearchSteps: int): int =
  var occupied = 0
  for (deltaX, deltaY) in DIRECTIONS:
    var checkX = x + deltaX
    var checkY = y + deltaY
    var seat = '.'
    var steps = 0

    while isInBounds(grid, checkX, checkY) and seat == '.' and steps < maxSearchSteps:
      seat = get(grid, checkX, checkY)
      steps += 1
      checkX += deltaX
      checkY += deltaY
    
    if seat == '#':
      occupied += 1

  return occupied

proc getSeatsToFlip(grid: seq[string], minAdjacentSeats: int, maxSearchSteps: int): seq[(int, int)] =
  let height = gridHeight(grid)
  let width = gridWidth(grid)

  var seatsToFlip: seq[(int, int)] = @[]

  for x in countup(0, height - 1):
    for y in countup(0, width - 1):
      let seat = get(grid, x, y)
      if seat == '.':
        continue
      
      let adjacentOccupied = countAdjacentOccupiedSeats(grid, x, y, maxSearchSteps)
      if (seat == '#' and adjacentOccupied >= minAdjacentSeats) or (seat == 'L' and adjacentOccupied == 0):
        seatsToFlip.add((x, y))
  
  return seatsToFlip

proc flipSeats(grid: var seq[string], seatsToFlip: seq[(int,int)]) =
  for (x,y) in seatsToFlip:
    let seat = get(grid, x, y)
    let newSeat = (if seat == 'L': '#' else: 'L')
    set(grid, x, y, newSeat)

proc isOccupied(grid: seq[string], x: int, y: int): bool =
  return get(grid, x, y) == '#'

proc countOccupied(grid: var seq[string]): int =
  let height = gridHeight(grid)
  let width = gridWidth(grid)

  var occupied = 0
  for x in countup(0, height - 1):
    for y in countup(0, width - 1):
      if isOccupied(grid, x, y):
        occupied += 1
  
  return occupied

proc solve(grid: var seq[string], minAdjacentSeats: int, maxSearchSteps: int): int =
  var flipped = -1
  while flipped != 0:
    let seatsToFlip = getSeatsToFlip(grid, minAdjacentSeats, maxSearchSteps)
    flipSeats(grid, seatsToFlip)
    flipped = seatsToFlip.len()

  return countOccupied(grid)

proc solveA(grid: seq[string]): int =
  var gridCopy: seq[string]
  deepCopy(gridCopy, grid)
  return solve(gridCopy, 4, 1)

proc solveB(grid: seq[string]): int =
  var gridCopy: seq[string]
  deepCopy(gridCopy, grid)
  return solve(gridCopy, 5, high(int))

let file = readFile("input.txt")
let grid = splitLines(file).filter do (s: string) -> bool: s != ""

echo solveA(grid)
echo solveB(grid)
