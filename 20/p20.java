import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigInteger;
import java.util.LinkedList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

enum TileOrientation {
  INITIAL,
  ROTATE_90,
  ROTATE_180,
  ROTATE_270,
  INITIAL_FLIP,
  ROTATE_90_FLIP,
  ROTATE_180_FLIP,
  ROTATE_270_FLIP
}

enum TileEdge {
  TOP,
  RIGHT,
  BOTTOM,
  LEFT
}

class Coordinates {
  public int x;
  public int y;

  public Coordinates(int x, int y) {
    this.x = x;
    this.y = y;
  }
}

public class p20 {
  private static final Pattern TILE_ID_LINE = Pattern.compile("Tile (\\d+):");

  public static void main(String[] args) throws IOException {
    LinkedList<Tile> tiles = parseInput();
    Grid grid = new Grid(tiles);
    grid.arrangeTiles();

    // Part 1
    LinkedList<Tile> cornerTiles = grid.getCornerTiles();
    BigInteger cornerProduct = cornerTiles
      .stream()
      .map(tile -> BigInteger.valueOf(tile.getId()))
      .reduce(BigInteger.ONE, (product, tileId) -> product.multiply(tileId));
    System.out.println(cornerProduct);

    // Part 2
    grid.killTheSeaMonsters();
    System.out.println(grid.getWaterRoughness());
  }

  private static LinkedList<Tile> parseInput() throws IOException {
    LinkedList<Tile> tiles = new LinkedList<Tile>();
    BufferedReader reader = new BufferedReader(new FileReader(new File("input.txt")));

    String line;
    Integer tileId = null;
    char[][] image = null;
    int index = 0;
    while ((line = reader.readLine()) != null) {
      Matcher tileIdMatcher = TILE_ID_LINE.matcher(line);

      if (line.equals("")) {
        tiles.add(new Tile(tileId, image));
      } else if (tileIdMatcher.find()) {
        tileId = Integer.parseInt(tileIdMatcher.group(1));
        image = null;
        index = 0;
      } else {
        if (image == null) {
          int tileSize = line.length();
          image = new char[tileSize][tileSize];
        }
        image[index] = line.toCharArray();
        index++;
      }
    }

    reader.close();

    return tiles;
  }
}

class Grid {
  private static final char[][] SEA_MONSTER = {
    "                  # ".toCharArray(),
    "#    ##    ##    ###".toCharArray(),
    " #  #  #  #  #  #   ".toCharArray()
  };

  protected LinkedList<Tile> tiles;
  protected Tile[][] grid;

  public Grid(LinkedList<Tile> tiles) {
    int gridSize = (int) Math.sqrt(tiles.size());
    this.grid = new Tile[gridSize][gridSize];
    this.tiles = tiles;
  }

  public void arrangeTiles() {
    LinkedList<Tile> remainingTiles = new LinkedList<Tile>(this.tiles);
    LinkedList<Tile> cornerTiles = findCornerTiles();

    this.placeFirstTile(cornerTiles.getFirst(), remainingTiles);
    this.placeRemainingTiles(remainingTiles);
    this.removeTileBorders();
    this.orientGrid();
  }

  public LinkedList<Tile> getCornerTiles() {
    LinkedList<Tile> cornerTiles = new LinkedList<Tile>();
    cornerTiles.push(this.grid[0][0]);
    cornerTiles.push(this.grid[0][this.grid.length - 1]);
    cornerTiles.push(this.grid[this.grid.length - 1][0]);
    cornerTiles.push(this.grid[this.grid.length - 1][this.grid.length - 1]);
    return cornerTiles;
  }

  public void killTheSeaMonsters() {
    LinkedList<Coordinates> seaMonsterCoordinates = new LinkedList<Coordinates>();
    int totalGridSize = this.grid.length * this.grid[0][0].getSize();
    for (int i = 0; i < totalGridSize - SEA_MONSTER.length + 1; i++)
      for (int j = 0; j < totalGridSize - SEA_MONSTER[0].length + 1; j++)
        if (this.hasSeaMonster(i, j))
          seaMonsterCoordinates.add(new Coordinates(i, j));
    
    for (Coordinates coordinates : seaMonsterCoordinates) {
      this.killSeaMonsterAt(coordinates.x, coordinates.y);
    }
  }

  public int getWaterRoughness() {
    int totalGridSize = this.grid.length * this.grid[0][0].getSize();
    int occupied = 0;
    for (int i = 0; i < totalGridSize; i++)
      for (int j = 0; j < totalGridSize; j++)
        if (this.getChar(i, j) == '#')
          occupied++;
    
    return occupied;
  }

  private void orientGrid() {
    for (int flips = 0; flips < 2; flips++) {
      for (int rotations = 0; rotations < 4; rotations++) {
        if (countSeaMonsters() > 0) {
          return;
        }
        this.rotateClockwise();
      }
      this.flip();
    }
  }

  private int countSeaMonsters() {
    int totalGridSize = this.grid.length * this.grid[0][0].getSize();
    int count = 0;
    for (int i = 0; i < totalGridSize - SEA_MONSTER.length + 1; i++)
      for (int j = 0; j < totalGridSize - SEA_MONSTER[0].length + 1; j++)
        if (this.hasSeaMonster(i, j)) count++;

    return count;
  }

  private LinkedList<Tile> findCornerTiles() {
    LinkedList<Tile> cornerTiles = new LinkedList<Tile>();
    for (Tile t1 : this.tiles) {
      LinkedList<Tile> t1MatchingTiles = new LinkedList<Tile>();
      for (Tile t2 : this.tiles) {
        if (t1 == t2) continue;

        if (t1.matches(t2)) {
          t1MatchingTiles.add(t2);
        }
      }

      if (t1MatchingTiles.size() == 2) {
        cornerTiles.push(t1);
      }
    }

    return cornerTiles;
  }

  private void placeFirstTile(Tile firstTile, LinkedList<Tile> remainingTiles) {
    this.grid[0][0] = firstTile;
    remainingTiles.remove(firstTile);

    for (int flips = 0; flips < 2; flips++) {
      for (int rotations = 0; rotations < 4; rotations++) {
        if (
          firstTile.matchesAny(TileEdge.RIGHT, remainingTiles) &&
          firstTile.matchesAny(TileEdge.BOTTOM, remainingTiles)
        ) {
          return;
        }
        firstTile.rotateClockwise();
      }
      firstTile.flip();
    }
  }

  private void placeRemainingTiles(LinkedList<Tile> remainingTiles) {
    for (int i = 0; i < this.grid.length; i++) {
      for (int j = 0; j < this.grid.length; j++) {
        if (i == 0 && j == 0) continue;

        Tile nextTile = null;
        for (Tile tile : remainingTiles) {
          if (this.tileFits(tile, i, j)) {
            nextTile = tile;
            break;
          }
        }

        this.grid[i][j] = nextTile;
        remainingTiles.remove(nextTile);
      }
    }
  }

  private boolean tileFits(Tile tile, int x, int y) {
    for (int flips = 0; flips < 2; flips++) {
      for (int rotations = 0; rotations < 4; rotations++) {
        if (
          (x == 0 || Tile.edgesMatch(this.grid[x - 1][y], TileEdge.BOTTOM, tile, TileEdge.TOP)) &&
          (y == 0 || Tile.edgesMatch(this.grid[x][y - 1], TileEdge.RIGHT, tile, TileEdge.LEFT))
        ) {
          return true;
        }

        tile.rotateClockwise();
      }
      tile.flip();
    }

    return false;
  }

  private void removeTileBorders() {
    for (int i = 0; i < this.grid.length; i++)
      for (int j = 0; j < this.grid.length; j++)
        this.grid[i][j].removeBorders();
  }

  private boolean hasSeaMonster(int x, int y) {
    char[][] searchArea = this.getChars(x, y, SEA_MONSTER.length, SEA_MONSTER[0].length);

    for (int i = 0; i < SEA_MONSTER.length; i++)
      for (int j = 0; j < SEA_MONSTER[0].length; j++)
        if (SEA_MONSTER[i][j] == '#' && searchArea[i][j] != '#')
          return false;
    
    return true;
  }

  private char[][] getChars(int x, int y, int height, int width) {
    char[][] chars = new char[height][width];
    for (int i = 0; i < height; i++)
      for (int j = 0; j < width; j++)
        chars[i][j] = this.getChar(x + i, y + j);

    return chars;
  }

  private char getChar(int x, int y) {
    int tileSize = this.grid[0][0].getSize();
    int gridX = Math.floorDiv(x, tileSize);
    int gridY = Math.floorDiv(y, tileSize);
    int tileX = x % tileSize;
    int tileY = y % tileSize;
    return this.grid[gridX][gridY].getChar(tileX, tileY);
  }

  private void setChar(int x, int y, char c) {
    int tileSize = this.grid[0][0].getSize();
    int gridX = Math.floorDiv(x, tileSize);
    int gridY = Math.floorDiv(y, tileSize);
    int tileX = x % tileSize;
    int tileY = y % tileSize;
    this.grid[gridX][gridY].setChar(tileX, tileY, c);
  }

  private void rotateClockwise() {
    int gridSize = this.grid.length;
    Tile[][] newGrid = new Tile[gridSize][gridSize];
    
    for (int i = 0; i < gridSize; i++) {
      for (int j = 0; j < gridSize; j++) {
        newGrid[i][j] = this.grid[gridSize - j - 1][i];
        newGrid[i][j].rotateClockwise();
      }
    }
   
    this.grid = newGrid;
  }

  private void flip() {
    int gridSize = this.grid.length;
    Tile[][] newGrid = new Tile[gridSize][gridSize];

    for (int i = 0; i < gridSize; i++) {
      for (int j = 0; j < gridSize; j++) {
        newGrid[i][j] = this.grid[this.grid.length - i - 1][j];
        newGrid[i][j].flip();
      }
    }

    this.grid = newGrid;
  }

  private void killSeaMonsterAt(int x, int y) {
    for (int i = 0; i < SEA_MONSTER.length; i++)
      for (int j = 0; j < SEA_MONSTER[0].length; j++)
        if (SEA_MONSTER[i][j] == '#')
          this.setChar(x + i, y + j, '.');
  }

  public String toString() {
    int gridSize = this.grid.length;
    int tileSize = this.grid[0][0].getSize();

    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < gridSize * tileSize; i++) {
      for (int j = 0; j < gridSize * tileSize; j++) {
        buffer.append(this.getChar(i, j));
      }
      buffer.append(String.format("%n"));
    }
    return buffer.toString();
  }
}

class Tile {
  protected int id;
  protected char[][] image;
  protected TileOrientation orientation = TileOrientation.INITIAL;

  public Tile(int id, char[][] image) {
    this.id = id;
    this.image = image;
  }

  public int getSize() {
    return this.image.length;
  }

  public int getId() {
    return this.id;
  }

  public char getChar(int x, int y) {
    return this.image[x][y];
  }

  public void setChar(int x, int y, char c) {
    this.image[x][y] = c;
  }

  public void rotateClockwise() {
    char[][] newImage = new char[this.image.length][this.image.length];
    
    for (int i = 0; i < this.image.length; i++) 
      for (int j = 0; j < this.image.length; j++) 
        newImage[i][j] = this.image[this.image.length - j - 1][i];

    this.image = newImage;
    if (this.orientation == TileOrientation.INITIAL) {
      this.orientation = TileOrientation.ROTATE_90;
    } else if (this.orientation == TileOrientation.ROTATE_90) {
      this.orientation = TileOrientation.ROTATE_180;
    } else if (this.orientation == TileOrientation.ROTATE_180) {
      this.orientation = TileOrientation.ROTATE_270;
    } else if (this.orientation == TileOrientation.ROTATE_270) {
      this.orientation = TileOrientation.INITIAL;
    } else if (this.orientation == TileOrientation.INITIAL_FLIP) {
      this.orientation = TileOrientation.ROTATE_270_FLIP;
    } else if (this.orientation == TileOrientation.ROTATE_90_FLIP) {
      this.orientation = TileOrientation.INITIAL_FLIP;
    } else if (this.orientation == TileOrientation.ROTATE_180_FLIP) {
      this.orientation = TileOrientation.ROTATE_90_FLIP;
    } else if (this.orientation == TileOrientation.ROTATE_270_FLIP) {
      this.orientation = TileOrientation.ROTATE_180_FLIP;
    }
  }

  public void flip() {
    char[][] newImage = new char[this.image.length][this.image.length];

    for (int i = 0; i < this.image.length; i++)
      for (int j = 0; j < this.image.length; j++)
        newImage[i][j] = this.image[this.image.length - i - 1][j];

    this.image = newImage;
    if (this.orientation == TileOrientation.INITIAL) {
      this.orientation = TileOrientation.INITIAL_FLIP;
    } else if (this.orientation == TileOrientation.ROTATE_90) {
      this.orientation = TileOrientation.ROTATE_90_FLIP;
    } else if (this.orientation == TileOrientation.ROTATE_180) {
      this.orientation = TileOrientation.ROTATE_180_FLIP;
    } else if (this.orientation == TileOrientation.ROTATE_270) {
      this.orientation = TileOrientation.ROTATE_270_FLIP;
    } else if (this.orientation == TileOrientation.INITIAL_FLIP) {
      this.orientation = TileOrientation.INITIAL;
    } else if (this.orientation == TileOrientation.ROTATE_90_FLIP) {
      this.orientation = TileOrientation.ROTATE_90;
    } else if (this.orientation == TileOrientation.ROTATE_180_FLIP) {
      this.orientation = TileOrientation.ROTATE_180;
    } else if (this.orientation == TileOrientation.ROTATE_270_FLIP) {
      this.orientation = TileOrientation.ROTATE_270;
    }
  }

  public boolean matchesAny(TileEdge edge, LinkedList<Tile> tiles) {
    char[] border = this.getBorder(edge);
    for (Tile tile : tiles)
      for (char[] otherBorder : tile.getBorders())
        if (bordersMatch(border, otherBorder))
          return true;
    
    return false;
  }

  public boolean matches(Tile tile) {
    LinkedList<char[]> borders = this.getBorders();
    LinkedList<char[]> tileBorders = tile.getBorders();
    for (char[] border : borders)
      for (char[] tileBorder : tileBorders)
        if (bordersMatch(border, tileBorder)) return true;
      
    return false;
  }

  public LinkedList<char[]> getBorders() {
    LinkedList<char[]> borders = new LinkedList<char[]>();
    borders.add(this.getBorder(TileEdge.TOP));
    borders.add(this.getBorder(TileEdge.RIGHT));
    borders.add(this.getBorder(TileEdge.BOTTOM));
    borders.add(this.getBorder(TileEdge.LEFT));
    this.flip();
    borders.add(this.getBorder(TileEdge.TOP));
    borders.add(this.getBorder(TileEdge.RIGHT));
    borders.add(this.getBorder(TileEdge.BOTTOM));
    borders.add(this.getBorder(TileEdge.LEFT));
    this.flip();
    return borders;
  }

  public char[] getBorder(TileEdge edge) {
    char[] border = new char[this.image.length];
    switch (edge) {
      case TOP:
        for (int i = 0; i < this.image.length; i++) {
          border[i] = this.image[0][i];
        }
        break;
      case RIGHT:
        for (int i = 0; i < this.image.length; i++) {
          border[i] = this.image[i][this.image.length - 1];
        }
        break;
      case BOTTOM:
        for (int i = 0; i < this.image.length; i++) {
          border[i] = this.image[this.image.length - 1][this.image.length - i - 1];
        }
        break;
      case LEFT:
        for (int i = 0; i < this.image.length; i++) {
          border[i] = this.image[this.image.length - i - 1][0];
        }
        break;
    }
    return border;
  }

  public void removeBorders() {
    int newSize = this.image.length - 2;
    char[][] newImage = new char[newSize][newSize];
    for (int i = 0; i < newSize; i++) {
      for (int j = 0; j < newSize; j++) {
        newImage[i][j] = this.image[i + 1][j + 1];
      }
    }
    this.image = newImage;
  }

  public String toString() {
    StringBuffer buffer = new StringBuffer(String.format("Tile %d%n", this.id));
    for (char[] row : this.image) {
      buffer.append(String.format("%s%n", new String(row)));
    }
    return buffer.toString();
  }

  public static boolean edgesMatch(Tile tile1, TileEdge edge1, Tile tile2, TileEdge edge2) {
    return bordersMatch(tile1.getBorder(edge1), tile2.getBorder(edge2));
  }

  public static boolean bordersMatch(char[] border1, char[] border2) {
    for (int i = 0; i < border1.length; i++)
      if (border1[i] != border2[border2.length - i - 1])
        return false;

    return true;
  }
}
