{ 
  tile1 = substr($0, idx1 + 1, 1)
  idx1 = (idx1 + 1) % length($0)
  if (tile1 == "#") {
    trees1++
  }
}

{ 
  tile2 = substr($0, idx2 + 1, 1)
  idx2 = (idx2 + 3) % length($0)
  if (tile2 == "#") {
    trees2++
  }
}

{ 
  tile3 = substr($0, idx3 + 1, 1)
  idx3 = (idx3 + 5) % length($0)
  if (tile3 == "#") {
    trees3++
  }
}

{ 
  tile4 = substr($0, idx4 + 1, 1)
  idx4 = (idx4 + 7) % length($0)
  if (tile4 == "#") {
    trees4++
  }
}

NR % 2 == 1 {
  tile5 = substr($0, idx5 + 1, 1)
  idx5 = (idx5 + 1) % length($0)
  if (tile5 == "#") {
    trees5++
  }
}

END {
  print trees1 * trees2 * trees3 * trees4 * trees5
}
