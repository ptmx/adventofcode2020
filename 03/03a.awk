{ 
  tile = substr($0, idx + 1, 1)
  idx = (idx + 3) % length($0)
  if (tile == "#") {
    trees++
  }
}

END {
  print trees
}
