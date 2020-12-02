findSum <- function (k) {
  lines <- readLines(file("input.txt", open = "r"))
  h <- new.env(hash = TRUE)

  for (i in 1:length(lines)) {
    current <- strtoi(lines[i])
    difference <- k - current
    if (!is.null(h[[as.character(difference)]])) {
      return(current * difference)
    }
    h[[as.character(current)]] <- i
  }
}

answer <- findSum(2020)
print(answer)
