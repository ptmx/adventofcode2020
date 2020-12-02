findSum <- function (k) {
  lines <- readLines(file("input.txt", open = "r"))
  h <- new.env(hash = TRUE)

  for (i in 1:length(lines)) {
    valuei <- strtoi(lines[i])
    for (j in i+1:length(lines)) {
      valuej <- strtoi(lines[j])
      difference <- k - valuei - valuej
      if (!is.null(h[[as.character(difference)]])) {
        return(valuei * valuej * difference)
      }
    }
    h[[as.character(valuei)]] <- i
  }
}

answer <- findSum(2020)
print(answer)
