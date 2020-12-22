package main

import (
  "fmt"
  "io/ioutil"
  "strconv"
  "strings"
)

func main() {
  cards1, cards2 := parseInput()

  var card1 int
  var card2 int
  for len(cards1) > 0 && len(cards2) > 0 {
    card1, cards1 = cards1[0], cards1[1:]
    card2, cards2 = cards2[0], cards2[1:]

    if (card1 > card2) {
      cards1 = append(cards1, card1, card2)
    } else {
      cards2 = append(cards2, card2, card1)
    }
  }

  winningCards := cards1
  if (len(cards2) > len(cards1)) {
    winningCards = cards2
  }

  var score, topCard int
  for len(winningCards) > 0 {
    topCard, winningCards = winningCards[0], winningCards[1:]
    score = score + ((len(winningCards) + 1) * topCard)
  }
  fmt.Println(score)
}

func parseInput() ([]int, []int) {
  b, _ := ioutil.ReadFile("input.txt")
  playerInputs := strings.Split(string(b), "\n\n")
  return parsePlayerInput(playerInputs[0]), parsePlayerInput(playerInputs[1])
}

func parsePlayerInput(input string) []int {
  playerLines := strings.Split(input, "\n")
  cards := []int{}
  for i := 1; i < len(playerLines); i++ {
    converted, _ := strconv.Atoi(playerLines[i])
    cards = append(cards, converted)
  }
  return cards
}
