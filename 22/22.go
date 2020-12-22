package main

import (
  "fmt"
  "io/ioutil"
  "strconv"
  "strings"
)

type Player int

const (
  PlayerOne Player = iota
  PlayerTwo
)

func main() {
  player1Cards, player2Cards := parseInput()
  playPart1(player1Cards, player2Cards)
  playPart2(player1Cards, player2Cards)
}

func playPart1(player1Cards, player2Cards []int) {
  _, winningCards := playPart1Game(player1Cards, player2Cards)
  fmt.Println(getWinningScore(winningCards))
}

func playPart2(player1Cards, player2Cards []int) {
  _, winningCards := playPart2Game(player1Cards, player2Cards)
  fmt.Println(getWinningScore(winningCards))
}

func playPart1Game(player1Cards, player2Cards []int) (Player, []int) {
  cards1 := make([]int, len(player1Cards))
  cards2 := make([]int, len(player2Cards))
  copy(cards1, player1Cards)
  copy(cards2, player2Cards)

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

  if (len(cards2) > len(cards1)) {
    return PlayerTwo, cards2
  }
  return PlayerOne, cards1
}

func playPart2Game(player1Cards, player2Cards []int) (Player, []int) {
  cards1 := make([]int, len(player1Cards))
  cards2 := make([]int, len(player2Cards))
  copy(cards1, player1Cards)
  copy(cards2, player2Cards)

  seen := map[string]bool{}
  var card1 int
  var card2 int
  for len(cards1) > 0 && len(cards2) > 0 {
    serializedDecks := serializeDecks(cards1, cards2)
    if seen[serializedDecks] {
      return PlayerOne, cards1
    }
    seen[serializedDecks] = true

    card1, cards1 = cards1[0], cards1[1:]
    card2, cards2 = cards2[0], cards2[1:]

    if len(cards1) >= card1 && len(cards2) >= card2 {
      subgameWinner, _ := playPart2Game(cards1[:card1], cards2[:card2])
      if subgameWinner == PlayerOne {
        cards1 = append(cards1, card1, card2)
      } else {
        cards2 = append(cards2, card2, card1)
      }
    } else if card1 > card2 {
      cards1 = append(cards1, card1, card2)
    } else {
      cards2 = append(cards2, card2, card1)
    }
  }
  
  if (len(cards2) > len(cards1)) {
    return PlayerTwo, cards2
  }
  return PlayerOne, cards1
}

func serializeDecks(player1Cards, player2Cards []int) string {
  serializedDeck1 := serializeDeck(player1Cards)
  serializedDeck2 := serializeDeck(player2Cards)
  return fmt.Sprintf("%s|%s", serializedDeck1, serializedDeck2)
}

func serializeDeck(cards []int) string {
  serializedCards := []string{}
  for i := 0; i < len(cards); i++ {
    serializedCard := strconv.Itoa(cards[i])
    serializedCards = append(serializedCards, serializedCard)
  }
  return strings.Join(serializedCards, ",")
}

func getWinningScore(cards []int) int {
  var score, topCard int
  for len(cards) > 0 {
    topCard, cards = cards[0], cards[1:]
    score = score + ((len(cards) + 1) * topCard)
  }
  return score
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
