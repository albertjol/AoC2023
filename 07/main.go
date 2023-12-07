package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

var _part int

func main() {
	fmt.Println("****************** PART ONE ********************")
	_part = 1
	calc("input.txt")
	fmt.Println("****************** PART TWO ********************")
	_part = 2
	calc("input.txt")
}

type hand struct {
	cards string
	bid   int
}

func card2Rank(card rune) int {
	switch card {
	case 'A':
		return 1
	case 'K':
		return 2
	case 'Q':
		return 3
	case 'J':
		if _part == 1 {
			return 4
		} else {
			return 14
		}
	case 'T':
		return 5
	case '9':
		return 6
	case '8':
		return 7
	case '7':
		return 8
	case '6':
		return 9
	case '5':
		return 10
	case '4':
		return 11
	case '3':
		return 12
	case '2':
		return 13
	default:
		log.Fatalln("Card not found")
		return -1
	}
}

/*
1. - 1 Five of a kind, where all five cards have the same label: AAAAA
2. - 2 Four of a kind, where four cards have the same label and one card has a different label: AA8AA
3. - 2 Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
4. - 3 Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
5. - 3 Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
6. - 4 One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
7. - 5 High card, where all cards' labels are distinct: 23456
*/
func determineType(cards string) int {
	// For now super dumb..., I don't know anything better at this moment.....
	// Distinct cards...
	cardMap := make(map[rune]int)

	if _part == 1 {
		for _, card := range cards {
			cardMap[card]++
		}
	} else { // Part 2

		jokers := 0
		for _, card := range cards {
			if card == 'J' {
				jokers++
			} else {
				cardMap[card]++
			}
		}

		maxCardValue := 0
		var maxCard rune
		for card := range cardMap {
			if cardMap[card] > maxCardValue {
				maxCard = card
				maxCardValue = cardMap[card]
			}
		}
		cardMap[maxCard] += jokers
	}

	switch len(cardMap) {
	case 1:
		return 1
	case 2:
		for card := range cardMap {
			// Just a way to get the first element
			if cardMap[card] == 1 || cardMap[card] == 4 {
				return 2
			} else {
				return 3
			}
		}
	case 3: // ToaK or 2P
		for card := range cardMap {
			if cardMap[card] == 3 {
				return 4
			}
		}
		return 5
	case 4:
		return 6
	case 5:
		return 7
	}
	log.Fatalln("Impossible hand length")
	return -1
}

func getChar(str string, index int) rune {
	return []rune(str)[index]
}

/*********** Sort Interface ****************/
type hands []hand

func (a hands) Less(i, j int) bool {
	typeI, typeJ := determineType(a[i].cards), determineType(a[j].cards)
	if typeI > typeJ {
		return true
	} else if typeI < typeJ {
		return false
	} else {
		for k := 0; k < 5; k++ {
			if card2Rank(getChar(a[i].cards, k)) > card2Rank(getChar(a[j].cards, k)) {
				return true
			} else if card2Rank(getChar(a[i].cards, k)) < card2Rank(getChar(a[j].cards, k)) {
				return false
			}
		}
		log.Fatalln("Identical rank... impossible?....")
		return false
	}
}

func (a hands) Swap(i, j int) {
	a[i], a[j] = a[j], a[i]
}

func (a hands) Len() int {
	return len(a)
}

/********************************************/

func calc(fileName string) {
	file, err := os.Open(fileName)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	/*
		32T3K 765
		T55J5 684
		KK677 28
		KTJJT 220
		QQQJA 483
	*/

	var myHands hands
	for scanner.Scan() {
		strings := strings.Split(scanner.Text(), " ")
		bid, _ := strconv.Atoi(strings[1])
		myHands = append(myHands, hand{strings[0], bid})
	}

	sort.Sort(myHands)

	winnings := 0
	for rank, hand := range myHands {
		winnings += hand.bid * (rank + 1)
	}

	fmt.Println(winnings)

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
