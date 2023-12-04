package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	partOne("input.txt")
	partTwo("input.txt")
}

// Contains returns true if the string is in the slice.
// Note: If you .Sort() the slice first, this function will do a log2(n) binary search through the list, which is much faster for large lists.
func contains[T comparable](ss []T, s T) bool {
	return index(ss, s) != -1
}

// Index returns the index of string in the slice, otherwise -1 if the string is not found.
// Note: .SortedIndex() will do a log2(n) binary search through the list, which is much faster for large lists.
func index[T comparable](ss []T, s T) int {
	for i, b := range ss {
		if b == s {
			return i
		}
	}
	return -1
}

func deleteIf[T any](ss []T, funcInterface interface{}) []T {
	f := func(i int, s T) bool {
		switch tf := funcInterface.(type) {
		case func(int, T) bool:
			return tf(i, s)
		case func(T) bool:
			return tf(s)
		default:
			panic(fmt.Sprintf("Filter cannot understand function type %T", funcInterface))
		}
	}

	result := []T{}

	for i, s := range ss {
		if !f(i, s) {
			result = append(result, s)
		}
	}
	return result
}

func partOne(fileName string) {
	fmt.Println("****************** PART ONE ********************")

	file, err := os.Open(fileName)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	/*
		Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
		Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
		Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
		Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
		Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
		Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
	*/
	totalPoints := 0
	for scanner.Scan() {
		/***** Each line *****/
		line := strings.Split(scanner.Text(), ":")
		data := strings.Split(line[1], "|")
		winning := strings.Split(data[0], " ")
		numbers := strings.Split(data[1], " ")

		winning = deleteIf(winning, func(x string) bool {
			return x == ""
		})
		numbers = deleteIf(numbers, func(x string) bool {
			return x == ""
		})

		gamePoints := 0
		for _, number := range numbers {
			if contains(winning, number) {
				if gamePoints == 0 {
					gamePoints = 1
				} else {
					gamePoints *= 2
				}
			}
		}
		totalPoints += gamePoints
	}

	fmt.Println(totalPoints)

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}

func partTwo(fileName string) {
	fmt.Println("****************** PART TWO ********************")

	file, err := os.Open(fileName)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	/*
		Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
		Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
		Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
		Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
		Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
		Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
	*/
	var nrOfCards [255]int
	for i := 0; i < len(nrOfCards); i++ {
		nrOfCards[i] = 1
	}
	totalCards := 0
	for scanner.Scan() {
		/***** Each line *****/
		line := scanner.Text()
		lineSplit := strings.Split(line, ":")
		cardIdSlice := strings.Split(lineSplit[0], " ")
		cardId, _ := strconv.Atoi(cardIdSlice[len(cardIdSlice)-1])

		data := strings.Split(lineSplit[1], "|")
		winning := strings.Split(data[0], " ")
		numbers := strings.Split(data[1], " ")

		winning = deleteIf(winning, func(x string) bool {
			return x == ""
		})
		numbers = deleteIf(numbers, func(x string) bool {
			return x == ""
		})

		for i := 0; i < nrOfCards[cardId]; i++ {
			totalCards++
			nrOfMatches := 0
			for _, number := range numbers {
				if contains(winning, number) {
					nrOfMatches++
					nrOfCards[cardId+nrOfMatches]++
				}
			}
		}
	}

	fmt.Println(totalCards)

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
