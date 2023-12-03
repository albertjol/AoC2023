package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"unicode"
)

func main() {
	partOne("input.txt")
	partTwo("input.txt")
}

func max(a, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

func abs(value int) int {
	if value < 0 {
		return -value
	} else {
		return value
	}
}

type point struct {
	x int
	y int
}

type number struct {
	number        string
	startPosition point
}

func isSymbol(char rune) bool {
	return !isDigit(char) && !isDot(char)
}

func isDigit(char rune) bool {
	return unicode.IsDigit(char)
}

func isDot(char rune) bool {
	return char == '.'
}

func inRange(num *number, symbol point) bool {
	if abs(num.startPosition.y-symbol.y) <= 1 {
		for x := num.startPosition.x; x < num.startPosition.x+len(num.number); x++ {
			if abs(x-symbol.x) <= 1 {
				return true
			}
		}
	}
	return false
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
		467..114..
		...*......
		..35..633.
		......#...
		617*......
		.....+.58.
		..592.....
		......755.
		...$.*....
		.664.598..
	*/

	var symbolPositions []point
	var numbers []*number
	for y := 0; scanner.Scan(); y++ {
		/***** Each line *****/
		curNumber := new(number)
		line := scanner.Text()
		for x, char := range line {

			if isDot(char) {
				if curNumber.number != "" {
					// Number is over; store it
					numbers = append(numbers, curNumber)
					curNumber = new(number)
				}
			} else if isSymbol(char) {
				if curNumber.number != "" {
					// Number is over; store it
					numbers = append(numbers, curNumber)
					curNumber = new(number)
				}
				symbolPositions = append(symbolPositions, point{x, y})
			} else if isDigit(char) {
				if (curNumber.number) == "" {
					curNumber.startPosition.x = x
					curNumber.startPosition.y = y
				}
				curNumber.number = curNumber.number + string(char)
				if x == len(line)-1 { // laatste karakter van regel
					// Number is over; store it
					numbers = append(numbers, curNumber)
					curNumber = new(number)
				}
			} else {
				log.Fatal("Unknown symbol")
			}
		}
	}

	// Alles is bekend, nu nog kijken wat een partnumber is
	partNrSum := 0
	for _, number := range numbers {
		for _, symbol := range symbolPositions {
			if inRange(number, symbol) {
				partNr, _ := strconv.Atoi(number.number)
				partNrSum += partNr
			}
		}
	}

	fmt.Println(partNrSum)

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
		467..114..
		...*......
		..35..633.
		......#...
		617*......
		.....+.58.
		..592.....
		......755.
		...$.*....
		.664.598..
	*/

	var symbolPositions []point
	var numbers []*number
	for y := 0; scanner.Scan(); y++ {
		/***** Each line *****/
		curNumber := new(number)
		line := scanner.Text()
		for x, char := range line {

			if isDot(char) {
				if curNumber.number != "" {
					// Number is over; store it
					numbers = append(numbers, curNumber)
					curNumber = new(number)
				}
			} else if isSymbol(char) {
				if curNumber.number != "" {
					// Number is over; store it
					numbers = append(numbers, curNumber)
					curNumber = new(number)
				}
				// alleen asterisks...
				if char == '*' {
					symbolPositions = append(symbolPositions, point{x, y})
				}
			} else if isDigit(char) {
				if (curNumber.number) == "" {
					curNumber.startPosition.x = x
					curNumber.startPosition.y = y
				}
				curNumber.number = curNumber.number + string(char)
				if x == len(line)-1 { // laatste karakter van regel
					// Number is over; store it
					numbers = append(numbers, curNumber)
					curNumber = new(number)
				}
			} else {
				log.Fatal("Unknown symbol")
			}
		}
	}

	// Alles is bekend, nu nog kijken wat een tandwiel is die 2 part numbers heeft
	ratioSum := 0
	for _, symbol := range symbolPositions {
		numsFound := 0
		ratio := 1
		for _, number := range numbers {
			if inRange(number, symbol) {
				partNr, _ := strconv.Atoi(number.number)
				ratio *= partNr
				numsFound++
				if numsFound == 2 {
					ratioSum += ratio
				} else if numsFound > 2 {
					fmt.Println("Euhhhh....")
				}
			}
		}
	}

	fmt.Println(ratioSum)

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
