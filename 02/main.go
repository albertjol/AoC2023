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

func max(a, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

func partOne(fileName string) {
	fmt.Println("****************** PART ONE ********************")
	file, err := os.Open(fileName)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	const MAX_RED = 12
	const MAX_GREEN = 13
	const MAX_BLUE = 14

	/*
		Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
		Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
		Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
		Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
		Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
	*/
	gameIdSum := 0
	for scanner.Scan() {
		/***** Each Game *****/
		game := scanner.Text()
		keyValue := strings.Split(game, ":")
		gameIdStr, found := strings.CutPrefix(keyValue[0], "Game ")
		if !found {
			log.Fatalln("No Game found")
		}

		gameId, _ := strconv.Atoi(gameIdStr)
		rounds := strings.Split(keyValue[1], ";")
		maxRed := 0
		maxGreen := 0
		maxBlue := 0
		for _, round := range rounds {
			/***** Each round in a Game *****/
			valueColors := strings.Split(round, ",")
			for _, valueColor := range valueColors {
				/***** Each color *****/
				valueColor = strings.TrimSpace(valueColor)
				valueColorSplit := strings.Split(valueColor, " ")
				numberOfBalls, _ := strconv.Atoi(valueColorSplit[0])
				switch valueColorSplit[1] {
				case "red":
					maxRed = max(maxRed, numberOfBalls)
				case "green":
					maxGreen = max(maxGreen, numberOfBalls)
				case "blue":
					maxBlue = max(maxBlue, numberOfBalls)
				default:
					log.Fatalln("Unknown color found")
				}
				if (maxRed > MAX_RED) ||
					(maxGreen > MAX_GREEN) ||
					(maxBlue > MAX_BLUE) {

					goto gameOver
				}
			}
		}

		gameIdSum += gameId

	gameOver:
	}

	fmt.Println(gameIdSum)

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
		Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
		Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
		Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
		Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
		Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
	*/
	powerSum := 0
	for scanner.Scan() {
		/***** Each Game *****/
		game := scanner.Text()
		keyValue := strings.Split(game, ":")

		rounds := strings.Split(keyValue[1], ";")
		maxRed := 0
		maxGreen := 0
		maxBlue := 0
		for _, round := range rounds {
			/***** Each round in a Game *****/
			valueColors := strings.Split(round, ",")
			for _, valueColor := range valueColors {
				/***** Each color *****/
				valueColor = strings.TrimSpace(valueColor)
				valueColorSplit := strings.Split(valueColor, " ")
				numberOfBalls, _ := strconv.Atoi(valueColorSplit[0])
				switch valueColorSplit[1] {
				case "red":
					maxRed = max(maxRed, numberOfBalls)
				case "green":
					maxGreen = max(maxGreen, numberOfBalls)
				case "blue":
					maxBlue = max(maxBlue, numberOfBalls)
				default:
					log.Fatalln("Unknown color found")
				}
			}
		}

		power := maxRed * maxGreen * maxBlue
		powerSum += power
	}

	fmt.Println(powerSum)

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
