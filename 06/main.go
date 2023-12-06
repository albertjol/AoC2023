package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
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

func stringSliceToInt(strings *[]string) []int {
	result := make([]int, len(*strings))
	for i, myString := range *strings {
		nr, err := strconv.Atoi(myString)
		if err != nil {
			log.Fatalln("No Int found")
		}
		result[i] = nr
	}
	return result
}

func calcDistance(totalTime, holdTime int) int {
	raceTime := totalTime - holdTime
	return raceTime * holdTime
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
		Time:      7  15   30
		Distance:  9  40  200
	*/
	numRegex := regexp.MustCompile(`\d+`)

	scanner.Scan()
	// De tijden
	timeStr := numRegex.FindAllString(scanner.Text(), -1)
	times := stringSliceToInt(&timeStr)

	scanner.Scan()
	// De distances
	distRecordsStr := numRegex.FindAllString(scanner.Text(), -1)
	distRecords := stringSliceToInt(&distRecordsStr)
	multipliedWaysToBreak := 1
	for i := 0; i < len(times); i++ {
		waysToBreakRecord := 0
		for t := 1; t < times[i]; t++ {
			if calcDistance(times[i], t) > distRecords[i] {
				waysToBreakRecord++
			}
		}
		multipliedWaysToBreak *= waysToBreakRecord
		// Enne... Als het record niet gebroken kan worden? dan is de totale uitkomst 0...
	}

	fmt.Println(multipliedWaysToBreak)

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
		Time:      7  15   30
		Distance:  9  40  200
	*/

	numRegex := regexp.MustCompile(`\d+`)

	scanner.Scan()
	// De tijden
	line := strings.ReplaceAll(scanner.Text(), " ", "")
	timeStr := numRegex.FindString(line)
	time, _ := strconv.Atoi(timeStr)

	scanner.Scan()
	// De distances
	line = strings.ReplaceAll(scanner.Text(), " ", "")
	distRecordStr := numRegex.FindString(line)
	distRecord, _ := strconv.Atoi(distRecordStr)

	waysToBreakRecord := 0
	for t := 1; t < time; t++ {
		if calcDistance(time, t) > distRecord {
			waysToBreakRecord++
		}
	}

	fmt.Println(waysToBreakRecord)
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
