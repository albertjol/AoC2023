package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"regexp"
	"sort"
	"strconv"
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

type mapping struct {
	src   int
	dest  int
	count int
}

func getNextMapping(scanner *bufio.Scanner) []mapping {
	var result []mapping
	numRegex := regexp.MustCompile(`\d+`)
	scanner.Scan()
	scanner.Scan()
	line := scanner.Text()
	for line != "" {
		var mapp mapping
		nrsStr := numRegex.FindAllString(scanner.Text(), -1)
		mapp.dest, _ = strconv.Atoi(nrsStr[0])
		mapp.src, _ = strconv.Atoi(nrsStr[1])
		mapp.count, _ = strconv.Atoi(nrsStr[2])
		result = append(result, mapp)
		scanner.Scan()
		line = scanner.Text()
	}
	return result
}

func getNextMap(src int, mapp []mapping) int {
	for _, mapElem := range mapp {
		if src >= mapElem.src && src < mapElem.src+mapElem.count {
			return mapElem.dest + src - mapElem.src
		}
	}
	return src
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
		seeds: 79 14 55 13

		seed-to-soil map:
		50 98 2
		52 50 48

		soil-to-fertilizer map:
		0 15 37
		37 52 2
		39 0 15

		fertilizer-to-water map:
		49 53 8
		0 11 42
		42 0 7
		57 7 4

		water-to-light map:
		88 18 7
		18 25 70

		light-to-temperature map:
		45 77 23
		81 45 19
		68 64 13

		temperature-to-humidity map:
		0 69 1
		1 0 69

		humidity-to-location map:
		60 56 37
		56 93 4
	*/
	scanner.Scan()
	// De seeds
	numRegex := regexp.MustCompile(`\d+`)
	seedsStr := numRegex.FindAllString(scanner.Text(), -1)
	seeds := stringSliceToInt(&seedsStr)

	scanner.Scan()

	seedSoilMap := getNextMapping(scanner)
	soilFertilizer := getNextMapping(scanner)
	fertilizerWater := getNextMapping(scanner)
	waterLight := getNextMapping(scanner)
	lightTemperature := getNextMapping(scanner)
	temperatureHumidity := getNextMapping(scanner)
	humidityLocation := getNextMapping(scanner)

	var locations []int
	for _, seed := range seeds {
		soil := getNextMap(seed, seedSoilMap)
		fert := getNextMap(soil, soilFertilizer)
		water := getNextMap(fert, fertilizerWater)
		light := getNextMap(water, waterLight)
		temp := getNextMap(light, lightTemperature)
		hum := getNextMap(temp, temperatureHumidity)
		location := getNextMap(hum, humidityLocation)
		locations = append(locations, location)
	}

	sort.Ints(locations)

	fmt.Println(locations[0])

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
		seeds: 79 14 55 13

		seed-to-soil map:
		50 98 2
		52 50 48

		soil-to-fertilizer map:
		0 15 37
		37 52 2
		39 0 15

		fertilizer-to-water map:
		49 53 8
		0 11 42
		42 0 7
		57 7 4

		water-to-light map:
		88 18 7
		18 25 70

		light-to-temperature map:
		45 77 23
		81 45 19
		68 64 13

		temperature-to-humidity map:
		0 69 1
		1 0 69

		humidity-to-location map:
		60 56 37
		56 93 4
	*/
	scanner.Scan()
	// De seeds
	numRegex := regexp.MustCompile(`\d+`)
	seedsStr := numRegex.FindAllString(scanner.Text(), -1)
	seedsRanges := stringSliceToInt(&seedsStr)

	scanner.Scan()

	seedSoilMap := getNextMapping(scanner)
	soilFertilizer := getNextMapping(scanner)
	fertilizerWater := getNextMapping(scanner)
	waterLight := getNextMapping(scanner)
	lightTemperature := getNextMapping(scanner)
	temperatureHumidity := getNextMapping(scanner)
	humidityLocation := getNextMapping(scanner)

	minLocation := math.MaxInt

	for i := 0; i < len(seedsRanges); i += 2 {
		for j := seedsRanges[i]; j < seedsRanges[i]+seedsRanges[i+1]; j++ {
			soil := getNextMap(j, seedSoilMap)
			fert := getNextMap(soil, soilFertilizer)
			water := getNextMap(fert, fertilizerWater)
			light := getNextMap(water, waterLight)
			temp := getNextMap(light, lightTemperature)
			hum := getNextMap(temp, temperatureHumidity)
			location := getNextMap(hum, humidityLocation)
			if location < minLocation {
				minLocation = location
			}
		}
	}

	fmt.Println(minLocation)

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
