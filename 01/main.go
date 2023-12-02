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

func main() {
	partOne("input.txt")
	partTwo("input.txt")
}

func partOne(fileName string) {
	fmt.Println("****************** PART ONE ********************")
	file, err := os.Open(fileName)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	// optionally, resize scanner's capacity for lines over 64K, see next example
	/*
		const maxCapacity int = longLineLen  // your required line length
		buf := make([]byte, maxCapacity)
		scanner.Buffer(buf, maxCapacity)
	*/
	sum := 0
	for scanner.Scan() {
		value := -1
		var lastNum int
		for _, char := range scanner.Text() {
			num, err := strconv.Atoi(string(char))
			if err == nil {
				//fmt.Print(num, " - ")
				if value == -1 {
					value = num * 10
				}
				lastNum = num
			}
		}
		//fmt.Println(lastNum)
		value += lastNum
		//fmt.Println(value)
		sum += value
	}
	fmt.Println(sum)

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
	// optionally, resize scanner's capacity for lines over 64K, see next example
	/*
		const maxCapacity int = longLineLen  // your required line length
		buf := make([]byte, maxCapacity)
		scanner.Buffer(buf, maxCapacity)
	*/

	sum := 0
	for scanner.Scan() {
		line := scanner.Text()
		//fmt.Println(line)
		value := getFirstNumber(line)*10 + getLastNumber(line)
		//fmt.Println(value)
		sum += value
	}
	fmt.Println(sum)

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}

type number struct {
	digit   string
	written string
}

var numbers = []number{
	{"1", "one"},
	{"2", "two"},
	{"3", "three"},
	{"4", "four"},
	{"5", "five"},
	{"6", "six"},
	{"7", "seven"},
	{"8", "eight"},
	{"9", "nine"},
}

type posValue struct {
	pos   int
	value int
}

/*********** Sort Interface ****************/
type posValues []posValue

func (a posValues) Less(i, j int) bool {
	return a[i].pos < a[j].pos
}

func (a posValues) Swap(i, j int) {
	a[i], a[j] = a[j], a[i]
}

func (a posValues) Len() int {
	return len(a)
}

/********************************************/

func minP(a, b int) int {
	if (b == -1) || ((a < b) && (a > -1)) {
		return a
	} else {
		return b
	}
}

func max(a, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

func getFirstNumber(line string) int {

	p := make(posValues, 9)
	for i := 0; i < 9; i++ {
		p[i].value = i + 1
		p[i].pos = minP(strings.Index(line, numbers[i].written),
			strings.Index(line, numbers[i].digit))
	}

	sort.Sort(p)
	for _, n := range p {
		if n.pos > -1 {
			return n.value
		}
	}
	log.Fatal("niks gevonden!")
	return 0xffffffff
}

func getLastNumber(line string) int {

	p := make(posValues, 9)
	for i := 0; i < 9; i++ {
		p[i].value = i + 1
		p[i].pos = max(strings.LastIndex(line, numbers[i].written),
			strings.LastIndex(line, numbers[i].digit))
	}

	sort.Sort(sort.Reverse(p))
	for _, n := range p {
		if n.pos > -1 {
			return n.value
		}
	}
	log.Fatal("niks gevonden!")
	return 0xffffffff
}
