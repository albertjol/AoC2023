package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"time"
)

func main() {
	partOne("input_ex.txt")
	partOne("input_ex2.txt")
	partOne("input.txt")
	//fmt.Println("****************** PART TWO ********************")
}

type crossing struct {
	LString string
	RString string
	L       *crossing
	R       *crossing
}

func (a crossing) getNext(dir rune) *crossing {
	if dir == 'L' {
		return a.L
	} else {
		return a.R
	}
}

func partOne(fileName string) {
	fmt.Println("****************** PART ONE ********************")
	start := time.Now()
	file, err := os.Open(fileName)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	/*
		RL

		AAA = (BBB, CCC)
		BBB = (DDD, EEE)
		CCC = (ZZZ, GGG)
		DDD = (DDD, DDD)
		EEE = (EEE, EEE)
		GGG = (GGG, GGG)
		ZZZ = (ZZZ, ZZZ)
		-----------------------------
		LLR

		AAA = (BBB, BBB)
		BBB = (AAA, ZZZ)
		ZZZ = (ZZZ, ZZZ)
	*/
	scanner.Scan()
	directions := []rune(scanner.Text())
	scanner.Scan()

	routeMap := make(map[string]*crossing)
	routeRegex := regexp.MustCompile(`(\w{3}).+(\w{3}).+(\w{3})`)
	var curPos, endPos *crossing
	for scanner.Scan() {
		match := routeRegex.FindStringSubmatch(scanner.Text())
		curCrossing := &crossing{match[2], match[3], nil, nil}
		routeMap[match[1]] = curCrossing
		if (curPos) == nil {
			curPos = curCrossing
		}
		endPos = curCrossing
	}

	for key := range routeMap {
		value := routeMap[key]
		value.L = routeMap[value.LString]
		value.R = routeMap[value.RString]
	}

	steps := 0
	for i, done := 0, false; !done; i, steps = i+1, steps+1 {
		if i == len(directions) {
			i = 0
		}
		curPos = curPos.getNext(directions[i])
		if curPos == endPos {
			done = true
		}
	}

	fmt.Println(steps)
	fmt.Println(time.Since(start))

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
