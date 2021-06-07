package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func toInts(input []string) ([]int, error) {

	var result []int
	for _, str_i := range input {
		i, err := strconv.Atoi(str_i)
		if err != nil {
			return result, error(err)
		}
		result = append(result, i)
	}
	return result, nil
}

func part1(input []string) {
	result := 0
	intarr, err := toInts(input)
	if err != nil {
		fmt.Println(err)
		return
	}
	for _, i := range intarr {
		result += i
	}
	fmt.Println(result)
}

func part2(input []string) {
	freqs := map[int]bool{0: true}
	currentFreq := 0
	intarr, err := toInts(input)
	if err != nil {
		fmt.Println(err)
		return
	}
	for {
		for _, inp := range intarr {
			currentFreq += inp
			if freqs[currentFreq] {
				fmt.Println(currentFreq)
				return
			} else {
				freqs[currentFreq] = true
			}
		}
	}
}

func main() {
	file, err := os.Open("input1.txt")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanWords)

	var input []string
	for scanner.Scan() {
		inpi := scanner.Text()
		input = append(input, inpi)
	}

	part1(input)
	part2(input)
}
