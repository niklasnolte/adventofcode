package main

import (
	"AOC2018/aocutils"
	"fmt"
	"strings"
)

func hasNOfAnyLetter(s string, n int) bool {
	m := make(map[rune]int)

	for _, c := range s {
		if _, ok := m[c]; ok {
			m[c] += 1
		} else {
			m[c] = 1
		}
	}

	for _, c := range m {
		if c == n {
			return true
		}
	}

	return false
}

func part1(lines []string) {
	sum_two := 0
	sum_three := 0

	for _, l := range lines {
		if hasNOfAnyLetter(l, 2) {
			sum_two += 1
		}
		if hasNOfAnyLetter(l, 3) {
			sum_three += 1
		}
	}
	fmt.Println(sum_two * sum_three)
}

func intersect(x string, y string) string {
	var sb strings.Builder
	for i, _ := range x {
		if x[i] == y[i] {
			sb.WriteByte(x[i])
		}
	}
	return sb.String()
}

func part2(lines []string) {
	// all lines have same length
	linelen := len(lines[0])
	for i, l1 := range lines {
		for _, l2 := range lines[i+1:] {
			intersection := intersect(l1, l2)
			if len(intersection) == linelen-1 {
				fmt.Println(intersection)
				return
			}
		}
	}
}

func main() {
	lines, err := aocutils.ReadInput("input2.txt")
	if err != nil {
		fmt.Println(err)
	}
	part1(lines)
	part2(lines)

}
