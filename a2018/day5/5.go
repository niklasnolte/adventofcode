package main

import (
	"AOC2018/aocutils"
	CL "container/list"
	"fmt"
	"log"
	"strings"
	"unicode"
)

func canReact(l, r rune) bool {
	return (unicode.ToLower(l) == unicode.ToLower(r)) && (unicode.IsLower(l) != unicode.IsLower(r))
}

func react(input *CL.List) {
	current := input.Front()
	for {
		if current != nil && current.Next() != nil {
			// fmt.Println(string(current.Value.(rune)))
			// at least two elements
			next := current.Next()
			if canReact(current.Value.(rune), next.Value.(rune)) {
				removeThis := current
				if current == input.Front() { // move on
					current = next.Next()
				} else { // go back one step
					current = current.Prev()
				}
				input.Remove(removeThis)
				input.Remove(next)
			} else {
				current = current.Next()
			}
		} else {
			return
		}
	}
}

func evalPolymer(input string) int {
	if len(input) == 0 {
		fmt.Println(0)
		return 0
	}
	l := CL.List{}
	for _, x := range input {
		l.PushBack(x)
	}
	react(&l)
	return l.Len()
}

func part1(input string) {
	fmt.Println(evalPolymer(input))
}

func part2(input string) {
	min := len(input)
	for _, x := range "abcdefghijklmnopqrstuvwxyz" {
		replacer := strings.NewReplacer(string(x), "", string(unicode.ToUpper(x)), "")
		removedX := replacer.Replace(input)
		min = aocutils.MinInt(evalPolymer(removedX), min)
	}
	fmt.Println(min)
}

func main() {
	input, err := aocutils.ReadLines("input5.txt")
	if err != nil {
		log.Fatal(err)
	}
	part1(input[0])
	part2(input[0])
}
