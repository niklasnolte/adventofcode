package main

import (
	"AOC2018/aocutils"
	"fmt"
	"log"
	"strings"
)

type Tree struct {
	children []Tree
	metadata []int
}

func parseInput(input string) []int {
	parsed := strings.Split(input, " ")
	nums := make([]int, len(parsed))
	for i, n := range parsed {
		nums[i] = aocutils.ParseInt(n)
	}
	return nums
}

func buildTree(input *[]int) Tree {
	node := Tree{}
	nChildren := (*input)[0]
	nMetadata := (*input)[1]
	*input = (*input)[2:]
	for i := 0; i < nChildren; i++ {
		node.children = append(node.children, buildTree(input))
	}
	for i := 0; i < nMetadata; i++ {
		node.metadata = append(node.metadata, (*input)[i])
	}
	*input = (*input)[nMetadata:]
	return node
}

func sumMetaData(node Tree) int {
	sum := 0
	for _, m := range node.metadata {
		sum += m
	}
	for _, c := range node.children {
		sum += sumMetaData(c)
	}
	return sum
}

func sumValues(node Tree) int {
	sum := 0
	if len(node.children) == 0 {
		for _, m := range node.metadata {
			sum += m
		}
	} else {
		for _, m := range node.metadata {
			idx := m - 1
			if idx >= 0 && idx < len(node.children) {
				sum += sumValues(node.children[idx])
			}
		}
	}
	return sum
}

func part1(input string) {
	nums := parseInput(input)
	t := buildTree(&nums)
	fmt.Println(sumMetaData(t))
}

func part2(input string) {
	nums := parseInput(input)
	t := buildTree(&nums)
	fmt.Println(sumValues(t))
}

func main() {
	input, err := aocutils.Read("input8.txt")
	if err != nil {
		log.Fatal(err)
	}
	part1(input)
	part2(input)
}
