package main

import (
	"AOC2018/aocutils"
	"fmt"
	"log"
	"math"
	"strings"
)

type Coordinate struct {
	x int
	y int
}

type Grid = map[Coordinate]int

func parseLine(input string) Coordinate {
	numstrs := strings.Split(input, ", ")
	return Coordinate{aocutils.ParseInt(numstrs[0]), aocutils.ParseInt(numstrs[1])}
}

func coordsFromInput(input []string) []Coordinate {
	coords := make([]Coordinate, 0, len(input))
	for _, v := range input {
		coords = append(coords, parseLine(v))
	}
	return coords
}

func findBoundaries(refs []Coordinate) (Coordinate, Coordinate) {
	// finds a rectangle in which the "closest neighbors" are calculated
	ul := refs[0]
	lr := refs[0]

	for _, c := range refs {
		ul.x = aocutils.MinInt(c.x, ul.x)
		ul.y = aocutils.MinInt(c.y, ul.y)
		lr.x = aocutils.MaxInt(c.x, lr.x)
		lr.y = aocutils.MaxInt(c.y, lr.y)
	}

	return ul, lr
}

func manhattan(p1, p2 Coordinate) int {
	return aocutils.AbsInt(p1.x-p2.x) + aocutils.AbsInt(p1.y-p2.y)
}

func findClosestRef(point Coordinate, refs []Coordinate) int {
	idx := 0
	min := math.MaxInt32
	for i, ref := range refs {
		mh := manhattan(point, ref)
		if mh < min {
			idx = i
			min = mh
		} else if mh == min {
			idx = -1
		}
	}
	return idx
}

func assignClosest(refs []Coordinate, ul, lr Coordinate) Grid {
	gridsize := (lr.x - ul.x + 1) * (lr.y - ul.y + 1)
	grid := make(Grid, gridsize)

	for x := ul.x; x <= lr.x; x++ {
		for y := ul.y; y <= lr.y; y++ {
			point := Coordinate{x, y}
			closest := findClosestRef(point, refs)
			if closest != -1 {
				// unambiguous
				grid[point] = closest
			}
		}
	}
	return grid
}

func getLargestNonInfiniteArea(grid Grid, ul, lr Coordinate) int {
	onEdge := func(p Coordinate) bool {
		return p.x == ul.x || p.x == lr.x || p.y == ul.y || p.y == lr.y
	}
	counts := make(map[int]int, len(grid))
	doNotCountThese := make(map[int]bool)
	for point, idx := range grid {
		if onEdge(point) {
			// if a point is on the edge, it belongs to an infinite area
			// and therefore this area should not go into consideration
			doNotCountThese[idx] = true
		}
		if _, ok := counts[idx]; ok {
			counts[idx] += 1
		} else {
			counts[idx] = 1
		}
	}

	maxArea := 0
	for idx, count := range counts {
		if _, ok := doNotCountThese[idx]; !ok {
			maxArea = aocutils.MaxInt(count, maxArea)
		}
	}
	return maxArea
}

func part1(input []string) {
	refs := coordsFromInput(input)
	ul, lr := findBoundaries(refs)
	grid := assignClosest(refs, ul, lr)
	area := getLargestNonInfiniteArea(grid, ul, lr)
	fmt.Println(area)
}

func findBoundariesP2(refs []Coordinate, maxMH int) (Coordinate, Coordinate) {
	ul, lr := findBoundaries(refs)
	// now we need to go ~ maxMH/nrefs in each direction,
	// because the region could extend up to these points
	// extend := maxMH / len(refs)
	// // more than needed, but easy to implement
	// ul.x -= extend
	// ul.y -= extend
	// lr.x += extend
	// lr.y += extend
	return ul, lr
}

func closeEnoughToAllRefs(point Coordinate, refs []Coordinate, maxMH int) bool {
	total := 0
	for _, ref := range refs {
		total += manhattan(point, ref)
	}
	return total < maxMH // the threshold to be counted
}

func getSafeArea(refs []Coordinate, ul, lr Coordinate, maxMH int) int {
	safeArea := 0
	for x := ul.x; x <= lr.x; x++ {
		for y := ul.y; y <= lr.y; y++ {
			if closeEnoughToAllRefs(Coordinate{x, y}, refs, maxMH) {
				safeArea += 1
			}
		}
	}
	return safeArea
}

func part2(input []string) {
	refs := coordsFromInput(input)
	maxMH := 10000
	ul, lr := findBoundariesP2(refs, maxMH)
	safeArea := getSafeArea(refs, ul, lr, maxMH)
	fmt.Println(safeArea)
}

func main() {
	input, err := aocutils.ReadLines("input6.txt")
	if err != nil {
		log.Fatal(err)
	}
	part1(input)
	part2(input)
}
