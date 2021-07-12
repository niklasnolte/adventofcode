package main

import (
	"AOC2018/aocutils"
	"fmt"
	"log"
	"strconv"

	psc "github.com/prataprc/goparsec"
)

type Position struct {
	x int
	y int
}

type Claim struct {
	ul Position
	lr Position
}

func makeClaim(x, y, w, h int) Claim {
	return Claim{Position{x, y}, Position{w + x, y + h}}
}

// such an overkill lolz
func buildClaimParser(ast *psc.AST) psc.Parser {
	number := psc.Int()
	ht := psc.Atom("#", "ht")
	at := psc.Atom("@", "at")
	comma := psc.Atom(",", "comma")
	x := psc.Atom("x", "x")
	colon := psc.Atom(":", "colon")
	return ast.And("p", nil, ht, number, at, number, comma, number, colon, number, x, number)
}

func parseClaim(s string, ast *psc.AST, p psc.Parser) Claim {
	// parse this: #19 @ 152,639: 22x26
	buf := []byte(s)
	root, news := ast.Parsewith(p, psc.NewScanner(buf))
	if root == nil {
		fmt.Println(news)
		return Claim{}
	}
	getChild := func(i int) int {
		j, err := strconv.Atoi(root.GetChildren()[i].GetValue())
		if err != nil {
			log.Fatal(err)
		}
		return j
	}
	return makeClaim(getChild(3), getChild(5), getChild(7), getChild(9))
}

func parseClaims(input []string) []Claim {
	ast := psc.NewAST("claim", 20)
	p := buildClaimParser(ast)

	parse := func(c string) Claim {
		return parseClaim(c, ast, p)
	}

	claims := make([]Claim, 0, len(input))

	for _, inputi := range input {
		claims = append(claims, parse(inputi))
	}
	return claims
}

func intersect(c1, c2 Claim) bool {
	ulx := aocutils.MaxInt(c1.ul.x, c2.ul.x)
	uly := aocutils.MaxInt(c1.ul.y, c2.ul.y)
	lrx := aocutils.MinInt(c1.lr.x, c2.lr.x)
	lry := aocutils.MinInt(c1.lr.y, c2.lr.y)

	return !(ulx >= lrx || uly >= lry)
}

func calculateOverlap(claims []Claim) int {
	counts := make(map[Position]int)
	for _, c := range claims {
		for x := c.ul.x; x < c.lr.x; x++ {
			for y := c.ul.y; y < c.lr.y; y++ {
				if count, ok := counts[Position{x, y}]; ok {
					counts[Position{x, y}] = count + 1
				} else {
					counts[Position{x, y}] = 1
				}
			}
		}
	}
	overlaparea := 0
	for _, count := range counts {
		if count > 1 {
			overlaparea++
		}
	}
	return overlaparea
}

func part1(input []string) {
	claims := parseClaims(input)
	fmt.Println(calculateOverlap(claims))
}

func part2(input []string) {
	claims := parseClaims(input)
	for i, c1 := range claims {
		has_intersection := false
		for _, c2 := range claims {
			if c1 == c2 {
				continue
			}
			if intersect(c1, c2) {
				has_intersection = true
				break
			}
		}
		if !has_intersection {
			fmt.Println(i + 1)
		}
	}
}

func main() {
	lines, err := aocutils.ReadLines("input3.txt")
	if err != nil {
		fmt.Println(err)
	}
	part1(lines)
	part2(lines)

}
