package main

import (
	"AOC2018/aocutils"
	"fmt"
	"log"
	"regexp"
	"sort"
)

type Dependency struct {
	from rune
	to   rune
}

type Job struct {
	which    rune
	finishAt int
}

func parseInput(input []string) ([]Dependency, map[rune]bool) {
	dependencies := make([]Dependency, 0, len(input))
	allRunes := make(map[rune]bool)
	re, _ := regexp.Compile("Step ([A-Z]) must be finished before step ([A-Z]) can begin.")
	for _, line := range input {
		parts := re.FindStringSubmatch(line)
		from := rune(parts[1][0])
		to := rune(parts[2][0])
		dependencies = append(dependencies, Dependency{from, to})
		allRunes[from] = true
		allRunes[to] = true
	}
	return dependencies, allRunes
}

func takesNSeconds(r rune) int {
	// 60 + (1 for A, 2 for B, 3 for C, etc)
	return 61 + int(r-'A')
}

func resolveDependencies(dependencies []Dependency, allRunes map[rune]bool, nWorkers int) ([]rune, int) {
	sortedRunes := make([]rune, 0, len(allRunes))
	time := 0
	nRunes := len(allRunes)
	readyWorkers := nWorkers
	jobsInFlight := make([]Job, 0)
	for {
		// see if jobs completed and remove from jobsInFlight
		// and add them to a list of newly completed things
		done := make([]rune, 0)
		jumpTo := time + 1
		for i := 0; ; {
			if i >= len(jobsInFlight) {
				break
			}
			if jobsInFlight[i].finishAt == time {
				done = append(done, jobsInFlight[i].which)
				fmt.Println(time, "done", string(jobsInFlight[i].which))
				readyWorkers++
				jobsInFlight = append(jobsInFlight[:i], jobsInFlight[i+1:]...)
			} else {
				jumpTo = aocutils.MinInt(jumpTo, jobsInFlight[i].finishAt)
				i++
			}
		}

		if len(jobsInFlight) > 0 && len(done) == 0 {
			// active jobs are still going, but no more completed jobs
			time = jumpTo
			continue
		}

		// we might be done now
		sortedRunes = append(sortedRunes, done...)
		if len(sortedRunes) == nRunes {
			break
		}

		// remove the runes that are done dependencies (they are now fulfilled)
		for _, d := range done {
			for i := 0; ; {
				if i >= len(dependencies) {
					break
				}
				dep := dependencies[i]
				if dep.from == d {
					dependencies[i] = dependencies[len(dependencies)-1]
					dependencies = dependencies[:len(dependencies)-1]
				} else {
					i++
				}
			}
		}

		// find new jobs to work on
		readyRunes := make([]rune, 0)

		for r := range allRunes {
			depsFulfilled := true
			for _, dep := range dependencies {
				if dep.to == r {
					depsFulfilled = false
					break
				}
			}
			if depsFulfilled {
				readyRunes = append(readyRunes, r)
			}
		}
		// find "smallest" ready runes and put them to work
		sort.Slice(readyRunes, func(i, j int) bool { return readyRunes[i] < readyRunes[j] })
		takeN := aocutils.MinInt(readyWorkers, len(readyRunes))
		readyRunes = readyRunes[:takeN]
		for _, r := range readyRunes {
			delete(allRunes, r)
			fmt.Println(time, "adding", string(r), "to jobsInFlight")
			jobsInFlight = append(jobsInFlight, Job{r, time + takesNSeconds(r)})
			readyWorkers--
		}
		time++
	}
	return sortedRunes, time
}

func part1(input []string) {
	dependencies, allRunes := parseInput(input)
	sortd, _ := resolveDependencies(dependencies, allRunes, 1)
	fmt.Println(string(sortd))
}

func part2(input []string) {
	dependencies, allRunes := parseInput(input)
	_, time := resolveDependencies(dependencies, allRunes, 4)
	fmt.Println(time)
}

func main() {
	input, err := aocutils.ReadLines("input7.txt")
	if err != nil {
		log.Fatal(err)
	}
	part1(input)
	part2(input)
}
