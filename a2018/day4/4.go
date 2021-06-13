package main

import (
	"AOC2018/aocutils"
	"errors"
	"fmt"
	"log"
	"regexp"
	"sort"
	"strings"

	psc "github.com/prataprc/goparsec"
)

type EventType int

const (
	NewShift EventType = iota
	FallAsleep
	WakeUp
)

type Date struct {
	year  int
	month int
	day   int
}

type Time struct {
	hour   int
	minute int
}

type Timestamp struct {
	date Date
	time Time
}

type Event struct {
	TS      Timestamp
	Type    EventType
	GuardID int
}

type ByEvent []Event // for sorting

func (a ByEvent) Len() int {
	return len(a)
}

func (a ByEvent) Less(i, j int) bool {
	d1 := a[i].TS.date
	d2 := a[j].TS.date
	if d1.year != d2.year {
		return d1.year < d2.year
	}
	if d1.month != d2.month {
		return d1.month < d2.month
	}
	if d1.day != d2.day {
		return d1.day < d2.day
	}
	t1 := a[i].TS.time
	t2 := a[j].TS.time
	if t1.hour != t2.hour {
		return t1.hour < t2.hour
	}
	if t1.minute != t2.minute {
		return t1.minute < t2.minute
	}
	return false
}

func (a ByEvent) Swap(i, j int) {
	a[i], a[j] = a[j], a[i]
}

func buildEventParser(ast *psc.AST) psc.Parser {
	number := psc.Int()
	bropen := psc.Atom("[", "bropen")
	brclose := psc.Atom("]", "brclose")
	minus := psc.Atom("-", "minus")
	colon := psc.Atom(":", "colon")
	date := ast.And("date", nil, number, minus, number, minus, number)
	time := ast.And("time", nil, number, colon, number)
	timestamp := ast.And("timestamp", nil, bropen, date, time, brclose)
	evttype := psc.Token(`[a-zA-Z#0-9 ]+`, "evttype")
	return ast.And("evt", nil, timestamp, evttype)
}

func extractIntFromStr(s string) (int, error) {
	r, err := regexp.Compile("[0-9]+")
	if err != nil {
		return 0, err
	}
	result := r.Find([]byte(s))
	if len(result) == 0 {
		return 0, errors.New("no int extractable from" + s)
	}
	return aocutils.ParseInt(string(result)), nil
}

func parseEvent(s string, ast *psc.AST, p psc.Parser) Event {
	// parse this: #19 @ 152,639: 22x26
	buf := []byte(s)
	root, news := ast.Parsewith(p, psc.NewScanner(buf))
	if root == nil {
		log.Fatal(news)
		return Event{}
	}

	pi := aocutils.ParseInt

	tsRepr := root.GetChildren()[0].GetChildren()
	timeRepr := tsRepr[2].GetChildren()
	time := Time{
		hour:   pi(timeRepr[0].GetValue()),
		minute: pi(timeRepr[2].GetValue())}
	dateRepr := tsRepr[1].GetChildren()
	date := Date{
		year:  pi(dateRepr[0].GetValue()),
		month: pi(dateRepr[2].GetValue()),
		day:   pi(dateRepr[4].GetValue())}
	ts := Timestamp{date, time}

	evttypeRepr := root.GetChildren()[1].GetValue()

	var evttype EventType
	guardid := 0

	if strings.Contains(evttypeRepr, "begins shift") {
		evttype = NewShift
		i, err := extractIntFromStr(evttypeRepr)
		if err != nil {
			log.Fatal(err)
		}
		guardid = i
	} else if strings.Contains(evttypeRepr, "falls asleep") {
		evttype = FallAsleep
	} else if strings.Contains(evttypeRepr, "wakes up") {
		evttype = WakeUp
	}

	return Event{ts, evttype, guardid}
}

func parseEvents(input []string) []Event {
	ast := psc.NewAST("claim", 20)
	p := buildEventParser(ast)

	parse := func(c string) Event {
		return parseEvent(c, ast, p)
	}

	events := make([]Event, 0, len(input))

	for _, inputi := range input {
		events = append(events, parse(inputi))
	}
	return events
}

func adjustGuardIDs(evts []Event) {
	// evts are sorted
	previousID := 0
	for i := range evts {
		evt := &evts[i]
		if evt.Type == NewShift {
			previousID = evt.GuardID
		} else {
			evt.GuardID = previousID
		}
	}
}

func gatherSleepSchedule(evts []Event) map[int]map[int]int {
	timeSlept := make(map[int]map[int]int)
	addMinute := func(schedule map[int]int, min int) {
		if _, ok := schedule[min]; ok {
			schedule[min] += 1
		} else {
			schedule[min] = 1
		}
	}
	for i, evt := range evts {
		if evt.Type == FallAsleep {
			id := evt.GuardID
			from := evt.TS.time.minute
			till := evts[i+1].TS.time.minute
			if _, ok := timeSlept[id]; ok {
				for min := from; min < till; min++ {
					addMinute(timeSlept[id], min)
				}
			} else {
				timeSlept[id] = map[int]int{from: 1}
				for min := from + 1; min < till; min++ {
					addMinute(timeSlept[id], min)
				}
			}
		}
	}
	return timeSlept
}

func findSleepiestDudeAndMinute(sleepSchedule map[int]map[int]int) (int, int) {
	sleepyDudeID := 0
	totalMax := 0
	bestMinute := 0
	for dudeID, sched := range sleepSchedule {
		total := 0
		maxMinute := 0
		maxMinuteIdx := 0
		for idx, count := range sched {
			total += count
			if count > maxMinute {
				maxMinute = count
				maxMinuteIdx = idx
			}
		}
		if total > totalMax {
			totalMax = total
			sleepyDudeID = dudeID
			bestMinute = maxMinuteIdx
		}
	}
	return sleepyDudeID, bestMinute
}

func findMostPredictableDudeAndMinute(sleepSchedule map[int]map[int]int) (int, int) {
	bestDudeID := 0
	bestMinute := 0
	bestMinuteIdx := 0
	for dudeID, sched := range sleepSchedule {
		for idx, count := range sched {
			if count > bestMinute {
				bestMinute = count
				bestMinuteIdx = idx
				bestDudeID = dudeID
			}
		}
	}
	return bestDudeID, bestMinuteIdx
}

func part1(inputs []string) {
	events := parseEvents(inputs)
	sort.Sort(ByEvent(events))
	adjustGuardIDs(events)
	sched := gatherSleepSchedule(events)
	id, minute := findSleepiestDudeAndMinute(sched)
	fmt.Println(id * minute)
}

func part2(inputs []string) {
	events := parseEvents(inputs)
	sort.Sort(ByEvent(events))
	adjustGuardIDs(events)
	sched := gatherSleepSchedule(events)
	id, minute := findMostPredictableDudeAndMinute(sched)
	fmt.Println(id * minute)
}

func main() {
	lines, err := aocutils.ReadLines("input4.txt")
	if err != nil {
		fmt.Println(err)
	}
	part1(lines)
	part2(lines)

}
