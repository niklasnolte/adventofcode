package aocutils

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"reflect"
	"strconv"
)

func MaxInt(x, y int) int {
	if x < y {
		return y
	}
	return x
}

func MinInt(x, y int) int {
	if x < y {
		return x
	}
	return y
}

func AbsInt(x int) int {
	if x >= 0 {
		return x
	} else {
		return -x
	}
}

func MaxIntInSlice(x []int) int {
	max := 0
	for _, v := range x {
		max = MaxInt(v, max)
	}
	return max
}

func ParseInt(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		log.Fatal(err)
	}
	return i
}

func ReadLines(which string) ([]string, error) {
	var ret []string

	file, err := os.Open(which)
	if err != nil {
		fmt.Println(err)
		return ret, error(err)
	}

	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)

	for scanner.Scan() {
		inpi := scanner.Text()
		ret = append(ret, inpi)
	}
	return ret, nil
}

func PrintMethodNames(obj interface{}) {
	newstype := reflect.TypeOf(obj)
	for i := 0; i < newstype.NumMethod(); i++ {
		method := newstype.Method(i)
		fmt.Println(method.Name)
	}
}
