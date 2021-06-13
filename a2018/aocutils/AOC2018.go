package aocutils

import (
	"bufio"
	"fmt"
	"os"
	"reflect"
)

func Max(x, y int) int {
	if x < y {
		return y
	}
	return x
}

func Min(x, y int) int {
	if x < y {
		return x
	}
	return y
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
