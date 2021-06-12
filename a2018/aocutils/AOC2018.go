package aocutils

import (
	"bufio"
	"fmt"
	"os"
)

func ReadInput(which string) ([]string, error) {
	var ret []string

	file, err := os.Open(which)
	if err != nil {
		fmt.Println(err)
		return ret, error(err)
	}

	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanWords)

	for scanner.Scan() {
		inpi := scanner.Text()
		ret = append(ret, inpi)
	}
	return ret, nil
}
