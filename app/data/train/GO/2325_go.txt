package main

import (
	"encoding/gob"
	"encoding/json"
	"log"
	"os"
	"sort"

	"github.com/tsenart/vegeta/lib"
)

func main() {
	var res vegeta.Results
	if err := gob.NewDecoder(os.Stdin).Decode(&res); err != nil {
		log.Fatal(err)
	}
	sort.Sort(res)
	if err := json.NewEncoder(os.Stdout).Encode(res); err != nil {
		log.Fatal(err)
	}
}
