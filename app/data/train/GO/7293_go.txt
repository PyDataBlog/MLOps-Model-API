package main

import "fmt"
import "io/ioutil"
import "io"

import "net/http"

type Response struct {
	Body io.ReadCloser
}

func (resp Response) String() string {
	body, _ := ioutil.ReadAll(resp.Body)

	return string(body)
}

func main() {
	fmt.Println("hello world")

	sendHttpGet("http://www.cnet.com")
	// var givenName string
	// givenName = askMeMyName()
	//
	// fmt.Printf("Your name is %s\n", givenName)
}

func askMeMyName() string {
	var givenName string
	fmt.Println("What is your name?")
	fmt.Scanf("%s", &givenName)

	return givenName
}

func sendHttpGet(url string) {
	// var response http.Response
	// var err error
	//
	response, err := http.Get(url)

	if err != nil {
		panic(err)
	}

	if response.StatusCode == 200 {
		// body, _ := ioutil.ReadAll(response.Body)
		body := Response{response.Body}

		fmt.Printf("%s", body)
	} else {
		fmt.Printf("The Status code is %s", response.StatusCode)
	}
}
