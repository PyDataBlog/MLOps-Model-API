package main

import (
	"github.com/fabioxgn/go-bot"
	_ "github.com/fabioxgn/go-bot/commands/catfacts"
	_ "github.com/fabioxgn/go-bot/commands/catgif"
	_ "github.com/fabioxgn/go-bot/commands/chucknorris"
	_ "github.com/nloadholtes/uniform/ratt"
	// Import all the commands you wish to use
	"flag"
	"os"
	"strings"
)

var slack_flag bool

func main() {
	flag.BoolVar(&slack_flag, "slack", false, "help message for flagname")

	flag.Parse()
	if slack_flag {
		bot.RunSlack(os.Getenv("SLACK_TOKEN"))
	} else {
		bot.Run(&bot.Config{
			Server:   os.Getenv("IRC_SERVER"),
			Channels: strings.Split(os.Getenv("IRC_CHANNELS"), ","),
			User:     os.Getenv("IRC_USER"),
			Nick:     os.Getenv("IRC_NICK"),
			Password: os.Getenv("IRC_PASSWORD"),
			UseTLS:   true,
			Debug:    os.Getenv("DEBUG") != ""})
	}
}

func init() {
}
