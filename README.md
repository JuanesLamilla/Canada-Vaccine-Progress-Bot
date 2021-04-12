# Canada Vaccine Progress Bot

The Canada Vaccine Progress Bot (CVPbot) uses statistics straight from health Canada to determine the percentage of Canadians that have received either just their first dose, or both doses, of the COVID-19 vaccination both Canada wide, and province by province. This data is then published directly to twitter, with an image of a progress bar that fills as more Canadians get vaccinated. 

Furthermore, the app uses linear regression techniques to estimate a date for when a certain percentage of Canadians will be vaccinated. The estimated date for when 70% of Canadians have received their first shot is posted on twitter.

## Installation

Installing the following packages is required to run the program:

- tidyverse
- rtweet

You may do this by typing the following into the terminal.
```bash
install.packages("tidyverse")
install.packages("rtweet")
```

To protect the API keys, they are stored in a file titled *api_keys.csv*. If you wish to run this program on your own, change the following code at the top of CVP-bot.R:

```R
twitter_keys <- read_csv("api_keys.csv")

appname <- twitter_keys$appname
key <- twitter_keys$api_key
secret <- twitter_keys$api_secret
access_token <- twitter_keys$access_token
access_secret <- twitter_keys$access_secret
```

to

```R
appname <- "YOUR TWITTER APPNAME"
key <- "YOUR TWITTER API KEY"
secret <- "YOUR TWITTER API SECRET KEY"
access_token <- "YOUR TWITTER API ACCESS TOKEN"
access_secret <- "YOUR TWITTER API SECRET ACESS TOKEN"
```

## Running the bot

To run the program, simply run CVP-bot.py whenever you need it to post the latest stats.

As Health Canada updates their data online every friday, the program is scheduled to run on a Heroku server as soon as new data is released every Friday at 12 pm Eastern Time. 

## Contributing
Pull requests and issues are welcome. I am open to any new features that could add value.

## License
[MIT](https://choosealicense.com/licenses/mit/)
