# Load libraries and packages
library(Lahman)
library(tidyverse)
library(dslabs)
library(rtweet)


twitter_keys <- read_csv("api_keys.csv")

appname <- twitter_keys$appname
key <- twitter_keys$api_key
secret <- twitter_keys$api_secret
access_token <- twitter_keys$access_token
access_secret <- twitter_keys$access_secret

# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)


# Load data from Health Canada
url <- "https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-map.csv"
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

dat[is.na(dat)] <- "0" # Change all NA data to 0

# Convert incorrectly typed columns from strings to numeric values
dat <- suppressWarnings(transform(dat, numtotal_1dose = as.numeric(numtotal_1dose), 
          numtotal_2doses = as.numeric(numtotal_2doses), 
          proptotal_1dose = as.numeric(proptotal_1dose), 
          proptotal_2doses = as.numeric(proptotal_2doses)))


# Dates where data was received, starting at the most recent (index 1 is the latest data).
dates <- rev(unique(dat$week_end))


format_value <- function(key, val) {
  sprintf(
    "%s: %d%%",
    key, val)
}

plot_dose_percentages <- function(prv_name) {

  
  
  # Get latest percent wide percent
  latest_percent_first <- round(dat[dat$prename == prv_name & dat$week_end == dates[1], ]$proptotal_atleast1dose, digits = 0)
  latest_percent_both <- round(dat[dat$prename == prv_name & dat$week_end == dates[1], ]$proptotal_2doses, digits = 0)
  
  
  
  df <- data.frame(
    "Received first dose" = latest_percent_first,
    "Received both doses" = latest_percent_both, check.names = FALSE)
  
  df <- df %>%
    gather(key, val) %>%
    mutate(
      key = factor(key, rev(unique(key))),
      Total = 100)
  
  
  ggplot(df, aes(key, val)) +
    geom_col(fill = "green") +
    geom_col(aes(y = Total), alpha = 0.5, colour = "black") +
    geom_text(
      aes(y = 5, label = format_value(key, val)),
      hjust = 0,
      fontface = "bold",
      colour = "white",
      cex = 13) +
    coord_flip() +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank())
  
}
tmp <- tempfile(fileext = ".jpeg")
jpeg(tmp,
     width=1200 , height=675)
plot_dose_percentages("Alberta")
dev.off()


post_tweet("a tweet with media attachment", media = tmp)

## lookup status_id
my_timeline <- get_timeline(rtweet:::home_user())

## ID for reply
reply_id <- my_timeline$status_id[1]

## post reply
post_tweet("second in the thread",
           in_reply_to_status_id = reply_id)








