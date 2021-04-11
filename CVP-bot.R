library(Lahman)
library(tidyverse)
library(dslabs)

# Load data from Health Canada
url <- "https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-map.csv"
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

# Dates where data was received, starting at the most recent (index 1 is the latest data).
dates <- rev(unique(dat$week_end))


# Get latest Canada wide percent
latest_percent <- dat[dat$prename == "Canada" & dat$week_end == dates[1], ]$proptotal_atleast1dose
round(latest_percent, digits = 2)



# Round to nearest 5 for the progress bar
mround <- function(x,base){
  base*round(x/base)
}

# Function that returns a progress bar based on the percent

vis_progress <- function(percent){
  
  progress_bar <- "░░░░░░░░░░░░░░░░░░░░"
  
  i <- 1
  j <- mround(percent, 5)/5
  while (i <= j) {
    substr(progress_bar, i, i) <- "█"
    i = i + 1
  }
  
  progress_bar
  
  
}

#
vis_progress(latest_percent)





















