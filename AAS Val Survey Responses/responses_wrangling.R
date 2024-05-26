##Clean entries for columns 5-8 to make them just numeric

library(tidyverse)
library(kableExtra)
library(datasets)
library(janitor)
library(readr)
library(stringr)

responses <- read_csv("Val Data/Copy -Valentine Hall Survey  (Responses) - Form Responses 1.csv")

# Extract numbers using regular expression (handles "th")
responses[[5]] <- str_extract(responses[[5]] , "\\d+")
responses[[6]] <- str_extract(responses[[6]] , "\\d+")
responses[[7]] <- str_extract(responses[[7]] , "\\d+")
responses[[8]] <- str_extract(responses[[8]] , "\\d+")

##Gather rank variables

responses <- responses |>
  mutate(
    trout_satis = responses[[4]],
    salmon_rank = as.numeric(responses[[5]]),
    mahi_rank = as.numeric(responses[[6]]),
    swordfish_rank = as.numeric(responses[[7]]),
    tilapia_rank = as.numeric(responses[[8]]),
  )

##Sort keywords in the open respose columns

# Define keywords
keywords <- c("fish", "\bsalmon\b", "catch of the day", "cod", "seafood" , "shrimp")

open_responses <- data.frame(responses[[3]], responses[[9]], responses[[10]], responses [[11]])

has_fish <- function(row, keywords) {
 any(sapply(keywords,grepl, text, ignore.case = TRUE))
}

writexl::write_xlsx(responses, "cleaned_responses.xlsx")

