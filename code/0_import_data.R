# Download PFTC Trait Data

# Trait data is stored on a shared google spreadsheet for all projects contributing to the trait wheel.


# Load packages
# install.packages("googlesheets4")
# install.packages("tidyverse")
# NB. You will need to configure googlesheets authentication to your google account
library(googlesheets4)
library(tidyverse)

# Download data
data <- read_sheet("https://docs.google.com/spreadsheets/d/1ncqbniu0NUzCfcNe2fOl6M2Yj-BeOEXcYlPZHwNiIbk/edit#gid=0", col_names = TRUE)

# Filter to INCLINE trait data
data <- data %>%
  filter(project == "Incline")
