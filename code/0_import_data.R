# Download PFTC Trait Data

# Trait data is stored on a shared google spreadsheet for all projects contributing to the trait wheel.


# Load packages
# install.packages("googlesheets4")
# install.packages("tidyverse")
# install.packages("osfr")
# Problems with normal dataDownloader package, install from here instead
# remotes::install_github("nyuglobalties/osfr@fix/use-wb-asset-id")
# install.packages("lubridate")
# install.packages("tidyverse")
# install.packages("validate")
# install.packagse("readxl")
# NB. You will need to configure googlesheets authentication to your google account
library(googlesheets4)
library(tidyverse)
library(osfr)
library(dataDownloader)
library(lubridate)
library(tidyverse)
library(validate)
library(readxl)


# Downloading data from OSF

# Get data from OSF Account
# osf_auth(token = "add OSF PAT Token Here")

# Download
 # get_file(node = "pk4bg", # Check this is either INCLINE or 3D, currently 3D as it's open access and incline is private
 #          file = "PFTC6_Norway_Leaf_traits_2022.xlsx",
 #          path = "raw_Data/Traits",
 #          remote_path = "RawData/Traits")

#### Load data ####
# Read in dataset
# data <- readxl::read_excel("raw_Data/Traits/PFTC6_Norway_Leaf_traits_2022.xlsx",
#                           sheet = "Data")

# OR Download data directly from google sheets (not used once on OSF)
 data <- read_sheet("https://docs.google.com/spreadsheets/d/1ncqbniu0NUzCfcNe2fOl6M2Yj-BeOEXcYlPZHwNiIbk/edit#gid=0",
                    sheet = "Data",
                    col_names = TRUE)

# Create data validation rules using 'validate' package


# Code to fix validation issues


# Filter to INCLINE trait data
data <- data %>%
  filter(project == "Incline")

# Write data to cleaned folder
