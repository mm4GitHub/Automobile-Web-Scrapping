#TOYOTA

# INSTALL AND RUN NEEDED LIBRARIES FOR RSELENIUM

install.packages("tidyverse")
install.packages("RSelenium")
install.packages("wdman")
install.packages("netstat")
install.packages("data.table")
install.packages("httr")
install.packages("robotstxt")

library(tidyverse)
library(RSelenium)
library(wdman)
library(netstat)
library(data.table)
library(httr)

# CHECK FOR PERMISSION TO SCRAP WEB

library(robotstxt)
paths_allowed("https://www.toyota.com/all-vehicles/")

# LAUNCH WEBSITE VIA FIREFOX

rD <- rsDriver(browser = "firefox", chromever = NULL, port = 4545L, verbose = FALSE)
remDr <- rD$client
url <- "https://www.toyota.com/all-vehicles/"
remDr$open()
remDr$navigate(url)
Sys.sleep(5)

# SCRAP WEB
models <- remDr$findElements(using = "css selector", value = ".vehicle-card")
model_names <- sapply(models, function(x) x$getElementText())
View(model_names)


# FORMAT SCRAPPED DATA

install.packages("stringr")
library(stringr)

Toyota_1 <- list(model_names)

library(dplyr)

extract_vehicle_data <- function(text) {
  category <- ifelse(str_detect(text, "Hybrid|Plug-in|Fuel Cell|Battery"),
                     str_extract(text, "Hybrid EV|Plug-in Hybrid EV|Fuel Cell EV|Battery EV"),
                     "Gasoline")
  
  price_shown <- str_extract(text, "\\$[0-9,]+(?= as shown)")
  year <- str_extract(text, "\\b\\d{4}\\b")
  model <- str_match(text, paste0(year, "\\n([A-Za-z0-9\\- ]+)\\n"))[,2]
  starting_msrp <- str_extract(text, "\\$[0-9,]+(?=\\nStarting MSRP)")
  mpg <- str_extract(text, "\\d{2,3}/\\d{2,3}|\\d{2,3} miles")
  
  return(data.frame(Category = category,
                    Price = price_shown,
                    Model = model,
                    MSRP = starting_msrp,
                    MPG = mpg,
                    stringsAsFactors = FALSE))
}
Toyota_dataset <- bind_rows(lapply(model_names[], extract_vehicle_data))

View(Toyota_dataset)

# ADD CAR NAME COLUMN

Toyota_dataset <- cbind("Car name" = "Toyota", Toyota_dataset)

View(Toyota_dataset)
     
save.image(file = "Explorers.Rdata")


# MERCEDES

# RUN NEEDED LIBRARIES FOR RVEST
library(rvest)
library(xml2)
library(tidyverse)
library(httr)
library(dplyr)
library(tidyverse)

# CHECK FOR PERMISSION TO SCRAP WEB

library(robotstxt)
paths_allowed("https://www.mbusa.com/en/home")

# READ PAGE
url <- "https://www.mbusa.com/en/all-vehicles"
page <- read_html(url)

#SCRAP WEB SITE

car_models <- page %>% html_nodes(".all-vehicles__class-name") %>% html_text()
car_prices <- page %>% html_nodes(".all-vehicles__class-price") %>% html_text()

print(car_prices)

# FORMAT CAR PRICES TO REMOVE STARTING AT

car_prices <- str_replace_all(car_prices, "Starting at", "")
car_prices <- str_replace_all(car_prices, "\\*", "")
car_prices <- trimws(car_prices)

print(car_prices)

car_tags <- page %>% html_nodes(".all-vehicles__class-tagline") %>% html_text()

Mercedes_dataset <- data.frame(Model = car_models, Price = car_prices, Tag = car_tags)
View(Mercedes_dataset)
print(Mercedes_dataset)

# REMOVE SPACES

Mercedes_dataset[] <- lapply(cars_data1, function(x) {
  if (is.character(x)) gsub("\n", "", x) else x
})

print(Mercedes_dataset)

# ADD CAR NAME
Mercedes_dataset <- cbind("Car name" = "Mercedes", Mercedes_dataset)

#MERGING  CARS DATASETS

Car_dataset <- merge(Toyota_dataset, Mercedes_dataset, by = c("Car name", "Model", "Price"), all = TRUE)

View(Car_dataset)

# CLEAN CAR_DATASET BY REMOVING UNCOMMON COLUMNS

Car_dataset <- Car_dataset[, !names(Car_dataset) %in% c("Category", "MSRP", "MPG", "Tag")]

View(Car_dataset)

# PERFORMING EXPLORATORY DATA ANALYSIS

# Check the structure of the Car_dataset
str(Car_dataset)

# Check for missing values in the Car_dataset
colSums(is.na(Car_dataset))

# Summary
summary(Car_dataset)

# Check the first and last rows
head(Car_dataset)
tail(Car_dataset)

# Count the number of unique car names and models
length(unique(Car_dataset$`Car name`))
length(unique(Car_dataset$Model))

# Create a copy of Car_dataset for EDA
Car_dataset01 <- Car_dataset

# Create a copy of Car_dataset as csv file
EXPLORERS <- Car_dataset
print(EXPLORERS)
write.csv(EXPLORERS, "EXPLORERS_Car_dataset.csv", row.names = FALSE)

# Remove dollar signs and commas, then convert to numeric to plot
Car_dataset01$Price <- as.numeric(gsub("[\\$,]", "", Car_dataset01$Price))

View(Car_dataset01)

# Histogram of car prices
hist(Car_dataset01$Price, main = "Car Price Distribution", xlab = "Price", col = "red", border = "black")

# Boxplot to detect outliers
boxplot(Car_dataset01$Price, main = "Car Prices", ylab = "Price", col = "lightgreen")

# TO REMOVE OUTLIERS
install.packages("DescTools")
library(DescTools)

?Winsorize
print(Winsorize)

# Winsorising Price column (capping at 5th and 95th percentiles)
Winsorised_Car_dataset01 <- Winsorize(Car_dataset01$Price,
                                      val = quantile(Car_dataset1$Price, c(0.05, 0.95), na.rm = TRUE))

# Comparing the original and winsorised data
summary(Car_dataset01$Price)
summary(Winsorised_Car_dataset01)

# Boxplot to confirm  outliers removal
boxplot(Winsorised_Car_dataset01, main = "Car Prices", ylab = "Price", col = "lightgreen")

# Histogram of winsorised car prices
hist(Winsorised_Car_dataset01, main = "Car Price Distribution", xlab = "Price", col = "red", border = "black")

save.image(file = "Explorers.Rdata")
