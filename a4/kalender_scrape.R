# load packages
library(tidyverse)
library(dplyr)
library(rvest)
library(lubridate)

# stores when the data was scraped
scraped <- Sys.time()
# url to be scraped
url <- "http://timeplan.uit.no/emne_timeplan.php?sem=20h&module%5B%5D=BED-2056-1&View=list"

# gets html text from websites
web <- read_html(url)

# Gets data from calender
dates <- html_text(html_nodes(web, "td:nth-child(1)")) 
# Keep only numbers
dates <- dmy(str_remove(dates, "Mandag"))
# Rest of calender data
time <- (html_text(html_nodes(web, "td:nth-child(2)")))
code <- (html_text(html_nodes(web, "td:nth-child(4) a")))
description <- (html_text(html_nodes(web, ".act-summary")))
teacher <- (html_text(html_nodes(web, "td:nth-child(6)")))

# Combine into one data frame
longdf <- data.frame(Date = dates, Time = time, Code = code, Description = description, Teacher = teacher, Scraped = scraped)
