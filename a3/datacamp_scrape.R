# load packages
library(tidyverse)
library(dplyr)
library(rvest)

# stores when the data was scraped
scraped <- Sys.time()
# urls to be scraped
py_url <- "https://www.datacamp.com/courses/tech:python"
r_url <- "https://www.datacamp.com/courses/tech:r"
# gets html text from websites
py_web <- read_html(py_url)
r_web <- read_html(r_url)

# stores the course-block__title, which contains the datacamp course title
py_courses <- html_text(html_nodes(py_web, ".course-block__title")) 
r_courses <- html_text(html_nodes(r_web, ".course-block__title"))

# test results
#
# py_courses
# r_courses

# scrape, and generate a data frame for each language
python <- data.frame(tech = py_courses, language = "Python", scraped = scraped)
r <- data.frame(tech = r_courses, language = "R", scraped = scraped)

# combine into final data frame
final_df <- bind_rows(python, r)

# test final data frame
#
# final_df
