
library(data.table)
library(tidyverse)
#install.packages("PxWebApiData")
library(PxWebApiData)

?ApiData

county <- ApiData("http://data.ssb.no/api/v0/dataset/95274.json?lang=no",
                  getDataByGET = TRUE)

whole_country <- ApiData("http://data.ssb.no/api/v0/dataset/95276.json?lang=no",
                         getDataByGET = TRUE)

# two similar lists, different labels and coding
head(county[[1]])
head(county[[2]])

head(whole_country[[1]])

# Use first list, rowbind both data
dframe <- bind_rows(county[[1]], whole_country[[1]])


# new names, could have used dplyr::rename()
names(dframe)
names(dframe) <- c("region", "date", "variable", "value")
str(dframe)

# Split date
dframe <- dframe %>% separate(date, 
                              into = c("year", "month"), 
                              sep = "M")
head(dframe)

# Make a new proper date variable
library(lubridate)
dframe <- dframe %>%  mutate(date = ymd(paste(year, month, 1)))
str(dframe)

# And how many levels has the variable?
dframe %>% select(variable) %>% unique()

# dplyr::recode()
dframe <- dframe %>% mutate(variable2 = dplyr::recode(variable,
                                                      "Utleigde rom"="rentedrooms",
                                                      "Pris per rom (kr)"="roomprice",
                                                      "Kapasitetsutnytting av rom (prosent)"="roomcap",
                                                      "Kapasitetsutnytting av senger (prosent)"="bedcap",
                                                      "Losjiomsetning (1 000 kr)"="revenue",
                                                      "Losjiomsetning per tilgjengeleg rom (kr)"="revperroom",
                                                      "Losjiomsetning, hittil i Ãr (1 000 kr)"="revsofar",
                                                      "Losjiomsetning per tilgjengeleg rom, hittil i Ãr (kr)"="revroomsofar",
                                                      "Pris per rom hittil i Ãr (kr)"="roompricesofar",
                                                      "Kapasitetsutnytting av rom hittil i Ãr (prosent)"="roomcapsofar",
                                                      "Kapasitetsutnytting av senger, hittil i Ãr (prosent)"="bedcapsofar"))

dframe %>% select(variable2) %>% unique()
with(dframe, table(variable, variable2))

# recode region
dframe <- dframe %>% mutate(region = 
                              ifelse(region == "Hele landet",
                                     "Whole country", region))

mosaic::tally(~region, data = dframe)

# we now have the data in long format ready for data wrangling

dframe <- dframe %>% mutate(region2 = dplyr::recode(region,
                                            "Møre og Romsdal"="More_og_Romsdal",
                                            "Trøndelag - Trøndelag - Trööndelage"="Trondelag",
                                            "Heile landet"="Heile_landet",
                                            "Vestfold og Telemark"="Vestfold_og_Telemark",
                                            "Troms og Finnmark - Romsa ja Finnmárku"="Troms_og_Finnmark"))


library(ggplot2)
# Plot
dframe %>%
  filter(variable2 == "roomcap") %>%
  ggplot(aes(x=month, y=value, group=region2)) +
  geom_line(aes(color=region2)) +
  ylab(expression("Room capasity")) +
  xlab(expression("Month of the year")) +
  labs(title = "Roomcap over time",
       subtitle = "Per month, 2020",
       caption = "") +
  theme_linedraw()


