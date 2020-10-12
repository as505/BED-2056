library(tidyverse)
library(dplyr)
library(rvest)
library(lubridate)

# stores when the data was scraped
scraped <- Sys.time()
# url(s) to be scraped
url1 <- "https://w2.brreg.no/kunngjoring/kombisok.jsp?datoFra=01.04.2020&datoTil=01.09.2020&id_region=0&id_niva1=51&id_niva2=-+-+-&id_bransje1=0"
url2 <- "https://w2.brreg.no/kunngjoring/kombisok.jsp?datoFra=01.04.2019&datoTil=01.09.2019&id_region=0&id_niva1=51&id_niva2=-+-+-&id_bransje1=0"
# gets html code from websites
web2020 <- read_html(url1)
web2019 <- read_html(url2)

create_table <- function(web) {
  table <- html_nodes(web, xpath = "//table") %>%     # Get html data
    html_nodes("table") %>%                           # Select table with data
    html_table() %>%                                  # 
    .[[1]] %>%                                        # 
    select(X2, X4, X6, X8) %>%                                    # Select relevant data, X2 = Name, X4 = Org.nr, X6 = Date, X8 = Type
    mutate(Fylke =ifelse(grepl("[A-z]", X4), X4, NA)) %>%         # Create row for region
    fill(Fylke) %>%                                               # Fill NA's with previous region name
    filter(nchar(X4, type = "chars") != 6) %>%                    # Only keep companies org.nr's
    filter(X2 != Fylke, X2 != X8) %>%                             # Removes rows declaring region along with empty rows by comparing with Name cell
    mutate(X6 = dmy(X6)) %>%                                      # Change X6 type to date
    mutate(X4 = as.numeric(gsub("[[:space:]]", "", X4))) %>%      # Remove spaces, then change X4 type to numeric 
    mutate(Konk. = ifelse(X8 == "Konkursåpning", 1, 0)) %>%       # Mark "Konkursåpning"
    rename("Navn" = X2, "Org.Nr" = X4, "Dato" = X6, "Type" = X8)  # Rename columns
  
  return(table)
}


# Takes in a data frame and region name, calculates cumulative sum for region
kumulativ <- function(df, region){
  output <- df %>%
    filter(Fylke == region, Type == "Konkursåpning") %>%
    arrange(Dato) %>%
    mutate(kumulativ = cumsum(Konk.))
  return(output)
}


tab1 <- create_table(web2019)
tab2 <- create_table(web2020)

# Use "kumulativ" function to calculate cumulative number for each individual region for 2019
Oslo <- kumulativ(tab1, "Oslo")
Viken <- kumulativ(tab1, "Viken")
Trøndelag <- kumulativ(tab1, "Trøndelag")
Rogaland <- kumulativ(tab1, "Rogaland")
Innlandet <- kumulativ(tab1, "Innlandet")
Troms_og_Finnmark <- kumulativ(tab1, "Troms_og_Finnmark")
Møre_og_Romsdal <- kumulativ(tab1, "Møre_og_Romsdal")
Vestfold_og_telemark <- kumulativ(tab1, "Vestfold_og_telemark")
Nordland <- kumulativ(tab1, "Nordland")
Agder <- kumulativ(tab1, "Agder")
Svalbard <- kumulativ(tab1, "Svalbard")
Vestland <- kumulativ(tab1, "Vestland")

fin_df <- bind_rows(Oslo, Viken, Trøndelag, Rogaland, Innlandet, Troms_og_Finnmark, Vestfold_og_telemark, Nordland, Agder, Svalbard, Vestland)
# Combine cumulative for all regions from 2019 data

# Use "kumulativ" function to calculate cumulative number for each individual region for 2020
Oslo <- kumulativ(tab2, "Oslo")
Viken <- kumulativ(tab2, "Viken")
Trøndelag <- kumulativ(tab2, "Trøndelag")
Rogaland <- kumulativ(tab2, "Rogaland")
Innlandet <- kumulativ(tab2, "Innlandet")
Troms_og_Finnmark <- kumulativ(tab2, "Troms_og_Finnmark")
Møre_og_Romsdal <- kumulativ(tab2, "Møre_og_Romsdal")
Vestfold_og_telemark <- kumulativ(tab2, "Vestfold_og_telemark")
Nordland <- kumulativ(tab2, "Nordland")
Agder <- kumulativ(tab2, "Agder")
Svalbard <- kumulativ(tab2, "Svalbard")
Vestland <- kumulativ(tab2, "Vestland")

fin_df2 <- bind_rows(Oslo, Viken, Trøndelag, Rogaland, Innlandet, Troms_og_Finnmark, Vestfold_og_telemark, Nordland, Agder, Svalbard, Vestland)
# Combine cumulative for all regions from 2020 data

# Remove year to improve plot readability
test1 <- fin_df %>%
  mutate(Dato = format.Date(Dato, "%m%d"))

test2 <- fin_df2 %>%
  mutate(Dato = format.Date(Dato, "%m%d"))

# Plot 2019 and 2020 data
ggplot() + 
  geom_line(data=test1, aes(x=Dato, y=kumulativ, group = Fylke), color='green') + 
  geom_line(data=test2, aes(x=Dato, y=kumulativ, group = Fylke), color='red') + 
  scale_x_discrete(breaks=as.numeric(seq(1, 7, 1))) + # Hack that stops x-axis from being filled with lines for each individual day
  ggtitle(""Commencement of liquidation proceedings" by region; 2019 (Green) VS 2020 (Red)") +
  ylab("Cumulative Amount") + xlab("Time period: 01.April - 30.August") + facet_wrap(~Fylke)























