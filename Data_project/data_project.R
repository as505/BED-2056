library(tidyverse)
# Org dataset
df_test <- read.csv2("C:\\Users\\as505\\Desktop\\BED-1234\\Data_project\\696902851_mdt_2018_2020.csv", check.names = FALSE)
df_test <- read.csv2("./Data_project\\696902851_mdt_2018_2020.csv", check.names = FALSE)

getwd()
list.dirs()
list.files("./Data_project")

# Nytt datasett fra 161 stasjoner (Tromsø, Bergen, Oslo)
df_test <- read.csv2("./Data_project//full_mdt_2018_2020.csv", check.names = FALSE)


colnames(df_test)

tmp <- df_test%>%
  filter(Felt == "Totalt") %>%
  select(Navn, År, Måned, `Antall døgn total`, `Antall døgn ugyldig`, "<5,6m" = `< 5,6m`, ">=5,6m" = `>= 5,6m`, "5,6m-7,6m" = `5,6m - 7,6m`, "7,6m-12,5m" = `7,6m - 12,5m`, "12,5m-16,0m" = `12,5m - 16,0m`, "16,0m-24,0m" = `16,0m - 24,0m`, ">=24,0m" = `>= 24,0m`) %>%                           
  mutate(År = as.factor(År))
  
tmp
colnames(tmp)

df_2018 <- tmp %>%
  filter(År == 2018) %>%
  group_by(Måned) %>%
  mutate(Av1 = mean(`<5,6m`)) %>%
  mutate(Av2 = mean(`>=5,6m`)) %>%
  mutate(Av3 = mean(`5,6m-7,6m`)) %>%
  mutate(Av4 = mean(`7,6m-12,5m`)) %>%
  mutate(Av5 = mean(`12,5m-16,0m`)) %>%
  mutate(Av6 = mean(`16,0m-24,0m`)) %>%
  mutate(Av7 = mean(`>=24,0m`))

df_2019 <- tmp %>%
  filter(År == 2019) %>%
  group_by(Måned) %>%
  mutate(Av1 = mean(`<5,6m`)) %>%
  mutate(Av2 = mean(`>=5,6m`)) %>%
  mutate(Av3 = mean(`5,6m-7,6m`)) %>%
  mutate(Av4 = mean(`7,6m-12,5m`)) %>%
  mutate(Av5 = mean(`12,5m-16,0m`)) %>%
  mutate(Av6 = mean(`16,0m-24,0m`)) %>%
  mutate(Av7 = mean(`>=24,0m`))

df_2020 <- tmp %>%
  filter(År == 2020) %>%
  group_by(Måned) %>%
  mutate(Av1 = mean(`<5,6m`)) %>%
  mutate(Av2 = mean(`>=5,6m`)) %>%
  mutate(Av3 = mean(`5,6m-7,6m`)) %>%
  mutate(Av4 = mean(`7,6m-12,5m`)) %>%
  mutate(Av5 = mean(`12,5m-16,0m`)) %>%
  mutate(Av6 = mean(`16,0m-24,0m`)) %>%
  mutate(Av7 = mean(`>=24,0m`))
  
df_2018 %>%
  ggplot() +
  geom_line(aes(x = Måned, y = Av1)) +
  geom_line(aes(x = Måned, y = Av2)) +
  geom_line(aes(x = Måned, y = Av3)) +
  geom_line(aes(x = Måned, y = Av4)) +
  geom_line(aes(x = Måned, y = Av5)) +
  geom_line(aes(x = Måned, y = Av6)) +
  geom_line(aes(x = Måned, y = Av7)) +
  scale_x_continuous(breaks = df_2018$Måned, name = "Måned")

test <- bind_rows(df_2018, df_2019, df_2020)
test2 <- bind_rows(df_2019, df_2020)
test3 <- bind_rows(df_2018, df_2019)

test %>%
  ggplot() +
  geom_line(aes(x = Måned, y = Av1, group = År, color = År)) +
  scale_x_continuous(breaks=test$Måned, name = "Month") +
  labs(y = "Vehicles counted", title = "Vehicles < 5,6m in length", color = "Year")

test %>%
  ggplot() +
  geom_line(aes(x = Måned, y = Av2, group = År, color = År)) +
  geom_line(aes(x = Måned, y = Av3, group = År, color = År)) +
  geom_line(aes(x = Måned, y = Av4, group = År, color = År)) +
  geom_line(aes(x = Måned, y = Av5, group = År, color = År)) +
  geom_line(aes(x = Måned, y = Av6, group = År, color = År)) +
  geom_line(aes(x = Måned, y = Av7, group = År, color = År)) +
  scale_x_continuous(breaks=test$Måned, name = "Måned")

mult_diff <- function(Input_df){
  output <- Input_df %>%
    select(Måned, År, 13:19) %>%
    filter(Måned <= 11) %>%
    group_by(Måned) %>%
    mutate(Differ = Av1 - lag(Av1)) %>%
    unique() %>%
    mutate(Differ = abs(Differ)) %>%
    mutate(Differ2 = Av2 - lag(Av2)) %>%
    unique() %>%
    mutate(Differ2 = abs(Differ2)) %>%
    mutate(Differ3 = Av3 - lag(Av3)) %>%
    unique() %>%
    mutate(Differ3 = abs(Differ3)) %>%
    mutate(Differ4 = Av4 - lag(Av4)) %>%
    unique() %>%
    mutate(Differ4 = abs(Differ4)) %>%
    mutate(Differ5 = Av5 - lag(Av5)) %>%
    unique() %>%
    mutate(Differ5 = abs(Differ5)) %>%
    mutate(Differ6 = Av6 - lag(Av6)) %>%
    unique() %>%
    mutate(Differ6 = abs(Differ6)) %>%
    mutate(Differ7 = Av7 - lag(Av7)) %>%
    unique() %>%
    mutate(Differ7 = abs(Differ7)) %>%
    filter(Differ > 0, Differ2 > 0, Differ3 > 0, Differ4 > 0, Differ5 > 0, Differ6 > 0, Differ7 > 0)
  
    return(output)
}

mult_diff_2 <- function(Input_df){
  output <- Input_df %>%
    filter(Måned <= 11) %>%
    group_by(Måned) %>%
    mutate(Differ = Differ - lag(Differ)) %>%
    unique() %>%
    mutate(Differ = abs(Differ)) %>%
    mutate(Differ2 = Differ2 - lag(Differ2)) %>%
    unique() %>%
    mutate(Differ2 = abs(Differ2)) %>%
    mutate(Differ3 = Differ3 - lag(Differ3)) %>%
    unique() %>%
    mutate(Differ3 = abs(Differ3)) %>%
    mutate(Differ4 = Differ4 - lag(Differ4)) %>%
    unique() %>%
    mutate(Differ4 = abs(Differ4)) %>%
    mutate(Differ5 = Differ5 - lag(Differ5)) %>%
    unique() %>%
    mutate(Differ5 = abs(Differ5)) %>%
    mutate(Differ6 = Differ6 - lag(Differ6)) %>%
    unique() %>%
    mutate(Differ6 = abs(Differ6)) %>%
    mutate(Differ7 = Differ7 - lag(Differ7)) %>%
    unique() %>%
    mutate(Differ7 = abs(Differ7)) %>%
    filter(Differ > 0, Differ2 > 0, Differ3 > 0, Differ4 > 0, Differ5 > 0, Differ6 > 0, Differ7 > 0)
  
  return(output)
}

diff <- mult_diff(test2)
diff2 <- mult_diff(test3)

# diff <- test2 %>%
#   select(Måned, År, Av1) %>%
#   filter(Måned <= 10) %>%
#   unique() %>%
#   group_by(Måned) %>%
#   mutate(Differ = Av1 - lag(Av1)) %>%
#   mutate(Differ = abs(Differ)) %>%
#   filter(Differ >= 0)
# 
# 
# diff2 <- test3 %>%
#   select(Måned, År, Av1) %>%
#   filter(Måned <= 10) %>%
#   unique() %>%
#   group_by(Måned) %>%
#   mutate(Differ = Av1 - lag(Av1)) %>%
#   mutate(Differ = abs(Differ)) %>%
#   filter(Differ >= 0)

diff_plot <- bind_rows(diff2, diff)
diff_test_2 <- mult_diff_2(diff_plot)

################################################################################
# Plot difference between 2020/2019 with difference between 2019/2018
################################################################################

ggplot() +
  geom_line(data = diff_plot, aes(x = Måned, y = Differ, group = År, color = År)) +
  scale_x_continuous(breaks=diff$Måned, name = "Måned")

ggplot() +
  geom_line(data = diff_plot, aes(x = Måned, y = Differ2, group = År, color = År)) +
  scale_x_continuous(breaks=diff$Måned, name = "Måned")

ggplot() +
  geom_line(data = diff_plot, aes(x = Måned, y = Differ3, group = År, color = År)) +
  scale_x_continuous(breaks=diff$Måned, name = "Måned")

ggplot() +
  geom_line(data = diff_plot, aes(x = Måned, y = Differ4, group = År, color = År)) +
  scale_x_continuous(breaks=diff$Måned, name = "Måned")

ggplot() +
  geom_line(data = diff_plot, aes(x = Måned, y = Differ5, group = År, color = År)) +
  scale_x_continuous(breaks=diff$Måned, name = "Måned")

ggplot() +
  geom_line(data = diff_plot, aes(x = Måned, y = Differ6, group = År, color = År)) +
  scale_x_continuous(breaks=diff$Måned, name = "Måned")

ggplot() +
  geom_line(data = diff_plot, aes(x = Måned, y = Differ7, group = År, color = År)) +
  scale_x_continuous(breaks=diff$Måned, name = "Måned")

######################################################################################
# Plot how/where 2019-2020 change differs from 2018-2019
######################################################################################

ggplot() +
  geom_line(data = diff_test_2, aes(x = Måned, y = Differ, group = År, color = År)) +
  scale_x_continuous(breaks=diff$Måned, name = "Måned")

ggplot() +
  geom_line(data = diff_test_2, aes(x = Måned, y = Differ2, group = År, color = År)) +
  scale_x_continuous(breaks=diff$Måned, name = "Måned")

ggplot() +
  geom_line(data = diff_test_2, aes(x = Måned, y = Differ3, group = År, color = År)) +
  scale_x_continuous(breaks=diff$Måned, name = "Måned")

ggplot() +
  geom_line(data = diff_test_2, aes(x = Måned, y = Differ4, group = År, color = År)) +
  scale_x_continuous(breaks=diff$Måned, name = "Måned")

ggplot() +
  geom_line(data = diff_test_2, aes(x = Måned, y = Differ5, group = År, color = År)) +
  scale_x_continuous(breaks=diff$Måned, name = "Måned")

ggplot() +
  geom_line(data = diff_test_2, aes(x = Måned, y = Differ6, group = År, color = År)) +
  scale_x_continuous(breaks=diff$Måned, name = "Måned")

ggplot() +
  geom_line(data = diff_test_2, aes(x = Måned, y = Differ7, group = År, color = År)) +
  scale_x_continuous(breaks=diff$Måned, name = "Måned")




  
            