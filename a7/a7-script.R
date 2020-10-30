library(tidyverse)

# Extract data from files year by year, assuming unzipped text files are available
df_2017 <- data.frame(read_fwf("a7/BED-a7-data/Nat2017PublicUS.c20180516.r20180808.txt", fwf_cols(birth_month = c(13, 14), sex = 475, weight = c(504, 507), day_of_week = 23, year = c(8, 12))))                                                                                      
df_2018 <- data.frame(read_fwf("a7/BED-a7-data/Nat2018PublicUS.c20190509.r20190717.txt", fwf_cols(birth_month = c(13, 14), sex = 475, weight = c(504, 507), day_of_week = 23, year = c(8, 12)))) 
df_2019 <- data.frame(read_fwf("a7/BED-a7-data/Nat2019PublicUS.c20200506.r20200915.txt", fwf_cols(birth_month = c(13, 14), sex = 475, weight = c(504, 507), day_of_week = 23, year = c(8, 12))))

# Fix data types
df_17 <- df_2017 %>%
  mutate(birth_month = as.numeric(birth_month), weight = as.numeric(weight))

df_18 <- df_2018 %>%
  mutate(birth_month = as.numeric(birth_month), weight = as.numeric(weight))

df_19 <- df_2019 %>%
  mutate(birth_month = as.numeric(birth_month), weight = as.numeric(weight))


# function to get average weight for specific sex in a data frame
average_weight <- function(df, char) {
  out <- df %>%
    filter(sex == char) %>%     # sort by sex
    group_by(birth_month) %>%   
    filter(weight < 9999) %>%   # ignore no-data
    mutate(average = mean(weight))
  return(out)
}


# average weight month by month, by gender, per year
df_M <- average_weight(df_17, "M")
df_F <- average_weight(df_17, "F")
df_final_17 <- bind_rows(df_M, df_F)

df_M <- average_weight(df_18, "M")
df_F <- average_weight(df_18, "F")
df_final_18 <- bind_rows(df_M, df_F)

df_M <- average_weight(df_19, "M")
df_F <- average_weight(df_19, "F")
df_final_19 <- bind_rows(df_M, df_F)

# function to generate plot given a data frame and year
plot_weight_func <- function(df, char) {
  df %>%
    ggplot(aes(x=birth_month, y=average, group = sex)) +
    geom_line(aes(color=sex)) +
    scale_x_continuous(name="Month", breaks=(seq(1, 12, 1)), limits = c(1, 12)) +
    expand_limits(y = c(3100, 3400)) +  # Ensures y-axis is similar between different plots, without removing any data
    ylab(expression("Weight in grams")) +
    labs(title = "Average birth weight",
         subtitle = char)
}

#plot_weight_func(df_final_17, "2017")

plot_pro_func <- function(df, char) {
  df %>%
    ggplot(aes(x = birth_month, y = pro)) + 
    geom_line() +
    scale_x_continuous(name="Month", breaks=(seq(1, 12, 1)), limits = c(1, 12)) +
    expand_limits(y = c(0.940, 0.970)) +  # Ensures y-axis is similar between different plots, without removing any data
    ylab(expression("")) +
    labs(title = "Number of Girls per Boy",
         subtitle = char)
}

#plot_pro_func(test, "2017")

# function to find proportion between F/M in dataframe
proportion_sex <- function(df) {
  # count total number of occurrences per sex
  tbl <- df %>%
    group_by(birth_month) %>%
    count(sex)
  
  # find proportion between F (n[1]) and M (n[2])
  out <- tbl %>%
    group_by(birth_month) %>%
    mutate(pro = n[1]/n[2]) %>%
    select(birth_month, pro) %>%    # keep only month and proportion
    filter(row_number() %% 2 != 0)  # remove duplicate rows
  
  return(out)
}

#test <- proportion_sex(df_17)

proportion_sex_week <- function(df) {
  # count total number of occurrences per sex
  tbl <- df %>%
    group_by(birth_month, day_of_week) %>%
    count(sex)
  
  # find proportion between F (n[1]) and M (n[2])
  out <- tbl %>%
    group_by(birth_month, day_of_week) %>%
    mutate(pro = n[1]/n[2]) %>%
    select(birth_month, pro) %>%    # keep only month and proportion
    filter(row_number() %% 2 != 0)  # remove duplicate rows
  
  return(out)
}


#test <- proportion_sex_week(df_final_17)
#test

plot_proWeek_func <- function(df, char) {
  
  df %>%
    ggplot(aes(x = birth_month, y = pro, group = day_of_week)) + 
    geom_line(aes(color = factor(day_of_week))) +
    scale_x_continuous(name="Month", breaks=(seq(1, 12, 1)), limits = c(1, 12)) +
    expand_limits(y = c(0.940, 0.970)) +  # Ensures y-axis is similar between different plots, without removing any data
    ylab(expression("")) +
    scale_color_discrete(name = "Day of the Week", labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +                                                   
    labs(title = "Number of Girls per Boy",
         subtitle = char)
}

#plot_proWeek_func(test, "2017")



