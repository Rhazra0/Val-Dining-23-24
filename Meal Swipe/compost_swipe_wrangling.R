pacman::p_load(tidyverse, readxl, readr, knitr, ggplot2, dplyr)
library(lubridate)
library(xlsx)


#this semester swipes
swipes24 <- read.csv("meal_swipes_spring24.csv")
compost24 <- read.csv("compost_spring24.csv")

#Cleaning
swipes24 <- swipes24 |>
  select(-c(X, Valentine.Dining.Hall))

colnames(swipes24) <- c("date", "meal", "count" )

#Collapse swipes by week
swipes24 <- swipes24 |>
  mutate(date_adj = as.Date(date, format = "%m/%d/%Y")) 

week1 <- sum(swipes24$count[1:20])
week2 <- sum(swipes24$count[21:41])
week3 <- sum(swipes24$count[42:63])
week4 <- sum(swipes24$count[64:84])
week5 <- sum(swipes24$count[85:105])
week6 <- sum(swipes24$count[106:129])
week7 <- sum(swipes24$count[130:153])
week8 <- sum(swipes24$count[154:177])
week9 <- sum(swipes24$count[178:201])
week10 <- sum(swipes24$count[202:225])
week11 <- sum(swipes24$count[226:249])
week12 <- sum(swipes24$count[250:273])
week13 <- sum(swipes24$count[274:294])

#Bind with compost

compost24 <- compost24 |>
  mutate(Week = c(1:13)) |>
  mutate(count_sum = c(week1, week2, week3, week4, week5, week6, week7, week8, week9, week10, week11, week12, week13))|>
  mutate(pounds = Tons*2000)

#Pivot table
compost24_long <- compost24 |>
  pivot_longer(cols = c(pounds, count_sum), names_to = "variable", values_to = "values")

#Plot Line Graph
# Create the ggplot object
ggplot(compost24_long, aes(x = Week, y = values, color = variable)) +
  
  # Define data aesthetics
  geom_line()+
  # Customize plot
  labs(title = "Meal Swipes and Compost (lbs) By Week", 
       x = "Week", y = "Values", color = "Variable") +  # Add labels and title
  theme_classic()  # Set plot theme



#Oct'22 - Apr '24 swipes
swipes2224 <- read.csv("meal_swipes_22-24.csv")
compost2224 <- read.csv("compost_22-24.csv")

#Collapse swipes by month
swipes2224 <- swipes2224 |>
  mutate(date_adj = as.Date(Date, format = "%m/%d/%Y"))

swipes2224 <- swipes2224 |>
  mutate(year_month = substr(date_adj, 1, 7))

ymswipes <- swipes2224 |>
  group_by(year_month) |>
  summarise(average_count = mean(Count))

#take out April'24's data
ymswipes <- ymswipes[-19,]

#Bind with compost

compost2224 <- compost2224 |>
  mutate(count_sum = c(ymswipes$average_count)) |>
  mutate(pounds = Tons * 2000)

#Pivot table
compost2224_long <- compost2224 |>
  pivot_longer(cols = c(pounds, count_sum), names_to = "variable", values_to = "values") |>
  mutate(pounds = Tons * 2000)

#Plot Line Graph
# Create the ggplot object
ggplot(compost2224_long, aes(x = Month, y = values, color = variable)) +
  
  # Define data aesthetics
  geom_line()+
  # Customize plot
  labs(title = "Meal Swipes and Compost (lbs) By Month", 
       x = "Month", y = "Values", color = "Variable") +  # Add labels and title
  theme_classic()  # Set plot theme

write.xlsx(compost2224, file = "compost2224.xlsx")
write.xlsx(compost2224_long, file = "compost2224_long.xlsx")
write.xlsx(compost24, file = "compost24.xlsx")
