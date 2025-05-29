## Ben Taylor
### ECON 453
#### PS2
##### 02/26/2025

# Clear environment, console, and plot pane
rm(list = ls())
cat("\014")
graphics.off()

# Turn off scientific notation
options(scipen = 999)

# Load packages
pacman::p_load(data.table)
library("readxl")
library(gt)
library(dplyr)


# Set seed
set.seed(418518)

# Set working directory 
setwd("C:/Users/ual-laptop/Desktop/Data")

#Extract Data for both questions
data <- read_xlsx("pset2_data.xlsx", sheet =2)
data_2 <- read_xlsx("pset2_data.xlsx", sheet =1)

### not necessary
mean_score <- mean(data$score)
print(mean_score)

#### QUESTION 1.1

ScoresTest <- t.test(data$score)
CI <- ScoresTest$conf.int
print(CI)

## Question 1.2 
TTest <- t.test(data$score, mu = 63)
print(TTest)
p <- (TTest$p.value)
print(p)


## Question 1.3
Kennedy_Data <- data %>% filter(school == "Kennedy")
Adams_Data <- data %>% filter(school == "Adams")

Kennedy_Scores <- Kennedy_Data$score
Adams_Scores <- Adams_Data$score

T_Test <- t.test(Kennedy_Scores, Adams_Scores)
print(T_Test)


### Question 2.1 

library(gt)
Bama_Data <- data_2 %>% filter(University == "University of Alabama")

summary_table <- Bama_Data %>%
  summarise(
    Min = min(Sale_amount, na.rm = TRUE),
    Max = max(Sale_amount, na.rm = TRUE),
    Mean = mean(Sale_amount, na.rm = TRUE),
    `Std Dev` = sd(Sale_amount, na.rm = TRUE),
    Count = n()
  )

# Create and display the table using gt
summary_table %>%
  gt() %>%
  tab_header(title = "University of Alabama Housing Sale Price Summary Statistics")


## 2.2
Price_Test <- t.test(Bama_Data$Sale_amount)
CI <- Price_Test$conf.int
print(CI)

## 2.3

smallhomes <- data_2 %>% filter(Sqft_home < 2000)
bighomes <- data_2 %>% filter(Sqft_home > 2000)
smallprice <- smallhomes$Sale_amount
bigprice <- bighomes$Sale_amount
T_Test <- t.test(smallprice, bigprice)
print(T_Test)


smallbamahomes <- Bama_Data %>% filter(Sqft_home < 2000)
bigbamahomes <- Bama_Data %>% filter(Sqft_home > 2000)
smallprice <- smallbamahomes$Sale_amount
bigprice <- bigbamahomes$Sale_amount
T_Test <- t.test(smallprice, bigprice)
print(T_Test)

