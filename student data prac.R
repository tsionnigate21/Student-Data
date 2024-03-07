library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

setwd('~/Documents/DATA/Data 332/student data')
#reading 
df_student_data  <- read_excel('~/Documents/DATA/Data 332/student data/Student.xlsx',.name_repair = 'universal')
df_registration_data <- read_excel('~/Documents/DATA/Data 332/student data/Registration.xlsx',.name_repair = 'universal')
df_course_data <- read_excel('~/Documents/DATA/Data 332/student data/Course.xlsx',.name_repair = 'universal')

#Left Join student data
df <- df_student_data %>%
  left_join(df_registration_data, by = "Student.ID") %>%
  left_join(df_course_data, by = "Instance.ID")


# major counts charting 
major_counts <- df %>%
  group_by(Title) %>%
  summarise(count = n())

# major count bar plot 
ggplot(major_counts, aes(x = Title, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Students per Major", x = "Major", y = "Number of Students") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Chart on the birth year of the student

df$Birth.Year <- as.integer(str_sub(df$Birth.Date, 1, 4))
Birthdate_of_student <- df %>%
  group_by(Birth.Year) %>%
  summarize(studentPerYear = n())

ggplot(Birthdate_of_student, aes(x = Birth.Year, y = studentPerYear)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Number of Students per Birth Year",
       x = "Birth Year",
       y = "Number of Students") +
  theme_minimal()

#Total cost per major, segment by payment plan
cost_per_major <- df %>%
  group_by(Title, Payment.Plan) %>%
  summarize(Total_Cost = sum(Total.Cost))

ggplot(df, aes(x = Title, y = Total.Cost, fill = Payment.Plan)) +
  geom_bar(stat = "identity") +  # Remove position = "dodge"
  labs(title = "Total Cost per Major Segmented by Payment Plan",
       x = "Major",
       y = "Total Cost",
       fill = "Payment Plan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Total balance due by major, segment by payment plan
balancedue_by_major <- df %>%
  group_by(Title, Payment.Plan) %>%
  summarize(Total_Balance_Due = sum(Balance.Due))


ggplot(df, aes(x = Title, y = Balance.Due, fill = Payment.Plan)) +
  geom_bar(stat = "identity") +  # Remove position = "dodge"
  labs(title = "Total Balance Due by Major Segmented by Payment Plan",
       x = "Major",
       y = "Total Balance Due",
       fill = "Payment Plan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Chart number of students with balance over 500$
students_over_500 <- df %>%
  filter(Balance.Due > 500)

ggplot(students_over_500, aes(x = Balance.Due)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = " Payment Due for Students with Balance Over $500",
       x = "Balance Due",
       y = "Number of Students") +
  theme_minimal()

# Total hours per major 
total_hour_per_major <- df %>%
  group_by(Title) %>%
  summarise(Total_Hours = sum(Hours.Per.Week))

# Create a bar plot for total hours per major
ggplot(total_hour_per_major, aes(x = Title, y = Total_Hours, fill = Title)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Hours per Major",
       x = "Major",
       y = "Total Hours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
