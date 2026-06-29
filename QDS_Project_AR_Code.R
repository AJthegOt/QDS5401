setwd("C:/Users/User2/Documents/MPH/QDS_Code")
set.seed(5401)

library(tableone)
library(dplyr)
library(ggplot2)
library(flextable)
library(tidyr)



#Read data
data <- read.csv("QDS_Data.csv")

#Filter data
filtered_data <- 
  data %>% 
  select(sex,
         race,
         school_location,
         school_type,
         grade_group,
         anxiety_score_pre,
         anxiety_score_post,
         anxiety_change)

#Only include cases where anxiety_change is complete
filtered_data <- filtered_data %>%
  filter(!is.na(anxiety_change))

#Check structure
str(filtered_data)

#Code our categoricals
filtered_data$sex <- factor(filtered_data$sex)
filtered_data$race <- factor(filtered_data$race,
                             levels = c("American Indian or Alaska Native",
                                        "Asian",
                                        "Black or African American",
                                        "Native Hawaiian or Pacific Islander",
                                        "White",
                                        "More than one race",
                                        "Other",
                                        "Unknown",
                                        "Declined or not reported"))
filtered_data$school_location <- factor(filtered_data$school_location)
filtered_data$school_type <- factor(filtered_data$school_type)
filtered_data$grade_group <- factor(filtered_data$grade_group)

#Check skewness
ggplot(filtered_data, 
       mapping = aes(x = anxiety_score_pre)) + 
  geom_histogram()
#appears right-skewed

ggplot(filtered_data, 
       mapping = aes(x = anxiety_score_post)) + 
  geom_histogram()

#right-skewed

ggplot(filtered_data, 
       mapping = aes(x = anxiety_change)) + 
  geom_histogram()

#symmetrical




#Create table one
table_one <- CreateTableOne(data = filtered_data,
               vars = names(filtered_data))


test <- print(table_one, 
      nonnormal = c("anxiety_score_pre", "anxiety_score_post"),
      cramVars = "grade_group")



#Creation of the line graph for pre-post scoring

#LLM Prompt: arrange my data so that I can make a pre-post line graph for before and after anxiety scores

anxiety_summary <- filtered_data %>%
  select(race, anxiety_score_pre, anxiety_score_post) %>%
  pivot_longer(
    cols = c(anxiety_score_pre, anxiety_score_post),
    names_to = "Time",
    values_to = "Anxiety"
  ) %>%
  group_by(race, Time) %>%
  summarise(
    Median = median(Anxiety, na.rm = TRUE),
    .groups = "drop"
  )

anxiety_summary$Time <- factor(
  anxiety_summary$Time,
  levels = c("anxiety_score_pre", "anxiety_score_post")
)


# Line graph
ggplot(anxiety_summary, aes(x = Time, y = Median, group = race, color = race)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    x = "Time Questionnaire Was Taken",
    y = "Median Anxiety Score",
    title = "Change in Pre/Post Anxiety Scores"
  ) +
  theme_classic()


#bar plot
#LLM code: need to adjust the x-axis labels so that they are at an angle for readability purposes.
ggplot(filtered_data, mapping = aes(x = race, y = anxiety_change, color = race)) +
  geom_boxplot() +
  labs(
    x = "Racial Group",
    y = "Change in Anxiety Score (Post-Pre)",
    title = "Change in Pre/Post Anxiety Scores"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



