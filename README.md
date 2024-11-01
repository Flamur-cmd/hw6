Flamur Kukaj,Abdullah Al Aman,Alicia persad Marvin Harricharran

# Setup: Load necessary libraries and data
library(ggplot2)
library(tidyverse)
library(haven)
library(codebookr)
library(plotly)

# Set working directory and load dataset
("/Users/Fkukaj000/Desktop/B2000/acs2021_recoded.RData")

# Calculate age difference and income difference
acs2021_couples$age_diff <- acs2021_couples$AGE - acs2021_couples$h_age
acs2021_couples$income_diff <- acs2021_couples$INCWAGE - acs2021_couples$h_incwage

# Recode education levels to numeric for partner and head
acs2021_couples$educ_numeric <- fct_recode(acs2021_couples$EDUC,
                                           "0" = "N/A or no schooling",
                                           "2" = "Nursery school to grade 4",
                                           "6.5" = "Grade 5, 6, 7, or 8",
                                           "9" = "Grade 9",
                                           "10" = "Grade 10",
                                           "11" = "Grade 11",
                                           "12" = "Grade 12",
                                           "13" = "1 year of college",
                                           "14" = "2 years of college",
                                           "15" = "3 years of college",
                                           "16" = "4 years of college",
                                           "17" = "5+ years of college") %>%
  as.numeric()

acs2021_couples$h_educ_numeric <- fct_recode(acs2021_couples$h_educ,
                                             "0" = "N/A or no schooling",
                                             "2" = "Nursery school to grade 4",
                                             "6.5" = "Grade 5, 6, 7, or 8",
                                             "9" = "Grade 9",
                                             "10" = "Grade 10",
                                             "11" = "Grade 11",
                                             "12" = "Grade 12",
                                             "13" = "1 year of college",
                                             "14" = "2 years of college",
                                             "15" = "3 years of college",
                                             "16" = "4 years of college",
                                             "17" = "5+ years of college") %>%
  as.numeric()

# Calculate education difference
acs2021_couples$educ_diff <- acs2021_couples$educ_numeric - acs2021_couples$h_educ_numeric

# Filter for working-age population (ages 25-55) who are in the labor force
acs_subgroup <- acs2021_couples %>%
  filter(AGE >= 25 & AGE <= 55 & LABFORCE == 2 & WKSWORK2 > 4 & UHRSWORK >= 35)

# Summary of age, income, and education differences by gender
summary(acs_subgroup$income_diff[acs_subgroup$SEX == "Female"])
summary(acs_subgroup$income_diff[acs_subgroup$SEX == "Male"])
summary(acs_subgroup$educ_diff[acs_subgroup$SEX == "Female"])
summary(acs_subgroup$educ_diff[acs_subgroup$SEX == "Male"])

# Regression model (based on Lab6)
model_1 <- lm(educ_numeric ~ AGE + h_educ_numeric, data = acs_subgroup)
model_asked <- lm(age_diff ~ educ_diff + educ_numeric + h_educ_numeric + poly(AGE, 2) + poly(h_age, 2), data = acs_subgroup)
summary(model_asked)

# Visual representation of age difference vs education difference
a1 <- ggplot(acs_subgroup, aes(x = educ_diff, y = age_diff)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Age Difference vs Education Difference",
       x = "Education Difference (years)",
       y = "Age Difference (years)") +
  theme_minimal()

fun <- ggplotly(a1)
fun

# Hypothesis test: Proportion of significant age differences
total_observations <- nrow(acs_subgroup)
significant_age_diff <- sum(abs(acs_subgroup$age_diff) > 5)  # Assuming age difference > 5 years is significant

# Proportion and hypothesis test
prop_significant <- significant_age_diff / total_observations
H0 <- "The proportion of couples with significant age difference is 0.5"
HA <- "The proportion of couples with significant age difference is not 0.5"

# Proportion test
prop_test <- prop.test(significant_age_diff, total_observations, p = 0.5, alternative = "two.sided", conf.level = 0.95)

# Extract and display key statistics
estimate <- prop_test$estimate
standard_error <- sqrt(estimate * (1 - estimate) / total_observations)
z_stat <- (estimate - 0.5) / standard_error
p_value <- prop_test$p.value

print(paste("Proportion of significant age differences:", estimate))
print(paste("Standard Error:", standard_error))
print(paste("Z-statistic:", z_stat))
print(paste("p-value:", p_value))

# Interpretation of the test
if (p_value < 0.05) {
  print("Result: Reject null hypothesis")
  print("The proportion of couples with significant age difference is not 0.5")
} else {
  print("Result: Fail to reject null hypothesis")
  print("There is not enough evidence to conclude that the proportion of couples with significant age difference differs from 0.5")
}

# Confidence interval for the proportion
conf_int <- prop_test$conf.int
print(paste("95% Confidence Interval:", round(conf_int[1], 4), "to", round(conf_int[2], 4)))

im doing avaition with marvin as my final project 

Summary of an Article on Data Accessibility and Methods that explores the usage of big data analytics in the aviation sector, emphasizing the utilization of data from diverse origins such as air traffic control systems, passenger databases, and airline activities. The writers emphasize that many of this information can be obtained by collaborating with government agencies and private sector groups. They utilize machine learning methods to examine patterns in flight delays, fuel usage, and passenger actions. The research focuses on how predictive analytics can enhance operational efficiency and customer experience in the aviation industry, with the goal of lowering expenses and enhancing service quality.

Utilizing Big Data to Improve Safety in Aviation

Summary of the article focuses on examining how big data can improve safety protocols in the aviation sector through econometric methods and research inquiries. They make use of an extensive dataset containing past incident reports, maintenance logs, and current sensor data from planes. Industry partnerships and regulatory bodies provide access to this information. The writers use methods like time-series analysis and risk assessment models to assess how effective safety interventions are. The inquiries aim to pinpoint main risk factors linked to aviation incidents and show how data analysis can help improve safety measures, ultimately reducing accidents in air travel.



