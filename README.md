# Clinical-Trial-Retrospective-study
Randomized control trial for prevention of post-pancreatic ERCP using Indomethacin 
library(dplyr)
library(tidyr)
library(medicaldata)
data("indo_rct")
View(indo_rct)

#Clean data#
columns_with_missing <- sum(sapply(indo_rct, function(x) any(is.na(x))))
print(columns_with_missing)

filtered_data <- indo_rct %>%
  select(-bleed) %>%
  filter(site == "1_UM", status == "1_outpatient")

View(filtered_data)

# Data Visualization #

library(ggplot2)
ggplot(filtered_data, aes(x = rx, fill = outcome)) +
  geom_bar() +
  theme_minimal()

#Statistics#
library(gtsummary)
ex_table <- filtered_data %>%
  select(rx, risk, age, gender, outcome, sod, pep, recpanc, difcan, prophystent, pdstent, bsphinc, train, type) %>%
  tbl_summary(
    by = outcome,
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ "{mean} ({sd})"
                     ),
    missing = "no"
  ) %>%
  add_overall() %>%
  modify_header(label = "**Variable**") %>%
  modify_spanning_header(c("stat_1","stat_2") ~ "**outcome**")
ex_table


# Association between two categorical variables #

chisq_result <- chisq.test(table(filtered_data$rx, filtered_data$outcome))
print(chisq_result)

#Logistic regression by adusting for covariates#

model <- glm(outcome ~ rx + risk, data = filtered_data, family = "binomial")

summary(model) 

#--------------------------------------------------------------#

indo_rct %>%
  count(status == "1_outpatient")

indo_rct %>%
  count(site == "2_IU")


#Data cleaning #

filtered_data2 <- indo_rct %>%
  select(-bleed) %>%
  filter(site == "2_IU", status == "1_outpatient")
View(filtered_data2)


#Data visualization#

library(ggplot2)

ggplot(filtered_data2, aes(x = rx, fill = outcome)) +
  geom_bar()

#Summary statistics#

library(gtsummary)

ex_table2 <- filtered_data2 %>%
  select(rx, risk, age, gender, outcome, sod, pep, recpanc, difcan, prophystent, pdstent, bsphinc, train, type) %>%
tbl_summary(
  by = outcome, 
  statistic = list(all_categorical() ~ "{n}({p}%)", 
                   all_continuous() ~ "{mean} ({sd})"),
  missing = "no"
) %>%
  add_overall() %>%
  modify_header(label = "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Outcome**")

ex_table2

# Chi square for categorical variables#

chisq_result2 <- chisq.test(table(filtered_data2$outcome, filtered_data2$rx))

print(chisq_result2)

# Linear regression with covariates #

model2 <- glm(outcome~rx + pep, data = filtered_data2, family = "binomial")

summary(model2)


# ------------------------------------------------------------#

#Data cleaning#

indo_rct2 <- indo_rct %>%
  select(-bleed)

#Data Visualization #

ggplot(indo_rct2, aes(x = rx, fill = outcome)) +
  geom_bar()

#Summary statistics#

ex_table3 <- indo_rct2 %>% 
  select(rx, risk, age, gender, outcome, sod, pep, recpanc, difcan, prophystent, pdstent, bsphinc, train, type) %>%
  tbl_summary(
    by = outcome,
    statistic = list(all_categorical() ~ "{n} ({p}%)", 
                     all_continuous() ~ "{mean} ({sd})"
                     ),
    missing = "no"
      ) %>%
      add_overall() %>%
      modify_header(label = "**Variable**") %>%
      modify_spanning_header(c("stat_1", "stat_2") ~ "**Outcome**")

ex_table3    


#Chisquare test for categorical variable#

chisq_result3 <- chisq.test(table(indo_rct2$outcome, indo_rct2$rx))
print(chisq_result3)

#linear regression model#

model3 <- glm(outcome ~ rx + pep, data = filtered_data2, family = "binomial") 


summary(model3)


