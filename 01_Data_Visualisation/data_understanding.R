# DATA UNDERSTANDING ----
# Libraries

library(tidyverse)
library(tidyquant)
library(readxl)
library(stringr)

# Load Data ----

path_train <- "00_Data/ts18individual14occupationsextaxableincomerange.xlsx"
train_raw_tbl <- read_excel(path_train, sheet = "Individual Table 14A", skip = 3)
path_age <- "00_Data/ts18individual13sexageresidencytaxassessmentrange.xlsx"
age_raw_tbl <- read_excel(path_age, sheet = "Individuals Table 13", skip = 2)

# Inspect Data
glimpse(train_raw_tbl)
glimpse(age_raw_tbl)

# Clean Data ----
trim_age_tbl <- age_raw_tbl %>%
  rename(
    age_range = `Age range3`,
    residency = `Residency status`,
    taxable_income_obs = `Taxable income or loss2\r\nno.`,
    taxable_income_au = `Taxable income or loss2\r\n$`,
    income_tot_au = `Total Income or Loss2\r\n$`,
    income_tot_nobs = `Total Income or Loss2\r\nno.`,
    salary_tot_au = `Salary or wages\r\n$`,
    salary_tot_nobs = `Salary or wages\r\nno.`,
    individuals_obs = `Number of individuals`,
    tax_bracket = `Tax assessment range`
         ) %>%
  select(
    individuals_obs,
    Sex,
    age_range,
    residency,
    taxable_income_obs,
    taxable_income_au,
    income_tot_au,
    income_tot_nobs,
    salary_tot_au,
    salary_tot_nobs,
    tax_bracket
  ) 

glimpse(trim_age_tbl)

trim_tbl <- train_raw_tbl %>%
  select(`Occupation - unit group1`:`Total Income or Loss3\r\n$`) %>%
  rename(
    taxable_income_obs = `Taxable income or loss3\r\nno.`,
    taxable_income_au = `Taxable income or loss3\r\n$`,
    income_tot_au = `Total Income or Loss3\r\n$`,
    income_tot_nobs = `Total Income or Loss3\r\nno.`,
    occupation_group = `Occupation - unit group1`,
    salary_tot_au = `Salary or wages\r\n$`,
    salary_tot_nobs = `Salary or wages\r\nno.`,
    individuals_obs = `Number of individuals`,
    tax_bracket = `Taxable income range - tax brackets`
  ) %>%
  select(
    occupation_group,
    individuals_obs,
    Sex,
    taxable_income_obs,
    taxable_income_au,
    income_tot_au,
    income_tot_nobs,
    salary_tot_au,
    salary_tot_nobs,
    tax_bracket
  ) 

glimpse(trim_tbl)

# 1. Prepare Data Tables ----

summary_tbl_120 <- trim_tbl %>%
  group_by(occupation_group, Sex) %>%
  summarize(individuals = sum(individuals_obs),
            ave_taxable_income = sum(taxable_income_au)/sum(taxable_income_obs),
            ave_total_income = sum(income_tot_au)/sum(income_tot_nobs),
            ave_salary = sum(salary_tot_au)/sum(salary_tot_nobs)
            ) %>%
  filter(ave_taxable_income > 120000) %>%
  arrange(desc(ave_taxable_income)) %>% 
  mutate(
       occupation_group = str_remove_all(occupation_group, pattern = "[0-9]"),
       n_text = str_c(format(individuals / 1e3, digits = 2), "k", sep = "")) 
summary_tbl_120 %>% glimpse()

summary_tbl_all <- trim_tbl %>%
  group_by(occupation_group, Sex) %>%
  summarize(individuals = sum(individuals_obs),
            ave_taxable_income = sum(taxable_income_au)/sum(taxable_income_obs),
            ave_total_income = sum(income_tot_au)/sum(income_tot_nobs),
            ave_salary = sum(salary_tot_au)/sum(salary_tot_nobs)
  ) %>%
  arrange(desc(ave_taxable_income)) %>% 
  mutate(
    occupation_group = str_remove_all(occupation_group, pattern = "[0-9]"),
    n_text = str_c(format(individuals / 1e3, digits = 2), "k", sep = "")) 
summary_tbl_low %>% glimpse()

summary_by_age_tbl <- trim_age_tbl %>%
  filter(residency != "Non-resident") %>%
  group_by(age_range, Sex) %>%
  summarize(individuals = sum(individuals_obs),
            ave_taxable_income = sum(taxable_income_au)/sum(taxable_income_obs),
            ave_total_income = sum(income_tot_au)/sum(income_tot_nobs),
            ave_salary = sum(salary_tot_au)/sum(salary_tot_nobs)
  ) %>%
  arrange(desc(ave_taxable_income)) %>% 
  mutate(
    age_range = str_remove_all(age_range, pattern = "[a-z]."),
    age_range = str_replace_all(age_range, pattern = "75", replacement = "75 +"),
    age_range = as_factor(age_range),
    Sex = as_factor(Sex),
    n_text = str_c(format(individuals / 1e3, digits = 2), "k", sep = "")) 
summary_by_age_tbl %>% glimpse()
summary_by_age_tbl %>% View()

  
# Visualisation of Income ----

  # Plotting
  
summary_tbl_120 %>%
  ggplot(aes(x = occupation_group, y = ave_salary, fill = Sex)) + 
  aes(reorder(occupation_group,desc(ave_salary)), ave_salary) +
  geom_col(position = "dodge2") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() +
  scale_size(range = c(3,5)) +
  labs(title = "Average Total Individual Salary 2017-18 - Australia",
       subtitle = "Grouped by Occupation - Sorted From Highest to Lowest",
       y = "Total Salary $A", x = "Occupation Group") +
  theme(legend.position = c(.8, .8)) +
  guides(x = guide_axis(angle = 45))
  
summary_tbl_120 %>%
  ggplot(aes(x = occupation_group, y = ave_taxable_income, fill = Sex)) + 
  aes(reorder(occupation_group,desc(ave_taxable_income)), ave_taxable_income) +
  geom_col(position = "dodge2") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() +
  scale_size(range = c(3,5)) +
  labs(title = "Average Taxable Income For Individuals in Australia - 2017-18",
       subtitle = "Grouped by Occupation - Sorted From Highest to Lowest",
       y = "Taxable Income - $A", x = "Occupation Group") +
  theme(legend.position = c(.8, .8)) +
  guides(x = guide_axis(angle = 45))

summary_tbl_all %>%
  ggplot(aes(x = occupation_group, y = ave_taxable_income, fill = Sex)) + 
  aes(reorder(occupation_group,ave_taxable_income), ave_taxable_income) +
  geom_col(position = "dodge2") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() +
  scale_size(range = c(3,5)) +
  labs(title = "Average Taxable Income For Individuals in Australia - 2017-18",
       subtitle = "Grouped by Occupation - Sorted From Lowest to Highest",
       y = "Taxable Income - $A", x = "") +
  theme(legend.position = c(.8, .8)) 

summary_by_age_tbl %>%
  ggplot(aes(x = age_range, y = ave_taxable_income, fill = Sex)) + 
  aes(reorder(age_range,desc(ave_taxable_income)), ave_taxable_income) +
  geom_col(position = "dodge2") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() +
  scale_size(range = c(3,5)) +
  scale_fill_tq() + 
  labs(title = "Average Taxable Income For Individuals in Australia - 2017-18",
       subtitle = "Grouped by Age Range - Sorted From Highest to Lowest",
       y = "Taxable Income - $A", x = "Age Group Range") +
  theme(legend.position = c(.8, .8)) 

  
