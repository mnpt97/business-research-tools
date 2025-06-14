rm(list = ls())

library(dplyr)


df <- read.csv("./data/data_SoSe25_mba_class_final.csv", sep = ";", header = TRUE)


# Sanitize dataframe column names:
names(df)[names(df) == "years.of.education"] <- "years_of_education"
names(df)[names(df) == "employment.status"] <- "employment_status"
names(df)[names(df) == "industry.sector"] <- "industry_sector"
names(df)[names(df) == "gross.salary.year"] <- "gross_salary_year"
names(df)[names(df) == "gross.salary.month"] <- "gross_salary_month"
names(df)[names(df) == "subjective.health"] <- "subjective_health"
names(df)[names(df) == "subjective.life.satisfaction.happiness"] <- "subjective_life_satisfaction"

#### Sanitize malformed / unusual values: ####
# Replace [10] completely satisfied in column "subjective_life_satisfaction" string value 10 and convert the whole column to numeric
df[df$subjective_life_satisfaction == "[10] completely satisfied", "subjective_life_satisfaction"] <- "10"
df$subjective_life_satisfaction <- as.numeric(df$subjective_life_satisfaction)

# Replace all fields with a monthly salary equal to 0 with 1/12 of yearly salary
condition_month_salary <- (!is.na(df$gross_salary_year) & df$gross_salary_year > 0 & df$gross_salary_year / 12 > df$gross_salary_month)
df[condition_month_salary, "gross_salary_month"] <- df[condition_month_salary, "gross_salary_year"] / 12

# Remove all rows where children >= pers
df <- df[df$pers > df$children, ]



mean_to_per <- function(value){
    return(round(mean(value, na.rm = TRUE), 3) * 100)
}

df_info <- df %>%
    reframe(
        Metric = c(
            "Female", 
            "Male", 
            "Age", 
            "People in Household",
            "Household with Children",
            "Single Parent Household",
            "Years of Education",
            "Gross Salary Year",
            "Gross Salay Month",
            "Employed Full-Time",
            "Employed Part-Time",
            "Training Apprenticeship",
            "Irregular employment or in marginal",
            "Unemployed",
            "Working in Agriculture",
            "Working in Manufacturing",
            "Working in Energy",
            "Working in Constructions",
            "Working in Trade",
            "Working in Traffic",
            "Working in Gastronomy & Hotel",
            "Working in Telecommunication",
            "Working in Services",
            "Working in Public Administration, Education and Health"
        ),
        "n" = c(
            sum(female, na.rm=TRUE), 
            sum(!female, na.rm = TRUE),
            sum(!is.na(age), na.rm = TRUE),
            sum(!is.na(pers), na.rm = TRUE),
            sum(!is.na(pers) & !is.na(children) & children > 0, na.rm=TRUE),
            sum(!is.na(pers) & !is.na(children) & children > 0 & pers-children == 1, na.rm=TRUE),
            sum(!is.na(years_of_education)),
            sum(!is.na(gross_salary_year)),
            sum(!is.na(gross_salary_month)),
            sum(!is.na(employment_status) & employment_status == 1),
            sum(!is.na(employment_status) & employment_status == 2),
            sum(!is.na(employment_status) & employment_status == 3),
            sum(!is.na(employment_status) & employment_status == 4),
            sum(!is.na(employment_status) & employment_status == 5),
            sum(!is.na(industry_sector) & industry_sector == 1),
            sum(!is.na(industry_sector) & industry_sector == 2),
            sum(!is.na(industry_sector) & industry_sector == 3),
            sum(!is.na(industry_sector) & industry_sector == 4),
            sum(!is.na(industry_sector) & industry_sector == 5),
            sum(!is.na(industry_sector) & industry_sector == 6),
            sum(!is.na(industry_sector) & industry_sector == 7),
            sum(!is.na(industry_sector) & industry_sector == 8),
            sum(!is.na(industry_sector) & industry_sector == 9),
            sum(!is.na(industry_sector) & industry_sector == 10)
        ),
        "%" = c(
            mean_to_per(female),
            mean_to_per(!female),
            NA,
            NA,
            mean_to_per(!is.na(pers) & !is.na(children) & children > 0),
            mean_to_per(!is.na(pers) & !is.na(children) & children > 0 & pers-children == 1),
            NA,
            NA,
            NA,
            mean_to_per(!is.na(employment_status) & employment_status == 1),
            mean_to_per(!is.na(employment_status) & employment_status == 2),
            mean_to_per(!is.na(employment_status) & employment_status == 3),
            mean_to_per(!is.na(employment_status) & employment_status == 4),
            mean_to_per(!is.na(employment_status) & employment_status == 5),
            mean_to_per(!is.na(industry_sector) & industry_sector == 1),
            mean_to_per(!is.na(industry_sector) & industry_sector == 2),
            mean_to_per(!is.na(industry_sector) & industry_sector == 3),
            mean_to_per(!is.na(industry_sector) & industry_sector == 4),
            mean_to_per(!is.na(industry_sector) & industry_sector == 5),
            mean_to_per(!is.na(industry_sector) & industry_sector == 6),
            mean_to_per(!is.na(industry_sector) & industry_sector == 7),
            mean_to_per(!is.na(industry_sector) & industry_sector == 8),
            mean_to_per(!is.na(industry_sector) & industry_sector == 9),
            mean_to_per(!is.na(industry_sector) & industry_sector == 10)
        ),
        "Mean" = c(
            mean(female, na.rm = TRUE),
            mean(!female, na.rm = TRUE),
            mean(age, na.rm = TRUE),
            mean(pers, na.rm=TRUE),
            mean(!is.na(pers) & !is.na(children) & children > 0, na.rm=TRUE),
            mean(!is.na(pers) & !is.na(children) & children > 0 & pers-children == 1, na.rm=TRUE),
            mean(years_of_education, na.rm=TRUE),
            mean(gross_salary_year, na.rm=TRUE),
            mean(gross_salary_month, na.rm=TRUE),
            mean(!is.na(employment_status) & employment_status == 1),
            mean(!is.na(employment_status) & employment_status == 2),
            mean(!is.na(employment_status) & employment_status == 3),
            mean(!is.na(employment_status) & employment_status == 4),
            mean(!is.na(employment_status) & employment_status == 5),
            mean(!is.na(industry_sector) & industry_sector == 1),
            mean(!is.na(industry_sector) & industry_sector == 2),
            mean(!is.na(industry_sector) & industry_sector == 3),
            mean(!is.na(industry_sector) & industry_sector == 4),
            mean(!is.na(industry_sector) & industry_sector == 5),
            mean(!is.na(industry_sector) & industry_sector == 6),
            mean(!is.na(industry_sector) & industry_sector == 7),
            mean(!is.na(industry_sector) & industry_sector == 8),
            mean(!is.na(industry_sector) & industry_sector == 9),
            mean(!is.na(industry_sector) & industry_sector == 10)
        ),
        "Min/Max" = c(
            NA,
            NA,
            paste(min(age, na.rm=TRUE), max(age, na.rm = TRUE), sep = "/"),
            paste(min(pers, na.rm=TRUE), max(pers, na.rm = TRUE), sep = "/"),
            NA,
            NA,
            paste(min(years_of_education, na.rm=TRUE), max(years_of_education, na.rm = TRUE), sep = "/"),
            paste(min(gross_salary_year, na.rm=TRUE), max(gross_salary_year, na.rm = TRUE), sep = "/"),
            paste(min(gross_salary_month, na.rm=TRUE), max(gross_salary_month, na.rm = TRUE), sep = "/"),
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA
        )
    )

write.csv(df_info, file = "./tables/df_info")



# Focus on working population in age range 18 - 65 or people who are employed
df_working_population <- df %>%
    filter((!is.na(age) & age > 18 & age <= 65)
        & (!is.na(employment_status) & employment_status != 5))

df_female_working_population <- df_working_population %>%
    filter(!is.na(female) & female)

df_male_working_population <- df_working_population %>%
    filter(!is.na(female) & !female)

salary_summary_by_gender <- df_working_population %>%
    group_by(female) %>%
    summarise(
        "n" = n(),
        "Mean Salary year" = mean(gross_salary_year, na.rm = TRUE),
        "Median Salary year" = median(gross_salary_year, na.rm = TRUE),
        "Mean Salary month" = mean(gross_salary_month, na.rm = TRUE),
        "Median Salary month" = median(gross_salary_month, na.rm = TRUE)
    )

education_summary_by_gender <- df_working_population %>%
    filter(!is.na(years_of_education)) %>%
    group_by(female) %>%
    summarise(
        "n" = n(),
        "Mean (years)" = mean(years_of_education, na.rm = TRUE),
        "Median (years)" = median(years_of_education, na.rm = TRUE),
    )


total_n_sector <- nrow(df_working_population %>% filter(!is.na(industry_sector)))

industry_sector_by_gender <- df_working_population %>%
    filter(!is.na(industry_sector)) %>%
    group_by(industry_sector) %>%
    summarise(
        "n" = n(),
        "Sector (%)" = round(n() / total_n_sector * 100, 1),
        "Male (%)" = round(mean(!female),3) * 100,
        "Female (%)" = round(mean(female),3) * 100,
        "Median yearly salary" = round(median(gross_salary_month, na.rm = TRUE)),
        "Median yearly salary male" = round(median(gross_salary_month[!female], na.rm = TRUE)),
        "Median yearly salary female" = round(median(gross_salary_month[female], na.rm = TRUE)),
    )
print("industry sectors")
print(industry_sector_by_gender)


industry_sector_by_gender_and_employment_status <- df_working_population %>%
    filter(!is.na(industry_sector) & !is.na(employment_status)) %>%
    group_by(industry_sector) %>%
    summarise(
        "Sector (%)" = round(n() / total_n_sector * 100, 1),
        "Female years of education by sector" = round(median(years_of_education[female], na.rm=TRUE)),
        "Male years of education by sector" = round(median(years_of_education[!female], na.rm = TRUE)),
        "Female median salary full-time" = round(median(gross_salary_year[female & employment_status == 1], na.rm=TRUE)),
        "Male median salary full-time" = round(median(gross_salary_year[!female & employment_status == 1], na.rm=TRUE)),
        "Female median age" = round(median(age[female & employment_status == 1], na.rm=TRUE)),
        "Male median age" = round(median(age[!female & employment_status == 1], na.rm=TRUE)),
    )


# Calculate spearman rho corelation between employment_status and gross_salary_month
cor_employment_status_salary <- cor(
    rank(df_working_population$employment_status),
    rank(df_working_population$gross_salary_month),
    use = "complete.obs", method = "spearman")

cor_satisfaction_salary <- cor(
    rank(df_working_population$subjective_life_satisfaction),
    rank(df_working_population$gross_salary_month),
    use = "complete.obs", method = "spearman")

total_n_employment_status <- nrow(df_working_population %>% filter(!is.na(employment_status)))
employment_status_by_gender <- df_working_population %>%
    filter(!is.na(employment_status)) %>%
    group_by(employment_status) %>%
    summarise(
        "n" = n(),
        "Employment Status (%)" = round(n() / total_n_employment_status * 100, 1),
        "Male (n)" = sum(!female),
        "Female (n)" = sum(female),
        "Male (%)" = round(mean(!female),3) * 100,
        "Female (%)" = round(mean(female),3) * 100,
    )


df_single_parents_working_population <- df %>%
  filter(!is.na(female), !is.na(children), !is.na(pers)) %>%
  filter(children > 0, pers - children == 1)

gender_single_parents_counts <- df_single_parents_working_population %>%
  count(female)

gender_single_parents_percentages <- gender_single_parents_counts %>%
  mutate(percentage = n / sum(n) * 100)

calculate_correlations <- function(){
    cor_employment_satisfaction <- cor(df_working_population$employment_status, df_working_population$subjective_life_satisfaction, use = "complete.obs")
    cor_salary_satisfaction <- cor(df_working_population$gross_salary_month, df_working_population$subjective_life_satisfaction, use = "complete.obs")
    cor_salary_education <- cor(df_working_population$gross_salary_month, df_working_population$years_of_education, use = "complete.obs")
    cor_children_satisfaction <- cor(df_working_population$children, df_working_population$subjective_life_satisfaction, use = "complete.obs" )
    cor_children_education <- cor(df_working_population$children, df_working_population$years_of_education, use = "complete.obs" )

    cor_female_salary_education <- cor(df_female_working_population$gross_salary_year, df_female_working_population$years_of_education, use="complete.obs")
    cor_male_salary_education <- cor(df_male_working_population$gross_salary_year, df_male_working_population$years_of_education, use="complete.obs")
}

# Calucalate some important correlations

median_female_salary_yearly <- median(df_female_working_population$gross_salary_year, rm.na = TRUE)
median_male_salary_yearly <- median(df_male_working_population$gross_salary_year, rm.na = TRUE)
median_female_education <- median(df_female_working_population$years_of_education, rm.na = TRUE)

# Gender pay-gap analyse
# Convert categorical variables to factors
df_working_population$female <- as.factor(as.numeric(df_working_population$female))
df_working_population$employment_status <- as.factor(df_working_population$employment_status)

df_working_population <- df_working_population %>%
    mutate(
        # Create a new temporary column with string labels
        employment_status_string = case_when(
            employment_status == 1 ~ "Full-Time",
            employment_status == 2 ~ "Part-Time",
            employment_status == 3 ~ "Training or Apprenticeship",
            employment_status == 4 ~ "Irregular or Marginal",
            TRUE ~ as.character(employment_status)
        ),
        industry_sector_string = case_when(
            industry_sector == 2 ~ "Manufacturing",
            industry_sector == 5 ~ "Trade",
            industry_sector == 9 ~ "Services",
            industry_sector == 10 ~ "Administration, Education & Health",
            industry_sector %in% c(1, 3, 4, 6, 7, 8) ~ "Others",
            TRUE ~ as.character(industry_sector)
        )
    )
desired_industry_levels <- c(
    "Manufacturing",
    "Trade",
    "Services",
    "Administration, Education & Health",
    "Others"
)

desired_employment_status <- c(
    "Full-Time",
    "Part-Time",
    "Training or Apprenticeship",
    "Irregular or Marginal"
)

df_working_population$industry_sector <- factor(
  df_working_population$industry_sector_string,
  levels = desired_industry_levels 
)

df_working_population$employment_status <- factor(
    df_working_population$employment_status_string,
    levels = desired_employment_status
)

df_working_population$industry_sector_string <- NULL
df_working_population$employment_status_string <- NULL
df_working_population$industry_sector <- as.factor(df_working_population$industry_sector)
df_working_population$employment_status <- as.factor(df_working_population$employment_status)
df_working_population$industry_sector <- relevel(df_working_population$industry_sector, ref="Administration, Education & Health")
df_working_population$employment_status <- relevel(df_working_population$employment_status, ref="Full-Time")


df_summary_by_sector <- df_working_population %>%
    reframe(
        tibble(
            Metric = c(
                "Persons [n]",
                "Females [%]",
                "Median education [years]",
                "Minimum education [years]",
                "Maximum education [years]",
                "Median male education [years]",
                "Median female education [years]",
                "Median age [years]",
                "Minimum age [years]",
                "Maximum age [years]",
                "Males employed full-time [%]",
                "Females employed full-time [%]",
                "Males employed part-time [%]",
                "Females employed part-time [%]",
                "Males employed irregular/marginal [%]",
                "Females employed irregular/marginal [%]"
            ),
            "Manufacturing" = c(
                sum(industry_sector == "Manufacturing", na.rm = TRUE),
                round(
                    (sum(industry_sector == "Manufacturing" & female == 1, na.rm = TRUE) /
                     sum(industry_sector == "Manufacturing", na.rm = TRUE)) * 100,
                    2
                ),
                median(years_of_education[industry_sector == "Manufacturing"], na.rm = TRUE),
                min(years_of_education[industry_sector == "Manufacturing"], na.rm = TRUE),
                max(years_of_education[industry_sector == "Manufacturing"], na.rm = TRUE),
                median(years_of_education[industry_sector == "Manufacturing" & female == 0], na.rm = TRUE),
                median(years_of_education[industry_sector == "Manufacturing" & female == 1], na.rm = TRUE),
                median(age[industry_sector == "Manufacturing"], na.rm = TRUE),
                min(age[industry_sector == "Manufacturing"], na.rm = TRUE),
                max(age[industry_sector == "Manufacturing"], na.rm = TRUE),
                round(
                    (sum(industry_sector == "Manufacturing" & female == 0 & employment_status == "Full-Time", na.rm = TRUE) /
                     sum(industry_sector == "Manufacturing" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Manufacturing" & female == 1 & employment_status == "Full-Time", na.rm = TRUE) /
                     sum(industry_sector == "Manufacturing" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Manufacturing" & female == 0 & employment_status == "Part-Time", na.rm = TRUE) /
                     sum(industry_sector == "Manufacturing" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Manufacturing" & female == 1 & employment_status == "Part-Time", na.rm = TRUE) /
                     sum(industry_sector == "Manufacturing" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Manufacturing" & female == 0 & employment_status == "Irregular or Marginal", na.rm = TRUE) /
                     sum(industry_sector == "Manufacturing" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Manufacturing" & female == 1 & employment_status == "Irregular or Marginal", na.rm = TRUE) /
                     sum(industry_sector == "Manufacturing" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                )

            ),
            # You'll need to apply this same correction to all other industry sectors
            "Trade" = c(
                sum(industry_sector == "Trade", na.rm = TRUE),
                round(
                    (sum(industry_sector == "Trade" & female == 1, na.rm = TRUE) /
                     sum(industry_sector == "Trade", na.rm = TRUE)) * 100, 2
                ),
                median(years_of_education[industry_sector == "Trade"], na.rm = TRUE),
                min(years_of_education[industry_sector == "Trade"], na.rm = TRUE),
                max(years_of_education[industry_sector == "Trade"], na.rm = TRUE),
                median(years_of_education[industry_sector == "Trade" & female == 0], na.rm = TRUE),
                median(years_of_education[industry_sector == "Trade" & female == 1], na.rm = TRUE),
                median(age[industry_sector == "Trade"], na.rm = TRUE),
                min(age[industry_sector == "Trade"], na.rm = TRUE),
                max(age[industry_sector == "Trade"], na.rm = TRUE),
                round(
                    (sum(industry_sector == "Trade" & female == 0 & employment_status == "Full-Time", na.rm = TRUE) /
                     sum(industry_sector == "Trade" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Trade" & female == 1 & employment_status == "Full-Time", na.rm = TRUE) /
                     sum(industry_sector == "Trade" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Trade" & female == 0 & employment_status == "Part-Time", na.rm = TRUE) /
                     sum(industry_sector == "Trade" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Trade" & female == 1 & employment_status == "Part-Time", na.rm = TRUE) /
                     sum(industry_sector == "Trade" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Trade" & female == 0 & employment_status == "Irregular or Marginal", na.rm = TRUE) /
                     sum(industry_sector == "Trade" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Trade" & female == 1 & employment_status == "Irregular or Marginal", na.rm = TRUE) /
                     sum(industry_sector == "Trade" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                )

            ),
            "Services" = c(
                sum(industry_sector == "Services", na.rm = TRUE),
                round(
                    (sum(industry_sector == "Services" & female == 1, na.rm = TRUE) /
                     sum(industry_sector == "Services", na.rm = TRUE)) * 100, 2
                ),
                median(years_of_education[industry_sector == "Services"], na.rm = TRUE),
                min(years_of_education[industry_sector == "Services"], na.rm = TRUE),
                max(years_of_education[industry_sector == "Services"], na.rm = TRUE),
                median(years_of_education[industry_sector == "Services" & female == 0], na.rm = TRUE),
                median(years_of_education[industry_sector == "Services" & female == 1], na.rm = TRUE),
                median(age[industry_sector == "Services"], na.rm = TRUE),
                min(age[industry_sector == "Services"], na.rm = TRUE),
                max(age[industry_sector == "Services"], na.rm = TRUE),
                round(
                    (sum(industry_sector == "Services" & female == 0 & employment_status == "Full-Time", na.rm = TRUE) /
                     sum(industry_sector == "Services" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Services" & female == 1 & employment_status == "Full-Time", na.rm = TRUE) /
                     sum(industry_sector == "Services" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Services" & female == 0 & employment_status == "Part-Time", na.rm = TRUE) /
                     sum(industry_sector == "Services" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Services" & female == 1 & employment_status == "Part-Time", na.rm = TRUE) /
                     sum(industry_sector == "Services" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Services" & female == 0 & employment_status == "Irregular or Marginal", na.rm = TRUE) /
                     sum(industry_sector == "Services" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Services" & female == 1 & employment_status == "Irregular or Marginal", na.rm = TRUE) /
                     sum(industry_sector == "Services" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                )

            ),
            `Administration, Education & Health` = c(
                sum(industry_sector == "Administration, Education & Health", na.rm = TRUE),
                round(
                    (sum(industry_sector == "Administration, Education & Health" & female == 1, na.rm = TRUE) /
                     sum(industry_sector == "Administration, Education & Health", na.rm = TRUE)) * 100, 2
                ),
                median(years_of_education[industry_sector == "Administration, Education & Health"], na.rm = TRUE),
                min(years_of_education[industry_sector == "Administration, Education & Health"], na.rm = TRUE),
                max(years_of_education[industry_sector == "Administration, Education & Health"], na.rm = TRUE),
                median(years_of_education[industry_sector == "Administration, Education & Health" & female == 0], na.rm = TRUE),
                median(years_of_education[industry_sector == "Administration, Education & Health" & female == 1], na.rm = TRUE),
                median(age[industry_sector == "Administration, Education & Health"], na.rm = TRUE),
                min(age[industry_sector == "Administration, Education & Health"], na.rm = TRUE),
                max(age[industry_sector == "Administration, Education & Health"], na.rm = TRUE),
                round(
                    (sum(industry_sector == "Administration, Education & Health" & female == 0 & employment_status == "Full-Time", na.rm = TRUE) /
                     sum(industry_sector == "Administration, Education & Health" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Administration, Education & Health" & female == 1 & employment_status == "Full-Time", na.rm = TRUE) /
                     sum(industry_sector == "Administration, Education & Health" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Administration, Education & Health" & female == 0 & employment_status == "Part-Time", na.rm = TRUE) /
                     sum(industry_sector == "Administration, Education & Health" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Administration, Education & Health" & female == 1 & employment_status == "Part-Time", na.rm = TRUE) /
                     sum(industry_sector == "Administration, Education & Health" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Administration, Education & Health" & female == 0 & employment_status == "Irregular or Marginal", na.rm = TRUE) /
                     sum(industry_sector == "Administration, Education & Health" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Administration, Education & Health" & female == 1 & employment_status == "Irregular or Marginal", na.rm = TRUE) /
                     sum(industry_sector == "Administration, Education & Health" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                )
            ),
            "Others" = c(
                sum(industry_sector == "Others", na.rm = TRUE),
                round(
                    (sum(industry_sector == "Others" & female == 1, na.rm = TRUE) /
                     sum(industry_sector == "Others", na.rm = TRUE)) * 100, 2
                ),
                median(years_of_education[industry_sector == "Others"], na.rm = TRUE),
                min(years_of_education[industry_sector == "Others"], na.rm = TRUE),
                max(years_of_education[industry_sector == "Others"], na.rm = TRUE),
                median(years_of_education[industry_sector == "Others" & female == 0], na.rm = TRUE),
                median(years_of_education[industry_sector == "Others" & female == 1], na.rm = TRUE),
                median(age[industry_sector == "Others"], na.rm = TRUE),
                min(age[industry_sector == "Others"], na.rm = TRUE),
                max(age[industry_sector == "Others"], na.rm = TRUE),
                round(
                    (sum(industry_sector == "Others" & female == 0 & employment_status == "Full-Time", na.rm = TRUE) /
                     sum(industry_sector == "Others" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Others" & female == 1 & employment_status == "Full-Time", na.rm = TRUE) /
                     sum(industry_sector == "Others" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Others" & female == 0 & employment_status == "Part-Time", na.rm = TRUE) /
                     sum(industry_sector == "Others" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Others" & female == 1 & employment_status == "Part-Time", na.rm = TRUE) /
                     sum(industry_sector == "Others" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Others" & female == 0 & employment_status == "Irregular or Marginal", na.rm = TRUE) /
                     sum(industry_sector == "Others" & female == 0, na.rm = TRUE)) * 100, 
                    2 
                ),
                round(
                    (sum(industry_sector == "Others" & female == 1 & employment_status == "Irregular or Marginal", na.rm = TRUE) /
                     sum(industry_sector == "Others" & female == 1, na.rm = TRUE)) * 100, 
                    2 
                )

            )
        )
    )


df_working_salary_month <- df_working_population %>%
    filter(!is.na(gross_salary_month) & gross_salary_month > 0)

df_working_salary_year <- df_working_population %>%
    filter(!is.na(gross_salary_year) & gross_salary_year > 0)

df_working_salary_year_ln <- df_working_salary_year %>%
    mutate(gross_salary_year_ln = log(gross_salary_year))

hist_from_df_column <- function(df, col_to_plot){
    png(filename = paste0("./histograms/histogram_", col_to_plot, ".png"), width = 800, height = 600)
    par(cex.lab = 1.5, cex.axis = 1.2)

    hist(df[[col_to_plot]],
        main = paste("Histogram of", col_to_plot),
        xlab = col_to_plot,
        ylab = "Frequency",
        col = "skyblue",
        border = "black",
        breaks = 15) 
    grid()
    par(cex.lab = 1, cex.axis = 1)

    dev.off()
}

hist_from_df_column(df_working_salary_year_ln, "gross_salary_year_ln")
hist_from_df_column(df_working_salary_month, "gross_salary_month")





# ------------------------------------ Analyse  Skewness ------------------------------------ #
library(e1071)
# Check skewness in salary distributions
skewness_value_salary_month <- skewness(df_working_salary_month$gross_salary_month, na.rm = TRUE)
cat("Skewness of Gross Monthly Salary:", skewness_value_salary_month, "\n")
skewness_value_salary_year <- skewness(df_working_population$gross_salary_year, na.rm = TRUE)
cat("Skewness of Gross Yearly Salary:", skewness_value_salary_year, "\n")

# ----------------------------------- Quantile Regressions ---------------------------------- #
library(quantreg)

# Define quantiles
taus_all <- seq(from = .05, to = .95, by = 0.05)

# ------------------------------ Plot 5%-quantile coefficience ------------------------------ #


# Define the directory to save the PNG files (optional)
output_dir <- "quantile_plots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

library(ggplot2)

create_monthly_coefficient_plots <- function(){
    model_qreg_all_month <- rq(gross_salary_month ~ female + age + years_of_education + employment_status + industry_sector,
                   tau = taus_all,
                   data = df_working_population)
    ## Create and save Years of Education coefficient plot
    png(filename = file.path(output_dir, "female_coefficient_month.png"), width = 600, height = 600)
    summary(model_qreg_all_month) |> plot("female1", main = "Coefficient for Female (monthly)",
                                ylab = "Estimated Coefficient (€) (monthly)")
    grid()
    dev.off()

    ## Create and save Years of Education coefficient plot
    png(filename = file.path(output_dir, "years_of_education_coefficient_month.png"), width = 600, height = 600)
    summary(model_qreg_all_month) |> plot("years_of_education", main = "Coefficient for Years of Education (monthly)",
                                ylab = "Estimated Coefficient (€) (monthly)")
    grid()
    dev.off()

    # Create and save Age coefficient plot
    png(filename = file.path(output_dir, "age_coefficient_month.png"), width = 600, height = 500)
    summary(model_qreg_all_month) |> plot("age", main = "Coefficient for Age (monthly)",
                                    ylab = "Estimated Coefficient (€) (monthly)")
    grid()
    dev.off()
}


create_yearly_coefficient_plots <- function(){
    model_qreg_all_year <- rq(gross_salary_year ~ female + age + years_of_education + employment_status + industry_sector,
                   tau = taus_all,
                   data = df_working_salary_year)
    ## Create and save Years of Education coefficient plot
    png(filename = file.path(output_dir, "female_coefficient_year.png"), width = 600, height = 600)
    summary(model_qreg_all_year) |> plot("female1", main = "Coefficient for Female (yearly)",
                                ylab = "Estimated Coefficient (€) (yearly)")
    grid()
    dev.off()

    ## Create and save Years of Education coefficient plot
    png(filename = file.path(output_dir, "years_of_education_coefficient_year.png"), width = 600, height = 600)
    summary(model_qreg_all_year) |> plot("years_of_education", main = "Coefficient for Years of Education (yearly)",
                                ylab = "Estimated Coefficient (€) (yearly)")
    grid()
    dev.off()

    # Create and save Age coefficient plot
    png(filename = file.path(output_dir, "age_coefficient_year.png"), width = 600, height = 500)
    summary(model_qreg_all_year) |> plot("age", main = "Coefficient for Age (yearly)",
                                    ylab = "Estimated Coefficient (€) (yearly)")
    grid()
    dev.off()
}

#create_monthly_coefficient_plots()
#create_yearly_coefficient_plots()



# ------------------------------ Plot 5%-quantile coefficience ------------------------------ #

create_quantile_summary <- function(){
    # Calculate mean/mode for comparisons
    mean_age_baseline <- mean(df_working_population$age, na.rm = TRUE)
    mean_education_baseline <- mean(df_working_population$years_of_education, na.rm = TRUE)
    mode_employment <- names(which.max(table(df_working_population$employment_status)))
    mode_industry <- names(which.max(table(df_working_population$industry_sector)))

    # Matrix to store the percentage increase per year of education/age and gender pay gap for each 
    #   tau and monthly or yearly gross salary
    summary_matrix <- matrix(NA, nrow = length(taus_all), ncol = 6)
    rownames(summary_matrix) <- paste0("tau ", taus_all)
    colnames(summary_matrix) <- c(
        "Gender pay gap (%) [monthly]", 
        "Salary increase per additional year of education (%) [monthly]", 
        "Salary increase per additional year of age (%) [monthly]",
        "Gender pay gap (%) [yearly]", 
        "Salary increase per additional year of education (%) [yearly]", 
        "Salary increase per additional year of age (%) [yearly]"
    )

    for (i in 1:length(taus_all)) {
        tau_val <- taus_all[i]

        # Fit the quantile regression model for the current tau in taus_all (each 5% quantile)
        model_qreg_month <- rq(gross_salary_month ~ female + age + years_of_education + employment_status + industry_sector,
                        tau = tau_val,
                        data = df_working_salary_month)

        model_qreg_year <- rq(gross_salary_year ~ female + age + years_of_education + employment_status + industry_sector,
                        tau = tau_val,
                        data = df_working_salary_year)

        # Create a "average" working person
        typical_individual_base <- data.frame(
            female = factor(names(which.max(table(df_working_population$female))), levels = c(0, 1)),
            age = mean_age_baseline,
            years_of_education = mean_education_baseline,
            employment_status = factor(mode_employment, levels = levels(df_working_population$employment_status)),
            industry_sector = factor(mode_industry, levels = levels(df_working_population$industry_sector))
        )

        # Create data frame for the same individual with one more year of education
        typical_individual_plus_one_edu <- typical_individual_base
        typical_individual_plus_one_edu$years_of_education <- mean_education_baseline + 1

        # Create a df for the same individual one year older
        typical_individual_plus_one_age <- typical_individual_base
        typical_individual_plus_one_age$age <- mean_age_baseline + 1

        # Predict the gross monthly/yearly salary for both scenarios at the current tau
        predicted_salary_base_month <- predict(model_qreg_month, newdata = typical_individual_base)
        predicted_salary_plus_one_edu_month <- predict(model_qreg_month, newdata = typical_individual_plus_one_edu)
        predicated_salary_plus_one_age_month <- predict(model_qreg_month, newdata = typical_individual_plus_one_age)

        predicted_salary_base_year <- predict(model_qreg_year, newdata = typical_individual_base)
        predicted_salary_plus_one_edu_year <- predict(model_qreg_year, newdata = typical_individual_plus_one_edu)
        predicated_salary_plus_one_age_year <- predict(model_qreg_year, newdata = typical_individual_plus_one_age)

        # Calculate relative difference and add to summary matrix
        percentage_increase_edu_month <- ((predicted_salary_plus_one_edu_month - predicted_salary_base_month) / predicted_salary_base_month) * 100
        percentage_increase_age_month <- ((predicated_salary_plus_one_age_month - predicted_salary_base_month) / predicted_salary_base_month) * 100
        summary_matrix[i, 2] <- percentage_increase_edu_month
        summary_matrix[i, 3] <- percentage_increase_age_month

        percentage_increase_edu_year <- ((predicted_salary_plus_one_edu_year - predicted_salary_base_year) / predicted_salary_base_year) * 100
        percentage_increase_age_year <- ((predicated_salary_plus_one_age_year - predicted_salary_base_year) / predicted_salary_base_year) * 100
        summary_matrix[i, 5] <- percentage_increase_edu_year
        summary_matrix[i, 6] <- percentage_increase_age_year

        # --- Calculate Relative Gender Pay Gap ---
        # Create data frames for a "average"/"typical" male and female
        typical_male <- data.frame(
            female = factor(0, levels = c(0, 1)),
            age = mean_age_baseline,
            years_of_education = mean_education_baseline,
            employment_status = factor(mode_employment, levels = levels(df_working_population$employment_status)),
            industry_sector = factor(mode_industry, levels = levels(df_working_population$industry_sector))
        )
        typical_female <- typical_male
        typical_female$female <- factor(1, levels = c(0, 1))

        # Predict the gross monthly/yearly salary for the typical male and female at the current tau
        predicted_salary_male_month <- predict(model_qreg_month, newdata = typical_male)
        predicted_salary_female_month <- predict(model_qreg_month, newdata = typical_female)

        predicted_salary_male_year <- predict(model_qreg_year, newdata = typical_male)
        predicted_salary_female_year <- predict(model_qreg_year, newdata = typical_female)

        # Calculate the percentage gender pay gap
        percentage_gap_gender_month <- ((predicted_salary_female_month - predicted_salary_male_month) / predicted_salary_male_month) * 100
        percentage_gap_gender_year <- ((predicted_salary_female_year - predicted_salary_male_year) / predicted_salary_male_year) * 100
        summary_matrix[i, 1] <- abs(percentage_gap_gender_month)
        summary_matrix[i, 4] <- abs(percentage_gap_gender_year)
    }
    return(summary_matrix)
}

prediction_summary <- create_quantile_summary()
#
## Print the results
#print("Summary at Different Taus:")
#print(prediction_summary)


# ------------------ Quantile Regression to analyze

taus_for_analysis <- c(0.1,0.2,0.3, 0.4, 0.5, 0.6,0.7,0.8,0.9)


# --- Crucial: Ensure these factor conversions run BEFORE your QR model fitting loop ---

#df_working_salary_year <- df_working_salary_year %>%
#    filter(!is.na(employment_status) & employment_status == "Full-Time")
df_working_salary_year$female <- factor(df_working_salary_year$female, levels = c(0, 1), labels = c("Male", "Female"))
#df_working_salary_year$employment_status <- factor(df_working_salary_year$employment_status)
df_working_salary_year$industry_sector <- factor(df_working_salary_year$industry_sector)
# -----------------------------------------------------------------------------------

# --- Rest of your code for setting up prediction_data variables ---
typical_age <- 30#round(mean(df_working_salary_year$age), 0)

# FIX 1: Use a valid employment_status level, e.g., "1" or the most frequent one
# (If you want the most frequent, uncomment this line instead)
# typical_employment_status <- names(sort(table(df_working_salary_year$employment_status), decreasing = TRUE)[1])
typical_employment_status <- "Full-Time" # Assuming "1" is a representative employment status. Choose a level from [1] "1" "2" "3" "4"



all_industries <- levels(df_working_salary_year$industry_sector)
education_levels <- c(7, 12, 18)
age_levels <- c(
    quantile(df_working_salary_year$age, probs = 0.25, na.rm = TRUE), 
    quantile(df_working_salary_year$age, probs = 0.5, na.rm = TRUE), 
    quantile(df_working_salary_year$age, probs = 0.75, na.rm = TRUE))
print(age_levels)
# ------------------------------------------------------------------

# Create the new dataframe for predictions
prediction_data <- expand.grid(
  female = factor(c("Male", "Female"), levels = levels(df_working_salary_year$female)),
  age = typical_age,
  years_of_education = education_levels,
  employment_status = factor(typical_employment_status),
  industry_sector = factor(all_industries, levels = levels(df_working_salary_year$industry_sector))
)

# --- Your loop for fitting and predicting QR models ---
predicted_salaries_list <- list()
for (t in taus_for_analysis) {
    model_qr_single_tau <- rq(gross_salary_year ~ female + age + years_of_education + industry_sector + female:employment_status + 
                                female:industry_sector + female:years_of_education + female:age + industry_sector:years_of_education,
                                tau = t, data = df_working_salary_year)

    

    preds <- predict(model_qr_single_tau, newdata = prediction_data)

    predicted_salaries_list[[as.character(t)]] <- cbind(prediction_data, Predicted_Salary = preds, Tau = t)
}

# Combine all predictions into a single dataframe
predicted_salaries_df <- do.call(rbind, predicted_salaries_list)

# Calculate the gender pay gap (Female - Male) for each scenario
gender_gap_df <- predicted_salaries_df %>%
  group_by(Tau, industry_sector, years_of_education, age, employment_status) %>%
  summarise(
    Male_Salary = Predicted_Salary[female == "Male"],
    Female_Salary = Predicted_Salary[female == "Female"],
    Gender_Gap = Female_Salary - Male_Salary,
    Relative_Gender_Gap = ifelse(Male_Salary != 0, (Gender_Gap / Male_Salary) * 100, NA),

    .groups = 'drop'
  )

print(head(gender_gap_df))

# Now, plot Gender_Gap for various scenarios
# For example, plot Gender_Gap vs. Tau, faceted by industry_sector and/or years_of_education

# Example plot for a specific industry (e.g., 'Finance') across education levels
plt<-ggplot(filter(gender_gap_df, industry_sector == "Services"), aes(x = Tau, y = Gender_Gap, color = factor(years_of_education))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  labs(
    title = "Gender Pay Gap in Trade Sector Across Salary Quantiles",
    subtitle = paste0("At age ", typical_age, " and employment status '", typical_employment_status, "'"),
    x = "Quantile (tau)",
    y = "Female - Male Salary Difference (€)",
    color = "Years of Education"
  ) +
  theme(
    # Plot Title
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5), # Increased size and centered
    # Plot Subtitle
    plot.subtitle = element_text(size = 14, hjust = 0.5), # Increased size and centered

    # Axis Labels (titles)
    axis.title.x = element_text(size = 16, margin = margin(t = 10)), # Increased size, added top margin
    axis.title.y = element_text(size = 16, margin = margin(r = 10)), # Increased size, added right margin

    # Axis Ticks (numbers/labels on the axes)
    axis.text.x = element_text(size = 14), # Increased size for X-axis ticks
    axis.text.y = element_text(size = 14), # Increased size for Y-axis ticks

    # Legend
    legend.title = element_text(face = "bold", size = 14), # Increased size and bold for legend title
    legend.text = element_text(size = 12), # Increased size for legend items
    legend.position = "right", # Ensures legend is visible on the right
    legend.background = element_rect(fill = "grey70", color = NA)
  )

ggsave("gender_gap_plot_reduced_height.png", plot = plt, width = 8, height = 5, units = "in", dpi = 300)
print(head(gender_gap_df))

label_mapping <- c(
  "Administration, Education & Health" = "Administration,\nEducation & Health",
  "Manufacturing" = "Manufacturing",
  "Trade" = "Trade",
  "Services" = "Services",
  "Others" = "Others"
)

gender_gap_df <- gender_gap_df %>%
  mutate(industry_sector = factor(industry_sector,
                                  levels = names(label_mapping), # Use original levels
                                  labels = unname(label_mapping)))

create_plots <- function(){
    gender_gap_df <- gender_gap_df %>%
        filter(industry_sector != "Others")

    plt <- ggplot(gender_gap_df, aes(x = Tau, y = Gender_Gap, color = industry_sector)) +
        geom_line(aes(group = industry_sector), linewidth = 0.8) +
        geom_point(size = 2) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "red", linewidth = 0.8) +

        facet_wrap(~ years_of_education,
                    labeller = labeller(years_of_education = function(x) paste0(x, " Years Education")),
                    ncol = 3) +

        labs(
            title = "Gender Pay Gap Across Industries\nby Education Level and Salary Quantile",
            subtitle = paste0("At age ", typical_age, " and employment status '", typical_employment_status, "'"),
            x = "Quantile (tau)",
            y = "Female - Male Salary Difference [€]",
            color = "Industry Sector"
        ) +
        theme_minimal() + # Start with a minimal theme as a base
        theme(
            # Set plot and panel background to white
            plot.background = element_rect(fill = "white", color = NA), # Overall plot background
            panel.background = element_rect(fill = "white", color = NA), # Background of the plotting area itself

            # Legend Position and Layout
            legend.position = "top",          # Place legend at the top
            legend.justification = "left",    # Justify legend content to the left
            legend.box.just = "left",         # Justify the entire legend box to the left
            legend.direction = "horizontal",  # Arrange legend items horizontally
            legend.box.margin = margin(10, 0, 10, 0), # Optional: Add some bottom margin to separate from plot
            legend.background = element_rect(fill = "grey90", color = NA),


            # Text sizes (as previously set for readability)
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            plot.subtitle = element_text(size = 14, hjust = 0.5),
            axis.title.x = element_text(size = 14, margin = margin(t = 10)),
            axis.title.y = element_text(size = 14, margin = margin(r = 10)),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.title = element_text(face = "bold", size = 14),
            legend.text = element_text(size = 12),

            # Facet strip text (labels above each small plot)
            strip.text = element_text(size = 12, face = "bold"),
            strip.background = element_rect(fill = "grey90", color = NA) # Background for facet labels
        )
    ggsave(paste0("./line-plots/gender_gap_plot_absolute_", typical_age, ".png"), plot = plt, width = 8, height = 6, units = "in", dpi = 300)
    
    plt <- ggplot(gender_gap_df, aes(x = Tau, y = Relative_Gender_Gap, color = industry_sector)) +
        geom_line(aes(group = industry_sector), linewidth = 0.8) +
        geom_point(size = 2) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "red", linewidth = 0.8) +

        facet_wrap(~ years_of_education,
                    labeller = labeller(years_of_education = function(x) paste0(x, " Years Education")),
                    ncol = 3) +

        labs(
            title = "Relative Gender Pay Gap Across Industries\nby Education Level and Salary Quantile",
            subtitle = paste0("At age ", typical_age, " and employment status '", typical_employment_status, "'"),
            x = "Quantile (tau)",
            y = "Female - Male Salary Difference [%]",
            color = "Industry Sector"
        ) +
        theme_minimal() + # Start with a minimal theme as a base
        theme(
            # Set plot and panel background to white
            plot.background = element_rect(fill = "white", color = NA), # Overall plot background
            panel.background = element_rect(fill = "white", color = NA), # Background of the plotting area itself

            # Legend Position and Layout
            legend.position = "top",          # Place legend at the top
            legend.justification = "left",    # Justify legend content to the left
            legend.box.just = "left",         # Justify the entire legend box to the left
            legend.direction = "horizontal",  # Arrange legend items horizontally
            legend.box.margin = margin(10, 0, 10, 0), # Optional: Add some bottom margin to separate from plot
            legend.background = element_rect(fill = "grey90", color = NA),


            # Text sizes (as previously set for readability)
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            plot.subtitle = element_text(size = 14, hjust = 0.5),
            axis.title.x = element_text(size = 14, margin = margin(t = 10)),
            axis.title.y = element_text(size = 14, margin = margin(r = 10)),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.title = element_text(face = "bold", size = 14),
            legend.text = element_text(size = 12),

            # Facet strip text (labels above each small plot)
            strip.text = element_text(size = 12, face = "bold"),
            strip.background = element_rect(fill = "grey90", color = NA) # Background for facet labels
        )
    ggsave(paste0("./line-plots/gender_gap_plot_relative_", typical_age, ".png"), plot = plt, width = 8, height = 6, units = "in", dpi = 300)
    
}

create_plots()




# Or plot for a specific education level across industries
#plt <- ggplot(filter(gender_gap_df, years_of_education == education_levels), aes(x = Tau, y = Gender_Gap, color = industry_sector)) +
#  geom_point() +
#  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
#  labs(
#    title = "Gender Pay Gap for 12 Years Education Across Industries",
#    subtitle = paste0("At age ", age_levels[1], " and employment status '", typical_employment_status, "'"),
#    x = "Quantile (tau)",
#    y = "Female - Male Salary Difference (€)",
#    color = "Industry Sector"
#  ) +
#  theme(
#    # Plot Title
#    plot.title = element_text(face = "bold", size = 16, hjust = 0.5), # Increased size and centered
#    # Plot Subtitle
#    plot.subtitle = element_text(size = 14, hjust = 0.5), # Increased size and centered
#
#    # Axis Labels (titles)
#    axis.title.x = element_text(size = 14, margin = margin(t = 10)), # Increased size, added top margin
#    axis.title.y = element_text(size = 14, margin = margin(r = 10)), # Increased size, added right margin
#
#    # Axis Ticks (numbers/labels on the axes)
#    axis.text.x = element_text(size = 14), # Increased size for X-axis ticks
#    axis.text.y = element_text(size = 14), # Increased size for Y-axis ticks
#
#    # Legend
#    legend.title = element_text(face = "bold", size = 14), # Increased size and bold for legend title
#    legend.text = element_text(size = 12), # Increased size for legend items
#    legend.position = "right" # Ensures legend is visible on the right
#  )

plt <- ggplot(gender_gap_df, aes(x = Tau, y = Gender_Gap, color = industry_sector)) +
  geom_line(aes(group = industry_sector), linewidth = 0.8) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red", linewidth = 0.8) +

  facet_wrap(~ years_of_education,
             labeller = labeller(years_of_education = function(x) paste0(x, " Years Education")),
             ncol = 3) +

  labs(
    title = "Gender Pay Gap Across Industries\nby Education Level and Salary Quantile",
    subtitle = paste0("At age ", typical_age, " and employment status '", typical_employment_status, "'"),
    x = "Quantile (tau)",
    y = "Female - Male Salary Difference (€)",
    color = "Industry Sector"
  ) +
  theme_minimal() + # Start with a minimal theme as a base
  theme(
    # Set plot and panel background to white
    plot.background = element_rect(fill = "white", color = NA), # Overall plot background
    panel.background = element_rect(fill = "white", color = NA), # Background of the plotting area itself

    # Legend Position and Layout
    legend.position = "top",          # Place legend at the top
    legend.justification = "left",    # Justify legend content to the left
    legend.box.just = "left",         # Justify the entire legend box to the left
    legend.direction = "horizontal",  # Arrange legend items horizontally
    legend.box.margin = margin(10, 0, 10, 0), # Optional: Add some bottom margin to separate from plot
    legend.background = element_rect(fill = "grey90", color = NA),


    # Text sizes (as previously set for readability)
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12),

    # Facet strip text (labels above each small plot)
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA) # Background for facet labels
  )

#ggsave("gender_gap_plot_reduced_height2.png", plot = plt, width = 8, height = 6, units = "in", dpi = 300)


# 4. Initialize a list to store the results for the 'female' coefficient
#qr_glass_ceiling_results <- list()
#
## 5. Loop through each tau, run the QR model, and extract the 'female' coefficient
#cat("Running Quantile Regressions for Glass Ceiling Analysis...\n")
#for (t in taus_for_analysis) {
#  # The full model including all specified interactions
#  model_qr <- rq(gross_salary_year ~ female + age + years_of_education + employment_status + industry_sector,
#                 tau = t, data = df_working_salary_year)
#
#  # Get the summary of the model coefficients.
#  summary_model_qr <- summary(model_qr, se = "boot", R = 200)
#
#  # Identify the coefficient name for the female indicator.
#  # This usually depends on how 'female' is coded (e.g., 'female1' if 0/1, or 'femaleFemale' if a factor with labels).
#  # We'll check for 'female1' as a common output for 0/1 numeric coding.
#  coefs <- summary_model_qr$coefficients
#  female_coef_name <- "female1" # Assuming 'female' is numeric 0/1, where 1 is female.
#
#  # Check if the coefficient exists in the summary table
#  if (female_coef_name %in% rownames(coefs)) {
#    female_row <- coefs[female_coef_name, ]
#    qr_glass_ceiling_results[[as.character(t)]] <- data.frame(
#      Tau = t,
#      Female_Coef_Estimate = female_row["Value"],
#      Female_Coef_StdError = female_row["Std. Error"],
#      Female_Coef_PValue = female_row["Pr(>|t|)"],
#      stringsAsFactors = FALSE
#    )
#    cat(paste0("  - Processed tau = ", t, "\n"))
#  } else {
#    warning(paste("Coefficient for 'female' (", female_coef_name, ") not found in model for tau =", t,
#                  ". Check the exact name of the female indicator in your model summary."))
#  }
#}
#
## 6. Combine all results into a single data frame
#qr_summary_df <- do.call(rbind, qr_glass_ceiling_results)
#print(qr_summary_df)
#
## 7. Prepare data for plotting (add significance and CI bounds)
#qr_summary_df_plot <- qr_summary_df %>%
#  mutate(
#    Significant = Female_Coef_PValue < 0.05,
#    lower_ci = Female_Coef_Estimate - 1.96 * Female_Coef_StdError,
#    upper_ci = Female_Coef_Estimate + 1.96 * Female_Coef_StdError
#  )
#
## 8. Create the Plot of Female Coefficient Across Quantiles
#plt<-ggplot(qr_summary_df_plot, aes(x = Tau, y = Female_Coef_Estimate, color = Significant)) +
#  geom_point(size = 4) +
#  # FIX: Set a static color for geom_line to avoid the error.
#  # This makes the line a consistent darkgrey color, while points still vary by significance.
#  geom_line(aes(group = 1), linetype = "dashed", linewidth = 0.8, color = "darkgrey") +
#  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.02, linewidth = 0.8) +
#  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50", linewidth = 0.8) +
#  
#  # Set colors for significance
#  scale_color_manual(
#    values = c("TRUE" = "steelblue", "FALSE" = "grey70"),
#    labels = c("TRUE" = "Statistically Significant (p < 0.05)", "FALSE" = "Not Significant (p \u2265 0.05)")
#  ) +
#  
#  # Labels and titles
#  labs(
#    title = "Gender Pay Gap (Female Coefficient) Across Salary Quantiles",
#    subtitle = "From Quantile Regression Model (Controlling for Age, Education, Employment, Industry, and Interactions)",
#    x = "Quantile (tau) of Conditional Salary Distribution",
#    y = "Female Coefficient Estimate (USD)",
#    color = "Statistical Significance"
#  ) +
#  
#  # Theme adjustments for readability
#  theme_minimal() +
#  theme(
#    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
#    plot.subtitle = element_text(size = 12, hjust = 0.5),
#    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
#    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
#    axis.text = element_text(size = 12),
#    legend.position = "bottom",
#    legend.title = element_text(face = "bold", size = 12),
#    legend.text = element_text(size = 10)
#  )
#
#ggsave(
#  filename = "gender_pay_gap_quantile_regression.png", # Name of your PNG file
#  plot = plt,                                  # The plot object you want to save
#  width = 10,                                        # Width of the plot in inches
#  height = 7,                                        # Height of the plot in inches
#  dpi = 300                                          # Resolution (dots per inch) for good quality
#)
#

# Create a linear regression model
# Using gross_salary_month as the dependent variable
model_month <- lm(gross_salary_month ~ female + age + years_of_education + employment_status + industry_sector, data = df_working_population)
#print(summary(model_month))
cat("Average Female Salary Month:", mean(df_working_salary_month$gross_salary_month[df_working_salary_month$female == 1], na.rm=TRUE), "\n")
cat("Average Male Salary Month:", mean(df_working_salary_month$gross_salary_month[df_working_salary_month$female == 0], na.rm=TRUE), "\n")
cat("Average Female Salary Year:", mean(df_working_salary_year$gross_salary_year[df_working_salary_year$female == 1], na.rm=TRUE), "\n")
cat("Average Male Salary Year:", mean(df_working_salary_year$gross_salary_year[df_working_salary_year$female == 0], na.rm=TRUE), "\n")

# Extract the coefficient for the 'female1' level (assuming 1 represents female)
sanitised_gap_month_coef <- summary(model_month)$coefficients["female1", "Estimate"]


#sanitised_gap_month_percent <- (sanitised_gap_month_coef / mean(df_working_population$gross_salary_month[df_working_population$female == 0], na.rm = TRUE)) * 100
#cat("Sanitised Gender Pay Gap (Monthly, from Regression):", sanitised_gap_month_percent, "%\n")
#
#model_year <- lm(gross_salary_year ~ female + age + pers + children + years_of_education + employment_status + industry_sector + subjective_health, data = df_working_population)
#summary_model_year <- summary(model_year)
#sanitised_gap_year_coef <- summary(model_year)$coefficients["female1", "Estimate"]
#sanitised_gap_year_percent <- (sanitised_gap_year_coef / mean(df_working_population$gross_salary_year[df_working_population$female == 0], na.rm = TRUE)) * 100
#cat("Sanitised Gender Pay Gap (Yearly, from Regression):", sanitised_gap_year_percent, "%\n")
#
#
#mean_salary_men <- mean(df_working_salary_month$gross_salary_month[df_working_salary_month$female == 0], na.rm = TRUE)
#mean_salary_women <- mean(df_working_salary_month$gross_salary_month[df_working_salary_month$female == 1], na.rm = TRUE)
#
#
#
## Calculate the non-sanitised gender pay gap (as a percentage of men's salary)
#non_sanitised_gap_mean <- ((mean_salary_men - mean_salary_women) / mean_salary_men) * 100
#cat("Non-Sanitised Gender Pay Gap (Mean):", non_sanitised_gap_mean, "%\n")
#
## Calculate mean gross yearly salary for men and women
#mean_salary_men_year <- mean(df_working_population$gross_salary_year[df_working_population$female == 0], na.rm = TRUE)
#mean_salary_women_year <- mean(df_working_population$gross_salary_year[df_working_population$female == 1], na.rm = TRUE)
#
## Calculate the non-sanitised gender pay gap (as a percentage of men's salary)
#non_sanitised_gap_mean_year <- ((mean_salary_men_year - mean_salary_women_year) / mean_salary_men_year) * 100
#cat("Non-Sanitised Gender Pay Gap Yearly (Mean):", non_sanitised_gap_mean_year, "%\n")
#
