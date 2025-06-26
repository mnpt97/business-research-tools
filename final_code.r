# Final code for paper
# author: Marc Peter
# date: 14.06.2026

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

# ----------- Sanitize malformed / unusual values: -------------- #
# Replace [10] completely satisfied in column "subjective_life_satisfaction" string value 10 and convert the whole column to numeric
df[df$subjective_life_satisfaction == "[10] completely satisfied", "subjective_life_satisfaction"] <- "10"
df$subjective_life_satisfaction <- as.numeric(df$subjective_life_satisfaction)

# Replace all fields with a monthly salary equal to 0 with 1/12 of yearly salary
condition_month_salary <- (!is.na(df$gross_salary_year) & df$gross_salary_year > 0 & df$gross_salary_year / 12 > df$gross_salary_month)
df[condition_month_salary, "gross_salary_month"] <- df[condition_month_salary, "gross_salary_year"] / 12

print(head(df))

# Helper function to calcluate the mean and format it to "%"
mean_to_per <- function(value){
    return(round(mean(value, na.rm = TRUE), 3) * 100)
}

summarise_df <- function(data_frame){
    return( data_frame %>%
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
                    paste(sum(female, na.rm=TRUE)), 
                    paste(sum(!female, na.rm = TRUE)),
                    paste(sum(!is.na(age), na.rm = TRUE)),
                    paste(sum(!is.na(pers), na.rm = TRUE)),
                    paste(sum(!is.na(pers) & !is.na(children) & children > 0, na.rm=TRUE)),
                    paste(sum(!is.na(pers) & !is.na(children) & children > 0 & pers-children == 1, na.rm=TRUE)),
                    paste(sum(!is.na(years_of_education))),
                    paste(sum(!is.na(gross_salary_year))),
                    paste(sum(!is.na(gross_salary_month))),
                    paste(sum(!is.na(employment_status) & employment_status == 1)),
                    paste(sum(!is.na(employment_status) & employment_status == 2)),
                    paste(sum(!is.na(employment_status) & employment_status == 3)),
                    paste(sum(!is.na(employment_status) & employment_status == 4)),
                    paste(sum(!is.na(employment_status) & employment_status == 5)),
                    paste(sum(!is.na(industry_sector) & industry_sector == 1)),
                    paste(sum(!is.na(industry_sector) & industry_sector == 2)),
                    paste(sum(!is.na(industry_sector) & industry_sector == 3)),
                    paste(sum(!is.na(industry_sector) & industry_sector == 4)),
                    paste(sum(!is.na(industry_sector) & industry_sector == 5)),
                    paste(sum(!is.na(industry_sector) & industry_sector == 6)),
                    paste(sum(!is.na(industry_sector) & industry_sector == 7)),
                    paste(sum(!is.na(industry_sector) & industry_sector == 8)),
                    paste(sum(!is.na(industry_sector) & industry_sector == 9)),
                    paste(sum(!is.na(industry_sector) & industry_sector == 10))
                ),
                "%" = c(
                    paste(mean_to_per(female)),
                    paste(mean_to_per(!female)),
                    paste(NA),
                    paste(NA),
                    paste(mean_to_per(!is.na(pers) & !is.na(children) & children > 0)),
                    paste(mean_to_per(!is.na(pers) & !is.na(children) & children > 0 & pers-children == 1)),
                    paste(NA),
                    paste(NA),
                    paste(NA),
                    paste(mean_to_per(!is.na(employment_status) & employment_status == 1)),
                    paste(mean_to_per(!is.na(employment_status) & employment_status == 2)),
                    paste(mean_to_per(!is.na(employment_status) & employment_status == 3)),
                    paste(mean_to_per(!is.na(employment_status) & employment_status == 4)),
                    paste(mean_to_per(!is.na(employment_status) & employment_status == 5)),
                    paste(mean_to_per(!is.na(industry_sector) & industry_sector == 1)),
                    paste(mean_to_per(!is.na(industry_sector) & industry_sector == 2)),
                    paste(mean_to_per(!is.na(industry_sector) & industry_sector == 3)),
                    paste(mean_to_per(!is.na(industry_sector) & industry_sector == 4)),
                    paste(mean_to_per(!is.na(industry_sector) & industry_sector == 5)),
                    paste(mean_to_per(!is.na(industry_sector) & industry_sector == 6)),
                    paste(mean_to_per(!is.na(industry_sector) & industry_sector == 7)),
                    paste(mean_to_per(!is.na(industry_sector) & industry_sector == 8)),
                    paste(mean_to_per(!is.na(industry_sector) & industry_sector == 9)),
                    paste(mean_to_per(!is.na(industry_sector) & industry_sector == 10))
                ),
                "Mean" = c(
                    paste(mean(female, na.rm = TRUE)),
                    paste(mean(!female, na.rm = TRUE)),
                    paste(mean(age, na.rm = TRUE)),
                    paste(mean(pers, na.rm=TRUE)),
                    paste(mean(!is.na(pers) & !is.na(children) & children > 0, na.rm=TRUE)),
                    paste(mean(!is.na(pers) & !is.na(children) & children > 0 & pers-children == 1, na.rm=TRUE)),
                    paste(mean(years_of_education, na.rm=TRUE)),
                    paste(mean(gross_salary_year, na.rm=TRUE)),
                    paste(mean(gross_salary_month, na.rm=TRUE)),
                    paste(mean(!is.na(employment_status) & employment_status == 1)),
                    paste(mean(!is.na(employment_status) & employment_status == 2)),
                    paste(mean(!is.na(employment_status) & employment_status == 3)),
                    paste(mean(!is.na(employment_status) & employment_status == 4)),
                    paste(mean(!is.na(employment_status) & employment_status == 5)),
                    paste(mean(!is.na(industry_sector) & industry_sector == 1)),
                    paste(mean(!is.na(industry_sector) & industry_sector == 2)),
                    paste(mean(!is.na(industry_sector) & industry_sector == 3)),
                    paste(mean(!is.na(industry_sector) & industry_sector == 4)),
                    paste(mean(!is.na(industry_sector) & industry_sector == 5)),
                    paste(mean(!is.na(industry_sector) & industry_sector == 6)),
                    paste(mean(!is.na(industry_sector) & industry_sector == 7)),
                    paste(mean(!is.na(industry_sector) & industry_sector == 8)),
                    paste(mean(!is.na(industry_sector) & industry_sector == 9)),
                    paste(mean(!is.na(industry_sector) & industry_sector == 10))
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
        )


}
write.csv(summarise_df(df), file = "./tables/df_summary.csv")


# Focus on working population in age range 18 - 65 or people who are employed
df_working_population <- df %>%
    filter((!is.na(age) & age > 18 & age <= 65)
        & (!is.na(employment_status) & employment_status != 5))


# Compare salaries grouped by gender
salary_summary_by_gender <- df_working_population %>%
    group_by(female) %>%
    summarise(
        "n" = n(),
        "Mean Salary year" = mean(gross_salary_year, na.rm = TRUE),
        "Median Salary year" = median(gross_salary_year, na.rm = TRUE),
        "Mean Salary month" = mean(gross_salary_month, na.rm = TRUE),
        "Median Salary month" = median(gross_salary_month, na.rm = TRUE)
    )

# Compare years of education grouped by gender
education_summary_by_gender <- df_working_population %>%
    filter(!is.na(years_of_education)) %>%
    group_by(female) %>%
    summarise(
        "n" = n(),
        "Mean (years)" = mean(years_of_education, na.rm = TRUE),
        "Median (years)" = median(years_of_education, na.rm = TRUE),
    )

# Caluclate some relevant corelations based on the working population

cor_employment_status_salary_month <- cor(
    rank(df_working_population$employment_status),
    rank(df_working_population$gross_salary_month),
    use = "complete.obs", method = "spearman")

cor_employment_status_salary_year <- cor(
    rank(df_working_population$employment_status),
    rank(df_working_population$gross_salary_year),
    use = "complete.obs", method = "spearman")

cor_education_salary_month <- cor(
    df_working_population$years_of_education,
    df_working_population$gross_salary_month,
    use = "complete.obs")

cor_education_salary_year <- cor(
    df_working_population$years_of_education,
    df_working_population$gross_salary_year,
    use = "complete.obs")


cor_satisfaction_salary <- cor(
    rank(df_working_population$subjective_life_satisfaction),
    rank(df_working_population$gross_salary_year),
    use = "complete.obs", method = "spearman")

# Summarise the distribution of employed people based on gender and the industry sector
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

write.csv(industry_sector_by_gender, file = "./tables/industry_sector_gender_summary.csv")

# Sumarise the deployment status by gender
total_n_employment_status <- nrow(df_working_population %>% filter(!is.na(employment_status)))
employment_status_by_gender <- df_working_population %>%
    filter(!is.na(employment_status)) %>%
    group_by(employment_status) %>%
    summarise(
        "n" = n(),
        "Employment Status  [%]" = round(n() / total_n_employment_status * 100, 1),
        "Male [n]" = sum(!female),
        "Female [n]" = sum(female),
        "Female [%]" = round(mean(female),3) * 100,
    )

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
        # Group industry sectors with a proportion less than 5% into "Others" 
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

# Create factor based on group data
df_working_population$female <- as.numeric(df_working_population$female)

cor_female_salary_year <- cor(
    df_working_population$female,
    df_working_population$gross_salary_year,
    use = "complete.obs", method = "spearman")

df_working_population$female <- as.factor(df_working_population$female)

df_working_population$employment_status <- as.factor(df_working_population$employment_status)
df_working_population$industry_sector <- as.factor(df_working_population$industry_sector)
df_working_population$employment_status <- as.factor(df_working_population$employment_status)


# Relevel to most employed industry sector and "Full-Time" employment status
df_working_population$industry_sector <- relevel(df_working_population$industry_sector, ref="Administration, Education & Health")
df_working_population$employment_status <- relevel(df_working_population$employment_status, ref="Full-Time")

# This function creates a summary grouped by each industry sector for comparison
create_summary_by_industry <- function(){
    return(df_working_population %>%
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
    ))
}

df_summary_by_sector <- create_summary_by_industry()
write.csv(df_summary_by_sector, file = "./tables/df_summary_by_industry_sector.csv")

# Filter based on salaries larger than 0
df_working_salary_month <- df_working_population %>%
    filter(!is.na(gross_salary_month) & gross_salary_month > 0)
df_working_salary_year <- df_working_population %>%
    filter(!is.na(gross_salary_year) & gross_salary_year > 0)

# ---------- Plot salary distribution ----------#

hist_from_df_column <- function(df, col_to_plot, col_label){
    png(filename = paste0("./histograms/histogram_", col_to_plot, ".png"), width = 800, height = 600)
    par(cex.lab = 1.7, cex.axis = 1.5, mar = c(5.1, 6, 4.1, 2.1) + 0.1) 


    hist(df[[col_to_plot]],
        main = paste("Histogram of", col_label),
        xlab = col_label,
        ylab = "Frequency",
        col = "#818cf8",
        border = "#312e81",
        breaks = 15) 
    grid()
    par(cex.lab = 1, cex.axis = 1, mar = c(5.1, 4.1, 4.1, 2.1) + 0.1)
    dev.off()
}

hist_from_df_column(df_working_salary_year, "gross_salary_year", "Yearly Salary [€]")
hist_from_df_column(df_working_salary_month, "gross_salary_month", "Monthly Salary [€]")

# ------------ Analyse  Skewness ------------- #
library(e1071)
# Check skewness in salary distributions
skewness_value_salary_month <- skewness(df_working_salary_month$gross_salary_month, na.rm = TRUE)
cat("Skewness of Gross Monthly Salary:", skewness_value_salary_month, "\n")
skewness_value_salary_year <- skewness(df_working_population$gross_salary_year, na.rm = TRUE)
cat("Skewness of Gross Yearly Salary:", skewness_value_salary_year, "\n")


# Necessary Libraries

library(quantreg)
library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------ Quantile Regression across industry sectors-------------- #

# Set taus: each 10% quantile
taus_for_analysis <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

# Convert to numeric levels to labels
df_working_salary_year$female <- factor(df_working_salary_year$female, levels = c(0, 1), labels = c("Male", "Female"))

typical_age <- round(mean(df_working_salary_year$age), 0)

# Only looking at people employed in Full-Time
typical_employment_status <- "Full-Time" 

all_industries <- levels(df_working_salary_year$industry_sector)

# Set education level for prediction to 20%, 50% and 80% quantile 
education_levels <- c(
    quantile(df_working_salary_year$years_of_education, probs = 0.2, na.rm = TRUE), 
    quantile(df_working_salary_year$years_of_education, probs = 0.5, na.rm = TRUE), 
    quantile(df_working_salary_year$years_of_education, probs = 0.8, na.rm = TRUE)
)

predict_salary_on_qr <- function(years_of_education_level){
    # Create the new dataframe for predictions
    qr_prediction_data <- expand.grid(
        female = factor(c("Male", "Female"), levels = levels(df_working_salary_year$female)),
        age = typical_age,
        years_of_education = years_of_education_level,
        employment_status = factor(typical_employment_status),
        industry_sector = factor(all_industries, levels = levels(df_working_salary_year$industry_sector))
    )

    # predict salary for each tau
    predicted_salaries_list <- list()
    for (t in taus_for_analysis) {
        # Conditional Quantile Regression model. 
        # 
        model_qr_single_tau <- rq(gross_salary_year ~ female + industry_sector + employment_status + age + years_of_education +
                                    female:industry_sector + female:employment_status + female:age + female:years_of_education,
                                    tau = t, data = df_working_salary_year)

        
        preds <- predict(model_qr_single_tau, newdata = qr_prediction_data)

        predicted_salaries_list[[as.character(t)]] <- cbind(qr_prediction_data, Predicted_Salary = preds, Tau = t)
    }

    # Combine all predictions into a single dataframe
    predicted_salaries_df <- do.call(rbind, predicted_salaries_list)

    # Calculate the gender pay gap (Female - Male) for each scenario
    gender_gap_df <- predicted_salaries_df %>%
        group_by(Tau, industry_sector, employment_status) %>%
        summarise(
            Male_Salary = Predicted_Salary[female == "Male"],
            Female_Salary = Predicted_Salary[female == "Female"],
            Gender_Gap = Female_Salary - Male_Salary,
            Relative_Gender_Gap = ifelse(Male_Salary != 0, (Gender_Gap / Male_Salary) * 100, NA),
            .groups = 'drop'
    )
    return(predicted_salaries_df)
}

# Plot Theming
plot_theme <- function(){
    return(theme(
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA),

        legend.position = "top",          
        legend.justification = "left",    
        legend.box.just = "left",         
        legend.direction = "horizontal",  
        legend.box.margin = margin(10, 0, 10, 0), 
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

        strip.text = element_text(size = 12, face = "bold"),
        strip.background = element_rect(fill = "grey90", color = NA) 
    ))
}

# Label mapping for improved styling in plot png
label_mapping <- c(
        "Administration, Education & Health" = "Administration,\nEducation & Health",
        "Manufacturing" = "Manufacturing",
        "Trade" = "Trade",
        "Services" = "Services",
        "Others" = "Others"
        )

custom_industry_colors <- c(
        "Administration,\nEducation & Health" = "#f59e0b",
        "Manufacturing" = "#16a34a",
        "Trade" = "#0ea5e9",
        "Services" = "#ec4899"
    )

# Group prediction by Tau and industry sector
gender_gap_df <- predict_salary_on_qr(education_levels[1]) %>%
        group_by(Tau, industry_sector) %>%
        summarise(
            Male_Salary = Predicted_Salary[female == "Male"],
            Female_Salary = Predicted_Salary[female == "Female"],
            Gender_Gap = Female_Salary - Male_Salary,
            Relative_Gender_Gap = ifelse(Male_Salary != 0, (Gender_Gap / Male_Salary) * 100, NA),
            .groups = 'drop')

create_gender_gap_qr_plots_without_education <- function(){
    # Label mapping to fit plots legend

    gender_gap_df <- gender_gap_df %>%
    mutate(industry_sector = factor(industry_sector,
                                    levels = names(label_mapping),
                                    labels = unname(label_mapping)))

    # Don't plot others
    gender_gap_df <- gender_gap_df %>%
        filter(industry_sector != "Others")
    
    
    plt <- ggplot(gender_gap_df, aes(x = Tau, y = Relative_Gender_Gap, color = industry_sector)) +
        geom_line(aes(group = industry_sector), linewidth = 0.8) +
        geom_point(size = 2) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "red", linewidth = 0.8) +
        scale_color_manual(values = custom_industry_colors) +
        labs(
            title = "Relative gender pay gap across industries",
            subtitle = paste0("By yearly salary quantile and employment status ",typical_employment_status),
            x = "Quantile (tau)",
            y = "Female - Male Salary Difference [%]",
            color = "Industry Sector"
        ) +
        theme_minimal() +
        plot_theme()
        
    ggsave(paste0("./line-plots/gender_gap_plot_relative_without_education.png"), plot = plt, width = 8, height = 6, units = "in", dpi = 300)
    
}

create_gender_gap_qr_plots_without_education()

# Group prediction by Tau, industry sector and years of education
gender_gap_df <- predict_salary_on_qr(education_levels) %>%
        group_by(Tau, industry_sector,  years_of_education) %>%
        summarise(
            Male_Salary = Predicted_Salary[female == "Male"],
            Female_Salary = Predicted_Salary[female == "Female"],
            Gender_Gap = Female_Salary - Male_Salary,
            Relative_Gender_Gap = ifelse(Male_Salary != 0, (Gender_Gap / Male_Salary) * 100, NA),
            .groups = 'drop'
    )

write.csv(gender_gap_df,file = "./tables/qr_all.csv")

create_gender_gap_qr_plots_with_education <- function(){
    # Label mapping to fit plots legend

    gender_gap_df <- gender_gap_df %>%
    mutate(industry_sector = factor(industry_sector,
                                    levels = names(label_mapping),
                                    labels = unname(label_mapping)))

    # Don't plot Others
    gender_gap_df <- gender_gap_df %>%
        filter(industry_sector != "Others")
    
    
    plt <- ggplot(gender_gap_df, aes(x = Tau, y = Relative_Gender_Gap, color = industry_sector)) +
        geom_line(aes(group = industry_sector), linewidth = 0.8) +
        geom_point(size = 2) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "red", linewidth = 0.8) +
        scale_color_manual(values = custom_industry_colors) +

        facet_wrap(~ years_of_education,
                    labeller = labeller(years_of_education = function(x) paste0(x, " Years Education")),
                    ncol = 3) +

        labs(
            title = "Relative Gender Pay Gap Across Industries",
            subtitle = paste0("By yearly salary quantile, years of education and employment status ",typical_employment_status),
            x = "Quantile (tau)",
            y = "Female - Male Salary Difference [%]",
            color = "Industry Sector"
        ) +
        theme_minimal() + # Start with a minimal theme as a base
        plot_theme()

    ggsave(paste0("./line-plots/gender_gap_plot_relative_with_education.png"), plot = plt, width = 8, height = 6, units = "in", dpi = 300)
    
}

create_gender_gap_qr_plots_with_education()
