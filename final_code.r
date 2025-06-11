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

df_working_population <- df_working_population %>%
    mutate(
        # Create a new temporary column with string labels
        employment_status_string = case_when(
            employment_status == 1 ~ "Full-Time",
            employment_status == 2 ~ "Part-Time",
            employment_status == 3 ~ "Training or Apprenticeship",
            employment_status == 4 ~ "Irregular or Marginal",
            TRUE ~ as.character(employment_status)
        ))

desired_employment_status <- c(
    "Full-Time",
    "Part-Time",
    "Training or Apprenticeship",
    "Irregular or Marginal"
)

df_working_population$employment_status <- factor(
    df_working_population$employment_status_string,
    levels = desired_employment_status
)

df_working_population$employment_status_string <- NULL


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
        ## 
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

# Factories relvant columns
df_working_population$female <- as.factor(as.numeric(df_working_population$female))
df_working_population$employment_status <- as.factor(df_working_population$employment_status)
df_working_population$industry_sector <- as.factor(df_working_population$industry_sector)
df_working_population$employment_status <- as.factor(df_working_population$employment_status)

# Relevel to most employed industry sector and "Full-Time" employment status
df_working_population$industry_sector <- relevel(df_working_population$industry_sector, ref="Administration, Education & Health")
df_working_population$employment_status <- relevel(df_working_population$employment_status, ref="Full-Time")


