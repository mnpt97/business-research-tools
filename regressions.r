


overall_summary <- function(taus, df, primary){
    model_qreg_interaction <- rq(gross_salary_year ~ female + age + years_of_education + employment_status + industry_sector + female:industry_sector + female:years_of_education + industry_sector:years_of_education,
                                tau = taus,
                                data = df)

    summary_by_gender_and_industry <- summary(model_qreg_interaction)
    print(summary_by_gender_and_industry)

    num_taus <- length(taus)
    extracted_coefs_matrix <- matrix(NA, nrow = num_taus, ncol = 3)
    colnames(extracted_coefs_matrix) <- c("female_in_industry_10", "female_in_industry_8", "female1_years_of_education_value")
    rownames(extracted_coefs_matrix) <- paste0("tau_", taus)

    for (i in 1:num_taus) {
        # Access the coefficients matrix for the current tau
        coef_matrix_for_tau <- summary_by_gender_and_industry[[i]]$coefficients

        # Safely extract the Value for each coefficient
        # (e.g., if rq couldn't estimate it for a particular tau)

        female1_val <- NA
        if ("female1" %in% rownames(coef_matrix_for_tau)) {
            female1_val <- coef_matrix_for_tau["female1", "Value"]
        } else {
            warning(paste("female1 coefficient not found for tau =", taus[i]))
        }

        female_industry_8 <- NA
        if ("female1:industry_sector8" %in% rownames(coef_matrix_for_tau)) {
            female_industry_8 <- coef_matrix_for_tau["female1:industry_sector8", "Value"]
        } else {
            warning(paste("female1 coefficient not found for tau =", taus[i]))
        }

        female_edu_interaction_val <- NA
        if ("female1:years_of_education" %in% rownames(coef_matrix_for_tau)) {
            female_edu_interaction_val <- coef_matrix_for_tau["female1:years_of_education", "Value"]
        } else {
            warning(paste("female1:years_of_education coefficient not found for tau =", taus[i]))
        }

        # Store the extracted values in the matrix
        extracted_coefs_matrix[i, "female_in_industry_10"] <- female1_val
        extracted_coefs_matrix[i, "female_in_industry_8"] <- female_industry_8
        extracted_coefs_matrix[i, "female1_years_of_education_value"] <- female_edu_interaction_val
    }

    # convert it to a data frame for easier use with ggplot2
    extracted_coefs_df <- as.data.frame(extracted_coefs_matrix)
    extracted_coefs_df$tau <- taus # Add the tau values as a column

    print(extracted_coefs_df)

}

taus_all <- c(0.1,0.2,0.3, 0.5, 0.6, 0.7,0.8, 0.9)
overall_sum <- overall_summary(taus = taus_all, df = df_working_salary_year, "gross_salary_year")

get_coef_value <- function(coef_matrix, row_name, col_name = "Value") {
  if (row_name %in% rownames(coef_matrix)) {
    return(coef_matrix[row_name, col_name])
  } else {
    # If you want to see which specific coefficients are not found, uncomment the warning:
    # warning(paste("Coefficient '", row_name, "' not found for current tau. Assuming 0.", sep = ""))
    return(0)
  }
}

# --- Corrected overall_summary_enhanced function ---
overall_summary_enhanced <- function(taus, df, primary = NULL){
    # Fit the model
    model_qreg_interaction <- rq(gross_salary_year ~ female + age + years_of_education + employment_status + industry_sector + female:industry_sector + female:years_of_education + industry_sector:years_of_education,
                                tau = taus,
                                data = df)

    # Get the summary
    summary_model <- summary(model_qreg_interaction)

    # Identify current baseline levels for factors from the data's factor levels
    baseline_employment_status_level <- levels(df$employment_status)[1]
    baseline_industry_sector_level <- levels(df$industry_sector)[1]
    mean_age <- mean(df$age, na.rm = TRUE)

    # Get all industry sector levels to loop through
    all_industry_levels <- levels(df$industry_sector)

    # Initialize the list to store results for ALL taus and scenarios
    # This must be outside the main tau loop
    all_scenario_results_list <- list()

    # Define representative education levels to analyze (e.g., min, mean, max)
    representative_edu_levels <- c(min(df$years_of_education, na.rm = TRUE),
                                   round(mean(df$years_of_education, na.rm = TRUE)),
                                   max(df$years_of_education, na.rm = TRUE))


    # --- Loop through each tau ---
    for (k in 1:length(taus)) {
        current_tau <- taus[k]
        coefs_at_current_tau <- summary_model[[k]]$coefficients

        # Initialize data frame for current tau's results across industries/education
        # This is where we build the data for ONE tau, across all industries and education levels
        tau_results_df_for_this_tau <- data.frame( # Renamed for clarity inside the loop
            tau = numeric(0),
            Industry_Sector = character(0),
            Years_of_Education = numeric(0),
            Predicted_Male_Salary = numeric(0),
            Predicted_Female_Salary = numeric(0),
            Gender_Pay_Gap_Absolute = numeric(0),
            Gender_Pay_Gap_Relative = numeric(0)
        )

        # --- Loop through representative education levels ---
        for (edu_val in representative_edu_levels) {

            # Extract main female effect and female:years_of_education interaction
            female_main_effect <- get_coef_value(coefs_at_current_tau, "female1")
            female_edu_interaction_effect <- get_coef_value(coefs_at_current_tau, "female1:years_of_education")

            # --- Loop through each industry sector ---
            for (industry_level in all_industry_levels) {

                # --- Calculate Predicted Male Salary for current scenario ---
                pred_male_salary <- get_coef_value(coefs_at_current_tau, "(Intercept)") +
                                    get_coef_value(coefs_at_current_tau, "age") * mean_age +
                                    get_coef_value(coefs_at_current_tau, "years_of_education") * edu_val

                # Add employment status effect (assuming baseline employment for this summary)
                if (baseline_employment_status_level != levels(df$employment_status)[1]) {
                  status_col_name <- paste0("employment_status", baseline_employment_status_level)
                  pred_male_salary <- pred_male_salary + get_coef_value(coefs_at_current_tau, status_col_name)
                }

                # Add industry sector main effect (if not baseline)
                if (industry_level != baseline_industry_sector_level) {
                  industry_col_name <- paste0("industry_sector", industry_level)
                  pred_male_salary <- pred_male_salary + get_coef_value(coefs_at_current_tau, industry_col_name)
                }

                # Add education x industry interaction effect for males
                if (industry_level != baseline_industry_sector_level) {
                  edu_industry_interaction_name <- paste0("years_of_education:industry_sector", industry_level)
                  pred_male_salary <- pred_male_salary + (get_coef_value(coefs_at_current_tau, edu_industry_interaction_name) * edu_val)
                }
                if(pred_male_salary < 0){
                    pred_male_salary <- 0
                }

                # --- Calculate Predicted Female Salary for current scenario ---
                pred_female_salary <- pred_male_salary + female_main_effect # Start with male salary + female main effect

                # Add female x industry interaction effect (if not baseline industry)
                if (industry_level != baseline_industry_sector_level) {
                  female_industry_interaction_name <- paste0("female1:industry_sector", industry_level)
                  pred_female_salary <- pred_female_salary + get_coef_value(coefs_at_current_tau, female_industry_interaction_name)
                }

                # Add female x years of education interaction effect
                pred_female_salary <- pred_female_salary + (female_edu_interaction_effect * edu_val)

                if(pred_female_salary < 0){
                    pred_female_salary <- 0
                }

                # --- Calculate Gaps ---
                gender_gap_abs <- pred_female_salary - pred_male_salary
                gender_gap_rel <- ifelse(pred_male_salary != 0, (gender_gap_abs / pred_male_salary) * 100, NaN)

                # Add this row to the current tau's results data frame
                tau_results_df_for_this_tau <- bind_rows(tau_results_df_for_this_tau,
                                            data.frame(
                                                tau = current_tau,
                                                Industry_Sector = industry_level,
                                                Years_of_Education = edu_val,
                                                Predicted_Male_Salary = pred_male_salary,
                                                Predicted_Female_Salary = pred_female_salary,
                                                Gender_Pay_Gap_Absolute = gender_gap_abs,
                                                Gender_Pay_Gap_Relative = gender_gap_rel,
                                                stringsAsFactors = FALSE
                                            ))
            } # End industry loop
        } # End education loop
        # Store the completed data frame for the current tau in the list
        all_scenario_results_list[[k]] <- tau_results_df_for_this_tau
    } # End tau loop

    # --- Combine all results from the list into one large data frame for summary ---
    final_summary_df <- bind_rows(all_scenario_results_list)

    return(final_summary_df)
}


# Run the enhanced summary function
summary_of_gaps <- overall_summary_enhanced(
  taus = taus_all,
  df = df_working_salary_year
)

# Print the head of the summary data frame
industries_to_remove <- c("1", "3", "7", "8") # Define the industry sectors to exclude

plot_data_scatter_filtered <- summary_of_gaps %>%
  # Filter out the specified industry sectors
  filter(!(Industry_Sector %in% industries_to_remove)) %>%
  # Ensure Industry_Sector and Years_of_Education are treated as factors for plotting aesthetics
  mutate(
    Industry_Sector = factor(Industry_Sector),
    Years_of_Education = factor(Years_of_Education) # Treat education levels as categories for faceting
  )

print(summary_of_gaps)

# 3. Create the scatter plot
plt<-ggplot(plot_data_scatter_filtered, aes(x = tau, y = Gender_Pay_Gap_Relative)) +
  # Add individual industry points (colored by industry)
  geom_point(aes(color = Industry_Sector), alpha = 0.7, size = 2) +

  # Add lines connecting points for each individual industry (also colored by industry)
  geom_line(aes(group = Industry_Sector, color = Industry_Sector), linewidth = 0.8, alpha = 0.5) + # Made slightly transparent

  # --- This is the key change: Add ONE smooth trend line for all industries within each facet ---
  # 'group = Years_of_Education' is implicitly handled by facet_wrap, but good to be explicit
  geom_smooth(aes(group = Years_of_Education), # Group by education year for separate lines per facet
              method = "loess",              # LOESS for a flexible, non-linear smooth trend
              se = FALSE,                    # Do not show standard error bands
              color = "black",               # Make this overall trend line stand out (e.g., black)
              linetype = "solid",            # Solid line
              linewidth = 1.5) +             # Thicker line for emphasis
  # Add points on the overall trend line if desired for emphasis
  # geom_point(aes(y = after_stat(y)), stat = "smooth", method = "loess", color = "black", size = 3, shape = 21, fill = "white", se = FALSE) +

  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") + # Reference line at 0 gap
  facet_wrap(~ Years_of_Education, ncol = 1, scales = "free_y") + # Separate plot for each education level, one column
  labs(
    title = "Relative Gender Pay Gap by Quantile and Education",
    subtitle = "Overall Smoothed Trend Across All Industries (Excluding Industries 1, 3, 7, 8)",
    x = "Quantile (tau)",
    y = "Relative Gender Pay Gap (%)",
    color = "Industry Sector" # Legend title for industry colors
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 11, face = "bold"), # Facet title (education level)
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels if many taus
    legend.position = "right", # Position legend outside the plot area
    legend.title = element_text(face = "bold")
  ) +
  scale_color_viridis_d()

  #print(plt)