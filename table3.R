################################################################################
# BETTER FAFSA - TABLE 3
# Changes in Refiling Outcomes by Tercile of Predicted Refiling
################################################################################

# Load required libraries
library(dplyr)
library(tidyr)
library(fixest)

# Set options
options(scipen = 999)

# Directories
project_dir  <- "/home/erik/Repos/fafsa"
data_dir     <- file.path(project_dir, "data")
tables_dir   <- file.path(project_dir, "tables")

# Create directories if they don't exist
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

# Load data
df <- readRDS(file.path(data_dir, "clean6.Rds"))

cat("Data loaded successfully.\n")
cat("Total observations:", nrow(df), "\n\n")

# Filter to pre-2024 data and create year_factor
df_reg <- df %>% 
  filter(year != 2024) %>%
  mutate(year_factor = as.factor(year))

cat("Regression sample (years 2021-2023):", nrow(df_reg), "\n\n")

# Add age polynomials for logistic model
df_reg <- df_reg %>% mutate(age_sq = age^2, age_cu = age^3)

# Covariates for logistic model
logit_covars <- c(
  "ind",
  "fresh",
  "soph",
  "uc",
  "csu",
  "age",
  "age_sq",
  "age_cu",
  "inc_q1",
  "inc_q2",
  "inc_q3",
  "inc_q4",
  "fam_1",
  "fam_2",
  "fam_3",
  "fam_4",
  "black",
  "hisp",
  "tmr",
  "white",
  "asian",
  "mhi_2",
  "mhi_3",
  "nw_1",
  "nw_2",
  "snap_1",
  "snap_2"
)

# Helper functions
run_reg <- function(data, outcome, covars) {
  rhs <- paste(c("year_factor", covars), collapse = " + ")
  formula_str <- as.formula(paste(outcome, "~", rhs, "| zip_code"))
  model <- feols(formula_str, data = data, cluster = "zip_code")
  return(model)
}

run_regression_for_group <- function(data, outcome, covars, group_var = NULL, group_val = NULL) {
  if (!is.null(group_var)) {
    filtered <- data[data[[group_var]] == group_val, ]
  } else {
    filtered <- data
  }
  
  baseline <- coef(lm(as.formula(paste(outcome, "~ year_factor")), data = filtered))[1]
  model <- run_reg(filtered, outcome, covars)
  coef_table <- coeftable(model)
  year_coefs <- coef_table[grep("year_factor", rownames(coef_table)), , drop = FALSE]
  
  return(list(
    baseline  = baseline,
    coef_2022 = year_coefs[1, 1],
    se_2022   = year_coefs[1, 2],
    coef_2023 = year_coefs[2, 1],
    se_2023   = year_coefs[2, 2],
    model     = model
  ))
}

format_coef <- function(coef, se) {
  if (is.na(coef) || is.na(se)) return(NA)
  t_stat <- abs(coef / se)
  stars <- ifelse(t_stat >= 2.576, "***",
                  ifelse(t_stat >= 1.96, "**",
                         ifelse(t_stat >= 1.645, "*", "")))
  sprintf("%.3f%s", coef, stars)
}

format_se <- function(se) {
  if (is.na(se)) return(NA)
  sprintf("(%.3f)", se)
}

# Function to create terciles based on predicted probabilities
create_terciles <- function(df, outcome_var, covariates) {
  cat("  Training logistic model on 2021 data...\n")
  
  # Filter to 2021 for training
  df_21 <- df %>% filter(year == 2021)
  
  # Prepare data - remove missing values
  model_data <- df_21 %>%
    select(all_of(c(outcome_var, covariates, "zip_code"))) %>%
    drop_na()
  
  cat("    Training sample size:", nrow(model_data), "\n")
  
  # Fit logistic model
  fml <- as.formula(paste(outcome_var, "~", paste(covariates, collapse = " + ")))
  model <- feglm(fml, data = model_data, family = binomial(link = "logit"))
  
  # Function to predict for a given year
  predict_year <- function(df_year) {
    df_clean <- df_year %>%
      mutate(row_id = row_number()) %>%
      select(row_id, all_of(c(outcome_var, covariates))) %>%
      drop_na()
    
    # Predict probabilities
    df_clean$prob <- predict(model, newdata = df_clean, type = "response")
    
    # Merge back
    df_year <- df_year %>%
      mutate(row_id = row_number()) %>%
      left_join(df_clean %>% select(row_id, prob), by = "row_id") %>%
      select(-row_id)
    
    return(df_year)
  }
  
  cat("  Predicting for all years...\n")
  
  # Predict for all years (pre-filter to avoid copying full df)
  df_2021 <- df %>% filter(year == 2021)
  df_2022 <- df %>% filter(year == 2022)
  df_2023 <- df %>% filter(year == 2023)
  
  df_21 <- predict_year(df_2021)
  df_22 <- predict_year(df_2022)
  df_23 <- predict_year(df_2023)
  
  # Clean up
  rm(df_2021, df_2022, df_2023)
  
  # Create terciles based on 2021 predictions
  tercile_breaks <- quantile(df_21$prob, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  
  cat("  Tercile breaks:", round(tercile_breaks, 3), "\n")
  
  # Assign terciles
  df_21$tercile <- cut(df_21$prob, breaks = tercile_breaks, 
                       labels = c("Low", "Middle", "High"), include.lowest = TRUE)
  df_22$tercile <- cut(df_22$prob, breaks = tercile_breaks, 
                       labels = c("Low", "Middle", "High"), include.lowest = TRUE)
  df_23$tercile <- cut(df_23$prob, breaks = tercile_breaks, 
                       labels = c("Low", "Middle", "High"), include.lowest = TRUE)
  
  # Combine
  combined <- bind_rows(df_21, df_22, df_23)
  
  # Return both data and model
  return(list(data = combined, model = model))
}

# Create terciles for app_next
cat("Creating terciles for app_next...\n")
app_tercile_result <- create_terciles(df_reg, "app_next", logit_covars)
df_app_tercile <- app_tercile_result$data
logit_model_app <- app_tercile_result$model

cat("\n")

# Create terciles for cal_next
cat("Creating terciles for cal_next...\n")
cal_tercile_result <- create_terciles(df_reg, "cal_next", logit_covars)
df_cal_tercile <- cal_tercile_result$data
logit_model_cal <- cal_tercile_result$model

cat("\n")

# Run regressions for app_next by tercile
cat("Running regressions for app_next by tercile...\n")
app_low <- run_regression_for_group(df_app_tercile %>% filter(tercile == "Low"), 
                                    "app_next", logit_covars)
app_mid <- run_regression_for_group(df_app_tercile %>% filter(tercile == "Middle"), 
                                    "app_next", logit_covars)
app_high <- run_regression_for_group(df_app_tercile %>% filter(tercile == "High"), 
                                     "app_next", logit_covars)

cat("  ✓ app_next regressions completed\n\n")

# Run regressions for cal_next by tercile
cat("Running regressions for cal_next by tercile...\n")
cal_low <- run_regression_for_group(df_cal_tercile %>% filter(tercile == "Low"), 
                                    "cal_next", logit_covars)
cal_mid <- run_regression_for_group(df_cal_tercile %>% filter(tercile == "Middle"), 
                                    "cal_next", logit_covars)
cal_high <- run_regression_for_group(df_cal_tercile %>% filter(tercile == "High"), 
                                     "cal_next", logit_covars)

cat("  ✓ cal_next regressions completed\n\n")

# Assemble Table 3
table3_app <- data.frame(
  Row = c("Baseline Mean Refile Rate (2022-23)",
          "",
          "Refiled in 2023-24",
          "SE_2023-24",
          "",
          "Refiled in 2024-25",
          "SE_2024-25"),
  Low = c(sprintf("%.3f", app_low$baseline),
          "",
          format_coef(app_low$coef_2022, app_low$se_2022),
          format_se(app_low$se_2022),
          "",
          format_coef(app_low$coef_2023, app_low$se_2023),
          format_se(app_low$se_2023)),
  Middle = c(sprintf("%.3f", app_mid$baseline),
             "",
             format_coef(app_mid$coef_2022, app_mid$se_2022),
             format_se(app_mid$se_2022),
             "",
             format_coef(app_mid$coef_2023, app_mid$se_2023),
             format_se(app_mid$se_2023)),
  High = c(sprintf("%.3f", app_high$baseline),
           "",
           format_coef(app_high$coef_2022, app_high$se_2022),
           format_se(app_high$se_2022),
           "",
           format_coef(app_high$coef_2023, app_high$se_2023),
           format_se(app_high$se_2023)),
  stringsAsFactors = FALSE
)

table3_cal <- data.frame(
  Row = c("Baseline Mean Refile Rate - Cal Grant (2022-23)",
          "",
          "Refiled by March 2 in 2023-24",
          "SE_2023-24",
          "",
          "Refiled by June 2 in 2024-25",
          "SE_2024-25"),
  Low = c(sprintf("%.3f", cal_low$baseline),
          "",
          format_coef(cal_low$coef_2022, cal_low$se_2022),
          format_se(cal_low$se_2022),
          "",
          format_coef(cal_low$coef_2023, cal_low$se_2023),
          format_se(cal_low$se_2023)),
  Middle = c(sprintf("%.3f", cal_mid$baseline),
             "",
             format_coef(cal_mid$coef_2022, cal_mid$se_2022),
             format_se(cal_mid$se_2022),
             "",
             format_coef(cal_mid$coef_2023, cal_mid$se_2023),
             format_se(cal_mid$se_2023)),
  High = c(sprintf("%.3f", cal_high$baseline),
           "",
           format_coef(cal_high$coef_2022, cal_high$se_2022),
           format_se(cal_high$se_2022),
           "",
           format_coef(cal_high$coef_2023, cal_high$se_2023),
           format_se(cal_high$se_2023)),
  stringsAsFactors = FALSE
)

table3 <- list(
  app_next = table3_app,
  cal_next = table3_cal,
  logit_model_app = logit_model_app,
  logit_model_cal = logit_model_cal
)

# Save Table 3
saveRDS(table3, file = file.path(tables_dir, "table3.Rds"))

cat("✓ Table 3 created and saved successfully\n")
cat("  Location:", file.path(tables_dir, "table3.Rds"), "\n\n")

# Cleanup
rm(app_low, app_mid, app_high, cal_low, cal_mid, cal_high)
rm(table3_app, table3_cal)
rm(df_app_tercile, df_cal_tercile, app_tercile_result, cal_tercile_result)
rm(logit_covars, create_terciles)
rm(run_reg, run_regression_for_group, format_coef, format_se)