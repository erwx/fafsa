################################################################################
# BETTER FAFSA - FINAL CORRECTED COMPLETE ANALYSIS SCRIPT
# This script produces all tables and figures for the paper
# FINAL VERSION: Based on better_03.R approach with proper baseline calculation
################################################################################

rm(list = ls())
gc()

# Load required libraries
library(dplyr)
library(tidyr)
library(fixest)

# Set options
options(scipen = 999)

# Directories ----
project_dir  <- "/home/erik/Repos/fafsa"
data_dir     <- file.path(project_dir, "data")
figures_dir  <- file.path(project_dir, "figures")
tables_dir   <- file.path(project_dir, "tables")

# Create directories if they don't exist
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

setwd(project_dir)

# Load data ----
tryCatch({
  df <- readRDS(file.path(data_dir, "better_clean.Rds"))
  cat("Data loaded successfully.\n")
  cat("Total observations:", nrow(df), "\n")
  cat("Years:", paste(unique(df$year), collapse = ", "), "\n\n")
}, error = function(e) {
  stop("ERROR: Could not load data file. ", e$message)
})

################################################################################
# PART 1: SUMMARY STATISTICS TABLES
################################################################################

cat("="  , rep("=", 78), "\n", sep = "")
cat("PART 1: Creating Summary Statistics Tables\n")
cat("="  , rep("=", 78), "\n", sep = "")

# Define variables for summary statistics
summary_vars <- c("app_prev", "ind", "fam_size", "age", "inc", "mhi",
                  "asian", "black", "hisp", "tmr", "white", "snap",
                  "uc", "csu", "cc", "paid", "enr")

# Summary table function
create_summary <- function(df, vars, group_name = "All") {
  means <- round(colMeans(df[vars], na.rm = TRUE), 2)
  sds <- round(apply(df[vars], 2, sd, na.rm = TRUE), 2)
  summary_table <- data.frame(
    Variable = vars,
    Summary = paste0("$", sprintf("%.2f", means), "$ ($", sprintf("%.2f", sds), "$)")
  )
  # Add sample size
  summary_table[nrow(summary_table) + 1, ] <- c("N", format(nrow(df), big.mark = ","))
  colnames(summary_table)[2] <- group_name
  return(summary_table)
}

# TABLE 1: Summary statistics by All/Dependent/Independent
tryCatch({
  all_summary <- create_summary(df, summary_vars, "All")
  dep_summary <- create_summary(df[df$dep == 1, ], summary_vars, "Dependent")
  ind_summary <- create_summary(df[df$ind == 1, ], summary_vars, "Independent")
  
  table1 <- cbind(all_summary, dep_summary[, 2, drop = FALSE], ind_summary[, 2, drop = FALSE])
  
  cat("Table 1 created: Summary statistics by dependency status\n")
}, error = function(e) {
  cat("ERROR creating Table 1:", e$message, "\n")
  table1 <- NULL
})

# APPENDIX TABLE A1: Summary statistics by year
tryCatch({
  summary_2021 <- create_summary(df[df$year == 2021, ], summary_vars, "Y2021")
  summary_2022 <- create_summary(df[df$year == 2022, ], summary_vars, "Y2022")
  summary_2023 <- create_summary(df[df$year == 2023, ], summary_vars, "Y2023")
  summary_2024 <- create_summary(df[df$year == 2024, ], summary_vars, "Y2024")
  
  tableA1 <- cbind(summary_2021, 
                   summary_2022[, 2, drop = FALSE], 
                   summary_2023[, 2, drop = FALSE], 
                   summary_2024[, 2, drop = FALSE])
  
  cat("Appendix Table A1 created: Summary statistics by year\n")
}, error = function(e) {
  cat("ERROR creating Appendix Table A1:", e$message, "\n")
  tableA1 <- NULL
})

# Save summary tables as RDS
saveRDS(list(Table1 = table1, TableA1 = tableA1), 
        file = file.path(tables_dir, "summary_tables.Rds"))

cat("\nSummary tables saved to:", file.path(tables_dir, "summary_tables.Rds"), "\n\n")

################################################################################
# PART 2: PERCENT CHANGE TABLES (APPENDIX TABLE A2)
################################################################################

cat("="  , rep("=", 78), "\n", sep = "")
cat("PART 2: Creating Percent Change Tables\n")
cat("="  , rep("=", 78), "\n", sep = "")

# Calculate percent changes from 2023 to 2024
calc_pct_change <- function(df, filter_col = NULL, filter_val = NULL) {
  if (!is.null(filter_col)) {
    df <- df[df[[filter_col]] == filter_val, ]
  }
  
  # Overall
  all_2023 <- sum(df$app == 1 & df$year == 2023, na.rm = TRUE)
  all_2024 <- sum(df$app == 1 & df$year == 2024, na.rm = TRUE)
  pct_all <- round((all_2024 - all_2023) / all_2023 * 100, 3)
  
  # Freshman
  fresh_2023 <- sum(df$fresh == 1 & df$app == 1 & df$year == 2023, na.rm = TRUE)
  fresh_2024 <- sum(df$fresh == 1 & df$app == 1 & df$year == 2024, na.rm = TRUE)
  pct_fresh <- round((fresh_2024 - fresh_2023) / fresh_2023 * 100, 3)
  
  # Sophomore
  soph_2023 <- sum(df$soph == 1 & df$app == 1 & df$year == 2023, na.rm = TRUE)
  soph_2024 <- sum(df$soph == 1 & df$app == 1 & df$year == 2024, na.rm = TRUE)
  pct_soph <- round((soph_2024 - soph_2023) / soph_2023 * 100, 3)
  
  # Junior/Senior
  jrsr_2023 <- sum(df$jrsr == 1 & df$app == 1 & df$year == 2023, na.rm = TRUE)
  jrsr_2024 <- sum(df$jrsr == 1 & df$app == 1 & df$year == 2024, na.rm = TRUE)
  pct_jrsr <- round((jrsr_2024 - jrsr_2023) / jrsr_2023 * 100, 3)
  
  return(c(All = pct_all, Freshman = pct_fresh, Sophomore = pct_soph, OtherUndergrad = pct_jrsr))
}

# Build Table A2 - create list to store rows
tableA2_rows <- list()

# Overall - Total Submissions
tableA2_rows[[length(tableA2_rows) + 1]] <- c("Overall_All_Applicants", calc_pct_change(df))
tableA2_rows[[length(tableA2_rows) + 1]] <- c("Overall_Independent", calc_pct_change(df, "ind", 1))
tableA2_rows[[length(tableA2_rows) + 1]] <- c("Overall_Dependent", calc_pct_change(df, "dep", 1))

# ZIP Median Household Income - Total Submissions
tableA2_rows[[length(tableA2_rows) + 1]] <- c("MHI_First_Tercile", calc_pct_change(df, "mhi_1", 1))
tableA2_rows[[length(tableA2_rows) + 1]] <- c("MHI_Second_Tercile", calc_pct_change(df, "mhi_2", 1))
tableA2_rows[[length(tableA2_rows) + 1]] <- c("MHI_Third_Tercile", calc_pct_change(df, "mhi_3", 1))

# ZIP Non-White Share - Total Submissions
tableA2_rows[[length(tableA2_rows) + 1]] <- c("NonWhite_First_Tercile", calc_pct_change(df, "nw_1", 1))
tableA2_rows[[length(tableA2_rows) + 1]] <- c("NonWhite_Second_Tercile", calc_pct_change(df, "nw_2", 1))
tableA2_rows[[length(tableA2_rows) + 1]] <- c("NonWhite_Third_Tercile", calc_pct_change(df, "nw_3", 1))

# ZIP SNAP Share - Total Submissions
tableA2_rows[[length(tableA2_rows) + 1]] <- c("SNAP_First_Tercile", calc_pct_change(df, "snap_1", 1))
tableA2_rows[[length(tableA2_rows) + 1]] <- c("SNAP_Second_Tercile", calc_pct_change(df, "snap_2", 1))
tableA2_rows[[length(tableA2_rows) + 1]] <- c("SNAP_Third_Tercile", calc_pct_change(df, "snap_3", 1))

# Cal Grant Deadline (subset to cal == 1)
tryCatch({
  df_cal <- df[df$cal == 1, ]
  if (nrow(df_cal) == 0) {
    warning("No observations with cal == 1")
  } else {
    tableA2_rows[[length(tableA2_rows) + 1]] <- c("CalGrant_All_Applicants", calc_pct_change(df_cal))
    tableA2_rows[[length(tableA2_rows) + 1]] <- c("CalGrant_Independent", calc_pct_change(df_cal, "ind", 1))
    tableA2_rows[[length(tableA2_rows) + 1]] <- c("CalGrant_Dependent", calc_pct_change(df_cal, "dep", 1))
    
    tableA2_rows[[length(tableA2_rows) + 1]] <- c("CalGrant_MHI_First_Tercile", calc_pct_change(df_cal, "mhi_1", 1))
    tableA2_rows[[length(tableA2_rows) + 1]] <- c("CalGrant_MHI_Second_Tercile", calc_pct_change(df_cal, "mhi_2", 1))
    tableA2_rows[[length(tableA2_rows) + 1]] <- c("CalGrant_MHI_Third_Tercile", calc_pct_change(df_cal, "mhi_3", 1))
    
    tableA2_rows[[length(tableA2_rows) + 1]] <- c("CalGrant_NonWhite_First_Tercile", calc_pct_change(df_cal, "nw_1", 1))
    tableA2_rows[[length(tableA2_rows) + 1]] <- c("CalGrant_NonWhite_Second_Tercile", calc_pct_change(df_cal, "nw_2", 1))
    tableA2_rows[[length(tableA2_rows) + 1]] <- c("CalGrant_NonWhite_Third_Tercile", calc_pct_change(df_cal, "nw_3", 1))
    
    tableA2_rows[[length(tableA2_rows) + 1]] <- c("CalGrant_SNAP_First_Tercile", calc_pct_change(df_cal, "snap_1", 1))
    tableA2_rows[[length(tableA2_rows) + 1]] <- c("CalGrant_SNAP_Second_Tercile", calc_pct_change(df_cal, "snap_2", 1))
    tableA2_rows[[length(tableA2_rows) + 1]] <- c("CalGrant_SNAP_Third_Tercile", calc_pct_change(df_cal, "snap_3", 1))
  }
}, error = function(e) {
  warning(paste("Error processing Cal Grant data:", e$message))
})

# Convert list to data frame
tableA2_data <- do.call(rbind, lapply(tableA2_rows, function(x) {
  data.frame(
    Category = x[1],
    All = as.numeric(x[2]),
    Freshman = as.numeric(x[3]),
    Sophomore = as.numeric(x[4]),
    OtherUndergrad = as.numeric(x[5]),
    stringsAsFactors = FALSE
  )
}))

tryCatch({
  saveRDS(tableA2_data, file = file.path(tables_dir, "tableA2.Rds"))
  cat("Appendix Table A2 created and saved successfully\n")
  cat("  Rows:", nrow(tableA2_data), "\n")
  cat("  Row labels indicate: [Section]_[Group]_[Tercile if applicable]\n")
  cat("  Overall = Total submissions, CalGrant = Cal Grant deadline submissions\n")
  cat("  MHI = Median Household Income, NonWhite = Non-White Share, SNAP = SNAP Share\n\n")
}, error = function(e) {
  cat("ERROR: Failed to save Table A2:", e$message, "\n\n")
})

################################################################################
# PART 3: REGRESSION ANALYSIS SETUP
################################################################################

cat("="  , rep("=", 78), "\n", sep = "")
cat("PART 3: Setting up Regression Analysis\n")
cat("="  , rep("=", 78), "\n", sep = "")

# Filter to pre-2024 data and create year_factor
df_reg <- df %>% 
  filter(year != 2024) %>%
  mutate(year_factor = as.factor(year))

cat("Regression sample: ", nrow(df_reg), " observations\n")
cat("Year distribution:\n")
print(table(df_reg$year))
cat("\n")

# Covariate sets (following better_03.R)
covars_base <- c("dep", "uc", "csu", "fam_2", "fam_3", "fam_4", "fam_5", "fam_6",
                 "age", "inc_q1", "inc_q2", "inc_q3", "inc_q4")

covars_no_dep <- c("uc", "csu", "fam_2", "fam_3", "fam_4", "fam_5", "fam_6",
                   "age", "inc_q1", "inc_q2", "inc_q3", "inc_q4")

covars_no_sector <- c("dep", "fam_2", "fam_3", "fam_4", "fam_5", "fam_6",
                      "age", "inc_q1", "inc_q2", "inc_q3", "inc_q4")

################################################################################
# HELPER FUNCTIONS FOR REGRESSION TABLES
################################################################################

# Core regression function (following better_03.R approach)
run_reg <- function(data, outcome, covars) {
  rhs <- paste(c("year_factor", covars), collapse = " + ")
  formula_str <- as.formula(paste(outcome, "~", rhs, "| zip_code"))
  model <- feols(formula_str, data = data, cluster = "zip_code")
  return(model)
}

# Function to run regression for a group and extract results
run_regression_for_group <- function(data, outcome, covars, group_var = NULL, group_val = NULL) {
  # Filter if needed
  if (!is.null(group_var)) {
    filtered <- data[data[[group_var]] == group_val, ]
  } else {
    filtered <- data
  }
  
  # Calculate baseline using simple lm (BEFORE filtering NAs)
  baseline <- coef(lm(as.formula(paste(outcome, "~ year_factor")), data = filtered))[1]
  
  # Run full regression with fixest
  model <- run_reg(filtered, outcome, covars)
  
  # Extract year coefficients
  coef_table <- coeftable(model)
  year_coefs <- coef_table[grep("year_factor", rownames(coef_table)), , drop = FALSE]
  
  return(list(
    baseline = baseline,
    coef_2022 = year_coefs[1, 1],
    se_2022 = year_coefs[1, 2],
    coef_2023 = year_coefs[2, 1],
    se_2023 = year_coefs[2, 2],
    model = model
  ))
}

# Function to format coefficient with stars
format_coef <- function(coef, se) {
  if (is.na(coef) || is.na(se)) return(NA)
  t_stat <- abs(coef / se)
  stars <- ifelse(t_stat >= 2.576, "***",
                  ifelse(t_stat >= 1.96, "**",
                         ifelse(t_stat >= 1.645, "*", "")))
  sprintf("%.3f%s", coef, stars)
}

# Function to format standard error
format_se <- function(se) {
  if (is.na(se)) return(NA)
  sprintf("(%.3f)", se)
}

################################################################################
# TABLE 2: Changes in Refiling Outcomes by Education Level and Dependency Status
################################################################################

cat("="  , rep("=", 78), "\n", sep = "")
cat("Creating TABLE 2: Refiling by Education Level and Dependency\n")
cat("="  , rep("=", 78), "\n", sep = "")

# Panel A: By Education Level
cat("Running Panel A regressions (Education Level)...\n")

# app_next outcome
panel_a_app_all <- run_regression_for_group(df_reg, "app_next", covars_base)
panel_a_app_fresh <- run_regression_for_group(df_reg, "app_next", covars_base, "fresh", 1)
panel_a_app_soph <- run_regression_for_group(df_reg, "app_next", covars_base, "soph", 1)
panel_a_app_other <- run_regression_for_group(df_reg, "app_next", covars_base, "jrsr", 1)

# cal_next outcome
panel_a_cal_all <- run_regression_for_group(df_reg, "cal_next", covars_base)
panel_a_cal_fresh <- run_regression_for_group(df_reg, "cal_next", covars_base, "fresh", 1)
panel_a_cal_soph <- run_regression_for_group(df_reg, "cal_next", covars_base, "soph", 1)
panel_a_cal_other <- run_regression_for_group(df_reg, "cal_next", covars_base, "jrsr", 1)

# Panel B: By Dependency Status
cat("Running Panel B regressions (Dependency Status)...\n")

# app_next outcome (add education dummies for all/dep/ind)
panel_b_app_all <- run_regression_for_group(df_reg, "app_next", c(covars_base, "fresh", "soph", "jrsr"))
panel_b_app_dep <- run_regression_for_group(df_reg, "app_next", c(covars_no_dep, "fresh", "soph", "jrsr"), "dep", 1)
panel_b_app_ind <- run_regression_for_group(df_reg, "app_next", c(covars_no_dep, "fresh", "soph", "jrsr"), "ind", 1)

# cal_next outcome
panel_b_cal_all <- run_regression_for_group(df_reg, "cal_next", c(covars_base, "fresh", "soph", "jrsr"))
panel_b_cal_dep <- run_regression_for_group(df_reg, "cal_next", c(covars_no_dep, "fresh", "soph", "jrsr"), "dep", 1)
panel_b_cal_ind <- run_regression_for_group(df_reg, "cal_next", c(covars_no_dep, "fresh", "soph", "jrsr"), "ind", 1)

# Assemble Table 2
cat("Assembling Table 2...\n")

# Panel A - app_next section
panel_a_app_rows <- data.frame(
  Row = c("Baseline Mean Refile Rate (Mar 25)", 
          "", 
          "Refiled by Mar 25 in 2024",
          "SE_2024",
          "",
          "Refiled by Mar 25 in 2025",
          "SE_2025"),
  All = c(sprintf("%.3f", panel_a_app_all$baseline),
          "",
          format_coef(panel_a_app_all$coef_2022, panel_a_app_all$se_2022),
          format_se(panel_a_app_all$se_2022),
          "",
          format_coef(panel_a_app_all$coef_2023, panel_a_app_all$se_2023),
          format_se(panel_a_app_all$se_2023)),
  Freshman = c(sprintf("%.3f", panel_a_app_fresh$baseline),
               "",
               format_coef(panel_a_app_fresh$coef_2022, panel_a_app_fresh$se_2022),
               format_se(panel_a_app_fresh$se_2022),
               "",
               format_coef(panel_a_app_fresh$coef_2023, panel_a_app_fresh$se_2023),
               format_se(panel_a_app_fresh$se_2023)),
  Sophomore = c(sprintf("%.3f", panel_a_app_soph$baseline),
                "",
                format_coef(panel_a_app_soph$coef_2022, panel_a_app_soph$se_2022),
                format_se(panel_a_app_soph$se_2022),
                "",
                format_coef(panel_a_app_soph$coef_2023, panel_a_app_soph$se_2023),
                format_se(panel_a_app_soph$se_2023)),
  Other = c(sprintf("%.3f", panel_a_app_other$baseline),
            "",
            format_coef(panel_a_app_other$coef_2022, panel_a_app_other$se_2022),
            format_se(panel_a_app_other$se_2022),
            "",
            format_coef(panel_a_app_other$coef_2023, panel_a_app_other$se_2023),
            format_se(panel_a_app_other$se_2023)),
  stringsAsFactors = FALSE
)

# Panel A - cal_next section
panel_a_cal_rows <- data.frame(
  Row = c("Baseline Mean Refile Rate (Cal Grant)",
          "",
          "Refiled by March 2 in 2023",
          "SE_2023",
          "",
          "Refiled by June 2 in 2024",
          "SE_2024"),
  All = c(sprintf("%.3f", panel_a_cal_all$baseline),
          "",
          format_coef(panel_a_cal_all$coef_2022, panel_a_cal_all$se_2022),
          format_se(panel_a_cal_all$se_2022),
          "",
          format_coef(panel_a_cal_all$coef_2023, panel_a_cal_all$se_2023),
          format_se(panel_a_cal_all$se_2023)),
  Freshman = c(sprintf("%.3f", panel_a_cal_fresh$baseline),
               "",
               format_coef(panel_a_cal_fresh$coef_2022, panel_a_cal_fresh$se_2022),
               format_se(panel_a_cal_fresh$se_2022),
               "",
               format_coef(panel_a_cal_fresh$coef_2023, panel_a_cal_fresh$se_2023),
               format_se(panel_a_cal_fresh$se_2023)),
  Sophomore = c(sprintf("%.3f", panel_a_cal_soph$baseline),
                "",
                format_coef(panel_a_cal_soph$coef_2022, panel_a_cal_soph$se_2022),
                format_se(panel_a_cal_soph$se_2022),
                "",
                format_coef(panel_a_cal_soph$coef_2023, panel_a_cal_soph$se_2023),
                format_se(panel_a_cal_soph$se_2023)),
  Other = c(sprintf("%.3f", panel_a_cal_other$baseline),
            "",
            format_coef(panel_a_cal_other$coef_2022, panel_a_cal_other$se_2022),
            format_se(panel_a_cal_other$se_2022),
            "",
            format_coef(panel_a_cal_other$coef_2023, panel_a_cal_other$se_2023),
            format_se(panel_a_cal_other$se_2023)),
  stringsAsFactors = FALSE
)

# Panel B - app_next section
panel_b_app_rows <- data.frame(
  Row = c("Baseline Mean Refile Rate (Mar 25)",
          "",
          "Refiled by Mar 25 in 2024",
          "SE_2024",
          "",
          "Refiled by Mar 25 in 2025",
          "SE_2025"),
  All = c(sprintf("%.3f", panel_b_app_all$baseline),
          "",
          format_coef(panel_b_app_all$coef_2022, panel_b_app_all$se_2022),
          format_se(panel_b_app_all$se_2022),
          "",
          format_coef(panel_b_app_all$coef_2023, panel_b_app_all$se_2023),
          format_se(panel_b_app_all$se_2023)),
  Dependent = c(sprintf("%.3f", panel_b_app_dep$baseline),
                "",
                format_coef(panel_b_app_dep$coef_2022, panel_b_app_dep$se_2022),
                format_se(panel_b_app_dep$se_2022),
                "",
                format_coef(panel_b_app_dep$coef_2023, panel_b_app_dep$se_2023),
                format_se(panel_b_app_dep$se_2023)),
  Independent = c(sprintf("%.3f", panel_b_app_ind$baseline),
                  "",
                  format_coef(panel_b_app_ind$coef_2022, panel_b_app_ind$se_2022),
                  format_se(panel_b_app_ind$se_2022),
                  "",
                  format_coef(panel_b_app_ind$coef_2023, panel_b_app_ind$se_2023),
                  format_se(panel_b_app_ind$se_2023)),
  stringsAsFactors = FALSE
)

# Panel B - cal_next section
panel_b_cal_rows <- data.frame(
  Row = c("Baseline Mean Refile Rate (Cal Grant)",
          "",
          "Refiled by March 2 in 2023",
          "SE_2023",
          "",
          "Refiled by June 2 in 2024",
          "SE_2024"),
  All = c(sprintf("%.3f", panel_b_cal_all$baseline),
          "",
          format_coef(panel_b_cal_all$coef_2022, panel_b_cal_all$se_2022),
          format_se(panel_b_cal_all$se_2022),
          "",
          format_coef(panel_b_cal_all$coef_2023, panel_b_cal_all$se_2023),
          format_se(panel_b_cal_all$se_2023)),
  Dependent = c(sprintf("%.3f", panel_b_cal_dep$baseline),
                "",
                format_coef(panel_b_cal_dep$coef_2022, panel_b_cal_dep$se_2022),
                format_se(panel_b_cal_dep$se_2022),
                "",
                format_coef(panel_b_cal_dep$coef_2023, panel_b_cal_dep$se_2023),
                format_se(panel_b_cal_dep$se_2023)),
  Independent = c(sprintf("%.3f", panel_b_cal_ind$baseline),
                  "",
                  format_coef(panel_b_cal_ind$coef_2022, panel_b_cal_ind$se_2022),
                  format_se(panel_b_cal_ind$se_2022),
                  "",
                  format_coef(panel_b_cal_ind$coef_2023, panel_b_cal_ind$se_2023),
                  format_se(panel_b_cal_ind$se_2023)),
  stringsAsFactors = FALSE
)

# Combine into final Table 2 structure
table2 <- list(
  Panel_A_app_next = panel_a_app_rows,
  Panel_A_cal_next = panel_a_cal_rows,
  Panel_B_app_next = panel_b_app_rows,
  Panel_B_cal_next = panel_b_cal_rows
)

# Save Table 2
saveRDS(table2, file = file.path(tables_dir, "table2.Rds"))
cat("Table 2 saved successfully to:", file.path(tables_dir, "table2.Rds"), "\n\n")

################################################################################
# TABLE 3: Changes in refiling outcomes by tercile of predicted refiling
################################################################################

cat("="  , rep("=", 78), "\n", sep = "")
cat("Creating TABLE 3: Refiling by Tercile of Predicted Refiling\n")
cat("="  , rep("=", 78), "\n", sep = "")

# Covariates for logistic model
logit_covars <- c("ind", "fresh", "soph", "uc", "csu", "age",
                  "inc_q1", "inc_q2", "inc_q3", "inc_q4",
                  "fam_1", "fam_2", "fam_3", "fam_4",
                  "black", "hisp", "tmr", "white", "asian",
                  "mhi_2", "mhi_3", "nw_1", "nw_2", "snap_1", "snap_2")

# Function to create terciles based on predicted probabilities
create_terciles <- function(df, outcome_var, covariates) {
  cat("  Fitting logistic model for", outcome_var, "...\n")
  
  # Filter to 2021 for training
  df_21 <- df %>% filter(year == 2021)
  
  # Prepare data - remove missing values
  model_data <- df_21 %>%
    select(all_of(c(outcome_var, covariates, "zip_code"))) %>%
    drop_na()
  
  # Fit logistic model
  fml <- as.formula(paste(outcome_var, "~", paste(covariates, collapse = " + ")))
  model <- feglm(fml, data = model_data, family = binomial(link = "logit"))
  
  cat("  Model fitted. Predicting probabilities...\n")
  
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
  
  # Predict for all years
  df_21 <- predict_year(df %>% filter(year == 2021))
  df_22 <- predict_year(df %>% filter(year == 2022))
  df_23 <- predict_year(df %>% filter(year == 2023))
  
  # Create terciles based on 2021 predictions
  tercile_breaks <- quantile(df_21$prob, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  
  cat("  Tercile breaks:", paste(round(tercile_breaks, 3), collapse = ", "), "\n")
  
  # Assign terciles
  df_21$tercile <- cut(df_21$prob, breaks = tercile_breaks, 
                       labels = c("Low", "Middle", "High"), include.lowest = TRUE)
  df_22$tercile <- cut(df_22$prob, breaks = tercile_breaks, 
                       labels = c("Low", "Middle", "High"), include.lowest = TRUE)
  df_23$tercile <- cut(df_23$prob, breaks = tercile_breaks, 
                       labels = c("Low", "Middle", "High"), include.lowest = TRUE)
  
  # Combine
  combined <- bind_rows(df_21, df_22, df_23)
  
  return(combined)
}

# Create terciles for app_next
cat("Creating terciles for app_next...\n")
df_app_tercile <- create_terciles(df_reg, "app_next", logit_covars)

# Create terciles for cal_next
cat("Creating terciles for cal_next...\n")
df_cal_tercile <- create_terciles(df_reg, "cal_next", logit_covars)

# Run regressions for each tercile
cat("Running tercile regressions...\n")

# app_next - Low tercile
app_low <- run_regression_for_group(df_app_tercile %>% filter(tercile == "Low"), 
                                    "app_next", logit_covars)
# app_next - Middle tercile
app_mid <- run_regression_for_group(df_app_tercile %>% filter(tercile == "Middle"), 
                                    "app_next", logit_covars)
# app_next - High tercile
app_high <- run_regression_for_group(df_app_tercile %>% filter(tercile == "High"), 
                                     "app_next", logit_covars)

# cal_next - Low tercile
cal_low <- run_regression_for_group(df_cal_tercile %>% filter(tercile == "Low"), 
                                    "cal_next", logit_covars)
# cal_next - Middle tercile
cal_mid <- run_regression_for_group(df_cal_tercile %>% filter(tercile == "Middle"), 
                                    "cal_next", logit_covars)
# cal_next - High tercile
cal_high <- run_regression_for_group(df_cal_tercile %>% filter(tercile == "High"), 
                                     "cal_next", logit_covars)

# Assemble Table 3
cat("Assembling Table 3...\n")

table3_app <- data.frame(
  Row = c("Baseline Mean Refile Rate (by Mar 25)",
          "",
          "Refiled by Mar 25 in 2024",
          "SE_2024",
          "",
          "Refiled by Mar 25 in 2025",
          "SE_2025"),
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
  Row = c("Baseline Mean Refile Rate (by Cal Grant)",
          "",
          "Refiled by March 2 in 2023",
          "SE_2023",
          "",
          "Refiled by June 2 in 2024",
          "SE_2024"),
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
  cal_next = table3_cal
)

# Save Table 3
saveRDS(table3, file = file.path(tables_dir, "table3.Rds"))
cat("Table 3 saved successfully to:", file.path(tables_dir, "table3.Rds"), "\n\n")

################################################################################
# TABLE 4: Changes in Refiling, Payment, and Enrollment by College Sector
################################################################################

cat("="  , rep("=", 78), "\n", sep = "")
cat("Creating TABLE 4: Changes by College Sector\n")
cat("="  , rep("=", 78), "\n", sep = "")

# Panel A: Refiling Outcomes (app_next and cal_next)
cat("Running Panel A regressions (Refiling by Sector)...\n")

# app_next by sector (add education and dependency dummies)
panel_a_app_uc <- run_regression_for_group(df_reg, "app_next", c(covars_no_sector, "fresh", "soph", "jrsr", "dep", "ind"), "uc", 1)
panel_a_app_csu <- run_regression_for_group(df_reg, "app_next", c(covars_no_sector, "fresh", "soph", "jrsr", "dep", "ind"), "csu", 1)
panel_a_app_cc <- run_regression_for_group(df_reg, "app_next", c(covars_no_sector, "fresh", "soph", "jrsr", "dep", "ind"), "cc", 1)

# cal_next by sector
panel_a_cal_uc <- run_regression_for_group(df_reg, "cal_next", c(covars_no_sector, "fresh", "soph", "jrsr", "dep", "ind"), "uc", 1)
panel_a_cal_csu <- run_regression_for_group(df_reg, "cal_next", c(covars_no_sector, "fresh", "soph", "jrsr", "dep", "ind"), "csu", 1)
panel_a_cal_cc <- run_regression_for_group(df_reg, "cal_next", c(covars_no_sector, "fresh", "soph", "jrsr", "dep", "ind"), "cc", 1)

# Panel B: Payment Outcomes (paid) - CSU only
cat("Running Panel B regressions (Payment - CSU only)...\n")
panel_b_paid_csu <- run_regression_for_group(df_reg, "paid", c(covars_no_sector, "fresh", "soph", "jrsr", "dep", "ind"), "csu", 1)

# Panel C: Enrollment Outcomes (enr)
cat("Running Panel C regressions (Enrollment by Sector)...\n")
panel_c_enr_uc <- run_regression_for_group(df_reg, "enr_next", c(covars_no_sector, "fresh", "soph", "jrsr", "dep", "ind"), "uc", 1)
panel_c_enr_csu <- run_regression_for_group(df_reg, "enr_next", c(covars_no_sector, "fresh", "soph", "jrsr", "dep", "ind"), "csu", 1)
panel_c_enr_cc <- run_regression_for_group(df_reg, "enr_next", c(covars_no_sector, "fresh", "soph", "jrsr", "dep", "ind"), "cc", 1)

# Assemble Table 4
cat("Assembling Table 4...\n")

# Panel A - app_next
panel_a_app <- data.frame(
  Row = c("Baseline Mean Refile Rate (Mar 25)",
          "",
          "Refiled by Mar 25 in 2024",
          "SE_2024",
          "",
          "Refiled by Mar 25 in 2025",
          "SE_2025"),
  UC = c(sprintf("%.3f", panel_a_app_uc$baseline),
         "",
         format_coef(panel_a_app_uc$coef_2022, panel_a_app_uc$se_2022),
         format_se(panel_a_app_uc$se_2022),
         "",
         format_coef(panel_a_app_uc$coef_2023, panel_a_app_uc$se_2023),
         format_se(panel_a_app_uc$se_2023)),
  CSU = c(sprintf("%.3f", panel_a_app_csu$baseline),
          "",
          format_coef(panel_a_app_csu$coef_2022, panel_a_app_csu$se_2022),
          format_se(panel_a_app_csu$se_2022),
          "",
          format_coef(panel_a_app_csu$coef_2023, panel_a_app_csu$se_2023),
          format_se(panel_a_app_csu$se_2023)),
  CCC = c(sprintf("%.3f", panel_a_app_cc$baseline),
          "",
          format_coef(panel_a_app_cc$coef_2022, panel_a_app_cc$se_2022),
          format_se(panel_a_app_cc$se_2022),
          "",
          format_coef(panel_a_app_cc$coef_2023, panel_a_app_cc$se_2023),
          format_se(panel_a_app_cc$se_2023)),
  stringsAsFactors = FALSE
)

# Panel A - cal_next
panel_a_cal <- data.frame(
  Row = c("Baseline Mean Refile Rate (Cal Grant)",
          "",
          "Refiled by March 2 in 2023",
          "SE_2023",
          "",
          "Refiled by June 2 in 2024",
          "SE_2024"),
  UC = c(sprintf("%.3f", panel_a_cal_uc$baseline),
         "",
         format_coef(panel_a_cal_uc$coef_2022, panel_a_cal_uc$se_2022),
         format_se(panel_a_cal_uc$se_2022),
         "",
         format_coef(panel_a_cal_uc$coef_2023, panel_a_cal_uc$se_2023),
         format_se(panel_a_cal_uc$se_2023)),
  CSU = c(sprintf("%.3f", panel_a_cal_csu$baseline),
          "",
          format_coef(panel_a_cal_csu$coef_2022, panel_a_cal_csu$se_2022),
          format_se(panel_a_cal_csu$se_2022),
          "",
          format_coef(panel_a_cal_csu$coef_2023, panel_a_cal_csu$se_2023),
          format_se(panel_a_cal_csu$se_2023)),
  CCC = c(sprintf("%.3f", panel_a_cal_cc$baseline),
          "",
          format_coef(panel_a_cal_cc$coef_2022, panel_a_cal_cc$se_2022),
          format_se(panel_a_cal_cc$se_2022),
          "",
          format_coef(panel_a_cal_cc$coef_2023, panel_a_cal_cc$se_2023),
          format_se(panel_a_cal_cc$se_2023)),
  stringsAsFactors = FALSE
)

# Panel B - paid
panel_b_paid <- data.frame(
  Row = c("Baseline Mean Aid Receipt Rate",
          "",
          "Refiled by Mar 25 in 2024",
          "SE_2024",
          "",
          "Refiled by Mar 25 in 2025",
          "SE_2025"),
  CSU = c(sprintf("%.3f", panel_b_paid_csu$baseline),
          "",
          format_coef(panel_b_paid_csu$coef_2022, panel_b_paid_csu$se_2022),
          format_se(panel_b_paid_csu$se_2022),
          "",
          format_coef(panel_b_paid_csu$coef_2023, panel_b_paid_csu$se_2023),
          format_se(panel_b_paid_csu$se_2023)),
  stringsAsFactors = FALSE
)

# Panel C - enr
panel_c_enr <- data.frame(
  Row = c("Baseline Mean Continuity Rate",
          "",
          "Refiled by Mar 25 in 2024",
          "SE_2024",
          "",
          "Refiled by Mar 25 in 2025",
          "SE_2025"),
  UC = c(sprintf("%.3f", panel_c_enr_uc$baseline),
         "",
         format_coef(panel_c_enr_uc$coef_2022, panel_c_enr_uc$se_2022),
         format_se(panel_c_enr_uc$se_2022),
         "",
         format_coef(panel_c_enr_uc$coef_2023, panel_c_enr_uc$se_2023),
         format_se(panel_c_enr_uc$se_2023)),
  CSU = c(sprintf("%.3f", panel_c_enr_csu$baseline),
          "",
          format_coef(panel_c_enr_csu$coef_2022, panel_c_enr_csu$se_2022),
          format_se(panel_c_enr_csu$se_2022),
          "",
          format_coef(panel_c_enr_csu$coef_2023, panel_c_enr_csu$se_2023),
          format_se(panel_c_enr_csu$se_2023)),
  CCC = c(sprintf("%.3f", panel_c_enr_cc$baseline),
          "",
          format_coef(panel_c_enr_cc$coef_2022, panel_c_enr_cc$se_2022),
          format_se(panel_c_enr_cc$se_2022),
          "",
          format_coef(panel_c_enr_cc$coef_2023, panel_c_enr_cc$se_2023),
          format_se(panel_c_enr_cc$se_2023)),
  stringsAsFactors = FALSE
)

table4 <- list(
  Panel_A_app_next = panel_a_app,
  Panel_A_cal_next = panel_a_cal,
  Panel_B_paid = panel_b_paid,
  Panel_C_enr = panel_c_enr
)

# Save Table 4
saveRDS(table4, file = file.path(tables_dir, "table4.Rds"))
cat("Table 4 saved successfully to:", file.path(tables_dir, "table4.Rds"), "\n\n")

################################################################################
# TABLE 5: Changes in Enrollment Outcomes by Education Level and Dependency
################################################################################

cat("="  , rep("=", 78), "\n", sep = "")
cat("Creating TABLE 5: Enrollment by Education Level and Dependency\n")
cat("="  , rep("=", 78), "\n", sep = "")

# Panel A: By Education Level (enr outcome)
cat("Running Panel A regressions (Enrollment by Education Level)...\n")

panel_a_enr_fresh <- run_regression_for_group(df_reg, "enr", covars_base, "fresh", 1)
panel_a_enr_soph <- run_regression_for_group(df_reg, "enr", covars_base, "soph", 1)
panel_a_enr_other <- run_regression_for_group(df_reg, "enr", covars_base, "jrsr", 1)

# Panel B: By Dependency Status (enr outcome)
cat("Running Panel B regressions (Enrollment by Dependency)...\n")

panel_b_enr_dep <- run_regression_for_group(df_reg, "enr", c(covars_no_dep, "fresh", "soph", "jrsr"), "dep", 1)
panel_b_enr_ind <- run_regression_for_group(df_reg, "enr", c(covars_no_dep, "fresh", "soph", "jrsr"), "ind", 1)

# Assemble Table 5
cat("Assembling Table 5...\n")

# Panel A
panel_a_enr <- data.frame(
  Row = c("Baseline Mean Continuity Rate",
          "",
          "Refiled by Mar 25 in 2024",
          "SE_2024",
          "",
          "Refiled by Mar 25 in 2025",
          "SE_2025"),
  Freshman = c(sprintf("%.3f", panel_a_enr_fresh$baseline),
               "",
               format_coef(panel_a_enr_fresh$coef_2022, panel_a_enr_fresh$se_2022),
               format_se(panel_a_enr_fresh$se_2022),
               "",
               format_coef(panel_a_enr_fresh$coef_2023, panel_a_enr_fresh$se_2023),
               format_se(panel_a_enr_fresh$se_2023)),
  Sophomore = c(sprintf("%.3f", panel_a_enr_soph$baseline),
                "",
                format_coef(panel_a_enr_soph$coef_2022, panel_a_enr_soph$se_2022),
                format_se(panel_a_enr_soph$se_2022),
                "",
                format_coef(panel_a_enr_soph$coef_2023, panel_a_enr_soph$se_2023),
                format_se(panel_a_enr_soph$se_2023)),
  JuniorSenior = c(sprintf("%.3f", panel_a_enr_other$baseline),
                   "",
                   format_coef(panel_a_enr_other$coef_2022, panel_a_enr_other$se_2022),
                   format_se(panel_a_enr_other$se_2022),
                   "",
                   format_coef(panel_a_enr_other$coef_2023, panel_a_enr_other$se_2023),
                   format_se(panel_a_enr_other$se_2023)),
  stringsAsFactors = FALSE
)

# Panel B
panel_b_enr <- data.frame(
  Row = c("Baseline Mean Continuity Rate",
          "",
          "Refiled by Mar 25 in 2024",
          "SE_2024",
          "",
          "Refiled by Mar 25 in 2025",
          "SE_2025"),
  Dependent = c(sprintf("%.3f", panel_b_enr_dep$baseline),
                "",
                format_coef(panel_b_enr_dep$coef_2022, panel_b_enr_dep$se_2022),
                format_se(panel_b_enr_dep$se_2022),
                "",
                format_coef(panel_b_enr_dep$coef_2023, panel_b_enr_dep$se_2023),
                format_se(panel_b_enr_dep$se_2023)),
  Independent = c(sprintf("%.3f", panel_b_enr_ind$baseline),
                  "",
                  format_coef(panel_b_enr_ind$coef_2022, panel_b_enr_ind$se_2022),
                  format_se(panel_b_enr_ind$se_2022),
                  "",
                  format_coef(panel_b_enr_ind$coef_2023, panel_b_enr_ind$se_2023),
                  format_se(panel_b_enr_ind$se_2023)),
  stringsAsFactors = FALSE
)

table5 <- list(
  Panel_A = panel_a_enr,
  Panel_B = panel_b_enr
)

# Save Table 5
saveRDS(table5, file = file.path(tables_dir, "table5.Rds"))
cat("Table 5 saved successfully to:", file.path(tables_dir, "table5.Rds"), "\n\n")

################################################################################
# PART 4: FIGURES
################################################################################

cat("="  , rep("=", 78), "\n", sep = "")
cat("PART 4: Creating Figures\n")
cat("="  , rep("=", 78), "\n", sep = "")

# Define Okabe-Ito color palette
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7", "#000000")

# Cumulative submission plot function
plot_cumulative <- function(df, group_var, title = "", filename = NULL) {
  # Filter data
  df_filtered <- df[df[[group_var]] == 1 & df$day <= 538, ]
  
  # Aggregate by year and day
  df_agg <- aggregate(app ~ year + day, data = df_filtered, sum)
  df_agg <- df_agg[order(df_agg$year, df_agg$day), ]
  
  # Calculate cumulative totals
  df_agg$cumul_total <- ave(df_agg$app, df_agg$year, FUN = cumsum)
  
  years <- sort(unique(df_agg$year))
  
  # Set up plot
  if (!is.null(filename)) {
    png(filename, width = 8, height = 6, units = "in", res = 300)
  }
  
  plot(NA,
       xlim = c(0, 550),
       ylim = c(0, max(df_agg$cumul_total) * 1.05),
       xlab = "Day",
       ylab = "Cumulative Total",
       main = title,
       xaxt = "n",
       yaxt = "n")
  
  # Add axes
  axis(1, at = seq(0, 550, by = 50))
  axis(2, at = pretty(c(0, max(df_agg$cumul_total))),
       labels = format(pretty(c(0, max(df_agg$cumul_total))), big.mark = ","))
  
  # Add deadline lines
  abline(v = 153, lty = 2, col = "gray50")
  abline(v = 215, lty = 2, col = "gray50")
  
  # Add labels for deadlines
  text(x = 153, y = max(df_agg$cumul_total) * 0.95, 
       labels = "Cal Grant 21-23", pos = 2, col = "black", cex = 0.8)
  text(x = 215, y = max(df_agg$cumul_total) * 0.95, 
       labels = "Cal Grant 24", pos = 4, col = "black", cex = 0.8)
  
  # Plot lines for each year
  for (i in seq_along(years)) {
    year_data <- df_agg[df_agg$year == years[i], ]
    
    if (years[i] == 2024) {
      lines(year_data$day, year_data$cumul_total, col = "#000000", lwd = 2)
    } else {
      lines(year_data$day, year_data$cumul_total, col = okabe_ito[i], lwd = 2)
    }
  }
  
  # Add legend
  legend_colors <- c(okabe_ito[1:3], "#000000")
  legend("bottomright",
         legend = years,
         col = legend_colors,
         lwd = 2,
         title = "Year",
         bty = "n")
  
  if (!is.null(filename)) {
    dev.off()
    cat("  Saved:", filename, "\n")
  }
}

# Create 4-panel figure (Figure 1)
cat("Creating Figure 1: Cumulative submissions by education level\n")

tryCatch({
  png(file.path(figures_dir, "figure1_education_level.png"), 
      width = 12, height = 9, units = "in", res = 300)
  
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  
  # Add an "all" indicator for the first panel
  df$all <- 1
  
  plot_cumulative(df, "all", title = "All Applicants")
  plot_cumulative(df, "fresh", title = "Freshman Applicants")
  plot_cumulative(df, "soph", title = "Sophomore Applicants")
  plot_cumulative(df, "jrsr", title = "Other Undergraduate Applicants")
  
  dev.off()
  cat("  Figure 1 saved successfully\n")
}, error = function(e) {
  cat("ERROR creating Figure 1:", e$message, "\n")
  if (dev.cur() > 1) dev.off()
})

# Create 2-panel figure (Figure 2)
cat("Creating Figure 2: Cumulative submissions by dependency status\n")

tryCatch({
  png(file.path(figures_dir, "figure2_dependency_status.png"), 
      width = 12, height = 5, units = "in", res = 300)
  
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  
  plot_cumulative(df, "ind", title = "Independent Applicants")
  plot_cumulative(df, "dep", title = "Dependent Applicants")
  
  dev.off()
  cat("  Figure 2 saved successfully\n\n")
}, error = function(e) {
  cat("ERROR creating Figure 2:", e$message, "\n\n")
  if (dev.cur() > 1) dev.off()
})

################################################################################
# SUMMARY
################################################################################

cat("="  , rep("=", 78), "\n", sep = "")
cat("ANALYSIS COMPLETE\n")
cat("="  , rep("=", 78), "\n", sep = "")

cat("\nAll tables and figures have been generated and saved to:\n")
cat("  Tables:", tables_dir, "\n")
cat("  Figures:", figures_dir, "\n\n")

cat("Tables created:\n")
cat("  - summary_tables.Rds (Table 1, Appendix Table A1)\n")
cat("  - tableA2.Rds (Appendix Table A2)\n")
cat("  - table2.Rds (Table 2 - all panels, properly assembled)\n")
cat("  - table3.Rds (Table 3 - tercile analysis, properly assembled)\n")
cat("  - table4.Rds (Table 4 - sector analysis, properly assembled)\n")
cat("  - table5.Rds (Table 5 - enrollment analysis, properly assembled)\n\n")

cat("Figures created:\n")
cat("  - figure1_education_level.png (Figure 1)\n")
cat("  - figure2_dependency_status.png (Figure 2)\n\n")

cat("To use the regression tables:\n")
cat("  table2 <- readRDS('", file.path(tables_dir, "table2.Rds"), "')\n", sep = "")
cat("  # Access Panel A, app_next section: table2$Panel_A_app_next\n")
cat("  # This is a data frame with formatted coefficients ready for LaTeX\n\n")

cat("Each regression table contains:\n")
cat("  - Baseline means formatted to 3 decimal places\n")
cat("  - Coefficients with significance stars (*, **, ***)\n")
cat("  - Standard errors in parentheses\n")
cat("  - All values ready to insert into LaTeX tables\n\n")

cat("Script completed successfully!\n")
cat("="  , rep("=", 78), "\n", sep = "")