################################################################################
# BETTER FAFSA - TABLE 4
# Changes in Refiling, Payment, and Enrollment by College Sector
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

# Filter to pre-2024 data and create year_factor
df_reg <- df %>% 
  filter(year != 2024) %>%
  mutate(year_factor = as.factor(year))

# Covariate set (no sector dummies since we're splitting by sector)
covars_no_sector <- c(
  "dep",
  "fam_2",
  "fam_3",
  "fam_4",
  "fam_5",
  "fam_6",
  "age",
  "inc_q1",
  "inc_q2",
  "inc_q3",
  "inc_q4"
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

# PANEL A: Refiling Outcomes (app_next and cal_next)
cat("Generating Panel A: Refiling Outcomes by Sector\n")

# app_next by sector (add education and dependency dummies)
panel_a_app_uc <- run_regression_for_group(df_reg, "app_next", c(covars_no_sector, "fresh", "soph", "jrsr"), "uc", 1)
panel_a_app_csu <- run_regression_for_group(df_reg, "app_next", c(covars_no_sector, "fresh", "soph", "jrsr"), "csu", 1)
panel_a_app_cc <- run_regression_for_group(df_reg, "app_next", c(covars_no_sector, "fresh", "soph", "jrsr"), "cc", 1)

# cal_next by sector
panel_a_cal_uc <- run_regression_for_group(df_reg, "cal_next", c(covars_no_sector, "fresh", "soph", "jrsr"), "uc", 1)
panel_a_cal_csu <- run_regression_for_group(df_reg, "cal_next", c(covars_no_sector, "fresh", "soph", "jrsr"), "csu", 1)
panel_a_cal_cc <- run_regression_for_group(df_reg, "cal_next", c(covars_no_sector, "fresh", "soph", "jrsr"), "cc", 1)

cat("  ✓ Panel A completed\n\n")

# PANEL B: Payment Continuation (paid -> paid_next, like re-enrollment)
cat("Generating Panel B: Aid Continuation by Sector\n")

# Filter to students who received aid in year X
df_reg_paid <- df_reg %>% filter(paid == 1)

cat("  Payment sample (students who received aid):", nrow(df_reg_paid), "\n")

panel_b_paid_uc <- run_regression_for_group(df_reg_paid, "paid_next", c(covars_no_sector, "fresh", "soph", "jrsr"), "uc", 1)
panel_b_paid_csu <- run_regression_for_group(df_reg_paid, "paid_next", c(covars_no_sector, "fresh", "soph", "jrsr"), "csu", 1)
panel_b_paid_cc <- run_regression_for_group(df_reg_paid, "paid_next", c(covars_no_sector, "fresh", "soph", "jrsr"), "cc", 1)

cat("  ✓ Panel B completed\n\n")

# PANEL C: Re-enrollment (enr -> enr_next)
cat("Generating Panel C: Re-enrollment by Sector\n")

# Filter to students who were enrolled in year X
df_reg_enr <- df_reg %>% filter(enr == 1)

cat("  Enrollment sample (students who were enrolled):", nrow(df_reg_enr), "\n")

panel_c_enr_uc <- run_regression_for_group(df_reg_enr, "enr_next", c(covars_no_sector, "fresh", "soph", "jrsr"), "uc", 1)
panel_c_enr_csu <- run_regression_for_group(df_reg_enr, "enr_next", c(covars_no_sector, "fresh", "soph", "jrsr"), "csu", 1)
panel_c_enr_cc <- run_regression_for_group(df_reg_enr, "enr_next", c(covars_no_sector, "fresh", "soph", "jrsr"), "cc", 1)

cat("  ✓ Panel C completed\n\n")

# Assemble Panel A - app_next
panel_a_app <- data.frame(
  Row = c("Baseline Mean Refile Rate (2022-23)",
          "",
          "Refiled in 2023-24",
          "SE_2023-24",
          "",
          "Refiled in 2024-25",
          "SE_2024-25"),
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
  Row = c("Baseline Mean Refile Rate - Cal Grant (2022-23)",
          "",
          "Refiled by March 2 in 2023-24",
          "SE_2023-24",
          "",
          "Refiled by June 2 in 2024-25",
          "SE_2024-25"),
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

# Panel B - paid_next (aid continuation)
panel_b_paid <- data.frame(
  Row = c("Baseline Mean Aid Continuation Rate (2022-23)",
          "",
          "Continued aid in 2023-24",
          "SE_2023-24",
          "",
          "Continued aid in 2024-25",
          "SE_2024-25"),
  UC = c(sprintf("%.3f", panel_b_paid_uc$baseline),
         "",
         format_coef(panel_b_paid_uc$coef_2022, panel_b_paid_uc$se_2022),
         format_se(panel_b_paid_uc$se_2022),
         "",
         format_coef(panel_b_paid_uc$coef_2023, panel_b_paid_uc$se_2023),
         format_se(panel_b_paid_uc$se_2023)),
  CSU = c(sprintf("%.3f", panel_b_paid_csu$baseline),
          "",
          format_coef(panel_b_paid_csu$coef_2022, panel_b_paid_csu$se_2022),
          format_se(panel_b_paid_csu$se_2022),
          "",
          format_coef(panel_b_paid_csu$coef_2023, panel_b_paid_csu$se_2023),
          format_se(panel_b_paid_csu$se_2023)),
  CCC = c(sprintf("%.3f", panel_b_paid_cc$baseline),
          "",
          format_coef(panel_b_paid_cc$coef_2022, panel_b_paid_cc$se_2022),
          format_se(panel_b_paid_cc$se_2022),
          "",
          format_coef(panel_b_paid_cc$coef_2023, panel_b_paid_cc$se_2023),
          format_se(panel_b_paid_cc$se_2023)),
  stringsAsFactors = FALSE
)

# Panel C - enr_next (re-enrollment)
panel_c_enr <- data.frame(
  Row = c("Baseline Mean Re-enrollment Rate (2022-23)",
          "",
          "Re-enrolled in 2023-24",
          "SE_2023-24",
          "",
          "Re-enrolled in 2024-25",
          "SE_2024-25"),
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

cat("✓ Table 4 created and saved successfully\n")
cat("  Location:", file.path(tables_dir, "table4.Rds"), "\n\n")

# Cleanup
rm(panel_a_app_uc, panel_a_app_csu, panel_a_app_cc)
rm(panel_a_cal_uc, panel_a_cal_csu, panel_a_cal_cc)
rm(panel_b_paid_uc, panel_b_paid_csu, panel_b_paid_cc)
rm(panel_c_enr_uc, panel_c_enr_csu, panel_c_enr_cc)
rm(panel_a_app, panel_a_cal, panel_b_paid, panel_c_enr)
rm(df_reg_paid, df_reg_enr)
rm(covars_no_sector)
rm(run_reg, run_regression_for_group, format_coef, format_se)