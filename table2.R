################################################################################
# BETTER FAFSA - TABLE 2
# Changes in Refiling Outcomes by Education Level and Dependency Status
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

# Covariate sets
covars_base <- c(
  "dep",
  "uc",
  "csu",
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

covars_no_dep <- c(
  "uc",
  "csu",
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

# PANEL A: By Education Level
# app_next outcome
panel_a_app_all   <- run_regression_for_group(df_reg, "app_next", covars_base)
panel_a_app_fresh <- run_regression_for_group(df_reg, "app_next", covars_base, "fresh", 1)
panel_a_app_soph  <- run_regression_for_group(df_reg, "app_next", covars_base, "soph", 1)
panel_a_app_other <- run_regression_for_group(df_reg, "app_next", covars_base, "jrsr", 1)

# cal_next outcome
panel_a_cal_all   <- run_regression_for_group(df_reg, "cal_next", covars_base)
panel_a_cal_fresh <- run_regression_for_group(df_reg, "cal_next", covars_base, "fresh", 1)
panel_a_cal_soph  <- run_regression_for_group(df_reg, "cal_next", covars_base, "soph", 1)
panel_a_cal_other <- run_regression_for_group(df_reg, "cal_next", covars_base, "jrsr", 1)

# PANEL B: By Dependency Status
# app_next outcome (add education dummies for all/dep/ind)
panel_b_app_all <- run_regression_for_group(df_reg, "app_next", c(covars_base, "fresh", "soph", "jrsr"))
panel_b_app_dep <- run_regression_for_group(df_reg, "app_next", c(covars_no_dep, "fresh", "soph", "jrsr"), "dep", 1)
panel_b_app_ind <- run_regression_for_group(df_reg, "app_next", c(covars_no_dep, "fresh", "soph", "jrsr"), "ind", 1)

# cal_next outcome
panel_b_cal_all <- run_regression_for_group(df_reg, "cal_next", c(covars_base, "fresh", "soph", "jrsr"))
panel_b_cal_dep <- run_regression_for_group(df_reg, "cal_next", c(covars_no_dep, "fresh", "soph", "jrsr"), "dep", 1)
panel_b_cal_ind <- run_regression_for_group(df_reg, "cal_next", c(covars_no_dep, "fresh", "soph", "jrsr"), "ind", 1)

# Assemble Panel A - app_next section
panel_a_app_rows <- data.frame(
  Row = c("Baseline Mean Refile Rate (2022-23)", 
          "", 
          "Refiled in 2023-24",
          "SE_2023-24",
          "",
          "Refiled in 2024-25",
          "SE_2024-25"),
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
  Row = c("Baseline Mean Refile Rate - Cal Grant (2022-23)",
          "",
          "Refiled by March 2 in 2023-24",
          "SE_2023-24",
          "",
          "Refiled by June 2 in 2024-25",
          "SE_2024-25"),
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
  Row = c("Baseline Mean Refile Rate (2022-23)",
          "",
          "Refiled in 2023-24",
          "SE_2023-24",
          "",
          "Refiled in 2024-25",
          "SE_2024-25"),
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
  Row = c("Baseline Mean Refile Rate - Cal Grant (2022-23)",
          "",
          "Refiled by March 2 in 2023-24",
          "SE_2023-24",
          "",
          "Refiled by June 2 in 2024-25",
          "SE_2024-25"),
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

cat("✓ Table 2 created and saved successfully\n")
cat("  Location:", file.path(tables_dir, "table2.Rds"), "\n\n")

# Cleanup
rm(panel_a_app_all, panel_a_app_fresh, panel_a_app_soph, panel_a_app_other)
rm(panel_a_cal_all, panel_a_cal_fresh, panel_a_cal_soph, panel_a_cal_other)
rm(panel_b_app_all, panel_b_app_dep, panel_b_app_ind)
rm(panel_b_cal_all, panel_b_cal_dep, panel_b_cal_ind)
rm(panel_a_app_rows, panel_a_cal_rows, panel_b_app_rows, panel_b_cal_rows)
rm(covars_base, covars_no_dep)
rm(run_reg, run_regression_for_group, format_coef, format_se)