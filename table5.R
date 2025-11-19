################################################################################
# BETTER FAFSA - TABLE 5
# Changes in Re-enrollment Outcomes by Education Level and Dependency
################################################################################

cat("\n>>> Generating Table 5: Re-enrollment Outcomes\n\n")

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

# Filter to pre-2024 data, enrolled students only, and create year_factor
df_reg <- df %>% 
  filter(year != 2024, enr == 1) %>%
  mutate(year_factor = as.factor(year))

cat("Regression sample (years 2021-2023, enrolled students):", nrow(df_reg), "\n\n")

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

# Panel A: By Education Level
cat("Generating Panel A: By Education Level\n")

panel_a_enr_fresh <- run_regression_for_group(df_reg, "enr_next", covars_base, "fresh", 1)
panel_a_enr_soph <- run_regression_for_group(df_reg, "enr_next", covars_base, "soph", 1)
panel_a_enr_other <- run_regression_for_group(df_reg, "enr_next", covars_base, "jrsr", 1)

panel_a_enr <- data.frame(
  Row = c("Baseline Mean Re-enrollment Rate (2022-23)",
          "",
          "Re-enrolled in 2023-24",
          "SE_2023-24",
          "",
          "Re-enrolled in 2024-25",
          "SE_2024-25"),
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

cat("  ✓ Panel A completed\n\n")

# Panel B: By Dependency Status
cat("Generating Panel B: By Dependency Status\n")

panel_b_enr_dep <- run_regression_for_group(df_reg, "enr_next", c(covars_no_dep, "fresh", "soph", "jrsr"), "dep", 1)
panel_b_enr_ind <- run_regression_for_group(df_reg, "enr_next", c(covars_no_dep, "fresh", "soph", "jrsr"), "ind", 1)

panel_b_enr <- data.frame(
  Row = c("Baseline Mean Re-enrollment Rate (2022-23)",
          "",
          "Re-enrolled in 2023-24",
          "SE_2023-24",
          "",
          "Re-enrolled in 2024-25",
          "SE_2024-25"),
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

cat("  ✓ Panel B completed\n\n")

# Combine panels
table5 <- list(
  Panel_A = panel_a_enr,
  Panel_B = panel_b_enr
)

# Save Table 5
saveRDS(table5, file = file.path(tables_dir, "table5.Rds"))

cat("✓ Table 5 created and saved successfully\n")
cat("  Location:", file.path(tables_dir, "table5.Rds"), "\n\n")

# Cleanup
rm(panel_a_enr_fresh, panel_a_enr_soph, panel_a_enr_other)
rm(panel_b_enr_dep, panel_b_enr_ind)
rm(panel_a_enr, panel_b_enr)
rm(covars_base, covars_no_dep)
rm(run_reg, run_regression_for_group, format_coef, format_se)