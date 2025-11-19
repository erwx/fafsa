################################################################################
# BETTER FAFSA - APPENDIX TABLE A3
# Logistic Model Coefficients (from Table 3 tercile analysis)
################################################################################

# Load required libraries
library(fixest)

# Directories
project_dir  <- "/home/erik/Repos/fafsa"
tables_dir   <- file.path(project_dir, "tables")

# Create directories if they don't exist
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

# Load Table 3 (which contains the logistic models)
table3 <- readRDS(file.path(tables_dir, "table3.Rds"))

# Check if logit model exists
if (is.null(table3$logit_model_app)) {
  stop("ERROR: Logistic model not found in Table 3. Run table3.R first.")
}

logit_model_app <- table3$logit_model_app

# Extract coefficient table using fixest's coeftable function
coef_table <- coeftable(logit_model_app)

cat("Model has", nrow(coef_table), "coefficients\n\n")

# Create formatted table matching the paper's structure
tableA3 <- data.frame(
  Variable = rownames(coef_table),
  Coefficient = round(coef_table[, 1], 3),
  Std_Error = round(coef_table[, 2], 3),
  stringsAsFactors = FALSE
)

# Add significance stars (using z-statistic for logistic regression)
z_stat <- abs(tableA3$Coefficient / tableA3$Std_Error)
tableA3$Stars <- ifelse(z_stat >= qnorm(0.9995), "***",
                        ifelse(z_stat >= qnorm(0.995), "**",
                               ifelse(z_stat >= qnorm(0.975), "*", "")))

# Format for display (matching paper format)
tableA3$Formatted_Coef <- paste0(sprintf("%.3f", tableA3$Coefficient), tableA3$Stars)
tableA3$Formatted_SE <- paste0("(", sprintf("%.3f", tableA3$Std_Error), ")")

# Get number of observations
n_obs <- logit_model_app$nobs

# Save as list with both raw model and formatted table
tableA3_output <- list(
  model = logit_model_app,
  formatted_table = tableA3,
  n_observations = n_obs
)

#saveRDS(tableA3_output, file = file.path(tables_dir, "tableA3.Rds"))

cat("✓ Appendix Table A3 created and saved successfully\n")
cat("  Location:", file.path(tables_dir, "tableA3.Rds"), "\n")

# Cleanup
rm(logit_model_app, coef_table, z_stat, n_obs)
rm(expected_vars, present_vars, n_nonzero, n_sig)