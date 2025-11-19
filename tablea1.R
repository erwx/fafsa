################################################################################
# BETTER FAFSA - APPENDIX TABLE A1
# Summary statistics by dependency status
################################################################################

# Load required libraries
library(dplyr)

# Directories
project_dir  <- "/home/erik/Repos/fafsa"
data_dir     <- file.path(project_dir, "data")
tables_dir   <- file.path(project_dir, "tables")

# Create directories if they don't exist
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

# Load data
df <- readRDS(file.path(data_dir, "clean6.Rds"))

# Define variables for summary statistics
summary_vars <- c(
  "app_prev",
  "ind",
  "fam_size",
  "age",
  "fresh",
  "soph",
  "jrsr",
  "inc",
  "mhi",
  "asian",
  "black",
  "hisp",
  "tmr",
  "white",
  "snap",
  "uc",
  "csu",
  "cc",
  "paid",
  "paid_cc",
  "paid_csu",
  "paid_uc",
  "enr"
)

# Summary table function
create_summary <- function(df, vars, group_name = "All") {
  means <- round(colMeans(df[vars], na.rm = TRUE), 2)
  sds   <- round(apply(df[vars], 2, sd, na.rm = TRUE), 2)
  
  summary_table <- data.frame(
    Variable = vars,
    Summary = paste0(sprintf("%.2f", means), "(", sprintf("%.2f", sds), ")")
  )
  
  summary_table[nrow(summary_table) + 1, ] <- c("N", format(nrow(df), big.mark = ","))
  colnames(summary_table)[2] <- group_name
  
  return(summary_table)
}

# Create Appendix Table A1 by dependency status
all_summary <- create_summary(df, summary_vars, "All")
dep_summary <- create_summary(df[df$dep == 1, ], summary_vars, "Dependent")
ind_summary <- create_summary(df[df$ind == 1, ], summary_vars, "Independent")

tableA1 <- cbind(all_summary, dep_summary[, 2, drop = FALSE], ind_summary[, 2, drop = FALSE])

# Save Appendix Table A1
saveRDS(tableA1, file = file.path(tables_dir, "tableA1.Rds"))

# Cleanup
rm(all_summary, dep_summary, ind_summary, summary_vars, create_summary)