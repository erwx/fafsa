################################################################################
# BETTER FAFSA - TABLE 1
# Summary statistics by year
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

# Create Table 1 by year
summary_2021 <- create_summary(df[df$year == 2021, ], summary_vars, "Y2021")
summary_2022 <- create_summary(df[df$year == 2022, ], summary_vars, "Y2022")
summary_2023 <- create_summary(df[df$year == 2023, ], summary_vars, "Y2023")
summary_2024 <- create_summary(df[df$year == 2024, ], summary_vars, "Y2024")

table1 <- cbind(summary_2021, 
                summary_2022[, 2, drop = FALSE], 
                summary_2023[, 2, drop = FALSE], 
                summary_2024[, 2, drop = FALSE])

# Save Table 1
saveRDS(table1, file = file.path(tables_dir, "table1.Rds"))

# Cleanup
rm(summary_2021, summary_2022, summary_2023, summary_2024)
rm(summary_vars, create_summary)