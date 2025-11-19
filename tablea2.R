################################################################################
# BETTER FAFSA - APPENDIX TABLE A2
# Percent changes from 2023 to 2024
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

# Function to calculate percent changes from 2023 to 2024
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
  
  return(
    c(
      All = pct_all,
      Freshman = pct_fresh,
      Sophomore = pct_soph,
      OtherUndergrad = pct_jrsr
    )
  )
}

# Build Table A2
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
df_cal <- df[df$cal == 1, ]

if (nrow(df_cal) > 0) {
  cat("  Processing Cal Grant deadline data...\n")
  
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
} else {
  cat("  ⚠ WARNING: No observations with cal == 1\n")
}

# Convert list to data frame
tableA2 <- do.call(rbind, lapply(tableA2_rows, function(x) {
  data.frame(
    Category = x[1],
    All = as.numeric(x[2]),
    Freshman = as.numeric(x[3]),
    Sophomore = as.numeric(x[4]),
    OtherUndergrad = as.numeric(x[5]),
    stringsAsFactors = FALSE
  )
}))

# Save Table A2
#saveRDS(tableA2, file = file.path(tables_dir, "tableA2.Rds"))

# Cleanup
rm(df_cal, tableA2_rows, calc_pct_change)
rm(n_2023, n_2024, manual_pct_change, table_pct_change)