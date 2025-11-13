################################################################################
# BETTER FAFSA
# This script cleans and produces an Rds file
################################################################################

rm(list = ls())
gc()

# Load required libraries
library(dplyr)
library(tidyr)
library(fixest)
library(haven)

# Set options
options(scipen = 999)

# Directories ----
project_dir  <- "/home/erik/Repos/fafsa"
data_dir     <- file.path(project_dir, "data")

setwd(project_dir)

# Load data ----
tryCatch({
  df <- read_dta(file.path(data_dir, "better6A.dta"))
  cat("Data loaded successfully.\n")
  cat("Total observations:", nrow(df), "\n")
}, error = function(e) {
  stop("ERROR: Could not load data file. ", e$message)
})

df <- as.data.frame(lapply(df, as.vector))  # Remove Stata labels

missing.summary <- function(df) {
  n_miss   <- sapply(df, function(x) sum(is.na(x)))
  n_rows   <- nrow(df)
  pct_miss <- (n_miss / n_rows) * 100
  sum_tab  <- data.frame(
    N_Rows   = n_rows,
    N_Miss   = n_miss,
    Pct_Miss = round(pct_miss,1)
  )
  return(sum_tab)
}

df <- df[df$stdt_bach_deg_flg == 2, ] # Remove applicants with a degree

vars_2024_only <- c()
for (col in names(df)) {
  # Check if there's data for the variable in years other than 2024
  data_other_years <- df[df$year != 2024, col]
  # If all values for years other than 2024 are NA, add the column to the list
  if (all(is.na(data_other_years))) {
    vars_2024_only <- c(vars_2024_only, col)
  }
}
vars_2024_only

cols_to_keep <- c(
  "fid",
  "year",
  "txn_nbr",
  "date_app_rcvd",
  "dep_stat",
  "stdt_zip_cde",
  "stdt_zip_cde_2024",
  "stdt_el",
  "stdt_nbr_fam_mbr",
  "par_nbr_fam_mbr",
  "stdt_tot_inc",
  "tot_inc_num",
  "par_zip_code",
  "par_tot_inc",
  "stdt_dob_yr",
  "stdt_hs_code"
  )


df <- df[cols_to_keep]


rm(cols_to_keep)


df$date_app_rcvd <- as.character(df$date_app_rcvd)
df$year_rcvd     <- as.integer(substr(df$date_app_rcvd, 1, 4))
df$month_rcvd    <- as.integer(substr(df$date_app_rcvd, 5, 6))
df$day_rcvd      <- as.integer(substr(df$date_app_rcvd, 7, 8))
df$date_app_rcvd <- as.integer(df$date_app_rcvd)


df_sorted <- df[order(
  df$fid,      # Group 1    fid
  df$year,     # Group 2    year within fid
  -df$txn_nbr  # Sort key   txt_nbr (highest first)
), ]


df <- df_sorted[!duplicated(df_sorted[, c("fid", "year")]), ]

df$txn_nbr <- NULL

rm(df_sorted)
gc()

df     <- df[df$dep_stat %in% c("D", "I"), ] # Remove incomplete applications

df$dep <- ifelse(df$dep_stat == "D", 1, 0)   # Make dependent dummy
df$ind <- ifelse(df$dep_stat == "I", 1, 0)   # Make independent dummy

# https://studentaid.gov/sites/default/files/2023-24-fafsa.pdf
# https://studentaid.gov/sites/default/files/2024-25-fafsa.pdf

df <- df[!(df$year == 2024 & df$stdt_el == 4), ]      # Remove grad students
df <- df[!(df$year  < 2024  & df$stdt_el %in% 6:7), ] # Remove grad students

df$fresh <- as.integer(df$stdt_el < 2)   # Make freshman dummy
df$soph  <- as.integer(df$stdt_el == 2)  # Make sophomore dummy
df$jrsr  <- as.integer(df$stdt_el > 2)   # Make junior and beyond dummy

df$stdt_el <- NULL

df$par_zip_code <- substr(df$par_zip_code, 1, 5)
df$stdt_zip_cde_2024 <- substr(df$stdt_zip_cde_2024, 1, 5)
df$stdt_zip_cde <- as.character(df$stdt_zip_cde)

df$zip_code <- ifelse(
  df$year == 2024,
  df$stdt_zip_cde_2024,
  df$stdt_zip_cde
  )

idx              <- is.na(df$stdt_zip_cde) & df$dep_stat == "D"
df$zip_code[idx] <- df$par_zip_code[idx]

rm(idx)
gc()

nrow(df)
df <- df[!is.na(df$zip_code), ]
nrow(df)

df$par_zip_code <- NULL
df$stdt_zip_cde <- NULL

table(df$year)
nrow(df)

df <- df[df$zip_code != 0, ]       # 0 isn't a valid zip code
df <- df[df$zip_code != "00000", ] # 00000 isn't a valid zip code
df <- df[nchar(df$zip_code) > 4, ] # 4-digit zip codes aren't valid

# These lines drop 2049 students

table(df$year)
nrow(df)

df$inc    <- df$tot_inc_num
i         <- is.na(df$inc) & df$dep_stat == "I"
df$inc[i] <- df$stdt_tot_inc[i]
i         <- is.na(df$inc)
df$inc[i] <- df$par_tot_inc[i]
rm(i)

df$tot_inc_num  <- NULL
df$stdt_tot_inc <- NULL
df$par_tot_inc  <- NULL

#+++ Income Winsorization
high_cut_point <-  quantile(df$inc, .95, na.rm = TRUE)
low_cut_point  <-  quantile(df$inc, .05, na.rm = TRUE)

df$inc <- ifelse(df$inc < low_cut_point, low_cut_point, df$inc)
df$inc <- ifelse(df$inc > high_cut_point, high_cut_point, df$inc)

df$inc <- ifelse(df$year == 2024, NA, df$inc)

rm(high_cut_point, low_cut_point)

# Calculate quintiles and create dummies
df <- within(df, {
  qtile_inc <- as.integer(cut(
    inc, 
    breaks = quantile(
      inc, probs = seq(0, 1, 0.2), include.lowest = TRUE, na.rm = TRUE
      )
    )
  )

  inc_q1 <- as.integer(qtile_inc == 1)
  inc_q2 <- as.integer(qtile_inc == 2)
  inc_q3 <- as.integer(qtile_inc == 3)
  inc_q4 <- as.integer(qtile_inc == 4)
  inc_q5 <- as.integer(qtile_inc == 5)
})

df$qtile_inc <- NULL

df$fam_size             <- df$stdt_nbr_fam_mbr
use_parent              <- is.na(df$stdt_nbr_fam_mbr) & df$dep_stat == "D"
df$fam_size[use_parent] <- df$par_nbr_fam_mbr[use_parent]

rm(use_parent)
gc()

df$stdt_nbr_fam_mbr <- NULL
df$par_nbr_fam_mbr  <- NULL

df$fam_size <- ifelse(df$fam_size >= 6, 6, df$fam_size)

df <- df[is.na(df$fam_size) | df$fam_size != 0, ]

df$fam_1 <- ifelse(df$fam_size == 1, 1, 0)
df$fam_2 <- ifelse(df$fam_size == 2, 1, 0)
df$fam_3 <- ifelse(df$fam_size == 3, 1, 0)
df$fam_4 <- ifelse(df$fam_size == 4, 1, 0)
df$fam_5 <- ifelse(df$fam_size == 5, 1, 0)
df$fam_6 <- ifelse(df$fam_size == 6, 1, 0) # Family size equals 6 or more

df$age  <- df$year - df$stdt_dob_yr

df$stdt_dob_yr <- NULL

# BEGIN COLLEGE
tryCatch({
  college <- read_dta(file.path(data_dir, "better6B.dta"))
  cat("Data loaded successfully.\n")
  cat("Total observations:", nrow(college), "\n")
}, error = function(e) {
  stop("ERROR: Could not load data file. ", e$message)
})

college <- as.data.frame(college)

colnames(college)[colnames(college) == "fid"] <- "fid"

college <- college[, c("fid", grep("2021|2022|2023|2024", names(college), value = TRUE))]

college$cc_2021 <- ifelse(college$enroll_cc_fall2021, 1, 0)
college$cc_2022 <- ifelse(college$enroll_cc_fall2022, 1, 0)
college$cc_2023 <- ifelse(college$enroll_cc_fall2023, 1, 0)
college$cc_2024 <- ifelse(college$enroll_cc_fall2024, 1, 0)

college <- college[, !grepl("fall|spring", names(college))]

names(college)[names(college) == "enroll_uc2021"]  <- "uc_2021"
names(college)[names(college) == "enroll_csu2021"] <- "csu_2021"
names(college)[names(college) == "enroll_uc2022"]  <- "uc_2022"
names(college)[names(college) == "enroll_csu2022"] <- "csu_2022"
names(college)[names(college) == "enroll_uc2023"]  <- "uc_2023"
names(college)[names(college) == "enroll_csu2023"] <- "csu_2023"
names(college)[names(college) == "enroll_uc2024"]  <- "uc_2024"
names(college)[names(college) == "enroll_csu2024"] <- "csu_2024"

# Helper function to generate yearly filtered and formatted data
extract_year_data <- function(df, year) {
  uc_col  <- paste0("uc_", year)
  csu_col <- paste0("csu_", year)
  cc_col  <- paste0("cc_", year)

  # subset relevant columns
  temp <- df[, c("fid", uc_col, csu_col, cc_col)]

  # filter rows where any of the three columns == 1
  temp <- temp[temp[[uc_col]] == 1 | temp[[csu_col]] == 1 | temp[[cc_col]] == 1, ]

  # add unified columns
  temp$year <- year
  temp$uc   <- temp[[uc_col]]
  temp$csu  <- temp[[csu_col]]
  temp$cc   <- temp[[cc_col]]

  # keep only final structure
  temp <- temp[, c("fid", "year", "uc", "csu", "cc")]

  return(temp)
}

# Loop over years and bind results
years <- 2021:2024
college_list <- lapply(years, function(y) extract_year_data(college, y))
college <- do.call(rbind, college_list)

df <- merge(df, college, by = c("fid", "year"), all.x = TRUE)

rm(college_list, years, college, extract_year_data)

df$uc  <- ifelse(df$uc  == 1 & !is.na(df$uc),  1, 0)
df$csu <- ifelse(df$csu == 1 & !is.na(df$csu), 1, 0)
df$cc  <- ifelse(df$cc  == 1 & !is.na(df$cc),  1, 0)

df$college <- NA 
df$college <- ifelse(df$uc  == 1, "uc",  df$college)
df$college <- ifelse(df$cc  == 1, "cc",  df$college)
df$college <- ifelse(df$csu == 1, "csu", df$college)

df$enr <- as.integer(!is.na(df$college))

make_cycle <- function(year) {
  # Define date range: Oct 1 of previous year to June 30 of next year
  start_date <- as.Date(paste0(year - 1, "-10-01"))
  end_date   <- as.Date(paste0(year + 1, "-06-30"))
  
  # Generate all dates in the range
  cycle_dates <- seq.Date(start_date, end_date, by = "day")
  
  # Total days in cycle
  n_days <- length(cycle_dates)
  
  # Get "YYYY-MM" format for each date, to map to month index 1:21
  unique_months <- unique(format(cycle_dates, "%Y-%m"))
  month_lookup  <- setNames(seq_along(unique_months), unique_months)
  month         <- month_lookup[format(cycle_dates, "%Y-%m")]
  
  # Build the data frame
  data.frame(
    year       = rep(year, n_days),
    month      = as.integer(month),
    day        = seq_len(n_days),
    year_rcvd  = as.integer(format(cycle_dates, "%Y")),
    month_rcvd = as.integer(format(cycle_dates, "%m")),
    day_rcvd   = as.integer(format(cycle_dates, "%d"))
  )
}

cycle_21 <- make_cycle(2021)
cycle_22 <- make_cycle(2022)
cycle_23 <- make_cycle(2023)
cycle_24 <- make_cycle(2024)

rm(make_cycle)

tmp_21 <- merge(
  x     = cycle_21,
  y     = subset(df, year == 2021),
  by    = c("year", "year_rcvd", "month_rcvd", "day_rcvd"),
  all.x = TRUE
)


tmp_22 <- merge(
  x     = cycle_22,
  y     = subset(df, year == 2022),
  by    = c("year", "year_rcvd", "month_rcvd", "day_rcvd"),
  all.x = TRUE
)


tmp_23 <- merge(
  x     = cycle_23,
  y     = subset(df, year == 2023),
  by    = c("year", "year_rcvd", "month_rcvd", "day_rcvd"),
  all.x = TRUE
)


tmp_24 <- merge(
  x     = cycle_24,
  y     = subset(df, year == 2024),
  by    = c("year", "year_rcvd", "month_rcvd", "day_rcvd"),
  all.x = TRUE
)


df <- rbind(tmp_21, tmp_22, tmp_23, tmp_24)
rm(cycle_21, cycle_22, cycle_23, cycle_24)
rm(tmp_21, tmp_22, tmp_23, tmp_24)


df$app <- as.integer(!is.na(df$date_app_rcvd))


# zcta <- unique(df$zip_code)
# write.csv(zcta, file = "data/zcta.csv", row.names = FALSE)
# rm(zcta)


df$num_zip <- as.numeric(df$zip_code)

tryCatch({
  acs <- readRDS(file.path(data_dir, "better_acs.Rds"))
  cat("Data loaded successfully.\n")
  cat("Total observations:", nrow(acs), "\n")
}, error = function(e) {
  stop("ERROR: Could not load data file. ", e$message)
})

names(acs) <- tolower(names(acs))
colnames(acs)[colnames(acs) == "zcta"] <- "num_zip"
acs$year <- NULL
df <- merge(df, acs, by = "num_zip", all.x = TRUE)

rm(acs)

# OCT 30 NO MORE DAY RESTRICTION
#df <- df[df$day <= 538, ]

df <- df[!is.na(df$in_acs), ] # drop obs with no acs match (~.75%)

# BEGIN PAY
tryCatch({
  pay <- read_dta(file.path(data_dir, "better6D.dta"))
  cat("Data loaded successfully.\n")
  cat("Total observations:", nrow(pay), "\n")
}, error = function(e) {
  stop("ERROR: Could not load data file. ", e$message)
})

pay <- pay[pay$year > 2020, ]

pay <- pay[, -grep("^payf[5-8]$", names(pay))]
pay <- pay[, -grep("^pay[5-8]$", names(pay))]

df <- merge(df, pay, by = c("fid", "year"), all.x = TRUE)

df$paid <- as.integer(rowSums(df[, grep("^pay", names(df))], na.rm = TRUE) > 0)

df$paid_cc  <- ifelse(!is.na(df$payf2) | !is.na(df$pay2), 1, 0)
df$paid_uc  <- ifelse(!is.na(df$payf3) | !is.na(df$pay3), 1, 0)
df$paid_csu <- ifelse(!is.na(df$payf4) | !is.na(df$pay4), 1, 0)
# END PAYMENT STUFF

df$cal <- as.integer(
  (df$day <= 154 & df$year < 2024) | (df$day <= 215 & df$year == 2024)
)

df$enr <- as.integer(!is.na(df$college))

prev_lookup <- df[, c("fid", "year", "app", "cal", "paid", "enr")]
next_lookup <- df[, c("fid", "year", "app", "cal", "paid", "enr")]

names(prev_lookup) <- c("fid",
                        "match_year",
                        "app_prev",
                        "cal_prev",
                        "paid_prev",
                        "enr_prev")

names(next_lookup) <- c("fid",
                        "match_year",
                        "app_next",
                        "cal_next",
                        "paid_next",
                        "enr_next")

df$app_prev <- prev_lookup$app_prev[match(
  paste(df$fid, df$year - 1), 
  paste(prev_lookup$fid, prev_lookup$match_year))]

df$cal_prev <- prev_lookup$cal_prev[match(
  paste(df$fid, df$year - 1), 
  paste(prev_lookup$fid, prev_lookup$match_year))]

df$paid_prev <- prev_lookup$paid_prev[match(
  paste(df$fid, df$year - 1), 
  paste(prev_lookup$fid, prev_lookup$match_year))]

df$enr_prev <- prev_lookup$enr_prev[match(
  paste(df$fid, df$year - 1), 
  paste(prev_lookup$fid, prev_lookup$match_year))]

df$app_next <- next_lookup$app_next[match(
  paste(df$fid, df$year + 1), 
  paste(next_lookup$fid, next_lookup$match_year))]

df$cal_next <- next_lookup$cal_next[match(
  paste(df$fid, df$year + 1), 
  paste(next_lookup$fid, next_lookup$match_year))]

df$paid_next <- next_lookup$paid_next[match(
  paste(df$fid, df$year + 1), 
  paste(next_lookup$fid, next_lookup$match_year))]

df$enr_next <- next_lookup$enr_next[match(
  paste(df$fid, df$year + 1), 
  paste(next_lookup$fid, next_lookup$match_year))]

prev_years <- 2022:2024
next_years <- 2021:2023

df[df$year %in% prev_years, c("app_prev", "cal_prev", "paid_prev", "enr_prev")] <- 
  lapply(df[df$year %in% prev_years, c("app_prev", "cal_prev", "paid_prev", "enr_prev")], 
         function(x) replace(x, is.na(x), 0))

df[df$year %in% next_years, c("app_next", "cal_next", "paid_next", "enr_next")] <- 
  lapply(df[df$year %in% next_years, c("app_next", "cal_next", "paid_next", "enr_next")], 
         function(x) replace(x, is.na(x), 0))

rm(prev_lookup, next_lookup)

df$lvl <- NA
df$lvl <- ifelse(df$fresh == 1, "fresh", df$lvl)
df$lvl <- ifelse(df$soph  == 1, "soph",  df$lvl)
df$lvl <- ifelse(df$jrsr  == 1, "jrsr", df$lvl)

df$not_white <- NULL
df$aian      <- NULL
df$nhpi      <- NULL
df$other     <- NULL
df$in_acs    <- NULL
df$paid_prev <- NULL
df$enr_prev  <- NULL
df$cal_prev  <- NULL

saveRDS(df, file = file.path(data_dir, "clean6.Rds"))
