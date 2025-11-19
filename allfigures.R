################################################################################
# BETTER FAFSA - FIGURES
# Generate Figure 1 (by education level) and Figure 2 (by dependency status)
################################################################################

cat("\n>>> Generating Figures\n\n")

# Load required libraries
library(dplyr)

# Directories
project_dir  <- "/home/erik/Repos/fafsa"
data_dir     <- file.path(project_dir, "data")
figures_dir  <- file.path(project_dir, "figures")

# Create directories if they don't exist
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

# Load data
df <- readRDS(file.path(data_dir, "clean6.Rds"))

cat("Data loaded successfully.\n")
cat("Total observations:", nrow(df), "\n")
cat("Years:", paste(unique(df$year), collapse = ", "), "\n\n")

# Define Okabe-Ito color palette
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7", "#000000")

# Cumulative submission plot function
plot_cumulative <- function(df, group_var, title = "", filename = NULL) {
  # Filter data
  df_filtered <- df[df[[group_var]] == 1 & df$day <= 639, ]
  
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
  
  plot(
    NA,
    xlim = c(0, 650),
    ylim = c(0, max(df_agg$cumul_total) * 1.05),
    xlab = "Day",
    ylab = "Cumulative Total",
    main = title,
    xaxt = "n",
    yaxt = "n"
  )
  
  # Add axes
  axis(1, at = seq(0, 650, by = 50))
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

# ============================================================================
# FIGURE 1: By Education Level (4-panel)
# ============================================================================

cat("Creating Figure 1: Cumulative Submissions by Education Level\n")

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
  cat("✓ Figure 1 saved successfully\n")
  cat("  Location:", file.path(figures_dir, "figure1_education_level.png"), "\n\n")
}, error = function(e) {
  cat("✗ ERROR creating Figure 1:", e$message, "\n\n")
  if (dev.cur() > 1) dev.off()
})

# ============================================================================
# FIGURE 2: By Dependency Status (2-panel)
# ============================================================================

cat("Creating Figure 2: Cumulative Submissions by Dependency Status\n")

tryCatch({
  png(file.path(figures_dir, "figure2_dependency_status.png"), 
      width = 12, height = 5, units = "in", res = 300)
  
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  
  plot_cumulative(df, "ind", title = "Independent Applicants")
  plot_cumulative(df, "dep", title = "Dependent Applicants")
  
  dev.off()
  cat("✓ Figure 2 saved successfully\n")
  cat("  Location:", file.path(figures_dir, "figure2_dependency_status.png"), "\n\n")
}, error = function(e) {
  cat("✗ ERROR creating Figure 2:", e$message, "\n\n")
  if (dev.cur() > 1) dev.off()
})