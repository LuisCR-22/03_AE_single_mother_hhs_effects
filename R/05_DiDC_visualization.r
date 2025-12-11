################################################################################
# CONSOLIDATE HOUSEHOLD-LEVEL PANEL DiDC RESULTS (p=1, BC Est, CL SE)
# Purpose: Extract and visualize DiDC results for Delta Attendance
# Outcome: Delta Attendance (mean_delta_group)
# Groups: Young adults (18-24) AND Children (<18) - hijo==1 only
# Specification: Cluster With Controls (Column E)
# Coefficient: BC Estimate (A13)
# CI/SE: Conventional SE (A12)
################################################################################

# Load required packages
if (!require("readxl")) install.packages("readxl"); library(readxl)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

# Set working directory and paths
# NOTE: Update 'base_path' to match your Stata output directory ($excel_dir_hh)
base_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Transfers causal analysis BRA/Outputs/01_New_Outputs_2025/Excel/RD_results/25_DiDC_HH"
output_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Transfers causal analysis BRA/Outputs/01_New_Outputs_2025/Docs/25_DiDC_HH"

# Create output directory if it doesn't exist
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# Verify input directory exists
if (!dir.exists(base_path)) {
  stop("Input directory not found: ", base_path)
}

# List files in directory
cat("Files found in directory:\n")
files_in_dir <- list.files(base_path, pattern = "\\.xlsx$")
print(files_in_dir)
cat("\n")

################################################################################
# DEFINE GROUP INFORMATION (6 GROUPS: 3 YOUNG ADULTS + 3 CHILDREN, hijo==1 only)
# NOTE: These reflect the final, corrected groups used in the Stata code.
################################################################################

# Group definitions for plot labels and categories
groups <- list(
  "01" = list(label = "Young Sons (18-24)", category = "Young Adults"),
  "02" = list(label = "Young Daughters (18-24)", category = "Young Adults"),
  "03" = list(label = "Youth (18-24)", category = "Young Adults"),
  "07" = list(label = "Boys (<18)", category = "Children"),
  "08" = list(label = "Girls (<18)", category = "Children"),
  "09" = list(label = "Kids (<18)", category = "Children")
)

# Group name mappings for file construction (matches Stata 'name_XX' macros)
group_names <- list(
  "01" = "young_sons",
  "02" = "young_daughters",
  "03" = "young_children",
  "07" = "boys",
  "08" = "girls",
  "09" = "kids"
)

################################################################################
# FUNCTION TO EXTRACT DATA FROM EXCEL FILE (ADAPTED FOR DiDC OUTPUT)
################################################################################

extract_group_data <- function(group_num, base_path, group_names) {
  
  # Construct filename for DiDC output
  group_name <- group_names[[group_num]]
  # NOTE: File pattern is XX_HH_DiDC_groupname_panel_V2.xlsx
  file_name <- paste0(group_num, "_HH_DiDC_", group_name, "_panel_V2.xlsx")
  file_path <- file.path(base_path, file_name)
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_name))
    return(rep(NA, 5))
  }
  
  cat("Reading:", file_name, "- Using Column E (Cluster With Controls)\n")
  
  tryCatch({
    # Read the Excel file - Column E is the 5th column (index 5)
    data <- read_excel(file_path, sheet = 1, col_names = FALSE)
    
    # Extract required values from Column E (index 5)
    original_n <- as.numeric(data[2, 5][[1]]) 
    
    # REQUIRED VALUES FROM STATA OUTPUT ROWS:
    conventional_se <- as.numeric(data[12, 5][[1]]) # E12: Conventional SE (se_tau_cl)
    bc_estimate <- as.numeric(data[13, 5][[1]]) *-1
    
    # Coefficient is the BC estimate
    coefficient <- bc_estimate
    
    # Calculate confidence interval half-width using 1.96 * Conventional SE
    ci_half_width <- 1.96 * conventional_se
    
    # Calculate significance stars using the t-stat from BC estimate / Conventional SE
    if (is.na(conventional_se) || conventional_se == 0) {
      sig_stars <- ""
    } else {
      # Use BC estimate and Conventional SE for t-statistic (as specified)
      t_stat <- coefficient / conventional_se 
      p_value <- 2 * (1 - pnorm(abs(t_stat)))
      
      if (is.na(p_value)) {
        sig_stars <- ""
      } else if (p_value < 0.01) {
        sig_stars <- "***"
      } else if (p_value < 0.05) {
        sig_stars <- "**"
      } else if (p_value < 0.10) {
        sig_stars <- "*"
      } else {
        sig_stars <- ""
      }
    }
    
    # Return all results
    results <- c(
      coefficient,
      conventional_se, # Stored as 'SE'
      ci_half_width, 
      original_n,
      sig_stars
    )
    
    return(results)
    
  }, error = function(e) {
    warning(paste("Error reading file", file_name, ":", e$message))
    return(rep(NA, 5))
  })
}

################################################################################
# FUNCTION TO CREATE VISUALIZATION
################################################################################

create_DiDC_plot <- function(base_path, groups, group_names, output_path) {
  
  cat("\n===============================================\n")
  cat("EXTRACTING DiDC RESULTS FOR: DELTA ATTENDANCE\n")
  cat("Specification: Cluster With Controls (Column E)\n")
  cat("Coefficient: BC Estimate (E13)\n")
  cat("CI/SE: Conventional SE (E12)\n")
  cat("===============================================\n\n")
  
  # Create matrix to store results
  stat_names <- c("Coefficient", "Conventional SE", "CI Half-Width (1.96*SE)", 
                  "Original N", "Significance")
  results_matrix <- matrix(NA, 
                           nrow = length(stat_names), 
                           ncol = length(groups))
  
  rownames(results_matrix) <- stat_names
  colnames(results_matrix) <- sapply(groups, function(g) g$label)
  
  # Extract data for each group
  for (i in seq_along(groups)) {
    group_num <- names(groups)[i]
    # Outcome type parameter is removed as there is only one outcome: Delta Attendance
    results_matrix[, i] <- extract_group_data(group_num, base_path, group_names) 
  }
  
  # Print extracted results
  cat("\n===============================================\n")
  cat("EXTRACTED RESULTS MATRIX\n")
  cat("===============================================\n\n")
  print(results_matrix)
  
  ################################################################################
  # PREPARE DATA FOR VISUALIZATION
  ################################################################################
  
  # Prepare data for plotting
  plot_data <- data.frame(
    Group = colnames(results_matrix),
    Estimate = as.numeric(results_matrix["Coefficient", ]),
    SE = as.numeric(results_matrix["Conventional SE", ]),
    CI_Width = as.numeric(results_matrix["CI Half-Width (1.96*SE)", ]),
    N = as.numeric(results_matrix["Original N", ]),
    Significance = as.character(results_matrix["Significance", ]),
    Category = sapply(groups, function(g) g$category),
    stringsAsFactors = FALSE
  )
  
  # Calculate confidence interval bounds (using the 1.96*SE Half-Width)
  plot_data$CI_Lower <- plot_data$Estimate - plot_data$CI_Width
  plot_data$CI_Upper <- plot_data$Estimate + plot_data$CI_Width
  
  # Set factor levels to maintain specified order (reverse for top-to-bottom display)
  plot_data$Group <- factor(plot_data$Group, 
                            levels = rev(c("Young Sons (18-24)", 
                                           "Young Daughters (18-24)", 
                                           "Youth (18-24)", 
                                           "Boys (<18)", 
                                           "Girls (<18)", 
                                           "Kids (<18)")))
  
  # Define colors for categories (reusing original colors)
  colors <- c("Young Adults" = "#4A9BC4", "Children" = "#FF7A85")
  
  # Print plot data for verification
  cat("Plot data:\n")
  print(plot_data)
  cat("\n")
  
  ################################################################################
  # CREATE PLOT
  ################################################################################
  
  cat("===============================================\n")
  cat("CREATING VISUALIZATION\n")
  cat("===============================================\n\n")
  
  # Fixed x-axis limits for consistency (reusing original limits)
  x_lower_limit <- -0.5
  x_upper_limit <- 0.5
  break_interval <- 0.1
  
  cat("X-axis range (fixed): [", x_lower_limit, ",", x_upper_limit, "]\n\n")
  
  plot_title <- "DiDC Estimates of AE's Effect on Delta Attendance (BC Estimate, CL SE)"
  plot_subtitle <- "Panel Data | Outcome: Change in Attendance (2020 - 2019) | Specification: Cluster With Controls"
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = Estimate, y = Group, color = Category, fill = Category)) +
    # Add vertical line at zero
    geom_vline(xintercept = 0, linetype = "solid", color = "gray50", linewidth = 0.5) +
    
    # Add confidence intervals (using the 1.96*CL SE)
    geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), 
                   height = 0.2, linewidth = 0.8, alpha = 0.6) +
    
    # Add point estimates
    geom_point(size = 3) +
    
    # Add estimate labels with significance stars above points
    geom_text(aes(label = sprintf("%.3f%s", Estimate, Significance)),
              vjust = -1, size = 3.5, color = "black", show.legend = FALSE) +
    
    # Add sample size labels below points
    geom_text(aes(label = sprintf("(N=%s)", format(round(N), big.mark = ","))),
              vjust = 1.5, size = 3, color = "black", show.legend = FALSE) +
    
    # Labels and titles
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Percentage Points (Change in Attendance)",
      y = NULL
    ) +
    
    # Color scale (no legend needed as colors are self-explanatory)
    scale_color_manual(values = colors, guide = "none") +
    scale_fill_manual(values = colors, guide = "none") +
    
    # Theme settings (reusing original theme)
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),
      plot.subtitle = element_text(hjust = 0.5, size = 11, face = "italic", color = "gray30"),
      axis.text.y = element_text(size = 10, color = "black"),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.title.x = element_text(size = 11, margin = margin(t = 10)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    
    # Set x-axis breaks and labels
    scale_x_continuous(
      breaks = seq(x_lower_limit, x_upper_limit, by = break_interval),
      labels = function(x) sprintf("%.1f", x)
    ) +
    
    # Set coordinate limits
    coord_cartesian(xlim = c(x_lower_limit, x_upper_limit), clip = "off")
  
  # Save plot
  plot_file <- file.path(output_path, "HH_DiDC_DeltaAttendance_BC_CLSE_ClusterControls.png")
  ggsave(plot_file, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
  
  cat("Plot saved successfully:\n")
  cat(plot_file, "\n\n")
  
  # Print summary
  cat("===============================================\n")
  cat("ANALYSIS COMPLETE\n")
  cat("===============================================\n")
  cat("Plot file created:", plot_file, "\n")
  cat("\n=== DETAILED RESULTS ===\n")
  for (i in 1:nrow(plot_data)) {
    cat(sprintf("  %s: %.4f%s (95%% CI: %.4f to %.4f) [N=%s]\n", 
                plot_data$Group[i], 
                plot_data$Estimate[i],
                plot_data$Significance[i],
                plot_data$CI_Lower[i],
                plot_data$CI_Upper[i],
                format(round(plot_data$N[i]), big.mark = ",")))
  }
  cat("\n")
  
  return(list(plot = p, data = plot_data, results = results_matrix))
}

################################################################################
# CREATE VISUALIZATION
################################################################################

cat("\n")
cat("================================================================================\n")
cat("GENERATING VISUALIZATION FOR DiDC DELTA ATTENDANCE (PANEL DATA)\n")
cat("================================================================================\n")

# Run the single function for the DiDC results
DiDC_results <- create_DiDC_plot(base_path, groups, group_names, output_path)

################################################################################
# FINAL SUMMARY
################################################################################

cat("\n")
cat("================================================================================\n")
cat("ALL VISUALIZATIONS COMPLETED\n")
cat("================================================================================\n")
cat("Output directory:", output_path, "\n")
cat("File created: HH_DiDC_DeltaAttendance_BC_CLSE_ClusterControls.png\n")
cat("\n")