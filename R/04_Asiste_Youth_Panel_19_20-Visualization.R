################################################################################
# CONSOLIDATE HOUSEHOLD-LEVEL PANEL RD TRANSITION RESULTS (p=1)
# Purpose: Extract and visualize RD results for educational transitions
# Outcomes: Start Attending and Stopped Attending
# Groups: Young adults (18-24) AND Children (<18) - hijo==1 only
################################################################################

# Load required packages
if (!require("readxl")) install.packages("readxl")
if (!require("ggplot2")) install.packages("ggplot2")

library(readxl)
library(ggplot2)

# Set working directory and paths
base_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Transfers causal analysis BRA/Outputs/02_Dissertation_checks/Excel/02_asiste_panel"
output_path <- base_path

# Verify directory exists
if (!dir.exists(base_path)) {
  stop("Directory not found: ", base_path)
}

# List files in directory
cat("Files found in directory:\n")
files_in_dir <- list.files(base_path, pattern = "\\.xlsx$")
print(files_in_dir)
cat("\n")

################################################################################
# DEFINE GROUP INFORMATION (6 GROUPS: 3 YOUNG ADULTS + 3 CHILDREN)
################################################################################

# Group definitions - focusing on hijo==1 groups only
groups <- list(
  "01" = list(label = "Young Sons 18-24", category = "Young Adults"),
  "02" = list(label = "Young Daughters 18-24", category = "Young Adults"),
  "03" = list(label = "Youth 18-24", category = "Young Adults"),
  "07" = list(label = "Boys <18", category = "Children"),
  "08" = list(label = "Girls <18", category = "Children"),
  "09" = list(label = "Kids <18", category = "Children")
)

# Group name mappings for file construction
group_names <- list(
  "01" = "young_sons_18-24",
  "02" = "young_daughters_18-24",
  "03" = "young_children_18-24",
  "07" = "boys_under18",
  "08" = "girls_under18",
  "09" = "kids_under18"
)

################################################################################
# FUNCTION TO EXTRACT DATA FROM EXCEL FILE
################################################################################

extract_group_data <- function(group_num, outcome_type, base_path, group_names) {
  
  # Construct filename for p=1
  group_name <- group_names[[group_num]]
  file_name <- paste0(group_num, "_HH_RD_", group_name, "_", outcome_type, "_asist_panel.xlsx")
  file_path <- file.path(base_path, file_name)
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_name))
    return(rep(NA, 5))
  }
  
  cat("Reading:", file_name, "- Using Column E (Cluster With Controls)\n")
  
  tryCatch({
    # Read the Excel file - Column E is the 5th column
    data <- read_excel(file_path, sheet = 1, col_names = FALSE)
    
    # Extract required values from Column E (index 5)
    original_n <- as.numeric(data[2, 5][[1]])       # E2: Original N
    conventional_se <- as.numeric(data[22, 5][[1]]) # E22: Conventional SE
    mean_above_conv <- as.numeric(data[26, 5][[1]]) # E26: Mean above (Conv)
    mean_below_conv <- as.numeric(data[27, 5][[1]]) # E27: Mean below (Conv)
    
    # Calculate coefficient: Mean below - Mean above
    # (Treatment is below threshold, so this gives the treatment effect)
    coefficient <- mean_below_conv - mean_above_conv
    
    # Calculate confidence interval half-width using 1.96 * SE
    ci_half_width <- conventional_se
    
    # Calculate significance stars
    if (is.na(conventional_se) || conventional_se == 0) {
      sig_stars <- ""
    } else {
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
      conventional_se,
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
# FUNCTION TO CREATE VISUALIZATION FOR GIVEN OUTCOME
################################################################################

create_transition_plot <- function(outcome_type, base_path, groups, group_names) {
  
  cat("\n===============================================\n")
  cat(paste("EXTRACTING RD RESULTS FOR:", toupper(outcome_type), "ATTENDING\n"))
  cat("Polynomial order: p=1\n")
  cat("Specification: Cluster With Controls (Column E)\n")
  cat("Coefficient: E27 - E26 (Mean below Conv - Mean above Conv)\n")
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
    results_matrix[, i] <- extract_group_data(group_num, outcome_type, base_path, group_names)
  }
  
  # Print extracted results
  cat("\n===============================================\n")
  cat("EXTRACTED RESULTS MATRIX\n")
  cat("===============================================\n\n")
  print(results_matrix)
  
  ################################################################################
  # PREPARE DATA FOR VISUALIZATION
  ################################################################################
  
  cat("\n===============================================\n")
  cat("PREPARING DATA FOR VISUALIZATION\n")
  cat("===============================================\n\n")
  
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
  
  # Calculate confidence interval bounds
  plot_data$CI_Lower <- plot_data$Estimate - plot_data$CI_Width
  plot_data$CI_Upper <- plot_data$Estimate + plot_data$CI_Width
  
  # Set factor levels to maintain specified order (reverse for top-to-bottom display)
  plot_data$Group <- factor(plot_data$Group, 
                            levels = rev(c("Young Sons 18-24", 
                                           "Young Daughters 18-24", 
                                           "Youth 18-24", 
                                           "Boys <18", 
                                           "Girls <18", 
                                           "Kids <18")))
  
  # Define colors for categories
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
  
  # Fixed x-axis limits for consistency
  x_lower_limit <- -0.5
  x_upper_limit <- 0.5
  break_interval <- 0.1
  
  cat("X-axis range (fixed): [", x_lower_limit, ",", x_upper_limit, "]\n\n")
  
  # Set title based on outcome type
  plot_title <- ifelse(outcome_type == "start", 
                       "AE's effects on starting educational attendance",
                       "AE's effects on stopping educational attendance")
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = Estimate, y = Group, color = Category, fill = Category)) +
    # Add vertical line at zero
    geom_vline(xintercept = 0, linetype = "solid", color = "gray50", linewidth = 0.5) +
    
    # Add confidence intervals with transparency
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
      x = "Percentage Points",
      y = NULL
    ) +
    
    # Color scale (no legend needed as colors are self-explanatory)
    scale_color_manual(values = colors, guide = "none") +
    scale_fill_manual(values = colors, guide = "none") +
    
    # Theme settings
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
  plot_file <- file.path(output_path, paste0("HH_RD_", outcome_type, "_asist_p1_ClusterControls.png"))
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
    cat(sprintf("  %s: %.4f%s (95%% CI: %.4f to %.4f) [N=%s]\n", 
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
# CREATE BOTH VISUALIZATIONS
################################################################################

cat("\n")
cat("================================================================================\n")
cat("GENERATING VISUALIZATIONS FOR EDUCATIONAL TRANSITIONS (PANEL DATA)\n")
cat("================================================================================\n")

# Create plot for START ATTENDING
start_results <- create_transition_plot("start", base_path, groups, group_names)

# Create plot for STOPPED ATTENDING
stopped_results <- create_transition_plot("stopped", base_path, groups, group_names)

################################################################################
# FINAL SUMMARY
################################################################################

cat("\n")
cat("================================================================================\n")
cat("ALL VISUALIZATIONS COMPLETED\n")
cat("================================================================================\n")
cat("Output directory:", output_path, "\n")
cat("Files created:\n")
cat("  1. HH_RD_start_asist_p1_ClusterControls.png\n")
cat("  2. HH_RD_stopped_asist_p1_ClusterControls.png\n")
cat("\n")
cat("Both graphs include:\n")
cat("  - Young adults (18-24): Sons, Daughters, All Children\n")
cat("  - Children (<18): Boys, Girls, All Kids\n")
cat("  - Polynomial order: p=1 (linear)\n")
cat("  - Specification: Cluster With Controls (Column E)\n")
cat("  - Groups focus on hijo==1 (children of household head)\n")
cat("  - No manual overrides applied\n")
cat("\n")