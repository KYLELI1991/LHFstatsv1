#' Plot survival curve
#' @export
#' 
#' @param data dataframe
#' @param time_col time column name
#' @param status_col status column name (0=censored, 1=event)
#' @param group_col grouping column name (optional)
#' @param group_names custom names for groups (vector, e.g., c("Treatment", "Control"))
#' @param colors color vector, e.g., c("blue", "red")
#' @param line_types line types for groups (e.g., c(1, 2) for solid, dashed)
#' @param show_hr whether to show HR and confidence interval
#' @param show_median whether to show median survival time
#' @param show_risktable whether to show risk table
#' @param show_ci whether to show confidence interval band
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param title plot title
#' @param break_time time axis break interval
#' @param legend_position legend position
#' @param risk_table_height risk table height
#' @param y_percent whether to show y-axis as percentage (default TRUE)
#' @param font_size base font size
#' @param theme_custom custom ggplot theme (optional)
#' @return survival curve plot object
plot_survival <- function(data, 
                          time_col, 
                          status_col, 
                          group_col = NULL,
                          group_names = NULL,
                          colors = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#3B9AB2"),
                          line_types = NULL,
                          show_hr = TRUE,
                          show_median = TRUE,
                          show_risktable = TRUE,
                          show_ci = TRUE,
                          xlab = "Follow-up time (months)",
                          ylab = "Survival probability",
                          title = "Survival curve",
                          break_time = NULL,
                          legend_position = c(0.85, 0.85),
                          risk_table_height = 0.25,
                          y_percent = TRUE,
                          font_size = 12,
                          theme_custom = NULL) {
  
  # Load required packages
  if (!require(survival)) install.packages("survival")
  if (!require(ggplot2)) install.packages("ggplot2")
  if (!require(survminer)) install.packages("survminer")
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(tidyr)) install.packages("tidyr")
  if (!require(scales)) install.packages("scales")
  
  library(survival)
  library(ggplot2)
  library(survminer)
  library(dplyr)
  library(tidyr)
  library(scales)
  
  # Data check
  if(!all(c(time_col, status_col) %in% names(data))) {
    stop("Specified columns do not exist in the dataframe")
  }
  
  # Create data copy and rename columns to avoid environment issues
  plot_data <- data %>%
    select(all_of(c(time_col, status_col, group_col))) %>%
    na.omit() %>%
    rename(
      time = !!time_col,
      status = !!status_col
    )
  
  if(!is.null(group_col)) {
    plot_data <- plot_data %>%
      rename(group = !!group_col)
    plot_data$group <- as.factor(plot_data$group)
    
    # Apply custom group names if provided
    if(!is.null(group_names)) {
      if(length(group_names) == length(levels(plot_data$group))) {
        levels(plot_data$group) <- group_names
      } else {
        warning("Length of group_names does not match number of groups. Using original group names.")
      }
    }
  }
  
  # Fit survival curve
  if(is.null(group_col)) {
    fit <- survfit(Surv(time, status) ~ 1, data = plot_data)
  } else {
    fit <- survfit(Surv(time, status) ~ group, data = plot_data)
  }
  
  # Calculate median survival time and confidence intervals
  get_median_summary <- function(fit, group_col = NULL) {
    # Get survival summary
    fit_summary <- summary(fit)
    
    if(is.null(group_col)) {
      # For single group
      med <- fit_summary$table
      result <- data.frame(
        Group = "Overall",
        Median = ifelse(is.na(med[["median"]]), NA, med[["median"]]),
        Lower = ifelse(is.na(med[["0.95LCL"]]), NA, med[["0.95LCL"]]),
        Upper = ifelse(is.na(med[["0.95UCL"]]), NA, med[["0.95UCL"]]),
        stringsAsFactors = FALSE
      )
    } else {
      # For multiple groups - extract median for each group
      result <- data.frame()
      
      # Check if fit_summary$table is a matrix or vector
      if (is.matrix(fit_summary$table)) {
        # Multiple groups case
        for(i in 1:nrow(fit_summary$table)) {
          med <- fit_summary$table[i, ]
          group_name <- rownames(fit_summary$table)[i]
          # Clean group name (remove "group=" if present)
          group_name <- gsub(".*=", "", group_name)
          
          temp_df <- data.frame(
            Group = group_name,
            Median = ifelse(is.na(med[["median"]]), NA, med[["median"]]),
            Lower = ifelse(is.na(med[["0.95LCL"]]), NA, med[["0.95LCL"]]),
            Upper = ifelse(is.na(med[["0.95UCL"]]), NA, med[["0.95UCL"]]),
            stringsAsFactors = FALSE
          )
          result <- rbind(result, temp_df)
        }
      } else {
        # Single group case but with group_col specified
        med <- fit_summary$table
        result <- data.frame(
          Group = "All",
          Median = ifelse(is.na(med[["median"]]), NA, med[["median"]]),
          Lower = ifelse(is.na(med[["0.95LCL"]]), NA, med[["0.95LCL"]]),
          Upper = ifelse(is.na(med[["0.95UCL"]]), NA, med[["0.95UCL"]]),
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Handle infinite values
    result$Median[is.infinite(result$Median)] <- NA
    result$Lower[is.infinite(result$Lower)] <- NA
    result$Upper[is.infinite(result$Upper)] <- NA
    
    return(result)
  }
  
  # Calculate overall follow-up time
  follow_up_summary <- function(data) {
    overall_time <- data$time
    list(
      median_followup = median(overall_time),
      lower_ci = quantile(overall_time, probs = 0.025),
      upper_ci = quantile(overall_time, probs = 0.975)
    )
  }
  
  # Calculate Cox model results if grouping and HR display requested
  hr_info <- NULL
  if(!is.null(group_col) && show_hr && length(unique(plot_data$group)) == 2) {
    group_levels <- levels(plot_data$group)
    
    # Fit Cox model
    cox_model <- coxph(Surv(time, status) ~ group, data = plot_data)
    cox_summary <- summary(cox_model)
    
    # Get HR and CI
    hr <- exp(coef(cox_model))
    hr_ci <- exp(confint(cox_model))
    p_value <- cox_summary$coefficients[, "Pr(>|z|)"]
    
    # Determine reference and comparison groups
    ref_group <- group_levels[1]
    comp_group <- group_levels[2]
    
    hr_info <- list(
      hr = hr,
      lower = hr_ci[1],
      upper = hr_ci[2],
      p = p_value,
      ref_group = ref_group,
      comp_group = comp_group
    )
  }
  
  # Set time axis breaks
  if(is.null(break_time)) {
    max_time <- max(plot_data$time, na.rm = TRUE)
    break_time <- ceiling(max_time / 5)
  }
  
  # Create custom theme
  if(is.null(theme_custom)) {
    theme_custom <- theme_classic() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = font_size + 2),
        axis.title = element_text(size = font_size),
        axis.text = element_text(size = font_size - 1),
        legend.title = element_text(size = font_size),
        legend.text = element_text(size = font_size - 1),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.position = legend_position,
        plot.margin = margin(10, 10, 10, 10)
      )
  }
  
  # Prepare plot parameters
  plot_params <- list(
    fit = fit,
    data = plot_data,
    conf.int = show_ci,
    conf.int.style = "ribbon",
    palette = colors,
    xlab = xlab,
    ylab = ylab,
    title = title,
    break.time.by = break_time,
    legend.title = ifelse(is.null(group_col), "", group_col),
    legend.labs = if(!is.null(group_col)) levels(plot_data$group) else NULL,
    legend = legend_position,
    risk.table = show_risktable,
    risk.table.col = "strata",
    risk.table.y.text = FALSE,
    risk.table.height = risk_table_height,
    ggtheme = theme_custom
  )
  
  # Add line types if specified
  if(!is.null(line_types) && !is.null(group_col)) {
    plot_params$linetype <- line_types
  }
  
  # Create base survival plot
  ggsurv <- do.call(ggsurvplot, plot_params)
  
  # Get x-axis range for annotations
  x_max <- max(plot_data$time, na.rm = TRUE)
  x_range <- x_max * 1.05
  x_breaks <- seq(0, ceiling(x_max), by = break_time)
  
  # Remove existing scales and add new ones for the main plot only
  ggsurv$plot <- ggsurv$plot + 
    scale_x_continuous(breaks = x_breaks, limits = c(0, x_range)) +
    scale_y_continuous(limits = c(0, 1), 
                       labels = if(y_percent) scales::percent_format(accuracy = 1) else waiver())
  
  # Add annotations to the plot
  annotations <- list()
  y_pos <- 0.92  # Starting y position
  
  # Add HR information
  if(!is.null(hr_info)) {
    hr_text <- sprintf("%s vs %s: HR = %.2f (%.2f-%.2f)", 
                       hr_info$comp_group, hr_info$ref_group,
                       hr_info$hr, hr_info$lower, hr_info$upper)
    p_text <- sprintf("p = %.4f", hr_info$p)
    
    annotations <- c(annotations, 
                     list(annotate("text", 
                                   x = x_max * 0.02, 
                                   y = y_pos, 
                                   label = paste0(hr_text, "\n", p_text),
                                   hjust = 0,
                                   vjust = 1,
                                   size = font_size * 0.3,
                                   color = "black",
                                   fontface = "bold")))
    y_pos <- y_pos - 0.08
  }
  
  # Add median survival times
  if(show_median) {
    medians <- get_median_summary(fit, group_col)
    
    # Add title for median section
    annotations <- c(annotations,
                     list(annotate("text", 
                                   x = x_max * 0.02, 
                                   y = y_pos, 
                                   label = "Median survival (months):",
                                   hjust = 0,
                                   vjust = 1,
                                   size = font_size * 0.3,
                                   fontface = "bold")))
    y_pos <- y_pos - 0.05
    
    if(is.null(group_col)) {
      if(!is.na(medians$Median[1])) {
        median_text <- sprintf("Overall: %.1f (%.1f-%.1f)", 
                               medians$Median[1], medians$Lower[1], medians$Upper[1])
        annotations <- c(annotations,
                         list(annotate("text", 
                                       x = x_max * 0.04, 
                                       y = y_pos, 
                                       label = median_text,
                                       hjust = 0,
                                       vjust = 1,
                                       size = font_size * 0.3)))
        y_pos <- y_pos - 0.05
      }
    } else {
      # Display median for each group
      for(i in 1:nrow(medians)) {
        if(!is.na(medians$Median[i])) {
          median_text <- sprintf("%s: %.1f (%.1f-%.1f)", 
                                 medians$Group[i], 
                                 medians$Median[i], 
                                 medians$Lower[i], 
                                 medians$Upper[i])
          annotations <- c(annotations,
                           list(annotate("text", 
                                         x = x_max * 0.04, 
                                         y = y_pos, 
                                         label = median_text,
                                         hjust = 0,
                                         vjust = 1,
                                         size = font_size * 0.3,
                                         color = colors[i])))
          y_pos <- y_pos - 0.05
        } else {
          # Handle cases where median is not reached
          median_text <- sprintf("%s: Not reached", medians$Group[i])
          annotations <- c(annotations,
                           list(annotate("text", 
                                         x = x_max * 0.04, 
                                         y = y_pos, 
                                         label = median_text,
                                         hjust = 0,
                                         vjust = 1,
                                         size = font_size * 0.3,
                                         color = colors[i])))
          y_pos <- y_pos - 0.05
        }
      }
    }
    y_pos <- y_pos - 0.02  # Extra space after medians
  }
  
  # Add overall follow-up time information
  follow_up <- follow_up_summary(plot_data)
  follow_text <- sprintf("Overall follow-up: median %.1f (%.1f-%.1f) months", 
                         follow_up$median_followup,
                         follow_up$lower_ci,
                         follow_up$upper_ci)
  
  annotations <- c(annotations,
                   list(annotate("text", 
                                 x = x_max * 0.02, 
                                 y = 0.08, 
                                 label = follow_text,
                                 hjust = 0,
                                 vjust = 1,
                                 size = font_size * 0.25,
                                 color = "gray30")))
  
  # Add all annotations to the plot
  for(ann in annotations) {
    ggsurv$plot <- ggsurv$plot + ann
  }
  
  # Do NOT modify risk table y-axis as it's discrete (group names)
  # Risk table remains unchanged
  
  # Return result
  return(ggsurv)
}

#' Function Title
#'
#' Function Description
#' @export
# Example usage
example_usage <- function() {
  # Create test data
  set.seed(123)
  test_data <- data.frame(
    time = c(sample(1:40, 100, replace = TRUE), sample(10:60, 100, replace = TRUE)),
    status = sample(0:1, 200, replace = TRUE, prob = c(0.3, 0.7)),
    treatment = c(rep("A", 100), rep("B", 100))
  )
  
  # Example with customizations
  p <- plot_survival(
    data = test_data,
    time_col = "time",
    status_col = "status",
    group_col = "treatment",
    group_names = c("Treatment", "Control"),
    colors = c("#FF6B6B", "#4ECDC4"),
    show_hr = TRUE,
    show_median = TRUE,
    show_risktable = TRUE,
    show_ci = TRUE,
    title = "Survival Analysis",
    xlab = "Time (months)",
    ylab = "Survival Probability (%)",
    y_percent = TRUE,
    font_size = 12
  )
  
  print(p)
}

# Run example
# example_usage()
