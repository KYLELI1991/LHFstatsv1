# Load required packages
library(ggplot2)
library(ggsci)
library(dplyr)
library(tidyr)

# Define function with subtitle and font settings
#' Function Title
#'
#' Function Description
#' @export
plot_patient_lines <- function(data, 
                               id_col = "ID", 
                               cohort_col = "cohort", 
                               days_col = "days", 
                               size_col = "size",
                               calculate_percentage = TRUE,
                               percentage_col_name = "percentage",
                               reference_day = 0,
                               title = "Patient Follow-up Line Plot",
                               subtitle = NULL,  # 新增副标题参数
                               show_author_date = TRUE,  # 是否显示作者和日期
                               author_name = "By LHF",  # 作者名称
                               xlab = "Follow-up Time (days)",
                               ylab = "Percentage from Baseline (%)",
                               legend_title = "Cohort",
                               palette = "lancet",
                               theme_style = "lancet",
                               line_alpha = 0.8,
                               line_size = 1,
                               point_size = 2,
                               show_points = TRUE,
                               show_baseline_reference = TRUE,
                               baseline_reference_y = 100,
                               baseline_reference_color = "gray50",
                               baseline_reference_type = "dashed",
                               show_PD_PR_reference = TRUE,
                               PD_threshold = 20,
                               PR_threshold = -30,
                               PD_reference_color = "red",
                               PR_reference_color = "green",
                               PD_PR_reference_type = "dashed",
                               custom_legend_labels = NULL,
                               font_family = "sans",  # 字体家族
                               font_size_base = 12,  # 基础字体大小
                               font_size_title = 14,  # 标题字体大小
                               font_size_subtitle = 11,  # 副标题字体大小
                               font_size_axis = 10,  # 坐标轴字体大小
                               font_size_legend = 10) {  # 图例字体大小
  
  # Parameter checking
  required_cols <- c(id_col, cohort_col, days_col, size_col)
  missing_cols <- required_cols[!required_cols %in% colnames(data)]
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Rename columns for processing
  plot_data <- data %>%
    rename(
      ID = !!id_col,
      cohort = !!cohort_col,
      days = !!days_col,
      size = !!size_col
    )
  
  # Calculate percentage (if needed)
  if (calculate_percentage) {
    # Check baseline values for each patient
    baseline_check <- plot_data %>%
      group_by(ID) %>%
      filter(days == reference_day) %>%
      summarise(has_baseline = n() > 0)
    
    patients_without_baseline <- baseline_check %>%
      filter(!has_baseline) %>%
      pull(ID)
    
    if (length(patients_without_baseline) > 0) {
      warning("Patients without baseline at days=", reference_day, " will be excluded: ", 
              paste(patients_without_baseline, collapse = ", "))
    }
    
    # Calculate percentage relative to baseline
    plot_data <- plot_data %>%
      group_by(ID) %>%
      mutate(
        baseline_value = size[days == reference_day][1],
        !!percentage_col_name := (size / baseline_value) * 100
      ) %>%
      ungroup() %>%
      filter(!is.na(baseline_value))
    
    y_col <- percentage_col_name
    y_label <- ylab
  } else {
    y_col <- "size"
    y_label <- ifelse(ylab == "Percentage from Baseline (%)", "Measurement Value", ylab)
  }
  
  # 构建副标题
  if (is.null(subtitle) && show_author_date) {
    current_date <- format(Sys.Date(), "%Y-%m-%d")
    subtitle <- paste0(author_name, " | ", current_date)
  }
  
  # 创建基础主题
  base_theme <- switch(theme_style,
                       "lancet" = theme_bw() +
                         theme(
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
                           axis.line = element_line(color = "black"),
                           legend.position = "right",
                           legend.background = element_blank(),
                           legend.key = element_blank(),
                           strip.background = element_rect(fill = "white", color = "black")
                         ),
                       "classic" = theme_classic(),
                       "minimal" = theme_minimal(),
                       theme_bw()
  )
  
  # 添加字体设置
  base_theme <- base_theme +
    theme(
      text = element_text(family = font_family, size = font_size_base),
      plot.title = element_text(family = font_family, size = font_size_title, face = "bold"),
      plot.subtitle = element_text(family = font_family, size = font_size_subtitle, color = "gray30"),
      axis.title = element_text(family = font_family, size = font_size_axis),
      axis.text = element_text(family = font_family, size = font_size_axis),
      legend.title = element_text(family = font_family, size = font_size_legend, face = "bold"),
      legend.text = element_text(family = font_family, size = font_size_legend)
    )
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = days, y = !!sym(y_col), group = ID, color = cohort)) +
    geom_line(alpha = line_alpha, linewidth = line_size) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = y_label,
      color = legend_title
    )
  
  # Add baseline reference line (100%)
  if (calculate_percentage && show_baseline_reference) {
    p <- p + geom_hline(yintercept = baseline_reference_y, 
                        linetype = baseline_reference_type, 
                        color = baseline_reference_color,
                        alpha = 0.5)
  }
  
  # Add PD/PR reference lines (RECIST criteria)
  if (calculate_percentage && show_PD_PR_reference) {
    # PD line (Progressive Disease - 20% increase)
    p <- p + geom_hline(yintercept = 100 + PD_threshold, 
                        linetype = PD_PR_reference_type, 
                        color = PD_reference_color,
                        alpha = 0.5)
    
    # PR line (Partial Response - 30% decrease)
    p <- p + geom_hline(yintercept = 100 + PR_threshold, 
                        linetype = PD_PR_reference_type, 
                        color = PR_reference_color,
                        alpha = 0.5)
  }
  
  # Add points (if needed)
  if (show_points) {
    p <- p + geom_point(size = point_size, alpha = line_alpha)
  }
  
  # Apply color palette
  if (palette == "lancet") {
    p <- p + scale_color_lancet()
  } else if (palette == "nejm") {
    p <- p + scale_color_nejm()
  } else if (palette == "jama") {
    p <- p + scale_color_jama()
  } else {
    warning("Unknown palette, using default colors")
  }
  
  # Custom legend labels (if provided)
  if (!is.null(custom_legend_labels)) {
    p <- p + scale_color_discrete(labels = custom_legend_labels)
  }
  
  # Apply theme
  p <- p + base_theme
  
  # Return plot with calculated data as attribute
  if (calculate_percentage) {
    attr(p, "calculated_data") <- plot_data
    message("Percentage calculation completed. New column '", percentage_col_name, "' added to returned data")
  }
  
  return(p)
}

if(F){
  # Generate example data
  set.seed(123)
  example_data <- data.frame(
    ID = rep(1:10, each = 5),
    cohort = rep(c("Treatment", "Control"), each = 25),
    days = rep(seq(0, 40, by = 10), times = 10),
    size = c(
      # Treatment group: slow growth
      rep(10, 5) + cumsum(c(0, rnorm(4, 0.5, 0.3))),
      rep(10, 5) + cumsum(c(0, rnorm(4, 0.3, 0.4))),
      rep(10, 5) + cumsum(c(0, rnorm(4, 0.4, 0.3))),
      rep(10, 5) + cumsum(c(0, rnorm(4, 0.2, 0.2))),
      rep(10, 5) + cumsum(c(0, rnorm(4, 0.6, 0.4))),
      # Control group: rapid growth
      rep(8, 5) + cumsum(c(0, rnorm(4, 1.2, 0.5))),
      rep(8, 5) + cumsum(c(0, rnorm(4, 1.5, 0.6))),
      rep(8, 5) + cumsum(c(0, rnorm(4, 1.0, 0.4))),
      rep(8, 5) + cumsum(c(0, rnorm(4, 1.8, 0.7))),
      rep(8, 5) + cumsum(c(0, rnorm(4, 1.3, 0.5)))
    )
  )
  
  # Ensure all values are positive
  example_data$size <- abs(example_data$size)
  
  # Example 1: Basic usage with default settings (auto-generated subtitle)
  p1 <- plot_patient_lines(
    data = example_data,
    title = "Tumor Size Change - RECIST Criteria",
    show_PD_PR_reference = TRUE
  )
  print(p1)
  
  # Example 2: Custom subtitle and font settings
  p2 <- plot_patient_lines(
    data = example_data,
    title = "Tumor Response Assessment",
    subtitle = "Clinical Trial Analysis - By LHF (2024-01-15)",  # 自定义副标题
    xlab = "Follow-up Time (days)",
    ylab = "Percentage Change from Baseline (%)",
    legend_title = "Treatment Group",
    palette = "lancet",
    show_PD_PR_reference = TRUE,
    PD_threshold = 25,
    PR_threshold = -35,
    PD_reference_color = "darkred",
    PR_reference_color = "darkgreen",
    show_baseline_reference = TRUE,
    baseline_reference_color = "blue",
    custom_legend_labels = c("Experimental Drug", "Place Control"),
    # 字体设置
    font_family = "serif",
    font_size_base = 12,
    font_size_title = 16,
    font_size_subtitle = 12,
    font_size_axis = 11,
    font_size_legend = 11
  )
  print(p2)
  
  # Example 3: With author and date only
  p3 <- plot_patient_lines(
    data = example_data,
    title = "Tumor Response with Author Info",
    show_author_date = TRUE,  # 显示作者和日期
    author_name = "LHF Lab",  # 自定义作者
    legend_title = "Treatment Arm",
    custom_legend_labels = c("Novel Therapy", "Standard Care"),
    font_family = "sans",
    font_size_base = 11
  ) +
    # Add annotation for reference lines
    annotate("text", x = 5, y = 125, label = "PD (+20%)", 
             color = "red", size = 3, hjust = 0) +
    annotate("text", x = 5, y = 75, label = "PR (-30%)", 
             color = "green", size = 3, hjust = 0)
  print(p3)
  
  # Example 4: Save plot with full customization
  p4 <- plot_patient_lines(
    data = example_data,
    calculate_percentage = TRUE,
    title = "Clinical Trial Results",
    subtitle = "Phase II Study Analysis",  # 自定义副标题
    show_author_date = TRUE,  # 同时显示作者日期
    author_name = "LHF",
    xlab = "Days",
    ylab = "% Change from Baseline",
    legend_title = "Study Arm",
    custom_legend_labels = c("Active Treatment", "Placebo"),
    show_PD_PR_reference = TRUE,
    PD_threshold = 20,
    PR_threshold = -30,
    # 字体设置
    font_family = "sans",
    font_size_base = 12,
    font_size_title = 14,
    font_size_subtitle = 11,
    font_size_axis = 10,
    font_size_legend = 10
  ) +
    # Customize legend appearance
    theme(
      legend.position = "bottom",
      plot.subtitle = element_text(color = "gray50", face = "italic")  # 副标题斜体
    ) +
    # Add title for reference lines
    labs(caption = "Dashed red: PD (+20%), Dashed green: PR (-30%)")
  print(p4)
  
  # Access calculated data
  calculated_data <- attr(p4, "calculated_data")
  head(calculated_data)
  
}
# Save plot
# ggsave("tumor_response_plot.png", p4, width = 10, height = 6, dpi = 300)
