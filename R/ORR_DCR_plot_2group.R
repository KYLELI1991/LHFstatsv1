# 加载必要的包
library(dplyr)
library(tidyr)
library(ggplot2)

# 定义绘图函数
#' Function Title
#'
#' Function Description
#' @export
plot_response_rates <- function(
    df, 
    group_labels = c("A" = "QL1706+LEV", "B" = "QL1706+LEV+CTX"),
    colors = c("A" = "#1f77b4", "B" = "#ff7f0e"),
    p_value_color = "black",          # P值标注颜色
    p_value_shape = "bracket",        # P值标注形状: "bracket", "line", "none"
    title = "ORR and DCR Rates by Treatment Group",
    subtitle = "with 95% Confidence Intervals",
    x_label = NULL,
    y_label = "Response Rate",
    legend_title = "Treatment",
    font_size = 12,
    show_ci_labels = TRUE,
    show_n_labels = TRUE,
    ci_label_position = "above"
) {
  
  # 1. 计算ORR和DCR
  df <- df %>%
    mutate(
      ORR = ifelse(response %in% c("CR", "PR"), 1, 0),
      DCR = ifelse(response %in% c("CR", "PR", "SD"), 1, 0)
    )
  
  # 2. 按组汇总基本统计
  summary_stats <- df %>%
    group_by(group) %>%
    summarise(
      n_total = n(),
      n_ORR = sum(ORR),
      n_DCR = sum(DCR),
      ORR_rate = mean(ORR),
      DCR_rate = mean(DCR)
    ) %>%
    ungroup()
  
  # 3. 使用prop.test计算置信区间
  calculate_ci <- function(success, total) {
    if (total == 0) return(c(0, 0))
    test <- prop.test(success, total, conf.level = 0.95)
    return(c(test$conf.int[1], test$conf.int[2]))
  }
  
  # 计算置信区间
  cis <- df %>%
    group_by(group) %>%
    summarise(
      ORR_ci_lower = calculate_ci(sum(ORR), n())[1],
      ORR_ci_upper = calculate_ci(sum(ORR), n())[2],
      DCR_ci_lower = calculate_ci(sum(DCR), n())[1],
      DCR_ci_upper = calculate_ci(sum(DCR), n())[2]
    )
  
  # 合并数据
  final_stats <- left_join(summary_stats, cis, by = "group")
  
  # 4. 计算组间差异的P值
  # ORR的P值
  orr_test <- prop.test(x = c(final_stats$n_ORR[1], final_stats$n_ORR[2]),
                        n = c(final_stats$n_total[1], final_stats$n_total[2]))
  orr_p <- orr_test$p.value
  
  # DCR的P值
  dcr_test <- prop.test(x = c(final_stats$n_DCR[1], final_stats$n_DCR[2]),
                        n = c(final_stats$n_total[1], final_stats$n_total[2]))
  dcr_p <- dcr_test$p.value
  
  # 5. 准备绘图数据
  plot_data <- final_stats %>%
    pivot_longer(
      cols = c(ORR_rate, DCR_rate),
      names_to = "category",
      values_to = "rate"
    ) %>%
    mutate(
      ci_lower = case_when(
        category == "ORR_rate" ~ ORR_ci_lower,
        category == "DCR_rate" ~ DCR_ci_lower
      ),
      ci_upper = case_when(
        category == "ORR_rate" ~ ORR_ci_upper,
        category == "DCR_rate" ~ DCR_ci_upper
      ),
      category_label = factor(
        ifelse(category == "ORR_rate", "ORR", "DCR"),
        levels = c("ORR", "DCR"),
        ordered = TRUE
      ),
      n_events = ifelse(category == "ORR_rate", n_ORR, n_DCR),
      # 格式化置信区间文本
      ci_text = sprintf("%.1f-%.1f%%", ci_lower * 100, ci_upper * 100),
      # 根据位置参数确定Y坐标
      ci_y_position = case_when(
        ci_label_position == "middle" ~ rate * 0.6,
        ci_label_position == "above" ~ ci_upper + 0.02,
        ci_label_position == "below" ~ ci_lower - 0.02,
        TRUE ~ ci_upper + 0.02  # 默认在上方
      ),
      # 创建分组变量，用于调整柱子的位置
      group_pos = ifelse(group == "A", -0.2, 0.2)
    )
  
  # 6. 创建柱状图 - ORR在左，DCR在右
  # 计算图形的最大Y值
  y_max <- max(plot_data$ci_upper) * 1.2
  
  # 创建图形
  p <- ggplot() +
    # 先画ORR（左边）
    geom_bar(
      data = plot_data %>% filter(category_label == "ORR"),
      aes(x = as.numeric(category_label) + group_pos, 
          y = rate, 
          fill = group),
      stat = "identity",
      width = 0.35,
      alpha = 0.8
    ) +
    # 再画DCR（右边）
    geom_bar(
      data = plot_data %>% filter(category_label == "DCR"),
      aes(x = as.numeric(category_label) + group_pos, 
          y = rate, 
          fill = group),
      stat = "identity",
      width = 0.35,
      alpha = 0.8
    ) +
    
    # ORR的误差线
    geom_errorbar(
      data = plot_data %>% filter(category_label == "ORR"),
      aes(x = as.numeric(category_label) + group_pos,
          ymin = ci_lower,
          ymax = ci_upper),
      width = 0.15,
      size = 0.8,
      color = "black"
    ) +
    
    # DCR的误差线
    geom_errorbar(
      data = plot_data %>% filter(category_label == "DCR"),
      aes(x = as.numeric(category_label) + group_pos,
          ymin = ci_lower,
          ymax = ci_upper),
      width = 0.15,
      size = 0.8,
      color = "black"
    ) +
    
    # ORR的百分比标签
    geom_text(
      data = plot_data %>% filter(category_label == "ORR"),
      aes(x = as.numeric(category_label) + group_pos,
          y = rate,
          label = sprintf("%.1f%%", rate * 100)),
      vjust = -1.2,
      size = 3.5,
      fontface = "bold"
    ) +
    
    # DCR的百分比标签
    geom_text(
      data = plot_data %>% filter(category_label == "DCR"),
      aes(x = as.numeric(category_label) + group_pos,
          y = rate,
          label = sprintf("%.1f%%", rate * 100)),
      vjust = -1.2,
      size = 3.5,
      fontface = "bold"
    )
  
  # 添加置信区间标签
  if (show_ci_labels) {
    p <- p +
      geom_text(
        data = plot_data %>% filter(category_label == "ORR"),
        aes(x = as.numeric(category_label) + group_pos,
            y = ci_y_position,
            label = ci_text),
        size = 3.2,
        color = "darkblue",
        fontface = "bold",
        vjust = ifelse(ci_label_position == "above", -0.5, 
                       ifelse(ci_label_position == "below", 1.5, 0))
      ) +
      
      geom_text(
        data = plot_data %>% filter(category_label == "DCR"),
        aes(x = as.numeric(category_label) + group_pos,
            y = ci_y_position,
            label = ci_text),
        size = 3.2,
        color = "darkblue",
        fontface = "bold",
        vjust = ifelse(ci_label_position == "above", -0.5, 
                       ifelse(ci_label_position == "below", 1.5, 0))
      )
  }
  
  # 添加样本量标签
  if (show_n_labels) {
    p <- p +
      geom_text(
        data = plot_data %>% filter(category_label == "ORR"),
        aes(x = as.numeric(category_label) + group_pos,
            y = rate/2,
            label = sprintf("%d/%d", n_events, n_total)),
        size = 3,
        color = "white",
        fontface = "bold"
      ) +
      
      geom_text(
        data = plot_data %>% filter(category_label == "DCR"),
        aes(x = as.numeric(category_label) + group_pos,
            y = rate/2,
            label = sprintf("%d/%d", n_events, n_total)),
        size = 3,
        color = "white",
        fontface = "bold"
      )
  }
  
  # 根据选择的形状添加P值标注
  if (p_value_shape == "bracket") {
    # 方括号形状
    p <- p +
      # ORR的方括号
      annotate("segment", x = 0.7, xend = 1.3, y = y_max * 0.9, yend = y_max * 0.9,
               size = 1.2, color = p_value_color) +
      annotate("segment", x = 0.7, xend = 0.7, y = y_max * 0.88, yend = y_max * 0.92,
               size = 1.2, color = p_value_color) +
      annotate("segment", x = 1.3, xend = 1.3, y = y_max * 0.88, yend = y_max * 0.92,
               size = 1.2, color = p_value_color) +
      annotate("text", x = 1, y = y_max * 0.95,
               label = ifelse(orr_p < 0.001, "P < 0.001", 
                              paste0("P = ", sprintf("%.3f", orr_p))),
               size = 4, fontface = "bold", color = p_value_color) +
      
      # DCR的方括号
      annotate("segment", x = 1.7, xend = 2.3, y = y_max * 0.9, yend = y_max * 0.9,
               size = 1.2, color = p_value_color) +
      annotate("segment", x = 1.7, xend = 1.7, y = y_max * 0.88, yend = y_max * 0.92,
               size = 1.2, color = p_value_color) +
      annotate("segment", x = 2.3, xend = 2.3, y = y_max * 0.88, yend = y_max * 0.92,
               size = 1.2, color = p_value_color) +
      annotate("text", x = 2, y = y_max * 0.95,
               label = ifelse(dcr_p < 0.001, "P < 0.001", 
                              paste0("P = ", sprintf("%.3f", dcr_p))),
               size = 4, fontface = "bold", color = p_value_color)
    
  } else if (p_value_shape == "line") {
    # 简单直线形状
    p <- p +
      # ORR的直线
      annotate("segment", x = 0.7, xend = 1.3, y = y_max * 0.9, yend = y_max * 0.9,
               size = 1.2, color = p_value_color) +
      annotate("text", x = 1, y = y_max * 0.95,
               label = ifelse(orr_p < 0.001, "P < 0.001", 
                              paste0("P = ", sprintf("%.3f", orr_p))),
               size = 4, fontface = "bold", color = p_value_color) +
      
      # DCR的直线
      annotate("segment", x = 1.7, xend = 2.3, y = y_max * 0.9, yend = y_max * 0.9,
               size = 1.2, color = p_value_color) +
      annotate("text", x = 2, y = y_max * 0.95,
               label = ifelse(dcr_p < 0.001, "P < 0.001", 
                              paste0("P = ", sprintf("%.3f", dcr_p))),
               size = 4, fontface = "bold", color = p_value_color)
    
  } else if (p_value_shape == "dashed") {
    # 虚线形状
    p <- p +
      # ORR的虚线
      annotate("segment", x = 0.7, xend = 1.3, y = y_max * 0.9, yend = y_max * 0.9,
               size = 1.0, color = p_value_color, linetype = "dashed") +
      annotate("text", x = 1, y = y_max * 0.95,
               label = ifelse(orr_p < 0.001, "P < 0.001", 
                              paste0("P = ", sprintf("%.3f", orr_p))),
               size = 4, fontface = "bold", color = p_value_color) +
      
      # DCR的虚线
      annotate("segment", x = 1.7, xend = 2.3, y = y_max * 0.9, yend = y_max * 0.9,
               size = 1.0, color = p_value_color, linetype = "dashed") +
      annotate("text", x = 2, y = y_max * 0.95,
               label = ifelse(dcr_p < 0.001, "P < 0.001", 
                              paste0("P = ", sprintf("%.3f", dcr_p))),
               size = 4, fontface = "bold", color = p_value_color)
    
  } else if (p_value_shape == "arrow") {
    # 箭头形状
    p <- p +
      # ORR的箭头
      annotate("segment", x = 0.7, xend = 1.3, y = y_max * 0.9, yend = y_max * 0.9,
               size = 1.0, color = p_value_color,
               arrow = arrow(ends = "both", length = unit(0.1, "inches"))) +
      annotate("text", x = 1, y = y_max * 0.95,
               label = ifelse(orr_p < 0.001, "P < 0.001", 
                              paste0("P = ", sprintf("%.3f", orr_p))),
               size = 4, fontface = "bold", color = p_value_color) +
      
      # DCR的箭头
      annotate("segment", x = 1.7, xend = 2.3, y = y_max * 0.9, yend = y_max * 0.9,
               size = 1.0, color = p_value_color,
               arrow = arrow(ends = "both", length = unit(0.1, "inches"))) +
      annotate("text", x = 2, y = y_max * 0.95,
               label = ifelse(dcr_p < 0.001, "P < 0.001", 
                              paste0("P = ", sprintf("%.3f", dcr_p))),
               size = 4, fontface = "bold", color = p_value_color)
    
  } else {
    # 只显示文本，不显示形状
    p <- p +
      annotate("text", x = 1, y = y_max * 0.95,
               label = ifelse(orr_p < 0.001, "P < 0.001", 
                              paste0("P = ", sprintf("%.3f", orr_p))),
               size = 4, fontface = "bold", color = p_value_color) +
      
      annotate("text", x = 2, y = y_max * 0.95,
               label = ifelse(dcr_p < 0.001, "P < 0.001", 
                              paste0("P = ", sprintf("%.3f", dcr_p))),
               size = 4, fontface = "bold", color = p_value_color)
  }
  
  # 设置X轴
  p <- p +
    scale_x_continuous(
      breaks = 1:2,
      labels = c("ORR", "DCR"),
      limits = c(0.5, 2.5),
      expand = expansion(mult = 0.1)
    )
  
  # 设置Y轴
  p <- p +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, y_max),
      breaks = seq(0, 1, by = 0.2),
      expand = expansion(mult = c(0, 0.1))
    )
  
  # 设置颜色
  p <- p +
    scale_fill_manual(
      values = colors,
      labels = group_labels,
      name = legend_title
    )
  
  # 标签和标题
  p <- p +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    )
  
  # 主题设置
  p <- p +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray50"),
      axis.text.x = element_text(size = 12, face = "bold", margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.line.x = element_line(color = "black", size = 0.5)
    )
  
  # 返回绘图结果和统计结果
  return(list(
    plot = p,
    stats = final_stats,
    p_values = list(ORR = orr_p, DCR = dcr_p),
    tests = list(ORR = orr_test, DCR = dcr_test)
  ))
}

# 示例1：使用方括号形状，黑色（推荐）
if(F){
  result <- plot_response_rates(
    df = df,
    group_labels = c("A" = "QL1706+LEV", "B" = "QL1706+LEV+CTX"),
    colors = c("A" = "#1f77b4", "B" = "#ff7f0e"),
    p_value_color = "black",
    p_value_shape = "bracket",  # 可改为 "line", "dashed", "arrow" 或保留为空只显示文本
    title = "ORR and DCR Rates by Treatment Group",
    subtitle = "with 95% Confidence Intervals",
    y_label = "Response Rate",
    legend_title = "Treatment",
    font_size = 12,
    show_ci_labels = TRUE,
    show_n_labels = TRUE,
    ci_label_position = "above"   #  "above" 或 "below" middle来调整位置
  )
  
  
  # 保存图形
  # ggsave("response_rates_plot.png", result$plot, width = 10, height = 6, dpi = 300)
  
  # 打印统计结果
  cat("===================== 详细统计结果 =====================\n\n")
  
  for(i in 1:nrow(result$stats)) {
    stats_row <- result$stats[i, ]
    group_name <- ifelse(stats_row$group == "A", "QL1706+LEV", "QL1706+LEV+CTX")
    
    cat(sprintf("组别: %s\n", group_name))
    cat(sprintf("  总例数: %d\n", stats_row$n_total))
    cat(sprintf("  ORR: %d/%d = %.1f%% (95%% CI: %.1f%%-%.1f%%)\n",
                stats_row$n_ORR, stats_row$n_total,
                stats_row$ORR_rate * 100,
                stats_row$ORR_ci_lower * 100,
                stats_row$ORR_ci_upper * 100))
    cat(sprintf("  DCR: %d/%d = %.1f%% (95%% CI: %.1f%%-%.1f%%)\n\n",
                stats_row$n_DCR, stats_row$n_total,
                stats_row$DCR_rate * 100,
                stats_row$DCR_ci_lower * 100,
                stats_row$DCR_ci_upper * 100))
  }
  
  cat("===================== 组间比较结果 =====================\n")
  cat(sprintf("ORR: P = %.4f\n", result$p_values$ORR))
  cat(sprintf("DCR: P = %.4f\n", result$p_values$DCR))
}
