# 定义绘图函数
#' Function Title
#'
#' Function Description
#' @export
plot_response_rates <- function(
    df, 
    group_col = "group",              # 组别列名
    response_col = "response",         # 响应列名
    group_labels = NULL,               # 组标签，如果为NULL则使用原始组名
    colors = NULL,                      # 颜色，如果为NULL则使用默认颜色
    p_value_color = "black",            # P值标注颜色
    p_value_shape = "bracket",          # P值标注形状: "bracket", "line", "dashed", "arrow", "none"
    title = "ORR and DCR Rates by Group",
    subtitle = "with 95% Confidence Intervals",
    x_label = NULL,
    y_label = "Response Rate",
    legend_title = "Group",
    font_size = 12,
    show_ci_labels = TRUE,
    show_n_labels = TRUE,
    ci_label_position = "above",        # "above", "below", "middle", "none"
    ci_label_size = 3,
    p_adjust_method = "none",           # 多重比较校正方法: "none", "bonferroni", "holm", "fdr"
    response_levels = c("CR", "PR", "SD", "PD"),  # 响应级别顺序
    orr_levels = c("CR", "PR"),          # ORR包含的响应级别
    dcr_levels = c("CR", "PR", "SD"),    # DCR包含的响应级别
    
    # 新增控制参数
    show_comparisons = TRUE,             # 是否显示两两比较（新增）
    max_comparisons_show = NULL,         # 最多显示多少个比较（NULL表示显示所有）
    comparison_start_y = 0.85,           # 比较标注起始Y位置（相对于y_max的比例）
    comparison_spacing = 0.08,           # 比较标注之间的垂直间距
    comparison_text_size = 3,            # 比较P值文本大小
    comparison_line_size = 0.8,          # 比较连接线粗细
    
    rotate_x_labels = FALSE,              # 是否旋转X轴标签
    x_label_angle = 45,                   # X轴标签旋转角度
    
    bar_width = 0.25,                     # 柱子宽度
    bar_alpha = 0.8,                      # 柱子透明度
    
    show_orr_only = FALSE,                 # 只显示ORR（不显示DCR）
    show_dcr_only = FALSE,                  # 只显示DCR（不显示ORR）
    
    y_max_manual = NULL,                    # 手动设置Y轴最大值
    y_breaks = seq(0, 1, by = 0.2),         # Y轴刻度
    
    show_legend = TRUE,                      # 是否显示图例
    legend_position = "top",                  # 图例位置
    
    output_stats_only = FALSE,                # 只输出统计结果，不绘图
    
    # 新增P值表格选项
    show_p_value_table = FALSE,               # 是否显示P值表格
    p_value_table_position = "bottom"         # P值表格位置: "bottom", "right", "top"
) {
  
  # 加载必要的包
  if (!require(dplyr)) stop("需要安装dplyr包")
  if (!require(tidyr)) stop("需要安装tidyr包")
  if (!require(ggplot2)) stop("需要安装ggplot2包")
  if (!require(rlang)) stop("需要安装rlang包")
  if (show_p_value_table && !require(gridExtra)) {
    warning("显示P值表格需要安装gridExtra包，将不显示表格")
    show_p_value_table <- FALSE
  }
  if (show_p_value_table && !require(grid)) {
    warning("显示P值表格需要安装grid包，将不显示表格")
    show_p_value_table <- FALSE
  }
  
  # 检查必要的列是否存在
  if (!group_col %in% colnames(df)) {
    stop(paste("组别列", group_col, "不存在于数据框中"))
  }
  if (!response_col %in% colnames(df)) {
    stop(paste("响应列", response_col, "不存在于数据框中"))
  }
  
  # 重命名列以便内部使用
  df <- df %>%
    rename(
      group = all_of(group_col),
      response = all_of(response_col)
    )
  
  # 检查响应列的数据类型
  if (!is.character(df$response) && !is.factor(df$response)) {
    stop("响应列必须是字符型或因子型")
  }
  
  # 将响应列转换为因子（如果还不是）
  if (!is.factor(df$response)) {
    df$response <- factor(df$response, levels = response_levels)
  }
  
  # 获取唯一的组标识
  groups <- unique(df$group)
  n_groups <- length(groups)
  
  # 检查组数量
  if (n_groups < 2) {
    stop("至少需要2个组进行比较")
  }
  
  # 检查show_orr_only和show_dcr_only不能同时为TRUE
  if (show_orr_only && show_dcr_only) {
    warning("show_orr_only和show_dcr_only不能同时为TRUE，将显示两者")
    show_orr_only <- FALSE
    show_dcr_only <- FALSE
  }
  
  # 设置组标签
  if (is.null(group_labels)) {
    group_labels <- setNames(as.character(groups), groups)
  } else {
    # 检查组标签长度
    if (length(group_labels) != n_groups) {
      warning("组标签数量与组数不匹配，将使用组名作为标签")
      group_labels <- setNames(as.character(groups), groups)
    }
  }
  
  # 设置颜色
  if (is.null(colors)) {
    # 使用ggplot2默认颜色
    colors <- setNames(
      scales::hue_pal()(n_groups),
      groups
    )
  } else {
    # 检查颜色数量
    if (length(colors) < n_groups) {
      warning("颜色数量不足，将使用默认颜色")
      colors <- setNames(
        scales::hue_pal()(n_groups),
        groups
      )
    }
  }
  
  # 1. 计算ORR和DCR
  df <- df %>%
    mutate(
      ORR = ifelse(response %in% orr_levels, 1, 0),
      DCR = ifelse(response %in% dcr_levels, 1, 0)
    )
  
  # 打印诊断信息
  cat("数据摘要:\n")
  cat("组别分布:\n")
  print(table(df$group))
  cat("\n响应分布:\n")
  print(table(df$response))
  cat("\nORR定义:", paste(orr_levels, collapse = ", "), "\n")
  cat("DCR定义:", paste(dcr_levels, collapse = ", "), "\n")
  
  # 2. 按组汇总基本统计
  summary_stats <- df %>%
    group_by(group) %>%
    summarise(
      n_total = n(),
      n_ORR = sum(ORR),
      n_DCR = sum(DCR),
      ORR_rate = mean(ORR),
      DCR_rate = mean(DCR),
      ORR_se = sqrt(mean(ORR) * (1 - mean(ORR)) / n()),  # 标准误
      DCR_se = sqrt(mean(DCR) * (1 - mean(DCR)) / n())
    ) %>%
    ungroup()
  
  # 3. 使用prop.test计算置信区间
  calculate_ci <- function(success, total) {
    if (total == 0) return(c(0, 0))
    if (success == 0) return(c(0, 0))
    if (success == total) return(c(1, 1))
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
  
  # 4. 计算组间差异的P值（两两比较）- 即使不显示也计算，用于表格
  # 生成所有组对
  group_pairs <- combn(groups, 2, simplify = FALSE)
  n_comparisons <- length(group_pairs)
  
  # 计算所有比较的原始P值
  p_values_raw <- list()
  
  for (i in seq_along(group_pairs)) {
    pair <- group_pairs[[i]]
    pair_name <- paste(pair, collapse = "_vs_")
    
    # 获取两组的数据
    stats_pair <- final_stats %>% filter(group %in% pair)
    
    # ORR的P值
    orr_test <- tryCatch({
      prop.test(
        x = stats_pair$n_ORR,
        n = stats_pair$n_total
      )
    }, error = function(e) {
      warning(paste("ORR比较", pair_name, "失败:", e$message))
      return(list(p.value = NA))
    })
    
    # DCR的P值
    dcr_test <- tryCatch({
      prop.test(
        x = stats_pair$n_DCR,
        n = stats_pair$n_total
      )
    }, error = function(e) {
      warning(paste("DCR比较", pair_name, "失败:", e$message))
      return(list(p.value = NA))
    })
    
    p_values_raw[[pair_name]] <- list(
      pair = pair,
      ORR_raw = ifelse(is.null(orr_test$p.value), NA, orr_test$p.value),
      DCR_raw = ifelse(is.null(dcr_test$p.value), NA, dcr_test$p.value),
      ORR_test = orr_test,
      DCR_test = dcr_test
    )
  }
  
  # 应用多重比较校正
  p_values_adjusted <- list()
  
  if (p_adjust_method != "none" && n_comparisons > 1) {
    # 提取所有原始P值（排除NA）
    all_orr_p <- sapply(p_values_raw, function(x) x$ORR_raw)
    all_dcr_p <- sapply(p_values_raw, function(x) x$DCR_raw)
    
    # 标记有效P值
    valid_orr <- !is.na(all_orr_p)
    valid_dcr <- !is.na(all_dcr_p)
    
    # 校正P值（只对有效值进行校正）
    adjusted_orr_p <- rep(NA, length(all_orr_p))
    adjusted_dcr_p <- rep(NA, length(all_dcr_p))
    
    if (sum(valid_orr) > 0) {
      adjusted_orr_p[valid_orr] <- p.adjust(all_orr_p[valid_orr], method = p_adjust_method)
    }
    if (sum(valid_dcr) > 0) {
      adjusted_dcr_p[valid_dcr] <- p.adjust(all_dcr_p[valid_dcr], method = p_adjust_method)
    }
    
    for (i in seq_along(p_values_raw)) {
      pair_name <- names(p_values_raw)[i]
      p_values_adjusted[[pair_name]] <- list(
        pair = p_values_raw[[i]]$pair,
        ORR = adjusted_orr_p[i],
        DCR = adjusted_dcr_p[i],
        ORR_raw = p_values_raw[[i]]$ORR_raw,
        DCR_raw = p_values_raw[[i]]$DCR_raw,
        ORR_test = p_values_raw[[i]]$ORR_test,
        DCR_test = p_values_raw[[i]]$DCR_test
      )
    }
  } else {
    # 不使用校正
    for (i in seq_along(p_values_raw)) {
      pair_name <- names(p_values_raw)[i]
      p_values_adjusted[[pair_name]] <- list(
        pair = p_values_raw[[i]]$pair,
        ORR = p_values_raw[[i]]$ORR_raw,
        DCR = p_values_raw[[i]]$DCR_raw,
        ORR_raw = p_values_raw[[i]]$ORR_raw,
        DCR_raw = p_values_raw[[i]]$DCR_raw,
        ORR_test = p_values_raw[[i]]$ORR_test,
        DCR_test = p_values_raw[[i]]$DCR_test
      )
    }
  }
  
  # 创建P值表格数据（用于显示）
  p_value_table_data <- NULL
  if (show_p_value_table) {
    p_value_table_data <- data.frame(
      比较 = names(p_values_adjusted),
      ORR_P值 = sapply(p_values_adjusted, function(x) {
        if (is.na(x$ORR)) return("NA")
        if (x$ORR < 0.001) return("<0.001")
        return(sprintf("%.3f", x$ORR))
      }),
      DCR_P值 = sapply(p_values_adjusted, function(x) {
        if (is.na(x$DCR)) return("NA")
        if (x$DCR < 0.001) return("<0.001")
        return(sprintf("%.3f", x$DCR))
      }),
      stringsAsFactors = FALSE
    )
    
    # 美化比较名称
    p_value_table_data$比较 <- sapply(p_values_adjusted, function(x) {
      paste(group_labels[x$pair[1]], "vs", group_labels[x$pair[2]])
    })
  }
  
  # 如果只输出统计结果，则提前返回
  if (output_stats_only) {
    return(list(
      stats = final_stats,
      p_values = p_values_adjusted,
      comparisons = group_pairs,
      n_comparisons = n_comparisons,
      p_adjust_method = p_adjust_method,
      p_value_table = p_value_table_data
    ))
  }
  
  # 5. 准备绘图数据
  # 确定要显示的类别
  categories_to_show <- c("ORR_rate", "DCR_rate")
  category_labels <- c("ORR", "DCR")
  
  if (show_orr_only) {
    categories_to_show <- "ORR_rate"
    category_labels <- "ORR"
  } else if (show_dcr_only) {
    categories_to_show <- "DCR_rate"
    category_labels <- "DCR"
  }
  
  plot_data <- final_stats %>%
    pivot_longer(
      cols = all_of(categories_to_show),
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
        levels = category_labels,
        ordered = TRUE
      ),
      n_events = ifelse(category == "ORR_rate", n_ORR, n_DCR),
      # 格式化置信区间文本
      ci_text = sprintf("%.1f-%.1f%%", ci_lower * 100, ci_upper * 100),
      # 根据位置参数确定Y坐标
      ci_y_position = case_when(
        ci_label_position == "middle" ~ rate * 0.5,
        ci_label_position == "above" ~ ci_upper + 0.02,
        ci_label_position == "below" ~ ci_lower - 0.02,
        ci_label_position == "none" ~ NA_real_,
        TRUE ~ ci_upper + 0.02  # 默认在上方
      )
    )
  
  # 计算每组的位置偏移量（用于分组柱状图）
  group_positions <- data.frame(
    group = groups,
    pos_offset = seq(-bar_width, bar_width, length.out = n_groups)
  )
  
  plot_data <- plot_data %>%
    left_join(group_positions, by = "group") %>%
    mutate(
      x_position = as.numeric(category_label) + pos_offset
    )
  
  # 计算图形的最大Y值
  if (!is.null(y_max_manual)) {
    y_max <- y_max_manual
  } else {
    y_max <- max(plot_data$ci_upper, na.rm = TRUE) * 1.3
    # 如果有多个比较且要显示，需要更多空间
    if (show_comparisons && n_comparisons > 1 && p_value_shape != "none") {
      n_show <- ifelse(is.null(max_comparisons_show), n_comparisons, 
                       min(n_comparisons, max_comparisons_show))
      y_max <- max(y_max, max(plot_data$ci_upper, na.rm = TRUE) * (1 + n_show * comparison_spacing * 0.8))
    }
  }
  
  # 6. 创建基础图形
  p <- ggplot() +
    # 添加柱子
    geom_bar(
      data = plot_data,
      aes(x = x_position, y = rate, fill = group),
      stat = "identity",
      width = bar_width * 0.8,
      alpha = bar_alpha,
      position = position_identity()
    ) +
    
    # 添加误差线
    geom_errorbar(
      data = plot_data,
      aes(x = x_position, ymin = ci_lower, ymax = ci_upper),
      width = bar_width * 0.3,
      size = 0.8,
      color = "black"
    ) +
    
    # 添加百分比标签
    geom_text(
      data = plot_data,
      aes(x = x_position, y = rate, label = sprintf("%.1f%%", rate * 100)),
      vjust = -1.2,
      size = font_size * 0.3,
      fontface = "bold"
    )
  
  # 添加置信区间标签
  if (show_ci_labels && ci_label_position != "none") {
    # 过滤掉ci_y_position为NA的行
    ci_label_data <- plot_data %>% filter(!is.na(ci_y_position))
    
    if (nrow(ci_label_data) > 0) {
      p <- p +
        geom_text(
          data = ci_label_data,
          aes(x = x_position, y = ci_y_position, label = ci_text),
          size = ci_label_size,
          color = "darkblue",
          fontface = "bold",
          vjust = ifelse(ci_label_position == "above", -0.2, 
                         ifelse(ci_label_position == "below", 1.2, 0.5))
        )
    }
  }
  
  # 添加样本量标签
  if (show_n_labels) {
    p <- p +
      geom_text(
        data = plot_data,
        aes(x = x_position, y = rate/2, label = sprintf("%d/%d", n_events, n_total)),
        size = font_size * 0.25,
        color = "white",
        fontface = "bold"
      )
  }
  
  # 7. 添加P值标注（两两比较）- 仅在show_comparisons为TRUE时显示
  if (show_comparisons && p_value_shape != "none" && n_comparisons > 0) {
    # 确定要显示的比较数量
    n_show <- ifelse(is.null(max_comparisons_show), n_comparisons, 
                     min(n_comparisons, max_comparisons_show))
    
    if (n_show > 0) {
      # 为每组比较计算标注位置
      # 根据P值大小排序，小的P值优先显示（更显著的结果）
      p_values_list <- p_values_adjusted[1:n_show]
      
      # 计算每个比较的显著性分数（用于排序）
      p_values_list <- p_values_list[order(
        sapply(p_values_list, function(x) min(x$ORR, x$DCR, na.rm = TRUE))
      )]
      
      # 基础Y位置
      base_y <- y_max * comparison_start_y
      
      for (i in seq_along(p_values_list)) {
        comparison <- p_values_list[[i]]
        pair <- comparison$pair
        
        # 获取两组的位置
        pos1 <- group_positions$pos_offset[group_positions$group == pair[1]]
        pos2 <- group_positions$pos_offset[group_positions$group == pair[2]]
        
        # 计算Y位置（根据比较索引调整高度，避免重叠）
        y_line <- base_y - (i-1) * comparison_spacing * y_max
        
        # 确定要显示的类别位置
        x_positions <- if (!show_orr_only && !show_dcr_only) {
          list(orr = 1, dcr = 2)
        } else if (show_orr_only) {
          list(orr = 1)
        } else if (show_dcr_only) {
          list(dcr = 1)
        }
        
        # 根据选择的形状添加标注
        for (cat_name in names(x_positions)) {
          cat_x_base <- x_positions[[cat_name]]
          p_value <- ifelse(cat_name == "orr", comparison$ORR, comparison$DCR)
          
          # 跳过NA的P值
          if (is.na(p_value)) next
          
          x_start <- cat_x_base + pos1
          x_end <- cat_x_base + pos2
          x_mid <- (x_start + x_end) / 2
          
          # 添加连接线
          if (p_value_shape %in% c("bracket", "line", "dashed", "arrow")) {
            
            line_type <- ifelse(p_value_shape == "dashed", "dashed", "solid")
            
            p <- p +
              annotate("segment", 
                       x = x_start, xend = x_end, 
                       y = y_line, yend = y_line,
                       size = comparison_line_size,
                       color = p_value_color,
                       linetype = line_type)
            
            # 如果是方括号，添加两端竖线
            if (p_value_shape == "bracket") {
              p <- p +
                annotate("segment", 
                         x = x_start, xend = x_start, 
                         y = y_line - 0.01, yend = y_line + 0.01,
                         size = comparison_line_size, color = p_value_color) +
                annotate("segment", 
                         x = x_end, xend = x_end, 
                         y = y_line - 0.01, yend = y_line + 0.01,
                         size = comparison_line_size, color = p_value_color)
            }
            
            # 如果是箭头，添加箭头
            if (p_value_shape == "arrow") {
              p <- p +
                annotate("segment", 
                         x = x_start, xend = x_end, 
                         y = y_line, yend = y_line,
                         size = comparison_line_size, color = p_value_color,
                         arrow = arrow(ends = "both", length = unit(0.1, "inches")))
            }
            
            # 添加P值文本
            p <- p +
              annotate("text", 
                       x = x_mid, 
                       y = y_line + 0.02,
                       label = ifelse(p_value < 0.001, 
                                      "P < 0.001", 
                                      paste0("P = ", sprintf("%.3f", p_value))),
                       size = comparison_text_size, 
                       fontface = "bold", 
                       color = p_value_color)
          }
        }
      }
      
      # 如果比较数量超过显示数量，添加提示
      if (n_comparisons > n_show) {
        p <- p # +
          #annotate("text", 
                   #x = ifelse(show_orr_only || show_dcr_only, 1, 1.5),
                   #y = base_y - n_show * comparison_spacing * y_max - 0.02,
                   #label = paste0("... 还有", n_comparisons - n_show, "组比较未显示"),
                   #size = comparison_text_size - 1,
                   #color = "gray50",
                   #fontface = "italic")
      }
    }
  }
  
  # 8. 添加多重比较校正说明
  if (p_adjust_method != "none" && n_comparisons > 1 && show_comparisons) {
    method_names <- c("bonferroni" = "Bonferroni", 
                      "holm" = "Holm", 
                      "hochberg" = "Hochberg",
                      "hommel" = "Hommel",
                      "fdr" = "FDR (Benjamini-Hochberg)",
                      "BH" = "Benjamini-Hochberg",
                      "BY" = "Benjamini-Yekutieli")
    method_label <- ifelse(p_adjust_method %in% names(method_names),
                           method_names[p_adjust_method],
                           p_adjust_method)
    
    p <- p +
      labs(caption = paste0("P值已使用", method_label, "方法进行多重比较校正"))
  }
  
  # 9. 设置X轴
  x_breaks <- 1:length(category_labels)
  
  p <- p +
    scale_x_continuous(
      breaks = x_breaks,
      labels = category_labels,
      limits = c(min(x_breaks) - 0.5, max(x_breaks) + 0.5),
      expand = expansion(mult = 0.1)
    )
  
  # 10. 设置Y轴
  p <- p +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, y_max),
      breaks = y_breaks,
      expand = expansion(mult = c(0, 0.05))
    )
  
  # 11. 设置颜色
  p <- p +
    scale_fill_manual(
      values = colors,
      labels = group_labels,
      name = legend_title
    )
  
  # 12. 标签和标题
  p <- p +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    )
  
  # 13. 主题设置
  theme_args <- list(
    plot.title = element_text(hjust = 0.5, face = "bold", size = font_size + 2),
    plot.subtitle = element_text(hjust = 0.5, size = font_size - 1, color = "gray50"),
    plot.caption = element_text(hjust = 0.5, size = font_size - 2, color = "gray50", face = "italic"),
    axis.text.x = element_text(size = font_size, face = "bold", margin = margin(t = 5)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    axis.line.x = element_line(color = "black", size = 0.5)
  )
  
  # 添加图例相关设置
  if (show_legend) {
    theme_args$legend.position <- legend_position
    theme_args$legend.title <- element_text(face = "bold", size = font_size - 1)
    theme_args$legend.text <- element_text(size = font_size - 2)
  } else {
    theme_args$legend.position <- "none"
  }
  
  # 添加X轴标签旋转
  if (rotate_x_labels) {
    theme_args$axis.text.x <- element_text(
      size = font_size, 
      face = "bold", 
      angle = x_label_angle, 
      hjust = 1,
      margin = margin(t = 5)
    )
  }
  
  p <- p + do.call(theme_minimal, list(base_size = font_size)) + do.call(theme, theme_args)
  
  # 14. 如果需要显示P值表格，创建组合图形
  if (show_p_value_table && !is.null(p_value_table_data)) {
    # 创建表格图形
    table_grob <- tableGrob(
      p_value_table_data,
      rows = NULL,
      theme = ttheme_minimal(
        core = list(fg_params = list(hjust = 0, x = 0.1),
                    bg_params = list(fill = "white")),
        colhead = list(fg_params = list(fontface = "bold", hjust = 0, x = 0.1))
      )
    )
    
    # 根据位置组合图形
    if (p_value_table_position == "bottom") {
      p <- gridExtra::grid.arrange(p, table_grob, ncol = 1, 
                                   heights = c(3, 1))
    } else if (p_value_table_position == "top") {
      p <- gridExtra::grid.arrange(table_grob, p, ncol = 1, 
                                   heights = c(1, 3))
    } else if (p_value_table_position == "right") {
      p <- gridExtra::grid.arrange(p, table_grob, ncol = 2, 
                                   widths = c(3, 1.5))
    }
  }
  
  # 返回绘图结果和统计结果
  return(list(
    plot = p,
    stats = final_stats,
    p_values = p_values_adjusted,
    comparisons = group_pairs,
    n_comparisons = n_comparisons,
    p_adjust_method = p_adjust_method,
    group_labels = group_labels,
    colors = colors,
    plot_data = plot_data,
    p_value_table = p_value_table_data
  ))
}

# 使用示例：
# 
# # 1. 不显示两两比较（简洁版）
# result_no_comparisons <- plot_response_rates(
#   df = your_data,
#   group_col = "treatment",
#   response_col = "response",
#   show_comparisons = FALSE,           # 不显示两两比较
#   show_ci_labels = TRUE,               # 显示置信区间
#   show_n_labels = TRUE                  # 显示样本量
# )
# 
# # 2. 显示两两比较但限制数量
# result_limited <- plot_response_rates(
#   df = your_data,
#   group_col = "treatment",
#   response_col = "response",
#   show_comparisons = TRUE,              # 显示两两比较
#   max_comparisons_show = 2,              # 只显示最显著的2组
#   p_value_shape = "line",                # 使用直线连接
#   comparison_start_y = 0.9,              # 从较高的位置开始
#   comparison_spacing = 0.06               # 比较之间更紧凑
# )
# 
# # 3. 显示P值表格（替代图形上的标注）
# result_with_table <- plot_response_rates(
#   df = your_data,
#   group_col = "treatment",
#   response_col = "response",
#   show_comparisons = FALSE,               # 图形上不显示比较
#   show_p_value_table = TRUE,               # 显示P值表格
#   p_value_table_position = "bottom",       # 表格放在底部
#   p_adjust_method = "bonferroni"           # 使用Bonferroni校正
# )
# 
# # 4. 完全简洁版（只显示柱状图）
# result_simple <- plot_response_rates(
#   df = your_data,
#   group_col = "treatment",
#   response_col = "response",
#   show_comparisons = FALSE,
#   show_ci_labels = FALSE,
#   show_n_labels = TRUE,
#   p_value_shape = "none"
# )
