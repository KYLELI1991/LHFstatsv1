#' Function Title
#'
#' Function Description
#' @export
create_waterfall_plot <- function(data,
                                  vars,
                                  cohort_col = "cohort",
                                  second_legend = "response",
                                  cohort_colors = c("cadetblue", "steelblue", "#9E58CC", "red"),
                                  second_legend_symbols = NULL,
                                  main_title = "Waterfall plot for Target Lesion",
                                  ylab_label = "Change from baseline (%)",
                                  y_limits = c(-100, 100),
                                  hline_values = c(20, -30),
                                  hline_col = "black",
                                  hline_lwd = 2,
                                  hline_lty = 3,
                                  bar_space = 0.5,
                                  show_date = TRUE,
                                  date_format = "%Y-%m-%d",
                                  date_cex = 1.0,
                                  date_font = 3,
                                  legend_cohorts = NULL,
                                  legend_cohorts_labels = NULL,
                                  second_legend_title = "Legend",
                                  second_legend_position = "bottomleft",
                                  second_legend_inset = c(0.02, 0.02),
                                  second_legend_cex = 0.8,
                                  legend_cohorts_cex = 0.9,
                                  legend_cohorts_position = "topright",
                                  legend_cohorts_inset = c(0.02, 0.02),
                                  cex_main = 1.2,
                                  cex_lab = 1.2,
                                  cex_axis = 1.5,
                                  symbol_y_offset = 3,
                                  add_group_separator = TRUE,
                                  separator_col = "gray50",
                                  separator_lwd = 2,
                                  separator_lty = 2,
                                  symbol_display_mode = "bottom",
                                  symbol_spacing = 2.5,
                                  second_legend_vars = NULL,
                                  auto_position_symbol = TRUE) {
  
  # 输入验证
  required_cols <- c(cohort_col, vars)
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    stop("数据框中缺少以下必要的列: ", paste(missing_cols, collapse = ", "))
  }
  
  # 处理第二图例变量
  if (is.null(second_legend_vars)) {
    second_legend_vars <- second_legend
    multi_legend <- FALSE
  } else {
    multi_legend <- TRUE
    missing_vars <- second_legend_vars[!second_legend_vars %in% names(data)]
    if (length(missing_vars) > 0) {
      stop("第二图例变量缺失: ", paste(missing_vars, collapse = ", "))
    }
  }
  
  # 确保second_legend_vars是字符向量
  if (is.character(second_legend_vars) && length(second_legend_vars) == 1) {
    multi_legend <- FALSE
  }
  
  # 获取唯一的cohort值
  unique_cohorts <- unique(data[[cohort_col]])
  n_cohorts <- length(unique_cohorts)
  
  if (length(cohort_colors) < n_cohorts) {
    warning("cohort数量多于提供的颜色数量，将循环使用颜色")
  }
  
  # 设置颜色向量
  col <- character(nrow(data))
  for (i in 1:nrow(data)) {
    cohort_idx <- match(data[[cohort_col]][i], unique_cohorts)
    col[i] <- cohort_colors[((cohort_idx - 1) %% length(cohort_colors)) + 1]
  }
  
  # 准备cohort图例文本
  if (is.null(legend_cohorts)) {
    if (!is.null(legend_cohorts_labels)) {
      if (length(legend_cohorts_labels) >= n_cohorts) {
        legend_text <- legend_cohorts_labels[1:n_cohorts]
      } else {
        warning("自定义标签数量少于cohort数量，将使用默认标签")
        legend_text <- unique_cohorts
      }
    } else {
      legend_text <- unique_cohorts
    }
  } else {
    if (length(legend_cohorts) >= n_cohorts) {
      legend_text <- legend_cohorts[1:n_cohorts]
    } else {
      warning("提供的图例文本数量少于cohort数量，将使用默认标签")
      legend_text <- unique_cohorts
    }
  }
  
  # 准备图例颜色
  legend_colors <- cohort_colors[1:n_cohorts]
  
  # 获取bar的值
  bar_values <- data[, vars]
  
  # 调整y轴范围，为符号预留更多空间
  adjusted_y_limits <- y_limits
  
  # 当使用自动位置时，计算需要的y轴范围
  if (auto_position_symbol && !multi_legend) {
    # 计算符号位置
    min_needed <- Inf
    max_needed <- -Inf
    
    for (i in 1:nrow(data)) {
      bar_value <- bar_values[i]
      
      if (bar_value >= 0) {
        # 正值：符号在下方
        symbol_pos <- bar_value - symbol_y_offset
        min_needed <- min(min_needed, symbol_pos)
      } else {
        # 负值：符号在上方
        symbol_pos <- bar_value + symbol_y_offset
        max_needed <- max(max_needed, symbol_pos)
      }
    }
    
    # 扩展y轴范围，预留更多空间
    if (!is.infinite(min_needed)) {
      adjusted_y_limits[1] <- min(min_needed - 5, y_limits[1])
    }
    if (!is.infinite(max_needed)) {
      adjusted_y_limits[2] <- max(max_needed + 5, y_limits[2])
    }
  } else {
    if (symbol_display_mode == "bottom") {
      adjusted_y_limits[1] <- y_limits[1] - 20
    } else if (symbol_display_mode == "top") {
      adjusted_y_limits[2] <- y_limits[2] + 20
    }
  }
  
  # 创建条形图 - 不添加边框，让符号显示在bar上层
  MCC <- barplot(bar_values, 
                 col = col, 
                 border = col, 
                 space = bar_space, 
                 ylim = adjusted_y_limits,
                 main = main_title, 
                 ylab = ylab_label,
                 cex.main = cex_main,
                 cex.lab = cex_lab,
                 cex.axis = cex_axis)
  
  # 添加分组分隔线
  if (add_group_separator) {
    current_cohort <- data[[cohort_col]][1]
    for (i in 1:nrow(data)) {
      if (data[[cohort_col]][i] != current_cohort) {
        if (i > 1) {
          sep_pos <- (MCC[i-1] + MCC[i]) / 2
          abline(v = sep_pos, col = separator_col, lwd = separator_lwd, lty = separator_lty)
        }
        current_cohort <- data[[cohort_col]][i]
      }
    }
  }
  
  # 添加cohort图例
  legend(legend_cohorts_position,
         legend = legend_text,
         fill = legend_colors,
         border = NA,
         title = cohort_col,
         cex = legend_cohorts_cex,
         inset = legend_cohorts_inset,
         bty = "n")
  
  # 添加日期
  if (show_date) {
    current_date <- format(Sys.Date(), date_format)
    mtext(text = paste("By LHF; Date:", current_date), 
          side = 3,
          line = 0.5,
          cex = date_cex,
          font = date_font)
  }
  
  # 添加参考线
  for (h in hline_values) {
    abline(h = h, col = hline_col, lwd = hline_lwd, lty = hline_lty)
  }
  
  # 定义不同变量的符号集
  var_pch_sets <- list(
    list(c(16, 17, 15, 18, 8, 3, 4, 6, 2, 1),
         c("darkgreen", "blue", "orange", "red", "purple", 
           "brown", "pink", "darkcyan", "magenta", "darkblue")),
    list(c(0, 1, 2, 5, 6, 9, 10, 11, 12, 13),
         c("darkgreen", "blue", "orange", "red", "purple", 
           "brown", "pink", "darkcyan", "magenta", "darkblue")),
    list(c(4, 8, 25, 24, 23, 22, 21, 3, 7, 14),
         c("darkgreen", "blue", "orange", "red", "purple", 
           "brown", "pink", "darkcyan", "magenta", "darkblue")),
    list(c(18, 19, 20, 21, 22, 23, 24, 25, 11, 12),
         c("darkgreen", "blue", "orange", "red", "purple", 
           "brown", "pink", "darkcyan", "magenta", "darkblue"))
  )
  
  # 处理第二图例
  all_legend_data <- list()
  
  if (multi_legend) {
    # 多个变量模式
    for (var_idx in 1:length(second_legend_vars)) {
      legend_var <- second_legend_vars[var_idx]
      unique_values <- unique(data[[legend_var]])
      n_unique <- length(unique_values)
      
      pch_set_idx <- ((var_idx - 1) %% length(var_pch_sets)) + 1
      pch_options <- var_pch_sets[[pch_set_idx]][[1]]
      color_options <- var_pch_sets[[pch_set_idx]][[2]]
      
      if (is.null(second_legend_symbols) || is.null(second_legend_symbols[[legend_var]])) {
        var_symbols <- list()
        for (i in 1:n_unique) {
          val <- as.character(unique_values[i])
          pch_idx <- ((i - 1) %% length(pch_options)) + 1
          color_idx <- ((i - 1) %% length(color_options)) + 1
          
          var_symbols[[val]] <- list(
            pch = pch_options[pch_idx],
            col = color_options[color_idx],
            cex = 1.5,
            var_name = legend_var
          )
        }
        all_legend_data[[legend_var]] <- var_symbols
      } else {
        for (val in names(second_legend_symbols[[legend_var]])) {
          second_legend_symbols[[legend_var]][[val]]$var_name <- legend_var
        }
        all_legend_data[[legend_var]] <- second_legend_symbols[[legend_var]]
      }
    }
    
    # 统一位置显示多个符号
    if (symbol_display_mode == "bottom") {
      base_y <- adjusted_y_limits[1] + symbol_y_offset
    } else {
      base_y <- adjusted_y_limits[2] - symbol_y_offset
    }
    
    for (i in 1:nrow(data)) {
      y_positions <- base_y + (0:(length(second_legend_vars)-1)) * symbol_spacing
      
      for (j in 1:length(second_legend_vars)) {
        legend_var <- second_legend_vars[j]
        val <- as.character(data[[legend_var]][i])
        symbol_params <- all_legend_data[[legend_var]][[val]]
        
        if (is.null(symbol_params)) {
          symbol_params <- list(pch = 16, col = "black", cex = 1.5)
        }
        
        points(x = MCC[i], 
               y = y_positions[j],
               pch = symbol_params$pch,
               col = symbol_params$col,
               cex = symbol_params$cex)
      }
    }
    
    # 构建图例
    legend_labels <- c()
    legend_pch <- c()
    legend_col <- c()
    legend_cex <- c()
    
    for (var_idx in 1:length(second_legend_vars)) {
      legend_var <- second_legend_vars[var_idx]
      var_symbols <- all_legend_data[[legend_var]]
      present_values <- names(var_symbols)
      
      if (var_idx > 1) {
        legend_labels <- c(legend_labels, "")
        legend_pch <- c(legend_pch, NA)
        legend_col <- c(legend_col, NA)
        legend_cex <- c(legend_cex, NA)
      }
      
      legend_labels <- c(legend_labels, paste0("--- ", legend_var, " ---"))
      legend_pch <- c(legend_pch, NA)
      legend_col <- c(legend_col, NA)
      legend_cex <- c(legend_cex, NA)
      
      for (val in present_values) {
        legend_labels <- c(legend_labels, val)
        legend_pch <- c(legend_pch, var_symbols[[val]]$pch)
        legend_col <- c(legend_col, var_symbols[[val]]$col)
        legend_cex <- c(legend_cex, var_symbols[[val]]$cex)
      }
    }
    
    legend(second_legend_position,
           legend = legend_labels,
           pch = legend_pch,
           col = legend_col,
           pt.cex = legend_cex,
           title = second_legend_title,
           cex = second_legend_cex,
           bty = "n",
           inset = second_legend_inset)
    
  } else {
    # 单个变量模式 - 确保符号在bar外部且不被遮挡
    unique_second_legend <- unique(data[[second_legend_vars[1]]])
    n_unique <- length(unique_second_legend)
    
    # 符号生成逻辑
    if (is.null(second_legend_symbols)) {
      second_legend_symbols <- list()
      pch_options <- c(16, 17, 15, 18, 8, 3, 4, 6, 2, 1)
      color_options <- c("darkgreen", "blue", "orange", "red", "purple", 
                         "brown", "pink", "darkcyan", "magenta", "darkblue")
      
      for (i in 1:n_unique) {
        val <- unique_second_legend[i]
        pch_idx <- ((i - 1) %% length(pch_options)) + 1
        color_idx <- ((i - 1) %% length(color_options)) + 1
        
        second_legend_symbols[[val]] <- list(
          pch = pch_options[pch_idx],
          col = color_options[color_idx],
          cex = 1.5
        )
      }
    }
    
    # 先绘制bar，然后在上面添加符号
    # 已经绘制了bar，现在添加符号（会在bar上层）
    
    for (i in 1:nrow(data)) {
      second_val <- as.character(data[[second_legend_vars[1]]][i])
      symbol_params <- second_legend_symbols[[second_val]]
      
      if (is.null(symbol_params)) {
        symbol_params <- list(pch = 16, col = "black", cex = 1.5)
      }
      
      bar_value <- bar_values[i]
      y_pos <- NULL
      
      if (auto_position_symbol) {
        # 确保符号在bar外部
        if (bar_value >= 0) {
          # 正值：符号放在bar下方（y轴负方向）
          # 确保符号在bar的下方，不在bar内部
          y_pos <- bar_value - symbol_y_offset
          # 如果符号位置仍然在bar内部（正值区域），强制放到0以下
          if (y_pos > 0 && y_pos < bar_value) {
            y_pos <- -symbol_y_offset
          }
          # 如果bar值很小（比如小于偏移量），直接放到0以下
          if (bar_value < symbol_y_offset) {
            y_pos <- -5
          }
        } else {
          # 负值：符号放在bar上方（y轴正方向）
          y_pos <- bar_value + symbol_y_offset
          # 如果符号位置仍然在bar内部（负值区域），强制放到0以上
          if (y_pos < 0 && y_pos > bar_value) {
            y_pos <- symbol_y_offset
          }
          # 如果bar值很小（接近0），直接放到0以上
          if (abs(bar_value) < symbol_y_offset) {
            y_pos <- 5
          }
        }
        
        # 边界保护
        y_pos <- max(y_pos, adjusted_y_limits[1] + 2)
        y_pos <- min(y_pos, adjusted_y_limits[2] - 2)
        
      } else {
        # 固定位置模式
        if (symbol_display_mode == "bottom") {
          y_pos <- adjusted_y_limits[1] + symbol_y_offset
        } else {
          y_pos <- adjusted_y_limits[2] - symbol_y_offset
        }
      }
      
      # 添加符号（在bar上层）
      points(x = MCC[i], 
             y = y_pos,
             pch = symbol_params$pch,
             col = symbol_params$col,
             cex = symbol_params$cex,
             lwd = 2)
      
      # 可选：添加调试信息
      # cat(sprintf("Bar %d: value=%.1f, symbol_y=%.1f, bar_value>=0=%s\n", 
      #             i, bar_value, y_pos, bar_value >= 0))
    }
    
    # 添加单个图例
    present_values <- unique_second_legend
    legend_pch <- sapply(present_values, function(x) second_legend_symbols[[x]]$pch)
    legend_col <- sapply(present_values, function(x) second_legend_symbols[[x]]$col)
    legend_cex <- sapply(present_values, function(x) second_legend_symbols[[x]]$cex)
    
    legend(second_legend_position, 
           legend = present_values,
           pch = legend_pch,
           col = legend_col,
           pt.cex = legend_cex,
           title = second_legend_title,
           cex = second_legend_cex,
           bty = "n",
           inset = second_legend_inset)
  }
  
  # 返回结果
  invisible(list(coords = MCC, 
                 cohorts = unique_cohorts, 
                 legend_text = legend_text,
                 second_legend_data = all_legend_data,
                 multi_legend = multi_legend))
}

# 使用示例
if(FALSE){
  # 创建测试数据
  test_data <- data.frame(
    vars = c(80, 60, 30, 10, 5, -5, -10, -30, -60, -80),
    cohort = rep(c("Cohort A", "Cohort B"), each = 5),
    response = c("CR", "PR", "SD", "PD", "CR", "PR", "SD", "PD", "CR", "PR")
  )
  
  # 自动位置模式 - 符号在bar外部（正值在下方，负值在上方）
  create_waterfall_plot(test_data,
                        vars = "vars",
                        cohort_col = "cohort",
                        second_legend = "response",
                        second_legend_title = "Best Response",
                        auto_position_symbol = TRUE,
                        symbol_y_offset = 12,  # 偏移量
                        y_limits = c(-100, 100))
  
  # 如果想增加距离，可以增加偏移量
  create_waterfall_plot(test_data,
                        vars = "vars",
                        cohort_col = "cohort",
                        second_legend = "response",
                        second_legend_title = "Best Response",
                        auto_position_symbol = TRUE,
                        symbol_y_offset = 15,  # 更大的偏移量
                        y_limits = c(-100, 100))
}
