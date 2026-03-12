#' Function Title
#'
#' Function Description
#' @export
create_waterfall_plot <- function(data,
                                  vars,
                                  cohort_col = "cohort",  # 指定cohort列名
                                  second_legend = "response",  # 指定第二个图例的列名
                                  cohort_colors = c("cadetblue", "steelblue", "#9E58CC", "red"),
                                  second_legend_symbols = NULL,  # 改为NULL，如果不提供则自动生成
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
                                  second_legend_title = "Response",
                                  second_legend_position = "bottomleft",
                                  second_legend_inset = c(0.02, -0.025),
                                  second_legend_cex = 0.9,
                                  legend_cohorts_cex = 0.9,
                                  legend_cohorts_position = "topright",
                                  legend_cohorts_inset = c(0.02, 0.02),
                                  cex_main = 1.2,
                                  cex_lab = 1.2,
                                  cex_axis = 1.5,
                                  symbol_y_offset = 3,
                                  add_group_separator = TRUE,  # 新增：是否添加分组分隔线
                                  separator_col = "gray50",  # 新增：分隔线颜色
                                  separator_lwd = 2,  # 新增：分隔线宽度
                                  separator_lty = 2) {  # 新增：分隔线类型（虚线）
  
  # 输入验证
  required_cols <- c(cohort_col, vars, second_legend)
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    stop("数据框中缺少以下必要的列: ", paste(missing_cols, collapse = ", "))
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
  
  # 准备cohort图例文本 - 修改这里，不再添加"cohort1:"前缀
  if (is.null(legend_cohorts)) {
    if (!is.null(legend_cohorts_labels)) {
      if (length(legend_cohorts_labels) >= n_cohorts) {
        legend_text <- legend_cohorts_labels[1:n_cohorts]
      } else {
        warning("自定义标签数量少于cohort数量，将使用默认标签")
        legend_text <- unique_cohorts  # 直接使用unique_cohorts值
      }
    } else {
      legend_text <- unique_cohorts  # 直接使用unique_cohorts值
    }
  } else {
    if (length(legend_cohorts) >= n_cohorts) {
      legend_text <- legend_cohorts[1:n_cohorts]
    } else {
      warning("提供的图例文本数量少于cohort数量，将使用默认标签")
      legend_text <- unique_cohorts  # 直接使用unique_cohorts值
    }
  }
  
  # 准备图例颜色
  legend_colors <- cohort_colors[1:n_cohorts]
  
  # 创建条形图
  MCC <- barplot(data[,vars], 
                 col = col, 
                 border = col, 
                 space = bar_space, 
                 ylim = y_limits,
                 main = main_title, 
                 ylab = ylab_label,
                 cex.main = cex_main,
                 cex.lab = cex_lab,
                 cex.axis = cex_axis)
  
  # 添加分组分隔线（虚线）
  if (add_group_separator) {
    # 找到每个cohort组的起始和结束位置
    current_cohort <- data[[cohort_col]][1]
    start_idx <- 1
    
    for (i in 1:nrow(data)) {
      # 当cohort发生变化时，在组间添加分隔线
      if (data[[cohort_col]][i] != current_cohort) {
        # 计算分隔线位置：前一个组的最后一个柱子和当前组第一个柱子的中间
        if (i > 1) {
          # 计算两个柱子之间的位置
          sep_pos <- (MCC[i-1] + MCC[i]) / 2
          # 添加垂直虚线
          abline(v = sep_pos, col = separator_col, lwd = separator_lwd, lty = separator_lty)
        }
        current_cohort <- data[[cohort_col]][i]
      }
    }
  }
  
  # 手动添加cohort图例
  legend(legend_cohorts_position,
         legend = legend_text,
         fill = legend_colors,
         border = NA,
         title = cohort_col,
         cex = legend_cohorts_cex,
         inset = legend_cohorts_inset,
         bty = "n")
  
  # 添加日期（如果需要）
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
  
  # 获取数据中实际的second_legend值
  unique_second_legend <- unique(data[[second_legend]])
  n_unique <- length(unique_second_legend)
  
  # 如果没有提供second_legend_symbols，自动生成
  if (is.null(second_legend_symbols)) {
    second_legend_symbols <- list()
    
    # 预定义的符号和颜色集
    pch_options <- c(16, 17, 15, 18, 8, 3, 4, 6, 2, 1)  # 不同形状
    color_options <- c("darkgreen", "blue", "orange", "red", "purple", 
                       "brown", "pink", "darkcyan", "magenta", "darkblue")
    
    for (i in 1:n_unique) {
      val <- unique_second_legend[i]
      # 循环使用符号和颜色
      pch_idx <- ((i - 1) %% length(pch_options)) + 1
      color_idx <- ((i - 1) %% length(color_options)) + 1
      
      second_legend_symbols[[val]] <- list(
        pch = pch_options[pch_idx],
        col = color_options[color_idx],
        cex = 1.5
      )
    }
    
    message("自动为 ", n_unique, " 个 unique 值生成了符号和颜色")
    
  } else {
    # 如果提供了部分符号，检查是否完整
    missing_symbols <- unique_second_legend[!unique_second_legend %in% names(second_legend_symbols)]
    
    if (length(missing_symbols) > 0) {
      warning("second_legend_symbols中缺少以下类型: ", 
              paste(missing_symbols, collapse = ", "), 
              "，将自动为这些值生成符号")
      
      # 为缺失的值生成符号
      pch_options <- c(16, 17, 15, 18, 8, 3, 4, 6, 2, 1)
      color_options <- c("darkgreen", "blue", "orange", "red", "purple", 
                         "brown", "pink", "darkcyan", "magenta", "darkblue")
      
      start_idx <- length(second_legend_symbols) + 1
      
      for (i in 1:length(missing_symbols)) {
        val <- missing_symbols[i]
        pch_idx <- ((start_idx + i - 2) %% length(pch_options)) + 1
        color_idx <- ((start_idx + i - 2) %% length(color_options)) + 1
        
        second_legend_symbols[[val]] <- list(
          pch = pch_options[pch_idx],
          col = color_options[color_idx],
          cex = 1.5
        )
      }
    }
  }
  
  # 在每个柱状图上添加图标注释
  for (i in 1:nrow(data)) {
    second_val <- data[[second_legend]][i]
    bar_value <- data[,vars][i]
    
    # 获取对应的图标参数
    symbol_params <- second_legend_symbols[[second_val]]
    
    # 根据值的正负确定图标位置
    if (bar_value >= 0) {
      points(x = MCC[i], 
             y = -symbol_y_offset,
             pch = symbol_params$pch,
             col = symbol_params$col,
             cex = symbol_params$cex)
    } else {
      points(x = MCC[i], 
             y = symbol_y_offset,
             pch = symbol_params$pch,
             col = symbol_params$col,
             cex = symbol_params$cex)
    }
  }
  
  # 准备第二个图例
  present_values <- unique_second_legend
  
  legend_pch <- sapply(present_values, function(x) second_legend_symbols[[x]]$pch)
  legend_col <- sapply(present_values, function(x) second_legend_symbols[[x]]$col)
  legend_cex <- sapply(present_values, function(x) second_legend_symbols[[x]]$cex)
  
  # 添加第二个图例
  legend(second_legend_position, 
         legend = present_values,
         pch = legend_pch,
         col = legend_col,
         pt.cex = legend_cex,
         title = second_legend_title,
         cex = second_legend_cex,
         bty = "n",
         inset = second_legend_inset)
  
  # 返回结果
  invisible(list(coords = MCC, 
                 cohorts = unique_cohorts, 
                 legend_text = legend_text,
                 second_legend_values = present_values,
                 second_legend_symbols = second_legend_symbols))
}



if(F){
  create_waterfall_plot(data_plot_wf,vars = "vars")
  # 自定义 cohort and pathology
  unique(data_plot_wf$cohort)
  create_waterfall_plot(data_plot_wf,
                        vars = "vars",
                        main_title = "Waterfall plot",
                        ylab_label = "Change from baseline (%)",
                        legend_cohorts_position = "topright",
                        legend_cohorts_inset = c(-0.1, 0.02),
                        cohort_col = "cohort",
                        add_group_separator = TRUE,
                        separator_col = "gray50",
                        separator_lwd = 2,
                        separator_lty = 1, # ly type 1 for solid
                        legend_cohorts = c("Cohort1: SHR-A1811+Adebrelimab", 
                                           "Cohort2: SHR-A1811+Adebrelimab", 
                                           "Cohort3: SHR-A1811"),
                        cohort_colors = c("#4B32C9", "#32B0C9", "#B0C932", "#C94B32"),
                        second_legend_position = "bottomleft",
                        second_legend = "pathology",
                        second_legend_title = "Pathology variant",
                        second_legend_symbols = list(
                          "uc_squamous" = list(pch = 16, col = "red", cex = 1.5),
                          "uc_nos" = list(pch = 17, col = "blue", cex = 1.5),
                          "uc_micropaplillary" = list(pch = 15, col = "green", cex = 1.5)
                        ),
                        second_legend_inset = c(0.02, 0.02))
  
  
  graph2ppt(file = paste0(project,"_waterfall_",date,".ppt"),
            width = 12,
            height = 8)
  graph2jpg(file = paste0(project,"_waterfall_",date,".jpg"),
            width = 12,
            height = 8)
  graph2svg(file = paste0(project,"_waterfall_",date,".svg"),
            width = 12,
            height = 8)
  
  # 自定义 response and pathology
  data_plot_wf <- data_plot_wf[order(data_plot_wf$vars,decreasing = T),]
  data_plot_wf <- data_plot_wf[order(data_plot_wf$pathology,decreasing = F),]
  unique(data_plot_wf$pathology)
  create_waterfall_plot(data_plot_wf,
                        vars = "vars",
                        main_title = "Waterfall plot",
                        ylab_label = "Change from baseline (%)",
                        add_group_separator = F,
                        #legend_cohorts_position = "topright",
                        cohort_col = "pathology",
                        cohort_colors = c("#4B32C9", "#32B0C9", "#B0C932", "#C94B32"),
                        second_legend_position = "bottomleft",
                        second_legend = "response",
                        second_legend_title = "Best of response",
                        second_legend_symbols = list(
                          "CR" = list(pch = 16, col = "darkgreen", cex = 1.5),
                          "PR" = list(pch = 17, col = "blue", cex = 1.5),
                          "SD" = list(pch = 15, col = "orange", cex = 1.5),
                          "PD" = list(pch = 18, col = "red", cex = 1.8)
                        ),
                        second_legend_inset = c(0.02, 0.02))
  
  
  graph2jpg(file = paste0(project,"_waterfall_patho2response_",date,".jpg"),
            width = 12,
            height = 8)
  
}
