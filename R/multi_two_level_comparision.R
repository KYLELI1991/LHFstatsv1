#' Function Title
#'
#' Function Description
#' @export
perform_pairwise_tests_simple <- function(df_expr, df_meta, 
                                          test.type = "t.test", 
                                          adjust.p = FALSE, 
                                          method = "BH",
                                          var.equal = TRUE) {
  
  # 参数验证
  if(!test.type %in% c("t.test", "wilcox", "wilcox.test")) {
    stop("test.type must be either 't.test' or 'wilcox'/'wilcox.test'")
  }
  
  if(!method %in% c("BH", "bonferroni", "holm", "hochberg", "hommel", "BY", "fdr", "none")) {
    stop("Invalid p-value adjustment method")
  }
  
  # 将表达数据转换为长格式
  expr_long <- df_expr %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    pivot_longer(
      cols = -variable,
      names_to = "sample",
      values_to = "expression"
    )
  
  # 合并分组信息
  expr_with_group <- expr_long %>%
    left_join(df_meta, by = "sample")
  
  # 获取所有可能的组对
  groups <- unique(expr_with_group$group)
  group_combinations <- combn(groups, 2, simplify = FALSE)
  
  # 创建分组对比的数据框
  comparisons <- data.frame(
    group1 = sapply(group_combinations, function(x) x[1]),
    group2 = sapply(group_combinations, function(x) x[2])
  )
  
  # 对每个变量和每组对比进行计算
  results <- expr_with_group %>%
    crossing(comparisons) %>%
    filter(group %in% c(group1, group2)) %>%
    group_by(variable, group1, group2) %>%
    summarise(
      n_group1 = sum(!is.na(expression[group == group1])),
      n_group2 = sum(!is.na(expression[group == group2])),
      mean_group1 = mean(expression[group == group1], na.rm = TRUE),
      mean_group2 = mean(expression[group == group2], na.rm = TRUE),
      sd_group1 = sd(expression[group == group1], na.rm = TRUE),
      sd_group2 = sd(expression[group == group2], na.rm = TRUE),
      p_value = if(test.type == "t.test") {
        tryCatch({
          t.test(expression[group == group1], 
                 expression[group == group2], 
                 var.equal = var.equal)$p.value
        }, error = function(e) NA)
      } else {
        tryCatch({
          wilcox.test(expression[group == group1], 
                      expression[group == group2],
                      exact = FALSE)$p.value
        }, error = function(e) NA)
      },
      .groups = "drop"
    )
  
  # 添加统计方法信息
  if(test.type == "t.test") {
    results$test_method <- ifelse(var.equal, 
                                  "Student's t-test (equal variance)", 
                                  "Welch's t-test (unequal variance)")
  } else {
    results$test_method <- "Mann-Whitney U test"
  }
  
  # P值校正
  if(adjust.p) {
    results <- results %>%
      group_by(variable) %>%
      mutate(
        p_adj = p.adjust(p_value, method = method),
        significant = p_adj < 0.05,
        significance_level = case_when(
          p_adj < 0.001 ~ "***",
          p_adj < 0.01 ~ "**",
          p_adj < 0.05 ~ "*",
          TRUE ~ "ns"
        ),
        padj_method = method
      ) %>%
      ungroup()
  } else {
    results <- results %>%
      mutate(
        significant = p_value < 0.05,
        significance_level = case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          TRUE ~ "ns"
        ),
        padj_method = "none",
        p_adj = p_value
      )
  }
  
  # 添加其他信息
  results <- results %>%
    mutate(
      mean_diff = mean_group1 - mean_group2,
      comparison = paste0(group1, "_vs_", group2),
      fold_change = ifelse(mean_group2 != 0, 
                           mean_group1 / mean_group2, 
                           NA),
      log2_fold_change = log2(ifelse(mean_group2 > 0 & mean_group1 > 0, 
                                     fold_change, NA))
    ) %>%
    select(
      variable, comparison, group1, group2,
      n_group1, n_group2,
      mean_group1, mean_group2, sd_group1, sd_group2,
      mean_diff, fold_change, log2_fold_change,
      test_method,
      p_value, p_adj, significant, significance_level, padj_method,
      everything()
    )
  
  # 添加函数调用参数信息作为属性
  attr(results, "parameters") <- list(
    test_type = test.type,
    var_equal = ifelse(test.type == "t.test", var.equal, NA),
    adjust_p = adjust.p,
    p_adjust_method = ifelse(adjust.p, method, "none")
  )
  
  return(results)
}



#' Function Title
#'
#' Function Description
#' @export




visualize_top_differential_features <- function(results_df, df_expr, df_meta = NULL, 
                                                top_n = 10, 
                                                plot_type = c("heatmap", "volcano", "boxplot"),
                                                specific_comparison = NULL,
                                                output_file = NULL) {
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(ggrepel)
  library(patchwork)
  
  # 确定要可视化的比较
  if(is.null(specific_comparison)) {
    # 找出总体最显著的比较
    top_comparison <- results_df %>%
      group_by(comparison) %>%
      summarise(mean_logp = mean(-log10(p_adj), na.rm = TRUE)) %>%
      arrange(desc(mean_logp)) %>%
      slice_head(n = 1) %>%
      pull(comparison)
    comparison_to_plot <- top_comparison
  } else {
    comparison_to_plot <- specific_comparison
  }
  
  # 提取特定比较的数据
  comp_data <- results_df %>%
    filter(comparison == comparison_to_plot) %>%
    arrange(p_adj) %>%
    mutate(
      rank = row_number(),
      significant = ifelse(p_adj < 0.05, "Significant", "Not significant"),
      direction = ifelse(mean_diff > 0, "Up", "Down")
    )
  
  # 获取top N的特征
  top_features <- comp_data %>%
    slice_head(n = top_n) %>%
    pull(variable)
  
  plots <- list()
  
  # 1. 火山图
  if("volcano" %in% plot_type) {
    p_volcano <- ggplot(comp_data, 
                        aes(x = log2_fold_change, 
                            y = -log10(p_adj))) +
      geom_point(aes(color = significant, 
                     alpha = significant,
                     size = significant)) +
      scale_color_manual(values = c("Significant" = "red", 
                                    "Not significant" = "grey60")) +
      scale_alpha_manual(values = c("Significant" = 0.8, 
                                    "Not significant" = 0.3)) +
      scale_size_manual(values = c("Significant" = 2, 
                                   "Not significant" = 1)) +
      geom_vline(xintercept = c(-1, 1), 
                 linetype = "dashed", alpha = 0.3) +
      geom_hline(yintercept = -log10(0.05), 
                 linetype = "dashed", alpha = 0.3) +
      labs(
        title = paste("Volcano Plot:", comparison_to_plot),
        x = "log2(Fold Change)",
        y = "-log10(Adjusted p-value)",
        color = "Significance"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank()
      )
    
    # 标记top N特征
    if(!is.null(top_features)) {
      top_label_data <- comp_data %>%
        filter(variable %in% top_features)
      
      p_volcano <- p_volcano +
        geom_point(data = top_label_data, 
                   aes(x = log2_fold_change, y = -log10(p_adj)),
                   color = "blue", size = 3, shape = 1) +
        geom_text_repel(
          data = top_label_data,
          aes(label = variable),
          size = 3,
          max.overlaps = 20,
          box.padding = 0.5,
          segment.color = "grey50"
        )
    }
    
    plots$volcano <- p_volcano
  }
  
  # 2. 箱线图（需要原始数据）
  if("boxplot" %in% plot_type && !is.null(df_expr) && !is.null(df_meta)) {
    
    # 准备数据
    plot_expr_data <- df_expr[top_features, , drop = FALSE] %>%
      as.data.frame() %>%
      rownames_to_column("variable") %>%
      pivot_longer(cols = -variable, 
                   names_to = "sample", 
                   values_to = "expression") %>%
      left_join(df_meta, by = "sample") %>%
      mutate(variable = factor(variable, levels = top_features))
    
    # 提取组信息
    groups <- comp_data %>% 
      select(group1, group2) %>% 
      distinct()
    group_levels <- c(groups$group1[1], groups$group2[1])
    
    plot_expr_data <- plot_expr_data %>%
      filter(group %in% group_levels) %>%
      mutate(group = factor(group, levels = group_levels))
    
    # 创建箱线图
    p_boxplot <- ggplot(plot_expr_data, 
                        aes(x = group, y = expression, fill = group)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      geom_jitter(width = 0.2, size = 0.5, alpha = 0.5) +
      facet_wrap(~variable, scales = "free_y", ncol = 5) +
      scale_fill_brewer(palette = "Set1") +
      labs(
        title = paste("Expression of Top", top_n, "Differential Features"),
        subtitle = paste("Comparison:", comparison_to_plot),
        x = "Group",
        y = "Expression Level",
        fill = "Group"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        strip.text = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(1, "lines")
      )
    
    plots$boxplot <- p_boxplot
  }
  
  # 3. 热图（需要原始数据）
  if("heatmap" %in% plot_type && !is.null(df_expr) && !is.null(df_meta)) {
    
    # 准备数据
    heatmap_data <- df_expr[top_features, ]
    
    # 标准化（Z-score）
    heatmap_data_scaled <- t(scale(t(heatmap_data)))
    
    # 准备注释
    group_anno <- df_meta %>%
      arrange(group) %>%
      column_to_rownames("sample")
    
    # 使用pheatmap或ComplexHeatmap
    tryCatch({
      library(pheatmap)
      
      p_heatmap <- pheatmap(
        heatmap_data_scaled,
        annotation_col = group_anno,
        show_rownames = TRUE,
        show_colnames = FALSE,
        main = paste("Top", top_n, "Differential Features"),
        fontsize_row = 8,
        color = colorRampPalette(c("blue", "white", "red"))(100),
        silent = TRUE
      )
      
      plots$heatmap <- p_heatmap
    }, error = function(e) {
      message("pheatmap not available, using ggplot2 instead")
      
      # 备选方案：使用ggplot2
      heatmap_long <- heatmap_data_scaled %>%
        as.data.frame() %>%
        rownames_to_column("variable") %>%
        pivot_longer(cols = -variable, 
                     names_to = "sample", 
                     values_to = "expression") %>%
        left_join(df_meta, by = "sample") %>%
        mutate(variable = factor(variable, levels = rev(top_features)))
      
      p_heatmap_alt <- ggplot(heatmap_long, 
                              aes(x = sample, y = variable, fill = expression)) +
        geom_tile() +
        scale_fill_gradient2(
          low = "blue", mid = "white", high = "red",
          midpoint = 0,
          name = "Z-score"
        ) +
        labs(
          title = paste("Top", top_n, "Differential Features"),
          x = "Sample",
          y = "Feature"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "right"
        )
      
      plots$heatmap <- p_heatmap_alt
    })
  }
  
  # 4. 条形图展示fold change
  if("bar" %in% plot_type) {
    bar_data <- comp_data %>%
      filter(variable %in% top_features) %>%
      mutate(
        variable = factor(variable, levels = rev(top_features)),
        color_group = ifelse(log2_fold_change > 0, "Up-regulated", "Down-regulated")
      )
    
    p_bar <- ggplot(bar_data, 
                    aes(x = log2_fold_change, 
                        y = variable, 
                        fill = color_group)) +
      geom_bar(stat = "identity", width = 0.7) +
      scale_fill_manual(values = c("Up-regulated" = "red", 
                                   "Down-regulated" = "blue")) +
      geom_vline(xintercept = 0, linetype = "solid") +
      labs(
        title = paste("Top", top_n, "Differential Features"),
        subtitle = paste("log2 Fold Change:", comparison_to_plot),
        x = "log2(Fold Change)",
        y = "Feature",
        fill = "Regulation"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.y = element_blank()
      )
    
    plots$bar <- p_bar
  }
  
  # 保存图形
  if(!is.null(output_file)) {
    # 保存所有图形
    for(plot_name in names(plots)) {
      if(!is.null(plots[[plot_name]])) {
        ggsave(
          paste0(output_file, "_", specific_comparison,"_", plot_name, ".png"),
          plots[[plot_name]],
          width = 12,
          height = 8,
          dpi = 300
        )
      }
    }
  }
  
  # 打印图形
  for(plot_name in names(plots)) {
    if(!is.null(plots[[plot_name]])) {
      print(plots[[plot_name]])
    }
  }
  
  return(plots)
}
