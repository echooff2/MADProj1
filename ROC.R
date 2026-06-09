draw_roc_plot <- function(real_classes, probabilities, name) {
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package 'pROC' is required.")
  }

  roc_obj <- pROC::roc(real_classes, probabilities)

  roc_df <- data.frame(
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities
  )

  auc_value <- round(as.numeric(pROC::auc(roc_obj)), 3)
  paste("AUC: ", auc_value)

  p <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
    geom_line(color = "#2166AC", linewidth = 1.5) +
    geom_abline(
      intercept = 0, slope = 1,
      linetype = "dashed", color = "gray50"
    ) +
    ggtitle(paste("Krzywa ROC: ", name, " (AUC = ", auc_value, ")", sep = "")) +
    labs(
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    coord_equal() +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 18)
    )

  ggsave(
    paste0("Plots/roc_curves/roc_", name, ".png"),
    plot = p,
    width = 8,
    height = 6,
    dpi = 80,
    create.dir = TRUE
  )

  return(p)
}

draw_averaged_roc_plot <- function(roc_runs, name) {
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package 'pROC' is required.")
  }

  fpr_grid <- seq(0, 1, by = 0.01)
  tpr_matrix <- matrix(NA_real_, nrow = length(roc_runs), ncol = length(fpr_grid))
  auc_values <- numeric(length(roc_runs))

  for (i in seq_along(roc_runs)) {
    roc_obj <- pROC::roc(roc_runs[[i]]$real_classes, roc_runs[[i]]$probabilities, quiet = TRUE)
    auc_values[i] <- as.numeric(pROC::auc(roc_obj))
    tpr_matrix[i, ] <- stats::approx(
      x = 1 - roc_obj$specificities,
      y = roc_obj$sensitivities,
      xout = fpr_grid,
      yleft = 0,
      yright = 1,
      rule = 2
    )$y
  }

  roc_df <- data.frame(
    FPR = fpr_grid,
    TPR = colMeans(tpr_matrix, na.rm = TRUE)
  )

  auc_value <- round(mean(auc_values), 3)

  p <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
    geom_line(color = "#2166AC", linewidth = 1.5) +
    geom_abline(
      intercept = 0, slope = 1,
      linetype = "dashed", color = "gray50"
    ) +
    ggtitle(paste("Krzywa ROC: ", name, " (śr. AUC = ", auc_value, ")", sep = "")) +
    labs(
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    coord_equal() +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 18)
    )

  ggsave(
    paste0("Plots/roc_curves/roc_", name, ".png"),
    plot = p,
    width = 8,
    height = 6,
    dpi = 80,
    create.dir = TRUE
  )

  return(p)
}
