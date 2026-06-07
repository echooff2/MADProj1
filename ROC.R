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
        geom_abline(intercept = 0, slope = 1,
                    linetype = "dashed", color = "gray50") +
        ggtitle(paste("Krzywa ROC: ", name, " (AUC = ", auc_value, ")", sep="")) +
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
        width = 7.5,
        height = 6,
        dpi = 80,
        create.dir = TRUE
    )
    
    return(p)
}
