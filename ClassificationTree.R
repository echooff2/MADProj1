draw_tree_plot <- function(tree_res, name, file_name) {
    png(paste("Plots/tree_plots/", file_name, ".png", sep=""), width = 800, height = 600, res = 80)
    
    par(
        bg = "white", 
        mar = c(3, 3, 5, 3),
        cex.main = 1.6,
        font.main = 2
    )
    plot(tree_res, type = "uniform", col = "transparent") 
    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col = "#EBEBEB", border = NA)
    grid(nx = NULL, ny = NULL, col = "white", lty = 1, lwd = 1.5)
    par(new = TRUE)
    plot(tree_res, type = "uniform", col = "#2166AC", lwd = 4)
    text(
        tree_res,
        cex = 0.95,
        col = "black",
        font = 2,
        pretty = 0,
        bg = "white"
    )
    par(mar = c(4, 4, 6, 4))
    title(main = name, cex.main = 1.8)
    
    dev.off()
}

draw_tree_cv_plot <- function(tree_res.cv, tree_res) {
    
    df <- data.frame(
        size = tree_res.cv$size,
        deviance = tree_res.cv$dev
    )
    
    p <- ggplot(df, aes(x = size, y = deviance)) +
        geom_line(color = "#2166AC", linewidth = 1) +
        geom_point(color = "#2166AC", size = 3) +
        ggtitle("Błąd CV dla różnych rozmiarów drzewa") +
        labs(
            x = "Tree size",
            y = "Deviance"
        ) +
        theme(
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 18)
        )
    
    print(tree_res)
    
    ggsave(
        "Plots/tree_plots/tree_cv.png",
        plot = p,
        width = 8.5,
        height = 6,
        dpi = 80
    )
}

do_classification_tree <- function(draw_plots = F) {
    set.seed(123)
    
    if (!exists("do_preliminary_analisys")) {
      source("PreliminaryAnalisys.R")
    }
    
    if (!exists("draw_confusion_matrix")) {
      source("TableWisualization.R")
    }
    
    if (!exists("draw_roc_plot")) {
        source("ROC.R")
    }
    
    df <- do_preliminary_analisys()
    df$blueWins <- as.factor(df$blueWins)
    
    library(tree)
    tree(df)
    
    split <- sample(seq_len(nrow(df)), round(0.7 * nrow(df)))
    train <- df[split, ]
    test <- df[-split, ]
    summary(train)
    tree_res <- tree(blueWins ~ ., train)
    
    tree_res2 <- tree(blueWins ~ ., train)
    tree_res.cv <- cv.tree(tree_res2, FUN = prune.tree)
    
    for (i in 2:5) {
      tree_res.cv$dev <- tree_res.cv$dev + cv.tree(tree_res2, FUN = prune.tree)$dev
    }
    tree_res.cv$dev <- tree_res.cv$dev / 5
    if (draw_plots) {
        draw_tree_cv_plot(tree_res.cv, tree_res)
        draw_tree_plot(tree_res, "Drzewo klasyfikacyjne", "classification_tree")
    }
    
    summary(tree_res)
    
    predict_probs <- predict(tree_res, newdata = test)[, 2]
    predict_test <- predict(tree_res, newdata = test, type = "class")
    confusion_matrix <- table(Predicted = predict_test, Actual = test$blueWins)
    confusion_matrix
    summary(predict_test)
    
    library(caret)
    confusionMatrix(predict_test, test$blueWins)
    
    small_tree <- prune.tree(tree_res2, best = 2)
    if (draw_plots) {
        draw_tree_plot(small_tree, "Przycięte drzewo klasyfikacyjne", "trimmed_classification_tree")
    }
    
    # 2. Robimy prognozy dla obu modeli na zbiorze testowym
    pred_small <- predict(small_tree, newdata = test, type = "class")
    pred_large <- predict(tree_res2, newdata = test, type = "class")
    
    # 3. Liczymy Accuracy dla obu
    acc_small <- mean(pred_small == test$blueWins)
    acc_large <- mean(pred_large == test$blueWins)
    acc_normal <- mean(predict_test == test$blueWins)
    
    # 4. Wyświetlamy wynik starcia
    cat(paste0(
      "Accuracy przyciętego tree_res2 drzewa: ", round(acc_small * 100, 2), "%\n",
      "Accuracy tree_res2 drzewa: ", round(acc_large * 100, 2), "%\n",
      "Accuracy tree_res drzewa: ", round(acc_normal * 100, 2), "%\n"
    ))
    
    tn <- confusion_matrix["0", "0"]
    tp <- confusion_matrix["1", "1"]
    fp <- confusion_matrix["0", "1"]
    fn <- confusion_matrix["1", "0"]
    
    if (draw_plots) {
        draw_confusion_matrix(tp, fn, tn, fp, "classification_tree_confusion_matrix")
        draw_roc_plot(test$blueWins, predict_probs, "Drzewo klasyfikacyjne")
    }
    
    output <- as.data.frame(predict_probs)
    output$test_class_tree<-test$blueWins
    
    return(output) # returns data for mixed model
}

if (sys.nframe() == 0L) {
    do_classification_tree(draw_plots = T)
}