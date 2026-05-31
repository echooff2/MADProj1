draw_piechart <- function(df, feature, name){
    counts <- as.data.frame(table(feature))
    colnames(counts) <- c("Wartość", "Count")
    counts$Percent <- round(100 * counts$Count / sum(counts$Count), 1)
    p <- ggplot(counts, aes(x = "", y = Count, fill = Wartość)) +
        ggtitle(paste("Piechart: ", name)) +
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        geom_text(
            aes(
                label = paste0(
                    #Wartość,
                    "\nN=", Count,
                    "\n", Percent, "%"
                )
            ),
            position = position_stack(vjust = 0.5),
            size = 6
        ) +
        scale_fill_manual(values = c(
            "0" = "#cdcc97",
            "1" = "#9798cd"
        )) + 
        theme(
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(size = 20, face = "bold", hjust=0.5), 
            text = element_text(size = 18)
        )
    ggsave(paste0("Plots/piecharts/pie_", name, ".png"), width=8.5, height=6, dpi=80)
    
}

draw_barplot <- function(df, feature, name){
    p <- ggplot(data.frame(feature), aes(x=feature)) +
        geom_bar(colour = 1, fill = "#9798cd") +
        ggtitle(paste("Barplot: ", name)) +
        labs(
            x = paste("Wartość zmiennej", name),
            y = "Częstość"
        ) +
        theme(
            plot.title = element_text(size = 20, face = "bold", hjust=0.5), 
            axis.title = element_text(size = 18),               
            axis.text = element_text(size = 18)    
        )
    ggsave(paste0("Plots/histograms_and_barplots/bar_", name, ".png"), width=8.5, height=6, dpi=80)
}


draw_historgram <- function(df, feature, name){
    bin_count <- if (grepl("KA", name, fixed=TRUE)) (max(feature) - min(feature))%/%2 + 1 else 30
    p <- ggplot(data.frame(feature), aes(x=feature)) +
        geom_histogram(aes(y = ..density..), colour = 1, fill = "#9798cd", bins=bin_count) +
        geom_density(color = "#001287", lwd = 1.5) +
        ggtitle(paste("Histogram i wykres gęstości:", name)) +
        labs(
           x = paste("Wartość zmiennej", name),
           y = "Gęstość prawdopodobieństwa"
        ) +
        theme(
            plot.title = element_text(size = 20, face = "bold", hjust=0.5), 
            axis.title = element_text(size = 18),               
            axis.text = element_text(size = 16)    
        )
    ggsave(paste0("Plots/histograms_and_barplots/hist_", name, ".png"), width=8.6, height=6, dpi=80)
}

draw_boxplot <- function(df, feature, name){
    p <- ggplot(data.frame(feature), aes(x=feature)) +
        geom_boxplot(colour = 1, fill = "#cdcc97") +
        ggtitle(paste("Boxplot:", name)) +
        labs(
            x = paste("Wartość zmiennej", name),
            y = name
        ) +
        theme(
            plot.title = element_text(size = 20, face = "bold", hjust=0.5), 
            axis.title = element_text(size = 18),               
            axis.text = element_text(size = 18)    
            )
    ggsave(paste0("Plots/boxplots/box_", name, ".png"), width=8.5, height=6, dpi=80)
}

draw_end_dataset_plots <- function(df){
    for (col_nr in 1:ncol(df)) {
        feature <- df[, col_nr]
        name <- names(df)[col_nr]

        if (max(feature) > 25 || grepl("PCA", name, fixed=TRUE) ){
            draw_historgram(df, feature, name)
            #draw_boxplot(df, feature, name)
        } 
        else if (max(feature) > 1) {
            draw_barplot(df, feature, name)
            draw_boxplot(df, feature, name)
        }
        else {
            #draw_barplot(df, feature, name)
            draw_piechart(df, feature, name)
        }
    }
}
