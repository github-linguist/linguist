library(cowplot)
library(pheatmap)
library(dplyr)

set.seed(12323414)
options(stringsAsFactors = FALSE)
font_size <- 8

gtable_select <- function (x, ...)
{
    matches <- c(...)
    x$layout <- x$layout[matches, , drop = FALSE]
    x$grobs <- x$grobs[matches]
    x
}

gtable_stack <- function(g1, g2){
    g1$grobs <- c(g1$grobs, g2$grobs)
    g1$layout <- transform(g1$layout, z= z-max(z), name="g2")
    g1$layout <- rbind(g1$layout, g2$layout)
    g1
}

get_a <- function() {
    d <- read.csv("data/2a.csv")

    d[d$Hierarchy == "Kolodziejczyk", ]$Hierarchy <- "Kolodz."

    d$Hierarchy <- factor(
        d$Hierarchy,
        levels = c("Biase", "Yan", "Goolam", "Deng", "Pollen1", "Pollen2",
                   "Kolodz.", "Treutlein", "Ting",
                    "Patel", "Usoskin1", "Usoskin2", "Usoskin3",
                    "Klein", "Zeisel")
    )

    d$Method <- factor(
        d$Method,
        levels = c("SC3", "tSNE+kmeans", "pcaReduce", "SNN-Cliq", "SINCERA", "SEURAT")
    )

    cols <- c("Biase" = "#bc80bd", "Treutlein" = "#8dd3c7", "Ting" = "#ffffb3",
              "Yan" = "#ccebc5", "Goolam" = "#ffed6f", "Deng" = "#bebada",
              "Pollen1" = "#fb8072", "Pollen2" = "#fb8072",
              "Patel" = "#80b1d3", "Usoskin1" = "#fdb462", "Usoskin2" = "#fdb462",
              "Usoskin3" = "#fdb462", "Kolodz." = "#bf812d",
              "Klein" = "#b3de69", "Zeisel" = "#fccde5", "Macosko" = "#d9d9d9")

    meth_cols <- c(
        "SC3" = "#e41a1c",
        "tSNE+kmeans" = "#377eb8",
        "pcaReduce" = "#40E0D0",
        "SNN-Cliq" = "#984ea3",
        "SINCERA" = "#ff7f00",
        "SEURAT" = "#ffff33"
    )

    d1 <- d %>%
        group_by(Method, Hierarchy) %>%
        dplyr::summarise(Median = median(ARI))

    p <- ggplot(d, aes(x = 1, y = ARI, fill = Method, group = Method)) +
        geom_bar(data = d1, aes(y = Median), position="dodge", stat="identity") +
        geom_point(position = position_jitterdodge(jitter.width = 0.45, dodge.width = 0.9), size = 0.4) +
        facet_wrap(ncol = 5, ~ Hierarchy) +
        scale_fill_manual(values = meth_cols) +
        scale_colour_manual(values = meth_cols) +
        geom_hline(yintercept = 0.8) +
        labs(x = "") +
        theme_classic(base_size = font_size) +
        theme(axis.ticks.x = element_blank(), axis.text.x=element_blank(),
              axis.title.x=element_blank(), axis.line=element_blank(),
              legend.key.size = unit(0.4, "cm")) +
        annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "black")+
        annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "black")


    dummy <- ggplot(d, aes(x = 1, y = ARI, fill = Method)) +
        facet_wrap(ncol = 5, ~ Hierarchy) +
        geom_rect(aes(fill = Hierarchy), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
        scale_fill_manual(values = cols) +
        theme_minimal()

    g1 <- ggplotGrob(p)
    g2 <- ggplotGrob(dummy)

    panels <- grepl(pattern="panel", g2$layout$name)
    strips <- grepl(pattern="strip-t", g2$layout$name)
    g2$layout$t[panels] <- g2$layout$t[panels] - 1
    g2$layout$b[panels] <- g2$layout$b[panels] - 1

    new_strips <- gtable_select(g2, panels | strips)

    new_plot <- gtable_stack(g1, new_strips)
    return(new_plot)
}

get_c <- function() {
    cols <- c("Treutlein" = "#8dd3c7", "Ting" = "#ffffb3", "Deng" = "#bebada",
              "Pollen2" = "#fb8072", "Patel" = "#80b1d3",
              "Kolodziejczyk" = "#bf812d", "Usoskin3" = "#fdb462",
              "Klein" = "#40E0D0", "Zeisel" = "#fccde5", "Macosko" = "#d9d9d9")

    d <- read.csv("data/2c.csv")

    d$Dataset <- factor(
        d$Dataset,
        levels = c(
            "Deng",
            "Pollen2",
            "Kolodziejczyk",
            "Patel",
            "Usoskin3",
            "Klein",
            "Zeisel",
            "Macosko"
        )
    )

    d$Fraction <- factor(
        d$Fraction,
        levels = sort(unique(as.numeric(d$Fraction)))
    )

    p <- ggplot(d, aes(x = 1, ARI, fill = Dataset, color = Dataset)) +
        geom_boxplot(position = position_dodge(width = 1.5), outlier.size = 0.8) +
        geom_hline(yintercept = 0.8) +
        labs(x = "# of training cells as % of N", y = "ARI") +
        scale_fill_manual(values = cols) +
        scale_colour_manual(values = cols) +
        facet_grid(. ~ Fraction) +
        theme_classic(base_size = font_size) +
        theme(axis.ticks.x = element_blank(), axis.text.x=element_blank(),
              axis.title.x=element_blank(), axis.line=element_blank(),
              strip.background = element_rect(colour = "white"),
              legend.key.size = unit(0.4, "cm")) +
        ylim(0,1) +
        annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "black")+
        annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "black")
    p <- ggdraw(p) +
        draw_label("% of total # of cells\nin a training set",
                   fontface = "bold",
                   size = font_size-3,
                   x = 0.87, y = 0.93)
    return(p)
}

get_d <- function() {
    d <- readRDS("data/2d.rds")
    ann <- data.frame(Stage = factor(d$cell.names, levels = unique(d$cell.names)))
    anno_colors <- list(Stage = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
                                  "#FB9A99", "#FF00FF", "#FDBF6F", "#FF7F00",
                                  "#CAB2D6", "#6A3D9A"))
    names(anno_colors$Stage) <- levels(ann$Stage)
    dat <- d$consensus
    colnames(dat) <- d$cell.names
    write.csv(dat[d$hc$order, d$hc$order], file = "data/2d.csv", quote = FALSE, row.names = FALSE)
    p <- pheatmap(d$consensus,
             cluster_rows = d$hc,
             cluster_cols = d$hc,
             cutree_rows = 10,
             cutree_cols = 10,
             treeheight_col = 9,
             treeheight_row = 9,
             annotation_col = ann,
             annotation_colors = anno_colors,
             show_rownames = F,
             show_colnames = F,
             fontsize = font_size,
             annotation_names_col = F,
             silent = TRUE)
    return(p$gtable)
}

first_col <- plot_grid(get_a(), get_c(), nrow = 2, labels = c("a", "c"), rel_heights = c(2, 1))

second_col <- plot_grid(NULL, get_d(), nrow = 2, labels = c("b", "d"), rel_heights = c(1.5, 1))

plot_grid(first_col, second_col, ncol = 2)

ggsave("jpeg/2.jpeg", w = 9, h = 6)
ggsave("pdf/2.pdf", w = 9, h = 6)

