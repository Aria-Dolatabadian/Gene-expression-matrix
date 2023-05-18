library(ComplexHeatmap)
library(circlize)

expr = read.csv("expression.csv")


mat = as.matrix(expr[, grep("cell", colnames(expr))])
base_mean = rowMeans(mat)
mat_scaled = t(apply(mat, 1, scale))

type = gsub("s\\d+_", "", colnames(mat))
ha = HeatmapAnnotation(type = type, annotation_name_side = "left")

ht_list = Heatmap(mat_scaled, name = "expression", row_km = 5, 
    col = colorRamp2(c(-2, 0, 2), c("green", "white", "red")),
    top_annotation = ha, 
    show_column_names = FALSE, row_title = NULL, show_row_dend = FALSE) +
Heatmap(base_mean, name = "base mean", 
    top_annotation = HeatmapAnnotation(summary = anno_summary(gp = gpar(fill = 2:6), 
        height = unit(2, "cm"))),
    width = unit(15, "mm")) +
rowAnnotation(length = anno_points(expr$length, pch = 16, size = unit(1, "mm"), 
    axis_param = list(at = c(0, 2e5, 4e5, 6e5), 
        labels = c("0kb", "200kb", "400kb", "600kb")),
    width = unit(2, "cm"))) +
Heatmap(expr$type, name = "gene type", 
    top_annotation = HeatmapAnnotation(summary = anno_summary(height = unit(2, "cm"))),
    width = unit(15, "mm"))

ht_list = rowAnnotation(block = anno_block(gp = gpar(fill = 2:6, col = NA)), 
    width = unit(2, "mm")) + ht_list

draw(ht_list)
