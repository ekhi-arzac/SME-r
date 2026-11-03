#' Dibujar una curva ROC y su AUC con ggplot2.
#' @param scores,class,positive Véase rocCurve().
#' @param main Título del gráfico.
#' @return Invisiblemente, lista curve, auc y plot (objeto ggplot reutilizable).
#' Dibuja la curva al llamar a la función; plot puede guardarse con ggplot2::ggsave().
#' @export
plotROC <- function(scores, class, positive = NULL, main = "Curva ROC") {
    curve <- rocCurve(scores, class, positive)
    area <- auc(scores, class, positive)
    # Solo cambia la representación: curva y área siguen siendo cálculos propios.
    chart <- ggplot2::ggplot(curve, ggplot2::aes(x = .data[["fpr"]], y = .data[["tpr"]])) +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray60") +
        ggplot2::geom_path(linewidth = 1, color = "#237e85") +
        ggplot2::coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
        ggplot2::labs(title = main, subtitle = sprintf("AUC = %.3f", area),
                      x = "Tasa de falsos positivos", y = "Tasa de verdaderos positivos") +
        ggplot2::theme_minimal(base_size = 12)
    print(chart)
    invisible(list(curve = curve, auc = area, plot = chart))
}

#' Dibujar matrices de correlación e información mutua con ggplot2.
#' @param association Lista devuelta por associationMatrix().
#' @param main Título del gráfico.
#' @return Invisiblemente, la lista de entrada con un campo plots que contiene los
#' objetos ggplot pearson y mi. Dibuja dos paneles con escalas independientes:
#' Pearson [-1,1] e información mutua en bits. Celdas ausentes en gris.
#' Los objetos de plots pueden personalizarse o guardarse con ggplot2::ggsave().
#' @export
plotAssociation <- function(association, main = "Asociaciones") {
    values <- association$values
    methods <- association$methods
    if (!is.matrix(values) || !is.matrix(methods) || !is.numeric(values) ||
        !identical(dim(values), dim(methods)) || !nrow(values) || nrow(values) != ncol(values)) {
        stop("Se requiere una matriz de asociación cuadrada no vacía.")
    }
    n <- nrow(values)
    rows <- rownames(values)
    columns <- colnames(values)
    if (is.null(rows)) rows <- as.character(seq_len(n))
    if (is.null(columns)) columns <- as.character(seq_len(n))
    # Una fila por celda; conserva también las celdas sin valor para pintarlas en gris.
    cells <- expand.grid(column = seq_len(n), row = seq_len(n))
    positions <- cbind(cells$row, cells$column)
    plots <- list()
    for (kind in c("pearson", "mi")) {
        mask <- if (kind == "pearson") methods == "pearson" else methods != "pearson"
        panel <- cells
        panel$value <- values[positions]
        panel$value[is.na(mask[positions]) | !mask[positions]] <- NA_real_
        panel$label <- ifelse(is.na(panel$value), "", sprintf("%.2f", panel$value))
        chart <- ggplot2::ggplot(panel, ggplot2::aes(x = .data[["column"]], y = .data[["row"]])) +
            ggplot2::geom_tile(ggplot2::aes(fill = .data[["value"]]), color = "white") +
            ggplot2::geom_text(ggplot2::aes(label = .data[["label"]]), size = 3) +
            ggplot2::scale_x_continuous(breaks = seq_len(n), labels = columns, expand = c(0, 0)) +
            ggplot2::scale_y_reverse(breaks = seq_len(n), labels = rows, expand = c(0, 0)) +
            ggplot2::coord_fixed() +
            ggplot2::labs(title = main, subtitle = if (kind == "pearson") "Pearson" else
                          "Información mutua", x = NULL, y = NULL) +
            ggplot2::theme_minimal(base_size = 11) +
            ggplot2::theme(panel.grid = ggplot2::element_blank(),
                           axis.text.x = ggplot2::element_text(angle = 55, hjust = 1))
        # No se comparte escala de color: MI está en bits y no está limitada a uno.
        if (kind == "pearson") {
            chart <- chart + ggplot2::scale_fill_gradient2(name = "Pearson", low = "#b34747",
                mid = "white", high = "#237e85", midpoint = 0, limits = c(-1, 1), na.value = "gray90")
        } else {
            upper <- max(c(1, panel$value), na.rm = TRUE)
            chart <- chart + ggplot2::scale_fill_gradient(name = "Bits", low = "white",
                high = "#237e85", limits = c(0, upper), na.value = "gray90")
        }
        plots[[kind]] <- chart
    }
    # grid forma parte de R y permite mostrar ambos ggplots en la misma página.
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
    on.exit(grid::popViewport())
    print(plots$pearson, newpage = FALSE, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(plots$mi, newpage = FALSE, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
    association$plots <- plots
    invisible(association)
}
