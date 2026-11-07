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

#' Visualizar PCA: observaciones, varianza explicada y pesos de atributos.
#'
#' Centra los atributos y calcula la SVD con base::svd(). Excluye la columna
#' de grupos y las columnas no numéricas; en sme_dataset excluye también la
#' clase y las categorías declaradas. Omite constantes y rechaza ausentes e
#' infinitos en predictores. Requiere dos filas y dos atributos variables.
#' Los grupos solo colorean la proyección y admiten más de dos categorías.
#' @param data Data frame u objeto sme_dataset; no se modifica.
#' @param group Nombre de la columna de grupos, o NULL. En sme_dataset utiliza
#' la clase por defecto. No se admiten grupos ausentes.
#' @param scale TRUE estandariza con desviación muestral; FALSE solo centra.
#' @param main Título de la figura.
#' @return Invisiblemente, lista con scores, loadings (vectores de la SVD),
#' explained_variance (fracciones), center, scale, dropped (constantes) y plots
#' (ggplots scores, variance y loadings). Dibuja tres paneles. Conserva las filas.
#' @examples
#' pca <- plotPCA(datasets::iris, group = "Species")
#' pca$explained_variance
#' @export
plotPCA <- function(data, group = NULL, scale = TRUE,
                    main = "Análisis de componentes principales") {
    excluded <- character()
    if (inherits(data, "sme_dataset")) {
        excluded <- unique(c(data$class, data$categorical))
        if (is.null(group)) group <- data$class
        data <- data$data
    }
    .checkData(data)
    if (!is.logical(scale) || length(scale) != 1L || is.na(scale)) {
        stop("scale debe ser TRUE o FALSE.")
    }
    if (!is.null(group)) {
        .checkColumns(data, group)
        if (length(group) != 1L) stop("Especifique una única columna de grupos.")
        if (anyNA(data[[group]])) stop("La columna de grupos no puede contener ausentes.")
        excluded <- unique(c(excluded, group))
    }
    columns <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], excluded)
    if (nrow(data) < 2L || length(columns) < 2L) {
        stop("PCA requiere dos filas y dos atributos numéricos variables.")
    }
    x <- as.matrix(data[columns])
    if (is.complex(x)) stop("PCA requiere atributos numéricos reales.")
    if (any(!is.finite(x))) stop("PCA no admite ausentes ni infinitos en los atributos numéricos.")
    variable <- vapply(data[columns], function(x) any(x != x[1L]), logical(1))
    dropped <- columns[!variable]
    columns <- columns[variable]
    if (length(columns) < 2L) stop("PCA requiere al menos dos atributos numéricos no constantes.")
    x <- x[, variable, drop = FALSE]
    center <- colMeans(x)
    centered <- sweep(x, 2, center, "-")
    scales <- if (scale) sqrt(colSums(centered^2) / (nrow(x) - 1L)) else
        stats::setNames(rep(1, length(columns)), columns)
    standardized <- sweep(centered, 2, scales, "/")
    fit <- base::svd(standardized, nu = 0L)
    loadings <- fit$v
    # El peso de mayor magnitud fija el signo de cada componente.
    signs <- vapply(seq_len(ncol(loadings)), function(j) {
        if (loadings[which.max(abs(loadings[, j])), j] < 0) -1 else 1
    }, numeric(1))
    loadings <- sweep(loadings, 2, signs, "*")
    components <- paste0("PC", seq_along(fit$d))
    dimnames(loadings) <- list(columns, components)
    scores <- as.data.frame(standardized %*% loadings)
    explained <- stats::setNames(fit$d^2 / sum(fit$d^2), components)
    panel <- scores
    if (!is.null(group)) panel$.group <- factor(data[[group]])
    score_plot <- ggplot2::ggplot(panel, ggplot2::aes(x = .data[["PC1"]], y = .data[["PC2"]])) +
        ggplot2::geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
        ggplot2::geom_vline(xintercept = 0, color = "gray70", linewidth = 0.3)
    if (is.null(group)) {
        score_plot <- score_plot + ggplot2::geom_point(color = "#237e85", alpha = 0.8, size = 2)
    } else {
        score_plot <- score_plot + ggplot2::geom_point(ggplot2::aes(color = .data[[".group"]]),
                                                      alpha = 0.8, size = 2)
    }
    score_plot <- score_plot + ggplot2::coord_fixed() +
        ggplot2::labs(title = "Observaciones", subtitle = main, color = group,
                      x = sprintf("PC1 (%.1f%%)", 100 * explained[1L]),
                      y = sprintf("PC2 (%.1f%%)", 100 * explained[2L])) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "bottom")
    variance_data <- data.frame(component = seq_along(explained),
                               individual = 100 * explained, cumulative = 100 * cumsum(explained))
    variance_plot <- ggplot2::ggplot(variance_data, ggplot2::aes(x = .data[["component"]])) +
        ggplot2::geom_col(ggplot2::aes(y = .data[["individual"]]), fill = "#237e85") +
        ggplot2::geom_line(ggplot2::aes(y = .data[["cumulative"]]), color = "#b34747") +
        ggplot2::geom_point(ggplot2::aes(y = .data[["cumulative"]]), color = "#b34747") +
        ggplot2::scale_x_continuous(breaks = seq_along(explained), labels = components) +
        ggplot2::coord_cartesian(ylim = c(0, 105)) +
        ggplot2::labs(title = "Varianza explicada", subtitle = "Barras: individual; línea: acumulada",
                      x = NULL, y = "Porcentaje (%)") + ggplot2::theme_minimal(base_size = 11)
    weights <- expand.grid(attribute = seq_along(columns), component = 1:2)
    weights$value <- loadings[cbind(weights$attribute, weights$component)]
    weights$label <- sprintf("%.2f", weights$value)
    loading_plot <- ggplot2::ggplot(weights,
        ggplot2::aes(x = .data[["component"]], y = .data[["attribute"]])) +
        ggplot2::geom_tile(ggplot2::aes(fill = .data[["value"]]), color = "white") +
        ggplot2::geom_text(ggplot2::aes(label = .data[["label"]]), size = 3) +
        ggplot2::scale_x_continuous(breaks = 1:2, labels = components[1:2], expand = c(0, 0)) +
        ggplot2::scale_y_reverse(breaks = seq_along(columns), labels = columns, expand = c(0, 0)) +
        ggplot2::scale_fill_gradient2(low = "#b34747", mid = "white", high = "#237e85",
                                      limits = c(-1, 1), name = "Peso") +
        ggplot2::labs(title = "Pesos de los atributos", subtitle = "Vectores unitarios de la SVD",
                      x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom")
    plots <- list(scores = score_plot, variance = variance_plot, loadings = loading_plot)
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 3)))
    on.exit(grid::popViewport())
    for (j in seq_along(plots)) {
        print(plots[[j]], newpage = FALSE, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = j))
    }
    invisible(list(scores = scores, loadings = loadings, explained_variance = explained,
                   center = center, scale = scales, dropped = dropped, plots = plots))
}
