#' Correlación de Pearson mediante productos de desviaciones.
#' @param x,y Vectores numéricos de igual longitud; omite pares con NA.
#' @return Coeficiente en [-1,1]; NA con constantes o menos de dos pares.
#' @export
pearson <- function(x, y) {
    .checkNumeric(x)
    .checkNumeric(y)
    pair <- .paired(x, y)
    if (length(pair$x) < 2L) return(NA_real_)
    dx <- pair$x - mean(pair$x)
    dy <- pair$y - mean(pair$y)
    denominator <- sqrt(sum(dx^2)) * sqrt(sum(dy^2))
    if (denominator == 0) return(NA_real_)
    max(-1, min(1, sum(dx * dy) / denominator))
}

#' Asociación por pares según el tipo de atributo.
#' @param df Data frame u objeto sme_dataset. Este último aporta sus categóricas
#' declaradas y trata también su clase como categórica.
#' @param categorical Nombres que fuerzan tipo categórico.
#' @param num.bins Intervalos de igual frecuencia para los pares mixtos.
#' @return Lista values (matriz simétrica) y methods (método de cada celda).
#' Dos numéricas: Pearson. Dos categóricas: información mutua en bits.
#' Mixtas: discretización de la numérica e información mutua sobre pares completos.
#' La diagonal categórica es su entropía; las escalas de Pearson y MI son distintas.
#' @export
associationMatrix <- function(df, categorical = character(), num.bins = 5L) {
    UseMethod("associationMatrix")
}

#' @rdname associationMatrix
#' @export
associationMatrix.data.frame <- function(df, categorical = character(), num.bins = 5L) {
    .checkColumns(df, categorical)
    .checkBins(num.bins)
    n <- ncol(df)
    values <- matrix(NA_real_, n, n, dimnames = list(names(df), names(df)))
    methods <- matrix("", n, n, dimnames = dimnames(values))
    numeric <- vapply(df, is.numeric, logical(1)) & !names(df) %in% categorical
    for (i in seq_len(n)) for (j in seq_len(i)) {
        x <- df[[i]]
        y <- df[[j]]
        if (numeric[i]) .checkNumeric(x)
        if (numeric[j]) .checkNumeric(y)
        pair <- .paired(x, y)
        if (numeric[i] && numeric[j]) {
            value <- pearson(pair$x, pair$y)
            method <- "pearson"
        } else {
            if (numeric[i]) pair$x <- discretizeEF(pair$x, num.bins)
            if (numeric[j]) pair$y <- discretizeEF(pair$y, num.bins)
            value <- mutualInfo(pair$x, pair$y)
            method <- if (numeric[i] || numeric[j]) "mi_discretized" else "mi"
        }
        values[i, j] <- values[j, i] <- value
        methods[i, j] <- methods[j, i] <- method
    }
    list(values = values, methods = methods)
}
