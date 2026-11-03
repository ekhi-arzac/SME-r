#' Normalización min-max a [0,1].
#' @param x Vector numérico, data frame u objeto sme_dataset. En este último caso
#' se conservan sus metadatos y se protegen su clase y categóricas declaradas.
#' @param exclude Nombres de columnas que se conservan, por ejemplo la clase.
#' @param ... Argumentos del método.
#' @return Vector o data frame. Conserva NA y no numéricas; constantes dan ceros.
#' @export
normalize <- function(x, ...) UseMethod("normalize")
#' @rdname normalize
#' @export
normalize.numeric <- function(x, ...) {
    .checkNumeric(x)
    observed <- !is.na(x)
    if (!any(observed)) return(x)
    lower <- min(x[observed])
    width <- max(x[observed]) - lower
    x[observed] <- if (width == 0) 0 else (x[observed] - lower) / width
    x
}
#' @rdname normalize
#' @export
normalize.data.frame <- function(x, exclude = character(), ...) .transformNumeric(x, normalize, exclude)

#' Estandarización con media cero y desviación típica muestral uno.
#' @param x Vector numérico, data frame u objeto sme_dataset. En este último caso
#' se conservan sus metadatos y se protegen su clase y categóricas declaradas.
#' @param exclude Nombres de columnas que se conservan.
#' @param ... Argumentos del método.
#' @return Vector o data frame. Conserva NA y no numéricas; constantes y columnas
#' con una sola observación dan ceros en las posiciones observadas.
#' @export
standardize <- function(x, ...) UseMethod("standardize")
#' @rdname standardize
#' @export
standardize.numeric <- function(x, ...) {
    .checkNumeric(x)
    observed <- !is.na(x)
    if (!any(observed)) return(x)
    deviation <- stdDev(x)
    x[observed] <- if (is.na(deviation) || deviation == 0) 0 else (x[observed] - mean(x)) / deviation
    x
}
#' @rdname standardize
#' @export
standardize.data.frame <- function(x, exclude = character(), ...) .transformNumeric(x, standardize, exclude)
