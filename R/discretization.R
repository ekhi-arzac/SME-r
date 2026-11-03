#' Transformar columnas seleccionadas.
#' @param df Data frame de entrada, sin modificar el original.
#' @param filter Predicado que selecciona cada vector columna.
#' @param fn Función de transformación del vector seleccionado.
#' @return Data frame con las mismas filas y nombres.
#' @export
applyColumns <- function(df, filter, fn) {
    .checkData(df)
    df[] <- lapply(df, function(column) if (filter(column)) fn(column) else column)
    df
}

#' Discretización de igual anchura, sin usar cut().
#' @param x Vector numérico, data frame u objeto sme_dataset. En este último caso
#' protege automáticamente su clase y categóricas declaradas y conserva metadatos.
#' @param num.bins Entero positivo: intervalos solicitados.
#' @param exclude Nombres de columnas que se conservan, por ejemplo la clase.
#' @param ... Argumentos del método.
#' @return Etiquetas I1, I2, ... o data frame. Conserva NA y no numéricas.
#' Un límite pertenece al intervalo inferior; una constante genera I1.
#' @export
discretizeEW <- function(x, ...) UseMethod("discretizeEW")
#' @rdname discretizeEW
#' @export
discretizeEW.numeric <- function(x, num.bins, ...) {
    .checkNumeric(x)
    .checkBins(num.bins)
    values <- x[!is.na(x)]
    if (!length(values) || min(values) == max(values)) return(discretize(x, numeric()))
    limits <- min(values) + (max(values) - min(values)) * seq_len(num.bins - 1L) / num.bins
    discretize(x, limits)
}
#' @rdname discretizeEW
#' @export
discretizeEW.data.frame <- function(x, num.bins, exclude = character(), ...) {
    .checkBins(num.bins)
    .transformNumeric(x, function(column) discretizeEW(column, num.bins), exclude)
}

#' Discretización de igual frecuencia conservando juntos los valores iguales.
#' @param x Vector numérico, data frame u objeto sme_dataset. En este último caso
#' protege automáticamente su clase y categóricas declaradas y conserva metadatos.
#' @param num.bins Entero positivo: intervalos solicitados.
#' @param exclude Nombres de columnas que no se transforman.
#' @param ... Argumentos del método.
#' @return Etiquetas I1, I2, ... o data frame. Conserva NA y no numéricas.
#' Los empates pueden generar menos intervalos y frecuencias desiguales.
#' @export
discretizeEF <- function(x, ...) UseMethod("discretizeEF")
#' @rdname discretizeEF
#' @export
discretizeEF.numeric <- function(x, num.bins, ...) {
    .checkNumeric(x)
    .checkBins(num.bins)
    values <- sort(x[!is.na(x)])
    if (length(values) < 2L || num.bins == 1L) return(discretize(x, numeric()))
    # Cortes en posiciones ordenadas próximas a múltiplos de n / num.bins.
    positions <- pmax(1L, floor(length(values) * seq_len(num.bins - 1L) / num.bins))
    limits <- unique(values[positions])
    discretize(x, limits[limits < max(values)])
}
#' @rdname discretizeEF
#' @export
discretizeEF.data.frame <- function(x, num.bins, exclude = character(), ...) {
    .checkBins(num.bins)
    .transformNumeric(x, function(column) discretizeEF(column, num.bins), exclude)
}

#' Asignar etiquetas a partir de cortes explícitos.
#' @param x Vector numérico, con NA permitidos e infinitos prohibidos.
#' @param cut.points Vector numérico finito y sin NA; se ordena y deduplica.
#' @param label.prefix Prefijo de etiquetas; por defecto I.
#' @return Vector de caracteres; conserva NA. Un corte pertenece al grupo inferior.
#' @export
discretize <- function(x, cut.points, label.prefix = "I") {
    .checkNumeric(x)
    .checkNumeric(cut.points)
    if (anyNA(cut.points)) stop("Los cortes no pueden contener NA.")
    if (length(label.prefix) != 1L || is.na(label.prefix)) stop("Prefijo inválido.")
    cut.points <- sort(unique(cut.points))
    vapply(x, function(value) {
        if (is.na(value)) NA_character_ else paste0(label.prefix, findRange(value, cut.points))
    }, character(1), USE.NAMES = FALSE)
}

#' Buscar un intervalo mediante búsqueda binaria.
#' @param value Número escalar o NA.
#' @param cut.points Cortes numéricos ordenados, sin NA.
#' @return Índice desde 1, o NA si value es NA.
#' @export
findRange <- function(value, cut.points) {
    if (is.na(value)) return(NA_integer_)
    left <- 1L
    right <- length(cut.points)
    while (left <= right) {
        middle <- (left + right) %/% 2L
        if (value <= cut.points[middle]) right <- middle - 1L else left <- middle + 1L
    }
    left
}
