# Validaciones y operaciones compartidas; solo funciones básicas de R.
.checkNumeric <- function(x) {
    if (!is.numeric(x) || is.complex(x) || !is.null(dim(x))) stop("Se requiere un vector numérico.")
    if (any(is.infinite(x))) stop("No se admiten infinitos.")
}
.checkBins <- function(n) {
    if (length(n) != 1L || !is.numeric(n) || is.na(n) || !is.finite(n) ||
        n < 1 || n != floor(n)) stop("num.bins debe ser un entero positivo.")
}
.checkData <- function(df) {
    if (!is.data.frame(df) || anyDuplicated(names(df))) stop("Data frame con nombres únicos requerido.")
    if (any(!vapply(df, function(x) is.atomic(x) && is.null(dim(x)), logical(1)))) {
        stop("Las columnas deben ser vectores atómicos.")
    }
}
.checkColumns <- function(df, columns) {
    .checkData(df)
    if (!is.character(columns) || anyNA(columns) || any(!columns %in% names(df))) {
        stop("Hay nombres de columnas desconocidos.")
    }
}
.transformNumeric <- function(df, fn, exclude) {
    .checkColumns(df, exclude)
    for (name in setdiff(names(df), exclude)) {
        if (is.numeric(df[[name]])) df[[name]] <- fn(df[[name]])
    }
    df
}
.paired <- function(x, y) {
    if (length(x) != length(y)) stop("Los vectores deben tener la misma longitud.")
    observed <- !is.na(x) & !is.na(y)
    list(x = x[observed], y = y[observed])
}
.validateClass <- function(df, class, positive) {
    .checkData(df)
    if (is.null(class)) {
        if (!is.null(positive)) stop("positive requiere una columna de clase.")
        return(invisible(NULL))
    }
    .checkColumns(df, class)
    if (length(class) != 1L) stop("Especifique una única columna de clase.")
    labels <- sort(unique(as.character(df[[class]][!is.na(df[[class]])])))
    if (length(labels) != 2L) stop("La clase debe ser binaria.")
    if (!is.null(positive) && (length(positive) != 1L || is.na(positive) ||
                             !as.character(positive) %in% labels)) stop("Clase positiva inválida.")
}
