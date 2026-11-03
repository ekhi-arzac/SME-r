#' Media aritmética propia; conserva el nombre de la implementación original.
#' @param x Vector numérico.
#' @param na.rm Omitir NA (TRUE por defecto).
#' @return Media escalar; NA si está vacío o quedan NA.
#' @export
mean <- function(x, na.rm = TRUE) {
    .checkNumeric(x)
    if (na.rm) x <- x[!is.na(x)]
    if (!length(x) || anyNA(x)) return(NA_real_)
    sum(x) / length(x)
}

#' Varianza muestral propia.
#' @param x Vector numérico; omite NA.
#' @return Suma de desviaciones cuadráticas dividida por n-1; NA si n < 2.
#' @export
variance <- function(x) {
    .checkNumeric(x)
    x <- x[!is.na(x)]
    if (length(x) < 2L) return(NA_real_)
    sum((x - mean(x))^2) / (length(x) - 1L)
}

#' Desviación típica muestral propia.
#' @param x Vector numérico; omite NA.
#' @return Raíz de variance(x), o NA si hay menos de dos observaciones.
#' @export
stdDev <- function(x) sqrt(variance(x))

#' Entropía empírica de Shannon en bits.
#' @param x Vector de categorías; omite NA y niveles no observados.
#' @return -sum(p * log2(p)); NA sin observaciones.
#' @export
entropy <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(NA_real_)
    counts <- table(x)
    p <- as.numeric(counts[counts > 0]) / length(x)
    -sum(p * log2(p))
}

#' Entropía cruzada H(P,Q), distinta de la entropía conjunta.
#' @param x Muestra de P; omite NA.
#' @param y Muestra de Q; omite NA, puede tener distinta longitud.
#' @return Bits; Inf si Q no observa una categoría de P, NA con muestras vacías.
#' @export
crossEntropy <- function(x, y) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
    if (!length(x) || !length(y)) return(NA_real_)
    labels <- unique(as.character(x))
    p <- vapply(labels, function(label) sum(as.character(x) == label) / length(x), numeric(1))
    q <- vapply(labels, function(label) sum(as.character(y) == label) / length(y), numeric(1))
    if (any(q == 0)) return(Inf)
    -sum(p * log2(q))
}

#' Información mutua empírica mediante frecuencias conjuntas.
#' @param x,y Vectores de categorías de igual longitud; omite pares con NA.
#' @return sum(p(x,y)*log2(p(x,y)/(p(x)*p(y)))) en bits; NA sin pares.
#' @export
mutualInfo <- function(x, y) {
    pair <- .paired(x, y)
    if (!length(pair$x)) return(NA_real_)
    joint <- table(pair$x, pair$y) / length(pair$x)
    px <- rowSums(joint)
    py <- colSums(joint)
    total <- 0
    # Se omiten celdas de frecuencia cero para evitar log2(0).
    for (i in seq_along(px)) for (j in seq_along(py)) {
        p <- joint[i, j]
        if (p > 0) total <- total + p * log2(p / (px[i] * py[j]))
    }
    max(0, total)
}

#' Curva ROC con tratamiento conjunto de empates.
#' @param scores Vector numérico: un valor alto predice la clase positiva.
#' @param class Etiquetas de exactamente dos clases observadas.
#' @param positive Etiqueta positiva; NULL elige la última etiqueta ordenada
#' lexicográficamente tras convertirla a carácter. Se recomienda especificarla.
#' @return Data frame threshold, fpr, tpr; empieza en (0,0) con umbral Inf.
#' Omite pares con NA; requiere ambas clases antes y después de omitirlos.
#' @export
rocCurve <- function(scores, class, positive = NULL) {
    .checkNumeric(scores)
    if (length(scores) != length(class)) stop("Longitudes distintas.")
    labels <- sort(unique(as.character(class[!is.na(class)])))
    if (length(labels) != 2L) stop("La clase debe ser binaria.")
    if (is.null(positive)) positive <- labels[2L]
    if (length(positive) != 1L || is.na(positive) || !as.character(positive) %in% labels) {
        stop("La etiqueta positiva no pertenece a la clase.")
    }
    pair <- .paired(scores, as.character(class))
    indices <- order(pair$x, decreasing = TRUE)
    scores <- pair$x[indices]
    actual <- pair$y[indices] == as.character(positive)
    positives <- sum(actual)
    negatives <- length(actual) - positives
    if (!positives || !negatives) stop("Se necesitan ambas clases entre los pares válidos.")
    result <- data.frame(threshold = Inf, fpr = 0, tpr = 0)
    tp <- fp <- 0
    i <- 1L
    # Cada grupo de puntuaciones iguales añade un único punto a la curva.
    while (i <= length(scores)) {
        j <- i
        while (j <= length(scores) && scores[j] == scores[i]) {
            if (actual[j]) tp <- tp + 1 else fp <- fp + 1
            j <- j + 1L
        }
        result[nrow(result) + 1L, ] <- c(scores[i], fp / negatives, tp / positives)
        i <- j
    }
    result
}

#' Área bajo la curva ROC con integración trapezoidal propia.
#' @param scores,class,positive Véase rocCurve().
#' @return AUC en [0,1]; empates aportan 1/2 y puntuaciones constantes dan 0.5.
#' @export
auc <- function(scores, class, positive = NULL) {
    curve <- rocCurve(scores, class, positive)
    n <- nrow(curve)
    sum((curve$fpr[-1L] - curve$fpr[-n]) * (curve$tpr[-1L] + curve$tpr[-n]) / 2)
}

#' Métricas de los atributos según su tipo.
#' @param df Data frame u objeto sme_dataset; este último aporta los metadatos
#' de clase, positiva y categóricas como valores por defecto.
#' @param class Nombre de la clase binaria; NULL para análisis no supervisado.
#' @param positive Etiqueta positiva para el AUC.
#' @param categorical Nombres que fuerzan tipo categórico aun siendo numéricos.
#' Factores, caracteres y lógicos se reconocen automáticamente como categóricos.
#' @return Data frame attribute, type, n, variance, entropy, auc; excluye la clase.
#' Las métricas no aplicables o indefinidas son NA; AUC es NA si no quedan ambas
#' clases entre los pares válidos de un atributo.
#' @export
attributeMetrics <- function(df, class = NULL, positive = NULL, categorical = character()) {
    UseMethod("attributeMetrics")
}

#' @rdname attributeMetrics
#' @export
attributeMetrics.data.frame <- function(df, class = NULL, positive = NULL, categorical = character()) {
    .checkColumns(df, categorical)
    .validateClass(df, class, positive)
    result <- data.frame(attribute = character(), type = character(), n = integer(),
                         variance = numeric(), entropy = numeric(), auc = numeric())
    for (name in setdiff(names(df), class)) {
        x <- df[[name]]
        numeric <- is.numeric(x) && !name %in% categorical
        v <- h <- a <- NA_real_
        if (numeric) {
            v <- variance(x)
            if (!is.null(class)) {
                pair <- .paired(x, df[[class]])
                if (length(unique(pair$y)) == 2L) a <- auc(x, df[[class]], positive)
            }
        } else h <- entropy(x)
        result[nrow(result) + 1L, ] <- list(name, if (numeric) "numeric" else "categorical",
                                           sum(!is.na(x)), v, h, a)
    }
    result
}
