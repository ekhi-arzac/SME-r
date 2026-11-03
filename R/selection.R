#' Filtrar atributos mediante umbrales mínimos inclusivos.
#' @param df Data frame u objeto sme_dataset; este último aporta sus metadatos
#' y devuelve otro sme_dataset con las categóricas actualizadas.
#' @param min.variance,min.entropy,min.auc Umbrales escalares finitos o NULL.
#' Varianza y AUC se aplican a numéricas; entropía a categóricas.
#' @param class Nombre de la clase binaria; requerido al filtrar por AUC.
#' @param positive Etiqueta positiva para el AUC.
#' @param categorical Nombres que fuerzan tipo categórico.
#' @return Data frame con los atributos que cumplen todos sus umbrales aplicables;
#' conserva la clase. Una métrica indefinida no supera su umbral.
#' @export
filterFeatures <- function(df, min.variance = NULL, min.entropy = NULL, min.auc = NULL,
                           class = NULL, positive = NULL, categorical = character()) {
    UseMethod("filterFeatures")
}

#' @rdname filterFeatures
#' @export
filterFeatures.data.frame <- function(df, min.variance = NULL, min.entropy = NULL, min.auc = NULL,
                                      class = NULL, positive = NULL, categorical = character()) {
    for (threshold in list(min.variance, min.entropy, min.auc)) {
        if (!is.null(threshold) && (length(threshold) != 1L || !is.numeric(threshold) ||
                                   is.na(threshold) || !is.finite(threshold))) stop("Umbral inválido.")
    }
    if (!is.null(min.auc) && is.null(class)) stop("El filtrado por AUC requiere clase.")
    metrics <- attributeMetrics(df, class, positive, categorical)
    keep <- rep(TRUE, nrow(metrics))
    for (metric in c("variance", "entropy", "auc")) {
        threshold <- switch(metric, variance = min.variance, entropy = min.entropy, auc = min.auc)
        if (is.null(threshold)) next
        applicable <- metrics$type == if (metric == "entropy") "categorical" else "numeric"
        values <- metrics[[metric]]
        keep[applicable] <- keep[applicable] & !is.na(values[applicable]) & values[applicable] >= threshold
    }
    df[, names(df) %in% c(metrics$attribute[keep], class), drop = FALSE]
}
