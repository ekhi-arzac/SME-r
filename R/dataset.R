#' Dataset S3 con metadatos para reutilizar las funciones de análisis.
#'
#' Guarda los datos, el nombre de la clase binaria, su etiqueta positiva y los
#' nombres de las columnas categóricas. Las transformaciones devuelven otro objeto
#' y protegen automáticamente la clase y los códigos categóricos declarados.
#'
#' @param data Data frame con nombres de columna únicos.
#' @param class Nombre de la clase binaria, o NULL para datos no supervisados.
#' @param positive Etiqueta positiva; NULL utiliza el criterio de auc().
#' @param categorical Nombres de columnas que representan categorías.
#' @return Objeto de clase sme_dataset con campos data, class, positive y categorical.
#' @examples
#' flowers <- droplevels(subset(datasets::iris, Species != "setosa"))
#' ds <- smeDataset(flowers, class = "Species", positive = "virginica")
#' attributeMetrics(ds)
#' normalize(ds)
#' @export
smeDataset <- function(data, class = NULL, positive = NULL, categorical = character()) {
    .checkColumns(data, categorical)
    .validateClass(data, class, positive)
    structure(list(data = data, class = class, positive = positive,
                   categorical = unique(categorical)), class = "sme_dataset")
}

#' @rdname smeDataset
#' @param x Objeto sme_dataset.
#' @param ... Argumentos adicionales del genérico; no se utilizan al imprimir.
#' @return print() devuelve invisiblemente el objeto; as.data.frame() devuelve sus datos.
#' @export
print.sme_dataset <- function(x, ...) {
    cat(sprintf("<sme_dataset> %d filas x %d columnas\n", nrow(x$data), ncol(x$data)))
    cat("Clase:", if (is.null(x$class)) "sin clase" else x$class, "\n")
    print(x$data)
    invisible(x)
}

#' @rdname smeDataset
#' @param row.names,optional Argumentos de compatibilidad de as.data.frame();
#' no alteran los datos almacenados.
#' @export
as.data.frame.sme_dataset <- function(x, row.names = NULL, optional = FALSE, ...) x$data

#' Leer un CSV como objeto sme_dataset.
#'
#' CSV almacena los valores, sin metadatos del objeto ni tipos de R. Al leer se
#' infieren tipos y deben especificarse nuevamente la clase y las categóricas.
#' Puede utilizar colClasses en ... para preservar códigos de texto o factores.
#' @param file Ruta de un CSV.
#' @param class,positive,categorical Metadatos; véase smeDataset().
#' @param ... Opciones adicionales de utils::read.csv(), por ejemplo sep o colClasses.
#' @return Un objeto sme_dataset.
#' @examples
#' path <- tempfile(fileext = ".csv")
#' writeDataset(smeDataset(datasets::iris), path)
#' readDataset(path)
#' unlink(path)
#' @export
readDataset <- function(file, class = NULL, positive = NULL, categorical = character(), ...) {
    data <- utils::read.csv(file, check.names = FALSE, stringsAsFactors = FALSE, ...)
    smeDataset(data, class, positive, categorical)
}

#' Escribir los valores de un dataset en CSV sin nombres de fila.
#' @param x Objeto sme_dataset.
#' @param file Ruta del CSV de salida.
#' @param ... Opciones adicionales de utils::write.csv(), por ejemplo na.
#' @return Invisiblemente, la ruta del archivo escrito.
#' @seealso readDataset
#' @export
writeDataset <- function(x, file, ...) {
    if (!inherits(x, "sme_dataset")) stop("Se requiere un objeto sme_dataset.")
    utils::write.csv(x$data, file, row.names = FALSE, ...)
    invisible(file)
}

# Estas operaciones reconstruyen el objeto para validar y conservar los metadatos.
.datasetTransform <- function(x, fn, exclude = character()) {
    protected <- unique(c(x$class, x$categorical, exclude))
    smeDataset(fn(x$data, protected), x$class, x$positive, x$categorical)
}

#' @rdname normalize
#' @export
normalize.sme_dataset <- function(x, exclude = character(), ...) {
    .datasetTransform(x, function(data, protected) normalize(data, exclude = protected), exclude)
}

#' @rdname standardize
#' @export
standardize.sme_dataset <- function(x, exclude = character(), ...) {
    .datasetTransform(x, function(data, protected) standardize(data, exclude = protected), exclude)
}

#' @rdname discretizeEW
#' @export
discretizeEW.sme_dataset <- function(x, num.bins, exclude = character(), ...) {
    .datasetTransform(x, function(data, protected) discretizeEW(data, num.bins, exclude = protected), exclude)
}

#' @rdname discretizeEF
#' @export
discretizeEF.sme_dataset <- function(x, num.bins, exclude = character(), ...) {
    .datasetTransform(x, function(data, protected) discretizeEF(data, num.bins, exclude = protected), exclude)
}

#' @rdname attributeMetrics
#' @export
attributeMetrics.sme_dataset <- function(df, class = df$class, positive = df$positive,
                                         categorical = df$categorical) {
    attributeMetrics(df$data, class, positive, categorical)
}

#' @rdname filterFeatures
#' @export
filterFeatures.sme_dataset <- function(df, min.variance = NULL, min.entropy = NULL, min.auc = NULL,
                                       class = df$class, positive = df$positive,
                                       categorical = df$categorical) {
    data <- filterFeatures(df$data, min.variance, min.entropy, min.auc, class, positive, categorical)
    smeDataset(data, class, positive, intersect(categorical, names(data)))
}

#' @rdname associationMatrix
#' @export
associationMatrix.sme_dataset <- function(df, categorical = df$categorical, num.bins = 5L) {
    # Una clase codificada con números sigue siendo categórica.
    associationMatrix(df$data, unique(c(categorical, df$class)), num.bins)
}

#' Consultar el documento de uso incluido en la instalación.
#' @param browse Si TRUE, abre el PDF con el visor configurado en R.
#' @return Invisiblemente, la ruta del PDF instalado. Con browse = FALSE no abre
#' el visor. La viñeta también es accesible con vignette("entrega", package = "sme").
#' @examples
#' openGuide(browse = FALSE)
#' @export
openGuide <- function(browse = interactive()) {
    path <- system.file("doc", "entrega.pdf", package = "sme", mustWork = TRUE)
    if (browse) print(utils::vignette("entrega", package = "sme"))
    invisible(path)
}
