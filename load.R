# source("load.R") desde SME-r, o source("ruta/a/SME-r/load.R").
local({
    root <- dirname(normalizePath(sys.frame(1)$ofile))
    for (file in c("utils.R", "discretization.R", "metrics.R", "preprocessing.R",
                   "selection.R", "association.R", "plots.R", "dataset.R")) {
        sys.source(file.path(root, "R", file), envir = parent.env(environment()))
    }
})
