# SME para R

Paquete `sme` con implementaciones propias para preparar y analizar atributos.
Los gráficos utilizan ggplot2. La entrega de R contiene su propia viñeta sobre Iris:

- Fuente: `vignettes/entrega.Rmd`.
- Documento compilado: `inst/doc/entrega.pdf`.

Instala la dependencia gráfica desde R:

```r
install.packages("ggplot2")
```

Desde el directorio que contiene `SME-r`:

```bash
R CMD INSTALL SME-r
```

El PDF incluido se consulta después de instalar el paquete:

```r
library(sme)
vignette("entrega", package = "sme")
openGuide(browse = FALSE)  # Localizar el PDF.
```

Para recompilarlo se necesitan knitr, rmarkdown, Pandoc y una distribución LaTeX con
XeLaTeX. Desde `SME-r`, con el paquete ya instalado:

```r
install.packages(c("knitr", "rmarkdown"))
rmarkdown::render("vignettes/entrega.Rmd", output_dir = "inst/doc")
```

La construcción completa también compila e incluye la viñeta:

```bash
R CMD build SME-r
```

Una vez publicados los archivos en el repositorio, puede instalarse con:

```r
devtools::install_github("ekhi-arzac/SME-r", build_vignettes = TRUE)
```

Ejemplo con observaciones originales de Iris y una clase binaria para AUC:

```r
library(sme)
flowers <- droplevels(subset(datasets::iris, Species != "setosa"))
ds <- smeDataset(flowers, class = "Species", positive = "virginica")
attributeMetrics(ds)
normalize(ds)
discretizeEF(ds, 3)
filterFeatures(ds, min.auc = 0.8)
associationMatrix(ds, num.bins = 3)
head(as.data.frame(ds))

path <- tempfile(fileext = ".csv")
writeDataset(ds, path)
loaded <- readDataset(path, class = "Species", positive = "virginica")
unlink(path)
```

CSV guarda valores, sin metadatos del objeto: se especifican de nuevo al leer.
`colClasses` controla los tipos inferidos. La ayuda de funciones se consulta con
`?smeDataset` o `?attributeMetrics`. Para cargar los archivos fuente se conserva
`source("load.R")`.

`plotROC()` y `plotAssociation()` dibujan con ggplot2 y devuelven los objetos gráficos
para personalizarlos o guardarlos con `ggplot2::ggsave()`.
