# Readme
Repositório para trabalho final da cadeira de Machine Learning do Mestrado de Data Science 2021/2023

![GitHub issues](https://img.shields.io/github/issues/pmags/fcup_ml_modelCompare?style=flat-square)
![GitHub milestone](https://img.shields.io/github/milestones/progress-percent/pmags/fcup_ml_modelCompare/1?style=flat-square)

## Instruções para construção do relatório

Projecto construido utilizando `Bookdown`. Para compilar relatório final utilizar as seguintes instruções:

```
# Html
bookdown::render_book("index.Rmd", "bookdown::gitbook")

bookdown::render_book("index.Rmd", "bookdown::html_book")

# HTML single file
bookdown::render_book("index.Rmd", "bookdown::html_document2")

# pdf
bookdown::render_book("index.Rmd", "bookdown::pdf_book")

```

## Estrutura do repositório

```
.
|-- build             # Compiled files (pdf or html)
|-- data              # Datasets utilizados (raw version)
|-- models            # Output modelos (em lógica para reprodução ou reutilização)
|-- notebooks         # Notebooks de trabalho. Um por capitulo ou subcapitulo cf necessário (*.rmd)
|-- scripts           # scripts de apoio aos notebooks
|-- test              # Testes unitários e verificações do output
|-- theme             # Estilos e Latex
|-- transformations   # Scripts de transformação de preparação para análise
```

## Ambiente de trabalho

Ambiente de trabalho em renv ((link)[https://rstudio.github.io/renv/articles/renv.html]).

Para guardar alterações ao ambiente `renv::snapshot()`.
Para iniciar packages num novo ambiente `renv::restore()`

**Novos projetos deverão ser criados diretamente do repositório github.**


## Links uteis


https://rpubs.com/ppaquay/65566
https://stackoverflow.com/questions/24052643/how-to-plot-non-linear-decision-boundaries-with-a-grid-in-r
https://mathformachines.com/posts/decision/
https://mathformachines.com/posts/decision/
https://www.phinguyen.me/blog/visualizing-decision-boundaries/
https://www.r-bloggers.com/2020/03/visualizing-decision-tree-partition-and-decision-boundaries/

https://discrim.tidymodels.org/
https://www.jeffdoser.com/files/for875/_book/classification.html
https://stackoverflow.com/questions/27863171/issues-plotting-a-fitted-svm-models-decision-boundary-using-ggplot2s-stat-cont
https://emilhvitfeldt.github.io/ISLR-tidymodels-labs/classification.html

https://rpubs.com/cliex159/885971
