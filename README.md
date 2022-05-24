# Readme
Marta Ferreira
Repositório para trabalho final da cadeira de Machine Learning do Mestrado de Data Science 2021/2023


## Instruções para construção do relatório

Projecto construido utilizando `Bookdown`. Para compilar relatório final utilizar as seguintes instruções:

```
# Html
bookdown::render_book("index.Rmd", "bookdown::gitbook")

# pdf
bookdown::render_book("index.Rmd", "bookdown::pdf_book")

```

## Estrutura do repositório

.
|-- build             # Compiled files (pdf or html)
|-- data              # Datasets utilizados (raw version)
|-- models            # Output modelos (em lógica para reprodução ou reutilização)
|-- notebooks         # Notebooks de trabalho. Um por capitulo ou subcapitulo cf necessário (*.rmd)
|-- scripts           # scripts de apoio aos notebooks
|-- test              # Testes unitários e verificações do output
|-- theme             # Estilos e Latex
|-- transformations   # Scripts de transformação de preparação para análise

## Ambiente de trabalho

Ambiente de trabalho em renv ((link)[https://rstudio.github.io/renv/articles/renv.html]).

Para guardar alterações ao ambiente `renv::snapshot()`.
Para iniciar packages num novo ambiente `renv::restore()`

**Novos projetos deverão ser criados diretamente do repositório github.**

Pedro: 8,4,2,6,7
Marta: 3,5,9,10,11

https://rpubs.com/ppaquay/65566
https://stackoverflow.com/questions/24052643/how-to-plot-non-linear-decision-boundaries-with-a-grid-in-r
https://mathformachines.com/posts/decision/