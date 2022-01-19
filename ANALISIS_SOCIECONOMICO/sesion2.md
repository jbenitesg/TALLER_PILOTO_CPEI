Análisis socioeconómico aplicado a la ENAHO
================
Josue Benites - Dirección Económica CPEI

# Información general

## Sobre la fuente de datos

Los datos a utilizar serán recolectados del [portal de Microdatos del
INEI](http://iinei.inei.gob.pe/microdatos/). Seleccionaremos la Encuesta
Nacional de Hogares (ENAHO) para los diversos módulos de los periodos
2018-2020. ![microdatos](./inputs/microdatos.png)

``` r
setwd("E:/GITHUB/TALLER_PILOTO_CPEI/ANALISIS_SOCIECONOMICO")
inputs<-"inputs/"
inputs
```

    ## [1] "inputs/"

``` r
library(haven)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
mod100<-haven::read_dta(paste0(inputs,"enaho01-2019-100.dta"))
```

``` r
head(mod100)
```

    ## # A tibble: 6 x 323
    ##   aÑo   mes   nconglome conglome vivienda hogar ubigeo  dominio  estrato periodo
    ##   <chr> <chr> <chr>     <chr>    <chr>    <chr> <chr>  <dbl+lb> <dbl+lb>   <dbl>
    ## 1 2019  10    007060    005001   007      11    010101 4 [sier~ 4 [ de ~       1
    ## 2 2019  10    007060    005001   017      11    010101 4 [sier~ 4 [ de ~       1
    ## 3 2019  10    007060    005001   028      11    010101 4 [sier~ 4 [ de ~       1
    ## 4 2019  10    007060    005001   040      11    010101 4 [sier~ 4 [ de ~       1
    ## 5 2019  10    007060    005001   050      11    010101 4 [sier~ 4 [ de ~       1
    ## 6 2019  10    007060    005001   061      11    010101 4 [sier~ 4 [ de ~       1
    ## # ... with 313 more variables: tipenc <dbl+lbl>, fecent <dbl>,
    ## #   result <dbl+lbl>, panel <dbl+lbl>, p22 <dbl+lbl>, p23 <chr>,
    ## #   p24a <dbl+lbl>, p24b <dbl+lbl>, p25_1 <dbl+lbl>, p25_2 <dbl+lbl>,
    ## #   p25_3 <dbl+lbl>, p25_4 <dbl+lbl>, p25_5 <dbl+lbl>, p101 <dbl+lbl>,
    ## #   p102 <dbl+lbl>, p103 <dbl+lbl>, p103a <dbl+lbl>, p104 <dbl>, p104a <dbl>,
    ## #   p104b1 <dbl+lbl>, p104b2 <dbl+lbl>, p105a <dbl+lbl>, p105b <dbl>,
    ## #   p106 <dbl>, p106a <dbl+lbl>, p106b <dbl+lbl>, p107b1 <dbl+lbl>, ...
