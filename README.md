<!-- README.md is generated from README.Rmd. Please edit that file -->



mpoly
=====

mpoly is a simple collection of tools to help deal with multivariate polynomials *symbolically* and functionally in R.

``` {.r}
library(mpoly)
#> Loading required package: stringr

p1 <- mp("x + y")
p2 <- mp("x - y")

p1 * p2
#> x^2  -  y^2
p1^2
#> x^2  +  2 x y  +  y^2




library(ggplot2)
(p <- round(as.mpoly(lm(dist ~ speed, data = cars))))
#> -17.579  +  3.932 speed
qplot(speed, dist, data = cars) +
  stat_function(fun = as.function(p), color = "red")
#> f(speed)
```

![](README-mp-1.png)

``` {.r}

(p <- round(as.mpoly(lm(dist ~ speed + I(speed^2), data = cars))))
#> 2.47  +  0.913 speed  +  0.1 speed^2
qplot(speed, dist, data = cars) +
  stat_function(fun = as.function(p), color = "red")
#> f(speed)
```

![](README-mp-2.png)

Installation
------------

-   From CRAN: `install.packages("mpoly")`

-   From Github (dev version):

    ``` {.R}
    # install.packages("devtools")
    devtools::install_github("Rexamine/stringi")
    devtools::install_github("hadley/stringr")
    devtools::install_github("dkahle/mpoly")
    ```
