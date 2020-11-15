
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HEobs

<!-- badges: start -->

<!-- badges: end -->

The **`{HEobs}`** package makes it easy to process naturalized flow data
and metadata from ONS stations.

## Installation

You can install HEobs from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lhmet-ped/HEobs")
```

## Example

Metadata from ONS stations can be obtained with:

``` r
library(HEobs)
## basic example code
qnat_meta <- extract_metadata(NA_character_, informative = TRUE)
qnat_meta
#> # A tibble: 87 x 5
#>    estacao_codigo latitude longitude nome_estacao municipio   
#>             <dbl>    <dbl>     <dbl> <chr>        <chr>       
#>  1             18   -19.9      -49.9 A. VERMELHA  A. VERMELHA 
#>  2            237   -22.6      -48.3 BARRA BONITA BARRA BONITA
#>  3            215   -27.8      -51.2 BARRA GRANDE BARRA GRANDE
#>  4            119   -23.8      -46.5 BILLINGS_PED BILLINGS_PED
#>  5            190    -6.80     -43.9 B. ESPERANCA B. ESPERANCA
#>  6             32   -18.6      -49.4 CACH.DOURADA CACH.DOURADA
#>  7             14   -21.6      -46.6 CACONDE      CACONDE     
#>  8            247   -18.5      -51.1 CACU         CACU        
#>  9              1   -21.4      -44.5 CAMARGOS     CAMARGOS    
#> 10            216   -27.6      -51.2 CAMPOS NOVOS CAMPOS NOVOS
#> # … with 77 more rows
```

The time series of daily naturalized streamflow …

``` r
qnat <- import_qnat(NA_character_, complete = TRUE, add_stn = TRUE)
str(qnat)
#> tibble [2,796,267 × 5] (S3: tbl_df/tbl/data.frame)
#>  $ date    : Date[1:2796267], format: "1931-01-02" "1931-01-03" ...
#>  $ id      : int [1:2796267] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ qnat    : num [1:2796267] 6117 5927 5820 5582 5409 ...
#>  $ code_stn: num [1:2796267] 18 18 18 18 18 18 18 18 18 18 ...
#>  $ name_stn: chr [1:2796267] "A. VERMELHA" "A. VERMELHA" "A. VERMELHA" "A. VERMELHA" ...
#>  - attr(*, ".internal.selfref")=<externalptr>
```
