---
title: "Climate PCAs_Growth Season"
author: "Brandie Quarles"
date: "2024-11-26"
output: 
  html_document: 
    keep_md: true
---



# Climate PCAs

-   Should use this code to check the significance of the PCA:
    <https://github.com/StatQuest/pca_demo/blob/master/pca_demo.R>
-   Remember this paper: Björklund, M. 2019. Be careful with your
    principal components. Evolution 73: 2151--2158.

-   PCAs for climate during the growth season

    -   Color monthly PCA by month relative to growth season?
    -   Bioclim vars only PCA okay? or combine with other Flint
        variables? Hard to combine b/c Flint data is for each month
        within a year and Bioclim variables are yearly summaries. Could
        do one with just Flint with all months in each year included and
        one with Flint growth season yearly averages combined with
        Bioclim variables
 
## Load necessary libraries 

``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(ggrepel)
#library(cowplot)
library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
## 
## The following object is masked from 'package:dplyr':
## 
##     combine
```

``` r
library(corrplot) #plotting correlations 
```

```
## corrplot 0.94 loaded
```

``` r
library(rstatix) #performing cor_test
```

```
## 
## Attaching package: 'rstatix'
## 
## The following object is masked from 'package:stats':
## 
##     filter
```

``` r
library(QBMS) #for function calc_biovars to calculate bioclim variables
library(ggfortify) #easier PCA figures
sem <- function(x, na.rm=FALSE) {
  sd(x,na.rm=na.rm)/sqrt(length(na.omit(x)))
} #standard error function 

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
} #legend function for grid_arrange

elev_three_palette <- c("#0043F0", "#C9727F", "#F5A540") #colors from Gremer et al 2019
elev_order <- c("High", "Mid", "Low")
month_order <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
```

## Load the Flint growth season data (from "Flint_Growth_Season.Rmd")

``` r
flint_recent_grwseason <- read_csv("../output/Climate/flint_climate_growthseason_recent.csv") %>%
  dplyr::select(month, parent.pop:tmx)
```

```
## Rows: 4980 Columns: 16
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (14): month, growmonth, elev_m, PckSum, Lat, Long, year, cwd, pck, ppt, ...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
head(flint_recent_grwseason, 12)
```

```
## # A tibble: 12 × 13
##    month parent.pop elevation.group elev_m PckSum   Lat  Long  year   cwd   pck
##    <dbl> <chr>      <chr>            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1    12 BH         Low               511.      0  37.4 -120.  1994  29.0     0
##  2     1 BH         Low               511.      0  37.4 -120.  1994  31.3     0
##  3     2 BH         Low               511.      0  37.4 -120.  1994  41.4     0
##  4     3 BH         Low               511.      0  37.4 -120.  1994  61.4     0
##  5     4 BH         Low               511.      0  37.4 -120.  1994  58.6     0
##  6     5 BH         Low               511.      0  37.4 -120.  1994  19.1     0
##  7     6 BH         Low               511.      0  37.4 -120.  1994 115.      0
##  8    11 CC         Low               313       0  39.6 -121.  1994  27.8     0
##  9    12 CC         Low               313       0  39.6 -121.  1994  18.1     0
## 10     1 CC         Low               313       0  39.6 -121.  1994  20.6     0
## 11     2 CC         Low               313       0  39.6 -121.  1994  30.9     0
## 12     3 CC         Low               313       0  39.6 -121.  1994  51.8     0
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
flint_recent_grwseason %>% filter(parent.pop=="DPR")
```

```
## # A tibble: 300 × 13
##    month parent.pop elevation.group elev_m PckSum   Lat  Long  year   cwd   pck
##    <dbl> <chr>      <chr>            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1     4 DPR        Mid              1019.   91.6  39.2 -121.  1994 43.3    0  
##  2     8 DPR        Mid              1019.   91.6  39.2 -121.  1994 39.7    0  
##  3    12 DPR        Mid              1019.   91.6  39.2 -121.  1994  9.24  36.3
##  4     7 DPR        Mid              1019.   91.6  39.2 -121.  1994 39.2    0  
##  5     6 DPR        Mid              1019.   91.6  39.2 -121.  1994 41      0  
##  6     3 DPR        Mid              1019.   91.6  39.2 -121.  1994 30.1    0  
##  7     5 DPR        Mid              1019.   91.6  39.2 -121.  1994 40.5    0  
##  8    11 DPR        Mid              1019.   91.6  39.2 -121.  1994 10.8    0  
##  9    10 DPR        Mid              1019.   91.6  39.2 -121.  1994 46.6    0  
## 10     9 DPR        Mid              1019.   91.6  39.2 -121.  1994 53.1    0  
## # ℹ 290 more rows
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
flint_historical_grwseason <- read_csv("../output/Climate/flint_climate_growthseason_historical.csv") %>%
  dplyr::select(month, parent.pop:tmx)
```

```
## Rows: 4620 Columns: 16
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (14): month, growmonth, elev_m, PckSum, Lat, Long, year, cwd, pck, ppt, ...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
head(flint_historical_grwseason, 12)
```

```
## # A tibble: 12 × 13
##    month parent.pop elevation.group elev_m PckSum   Lat  Long  year   cwd   pck
##    <dbl> <chr>      <chr>            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1    11 BH         Low               511.  0.234  37.4 -120.  1964  40.2     0
##  2    12 BH         Low               511.  0.234  37.4 -120.  1964  27.9     0
##  3     1 BH         Low               511.  0.234  37.4 -120.  1964  28.1     0
##  4     2 BH         Low               511.  0.234  37.4 -120.  1964  40.4     0
##  5     3 BH         Low               511.  0.234  37.4 -120.  1964  55.9     0
##  6     4 BH         Low               511.  0.234  37.4 -120.  1964  70.3     0
##  7     5 BH         Low               511.  0.234  37.4 -120.  1964  42.0     0
##  8     6 BH         Low               511.  0.234  37.4 -120.  1964  42.8     0
##  9    11 CC         Low               313   0.951  39.6 -121.  1964  28.6     0
## 10    12 CC         Low               313   0.951  39.6 -121.  1964  19.3     0
## 11     1 CC         Low               313   0.951  39.6 -121.  1964  19.4     0
## 12     2 CC         Low               313   0.951  39.6 -121.  1964  32.6     0
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

## Load the Bioclim growth season data (from "BioClim_Growth_Season.Rmd")

``` r
bioclim_recent_grwseason <- read_csv("../output/Climate/BioClim_growthseason_Recent.csv")
```

```
## Rows: 690 Columns: 14
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (12): elev_m, year, ann_tmean, mean_diurnal_range, temp_seasonality, tem...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
bioclim_historical_grwseason <- read_csv("../output/Climate/BioClim_growthseason_Historical.csv")
```

```
## Rows: 690 Columns: 14
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (12): elev_m, year, ann_tmean, mean_diurnal_range, temp_seasonality, tem...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


## All years and months included (Flint)

### Correlations - Flint Recent


``` r
#normalize the data
climate_normalized_all_flint_recent <- flint_recent_grwseason %>% select(cwd, pck, ppt, tmn, tmx) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_all_flint_recent)
```

```
##              cwd        pck        ppt         tmn         tmx
## [1,] -0.86251414 -0.2402228 -0.1500623 -0.76764081 -0.96602478
## [2,] -0.79994164 -0.2402228 -0.2881219 -0.63215078 -0.54476885
## [3,] -0.52287391 -0.2402228  0.2133010 -0.61807389 -0.74153103
## [4,]  0.02169808 -0.2402228 -0.5677457 -0.09195026  0.03231214
## [5,] -0.05453654 -0.2402228 -0.1164927  0.12800108  0.32679513
## [6,] -1.13329730 -0.2402228 -0.3165270  0.65412471  0.71503726
```

``` r
cor.norm = cor(climate_normalized_all_flint_recent) #test correlations among the traits
corrplot(cor.norm)
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

``` r
#tmn and tmx highly correlated, consider removing one 
```

### PCA - Flint Recent


``` r
all_flint_recent.pc = prcomp(flint_recent_grwseason[c(9:13)], scale = TRUE, center = TRUE)

str(all_flint_recent.pc)
```

```
## List of 5
##  $ sdev    : num [1:5] 1.794 0.896 0.775 0.588 0.181
##  $ rotation: num [1:5, 1:5] 0.421 -0.339 -0.442 0.491 0.521 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##   .. ..$ : chr [1:5] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:5] 60.58 14.61 75.6 5.88 19.07
##   ..- attr(*, "names")= chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##  $ scale   : Named num [1:5] 36.6 60.82 108.43 5.68 7.57
##   ..- attr(*, "names")= chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##  $ x       : num [1:4980, 1:5] -1.096 -0.723 -0.923 0.313 0.343 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:5] "PC1" "PC2" "PC3" "PC4" ...
##  - attr(*, "class")= chr "prcomp"
```

plot % Variance Explained

``` r
summary(all_flint_recent.pc)
```

```
## Importance of components:
##                           PC1    PC2    PC3     PC4     PC5
## Standard deviation     1.7936 0.8963 0.7753 0.58803 0.18095
## Proportion of Variance 0.6434 0.1607 0.1202 0.06916 0.00655
## Cumulative Proportion  0.6434 0.8041 0.9243 0.99345 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:5,2,pad="0")),
       percent_var=all_flint_recent.pc$sdev[1:5]^2/sum(all_flint_recent.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Combine PCs with metadata

``` r
all_flint_recent.pc.dat = data.frame(all_flint_recent.pc$x)

all_flint_recent_locs.pc = cbind(flint_recent_grwseason, all_flint_recent.pc.dat)

all_flint_recent_loadings = data.frame(varnames=rownames(all_flint_recent.pc$rotation), all_flint_recent.pc$rotation)
all_flint_recent_loadings
```

```
##     varnames        PC1        PC2         PC3         PC4         PC5
## cwd      cwd  0.4209633 -0.4046565  0.52803861 -0.61476189  0.04781279
## pck      pck -0.3386036 -0.8402811  0.09896283  0.40951887  0.04214116
## ppt      ppt -0.4415379 -0.2134006 -0.55927949 -0.65325346 -0.14131813
## tmn      tmn  0.4914009 -0.2018630 -0.54036499  0.05579054  0.65013043
## tmx      tmx  0.5212551 -0.2095049 -0.32648693  0.15655495 -0.74383982
```


``` r
autoplot(all_flint_recent.pc, data = flint_recent_grwseason,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=8, loadings.label.colour="black", loadings.label.vjust = -0.2) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

``` r
#for plot customizations see: ?ggbiplot
```

PCs 3 and 4

``` r
autoplot(all_flint_recent.pc, data = flint_recent_grwseason,
         x=3, y=4,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=8, loadings.label.colour="black", loadings.label.vjust = -0.2) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Correlations - Flint Historical


``` r
#normalize the data
climate_normalized_all_flint_historical <- flint_historical_grwseason %>% select(cwd, pck, ppt, tmn, tmx) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_all_flint_historical)
```

```
##              cwd        pck         ppt         tmn        tmx
## [1,] -0.51979441 -0.2387512  0.37680686 -0.29541143 -0.4545244
## [2,] -0.87681447 -0.2387512  1.28117671 -0.07285286 -0.7038260
## [3,] -0.87126343 -0.2387512 -0.12383791 -0.77636426 -0.9666766
## [4,] -0.51336688 -0.2387512 -0.70526437 -0.88198528 -0.4531695
## [5,] -0.06081035 -0.2387512 -0.05213384 -0.54060307 -0.3827147
## [6,]  0.36136149 -0.2387512 -0.46310268 -0.11623292  0.2120864
```

``` r
cor.norm = cor(climate_normalized_all_flint_historical) #test correlations among the traits
corrplot(cor.norm)
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

``` r
#tmn and tmx highly correlated, consider removing one 
```

### PCA - Flint Historical


``` r
all_flint_historical.pc = prcomp(flint_historical_grwseason[c(9:13)], scale = TRUE, center = TRUE)

str(all_flint_historical.pc)
```

```
## List of 5
##  $ sdev    : num [1:5] 1.791 0.91 0.784 0.555 0.199
##  $ rotation: num [1:5, 1:5] 0.402 -0.367 -0.453 0.481 0.517 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##   .. ..$ : chr [1:5] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:5] 57.95 15.85 79.27 4.64 18.56
##   ..- attr(*, "names")= chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##  $ scale   : Named num [1:5] 34.23 66.4 109.06 5.3 7.38
##   ..- attr(*, "names")= chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##  $ x       : num [1:4620, 1:5] -0.669 -1.243 -1.079 -0.458 -0.371 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:5] "PC1" "PC2" "PC3" "PC4" ...
##  - attr(*, "class")= chr "prcomp"
```

plot % Variance Explained

``` r
summary(all_flint_historical.pc)
```

```
## Importance of components:
##                           PC1    PC2    PC3     PC4     PC5
## Standard deviation     1.7913 0.9104 0.7844 0.55473 0.19854
## Proportion of Variance 0.6418 0.1658 0.1231 0.06154 0.00788
## Cumulative Proportion  0.6418 0.8075 0.9306 0.99212 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:5,2,pad="0")),
       percent_var=all_flint_historical.pc$sdev[1:5]^2/sum(all_flint_historical.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Combine PCs with metadata

``` r
all_flint_historical.pc.dat = data.frame(all_flint_historical.pc$x)

all_flint_historical_locs.pc = cbind(flint_historical_grwseason, all_flint_historical.pc.dat)

all_flint_historical_loadings = data.frame(varnames=rownames(all_flint_historical.pc$rotation), all_flint_historical.pc$rotation)
all_flint_historical_loadings
```

```
##     varnames        PC1        PC2        PC3         PC4         PC5
## cwd      cwd  0.4015908 -0.3597346 -0.7169875 -0.43715586  0.06433885
## pck      pck -0.3672729 -0.7532999 -0.1416984  0.52355590  0.05882720
## ppt      ppt -0.4528282 -0.3579272  0.3501661 -0.71964556 -0.16226126
## tmn      tmn  0.4811828 -0.3112255  0.5099975 -0.04412598  0.63996661
## tmx      tmx  0.5169576 -0.2795658  0.2883344  0.12225850 -0.74599859
```


``` r
autoplot(all_flint_historical.pc, data = flint_historical_grwseason,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=8, loadings.label.colour="black", loadings.label.vjust = -0.2) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

PCs 3 and 4

``` r
autoplot(all_flint_historical.pc, data = flint_historical_grwseason,
         x=3, y=4,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=8, loadings.label.colour="black", loadings.label.vjust = -0.2) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

## All years included (bioclim)

### Correlations - bioclim Recent


``` r
#normalize the data
climate_normalized_all_bioclim_recent <- bioclim_recent_grwseason %>% select(ann_tmean:ppt_coldest_month) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_all_bioclim_recent)
```

```
##       ann_tmean mean_diurnal_range temp_seasonality temp_ann_range    ann_ppt
## [1,]  0.2761737          0.6138628       -1.0023399      0.3211031 -0.6686580
## [2,]  0.1726151         -1.1536434       -2.8805248     -2.1893221  0.8644685
## [3,]  0.5581316         -0.1385138       -1.5496956     -0.7530692  0.5970773
## [4,]  0.6144072          0.5212626       -0.6889491     -0.5036360 -0.4335098
## [5,] -0.4226294         -1.3041187       -2.4294195     -1.1754965  0.9673067
## [6,] -0.1403814          0.6809979       -1.6095230     -0.5398440 -0.5508902
##      ppt_seasonality tmean_wettest_month tmean_driest_month ppt_warmest_month
## [1,]      -1.7110673           0.8149577          0.8620499       -0.52448175
## [2,]      -1.1689020           1.0816768          0.3661528        1.48792262
## [3,]      -0.7061789           1.0555976          0.8936812        0.20546501
## [4,]       2.3786780           0.9026787          0.5783886       -0.36461375
## [5,]      -1.5803150           0.7888785          0.2365666        2.67727029
## [6,]      -0.8224144           0.8019181         -1.9357891       -0.05541849
##      ppt_coldest_month
## [1,]       -1.08795478
## [2,]        0.17223809
## [3,]        0.57208939
## [4,]        0.49161263
## [5,]        0.04540278
## [6,]       -0.90709311
```

``` r
cor.norm = cor(climate_normalized_all_bioclim_recent) #test correlations among the traits
corrplot(cor.norm)
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

### PCA - bioclim Recent


``` r
all_bioclim_recent.pc = prcomp(bioclim_recent_grwseason[c(5:14)], scale = TRUE, center = TRUE) 

str(all_bioclim_recent.pc)
```

```
## List of 5
##  $ sdev    : num [1:10] 1.721 1.521 1.35 0.961 0.894 ...
##  $ rotation: num [1:10, 1:10] -0.4729 0.0836 -0.0875 -0.2766 -0.4374 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:10] "ann_tmean" "mean_diurnal_range" "temp_seasonality" "temp_ann_range" ...
##   .. ..$ : chr [1:10] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:10] 12 13.2 6.48 29.8 545.65 ...
##   ..- attr(*, "names")= chr [1:10] "ann_tmean" "mean_diurnal_range" "temp_seasonality" "temp_ann_range" ...
##  $ scale   : Named num [1:10] 2.462 1.234 0.906 2.486 335.576 ...
##   ..- attr(*, "names")= chr [1:10] "ann_tmean" "mean_diurnal_range" "temp_seasonality" "temp_ann_range" ...
##  $ x       : num [1:690, 1:10] -0.152 0.235 -1.008 -0.693 0.644 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:10] "PC1" "PC2" "PC3" "PC4" ...
##  - attr(*, "class")= chr "prcomp"
```

plot % Variance Explained

``` r
summary(all_bioclim_recent.pc)
```

```
## Importance of components:
##                           PC1    PC2    PC3    PC4     PC5     PC6     PC7
## Standard deviation     1.7209 1.5213 1.3502 0.9612 0.89351 0.74108 0.54910
## Proportion of Variance 0.2962 0.2314 0.1823 0.0924 0.07984 0.05492 0.03015
## Cumulative Proportion  0.2962 0.5276 0.7099 0.8023 0.88212 0.93704 0.96719
##                            PC8     PC9    PC10
## Standard deviation     0.43075 0.31743 0.20436
## Proportion of Variance 0.01855 0.01008 0.00418
## Cumulative Proportion  0.98575 0.99582 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:10,2,pad="0")),
       percent_var=all_bioclim_recent.pc$sdev[1:10]^2/sum(all_bioclim_recent.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

Combine PCs with metadata

``` r
all_bioclim_recent.pc.dat = data.frame(all_bioclim_recent.pc$x)

all_bioclim_recent_locs.pc = cbind(bioclim_recent_grwseason, all_bioclim_recent.pc.dat)

all_bioclim_recent_loadings = data.frame(varnames=rownames(all_bioclim_recent.pc$rotation), all_bioclim_recent.pc$rotation)
all_bioclim_recent_loadings
```

```
##                                varnames         PC1         PC2         PC3
## ann_tmean                     ann_tmean -0.47294969 -0.10769683 -0.18695274
## mean_diurnal_range   mean_diurnal_range  0.08355263  0.20062996 -0.39175403
## temp_seasonality       temp_seasonality -0.08752383  0.56123542 -0.01918244
## temp_ann_range           temp_ann_range -0.27663379  0.42980564 -0.32891011
## ann_ppt                         ann_ppt -0.43739126 -0.11375540  0.38785094
## ppt_seasonality         ppt_seasonality -0.02957250  0.41182770  0.28153119
## tmean_wettest_month tmean_wettest_month -0.23554729 -0.47610737 -0.26206924
## tmean_driest_month   tmean_driest_month -0.50922379 -0.05061960 -0.15030124
## ppt_warmest_month     ppt_warmest_month  0.26624536 -0.16681235  0.27372074
## ppt_coldest_month     ppt_coldest_month -0.32754730  0.09392366  0.55420724
##                              PC4         PC5          PC6         PC7
## ann_tmean            0.198121730  0.03884254  0.499322651 -0.06481732
## mean_diurnal_range   0.703035880 -0.26210066 -0.399345532  0.00351096
## temp_seasonality    -0.382382171 -0.20271266  0.156805591  0.29366880
## temp_ann_range      -0.097739404 -0.32267517  0.038230219  0.15221920
## ann_ppt             -0.009604171 -0.22563951 -0.321755149  0.11507947
## ppt_seasonality      0.470351467  0.43104495  0.404100954  0.03129445
## tmean_wettest_month  0.107799504  0.02619142  0.154677299  0.64333594
## tmean_driest_month  -0.061114330 -0.05107695 -0.002917302 -0.64372169
## ppt_warmest_month    0.175169026 -0.73458756  0.472222104 -0.11843381
## ppt_coldest_month    0.208222454 -0.07039990 -0.226714012  0.17281896
##                             PC8         PC9        PC10
## ann_tmean           -0.57567289  0.30520244  0.10457290
## mean_diurnal_range   0.02019280  0.26085679 -0.09162947
## temp_seasonality     0.20088565  0.56155529 -0.15214835
## temp_ann_range      -0.11009264 -0.67103872  0.17274594
## ann_ppt             -0.22368833 -0.09482031 -0.64692952
## ppt_seasonality      0.22692706 -0.21774610 -0.28292608
## tmean_wettest_month  0.44335953 -0.03483613 -0.03556411
## tmean_driest_month   0.54066586  0.04342030 -0.02456608
## ppt_warmest_month    0.12606652 -0.05016327 -0.02513013
## ppt_coldest_month    0.09698029  0.10558605  0.65315655
```


``` r
autoplot(all_bioclim_recent.pc, data = bioclim_recent_grwseason,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

PCs 3 and 4

``` r
autoplot(all_bioclim_recent.pc, data = bioclim_recent_grwseason,
         x=3, y=4,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

PCs 4 and 5

``` r
autoplot(all_bioclim_recent.pc, data = bioclim_recent_grwseason,
         x=4, y=5,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

### Correlations - bioclim Historical


``` r
#normalize the data
climate_normalized_all_bioclim_historical <- bioclim_historical_grwseason %>% select(ann_tmean:ppt_coldest_month) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_all_bioclim_historical)
```

```
##        ann_tmean mean_diurnal_range temp_seasonality temp_ann_range     ann_ppt
## [1,] 0.004943863         0.08496148       -1.3986162     0.06319998  0.05517251
## [2,] 0.088110769        -0.33804436       -1.6594862    -0.55943285  0.14087650
## [3,] 0.562453554         0.41853691       -0.2024013     0.65199407 -0.53185276
## [4,] 0.001805490        -0.24950825       -1.0257704     0.19517107  0.38003146
## [5,] 0.450592945         0.39081349       -0.5120826     1.26109141 -0.24422639
## [6,] 0.236511017        -0.15202911       -1.1052001    -0.85721378  0.94709053
##      ppt_seasonality tmean_wettest_month tmean_driest_month ppt_warmest_month
## [1,]     -0.39890083           0.8766686         -1.7026494       -0.01293049
## [2,]     -0.63479745           1.4648974          0.5239790       -0.50592456
## [3,]     -0.02319313           0.4300906          1.1153033       -0.48463092
## [4,]     -1.09093643           0.7025789          0.0762761        0.29073534
## [5,]     -1.26741830           0.2754643          1.3242973       -0.50592456
## [6,]     -0.33998990           0.4311719          0.3516332       -0.27910099
##      ppt_coldest_month
## [1,]        -0.1570492
## [2,]        -0.2029876
## [3,]        -0.6296645
## [4,]        -0.2734897
## [5,]        -0.7154342
## [6,]         0.5339937
```

``` r
cor.norm = cor(climate_normalized_all_bioclim_historical) #test correlations among the traits
corrplot(cor.norm)
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

### PCA - bioclim Historical


``` r
all_bioclim_historical.pc = prcomp(bioclim_historical_grwseason[c(5:14)], scale = TRUE, center = TRUE) 

str(all_bioclim_historical.pc)
```

```
## List of 5
##  $ sdev    : num [1:10] 1.749 1.412 1.352 0.989 0.932 ...
##  $ rotation: num [1:10, 1:10] -0.4563 -0.0359 -0.3388 -0.4612 -0.2615 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:10] "ann_tmean" "mean_diurnal_range" "temp_seasonality" "temp_ann_range" ...
##   .. ..$ : chr [1:10] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:10] 10.97 13.96 6.02 29.28 530.74 ...
##   ..- attr(*, "names")= chr [1:10] "ann_tmean" "mean_diurnal_range" "temp_seasonality" "temp_ann_range" ...
##  $ scale   : Named num [1:10] 2.788 1.398 0.929 2.955 318.538 ...
##   ..- attr(*, "names")= chr [1:10] "ann_tmean" "mean_diurnal_range" "temp_seasonality" "temp_ann_range" ...
##  $ x       : num [1:690, 1:10] 1.218 0.314 -0.978 0.365 -1.026 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:10] "PC1" "PC2" "PC3" "PC4" ...
##  - attr(*, "class")= chr "prcomp"
```

plot % Variance Explained

``` r
summary(all_bioclim_historical.pc)
```

```
## Importance of components:
##                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
## Standard deviation     1.7495 1.4116 1.3523 0.98939 0.93170 0.78260 0.57330
## Proportion of Variance 0.3061 0.1993 0.1829 0.09789 0.08681 0.06125 0.03287
## Cumulative Proportion  0.3061 0.5053 0.6882 0.78608 0.87289 0.93413 0.96700
##                            PC8     PC9    PC10
## Standard deviation     0.40079 0.32781 0.24884
## Proportion of Variance 0.01606 0.01075 0.00619
## Cumulative Proportion  0.98306 0.99381 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:10,2,pad="0")),
       percent_var=all_bioclim_historical.pc$sdev[1:10]^2/sum(all_bioclim_historical.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

Combine PCs with metadata

``` r
all_bioclim_historical.pc.dat = data.frame(all_bioclim_historical.pc$x)

all_bioclim_historical_locs.pc = cbind(bioclim_historical_grwseason, all_bioclim_historical.pc.dat)

all_bioclim_historical_loadings = data.frame(varnames=rownames(all_bioclim_historical.pc$rotation), all_bioclim_historical.pc$rotation)
all_bioclim_historical_loadings
```

```
##                                varnames         PC1         PC2        PC3
## ann_tmean                     ann_tmean -0.45629135  0.23710220 -0.1730209
## mean_diurnal_range   mean_diurnal_range -0.03589931  0.20759188  0.3758794
## temp_seasonality       temp_seasonality -0.33883556 -0.21787815  0.3895847
## temp_ann_range           temp_ann_range -0.46120137  0.09915378  0.2911962
## ann_ppt                         ann_ppt -0.26150039 -0.36316478 -0.4703598
## ppt_seasonality         ppt_seasonality -0.17766595 -0.35032818  0.2834721
## tmean_wettest_month tmean_wettest_month -0.13189421  0.45294850 -0.4112170
## tmean_driest_month   tmean_driest_month -0.47786757  0.14056739 -0.1880886
## ppt_warmest_month     ppt_warmest_month  0.29109467 -0.16326031 -0.1921435
## ppt_coldest_month     ppt_coldest_month -0.18043268 -0.58068222 -0.2218475
##                             PC4         PC5         PC6          PC7
## ann_tmean            0.12434285 -0.03245174  0.40551041 -0.156410516
## mean_diurnal_range   0.53489661  0.60658285 -0.25765421 -0.096763322
## temp_seasonality    -0.43409344  0.03307222  0.12688949  0.344888005
## temp_ann_range      -0.16920098  0.26617964 -0.04672939  0.288299147
## ann_ppt             -0.01975146  0.20633185 -0.29116405  0.087176266
## ppt_seasonality      0.53514726 -0.32790971  0.44845408 -0.022024384
## tmean_wettest_month  0.27381442 -0.05439636  0.10431513  0.610768181
## tmean_driest_month  -0.14253241  0.07018660  0.01233685 -0.609538251
## ppt_warmest_month   -0.13791284  0.62464646  0.65208778  0.008220436
## ppt_coldest_month    0.28333001  0.10436874 -0.16956167  0.106683996
##                             PC8         PC9         PC10
## ann_tmean            0.58793938 -0.36611183  0.143236626
## mean_diurnal_range  -0.05985265 -0.25779750 -0.120348170
## temp_seasonality    -0.26628343 -0.52867158 -0.104705626
## temp_ann_range       0.22139721  0.66023638  0.151516214
## ann_ppt              0.20466132  0.00912860 -0.633389142
## ppt_seasonality     -0.13934819  0.23220143 -0.310659997
## tmean_wettest_month -0.38221070 -0.01468383 -0.013739820
## tmean_driest_month  -0.55382736  0.11211205  0.016777639
## ppt_warmest_month   -0.08749058  0.09769600 -0.001591372
## ppt_coldest_month   -0.09514630 -0.08812938  0.657962699
```


``` r
autoplot(all_bioclim_historical.pc, data = bioclim_historical_grwseason,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

PCs 3 and 4

``` r
autoplot(all_bioclim_historical.pc, data = bioclim_historical_grwseason,
         x=3, y=4,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

PCs 4 and 5

``` r
autoplot(all_bioclim_historical.pc, data = bioclim_historical_grwseason,
         x=4, y=5,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

## Monthly Averages - Flint  

### Calculate avgs


``` r
flint_recent_grwseason_mosavgs <- flint_recent_grwseason %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long, month) %>% 
  summarise_at(c("cwd",  "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Recent") 
flint_recent_grwseason_mosavgs
```

```
## # A tibble: 166 × 12
## # Groups:   parent.pop, elevation.group, elev_m, Lat, Long [23]
##    parent.pop elevation.group elev_m   Lat  Long month   cwd   pck    ppt   tmn
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
##  1 BH         Low               511.  37.4 -120.     1  29.4     0 124.    2.81
##  2 BH         Low               511.  37.4 -120.     2  41.0     0  93.9   3.32
##  3 BH         Low               511.  37.4 -120.     3  53.9     0  90.1   4.82
##  4 BH         Low               511.  37.4 -120.     4  59.0     0  48.2   6.41
##  5 BH         Low               511.  37.4 -120.     5  51.4     0  23.2   9.78
##  6 BH         Low               511.  37.4 -120.     6  89.3     0   6.34 13.6 
##  7 BH         Low               511.  37.4 -120.    12  30.0     0 111.    2.59
##  8 CC         Low               313   39.6 -121.     1  19.6     0 184.    4.27
##  9 CC         Low               313   39.6 -121.     2  31.4     0 165.    4.81
## 10 CC         Low               313   39.6 -121.     3  45.3     0 148.    6.18
## # ℹ 156 more rows
## # ℹ 2 more variables: tmx <dbl>, TimePd <chr>
```

``` r
flint_historical_grwseason_mosavgs <- flint_historical_grwseason %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long, month) %>% 
  summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Historical")
flint_historical_grwseason_mosavgs
```

```
## # A tibble: 154 × 12
## # Groups:   parent.pop, elevation.group, elev_m, Lat, Long [23]
##    parent.pop elevation.group elev_m   Lat  Long month   cwd   pck    ppt   tmn
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
##  1 BH         Low               511.  37.4 -120.     1  28.0 0.234 104.    1.46
##  2 BH         Low               511.  37.4 -120.     2  40.4 0      92.2   2.76
##  3 BH         Low               511.  37.4 -120.     3  51.2 0     101.    4.04
##  4 BH         Low               511.  37.4 -120.     4  62.2 0      46.4   5.57
##  5 BH         Low               511.  37.4 -120.     5  63.5 0      12.6   8.83
##  6 BH         Low               511.  37.4 -120.     6  87.5 0       5.73 12.4 
##  7 BH         Low               511.  37.4 -120.    11  43.8 0      82.0   4.37
##  8 BH         Low               511.  37.4 -120.    12  28.5 0      89.3   1.44
##  9 CC         Low               313   39.6 -121.     1  18.7 0.951 191.    2.42
## 10 CC         Low               313   39.6 -121.     2  31.1 0     146.    4.04
## # ℹ 144 more rows
## # ℹ 2 more variables: tmx <dbl>, TimePd <chr>
```

``` r
flint_grwseason_mosavgs <- bind_rows(flint_recent_grwseason_mosavgs, flint_historical_grwseason_mosavgs) #combine into 1 dataframe 
head(flint_grwseason_mosavgs)
```

```
## # A tibble: 6 × 12
## # Groups:   parent.pop, elevation.group, elev_m, Lat, Long [1]
##   parent.pop elevation.group elev_m   Lat  Long month   cwd   pck    ppt   tmn
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
## 1 BH         Low               511.  37.4 -120.     1  29.4     0 124.    2.81
## 2 BH         Low               511.  37.4 -120.     2  41.0     0  93.9   3.32
## 3 BH         Low               511.  37.4 -120.     3  53.9     0  90.1   4.82
## 4 BH         Low               511.  37.4 -120.     4  59.0     0  48.2   6.41
## 5 BH         Low               511.  37.4 -120.     5  51.4     0  23.2   9.78
## 6 BH         Low               511.  37.4 -120.     6  89.3     0   6.34 13.6 
## # ℹ 2 more variables: tmx <dbl>, TimePd <chr>
```

``` r
tail(flint_grwseason_mosavgs)
```

```
## # A tibble: 6 × 12
## # Groups:   parent.pop, elevation.group, elev_m, Lat, Long [2]
##   parent.pop elevation.group elev_m   Lat  Long month   cwd   pck   ppt    tmn
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
## 1 YO7        High             2470.  37.8 -120.    11  33.2  82.1 149.  -5.04 
## 2 YO8        High             2591.  37.8 -119.     7 117.    0    15.7  6.14 
## 3 YO8        High             2591.  37.8 -119.     8 113.    0    14.1  6.00 
## 4 YO8        High             2591.  37.8 -119.     9  98.1   0    32.5  2.92 
## 5 YO8        High             2591.  37.8 -119.    10  71.9   0    64.0 -0.564
## 6 YO8        High             2591.  37.8 -119.    11  31.9  88.9 148.  -5.77 
## # ℹ 2 more variables: tmx <dbl>, TimePd <chr>
```

### Correlations - Recent + Historical


``` r
#normalize the data
climate_normalized_flint_grwseason_mosavgs <- flint_grwseason_mosavgs %>% ungroup() %>% 
  select(cwd:tmx) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_flint_grwseason_mosavgs)
```

```
##               cwd        pck        ppt         tmn        tmx
## [1,] -0.921478463 -0.3466925  0.6370523 -0.46312448 -0.6766390
## [2,] -0.564920883 -0.3466925  0.2242746 -0.36707575 -0.5376371
## [3,] -0.165578858 -0.3466925  0.1721933 -0.08609735 -0.1940932
## [4,] -0.008743628 -0.3466925 -0.3952891  0.20996462  0.2278868
## [5,] -0.242892723 -0.3466925 -0.7334882  0.84073244  0.9548144
## [6,]  0.923602028 -0.3466925 -0.9618371  1.55259007  1.7153641
```

``` r
cor.norm = cor(climate_normalized_flint_grwseason_mosavgs) #test correlations among the traits
corrplot(cor.norm)
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

``` r
#tmn and tmx highly correlated, consider removing one (96%)
#tmx and ppt highly neg correlated (-82%)
```

### PCA - Recent + Historical


``` r
#flint_grwseason_mosavgs[c(8:12)]
mos_flint.pc = prcomp(flint_grwseason_mosavgs[c(7:11)], scale = TRUE, center = TRUE)

str(mos_flint.pc)
```

```
## List of 5
##  $ sdev    : num [1:5] 1.923 0.813 0.673 0.403 0.163
##  $ rotation: num [1:5, 1:5] 0.408 -0.369 -0.478 0.475 0.493 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##   .. ..$ : chr [1:5] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:5] 59.31 15.21 77.36 5.28 18.82
##   ..- attr(*, "names")= chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##  $ scale   : Named num [1:5] 32.47 43.87 73.85 5.35 7.24
##   ..- attr(*, "names")= chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##  $ x       : num [1:320, 1:5] -1.106 -0.649 -0.159 0.526 1.25 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:5] "PC1" "PC2" "PC3" "PC4" ...
##  - attr(*, "class")= chr "prcomp"
```

plot % Variance Explained

``` r
summary(mos_flint.pc)
```

```
## Importance of components:
##                           PC1    PC2     PC3    PC4     PC5
## Standard deviation     1.9230 0.8129 0.67265 0.4031 0.16259
## Proportion of Variance 0.7396 0.1322 0.09049 0.0325 0.00529
## Cumulative Proportion  0.7396 0.8717 0.96222 0.9947 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:5,2,pad="0")),
       percent_var=mos_flint.pc$sdev[1:5]^2/sum(mos_flint.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

Combine PCs with metadata

``` r
mos_flint.pc.dat = data.frame(mos_flint.pc$x)

mos_flint_locs.pc = cbind(flint_grwseason_mosavgs, mos_flint.pc.dat)

mos_flint_loadings = data.frame(varnames=rownames(mos_flint.pc$rotation), mos_flint.pc$rotation)
mos_flint_loadings
```

```
##     varnames        PC1         PC2        PC3         PC4         PC5
## cwd      cwd  0.4079383 -0.62441458  0.4356779 -0.50282739  0.03228327
## pck      pck -0.3689755 -0.73758843 -0.5429174  0.14952145  0.05200364
## ppt      ppt -0.4784822  0.20391233 -0.1918978 -0.81764612 -0.15525646
## tmn      tmn  0.4747355  0.15600497 -0.5378861 -0.23370847  0.63745394
## tmx      tmx  0.4930765  0.01232635 -0.4350623 -0.04053562 -0.75219766
```


``` r
autoplot(mos_flint.pc, data = flint_grwseason_mosavgs,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="month",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-35-1.png)<!-- -->

``` r
#high elev seems most similar to low elev in summer months 
```



``` r
mos_flint_locs.pc_avg <- mos_flint_locs.pc %>%
  group_by(parent.pop, elev_m, TimePd,month) %>%
  summarise(across(.cols=starts_with("PC"), .fns = mean)) %>%
  ungroup()
```

```
## `summarise()` has grouped output by 'parent.pop', 'elev_m', 'TimePd'. You can
## override using the `.groups` argument.
```

``` r
mos_flint_locs.pc_avg
```

```
## # A tibble: 320 × 10
##    parent.pop elev_m TimePd    month   pck    PC1    PC2    PC3     PC4      PC5
##    <chr>       <dbl> <chr>     <dbl> <dbl>  <dbl>  <dbl>  <dbl>   <dbl>    <dbl>
##  1 BH           511. Historic…     1 0.234 -1.20   0.804  0.458  0.342   0.0917 
##  2 BH           511. Historic…     2 0     -0.691  0.580  0.381  0.209   0.0297 
##  3 BH           511. Historic…     3 0     -0.394  0.437  0.281 -0.121   0.0158 
##  4 BH           511. Historic…     4 0      0.476  0.125  0.204  0.227  -0.0453 
##  5 BH           511. Historic…     5 0      1.39   0.113 -0.364  0.407  -0.179  
##  6 BH           511. Historic…     6 0      2.38  -0.257 -0.670 -0.0708 -0.220  
##  7 BH           511. Historic…    11 0     -0.273  0.538  0.143  0.184  -0.00600
##  8 BH           511. Historic…    12 0     -1.10   0.759  0.503  0.496   0.113  
##  9 BH           511. Recent        1 0     -1.11   0.880  0.208  0.0263  0.0671 
## 10 BH           511. Recent        2 0     -0.649  0.590  0.330  0.156   0.0993 
## # ℹ 310 more rows
```


``` r
mos_flint_locs.pc_avg %>% 
  ggplot(aes(x=PC1, y=PC2, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point() +
  facet_wrap(~month) +
  coord_fixed(ratio = 1.5)
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

``` r
#as I suspected from the raw PC figure, high elev pop cliamte is most similar to low elev climate in the summer months 
#hard to see differences between recent and historical with this fig though
```


``` r
mos_flint_locs.pc_avg %>% 
  mutate(group=str_c(parent.pop,elev_m, month))  %>%
  ggplot(aes(x=PC1, y=PC2, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point() +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")))
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-38-1.png)<!-- -->

``` r
#high elev climate seems to be shifting to be more similar to low elev 
#hard to see which months though 
```

## Avg across years and months (Flint)

### Calculate avgs


``` r
flint_recent_grwseason_avgs <- flint_recent_grwseason %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("cwd",  "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Recent") 
flint_recent_grwseason_avgs
```

```
## # A tibble: 23 × 11
## # Groups:   parent.pop, elevation.group, elev_m, Lat [23]
##    parent.pop elevation.group elev_m   Lat  Long   cwd    pck   ppt   tmn   tmx
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
##  1 BH         Low               511.  37.4 -120.  50.6  0      71.0  6.19  19.6
##  2 CC         Low               313   39.6 -121.  48.4  0     105.   8.56  21.0
##  3 CP2        High             2244.  38.7 -120.  75.1 41.8    78.2  3.94  16.5
##  4 CP3        High             2266.  38.7 -120.  57.8 44.3    75.4  3.36  15.7
##  5 DPR        Mid              1019.  39.2 -121.  30.4  2.72   96.9  9.06  21.9
##  6 FR         Mid               787   40.0 -121.  89.7  4.01   58.1  7.37  22.9
##  7 IH         Low               454.  39.1 -121.  40.5  0.201 100.   8.07  21.2
##  8 LV1        High             2593.  40.5 -122.  65.5 28.0    73.6  2.35  16.4
##  9 LV3        High             2354.  40.5 -122.  53.6 28.2    72.7  2.31  16.4
## 10 LVTR1      High             2741.  40.5 -122.  73.7 29.0    76.8  2.03  16.3
## # ℹ 13 more rows
## # ℹ 1 more variable: TimePd <chr>
```

``` r
flint_historical_grwseason_avgs <- flint_historical_grwseason %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Historical")
flint_historical_grwseason_avgs
```

```
## # A tibble: 23 × 11
## # Groups:   parent.pop, elevation.group, elev_m, Lat [23]
##    parent.pop elevation.group elev_m   Lat  Long   cwd     pck   ppt   tmn   tmx
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl>
##  1 BH         Low               511.  37.4 -120.  50.6  0.0292  66.6  5.11  18.8
##  2 CC         Low               313   39.6 -121.  42.5  0.119  112.   6.36  18.9
##  3 CP2        High             2244.  38.7 -120.  79.7 18.4     69.0  3.99  17.6
##  4 CP3        High             2266.  38.7 -120.  60.3 20.7     67.1  3.54  16.8
##  5 DPR        Mid              1019.  39.2 -121.  30.0  5.13    82.5  8.02  22.2
##  6 FR         Mid               787   40.0 -121.  87.8  4.54    57.8  6.15  23.0
##  7 IH         Low               454.  39.1 -121.  41.5  1.77    97.9  7.07  20.9
##  8 LV1        High             2593.  40.5 -122.  55.5 61.7    127.  -1.16  13.9
##  9 LV3        High             2354.  40.5 -122.  43.8 62.3    125.  -1.20  13.9
## 10 LVTR1      High             2741.  40.5 -122.  60.4 64.5    133.  -1.35  13.7
## # ℹ 13 more rows
## # ℹ 1 more variable: TimePd <chr>
```

``` r
flint_grwseason_avgs <- bind_rows(flint_recent_grwseason_avgs, flint_historical_grwseason_avgs) #combine into 1 dataframe 
head(flint_grwseason_avgs)
```

```
## # A tibble: 6 × 11
## # Groups:   parent.pop, elevation.group, elev_m, Lat [6]
##   parent.pop elevation.group elev_m   Lat  Long   cwd   pck   ppt   tmn   tmx
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 BH         Low               511.  37.4 -120.  50.6  0     71.0  6.19  19.6
## 2 CC         Low               313   39.6 -121.  48.4  0    105.   8.56  21.0
## 3 CP2        High             2244.  38.7 -120.  75.1 41.8   78.2  3.94  16.5
## 4 CP3        High             2266.  38.7 -120.  57.8 44.3   75.4  3.36  15.7
## 5 DPR        Mid              1019.  39.2 -121.  30.4  2.72  96.9  9.06  21.9
## 6 FR         Mid               787   40.0 -121.  89.7  4.01  58.1  7.37  22.9
## # ℹ 1 more variable: TimePd <chr>
```

``` r
tail(flint_grwseason_avgs)
```

```
## # A tibble: 6 × 11
## # Groups:   parent.pop, elevation.group, elev_m, Lat [6]
##   parent.pop elevation.group elev_m   Lat  Long   cwd   pck   ppt    tmn   tmx
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
## 1 WR         Mid              1158   39.3 -121.  50.2 10.8   89.3  7.59   21.8
## 2 WV         Mid               749.  40.7 -123.  57.9  9.49  62.0  6.09   22.5
## 3 YO11       High             2872.  37.9 -119.  71.8 15.9   42.1 -0.346  14.6
## 4 YO4        High             2158.  37.8 -120.  62.0 28.6   64.2  3.94   16.9
## 5 YO7        High             2470.  37.8 -120.  67.9 16.4   55.2  2.43   16.5
## 6 YO8        High             2591.  37.8 -119.  86.4 17.8   54.9  1.74   16.1
## # ℹ 1 more variable: TimePd <chr>
```

``` r
write_csv(flint_grwseason_avgs, "../output/Climate/growthseason_FlintAvgs.csv")
```

### Correlations - Recent + Historical


``` r
#normalize the data
climate_normalized_flint_grwseason_avgs <- flint_grwseason_avgs %>% ungroup() %>% 
  select(cwd:tmx) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_flint_grwseason_avgs)
```

```
##             cwd        pck         ppt        tmn        tmx
## [1,] -0.6172020 -1.0311639 -0.23764779  0.5500615  0.5166783
## [2,] -0.7518415 -1.0311639  1.12860553  1.4255887  1.0406251
## [3,]  0.8805006  1.3254429  0.05282515 -0.2773442 -0.6693810
## [4,] -0.1778697  1.4692660 -0.06106440 -0.4937444 -0.9718274
## [5,] -1.8504094 -0.8776851  0.80338679  1.6105900  1.3922648
## [6,]  1.7668993 -0.8050003 -0.75501629  0.9867385  1.7718521
```

``` r
cor.norm = cor(climate_normalized_flint_grwseason_avgs) #test correlations among the traits
corrplot(cor.norm)
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-40-1.png)<!-- -->

``` r
#tmn and tmx highly correlated, consider removing one (90%)
```

### PCA - Recent + Historical


``` r
avgs_flint.pc = prcomp(flint_grwseason_avgs[c(6:10)], scale = TRUE, center = TRUE)

str(avgs_flint.pc)
```

```
## List of 5
##  $ sdev    : num [1:5] 1.662 1.289 0.569 0.404 0.299
##  $ rotation: num [1:5, 1:5] 0.2722 0.5299 -0.0457 -0.5749 -0.5591 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##   .. ..$ : chr [1:5] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:5] 60.7 18.3 76.9 4.7 18.3
##   ..- attr(*, "names")= chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##  $ scale   : Named num [1:5] 16.4 17.72 24.88 2.71 2.62
##   ..- attr(*, "names")= chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##  $ x       : num [1:46, 1:5] -1.31 -2.2 1.47 1.56 -2.71 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:5] "PC1" "PC2" "PC3" "PC4" ...
##  - attr(*, "class")= chr "prcomp"
```

plot % Variance Explained

``` r
summary(avgs_flint.pc)
```

```
## Importance of components:
##                           PC1    PC2     PC3     PC4     PC5
## Standard deviation     1.6617 1.2893 0.56913 0.40400 0.29852
## Proportion of Variance 0.5523 0.3325 0.06478 0.03264 0.01782
## Cumulative Proportion  0.5523 0.8848 0.94953 0.98218 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:5,2,pad="0")),
       percent_var=avgs_flint.pc$sdev[1:5]^2/sum(avgs_flint.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-42-1.png)<!-- -->

Combine PCs with metadata

``` r
avgs_flint.pc.dat = data.frame(avgs_flint.pc$x)

avgs_flint_locs.pc = cbind(flint_grwseason_avgs, avgs_flint.pc.dat)

avgs_flint_loadings = data.frame(varnames=rownames(avgs_flint.pc$rotation), avgs_flint.pc$rotation)
avgs_flint_loadings
```

```
##     varnames         PC1         PC2       PC3        PC4         PC5
## cwd      cwd  0.27216688  0.63669484 0.4813808 -0.5293287  0.09288984
## pck      pck  0.52989950 -0.18977160 0.5783539  0.5846822  0.08274511
## ppt      ppt -0.04572174 -0.73485549 0.3688738 -0.5664219  0.03154816
## tmn      tmn -0.57486538  0.07286451 0.2712605  0.1702671  0.74943140
## tmx      tmx -0.55908052  0.11525882 0.4734229  0.1677293 -0.64952445
```


``` r
autoplot(avgs_flint.pc, data = flint_grwseason_avgs,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-44-1.png)<!-- -->


``` r
avgs_flint_locs.pc_avg <- avgs_flint_locs.pc %>%
  group_by(parent.pop, elev_m, TimePd) %>%
  summarise(across(.cols=starts_with("PC"), .fns = mean)) %>%
  ungroup()
```

```
## `summarise()` has grouped output by 'parent.pop', 'elev_m'. You can override
## using the `.groups` argument.
```

``` r
avgs_flint_locs.pc_avg
```

```
## # A tibble: 46 × 9
##    parent.pop elev_m TimePd         pck    PC1     PC2      PC3     PC4     PC5
##    <chr>       <dbl> <chr>        <dbl>  <dbl>   <dbl>    <dbl>   <dbl>   <dbl>
##  1 BH           511. Historical  0.0292 -0.899  0.144  -0.902    0.0187 -0.179 
##  2 BH           511. Recent      0      -1.31   0.0770 -0.587    0.0387 -0.0735
##  3 CC           313  Historical  0.119  -1.40  -1.49   -0.314   -0.673   0.153 
##  4 CC           313  Recent      0      -2.20  -0.889   0.337   -0.427   0.273 
##  5 CP2         2244. Historical 18.4     0.619  0.922   0.262   -0.513   0.0590
##  6 CP2         2244. Recent     41.8     1.47   0.173   0.818    0.119   0.420 
##  7 CP3         2266. Historical 20.7     0.639  0.151  -0.456    0.153   0.0383
##  8 CP3         2266. Recent     44.3     1.56  -0.495   0.148    0.741   0.364 
##  9 DPR         1019. Historical  5.13   -2.46  -0.952  -0.202    0.891  -0.288 
## 10 DPR         1019. Recent      2.72   -2.71  -1.32   -0.00600  0.519   0.0836
## # ℹ 36 more rows
```


``` r
avgs_flint_locs.pc_avg %>% 
  ggplot(aes(x=PC1, y=PC2, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point() +
  facet_wrap(~TimePd) +
  coord_fixed(ratio = 1.5)
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-46-1.png)<!-- -->


``` r
avgs_flint_locs.pc_avg %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC2, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-47-1.png)<!-- -->

``` r
avgs_flint_locs.pc_avg %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC2, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_text_repel(aes(label = parent.pop)) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed")  +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-47-2.png)<!-- -->

``` r
#growth season climate has shifted for high elev way more than for low elev & directions are varied 
```

## Avg across years and months (Flint + bioclim)

### Calculate avgs


``` r
bioclim_recent_grwseason_avgs <- bioclim_recent_grwseason %>% 
  group_by(parent.pop, elevation.group, elev_m) %>% 
  summarise_at(c("ann_tmean", "mean_diurnal_range", "temp_seasonality", "temp_ann_range",
                 "tmean_wettest_month", "tmean_driest_month", "ann_ppt",
                 "ppt_seasonality","ppt_warmest_month", "ppt_coldest_month"),
               c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Recent") 
bioclim_recent_grwseason_avgs
```

```
## # A tibble: 23 × 14
## # Groups:   parent.pop, elevation.group [23]
##    parent.pop elevation.group elev_m ann_tmean mean_diurnal_range
##    <chr>      <chr>            <dbl>     <dbl>              <dbl>
##  1 BH         Low               511.     12.9                13.4
##  2 CC         Low               313      14.8                12.4
##  3 CP2        High             2244.     10.2                12.6
##  4 CP3        High             2266.      9.54               12.4
##  5 DPR        Mid              1019.     15.5                12.9
##  6 FR         Mid               787      15.1                15.5
##  7 IH         Low               454.     14.7                13.2
##  8 LV1        High             2593.      9.38               14.1
##  9 LV3        High             2354.      9.37               14.1
## 10 LVTR1      High             2741.      9.16               14.3
## # ℹ 13 more rows
## # ℹ 9 more variables: temp_seasonality <dbl>, temp_ann_range <dbl>,
## #   tmean_wettest_month <dbl>, tmean_driest_month <dbl>, ann_ppt <dbl>,
## #   ppt_seasonality <dbl>, ppt_warmest_month <dbl>, ppt_coldest_month <dbl>,
## #   TimePd <chr>
```

``` r
bioclim_historical_grwseason_avgs <- bioclim_historical_grwseason %>% 
  group_by(parent.pop, elevation.group, elev_m) %>% 
  summarise_at(c("ann_tmean", "mean_diurnal_range", "temp_seasonality", "temp_ann_range",
                 "tmean_wettest_month", "tmean_driest_month", "ann_ppt",
                 "ppt_seasonality","ppt_warmest_month", "ppt_coldest_month"),
               c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Historical") 
bioclim_historical_grwseason_avgs
```

```
## # A tibble: 23 × 14
## # Groups:   parent.pop, elevation.group [23]
##    parent.pop elevation.group elev_m ann_tmean mean_diurnal_range
##    <chr>      <chr>            <dbl>     <dbl>              <dbl>
##  1 BH         Low               511.     12.0                13.7
##  2 CC         Low               313      12.6                12.6
##  3 CP2        High             2244.     10.8                13.7
##  4 CP3        High             2266.     10.2                13.3
##  5 DPR        Mid              1019.     15.1                14.2
##  6 FR         Mid               787      14.6                16.8
##  7 IH         Low               454.     14.0                13.8
##  8 LV1        High             2593.      6.36               15.0
##  9 LV3        High             2354.      6.35               15.1
## 10 LVTR1      High             2741.      6.19               15.1
## # ℹ 13 more rows
## # ℹ 9 more variables: temp_seasonality <dbl>, temp_ann_range <dbl>,
## #   tmean_wettest_month <dbl>, tmean_driest_month <dbl>, ann_ppt <dbl>,
## #   ppt_seasonality <dbl>, ppt_warmest_month <dbl>, ppt_coldest_month <dbl>,
## #   TimePd <chr>
```

``` r
bioclim_grwseason_avgs <- bind_rows(bioclim_recent_grwseason_avgs, bioclim_historical_grwseason_avgs) #combine into 1 dataframe 
head(bioclim_grwseason_avgs)
```

```
## # A tibble: 6 × 14
## # Groups:   parent.pop, elevation.group [6]
##   parent.pop elevation.group elev_m ann_tmean mean_diurnal_range
##   <chr>      <chr>            <dbl>     <dbl>              <dbl>
## 1 BH         Low               511.     12.9                13.4
## 2 CC         Low               313      14.8                12.4
## 3 CP2        High             2244.     10.2                12.6
## 4 CP3        High             2266.      9.54               12.4
## 5 DPR        Mid              1019.     15.5                12.9
## 6 FR         Mid               787      15.1                15.5
## # ℹ 9 more variables: temp_seasonality <dbl>, temp_ann_range <dbl>,
## #   tmean_wettest_month <dbl>, tmean_driest_month <dbl>, ann_ppt <dbl>,
## #   ppt_seasonality <dbl>, ppt_warmest_month <dbl>, ppt_coldest_month <dbl>,
## #   TimePd <chr>
```

``` r
tail(bioclim_grwseason_avgs)
```

```
## # A tibble: 6 × 14
## # Groups:   parent.pop, elevation.group [6]
##   parent.pop elevation.group elev_m ann_tmean mean_diurnal_range
##   <chr>      <chr>            <dbl>     <dbl>              <dbl>
## 1 WR         Mid              1158      14.7                14.2
## 2 WV         Mid               749.     14.3                16.4
## 3 YO11       High             2872.      7.14               15.0
## 4 YO4        High             2158.     10.4                12.9
## 5 YO7        High             2470.      9.46               14.1
## 6 YO8        High             2591.      8.92               14.4
## # ℹ 9 more variables: temp_seasonality <dbl>, temp_ann_range <dbl>,
## #   tmean_wettest_month <dbl>, tmean_driest_month <dbl>, ann_ppt <dbl>,
## #   ppt_seasonality <dbl>, ppt_warmest_month <dbl>, ppt_coldest_month <dbl>,
## #   TimePd <chr>
```

``` r
write_csv(bioclim_grwseason_avgs, "../output/Climate/growthseason_BioClimAvgs.csv")
```

Merge with flint

``` r
bioclim_flint_grwseason_avgs <- full_join(flint_grwseason_avgs, bioclim_grwseason_avgs) %>% 
  select(TimePd, parent.pop:ppt_coldest_month)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, TimePd)`
```

``` r
head(bioclim_flint_grwseason_avgs)
```

```
## # A tibble: 6 × 21
## # Groups:   parent.pop, elevation.group, elev_m, Lat [6]
##   TimePd parent.pop elevation.group elev_m   Lat  Long   cwd   pck   ppt   tmn
##   <chr>  <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 Recent BH         Low               511.  37.4 -120.  50.6  0     71.0  6.19
## 2 Recent CC         Low               313   39.6 -121.  48.4  0    105.   8.56
## 3 Recent CP2        High             2244.  38.7 -120.  75.1 41.8   78.2  3.94
## 4 Recent CP3        High             2266.  38.7 -120.  57.8 44.3   75.4  3.36
## 5 Recent DPR        Mid              1019.  39.2 -121.  30.4  2.72  96.9  9.06
## 6 Recent FR         Mid               787   40.0 -121.  89.7  4.01  58.1  7.37
## # ℹ 11 more variables: tmx <dbl>, ann_tmean <dbl>, mean_diurnal_range <dbl>,
## #   temp_seasonality <dbl>, temp_ann_range <dbl>, tmean_wettest_month <dbl>,
## #   tmean_driest_month <dbl>, ann_ppt <dbl>, ppt_seasonality <dbl>,
## #   ppt_warmest_month <dbl>, ppt_coldest_month <dbl>
```


### Correlations - Recent + Historical


``` r
#normalize the data
climate_normalized_bioclim_flint_grwseason_avgs <- bioclim_flint_grwseason_avgs %>% ungroup() %>% 
  select(cwd:ppt_coldest_month) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_bioclim_flint_grwseason_avgs)
```

```
##             cwd        pck         ppt        tmn        tmx  ann_tmean
## [1,] -0.6172020 -1.0311639 -0.23764779  0.5500615  0.5166783  0.5469031
## [2,] -0.7518415 -1.0311639  1.12860553  1.4255887  1.0406251  1.2672133
## [3,]  0.8805006  1.3254429  0.05282515 -0.2773442 -0.6693810 -0.4815115
## [4,] -0.1778697  1.4692660 -0.06106440 -0.4937444 -0.9718274 -0.7465804
## [5,] -1.8504094 -0.8776851  0.80338679  1.6105900  1.3922648  1.5406620
## [6,]  1.7668993 -0.8050003 -0.75501629  0.9867385  1.7718521  1.4063113
##      mean_diurnal_range temp_seasonality temp_ann_range tmean_wettest_month
## [1,]         -0.1195332       -1.1209686    -0.07197142           1.3220079
## [2,]         -0.9777971        0.2176292     0.90627987           1.5057615
## [3,]         -0.8548960        1.8467349     0.26777755          -1.0659034
## [4,]         -1.0298625        1.9066012     0.24520507          -1.3111842
## [5,]         -0.6198840        0.6320417     0.64752626           0.7558527
## [6,]          1.6769223        0.7924384     1.93598312           0.6274792
##      tmean_driest_month    ann_ppt ppt_seasonality ppt_warmest_month
## [1,]         0.83013479 -0.1784713      -1.2746462        -0.3680526
## [2,]         1.98990615  1.7683680      -0.9482063        -1.1973623
## [3,]        -0.08712226 -0.2986810       0.4900038        -0.2830821
## [4,]        -0.24044817 -0.3725666       0.3468043        -0.1547530
## [5,]         1.54258744  1.8731054       0.5325080        -1.3291333
## [6,]         1.29917434 -0.0646273       0.5488315        -1.1246699
##      ppt_coldest_month
## [1,]       -0.76904922
## [2,]        1.18650529
## [3,]        0.21409241
## [4,]        0.02843811
## [5,]        2.02788360
## [6,]       -0.23405993
```

``` r
cor.norm = cor(climate_normalized_bioclim_flint_grwseason_avgs) #test correlations among the traits
cor.norm
```

```
##                              cwd         pck          ppt          tmn
## cwd                  1.000000000  0.23774346 -0.705436341 -0.321135064
## pck                  0.237743464  1.00000000  0.180204389 -0.791573862
## ppt                 -0.705436341  0.18020439  1.000000000  0.002345286
## tmn                 -0.321135064 -0.79157386  0.002345286  1.000000000
## tmx                 -0.244239688 -0.75453631 -0.030980200  0.904342122
## ann_tmean           -0.290402190 -0.79256589 -0.014367324  0.976660759
## mean_diurnal_range   0.198275736  0.14738775 -0.074783031 -0.295576646
## temp_seasonality     0.318540703  0.18692344 -0.359281428  0.212597043
## temp_ann_range       0.005967133 -0.36067657 -0.164934870  0.594149922
## tmean_wettest_month -0.564812550 -0.84264668  0.299482004  0.783527645
## tmean_driest_month  -0.446002154 -0.69884120  0.212730550  0.923049105
## ann_ppt             -0.764781593 -0.41977866  0.735711695  0.616211091
## ppt_seasonality      0.474607306  0.21848579 -0.488168824  0.113209481
## ppt_warmest_month    0.013355561  0.57475767  0.325364952 -0.826857051
## ppt_coldest_month   -0.669308511  0.02795972  0.846232303  0.359770146
##                            tmx   ann_tmean mean_diurnal_range temp_seasonality
## cwd                 -0.2442397 -0.29040219        0.198275736      0.318540703
## pck                 -0.7545363 -0.79256589        0.147387747      0.186923444
## ppt                 -0.0309802 -0.01436732       -0.074783031     -0.359281428
## tmn                  0.9043421  0.97666076       -0.295576646      0.212597043
## tmx                  1.0000000  0.97490864        0.140435682      0.216598637
## ann_tmean            0.9749086  1.00000000       -0.083487324      0.219883173
## mean_diurnal_range   0.1404357 -0.08348732        1.000000000     -0.008362923
## temp_seasonality     0.2165986  0.21988317       -0.008362923      1.000000000
## temp_ann_range       0.7638816  0.69430075        0.331504402      0.629673853
## tmean_wettest_month  0.7651390  0.79370506       -0.104990551     -0.341990317
## tmean_driest_month   0.9205245  0.94467002       -0.080848873      0.201868335
## ann_ppt              0.5391320  0.59270359       -0.222725893     -0.107209424
## ppt_seasonality      0.1286005  0.12376247        0.025226748      0.728700508
## ppt_warmest_month   -0.7642327 -0.81584773        0.207532806     -0.578408441
## ppt_coldest_month    0.3217704  0.34956933       -0.114363623      0.043254214
##                     temp_ann_range tmean_wettest_month tmean_driest_month
## cwd                    0.005967133          -0.5648126        -0.44600215
## pck                   -0.360676569          -0.8426467        -0.69884120
## ppt                   -0.164934870           0.2994820         0.21273055
## tmn                    0.594149922           0.7835276         0.92304911
## tmx                    0.763881582           0.7651390         0.92052446
## ann_tmean              0.694300753           0.7937051         0.94467002
## mean_diurnal_range     0.331504402          -0.1049906        -0.08084887
## temp_seasonality       0.629673853          -0.3419903         0.20186833
## temp_ann_range         1.000000000           0.3449920         0.71014401
## tmean_wettest_month    0.344991950           1.0000000         0.82354620
## tmean_driest_month     0.710144009           0.8235462         1.00000000
## ann_ppt                0.320155603           0.7276664         0.75313122
## ppt_seasonality        0.307197738          -0.4463967        -0.04057608
## ppt_warmest_month     -0.760271119          -0.4394938        -0.77055498
## ppt_coldest_month      0.194979987           0.3611721         0.51759851
##                        ann_ppt ppt_seasonality ppt_warmest_month
## cwd                 -0.7647816      0.47460731        0.01335556
## pck                 -0.4197787      0.21848579        0.57475767
## ppt                  0.7357117     -0.48816882        0.32536495
## tmn                  0.6162111      0.11320948       -0.82685705
## tmx                  0.5391320      0.12860054       -0.76423267
## ann_tmean            0.5927036      0.12376247       -0.81584773
## mean_diurnal_range  -0.2227259      0.02522675        0.20753281
## temp_seasonality    -0.1072094      0.72870051       -0.57840844
## temp_ann_range       0.3201556      0.30719774       -0.76027112
## tmean_wettest_month  0.7276664     -0.44639665       -0.43949383
## tmean_driest_month   0.7531312     -0.04057608       -0.77055498
## ann_ppt              1.0000000     -0.37177534       -0.30947702
## ppt_seasonality     -0.3717753      1.00000000       -0.39284582
## ppt_warmest_month   -0.3094770     -0.39284582        1.00000000
## ppt_coldest_month    0.8460232     -0.07921628       -0.11536553
##                     ppt_coldest_month
## cwd                       -0.66930851
## pck                        0.02795972
## ppt                        0.84623230
## tmn                        0.35977015
## tmx                        0.32177042
## ann_tmean                  0.34956933
## mean_diurnal_range        -0.11436362
## temp_seasonality           0.04325421
## temp_ann_range             0.19497999
## tmean_wettest_month        0.36117208
## tmean_driest_month         0.51759851
## ann_ppt                    0.84602316
## ppt_seasonality           -0.07921628
## ppt_warmest_month         -0.11536553
## ppt_coldest_month          1.00000000
```

``` r
corrplot(cor.norm)
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-50-1.png)<!-- -->

``` r
#tmn, tmx, tmean_driest_month and ann_tmean all highly correlated (90-98%) - only keep ann_tmean 
```

### PCA - Recent + Historical


``` r
all_bioclim_flint_avgs.pc = prcomp(bioclim_flint_grwseason_avgs[c(7:8, 12:16, 18:21)], scale = TRUE, center = TRUE) 
str(all_bioclim_flint_avgs.pc)
```

```
## List of 5
##  $ sdev    : num [1:11] 2.113 1.737 1.24 1.079 0.612 ...
##  $ rotation: num [1:11, 1:11] 0.2868 0.3572 -0.4288 0.0831 -0.0152 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:11] "cwd" "pck" "ann_tmean" "mean_diurnal_range" ...
##   .. ..$ : chr [1:11] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:11] 60.7 18.27 11.48 13.58 6.25 ...
##   ..- attr(*, "names")= chr [1:11] "cwd" "pck" "ann_tmean" "mean_diurnal_range" ...
##  $ scale   : Named num [1:11] 16.396 17.718 2.6 1.169 0.643 ...
##   ..- attr(*, "names")= chr [1:11] "cwd" "pck" "ann_tmean" "mean_diurnal_range" ...
##  $ x       : num [1:46, 1:11] -1.26 -3.6 1.23 1.29 -3.74 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:11] "PC1" "PC2" "PC3" "PC4" ...
##  - attr(*, "class")= chr "prcomp"
```

plot % Variance Explained

``` r
summary(all_bioclim_flint_avgs.pc)
```

```
## Importance of components:
##                           PC1    PC2    PC3    PC4    PC5     PC6     PC7
## Standard deviation     2.1129 1.7374 1.2404 1.0786 0.6116 0.49865 0.29210
## Proportion of Variance 0.4058 0.2744 0.1399 0.1058 0.0340 0.02261 0.00776
## Cumulative Proportion  0.4058 0.6803 0.8202 0.9259 0.9599 0.98252 0.99028
##                            PC8     PC9    PC10    PC11
## Standard deviation     0.22791 0.19645 0.11435 0.05749
## Proportion of Variance 0.00472 0.00351 0.00119 0.00030
## Cumulative Proportion  0.99500 0.99851 0.99970 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:11,2,pad="0")),
       percent_var=all_bioclim_flint_avgs.pc$sdev[1:11]^2/sum(all_bioclim_flint_avgs.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-52-1.png)<!-- -->

Combine PCs with metadata

``` r
all_bioclim_flint_avgs.pc.dat = data.frame(all_bioclim_flint_avgs.pc$x)

all_bioclim_flint_avgs_locs.pc = cbind(bioclim_flint_grwseason_avgs, all_bioclim_flint_avgs.pc.dat)

all_bioclim_flint_avgs_loadings = data.frame(varnames=rownames(all_bioclim_flint_avgs.pc$rotation), all_bioclim_flint_avgs.pc$rotation)
all_bioclim_flint_avgs_loadings
```

```
##                                varnames         PC1          PC2         PC3
## cwd                                 cwd  0.28680303  0.327441196 -0.28822861
## pck                                 pck  0.35723241 -0.007749669  0.49142779
## ann_tmean                     ann_tmean -0.42881133  0.186494965 -0.13184196
## mean_diurnal_range   mean_diurnal_range  0.08310213  0.087754510 -0.10073156
## temp_seasonality       temp_seasonality -0.01522327  0.504889231  0.30633715
## temp_ann_range           temp_ann_range -0.27413141  0.386255302  0.02360241
## tmean_wettest_month tmean_wettest_month -0.42456104 -0.151653843 -0.25173020
## ann_ppt                         ann_ppt -0.40294787 -0.183172287  0.29359587
## ppt_seasonality         ppt_seasonality  0.08765410  0.476292970  0.19773503
## ppt_warmest_month     ppt_warmest_month  0.31792621 -0.380463775  0.08011007
## ppt_coldest_month     ppt_coldest_month -0.26941558 -0.134074734  0.59867431
##                              PC4         PC5         PC6          PC7
## cwd                  0.021611974  0.08840880 -0.84540413  0.020005889
## pck                 -0.105643314 -0.13523326 -0.05930551  0.593657240
## ann_tmean            0.009685354  0.27337339 -0.03341264  0.016970083
## mean_diurnal_range  -0.884009096  0.18444646  0.11393480 -0.008691253
## temp_seasonality     0.065839429 -0.37507093  0.06713824 -0.437971002
## temp_ann_range      -0.358100869 -0.35551709 -0.03451010  0.044178516
## tmean_wettest_month -0.051323581  0.10453701 -0.06890233  0.217741072
## ann_ppt             -0.033467656 -0.08533869 -0.31592323 -0.231965263
## ppt_seasonality      0.149342141  0.68736575  0.22298797 -0.056562592
## ppt_warmest_month   -0.196791872  0.15486522 -0.09276813 -0.588345102
## ppt_coldest_month   -0.097217039  0.28880121 -0.31479214  0.049685412
##                             PC8         PC9         PC10        PC11
## cwd                  0.01876541  0.02760019 -0.053242664  0.01050158
## pck                 -0.37673051 -0.01622169  0.035413379  0.31935238
## ann_tmean           -0.44716691  0.31898430  0.591071799  0.19144677
## mean_diurnal_range   0.20860091  0.26547524 -0.090221071  0.15673480
## temp_seasonality    -0.20463773  0.41032251 -0.309332278  0.06120350
## temp_ann_range      -0.14339441 -0.61557373  0.171611983 -0.29729914
## tmean_wettest_month -0.43142752 -0.03580975 -0.692148639  0.03432718
## ann_ppt              0.28671028 -0.23520489 -0.005875233  0.64676549
## ppt_seasonality      0.03566604 -0.35723252 -0.174122211  0.13949632
## ppt_warmest_month   -0.52149189 -0.23068786  0.056134054 -0.04432540
## ppt_coldest_month    0.10125460  0.20497754 -0.026352098 -0.55104041
```


``` r
autoplot(all_bioclim_flint_avgs.pc, data = bioclim_flint_grwseason_avgs,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-54-1.png)<!-- -->


``` r
autoplot(all_bioclim_flint_avgs.pc, data = bioclim_flint_grwseason_avgs,
         x=1, y=3,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-55-1.png)<!-- -->


``` r
autoplot(all_bioclim_flint_avgs.pc, data = bioclim_flint_grwseason_avgs,
         x=4, y=5,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-56-1.png)<!-- -->


``` r
all_bioclim_flint_avgs_locs.pc_avg <- all_bioclim_flint_avgs_locs.pc %>%
  group_by(parent.pop, elev_m, TimePd) %>%
  summarise(across(.cols=starts_with("PC"), .fns = mean)) %>%
  ungroup()
```

```
## `summarise()` has grouped output by 'parent.pop', 'elev_m'. You can override
## using the `.groups` argument.
```

``` r
all_bioclim_flint_avgs_locs.pc_avg
```

```
## # A tibble: 46 × 15
##    parent.pop elev_m TimePd        pck   PC1    PC2    PC3     PC4    PC5    PC6
##    <chr>       <dbl> <chr>       <dbl> <dbl>  <dbl>  <dbl>   <dbl>  <dbl>  <dbl>
##  1 BH           511. Historic…  0.0292 -1.28 -1.49  -2.16  -0.311  -0.639  0.392
##  2 BH           511. Recent     0      -1.26 -1.23  -1.86   0.0535 -0.343  0.436
##  3 CC           313  Historic…  0.119  -2.30 -3.04  -0.399  0.311  -0.106 -0.524
##  4 CC           313  Recent     0      -3.60 -0.335  0.296  0.501  -0.653 -0.610
##  5 CP2         2244. Historic… 18.4     1.64 -0.799 -1.17   0.301   0.687 -0.632
##  6 CP2         2244. Recent    41.8     1.23  1.68   1.50   0.829  -0.910 -0.554
##  7 CP3         2266. Historic… 20.7     1.65 -1.37  -0.734  0.574   0.386  0.366
##  8 CP3         2266. Recent    44.3     1.29  1.25   1.86   0.941  -1.29   0.381
##  9 DPR         1019. Historic…  5.13   -2.99  0.252  0.597 -0.633   0.663  1.15 
## 10 DPR         1019. Recent     2.72   -3.74  0.233  1.74   0.468   0.460  0.474
## # ℹ 36 more rows
## # ℹ 5 more variables: PC7 <dbl>, PC8 <dbl>, PC9 <dbl>, PC10 <dbl>, PC11 <dbl>
```


``` r
all_bioclim_flint_avgs_locs.pc_avg %>% 
  ggplot(aes(x=PC1, y=PC2, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point() +
  facet_wrap(~TimePd) +
  coord_fixed(ratio = 1.5)
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-58-1.png)<!-- -->


``` r
all_bioclim_flint_avgs_locs.pc_avg %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC2, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-59-1.png)<!-- -->

``` r
all_bioclim_flint_avgs_locs.pc_avg %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC2, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_text_repel(aes(label = parent.pop)) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed")  +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_Growth_Season_files/figure-html/unnamed-chunk-59-2.png)<!-- -->

``` r
#high elev climate seems to be shifting to be more similar to low elev 
```
