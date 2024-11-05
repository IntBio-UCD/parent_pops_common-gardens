---
title: "Flint_Growth_Season"
author: "Brandie Quarles"
date: "2024-11-05"
output: 
  html_document: 
    keep_md: yes
editor_options: 
  markdown: 
    wrap: 72
---




# Growth Season by snow/rain

-   First month with no snow pack or w/ significant rain as start of
    growth season

    -   This month isn't included in the final "growth season" - because
        we're saying how close are the conditions upon establishment

-   First month with snow pack or w/ significant CWD as end of growth
    season
    
    -   This month is included in the final "growth season" - because
        that month is likely a month when they're maturing seeds (esp at lower elevs)

Mean height at WL2 in Oct was 7 cm --\> 70 mm

## Notes: 
 - Need to come back and do this to include 2024 months for biennials 
 - Calculate grow season for gardens in the specific year of the garden (2022-2023)

## Relevant Libraries and Functions


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
library(cowplot)
```

```
## 
## Attaching package: 'cowplot'
## 
## The following object is masked from 'package:lubridate':
## 
##     stamp
```

``` r
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
library(naniar) #replaces values with NA
sem <- function(x, na.rm=FALSE) {
  sd(x,na.rm=na.rm)/sqrt(length(na.omit(x)))
} #standard error function 

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
} #legend function for grid_arrange

true_round <- function(number, digits) {
  posneg <- sign(number)
  number <- abs(number) * 10^digits
  number <- number + 0.5 + sqrt(.Machine$double.eps)
  number <- trunc(number)
  number <- number / 10 ^ digits
  number * posneg
} #rounding function that bypasses R's round to even default

elev_three_palette <- c("#0043F0", "#C9727F", "#F5A540") #colors from Gremer et al 2019
elev_order <- c("High", "Mid", "Low")
```

## Load the data (from "Climate_Prep.Rmd")


``` r
pop_elev_climate <- read_csv("../output/Climate/flint_climate_UCDpops.csv")
```

```
## Rows: 38675 Columns: 14
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (3): parent.pop, elevation.group, month
## dbl (11): elev_m, Lat, Long, year, aet, cwd, pck, pet, ppt, tmn, tmx
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

## Calculation of recent (last 30 years) and historical climate (prior 30 years)

For 2023 (annuals-only analysis)


``` r
unique(pop_elev_climate$parent.pop) #double check all 23 populations and garden sites are in the data set 
```

```
##  [1] "BH"         "CC"         "CP2"        "CP3"        "DPR"       
##  [6] "FR"         "IH"         "LV1"        "LV3"        "LVTR1"     
## [11] "SC"         "SQ1"        "SQ2"        "SQ3"        "TM2"       
## [16] "WL1"        "WL2"        "WR"         "WV"         "YO11"      
## [21] "YO4"        "YO7"        "YO8"        "UCD_Garden" "WL2_Garden"
```

``` r
pop_elev_climate_recent <- pop_elev_climate %>% 
  filter(parent.pop != "UCD_Garden", parent.pop != "WL2_Garden") %>% #remove garden sites since we don't need their historic growth seasons 
  filter(year>1993 & year<2024) %>% 
  select(parent.pop:month, cwd, pck, ppt, tmn, tmx) %>% 
  mutate(month = recode(month, #convert months to numbers 
  jan = 1,
  feb = 2,
  mar = 3,
  apr = 4,
  may = 5,
  jun = 6,
  jul = 7,
  aug = 8,
  sep = 9,
  oct = 10,
  nov = 11,
  dec = 12
))
head(pop_elev_climate_recent)
```

```
## # A tibble: 6 × 12
##   parent.pop elevation.group elev_m   Lat  Long  year month   cwd   pck   ppt
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 BH         Low               511.  37.4 -120.  1994     4  58.6     0  63.0
## 2 BH         Low               511.  37.4 -120.  1994     8 180.      0   0  
## 3 BH         Low               511.  37.4 -120.  1994    12  29.0     0  59.3
## 4 BH         Low               511.  37.4 -120.  1994     2  41.4     0  98.7
## 5 BH         Low               511.  37.4 -120.  1994     1  31.3     0  44.4
## 6 BH         Low               511.  37.4 -120.  1994     7 174.      0   0  
## # ℹ 2 more variables: tmn <dbl>, tmx <dbl>
```

``` r
xtabs(~parent.pop+month, data=pop_elev_climate_recent) #check that there's 30 years for all pops and months 
```

```
##           month
## parent.pop  1  2  3  4  5  6  7  8  9 10 11 12
##      BH    30 30 30 30 30 30 30 30 30 30 30 30
##      CC    30 30 30 30 30 30 30 30 30 30 30 30
##      CP2   30 30 30 30 30 30 30 30 30 30 30 30
##      CP3   30 30 30 30 30 30 30 30 30 30 30 30
##      DPR   30 30 30 30 30 30 30 30 30 30 30 30
##      FR    30 30 30 30 30 30 30 30 30 30 30 30
##      IH    30 30 30 30 30 30 30 30 30 30 30 30
##      LV1   30 30 30 30 30 30 30 30 30 30 30 30
##      LV3   30 30 30 30 30 30 30 30 30 30 30 30
##      LVTR1 30 30 30 30 30 30 30 30 30 30 30 30
##      SC    30 30 30 30 30 30 30 30 30 30 30 30
##      SQ1   30 30 30 30 30 30 30 30 30 30 30 30
##      SQ2   30 30 30 30 30 30 30 30 30 30 30 30
##      SQ3   30 30 30 30 30 30 30 30 30 30 30 30
##      TM2   30 30 30 30 30 30 30 30 30 30 30 30
##      WL1   30 30 30 30 30 30 30 30 30 30 30 30
##      WL2   30 30 30 30 30 30 30 30 30 30 30 30
##      WR    30 30 30 30 30 30 30 30 30 30 30 30
##      WV    30 30 30 30 30 30 30 30 30 30 30 30
##      YO11  30 30 30 30 30 30 30 30 30 30 30 30
##      YO4   30 30 30 30 30 30 30 30 30 30 30 30
##      YO7   30 30 30 30 30 30 30 30 30 30 30 30
##      YO8   30 30 30 30 30 30 30 30 30 30 30 30
```

``` r
unique(pop_elev_climate_recent$month)
```

```
##  [1]  4  8 12  2  1  7  6  3  5 11 10  9
```

``` r
pop_elev_climate_historical <- pop_elev_climate %>% 
  filter(parent.pop != "UCD_Garden", parent.pop != "WL2_Garden") %>% #remove garden sites since we don't need their historic growth seasons 
  filter(year<=1993 & year>1963) %>% 
  select(parent.pop:month, cwd, pck, ppt, tmn, tmx) %>% 
  mutate(month = recode(month, #convert months to numbers 
  jan = 1,
  feb = 2,
  mar = 3,
  apr = 4,
  may = 5,
  jun = 6,
  jul = 7,
  aug = 8,
  sep = 9,
  oct = 10,
  nov = 11,
  dec = 12
))
head(pop_elev_climate_historical, 13)
```

```
## # A tibble: 13 × 12
##    parent.pop elevation.group elev_m   Lat  Long  year month   cwd   pck    ppt
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
##  1 BH         Low               511.  37.4 -120.  1964     4  70.3     0  28.8 
##  2 BH         Low               511.  37.4 -120.  1964     8 177.      0   0.04
##  3 BH         Low               511.  37.4 -120.  1964    12  27.9     0 219.  
##  4 BH         Low               511.  37.4 -120.  1964     2  40.4     0   2.35
##  5 BH         Low               511.  37.4 -120.  1964     1  28.1     0  65.8 
##  6 BH         Low               511.  37.4 -120.  1964     7 161.      0   0   
##  7 BH         Low               511.  37.4 -120.  1964     6  42.8     0  10.6 
##  8 BH         Low               511.  37.4 -120.  1964     3  55.9     0  73.6 
##  9 BH         Low               511.  37.4 -120.  1964     5  42.0     0  27.9 
## 10 BH         Low               511.  37.4 -120.  1964    11  40.2     0 120.  
## 11 BH         Low               511.  37.4 -120.  1964    10  92       0  46.8 
## 12 BH         Low               511.  37.4 -120.  1964     9 129.      0   5.56
## 13 BH         Low               511.  37.4 -120.  1965     4  39.2     0 104.  
## # ℹ 2 more variables: tmn <dbl>, tmx <dbl>
```

``` r
summary(pop_elev_climate_historical)
```

```
##   parent.pop        elevation.group        elev_m            Lat       
##  Length:8280        Length:8280        Min.   : 313.0   Min.   :36.56  
##  Class :character   Class :character   1st Qu.: 748.9   1st Qu.:37.81  
##  Mode  :character   Mode  :character   Median :1934.5   Median :38.79  
##                                        Mean   :1649.7   Mean   :38.74  
##                                        3rd Qu.:2373.2   3rd Qu.:39.59  
##                                        Max.   :2872.3   Max.   :40.74  
##       Long             year          month            cwd        
##  Min.   :-123.0   Min.   :1964   Min.   : 1.00   Min.   :  0.00  
##  1st Qu.:-121.4   1st Qu.:1971   1st Qu.: 3.75   1st Qu.: 24.84  
##  Median :-120.3   Median :1978   Median : 6.50   Median : 46.24  
##  Mean   :-120.4   Mean   :1978   Mean   : 6.50   Mean   : 53.88  
##  3rd Qu.:-119.6   3rd Qu.:1986   3rd Qu.: 9.25   3rd Qu.: 77.08  
##  Max.   :-118.8   Max.   :1993   Max.   :12.00   Max.   :202.50  
##       pck              ppt              tmn               tmx       
##  Min.   :   0.0   Min.   :  0.00   Min.   :-14.970   Min.   :-2.65  
##  1st Qu.:   0.0   1st Qu.: 11.57   1st Qu.: -3.090   1st Qu.: 8.54  
##  Median :   0.0   Median : 52.50   Median :  2.010   Median :15.13  
##  Mean   : 161.6   Mean   : 99.71   Mean   :  2.121   Mean   :15.76  
##  3rd Qu.: 148.8   3rd Qu.:139.21   3rd Qu.:  7.240   3rd Qu.:22.24  
##  Max.   :2594.7   Max.   :951.79   Max.   : 18.820   Max.   :37.76
```

``` r
xtabs(~parent.pop+month, data=pop_elev_climate_historical) #check that there's 30 years for all pops and months 
```

```
##           month
## parent.pop  1  2  3  4  5  6  7  8  9 10 11 12
##      BH    30 30 30 30 30 30 30 30 30 30 30 30
##      CC    30 30 30 30 30 30 30 30 30 30 30 30
##      CP2   30 30 30 30 30 30 30 30 30 30 30 30
##      CP3   30 30 30 30 30 30 30 30 30 30 30 30
##      DPR   30 30 30 30 30 30 30 30 30 30 30 30
##      FR    30 30 30 30 30 30 30 30 30 30 30 30
##      IH    30 30 30 30 30 30 30 30 30 30 30 30
##      LV1   30 30 30 30 30 30 30 30 30 30 30 30
##      LV3   30 30 30 30 30 30 30 30 30 30 30 30
##      LVTR1 30 30 30 30 30 30 30 30 30 30 30 30
##      SC    30 30 30 30 30 30 30 30 30 30 30 30
##      SQ1   30 30 30 30 30 30 30 30 30 30 30 30
##      SQ2   30 30 30 30 30 30 30 30 30 30 30 30
##      SQ3   30 30 30 30 30 30 30 30 30 30 30 30
##      TM2   30 30 30 30 30 30 30 30 30 30 30 30
##      WL1   30 30 30 30 30 30 30 30 30 30 30 30
##      WL2   30 30 30 30 30 30 30 30 30 30 30 30
##      WR    30 30 30 30 30 30 30 30 30 30 30 30
##      WV    30 30 30 30 30 30 30 30 30 30 30 30
##      YO11  30 30 30 30 30 30 30 30 30 30 30 30
##      YO4   30 30 30 30 30 30 30 30 30 30 30 30
##      YO7   30 30 30 30 30 30 30 30 30 30 30 30
##      YO8   30 30 30 30 30 30 30 30 30 30 30 30
```

## Averages and Preliminary Calcs

### Recent climate


``` r
pop_elev_climate_recent %>% filter(ppt == 0) #months 7-10 usually have ppt = 0 for low elev 
```

```
## # A tibble: 541 × 12
##    parent.pop elevation.group elev_m   Lat  Long  year month   cwd   pck   ppt
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 BH         Low               511.  37.4 -120.  1994     8 180.      0     0
##  2 BH         Low               511.  37.4 -120.  1994     7 174.      0     0
##  3 BH         Low               511.  37.4 -120.  1995    10  92.1     0     0
##  4 BH         Low               511.  37.4 -120.  1996     9 132.      0     0
##  5 BH         Low               511.  37.4 -120.  1997     8 155.      0     0
##  6 BH         Low               511.  37.4 -120.  1998     8 137       0     0
##  7 BH         Low               511.  37.4 -120.  1999     8 129.      0     0
##  8 BH         Low               511.  37.4 -120.  1999     7 120.      0     0
##  9 BH         Low               511.  37.4 -120.  1999     9 121.      0     0
## 10 BH         Low               511.  37.4 -120.  2000     7 134.      0     0
## # ℹ 531 more rows
## # ℹ 2 more variables: tmn <dbl>, tmx <dbl>
```

``` r
pop_elev_climate_recent %>% filter(pck >0, ppt == 0) #in sequoia pops, 2022, ppt=0, but there was still snowpack in January 
```

```
## # A tibble: 3 × 12
##   parent.pop elevation.group elev_m   Lat  Long  year month   cwd   pck   ppt
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 SQ1        Mid              1921.  36.6 -119.  2022     1  32.1  71.3     0
## 2 SQ2        Mid              1934.  36.7 -119.  2022     1  27.6  98.0     0
## 3 SQ3        High             2373.  36.7 -119.  2022     1  32.5 236.      0
## # ℹ 2 more variables: tmn <dbl>, tmx <dbl>
```

``` r
pop_elev_climate_recent %>% filter(ppt > 0, tmx<1.5) %>% arrange(pck) #if the temp is less than 1.5 (Flint's criteria for snowfall) and there is precipitation then pck > 0
```

```
## # A tibble: 165 × 12
##    parent.pop elevation.group elev_m   Lat  Long  year month   cwd   pck   ppt
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 YO11       High             2872.  37.9 -119.  2001     1  19.0  98.1  91.4
##  2 YO11       High             2872.  37.9 -119.  1997    12  16.9 106.   69.8
##  3 YO11       High             2872.  37.9 -119.  2007    12  17.8 107.  114. 
##  4 YO11       High             2872.  37.9 -119.  2009    12  18.1 140.  146. 
##  5 YO11       High             2872.  37.9 -119.  2019    12  18.7 146.  153. 
##  6 YO11       High             2872.  37.9 -119.  1994    11  16.7 191.  198. 
##  7 LV3        High             2354.  40.5 -122.  2007    12  10.8 193.  198. 
##  8 LV1        High             2593.  40.5 -122.  2007    12  12.2 198.  204. 
##  9 LVTR1      High             2741.  40.5 -122.  2007    12  13.5 205.  211. 
## 10 LV1        High             2593.  40.5 -122.  2009    12  12.0 223.  187. 
## # ℹ 155 more rows
## # ℹ 2 more variables: tmn <dbl>, tmx <dbl>
```

``` r
#first calculate 30 year monthly averages for pck and ppt 
pop_elev_climate_recent_avgs <- pop_elev_climate_recent %>% 
  group_by(parent.pop, elevation.group, elev_m, month) %>% 
  summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean, sem), na.rm = TRUE)
names(pop_elev_climate_recent_avgs) <- gsub("fn2", "sem", colnames(pop_elev_climate_recent_avgs))
names(pop_elev_climate_recent_avgs) <-gsub("fn1", "mean", colnames(pop_elev_climate_recent_avgs))
pop_elev_climate_recent_avgs #30 year averages per month for each pop
```

```
## # A tibble: 276 × 14
## # Groups:   parent.pop, elevation.group, elev_m [23]
##    parent.pop elevation.group elev_m month cwd_mean pck_mean ppt_mean tmn_mean
##    <chr>      <chr>            <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
##  1 BH         Low               511.     1     29.4        0  124.        2.81
##  2 BH         Low               511.     2     41.0        0   93.9       3.32
##  3 BH         Low               511.     3     53.9        0   90.1       4.82
##  4 BH         Low               511.     4     59.0        0   48.2       6.41
##  5 BH         Low               511.     5     51.4        0   23.2       9.78
##  6 BH         Low               511.     6     89.3        0    6.34     13.6 
##  7 BH         Low               511.     7    138.         0    0.281    17.5 
##  8 BH         Low               511.     8    154.         0    1.00     17.1 
##  9 BH         Low               511.     9    130.         0    3.75     14.4 
## 10 BH         Low               511.    10     89.1        0   27.7       9.63
## # ℹ 266 more rows
## # ℹ 6 more variables: tmx_mean <dbl>, cwd_sem <dbl>, pck_sem <dbl>,
## #   ppt_sem <dbl>, tmn_sem <dbl>, tmx_sem <dbl>
```

``` r
pop_elev_climate_recent_avgs <- pop_elev_climate_recent_avgs %>% mutate(PckSum=sum(pck_mean)) #estimate of average total snowpack in a year 
pop_elev_climate_recent_avgs %>% arrange(PckSum)
```

```
## # A tibble: 276 × 15
## # Groups:   parent.pop, elevation.group, elev_m [23]
##    parent.pop elevation.group elev_m month cwd_mean pck_mean ppt_mean tmn_mean
##    <chr>      <chr>            <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
##  1 BH         Low               511.     1     29.4        0  124.        2.81
##  2 BH         Low               511.     2     41.0        0   93.9       3.32
##  3 BH         Low               511.     3     53.9        0   90.1       4.82
##  4 BH         Low               511.     4     59.0        0   48.2       6.41
##  5 BH         Low               511.     5     51.4        0   23.2       9.78
##  6 BH         Low               511.     6     89.3        0    6.34     13.6 
##  7 BH         Low               511.     7    138.         0    0.281    17.5 
##  8 BH         Low               511.     8    154.         0    1.00     17.1 
##  9 BH         Low               511.     9    130.         0    3.75     14.4 
## 10 BH         Low               511.    10     89.1        0   27.7       9.63
## # ℹ 266 more rows
## # ℹ 7 more variables: tmx_mean <dbl>, cwd_sem <dbl>, pck_sem <dbl>,
## #   ppt_sem <dbl>, tmn_sem <dbl>, tmx_sem <dbl>, PckSum <dbl>
```

``` r
#Exploratory filters
pop_elev_climate_recent %>% filter(parent.pop=="SC") %>% filter(year==2016 | year==2017) #snow pack in Jan 2017
```

```
## # A tibble: 24 × 12
##    parent.pop elevation.group elev_m   Lat  Long  year month   cwd   pck    ppt
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
##  1 SC         Low               422.  38.4 -121.  2016     4  37.5     0  77.3 
##  2 SC         Low               422.  38.4 -121.  2016     8 159.      0   0.03
##  3 SC         Low               422.  38.4 -121.  2016    12  20.6     0 135.  
##  4 SC         Low               422.  38.4 -121.  2016     2  34.6     0  33.4 
##  5 SC         Low               422.  38.4 -121.  2016     1  20.2     0 212.  
##  6 SC         Low               422.  38.4 -121.  2016     7 142.      0   0   
##  7 SC         Low               422.  38.4 -121.  2016     6  94.8     0   0   
##  8 SC         Low               422.  38.4 -121.  2016     3  30.6     0 223.  
##  9 SC         Low               422.  38.4 -121.  2016     5  69.0     0  12.2 
## 10 SC         Low               422.  38.4 -121.  2016    11  29.6     0  93.9 
## # ℹ 14 more rows
## # ℹ 2 more variables: tmn <dbl>, tmx <dbl>
```

``` r
pop_elev_climate_recent %>% filter(parent.pop=="IH") %>% filter(pck >0) #snow pack in Jan 2017 and Feb 2019
```

```
## # A tibble: 2 × 12
##   parent.pop elevation.group elev_m   Lat  Long  year month   cwd   pck   ppt
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 IH         Low               454.  39.1 -121.  2017     1  15.6  18.0  495.
## 2 IH         Low               454.  39.1 -121.  2019     2  19.9  42.3  429.
## # ℹ 2 more variables: tmn <dbl>, tmx <dbl>
```

``` r
pop_elev_climate_recent %>% filter(parent.pop=="SQ3") %>% filter(pck==0, ppt < 10) #high elev pops get very little rain and high cwd during growth season ...
```

```
## # A tibble: 105 × 12
##    parent.pop elevation.group elev_m   Lat  Long  year month   cwd   pck   ppt
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 SQ3        High             2373.  36.7 -119.  1994     8 126.      0  0.3 
##  2 SQ3        High             2373.  36.7 -119.  1994     7 114.      0  6.24
##  3 SQ3        High             2373.  36.7 -119.  1994     6  95.1     0  0.01
##  4 SQ3        High             2373.  36.7 -119.  1995     8  85.1     0  0.2 
##  5 SQ3        High             2373.  36.7 -119.  1995     7  90.6     0  5.44
##  6 SQ3        High             2373.  36.7 -119.  1995    11  46.9     0  0.84
##  7 SQ3        High             2373.  36.7 -119.  1995    10  77.1     0  0   
##  8 SQ3        High             2373.  36.7 -119.  1995     9  84.2     0  0.34
##  9 SQ3        High             2373.  36.7 -119.  1996     7 101.      0  6.29
## 10 SQ3        High             2373.  36.7 -119.  1996     9 101.      0  3.39
## # ℹ 95 more rows
## # ℹ 2 more variables: tmn <dbl>, tmx <dbl>
```

### Historical climate


``` r
pop_elev_climate_historical_avgs <- pop_elev_climate_historical %>% 
  group_by(parent.pop, elevation.group, elev_m, month) %>% 
  summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean, sem), na.rm = TRUE)
names(pop_elev_climate_historical_avgs) <- gsub("fn2", "sem", colnames(pop_elev_climate_historical_avgs))
names(pop_elev_climate_historical_avgs) <-gsub("fn1", "mean", colnames(pop_elev_climate_historical_avgs))
pop_elev_climate_historical_avgs #30 year averages per month for each pop
```

```
## # A tibble: 276 × 14
## # Groups:   parent.pop, elevation.group, elev_m [23]
##    parent.pop elevation.group elev_m month cwd_mean pck_mean ppt_mean tmn_mean
##    <chr>      <chr>            <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
##  1 BH         Low               511.     1     28.0    0.234   104.       1.46
##  2 BH         Low               511.     2     40.4    0        92.2      2.76
##  3 BH         Low               511.     3     51.2    0       101.       4.04
##  4 BH         Low               511.     4     62.2    0        46.4      5.57
##  5 BH         Low               511.     5     63.5    0        12.6      8.83
##  6 BH         Low               511.     6     87.5    0         5.73    12.4 
##  7 BH         Low               511.     7    126.     0         2.09    15.6 
##  8 BH         Low               511.     8    149.     0         1.82    15.3 
##  9 BH         Low               511.     9    127.     0        10.1     12.6 
## 10 BH         Low               511.    10     87.3    0        31.9      8.48
## # ℹ 266 more rows
## # ℹ 6 more variables: tmx_mean <dbl>, cwd_sem <dbl>, pck_sem <dbl>,
## #   ppt_sem <dbl>, tmn_sem <dbl>, tmx_sem <dbl>
```

``` r
pop_elev_climate_historical_avgs <- pop_elev_climate_historical_avgs %>%
  mutate(PckSum=sum(pck_mean)) #estimate of average total snowpack in a year 
pop_elev_climate_historical_avgs %>% arrange(PckSum)
```

```
## # A tibble: 276 × 15
## # Groups:   parent.pop, elevation.group, elev_m [23]
##    parent.pop elevation.group elev_m month cwd_mean pck_mean ppt_mean tmn_mean
##    <chr>      <chr>            <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
##  1 BH         Low               511.     1     28.0    0.234   104.       1.46
##  2 BH         Low               511.     2     40.4    0        92.2      2.76
##  3 BH         Low               511.     3     51.2    0       101.       4.04
##  4 BH         Low               511.     4     62.2    0        46.4      5.57
##  5 BH         Low               511.     5     63.5    0        12.6      8.83
##  6 BH         Low               511.     6     87.5    0         5.73    12.4 
##  7 BH         Low               511.     7    126.     0         2.09    15.6 
##  8 BH         Low               511.     8    149.     0         1.82    15.3 
##  9 BH         Low               511.     9    127.     0        10.1     12.6 
## 10 BH         Low               511.    10     87.3    0        31.9      8.48
## # ℹ 266 more rows
## # ℹ 7 more variables: tmx_mean <dbl>, cwd_sem <dbl>, pck_sem <dbl>,
## #   ppt_sem <dbl>, tmn_sem <dbl>, tmx_sem <dbl>, PckSum <dbl>
```

``` r
#Exploratory filters
pop_elev_climate_historical %>% filter(parent.pop=="BH") %>% filter(pck >0) #snow pack in Jan 1982
```

```
## # A tibble: 1 × 12
##   parent.pop elevation.group elev_m   Lat  Long  year month   cwd   pck   ppt
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 BH         Low               511.  37.4 -120.  1982     1  24.3  7.02  203.
## # ℹ 2 more variables: tmn <dbl>, tmx <dbl>
```

``` r
pop_elev_climate_historical %>% filter(parent.pop=="SC") %>% filter(pck >0) #snow pack in Jan 1973 and 1982
```

```
## # A tibble: 2 × 12
##   parent.pop elevation.group elev_m   Lat  Long  year month   cwd   pck   ppt
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 SC         Low               422.  38.4 -121.  1973     1  19.1  2.94  273.
## 2 SC         Low               422.  38.4 -121.  1982     1  18.6 15.0   209.
## # ℹ 2 more variables: tmn <dbl>, tmx <dbl>
```

``` r
pop_elev_climate_historical %>% filter(parent.pop=="CC") %>% filter(pck >0) #snow pack in Jan 1969 and 1973
```

```
## # A tibble: 2 × 12
##   parent.pop elevation.group elev_m   Lat  Long  year month   cwd   pck   ppt
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 CC         Low                313  39.6 -121.  1969     1  16.7  21.4  415.
## 2 CC         Low                313  39.6 -121.  1973     1  16.8   7.1  358.
## # ℹ 2 more variables: tmn <dbl>, tmx <dbl>
```

``` r
pop_elev_climate_historical %>% filter(parent.pop=="TM2") %>% filter(pck >0) #snow pack in Dec '68, Jan-Feb '69, Dec '71, Dec '72, Jan '73, Jan '82
```

```
## # A tibble: 7 × 12
##   parent.pop elevation.group elev_m   Lat  Long  year month   cwd   pck   ppt
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 TM2        Low               379.  39.6 -122.  1968    12 10.3   6.04  263.
## 2 TM2        Low               379.  39.6 -122.  1969     2  0    30.5   312.
## 3 TM2        Low               379.  39.6 -122.  1969     1  3.81 61.4   464.
## 4 TM2        Low               379.  39.6 -122.  1971    12 17.2   3.88  199.
## 5 TM2        Low               379.  39.6 -122.  1972    12 10.1  11.4   153.
## 6 TM2        Low               379.  39.6 -122.  1973     1  4.85 47.9   388.
## 7 TM2        Low               379.  39.6 -122.  1982     1  3.91  2.98  201.
## # ℹ 2 more variables: tmn <dbl>, tmx <dbl>
```

``` r
pop_elev_climate_historical %>% filter(parent.pop=="IH") %>% filter(pck >0) #snow pack in Dec '68, Jan-Feb '69, Dec '70, Dec '71, Jan & Dec '72, Jan-Feb '73, Jan '79
```

```
## # A tibble: 13 × 12
##    parent.pop elevation.group elev_m   Lat  Long  year month   cwd    pck   ppt
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
##  1 IH         Low               454.  39.1 -121.  1968    12  16.5  17.8  230. 
##  2 IH         Low               454.  39.1 -121.  1969     2  19.7 129.   301. 
##  3 IH         Low               454.  39.1 -121.  1969     1  15.3 107.   529. 
##  4 IH         Low               454.  39.1 -121.  1970    12  16.6  27.9  345. 
##  5 IH         Low               454.  39.1 -121.  1971    12  17.4  59.3  262. 
##  6 IH         Low               454.  39.1 -121.  1972    12  17.1  20.4  143. 
##  7 IH         Low               454.  39.1 -121.  1972     1  18.3  19.6   81.1
##  8 IH         Low               454.  39.1 -121.  1973     2  21.6   0.48 247. 
##  9 IH         Low               454.  39.1 -121.  1973     1  14.9 105.   381. 
## 10 IH         Low               454.  39.1 -121.  1979     1  17.4   4.82 227. 
## 11 IH         Low               454.  39.1 -121.  1982     1  14.9  29.3  229. 
## 12 IH         Low               454.  39.1 -121.  1992    12  18.8   4.13 301. 
## 13 IH         Low               454.  39.1 -121.  1993     1  15.6   5.34 364. 
## # ℹ 2 more variables: tmn <dbl>, tmx <dbl>
```

### Simpler version (similar results to above calcs)
Take the sum of snowpack within each year and then average that 
In code above, take average snowpack for each month (across all years) and then sum that 

``` r
##recent
pop_elev_climate_recent_sums <- pop_elev_climate_recent %>% 
  group_by(parent.pop, elevation.group, elev_m, year) %>% 
  summarise(PckSum=sum(pck))
```

```
## `summarise()` has grouped output by 'parent.pop', 'elevation.group', 'elev_m'.
## You can override using the `.groups` argument.
```

``` r
pop_elev_climate_recent_sums_avgs <- pop_elev_climate_recent_sums %>% 
  group_by(parent.pop, elevation.group, elev_m) %>% 
  summarise(meanPckSum=mean(PckSum), semPckSum=sem(PckSum)) %>% 
  arrange(meanPckSum)
```

```
## `summarise()` has grouped output by 'parent.pop', 'elevation.group'. You can
## override using the `.groups` argument.
```

``` r
pop_elev_climate_recent_sums_avgs
```

```
## # A tibble: 23 × 5
## # Groups:   parent.pop, elevation.group [23]
##    parent.pop elevation.group elev_m meanPckSum semPckSum
##    <chr>      <chr>            <dbl>      <dbl>     <dbl>
##  1 BH         Low               511.      0         0    
##  2 CC         Low               313       0         0    
##  3 TM2        Low               379.      0         0    
##  4 SC         Low               422.      0.252     0.252
##  5 IH         Low               454.      2.01      1.51 
##  6 DPR        Mid              1019.     91.6      22.5  
##  7 FR         Mid               787     170.       39.2  
##  8 WV         Mid               749.    317.       49.9  
##  9 WR         Mid              1158     374.       64.8  
## 10 SQ1        Mid              1921.    671.      130.   
## # ℹ 13 more rows
```

``` r
##historical
pop_elev_climate_historical_sums <- pop_elev_climate_historical %>% 
  group_by(parent.pop, elevation.group, elev_m, year) %>% 
  summarise(PckSum=sum(pck))
```

```
## `summarise()` has grouped output by 'parent.pop', 'elevation.group', 'elev_m'.
## You can override using the `.groups` argument.
```

``` r
pop_elev_climate_historical_sums_avgs <- pop_elev_climate_historical_sums %>% 
  group_by(parent.pop, elevation.group, elev_m) %>% 
  summarise(meanPckSum=mean(PckSum), semPckSum=sem(PckSum)) %>% 
  arrange(meanPckSum)
```

```
## `summarise()` has grouped output by 'parent.pop', 'elevation.group'. You can
## override using the `.groups` argument.
```

``` r
pop_elev_climate_historical_sums_avgs
```

```
## # A tibble: 23 × 5
## # Groups:   parent.pop, elevation.group [23]
##    parent.pop elevation.group elev_m meanPckSum semPckSum
##    <chr>      <chr>            <dbl>      <dbl>     <dbl>
##  1 BH         Low               511.      0.234     0.234
##  2 SC         Low               422.      0.597     0.506
##  3 CC         Low               313       0.951     0.745
##  4 TM2        Low               379.      5.47      3.40 
##  5 IH         Low               454.     17.7       8.62 
##  6 FR         Mid               787     231.       58.3  
##  7 DPR        Mid              1019.    246.       62.4  
##  8 WV         Mid               749.    438.       84.6  
##  9 WR         Mid              1158     596.      114.   
## 10 WL1        Mid              1614.    941.      140.   
## # ℹ 13 more rows
```

## Populations that get less than 70 mm (b/c that's the avg height in Oct at WL2 garden) of snow pack in a year (on average)

-   First month = ppt \>= 25

    -   Remember this month is not included in the final "growth season"

    -   Jenny has used 25 mm as germinating inducing rain (esp b/c it's
        a sum of ppt for the whole month), 10 mm would probably be fine
        for sustaining growth

-   Last month = cwd \> 3rd quartile of CWD for those groups of pops

    -   Remember this month is included in the final "growth season"

### Recent Climate

Prep

``` r
nosnow_pops_recent <- pop_elev_climate_recent_avgs %>% filter(PckSum < 70)
unique(nosnow_pops_recent$parent.pop) #BH, CC, TM2, SC, IH 
```

```
## [1] "BH"  "CC"  "IH"  "SC"  "TM2"
```

``` r
summary(nosnow_pops_recent) #3rd quartile of CWD = 89
```

```
##   parent.pop        elevation.group        elev_m          month      
##  Length:60          Length:60          Min.   :313.0   Min.   : 1.00  
##  Class :character   Class :character   1st Qu.:379.2   1st Qu.: 3.75  
##  Mode  :character   Mode  :character   Median :421.5   Median : 6.50  
##                                        Mean   :415.8   Mean   : 6.50  
##                                        3rd Qu.:454.1   3rd Qu.: 9.25  
##                                        Max.   :511.4   Max.   :12.00  
##     cwd_mean         pck_mean          ppt_mean            tmn_mean     
##  Min.   : 13.54   Min.   :0.00000   Min.   :  0.09033   Min.   : 2.592  
##  1st Qu.: 31.16   1st Qu.:0.00000   1st Qu.:  9.79167   1st Qu.: 4.765  
##  Median : 51.18   Median :0.00000   Median : 56.63300   Median : 8.614  
##  Mean   : 62.45   Mean   :0.03768   Mean   : 76.40618   Mean   : 9.246  
##  3rd Qu.: 89.12   3rd Qu.:0.00000   3rd Qu.:124.80833   3rd Qu.:13.982  
##  Max.   :168.29   Max.   :1.41000   Max.   :233.60533   Max.   :17.674  
##     tmx_mean        cwd_sem          pck_sem           ppt_sem       
##  Min.   :12.41   Min.   :0.1882   Min.   :0.00000   Min.   : 0.0424  
##  1st Qu.:16.20   1st Qu.:0.5563   1st Qu.:0.00000   1st Qu.: 2.4494  
##  Median :22.33   Median :1.5942   Median :0.00000   Median :10.1162  
##  Mean   :22.92   Mean   :2.4321   Mean   :0.03768   Mean   :11.0920  
##  3rd Qu.:30.59   3rd Qu.:3.9665   3rd Qu.:0.00000   3rd Qu.:18.0488  
##  Max.   :35.28   Max.   :9.7042   Max.   :1.41000   Max.   :28.2653  
##     tmn_sem          tmx_sem           PckSum      
##  Min.   :0.1784   Min.   :0.1738   Min.   :0.0000  
##  1st Qu.:0.2266   1st Qu.:0.2674   1st Qu.:0.0000  
##  Median :0.2455   Median :0.3464   Median :0.0000  
##  Mean   :0.2455   Mean   :0.3333   Mean   :0.4522  
##  3rd Qu.:0.2663   3rd Qu.:0.3711   3rd Qu.:0.2523  
##  Max.   :0.2926   Max.   :0.4968   Max.   :2.0087
```

``` r
nosnow_pops_recent_tojoin <- nosnow_pops_recent %>% select(parent.pop:elev_m, PckSum) %>% distinct()

nosnow_pops_recent_years <- left_join(nosnow_pops_recent_tojoin, pop_elev_climate_recent) 
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m)`
```

``` r
nosnow_pops_recent_years %>% filter(month=="7", ppt>=25) #never get >=25mm of ppt in July 
```

```
## # A tibble: 0 × 13
## # Groups:   parent.pop, elevation.group, elev_m [0]
## # ℹ 13 variables: parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   PckSum <dbl>, Lat <dbl>, Long <dbl>, year <dbl>, month <dbl>, cwd <dbl>,
## #   pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_pops_recent_years %>% filter(month=="8", ppt>=25) #sometimes get >= 25 mm of ppt in Aug
```

```
## # A tibble: 3 × 13
## # Groups:   parent.pop, elevation.group, elev_m [3]
##   parent.pop elevation.group elev_m PckSum   Lat  Long  year month   cwd   pck
##   <chr>      <chr>            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 CC         Low               313    0     39.6 -121.  2003     8  71.8     0
## 2 IH         Low               454.   2.01  39.1 -121.  2003     8  54.6     0
## 3 TM2        Low               379.   0     39.6 -122.  1997     8 139.      0
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#going to start the "grow year" in Sept though due to after ripening requirements 

growyear_months <- tibble(month=c(1:12), growmonth=c(5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4))

nosnow_recent_growyear_months <- full_join(growyear_months, nosnow_pops_recent_years)
```

```
## Joining with `by = join_by(month)`
```

``` r
dim(nosnow_recent_growyear_months)
```

```
## [1] 1800   14
```

``` r
#nosnow_recent_growyear_months %>% filter(ppt==0, cwd<89)
```

First month 

``` r
nosnow_recent_first_month <- nosnow_recent_growyear_months %>%
  group_by(parent.pop, elevation.group, elev_m, year) %>% 
  filter(ppt>=25) %>% 
  arrange(growmonth) %>% 
  filter(row_number()==1) #get first month for each pop and year with germinating inducing rain 

#nosnow_recent_first_month %>% filter(parent.pop=="IH") %>% arrange(year)

nosnow_recent_first_month_tomerge <- nosnow_recent_first_month %>% 
  select(parent.pop:elev_m, year, firstmonth=growmonth) #first month is in grow month not calendar month format

nosnow_recent_first_month_col <- full_join(nosnow_recent_growyear_months, nosnow_recent_first_month_tomerge)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, year)`
```

``` r
dim(nosnow_recent_first_month_col)
```

```
## [1] 1800   15
```

Last month

``` r
nosnow_recent_last_month <- nosnow_recent_growyear_months %>%
  group_by(parent.pop, elevation.group, elev_m, year) %>% 
  filter(cwd>89) %>% #last month is when cwd gets too high 
  arrange(month) %>% 
  filter(row_number()==1)

nosnow_recent_last_month_tomerge <- nosnow_recent_last_month %>% 
  select(parent.pop:elev_m, year, lastmonth=growmonth) #last month is in grow month not calendar month format

nosnow_recent_last_month_col <- full_join(nosnow_recent_first_month_col, nosnow_recent_last_month_tomerge) %>% 
  select(firstmonth, lastmonth, month:tmx)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, year)`
```

``` r
dim(nosnow_recent_last_month_col)
```

```
## [1] 1800   16
```

Checking for weird cases 

``` r
summary(nosnow_recent_last_month_col)
```

```
##    firstmonth      lastmonth          month         growmonth    
##  Min.   :1.000   Min.   : 1.000   Min.   : 1.00   Min.   : 1.00  
##  1st Qu.:2.000   1st Qu.:10.000   1st Qu.: 3.75   1st Qu.: 3.75  
##  Median :2.000   Median :10.000   Median : 6.50   Median : 6.50  
##  Mean   :2.369   Mean   : 9.905   Mean   : 6.50   Mean   : 6.50  
##  3rd Qu.:3.000   3rd Qu.:11.000   3rd Qu.: 9.25   3rd Qu.: 9.25  
##  Max.   :4.000   Max.   :12.000   Max.   :12.00   Max.   :12.00  
##  NA's   :12      NA's   :36                                      
##   parent.pop        elevation.group        elev_m          PckSum      
##  Length:1800        Length:1800        Min.   :313.0   Min.   :0.0000  
##  Class :character   Class :character   1st Qu.:379.2   1st Qu.:0.0000  
##  Mode  :character   Mode  :character   Median :421.5   Median :0.0000  
##                                        Mean   :415.8   Mean   :0.4522  
##                                        3rd Qu.:454.1   3rd Qu.:0.2523  
##                                        Max.   :511.4   Max.   :2.0087  
##                                                                        
##       Lat             Long             year           cwd        
##  Min.   :37.41   Min.   :-121.6   Min.   :1994   Min.   :  0.00  
##  1st Qu.:38.41   1st Qu.:-121.4   1st Qu.:2001   1st Qu.: 27.89  
##  Median :39.09   Median :-120.9   Median :2008   Median : 50.36  
##  Mean   :38.82   Mean   :-120.9   Mean   :2008   Mean   : 62.45  
##  3rd Qu.:39.59   3rd Qu.:-120.7   3rd Qu.:2016   3rd Qu.: 91.05  
##  Max.   :39.59   Max.   :-120.0   Max.   :2023   Max.   :194.73  
##                                                                  
##       pck                ppt               tmn              tmx       
##  Min.   : 0.00000   Min.   :  0.000   Min.   :-1.050   Min.   : 9.38  
##  1st Qu.: 0.00000   1st Qu.:  1.518   1st Qu.: 4.817   1st Qu.:15.59  
##  Median : 0.00000   Median : 31.325   Median : 8.155   Median :22.00  
##  Mean   : 0.03768   Mean   : 76.406   Mean   : 9.246   Mean   :22.92  
##  3rd Qu.: 0.00000   3rd Qu.:113.730   3rd Qu.:14.030   3rd Qu.:30.86  
##  Max.   :42.30000   Max.   :614.040   Max.   :21.350   Max.   :37.38  
## 
```

``` r
nosnow_recent_last_month_col %>% filter(is.na(lastmonth)) %>% arrange(year) #3 years (1998, 2005, 2019) when IH doesn't have a last month based on these criteria (cwd never goes above 89 in those years)
```

```
## # A tibble: 36 × 16
##    firstmonth lastmonth month growmonth parent.pop elevation.group elev_m PckSum
##         <dbl>     <dbl> <dbl>     <dbl> <chr>      <chr>            <dbl>  <dbl>
##  1          1        NA     1         5 IH         Low               454.   2.01
##  2          1        NA     2         6 IH         Low               454.   2.01
##  3          1        NA     3         7 IH         Low               454.   2.01
##  4          1        NA     4         8 IH         Low               454.   2.01
##  5          1        NA     5         9 IH         Low               454.   2.01
##  6          1        NA     6        10 IH         Low               454.   2.01
##  7          1        NA     7        11 IH         Low               454.   2.01
##  8          1        NA     8        12 IH         Low               454.   2.01
##  9          1        NA     9         1 IH         Low               454.   2.01
## 10          1        NA    10         2 IH         Low               454.   2.01
## # ℹ 26 more rows
## # ℹ 8 more variables: Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#****CAN EXCLUDE THESE CASES
nosnow_recent_last_month_col %>% filter(is.na(firstmonth)) %>% arrange(year) #1 year (2013) when BH doesn't have a first month based on these criteria (ppt never above 25)
```

```
## # A tibble: 12 × 16
##    firstmonth lastmonth month growmonth parent.pop elevation.group elev_m PckSum
##         <dbl>     <dbl> <dbl>     <dbl> <chr>      <chr>            <dbl>  <dbl>
##  1         NA        10     1         5 BH         Low               511.      0
##  2         NA        10     2         6 BH         Low               511.      0
##  3         NA        10     3         7 BH         Low               511.      0
##  4         NA        10     4         8 BH         Low               511.      0
##  5         NA        10     5         9 BH         Low               511.      0
##  6         NA        10     6        10 BH         Low               511.      0
##  7         NA        10     7        11 BH         Low               511.      0
##  8         NA        10     8        12 BH         Low               511.      0
##  9         NA        10     9         1 BH         Low               511.      0
## 10         NA        10    10         2 BH         Low               511.      0
## 11         NA        10    11         3 BH         Low               511.      0
## 12         NA        10    12         4 BH         Low               511.      0
## # ℹ 8 more variables: Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#****CAN EXCLUDE THIS CASE 
nosnow_recent_last_month_col %>% filter(lastmonth<4) %>% arrange(parent.pop, year) #some cases where last month is less than 4 (earlier than December)
```

```
## # A tibble: 120 × 16
##    firstmonth lastmonth month growmonth parent.pop elevation.group elev_m PckSum
##         <dbl>     <dbl> <dbl>     <dbl> <chr>      <chr>            <dbl>  <dbl>
##  1          2         1     1         5 CC         Low                313      0
##  2          2         1     2         6 CC         Low                313      0
##  3          2         1     3         7 CC         Low                313      0
##  4          2         1     4         8 CC         Low                313      0
##  5          2         1     5         9 CC         Low                313      0
##  6          2         1     6        10 CC         Low                313      0
##  7          2         1     7        11 CC         Low                313      0
##  8          2         1     8        12 CC         Low                313      0
##  9          2         1     9         1 CC         Low                313      0
## 10          2         1    10         2 CC         Low                313      0
## # ℹ 110 more rows
## # ℹ 8 more variables: Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_recent_last_month_col %>% filter(lastmonth<firstmonth, lastmonth<4) %>% arrange(parent.pop, year) #all of the above are when the last month is before the first month (in growyear, not calendar year)
```

```
## # A tibble: 120 × 16
##    firstmonth lastmonth month growmonth parent.pop elevation.group elev_m PckSum
##         <dbl>     <dbl> <dbl>     <dbl> <chr>      <chr>            <dbl>  <dbl>
##  1          2         1     1         5 CC         Low                313      0
##  2          2         1     2         6 CC         Low                313      0
##  3          2         1     3         7 CC         Low                313      0
##  4          2         1     4         8 CC         Low                313      0
##  5          2         1     5         9 CC         Low                313      0
##  6          2         1     6        10 CC         Low                313      0
##  7          2         1     7        11 CC         Low                313      0
##  8          2         1     8        12 CC         Low                313      0
##  9          2         1     9         1 CC         Low                313      0
## 10          2         1    10         2 CC         Low                313      0
## # ℹ 110 more rows
## # ℹ 8 more variables: Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#****NEED TO CONVERT LAST MONTH TO CALENDAR YEAR FORMAT FOR AVG CALCS 
nosnow_recent_last_month_col %>% filter(lastmonth>firstmonth & lastmonth<4) %>% arrange(parent.pop, year) #0 cases where last month is after the first month 
```

```
## # A tibble: 0 × 16
## # ℹ 16 variables: firstmonth <dbl>, lastmonth <dbl>, month <dbl>,
## #   growmonth <dbl>, parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   PckSum <dbl>, Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_recent_last_month_col %>% filter(lastmonth==1) %>% arrange(parent.pop, year) #10 cases where Sept is last month 
```

```
## # A tibble: 120 × 16
##    firstmonth lastmonth month growmonth parent.pop elevation.group elev_m PckSum
##         <dbl>     <dbl> <dbl>     <dbl> <chr>      <chr>            <dbl>  <dbl>
##  1          2         1     1         5 CC         Low                313      0
##  2          2         1     2         6 CC         Low                313      0
##  3          2         1     3         7 CC         Low                313      0
##  4          2         1     4         8 CC         Low                313      0
##  5          2         1     5         9 CC         Low                313      0
##  6          2         1     6        10 CC         Low                313      0
##  7          2         1     7        11 CC         Low                313      0
##  8          2         1     8        12 CC         Low                313      0
##  9          2         1     9         1 CC         Low                313      0
## 10          2         1    10         2 CC         Low                313      0
## # ℹ 110 more rows
## # ℹ 8 more variables: Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_recent_last_month_col %>% filter(lastmonth==2) #0 cases where Oct-Dec is the last month
```

```
## # A tibble: 0 × 16
## # ℹ 16 variables: firstmonth <dbl>, lastmonth <dbl>, month <dbl>,
## #   growmonth <dbl>, parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   PckSum <dbl>, Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_recent_last_month_col %>% filter(firstmonth>4) #0 cases where first month is after December
```

```
## # A tibble: 0 × 16
## # ℹ 16 variables: firstmonth <dbl>, lastmonth <dbl>, month <dbl>,
## #   growmonth <dbl>, parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   PckSum <dbl>, Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_recent_last_month_col %>% filter(lastmonth==firstmonth) %>% arrange(parent.pop, year) #first month and last month are never the same 
```

```
## # A tibble: 0 × 16
## # ℹ 16 variables: firstmonth <dbl>, lastmonth <dbl>, month <dbl>,
## #   growmonth <dbl>, parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   PckSum <dbl>, Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_recent_last_month_col %>% filter(growmonth==firstmonth+1, cwd>89) %>% arrange(parent.pop, year) #1 case at UCD garden where cwd is high in the second growth month (2013) - see above note
```

```
## # A tibble: 0 × 16
## # ℹ 16 variables: firstmonth <dbl>, lastmonth <dbl>, month <dbl>,
## #   growmonth <dbl>, parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   PckSum <dbl>, Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#nosnow_recent_last_month_col %>% filter(growmonth==lastmonth) %>% arrange(cwd, ppt)
```

Average first and last month

``` r
#remove IH: 1998, 2005, 2019
#remove BH: 2013
#convert last month to cal year for avg calc 

nosnow_recentfirst_last_month_for_calc <- nosnow_recent_last_month_col %>% 
  filter(!(parent.pop=="IH" & year==1998)) %>% 
  filter(!(parent.pop=="IH" & year==2005)) %>% 
  filter(!(parent.pop=="IH" & year==2019)) %>% 
  filter(!(parent.pop=="BH" & year==2013)) %>% 
  mutate(lastmonth_cal=if_else(lastmonth<=4, lastmonth+8, lastmonth-4)) %>% 
  select(firstmonth:lastmonth, lastmonth_cal, parent.pop, elevation.group, elev_m, year) %>% 
  distinct()

nosnow_recent_avg_first_last_month <- nosnow_recentfirst_last_month_for_calc %>% 
  group_by(parent.pop, elevation.group, elev_m) %>% 
  summarise(AvgFirstMonth=mean(firstmonth), First_sem=sem(firstmonth),
            AvgLastMonth=mean(lastmonth), Last_sem=sem(lastmonth),
            AvgLastMonth_cal=mean(lastmonth_cal), Last_cal_sem=sem(lastmonth_cal))
```

```
## `summarise()` has grouped output by 'parent.pop', 'elevation.group'. You can
## override using the `.groups` argument.
```

``` r
nosnow_recent_avg_first_last_month
```

```
## # A tibble: 5 × 9
## # Groups:   parent.pop, elevation.group [5]
##   parent.pop elevation.group elev_m AvgFirstMonth First_sem AvgLastMonth
##   <chr>      <chr>            <dbl>         <dbl>     <dbl>        <dbl>
## 1 BH         Low               511.          2.83     0.122        10.3 
## 2 CC         Low               313           2.2      0.121        10.2 
## 3 IH         Low               454.          2.15     0.138         8.33
## 4 SC         Low               422.          2.5      0.115        10.4 
## 5 TM2        Low               379.          2.23     0.124        10.2 
## # ℹ 3 more variables: Last_sem <dbl>, AvgLastMonth_cal <dbl>,
## #   Last_cal_sem <dbl>
```

``` r
#2 in growmonth = October 
#8 in growmonth = April - w/o converting last month to calendar year format, the cases where growmonth=1 is pulling the last month much earlier and with a much greater standard error --> wrong!
#8 in lastmonth_cal = August 

#Used Standard Rounding
nosnow_avg_first_last_month_recent <- nosnow_recent_avg_first_last_month %>% 
  mutate(AvgFirstMonth=true_round(AvgFirstMonth, 0), 
          AvgLastMonth=true_round(AvgLastMonth_cal, 0) + 4) %>% #avg in calendar year first to account for cases where CC and TM2 had a last month of Sept, then convert back to grow month format
  select(parent.pop:elev_m, AvgFirstMonth, AvgLastMonth)
nosnow_recent_avg_first_last_month
```

```
## # A tibble: 5 × 9
## # Groups:   parent.pop, elevation.group [5]
##   parent.pop elevation.group elev_m AvgFirstMonth First_sem AvgLastMonth
##   <chr>      <chr>            <dbl>         <dbl>     <dbl>        <dbl>
## 1 BH         Low               511.          2.83     0.122        10.3 
## 2 CC         Low               313           2.2      0.121        10.2 
## 3 IH         Low               454.          2.15     0.138         8.33
## 4 SC         Low               422.          2.5      0.115        10.4 
## 5 TM2        Low               379.          2.23     0.124        10.2 
## # ℹ 3 more variables: Last_sem <dbl>, AvgLastMonth_cal <dbl>,
## #   Last_cal_sem <dbl>
```

``` r
nosnow_avg_first_last_month_recent
```

```
## # A tibble: 5 × 5
## # Groups:   parent.pop, elevation.group [5]
##   parent.pop elevation.group elev_m AvgFirstMonth AvgLastMonth
##   <chr>      <chr>            <dbl>         <dbl>        <dbl>
## 1 BH         Low               511.             3           10
## 2 CC         Low               313              2           11
## 3 IH         Low               454.             2           12
## 4 SC         Low               422.             3           10
## 5 TM2        Low               379.             2           10
```

``` r
nosnow_recent_avg_first_last_month_allmonths <- left_join(nosnow_recent_growyear_months, nosnow_avg_first_last_month_recent)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m)`
```
Apriori Assumption for growth season: Low: Oct-July

Fill in all the months b/t the first and last for the full growth season 

``` r
nosnow_grwseason_recent <- nosnow_recent_avg_first_last_month_allmonths %>% 
  group_by(parent.pop, elevation.group, elev_m, year) %>% 
  filter(growmonth>AvgFirstMonth) %>% #first and last month are in grow month format not calendar year 
  filter(growmonth<=AvgLastMonth) %>% 
  arrange(year,parent.pop, growmonth)
summary(nosnow_grwseason_recent) 
```

```
##      month          growmonth       parent.pop        elevation.group   
##  Min.   : 1.000   Min.   : 3.000   Length:1230        Length:1230       
##  1st Qu.: 3.000   1st Qu.: 5.000   Class :character   Class :character  
##  Median : 5.000   Median : 7.000   Mode  :character   Mode  :character  
##  Mean   : 5.366   Mean   : 7.024                                        
##  3rd Qu.: 7.000   3rd Qu.: 9.000                                        
##  Max.   :12.000   Max.   :12.000                                        
##      elev_m          PckSum            Lat             Long       
##  Min.   :313.0   Min.   :0.0000   Min.   :37.41   Min.   :-121.6  
##  1st Qu.:379.2   1st Qu.:0.0000   1st Qu.:38.41   1st Qu.:-121.4  
##  Median :421.5   Median :0.0000   Median :39.09   Median :-120.9  
##  Mean   :412.7   Mean   :0.5330   Mean   :38.90   Mean   :-121.0  
##  3rd Qu.:454.1   3rd Qu.:0.2523   3rd Qu.:39.59   3rd Qu.:-120.7  
##  Max.   :511.4   Max.   :2.0087   Max.   :39.59   Max.   :-120.0  
##       year           cwd              pck                ppt        
##  Min.   :1994   Min.   :  0.00   Min.   : 0.00000   Min.   :  0.00  
##  1st Qu.:2001   1st Qu.: 21.24   1st Qu.: 0.00000   1st Qu.: 12.31  
##  Median :2008   Median : 32.73   Median : 0.00000   Median : 62.80  
##  Mean   :2008   Mean   : 42.50   Mean   : 0.05515   Mean   :101.48  
##  3rd Qu.:2016   3rd Qu.: 58.63   3rd Qu.: 0.00000   3rd Qu.:147.75  
##  Max.   :2023   Max.   :166.80   Max.   :42.30000   Max.   :614.04  
##       tmn              tmx        AvgFirstMonth    AvgLastMonth  
##  Min.   :-1.050   Min.   : 9.38   Min.   :2.000   Min.   :10.00  
##  1st Qu.: 3.980   1st Qu.:14.16   1st Qu.:2.000   1st Qu.:10.00  
##  Median : 6.030   Median :17.97   Median :2.000   Median :10.00  
##  Mean   : 7.373   Mean   :20.03   Mean   :2.341   Mean   :10.71  
##  3rd Qu.:10.293   3rd Qu.:25.33   3rd Qu.:3.000   3rd Qu.:11.00  
##  Max.   :20.150   Max.   :37.09   Max.   :3.000   Max.   :12.00
```

``` r
xtabs(~parent.pop+month, data=nosnow_grwseason_recent)
```

```
##           month
## parent.pop  1  2  3  4  5  6  7  8 11 12
##        BH  30 30 30 30 30 30  0  0  0 30
##        CC  30 30 30 30 30 30 30  0 30 30
##        IH  30 30 30 30 30 30 30 30 30 30
##        SC  30 30 30 30 30 30  0  0  0 30
##        TM2 30 30 30 30 30 30  0  0 30 30
```

``` r
nosnow_grwseason_recent %>% ggplot(aes(x=month)) + geom_histogram() +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5,6,7,8,9,10, 11, 12)) +
  facet_wrap(~parent.pop)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


### Historical Climate

Prep

``` r
nosnow_pops_historical <- pop_elev_climate_historical_avgs %>% filter(PckSum < 70)
unique(nosnow_pops_historical$parent.pop)
```

```
## [1] "BH"  "CC"  "IH"  "SC"  "TM2"
```

``` r
summary(nosnow_pops_historical) #3rd quartile of CWD = 83 - was 87 before we shifted historical to be 1964-1993 (from 1962-1991)
```

```
##   parent.pop        elevation.group        elev_m          month      
##  Length:60          Length:60          Min.   :313.0   Min.   : 1.00  
##  Class :character   Class :character   1st Qu.:379.2   1st Qu.: 3.75  
##  Mode  :character   Mode  :character   Median :421.5   Median : 6.50  
##                                        Mean   :415.8   Mean   : 6.50  
##                                        3rd Qu.:454.1   3rd Qu.: 9.25  
##                                        Max.   :511.4   Max.   :12.00  
##     cwd_mean         pck_mean         ppt_mean          tmn_mean     
##  Min.   : 11.66   Min.   :0.0000   Min.   :  1.819   Min.   : 1.341  
##  1st Qu.: 29.48   1st Qu.:0.0000   1st Qu.: 12.815   1st Qu.: 4.009  
##  Median : 56.56   Median :0.0000   Median : 58.294   Median : 7.767  
##  Mean   : 61.98   Mean   :0.4152   Mean   : 74.334   Mean   : 8.130  
##  3rd Qu.: 83.24   3rd Qu.:0.0000   3rd Qu.:137.932   3rd Qu.:12.552  
##  Max.   :162.85   Max.   :9.0110   Max.   :212.370   Max.   :16.364  
##     tmx_mean        cwd_sem           pck_sem          ppt_sem       
##  Min.   :12.19   Min.   : 0.1818   Min.   :0.0000   Min.   : 0.9454  
##  1st Qu.:15.79   1st Qu.: 0.6535   1st Qu.:0.0000   1st Qu.: 2.8022  
##  Median :22.33   Median : 1.7091   Median :0.0000   Median : 9.1549  
##  Mean   :22.59   Mean   : 2.5391   Mean   :0.2834   Mean   :10.4890  
##  3rd Qu.:30.25   3rd Qu.: 3.8163   3rd Qu.:0.0000   3rd Qu.:17.7863  
##  Max.   :34.59   Max.   :10.8404   Max.   :4.9334   Max.   :25.8171  
##     tmn_sem          tmx_sem           PckSum       
##  Min.   :0.1661   Min.   :0.2124   Min.   : 0.2340  
##  1st Qu.:0.2076   1st Qu.:0.2992   1st Qu.: 0.5973  
##  Median :0.2447   Median :0.3504   Median : 0.9510  
##  Mean   :0.2453   Mean   :0.3464   Mean   : 4.9819  
##  3rd Qu.:0.2817   3rd Qu.:0.3781   3rd Qu.: 5.4673  
##  Max.   :0.3271   Max.   :0.5246   Max.   :17.6597
```

``` r
nosnow_pops_historical_tojoin <- nosnow_pops_historical %>% select(parent.pop:elev_m, PckSum) %>% distinct()

nosnow_pops_historical_years <- left_join(nosnow_pops_historical_tojoin, pop_elev_climate_historical) 
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m)`
```

``` r
nosnow_pops_historical_years %>% filter(month=="7", ppt>=25) #in 1974, ppt>25 for 4 pops in July + BH in 1992... unlikely seeds would have after ripened enough 
```

```
## # A tibble: 5 × 13
## # Groups:   parent.pop, elevation.group, elev_m [5]
##   parent.pop elevation.group elev_m PckSum   Lat  Long  year month   cwd   pck
##   <chr>      <chr>            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 BH         Low               511.  0.234  37.4 -120.  1992     7  82.4     0
## 2 CC         Low               313   0.951  39.6 -121.  1974     7  52.4     0
## 3 IH         Low               454. 17.7    39.1 -121.  1974     7   0       0
## 4 SC         Low               422.  0.597  38.4 -121.  1974     7 127.      0
## 5 TM2        Low               379.  5.47   39.6 -122.  1974     7   0       0
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_pops_historical_years %>% filter(month=="8", ppt>=25) %>% arrange(year, parent.pop) #11 cases of ppt>25 in Aug 
```

```
## # A tibble: 11 × 13
## # Groups:   parent.pop, elevation.group, elev_m [5]
##    parent.pop elevation.group elev_m PckSum   Lat  Long  year month   cwd   pck
##    <chr>      <chr>            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 CC         Low               313   0.951  39.6 -121.  1965     8  70.3     0
##  2 TM2        Low               379.  5.47   39.6 -122.  1965     8 156.      0
##  3 CC         Low               313   0.951  39.6 -121.  1968     8  69.3     0
##  4 TM2        Low               379.  5.47   39.6 -122.  1968     8 151.      0
##  5 IH         Low               454. 17.7    39.1 -121.  1975     8  94.4     0
##  6 SC         Low               422.  0.597  38.4 -121.  1975     8 169.      0
##  7 BH         Low               511.  0.234  37.4 -120.  1976     8 168.      0
##  8 CC         Low               313   0.951  39.6 -121.  1976     8  78.0     0
##  9 IH         Low               454. 17.7    39.1 -121.  1976     8 152.      0
## 10 SC         Low               422.  0.597  38.4 -121.  1976     8 148.      0
## 11 TM2        Low               379.  5.47   39.6 -122.  1976     8 150.      0
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_pops_historical_years %>% filter(year==1965) %>% arrange(parent.pop)
```

```
## # A tibble: 60 × 13
## # Groups:   parent.pop, elevation.group, elev_m [5]
##    parent.pop elevation.group elev_m PckSum   Lat  Long  year month   cwd   pck
##    <chr>      <chr>            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 BH         Low               511.  0.234  37.4 -120.  1965     4  39.2     0
##  2 BH         Low               511.  0.234  37.4 -120.  1965     8  99.9     0
##  3 BH         Low               511.  0.234  37.4 -120.  1965    12  25.4     0
##  4 BH         Low               511.  0.234  37.4 -120.  1965     2  41.0     0
##  5 BH         Low               511.  0.234  37.4 -120.  1965     1  27.4     0
##  6 BH         Low               511.  0.234  37.4 -120.  1965     7 119.      0
##  7 BH         Low               511.  0.234  37.4 -120.  1965     6  91.2     0
##  8 BH         Low               511.  0.234  37.4 -120.  1965     3  57.2     0
##  9 BH         Low               511.  0.234  37.4 -120.  1965     5  70.8     0
## 10 BH         Low               511.  0.234  37.4 -120.  1965    11  45.2     0
## # ℹ 50 more rows
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_pops_historical_years %>% filter(year==1968) %>% arrange(parent.pop) 
```

```
## # A tibble: 60 × 13
## # Groups:   parent.pop, elevation.group, elev_m [5]
##    parent.pop elevation.group elev_m PckSum   Lat  Long  year month   cwd   pck
##    <chr>      <chr>            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 BH         Low               511.  0.234  37.4 -120.  1968     4  72.2     0
##  2 BH         Low               511.  0.234  37.4 -120.  1968     8 149.      0
##  3 BH         Low               511.  0.234  37.4 -120.  1968    12  26.2     0
##  4 BH         Low               511.  0.234  37.4 -120.  1968     2  44.7     0
##  5 BH         Low               511.  0.234  37.4 -120.  1968     1  28.7     0
##  6 BH         Low               511.  0.234  37.4 -120.  1968     7 135.      0
##  7 BH         Low               511.  0.234  37.4 -120.  1968     6 101.      0
##  8 BH         Low               511.  0.234  37.4 -120.  1968     3  59.1     0
##  9 BH         Low               511.  0.234  37.4 -120.  1968     5  73.7     0
## 10 BH         Low               511.  0.234  37.4 -120.  1968    11  41.9     0
## # ℹ 50 more rows
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_pops_historical_years %>% filter(year==1975) %>% arrange(parent.pop) 
```

```
## # A tibble: 60 × 13
## # Groups:   parent.pop, elevation.group, elev_m [5]
##    parent.pop elevation.group elev_m PckSum   Lat  Long  year month   cwd   pck
##    <chr>      <chr>            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 BH         Low               511.  0.234  37.4 -120.  1975     4  35.9     0
##  2 BH         Low               511.  0.234  37.4 -120.  1975     8 103.      0
##  3 BH         Low               511.  0.234  37.4 -120.  1975    12  29.4     0
##  4 BH         Low               511.  0.234  37.4 -120.  1975     2  37.2     0
##  5 BH         Low               511.  0.234  37.4 -120.  1975     1  28.3     0
##  6 BH         Low               511.  0.234  37.4 -120.  1975     7 121.      0
##  7 BH         Low               511.  0.234  37.4 -120.  1975     6  95.4     0
##  8 BH         Low               511.  0.234  37.4 -120.  1975     3  39.2     0
##  9 BH         Low               511.  0.234  37.4 -120.  1975     5  72.4     0
## 10 BH         Low               511.  0.234  37.4 -120.  1975    11  41.4     0
## # ℹ 50 more rows
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_pops_historical_years %>% filter(year==1976) %>% arrange(parent.pop)
```

```
## # A tibble: 60 × 13
## # Groups:   parent.pop, elevation.group, elev_m [5]
##    parent.pop elevation.group elev_m PckSum   Lat  Long  year month   cwd   pck
##    <chr>      <chr>            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 BH         Low               511.  0.234  37.4 -120.  1976     4  70.1     0
##  2 BH         Low               511.  0.234  37.4 -120.  1976     8 168.      0
##  3 BH         Low               511.  0.234  37.4 -120.  1976    12  30.1     0
##  4 BH         Low               511.  0.234  37.4 -120.  1976     2  42.6     0
##  5 BH         Low               511.  0.234  37.4 -120.  1976     1  29.4     0
##  6 BH         Low               511.  0.234  37.4 -120.  1976     7 166.      0
##  7 BH         Low               511.  0.234  37.4 -120.  1976     6 112.      0
##  8 BH         Low               511.  0.234  37.4 -120.  1976     3  57.1     0
##  9 BH         Low               511.  0.234  37.4 -120.  1976     5  79.7     0
## 10 BH         Low               511.  0.234  37.4 -120.  1976    11  44.8     0
## # ℹ 50 more rows
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#some cases where ppt remains high in following month 

#going to start in Sept though due to afterripening requirements 
growyear_months <- tibble(month=c(1:12), growmonth=c(5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4))

nosnow_historical_growyear_months <- full_join(growyear_months, nosnow_pops_historical_years)
```

```
## Joining with `by = join_by(month)`
```

``` r
dim(nosnow_historical_growyear_months)
```

```
## [1] 1800   14
```

First month 

``` r
nosnow_historical_first_month <- nosnow_historical_growyear_months %>%
  group_by(parent.pop, elevation.group, elev_m, year) %>% 
  filter(ppt>=25) %>% 
  arrange(growmonth) %>% 
  filter(row_number()==1) #get first month for each pop and year with germinating inducing rain 

nosnow_historical_first_month_tomerge <- nosnow_historical_first_month %>% 
  select(parent.pop:elev_m, year, firstmonth=growmonth)

nosnow_historical_first_month_col <- full_join(nosnow_historical_growyear_months, nosnow_historical_first_month_tomerge)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, year)`
```

``` r
dim(nosnow_historical_first_month_col)
```

```
## [1] 1800   15
```

Last month 

``` r
nosnow_historical_last_month <- nosnow_historical_growyear_months %>%
  group_by(parent.pop, elevation.group, elev_m, year) %>% 
  filter(cwd>83) %>% #adjusted for the historical 3rd quartile of cwd
  arrange(month) %>% 
  filter(row_number()==1)

nosnow_historical_last_month_tomerge <- nosnow_historical_last_month %>% 
  select(parent.pop:elev_m, year, lastmonth=growmonth) #last month is in grow month not calendar month format

nosnow_historical_last_month_col <- full_join(nosnow_historical_first_month_col, nosnow_historical_last_month_tomerge)  %>% 
  select(firstmonth, lastmonth, month:tmx)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, year)`
```

``` r
dim(nosnow_historical_last_month_col)
```

```
## [1] 1800   16
```

Checking for weird cases

``` r
summary(nosnow_historical_last_month_col)
```

```
##    firstmonth     lastmonth         month         growmonth    
##  Min.   :1.00   Min.   : 1.00   Min.   : 1.00   Min.   : 1.00  
##  1st Qu.:1.00   1st Qu.:10.00   1st Qu.: 3.75   1st Qu.: 3.75  
##  Median :2.00   Median :10.00   Median : 6.50   Median : 6.50  
##  Mean   :2.02   Mean   :10.11   Mean   : 6.50   Mean   : 6.50  
##  3rd Qu.:3.00   3rd Qu.:11.00   3rd Qu.: 9.25   3rd Qu.: 9.25  
##  Max.   :4.00   Max.   :12.00   Max.   :12.00   Max.   :12.00  
##   parent.pop        elevation.group        elev_m          PckSum       
##  Length:1800        Length:1800        Min.   :313.0   Min.   : 0.2340  
##  Class :character   Class :character   1st Qu.:379.2   1st Qu.: 0.5973  
##  Mode  :character   Mode  :character   Median :421.5   Median : 0.9510  
##                                        Mean   :415.8   Mean   : 4.9819  
##                                        3rd Qu.:454.1   3rd Qu.: 5.4673  
##                                        Max.   :511.4   Max.   :17.6597  
##       Lat             Long             year           cwd        
##  Min.   :37.41   Min.   :-121.6   Min.   :1964   Min.   :  0.00  
##  1st Qu.:38.41   1st Qu.:-121.4   1st Qu.:1971   1st Qu.: 27.53  
##  Median :39.09   Median :-120.9   Median :1978   Median : 51.52  
##  Mean   :38.82   Mean   :-120.9   Mean   :1978   Mean   : 61.98  
##  3rd Qu.:39.59   3rd Qu.:-120.7   3rd Qu.:1986   3rd Qu.: 88.80  
##  Max.   :39.59   Max.   :-120.0   Max.   :1993   Max.   :202.50  
##       pck                ppt               tmn              tmx       
##  Min.   :  0.0000   Min.   :  0.000   Min.   :-1.830   Min.   : 8.63  
##  1st Qu.:  0.0000   1st Qu.:  3.505   1st Qu.: 3.817   1st Qu.:15.29  
##  Median :  0.0000   Median : 36.370   Median : 7.665   Median :22.36  
##  Mean   :  0.4152   Mean   : 74.335   Mean   : 8.130   Mean   :22.59  
##  3rd Qu.:  0.0000   3rd Qu.:106.640   3rd Qu.:12.800   3rd Qu.:30.16  
##  Max.   :129.4600   Max.   :559.960   Max.   :18.820   Max.   :37.76
```

``` r
nosnow_historical_last_month_col %>% filter(is.na(lastmonth)) %>% arrange(year) #0 years where there isn't a last month
```

```
## # A tibble: 0 × 16
## # ℹ 16 variables: firstmonth <dbl>, lastmonth <dbl>, month <dbl>,
## #   growmonth <dbl>, parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   PckSum <dbl>, Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_historical_last_month_col %>% filter(is.na(firstmonth)) %>% arrange(year) #0 years where there isn't a first month
```

```
## # A tibble: 0 × 16
## # ℹ 16 variables: firstmonth <dbl>, lastmonth <dbl>, month <dbl>,
## #   growmonth <dbl>, parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   PckSum <dbl>, Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_historical_last_month_col %>% filter(lastmonth<4) %>% arrange(parent.pop, year) #some cases where last month is less than 4 (earlier than December)
```

```
## # A tibble: 72 × 16
##    firstmonth lastmonth month growmonth parent.pop elevation.group elev_m PckSum
##         <dbl>     <dbl> <dbl>     <dbl> <chr>      <chr>            <dbl>  <dbl>
##  1          3         1     1         5 IH         Low               454.   17.7
##  2          3         1     2         6 IH         Low               454.   17.7
##  3          3         1     3         7 IH         Low               454.   17.7
##  4          3         1     4         8 IH         Low               454.   17.7
##  5          3         1     5         9 IH         Low               454.   17.7
##  6          3         1     6        10 IH         Low               454.   17.7
##  7          3         1     7        11 IH         Low               454.   17.7
##  8          3         1     8        12 IH         Low               454.   17.7
##  9          3         1     9         1 IH         Low               454.   17.7
## 10          3         1    10         2 IH         Low               454.   17.7
## # ℹ 62 more rows
## # ℹ 8 more variables: Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_historical_last_month_col %>% filter(lastmonth<firstmonth, lastmonth<4) %>% arrange(parent.pop, year) #most of the above are when the last month is before the first month (in growyear, not calendar year)
```

```
## # A tibble: 60 × 16
##    firstmonth lastmonth month growmonth parent.pop elevation.group elev_m PckSum
##         <dbl>     <dbl> <dbl>     <dbl> <chr>      <chr>            <dbl>  <dbl>
##  1          3         1     1         5 IH         Low               454.   17.7
##  2          3         1     2         6 IH         Low               454.   17.7
##  3          3         1     3         7 IH         Low               454.   17.7
##  4          3         1     4         8 IH         Low               454.   17.7
##  5          3         1     5         9 IH         Low               454.   17.7
##  6          3         1     6        10 IH         Low               454.   17.7
##  7          3         1     7        11 IH         Low               454.   17.7
##  8          3         1     8        12 IH         Low               454.   17.7
##  9          3         1     9         1 IH         Low               454.   17.7
## 10          3         1    10         2 IH         Low               454.   17.7
## # ℹ 50 more rows
## # ℹ 8 more variables: Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#****NEED TO CONVERT LAST MONTH TO CALENDAR YEAR FORMAT FOR AVG CALCS 
nosnow_historical_last_month_col %>% filter(lastmonth>firstmonth & lastmonth<4) %>% arrange(parent.pop, year) #0 cases where the last month is after the first month before Dec
```

```
## # A tibble: 0 × 16
## # ℹ 16 variables: firstmonth <dbl>, lastmonth <dbl>, month <dbl>,
## #   growmonth <dbl>, parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   PckSum <dbl>, Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_historical_last_month_col %>% filter(lastmonth==firstmonth) %>% arrange(parent.pop, year) #first month and last month are the same for IH in 1 year ('89)
```

```
## # A tibble: 12 × 16
##    firstmonth lastmonth month growmonth parent.pop elevation.group elev_m PckSum
##         <dbl>     <dbl> <dbl>     <dbl> <chr>      <chr>            <dbl>  <dbl>
##  1          1         1     1         5 IH         Low               454.   17.7
##  2          1         1     2         6 IH         Low               454.   17.7
##  3          1         1     3         7 IH         Low               454.   17.7
##  4          1         1     4         8 IH         Low               454.   17.7
##  5          1         1     5         9 IH         Low               454.   17.7
##  6          1         1     6        10 IH         Low               454.   17.7
##  7          1         1     7        11 IH         Low               454.   17.7
##  8          1         1     8        12 IH         Low               454.   17.7
##  9          1         1     9         1 IH         Low               454.   17.7
## 10          1         1    10         2 IH         Low               454.   17.7
## 11          1         1    11         3 IH         Low               454.   17.7
## 12          1         1    12         4 IH         Low               454.   17.7
## # ℹ 8 more variables: Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#****CAN DROP THIS CASE 

nosnow_historical_last_month_col %>% filter(lastmonth==1) %>% arrange(parent.pop, year) #6 cases where Sept is last month 
```

```
## # A tibble: 72 × 16
##    firstmonth lastmonth month growmonth parent.pop elevation.group elev_m PckSum
##         <dbl>     <dbl> <dbl>     <dbl> <chr>      <chr>            <dbl>  <dbl>
##  1          3         1     1         5 IH         Low               454.   17.7
##  2          3         1     2         6 IH         Low               454.   17.7
##  3          3         1     3         7 IH         Low               454.   17.7
##  4          3         1     4         8 IH         Low               454.   17.7
##  5          3         1     5         9 IH         Low               454.   17.7
##  6          3         1     6        10 IH         Low               454.   17.7
##  7          3         1     7        11 IH         Low               454.   17.7
##  8          3         1     8        12 IH         Low               454.   17.7
##  9          3         1     9         1 IH         Low               454.   17.7
## 10          3         1    10         2 IH         Low               454.   17.7
## # ℹ 62 more rows
## # ℹ 8 more variables: Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_historical_last_month_col %>% filter(lastmonth==2) #0 cases where Oct-Dec is the last month
```

```
## # A tibble: 0 × 16
## # ℹ 16 variables: firstmonth <dbl>, lastmonth <dbl>, month <dbl>,
## #   growmonth <dbl>, parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   PckSum <dbl>, Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_historical_last_month_col %>% filter(firstmonth>4) #0 cases where first month is after December
```

```
## # A tibble: 0 × 16
## # ℹ 16 variables: firstmonth <dbl>, lastmonth <dbl>, month <dbl>,
## #   growmonth <dbl>, parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   PckSum <dbl>, Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
nosnow_historical_last_month_col %>% filter(growmonth==firstmonth+1, cwd>83) %>% arrange(parent.pop, year) #3 cases at BH where cwd is high in the second growth month 
```

```
## # A tibble: 3 × 16
##   firstmonth lastmonth month growmonth parent.pop elevation.group elev_m PckSum
##        <dbl>     <dbl> <dbl>     <dbl> <chr>      <chr>            <dbl>  <dbl>
## 1          1        10    10         2 BH         Low               511.  0.234
## 2          1        10    10         2 BH         Low               511.  0.234
## 3          1        10    10         2 BH         Low               511.  0.234
## # ℹ 8 more variables: Lat <dbl>, Long <dbl>, year <dbl>, cwd <dbl>, pck <dbl>,
## #   ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#nosnow_historical_last_month_col %>% filter(growmonth==lastmonth) %>% arrange(cwd, ppt)
```

Average first and last month

``` r
#remove IH: 1989
#convert last month to cal year for avg calc 

nosnow_historicalfirst_last_month_for_calc <- nosnow_historical_last_month_col %>% 
  filter(!(parent.pop=="IH" & year==1989)) %>% 
  mutate(lastmonth_cal=if_else(lastmonth<=4, lastmonth+8, lastmonth-4)) %>% 
  select(firstmonth:lastmonth, lastmonth_cal, parent.pop, elevation.group, elev_m, year) %>% 
  distinct()

nosnow_historical_avg_first_last_month <- nosnow_historicalfirst_last_month_for_calc %>% 
  group_by(parent.pop, elevation.group, elev_m) %>% 
  summarise(AvgFirstMonth=mean(firstmonth), First_sem=sem(firstmonth),
            AvgLastMonth=mean(lastmonth), Last_sem=sem(lastmonth),
            AvgLastMonth_cal=mean(lastmonth_cal), Last_cal_sem=sem(lastmonth_cal))
```

```
## `summarise()` has grouped output by 'parent.pop', 'elevation.group'. You can
## override using the `.groups` argument.
```

``` r
nosnow_historical_avg_first_last_month
```

```
## # A tibble: 5 × 9
## # Groups:   parent.pop, elevation.group [5]
##   parent.pop elevation.group elev_m AvgFirstMonth First_sem AvgLastMonth
##   <chr>      <chr>            <dbl>         <dbl>     <dbl>        <dbl>
## 1 BH         Low               511.          2.3      0.153        10.2 
## 2 CC         Low               313           1.93     0.143        10.3 
## 3 IH         Low               454.          1.90     0.135        10.0 
## 4 SC         Low               422.          2.2      0.139        10.3 
## 5 TM2        Low               379.          1.8      0.121         9.97
## # ℹ 3 more variables: Last_sem <dbl>, AvgLastMonth_cal <dbl>,
## #   Last_cal_sem <dbl>
```

``` r
#2 in growmonth = October 
#10 in growmonth = June - w/o converting last month to calendar year format, the cases where growmonth=1 is pulling the last month much earlier and with a much greater standard error --> wrong!
#8 in lastmonth_cal = August 

#Used Standard Rounding
nosnow_avg_first_last_month_historical <- nosnow_historical_avg_first_last_month %>% 
  mutate(AvgFirstMonth=true_round(AvgFirstMonth, 0), 
         AvgLastMonth=true_round(AvgLastMonth_cal, 0) + 4) %>% #avg in calendar year first to account for cases where CC and TM2 had a last month of Sept, then convert back to grow month format
  select(parent.pop:elev_m, AvgFirstMonth, AvgLastMonth)
nosnow_historical_avg_first_last_month
```

```
## # A tibble: 5 × 9
## # Groups:   parent.pop, elevation.group [5]
##   parent.pop elevation.group elev_m AvgFirstMonth First_sem AvgLastMonth
##   <chr>      <chr>            <dbl>         <dbl>     <dbl>        <dbl>
## 1 BH         Low               511.          2.3      0.153        10.2 
## 2 CC         Low               313           1.93     0.143        10.3 
## 3 IH         Low               454.          1.90     0.135        10.0 
## 4 SC         Low               422.          2.2      0.139        10.3 
## 5 TM2        Low               379.          1.8      0.121         9.97
## # ℹ 3 more variables: Last_sem <dbl>, AvgLastMonth_cal <dbl>,
## #   Last_cal_sem <dbl>
```

``` r
nosnow_avg_first_last_month_historical
```

```
## # A tibble: 5 × 5
## # Groups:   parent.pop, elevation.group [5]
##   parent.pop elevation.group elev_m AvgFirstMonth AvgLastMonth
##   <chr>      <chr>            <dbl>         <dbl>        <dbl>
## 1 BH         Low               511.             2           10
## 2 CC         Low               313              2           10
## 3 IH         Low               454.             2           12
## 4 SC         Low               422.             2           10
## 5 TM2        Low               379.             2           10
```

``` r
nosnow_historical_avg_first_last_month_allmonths <- left_join(nosnow_historical_growyear_months, nosnow_avg_first_last_month_historical)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m)`
```

Fill in all the months b/t the first and last for the full growth season 

``` r
nosnow_grwseason_historical <- nosnow_historical_avg_first_last_month_allmonths %>% 
  group_by(parent.pop, elevation.group, elev_m, year) %>% 
  filter(growmonth>AvgFirstMonth) %>% #first and last month are in grow month format not calendar year 
  filter(growmonth<=AvgLastMonth) %>% 
  arrange(year,parent.pop, growmonth)
summary(nosnow_grwseason_historical) 
```

```
##      month          growmonth       parent.pop        elevation.group   
##  Min.   : 1.000   Min.   : 3.000   Length:1260        Length:1260       
##  1st Qu.: 3.000   1st Qu.: 5.000   Class :character   Class :character  
##  Median : 5.000   Median : 7.000   Mode  :character   Mode  :character  
##  Mean   : 5.595   Mean   : 6.738                                        
##  3rd Qu.: 8.000   3rd Qu.: 9.000                                        
##  Max.   :12.000   Max.   :12.000                                        
##      elev_m          PckSum             Lat             Long       
##  Min.   :313.0   Min.   : 0.2340   Min.   :37.41   Min.   :-121.6  
##  1st Qu.:379.2   1st Qu.: 0.5973   1st Qu.:38.41   1st Qu.:-121.4  
##  Median :421.5   Median : 0.9510   Median :39.09   Median :-120.9  
##  Mean   :417.7   Mean   : 5.5856   Mean   :38.83   Mean   :-120.9  
##  3rd Qu.:454.1   3rd Qu.: 5.4673   3rd Qu.:39.59   3rd Qu.:-120.7  
##  Max.   :511.4   Max.   :17.6597   Max.   :39.59   Max.   :-120.0  
##       year           cwd              pck                ppt        
##  Min.   :1964   Min.   :  0.00   Min.   :  0.0000   Min.   :  0.00  
##  1st Qu.:1971   1st Qu.: 21.45   1st Qu.:  0.0000   1st Qu.: 16.46  
##  Median :1978   Median : 33.62   Median :  0.0000   Median : 60.99  
##  Mean   :1978   Mean   : 42.20   Mean   :  0.5931   Mean   : 97.60  
##  3rd Qu.:1986   3rd Qu.: 58.30   3rd Qu.:  0.0000   3rd Qu.:147.46  
##  Max.   :1993   Max.   :183.92   Max.   :129.4600   Max.   :559.96  
##       tmn              tmx        AvgFirstMonth  AvgLastMonth  
##  Min.   :-1.830   Min.   : 8.63   Min.   :2     Min.   :10.00  
##  1st Qu.: 2.748   1st Qu.:13.80   1st Qu.:2     1st Qu.:10.00  
##  Median : 5.005   Median :17.23   Median :2     Median :10.00  
##  Mean   : 6.026   Mean   :19.25   Mean   :2     Mean   :10.48  
##  3rd Qu.: 8.822   3rd Qu.:24.70   3rd Qu.:2     3rd Qu.:10.00  
##  Max.   :17.940   Max.   :36.23   Max.   :2     Max.   :12.00
```

``` r
xtabs(~parent.pop+month, data=nosnow_grwseason_historical)
```

```
##           month
## parent.pop  1  2  3  4  5  6  7  8 11 12
##        BH  30 30 30 30 30 30  0  0 30 30
##        CC  30 30 30 30 30 30  0  0 30 30
##        IH  30 30 30 30 30 30 30 30 30 30
##        SC  30 30 30 30 30 30  0  0 30 30
##        TM2 30 30 30 30 30 30  0  0 30 30
```

``` r
nosnow_grwseason_historical %>% ggplot(aes(x=month)) + geom_histogram() +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5,6,7,8,9,10, 11, 12)) +
  facet_wrap(~parent.pop)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

``` r
#note this includes a UCD_Garden growing season that uses the same season criteria as the other low elevation pops, but CWD is higher at UCD so the last month of the growing season is probably later than this indicates
```

## Populations that get more than 70 mm of snow pack in a year (on average)

-   First month = snowpack = 0 and min temp > 0

    -   Remember this month is not included in the final "growth season"

-   Last month = snowpack. \> 70 mm OR pck > 0 and min temp < 0 OR min temp < -5 (moderate freeze)

Note: “The month name of the snowpack file (that is, pckmar.asc) relates to the first day of the next month (that is, station observations on April 1st correlate with the snowpack file for March)." But this makes sense, a survey on April 1 gives a sense for the total snowpack in March. 

### Recent climate

Prep

``` r
snow_pops_recent <- pop_elev_climate_recent_avgs %>% filter(PckSum >= 70)
unique(snow_pops_recent$parent.pop) #18 pops get some significant snowpack per year 
```

```
##  [1] "CP2"   "CP3"   "DPR"   "FR"    "LV1"   "LV3"   "LVTR1" "SQ1"   "SQ2"  
## [10] "SQ3"   "WL1"   "WL2"   "WR"    "WV"    "YO11"  "YO4"   "YO7"   "YO8"
```

``` r
summary(snow_pops_recent)
```

```
##   parent.pop        elevation.group        elev_m           month      
##  Length:216         Length:216         Min.   : 748.9   Min.   : 1.00  
##  Class :character   Class :character   1st Qu.:1613.8   1st Qu.: 3.75  
##  Mode  :character   Mode  :character   Median :2200.9   Median : 6.50  
##                                        Mean   :1992.5   Mean   : 6.50  
##                                        3rd Qu.:2470.0   3rd Qu.: 9.25  
##                                        Max.   :2872.3   Max.   :12.00  
##     cwd_mean          pck_mean         ppt_mean           tmn_mean     
##  Min.   :  9.313   Min.   :   0.0   Min.   :  0.1547   Min.   :-9.563  
##  1st Qu.: 27.271   1st Qu.:   0.0   1st Qu.: 17.7187   1st Qu.:-3.246  
##  Median : 46.023   Median :  30.1   Median : 84.9597   Median : 1.320  
##  Mean   : 54.455   Mean   : 174.7   Mean   :105.3230   Mean   : 2.051  
##  3rd Qu.: 80.181   3rd Qu.: 255.7   3rd Qu.:177.2308   3rd Qu.: 7.182  
##  Max.   :148.638   Max.   :1118.4   Max.   :357.2016   Max.   :16.472  
##     tmx_mean         cwd_sem          pck_sem          ppt_sem        
##  Min.   : 1.908   Min.   :0.1325   Min.   :  0.00   Min.   : 0.07985  
##  1st Qu.: 8.211   1st Qu.:0.5525   1st Qu.:  0.00   1st Qu.: 3.67177  
##  Median :13.203   Median :1.2497   Median : 12.06   Median :13.29838  
##  Mean   :14.722   Mean   :1.6800   Mean   : 25.98   Mean   :15.38336  
##  3rd Qu.:21.506   3rd Qu.:2.4669   3rd Qu.: 50.47   3rd Qu.:26.08435  
##  Max.   :32.902   Max.   :8.5152   Max.   :107.93   Max.   :48.21752  
##     tmn_sem          tmx_sem           PckSum       
##  Min.   :0.1805   Min.   :0.1917   Min.   :  91.58  
##  1st Qu.:0.2616   1st Qu.:0.2992   1st Qu.: 671.23  
##  Median :0.2866   Median :0.3925   Median :1728.15  
##  Mean   :0.2895   Mean   :0.3671   Mean   :2096.00  
##  3rd Qu.:0.3170   3rd Qu.:0.4250   3rd Qu.:2832.44  
##  Max.   :0.4096   Max.   :0.5530   Max.   :5447.93
```

``` r
snow_pops_recent_tojoin <- snow_pops_recent %>% select(parent.pop:elev_m, PckSum) %>% distinct()

snow_pops_recent_years <- left_join(snow_pops_recent_tojoin, pop_elev_climate_recent) 
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m)`
```

``` r
summary(snow_pops_recent_years)
```

```
##   parent.pop        elevation.group        elev_m           PckSum       
##  Length:6480        Length:6480        Min.   : 748.9   Min.   :  91.58  
##  Class :character   Class :character   1st Qu.:1613.8   1st Qu.: 671.23  
##  Mode  :character   Mode  :character   Median :2200.9   Median :1728.15  
##                                        Mean   :1992.5   Mean   :2096.00  
##                                        3rd Qu.:2470.0   3rd Qu.:2832.44  
##                                        Max.   :2872.3   Max.   :5447.93  
##       Lat             Long             year          month      
##  Min.   :36.56   Min.   :-123.0   Min.   :1994   Min.   : 1.00  
##  1st Qu.:37.81   1st Qu.:-121.2   1st Qu.:2001   1st Qu.: 3.75  
##  Median :38.75   Median :-120.2   Median :2008   Median : 6.50  
##  Mean   :38.72   Mean   :-120.3   Mean   :2008   Mean   : 6.50  
##  3rd Qu.:40.01   3rd Qu.:-119.5   3rd Qu.:2016   3rd Qu.: 9.25  
##  Max.   :40.74   Max.   :-118.8   Max.   :2023   Max.   :12.00  
##       cwd              pck              ppt               tmn         
##  Min.   :  0.00   Min.   :   0.0   Min.   :  0.000   Min.   :-13.180  
##  1st Qu.: 25.39   1st Qu.:   0.0   1st Qu.:  9.428   1st Qu.: -3.190  
##  Median : 46.06   Median :   0.0   Median : 49.415   Median :  1.540  
##  Mean   : 54.46   Mean   : 174.7   Mean   :105.323   Mean   :  2.051  
##  3rd Qu.: 80.44   3rd Qu.: 205.0   3rd Qu.:150.970   3rd Qu.:  7.040  
##  Max.   :182.70   Max.   :2183.6   Max.   :981.420   Max.   : 19.730  
##       tmx        
##  Min.   :-3.570  
##  1st Qu.: 8.047  
##  Median :13.550  
##  Mean   :14.722  
##  3rd Qu.:21.460  
##  Max.   :35.130
```

First month 

``` r
snow_recent_first_month <- snow_pops_recent_years %>%
  group_by(parent.pop, elevation.group, elev_m, year) %>% 
  filter(pck==0) %>% 
  filter(tmn > 0) %>% 
  arrange(month) %>% 
  filter(row_number()==1) #get first month for each pop and year with no snowpack for germ

snow_recent_first_month_tomerge <- snow_recent_first_month %>% 
  select(parent.pop:elev_m, year, firstmonth=month)

snow_recent_first_month_col <- full_join(snow_pops_recent_years, snow_recent_first_month_tomerge)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, year)`
```

``` r
dim(snow_recent_first_month_col)
```

```
## [1] 6480   14
```

Last month

``` r
snow_recent_last_month <- snow_recent_first_month_col %>%
  group_by(parent.pop, elevation.group, elev_m, year) %>% 
  filter(month>firstmonth) %>% 
  filter(if_else(pck>70, pck>70, 
                 if_else(pck>0, tmn < 0, tmn <= -5))) %>%  #-5 is moderate freeze (Pardee et al. 2017)
  arrange(month) %>% 
  filter(row_number()==1) #get first month after growstart for each pop and year with pck >70 OR pck>0 & tmn <0 OR tmn <-5

#snow_recent_last_month %>% filter(tmn <=-5, pck <=0)
#snow_recent_last_month %>% filter(pck >0, tmn < 0)

snow_recent_last_month_tomerge <- snow_recent_last_month %>% 
  select(parent.pop:elev_m, year, firstmonth,lastmonth=month)

snow_recent_last_month_col <- full_join(snow_recent_first_month_col, snow_recent_last_month_tomerge) %>% 
  select(firstmonth, lastmonth, year:tmx)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, year,
## firstmonth)`
## Adding missing grouping variables: `parent.pop`, `elevation.group`, `elev_m`
```

``` r
dim(snow_recent_last_month_col)
```

```
## [1] 6480   12
```

Check weird cases

``` r
summary(snow_recent_last_month_col)
```

```
##   parent.pop        elevation.group        elev_m         firstmonth   
##  Length:6480        Length:6480        Min.   : 748.9   Min.   :1.000  
##  Class :character   Class :character   1st Qu.:1613.8   1st Qu.:4.000  
##  Mode  :character   Mode  :character   Median :2200.9   Median :5.000  
##                                        Mean   :1992.5   Mean   :4.824  
##                                        3rd Qu.:2470.0   3rd Qu.:6.000  
##                                        Max.   :2872.3   Max.   :8.000  
##                                                                        
##    lastmonth          year          month            cwd        
##  Min.   : 2.00   Min.   :1994   Min.   : 1.00   Min.   :  0.00  
##  1st Qu.:11.00   1st Qu.:2001   1st Qu.: 3.75   1st Qu.: 25.39  
##  Median :12.00   Median :2008   Median : 6.50   Median : 46.06  
##  Mean   :11.31   Mean   :2008   Mean   : 6.50   Mean   : 54.46  
##  3rd Qu.:12.00   3rd Qu.:2016   3rd Qu.: 9.25   3rd Qu.: 80.44  
##  Max.   :12.00   Max.   :2023   Max.   :12.00   Max.   :182.70  
##  NA's   :1404                                                   
##       pck              ppt               tmn               tmx        
##  Min.   :   0.0   Min.   :  0.000   Min.   :-13.180   Min.   :-3.570  
##  1st Qu.:   0.0   1st Qu.:  9.428   1st Qu.: -3.190   1st Qu.: 8.047  
##  Median :   0.0   Median : 49.415   Median :  1.540   Median :13.550  
##  Mean   : 174.7   Mean   :105.323   Mean   :  2.051   Mean   :14.722  
##  3rd Qu.: 205.0   3rd Qu.:150.970   3rd Qu.:  7.040   3rd Qu.:21.460  
##  Max.   :2183.6   Max.   :981.420   Max.   : 19.730   Max.   :35.130  
## 
```

``` r
snow_recent_last_month_col %>% filter(is.na(firstmonth)) #no cases where there isn't a firstmonth
```

```
## # A tibble: 0 × 12
## # Groups:   parent.pop, elevation.group, elev_m [0]
## # ℹ 12 variables: parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   firstmonth <dbl>, lastmonth <dbl>, year <dbl>, month <dbl>, cwd <dbl>,
## #   pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
snow_recent_last_month_col %>% filter(is.na(lastmonth)) #117 cases where there isn't a lastmonth 
```

```
## # A tibble: 1,404 × 12
## # Groups:   parent.pop, elevation.group, elev_m [13]
##    parent.pop elevation.group elev_m firstmonth lastmonth  year month   cwd
##    <chr>      <chr>            <dbl>      <dbl>     <dbl> <dbl> <dbl> <dbl>
##  1 CP2        High             2244.          6        NA  2011     4  34.5
##  2 CP2        High             2244.          6        NA  2011     8 122. 
##  3 CP2        High             2244.          6        NA  2011    12  21.2
##  4 CP2        High             2244.          6        NA  2011     2  27.1
##  5 CP2        High             2244.          6        NA  2011     1  23.5
##  6 CP2        High             2244.          6        NA  2011     7  84.2
##  7 CP2        High             2244.          6        NA  2011     6  63.0
##  8 CP2        High             2244.          6        NA  2011     3  19.9
##  9 CP2        High             2244.          6        NA  2011     5  30.3
## 10 CP2        High             2244.          6        NA  2011    11  28.9
## # ℹ 1,394 more rows
## # ℹ 4 more variables: pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#***WHAT TO DO FOR THESE CASES? Make the last month 12 (Dec)
#*no snow_pack, or min temp > -5 

snow_recent_last_month_col %>% filter(lastmonth==firstmonth) %>% arrange(parent.pop, year) #no cases where last month is first month 
```

```
## # A tibble: 0 × 12
## # Groups:   parent.pop, elevation.group, elev_m [0]
## # ℹ 12 variables: parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   firstmonth <dbl>, lastmonth <dbl>, year <dbl>, month <dbl>, cwd <dbl>,
## #   pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
snow_recent_last_month_col %>% filter(lastmonth==firstmonth+1) %>% arrange(parent.pop, year) 
```

```
## # A tibble: 144 × 12
## # Groups:   parent.pop, elevation.group, elev_m [6]
##    parent.pop elevation.group elev_m firstmonth lastmonth  year month   cwd
##    <chr>      <chr>            <dbl>      <dbl>     <dbl> <dbl> <dbl> <dbl>
##  1 DPR        Mid              1019.          1         2  1994     4 43.3 
##  2 DPR        Mid              1019.          1         2  1994     8 39.7 
##  3 DPR        Mid              1019.          1         2  1994    12  9.24
##  4 DPR        Mid              1019.          1         2  1994     2 18.3 
##  5 DPR        Mid              1019.          1         2  1994     1 11.4 
##  6 DPR        Mid              1019.          1         2  1994     7 39.2 
##  7 DPR        Mid              1019.          1         2  1994     6 41   
##  8 DPR        Mid              1019.          1         2  1994     3 30.1 
##  9 DPR        Mid              1019.          1         2  1994     5 40.5 
## 10 DPR        Mid              1019.          1         2  1994    11 10.8 
## # ℹ 134 more rows
## # ℹ 4 more variables: pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#***I added a temperature requirement for temp >0 for the first month and still get 12 cases where there is heavy snow in the month after the first month 
snow_recent_last_month_col %>% filter(lastmonth==firstmonth+2) #1 case where there was sig snowpack in the second month after the first month of 0 snowpack (WL1 - 2018)
```

```
## # A tibble: 12 × 12
## # Groups:   parent.pop, elevation.group, elev_m [1]
##    parent.pop elevation.group elev_m firstmonth lastmonth  year month   cwd
##    <chr>      <chr>            <dbl>      <dbl>     <dbl> <dbl> <dbl> <dbl>
##  1 WL1        Mid              1614.          1         3  2018     4  44.4
##  2 WL1        Mid              1614.          1         3  2018     8 105. 
##  3 WL1        Mid              1614.          1         3  2018    12  20.6
##  4 WL1        Mid              1614.          1         3  2018     2  30  
##  5 WL1        Mid              1614.          1         3  2018     1  21.2
##  6 WL1        Mid              1614.          1         3  2018     7 110. 
##  7 WL1        Mid              1614.          1         3  2018     6  98.1
##  8 WL1        Mid              1614.          1         3  2018     3  28.8
##  9 WL1        Mid              1614.          1         3  2018     5  80.3
## 10 WL1        Mid              1614.          1         3  2018    11  32.6
## 11 WL1        Mid              1614.          1         3  2018    10  68.0
## 12 WL1        Mid              1614.          1         3  2018     9  92.3
## # ℹ 4 more variables: pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
heavysnow_recent <- snow_recent_last_month_col %>% filter(lastmonth==firstmonth+1) %>% filter(month==firstmonth)
summary(heavysnow_recent)
```

```
##   parent.pop        elevation.group        elev_m       firstmonth   
##  Length:12          Length:12          Min.   : 787   Min.   :1.000  
##  Class :character   Class :character   1st Qu.:1019   1st Qu.:1.000  
##  Mode  :character   Mode  :character   Median :1158   Median :1.000  
##                                        Mean   :1312   Mean   :1.083  
##                                        3rd Qu.:1614   3rd Qu.:1.000  
##                                        Max.   :2158   Max.   :2.000  
##    lastmonth          year          month            cwd             pck   
##  Min.   :2.000   Min.   :1994   Min.   :1.000   Min.   : 6.28   Min.   :0  
##  1st Qu.:2.000   1st Qu.:1998   1st Qu.:1.000   1st Qu.:13.95   1st Qu.:0  
##  Median :2.000   Median :2008   Median :1.000   Median :16.95   Median :0  
##  Mean   :2.083   Mean   :2006   Mean   :1.083   Mean   :17.17   Mean   :0  
##  3rd Qu.:2.000   3rd Qu.:2014   3rd Qu.:1.000   3rd Qu.:21.32   3rd Qu.:0  
##  Max.   :3.000   Max.   :2019   Max.   :2.000   Max.   :30.69   Max.   :0  
##       ppt              tmn             tmx       
##  Min.   : 52.89   Min.   :0.080   Min.   :10.42  
##  1st Qu.: 69.62   1st Qu.:0.275   1st Qu.:11.27  
##  Median : 99.40   Median :1.170   Median :11.48  
##  Mean   :161.98   Mean   :1.176   Mean   :11.88  
##  3rd Qu.:225.58   3rd Qu.:1.833   3rd Qu.:12.10  
##  Max.   :397.32   Max.   :2.910   Max.   :14.15
```

``` r
unique(heavysnow_recent$parent.pop)
```

```
## [1] "DPR" "FR"  "WL1" "WL2" "WR"  "YO4"
```

``` r
heavysnow_recent %>% filter(elev_m>1800)
```

```
## # A tibble: 2 × 12
## # Groups:   parent.pop, elevation.group, elev_m [2]
##   parent.pop elevation.group elev_m firstmonth lastmonth  year month   cwd   pck
##   <chr>      <chr>            <dbl>      <dbl>     <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 WL2        High             2020.          1         2  2014     1  23.6     0
## 2 YO4        High             2158.          1         2  2014     1  30.7     0
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#"DPR" "WL1" "WL2" "WR", "YO4" 
#*Temps range from just above freezing (0.080) to 2.9 (which seems like an acceptable temp given we vernalize at 4C)
#*OR START THE CHECK IN FEB FOR POPS > 6000 FT (ONLY 2 ABOVE 6000 FEET, both in 2014)
#*OR just take these cases out
```

Average first and last month

``` r
#for cases with no last month - make last month 12
#remove cases where last month is first month +1 or 2 

snow_recentfirst_last_month_for_calc <- snow_recent_last_month_col %>% 
  mutate(lastmonth=if_else(is.na(lastmonth), 12, lastmonth)) %>% 
  filter(lastmonth > firstmonth+2) %>% 
  select(firstmonth:lastmonth, parent.pop, elevation.group, elev_m, year) %>% 
  distinct()
#snow_recentfirst_last_month_for_calc %>% filter(parent.pop=="CP2", year=="2011") #converting last month to 12 worked for the cases where it was NA before 

snow_recent_avg_first_last_month <- snow_recentfirst_last_month_for_calc %>% 
  group_by(parent.pop, elevation.group, elev_m) %>% 
  summarise(AvgFirstMonth=mean(firstmonth), First_sem=sem(firstmonth),
            AvgLastMonth=mean(lastmonth), Last_sem=sem(lastmonth)) %>% 
  arrange(elevation.group, elev_m)
```

```
## `summarise()` has grouped output by 'parent.pop', 'elevation.group'. You can
## override using the `.groups` argument.
```

``` r
snow_recent_avg_first_last_month
```

```
## # A tibble: 18 × 7
## # Groups:   parent.pop, elevation.group [18]
##    parent.pop elevation.group elev_m AvgFirstMonth First_sem AvgLastMonth
##    <chr>      <chr>            <dbl>         <dbl>     <dbl>        <dbl>
##  1 WL2        High             2020.          5.21    0.160          11.8
##  2 YO4        High             2158.          5.03    0.175          11.8
##  3 CP2        High             2244.          5.57    0.133          11.7
##  4 CP3        High             2266.          5.73    0.135          11.6
##  5 LV3        High             2354.          6.43    0.133          11.1
##  6 SQ3        High             2373.          5.37    0.122          11.8
##  7 YO7        High             2470.          5.87    0.124          11.6
##  8 YO8        High             2591.          6.03    0.102          11.6
##  9 LV1        High             2593.          6.43    0.133          11.2
## 10 LVTR1      High             2741.          6.43    0.133          11.1
## 11 YO11       High             2872.          6.13    0.0926         11.4
## 12 WV         Mid               749.          3.13    0.178          12.0
## 13 FR         Mid               787           2.59    0.182          12  
## 14 DPR        Mid              1019.          1.77    0.178          12  
## 15 WR         Mid              1158           2.85    0.260          12  
## 16 WL1        Mid              1614.          4.11    0.229          12.0
## 17 SQ1        Mid              1921.          4.37    0.206          12.0
## 18 SQ2        Mid              1934.          4.67    0.154          12.0
## # ℹ 1 more variable: Last_sem <dbl>
```

``` r
#Start = May-June for "high" elev, Feb-May for "mid" elevation 
#End = Nov-Dec

#Used Standard Rounding
snow_avg_first_last_month_recent <- snow_recent_avg_first_last_month %>% 
  mutate(AvgFirstMonth=true_round(AvgFirstMonth, 0), AvgLastMonth=true_round(AvgLastMonth, 0)) %>% 
  select(parent.pop:elev_m, AvgFirstMonth, AvgLastMonth)
snow_recent_avg_first_last_month
```

```
## # A tibble: 18 × 7
## # Groups:   parent.pop, elevation.group [18]
##    parent.pop elevation.group elev_m AvgFirstMonth First_sem AvgLastMonth
##    <chr>      <chr>            <dbl>         <dbl>     <dbl>        <dbl>
##  1 WL2        High             2020.          5.21    0.160          11.8
##  2 YO4        High             2158.          5.03    0.175          11.8
##  3 CP2        High             2244.          5.57    0.133          11.7
##  4 CP3        High             2266.          5.73    0.135          11.6
##  5 LV3        High             2354.          6.43    0.133          11.1
##  6 SQ3        High             2373.          5.37    0.122          11.8
##  7 YO7        High             2470.          5.87    0.124          11.6
##  8 YO8        High             2591.          6.03    0.102          11.6
##  9 LV1        High             2593.          6.43    0.133          11.2
## 10 LVTR1      High             2741.          6.43    0.133          11.1
## 11 YO11       High             2872.          6.13    0.0926         11.4
## 12 WV         Mid               749.          3.13    0.178          12.0
## 13 FR         Mid               787           2.59    0.182          12  
## 14 DPR        Mid              1019.          1.77    0.178          12  
## 15 WR         Mid              1158           2.85    0.260          12  
## 16 WL1        Mid              1614.          4.11    0.229          12.0
## 17 SQ1        Mid              1921.          4.37    0.206          12.0
## 18 SQ2        Mid              1934.          4.67    0.154          12.0
## # ℹ 1 more variable: Last_sem <dbl>
```

``` r
snow_avg_first_last_month_recent
```

```
## # A tibble: 18 × 5
## # Groups:   parent.pop, elevation.group [18]
##    parent.pop elevation.group elev_m AvgFirstMonth AvgLastMonth
##    <chr>      <chr>            <dbl>         <dbl>        <dbl>
##  1 WL2        High             2020.             5           12
##  2 YO4        High             2158.             5           12
##  3 CP2        High             2244.             6           12
##  4 CP3        High             2266.             6           12
##  5 LV3        High             2354.             6           11
##  6 SQ3        High             2373.             5           12
##  7 YO7        High             2470.             6           12
##  8 YO8        High             2591.             6           12
##  9 LV1        High             2593.             6           11
## 10 LVTR1      High             2741.             6           11
## 11 YO11       High             2872.             6           11
## 12 WV         Mid               749.             3           12
## 13 FR         Mid               787              3           12
## 14 DPR        Mid              1019.             2           12
## 15 WR         Mid              1158              3           12
## 16 WL1        Mid              1614.             4           12
## 17 SQ1        Mid              1921.             4           12
## 18 SQ2        Mid              1934.             5           12
```

``` r
snow_recent_avg_first_last_month_allmonths <- left_join(snow_pops_recent_years, snow_avg_first_last_month_recent)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m)`
```

Fill in months b/t start and stop 

``` r
snow_grwseason_recent <- snow_recent_avg_first_last_month_allmonths %>% 
  group_by(parent.pop, elevation.group, elev_m, year) %>% 
  filter(month>AvgFirstMonth) %>% 
  filter(month<=AvgLastMonth)
summary(snow_grwseason_recent) 
```

```
##   parent.pop        elevation.group        elev_m           PckSum       
##  Length:3750        Length:3750        Min.   : 748.9   Min.   :  91.58  
##  Class :character   Class :character   1st Qu.:1158.0   1st Qu.: 373.57  
##  Mode  :character   Mode  :character   Median :2020.1   Median :1579.99  
##                                        Mean   :1858.7   Mean   :1757.83  
##                                        3rd Qu.:2373.2   3rd Qu.:2808.97  
##                                        Max.   :2872.3   Max.   :5447.93  
##       Lat             Long             year          month       
##  Min.   :36.56   Min.   :-123.0   Min.   :1994   Min.   : 3.000  
##  1st Qu.:37.81   1st Qu.:-121.2   1st Qu.:2001   1st Qu.: 7.000  
##  Median :38.79   Median :-120.2   Median :2008   Median : 9.000  
##  Mean   :38.72   Mean   :-120.3   Mean   :2008   Mean   : 8.696  
##  3rd Qu.:40.01   3rd Qu.:-119.5   3rd Qu.:2016   3rd Qu.:11.000  
##  Max.   :40.74   Max.   :-118.8   Max.   :2023   Max.   :12.000  
##       cwd              pck              ppt               tmn         
##  Min.   :  0.00   Min.   :  0.00   Min.   :  0.000   Min.   :-11.090  
##  1st Qu.: 32.83   1st Qu.:  0.00   1st Qu.:  2.788   1st Qu.:  1.170  
##  Median : 66.43   Median :  0.00   Median : 21.115   Median :  5.915  
##  Mean   : 66.51   Mean   : 19.39   Mean   : 67.112   Mean   :  5.394  
##  3rd Qu.: 93.34   3rd Qu.:  0.00   3rd Qu.: 84.797   3rd Qu.:  9.918  
##  Max.   :182.70   Max.   :637.09   Max.   :894.020   Max.   : 19.730  
##       tmx        AvgFirstMonth    AvgLastMonth  
##  Min.   :-0.10   Min.   :2.000   Min.   :11.00  
##  1st Qu.:12.61   1st Qu.:3.000   1st Qu.:12.00  
##  Median :20.12   Median :5.000   Median :12.00  
##  Mean   :18.75   Mean   :4.552   Mean   :11.84  
##  3rd Qu.:24.19   3rd Qu.:6.000   3rd Qu.:12.00  
##  Max.   :35.13   Max.   :6.000   Max.   :12.00
```

``` r
xtabs(~parent.pop+month, data=snow_grwseason_recent)
```

```
##           month
## parent.pop  3  4  5  6  7  8  9 10 11 12
##      CP2    0  0  0  0 30 30 30 30 30 30
##      CP3    0  0  0  0 30 30 30 30 30 30
##      DPR   30 30 30 30 30 30 30 30 30 30
##      FR     0 30 30 30 30 30 30 30 30 30
##      LV1    0  0  0  0 30 30 30 30 30  0
##      LV3    0  0  0  0 30 30 30 30 30  0
##      LVTR1  0  0  0  0 30 30 30 30 30  0
##      SQ1    0  0 30 30 30 30 30 30 30 30
##      SQ2    0  0  0 30 30 30 30 30 30 30
##      SQ3    0  0  0 30 30 30 30 30 30 30
##      WL1    0  0 30 30 30 30 30 30 30 30
##      WL2    0  0  0 30 30 30 30 30 30 30
##      WR     0 30 30 30 30 30 30 30 30 30
##      WV     0 30 30 30 30 30 30 30 30 30
##      YO11   0  0  0  0 30 30 30 30 30  0
##      YO4    0  0  0 30 30 30 30 30 30 30
##      YO7    0  0  0  0 30 30 30 30 30 30
##      YO8    0  0  0  0 30 30 30 30 30 30
```

``` r
snow_grwseason_recent %>% ggplot(aes(x=month)) + geom_histogram() +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5,6,7,8,9,10, 11, 12)) +
  facet_wrap(~parent.pop)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

### Historical climate

Prep

``` r
snow_pops_historical <- pop_elev_climate_historical_avgs %>% filter(PckSum >= 70)
unique(snow_pops_historical$parent.pop) #18 pops get some significant snowpack per year 
```

```
##  [1] "CP2"   "CP3"   "DPR"   "FR"    "LV1"   "LV3"   "LVTR1" "SQ1"   "SQ2"  
## [10] "SQ3"   "WL1"   "WL2"   "WR"    "WV"    "YO11"  "YO4"   "YO7"   "YO8"
```

``` r
summary(snow_pops_historical)
```

```
##   parent.pop        elevation.group        elev_m           month      
##  Length:216         Length:216         Min.   : 748.9   Min.   : 1.00  
##  Class :character   Class :character   1st Qu.:1613.8   1st Qu.: 3.75  
##  Mode  :character   Mode  :character   Median :2200.9   Median : 6.50  
##                                        Mean   :1992.5   Mean   : 6.50  
##                                        3rd Qu.:2470.0   3rd Qu.: 9.25  
##                                        Max.   :2872.3   Max.   :12.00  
##     cwd_mean         pck_mean          ppt_mean          tmn_mean       
##  Min.   :  8.72   Min.   :   0.00   Min.   :  3.242   Min.   :-11.4717  
##  1st Qu.: 26.58   1st Qu.:   0.00   1st Qu.: 28.115   1st Qu.: -4.4226  
##  Median : 43.83   Median :  61.25   Median : 88.206   Median :  0.1093  
##  Mean   : 51.63   Mean   : 206.42   Mean   :106.757   Mean   :  0.4516  
##  3rd Qu.: 75.03   3rd Qu.: 318.59   3rd Qu.:179.341   3rd Qu.:  4.9750  
##  Max.   :137.19   Max.   :1205.56   Max.   :296.036   Max.   : 14.3020  
##     tmx_mean         cwd_sem          pck_sem          ppt_sem      
##  Min.   : 0.901   Min.   :0.1909   Min.   :  0.00   Min.   : 1.187  
##  1st Qu.: 7.288   1st Qu.:0.5802   1st Qu.:  0.00   1st Qu.: 5.393  
##  Median :12.625   Median :1.3775   Median : 17.55   Median :13.151  
##  Mean   :13.866   Mean   :1.6666   Mean   : 27.94   Mean   :15.462  
##  3rd Qu.:20.828   3rd Qu.:2.4389   3rd Qu.: 46.86   3rd Qu.:24.499  
##  Max.   :32.296   Max.   :8.0275   Max.   :115.84   Max.   :44.172  
##     tmn_sem          tmx_sem           PckSum      
##  Min.   :0.1753   Min.   :0.1980   Min.   : 230.6  
##  1st Qu.:0.2507   1st Qu.:0.3223   1st Qu.: 940.7  
##  Median :0.2925   Median :0.3850   Median :2088.4  
##  Mean   :0.2950   Mean   :0.3849   Mean   :2477.1  
##  3rd Qu.:0.3251   3rd Qu.:0.4280   3rd Qu.:3380.9  
##  Max.   :0.4306   Max.   :0.5821   Max.   :6349.6
```

``` r
snow_pops_historical_tojoin <- snow_pops_historical %>% select(parent.pop:elev_m, PckSum) %>% distinct()

snow_pops_historical_years <- left_join(snow_pops_historical_tojoin, pop_elev_climate_historical) 
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m)`
```

``` r
summary(snow_pops_historical_years)
```

```
##   parent.pop        elevation.group        elev_m           PckSum      
##  Length:6480        Length:6480        Min.   : 748.9   Min.   : 230.6  
##  Class :character   Class :character   1st Qu.:1613.8   1st Qu.: 940.7  
##  Mode  :character   Mode  :character   Median :2200.9   Median :2088.4  
##                                        Mean   :1992.5   Mean   :2477.1  
##                                        3rd Qu.:2470.0   3rd Qu.:3380.9  
##                                        Max.   :2872.3   Max.   :6349.6  
##       Lat             Long             year          month      
##  Min.   :36.56   Min.   :-123.0   Min.   :1964   Min.   : 1.00  
##  1st Qu.:37.81   1st Qu.:-121.2   1st Qu.:1971   1st Qu.: 3.75  
##  Median :38.75   Median :-120.2   Median :1978   Median : 6.50  
##  Mean   :38.72   Mean   :-120.3   Mean   :1978   Mean   : 6.50  
##  3rd Qu.:40.01   3rd Qu.:-119.5   3rd Qu.:1986   3rd Qu.: 9.25  
##  Max.   :40.74   Max.   :-118.8   Max.   :1993   Max.   :12.00  
##       cwd              pck              ppt              tmn          
##  Min.   :  0.00   Min.   :   0.0   Min.   :  0.00   Min.   :-14.9700  
##  1st Qu.: 24.23   1st Qu.:   0.0   1st Qu.: 13.88   1st Qu.: -4.4400  
##  Median : 45.31   Median :   0.0   Median : 57.66   Median :  0.1750  
##  Mean   : 51.63   Mean   : 206.4   Mean   :106.76   Mean   :  0.4516  
##  3rd Qu.: 74.83   3rd Qu.: 274.5   3rd Qu.:147.70   3rd Qu.:  5.2900  
##  Max.   :164.00   Max.   :2594.7   Max.   :951.79   Max.   : 16.8400  
##       tmx       
##  Min.   :-2.65  
##  1st Qu.: 7.10  
##  Median :13.04  
##  Mean   :13.87  
##  3rd Qu.:20.37  
##  Max.   :35.20
```

``` r
snow_pops_historical_years %>% filter(pck < 2, pck >0) %>% arrange(parent.pop, pck) #What about when snowpack is 1 mm? This mostly occurs Nov-Jan, 
```

```
## # A tibble: 10 × 13
## # Groups:   parent.pop, elevation.group, elev_m [8]
##    parent.pop elevation.group elev_m PckSum   Lat  Long  year month   cwd   pck
##    <chr>      <chr>            <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 CP2        High             2244.  3178.  38.7 -120.  1976     1  22.8  0.03
##  2 FR         Mid               787    231.  40.0 -121.  1971     3  50.1  1.78
##  3 LV1        High             2593.  6123.  40.5 -122.  1978     7  76.7  1.61
##  4 LV1        High             2593.  6123.  40.5 -122.  1976    12  13.3  1.66
##  5 SQ3        High             2373.  2036.  36.7 -119.  1991     2  47.0  1.78
##  6 WL1        Mid              1614.   941.  38.8 -120.  1979    12  20.8  1.84
##  7 WL2        High             2020.  2140.  38.8 -120.  1979    11  25.7  0.89
##  8 WL2        High             2020.  2140.  38.8 -120.  1978    11  24.8  1.36
##  9 YO11       High             2872.  2434.  37.9 -119.  1980    11  26.0  0.05
## 10 YO8        High             2591.  3392.  37.8 -119.  1975    11  33.6  1.56
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#one case in Feb (SQ3), one case in March (FR) and one case in July (LV1)
```

First month 

``` r
snow_historical_first_month <- snow_pops_historical_years %>%
  group_by(parent.pop, elevation.group, elev_m, year) %>% 
  filter(pck==0) %>% 
  filter(tmn > 0) %>% 
  arrange(month) %>% 
  filter(row_number()==1) #get first month for each pop and year with no snowpack for germ

snow_historical_first_month_tomerge <- snow_historical_first_month %>% 
  select(parent.pop:elev_m, year, firstmonth=month)

snow_historical_first_month_col <- full_join(snow_pops_historical_years, snow_historical_first_month_tomerge)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, year)`
```

``` r
dim(snow_historical_first_month_col)
```

```
## [1] 6480   14
```

Last month

``` r
snow_historical_last_month <- snow_historical_first_month_col %>%
  group_by(parent.pop, elevation.group, elev_m, year) %>% 
  filter(month>firstmonth) %>% 
  filter(if_else(pck>70, pck>70, 
                 if_else(pck>0, tmn < 0, tmn <= -5))) %>%  #-5 is moderate freeze (Pardee et al. 2017) 
  arrange(month) %>% 
  filter(row_number()==1) #get first month after growstart for each pop and year with pck >70 OR PCK > 0 & TMN <0 OR TMN <-5

#snow_historical_last_month %>% filter(tmn <= -5, pck <=0)

snow_historical_last_month_tomerge <- snow_historical_last_month %>% 
  select(parent.pop:elev_m, year, firstmonth,lastmonth=month)

snow_historical_last_month_col <- full_join(snow_historical_first_month_col, snow_historical_last_month_tomerge) %>% 
  select(firstmonth, lastmonth, year:tmx)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, year,
## firstmonth)`
## Adding missing grouping variables: `parent.pop`, `elevation.group`, `elev_m`
```

``` r
dim(snow_historical_last_month_col)
```

```
## [1] 6480   12
```

check weird cases

``` r
snow_historical_last_month_col %>% filter(is.na(firstmonth)) #no cases where there isn't a firstmonth
```

```
## # A tibble: 0 × 12
## # Groups:   parent.pop, elevation.group, elev_m [0]
## # ℹ 12 variables: parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   firstmonth <dbl>, lastmonth <dbl>, year <dbl>, month <dbl>, cwd <dbl>,
## #   pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
snow_historical_last_month_col %>% filter(is.na(lastmonth)) %>% arrange(elev_m) #99 cases where there isn't a lastmonth 
```

```
## # A tibble: 1,188 × 12
## # Groups:   parent.pop, elevation.group, elev_m [12]
##    parent.pop elevation.group elev_m firstmonth lastmonth  year month    cwd
##    <chr>      <chr>            <dbl>      <dbl>     <dbl> <dbl> <dbl>  <dbl>
##  1 WV         Mid               749.          4        NA  1973     4   0   
##  2 WV         Mid               749.          4        NA  1973     8 137.  
##  3 WV         Mid               749.          4        NA  1973    12   0   
##  4 WV         Mid               749.          4        NA  1973     2   0   
##  5 WV         Mid               749.          4        NA  1973     1   3.07
##  6 WV         Mid               749.          4        NA  1973     7 106.  
##  7 WV         Mid               749.          4        NA  1973     6  96.3 
##  8 WV         Mid               749.          4        NA  1973     3   0   
##  9 WV         Mid               749.          4        NA  1973     5  79.4 
## 10 WV         Mid               749.          4        NA  1973    11  11.2 
## # ℹ 1,178 more rows
## # ℹ 4 more variables: pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#****range of elevations 

snow_historical_last_month_col %>% filter(lastmonth==firstmonth) #no cases where last month is first month 
```

```
## # A tibble: 0 × 12
## # Groups:   parent.pop, elevation.group, elev_m [0]
## # ℹ 12 variables: parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   firstmonth <dbl>, lastmonth <dbl>, year <dbl>, month <dbl>, cwd <dbl>,
## #   pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
snow_historical_last_month_col %>% filter(lastmonth==firstmonth+1) #4 cases where there was sig snowpack in the month after the first month of 0 snowpack
```

```
## # A tibble: 48 × 12
## # Groups:   parent.pop, elevation.group, elev_m [3]
##    parent.pop elevation.group elev_m firstmonth lastmonth  year month   cwd
##    <chr>      <chr>            <dbl>      <dbl>     <dbl> <dbl> <dbl> <dbl>
##  1 FR         Mid                787          2         3  1991     4  86.0
##  2 FR         Mid                787          2         3  1991     8 105. 
##  3 FR         Mid                787          2         3  1991    12  15.6
##  4 FR         Mid                787          2         3  1991     2  32.9
##  5 FR         Mid                787          2         3  1991     1  17.0
##  6 FR         Mid                787          2         3  1991     7 143. 
##  7 FR         Mid                787          2         3  1991     6 111. 
##  8 FR         Mid                787          2         3  1991     3  44.5
##  9 FR         Mid                787          2         3  1991     5 107. 
## 10 FR         Mid                787          2         3  1991    11  28.7
## # ℹ 38 more rows
## # ℹ 4 more variables: pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
snow_historical_last_month_col %>% filter(lastmonth==firstmonth+2) #7 cases where there was sig snowpack in the second month after the first month of 0 snowpack
```

```
## # A tibble: 84 × 12
## # Groups:   parent.pop, elevation.group, elev_m [4]
##    parent.pop elevation.group elev_m firstmonth lastmonth  year month   cwd
##    <chr>      <chr>            <dbl>      <dbl>     <dbl> <dbl> <dbl> <dbl>
##  1 DPR        Mid              1019.          1         3  1991     4 40.9 
##  2 DPR        Mid              1019.          1         3  1991     8  0   
##  3 DPR        Mid              1019.          1         3  1991    12 13.5 
##  4 DPR        Mid              1019.          1         3  1991     2 21.9 
##  5 DPR        Mid              1019.          1         3  1991     1 11.6 
##  6 DPR        Mid              1019.          1         3  1991     7 38.6 
##  7 DPR        Mid              1019.          1         3  1991     6  0   
##  8 DPR        Mid              1019.          1         3  1991     3 10.6 
##  9 DPR        Mid              1019.          1         3  1991     5  5.91
## 10 DPR        Mid              1019.          1         3  1991    11 18.5 
## # ℹ 74 more rows
## # ℹ 4 more variables: pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
heavysnow_historical <- snow_historical_last_month_col %>% filter(lastmonth==firstmonth+1) %>% filter(month==firstmonth)
summary(heavysnow_historical)
```

```
##   parent.pop        elevation.group        elev_m       firstmonth  
##  Length:4           Length:4           Min.   : 787   Min.   :1.00  
##  Class :character   Class :character   1st Qu.:1065   1st Qu.:1.75  
##  Mode  :character   Mode  :character   Median :1158   Median :2.00  
##                                        Mean   :1179   Mean   :1.75  
##                                        3rd Qu.:1272   3rd Qu.:2.00  
##                                        Max.   :1614   Max.   :2.00  
##    lastmonth         year          month           cwd             pck   
##  Min.   :2.00   Min.   :1986   Min.   :1.00   Min.   :17.51   Min.   :0  
##  1st Qu.:2.75   1st Qu.:1990   1st Qu.:1.75   1st Qu.:23.74   1st Qu.:0  
##  Median :3.00   Median :1991   Median :2.00   Median :29.36   Median :0  
##  Mean   :2.75   Mean   :1990   Mean   :1.75   Mean   :27.68   Mean   :0  
##  3rd Qu.:3.00   3rd Qu.:1991   3rd Qu.:2.00   3rd Qu.:33.31   3rd Qu.:0  
##  Max.   :3.00   Max.   :1991   Max.   :2.00   Max.   :34.50   Max.   :0  
##       ppt              tmn             tmx       
##  Min.   : 51.74   Min.   :1.640   Min.   :13.03  
##  1st Qu.: 63.63   1st Qu.:1.940   1st Qu.:13.57  
##  Median : 78.72   Median :2.475   Median :14.99  
##  Mean   :118.55   Mean   :2.475   Mean   :15.02  
##  3rd Qu.:133.63   3rd Qu.:3.010   3rd Qu.:16.45  
##  Max.   :265.03   Max.   :3.310   Max.   :17.07
```

``` r
unique(heavysnow_historical$parent.pop)
```

```
## [1] "FR"  "WL1" "WR"
```

``` r
heavysnow_historical %>% filter(elev_m>1800)
```

```
## # A tibble: 0 × 12
## # Groups:   parent.pop, elevation.group, elev_m [0]
## # ℹ 12 variables: parent.pop <chr>, elevation.group <chr>, elev_m <dbl>,
## #   firstmonth <dbl>, lastmonth <dbl>, year <dbl>, month <dbl>, cwd <dbl>,
## #   pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#"FR" "WL1" "WR"  
#****CHECK TEMPS FOR THESE CASES 
#*Temps range from 1.640 to 3.3 (which seems like an acceptable temp given we vernalize at 4C)
#*OR START THE CHECK IN FEB FOR POPS > 6000 FT (ALL BELOW 6000 FT)
#*OR just take these cases out

#snow_historical_last_month_col %>% filter(year==1991)
#snow_historical_last_month_col %>% filter(parent.pop=="DPR") %>% filter(year==1962|year==1975)
#snow_historical_last_month_col %>% filter(parent.pop=="WR") %>% filter(year==1975|year==1986)
#snow_historical_last_month_col %>% filter(parent.pop=="YO7") %>% filter(year==1976)
#snow_historical_last_month_col %>% filter(parent.pop=="FR") %>% filter(year==1975)
#snow_historical_last_month_col %>% filter(parent.pop=="SQ3") %>% filter(year==1976)
```

Average first and last month 

``` r
#for cases with no last month - make last month 12
#remove cases where last month is first month +1 or 2 

snow_historicalfirst_last_month_for_calc <- snow_historical_last_month_col %>% 
  mutate(lastmonth=if_else(is.na(lastmonth), 12, lastmonth)) %>% 
  filter(lastmonth > firstmonth+2) %>% 
  select(firstmonth:lastmonth, parent.pop, elevation.group, elev_m, year) %>% 
  distinct()

snow_historical_avg_first_last_month <- snow_historicalfirst_last_month_for_calc %>% 
  group_by(parent.pop, elevation.group, elev_m) %>% 
  summarise(AvgFirstMonth=mean(firstmonth), First_sem=sem(firstmonth),
            AvgLastMonth=mean(lastmonth), Last_sem=sem(lastmonth)) %>% 
  arrange(elevation.group, elev_m)
```

```
## `summarise()` has grouped output by 'parent.pop', 'elevation.group'. You can
## override using the `.groups` argument.
```

``` r
snow_historical_avg_first_last_month
```

```
## # A tibble: 18 × 7
## # Groups:   parent.pop, elevation.group [18]
##    parent.pop elevation.group elev_m AvgFirstMonth First_sem AvgLastMonth
##    <chr>      <chr>            <dbl>         <dbl>     <dbl>        <dbl>
##  1 WL2        High             2020.          5.43    0.133          11.5
##  2 YO4        High             2158.          5.37    0.131          11.5
##  3 CP2        High             2244.          5.8     0.139          11.4
##  4 CP3        High             2266.          6.03    0.112          11.4
##  5 LV3        High             2354.          6.71    0.124          10.9
##  6 SQ3        High             2373.          5.67    0.130          11.7
##  7 YO7        High             2470.          6.17    0.0969         11.4
##  8 YO8        High             2591.          6.27    0.0951         11.2
##  9 LV1        High             2593.          6.71    0.124          10.9
## 10 LVTR1      High             2741.          6.71    0.124          10.9
## 11 YO11       High             2872.          6.47    0.0926         10.9
## 12 WV         Mid               749.          3.63    0.206          12  
## 13 FR         Mid               787           3.03    0.182          12  
## 14 DPR        Mid              1019.          2.86    0.215          12  
## 15 WR         Mid              1158           3.71    0.191          11.9
## 16 WL1        Mid              1614.          4.52    0.118          11.8
## 17 SQ1        Mid              1921.          4.8     0.111          11.8
## 18 SQ2        Mid              1934.          4.87    0.104          11.8
## # ℹ 1 more variable: Last_sem <dbl>
```

``` r
#Start = May-July for "high" elev, March-May for "mid" elevation 
#End = Nov-Dec
#interestingly, the starts are later than recent climate and ends are earlier in some cases 

#Used Standard Rounding
snow_avg_first_last_month_historical <- snow_historical_avg_first_last_month %>% 
  mutate(AvgFirstMonth=true_round(AvgFirstMonth, 0), AvgLastMonth=true_round(AvgLastMonth, 0)) %>% 
  select(parent.pop:elev_m, AvgFirstMonth, AvgLastMonth)
snow_historical_avg_first_last_month
```

```
## # A tibble: 18 × 7
## # Groups:   parent.pop, elevation.group [18]
##    parent.pop elevation.group elev_m AvgFirstMonth First_sem AvgLastMonth
##    <chr>      <chr>            <dbl>         <dbl>     <dbl>        <dbl>
##  1 WL2        High             2020.          5.43    0.133          11.5
##  2 YO4        High             2158.          5.37    0.131          11.5
##  3 CP2        High             2244.          5.8     0.139          11.4
##  4 CP3        High             2266.          6.03    0.112          11.4
##  5 LV3        High             2354.          6.71    0.124          10.9
##  6 SQ3        High             2373.          5.67    0.130          11.7
##  7 YO7        High             2470.          6.17    0.0969         11.4
##  8 YO8        High             2591.          6.27    0.0951         11.2
##  9 LV1        High             2593.          6.71    0.124          10.9
## 10 LVTR1      High             2741.          6.71    0.124          10.9
## 11 YO11       High             2872.          6.47    0.0926         10.9
## 12 WV         Mid               749.          3.63    0.206          12  
## 13 FR         Mid               787           3.03    0.182          12  
## 14 DPR        Mid              1019.          2.86    0.215          12  
## 15 WR         Mid              1158           3.71    0.191          11.9
## 16 WL1        Mid              1614.          4.52    0.118          11.8
## 17 SQ1        Mid              1921.          4.8     0.111          11.8
## 18 SQ2        Mid              1934.          4.87    0.104          11.8
## # ℹ 1 more variable: Last_sem <dbl>
```

``` r
snow_avg_first_last_month_historical
```

```
## # A tibble: 18 × 5
## # Groups:   parent.pop, elevation.group [18]
##    parent.pop elevation.group elev_m AvgFirstMonth AvgLastMonth
##    <chr>      <chr>            <dbl>         <dbl>        <dbl>
##  1 WL2        High             2020.             5           12
##  2 YO4        High             2158.             5           12
##  3 CP2        High             2244.             6           11
##  4 CP3        High             2266.             6           11
##  5 LV3        High             2354.             7           11
##  6 SQ3        High             2373.             6           12
##  7 YO7        High             2470.             6           11
##  8 YO8        High             2591.             6           11
##  9 LV1        High             2593.             7           11
## 10 LVTR1      High             2741.             7           11
## 11 YO11       High             2872.             6           11
## 12 WV         Mid               749.             4           12
## 13 FR         Mid               787              3           12
## 14 DPR        Mid              1019.             3           12
## 15 WR         Mid              1158              4           12
## 16 WL1        Mid              1614.             5           12
## 17 SQ1        Mid              1921.             5           12
## 18 SQ2        Mid              1934.             5           12
```

``` r
snow_historical_avg_first_last_month_allmonths <- left_join(snow_pops_historical_years, snow_avg_first_last_month_historical)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m)`
```

Fill in months b/t start and stop

``` r
snow_grwseason_historical <- snow_historical_avg_first_last_month_allmonths %>%
  group_by(parent.pop, elevation.group, elev_m, year) %>% 
  filter(month>AvgFirstMonth) %>% 
  filter(month<=AvgLastMonth)
summary(snow_grwseason_historical) 
```

```
##   parent.pop        elevation.group        elev_m           PckSum      
##  Length:3360        Length:3360        Min.   : 748.9   Min.   : 230.6  
##  Class :character   Class :character   1st Qu.:1158.0   1st Qu.: 595.9  
##  Mode  :character   Mode  :character   Median :2020.1   Median :1814.1  
##                                        Mean   :1841.5   Mean   :2012.0  
##                                        3rd Qu.:2373.2   3rd Qu.:3177.6  
##                                        Max.   :2872.3   Max.   :6349.6  
##       Lat             Long             year          month       
##  Min.   :36.56   Min.   :-123.0   Min.   :1964   Min.   : 4.000  
##  1st Qu.:37.81   1st Qu.:-121.2   1st Qu.:1971   1st Qu.: 7.000  
##  Median :38.79   Median :-120.2   Median :1978   Median : 9.000  
##  Mean   :38.70   Mean   :-120.3   Mean   :1978   Mean   : 8.848  
##  3rd Qu.:40.01   3rd Qu.:-119.5   3rd Qu.:1986   3rd Qu.:10.250  
##  Max.   :40.74   Max.   :-118.8   Max.   :1993   Max.   :12.000  
##       cwd              pck              ppt              tmn          
##  Min.   :  0.00   Min.   :  0.00   Min.   :  0.00   Min.   :-10.0900  
##  1st Qu.: 33.70   1st Qu.:  0.00   1st Qu.:  5.33   1st Qu.: -0.1125  
##  Median : 64.06   Median :  0.00   Median : 26.88   Median :  4.7400  
##  Mean   : 63.86   Mean   : 21.57   Mean   : 72.39   Mean   :  4.1152  
##  3rd Qu.: 88.98   3rd Qu.:  0.00   3rd Qu.: 92.02   3rd Qu.:  8.4500  
##  Max.   :164.00   Max.   :882.99   Max.   :951.79   Max.   : 16.8400  
##       tmx        AvgFirstMonth    AvgLastMonth  
##  Min.   : 0.49   Min.   :3.000   Min.   :11.00  
##  1st Qu.:12.15   1st Qu.:4.000   1st Qu.:11.00  
##  Median :19.59   Median :5.000   Median :12.00  
##  Mean   :18.31   Mean   :5.027   Mean   :11.67  
##  3rd Qu.:23.59   3rd Qu.:6.000   3rd Qu.:12.00  
##  Max.   :35.20   Max.   :7.000   Max.   :12.00
```

``` r
xtabs(~parent.pop+month, data=snow_grwseason_historical)
```

```
##           month
## parent.pop  4  5  6  7  8  9 10 11 12
##      CP2    0  0  0 30 30 30 30 30  0
##      CP3    0  0  0 30 30 30 30 30  0
##      DPR   30 30 30 30 30 30 30 30 30
##      FR    30 30 30 30 30 30 30 30 30
##      LV1    0  0  0  0 30 30 30 30  0
##      LV3    0  0  0  0 30 30 30 30  0
##      LVTR1  0  0  0  0 30 30 30 30  0
##      SQ1    0  0 30 30 30 30 30 30 30
##      SQ2    0  0 30 30 30 30 30 30 30
##      SQ3    0  0  0 30 30 30 30 30 30
##      WL1    0  0 30 30 30 30 30 30 30
##      WL2    0  0 30 30 30 30 30 30 30
##      WR     0 30 30 30 30 30 30 30 30
##      WV     0 30 30 30 30 30 30 30 30
##      YO11   0  0  0 30 30 30 30 30  0
##      YO4    0  0 30 30 30 30 30 30 30
##      YO7    0  0  0 30 30 30 30 30  0
##      YO8    0  0  0 30 30 30 30 30  0
```

``` r
snow_grwseason_historical %>% ggplot(aes(x=month)) + geom_histogram() +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5,6,7,8,9,10, 11, 12)) +
  facet_wrap(~parent.pop)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-30-1.png)<!-- -->

## Bind snow and no_snow pops together


``` r
unique(nosnow_grwseason_recent$parent.pop)
```

```
## [1] "BH"  "CC"  "IH"  "SC"  "TM2"
```

``` r
unique(snow_grwseason_recent$parent.pop)
```

```
##  [1] "CP2"   "CP3"   "DPR"   "FR"    "LV1"   "LV3"   "LVTR1" "SQ1"   "SQ2"  
## [10] "SQ3"   "WL1"   "WL2"   "WR"    "WV"    "YO11"  "YO4"   "YO7"   "YO8"
```

``` r
allpops_recent_grwseason <- rbind(nosnow_grwseason_recent, snow_grwseason_recent)
summary(allpops_recent_grwseason)
```

```
##      month          growmonth       parent.pop        elevation.group   
##  Min.   : 1.000   Min.   : 3.000   Length:4980        Length:4980       
##  1st Qu.: 6.000   1st Qu.: 5.000   Class :character   Class :character  
##  Median : 8.000   Median : 7.000   Mode  :character   Mode  :character  
##  Mean   : 7.873   Mean   : 7.024                                        
##  3rd Qu.:10.000   3rd Qu.: 9.000                                        
##  Max.   :12.000   Max.   :12.000                                        
##                   NA's   :3750                                          
##      elev_m           PckSum             Lat             Long       
##  Min.   : 313.0   Min.   :   0.00   Min.   :36.56   Min.   :-123.0  
##  1st Qu.: 748.9   1st Qu.:  91.58   1st Qu.:37.81   1st Qu.:-121.2  
##  Median :1613.8   Median : 671.23   Median :38.83   Median :-120.7  
##  Mean   :1501.6   Mean   :1323.80   Mean   :38.77   Mean   :-120.5  
##  3rd Qu.:2266.4   3rd Qu.:2318.89   3rd Qu.:39.59   3rd Qu.:-119.8  
##  Max.   :2872.3   Max.   :5447.93   Max.   :40.74   Max.   :-118.8  
##                                                                     
##       year           cwd              pck              ppt         
##  Min.   :1994   Min.   :  0.00   Min.   :  0.00   Min.   :  0.000  
##  1st Qu.:2001   1st Qu.: 28.43   1st Qu.:  0.00   1st Qu.:  3.743  
##  Median :2008   Median : 57.42   Median :  0.00   Median : 27.720  
##  Mean   :2008   Mean   : 60.58   Mean   : 14.61   Mean   : 75.602  
##  3rd Qu.:2016   3rd Qu.: 89.04   3rd Qu.:  0.00   3rd Qu.:106.073  
##  Max.   :2023   Max.   :182.70   Max.   :637.09   Max.   :894.020  
##                                                                    
##       tmn               tmx        AvgFirstMonth    AvgLastMonth  
##  Min.   :-11.090   Min.   :-0.10   Min.   :2.000   Min.   :10.00  
##  1st Qu.:  2.350   1st Qu.:13.32   1st Qu.:3.000   1st Qu.:11.00  
##  Median :  5.940   Median :19.52   Median :4.000   Median :12.00  
##  Mean   :  5.883   Mean   :19.07   Mean   :4.006   Mean   :11.56  
##  3rd Qu.:  9.963   3rd Qu.:24.36   3rd Qu.:6.000   3rd Qu.:12.00  
##  Max.   : 20.150   Max.   :37.09   Max.   :6.000   Max.   :12.00  
## 
```

``` r
unique(allpops_recent_grwseason$parent.pop)
```

```
##  [1] "BH"    "CC"    "IH"    "SC"    "TM2"   "CP2"   "CP3"   "DPR"   "FR"   
## [10] "LV1"   "LV3"   "LVTR1" "SQ1"   "SQ2"   "SQ3"   "WL1"   "WL2"   "WR"   
## [19] "WV"    "YO11"  "YO4"   "YO7"   "YO8"
```

``` r
write_csv(allpops_recent_grwseason, "../output/Climate/flint_climate_growthseason_recent.csv")

unique(nosnow_grwseason_historical$parent.pop)
```

```
## [1] "BH"  "CC"  "IH"  "SC"  "TM2"
```

``` r
unique(snow_grwseason_historical$parent.pop)
```

```
##  [1] "CP2"   "CP3"   "DPR"   "FR"    "LV1"   "LV3"   "LVTR1" "SQ1"   "SQ2"  
## [10] "SQ3"   "WL1"   "WL2"   "WR"    "WV"    "YO11"  "YO4"   "YO7"   "YO8"
```

``` r
allpops_historical_grwseason <- rbind(nosnow_grwseason_historical, snow_grwseason_historical)
summary(allpops_historical_grwseason)
```

```
##      month          growmonth       parent.pop        elevation.group   
##  Min.   : 1.000   Min.   : 3.000   Length:4620        Length:4620       
##  1st Qu.: 6.000   1st Qu.: 5.000   Class :character   Class :character  
##  Median : 8.000   Median : 7.000   Mode  :character   Mode  :character  
##  Mean   : 7.961   Mean   : 6.738                                        
##  3rd Qu.:10.000   3rd Qu.: 9.000                                        
##  Max.   :12.000   Max.   :12.000                                        
##                   NA's   :3360                                          
##      elev_m           PckSum              Lat             Long       
##  Min.   : 313.0   Min.   :   0.234   Min.   :36.56   Min.   :-123.0  
##  1st Qu.: 511.4   1st Qu.:  17.660   1st Qu.:37.81   1st Qu.:-121.2  
##  Median :1613.8   Median : 940.731   Median :38.83   Median :-120.7  
##  Mean   :1453.2   Mean   :1464.761   Mean   :38.74   Mean   :-120.5  
##  3rd Qu.:2244.1   3rd Qu.:2140.448   3rd Qu.:39.59   3rd Qu.:-119.8  
##  Max.   :2872.3   Max.   :6349.556   Max.   :40.74   Max.   :-118.8  
##                                                                      
##       year           cwd              pck              ppt         
##  Min.   :1964   Min.   :  0.00   Min.   :  0.00   Min.   :  0.000  
##  1st Qu.:1971   1st Qu.: 28.26   1st Qu.:  0.00   1st Qu.:  7.065  
##  Median :1978   Median : 55.95   Median :  0.00   Median : 35.045  
##  Mean   :1978   Mean   : 57.95   Mean   : 15.85   Mean   : 79.266  
##  3rd Qu.:1986   3rd Qu.: 83.71   3rd Qu.:  0.00   3rd Qu.:107.795  
##  Max.   :1993   Max.   :183.92   Max.   :882.99   Max.   :951.790  
##                                                                    
##       tmn               tmx        AvgFirstMonth    AvgLastMonth  
##  Min.   :-10.090   Min.   : 0.49   Min.   :2.000   Min.   :10.00  
##  1st Qu.:  1.110   1st Qu.:13.07   1st Qu.:2.000   1st Qu.:11.00  
##  Median :  4.870   Median :18.92   Median :5.000   Median :12.00  
##  Mean   :  4.636   Mean   :18.56   Mean   :4.201   Mean   :11.34  
##  3rd Qu.:  8.530   3rd Qu.:23.77   3rd Qu.:6.000   3rd Qu.:12.00  
##  Max.   : 17.940   Max.   :36.23   Max.   :7.000   Max.   :12.00  
## 
```

``` r
unique(allpops_historical_grwseason$parent.pop)
```

```
##  [1] "BH"    "CC"    "IH"    "SC"    "TM2"   "CP2"   "CP3"   "DPR"   "FR"   
## [10] "LV1"   "LV3"   "LVTR1" "SQ1"   "SQ2"   "SQ3"   "WL1"   "WL2"   "WR"   
## [19] "WV"    "YO11"  "YO4"   "YO7"   "YO8"
```

``` r
write_csv(allpops_historical_grwseason, "../output/Climate/flint_climate_growthseason_historical.csv")
```


## Climate traits across pops

### Totals


``` r
names(allpops_recent_grwseason)
```

```
##  [1] "month"           "growmonth"       "parent.pop"      "elevation.group"
##  [5] "elev_m"          "PckSum"          "Lat"             "Long"           
##  [9] "year"            "cwd"             "pck"             "ppt"            
## [13] "tmn"             "tmx"             "AvgFirstMonth"   "AvgLastMonth"
```

``` r
allpops_recent_grwseason_yearlytot <- allpops_recent_grwseason %>%  group_by(parent.pop, year, elev_m) %>% summarise_at(c("pck", "ppt"), sum, na.rm = TRUE)

recent_ppt_total <- allpops_recent_grwseason_yearlytot %>% ggplot(aes(x=year, y=ppt, group=parent.pop, color=elev_m)) + 
  geom_point() + geom_line() + 
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  ggtitle("Recent Climate")  + 
  theme_classic() + 
  ylim(0, 2000) +
  theme(text=element_text(size=30), axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("../output/Climate/growthseasonTot_Precip_RecentClim.png", width = 12, height = 6, units = "in")

allpops_historical_grwseason_yearlytot <- allpops_historical_grwseason %>% group_by(parent.pop, year, elev_m) %>% summarise_at(c("pck", "ppt"), sum, na.rm = TRUE)

hist_ppt_total <- allpops_historical_grwseason_yearlytot %>% ggplot(aes(x=year, y=ppt, group=parent.pop, color=elev_m)) + 
  geom_point() + geom_line() + 
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  ggtitle("Historical Climate") + 
  theme_classic() + 
  theme(text=element_text(size=30), axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("../output/Climate/growthseasonTot_Precip_HistoricalClim.png", width = 12, height = 6, units = "in")

#should combine these into one figure and save that instead
legend <- get_legend(hist_ppt_total)
hist_ppt_total <- hist_ppt_total + theme(legend.position="none")
recent_ppt_total <- recent_ppt_total + theme(legend.position="none")
grid.arrange(hist_ppt_total, recent_ppt_total, legend, ncol=3, widths=c(3.12, 3.12, 1.09))
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

``` r
 #2000 x 850
```

### Monthly Averages

#### Recent Years - Growth Season

``` r
allpops_recent_grwseason_mosavgs <- allpops_recent_grwseason %>% group_by(parent.pop, elevation.group, elev_m, month) %>% summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean, sem), na.rm = TRUE) 
names(allpops_recent_grwseason_mosavgs) <- gsub("fn2", "sem", colnames(allpops_recent_grwseason_mosavgs))
names(allpops_recent_grwseason_mosavgs) <-gsub("fn1", "mean", colnames(allpops_recent_grwseason_mosavgs))
allpops_recent_grwseason_mosavgs #30 year averages during growth season months 
```

```
## # A tibble: 166 × 14
## # Groups:   parent.pop, elevation.group, elev_m [23]
##    parent.pop elevation.group elev_m month cwd_mean pck_mean ppt_mean tmn_mean
##    <chr>      <chr>            <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
##  1 BH         Low               511.     1     29.4        0   124.       2.81
##  2 BH         Low               511.     2     41.0        0    93.9      3.32
##  3 BH         Low               511.     3     53.9        0    90.1      4.82
##  4 BH         Low               511.     4     59.0        0    48.2      6.41
##  5 BH         Low               511.     5     51.4        0    23.2      9.78
##  6 BH         Low               511.     6     89.3        0     6.34    13.6 
##  7 BH         Low               511.    12     30.0        0   111.       2.59
##  8 CC         Low               313      1     19.6        0   184.       4.27
##  9 CC         Low               313      2     31.4        0   165.       4.81
## 10 CC         Low               313      3     45.3        0   148.       6.18
## # ℹ 156 more rows
## # ℹ 6 more variables: tmx_mean <dbl>, cwd_sem <dbl>, pck_sem <dbl>,
## #   ppt_sem <dbl>, tmn_sem <dbl>, tmx_sem <dbl>
```

``` r
allpops_recent_grwseason_mosavgs$elevation.group <- factor(allpops_recent_grwseason_mosavgs$elevation.group, levels=elev_order)       

allpops_recent_grwseason_mosavgs %>% ggplot(aes(x=month, y=cwd_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=cwd_mean-cwd_sem,ymax=cwd_mean+cwd_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0")+ 
  labs(fill="Elevation",x="Population", y="Avg CWD" ,title = "Average CWD during Growth Season - Recent Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1)) + 
  facet_wrap(~parent.pop)
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_Monthly_CWD_RecentClim.png", width = 16, height = 10, units = "in")

allpops_recent_grwseason_mosavgs %>% ggplot(aes(x=month, y=ppt_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=ppt_mean-ppt_sem,ymax=ppt_mean+ppt_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") + 
  labs(fill="Elevation",x="Population", y="Avg Precip" ,title = "Average Precip during Growth Season - Recent Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))+ 
  facet_wrap(~parent.pop)
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-33-2.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_Monthly_Precip_RecentClim.png", width = 12, height = 6, units = "in")

allpops_recent_grwseason_mosavgs %>% ggplot(aes(x=month, y=tmn_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=tmn_mean-tmn_sem,ymax=tmn_mean+tmn_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") + 
  labs(fill="Elevation",x="Population", y="Avg MinTemp" ,title = "Average MinTemp during Growth Season - Recent Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))+ 
  facet_wrap(~parent.pop)
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-33-3.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_Monthly_MinTemp_RecentClim.png", width = 12, height = 6, units = "in")

allpops_recent_grwseason_mosavgs %>% ggplot(aes(x=month, y=tmx_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=tmx_mean-tmx_sem,ymax=tmx_mean+tmx_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") + 
  labs(fill="Elevation",x="Population", y="Avg MaxTemp" ,title = "Average MaxTemp during Growth Season - Recent Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))+ 
  facet_wrap(~parent.pop)
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-33-4.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_Monthly_MaxTemp_RecentClim.png", width = 12, height = 6, units = "in")
```

#### Historical Years - Growth Season

``` r
allpops_historical_grwseason_mosavgs <- allpops_historical_grwseason %>% group_by(parent.pop, elevation.group, elev_m, month) %>% summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean, sem), na.rm = TRUE) 
names(allpops_historical_grwseason_mosavgs) <- gsub("fn2", "sem", colnames(allpops_historical_grwseason_mosavgs))
names(allpops_historical_grwseason_mosavgs) <-gsub("fn1", "mean", colnames(allpops_historical_grwseason_mosavgs))
allpops_historical_grwseason_mosavgs #30 year averages during growth season months 
```

```
## # A tibble: 154 × 14
## # Groups:   parent.pop, elevation.group, elev_m [23]
##    parent.pop elevation.group elev_m month cwd_mean pck_mean ppt_mean tmn_mean
##    <chr>      <chr>            <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
##  1 BH         Low               511.     1     28.0    0.234   104.       1.46
##  2 BH         Low               511.     2     40.4    0        92.2      2.76
##  3 BH         Low               511.     3     51.2    0       101.       4.04
##  4 BH         Low               511.     4     62.2    0        46.4      5.57
##  5 BH         Low               511.     5     63.5    0        12.6      8.83
##  6 BH         Low               511.     6     87.5    0         5.73    12.4 
##  7 BH         Low               511.    11     43.8    0        82.0      4.37
##  8 BH         Low               511.    12     28.5    0        89.3      1.44
##  9 CC         Low               313      1     18.7    0.951   191.       2.42
## 10 CC         Low               313      2     31.1    0       146.       4.04
## # ℹ 144 more rows
## # ℹ 6 more variables: tmx_mean <dbl>, cwd_sem <dbl>, pck_sem <dbl>,
## #   ppt_sem <dbl>, tmn_sem <dbl>, tmx_sem <dbl>
```

``` r
allpops_historical_grwseason_mosavgs$elevation.group <- factor(allpops_historical_grwseason_mosavgs$elevation.group, levels=elev_order)       

allpops_historical_grwseason_mosavgs %>% ggplot(aes(x=month, y=cwd_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=cwd_mean-cwd_sem,ymax=cwd_mean+cwd_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0")+ 
  labs(fill="Elevation",x="Population", y="Avg CWD" ,title = "Average CWD during Growth Season - Historical Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1)) + 
  facet_wrap(~parent.pop)
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_Monthly_CWD_HistoricalClim.png", width = 16, height = 10, units = "in")

allpops_historical_grwseason_mosavgs %>% ggplot(aes(x=month, y=ppt_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=ppt_mean-ppt_sem,ymax=ppt_mean+ppt_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") + 
  labs(fill="Elevation",x="Population", y="Avg Precip" ,title = "Average Precip during Growth Season - Historical Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))+ 
  facet_wrap(~parent.pop)
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-34-2.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_Monthly_Precip_HistoricalClim.png", width = 12, height = 6, units = "in")

allpops_historical_grwseason_mosavgs %>% ggplot(aes(x=month, y=tmn_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=tmn_mean-tmn_sem,ymax=tmn_mean+tmn_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") + 
  labs(fill="Elevation",x="Population", y="Avg MinTemp" ,title = "Average MinTemp during Growth Season - Historical Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))+ 
  facet_wrap(~parent.pop)
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-34-3.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_Monthly_MinTemp_HistoricalClim.png", width = 12, height = 6, units = "in")

allpops_historical_grwseason_mosavgs %>% ggplot(aes(x=month, y=tmx_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=tmx_mean-tmx_sem,ymax=tmx_mean+tmx_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") + 
  labs(fill="Elevation",x="Population", y="Avg MaxTemp" ,title = "Average MaxTemp during Growth Season - Historical Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))+ 
  facet_wrap(~parent.pop)
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-34-4.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_Monthly_MaxTemp_HistoricalClim.png", width = 12, height = 6, units = "in")
```


### 30 Year Averages

#### Recent Years - Growth Season


``` r
allpops_recent_grwseason_avgs <- allpops_recent_grwseason %>% group_by(parent.pop, elevation.group, elev_m) %>% summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean, sem), na.rm = TRUE) 
names(allpops_recent_grwseason_avgs) <- gsub("fn2", "sem", colnames(allpops_recent_grwseason_avgs))
names(allpops_recent_grwseason_avgs) <-gsub("fn1", "mean", colnames(allpops_recent_grwseason_avgs))
allpops_recent_grwseason_avgs #30 year averages during growth season months 
```

```
## # A tibble: 23 × 13
## # Groups:   parent.pop, elevation.group [23]
##    parent.pop elevation.group elev_m cwd_mean pck_mean ppt_mean tmn_mean
##    <chr>      <chr>            <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
##  1 BH         Low               511.     50.6    0         71.0     6.19
##  2 CC         Low               313      48.4    0        105.      8.56
##  3 CP2        High             2244.     75.1   41.8       78.2     3.94
##  4 CP3        High             2266.     57.8   44.3       75.4     3.36
##  5 DPR        Mid              1019.     30.4    2.72      96.9     9.06
##  6 FR         Mid               787      89.7    4.01      58.1     7.37
##  7 IH         Low               454.     40.5    0.201    100.      8.07
##  8 LV1        High             2593.     65.5   28.0       73.6     2.35
##  9 LV3        High             2354.     53.6   28.2       72.7     2.31
## 10 LVTR1      High             2741.     73.7   29.0       76.8     2.03
## # ℹ 13 more rows
## # ℹ 6 more variables: tmx_mean <dbl>, cwd_sem <dbl>, pck_sem <dbl>,
## #   ppt_sem <dbl>, tmn_sem <dbl>, tmx_sem <dbl>
```

``` r
allpops_recent_grwseason_avgs$elevation.group <- factor(allpops_recent_grwseason_avgs$elevation.group, levels=elev_order)    
#write_csv(allpops_recent_grwseason_avgs, "../output/Climate/growthseason_FlintAvgs_Recent.csv")

allpops_recent_grwseason_avgs %>% ggplot(aes(x=fct_reorder(parent.pop, cwd_mean), y=cwd_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=cwd_mean-cwd_sem,ymax=cwd_mean+cwd_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0")+ 
  labs(fill="Elevation",x="Population", y="Avg CWD" ,title = "Average CWD during Growth Season - Recent Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-35-1.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_CWD_RecentClim.png", width = 12, height = 6, units = "in")

allpops_recent_grwseason_avgs %>% ggplot(aes(x=fct_reorder(parent.pop, ppt_mean), y=ppt_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=ppt_mean-ppt_sem,ymax=ppt_mean+ppt_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") + 
  labs(fill="Elevation",x="Population", y="Avg Precip" ,title = "Average Precip during Growth Season - Recent Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-35-2.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_Precip_RecentClim.png", width = 12, height = 6, units = "in")

allpops_recent_grwseason_avgs %>% ggplot(aes(x=fct_reorder(parent.pop, tmn_mean), y=tmn_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=tmn_mean-tmn_sem,ymax=tmn_mean+tmn_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") + 
  labs(fill="Elevation",x="Population", y="Avg MinTemp" ,title = "Average MinTemp during Growth Season - Recent Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-35-3.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_MinTemp_RecentClim.png", width = 12, height = 6, units = "in")

allpops_recent_grwseason_avgs %>% ggplot(aes(x=fct_reorder(parent.pop, tmx_mean), y=tmx_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=tmx_mean-tmx_sem,ymax=tmx_mean+tmx_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") + 
  labs(fill="Elevation",x="Population", y="Avg MaxTemp" ,title = "Average MaxTemp during Growth Season - Recent Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-35-4.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_MaxTemp_RecentClim.png", width = 12, height = 6, units = "in")
```

#### Historical Years - Growth Season


``` r
allpops_historical_grwseason_avgs <- allpops_historical_grwseason %>% group_by(parent.pop, elevation.group, elev_m) %>% summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean, sem), na.rm = TRUE) 
names(allpops_historical_grwseason_avgs) <- gsub("fn2", "sem", colnames(allpops_historical_grwseason_avgs))
names(allpops_historical_grwseason_avgs) <-gsub("fn1", "mean", colnames(allpops_historical_grwseason_avgs))
allpops_historical_grwseason_avgs #30 year averages during growth season months 
```

```
## # A tibble: 23 × 13
## # Groups:   parent.pop, elevation.group [23]
##    parent.pop elevation.group elev_m cwd_mean pck_mean ppt_mean tmn_mean
##    <chr>      <chr>            <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
##  1 BH         Low               511.     50.6   0.0292     66.6     5.11
##  2 CC         Low               313      42.5   0.119     112.      6.36
##  3 CP2        High             2244.     79.7  18.4        69.0     3.99
##  4 CP3        High             2266.     60.3  20.7        67.1     3.54
##  5 DPR        Mid              1019.     30.0   5.13       82.5     8.02
##  6 FR         Mid               787      87.8   4.54       57.8     6.15
##  7 IH         Low               454.     41.5   1.77       97.9     7.07
##  8 LV1        High             2593.     55.5  61.7       127.     -1.16
##  9 LV3        High             2354.     43.8  62.3       125.     -1.20
## 10 LVTR1      High             2741.     60.4  64.5       133.     -1.35
## # ℹ 13 more rows
## # ℹ 6 more variables: tmx_mean <dbl>, cwd_sem <dbl>, pck_sem <dbl>,
## #   ppt_sem <dbl>, tmn_sem <dbl>, tmx_sem <dbl>
```

``` r
allpops_historical_grwseason_avgs$elevation.group <- factor(allpops_historical_grwseason_avgs$elevation.group, levels=elev_order)    
#write_csv(allpops_historical_grwseason_avgs, "../output/Climate/growthseason_FlintAvgs_Historic.csv")

allpops_historical_grwseason_avgs %>% ggplot(aes(x=fct_reorder(parent.pop, cwd_mean), y=cwd_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=cwd_mean-cwd_sem,ymax=cwd_mean+cwd_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0")+ 
  labs(fill="Elevation",x="Population", y="Avg CWD" ,title = "Average CWD during Growth Season - Historical Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-36-1.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_CWD_HistoricalClim.png", width = 12, height = 6, units = "in")

allpops_historical_grwseason_avgs %>% ggplot(aes(x=fct_reorder(parent.pop, ppt_mean), y=ppt_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=ppt_mean-ppt_sem,ymax=ppt_mean+ppt_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") + 
  labs(fill="Elevation",x="Population", y="Avg Precip" ,title = "Average Precip during Growth Season - Historical Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-36-2.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_Precip_HistoricalClim.png", width = 12, height = 6, units = "in")

allpops_historical_grwseason_avgs %>% ggplot(aes(x=fct_reorder(parent.pop, tmn_mean), y=tmn_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=tmn_mean-tmn_sem,ymax=tmn_mean+tmn_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") + 
  labs(fill="Elevation",x="Population", y="Avg MinTemp" ,title = "Average MinTemp during Growth Season - Historical Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-36-3.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_MinTemp_HistoricalClim.png", width = 12, height = 6, units = "in")

allpops_historical_grwseason_avgs %>% ggplot(aes(x=fct_reorder(parent.pop, tmx_mean), y=tmx_mean, fill=elev_m)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=tmx_mean-tmx_sem,ymax=tmx_mean+tmx_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") + 
  labs(fill="Elevation",x="Population", y="Avg MaxTemp" ,title = "Average MaxTemp during Growth Season - Historical Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-36-4.png)<!-- -->

``` r
ggsave("../output/Climate/growthseasonAvg_MaxTemp_HistoricalClim.png", width = 12, height = 6, units = "in")
```

## Climate trait correlations

``` r
head(allpops_recent_grwseason)
```

```
## # A tibble: 6 × 16
## # Groups:   parent.pop, elevation.group, elev_m, year [1]
##   month growmonth parent.pop elevation.group elev_m PckSum   Lat  Long  year
##   <dbl>     <dbl> <chr>      <chr>            <dbl>  <dbl> <dbl> <dbl> <dbl>
## 1    12         4 BH         Low               511.      0  37.4 -120.  1994
## 2     1         5 BH         Low               511.      0  37.4 -120.  1994
## 3     2         6 BH         Low               511.      0  37.4 -120.  1994
## 4     3         7 BH         Low               511.      0  37.4 -120.  1994
## 5     4         8 BH         Low               511.      0  37.4 -120.  1994
## 6     5         9 BH         Low               511.      0  37.4 -120.  1994
## # ℹ 7 more variables: cwd <dbl>, pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>,
## #   AvgFirstMonth <dbl>, AvgLastMonth <dbl>
```

``` r
allpops_recent_grwseason <- tibble(allpops_recent_grwseason)
allpops_recent_grwseason %>% cor_test(cwd, ppt, tmn, tmx, method = "pearson")
```

```
## # A tibble: 16 × 8
##    var1  var2    cor    statistic         p conf.low conf.high method 
##    <chr> <chr> <dbl>        <dbl>     <dbl>    <dbl>     <dbl> <chr>  
##  1 cwd   cwd    1    3348053087.  0            1         1     Pearson
##  2 cwd   ppt   -0.57        -48.6 0           -0.586    -0.548 Pearson
##  3 cwd   tmn    0.55         46.3 0            0.529     0.568 Pearson
##  4 cwd   tmx    0.64         58.1 0            0.619     0.652 Pearson
##  5 ppt   cwd   -0.57        -48.6 0           -0.586    -0.548 Pearson
##  6 ppt   ppt    1    4734862083.  0            1         1     Pearson
##  7 ppt   tmn   -0.5         -40.4 1.75e-309   -0.518    -0.476 Pearson
##  8 ppt   tmx   -0.63        -56.7 0           -0.643    -0.609 Pearson
##  9 tmn   cwd    0.55         46.3 0            0.529     0.568 Pearson
## 10 tmn   ppt   -0.5         -40.4 1.75e-309   -0.518    -0.476 Pearson
## 11 tmn   tmn    1           Inf   0            1         1     Pearson
## 12 tmn   tmx    0.95        218.  0            0.949     0.954 Pearson
## 13 tmx   cwd    0.64         58.1 0            0.619     0.652 Pearson
## 14 tmx   ppt   -0.63        -56.7 0           -0.643    -0.609 Pearson
## 15 tmx   tmn    0.95        218.  0            0.949     0.954 Pearson
## 16 tmx   tmx    1           Inf   0            1         1     Pearson
```

``` r
recent_cor_mat <- allpops_recent_grwseason %>% select(cwd, ppt, tmn, tmx) %>% cor_mat()
recent_cor_mat
```

```
## # A tibble: 4 × 5
##   rowname   cwd   ppt   tmn   tmx
## * <chr>   <dbl> <dbl> <dbl> <dbl>
## 1 cwd      1    -0.57  0.55  0.64
## 2 ppt     -0.57  1    -0.5  -0.63
## 3 tmn      0.55 -0.5   1     0.95
## 4 tmx      0.64 -0.63  0.95  1
```

``` r
recent_cor = allpops_recent_grwseason %>% select(cwd, ppt, tmn, tmx) %>% cor()
file_path= "../output/Climate/GrowthSeason_RecentClim_Cors.png"
png(width = 12, height = 6, res= 300, units = "in", file=file_path, type = "cairo")
```

```
## Warning in grSoftVersion(): unable to load shared object '/Library/Frameworks/R.framework/Resources/modules//R_X11.so':
##   dlopen(/Library/Frameworks/R.framework/Resources/modules//R_X11.so, 0x0006): Library not loaded: /opt/X11/lib/libSM.6.dylib
##   Referenced from: <31EADEB5-0A17-3546-9944-9B3747071FE8> /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/modules/R_X11.so
##   Reason: tried: '/opt/X11/lib/libSM.6.dylib' (no such file), '/System/Volumes/Preboot/Cryptexes/OS/opt/X11/lib/libSM.6.dylib' (no such file), '/opt/X11/lib/libSM.6.dylib' (no such file), '/Library/Frameworks/R.framework/Resources/lib/libSM.6.dylib' (no such file), '/Library/Java/JavaVirtualMachines/jdk-11.0.18+10/Contents/Home/lib/server/libSM.6.dylib' (no such file)
```

```
## Warning in cairoVersion(): unable to load shared object '/Library/Frameworks/R.framework/Resources/library/grDevices/libs//cairo.so':
##   dlopen(/Library/Frameworks/R.framework/Resources/library/grDevices/libs//cairo.so, 0x0006): Library not loaded: /opt/X11/lib/libXrender.1.dylib
##   Referenced from: <63619C6D-FE72-3544-BCEF-9C834A5E39D8> /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/grDevices/libs/cairo.so
##   Reason: tried: '/opt/X11/lib/libXrender.1.dylib' (no such file), '/System/Volumes/Preboot/Cryptexes/OS/opt/X11/lib/libXrender.1.dylib' (no such file), '/opt/X11/lib/libXrender.1.dylib' (no such file), '/Library/Frameworks/R.framework/Resources/lib/libXrender.1.dylib' (no such file), '/Library/Java/JavaVirtualMachines/jdk-11.0.18+10/Contents/Home/lib/server/libXrender.1.dylib' (no such file)
```

```
## Warning in png(width = 12, height = 6, res = 300, units = "in", file =
## file_path, : failed to load cairo DLL
```

``` r
corrplot(recent_cor)
```

![](Flint_Growth_Season_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

``` r
dev.off()
```

```
## null device 
##           1
```

``` r
allpops_historical_grwseason <- tibble(allpops_historical_grwseason)
allpops_historical_grwseason %>% cor_test(cwd, ppt, tmn, tmx, method = "pearson")
```

```
## # A tibble: 16 × 8
##    var1  var2    cor    statistic         p conf.low conf.high method 
##    <chr> <chr> <dbl>        <dbl>     <dbl>    <dbl>     <dbl> <chr>  
##  1 cwd   cwd    1    3224718831.  0            1         1     Pearson
##  2 cwd   ppt   -0.53        -43.0 0           -0.555    -0.514 Pearson
##  3 cwd   tmn    0.5          38.8 8.41e-285    0.473     0.517 Pearson
##  4 cwd   tmx    0.6          51.5 0            0.585     0.622 Pearson
##  5 ppt   cwd   -0.53        -43.0 0           -0.555    -0.514 Pearson
##  6 ppt   ppt    1           Inf   0            1         1     Pearson
##  7 ppt   tmn   -0.49        -38.3 2.29e-279   -0.513    -0.469 Pearson
##  8 ppt   tmx   -0.63        -54.9 0           -0.646    -0.611 Pearson
##  9 tmn   cwd    0.5          38.8 8.41e-285    0.473     0.517 Pearson
## 10 tmn   ppt   -0.49        -38.3 2.29e-279   -0.513    -0.469 Pearson
## 11 tmn   tmn    1           Inf   0            1         1     Pearson
## 12 tmn   tmx    0.94        188.  0            0.937     0.944 Pearson
## 13 tmx   cwd    0.6          51.5 0            0.585     0.622 Pearson
## 14 tmx   ppt   -0.63        -54.9 0           -0.646    -0.611 Pearson
## 15 tmx   tmn    0.94        188.  0            0.937     0.944 Pearson
## 16 tmx   tmx    1           Inf   0            1         1     Pearson
```

``` r
historical_cor_mat <- allpops_historical_grwseason %>% select(cwd, ppt, tmn, tmx) %>% cor_mat()
historical_cor_mat
```

```
## # A tibble: 4 × 5
##   rowname   cwd   ppt   tmn   tmx
## * <chr>   <dbl> <dbl> <dbl> <dbl>
## 1 cwd      1    -0.53  0.5   0.6 
## 2 ppt     -0.53  1    -0.49 -0.63
## 3 tmn      0.5  -0.49  1     0.94
## 4 tmx      0.6  -0.63  0.94  1
```

``` r
historical_cor = allpops_historical_grwseason %>% select(cwd, ppt, tmn, tmx) %>% cor()
file_path= "../output/Climate/GrowthSeason_HistoricalClim_Cors.png"
png(width = 12, height = 6, res= 300, units = "in", file=file_path, type = "cairo")
```

```
## Warning in png(width = 12, height = 6, res = 300, units = "in", file =
## file_path, : failed to load cairo DLL
```

``` r
corrplot(historical_cor)
dev.off()
```

```
## null device 
##           1
```
Trait correlations are the same across recent and historical time
periods (during the growth season)
