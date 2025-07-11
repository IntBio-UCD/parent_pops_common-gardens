---
title: "Climate_PCAs_AllYear"
author: "Brandie QC"
date: "2025-06-10"
output: 
  html_document: 
    keep_md: true
---



To Do:

-   Check the significance of the PCA

    -   Remember this paper: Björklund, M. 2019. Be careful with your principal components. Evolution 73: 2151--2158.

# Climate PCAs for Full "Water" Year

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
library(lmerTest)
```

```
## Loading required package: lme4
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## 
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
## 
## 
## Attaching package: 'lmerTest'
## 
## The following object is masked from 'package:lme4':
## 
##     lmer
## 
## The following object is masked from 'package:stats':
## 
##     step
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
library(QBMS) #for function calc_biovars to calculate bioclim variables
library(ggfortify) #easier PCA figures
library(vegan) #for permanova 
```

```
## Loading required package: permute
## Loading required package: lattice
```

``` r
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

## Load Flint Data


``` r
flint_all_year <- read_csv("../output/Climate/flint_climate_UCDpops.csv")
```

```
## Rows: 38775 Columns: 14
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (3): parent.pop, elevation.group, month
## dbl (11): elev_m, Lat, Long, year, aet, cwd, pck, pet, ppt, tmn, tmx
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
head(flint_all_year)
```

```
## # A tibble: 6 × 14
##   parent.pop elevation.group elev_m   Lat  Long  year month   aet   cwd   pck
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr> <dbl> <dbl> <dbl>
## 1 BH         Low               511.  37.4 -120.  1895 dec    3.23  27.9     0
## 2 BH         Low               511.  37.4 -120.  1895 nov    4.89  40.8     0
## 3 BH         Low               511.  37.4 -120.  1895 oct    8.9   80.8     0
## 4 BH         Low               511.  37.4 -120.  1896 apr   72.5   36.2     0
## 5 BH         Low               511.  37.4 -120.  1896 aug   24.5  149.      0
## 6 BH         Low               511.  37.4 -120.  1896 dec    3.38  30.3     0
## # ℹ 4 more variables: pet <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

### Add month numbers


``` r
month_nums <- tibble(month=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),
                     month_nums=c(1:12))
month_nums
```

```
## # A tibble: 12 × 2
##    month month_nums
##    <chr>      <int>
##  1 jan            1
##  2 feb            2
##  3 mar            3
##  4 apr            4
##  5 may            5
##  6 jun            6
##  7 jul            7
##  8 aug            8
##  9 sep            9
## 10 oct           10
## 11 nov           11
## 12 dec           12
```

``` r
flint_all_year_mosnums <- left_join(flint_all_year, month_nums) %>% arrange(parent.pop, year, month_nums)
```

```
## Joining with `by = join_by(month)`
```

``` r
tail(flint_all_year_mosnums, 12) 
```

```
## # A tibble: 12 × 15
##    parent.pop elevation.group elev_m   Lat  Long  year month   aet   cwd   pck
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr> <dbl> <dbl> <dbl>
##  1 YO8        High             2591.  37.8 -119.  2024 jan    5.81  23.8 185. 
##  2 YO8        High             2591.  37.8 -119.  2024 feb    9.54  27.3 462. 
##  3 YO8        High             2591.  37.8 -119.  2024 mar   30.4   27.6 713. 
##  4 YO8        High             2591.  37.8 -119.  2024 apr   24.2   57.9 627. 
##  5 YO8        High             2591.  37.8 -119.  2024 may   31.9   88.0 234. 
##  6 YO8        High             2591.  37.8 -119.  2024 jun   28.8  128.    0  
##  7 YO8        High             2591.  37.8 -119.  2024 jul   31.3  129.    0  
##  8 YO8        High             2591.  37.8 -119.  2024 aug   11.1  122.    0  
##  9 YO8        High             2591.  37.8 -119.  2024 sep    2.9  104.    0  
## 10 YO8        High             2591.  37.8 -119.  2024 oct    0.94  78.3   0  
## 11 YO8        High             2591.  37.8 -119.  2024 nov    4.55  33.6  86.1
## 12 YO8        High             2591.  37.8 -119.  2024 dec    3.78  29.5 141. 
## # ℹ 5 more variables: pet <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>,
## #   month_nums <int>
```

### Water Year


``` r
flint_all_year_wtr_yr <- flint_all_year_mosnums %>% mutate(wtr_yr = year + month_nums %in% c(11:12))
write_csv(flint_all_year_wtr_yr, "../output/Climate/flint_all_year_wtr_yr.csv")
```

## Generate bioclim for all year - 2023

-   annual mean temperature (BIO1)
-   mean diurnal range (BIO2) - (Mean of monthly (max temp - min temp))
-   temperature seasonality (BIO4) (standard deviation \*100)
-   temperature annual range (BIO7) (Max Temperature of Warmest Month - Min Temperature of Coldest Month)
-   mean temp of wettest quarter (BIO8)
-   mean temp of driest quarter (BIO9)
-   annual precipitation (BIO12) - sum of ppt for the entire year (not the avg)
-   precipitation seasonality (BIO15) (Coefficient of Variation)
-   precip of warmest quarter (BIO18)
-   precip of coldest quarter (BIO19)

### Prep


``` r
bioclim_allyear_prep <- flint_all_year_wtr_yr %>% 
  filter(parent.pop != "UCD_Garden", parent.pop != "WL2_Garden") %>%  #remove garden sites 
  rename(tmin=tmn, tmax=tmx, year_cal=year, year=wtr_yr) %>% #rename columns to match what calc_biovars expects, also make sure it uses water year 
  filter(year != "1895", year < "2024") %>%  #remove years with less than 12 months of data
  arrange(parent.pop, year, month)

bioclim_all_year <- tibble(bio1=NA, bio2=NA, bio4=NA, bio7=NA, bio8=NA, bio9=NA, bio12=NA, bio15=NA, bio18=NA, bio19=NA, year=2025) #blank tibble to bind calculations to
bioclim_all_year
```

```
## # A tibble: 1 × 11
##   bio1  bio2  bio4  bio7  bio8  bio9  bio12 bio15 bio18 bio19  year
##   <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <dbl>
## 1 NA    NA    NA    NA    NA    NA    NA    NA    NA    NA     2025
```

``` r
popids <- unique(bioclim_allyear_prep$parent.pop) #list of pop ids for for loop 

pop_elev <- flint_all_year_wtr_yr %>% select(parent.pop:Long) %>% distinct()
```

### Calculation


``` r
for(i in popids) {
  A <- bioclim_allyear_prep %>% filter(parent.pop==i) %>% calc_biovars() %>% mutate(parent.pop=i)
  #print(A)
  bioclim_all_year <- bind_rows(bioclim_all_year, A)
}
unique(bioclim_all_year$parent.pop) #has all the populations in there!
```

```
##  [1] NA      "BH"    "CC"    "CP2"   "CP3"   "DPR"   "FR"    "IH"    "LV1"  
## [10] "LV3"   "LVTR1" "SC"    "SQ1"   "SQ2"   "SQ3"   "TM2"   "WL1"   "WL2"  
## [19] "WR"    "WV"    "YO11"  "YO4"   "YO7"   "YO8"
```

``` r
unique(bioclim_allyear_prep$year)
```

```
##   [1] 1896 1897 1898 1899 1900 1901 1902 1903 1904 1905 1906 1907 1908 1909 1910
##  [16] 1911 1912 1913 1914 1915 1916 1917 1918 1919 1920 1921 1922 1923 1924 1925
##  [31] 1926 1927 1928 1929 1930 1931 1932 1933 1934 1935 1936 1937 1938 1939 1940
##  [46] 1941 1942 1943 1944 1945 1946 1947 1948 1949 1950 1951 1952 1953 1954 1955
##  [61] 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970
##  [76] 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985
##  [91] 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000
## [106] 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
## [121] 2016 2017 2018 2019 2020 2021 2022 2023
```

### SUBSET


``` r
bioclim_all_year_final <- bioclim_all_year %>% 
  select(parent.pop, year, ann_tmean=bio1, mean_diurnal_range=bio2, 
         temp_seasonality=bio4, temp_ann_range=bio7, tmean_wettest_quarter=bio8,
         tmean_driest_quarter=bio9, ann_ppt=bio12, ppt_seasonality=bio15,
         ppt_warmest_quarter=bio18, ppt_coldest_quarter=bio19) %>%
  filter(year!=2025)
head(bioclim_all_year_final)
```

```
## # A tibble: 6 × 12
##   parent.pop  year ann_tmean mean_diurnal_range temp_seasonality temp_ann_range
##   <chr>      <dbl>     <dbl>              <dbl>            <dbl>          <dbl>
## 1 BH          1896      14.6               14.2             644.           33.2
## 2 BH          1897      14.6               13.7             688.           32.1
## 3 BH          1898      14.5               14.5             697.           35.7
## 4 BH          1899      14.7               15.7             668.           36.3
## 5 BH          1900      14.9               14.8             610.           34.5
## 6 BH          1901      15.2               15.1             650.           33.8
## # ℹ 6 more variables: tmean_wettest_quarter <dbl>, tmean_driest_quarter <dbl>,
## #   ann_ppt <dbl>, ppt_seasonality <dbl>, ppt_warmest_quarter <dbl>,
## #   ppt_coldest_quarter <dbl>
```

### Merge with pop info


``` r
pop_elev_bioclim_all_year <- left_join(bioclim_all_year_final, pop_elev) %>% 
  select(parent.pop, elevation.group:Long, year:ppt_coldest_quarter)
```

```
## Joining with `by = join_by(parent.pop)`
```

``` r
head(pop_elev_bioclim_all_year)
```

```
## # A tibble: 6 × 16
##   parent.pop elevation.group elev_m   Lat  Long  year ann_tmean
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl>     <dbl>
## 1 BH         Low               511.  37.4 -120.  1896      14.6
## 2 BH         Low               511.  37.4 -120.  1897      14.6
## 3 BH         Low               511.  37.4 -120.  1898      14.5
## 4 BH         Low               511.  37.4 -120.  1899      14.7
## 5 BH         Low               511.  37.4 -120.  1900      14.9
## 6 BH         Low               511.  37.4 -120.  1901      15.2
## # ℹ 9 more variables: mean_diurnal_range <dbl>, temp_seasonality <dbl>,
## #   temp_ann_range <dbl>, tmean_wettest_quarter <dbl>,
## #   tmean_driest_quarter <dbl>, ann_ppt <dbl>, ppt_seasonality <dbl>,
## #   ppt_warmest_quarter <dbl>, ppt_coldest_quarter <dbl>
```

``` r
#write_csv(pop_elev_bioclim_all_year, "../output/Climate/bioclim_all_year_UCD_pops_wtr_year.csv")
```

## Calculation of recent (last 30 years) and historical climate (prior 30 years) - 2023


``` r
flint_all_year_recent <- flint_all_year_wtr_yr %>% 
  filter(parent.pop != "UCD_Garden", parent.pop != "WL2_Garden") %>%  #remove garden sites 
  filter(wtr_yr>1993 & wtr_yr<=2023) %>% 
  select(parent.pop:month, wtr_yr, cwd, pck, ppt, tmn, tmx)
head(flint_all_year_recent)
```

```
## # A tibble: 6 × 13
##   parent.pop elevation.group elev_m   Lat  Long  year month wtr_yr   cwd   pck
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr>  <dbl> <dbl> <dbl>
## 1 BH         Low               511.  37.4 -120.  1993 nov     1994  46.1     0
## 2 BH         Low               511.  37.4 -120.  1993 dec     1994  30.3     0
## 3 BH         Low               511.  37.4 -120.  1994 jan     1994  31.3     0
## 4 BH         Low               511.  37.4 -120.  1994 feb     1994  41.4     0
## 5 BH         Low               511.  37.4 -120.  1994 mar     1994  61.4     0
## 6 BH         Low               511.  37.4 -120.  1994 apr     1994  58.6     0
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
tail(flint_all_year_recent)
```

```
## # A tibble: 6 × 13
##   parent.pop elevation.group elev_m   Lat  Long  year month wtr_yr   cwd   pck
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr>  <dbl> <dbl> <dbl>
## 1 YO8        High             2591.  37.8 -119.  2023 may     2023  88.0 1426.
## 2 YO8        High             2591.  37.8 -119.  2023 jun     2023 105.   884.
## 3 YO8        High             2591.  37.8 -119.  2023 jul     2023 136.     0 
## 4 YO8        High             2591.  37.8 -119.  2023 aug     2023 109.     0 
## 5 YO8        High             2591.  37.8 -119.  2023 sep     2023  99.2    0 
## 6 YO8        High             2591.  37.8 -119.  2023 oct     2023  74.7    0 
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#write_csv(flint_all_year_recent, "../output/Climate/Flint_Water_Year_Recent_AllMonths.csv")

flint_all_year_historical <- flint_all_year_wtr_yr %>% 
  filter(parent.pop != "UCD_Garden", parent.pop != "WL2_Garden") %>%  #remove garden sites 
  filter(wtr_yr<=1993 & wtr_yr>1963) %>% 
  select(parent.pop:month, wtr_yr, cwd, pck, ppt, tmn, tmx)
head(flint_all_year_historical, 13)
```

```
## # A tibble: 13 × 13
##    parent.pop elevation.group elev_m   Lat  Long  year month wtr_yr   cwd   pck
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr>  <dbl> <dbl> <dbl>
##  1 BH         Low               511.  37.4 -120.  1963 nov     1964  40.8     0
##  2 BH         Low               511.  37.4 -120.  1963 dec     1964  26.9     0
##  3 BH         Low               511.  37.4 -120.  1964 jan     1964  28.1     0
##  4 BH         Low               511.  37.4 -120.  1964 feb     1964  40.4     0
##  5 BH         Low               511.  37.4 -120.  1964 mar     1964  55.9     0
##  6 BH         Low               511.  37.4 -120.  1964 apr     1964  70.3     0
##  7 BH         Low               511.  37.4 -120.  1964 may     1964  42.0     0
##  8 BH         Low               511.  37.4 -120.  1964 jun     1964  42.8     0
##  9 BH         Low               511.  37.4 -120.  1964 jul     1964 161.      0
## 10 BH         Low               511.  37.4 -120.  1964 aug     1964 177.      0
## 11 BH         Low               511.  37.4 -120.  1964 sep     1964 129.      0
## 12 BH         Low               511.  37.4 -120.  1964 oct     1964  92       0
## 13 BH         Low               511.  37.4 -120.  1964 nov     1965  40.2     0
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
tail(flint_all_year_historical, 13)
```

```
## # A tibble: 13 × 13
##    parent.pop elevation.group elev_m   Lat  Long  year month wtr_yr    cwd
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr>  <dbl>  <dbl>
##  1 YO8        High             2591.  37.8 -119.  1992 oct     1992  76.3 
##  2 YO8        High             2591.  37.8 -119.  1992 nov     1993  36.1 
##  3 YO8        High             2591.  37.8 -119.  1992 dec     1993  20.5 
##  4 YO8        High             2591.  37.8 -119.  1993 jan     1993   9.32
##  5 YO8        High             2591.  37.8 -119.  1993 feb     1993  13.9 
##  6 YO8        High             2591.  37.8 -119.  1993 mar     1993  20.0 
##  7 YO8        High             2591.  37.8 -119.  1993 apr     1993  56.8 
##  8 YO8        High             2591.  37.8 -119.  1993 may     1993  91.7 
##  9 YO8        High             2591.  37.8 -119.  1993 jun     1993  80.4 
## 10 YO8        High             2591.  37.8 -119.  1993 jul     1993 125.  
## 11 YO8        High             2591.  37.8 -119.  1993 aug     1993 121.  
## 12 YO8        High             2591.  37.8 -119.  1993 sep     1993 104.  
## 13 YO8        High             2591.  37.8 -119.  1993 oct     1993  73.0 
## # ℹ 4 more variables: pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
#write_csv(flint_all_year_recent, "../output/Climate/Flint_Water_Year_Hist_AllMonths.csv")

bioclim_all_year_recent <- pop_elev_bioclim_all_year %>% filter(year>1993 & year<=2023) #year here means water year (see above code where bioclim vars were calculated)
#head(bioclim_all_year_recent)
#tail(bioclim_all_year_recent)
#write_csv(flint_all_year_recent, "../output/Climate/Bioclim_Water_Year_Recent_AllMonths.csv")

bioclim_all_year_historical <- pop_elev_bioclim_all_year %>% filter(year<=1993 & year>1963)
#head(bioclim_all_year_historical, 13)
#tail(bioclim_all_year_historical, 13)
#write_csv(flint_all_year_recent, "../output/Climate/Bioclim_Water_Year_Hist_AllMonths.csv")
```

### Yearly averages for Flint (for climate distance calc)

``` r
flint_all_year_recent_yravgs <- flint_all_year_recent %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long, wtr_yr) %>% 
  summarise_at(c("cwd",  "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  rename(year=wtr_yr)
flint_all_year_recent_yravgs
```

```
## # A tibble: 690 × 11
## # Groups:   parent.pop, elevation.group, elev_m, Lat, Long [23]
##    parent.pop elevation.group elev_m   Lat  Long  year   cwd   pck   ppt   tmn
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 BH         Low               511.  37.4 -120.  1994  81.5     0  33.5  8.29
##  2 BH         Low               511.  37.4 -120.  1995  62.9     0  71.1  8.31
##  3 BH         Low               511.  37.4 -120.  1996  71.6     0  49.6  9.16
##  4 BH         Low               511.  37.4 -120.  1997  80.1     0  71.6  9.10
##  5 BH         Low               511.  37.4 -120.  1998  60.9     0  79.8  8.35
##  6 BH         Low               511.  37.4 -120.  1999  73.1     0  37.4  7.45
##  7 BH         Low               511.  37.4 -120.  2000  70.2     0  62.4  8.57
##  8 BH         Low               511.  37.4 -120.  2001  76.1     0  32.6  8.54
##  9 BH         Low               511.  37.4 -120.  2002  75.9     0  38.8  8.52
## 10 BH         Low               511.  37.4 -120.  2003  69.4     0  42.4  8.96
## # ℹ 680 more rows
## # ℹ 1 more variable: tmx <dbl>
```

``` r
flint_all_year_historical_yravgs <- flint_all_year_historical %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long, wtr_yr) %>% 
  summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  rename(year=wtr_yr)
flint_all_year_historical_yravgs
```

```
## # A tibble: 690 × 11
## # Groups:   parent.pop, elevation.group, elev_m, Lat, Long [23]
##    parent.pop elevation.group elev_m   Lat  Long  year   cwd   pck   ppt   tmn
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 BH         Low               511.  37.4 -120.  1964  75.5     0  33.6  6.66
##  2 BH         Low               511.  37.4 -120.  1965  68.6     0  52.3  7.14
##  3 BH         Low               511.  37.4 -120.  1966  77.5     0  35.0  7.40
##  4 BH         Low               511.  37.4 -120.  1967  67.3     0  65.3  7.69
##  5 BH         Low               511.  37.4 -120.  1968  79.7     0  33.9  7.49
##  6 BH         Low               511.  37.4 -120.  1969  69.8     0  82.4  7.40
##  7 BH         Low               511.  37.4 -120.  1970  73.8     0  42.2  7.51
##  8 BH         Low               511.  37.4 -120.  1971  72.6     0  37.5  6.91
##  9 BH         Low               511.  37.4 -120.  1972  85.2     0  27.4  7.49
## 10 BH         Low               511.  37.4 -120.  1973  75.3     0  61.5  7.66
## # ℹ 680 more rows
## # ℹ 1 more variable: tmx <dbl>
```

### Merge with bioclim

``` r
bioclim_flint_yrly_avgs_recent <- full_join(flint_all_year_recent_yravgs, bioclim_all_year_recent) 
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, Lat, Long,
## year)`
```

``` r
#write_csv(bioclim_flint_yrly_avgs_recent, "../output/Climate/fullyear_wtr_year_avgs_Recent.csv")
bioclim_flint_yrly_avgs_historical <- full_join(flint_all_year_historical_yravgs, bioclim_all_year_historical) 
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, Lat, Long,
## year)`
```

``` r
#write_csv(bioclim_flint_yrly_avgs_historical, "../output/Climate/fullyear_wtr_year_avgs_Historical.csv")
```

## Calculation of extra historical climate (prior 60 years) - 2023

``` r
flint_all_year_historical_extra <- flint_all_year_wtr_yr %>% 
  filter(parent.pop != "UCD_Garden", parent.pop != "WL2_Garden") %>%  #remove garden sites 
  filter(wtr_yr<=1993 & wtr_yr>1933) %>% 
  select(parent.pop:month, wtr_yr, cwd, pck, ppt, tmn, tmx)
head(flint_all_year_historical_extra, 13)
```

```
## # A tibble: 13 × 13
##    parent.pop elevation.group elev_m   Lat  Long  year month wtr_yr   cwd   pck
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr>  <dbl> <dbl> <dbl>
##  1 BH         Low               511.  37.4 -120.  1933 nov     1934  48.9     0
##  2 BH         Low               511.  37.4 -120.  1933 dec     1934  32.9     0
##  3 BH         Low               511.  37.4 -120.  1934 jan     1934  31.0     0
##  4 BH         Low               511.  37.4 -120.  1934 feb     1934  43.6     0
##  5 BH         Low               511.  37.4 -120.  1934 mar     1934  65.0     0
##  6 BH         Low               511.  37.4 -120.  1934 apr     1934  79.4     0
##  7 BH         Low               511.  37.4 -120.  1934 may     1934  83.3     0
##  8 BH         Low               511.  37.4 -120.  1934 jun     1934  72.9     0
##  9 BH         Low               511.  37.4 -120.  1934 jul     1934 206       0
## 10 BH         Low               511.  37.4 -120.  1934 aug     1934 181.      0
## 11 BH         Low               511.  37.4 -120.  1934 sep     1934 135.      0
## 12 BH         Low               511.  37.4 -120.  1934 oct     1934  89.8     0
## 13 BH         Low               511.  37.4 -120.  1934 nov     1935  45.9     0
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
tail(flint_all_year_historical_extra, 13)
```

```
## # A tibble: 13 × 13
##    parent.pop elevation.group elev_m   Lat  Long  year month wtr_yr    cwd
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr>  <dbl>  <dbl>
##  1 YO8        High             2591.  37.8 -119.  1992 oct     1992  76.3 
##  2 YO8        High             2591.  37.8 -119.  1992 nov     1993  36.1 
##  3 YO8        High             2591.  37.8 -119.  1992 dec     1993  20.5 
##  4 YO8        High             2591.  37.8 -119.  1993 jan     1993   9.32
##  5 YO8        High             2591.  37.8 -119.  1993 feb     1993  13.9 
##  6 YO8        High             2591.  37.8 -119.  1993 mar     1993  20.0 
##  7 YO8        High             2591.  37.8 -119.  1993 apr     1993  56.8 
##  8 YO8        High             2591.  37.8 -119.  1993 may     1993  91.7 
##  9 YO8        High             2591.  37.8 -119.  1993 jun     1993  80.4 
## 10 YO8        High             2591.  37.8 -119.  1993 jul     1993 125.  
## 11 YO8        High             2591.  37.8 -119.  1993 aug     1993 121.  
## 12 YO8        High             2591.  37.8 -119.  1993 sep     1993 104.  
## 13 YO8        High             2591.  37.8 -119.  1993 oct     1993  73.0 
## # ℹ 4 more variables: pck <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
bioclim_all_year_historical_extra <- pop_elev_bioclim_all_year %>% filter(year<=1993 & year>1933)
#head(bioclim_all_year_historical_extra, 13)
#tail(bioclim_all_year_historical_extra, 13)
```


## WL2 Garden 2023

``` r
bioclim_allyear_prep_WL2Grdn <- flint_all_year_wtr_yr %>% 
  filter(parent.pop == "WL2_Garden") %>%  #only keep WL2 garden
  rename(tmin=tmn, tmax=tmx, year_cal=year, year=wtr_yr) %>% #rename columns to match what calc_biovars expects, also make sure it uses water year 
  filter(year=="2023") %>% #year of the experiment only 
  arrange(parent.pop, year, month)

bioclim_all_year_WL2Grdn <- bioclim_allyear_prep_WL2Grdn %>% 
  calc_biovars() %>% 
  mutate(parent.pop="WL2_Garden", year=="2023") 

bioclim_all_year_final_WL2Grdn <- bioclim_all_year_WL2Grdn %>% 
  select(parent.pop, year, ann_tmean=bio1, mean_diurnal_range=bio2, 
         temp_seasonality=bio4, temp_ann_range=bio7, tmean_wettest_quarter=bio8,
         tmean_driest_quarter=bio9, ann_ppt=bio12, ppt_seasonality=bio15,
         ppt_warmest_quarter=bio18, ppt_coldest_quarter=bio19) 

WL2Grdn_elev_bioclim_all_year <- left_join(bioclim_all_year_final_WL2Grdn, pop_elev) %>% 
  select(parent.pop, elevation.group:Long, year:ppt_coldest_quarter)
```

```
## Joining with `by = join_by(parent.pop)`
```

``` r
WL2Grdn_elev_bioclim_all_year
```

```
##   parent.pop elevation.group elev_m      Lat      Long year ann_tmean
## 1 WL2_Garden            High   2020 38.82599 -120.2509 2023   6.92125
##   mean_diurnal_range temp_seasonality temp_ann_range tmean_wettest_quarter
## 1           10.31917         758.8279           32.1                -0.555
##   tmean_driest_quarter ann_ppt ppt_seasonality ppt_warmest_quarter
## 1                 9.33 2486.31        119.8368              115.79
##   ppt_coldest_quarter
## 1             1480.39
```

``` r
WL2Grdn_flint_all_year <- flint_all_year_wtr_yr %>% 
  filter(parent.pop == "WL2_Garden", wtr_yr=="2023") %>%  #only keep WL2 garden
  select(parent.pop:month, wtr_yr, cwd, pck, ppt, tmn, tmx)

WL2Grdn_flint_all_year_summary <- WL2Grdn_flint_all_year %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("cwd",  "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) 

WL2Grdn_flint_bioclim_all_year <- left_join(WL2Grdn_flint_all_year_summary, WL2Grdn_elev_bioclim_all_year) %>% 
  select(-year)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, Lat, Long)`
```
## WL2 Garden 2024

``` r
bioclim_2024_allyear_prep_WL2Grdn <- flint_all_year_wtr_yr %>% 
  filter(parent.pop == "WL2_Garden") %>%  #only keep WL2 garden
  rename(tmin=tmn, tmax=tmx, year_cal=year, year=wtr_yr) %>% #rename columns to match what calc_biovars expects, also make sure it uses water year 
  filter(year=="2024") %>% #year of the experiment only 
  arrange(parent.pop, year, month)

bioclim_2024_all_year_WL2Grdn <- bioclim_2024_allyear_prep_WL2Grdn %>% 
  calc_biovars() %>% 
  mutate(parent.pop="WL2_Garden", year=="2024") 

bioclim_2024_all_year_final_WL2Grdn <- bioclim_2024_all_year_WL2Grdn %>% 
  select(parent.pop, year, ann_tmean=bio1, mean_diurnal_range=bio2, 
         temp_seasonality=bio4, temp_ann_range=bio7, tmean_wettest_quarter=bio8,
         tmean_driest_quarter=bio9, ann_ppt=bio12, ppt_seasonality=bio15,
         ppt_warmest_quarter=bio18, ppt_coldest_quarter=bio19) 

WL2Grdn_elev_bioclim_2024_all_year <- left_join(bioclim_2024_all_year_final_WL2Grdn, pop_elev) %>% 
  select(parent.pop, elevation.group:Long, year:ppt_coldest_quarter)
```

```
## Joining with `by = join_by(parent.pop)`
```

``` r
WL2Grdn_elev_bioclim_2024_all_year
```

```
##   parent.pop elevation.group elev_m      Lat      Long year ann_tmean
## 1 WL2_Garden            High   2020 38.82599 -120.2509 2024  8.952917
##   mean_diurnal_range temp_seasonality temp_ann_range tmean_wettest_quarter
## 1           10.32917         758.4948          31.36              1.296667
##   tmean_driest_quarter ann_ppt ppt_seasonality ppt_warmest_quarter
## 1               12.885 1193.99        109.3371               78.71
##   ppt_coldest_quarter
## 1              621.81
```

``` r
WL2Grdn_flint_2024_all_year <- flint_all_year_wtr_yr %>% 
  filter(parent.pop == "WL2_Garden", wtr_yr=="2024") %>%  #only keep WL2 garden
  select(parent.pop:month, wtr_yr, cwd, pck, ppt, tmn, tmx)

WL2Grdn_flint_2024_all_year_summary <- WL2Grdn_flint_2024_all_year %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("cwd",  "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) 

WL2Grdn_flint_bioclim_2024_all_year <- left_join(WL2Grdn_flint_2024_all_year_summary, WL2Grdn_elev_bioclim_2024_all_year) %>% 
  select(-year)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, Lat, Long)`
```

## Avg across years and months (Flint + bioclim) - 2023

### Calculate avgs for flint


``` r
flint_all_year_recent_avgs <- flint_all_year_recent %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("cwd",  "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Recent") 
#flint_all_year_recent_avgs

flint_all_year_historical_avgs <- flint_all_year_historical %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Historical")
#flint_all_year_historical_avgs

flint_all_year_avgs <- bind_rows(flint_all_year_recent_avgs, flint_all_year_historical_avgs) #combine into 1 dataframe 
head(flint_all_year_avgs)
```

```
## # A tibble: 6 × 11
## # Groups:   parent.pop, elevation.group, elev_m, Lat [6]
##   parent.pop elevation.group elev_m   Lat  Long   cwd    pck   ppt    tmn   tmx
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl>
## 1 BH         Low               511.  37.4 -120.  75.9   0     48.6  8.91   23.6
## 2 CC         Low               313   39.6 -121.  59.9   0     84.5 10.0    23.3
## 3 CP2        High             2244.  38.7 -120.  62.9 218.   107.   1.16   13.4
## 4 CP3        High             2266.  38.7 -120.  46.2 236.   103.   0.512  12.6
## 5 DPR        Mid              1019.  39.2 -121.  27.5   7.63 121.   7.87   20.2
## 6 FR         Mid               787   40.0 -121.  75.5  14.1   84.9  5.69   20.1
## # ℹ 1 more variable: TimePd <chr>
```

``` r
tail(flint_all_year_avgs)
```

```
## # A tibble: 6 × 11
## # Groups:   parent.pop, elevation.group, elev_m, Lat [6]
##   parent.pop elevation.group elev_m   Lat  Long   cwd   pck   ppt   tmn   tmx
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 WR         Mid              1158   39.3 -121.  42.1  49.6 134.   5.22 18.6 
## 2 WV         Mid               749.  40.7 -123.  43.5  36.5  84.6  3.97 18.9 
## 3 YO11       High             2872.  37.9 -119.  54.3 203.   66.9 -4.69  9.69
## 4 YO4        High             2158.  37.8 -120.  53.0 151.   95.0  1.07 13.6 
## 5 YO7        High             2470.  37.8 -120.  50.3 268.  102.  -1.95 11.7 
## 6 YO8        High             2591.  37.8 -119.  64.7 283.  101.  -2.65 11.3 
## # ℹ 1 more variable: TimePd <chr>
```

``` r
#write_csv(flint_all_year_avgs, "../output/Climate/fullyear_FlintAvgs_wtr_year.csv")
```

### Calculate avgs for bioclim 


``` r
bioclim_all_year_recent_avgs <- bioclim_all_year_recent %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("ann_tmean", "mean_diurnal_range", "temp_seasonality", "temp_ann_range",
                 "tmean_wettest_quarter", "tmean_driest_quarter", "ann_ppt",
                 "ppt_seasonality","ppt_warmest_quarter", "ppt_coldest_quarter"),
               c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Recent") 
#bioclim_all_year_recent_avgs

bioclim_all_year_historical_avgs <- bioclim_all_year_historical %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("ann_tmean", "mean_diurnal_range", "temp_seasonality", "temp_ann_range",
                 "tmean_wettest_quarter", "tmean_driest_quarter", "ann_ppt",
                 "ppt_seasonality","ppt_warmest_quarter", "ppt_coldest_quarter"),
               c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Historical") 
#bioclim_all_year_historical_avgs

bioclim_all_year_avgs <- bind_rows(bioclim_all_year_recent_avgs, bioclim_all_year_historical_avgs) #combine into 1 dataframe 
head(bioclim_all_year_avgs)
```

```
## # A tibble: 6 × 16
## # Groups:   parent.pop, elevation.group, elev_m, Lat [6]
##   parent.pop elevation.group elev_m   Lat  Long ann_tmean mean_diurnal_range
##   <chr>      <chr>            <dbl> <dbl> <dbl>     <dbl>              <dbl>
## 1 BH         Low               511.  37.4 -120.     16.2                14.7
## 2 CC         Low               313   39.6 -121.     16.6                13.3
## 3 CP2        High             2244.  38.7 -120.      7.28               12.2
## 4 CP3        High             2266.  38.7 -120.      6.58               12.1
## 5 DPR        Mid              1019.  39.2 -121.     14.1                12.4
## 6 FR         Mid               787   40.0 -121.     12.9                14.4
## # ℹ 9 more variables: temp_seasonality <dbl>, temp_ann_range <dbl>,
## #   tmean_wettest_quarter <dbl>, tmean_driest_quarter <dbl>, ann_ppt <dbl>,
## #   ppt_seasonality <dbl>, ppt_warmest_quarter <dbl>,
## #   ppt_coldest_quarter <dbl>, TimePd <chr>
```

``` r
tail(bioclim_all_year_avgs)
```

```
## # A tibble: 6 × 16
## # Groups:   parent.pop, elevation.group, elev_m, Lat [6]
##   parent.pop elevation.group elev_m   Lat  Long ann_tmean mean_diurnal_range
##   <chr>      <chr>            <dbl> <dbl> <dbl>     <dbl>              <dbl>
## 1 WR         Mid              1158   39.3 -121.     11.9                13.3
## 2 WV         Mid               749.  40.7 -123.     11.4                14.9
## 3 YO11       High             2872.  37.9 -119.      2.50               14.4
## 4 YO4        High             2158.  37.8 -120.      7.35               12.6
## 5 YO7        High             2470.  37.8 -120.      4.85               13.6
## 6 YO8        High             2591.  37.8 -119.      4.31               13.9
## # ℹ 9 more variables: temp_seasonality <dbl>, temp_ann_range <dbl>,
## #   tmean_wettest_quarter <dbl>, tmean_driest_quarter <dbl>, ann_ppt <dbl>,
## #   ppt_seasonality <dbl>, ppt_warmest_quarter <dbl>,
## #   ppt_coldest_quarter <dbl>, TimePd <chr>
```

``` r
#write_csv(bioclim_all_year_avgs, "../output/Climate/fullyear_BioClimAvgs_wtr_year.csv")
```

Merge with flint


``` r
bioclim_flint_all_year_avgs <- full_join(flint_all_year_avgs, bioclim_all_year_avgs) %>% 
  select(TimePd, parent.pop:ppt_coldest_quarter)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, Lat, Long,
## TimePd)`
```

``` r
head(bioclim_flint_all_year_avgs)
```

```
## # A tibble: 6 × 21
## # Groups:   parent.pop, elevation.group, elev_m, Lat [6]
##   TimePd parent.pop elevation.group elev_m   Lat  Long   cwd    pck   ppt    tmn
##   <chr>  <chr>      <chr>            <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl>
## 1 Recent BH         Low               511.  37.4 -120.  75.9   0     48.6  8.91 
## 2 Recent CC         Low               313   39.6 -121.  59.9   0     84.5 10.0  
## 3 Recent CP2        High             2244.  38.7 -120.  62.9 218.   107.   1.16 
## 4 Recent CP3        High             2266.  38.7 -120.  46.2 236.   103.   0.512
## 5 Recent DPR        Mid              1019.  39.2 -121.  27.5   7.63 121.   7.87 
## 6 Recent FR         Mid               787   40.0 -121.  75.5  14.1   84.9  5.69 
## # ℹ 11 more variables: tmx <dbl>, ann_tmean <dbl>, mean_diurnal_range <dbl>,
## #   temp_seasonality <dbl>, temp_ann_range <dbl>, tmean_wettest_quarter <dbl>,
## #   tmean_driest_quarter <dbl>, ann_ppt <dbl>, ppt_seasonality <dbl>,
## #   ppt_warmest_quarter <dbl>, ppt_coldest_quarter <dbl>
```

### Correlations - Recent + Historical


``` r
#normalize the data
climate_normalized_bioclim_flint_all_year_avgs <- bioclim_flint_all_year_avgs %>% ungroup() %>% 
  select(cwd:ppt_coldest_quarter) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_bioclim_flint_all_year_avgs)
```

```
##             cwd        pck        ppt        tmn        tmx  ann_tmean
## [1,]  1.7785471 -0.9367972 -1.9289972  1.4141866  1.6144369  1.5259093
## [2,]  0.4162065 -0.9367972 -0.5691081  1.6728378  1.5530014  1.6191055
## [3,]  0.6732155  0.4303289  0.2959730 -0.3966053 -0.5919655 -0.5004774
## [4,] -0.7521931  0.5445863  0.1231076 -0.5491665 -0.7584042 -0.6610654
## [5,] -2.3485552 -0.8889143  0.8274893  1.1706967  0.8927377  1.0319476
## [6,]  1.7506304 -0.8481292 -0.5518531  0.6619430  0.8658246  0.7716383
##      mean_diurnal_range temp_seasonality temp_ann_range tmean_wettest_quarter
## [1,]        1.441487134        1.2872662      1.6177555             1.4529401
## [2,]       -0.004726068       -0.2588169     -0.2791078             1.6478308
## [3,]       -1.071503185        0.7468844     -0.6406373            -0.5346215
## [4,]       -1.189979340        0.9181595     -0.6181875            -0.7147280
## [5,]       -0.933676311        0.9487769     -0.6429697             0.9609697
## [6,]        1.202712559        1.9133622      1.8804475             0.6229397
##      tmean_driest_quarter    ann_ppt ppt_seasonality ppt_warmest_quarter
## [1,]            1.4100602 -1.9289972       1.7612042         -1.82639599
## [2,]            1.5553884 -0.5691081       1.4690910         -1.06865676
## [3,]           -0.5067601  0.2959730      -0.2608478          0.16723005
## [4,]           -0.6619733  0.1231076      -0.4411963          0.02274441
## [5,]            1.0519122  0.8274893       1.0141137          0.14095396
## [6,]            0.7253838 -0.5518531       1.3237290         -0.68063729
##      ppt_coldest_quarter
## [1,]          -1.9686972
## [2,]          -0.2854258
## [3,]           0.4428731
## [4,]           0.2076647
## [5,]           1.3644989
## [6,]          -0.3122939
```

``` r
dim(climate_normalized_bioclim_flint_all_year_avgs)
```

```
## [1] 46 15
```

``` r
cor.norm = cor(climate_normalized_bioclim_flint_all_year_avgs) #test correlations among the traits
cor.sig <- cor.mtest(climate_normalized_bioclim_flint_all_year_avgs, method = "pearson")

corrplot(cor.norm, type="upper",
         tl.srt = 45, p.mat = cor.sig$p, 
         sig.level = 0.05, insig="blank")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

``` r
#tmn, tmx, tmean_wettest_quarter, tmean_driest_quarter and ann_tmean all highly correlated (97-99%) - only keep ann_tmean 
#temp_ann_range and mean_diurnal range 92% correlated - keep temp_ann_range 
#ann_ppt and ppt 100% correlated (ppt = avg across monts, ann_ppt = avg of the total ppt in a year) - only keep ann_ppt 
#ppt_warmest and coldest quarter highly correlated with ann_ppt and ppt - take both out 
```

Recent

``` r
climate_normalized_bioclim_flint_all_year_avgs_recent <- bioclim_flint_all_year_avgs %>% 
  ungroup() %>% 
  filter(TimePd=="Recent") %>% 
  select(tmn:tmean_driest_quarter, cwd, pck, ppt, ann_ppt:ppt_coldest_quarter) %>% 
  scale() #normalize the data so they're all on the same scale

cor.norm_recent = cor(climate_normalized_bioclim_flint_all_year_avgs_recent) #test correlations among the traits
cor.sig_recent <- cor.mtest(climate_normalized_bioclim_flint_all_year_avgs_recent, method = "pearson")

corrplot(cor.norm_recent, type="upper",
         tl.srt = 45, p.mat = cor.sig_recent$p, 
         sig.level = 0.05, insig="blank")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

``` r
#800 x 734
```

Historical

``` r
climate_normalized_bioclim_flint_all_year_avgs_historic <- bioclim_flint_all_year_avgs %>% 
  ungroup() %>% 
  filter(TimePd=="Historical") %>% 
  select(tmn:tmean_driest_quarter, cwd, pck, ppt, ann_ppt:ppt_coldest_quarter) %>% 
  scale() #normalize the data so they're all on the same scale

cor.norm_historic = cor(climate_normalized_bioclim_flint_all_year_avgs_historic) #test correlations among the traits
cor.sig_historic <- cor.mtest(climate_normalized_bioclim_flint_all_year_avgs_historic, method = "pearson")

corrplot(cor.norm_historic, type="upper",
         tl.srt = 45, p.mat = cor.sig_historic$p, 
         sig.level = 0.05, insig="blank")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

``` r
#800 x 734
```


### PCA - Recent + Historical


``` r
all_bioclim_flint_avgs.pc = prcomp(bioclim_flint_all_year_avgs[c(7:8, 12, 14:15, 18:19)], scale = TRUE, center = TRUE)
str(all_bioclim_flint_avgs.pc)
```

```
## List of 5
##  $ sdev    : num [1:7] 1.891 1.078 1.05 0.752 0.604 ...
##  $ rotation: num [1:7, 1:7] -0.28 0.479 -0.44 -0.311 -0.215 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:7] "cwd" "pck" "ann_tmean" "temp_seasonality" ...
##   .. ..$ : chr [1:7] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:7] 55.03 149.32 9.49 672.21 32.25 ...
##   ..- attr(*, "names")= chr [1:7] "cwd" "pck" "ann_tmean" "temp_seasonality" ...
##  $ scale   : Named num [1:7] 11.71 159.39 4.42 20.58 1.14 ...
##   ..- attr(*, "names")= chr [1:7] "cwd" "pck" "ann_tmean" "temp_seasonality" ...
##  $ x       : num [1:46, 1:7] -3.923 -1.994 0.378 0.847 -0.453 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:7] "PC1" "PC2" "PC3" "PC4" ...
##  - attr(*, "class")= chr "prcomp"
```

plot % Variance Explained


``` r
summary(all_bioclim_flint_avgs.pc)
```

```
## Importance of components:
##                           PC1    PC2    PC3     PC4     PC5    PC6     PC7
## Standard deviation     1.8915 1.0775 1.0505 0.75177 0.60401 0.4266 0.21412
## Proportion of Variance 0.5111 0.1659 0.1577 0.08074 0.05212 0.0260 0.00655
## Cumulative Proportion  0.5111 0.6770 0.8346 0.91533 0.96745 0.9935 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:7,2,pad="0")),
       percent_var=all_bioclim_flint_avgs.pc$sdev[1:7]^2/sum(all_bioclim_flint_avgs.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

Combine PCs with metadata


``` r
all_bioclim_flint_avgs.pc.dat = data.frame(all_bioclim_flint_avgs.pc$x)

all_bioclim_flint_avgs_locs.pc = cbind(bioclim_flint_all_year_avgs, all_bioclim_flint_avgs.pc.dat)

all_bioclim_flint_avgs_loadings = data.frame(varnames=rownames(all_bioclim_flint_avgs.pc$rotation), all_bioclim_flint_avgs.pc$rotation)
all_bioclim_flint_avgs_loadings
```

```
##                          varnames        PC1         PC2         PC3
## cwd                           cwd -0.2797103 -0.16422605 -0.72255315
## pck                           pck  0.4788965 -0.22506720 -0.13823349
## ann_tmean               ann_tmean -0.4402340  0.17430773  0.25402702
## temp_seasonality temp_seasonality -0.3110189 -0.34311863  0.47280196
## temp_ann_range     temp_ann_range -0.2149613 -0.78689618  0.09946240
## ann_ppt                   ann_ppt  0.4247974  0.06027161  0.39685573
## ppt_seasonality   ppt_seasonality -0.4185933  0.38913072  0.05787736
##                          PC4        PC5        PC6         PC7
## cwd              -0.22442677  0.3806515  0.4126631 -0.08458757
## pck              -0.21672469  0.3193782 -0.2782778  0.68882783
## ann_tmean         0.48591759  0.3234923  0.2911955  0.53453078
## temp_seasonality -0.66346611 -0.1341457  0.2771019  0.16439992
## temp_ann_range    0.33856853  0.2132447 -0.3420116 -0.21821861
## ann_ppt          -0.04118123  0.6432578  0.3129935 -0.38076052
## ppt_seasonality  -0.33171571  0.4137701 -0.6129873 -0.11407334
```


``` r
autoplot(all_bioclim_flint_avgs.pc, data = bioclim_flint_all_year_avgs,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-24-1.png)<!-- -->


``` r
autoplot(all_bioclim_flint_avgs.pc, data = bioclim_flint_all_year_avgs,
         x=1, y=4,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-25-1.png)<!-- -->


``` r
all_bioclim_flint_avgs_locs.pc_avg <- all_bioclim_flint_avgs_locs.pc %>%
  group_by(parent.pop, elev_m, TimePd) %>%
  summarise(across(.cols=starts_with("PC", ignore.case = FALSE), .fns = mean)) %>%
  ungroup()
```

```
## `summarise()` has grouped output by 'parent.pop', 'elev_m'. You can override
## using the `.groups` argument.
```

``` r
all_bioclim_flint_avgs.pc
```

```
## Standard deviations (1, .., p=7):
## [1] 1.8914552 1.0775169 1.0504922 0.7517688 0.6040113 0.4265976 0.2141242
## 
## Rotation (n x k) = (7 x 7):
##                         PC1         PC2         PC3         PC4        PC5
## cwd              -0.2797103 -0.16422605 -0.72255315 -0.22442677  0.3806515
## pck               0.4788965 -0.22506720 -0.13823349 -0.21672469  0.3193782
## ann_tmean        -0.4402340  0.17430773  0.25402702  0.48591759  0.3234923
## temp_seasonality -0.3110189 -0.34311863  0.47280196 -0.66346611 -0.1341457
## temp_ann_range   -0.2149613 -0.78689618  0.09946240  0.33856853  0.2132447
## ann_ppt           0.4247974  0.06027161  0.39685573 -0.04118123  0.6432578
## ppt_seasonality  -0.4185933  0.38913072  0.05787736 -0.33171571  0.4137701
##                         PC6         PC7
## cwd               0.4126631 -0.08458757
## pck              -0.2782778  0.68882783
## ann_tmean         0.2911955  0.53453078
## temp_seasonality  0.2771019  0.16439992
## temp_ann_range   -0.3420116 -0.21821861
## ann_ppt           0.3129935 -0.38076052
## ppt_seasonality  -0.6129873 -0.11407334
```


``` r
all_bioclim_flint_avgs_locs.pc_avg %>% 
  ggplot(aes(x=PC1, y=PC2, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point() +
  facet_wrap(~TimePd) +
  coord_fixed(ratio = 1.5)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

## Permanova - 2023
See Jenny's github: https://github.com/jrgremer/Seedbanks_drought_deluge  

``` r
all_bioclim_flint_avgs_locs.pc_dist <- all_bioclim_flint_avgs_locs.pc %>% ungroup() %>% select(PC1:PC7)
dist_matrix_all_year <- dist(all_bioclim_flint_avgs_locs.pc_dist, method = "euclidian") #use a distance function to calculate euclidian distance in PCA space
permanova_results_all_year <- adonis2(dist_matrix_all_year ~ TimePd*elev_m*Lat, data = all_bioclim_flint_avgs_locs.pc) #use adonis2 to run the permanova
#rows in data must be in same order as rows in distance matrix 

#look at output 
summary(permanova_results_all_year)
```

```
##        Df          SumOfSqs           R2               F        
##  Min.   : 7.0   Min.   :105.3   Min.   :0.3344   Min.   :10.81  
##  1st Qu.:22.5   1st Qu.:157.5   1st Qu.:0.5000   1st Qu.:10.81  
##  Median :38.0   Median :209.7   Median :0.6656   Median :10.81  
##  Mean   :30.0   Mean   :210.0   Mean   :0.6667   Mean   :10.81  
##  3rd Qu.:41.5   3rd Qu.:262.3   3rd Qu.:0.8328   3rd Qu.:10.81  
##  Max.   :45.0   Max.   :315.0   Max.   :1.0000   Max.   :10.81  
##                                                  NA's   :2      
##      Pr(>F)     
##  Min.   :0.001  
##  1st Qu.:0.001  
##  Median :0.001  
##  Mean   :0.001  
##  3rd Qu.:0.001  
##  Max.   :0.001  
##  NA's   :2
```

``` r
permanova_results_all_year
```

```
## Permutation test for adonis under reduced model
## Permutation: free
## Number of permutations: 999
## 
## adonis2(formula = dist_matrix_all_year ~ TimePd * elev_m * Lat, data = all_bioclim_flint_avgs_locs.pc)
##          Df SumOfSqs      R2      F Pr(>F)    
## Model     7   209.67 0.66561 10.806  0.001 ***
## Residual 38   105.33 0.33439                  
## Total    45   315.00 1.00000                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
permanova_results_all_year$`Pr(>F)`
```

```
## [1] 0.001    NA    NA
```

``` r
permanova_results_all_year_terms <- adonis2(dist_matrix_all_year ~ TimePd*elev_m*Lat, data = all_bioclim_flint_avgs_locs.pc, by = "terms")
permanova_results_all_year_terms
```

```
## Permutation test for adonis under reduced model
## Terms added sequentially (first to last)
## Permutation: free
## Number of permutations: 999
## 
## adonis2(formula = dist_matrix_all_year ~ TimePd * elev_m * Lat, data = all_bioclim_flint_avgs_locs.pc, by = "terms")
##                   Df SumOfSqs      R2       F Pr(>F)    
## TimePd             1   28.589 0.09076 10.3137  0.001 ***
## elev_m             1  109.439 0.34743 39.4811  0.001 ***
## Lat                1   55.665 0.17671 20.0816  0.001 ***
## TimePd:elev_m      1    3.008 0.00955  1.0853  0.319    
## TimePd:Lat         1    3.629 0.01152  1.3092  0.243    
## elev_m:Lat         1    9.033 0.02868  3.2587  0.020 *  
## TimePd:elev_m:Lat  1    0.303 0.00096  0.1093  0.991    
## Residual          38  105.334 0.33439                   
## Total             45  315.000 1.00000                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## LM on PCs (follow up on permanova) - 2023

For each PC fit a model with timepd and parent.pop/elev to test for climate change


``` r
lmer_results_all_year <- all_bioclim_flint_avgs_locs.pc %>%
  ungroup() %>% 
  select(TimePd, parent.pop, elev_m, Lat, Long, PC1:PC7) %>% 
  pivot_longer(starts_with("PC", ignore.case = FALSE), 
               names_to = "PC", values_to = "value") %>% 
  group_by(PC) %>% 
  nest(data=c(TimePd, parent.pop, elev_m, Lat, Long, value)) %>% 
  mutate(glm=map(data, ~ glm(value ~ TimePd*elev_m*Lat,
                               data=.x)),
         anova = map(glm, ~ broom.mixed::tidy(anova(.x))))

#lmer_results_all_year
```


``` r
PC_anova <- lmer_results_all_year %>% select(-data, -glm) %>% unnest(anova) %>%
  select(PC, term, p.value) %>%
  filter(p.value < 0.05) %>%
  arrange(term, p.value)

PC_anova
```

```
## # A tibble: 16 × 3
## # Groups:   PC [6]
##    PC    term        p.value
##    <chr> <chr>         <dbl>
##  1 PC1   Lat        2.38e- 9
##  2 PC3   Lat        8.65e- 5
##  3 PC2   Lat        2.45e- 3
##  4 PC5   Lat        6.24e- 3
##  5 PC4   Lat        2.36e- 2
##  6 PC4   TimePd     7.54e-11
##  7 PC1   TimePd     3.05e- 5
##  8 PC5   TimePd     1.57e- 2
##  9 PC7   TimePd     4.83e- 2
## 10 PC1   elev_m     5.62e-17
## 11 PC4   elev_m     2.07e- 7
## 12 PC3   elev_m     2.05e- 2
## 13 PC7   elev_m:Lat 1.44e- 3
## 14 PC5   elev_m:Lat 2.43e- 3
## 15 PC1   elev_m:Lat 1.05e- 2
## 16 PC4   elev_m:Lat 2.57e- 2
```

``` r
lmer_results_all_year %>% select(-data, -glm) %>% unnest(anova) %>% 
  filter(PC=="PC1"| PC=="PC4") #the two PCs with most sig p-values for timepd
```

```
## # A tibble: 16 × 8
## # Groups:   PC [2]
##    PC    term      df deviance df.residual residual.deviance statistic   p.value
##    <chr> <chr>  <int>    <dbl>       <int>             <dbl>     <dbl>     <dbl>
##  1 PC1   NULL      NA NA                45            161.    NA       NA       
##  2 PC1   TimePd     1  1.06e+1          44            150.    22.4      3.05e- 5
##  3 PC1   elev_m     1  9.86e+1          43             51.8  207.       5.62e-17
##  4 PC1   Lat        1  2.86e+1          42             23.2   60.2      2.38e- 9
##  5 PC1   TimeP…     1  1.16e+0          41             22.0    2.44     1.26e- 1
##  6 PC1   TimeP…     1  5.02e-1          40             21.5    1.06     3.10e- 1
##  7 PC1   elev_…     1  3.45e+0          39             18.1    7.25     1.05e- 2
##  8 PC1   TimeP…     1  1.88e-2          38             18.1    0.0397   8.43e- 1
##  9 PC4   NULL      NA NA                45             25.4   NA       NA       
## 10 PC4   TimePd     1  1.17e+1          44             13.7   79.5      7.54e-11
## 11 PC4   elev_m     1  5.90e+0          43              7.81  40.0      2.07e- 7
## 12 PC4   Lat        1  8.21e-1          42              6.98   5.56     2.36e- 2
## 13 PC4   TimeP…     1  5.52e-1          41              6.43   3.74     6.07e- 2
## 14 PC4   TimeP…     1  2.76e-2          40              6.41   0.187    6.68e- 1
## 15 PC4   elev_…     1  7.96e-1          39              5.61   5.39     2.57e- 2
## 16 PC4   TimeP…     1  5.52e-4          38              5.61   0.00374  9.52e- 1
```

``` r
#mod_test <- glm(PC4 ~ TimePd*elev_m*Lat, data=all_bioclim_flint_avgs_locs.pc)
#summary(mod_test)
#anova(mod_test)
```


``` r
autoplot(all_bioclim_flint_avgs.pc, data = bioclim_flint_all_year_avgs,
         x=1, y=4,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

``` r
all_bioclim_flint_avgs_locs.pc_avg %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-31-2.png)<!-- -->

``` r
all_bioclim_flint_avgs_locs.pc_avg %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_text_repel(aes(label = parent.pop)) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed")  +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-31-3.png)<!-- -->

``` r
#high elev climate seems to be shifting to be more similar to low elev 
```


``` r
home_sites_pca <- all_bioclim_flint_avgs_locs.pc_avg %>%  
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  #geom_point(size=2, alpha=0.7) +
  labs(x="PC1 (51.11%)", y="PC4 (8.07%)", color="Elevation (m)") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  annotate("text", x = 1.3, y= 0.52, label = "WL2", colour = "purple") +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8) +
  annotate("text", x = -4, y = -5.1, label = "Warm \n No Snow") +
  annotate("text", x = 4, y = -5.1, label = "Cold \n Snow") +
  annotate("text", x = -5.4, y = -3.5, label = "High Temp \n Seasonality") +
  annotate("text", x = -5.4, y = 1.5, label = "Low Temp \n Seasonality") +
  coord_cartesian(ylim = c(-4.1, 2), xlim = c(-4,4), clip = "off") +
  theme_classic() +
  theme(text=element_text(size=28))
#ggsave("../output/Climate/Wtr_Year_PC1-PC4.png", width = 7.4, height = 6, units = "in")

## add WL2 garden 2023 and 2024
WL2Grdn_pc_prep <- WL2Grdn_flint_bioclim_all_year %>% ungroup() %>% select(cwd:pck, ann_tmean, temp_seasonality, temp_ann_range, ann_ppt, ppt_seasonality)
#scale(WL2Grdn_pc_prep, all_bioclim_flint_avgs.pc$center, all_bioclim_flint_avgs.pc$scale) %*% all_bioclim_flint_avgs.pc$rotation  #gives same result as below 
WL2Grdn_predicted <- predict(all_bioclim_flint_avgs.pc, newdata = WL2Grdn_pc_prep)
#WL2Grdn_predicted

WL2Grdn_pc_prep_2024 <- WL2Grdn_flint_bioclim_2024_all_year %>% ungroup() %>% select(cwd:pck, ann_tmean, temp_seasonality, temp_ann_range, ann_ppt, ppt_seasonality)
WL2Grdn_2024_predicted <- predict(all_bioclim_flint_avgs.pc, newdata = WL2Grdn_pc_prep_2024)

#str(home_sites_pca) #can add predicted WL2 point to the existing plot's data 
home_sites_pca$data <- rbind(home_sites_pca$data, 
  data.frame(
    parent.pop = "WL2_Garden",
    elev_m = 2020,
    TimePd = c("2023", "2024"),
    PC1 = c(WL2Grdn_predicted[, "PC1"], WL2Grdn_2024_predicted[, "PC1"]),
    PC2 = c(WL2Grdn_predicted[, "PC2"], WL2Grdn_2024_predicted[, "PC2"]),
    PC3 = c(WL2Grdn_predicted[, "PC3"], WL2Grdn_2024_predicted[, "PC3"]),
    PC4 = c(WL2Grdn_predicted[, "PC4"], WL2Grdn_2024_predicted[, "PC4"]),
    PC5 = c(WL2Grdn_predicted[, "PC5"], WL2Grdn_2024_predicted[, "PC5"]),
    PC6 = c(WL2Grdn_predicted[, "PC6"], WL2Grdn_2024_predicted[, "PC6"]),
    PC7 = c(WL2Grdn_predicted[, "PC7"], WL2Grdn_2024_predicted[, "PC7"]),
    group = c("new", "new2")
  )
)

home_sites_pca + 
  geom_point(data=filter(home_sites_pca$data, parent.pop == "WL2_Garden"), size=3, shape = 8, show_guide = FALSE) +
  annotate("text", x = 2.55, y= -3.6, label = "WL2 Garden \n 2023", colour = "purple") +
  annotate("text", x = -0.8, y= -3.2, label = "WL2 Garden \n 2024", colour = "purple") 
```

```
## Warning: The `show_guide` argument of `layer()` is deprecated as of ggplot2 2.0.0.
## ℹ Please use the `show.legend` argument instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

``` r
#ggsave("../output/Climate/Wtr_Year_PC1-PC4_PlusGarden.png", width = 7.4, height = 6, units = "in")
```

## Extra historical PCA (60 years) - 2023

### Extra Historical Averages

``` r
flint_all_year_historical_avgs_extra <- flint_all_year_historical_extra %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Historical")
#flint_all_year_historical_avgs_extra

flint_all_year_avgs_extra <- bind_rows(flint_all_year_recent_avgs, flint_all_year_historical_avgs_extra) #combine into 1 dataframe 

bioclim_all_year_historical_avgs_extra <- bioclim_all_year_historical_extra %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("ann_tmean", "mean_diurnal_range", "temp_seasonality", "temp_ann_range",
                 "tmean_wettest_quarter", "tmean_driest_quarter", "ann_ppt",
                 "ppt_seasonality","ppt_warmest_quarter", "ppt_coldest_quarter"),
               c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Historical") 
#bioclim_all_year_historical_avgs_extra

bioclim_all_year_avgs_extra <- bind_rows(bioclim_all_year_recent_avgs, bioclim_all_year_historical_avgs_extra) #combine into 1 dataframe 


bioclim_flint_all_year_avgs_extra <- full_join(flint_all_year_avgs_extra, bioclim_all_year_avgs_extra) %>% 
  select(TimePd, parent.pop:ppt_coldest_quarter)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, Lat, Long,
## TimePd)`
```

``` r
head(bioclim_flint_all_year_avgs_extra)
```

```
## # A tibble: 6 × 21
## # Groups:   parent.pop, elevation.group, elev_m, Lat [6]
##   TimePd parent.pop elevation.group elev_m   Lat  Long   cwd    pck   ppt    tmn
##   <chr>  <chr>      <chr>            <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl>
## 1 Recent BH         Low               511.  37.4 -120.  75.9   0     48.6  8.91 
## 2 Recent CC         Low               313   39.6 -121.  59.9   0     84.5 10.0  
## 3 Recent CP2        High             2244.  38.7 -120.  62.9 218.   107.   1.16 
## 4 Recent CP3        High             2266.  38.7 -120.  46.2 236.   103.   0.512
## 5 Recent DPR        Mid              1019.  39.2 -121.  27.5   7.63 121.   7.87 
## 6 Recent FR         Mid               787   40.0 -121.  75.5  14.1   84.9  5.69 
## # ℹ 11 more variables: tmx <dbl>, ann_tmean <dbl>, mean_diurnal_range <dbl>,
## #   temp_seasonality <dbl>, temp_ann_range <dbl>, tmean_wettest_quarter <dbl>,
## #   tmean_driest_quarter <dbl>, ann_ppt <dbl>, ppt_seasonality <dbl>,
## #   ppt_warmest_quarter <dbl>, ppt_coldest_quarter <dbl>
```

### Correlations - Extra Historical 


``` r
#normalize the data
climate_normalized_bioclim_flint_all_year_avgs_extra <- bioclim_flint_all_year_avgs_extra %>% ungroup() %>% 
  select(cwd:ppt_coldest_quarter) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_bioclim_flint_all_year_avgs_extra)
```

```
##             cwd        pck        ppt        tmn        tmx  ann_tmean
## [1,]  1.7768948 -0.9396132 -1.9208166  1.4245683  1.5904281  1.5189268
## [2,]  0.4148664 -0.9396132 -0.5742173  1.6836586  1.5292121  1.6120433
## [3,]  0.6718165  0.4581622  0.2824096 -0.3892978 -0.6080917 -0.5057276
## [4,] -0.7532655  0.5749811  0.1112336 -0.5421180 -0.7739358 -0.6661783
## [5,] -2.3492618 -0.8906568  0.8087316  1.1806651  0.8713073  1.0253873
## [6,]  1.7489845 -0.8489574 -0.5571309  0.6710475  0.8444904  0.7653007
##      mean_diurnal_range temp_seasonality temp_ann_range tmean_wettest_quarter
## [1,]          1.2859021        1.2989086      1.3722142             1.4900356
## [2,]         -0.1248121       -0.3720245     -0.5019251             1.6835586
## [3,]         -1.1654038        0.7148896     -0.8591235            -0.4835778
## [4,]         -1.2809719        0.8999955     -0.8369426            -0.6624204
## [5,]         -1.0309601        0.9330854     -0.8614280             1.0015177
## [6,]          1.0529885        1.9755634      1.6317591             0.6658599
##      tmean_driest_quarter    ann_ppt ppt_seasonality ppt_warmest_quarter
## [1,]            1.3937166 -1.9208166       1.6906659        -1.871594939
## [2,]            1.5380567 -0.5742173       1.4044909        -1.103136179
## [3,]           -0.5100714  0.2824096      -0.2902806         0.150234390
## [4,]           -0.6642293  0.1112336      -0.4669629         0.003704755
## [5,]            1.0380036  0.8087316       0.9587626         0.123586585
## [6,]            0.7136953 -0.5571309       1.2620839        -0.709627504
##      ppt_coldest_quarter
## [1,]          -2.0618668
## [2,]          -0.3834225
## [3,]           0.3427879
## [4,]           0.1082540
## [5,]           1.2617708
## [6,]          -0.4102135
```

``` r
dim(climate_normalized_bioclim_flint_all_year_avgs_extra)
```

```
## [1] 46 15
```

``` r
cor.norm_extra = cor(climate_normalized_bioclim_flint_all_year_avgs_extra) #test correlations among the traits
cor.sig_extra <- cor.mtest(climate_normalized_bioclim_flint_all_year_avgs_extra, method = "pearson")

corrplot(cor.norm_extra, type="upper",
         tl.srt = 45, p.mat = cor.sig_extra$p, 
         sig.level = 0.05, insig="blank")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

``` r
#tmn, tmx, tmean_wettest_quarter, tmean_driest_quarter and ann_tmean all highly correlated - only keep ann_tmean 
#temp_ann_range and mean_diurnal range correlated - keep temp_ann_range 
#ann_ppt and ppt 100% correlated (ppt = avg across monts, ann_ppt = avg of the total ppt in a year) - only keep ann_ppt 
#ppt_warmest and coldest quarter highly correlated with ann_ppt and ppt - take both out 
```

Extra Historical

``` r
climate_normalized_bioclim_flint_all_year_avgs_extra_historic <- bioclim_flint_all_year_avgs_extra %>% 
  ungroup() %>% 
  filter(TimePd=="Historical") %>% 
  select(tmn:tmean_driest_quarter, cwd, pck, ppt, ann_ppt:ppt_coldest_quarter) %>% 
  scale() #normalize the data so they're all on the same scale

cor.norm_historic_extra = cor(climate_normalized_bioclim_flint_all_year_avgs_extra_historic) #test correlations among the traits
cor.sig_historic_extra <- cor.mtest(climate_normalized_bioclim_flint_all_year_avgs_extra_historic, method = "pearson")

corrplot(cor.norm_historic_extra, type="upper",
         tl.srt = 45, p.mat = cor.sig_historic_extra$p, 
         sig.level = 0.05, insig="blank")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-35-1.png)<!-- -->

``` r
#800 x 734
```


### PCA - Recent + Extra Historical


``` r
all_bioclim_flint_avgs.pc_extra = prcomp(bioclim_flint_all_year_avgs_extra[c(7:8, 12, 14:15, 18:19)], scale = TRUE, center = TRUE)
str(all_bioclim_flint_avgs.pc_extra)
```

```
## List of 5
##  $ sdev    : num [1:7] 1.882 1.105 1.057 0.771 0.565 ...
##  $ rotation: num [1:7, 1:7] -0.286 0.488 -0.438 -0.293 -0.201 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:7] "cwd" "pck" "ann_tmean" "temp_seasonality" ...
##   .. ..$ : chr [1:7] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:7] 55.04 146.48 9.52 673.97 32.52 ...
##   ..- attr(*, "names")= chr [1:7] "cwd" "pck" "ann_tmean" "temp_seasonality" ...
##  $ scale   : Named num [1:7] 11.71 155.89 4.42 19.04 1.16 ...
##   ..- attr(*, "names")= chr [1:7] "cwd" "pck" "ann_tmean" "temp_seasonality" ...
##  $ x       : num [1:46, 1:7] -3.828 -1.901 0.459 0.932 -0.346 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:7] "PC1" "PC2" "PC3" "PC4" ...
##  - attr(*, "class")= chr "prcomp"
```

plot % Variance Explained


``` r
summary(all_bioclim_flint_avgs.pc_extra)
```

```
## Importance of components:
##                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
## Standard deviation     1.8822 1.1052 1.0569 0.77126 0.56456 0.38675 0.23608
## Proportion of Variance 0.5061 0.1745 0.1596 0.08498 0.04553 0.02137 0.00796
## Cumulative Proportion  0.5061 0.6806 0.8402 0.92514 0.97067 0.99204 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:7,2,pad="0")),
       percent_var=all_bioclim_flint_avgs.pc_extra$sdev[1:7]^2/sum(all_bioclim_flint_avgs.pc_extra$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

Combine PCs with metadata


``` r
all_bioclim_flint_avgs.pc_extra.dat = data.frame(all_bioclim_flint_avgs.pc_extra$x)

all_bioclim_flint_avgs_locs.pc_extra = cbind(bioclim_flint_all_year_avgs_extra, all_bioclim_flint_avgs.pc_extra.dat)

all_bioclim_flint_avgs_loadings_extra = data.frame(varnames=rownames(all_bioclim_flint_avgs.pc_extra$rotation), all_bioclim_flint_avgs.pc_extra$rotation)
all_bioclim_flint_avgs_loadings_extra
```

```
##                          varnames        PC1          PC2        PC3
## cwd                           cwd -0.2864642 -0.081486756 -0.7212787
## pck                           pck  0.4875103  0.073129331 -0.2201763
## ann_tmean               ann_tmean -0.4379425 -0.009682020  0.3060436
## temp_seasonality temp_seasonality -0.2928951  0.370004765  0.4069566
## temp_ann_range     temp_ann_range -0.2005957  0.783105091 -0.1833693
## ann_ppt                   ann_ppt  0.4417826  0.006364002  0.3581785
## ppt_seasonality   ppt_seasonality -0.4089991 -0.487566054  0.1004339
##                          PC4         PC5         PC6        PC7
## cwd              -0.22862456 -0.48216828  0.30890753 -0.1042878
## pck              -0.22927006 -0.24936301 -0.37422176  0.6735871
## ann_tmean         0.48211048 -0.42666603  0.17227107  0.5199021
## temp_seasonality -0.75381257 -0.06096055  0.16225786  0.1158363
## temp_ann_range    0.24311947 -0.09850237 -0.45014513 -0.2035757
## ann_ppt           0.02012349 -0.69545637  0.02929008 -0.4376909
## ppt_seasonality  -0.18705646 -0.16189338 -0.71066217 -0.1366447
```


``` r
autoplot(all_bioclim_flint_avgs.pc_extra, data = bioclim_flint_all_year_avgs_extra,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-39-1.png)<!-- -->


``` r
autoplot(all_bioclim_flint_avgs.pc_extra, data = bioclim_flint_all_year_avgs_extra,
         x=1, y=4,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-40-1.png)<!-- -->


``` r
all_bioclim_flint_avgs_locs.pc_avg_extra <- all_bioclim_flint_avgs_locs.pc_extra %>%
  group_by(parent.pop, elev_m, TimePd) %>%
  summarise(across(.cols=starts_with("PC", ignore.case = FALSE), .fns = mean)) %>%
  ungroup()
```

```
## `summarise()` has grouped output by 'parent.pop', 'elev_m'. You can override
## using the `.groups` argument.
```


``` r
all_bioclim_flint_avgs_locs.pc_avg_extra %>% 
  ggplot(aes(x=PC1, y=PC2, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point() +
  facet_wrap(~TimePd) +
  coord_fixed(ratio = 1.5)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-42-1.png)<!-- -->

## Permanova - Extra Historical 2023
See Jenny's github: https://github.com/jrgremer/Seedbanks_drought_deluge  

``` r
all_bioclim_flint_avgs_locs.pc_dist_extra <- all_bioclim_flint_avgs_locs.pc_extra %>% ungroup() %>% select(PC1:PC7)
dist_matrix_all_year_extra <- dist(all_bioclim_flint_avgs_locs.pc_dist_extra, method = "euclidian") #use a distance function to calculate euclidian distance in PCA space
permanova_results_all_year_extra <- adonis2(dist_matrix_all_year_extra ~ TimePd*elev_m*Lat, data = all_bioclim_flint_avgs_locs.pc_extra) #use adonis2 to run the permanova
#rows in data must be in same order as rows in distance matrix 

#look at output 
summary(permanova_results_all_year_extra)
```

```
##        Df          SumOfSqs           R2               F        
##  Min.   : 7.0   Min.   :102.8   Min.   :0.3263   Min.   :11.21  
##  1st Qu.:22.5   1st Qu.:157.5   1st Qu.:0.5000   1st Qu.:11.21  
##  Median :38.0   Median :212.2   Median :0.6737   Median :11.21  
##  Mean   :30.0   Mean   :210.0   Mean   :0.6667   Mean   :11.21  
##  3rd Qu.:41.5   3rd Qu.:263.6   3rd Qu.:0.8369   3rd Qu.:11.21  
##  Max.   :45.0   Max.   :315.0   Max.   :1.0000   Max.   :11.21  
##                                                  NA's   :2      
##      Pr(>F)     
##  Min.   :0.001  
##  1st Qu.:0.001  
##  Median :0.001  
##  Mean   :0.001  
##  3rd Qu.:0.001  
##  Max.   :0.001  
##  NA's   :2
```

``` r
permanova_results_all_year_extra
```

```
## Permutation test for adonis under reduced model
## Permutation: free
## Number of permutations: 999
## 
## adonis2(formula = dist_matrix_all_year_extra ~ TimePd * elev_m * Lat, data = all_bioclim_flint_avgs_locs.pc_extra)
##          Df SumOfSqs      R2      F Pr(>F)    
## Model     7   212.22 0.67372 11.209  0.001 ***
## Residual 38   102.78 0.32628                  
## Total    45   315.00 1.00000                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
permanova_results_all_year_extra$`Pr(>F)`
```

```
## [1] 0.001    NA    NA
```

``` r
permanova_results_all_year_terms_extra <- adonis2(dist_matrix_all_year_extra ~ TimePd*elev_m*Lat, data = all_bioclim_flint_avgs_locs.pc_extra, by = "terms")
permanova_results_all_year_terms_extra
```

```
## Permutation test for adonis under reduced model
## Terms added sequentially (first to last)
## Permutation: free
## Number of permutations: 999
## 
## adonis2(formula = dist_matrix_all_year_extra ~ TimePd * elev_m * Lat, data = all_bioclim_flint_avgs_locs.pc_extra, by = "terms")
##                   Df SumOfSqs      R2       F Pr(>F)    
## TimePd             1   29.653 0.09414 10.9633  0.001 ***
## elev_m             1  107.941 0.34267 39.9083  0.001 ***
## Lat                1   59.420 0.18863 21.9690  0.001 ***
## TimePd:elev_m      1    3.492 0.01109  1.2912  0.258    
## TimePd:Lat         1    2.382 0.00756  0.8808  0.425    
## elev_m:Lat         1    9.193 0.02918  3.3987  0.022 *  
## TimePd:elev_m:Lat  1    0.140 0.00044  0.0517  0.998    
## Residual          38  102.779 0.32628                   
## Total             45  315.000 1.00000                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## LM on PCs (follow up on permanova) - Extra Historical 2023

For each PC fit a model with timepd and parent.pop/elev to test for climate change


``` r
lmer_results_all_year_extra <- all_bioclim_flint_avgs_locs.pc_extra %>%
  ungroup() %>% 
  select(TimePd, parent.pop, elev_m, Lat, Long, PC1:PC7) %>% 
  pivot_longer(starts_with("PC", ignore.case = FALSE), 
               names_to = "PC", values_to = "value") %>% 
  group_by(PC) %>% 
  nest(data=c(TimePd, parent.pop, elev_m, Lat, Long, value)) %>% 
  mutate(glm=map(data, ~ glm(value ~ TimePd*elev_m*Lat,
                               data=.x)),
         anova = map(glm, ~ broom.mixed::tidy(anova(.x))))

#lmer_results_all_year_extra
```


``` r
PC_anova_extra <- lmer_results_all_year_extra %>% select(-data, -glm) %>% unnest(anova) %>%
  select(PC, term, p.value) %>%
  filter(p.value < 0.05) %>%
  arrange(term, p.value)

PC_anova_extra
```

```
## # A tibble: 14 × 3
## # Groups:   PC [6]
##    PC    term        p.value
##    <chr> <chr>         <dbl>
##  1 PC1   Lat        8.73e-11
##  2 PC2   Lat        3.00e- 5
##  3 PC5   Lat        1.98e- 3
##  4 PC3   Lat        1.18e- 2
##  5 PC4   TimePd     3.57e- 7
##  6 PC1   TimePd     3.90e- 4
##  7 PC2   TimePd     9.60e- 4
##  8 PC7   TimePd     1.65e- 2
##  9 PC1   elev_m     3.61e-17
## 10 PC4   elev_m     4.91e- 5
## 11 PC3   elev_m     2.95e- 2
## 12 PC7   elev_m:Lat 1.36e- 3
## 13 PC5   elev_m:Lat 2.20e- 3
## 14 PC1   elev_m:Lat 7.26e- 3
```

``` r
lmer_results_all_year_extra %>% select(-data, -glm) %>% unnest(anova) %>% 
  filter(PC=="PC1"| PC=="PC4") #the two PCs with most sig p-values for timepd
```

```
## # A tibble: 16 × 8
## # Groups:   PC [2]
##    PC    term      df deviance df.residual residual.deviance statistic   p.value
##    <chr> <chr>  <int>    <dbl>       <int>             <dbl>     <dbl>     <dbl>
##  1 PC1   NULL      NA  NA               45            159.     NA      NA       
##  2 PC1   TimePd     1   6.80            44            153.     15.1     3.90e- 4
##  3 PC1   elev_m     1  95.8             43             56.9   213.      3.61e-17
##  4 PC1   Lat        1  35.3             42             21.6    78.6     8.73e-11
##  5 PC1   TimeP…     1   0.836           41             20.7     1.86    1.80e- 1
##  6 PC1   TimeP…     1   0.0395          40             20.7     0.0880  7.68e- 1
##  7 PC1   elev_…     1   3.62            39             17.1     8.05    7.26e- 3
##  8 PC1   TimeP…     1   0.0134          38             17.1     0.0298  8.64e- 1
##  9 PC4   NULL      NA  NA               45             26.8    NA      NA       
## 10 PC4   TimePd     1   9.64            44             17.1    37.8     3.57e- 7
## 11 PC4   elev_m     1   5.35            43             11.8    21.0     4.91e- 5
## 12 PC4   Lat        1   0.212           42             11.6     0.830   3.68e- 1
## 13 PC4   TimeP…     1   0.858           41             10.7     3.36    7.45e- 2
## 14 PC4   TimeP…     1   0.126           40             10.6     0.494   4.86e- 1
## 15 PC4   elev_…     1   0.877           39              9.71    3.44    7.14e- 2
## 16 PC4   TimeP…     1   0.0159          38              9.69    0.0625  8.04e- 1
```

``` r
#mod_test <- glm(PC4 ~ TimePd*elev_m*Lat, data=all_bioclim_flint_avgs_locs.pc_extra)
#summary(mod_test)
#anova(mod_test)
```


``` r
autoplot(all_bioclim_flint_avgs.pc_extra, data = bioclim_flint_all_year_avgs_extra,
         x=1, y=4,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-46-1.png)<!-- -->

``` r
all_bioclim_flint_avgs_locs.pc_avg_extra %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-46-2.png)<!-- -->

``` r
all_bioclim_flint_avgs_locs.pc_avg_extra %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_text_repel(aes(label = parent.pop)) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed")  +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-46-3.png)<!-- -->

``` r
#high elev climate seems to be shifting to be more similar to low elev 
```


``` r
home_sites_pca_extra <- all_bioclim_flint_avgs_locs.pc_avg_extra %>%  
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  #geom_point(size=2, alpha=0.7) +
  labs(x="PC1 (50.61%)", y="PC4 (8.5%)", color="Elevation (m)") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8) +
  annotate("text", x = -4, y = -5.1, label = "Warm \n No Snow") +
  annotate("text", x = 4, y = -5.1, label = "Cold \n Snow") +
  annotate("text", x = -5.4, y = -3.5, label = "High Temp \n Seasonality") +
  annotate("text", x = -5.4, y = 1.5, label = "Low Temp \n Seasonality") +
  coord_cartesian(ylim = c(-4.1, 2), xlim = c(-4,4), clip = "off") +
  theme_classic() +
  theme(text=element_text(size=28))
#ggsave("../output/Climate/Wtr_Year_PC1-PC4_ExtraHist.png", width = 7.4, height = 6, units = "in")

#str(home_sites_pca) #can add predicted WL2 point to the existing plot's data 
home_sites_pca_extra$data <- rbind(home_sites_pca_extra$data, 
  data.frame(
    parent.pop = "WL2_Garden",
    elev_m = 2020,
    TimePd = "2023",
    PC1 = WL2Grdn_predicted[, "PC1"],
    PC2 = WL2Grdn_predicted[, "PC2"],
    PC3 = WL2Grdn_predicted[, "PC3"],
    PC4 = WL2Grdn_predicted[, "PC4"],
    PC5 = WL2Grdn_predicted[, "PC5"],
    PC6 = WL2Grdn_predicted[, "PC6"],
    PC7 = WL2Grdn_predicted[, "PC7"],
    group = "new"
  )
)

home_sites_pca_extra + 
  geom_point(data=filter(home_sites_pca_extra$data, parent.pop == "WL2_Garden"), size=3, shape = 8, show_guide = FALSE)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-47-1.png)<!-- -->

``` r
#ggsave("../output/Climate/Wtr_Year_PC1-PC4_PlusGarden_ExtraHist.png", width = 7.4, height = 6, units = "in")
```

## For 2024 (Water Year Avgs)
### Prep


``` r
bioclim_2024_allyear_prep <- flint_all_year_wtr_yr %>% 
  filter(parent.pop != "UCD_Garden", parent.pop != "WL2_Garden") %>%  #remove garden sites 
  rename(tmin=tmn, tmax=tmx, year_cal=year, year=wtr_yr) %>% #rename columns to match what calc_biovars expects, also make sure it uses water year 
  filter(year != "1895", year !="2025") %>%  #remove years with less than 12 months of data
  arrange(parent.pop, year, month)

bioclim_2024_all_year <- tibble(bio1=NA, bio2=NA, bio4=NA, bio7=NA, bio8=NA, bio9=NA, bio12=NA, bio15=NA, bio18=NA, bio19=NA, year=2025) #blank tibble to bind calculations to
 
popids <- unique(bioclim_2024_allyear_prep$parent.pop) #list of pop ids for for loop 

pop_elev <- flint_all_year_wtr_yr %>% select(parent.pop:Long) %>% distinct()
```

### Calculation


``` r
for(i in popids) {
  A <- bioclim_2024_allyear_prep %>% filter(parent.pop==i) %>% calc_biovars() %>% mutate(parent.pop=i)
  #print(A)
  bioclim_2024_all_year <- bind_rows(bioclim_2024_all_year, A)
}
unique(bioclim_2024_all_year$parent.pop) #has all the populations in there!
```

```
##  [1] NA      "BH"    "CC"    "CP2"   "CP3"   "DPR"   "FR"    "IH"    "LV1"  
## [10] "LV3"   "LVTR1" "SC"    "SQ1"   "SQ2"   "SQ3"   "TM2"   "WL1"   "WL2"  
## [19] "WR"    "WV"    "YO11"  "YO4"   "YO7"   "YO8"
```

### SUBSET


``` r
bioclim_2024_all_year_final <- bioclim_2024_all_year %>% 
  select(parent.pop, year, ann_tmean=bio1, mean_diurnal_range=bio2, 
         temp_seasonality=bio4, temp_ann_range=bio7, tmean_wettest_quarter=bio8,
         tmean_driest_quarter=bio9, ann_ppt=bio12, ppt_seasonality=bio15,
         ppt_warmest_quarter=bio18, ppt_coldest_quarter=bio19) %>%
  filter(year!=2025)
head(bioclim_2024_all_year_final)
```

```
## # A tibble: 6 × 12
##   parent.pop  year ann_tmean mean_diurnal_range temp_seasonality temp_ann_range
##   <chr>      <dbl>     <dbl>              <dbl>            <dbl>          <dbl>
## 1 BH          1896      14.6               14.2             644.           33.2
## 2 BH          1897      14.6               13.7             688.           32.1
## 3 BH          1898      14.5               14.5             697.           35.7
## 4 BH          1899      14.7               15.7             668.           36.3
## 5 BH          1900      14.9               14.8             610.           34.5
## 6 BH          1901      15.2               15.1             650.           33.8
## # ℹ 6 more variables: tmean_wettest_quarter <dbl>, tmean_driest_quarter <dbl>,
## #   ann_ppt <dbl>, ppt_seasonality <dbl>, ppt_warmest_quarter <dbl>,
## #   ppt_coldest_quarter <dbl>
```

### Merge with pop info


``` r
pop_elev_bioclim_2024_all_year <- left_join(bioclim_2024_all_year_final, pop_elev) %>% 
  select(parent.pop, elevation.group:Long, year:ppt_coldest_quarter)
```

```
## Joining with `by = join_by(parent.pop)`
```

``` r
head(pop_elev_bioclim_2024_all_year)
```

```
## # A tibble: 6 × 16
##   parent.pop elevation.group elev_m   Lat  Long  year ann_tmean
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl>     <dbl>
## 1 BH         Low               511.  37.4 -120.  1896      14.6
## 2 BH         Low               511.  37.4 -120.  1897      14.6
## 3 BH         Low               511.  37.4 -120.  1898      14.5
## 4 BH         Low               511.  37.4 -120.  1899      14.7
## 5 BH         Low               511.  37.4 -120.  1900      14.9
## 6 BH         Low               511.  37.4 -120.  1901      15.2
## # ℹ 9 more variables: mean_diurnal_range <dbl>, temp_seasonality <dbl>,
## #   temp_ann_range <dbl>, tmean_wettest_quarter <dbl>,
## #   tmean_driest_quarter <dbl>, ann_ppt <dbl>, ppt_seasonality <dbl>,
## #   ppt_warmest_quarter <dbl>, ppt_coldest_quarter <dbl>
```

``` r
#write_csv(pop_elev_bioclim_2024_all_year, "../output/Climate/bioclim_all_year_UCD_pops_wtr_year_2024.csv")
```

### Calculation of recent (last 30 years) and historical climate (prior 30 years)


``` r
flint_all_year_recent_2024 <- flint_all_year_wtr_yr %>% 
  filter(parent.pop != "UCD_Garden", parent.pop != "WL2_Garden") %>%  #remove garden sites 
  filter(wtr_yr>1994 & wtr_yr<=2024) %>% 
  select(parent.pop:month, wtr_yr, cwd, pck, ppt, tmn, tmx)
head(flint_all_year_recent_2024)
```

```
## # A tibble: 6 × 13
##   parent.pop elevation.group elev_m   Lat  Long  year month wtr_yr   cwd   pck
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr>  <dbl> <dbl> <dbl>
## 1 BH         Low               511.  37.4 -120.  1994 nov     1995  40.6     0
## 2 BH         Low               511.  37.4 -120.  1994 dec     1995  29.0     0
## 3 BH         Low               511.  37.4 -120.  1995 jan     1995  27.9     0
## 4 BH         Low               511.  37.4 -120.  1995 feb     1995  43.9     0
## 5 BH         Low               511.  37.4 -120.  1995 mar     1995  41.1     0
## 6 BH         Low               511.  37.4 -120.  1995 apr     1995  38.9     0
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
tail(flint_all_year_recent_2024)
```

```
## # A tibble: 6 × 13
##   parent.pop elevation.group elev_m   Lat  Long  year month wtr_yr   cwd   pck
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr>  <dbl> <dbl> <dbl>
## 1 YO8        High             2591.  37.8 -119.  2024 may     2024  88.0  234.
## 2 YO8        High             2591.  37.8 -119.  2024 jun     2024 128.     0 
## 3 YO8        High             2591.  37.8 -119.  2024 jul     2024 129.     0 
## 4 YO8        High             2591.  37.8 -119.  2024 aug     2024 122.     0 
## 5 YO8        High             2591.  37.8 -119.  2024 sep     2024 104.     0 
## 6 YO8        High             2591.  37.8 -119.  2024 oct     2024  78.3    0 
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
flint_all_year_historical_2024 <- flint_all_year_wtr_yr %>% 
  filter(parent.pop != "UCD_Garden", parent.pop != "WL2_Garden") %>%  #remove garden sites 
  filter(wtr_yr<=1994 & wtr_yr>1964) %>% 
  select(parent.pop:month, wtr_yr, cwd, pck, ppt, tmn, tmx)
head(flint_all_year_historical_2024, 13)
```

```
## # A tibble: 13 × 13
##    parent.pop elevation.group elev_m   Lat  Long  year month wtr_yr   cwd   pck
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr>  <dbl> <dbl> <dbl>
##  1 BH         Low               511.  37.4 -120.  1964 nov     1965  40.2     0
##  2 BH         Low               511.  37.4 -120.  1964 dec     1965  27.9     0
##  3 BH         Low               511.  37.4 -120.  1965 jan     1965  27.4     0
##  4 BH         Low               511.  37.4 -120.  1965 feb     1965  41.0     0
##  5 BH         Low               511.  37.4 -120.  1965 mar     1965  57.2     0
##  6 BH         Low               511.  37.4 -120.  1965 apr     1965  39.2     0
##  7 BH         Low               511.  37.4 -120.  1965 may     1965  70.8     0
##  8 BH         Low               511.  37.4 -120.  1965 jun     1965  91.2     0
##  9 BH         Low               511.  37.4 -120.  1965 jul     1965 119.      0
## 10 BH         Low               511.  37.4 -120.  1965 aug     1965  99.9     0
## 11 BH         Low               511.  37.4 -120.  1965 sep     1965 119.      0
## 12 BH         Low               511.  37.4 -120.  1965 oct     1965  90.1     0
## 13 BH         Low               511.  37.4 -120.  1965 nov     1966  45.2     0
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
tail(flint_all_year_historical_2024, 13)
```

```
## # A tibble: 13 × 13
##    parent.pop elevation.group elev_m   Lat  Long  year month wtr_yr   cwd   pck
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr>  <dbl> <dbl> <dbl>
##  1 YO8        High             2591.  37.8 -119.  1993 oct     1993  73.0   0  
##  2 YO8        High             2591.  37.8 -119.  1993 nov     1994  36.2   0  
##  3 YO8        High             2591.  37.8 -119.  1993 dec     1994  26.2  80.9
##  4 YO8        High             2591.  37.8 -119.  1994 jan     1994  24.9 127. 
##  5 YO8        High             2591.  37.8 -119.  1994 feb     1994  29.2 323. 
##  6 YO8        High             2591.  37.8 -119.  1994 mar     1994  52.6 272. 
##  7 YO8        High             2591.  37.8 -119.  1994 apr     1994  71.8 146. 
##  8 YO8        High             2591.  37.8 -119.  1994 may     1994  90.3   0  
##  9 YO8        High             2591.  37.8 -119.  1994 jun     1994 123.    0  
## 10 YO8        High             2591.  37.8 -119.  1994 jul     1994 135.    0  
## 11 YO8        High             2591.  37.8 -119.  1994 aug     1994 127.    0  
## 12 YO8        High             2591.  37.8 -119.  1994 sep     1994  99.8   0  
## 13 YO8        High             2591.  37.8 -119.  1994 oct     1994  70.2   0  
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
bioclim_2024_all_year_recent <- pop_elev_bioclim_2024_all_year %>% filter(year>1994 & year<=2024) #year here means water year (see above code where bioclim vars were calculated)
#head(bioclim_2024_all_year_recent)
#tail(bioclim_2024_all_year_recent)

bioclim_2024_all_year_historical <- pop_elev_bioclim_2024_all_year %>% filter(year<=1994 & year>1964)
#head(bioclim_2024_all_year_historical, 13)
#tail(bioclim_2024_all_year_historical, 13)
```

### Yearly averages for Flint

``` r
flint_all_year_recent_yravgs_2024 <- flint_all_year_recent_2024 %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long, wtr_yr) %>% 
  summarise_at(c("cwd",  "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  rename(year=wtr_yr)
#flint_all_year_recent_yravgs_2024

flint_all_year_historical_yravgs_2024 <- flint_all_year_historical_2024 %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long, wtr_yr) %>% 
  summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  rename(year=wtr_yr)
#flint_all_year_historical_yravgs_2024
```

### Merge with bioclim

``` r
bioclim_2024_flint_yrly_avgs_recent <- full_join(flint_all_year_recent_yravgs_2024, bioclim_2024_all_year_recent) 
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, Lat, Long,
## year)`
```

``` r
#write_csv(bioclim_2024_flint_yrly_avgs_recent, "../output/Climate/fullyear_wtr_year_avgs_Recent_2024.csv")
bioclim_2024_flint_yrly_avgs_historical <- full_join(flint_all_year_historical_yravgs_2024, bioclim_2024_all_year_historical) 
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, Lat, Long,
## year)`
```

``` r
#write_csv(bioclim_2024_flint_yrly_avgs_historical, "../output/Climate/fullyear_wtr_year_avgs_Historical_2024.csv")
```

### Calculation of extra historical climate (prior 60 years)

``` r
flint_all_year_historical_extra_2024 <- flint_all_year_wtr_yr %>% 
  filter(parent.pop != "UCD_Garden", parent.pop != "WL2_Garden") %>%  #remove garden sites 
  filter(wtr_yr<=1994 & wtr_yr>1934) %>% 
  select(parent.pop:month, wtr_yr, cwd, pck, ppt, tmn, tmx)
head(flint_all_year_historical_extra_2024, 13)
```

```
## # A tibble: 13 × 13
##    parent.pop elevation.group elev_m   Lat  Long  year month wtr_yr   cwd   pck
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr>  <dbl> <dbl> <dbl>
##  1 BH         Low               511.  37.4 -120.  1934 nov     1935  45.9     0
##  2 BH         Low               511.  37.4 -120.  1934 dec     1935  28.9     0
##  3 BH         Low               511.  37.4 -120.  1935 jan     1935  25.7     0
##  4 BH         Low               511.  37.4 -120.  1935 feb     1935  41.5     0
##  5 BH         Low               511.  37.4 -120.  1935 mar     1935  46.6     0
##  6 BH         Low               511.  37.4 -120.  1935 apr     1935  39.4     0
##  7 BH         Low               511.  37.4 -120.  1935 may     1935  72.4     0
##  8 BH         Low               511.  37.4 -120.  1935 jun     1935  97.7     0
##  9 BH         Low               511.  37.4 -120.  1935 jul     1935 120.      0
## 10 BH         Low               511.  37.4 -120.  1935 aug     1935 133.      0
## 11 BH         Low               511.  37.4 -120.  1935 sep     1935 121.      0
## 12 BH         Low               511.  37.4 -120.  1935 oct     1935  83.4     0
## 13 BH         Low               511.  37.4 -120.  1935 nov     1936  42.3     0
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
tail(flint_all_year_historical_extra_2024, 13)
```

```
## # A tibble: 13 × 13
##    parent.pop elevation.group elev_m   Lat  Long  year month wtr_yr   cwd   pck
##    <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr>  <dbl> <dbl> <dbl>
##  1 YO8        High             2591.  37.8 -119.  1993 oct     1993  73.0   0  
##  2 YO8        High             2591.  37.8 -119.  1993 nov     1994  36.2   0  
##  3 YO8        High             2591.  37.8 -119.  1993 dec     1994  26.2  80.9
##  4 YO8        High             2591.  37.8 -119.  1994 jan     1994  24.9 127. 
##  5 YO8        High             2591.  37.8 -119.  1994 feb     1994  29.2 323. 
##  6 YO8        High             2591.  37.8 -119.  1994 mar     1994  52.6 272. 
##  7 YO8        High             2591.  37.8 -119.  1994 apr     1994  71.8 146. 
##  8 YO8        High             2591.  37.8 -119.  1994 may     1994  90.3   0  
##  9 YO8        High             2591.  37.8 -119.  1994 jun     1994 123.    0  
## 10 YO8        High             2591.  37.8 -119.  1994 jul     1994 135.    0  
## 11 YO8        High             2591.  37.8 -119.  1994 aug     1994 127.    0  
## 12 YO8        High             2591.  37.8 -119.  1994 sep     1994  99.8   0  
## 13 YO8        High             2591.  37.8 -119.  1994 oct     1994  70.2   0  
## # ℹ 3 more variables: ppt <dbl>, tmn <dbl>, tmx <dbl>
```

``` r
bioclim_all_year_historical_extra_2024 <- pop_elev_bioclim_2024_all_year %>% filter(year<=1994 & year>1934)
#head(bioclim_all_year_historical_extra_2024, 13)
#tail(bioclim_all_year_historical_extra_2024, 13)
```

## Avg across years and months (Flint + bioclim) - 2024

### Calculate avgs for flint


``` r
flint_all_year_recent_avgs_2024 <- flint_all_year_recent_2024 %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("cwd",  "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Recent") 
#flint_all_year_recent_avgs_2024

flint_all_year_historical_avgs_2024 <- flint_all_year_historical_2024 %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Historical")
#flint_all_year_historical_avgs_2024

flint_all_year_avgs_2024 <- bind_rows(flint_all_year_recent_avgs_2024, flint_all_year_historical_avgs_2024) #combine into 1 dataframe 
head(flint_all_year_avgs_2024)
```

```
## # A tibble: 6 × 11
## # Groups:   parent.pop, elevation.group, elev_m, Lat [6]
##   parent.pop elevation.group elev_m   Lat  Long   cwd    pck   ppt    tmn   tmx
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl>
## 1 BH         Low               511.  37.4 -120.  75.6   0     48.9  9.02   23.6
## 2 CC         Low               313   39.6 -121.  59.8   0     85.2 10.1    23.3
## 3 CP2        High             2244.  38.7 -120.  62.9 221.   108.   1.23   13.4
## 4 CP3        High             2266.  38.7 -120.  46.2 240.   104.   0.571  12.6
## 5 DPR        Mid              1019.  39.2 -121.  27.4   7.58 122.   7.96   20.2
## 6 FR         Mid               787   40.0 -121.  75.5  14.2   85.9  5.77   20.1
## # ℹ 1 more variable: TimePd <chr>
```

``` r
tail(flint_all_year_avgs_2024)
```

```
## # A tibble: 6 × 11
## # Groups:   parent.pop, elevation.group, elev_m, Lat [6]
##   parent.pop elevation.group elev_m   Lat  Long   cwd   pck   ppt   tmn   tmx
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 WR         Mid              1158   39.3 -121.  42.4  49.4 133.   5.27 18.6 
## 2 WV         Mid               749.  40.7 -123.  43.6  36.1  84.1  4.01 18.9 
## 3 YO11       High             2872.  37.9 -119.  54.4 202.   66.8 -4.64  9.73
## 4 YO4        High             2158.  37.8 -120.  53.2 151.   94.6  1.13 13.6 
## 5 YO7        High             2470.  37.8 -120.  50.5 267.  101.  -1.90 11.7 
## 6 YO8        High             2591.  37.8 -119.  64.9 281.  100.  -2.60 11.3 
## # ℹ 1 more variable: TimePd <chr>
```

``` r
#write_csv(flint_all_year_avgs_2024, "../output/Climate/fullyear_FlintAvgs_wtr_year_2024.csv")
```

### Calculate avgs for bioclim 


``` r
bioclim_all_year_recent_avgs_2024 <- bioclim_2024_all_year_recent %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("ann_tmean", "mean_diurnal_range", "temp_seasonality", "temp_ann_range",
                 "tmean_wettest_quarter", "tmean_driest_quarter", "ann_ppt",
                 "ppt_seasonality","ppt_warmest_quarter", "ppt_coldest_quarter"),
               c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Recent") 
#bioclim_all_year_recent_avgs_2024

bioclim_all_year_historical_avgs_2024 <- bioclim_2024_all_year_historical %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("ann_tmean", "mean_diurnal_range", "temp_seasonality", "temp_ann_range",
                 "tmean_wettest_quarter", "tmean_driest_quarter", "ann_ppt",
                 "ppt_seasonality","ppt_warmest_quarter", "ppt_coldest_quarter"),
               c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Historical") 
#bioclim_all_year_historical_avgs_2024

bioclim_all_year_avgs_2024 <- bind_rows(bioclim_all_year_recent_avgs_2024, bioclim_all_year_historical_avgs_2024) #combine into 1 dataframe 
head(bioclim_all_year_avgs_2024)
```

```
## # A tibble: 6 × 16
## # Groups:   parent.pop, elevation.group, elev_m, Lat [6]
##   parent.pop elevation.group elev_m   Lat  Long ann_tmean mean_diurnal_range
##   <chr>      <chr>            <dbl> <dbl> <dbl>     <dbl>              <dbl>
## 1 BH         Low               511.  37.4 -120.     16.3                14.5
## 2 CC         Low               313   39.6 -121.     16.7                13.2
## 3 CP2        High             2244.  38.7 -120.      7.30               12.1
## 4 CP3        High             2266.  38.7 -120.      6.59               12.0
## 5 DPR        Mid              1019.  39.2 -121.     14.1                12.3
## 6 FR         Mid               787   40.0 -121.     12.9                14.3
## # ℹ 9 more variables: temp_seasonality <dbl>, temp_ann_range <dbl>,
## #   tmean_wettest_quarter <dbl>, tmean_driest_quarter <dbl>, ann_ppt <dbl>,
## #   ppt_seasonality <dbl>, ppt_warmest_quarter <dbl>,
## #   ppt_coldest_quarter <dbl>, TimePd <chr>
```

``` r
tail(bioclim_all_year_avgs_2024)
```

```
## # A tibble: 6 × 16
## # Groups:   parent.pop, elevation.group, elev_m, Lat [6]
##   parent.pop elevation.group elev_m   Lat  Long ann_tmean mean_diurnal_range
##   <chr>      <chr>            <dbl> <dbl> <dbl>     <dbl>              <dbl>
## 1 WR         Mid              1158   39.3 -121.     11.9                13.3
## 2 WV         Mid               749.  40.7 -123.     11.5                14.9
## 3 YO11       High             2872.  37.9 -119.      2.55               14.4
## 4 YO4        High             2158.  37.8 -120.      7.39               12.5
## 5 YO7        High             2470.  37.8 -120.      4.89               13.6
## 6 YO8        High             2591.  37.8 -119.      4.36               13.9
## # ℹ 9 more variables: temp_seasonality <dbl>, temp_ann_range <dbl>,
## #   tmean_wettest_quarter <dbl>, tmean_driest_quarter <dbl>, ann_ppt <dbl>,
## #   ppt_seasonality <dbl>, ppt_warmest_quarter <dbl>,
## #   ppt_coldest_quarter <dbl>, TimePd <chr>
```

``` r
#write_csv(bioclim_all_year_avgs_2024, "../output/Climate/fullyear_BioClimAvgs_wtr_year_2024.csv")
```

Merge with flint


``` r
bioclim_flint_all_year_avgs_2024 <- full_join(flint_all_year_avgs_2024, bioclim_all_year_avgs_2024) %>% 
  select(TimePd, parent.pop:ppt_coldest_quarter)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, Lat, Long,
## TimePd)`
```

``` r
head(bioclim_flint_all_year_avgs_2024)
```

```
## # A tibble: 6 × 21
## # Groups:   parent.pop, elevation.group, elev_m, Lat [6]
##   TimePd parent.pop elevation.group elev_m   Lat  Long   cwd    pck   ppt    tmn
##   <chr>  <chr>      <chr>            <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl>
## 1 Recent BH         Low               511.  37.4 -120.  75.6   0     48.9  9.02 
## 2 Recent CC         Low               313   39.6 -121.  59.8   0     85.2 10.1  
## 3 Recent CP2        High             2244.  38.7 -120.  62.9 221.   108.   1.23 
## 4 Recent CP3        High             2266.  38.7 -120.  46.2 240.   104.   0.571
## 5 Recent DPR        Mid              1019.  39.2 -121.  27.4   7.58 122.   7.96 
## 6 Recent FR         Mid               787   40.0 -121.  75.5  14.2   85.9  5.77 
## # ℹ 11 more variables: tmx <dbl>, ann_tmean <dbl>, mean_diurnal_range <dbl>,
## #   temp_seasonality <dbl>, temp_ann_range <dbl>, tmean_wettest_quarter <dbl>,
## #   tmean_driest_quarter <dbl>, ann_ppt <dbl>, ppt_seasonality <dbl>,
## #   ppt_warmest_quarter <dbl>, ppt_coldest_quarter <dbl>
```

### Correlations - Recent + Historical


``` r
#normalize the data
climate_normalized_bioclim_flint_all_year_avgs_2024 <- bioclim_flint_all_year_avgs_2024 %>% ungroup() %>% 
  select(cwd:ppt_coldest_quarter) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_bioclim_flint_all_year_avgs_2024)
```

```
##             cwd        pck        ppt        tmn        tmx  ann_tmean
## [1,]  1.7530343 -0.9376953 -1.9208257  1.4235410  1.6136028  1.5302377
## [2,]  0.4007021 -0.9376953 -0.5522505  1.6810375  1.5519896  1.6229687
## [3,]  0.6665473  0.4478285  0.3258408 -0.3939380 -0.6009172 -0.5038532
## [4,] -0.7604450  0.5647349  0.1528230 -0.5481260 -0.7661768 -0.6646435
## [5,] -2.3677959 -0.8902448  0.8493099  1.1755111  0.8916697  1.0340948
## [6,]  1.7412272 -0.8489181 -0.5228397  0.6647003  0.8540910  0.7669272
##      mean_diurnal_range temp_seasonality temp_ann_range tmean_wettest_quarter
## [1,]         1.36072367        1.2977953      1.5948853             1.4746119
## [2,]        -0.06065264       -0.2350646     -0.3171491             1.6685715
## [3,]        -1.10482511        0.7400804     -0.7182998            -0.5176046
## [4,]        -1.20732615        0.9010992     -0.6955270            -0.6972659
## [5,]        -0.95568608        1.0056397     -0.6479378             0.9787578
## [6,]         1.11013709        1.9237913      1.8220288             0.6328632
##      tmean_driest_quarter    ann_ppt ppt_seasonality ppt_warmest_quarter
## [1,]            1.3895902 -1.9208257       1.8118887        -1.805402797
## [2,]            1.5346092 -0.5522505       1.4584205        -1.029985366
## [3,]           -0.5289281  0.3258408      -0.1916034         0.083289680
## [4,]           -0.6850124  0.1528230      -0.3682256         0.004863659
## [5,]            1.0328742  0.8493099       1.0010839         0.208223403
## [6,]            0.6937778 -0.5228397       1.2723301        -0.704715144
##      ppt_coldest_quarter
## [1,]          -1.9682814
## [2,]          -0.2783720
## [3,]           0.4347473
## [4,]           0.2034577
## [5,]           1.3515110
## [6,]          -0.3090945
```

``` r
dim(climate_normalized_bioclim_flint_all_year_avgs_2024)
```

```
## [1] 46 15
```

``` r
cor.norm_2024 = cor(climate_normalized_bioclim_flint_all_year_avgs_2024) #test correlations among the traits
cor.sig_2024 <- cor.mtest(climate_normalized_bioclim_flint_all_year_avgs_2024, method = "pearson")

corrplot(cor.norm_2024, type="upper",
         tl.srt = 45, p.mat = cor.sig_2024$p, 
         sig.level = 0.05, insig="blank")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-59-1.png)<!-- -->

``` r
#tmn, tmx, tmean_wettest_quarter, tmean_driest_quarter and ann_tmean all highly correlated (97-99%) - only keep ann_tmean 
#temp_ann_range and mean_diurnal range 92% correlated - keep temp_ann_range 
#ann_ppt and ppt 100% correlated (ppt = avg across monts, ann_ppt = avg of the total ppt in a year) - only keep ann_ppt 
#ppt_warmest and coldest quarter highly correlated with ann_ppt and ppt - take both out 
```

Recent

``` r
climate_normalized_bioclim_flint_all_year_avgs_recent_2024 <- bioclim_flint_all_year_avgs_2024 %>% 
  ungroup() %>% 
  filter(TimePd=="Recent") %>% 
  select(tmn:tmean_driest_quarter, cwd, pck, ppt, ann_ppt:ppt_coldest_quarter) %>% 
  scale() #normalize the data so they're all on the same scale

cor.norm_recent_2024 = cor(climate_normalized_bioclim_flint_all_year_avgs_recent_2024) #test correlations among the traits
cor.sig_recent_2024 <- cor.mtest(climate_normalized_bioclim_flint_all_year_avgs_recent_2024, method = "pearson")

corrplot(cor.norm_recent_2024, type="upper",
         tl.srt = 45, p.mat = cor.sig_recent_2024$p, 
         sig.level = 0.05, insig="blank")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-60-1.png)<!-- -->

``` r
#800 x 734
```

Historical

``` r
climate_normalized_bioclim_flint_all_year_avgs_historic_2024 <- bioclim_flint_all_year_avgs_2024 %>% 
  ungroup() %>% 
  filter(TimePd=="Historical") %>% 
  select(tmn:tmean_driest_quarter, cwd, pck, ppt, ann_ppt:ppt_coldest_quarter) %>% 
  scale() #normalize the data so they're all on the same scale

cor.norm_historic_2024 = cor(climate_normalized_bioclim_flint_all_year_avgs_historic_2024) #test correlations among the traits
cor.sig_historic_2024 <- cor.mtest(climate_normalized_bioclim_flint_all_year_avgs_historic_2024, method = "pearson")

corrplot(cor.norm_historic_2024, type="upper",
         tl.srt = 45, p.mat = cor.sig_historic_2024$p, 
         sig.level = 0.05, insig="blank")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-61-1.png)<!-- -->

``` r
#800 x 734
```

### PCA - Recent + Historical


``` r
all_bioclim_flint_avgs.pc_2024 = prcomp(bioclim_flint_all_year_avgs_2024[c(7:8, 12, 14:15, 18:19)], scale = TRUE, center = TRUE)
str(all_bioclim_flint_avgs.pc_2024)
```

```
## List of 5
##  $ sdev    : num [1:7] 1.871 1.081 1.054 0.791 0.605 ...
##  $ rotation: num [1:7, 1:7] -0.282 0.484 -0.444 -0.306 -0.202 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:7] "cwd" "pck" "ann_tmean" "temp_seasonality" ...
##   .. ..$ : chr [1:7] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:7] 55.1 149.85 9.53 673.45 32.24 ...
##   ..- attr(*, "names")= chr [1:7] "cwd" "pck" "ann_tmean" "temp_seasonality" ...
##  $ scale   : Named num [1:7] 11.7 159.81 4.42 20.71 1.14 ...
##   ..- attr(*, "names")= chr [1:7] "cwd" "pck" "ann_tmean" "temp_seasonality" ...
##  $ x       : num [1:46, 1:7] -3.919 -1.991 0.39 0.866 -0.448 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:7] "PC1" "PC2" "PC3" "PC4" ...
##  - attr(*, "class")= chr "prcomp"
```

plot % Variance Explained


``` r
summary(all_bioclim_flint_avgs.pc_2024)
```

```
## Importance of components:
##                          PC1    PC2    PC3     PC4     PC5     PC6     PC7
## Standard deviation     1.871 1.0810 1.0544 0.79094 0.60525 0.42752 0.21254
## Proportion of Variance 0.500 0.1669 0.1588 0.08937 0.05233 0.02611 0.00645
## Cumulative Proportion  0.500 0.6669 0.8257 0.91510 0.96744 0.99355 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:7,2,pad="0")),
       percent_var=all_bioclim_flint_avgs.pc_2024$sdev[1:7]^2/sum(all_bioclim_flint_avgs.pc_2024$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-63-1.png)<!-- -->

Combine PCs with metadata


``` r
all_bioclim_flint_avgs.pc.dat_2024 = data.frame(all_bioclim_flint_avgs.pc_2024$x)

all_bioclim_flint_avgs_locs.pc_2024 = cbind(bioclim_flint_all_year_avgs_2024, all_bioclim_flint_avgs.pc.dat_2024)

all_bioclim_flint_avgs_loadings_2024 = data.frame(varnames=rownames(all_bioclim_flint_avgs.pc_2024$rotation), all_bioclim_flint_avgs.pc_2024$rotation)
all_bioclim_flint_avgs_loadings_2024 #very similar to 2023 
```

```
##                          varnames        PC1        PC2           PC3
## cwd                           cwd -0.2824397 -0.2680298 -0.6755504527
## pck                           pck  0.4843667 -0.2146801 -0.0966977165
## ann_tmean               ann_tmean -0.4436064  0.1843507  0.2144450462
## temp_seasonality temp_seasonality -0.3056253 -0.2146457  0.5409147776
## temp_ann_range     temp_ann_range -0.2018255 -0.7795682  0.2284072926
## ann_ppt                   ann_ppt  0.4283933  0.1401187  0.3788605533
## ppt_seasonality   ppt_seasonality -0.4137257  0.4179170 -0.0006729689
##                          PC4        PC5        PC6        PC7
## cwd              -0.27286384  0.3744426  0.4133618 -0.0805577
## pck              -0.26725331  0.3049466 -0.2862354  0.6808843
## ann_tmean         0.48283079  0.3673681  0.2575700  0.5374145
## temp_seasonality -0.65296609 -0.1690917  0.2909449  0.1683033
## temp_ann_range    0.26440135  0.2368869 -0.3531347 -0.2205502
## ann_ppt          -0.05580844  0.6368383  0.3146415 -0.3816198
## ppt_seasonality  -0.34872344  0.3762586 -0.6103333 -0.1359804
```


``` r
autoplot(all_bioclim_flint_avgs.pc_2024, data = bioclim_flint_all_year_avgs_2024,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-65-1.png)<!-- -->


``` r
autoplot(all_bioclim_flint_avgs.pc_2024, data = bioclim_flint_all_year_avgs_2024,
         x=1, y=4,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-66-1.png)<!-- -->


``` r
all_bioclim_flint_avgs_locs.pc_2024_avg <- all_bioclim_flint_avgs_locs.pc_2024 %>%
  group_by(parent.pop, elev_m, TimePd) %>%
  summarise(across(.cols=starts_with("PC", ignore.case = FALSE), .fns = mean)) %>%
  ungroup()
```

```
## `summarise()` has grouped output by 'parent.pop', 'elev_m'. You can override
## using the `.groups` argument.
```


``` r
all_bioclim_flint_avgs_locs.pc_2024_avg %>% 
  ggplot(aes(x=PC1, y=PC2, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point() +
  facet_wrap(~TimePd) +
  coord_fixed(ratio = 1.5)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-68-1.png)<!-- -->

## Permanova - 2024
See Jenny's github: https://github.com/jrgremer/Seedbanks_drought_deluge  

``` r
all_bioclim_flint_avgs_locs.pc_dist_2024 <- all_bioclim_flint_avgs_locs.pc_2024 %>% ungroup() %>% select(PC1:PC7)
dist_matrix_all_year_2024 <- dist(all_bioclim_flint_avgs_locs.pc_dist_2024, method = "euclidian") #use a distance function to calculate euclidian distance in PCA space
permanova_results_all_year_2024 <- adonis2(dist_matrix_all_year_2024 ~ TimePd*elev_m*Lat, data = all_bioclim_flint_avgs_locs.pc_2024) #use adonis2 to run the permanova
#rows in data must be in same order as rows in distance matrix 

#look at output 
summary(permanova_results_all_year_2024)
```

```
##        Df          SumOfSqs           R2               F        
##  Min.   : 7.0   Min.   :104.7   Min.   :0.3323   Min.   :10.91  
##  1st Qu.:22.5   1st Qu.:157.5   1st Qu.:0.5000   1st Qu.:10.91  
##  Median :38.0   Median :210.3   Median :0.6677   Median :10.91  
##  Mean   :30.0   Mean   :210.0   Mean   :0.6667   Mean   :10.91  
##  3rd Qu.:41.5   3rd Qu.:262.7   3rd Qu.:0.8339   3rd Qu.:10.91  
##  Max.   :45.0   Max.   :315.0   Max.   :1.0000   Max.   :10.91  
##                                                  NA's   :2      
##      Pr(>F)     
##  Min.   :0.001  
##  1st Qu.:0.001  
##  Median :0.001  
##  Mean   :0.001  
##  3rd Qu.:0.001  
##  Max.   :0.001  
##  NA's   :2
```

``` r
permanova_results_all_year_2024
```

```
## Permutation test for adonis under reduced model
## Permutation: free
## Number of permutations: 999
## 
## adonis2(formula = dist_matrix_all_year_2024 ~ TimePd * elev_m * Lat, data = all_bioclim_flint_avgs_locs.pc_2024)
##          Df SumOfSqs      R2      F Pr(>F)    
## Model     7   210.33 0.66771 10.908  0.001 ***
## Residual 38   104.67 0.33229                  
## Total    45   315.00 1.00000                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
permanova_results_all_year_2024$`Pr(>F)`
```

```
## [1] 0.001    NA    NA
```

``` r
permanova_results_all_year_terms_2024 <- adonis2(dist_matrix_all_year_2024 ~ TimePd*elev_m*Lat, data = all_bioclim_flint_avgs_locs.pc_2024, by = "terms")
permanova_results_all_year_terms_2024 #very similar to 2023
```

```
## Permutation test for adonis under reduced model
## Terms added sequentially (first to last)
## Permutation: free
## Number of permutations: 999
## 
## adonis2(formula = dist_matrix_all_year_2024 ~ TimePd * elev_m * Lat, data = all_bioclim_flint_avgs_locs.pc_2024, by = "terms")
##                   Df SumOfSqs      R2       F Pr(>F)    
## TimePd             1   32.192 0.10220 11.6872  0.001 ***
## elev_m             1  106.820 0.33911 38.7801  0.001 ***
## Lat                1   56.073 0.17801 20.3568  0.001 ***
## TimePd:elev_m      1    2.214 0.00703  0.8037  0.506    
## TimePd:Lat         1    3.659 0.01162  1.3283  0.246    
## elev_m:Lat         1    9.182 0.02915  3.3335  0.026 *  
## TimePd:elev_m:Lat  1    0.189 0.00060  0.0685  0.999    
## Residual          38  104.671 0.33229                   
## Total             45  315.000 1.00000                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## LM on PCs (follow up on permanova) - 2024

For each PC fit a model with timepd and parent.pop/elev to test for climate change


``` r
lmer_results_all_year_2024 <- all_bioclim_flint_avgs_locs.pc_2024 %>%
  ungroup() %>% 
  select(TimePd, parent.pop, elev_m, Lat, Long, PC1:PC7) %>% 
  pivot_longer(starts_with("PC", ignore.case = FALSE), 
               names_to = "PC", values_to = "value") %>% 
  group_by(PC) %>% 
  nest(data=c(TimePd, parent.pop, elev_m, Lat, Long, value)) %>% 
  mutate(glm=map(data, ~ glm(value ~ TimePd*elev_m*Lat,
                               data=.x)),
         anova = map(glm, ~ broom.mixed::tidy(anova(.x))))

#lmer_results_all_year_2024
```


``` r
PC_anova_2024 <- lmer_results_all_year_2024 %>% select(-data, -glm) %>% unnest(anova) %>%
  select(PC, term, p.value) %>%
  filter(p.value < 0.05) %>%
  arrange(term, p.value)

PC_anova_2024
```

```
## # A tibble: 14 × 3
## # Groups:   PC [6]
##    PC    term        p.value
##    <chr> <chr>         <dbl>
##  1 PC1   Lat        1.15e- 9
##  2 PC3   Lat        6.93e- 6
##  3 PC5   Lat        3.42e- 3
##  4 PC2   Lat        1.80e- 2
##  5 PC4   TimePd     1.04e-10
##  6 PC1   TimePd     2.81e- 5
##  7 PC5   TimePd     2.75e- 2
##  8 PC2   TimePd     2.85e- 2
##  9 PC1   elev_m     5.42e-17
## 10 PC4   elev_m     2.48e- 7
## 11 PC3   elev_m     2.13e- 2
## 12 PC5   elev_m:Lat 1.26e- 3
## 13 PC7   elev_m:Lat 1.60e- 3
## 14 PC1   elev_m:Lat 9.51e- 3
```

``` r
lmer_results_all_year_2024 %>% select(-data, -glm) %>% unnest(anova) %>% 
  filter(PC=="PC1"| PC=="PC4") #the two PCs with most sig p-values for timepd
```

```
## # A tibble: 16 × 8
## # Groups:   PC [2]
##    PC    term      df deviance df.residual residual.deviance statistic   p.value
##    <chr> <chr>  <int>    <dbl>       <int>             <dbl>     <dbl>     <dbl>
##  1 PC1   NULL      NA NA                45            157.    NA       NA       
##  2 PC1   TimePd     1  1.04e+1          44            147.     2.26e+1  2.81e- 5
##  3 PC1   elev_m     1  9.54e+1          43             51.7    2.08e+2  5.42e-17
##  4 PC1   Lat        1  2.93e+1          42             22.3    6.39e+1  1.15e- 9
##  5 PC1   TimeP…     1  8.67e-1          41             21.5    1.89e+0  1.77e- 1
##  6 PC1   TimeP…     1  5.90e-1          40             20.9    1.29e+0  2.64e- 1
##  7 PC1   elev_…     1  3.42e+0          39             17.4    7.46e+0  9.51e- 3
##  8 PC1   TimeP…     1  7.16e-3          38             17.4    1.56e-2  9.01e- 1
##  9 PC4   NULL      NA NA                45             28.2   NA       NA       
## 10 PC4   TimePd     1  1.32e+1          44             14.9    7.75e+1  1.04e-10
## 11 PC4   elev_m     1  6.69e+0          43              8.23   3.92e+1  2.48e- 7
## 12 PC4   Lat        1  5.75e-1          42              7.65   3.37e+0  7.43e- 2
## 13 PC4   TimeP…     1  4.47e-1          41              7.21   2.62e+0  1.14e- 1
## 14 PC4   TimeP…     1  1.49e-1          40              7.06   8.74e-1  3.56e- 1
## 15 PC4   elev_…     1  5.70e-1          39              6.49   3.34e+0  7.55e- 2
## 16 PC4   TimeP…     1  1.14e-4          38              6.49   6.65e-4  9.80e- 1
```

``` r
#mod_test <- glm(PC4 ~ TimePd*elev_m*Lat, data=all_bioclim_flint_avgs_locs.pc_2024)
#summary(mod_test)
#anova(mod_test)
```


``` r
autoplot(all_bioclim_flint_avgs.pc_2024, data = bioclim_flint_all_year_avgs_2024,
         x=1, y=4,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-72-1.png)<!-- -->

``` r
all_bioclim_flint_avgs_locs.pc_2024_avg %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-72-2.png)<!-- -->

``` r
all_bioclim_flint_avgs_locs.pc_2024_avg %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_text_repel(aes(label = parent.pop)) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed")  +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-72-3.png)<!-- -->

``` r
#high elev climate seems to be shifting to be more similar to low elev 
```


``` r
home_sites_pca_2024 <- all_bioclim_flint_avgs_locs.pc_2024_avg %>%  
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  #geom_point(size=2, alpha=0.7) +
  labs(x="PC1 (50%)", y="PC4 (8.94%)", color="Elevation (m)") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8) +
  annotate("text", x = -4, y = -5.1, label = "Warm \n No Snow") +
  annotate("text", x = 4, y = -5.1, label = "Cold \n Snow") +
  annotate("text", x = -5.4, y = -3.5, label = "High Temp \n Seasonality") +
  annotate("text", x = -5.4, y = 1.5, label = "Low Temp \n Seasonality") +
  coord_cartesian(ylim = c(-4.1, 2), xlim = c(-4,4), clip = "off") +
  theme_classic() +
  theme(text=element_text(size=28))
#ggsave("../output/Climate/Wtr_Year_PC1-PC4_2024.png", width = 7.4, height = 6, units = "in")

## add WL2 garden 2024
WL2Grdn_pc_prep_2024 <- WL2Grdn_flint_bioclim_2024_all_year %>% ungroup() %>% select(cwd:pck, ann_tmean, temp_seasonality, temp_ann_range, ann_ppt, ppt_seasonality)
#scale(WL2Grdn_pc_prep_2024, all_bioclim_flint_avgs.pc$center, all_bioclim_flint_avgs.pc$scale) %*% all_bioclim_flint_avgs.pc$rotation  #gives same result as below 
WL2Grdn_predicted_2024 <- predict(all_bioclim_flint_avgs.pc_2024, newdata = WL2Grdn_pc_prep_2024)
WL2Grdn_predicted_2024
```

```
##             PC1        PC2      PC3       PC4       PC5      PC6       PC7
## [1,] -0.7663093 -0.6707531 1.944818 -2.663075 -1.232261 2.015379 0.8475977
```

``` r
#str(home_sites_pca_2024) #can add predicted WL2 point to the existing plot's data 
home_sites_pca_2024$data <- rbind(home_sites_pca_2024$data, 
  data.frame(
    parent.pop = "WL2_Garden",
    elev_m = 2020,
    TimePd = "2024",
    PC1 = WL2Grdn_predicted_2024[, "PC1"],
    PC2 = WL2Grdn_predicted_2024[, "PC2"],
    PC3 = WL2Grdn_predicted_2024[, "PC3"],
    PC4 = WL2Grdn_predicted_2024[, "PC4"],
    PC5 = WL2Grdn_predicted_2024[, "PC5"],
    PC6 = WL2Grdn_predicted_2024[, "PC6"],
    PC7 = WL2Grdn_predicted_2024[, "PC7"],
    group = "new"
  )
)

home_sites_pca_2024 + 
  geom_point(data=filter(home_sites_pca_2024$data, parent.pop == "WL2_Garden"), size=3, shape = 8, show_guide = FALSE)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-73-1.png)<!-- -->

``` r
ggsave("../output/Climate/Wtr_Year_PC1-PC4_PlusGarden_2024.png", width = 7.4, height = 6, units = "in")
```

## Extra historical PCA (60 years) - 2024

### Extra Historical Averages


``` r
flint_all_year_historical_avgs_2024_extra <- flint_all_year_historical_extra_2024 %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Historical")

flint_all_year_avgs_2024_extra <- bind_rows(flint_all_year_recent_avgs_2024, flint_all_year_historical_avgs_2024_extra) #combine into 1 dataframe 

bioclim_all_year_historical_avgs_2024_extra <- bioclim_all_year_historical_extra_2024 %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
  summarise_at(c("ann_tmean", "mean_diurnal_range", "temp_seasonality", "temp_ann_range",
                 "tmean_wettest_quarter", "tmean_driest_quarter", "ann_ppt",
                 "ppt_seasonality","ppt_warmest_quarter", "ppt_coldest_quarter"),
               c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Historical") 

bioclim_all_year_avgs_2024_extra <- bind_rows(bioclim_all_year_recent_avgs_2024, bioclim_all_year_historical_avgs_2024_extra) #combine into 1 dataframe 

bioclim_flint_all_year_avgs_2024_extra <- full_join(flint_all_year_avgs_2024_extra, bioclim_all_year_avgs_2024_extra) %>% 
  select(TimePd, parent.pop:ppt_coldest_quarter)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, Lat, Long,
## TimePd)`
```

### Correlations - Recent + Extra Historical


``` r
#normalize the data
climate_normalized_bioclim_flint_all_year_avgs_2024_extra <- bioclim_flint_all_year_avgs_2024_extra %>% ungroup() %>% 
  select(cwd:ppt_coldest_quarter) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_bioclim_flint_all_year_avgs_2024_extra)
```

```
##             cwd        pck        ppt        tmn        tmx  ann_tmean
## [1,]  1.7607283 -0.9398997 -1.9160573  1.4380458  1.5946957  1.5278952
## [2,]  0.4080370 -0.9398997 -0.5613668  1.6956114  1.5333448  1.6204510
## [3,]  0.6739528  0.4716219  0.3078160 -0.3799212 -0.6103973 -0.5023542
## [4,] -0.7534184  0.5907219  0.1365535 -0.5341506 -0.7749533 -0.6628408
## [5,] -2.3611962 -0.8915589  0.8259742  1.1899493  0.8758358  1.0326893
## [6,]  1.7489181 -0.8494567 -0.5322543  0.6790014  0.8384170  0.7660262
##      mean_diurnal_range temp_seasonality temp_ann_range tmean_wettest_quarter
## [1,]          1.2119053        1.3001330      1.3373966             1.5003838
## [2,]         -0.1753709       -0.3688543     -0.5276860             1.6927798
## [3,]         -1.1944928        0.6928895     -0.9189860            -0.4757736
## [4,]         -1.2945347        0.8682079     -0.8967724            -0.6539866
## [5,]         -1.0489317        0.9820322     -0.8503518             1.0085267
## [6,]          0.9673305        1.9817213      1.5589624             0.6654203
##      tmean_driest_quarter    ann_ppt ppt_seasonality ppt_warmest_quarter
## [1,]            1.3958796 -1.9160573       1.7610245         -1.86095527
## [2,]            1.5399063 -0.5613668       1.4086974         -1.06823740
## [3,]           -0.5095102  0.3078160      -0.2359997          0.06987606
## [4,]           -0.6645264  0.1365535      -0.4120517         -0.01029973
## [5,]            1.0416047  0.8259742       0.9528372          0.19759719
## [6,]            0.7048287 -0.5322543       1.2232077         -0.73571004
##      ppt_coldest_quarter
## [1,]          -2.0333402
## [2,]          -0.3459760
## [3,]           0.3660693
## [4,]           0.1351281
## [5,]           1.2814522
## [6,]          -0.3766522
```

``` r
dim(climate_normalized_bioclim_flint_all_year_avgs_2024_extra)
```

```
## [1] 46 15
```

``` r
cor.norm_2024_extra = cor(climate_normalized_bioclim_flint_all_year_avgs_2024_extra) #test correlations among the traits
cor.sig_2024_extra <- cor.mtest(climate_normalized_bioclim_flint_all_year_avgs_2024_extra, method = "pearson")

corrplot(cor.norm_2024_extra, type="upper",
         tl.srt = 45, p.mat = cor.sig_2024_extra$p, 
         sig.level = 0.05, insig="blank")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-75-1.png)<!-- -->

``` r
#tmn, tmx, tmean_wettest_quarter, tmean_driest_quarter and ann_tmean all highly correlated (97-99%) - only keep ann_tmean 
#temp_ann_range and mean_diurnal range 92% correlated - keep temp_ann_range 
#ann_ppt and ppt 100% correlated (ppt = avg across monts, ann_ppt = avg of the total ppt in a year) - only keep ann_ppt 
#ppt_warmest and coldest quarter highly correlated with ann_ppt and ppt - take both out 
```

Historical

``` r
climate_normalized_bioclim_flint_all_year_avgs_historic_2024_extra <- bioclim_flint_all_year_avgs_2024_extra %>% 
  ungroup() %>% 
  filter(TimePd=="Historical") %>% 
  select(tmn:tmean_driest_quarter, cwd, pck, ppt, ann_ppt:ppt_coldest_quarter) %>% 
  scale() #normalize the data so they're all on the same scale

cor.norm_historic_2024_extra = cor(climate_normalized_bioclim_flint_all_year_avgs_historic_2024_extra) #test correlations among the traits
cor.sig_historic_2024_extra <- cor.mtest(climate_normalized_bioclim_flint_all_year_avgs_historic_2024_extra, method = "pearson")

corrplot(cor.norm_historic_2024_extra, type="upper",
         tl.srt = 45, p.mat = cor.sig_historic_2024_extra$p, 
         sig.level = 0.05, insig="blank")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-76-1.png)<!-- -->

``` r
#800 x 734
```

### PCA - Recent + Extra Historical


``` r
all_bioclim_flint_avgs.pc_2024_extra = prcomp(bioclim_flint_all_year_avgs_2024_extra[c(7:8, 12, 14:15, 18:19)], scale = TRUE, center = TRUE)
str(all_bioclim_flint_avgs.pc_2024_extra)
```

```
## List of 5
##  $ sdev    : num [1:7] 1.871 1.107 1.061 0.787 0.568 ...
##  $ rotation: num [1:7, 1:7] -0.287 0.49 -0.44 -0.296 -0.19 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:7] "cwd" "pck" "ann_tmean" "temp_seasonality" ...
##   .. ..$ : chr [1:7] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:7] 55.02 147.44 9.53 675.59 32.5 ...
##   ..- attr(*, "names")= chr [1:7] "cwd" "pck" "ann_tmean" "temp_seasonality" ...
##  $ scale   : Named num [1:7] 11.7 156.87 4.43 19.02 1.17 ...
##   ..- attr(*, "names")= chr [1:7] "cwd" "pck" "ann_tmean" "temp_seasonality" ...
##  $ x       : num [1:46, 1:7] -3.839 -1.901 0.461 0.939 -0.364 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:7] "PC1" "PC2" "PC3" "PC4" ...
##  - attr(*, "class")= chr "prcomp"
```

plot % Variance Explained


``` r
summary(all_bioclim_flint_avgs.pc_2024_extra)
```

```
## Importance of components:
##                          PC1    PC2    PC3     PC4     PC5     PC6     PC7
## Standard deviation     1.871 1.1070 1.0612 0.78699 0.56848 0.38891 0.23451
## Proportion of Variance 0.500 0.1751 0.1609 0.08848 0.04617 0.02161 0.00786
## Cumulative Proportion  0.500 0.6750 0.8359 0.92437 0.97054 0.99214 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:7,2,pad="0")),
       percent_var=all_bioclim_flint_avgs.pc_2024_extra$sdev[1:7]^2/sum(all_bioclim_flint_avgs.pc_2024_extra$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-78-1.png)<!-- -->

Combine PCs with metadata


``` r
all_bioclim_flint_avgs.pc.dat_2024_extra = data.frame(all_bioclim_flint_avgs.pc_2024_extra$x)

all_bioclim_flint_avgs_locs.pc_2024_extra = cbind(bioclim_flint_all_year_avgs_2024_extra, all_bioclim_flint_avgs.pc.dat_2024_extra)

all_bioclim_flint_avgs_loadings_2024_extra = data.frame(varnames=rownames(all_bioclim_flint_avgs.pc_2024_extra$rotation), all_bioclim_flint_avgs.pc_2024_extra$rotation)
all_bioclim_flint_avgs_loadings_2024_extra #very similar to 2023 
```

```
##                          varnames        PC1         PC2         PC3
## cwd                           cwd -0.2867430 -0.01870620 -0.72038515
## pck                           pck  0.4902176 -0.08119701 -0.19702009
## ann_tmean               ann_tmean -0.4399208  0.03709301  0.29064370
## temp_seasonality temp_seasonality -0.2955555 -0.25463190  0.47451141
## temp_ann_range     temp_ann_range -0.1901512 -0.81409031 -0.05430375
## ann_ppt                   ann_ppt  0.4423740  0.06266776  0.35903460
## ppt_seasonality   ppt_seasonality -0.4058689  0.51007211  0.02717995
##                           PC4        PC5         PC6         PC7
## cwd              -0.249330563 0.47630025  0.31618391 -0.09733381
## pck              -0.256655324 0.24913857 -0.38262756  0.66327291
## ann_tmean         0.484100667 0.44039546  0.14417298  0.52108790
## temp_seasonality -0.757136884 0.03932838  0.17998550  0.12430091
## temp_ann_range    0.177230780 0.12172788 -0.45389837 -0.21423918
## ann_ppt           0.008192501 0.69064415  0.05427583 -0.43757266
## ppt_seasonality  -0.181313907 0.15416272 -0.70105182 -0.16199876
```


``` r
autoplot(all_bioclim_flint_avgs.pc_2024_extra, data = bioclim_flint_all_year_avgs_2024_extra,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-80-1.png)<!-- -->


``` r
autoplot(all_bioclim_flint_avgs.pc_2024_extra, data = bioclim_flint_all_year_avgs_2024_extra,
         x=1, y=4,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-81-1.png)<!-- -->


``` r
all_bioclim_flint_avgs_locs.pc_2024_extra_avg <- all_bioclim_flint_avgs_locs.pc_2024_extra %>%
  group_by(parent.pop, elev_m, TimePd) %>%
  summarise(across(.cols=starts_with("PC", ignore.case = FALSE), .fns = mean)) %>%
  ungroup()
```

```
## `summarise()` has grouped output by 'parent.pop', 'elev_m'. You can override
## using the `.groups` argument.
```


``` r
all_bioclim_flint_avgs_locs.pc_2024_extra_avg %>% 
  ggplot(aes(x=PC1, y=PC2, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point() +
  facet_wrap(~TimePd) +
  coord_fixed(ratio = 1.5)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-83-1.png)<!-- -->

## Permanova - Extra Historical 2024_extra
See Jenny's github: https://github.com/jrgremer/Seedbanks_drought_deluge  

``` r
all_bioclim_flint_avgs_locs.pc_dist_2024_extra <- all_bioclim_flint_avgs_locs.pc_2024_extra %>% ungroup() %>% select(PC1:PC7)
dist_matrix_all_year_2024_extra <- dist(all_bioclim_flint_avgs_locs.pc_dist_2024_extra, method = "euclidian") #use a distance function to calculate euclidian distance in PCA space
permanova_results_all_year_2024_extra <- adonis2(dist_matrix_all_year_2024_extra ~ TimePd*elev_m*Lat, data = all_bioclim_flint_avgs_locs.pc_2024_extra) #use adonis2 to run the permanova
#rows in data must be in same order as rows in distance matrix 

#look at output 
summary(permanova_results_all_year_2024_extra)
```

```
##        Df          SumOfSqs           R2               F        
##  Min.   : 7.0   Min.   :102.3   Min.   :0.3247   Min.   :11.29  
##  1st Qu.:22.5   1st Qu.:157.5   1st Qu.:0.5000   1st Qu.:11.29  
##  Median :38.0   Median :212.7   Median :0.6753   Median :11.29  
##  Mean   :30.0   Mean   :210.0   Mean   :0.6667   Mean   :11.29  
##  3rd Qu.:41.5   3rd Qu.:263.9   3rd Qu.:0.8377   3rd Qu.:11.29  
##  Max.   :45.0   Max.   :315.0   Max.   :1.0000   Max.   :11.29  
##                                                  NA's   :2      
##      Pr(>F)     
##  Min.   :0.001  
##  1st Qu.:0.001  
##  Median :0.001  
##  Mean   :0.001  
##  3rd Qu.:0.001  
##  Max.   :0.001  
##  NA's   :2
```

``` r
permanova_results_all_year_2024_extra
```

```
## Permutation test for adonis under reduced model
## Permutation: free
## Number of permutations: 999
## 
## adonis2(formula = dist_matrix_all_year_2024_extra ~ TimePd * elev_m * Lat, data = all_bioclim_flint_avgs_locs.pc_2024_extra)
##          Df SumOfSqs      R2      F Pr(>F)    
## Model     7   212.74 0.67535 11.293  0.001 ***
## Residual 38   102.27 0.32465                  
## Total    45   315.00 1.00000                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
permanova_results_all_year_2024_extra$`Pr(>F)`
```

```
## [1] 0.001    NA    NA
```

``` r
permanova_results_all_year_terms_2024_extra <- adonis2(dist_matrix_all_year_2024_extra ~ TimePd*elev_m*Lat, data = all_bioclim_flint_avgs_locs.pc_2024_extra, by = "terms")
permanova_results_all_year_terms_2024_extra #very similar to 2023
```

```
## Permutation test for adonis under reduced model
## Terms added sequentially (first to last)
## Permutation: free
## Number of permutations: 999
## 
## adonis2(formula = dist_matrix_all_year_2024_extra ~ TimePd * elev_m * Lat, data = all_bioclim_flint_avgs_locs.pc_2024_extra, by = "terms")
##                   Df SumOfSqs      R2       F Pr(>F)    
## TimePd             1   32.914 0.10449 12.2303  0.001 ***
## elev_m             1  106.406 0.33780 39.5387  0.001 ***
## Lat                1   58.909 0.18701 21.8898  0.001 ***
## TimePd:elev_m      1    2.889 0.00917  1.0734  0.363    
## TimePd:Lat         1    2.163 0.00687  0.8038  0.499    
## elev_m:Lat         1    9.380 0.02978  3.4856  0.020 *  
## TimePd:elev_m:Lat  1    0.074 0.00024  0.0275  1.000    
## Residual          38  102.265 0.32465                   
## Total             45  315.000 1.00000                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## LM on PCs (follow up on permanova) - Extra Historical 2024

For each PC fit a model with timepd and parent.pop/elev to test for climate change


``` r
lmer_results_all_year_2024_extra <- all_bioclim_flint_avgs_locs.pc_2024_extra %>%
  ungroup() %>% 
  select(TimePd, parent.pop, elev_m, Lat, Long, PC1:PC7) %>% 
  pivot_longer(starts_with("PC", ignore.case = FALSE), 
               names_to = "PC", values_to = "value") %>% 
  group_by(PC) %>% 
  nest(data=c(TimePd, parent.pop, elev_m, Lat, Long, value)) %>% 
  mutate(glm=map(data, ~ glm(value ~ TimePd*elev_m*Lat,
                               data=.x)),
         anova = map(glm, ~ broom.mixed::tidy(anova(.x))))

#lmer_results_all_year_2024_extra
```


``` r
PC_anova_2024_extra <- lmer_results_all_year_2024_extra %>% select(-data, -glm) %>% unnest(anova) %>%
  select(PC, term, p.value) %>%
  filter(p.value < 0.05) %>%
  arrange(term, p.value)

PC_anova_2024_extra
```

```
## # A tibble: 15 × 3
## # Groups:   PC [6]
##    PC    term        p.value
##    <chr> <chr>         <dbl>
##  1 PC1   Lat        6.59e-11
##  2 PC2   Lat        2.82e- 4
##  3 PC5   Lat        1.21e- 3
##  4 PC3   Lat        1.61e- 3
##  5 PC4   TimePd     2.53e- 6
##  6 PC2   TimePd     8.61e- 5
##  7 PC1   TimePd     2.97e- 4
##  8 PC7   TimePd     2.49e- 2
##  9 PC1   elev_m     3.64e-17
## 10 PC4   elev_m     1.05e- 4
## 11 PC3   elev_m     1.92e- 2
## 12 PC5   elev_m     4.78e- 2
## 13 PC7   elev_m:Lat 1.66e- 3
## 14 PC5   elev_m:Lat 1.68e- 3
## 15 PC1   elev_m:Lat 6.48e- 3
```

``` r
lmer_results_all_year_2024_extra %>% select(-data, -glm) %>% unnest(anova) %>% 
  filter(PC=="PC1"| PC=="PC4" | PC =="PC2") #the three PCs with most sig p-values for timepd
```

```
## # A tibble: 24 × 8
## # Groups:   PC [3]
##    PC    term      df deviance df.residual residual.deviance statistic   p.value
##    <chr> <chr>  <int>    <dbl>       <int>             <dbl>     <dbl>     <dbl>
##  1 PC1   NULL      NA NA                45             157.    NA      NA       
##  2 PC1   TimePd     1  6.99             44             150.    15.9     2.97e- 4
##  3 PC1   elev_m     1 93.9              43              56.6  213.      3.64e-17
##  4 PC1   Lat        1 35.4              42              21.2   80.3     6.59e-11
##  5 PC1   TimeP…     1  0.694            41              20.5    1.58    2.17e- 1
##  6 PC1   TimeP…     1  0.104            40              20.4    0.236   6.30e- 1
##  7 PC1   elev_…     1  3.66             39              16.8    8.30    6.48e- 3
##  8 PC1   TimeP…     1  0.00472          38              16.7    0.0107  9.18e- 1
##  9 PC2   NULL      NA NA                45              55.1   NA      NA       
## 10 PC2   TimePd     1 13.5              44              41.6   19.3     8.61e- 5
## # ℹ 14 more rows
```

``` r
#NOTE NOW TOP PCS = 2 and 4, but PC1 not too far behind 

#mod_test <- glm(PC4 ~ TimePd*elev_m*Lat, data=all_bioclim_flint_avgs_locs.pc_2024_extra)
#summary(mod_test)
#anova(mod_test)
```


``` r
autoplot(all_bioclim_flint_avgs.pc_2024_extra, data = bioclim_flint_all_year_avgs_2024_extra,
         x=1, y=4,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-87-1.png)<!-- -->

``` r
all_bioclim_flint_avgs_locs.pc_2024_extra_avg %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-87-2.png)<!-- -->

``` r
all_bioclim_flint_avgs_locs.pc_2024_extra_avg %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_text_repel(aes(label = parent.pop)) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed")  +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-87-3.png)<!-- -->

``` r
#high elev climate seems to be shifting to be more similar to low elev 
```


``` r
home_sites_pca_2024_extra <- all_bioclim_flint_avgs_locs.pc_2024_extra_avg %>%  
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  #geom_point(size=2, alpha=0.7) +
  labs(x="PC1 (50%)", y="PC4 (8.85%)", color="Elevation (m)") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8) +
  annotate("text", x = -4, y = -5.1, label = "Warm \n No Snow") +
  annotate("text", x = 4, y = -5.1, label = "Cold \n Snow") +
  annotate("text", x = -5.4, y = -3.5, label = "High Temp \n Seasonality") +
  annotate("text", x = -5.4, y = 1.5, label = "Low Temp \n Seasonality") +
  coord_cartesian(ylim = c(-4.1, 2), xlim = c(-4,4), clip = "off") +
  theme_classic() +
  theme(text=element_text(size=28))
#ggsave("../output/Climate/Wtr_Year_PC1-PC4_2024_ExtraHist.png", width = 7.4, height = 6, units = "in")

## add WL2 garden 2024
#str(home_sites_pca_2024_extra) #can add predicted WL2 point to the existing plot's data 
home_sites_pca_2024_extra$data <- rbind(home_sites_pca_2024_extra$data, 
  data.frame(
    parent.pop = "WL2_Garden",
    elev_m = 2020,
    TimePd = "2024",
    PC1 = WL2Grdn_predicted_2024[, "PC1"],
    PC2 = WL2Grdn_predicted_2024[, "PC2"],
    PC3 = WL2Grdn_predicted_2024[, "PC3"],
    PC4 = WL2Grdn_predicted_2024[, "PC4"],
    PC5 = WL2Grdn_predicted_2024[, "PC5"],
    PC6 = WL2Grdn_predicted_2024[, "PC6"],
    PC7 = WL2Grdn_predicted_2024[, "PC7"],
    group = "new"
  )
)

home_sites_pca_2024_extra + 
  geom_point(data=filter(home_sites_pca_2024_extra$data, parent.pop == "WL2_Garden"), size=3, shape = 8, show_guide = FALSE)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-88-1.png)<!-- -->

``` r
#ggsave("../output/Climate/Wtr_Year_PC1-PC4_PlusGarden_2024_ExtraHist.png", width = 7.4, height = 6, units = "in")
```


``` r
autoplot(all_bioclim_flint_avgs.pc_2024_extra, data = bioclim_flint_all_year_avgs_2024_extra,
         x=2, y=4,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-89-1.png)<!-- -->

``` r
all_bioclim_flint_avgs_locs.pc_2024_extra_avg %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC2, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-89-2.png)<!-- -->

``` r
all_bioclim_flint_avgs_locs.pc_2024_extra_avg %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC2, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_text_repel(aes(label = parent.pop)) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed")  +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-89-3.png)<!-- -->

``` r
#high elev climate seems to be shifting to be more similar to low elev 
```


``` r
home_sites_pca_2024_extra_2 <- all_bioclim_flint_avgs_locs.pc_2024_extra_avg %>%  
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC2, y=PC4, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  #geom_point(size=2, alpha=0.7) +
  labs(x="PC2 (17.51%)", y="PC4 (8.85%)", color="Elevation (m)") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8) +
  annotate("text", x = -4, y = -3.9, label = "High Ann \n Temp Range") +
  annotate("text", x = 3, y = -3.9, label = "Low Ann \n Temp Range") +
  annotate("text", x = -5.2, y = -2.5, label = "High Temp \n Seasonality") +
  annotate("text", x = -5.2, y = 1.5, label = "Low Temp \n Seasonality") +
  coord_cartesian(ylim = c(-3.1, 2), xlim = c(-4,3), clip = "off") +
  theme_classic() +
  theme(text=element_text(size=28))
#ggsave("../output/Climate/Wtr_Year_PC2-PC4_2024_ExtraHist.png", width = 7.4, height = 6, units = "in")

## add WL2 garden 2024
#str(home_sites_pca_2024_extra) #can add predicted WL2 point to the existing plot's data 
home_sites_pca_2024_extra_2$data <- rbind(home_sites_pca_2024_extra_2$data, 
  data.frame(
    parent.pop = "WL2_Garden",
    elev_m = 2020,
    TimePd = "2024",
    PC1 = WL2Grdn_predicted_2024[, "PC1"],
    PC2 = WL2Grdn_predicted_2024[, "PC2"],
    PC3 = WL2Grdn_predicted_2024[, "PC3"],
    PC4 = WL2Grdn_predicted_2024[, "PC4"],
    PC5 = WL2Grdn_predicted_2024[, "PC5"],
    PC6 = WL2Grdn_predicted_2024[, "PC6"],
    PC7 = WL2Grdn_predicted_2024[, "PC7"],
    group = "new"
  )
)

home_sites_pca_2024_extra_2 + 
  geom_point(data=filter(home_sites_pca_2024_extra_2$data, parent.pop == "WL2_Garden"), size=3, shape = 8, show_guide = FALSE)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-90-1.png)<!-- -->

``` r
#ggsave("../output/Climate/Wtr_Year_PC2-PC4_PlusGarden_2024_ExtraHist.png", width = 7.4, height = 6, units = "in")
```

---

## 2023 Old PCAs

### All years and months included (Flint)

#### Correlations - Flint Recent


``` r
#normalize the data
climate_normalized_all_flint_recent <- flint_all_year_recent %>% select(cwd, pck, ppt, tmn, tmx) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_all_flint_recent)
```

```
##             cwd        pck          ppt        tmn        tmx
## [1,] -0.2706522 -0.4411046 -0.527676967  0.1096802  0.2691419
## [2,] -0.6933967 -0.4411046 -0.304713705 -0.2760760 -0.4160002
## [3,] -0.6658381 -0.4411046 -0.418144449 -0.1882733 -0.1766542
## [4,] -0.3945324 -0.4411046 -0.002562943 -0.1767582 -0.3456711
## [5,]  0.1387144 -0.4411046 -0.649897829  0.2536191  0.3190530
## [6,]  0.0640652 -0.4411046 -0.275897404  0.4335427  0.5720111
```

``` r
cor.norm = cor(climate_normalized_all_flint_recent) #test correlations among the traits
corrplot(cor.norm, type = "upper")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-91-1.png)<!-- -->

``` r
#tmn and tmx highly correlated, consider removing one 
```

#### PCA - Flint Recent


``` r
#flint_all_year_recent[c(8:12)]
all_flint_recent.pc = prcomp(flint_all_year_recent[c(8:12)], scale = TRUE, center = TRUE)

str(all_flint_recent.pc)
```

```
## List of 5
##  $ sdev    : num [1:5] 1.591 0.997 0.841 0.711 0.513
##  $ rotation: num [1:5, 1:5] 0.0655 0.5071 -0.4396 -0.5 0.5434 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:5] "wtr_yr" "cwd" "pck" "ppt" ...
##   .. ..$ : chr [1:5] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:5] 2008.5 56.2 136.8 99.1 3.6
##   ..- attr(*, "names")= chr [1:5] "wtr_yr" "cwd" "pck" "ppt" ...
##  $ scale   : Named num [1:5] 8.66 37.37 310.09 130.83 6.95
##   ..- attr(*, "names")= chr [1:5] "wtr_yr" "cwd" "pck" "ppt" ...
##  $ x       : num [1:8280, 1:5] 0.27 -0.265 -0.147 -0.211 0.617 ...
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
##                           PC1    PC2    PC3    PC4     PC5
## Standard deviation     1.5907 0.9970 0.8409 0.7106 0.51328
## Proportion of Variance 0.5061 0.1988 0.1414 0.1010 0.05269
## Cumulative Proportion  0.5061 0.7049 0.8463 0.9473 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:5,2,pad="0")),
       percent_var=all_flint_recent.pc$sdev[1:5]^2/sum(all_flint_recent.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-93-1.png)<!-- -->

Combine PCs with metadata


``` r
all_flint_recent.pc.dat = data.frame(all_flint_recent.pc$x)

all_flint_recent_locs.pc = cbind(flint_all_year_recent, all_flint_recent.pc.dat)

all_flint_recent_loadings = data.frame(varnames=rownames(all_flint_recent.pc$rotation), all_flint_recent.pc$rotation)
all_flint_recent_loadings
```

```
##        varnames        PC1          PC2           PC3         PC4         PC5
## wtr_yr   wtr_yr  0.0655035  0.997369124  0.0009271914  0.02308548 -0.02074425
## cwd         cwd  0.5070734 -0.041541080 -0.5712524703 -0.17747366 -0.61913214
## pck         pck -0.4396498  0.034802689 -0.8154490666  0.10103807  0.36101375
## ppt         ppt -0.5000012  0.047102728  0.0648793677 -0.82975735 -0.23467757
## tmn         tmn  0.5434084 -0.009963733 -0.0671062059 -0.51890674  0.65638447
```


``` r
autoplot(all_flint_recent.pc, data = flint_all_year_recent,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=8, loadings.label.colour="black", loadings.label.vjust = -0.2) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-95-1.png)<!-- -->

``` r
#for plot customizations see: ?ggbiplot
```

PCs 3 and 4


``` r
autoplot(all_flint_recent.pc, data = flint_all_year_recent,
         x=3, y=4,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=8, loadings.label.colour="black", loadings.label.vjust = -0.2) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-96-1.png)<!-- -->

#### Correlations - Flint Historical


``` r
#normalize the data
climate_normalized_all_flint_historical <- flint_all_year_historical %>% select(cwd, pck, ppt, tmn, tmx) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_all_flint_historical)
```

```
##              cwd        pck        ppt         tmn          tmx
## [1,] -0.36724416 -0.4792335  0.2605971  0.34562048 -0.006821268
## [2,] -0.75783483 -0.4792335 -0.7203926 -0.33206633 -0.641162795
## [3,] -0.72380930 -0.4792335 -0.2707262 -0.23317641 -0.488785382
## [4,] -0.37933588 -0.4792335 -0.7724567 -0.31461516 -0.060999904
## [5,]  0.05624725 -0.4792335 -0.2088505 -0.05139346 -0.002306382
## [6,]  0.46258528 -0.4792335 -0.5634880  0.27581583  0.493202391
```

``` r
cor.norm = cor(climate_normalized_all_flint_historical) #test correlations among the traits
corrplot(cor.norm)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-97-1.png)<!-- -->

``` r
#tmn and tmx highly correlated, consider removing one 
```

#### PCA - Flint Historical


``` r
#flint_all_year_historical[c(8:12)]
all_flint_historical.pc = prcomp(flint_all_year_historical[c(8:12)], scale = TRUE, center = TRUE)

str(all_flint_historical.pc)
```

```
## List of 5
##  $ sdev    : num [1:5] 1.578 1.004 0.835 0.729 0.524
##  $ rotation: num [1:5, 1:5] 0.0534 0.5075 -0.4486 -0.4914 0.5449 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:5] "wtr_yr" "cwd" "pck" "ppt" ...
##   .. ..$ : chr [1:5] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:5] 1978.5 53.87 161.85 99.98 2.12
##   ..- attr(*, "names")= chr [1:5] "wtr_yr" "cwd" "pck" "ppt" ...
##  $ scale   : Named num [1:5] 8.66 35.56 337.73 126.38 6.88
##   ..- attr(*, "names")= chr [1:5] "wtr_yr" "cwd" "pck" "ppt" ...
##  $ x       : num [1:8280, 1:5] -0.000515 -0.085945 -0.235757 0.141226 0.228749 ...
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
##                           PC1    PC2    PC3    PC4     PC5
## Standard deviation     1.5778 1.0040 0.8347 0.7291 0.52365
## Proportion of Variance 0.4979 0.2016 0.1393 0.1063 0.05484
## Cumulative Proportion  0.4979 0.6995 0.8388 0.9452 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:5,2,pad="0")),
       percent_var=all_flint_historical.pc$sdev[1:5]^2/sum(all_flint_historical.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-99-1.png)<!-- -->

Combine PCs with metadata


``` r
all_flint_historical.pc.dat = data.frame(all_flint_historical.pc$x)

all_flint_historical_locs.pc = cbind(flint_all_year_historical, all_flint_historical.pc.dat)

all_flint_historical_loadings = data.frame(varnames=rownames(all_flint_historical.pc$rotation), all_flint_historical.pc$rotation)
all_flint_historical_loadings
```

```
##        varnames         PC1         PC2         PC3         PC4         PC5
## wtr_yr   wtr_yr  0.05337062  0.97890477 -0.19618454 0.008675332  0.01825924
## cwd         cwd  0.50747380 -0.14718355 -0.52782986 0.290309243  0.59826710
## pck         pck -0.44859975 -0.12825402 -0.79483681 0.062986105 -0.38285293
## ppt         ppt -0.49139497  0.05927696  0.21950926 0.807774079  0.23309644
## tmn         tmn  0.54489555 -0.01093654  0.05438192 0.509096326 -0.66395238
```


``` r
autoplot(all_flint_historical.pc, data = flint_all_year_historical,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=8, loadings.label.colour="black", loadings.label.vjust = -0.2) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-101-1.png)<!-- -->

PCs 3 and 4


``` r
autoplot(all_flint_historical.pc, data = flint_all_year_historical,
         x=3, y=4,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=8, loadings.label.colour="black", loadings.label.vjust = -0.2) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-102-1.png)<!-- -->

### All years included (bioclim)

#### Correlations - bioclim Recent


``` r
#normalize the data
climate_normalized_all_bioclim_recent <- bioclim_all_year_recent %>% select(ann_tmean:ppt_coldest_quarter) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_all_bioclim_recent)
```

```
##      ann_tmean mean_diurnal_range temp_seasonality temp_ann_range
## [1,]  1.382638          2.3560895        0.1548043      1.1605379
## [2,]  1.215908          0.9517316       -0.1872862      0.6344628
## [3,]  1.554524          2.0928718       -0.7769427      0.5439553
## [4,]  1.488280          1.6706102       -0.9609797     -1.0568969
## [5,]  1.156764          0.3791734       -0.0634623      1.1435677
## [6,]  1.165811          2.1803460        0.4343848      1.3076126
##      tmean_wettest_quarter tmean_driest_quarter    ann_ppt ppt_seasonality
## [1,]             0.8100022            1.5663953 -1.5200951      -1.2663821
## [2,]             1.4714811            1.0185365 -0.6488993       0.5083564
## [3,]             1.1335142            0.9953578 -1.1466731      -0.6870224
## [4,]             1.0003236            1.1902692 -0.6371575       2.5067915
## [5,]             0.7924499            0.8236252 -0.4463330      -0.3894973
## [6,]             0.6544411            1.0360961 -1.4295978      -0.1450034
##      ppt_warmest_quarter ppt_coldest_quarter
## [1,]          -0.6128865          -1.3098860
## [2,]          -0.7106767          -0.7887649
## [3,]          -0.7174351          -0.8210002
## [4,]          -1.4069318           0.1311461
## [5,]          -0.3082059          -0.1778300
## [6,]          -0.6428166          -1.0197999
```

``` r
cor.norm = cor(climate_normalized_all_bioclim_recent) #test correlations among the traits
corrplot(cor.norm)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-103-1.png)<!-- -->

``` r
#ann_ppt and ppt_coldest quarter highly correlated, consider removing one 
#ann_tmean and t_mean wettest and driest quarter are highly correlated, consider only keeping one 
```

#### PCA - bioclim Recent


``` r
all_bioclim_recent.pc = prcomp(bioclim_all_year_recent[c(7:10, 13:15)], scale = TRUE, center = TRUE) #took out ppt_coldest quarter & t_mean wettest and driest quarter

str(all_bioclim_recent.pc)
```

```
## List of 5
##  $ sdev    : num [1:7] 1.532 1.272 0.994 0.902 0.802 ...
##  $ rotation: num [1:7, 1:7] -0.415 -0.49 -0.157 -0.353 0.497 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:7] "ann_tmean" "mean_diurnal_range" "temp_seasonality" "temp_ann_range" ...
##   .. ..$ : chr [1:7] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:7] 10 12.9 681.8 32.1 1188.8 ...
##   ..- attr(*, "names")= chr [1:7] "ann_tmean" "mean_diurnal_range" "temp_seasonality" "temp_ann_range" ...
##  $ scale   : Named num [1:7] 4.28 1.05 46.02 1.77 517.81 ...
##   ..- attr(*, "names")= chr [1:7] "ann_tmean" "mean_diurnal_range" "temp_seasonality" "temp_ann_range" ...
##  $ x       : num [1:690, 1:7] -2.94 -1.86 -2.47 -2.24 -1.33 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:7] "PC1" "PC2" "PC3" "PC4" ...
##  - attr(*, "class")= chr "prcomp"
```

plot % Variance Explained


``` r
summary(all_bioclim_recent.pc)
```

```
## Importance of components:
##                           PC1    PC2    PC3    PC4     PC5     PC6     PC7
## Standard deviation     1.5324 1.2718 0.9945 0.9022 0.80170 0.64571 0.41399
## Proportion of Variance 0.3355 0.2311 0.1413 0.1163 0.09182 0.05956 0.02448
## Cumulative Proportion  0.3355 0.5666 0.7078 0.8241 0.91595 0.97552 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:5,2,pad="0")),
       percent_var=all_bioclim_recent.pc$sdev[1:5]^2/sum(all_bioclim_recent.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-105-1.png)<!-- -->

Combine PCs with metadata


``` r
all_bioclim_recent.pc.dat = data.frame(all_bioclim_recent.pc$x)

all_bioclim_recent_locs.pc = cbind(bioclim_all_year_recent, all_bioclim_recent.pc.dat)

all_bioclim_recent_loadings = data.frame(varnames=rownames(all_bioclim_recent.pc$rotation), all_bioclim_recent.pc$rotation)
all_bioclim_recent_loadings
```

```
##                                varnames        PC1         PC2           PC3
## ann_tmean                     ann_tmean -0.4148991 -0.17774682 -0.1819344726
## mean_diurnal_range   mean_diurnal_range -0.4896581  0.08210022  0.3851125812
## temp_seasonality       temp_seasonality -0.1571909  0.61187414 -0.4022569054
## temp_ann_range           temp_ann_range -0.3529998  0.60834138 -0.0003889499
## ann_ppt                         ann_ppt  0.4971022  0.22364037 -0.1105726797
## ppt_seasonality         ppt_seasonality -0.1783531 -0.29331724 -0.7973357509
## ppt_warmest_quarter ppt_warmest_quarter  0.3998192  0.28486208 -0.0938443458
##                            PC4        PC5         PC6         PC7
## ann_tmean           -0.3814715  0.7779958  0.09686016  0.05463276
## mean_diurnal_range  -0.4300169 -0.2952307 -0.25307358 -0.51871080
## temp_seasonality     0.3624607  0.1580632  0.14103555 -0.51267698
## temp_ann_range      -0.1125844 -0.1328645 -0.15183059  0.67225814
## ann_ppt             -0.2514883  0.2659919 -0.73962461 -0.09800719
## ppt_seasonality     -0.1918136 -0.4215976 -0.17784070  0.01570238
## ppt_warmest_quarter -0.6522830 -0.1281722  0.55222861 -0.05773825
```


``` r
autoplot(all_bioclim_recent.pc, data = bioclim_all_year_recent,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-107-1.png)<!-- -->

PCs 3 and 4


``` r
autoplot(all_bioclim_recent.pc, data = bioclim_all_year_recent,
         x=3, y=4,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-108-1.png)<!-- -->

PCs 4 and 5


``` r
autoplot(all_bioclim_recent.pc, data = bioclim_all_year_recent,
         x=4, y=5,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-109-1.png)<!-- -->

#### Correlations - bioclim Historical


``` r
#normalize the data
climate_normalized_all_bioclim_historical <- bioclim_all_year_historical %>% select(ann_tmean:ppt_coldest_quarter) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_all_bioclim_historical)
```

```
##      ann_tmean mean_diurnal_range temp_seasonality temp_ann_range
## [1,]  1.252545           1.936627        1.3249978     1.41220640
## [2,]  1.277704           1.305108       -0.7557605     0.06879103
## [3,]  1.453449           2.230458        0.7849087     1.71125434
## [4,]  1.405998           1.336538        1.9031125     1.66064622
## [5,]  1.476018           2.242153        0.2652660     2.18513031
## [6,]  1.309708           1.097525        1.3253706     1.37079976
##      tmean_wettest_quarter tmean_driest_quarter    ann_ppt ppt_seasonality
## [1,]             1.6902227           0.09046376 -1.4752934       0.2226713
## [2,]             0.9878280           1.13252458 -1.0598252       0.7660424
## [3,]             2.2368458           1.38274274 -1.4433600       2.7244130
## [4,]             2.2660085           1.14213515 -0.7706645      -0.1087671
## [5,]             0.8886036           1.47541613 -1.4682177      -1.1300182
## [6,]             0.6695276           1.68307318 -0.3909455       0.4190681
##      ppt_warmest_quarter ppt_coldest_quarter
## [1,]          -0.9490847          -1.4973563
## [2,]          -0.1876537          -0.7205898
## [3,]          -1.0938059          -1.2243013
## [4,]           0.7471331          -0.8384978
## [5,]          -0.6807313          -1.1236015
## [6,]          -0.5134700           0.3291709
```

``` r
cor.norm = cor(climate_normalized_all_bioclim_historical) #test correlations among the traits
corrplot(cor.norm)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-110-1.png)<!-- -->

``` r
#ann_ppt and ppt_coldest quarter highly correlated, consider removing one 
#ann_tmean and t_mean wettest and driest quarter are highly correlated, consider only keeping one 
```

#### PCA - bioclim Historical


``` r
all_bioclim_historical.pc = prcomp(bioclim_all_year_historical[c(7:10, 13:15)], scale = TRUE, center = TRUE) #took out ppt_coldest quarter & t_mean wettest and driest quarter

str(all_bioclim_historical.pc)
```

```
## List of 5
##  $ sdev    : num [1:7] 1.69 1.133 1.026 0.905 0.785 ...
##  $ rotation: num [1:7, 1:7] -0.329 -0.436 -0.332 -0.449 0.43 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:7] "ann_tmean" "mean_diurnal_range" "temp_seasonality" "temp_ann_range" ...
##   .. ..$ : chr [1:7] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:7] 8.94 13.64 662.63 32.45 1199.7 ...
##   ..- attr(*, "names")= chr [1:7] "ann_tmean" "mean_diurnal_range" "temp_seasonality" "temp_ann_range" ...
##  $ scale   : Named num [1:7] 4.5 1.14 53.79 2.17 539.87 ...
##   ..- attr(*, "names")= chr [1:7] "ann_tmean" "mean_diurnal_range" "temp_seasonality" "temp_ann_range" ...
##  $ x       : num [1:690, 1:7] -3.4 -1.44 -4.03 -2.42 -3.25 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:7] "PC1" "PC2" "PC3" "PC4" ...
##  - attr(*, "class")= chr "prcomp"
```

plot % Variance Explained


``` r
summary(all_bioclim_historical.pc)
```

```
## Importance of components:
##                          PC1    PC2    PC3    PC4    PC5     PC6     PC7
## Standard deviation     1.690 1.1334 1.0261 0.9051 0.7853 0.47943 0.37537
## Proportion of Variance 0.408 0.1835 0.1504 0.1170 0.0881 0.03284 0.02013
## Cumulative Proportion  0.408 0.5915 0.7419 0.8589 0.9470 0.97987 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:5,2,pad="0")),
       percent_var=all_bioclim_historical.pc$sdev[1:5]^2/sum(all_bioclim_historical.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-112-1.png)<!-- -->

Combine PCs with metadata


``` r
all_bioclim_historical.pc.dat = data.frame(all_bioclim_historical.pc$x)

all_bioclim_historical_locs.pc = cbind(bioclim_all_year_historical, all_bioclim_historical.pc.dat)

all_bioclim_historical_loadings = data.frame(varnames=rownames(all_bioclim_historical.pc$rotation), all_bioclim_historical.pc$rotation)
all_bioclim_historical_loadings
```

```
##                                varnames        PC1         PC2         PC3
## ann_tmean                     ann_tmean -0.3294470 -0.21206341  0.19416446
## mean_diurnal_range   mean_diurnal_range -0.4360648  0.11418728 -0.33952927
## temp_seasonality       temp_seasonality -0.3323013  0.55067705  0.21162465
## temp_ann_range           temp_ann_range -0.4486783  0.49089151 -0.15897799
## ann_ppt                         ann_ppt  0.4304676  0.46140476  0.19716643
## ppt_seasonality         ppt_seasonality -0.1733639 -0.03224276  0.85760365
## ppt_warmest_quarter ppt_warmest_quarter  0.4173948  0.42876187 -0.05101388
##                            PC4          PC5         PC6         PC7
## ann_tmean           -0.7096358  0.536546855 -0.01656212 -0.13059168
## mean_diurnal_range  -0.3766696 -0.498771135 -0.30340570  0.44589344
## temp_seasonality     0.3151151  0.468091101 -0.01903028  0.47199226
## temp_ann_range       0.0312569 -0.143945422  0.19483502 -0.68759096
## ann_ppt             -0.2280080  0.003342324 -0.68554183 -0.20239131
## ppt_seasonality     -0.0346791 -0.466473282  0.11897199  0.02161092
## ppt_warmest_quarter -0.4484108 -0.076855516  0.62066569  0.21711186
```


``` r
autoplot(all_bioclim_historical.pc, data = bioclim_all_year_historical,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-114-1.png)<!-- -->

PCs 3 and 4


``` r
autoplot(all_bioclim_historical.pc, data = bioclim_all_year_historical,
         x=3, y=4,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-115-1.png)<!-- -->

PCs 4 and 5


``` r
autoplot(all_bioclim_historical.pc, data = bioclim_all_year_historical,
         x=4, y=5,
         colour='elev_m', alpha=0.5,
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-116-1.png)<!-- -->

### Monthly Averages - Flint

#### Calculate avgs


``` r
flint_all_year_recent_mosavgs <- flint_all_year_recent %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long, month) %>% 
  summarise_at(c("cwd",  "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Recent") 
flint_all_year_recent_mosavgs
```

```
## # A tibble: 276 × 12
## # Groups:   parent.pop, elevation.group, elev_m, Lat, Long [23]
##    parent.pop elevation.group elev_m   Lat  Long month   cwd   pck     ppt   tmn
##    <chr>      <chr>            <dbl> <dbl> <dbl> <chr> <dbl> <dbl>   <dbl> <dbl>
##  1 BH         Low               511.  37.4 -120. apr    59.0     0  48.2    6.41
##  2 BH         Low               511.  37.4 -120. aug   154.      0   1.00  17.1 
##  3 BH         Low               511.  37.4 -120. dec    29.9     0 112.     2.44
##  4 BH         Low               511.  37.4 -120. feb    41.0     0  93.9    3.32
##  5 BH         Low               511.  37.4 -120. jan    29.4     0 124.     2.81
##  6 BH         Low               511.  37.4 -120. jul   138.      0   0.281 17.5 
##  7 BH         Low               511.  37.4 -120. jun    89.3     0   6.34  13.6 
##  8 BH         Low               511.  37.4 -120. mar    53.9     0  90.1    4.82
##  9 BH         Low               511.  37.4 -120. may    51.4     0  23.2    9.78
## 10 BH         Low               511.  37.4 -120. nov    45.5     0  52.5    5.13
## # ℹ 266 more rows
## # ℹ 2 more variables: tmx <dbl>, TimePd <chr>
```

``` r
flint_all_year_historical_mosavgs <- flint_all_year_historical %>% 
  group_by(parent.pop, elevation.group, elev_m, Lat, Long, month) %>% 
  summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean), na.rm = TRUE) %>% 
  mutate(TimePd = "Historical")
flint_all_year_historical_mosavgs
```

```
## # A tibble: 276 × 12
## # Groups:   parent.pop, elevation.group, elev_m, Lat, Long [23]
##    parent.pop elevation.group elev_m   Lat  Long month   cwd   pck    ppt   tmn
##    <chr>      <chr>            <dbl> <dbl> <dbl> <chr> <dbl> <dbl>  <dbl> <dbl>
##  1 BH         Low               511.  37.4 -120. apr    62.2 0      46.4   5.57
##  2 BH         Low               511.  37.4 -120. aug   149.  0       1.82 15.3 
##  3 BH         Low               511.  37.4 -120. dec    28.4 0      87.7   1.38
##  4 BH         Low               511.  37.4 -120. feb    40.4 0      92.2   2.76
##  5 BH         Low               511.  37.4 -120. jan    28.0 0.234 104.    1.46
##  6 BH         Low               511.  37.4 -120. jul   126.  0       2.09 15.6 
##  7 BH         Low               511.  37.4 -120. jun    87.5 0       5.73 12.4 
##  8 BH         Low               511.  37.4 -120. mar    51.2 0     101.    4.04
##  9 BH         Low               511.  37.4 -120. may    63.5 0      12.6   8.83
## 10 BH         Low               511.  37.4 -120. nov    43.6 0      85.5   4.38
## # ℹ 266 more rows
## # ℹ 2 more variables: tmx <dbl>, TimePd <chr>
```

``` r
flint_all_year_mosavgs <- bind_rows(flint_all_year_recent_mosavgs, flint_all_year_historical_mosavgs) #combine into 1 dataframe 
head(flint_all_year_mosavgs)
```

```
## # A tibble: 6 × 12
## # Groups:   parent.pop, elevation.group, elev_m, Lat, Long [1]
##   parent.pop elevation.group elev_m   Lat  Long month   cwd   pck     ppt   tmn
##   <chr>      <chr>            <dbl> <dbl> <dbl> <chr> <dbl> <dbl>   <dbl> <dbl>
## 1 BH         Low               511.  37.4 -120. apr    59.0     0  48.2    6.41
## 2 BH         Low               511.  37.4 -120. aug   154.      0   1.00  17.1 
## 3 BH         Low               511.  37.4 -120. dec    29.9     0 112.     2.44
## 4 BH         Low               511.  37.4 -120. feb    41.0     0  93.9    3.32
## 5 BH         Low               511.  37.4 -120. jan    29.4     0 124.     2.81
## 6 BH         Low               511.  37.4 -120. jul   138.      0   0.281 17.5 
## # ℹ 2 more variables: tmx <dbl>, TimePd <chr>
```

``` r
tail(flint_all_year_mosavgs)
```

```
## # A tibble: 6 × 12
## # Groups:   parent.pop, elevation.group, elev_m, Lat, Long [1]
##   parent.pop elevation.group elev_m   Lat  Long month   cwd   pck   ppt    tmn
##   <chr>      <chr>            <dbl> <dbl> <dbl> <chr> <dbl> <dbl> <dbl>  <dbl>
## 1 YO8        High             2591.  37.8 -119. jun   102.  108.   20.1  1.97 
## 2 YO8        High             2591.  37.8 -119. mar    35.0 741.  172.  -8.11 
## 3 YO8        High             2591.  37.8 -119. may    79.7 409.   42.7 -2.19 
## 4 YO8        High             2591.  37.8 -119. nov    31.6  94.2 156.  -5.75 
## 5 YO8        High             2591.  37.8 -119. oct    71.9   0    64.0 -0.564
## 6 YO8        High             2591.  37.8 -119. sep    98.1   0    32.5  2.92 
## # ℹ 2 more variables: tmx <dbl>, TimePd <chr>
```

#### Correlations - Recent + Historical


``` r
#normalize the data
climate_normalized_flint_all_year_mosavgs <- flint_all_year_mosavgs %>% ungroup() %>% 
  select(cwd:tmx) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_flint_all_year_mosavgs)
```

```
##             cwd        pck         ppt          tmn        tmx
## [1,]  0.1177043 -0.5744569 -0.61393164  0.522569507  0.5042666
## [2,]  2.8986012 -0.5744569 -1.17795339  2.093537377  2.1573833
## [3,] -0.7404077 -0.5744569  0.14433614 -0.062360634 -0.2992340
## [4,] -0.4134458 -0.5744569 -0.06688057  0.067656600 -0.1387582
## [5,] -0.7539588 -0.5744569  0.29758633 -0.008063943 -0.2555171
## [6,]  2.4436098 -0.5744569 -1.18657822  2.159332190  2.2222236
```

``` r
cor.norm = cor(climate_normalized_flint_all_year_mosavgs) #test correlations among the traits
corrplot(cor.norm)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-118-1.png)<!-- -->

``` r
#tmn and tmx highly correlated, consider removing one (98%)
#tmx and pck highly neg correlated (-85%)
#tmn and pck highly neg correlated (-86%)
```

#### PCA - Recent + Historical


``` r
#flint_all_year_mosavgs[c(8:12)]
mos_flint.pc = prcomp(flint_all_year_mosavgs[c(7:11)], scale = TRUE, center = TRUE)

str(mos_flint.pc)
```

```
## List of 5
##  $ sdev    : num [1:5] 1.955 0.834 0.527 0.435 0.117
##  $ rotation: num [1:5, 1:5] -0.428 0.357 0.459 -0.484 -0.495 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##   .. ..$ : chr [1:5] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:5] 55.03 149.32 99.52 2.86 16.13
##   ..- attr(*, "names")= chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##  $ scale   : Named num [1:5] 33.99 259.93 83.63 6.78 8.62
##   ..- attr(*, "names")= chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##  $ x       : num [1:552, 1:5] -1.0394 -4.0668 0.3565 -0.0227 0.3848 ...
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
## Standard deviation     1.9553 0.8342 0.52738 0.4347 0.11728
## Proportion of Variance 0.7647 0.1392 0.05563 0.0378 0.00275
## Cumulative Proportion  0.7647 0.9038 0.95945 0.9972 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:5,2,pad="0")),
       percent_var=mos_flint.pc$sdev[1:5]^2/sum(mos_flint.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-120-1.png)<!-- -->

Combine PCs with metadata


``` r
mos_flint.pc.dat = data.frame(mos_flint.pc$x)

mos_flint_locs.pc = cbind(flint_all_year_mosavgs, mos_flint.pc.dat)

mos_flint_loadings = data.frame(varnames=rownames(mos_flint.pc$rotation), mos_flint.pc$rotation)
mos_flint_loadings
```

```
##     varnames        PC1        PC2        PC3         PC4         PC5
## cwd      cwd -0.4281437  0.5034844 -0.4344468 -0.60960697 -0.05321426
## pck      pck  0.3566536  0.8078197  0.4556720  0.09689889 -0.05656168
## ppt      ppt  0.4589171 -0.2765907  0.3090919 -0.77958460  0.09799344
## tmn      tmn -0.4840338 -0.1307796  0.5507757 -0.10303298 -0.65927098
## tmx      tmx -0.4945680  0.0180313  0.4524680 -0.02494010  0.74143649
```


``` r
autoplot(mos_flint.pc, data = flint_all_year_mosavgs,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="month",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-122-1.png)<!-- -->

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
## # A tibble: 552 × 10
##    parent.pop elev_m TimePd   month   pck     PC1    PC2     PC3     PC4     PC5
##    <chr>       <dbl> <chr>    <chr> <dbl>   <dbl>  <dbl>   <dbl>   <dbl>   <dbl>
##  1 BH           511. Histori… apr   0     -1.01   -0.226 -0.123   0.258   0.0349
##  2 BH           511. Histori… aug   0     -3.83    1.05   0.122  -1.07    0.0979
##  3 BH           511. Histori… dec   0      0.378  -0.798 -0.275   0.566  -0.105 
##  4 BH           511. Histori… feb   0      0.0115 -0.657 -0.170   0.280  -0.0414
##  5 BH           511. Histori… jan   0.234  0.464  -0.858 -0.202   0.420  -0.0919
##  6 BH           511. Histori… jul   0     -3.60    0.700  0.478  -0.665   0.158 
##  7 BH           511. Histori… jun   0     -2.64    0.173  0.511   0.0504  0.194 
##  8 BH           511. Histori… mar   0     -0.256  -0.548 -0.0905 -0.0198 -0.0409
##  9 BH           511. Histori… may   0     -1.77   -0.147  0.302   0.485   0.169 
## 10 BH           511. Histori… nov   0     -0.315  -0.613  0.0178  0.253  -0.0113
## # ℹ 542 more rows
```

``` r
mos_flint_locs.pc_avg$month <- factor(mos_flint_locs.pc_avg$month, levels=month_order) 
```


``` r
mos_flint_locs.pc_avg %>% 
  ggplot(aes(x=PC1, y=PC2, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point() +
  facet_wrap(~month) +
  coord_fixed(ratio = 1.5)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-124-1.png)<!-- -->

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

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-125-1.png)<!-- -->

``` r
#high elev climate seems to be shifting to be more similar to low elev 
#hard to see which months though 
```

### Avg across years and months (Flint)

#### Correlations - Recent + Historical


``` r
#normalize the data
climate_normalized_flint_all_year_avgs <- flint_all_year_avgs %>% ungroup() %>% 
  select(cwd:tmx) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_flint_all_year_avgs)
```

```
##             cwd        pck        ppt        tmn        tmx
## [1,]  1.7785471 -0.9367972 -1.9289972  1.4141866  1.6144369
## [2,]  0.4162065 -0.9367972 -0.5691081  1.6728378  1.5530014
## [3,]  0.6732155  0.4303289  0.2959730 -0.3966053 -0.5919655
## [4,] -0.7521931  0.5445863  0.1231076 -0.5491665 -0.7584042
## [5,] -2.3485552 -0.8889143  0.8274893  1.1706967  0.8927377
## [6,]  1.7506304 -0.8481292 -0.5518531  0.6619430  0.8658246
```

``` r
cor.norm = cor(climate_normalized_flint_all_year_avgs) #test correlations among the traits
corrplot(cor.norm)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-126-1.png)<!-- -->

``` r
#tmn and tmx highly correlated, consider removing one (98%)
#tmx and pck highly neg correlated (-85%)
#tmn and pck highly neg correlated (-86%)
```

#### PCA - Recent + Historical


``` r
#flint_all_year_avgs[c(8:12)]
avgs_flint.pc = prcomp(flint_all_year_avgs[c(6:10)], scale = TRUE, center = TRUE)

str(avgs_flint.pc)
```

```
## List of 5
##  $ sdev    : num [1:5] 1.835 1.081 0.602 0.294 0.122
##  $ rotation: num [1:5, 1:5] -0.259 0.513 0.419 -0.494 -0.5 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##   .. ..$ : chr [1:5] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:5] 55.03 149.32 99.52 2.86 16.13
##   ..- attr(*, "names")= chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##  $ scale   : Named num [1:5] 11.71 159.39 26.41 4.28 4.6
##   ..- attr(*, "names")= chr [1:5] "cwd" "pck" "ppt" "tmn" ...
##  $ x       : num [1:46, 1:5] -3.256 -2.429 0.662 1.176 -0.525 ...
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
## Standard deviation     1.8350 1.0812 0.60205 0.29433 0.12208
## Proportion of Variance 0.6734 0.2338 0.07249 0.01733 0.00298
## Cumulative Proportion  0.6734 0.9072 0.97969 0.99702 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:5,2,pad="0")),
       percent_var=avgs_flint.pc$sdev[1:5]^2/sum(avgs_flint.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-128-1.png)<!-- -->

Combine PCs with metadata


``` r
avgs_flint.pc.dat = data.frame(avgs_flint.pc$x)

avgs_flint_locs.pc = cbind(flint_all_year_avgs, avgs_flint.pc.dat)

avgs_flint_loadings = data.frame(varnames=rownames(avgs_flint.pc$rotation), avgs_flint.pc$rotation)
avgs_flint_loadings
```

```
##     varnames        PC1        PC2       PC3         PC4         PC5
## cwd      cwd -0.2589820 -0.7458758 0.5751483 -0.21336273  0.01668451
## pck      pck  0.5134733 -0.1272787 0.3493988  0.74825509 -0.19539739
## ppt      ppt  0.4194411  0.4639996 0.6150857 -0.46330239  0.12567415
## tmn      tmn -0.4935219  0.3495497 0.2799340  0.07366548 -0.74193090
## tmx      tmx -0.4997787  0.2999814 0.3007182  0.41774988  0.62871777
```


``` r
autoplot(avgs_flint.pc, data = flint_all_year_avgs,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-130-1.png)<!-- -->


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
##    parent.pop elev_m TimePd          pck    PC1    PC2    PC3     PC4     PC5
##    <chr>       <dbl> <chr>         <dbl>  <dbl>  <dbl>  <dbl>   <dbl>   <dbl>
##  1 BH           511. Historical   0.0195 -3.02  -1.18   0.202  0.542   0.0514
##  2 BH           511. Recent       0      -3.26  -1.12   0.390  0.592  -0.0639
##  3 CC           313  Historical   0.0793 -2.30   0.453  0.331  0.244  -0.0132
##  4 CC           313  Recent       0      -2.43   0.595  0.497  0.246  -0.146 
##  5 CP2         2244. Historical 265.      1.19  -0.737  0.330 -0.127  -0.0264
##  6 CP2         2244. Recent     218.      0.662 -0.736  0.431 -0.235  -0.114 
##  7 CP3         2266. Historical 282.      1.69   0.145 -0.624  0.247  -0.0977
##  8 CP3         2266. Recent     236.      1.18   0.129 -0.548  0.154  -0.173 
##  9 DPR         1019. Historical  20.5    -0.221  2.80  -0.740 -0.0752  0.119 
## 10 DPR         1019. Recent       7.63   -0.525  2.93  -0.556 -0.0882 -0.0688
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

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-132-1.png)<!-- -->


``` r
avgs_flint_locs.pc_avg %>% 
  mutate(group=str_c(parent.pop,elev_m))  %>%
  ggplot(aes(x=PC1, y=PC2, shape=TimePd, color=elev_m)) +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_point(size=2, alpha=0.7) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  geom_path(aes(group=group),arrow = arrow(length=unit(5, "points")), linewidth = .8)
```

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-133-1.png)<!-- -->

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

![](Climate_PCAs_AllYear_files/figure-html/unnamed-chunk-133-2.png)<!-- -->

``` r
#high elev climate seems to be shifting to be more similar to low elev 
```
