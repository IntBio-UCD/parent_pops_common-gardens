---
title: "ClimateDist_Comp"
author: "Brandie QC"
date: "2025-06-10"
output: 
  html_document: 
    keep_md: true
---



# Comparing water year and growth season climate distances

## Libraries

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
library(lmerTest) #mixed models
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
library(geosphere) #for calculating geographic distance
```

## Load Gower's Distance


``` r
wl2_gowers_2023 <- read_csv("../output/Climate/Gowers_WL2.csv") %>% 
  select(parent.pop:GrwSsn_GD, Wtr_Year_GD) %>% 
  pivot_wider(names_from = TimePd, values_from = c(GrwSsn_GD, Wtr_Year_GD)) %>% 
  mutate(WL2_Lat=38.82599, WL2_Long=-120.2509, WL2_Elev=2020) %>% 
  mutate(Geographic_Dist=distHaversine(cbind(WL2_Long, WL2_Lat), cbind(Long, Lat)),
         Elev_Dist=elev_m-WL2_Elev) %>% # Calculate the distance using the haversine formula
  rename(pop=parent.pop)
```

```
## Rows: 46 Columns: 12
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (3): parent.pop, elevation.group, TimePd
## dbl (9): elev_m, Lat, Long, GrwSsn_GD, GrwSsn_FLINT_GD, GrwSsn_BIOCLIM_GD, W...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_gowers_2324 <- read_csv("../output/Climate/Gowers_WL2_2324.csv") %>% 
  select(parent.pop:GrwSsn_GD, Wtr_Year_GD) %>% 
  pivot_wider(names_from = TimePd, values_from = c(GrwSsn_GD, Wtr_Year_GD)) %>% 
  mutate(WL2_Lat=38.82599, WL2_Long=-120.2509, WL2_Elev=2020) %>% 
  mutate(Geographic_Dist=distHaversine(cbind(WL2_Long, WL2_Lat), cbind(Long, Lat)),
         Elev_Dist=elev_m-WL2_Elev) %>% # Calculate the distance using the haversine formula
  rename(pop=parent.pop)
```

```
## Rows: 46 Columns: 8
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (3): parent.pop, elevation.group, TimePd
## dbl (5): elev_m, Lat, Long, GrwSsn_GD, Wtr_Year_GD
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

## Stats

``` r
wl2_gowers_2023_recent <- wl2_gowers_2023 %>% 
  select(pop:GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  pivot_longer(cols = ends_with("Recent"),
               names_to = "Season",
               values_to = "GD")

mod_2023_recent <- lmer(GD ~ Season + (1|pop), data=wl2_gowers_2023_recent)
summary(mod_2023_recent)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: GD ~ Season + (1 | pop)
##    Data: wl2_gowers_2023_recent
## 
## REML criterion at convergence: -88.5
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.3087 -0.7304 -0.1270  0.6107  2.9722 
## 
## Random effects:
##  Groups   Name        Variance  Std.Dev.
##  pop      (Intercept) 0.0009496 0.03082 
##  Residual             0.0059098 0.07688 
## Number of obs: 46, groups:  pop, 23
## 
## Fixed effects:
##                          Estimate Std. Error       df t value Pr(>|t|)    
## (Intercept)               0.33838    0.01727 43.17260  19.594  < 2e-16 ***
## SeasonWtr_Year_GD_Recent  0.12602    0.02267 22.00000   5.559 1.38e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## SsnW_Y_GD_R -0.656
```

``` r
wl2_gowers_2023_hist <- wl2_gowers_2023 %>% 
  select(pop:GrwSsn_GD_Historical, Wtr_Year_GD_Historical) %>% 
  pivot_longer(cols = ends_with("Historical"),
               names_to = "Season",
               values_to = "GD")

mod_2023_hist <- lmer(GD ~ Season + (1|pop), data=wl2_gowers_2023_hist)
summary(mod_2023_hist)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: GD ~ Season + (1 | pop)
##    Data: wl2_gowers_2023_hist
## 
## REML criterion at convergence: -87.6
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.3037 -0.5970 -0.3431  0.6448  2.0510 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  pop      (Intercept) 0.001202 0.03468 
##  Residual             0.005835 0.07639 
## Number of obs: 46, groups:  pop, 23
## 
## Fixed effects:
##                              Estimate Std. Error       df t value Pr(>|t|)    
## (Intercept)                   0.34252    0.01749 42.75181  19.581  < 2e-16 ***
## SeasonWtr_Year_GD_Historical  0.14002    0.02252 22.00000   6.216 2.95e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## SsnW_Y_GD_H -0.644
```


``` r
wl2_gowers_2324_recent <- wl2_gowers_2324 %>% 
  select(pop:GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  pivot_longer(cols = ends_with("Recent"),
               names_to = "Season",
               values_to = "GD")

mod_2324_recent <- lmer(GD ~ Season + (1|pop), data=wl2_gowers_2324_recent)
summary(mod_2324_recent)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: GD ~ Season + (1 | pop)
##    Data: wl2_gowers_2324_recent
## 
## REML criterion at convergence: -89
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -1.68129 -0.41941  0.03636  0.36389  2.15255 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  pop      (Intercept) 0.005805 0.07619 
##  Residual             0.003068 0.05539 
## Number of obs: 46, groups:  pop, 23
## 
## Fixed effects:
##                          Estimate Std. Error       df t value Pr(>|t|)    
## (Intercept)               0.35092    0.01964 30.81263  17.866   <2e-16 ***
## SeasonWtr_Year_GD_Recent  0.01419    0.01633 22.00000   0.869    0.394    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## SsnW_Y_GD_R -0.416
```

``` r
wl2_gowers_2324_hist <- wl2_gowers_2324 %>% 
  select(pop:GrwSsn_GD_Historical, Wtr_Year_GD_Historical) %>% 
  pivot_longer(cols = ends_with("Historical"),
               names_to = "Season",
               values_to = "GD")

mod_2324_hist <- lmer(GD ~ Season + (1|pop), data=wl2_gowers_2324_hist)
summary(mod_2324_hist)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: GD ~ Season + (1 | pop)
##    Data: wl2_gowers_2324_hist
## 
## REML criterion at convergence: -86.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.5296 -0.3895 -0.1958  0.3252  1.8692 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  pop      (Intercept) 0.008697 0.09326 
##  Residual             0.002545 0.05045 
## Number of obs: 46, groups:  pop, 23
## 
## Fixed effects:
##                              Estimate Std. Error       df t value Pr(>|t|)    
## (Intercept)                   0.40170    0.02211 27.52691  18.169   <2e-16 ***
## SeasonWtr_Year_GD_Historical -0.03359    0.01488 22.00000  -2.258   0.0342 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## SsnW_Y_GD_H -0.336
```
