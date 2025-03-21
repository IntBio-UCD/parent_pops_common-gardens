---
title: "Surv_to_Rep_Y1"
author: "Brandie QC"
date: "2025-03-21"
output: 
  html_document: 
    keep_md: true
---



# Survival to Bolting in Year 1 

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
library(magrittr)
```

```
## 
## Attaching package: 'magrittr'
## 
## The following object is masked from 'package:purrr':
## 
##     set_names
## 
## The following object is masked from 'package:tidyr':
## 
##     extract
```

``` r
#conflicted::conflicts_prefer(dplyr::filter)
library(viridis) #for pretty colors
```

```
## Loading required package: viridisLite
```

``` r
library(ggrepel) #for non-overlapping labels on plots
library(ggdist) #visualizations of distributions and uncertainty 
library(ggpubr)

#library(sp) #for calculating geographic distance 
library(geosphere) #for calculating geographic distance
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
conflicted::conflicts_prefer(lmerTest::lmer)
```

```
## [conflicted] Will prefer lmerTest::lmer over any other package.
```

``` r
library(broom.mixed)
library(tidymodels)
```

```
## ── Attaching packages ────────────────────────────────────── tidymodels 1.2.0 ──
## ✔ broom        1.0.7     ✔ rsample      1.2.1
## ✔ dials        1.3.0     ✔ tune         1.2.1
## ✔ infer        1.0.7     ✔ workflows    1.1.4
## ✔ modeldata    1.4.0     ✔ workflowsets 1.1.0
## ✔ parsnip      1.2.1     ✔ yardstick    1.3.1
## ✔ recipes      1.1.0     
## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
## ✖ infer::chisq_test()   masks rstatix::chisq_test()
## ✖ scales::discard()     masks purrr::discard()
## ✖ Matrix::expand()      masks tidyr::expand()
## ✖ magrittr::extract()   masks tidyr::extract()
## ✖ rstatix::filter()     masks dplyr::filter(), stats::filter()
## ✖ recipes::fixed()      masks stringr::fixed()
## ✖ dials::get_n()        masks rstatix::get_n()
## ✖ dplyr::lag()          masks stats::lag()
## ✖ Matrix::pack()        masks tidyr::pack()
## ✖ infer::prop_test()    masks rstatix::prop_test()
## ✖ magrittr::set_names() masks purrr::set_names()
## ✖ yardstick::spec()     masks readr::spec()
## ✖ recipes::step()       masks lmerTest::step(), stats::step()
## ✖ infer::t_test()       masks rstatix::t_test()
## ✖ Matrix::unpack()      masks tidyr::unpack()
## ✖ recipes::update()     masks Matrix::update(), stats::update()
## • Search for functions across packages at https://www.tidymodels.org/find/
```

``` r
library(furrr) #Apply Mapping Functions in Parallel using Futures
```

```
## Loading required package: future
```

``` r
tidymodels_prefer()
# install.packages("multilevelmod")
library(multilevelmod)

library(brms)
```

```
## Loading required package: Rcpp
## 
## Attaching package: 'Rcpp'
## 
## The following object is masked from 'package:rsample':
## 
##     populate
## 
## Loading 'brms' package (version 2.22.0). Useful instructions
## can be found by typing help('brms'). A more detailed introduction
## to the package is available through vignette('brms_overview').
```

``` r
#conflicted::conflicts_prefer(brms::ar)
#conflicted::conflicts_prefer(dplyr::combine)
#conflicted::conflicts_prefer(brms::dstudent_t)
library(tidybayes) #for extracting and visiaulizing brms model output 
library(modelr) #for data grid

sem <- function(x, na.rm=FALSE) {           #for caclulating standard error
  sd(x,na.rm=na.rm)/sqrt(length(na.omit(x)))
} 

cbbPalette2 <- c("#E69F00","#000000", "#56B4E9","#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
timepd_palette <- c("#56B4E9","#D55E00")

options(mc.cores = parallel::detectCores())
```

## Load WL2 surv

``` r
wl2_ann_cens <- read_csv("../input/WL2_Data/CorrectedCSVs/WL2_annual_census_20231027_corrected.csv") %>% 
  unite(Genotype, pop:rep, sep="_", remove = FALSE) %>% 
  unite(BedLoc, bed:`bed-col`, sep="_", remove = FALSE) %>% 
  filter(BedLoc!="K_5_C") %>% #get rid of duplicate locations
  filter(BedLoc!="B_32_A") %>% #get rid of duplicate locations
  filter(!is.na(pop), !str_detect(Genotype, ".*buff*")) #remove buffers 
```

```
## Warning: One or more parsing issues, call `problems()` on your data frame for details,
## e.g.:
##   dat <- vroom(...)
##   problems(dat)
```

```
## Rows: 1826 Columns: 19
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (10): date, block, bed, bed-col, pop, mf, rep, pheno, herbiv.y.n, survey...
## dbl  (7): bed-row, diam.mm, num.flw, num.fruit, long.fruit.cm, total.branch,...
## lgl  (2): height.cm, long.leaf.cm
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_surv_1020 <- read_csv("../input/WL2_Data/CorrectedCSVs/WL2_mort_pheno_20231020_corrected.csv") #need to add in 10/27 mortality 
```

```
## Rows: 1826 Columns: 14
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (12): block, bed, bed.col, pop, mf, rep, bud.date, flower.date, fruit.da...
## dbl  (1): bed.row
## lgl  (1): last.fruit.date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_surv_1020 %>% rowwise() %>%  #checking if mf and rep can be converted to numeric 
  filter(!is.na(mf)) %>%  
  filter(is.na(as.numeric(mf))) #all buffers
```

```
## Warning: There were 87 warnings in `filter()`.
## The first warning was:
## ℹ In argument: `is.na(as.numeric(mf))`.
## ℹ In row 116.
## Caused by warning:
## ! NAs introduced by coercion
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 86 remaining warnings.
```

```
## # A tibble: 87 × 14
## # Rowwise: 
##    block bed   bed.row bed.col pop   mf    rep   bud.date flower.date fruit.date
##    <chr> <chr>   <dbl> <chr>   <chr> <chr> <chr> <chr>    <chr>       <chr>     
##  1 B     A          59 B       WL1*  buff… buff… <NA>     <NA>        <NA>      
##  2 B     A          60 A       WL1*  buff… buff… <NA>     <NA>        <NA>      
##  3 B     A          59 C       WL1*  buff… buff… <NA>     <NA>        <NA>      
##  4 B     A          59 D       WL1*  buff… buff… <NA>     <NA>        <NA>      
##  5 C     B          53 A       WL1*  buff… buff… <NA>     <NA>        <NA>      
##  6 C     B          53 B       WL1*  buff… buff… <NA>     <NA>        <NA>      
##  7 B     B          55 A       WL1*  buff… buff… <NA>     <NA>        <NA>      
##  8 B     B          56 B       WL1*  buff… buff… <NA>     <NA>        <NA>      
##  9 B     B          60 A       WL1*  buff… buff… <NA>     <NA>        <NA>      
## 10 B     B          60 B       WL1*  buff… buff… <NA>     <NA>        <NA>      
## # ℹ 77 more rows
## # ℹ 4 more variables: last.flower.date <chr>, last.fruit.date <lgl>,
## #   death.date <chr>, survey.notes <chr>
```

``` r
wl2_surv_1027 <- wl2_ann_cens %>% 
  filter(pheno=="X") %>% 
  select(death.date_2=date, block:bed, bed.row=`bed-row`, bed.col=`bed-col`, pop:rep) #add in 10/27 death dates
#note: 10/27 pheno dates (like flowering), not updated with this code 
wl2_surv_y1 <- left_join(wl2_surv_1020, wl2_surv_1027) %>% 
  mutate(death.date=if_else(is.na(death.date), death.date_2, death.date)) %>% 
  mutate(pop= str_replace(pop, "Y08", "YO8")) %>% 
  mutate(pop= str_replace(pop, "Y04", "YO4")) %>% 
  unite(BedLoc, bed:bed.col, sep="_", remove = FALSE) %>% 
  filter(BedLoc!="K_5_C") %>% #get rid of duplicate locations
  filter(BedLoc!="B_32_A") %>% #get rid of duplicate locations
  unite(Genotype, pop:rep, sep="_", remove = FALSE) %>% 
  filter(!is.na(pop), !str_detect(Genotype, ".*buff*")) %>%  #remove buffers 
  mutate(mf=as.double(mf), rep=as.double(rep)) %>% 
  select(block:survey.notes)
```

```
## Joining with `by = join_by(block, bed, bed.row, bed.col, pop, mf, rep)`
```

``` r
#wl2_surv_y1_to_export <- wl2_surv_y1 %>% select(block:rep, death.date, survey.notes)
#write_csv(wl2_surv_y1_to_export, "../output/WL2_Traits/WL2_Mortality_2023.csv")
#unique(wl2_surv_y1$pop)
```


## Load UCD Surv

``` r
ucd_surv <- read_csv("../input/UCD_Data/CorrectedCSVs/UCD_transplants_pheno_mort_20231016_corrected.csv") %>% 
  rename(death.date=`Death Date`, bud.date=`Date First Bud`, flower.date=`Date First Flower`, 
         fruit.date=`Date First Fruit`, last.flower.date=`Date Last Flower`, last.fruit.date=`Date Last Fruit`) %>% 
  filter(!is.na(pop)) %>% 
  unite(BedLoc, block:col, sep="_", remove = FALSE) %>% 
  unite(Genotype, pop:rep, sep="_", remove = FALSE) 
```

```
## Rows: 858 Columns: 13
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (10): block, col, pop, Date First Bud, Date First Flower, Date First Fru...
## dbl  (3): row, mf, rep
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
#unique(ucd_surv$pop)
#this has 1 more row than the annual census dataset --> WL2_4_10 died in Jan (1/6/23), new germ germinated so it was converted to rep 100
#including the plant above tehre are 3 plants in this scenario, WL2_3_100(L1_5_A)-1/6/23 and SQ2_6_100(L2_37_C)-12/13/22
```

## Establishment

``` r
ucd_est <- read_csv("../output/UCD_Traits/UCD_Establishment.csv") %>% select(block:rep, Establishment) 
```

```
## Rows: 757 Columns: 19
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr   (5): block, col, Genotype, pop, elevation.group
## dbl  (13): row, mf, rep, elev_m, Lat, Long, GrwSsn_GD_Recent, GrwSsn_GD_Hist...
## date  (1): death.date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_est <- read_csv("../output/WL2_Traits/WL2_Establishment.csv") %>% select(block:rep, Establishment) 
```

```
## Rows: 1573 Columns: 21
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr   (7): block, BedLoc, bed, bed.col, Genotype, pop, elevation.group
## dbl  (13): bed.row, mf, rep, elev_m, Lat, Long, GrwSsn_GD_Recent, GrwSsn_GD_...
## date  (1): death.date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

## Gower's Distance

``` r
garden_climate <- read_csv("../output/Climate/flint_climate_UCDpops.csv") %>% 
  filter(parent.pop=="WL2_Garden" | parent.pop=="UCD_Garden") %>% 
  select(parent.pop:Long) %>% 
  distinct()
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
garden_climate
```

```
## # A tibble: 2 × 5
##   parent.pop elevation.group elev_m   Lat  Long
##   <chr>      <chr>            <dbl> <dbl> <dbl>
## 1 UCD_Garden Low                 16  38.5 -122.
## 2 WL2_Garden High              2020  38.8 -120.
```

``` r
#UCD LAT/LONG = 38.53250, -121.7830
#WL2 Lat/Long = 38.82599, -120.2509

ucd_gowers <- read_csv("../output/Climate/Gowers_UCD.csv") %>% 
  select(parent.pop:GrwSsn_GD, Wtr_Year_GD) %>% 
  pivot_wider(names_from = TimePd, values_from = c(GrwSsn_GD, Wtr_Year_GD)) %>% 
  mutate(UCD_Lat=38.53250, UCD_Long=-121.7830, UCD_Elev=16) %>% 
  mutate(Geographic_Dist=distHaversine(cbind(UCD_Long, UCD_Lat), cbind(Long, Lat)),
         Elev_Dist=elev_m-UCD_Elev) %>% # Calculate the distance using the haversine formula (dist in meters)
  #mutate(Lat_Dist=UCD_Lat-Lat, Long_Dist=UCD_Long-Long) %>% #Garden-Home - lat and long per Gerst et al 2011 which kept them separate for some directionality
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
wl2_gowers_2023 <- read_csv("../output/Climate/Gowers_WL2.csv") %>% 
  select(parent.pop:GrwSsn_GD, Wtr_Year_GD) %>% 
  pivot_wider(names_from = TimePd, values_from = c(GrwSsn_GD, Wtr_Year_GD)) %>% 
  mutate(WL2_Lat=38.82599, WL2_Long=-120.2509, WL2_Elev=2020) %>% 
  mutate(Geographic_Dist=distHaversine(cbind(WL2_Long, WL2_Lat), cbind(Long, Lat)),
         Elev_Dist=elev_m-WL2_Elev) %>% # Calculate the distance using the haversine formula
  #mutate(Lat_Dist=WL2_Lat-Lat, Long_Dist=WL2_Long-Long) %>% #Garden-Home - lat and long per Gerst et al 2011 which kept them separate for some directionality
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

## Climate Subtraction Distances

``` r
ucd_wtr_year_sub_recent <- read_csv("../output/Climate/full_year_Subtraction_Dist_from_Home_Davis_Recent.csv") %>% 
  select(parent.pop, Wtr_Year_TempDist_Recent=ann_tmean_dist, Wtr_Year_PPTDist_Recent=ann_ppt_dist)
```

```
## Rows: 23 Columns: 18
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (16): elev_m, ppt_dist, cwd_dist, pck_dist, tmn_dist, tmx_dist, ann_tmea...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
ucd_wtr_year_sub_historic <- read_csv("../output/Climate/full_year_Subtraction_Dist_from_Home_Davis_Historical.csv") %>%
  select(parent.pop, Wtr_Year_TempDist_Historic=ann_tmean_dist, Wtr_Year_PPTDist_Historic=ann_ppt_dist)
```

```
## Rows: 23 Columns: 18
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (16): elev_m, ppt_dist, cwd_dist, pck_dist, tmn_dist, tmx_dist, ann_tmea...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
ucd_grwssn_sub_recent <- read_csv("../output/Climate/grwssn_Subtraction_Dist_from_Home_Davis_Recent.csv") %>% 
  select(parent.pop, GrwSsn_TempDist_Recent=ann_tmean_dist, GrwSsn_PPTDist_Recent=ann_ppt_dist)
```

```
## Rows: 23 Columns: 17
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (15): elev_m, ppt_dist, cwd_dist, tmn_dist, tmx_dist, ann_tmean_dist, me...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
ucd_grwssn_sub_historic <- read_csv("../output/Climate/grwssn_Subtraction_Dist_from_Home_Davis_Historical.csv") %>% 
  select(parent.pop, GrwSsn_TempDist_Historic=ann_tmean_dist, GrwSsn_PPTDist_Historic=ann_ppt_dist)
```

```
## Rows: 23 Columns: 17
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (15): elev_m, ppt_dist, cwd_dist, tmn_dist, tmx_dist, ann_tmean_dist, me...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
ucd_sub_dist <- ucd_wtr_year_sub_recent %>% 
  left_join(ucd_wtr_year_sub_historic) %>% 
  left_join(ucd_grwssn_sub_recent) %>% 
  left_join(ucd_grwssn_sub_historic) %>% 
  rename(pop=parent.pop) %>% 
  left_join(ucd_gowers)
```

```
## Joining with `by = join_by(parent.pop)`
## Joining with `by = join_by(parent.pop)`
## Joining with `by = join_by(parent.pop)`
## Joining with `by = join_by(pop)`
```

``` r
wl2_wtr_year_sub_recent <- read_csv("../output/Climate/full_year_Subtraction_Dist_from_Home_WL2_Recent.csv") %>% 
  select(parent.pop, Wtr_Year_TempDist_Recent=ann_tmean_dist, Wtr_Year_PPTDist_Recent=ann_ppt_dist)
```

```
## Rows: 23 Columns: 18
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (16): elev_m, ppt_dist, cwd_dist, pck_dist, tmn_dist, tmx_dist, ann_tmea...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_wtr_year_sub_historic <- read_csv("../output/Climate/full_year_Subtraction_Dist_from_Home_WL2_Historical.csv") %>% 
  select(parent.pop, Wtr_Year_TempDist_Historic=ann_tmean_dist, Wtr_Year_PPTDist_Historic=ann_ppt_dist)
```

```
## Rows: 23 Columns: 18
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (16): elev_m, ppt_dist, cwd_dist, pck_dist, tmn_dist, tmx_dist, ann_tmea...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_grwssn_sub_recent <- read_csv("../output/Climate/grwssn_Subtraction_Dist_from_Home_WL2_Recent.csv") %>% 
  select(parent.pop, GrwSsn_TempDist_Recent=ann_tmean_dist, GrwSsn_PPTDist_Recent=ann_ppt_dist)
```

```
## Rows: 23 Columns: 17
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (15): elev_m, ppt_dist, cwd_dist, tmn_dist, tmx_dist, ann_tmean_dist, me...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_grwssn_sub_historic <- read_csv("../output/Climate/grwssn_Subtraction_Dist_from_Home_WL2_Historical.csv") %>% 
  select(parent.pop, GrwSsn_TempDist_Historic=ann_tmean_dist, GrwSsn_PPTDist_Historic=ann_ppt_dist)
```

```
## Rows: 23 Columns: 17
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (15): elev_m, ppt_dist, cwd_dist, tmn_dist, tmx_dist, ann_tmean_dist, me...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_sub_dist_2023 <- wl2_wtr_year_sub_recent %>% 
  left_join(wl2_wtr_year_sub_historic) %>% 
  left_join(wl2_grwssn_sub_recent) %>% 
  left_join(wl2_grwssn_sub_historic) %>% 
  rename(pop=parent.pop) %>% 
  left_join(wl2_gowers_2023)
```

```
## Joining with `by = join_by(parent.pop)`
## Joining with `by = join_by(parent.pop)`
## Joining with `by = join_by(parent.pop)`
## Joining with `by = join_by(pop)`
```

## Davis

``` r
ucd_surv %>% filter(!is.na(bud.date), is.na(fruit.date)) #all plants that initiated reproduction, but did not finish, have a death date 
```

```
## # A tibble: 18 × 15
##    BedLoc  block   row col   Genotype pop      mf   rep bud.date flower.date
##    <chr>   <chr> <dbl> <chr> <chr>    <chr> <dbl> <dbl> <chr>    <chr>      
##  1 D2_37_B D2       37 B     TM2_6_5  TM2       6     5 3/17/23  <NA>       
##  2 F1_8_A  F1        8 A     TM2_2_2  TM2       2     2 3/22/23  <NA>       
##  3 F1_6_C  F1        6 C     DPR_6_9  DPR       6     9 3/31/23  <NA>       
##  4 F2_40_C F2       40 C     DPR_7_4  DPR       7     4 3/17/23  <NA>       
##  5 H1_4_B  H1        4 B     BH_2_12  BH        2    12 5/30/23  <NA>       
##  6 H1_5_B  H1        5 B     SC_4_2   SC        4     2 5/11/23  <NA>       
##  7 H1_12_B H1       12 B     FR_2_8   FR        2     8 4/17/23  <NA>       
##  8 H1_15_B H1       15 B     CC_3_3   CC        3     3 4/10/23  <NA>       
##  9 H1_16_A H1       16 A     CC_4_10  CC        4    10 4/17/23  <NA>       
## 10 H1_17_B H1       17 B     DPR_5_2  DPR       5     2 3/22/23  4/13/23    
## 11 H1_6_C  H1        6 C     TM2_1_5  TM2       1     5 3/22/23  <NA>       
## 12 H2_25_C H2       25 C     DPR_2_6  DPR       2     6 3/22/23  <NA>       
## 13 H2_30_D H2       30 D     TM2_1_6  TM2       1     6 3/22/23  <NA>       
## 14 J1_6_B  J1        6 B     DPR_6_10 DPR       6    10 3/31/23  4/17/23    
## 15 J1_19_B J1       19 B     BH_3_11  BH        3    11 5/18/23  <NA>       
## 16 J2_33_B J2       33 B     TM2_2_6  TM2       2     6 3/22/23  <NA>       
## 17 L1_4_A  L1        4 A     SC_5_8   SC        5     8 4/13/22  <NA>       
## 18 L2_30_A L2       30 A     TM2_1_12 TM2       1    12 3/22/23  4/13/23    
## # ℹ 5 more variables: fruit.date <chr>, last.flower.date <chr>,
## #   last.fruit.date <chr>, death.date <chr>, Notes <chr>
```

``` r
ucd_surv %>% filter(!is.na(bud.date), !is.na(death.date)) #many plants with a bud date, have a later death date 
```

```
## # A tibble: 46 × 15
##    BedLoc  block   row col   Genotype pop      mf   rep bud.date flower.date
##    <chr>   <chr> <dbl> <chr> <chr>    <chr> <dbl> <dbl> <chr>    <chr>      
##  1 D2_31_B D2       31 B     TM2_4_11 TM2       4    11 4/10/23  4/24/23    
##  2 D2_37_B D2       37 B     TM2_6_5  TM2       6     5 3/17/23  <NA>       
##  3 D2_35_D D2       35 D     BH_3_6   BH        3     6 5/1/23   5/18/23    
##  4 F1_8_A  F1        8 A     TM2_2_2  TM2       2     2 3/22/23  <NA>       
##  5 F1_4_C  F1        4 C     TM2_5_7  TM2       5     7 3/17/23  4/17/23    
##  6 F1_6_C  F1        6 C     DPR_6_9  DPR       6     9 3/31/23  <NA>       
##  7 F2_35_D F2       35 D     BH_2_1   BH        2     1 5/22/23  6/5/23     
##  8 F2_40_C F2       40 C     DPR_7_4  DPR       7     4 3/17/23  <NA>       
##  9 F2_40_D F2       40 D     TM2_1_4  TM2       1     4 3/17/23  4/17/23    
## 10 H1_4_B  H1        4 B     BH_2_12  BH        2    12 5/30/23  <NA>       
## # ℹ 36 more rows
## # ℹ 5 more variables: fruit.date <chr>, last.flower.date <chr>,
## #   last.fruit.date <chr>, death.date <chr>, Notes <chr>
```

``` r
ucd_surv %>% filter(!is.na(bud.date), is.na(death.date)) #some budding plants do not have a death date
```

```
## # A tibble: 4 × 15
##   BedLoc  block   row col   Genotype pop      mf   rep bud.date flower.date
##   <chr>   <chr> <dbl> <chr> <chr>    <chr> <dbl> <dbl> <chr>    <chr>      
## 1 D2_26_B D2       26 B     BH_5_15  BH        5    15 5/18/23  5/30/23    
## 2 D2_29_D D2       29 D     BH_2_9   BH        2     9 6/1/23   6/12/23    
## 3 J2_25_D J2       25 D     BH_2_5   BH        2     5 5/8/23   5/22/23    
## 4 L1_13_C L1       13 C     BH_3_13  BH        3    13 4/27/23  5/15/23    
## # ℹ 5 more variables: fruit.date <chr>, last.flower.date <chr>,
## #   last.fruit.date <chr>, death.date <chr>, Notes <chr>
```

``` r
ucd_surv %>% filter(is.na(bud.date), !is.na(flower.date)) #did not miss any budding plants
```

```
## # A tibble: 0 × 15
## # ℹ 15 variables: BedLoc <chr>, block <chr>, row <dbl>, col <chr>,
## #   Genotype <chr>, pop <chr>, mf <dbl>, rep <dbl>, bud.date <chr>,
## #   flower.date <chr>, fruit.date <chr>, last.flower.date <chr>,
## #   last.fruit.date <chr>, death.date <chr>, Notes <chr>
```

``` r
ucd_surv %>% filter(is.na(bud.date), !is.na(fruit.date)) #did not miss any budding plants 
```

```
## # A tibble: 0 × 15
## # ℹ 15 variables: BedLoc <chr>, block <chr>, row <dbl>, col <chr>,
## #   Genotype <chr>, pop <chr>, mf <dbl>, rep <dbl>, bud.date <chr>,
## #   flower.date <chr>, fruit.date <chr>, last.flower.date <chr>,
## #   last.fruit.date <chr>, death.date <chr>, Notes <chr>
```


``` r
ucd_surv_to_rep <- ucd_surv %>% 
  filter(!str_detect(Genotype, "buffer")) %>% 
  left_join(ucd_est) %>% 
  left_join(ucd_gowers) %>% 
  filter(Establishment==1) %>% 
  mutate(death.date=mdy(death.date)) %>% 
  mutate(SurvtoRep_Y1=if_else(is.na(bud.date), 0, 1)) %>% 
  select(block:rep, elevation.group:Wtr_Year_GD_Historical, Geographic_Dist, Elev_Dist, bud.date, death.date, SurvtoRep_Y1) 
```

```
## Joining with `by = join_by(block, row, col, Genotype, pop, mf, rep)`
## Joining with `by = join_by(pop)`
```

``` r
#ucd_surv_to_rep %>% filter(rep==100) #double check 0 for rep 100s
#ucd_surv_to_rep %>% filter(Genotype=="WL2_4_10")

#write_csv(ucd_surv_to_rep, "../output/UCD_Traits/UCD_SurvtoRep.csv")
```


``` r
ucd_surv_to_rep_sub_dist <- ucd_surv_to_rep %>% 
  select(block:rep, SurvtoRep_Y1) %>% 
  left_join(ucd_sub_dist)
```

```
## Joining with `by = join_by(pop)`
```

``` r
unique(ucd_surv_to_rep_sub_dist$block)
```

```
##  [1] "D1" "D2" "F1" "F2" "H1" "H2" "J1" "J2" "L1" "L2"
```

``` r
unique(ucd_surv_to_rep_sub_dist$pop)
```

```
##  [1] "WL2"   "CP2"   "YO11"  "CC"    "FR"    "BH"    "IH"    "LV3"   "SC"   
## [10] "LVTR1" "SQ3"   "TM2"   "WL1"   "YO7"   "DPR"   "SQ2"   "SQ1"   "YO8"  
## [19] "YO4"   "WR"    "WV"    "CP3"   "LV1"
```

``` r
unique(ucd_surv_to_rep_sub_dist$mf)
```

```
##  [1]  4 10  5  3  6  1  8  7  2  9 12
```


### Bar plots 

``` r
ucd_surv_to_rep %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=fct_reorder(pop, meanSurv), y=meanSurv, fill=GrwSsn_GD_Recent)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.2, position = 
                  position_dodge(0.75)) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="Surv-to-Rep Y1", x="Parent Population", fill="Growth Season Gowers") +
  scale_fill_viridis(option="mako", direction = -1) +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

![](Surv_to_Rep_Y1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

``` r
#ggsave("../output/UCD_Traits/UCD_SurvtoRepY1_GrwSsn_GD_Recent.png", width = 12, height = 8, units = "in")

ucd_surv_to_rep %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=fct_reorder(pop, meanSurv), y=meanSurv, fill=Wtr_Year_GD_Recent)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.2, position = 
                  position_dodge(0.75)) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="Surv-to-Rep Y1", x="Parent Population", fill="Water Year Gowers") +
  scale_fill_viridis(option="mako", direction = -1) +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

![](Surv_to_Rep_Y1_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

``` r
#ggsave("../output/UCD_Traits/UCD_SurvtoRepY1_Wtr_Year_GD_Recent.png", width = 12, height = 8, units = "in")
```


## WL2

``` r
wl2_ann_cens %>% filter(pheno=="B") #check for any plants that started budding the week of the annual census
```

```
## # A tibble: 1 × 21
##   date   block BedLoc bed   `bed-row` `bed-col` Genotype pop   mf    rep   pheno
##   <chr>  <chr> <chr>  <chr>     <dbl> <chr>     <chr>    <chr> <chr> <chr> <chr>
## 1 10/27… J     F_2_B  F             2 B         TM2_1_11 TM2   1     11    B    
## # ℹ 10 more variables: diam.mm <dbl>, height.cm <lgl>, long.leaf.cm <lgl>,
## #   num.flw <dbl>, num.fruit <dbl>, long.fruit.cm <dbl>, total.branch <dbl>,
## #   repro.branch <dbl>, herbiv.y.n <chr>, survey.notes <chr>
```


``` r
wl2_surv_y1 %>% filter(!is.na(bud.date), is.na(fruit.date)) #some plants initiated reproduction but did not make fruits
```

```
## # A tibble: 8 × 16
##   block BedLoc bed   bed.row bed.col Genotype pop      mf   rep bud.date
##   <chr> <chr>  <chr>   <dbl> <chr>   <chr>    <chr> <dbl> <dbl> <chr>   
## 1 A     A_23_A A          23 A       FR_7_3   FR        7     3 9/6/23  
## 2 B     A_46_B A          46 B       TM2_3_11 TM2       3    11 9/20/23 
## 3 I     E_48_D E          48 D       TM2_1_10 TM2       1    10 8/2/23  
## 4 I     F_21_D F          21 D       FR_7_11  FR        7    11 8/30/23 
## 5 L     H_13_A H          13 A       TM2_2_6  TM2       2     6 10/20/23
## 6 K     H_21_B H          21 B       TM2_1_12 TM2       1    12 8/2/23  
## 7 L     H_6_C  H           6 C       TM2_5_11 TM2       5    11 8/2/23  
## 8 M     J_10_D J          10 D       TM2_3_10 TM2       3    10 8/2/23  
## # ℹ 6 more variables: flower.date <chr>, fruit.date <chr>,
## #   last.flower.date <chr>, last.fruit.date <lgl>, death.date <chr>,
## #   survey.notes <chr>
```

``` r
wl2_surv_y1 %>% filter(!is.na(bud.date), !is.na(death.date))  #most of above lived
```

```
## # A tibble: 3 × 16
##   block BedLoc bed   bed.row bed.col Genotype pop      mf   rep bud.date
##   <chr> <chr>  <chr>   <dbl> <chr>   <chr>    <chr> <dbl> <dbl> <chr>   
## 1 A     A_23_A A          23 A       FR_7_3   FR        7     3 9/6/23  
## 2 I     E_48_D E          48 D       TM2_1_10 TM2       1    10 8/2/23  
## 3 M     J_10_D J          10 D       TM2_3_10 TM2       3    10 8/2/23  
## # ℹ 6 more variables: flower.date <chr>, fruit.date <chr>,
## #   last.flower.date <chr>, last.fruit.date <lgl>, death.date <chr>,
## #   survey.notes <chr>
```

``` r
wl2_surv_y1 %>% filter(is.na(bud.date), !is.na(flower.date)) #didn't miss any budding plants
```

```
## # A tibble: 0 × 16
## # ℹ 16 variables: block <chr>, BedLoc <chr>, bed <chr>, bed.row <dbl>,
## #   bed.col <chr>, Genotype <chr>, pop <chr>, mf <dbl>, rep <dbl>,
## #   bud.date <chr>, flower.date <chr>, fruit.date <chr>,
## #   last.flower.date <chr>, last.fruit.date <lgl>, death.date <chr>,
## #   survey.notes <chr>
```

``` r
wl2_surv_y1 %>% filter(is.na(bud.date), !is.na(fruit.date)) #didn't miss any budding plants 
```

```
## # A tibble: 0 × 16
## # ℹ 16 variables: block <chr>, BedLoc <chr>, bed <chr>, bed.row <dbl>,
## #   bed.col <chr>, Genotype <chr>, pop <chr>, mf <dbl>, rep <dbl>,
## #   bud.date <chr>, flower.date <chr>, fruit.date <chr>,
## #   last.flower.date <chr>, last.fruit.date <lgl>, death.date <chr>,
## #   survey.notes <chr>
```

``` r
wl2_surv_y1 %>% filter(!is.na(last.flower.date)) #NAs
```

```
## # A tibble: 2 × 16
##   block BedLoc bed   bed.row bed.col Genotype pop      mf   rep bud.date
##   <chr> <chr>  <chr>   <dbl> <chr>   <chr>    <chr> <dbl> <dbl> <chr>   
## 1 G     D_16_C D          16 C       TM2_6_13 TM2       6    13 <NA>    
## 2 L     I_8_A  I           8 A       WL1_7_16 WL1       7    16 <NA>    
## # ℹ 6 more variables: flower.date <chr>, fruit.date <chr>,
## #   last.flower.date <chr>, last.fruit.date <lgl>, death.date <chr>,
## #   survey.notes <chr>
```

``` r
wl2_surv_y1 %>% filter(!is.na(last.fruit.date)) #none 
```

```
## # A tibble: 0 × 16
## # ℹ 16 variables: block <chr>, BedLoc <chr>, bed <chr>, bed.row <dbl>,
## #   bed.col <chr>, Genotype <chr>, pop <chr>, mf <dbl>, rep <dbl>,
## #   bud.date <chr>, flower.date <chr>, fruit.date <chr>,
## #   last.flower.date <chr>, last.fruit.date <lgl>, death.date <chr>,
## #   survey.notes <chr>
```

``` r
wl2_surv_y1 %>% filter(is.na(bud.date))
```

```
## # A tibble: 1,539 × 16
##    block BedLoc bed   bed.row bed.col Genotype   pop      mf   rep bud.date
##    <chr> <chr>  <chr>   <dbl> <chr>   <chr>      <chr> <dbl> <dbl> <chr>   
##  1 A     A_1_B  A           1 B       LVTR1_7_1  LVTR1     7     1 <NA>    
##  2 A     A_2_A  A           2 A       SQ2_6_14   SQ2       6    14 <NA>    
##  3 A     A_2_B  A           2 B       YO8_8_3    YO8       8     3 <NA>    
##  4 A     A_3_A  A           3 A       CC_2_3     CC        2     3 <NA>    
##  5 A     A_3_B  A           3 B       YO11_5_14  YO11      5    14 <NA>    
##  6 A     A_4_A  A           4 A       BH_6_3     BH        6     3 <NA>    
##  7 A     A_4_B  A           4 B       DPR_4_8    DPR       4     8 <NA>    
##  8 A     A_5_A  A           5 A       CP2_5_1    CP2       5     1 <NA>    
##  9 A     A_5_B  A           5 B       LVTR1_3_12 LVTR1     3    12 <NA>    
## 10 A     A_6_A  A           6 A       CC_5_3     CC        5     3 <NA>    
## # ℹ 1,529 more rows
## # ℹ 6 more variables: flower.date <chr>, fruit.date <chr>,
## #   last.flower.date <chr>, last.fruit.date <lgl>, death.date <chr>,
## #   survey.notes <chr>
```

``` r
wl2_surv_to_rep_y1 <- wl2_surv_y1 %>% 
  left_join(wl2_est) %>% 
  left_join(wl2_gowers_2023) %>% 
  filter(Establishment==1) %>% 
  mutate(death.date=mdy(death.date)) %>% 
  mutate(bud.date=if_else(Genotype=="TM2_1_11", "10/27/23", bud.date)) %>% #add in bud date for plant that started budding the week of the annual census 
  mutate(SurvtoRep_Y1=if_else(is.na(bud.date), 0, 1)) %>% 
  select(block:rep, elevation.group:Wtr_Year_GD_Historical, Geographic_Dist, Elev_Dist, bud.date, death.date, SurvtoRep_Y1) 
```

```
## Joining with `by = join_by(block, BedLoc, bed, bed.row, bed.col, Genotype, pop,
## mf, rep)`
## Joining with `by = join_by(pop)`
```

``` r
#wl2_surv_to_rep_y1 %>% filter(SurvtoRep_Y1>0)
#this is quite uninteresting since only TM2 and FR budded in year 1 

#write_csv(wl2_surv_to_rep_y1, "../output/WL2_Traits/WL2_SurvtoRep_y1.csv")
```


``` r
wl2_surv_to_rep_y1_sub_dist <- wl2_surv_to_rep_y1 %>% 
  select(block:rep, SurvtoRep_Y1) %>% 
  left_join(wl2_sub_dist_2023)
```

```
## Joining with `by = join_by(pop)`
```

``` r
unique(wl2_surv_to_rep_y1_sub_dist$block)
```

```
##  [1] "A" "B" "D" "C" "E" "F" "G" "H" "I" "J" "K" "L" "M"
```

``` r
unique(wl2_surv_to_rep_y1_sub_dist$pop)
```

```
##  [1] "TM2"   "CC"    "CP2"   "IH"    "CP3"   "SQ2"   "YO11"  "BH"    "LVTR1"
## [10] "SQ3"   "WL2"   "FR"    "WL1"   "YO4"   "SC"    "DPR"   "YO7"   "LV1"  
## [19] "LV3"   "YO8"   "SQ1"   "WR"
```

``` r
unique(wl2_surv_to_rep_y1_sub_dist$mf)
```

```
##  [1]  6  2  5  3  8  7  4  9  1 14 10 13 11
```

### Bar plots 

``` r
wl2_surv_to_rep_y1 %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=fct_reorder(pop, meanSurv), y=meanSurv, fill=GrwSsn_GD_Recent)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.2, position = 
                  position_dodge(0.75)) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="Surv-to-Rep Y1", x="Parent Population", fill="Growth Season Gowers") +
  scale_fill_viridis(option="mako", direction = -1) +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

![](Surv_to_Rep_Y1_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

``` r
#ggsave("../output/WL2_Traits/WL2_SurvtoRepY1_GrwSsn_GD_Recent.png", width = 12, height = 8, units = "in")

wl2_surv_to_rep_y1 %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=fct_reorder(pop, meanSurv), y=meanSurv, fill=Wtr_Year_GD_Recent)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.2, position = 
                  position_dodge(0.75)) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="Surv-to-Rep Y1", x="Parent Population", fill="Water Year Gowers") +
  scale_fill_viridis(option="mako", direction = -1) +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

![](Surv_to_Rep_Y1_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

``` r
#ggsave("../output/WL2_Traits/WL2_SurvtoRepY1_Wtr_Year_GD_Recent.png", width = 12, height = 8, units = "in")
```

## Scatterplots

### Davis

``` r
#scatter plots
GSCD_recent <- ucd_surv_to_rep %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=GrwSsn_GD_Recent, y=meanSurv, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Recent Growth Season CD") +
  theme(text=element_text(size=25))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

``` r
WYCD_recent <- ucd_surv_to_rep %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Wtr_Year_GD_Recent, y=meanSurv, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Recent Water Year CD") +
  theme(text=element_text(size=25))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

``` r
GD <- ucd_surv_to_rep %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, Geographic_Dist) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Geographic_Dist, y=meanSurv, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Geographic Distance (m)") +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m'. You can override using the
## `.groups` argument.
```

``` r
ED <- ucd_surv_to_rep %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, Elev_Dist) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Elev_Dist, y=meanSurv, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Elevation Distance (m)") +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m'. You can override using the
## `.groups` argument.
```

``` r
ucd_surv_to_rep_y1_FIG <- ggarrange(GSCD_recent, WYCD_recent, GD, ED, ncol=2, nrow=2) 
ggsave("../output/UCD_Traits/UCD_SurvtoRep_Y1_SCATTERS_Recent.png", width = 24, height = 18, units = "in")
```


``` r
#scatter plots
GSCD_historic <- ucd_surv_to_rep %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, GrwSsn_GD_Historical, Wtr_Year_GD_Historical) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=GrwSsn_GD_Historical, y=meanSurv, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Historic Growth Season CD") +
  theme(text=element_text(size=25))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Historical'.
## You can override using the `.groups` argument.
```

``` r
WYCD_historic <- ucd_surv_to_rep %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, GrwSsn_GD_Historical, Wtr_Year_GD_Historical) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Wtr_Year_GD_Historical, y=meanSurv, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Historic Water Year CD") +
  theme(text=element_text(size=25))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Historical'.
## You can override using the `.groups` argument.
```

``` r
ucd_surv_to_rep_y1_FIG <- ggarrange(GSCD_historic, WYCD_historic, GD, ED, ncol=2, nrow=2) 
ggsave("../output/UCD_Traits/UCD_SurvtoRep_Y1_SCATTERS_Historic.png", width = 24, height = 18, units = "in")
```

### WL2

``` r
#scatter plots
GSCD_recent <- wl2_surv_to_rep_y1 %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=GrwSsn_GD_Recent, y=meanSurv, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Recent Growth Season CD") +
  theme(text=element_text(size=25))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

``` r
WYCD_recent <- wl2_surv_to_rep_y1 %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Wtr_Year_GD_Recent, y=meanSurv, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Recent Water Year CD") +
  theme(text=element_text(size=25))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

``` r
GD <- wl2_surv_to_rep_y1 %>% 
  group_by(pop, elev_m, Geographic_Dist) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Geographic_Dist, y=meanSurv, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Geographic Distance (m)") +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m'. You can override using the
## `.groups` argument.
```

``` r
ED <- wl2_surv_to_rep_y1 %>% 
  group_by(pop, elev_m, Elev_Dist) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Elev_Dist, y=meanSurv, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Elevation Distance (m)") +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m'. You can override using the
## `.groups` argument.
```

``` r
wl2_surv_to_rep_y1_FIG <- ggarrange(GSCD_recent, WYCD_recent, GD, ED, ncol=2, nrow=2) 
ggsave("../output/WL2_Traits/WL2_SurvtoRep_Y1_SCATTERS_Recent.png", width = 24, height = 18, units = "in")
```


``` r
#scatter plots
GSCD_historic <- wl2_surv_to_rep_y1 %>% 
  group_by(pop, elev_m, GrwSsn_GD_Historical, Wtr_Year_GD_Historical) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=GrwSsn_GD_Historical, y=meanSurv, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Historic Growth Season CD") +
  theme(text=element_text(size=25))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Historical'.
## You can override using the `.groups` argument.
```

``` r
WYCD_historic <- wl2_surv_to_rep_y1 %>% 
  group_by(pop, elev_m, GrwSsn_GD_Historical, Wtr_Year_GD_Historical) %>% 
  summarise(meanSurv=mean(SurvtoRep_Y1, na.rm = TRUE), semSurv=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Wtr_Year_GD_Historical, y=meanSurv, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanSurv-semSurv,ymax=meanSurv+semSurv),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Historic Water Year CD") +
  theme(text=element_text(size=25))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Historical'.
## You can override using the `.groups` argument.
```

``` r
wl2_surv_to_rep_y1_FIG <- ggarrange(GSCD_historic, WYCD_historic, GD, ED, ncol=2, nrow=2) 
ggsave("../output/WL2_Traits/WL2_SurvtoRep_Y1_SCATTERS_Historic.png", width = 24, height = 18, units = "in")
```

### Directional Distance

``` r
#scatter plots - recent
GSCD_prob_recent <- wl2_surv_to_rep_y1_sub_dist %>% 
  group_by(pop, elev_m, GrwSsn_TempDist_Recent, GrwSsn_TempDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=GrwSsn_TempDist_Recent, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Recent Growth Season Temp Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_TempDist_Recent'.
## You can override using the `.groups` argument.
```

``` r
WYCD_prob_recent <- wl2_surv_to_rep_y1_sub_dist %>% 
  group_by(pop, elev_m, Wtr_Year_TempDist_Recent, Wtr_Year_TempDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Wtr_Year_TempDist_Recent, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3,linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Recent Water Year Temp Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m',
## 'Wtr_Year_TempDist_Recent'. You can override using the `.groups` argument.
```

``` r
GD_prob <- wl2_surv_to_rep_y1_sub_dist %>% 
  group_by(pop, elev_m, GrwSsn_TempDist_Recent, Wtr_Year_TempDist_Recent, Geographic_Dist) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Geographic_Dist, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.2, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Geographic Distance (m)") +
  theme(text=element_text(size=30), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_TempDist_Recent',
## 'Wtr_Year_TempDist_Recent'. You can override using the `.groups` argument.
```

``` r
ED_prob <- wl2_surv_to_rep_y1_sub_dist %>% 
  group_by(pop, elev_m, Elev_Dist) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Elev_Dist, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Elevation Distance (m)") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m'. You can override using the
## `.groups` argument.
```

``` r
WL2_establishment_sub_FIG_prob_recent <- ggarrange(GSCD_prob_recent, WYCD_prob_recent, GD_prob, ED_prob, ncol=2, nrow=2) 
ggsave("../output/WL2_Traits/WL2_SurvtoRep_Y1_TmpSubDist_SCATTERS_Recent.png", width = 24, height = 18, units = "in")
```


``` r
#scatter plots - historic
GSCD_prob_historic <- wl2_surv_to_rep_y1_sub_dist %>% 
  group_by(pop, elev_m, GrwSsn_TempDist_Recent, GrwSsn_TempDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=GrwSsn_TempDist_Historic, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Historic Growth Season Temp Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_TempDist_Recent'.
## You can override using the `.groups` argument.
```

``` r
WYCD_prob_historic <- wl2_surv_to_rep_y1_sub_dist %>% 
  group_by(pop, elev_m, Wtr_Year_TempDist_Recent, Wtr_Year_TempDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Wtr_Year_TempDist_Historic, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3,linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Historic Water Year Temp Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m',
## 'Wtr_Year_TempDist_Recent'. You can override using the `.groups` argument.
```

``` r
WL2_establishment_sub_FIG_prob_historic <- ggarrange(GSCD_prob_historic, WYCD_prob_historic, GD_prob, ED_prob, ncol=2, nrow=2) 
ggsave("../output/WL2_Traits/WL2_SurvtoRep_Y1_TmpSubDist_SCATTERS_Historic.png", width = 24, height = 18, units = "in")
```


``` r
#scatter plots - recent
GSCD_prob_recent <- ucd_surv_to_rep_sub_dist %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, GrwSsn_TempDist_Recent, GrwSsn_TempDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=GrwSsn_TempDist_Recent, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Recent Growth Season Temp Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_TempDist_Recent'.
## You can override using the `.groups` argument.
```

``` r
WYCD_prob_recent <- ucd_surv_to_rep_sub_dist %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, Wtr_Year_TempDist_Recent, Wtr_Year_TempDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Wtr_Year_TempDist_Recent, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3,linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Recent Water Year Temp Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m',
## 'Wtr_Year_TempDist_Recent'. You can override using the `.groups` argument.
```

``` r
GD_prob <- ucd_surv_to_rep_sub_dist %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, GrwSsn_TempDist_Recent, Wtr_Year_TempDist_Recent, Geographic_Dist) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Geographic_Dist, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Geographic Distance (m)") +
  theme(text=element_text(size=30), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_TempDist_Recent',
## 'Wtr_Year_TempDist_Recent'. You can override using the `.groups` argument.
```

``` r
ED_prob <- ucd_surv_to_rep_sub_dist %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, Elev_Dist) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Elev_Dist, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Elevation Distance (m)") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m'. You can override using the
## `.groups` argument.
```

``` r
UCD_establishment_sub_FIG_prob_recent <- ggarrange(GSCD_prob_recent, WYCD_prob_recent, GD_prob, ED_prob, ncol=2, nrow=2) 
ggsave("../output/UCD_Traits/UCD_SurvtoRep_Y1_TmpSubDist_SCATTERS_Recent.png", width = 24, height = 18, units = "in")
```


``` r
#scatter plots - historic
GSCD_prob_historic <- ucd_surv_to_rep_sub_dist %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, GrwSsn_TempDist_Recent, GrwSsn_TempDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=GrwSsn_TempDist_Historic, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Historic Growth Season Temp Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_TempDist_Recent'.
## You can override using the `.groups` argument.
```

``` r
WYCD_prob_historic <- ucd_surv_to_rep_sub_dist %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, Wtr_Year_TempDist_Recent, Wtr_Year_TempDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Wtr_Year_TempDist_Historic, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3,linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Historic Water Year Temp Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m',
## 'Wtr_Year_TempDist_Recent'. You can override using the `.groups` argument.
```

``` r
UCD_establishment_sub_FIG_prob_historic <- ggarrange(GSCD_prob_historic, WYCD_prob_historic, GD_prob, ED_prob, ncol=2, nrow=2) 
ggsave("../output/UCD_Traits/UCD_SurvtoRep_Y1_TmpSubDist_SCATTERS_Historic.png", width = 24, height = 18, units = "in")
```


``` r
#scatter plots - recent
GSCD_prob_recent <- wl2_surv_to_rep_y1_sub_dist %>% 
  group_by(pop, elev_m, GrwSsn_PPTDist_Recent, GrwSsn_PPTDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=GrwSsn_PPTDist_Recent, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=0.3, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Recent Growth Season PPT Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_PPTDist_Recent'.
## You can override using the `.groups` argument.
```

``` r
WYCD_prob_recent <- wl2_surv_to_rep_y1_sub_dist %>% 
  group_by(pop, elev_m, Wtr_Year_PPTDist_Recent, Wtr_Year_PPTDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Wtr_Year_PPTDist_Recent, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3,linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Recent Water Year PPT Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'Wtr_Year_PPTDist_Recent'.
## You can override using the `.groups` argument.
```

``` r
GD_prob <- wl2_surv_to_rep_y1_sub_dist %>% 
  group_by(pop, elev_m, GrwSsn_PPTDist_Recent, Wtr_Year_PPTDist_Recent, Geographic_Dist) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Geographic_Dist, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.2, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Geographic Distance (m)") +
  theme(text=element_text(size=30), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_PPTDist_Recent',
## 'Wtr_Year_PPTDist_Recent'. You can override using the `.groups` argument.
```

``` r
ED_prob <- wl2_surv_to_rep_y1_sub_dist %>% 
  group_by(pop, elev_m, Elev_Dist) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Elev_Dist, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Elevation Distance (m)") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m'. You can override using the
## `.groups` argument.
```

``` r
WL2_establishment_sub_FIG_prob_recent <- ggarrange(GSCD_prob_recent, WYCD_prob_recent, GD_prob, ED_prob, ncol=2, nrow=2) 
ggsave("../output/WL2_Traits/WL2_SurvtoRep_Y1_PPTSubDist_SCATTERS_Recent.png", width = 24, height = 18, units = "in")
```


``` r
#scatter plots - historic
GSCD_prob_historic <- wl2_surv_to_rep_y1_sub_dist %>% 
  group_by(pop, elev_m, GrwSsn_PPTDist_Recent, GrwSsn_PPTDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=GrwSsn_PPTDist_Historic, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Historic Growth Season PPT Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_PPTDist_Recent'.
## You can override using the `.groups` argument.
```

``` r
WYCD_prob_historic <- wl2_surv_to_rep_y1_sub_dist %>% 
  group_by(pop, elev_m, Wtr_Year_PPTDist_Recent, Wtr_Year_PPTDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Wtr_Year_PPTDist_Historic, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3,linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Historic Water Year PPT Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'Wtr_Year_PPTDist_Recent'.
## You can override using the `.groups` argument.
```

``` r
WL2_establishment_sub_FIG_prob_historic <- ggarrange(GSCD_prob_historic, WYCD_prob_historic, GD_prob, ED_prob, ncol=2, nrow=2) 
ggsave("../output/WL2_Traits/WL2_SurvtoRep_Y1_PPTSubDist_SCATTERS_Historic.png", width = 24, height = 18, units = "in")
```


``` r
#scatter plots - recent
GSCD_prob_recent <- ucd_surv_to_rep_sub_dist %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, GrwSsn_PPTDist_Recent, GrwSsn_PPTDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=GrwSsn_PPTDist_Recent, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Recent Growth Season PPT Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_PPTDist_Recent'.
## You can override using the `.groups` argument.
```

``` r
WYCD_prob_recent <- ucd_surv_to_rep_sub_dist %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, Wtr_Year_PPTDist_Recent, Wtr_Year_PPTDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Wtr_Year_PPTDist_Recent, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3,linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Recent Water Year PPT Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'Wtr_Year_PPTDist_Recent'.
## You can override using the `.groups` argument.
```

``` r
GD_prob <- ucd_surv_to_rep_sub_dist %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, GrwSsn_PPTDist_Recent, Wtr_Year_PPTDist_Recent, Geographic_Dist) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Geographic_Dist, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Geographic Distance (m)") +
  theme(text=element_text(size=30), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_PPTDist_Recent',
## 'Wtr_Year_PPTDist_Recent'. You can override using the `.groups` argument.
```

``` r
ED_prob <- ucd_surv_to_rep_sub_dist %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, Elev_Dist) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Elev_Dist, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Elevation Distance (m)") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m'. You can override using the
## `.groups` argument.
```

``` r
UCD_establishment_sub_FIG_prob_recent <- ggarrange(GSCD_prob_recent, WYCD_prob_recent, GD_prob, ED_prob, ncol=2, nrow=2) 
ggsave("../output/UCD_Traits/UCD_SurvtoRep_Y1_PPTSubDist_SCATTERS_Recent.png", width = 24, height = 18, units = "in")
```


``` r
#scatter plots - historic
GSCD_prob_historic <- ucd_surv_to_rep_sub_dist %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, GrwSsn_PPTDist_Recent, GrwSsn_PPTDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=GrwSsn_PPTDist_Historic, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Historic Growth Season PPT Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_PPTDist_Recent'.
## You can override using the `.groups` argument.
```

``` r
WYCD_prob_historic <- ucd_surv_to_rep_sub_dist %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  group_by(pop, elev_m, Wtr_Year_PPTDist_Recent, Wtr_Year_PPTDist_Historic) %>% 
  summarise(meanEst=mean(SurvtoRep_Y1, na.rm = TRUE), semEst=sem(SurvtoRep_Y1, na.rm=TRUE)) %>% 
  ggplot(aes(x=Wtr_Year_PPTDist_Historic, y=meanEst, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanEst-semEst,ymax=meanEst+semEst),width=.3,linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="SurvtoRep_Y1", x="Historic Water Year PPT Dist") +
  theme(text=element_text(size=30))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'Wtr_Year_PPTDist_Recent'.
## You can override using the `.groups` argument.
```

``` r
UCD_establishment_sub_FIG_prob_historic <- ggarrange(GSCD_prob_historic, WYCD_prob_historic, GD_prob, ED_prob, ncol=2, nrow=2) 
ggsave("../output/UCD_Traits/UCD_SurvtoRep_Y1_PPTSubDist_SCATTERS_Historic.png", width = 24, height = 18, units = "in")
```

## Stats

No stats for WL2 since only 2 pops with surv (model possible, but wouldn't yield interesting info)

### Scaling 

``` r
 ucd_surv_to_rep %>% 
  group_by(pop) %>% 
  summarise(n=n()) %>% 
   arrange(n) #smallest N=2
```

```
## # A tibble: 23 × 2
##    pop       n
##    <chr> <int>
##  1 WV        2
##  2 LV1       3
##  3 YO4       5
##  4 CP3       6
##  5 SQ3       9
##  6 WR        9
##  7 LVTR1    13
##  8 YO11     13
##  9 YO8      13
## 10 YO7      16
## # ℹ 13 more rows
```

``` r
ucd_surv_to_rep_scaled <- ucd_surv_to_rep %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  mutate_at(c("GrwSsn_GD_Recent","Wtr_Year_GD_Recent",                                                           "GrwSsn_GD_Historical","Wtr_Year_GD_Historical","Geographic_Dist"),
                                                            scale) 


 #CHECK MODEL PARAMS
unique(ucd_surv_to_rep_scaled$pop)
```

```
##  [1] "WL2"   "CP2"   "YO11"  "CC"    "FR"    "BH"    "IH"    "LV3"   "SC"   
## [10] "LVTR1" "SQ3"   "TM2"   "WL1"   "YO7"   "DPR"   "SQ2"   "SQ1"   "YO8"  
## [19] "YO4"   "WR"    "CP3"   "LV1"
```

``` r
unique(ucd_surv_to_rep_scaled$mf)
```

```
##  [1]  4 10  5  3  6  1  8  7  2  9 12
```

``` r
unique(ucd_surv_to_rep_scaled$block)
```

```
##  [1] "D1" "D2" "F1" "F2" "H1" "H2" "J1" "J2" "L1" "L2"
```

``` r
summary(ucd_surv_to_rep_scaled)
```

```
##     block                row           col              Genotype        
##  Length:734         Min.   : 3.0   Length:734         Length:734        
##  Class :character   1st Qu.:12.0   Class :character   Class :character  
##  Mode  :character   Median :22.0   Mode  :character   Mode  :character  
##                     Mean   :21.9                                        
##                     3rd Qu.:32.0                                        
##                     Max.   :42.0                                        
##                                                                         
##      pop                  mf              rep          elevation.group   
##  Length:734         Min.   : 1.000   Min.   :  1.000   Length:734        
##  Class :character   1st Qu.: 2.000   1st Qu.:  4.000   Class :character  
##  Mode  :character   Median : 4.000   Median :  7.500   Mode  :character  
##                     Mean   : 4.343   Mean   :  8.606                     
##                     3rd Qu.: 6.000   3rd Qu.: 12.000                     
##                     Max.   :12.000   Max.   :100.000                     
##                                                                          
##      elev_m            Lat             Long        GrwSsn_GD_Recent.V1 
##  Min.   : 313.0   Min.   :36.56   Min.   :-121.6   Min.   :-1.1685201  
##  1st Qu.: 511.4   1st Qu.:37.41   1st Qu.:-121.2   1st Qu.:-0.8455109  
##  Median :1613.8   Median :38.79   Median :-120.2   Median :-0.2841470  
##  Mean   :1350.9   Mean   :38.64   Mean   :-120.4   Mean   : 0.0000000  
##  3rd Qu.:2020.1   3rd Qu.:39.59   3rd Qu.:-120.0   3rd Qu.: 0.6530518  
##  Max.   :2872.3   Max.   :40.48   Max.   :-118.8   Max.   : 2.5753028  
##                                                                        
##  GrwSsn_GD_Historical.V1 Wtr_Year_GD_Recent.V1 Wtr_Year_GD_Historical.V1
##  Min.   :-0.9759097      Min.   :-1.3490667    Min.   :-1.2100890       
##  1st Qu.:-0.8861133      1st Qu.:-0.8908981    1st Qu.:-1.0183353       
##  Median :-0.3788771      Median : 0.0672353    Median : 0.1248113       
##  Mean   : 0.0000000      Mean   : 0.0000000    Mean   : 0.0000000       
##  3rd Qu.: 0.2482141      3rd Qu.: 0.7704044    3rd Qu.: 0.7315006       
##  Max.   : 2.2724861      Max.   : 2.0517185    Max.   : 2.0674083       
##                                                                         
##   Geographic_Dist.V1    Elev_Dist        bud.date           death.date        
##  Min.   :-1.2302781   Min.   : 297.0   Length:734         Min.   :2023-01-06  
##  1st Qu.:-0.8110917   1st Qu.: 495.4   Class :character   1st Qu.:2023-02-10  
##  Median :-0.4717116   Median :1597.8   Mode  :character   Median :2023-03-03  
##  Mean   : 0.0000000   Mean   :1334.9                      Mean   :2023-03-09  
##  3rd Qu.: 0.4809988   3rd Qu.:2004.1                      3rd Qu.:2023-03-24  
##  Max.   : 2.4276553   Max.   :2856.3                      Max.   :2023-10-16  
##                                                           NA's   :9           
##   SurvtoRep_Y1    
##  Min.   :0.00000  
##  1st Qu.:0.00000  
##  Median :0.00000  
##  Mean   :0.06812  
##  3rd Qu.:0.00000  
##  Max.   :1.00000  
## 
```

``` r
names(ucd_surv_to_rep_scaled)
```

```
##  [1] "block"                  "row"                    "col"                   
##  [4] "Genotype"               "pop"                    "mf"                    
##  [7] "rep"                    "elevation.group"        "elev_m"                
## [10] "Lat"                    "Long"                   "GrwSsn_GD_Recent"      
## [13] "GrwSsn_GD_Historical"   "Wtr_Year_GD_Recent"     "Wtr_Year_GD_Historical"
## [16] "Geographic_Dist"        "Elev_Dist"              "bud.date"              
## [19] "death.date"             "SurvtoRep_Y1"
```


``` r
ucd_surv_to_rep_scaled_sub <- ucd_surv_to_rep_sub_dist %>% 
  filter(pop!="WV") %>% #Only 2 indivs for WV
  mutate_at(c("Wtr_Year_TempDist_Recent",  "Wtr_Year_PPTDist_Recent", 
                 "Wtr_Year_TempDist_Historic", "Wtr_Year_PPTDist_Historic",
                 "GrwSsn_TempDist_Recent", "GrwSsn_PPTDist_Recent",
                 "GrwSsn_TempDist_Historic", "GrwSsn_PPTDist_Historic",
              "Geographic_Dist"), scale)
```


### Basic Model Workflow 

``` r
glmer.model_binomial <- 
  linear_reg() %>% 
  set_engine("glmer", family=binomial)

surv_wflow <- workflow() %>% 
  add_variables(outcomes = SurvtoRep_Y1, predictors = c(pop, mf, block))

surv_fits <- tibble(wflow=list(
  pop = {surv_wflow %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_Y1 ~ (1|pop))},
  
  pop.mf = {surv_wflow %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_Y1 ~ (1|pop/mf))},
  
  pop.block = {surv_wflow %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_Y1 ~ (1|pop) + (1|block))},
  
  pop.mf.block = {surv_wflow %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_Y1 ~ (1|pop/mf) + (1|block))}
),
name=names(wflow)
) %>% 
  select(name,wflow)

surv_fits_ucd <- surv_fits %>%
  mutate(fit = map(wflow, fit, data = ucd_surv_to_rep_scaled))
#mod_test <- glmer(SurvtoRep_Y1 ~ (1|pop/mf) + (1|block), data=ucd_surv_to_rep_scaled, family=binomial)
#summary(mod_test)

surv_fits_ucd %>% mutate(glance=map(fit, glance)) %>% unnest(glance) %>% arrange(AIC) %>% select(-wflow:-sigma)
```

```
## # A tibble: 4 × 6
##   name         logLik   AIC   BIC deviance df.residual
##   <chr>         <dbl> <dbl> <dbl>    <dbl>       <int>
## 1 pop.block     -140.  286.  300.     230.         731
## 2 pop           -141.  286.  295.     244.         732
## 3 pop.mf.block  -139.  287.  305.     219.         730
## 4 pop.mf        -141.  287.  301.     232.         731
```

``` r
#pop.block model best by AIC and BIC, but no issues with full model 
```


``` r
surv_GD_wflow_ucd <- workflow() %>%
  add_variables(outcomes = SurvtoRep_Y1, predictors = c(pop, mf, block, contains("GD"), Geographic_Dist)) 

surv_GD_fits_ucd <- tibble(wflow=list(
  pop.block = {surv_GD_wflow_ucd %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_Y1 ~ (1|pop/mf) + (1|block))},
  
  GS_Recent = {surv_GD_wflow_ucd %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_Y1 ~ GrwSsn_GD_Recent + Geographic_Dist + (1|pop/mf) + (1|block))},
  
  GS_Historical = {surv_GD_wflow_ucd %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_Y1 ~ GrwSsn_GD_Historical + Geographic_Dist + (1|pop/mf) + (1|block))},
  
  WY_Recent = {surv_GD_wflow_ucd %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_Y1 ~ Wtr_Year_GD_Recent + Geographic_Dist + (1|pop/mf) + (1|block))},
  
  WY_Historical = {surv_GD_wflow_ucd %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_Y1 ~ Wtr_Year_GD_Historical + Geographic_Dist + (1|pop/mf) + (1|block))}
  
),
name=names(wflow)
) %>% 
  select(name,wflow) %>%
  mutate(fit = map(wflow, fit, data = ucd_surv_to_rep_scaled))

surv_GD_fits_ucd %>% mutate(glance=map(fit, glance)) %>% unnest(glance) %>% arrange(AIC) %>% select(-wflow:-sigma)
```

```
## # A tibble: 5 × 6
##   name          logLik   AIC   BIC deviance df.residual
##   <chr>          <dbl> <dbl> <dbl>    <dbl>       <int>
## 1 WY_Historical  -136.  284.  311.     220.         728
## 2 WY_Recent      -136.  284.  312.     220.         728
## 3 pop.block      -139.  287.  305.     219.         730
## 4 GS_Historical  -138.  288.  316.     219.         728
## 5 GS_Recent      -138.  288.  316.     219.         728
```

``` r
#water year models the best 

surv_GD_fits_ucd %>% mutate(tidy=map(fit, tidy)) %>% unnest(tidy) %>%
  filter(str_detect(term, "GD") | term=="Geographic_Dist") %>%
  drop_na(p.value) %>%
  select(-wflow:-group)# %>%
```

```
## # A tibble: 8 × 6
##   name          term                   estimate std.error statistic p.value
##   <chr>         <chr>                     <dbl>     <dbl>     <dbl>   <dbl>
## 1 GS_Recent     GrwSsn_GD_Recent          0.152     0.645     0.236  0.813 
## 2 GS_Recent     Geographic_Dist          -0.927     0.632    -1.47   0.143 
## 3 GS_Historical GrwSsn_GD_Historical     -0.538     0.768    -0.701  0.483 
## 4 GS_Historical Geographic_Dist          -0.640     0.646    -0.991  0.322 
## 5 WY_Recent     Wtr_Year_GD_Recent       -1.30      0.668    -1.94   0.0523
## 6 WY_Recent     Geographic_Dist          -0.437     0.539    -0.811  0.418 
## 7 WY_Historical Wtr_Year_GD_Historical   -1.39      0.676    -2.06   0.0396
## 8 WY_Historical Geographic_Dist          -0.296     0.545    -0.543  0.587
```

``` r
#  arrange(p.value)

#mod_test <- glmer(SurvtoRep_Y1 ~ GrwSsn_GD_Historical + Geographic_Dist + (1|pop/mf) + (1|block), data=ucd_surv_to_rep_scaled, family=binomial)
#summary(mod_test)
```


``` r
surv_GD_wflow_ucd_sub <- workflow() %>%
  add_variables(outcomes = SurvtoRep_Y1, predictors = c(pop, mf, block, contains("Dist"))) 

surv_GD_fits_ucd_sub <- tibble(wflow=list(
  pop.block = {surv_GD_wflow_ucd_sub %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_Y1 ~ (1|pop/mf) + (1|block))},
  
  GS_Recent = {surv_GD_wflow_ucd_sub %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_Y1 ~ GrwSsn_TempDist_Recent + GrwSsn_PPTDist_Recent + Geographic_Dist + (1|pop/mf) + (1|block))},
  
  GS_Historical = {surv_GD_wflow_ucd_sub %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_Y1 ~ GrwSsn_TempDist_Historic + GrwSsn_PPTDist_Historic + Geographic_Dist + (1|pop/mf) + (1|block))},
  
  WY_Recent = {surv_GD_wflow_ucd_sub %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_Y1 ~ Wtr_Year_TempDist_Recent + Wtr_Year_PPTDist_Recent + Geographic_Dist + (1|pop/mf) + (1|block))},
  
  WY_Historical = {surv_GD_wflow_ucd_sub %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_Y1 ~ Wtr_Year_TempDist_Historic + Wtr_Year_PPTDist_Historic + Geographic_Dist + (1|pop/mf) + (1|block))}
  
),
name=names(wflow)
) %>% 
  select(name,wflow) %>%
  mutate(fit = map(wflow, fit, data = ucd_surv_to_rep_scaled_sub))

surv_GD_fits_ucd_sub %>% mutate(glance=map(fit, glance)) %>% unnest(glance) %>% arrange(AIC) %>% select(-wflow:-sigma)
```

```
## # A tibble: 5 × 6
##   name          logLik   AIC   BIC deviance df.residual
##   <chr>          <dbl> <dbl> <dbl>    <dbl>       <int>
## 1 WY_Recent      -135.  284.  316.     222.         727
## 2 WY_Historical  -135.  284.  316.     222.         727
## 3 GS_Recent      -136.  287.  319.     220.         727
## 4 pop.block      -139.  287.  305.     219.         730
## 5 GS_Historical  -137.  287.  319.     220.         727
```

``` r
#water year models the best 

surv_GD_fits_ucd_sub %>% mutate(tidy=map(fit, tidy)) %>% unnest(tidy) %>%
  filter(str_detect(term, "Dist")) %>%
  drop_na(p.value) %>%
  select(-wflow:-group)# %>%
```

```
## # A tibble: 12 × 6
##    name          term                       estimate std.error statistic p.value
##    <chr>         <chr>                         <dbl>     <dbl>     <dbl>   <dbl>
##  1 GS_Recent     GrwSsn_TempDist_Recent      -0.120      0.628   -0.192   0.848 
##  2 GS_Recent     GrwSsn_PPTDist_Recent        1.36       0.784    1.74    0.0826
##  3 GS_Recent     Geographic_Dist              0.219      0.694    0.316   0.752 
##  4 GS_Historical GrwSsn_TempDist_Historic     0.355      0.620    0.573   0.567 
##  5 GS_Historical GrwSsn_PPTDist_Historic      1.03       0.651    1.57    0.116 
##  6 GS_Historical Geographic_Dist              0.0764     0.672    0.114   0.909 
##  7 WY_Recent     Wtr_Year_TempDist_Recent     1.62       0.866    1.87    0.0610
##  8 WY_Recent     Wtr_Year_PPTDist_Recent      0.0852     0.801    0.106   0.915 
##  9 WY_Recent     Geographic_Dist              0.0462     0.674    0.0686  0.945 
## 10 WY_Historical Wtr_Year_TempDist_Historic   1.74       0.923    1.88    0.0598
## 11 WY_Historical Wtr_Year_PPTDist_Historic    0.211      0.823    0.257   0.797 
## 12 WY_Historical Geographic_Dist              0.125      0.661    0.190   0.850
```

``` r
#  arrange(p.value)
```

