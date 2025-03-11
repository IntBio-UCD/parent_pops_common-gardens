---
title: "Fruits_Y1"
author: "Brandie QC"
date: "2025-03-11"
output: 
  html_document: 
    keep_md: true
---



# Year 1 Fruit Production

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
## • Learn how to get started at https://www.tidymodels.org/start/
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

## Need the UCD 2023 and WL2 2023 & 2024 annual census data sheets (for fruit number)

``` r
ucd_ann_cens <- read_csv("../output/UCD_Traits/UCD2023_Annual_Census_Combined.csv") %>%  #note this is only for plants with annual census data 
  rename(pop=parent.pop) %>% 
  filter(!is.na(pop)) %>% 
  filter(rep != 100) %>% #get rid of individuals that germinated in the field 
  unite(Genotype, pop:rep, sep="_", remove = FALSE) 
```

```
## Rows: 63 Columns: 20
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (4): block, col, parent.pop, elevation.group
## dbl (16): row, mf, rep, diam, height, total_branch, longest_leaf, flowers, f...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

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
         Elev_Dist=UCD_Elev-elev_m) %>% # Calculate the distance using the haversine formula (dist in meters)
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
         Elev_Dist=WL2_Elev-elev_m) %>% # Calculate the distance using the haversine formula
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

## Davis

``` r
ucd_fruits <- ucd_ann_cens %>% select(block:rep, elevation.group, elev_m, Lat:Long, flowers, fruits) %>%  #note this is only for plants that survived to rep 
  mutate(FrFlN=fruits+flowers) %>% 
  left_join(ucd_gowers)
```

```
## Joining with `by = join_by(pop, elevation.group, elev_m, Lat, Long)`
```

``` r
ucd_fruits %>% group_by(pop) %>% summarise(n=n()) %>% arrange(n) #SQ3 & WL1 only 1 individual 
```

```
## # A tibble: 10 × 2
##    pop       n
##    <chr> <int>
##  1 SQ3       1
##  2 WL1       1
##  3 DPR       2
##  4 YO7       2
##  5 CP2       3
##  6 WL2       3
##  7 IH        4
##  8 SC        4
##  9 TM2      12
## 10 BH       31
```

``` r
ucd_fruits %>% filter(FrFlN>0) %>% arrange(pop) #DPR 2 indivs, BH and TM2 lots, rest only 1 indiv ...
```

```
## # A tibble: 33 × 23
##    block   row col   Genotype pop      mf   rep elevation.group elev_m   Lat
##    <chr> <dbl> <chr> <chr>    <chr> <dbl> <dbl> <chr>            <dbl> <dbl>
##  1 D2       26 B     BH_5_15  BH        5    15 Low               511.  37.4
##  2 D2       29 D     BH_2_9   BH        2     9 Low               511.  37.4
##  3 D2       35 D     BH_3_6   BH        3     6 Low               511.  37.4
##  4 F2       35 D     BH_2_1   BH        2     1 Low               511.  37.4
##  5 H2       24 B     BH_5_29  BH        5    29 Low               511.  37.4
##  6 J1       10 C     BH_3_1   BH        3     1 Low               511.  37.4
##  7 J1       19 C     BH_5_20  BH        5    20 Low               511.  37.4
##  8 J2       23 D     BH_4_9   BH        4     9 Low               511.  37.4
##  9 J2       25 D     BH_2_5   BH        2     5 Low               511.  37.4
## 10 J2       37 B     BH_1_7   BH        1     7 Low               511.  37.4
## # ℹ 23 more rows
## # ℹ 13 more variables: Long <dbl>, flowers <dbl>, fruits <dbl>, FrFlN <dbl>,
## #   GrwSsn_GD_Recent <dbl>, GrwSsn_GD_Historical <dbl>,
## #   Wtr_Year_GD_Recent <dbl>, Wtr_Year_GD_Historical <dbl>, UCD_Lat <dbl>,
## #   UCD_Long <dbl>, UCD_Elev <dbl>, Geographic_Dist <dbl>, Elev_Dist <dbl>
```

``` r
ucd_fruits %>% filter(fruits>0) %>% arrange(pop) #only BH and TM2 with greater than 1 indiv 
```

```
## # A tibble: 30 × 23
##    block   row col   Genotype pop      mf   rep elevation.group elev_m   Lat
##    <chr> <dbl> <chr> <chr>    <chr> <dbl> <dbl> <chr>            <dbl> <dbl>
##  1 D2       26 B     BH_5_15  BH        5    15 Low               511.  37.4
##  2 D2       29 D     BH_2_9   BH        2     9 Low               511.  37.4
##  3 D2       35 D     BH_3_6   BH        3     6 Low               511.  37.4
##  4 F2       35 D     BH_2_1   BH        2     1 Low               511.  37.4
##  5 H2       24 B     BH_5_29  BH        5    29 Low               511.  37.4
##  6 J1       10 C     BH_3_1   BH        3     1 Low               511.  37.4
##  7 J1       19 C     BH_5_20  BH        5    20 Low               511.  37.4
##  8 J2       23 D     BH_4_9   BH        4     9 Low               511.  37.4
##  9 J2       25 D     BH_2_5   BH        2     5 Low               511.  37.4
## 10 J2       37 B     BH_1_7   BH        1     7 Low               511.  37.4
## # ℹ 20 more rows
## # ℹ 13 more variables: Long <dbl>, flowers <dbl>, fruits <dbl>, FrFlN <dbl>,
## #   GrwSsn_GD_Recent <dbl>, GrwSsn_GD_Historical <dbl>,
## #   Wtr_Year_GD_Recent <dbl>, Wtr_Year_GD_Historical <dbl>, UCD_Lat <dbl>,
## #   UCD_Long <dbl>, UCD_Elev <dbl>, Geographic_Dist <dbl>, Elev_Dist <dbl>
```

``` r
#write_csv(ucd_fruits, "../output/UCD_Traits/UCD_Fruits.csv")
```

### Bar Plots 

``` r
ucd_fruits %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanFruits=mean(fruits, na.rm = TRUE), semFruits=sem(fruits, na.rm=TRUE)) %>% 
  ggplot(aes(x=fct_reorder(pop, meanFruits), y=meanFruits, fill=GrwSsn_GD_Recent)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=meanFruits-semFruits,ymax=meanFruits+semFruits),width=.2, position = 
                  position_dodge(0.75)) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="Fruit Number", x="Parent Population", fill="Growth Season Gowers") +
  scale_fill_viridis(option="mako", direction = -1) +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

![](Fruits_Y1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

``` r
#ggsave("../output/UCD_Traits/UCD_Fruits_GrwSsn_GD_Recent.png", width = 12, height = 8, units = "in")

ucd_fruits %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanFruits=mean(fruits, na.rm = TRUE), semFruits=sem(fruits, na.rm=TRUE)) %>% 
  ggplot(aes(x=fct_reorder(pop, meanFruits), y=meanFruits, fill=Wtr_Year_GD_Recent)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=meanFruits-semFruits,ymax=meanFruits+semFruits),width=.2, position = 
                  position_dodge(0.75)) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="Fruit Number", x="Parent Population", fill="Water Year Gowers") +
  scale_fill_viridis(option="mako", direction = -1) +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

![](Fruits_Y1_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

``` r
#ggsave("../output/UCD_Traits/UCD_Fruits_Wtr_Year_GD_Recent.png", width = 12, height = 8, units = "in")
```

## WL2

``` r
wl2_fruits_y1 <- wl2_ann_cens %>% select(block:rep, flowers=num.flw, fruits=num.fruit) %>% 
  mutate(FrFlN=fruits+flowers) %>% 
  left_join(wl2_gowers_2023)
```

```
## Joining with `by = join_by(pop)`
```

``` r
summary(wl2_fruits_y1)
```

```
##     block              BedLoc              bed               bed-row     
##  Length:1573        Length:1573        Length:1573        Min.   : 1.00  
##  Class :character   Class :character   Class :character   1st Qu.:12.00  
##  Mode  :character   Mode  :character   Mode  :character   Median :23.00  
##                                                           Mean   :25.31  
##                                                           3rd Qu.:39.00  
##                                                           Max.   :59.00  
##                                                                          
##    bed-col            Genotype             pop                 mf           
##  Length:1573        Length:1573        Length:1573        Length:1573       
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##      rep               flowers            fruits           FrFlN        
##  Length:1573        Min.   : 0.0000   Min.   : 0.000   Min.   : 0.0000  
##  Class :character   1st Qu.: 0.0000   1st Qu.: 0.000   1st Qu.: 0.0000  
##  Mode  :character   Median : 0.0000   Median : 0.000   Median : 0.0000  
##                     Mean   : 0.1681   Mean   : 0.306   Mean   : 0.4752  
##                     3rd Qu.: 0.0000   3rd Qu.: 0.000   3rd Qu.: 0.0000  
##                     Max.   :20.0000   Max.   :22.000   Max.   :35.0000  
##                     NA's   :1109      NA's   :1109     NA's   :1110     
##  elevation.group        elev_m            Lat             Long       
##  Length:1573        Min.   : 313.0   Min.   :36.56   Min.   :-123.0  
##  Class :character   1st Qu.: 511.4   1st Qu.:37.81   1st Qu.:-121.2  
##  Mode  :character   Median :2020.1   Median :38.71   Median :-120.3  
##                     Mean   :1666.3   Mean   :38.72   Mean   :-120.4  
##                     3rd Qu.:2470.0   3rd Qu.:39.59   3rd Qu.:-119.6  
##                     Max.   :2872.3   Max.   :40.74   Max.   :-118.8  
##                                                                      
##  GrwSsn_GD_Recent GrwSsn_GD_Historical Wtr_Year_GD_Recent
##  Min.   :0.2165   Min.   :0.2154       Min.   :0.3641    
##  1st Qu.:0.2871   1st Qu.:0.2609       1st Qu.:0.3922    
##  Median :0.3595   Median :0.3419       Median :0.4233    
##  Mean   :0.3439   Mean   :0.3454       Mean   :0.4724    
##  3rd Qu.:0.4089   3rd Qu.:0.4148       3rd Qu.:0.5594    
##  Max.   :0.4460   Max.   :0.5230       Max.   :0.7142    
##                                                          
##  Wtr_Year_GD_Historical    WL2_Lat         WL2_Long         WL2_Elev   
##  Min.   :0.4075         Min.   :38.83   Min.   :-120.3   Min.   :2020  
##  1st Qu.:0.4252         1st Qu.:38.83   1st Qu.:-120.3   1st Qu.:2020  
##  Median :0.4629         Median :38.83   Median :-120.3   Median :2020  
##  Mean   :0.4921         Mean   :38.83   Mean   :-120.3   Mean   :2020  
##  3rd Qu.:0.5537         3rd Qu.:38.83   3rd Qu.:-120.3   3rd Qu.:2020  
##  Max.   :0.6473         Max.   :38.83   Max.   :-120.3   Max.   :2020  
##                                                                        
##  Geographic_Dist      Elev_Dist        
##  Min.   :   136.3   Min.   :-852.2950  
##  1st Qu.: 62498.9   1st Qu.:-449.9787  
##  Median :131232.0   Median :  -0.1158  
##  Mean   :119848.0   Mean   : 353.6799  
##  3rd Qu.:159625.6   3rd Qu.:1508.5706  
##  Max.   :317600.4   Max.   :1707.0000  
## 
```

``` r
wl2_fruits_y1 %>% group_by(pop) %>% summarise(n=n()) %>% arrange(n)
```

```
## # A tibble: 23 × 2
##    pop       n
##    <chr> <int>
##  1 WV        3
##  2 WR       14
##  3 LV3      27
##  4 SQ1      30
##  5 SQ3      33
##  6 YO4      40
##  7 FR       48
##  8 WL1      48
##  9 SQ2      61
## 10 TM2      84
## # ℹ 13 more rows
```

``` r
wl2_fruits_y1 %>% filter(fruits>0) #all TM2
```

```
## # A tibble: 25 × 25
##    block BedLoc bed   `bed-row` `bed-col` Genotype pop   mf    rep   flowers
##    <chr> <chr>  <chr>     <dbl> <chr>     <chr>    <chr> <chr> <chr>   <dbl>
##  1 A     A_1_A  A             1 A         TM2_6_11 TM2   6     11          0
##  2 B     A_43_A A            43 A         TM2_6_1  TM2   6     1           0
##  3 B     A_56_B A            56 B         TM2_2_12 TM2   2     12          0
##  4 A     A_23_D A            23 D         TM2_3_3  TM2   3     3           1
##  5 B     A_44_C A            44 C         TM2_6_3  TM2   6     3           1
##  6 C     B_37_A B            37 A         TM2_1_16 TM2   1     16          5
##  7 C     B_36_D B            36 D         TM2_1_4  TM2   1     4           2
##  8 C     B_43_D B            43 D         TM2_7_10 TM2   7     10         20
##  9 F     C_54_B C            54 B         TM2_1_14 TM2   1     14          2
## 10 E     C_32_D C            32 D         TM2_7_12 TM2   7     12          3
## # ℹ 15 more rows
## # ℹ 15 more variables: fruits <dbl>, FrFlN <dbl>, elevation.group <chr>,
## #   elev_m <dbl>, Lat <dbl>, Long <dbl>, GrwSsn_GD_Recent <dbl>,
## #   GrwSsn_GD_Historical <dbl>, Wtr_Year_GD_Recent <dbl>,
## #   Wtr_Year_GD_Historical <dbl>, WL2_Lat <dbl>, WL2_Long <dbl>,
## #   WL2_Elev <dbl>, Geographic_Dist <dbl>, Elev_Dist <dbl>
```

``` r
wl2_fruits_y1 %>% filter(FrFlN>0) #all TM2
```

```
## # A tibble: 26 × 25
##    block BedLoc bed   `bed-row` `bed-col` Genotype pop   mf    rep   flowers
##    <chr> <chr>  <chr>     <dbl> <chr>     <chr>    <chr> <chr> <chr>   <dbl>
##  1 A     A_1_A  A             1 A         TM2_6_11 TM2   6     11          0
##  2 B     A_43_A A            43 A         TM2_6_1  TM2   6     1           0
##  3 B     A_46_B A            46 B         TM2_3_11 TM2   3     11          2
##  4 B     A_56_B A            56 B         TM2_2_12 TM2   2     12          0
##  5 A     A_23_D A            23 D         TM2_3_3  TM2   3     3           1
##  6 B     A_44_C A            44 C         TM2_6_3  TM2   6     3           1
##  7 C     B_37_A B            37 A         TM2_1_16 TM2   1     16          5
##  8 C     B_36_D B            36 D         TM2_1_4  TM2   1     4           2
##  9 C     B_43_D B            43 D         TM2_7_10 TM2   7     10         20
## 10 F     C_54_B C            54 B         TM2_1_14 TM2   1     14          2
## # ℹ 16 more rows
## # ℹ 15 more variables: fruits <dbl>, FrFlN <dbl>, elevation.group <chr>,
## #   elev_m <dbl>, Lat <dbl>, Long <dbl>, GrwSsn_GD_Recent <dbl>,
## #   GrwSsn_GD_Historical <dbl>, Wtr_Year_GD_Recent <dbl>,
## #   Wtr_Year_GD_Historical <dbl>, WL2_Lat <dbl>, WL2_Long <dbl>,
## #   WL2_Elev <dbl>, Geographic_Dist <dbl>, Elev_Dist <dbl>
```

``` r
#write_csv(wl2_fruits_y1, "../output/WL2_Traits/WL2_Fruits_Y1.csv")
```

### Bar Plots

``` r
wl2_fruits_y1 %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanFruits=mean(fruits, na.rm = TRUE), semFruits=sem(fruits, na.rm=TRUE)) %>%
  filter(pop!="WV") %>% 
  ggplot(aes(x=fct_reorder(pop, meanFruits), y=meanFruits, fill=GrwSsn_GD_Recent)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=meanFruits-semFruits,ymax=meanFruits+semFruits),width=.2, position = 
                  position_dodge(0.75)) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="Fruit Number", x="Parent Population", fill="Growth Season Gowers") +
  scale_fill_viridis(option="mako", direction = -1) +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

![](Fruits_Y1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

``` r
#ggsave("../output/WL2_Traits/WL2_FruitsY1_GrwSsn_GD_Recent.png", width = 12, height = 8, units = "in")

wl2_fruits_y1 %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanFruits=mean(fruits, na.rm = TRUE), semFruits=sem(fruits, na.rm=TRUE)) %>% 
  filter(pop!="WV") %>% 
  ggplot(aes(x=fct_reorder(pop, meanFruits), y=meanFruits, fill=Wtr_Year_GD_Recent)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin=meanFruits-semFruits,ymax=meanFruits+semFruits),width=.2, position = 
                  position_dodge(0.75)) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="Fruit Number", x="Parent Population", fill="Water Year Gowers") +
  scale_fill_viridis(option="mako", direction = -1) +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

![](Fruits_Y1_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

``` r
#ggsave("../output/WL2_Traits/WL2_FruitsY1_Wtr_Year_GD_Recent.png", width = 12, height = 8, units = "in")
```

## Scatterplots (didn't edit these due to low sample sizes, can just report means in text)
### Davis

``` r
#scatter plots
GSCD <- ucd_fruits %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanFruits=mean(fruits, na.rm = TRUE), semFruits=sem(fruits, na.rm=TRUE)) %>% 
  ggplot(aes(x=GrwSsn_GD_Recent, y=meanFruits, color=GrwSsn_GD_Recent, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanFruits-semFruits,ymax=meanFruits+semFruits),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="Fruit Number", x="Growth Season CD", color="Growth Season \n Climate Distance") +
  scale_color_viridis(option="mako", direction = -1) +
  theme(text=element_text(size=25))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

``` r
WYCD <- ucd_fruits %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanFruits=mean(fruits, na.rm = TRUE), semFruits=sem(fruits, na.rm=TRUE)) %>% 
  ggplot(aes(x=Wtr_Year_GD_Recent, y=meanFruits, color=Wtr_Year_GD_Recent, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanFruits-semFruits,ymax=meanFruits+semFruits),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="Fruit Number", x="Water Year CD", color="Water Year \n Climate Distance") +
  scale_color_viridis(option="mako", direction = -1) +
  theme(text=element_text(size=25))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

``` r
GD <- ucd_fruits %>% 
  group_by(pop, elev_m, Geographic_Dist) %>% 
  summarise(meanFruits=mean(fruits, na.rm = TRUE), semFruits=sem(fruits, na.rm=TRUE)) %>% 
  ggplot(aes(x=Geographic_Dist, y=meanFruits, color=Geographic_Dist, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanFruits-semFruits,ymax=meanFruits+semFruits),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="Fruit Number", x="Geographic Distance (m)", color="Geographic Distance") +
  scale_color_viridis(option="mako", direction = -1) +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m'. You can override using the
## `.groups` argument.
```

``` r
ucd_fruits_FIG <- ggarrange(GSCD, WYCD, GD, ncol=2, nrow=2) 
#ggsave("../output/UCD_Traits/UCD_fruits_SCATTERS.png", width = 24, height = 18, units = "in")
```

### WL2

``` r
#scatter plots
GSCD <- wl2_fruits_y1 %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanFruits=mean(fruits, na.rm = TRUE), semFruits=sem(fruits, na.rm=TRUE)) %>% 
  ggplot(aes(x=GrwSsn_GD_Recent, y=meanFruits, color=GrwSsn_GD_Recent, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanFruits-semFruits,ymax=meanFruits+semFruits),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="Fruit Number", x="Growth Season CD", color="Growth Season \n Climate Distance") +
  scale_color_viridis(option="mako", direction = -1) +
  theme(text=element_text(size=25))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

``` r
WYCD <- wl2_fruits_y1 %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent) %>% 
  summarise(meanFruits=mean(fruits, na.rm = TRUE), semFruits=sem(fruits, na.rm=TRUE)) %>% 
  ggplot(aes(x=Wtr_Year_GD_Recent, y=meanFruits, color=Wtr_Year_GD_Recent, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanFruits-semFruits,ymax=meanFruits+semFruits),width=.02,linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="Fruit Number", x="Water Year CD", color="Water Year \n Climate Distance") +
  scale_color_viridis(option="mako", direction = -1) +
  theme(text=element_text(size=25))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent'. You
## can override using the `.groups` argument.
```

``` r
GD <- wl2_fruits_y1 %>% 
  group_by(pop, elev_m, GrwSsn_GD_Recent, Wtr_Year_GD_Recent, Geographic_Dist) %>% 
  summarise(meanFruits=mean(fruits, na.rm = TRUE), semFruits=sem(fruits, na.rm=TRUE)) %>% 
  ggplot(aes(x=Geographic_Dist, y=meanFruits, color=Geographic_Dist, group = pop)) +
  geom_point(size=6) + 
  geom_errorbar(aes(ymin=meanFruits-semFruits,ymax=meanFruits+semFruits),width=.02, linewidth = 2) +
  theme_classic() + 
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(y="Fruit Number", x="Geographic Distance (m)", color="Geographic Distance") +
  scale_color_viridis(option="mako", direction = -1) +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

```
## `summarise()` has grouped output by 'pop', 'elev_m', 'GrwSsn_GD_Recent',
## 'Wtr_Year_GD_Recent'. You can override using the `.groups` argument.
```

``` r
wl2_fruits_y1_FIG <- ggarrange(GSCD, WYCD, GD, ncol=2, nrow=2) 
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
## Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
## Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
```

``` r
#ggsave("../output/WL2_Traits/WL2_fruits_y1_SCATTERS.png", width = 24, height = 18, units = "in")
```

## No Stats due to low sample sizes (within pops)
