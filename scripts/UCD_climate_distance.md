---
title: "UCD Climate Distance"
author: "Brandie Quarles"
date: "2024-05-03"
output: 
  html_document: 
    keep_md: yes
editor_options: 
  markdown: 
    wrap: 72
---



# Climate Distance at the Davis Garden

Options:

-   Basic subtraction as in Moran et al 2017 (garden site - home site)

-   Gower's environmental distance metric as in Rutter and Fenster 2007

-   From PCAs "We calculated an overall environmental distance using
    PCA, with EDij corresponding to the Euclidian distance between i and
    j based on PCA1 & PCA2." as in Moran et al 2017

Davis CWD

Evapotranspiration can be computed as reference, potential, or actual
evapotranspiration.   Reference evapotranspiration is that from a grass
surface that is well-watered.  Potential evapotranspiration is that from
a surface that has unlimited water (such as a lake). 

-   CWD = PET - AET

    -   PET = modeled on an hourly basis from solar radiation that is
        modeled using topgraphic shading and cloudiness

        -   Priestley-Taylor Equation

        -   Still figuring out how to calculate this. The people we
            reached out to say Eto(ref) is similar to PET, but not quite
            the same...

    -   AET = PET x monthly Kv for each vegtype when soil is above
        wilting point

        -   Kv values used by flint for different veg types is in the
            control file - can't access the control file because they're
            currently revising the data release

## Relevant Libraries and Functions


```r
library(raster)
```

```
## Loading required package: sp
```

```r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.3     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.0
## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
## ✔ purrr     1.0.2
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ tidyr::extract() masks raster::extract()
## ✖ dplyr::filter()  masks stats::filter()
## ✖ dplyr::lag()     masks stats::lag()
## ✖ dplyr::select()  masks raster::select()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(conflicted)
conflicts_prefer(dplyr::select())
```

```
## [conflicted] Will prefer dplyr::select over any other package.
```

```r
conflicts_prefer(dplyr::filter)
```

```
## [conflicted] Will prefer dplyr::filter over any other package.
```

```r
library(ggrepel)
library(cowplot)
library(gridExtra)
library(corrplot) #plotting correlations 
```

```
## corrplot 0.92 loaded
```

```r
library(rstatix) #performing cor_test
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

elev_three_palette <- c("#0043F0", "#C9727F", "#F5A540") #colors from Gremer et al 2019
elev_order <- c("High", "Mid", "Low")

#For scree plots 
#library("devtools") #The package devtools is required for the installation as factoextra is hosted on github.
#1install_github("kassambara/factoextra")
library("factoextra")
```

```
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
```

## Home Climates

### Flint 

```r
pops_flint_recent_avgs <- read_csv("../output/Climate/Pops_FlintAvgs_Recent.csv") %>% 
  select(parent.pop:tmx_mean)
```

```
## Rows: 23 Columns: 13
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (11): elev_m, cwd_mean, pck_mean, ppt_mean, tmn_mean, tmx_mean, cwd_sem,...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
head(pops_flint_recent_avgs)
```

```
## # A tibble: 6 × 8
##   parent.pop elevation.group elev_m cwd_mean pck_mean ppt_mean tmn_mean tmx_mean
##   <chr>      <chr>            <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
## 1 BH         Low               511.     50.2     0        71.2     6.13     19.6
## 2 CC         Low               313      40.7     0       120.      7.40     19.3
## 3 CP2        High             2244.     74.9    40.3      77.0     3.79     16.5
## 4 CP3        High             2266.     57.7    42.6      74.3     3.22     15.7
## 5 DPR        Mid              1019.     30.3     2.89     96.4     8.97     21.9
## 6 FR         Mid               787      66.7     4.87     56.2     6.97     22.8
```

```r
pops_flint_historic_avgs <- read_csv("../output/Climate/Pops_FlintAvgs_Historic.csv") %>% 
  select(parent.pop:tmx_mean)
```

```
## Rows: 23 Columns: 13
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (11): elev_m, cwd_mean, pck_mean, ppt_mean, tmn_mean, tmx_mean, cwd_sem,...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
head(pops_flint_historic_avgs)
```

```
## # A tibble: 6 × 8
##   parent.pop elevation.group elev_m cwd_mean pck_mean ppt_mean tmn_mean tmx_mean
##   <chr>      <chr>            <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
## 1 BH         Low               511.     50.4   0.0292     66.8     4.99     18.8
## 2 CC         Low               313      49.0   0.106      99.1     7.41     20.6
## 3 CP2        High             2244.     79.9  19.4        72.2     3.92     17.6
## 4 CP3        High             2266.     60.4  21.8        70.2     3.49     16.8
## 5 DPR        Mid              1019.     29.8   4.69       84.3     7.96     22.2
## 6 FR         Mid               787      65.4   4.96       57.6     5.72     22.8
```

```r
names(pops_flint_historic_avgs)
```

```
## [1] "parent.pop"      "elevation.group" "elev_m"          "cwd_mean"       
## [5] "pck_mean"        "ppt_mean"        "tmn_mean"        "tmx_mean"
```

### BioClim

```r
pops_bioclim_recent_avgs <- read_csv("../output/Climate/Pops_BioClimAvgs_Recent.csv") %>% 
  select(parent.pop:ppt_coldest_month_avg)
```

```
## Rows: 23 Columns: 23
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (21): elev_m, ann_tmean_avg, mean_diurnal_range_avg, temp_seasonality_av...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
head(pops_bioclim_recent_avgs)
```

```
## # A tibble: 6 × 13
##   parent.pop elevation.group elev_m ann_tmean_avg mean_diurnal_range_avg
##   <chr>      <chr>            <dbl>         <dbl>                  <dbl>
## 1 BH         Low               511.         12.9                    13.5
## 2 CC         Low               313          13.3                    11.9
## 3 CP2        High             2244.         10.1                    12.7
## 4 CP3        High             2266.          9.46                   12.5
## 5 DPR        Mid              1019.         15.4                    13.0
## 6 FR         Mid               787          14.9                    15.8
## # ℹ 8 more variables: temp_seasonality_avg <dbl>, temp_ann_range_avg <dbl>,
## #   tmean_wettest_month_avg <dbl>, tmean_driest_month_avg <dbl>,
## #   ann_ppt_avg <dbl>, ppt_seasonality_avg <dbl>, ppt_warmest_month_avg <dbl>,
## #   ppt_coldest_month_avg <dbl>
```

```r
pops_bioclim_historical_avgs <- read_csv("../output/Climate/Pops_BioClimAvgs_Historical.csv")  %>% 
  select(parent.pop:ppt_coldest_month_avg)
```

```
## Rows: 23 Columns: 23
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (21): elev_m, ann_tmean_avg, mean_diurnal_range_avg, temp_seasonality_av...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
head(pops_bioclim_historical_avgs)
```

```
## # A tibble: 6 × 13
##   parent.pop elevation.group elev_m ann_tmean_avg mean_diurnal_range_avg
##   <chr>      <chr>            <dbl>         <dbl>                  <dbl>
## 1 BH         Low               511.          11.9                   13.8
## 2 CC         Low               313           14.0                   13.2
## 3 CP2        High             2244.          10.8                   13.7
## 4 CP3        High             2266.          10.2                   13.3
## 5 DPR        Mid              1019.          15.1                   14.2
## 6 FR         Mid               787           14.3                   17.1
## # ℹ 8 more variables: temp_seasonality_avg <dbl>, temp_ann_range_avg <dbl>,
## #   tmean_wettest_month_avg <dbl>, tmean_driest_month_avg <dbl>,
## #   ann_ppt_avg <dbl>, ppt_seasonality_avg <dbl>, ppt_warmest_month_avg <dbl>,
## #   ppt_coldest_month_avg <dbl>
```

```r
names(pops_bioclim_historical_avgs)
```

```
##  [1] "parent.pop"              "elevation.group"        
##  [3] "elev_m"                  "ann_tmean_avg"          
##  [5] "mean_diurnal_range_avg"  "temp_seasonality_avg"   
##  [7] "temp_ann_range_avg"      "tmean_wettest_month_avg"
##  [9] "tmean_driest_month_avg"  "ann_ppt_avg"            
## [11] "ppt_seasonality_avg"     "ppt_warmest_month_avg"  
## [13] "ppt_coldest_month_avg"
```


## Davis Climate Data (Nov 2022-Oct 2023)

### From CIMIS


```r
davis_climate <- read_csv("../input/CIMS_Davis_monthly.csv")
```

```
## New names:
## Rows: 14 Columns: 30
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (16): Stn Name, CIMIS Region, Month Year, qc...6, qc...8, qc...10, qc...... dbl
## (14): Stn Id, Total ETo (mm), Total Precip (mm), Avg Sol Rad (W/sq.m), A...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## • `qc` -> `qc...6`
## • `qc` -> `qc...8`
## • `qc` -> `qc...10`
## • `qc` -> `qc...12`
## • `qc` -> `qc...14`
## • `qc` -> `qc...16`
## • `qc` -> `qc...18`
## • `qc` -> `qc...20`
## • `qc` -> `qc...22`
## • `qc` -> `qc...24`
## • `qc` -> `qc...26`
## • `qc` -> `qc...28`
## • `qc` -> `qc...30`
```

```r
head(davis_climate)
```

```
## # A tibble: 6 × 30
##   `Stn Id` `Stn Name` `CIMIS Region`    `Month Year` `Total ETo (mm)` qc...6
##      <dbl> <chr>      <chr>             <chr>                   <dbl> <chr> 
## 1        6 Davis      Sacramento Valley Nov 2022                 56.4 <NA>  
## 2        6 Davis      Sacramento Valley Dec 2022                 20.1 <NA>  
## 3        6 Davis      Sacramento Valley Jan 2023                 45.4 <NA>  
## 4        6 Davis      Sacramento Valley Feb 2023                 54.0 <NA>  
## 5        6 Davis      Sacramento Valley Mar 2023                 70.8 <NA>  
## 6        6 Davis      Sacramento Valley Apr 2023                146.  <NA>  
## # ℹ 24 more variables: `Total Precip (mm)` <dbl>, qc...8 <chr>,
## #   `Avg Sol Rad (W/sq.m)` <dbl>, qc...10 <chr>, `Avg Vap Pres (kPa)` <dbl>,
## #   qc...12 <chr>, `Avg Max Air Temp (C)` <dbl>, qc...14 <chr>,
## #   `Avg Min Air Temp (C)` <dbl>, qc...16 <chr>, `Avg Air Temp (C)` <dbl>,
## #   qc...18 <chr>, `Avg Max Rel Hum (%)` <dbl>, qc...20 <chr>,
## #   `Avg Min Rel Hum (%)` <dbl>, qc...22 <chr>, `Avg Rel Hum (%)` <dbl>,
## #   qc...24 <chr>, `Avg Dew Point (C)` <dbl>, qc...26 <chr>, …
```

```r
davis_climate_short <- davis_climate %>% 
  select(month_year=`Month Year`, refet=`Total ETo (mm)`, ppt=`Total Precip (mm)`,
         avg_sol_rad=`Avg Sol Rad (W/sq.m)`, avg_vapor_pres=`Avg Vap Pres (kPa)`,
         tmx=`Avg Max Air Temp (C)`, tmn=`Avg Min Air Temp (C)`, tavg=`Avg Air Temp (C)`,
         avg_max_rel_hum=`Avg Max Rel Hum (%)`, avg_min_rel_hum=`Avg Min Rel Hum (%)`,
         avg_rel_hum=`Avg Rel Hum (%)`, avg_dew_pt=`Avg Dew Point (C)`,
         avg_wind_spd=`Avg Wind Speed (m/s)`, avg_soil_temp=`Avg Soil Temp (C)`) %>% 
  separate(month_year, c("month", "year"), " ", remove = FALSE) %>% 
  mutate(month=match(month, month.abb),  t_diurnal = (tmx-tmn))
summary(davis_climate_short)
```

```
##   month_year            month            year               refet       
##  Length:14          Min.   : 1.000   Length:14          Min.   : 20.06  
##  Class :character   1st Qu.: 4.250   Class :character   1st Qu.: 54.64  
##  Mode  :character   Median : 7.500   Mode  :character   Median : 85.82  
##                     Mean   : 7.214                      Mean   :104.34  
##                     3rd Qu.:10.750                      3rd Qu.:155.41  
##                     Max.   :12.000                      Max.   :211.62  
##       ppt           avg_sol_rad    avg_vapor_pres       tmx       
##  Min.   :  0.000   Min.   : 63.0   Min.   :0.700   Min.   :11.30  
##  1st Qu.:  5.725   1st Qu.:117.0   1st Qu.:0.900   1st Qu.:14.82  
##  Median : 29.850   Median :167.0   Median :1.000   Median :21.20  
##  Mean   : 69.436   Mean   :189.7   Mean   :1.114   Mean   :21.76  
##  3rd Qu.:113.850   3rd Qu.:280.2   3rd Qu.:1.375   3rd Qu.:28.00  
##  Max.   :247.400   Max.   :328.0   Max.   :1.600   Max.   :34.00  
##       tmn              tavg       avg_max_rel_hum avg_min_rel_hum
##  Min.   : 2.200   Min.   : 7.10   Min.   :86.00   Min.   :30.00  
##  1st Qu.: 4.675   1st Qu.: 9.45   1st Qu.:89.00   1st Qu.:34.50  
##  Median : 6.450   Median :13.65   Median :91.00   Median :40.50  
##  Mean   : 7.671   Mean   :14.49   Mean   :90.86   Mean   :44.57  
##  3rd Qu.:11.175   3rd Qu.:19.27   3rd Qu.:92.75   3rd Qu.:53.50  
##  Max.   :14.700   Max.   :24.00   Max.   :97.00   Max.   :72.00  
##   avg_rel_hum      avg_dew_pt      avg_wind_spd  avg_soil_temp  
##  Min.   :54.00   Min.   : 2.500   Min.   :1.80   Min.   : 8.30  
##  1st Qu.:58.50   1st Qu.: 5.025   1st Qu.:2.10   1st Qu.:10.30  
##  Median :62.50   Median : 6.550   Median :2.30   Median :13.05  
##  Mean   :65.86   Mean   : 7.736   Mean   :2.35   Mean   :14.21  
##  3rd Qu.:73.00   3rd Qu.:11.325   3rd Qu.:2.40   3rd Qu.:17.77  
##  Max.   :88.00   Max.   :14.200   Max.   :3.30   Max.   :21.90  
##    t_diurnal    
##  Min.   : 7.90  
##  1st Qu.:11.20  
##  Median :14.45  
##  Mean   :14.09  
##  3rd Qu.:16.68  
##  Max.   :20.60
```

```r
names(davis_climate_short)
```

```
##  [1] "month_year"      "month"           "year"            "refet"          
##  [5] "ppt"             "avg_sol_rad"     "avg_vapor_pres"  "tmx"            
##  [9] "tmn"             "tavg"            "avg_max_rel_hum" "avg_min_rel_hum"
## [13] "avg_rel_hum"     "avg_dew_pt"      "avg_wind_spd"    "avg_soil_temp"  
## [17] "t_diurnal"
```

### BioClim

-   annual mean temperature (BIO1)
-   mean diurnal range (BIO2) - (Mean of monthly (max temp - min temp))
-   temperature seasonality (BIO4) (standard deviation *100)
-   temperature annual range (BIO7) (Max Temperature of Warmest Month - Min Temperature of Coldest Month)
-   annual precipitation (BIO12) - sum of ppt for the entire year (not the avg)
-   precipitation seasonality (BIO15)  (Coefficient of Variation)

-   mean temp of wettest quarter (BIO8) - CHANGED TO mean temp of wettest month 
-   mean temp of driest quarter (BIO9) - CHANGED TO mean temp of driest month 
-   precip of warmest quarter (BIO18) - CHANGED TO precip of warmest month
-   precip of coldest quarter (BIO19) - CHANGED TO precip of coldest month 


Calculating wettest, driest, warmest, and coldest months 


```r
davis_wettest_month <- davis_climate_short %>%  
  slice_max(ppt)

davis_driest_month <- davis_climate_short %>%  #there's more than one driest month
  slice_min(ppt)

davis_warmest_month <- davis_climate_short %>% 
  slice_max(tavg)

davis_coldest_month <- davis_climate_short %>%
  slice_min(tavg)
```

Bio 1, 2, 4, 7, 12, 15

```r
bioclim_davis_calc <- davis_climate_short %>% 
  summarise(ann_tmean=mean(tavg),  #Bio1 - Annual Mean Temperature
            mean_diurnal_range=mean(t_diurnal), #Bio2 - Mean Diurnal Range
            temp_seasonality=sd(tavg), #Bio4 - Temperature Seasonality
            temp_ann_range=(max(tmx))-(min(tmn)), #bio7 - temp annual range
            ann_ppt=sum(ppt), #bio12 - annual precip
            ppt_seasonality=cv(ppt+1)) #bio15 - Precipitation Seasonality (+1 to avoid strange CVs for areas where mean rainfaill is < 1)
bioclim_davis_calc
```

```
## # A tibble: 1 × 6
##   ann_tmean mean_diurnal_range temp_seasonality temp_ann_range ann_ppt
##       <dbl>              <dbl>            <dbl>          <dbl>   <dbl>
## 1      14.5               14.1             5.90           31.8    972.
## # ℹ 1 more variable: ppt_seasonality <dbl>
```

Bio 8(Q), 9(Q), 18(Q), 19(Q)

```r
#bio8 = tmean_wettest_month
bio8_davis <- davis_wettest_month %>% 
  dplyr::select(tmean_wettest_month=tavg)

#bio9 = tmean_driest_month
bio9_davis <- davis_driest_month %>% 
  summarise(tmean_driest_month=mean(tavg)) #taking the average b/c multiple driest months 

bio8_9_davis <- bind_cols(bio8_davis, bio9_davis)

#bio18 = ppt_warmest_month
bio18_davis <- davis_warmest_month %>% 
  dplyr::select(ppt_warmest_month=ppt)

#bio19 = ppt_coldest_month
bio19_davis <- davis_wettest_month %>% 
  dplyr::select(ppt_coldest_month=ppt)

bio18_19_davis <- bind_cols(bio18_davis, bio19_davis)

all_periods_davis <- bind_cols(bio8_9_davis, bio18_19_davis)
```

Merge all bioclims

```r
davis_bioclim_final <- bind_cols(bioclim_davis_calc, all_periods_davis) %>% 
  select_all(.funs = funs(paste0(., "_Davis")))
```

```
## Warning: `funs()` was deprecated in dplyr 0.8.0.
## ℹ Please use a list of either functions or lambdas:
## 
## # Simple named list: list(mean = mean, median = median)
## 
## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
## 
## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```r
summary(davis_bioclim_final)
```

```
##  ann_tmean_Davis mean_diurnal_range_Davis temp_seasonality_Davis
##  Min.   :14.49   Min.   :14.09            Min.   :5.902         
##  1st Qu.:14.49   1st Qu.:14.09            1st Qu.:5.902         
##  Median :14.49   Median :14.09            Median :5.902         
##  Mean   :14.49   Mean   :14.09            Mean   :5.902         
##  3rd Qu.:14.49   3rd Qu.:14.09            3rd Qu.:5.902         
##  Max.   :14.49   Max.   :14.09            Max.   :5.902         
##  temp_ann_range_Davis ann_ppt_Davis   ppt_seasonality_Davis
##  Min.   :31.8         Min.   :972.1   Min.   :117.4        
##  1st Qu.:31.8         1st Qu.:972.1   1st Qu.:117.4        
##  Median :31.8         Median :972.1   Median :117.4        
##  Mean   :31.8         Mean   :972.1   Mean   :117.4        
##  3rd Qu.:31.8         3rd Qu.:972.1   3rd Qu.:117.4        
##  Max.   :31.8         Max.   :972.1   Max.   :117.4        
##  tmean_wettest_month_Davis tmean_driest_month_Davis ppt_warmest_month_Davis
##  Min.   :9                 Min.   :18.75            Min.   :122.7          
##  1st Qu.:9                 1st Qu.:18.75            1st Qu.:122.7          
##  Median :9                 Median :18.75            Median :122.7          
##  Mean   :9                 Mean   :18.75            Mean   :122.7          
##  3rd Qu.:9                 3rd Qu.:18.75            3rd Qu.:122.7          
##  Max.   :9                 Max.   :18.75            Max.   :122.7          
##  ppt_coldest_month_Davis
##  Min.   :247.4          
##  1st Qu.:247.4          
##  Median :247.4          
##  Mean   :247.4          
##  3rd Qu.:247.4          
##  Max.   :247.4
```

## Davis Climate Trends


```r
davis_climate_short$month_year <- factor(davis_climate_short$month_year, levels = c("Nov 2022","Dec 2022","Jan 2023", "Feb 2023", "Mar 2023", "Apr 2023", "May 2023", "Jun 2023", "Jul 2023", "Aug 2023", "Sep 2023", "Oct 2023"))
davis_climate_short %>% ggplot(aes(x=month_year, y=tavg)) + geom_point() + theme_classic()
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
davis_climate_short %>% ggplot(aes(x=month_year, y=ppt)) + geom_point() + theme_classic()
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
davis_climate_short %>% ggplot(aes(x=month_year, y=tmx)) + geom_point() + theme_classic()
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-9-3.png)<!-- -->

```r
davis_climate_short %>% ggplot(aes(x=month_year, y=avg_rel_hum)) + geom_point() + theme_classic()
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-9-4.png)<!-- -->

```r
davis_climate_short %>% ggplot(aes(x=month_year, y=avg_wind_spd)) + geom_point() + theme_classic()
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-9-5.png)<!-- -->

```r
davis_climate_short %>% ggplot(aes(x=month_year, y=avg_sol_rad)) + geom_point() + theme_classic()
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-9-6.png)<!-- -->

## Flint Climate Distance

### Subtraction

```r
davis_climate_short_flint <- davis_climate_short %>%
  summarise(ppt_mean_Davis=mean(ppt), tmn_mean_Davis=mean(tmn), tmx_mean_Davis=mean(tmx))

recent_flint_dist_prep <- bind_cols(davis_climate_short_flint, pops_flint_recent_avgs)
names(recent_flint_dist_prep)
```

```
##  [1] "ppt_mean_Davis"  "tmn_mean_Davis"  "tmx_mean_Davis"  "parent.pop"     
##  [5] "elevation.group" "elev_m"          "cwd_mean"        "pck_mean"       
##  [9] "ppt_mean"        "tmn_mean"        "tmx_mean"
```

```r
recent_flint_dist <- recent_flint_dist_prep %>% 
  mutate(ppt_mean_dist=ppt_mean_Davis - ppt_mean,
         tmn_mean_dist=tmn_mean_Davis - tmn_mean,
         tmx_mean_dist=tmx_mean_Davis - tmx_mean) %>% 
 dplyr::select(parent.pop, elevation.group, elev_m, ends_with("_dist"))

historic_flint_dist_prep <- bind_cols(davis_climate_short_flint, pops_flint_historic_avgs)
names(historic_flint_dist_prep)
```

```
##  [1] "ppt_mean_Davis"  "tmn_mean_Davis"  "tmx_mean_Davis"  "parent.pop"     
##  [5] "elevation.group" "elev_m"          "cwd_mean"        "pck_mean"       
##  [9] "ppt_mean"        "tmn_mean"        "tmx_mean"
```

```r
historic_flint_dist <- historic_flint_dist_prep %>% 
  mutate(ppt_mean_dist=ppt_mean_Davis - ppt_mean,
         tmn_mean_dist=tmn_mean_Davis - tmn_mean,
         tmx_mean_dist=tmx_mean_Davis - tmx_mean) %>% 
 dplyr::select(parent.pop, elevation.group, elev_m, ends_with("_dist"))
```

Figures Recent (subtraction distance)


```r
recent_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_mean_dist), y=ppt_mean_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
ggsave("../output/Climate/MeanPPT_Dist_RecentClim.png", width = 12, height = 6, units = "in")

recent_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmn_mean_dist), y=tmn_mean_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```r
ggsave("../output/Climate/MeanTMN_Dist_RecentClim.png", width = 12, height = 6, units = "in")

recent_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmx_mean_dist), y=tmx_mean_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-11-3.png)<!-- -->

```r
ggsave("../output/Climate/MeanTMX_Dist_RecentClim.png", width = 12, height = 6, units = "in")
```

Figures Historical (subtraction distance)


```r
historic_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_mean_dist), y=ppt_mean_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
ggsave("../output/Climate/MeanPPT_Dist_HistoricalClim.png", width = 12, height = 6, units = "in")

historic_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmn_mean_dist), y=tmn_mean_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-12-2.png)<!-- -->

```r
ggsave("../output/Climate/MeanTMN_Dist_HistoricalClim.png", width = 12, height = 6, units = "in")

historic_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmx_mean_dist), y=tmx_mean_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-12-3.png)<!-- -->

```r
ggsave("../output/Climate/MeanTMX_Dist_HistoricalClim.png", width = 12, height = 6, units = "in")
```

## Bioclim Climate Distance

### Subtraction


```r
#Recent
names(pops_bioclim_recent_avgs)
```

```
##  [1] "parent.pop"              "elevation.group"        
##  [3] "elev_m"                  "ann_tmean_avg"          
##  [5] "mean_diurnal_range_avg"  "temp_seasonality_avg"   
##  [7] "temp_ann_range_avg"      "tmean_wettest_month_avg"
##  [9] "tmean_driest_month_avg"  "ann_ppt_avg"            
## [11] "ppt_seasonality_avg"     "ppt_warmest_month_avg"  
## [13] "ppt_coldest_month_avg"
```

```r
recent_bioclim_dist_prep <- bind_cols(davis_bioclim_final, pops_bioclim_recent_avgs)
recent_bioclim_dist <- recent_bioclim_dist_prep %>% 
  mutate(ann_tmean_dist=ann_tmean_Davis - ann_tmean_avg,
         mean_diurnal_range_dist=mean_diurnal_range_Davis - mean_diurnal_range_avg,
         temp_seasonality_dist=temp_seasonality_Davis - temp_seasonality_avg,
         temp_ann_range_dist=temp_ann_range_Davis - temp_ann_range_avg,
         tmean_wettest_month_dist=tmean_wettest_month_Davis - tmean_wettest_month_avg,
         tmean_driest_month_dist=tmean_driest_month_Davis - tmean_driest_month_avg,
         ann_ppt_dist=ann_ppt_Davis - ann_ppt_avg,
         ppt_seasonality_dist=ppt_seasonality_Davis - ppt_seasonality_avg, 
         ppt_warmest_month_dist=ppt_warmest_month_Davis - ppt_warmest_month_avg,
         ppt_coldest_month_dist=ppt_coldest_month_Davis - ppt_coldest_month_avg) %>% 
 dplyr::select(parent.pop, elevation.group, elev_m, ends_with("_dist"))
recent_bioclim_dist
```

```
## # A tibble: 23 × 13
##    parent.pop elevation.group elev_m ann_tmean_dist mean_diurnal_range_dist
##    <chr>      <chr>            <dbl>          <dbl>                   <dbl>
##  1 BH         Low               511.          1.61                  0.599  
##  2 CC         Low               313           1.16                  2.22   
##  3 CP2        High             2244.          4.34                  1.37   
##  4 CP3        High             2266.          5.03                  1.60   
##  5 DPR        Mid              1019.         -0.963                 1.14   
##  6 FR         Mid               787          -0.392                -1.74   
##  7 IH         Low               454.         -0.152                 0.896  
##  8 LV1        High             2593.          5.21                 -0.0713 
##  9 LV3        High             2354.          5.24                  0.00938
## 10 LVTR1      High             2741.          5.42                 -0.239  
## # ℹ 13 more rows
## # ℹ 8 more variables: temp_seasonality_dist <dbl>, temp_ann_range_dist <dbl>,
## #   tmean_wettest_month_dist <dbl>, tmean_driest_month_dist <dbl>,
## #   ann_ppt_dist <dbl>, ppt_seasonality_dist <dbl>,
## #   ppt_warmest_month_dist <dbl>, ppt_coldest_month_dist <dbl>
```

```r
#Historical
names(pops_bioclim_historical_avgs)
```

```
##  [1] "parent.pop"              "elevation.group"        
##  [3] "elev_m"                  "ann_tmean_avg"          
##  [5] "mean_diurnal_range_avg"  "temp_seasonality_avg"   
##  [7] "temp_ann_range_avg"      "tmean_wettest_month_avg"
##  [9] "tmean_driest_month_avg"  "ann_ppt_avg"            
## [11] "ppt_seasonality_avg"     "ppt_warmest_month_avg"  
## [13] "ppt_coldest_month_avg"
```

```r
historical_bioclim_dist_prep <- bind_cols(davis_bioclim_final, pops_bioclim_historical_avgs)
historical_bioclim_dist <- historical_bioclim_dist_prep %>% 
  mutate(ann_tmean_dist=ann_tmean_Davis - ann_tmean_avg,
         mean_diurnal_range_dist=mean_diurnal_range_Davis - mean_diurnal_range_avg,
         temp_seasonality_dist=temp_seasonality_Davis - temp_seasonality_avg,
         temp_ann_range_dist=temp_ann_range_Davis - temp_ann_range_avg,
         tmean_wettest_month_dist=tmean_wettest_month_Davis - tmean_wettest_month_avg,
         tmean_driest_month_dist=tmean_driest_month_Davis - tmean_driest_month_avg,
         ann_ppt_dist=ann_ppt_Davis - ann_ppt_avg,
         ppt_seasonality_dist=ppt_seasonality_Davis - ppt_seasonality_avg, 
         ppt_warmest_month_dist=ppt_warmest_month_Davis - ppt_warmest_month_avg,
         ppt_coldest_month_dist=ppt_coldest_month_Davis - ppt_coldest_month_avg) %>% 
  dplyr::select(parent.pop, elevation.group, elev_m, ends_with("_dist"))
historical_bioclim_dist
```

```
## # A tibble: 23 × 13
##    parent.pop elevation.group elev_m ann_tmean_dist mean_diurnal_range_dist
##    <chr>      <chr>            <dbl>          <dbl>                   <dbl>
##  1 BH         Low               511.          2.60                    0.308
##  2 CC         Low               313           0.472                   0.871
##  3 CP2        High             2244.          3.70                    0.362
##  4 CP3        High             2266.          4.33                    0.749
##  5 DPR        Mid              1019.         -0.570                  -0.104
##  6 FR         Mid               787           0.231                  -2.99 
##  7 IH         Low               454.          0.579                   0.256
##  8 LV1        High             2593.          8.13                   -0.886
##  9 LV3        High             2354.          8.19                   -0.865
## 10 LVTR1      High             2741.          8.31                   -0.913
## # ℹ 13 more rows
## # ℹ 8 more variables: temp_seasonality_dist <dbl>, temp_ann_range_dist <dbl>,
## #   tmean_wettest_month_dist <dbl>, tmean_driest_month_dist <dbl>,
## #   ann_ppt_dist <dbl>, ppt_seasonality_dist <dbl>,
## #   ppt_warmest_month_dist <dbl>, ppt_coldest_month_dist <dbl>
```

Figures Recent (subtraction distance)


```r
recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ann_tmean_dist), y=ann_tmean_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
ggsave("../output/Climate/Ann_Tmean_Dist_RecentClim.png", width = 12, height = 6, units = "in")

recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, mean_diurnal_range_dist), y=mean_diurnal_range_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

```r
ggsave("../output/Climate/Diurnal_Range_Dist_RecentClim.png", width = 12, height = 6, units = "in")

recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, temp_seasonality_dist), y=temp_seasonality_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-14-3.png)<!-- -->

```r
ggsave("../output/Climate/Temp_Seasonality_Dist_RecentClim.png", width = 12, height = 6, units = "in")

recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, temp_ann_range_dist), y=temp_ann_range_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-14-4.png)<!-- -->

```r
ggsave("../output/Climate/Temp_Ann_Range_Dist_RecentClim.png", width = 12, height = 6, units = "in")

recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmean_wettest_month_dist), y=tmean_wettest_month_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-14-5.png)<!-- -->

```r
ggsave("../output/Climate/Temp_Wet_Dist_RecentClim.png", width = 12, height = 6, units = "in")

recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmean_driest_month_dist), y=tmean_driest_month_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-14-6.png)<!-- -->

```r
ggsave("../output/Climate/Temp_Dry_Dist_RecentClim.png", width = 12, height = 6, units = "in")

recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ann_ppt_dist), y=ann_ppt_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-14-7.png)<!-- -->

```r
ggsave("../output/Climate/Ann_PPT_Dist_RecentClim.png", width = 12, height = 6, units = "in")

recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_seasonality_dist), y=ppt_seasonality_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-14-8.png)<!-- -->

```r
ggsave("../output/Climate/PPT_Seasonality_Dist_RecentClim.png", width = 12, height = 6, units = "in")

recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_warmest_month_dist), y=ppt_warmest_month_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-14-9.png)<!-- -->

```r
ggsave("../output/Climate/PPT_Warm_Dist_RecentClim.png", width = 12, height = 6, units = "in")

recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_coldest_month_dist), y=ppt_coldest_month_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-14-10.png)<!-- -->

```r
ggsave("../output/Climate/PPT_Cold_Dist_RecentClim.png", width = 12, height = 6, units = "in")
```

Historical (subtraction distance)


```r
historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ann_tmean_dist), y=ann_tmean_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
ggsave("../output/Climate/Ann_Tmean_Dist_HistoricalClim.png", width = 12, height = 6, units = "in")

historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, mean_diurnal_range_dist), y=mean_diurnal_range_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-15-2.png)<!-- -->

```r
ggsave("../output/Climate/Diurnal_Range_Dist_HistoricalClim.png", width = 12, height = 6, units = "in")

historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, temp_seasonality_dist), y=temp_seasonality_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-15-3.png)<!-- -->

```r
ggsave("../output/Climate/Temp_Seasonality_Dist_HistoricalClim.png", width = 12, height = 6, units = "in")

historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, temp_ann_range_dist), y=temp_ann_range_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-15-4.png)<!-- -->

```r
ggsave("../output/Climate/Temp_Ann_Range_Dist_HistoricalClim.png", width = 12, height = 6, units = "in")

historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmean_wettest_month_dist), y=tmean_wettest_month_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-15-5.png)<!-- -->

```r
ggsave("../output/Climate/Temp_Wet_Dist_HistoricalClim.png", width = 12, height = 6, units = "in")

historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmean_driest_month_dist), y=tmean_driest_month_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-15-6.png)<!-- -->

```r
ggsave("../output/Climate/Temp_Dry_Dist_HistoricalClim.png", width = 12, height = 6, units = "in")

historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ann_ppt_dist), y=ann_ppt_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-15-7.png)<!-- -->

```r
ggsave("../output/Climate/Ann_PPT_Dist_HistoricalClim.png", width = 12, height = 6, units = "in")

historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_seasonality_dist), y=ppt_seasonality_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-15-8.png)<!-- -->

```r
ggsave("../output/Climate/PPT_Seasonality_Dist_HistoricalClim.png", width = 12, height = 6, units = "in")

historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_warmest_month_dist), y=ppt_warmest_month_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-15-9.png)<!-- -->

```r
ggsave("../output/Climate/PPT_Warm_Dist_HistoricalClim.png", width = 12, height = 6, units = "in")

historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_coldest_month_dist), y=ppt_coldest_month_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
```

![](UCD_climate_distance_files/figure-html/unnamed-chunk-15-10.png)<!-- -->

```r
ggsave("../output/Climate/PPT_Cold_Dist_HistoricalClim.png", width = 12, height = 6, units = "in")
```
