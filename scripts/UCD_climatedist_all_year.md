---
title: "UCD Climate Distance"
author: "Brandie Quarles"
date: "2025-03-20"
output: 
  html_document: 
    keep_md: yes
editor_options: 
  markdown: 
    wrap: 72
---



# Climate Distance at the Davis Garden

Annual data might give a different result, worth presenting it both ways
because that helps highlight the importance of the life history
differences

\*Used "water year" with a start date of Nov 1 b/c that's the first
month UCD was planted and includes all the months we collected data from
WL2. This (as opposed to calendar year) allowed for the inclusion of the
2022 months at the UCD garden

Options:

-   Basic subtraction as in Moran et al 2017 (garden site - home site)

-   Gower's environmental distance metric as in Rutter and Fenster 2007

-   From PCAs "We calculated an overall environmental distance using
    PCA, with EDij corresponding to the Euclidian distance between i and
    j based on PCA1 & PCA2." as in Moran et al 2017

## Relevant Libraries and Functions


``` r
library(raster) #for cv function 
```

```
## Loading required package: sp
```

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
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ tidyr::extract() masks raster::extract()
## ✖ dplyr::filter()  masks stats::filter()
## ✖ dplyr::lag()     masks stats::lag()
## ✖ dplyr::select()  masks raster::select()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(conflicted)
conflicts_prefer(dplyr::select())
```

```
## [conflicted] Will prefer dplyr::select over any other package.
```

``` r
conflicts_prefer(dplyr::filter)
```

```
## [conflicted] Will prefer dplyr::filter over any other package.
```

``` r
library(cowplot) #for combining plots 
library(boot)
library(broom)
library(ggpubr) #for ggarange 
library(corrplot) #plotting correlations 
```

```
## corrplot 0.94 loaded
```

``` r
library(ggfortify) #easier PCA figures

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
} #legend function for grid_arrange

elev_three_palette <- c("#0043F0", "#C9727F", "#F5A540") #colors from Gremer et al 2019
elev_order <- c("High", "Mid", "Low")
```

## Davis Climate Data (Nov 2022-Oct 2023)

### From Flint (changed this from CIMIS)


``` r
davis_climate <- read_csv("../output/Climate/flint_climate_UCDpops.csv") %>% 
  filter(parent.pop=="UCD_Garden") %>% 
  filter(year>=2022, year<2024) %>% 
  filter(if_else(year==2022, month=="nov" | month=="dec",
                 month!="nov" & month !="dec")) %>% 
  mutate(tavg = (tmn + tmx)/2, t_diurnal = (tmx-tmn))
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
head(davis_climate)
```

```
## # A tibble: 6 × 16
##   parent.pop elevation.group elev_m   Lat  Long  year month   aet   cwd   pck
##   <chr>      <chr>            <dbl> <dbl> <dbl> <dbl> <chr> <dbl> <dbl> <dbl>
## 1 UCD_Garden Low                 16  38.5 -122.  2022 dec    3.74  20.5     0
## 2 UCD_Garden Low                 16  38.5 -122.  2022 nov    3.77  33.3     0
## 3 UCD_Garden Low                 16  38.5 -122.  2023 apr   29.4   88.9     0
## 4 UCD_Garden Low                 16  38.5 -122.  2023 aug   42.4  139.      0
## 5 UCD_Garden Low                 16  38.5 -122.  2023 feb    8.57  29.9     0
## 6 UCD_Garden Low                 16  38.5 -122.  2023 jan    5.42  21.7     0
## # ℹ 6 more variables: pet <dbl>, ppt <dbl>, tmn <dbl>, tmx <dbl>, tavg <dbl>,
## #   t_diurnal <dbl>
```

``` r
summary(davis_climate)
```

```
##   parent.pop        elevation.group        elev_m        Lat       
##  Length:12          Length:12          Min.   :16   Min.   :38.53  
##  Class :character   Class :character   1st Qu.:16   1st Qu.:38.53  
##  Mode  :character   Mode  :character   Median :16   Median :38.53  
##                                        Mean   :16   Mean   :38.53  
##                                        3rd Qu.:16   3rd Qu.:38.53  
##                                        Max.   :16   Max.   :38.53  
##       Long             year         month                aet        
##  Min.   :-121.8   Min.   :2022   Length:12          Min.   : 3.740  
##  1st Qu.:-121.8   1st Qu.:2023   Class :character   1st Qu.: 5.298  
##  Median :-121.8   Median :2023   Mode  :character   Median :19.155  
##  Mean   :-121.8   Mean   :2023                      Mean   :25.706  
##  3rd Qu.:-121.8   3rd Qu.:2023                      3rd Qu.:44.822  
##  Max.   :-121.8   Max.   :2023                      Max.   :60.480  
##       cwd              pck         pet              ppt         
##  Min.   : 20.46   Min.   :0   Min.   : 24.20   Min.   :  0.000  
##  1st Qu.: 32.48   1st Qu.:0   1st Qu.: 38.15   1st Qu.:  0.415  
##  Median : 85.09   Median :0   Median :102.25   Median : 15.850  
##  Mean   : 79.91   Mean   :0   Mean   :105.62   Mean   : 60.338  
##  3rd Qu.:115.29   3rd Qu.:0   3rd Qu.:168.95   3rd Qu.: 83.745  
##  Max.   :144.82   Max.   :0   Max.   :205.30   Max.   :258.000  
##       tmn              tmx             tavg          t_diurnal    
##  Min.   : 2.360   Min.   :11.62   Min.   : 7.275   Min.   : 8.69  
##  1st Qu.: 4.497   1st Qu.:14.57   1st Qu.: 9.669   1st Qu.:11.51  
##  Median : 8.720   Median :23.41   Median :16.067   Median :14.70  
##  Mean   : 8.487   Mean   :22.48   Mean   :15.484   Mean   :13.99  
##  3rd Qu.:12.342   3rd Qu.:28.16   3rd Qu.:20.253   3rd Qu.:15.75  
##  Max.   :15.470   Max.   :34.37   Max.   :24.520   Max.   :19.75
```

``` r
davis_climate_flint <- davis_climate %>% #get the means for the variables to compare to home sites 
  summarise(cwd_Davis=mean(cwd),ppt_Davis=mean(ppt), pck_Davis=mean(pck), tmn_Davis=mean(tmn), tmx_Davis=mean(tmx))
davis_climate_flint
```

```
## # A tibble: 1 × 5
##   cwd_Davis ppt_Davis pck_Davis tmn_Davis tmx_Davis
##       <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
## 1      79.9      60.3         0      8.49      22.5
```

## Davis Climate Trends


``` r
davis_climate$month <- factor(davis_climate$month, levels = c("nov","dec","jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct"))

davis_climate %>% 
  ggplot(aes(x=month,y=cwd)) +
  geom_point()
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

``` r
davis_climate %>% 
  ggplot(aes(x=month,y=tmx)) +
  geom_point()
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

``` r
davis_climate %>% 
  ggplot(aes(x=month,y=tmn)) +
  geom_point()
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

``` r
davis_climate %>% 
  ggplot(aes(x=month,y=ppt)) +
  geom_point() 
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-3-4.png)<!-- -->

### BioClim

Calculating wettest, driest, warmest, and coldest months

Note I called these "quarter" instead of month for merging with the home
climate quarter data.


``` r
davis_wettest_quarter <- davis_climate %>%  
  slice_max(ppt)

davis_driest_quarter <- davis_climate %>% 
  slice_min(ppt)

davis_warmest_quarter <- davis_climate %>% 
  slice_max(tavg)

davis_coldest_quarter <- davis_climate %>%
  slice_min(tavg)
```

Bio 1, 2, 4, 7, 12, 15


``` r
bioclim_davis_calc <- davis_climate %>% 
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
## 1      15.5               14.0             6.34           32.0    724.
## # ℹ 1 more variable: ppt_seasonality <dbl>
```

Bio 8(Q), 9(Q), 18(Q), 19(Q)


``` r
#bio8 = tmean_wettest_quarter
bio8_davis <- davis_wettest_quarter %>% 
  dplyr::select(tmean_wettest_quarter=tavg)

#bio9 = tmean_driest_quarter
bio9_davis <- davis_driest_quarter %>% 
  dplyr::select(tmean_driest_quarter=tavg)

bio8_9_davis <- bind_cols(bio8_davis, bio9_davis)

#bio18 = ppt_warmest_quarter
bio18_davis <- davis_warmest_quarter %>% 
  dplyr::select(ppt_warmest_quarter=ppt)

#bio19 = ppt_coldest_quarter
bio19_davis <- davis_wettest_quarter %>% 
  dplyr::select(ppt_coldest_quarter=ppt)

bio18_19_davis <- bind_cols(bio18_davis, bio19_davis)

all_periods_davis <- bind_cols(bio8_9_davis, bio18_19_davis)
```

Merge all bioclims


``` r
davis_bioclim_final <- bind_cols(bioclim_davis_calc, all_periods_davis) %>% 
  rename_with(~paste0(., "_Davis"), 1:10)
summary(davis_bioclim_final)
```

```
##  ann_tmean_Davis mean_diurnal_range_Davis temp_seasonality_Davis
##  Min.   :15.48   Min.   :14               Min.   :6.339         
##  1st Qu.:15.48   1st Qu.:14               1st Qu.:6.339         
##  Median :15.48   Median :14               Median :6.339         
##  Mean   :15.48   Mean   :14               Mean   :6.339         
##  3rd Qu.:15.48   3rd Qu.:14               3rd Qu.:6.339         
##  Max.   :15.48   Max.   :14               Max.   :6.339         
##  temp_ann_range_Davis ann_ppt_Davis   ppt_seasonality_Davis
##  Min.   :32.01        Min.   :724.1   Min.   :144.2        
##  1st Qu.:32.01        1st Qu.:724.1   1st Qu.:144.2        
##  Median :32.01        Median :724.1   Median :144.2        
##  Mean   :32.01        Mean   :724.1   Mean   :144.2        
##  3rd Qu.:32.01        3rd Qu.:724.1   3rd Qu.:144.2        
##  Max.   :32.01        Max.   :724.1   Max.   :144.2        
##  tmean_wettest_quarter_Davis tmean_driest_quarter_Davis
##  Min.   :9.305               Min.   :24.5              
##  1st Qu.:9.305               1st Qu.:24.5              
##  Median :9.305               Median :24.5              
##  Mean   :9.305               Mean   :24.5              
##  3rd Qu.:9.305               3rd Qu.:24.5              
##  Max.   :9.305               Max.   :24.5              
##  ppt_warmest_quarter_Davis ppt_coldest_quarter_Davis
##  Min.   :0.16              Min.   :258              
##  1st Qu.:0.16              1st Qu.:258              
##  Median :0.16              Median :258              
##  Mean   :0.16              Mean   :258              
##  3rd Qu.:0.16              3rd Qu.:258              
##  Max.   :0.16              Max.   :258
```

## Gower's Climate Distance

(1/P) \* SUM ((absolute value(Ai - Bi)) / range(i)) for each variable

-   P = number of environmental variables = 15

-   Ai = 30 year avg of that variable for the home site

-   Bi = Nov 2022-Oct 2023 avg of that variable for the Davis garden

-   Range(i) = maximum - minimum of that variable in the whole data set
    (across sites)

### Prep Davis data for range calculations 


``` r
davis_climate_all <- bind_cols(davis_climate_flint, davis_bioclim_final)

davis_range_prep <- davis_climate_all %>% 
  mutate(parent.pop="Davis") %>% 
  rename_with(~str_remove(., "_Davis"), everything())
```

### Bootstrapping 
#### Load data with all 30 years

``` r
recent_clim_boot <- read_csv("../output/Climate/fullyear_wtr_year_avgs_Recent.csv")
```

```
## Rows: 690 Columns: 21
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (19): elev_m, Lat, Long, year, cwd, pck, ppt, tmn, tmx, ann_tmean, mean_...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
recent_clim_boot_nest <- recent_clim_boot %>% nest(.by=year) #nest to prepare for bootstrapping 
recent_clim_boot_nest
```

```
## # A tibble: 30 × 2
##     year data              
##    <dbl> <list>            
##  1  1994 <tibble [23 × 20]>
##  2  1995 <tibble [23 × 20]>
##  3  1996 <tibble [23 × 20]>
##  4  1997 <tibble [23 × 20]>
##  5  1998 <tibble [23 × 20]>
##  6  1999 <tibble [23 × 20]>
##  7  2000 <tibble [23 × 20]>
##  8  2001 <tibble [23 × 20]>
##  9  2002 <tibble [23 × 20]>
## 10  2003 <tibble [23 × 20]>
## # ℹ 20 more rows
```

``` r
historical_clim_boot <- read_csv("../output/Climate/fullyear_wtr_year_avgs_Historical.csv")
```

```
## Rows: 690 Columns: 21
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): parent.pop, elevation.group
## dbl (19): elev_m, Lat, Long, year, cwd, pck, ppt, tmn, tmx, ann_tmean, mean_...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
historical_clim_boot_nest <- historical_clim_boot %>% nest(.by=year)
historical_clim_boot_nest
```

```
## # A tibble: 30 × 2
##     year data              
##    <dbl> <list>            
##  1  1964 <tibble [23 × 20]>
##  2  1965 <tibble [23 × 20]>
##  3  1966 <tibble [23 × 20]>
##  4  1967 <tibble [23 × 20]>
##  5  1968 <tibble [23 × 20]>
##  6  1969 <tibble [23 × 20]>
##  7  1970 <tibble [23 × 20]>
##  8  1971 <tibble [23 × 20]>
##  9  1972 <tibble [23 × 20]>
## 10  1973 <tibble [23 × 20]>
## # ℹ 20 more rows
```

#### Create the gower_calc function 

``` r
#data <- recent_clim_boot_nest
#P=14

gowers_calc <- function(data, indices, P) { #function with all of the code necessary for calculating gowers distance 
  #data = _clim_boot (recent or historical) - needs to be nested by year; P = # climate variables 

  #need to make davis_range_prep before running this function 
  
  data <-data[indices,] # subset per bootstrap indices
  
  data <- data %>% unnest(data) #unnest so the function can access the climate data
  
  data_means <- data %>% 
    group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
    summarise_at(c("cwd",  "pck", "ppt", "tmn", "tmx", "ann_tmean", "mean_diurnal_range", 
                   "temp_seasonality", "temp_ann_range",
                 "tmean_wettest_quarter", "tmean_driest_quarter", "ann_ppt",
                 "ppt_seasonality","ppt_warmest_quarter", "ppt_coldest_quarter"),
               c(mean), na.rm = TRUE) #get 30 year averages for each climate variable 
  
  range_merge <- bind_rows(data_means, davis_range_prep)
  
  davis_home_climate_ranges <- range_merge %>% #calculate ranges
    ungroup() %>% 
  summarise(cwd_range=max(cwd)-min(cwd),
            pck_range=max(pck)-min(pck),
            ppt_range=max(ppt)-min(ppt), 
            tmn_range=max(tmn)-min(tmn), 
            tmx_range=max(tmx)-min(tmx), 
            ann_tmean_range=max(ann_tmean)-min(ann_tmean),
            mean_diurnal_range_range=max(mean_diurnal_range)-min(mean_diurnal_range),
            temp_seasonality_range=max(temp_seasonality)-min(temp_seasonality),
            temp_ann_range_range=max(temp_ann_range)-min(temp_ann_range),
            tmean_wettest_quarter_range=max(tmean_wettest_quarter)-min(tmean_wettest_quarter),
            tmean_driest_quarter_range=max(tmean_driest_quarter)-min(tmean_driest_quarter),
            ann_ppt_range=max(ann_ppt)-min(ann_ppt), 
            ppt_seasonality_range=max(ppt_seasonality)-min(ppt_seasonality),
            ppt_warmest_quarter_range=max(ppt_warmest_quarter)-min(ppt_warmest_quarter), 
            ppt_coldest_quarter_range=max(ppt_coldest_quarter)-min(ppt_coldest_quarter))
  
  davis_home_climate <- bind_cols(davis_climate_all, data_means) #add davis climate data to home climate data 
  
  davis_home_climate_with_ranges <- bind_cols(davis_home_climate, davis_home_climate_ranges) #add in ranges 
  
  gowers_calc_each_var <- davis_home_climate_with_ranges %>% #variable by variable calc
  mutate(cwd_gowers=abs(cwd_Davis-cwd) / cwd_range,
         pck_gowers=abs(pck_Davis-pck) / pck_range,
         ppt_gowers=abs(ppt_Davis - ppt) / ppt_range,
         tmn_gowers=abs(tmn_Davis - tmn) / tmn_range,
         tmx_gowers=abs(tmx_Davis - tmx) / tmx_range,
         ann_tmean_gowers=abs(ann_tmean_Davis - ann_tmean) / ann_tmean_range,
         mean_diurnal_range_gowers=abs(mean_diurnal_range_Davis - mean_diurnal_range) / mean_diurnal_range_range,
         temp_seasonality_gowers=abs(temp_seasonality_Davis - temp_seasonality) / temp_seasonality_range,
         temp_ann_range_gowers=abs(temp_ann_range_Davis - temp_ann_range) / temp_ann_range_range,
         tmean_wettest_quarter_gowers=abs(tmean_wettest_quarter_Davis - tmean_wettest_quarter) / tmean_wettest_quarter_range,
         tmean_driest_quarter_gowers=abs(tmean_driest_quarter_Davis - tmean_driest_quarter) / tmean_driest_quarter_range,
         ann_ppt_gowers=abs(ann_ppt_Davis - ann_ppt) / ann_ppt_range,
         ppt_seasonality_gowers=abs(ppt_seasonality_Davis - ppt_seasonality) / ppt_seasonality_range,
         ppt_warmest_quarter_gowers=abs(ppt_warmest_quarter_Davis - ppt_warmest_quarter) / ppt_warmest_quarter_range,
         ppt_coldest_quarter_gowers=abs(ppt_coldest_quarter_Davis - ppt_coldest_quarter) / ppt_coldest_quarter_range) %>% 
  dplyr::select(parent.pop, elevation.group, elev_m, ends_with("_gowers"))

 gowers_calc_per_pop <- gowers_calc_each_var %>% #final gowers calc 
  mutate(Gowers_Dist=(1/P)*(cwd_gowers + pck_gowers + ppt_gowers + tmn_gowers + tmx_gowers +
                                ann_tmean_gowers + mean_diurnal_range_gowers +
                                temp_seasonality_gowers +temp_ann_range_gowers +
                                tmean_wettest_quarter_gowers +
                                tmean_driest_quarter_gowers +ann_ppt_gowers +
                                ppt_seasonality_gowers + ppt_warmest_quarter_gowers +
                                ppt_coldest_quarter_gowers)) %>% 
  dplyr::select(parent.pop, elevation.group, elev_m, Gowers_Dist)
  
 gowers_calc_per_pop %>% pull(Gowers_Dist) #make the result a vector 
   }

#gowers_calc(recent_clim_boot_nest, P=15) #the function works
```

#### Perform the bootstrap sampling 
Recent

``` r
gowers.boot_recent <- boot(data=recent_clim_boot_nest, statistic=gowers_calc, R=1000, P=15) #will sample each row (year) with replacement 
gowers.boot_recent$t0 #looks correct 
```

```
##  [1] 0.2258347 0.2741580 0.6006224 0.6359000 0.4602717 0.3494019 0.2858453
##  [8] 0.7914464 0.7954913 0.8012928 0.1920826 0.4284632 0.4457962 0.5146283
## [15] 0.3053494 0.4708242 0.5828742 0.5146265 0.4101416 0.5788534 0.5396814
## [22] 0.5787872 0.5717606
```

``` r
#str(gowers.boot_recent)

for(i in 1:23) {
  plot(gowers.boot_recent, index=i) #distributions look normal for the most part 
}
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-1.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-2.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-3.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-4.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-5.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-6.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-7.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-8.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-9.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-10.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-11.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-12.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-13.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-14.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-15.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-16.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-17.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-18.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-19.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-20.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-21.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-22.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-11-23.png)<!-- -->

``` r
#boot.ci(gowers.boot_recent, type="norm", index = 1) # for the first pop
boot_recent_results <- tidy(gowers.boot_recent,conf.int=TRUE,conf.method="norm") %>%  #all pops
  rename(Gowers_Dist = statistic) %>% 
  mutate(TimePd="Recent")

boot.ci(gowers.boot_recent, index=2)
```

```
## Warning in boot.ci(gowers.boot_recent, index = 2): bootstrap variances needed
## for studentized intervals
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 1000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = gowers.boot_recent, index = 2)
## 
## Intervals : 
## Level      Normal              Basic         
## 95%   ( 0.2557,  0.2919 )   ( 0.2566,  0.2915 )  
## 
## Level     Percentile            BCa          
## 95%   ( 0.2568,  0.2917 )   ( 0.2558,  0.2908 )  
## Calculations and Intervals on Original Scale
```

``` r
tidy(gowers.boot_recent,conf.int=TRUE,conf.method="perc")
```

```
## # A tibble: 23 × 5
##    statistic      bias std.error conf.low conf.high
##        <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
##  1     0.226  0.000439   0.0119     0.201     0.248
##  2     0.274  0.000370   0.00925    0.257     0.292
##  3     0.601 -0.00202    0.0165     0.569     0.631
##  4     0.636 -0.00199    0.0154     0.606     0.664
##  5     0.460 -0.00194    0.0141     0.431     0.484
##  6     0.349 -0.00142    0.0128     0.321     0.371
##  7     0.286  0.00101    0.0126     0.262     0.312
##  8     0.791 -0.000956   0.0106     0.771     0.811
##  9     0.795 -0.000979   0.0104     0.775     0.815
## 10     0.801  0.000417   0.0107     0.781     0.823
## # ℹ 13 more rows
```

``` r
confint(gowers.boot_recent)
```

```
## Bootstrap bca confidence intervals
## 
##        2.5 %    97.5 %
## 1  0.1986417 0.2460691
## 2  0.2558366 0.2907898
## 3  0.5728749 0.6396754
## 4  0.6100220 0.6720851
## 5  0.4346228 0.4895538
## 6  0.3214416 0.3711806
## 7  0.2606376 0.3109726
## 8  0.7715776 0.8122509
## 9  0.7755944 0.8156343
## 10 0.7795080 0.8213220
## 11 0.1715049 0.2127151
## 12 0.3941807 0.4734813
## 13 0.4094412 0.4876212
## 14 0.4764940 0.5593892
## 15 0.2853039 0.3275958
## 16 0.4433067 0.5090584
## 17 0.5544211 0.6259116
## 18 0.4870572 0.5442676
## 19 0.3926785 0.4310231
## 20 0.5523509 0.6107240
## 21 0.5089557 0.5799670
## 22 0.5467262 0.6126048
## 23 0.5386878 0.6053763
```

Historical

``` r
gowers.boot_historical <- boot(data=historical_clim_boot_nest, statistic=gowers_calc, R=1000, P=15) #will sample each row (year) with replacement 
gowers.boot_historical$t0 #looks correct 
```

```
##  [1] 0.2321699 0.2568375 0.6028634 0.6396478 0.4422507 0.3810693 0.2930394
##  [8] 0.7888906 0.7910061 0.8014085 0.2229969 0.4635438 0.4869071 0.5639142
## [15] 0.2804400 0.4585796 0.5656480 0.4829103 0.4411298 0.5830731 0.5254463
## [22] 0.5897844 0.5812688
```

``` r
#str(gowers.boot_historical)

for(i in 1:23) {
  plot(gowers.boot_historical, index=i) #distributions look normal for the most part 
}
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-1.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-2.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-3.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-4.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-5.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-6.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-7.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-8.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-9.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-10.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-11.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-12.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-13.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-14.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-15.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-16.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-17.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-18.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-19.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-20.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-21.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-22.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-12-23.png)<!-- -->

``` r
#boot.ci(gowers.boot_historical, type="norm", index = 1) # for the first pop
boot_historical_results <- tidy(gowers.boot_historical,conf.int=TRUE,conf.method="norm") %>%  #all pops
  rename(Gowers_Dist = statistic) %>% 
  mutate(TimePd="Historical")
```

#### Plot the results 

``` r
pops <- recent_clim_boot %>% select(parent.pop, elevation.group, elev_m, Lat, Long) %>% unique()

boot_gowers_recent_pops <- bind_cols(pops, boot_recent_results) %>% arrange(Gowers_Dist)
boot_gowers_historical_pops <- bind_cols(pops, boot_historical_results) %>% arrange(Gowers_Dist)
boot_gowers_results_all <- bind_rows(boot_gowers_recent_pops, boot_gowers_historical_pops)
#write_csv(boot_gowers_results_all, "../output/Climate/full_year_GowersEnvtalDist_UCD_wtr_year.csv")

recent_fig <- boot_gowers_results_all %>% 
  filter(TimePd=="Recent") %>% 
  ggplot(aes(x=fct_reorder(parent.pop, Gowers_Dist), y=Gowers_Dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                width=.1, position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(y="Gowers Envtal Distance \n from Davis", fill="Elevation (m)", x="Population", title = "Recent Climate") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))

historical_fig <- boot_gowers_results_all %>% 
  filter(TimePd=="Historical") %>% 
  ggplot(aes(x=fct_reorder(parent.pop, Gowers_Dist), y=Gowers_Dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                width=.1, position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(y="Gowers Envtal Distance \n from Davis", fill="Elevation (m)", x="Population", title="Historic Climate") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1)) 
  

plot_grid(historical_fig, recent_fig)
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

``` r
#ggsave("../output/Climate/full_year_Gowers_fromDavis_wtr_year.png", width = 24, height = 8, units = "in")
```

## PCA Climate Distance

### Merge garden climate with home sites 

``` r
full_year_home_avgs_flint <- read_csv("../output/Climate/fullyear_FlintAvgs.csv")
```

```
## Rows: 46 Columns: 11
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (3): parent.pop, elevation.group, TimePd
## dbl (8): elev_m, Lat, Long, cwd, pck, ppt, tmn, tmx
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
full_year_home_avgs_bioclim <- read_csv("../output/Climate/fullyear_BioClimAvgs.csv")
```

```
## Rows: 46 Columns: 16
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (3): parent.pop, elevation.group, TimePd
## dbl (13): elev_m, Lat, Long, ann_tmean, mean_diurnal_range, temp_seasonality...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
full_year_home_avgs <- full_join(full_year_home_avgs_flint, full_year_home_avgs_bioclim)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, Lat, Long,
## TimePd)`
```

``` r
names(full_year_home_avgs)
```

```
##  [1] "parent.pop"            "elevation.group"       "elev_m"               
##  [4] "Lat"                   "Long"                  "cwd"                  
##  [7] "pck"                   "ppt"                   "tmn"                  
## [10] "tmx"                   "TimePd"                "ann_tmean"            
## [13] "mean_diurnal_range"    "temp_seasonality"      "temp_ann_range"       
## [16] "tmean_wettest_quarter" "tmean_driest_quarter"  "ann_ppt"              
## [19] "ppt_seasonality"       "ppt_warmest_quarter"   "ppt_coldest_quarter"
```

``` r
davis_climate_all_long <- davis_climate_all %>% 
  mutate(parent.pop="UCD_Garden", elevation.group="Low", elev_m=16, Lat=38.53250, Long=-121.7830) %>% 
  rename_with(~str_remove(., "_Davis"), everything())

davis_home_recent_avgs <- full_year_home_avgs %>% 
  filter(TimePd=="Recent") %>% 
  bind_rows(davis_climate_all_long) %>% 
  select(-TimePd)
names(davis_home_recent_avgs)
```

```
##  [1] "parent.pop"            "elevation.group"       "elev_m"               
##  [4] "Lat"                   "Long"                  "cwd"                  
##  [7] "pck"                   "ppt"                   "tmn"                  
## [10] "tmx"                   "ann_tmean"             "mean_diurnal_range"   
## [13] "temp_seasonality"      "temp_ann_range"        "tmean_wettest_quarter"
## [16] "tmean_driest_quarter"  "ann_ppt"               "ppt_seasonality"      
## [19] "ppt_warmest_quarter"   "ppt_coldest_quarter"
```

### Correlations - Recent


``` r
#normalize the data
climate_normalized_davis_home_recent_avgs <- davis_home_recent_avgs %>% ungroup() %>% 
  select(cwd:ppt_coldest_quarter) %>% scale() #normalize the data so they're all on the same scale
head(climate_normalized_davis_home_recent_avgs)
```

```
##             cwd        pck        ppt        tmn        tmx  ann_tmean
## [1,]  1.4989099 -0.8814216 -1.8178901  1.2264371  1.4929825  1.3706692
## [2,]  0.2188586 -0.8814216 -0.4862199  1.4919790  1.4305111  1.4650613
## [3,]  0.4602583  0.5837614  0.3728914 -0.6325558 -0.7319528 -0.6869674
## [4,] -0.8784741  0.7066364  0.2037586 -0.7893336 -0.8998452 -0.8501371
## [5,] -2.3776235 -0.8300730  0.8821791  0.9762311  0.7644412  0.8686005
## [6,]  1.4715007 -0.7863360 -0.4639536  0.4526587  0.7362138  0.6030497
##      mean_diurnal_range temp_seasonality temp_ann_range tmean_wettest_quarter
## [1,]          2.0370450       0.31745934      2.0859958             1.3336686
## [2,]          0.3745344       0.08883437     -0.2751311             1.4642649
## [3,]         -0.8429791       0.24656579     -0.4944888            -0.6907785
## [4,]         -0.9781736       0.27238822     -0.4768520            -0.8640755
## [5,]         -0.6905992       0.27282471     -0.6194161             0.8905755
## [6,]          1.7617127       0.41747816      2.5129534             0.5297829
##      tmean_driest_quarter    ann_ppt ppt_seasonality ppt_warmest_quarter
## [1,]            1.1652179 -1.8178901       1.2065244          -1.4492471
## [2,]            1.2842030 -0.4862199       0.4230656          -0.6861626
## [3,]           -0.6044770  0.3728914      -0.7733915           0.5539222
## [4,]           -0.7460267  0.2037586      -0.9490561           0.4000541
## [5,]            0.7540405  0.8821791       0.2678087           0.5319811
## [6,]            0.5282097 -0.4639536       0.1963164          -0.2954059
##      ppt_coldest_quarter
## [1,]          -1.7834517
## [2,]          -0.3127153
## [3,]           0.3284789
## [4,]           0.1244154
## [5,]           1.1158283
## [6,]          -0.3369283
```

``` r
cor.norm = cor(climate_normalized_davis_home_recent_avgs) #test correlations among the traits
cor.norm
```

```
##                              cwd        pck        ppt        tmn        tmx
## cwd                    1.0000000 -0.2908271 -0.6638310  0.1974337  0.2657756
## pck                   -0.2908271  1.0000000  0.6906214 -0.8660033 -0.8538379
## ppt                   -0.6638310  0.6906214  1.0000000 -0.4497063 -0.5084100
## tmn                    0.1974337 -0.8660033 -0.4497063  1.0000000  0.9857497
## tmx                    0.2657756 -0.8538379 -0.5084100  0.9857497  1.0000000
## ann_tmean              0.2339964 -0.8627158 -0.4821138  0.9960975  0.9967497
## mean_diurnal_range     0.4666264 -0.3456235 -0.5340730  0.3979969  0.5466470
## temp_seasonality      -0.3850879  0.1434570  0.2643948 -0.2244306 -0.2526784
## temp_ann_range         0.3326887 -0.2649529 -0.4540195  0.1718506  0.3107847
## tmean_wettest_quarter  0.2430771 -0.8674308 -0.4923871  0.9963192  0.9941327
## tmean_driest_quarter   0.2988413 -0.8233367 -0.4940579  0.9639935  0.9695388
## ann_ppt               -0.6638310  0.6906214  1.0000000 -0.4497063 -0.5084100
## ppt_seasonality        0.5515955 -0.7131134 -0.6975891  0.6255442  0.6406512
## ppt_warmest_quarter   -0.6251734  0.7413381  0.9306907 -0.5844599 -0.6384474
## ppt_coldest_quarter   -0.7133996  0.5857133  0.9650039 -0.3996384 -0.4676498
##                        ann_tmean mean_diurnal_range temp_seasonality
## cwd                    0.2339964          0.4666264      -0.38508790
## pck                   -0.8627158         -0.3456235       0.14345703
## ppt                   -0.4821138         -0.5340730       0.26439480
## tmn                    0.9960975          0.3979969      -0.22443057
## tmx                    0.9967497          0.5466470      -0.25267843
## ann_tmean              1.0000000          0.4774118      -0.24005356
## mean_diurnal_range     0.4774118          1.0000000      -0.26081493
## temp_seasonality      -0.2400536         -0.2608149       1.00000000
## temp_ann_range         0.2453595          0.8394332       0.05810144
## tmean_wettest_quarter  0.9987331          0.4620360      -0.25016533
## tmean_driest_quarter   0.9703485          0.4888242      -0.46182868
## ann_ppt               -0.4821138         -0.5340730       0.26439480
## ppt_seasonality        0.6357062          0.3799650      -0.62445906
## ppt_warmest_quarter   -0.6148746         -0.5724570       0.53632589
## ppt_coldest_quarter   -0.4367504         -0.5610147       0.45220563
##                       temp_ann_range tmean_wettest_quarter tmean_driest_quarter
## cwd                       0.33268866             0.2430771            0.2988413
## pck                      -0.26495286            -0.8674308           -0.8233367
## ppt                      -0.45401952            -0.4923871           -0.4940579
## tmn                       0.17185059             0.9963192            0.9639935
## tmx                       0.31078467             0.9941327            0.9695388
## ann_tmean                 0.24535945             0.9987331            0.9703485
## mean_diurnal_range        0.83943318             0.4620360            0.4888242
## temp_seasonality          0.05810144            -0.2501653           -0.4618287
## temp_ann_range            1.00000000             0.2172576            0.2172361
## tmean_wettest_quarter     0.21725764             1.0000000            0.9699820
## tmean_driest_quarter      0.21723614             0.9699820            1.0000000
## ann_ppt                  -0.45401952            -0.4923871           -0.4940579
## ppt_seasonality           0.12529862             0.6487256            0.7250235
## ppt_warmest_quarter      -0.37398602            -0.6237992           -0.6838033
## ppt_coldest_quarter      -0.44082041            -0.4471730           -0.4997473
##                          ann_ppt ppt_seasonality ppt_warmest_quarter
## cwd                   -0.6638310       0.5515955          -0.6251734
## pck                    0.6906214      -0.7131134           0.7413381
## ppt                    1.0000000      -0.6975891           0.9306907
## tmn                   -0.4497063       0.6255442          -0.5844599
## tmx                   -0.5084100       0.6406512          -0.6384474
## ann_tmean             -0.4821138       0.6357062          -0.6148746
## mean_diurnal_range    -0.5340730       0.3799650          -0.5724570
## temp_seasonality       0.2643948      -0.6244591           0.5363259
## temp_ann_range        -0.4540195       0.1252986          -0.3739860
## tmean_wettest_quarter -0.4923871       0.6487256          -0.6237992
## tmean_driest_quarter  -0.4940579       0.7250235          -0.6838033
## ann_ppt                1.0000000      -0.6975891           0.9306907
## ppt_seasonality       -0.6975891       1.0000000          -0.8598803
## ppt_warmest_quarter    0.9306907      -0.8598803           1.0000000
## ppt_coldest_quarter    0.9650039      -0.6944527           0.9340443
##                       ppt_coldest_quarter
## cwd                            -0.7133996
## pck                             0.5857133
## ppt                             0.9650039
## tmn                            -0.3996384
## tmx                            -0.4676498
## ann_tmean                      -0.4367504
## mean_diurnal_range             -0.5610147
## temp_seasonality                0.4522056
## temp_ann_range                 -0.4408204
## tmean_wettest_quarter          -0.4471730
## tmean_driest_quarter           -0.4997473
## ann_ppt                         0.9650039
## ppt_seasonality                -0.6944527
## ppt_warmest_quarter             0.9340443
## ppt_coldest_quarter             1.0000000
```

``` r
corrplot(cor.norm)
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

``` r
#tmn, tmx, tmean_wettest_quarter, tmean_driest_quarter and ann_tmean all highly correlated (96-99%) - only keep ann_tmean 
#ann_ppt and ppt 100% correlated (ppt = avg across monts, ann_ppt = avg of the total ppt in a year) - only keep ann_ppt
#ppt_warmest and coldest quarter highly correlated with ann_ppt and ppt - take both out 
```

### PCA - Recent + Historical


``` r
davis_home_recent_avgs.pc = prcomp(davis_home_recent_avgs[c(6:7, 11:14, 17:18)], scale = TRUE, center = TRUE)
#str(davis_home_recent_avgs.pc)
```

plot % Variance Explained


``` r
summary(davis_home_recent_avgs.pc)
```

```
## Importance of components:
##                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
## Standard deviation     2.0385 1.2103 1.0724 0.82793 0.53543 0.35638 0.32458
## Proportion of Variance 0.5194 0.1831 0.1437 0.08568 0.03584 0.01588 0.01317
## Cumulative Proportion  0.5194 0.7025 0.8463 0.93194 0.96778 0.98366 0.99682
##                            PC8
## Standard deviation     0.15939
## Proportion of Variance 0.00318
## Cumulative Proportion  1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:7,2,pad="0")),
       percent_var=davis_home_recent_avgs.pc$sdev[1:7]^2/sum(davis_home_recent_avgs.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

Combine PCs with metadata


``` r
davis_home_recent_avgs.pc.dat = data.frame(davis_home_recent_avgs.pc$x)

davis_home_recent_avgs_locs.pc = cbind(davis_home_recent_avgs, davis_home_recent_avgs.pc.dat)

davis_home_recent_avgs_loadings = data.frame(varnames=rownames(davis_home_recent_avgs.pc$rotation), davis_home_recent_avgs.pc$rotation)
davis_home_recent_avgs_loadings
```

```
##                              varnames        PC1         PC2         PC3
## cwd                               cwd -0.3331965 -0.03532342 -0.47285047
## pck                               pck  0.3897348 -0.17461634 -0.49773162
## ann_tmean                   ann_tmean -0.3711544  0.15320840  0.46936556
## mean_diurnal_range mean_diurnal_range -0.3585899 -0.46109082 -0.11945302
## temp_seasonality     temp_seasonality  0.2284730 -0.39894528  0.53670357
## temp_ann_range         temp_ann_range -0.2649604 -0.66227271 -0.01249849
## ann_ppt                       ann_ppt  0.4214331  0.03397912  0.03008809
## ppt_seasonality       ppt_seasonality -0.4132007  0.36505722 -0.07064269
##                             PC4         PC5         PC6         PC7         PC8
## cwd                -0.492109159  0.61877546  0.07293657 -0.17416497  0.05862668
## pck                 0.112587967  0.09108054 -0.05745115  0.41740476 -0.60940962
## ann_tmean           0.294309607  0.48513680 -0.23002648  0.03464307 -0.49232045
## mean_diurnal_range  0.379697356  0.10989269 -0.14464555  0.51297517  0.45191500
## temp_seasonality   -0.524278116  0.16944649  0.24874070  0.36698236 -0.02154950
## temp_ann_range      0.142878078 -0.21450589  0.31685815 -0.46741687 -0.32516161
## ann_ppt             0.468035959  0.49202207  0.52702587 -0.17181157  0.22784950
## ppt_seasonality     0.004995121 -0.21113157  0.69102664  0.38525622 -0.14306076
```


``` r
autoplot(davis_home_recent_avgs.pc, data = davis_home_recent_avgs,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


``` r
autoplot(davis_home_recent_avgs.pc, data = davis_home_recent_avgs,
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

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-20-1.png)<!-- -->


``` r
autoplot(davis_home_recent_avgs.pc, data = davis_home_recent_avgs,
         x=1, y=,
         colour='elev_m', alpha=0.5,
         label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-21-1.png)<!-- -->


## Flint Climate Distance

### Gowers

``` r
#need to modify the gowers_calc function to only include Flint variables 
gowers_calc_flint <- function(data, indices, P) { #function with all of the code necessary for calculating gowers distance 
  #data = _clim_boot (recent or historical) - needs to be nested by year; P = # climate variables 
  #need to make davis_range_prep before running this function 
  
  data <-data[indices,] # subset per bootstrap indices
  
  data <- data %>% unnest(data) #unnest so the function can access the climate data
  
  data_means <- data %>% 
    group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
    summarise_at(c("cwd",  "pck", "ppt", "tmn", "tmx"),
               c(mean), na.rm = TRUE) #get 30 year averages for each climate variable 
  
  range_merge <- bind_rows(data_means, davis_range_prep)
  
  davis_home_climate_ranges <- range_merge %>% #calculate ranges
    ungroup() %>% 
  summarise(cwd_range=max(cwd)-min(cwd),
            pck_range=max(pck)-min(pck),
            ppt_range=max(ppt)-min(ppt), 
            tmn_range=max(tmn)-min(tmn), 
            tmx_range=max(tmx)-min(tmx))
  
  davis_home_climate <- bind_cols(davis_climate_all, data_means) #add davis climate data to home climate data 
  
  davis_home_climate_with_ranges <- bind_cols(davis_home_climate, davis_home_climate_ranges) #add in ranges 
  
  gowers_calc_each_var <- davis_home_climate_with_ranges %>% #variable by variable calc
  mutate(cwd_gowers=abs(cwd_Davis-cwd) / cwd_range,
         pck_gowers=abs(pck_Davis-pck) / pck_range,
         ppt_gowers=abs(ppt_Davis - ppt) / ppt_range,
         tmn_gowers=abs(tmn_Davis - tmn) / tmn_range,
         tmx_gowers=abs(tmx_Davis - tmx) / tmx_range)

 gowers_calc_per_pop <- gowers_calc_each_var %>% #final gowers calc 
  mutate(Gowers_Dist=(1/P)*(cwd_gowers + pck_gowers + ppt_gowers + tmn_gowers + tmx_gowers)) %>% 
  dplyr::select(parent.pop, elevation.group, elev_m, Gowers_Dist)
 
 gowers_calc_per_pop %>% pull(Gowers_Dist) #make the result a vector 
   }
```

#### Recent


``` r
recent_clim_boot_flint_nest <- recent_clim_boot %>% select(parent.pop:tmx) %>% nest(.by=year)

gowers.boot_flint_recent <- boot(data=recent_clim_boot_flint_nest, statistic=gowers_calc_flint, R=1000, P=5) #will sample each row (year) with replacement 
gowers.boot_flint_recent$t0 #looks correct 
```

```
##  [1] 0.06148443 0.15976363 0.50661555 0.59147692 0.36618020 0.15122582
##  [7] 0.18044270 0.80464242 0.83100030 0.81570911 0.05094854 0.26202428
## [13] 0.34212057 0.43370373 0.18386515 0.35014331 0.47399126 0.36474473
## [19] 0.29423862 0.54922662 0.43085109 0.57490561 0.53769304
```

``` r
#str(gowers.boot_flint_recent)

for(i in 1:23) {
  plot(gowers.boot_flint_recent, index=i) #distributions look normal for the most part 
}
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-1.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-2.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-3.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-4.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-5.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-6.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-7.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-8.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-9.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-10.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-11.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-12.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-13.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-14.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-15.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-16.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-17.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-18.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-19.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-20.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-21.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-22.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-23-23.png)<!-- -->

``` r
#boot.ci(gowers.boot_flint_recent, type="norm", index = 1) # for the first pop
boot_flint_recent_results <- tidy(gowers.boot_flint_recent,conf.int=TRUE,conf.method="norm") %>%  #all pops
  rename(Gowers_Dist = statistic) %>% 
  mutate(TimePd="Recent")
```

#### Historic


``` r
historical_clim_boot_flint_nest <- historical_clim_boot %>% select(parent.pop:tmx) %>% nest(.by=year)

gowers.boot_flint_historical <- boot(data=historical_clim_boot_flint_nest, statistic=gowers_calc_flint, R=1000, P=5) #will sample each row (year) with replacement 
gowers.boot_flint_historical$t0 #looks correct 
```

```
##  [1] 0.06050692 0.12923793 0.54693151 0.62756937 0.39647425 0.16835468
##  [7] 0.18555941 0.84520222 0.86905782 0.85678635 0.04382321 0.33392429
## [13] 0.40378893 0.49813102 0.16202204 0.37073887 0.50096582 0.40499582
## [19] 0.31672254 0.57200096 0.46557822 0.60667976 0.57277814
```

``` r
#str(gowers.boot_flint_historical)

for(i in 1:23) {
  plot(gowers.boot_flint_historical, index=i) #distributions look normal for the most part 
}
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-1.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-2.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-3.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-4.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-5.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-6.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-7.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-8.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-9.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-10.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-11.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-12.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-13.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-14.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-15.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-16.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-17.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-18.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-19.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-20.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-21.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-22.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-24-23.png)<!-- -->

``` r
#boot.ci(gowers.boot_flint_historical, type="norm", index = 1) # for the first pop
boot_flint_historical_results <- tidy(gowers.boot_flint_historical,conf.int=TRUE,conf.method="norm") %>%  #all pops
  rename(Gowers_Dist = statistic) %>% 
  mutate(TimePd="Historical")
```

#### Plot the results 


``` r
boot_flint_gowers_recent_pops <- bind_cols(pops, boot_flint_recent_results) %>% arrange(Gowers_Dist)
boot_flint_gowers_historical_pops <- bind_cols(pops, boot_flint_historical_results) %>% arrange(Gowers_Dist)
boot_flint_gowers_results_all <- bind_rows(boot_flint_gowers_recent_pops, boot_flint_gowers_historical_pops)
#write_csv(boot_flint_gowers_results_all, "../output/Climate/full_year_GowersEnvtalDist_UCDFlint_wtr_year.csv")

recent_fig <- boot_flint_gowers_results_all %>% 
  filter(TimePd=="Recent") %>% 
  ggplot(aes(x=fct_reorder(parent.pop, Gowers_Dist), y=Gowers_Dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                width=.1, position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(y="Gowers Envtal Distance \n from Davis", fill="Elevation (m)", x="Population", title = "Recent Climate") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))

historical_fig <- boot_flint_gowers_results_all %>% 
  filter(TimePd=="Historical") %>% 
  ggplot(aes(x=fct_reorder(parent.pop, Gowers_Dist), y=Gowers_Dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                width=.1, position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(y="Gowers Envtal Distance \n from Davis", fill="Elevation (m)", x="Population", title="Historic Climate") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1)) 

plot_grid(historical_fig, recent_fig)
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

``` r
#ggsave("../output/Climate/full_year_Gowers_Flint_fromDavis_wtr_year.png", width = 24, height = 8, units = "in")
```

### Subtraction


``` r
pops_flint_avgs <- read_csv("../output/Climate/fullyear_FlintAvgs_wtr_year.csv")
```

```
## Rows: 46 Columns: 11
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (3): parent.pop, elevation.group, TimePd
## dbl (8): elev_m, Lat, Long, cwd, pck, ppt, tmn, tmx
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
pops_flint_recent_avgs <- pops_flint_avgs %>% filter(TimePd=="Recent")
recent_flint_dist_prep <- bind_cols(davis_climate_flint, pops_flint_recent_avgs)
names(recent_flint_dist_prep)
```

```
##  [1] "cwd_Davis"       "ppt_Davis"       "pck_Davis"       "tmn_Davis"      
##  [5] "tmx_Davis"       "parent.pop"      "elevation.group" "elev_m"         
##  [9] "Lat"             "Long"            "cwd"             "pck"            
## [13] "ppt"             "tmn"             "tmx"             "TimePd"
```

``` r
recent_flint_dist <- recent_flint_dist_prep %>% 
  mutate(ppt_dist=ppt - ppt_Davis,
         cwd_dist=cwd - cwd_Davis,
         pck_dist=pck - pck_Davis,
         tmn_dist=tmn - tmn_Davis,
         tmx_dist=tmx - tmx_Davis) %>% 
 dplyr::select(parent.pop, elevation.group, elev_m, ends_with("_dist"))

pops_flint_historic_avgs <-  pops_flint_avgs %>% filter(TimePd=="Historical")
historic_flint_dist_prep <- bind_cols(davis_climate_flint, pops_flint_historic_avgs)
names(historic_flint_dist_prep)
```

```
##  [1] "cwd_Davis"       "ppt_Davis"       "pck_Davis"       "tmn_Davis"      
##  [5] "tmx_Davis"       "parent.pop"      "elevation.group" "elev_m"         
##  [9] "Lat"             "Long"            "cwd"             "pck"            
## [13] "ppt"             "tmn"             "tmx"             "TimePd"
```

``` r
historic_flint_dist <- historic_flint_dist_prep %>% 
  mutate(ppt_dist=ppt - ppt_Davis,
         cwd_dist=cwd - cwd_Davis,
         pck_dist=pck - pck_Davis,
         tmn_dist=tmn - tmn_Davis,
         tmx_dist=tmx - tmx_Davis) %>% 
 dplyr::select(parent.pop, elevation.group, elev_m, ends_with("_dist"))
```

Figures Recent (subtraction distance)


``` r
cwd_dist_fig_recent <- recent_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, cwd_dist), y=cwd_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_MeanCWD_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")

pck_dist_fig_recent <- recent_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, pck_dist), y=pck_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_MeanPCK_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")

ppt_dist_fig_recent <- recent_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_dist), y=ppt_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_MeanPPT_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")

tmn_dist_fig_recent <- recent_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmn_dist), y=tmn_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_MeanTMN_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")

tmx_dist_fig_recent <- recent_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmx_dist), y=tmx_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_MeanTMX_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")
```

Figures Historical (subtraction distance)


``` r
cwd_dist_fig_historical <- historic_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, cwd_dist), y=cwd_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_MeanCWD_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")

pck_dist_fig_historical <- historic_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, pck_dist), y=pck_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_MeanPCK_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")

ppt_dist_fig_historical <- historic_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_dist), y=ppt_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_MeanPPT_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")

tmn_dist_fig_historical <- historic_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmn_dist), y=tmn_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_MeanTMN_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")

tmx_dist_fig_historical <- historic_flint_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmx_dist), y=tmx_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_MeanTMX_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")
```

## Bioclim Climate Distance

### Gowers

``` r
#need to modify the gowers_calc function to only include bioclim variables 
gowers_calc_bioclim <- function(data, indices, P) { #function with all of the code necessary for calculating gowers distance 
  #data = _clim_boot (recent or historical) - needs to be nested by year; P = # climate variables 
  #need to make davis_range_prep before running this function 
  
  data <-data[indices,] # subset per bootstrap indices
  
  data <- data %>% unnest(data) #unnest so the function can access the climate data
  
  data_means <- data %>% 
    group_by(parent.pop, elevation.group, elev_m, Lat, Long) %>% 
    summarise_at(c("ann_tmean", "mean_diurnal_range", 
                   "temp_seasonality", "temp_ann_range",
                 "tmean_wettest_quarter", "tmean_driest_quarter", "ann_ppt",
                 "ppt_seasonality","ppt_warmest_quarter", "ppt_coldest_quarter"),
               c(mean), na.rm = TRUE) #get 30 year averages for each climate variable 
  
  range_merge <- bind_rows(data_means, davis_range_prep)
  
  davis_home_climate_ranges <- range_merge %>% #calculate ranges
    ungroup() %>% 
  summarise(ann_tmean_range=max(ann_tmean)-min(ann_tmean),
            mean_diurnal_range_range=max(mean_diurnal_range)-min(mean_diurnal_range),
            temp_seasonality_range=max(temp_seasonality)-min(temp_seasonality),
            temp_ann_range_range=max(temp_ann_range)-min(temp_ann_range),
            tmean_wettest_quarter_range=max(tmean_wettest_quarter)-min(tmean_wettest_quarter),
            tmean_driest_quarter_range=max(tmean_driest_quarter)-min(tmean_driest_quarter),
            ann_ppt_range=max(ann_ppt)-min(ann_ppt), 
            ppt_seasonality_range=max(ppt_seasonality)-min(ppt_seasonality),
            ppt_warmest_quarter_range=max(ppt_warmest_quarter)-min(ppt_warmest_quarter), 
            ppt_coldest_quarter_range=max(ppt_coldest_quarter)-min(ppt_coldest_quarter))
  
  davis_home_climate <- bind_cols(davis_climate_all, data_means) #add davis climate data to home climate data 
  
  davis_home_climate_with_ranges <- bind_cols(davis_home_climate, davis_home_climate_ranges) #add in ranges 
  
  gowers_calc_each_var <- davis_home_climate_with_ranges %>% #variable by variable calc
  mutate(ann_tmean_gowers=abs(ann_tmean_Davis - ann_tmean) / ann_tmean_range,
         mean_diurnal_range_gowers=abs(mean_diurnal_range_Davis - mean_diurnal_range) / mean_diurnal_range_range,
         temp_seasonality_gowers=abs(temp_seasonality_Davis - temp_seasonality) / temp_seasonality_range,
         temp_ann_range_gowers=abs(temp_ann_range_Davis - temp_ann_range) / temp_ann_range_range,
         tmean_wettest_quarter_gowers=abs(tmean_wettest_quarter_Davis - tmean_wettest_quarter) / tmean_wettest_quarter_range,
         tmean_driest_quarter_gowers=abs(tmean_driest_quarter_Davis - tmean_driest_quarter) / tmean_driest_quarter_range,
         ann_ppt_gowers=abs(ann_ppt_Davis - ann_ppt) / ann_ppt_range,
         ppt_seasonality_gowers=abs(ppt_seasonality_Davis - ppt_seasonality) / ppt_seasonality_range,
         ppt_warmest_quarter_gowers=abs(ppt_warmest_quarter_Davis - ppt_warmest_quarter) / ppt_warmest_quarter_range,
         ppt_coldest_quarter_gowers=abs(ppt_coldest_quarter_Davis - ppt_coldest_quarter) / ppt_coldest_quarter_range) %>% 
  dplyr::select(parent.pop, elevation.group, elev_m, ends_with("_gowers"))

 gowers_calc_per_pop <- gowers_calc_each_var %>% #final gowers calc 
  mutate(Gowers_Dist=(1/P)*(ann_tmean_gowers + mean_diurnal_range_gowers +
                                temp_seasonality_gowers +temp_ann_range_gowers +
                                tmean_wettest_quarter_gowers +
                                tmean_driest_quarter_gowers +ann_ppt_gowers +
                                ppt_seasonality_gowers + ppt_warmest_quarter_gowers +
                                ppt_coldest_quarter_gowers)) %>% 
  dplyr::select(parent.pop, elevation.group, elev_m, Gowers_Dist)
 
 gowers_calc_per_pop %>% pull(Gowers_Dist) #make the result a vector 
   }
```

#### Recent


``` r
recent_clim_boot_bioclim_nest <- recent_clim_boot %>% select(parent.pop:year, ann_tmean:ppt_coldest_quarter) %>% nest(.by=year)

gowers.boot_bioclim_recent <- boot(data=recent_clim_boot_bioclim_nest, statistic=gowers_calc_bioclim, R=1000, P=10) #will sample each row (year) with replacement 
gowers.boot_bioclim_recent$t0 #looks correct 
```

```
##  [1] 0.3080099 0.3313552 0.6476258 0.6581115 0.5073175 0.4484900 0.3385466
##  [8] 0.7848484 0.7777368 0.7940846 0.2626496 0.5116827 0.4976341 0.5550906
## [15] 0.3660915 0.5311646 0.6373157 0.5895674 0.4680930 0.5936668 0.5940966
## [22] 0.5807280 0.5887944
```

``` r
#str(gowers.boot_bioclim_recent)

for(i in 1:23) {
  plot(gowers.boot_bioclim_recent, index=i) #distributions look normal for the most part 
}
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-1.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-2.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-3.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-4.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-5.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-6.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-7.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-8.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-9.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-10.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-11.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-12.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-13.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-14.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-15.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-16.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-17.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-18.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-19.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-20.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-21.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-22.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-30-23.png)<!-- -->

``` r
#boot.ci(gowers.boot_bioclim_recent, type="norm", index = 1) # for the first pop
boot_bioclim_recent_results <- tidy(gowers.boot_bioclim_recent,conf.int=TRUE,conf.method="norm") %>%  #all pops
  rename(Gowers_Dist = statistic) %>% 
  mutate(TimePd="Recent")
```

#### Historic


``` r
historical_clim_boot_bioclim_nest <- historical_clim_boot %>% select(parent.pop:year, ann_tmean:ppt_coldest_quarter) %>% nest(.by=year)

gowers.boot_bioclim_historical <- boot(data=historical_clim_boot_bioclim_nest, statistic=gowers_calc_bioclim, R=1000, P=10) #will sample each row (year) with replacement 
gowers.boot_bioclim_historical$t0 #looks correct 
```

```
##  [1] 0.3180013 0.3206373 0.6308294 0.6456870 0.4651389 0.4874267 0.3467794
##  [8] 0.7607347 0.7519802 0.7737196 0.3125837 0.5283536 0.5284662 0.5968057
## [15] 0.3396490 0.5025000 0.5979890 0.5218675 0.5033334 0.5886092 0.5553803
## [22] 0.5813368 0.5855142
```

``` r
#str(gowers.boot_bioclim_historical)

for(i in 1:23) {
  plot(gowers.boot_bioclim_historical, index=i) #distributions look normal for the most part 
}
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-1.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-2.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-3.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-4.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-5.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-6.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-7.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-8.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-9.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-10.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-11.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-12.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-13.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-14.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-15.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-16.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-17.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-18.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-19.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-20.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-21.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-22.png)<!-- -->![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-31-23.png)<!-- -->

``` r
#boot.ci(gowers.boot_bioclim_historical, type="norm", index = 1) # for the first pop
boot_bioclim_historical_results <- tidy(gowers.boot_bioclim_historical,conf.int=TRUE,conf.method="norm") %>%  #all pops
  rename(Gowers_Dist = statistic) %>% 
  mutate(TimePd="Historical")
```

#### Plot the results 


``` r
boot_bioclim_gowers_recent_pops <- bind_cols(pops, boot_bioclim_recent_results) %>% arrange(Gowers_Dist)
boot_bioclim_gowers_historical_pops <- bind_cols(pops, boot_bioclim_historical_results) %>% arrange(Gowers_Dist)
boot_bioclim_gowers_results_all <- bind_rows(boot_bioclim_gowers_recent_pops, boot_bioclim_gowers_historical_pops)
#write_csv(boot_bioclim_gowers_results_all, "../output/Climate/full_year_GowersEnvtalDist_UCDbioclim_wtr_year.csv")

recent_fig <- boot_bioclim_gowers_results_all %>% 
  filter(TimePd=="Recent") %>% 
  ggplot(aes(x=fct_reorder(parent.pop, Gowers_Dist), y=Gowers_Dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                width=.1, position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(y="Gowers Envtal Distance \n from Davis", fill="Elevation (m)", x="Population", title = "Recent Climate") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))

historical_fig <- boot_bioclim_gowers_results_all %>% 
  filter(TimePd=="Historical") %>% 
  ggplot(aes(x=fct_reorder(parent.pop, Gowers_Dist), y=Gowers_Dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                width=.1, position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(y="Gowers Envtal Distance \n from Davis", fill="Elevation (m)", x="Population", title="Historic Climate") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1)) 

plot_grid(historical_fig, recent_fig)
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

``` r
#ggsave("../output/Climate/full_year_Gowers_BioClim_fromDavis_wtr_year.png", width = 24, height = 8, units = "in")
```

### Subtraction


``` r
pops_bioclim_avgs <-  read_csv("../output/Climate/fullyear_BioClimAvgs_wtr_year.csv") 
```

```
## Rows: 46 Columns: 16
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (3): parent.pop, elevation.group, TimePd
## dbl (13): elev_m, Lat, Long, ann_tmean, mean_diurnal_range, temp_seasonality...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
#Recent
pops_bioclim_recent_avgs <- pops_bioclim_avgs %>%  filter(TimePd=="Recent")
recent_bioclim_dist_prep <- bind_cols(davis_bioclim_final, pops_bioclim_recent_avgs)
recent_bioclim_dist <- recent_bioclim_dist_prep %>% 
  mutate(ann_tmean_dist=ann_tmean - ann_tmean_Davis,
         mean_diurnal_range_dist=mean_diurnal_range - mean_diurnal_range_Davis,
         temp_seasonality_dist=temp_seasonality - temp_seasonality_Davis,
         temp_ann_range_dist=temp_ann_range - temp_ann_range_Davis,
         tmean_wettest_quarter_dist=tmean_wettest_quarter - tmean_wettest_quarter_Davis,
         tmean_driest_quarter_dist=tmean_driest_quarter - tmean_driest_quarter_Davis,
         ann_ppt_dist=ann_ppt - ann_ppt_Davis,
         ppt_seasonality_dist=ppt_seasonality - ppt_seasonality_Davis, 
         ppt_warmest_quarter_dist=ppt_warmest_quarter - ppt_warmest_quarter_Davis,
         ppt_coldest_quarter_dist=ppt_coldest_quarter - ppt_coldest_quarter_Davis) %>% 
 dplyr::select(parent.pop, elevation.group, elev_m, ends_with("_dist"))
recent_bioclim_dist
```

```
## # A tibble: 23 × 13
##    parent.pop elevation.group elev_m ann_tmean_dist mean_diurnal_range_dist
##    <chr>      <chr>            <dbl>          <dbl>                   <dbl>
##  1 BH         Low               511.         0.750                    0.658
##  2 CC         Low               313          1.16                    -0.731
##  3 CP2        High             2244.        -8.20                    -1.76 
##  4 CP3        High             2266.        -8.91                    -1.87 
##  5 DPR        Mid              1019.        -1.43                    -1.62 
##  6 FR         Mid               787         -2.58                     0.428
##  7 IH         Low               454.        -0.0529                  -0.446
##  8 LV1        High             2593.       -10.6                     -1.39 
##  9 LV3        High             2354.       -10.6                     -1.37 
## 10 LVTR1      High             2741.       -10.7                     -1.28 
## # ℹ 13 more rows
## # ℹ 8 more variables: temp_seasonality_dist <dbl>, temp_ann_range_dist <dbl>,
## #   tmean_wettest_quarter_dist <dbl>, tmean_driest_quarter_dist <dbl>,
## #   ann_ppt_dist <dbl>, ppt_seasonality_dist <dbl>,
## #   ppt_warmest_quarter_dist <dbl>, ppt_coldest_quarter_dist <dbl>
```

``` r
#Historical
pops_bioclim_historical_avgs <- pops_bioclim_avgs %>% filter(TimePd=="Historical")
historical_bioclim_dist_prep <- bind_cols(davis_bioclim_final, pops_bioclim_historical_avgs)
historical_bioclim_dist <- historical_bioclim_dist_prep %>% 
  mutate(ann_tmean_dist=ann_tmean - ann_tmean_Davis,
         mean_diurnal_range_dist=mean_diurnal_range - mean_diurnal_range_Davis,
         temp_seasonality_dist=temp_seasonality - temp_seasonality_Davis,
         temp_ann_range_dist=temp_ann_range - temp_ann_range_Davis,
         tmean_wettest_quarter_dist=tmean_wettest_quarter - tmean_wettest_quarter_Davis,
         tmean_driest_quarter_dist=tmean_driest_quarter - tmean_driest_quarter_Davis,
         ann_ppt_dist=ann_ppt - ann_ppt_Davis,
         ppt_seasonality_dist=ppt_seasonality - ppt_seasonality_Davis, 
         ppt_warmest_quarter_dist=ppt_warmest_quarter - ppt_warmest_quarter_Davis,
         ppt_coldest_quarter_dist=ppt_coldest_quarter - ppt_coldest_quarter_Davis) %>% 
  dplyr::select(parent.pop, elevation.group, elev_m, ends_with("_dist"))
historical_bioclim_dist
```

```
## # A tibble: 23 × 13
##    parent.pop elevation.group elev_m ann_tmean_dist mean_diurnal_range_dist
##    <chr>      <chr>            <dbl>          <dbl>                   <dbl>
##  1 BH         Low               511.         -0.153                  1.20  
##  2 CC         Low               313           0.430                  0.0327
##  3 CP2        High             2244.         -9.37                  -1.17  
##  4 CP3        High             2266.        -10.0                   -1.38  
##  5 DPR        Mid              1019.         -2.53                  -0.540 
##  6 FR         Mid               787          -3.16                   1.56  
##  7 IH         Low               454.         -0.730                  0.145 
##  8 LV1        High             2593.        -12.1                   -0.216 
##  9 LV3        High             2354.        -12.1                   -0.197 
## 10 LVTR1      High             2741.        -12.2                   -0.216 
## # ℹ 13 more rows
## # ℹ 8 more variables: temp_seasonality_dist <dbl>, temp_ann_range_dist <dbl>,
## #   tmean_wettest_quarter_dist <dbl>, tmean_driest_quarter_dist <dbl>,
## #   ann_ppt_dist <dbl>, ppt_seasonality_dist <dbl>,
## #   ppt_warmest_quarter_dist <dbl>, ppt_coldest_quarter_dist <dbl>
```

Figures Recent (subtraction distance)


``` r
ann_tmean_dist_fig_recent <- recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ann_tmean_dist), y=ann_tmean_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_Ann_Tmean_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")

diurnal_range_dist_fig_recent <- recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, mean_diurnal_range_dist), y=mean_diurnal_range_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_Diurnal_Range_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")

tmp_seasonality_dist_fig_recent <- recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, temp_seasonality_dist), y=temp_seasonality_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_Temp_Seasonality_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")

tmp_ann_range_dist_fig_recent <- recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, temp_ann_range_dist), y=temp_ann_range_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_Temp_Ann_Range_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")

tmean_wet_dist_fig_recent <- recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmean_wettest_quarter_dist), y=tmean_wettest_quarter_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_Temp_Wet_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")

tmean_dry_dist_fig_recent <- recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmean_driest_quarter_dist), y=tmean_driest_quarter_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_Temp_Dry_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")

ann_ppt_dist_fig_recent <- recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ann_ppt_dist), y=ann_ppt_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_Ann_PPT_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")

ppt_seasonality_dist_fig_recent <- recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_seasonality_dist), y=ppt_seasonality_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_PPT_Seasonality_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")

ppt_warm_dist_fig_recent <- recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_warmest_quarter_dist), y=ppt_warmest_quarter_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_PPT_Warm_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")

ppt_cold_dist_fig_recent <- recent_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_coldest_quarter_dist), y=ppt_coldest_quarter_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_PPT_Cold_DistfromHome_Davis_RecentClim_wtr_year.png", width = 12, height = 6, units = "in")
```

Historical (subtraction distance)


``` r
ann_tmean_dist_fig_historical <- historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ann_tmean_dist), y=ann_tmean_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_Ann_Tmean_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")

diurnal_range_dist_fig_historical <- historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, mean_diurnal_range_dist), y=mean_diurnal_range_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_Diurnal_Range_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")

tmp_seasonality_dist_fig_historical <- historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, temp_seasonality_dist), y=temp_seasonality_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_Temp_Seasonality_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")

tmp_ann_range_dist_fig_historical <- historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, temp_ann_range_dist), y=temp_ann_range_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_Temp_Ann_Range_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")

tmean_wet_dist_fig_historical <- historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmean_wettest_quarter_dist), y=tmean_wettest_quarter_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_Temp_Wet_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")

tmean_dry_dist_fig_historical <- historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, tmean_driest_quarter_dist), y=tmean_driest_quarter_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_Temp_Dry_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")

ann_ppt_dist_fig_historical <- historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ann_ppt_dist), y=ann_ppt_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_Ann_PPT_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")

ppt_seasonality_dist_fig_historical <- historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_seasonality_dist), y=ppt_seasonality_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_PPT_Seasonality_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")

ppt_warm_dist_fig_historical <- historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_warmest_quarter_dist), y=ppt_warmest_quarter_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_PPT_Warm_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")

ppt_cold_dist_fig_historical <- historical_bioclim_dist %>% 
  ggplot(aes(x=fct_reorder(parent.pop, ppt_coldest_quarter_dist), y=ppt_coldest_quarter_dist, group=parent.pop, fill=elev_m)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(fill="Elevation (m)",x="Population") +
  theme_classic() +
  theme(text=element_text(size=25), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Climate/all-year_PPT_Cold_DistfromHome_Davis_HistoricalClim_wtr_year.png", width = 12, height = 6, units = "in")
```


## Combine all Subtraction Figs

``` r
ggarrange(cwd_dist_fig_recent, pck_dist_fig_recent, ppt_dist_fig_recent, tmn_dist_fig_recent, tmx_dist_fig_recent,
          ann_tmean_dist_fig_recent, diurnal_range_dist_fig_recent, tmp_seasonality_dist_fig_recent,
          tmp_ann_range_dist_fig_recent, tmean_wet_dist_fig_recent, tmean_dry_dist_fig_recent,
          ann_ppt_dist_fig_recent, ppt_seasonality_dist_fig_recent, ppt_warm_dist_fig_recent,
          ppt_cold_dist_fig_recent, ncol=3,nrow=5)
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-36-1.png)<!-- -->

``` r
ggsave("../output/Climate/all_year_Sub_dist_from_Home_Davis_Recent.png", width = 24, height = 24, units = "in")

ggarrange(cwd_dist_fig_historical, pck_dist_fig_historical, ppt_dist_fig_historical, 
          tmn_dist_fig_historical, tmx_dist_fig_historical,
          ann_tmean_dist_fig_historical, diurnal_range_dist_fig_historical, tmp_seasonality_dist_fig_historical,
          tmp_ann_range_dist_fig_historical, tmean_wet_dist_fig_historical, tmean_dry_dist_fig_historical,
          ann_ppt_dist_fig_historical, ppt_seasonality_dist_fig_historical, ppt_warm_dist_fig_historical,
          ppt_cold_dist_fig_historical, ncol=3,nrow=5)
```

![](UCD_climatedist_all_year_files/figure-html/unnamed-chunk-36-2.png)<!-- -->

``` r
ggsave("../output/Climate/all_year_Sub_dist_from_Home_Davis_Historical.png", width = 24, height = 24, units = "in")
```

## Combine all Subtraction data

``` r
recent_sub_dist_from_Davis <- full_join(recent_flint_dist, recent_bioclim_dist)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m)`
```

``` r
write_csv(recent_sub_dist_from_Davis, "../output/Climate/full_year_Subtraction_Dist_from_Home_Davis_Recent.csv")

historic_sub_dist_from_Davis <- full_join(historic_flint_dist, historical_bioclim_dist)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m)`
```

``` r
write_csv(historic_sub_dist_from_Davis, "../output/Climate/full_year_Subtraction_Dist_from_Home_Davis_Historical.csv")
```
