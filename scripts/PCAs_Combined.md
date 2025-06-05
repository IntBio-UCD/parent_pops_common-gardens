---
title: "PCAs_Combined"
author: "Brandie QC"
date: "2025-06-04"
output: 
  html_document: 
    keep_md: true
---



# Additional PCAs for paper 

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
library(ggalt) #for geom_encircle
```

```
## Registered S3 methods overwritten by 'ggalt':
##   method                  from   
##   grid.draw.absoluteGrob  ggplot2
##   grobHeight.absoluteGrob ggplot2
##   grobWidth.absoluteGrob  ggplot2
##   grobX.absoluteGrob      ggplot2
##   grobY.absoluteGrob      ggplot2
```

``` r
library(corrplot) #plotting correlations 
```

```
## corrplot 0.94 loaded
```

``` r
library(ggfortify) #easier PCA figures
```

```
## Registered S3 method overwritten by 'ggfortify':
##   method        from 
##   fortify.table ggalt
```

## Combine Water Year and Growing Season PCAs

### Water Year Averages

``` r
flint_all_year_avgs <- read_csv("../output/Climate/fullyear_FlintAvgs_wtr_year.csv")
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
bioclim_all_year_avgs <- read_csv("../output/Climate/fullyear_BioClimAvgs_wtr_year.csv")
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
#Merge
bioclim_flint_all_year_avgs <- full_join(flint_all_year_avgs, bioclim_all_year_avgs) %>% 
  select(TimePd, parent.pop:ppt_coldest_quarter) %>% 
  mutate(Season="Water Year")
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, Lat, Long,
## TimePd)`
```

``` r
names(bioclim_flint_all_year_avgs)
```

```
##  [1] "TimePd"                "parent.pop"            "elevation.group"      
##  [4] "elev_m"                "Lat"                   "Long"                 
##  [7] "cwd"                   "pck"                   "ppt"                  
## [10] "tmn"                   "tmx"                   "ann_tmean"            
## [13] "mean_diurnal_range"    "temp_seasonality"      "temp_ann_range"       
## [16] "tmean_wettest_quarter" "tmean_driest_quarter"  "ann_ppt"              
## [19] "ppt_seasonality"       "ppt_warmest_quarter"   "ppt_coldest_quarter"  
## [22] "Season"
```

### Growth Season Averages

``` r
flint_grwseason_avgs <- read_csv("../output/Climate/growthseason_FlintAvgs.csv")
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
bioclim_grwseason_avgs <- read_csv("../output/Climate/growthseason_BioClimAvgs.csv")
```

```
## Rows: 46 Columns: 14
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (3): parent.pop, elevation.group, TimePd
## dbl (11): elev_m, ann_tmean, mean_diurnal_range, temp_seasonality, temp_ann_...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
#Merge
bioclim_flint_grwseason_avgs <- full_join(flint_grwseason_avgs, bioclim_grwseason_avgs) %>% 
  select(TimePd, parent.pop:ppt_coldest_month) %>% 
  rename(tmean_wettest_quarter=tmean_wettest_month, 
         tmean_driest_quarter=tmean_driest_month,
         ppt_warmest_quarter=ppt_warmest_month,
         ppt_coldest_quarter=ppt_coldest_month) %>% 
  mutate(Season="Growth Season")
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m, TimePd)`
```

``` r
names(bioclim_flint_grwseason_avgs) #change month to quarter
```

```
##  [1] "TimePd"                "parent.pop"            "elevation.group"      
##  [4] "elev_m"                "Lat"                   "Long"                 
##  [7] "cwd"                   "pck"                   "ppt"                  
## [10] "tmn"                   "tmx"                   "ann_tmean"            
## [13] "mean_diurnal_range"    "temp_seasonality"      "temp_ann_range"       
## [16] "tmean_wettest_quarter" "tmean_driest_quarter"  "ann_ppt"              
## [19] "ppt_seasonality"       "ppt_warmest_quarter"   "ppt_coldest_quarter"  
## [22] "Season"
```

### Dataframe with a column for seasonal summary 

``` r
wtryr_grwssn_avgs <- bind_rows(bioclim_flint_all_year_avgs, bioclim_flint_grwseason_avgs)
```

### Correlations - Recent + Historical


``` r
#normalize the data
climate_normalized_wtryr_grwssn_avgs <- wtryr_grwssn_avgs %>% ungroup() %>% 
  select(cwd:ppt_coldest_quarter) %>% scale() #normalize the data so they're all on the same scale

cor.norm = cor(climate_normalized_wtryr_grwssn_avgs) #test correlations among the traits
cor.sig <- cor.mtest(climate_normalized_wtryr_grwssn_avgs, method = "pearson")

corrplot(cor.norm, type="upper",
         tl.srt = 45, p.mat = cor.sig$p, 
         sig.level = 0.05, insig="blank")
```

![](PCAs_Combined_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

``` r
#tmn, tmx, tmean_wettest_quarter, tmean_driest_quarter and ann_tmean all highly correlated (92-99%) - only keep ann_tmean 
#ann_ppt and ppt_coldest_quarter highly correlated (96%) - only keep ann_ppt 
#ppt_coldest_quarter and ppt_warmest_quarter (92%) - only keek ppt_warmest_quarter
```

### PCA - Recent + Historical

``` r
wtryr_grwssn_avgs.pc = prcomp(wtryr_grwssn_avgs[c(7:9, 12:15, 18:20)], scale = TRUE, center = TRUE)
str(wtryr_grwssn_avgs.pc)
```

```
## List of 5
##  $ sdev    : num [1:10] 2.198 1.317 1.14 1.002 0.713 ...
##  $ rotation: num [1:10, 1:10] -0.247 0.372 0.375 -0.248 -0.123 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:10] "cwd" "pck" "ppt" "ann_tmean" ...
##   .. ..$ : chr [1:10] "PC1" "PC2" "PC3" "PC4" ...
##  $ center  : Named num [1:10] 57.9 83.8 88.2 10.5 13.4 ...
##   ..- attr(*, "names")= chr [1:10] "cwd" "pck" "ppt" "ann_tmean" ...
##  $ scale   : Named num [1:10] 14.45 130.61 27.93 3.74 1.08 ...
##   ..- attr(*, "names")= chr [1:10] "cwd" "pck" "ppt" "ann_tmean" ...
##  $ x       : num [1:92, 1:10] -1.554 -0.213 2.257 2.532 1.961 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:10] "PC1" "PC2" "PC3" "PC4" ...
##  - attr(*, "class")= chr "prcomp"
```

plot % Variance Explained


``` r
summary(wtryr_grwssn_avgs.pc)
```

```
## Importance of components:
##                           PC1    PC2    PC3    PC4     PC5     PC6     PC7
## Standard deviation     2.1983 1.3173 1.1404 1.0021 0.71262 0.51400 0.47578
## Proportion of Variance 0.4833 0.1735 0.1301 0.1004 0.05078 0.02642 0.02264
## Cumulative Proportion  0.4833 0.6568 0.7869 0.8873 0.93805 0.96447 0.98711
##                            PC8     PC9    PC10
## Standard deviation     0.29570 0.16825 0.11465
## Proportion of Variance 0.00874 0.00283 0.00131
## Cumulative Proportion  0.99585 0.99869 1.00000
```

``` r
tibble(PC=str_c("PC",str_pad(1:10,2,pad="0")),
       percent_var=wtryr_grwssn_avgs.pc$sdev[1:10]^2/sum(wtryr_grwssn_avgs.pc$sdev^2)*100) %>%
  ggplot(aes(x=PC, y=percent_var)) +
  geom_col() +
  ggtitle("Percent Variance Explained")
```

![](PCAs_Combined_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Combine PCs with metadata


``` r
wtryr_grwssn_avgs.pc.dat = data.frame(wtryr_grwssn_avgs.pc$x)

wtryr_grwssn_avgs_locs.pc = cbind(wtryr_grwssn_avgs, wtryr_grwssn_avgs.pc.dat)

wtryr_grwssn_avgs_loadings = data.frame(varnames=rownames(wtryr_grwssn_avgs.pc$rotation), wtryr_grwssn_avgs.pc$rotation)
wtryr_grwssn_avgs_loadings
```

```
##                                varnames        PC1         PC2         PC3
## cwd                                 cwd -0.2467869 -0.24204061  0.59416539
## pck                                 pck  0.3717771  0.08522297  0.39404325
## ppt                                 ppt  0.3750428  0.21398227 -0.22673679
## ann_tmean                     ann_tmean -0.2478222 -0.29889638 -0.59815700
## mean_diurnal_range   mean_diurnal_range -0.1231616 -0.36651876  0.05755816
## temp_seasonality       temp_seasonality  0.3487722 -0.36848150  0.05050116
## temp_ann_range           temp_ann_range  0.1900110 -0.63894945 -0.10377049
## ann_ppt                         ann_ppt  0.4322113 -0.09288147 -0.17124771
## ppt_seasonality         ppt_seasonality -0.2428413 -0.28967722  0.14155060
## ppt_warmest_quarter ppt_warmest_quarter  0.4243956 -0.16767546  0.12846037
##                             PC4         PC5          PC6         PC7
## cwd                  0.07587132 -0.21552759  0.545649558  0.39266624
## pck                 -0.07114227  0.30196995 -0.198779712  0.35181658
## ppt                 -0.08504118  0.38413787  0.519292286  0.08853978
## ann_tmean            0.12458551 -0.05881532  0.308747028  0.28104490
## mean_diurnal_range  -0.78726079  0.27320311  0.170452081 -0.29604264
## temp_seasonality     0.19465690 -0.37979997  0.084490042 -0.47211213
## temp_ann_range      -0.06611719  0.03964960 -0.432233513  0.39731682
## ann_ppt              0.11394879  0.09657246  0.174414257  0.19170516
## ppt_seasonality      0.53315648  0.68740365  0.003095675 -0.28113417
## ppt_warmest_quarter  0.08453563 -0.09503050  0.210513124 -0.22590645
##                              PC8          PC9        PC10
## cwd                 -0.148057214 -0.008478785 -0.03685687
## pck                  0.508041060  0.422130549  0.05842525
## ppt                 -0.431280421  0.193681816  0.32608555
## ann_tmean            0.490618566  0.203102032  0.12729714
## mean_diurnal_range   0.114381154  0.010889959 -0.15121509
## temp_seasonality    -0.078936179  0.562662857 -0.07099867
## temp_ann_range      -0.368140878 -0.102958251  0.21934666
## ann_ppt              0.023979689 -0.249178788 -0.78936292
## ppt_seasonality     -0.006499217 -0.013638353 -0.03201181
## ppt_warmest_quarter  0.371220992 -0.594463011  0.41539545
```


``` r
autoplot(wtryr_grwssn_avgs.pc, data = wtryr_grwssn_avgs,
         colour='elev_m', alpha=0.5,
         #label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
   scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic()
```

![](PCAs_Combined_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


``` r
autoplot(wtryr_grwssn_avgs.pc, data = wtryr_grwssn_avgs,
         colour='Season', alpha=0.5,
         #label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic() +
  geom_encircle(aes(group=Season, colour = Season))
```

![](PCAs_Combined_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


``` r
autoplot(wtryr_grwssn_avgs.pc, data = wtryr_grwssn_avgs,
          x=1, y=3,
         colour='Season', alpha=0.5,
         #label=TRUE, label.label="parent.pop",
         loadings=TRUE, loadings.colour='black', loadings.linewidth = 0.7,
         loadings.label = TRUE, loadings.label.size=6, loadings.label.colour="black", 
         loadings.label.vjust = -0.2, loadings.label.repel=TRUE) +
  geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
  theme_classic() +
  geom_encircle(aes(group=Season, colour = Season))
```

![](PCAs_Combined_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


## WL2-only PCA to show climate variation across years - NEED TO DO THIS 
Need to save the dataframes with all years included used to make the previous PCAs 

### Water Year


### Growth Season 

