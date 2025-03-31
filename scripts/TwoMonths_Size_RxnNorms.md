---
title: "Single Time Size Reaction Norms"
author: "Brandie Quarles"
date: "2025-03-31"
output: 
  html_document: 
    keep_md: yes
---



To Do:

-   Try using ggcirlce to make different sized points for environmental distance

-   Lines = elevation Points = distance from home, filter by site? - how to represent it though? dark black = close, light gray = far?

# Reaction Norms of Size (between UCD and WL2) - Two Months Post-Transplanting

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
library(lmerTest) #for mixed effect models
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
library(emmeans) #for post-hoc pairwise comparisons 
```

```
## Welcome to emmeans.
## Caution: You lose important information if you filter this package's results.
## See '? untidy'
```

``` r
library(naniar) #replaces values with NA
library(ggnewscale)
library(corrplot) #plotting correlations 
```

```
## corrplot 0.94 loaded
```

``` r
library(geosphere) #for calculating geographic distance
conflicted::conflicts_prefer(lmerTest::lmer)
```

```
## [conflicted] Will prefer lmerTest::lmer over any other package.
```

``` r
conflicted::conflicts_prefer(dplyr::filter)
```

```
## [conflicted] Will prefer dplyr::filter over any other package.
```

``` r
sem <- function(x, na.rm=FALSE) {           #for calculating standard error
  sd(x,na.rm=na.rm)/sqrt(length(na.omit(x)))
} 

elev_three_palette <- c("#0043F0", "#C9727F", "#F5A540") #colors from Gremer et al 2019
elev_order <- c("High", "Mid", "Low") #for proper arrangement in figures 
```

## Load Size data from both Gardens


``` r
WL2_twomonths_size <- read_csv("../input/WL2_Data/CorrectedCSVs/WL2_size_survey_20230913_corrected.csv", 
                               na = c("", "NA", "-", "N/A")) %>% 
  filter(!is.na(pop)) %>% 
  rename(parent.pop=pop) %>% 
  mutate(parent.pop= str_replace(parent.pop, ".*VTR.*", "LVTR1")) %>% 
  unite(BedLoc, bed:bed.col, sep="_", remove = FALSE) %>% 
  filter(BedLoc!="K_5_C") %>% 
  filter(BedLoc!="B_32_A") %>% 
  unite(Genotype, parent.pop:rep, sep="_", remove = FALSE) %>% 
  filter(!str_detect(Genotype, ".*buff*")) %>% 
  unite(pop.mf, parent.pop:mf, sep="_", remove = FALSE)
```

```
## Rows: 1826 Columns: 11
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (8): block, bed, bed.col, pop, mf, rep, herbiv.y.n, survey.notes
## dbl (3): bed.row, height.cm, long.leaf.cm
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
head(WL2_twomonths_size)
```

```
## # A tibble: 6 × 14
##   block BedLoc bed   bed.row bed.col Genotype  pop.mf  parent.pop mf    rep  
##   <chr> <chr>  <chr>   <dbl> <chr>   <chr>     <chr>   <chr>      <chr> <chr>
## 1 A     A_1_A  A           1 A       TM2_6_11  TM2_6   TM2        6     11   
## 2 A     A_1_B  A           1 B       LVTR1_7_1 LVTR1_7 LVTR1      7     1    
## 3 A     A_2_A  A           2 A       SQ2_6_14  SQ2_6   SQ2        6     14   
## 4 A     A_2_B  A           2 B       YO8_8_3   YO8_8   YO8        8     3    
## 5 A     A_3_A  A           3 A       CC_2_3    CC_2    CC         2     3    
## 6 A     A_3_B  A           3 B       YO11_5_14 YO11_5  YO11       5     14   
## # ℹ 4 more variables: height.cm <dbl>, long.leaf.cm <dbl>, herbiv.y.n <chr>,
## #   survey.notes <chr>
```

``` r
dim(WL2_twomonths_size) #2 extra rows...
```

```
## [1] 1575   14
```

``` r
#summary(WL2_twomonths_size)
WL2_twomonths_size %>% rowwise() %>% filter(!is.na(mf), mf != "buffer") %>%  filter(is.na(as.numeric(mf)))
```

```
## # A tibble: 0 × 14
## # Rowwise: 
## # ℹ 14 variables: block <chr>, BedLoc <chr>, bed <chr>, bed.row <dbl>,
## #   bed.col <chr>, Genotype <chr>, pop.mf <chr>, parent.pop <chr>, mf <chr>,
## #   rep <chr>, height.cm <dbl>, long.leaf.cm <dbl>, herbiv.y.n <chr>,
## #   survey.notes <chr>
```

``` r
WL2_twomonths_size %>% rowwise() %>% filter(!is.na(rep), rep != "buffer") %>%  filter(is.na(as.numeric(rep))) #all buffer rows 
```

```
## # A tibble: 0 × 14
## # Rowwise: 
## # ℹ 14 variables: block <chr>, BedLoc <chr>, bed <chr>, bed.row <dbl>,
## #   bed.col <chr>, Genotype <chr>, pop.mf <chr>, parent.pop <chr>, mf <chr>,
## #   rep <chr>, height.cm <dbl>, long.leaf.cm <dbl>, herbiv.y.n <chr>,
## #   survey.notes <chr>
```

``` r
unique(WL2_twomonths_size$parent.pop) #all pops with * are buffers
```

```
##  [1] "TM2"   "LVTR1" "SQ2"   "YO8"   "CC"    "YO11"  "BH"    "DPR"   "CP2"  
## [10] "WL1"   "IH"    "CP3"   "SC"    "FR"    "LV3"   "YO7"   "WV"    "SQ3"  
## [19] "WL2"   "LV1"   "YO4"   "WR"    "SQ1"
```

``` r
UCD_twomonths_size <- read_csv("../input/UCD_Data/CorrectedCSVs/Size_survey_transplants_20230127_corrected.csv", 
                               na = c("", "NA", "-", "N/A")) %>% 
  rename(height.cm = `height (cm)`, long.leaf.cm = `longest leaf (cm)`, parent.pop=pop) %>% 
  mutate(parent.pop= str_replace(parent.pop, ".*VTR.*", "LVTR1")) %>% 
  unite(BedLoc, block:col, sep="_", remove = FALSE) %>% 
  filter(rep != 100)  %>% #get rid of individuals that germinated in the field 
  unite(Genotype, parent.pop:rep, sep="_", remove = FALSE) %>% 
  filter(!str_detect(Genotype, ".*buff*")) %>% 
  unite(pop.mf, parent.pop:mf, sep="_", remove = FALSE)
```

```
## New names:
## Rows: 858 Columns: 10
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): block, col, pop, Notes, ...10 dbl (5): row, mf, rep, height (cm), longest
## leaf (cm)
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## • `` -> `...10`
```

``` r
head(UCD_twomonths_size)
```

```
## # A tibble: 6 × 13
##   BedLoc block   row col   Genotype  pop.mf parent.pop    mf   rep height.cm
##   <chr>  <chr> <dbl> <chr> <chr>     <chr>  <chr>      <dbl> <dbl>     <dbl>
## 1 D1_3_A D1        3 A     WL2_4_11  WL2_4  WL2            4    11       1.3
## 2 D1_3_B D1        3 B     CP2_10_4  CP2_10 CP2           10     4      NA  
## 3 D1_4_A D1        4 A     YO11_4_10 YO11_4 YO11           4    10      NA  
## 4 D1_4_B D1        4 B     CC_5_12   CC_5   CC             5    12       3.7
## 5 D1_5_A D1        5 A     FR_3_6    FR_3   FR             3     6       4.1
## 6 D1_5_B D1        5 B     BH_5_24   BH_5   BH             5    24       3.9
## # ℹ 3 more variables: long.leaf.cm <dbl>, Notes <chr>, ...10 <chr>
```

``` r
dim(UCD_twomonths_size) #correct number of rows 
```

```
## [1] 757  13
```

``` r
#summary(UCD_twomonths_size)
unique(UCD_twomonths_size$parent.pop) #all pops with * are buffers
```

```
##  [1] "WL2"   "CP2"   "YO11"  "CC"    "FR"    "BH"    "IH"    "LV3"   "SC"   
## [10] "LVTR1" "SQ3"   "TM2"   "WL1"   "YO7"   "DPR"   "SQ2"   "SQ1"   "YO8"  
## [19] "YO4"   "WR"    "WV"    "CP3"   "LV1"
```

``` r
UCD_twomonths_size %>% filter(!is.na(...10)) #extra notes 
```

```
## # A tibble: 2 × 13
##   BedLoc  block   row col   Genotype pop.mf parent.pop    mf   rep height.cm
##   <chr>   <chr> <dbl> <chr> <chr>    <chr>  <chr>      <dbl> <dbl>     <dbl>
## 1 H2_28_A H2       28 A     WL1_1_8  WL1_1  WL1            1     8      NA  
## 2 L1_17_B L1       17 B     FR_2_1   FR_2   FR             2     1       1.7
## # ℹ 3 more variables: long.leaf.cm <dbl>, Notes <chr>, ...10 <chr>
```

### Merge the Data


``` r
WL2_twomonths_size_prep <- WL2_twomonths_size %>% 
  filter(BedLoc!="C_4_D", BedLoc!="C_5_D") %>% 
  select(block:BedLoc, Genotype:long.leaf.cm) %>% 
  mutate(Site="WL2") %>%
  mutate(mf=as.double(mf), rep=as.double(rep)) 
names(WL2_twomonths_size_prep)
```

```
##  [1] "block"        "BedLoc"       "Genotype"     "pop.mf"       "parent.pop"  
##  [6] "mf"           "rep"          "height.cm"    "long.leaf.cm" "Site"
```

``` r
UCD_twomonths_size_prep <- UCD_twomonths_size %>% 
  select(BedLoc:block, Genotype:long.leaf.cm) %>% mutate(Site="UCD")
names(UCD_twomonths_size_prep)
```

```
##  [1] "BedLoc"       "block"        "Genotype"     "pop.mf"       "parent.pop"  
##  [6] "mf"           "rep"          "height.cm"    "long.leaf.cm" "Site"
```

``` r
twomonths_rxnnorms <- bind_rows(UCD_twomonths_size_prep, WL2_twomonths_size_prep) %>% 
  arrange(pop.mf, Site)
head(twomonths_rxnnorms)
```

```
## # A tibble: 6 × 10
##   BedLoc  block Genotype pop.mf parent.pop    mf   rep height.cm long.leaf.cm
##   <chr>   <chr> <chr>    <chr>  <chr>      <dbl> <dbl>     <dbl>        <dbl>
## 1 D1_9_C  D1    BH_1_8   BH_1   BH             1     8       2.1          2.2
## 2 D2_30_D D2    BH_1_10  BH_1   BH             1    10       2.3          1.8
## 3 J2_37_B J2    BH_1_7   BH_1   BH             1     7       3            3.2
## 4 A_7_D   A     BH_1_3   BH_1   BH             1     3      NA           NA  
## 5 A_37_D  B     BH_1_4   BH_1   BH             1     4       2.1          3.4
## 6 B_6_C   D     BH_1_6   BH_1   BH             1     6      NA           NA  
## # ℹ 1 more variable: Site <chr>
```

## Load the pop and location data


``` r
ucd_gowers_size <- read_csv("../output/Climate/Gowers_UCD.csv") %>% 
  select(parent.pop:GrwSsn_GD, Wtr_Year_GD) %>% 
  pivot_wider(names_from = TimePd, values_from = c(GrwSsn_GD, Wtr_Year_GD)) %>% 
  mutate(Site="UCD", Garden_Lat=38.53250, Garden_Long=-121.7830, Garden_Elev=16) %>% 
  mutate(Geographic_Dist=distHaversine(cbind(Garden_Long, Garden_Lat), cbind(Long, Lat)),
         Elev_Dist=elev_m-Garden_Elev) # Calculate the distance using the haversine formula (dist in meters)
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
wl2_gowers_2023_size <- read_csv("../output/Climate/Gowers_WL2.csv") %>% 
  select(parent.pop:GrwSsn_GD, Wtr_Year_GD) %>% 
  pivot_wider(names_from = TimePd, values_from = c(GrwSsn_GD, Wtr_Year_GD)) %>% 
  mutate(Site="WL2", Garden_Lat=38.82599, Garden_Long=-120.2509, Garden_Elev=2020) %>% 
  mutate(Geographic_Dist=distHaversine(cbind(Garden_Long, Garden_Lat), cbind(Long, Lat)),
         Elev_Dist=elev_m-Garden_Elev) # Calculate the distance using the haversine formula
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
gowersdist_all <- bind_rows(ucd_gowers_size, wl2_gowers_2023_size) %>% 
  select(Site, Garden_Lat:Garden_Elev, parent.pop:Wtr_Year_GD_Historical, Geographic_Dist:Elev_Dist)
head(gowersdist_all)
```

```
## # A tibble: 6 × 15
##   Site  Garden_Lat Garden_Long Garden_Elev parent.pop elevation.group elev_m
##   <chr>      <dbl>       <dbl>       <dbl> <chr>      <chr>            <dbl>
## 1 UCD         38.5       -122.          16 WL1        Mid              1614.
## 2 UCD         38.5       -122.          16 IH         Low               454.
## 3 UCD         38.5       -122.          16 CC         Low               313 
## 4 UCD         38.5       -122.          16 BH         Low               511.
## 5 UCD         38.5       -122.          16 WR         Mid              1158 
## 6 UCD         38.5       -122.          16 FR         Mid               787 
## # ℹ 8 more variables: Lat <dbl>, Long <dbl>, GrwSsn_GD_Recent <dbl>,
## #   GrwSsn_GD_Historical <dbl>, Wtr_Year_GD_Recent <dbl>,
## #   Wtr_Year_GD_Historical <dbl>, Geographic_Dist <dbl>, Elev_Dist <dbl>
```

## Add in location info


``` r
twomonths_rxnnorms_loc <-left_join(twomonths_rxnnorms, gowersdist_all) 
```

```
## Joining with `by = join_by(parent.pop, Site)`
```

``` r
head(twomonths_rxnnorms_loc)
```

```
## # A tibble: 6 × 23
##   BedLoc  block Genotype pop.mf parent.pop    mf   rep height.cm long.leaf.cm
##   <chr>   <chr> <chr>    <chr>  <chr>      <dbl> <dbl>     <dbl>        <dbl>
## 1 D1_9_C  D1    BH_1_8   BH_1   BH             1     8       2.1          2.2
## 2 D2_30_D D2    BH_1_10  BH_1   BH             1    10       2.3          1.8
## 3 J2_37_B J2    BH_1_7   BH_1   BH             1     7       3            3.2
## 4 A_7_D   A     BH_1_3   BH_1   BH             1     3      NA           NA  
## 5 A_37_D  B     BH_1_4   BH_1   BH             1     4       2.1          3.4
## 6 B_6_C   D     BH_1_6   BH_1   BH             1     6      NA           NA  
## # ℹ 14 more variables: Site <chr>, Garden_Lat <dbl>, Garden_Long <dbl>,
## #   Garden_Elev <dbl>, elevation.group <chr>, elev_m <dbl>, Lat <dbl>,
## #   Long <dbl>, GrwSsn_GD_Recent <dbl>, GrwSsn_GD_Historical <dbl>,
## #   Wtr_Year_GD_Recent <dbl>, Wtr_Year_GD_Historical <dbl>,
## #   Geographic_Dist <dbl>, Elev_Dist <dbl>
```

``` r
#write_csv(twomonths_rxnnorms_loc, "../output/TwoMonths_Size_BothSites.csv")
```

## Plot Reaction Norms

### Means by maternal family


``` r
twomonths_rxnnorms_summary_mfs <- twomonths_rxnnorms_loc %>% 
  group_by(pop.mf, parent.pop, Site, elev_m) %>% 
  summarise(N_height = sum(!is.na(height.cm)), mean_height.cm = mean(height.cm,na.rm=(TRUE)), 
            sem_height.cm=sem(height.cm, na.rm=(TRUE)), N_length = sum(!is.na(long.leaf.cm)),
            mean_long.leaf.cm=mean(long.leaf.cm, na.rm=(TRUE)), 
            sem_long.leaf.cm=sem(long.leaf.cm, na.rm=TRUE))
```

```
## `summarise()` has grouped output by 'pop.mf', 'parent.pop', 'Site'. You can
## override using the `.groups` argument.
```

``` r
#twomonths_rxnnorms_summary_mfs %>% arrange(N_height)

#restrict to mfs with plants at both sites
twomonths_rxnnorms_summary_mfs_wide <- twomonths_rxnnorms_summary_mfs %>% 
  select(pop.mf, Site, N_height) %>% 
  spread(Site, N_height) %>% 
  mutate(Both.Sites=if_else(!is.na(UCD) & !is.na(WL2), TRUE, FALSE)) %>% 
  filter(Both.Sites != "FALSE")
```

```
## Adding missing grouping variables: `parent.pop`
```

``` r
#twomonths_rxnnorms_summary_mfs_wide %>% arrange(Both.Sites)

twomonths_rxnnorms_summary_mfs_bothsites <- left_join(twomonths_rxnnorms_summary_mfs_wide, twomonths_rxnnorms_summary_mfs)
```

```
## Joining with `by = join_by(parent.pop, pop.mf)`
```

``` r
#twomonths_rxnnorms_summary_mfs_bothsites

twomonths_rxnnorms_summary_mfs2 <- twomonths_rxnnorms_summary_mfs_bothsites %>% 
  mutate(Site=str_replace_all(Site, "UCD", "Low Elev"), 
         Site=str_replace_all(Site, "WL2", "High Elev")) 
twomonths_rxnnorms_summary_mfs2$Site <- factor(twomonths_rxnnorms_summary_mfs2$Site,
                                               levels = c('Low Elev','High Elev'))

twomonths_rxnnorms_summary_mfs2 %>% filter(N_height!=1) %>% filter(is.na(sem_height.cm))
```

```
## # A tibble: 21 × 13
## # Groups:   pop.mf, parent.pop [20]
##    parent.pop pop.mf   UCD   WL2 Both.Sites Site  elev_m N_height mean_height.cm
##    <chr>      <chr>  <int> <int> <lgl>      <fct>  <dbl>    <int>          <dbl>
##  1 CP3        CP3_7      0     1 TRUE       Low …  2266.        0            NaN
##  2 DPR        DPR_3      0     5 TRUE       Low …  1019.        0            NaN
##  3 FR         FR_1       6     0 TRUE       High…   787         0            NaN
##  4 FR         FR_3       3     0 TRUE       High…   787         0            NaN
##  5 LV1        LV1_6      0     3 TRUE       Low …  2593.        0            NaN
##  6 LV3        LV3_1     11     0 TRUE       High…  2354.        0            NaN
##  7 LV3        LV3_2      3     0 TRUE       High…  2354.        0            NaN
##  8 LVTR1      LVTR1…     2     0 TRUE       High…  2741.        0            NaN
##  9 SQ1        SQ1_3      0     0 TRUE       Low …  1921.        0            NaN
## 10 SQ1        SQ1_3      0     0 TRUE       High…  1921.        0            NaN
## # ℹ 11 more rows
## # ℹ 4 more variables: sem_height.cm <dbl>, N_length <int>,
## #   mean_long.leaf.cm <dbl>, sem_long.leaf.cm <dbl>
```

``` r
#20 maternal families with 0 plants left alive at one of the sites by these measurements. 
```

### Plot mf avgs


``` r
twomonths_rxnnorms_summary_mfs %>% 
   ggplot(aes(x=Site, y=mean_height.cm, group=pop.mf, color=pop.mf)) + 
  geom_point(size=0.8) + geom_line(linewidth=0.8) +
  geom_errorbar(aes(ymin=mean_height.cm-sem_height.cm,ymax=mean_height.cm+sem_height.cm),width=.2) +
  theme_classic() +
   theme(text=element_text(size=25))

twomonths_rxnnorms_summary_mfs %>% 
   ggplot(aes(x=Site, y=mean_long.leaf.cm, group=pop.mf, color=pop.mf)) + 
  geom_point(size=0.8) + geom_line(linewidth=0.8) +
  geom_errorbar(aes(ymin=mean_long.leaf.cm-sem_long.leaf.cm,ymax=mean_long.leaf.cm+sem_long.leaf.cm),width=.2) +
  theme_classic() +
   theme(text=element_text(size=25))
```


``` r
#note that there are a lot of maternal families with only 1 plant - removed those from these figures 
twomonths_rxnnorms_summary_mfs2 %>% 
  filter(N_height != 1) %>% 
  ggplot(aes(x=Site, y=mean_height.cm, group=pop.mf, color=elev_m)) + 
  geom_point(size=0.8) + geom_line(linewidth=0.8) +
  geom_errorbar(aes(ymin=mean_height.cm-sem_height.cm,ymax=mean_height.cm+sem_height.cm),width=.1) +
  theme_classic() + scale_colour_gradient(low = "#F5A540", high = "#0043F0")  +
  labs(y="Height (cm)", color="Elevation (m)") +
  theme(text=element_text(size=28))
```

```
## Warning: Removed 21 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

```
## Warning: Removed 21 rows containing missing values or values outside the scale range
## (`geom_line()`).
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

``` r
ggsave("../output/TwoMonths_RxNorms_Height_MFAvgs.png", width = 14, height = 8, units = "in")
```

```
## Warning: Removed 21 rows containing missing values or values outside the scale range
## (`geom_point()`).
## Removed 21 rows containing missing values or values outside the scale range
## (`geom_line()`).
```

``` r
twomonths_rxnnorms_summary_mfs2 %>% 
  filter(N_length != 1) %>% 
  ggplot(aes(x=Site, y=mean_long.leaf.cm, group=pop.mf, color=elev_m)) + 
  geom_point(size=0.8) + geom_line(linewidth=0.8) +
  geom_errorbar(aes(ymin=mean_long.leaf.cm-sem_long.leaf.cm,ymax=mean_long.leaf.cm+sem_long.leaf.cm),
                width=.1) +
  theme_classic() + scale_colour_gradient(low = "#F5A540", high = "#0043F0")  +
  labs(y="Leaf Length (cm)", color="Elevation (m)") +
  theme(text=element_text(size=28))
```

```
## Warning: Removed 25 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

```
## Warning: Removed 25 rows containing missing values or values outside the scale range
## (`geom_line()`).
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

``` r
ggsave("../output/TwoMonths_RxNorms_LongestLength_MFAvgs.png", width = 14, height = 8, units = "in")
```

```
## Warning: Removed 25 rows containing missing values or values outside the scale range
## (`geom_point()`).
## Removed 25 rows containing missing values or values outside the scale range
## (`geom_line()`).
```


``` r
twomonths_rxnnorms_summary_mfs2 %>% 
  filter(N_height != 1) %>% 
  filter(parent.pop=="BH"|parent.pop=="WL1"|parent.pop=="CP2") %>%  
  ggplot(aes(x=Site, y=mean_height.cm, group=pop.mf, color=elev_m)) + 
  geom_point(size=0.8) + geom_line(linewidth=0.8) +
  geom_errorbar(aes(ymin=mean_height.cm-sem_height.cm,ymax=mean_height.cm+sem_height.cm),width=.1) +
  theme_classic() + scale_colour_gradient(low = "#F5A540", high = "#0043F0")  +
  labs(y="Height (cm)", color="Elevation (m)") +
  theme(text=element_text(size=28))
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

``` r
#ggsave("../output/TwoMonths_RxNorms_Height_BH_WL1_CP2_MFAvgs.png", width = 14, height = 8, units = "in")

twomonths_rxnnorms_summary_mfs2 %>% 
  filter(N_length != 1) %>% 
  filter(parent.pop=="BH"|parent.pop=="WL1"|parent.pop=="CP2") %>%  
  ggplot(aes(x=Site, y=mean_long.leaf.cm, group=pop.mf, color=elev_m)) + 
  geom_point(size=0.8) + geom_line(linewidth=0.8) +
  geom_errorbar(aes(ymin=mean_long.leaf.cm-sem_long.leaf.cm,ymax=mean_long.leaf.cm+sem_long.leaf.cm),
                width=.1) +
  theme_classic() + scale_colour_gradient(low = "#F5A540", high = "#0043F0")  +
  labs(y="Leaf Length (cm)", color="Elevation (m)") +
  theme(text=element_text(size=28))
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

``` r
#ggsave("../output/TwoMonths_RxNorms_LongestLength_BH_WL1_CP2_MFAvgs.png", width = 14, height = 8, units = "in")
```

### Means by Pop


``` r
twomonths_rxnnorms_summary_pops <- twomonths_rxnnorms_loc %>% 
  filter(!is.na(parent.pop)) %>% 
  group_by(parent.pop, Site, elev_m, Wtr_Year_GD_Recent, Wtr_Year_GD_Historical) %>% 
  summarise(N_height = sum(!is.na(height.cm)), mean_height.cm = mean(height.cm,na.rm=(TRUE)), 
            sem_height.cm=sem(height.cm, na.rm=(TRUE)), N_length = sum(!is.na(long.leaf.cm)),
            mean_long.leaf.cm=mean(long.leaf.cm, na.rm=(TRUE)), 
            sem_long.leaf.cm=sem(long.leaf.cm, na.rm=TRUE))
```

```
## `summarise()` has grouped output by 'parent.pop', 'Site', 'elev_m',
## 'Wtr_Year_GD_Recent'. You can override using the `.groups` argument.
```

``` r
#twomonths_rxnnorms_summary_pops

twomonths_rxnnorms_summary_pops %>% arrange(N_height)
```

```
## # A tibble: 46 × 11
## # Groups:   parent.pop, Site, elev_m, Wtr_Year_GD_Recent [46]
##    parent.pop Site  elev_m Wtr_Year_GD_Recent Wtr_Year_GD_Historical N_height
##    <chr>      <chr>  <dbl>              <dbl>                  <dbl>    <int>
##  1 WV         WL2     749.              0.481                  0.493        0
##  2 LV1        UCD    2593.              0.791                  0.789        1
##  3 WV         UCD     749.              0.410                  0.441        1
##  4 WR         WL2    1158               0.418                  0.407        3
##  5 YO4        UCD    2158.              0.540                  0.525        5
##  6 CP3        UCD    2266.              0.636                  0.640        6
##  7 WR         UCD    1158               0.515                  0.483        7
##  8 LV3        WL2    2354.              0.373                  0.466        8
##  9 YO11       UCD    2872.              0.579                  0.583        8
## 10 LVTR1      UCD    2741.              0.801                  0.801        9
## # ℹ 36 more rows
## # ℹ 5 more variables: mean_height.cm <dbl>, sem_height.cm <dbl>,
## #   N_length <int>, mean_long.leaf.cm <dbl>, sem_long.leaf.cm <dbl>
```

``` r
twomonths_rxnnorms_summary_pops %>% filter(elev_m>2800) %>% filter(Site=="WL2") %>% summary()
```

```
##   parent.pop            Site               elev_m     Wtr_Year_GD_Recent
##  Length:1           Length:1           Min.   :2872   Min.   :0.4605    
##  Class :character   Class :character   1st Qu.:2872   1st Qu.:0.4605    
##  Mode  :character   Mode  :character   Median :2872   Median :0.4605    
##                                        Mean   :2872   Mean   :0.4605    
##                                        3rd Qu.:2872   3rd Qu.:0.4605    
##                                        Max.   :2872   Max.   :0.4605    
##  Wtr_Year_GD_Historical    N_height  mean_height.cm  sem_height.cm   
##  Min.   :0.5558         Min.   :28   Min.   :2.625   Min.   :0.2669  
##  1st Qu.:0.5558         1st Qu.:28   1st Qu.:2.625   1st Qu.:0.2669  
##  Median :0.5558         Median :28   Median :2.625   Median :0.2669  
##  Mean   :0.5558         Mean   :28   Mean   :2.625   Mean   :0.2669  
##  3rd Qu.:0.5558         3rd Qu.:28   3rd Qu.:2.625   3rd Qu.:0.2669  
##  Max.   :0.5558         Max.   :28   Max.   :2.625   Max.   :0.2669  
##     N_length  mean_long.leaf.cm sem_long.leaf.cm
##  Min.   :22   Min.   :1.941     Min.   :0.2728  
##  1st Qu.:22   1st Qu.:1.941     1st Qu.:0.2728  
##  Median :22   Median :1.941     Median :0.2728  
##  Mean   :22   Mean   :1.941     Mean   :0.2728  
##  3rd Qu.:22   3rd Qu.:1.941     3rd Qu.:0.2728  
##  Max.   :22   Max.   :1.941     Max.   :0.2728
```

``` r
twomonths_rxnnorms_summary_pops %>% filter(elev_m>2800) %>% filter(Site=="UCD") %>% summary()
```

```
##   parent.pop            Site               elev_m     Wtr_Year_GD_Recent
##  Length:1           Length:1           Min.   :2872   Min.   :0.5789    
##  Class :character   Class :character   1st Qu.:2872   1st Qu.:0.5789    
##  Mode  :character   Mode  :character   Median :2872   Median :0.5789    
##                                        Mean   :2872   Mean   :0.5789    
##                                        3rd Qu.:2872   3rd Qu.:0.5789    
##                                        Max.   :2872   Max.   :0.5789    
##  Wtr_Year_GD_Historical    N_height mean_height.cm  sem_height.cm   
##  Min.   :0.5831         Min.   :8   Min.   :2.438   Min.   :0.3041  
##  1st Qu.:0.5831         1st Qu.:8   1st Qu.:2.438   1st Qu.:0.3041  
##  Median :0.5831         Median :8   Median :2.438   Median :0.3041  
##  Mean   :0.5831         Mean   :8   Mean   :2.438   Mean   :0.3041  
##  3rd Qu.:0.5831         3rd Qu.:8   3rd Qu.:2.438   3rd Qu.:0.3041  
##  Max.   :0.5831         Max.   :8   Max.   :2.438   Max.   :0.3041  
##     N_length mean_long.leaf.cm sem_long.leaf.cm
##  Min.   :8   Min.   :1.525     Min.   :0.1623  
##  1st Qu.:8   1st Qu.:1.525     1st Qu.:0.1623  
##  Median :8   Median :1.525     Median :0.1623  
##  Mean   :8   Mean   :1.525     Mean   :0.1623  
##  3rd Qu.:8   3rd Qu.:1.525     3rd Qu.:0.1623  
##  Max.   :8   Max.   :1.525     Max.   :0.1623
```

``` r
twomonths_rxnnorms_summary_pops2 <- twomonths_rxnnorms_summary_pops %>% 
  mutate(Site=str_replace_all(Site, "UCD", "Low Elev"), 
         Site=str_replace_all(Site, "WL2", "High Elev"))
twomonths_rxnnorms_summary_pops2$Site <- factor(twomonths_rxnnorms_summary_pops2$Site,
                                               levels = c('Low Elev','High Elev'))
#no data for WV at high elevation (dead by the time of these measurements)
```

### Plot Pop Avgs


``` r
twomonths_rxnnorms_summary_pops %>% 
   ggplot(aes(x=Site, y=mean_height.cm, group=parent.pop, color=parent.pop)) + 
  geom_point(size=0.8) + geom_line(linewidth=0.8) +
  geom_errorbar(aes(ymin=mean_height.cm-sem_height.cm,ymax=mean_height.cm+sem_height.cm),width=.2) +
  theme_classic() +
   theme(text=element_text(size=25))
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

``` r
ggsave("../output/TwoMonths_RxNorms_Height_ALL_PopAvgs2.png", width = 12, height = 8, units = "in")
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
## Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
```

``` r
twomonths_rxnnorms_summary_pops %>% 
   ggplot(aes(x=Site, y=mean_long.leaf.cm, group=parent.pop, color=parent.pop)) + 
  geom_point(size=0.8) + geom_line(linewidth=0.8) +
  geom_errorbar(aes(ymin=mean_long.leaf.cm-sem_long.leaf.cm,ymax=mean_long.leaf.cm+sem_long.leaf.cm),width=.2) +
  theme_classic() +
   theme(text=element_text(size=25))
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
## Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

``` r
ggsave("../output/TwoMonths_RxNorms_LongestLength_ALL_PopAvgs2.png", width = 12, height = 8, units = "in")
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
## Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
```


``` r
twomonths_rxnnorms_summary_pops2 %>% 
  filter(N_height != 1) %>% 
  ggplot(aes(x=Site, y=mean_height.cm, group=parent.pop, color=elev_m)) + 
  geom_point(size=1.5) + geom_line(linewidth=1.5) +
  geom_errorbar(aes(ymin=mean_height.cm-sem_height.cm,ymax=mean_height.cm+sem_height.cm),width=.1) +
  theme_classic() + scale_colour_gradient(low = "#F5A540", high = "#0043F0")  +
  labs(y="Height (cm)", color="Elevation (m)") +
 # ylim(0,4) +
  theme(text=element_text(size=28))
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

``` r
ggsave("../output/TwoMonths_RxNorms_Height_ALL_PopAvgs.png", width = 14, height = 8, units = "in")
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
## Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
```

``` r
twomonths_rxnnorms_summary_pops2 %>% 
  filter(N_length != 1) %>% 
  ggplot(aes(x=Site, y=mean_long.leaf.cm, group=parent.pop, color=elev_m)) + 
  geom_point(size=1.5) + geom_line(linewidth=1.5) +
  geom_errorbar(aes(ymin=mean_long.leaf.cm-sem_long.leaf.cm,
                    ymax=mean_long.leaf.cm+sem_long.leaf.cm),width=.1) +
  theme_classic() + scale_colour_gradient(low = "#F5A540", high = "#0043F0")  +
  labs(y="Leaf Length (cm)", color="Elevation (m)") +
  theme(text=element_text(size=28))
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
## Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-12-2.png)<!-- -->

``` r
ggsave("../output/TwoMonths_RxNorms_LongestLength_ALL_PopAvgs.png", width = 14, height = 8, units = "in")
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
## Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
```

### By Gowers Distance


``` r
ggplot() +
  geom_line(data = filter(twomonths_rxnnorms_summary_pops2, N_height != 1), linewidth=1.5, aes(x = Site, y = mean_height.cm, group=parent.pop, color=elev_m)) + scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  labs(color="Elevation (m)") +
   
  # start a new scale
  new_scale_colour() +
  
  geom_point(data = filter(twomonths_rxnnorms_summary_pops2, N_height != 1), 
             size=2.5, aes(x = Site,y = mean_height.cm, color=Wtr_Year_GD_Recent), alpha=0.9) +
  geom_errorbar(data = filter(twomonths_rxnnorms_summary_pops2, N_height != 1), 
                aes(x=Site, y=mean_height.cm,ymin=mean_height.cm-sem_height.cm,ymax=mean_height.cm+sem_height.cm,
                    color=Wtr_Year_GD_Recent), alpha=0.9, width=.1) +
  theme_classic() + scale_colour_gradientn(colours = c("black", "grey80")) +
  labs(y="Height (cm)", color="Envtal Dist \n(from garden year)") +
  theme(text=element_text(size=28))
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

``` r
ggsave("../output/TwoMonths_RxNorms_Height_ALL_PopAvgs_Gowers.png", width = 14, height = 8, units = "in")
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
## Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
```


``` r
ggplot() +
  geom_line(data = filter(twomonths_rxnnorms_summary_pops2, N_length != 1), linewidth=1.5, aes(x = Site, y = mean_long.leaf.cm, group=parent.pop, color=elev_m)) + scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  labs(color="Elevation (m)") +
   
  # start a new scale
  new_scale_colour() +
  
  geom_point(data = filter(twomonths_rxnnorms_summary_pops2, N_length != 1), 
             size=2.5, aes(x = Site,y = mean_long.leaf.cm, color=Wtr_Year_GD_Recent), alpha=0.9) +
  geom_errorbar(data = filter(twomonths_rxnnorms_summary_pops2, N_length != 1), 
                aes(x=Site, y=mean_long.leaf.cm,ymin=mean_long.leaf.cm-sem_long.leaf.cm,ymax=mean_long.leaf.cm+sem_long.leaf.cm,
                    color=Wtr_Year_GD_Recent), alpha=0.9, width=.1) +
  theme_classic() + scale_colour_gradientn(colours = c("black", "grey80")) +
  labs(y="Leaf Length (cm)", color="Envtal Dist \n(from garden year)") +
  theme(text=element_text(size=28))
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

``` r
ggsave("../output/TwoMonths_RxNorms_LongestLength_ALL_PopAvgs_Gowers.png", width = 14, height = 8, units = "in")
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
## Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
```

### Combine mf and pop averages


``` r
twomonths_rxnnorms_summary_pops2 %>% 
  mutate(pop.mf=parent.pop) %>% 
  filter(N_height != 1) %>% 
  ggplot(aes(x=Site, y=mean_height.cm, group=pop.mf, color=elev_m)) + 
  geom_point(size=1.5) + geom_line(linewidth=1.5, alpha=0.7) +
  geom_errorbar(aes(ymin=mean_height.cm-sem_height.cm,ymax=mean_height.cm+sem_height.cm),width=.1) +
  theme_classic() + scale_colour_gradient(low = "#F5A540", high = "#0043F0")  +
  labs(y="Height (cm)", color="Elevation (m)") +
  theme(text=element_text(size=28)) +
 geom_line(data = twomonths_rxnnorms_summary_mfs2, size=0.2) +
  geom_point(data = twomonths_rxnnorms_summary_mfs2, size=0.8) 
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
```

```
## Warning: Removed 21 rows containing missing values or values outside the scale range
## (`geom_line()`).
```

```
## Warning: Removed 21 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

``` r
ggsave("../output/TwoMonths_RxNorms_Height_ALL_PopMFAvgs.png", width = 14, height = 8, units = "in")
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
```

```
## Warning: Removed 21 rows containing missing values or values outside the scale range
## (`geom_line()`).
```

```
## Warning: Removed 21 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

``` r
twomonths_rxnnorms_summary_pops2 %>% 
  mutate(pop.mf=parent.pop) %>% 
  filter(N_length != 1) %>% 
  ggplot(aes(x=Site, y=mean_long.leaf.cm, group=pop.mf, color=elev_m)) + 
  geom_point(size=1.5) + geom_line(linewidth=1.5,  alpha=0.7) +
  geom_errorbar(aes(ymin=mean_long.leaf.cm-sem_long.leaf.cm,
                    ymax=mean_long.leaf.cm+sem_long.leaf.cm),width=.1) +
  theme_classic() + scale_colour_gradient(low = "#F5A540", high = "#0043F0")  +
  labs(y="Leaf Length (cm)", color="Elevation (m)") +
  theme(text=element_text(size=28)) +
geom_line(data = twomonths_rxnnorms_summary_mfs2, size=0.2) +
  geom_point(data = twomonths_rxnnorms_summary_mfs2, size=0.8) 
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
```

```
## Warning: Removed 25 rows containing missing values or values outside the scale range
## (`geom_line()`).
```

```
## Warning: Removed 25 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-15-2.png)<!-- -->

``` r
ggsave("../output/TwoMonths_RxNorms_LongestLength_ALL_PopMFAvgs.png", width = 14, height = 8, units = "in")
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_point()`).
```

```
## Warning: Removed 1 row containing missing values or values outside the scale range
## (`geom_line()`).
```

```
## Warning: Removed 25 rows containing missing values or values outside the scale range
## (`geom_line()`).
```

```
## Warning: Removed 25 rows containing missing values or values outside the scale range
## (`geom_point()`).
```


``` r
twomonths_rxnnorms_summary_pops2 %>% 
  filter(parent.pop=="TM2"|parent.pop=="WL2") %>%  
   ggplot(aes(x=Site, y=mean_height.cm, group=parent.pop, color=parent.pop)) + 
  geom_point(size=0.8) + geom_line(linewidth=0.8) +
  geom_errorbar(aes(ymin=mean_height.cm-sem_height.cm,ymax=mean_height.cm+sem_height.cm),width=.2) +
  theme_classic()  +
   theme(text=element_text(size=25))
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

``` r
ggsave("../output/TwoMonths_RxNorms_Height_TM2_WL2_PopAvgs.png", width = 12, height = 8, units = "in")

twomonths_rxnnorms_summary_pops2 %>% 
  filter(parent.pop=="TM2"|parent.pop=="WL2") %>%  
   ggplot(aes(x=Site, y=mean_long.leaf.cm, group=parent.pop, color=parent.pop)) + 
  geom_point(size=0.8) + geom_line(linewidth=0.8) +
  geom_errorbar(aes(ymin=mean_long.leaf.cm-sem_long.leaf.cm,ymax=mean_long.leaf.cm+sem_long.leaf.cm),width=.2) +
  theme_classic()  +
   theme(text=element_text(size=25))
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-16-2.png)<!-- -->

``` r
ggsave("../output/TwoMonths_RxNorms_LongestLength_TM2_WL2_PopAvgs.png", width = 12, height = 8, units = "in")
```

### Means by Elevation


``` r
twomonths_rxnnorms_summary_elev <- twomonths_rxnnorms_loc %>% 
  group_by(elevation.group, Site) %>% 
  summarise(N_height = sum(!is.na(height.cm)), mean_height.cm = mean(height.cm,na.rm=(TRUE)), 
            sem_height.cm=sem(height.cm, na.rm=(TRUE)), N_length = sum(!is.na(long.leaf.cm)),
            mean_long.leaf.cm=mean(long.leaf.cm, na.rm=(TRUE)), 
            sem_long.leaf.cm=sem(long.leaf.cm, na.rm=TRUE))
```

```
## `summarise()` has grouped output by 'elevation.group'. You can override using
## the `.groups` argument.
```

``` r
twomonths_rxnnorms_summary_elev
```

```
## # A tibble: 6 × 8
## # Groups:   elevation.group [3]
##   elevation.group Site  N_height mean_height.cm sem_height.cm N_length
##   <chr>           <chr>    <int>          <dbl>         <dbl>    <int>
## 1 High            UCD        163           1.94        0.0811      163
## 2 High            WL2        284           2.73        0.0885      216
## 3 Low             UCD        243           3.91        0.112       243
## 4 Low             WL2        246           9.53        0.415       246
## 5 Mid             UCD        208           2.87        0.100       207
## 6 Mid             WL2        112           4.01        0.262        96
## # ℹ 2 more variables: mean_long.leaf.cm <dbl>, sem_long.leaf.cm <dbl>
```


``` r
twomonths_rxnnorms_summary_elev$elevation.group <- factor(twomonths_rxnnorms_summary_elev$elevation.group, levels = c('High','Mid', 'Low'))
twomonths_rxnnorms_summary_elev %>% 
   ggplot(aes(x=Site, y=mean_height.cm, group=elevation.group, color=elevation.group)) + 
  geom_point(size=0.8) + geom_line(linewidth=0.8) +
  geom_errorbar(aes(ymin=mean_height.cm-sem_height.cm,ymax=mean_height.cm+sem_height.cm),width=.2) +
  theme_classic() + scale_colour_manual(values=elev_three_palette)
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

``` r
twomonths_rxnnorms_summary_elev %>% 
   ggplot(aes(x=Site, y=mean_long.leaf.cm, group=elevation.group, color=elevation.group)) + 
  geom_point(size=0.8) + geom_line(linewidth=0.8) +
  geom_errorbar(aes(ymin=mean_long.leaf.cm-sem_long.leaf.cm,ymax=mean_long.leaf.cm+sem_long.leaf.cm),width=.2) +
  theme_classic() + scale_colour_manual(values=elev_three_palette)
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

## Stats

Height


``` r
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}
summary(twomonths_rxnnorms_loc)
```

```
##     BedLoc             block             Genotype            pop.mf         
##  Length:2330        Length:2330        Length:2330        Length:2330       
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##   parent.pop              mf              rep           height.cm     
##  Length:2330        Min.   : 1.000   Min.   : 1.000   Min.   : 0.100  
##  Class :character   1st Qu.: 2.000   1st Qu.: 4.000   1st Qu.: 2.100  
##  Mode  :character   Median : 5.000   Median : 8.000   Median : 3.200  
##                     Mean   : 4.513   Mean   : 8.102   Mean   : 4.325  
##                     3rd Qu.: 6.000   3rd Qu.:11.000   3rd Qu.: 5.200  
##                     Max.   :14.000   Max.   :31.000   Max.   :38.100  
##                                                       NA's   :1074    
##   long.leaf.cm       Site             Garden_Lat     Garden_Long    
##  Min.   :0.100   Length:2330        Min.   :38.53   Min.   :-121.8  
##  1st Qu.:1.800   Class :character   1st Qu.:38.53   1st Qu.:-121.8  
##  Median :2.500   Mode  :character   Median :38.83   Median :-120.3  
##  Mean   :2.602                      Mean   :38.73   Mean   :-120.7  
##  3rd Qu.:3.300                      3rd Qu.:38.83   3rd Qu.:-120.3  
##  Max.   :7.600                      Max.   :38.83   Max.   :-120.3  
##  NA's   :1159                                                       
##   Garden_Elev   elevation.group        elev_m            Lat       
##  Min.   :  16   Length:2330        Min.   : 313.0   Min.   :36.56  
##  1st Qu.:  16   Class :character   1st Qu.: 511.4   1st Qu.:37.81  
##  Median :2020   Mode  :character   Median :1921.0   Median :38.79  
##  Mean   :1369                      Mean   :1567.5   Mean   :38.70  
##  3rd Qu.:2020                      3rd Qu.:2353.6   3rd Qu.:39.59  
##  Max.   :2020                      Max.   :2872.3   Max.   :40.74  
##                                                                    
##       Long        GrwSsn_GD_Recent GrwSsn_GD_Historical Wtr_Year_GD_Recent
##  Min.   :-123.0   Min.   :0.1475   Min.   :0.1837       Min.   :0.1921    
##  1st Qu.:-121.2   1st Qu.:0.2635   1st Qu.:0.2512       1st Qu.:0.3850    
##  Median :-120.2   Median :0.3140   Median :0.2949       Median :0.4285    
##  Mean   :-120.4   Mean   :0.3197   Mean   :0.3313       Mean   :0.4605    
##  3rd Qu.:-120.0   3rd Qu.:0.3995   3rd Qu.:0.4148       3rd Qu.:0.5594    
##  Max.   :-118.8   Max.   :0.5356   Max.   :0.5749       Max.   :0.8013    
##                                                                           
##  Wtr_Year_GD_Historical Geographic_Dist      Elev_Dist      
##  Min.   :0.2230         Min.   :   136.3   Min.   :-1707.0  
##  1st Qu.:0.4235         1st Qu.: 66245.7   1st Qu.:-1001.4  
##  Median :0.4586         Median :132940.1   Median :  363.2  
##  Mean   :0.4748         Mean   :138584.2   Mean   :  198.6  
##  3rd Qu.:0.5537         3rd Qu.:202681.2   3rd Qu.:  721.4  
##  Max.   :0.8014         Max.   :344080.6   Max.   : 2856.3  
## 
```

``` r
twomonths_rxnnorms_loc_scale <- twomonths_rxnnorms_loc %>% 
  mutate(Wtr_Year_GD_Recent = scale_this(Wtr_Year_GD_Recent), 
         elev_m_s = scale_this(elev_m))
lmeheight <- lmer(height.cm ~ elev_m_s*(Site/Wtr_Year_GD_Recent) + (1|parent.pop/mf), data = twomonths_rxnnorms_loc_scale) 
summary(lmeheight)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: height.cm ~ elev_m_s * (Site/Wtr_Year_GD_Recent) + (1 | parent.pop/mf)
##    Data: twomonths_rxnnorms_loc_scale
## 
## REML criterion at convergence: 5851.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.8753 -0.4115 -0.0082  0.3957  8.2660 
## 
## Random effects:
##  Groups        Name        Variance Std.Dev.
##  mf:parent.pop (Intercept) 0.8955   0.9463  
##  parent.pop    (Intercept) 4.0113   2.0028  
##  Residual                  5.3582   2.3148  
## Number of obs: 1256, groups:  mf:parent.pop, 168; parent.pop, 23
## 
## Fixed effects:
##                                      Estimate Std. Error        df t value
## (Intercept)                            3.3926     0.5374   40.2038   6.313
## elev_m_s                              -0.3762     0.6764   72.5249  -0.556
## SiteWL2                                1.6287     0.3262 1100.4312   4.992
## SiteUCD:Wtr_Year_GD_Recent            -0.4176     0.4102  474.7039  -1.018
## SiteWL2:Wtr_Year_GD_Recent            -2.3167     0.7622  494.5927  -3.040
## elev_m_s:SiteWL2                      -3.9792     0.4227 1181.2697  -9.414
## elev_m_s:SiteUCD:Wtr_Year_GD_Recent   -0.3783     0.2938  373.2727  -1.288
## elev_m_s:SiteWL2:Wtr_Year_GD_Recent   -0.7075     0.6560  414.1147  -1.079
##                                     Pr(>|t|)    
## (Intercept)                         1.68e-07 ***
## elev_m_s                             0.57977    
## SiteWL2                             6.93e-07 ***
## SiteUCD:Wtr_Year_GD_Recent           0.30922    
## SiteWL2:Wtr_Year_GD_Recent           0.00249 ** 
## elev_m_s:SiteWL2                     < 2e-16 ***
## elev_m_s:SiteUCD:Wtr_Year_GD_Recent  0.19867    
## elev_m_s:SiteWL2:Wtr_Year_GD_Recent  0.28141    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) elv_m_ SitWL2 SUCD:W SWL2:W el__:SWL2 e__:SU
## elev_m_s    -0.158                                             
## SiteWL2     -0.179  0.156                                      
## SUCD:W_Y_GD  0.109 -0.717 -0.217                               
## SWL2:W_Y_GD  0.079  0.363  0.540 -0.540                        
## elv_m_:SWL2  0.079 -0.509  0.049  0.590  0.190                 
## e__:SUCD:W_ -0.539  0.302  0.211 -0.347 -0.031 -0.259          
## e__:SWL2:W_  0.339  0.043  0.546 -0.091  0.710  0.216    -0.565
```

``` r
ranova(lmeheight) 
```

```
## ANOVA-like table for random-effects: Single term deletions
## 
## Model:
## height.cm ~ elev_m_s + Site + (1 | mf:parent.pop) + (1 | parent.pop) + Site:Wtr_Year_GD_Recent + elev_m_s:Site + elev_m_s:Site:Wtr_Year_GD_Recent
##                     npar  logLik    AIC    LRT Df Pr(>Chisq)    
## <none>                11 -2925.5 5873.1                         
## (1 | mf:parent.pop)   10 -2955.6 5931.1  60.03  1  9.343e-15 ***
## (1 | parent.pop)      10 -2985.0 5989.9 118.86  1  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
anova(lmeheight)
```

```
## Type III Analysis of Variance Table with Satterthwaite's method
##                                  Sum Sq Mean Sq NumDF   DenDF F value    Pr(>F)
## elev_m_s                          84.08   84.08     1   47.86 15.6920 0.0002471
## Site                             133.55  133.55     1 1100.43 24.9236 6.930e-07
## Site:Wtr_Year_GD_Recent          103.03   51.51     2  586.92  9.6138 7.793e-05
## elev_m_s:Site                    474.89  474.89     1 1181.27 88.6283 < 2.2e-16
## elev_m_s:Site:Wtr_Year_GD_Recent  34.55   17.27     2  496.46  3.2237 0.0406432
##                                     
## elev_m_s                         ***
## Site                             ***
## Site:Wtr_Year_GD_Recent          ***
## elev_m_s:Site                    ***
## elev_m_s:Site:Wtr_Year_GD_Recent *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
#cof 
#rnef 
#or fixed effects

emtrends(lmeheight, pairwise ~ Site, var = "elev_m_s")
```

```
## NOTE: Results may be misleading due to involvement in interactions
```

```
## $emtrends
##  Site elev_m_s.trend    SE   df lower.CL upper.CL
##  UCD          -0.372 0.681 67.7    -1.73    0.986
##  WL2          -4.348 0.590 43.8    -5.54   -3.159
## 
## Degrees-of-freedom method: kenward-roger 
## Confidence level used: 0.95 
## 
## $contrasts
##  contrast  estimate    SE   df t.ratio p.value
##  UCD - WL2     3.98 0.422 1176   9.412  <.0001
## 
## Degrees-of-freedom method: kenward-roger
```

``` r
emmip(lmeheight, Site ~ elev_m_s, cov.reduce = range)
```

```
## NOTE: A nesting structure was detected in the fitted model:
##     Wtr_Year_GD_Recent %in% Site
## NOTE: Results may be misleading due to involvement in interactions
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

Length


``` r
summary(twomonths_rxnnorms_loc)
```

```
##     BedLoc             block             Genotype            pop.mf         
##  Length:2330        Length:2330        Length:2330        Length:2330       
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##   parent.pop              mf              rep           height.cm     
##  Length:2330        Min.   : 1.000   Min.   : 1.000   Min.   : 0.100  
##  Class :character   1st Qu.: 2.000   1st Qu.: 4.000   1st Qu.: 2.100  
##  Mode  :character   Median : 5.000   Median : 8.000   Median : 3.200  
##                     Mean   : 4.513   Mean   : 8.102   Mean   : 4.325  
##                     3rd Qu.: 6.000   3rd Qu.:11.000   3rd Qu.: 5.200  
##                     Max.   :14.000   Max.   :31.000   Max.   :38.100  
##                                                       NA's   :1074    
##   long.leaf.cm       Site             Garden_Lat     Garden_Long    
##  Min.   :0.100   Length:2330        Min.   :38.53   Min.   :-121.8  
##  1st Qu.:1.800   Class :character   1st Qu.:38.53   1st Qu.:-121.8  
##  Median :2.500   Mode  :character   Median :38.83   Median :-120.3  
##  Mean   :2.602                      Mean   :38.73   Mean   :-120.7  
##  3rd Qu.:3.300                      3rd Qu.:38.83   3rd Qu.:-120.3  
##  Max.   :7.600                      Max.   :38.83   Max.   :-120.3  
##  NA's   :1159                                                       
##   Garden_Elev   elevation.group        elev_m            Lat       
##  Min.   :  16   Length:2330        Min.   : 313.0   Min.   :36.56  
##  1st Qu.:  16   Class :character   1st Qu.: 511.4   1st Qu.:37.81  
##  Median :2020   Mode  :character   Median :1921.0   Median :38.79  
##  Mean   :1369                      Mean   :1567.5   Mean   :38.70  
##  3rd Qu.:2020                      3rd Qu.:2353.6   3rd Qu.:39.59  
##  Max.   :2020                      Max.   :2872.3   Max.   :40.74  
##                                                                    
##       Long        GrwSsn_GD_Recent GrwSsn_GD_Historical Wtr_Year_GD_Recent
##  Min.   :-123.0   Min.   :0.1475   Min.   :0.1837       Min.   :0.1921    
##  1st Qu.:-121.2   1st Qu.:0.2635   1st Qu.:0.2512       1st Qu.:0.3850    
##  Median :-120.2   Median :0.3140   Median :0.2949       Median :0.4285    
##  Mean   :-120.4   Mean   :0.3197   Mean   :0.3313       Mean   :0.4605    
##  3rd Qu.:-120.0   3rd Qu.:0.3995   3rd Qu.:0.4148       3rd Qu.:0.5594    
##  Max.   :-118.8   Max.   :0.5356   Max.   :0.5749       Max.   :0.8013    
##                                                                           
##  Wtr_Year_GD_Historical Geographic_Dist      Elev_Dist      
##  Min.   :0.2230         Min.   :   136.3   Min.   :-1707.0  
##  1st Qu.:0.4235         1st Qu.: 66245.7   1st Qu.:-1001.4  
##  Median :0.4586         Median :132940.1   Median :  363.2  
##  Mean   :0.4748         Mean   :138584.2   Mean   :  198.6  
##  3rd Qu.:0.5537         3rd Qu.:202681.2   3rd Qu.:  721.4  
##  Max.   :0.8014         Max.   :344080.6   Max.   : 2856.3  
## 
```

``` r
lmelength <- lmer(long.leaf.cm ~ elev_m*Site + (1|parent.pop/mf), data = twomonths_rxnnorms_loc) 
```

```
## boundary (singular) fit: see help('isSingular')
```

``` r
summary(lmelength)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: long.leaf.cm ~ elev_m * Site + (1 | parent.pop/mf)
##    Data: twomonths_rxnnorms_loc
## 
## REML criterion at convergence: 3292.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.0339 -0.6347 -0.0062  0.6189  3.9497 
## 
## Random effects:
##  Groups        Name        Variance Std.Dev.
##  mf:parent.pop (Intercept) 0.0000   0.0000  
##  parent.pop    (Intercept) 0.1125   0.3355  
##  Residual                  0.9115   0.9547  
## Number of obs: 1171, groups:  mf:parent.pop, 162; parent.pop, 23
## 
## Fixed effects:
##                  Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)     2.931e+00  1.757e-01  2.427e+01  16.686 8.37e-15 ***
## elev_m         -4.104e-04  9.873e-05  3.010e+01  -4.156 0.000247 ***
## SiteWL2         6.965e-01  1.053e-01  1.163e+03   6.615 5.66e-11 ***
## elev_m:SiteWL2 -2.985e-04  6.900e-05  1.161e+03  -4.326 1.65e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) elev_m SitWL2
## elev_m      -0.872              
## SiteWL2     -0.311  0.312       
## elv_m:StWL2  0.277 -0.403 -0.821
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')
```

``` r
ranova(lmelength) 
```

```
## ANOVA-like table for random-effects: Single term deletions
## 
## Model:
## long.leaf.cm ~ elev_m + Site + (1 | mf:parent.pop) + (1 | parent.pop) + elev_m:Site
##                     npar  logLik    AIC    LRT Df Pr(>Chisq)    
## <none>                 7 -1646.3 3306.7                         
## (1 | mf:parent.pop)    6 -1646.3 3304.7  0.000  1          1    
## (1 | parent.pop)       6 -1676.2 3364.4 59.659  1  1.128e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
anova(lmelength)
```

```
## Type III Analysis of Variance Table with Satterthwaite's method
##             Sum Sq Mean Sq NumDF   DenDF F value    Pr(>F)    
## elev_m      34.849  34.849     1   21.68  38.231 3.400e-06 ***
## Site        39.883  39.883     1 1163.08  43.755 5.663e-11 ***
## elev_m:Site 17.059  17.059     1 1160.97  18.715 1.648e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
emmip(lmelength, elev_m ~ Site)
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

``` r
emtrends(lmelength, pairwise ~ Site, var = "elev_m")
```

```
## $emtrends
##  Site elev_m.trend       SE   df  lower.CL  upper.CL
##  UCD     -0.000410 9.90e-05 26.5 -0.000614 -0.000207
##  WL2     -0.000709 9.51e-05 23.1 -0.000905 -0.000512
## 
## Degrees-of-freedom method: kenward-roger 
## Confidence level used: 0.95 
## 
## $contrasts
##  contrast  estimate       SE   df t.ratio p.value
##  UCD - WL2 0.000298 6.94e-05 1062   4.303  <.0001
## 
## Degrees-of-freedom method: kenward-roger
```

``` r
emmip(lmelength, Site ~ elev_m, cov.reduce = range)
```

![](TwoMonths_Size_RxnNorms_files/figure-html/unnamed-chunk-20-2.png)<!-- -->
