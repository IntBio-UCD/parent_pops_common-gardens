---
title: "PreTransplant_Size"
author: "Brandie Quarles"
date: "2024-07-17"
output: 
  html_document: 
    keep_md: yes
---



# Pre-Transplant Size for both gardens 
There shouldn't be any large differences since the plants were grown in the same environment pre-transplant. 
Is there a relationship between pre-transplant size and survival to the end of the first year? Does that differ across gardens?

## Relevant Libraries and Functions


```r
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

```r
library(tidymodels)
```

```
## ── Attaching packages ────────────────────────────────────── tidymodels 1.2.0 ──
## ✔ broom        1.0.5      ✔ rsample      1.2.1 
## ✔ dials        1.2.1      ✔ tune         1.2.1 
## ✔ infer        1.0.7      ✔ workflows    1.1.4 
## ✔ modeldata    1.3.0      ✔ workflowsets 1.1.0 
## ✔ parsnip      1.2.1      ✔ yardstick    1.3.1 
## ✔ recipes      1.0.10     
## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
## ✖ scales::discard() masks purrr::discard()
## ✖ dplyr::filter()   masks stats::filter()
## ✖ recipes::fixed()  masks stringr::fixed()
## ✖ dplyr::lag()      masks stats::lag()
## ✖ yardstick::spec() masks readr::spec()
## ✖ recipes::step()   masks stats::step()
## • Search for functions across packages at https://www.tidymodels.org/find/
```

```r
tidymodels_prefer()
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
```

```r
conflicted::conflicts_prefer(lmerTest::lmer)
```

```
## [conflicted] Will prefer lmerTest::lmer over any other package.
```

```r
library(broom.mixed) #tidy method for lmerTest
library(emmeans) #for post-hoc pairwise comparisons 
library(naniar) #replaces values with NA

sem <- function(x, na.rm=FALSE) {           #for caclulating standard error
  sd(x,na.rm=na.rm)/sqrt(length(na.omit(x)))
} 

elev_three_palette <- c("#0043F0", "#C9727F", "#F5A540") #colors from Gremer et al 2019
elev_order <- c("High", "Mid", "Low") #for proper arrangement in figures 
```

## Load the pop and location data


```r
gowersdist_UCD <- read_csv("../output/Climate/Pops_GowersEnvtalDist_UCD.csv") %>% 
  rename(Recent_Gowers_Dist_UCD = Recent_Gowers_Dist, Historic_Gowers_Dist_UCD = Historic_Gowers_Dist)
```

```
## Rows: 23 Columns: 5
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): parent.pop, elevation.group
## dbl (3): elev_m, Recent_Gowers_Dist, Historic_Gowers_Dist
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
gowersdist_WL2 <- read_csv("../output/Climate/Pops_GowersEnvtalDist_WL2.csv") %>% 
  rename(Recent_Gowers_Dist_WL2 = Recent_Gowers_Dist, Historic_Gowers_Dist_WL2 = Historic_Gowers_Dist)
```

```
## Rows: 23 Columns: 5
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): parent.pop, elevation.group
## dbl (3): elev_m, Recent_Gowers_Dist, Historic_Gowers_Dist
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
gowersdist_all <- full_join(gowersdist_UCD, gowersdist_WL2)
```

```
## Joining with `by = join_by(parent.pop, elevation.group, elev_m)`
```

```r
head(gowersdist_all)
```

```
## # A tibble: 6 × 7
##   parent.pop elevation.group elev_m Recent_Gowers_Dist_UCD
##   <chr>      <chr>            <dbl>                  <dbl>
## 1 BH         Low               511.                  0.273
## 2 CC         Low               313                   0.331
## 3 CP2        High             2244.                  0.445
## 4 CP3        High             2266.                  0.476
## 5 DPR        Mid              1019.                  0.267
## 6 FR         Mid               787                   0.296
## # ℹ 3 more variables: Historic_Gowers_Dist_UCD <dbl>,
## #   Recent_Gowers_Dist_WL2 <dbl>, Historic_Gowers_Dist_WL2 <dbl>
```

## Load Pre-Transplant Size data from both Gardens

```r
ucd_pretrans_size <- read_csv("../input/UCD_Data/CorrectedCSVs/UCD_garden_size_measurements_20221128_corrected.csv",
                              na = c("", "NA", "-", "N/A"))
```

```
## Warning: One or more parsing issues, call `problems()` on your data frame for details,
## e.g.:
##   dat <- vroom(...)
##   problems(dat)
```

```
## Rows: 2264 Columns: 7
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): parent.pop, Notes
## dbl (5): mf, rep, germinated?, Height (cm), Longest leaf (cm)
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
head(ucd_pretrans_size) #includes germs and non-germs 
```

```
## # A tibble: 6 × 7
##   parent.pop    mf   rep `germinated?` `Height (cm)` `Longest leaf (cm)` Notes
##   <chr>      <dbl> <dbl>         <dbl>         <dbl>               <dbl> <chr>
## 1 BH             1     1             0            NA                  NA <NA> 
## 2 BH             1     2             0            NA                  NA <NA> 
## 3 BH             1     3             0            NA                  NA <NA> 
## 4 BH             1     4             0            NA                  NA <NA> 
## 5 BH             1     5             0            NA                  NA <NA> 
## 6 BH             1     6             0            NA                  NA <NA>
```

```r
ucd_pretrans_size_germonly <- ucd_pretrans_size %>% 
  rename(germ = `germinated?`, height.cm = `Height (cm)`, long.leaf.cm=`Longest leaf (cm)`) %>% 
  filter(germ==1) %>% 
  unite(Genotype, parent.pop:rep, sep="_", remove = FALSE) %>% 
  unite(pop.mf, parent.pop:mf, sep="_", remove = FALSE)
head(ucd_pretrans_size_germonly)
```

```
## # A tibble: 6 × 9
##   Genotype pop.mf parent.pop    mf   rep  germ height.cm long.leaf.cm Notes
##   <chr>    <chr>  <chr>      <dbl> <dbl> <dbl>     <dbl>        <dbl> <chr>
## 1 BH_1_7   BH_1   BH             1     7     1       1.7          1.9 <NA> 
## 2 BH_1_8   BH_1   BH             1     8     1       1.7          1.6 <NA> 
## 3 BH_1_10  BH_1   BH             1    10     1       1.2          1.5 <NA> 
## 4 BH_2_1   BH_2   BH             2     1     1       1.4          2.7 <NA> 
## 5 BH_2_2   BH_2   BH             2     2     1       1.4          1.3 <NA> 
## 6 BH_2_3   BH_2   BH             2     3     1       1.1          1.7 <NA>
```

```r
unique(ucd_pretrans_size_germonly$parent.pop) #looks correct
```

```
##  [1] "BH"    "CC"    "CP2"   "CP3"   "DPR"   "FR"    "IH"    "LV1"   "LV3"  
## [10] "LVTR1" "SC"    "SQ1"   "SQ2"   "SQ3"   "TM2"   "WL1"   "WL2"   "WR"   
## [19] "WV"    "YO11"  "YO4"   "YO7"   "YO8"
```


```r
wl2_pretrans_size1 <- read_csv("../input/WL2_Data/CorrectedCSVs/WL2_DNA_Collection_Size_survey_combined20230703_corrected.csv")
```

```
## Rows: 1427 Columns: 7
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): Pop, Notes
## dbl (5): mf, rep, DNA, height (cm), longest leaf (cm)
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
head(wl2_pretrans_size1)
```

```
## # A tibble: 6 × 7
##   Pop      mf   rep   DNA `height (cm)` `longest leaf (cm)` Notes
##   <chr> <dbl> <dbl> <dbl>         <dbl>               <dbl> <chr>
## 1 CP2       1     1     1           0.5                 1.6 KN   
## 2 CP2       1     2     1           0.7                 1.8 KN   
## 3 CP2       1     3     1           1.1                 1.8 KN   
## 4 CP2       1     4     1           0.8                 1.6 KN   
## 5 CP2       1     5     1           0.9                 1.8 KN   
## 6 CP2       1     6     1           1                   1.9 KN
```

```r
wl2_pretrans_size2 <- read_csv("../input/WL2_Data/CorrectedCSVs/WL2_Extras_DNA_collection_size_survey_combined20230706_corrected.csv") %>% 
  filter(!is.na(`height (cm)`)) #to get rid of genotypes that were measured on the other data sheet (NAs on this sheet)
```

```
## Rows: 152 Columns: 7
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): Pop, Notes
## dbl (5): mf, rep, DNA, height (cm), longest leaf (cm)
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
head(wl2_pretrans_size2)
```

```
## # A tibble: 6 × 7
##   Pop      mf   rep   DNA `height (cm)` `longest leaf (cm)` Notes
##   <chr> <dbl> <dbl> <dbl>         <dbl>               <dbl> <chr>
## 1 CP2       1    14     1           1.2                 0.9 <NA> 
## 2 CP2       2    14     1           0.8                 0.7 <NA> 
## 3 CP2       3    14     1           0.5                 1   <NA> 
## 4 CP2       4    14     1           1.1                 0.9 <NA> 
## 5 CP2       5    14     1           1.4                 1.6 <NA> 
## 6 CP2       7    14     1           0.9                 0.8 <NA>
```

```r
wl2_pretrans_size_all <- bind_rows(wl2_pretrans_size1, wl2_pretrans_size2) %>%
  rename(parent.pop=Pop, height.cm = `height (cm)`, long.leaf.cm=`longest leaf (cm)`) %>% 
  unite(Genotype, parent.pop:rep, sep="_", remove = FALSE) %>% 
  unite(pop.mf, parent.pop:mf, sep="_", remove = FALSE)
head(wl2_pretrans_size_all)
```

```
## # A tibble: 6 × 9
##   Genotype pop.mf parent.pop    mf   rep   DNA height.cm long.leaf.cm Notes
##   <chr>    <chr>  <chr>      <dbl> <dbl> <dbl>     <dbl>        <dbl> <chr>
## 1 CP2_1_1  CP2_1  CP2            1     1     1       0.5          1.6 KN   
## 2 CP2_1_2  CP2_1  CP2            1     2     1       0.7          1.8 KN   
## 3 CP2_1_3  CP2_1  CP2            1     3     1       1.1          1.8 KN   
## 4 CP2_1_4  CP2_1  CP2            1     4     1       0.8          1.6 KN   
## 5 CP2_1_5  CP2_1  CP2            1     5     1       0.9          1.8 KN   
## 6 CP2_1_6  CP2_1  CP2            1     6     1       1            1.9 KN
```

## Merge the two sites

```r
ucd_pretrans_size_prep <- ucd_pretrans_size_germonly %>% select(Genotype:rep, height.cm:long.leaf.cm) %>%
  mutate(Site="UCD")
names(ucd_pretrans_size_prep)
```

```
## [1] "Genotype"     "pop.mf"       "parent.pop"   "mf"           "rep"         
## [6] "height.cm"    "long.leaf.cm" "Site"
```

```r
wl2_pretrans_size_prep <- wl2_pretrans_size_all %>% select(Genotype:rep, height.cm:long.leaf.cm) %>%
  mutate(Site="WL2")
names(wl2_pretrans_size_prep)
```

```
## [1] "Genotype"     "pop.mf"       "parent.pop"   "mf"           "rep"         
## [6] "height.cm"    "long.leaf.cm" "Site"
```

```r
pretrans_rxnnorms <- bind_rows(ucd_pretrans_size_prep, wl2_pretrans_size_prep) %>% 
  arrange(pop.mf, Site)
head(pretrans_rxnnorms)
```

```
## # A tibble: 6 × 8
##   Genotype pop.mf parent.pop    mf   rep height.cm long.leaf.cm Site 
##   <chr>    <chr>  <chr>      <dbl> <dbl>     <dbl>        <dbl> <chr>
## 1 BH_1_7   BH_1   BH             1     7       1.7          1.9 UCD  
## 2 BH_1_8   BH_1   BH             1     8       1.7          1.6 UCD  
## 3 BH_1_10  BH_1   BH             1    10       1.2          1.5 UCD  
## 4 BH_1_1   BH_1   BH             1     1       2            2.2 WL2  
## 5 BH_1_2   BH_1   BH             1     2       2.3          2.1 WL2  
## 6 BH_1_3   BH_1   BH             1     3       3.6          2.6 WL2
```

## Add in Location Info

```r
pretrans_rxnnorms_loc <-left_join(pretrans_rxnnorms, gowersdist_all) 
```

```
## Joining with `by = join_by(parent.pop)`
```

```r
head(pretrans_rxnnorms_loc)
```

```
## # A tibble: 6 × 14
##   Genotype pop.mf parent.pop    mf   rep height.cm long.leaf.cm Site 
##   <chr>    <chr>  <chr>      <dbl> <dbl>     <dbl>        <dbl> <chr>
## 1 BH_1_7   BH_1   BH             1     7       1.7          1.9 UCD  
## 2 BH_1_8   BH_1   BH             1     8       1.7          1.6 UCD  
## 3 BH_1_10  BH_1   BH             1    10       1.2          1.5 UCD  
## 4 BH_1_1   BH_1   BH             1     1       2            2.2 WL2  
## 5 BH_1_2   BH_1   BH             1     2       2.3          2.1 WL2  
## 6 BH_1_3   BH_1   BH             1     3       3.6          2.6 WL2  
## # ℹ 6 more variables: elevation.group <chr>, elev_m <dbl>,
## #   Recent_Gowers_Dist_UCD <dbl>, Historic_Gowers_Dist_UCD <dbl>,
## #   Recent_Gowers_Dist_WL2 <dbl>, Historic_Gowers_Dist_WL2 <dbl>
```

## Load survival to end of first year data

```r
firstyear_surv <- read_csv("../output/firstyear_mort_both_sites.csv")
```

```
## Rows: 2327 Columns: 14
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (6): Genotype, pop.mf, parent.pop, pheno, Site, elevation.group
## dbl (8): mf, rep, Survival, elev_m, Recent_Gowers_Dist_UCD, Historic_Gowers_...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
head(firstyear_surv)
```

```
## # A tibble: 6 × 14
##   Genotype pop.mf parent.pop    mf   rep pheno Site  Survival elevation.group
##   <chr>    <chr>  <chr>      <dbl> <dbl> <chr> <chr>    <dbl> <chr>          
## 1 BH_1_1   BH_1   BH             1     1 V     WL2          1 Low            
## 2 BH_1_10  BH_1   BH             1    10 X     UCD          0 Low            
## 3 BH_1_10  BH_1   BH             1    10 V     WL2          1 Low            
## 4 BH_1_11  BH_1   BH             1    11 <NA>  WL2          0 Low            
## 5 BH_1_12  BH_1   BH             1    12 V     WL2          1 Low            
## 6 BH_1_13  BH_1   BH             1    13 V     WL2          1 Low            
## # ℹ 5 more variables: elev_m <dbl>, Recent_Gowers_Dist_UCD <dbl>,
## #   Historic_Gowers_Dist_UCD <dbl>, Recent_Gowers_Dist_WL2 <dbl>,
## #   Historic_Gowers_Dist_WL2 <dbl>
```

```r
firstyear_surv_merge_prep <- firstyear_surv %>% select(Genotype:rep, Site:Historic_Gowers_Dist_WL2)
```

## Merge size and survival

```r
pretrans_surv <- left_join(pretrans_rxnnorms_loc, firstyear_surv_merge_prep)
```

```
## Joining with `by = join_by(Genotype, pop.mf, parent.pop, mf, rep, Site,
## elevation.group, elev_m, Recent_Gowers_Dist_UCD, Historic_Gowers_Dist_UCD,
## Recent_Gowers_Dist_WL2, Historic_Gowers_Dist_WL2)`
```

```r
pretrans_surv %>% filter(is.na(Survival)) #12 genotypes with no survival information... should investigate
```

```
## # A tibble: 12 × 15
##    Genotype  pop.mf  parent.pop    mf   rep height.cm long.leaf.cm Site 
##    <chr>     <chr>   <chr>      <dbl> <dbl>     <dbl>        <dbl> <chr>
##  1 DPR_4_2   DPR_4   DPR            4     2       1.4          0.9 WL2  
##  2 DPR_5_1   DPR_5   DPR            5     1      NA           NA   WL2  
##  3 LV1_6_7   LV1_6   LV1            6     7       1            0.8 WL2  
##  4 LVTR1_2_6 LVTR1_2 LVTR1          2     6      NA           NA   WL2  
##  5 LVTR1_6_3 LVTR1_6 LVTR1          6     3      NA           NA   WL2  
##  6 SQ1_14_4  SQ1_14  SQ1           14     4      NA           NA   WL2  
##  7 SQ2_6_1   SQ2_6   SQ2            6     1       0.6          1.1 UCD  
##  8 SQ3_2_2   SQ3_2   SQ3            2     2       0.2          0.3 WL2  
##  9 WL1_2_9   WL1_2   WL1            2     9      NA           NA   WL2  
## 10 WL2_3_8   WL2_3   WL2            3     8      NA           NA   UCD  
## 11 WL2_4_10  WL2_4   WL2            4    10       1.7          1.1 UCD  
## 12 WR_1_7    WR_1    WR             1     7       0.7          1.6 WL2  
## # ℹ 7 more variables: elevation.group <chr>, elev_m <dbl>,
## #   Recent_Gowers_Dist_UCD <dbl>, Historic_Gowers_Dist_UCD <dbl>,
## #   Recent_Gowers_Dist_WL2 <dbl>, Historic_Gowers_Dist_WL2 <dbl>,
## #   Survival <dbl>
```

```r
pretrans_surv_complete <- pretrans_surv %>% filter(!is.na(Survival))
```


## Plot Reaction Norms

### Means by Pop

```r
pretrans_rxnnorms_summary <- pretrans_rxnnorms_loc %>% 
  group_by(parent.pop, Site, elev_m) %>% 
  summarise(N_height = sum(!is.na(height.cm)), mean_height.cm = mean(height.cm,na.rm=(TRUE)), 
            sem_height.cm=sem(height.cm, na.rm=(TRUE)), N_length = sum(!is.na(long.leaf.cm)),
            mean_long.leaf.cm=mean(long.leaf.cm, na.rm=(TRUE)), 
            sem_long.leaf.cm=sem(long.leaf.cm, na.rm=TRUE)) %>% 
  mutate(Site=str_replace_all(Site, "UCD", "Low Elev"), 
         Site=str_replace_all(Site, "WL2", "High Elev"))
```

```
## `summarise()` has grouped output by 'parent.pop', 'Site'. You can override
## using the `.groups` argument.
```

```r
pretrans_rxnnorms_summary
```

```
## # A tibble: 46 × 9
## # Groups:   parent.pop, Site [46]
##    parent.pop Site      elev_m N_height mean_height.cm sem_height.cm N_length
##    <chr>      <chr>      <dbl>    <int>          <dbl>         <dbl>    <int>
##  1 BH         Low Elev    511.      111          2.11         0.0625      111
##  2 BH         High Elev   511.       91          2.27         0.0722       91
##  3 CC         Low Elev    313        44          3.33         0.173        44
##  4 CC         High Elev   313        91          3.04         0.0987       91
##  5 CP2        Low Elev   2244.       38          1.39         0.0809       38
##  6 CP2        High Elev  2244.       91          1.04         0.0388       91
##  7 CP3        Low Elev   2266.        7          1.34         0.282         7
##  8 CP3        High Elev  2266.       91          0.971        0.0339       91
##  9 DPR        Low Elev   1019.       22          3.30         0.310        22
## 10 DPR        High Elev  1019.       90          2.20         0.124        90
## # ℹ 36 more rows
## # ℹ 2 more variables: mean_long.leaf.cm <dbl>, sem_long.leaf.cm <dbl>
```

```r
pretrans_rxnnorms_summary$Site <- factor(pretrans_rxnnorms_summary$Site,
                                               levels = c('Low Elev','High Elev'))
```

### Plot Pop Avgs

```r
pretrans_rxnnorms_summary %>% 
  filter(N_height != 1) %>% 
  ggplot(aes(x=Site, y=mean_height.cm, group=parent.pop, color=elev_m)) + 
  geom_point(size=1.5) + geom_line(linewidth=1.5) +
  geom_errorbar(aes(ymin=mean_height.cm-sem_height.cm,ymax=mean_height.cm+sem_height.cm),width=.1) +
  theme_classic() + scale_colour_gradient(low = "#F5A540", high = "#0043F0")  +
  labs(y="Height (cm)", color="Elevation (m)", title = "Pre-Transplant") +
 # ylim(0,4) +
  theme(text=element_text(size=28)) 
```

![](PreTransplant_Size_MortPred_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
ggsave("../output/PreTrans_RxNorms_Height_ALL_PopAvgs.png", width = 14, height = 8, units = "in")

pretrans_rxnnorms_summary %>% 
  filter(N_length != 1) %>% 
  ggplot(aes(x=Site, y=mean_long.leaf.cm, group=parent.pop, color=elev_m)) + 
  geom_point(size=1.5) + geom_line(linewidth=1.5) +
  geom_errorbar(aes(ymin=mean_long.leaf.cm-sem_long.leaf.cm,
                    ymax=mean_long.leaf.cm+sem_long.leaf.cm),width=.1) +
  theme_classic() + scale_colour_gradient(low = "#F5A540", high = "#0043F0")  +
  labs(y="Leaf Length (cm)", color="Elevation (m)", title = "Pre-Transplant") +
  theme(text=element_text(size=28))
```

![](PreTransplant_Size_MortPred_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

```r
ggsave("../output/PreTrans_RxNorms_LongestLength_ALL_PopAvgs.png", width = 14, height = 8, units = "in")
```
Plants were overall smaller when planted at WL2 than at Davis.

## Connection to survival to end of first year

```r
pretrans_surv_pop_avgs <- pretrans_surv_complete %>% 
  group_by(parent.pop, Site, elev_m) %>% 
  summarise(N_Surv = sum(!is.na(Survival)), mean_Surv = mean(Survival,na.rm=(TRUE)), 
            sem_surv=sem(Survival, na.rm=(TRUE)), 
            N_height = sum(!is.na(height.cm)), mean_height.cm = mean(height.cm,na.rm=(TRUE)), 
            sem_height.cm=sem(height.cm, na.rm=(TRUE)), N_length = sum(!is.na(long.leaf.cm)),
            mean_long.leaf.cm=mean(long.leaf.cm, na.rm=(TRUE)), 
            sem_long.leaf.cm=sem(long.leaf.cm, na.rm=TRUE)) %>% 
  mutate(Site=str_replace_all(Site, "UCD", "Low Elev"), 
         Site=str_replace_all(Site, "WL2", "High Elev"))
```

```
## `summarise()` has grouped output by 'parent.pop', 'Site'. You can override
## using the `.groups` argument.
```

```r
pretrans_surv_pop_avgs %>% arrange(desc(mean_height.cm))
```

```
## # A tibble: 46 × 12
## # Groups:   parent.pop, Site [46]
##    parent.pop Site      elev_m N_Surv mean_Surv sem_surv N_height mean_height.cm
##    <chr>      <chr>      <dbl>  <int>     <dbl>    <dbl>    <int>          <dbl>
##  1 TM2        Low Elev    379.     37     0.216   0.0686       37           4.09
##  2 CC         Low Elev    313      44     0       0            44           3.33
##  3 DPR        Low Elev   1019.     22     0       0            22           3.30
##  4 CC         High Elev   313      91     0.560   0.0523       91           3.04
##  5 TM2        High Elev   379.     84     0.452   0.0546       84           2.96
##  6 FR         Low Elev    787      38     0       0            36           2.91
##  7 IH         High Elev   454.     91     0.593   0.0518       91           2.81
##  8 YO4        Low Elev   2158.      6     0       0             6           2.42
##  9 BH         High Elev   511.     91     0.582   0.0520       91           2.27
## 10 DPR        High Elev  1019.     89     0.337   0.0504       89           2.20
## # ℹ 36 more rows
## # ℹ 4 more variables: sem_height.cm <dbl>, N_length <int>,
## #   mean_long.leaf.cm <dbl>, sem_long.leaf.cm <dbl>
```

```r
pretrans_surv_pop_avgs$Site <- factor(pretrans_surv_pop_avgs$Site,
                                               levels = c('Low Elev','High Elev'))
```


```r
pretrans_surv_pop_avgs %>% 
  ggplot(aes(x=mean_height.cm, y=mean_Surv, group=parent.pop, color=elev_m)) +
  geom_point(size=8) +
  theme_classic() + scale_colour_gradient(low = "#F5A540", high = "#0043F0")  +
  labs(x="Height (cm)" ,y="Survival", color="Elevation (m)",title = "Pre-Transplant Size") +
  theme(text=element_text(size=28)) +
  facet_wrap(vars(Site), scales="free")
```

![](PreTransplant_Size_MortPred_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
ggsave("../output/Surv_PreTrans_Height_BothGardens.png", width = 18, height = 8, units = "in")

pretrans_surv_pop_avgs %>% 
  ggplot(aes(x=mean_long.leaf.cm, y=mean_Surv, group=parent.pop, color=elev_m)) +
  geom_point(size=8) +
  theme_classic() + scale_colour_gradient(low = "#F5A540", high = "#0043F0")  +
  labs(x="Leaf Length (cm)" ,y="Survival", color="Elevation (m)", title = "Pre-Transplant Size") +
  theme(text=element_text(size=28)) +
  facet_wrap(vars(Site), scales="free")
```

![](PreTransplant_Size_MortPred_files/figure-html/unnamed-chunk-12-2.png)<!-- -->

```r
ggsave("../output/Surv_PreTrans_Length_BothGardens.png", width = 18, height = 8, units = "in")
```

### Log Reg survival ~ size 
Height

```r
basiclogit_height <- glm(Survival ~ height.cm, data = pretrans_surv_complete, family = "binomial")
basiclogit_height
```

```
## 
## Call:  glm(formula = Survival ~ height.cm, family = "binomial", data = pretrans_surv_complete)
## 
## Coefficients:
## (Intercept)    height.cm  
##     -1.7251       0.2726  
## 
## Degrees of Freedom: 2311 Total (i.e. Null);  2310 Residual
##   (9 observations deleted due to missingness)
## Null Deviance:	    2467 
## Residual Deviance: 2430 	AIC: 2434
```

```r
summary(basiclogit_height)
```

```
## 
## Call:
## glm(formula = Survival ~ height.cm, family = "binomial", data = pretrans_surv_complete)
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.72508    0.09646 -17.884  < 2e-16 ***
## height.cm    0.27260    0.04403   6.192 5.95e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2467.3  on 2311  degrees of freedom
## Residual deviance: 2429.7  on 2310  degrees of freedom
##   (9 observations deleted due to missingness)
## AIC: 2433.7
## 
## Number of Fisher Scoring iterations: 4
```

```r
basiclogit_height2 <- glmer(Survival ~ height.cm + (1|parent.pop/mf), data = pretrans_surv_complete, family = binomial(link = "logit"))
summary(basiclogit_height2)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: Survival ~ height.cm + (1 | parent.pop/mf)
##    Data: pretrans_surv_complete
## 
##      AIC      BIC   logLik deviance df.resid 
##   2226.1   2249.1  -1109.1   2218.1     2308 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.0051 -0.6268 -0.3178 -0.2218  4.5087 
## 
## Random effects:
##  Groups        Name        Variance Std.Dev.
##  mf:parent.pop (Intercept) 0.1528   0.3908  
##  parent.pop    (Intercept) 0.9468   0.9730  
## Number of obs: 2312, groups:  mf:parent.pop, 173; parent.pop, 23
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.692737   0.244878  -6.913 4.76e-12 ***
## height.cm    0.000592   0.066621   0.009    0.993    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##           (Intr)
## height.cm -0.420
```

```r
logit_height <- glm(Survival ~ height.cm*Site, data = pretrans_surv_complete, family = "binomial")
summary(logit_height)
```

```
## 
## Call:
## glm(formula = Survival ~ height.cm * Site, family = "binomial", 
##     data = pretrans_surv_complete)
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -2.9821     0.3021  -9.870  < 2e-16 ***
## height.cm           0.2187     0.1144   1.912   0.0559 .  
## SiteWL2             1.2549     0.3211   3.907 9.33e-05 ***
## height.cm:SiteWL2   0.3193     0.1274   2.506   0.0122 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2467.3  on 2311  degrees of freedom
## Residual deviance: 2202.6  on 2308  degrees of freedom
##   (9 observations deleted due to missingness)
## AIC: 2210.6
## 
## Number of Fisher Scoring iterations: 5
```

```r
logit_height2 <- glmer(Survival ~ height.cm*Site + (1|parent.pop/mf), data = pretrans_surv_complete, family = binomial(link = "logit")) 
summary(logit_height2)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: Survival ~ height.cm * Site + (1 | parent.pop/mf)
##    Data: pretrans_surv_complete
## 
##      AIC      BIC   logLik deviance df.resid 
##   2027.4   2061.9  -1007.7   2015.4     2306 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.7083 -0.4698 -0.3152 -0.1267  5.7453 
## 
## Random effects:
##  Groups        Name        Variance Std.Dev.
##  mf:parent.pop (Intercept) 0.02034  0.1426  
##  parent.pop    (Intercept) 0.90473  0.9512  
## Number of obs: 2312, groups:  mf:parent.pop, 173; parent.pop, 23
## 
## Fixed effects:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       -3.53680    0.40034  -8.835  < 2e-16 ***
## height.cm          0.14673    0.13002   1.129    0.259    
## SiteWL2            2.02271    0.36607   5.526 3.29e-08 ***
## height.cm:SiteWL2  0.04186    0.14101   0.297    0.767    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) hght.c SitWL2
## height.cm   -0.750              
## SiteWL2     -0.800  0.784       
## hght.c:SWL2  0.688 -0.838 -0.888
```

```r
emtrends(logit_height2, pairwise ~ Site, var = "height.cm")
```

```
## $emtrends
##  Site height.cm.trend     SE  df asymp.LCL asymp.UCL
##  UCD            0.147 0.1300 Inf   -0.1081     0.402
##  WL2            0.189 0.0778 Inf    0.0361     0.341
## 
## Confidence level used: 0.95 
## 
## $contrasts
##  contrast  estimate    SE  df z.ratio p.value
##  UCD - WL2  -0.0419 0.141 Inf  -0.297  0.7666
```

```r
emmip(logit_height2, Site ~ height.cm, cov.reduce = range)
```

![](PreTransplant_Size_MortPred_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

Length

```r
basiclogit_length <- glm(Survival ~ long.leaf.cm, data = pretrans_surv_complete, family = "binomial")
summary(basiclogit_length)
```

```
## 
## Call:
## glm(formula = Survival ~ long.leaf.cm, family = "binomial", data = pretrans_surv_complete)
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -2.32424    0.14221 -16.344   <2e-16 ***
## long.leaf.cm  0.60329    0.07052   8.555   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2466.8  on 2310  degrees of freedom
## Residual deviance: 2391.7  on 2309  degrees of freedom
##   (10 observations deleted due to missingness)
## AIC: 2395.7
## 
## Number of Fisher Scoring iterations: 4
```

```r
basiclogit_length2 <- glmer(Survival ~ long.leaf.cm + (1|parent.pop/mf), data = pretrans_surv_complete, family = binomial(link = "logit"))
summary(basiclogit_length2)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: Survival ~ long.leaf.cm + (1 | parent.pop/mf)
##    Data: pretrans_surv_complete
## 
##      AIC      BIC   logLik deviance df.resid 
##   2219.0   2242.0  -1105.5   2211.0     2307 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.1212 -0.6092 -0.3172 -0.2237  4.5071 
## 
## Random effects:
##  Groups        Name        Variance Std.Dev.
##  mf:parent.pop (Intercept) 0.1573   0.3966  
##  parent.pop    (Intercept) 0.8224   0.9069  
## Number of obs: 2311, groups:  mf:parent.pop, 173; parent.pop, 23
## 
## Fixed effects:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -2.08873    0.26127  -7.995  1.3e-15 ***
## long.leaf.cm  0.24510    0.09435   2.598  0.00938 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## long.lef.cm -0.598
```

```r
logit_length2 <- glmer(Survival ~ long.leaf.cm*Site + (1|parent.pop/mf), data = pretrans_surv_complete, family = binomial(link = "logit")) 
summary(logit_length2)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: Survival ~ long.leaf.cm * Site + (1 | parent.pop/mf)
##    Data: pretrans_surv_complete
## 
##      AIC      BIC   logLik deviance df.resid 
##   2003.1   2037.6   -995.6   1991.1     2305 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.8642 -0.4630 -0.2979 -0.1379  5.7361 
## 
## Random effects:
##  Groups        Name        Variance Std.Dev.
##  mf:parent.pop (Intercept) 0.003121 0.05586 
##  parent.pop    (Intercept) 0.733516 0.85646 
## Number of obs: 2311, groups:  mf:parent.pop, 173; parent.pop, 23
## 
## Fixed effects:
##                      Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -3.6245     0.5232  -6.928 4.26e-12 ***
## long.leaf.cm           0.2662     0.2104   1.265  0.20584    
## SiteWL2                1.4023     0.5264   2.664  0.00773 ** 
## long.leaf.cm:SiteWL2   0.3656     0.2296   1.593  0.11122    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) lng.l. SitWL2
## long.lef.cm -0.881              
## SiteWL2     -0.867  0.850       
## lng.l.:SWL2  0.801 -0.865 -0.948
```

```r
emtrends(logit_length2, pairwise ~ Site, var = "long.leaf.cm")
```

```
## $emtrends
##  Site long.leaf.cm.trend    SE  df asymp.LCL asymp.UCL
##  UCD               0.266 0.210 Inf    -0.146     0.679
##  WL2               0.632 0.116 Inf     0.405     0.858
## 
## Confidence level used: 0.95 
## 
## $contrasts
##  contrast  estimate   SE  df z.ratio p.value
##  UCD - WL2   -0.366 0.23 Inf  -1.593  0.1112
```

```r
emmip(logit_length2, Site ~ long.leaf.cm, cov.reduce = range)
```

![](PreTransplant_Size_MortPred_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
