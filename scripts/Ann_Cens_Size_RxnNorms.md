---
title: "Ann_Cens_Size_RxnNorms"
author: "Brandie QC"
date: "2025-10-13"
output: 
  html_document: 
    keep_md: true
---



# Reaction Norms of Size (between UCD and WL2) - Stem Diameter and Branch Number 

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

Year 1 

``` r
ucd_ann_cens <- read_csv("../output/UCD_Traits/UCD2023_Annual_Census_Combined.csv") %>%  #note this is only for plants that survived to rep 
  filter(!is.na(parent.pop)) %>% 
  unite(BedLoc, block:col, sep="_", remove = FALSE) %>% 
  filter(rep != 100) %>% #get rid of individuals that germinated in the field 
  unite(Genotype, parent.pop:rep, sep="_", remove = FALSE) %>% 
  unite(pop.mf, parent.pop:mf, sep="_", remove = FALSE)
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
  rename(parent.pop=pop) %>% 
  unite(Genotype, parent.pop:rep, sep="_", remove = FALSE) %>% 
  unite(BedLoc, bed:`bed-col`, sep="_", remove = FALSE) %>% 
  filter(BedLoc!="K_5_C") %>% #get rid of duplicate locations
  filter(BedLoc!="B_32_A") %>% #get rid of duplicate locations
  filter(!is.na(parent.pop), !str_detect(Genotype, ".*buff*")) %>% #remove buffers 
  unite(pop.mf, parent.pop:mf, sep="_", remove = FALSE)
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

### Merge the Data

``` r
wl2_ann_cens_prep <- wl2_ann_cens %>% 
  select(block:BedLoc, Genotype:long.leaf.cm, total.branch:repro.branch) %>% 
  mutate(Site="WL2") %>%
  mutate(mf=as.double(mf), rep=as.double(rep)) 
names(wl2_ann_cens_prep)
```

```
##  [1] "block"        "BedLoc"       "Genotype"     "pop.mf"       "parent.pop"  
##  [6] "mf"           "rep"          "pheno"        "diam.mm"      "height.cm"   
## [11] "long.leaf.cm" "total.branch" "repro.branch" "Site"
```

``` r
ucd_ann_cens_prep <- ucd_ann_cens %>% 
  select(BedLoc:block, Genotype:rep, diam.mm=diam, height.cm=height, long.leaf.cm=longest_leaf, 
         total.branch=total_branch, repro.branch=repro_branch) %>% 
  mutate(Site="UCD")
names(ucd_ann_cens_prep)
```

```
##  [1] "BedLoc"       "block"        "Genotype"     "pop.mf"       "parent.pop"  
##  [6] "mf"           "rep"          "diam.mm"      "height.cm"    "long.leaf.cm"
## [11] "total.branch" "repro.branch" "Site"
```

``` r
anncens_rxnnorms <- bind_rows(ucd_ann_cens_prep, wl2_ann_cens_prep) %>% 
  select(-pheno) %>% 
  arrange(pop.mf, Site)
head(anncens_rxnnorms)
```

```
## # A tibble: 6 × 13
##   BedLoc  block Genotype pop.mf parent.pop    mf   rep diam.mm height.cm
##   <chr>   <chr> <chr>    <chr>  <chr>      <dbl> <dbl>   <dbl>     <dbl>
## 1 J2_37_B J2    BH_1_7   BH_1   BH             1     7   16.1       44.6
## 2 A_7_D   A     BH_1_3   BH_1   BH             1     3   NA         NA  
## 3 A_37_D  B     BH_1_4   BH_1   BH             1     4    1.89      NA  
## 4 B_6_C   D     BH_1_6   BH_1   BH             1     6   NA         NA  
## 5 B_46_D  C     BH_1_5   BH_1   BH             1     5   NA         NA  
## 6 C_40_B  E     BH_1_7   BH_1   BH             1     7    2.06      NA  
## # ℹ 4 more variables: long.leaf.cm <dbl>, total.branch <dbl>,
## #   repro.branch <dbl>, Site <chr>
```

### Year 2 at WL2

``` r
wl2_y2_pops <- read_csv("../input/WL2_Data/Final_2023_2024_Pop_Loc_Info.csv") %>%
  select(Pop.Type:unique.ID) %>% 
  filter(Pop.Type=="2023-survivor") %>% 
  select(Pop.Type, loc:bed, row=bedrow, col=bedcol, pop:unique.ID)
```

```
## Rows: 1217 Columns: 15
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (8): Pop.Type, status, block, loc, bed, bedcol, pop, unique.ID
## dbl (7): bed.block.order, bed.order, AB.CD.order, column.order, bedrow, mf, rep
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_blocks <- read_csv("../input/WL2_Data/CorrectedCSVs/WL2_mort_pheno_20231020_corrected.csv") %>% 
  unite(BedLoc, bed:bed.col, sep="_", remove = FALSE) %>% 
  filter(BedLoc!="K_5_C") %>% #get rid of duplicate locations
  select(block, pop, mf, rep) %>% #add in block info 
  mutate(mf=as.double(mf), rep=as.double(rep)) #convert to num
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

```
## Warning: There were 2 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `mf = as.double(mf)`.
## Caused by warning:
## ! NAs introduced by coercion
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
```

``` r
wl2_y2_pops_blocks <- left_join(wl2_y2_pops, wl2_blocks)
```

```
## Joining with `by = join_by(pop, mf, rep)`
```

``` r
wl2_ann_cens_2024 <- read_csv("../input/WL2_Data/WL2_Annual_Census_20241023_corrected.csv")
```

```
## Rows: 1217 Columns: 15
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (7): bed, col, unique.ID, phen, survey.date, collected.date, survey.notes
## dbl (8): row, diam.mm, num.flw, num.fruit, long.fruit.cm, total.branch, over...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_ann_cens_2024_pops <- left_join(wl2_y2_pops_blocks, wl2_ann_cens_2024) %>%  
  rename(Genotype=unique.ID, parent.pop=pop)
```

```
## Joining with `by = join_by(bed, row, col, unique.ID)`
```

## Load the pop and location data


``` r
#pop info
pops_common_garden <- read_csv("../input/WL2_Data/Pops_for_2023_WL2.csv") #pops included in common garden 
```

```
## Rows: 23 Columns: 5
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): parent.pop, elevation.group
## dbl (2): phylogroup, seed year
## lgl (1): notes
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
pops_common_garden_nonotes <- pops_common_garden %>% select(parent.pop, elevation.group) #subset columns

#extra location info 
pop_loc <- read_csv("../input/Strep_tort_locs.csv")
```

```
## Rows: 54 Columns: 7
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (6): Species epithet, Species Code, Site, Site code, Lat, Long
## dbl (1): Elevation (m)
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
#need to change YOSE to YO
pop_loc_yo <- pop_loc %>% mutate(parent.pop = str_replace(`Site code`, "YOSE(\\d+)", "YO\\1")) %>% select(Lat, Long, elev_m=`Elevation (m)`, parent.pop)

#merge in location info
pop_elev <- left_join(pops_common_garden_nonotes, pop_loc_yo)
```

```
## Joining with `by = join_by(parent.pop)`
```

## Add in location info


``` r
anncens_rxnnorms_loc <-left_join(anncens_rxnnorms, pop_elev) 
```

```
## Joining with `by = join_by(parent.pop)`
```

``` r
head(anncens_rxnnorms_loc)
```

```
## # A tibble: 6 × 17
##   BedLoc  block Genotype pop.mf parent.pop    mf   rep diam.mm height.cm
##   <chr>   <chr> <chr>    <chr>  <chr>      <dbl> <dbl>   <dbl>     <dbl>
## 1 J2_37_B J2    BH_1_7   BH_1   BH             1     7   16.1       44.6
## 2 A_7_D   A     BH_1_3   BH_1   BH             1     3   NA         NA  
## 3 A_37_D  B     BH_1_4   BH_1   BH             1     4    1.89      NA  
## 4 B_6_C   D     BH_1_6   BH_1   BH             1     6   NA         NA  
## 5 B_46_D  C     BH_1_5   BH_1   BH             1     5   NA         NA  
## 6 C_40_B  E     BH_1_7   BH_1   BH             1     7    2.06      NA  
## # ℹ 8 more variables: long.leaf.cm <dbl>, total.branch <dbl>,
## #   repro.branch <dbl>, Site <chr>, elevation.group <chr>, Lat <chr>,
## #   Long <chr>, elev_m <dbl>
```

``` r
#write_csv(anncens_rxnnorms_loc, "../output/AnnCens_Size_BothSites_Y1.csv")
```


``` r
wl2_ann_cens_2024_pops_loc <- left_join(wl2_ann_cens_2024_pops, pop_elev)
```

```
## Joining with `by = join_by(parent.pop)`
```

``` r
head(wl2_ann_cens_2024_pops_loc)
```

```
## # A tibble: 6 × 25
##   Pop.Type   loc   bed     row col   parent.pop    mf   rep Genotype block phen 
##   <chr>      <chr> <chr> <dbl> <chr> <chr>      <dbl> <dbl> <chr>    <chr> <chr>
## 1 2023-surv… A_6_B A         6 B     CC             3     3 CC_3_3   A     <NA> 
## 2 2023-surv… A_16… A        16 B     BH             3     3 BH_3_3   A     <NA> 
## 3 2023-surv… A_17… A        17 A     BH             7     3 BH_7_3   A     P    
## 4 2023-surv… A_18… A        18 A     BH             4     3 BH_4_3   A     <NA> 
## 5 2023-surv… A_24… A        24 A     WL2            7     9 WL2_7_9  A     P    
## 6 2023-surv… A_32… A        32 B     IH             7     4 IH_7_4   B     P    
## # ℹ 14 more variables: diam.mm <dbl>, num.flw <dbl>, num.fruit <dbl>,
## #   long.fruit.cm <dbl>, total.branch <dbl>, overhd.diam <dbl>,
## #   overhd.perp <dbl>, survey.date <chr>, collected.date <chr>,
## #   survey.notes <chr>, elevation.group <chr>, Lat <chr>, Long <chr>,
## #   elev_m <dbl>
```

## Maternal Family Sample Sizes 

``` r
#restrict to mfs with data at both sites 
anncens_surv_mfs_wide <- anncens_rxnnorms_loc %>% 
  group_by(pop.mf, parent.pop, Site, elev_m) %>% 
  summarise(N_height = sum(!is.na(height.cm))) %>% 
  select(pop.mf, Site, N_height) %>% 
  spread(Site, N_height) %>% 
  mutate(Both.Sites=if_else(!is.na(UCD) & !is.na(WL2), TRUE, FALSE)) %>% 
  filter(Both.Sites != "FALSE")
```

```
## `summarise()` has grouped output by 'pop.mf', 'parent.pop', 'Site'. You can
## override using the `.groups` argument.
## Adding missing grouping variables: `parent.pop`
```

``` r
anncens_surv_mfs_bothsites <- left_join(anncens_surv_mfs_wide, anncens_rxnnorms_loc)
```

```
## Joining with `by = join_by(parent.pop, pop.mf)`
```

``` r
mf_sample_size <- anncens_surv_mfs_bothsites %>% 
  count(parent.pop, pop.mf, Site) %>% 
  arrange(n)
mf_sample_size #24 mfs with data at both sites 
```

```
## # A tibble: 48 × 4
## # Groups:   pop.mf, parent.pop [24]
##    pop.mf parent.pop Site      n
##    <chr>  <chr>      <chr> <int>
##  1 BH_1   BH         UCD       1
##  2 CP2_1  CP2        UCD       1
##  3 DPR_5  DPR        UCD       1
##  4 DPR_6  DPR        UCD       1
##  5 IH_4   IH         UCD       1
##  6 IH_6   IH         UCD       1
##  7 SC_4   SC         UCD       1
##  8 SC_5   SC         UCD       1
##  9 SQ3_4  SQ3        UCD       1
## 10 TM2_6  TM2        UCD       1
## # ℹ 38 more rows
```

``` r
summary(mf_sample_size)
```

```
##     pop.mf           parent.pop            Site                 n         
##  Length:48          Length:48          Length:48          Min.   : 1.000  
##  Class :character   Class :character   Class :character   1st Qu.: 2.000  
##  Mode  :character   Mode  :character   Mode  :character   Median : 6.500  
##                                                           Mean   : 7.542  
##                                                           3rd Qu.:13.000  
##                                                           Max.   :21.000
```

``` r
mf_sample_size %>% 
  ggplot(aes(x=pop.mf, y=n)) +
  geom_col() + 
  labs(x="Pop.MF", y="Sample Size", title="Annual Census") +
  facet_wrap(~Site)
```

![](Ann_Cens_Size_RxnNorms_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

``` r
mf_per_pop <- anncens_surv_mfs_bothsites %>% 
  select(parent.pop, pop.mf) %>% 
  distinct() %>% 
  group_by(parent.pop) %>% 
  summarise(n = n()) %>% 
  arrange(n)

mf_per_pop %>% 
  ggplot(aes(x=fct_reorder(parent.pop, n), y=n)) +
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  theme_classic() +
  scale_y_continuous(limits = c(0,8), breaks = c(2, 4, 6, 8), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1)) +
  labs(x="Parent Population", y="# Maternal Families", title="Annual Census")
```

![](Ann_Cens_Size_RxnNorms_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

## Plot Reaction Norms
### Means by Pop


``` r
anncens_rxnnorms_summary_pops <- anncens_rxnnorms_loc %>% 
  group_by(parent.pop, Site, elev_m) %>% 
  summarise(N_diam = sum(!is.na(diam.mm)), mean_diam.mm = mean(diam.mm,na.rm=(TRUE)), 
            sem_diam.mm=sem(diam.mm, na.rm=(TRUE)), N_branch = sum(!is.na(total.branch)),
            mean_total.branch=mean(total.branch, na.rm=(TRUE)), 
            sem_total.branch=sem(total.branch, na.rm=TRUE))
```

```
## `summarise()` has grouped output by 'parent.pop', 'Site'. You can override
## using the `.groups` argument.
```

``` r
#anncens_rxnnorms_summary_pops
#No CC, CP3, FR, LV1, LV3, LVTR1, SQ1, SQ2, WR, WV, YO11, YO4, YO8 at UCD

#anncens_rxnnorms_summary_pops %>% arrange(N_diam) 
#1 SQ3, WL1 at UCD, 1 WR at WL2, 2 DPR, YO7 at UCD

anncens_rxnnorms_summary_pops2 <- anncens_rxnnorms_summary_pops %>% 
  filter(N_diam>2) %>% 
  filter(parent.pop=="BH" | parent.pop=="CP2" | parent.pop=="IH" | parent.pop=="SC" | 
           parent.pop=="TM2" | parent.pop=="WL2") %>% 
  mutate(Site=str_replace_all(Site, "UCD", "Low Elev"), 
         Site=str_replace_all(Site, "WL2", "High Elev"))
anncens_rxnnorms_summary_pops2$Site <- factor(anncens_rxnnorms_summary_pops2$Site,
                                               levels = c('Low Elev','High Elev'))
```

### Plot Pop Avgs


``` r
anncens_rxnnorms_summary_pops2 %>% 
  ggplot(aes(x=Site, y=mean_diam.mm, group=parent.pop, color=elev_m)) + 
  geom_point(size=1.5) + geom_line(linewidth=1.5) +
  geom_errorbar(aes(ymin=mean_diam.mm-sem_diam.mm,ymax=mean_diam.mm+sem_diam.mm),width=.1) +
  theme_classic() + scale_colour_gradient(low = "#F5A540", high = "#0043F0")  +
  labs(y="Stem Diam (mm)", color="Elevation (m)") +
  theme(text=element_text(size=28))
```

![](Ann_Cens_Size_RxnNorms_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

``` r
ggsave("../output/AnnCens_RxNorms_Diam_ALL_PopAvgs.png", width = 14, height = 8, units = "in")

anncens_rxnnorms_summary_pops2 %>% 
  ggplot(aes(x=Site, y=mean_total.branch, group=parent.pop, color=elev_m)) + 
  geom_point(size=1.5) + geom_line(linewidth=1.5) +
  geom_errorbar(aes(ymin=mean_total.branch-sem_total.branch,
                    ymax=mean_total.branch+sem_total.branch),width=.1) +
  theme_classic() + scale_colour_gradient(low = "#F5A540", high = "#0043F0")  +
  labs(y="Total Basal Branches", color="Elevation (m)") +
  theme(text=element_text(size=28))
```

![](Ann_Cens_Size_RxnNorms_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

``` r
ggsave("../output/AnnCens_RxNorms_Tot_Bas_Branch_ALL_PopAvgs.png", width = 14, height = 8, units = "in")
```

## Year 2 Pop Trends at WL2


``` r
wl2_ann_cens_2024_pops_loc %>% 
  filter(!is.na(diam.mm)) %>% 
  ggplot(aes(x=parent.pop, y=diam.mm)) +
  geom_boxplot()
```

![](Ann_Cens_Size_RxnNorms_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

``` r
wl2_ann_cens_2024_pops_loc %>% 
  filter(!is.na(total.branch)) %>% 
  ggplot(aes(x=parent.pop, y=total.branch)) +
  geom_boxplot()
```

![](Ann_Cens_Size_RxnNorms_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

``` r
wl2_ann_cens_2024_pops_loc %>% 
  filter(!is.na(overhd.diam)) %>% 
  ggplot(aes(x=parent.pop, y=overhd.diam)) +
  geom_boxplot()
```

![](Ann_Cens_Size_RxnNorms_files/figure-html/unnamed-chunk-11-3.png)<!-- -->

``` r
wl2_ann_cens_2024_pops_loc %>% 
  filter(!is.na(overhd.perp)) %>% 
  ggplot(aes(x=parent.pop, y=overhd.perp)) +
  geom_boxplot()
```

![](Ann_Cens_Size_RxnNorms_files/figure-html/unnamed-chunk-11-4.png)<!-- -->



``` r
wl2_ann_cens_2024_summary <- wl2_ann_cens_2024_pops_loc %>% 
  group_by(parent.pop) %>% 
  summarise(N_diam = sum(!is.na(diam.mm)), 
            mean_diam.mm = mean(diam.mm, na.rm=TRUE), sem_diam.mm=sem(diam.mm, na.rm=TRUE), 
            #N_branch = sum(!is.na(total.branch)), #very little variation in this trait 
            #mean_total.branch=mean(total.branch, na.rm=TRUE), sem_total.branch=sem(total.branch, na.rm=TRUE),
            N_overhd.diam = sum(!is.na(overhd.diam)),
            mean_overhd.diam=mean(overhd.diam, na.rm=TRUE), sem_overhd.diam=sem(overhd.diam, na.rm=TRUE),
            N_overhd.perp = sum(!is.na(overhd.perp)),
            mean_overhd.perp=mean(overhd.perp, na.rm=TRUE), sem_overhd.perp=sem(overhd.perp, na.rm=TRUE))

wl2_ann_cens_2024_summary %>% arrange(N_diam) #different sample sizes depending on the trait 
```

```
## # A tibble: 10 × 10
##    parent.pop N_diam mean_diam.mm sem_diam.mm N_overhd.diam mean_overhd.diam
##    <chr>       <int>        <dbl>       <dbl>         <int>            <dbl>
##  1 LV1             0       NaN         NA                 0           NaN   
##  2 SQ1             1         3.58      NA                 0           NaN   
##  3 WR              1         4.62      NA                 1            40.9 
##  4 TM2             3         3.27       1.09              0           NaN   
##  5 WL2             5         1.84       0.551             0           NaN   
##  6 CC             10         3.02       0.380             3            11.1 
##  7 SC             12         5.11       0.624             4            27.5 
##  8 IH             17         3.24       0.307             7            16.0 
##  9 YO7            17         1.85       0.261            11             8.54
## 10 BH             19         3.86       0.370            11            20.2 
## # ℹ 4 more variables: sem_overhd.diam <dbl>, N_overhd.perp <int>,
## #   mean_overhd.perp <dbl>, sem_overhd.perp <dbl>
```




