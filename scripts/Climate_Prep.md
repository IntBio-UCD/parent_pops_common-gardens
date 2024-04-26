---
title: "Climate Data Prep"
author: "Brandie Quarles"
date: "2023-10-18"
output: 
  html_document: 
    keep_md: yes
editor_options: 
  markdown: 
    wrap: 72
---

# Climate Data Preparation 



## Relevant Libraries and Functions


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
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(ggrepel)
library(cowplot)
```

```
## 
## Attaching package: 'cowplot'
## 
## The following object is masked from 'package:lubridate':
## 
##     stamp
```

```r
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

```r
library(corrplot) #plotting correlations 
```

```
## corrplot 0.92 loaded
```

```r
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

```r
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

```r
library(naniar) #replaces values with NA
library(QBMS) #for function calc_biovars to calculate bioclim variables
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

## Load the pop and location data


```r
#pop info
pops_common_garden <- read_csv("../input/UCD_Data/Pops_for_2022_UCD.csv") #pops included in common garden 
```

```
## Rows: 23 Columns: 11
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (5): parent.pop, elevation.group, collection.priority., on.climate.PCA.,...
## dbl (5): phylogroup, maternal.families, approx.number.seeds, UCD.seed.year, ...
## lgl (1): JGI.DNA
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
summary(pops_common_garden)
```

```
##   parent.pop          phylogroup    elevation.group    maternal.families
##  Length:23          Min.   :1.000   Length:23          Min.   : 3.0     
##  Class :character   1st Qu.:2.000   Class :character   1st Qu.:16.5     
##  Mode  :character   Median :4.000   Mode  :character   Median :23.0     
##                     Mean   :4.435                      Mean   :22.7     
##                     3rd Qu.:7.000                      3rd Qu.:29.5     
##                     Max.   :9.000                      Max.   :44.0     
##  approx.number.seeds UCD.seed.year  proposed.WL2.seed.year collection.priority.
##  Min.   : 100.0      Min.   :2014   Min.   :2014           Length:23           
##  1st Qu.: 500.0      1st Qu.:2018   1st Qu.:2020           Class :character    
##  Median : 500.0      Median :2020   Median :2021           Mode  :character    
##  Mean   : 608.7      Mean   :2020   Mean   :2020                               
##  3rd Qu.:1000.0      3rd Qu.:2021   3rd Qu.:2021                               
##  Max.   :1000.0      Max.   :2021   Max.   :2022                               
##  on.climate.PCA.    JGI.DNA           notes          
##  Length:23          Mode:logical   Length:23         
##  Class :character   NA's:23        Class :character  
##  Mode  :character                  Mode  :character  
##                                                      
##                                                      
## 
```

```r
names(pops_common_garden)
```

```
##  [1] "parent.pop"             "phylogroup"             "elevation.group"       
##  [4] "maternal.families"      "approx.number.seeds"    "UCD.seed.year"         
##  [7] "proposed.WL2.seed.year" "collection.priority."   "on.climate.PCA."       
## [10] "JGI.DNA"                "notes"
```

```r
pops_common_garden_nonotes <- pops_common_garden %>% select(parent.pop:elevation.group, UCD.seed.year)
pops_common_garden_nonotes
```

```
## # A tibble: 23 × 4
##    parent.pop phylogroup elevation.group UCD.seed.year
##    <chr>           <dbl> <chr>                   <dbl>
##  1 BH                  4 low                      2021
##  2 CC                  7 low                      2018
##  3 CP2                 2 high                     2019
##  4 CP3                 2 high                     2018
##  5 DPR                 5 mid                      2020
##  6 FR                  7 mid                      2019
##  7 IH                  5 low                      2021
##  8 LV1                 9 high                     2018
##  9 LV3                 9 high                     2018
## 10 LVTR1               9 high                     2020
## # ℹ 13 more rows
```

```r
pops_common_garden_nonotes$elevation.group <- str_to_title(pops_common_garden$elevation.group)

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

```r
names(pop_loc)
```

```
## [1] "Species epithet" "Species Code"    "Site"            "Site code"      
## [5] "Lat"             "Long"            "Elevation (m)"
```

```r
head(pop_loc)
```

```
## # A tibble: 6 × 7
##   `Species epithet` `Species Code` Site  `Site code` Lat   Long  `Elevation (m)`
##   <chr>             <chr>          <chr> <chr>       <chr> <chr>           <dbl>
## 1 Streptanthus tor… STTO           Ben … BH          37.4… -119…            511.
## 2 Streptanthus tor… STTO           Bidw… BB          39.5… -121…            283.
## 3 Streptanthus tor… STTO           Cany… CC          39.5… -121…            313 
## 4 Streptanthus tor… STTO           Cars… CP1         38.6… -120…           2422.
## 5 Streptanthus tor… STTO           Cars… CP2         38.6… -120…           2244.
## 6 Streptanthus tor… STTO           Cars… CP3         38.7… -120…           2266.
```

```r
unique(pop_loc$`Site code`)
```

```
##  [1] "BH"     "BB"     "CC"     "CP1"    "CP2"    "CP3"    "DP"     "DPR"   
##  [9] "FR"     NA       "HH"     "IH"     "KC1"    "KC2"    "KC3"    "LV1"   
## [17] "LV2"    "LV3"    "LVTR1"  "LVTR2"  "LVTR3"  "SQ1"    "SQ2"    "SQ3"   
## [25] "SHA"    "SC"     "TM1"    "TM2"    "WR"     "WV"     "WL1"    "WL2"   
## [33] "WL3"    "WL4"    "YOSE1"  "YOSE10" "YOSE11" "YOSE12" "YOSE13" "YOSE2" 
## [41] "YOSE3"  "YOSE4"  "YOSE5"  "YOSE6"  "YOSE7"  "YOSE8"  "YOSE9"
```

```r
unique(pops_common_garden_nonotes$parent.pop)
```

```
##  [1] "BH"    "CC"    "CP2"   "CP3"   "DPR"   "FR"    "IH"    "LV1"   "LV3"  
## [10] "LVTR1" "SC"    "SQ1"   "SQ2"   "SQ3"   "TM2"   "WL1"   "WL2"   "WR"   
## [19] "WV"    "YO11"  "YO4"   "YO7"   "YO8"
```

```r
#need to change YOSE to YO
pop_loc_yo <- pop_loc %>% mutate(parent.pop = str_replace(`Site code`, "YOSE(\\d+)", "YO\\1")) %>% select(Lat, Long, elev_m=`Elevation (m)`, parent.pop)
unique(pop_loc_yo$parent.pop)
```

```
##  [1] "BH"    "BB"    "CC"    "CP1"   "CP2"   "CP3"   "DP"    "DPR"   "FR"   
## [10] NA      "HH"    "IH"    "KC1"   "KC2"   "KC3"   "LV1"   "LV2"   "LV3"  
## [19] "LVTR1" "LVTR2" "LVTR3" "SQ1"   "SQ2"   "SQ3"   "SHA"   "SC"    "TM1"  
## [28] "TM2"   "WR"    "WV"    "WL1"   "WL2"   "WL3"   "WL4"   "YO1"   "YO10" 
## [37] "YO11"  "YO12"  "YO13"  "YO2"   "YO3"   "YO4"   "YO5"   "YO6"   "YO7"  
## [46] "YO8"   "YO9"
```

```r
#merge in location info
pop_elev <- left_join(pops_common_garden_nonotes, pop_loc_yo)
```

```
## Joining with `by = join_by(parent.pop)`
```

```r
head(pop_elev)
```

```
## # A tibble: 6 × 7
##   parent.pop phylogroup elevation.group UCD.seed.year Lat      Long       elev_m
##   <chr>           <dbl> <chr>                   <dbl> <chr>    <chr>       <dbl>
## 1 BH                  4 Low                      2021 37.40985 -119.96458   511.
## 2 CC                  7 Low                      2018 39.58597 -121.43311   313 
## 3 CP2                 2 High                     2019 38.66169 -120.13065  2244.
## 4 CP3                 2 High                     2018 38.70649 -120.08797  2266.
## 5 DPR                 5 Mid                      2020 39.22846 -120.81518  1019.
## 6 FR                  7 Mid                      2019 40.01362 -121.18498   787
```

## Map populations


```r
names(pop_elev)
```

```
## [1] "parent.pop"      "phylogroup"      "elevation.group" "UCD.seed.year"  
## [5] "Lat"             "Long"            "elev_m"
```

```r
sapply(pop_elev, class) #lat and long are characters, need to be numeric 
```

```
##      parent.pop      phylogroup elevation.group   UCD.seed.year             Lat 
##     "character"       "numeric"     "character"       "numeric"     "character" 
##            Long          elev_m 
##     "character"       "numeric"
```

```r
pop_elev <- pop_elev %>% mutate_at(c("Lat", "Long"), as.double)
states <- map_data("state") %>% filter(region == "california")


#without labels 
ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "gray") +
  coord_quickmap(xlim = c(-125, -114), ylim = c(35, 43))+
  geom_point(data = pop_elev,
             aes(x = Long, y = Lat, color=elev_m),
             size = 4) +
  labs(x="Long", y="Lat", color="Elevation (m)") +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  theme_classic() +
  theme(text=element_text(size=25))
```

![](Climate_Prep_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#ggsave("../output/Pop_Map_NoLabels.png", width = 6, height = 4, units = "in")

#with labels 
ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "gray") +
  coord_quickmap(xlim = c(-125, -114), ylim = c(35, 43))+
  geom_point(data = pop_elev,
             aes(x = Long, y = Lat, color=elev_m),
             size = 4) +
  geom_text_repel(data = pop_elev,
         aes(x = Long, y = Lat,
             label = `parent.pop`),
         min.segment.length = 0,
         max.overlaps = 100,
        # label.padding = 1,
        # point.padding = 0.5,
         size = 4) +
  labs(x="Long", y="Lat", color="Elevation (m)") +
  scale_colour_gradient(low = "#F5A540", high = "#0043F0") +
  theme_classic() +
  theme(text=element_text(size=25))
```

![](Climate_Prep_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
#ggsave("../output/Pop_Map_Labels.png", width = 12, height = 8, units = "in")
```

## Elevation group changes


```r
names(pop_elev)
unique(pop_elev$parent.pop)
xtabs(~parent.pop+elevation.group, data = pop_elev)

pop_elev$elevation.group <- factor(pop_elev$elevation.group, levels=elev_order)
pop_elev %>% ggplot(aes(x=fct_reorder(parent.pop, elev_m), y=elev_m, fill=elevation.group)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=elev_three_palette) + 
  labs(title="New Elevation Classifications", x="Parent Populations", y= "Elevation (m)") + 
  theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))
ggsave("../output/Elevation_Class_New.png", width = 12, height = 6, units = "in")
```

# Flint Climate Data (1895-2022)

-   aet (mm) = actual evapotranspiration (water transpired by plants, if
    water not limited)
-   cwd (mm) = climate water deficit (estimate of drought) = pet-aet
-   pck (mm) = snowpack (prior month pck + snow - lost snow)
-   pet (mm) = potential evapotranspiration (total amount of water that
    can evaporate from the ground or be transpired by plants)
-   ppt (mm) = precipitation
-   rch (mm) = recharge (water that penetrates below the root zone)
-   run (mm) = runoff (water that becomes streamflow)
-   str (mm) = soil water storage (avg amount of water stored in the
    soil annually)
-   all above are totals or sums per month
-   tmn (deg C) = min air temp (for a given month)
-   tmx (deg C) = max air temp (for a given month) 127 years

Notes

-   Did some lit review to see what variables other people have focused
    on or found to be significant (CWD, temp, precip, AET)

-   Temperature likely highly correlated within pops w/in years

    -   Take all variables (including avg temp) for the 3 months when we
        think germination is occurring (Low: Oct-Jan, Mid: Nov-Jan,
        High: April-June)?

    -   Decided to use growth season rather than germ season: Low:
        Oct-July, Mid: Nov-Aug, High: April-Sept

## Load the climate data


```r
climate_data <- read_csv("../input/Dimensions_All_1895-2022.csv")
```

```
## Rows: 225552 Columns: 13
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (1): pop
## dbl (12): year, month, aet, cwd, pck, pet, ppt, rch, run, str, tmn, tmx
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
head(climate_data)
```

```
## # A tibble: 6 × 13
##   pop     year month   aet   cwd   pck   pet   ppt   rch   run   str   tmn   tmx
##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 CAAM-…  1895    10 25.8   62.8     0  88.6  6.78     0     0 108.   9.76  21.9
## 2 CAAM-…  1895    10 13.7   80.0     0  93.7  6.76     0     0  95.1  9.91  22.1
## 3 CAAM-…  1895    10  9.99  92.8     0 103.   6.59     0     0 129.   9.37  23.5
## 4 CAAM-…  1895    10 10.6   88.1     0  98.7 11.1      0     0 338.   9.30  23.1
## 5 CAAM-…  1895    10  8.85  90.6     0  99.4  6.29     0     0 103.   6.86  19.1
## 6 CAAM-…  1895    10 11.4   65.9     0  77.3  7.06     0     0 193.   4.45  15.9
```

```r
names(climate_data)
```

```
##  [1] "pop"   "year"  "month" "aet"   "cwd"   "pck"   "pet"   "ppt"   "rch"  
## [10] "run"   "str"   "tmn"   "tmx"
```

```r
unique(climate_data$pop)
```

```
##   [1] "CAAM-CC"    "CAAM-GB"    "CAAM-Oaks"  "CAAM-FMR"   "CAAM-IHC"  
##   [6] "CAAM-MB"    "CAAM-OCT"   "CAAN-BCB"   "CAAN1"      "CAAN2"     
##  [11] "CAAN-BCR"   "CAAN-BCT"   "CAAN-CV3"   "CAAN-CCMA"  "CAAN-GH"   
##  [16] "CAAN-UC"    "CAAN-SLSR"  "CACO1"      "CACO2"      "CACO3"     
##  [21] "CACO-GSE"   "CACO-OC"    "CACO-BC"    "CACO-Davis" "CACO-Oaks" 
##  [26] "CACO-BCR"   "CACO-ODR"   "CACO-SC"    "CAIN2"      "CAIN3"     
##  [31] "CAIN4"      "CAIN-395"   "CAIN-S-395" "CAIN-BCC"   "CAIN-BCR"  
##  [36] "CAIN-KJ"    "CAIN-MR"    "CAIN-SMR"   "STBR1"      "STBR2"     
##  [41] "STBR3"      "STBR-BCG"   "STBR-HC"    "STBR-LM"    "STBR-MPCH" 
##  [46] "STBR-MCH"   "STBR-MNJ"   "STBR-WR"    "STBR-M"     "STDI"      
##  [51] "STDI-HL"    "STDI-TM"    "STDR2"      "STDR-BR"    "STDR-45"   
##  [56] "STDR-P1"    "STDR-P2"    "STGL1"      "STGL2"      "STGL3"     
##  [61] "STGL-BCR"   "STGL-MH"    "STGL-AQSM"  "STGL-CR"    "STGL-CHM"  
##  [66] "STGL-KBR"   "STGL-MM"    "STGL-TFA"   "STGL-MRH"   "STGL-MHO"  
##  [71] "STGL-MMSP"  "STGL-MTSP"  "STGL-SG"    "STGL-SR"    "STIN"      
##  [76] "STIN-JM"    "STIN-TM"    "STPO1"      "STPO2"      "STPO3"     
##  [81] "STPO-LS"    "STPO-B"     "STPO-F"     "STPO-IHR"   "STPO-P"    
##  [86] "STPO-PU"    "STPO-R"     "STPO-RH"    "STPO-S"     "STPO-YH"   
##  [91] "BH"         "BB"         "BR"         "CC"         "CP1"       
##  [96] "CP2"        "CP3"        "CL"         "DP"         "DPR"       
## [101] "FR"         "GM"         "HH"         "HM"         "IH"        
## [106] "JB"         "KC1"        "KC2"        "KC3"        "LV1"       
## [111] "LV2"        "LV3"        "LVTR1"      "LVTR2"      "LVTR3"     
## [116] "MSH"        "PL"         "RHC"        "RB"         "RG"        
## [121] "SQ1"        "SQ2"        "SQ3"        "SH"         "SO"        
## [126] "SC"         "TM1"        "TM2"        "TFC"        "WR"        
## [131] "WV"         "WL1"        "WL2"        "WL3"        "WL4"       
## [136] "YOSE1"      "YOSE10"     "YOSE11"     "YOSE12"     "YOSE13"    
## [141] "YOSE2"      "YOSE3"      "YOSE4"      "YOSE5"      "YOSE6"     
## [146] "YOSE7"      "YOSE8"      "YOSE9"
```

```r
climate_data$year = as.character(climate_data$year)
head(climate_data)
```

```
## # A tibble: 6 × 13
##   pop    year  month   aet   cwd   pck   pet   ppt   rch   run   str   tmn   tmx
##   <chr>  <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 CAAM-… 1895     10 25.8   62.8     0  88.6  6.78     0     0 108.   9.76  21.9
## 2 CAAM-… 1895     10 13.7   80.0     0  93.7  6.76     0     0  95.1  9.91  22.1
## 3 CAAM-… 1895     10  9.99  92.8     0 103.   6.59     0     0 129.   9.37  23.5
## 4 CAAM-… 1895     10 10.6   88.1     0  98.7 11.1      0     0 338.   9.30  23.1
## 5 CAAM-… 1895     10  8.85  90.6     0  99.4  6.29     0     0 103.   6.86  19.1
## 6 CAAM-… 1895     10 11.4   65.9     0  77.3  7.06     0     0 193.   4.45  15.9
```

```r
climate_data_yo <- climate_data %>% mutate(pop = str_replace(pop, "YOSE(\\d+)", "YO\\1")) #changing YOSE to YO 
#`(\\d+)`: This part contains a regular expression within parentheses: 
#`\\d`: Matches a digit (equivalent to `[0-9]`). 
#The double backslash `\\` is used to escape the `\` #character in R strings, so `\\d` is interpreted as `\d` in the regex, denoting a digit.
#`+`: This qualifier means "one or more of the preceding element," so `\\d+` will match one or more digits.
#`()`: Parentheses are used to define a capturing group, which means that the part of the regex within the parentheses `(\\d+)` is captured to be used in back-references or for extracting matched parts.
#\\1 references the first capturing group
unique(climate_data_yo$pop)
```

```
##   [1] "CAAM-CC"    "CAAM-GB"    "CAAM-Oaks"  "CAAM-FMR"   "CAAM-IHC"  
##   [6] "CAAM-MB"    "CAAM-OCT"   "CAAN-BCB"   "CAAN1"      "CAAN2"     
##  [11] "CAAN-BCR"   "CAAN-BCT"   "CAAN-CV3"   "CAAN-CCMA"  "CAAN-GH"   
##  [16] "CAAN-UC"    "CAAN-SLSR"  "CACO1"      "CACO2"      "CACO3"     
##  [21] "CACO-GSE"   "CACO-OC"    "CACO-BC"    "CACO-Davis" "CACO-Oaks" 
##  [26] "CACO-BCR"   "CACO-ODR"   "CACO-SC"    "CAIN2"      "CAIN3"     
##  [31] "CAIN4"      "CAIN-395"   "CAIN-S-395" "CAIN-BCC"   "CAIN-BCR"  
##  [36] "CAIN-KJ"    "CAIN-MR"    "CAIN-SMR"   "STBR1"      "STBR2"     
##  [41] "STBR3"      "STBR-BCG"   "STBR-HC"    "STBR-LM"    "STBR-MPCH" 
##  [46] "STBR-MCH"   "STBR-MNJ"   "STBR-WR"    "STBR-M"     "STDI"      
##  [51] "STDI-HL"    "STDI-TM"    "STDR2"      "STDR-BR"    "STDR-45"   
##  [56] "STDR-P1"    "STDR-P2"    "STGL1"      "STGL2"      "STGL3"     
##  [61] "STGL-BCR"   "STGL-MH"    "STGL-AQSM"  "STGL-CR"    "STGL-CHM"  
##  [66] "STGL-KBR"   "STGL-MM"    "STGL-TFA"   "STGL-MRH"   "STGL-MHO"  
##  [71] "STGL-MMSP"  "STGL-MTSP"  "STGL-SG"    "STGL-SR"    "STIN"      
##  [76] "STIN-JM"    "STIN-TM"    "STPO1"      "STPO2"      "STPO3"     
##  [81] "STPO-LS"    "STPO-B"     "STPO-F"     "STPO-IHR"   "STPO-P"    
##  [86] "STPO-PU"    "STPO-R"     "STPO-RH"    "STPO-S"     "STPO-YH"   
##  [91] "BH"         "BB"         "BR"         "CC"         "CP1"       
##  [96] "CP2"        "CP3"        "CL"         "DP"         "DPR"       
## [101] "FR"         "GM"         "HH"         "HM"         "IH"        
## [106] "JB"         "KC1"        "KC2"        "KC3"        "LV1"       
## [111] "LV2"        "LV3"        "LVTR1"      "LVTR2"      "LVTR3"     
## [116] "MSH"        "PL"         "RHC"        "RB"         "RG"        
## [121] "SQ1"        "SQ2"        "SQ3"        "SH"         "SO"        
## [126] "SC"         "TM1"        "TM2"        "TFC"        "WR"        
## [131] "WV"         "WL1"        "WL2"        "WL3"        "WL4"       
## [136] "YO1"        "YO10"       "YO11"       "YO12"       "YO13"      
## [141] "YO2"        "YO3"        "YO4"        "YO5"        "YO6"       
## [146] "YO7"        "YO8"        "YO9"
```

## Combine pop info with climate data

```r
names(pop_elev)
```

```
## [1] "parent.pop"      "phylogroup"      "elevation.group" "UCD.seed.year"  
## [5] "Lat"             "Long"            "elev_m"
```

```r
names(climate_data_yo)
```

```
##  [1] "pop"   "year"  "month" "aet"   "cwd"   "pck"   "pet"   "ppt"   "rch"  
## [10] "run"   "str"   "tmn"   "tmx"
```

```r
pop_elev_climate <- left_join(pop_elev, climate_data_yo, by=c("parent.pop"="pop")) %>% select(parent.pop, elevation.group, elev_m, year:tmx)
#write_csv(pop_elev_climate, "../output/Climate/flint_climate_UCDpops.csv") #this is the file used in "Flint_Growth_Season.Rmd"
unique(pop_elev_climate$parent.pop)
```

```
##  [1] "BH"    "CC"    "CP2"   "CP3"   "DPR"   "FR"    "IH"    "LV1"   "LV3"  
## [10] "LVTR1" "SC"    "SQ1"   "SQ2"   "SQ3"   "TM2"   "WL1"   "WL2"   "WR"   
## [19] "WV"    "YO11"  "YO4"   "YO7"   "YO8"
```

```r
#unique(pop_elev$parent.pop)
head(pop_elev_climate, 30)
```

```
## # A tibble: 30 × 15
##    parent.pop elevation.group elev_m year  month    aet   cwd   pck   pet
##    <chr>      <chr>            <dbl> <chr> <dbl>  <dbl> <dbl> <dbl> <dbl>
##  1 BH         Low               511. 1895     10   8.90  80.8     0  89.7
##  2 BH         Low               511. 1895     11   4.89  40.8     0  45.7
##  3 BH         Low               511. 1895     12   3.23  27.9     0  31.1
##  4 BH         Low               511. 1896      1   8.75  27.0     0  35.7
##  5 BH         Low               511. 1896      2   6.57  42.8     0  49.4
##  6 BH         Low               511. 1896      3  24.9   57.9     0  82.8
##  7 BH         Low               511. 1896      4  72.5   36.2     0 109. 
##  8 BH         Low               511. 1896      5 141.    11.6     0 153. 
##  9 BH         Low               511. 1896      6  90.7   96.2     0 187. 
## 10 BH         Low               511. 1896      7 133.    70.5     0 203. 
## # ℹ 20 more rows
## # ℹ 6 more variables: ppt <dbl>, rch <dbl>, run <dbl>, str <dbl>, tmn <dbl>,
## #   tmx <dbl>
```

```r
names(pop_elev_climate)
```

```
##  [1] "parent.pop"      "elevation.group" "elev_m"          "year"           
##  [5] "month"           "aet"             "cwd"             "pck"            
##  [9] "pet"             "ppt"             "rch"             "run"            
## [13] "str"             "tmn"             "tmx"
```

## Calculation of recent (last 30 years) and historical climate (prior 30 years)

Took out 2022 b/c only 9 months for that year Flint variables


```r
pop_elev_climate_recent <- pop_elev_climate %>% filter(year>1991 & year<2022) %>% select(parent.pop:month, cwd, pck, ppt, tmn, tmx)
head(pop_elev_climate_recent)
```

```
## # A tibble: 6 × 10
##   parent.pop elevation.group elev_m year  month   cwd   pck      ppt   tmn   tmx
##   <chr>      <chr>            <dbl> <chr> <dbl> <dbl> <dbl>    <dbl> <dbl> <dbl>
## 1 BH         Low               511. 1992      1  28.3     0  42.7     1.22  10.6
## 2 BH         Low               511. 1992      2  40.1     0 175.      5.26  16.0
## 3 BH         Low               511. 1992      3  52.0     0  76.3     6.21  17.4
## 4 BH         Low               511. 1992      4  75.9     0   2.19    8.31  23.6
## 5 BH         Low               511. 1992      5  78.6     0   0.0800 12.7   29.4
## 6 BH         Low               511. 1992      6  99.1     0   1.77   13.6   30.2
```

```r
tail(pop_elev_climate_recent)
```

```
## # A tibble: 6 × 10
##   parent.pop elevation.group elev_m year  month   cwd   pck    ppt    tmn   tmx
##   <chr>      <chr>            <dbl> <chr> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl>
## 1 YO8        High             2591. 2021      7 114.     0   30.0  10.8   24.1 
## 2 YO8        High             2591. 2021      8 127.     0    3.33  9.11  22.7 
## 3 YO8        High             2591. 2021      9 107.     0    3.39  5.92  20.4 
## 4 YO8        High             2591. 2021     10  68.4    0  202.   -0.660 10.6 
## 5 YO8        High             2591. 2021     11  41.0    0   30.0  -0.820 10.8 
## 6 YO8        High             2591. 2021     12  23.3  400. 408.   -5.79   2.32
```

```r
summary(pop_elev_climate_recent)
```

```
##   parent.pop        elevation.group        elev_m           year          
##  Length:8280        Length:8280        Min.   : 313.0   Length:8280       
##  Class :character   Class :character   1st Qu.: 748.9   Class :character  
##  Mode  :character   Mode  :character   Median :1934.5   Mode  :character  
##                                        Mean   :1649.7                     
##                                        3rd Qu.:2373.2                     
##                                        Max.   :2872.3                     
##      month            cwd              pck               ppt        
##  Min.   : 1.00   Min.   :  0.00   Min.   :   0.00   Min.   :  0.00  
##  1st Qu.: 3.75   1st Qu.: 25.38   1st Qu.:   0.00   1st Qu.:  7.48  
##  Median : 6.50   Median : 46.28   Median :   0.00   Median : 48.34  
##  Mean   : 6.50   Mean   : 55.83   Mean   : 131.07   Mean   : 99.06  
##  3rd Qu.: 9.25   3rd Qu.: 82.86   3rd Qu.:  84.82   3rd Qu.:143.49  
##  Max.   :12.00   Max.   :194.73   Max.   :2183.62   Max.   :981.42  
##       tmn               tmx        
##  Min.   :-13.180   Min.   :-3.220  
##  1st Qu.: -1.863   1st Qu.: 9.398  
##  Median :  3.490   Median :15.680  
##  Mean   :  3.531   Mean   :16.506  
##  3rd Qu.:  8.650   3rd Qu.:23.020  
##  Max.   : 21.350   Max.   :37.380
```

```r
xtabs(~parent.pop+month, data=pop_elev_climate_recent)
```

```
##           month
## parent.pop  1  2  3  4  5  6  7  8  9 10 11 12
##      BH    30 30 30 30 30 30 30 30 30 30 30 30
##      CC    30 30 30 30 30 30 30 30 30 30 30 30
##      CP2   30 30 30 30 30 30 30 30 30 30 30 30
##      CP3   30 30 30 30 30 30 30 30 30 30 30 30
##      DPR   30 30 30 30 30 30 30 30 30 30 30 30
##      FR    30 30 30 30 30 30 30 30 30 30 30 30
##      IH    30 30 30 30 30 30 30 30 30 30 30 30
##      LV1   30 30 30 30 30 30 30 30 30 30 30 30
##      LV3   30 30 30 30 30 30 30 30 30 30 30 30
##      LVTR1 30 30 30 30 30 30 30 30 30 30 30 30
##      SC    30 30 30 30 30 30 30 30 30 30 30 30
##      SQ1   30 30 30 30 30 30 30 30 30 30 30 30
##      SQ2   30 30 30 30 30 30 30 30 30 30 30 30
##      SQ3   30 30 30 30 30 30 30 30 30 30 30 30
##      TM2   30 30 30 30 30 30 30 30 30 30 30 30
##      WL1   30 30 30 30 30 30 30 30 30 30 30 30
##      WL2   30 30 30 30 30 30 30 30 30 30 30 30
##      WR    30 30 30 30 30 30 30 30 30 30 30 30
##      WV    30 30 30 30 30 30 30 30 30 30 30 30
##      YO11  30 30 30 30 30 30 30 30 30 30 30 30
##      YO4   30 30 30 30 30 30 30 30 30 30 30 30
##      YO7   30 30 30 30 30 30 30 30 30 30 30 30
##      YO8   30 30 30 30 30 30 30 30 30 30 30 30
```

```r
pop_elev_climate_historical <- pop_elev_climate %>% filter(year<=1991 & year>1961) %>% select(parent.pop:month, cwd, pck, ppt, tmn, tmx)
head(pop_elev_climate_historical, 13)
```

```
## # A tibble: 13 × 10
##    parent.pop elevation.group elev_m year  month   cwd   pck     ppt   tmn   tmx
##    <chr>      <chr>            <dbl> <chr> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>
##  1 BH         Low               511. 1962      1  28.5     0  59.8   -1.44  11.9
##  2 BH         Low               511. 1962      2  37.4     0 289.     2.55  12.5
##  3 BH         Low               511. 1962      3  45.3     0  80.6    2.15  15.0
##  4 BH         Low               511. 1962      4  74.5     0   8.19   6.16  23.9
##  5 BH         Low               511. 1962      5  70.6     0   3.08   6.72  23.8
##  6 BH         Low               511. 1962      6  97.2     0   0.820 11.6   31.5
##  7 BH         Low               511. 1962      7 127.      0   1.31  14.5   35.3
##  8 BH         Low               511. 1962      8 141.      0   0     13.9   34.4
##  9 BH         Low               511. 1962      9 125.      0   1.44  11.9   32.1
## 10 BH         Low               511. 1962     10  86.2     0  48.8    8.47  24.1
## 11 BH         Low               511. 1962     11  44.6     0   9.85   4.13  19.4
## 12 BH         Low               511. 1962     12  31.2     0  57.8    2.20  15.4
## 13 BH         Low               511. 1963      1  28.1     0 109.    -1.85  12.3
```

```r
xtabs(~parent.pop+month, data=pop_elev_climate_historical)
```

```
##           month
## parent.pop  1  2  3  4  5  6  7  8  9 10 11 12
##      BH    30 30 30 30 30 30 30 30 30 30 30 30
##      CC    30 30 30 30 30 30 30 30 30 30 30 30
##      CP2   30 30 30 30 30 30 30 30 30 30 30 30
##      CP3   30 30 30 30 30 30 30 30 30 30 30 30
##      DPR   30 30 30 30 30 30 30 30 30 30 30 30
##      FR    30 30 30 30 30 30 30 30 30 30 30 30
##      IH    30 30 30 30 30 30 30 30 30 30 30 30
##      LV1   30 30 30 30 30 30 30 30 30 30 30 30
##      LV3   30 30 30 30 30 30 30 30 30 30 30 30
##      LVTR1 30 30 30 30 30 30 30 30 30 30 30 30
##      SC    30 30 30 30 30 30 30 30 30 30 30 30
##      SQ1   30 30 30 30 30 30 30 30 30 30 30 30
##      SQ2   30 30 30 30 30 30 30 30 30 30 30 30
##      SQ3   30 30 30 30 30 30 30 30 30 30 30 30
##      TM2   30 30 30 30 30 30 30 30 30 30 30 30
##      WL1   30 30 30 30 30 30 30 30 30 30 30 30
##      WL2   30 30 30 30 30 30 30 30 30 30 30 30
##      WR    30 30 30 30 30 30 30 30 30 30 30 30
##      WV    30 30 30 30 30 30 30 30 30 30 30 30
##      YO11  30 30 30 30 30 30 30 30 30 30 30 30
##      YO4   30 30 30 30 30 30 30 30 30 30 30 30
##      YO7   30 30 30 30 30 30 30 30 30 30 30 30
##      YO8   30 30 30 30 30 30 30 30 30 30 30 30
```

## Snow Cover - Average across years (all months included)


```r
names(pop_elev_climate_recent)
```

```
##  [1] "parent.pop"      "elevation.group" "elev_m"          "year"           
##  [5] "month"           "cwd"             "pck"             "ppt"            
##  [9] "tmn"             "tmx"
```

```r
pop_elev_climate_recent %>% filter(year==2006) %>% ggplot(aes(x=month, y=pck, group=parent.pop, color=parent.pop)) + geom_point() + geom_line()
```

![](Climate_Prep_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
pop_elev_climate_recent %>% filter(year==2006) %>% ggplot(aes(x=month, y=pck, group=parent.pop, color=elevation.group)) + geom_point() + geom_line()
```

![](Climate_Prep_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

```r
#average snowpack across entire year for 30-year period 
recent_climate_avgs <- pop_elev_climate_recent %>% group_by(parent.pop,elevation.group ,elev_m) %>% summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean, sem), na.rm = TRUE) 
names(recent_climate_avgs) <- gsub("fn2", "sem", colnames(recent_climate_avgs))
names(recent_climate_avgs) <-gsub("fn1", "mean", colnames(recent_climate_avgs))
recent_climate_avgs #30 year averages
```

```
## # A tibble: 23 × 13
## # Groups:   parent.pop, elevation.group [23]
##    parent.pop elevation.group elev_m cwd_mean pck_mean ppt_mean tmn_mean
##    <chr>      <chr>            <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
##  1 BH         Low               511.     75.5    0         48.6    8.81 
##  2 CC         Low               313      59.6    0         85.7    9.97 
##  3 CP2        High             2244.     62.7  203.       106.     1.12 
##  4 CP3        High             2266.     46.2  221.       102.     0.475
##  5 DPR        Mid              1019.     27.5    8.95     122.     7.80 
##  6 FR         Mid               787      55.8   19.0       83.1    5.33 
##  7 IH         Low               454.     49.1    0.194     89.9    8.63 
##  8 LV1        High             2593.     49.4  438.       148.    -1.44 
##  9 LV3        High             2354.     57.2  423.       147.    -1.42 
## 10 LVTR1      High             2741.     51.6  451.       153.    -1.63 
## # ℹ 13 more rows
## # ℹ 6 more variables: tmx_mean <dbl>, cwd_sem <dbl>, pck_sem <dbl>,
## #   ppt_sem <dbl>, tmn_sem <dbl>, tmx_sem <dbl>
```

```r
recent_climate_avgs$elevation.group <- factor(recent_climate_avgs$elevation.group, levels=elev_order)
recent_snwpck <- recent_climate_avgs %>% ggplot(aes(x=fct_reorder(parent.pop, pck_mean), y=pck_mean, fill=elevation.group)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=pck_mean-pck_sem,ymax=pck_mean+pck_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=elev_three_palette) + 
  #scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(x="Population", y="Avg SnwPck (mm)" ,title = "Recent Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))
recent_snwpck
```

![](Climate_Prep_files/figure-html/unnamed-chunk-8-3.png)<!-- -->

```r
#ggsave("../output/Climate/All_Year_Avg_PCK_RecentClim.png", width = 12, height = 6, units = "in")

historical_climate_avgs <- pop_elev_climate_historical %>% group_by(parent.pop, elevation.group ,elev_m) %>% summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), c(mean, sem), na.rm = TRUE) 
names(historical_climate_avgs) <- gsub("fn2", "sem", colnames(historical_climate_avgs))
names(historical_climate_avgs) <-gsub("fn1", "mean", colnames(historical_climate_avgs))
historical_climate_avgs #30 year averages
```

```
## # A tibble: 23 × 13
## # Groups:   parent.pop, elevation.group [23]
##    parent.pop elevation.group elev_m cwd_mean pck_mean ppt_mean tmn_mean
##    <chr>      <chr>            <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
##  1 BH         Low               511.     74.4   0.0195     48.4    7.63 
##  2 CC         Low               313      59.9   0.0793     82.1    8.85 
##  3 CP2        High             2244.     60.3 263.        112.    -0.396
##  4 CP3        High             2266.     43.7 281.        108.    -0.938
##  5 DPR        Mid              1019.     26.3  19.0       122.     6.15 
##  6 FR         Mid               787      54.7  21.7        82.8    4.08 
##  7 IH         Low               454.     50.2   1.45       88.9    7.61 
##  8 LV1        High             2593.     46.7 510.        151.    -3.51 
##  9 LV3        High             2354.     54.6 494.        149.    -3.52 
## 10 LVTR1      High             2741.     49.3 530.        156.    -3.66 
## # ℹ 13 more rows
## # ℹ 6 more variables: tmx_mean <dbl>, cwd_sem <dbl>, pck_sem <dbl>,
## #   ppt_sem <dbl>, tmn_sem <dbl>, tmx_sem <dbl>
```

```r
historical_climate_avgs$elevation.group <- factor(historical_climate_avgs$elevation.group, levels=elev_order)
hist_snwpck <- historical_climate_avgs %>% ggplot(aes(x=fct_reorder(parent.pop, pck_mean), y=pck_mean, fill=elevation.group)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=pck_mean-pck_sem,ymax=pck_mean+pck_sem),width=.2, position = 
                  position_dodge(0.75)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=elev_three_palette) + 
  #scale_fill_gradient(low = "#F5A540", high = "#0043F0") +
  labs(x="Population", y="Avg SnwPck (mm)" ,title = "Historical Climate") +
   theme_classic() +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45,  hjust = 1))
hist_snwpck
```

![](Climate_Prep_files/figure-html/unnamed-chunk-8-4.png)<!-- -->

```r
#ggsave("../output/Climate/All_Year_Avg_PCK_HistoricalClim.png", width = 8, height = 4, units = "in")

legend <- get_legend(hist_snwpck)
hist_snwpck <- hist_snwpck + theme(legend.position="none")
recent_snwpck <- recent_snwpck + theme(legend.position="none")
grid.arrange(hist_snwpck, recent_snwpck, legend, ncol=3, widths=c(3.12, 3.12, 1.09))
```

![](Climate_Prep_files/figure-html/unnamed-chunk-8-5.png)<!-- -->

## Consistency of winter snow cover


```r
#monthly_pck <- pop_elev_climate_recent %>% filter(parent.pop==c("DPR","WR","WL1", "SQ1", "SQ2", "WL2", "YO4")) %>% group_by(parent.pop, elev_m, month) %>%
 # summarise(pck_mean=mean(pck), pck_sem=sem(pck))
#monthly_pck$parent.pop <- factor(monthly_pck$parent.pop, levels=c("DPR","WR","WL1", "SQ1", "SQ2", "WL2", "YO4"))

monthly_pck <- pop_elev_climate_recent %>% 
  filter(parent.pop==c("WL1", "SQ1", "SQ2", "WL2", "YO4")) %>% 
  group_by(parent.pop, elev_m, month) %>%
  summarise(pck_mean=mean(pck), pck_sem=sem(pck))
```

```
## `summarise()` has grouped output by 'parent.pop', 'elev_m'. You can override
## using the `.groups` argument.
```

```r
monthly_pck$parent.pop <- factor(monthly_pck$parent.pop, levels=c("WL1", "SQ1", "SQ2", "WL2", "YO4"))
monthly_pck
```

```
## # A tibble: 60 × 5
## # Groups:   parent.pop, elev_m [5]
##    parent.pop elev_m month pck_mean pck_sem
##    <fct>       <dbl> <dbl>    <dbl>   <dbl>
##  1 SQ1         1921.     1    128.     51.0
##  2 SQ1         1921.     2    211.    101. 
##  3 SQ1         1921.     3     96.3    66.8
##  4 SQ1         1921.     4     62.5    35.3
##  5 SQ1         1921.     5     23.2    23.2
##  6 SQ1         1921.     6      0       0  
##  7 SQ1         1921.     7      0       0  
##  8 SQ1         1921.     8      0       0  
##  9 SQ1         1921.     9      0       0  
## 10 SQ1         1921.    10      0       0  
## # ℹ 50 more rows
```

```r
monthly_pck %>% ggplot(aes(x=month, y=pck_mean, group=parent.pop, fill=parent.pop)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=pck_mean-pck_sem,ymax=pck_mean+pck_sem),width=.2, position=position_dodge(0.75)) +
  labs(title="Recent Climate", y="Avg SnwPck (mm)")  + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5,6,7,8,9,10, 11, 12)) +
  theme_classic() + 
  theme(text=element_text(size=30))
```

![](Climate_Prep_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
#ggsave("../output/Climate/Monthly_Avg_PCK_RecnetClim.png", width = 14, height = 6, units = "in")

#check even higher elevation
monthly_pck_high_elev <- pop_elev_climate_recent %>% 
  filter(elev_m>2200) %>%
  group_by(parent.pop, elev_m, month) %>%
  summarise(pck_mean=mean(pck), pck_sem=sem(pck))
```

```
## `summarise()` has grouped output by 'parent.pop', 'elev_m'. You can override
## using the `.groups` argument.
```

```r
monthly_pck_high_elev$parent.pop <- factor(monthly_pck_high_elev$parent.pop, levels=c("CP2", "CP3", "LV3", "SQ3", "YO7","YO8", "LV1", "LVTR1", "YO11"))

monthly_pck_high_elev %>% ggplot(aes(x=month, y=pck_mean, group=parent.pop, fill=parent.pop)) + 
  geom_col(width = 0.7,position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin=pck_mean-pck_sem,ymax=pck_mean+pck_sem),width=.2, position=position_dodge(0.75)) +
  labs(title="Recent Climate", y="Avg SnwPck (mm)")  + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5,6,7,8,9,10, 11, 12)) +
  theme_classic() + 
  theme(text=element_text(size=30))
```

![](Climate_Prep_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
#ggsave("../output/Climate/Monthly_Avg_PCK_HIGHELEV_RecnetClim.png", width = 14, height = 6, units = "in")
```

## CWD Across the Year


```r
names(pop_elev_climate_recent)
```

```
##  [1] "parent.pop"      "elevation.group" "elev_m"          "year"           
##  [5] "month"           "cwd"             "pck"             "ppt"            
##  [9] "tmn"             "tmx"
```

```r
monthly_cwd <- pop_elev_climate_recent %>% 
  group_by(parent.pop, elev_m, month) %>%
  summarise(cwd_mean=mean(cwd), pck_sem=sem(cwd))
```

```
## `summarise()` has grouped output by 'parent.pop', 'elev_m'. You can override
## using the `.groups` argument.
```

```r
monthly_cwd
```

```
## # A tibble: 276 × 5
## # Groups:   parent.pop, elev_m [23]
##    parent.pop elev_m month cwd_mean pck_sem
##    <chr>       <dbl> <dbl>    <dbl>   <dbl>
##  1 BH           511.     1     29.3   0.430
##  2 BH           511.     2     40.9   0.492
##  3 BH           511.     3     53.8   1.52 
##  4 BH           511.     4     59.0   2.92 
##  5 BH           511.     5     51.5   5.37 
##  6 BH           511.     6     87.1   5.45 
##  7 BH           511.     7    136.    4.35 
##  8 BH           511.     8    154.    3.31 
##  9 BH           511.     9    130.    1.03 
## 10 BH           511.    10     88.9   0.537
## # ℹ 266 more rows
```

```r
monthly_cwd %>% #low elev
  filter(elev_m<520) %>% 
  ggplot(aes(x=month, y=cwd_mean)) +
  geom_line() +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5,6,7,8,9,10, 11, 12)) +
  theme_classic() +
  facet_wrap(~parent.pop)
```

![](Climate_Prep_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
monthly_cwd %>% #mid elev
  filter(elev_m>520, elev_m<1940) %>% 
  ggplot(aes(x=month, y=cwd_mean)) +
  geom_line() +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5,6,7,8,9,10, 11, 12)) +
  theme_classic() +
  facet_wrap(~parent.pop)
```

![](Climate_Prep_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

```r
monthly_cwd %>% #high elev
  filter(elev_m>1940) %>% 
  ggplot(aes(x=month, y=cwd_mean)) +
  geom_line() +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5,6,7,8,9,10, 11, 12)) +
  theme_classic() +
  facet_wrap(~parent.pop)
```

![](Climate_Prep_files/figure-html/unnamed-chunk-10-3.png)<!-- -->

### Averages

#### Across last 30 years (all months included)


```r
pop_elev_climate_avgs <- pop_elev_climate %>% filter(year>1992) %>% group_by(parent.pop, elevation.group) %>% summarise_at(c("cwd", "pck", "ppt", "tmn", "tmx"), mean, na.rm = TRUE)
pop_elev_climate_avgs #30 year averages of all climate variables 
```

```
## # A tibble: 23 × 7
## # Groups:   parent.pop [23]
##    parent.pop elevation.group   cwd     pck   ppt    tmn   tmx
##    <chr>      <chr>           <dbl>   <dbl> <dbl>  <dbl> <dbl>
##  1 BH         Low              75.9   0      47.8  8.88   23.6
##  2 CC         Low              59.7   0      84.5 10.0    23.3
##  3 CP2        High             63.1 206.    105.   1.17   13.5
##  4 CP3        High             46.4 224.    100.   0.521  12.7
##  5 DPR        Mid              27.4   8.70  121.   7.86   20.3
##  6 FR         Mid              56.0  19.0    82.3  5.36   20.0
##  7 IH         Low              49.1   0.184  88.7  8.67   22.3
##  8 LV1        High             49.8 440.    147.  -1.38   11.2
##  9 LV3        High             57.6 426.    146.  -1.36   11.2
## 10 LVTR1      High             52.0 453.    152.  -1.58   11.2
## # ℹ 13 more rows
```

```r
pop_elev_climate_avgs$elevation.group <- factor(pop_elev_climate_avgs$elevation.group, levels=elev_order)
```

