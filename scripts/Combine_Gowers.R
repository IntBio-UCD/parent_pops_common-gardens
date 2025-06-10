# Combining all gower's distances into one data frame 

## LIBRARIES
library(tidyverse)

## UCD - FULL WATER YEAR
UCD_wtr_year_all_vars <- read_csv("output/Climate/full_year_GowersEnvtalDist_UCD_wtr_year.csv") %>% 
  select(parent.pop:Long, Wtr_Year_GD=Gowers_Dist, TimePd)
UCD_wtr_year_FLINT <- read_csv("output/Climate/full_year_GowersEnvtalDist_UCDFlint_wtr_year.csv") %>% 
  select(parent.pop:Long, Wtr_Year_FLINT_GD=Gowers_Dist, TimePd)
UCD_wtr_year_BIOCLIM <- read_csv("output/Climate/full_year_GowersEnvtalDist_UCDbioclim_wtr_year.csv") %>% 
  select(parent.pop:Long, Wtr_Year_BIOCLIM_GD=Gowers_Dist, TimePd)

UCD_wtr_year_others <- full_join(UCD_wtr_year_FLINT, UCD_wtr_year_BIOCLIM)

UCD_wtr_year_all <- full_join(UCD_wtr_year_all_vars, UCD_wtr_year_others)
names(UCD_wtr_year_all)

## UCD - GROWTH SEASON 
UCD_grwssn_all_vars <- read_csv("output/Climate/growthseason_GowersEnvtalDist_UCD.csv") %>% 
  select(parent.pop:Long, GrwSsn_GD=Gowers_Dist, TimePd)
UCD_grwssn_FLINT <- read_csv("output/Climate/growthseason_GowersEnvtalDist_UCDFlint.csv")  %>% 
  select(parent.pop:Long, GrwSsn_FLINT_GD=Gowers_Dist, TimePd)
UCD_grwssn_BIOCLIM <- read_csv("output/Climate/growthseason_GowersEnvtalDist_UCDbioclim.csv") %>% 
  select(parent.pop:Long, GrwSsn_BIOCLIM_GD=Gowers_Dist, TimePd)

UCD_grwssn_others <- full_join(UCD_grwssn_FLINT, UCD_grwssn_BIOCLIM)

UCD_grwssn_all <- full_join(UCD_grwssn_all_vars, UCD_grwssn_others)
names(UCD_grwssn_all)

## ALL UCD 
gowers_UCD <- full_join(UCD_grwssn_all, UCD_wtr_year_all) %>% 
  select(parent.pop:Long, TimePd, GrwSsn_GD:Wtr_Year_BIOCLIM_GD)
names(gowers_UCD)
write_csv(gowers_UCD, "output/Climate/Gowers_UCD.csv")

## WL2 - FULL WATER YEAR
WL2_wtr_year_all_vars <- read_csv("output/Climate/full_year_GowersEnvtalDist_WL2_wtr_year.csv") %>% 
  select(parent.pop:Long, Wtr_Year_GD=Gowers_Dist, TimePd)
WL2_wtr_year_FLINT <- read_csv("output/Climate/full_year_GowersEnvtalDist_WL2Flint_wtr_year.csv") %>% 
  select(parent.pop:Long, Wtr_Year_FLINT_GD=Gowers_Dist, TimePd)
WL2_wtr_year_BIOCLIM <- read_csv("output/Climate/full_year_GowersEnvtalDist_WL2bioclim_wtr_year.csv") %>% 
  select(parent.pop:Long, Wtr_Year_BIOCLIM_GD=Gowers_Dist, TimePd)

WL2_wtr_year_others <- full_join(WL2_wtr_year_FLINT, WL2_wtr_year_BIOCLIM)

WL2_wtr_year_all <- full_join(WL2_wtr_year_all_vars, WL2_wtr_year_others)
names(WL2_wtr_year_all)

## WL2 - GROWTH SEASON
WL2_grwssn_all_vars <- read_csv("output/Climate/growthseason_GowersEnvtalDist_WL2.csv") %>% 
  select(parent.pop:Long, GrwSsn_GD=Gowers_Dist, TimePd)
WL2_grwssn_FLINT <- read_csv("output/Climate/growthseason_GowersEnvtalDist_WL2Flint.csv")  %>% 
  select(parent.pop:Long, GrwSsn_FLINT_GD=Gowers_Dist, TimePd)
WL2_grwssn_BIOCLIM <- read_csv("output/Climate/growthseason_GowersEnvtalDist_WL2bioclim.csv") %>% 
  select(parent.pop:Long, GrwSsn_BIOCLIM_GD=Gowers_Dist, TimePd)

WL2_grwssn_others <- full_join(WL2_grwssn_FLINT, WL2_grwssn_BIOCLIM)

WL2_grwssn_all <- full_join(WL2_grwssn_all_vars, WL2_grwssn_others)
names(WL2_grwssn_all)

## ALL WL2
gowers_WL2 <- full_join(WL2_grwssn_all, WL2_wtr_year_all) %>% 
  select(parent.pop:Long, TimePd, GrwSsn_GD:Wtr_Year_BIOCLIM_GD)
names(gowers_WL2)
write_csv(gowers_WL2, "output/Climate/Gowers_WL2.csv")

#UCD + WL2 - MAIN GOWERS
gowers_UCD_prep <- gowers_UCD %>% select(parent.pop:TimePd, GrwSsn_GD_UCD=GrwSsn_GD, Wtr_Year_GD_UCD=Wtr_Year_GD)
gowers_WL2_prep <- gowers_WL2 %>% select(parent.pop:TimePd, GrwSsn_GD_WL2=GrwSsn_GD, Wtr_Year_GD_WL2=Wtr_Year_GD)
gowers_UCD_WL2 <- full_join(gowers_UCD_prep, gowers_WL2_prep) 
names(gowers_UCD_WL2)
write_csv(gowers_UCD_WL2, "output/Climate/Gowers_UCD_WL2_ALL_VARS.csv")

#WL2 - 2024
WL2_grwssn_all_vars_2024 <- read_csv("output/Climate/growthseason_GowersEnvtalDist_WL2_2024.csv") %>% 
  select(parent.pop:Long, GrwSsn_GD=Gowers_Dist, TimePd)

WL2_wtr_year_all_vars_2024 <- read_csv("output/Climate/full_year_GowersEnvtalDist_WL2_wtr_year_2024.csv") %>% 
  select(parent.pop:Long, Wtr_Year_GD=Gowers_Dist, TimePd)

gowers_WL2_2024 <- full_join(WL2_grwssn_all_vars_2024, WL2_wtr_year_all_vars_2024) %>% 
  select(parent.pop:Long, TimePd, GrwSsn_GD, Wtr_Year_GD)
names(gowers_WL2_2024)
write_csv(gowers_WL2_2024, "output/Climate/Gowers_WL2_2024.csv")

#WL2 - Garden 2024 - Home Climates up to 2023
WL2_grwssn_all_vars_2324 <- read_csv("output/Climate/growthseason_GowersEnvtalDist_WL2_2324.csv") %>% 
  select(parent.pop:Long, GrwSsn_GD=Gowers_Dist, TimePd)

WL2_wtr_year_all_vars_2324 <- read_csv("output/Climate/full_year_GowersEnvtalDist_WL2_wtr_year_2324.csv") %>% 
  select(parent.pop:Long, Wtr_Year_GD=Gowers_Dist, TimePd)

gowers_WL2_2324 <- full_join(WL2_grwssn_all_vars_2324, WL2_wtr_year_all_vars_2324) %>% 
  select(parent.pop:Long, TimePd, GrwSsn_GD, Wtr_Year_GD)
names(gowers_WL2_2324)
write_csv(gowers_WL2_2324, "output/Climate/Gowers_WL2_2324.csv")
