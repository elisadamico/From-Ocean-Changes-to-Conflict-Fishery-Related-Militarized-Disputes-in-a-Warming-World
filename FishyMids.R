### Fishy MIDs Proejct ###
# Elisa D'Amico

#rm(list = ls())

library(caret)
library(countrycode)
library(doParallel)
library(dplyr)
library(gfwr)
library(glmnet)
library(MASS)
library(mgcv)
library(mice)
library(peacesciencer)
# see notation: https://svmiller.com/peacesciencer/ms.pdf
#ps_cite("peacesciencer")
library(plm)
library(pglm)
library(readxl)
library(sf)
library(stevemisc)
library(tictoc)
library(tidyverse)

setwd("C:/Users/elisa/OneDrive/Documents/Current Projects/WWF")

################################################################ 
######## Creating Data Shell & Adding Control Variables ######## 

# Create Dyad-Year Shell Dataset from 1950-present
dyad_years <- create_dyadyears()
data_shell <- subset(dyad_years, year >= 1950 & year <= 2023)

################################################################ 
######## Adding Fishy MIDs Data ######## 
# --------------- #
# -- 2011-2014 -- #

fm_20112014 <- read.csv("FishyMids20112014.csv")

fm_20112014 <- fm_20112014 %>%
  select(fishy_mid, ccode1, ccode2, name1, name2, year)

dyadic_20112014 <- left_join(data_shell, fm_20112014, by = c("ccode1", "ccode2", "year"))

# make all instances of fishy_mid 0 when missing for 2011-2014
dyadic_20112014 <- dyadic_20112014 %>%
  mutate(fishy_mid = ifelse(year %in% 2011:2014, ifelse(is.na(fishy_mid), 0, fishy_mid), fishy_mid))

# --------------- #
# -- 2010 & Prior -- #
fm_2010pre <- read.csv("FishyMIDs2010pre.csv")

fm_2010pre <- fm_2010pre %>%
  rename(ccode1 = ccode_1, ccode2 = ccode_2, name1 = countryname_1, name2 = countryname_2, fishy_mid = fishymidonset)

fm_2010pre <- fm_2010pre %>%
  select(fishy_mid, ccode1, ccode2, name1, name2, year)

dyadic_2014pre <- left_join(dyadic_20112014, fm_2010pre, by = c("ccode1", "ccode2", "year"))

# Coalescing 
dyadic_2014pre_subset <- dyadic_2014pre %>%
  select(fishy_mid.x, fishy_mid.y, ccode1, ccode2, name1.x, name1.y, name2.x, name2.y, year) %>%
  mutate(
    fishy_mid = coalesce(fishy_mid.x, fishy_mid.y),
    name1 = coalesce(name1.x, name1.y),
    name2 = coalesce(name2.x, name2.y)
  ) %>%
  select(-fishy_mid.x, -fishy_mid.y, -name1.x, -name1.y, -name2.x, -name2.y)

# make all instances of fishy_mid 0 when missing for 1950-2010
dyadic_2014pre_subset <- dyadic_2014pre_subset %>%
  mutate(fishy_mid = ifelse(year %in% 1950:2010, ifelse(is.na(fishy_mid), 0, fishy_mid), fishy_mid))


### -- Joining Control Variables -- ### 

# Drawing from Chen, F. R. (2021): https://www.journals.uchicago.edu/doi/pdf/10.1086/709149?casa_token=tLy4V-fYLXgAAAAA:vhZRvMcDNhJPvXfKtxsJmy8s8AhgT0EzMoow51jor6U-i0VYXQHp3Uykd_UHPn2Es4o1NC9azSqTng
# Add Distance
data_cv_chen <- add_capital_distance(dyadic_2014pre_subset)

# Add Contiguity
## Add CoW direct contiguity information to a data frame
data_cv_chen <- add_contiguity(data_cv_chen)

# Mutual Defensive Allies Count
##  Add Alliance Treaty Obligations and Provisions (ATOP) alliance 
#   data to a data frame
data_cv_chen <- add_atop_alliance(data_cv_chen) 

# Dyadic Defense Pact
## Add CoW alliance data to a data frame
data_cv_chen <- add_cow_alliance(data_cv_chen) 

# **Challenger** Capability Share
## Add Correlates of War National Military Capabilities data
data_cv_chen <- add_nmc(data_cv_chen)

# Target Major Power
## Add CoW major power information to a data frame
data_cv_chen <- add_cow_majors(data_cv_chen) 

# Add Challenger Democracy
## Add democracy information to a data frame
data_cv_chen <- add_democracy(data_cv_chen) 


# Additional Potential CVs (Not in Chen, F. R., 2021):

# Trade
## Add CoW trade data to a data frame
# download_extdata()
data_cvs <- add_cow_trade(data_cv_chen)

# Fractionalization
## Add fractionalization/polarization estimates from CREG to a data frame
data_cvs <- add_creg_fractionalization(data_cvs)
## See hief for historical ethnic fractionalizationd ata

# Foreign Policy Similarity
## Add dyadic foreign policy similarity measures to a data frame
data_cvs <- add_fpsim(data_cvs) 

# IGOs
## Add CoW intergovernmental organizations (IGO) data to a data frame
data_cvs <- add_igos(data_cvs) 

# GDP
## Add (surplus, gross) domestic product data to a data frame
data_cvs <- add_sdp_gdp(data_cvs) 

# Strategic Rivalry
## Add strategic rivalry information to a data frame
data_cvs <- add_strategic_rivalries(data_cvs)

data_cvs <- data_cvs %>%
  select(ccode1, name1, ccode2, name2, year, fishy_mid, everything())


# Display the count of fishy_mids
fishy_mid_counts <- table(data_cvs$fishy_mid)
print(fishy_mid_counts) 


##################################### 
######## January 2024 Update ######## 
##################################### 

### Adding in additional controls ### 

# -- World Bank Worldwide Governance Indicators -- #
## See: https://www.worldbank.org/en/publication/worldwide-governance-indicators
wb_wgi <- read.csv("WBWorldwideGovernance.csv")

# Adjust country codes to COW
str(wb_wgi)
wb_wgi$cow_code <- countrycode(
  sourcevar = wb_wgi$Country.Code,
  origin = "wb", 
  destination = "cown", 
  warn = TRUE
)

# Rename and subset variables
wb_wgi_renamed <- wb_wgi %>%
  select(
    year = Time,
    country1name_wb = Country.Name,
    rol_est = `Rule.of.Law..Estimate..RL.EST.`,
    rol_percentile = `Rule.of.Law..Percentile.Rank..RL.PER.RNK.`,
    corruption_est = `Control.of.Corruption..Estimate..CC.EST.`,
    corruption_percentile = `Control.of.Corruption..Percentile.Rank..CC.PER.RNK.`,
    goveffect_upper90 = `Government.Effectiveness..Percentile.Rank..Upper.Bound.of.90..Confidence.Interval..GE.PER.RNK.UPPER.`,
    goveffect_lower90 = `Government.Effectiveness..Percentile.Rank..Lower.Bound.of.90..Confidence.Interval..GE.PER.RNK.LOWER.`,
    voiceaccount_upper90 = `Voice.and.Accountability..Percentile.Rank..Upper.Bound.of.90..Confidence.Interval..VA.PER.RNK.UPPER.`,
    voiceaccount_lower90 = `Voice.and.Accountability..Percentile.Rank..Lower.Bound.of.90..Confidence.Interval..VA.PER.RNK.LOWER.`,
    polstability_est = `Political.Stability.and.Absence.of.Violence.Terrorism..Estimate..PV.EST.`,
    polstability_percentile = `Political.Stability.and.Absence.of.Violence.Terrorism..Percentile.Rank..PV.PER.RNK.`,
    regqual_lower90 = `Regulatory.Quality..Percentile.Rank..Lower.Bound.of.90..Confidence.Interval..RQ.PER.RNK.LOWER.`,
    regqual_upper90 = `Regulatory.Quality..Percentile.Rank..Upper.Bound.of.90..Confidence.Interval..RQ.PER.RNK.UPPER.`,
    ccode1 = cow_code
  )

# Merge onto master dataset (only merged for ccode1) /// Can do for both

data_cvs_jan <- merge(data_cvs, wb_wgi_renamed, by = c("ccode1", "year"), all.x = TRUE)


# -- Power At Sea: A Naval Dataset, 1865-2011, Crisher-Souva -- #
## See: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/24098
cs_navalcap <- read.csv("crisher-souva_navaldata_II.csv")

# Rename and subset variables
cs_navalcap <- cs_navalcap %>%
  rename(ccode1 = cowcode) %>%
  select(-country) %>%
  rename_at(vars(-ccode1, -year), ~paste0(., "_CS"))

# Merge onto master dataset (only merged for ccode1) /// Can do for both

data_cvs_jan <- merge(data_cvs_jan, cs_navalcap, by = c("ccode1", "year"), all.x = TRUE)


# -- ICOW Territorial Claims Data -- #
## See: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/E6PSGZ
icow_claims <- read.csv("ICOWdyadyr.csv")

# Rename and subset variables
icow_claims <- icow_claims %>%
  rename(ccode1 = chal, ccode2 = tgt) %>%
  select(-region, -dyadnum, -issue, -dyad, -icowsalc) %>%
  rename_at(vars(-year, -ccode1, -ccode2), ~paste0(., "_icow")) %>%
  filter(claim_icow >= 2000)

### I dropped all observations where claim code is less than 2000 because only 2000+ indicate maritime claims

# Collapse into dyad year format
## I took an average of the salience score, count of the claims, and sums of everything else
icow_claims <- icow_claims %>%
  group_by(ccode1, ccode2, year) %>%
  summarize(
    claim_icow = as.character(n()),
    icowsal_icow = mean(icowsal_icow),
    attbilat_icow = sum(attbilat_icow),
    att3non_icow = sum(att3non_icow),
    att3bind_icow = sum(att3bind_icow),
    midissyr_icow = sum(midissyr_icow),
    recno5_icow = sum(recno5_icow),
    recyes5_icow = sum(recyes5_icow),
    recmid5_icow = sum(recmid5_icow),
    recwar5_icow = sum(recwar5_icow),
    recno15_icow = sum(recno15_icow),
    recyes15_icow = sum(recyes15_icow),
    recmid15_icow = sum(recmid15_icow),
    recwar15_icow = sum(recwar15_icow)
  )

# Merge onto master dataset on dyad year

data_cvs_jan <- merge(data_cvs_jan, icow_claims, by = c("ccode1", "ccode2", "year"), all.x = TRUE)


## EXPORT DATA ##
#write.csv(data_cvs_jan, "dyadic_data.csv", row.names = FALSE)



#########################################
######## MARCH-APRIL 2024 Update ######## 
#########################################

#   Play with fishy MIDs outcome, what kind of model maximizes in- 
#   and out-of-sample predictive power

#   FAO Fisheries and Aquaculture data as a predictor 
#   (https://www.fao.org/fishery/en/collection/aquaculture)

aquaculture_quantity <- read.csv("Aquaculture_Quantity.csv")
aquaculture_value <- read.csv("Aquaculture_Value.csv")

# Convert from UN codes to COW codes
aquaculture_quantity$ccode1 <- countrycode(aquaculture_quantity$COUNTRY.UN_CODE, "un", "cown")
aquaculture_value$ccode1 <- countrycode(aquaculture_value$COUNTRY.UN_CODE, "un", "cown")
aquaculture_quantity$ccode2 <- countrycode(aquaculture_quantity$COUNTRY.UN_CODE, "un", "cown")
aquaculture_value$ccode2 <- countrycode(aquaculture_value$COUNTRY.UN_CODE, "un", "cown")

# Country codes (not matched) remove
codes_to_remove <- c(16, 92, 136, 158, 175, 184, 200, 234, 238, 254, 258, 275, 312, 316, 344, 
                     474, 530, 531, 533, 535, 540, 580, 630, 638, 666, 688, 736, 796, 810, 
                     830, 836, 850, 890, 891)

aquaculture_quantity <- aquaculture_quantity %>%
  filter(!COUNTRY.UN_CODE %in% codes_to_remove)

aquaculture_value <- aquaculture_value %>%
  filter(!COUNTRY.UN_CODE %in% codes_to_remove)

# Subset for cleaning and merging
collapsed_aquaculture_value <- aquaculture_value %>%
  group_by(ccode1, ccode2, PERIOD) %>%
  summarise(VALUE_sum = sum(VALUE)) %>%
  select(ccode1, ccode2, year = PERIOD, fishvalue = VALUE_sum)

collapsed_aquaculture_quantity <- aquaculture_quantity %>%
  group_by(ccode1, ccode2, PERIOD) %>%
  summarise(VALUE_sum = sum(VALUE)) %>%
  select(ccode1, ccode2, year = PERIOD, fishnum = VALUE_sum)

## For collapsed_aquaculture_quantity
collapsed_aquaculture_quantity <- collapsed_aquaculture_quantity %>%
  mutate(fishnum1 = fishnum,
         fishnum2 = fishnum)

## For collapsed_aquaculture_value
collapsed_aquaculture_value <- collapsed_aquaculture_value %>%
  mutate(fishvalue1 = fishvalue,
         fishvalue2 = fishvalue)

# Ensure ccode1 and ccode2 are characters
data_cvs_jan$ccode1 <- as.character(data_cvs_jan$ccode1)
data_cvs_jan$ccode2 <- as.character(data_cvs_jan$ccode2)
data_cvs_jan$year <- as.character(data_cvs_jan$year)

collapsed_aquaculture_value$ccode1 <- as.character(collapsed_aquaculture_value$ccode1)
collapsed_aquaculture_value$ccode2 <- as.character(collapsed_aquaculture_value$ccode2)
collapsed_aquaculture_value$year <- as.character(collapsed_aquaculture_value$year)

collapsed_aquaculture_quantity$ccode1 <- as.character(collapsed_aquaculture_quantity$ccode1)
collapsed_aquaculture_quantity$ccode2 <- as.character(collapsed_aquaculture_quantity$ccode2)
collapsed_aquaculture_quantity$year <- as.character(collapsed_aquaculture_quantity$year)

# Merge collapsed_aquaculture_value for ccode1 and year
data_cvs_jan_ag <- inner_join(data_cvs_jan, collapsed_aquaculture_value %>% select(ccode1, year, fishvalue1), by = c("ccode1", "year"))
data_cvs_jan_ag <- select(data_cvs_jan_ag, -ccode2.y)
data_cvs_jan_ag <- rename(data_cvs_jan_ag, ccode2 = ccode2.x)

# Merge collapsed_aquaculture_quantity for ccode1 and year
data_cvs_jan_ag <- inner_join(data_cvs_jan_ag, collapsed_aquaculture_quantity %>% select(ccode1, year, fishnum1), by = c("ccode1", "year"))
data_cvs_jan_ag <- select(data_cvs_jan_ag, -ccode2.y)
data_cvs_jan_ag <- rename(data_cvs_jan_ag, ccode2 = ccode2.x)

# Merge collapsed_aquaculture_value for ccode2 and year
data_cvs_jan_ag <- inner_join(data_cvs_jan_ag, collapsed_aquaculture_value %>% select(ccode2, year, fishvalue2), by = c("ccode2", "year"))
data_cvs_jan_ag <- select(data_cvs_jan_ag, -ccode1.y)
data_cvs_jan_ag <- rename(data_cvs_jan_ag, ccode1 = ccode1.x)

# Merge collapsed_aquaculture_quantity for ccode2 and year
data_cvs_jan_ag <- inner_join(data_cvs_jan_ag, collapsed_aquaculture_quantity %>% select(ccode2, year, fishnum2), by = c("ccode2", "year"))
data_cvs_jan_ag <- select(data_cvs_jan_ag, -ccode1.y)
data_cvs_jan_ag <- rename(data_cvs_jan_ag, ccode1 = ccode1.x)


data_cvs_jan_ag <- data_cvs_jan_ag %>%
  group_by(ccode1) %>%
  arrange(year) %>%
  mutate(percent_change_fishnum1 = (fishnum1 - lag(fishnum1)) / (lag(fishnum1) + 0.000000000000000001) * 100,
         percent_change_fishnum2 = (fishnum2 - lag(fishnum2)) / (lag(fishnum2) + 0.000000000000000001) * 100,
         percent_change_fishvalue1 = (fishvalue1 - lag(fishvalue1)) / (lag(fishvalue1) + 0.000000000000000001) * 100,
         percent_change_fishvalue2 = (fishvalue2 - lag(fishvalue2)) / (lag(fishvalue2) + 0.000000000000000001) * 100) %>%
  ungroup()


# PREPARE PANEL DATA #

# Create a unique dyad ID number
data_cvs_jan_ag$ccode1 <- as.character(data_cvs_jan_ag$ccode1)
data_cvs_jan_ag$ccode2 <- as.character(data_cvs_jan_ag$ccode2)
data_cvs_jan_ag$dyad_id <- paste(data_cvs_jan_ag$ccode1, data_cvs_jan_ag$ccode2, data_cvs_jan_ag$year, sep = "_")
data_cvs_jan_ag$dyad_id <- as.factor(data_cvs_jan_ag$dyad_id)

# Set as panel data
data_cvs_jan_ag <- pdata.frame(data_cvs_jan_ag, index = c("dyad_id", "year"))




###########################
###### MODEL TESTING ###### 
###########################

## REPLICATE CHEN + FISH DATA ##

# Subset data to complete observations for Chen variable replication
subset_chen <- data_cvs_jan_ag[complete.cases(data_cvs_jan_ag[, c("percent_change_fishnum1","fishvalue1", "fishvalue2", "fishnum2", "fishnum1", "fishy_mid", "cinc1", "atop_defense", "taub", "cowmaj2", "polity21", "polity22", "conttype", "capdist")]), ]
subset_chen <- pdata.frame(subset_chen, index = c("dyad_id", "year"))

# Replace NaN and Inf values with 0 
subset_chen$percent_change_fishnum1 <- replace(subset_chen$percent_change_fishnum1, is.infinite(subset_chen$percent_change_fishnum1), 0)
subset_chen$percent_change_fishvalue1 <- replace(subset_chen$percent_change_fishvalue1, is.infinite(subset_chen$percent_change_fishvalue1), 0)
subset_chen$percent_change_fishnum2[is.nan(subset_chen$percent_change_fishnum2) | is.infinite(subset_chen$percent_change_fishnum2)] <- 0
subset_chen$percent_change_fishvalue1[is.nan(subset_chen$percent_change_fishvalue1) | is.infinite(subset_chen$percent_change_fishvalue1)] <- 0
subset_chen$percent_change_fishvalue2[is.nan(subset_chen$percent_change_fishvalue2) | is.infinite(subset_chen$percent_change_fishvalue2)] <- 0

subset_chen <- subset(subset_chen, year != 1984)


# Fit the logit model
model_samplechen1 <- glm(fishy_mid ~ percent_change_fishnum1 + percent_change_fishvalue1+ cinc1 + atop_defense + taub + cowmaj2 + polity21 + polity22 + conttype + capdist +factor(year), 
                        data = subset_chen, family = binomial(link = "logit"))
summary(model_samplechen1)



## STEPWISE ##

# Set up parallel processing
num_cores <- parallel::detectCores()
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Perform stepwise selection using AIC in parallel

formula <- fishy_mid ~ capdist + conttype + atop_defense + atop_offense + atop_neutral + atop_nonagg + 
  atop_consul + cow_defense + cow_neutral + cow_nonagg + cow_entente + irst1 + tpop1 + upop1 + 
  cinc1 + irst2 + tpop2 + upop2 + cinc2 + cowmaj1 + cowmaj2 + polity21 + xm_qudsest1 + 
  polity22 + xm_qudsest2 + taub + srsvas + srswvas + srsvaa + srswvaa + kappava + piva + srsba + 
  srswba + kappaba + piba + dyadigos + ongoingrivalry + fishvalue1 + fishnum1 + fishvalue2 + 
  fishnum2 + percent_change_fishnum1 + percent_change_fishvalue1

stepwise_model_2 <- step(glm(formula, data = subset_chen, family = "binomial"), direction = "both")
stopCluster(cl)
summary(stepwise_model_2)

# -- Check for multi-colinearity among predictive variables-- #
predictor_vars <- subset_chen[, c("capdist", "conttype", "atop_nonagg", "atop_consul", 
                                  "cow_defense", "cow_entente", "irst1", "upop1", 
                                  "cinc1", "irst2", "tpop2", "cinc2", "polity21", 
                                  "xm_qudsest1", "polity22", "xm_qudsest2", 
                                  "srsvas", "srswvas", "srswba", "dyadigos", 
                                  "fishvalue2", "fishnum2")]

correlation_matrix <- cor(predictor_vars)
threshold <- 0.7
high_correlations <- which(abs(correlation_matrix) > threshold & correlation_matrix != 1, arr.ind = TRUE)

# Print the pairs of variables with correlations above the threshold
for (i in 1:nrow(high_correlations)) {
  row_index <- high_correlations[i, 1]
  col_index <- high_correlations[i, 2]
  variable1 <- colnames(predictor_vars)[row_index]
  variable2 <- colnames(predictor_vars)[col_index]
  correlation <- correlation_matrix[row_index, col_index]
  cat("High correlation (>", threshold, "):", variable1, "and", variable2, "(Correlation:", correlation, ")\n")
}

### High correlation (> 0.7 ): cow_defense and atop_consul (Correlation: 0.7241357 )
### High correlation (> 0.7 ): cow_entente and atop_consul (Correlation: 0.7902409 )
### High correlation (> 0.7 ): atop_consul and cow_defense (Correlation: 0.7241357 )
### High correlation (> 0.7 ): cow_entente and cow_defense (Correlation: 0.8477965 )
### High correlation (> 0.7 ): atop_consul and cow_entente (Correlation: 0.7902409 )
### High correlation (> 0.7 ): cow_defense and cow_entente (Correlation: 0.8477965 )
### High correlation (> 0.7 ): upop1 and irst1 (Correlation: 0.754624 )
### High correlation (> 0.7 ): cinc1 and irst1 (Correlation: 0.7835802 )
### High correlation (> 0.7 ): irst1 and upop1 (Correlation: 0.754624 )
### High correlation (> 0.7 ): cinc1 and upop1 (Correlation: 0.8722836 )
### High correlation (> 0.7 ): irst1 and cinc1 (Correlation: 0.7835802 )
### High correlation (> 0.7 ): upop1 and cinc1 (Correlation: 0.8722836 )
### High correlation (> 0.7 ): cinc2 and irst2 (Correlation: 0.7857587 )
### High correlation (> 0.7 ): fishvalue2 and irst2 (Correlation: 0.9062318 )
### High correlation (> 0.7 ): fishnum2 and irst2 (Correlation: 0.8481654 )
### High correlation (> 0.7 ): cinc2 and tpop2 (Correlation: 0.8100425 )
### High correlation (> 0.7 ): fishnum2 and tpop2 (Correlation: 0.7279188 )
### High correlation (> 0.7 ): irst2 and cinc2 (Correlation: 0.7857587 )
### High correlation (> 0.7 ): tpop2 and cinc2 (Correlation: 0.8100425 )
### High correlation (> 0.7 ): xm_qudsest1 and polity21 (Correlation: 0.934788 )
### High correlation (> 0.7 ): polity21 and xm_qudsest1 (Correlation: 0.934788 )
### High correlation (> 0.7 ): xm_qudsest2 and polity22 (Correlation: 0.9370776 )
### High correlation (> 0.7 ): polity22 and xm_qudsest2 (Correlation: 0.9370776 )
### High correlation (> 0.7 ): srswba and srswvas (Correlation: 0.9580635 )
### High correlation (> 0.7 ): srswvas and srswba (Correlation: 0.9580635 )
### High correlation (> 0.7 ): irst2 and fishvalue2 (Correlation: 0.9062318 )
### High correlation (> 0.7 ): fishnum2 and fishvalue2 (Correlation: 0.9396106 )
### High correlation (> 0.7 ): irst2 and fishnum2 (Correlation: 0.8481654 )
### High correlation (> 0.7 ): tpop2 and fishnum2 (Correlation: 0.7279188 )
### High correlation (> 0.7 ): fishvalue2 and fishnum2 (Correlation: 0.9396106 )


# Test logits with randomly generated non-colinear predictors to find good fit

formula <- "fishy_mid ~ conttype + atop_nonagg + cow_defense + irst1 + cinc1 + tpop2 + polity21 + srsvas + dyadigos + percent_change_fishvalue1"
logit_model <- glm(formula, data = subset_chen, family = "binomial")
summary(logit_model)

formula <- "fishy_mid ~ conttype + atop_nonagg + irst1 + tpop2 + polity21 + srswvas  + percent_change_fishvalue1"
logit_model_2 <- glm(formula, data = subset_chen, family = "binomial")
summary(logit_model_2)

formula_3 <- "fishy_mid ~ conttype + cow_defense + irst1 + tpop2 + polity21 + srsvas + percent_change_fishvalue1 + dyadigos"
logit_model_3 <- glm(formula_3, data = subset_chen, family = "binomial")
summary(logit_model_3)

formula_4 <- "fishy_mid ~ atop_nonagg + cow_entente + irst2 + tpop2 + polity22 + srswvas + percent_change_fishvalue1 + ongoingrivalry"
logit_model_4 <- glm(formula_4, data = subset_chen, family = "binomial")
summary(logit_model_4)

formula_5 <- "fishy_mid ~ conttype + cow_defense + irst1 + cinc2 + tpop2 + xm_qudsest1 + percent_change_fishvalue1 "
logit_model_5 <- glm(formula_5, data = subset_chen, family = "binomial")
summary(logit_model_5)

formula_6 <- "fishy_mid ~ atop_nonagg + cow_entente + cinc1 + irst2 + polity22 + srswvas + percent_change_fishvalue1 + fishnum2"
logit_model_6 <- glm(formula_6, data = subset_chen, family = "binomial")
summary(logit_model_6)

formula_7 <- "fishy_mid ~ conttype + cow_defense + irst2 + tpop2 + polity21 + srswvas  + percent_change_fishvalue1"
logit_model_7 <- glm(formula_7, data = subset_chen, family = "binomial")
summary(logit_model_7)

formula_8 <- "fishy_mid ~ atop_nonagg + cow_entente + irst1 + cinc2 + polity22 + srsvas + fishvalue2 + ongoingrivalry"
logit_model_8 <- glm(formula_8, data = subset_chen, family = "binomial")
summary(logit_model_8)

# Extract AIC values for all models
AIC_values <- c(AIC(logit_model), AIC(logit_model_2), AIC(logit_model_3), AIC(logit_model_4), AIC(logit_model_5),
                AIC(logit_model_6), AIC(logit_model_7), AIC(logit_model_8))

models <- c("logit_model", "logit_model_2", "logit_model_3", "logit_model_4", "logit_model_5",
            "logit_model_6", "logit_model_7", "logit_model_8")

AIC_table <- data.frame(Model = models, AIC = AIC_values)
print(AIC_table)


################
# Best Logit Fit
################

# Create logged variables
epsilon <- 1e-10  # small positive value
subset_chen$log_fishvalue1 <- log(subset_chen$fishvalue1 + epsilon)
subset_chen$log_fishvalue2 <- log(subset_chen$fishvalue2 + epsilon)
subset_chen$log_fishnum1 <- log(subset_chen$fishnum1 + epsilon)
subset_chen$log_fishnum2 <- log(subset_chen$fishnum2 + epsilon)


formula <- "fishy_mid ~ factor(conttype) + atop_nonagg + cinc1 + tpop2 + polity21 + taub + cowmaj2 + polity21 + polity22 +srsvas + dyadigos + fishnum1 +fishnum2"
logit_model_fishnum <- glm(formula, data = subset_chen, family = "binomial")
summary(logit_model_fishnum)

formula <- "fishy_mid ~ factor(conttype) + atop_nonagg + cinc1 + tpop2 + polity21 + taub + cowmaj2 + polity21 + polity22 +srsvas + dyadigos + log_fishnum1 +log_fishnum2"
logit_model_fishnum_log <- glm(formula, data = subset_chen, family = "binomial")
summary(logit_model_fishnum_log)


formula <- "fishy_mid ~ factor(conttype) + atop_nonagg + cinc1 + tpop2 + polity21 + taub + cowmaj2 + polity21 + polity22 +srsvas + dyadigos + fishvalue1 + fishvalue2 "
logit_model_fishvalue <- glm(formula, data = subset_chen, family = "binomial")
summary(logit_model_fishvalue)

formula <- "fishy_mid ~ factor(conttype) + atop_nonagg + cinc1 + tpop2 + polity21 + taub + cowmaj2 + polity21 + polity22 +srsvas + dyadigos + log_fishvalue1 + log_fishvalue2 "
logit_model_fishvalue_log <- glm(formula, data = subset_chen, family = "binomial")
summary(logit_model_fishvalue_log)


# Fit the logit model
model_samplechen1 <- glm(fishy_mid ~ percent_change_fishnum1 + cinc1 + atop_defense + taub + cowmaj2 + polity21 + polity22 + conttype + capdist +factor(year), 
                         data = subset_chen, family = binomial(link = "logit"))
summary(model_samplechen1)



## Check for potential nonlinear relationships using GAM ##

# Compute correlation between fishy_mid and each variable individually
cor_conttype <- cor(subset_chen$fishy_mid, subset_chen$conttype)
cor_atop_nonagg <- cor(subset_chen$fishy_mid, subset_chen$atop_nonagg)
cor_cow_defense <- cor(subset_chen$fishy_mid, subset_chen$cow_defense)
cor_irst1 <- cor(subset_chen$fishy_mid, subset_chen$irst1)
cor_cinc1 <- cor(subset_chen$fishy_mid, subset_chen$cinc1)
cor_tpop2 <- cor(subset_chen$fishy_mid, subset_chen$tpop2)
cor_polity21 <- cor(subset_chen$fishy_mid, subset_chen$polity21)
cor_srsvas <- cor(subset_chen$fishy_mid, subset_chen$srsvas)
cor_dyadigos <- cor(subset_chen$fishy_mid, subset_chen$dyadigos)
cor_percent_change_fishvalue1 <- cor(subset_chen$fishy_mid, subset_chen$percent_change_fishvalue1)

# Print correlation coefficients
print("Correlation with fishy_mid:")
print(paste("conttype:", cor_conttype))
print(paste("atop_nonagg:", cor_atop_nonagg))
print(paste("cow_defense:", cor_cow_defense))
print(paste("irst1:", cor_irst1))
print(paste("cinc1:", cor_cinc1))
print(paste("tpop2:", cor_tpop2))
print(paste("polity21:", cor_polity21))
print(paste("srsvas:", cor_srsvas))
print(paste("dyadigos:", cor_dyadigos))
print(paste("percent_change_fishvalue1:", cor_percent_change_fishvalue1))

# potential nonlinear: tpop2, polity21, srsvas

# TEST GAM

gam_model_all <- gam(fishy_mid ~ factor(year)+s(tpop2, k = 2) + s(polity21, k = 2) + s(srsvas, k = 2) +factor(conttype) + atop_nonagg + cinc1  + polity21 + taub + cowmaj2  + polity22 + dyadigos + log_fishvalue1 + log_fishvalue2
, 
                        data = subset_chen, family = binomial)
summary(gam_model_all)














######################################
### --- DEAL WITH MISSINGNESS --- ###
######################################

## SUBSETTING BASED ON missing dyad-years (rows)
data_pre2015_clean <- data_pre2015 %>%
  filter(rowSums(is.na(.)) / ncol(.) <= 0.2)

#SUBSETTING BASED ON missing variables (columns)
data_pre2015_clean <- data_pre2015_clean %>%
  select(-which(colMeans(is.na(.)) > 0.2))

# Extract numeric columns
numeric_columns <- sapply(data_pre2015_clean, is.numeric)
lower_bounds <- apply(data_pre2015_clean[, numeric_columns], 2, min, na.rm = TRUE)
upper_bounds <- apply(data_pre2015_clean[, numeric_columns], 2, max, na.rm = TRUE)
data_for_imputation <- data_pre2015_clean %>%
  select(-ccode1, -ccode2)

# IMPUTE DATA
imputed_data <- mice(data_for_imputation, method = "rf", m = 5, maxit = 10, 
                     bounds = cbind(lower_bounds, upper_bounds))

# Choose an iteration 
chosen_iteration <- complete(imputed_data, action = 3)
chosen_iteration_df <- as.data.frame(chosen_iteration)
imputed_data_with_ids <- cbind(data_pre2015_clean[, c("ccode1", "ccode2")], chosen_iteration_df)


## EXPORT DATA ##
#write.csv(imputed_data_with_ids, "imputed_data.csv", row.names = FALSE)


######################
######################
######################
### --- MODELS --- ###

#Import df
#impdata_mar <- read.csv("imputed_data.csv")
#impdata_mar <- read_excel("~/Current Projects/WWF/imputed_data.xlsx")

impdata_mar <- imputed_data
impdata_mar <- complete(impdata_mar)

# PREPARE PANEL DATA #
impdata_mar <- transform(impdata_mar, 
                         ccode1 = as.factor(ccode1), 
                         ccode2 = as.factor(ccode2), 
                         year = as.factor(year))

# Create a unique dyad ID number
impdata_mar$ccode1 <- as.character(impdata_mar$ccode1)
impdata_mar$ccode2 <- as.character(impdata_mar$ccode2)
impdata_mar$dyad_id <- paste(impdata_mar$ccode1, impdata_mar$ccode2, impdata_mar$year, sep = "_")
impdata_mar$dyad_id <- as.factor(impdata_mar$dyad_id)

# Set as panel data
pdata <- pdata.frame(impdata_mar, index = c("dyad_id", "year"))
print(colnames(pdata))

################################################

# -- SUBSET DATA RANDOMLY FOR TESTING -- #

# Set seed & prepare for random subsetting
set.seed(123)
num_subsets <- 14
subset_size <- ceiling(nrow(pdata) / num_subsets)
indices <- sample(1:nrow(pdata))

# Create subsets
random_subsets <- lapply(1:num_subsets, function(i) {
  start_index <- (i - 1) * subset_size + 1
  end_index <- min(i * subset_size, nrow(pdata))
  subset_data <- pdata[indices[start_index:end_index], ]
  return(subset_data)
})

# Specify the formula
formula <- fishy_mid ~ cinc1 + atop_defense + taub + cowmaj2 + polity21 + polity22 + conttype + capdist

# Fit the logit model with fixed effects (Sample)
model <- pglm(formula, data = random_subsets[[1]], family = binomial(link = "logit"), method = "bfgs")
# Display the summary of the model
summary(model)


model <- pglm(formula, data = pdata, family = binomial(link = "logit"), method = "bfgs")
model_fullimpute <-model