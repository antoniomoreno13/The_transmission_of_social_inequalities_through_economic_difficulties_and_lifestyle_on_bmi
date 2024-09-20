
library("haven")
library("plyr")
library("dplyr")
library("car")
library("forcats")
library("reshape2")
library("tidyverse")

#### Read Health on Equal Terms data and defining categories #### 
HET <- read_dta("/Users/Desktop/...", encoding = "iso-8859-1")

HET <- HET %>% select(sex, edu3, InvAr, EK002, SO009, HA026, HA027, HA074, LV048, LV049, LV004, LV005, LV018, LV019, LV020, LV053, argang, Alder)
HET$sex <- as.factor(HET$sex)
HET$edu3 <- as.factor(HET$edu3)

# Gender
HET$sex_cat[HET$sex == "1"] <- "Men"
HET$sex_cat[HET$sex == "2"] <- "Women"
HET$sex_cat <- as.factor(HET$sex_cat)

# Education
HET$edu3_cat[HET$edu3 == "1"] <- "High"
HET$edu3_cat[HET$edu3 == "2"] <- "Medium"
HET$edu3_cat[HET$edu3 == "3"] <- "Low"
HET$edu3_cat <- as.factor(HET$edu3_cat)

# Migration
HET$migration[HET$InvAr >= 0] <- "Immigrant"
HET$migration[which(is.na(HET$InvAr))] <- "Native"
HET$migration <- as.factor(HET$migration)

# Strata combined
HET$Strata[HET$sex_cat == "Men" & HET$edu3_cat == "High" & HET$migration == "Immigrant"] <- "Men_High_Immigrant"
HET$Strata[HET$sex_cat == "Men" & HET$edu3_cat == "Medium" & HET$migration == "Immigrant"] <- "Men_Medium_Immigrant"
HET$Strata[HET$sex_cat == "Men" & HET$edu3_cat == "Low" & HET$migration == "Immigrant"] <- "Men_Low_Immigrant"
HET$Strata[HET$sex_cat == "Men" & HET$edu3_cat == "High" & HET$migration == "Native"] <- "Men_High_Native"
HET$Strata[HET$sex_cat == "Men" & HET$edu3_cat == "Medium" & HET$migration == "Native"] <- "Men_Medium_Native"
HET$Strata[HET$sex_cat == "Men" & HET$edu3_cat == "Low" & HET$migration == "Native"] <- "Men_Low_Native"
HET$Strata[HET$sex_cat == "Women" & HET$edu3_cat == "High" & HET$migration == "Immigrant"] <- "Women_High_Immigrant"
HET$Strata[HET$sex_cat == "Women" & HET$edu3_cat == "Medium" & HET$migration == "Immigrant"] <- "Women_Medium_Immigrant"
HET$Strata[HET$sex_cat == "Women" & HET$edu3_cat == "Low" & HET$migration == "Immigrant"] <- "Women_Low_Immigrant"
HET$Strata[HET$sex_cat == "Women" & HET$edu3_cat == "High" & HET$migration == "Native"] <- "Women_High_Native"
HET$Strata[HET$sex_cat == "Women" & HET$edu3_cat == "Medium" & HET$migration == "Native"] <- "Women_Medium_Native"
HET$Strata[HET$sex_cat == "Women" & HET$edu3_cat == "Low" & HET$migration == "Native"] <- "Women_Low_Native"
HET$Strata <- as.factor(HET$Strata)

# Financial issues
HET$DMEM[HET$EK002 == "1"] <- "No"
HET$DMEM[HET$EK002 == "2"] <- "Once"
HET$DMEM[HET$EK002 == "3"] <- "More than once"
HET$DMEM <- as.factor(HET$DMEM)

HET$DMEM_2cat[HET$EK002 == "1"] <- "No"
HET$DMEM_2cat[HET$EK002 == "2"] <- "Once or more"
HET$DMEM_2cat[HET$EK002 == "3"] <- "Once or more"
HET$DMEM_2cat <- as.factor(HET$DMEM_2cat)

# Social support 
HET$Socsupport[HET$SO009 == "1"] <- "Yes"
HET$Socsupport[HET$SO009 == "2"] <- "No"
HET$Socsupport <- as.factor(HET$Socsupport)

# BMI
HET$HA026 <- HET$HA026
HET$BMI <- HET$HA027/((HET$HA026/100)*(HET$HA026/100))
HET$BMI_cat[HET$BMI < 25] <- "Normal weight"
HET$BMI_cat[HET$BMI >= 25] <- "Overweight and obesity"
HET$BMI_cat <- as.factor(HET$BMI_cat)

HET$HA026[HET$HA026 < 100 | HET$HA026 > 250] <- NA
HET$HA027[HET$HA027 < 12 | HET$HA027 > 300] <- NA
HET$BMI[HET$BMI < 10 | HET$BMI > 80] <- NA

# Physical limitations
HET$Limitations[HET$HA074 == "1"] <- "Yes (No limitations)"
HET$Limitations[HET$HA074 == "2"] <- "No (limitations)"
HET$Limitations <- as.factor(HET$Limitations)

# Physical activity
HET$vig_PA[HET$LV048 == 1] <- 0
HET$vig_PA[HET$LV048 == 2] <- 0 + (30 - 0)/2
HET$vig_PA[HET$LV048 == 3] <- 30 + (60 - 30)/2
HET$vig_PA[HET$LV048 == 4] <- 60 + (90 - 60)/2
HET$vig_PA[HET$LV048 == 5] <- 90 + (120 - 90)/2
HET$vig_PA[HET$LV048 == 6] <- 120

HET$mod_PA[HET$LV049 == 1] <- 0
HET$mod_PA[HET$LV049 == 2] <- 0 + (30 - 0)/2
HET$mod_PA[HET$LV049 == 3] <- 30 + (60 - 30)/2
HET$mod_PA[HET$LV049 == 4] <- 60 + (90 - 60)/2
HET$mod_PA[HET$LV049 == 5] <- 90 + (150 - 90)/2
HET$mod_PA[HET$LV049 == 6] <- 150 + (300 - 150)/2
HET$mod_PA[HET$LV049 == 7] <- 300

HET$Physical_activity_total <- HET$mod_PA + 2*HET$vig_PA
HET$Physical_activity_recom [HET$Physical_activity_total < 150] <- "Inactive"
HET$Physical_activity_recom [HET$Physical_activity_total >= 150] <- "Active"
HET$Physical_activity_recom <- as.factor(HET$Physical_activity_recom)

# Fruits
HET$Fruit[HET$LV005 == 1] <- "3 times a day or more"
HET$Fruit[HET$LV005 == 2] <- "Twice a day"
HET$Fruit[HET$LV005 == 3] <- "Once a day"
HET$Fruit[HET$LV005 == 4] <- "5-6 times a week"
HET$Fruit[HET$LV005 == 5] <- "3-4 times a week"
HET$Fruit[HET$LV005 == 6] <- "1-2 times a week"
HET$Fruit[HET$LV005 == 7] <- "A few times a month or never"
HET$Fruit <- as.factor(HET$Fruit)

HET$Fruit_recom[HET$Fruit == "3 times a day or more" | HET$Fruit == "Twice a day"] <- "Adecuate"
HET$Fruit_recom[HET$Fruit == "Once a day" | HET$Fruit == "5-6 times a week" |
                HET$Fruit == "3-4 times a week" | HET$Fruit == "1-2 times a week" |
                HET$Fruit == "A few times a month or never" ] <- "Inadecuate"
HET$Fruit_recom <- as.factor(HET$Fruit_recom)

# Vegetables
HET$Vegetables[HET$LV004 == 1] <- "3 times a day or more"
HET$Vegetables[HET$LV004 == 2] <- "Twice a day"
HET$Vegetables[HET$LV004 == 3] <- "Once a day"
HET$Vegetables[HET$LV004 == 4] <- "5-6 times a week"
HET$Vegetables[HET$LV004 == 5] <- "3-4 times a week"
HET$Vegetables[HET$LV004 == 6] <- "1-2 times a week"
HET$Vegetables[HET$LV004 == 7] <- "A few times a month or never"
HET$Vegetables <- as.factor(HET$Vegetables)

HET$Vegetable_recom[HET$Vegetables == "3 times a day or more" | HET$Vegetables == "Twice a day"] <- "Adecuate"
HET$Vegetable_recom[HET$Vegetables == "Once a day" | HET$Vegetables == "5-6 times a week" |
                  HET$Vegetables == "3-4 times a week" | HET$Vegetables == "1-2 times a week" |
                  HET$Vegetables == "A few times a month or never" ] <- "Inadecuate"
HET$Vegetable_recom <- as.factor(HET$Vegetable_recom)

# Fruit and vegetable combined
HET$LV005_weight[HET$LV005 == 1] <- 3
HET$LV005_weight[HET$LV005 == 2] <- 2
HET$LV005_weight[HET$LV005 == 3] <- 1
HET$LV005_weight[HET$LV005 == 4] <- 0.8
HET$LV005_weight[HET$LV005 == 5] <- 0.5
HET$LV005_weight[HET$LV005 == 6] <- 0.2
HET$LV005_weight[HET$LV005 == 7] <- 0.07
HET$LV005_weight[which(is.na(HET$LV005))] <- 0

HET$LV004_weight[HET$LV004 == 1] <- 3
HET$LV004_weight[HET$LV004 == 2] <- 2
HET$LV004_weight[HET$LV004 == 3] <- 1
HET$LV004_weight[HET$LV004 == 4] <- 0.8
HET$LV004_weight[HET$LV004 == 5] <- 0.5
HET$LV004_weight[HET$LV004 == 6] <- 0.2
HET$LV004_weight[HET$LV004 == 7] <- 0.07
HET$LV004_weight[which(is.na(HET$LV004))] <- 0

HET$Fruit_Vegetable_recom <- HET$LV005_weight + HET$LV004_weight
HET$Fruit_Vegetable_recom[HET$Fruit_Vegetable_recom == 0] <- NA

HET$Fruit_Vegetable_recom[HET$Fruit_Vegetable_recom >= 3] <- "Adecuate"
HET$Fruit_Vegetable_recom[HET$Fruit_Vegetable_recom < 3] <- "Inadecuate"
HET$Fruit_Vegetable_recom <- as.factor(HET$Fruit_Vegetable_recom)

# Fruit/Vegetables and Physical activity combined
HET$Fruit_Vegetable_PA_recom[HET$Physical_activity_recom == "Active" & HET$Fruit_Vegetable_recom == "Adecuate"] <- "Both"
HET$Fruit_Vegetable_PA_recom[HET$Physical_activity_recom == "Active" & HET$Fruit_Vegetable_recom == "Inadecuate"] <- "Physical activity"
HET$Fruit_Vegetable_PA_recom[HET$Physical_activity_recom == "Inactive" & HET$Fruit_Vegetable_recom == "Adecuate"] <- "Fruit and vegetables"
HET$Fruit_Vegetable_PA_recom[HET$Physical_activity_recom == "Inactive" & HET$Fruit_Vegetable_recom == "Inadecuate"] <- "None"
HET$Fruit_Vegetable_PA_recom <- as.factor(HET$Fruit_Vegetable_PA_recom)

# Alcohol
HET$LV019[which(is.na(HET$LV018))] <- 1
HET$LV020[which(is.na(HET$LV018))] <- 5

HET$Alcohol_freq_weight[HET$LV018 == 1] <- 4
HET$Alcohol_freq_weight[HET$LV018 == 2] <- 3
HET$Alcohol_freq_weight[HET$LV018 == 3] <- 2
HET$Alcohol_freq_weight[HET$LV018 == 4] <- 1
HET$Alcohol_freq_weight[HET$LV018 == 5] <- 0

HET$Alcohol_quant_weight[HET$LV019 == 1] <- 0
HET$Alcohol_quant_weight[HET$LV019 == 2] <- 1
HET$Alcohol_quant_weight[HET$LV019 == 3] <- 2
HET$Alcohol_quant_weight[HET$LV019 == 4] <- 3
HET$Alcohol_quant_weight[HET$LV019 == 5] <- 4
HET$Alcohol_quant_weight[HET$LV019 == 6] <- NA

HET$Alcohol_binge_weight[HET$LV020 == 1] <- 4
HET$Alcohol_binge_weight[HET$LV020 == 2] <- 3
HET$Alcohol_binge_weight[HET$LV020 == 3] <- 2
HET$Alcohol_binge_weight[HET$LV020 == 4] <- 1
HET$Alcohol_binge_weight[HET$LV020 == 5] <- 0

HET$Alcohol_comb_risk <- HET$Alcohol_freq_weight + HET$Alcohol_quant_weight + HET$Alcohol_binge_weight 
HET$Alcohol_risk_cat[HET$Alcohol_comb_risk >= 6 & HET$sex_cat == "Men"] <- "Risky"
HET$Alcohol_risk_cat[HET$Alcohol_comb_risk >= 5 & HET$sex_cat == "Women"] <- "Risky"
HET$Alcohol_risk_cat[HET$Alcohol_comb_risk < 6 & HET$sex_cat == "Men"] <- "Not risky"
HET$Alcohol_risk_cat[HET$Alcohol_comb_risk < 5 & HET$sex_cat == "Women"] <- "Not risky"
HET$Alcohol_risk_cat <- as.factor(HET$Alcohol_risk_cat)

HET$Alcohol_freq[HET$LV018 == 1] <- "4 times a week or more"
HET$Alcohol_freq[HET$LV018 == 2] <- "2-3 times a week"
HET$Alcohol_freq[HET$LV018 == 3] <- "2-4 times a month"
HET$Alcohol_freq[HET$LV018 == 4] <- "Once a month or less"
HET$Alcohol_freq[HET$LV018 == 5] <- "Never"
HET$Alcohol_freq <- as.factor(HET$Alcohol_freq)

HET$Alcohol_2cat[HET$LV018 == 1] <- "Weekly of daily"
HET$Alcohol_2cat[HET$LV018 == 2] <- "Weekly of daily"
HET$Alcohol_2cat[HET$LV018 == 3] <- "Monthly or never"
HET$Alcohol_2cat[HET$LV018 == 4] <- "Monthly or never"
HET$Alcohol_2cat[HET$LV018 == 5] <- "Monthly or never"
HET$Alcohol_2cat <- as.factor(HET$Alcohol_2cat)

HET$Alcohol_quant[HET$LV019 == 1] <- "1-2"
HET$Alcohol_quant[HET$LV019 == 2] <- "3-4"
HET$Alcohol_quant[HET$LV019 == 3] <- "5-6"
HET$Alcohol_quant[HET$LV019 == 4] <- "7-9"
HET$Alcohol_quant[HET$LV019 == 5] <- "10 or more"
HET$Alcohol_quant[HET$LV019 == 6] <- "DK"
HET$Alcohol_quant <- as.factor(HET$Alcohol_quant)

# Smoking
HET$Smoking[HET$LV053 == 1] <- "Yes"
HET$Smoking[HET$LV053 == 2] <- "No"
HET$Smoking[HET$LV053 == 3] <- "No"
HET$Smoking <- as.factor(HET$Smoking)



#### Removing cases <25 years old and those with missing data ####
HET <- HET[which(HET$Alder > 24), ]
HET <- HET %>% select(sex_cat, edu3_cat, migration, Strata, DMEM, DMEM_2cat, Fruit_Vegetable_PA_recom, BMI, BMI_cat, 
                      Limitations, Socsupport, Smoking, Alcohol_risk_cat, argang, Alder)
HET <- HET[complete.cases(HET), ]

HET$Fruit_Vegetable_PA_recom_2cat[HET$Fruit_Vegetable_PA_recom == "Both"] <- "Both"
HET$Fruit_Vegetable_PA_recom_2cat[HET$Fruit_Vegetable_PA_recom == "Physical activity"] <- "None"
HET$Fruit_Vegetable_PA_recom_2cat[HET$Fruit_Vegetable_PA_recom == "Fruit and vegetables"] <- "None"
HET$Fruit_Vegetable_PA_recom_2cat[HET$Fruit_Vegetable_PA_recom == "None"] <- "None"
HET$Fruit_Vegetable_PA_recom_2cat <- as.factor(HET$Fruit_Vegetable_PA_recom_2cat)


#### Descriptive statistics ####
## General descriptive
# Independent variable
table(HET$sex_cat);prop.table(table(HET$sex_cat))
table(HET$edu3_cat);prop.table(table(HET$edu3_cat))
table(HET$migration);prop.table(table(HET$migration))
table(HET$Strata);prop.table(table(HET$Strata))

# Mediators
table(HET$DMEM_2cat);prop.table(table(HET$DMEM_2cat))
table(HET$Fruit_Vegetable_PA_recom_2cat);prop.table(table(HET$Fruit_Vegetable_PA_recom_2cat))

# Outcomes
mean(HET$BMI);sd(HET$BMI);summary(HET$BMI)
table(HET$BMI_cat);prop.table(table(HET$BMI_cat))

# Covariates
table(HET$Limitations);prop.table(table(HET$Limitations)) # physical limitation
table(HET$Socsupport);prop.table(table(HET$Socsupport)) # social support
table(HET$argang);prop.table(table(HET$argang)) # year or survey
table(HET$Smoking);prop.table(table(HET$Smoking)) # smoking
table(HET$Alcohol_risk_cat);prop.table(table(HET$Alcohol_risk_cat)) # alcohol
mean(HET$Alder);sd(HET$Alder);summary(HET$Alder) # age


## Across strata and mediators descriptive
library(descr)

# Mediators
crosstab(HET$Strata, HET$DMEM_2cat, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(HET$Strata, HET$Fruit_Vegetable_PA_recom_2cat, prop.r = T,digits = 2, chisq = T, plot = F)

crosstab(HET$sex_cat, HET$DMEM_2cat, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(HET$sex_cat, HET$Fruit_Vegetable_PA_recom_2cat, prop.r = T,digits = 2, chisq = T, plot = F)

crosstab(HET$edu3_cat, HET$DMEM_2cat, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(HET$edu3_cat, HET$Fruit_Vegetable_PA_recom_2cat, prop.r = T,digits = 2, chisq = T, plot = F)

crosstab(HET$migration, HET$DMEM_2cat, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(HET$migration, HET$Fruit_Vegetable_PA_recom_2cat, prop.r = T,digits = 2, chisq = T, plot = F)

# Outcomes
crosstab(HET$Strata, HET$BMI_cat, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(HET$DMEM_2cat, HET$BMI_cat, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(HET$Fruit_Vegetable_PA_recom_2cat, HET$BMI_cat, prop.r = T,digits = 2, chisq = T, plot = F)

HET %>%  group_by(Strata) %>% summarise(n = n(), 
            mean_BMI = mean(BMI, na.rm = T), med_BMI = median(BMI, na.rm = T), sd_BMI = sd(BMI, na.rm = T))
HET %>%  group_by(DMEM_2cat) %>% summarise(n = n(), 
            mean_BMI = mean(BMI, na.rm = T), med_BMI = median(BMI, na.rm = T), sd_BMI = sd(BMI, na.rm = T))
HET %>%  group_by(Fruit_Vegetable_PA_recom_2cat) %>% summarise(n = n(), 
            mean_BMI = mean(BMI, na.rm = T), med_BMI = median(BMI, na.rm = T), sd_BMI = sd(BMI, na.rm = T))

summary(aov(BMI ~ Strata, data = HET)); pairwise.t.test(HET$BMI, HET$Strata, p.adjust.method = "BH")
summary(aov(BMI ~ DMEM_2cat, data = HET)); pairwise.t.test(HET$BMI, HET$DMEM_2cat, p.adjust.method = "BH")
summary(aov(BMI ~ Fruit_Vegetable_PA_recom_2cat, data = HET)); pairwise.t.test(HET$BMI, HET$Fruit_Vegetable_PA_recom_2cat, p.adjust.method = "BH")



crosstab(HET$sex_cat, HET$BMI_cat, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(HET$edu3_cat, HET$BMI_cat, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(HET$migration, HET$BMI_cat, prop.r = T,digits = 2, chisq = T, plot = F)

HET %>%  group_by(sex_cat) %>% summarise(n = n(), 
                                        mean_BMI = mean(BMI, na.rm = T), med_BMI = median(BMI, na.rm = T), sd_BMI = sd(BMI, na.rm = T))
HET %>%  group_by(edu3_cat) %>% summarise(n = n(), 
                                      mean_BMI = mean(BMI, na.rm = T), med_BMI = median(BMI, na.rm = T), sd_BMI = sd(BMI, na.rm = T))
HET %>%  group_by(migration) %>% summarise(n = n(), 
                                           mean_BMI = mean(BMI, na.rm = T), med_BMI = median(BMI, na.rm = T), sd_BMI = sd(BMI, na.rm = T))
summary(aov(BMI ~ sex_cat, data = HET)); pairwise.t.test(HET$BMI, HET$sex_cat, p.adjust.method = "BH")
summary(aov(BMI ~ edu3_cat, data = HET)); pairwise.t.test(HET$BMI, HET$edu3_cat, p.adjust.method = "BH")
summary(aov(BMI ~ migration, data = HET)); pairwise.t.test(HET$BMI, HET$migration, p.adjust.method = "BH")


# Covariates
crosstab(HET$Strata, HET$Limitations, prop.r = T,digits = 2, chisq = T, plot = F) # physical limitation
crosstab(HET$Strata, HET$Socsupport, prop.r = T,digits = 2, chisq = T, plot = F) # social support
crosstab(HET$Strata, HET$argang, prop.r = T,digits = 2, chisq = T, plot = F) # year of survey
crosstab(HET$Strata, HET$Smoking, prop.r = T,digits = 2, chisq = T, plot = F) # Smoking
crosstab(HET$Strata, HET$Alcohol_risk_cat, prop.r = T,digits = 2, chisq = T, plot = F) # Alcohol_risk_cat

crosstab(HET$sex_cat, HET$Limitations, prop.r = T,digits = 2, chisq = T, plot = F) # physical limitation
crosstab(HET$sex_cat, HET$Socsupport, prop.r = T,digits = 2, chisq = T, plot = F) # social support
crosstab(HET$sex_cat, HET$argang, prop.r = T,digits = 2, chisq = T, plot = F) # year of survey
crosstab(HET$sex_cat, HET$Smoking, prop.r = T,digits = 2, chisq = T, plot = F) # Smoking
crosstab(HET$sex_cat, HET$Alcohol_risk_cat, prop.r = T,digits = 2, chisq = T, plot = F) # Alcohol_risk_cat

crosstab(HET$edu3_cat, HET$Limitations, prop.r = T,digits = 2, chisq = T, plot = F) # physical limitation
crosstab(HET$edu3_cat, HET$Socsupport, prop.r = T,digits = 2, chisq = T, plot = F) # social support
crosstab(HET$edu3_cat, HET$argang, prop.r = T,digits = 2, chisq = T, plot = F) # year of survey
crosstab(HET$edu3_cat, HET$Smoking, prop.r = T,digits = 2, chisq = T, plot = F) # Smoking
crosstab(HET$edu3_cat, HET$Alcohol_risk_cat, prop.r = T,digits = 2, chisq = T, plot = F) # Alcohol_risk_cat

crosstab(HET$migration, HET$Limitations, prop.r = T,digits = 2, chisq = T, plot = F) # physical limitation
crosstab(HET$migration, HET$Socsupport, prop.r = T,digits = 2, chisq = T, plot = F) # social support
crosstab(HET$migration, HET$argang, prop.r = T,digits = 2, chisq = T, plot = F) # year of survey
crosstab(HET$migration, HET$Smoking, prop.r = T,digits = 2, chisq = T, plot = F) # Smoking
crosstab(HET$migration, HET$Alcohol_risk_cat, prop.r = T,digits = 2, chisq = T, plot = F) # Alcohol_risk_cat

crosstab(HET$DMEM_2cat, HET$Limitations, prop.r = T,digits = 2, chisq = T, plot = F) # physical limitation
crosstab(HET$DMEM_2cat, HET$Socsupport, prop.r = T,digits = 2, chisq = T, plot = F) # social support
crosstab(HET$DMEM_2cat, HET$argang, prop.r = T,digits = 2, chisq = T, plot = F) # year of survey
crosstab(HET$DMEM_2cat, HET$Smoking, prop.r = T,digits = 2, chisq = T, plot = F) # Smoking
crosstab(HET$DMEM_2cat, HET$Alcohol_risk_cat, prop.r = T,digits = 2, chisq = T, plot = F) # Alcohol_risk_cat 

crosstab(HET$Fruit_Vegetable_PA_recom_2cat, HET$Limitations, prop.r = T,digits = 2, chisq = T, plot = F) # physical limitation
crosstab(HET$Fruit_Vegetable_PA_recom_2cat, HET$Socsupport, prop.r = T,digits = 2, chisq = T, plot = F) # social support
crosstab(HET$Fruit_Vegetable_PA_recom_2cat, HET$argang, prop.r = T,digits = 2, chisq = T, plot = F) # year of survey
crosstab(HET$Fruit_Vegetable_PA_recom_2cat, HET$Smoking, prop.r = T,digits = 2, chisq = T, plot = F) # Smoking
crosstab(HET$Fruit_Vegetable_PA_recom_2cat, HET$Alcohol_risk_cat, prop.r = T,digits = 2, chisq = T, plot = F) # Alcohol

crosstab(HET$BMI_cat, HET$Limitations, prop.r = T,digits = 2, chisq = T, plot = F) # physical limitation
crosstab(HET$BMI_cat, HET$Socsupport, prop.r = T,digits = 2, chisq = T, plot = F) # social support
crosstab(HET$BMI_cat, HET$argang, prop.r = T,digits = 2, chisq = T, plot = F) # year of survey
crosstab(HET$BMI_cat, HET$Smoking, prop.r = T,digits = 2, chisq = T, plot = F) # Smoking
crosstab(HET$BMI_cat, HET$Alcohol_risk_cat, prop.r = T,digits = 2, chisq = T, plot = F) # Alcohol 

HET %>%  group_by(Strata) %>% summarise(n = n(), 
               mean_Alder = mean(Alder, na.rm = T), med_Alder = median(Alder, na.rm = T), sd_Alder = sd(Alder, na.rm = T))
HET %>%  group_by(sex_cat) %>% summarise(n = n(), 
                                        mean_Alder = mean(Alder, na.rm = T), med_Alder = median(Alder, na.rm = T), sd_Alder = sd(Alder, na.rm = T))
HET %>%  group_by(edu3_cat) %>% summarise(n = n(), 
                                        mean_Alder = mean(Alder, na.rm = T), med_Alder = median(Alder, na.rm = T), sd_Alder = sd(Alder, na.rm = T))
HET %>%  group_by(migration) %>% summarise(n = n(), 
                                        mean_Alder = mean(Alder, na.rm = T), med_Alder = median(Alder, na.rm = T), sd_Alder = sd(Alder, na.rm = T))
HET %>%  group_by(DMEM_2cat) %>% summarise(n = n(), 
                mean_Alder = mean(Alder, na.rm = T), med_Alder = median(Alder, na.rm = T), sd_Alder = sd(Alder, na.rm = T))
HET %>%  group_by(Fruit_Vegetable_PA_recom_2cat) %>% summarise(n = n(), 
                mean_Alder = mean(Alder, na.rm = T), med_Alder = median(Alder, na.rm = T), sd_Alder = sd(Alder, na.rm = T))
HET %>%  group_by(BMI_cat) %>% summarise(n = n(), 
                mean_Alder = mean(Alder, na.rm = T), med_Alder = median(Alder, na.rm = T), sd_Alder = sd(Alder, na.rm = T))

summary(aov(Alder ~ Strata, data = HET)); pairwise.t.test(HET$Alder, HET$Strata, p.adjust.method = "BH")
summary(aov(Alder ~ sex_cat, data = HET)); pairwise.t.test(HET$Alder, HET$sex_cat, p.adjust.method = "BH")
summary(aov(Alder ~ edu3_cat, data = HET)); pairwise.t.test(HET$Alder, HET$edu3_cat, p.adjust.method = "BH")
summary(aov(Alder ~ migration, data = HET)); pairwise.t.test(HET$Alder, HET$migration, p.adjust.method = "BH")
summary(aov(Alder ~ DMEM_2cat, data = HET)); pairwise.t.test(HET$Alder, HET$DMEM_2cat, p.adjust.method = "BH")
summary(aov(Alder ~ Fruit_Vegetable_PA_recom_2cat, data = HET)); pairwise.t.test(HET$Alder, HET$Fruit_Vegetable_PA_recom_2cat, p.adjust.method = "BH")
summary(aov(Alder ~ BMI_cat, data = HET)); pairwise.t.test(HET$Alder, HET$BMI_cat, p.adjust.method = "BH")



#### Generalized linear models ####
library(sjPlot)
HET$Strata <- relevel(HET$Strata, ref = "Men_High_Native")
HET$sex_cat <- relevel(HET$sex_cat, ref = "Men")
HET$edu3_cat <- relevel(HET$edu3_cat, ref = "High")
HET$migration <- relevel(HET$migration, ref = "Native")
HET$BMI_cat <- relevel(HET$BMI_cat, ref = "Normal weight")
HET$DMEM_2cat <- relevel(HET$DMEM_2cat, ref = "No")
HET$Fruit_Vegetable_PA_recom_2cat <- relevel(HET$Fruit_Vegetable_PA_recom_2cat, ref = "Both")
HET$Limitations <- relevel(HET$Limitations, ref = "Yes (No limitations)")
HET$Socsupport <- relevel(HET$Socsupport, ref = "Yes")
HET$Smoking <- relevel(HET$Smoking, ref = "No")
HET$Alcohol_risk_cat <- relevel(HET$Alcohol_risk_cat, ref = "Not risky")

HET$Year[HET$argang == 2016] <- "2016"
HET$Year[HET$argang == 2018] <- "2018"
HET$Year[HET$argang == 2020] <- "2020"
HET$Year[HET$argang == 2021] <- "2021"
HET$Year <- as.factor(HET$Year)
HET$Year <- relevel(HET$Year, ref = "2016")

model0_Strata <- glm(BMI ~ Strata, family = gaussian(link="identity"), data = HET)
model0_Strata_DMEM <- glm(DMEM_2cat ~ Strata, family = binomial(link="identity"), data = HET)
model0_Strata_Lifestyle_both <- glm(Fruit_Vegetable_PA_recom_2cat ~ Strata, family = binomial(link="identity"), data = HET)
tab_model(model0_Strata, digits = 2)
tab_model(model0_Strata_DMEM, digits = 4)
tab_model(model0_Strata_Lifestyle_both, digits = 4)


model0_DMEM_2cat <- glm(BMI ~ DMEM_2cat, family = gaussian(link="identity"), data = HET)
model0_Fruit_Vegetable_PA_recom <- glm(BMI ~ Fruit_Vegetable_PA_recom_2cat, family = gaussian(link="identity"), data = HET)
tab_model(model0_DMEM_2cat, model0_Fruit_Vegetable_PA_recom, digits = 2)


model1 <- glm(BMI ~ Alder + Limitations + Socsupport + Year + Smoking + Alcohol_risk_cat, family = gaussian(link="identity"), data = HET)
model2 <- glm(BMI ~ Alder + Limitations + Socsupport + Year + Smoking + Alcohol_risk_cat + Strata, family = gaussian(link="identity"), data = HET)
model3 <- glm(BMI ~ Alder + Limitations + Socsupport + Year + Smoking + Alcohol_risk_cat + Strata + DMEM_2cat, family = gaussian(link="identity"), data = HET)
model4 <- glm(BMI ~ Alder + Limitations + Socsupport + Year + Smoking + Alcohol_risk_cat + Strata + DMEM_2cat + Fruit_Vegetable_PA_recom_2cat, family = gaussian(link="identity"), data = HET)
tab_model(model1, model2, model3, model4, digits = 2)


#### Mediation analysis ####
library(lavaan)

# Categorical variables #
HET$Strata_group1 <- ifelse(HET$Strata == "Men_High_Immigrant", 1, 0)
HET$Strata_group2 <- ifelse(HET$Strata == "Men_Low_Immigrant", 1, 0)
HET$Strata_group3 <- ifelse(HET$Strata == "Men_Low_Native", 1, 0)
HET$Strata_group4 <- ifelse(HET$Strata == "Men_Medium_Immigrant", 1, 0)
HET$Strata_group5 <- ifelse(HET$Strata == "Men_Medium_Native", 1, 0)
HET$Strata_group6 <- ifelse(HET$Strata == "Women_High_Immigrant", 1, 0)
HET$Strata_group7 <- ifelse(HET$Strata == "Women_High_Native", 1, 0)
HET$Strata_group8 <- ifelse(HET$Strata == "Women_Low_Immigrant", 1, 0)
HET$Strata_group9 <- ifelse(HET$Strata == "Women_Low_Native", 1, 0)
HET$Strata_group10 <- ifelse(HET$Strata == "Women_Medium_Immigrant", 1, 0)
HET$Strata_group11 <- ifelse(HET$Strata == "Women_Medium_Native", 1, 0)

# Continuous variables #
HET$Fruit_Vegetable_PA_recom_cont[HET$Fruit_Vegetable_PA_recom == "Both"] <- 0
HET$Fruit_Vegetable_PA_recom_cont[HET$Fruit_Vegetable_PA_recom == "Physical activity"] <- 1
HET$Fruit_Vegetable_PA_recom_cont[HET$Fruit_Vegetable_PA_recom == "Fruit and vegetables"] <- 1
HET$Fruit_Vegetable_PA_recom_cont[HET$Fruit_Vegetable_PA_recom == "None"] <- 1

HET$EK002_cat[HET$DMEM == "No"] <- 0
HET$EK002_cat[HET$DMEM == "Once"] <- 1
HET$EK002_cat[HET$DMEM == "More than once"] <- 1

# Covariates
HET$Limitations_cont[HET$Limitations == "Yes (No limitations)"] <- 0
HET$Limitations_cont[HET$Limitations == "No (limitations)"] <- 1

HET$Socsupport_cont[HET$Socsupport == "Yes"] <- 0
HET$Socsupport_cont[HET$Socsupport == "No"] <- 1

HET$Smoking_cont[HET$Smoking == "No"] <- 0
HET$Smoking_cont[HET$Smoking == "Yes"] <- 1

HET$Alcohol_risk_cat_cont[HET$Alcohol_risk_cat == "Not risky"] <- 0
HET$Alcohol_risk_cat_cont[HET$Alcohol_risk_cat == "Risky"] <- 1

HET$Year_centred <- scale(HET$argang, scale=FALSE)
HET$Age_centred <- scale(HET$Alder, scale=FALSE)

## Sequential mediation analysis ##
set.seed(123456)

## Strata - DMEM 
model_DMEM_adjusted <- "#sequential mediation
  EK002_cat ~ a1*Strata_group1 + a11*Strata_group2 + a12*Strata_group3 + a13*Strata_group4 + a14*Strata_group5 + a15*Strata_group6 + a16*Strata_group7 + a17*Strata_group8 + a18*Strata_group9 + a19*Strata_group10 + a100*Strata_group11 + Age_centred + Year_centred
  Fruit_Vegetable_PA_recom_cont ~ a2*Strata_group1 + a21*Strata_group2 + a22*Strata_group3 + a23*Strata_group4 + a24*Strata_group5 + a25*Strata_group6 + a26*Strata_group7 + a27*Strata_group8 + a28*Strata_group9 + a29*Strata_group10 + a200*Strata_group11 + d21*EK002_cat + e1*Strata_group1:EK002_cat + e11*Strata_group2:EK002_cat + e12*Strata_group3:EK002_cat + e13*Strata_group4:EK002_cat + e14*Strata_group5:EK002_cat + e15*Strata_group6:EK002_cat + e16*Strata_group7:EK002_cat + e17*Strata_group8:EK002_cat + e18*Strata_group9:EK002_cat + e19*Strata_group10:EK002_cat + e100*Strata_group11:EK002_cat + Age_centred + Limitations_cont + Year_centred
  BMI ~  cp1*Strata_group1 + cp11*Strata_group2 + cp12*Strata_group3 + cp13*Strata_group4 + cp14*Strata_group5 + cp15*Strata_group6 + cp16*Strata_group7 + cp17*Strata_group8 + cp18*Strata_group9 + cp19*Strata_group10 + cp100*Strata_group11 + b1*EK002_cat + b2*Fruit_Vegetable_PA_recom_cont + f1*Strata_group1:EK002_cat + f11*Strata_group2:EK002_cat + f12*Strata_group3:EK002_cat + f13*Strata_group4:EK002_cat + f14*Strata_group5:EK002_cat + f15*Strata_group6:EK002_cat + f16*Strata_group7:EK002_cat + f17*Strata_group8:EK002_cat + f18*Strata_group9:EK002_cat + f19*Strata_group10:EK002_cat + f100*Strata_group11:EK002_cat + g1*Strata_group1:Fruit_Vegetable_PA_recom_cont + g11*Strata_group2:Fruit_Vegetable_PA_recom_cont + g12*Strata_group3:Fruit_Vegetable_PA_recom_cont + g13*Strata_group4:Fruit_Vegetable_PA_recom_cont + g14*Strata_group5:Fruit_Vegetable_PA_recom_cont + g15*Strata_group6:Fruit_Vegetable_PA_recom_cont + g16*Strata_group7:Fruit_Vegetable_PA_recom_cont + g17*Strata_group8:Fruit_Vegetable_PA_recom_cont + g18*Strata_group9:Fruit_Vegetable_PA_recom_cont + g19*Strata_group10:Fruit_Vegetable_PA_recom_cont + g100*Strata_group11:Fruit_Vegetable_PA_recom_cont + h1*Fruit_Vegetable_PA_recom_cont:EK002_cat + Age_centred + Limitations_cont + Socsupport_cont + Year_centred + Smoking_cont + Alcohol_risk_cat_cont
 
  # Pure indirect serial effect
  ind_serial_eff1:= a1*d21*b2
  ind_serial_eff2:= a11*d21*b2
  ind_serial_eff3:= a12*d21*b2
  ind_serial_eff4:= a13*d21*b2
  ind_serial_eff5:= a14*d21*b2
  ind_serial_eff6:= a15*d21*b2
  ind_serial_eff7:= a16*d21*b2
  ind_serial_eff8:= a17*d21*b2
  ind_serial_eff9:= a18*d21*b2
  ind_serial_eff10:= a19*d21*b2
  ind_serial_eff11:= a100*d21*b2
  
  
  # Pure indirect primary effects
  ind_prim_m11:= a1*b1
  ind_prim_m12:= a11*b1
  ind_prim_m13:= a12*b1
  ind_prim_m14:= a13*b1
  ind_prim_m15:= a14*b1
  ind_prim_m16:= a15*b1
  ind_prim_m17:= a16*b1
  ind_prim_m18:= a17*b1
  ind_prim_m19:= a18*b1
  ind_prim_m110:= a19*b1
  ind_prim_m111:= a100*b1
  
  ind_prim_m21:= a2*b2
  ind_prim_m22:= a21*b2
  ind_prim_m23:= a22*b2
  ind_prim_m24:= a23*b2
  ind_prim_m25:= a24*b2
  ind_prim_m26:= a25*b2
  ind_prim_m27:= a26*b2
  ind_prim_m28:= a27*b2
  ind_prim_m29:= a28*b2
  ind_prim_m210:= a29*b2
  ind_prim_m211:= a200*b2
  
  
  # Pure indirect secundary effects
  ind_sec_m11:= a1*d21
  ind_sec_m12:= a11*d21
  ind_sec_m13:= a12*d21
  ind_sec_m14:= a13*d21
  ind_sec_m15:= a14*d21
  ind_sec_m16:= a15*d21
  ind_sec_m17:= a16*d21
  ind_sec_m18:= a17*d21
  ind_sec_m19:= a18*d21
  ind_sec_m110:= a19*d21
  ind_sec_m111:= a100*d21
  
  ind_sec_m2:= d21*b2
  
  
  # Interaction serial indirect effect
  int_serial_eff1:= a1*d21*b2*e1*h1
  int_serial_eff2:= a11*d21*b2*e11*h1
  int_serial_eff3:= a12*d21*b2*e12*h1
  int_serial_eff4:= a13*d21*b2*e13*h1
  int_serial_eff5:= a14*d21*b2*e14*h1
  int_serial_eff6:= a15*d21*b2*e15*h1
  int_serial_eff7:= a16*d21*b2*e16*h1
  int_serial_eff8:= a17*d21*b2*e17*h1
  int_serial_eff9:= a18*d21*b2*e18*h1
  int_serial_eff10:= a19*d21*b2*e19*h1
  int_serial_eff11:= a100*d21*b2*e100*h1
  
  
  # Interaction primary indirect effects
  int_prim_ind_eff_m11:= f1*a1
  int_prim_ind_eff_m12:= f11*a11
  int_prim_ind_eff_m13:= f12*a12
  int_prim_ind_eff_m14:= f13*a13
  int_prim_ind_eff_m15:= f14*a14
  int_prim_ind_eff_m16:= f15*a15
  int_prim_ind_eff_m17:= f16*a16
  int_prim_ind_eff_m18:= f17*a17
  int_prim_ind_eff_m19:= f18*a18
  int_prim_ind_eff_m110:= f19*a19
  int_prim_ind_eff_m111:= f100*a100
  
  int_prim_ind_eff_m21:= g1*a2
  int_prim_ind_eff_m22:= g11*a21
  int_prim_ind_eff_m23:= g12*a22
  int_prim_ind_eff_m24:= g13*a23
  int_prim_ind_eff_m25:= g14*a24
  int_prim_ind_eff_m26:= g15*a25
  int_prim_ind_eff_m27:= g16*a26
  int_prim_ind_eff_m28:= g17*a27
  int_prim_ind_eff_m29:= g18*a28
  int_prim_ind_eff_m210:= g19*a29
  int_prim_ind_eff_m211:= g100*a200
  
  
  # Interaction secundary indirect effects
  int_secund_ind_eff_m11:= e1*a1
  int_secund_ind_eff_m12:= e11*a11
  int_secund_ind_eff_m13:= e12*a12
  int_secund_ind_eff_m14:= e13*a13
  int_secund_ind_eff_m15:= e14*a14
  int_secund_ind_eff_m16:= e15*a15
  int_secund_ind_eff_m17:= e16*a16
  int_secund_ind_eff_m18:= e17*a17
  int_secund_ind_eff_m19:= e18*a18
  int_secund_ind_eff_m110:= e19*a19
  int_secund_ind_eff_m111:= e100*a100
  
  int_secund_ind_eff_m2:= h1*d21
  
  
  # Total pure indirect effects
  total_pure_ind_eff1:= a1*d21*b2 + a1*b1 + a2*b2
  total_pure_ind_eff2:= a11*d21*b2 + a1*b1 + a21*b2
  total_pure_ind_eff3:= a12*d21*b2 + a1*b1 + a22*b2
  total_pure_ind_eff4:= a13*d21*b2 + a1*b1 + a23*b2
  total_pure_ind_eff5:= a14*d21*b2 + a1*b1 + a24*b2
  total_pure_ind_eff6:= a15*d21*b2 + a1*b1 + a25*b2
  total_pure_ind_eff7:= a16*d21*b2 + a1*b1 + a26*b2
  total_pure_ind_eff8:= a17*d21*b2 + a1*b1 + a27*b2
  total_pure_ind_eff9:= a18*d21*b2 + a1*b1 + a28*b2
  total_pure_ind_eff10:= a19*d21*b2 + a1*b1 + a29*b2
  total_pure_ind_eff11:= a100*d21*b2 + a1*b1 + a200*b2
  
  # Total indirect effects
  total_tot_ind_eff1:= a1*d21*b2 + a1*b1 + a2*b2 + a1*d21*b2*e1*h1 + f1*a1 + g1*a2
  total_tot_ind_eff2:= a11*d21*b2 + a1*b1 + a21*b2 + a11*d21*b2*e11*h1 + f11*a11 + g11*a21
  total_tot_ind_eff3:= a12*d21*b2 + a1*b1 + a22*b2 + a12*d21*b2*e12*h1 + f12*a12 + g12*a22
  total_tot_ind_eff4:= a13*d21*b2 + a1*b1 + a23*b2 + a13*d21*b2*e13*h1 + f13*a13 + g13*a23
  total_tot_ind_eff5:= a14*d21*b2 + a1*b1 + a24*b2 + a14*d21*b2*e14*h1 + f14*a14 + g14*a24
  total_tot_ind_eff6:= a15*d21*b2 + a1*b1 + a25*b2 + a15*d21*b2*e15*h1 + f15*a15 + g15*a25
  total_tot_ind_eff7:= a16*d21*b2 + a1*b1 + a26*b2 + a16*d21*b2*e16*h1 + f16*a16 + g16*a26
  total_tot_ind_eff8:= a17*d21*b2 + a1*b1 + a27*b2 + a17*d21*b2*e17*h1 + f17*a17 + g17*a27
  total_tot_ind_eff9:= a18*d21*b2 + a1*b1 + a28*b2 + a18*d21*b2*e18*h1 + f18*a18 + g18*a28
  total_tot_ind_eff10:= a19*d21*b2 + a1*b1 + a29*b2 + a19*d21*b2*e19*h1 + f19*a19 + g19*a29
  total_tot_ind_eff11:= a100*d21*b2 + a1*b1 + a200*b2 + a100*d21*b2*e100*h1 + f100*a100 + g100*a200
  
  
  # Total total effects
  total_tot_tot_eff1:= a1*d21*b2 + a1*b1 + a2*b2 + a1*d21*b2*e1*h1 + f1*a1 + g1*a2 + cp1
  total_tot_tot_eff2:= a11*d21*b2 + a1*b1 + a21*b2 + a11*d21*b2*e11*h1 + f11*a11 + g11*a21 + cp11
  total_tot_tot_eff3:= a12*d21*b2 + a1*b1 + a22*b2 + a12*d21*b2*e12*h1 + f12*a12 + g12*a22 + cp12
  total_tot_tot_eff4:= a13*d21*b2 + a1*b1 + a23*b2 + a13*d21*b2*e13*h1 + f13*a13 + g13*a23 + cp13
  total_tot_tot_eff5:= a14*d21*b2 + a1*b1 + a24*b2 + a14*d21*b2*e14*h1 + f14*a14 + g14*a24 + cp14
  total_tot_tot_eff6:= a15*d21*b2 + a1*b1 + a25*b2 + a15*d21*b2*e15*h1 + f15*a15 + g15*a25 + cp15
  total_tot_tot_eff7:= a16*d21*b2 + a1*b1 + a26*b2 + a16*d21*b2*e16*h1 + f16*a16 + g16*a26 + cp16
  total_tot_tot_eff8:= a17*d21*b2 + a1*b1 + a27*b2 + a17*d21*b2*e17*h1 + f17*a17 + g17*a27 + cp17
  total_tot_tot_eff9:= a18*d21*b2 + a1*b1 + a28*b2 + a18*d21*b2*e18*h1 + f18*a18 + g18*a28 + cp18
  total_tot_tot_eff10:= a19*d21*b2 + a1*b1 + a29*b2 + a19*d21*b2*e19*h1 + f19*a19 + g19*a29 + cp19
  total_tot_tot_eff11:= a100*d21*b2 + a1*b1 + a200*b2 + a100*d21*b2*e100*h1 + f100*a100 + g100*a200 + cp100
"
fitmod_HET_DMEM_adjusted <- sem(model_DMEM_adjusted, data = HET, std.ov = F)
summary(fitmod_HET_DMEM_adjusted, fit.measures=F, ci=T, nd=4)


