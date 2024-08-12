####### PART 1, DATA CLEANING

# Install package
packages <- c("reshape2", "stringr", "dplyr", "tidyverse", "car", "regclass", "DMwR2", "party", 
              "e1071", "broom", "LambertW", "ggpubr", "vcd", "randomForest", "glmnet", "Metrics", 
              "DescTools", "pROC", "janitor", "RWeka", "gmodels", "caret", "C50", "leaps", 
              "ggplot2", "VIM", "lubridate", "rcompanion", "nortest", "Epi", "factoextra", 
              "MASS", "mice", "pwr")

install.packages(packages)

# Import
## Library
library(reshape2)
library(stringr)
library(dplyr)
library(tidyverse)
library(car)
library(regclass)
library(DMwR2)
library(party)
library(e1071)
library(broom)
library(LambertW)
library(ggpubr)
library(vcd)
library(randomForest)
library(glmnet)
library(Metrics)
library(DescTools)
library(pROC)
library(janitor)
library(RWeka)
library(gmodels)
library(caret)
library(C50)
library(leaps)
library(dplyr)
library(tidyr)
library(ggplot2)
library(VIM)
library(lubridate)
library(rcompanion)
library(nortest)
library(Epi)
library(factoextra)
library(pROC)
library(MASS)
library(mice)
library(pwr)
library(rcompanion)

## Import Dataset
dem50 = read.csv("D:/Project-3697/Extr_23189/OpenClinica_Demographics/OpenClinica_Demographics.csv", 
                 header = T, na.strings = c(""))
dem55 = read.csv("D:/Project-3697/Extr_23215/OpenClinica_Demographics/OpenClinica_Demographics.csv", 
                 header = T, na.strings = c(""))
ecom50 = read.csv("D:/project-3697/Extr_23189/eDRIS_Comorbidities/eDRIS_Comorbidities.csv", 
                  header = T, na.strings = c(""))
ecom55 = read.csv("D:/project-3697/Extr_23215/eDRIS_Comorbidities/eDRIS_Comorbidities.csv", 
                  header = T, na.strings = c(""))
pres50 = read.csv("D:/Project-3697/Extr_23189/eDRIS_Prescribing/eDRIS_Prescribing.csv", 
                  header = T, na.strings = c(""))
pres55 = read.csv("D:/Project-3697/Extr_23215/eDRIS_Prescribing/eDRIS_Prescribing.csv", 
                  header = T, na.strings = c(""))
medical_history = read.csv("D:/project-3697-study/Extr_24534/OpenClinica_Medical_History/OpenClinica_Medical_History.csv",
                           header = T, na.strings = c(""))
views_health = read.csv("D:/project-3697-study/Extr_24534/OpenClinica_Views_Health/OpenClinica_Views_Health.csv",
                        header = T, na.strings = c(""))
blood_test = read.csv("D:/project-3697-study/Extr_24534/OpenClinica_Oncimmune_Blood_Test/OpenClinica_Oncimmune_Blood_Test.csv",
                      header = T, na.strings = c(""))


## Merge Big Data set
comorb1 = rbind(ecom50, ecom55)
demog1 = rbind(dem50, dem55)
pres1 = rbind(pres50, pres55)

## Analyze different tables
length(unique(demog1$PROCHI)) # 12141
colnames((demog1))
length(unique(views_health$PROCHI)) # 8537
colnames((views_health))
length(unique(blood_test$PROCHI)) # 4661
colnames((pres1))
length(unique(pres1$PROCHI)) # 12139
table(views_health$TestGroup) # No test 4261, Test 4268

## Delete duplicate blood test data
patients_with_two_tests <- blood_test$PROCHI[duplicated(blood_test$PROCHI)]
length(patients_with_two_tests) # 21
filtered_data <- blood_test[!(blood_test$PROCHI %in% patients_with_two_tests & blood_test$Blood_test_number == 1), ]
filtered_data <- filtered_data[filtered_data$Laboratory_result != "Invalid", ]
blood_test <- filtered_data
length(blood_test$PROCHI) # 4660
table(blood_test$Laboratory_result) # Positive 465, negative 4195


## Delete duplicate demographic data
duplicated_PROCHI <- demog1$PROCHI[duplicated(demog1$PROCHI) | duplicated(demog1$PROCHI, fromLast = TRUE)]
length(duplicated_PROCHI) # 1276
duplicated_rows <- demog1[demog1$PROCHI %in% duplicated_PROCHI, ]
demog1 <- demog1[!duplicated(demog1$PROCHI), ]
length(demog1$PROCHI) # 12141

## Define age categories and pack years
# Define pack year range 
a1 = demog1$Pack_year_history >= 1 & demog1$Pack_year_history <= 9
a2 = demog1$Pack_year_history >= 10 & demog1$Pack_year_history <= 19
a3 = demog1$Pack_year_history >= 20 & demog1$Pack_year_history <= 29
a4 = demog1$Pack_year_history >= 30 & demog1$Pack_year_history <= 49
a5 = demog1$Pack_year_history >= 50 & demog1$Pack_year_history <= 79
a6 = demog1$Pack_year_history >= 80

demog1$Pack_year_history_cat = demog1$Pack_year_history
demog1$Pack_year_history_cat[a1] = "1 - 9"
demog1$Pack_year_history_cat[a2] = "10 - 19"
demog1$Pack_year_history_cat[a3] = "20 - 29"
demog1$Pack_year_history_cat[a4] = "30 - 49"
demog1$Pack_year_history_cat[a5] = "50 - 79"
demog1$Pack_year_history_cat[a6] = "80 +"
demog1$Pack_year_history_cat = factor(demog1$Pack_year_history_cat, 
                                      levels = c("1 - 9", "10 - 19", "20 - 29", "30 - 49", 
                                                 "50 - 79",  "80 +")) 
# Define age cat
prop.table(table(demog1$age))
table(demog1$age)
table(demog1$age)
b1 = demog1$age >= 50 & demog1$age <=54
b2 = demog1$age >= 55 & demog1$age <= 59
b3 = demog1$age >= 60 & demog1$age <= 64
b4 = demog1$age >= 65 & demog1$age <= 69
b5 = demog1$age >= 70 

demog1$age_cat = demog1$age
demog1$age_cat[b1] = "50 - 54"
demog1$age_cat[b2] = "55 - 59"
demog1$age_cat[b3] = "60 - 64"
demog1$age_cat[b4] = "65 - 69"
demog1$age_cat[b5] = "70+"
demog1$age_cat <- as.factor(demog1$age_cat)

### Merge
# Merge prescription data, blood test results, and health views
pres1 <- merge(pres1,  views_health, by = "PROCHI",  all.y = TRUE)
pres1 <- merge(pres1, blood_test, by = "PROCHI",  all.x = TRUE)
pres1 <- merge(pres1, demog1[, c("PROCHI", "age", "Gender",  "Smoking_status", "Pack_year_history","Pack_year_history_cat","age_cat" )], by="PROCHI", all.x = TRUE)
length(unique(pres1$PROCHI)) # 8537 participants
# Pass the merged prescription data from pres1 to pres2
pres2 <- pres1

# Calculate how many BNF entries are miss
filtered_data <- pres1[!is.na(pres1$Date_dispensed) & is.na(pres1$BNF_Code), ]
unique_prchi_count <- length(unique(filtered_data$PROCHI))
unique_prchi_count # 262

# Remove columns that are not needed
pres2 <- subset(pres2, select = -c(Date_paid ,Date_prescribed,International_nonproprietary_name,
                                   Medication_name,Strength,Formulation,Drug_description,Product_description,
                                   Item_Type,Quantity_of_drugs,Quantity_of_drugs_dispensed,hb,Qu1,Qu2,Qu3,Qu4,
                                   Qu5,Qu6,Qu7,Qu8,Qu9,Visit.x,Visit.y,Blood_test_number,Date_blood_taken))

### Assign test group to those who have blood test result
# Count for have Lab result but don't have Testgroup
sum(is.na(pres2$TestGroup)) # 613
unique_prochi_count <- pres2 %>%
  filter(!is.na(Laboratory_result) & (is.na(TestGroup) | TestGroup == "")) %>%
  summarise(unique_count = n_distinct(PROCHI))
unique_prochi_count # 3 participant belong to this

pres2 <- pres2 %>%
  mutate(TestGroup = case_when(
    !is.na(Laboratory_result) & (is.na(TestGroup) | TestGroup == "") ~ "Test",
    TRUE ~ TestGroup
  ))

test_group <- pres2%>%
  filter(TestGroup == "Test")
result <- pres2 %>%
  filter(TestGroup == "Test" & (is.na(Laboratory_result) | Laboratory_result == ""))
length(unique(result$PROCHI)) # 20 belong to test group but lab result is NA

# Choose testgroup is NA
new_table <- pres2 %>%
  filter(is.na(TestGroup) | TestGroup == "")
length(unique(new_table$PROCHI)) # 5 people dont have group result
# Delete TestGroup NA row
filtered_pres2 <- pres2 %>%
  filter(!is.na(TestGroup))
length(unique(filtered_pres2$PROCHI)) # total 8532
pres2 <- filtered_pres2


# Check the number of test group
new_test <- pres2 %>%
  filter(TestGroup == "Test")
sum(is.na(new_test$Laboratory_result)) #2302
length(unique(new_test$PROCHI)) # 4271 test group

# Choose NA lab result
# choose Laboratory NA
laboratory_na <- new_test %>%
  filter(is.na(Laboratory_result) | Laboratory_result == "")
# Pick up  PROCHI value
prochi_values <- unique(laboratory_na$PROCHI)
# pick up across PROCHI 
related_rows <- new_test %>%
  filter(PROCHI %in% prochi_values)

length(related_rows$PROCHI) # 2302 rows
sum(is.na(related_rows$Laboratory_result)) #2302
related_rows_unique <- related_rows %>%
  distinct(PROCHI, .keep_all = TRUE)
table(related_rows_unique$TestGroup) # 20 NA belong to test group


### Filling out the questionnaire time assignment
# Check dispensed not NA, questionnaire is NA
sum(is.na(pres2$Date_dispensed)) # 2988
sum(is.na(pres2$DateQuestionnaireFilledIn)) # 1687
missing_date_records <- pres2 %>% filter(!is.na(Date_dispensed) & is.na(DateQuestionnaireFilledIn))
length(unique(missing_date_records$PROCHI)) # 24 data record
print(names(pres2))

# Assign time
pres2 <- pres2 %>%
  mutate(DateQuestionnaireFilledIn = ifelse(is.na(DateQuestionnaireFilledIn), Date_dispensed, DateQuestionnaireFilledIn))

## Check for date format
# head(pres2$Date_dispensed)
# head(pres2$DateQuestionnaireFilledIn)

# Define a function to analyze date
parse_date_safe <- function(date_strings) {
  # Try multiple data analysis way
  parsed_dates <- dmy_hms(date_strings, quiet = TRUE)
  parsed_dates[is.na(parsed_dates)] <- dmy(date_strings[is.na(parsed_dates)], quiet = TRUE)
  parsed_dates[is.na(parsed_dates)] <- mdy_hms(date_strings[is.na(parsed_dates)], quiet = TRUE)
  parsed_dates[is.na(parsed_dates)] <- mdy(date_strings[is.na(parsed_dates)], quiet = TRUE)
  parsed_dates[is.na(parsed_dates)] <- ymd_hms(date_strings[is.na(parsed_dates)], quiet = TRUE)
  parsed_dates[is.na(parsed_dates)] <- ymd(date_strings[is.na(parsed_dates)], quiet = TRUE)
  
  return(parsed_dates)
}
# parse Date_dispensed and DateQuestionnaireFilledIn
pres2 <- pres2 %>%
  mutate(Date_dispensed = parse_date_safe(Date_dispensed),
         DateQuestionnaireFilledIn = parse_date_safe(DateQuestionnaireFilledIn))
# Check for NA
sum(is.na(pres2$Date_dispensed)) # 2988 missing
sum(is.na(pres2$DateQuestionnaireFilledIn)) # 13

### Formatting date
#pres2$Date_dispensed = as.Date(mdy_hms(pres2$Date_dispensed))
#pres2$DateQuestionnaireFilledIn = as.Date(mdy_hms(pres2$DateQuestionnaireFilledIn))
# Set 5 month and 24 month
pres2$plus_5month <- pres2$DateQuestionnaireFilledIn %m+% months(5)
pres2$plus_24month <- pres2$DateQuestionnaireFilledIn %m+% months(24)
# Define intervals
interval_baseline <- interval(start = pres2$DateQuestionnaireFilledIn, end = pres2$plus_5month)
interval_2years <- interval(start = pres2$plus_5month, end = pres2$plus_24month)
# Assign 0, 1 for interval
pres2$baseline <- ifelse(pres2$Date_dispensed %within% interval_baseline, 1, 0) 
pres2$two_years <- ifelse(pres2$Date_dispensed %within% interval_2years, 1, 0)
pres2 <- pres2 %>%
  mutate(baseline = ifelse(is.na(baseline), 0, baseline))
pres2 <- pres2 %>%
  mutate(two_years = ifelse(is.na(two_years), 0, two_years))

# Select Test and No test group
pres3 <- pres2
#pres3 <- pres3 %>%
#filter(!is.na(TestGroup) & TestGroup != "")
length(unique(pres3$PROCHI)) # 8532 people have group information
# Count for unique participants in pres3 don't have BNF code
unique_participants_with_empty_BNF <- pres3 %>%
  filter(is.na(BNF_Code) | BNF_Code == "") %>%
  distinct(PROCHI) %>%
  nrow()
unique_participants_with_empty_BNF # 3045 don't have BNF code 35.69%

### Set BNF categories
# First 6 character
pres3$BNF_6 <- str_sub(string = pres3$BNF_Code, end = 6)
length(unique(pres3$BNF_6)) # 341
# First 4 character
pres3$BNF_4 <- str_sub(string = pres3$BNF_Code, end = 4)
length(unique(pres3$BNF_4)) # 146
# Frequency 6
BNF_6_freq <- pres3 %>%
  group_by(BNF_6) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))
# Frequency 4
BNF_4_freq <- pres3 %>%
  group_by(BNF_4) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

# Define morbidity categories and BNF codes
categories <- list(
  CVD = c("0201", "0202", "0203", "0204", "0205", "0206", "0208", "0209", "0212"),
  Stomach = c("0101","0103"),
  Intestine = c("0104", "0105", "0106", "0107"),
  Other_Abdominal = c("0102"),
  Hepatobiliary = c("0109"),
  Depression = c("0403","0401"),
  Neurological = c("0405", "0408", "0409", "0411"),
  Obesity = c("0405"),
  ADHD = c("0404"),
  Asthma_COPD = c("0301", "0302", "0303", "0304"),
  Other_Respiratory = c("0307", "0309", "0311"),
  Diabetes = c("0601"),
  Hyperthyroidism = c("0602"),
  Bone = c("0606"),
  Solid_cancers = c("0803", "0801", "0802"),
  Anemia = c("0901")
)
# Loop for new morbidity column
for (category in names(categories)) {
  pres3[[category]] <- ifelse(pres3$BNF_4 %in% categories[[category]], 1, 0)
}

result <- pres3 %>%
  filter(!is.na(Laboratory_result) & TestGroup == "No Test")
length(result$PROCHI) # 277
pres3 <- pres3 %>%
  mutate(TestGroup = case_when(
    !is.na(Laboratory_result) & TestGroup == "No Test" ~ "Test",
    TRUE ~ TestGroup
  ))

# Extracting the baseline
BNF_baseline <- pres3 %>%
  filter(baseline == 1)
length(unique(BNF_baseline$PROCHI)) # 5267 61.7%
# Extracting 2 years
BNF_2years <- pres3 %>%
  filter(two_years == 1)
length(unique(BNF_2years$PROCHI))# 5686 66.6%
# Count for every category
category_counts <- colSums(pres3[, names(categories)])
# Print category number
print(category_counts)

--------------------------------------------------------------------------------

# Part 2, Analysis baseline

### 
# All disease name
disease_cols <- c("CVD", "Stomach", "Intestine", "Other_Abdominal", 
                  "Hepatobiliary", "Depression", "Neurological", "Obesity", "ADHD", 
                  "Asthma_COPD", "Other_Respiratory", "Diabetes", "Hyperthyroidism", 
                  "Bone", "Solid_cancers", "Anemia")
# Pass all information to one row
BNF_baseline <- BNF_baseline %>%
  group_by(PROCHI) %>%
  mutate(across(all_of(disease_cols), ~ max(.x, na.rm = TRUE)))
BNF_baseline <- aggregate(BNF_baseline, list(BNF_baseline$PROCHI), FUN = head, 1, na.action = "na.pass")
colnames(BNF_baseline)
### Combine people baseline = 0
# Step 1: pick up pres3 baseline==0
baseline_zero <- pres3 %>%
  filter(baseline == 0 | is.na(baseline))
# Step 2: change baseline_zero's disease_cols as 0
baseline_zero <- baseline_zero %>%
  mutate(across(all_of(disease_cols), ~ 0))
length(baseline_zero$PROCHI)
# Step 3: De-emphasis make sure only one PROCHI
baseline_zero <- baseline_zero %>%
  group_by(PROCHI) %>%
  summarise(across(everything(), first)) %>%
  ungroup()
# Step 4: combine full data
Full_baseline <- bind_rows(BNF_baseline, baseline_zero)
length(Full_baseline$PROCHI)
### Delect duplicate PROCHI
# Step 1: across PROCHI and baseline sorting
Full_baseline <- Full_baseline %>%
  arrange(PROCHI, desc(baseline))
# Step 2: keep first PROCHI
Full_baseline <- Full_baseline %>%
  group_by(PROCHI) %>%
  slice(1) %>%
  ungroup()
length(Full_baseline$PROCHI) # 8532

## Create morbidity and number
Full_baseline$morbidity_count <- rowSums(Full_baseline[, disease_cols] > 0)
Full_baseline$multimorbidities <- ifelse(Full_baseline$morbidity_count > 1, 1, 0)

### Analysis for baseline
# Replace NA for baseline as 0
Full_baseline$baseline[is.na(Full_baseline$baseline)] <- 0
table(Full_baseline$baseline) # 0:3265, 1:5267
table(Full_baseline$TestGroup) #No Test 4249, Test 4283
sum(is.na(Full_baseline$TestGroup)) # 0
table(Full_baseline$Laboratory_result) # Negative  3841, Positive 422. Have 20 testgroup dont have result
sum(is.na(Full_baseline$Laboratory_result)) # 4269 is NA
table(Full_baseline$multimorbidities) # no 5019, with 3513

### Pick with morbidity
Full_base_with <- Full_baseline %>%
  filter(multimorbidities ==1 )
length(Full_base_with$PROCHI) # 3513
table(Full_base_with$TestGroup) # Test 1777, no test 1736
table(Full_base_with$Laboratory_result) # negative 1613, positive 152

### Pick without multimorbidity
Full_base_without <- Full_baseline %>%
  filter(multimorbidities == 0 )
length(Full_base_without$PROCHI) # 5019
table(Full_base_without$TestGroup) # Test 2506, no test 2513
table(Full_base_without$Laboratory_result) # negative 2228 positive 270
result <- Full_base_without %>%
  filter(!is.na(Laboratory_result) & TestGroup == "No Test")
length(result$PROCHI)
# Count for NA
na_counts <- colSums(is.na(Full_baseline))
na_counts
# Display multimorbidity distribution
prop.table(table(Full_baseline$multimorbidities)) # 0:58.8%, mul-mo:41.2%
# Draw a figure for number prop
prop_counts <- table(Full_baseline$morbidity_count)
total_counts <- length(Full_baseline$PROCHI)
proportions <- prop_counts / total_counts
proportions 
bar_centers <- barplot(proportions, names.arg = names(proportions), main = "Proportion of multimorbidities Counts at Baseline",
                       xlab = "Number of Comorbidities", ylab = "Proportion", las = 2,
                       ylim=c(0, max(proportions) + 0.1))  
text(x = bar_centers, y = proportions + 0.01, labels = paste0(round(proportions * 100, 1), "%"), pos = 3, cex = 0.8)
# morbidity count analysis
disease_counts_baseline <- colSums(Full_baseline[, disease_cols])
disease_counts_baseline
disease_proportions_baseline <- disease_counts_baseline / total_counts
disease_summary_baseline <- data.frame(Disease = names(disease_proportions_baseline), 
                                       Count = disease_counts_baseline, 
                                       Proportion = round(disease_proportions_baseline * 100, 2))
disease_summary_baseline


# Chi test for multimorbidity and Testgroup
table_com_group <- table(Full_baseline$multimorbidities, Full_baseline$TestGroup)
table_com_group
chisq.test(table_com_group) # X-squared = 0.32714, df = 1, p-value = 0.5673
# Chi for multimorbidity and blood test
table_com_blood <- table(Full_baseline$multimorbidities, Full_baseline$Laboratory_result)
table_com_blood
chisq.test(table_com_blood) # X-squared = 5.3521, df = 1, p-value = 0.0207
# Chi for com and age
Full_baseline$age_cat <- cut(Full_baseline$age, 
                             breaks=c(54, 59, 64, 69, Inf), 
                             labels=c("55-59", "60-64", "65-69", "70+"))
table_com_age <- table(Full_baseline$multimorbidities, Full_baseline$age_cat)
table_com_age
chisq.test(table_com_age) # X-squared = 50.491, df = 3, p-value = 6.28e-11
# Chi for com and pack year
table_com_pack <- table(Full_baseline$multimorbidities, Full_baseline$Pack_year_history_cat)
table_com_pack
chisq.test(table_com_pack) # X-squared = 48.02, df = 5, p-value = 3.519e-09
# Chi for com and gender
table_com_gender <- table(Full_baseline$multimorbidities, Full_baseline$Gender)
table_com_gender
chisq.test(table_com_gender) # X-squared = 37.975, df = 1, p-value = 7.166e-10
# Chi for com and staus
table_com_status <- table(Full_baseline$multimorbidities, Full_baseline$Smoking_status)
table_com_status
chisq.test(table_com_status) # X-squared = 4.5943, df = 1, p-value = 0.03208




# Chi for morbidity and Lab result
# travel through all disease
results <- list()  # initialise a list to store the results
outcome_variable <- "Laboratory_result"
for(disease in disease_cols) {
  # Create table for chi-analysis
  table <- table(Full_baseline[[disease]],Full_baseline[[outcome_variable]])
  test <- chisq.test(table)
  # Save result in list
  results[[disease]] <- list(
    p_value = test$p.value,
    statistic = test$statistic,
    table = table
  )
}
# Check the result
results

### Check disease and sub group
##
# All column name
group_cols <- c("TestGroup", "Laboratory_result", "age", "Pack_year_history_cat", "Gender", "Smoking_status")
# Create a table to store
results <- data.frame(Disease = character(), Group = character(), Chi_squared = numeric(), P_value = numeric(), stringsAsFactors = FALSE)

# Loop for chi-test
for (disease in disease_cols) {
  for (group in group_cols) {
    # Make sure no NA
    data_subset <- na.omit(Full_baseline[c(disease, group)])
    # Create table
    if (nrow(data_subset) > 0) {  # Make sure non-NA
      table_data <- table(data_subset[[disease]], data_subset[[group]])
      # Chi_test
      test_result <- chisq.test(table_data)
      # Collect result
      results <- rbind(results, data.frame(Disease = disease, Group = group, Chi_squared = test_result$statistic, P_value = test_result$p.value))
    } else {
      # IF subset is NA, record
      results <- rbind(results, data.frame(Disease = disease, Group = group, Chi_squared = NA, P_value = NA))
    }
  }
}
# Print result
print(results)




### Analysis of regression
##
#
baseline <- Full_baseline
baseline$test_result_binary <- ifelse(baseline$Laboratory_result == "Positive", 1, 0)
baseline$age_cat <- factor(baseline$age_cat)
baseline$Pack_year_history_cat <- factor(baseline$Pack_year_history_cat)
baseline$Gender <- factor(baseline$Gender)
baseline$Smoking_Status <- factor(baseline$Smoking_status)
all_vars <- c("age_cat", "Pack_year_history_cat", "Gender",  disease_cols)

for (column in all_vars) {
  if (is.factor(baseline[[column]])) {
    baseline[[column]] <- as.numeric(as.factor(baseline[[column]]))
  }
}

# Add all disease
formula_str <- paste("test_result_binary ~", paste(all_vars, collapse = " + "))
formula <- as.formula(formula_str)
baseline <- na.omit(baseline[, c("test_result_binary", all_vars)])
model <- glm(formula, data = baseline, family = binomial)
# Result
summary(model)


### People have morbidity
## Pick up people have morbidity
Full_baseline_with_com <- Full_baseline %>%
  filter(multimorbidities == 1)
table(Full_baseline_with_com$TestGroup) # No Test 2671, Test 2441
table(Full_baseline_with_com$Laboratory_result) # Negative 2410, Positive 249, 12 missing


### People not have morbidity
Full_baseline_without_com <- Full_baseline %>%
  filter(multimorbidities == 0)
table(Full_baseline_without_com$TestGroup) # No Test 1808, Test 2009
table(Full_baseline_without_com$Laboratory_result) # Negative 1785, Positive 216, 8 missing

### Pick up test group
Full_baseline_test <- Full_baseline %>%
  filter(TestGroup == "Test")

-----------------------------------------------------------------------------------------
####  Part 3, 2 years follow-up
  
# Pass all information to one row
BNF_2years <- BNF_2years %>%
group_by(PROCHI) %>%
mutate(across(all_of(disease_cols), ~ max(.x, na.rm = TRUE)))
BNF_2years <- aggregate(BNF_2years, list(BNF_2years$PROCHI), FUN = head, 1, na.action = "na.pass")
ength(BNF_2years$PROCHI) # 5686

### Combine people two_years = 0
# Step 1: pick up pres3 two_years==0
two_years_zero <- pres3 %>%
  filter(two_years == 0 | is.na(two_years))
# Step 2: change two_years_zero's disease_cols as 0
two_years_zero <- two_years_zero %>%
  mutate(across(all_of(disease_cols), ~ 0))
length(two_years_zero$PROCHI)
# Step 3: De-emphasis make sure only one PROCHI
two_years_zero <- two_years_zero %>%
  group_by(PROCHI) %>%
  summarise(across(everything(), first)) %>%
  ungroup()
# Step 4: combine full data
Full_two_years <- bind_rows(BNF_2years, two_years_zero)
length(Full_two_years$PROCHI) 
### Delect duplicate PROCHI
# Step 1: across PROCHI and two_years sorting
Full_two_years <- Full_two_years %>%
  arrange(PROCHI, desc(two_years))
# Step 2: keep first PROCHI
Full_two_years <- Full_two_years %>%
  group_by(PROCHI) %>%
  slice(1) %>%
  ungroup()
length(Full_two_years$PROCHI) # 8532

# Change NA two years as 0
Full_two_years$baseline[is.na(Full_two_years$baseline)] <- 0
## Create co-morbidity and number
Full_two_years$comorbidity_count <- rowSums(Full_two_years[, disease_cols] > 0)
Full_two_years$comorbidity <- ifelse(Full_two_years$comorbidity_count > 0, 1, 0)
table(Full_two_years$comorbidity) # no 3216, with 5316

# Pick up with morbidity
withcom_2 <- Full_two_years %>%
  filter(comorbidity == 1)
table(withcom_2$TestGroup) # no test 2665, test 2651
table(withcom_2$Laboratory_result) # negative 2395, positive 244
# Pick up with-out morbidity
withoutcom_2 <- Full_two_years %>%
  filter(comorbidity == 0)
table(withoutcom_2$TestGroup) # no test 1584, test 1632
table(withoutcom_2$Laboratory_result) # negative 1446, positive 178



# Display morbidity distribution
prop.table(table(Full_two_years$comorbidity)) # 37.69% dont have co-m, 62.31% have co-m

# Draw a figure for number prop
prop_counts <- table(Full_two_years$comorbidity_count)
total_counts <- length(Full_two_years$PROCHI)
proportions <- prop_counts / total_counts
proportions # Max number is 11
bar_centers <- barplot(proportions, names.arg = names(proportions), main = "Proportion of Comorbidity Counts at Two Years",
                       xlab = "Number of Comorbidities", ylab = "Proportion", las = 2,
                       ylim=c(0, max(proportions) + 0.1))  
text(x = bar_centers, y = proportions + 0.01, labels = paste0(round(proportions * 100, 1), "%"), pos = 3, cex = 0.8)
# morbidity count analysis
disease_counts_two_years <- colSums(Full_two_years[, disease_cols])
disease_counts_two_years
disease_proportions_two_years <- disease_counts_two_years / total_counts
disease_summary_two_years <- data.frame(Disease = names(disease_proportions_two_years), 
                                        Count = disease_counts_two_years, 
                                        Proportion = round(disease_proportions_two_years * 100, 2))
disease_summary_two_years
summary(Full_two_years$comorbidity_count)

table_com_group <- table(Full_baseline$multimorbidities, Full_baseline$TestGroup)
table_com_group
chisq.test(table_com_group) # X-squared = 0.32714, df = 1, p-value = 0.5673
# Chi for multimorbidity and blood test
table_com_blood <- table(Full_baseline$multimorbidities, Full_baseline$Laboratory_result)
table_com_blood
chisq.test(table_com_blood) # X-squared = 5.3521, df = 1, p-value = 0.0207
# Chi for com and age
Full_baseline$age_cat <- cut(Full_baseline$age, 
                             breaks=c(54, 59, 64, 69, Inf), 
                             labels=c("55-59", "60-64", "65-69", "70+"))
table_com_age <- table(Full_baseline$multimorbidities, Full_baseline$age_cat)
table_com_age
chisq.test(table_com_age) # X-squared = 50.491, df = 3, p-value = 6.28e-11
# Chi for com and pack year
table_com_pack <- table(Full_baseline$multimorbidities, Full_baseline$Pack_year_history_cat)
table_com_pack
chisq.test(table_com_pack) # X-squared = 48.02, df = 5, p-value = 3.519e-09
# Chi for com and gender
table_com_gender <- table(Full_baseline$multimorbidities, Full_baseline$Gender)
table_com_gender
chisq.test(table_com_gender) # X-squared = 37.975, df = 1, p-value = 7.166e-10
# Chi for com and staus
table_com_status <- table(Full_baseline$multimorbidities, Full_baseline$Smoking_status)
table_com_status
chisq.test(table_com_status) # X-squared = 4.5943, df = 1, p-value = 0.03208


# Chi for disease and Lab result
# travel through all disease
results <- list()  # initialise a list to store the results
outcome_variable <- "Laboratory_result"
for(disease in disease_cols) {
  # Create table for chi-analysis
  table <- table(Full_two_years[[disease]],Full_two_years[[outcome_variable]])
  test <- chisq.test(table)
  # Save result in list
  results[[disease]] <- list(
    p_value = test$p.value,
    statistic = test$statistic,
    table = table
  )
}
# Check the result
results



### Analysis of regression
##
#
Full_2 <- Full_two_years
Full_2$test_result_binary <- ifelse(Full_2$Laboratory_result == "Positive", 1, 0)
for (column in disease_cols) {
  if (is.numeric(Full_2[[column]]) && is.factor(Full_2[[column]])) {
    Full_2[[column]] <- as.numeric(as.factor(Full_2[[column]]))
  }
}
# Add all disease
formula_str <- paste("test_result_binary ~", paste(disease_cols, collapse = " + "))
formula <- as.formula(formula_str)
Full_2 <- na.omit(Full_2[, c("test_result_binary", disease_cols)])
model <- glm(formula, data = Full_2, family = binomial)
# Result
summary(model)




### People have morbidity
## Pick up people have morbidity
Full_two_years_with_com <- Full_two_years %>%
  filter(comorbidity == 1)
table(Full_two_years_with_com$TestGroup) # No Test 2671, Test 2441
table(Full_two_years_with_com$Laboratory_result) # Negative 2410, Positive 249, 12 missing


### People not have morbidity
Full_two_years_without_com <- Full_two_years %>%
  filter(comorbidity == 0)
table(Full_two_years_without_com$TestGroup) # No Test 1808, Test 2009
table(Full_two_years_without_com$Laboratory_result) # Negative 1785, Positive 216, 8 missing

### Pick up test group
Full_two_years_test <- Full_two_years %>%
  filter(TestGroup == "Test")

------------------------------------------------------------------------------------
### Part 4, the figure compares prop of morbidities and heatmap
# Add a new column, add date point
Full_baseline$time_point <- "Baseline"
Full_two_years$time_point <- "Two Years"

# Combine data set
combined_data <- rbind(Full_baseline, Full_two_years)

# Calculate percentage of every disease
combined_proportions <- combined_data %>%
  group_by(time_point, morbidity_count) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))
print(combined_proportions, n = 22)
combined_proportions

# draw plot
P <- ggplot(combined_proportions, aes(x = factor(morbidity_count), y = proportion, fill = time_point)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Morbidity Counts at Baseline and Two Years",
       x = "Number of Morbidities", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        legend.title = element_blank(),  
        legend.text = element_text(size = 12))
png(file = "ggplot_output_1.png", width = 800, height = 600)
print(P)
dev.off()


#  For baseline
full_baseline <- Full_baseline
# Define the disease columns as provided
disease_cols <- c("CVD", "Stomach", "Intestine", "Other_Abdominal", 
                  "Hepatobiliary", "Depression", "Neurological", "Obesity", "ADHD", 
                  "Asthma_COPD", "Other_Respiratory", "Diabetes", "Hyperthyroidism", 
                  "Bone", "Solid_cancers", "Anemia")



#### Heatmap for correlation between morbidities

## Baseline
# Check if all required disease columns are in the data
missing_cols <- setdiff(disease_cols, colnames(Full_baseline))
if(length(missing_cols) > 0) {
  stop(paste("The following columns are missing from the dataset:", paste(missing_cols, collapse = ", ")))
}
# Calculate the correlation matrix for the selected disease columns
corr_matrix <- cor(Full_baseline[, disease_cols], use = "complete.obs")

# Convert to data set
melted_corr_matrix <- melt(corr_matrix)

# Heatmap
N<-ggplot(data = melted_corr_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Pearson\nCorrelation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()


png(file = "ggplot_output_2.png", width = 800, height = 600)
print(N)
dev.off()




# Draw heatmap for Two years
two_years <- Full_two_years
# Check if all required disease columns are in the data
missing_cols <- setdiff(disease_cols, colnames(two_years))
if(length(missing_cols) > 0) {
  stop(paste("The following columns are missing from the dataset:", paste(missing_cols, collapse = ", ")))
}
#Calculate the correlation matrix for the selected disease columns
corr_matrix <- cor(Full_two_years[, disease_cols], use = "complete.obs")

# Convert to data set
melted_corr_matrix <- melt(corr_matrix)

# Heatmap
G<-ggplot(data = melted_corr_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Pearson\nCorrelation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()

png(file = "ggplot_output_4.png", width = 800, height = 600)
print(G)
dev.off()


-------------------------------------------------------------------------------

#### Part 5, calculate new and recover for disease
  
##
# Diseases column
  disease_cols <- c("CVD", "Stomach", "Intestine", "Other_Abdominal", "Hepatobiliary", 
                    "Depression", "Neurological", "Obesity", "ADHD", "Asthma_COPD", 
                    "Other_Respiratory", "Diabetes", "Hyperthyroidism", "Bone", 
                    "Solid_cancers", "Anemia")

# Add PROCHI
disease_cols <- c("PROCHI", disease_cols)

# Choose two column
baseline <- Full_baseline[, disease_cols]
two_years <- Full_two_years[, disease_cols]

# Convert to long format
baseline_long <- baseline %>%
  pivot_longer(cols = -PROCHI, names_to = "disease", values_to = "present_baseline") %>%
  filter(present_baseline == 1)

two_years_long <- two_years %>%
  pivot_longer(cols = -PROCHI, names_to = "disease", values_to = "present_two_years") %>%
  filter(present_two_years == 1)

# Merge two dataset
merged_df <- full_join(baseline_long, two_years_long, by = c("PROCHI", "disease"))

# Calculate unchange
same_disease <- merged_df %>%
  filter(!is.na(present_baseline) & !is.na(present_two_years)) %>%
  count(disease) %>%
  rename(unchanged = n)

# Calculate new
new_disease <- merged_df %>%
  filter(is.na(present_baseline) & !is.na(present_two_years)) %>%
  count(disease) %>%
  rename(new = n)

# Calculate disappear
disappeared_disease <- merged_df %>%
  filter(!is.na(present_baseline) & is.na(present_two_years)) %>%
  count(disease) %>%
  rename(disappeared = n)

# Merge result
result <- full_join(same_disease, new_disease, by = "disease") %>%
  full_join(disappeared_disease, by = "disease") %>%
  replace_na(list(unchanged = 0, new = 0, disappeared = 0))

print(result)


  
  
  
  
