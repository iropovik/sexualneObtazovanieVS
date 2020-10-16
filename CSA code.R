#R code for the paper "The prevalence of child sexual abuse in Slovak young adults"

# Data file
CSA <- read.csv(file = "Data.csv", header = TRUE, sep = ";")

# Required R packages
library(dplyr) # for data manipulation
library(epitools) # for odds ratios, proportions and CIs
library(BayesFactor) # for Bayes factors
library(car) # for recoding of response categories
library(stringr) # for counting multiple responses within a cell

##################### Table 1 #####################

# Recodes variables, resulting in less categories
CSA$age_recode<- Recode(CSA$age, "21:25 = '21+'")
CSA$siblings <- Recode(CSA$siblings, "2:5 = '2+'")
CSA$school_type <- Recode(CSA$school_type, "2:3 = '2'")
CSA$family_structure <- Recode(CSA$family_structure, "2:4 = '2'")
CSA$education_mother_Dichotomy <- Recode(CSA$education_mother, "1:2 = '1'")

# Computes the CIs for proportions for the freq table 1
lapply(CSA[,c("gender", "age_recode", "residence", "school_type", "education_mother",
              "education_father", "family_structure", "siblings")],
       function(x){(binom.exact(table(x), sum(!is.na(x))))})

##################### Table 2 #####################

# Creates variables needed to discern contact from non-contact forms of abuse
# Non-contact CSA
CSA$CSA_9nopen  <- CSA$CSA_9A %in% c(1,2)
CSA$CSA_10nopen  <- CSA$CSA_10A %in% c(1,2)
CSA$CSA_15nopen  <- CSA$CSA_15A %in% c(1,2)
CSA$CSA_11nopen  <- CSA$CSA_11A %in% 1
CSA$CSA_12nopen  <- CSA$CSA_12A %in% 1
CSA$CSA_13nopen  <- CSA$CSA_13A %in% 1
CSA$CSA_14nopen  <- CSA$CSA_14A %in% 1

# Contact CSA
CSA$CSA_11pen  <- CSA$CSA_11A %in% 2
CSA$CSA_12pen  <- CSA$CSA_12A %in% 2
CSA$CSA_13pen  <- CSA$CSA_13A %in% 2
CSA$CSA_14pen  <- CSA$CSA_14A %in% 2

# Displays the frequency tables (n's and %) for all types of abuse by gender.
freq.tab <- CSA %>%
  group_by(gender) %>%
  summarise_each(funs(n = sum(., na.rm = TRUE),
                      perc = sum(., na.rm = TRUE)*100/n()),
                 victim, CSA_without_contact, CSA_1A, CSA_2A, CSA_3A, CSA_4A, CSA_5A,
                 CSA_6A, CSA_7A, CSA_8A,
                 CSA_without_penetration, CSA_9nopen, CSA_10nopen, CSA_11nopen, CSA_12nopen,
                 CSA_13nopen, CSA_14nopen, CSA_15nopen,
                 CSA_with_penetration, CSA_11pen, CSA_12pen, CSA_13pen, CSA_14pen) %>%
  na.omit()
View(freq.tab)

# Computes the proportions (prevalence rates) and CIs of CSA forms in girls
lapply(CSA[,c("victim", "CSA_without_contact",
              "CSA_1A", "CSA_2A", "CSA_3A", "CSA_4A", "CSA_5A", "CSA_6A","CSA_7A", "CSA_8A",
              "CSA_without_penetration", "CSA_9nopen", "CSA_10nopen", "CSA_11nopen",
              "CSA_12nopen", "CSA_13nopen", "CSA_14nopen", "CSA_15nopen",
              "CSA_with_penetration", "CSA_11pen", "CSA_12pen", "CSA_13pen", "CSA_14pen")],
       function(x){binom.exact(sum(x[CSA$gender == 1], na.rm = TRUE),
                               sum(CSA$gender == 1, na.rm = TRUE))})

# Computes the proportions (prevalence rates) and CIs of CSA forms in boys
lapply(CSA[,c("victim", "CSA_without_contact",
              "CSA_1A", "CSA_2A", "CSA_3A", "CSA_4A", "CSA_5A", "CSA_6A","CSA_7A", "CSA_8A",
              "CSA_without_penetration", "CSA_9nopen", "CSA_10nopen", "CSA_11nopen",
              "CSA_12nopen", "CSA_13nopen", "CSA_14nopen", "CSA_15nopen",
              "CSA_with_penetration", "CSA_11pen", "CSA_12pen", "CSA_13pen", "CSA_14pen")],
       function(x){binom.exact(sum(x[CSA$gender == 2], na.rm = TRUE),
                               sum(CSA$gender == 2, na.rm = TRUE))})

# Computes odds ratios ($measure) for: type of abuse by gender contingency tables.
lapply(CSA[,c("victim", "CSA_without_contact",
              "CSA_1A", "CSA_2A", "CSA_3A", "CSA_4A", "CSA_5A", "CSA_6A","CSA_7A", "CSA_8A",
              "CSA_without_penetration", "CSA_9nopen", "CSA_10nopen", "CSA_11nopen",
              "CSA_12nopen", "CSA_13nopen", "CSA_14nopen", "CSA_15nopen",
              "CSA_with_penetration", "CSA_11pen", "CSA_12pen", "CSA_13pen", "CSA_14pen")],
       function(x){
         riskratio.boot(table(CSA$gender, x), rev = "rows", replicates = 1e6)$measure})

# Computes Bayes factors (Poisson BF)  for: type of abuse by gender contingency tables.
lapply(CSA[,c("victim", "CSA_without_contact",
              "CSA_1A", "CSA_2A", "CSA_3A", "CSA_4A", "CSA_5A", "CSA_6A","CSA_7A", "CSA_8A",
              "CSA_without_penetration", "CSA_9nopen", "CSA_10nopen", "CSA_11nopen",
              "CSA_12nopen", "CSA_13nopen", "CSA_14nopen", "CSA_15nopen",
              "CSA_with_penetration", "CSA_11pen", "CSA_12pen", "CSA_13pen", "CSA_14pen")],
       function(x){
         contingencyTableBF(x = table(CSA$gender, x), sampleType = "poisson",
                            priorConcentration = 1)})

# Computes p-values for: type of abuse by gender contingency tables.
lapply(CSA[,c("victim", "CSA_without_contact",
              "CSA_1A", "CSA_2A", "CSA_3A", "CSA_4A", "CSA_5A", "CSA_6A","CSA_7A", "CSA_8A",
              "CSA_without_penetration", "CSA_9nopen", "CSA_10nopen", "CSA_11nopen",
              "CSA_12nopen", "CSA_13nopen", "CSA_14nopen", "CSA_15nopen",
              "CSA_with_penetration", "CSA_11pen", "CSA_12pen", "CSA_13pen", "CSA_14pen")],
       function(x){
         chisq.test(x = table(CSA$gender, x), correct = FALSE)})

##################### Table 4 #####################

# use rev = "rows", or rev = "neither" to reverse order categories in inverse-coded items

# Computes odds ratios for CSA characteristics - CSA without contact
lapply(CSA[,c("gender", "residence", "school_type","education_mother",
              "education_father", "family_structure", "siblings")],
       function(x){riskratio.boot(table(x, CSA$CSA_without_contact),
                             replicates = 1e6, rev = "rows")$measure})

# Computes odds ratios for CSA characteristics - CSA without penetration
lapply(CSA[,c("gender", "residence", "school_type","education_mother",
              "education_father", "family_structure", "siblings")],
       function(x){riskratio.boot(table(x, CSA$CSA_without_penetration),
                             replicates = 1e6, rev = "rows")$measure})

# Computes odds ratios for CSA characteristics - CSA with penetration
lapply(CSA[,c("gender", "residence", "school_type","education_mother",
              "education_father", "family_structure", "siblings")],
       function(x){riskratio.boot(table(x, CSA$CSA_with_penetration),
                             replicates = 1e6, rev = "neither")$measure})

# Bayes factors (Poisson BF10, BF01)  for: sociodemographic characteristics
# by CSA without physical contact contingency tables
lapply(CSA[,c("gender", "residence", "school_type","education_mother",
              "education_father",
              "family_structure", "siblings")],
       function(x){
         contingencyTableBF(x = table(x, CSA$CSA_without_contact), sampleType = "poisson",
                            priorConcentration = 1)})

# Bayes factors (Poisson BF10, BF01)  for: sociodemographic characteristics
# by CSA with physical contact without penetration contingency tables
lapply(CSA[,c("gender", "residence", "school_type","education_mother",
              "education_father",
              "family_structure", "siblings")],
       function(x){
         contingencyTableBF(x = table(x, CSA$CSA_without_penetration), sampleType = "poisson",
                            priorConcentration = 1)})

# Bayes factors (Poisson BF10, BF01)  for: sociodemographic characteristics
# by CSA with physical contact with penetration contingency tables
lapply(CSA[,c("gender", "residence", "school_type","education_mother",
              "education_father",
              "family_structure", "siblings")],
       function(x){
         contingencyTableBF(x = table(x, CSA$CSA_with_penetration), sampleType = "poisson",
                            priorConcentration = 1)})

##################### Table 3 #####################

# Recodes 12 categories of relationship to the perpetrator to 4 categories
CSA$perpetrator_relationship_4cat <- Recode(CSA$perpetrator_relationship,
"1:5 = 'Family member'; 7 = 'Partner';  c(6, 8, 9) = 'Acquaintance'; c(7, 10, 11, 12) = 'Stranger'")

# Computes the proportions (prevalence rates) and CIs for the freq tables in girls
# CSA without contact
lapply(CSA[,c("age_first_CSA", "perpetrator_age", "perpetrator_gender",
              "perpetrator_relationship_4cat", "disclosure", "police")],
       function(x){binom.exact(table(x[CSA$gender == 1 & CSA$CSA_without_contact == 1]),
                               sum(CSA$gender == 1 & CSA$CSA_without_contact == 1, na.rm = TRUE))})

# Computes the proportions (prevalence rates) and CIs for the freq tables in boys
# CSA without contact
lapply(CSA[,c("age_first_CSA", "perpetrator_age", "perpetrator_gender",
              "perpetrator_relationship_4cat", "disclosure", "police")],
       function(x){binom.exact(table(x[CSA$gender == 2 & CSA$CSA_without_contact == 1]),
                               sum(CSA$gender == 2 & CSA$CSA_without_contact == 1, na.rm = TRUE))})

# Computes the proportions (prevalence rates) and CIs for the freq tables in girls
# CSA without penetration
lapply(CSA[,c("age_first_CSA", "perpetrator_age", "perpetrator_gender",
              "perpetrator_relationship_4cat", "disclosure", "police")],
       function(x){binom.exact(table(x[CSA$gender == 1 & CSA$CSA_without_penetration == 1]),
                               sum(CSA$gender == 1 & CSA$CSA_without_penetration == 1, na.rm = TRUE))})

# Computes the proportions (prevalence rates) and CIs for the freq tables in boys
# CSA without penetration
lapply(CSA[,c("age_first_CSA", "perpetrator_age", "perpetrator_gender",
              "perpetrator_relationship_4cat", "disclosure", "police")],
       function(x){binom.exact(table(x[CSA$gender == 2 & CSA$CSA_without_penetration == 1]),
                               sum(CSA$gender == 2 & CSA$CSA_without_penetration == 1, na.rm = TRUE))})

# Computes the proportions (prevalence rates) and CIs for the freq tables in girls
# CSA with penetration
lapply(CSA[,c("age_first_CSA", "perpetrator_age", "perpetrator_gender",
              "perpetrator_relationship_4cat", "disclosure", "police")],
       function(x){binom.exact(table(x[CSA$gender == 1 & CSA$CSA_with_penetration == 1]),
                               sum(CSA$gender == 1 & CSA$CSA_with_penetration == 1, na.rm = TRUE))})

# Computes the proportions (prevalence rates) and CIs for the freq tables in boys
# CSA with penetration
lapply(CSA[,c("age_first_CSA", "perpetrator_age", "perpetrator_gender",
              "perpetrator_relationship_4cat", "disclosure", "police")],
       function(x){binom.exact(table(x[CSA$gender == 2 & CSA$CSA_with_penetration == 1]),
                               sum(CSA$gender == 2 & CSA$CSA_with_penetration == 1, na.rm = TRUE))})

#####################
# Analyses of variables with more than one answer allowed

# Computes the proportions (prevalence rates) and the CIs for
# place_CSA_6cat and CSA without contact in girls
lapply(c("1","2","3","4","5","6"),
       function(x){binom.exact(x =  sum(str_count(CSA$place_CSA_6cat, x)[
         CSA$CSA_without_contact == 1 & CSA$gender == 1], na.rm = TRUE),
         n =  sum((CSA$CSA_without_contact == 1 & CSA$gender == 1), na.rm = TRUE))})

# Computes the proportions (prevalence rates) and the CIs for
# place_CSA_6cat and CSA without contact in boys
lapply(c("1","2","3","4","5","6"),
       function(x){binom.exact(x =  sum(str_count(CSA$place_CSA_6cat, x)[
         CSA$CSA_without_contact == 1 & CSA$gender == 2], na.rm = TRUE),
         n =  sum((CSA$CSA_without_contact == 1 & CSA$gender == 2), na.rm = TRUE))})

# Computes the proportions (prevalence rates) and the CIs for
# place_CSA_6cat and CSA without penetration in girls
lapply(c("1","2","3","4","5","6"),
       function(x){binom.exact(x =  sum(str_count(CSA$place_CSA_6cat, x)[
         CSA$CSA_without_penetration == 1 & CSA$gender == 1], na.rm = TRUE),
         n =  sum((CSA$CSA_without_penetration == 1 & CSA$gender == 1), na.rm = TRUE))})

# Computes the proportions (prevalence rates) and the CIs for
# place_CSA_6cat and CSA without penetration in boys
lapply(c("1","2","3","4","5","6"),
       function(x){binom.exact(x =  sum(str_count(CSA$place_CSA_6cat, x)[
         CSA$CSA_without_penetration == 1 & CSA$gender == 2], na.rm = TRUE),
         n =  sum((CSA$CSA_without_penetration == 1 & CSA$gender == 2), na.rm = TRUE))})

# Computes the proportions (prevalence rates) and the CIs for
# place_CSA_6cat and CSA with penetration in girls
lapply(c("1","2","3","4","5","6"),
       function(x){binom.exact(x =  sum(str_count(CSA$place_CSA_6cat, x)[
         CSA$CSA_with_penetration == 1 & CSA$gender == 1], na.rm = TRUE),
         n =  sum((CSA$CSA_with_penetration == 1 & CSA$gender == 1), na.rm = TRUE))})

# Computes the proportions (prevalence rates) and the CIs for
# place_CSA_6cat and CSA with penetration contact in boys
lapply(c("1","2","3","4","5","6"),
       function(x){binom.exact(x =  sum(str_count(CSA$place_CSA_6cat, x)[
         CSA$CSA_with_penetration == 1 & CSA$gender == 2], na.rm = TRUE),
         n =  sum((CSA$CSA_with_penetration == 1 & CSA$gender == 2), na.rm = TRUE))})

#####################

# Computes the proportions (prevalence rates) and the CIs for
# to_whom_disclosed_4cat and CSA without contact in girls
lapply(c("1","2","3","4"),
       function(x){binom.exact(x =  sum(str_count(CSA$to_whom_disclosed_4cat, x)[
         CSA$CSA_without_contact == 1 & CSA$gender == 1], na.rm = TRUE),
         n =  sum((CSA$CSA_without_contact == 1 & CSA$gender == 1), na.rm = TRUE))})

# Computes the proportions (prevalence rates) and the CIs for
# to_whom_disclosed_4cat and CSA without contact in boys
lapply(c("1","2","3","4"),
       function(x){binom.exact(x =  sum(str_count(CSA$to_whom_disclosed_4cat, x)[
         CSA$CSA_without_contact == 1 & CSA$gender == 2], na.rm = TRUE),
         n =  sum((CSA$CSA_without_contact == 1 & CSA$gender == 2), na.rm = TRUE))})

# Computes the proportions (prevalence rates) and the CIs for
# to_whom_disclosed_4cat and CSA without penetration in girls
lapply(c("1","2","3","4"),
       function(x){binom.exact(x =  sum(str_count(CSA$to_whom_disclosed_4cat, x)[
         CSA$CSA_without_penetration == 1 & CSA$gender == 1], na.rm = TRUE),
         n =  sum((CSA$CSA_without_penetration == 1 & CSA$gender == 1), na.rm = TRUE))})

# Computes the proportions (prevalence rates) and the CIs for
# to_whom_disclosed_4cat and CSA without penetration in boys
lapply(c("1","2","3","4"),
       function(x){binom.exact(x =  sum(str_count(CSA$to_whom_disclosed_4cat, x)[
         CSA$CSA_without_penetration == 1 & CSA$gender == 2], na.rm = TRUE),
         n =  sum((CSA$CSA_without_penetration == 1 & CSA$gender == 2), na.rm = TRUE))})

# Computes the proportions (prevalence rates) and the CIs for
# to_whom_disclosed_4cat and CSA with penetration in girls
lapply(c("1","2","3","4"),
       function(x){binom.exact(x =  sum(str_count(CSA$to_whom_disclosed_4cat, x)[
         CSA$CSA_with_penetration == 1 & CSA$gender == 1], na.rm = TRUE),
         n =  sum((CSA$CSA_with_penetration == 1 & CSA$gender == 1), na.rm = TRUE))})

# Computes the proportions (prevalence rates) and the CIs for
# to_whom_disclosed_4cat and CSA with penetration in boys
lapply(c("1","2","3","4"),
       function(x){binom.exact(x =  sum(str_count(CSA$to_whom_disclosed_4cat, x)[
         CSA$CSA_with_penetration == 1 & CSA$gender == 2], na.rm = TRUE),
         n =  sum((CSA$CSA_with_penetration == 1 & CSA$gender == 2), na.rm = TRUE))})

##################### Revictimisation #####################

# Computes the proportion of revictimization of CSA without contact
table(CSA$CSA_without_contact)
CSA %>%
  filter(CSA_without_contact == 1) %>%
  summarize(
    n_revictimization = sum(CSA$CSA_1B > 1 | CSA$CSA_2B > 1 | CSA$CSA_3B > 1 | CSA$CSA_4B > 1
    | CSA$CSA_5B > 1 | CSA$CSA_6B > 1 | CSA$CSA_7B > 1 | CSA$CSA_8B > 1, na.rm = TRUE),
    percent = n_revictimization/n(),
    n_more_than_5 = sum(CSA$CSA_1B == 3 | CSA$CSA_2B == 3 | CSA$CSA_3B == 3 | CSA$CSA_4B == 3
                        | CSA$CSA_5B == 3 | CSA$CSA_6B == 3 | CSA$CSA_7B == 3 | CSA$CSA_8B == 3, na.rm = TRUE),
    percent_more_than_5 = n_more_than_5/n())

# Computes the proportion of revictimization of CSA without contact without penetration
table(CSA$CSA_without_penetration)
CSA %>%
  filter(CSA_without_penetration == 1) %>%
  summarize(
    n_revictimization = sum(CSA_9B > 1 | CSA_10B > 1 | CSA_11B > 1 | CSA_12B > 1
    | CSA_13B > 1 | CSA_14B > 1 | CSA_15B > 1, na.rm = TRUE),
    percent = n_revictimization/n(),
    n_more_than_5 = sum(CSA_9B == 3 | CSA_10B == 3 | CSA_11B == 3 | CSA_12B == 3
                            | CSA_13B == 3 | CSA_14B == 3 | CSA_15B == 3, na.rm = TRUE),
    percent_more_than_5 = n_more_than_5/n())

# Computes the proportion of revictimization of CSA without contact with penetration
table(CSA$CSA_with_penetration)
CSA %>%
  filter(CSA_with_penetration == 1) %>%
  summarize(
    n_revictimization = sum(CSA_11B > 1 | CSA_12B > 1 | CSA_13B > 1 | CSA_14B > 1, na.rm = TRUE),
    percent = n_revictimization/n(),
    n_more_than_5 = sum(CSA_11B == 3 | CSA_12B == 3 | CSA_13B == 3 | CSA_14B == 3, na.rm = TRUE),
    percent_more_than_5 = n_more_than_5/n())

