# Notes
# exclude ked timeTaken prilis maly

# Script file to be sourced by the Rmarkdown manuscript
rm(list = ls())

# install required R libraries if not installed already
list.of.packages <- c("car", "tidyverse", "psych", "epitools", "plotly", "survey", "BayesFactor", "magrittr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required libraries
lapply(list.of.packages, require, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)

# define a color-blind-friendly palette for plotting
cbPalette <- c("#E69F00", "#999999", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#' prior width (r scale) for linear bayesian models
rScale <- sqrt(2)/4

# read in the data
data <- read_delim("data/data.csv", delim = ";", skip_empty_rows = TRUE)
# View(data)

# participant exclusions
data <- data[-85,] #89 nema vyplneny odbor

# some initial data recoding
data <- data %>% filter(!is.na(sexualOrientation)) %>% mutate(
  nonheterosexual = ifelse(sexualOrientation == "Heterosexuál/ka (osoba, ktorá je citovo a sexuálne priťahovaná opačným pohlavím)", 0, 1)) # 0 = "Heterosexuál/ka", 1 = "Non-heterosexuál/ka"

data <- data %>% mutate(ageOver23 = as.factor(case_when(age < 23 ~ 0, # 0 = '17-22', 1 = '23+'
                                             age >= 23  ~ 1)),
                        genderBinary = as.factor(case_when(gender == "Muž" ~ 0,
                                                 gender == "Žena" ~ 1)),
                        disclosureBinary = as.factor(case_when(disclosure == "Áno" ~ 1,
                                                               disclosure == "Nie" ~ 0)),
                        minority = as.factor(ifelse(rowSums(cbind(minority_ethnic,minority_immigrant, minority_religion, minority_disabled), na.rm = T) > 0, 1, 0)),
                        languageOther = ifelse(lng_slovak == 1, 0, 1),

                        believer =  as.factor(ifelse(religion %in% c("Katolícke", "Kresťanské ne-katolícke (napr. evanjelické)", "Židovské", "Pravoslávne"), 1, 0)), #Doplniť do kategórie veriaci 1 = "Veriaci", 0 = "Neveriaci"
                        anyYes = recode(anyYes, "Áno" = 1, "Nie" = 0),
                        investigation = recode(investigation, "Áno" = 1, "Nie" = 0))

data <- data %>% map_at(., .at = vars(contains("disclosure_")), .f =~recode(., "Neoslovil/a som ich" = 0,
                                                                          "Veľmi nespokojný/á s reakciou" = -2,
                                                                          "Skôr nespokojný/á s reakciou" = -1,
                                                                          "Skôr spokojný/á s reakciou" = 1,
                                                                          "Veľmi spokojný/á s reakciou" = 2)) %>% as.tibble()

data <- data %>% map_at(., .at = vars(contains("att")), .f =~recode(., "Určite nie" = 0,
                                                                            "Skôr nie" = 1,
                                                                            "Neviem" = 2,
                                                                            "Skôr áno" = 3,
                                                                            "Určite áno" = 4)) %>% as.tibble()

data <- data %>% map_at(., .at = vars(starts_with("know")), .f =~recode(., "Určite nie" = 0,
                                                                    "Skôr nie" = 1,
                                                                    "Neviem" = 2,
                                                                    "Skôr áno" = 3,
                                                                    "Určite áno" = 4)) %>% as.tibble()

data <- data %>% map_at(., .at = vars(starts_with("misconcept")), .f =~recode(., "Úplne nesúhlasím" = 0,
                                                                        "Skôr nesúhlasím" = 1,
                                                                        "Neviem" = 2,
                                                                        "Skôr súhlasím" = 3,
                                                                        "Úplne súhlasím" = 4)) %>% as.tibble()

data <- data %>% map_at(., .at = vars(c(misconcept2, misconcept3, misconcept4, misconcept11)), .f =~ 4-.) %>% as.tibble() # dealing with the inverse scaling of items

data <- data %>% mutate(satisfaction = rowSums(data %>% select(contains("disclosure_") & !contains(c("whyNo"))), na.rm = T))

data$facultyRegion <- as.factor(data$facultyRegion)
data$fieldStudy <- as.factor(data$fieldStudy)

# Sample description ------------------------------------------------------

# total N
ss_total_n <- nrow(data)

# region participant
ss_region_n <- data %>%
  group_by(factor(region)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# region of the faculty
ss_facultyRegion_n <- data %>%
  group_by(factor(facultyRegion)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# field of study
ss_fieldStudy_n <- data %>%
  group_by(factor(fieldStudy)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# age
ageRange <- 17:30
ss_age_plot <- ggplotly(data %>% filter(age %in% ageRange) %>%
                      ggplot(aes(age, fill = fct_rev(genderBinary))) + geom_histogram(binwidth=.5, bins = length(ageRange), position = position_stack(), alpha = .6) +
                      scale_x_continuous(name = "Vek", breaks=seq(17,30,1)) + labs(title = "Vek podľa pohlavia", y = "Počet", fill = "Pohlavie") +
                      scale_fill_manual(values=c(cbPalette[1], cbPalette[2])))

# age groups
ss_ageGroups_n <- data %>%
  group_by(factor(ageOver23)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# gender
genderLevels <- c("Žena", "Muž", "Transrodová osoba")
ss_gender_n <- data %>%
  group_by(factor(gender, levels = genderLevels)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# sexual orientation
sexOrienLevels <- c("Heterosexuál/ka (osoba, ktorá je citovo a sexuálne priťahovaná opačným pohlavím)",
                    "Bisexuál/ka (osoba, ktorá je citovo a sexuálne priťahovaná oboma pohlaviami)",
                    "Gej (muž, ktorý je citovo a sexuálne priťahovaný mužmi)",
                    "Lesba (žena, ktorá je citovo a sexuálne priťahovaná ženami)")
ss_sexOrien_n <- data %>%
  group_by(factor(sexualOrientation, levels = sexOrienLevels)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2)) %>%
  arrange(desc(n))

# year of study
data$yearStudy <- as.factor(as.numeric(gsub("([0-9]+).*$", "\\1", data$yearStudy)))
ss_yearStudy_plot <- ggplotly(data %>% filter(yearStudy %in% 1:7) %>%
                      ggplot(aes(yearStudy, fill = fct_rev(genderBinary))) + geom_bar(stat = "count", position = position_stack(), alpha = .6) +
                      scale_x_discrete(name = "Rok štúdia") + labs(title = "Rok štúdia", y = "Počet", fill = "Pohlavie") +
                      scale_fill_manual(values=c(cbPalette[1], cbPalette[2])))

# field of study by year
ss_fieldStudyYear_n <- data %>%
  group_by(yearStudy, factor(fieldStudy)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# field of study by region
ss_fieldStudyXregion_n <- data %>%
  group_by(facultyRegion, fieldStudy) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# Sampling weights --------------------------------------------------------

# population data
popCounts <- matrix(
c(12297,	4783,	4471,
3498,	500,	262,
3298,	2294,	3325,
13004,	2637,	3627,
18812,	6494,	6087,
7199,	2413,	4204), nrow = 6, ncol = 3, byrow = T,
dimnames = list(c("filozofický, humanitný, pedagogický, teologický", "mediálny, umelecký", "prírodovedecký", "spoločenský, ekonomický, právny", "technický", "zdravotnícky"),
                c("Západné Slovensko", "Stredné Slovensko", "Východné Slovensko")))

popProp <- as.data.frame(t(popCounts)/99205)
popProp <- cbind(gather(popProp), region = rep(c("Západné Slovensko", "Stredné Slovensko", "Východné Slovensko"), 3))
names(popProp) <- c("fieldStudy", "popProp", "facultyRegion")

# Compute and trim weights
data <- data %>% left_join(popProp, by = c("fieldStudy", "facultyRegion")) %>%
                  left_join(ss_fieldStudyXregion_n[-3], by = c("fieldStudy", "facultyRegion")) %>%
                  mutate(weight = ifelse(popProp/(percent/100) < .3,  .3, popProp/(percent/100)),
                         weight = ifelse(popProp/(percent/100) > 3,  3, popProp/(percent/100))) %>%
                  select(-c(popProp, percent))

# RQ1 Prevalence rates---------------------------------------------------------------------
# RQ1 Aký je celkový výskyt všetkých foriem sexuálneho obťažovania
data <- data %>% mutate_at(vars(starts_with("q") & !contains("_")), funs(recode(.,"Nikdy sa mi to nestalo" = 0, "Stalo sa mi to raz" = 1, "Stalo sa mi to opakovane" = 2)))

### RQ1.1 Individual forms
# frequency table (n's and %) for all individual types of abuse across the board
rq1.1_items_n <- data %>%
  summarise_at(vars(starts_with("q") & !contains("_")),
               funs(n = sum(., na.rm = TRUE),
                    perc = sum(., na.rm = TRUE)*100/n()))

# individual forms by gender
# frequency tables (n's and %) for all individual types of abuse by gender.
# Dropping other than female, male
rq1.1_itemsGender_n <- data %>%
  group_by(factor(genderBinary)) %>%
  summarise_at(vars(starts_with("q") & !contains("_")),
               funs(n = sum(., na.rm = TRUE),
                    perc = sum(., na.rm = TRUE)*100/n()))

### Frequencies by cluster
# RQ1.2 Aký je výskyt SO v prvom klastri otázok (rodové obťažovanie), (otázky 2-9)? Aký je výskyt SO v druhom klastri otázok (nechcená sexuálna pozornosť), (otázky 10 - 17)? Aký je výskyt SO v treťom klastri otázok (sexuálne donútenie/násilie), (otázky 18 - 21)?
# aggregate freuqencies
data <- data %>% mutate(
  genderMotivHarr = ifelse(rowSums(cbind(q1, q2, q3, q4, q5, q6, q7, q8), na.rm = T) >= 1, ifelse(rowSums(cbind(q1, q2, q3, q4, q5, q6, q7, q8), na.rm = T) >= 2, 2, 1), 0),
  unwantedSexAtt = ifelse(rowSums(cbind(q9, q10, q11, q12, q13, q14, q15, q16), na.rm = T) >= 1, ifelse(rowSums(cbind(q9, q10, q11, q12, q13, q14, q15, q16), na.rm = T) >= 2, 2, 1), 0),
  sexAbuse = ifelse(rowSums(cbind(q17, q18, q19, q20), na.rm = T) >= 1, ifelse(rowSums(cbind(q17, q18, q19, q20), na.rm = T) >= 2, 2, 1), 0),
  harrassed = ifelse(rowSums(cbind(genderMotivHarr, unwantedSexAtt, sexAbuse)) > 0, 1, 0),
  harrassedSeverity = case_when(sexAbuse == 2 ~ 6,
                                sexAbuse == 1 ~ 5,
                                unwantedSexAtt == 2 ~ 4,
                                unwantedSexAtt == 1 ~ 3,
                                genderMotivHarr == 2 ~ 2,
                                genderMotivHarr == 1 ~ 1),
  harrassedSeverity = ifelse(is.na(harrassedSeverity), 0, harrassedSeverity)
)

data$genderMotivHarr <- as.factor(data$genderMotivHarr)
data$unwantedSexAtt <- as.factor(data$unwantedSexAtt)
data$sexAbuse <- as.factor(data$sexAbuse)
data$harrassed <- as.factor(data$harrassed)


###
# frequency table (n's and %) for aggregate categories of abuse across the board
rq1.2_cluster_n <- data %>%
  summarise_at(vars(genderMotivHarr, unwantedSexAtt, sexAbuse),
               funs(n = sum(as.logical(.), na.rm = TRUE),
                    perc = sum(as.logical(.), na.rm = TRUE)*100/n()))

# frequency tables (n's and %) for aggregate categories of abuse by gender.
# dropping other than female, male
rq1.2_clusterGender_n <- data %>%
  group_by(factor(genderBinary)) %>%
  summarise_at(vars(genderMotivHarr, unwantedSexAtt, sexAbuse),
               funs(n = sum(as.logical(.), na.rm = TRUE),
                    perc = sum(as.logical(.), na.rm = TRUE)*100/n())) %>%
  na.omit()

# Computes the proportions (prevalence rates) and CIs of CSA forms in girls
rq1.2_female_ci <- data %>% filter(gender == "Žena") %>%  select(starts_with("q") & !contains("_"), genderMotivHarr, unwantedSexAtt, sexAbuse) %>%
map(~binom.exact(sum(as.logical(.),na.rm = TRUE), n = length(.)))

# Computes the proportions (prevalence rates) and CIs of CSA forms in boys
rq1.2_male_ci <- data %>% filter(gender == "Muž") %>%  select(starts_with("q") & !contains("_"), genderMotivHarr, unwantedSexAtt, sexAbuse) %>%
  map(~binom.exact(sum(as.logical(.),na.rm = TRUE), n = length(.)))

# Computes odds ratios ($measure) for: type of abuse by gender contingency tables.
rq1.2_clusterGender_or <- data %>% select(starts_with("q") & !contains("_"), genderMotivHarr, unwantedSexAtt, sexAbuse) %>%
  map(~riskratio.boot(table(data$genderBinary, .), replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by gender contingency tables.
rq1.2_clusterGender_bf <- data %>% select(starts_with("q") & !contains("_"), genderMotivHarr, unwantedSexAtt, sexAbuse) %>%
  map(~contingencyTableBF(table(data$genderBinary, .), sampleType = "poisson", priorConcentration = 1))

# RQ2 Risk factors ------------------------------------------------------------
# Existuje rozdiel vo výskyte SO vzhľadom na ... podľa klastrov?
# Otázky:
# Aký je pomer pohlaví vo vzťahu k riziku SO všeobecne?
# Sú niektorým formám SO vystavení častejšie muži alebo ženy?
# MALO DAT Je u transrodových osôb zvýšené riziko SO?
# Sú študenti / študentky niektorých odborov vystavení väčšiemu riziku SO než u iných študijných odborov
# Zvyšuje sa s dĺžkou štúdia aj riziko SO?
# Existuje zvýšené riziko SO s ohľadom na to, akým jazykom respondenti/respondentky hovoria?
# Existuje zvýšené riziko SO s ohľadom na to, aké je ich vierovyznanie?
# Existuje zvýšené riziko SO s ohľadom na príslušnosť k niektorej menšine (otázka 79) ?
# Existuje zvýšené riziko SO s ohľadom na príslušnosť s ohľadom na sexuálnu orientáciu? (oddelene gej, lesba, bi)

rfData <- data %>% select(genderBinary, nonheterosexual, minority, ageOver23, believer, languageOther, yearStudy, fieldStudy, facultyRegion)

# Computes the CIs of proportions for the risk factors
rq2_props_ci <- rfData %>% map(~binom.exact(table(.), n = length(.)))

# Computes odds ratios ($measure) for genderMotivHarr
rq2_gmh_or <- rfData %>% map(~riskratio.boot(table(., data$genderMotivHarr), replicates = 1e5))

# Computes odds ratios ($measure) for unwantedSexAtt
rq2_usa_or <- rfData %>% map(~riskratio.boot(table(., data$unwantedSexAtt), replicates = 1e5))

# Computes odds ratios ($measure) for sexAbuse
rq2_sab_or <- rfData %>% map(~riskratio.boot(table(., data$sexAbuse), replicates = 1e5))

# Bayes factors (Poisson BF10, BF01)  for: participant risk factors
# by genderMotivHarr contingency tables
rq2_gmh_bf <- rfData %>% map(~contingencyTableBF(table(., data$genderMotivHarr), sampleType = "poisson", priorConcentration = 1))

# Bayes factors (Poisson BF10, BF01)  for: participant risk factors
# by unwantedSexAtt contingency tables
rq2_usa_bf <- rfData %>% map(~contingencyTableBF(table(., data$unwantedSexAtt), sampleType = "poisson", priorConcentration = 1))

# Bayes factors (Poisson BF10, BF01)  for: participant risk factors
# by sexAbuse contingency tables
rq2_sab_bf <- rfData %>% map(~contingencyTableBF(table(., data$sexAbuse), sampleType = "poisson", priorConcentration = 1))

###

# Regions

# Computes odds ratios ($measure) for: type of abuse by region
rq2_region_or <- data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~riskratio.boot(table(data$facultyRegion, .), rev = "rows", replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by region
rq2_region_bf <- data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~contingencyTableBF(table(data$facultyRegion, .), sampleType = "poisson", priorConcentration = 1))

rq2_region_mod <- lm(harrassedSeverity ~ facultyRegion, weights = data$weight, data)
rq2_region_bf <- lmBF(harrassedSeverity ~ facultyRegion, data, rscaleEffects = rScale)
rq2_region_summary <- summary(rq2_region_mod)
rq2_region_anova <- anova(rq2_region_mod)


# Length of study

# Computes odds ratios ($measure) for: type of abuse by length of study
rq2_studyLengthMod <- lm(harrassedSeverity ~ yearStudy, weights = data$weight , data)
rq2_studyLengthBF <- lmBF(harrassedSeverity ~ yearStudy, data[!is.na(data$yearStudy),], rscaleEffects = rScale)
rq2_studyLengthSummary <- summary(rq2_studyLengthMod)
rq2_studyLengthAnova <- anova(rq2_studyLengthMod)


# Language
# Computes odds ratios ($measure) for: type of abuse by language.
rq2_language_or <- data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~riskratio.boot(table(data$languageOther, .), replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by language
rq2_language_bf <- data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~contingencyTableBF(table(data$languageOther, .), sampleType = "poisson", priorConcentration = 1))

rq2_language_mod <- lm(harrassedSeverity ~ languageOther, weights = data$weight , data)
rq2_language_bf <- lmBF(harrassedSeverity ~ languageOther, data[!is.na(data$languageOther),], rscaleEffects = rScale)
rq2_language_summary <- summary(rq2_language_mod)

# Religious belief

# Computes odds ratios ($measure) for: type of abuse by religious belief
rq2_belief_or <- data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~riskratio.boot(table(data$believer, .), replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by religious belief
rq2_belief_bf <- data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~contingencyTableBF(table(data$believer, .), sampleType = "poisson", priorConcentration = 1))

rq2_belief_mod <- lm(harrassedSeverity ~ believer, weights = data$weight , data)
rq2_belief_bf <- lmBF(harrassedSeverity ~ believer, data[!is.na(data$believer),], rscaleEffects = rScale)
rq2_belief_summary <- summary(rq2_belief_mod)


# Minority
# Computes odds ratios ($measure) for: type of abuse by minority status
rq2_minority_or <- data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~riskratio.boot(table(data$minority, .), replicates = 1e5))

rq2_minority_table <- table(data$genderMotivHarr)

# Computes Bayes factors (Poisson BF)  for: type of abuse by minority status
rq2_minority_bf <- data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~contingencyTableBF(table(data$minority, .), sampleType = "poisson", priorConcentration = 1))

rq2_minority_mod <- lm(harrassedSeverity ~ minority, weights = data$weight , data)
rq2_minority_bf <- lmBF(harrassedSeverity ~ minority, data[!is.na(data$minority),], rscaleEffects = rScale)
rq2_minority_summary <- summary(rq2_minority_mod)

# Sexual orientation
# Computes odds ratios ($measure) for: type of abuse by sexual orientation
rq2_orientation_or <- data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~riskratio.boot(table(data$nonheterosexual, .), replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by minority status
rq2_orientation_bf <- data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~contingencyTableBF(table(data$nonheterosexual, .), sampleType = "poisson", priorConcentration = 1))

rq2_orientation_mod <- lm(harrassedSeverity ~ nonheterosexual, weights = data$weight , data)
rq2_orientation_bf <- lmBF(harrassedSeverity ~ nonheterosexual, data[!is.na(data$nonheterosexual),], rscaleEffects = rScale)
rq2_orientation_summary <- summary(rq2_orientation_mod)


# RQ3 Who are the perpetrators---------------------------------------------------------------------
# Aký je pomer pohlaví páchateľov pri všetkých formách SO spolu?
# Aký je pomer pohlaví páchateľov pri jednotlivých formách SO osobitne?

# Overall count
# Kto sú páchatelia SO?
rq3_perpetratorsItems_n <- do.call(rbind.data.frame, data %>% select(contains("_who") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>%
  rownames_to_column("who")

whos <- c("Teacher_M", "Teacher_F", "Student_M", "Student_F", "Employee_M", "Employee_F")
rq3_perpetratorsOverall_n <- list()
for(n in 1:6){
  for(i in whos){
    rq3_perpetratorsOverall_n[[i]] <- rq3_perpetratorsItems_n %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}

# Count per cluster
# GenderMotivHarr
whoGenderMotivHarr <- do.call(rbind.data.frame, data %>% select(q1:q9 & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>% rownames_to_column("who")
rq3_perpetratorsGMH_n <- list(NA)
for(n in 1:6){
  for(i in whos){
    rq3_perpetratorsGMH_n[[i]] <- whoGenderMotivHarr %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq3_perpetratorsGMH_n[1] <- NULL

# UnwantedSexAtt
whoUnwantedSexAtt <- do.call(rbind.data.frame, data %>% select(q9:q17 & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>% rownames_to_column("who")
rq3_perpetratorsUSA_n <- list(NA)
for(n in 1:6){
  for(i in whos){
    rq3_perpetratorsUSA_n[[i]] <- whoUnwantedSexAtt %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq3_perpetratorsUSA_n[1] <- NULL

# SexAbuse
whoSexAbuse <- do.call(rbind.data.frame, data %>% select(q17:anyYes & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>% rownames_to_column("who")
rq3_perpetratorsSAB_n <- NA
for(n in 1:6){
  for(i in whos){
    rq3_perpetratorsSAB_n[[i]] <- whoSexAbuse %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq3_perpetratorsSAB_n[1] <- NULL

# Sú ženy alebo muži náchylnejší páchať niektorú z foriem SO?
# Aké osoby (učitelia, spolužiaci, atď.) najčastejšie páchajú aké druhy obťažovania?
# theoretical number of possible abuses overall
# !!! Inferential test assumes equal proportion of encounters with perpetrators (e.g., equal number of female and male teachers) !!!
possibleAbusesOverall <- nrow(data) * 20
possibleAbusesGMH <- nrow(data) * 8
possibleAbusesUSA <- nrow(data) * 8
possibleAbusesSAB <- nrow(data) * 4

abusesMaleOverall <- as_tibble(rq3_perpetratorsOverall_n)[,c(1, 3, 5)] %>% rowSums(.)
abusesFemaleOverall <- as_tibble(rq3_perpetratorsOverall_n)[,c(2, 4, 6)] %>% rowSums(.)

abusesMaleGMH <- as_tibble(rq3_perpetratorsGMH_n)[,c(1, 3, 5)] %>% rowSums(.)
abusesFemaleGMH <- as_tibble(rq3_perpetratorsGMH_n)[,c(2, 4, 6)] %>% rowSums(.)

abusesMaleUSA <- as_tibble(rq3_perpetratorsUSA_n)[,c(1, 3, 5)] %>% rowSums(.)
abusesFemaleUSA <- as_tibble(rq3_perpetratorsUSA_n)[,c(2, 4, 6)] %>% rowSums(.)

abusesMaleSAB <- as_tibble(rq3_perpetratorsSAB_n)[,c(1, 3, 5)] %>% rowSums(.)
abusesFemaleSAB <- as_tibble(rq3_perpetratorsSAB_n)[,c(2, 4, 6)] %>% rowSums(.)

abusesByPerpetratorGenderOverall <- matrix(c(
  abusesMaleOverall, possibleAbusesOverall - abusesMaleOverall,
  abusesFemaleOverall, possibleAbusesOverall - abusesFemaleOverall), nrow = 2, ncol = 2, byrow = T, dimnames = list(c("Male", "Female"), c("Abuses", "Non-abuses")))

abusesByPerpetratorGenderGMH <- matrix(c(
  abusesMaleGMH, possibleAbusesGMH - abusesMaleGMH,
  abusesFemaleGMH, possibleAbusesGMH - abusesFemaleGMH), nrow = 2, ncol = 2, byrow = T, dimnames = list(c("Male", "Female"), c("Abuses", "Non-abuses")))

abusesByPerpetratorGenderUSA <- matrix(c(
  abusesMaleUSA, possibleAbusesOverall - abusesMaleUSA,
  abusesFemaleUSA, possibleAbusesOverall - abusesFemaleUSA), nrow = 2, ncol = 2, byrow = T, dimnames = list(c("Male", "Female"), c("Abuses", "Non-abuses")))

abusesByPerpetratorGenderSAB <- matrix(c(
  abusesMaleSAB, possibleAbusesOverall - abusesMaleSAB,
  abusesFemaleSAB, possibleAbusesOverall - abusesFemaleSAB), nrow = 2, ncol = 2, byrow = T, dimnames = list(c("Male", "Female"), c("Abuses", "Non-abuses")))

# Computes odds ratios ($measure) and Bayes factors (Poisson BF) for: type of abuse by gender of the perpetrator overall
rq3_abusesByPerpetratorGenderOverall_or <- riskratio.boot(abusesByPerpetratorGenderOverall, rev = "both", replicates = 1e5)
rq3_abusesByPerpetratorGenderOverall_bf <- contingencyTableBF(abusesByPerpetratorGenderOverall, sampleType = "poisson", priorConcentration = 1)

# Computes odds ratios ($measure) and Bayes factors (Poisson BF) for: type of abuse by gender of the perpetrator overall
rq3_abusesByPerpetratorGenderGMH_or <- riskratio.boot(abusesByPerpetratorGenderGMH, rev = "both", replicates = 1e5)
rq3_abusesByPerpetratorGenderGMH_bf <- contingencyTableBF(abusesByPerpetratorGenderGMH, sampleType = "poisson", priorConcentration = 1)

# Computes odds ratios ($measure) and Bayes factors (Poisson BF) for: type of abuse by gender of the perpetrator overall
rq3_abusesByPerpetratorGenderUSA_or <- riskratio.boot(abusesByPerpetratorGenderUSA, rev = "both", replicates = 1e5, correction = TRUE)
rq3_abusesByPerpetratorGenderUSA_bf <- contingencyTableBF(abusesByPerpetratorGenderUSA, sampleType = "poisson", priorConcentration = 1)

# Computes odds ratios ($measure) and Bayes factors (Poisson BF) for: type of abuse by gender of the perpetrator overall
rq3_abusesByPerpetratorGenderSAB_or <- riskratio.boot(abusesByPerpetratorGenderSAB, rev = "both", replicates = 1e5, correction = TRUE)
rq3_abusesByPerpetratorGenderSAB_bf <- contingencyTableBF(abusesByPerpetratorGenderSAB, sampleType = "poisson", priorConcentration = 1)

# RQ4 Where did it happen -------------------------------------------------------------------

rq4_whereItems_n <- do.call(rbind.data.frame, data %>% select(contains("_where") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>%
  rownames_to_column("where")

wheres <- c("EduProcess", "Break", "Dorm", "Lab", "Practice", "Online", "DontKnow")

rq4_whereOverall_n <- list(NA)
for(n in 1:6){
  for(i in wheres){
    rq4_whereOverall_n[[i]] <- rq4_whereItems_n %>% filter(str_detect(where, i) & grepl(".1", where, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq4_whereOverall_n[1] <- NULL

# Existujú rozdiely medzi klastrami SO a prostredím v ktorom sa SO odohráva?
# Count per cluster
# GenderMotivHarr
whereGenderMotivHarr <- do.call(rbind.data.frame, data %>% select(q1:q9 & contains("_where") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>% rownames_to_column("where")
rq4_whereGMH_n <- list(NA)
for(n in 1:6){
  for(i in wheres){
    rq4_whereGMH_n[[i]] <- whereGenderMotivHarr %>% filter(str_detect(where, i) & grepl(".1", where, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq4_whereGMH_n[1] <- NULL

# UnwantedSexAtt
whereUnwantedSexAtt <- do.call(rbind.data.frame, data %>% select(q9:q17 & contains("_where") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>% rownames_to_column("where")
rq4_whereUSA_n <- list(NA)
for(n in 1:6){
  for(i in wheres){
    rq4_whereUSA_n[[i]] <- whereUnwantedSexAtt %>% filter(str_detect(where, i) & grepl(".1", where, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq4_whereUSA_n[1] <- NULL

# SexAbuse
whereSexAbuse <- do.call(rbind.data.frame, data %>% select(q17:anyYes & contains("_where") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>% rownames_to_column("where")
rq4_whereSAB_n <- NA
for(n in 1:6){
  for(i in wheres){
    rq4_whereSAB_n[[i]] <- whereSexAbuse %>% filter(str_detect(where, i) & grepl(".1", where, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq4_whereSAB_n[1] <- NULL


# RQ5 Abuse Impacts -----------------------------------------------------------
# Subjektívne vnímané dôsledky sexuálneho obťažovania
# RQ5.1 Aké psychické / psychosomatické dôsledky obete najčastejšie uvádzajú?
data <- data %>% mutate(impactSeverity = rowSums(data %>% select(contains("suffer_") & !contains(c("_other", "_none", "dontKnow"))), na.rm = T))

rq5_impacts_n <- do.call(rbind.data.frame, data %>% filter(harrassed == 1) %>% select(contains("suffer_") & !contains(c("_other", "_none", "dontKnow"))) %>%
                         map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>%
  rownames_to_column("impactType") %>%
  filter(grepl(".1", impactType, fixed = TRUE)) %>%
  arrange(desc(freq))


# Je rozsah uvádzaných dôsledkov podmienený zažitou formou SO?
rq5_harrassedByImpactSeverity_cor <- data %>% filter(anyYes == 1) %$% cor.test(harrassedSeverity, impactSeverity)
rq5_harrassedByImpactSeverity_bf <- data %>% filter(anyYes == 1) %$% correlationBF(y = harrassedSeverity, x = impactSeverity, rscale = rScale)

# Je rozsah uvádzaných dôsledkov podmienený tým, kto je páchateľom?
# compute frequencies of abuses per cluster and perpetrator

data$gmhTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(as.numeric(genderMotivHarr) > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$gmhStudentFreq <- data %>% transmute(gmhStudent = ifelse(as.numeric(genderMotivHarr) > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$gmhEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(as.numeric(genderMotivHarr) > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
#
data$usaTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(as.numeric(unwantedSexAtt) > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$usaStudentFreq <- data %>% transmute(gmhStudent = ifelse(as.numeric(unwantedSexAtt) > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$usaEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(as.numeric(unwantedSexAtt) > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
#
data$sabTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(as.numeric(sexAbuse) > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$sabStudentFreq <- data %>% transmute(gmhStudent = ifelse(as.numeric(sexAbuse) > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$sabEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(as.numeric(sexAbuse) > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
#

# determine the most frequent perpetrator per cluster
# GMH
data$mostFreqAbuserGMH <- data %>% select(gmhTeacherFreq, gmhStudentFreq, gmhEmployeeFreq) %$%
  colnames(.)[ifelse(data$genderMotivHarr == 0, NA, max.col(., ties.method="random"))]  %>% str_remove("gmh") %>% str_remove("Freq")
# USA
data$mostFreqAbuserUSA <- data %>% select(usaTeacherFreq, usaStudentFreq, usaEmployeeFreq) %$%
  colnames(.)[ifelse(data$unwantedSexAtt == 0, NA, max.col(., ties.method="random"))]  %>% str_remove("usa") %>% str_remove("Freq")
# SAB
data$mostFreqAbuserSAB <- data %>% select(sabTeacherFreq, sabStudentFreq, sabEmployeeFreq) %$%
  colnames(.)[ifelse(data$sexAbuse == 0, NA, max.col(., ties.method="random"))]  %>% str_remove("sab") %>% str_remove("Freq")

# identify the most frequent perpetrator committing the most severe type of abuse
data$perpetratorMostSevere <- data %$% ifelse(!is.na(mostFreqAbuserSAB), mostFreqAbuserSAB, ifelse(!is.na(mostFreqAbuserUSA), mostFreqAbuserUSA, ifelse(!is.na(mostFreqAbuserGMH), mostFreqAbuserGMH, NA)))

rq5_severPerpMod <- lm(impactSeverity ~ perpetratorMostSevere, weights = data$weight, data)
rq5_severPerpBF <- lmBF(impactSeverity ~ perpetratorMostSevere, data[!is.na(data$perpetratorMostSevere),], rscaleEffects = rScale)
rq5_severPerpSummary <- summary(rq5_severPerpMod)
rq5_severPerpAnova <- anova(rq5_severPerpMod)

# Existuje rozdiel medzi mužmi a ženami z hľadiska množstva / typu uvádzaných následkov?
rq5_severGenderMod <- data %>% filter(harrassed == 1) %$% lm(impactSeverity ~ genderBinary, weights = weight)
rq5_severGenderBF <- data %>% filter(harrassed == 1) %$% lmBF(impactSeverity ~ genderBinary, .[!is.na(genderBinary),], rscaleEffects = rScale)
rq5_severGenderSummary <- summary(rq5_severGenderMod)
rq5_severGenderAnova <- anova(rq5_severGenderMod)

# Je rozsah uvádzaných dôsledkov podmienený tým, či sa obete zdôverili (či hľadali podporu)? # ADRESOVANE V RQ7 V OPACNEJ DIREKTIVITE

# Je rozsah úvádzaných dôsledkov podmienený tým, či osoba zažila negatívne reakcie zo strany školy?
data <- data %>% mutate(consequencesSeverity = rowSums(data %>% select(contains("conseq_") & !contains(c("_other", "_none", "dontKnow"))), na.rm = T))

rq5_severConseqMod <- data %>% filter(harrassed == 1) %$% lm(impactSeverity ~ as.logical(consequencesSeverity), weights = weight)
rq5_severConseqBF <- data %>% filter(harrassed == 1) %>% mutate(consequencesSeverity = as.logical(consequencesSeverity)) %$% lmBF(impactSeverity ~ consequencesSeverity, .[!is.na(consequencesSeverity),], rscaleEffects = rScale)
rq5_severConseqSummary <- summary(rq5_severConseqMod)
rq5_severConseqAnova <- anova(rq5_severConseqMod)

# Je rozsah uvádzaných dôsledkov podmienený mierou spokojnosti obetí s tým, ako reagovali osoby, u ktorých hľadali pomoc/radu?
rq5_severSatisfactionMod <- data %>% filter(harrassed == 1) %$% lm(impactSeverity ~ satisfaction, weights = weight)
rq5_severSatisfactionBF <- data %>% filter(harrassed == 1) %$% lmBF(impactSeverity ~ satisfaction, .[!is.na(satisfaction),], rscaleEffects = rScale)
rq5_severSatisfactionSummary <- summary(rq5_severSatisfactionMod)
rq5_severSatisfactionAnova <- anova(rq5_severSatisfactionMod)

# Je rozsah uvádzaných dôsledkov podmienený príslušnosťou k niektorej menšine?
rq5_severMinorityMod <- data %>% filter(harrassed == 1) %$% lm(impactSeverity ~ minority, weights = weight)
rq5_severMinorityBF <- data %>% filter(harrassed == 1) %$% lmBF(impactSeverity ~ minority, .[!is.na(minority),], rscaleEffects = rScale)
rq5_severMinoritySummary <- summary(rq5_severMinorityMod)
rq5_severMinorityAnova <- anova(rq5_severMinorityMod)

# Je rozsah uvádzaných dôsledkov podmienený vierovyznaním obetí?
rq5_severBelieverMod <- data %>% filter(harrassed == 1) %$% lm(impactSeverity ~ believer, weights = weight)
rq5_severBelieverBF <- data %>% filter(harrassed == 1) %$% lmBF(impactSeverity ~ believer, .[!is.na(believer),], rscaleEffects = rScale)
rq5_severBelieverSummary <- summary(rq5_severBelieverMod)
rq5_severBelieverAnova <- anova(rq5_severBelieverMod)


# RQ6 Hladanie pomoci -----------------------------------------------------

# Koľko percent obetí vôbec hľadalo pomoc/radu?
rq6_disclosureN <- data %>%
  filter(harrassed == 1) %>%
  group_by(disclosureBinary) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# Koľko percent obetí vôbec hľadalo pomoc/radu?
# By gender
rq6_disclosureN <- data %>%
  filter(harrassed == 1) %>%
  group_by(disclosureBinary, genderBinary) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# U koho hľadali obete najčastejšie pomoc?
rq6_disclosureWhoFreq <- do.call(rbind.data.frame, data %>% select(contains("disclosure_") & !contains("whyNo")) %>% map(~cbind("freq" = table(as.logical(.)), "perc" = prop.table(table(as.logical(.)))))) %>% #Turning disclosure_ vars to logical; T = disclosed, F = not disclosed
  rownames_to_column("disclosureWho") %>% filter(grepl(".TRUE", disclosureWho, fixed = TRUE)) %>% arrange(desc(freq))

rq6_disclosureWhoFreq[,"disclosureWho"] <- rq6_disclosureWhoFreq[,"disclosureWho"] %>% str_remove("disclosure_") %>% str_remove(".TRUE")

# Existuje súvis medzi zažitou formou SO a tendenciou obetí hľadať pomoc? & Existuje súvis medzi častosťou (frekvenciou) SO a tendenciou obetí hľadať pomoc?
# Severity as predictor

rq6_severDisclosureMod0 <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ 1, family=binomial(), weights = weight)
rq6_severDisclosureMod <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ impactSeverity, family=binomial(), weights = weight)
rq6_severDisclosureSummary <- summary(rq6_severDisclosureMod)
rq6_severDisclosureAnova <- anova(rq6_severDisclosureMod)

rq6_severDisclosureBF10 <- 1/exp((BIC(rq6_severDisclosureMod) - BIC(rq6_severDisclosureMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
rq6_severDisclosurePosterior <- rq6_severDisclosureBF10/(1+rq6_severDisclosureBF10)
rq6_severDisclosureBayesOut <- data.frame(BF10 = round(rq6_severDisclosureBF10, 3), Posterior = round(rq6_severDisclosurePosterior, 3))

# Existuje rozdiel medzi pohlaviami v tendencii hľadať pomoc?
rq6_genderDisclosureOR <- data %$% riskratio.boot(table(genderBinary, disclosureBinary), replicates = 1e5)
rq6_genderDisclosureBF <- data %$% contingencyTableBF(table(genderBinary, disclosureBinary), sampleType = "poisson", priorConcentration = 1)

# Súvisí hľadanie pomoci s vierovyznaním obete?
rq6_believerDisclosureOR <- data %$% riskratio.boot(table(believer, disclosureBinary), replicates = 1e5)
rq6_believerDisclosureBF <- data %$% contingencyTableBF(table(believer, disclosureBinary), sampleType = "poisson", priorConcentration = 1)

# Súvisí hľadanie pomoci s odborom štúdia?
rq6_fieldDisclosureMod0 <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ 1, family=binomial(), weights = weight)
rq6_fieldDisclosureMod <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ fieldStudy, family=binomial(), weights = weight)
rq6_fieldDisclosureSummary <- summary(rq6_fieldDisclosureMod)
rq6_fieldDisclosureAnova <- anova(rq6_fieldDisclosureMod)

rq6_fieldDisclosureBF10 <- 1/exp((BIC(rq6_fieldDisclosureMod) - BIC(rq6_fieldDisclosureMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
rq6_fieldDisclosurePosterior <- rq6_fieldDisclosureBF10/(1+rq6_fieldDisclosureBF10)
rq6_fieldDisclosureBayesOut <- data.frame(BF10 = round(rq6_fieldDisclosureBF10, 3), Posterior = round(rq6_fieldDisclosurePosterior, 3))

# Súvisí hľadanie pomoci s regiónom, z ktorého obete pochádzajú?
rq6_regionDisclosureMod0 <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ 1, family=binomial(), weights = weight)
rq6_regionDisclosureMod <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ facultyRegion, family=binomial(), weights = weight)
rq6_regionDisclosureSummary <- summary(rq6_regionDisclosureMod)
rq6_regionDisclosureAnova <- anova(rq6_regionDisclosureMod)

rq6_regionDisclosureBF10 <- 1/exp((BIC(rq6_regionDisclosureMod) - BIC(rq6_regionDisclosureMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
rq6_regionDisclosurePosterior <- rq6_regionDisclosureBF10/(1+rq6_regionDisclosureBF10)
rq6_regionDisclosureBayesOut <- data.frame(BF10 = round(rq6_regionDisclosureBF10, 3), Posterior = round(rq6_regionDisclosurePosterior, 3))

# Súvisí hľadanie pomoci s povedomím o tom čo je SO?
# Compute sensitivity to harrassment by summing the att items
data <- data %>% mutate(sensitivityToHarrasment = rowSums(data %>% select(contains("att") & !contains(c("unwanted"))), na.rm = T))
rq6_sensitivityDisclosureMod0 <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ 1, family=binomial(), weights = weight)
rq6_sensitivityDisclosureMod <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ sensitivityToHarrasment, family=binomial(), weights = weight)
rq6_sensitivityDisclosureSummary <- summary(rq6_sensitivityDisclosureMod)
rq6_sensitivityDisclosureAnova <- anova(rq6_sensitivityDisclosureMod)

rq6_sensitivityDisclosureBF10 <- 1/exp((BIC(rq6_sensitivityDisclosureMod) - BIC(rq6_sensitivityDisclosureMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
rq6_sensitivityDisclosurePosterior <- rq6_sensitivityDisclosureBF10/(1+rq6_sensitivityDisclosureBF10)
rq6_sensitivityDisclosureBayesOut <- data.frame(BF10 = round(rq6_sensitivityDisclosureBF10, 3), Posterior = round(rq6_sensitivityDisclosurePosterior, 3))

# V akej miere boli obete spokojné s reakciou osôb, u ktorých hľadali pomoc?
rq6_satisfaction_n <- data %>% filter(satisfaction != 0) %>% summarise(meanSatisfaction = mean(satisfaction, na.rm = T),
                   sdSatisfaction = sd(satisfaction, na.rm = T))
rq6_satisfaction_table <- data %>% filter(satisfaction != 0) %$% table(satisfaction)


rq6_disclosureSatisfaction <- do.call(rbind.data.frame, data %>% select(contains("disclosure_") & !contains("whyNo")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>%
  rownames_to_column("disclosureWho") %>% filter(!grepl(".0", disclosureWho, fixed = TRUE)) %>% arrange(desc(freq))

# V koľkých percentách prípadov bolo spustené oficiálne konanie voči osobe, ktorá sa dopustila SO?
rq6_investigationN <- data %>%
  filter(harrassed == 1) %>%
  group_by(investigation) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# Existuje súvis medzi zažitou formou SO a spustením oficiálneho konania? & Existuje súvis medzi častosťou (frekvenciou) SO a spustením oficiálneho konania?
rq6_investigationMod0 <- data %>% filter(harrassed == 1) %$% glm(investigation ~ 1, family=binomial(), weights = weight)
rq6_investigationMod <- data %>% filter(harrassed == 1) %$% glm(investigation ~ harrassedSeverity, family=binomial(), weights = weight)
investigationSummary <- summary(rq6_investigationMod)
rq6_investigationAnova <- anova(rq6_investigationMod)

rq6_investigationBF10 <- 1/exp((BIC(rq6_investigationMod) - BIC(rq6_investigationMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
rq6_investigationPosterior <- rq6_investigationBF10/(1+rq6_investigationBF10)
rq6_investigationBayesOut <- data.frame(BF10 = round(rq6_investigationBF10, 3), Posterior = round(rq6_investigationPosterior, 3))

rq6_investigationHarrassed_cor <- data %$% cor(investigation, harrassedSeverity, use = "complete.obs")

# Aké sú dôvody, pre ktoré obete nehľadali pomoc

rq6_disclosureWhyNoFreq <- do.call(rbind.data.frame, data %>% select(contains("whyNo") & !contains("_other")) %>% map(~cbind("freq" = table(as.logical(.)), "perc" = prop.table(table(as.logical(.)))))) %>% #Turning disclosure_ vars to logical; T = disclosed, F = not disclosed
  rownames_to_column("disclosureWhyNo") %>% filter(grepl(".TRUE", disclosureWhyNo, fixed = TRUE)) %>% arrange(desc(freq))

rq6_disclosureWhyNoFreq[,"disclosureWhyNo"] <- rq6_disclosureWhyNoFreq[,"disclosureWhyNo"] %>% str_remove("whyNoDisclosure_") %>% str_remove(".TRUE")

# Je rozsah uvádzaných dôsledkov podmienený tým, kto je páchateľom?

# compute frequencies of abuses per cluster and perpetrator
data$gmhTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(as.numeric(genderMotivHarr) > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$gmhStudentFreq <- data %>% transmute(gmhStudent = ifelse(as.numeric(genderMotivHarr) > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$gmhEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(as.numeric(genderMotivHarr) > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
#
data$usaTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(as.numeric(unwantedSexAtt) > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$usaStudentFreq <- data %>% transmute(gmhStudent = ifelse(as.numeric(unwantedSexAtt) > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$usaEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(as.numeric(unwantedSexAtt) > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
#
data$sabTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(as.numeric(sexAbuse) > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$sabStudentFreq <- data %>% transmute(gmhStudent = ifelse(as.numeric(sexAbuse) > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$sabEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(as.numeric(sexAbuse) > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
#

# determine the most frequent perpetrator per cluster
# GMH
data$mostFreqAbuserGMH <- data %>% select(gmhTeacherFreq, gmhStudentFreq, gmhEmployeeFreq) %$%
  colnames(.)[ifelse(data$genderMotivHarr == 0, NA, max.col(., ties.method="random"))]  %>% str_remove("gmh") %>% str_remove("Freq")
# USA
data$mostFreqAbuserUSA <- data %>% select(usaTeacherFreq, usaStudentFreq, usaEmployeeFreq) %$%
  colnames(.)[ifelse(data$unwantedSexAtt == 0, NA, max.col(., ties.method="random"))]  %>% str_remove("usa") %>% str_remove("Freq")
# SAB
data$mostFreqAbuserSAB <- data %>% select(sabTeacherFreq, sabStudentFreq, sabEmployeeFreq) %$%
  colnames(.)[ifelse(data$sexAbuse == 0, NA, max.col(., ties.method="random"))]  %>% str_remove("sab") %>% str_remove("Freq")

# identify the most frequent perpetrator committing the most severe type of abuse
data$perpetratorMostSevere <- data %$% ifelse(!is.na(mostFreqAbuserSAB), mostFreqAbuserSAB, ifelse(!is.na(mostFreqAbuserUSA), mostFreqAbuserUSA, ifelse(!is.na(mostFreqAbuserGMH), mostFreqAbuserGMH, NA)))

rq6_severPerpMod <- lm(impactSeverity ~ perpetratorMostSevere, weights = data$weight, data)
rq6_severPerpBF <- lmBF(impactSeverity ~ perpetratorMostSevere, data[!is.na(data$perpetratorMostSevere),], rscaleEffects = rScale)
rq6_severPerpSummary <- summary(rq6_severPerpMod)
rq6_severPerpAnova <- anova(rq6_severPerpMod)

# RQ7 ---------------------------------------------------------------------
# Aké dôsledky spojené so zverejnením sťažnosti na pôde VŠ sú uvádzané najčastejšie?
rq7_investigation_n <- do.call(rbind.data.frame, data %>% select(contains("investigation_") & !contains("_other")) %>% map(~cbind("freq" = table(as.logical(.)), "perc" = prop.table(table(as.logical(.)))))) %>% #Turning disclosure_ vars to logical; T = disclosed, F = not disclosed
  rownames_to_column("investigationConsq") %>% filter(grepl(".TRUE", investigationConsq, fixed = TRUE)) %>% arrange(desc(freq))


# RQ8 ---------------------------------------------------------------------
# Vnímanie sexuálneho obťažovania (operacionalizovane ako senzitivita)
# Ktoré prejavy správania považujú respondenti/tky za SO?
rq8_attitudes_n <- data %>% select(starts_with("att") & !contains(c("unwanted"))) %>%
  map(~mean(.,na.rm = TRUE)) %>% as.tibble() %>% gather(key = "perception", value = "mean") %>% arrange(desc(mean))

# Existujú rozdiely s ohľadom na klastre foriem SO (1. rodové obťažovanie, 2. nechcená sexuálna pozornosť, 3. sexuálne donútenie/násilie) ?
# Tazko zodpovedatelne. Alternativa: vztah zavaznosti SO a senzitivity na obtazovanie

rq8_harrassedSensitivity_cor <-  data %$% cor.test(harrassedSeverity, sensitivityToHarrasment)
rq8_harrassedSensitivity_bf <- data %$% correlationBF(harrassedSeverity, sensitivityToHarrasment, rscale = rScale)

# Existujú rozdiely medzi pohlaviami?
rq8_genderSensitivityMod <- lm(sensitivityToHarrasment ~ genderBinary, weights = weight, data)
rq8_genderSensitivityBF <- lmBF(sensitivityToHarrasment ~ genderBinary, data[!is.na(data$genderBinary),], rscaleEffects = rScale)
rq8_genderSensitivitySummary <- summary(rq8_genderSensitivityMod)
rq8_genderSensitivityAnova <- anova(rq8_genderSensitivityMod)

# Existujú rozdiely v závislosti od veku respondentov/tiek?
rq8_ageSensitivityMod <- lm(sensitivityToHarrasment ~ age, weights = weight, data)
rq8_ageSensitivityBF <- lmBF(sensitivityToHarrasment ~ age, data[!is.na(data$age),], rscaleEffects = rScale)
rq8_ageSensitivitySummary <- summary(rq8_ageSensitivityMod)
rq8_ageSensitivityAnova <- anova(rq8_ageSensitivityMod)

# Existujú rozdiely v závislosti od odboru štúdia respondentov/tiek?
rq8_fieldSensitivityMod <- lm(sensitivityToHarrasment ~ fieldStudy, weights = weight, data)
rq8_fieldSensitivityBF <- lmBF(sensitivityToHarrasment ~ fieldStudy, data[!is.na(data$fieldStudy),], rscaleEffects = rScale)
rq8_fieldSensitivitySummary <- summary(rq8_fieldSensitivityMod)
rq8_fieldSensitivityAnova <- anova(rq8_fieldSensitivityMod)

rq8_fieldSensitivity_means <- data %>% group_by(fieldStudy) %>% summarise(meanSensitivity = mean(sensitivityToHarrasment, na.rm = T),
                                            sdSensitivity = sd(sensitivityToHarrasment, na.rm = T))

# Existujú rozdiely v závislosti od regiónov, z ktorých respondenti/tky pochádzajú?
rq8_regionSensitivityMod <- lm(sensitivityToHarrasment ~ facultyRegion, weights = weight, data)
rq8_regionSensitivityBF <- anovaBF(sensitivityToHarrasment ~ facultyRegion, data[!is.na(data$facultyRegion),], rscaleEffects = rScale)
rq8_regionSensitivitySummary <- summary(rq8_regionSensitivityMod)
rq8_regionSensitivityAnova <- anova(rq8_regionSensitivityMod)

rq8_regionSensitivity_means <- data %>% group_by(facultyRegion) %>% summarise(meanSensitivity = mean(sensitivityToHarrasment, na.rm = T),
                                            sdSensitivity = sd(sensitivityToHarrasment, na.rm = T))

# Existujú rozdiely v závislosti od vierovyznania respondentov/tiek?
rq8_believerSensitivityMod <- lm(sensitivityToHarrasment ~ believer, weights = weight, data)
rq8_believerSensitivityBF <- anovaBF(sensitivityToHarrasment ~ believer, data[!is.na(data$believer),], rscaleEffects = rScale)
rq8_believerSensitivitySummary <- summary(rq8_believerSensitivityMod)
rq8_believerSensitivityAnova <- anova(rq8_believerSensitivityMod)

rq8_believerSensitivity_means <- data %>% group_by(believer) %>% summarise(meanSensitivity = mean(sensitivityToHarrasment, na.rm = T),
                                               sdSensitivity = sd(sensitivityToHarrasment, na.rm = T))

# Existujú rozdiely medzi obeťami SO a osobami, ktoré nemajú osobnú skúsenosť s SO? (častý poznatok - osoby, ktoré majú viac skúseností so SO zvyknú aj viac situácií vyhodnocovať ako SO).
rq8_harrassedSensitivityMod <- lm(sensitivityToHarrasment ~ harrassed, weights = weight, data)
rq8_harrassedSensitivityBF <- anovaBF(sensitivityToHarrasment ~ harrassed, data[!is.na(data$harrassed),], rscaleEffects = rScale)
rq8_harrassedSensitivitySummary <- summary(rq8_harrassedSensitivityMod)
rq8_harrassedSensitivityAnova <- anova(rq8_harrassedSensitivityMod)

rq8_harrassedSensitivity_means <- data %>% group_by(harrassed) %>% summarise(meanSensitivity = mean(sensitivityToHarrasment, na.rm = T),
                                          sdSensitivity = sd(sensitivityToHarrasment, na.rm = T))

# Existuje súvis medzi tým, čo respondenti/tky považujú za SO a tým, či majú tendenciou hľadať pomoc, ak sa s niektorou z foriem SO osobne stretli?
# controlling for harrassedSeverity
rq8_disclosureSensitivityMod <- lm(sensitivityToHarrasment ~ disclosureBinary + harrassedSeverity, weights = weight, data)
rq8_disclosureSensitivityBF <- lmBF(sensitivityToHarrasment ~ disclosureBinary + harrassedSeverity, data[!is.na(data$disclosureBinary),], rscaleEffects = rScale)
rq8_disclosureSensitivitySummary <- summary(rq8_disclosureSensitivityMod)
rq8_disclosureSensitivityAnova <- anova(rq8_disclosureSensitivityMod)
rq8_disclosureSensitivity_means <- data %>% group_by(disclosureBinary) %>% summarise(meanSensitivity = mean(sensitivityToHarrasment, na.rm = T),
                                           sdSensitivity = sd(sensitivityToHarrasment, na.rm = T))

# Aké je právne povedomie o SO?
data <- data %>% mutate(legalAwareness = rowSums(cbind(know1, know2), na.rm = T))

rq8_legalAwareness_mean <- data %>%
  summarise(meanLegalAwareness = mean(legalAwareness, na.rm = TRUE),
            sdLegalAwareness = sd(legalAwareness, na.rm = TRUE))

# Existujú rozdiely medzi pohlaviami?
rq8_genderLegalAwarenessMod <- lm(legalAwareness ~ genderBinary, weights = weight, data)
rq8_genderLegalAwarenessBF <- lmBF(legalAwareness ~ genderBinary, data[!is.na(data$genderBinary) & !is.na(data$legalAwareness),], rscaleEffects = rScale)
rq8_genderLegalAwarenessSummary <- summary(rq8_genderLegalAwarenessMod)
rq8_genderLegalAwarenessAnova <- anova(rq8_genderLegalAwarenessMod)

rq8_genderLegalAwareness_means <- data %>% group_by(genderBinary) %>% summarise(meanLegalAwareness = mean(legalAwareness, na.rm = T),
                                                  sdLegalAwareness = sd(legalAwareness, na.rm = T))

# Existujú rozdiely v závislosti od regiónov, z ktorých respondenti/tky pochádzajú?
rq8_regionLegalAwarenessMod <- lm(legalAwareness ~ facultyRegion, weights = weight, data)
rq8_regionLegalAwarenessBF <- anovaBF(legalAwareness ~ facultyRegion, data[!is.na(data$facultyRegion) & !is.na(data$legalAwareness),], rscaleEffects = rScale)
rq8_regionLegalAwarenessSummary <- summary(rq8_regionLegalAwarenessMod)
rq8_regionLegalAwarenessAnova <- anova(rq8_regionLegalAwarenessMod)

# Existuje súvis medzi tým, čo respondenti/tky považujú za SO a tým, či majú tendenciou hľadať pomoc, ak sa s niektorou z foriem SO osobne stretli?
# controlling for harrassedSeverity
rq8_disclosureLegalAwarenessMod <- lm(legalAwareness ~ disclosureBinary + harrassedSeverity, weights = weight, data)
rq8_disclosureLegalAwarenessBF <- lmBF(legalAwareness ~ disclosureBinary + harrassedSeverity, data[!is.na(data$disclosureBinary),], rscaleEffects = rScale)
rq8_disclosureLegalAwarenessSummary <- summary(rq8_disclosureLegalAwarenessMod)
rq8_disclosureLegalAwarenessAnova <- anova(rq8_disclosureLegalAwarenessMod)

rq8_disclosureLegalAwareness_means <- data %>% group_by(disclosureBinary) %>% summarise(meanLegalAwareness = mean(legalAwareness, na.rm = T),
                                                  sdLegalAwareness = sd(legalAwareness, na.rm = T))

# RQ9 --------------------------------------------------------------------
# Poskytla im ich vysoká škola dostatok informácií o SO?
# dostatok informacii = odpoved skor ano alebo urcite ano
rq9_dostatokInformacii_n <- data %>%
  summarise(dostatokInformaciiN = sum(know3 > 2, na.rm = TRUE),
            dostatokInformaciiPerc = sum(know3 > 2, na.rm = TRUE)*100/n())

# Existujú rozdiely medzi pohlaviami?
rq9_genderInformationMod <- lm(know3 ~ genderBinary, weights = weight, data)
rq9_genderInformationBF <- lmBF(know3 ~ genderBinary, data[!is.na(data$genderBinary) & !is.na(data$know3),], rscaleEffects = rScale)
rq9_genderInformationSummary <- summary(rq9_genderInformationMod)
rq9_genderInformationAnova <- anova(rq9_genderInformationMod)

# Existujú rozdiely v závislosti od regiónov, z ktorých respondenti/tky pochádzajú?
rq9_regionInformationMod <- lm(know3 ~ facultyRegion, weights = weight, data)
rq9_regionInformationBF <- anovaBF(know3 ~ facultyRegion, data[!is.na(data$facultyRegion) & !is.na(data$know3),], rscaleEffects = rScale)
rq9_regionInformationSummary <- summary(rq9_regionInformationMod)
rq9_regionInformationAnova <- anova(rq9_regionInformationMod)

# Existujú rozdiely v závislosti od odboru štúdia respondentov/tiek?
rq9_fieldInformationMod <- lm(know3 ~ fieldStudy, weights = weight, data)
rq9_fieldInformationBF <- lmBF(know3 ~ fieldStudy, data[!is.na(data$fieldStudy) & !is.na(data$know3),], rscaleEffects = rScale)
rq9_fieldInformationSummary <- summary(rq9_fieldInformationMod)
rq9_fieldInformationAnova <- anova(rq9_fieldInformationMod)


# RQ10 --------------------------------------------------------------------

# Tvrdenia / stereotypy /  predsudky o sexuálnom obťažovaní
data <- data %>% mutate(misconceptScore = rowSums(data %>% select(starts_with("misconcept")), na.rm = T))

# V akej miere súhlasia respondenti/tky s jednotlivými tvrdeniami?
# min score = 0, max score 55. The higher the score the higher the agreement with misconceptions
rq.10_agreeMisconceptions <- data %>%
  summarise(meanAgreementMisconceptions = mean(misconceptScore, na.rm = TRUE),
            sdAgreementMisconceptions = sd(misconceptScore, na.rm = TRUE))

# Existujú rozdiely medzi pohlaviami?
rq.10_genderMisconceptionsMod <- lm(misconceptScore ~ genderBinary, weights = weight, data)
rq.10_genderMisconceptionsBF <- lmBF(misconceptScore ~ genderBinary, data[!is.na(data$genderBinary) & !is.na(data$misconceptScore),], rscaleEffects = rScale)
rq.10_genderMisconceptionsSummary <- summary(rq.10_genderMisconceptionsMod)
rq.10_genderMisconceptionsAnova <- anova(rq.10_genderMisconceptionsMod)

# Existujú rozdiely v závislosti od veku respondentov/tiek?
rq.10_ageMisconceptionsMod <- lm(misconceptScore ~ age, weights = weight, data)
rq.10_ageMisconceptionsBF <- lmBF(misconceptScore ~ age, data[!is.na(data$age),], rscaleEffects = rScale)
rq.10_ageMisconceptionsSummary <- summary(rq.10_ageMisconceptionsMod)
rq.10_ageMisconceptionsAnova <- anova(rq.10_ageMisconceptionsMod)

# Existujú rozdiely v závislosti od odboru štúdia respondentov/tiek?
rq.10_fieldMisconceptionsMod <- lm(misconceptScore ~ fieldStudy, weights = weight, data)
rq.10_fieldMisconceptionsBF <- lmBF(misconceptScore ~ fieldStudy, data[!is.na(data$fieldStudy) & !is.na(data$misconceptScore),], rscaleEffects = rScale)
rq.10_fieldMisconceptionsSummary <- summary(rq.10_fieldMisconceptionsMod)
rq.10_fieldMisconceptionsAnova <- anova(rq.10_fieldMisconceptionsMod)

# Existujú rozdiely v závislosti od regiónov, z ktorých respondenti/tky pochádzajú?
rq.10_regionMisconceptionsMod <- lm(misconceptScore ~ facultyRegion, weights = weight, data)
rq.10_regionMisconceptionsBF <- anovaBF(misconceptScore ~ facultyRegion, data[!is.na(data$facultyRegion) & !is.na(data$misconceptScore),], rscaleEffects = rScale)
rq.10_regionMisconceptionsSummary <- summary(rq.10_regionMisconceptionsMod)
rq.10_regionMisconceptionsAnova <- anova(rq.10_regionMisconceptionsMod)

# Existujú rozdiely v závislosti od vierovyznania respondentov/tiek?
rq.10_believerMisconceptionsMod <- lm(misconceptScore ~ believer, weights = weight, data)
rq.10_believerMisconceptionsBF <- anovaBF(misconceptScore ~ believer, data[!is.na(data$believer),], rscaleEffects = rScale)
rq.10_believerMisconceptionsSummary <- summary(rq.10_believerMisconceptionsMod)
rq.10_believerMisconceptionsAnova <- anova(rq.10_believerMisconceptionsMod)

rq.10_believerMisconceptions_means <- data %>% group_by(believer) %>% summarise(meanMisconceptions = mean(misconceptScore, na.rm = T),
                                          sdMisconceptions = sd(misconceptScore, na.rm = T))

# Existujú rozdiely medzi obeťami SO a osobami, ktoré nemajú osobnú skúsenosť s SO? (častý poznatok - osoby, ktoré majú viac skúseností so SO zvyknú aj viac situácií vyhodnocovať ako SO).
rq.10_harrassedMisconceptionsMod <- lm(misconceptScore ~ harrassed, weights = weight, data)
rq.10_harrassedMisconceptionsBF <- anovaBF(misconceptScore ~ harrassed, data[!is.na(data$harrassed),], rscaleEffects = rScale)
rq.10_harrassedMisconceptionsSummary <- summary(rq.10_harrassedMisconceptionsMod)
rq.10_harrassedMisconceptionsAnova <- anova(rq.10_harrassedMisconceptionsMod)

rq.10_harrassedMisconceptions_means <- data %>% group_by(harrassed) %>% summarise(meanMisconceptions = mean(misconceptScore, na.rm = T),
                                           sdMisconceptions = sd(misconceptScore, na.rm = T))

# Existuje súvis medzi tým, čo respondenti/tky považujú za SO a tým, či majú tendenciou hľadať pomoc, ak sa s niektorou z foriem SO osobne stretli?
# controlling for harrassedSeverity
rq.10_disclosureMisconceptionsMod <- lm(misconceptScore ~ disclosureBinary + harrassedSeverity, weights = weight, data)
rq.10_disclosureMisconceptionsBF <- lmBF(misconceptScore ~ disclosureBinary + harrassedSeverity, data[!is.na(data$disclosureBinary),], rscaleEffects = rScale)
rq.10_disclosureMisconceptionsSummary <- summary(rq.10_disclosureMisconceptionsMod)
rq.10_disclosureMisconceptionsAnova <- anova(rq.10_disclosureMisconceptionsMod)

rq.10_disclosureMisconceptions_means <- data %>% group_by(disclosureBinary) %>% summarise(meanMisconceptions = mean(misconceptScore, na.rm = T),
                                                  sdMisconceptions = sd(misconceptScore, na.rm = T))