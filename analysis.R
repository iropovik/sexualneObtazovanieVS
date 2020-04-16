# Notes
# exclude ked timeTaken prilis maly
# check rows collumns both v contingency tables


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

# Data recoding
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


data$facultyRegion <- as.factor(data$facultyRegion)
data$fieldStudy <- as.factor(data$fieldStudy)

# Sample description ------------------------------------------------------

# total N
totalN <- nrow(data)

# region
regionN <- data %>%
  group_by(factor(region)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

facultyRegionN <- data %>%
  group_by(factor(facultyRegion)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

fieldStudyN <- data %>%
  group_by(factor(fieldStudy)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# age
ageRange <- 17:30
agePlot <- ggplotly(data %>% filter(age %in% ageRange) %>%
                      ggplot(aes(age, fill = fct_rev(genderBinary))) + geom_histogram(binwidth=.5, bins = length(ageRange), position = position_stack(), alpha = .6) +
                      scale_x_continuous(name = "Vek", breaks=seq(17,30,1)) + labs(title = "Vek podľa pohlavia", y = "Počet", fill = "Pohlavie") +
                      scale_fill_manual(values=c(cbPalette[1], cbPalette[2])))

# age groups
ageGroupsN <- data %>%
  group_by(factor(ageOver23)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))


# gender
genderLevels <- c("Žena", "Muž", "Transrodová osoba")
genderN <- data %>%
  group_by(factor(gender, levels = genderLevels)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# sexual orientation
sexOrienLevels <- c("Heterosexuál/ka (osoba, ktorá je citovo a sexuálne priťahovaná opačným pohlavím)",
                    "Bisexuál/ka (osoba, ktorá je citovo a sexuálne priťahovaná oboma pohlaviami)",
                    "Gej (muž, ktorý je citovo a sexuálne priťahovaný mužmi)",
                    "Lesba (žena, ktorá je citovo a sexuálne priťahovaná ženami)")
sexOrienN <- data %>%
  group_by(factor(sexualOrientation, levels = sexOrienLevels)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2)) %>%
  arrange(desc(n))
sexOrienN

# year of study
data$yearStudy <- as.factor(as.numeric(gsub("([0-9]+).*$", "\\1", data$yearStudy)))
yearStudyPlot <- ggplotly(data %>% filter(yearStudy %in% 1:7) %>%
                      ggplot(aes(yearStudy, fill = fct_rev(genderBinary))) + geom_bar(stat = "count", position = position_stack(), alpha = .6) +
                      scale_x_discrete(name = "Rok štúdia") + labs(title = "Rok štúdia", y = "Počet", fill = "Pohlavie") +
                      scale_fill_manual(values=c(cbPalette[1], cbPalette[2])))


fieldStudyNbyYear <- data %>%
  group_by(yearStudy, factor(fieldStudy)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2)) %>% print(n = 100)

fieldStudyNbyRegion <- data %>%
  group_by(facultyRegion, fieldStudy) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2)) %>% print(n = 100)

# Weight


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
                  left_join(fieldStudyNbyRegion[-3], by = c("fieldStudy", "facultyRegion")) %>%
                  mutate(weight = ifelse(popProp/(percent/100) < .3,  .3, popProp/(percent/100)),
                         weight = ifelse(popProp/(percent/100) > 3,  3, popProp/(percent/100))) %>%
                  select(-c(popProp, percent))

table(data$weight)


# Prevalence rates --------------------------------------------------------
data <- data %>% mutate_at(vars(starts_with("q") & !contains("_")), funs(recode(.,"Nikdy sa mi to nestalo" = 0, "Stalo sa mi to raz" = 1, "Stalo sa mi to opakovane" = 2)))

# Individual forms
# frequency table (n's and %) for all individual types of abuse across the board
freqItems <- data %>%
  summarise_at(vars(starts_with("q") & !contains("_")),
               funs(n = sum(., na.rm = TRUE),
                    perc = sum(., na.rm = TRUE)*100/n()))

# Individual forms by gender
# frequency tables (n's and %) for all individual types of abuse by gender.
# Dropping other than female, male
freqItemsByGender <- data %>%
  group_by(factor(genderBinary)) %>%
  summarise_at(vars(starts_with("q") & !contains("_")),
               funs(n = sum(., na.rm = TRUE),
                    perc = sum(., na.rm = TRUE)*100/n())) %>%
  na.omit()

# Aggregate freuqencies
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



# frequency table (n's and %) for aggregate categories of abuse across the board
freqClusters <- data %>%
  summarise_at(vars(genderMotivHarr, unwantedSexAtt, sexAbuse),
               funs(n = sum(as.logical(.), na.rm = TRUE),
                    perc = sum(as.logical(.), na.rm = TRUE)*100/n()))

# frequency tables (n's and %) for aggregate categories of abuse by gender.
# Dropping other than female, male
freqByGender <- data %>%
  group_by(factor(genderBinary)) %>%
  summarise_at(vars(genderMotivHarr, unwantedSexAtt, sexAbuse),
               funs(n = sum(as.logical(.), na.rm = TRUE),
                    perc = sum(as.logical(.), na.rm = TRUE)*100/n())) %>%
  na.omit()

# Computes the proportions (prevalence rates) and CIs of CSA forms in girls
femaleCI <- data %>% filter(gender == "Žena") %>%  select(starts_with("q") & !contains("_"), genderMotivHarr, unwantedSexAtt, sexAbuse) %>%
map(~binom.exact(sum(as.logical(.),na.rm = TRUE), n = length(.)))

# Computes the proportions (prevalence rates) and CIs of CSA forms in boys
maleCI <- data %>% filter(gender == "Muž") %>%  select(starts_with("q") & !contains("_"), genderMotivHarr, unwantedSexAtt, sexAbuse) %>%
  map(~binom.exact(sum(as.logical(.),na.rm = TRUE), n = length(.)))

# Computes odds ratios ($measure) for: type of abuse by gender contingency tables.
data %>% select(starts_with("q") & !contains("_"), genderMotivHarr, unwantedSexAtt, sexAbuse) %>%
  map(~riskratio.boot(table(data$genderBinary, .), replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by gender contingency tables.
data %>% select(starts_with("q") & !contains("_"), genderMotivHarr, unwantedSexAtt, sexAbuse) %>%
  map(~contingencyTableBF(table(data$genderBinary, .), sampleType = "poisson", priorConcentration = 1))

# Risk factors ------------------------------------------------------------
# Keep in mind the row order when interpreting risk ratios or reverse rows using rev argument
rfData <- data %>% select(genderBinary, nonheterosexual, minority, ageOver23, believer, languageOther, yearStudy, fieldStudy, facultyRegion)

# Computes the CIs of proportions for the risk factors
rfProps <- rfData %>% map(~binom.exact(table(.), n = length(.)))

# Computes odds ratios ($measure) for genderMotivHarr
rfData %>% map(~riskratio.boot(table(., data$genderMotivHarr), replicates = 1e5))

# Computes odds ratios ($measure) for unwantedSexAtt
rfData %>% map(~riskratio.boot(table(., data$unwantedSexAtt), replicates = 1e5))

# Computes odds ratios ($measure) for sexAbuse
rfData %>% map(~riskratio.boot(table(., data$sexAbuse), replicates = 1e5))

# Bayes factors (Poisson BF10, BF01)  for: participant risk factors
# by genderMotivHarr contingency tables
rfData %>% map(~contingencyTableBF(table(., data$genderMotivHarr), sampleType = "poisson", priorConcentration = 1))

# Bayes factors (Poisson BF10, BF01)  for: participant risk factors
# by unwantedSexAtt contingency tables
rfData %>% map(~contingencyTableBF(table(., data$unwantedSexAtt), sampleType = "poisson", priorConcentration = 1))

# Bayes factors (Poisson BF10, BF01)  for: participant risk factors
# by sexAbuse contingency tables
rfData %>% map(~contingencyTableBF(table(., data$sexAbuse), sampleType = "poisson", priorConcentration = 1))


# Who RQ4---------------------------------------------------------------------

# Overall count
whoFreq <- do.call(rbind.data.frame, data %>% select(contains("_who") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>%
  rownames_to_column("who")
whoFreq

whos <- c("Teacher_M", "Teacher_F", "Student_M", "Student_F", "Employee_M", "Employee_F")
whoCountOverall <- list()
for(n in 1:6){
  for(i in whos){
  whoCountOverall[[i]] <- whoFreq %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}

# Count per cluster
# GenderMotivHarr
whoGenderMotivHarr <- do.call(rbind.data.frame, data %>% select(q1:q9 & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>% rownames_to_column("who")
whoCountGenderMotivHarr <- list(NA)
for(n in 1:6){
  for(i in whos){
    whoCountGenderMotivHarr[[i]] <- whoGenderMotivHarr %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
whoCountGenderMotivHarr[1] <- NULL

# UnwantedSexAtt
whoUnwantedSexAtt <- do.call(rbind.data.frame, data %>% select(q9:q17 & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>% rownames_to_column("who")
whoCountUnwantedSexAtt <- list(NA)
for(n in 1:6){
  for(i in whos){
    whoCountUnwantedSexAtt[[i]] <- whoUnwantedSexAtt %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
whoCountUnwantedSexAtt[1] <- NULL

# SexAbuse
whoSexAbuse <- do.call(rbind.data.frame, data %>% select(q17:anyYes & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>% rownames_to_column("who")
whoCountSexAbuse <- NA
for(n in 1:6){
  for(i in whos){
    whoCountSexAbuse[[i]] <- whoSexAbuse %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
whoCountSexAbuse[1] <- NULL


# Who Gender RQ4--------------------------------------------------------------
# theoretical number of possible abuses overall
# !!! Inferential test assumes equal proportion of encounters with perpetrators (e.g., equal number of female and male teachers) !!!
possibleAbusesOverall <- nrow(data) * 20
possibleAbusesGMH <- nrow(data) * 8
possibleAbusesUSA <- nrow(data) * 8
possibleAbusesSAB <- nrow(data) * 4

abusesMaleOverall <- as_tibble(whoCountOverall)[,c(1, 3, 5)] %>% rowSums(.)
abusesFemaleOverall <- as_tibble(whoCountOverall)[,c(2, 4, 6)] %>% rowSums(.)

abusesMaleGMH <- as_tibble(whoCountGenderMotivHarr)[,c(1, 3, 5)] %>% rowSums(.)
abusesFemaleGMH <- as_tibble(whoCountGenderMotivHarr)[,c(2, 4, 6)] %>% rowSums(.)

abusesMaleUSA <- as_tibble(whoCountUnwantedSexAtt)[,c(1, 3, 5)] %>% rowSums(.)
abusesFemaleUSA <- as_tibble(whoCountUnwantedSexAtt)[,c(2, 4, 6)] %>% rowSums(.)

abusesMaleSAB <- as_tibble(whoCountSexAbuse)[,c(1, 3, 5)] %>% rowSums(.)
abusesFemaleSAB <- as_tibble(whoCountSexAbuse)[,c(2, 4, 6)] %>% rowSums(.)

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
abusesByPerpetratorGenderOverall_OR <- riskratio.boot(abusesByPerpetratorGenderOverall, rev = "both", replicates = 1e5)
abusesByPerpetratorGenderOverall_BF <- contingencyTableBF(abusesByPerpetratorGenderOverall, sampleType = "poisson", priorConcentration = 1)

# Computes odds ratios ($measure) and Bayes factors (Poisson BF) for: type of abuse by gender of the perpetrator overall
abusesByPerpetratorGenderGMH_OR <- riskratio.boot(abusesByPerpetratorGenderGMH, rev = "both", replicates = 1e5)
abusesByPerpetratorGenderGMH_BF <- contingencyTableBF(abusesByPerpetratorGenderGMH, sampleType = "poisson", priorConcentration = 1)

# Computes odds ratios ($measure) and Bayes factors (Poisson BF) for: type of abuse by gender of the perpetrator overall
abusesByPerpetratorGenderUSA_OR <- riskratio.boot(abusesByPerpetratorGenderUSA, rev = "both", replicates = 1e5, correction = TRUE)
abusesByPerpetratorGenderUSA_BF <- contingencyTableBF(abusesByPerpetratorGenderUSA, sampleType = "poisson", priorConcentration = 1)

# Computes odds ratios ($measure) and Bayes factors (Poisson BF) for: type of abuse by gender of the perpetrator overall
abusesByPerpetratorGenderSAB_OR <- riskratio.boot(abusesByPerpetratorGenderSAB, rev = "both", replicates = 1e5, correction = TRUE)
abusesByPerpetratorGenderSAB_BF <- contingencyTableBF(abusesByPerpetratorGenderSAB, sampleType = "poisson", priorConcentration = 1)

# Where -------------------------------------------------------------------

whereFreq <- do.call(rbind.data.frame, data %>% select(contains("_where") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>%
  rownames_to_column("where")
whereFreq

wheres <- c("EduProcess", "Break", "Dorm", "Lab", "Practice", "Online", "DontKnow")

whereCountOverall <- list(NA)
for(n in 1:6){
  for(i in wheres){
    whereCountOverall[[i]] <- whereFreq %>% filter(str_detect(where, i) & grepl(".1", where, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
whereCountOverall[1] <- NULL

# Count per cluster
# GenderMotivHarr
whereGenderMotivHarr <- do.call(rbind.data.frame, data %>% select(q1:q9 & contains("_where") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>% rownames_to_column("where")
whereCountGenderMotivHarr <- list(NA)
for(n in 1:6){
  for(i in wheres){
    whereCountGenderMotivHarr[[i]] <- whereGenderMotivHarr %>% filter(str_detect(where, i) & grepl(".1", where, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
whereCountGenderMotivHarr[1] <- NULL

# UnwantedSexAtt
whereUnwantedSexAtt <- do.call(rbind.data.frame, data %>% select(q9:q17 & contains("_where") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>% rownames_to_column("where")
whereCountUnwantedSexAtt <- list(NA)
for(n in 1:6){
  for(i in wheres){
    whereCountUnwantedSexAtt[[i]] <- whereUnwantedSexAtt %>% filter(str_detect(where, i) & grepl(".1", where, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
whereCountUnwantedSexAtt[1] <- NULL

# SexAbuse
whereSexAbuse <- do.call(rbind.data.frame, data %>% select(q17:anyYes & contains("_where") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>% rownames_to_column("where")
whereCountSexAbuse <- NA
for(n in 1:6){
  for(i in wheres){
    whereCountSexAbuse[[i]] <- whereSexAbuse %>% filter(str_detect(where, i) & grepl(".1", where, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
whereCountSexAbuse[1] <- NULL


# Regions -----------------------------------------------------------------

# Computes odds ratios ($measure) for: type of abuse by region
data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~riskratio.boot(table(data$facultyRegion, .), rev = "rows", replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by region
data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~contingencyTableBF(table(data$facultyRegion, .), sampleType = "poisson", priorConcentration = 1))

regionMod <- lm(harrassedSeverity ~ facultyRegion, weights = data$weight, data)
regionBF <- lmBF(harrassedSeverity ~ facultyRegion, data, rscaleEffects = rScale)
regionSummary <- summary(regionMod)
regionAnova <- anova(regionMod)


# Length of study RQ5---------------------------------------------------------

# Computes odds ratios ($measure) for: type of abuse by length of study
data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~summary(lm(. ~ data$yearStudy, weights = data$weight)))

studyLengthMod <- lm(harrassedSeverity ~ yearStudy, weights = data$weight , data)
studyLengthBF <- lmBF(harrassedSeverity ~ yearStudy, data[!is.na(data$yearStudy),], rscaleEffects = rScale)
studyLengthSummary <- summary(studyLengthMod)
studyLengthAnova <- anova(studyLengthMod)


# Language RQ5----------------------------------------------------------------

# Computes odds ratios ($measure) for: type of abuse by language.
data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~riskratio.boot(table(data$languageOther, .), replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by language
data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~contingencyTableBF(table(data$languageOther, .), sampleType = "poisson", priorConcentration = 1))

languageMod <- lm(harrassedSeverity ~ languageOther, weights = data$weight , data)
languageBF <- lmBF(harrassedSeverity ~ languageOther, data[!is.na(data$languageOther),], rscaleEffects = rScale)
languageSummary <- summary(languageMod)

# Religious belief RQ5----------------------------------------------------------------

# Computes odds ratios ($measure) for: type of abuse by religious belief
data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~riskratio.boot(table(data$believer, .), replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by religious belief
data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~contingencyTableBF(table(data$believer, .), sampleType = "poisson", priorConcentration = 1))

believeMod <- lm(harrassedSeverity ~ believer, weights = data$weight , data)
believeBF <- lmBF(harrassedSeverity ~ believer, data[!is.na(data$believer),], rscaleEffects = rScale)
believeSummary <- summary(believeMod)


# Minority RQ5----------------------------------------------------------------

# Computes odds ratios ($measure) for: type of abuse by minority status
data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~riskratio.boot(table(data$minority, .), replicates = 1e5))

table(data$genderMotivHarr)

# Computes Bayes factors (Poisson BF)  for: type of abuse by minority status
data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~contingencyTableBF(table(data$minority, .), sampleType = "poisson", priorConcentration = 1))

minorityMod <- lm(harrassedSeverity ~ minority, weights = data$weight , data)
minorityBF <- lmBF(harrassedSeverity ~ minority, data[!is.na(data$minority),], rscaleEffects = rScale)
minoritySummary <- summary(minorityMod)


# Sexual orientation RQ5------------------------------------------------------

# Computes odds ratios ($measure) for: type of abuse by sexual orientation
data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~riskratio.boot(table(data$nonheterosexual, .), replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by minority status
data %>% select(genderMotivHarr, unwantedSexAtt, sexAbuse, harrassed) %>%
  map(~contingencyTableBF(table(data$nonheterosexual, .), sampleType = "poisson", priorConcentration = 1))

orientationMod <- lm(harrassedSeverity ~ nonheterosexual, weights = data$weight , data)
orientationBF <- lmBF(harrassedSeverity ~ nonheterosexual, data[!is.na(data$nonheterosexual),], rscaleEffects = rScale)
orientationSummary <- summary(orientationMod)


# Abuse Impacts -----------------------------------------------------------
# Aké psychické / psychosomatické dôsledky obete najčastejšie uvádzajú?
data <- data %>% mutate(impactSeverity = rowSums(data %>% select(contains("suffer_") & !contains(c("_other", "_none", "dontKnow"))), na.rm = T))

impactsFreq <- do.call(rbind.data.frame, data %>% filter(harrassed == 1) %>% select(contains("suffer_") & !contains(c("_other", "_none", "dontKnow"))) %>%
                         map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>%
  rownames_to_column("impactType") %>%
  filter(grepl(".1", impactType, fixed = TRUE)) %>%
  arrange(desc(freq))


# Je rozsah uvádzaných dôsledkov podmienený zažitou formou SO?
data %>% filter(anyYes == 1) %$% cor.test(harrassedSeverity, impactSeverity)
data %>% filter(anyYes == 1) %$% correlationBF(y = harrassedSeverity, x = impactSeverity, rscale = rScale)

# Je rozsah uvádzaných dôsledkov podmienený tým, kto je páchateľom?

# compute frequencies of abuses per cluster and perpetrator

data$gmhTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(genderMotivHarr > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$gmhStudentFreq <- data %>% transmute(gmhStudent = ifelse(genderMotivHarr > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$gmhEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(genderMotivHarr > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
#
data$usaTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(unwantedSexAtt > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$usaStudentFreq <- data %>% transmute(gmhStudent = ifelse(unwantedSexAtt > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$usaEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(unwantedSexAtt > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
#
data$sabTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(sexAbuse > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$sabStudentFreq <- data %>% transmute(gmhStudent = ifelse(sexAbuse > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$sabEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(sexAbuse > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
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

severPerpMod <- lm(impactSeverity ~ perpetratorMostSevere, weights = data$weight, data)
severPerpBF <- lmBF(impactSeverity ~ perpetratorMostSevere, data[!is.na(data$perpetratorMostSevere),], rscaleEffects = rScale)
severPerpSummary <- summary(severPerpMod)
severPerpAnova <- anova(severPerpMod)

# Existuje rozdiel medzi mužmi a ženami z hľadiska množstva / typu uvádzaných následkov?
severGenderMod <- data %>% filter(harrassed == 1) %$% lm(impactSeverity ~ genderBinary, weights = weight)
severGenderBF <- data %>% filter(harrassed == 1) %$% lmBF(impactSeverity ~ genderBinary, .[!is.na(genderBinary),], rscaleEffects = rScale)
severGenderSummary <- summary(severGenderMod)
severGenderAnova <- anova(severGenderMod)

# Je rozsah uvádzaných dôsledkov podmienený tým, či sa obete zdôverili (či hľadali podporu)? # ADRESOVANE V RQ7 V OPACNEJ DIREKTIVITE

# Je rozsah úvádzaných dôsledkov podmienený tým, či osoba zažila negatívne reakcie zo strany školy?
data <- data %>% mutate(consequencesSeverity = rowSums(data %>% select(contains("conseq_") & !contains(c("_other", "_none", "dontKnow"))), na.rm = T))

severConseqMod <- data %>% filter(harrassed == 1) %$% lm(impactSeverity ~ as.logical(consequencesSeverity), weights = weight)
severConseqBF <- data %>% filter(harrassed == 1) %>% mutate(consequencesSeverity = as.logical(consequencesSeverity)) %$% lmBF(impactSeverity ~ consequencesSeverity, .[!is.na(consequencesSeverity),], rscaleEffects = rScale)
severConseqSummary <- summary(severConseqMod)
severConseqAnova <- anova(severConseqMod)

# Je rozsah uvádzaných dôsledkov podmienený príslušnosťou k niektorej menšine?
severMinorityMod <- data %>% filter(harrassed == 1) %$% lm(impactSeverity ~ minority, weights = weight)
severMinorityBF <- data %>% filter(harrassed == 1) %$% lmBF(impactSeverity ~ minority, .[!is.na(minority),], rscaleEffects = rScale)
severMinoritySummary <- summary(severMinorityMod)
severMinorityAnova <- anova(severMinorityMod)

# Je rozsah uvádzaných dôsledkov podmienený vierovyznaním obetí?
severBelieverMod <- data %>% filter(harrassed == 1) %$% lm(impactSeverity ~ believer, weights = weight)
severBelieverBF <- data %>% filter(harrassed == 1) %$% lmBF(impactSeverity ~ believer, .[!is.na(believer),], rscaleEffects = rScale)
severBelieverSummary <- summary(severBelieverMod)
severBelieverAnova <- anova(severBelieverMod)


# RQ7 Hladanie pomoci -----------------------------------------------------

# Koľko percent obetí vôbec hľadalo pomoc/radu?
disclosureN <- data %>%
  filter(harrassed == 1) %>%
  group_by(disclosureBinary) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# Koľko percent obetí vôbec hľadalo pomoc/radu?
# By gender
disclosureN <- data %>%
  filter(harrassed == 1) %>%
  group_by(disclosureBinary, genderBinary) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# U koho hľadali obete najčastejšie pomoc?
disclosureWhoFreq <- do.call(rbind.data.frame, data %>% select(contains("disclosure_") & !contains("whyNo")) %>% map(~cbind("freq" = table(as.logical(.)), "perc" = prop.table(table(as.logical(.)))))) %>% #Turning disclosure_ vars to logical; T = disclosed, F = not disclosed
  rownames_to_column("disclosureWho") %>% filter(grepl(".TRUE", disclosureWho, fixed = TRUE)) %>% arrange(desc(freq))

disclosureWhoFreq[,"disclosureWho"] <- disclosureWhoFreq[,"disclosureWho"] %>% str_remove("disclosure_") %>% str_remove(".TRUE")

# Existuje súvis medzi zažitou formou SO a tendenciou obetí hľadať pomoc? & Existuje súvis medzi častosťou (frekvenciou) SO a tendenciou obetí hľadať pomoc?
# Severity as predictor

severDisclosureMod0 <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ 1, family=binomial(), weights = weight)
severDisclosureMod <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ impactSeverity, family=binomial(), weights = weight)
severDisclosureSummary <- summary(severDisclosureMod)
severDisclosureAnova <- anova(severDisclosureMod)

severDisclosureBF10 <- 1/exp((BIC(severDisclosureMod) - BIC(severDisclosureMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
severDisclosurePosterior <- severDisclosureBF10/(1+severDisclosureBF10)
severDisclosureBayesOut <- data.frame(BF10 = round(severDisclosureBF10, 3), Posterior = round(severDisclosurePosterior, 3))

# Existuje rozdiel medzi pohlaviami v tendencii hľadať pomoc?
genderDisclosureOR <- data %$% riskratio.boot(table(genderBinary, disclosureBinary), replicates = 1e5)
genderDisclosureBF <- data %$% contingencyTableBF(table(genderBinary, disclosureBinary), sampleType = "poisson", priorConcentration = 1)

# Súvisí hľadanie pomoci s vierovyznaním obete?
believerDisclosureOR <- data %$% riskratio.boot(table(believer, disclosureBinary), replicates = 1e5)
believerDisclosureBF <- data %$% contingencyTableBF(table(believer, disclosureBinary), sampleType = "poisson", priorConcentration = 1)

# Súvisí hľadanie pomoci s odborom štúdia?
fieldDisclosureMod0 <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ 1, family=binomial(), weights = weight)
fieldDisclosureMod <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ fieldStudy, family=binomial(), weights = weight)
fieldDisclosureSummary <- summary(fieldDisclosureMod)
fieldDisclosureAnova <- anova(fieldDisclosureMod)

fieldDisclosureBF10 <- 1/exp((BIC(fieldDisclosureMod) - BIC(fieldDisclosureMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
fieldDisclosurePosterior <- fieldDisclosureBF10/(1+fieldDisclosureBF10)
fieldDisclosureBayesOut <- data.frame(BF10 = round(fieldDisclosureBF10, 3), Posterior = round(fieldDisclosurePosterior, 3))

# Súvisí hľadanie pomoci s regiónom, z ktorého obete pochádzajú?
regionDisclosureMod0 <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ 1, family=binomial(), weights = weight)
regionDisclosureMod <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ facultyRegion, family=binomial(), weights = weight)
regionDisclosureSummary <- summary(regionDisclosureMod)
regionDisclosureAnova <- anova(regionDisclosureMod)

regionDisclosureBF10 <- 1/exp((BIC(regionDisclosureMod) - BIC(regionDisclosureMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
regionDisclosurePosterior <- regionDisclosureBF10/(1+regionDisclosureBF10)
regionDisclosureBayesOut <- data.frame(BF10 = round(regionDisclosureBF10, 3), Posterior = round(regionDisclosurePosterior, 3))

# Súvisí hľadanie pomoci s povedomím o tom čo je SO?
# Compute sensitivity to harrassment by summing the att items
data <- data %>% mutate(sensitivityToHarrasment = rowSums(data %>% select(contains("att") & !contains(c("unwanted"))), na.rm = T))
sensitivityDisclosureMod0 <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ 1, family=binomial(), weights = weight)
sensitivityDisclosureMod <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ sensitivityToHarrasment, family=binomial(), weights = weight)
sensitivityDisclosureSummary <- summary(sensitivityDisclosureMod)
sensitivityDisclosureAnova <- anova(sensitivityDisclosureMod)

sensitivityDisclosureBF10 <- 1/exp((BIC(sensitivityDisclosureMod) - BIC(sensitivityDisclosureMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
sensitivityDisclosurePosterior <- sensitivityDisclosureBF10/(1+sensitivityDisclosureBF10)
sensitivityDisclosureBayesOut <- data.frame(BF10 = round(sensitivityDisclosureBF10, 3), Posterior = round(sensitivityDisclosurePosterior, 3))

# V akej miere boli obete spokojné s reakciou osôb, u ktorých hľadali pomoc?
data <- data %>% mutate(satisfaction = rowSums(data %>% select(contains("disclosure_") & !contains(c("whyNo"))), na.rm = T))
data %>% filter(satisfaction != 0) %>% summarise(meanSatisfaction = mean(satisfaction, na.rm = T),
                   sdSatisfaction = sd(satisfaction, na.rm = T))
data %>% filter(satisfaction != 0) %$% table(satisfaction)


disclosureSatisfaction <- do.call(rbind.data.frame, data %>% select(contains("disclosure_") & !contains("whyNo")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))) %>%
  rownames_to_column("disclosureWho") %>% filter(!grepl(".0", disclosureWho, fixed = TRUE)) %>% arrange(desc(freq))

# V koľkých percentách prípadov bolo spustené oficiálne konanie voči osobe, ktorá sa dopustila SO?
investigationN <- data %>%
  filter(harrassed == 1) %>%
  group_by(investigation) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# Existuje súvis medzi zažitou formou SO a spustením oficiálneho konania? & Existuje súvis medzi častosťou (frekvenciou) SO a spustením oficiálneho konania?
investigationMod0 <- data %>% filter(harrassed == 1) %$% glm(investigation ~ 1, family=binomial(), weights = weight)
investigationMod <- data %>% filter(harrassed == 1) %$% glm(investigation ~ harrassedSeverity, family=binomial(), weights = weight)
investigationSummary <- summary(investigationMod)
investigationAnova <- anova(investigationMod)

investigationBF10 <- 1/exp((BIC(investigationMod) - BIC(investigationMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
investigationPosterior <- investigationBF10/(1+investigationBF10)
investigationBayesOut <- data.frame(BF10 = round(investigationBF10, 3), Posterior = round(investigationPosterior, 3))

data %$% cor(investigation, harrassedSeverity, use = "complete.obs")

table(data$investigation_worseGrades)


# RQ8 ---------------------------------------------------------------------

# Aké dôsledky spojené so zverejnením sťažnosti na pôde VŠ sú uvádzané najčastejšie?
investigationFreq <- do.call(rbind.data.frame, data %>% select(contains("investigation_") & !contains("_other")) %>% map(~cbind("freq" = table(as.logical(.)), "perc" = prop.table(table(as.logical(.)))))) %>% #Turning disclosure_ vars to logical; T = disclosed, F = not disclosed
  rownames_to_column("investigationConsq") %>% filter(grepl(".TRUE", investigationConsq, fixed = TRUE)) %>% arrange(desc(freq))


# RQ9 ---------------------------------------------------------------------
# Vnímanie sexuálneho obťažovania (operacionalizovane ako senzitivita)
# Ktoré prejavy správania považujú respondenti/tky za SO?
data %>% select(starts_with("att") & !contains(c("unwanted"))) %>%
  map(~mean(.,na.rm = TRUE)) %>% as.tibble() %>% gather(key = "perception", value = "mean") %>% arrange(desc(mean))

# Existujú rozdiely s ohľadom na klastre foriem SO (1. rodové obťažovanie, 2. nechcená sexuálna pozornosť, 3. sexuálne donútenie/násilie) ?
# Tazko zodpovedatelne. Alternativa: vztah zavaznosti SO a senzitivity na obtazovanie

data %$% cor.test(harrassedSeverity, sensitivityToHarrasment)
data %$% correlationBF(harrassedSeverity, sensitivityToHarrasment, rscale = rScale)

# Existujú rozdiely medzi pohlaviami?
genderSensitivityMod <- lm(sensitivityToHarrasment ~ genderBinary, weights = weight, data)
genderSensitivityBF <- lmBF(sensitivityToHarrasment ~ genderBinary, data[!is.na(data$genderBinary),], rscaleEffects = rScale)
genderSensitivitySummary <- summary(genderSensitivityMod)
genderSensitivityAnova <- anova(genderSensitivityMod)

# Existujú rozdiely v závislosti od veku respondentov/tiek?
ageSensitivityMod <- lm(sensitivityToHarrasment ~ age, weights = weight, data)
ageSensitivityBF <- lmBF(sensitivityToHarrasment ~ age, data[!is.na(data$age),], rscaleEffects = rScale)
ageSensitivitySummary <- summary(ageSensitivityMod)
ageSensitivityAnova <- anova(ageSensitivityMod)

# Existujú rozdiely v závislosti od odboru štúdia respondentov/tiek?
fieldSensitivityMod <- lm(sensitivityToHarrasment ~ fieldStudy, weights = weight, data)
fieldSensitivityBF <- lmBF(sensitivityToHarrasment ~ fieldStudy, data[!is.na(data$fieldStudy),], rscaleEffects = rScale)
fieldSensitivitySummary <- summary(fieldSensitivityMod)
fieldSensitivityAnova <- anova(fieldSensitivityMod)

data %>% group_by(fieldStudy) %>% summarise(meanSensitivity = mean(sensitivityToHarrasment, na.rm = T),
                                            sdSensitivity = sd(sensitivityToHarrasment, na.rm = T))

# Existujú rozdiely v závislosti od regiónov, z ktorých respondenti/tky pochádzajú?
regionSensitivityMod <- lm(sensitivityToHarrasment ~ facultyRegion, weights = weight, data)
regionSensitivityBF <- anovaBF(sensitivityToHarrasment ~ facultyRegion, data[!is.na(data$facultyRegion),], rscaleEffects = rScale)
regionSensitivitySummary <- summary(regionSensitivityMod)
regionSensitivityAnova <- anova(regionSensitivityMod)

data %>% group_by(facultyRegion) %>% summarise(meanSensitivity = mean(sensitivityToHarrasment, na.rm = T),
                                            sdSensitivity = sd(sensitivityToHarrasment, na.rm = T))

# Existujú rozdiely v závislosti od vierovyznania respondentov/tiek?
believerSensitivityMod <- lm(sensitivityToHarrasment ~ believer, weights = weight, data)
believerSensitivityBF <- anovaBF(sensitivityToHarrasment ~ believer, data[!is.na(data$believer),], rscaleEffects = rScale)
believerSensitivitySummary <- summary(believerSensitivityMod)
believerSensitivityAnova <- anova(believerSensitivityMod)

data %>% group_by(believer) %>% summarise(meanSensitivity = mean(sensitivityToHarrasment, na.rm = T),
                                               sdSensitivity = sd(sensitivityToHarrasment, na.rm = T))

# Existujú rozdiely medzi obeťami SO a osobami, ktoré nemajú osobnú skúsenosť s SO? (častý poznatok - osoby, ktoré majú viac skúseností so SO zvyknú aj viac situácií vyhodnocovať ako SO).
harrassedSensitivityMod <- lm(sensitivityToHarrasment ~ harrassed, weights = weight, data)
harrassedSensitivityBF <- anovaBF(sensitivityToHarrasment ~ harrassed, data[!is.na(data$harrassed),], rscaleEffects = rScale)
harrassedSensitivitySummary <- summary(harrassedSensitivityMod)
harrassedSensitivityAnova <- anova(harrassedSensitivityMod)
data %>% group_by(harrassed) %>% summarise(meanSensitivity = mean(sensitivityToHarrasment, na.rm = T),
                                          sdSensitivity = sd(sensitivityToHarrasment, na.rm = T))

# Existuje súvis medzi tým, čo respondenti/tky považujú za SO a tým, či majú tendenciou hľadať pomoc, ak sa s niektorou z foriem SO osobne stretli?
# controlling for harrassedSeverity
disclosureSensitivityMod <- lm(sensitivityToHarrasment ~ disclosureBinary + harrassedSeverity, weights = weight, data)
disclosureSensitivityBF <- lmBF(sensitivityToHarrasment ~ disclosureBinary + harrassedSeverity, data[!is.na(data$disclosureBinary),], rscaleEffects = rScale)
disclosureSensitivitySummary <- summary(disclosureSensitivityMod)
disclosureSensitivityAnova <- anova(disclosureSensitivityMod)
data %>% group_by(disclosureBinary) %>% summarise(meanSensitivity = mean(sensitivityToHarrasment, na.rm = T),
                                           sdSensitivity = sd(sensitivityToHarrasment, na.rm = T))

#
#
#
#
#
#
#


# RQ10 --------------------------------------------------------------------
# Poskytla im ich vysoká škola dostatok informácií o SO?
# dostatok informacii = odpoved skor ano alebo urcite ano
dostatokInformaciiFreq <- data %>%
  summarise(dostatokInformaciiN = sum(know3 > 2, na.rm = TRUE),
            dostatokInformaciiPerc = sum(know3 > 2, na.rm = TRUE)*100/n())

# Existujú rozdiely medzi pohlaviami?
genderInformationMod <- lm(know3 ~ genderBinary, weights = weight, data)
genderInformationBF <- lmBF(know3 ~ genderBinary, data[!is.na(data$genderBinary) & !is.na(data$know3),], rscaleEffects = rScale)
genderInformationSummary <- summary(genderInformationMod)
genderInformationAnova <- anova(genderInformationMod)

table(data$know3)

# Existujú rozdiely v závislosti od regiónov, z ktorých respondenti/tky pochádzajú?
regionInformationMod <- lm(know3 ~ facultyRegion, weights = weight, data)
regionInformationBF <- anovaBF(know3 ~ facultyRegion, data[!is.na(data$facultyRegion) & !is.na(data$know3),], rscaleEffects = rScale)
regionInformationSummary <- summary(regionInformationMod)
regionInformationAnova <- anova(regionInformationMod)

# Existujú rozdiely v závislosti od odboru štúdia respondentov/tiek?
fieldInformationMod <- lm(know3 ~ fieldStudy, weights = weight, data)
fieldInformationBF <- lmBF(know3 ~ fieldStudy, data[!is.na(data$fieldStudy) & !is.na(data$know3),], rscaleEffects = rScale)
fieldInformationSummary <- summary(fieldInformationMod)
fieldInformationAnova <- anova(fieldInformationMod)


# RQ10 --------------------------------------------------------------------


# Tvrdenia / stereotypy /  predsudky o sexuálnom obťažovaní
data <- data %>% mutate(misconceptScore = rowSums(data %>% select(starts_with("misconcept")), na.rm = T))

# V akej miere súhlasia respondenti/tky s jednotlivými tvrdeniami?
# min score = 0, max score 55. The higher the score the higher the agreement with misconceptions
agreeMisconceptions <- data %>%
  summarise(meanAgreementMisconceptions = mean(misconceptScore, na.rm = TRUE),
            sdAgreementMisconceptions = sd(misconceptScore, na.rm = TRUE))

# Existujú rozdiely medzi pohlaviami?
genderMisconceptionsMod <- lm(misconceptScore ~ genderBinary, weights = weight, data)
genderMisconceptionsBF <- lmBF(misconceptScore ~ genderBinary, data[!is.na(data$genderBinary) & !is.na(data$misconceptScore),], rscaleEffects = rScale)
genderMisconceptionsSummary <- summary(genderMisconceptionsMod)
genderMisconceptionsAnova <- anova(genderMisconceptionsMod)

# Existujú rozdiely v závislosti od veku respondentov/tiek?
ageMisconceptionsMod <- lm(misconceptScore ~ age, weights = weight, data)
ageMisconceptionsBF <- lmBF(misconceptScore ~ age, data[!is.na(data$age),], rscaleEffects = rScale)
ageMisconceptionsSummary <- summary(ageMisconceptionsMod)
ageMisconceptionsAnova <- anova(ageMisconceptionsMod)

# Existujú rozdiely v závislosti od odboru štúdia respondentov/tiek?
fieldMisconceptionsMod <- lm(misconceptScore ~ fieldStudy, weights = weight, data)
fieldMisconceptionsBF <- lmBF(misconceptScore ~ fieldStudy, data[!is.na(data$fieldStudy) & !is.na(data$misconceptScore),], rscaleEffects = rScale)
fieldMisconceptionsSummary <- summary(fieldMisconceptionsMod)
fieldMisconceptionsAnova <- anova(fieldMisconceptionsMod)

# Existujú rozdiely v závislosti od regiónov, z ktorých respondenti/tky pochádzajú?
regionMisconceptionsMod <- lm(misconceptScore ~ facultyRegion, weights = weight, data)
regionMisconceptionsBF <- anovaBF(misconceptScore ~ facultyRegion, data[!is.na(data$facultyRegion) & !is.na(data$misconceptScore),], rscaleEffects = rScale)
regionMisconceptionsSummary <- summary(regionMisconceptionsMod)
regionMisconceptionsAnova <- anova(regionMisconceptionsMod)

# Existujú rozdiely v závislosti od vierovyznania respondentov/tiek?
believerMisconceptionsMod <- lm(misconceptScore ~ believer, weights = weight, data)
believerMisconceptionsBF <- anovaBF(misconceptScore ~ believer, data[!is.na(data$believer),], rscaleEffects = rScale)
believerMisconceptionsSummary <- summary(believerMisconceptionsMod)
believerMisconceptionsAnova <- anova(believerMisconceptionsMod)

data %>% group_by(believer) %>% summarise(meanMisconceptions = mean(misconceptScore, na.rm = T),
                                          sdMisconceptions = sd(misconceptScore, na.rm = T))

# Existujú rozdiely medzi obeťami SO a osobami, ktoré nemajú osobnú skúsenosť s SO? (častý poznatok - osoby, ktoré majú viac skúseností so SO zvyknú aj viac situácií vyhodnocovať ako SO).
harrassedMisconceptionsMod <- lm(misconceptScore ~ harrassed, weights = weight, data)
harrassedMisconceptionsBF <- anovaBF(misconceptScore ~ harrassed, data[!is.na(data$harrassed),], rscaleEffects = rScale)
harrassedMisconceptionsSummary <- summary(harrassedMisconceptionsMod)
harrassedMisconceptionsAnova <- anova(harrassedMisconceptionsMod)
data %>% group_by(harrassed) %>% summarise(meanMisconceptions = mean(misconceptScore, na.rm = T),
                                           sdMisconceptions = sd(misconceptScore, na.rm = T))

# Existuje súvis medzi tým, čo respondenti/tky považujú za SO a tým, či majú tendenciou hľadať pomoc, ak sa s niektorou z foriem SO osobne stretli?
# controlling for harrassedSeverity
disclosureMisconceptionsMod <- lm(misconceptScore ~ disclosureBinary + harrassedSeverity, weights = weight, data)
disclosureMisconceptionsBF <- lmBF(misconceptScore ~ disclosureBinary + harrassedSeverity, data[!is.na(data$disclosureBinary),], rscaleEffects = rScale)
disclosureMisconceptionsSummary <- summary(disclosureMisconceptionsMod)
disclosureMisconceptionsAnova <- anova(disclosureMisconceptionsMod)
data %>% group_by(disclosureBinary) %>% summarise(meanMisconceptions = mean(misconceptScore, na.rm = T),
                                                  sdMisconceptions = sd(misconceptScore, na.rm = T))


#'## Bayesian analysis of predictive power
#'
#' **For achievement measures**
#'
#' **Bayes factor in favor of the alternative hypothesis (BF10) and posterior probability for model parameters (given 1:1 prior odds for H0:Ha)**
#'
#'Bayes factors show whether there is evidence either for Ha (effect present) or H0 (effect absent), i.e., whether the data are more consistent with Ha, H0, or inconclusive. Posterior probability refers to the probability of the *parameter* not being zero (as oposed to probability of the data under a null).
#'Frequentist approach without specific procedures (like equivalence testing), on the other hand, cannot provide evidence for H0, by definition (the only possible conclusions are H0 being rejected or failed to be rejected).
#'These are Bayes Factors based on model selection / information criteria approach as proposed by Wagenmakers, 2007. Each BF represents the relative evidence in the data favoring alternative hypothesis (parameter freely estimated) over the null (the given parameter fixed to 0).
#'Bayes Factors using BIC approximation implicitly assume unit information prior which makes them rather conservative with regard to the alternative hypothesis.