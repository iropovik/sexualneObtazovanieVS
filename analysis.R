# Notes

# doplnit formulacie otazok z wordu s otazkami
# napisat, ze pri binomial logisticka regresii bolo BF zalozene na BIC aproximacii

# Calculates risk ratio by unconditional maximum likelihood estimation (Wald), and small sample adjustment (small). Confidence intervals are calculated using normal approximation (Wald), and normal approximation with small sample adjustment (small), and bootstrap method (boot). median-unbiased estimation and exact confidence interval using the mid-p method (Rothman 1998, p. 251).

# Script file to be sourced by the Rmarkdown manuscript
rm(list = ls())

# install required R libraries if not installed already
list.of.packages <- c("car", "tidyverse", "psych", "epitools", "plotly", "survey", "sjstats", "questionr", "BayesFactor", "magrittr", "careless", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required libraries
lapply(list.of.packages, require, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)

# define a color-blind-friendly palette for plotting
cbPalette <- c("#E69F00", "#999999", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#' prior width (r scale) for linear bayesian models
rScale <- sqrt(2)/4

# read in the data
data <- read_delim("data/dataAnonymized.csv", delim = ",", skip_empty_rows = T)
# View(data)

# some initial data recoding
data <- data %>% mutate(
  Neheterosexuál = ifelse(is.na(sexualOrientation), NA, ifelse(sexualOrientation == "Heterosexuál/ka (osoba, ktorá je citovo a sexuálne priťahovaná opačným pohlavím)", 0, 1))) # 0 = "Heterosexuál/ka", 1 = "Non-heterosexuál/ka"

data <- data %>% mutate(`Vek viac ako 23` = as.factor(case_when(age < 23 ~ 0, # 0 = '17-22', 1 = '23+'
                                             age >= 23  ~ 1)),
                        Rod = as.factor(case_when(gender == "Muž" ~ 0,
                                                 gender == "Žena" ~ 1)),
                        disclosureBinary = as.factor(case_when(disclosure == "Áno" ~ 1,
                                                               disclosure == "Nie" ~ 0)),
                        Minorita = as.factor(ifelse(rowSums(cbind(minority_ethnic,minority_immigrant, minority_religion, minority_disabled), na.rm = T) > 0, 1, 0)),
                        `Iný materinský jazyk` = ifelse(lng_slovak == 1, 0, 1),

                        Veriaci =  as.factor(ifelse(religion %in% c("Katolícke", "Kresťanské ne-katolícke (napr. evanjelické)", "Židovské", "Pravoslávne"), 1, 0)), #Doplniť do kategórie veriaci 1 = "Veriaci", 0 = "Neveriaci"
                        anyYes = recode(anyYes, "Áno" = 1, "Nie" = 0),
                        investigation = recode(as.factor(investigation), "Áno" = 1, "Nie" = 0, "Neviem" = NA_real_))

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

data <- data %>% mutate(satisfaction = rowSums(data %>% select(contains("disclosure_") & !contains(c("whyNo"))), na.rm = T))

data$facultyRegion <- factor(data$facultyRegion)
data$fieldStudy <- factor(data$fieldStudy)

# Sample description ------------------------------------------------------

# total N
ss_total_n <- nrow(data)

# region participant
ss_region_n <- data %>%
  group_by(region = factor(region)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2)) %>% arrange(n) %>%
  na.omit()

ss_region_plot <- ggplotly(ggplot(ss_region_n, aes(x = region, y = n)) + geom_bar(stat = "identity",  alpha = .6, fill = cbPalette[1])  +
           labs(title = "Región", x = "", y = "Počet participantov") + scale_fill_manual(cbPalette[1]) +
           coord_flip() + scale_x_discrete(limits = ss_region_n$region))

# region of the faculty
ss_facultyRegion_n <- data %>%
  group_by(factor(`facultyRegion`)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))


# field of study
ss_fieldStudy_n <- data %>%
  group_by(Odbor = factor(`fieldStudy`)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2)) %>% arrange(n)

ss_fieldStudy_plot <- ggplotly(data %>% filter(Rod %in% c(0, 1)) %>% mutate(Rod = fct_rev(Rod)) %>%
           ggplot(aes(`fieldStudy`, fill = Rod)) + geom_histogram(stat = "count", alpha = .6, binwidth=.5, bins = length(data$`fieldStudy`)) +
           labs(title = "fieldStudy", x = "", y = "Počet participantov", fill = "Pohlavie") + coord_flip() + scale_fill_manual(values=c(cbPalette[1], cbPalette[2])) +
           scale_x_discrete(limits = ss_fieldStudy_n$Odbor))

# age
ageRange <- 17:30
ss_age_plot <- ggplotly(data %>% filter(age %in% ageRange & Rod %in% c(0, 1)) %>% mutate(Rod = fct_rev(Rod)) %>%
                      ggplot(aes(age, fill = Rod)) + geom_histogram(binwidth=.5, bins = length(ageRange), position = position_stack(), alpha = .6) +
                      scale_x_continuous(name = "Vek", breaks=seq(17,30,1)) + labs(title = "Vek podľa pohlavia", y = "Počet", fill = "Pohlavie") +
                      scale_fill_manual(values=c(cbPalette[1], cbPalette[2])))

# age groups
ss_ageGroups_n <- data %>%
  group_by(factor(`Vek viac ako 23`)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# gender
genderLevels <- c("Žena", "Muž", "Transrodová osoba")
ss_gender_n <- data %>%
  group_by(factor(gender, levels = genderLevels)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

minorityEthnic <- table(data$minority_ethnic)
minorityImmigrant <- table(data$minority_immigrant)
minorityReligion <- table(data$minority_religion)
minorityDisabled <- table(data$minority_disabled)
minorityOther <- table(data$minority_other)
minoritySexual <- table(data$sexualOrientation)

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
data$`Rok štúdia` <- as.factor(as.numeric(gsub("([0-9]+).*$", "\\1", data$yearStudy)))
ss_yearStudy_plot <- ggplotly(data %>% filter(`Rok štúdia` %in% 1:7 & Rod %in% c(0, 1)) %>%
                      ggplot(aes(`Rok štúdia`, fill = fct_rev(Rod))) + geom_bar(stat = "count", position = "stack", alpha = .6) +
                      scale_x_discrete(name = "Rok štúdia") + labs(title = "Rok štúdia", y = "Počet", fill = "Pohlavie") +
                      scale_fill_manual(values=c(cbPalette[1], cbPalette[2])))

# field of study by year
ss_fieldStudyYear_n <- data %>%
  group_by(`Rok štúdia`, factor(`fieldStudy`)) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

ss_fieldStudyYear_plot <- ggplotly(data %>% filter(`Rok štúdia` %in% 1:7 & Rod %in% c(0, 1)) %>%
           ggplot(aes(`fieldStudy`, fill = `Rok štúdia`)) + geom_bar(stat = "count", position = position_stack(), alpha = .6) +
           scale_x_discrete(name = "Odbor", limits = ss_fieldStudy_n$Odbor) + labs(title = "Rok štúdia v rámci odborov", y = "Počet", fill = "Ročník") +
           scale_fill_manual(values=cbPalette) + coord_flip())

# field of study by region
ss_fieldStudyRegion_n <- data %>%
  group_by(`facultyRegion`, `fieldStudy`) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))


# Careless responders exclusion -------------------------------------------

# participant exclusions
data <- data %>% filter(is.na(exclude))

# careless responders
data$longstringAtt <- data %>% select(att1:att19) %>% longstring()
data$longstringMisconcept <- data %>% select(misconcept1:misconcept11) %>% longstring()

data$irv <- data %>% select(att1:att19) %>% irv()
data$mahadRaw <- data %>% select(att1:att19, misconcept1:misconcept11) %>% mahad(plot = F, flag = T, confidence = 1) %>% .$raw
data$mahadFlagged <- data %>% select(att1:att19, misconcept1:misconcept11) %>% mahad(plot = F, flag = T, confidence = 1) %>% .$flagged

# randResp <- NA
# classification <- NA
# nsim <-  100
# for(j in 1:nsim){
#   items <- length(data[!is.na(data$irv),]$irv)
#   for(i in 1:items){
#     randResp[i] <- sd(sample(1:5, 19, replace = TRUE))
#   }
#
#   df <- data.frame(cbind(sds = c(data[!is.na(data$irv),]$irv, randResp),
#                          genuineResp = c(ifelse(data[!is.na(data$irv),]$irv < 1.3, 1, 0), rep(0, length(randResp)))))
#
#   model <- glm(genuineResp ~sds,family=binomial(link='logit'),data=df)
#   fittedProbs <- model %>% fitted.values()
#   predicted.classes <- ifelse(fittedProbs > 0.5, 1, 0)
#   classification[j] <- mean(predicted.classes == df$genuineResp)
# }
# mean(classification)
# quantile(randResp, probs = .5)

table(data$irv > 1.3)/length(data$irv)

data <- data %>% filter(longstringAtt < 19 & longstringMisconcept < 11 & mahadFlagged == FALSE)
nrow(data)

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

sampleProp <- data %>%
  group_by(Odbor = factor(data$fieldStudy), Region = relevel(factor(data$facultyRegion), ref = "Západné Slovensko")) %>%
  summarise(sampleProp = round(100*n()/nrow(.), 3))


popProp <- as.data.frame(t(round((popCounts)*100/99205, 3)))
popProp <- cbind(gather(popProp), region = rep(c("Západné Slovensko", "Stredné Slovensko", "Východné Slovensko"), 3), sampleProp$sampleProp)
names(popProp) <- c("fieldStudy", "popProp", "facultyRegion", "sampleProp")

# Compute and trim weights
data <- data %>% left_join(popProp, by = c("fieldStudy", "facultyRegion")) %>%
                  left_join(ss_fieldStudyRegion_n[-3], by = c("fieldStudy", "facultyRegion")) %>%
                  mutate(w = ifelse(popProp/sampleProp < .3,  .3, popProp/sampleProp),
                         w = ifelse(popProp/sampleProp > 3,  3, popProp/sampleProp)) %>%
                  select(-c(popProp, percent))

design <- svydesign(ids = ~1, data = data, weights = ~w)
data$`Región fakulty` <- data$facultyRegion
data$`Odbor štúdia`<- data$fieldStudy

# RQ1 Prevalence rates---------------------------------------------------------------------
# RQ1 Aký je celkový výskyt všetkých foriem sexuálneho obťažovania
data <- data %>% mutate_at(vars(starts_with("q") & !contains("_")), funs(recode(.,"Nikdy sa mi to nestalo" = 0, "Stalo sa mi to raz" = 1, "Stalo sa mi to opakovane" = 2)))

### RQ1.1 Individual forms
# frequency table (n's and %) for all individual types of abuse across the board
rq1.1_items_n <- data %>%
  summarise_at(vars(starts_with("q") & !contains("_")),
               funs(n = round(svytotal(as.logical(.), design, na.rm = T), 0),
                    perc = round(svytotal(as.logical(.), design, na.rm = T), 3)*100/sum(w)))

rq1.1_items_plotData <- as_tibble(cbind(item = names(rq1.1_items_n), t(rq1.1_items_n))) %>% filter(!grepl("_n", item, fixed = TRUE)) %>%
  mutate(perc = as.numeric(V2), item = str_remove(item, "_perc"), cluster = c(rep("GMH", 8), rep("USA", 8), rep("SAB", 4)), V2 = NULL) %>% arrange(perc)

rq1.1_items_plot <- ggplotly(rq1.1_items_plotData %>% ggplot(aes(x = item, y = perc, fill = cluster)) + geom_bar(stat = "identity",  alpha = .6) +
           labs(title = "Forma sexuálneho obťažovania", x = "", y = "Percento participantov") + scale_fill_manual("legend", values = c("GMH" = cbPalette[1], "USA" = cbPalette[2], "SAB" = cbPalette[3])) +
           coord_flip() + scale_x_discrete(limits = rq1.1_items_plotData$item))

# individual forms by gender
# frequency tables (n's and %) for all individual types of abuse by gender.
# Dropping other than female, male
rq1.1_itemsGender_n <- data %>%
  group_by(factor(Rod)) %>%
  summarise_at(vars(starts_with("q") & !contains("_")),
               funs(n = round(weighted.mean(as.logical(.), w, na.rm = T)*sum(w), 0),
                    perc = weighted.mean(as.logical(.), w, na.rm = T)*100)) %>% na.omit()

rq1.1_itemsGender_plotData <- as_tibble(cbind(item = names(rq1.1_itemsGender_n), t(rq1.1_itemsGender_n)))[-1,] %>% filter(!grepl("_n", item, fixed = TRUE)) %>%
  mutate(male = as.numeric(V2), female = as.numeric(V3), item = str_remove(item, "_perc"), V2 = NULL, V3 = NULL) %>% pivot_longer(cols = c(male, female), names_to = "gender", values_to = "perc")

rq1.1_itemsGender_plot <- ggplotly(rq1.1_itemsGender_plotData %>% ggplot(aes(x = item, y = perc, fill = gender)) + geom_bar(stat = "identity",  alpha = .6, position = "dodge") +
                               labs(title = "Forma sexuálneho obťažovania podľa pohlavia", x = "", y = "Percento participantov") + scale_fill_manual("legend", values = c("female" = cbPalette[1], "male" = cbPalette[2])) +
                               coord_flip() + scale_x_discrete(limits = rev(rq1.1_itemsGender_plotData$item)))

### Frequencies by cluster
# RQ1.2 Aký je výskyt SO v prvom klastri otázok (rodové obťažovanie), (otázky 2-9)? Aký je výskyt SO v druhom klastri otázok (nechcená sexuálna pozornosť), (otázky 10 - 17)? Aký je výskyt SO v treťom klastri otázok (sexuálne donútenie/násilie), (otázky 18 - 21)?
# aggregate freuqencies
data <- data %>% mutate(
  `Rodovo motivované obťažovanie` = ifelse(rowSums(cbind(q1, q2, q3, q4, q5, q6, q7, q8), na.rm = T) >= 1, ifelse(rowSums(cbind(q1, q2, q3, q4, q5, q6, q7, q8), na.rm = T) >= 2, 2, 1), 0),
  `Nechcená sexuálna pozornosť` = ifelse(rowSums(cbind(q9, q10, q11, q12, q13, q14, q15, q16), na.rm = T) >= 1, ifelse(rowSums(cbind(q9, q10, q11, q12, q13, q14, q15, q16), na.rm = T) >= 2, 2, 1), 0),
  `Sexuálny nátlak` = ifelse(rowSums(cbind(q17, q18, q19, q20), na.rm = T) >= 1, ifelse(rowSums(cbind(q17, q18, q19, q20), na.rm = T) >= 2, 2, 1), 0),
  harrassed = factor(ifelse(rowSums(cbind(`Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`)) > 0, 1, 0)),
  harrassedSeverity = case_when(`Sexuálny nátlak` == 2 ~ 6,
                                `Sexuálny nátlak` == 1 ~ 5,
                                `Nechcená sexuálna pozornosť` == 2 ~ 4,
                                `Nechcená sexuálna pozornosť` == 1 ~ 3,
                                `Rodovo motivované obťažovanie` == 2 ~ 2,
                                `Rodovo motivované obťažovanie` == 1 ~ 1),
  harrassedSeverity = ifelse(is.na(harrassedSeverity), 0, harrassedSeverity)
)

###
# frequency table (n's and %) for aggregate categories of abuse across the board
rq1.2_cluster_n <- data %>%
  summarise_at(vars(`Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`),
               funs(n = round(svytotal(as.logical(.), design, na.rm = T), 0),
                    perc = round(svytotal(as.logical(.), design, na.rm = T), 3)*100/sum(w)))

# frequency tables (n's and %) for aggregate categories of abuse, showing percentages for one-time and repeated abuses
rq1.2_cluster_exposure <- data %>% select(`Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`) %>% map(., ~round(prop.table(wtd.table(., weights = data$w)), 2))

# frequency tables (n's and %) for aggregate categories of abuse by gender.
# dropping other than female, male
rq1.2_clusterGender_n <- data %>%
  group_by(factor(Rod)) %>%
  summarise_at(vars(`Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`),
               funs(n = round(weighted.mean(as.logical(.), w, na.rm = T)*sum(w), 0),
                    perc = weighted.mean(as.logical(.), w, na.rm = T)*100)) %>%
  na.omit()

rq1.2_clusterGender_plotData <- as_tibble(cbind(item = names(rq1.2_clusterGender_n), t(rq1.2_clusterGender_n)))[-1,] %>% filter(!grepl("_n", item, fixed = TRUE)) %>%
  mutate(male = as.numeric(V2), female = as.numeric(V3), item = str_remove(item, "_perc"), V2 = NULL, V3 = NULL) %>% pivot_longer(cols = c(male, female), names_to = "gender", values_to = "perc")

rq1.2_clusterGender_plot <- ggplotly(rq1.2_clusterGender_plotData %>% mutate(gender = fct_recode(gender, "Muži" = "male", "Ženy" = "female")) %>%
                                     ggplot(aes(x = item, y = perc, fill = gender)) + geom_bar(stat = "identity",  alpha = .6, position = "dodge") +
                                     labs(title = "Klastre sexuálneho obťažovania podľa pohlavia", x = "", y = "Počet participantov") + scale_fill_manual("Pohlavie", values = c("Ženy" = cbPalette[1], "Muži" = cbPalette[2])) +
                                     coord_flip() + scale_x_discrete(limits = rev(rq1.2_clusterGender_plotData$item), labels = c(`Rodovo motivované obťažovanie` = "Rodovo motivované\nobťažovanie", `Nechcená sexuálna pozornosť` = "Nechcená sexuálna\npozornosť", `Sexuálny nátlak` = "Sexuálne\nnásilie")))

# Computes the weighted proportions (prevalence rates) and CIs of CSA forms in girls
rq1.2_female_ci <- data[data$Rod == 1,] %>%  select(starts_with("q") & !contains("_"), `Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`) %>% map(~as.integer(wtd.table(as.logical(.), weights = data[data$Rod ==1,]$w, normwt = T))) %>%
  map(~binom.exact(.[2], n = .[1] + .[2])[3:5]*100) %>% map(~round(., 2)) %>% rbindlist(., use.names=TRUE, idcol="Forma SO u dievčat")
names(rq1.2_female_ci)[2:4] <- c("Prevalencia v %", "CI spodný", "CI horný")
rq1.2_female_ci

# Computes the weighted proportions (prevalence rates) and CIs of CSA forms in boys
rq1.2_male_ci <- data[data$Rod == 0,] %>%  select(starts_with("q") & !contains("_"), `Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`) %>% map(~as.integer(wtd.table(as.logical(.), weights = data[data$Rod == 0,]$w, normwt = T))) %>%
  map(~binom.exact(.[2], n = .[1] + .[2])[3:5]*100) %>% map(~round(., 2)) %>% rbindlist(., use.names=TRUE, idcol="Forma SO u chlapcov")
names(rq1.2_male_ci)[2:4] <- c("Prevalencia v %", "CI spodný", "CI horný")
rq1.2_male_ci

# Computes odds ratios ($measure) for: type of abuse by gender contingency tables.
rq1.2_clusterGender_or  <- data %>% mutate(`Rodovo motivované obťažovanie` = as.logical(`Rodovo motivované obťažovanie`), `Nechcená sexuálna pozornosť` = as.logical(`Nechcená sexuálna pozornosť`), `Sexuálny nátlak` = as.logical(`Sexuálny nátlak`)) %>%
  select(starts_with("q") & !contains("_"), `Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`) %>%
  map(~riskratio.boot(round(wtd.table(data$Rod, as.logical(.), weights = data$w, normwt = T), 0), replicates = 1e5)) %>% map(~.$measure[c(2,4,6)])
rq1.2_clusterGender_or <- data.frame(t(sapply(rq1.2_clusterGender_or,c)))
names(rq1.2_clusterGender_or) <- c("Relatívne riziko", "CI spodný", "CI horný")

# Computes and adds Bayes factors (Poisson BF)  for: type of abuse by gender contingency tables.
rq1.2_clusterGender_bf <- data %>% mutate(`Rodovo motivované obťažovanie` = as.logical(`Rodovo motivované obťažovanie`), `Nechcená sexuálna pozornosť` = as.logical(`Nechcená sexuálna pozornosť`), `Sexuálny nátlak` = as.logical(`Sexuálny nátlak`)) %>%
  select(starts_with("q") & !contains("_"), `Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`) %>%
  map(~contingencyTableBF(round(wtd.table(data$Rod, as.logical(.), weights = data$w, normwt = T), 0), sampleType = "poisson", priorConcentration = 1)) %>%
  map(~paste(ifelse(extractBF(.)$bf > 1, "BF10 = ", "BF01 = "), round(ifelse(extractBF(.)$bf > 1, as.numeric(format(extractBF(.)$bf, digits = 3, nsmall = 3)), 1/extractBF(.)$bf), 2), sep = ""))

rq1.2_clusterGender_table <- cbind(round(rq1.2_clusterGender_or, 2), "Bayesov faktor" = t(data.frame(t(sapply(rq1.2_clusterGender_bf,c)))))

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

rfData <- data %>% select(Rod, Neheterosexuál, Minorita, `Vek viac ako 23`, Veriaci, `Iný materinský jazyk`)
rfDataMultinomial <- data %>% select(`Rok štúdia`, `Odbor štúdia`, `Región fakulty`)

# Computes the CIs of proportions for the risk factors
rq2_props_ci <- rfData %>% map(~binom.exact(round(wtd.table(., weights = data$w, normwt = T), 0), n = length(.)))

# Computes odds ratios ($measure) for `Rodovo motivované obťažovanie`
rq2_gmh_or <- rfData %>% map(~riskratio.boot(round(wtd.table(., as.logical(data$`Rodovo motivované obťažovanie`), weights = data$w, normwt = T), 0), replicates = 1e5)) %>%
  map(~.$measure[c(2,4,6)])
rq2_gmh_or <- data.frame(t(sapply(rq2_gmh_or,c)))
names(rq2_gmh_or) <- c("Relatívne riziko", "CI spodný", "CI horný")

# Computes odds ratios ($measure) for `Nechcená sexuálna pozornosť`
rq2_usa_or <- rfData %>% map(~riskratio.boot(round(wtd.table(., as.logical(data$`Nechcená sexuálna pozornosť`), weights = data$w, normwt = T), 0), replicates = 1e5)) %>%
  map(~.$measure[c(2,4,6)])
rq2_usa_or <- data.frame(t(sapply(rq2_usa_or,c)))
names(rq2_usa_or) <- c("Relatívne riziko", "CI spodný", "CI horný")

# Computes odds ratios ($measure) for `Sexuálny nátlak`
rq2_sab_or <- rfData %>% map(~riskratio.boot(round(wtd.table(., as.logical(data$`Sexuálny nátlak`), weights = data$w, normwt = T), 0), replicates = 1e5)) %>%
  map(~.$measure[c(2,4,6)])
rq2_sab_or <- data.frame(t(sapply(rq2_sab_or,c)))
names(rq2_sab_or) <- c("Relatívne riziko", "CI spodný", "CI horný")

# Bayes factors (Poisson BF10, BF01)  for: participant risk factors
# by `Rodovo motivované obťažovanie` contingency tables
rq2_gmh_bf <- rfData %>% map(~contingencyTableBF(round(wtd.table(., as.logical(data$`Rodovo motivované obťažovanie`), weights = data$w, normwt = T), 0), sampleType = "poisson", priorConcentration = 1)) %>%
  map(~paste(ifelse(extractBF(.)$bf > 1, "BF10 = ", "BF01 = "), round(ifelse(extractBF(.)$bf > 1, as.numeric(format(extractBF(.)$bf, digits=3, nsmall=3)), 1/extractBF(.)$bf), 2), sep = ""))

rq2_gmh_or_table <- cbind(round(rq2_gmh_or, 2), "Bayesov faktor" = t(data.frame(t(sapply(rq2_gmh_bf,c)))))

# Bayes factors (Poisson BF10, BF01)  for: participant risk factors
# by `Nechcená sexuálna pozornosť` contingency tables
rq2_usa_bf <- rfData %>% map(~contingencyTableBF(round(wtd.table(., as.logical(data$`Nechcená sexuálna pozornosť`), weights = data$w, normwt = T), 0), sampleType = "poisson", priorConcentration = 1)) %>%
  map(~paste(ifelse(extractBF(.)$bf > 1, "BF10 = ", "BF01 = "), round(ifelse(extractBF(.)$bf > 1, as.numeric(format(extractBF(.)$bf, digits=3, nsmall=3)), 1/extractBF(.)$bf), 2), sep = ""))

rq2_usa_or_table <- cbind(round(rq2_usa_or, 2), "Bayesov faktor" = t(data.frame(t(sapply(rq2_usa_bf,c)))))

# Bayes factors (Poisson BF10, BF01)  for: participant risk factors
# by `Sexuálny nátlak` contingency tables
rq2_sab_bf <- rfData %>% map(~contingencyTableBF(round(wtd.table(., as.logical(data$`Sexuálny nátlak`), weights = data$w, normwt = T), 0), sampleType = "poisson", priorConcentration = 1)) %>%
  map(~paste(ifelse(extractBF(.)$bf > 1, "BF10 = ", "BF01 = "), round(ifelse(extractBF(.)$bf > 1, as.numeric(format(extractBF(.)$bf, digits=3, nsmall=3)), 1/extractBF(.)$bf), 2), sep = ""))

rq2_sab_or_table <- cbind(round(rq2_sab_or, 2), "Bayesov faktor" = t(data.frame(t(sapply(rq2_sab_bf,c)))))

# Multinomial risk factors
# Computes the CIs of proportions for the risk factors
rq2_props_ci <- rfDataMultinomial %>% map(~binom.exact(round(wtd.table(., weights = data$w, normwt = T), 0), n = length(.)))

# Computes odds ratios ($measure) for `Rodovo motivované obťažovanie`
rq2_gmh_or_multinomial <- rfDataMultinomial %>% map(~riskratio.boot(round(wtd.table(., as.logical(data$`Rodovo motivované obťažovanie`), weights = data$w, normwt = T), 0), replicates = 1e5)) %>%
  map(~.$measure)

# Computes odds ratios ($measure) for `Nechcená sexuálna pozornosť`
rq2_usa_or_multinomial <- rfDataMultinomial %>% map(~riskratio.boot(round(wtd.table(., as.logical(data$`Nechcená sexuálna pozornosť`), weights = data$w, normwt = T), 0), replicates = 1e5)) %>%
  map(~.$measure)

# Computes odds ratios ($measure) for `Sexuálny nátlak`
rq2_sab_or_multinomial <- rfDataMultinomial %>% map(~riskratio.boot(round(wtd.table(., as.logical(data$`Sexuálny nátlak`), weights = data$w, normwt = T), 0), replicates = 1e5)) %>%
  map(~.$measure)

# Bayes factors (Poisson BF10, BF01)  for: participant risk factors
# by `Rodovo motivované obťažovanie` contingency tables
rq2_gmh_bf_multinomial <- rfDataMultinomial %>% map(~contingencyTableBF(round(wtd.table(., as.logical(data$`Rodovo motivované obťažovanie`), weights = data$w, normwt = T), 0), sampleType = "poisson", priorConcentration = 1)) %>%
  map(~paste(ifelse(extractBF(.)$bf > 1, "BF10 = ", "BF01 = "), round(ifelse(extractBF(.)$bf > 1, as.numeric(format(extractBF(.)$bf, digits=3, nsmall=3)), 1/extractBF(.)$bf), 2), sep = ""))

rq2_gmh_or_table_multinomial <- t(data.frame(t(sapply(rq2_gmh_bf_multinomial,c))))

# Bayes factors (Poisson BF10, BF01)  for: participant risk factors
# by `Nechcená sexuálna pozornosť` contingency tables
rq2_usa_bf_multinomial <- rfDataMultinomial %>% map(~contingencyTableBF(round(wtd.table(., as.logical(data$`Nechcená sexuálna pozornosť`), weights = data$w, normwt = T), 0), sampleType = "poisson", priorConcentration = 1)) %>%
  map(~paste(ifelse(extractBF(.)$bf > 1, "BF10 = ", "BF01 = "), round(ifelse(extractBF(.)$bf > 1, as.numeric(format(extractBF(.)$bf, digits=3, nsmall=3)), 1/extractBF(.)$bf), 2), sep = ""))

rq2_usa_or_table_multinomial <- t(data.frame(t(sapply(rq2_usa_bf_multinomial,c))))

# Bayes factors (Poisson BF10, BF01)  for: participant risk factors
# by `Sexuálny nátlak` contingency tables
rq2_sab_bf_multinomial <- rfDataMultinomial %>% map(~contingencyTableBF(round(wtd.table(., as.logical(data$`Sexuálny nátlak`), weights = data$w, normwt = T), 0), sampleType = "poisson", priorConcentration = 1)) %>%
  map(~paste(ifelse(extractBF(.)$bf > 1, "BF10 = ", "BF01 = "), round(ifelse(extractBF(.)$bf > 1, as.numeric(format(extractBF(.)$bf, digits=3, nsmall=3)), 1/extractBF(.)$bf), 2), sep = ""))

rq2_sab_or_table_multinomial <- t(data.frame(t(sapply(rq2_sab_bf_multinomial,c))))

###

# Regions

# Computes odds ratios ($measure) for: type of abuse by region
rq2_region_or <- data %>% mutate(`Rodovo motivované obťažovanie` = as.logical(`Rodovo motivované obťažovanie`), `Nechcená sexuálna pozornosť` = as.logical(`Nechcená sexuálna pozornosť`), `Sexuálny nátlak` = as.logical(`Sexuálny nátlak`)) %>% select(`Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`, harrassed) %>%
  map(~riskratio.boot(round(wtd.table(data$`Región fakulty`, ., weights = data$w, normwt = T), 0), rev = "rows", replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by region
rq2_region_bf <- data %>% mutate(`Rodovo motivované obťažovanie` = as.logical(`Rodovo motivované obťažovanie`), `Nechcená sexuálna pozornosť` = as.logical(`Nechcená sexuálna pozornosť`), `Sexuálny nátlak` = as.logical(`Sexuálny nátlak`)) %>% select(`Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`, harrassed) %>%
  map(~contingencyTableBF(round(wtd.table(data$`Región fakulty`, ., weights = data$w, normwt = T), 0), sampleType = "poisson", priorConcentration = 1))

rq2_region_mod <- lm(harrassedSeverity ~ `Región fakulty`, weights = data$w, data)
rq2_region_bf <- lmBF(harrassedSeverity ~ `Región fakulty`, data, rscaleEffects = rScale)
rq2_region_summary <- summary(rq2_region_mod)
rq2_region_anova <- Anova(rq2_region_mod)

# Length of study

# Computes odds ratios ($measure) for: type of abuse by length of study
rq2_studyLengthMod <- lm(harrassedSeverity ~ as.numeric(`Rok štúdia`), weights = data$w , data)
rq2_studyLengthBF <- data[!is.na(data$`Rok štúdia`),] %>% mutate(`Rok štúdia` == as.numeric(`Rok štúdia`)) %$% lmBF(harrassedSeverity ~ `Rok štúdia`, data = .,rscaleEffects = rScale)
rq2_studyLengthSummary <- summary(rq2_studyLengthMod)
rq2_studyLengthAnova <- Anova(rq2_studyLengthMod)

# Language
# Computes odds ratios ($measure) for: type of abuse by language.
rq2_language_or <- data %>% mutate(`Rodovo motivované obťažovanie` = as.logical(`Rodovo motivované obťažovanie`), `Nechcená sexuálna pozornosť` = as.logical(`Nechcená sexuálna pozornosť`), `Sexuálny nátlak` = as.logical(`Sexuálny nátlak`)) %>% select(`Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`, harrassed) %>%
  map(~riskratio.boot(round(wtd.table(data$`Iný materinský jazyk`, ., weights = data$w, normwt = T), 0), replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by language
rq2_language_bf <- data %>% mutate(`Rodovo motivované obťažovanie` = as.logical(`Rodovo motivované obťažovanie`), `Nechcená sexuálna pozornosť` = as.logical(`Nechcená sexuálna pozornosť`), `Sexuálny nátlak` = as.logical(`Sexuálny nátlak`)) %>% select(`Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`, harrassed) %>%
  map(~contingencyTableBF(round(wtd.table(data$`Iný materinský jazyk`, ., weights = data$w, normwt = T), 0), sampleType = "poisson", priorConcentration = 1))

rq2_language_mod <- lm(harrassedSeverity ~ `Iný materinský jazyk`, weights = data$w , data)
rq2_language_bf <- lmBF(harrassedSeverity ~ `Iný materinský jazyk`, data[!is.na(data$`Iný materinský jazyk`),], rscaleEffects = rScale)
rq2_language_summary <- summary(rq2_language_mod)
rq2_language_anova <- Anova(rq2_language_mod)

# Religious belief

# Computes odds ratios ($measure) for: type of abuse by religious belief
rq2_belief_or <- data %>% mutate(`Rodovo motivované obťažovanie` = as.logical(`Rodovo motivované obťažovanie`), `Nechcená sexuálna pozornosť` = as.logical(`Nechcená sexuálna pozornosť`), `Sexuálny nátlak` = as.logical(`Sexuálny nátlak`)) %>% select(`Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`, harrassed) %>%
  map(~riskratio.boot(round(wtd.table(data$Veriaci, ., weights = data$w, normwt = T), 0), replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by religious belief
rq2_belief_bf <- data %>% mutate(`Rodovo motivované obťažovanie` = as.logical(`Rodovo motivované obťažovanie`), `Nechcená sexuálna pozornosť` = as.logical(`Nechcená sexuálna pozornosť`), `Sexuálny nátlak` = as.logical(`Sexuálny nátlak`)) %>% select(`Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`, harrassed) %>%
  map(~contingencyTableBF(round(wtd.table(data$Veriaci, ., weights = data$w, normwt = T), 0), sampleType = "poisson", priorConcentration = 1))

rq2_belief_mod <- lm(harrassedSeverity ~ Veriaci, weights = data$w , data)
rq2_belief_bf <- lmBF(harrassedSeverity ~ Veriaci, data[!is.na(data$Veriaci),], rscaleEffects = rScale)
rq2_belief_summary <- summary(rq2_belief_mod)
rq2_belief_anova <- Anova(rq2_belief_mod)

# Minority
# Computes odds ratios ($measure) for: type of abuse by Minorita status
rq2_minority_or <- data %>% mutate(`Rodovo motivované obťažovanie` = as.logical(`Rodovo motivované obťažovanie`), `Nechcená sexuálna pozornosť` = as.logical(`Nechcená sexuálna pozornosť`), `Sexuálny nátlak` = as.logical(`Sexuálny nátlak`)) %>% select(`Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`, harrassed) %>%
  map(~riskratio.boot(round(wtd.table(data$Minorita, ., weights = data$w, normwt = T), 0), replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by Minorita status
rq2_minority_bf <- data %>% mutate(`Rodovo motivované obťažovanie` = as.logical(`Rodovo motivované obťažovanie`), `Nechcená sexuálna pozornosť` = as.logical(`Nechcená sexuálna pozornosť`), `Sexuálny nátlak` = as.logical(`Sexuálny nátlak`)) %>% select(`Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`, harrassed) %>%
  map(~contingencyTableBF(round(wtd.table(data$Minorita, ., weights = data$w, normwt = T), 0), sampleType = "poisson", priorConcentration = 1))

rq2_minority_mod <- lm(harrassedSeverity ~ Minorita, weights = data$w , data)
rq2_minority_bf <- lmBF(harrassedSeverity ~ Minorita, data[!is.na(data$Minorita),], rscaleEffects = rScale)
rq2_minority_summary <- summary(rq2_minority_mod)
rq2_minority_anova <- Anova(rq2_minority_mod)

# Sexual orientation
# Computes odds ratios ($measure) for: type of abuse by sexual orientation
rq2_orientation_or <- data %>% mutate(`Rodovo motivované obťažovanie` = as.logical(`Rodovo motivované obťažovanie`), `Nechcená sexuálna pozornosť` = as.logical(`Nechcená sexuálna pozornosť`), `Sexuálny nátlak` = as.logical(`Sexuálny nátlak`)) %>% select(`Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`, harrassed) %>%
  map(~riskratio.boot(round(wtd.table(data$Neheterosexuál, ., weights = data$w, normwt = T), 0), replicates = 1e5))

# Computes Bayes factors (Poisson BF)  for: type of abuse by sexual orientation
rq2_orientation_bf <- data %>% mutate(`Rodovo motivované obťažovanie` = as.logical(`Rodovo motivované obťažovanie`), `Nechcená sexuálna pozornosť` = as.logical(`Nechcená sexuálna pozornosť`), `Sexuálny nátlak` = as.logical(`Sexuálny nátlak`)) %>% select(`Rodovo motivované obťažovanie`, `Nechcená sexuálna pozornosť`, `Sexuálny nátlak`, harrassed) %>%
  map(~contingencyTableBF(round(wtd.table(data$Neheterosexuál, ., weights = data$w, normwt = T), 0), sampleType = "poisson", priorConcentration = 1))

rq2_orientation_mod <- lm(harrassedSeverity ~ Neheterosexuál, weights = data$w , data)
rq2_orientation_bf <- lmBF(harrassedSeverity ~ Neheterosexuál, data[!is.na(data$Neheterosexuál),], rscaleEffects = rScale)
rq2_orientation_summary <- summary(rq2_orientation_mod)
rq2_orientation_anova <- Anova(rq2_orientation_mod)

# RQ3 Who are the perpetrators---------------------------------------------------------------------
# Aký je pomer pohlaví páchateľov pri všetkých formách SO spolu?
# Aký je pomer pohlaví páchateľov pri jednotlivých formách SO osobitne?

# Overall count
# Kto sú páchatelia SO?
rq3_perpetratorsItems_n <- do.call(rbind.data.frame, data %>% select(contains("_who") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data$w, normwt = T), 0))))) %>%
  rownames_to_column("who")

data %>% select(contains("_who") & !contains("Other")) %>% map(~table(.))


# Kto sú pachatelia SO v rámci jednotlivých foriem SO
seqq <- paste0("q", 1:20, "_")
rq3_perpByItem <- list()
for(i in seqq){
rq3_perpByItem[[i]] <- do.call(rbind.data.frame, data %>% select(contains("_who") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data$w, normwt = T), 0), "perc" = round(prop.table(wtd.table(., weights = data$w, normwt = T)), 2)))) %>%
  rownames_to_column("who") %>% filter(grepl(".1", who, fixed = TRUE)) %>% filter(str_detect(who, i))
}
rq3_perpetratorsByItem_most <- rq3_perpByItem %>% map(~arrange(., desc(freq))[1,]) %$% data.frame(t(sapply(.,c)))
rq3_perpetratorsByItem_2ndmost <- rq3_perpByItem %>% map(~arrange(., desc(freq))[2,]) %$% data.frame(t(sapply(.,c)))
rq3_perpetratorsByItem_most <- rq3_perpetratorsByItem_most %>% mutate(who = str_replace_all(who, c(".*Student_M.1.*" = "Študent Muž",
                                                                    ".*Teacher_M.1.*" = "Učiteľ Muž",
                                                                    ".*Teacher_M.1.*" = "Učiteľ Muž",
                                                                    ".*Teacher_F.1.*" = "Učiteľ Žena",
                                                                    ".*Student_F.1.*" = "Študent Žena"))) %>% rename("1. najčastejší" = who,
                                                                                                                     "N" = freq,
                                                                                                                     "Percent" = perc)

rq3_perpetratorsByItem_2ndmost <- rq3_perpetratorsByItem_2ndmost %>% mutate(who = str_replace_all(who, c(".*Student_M.1.*" = "Študent Muž",
                                                                       ".*Teacher_M.1.*" = "Učiteľ Muž",
                                                                       ".*Teacher_M.1.*" = "Učiteľ Muž",
                                                                       ".*Teacher_F.1.*" = "Učiteľ Žena",
                                                                       ".*Student_F.1.*" = "Študent Žena",
                                                                       ".*Employee_F.1.*" = "Zamestnanec Žena"))) %>% rename("2. najčastejší" = who,
                                                                                                                             "N" = freq,
                                                                                                                             "Percent" = perc)

rq3_perpOverallMost <- cbind(rq3_perpetratorsByItem_most, rq3_perpetratorsByItem_2ndmost)
rownames(rq3_perpOverallMost) <- noquote(sprintf("q%d", 1:20))

# Kto sú pachatelia SO v rámci jednotlivých foriem SO u dievčat
rq3_perpByItem_female <- list()
for(i in seqq){
  rq3_perpByItem_female[[i]] <- do.call(rbind.data.frame, data[data$Rod == 1,] %>% select(contains("_who") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data[data$Rod == 1,]$w, normwt = T), 0), "perc" = round(prop.table(wtd.table(., weights = data[data$Rod == 1,]$w, normwt = T)), 2)))) %>%
    rownames_to_column("who") %>% filter(grepl(".1", who, fixed = TRUE)) %>% filter(str_detect(who, i))
}
rq3_perpetratorsByItem_female_most <- rq3_perpByItem_female %>% map(~arrange(., desc(freq))[1,]) %$% data.frame(t(sapply(.,c))) %>%
  mutate(who = str_replace_all(who, c(".*Student_M.1.*" = "Študent Muž",
                                      ".*Teacher_M.1.*" = "Učiteľ Muž",
                                      ".*Teacher_M.1.*" = "Učiteľ Muž",
                                      ".*Teacher_F.1.*" = "Učiteľ Žena",
                                      ".*Student_F.1.*" = "Študent Žena"))) %>% rename("1. najčastejší" = who,
                                                                                       "N" = freq,
                                                                                       "Percent" = perc)

rq3_perpetratorsByItem_female_2ndmost <- rq3_perpByItem_female %>% map(~arrange(., desc(freq))[2,]) %$% data.frame(t(sapply(.,c))) %>%
  mutate(who = str_replace_all(who, c(".*Student_M.1.*" = "Študent Muž",
                                      ".*Teacher_M.1.*" = "Učiteľ Muž",
                                      ".*Teacher_M.1.*" = "Učiteľ Muž",
                                      ".*Teacher_F.1.*" = "Učiteľ Žena",
                                      ".*Student_F.1.*" = "Študent Žena",
                                      ".*Employee_F.1.*" = "Zamestnanec Žena",
                                      ".*Employee_M.1.*" = "Zamestnanec Muž"))) %>% rename("2. najčastejší" = who,
                                                                                            "N" = freq,
                                                                                            "Percent" = perc)

rq3_perpOverallMostFemale <- cbind(rq3_perpetratorsByItem_female_most, rq3_perpetratorsByItem_female_2ndmost)
rownames(rq3_perpOverallMostFemale) <- noquote(sprintf("q%d", 1:20))

# Kto sú pachatelia SO v rámci jednotlivých foriem SO u chlapcov
rq3_perpByItem_male <- list()
for(i in seqq){
  rq3_perpByItem_male[[i]] <- do.call(rbind.data.frame, data[data$Rod == 0,] %>% select(contains("_who") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data[data$Rod == 0,]$w, normwt = T), 0), "perc" = round(prop.table(wtd.table(., weights = data[data$Rod == 0,]$w, normwt = T)), 2)))) %>%
    rownames_to_column("who") %>% filter(grepl(".1", who, fixed = TRUE)) %>% filter(str_detect(who, i))
}
rq3_perpetratorsByItem_male_most <- rq3_perpByItem_male %>% map(~arrange(., desc(freq))[1,]) %$% data.frame(t(sapply(.,c))) %>%
  mutate(who = str_replace_all(who, c(".*Student_M.1.*" = "Študent Muž",
                                      ".*Teacher_M.1.*" = "Učiteľ Muž",
                                      ".*Teacher_M.1.*" = "Učiteľ Muž",
                                      ".*Teacher_F.1.*" = "Učiteľ Žena",
                                      ".*Student_F.1.*" = "Študent Žena"))) %>% rename("1. najčastejší" = who,
                                                                                       "N" = freq,
                                                                                       "Percent" = perc)

rq3_perpetratorsByItem_male_2ndmost <- rq3_perpByItem_male %>% map(~arrange(., desc(freq))[2,]) %$% data.frame(t(sapply(.,c))) %>%
  mutate(who = str_replace_all(who, c(".*Student_M.1.*" = "Študent Muž",
                                      ".*Teacher_M.1.*" = "Učiteľ Muž",
                                      ".*Teacher_M.1.*" = "Učiteľ Muž",
                                      ".*Teacher_F.1.*" = "Učiteľ Žena",
                                      ".*Student_F.1.*" = "Študent Žena",
                                      ".*Employee_F.1.*" = "Zamestnanec Žena",
                                      ".*Employee_M.1.*" = "Zamestnanec Muž"))) %>% rename("2. najčastejší" = who,
                                                                                           "N" = freq,
                                                                                           "Percent" = perc)

rq3_perpOverallMostMmale <- cbind(rq3_perpetratorsByItem_male_most, rq3_perpetratorsByItem_male_2ndmost)[-18,]
rownames(rq3_perpOverallMostMmale) <- noquote(sprintf("q%d", 1:20))[-18]

whos <- c("Teacher_M", "Teacher_F", "Student_M", "Student_F", "Employee_M", "Employee_F")
rq3_perpetratorsOverall_n <- list()
for(n in 1:6){
  for(i in whos){
    rq3_perpetratorsOverall_n[[i]] <- rq3_perpetratorsItems_n %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}

rq3_perpetratorsOverall_table <- round(unlist(rq3_perpetratorsOverall_n)*100/sum(unlist(rq3_perpetratorsOverall_n)), 2)
names(rq3_perpetratorsOverall_table) <- gsub(".count.freq", "", names(rq3_perpetratorsOverall_table))

# Count per cluster overall
# GenderMotivHarr
whoGenderMotivHarr <- do.call(rbind.data.frame, data %>% select(q1:q9 & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data$w, normwt = T), 0))))) %>% rownames_to_column("who")
rq3_perpetratorsGMH_n <- list(NA)
for(n in 1:6){
  for(i in whos){
    rq3_perpetratorsGMH_n[[i]] <- whoGenderMotivHarr %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq3_perpetratorsGMH_n[1] <- NULL

# UnwantedSexAtt
whoUnwantedSexAtt <- do.call(rbind.data.frame, data %>% select(q9:q17 & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data$w, normwt = T), 0))))) %>% rownames_to_column("who")
rq3_perpetratorsUSA_n <- list(NA)
for(n in 1:6){
  for(i in whos){
    rq3_perpetratorsUSA_n[[i]] <- whoUnwantedSexAtt %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq3_perpetratorsUSA_n[1] <- NULL

# SexAbuse
whoSexAbuse <- do.call(rbind.data.frame, data %>% select(q17:anyYes & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data$w, normwt = T), 0))))) %>% rownames_to_column("who")
rq3_perpetratorsSAB_n <- NA
for(n in 1:6){
  for(i in whos){
    rq3_perpetratorsSAB_n[[i]] <- whoSexAbuse %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq3_perpetratorsSAB_n[1] <- NULL

perpertatorsTable <- round(rbind("GMH" = unlist(rq3_perpetratorsGMH_n)*100/sum(unlist(rq3_perpetratorsGMH_n)), "USA" = unlist(rq3_perpetratorsUSA_n)*100/sum(unlist(rq3_perpetratorsUSA_n)), "SAB" = unlist(rq3_perpetratorsSAB_n)*100/sum(unlist(rq3_perpetratorsSAB_n))), 2)
colnames(perpertatorsTable) <- c("Učiteľ Muž",	"Učiteľ Žena",	"Študent Muž",	"Študent Žena",	"Zamestnanec Muž",	"Zamestnanec Žena")
rownames(perpertatorsTable) <- c("Rodovo motivované obťažovanie", "Nechcená sexuálna pozornosť", "Sexuálny nátlak")


# Pre dievčatá
# GenderMotivHarr
whoGenderMotivHarr_female <- do.call(rbind.data.frame, data[data$Rod == 1,] %>% select(q1:q9 & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data[data$Rod == 1,]$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data[data$Rod == 1,]$w, normwt = T), 0))))) %>% rownames_to_column("who")
rq3_perpetratorsGMH_n_female <- list(NA)
for(n in 1:6){
  for(i in whos){
    rq3_perpetratorsGMH_n_female[[i]] <- whoGenderMotivHarr_female %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq3_perpetratorsGMH_n_female[1] <- NULL

# UnwantedSexAtt
whoUnwantedSexAtt_female <- do.call(rbind.data.frame, data[data$Rod == 1,] %>% select(q9:q17 & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data[data$Rod == 1,]$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data[data$Rod == 1,]$w, normwt = T), 0))))) %>% rownames_to_column("who")
rq3_perpetratorsUSA_n_female <- list(NA)
for(n in 1:6){
  for(i in whos){
    rq3_perpetratorsUSA_n_female[[i]] <- whoUnwantedSexAtt_female %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq3_perpetratorsUSA_n_female[1] <- NULL

# SexAbuse
whoSexAbuse_female <- do.call(rbind.data.frame, data[data$Rod == 1,] %>% select(q17:anyYes & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data[data$Rod == 1,]$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data[data$Rod == 1,]$w, normwt = T), 0))))) %>% rownames_to_column("who")
rq3_perpetratorsSAB_n_female <- NA
for(n in 1:6){
  for(i in whos){
    rq3_perpetratorsSAB_n_female[[i]] <- whoSexAbuse_female %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq3_perpetratorsSAB_n_female[1] <- NULL

perpertatorsTable_female <- round(rbind("GMH" = unlist(rq3_perpetratorsGMH_n_female)*100/sum(unlist(rq3_perpetratorsGMH_n_female)), "USA" = unlist(rq3_perpetratorsUSA_n_female)*100/sum(unlist(rq3_perpetratorsUSA_n_female)), "SAB" = unlist(rq3_perpetratorsSAB_n_female)*100/sum(unlist(rq3_perpetratorsSAB_n_female))), 2)
colnames(perpertatorsTable_female) <- c("Učiteľ Muž",	"Učiteľ Žena",	"Študent Muž",	"Študent Žena",	"Zamestnanec Muž",	"Zamestnanec Žena")
rownames(perpertatorsTable_female) <- c("Rodovo motivované obťažovanie", "Nechcená sexuálna pozornosť", "Sexuálny nátlak")

# Pre chlapcov
# GenderMotivHarr
whoGenderMotivHarr_male <- do.call(rbind.data.frame, data[data$Rod == 0,] %>% select(q1:q9 & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data[data$Rod == 0,]$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data[data$Rod == 0,]$w, normwt = T), 0))))) %>% rownames_to_column("who")
rq3_perpetratorsGMH_n_male <- list(NA)
for(n in 1:6){
  for(i in whos){
    rq3_perpetratorsGMH_n_male[[i]] <- whoGenderMotivHarr_male %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq3_perpetratorsGMH_n_male[1] <- NULL

# UnwantedSexAtt
whoUnwantedSexAtt_male <- do.call(rbind.data.frame, data[data$Rod == 0,] %>% select(q9:q17 & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data[data$Rod == 0,]$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data[data$Rod == 0,]$w, normwt = T), 0))))) %>% rownames_to_column("who")
rq3_perpetratorsUSA_n_male <- list(NA)
for(n in 1:6){
  for(i in whos){
    rq3_perpetratorsUSA_n_male[[i]] <- whoUnwantedSexAtt_male %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq3_perpetratorsUSA_n_male[1] <- NULL

# SexAbuse
whoSexAbuse_male <- do.call(rbind.data.frame, data[data$Rod == 0,] %>% select(q17:anyYes & contains("_who") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data[data$Rod == 0,]$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data[data$Rod == 0,]$w, normwt = T), 0))))) %>% rownames_to_column("who")
rq3_perpetratorsSAB_n_male <- NA
for(n in 1:6){
  for(i in whos){
    rq3_perpetratorsSAB_n_male[[i]] <- whoSexAbuse_male %>% filter(str_detect(who, i) & grepl(".1", who, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq3_perpetratorsSAB_n_male[1] <- NULL

perpertatorsTable_male <- round(rbind("GMH" = unlist(rq3_perpetratorsGMH_n_male)*100/sum(unlist(rq3_perpetratorsGMH_n_male)), "USA" = unlist(rq3_perpetratorsUSA_n_male)*100/sum(unlist(rq3_perpetratorsUSA_n_male)), "SAB" = unlist(rq3_perpetratorsSAB_n_male)*100/sum(unlist(rq3_perpetratorsSAB_n_male))), 2)
colnames(perpertatorsTable_male) <- c("Učiteľ Muž",	"Učiteľ Žena",	"Študent Muž",	"Študent Žena",	"Zamestnanec Muž",	"Zamestnanec Žena")
rownames(perpertatorsTable_male) <- c("Rodovo motivované obťažovanie", "Nechcená sexuálna pozornosť", "Sexuálny nátlak")

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
rq3_abusesByPerpetratorGenderOverall_rr <- riskratio.boot(abusesByPerpetratorGenderOverall, rev = "both", replicates = 1e5)
rq3_abusesByPerpetratorGenderOverall_bf <- contingencyTableBF(abusesByPerpetratorGenderOverall, sampleType = "poisson", priorConcentration = 1)

# Computes odds ratios ($measure) and Bayes factors (Poisson BF) for: type of abuse by gender of the perpetrator for GMH
rq3_abusesByPerpetratorGenderGMH_rr <- riskratio.boot(abusesByPerpetratorGenderGMH, rev = "both", replicates = 1e5)
rq3_abusesByPerpetratorGenderGMH_bf <- contingencyTableBF(abusesByPerpetratorGenderGMH, sampleType = "poisson", priorConcentration = 1)

# Computes odds ratios ($measure) and Bayes factors (Poisson BF) for: type of abuse by gender of the perpetrator for USA
rq3_abusesByPerpetratorGenderUSA_rr <- riskratio.boot(abusesByPerpetratorGenderUSA, rev = "both", replicates = 1e5, correction = TRUE)
rq3_abusesByPerpetratorGenderUSA_bf <- contingencyTableBF(abusesByPerpetratorGenderUSA, sampleType = "poisson", priorConcentration = 1)

# Computes odds ratios ($measure) and Bayes factors (Poisson BF) for: type of abuse by gender of the perpetrator for SAB
rq3_abusesByPerpetratorGenderSAB_rr <- riskratio.boot(abusesByPerpetratorGenderSAB, rev = "both", replicates = 1e5, correction = TRUE)
rq3_abusesByPerpetratorGenderSAB_bf <- contingencyTableBF(abusesByPerpetratorGenderSAB, sampleType = "poisson", priorConcentration = 1)

# RQ4 Where did it happen -------------------------------------------------------------------

rq4_whereItems_n <- do.call(rbind.data.frame, data %>% select(contains("_where") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data$w, normwt = T), 0))))) %>%
  rownames_to_column("where")

wheres <- c("EduProcess", "Break", "Dorm", "Lab", "Practice", "Online", "DontKnow")

rq4_whereOverall_n <- list(NA)
for(n in 1:6){
  for(i in wheres){
    rq4_whereOverall_n[[i]] <- rq4_whereItems_n %>% filter(str_detect(where, i) & grepl(".1", where, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq4_whereOverall_n[1] <- NULL

rq4_whereOverall_table <- round(unlist(rq4_whereOverall_n)*100/sum(unlist(rq4_whereOverall_n)), 2)
names(rq4_whereOverall_table) <- gsub(".count.freq", "", names(rq4_whereOverall_table))

# For females
rq4_whereItems_n_females <- do.call(rbind.data.frame, data[data$Rod == 1,] %>% select(contains("_where") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data[data$Rod == 1,]$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data[data$Rod == 1,]$w, normwt = T), 0))))) %>%
  rownames_to_column("where")

rq4_whereOverall_n_females <- list(NA)
for(n in 1:6){
  for(i in wheres){
    rq4_whereOverall_n_females[[i]] <- rq4_whereItems_n_females %>% filter(str_detect(where, i) & grepl(".1", where, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq4_whereOverall_n_females[1] <- NULL

rq4_whereOverall_table_females <- round(unlist(rq4_whereOverall_n_females)*100/sum(unlist(rq4_whereOverall_n_females)), 2)
names(rq4_whereOverall_table_females) <- gsub(".count.freq", "", names(rq4_whereOverall_table_females))


# For males
rq4_whereItems_n_males <- do.call(rbind.data.frame, data[data$Rod == 0,] %>% select(contains("_where") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data[data$Rod == 0,]$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data[data$Rod == 0,]$w, normwt = T), 0))))) %>%
  rownames_to_column("where")

rq4_whereOverall_n_males <- list(NA)
for(n in 1:6){
  for(i in wheres){
    rq4_whereOverall_n_males[[i]] <- rq4_whereItems_n_males %>% filter(str_detect(where, i) & grepl(".1", where, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq4_whereOverall_n_males[1] <- NULL

rq4_whereOverall_table_males <- round(unlist(rq4_whereOverall_n_males)*100/sum(unlist(rq4_whereOverall_n_males)), 2)
names(rq4_whereOverall_table_males) <- gsub(".count.freq", "", names(rq4_whereOverall_table_males))

rq4where_table <- rbind(rq4_whereOverall_table, rq4_whereOverall_table_females, rq4_whereOverall_table_males)
rownames(rq4where_table) <- c("Celkovo", "Ženy", "Muži")
colnames(rq4where_table) <- c("Výučba", "Prestávka", "Internát", "Laboratórium", "Prax", "Online", "Neviem")

# Existujú rozdiely medzi klastrami SO a prostredím v ktorom sa SO odohráva?
# Count per cluster
# GenderMotivHarr
whereGenderMotivHarr <- do.call(rbind.data.frame, data %>% select(q1:q9 & contains("_where") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data$w, normwt = T), 0))))) %>% rownames_to_column("where")
rq4_whereGMH_n <- list(NA)
for(n in 1:6){
  for(i in wheres){
    rq4_whereGMH_n[[i]] <- whereGenderMotivHarr %>% filter(str_detect(where, i) & grepl(".1", where, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq4_whereGMH_n[1] <- NULL

# UnwantedSexAtt
whereUnwantedSexAtt <- do.call(rbind.data.frame, data %>% select(q9:q17 & contains("_where") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data$w, normwt = T), 0))))) %>% rownames_to_column("where")
rq4_whereUSA_n <- list(NA)
for(n in 1:6){
  for(i in wheres){
    rq4_whereUSA_n[[i]] <- whereUnwantedSexAtt %>% filter(str_detect(where, i) & grepl(".1", where, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq4_whereUSA_n[1] <- NULL

# SexAbuse
whereSexAbuse <- do.call(rbind.data.frame, data %>% select(q17:anyYes & contains("_where") & !contains("Other")) %>% map(~cbind("freq" = round(wtd.table(., weights = data$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data$w, normwt = T), 0))))) %>% rownames_to_column("where")
rq4_whereSAB_n <- NA
for(n in 1:6){
  for(i in wheres){
    rq4_whereSAB_n[[i]] <- whereSexAbuse %>% filter(str_detect(where, i) & grepl(".1", where, fixed = TRUE)) %>% summarise(count = colSums(.["freq"]))
  }
}
rq4_whereSAB_n[1] <- NULL

whereTable <- round(rbind("GMH" = unlist(rq4_whereGMH_n)*100/sum(unlist(rq4_whereGMH_n)), "USA" = unlist(rq4_whereUSA_n)*100/sum(unlist(rq4_whereUSA_n)), "SAB" = unlist(rq4_whereSAB_n)*100/sum(unlist(rq4_whereSAB_n))), 2)
colnames(whereTable) <- c("Výučba", "Prestávka", "Internát", "Laboratórium", "Prax", "Online", "Neviem")
rownames(whereTable) <- c("Rodovo motivované obťažovanie", "Nechcená sexuálna pozornosť", "Sexuálny nátlak")


# RQ5 Abuse Impacts -----------------------------------------------------------
# Subjektívne vnímané dôsledky sexuálneho obťažovania
# RQ5.1 Aké psychické / psychosomatické dôsledky obete najčastejšie uvádzajú?
data <- data %>% mutate(impactSeverity = rowSums(data %>% select(contains("suffer_") & !contains(c("_other", "_none", "dontKnow"))), na.rm = T))

rq5_impacts_n <- do.call(rbind.data.frame, data %>% filter(harrassed == 1) %>% select(contains("suffer_") & !contains(c("_other", "_none", "dontKnow"))) %>%
                           map(~cbind("freq" = round(wtd.table(., weights = data[data$harrassed == 1,]$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data[data$harrassed == 1,]$w, normwt = T), 0))))) %>%
  rownames_to_column("Celkovo") %>%
  filter(grepl(".1", Celkovo, fixed = TRUE)) %>%
  arrange(desc(freq)) %>% mutate(Celkovo = str_remove(Celkovo, ".1"),
                                 perc = round(perc, 2)*100)

rq5_impacts_n <- rq5_impacts_n %>% mutate(Celkovo = recode(Celkovo, "suffer_vulnerable" = "Pocity zraniteľnosti",
                                             "suffer_lostConfidence" = "Strata sebaistoty",
                                             "suffer_anxiety" = "Úzkosť",
                                             "suffer_concetration" = "Problémy s koncentráciou",
                                             "suffer_helplessness" = "Pocity bezmocnosti",
                                             "suffer_depression" = "Depresívne stavy",
                                             "suffer_study" = "Ťažkosti s učením",
                                             "suffer_relationships" = "Vzťahové problémy",
                                             "suffer_sleep" = "Poruchy spánku",
                                             "suffer_eating" = "Poruchy príjmu potravy"))

# U dievčat
rq5_impacts_n_females <- do.call(rbind.data.frame, data[data$harrassed == 1 & data$Rod == 1,] %>% select(contains("suffer_") & !contains(c("_other", "_none", "dontKnow"))) %>%
                           map(~cbind("freq" = round(wtd.table(., weights = data[data$harrassed == 1 & data$Rod == 1,]$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data[data$harrassed == 1 & data$Rod == 1,]$w, normwt = T), 0))))) %>%
  rownames_to_column("Ženy") %>%
  filter(grepl(".1", Ženy, fixed = TRUE)) %>%
  arrange(desc(freq)) %>% mutate(Ženy = str_remove(Ženy, ".1"),
                                 perc = round(perc, 2)*100)

rq5_impacts_n_females <- rq5_impacts_n_females %>% mutate(Ženy = recode(Ženy, "suffer_vulnerable" = "Pocity zraniteľnosti",
                                                              "suffer_lostConfidence" = "Strata sebaistoty",
                                                              "suffer_anxiety" = "Úzkosť",
                                                              "suffer_concetration" = "Problémy s koncentráciou",
                                                              "suffer_helplessness" = "Pocity bezmocnosti",
                                                              "suffer_depression" = "Depresívne stavy",
                                                              "suffer_study" = "Ťažkosti s učením",
                                                              "suffer_relationships" = "Vzťahové problémy",
                                                              "suffer_sleep" = "Poruchy spánku",
                                                              "suffer_eating" = "Poruchy príjmu potravy"))

# U chlapcov
rq5_impacts_n_males <- do.call(rbind.data.frame, data[data$harrassed == 1 & data$Rod == 0,] %>% select(contains("suffer_") & !contains(c("_other", "_none", "dontKnow"))) %>%
                           map(~cbind("freq" = round(wtd.table(., weights = data[data$harrassed == 1 & data$Rod == 0,]$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data[data$harrassed == 1 & data$Rod == 0,]$w, normwt = T), 0))))) %>%
  rownames_to_column("Muži") %>%
  filter(grepl(".1", Muži, fixed = TRUE)) %>%
  arrange(desc(freq)) %>% mutate(Muži = str_remove(Muži, ".1"),
                                 perc = round(perc, 2)*100)

rq5_impacts_n_males <- rq5_impacts_n_males %>% mutate(Muži = recode(Muži, "suffer_vulnerable" = "Pocity zraniteľnosti",
                                                                              "suffer_lostConfidence" = "Strata sebaistoty",
                                                                              "suffer_anxiety" = "Úzkosť",
                                                                              "suffer_concetration" = "Problémy s koncentráciou",
                                                                              "suffer_helplessness" = "Pocity bezmocnosti",
                                                                              "suffer_depression" = "Depresívne stavy",
                                                                              "suffer_study" = "Ťažkosti s učením",
                                                                              "suffer_relationships" = "Vzťahové problémy",
                                                                              "suffer_sleep" = "Poruchy spánku",
                                                                              "suffer_eating" = "Poruchy príjmu potravy"))

sufferOverallTable <- cbind(rq5_impacts_n, rq5_impacts_n_females, rq5_impacts_n_males)
colnames(sufferOverallTable)[c(2,5,8)] <- "N"
colnames(sufferOverallTable)[c(3,6,9)] <- "%"


# Je rozsah uvádzaných dôsledkov podmienený zažitou formou SO?
rq5_harrassedByImpactSeverity_cor <- data %>% filter(anyYes == 1) %>% weighted_correlation(harrassedSeverity, impactSeverity, weights = w)
rq5_harrassedByImpactSeverity_bf <- data %>% filter(anyYes == 1) %$% correlationBF(y = harrassedSeverity, x = impactSeverity, rscale = rScale)

# Je rozsah uvádzaných dôsledkov podmienený tým, kto je páchateľom?
# compute frequencies of abuses per cluster and perpetrator

data$gmhTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(as.numeric(`Rodovo motivované obťažovanie`) > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$gmhStudentFreq <- data %>% transmute(gmhStudent = ifelse(as.numeric(`Rodovo motivované obťažovanie`) > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$gmhEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(as.numeric(`Rodovo motivované obťažovanie`) > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
#
data$usaTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(as.numeric(`Nechcená sexuálna pozornosť`) > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$usaStudentFreq <- data %>% transmute(gmhStudent = ifelse(as.numeric(`Nechcená sexuálna pozornosť`) > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$usaEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(as.numeric(`Nechcená sexuálna pozornosť`) > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
#
data$sabTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(as.numeric(`Sexuálny nátlak`) > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$sabStudentFreq <- data %>% transmute(gmhStudent = ifelse(as.numeric(`Sexuálny nátlak`) > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$sabEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(as.numeric(`Sexuálny nátlak`) > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
#

# determine the most frequent perpetrator per cluster
# GMH
data$mostFreqAbuserGMH <- data %>% select(gmhTeacherFreq, gmhStudentFreq, gmhEmployeeFreq) %$%
  colnames(.)[ifelse(data$`Rodovo motivované obťažovanie` == 0, NA, max.col(., ties.method="random"))]  %>% str_remove("gmh") %>% str_remove("Freq")
# USA
data$mostFreqAbuserUSA <- data %>% select(usaTeacherFreq, usaStudentFreq, usaEmployeeFreq) %$%
  colnames(.)[ifelse(data$`Nechcená sexuálna pozornosť` == 0, NA, max.col(., ties.method="random"))]  %>% str_remove("usa") %>% str_remove("Freq")
# SAB
data$mostFreqAbuserSAB <- data %>% select(sabTeacherFreq, sabStudentFreq, sabEmployeeFreq) %$%
  colnames(.)[ifelse(data$`Sexuálny nátlak` == 0, NA, max.col(., ties.method="random"))]  %>% str_remove("sab") %>% str_remove("Freq")

# identify the most frequent perpetrator committing the most severe type of abuse
data$perpetratorMostSevere <- data %$% ifelse(!is.na(mostFreqAbuserSAB), mostFreqAbuserSAB, ifelse(!is.na(mostFreqAbuserUSA), mostFreqAbuserUSA, ifelse(!is.na(mostFreqAbuserGMH), mostFreqAbuserGMH, NA)))
perpetratorMostFrequent <- table(data$perpetratorMostSevere)

data %>% select(contains("_whoTeacher") & !contains("Other"))

rq5_severPerpMod <- lm(impactSeverity ~ perpetratorMostSevere, weights = data$w, data)
rq5_severPerpBF <- lmBF(impactSeverity ~ perpetratorMostSevere, data[!is.na(data$perpetratorMostSevere),], rscaleEffects = rScale)
rq5_severPerpSummary <- summary(rq5_severPerpMod)
rq5_severPerpAnova <- Anova(rq5_severPerpMod)

# Existuje rozdiel medzi mužmi a ženami z hľadiska množstva / typu uvádzaných následkov?
rq5_severGenderMod <- data %>% filter(harrassed == 1) %$% lm(impactSeverity ~ Rod, weights = w)
rq5_severGenderBF <- data %>% filter(harrassed == 1) %$% lmBF(impactSeverity ~ Rod, .[!is.na(Rod),], rscaleEffects = rScale)
rq5_severGenderSummary <- summary(rq5_severGenderMod)
rq5_severGenderAnova <- Anova(rq5_severGenderMod)

# Je rozsah uvádzaných dôsledkov podmienený tým, či sa obete zdôverili (či hľadali podporu)? # ADRESOVANE V RQ6 V OPACNEJ DIREKTIVITE
rq5_severDisclosureMod <- data %>% filter(harrassed == 1) %$% lm(impactSeverity ~ disclosureBinary, weights = w)
rq5_severDisclosureBF <- data %>% filter(harrassed == 1) %$% lmBF(impactSeverity ~ disclosureBinary, .[!is.na(disclosureBinary),], rscaleEffects = rScale)
rq5_severDisclosureSummary <- summary(rq5_severDisclosureMod)
rq5_severDisclosureAnova <- Anova(rq5_severDisclosureMod)

data <- data %>% mutate(consequencesSeverity = rowSums(data %>% select(contains("conseq_") & !contains(c("_other", "_none", "dontKnow"))), na.rm = T))

# # Je rozsah uvádzaných dôsledkov podmienený mierou spokojnosti obetí s tým, ako reagovali osoby, u ktorých hľadali pomoc/radu?
# rq5_severSatisfactionMod <- data %>% filter(harrassed == 1 & disclosureBinary == 1) %$% lm(impactSeverity ~ satisfaction, weights = w)
# rq5_severSatisfactionBF <- data %>% filter(harrassed == 1 & disclosureBinary == 1) %$% lmBF(impactSeverity ~ satisfaction, .[!is.na(satisfaction),], rscaleEffects = rScale)
# rq5_severSatisfactionSummary <- summary(rq5_severSatisfactionMod)
# rq5_severSatisfactionAnova <- Anova(rq5_severSatisfactionMod)

# Je rozsah uvádzaných dôsledkov podmienený príslušnosťou k niektorej menšine?
rq5_severMinorityMod <- data %>% filter(harrassed == 1) %$% lm(impactSeverity ~ Minorita, weights = w)
rq5_severMinorityBF <- data %>% filter(harrassed == 1) %$% lmBF(impactSeverity ~ Minorita, .[!is.na(Minorita),], rscaleEffects = rScale)
rq5_severMinoritySummary <- summary(rq5_severMinorityMod)
rq5_severMinorityAnova <- Anova(rq5_severMinorityMod)

# Je rozsah uvádzaných dôsledkov podmienený vierovyznaním obetí?
rq5_severBelieverMod <- data %>% filter(harrassed == 1) %$% lm(impactSeverity ~ Veriaci, weights = w)
rq5_severBelieverBF <- data %>% filter(harrassed == 1) %$% lmBF(impactSeverity ~ Veriaci, .[!is.na(Veriaci),], rscaleEffects = rScale)
rq5_severBelieverSummary <- summary(rq5_severBelieverMod)
rq5_severBelieverAnova <- Anova(rq5_severBelieverMod)

# RQ6 Hladanie pomoci -----------------------------------------------------

# Koľko percent obetí vôbec hľadalo pomoc/radu?
rq6_disclosureOverall <- data %>%
  filter(harrassed == 1) %>%
  group_by(disclosureBinary) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# Koľko percent obetí vôbec hľadalo pomoc/radu?
# By gender
rq6_disclosureN <- data %>%
  filter(harrassed == 1) %>%
  group_by(disclosureBinary, Rod) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# U koho hľadali obete najčastejšie pomoc?
rq6_disclosureWhoFreq <- do.call(rbind.data.frame, data %>% select(contains("disclosure_") & !contains("whyNo")) %>% map(~cbind("freq" = round(wtd.table(as.logical(.), weights = data$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(as.logical(.), weights = data$w, normwt = T), 0))*100))) %>% #Turning disclosure_ vars to logical; T = disclosed, F = not disclosed
  rownames_to_column("disclosureWho") %>% filter(grepl(".TRUE", disclosureWho, fixed = TRUE)) %>% arrange(desc(freq))
rq6_disclosureWhoFreq[,"disclosureWho"] <- rq6_disclosureWhoFreq[,"disclosureWho"] %>% str_remove("disclosure_") %>% str_remove(".TRUE")

# Existuje súvis medzi zažitou formou SO a tendenciou obetí hľadať pomoc? & Existuje súvis medzi častosťou (frekvenciou) SO a tendenciou obetí hľadať pomoc?
# Severity as predictor

rq6_severDisclosureMod0 <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ 1, family=binomial(), weights = w)
rq6_severDisclosureMod <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ impactSeverity, family=binomial(), weights = w)
rq6_severDisclosureSummary <- summary(rq6_severDisclosureMod)
rq6_severDisclosureAnova <- Anova(rq6_severDisclosureMod)

rq6_severDisclosureBF10 <- 1/exp((BIC(rq6_severDisclosureMod) - BIC(rq6_severDisclosureMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
rq6_severDisclosurePosterior <- rq6_severDisclosureBF10/(1+rq6_severDisclosureBF10)
rq6_severDisclosureBayesOut <- data.frame(BF10 = round(rq6_severDisclosureBF10, 3), Posterior = round(rq6_severDisclosurePosterior, 3))

# Existuje rozdiel medzi pohlaviami v tendencii hľadať pomoc?
rq6_genderDisclosureOR <- data %$% riskratio.boot(round(wtd.table(Rod, disclosureBinary, weights = data$w, normwt = T), 0), replicates = 1e5)
rq6_genderDisclosureBF <- data %$% contingencyTableBF(round(wtd.table(Rod, disclosureBinary, weights = data$w, normwt = T), 0), sampleType = "poisson", priorConcentration = 1)

# Súvisí hľadanie pomoci s vierovyznaním obete?
rq6_believerDisclosureOR <- data %$% riskratio.boot(round(wtd.table(Veriaci, disclosureBinary, weights = data$w, normwt = T), 0), replicates = 1e5)
rq6_believerDisclosureBF <- data %$% contingencyTableBF(round(wtd.table(Veriaci, disclosureBinary, weights = data$w, normwt = T), 0), sampleType = "poisson", priorConcentration = 1)

# Súvisí hľadanie pomoci s odborom štúdia?
rq6_fieldDisclosureMod0 <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ 1, family=binomial(), weights = w)
rq6_fieldDisclosureMod <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ `Odbor štúdia`, family=binomial(), weights = w)
rq6_fieldDisclosureSummary <- summary(rq6_fieldDisclosureMod)
rq6_fieldDisclosureAnova <- Anova(rq6_fieldDisclosureMod)

rq6_fieldDisclosureBF10 <- 1/exp((BIC(rq6_fieldDisclosureMod) - BIC(rq6_fieldDisclosureMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
rq6_fieldDisclosurePosterior <- rq6_fieldDisclosureBF10/(1+rq6_fieldDisclosureBF10)
rq6_fieldDisclosureBayesOut <- data.frame(BF10 = round(rq6_fieldDisclosureBF10, 3), Posterior = round(rq6_fieldDisclosurePosterior, 3))

# Súvisí hľadanie pomoci s regiónom, z ktorého obete pochádzajú?
rq6_regionDisclosureMod0 <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ 1, family=binomial(), weights = w)
rq6_regionDisclosureMod <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ `Región fakulty`, family=binomial(), weights = w)
rq6_regionDisclosureSummary <- summary(rq6_regionDisclosureMod)
rq6_regionDisclosureAnova <- Anova(rq6_regionDisclosureMod)

rq6_regionDisclosureBF10 <- 1/exp((BIC(rq6_regionDisclosureMod) - BIC(rq6_regionDisclosureMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
rq6_regionDisclosurePosterior <- rq6_regionDisclosureBF10/(1+rq6_regionDisclosureBF10)
rq6_regionDisclosureBayesOut <- data.frame(BF10 = round(rq6_regionDisclosureBF10, 3), Posterior = round(rq6_regionDisclosurePosterior, 3))

# Súvisí hľadanie pomoci s povedomím o tom čo je SO?
# Compute sensitivity to harrassment by summing the att items
data <- data %>% mutate(sensitivityToHarrasment = rowSums(data %>% select(contains("att") & !contains(c("unwanted"))), na.rm = T))

rq6_sensitivityDisclosureMod0 <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ 1, family=binomial(), weights = w)
rq6_sensitivityDisclosureMod <- data %>% filter(harrassed == 1) %$% glm(disclosureBinary ~ sensitivityToHarrasment, family=binomial(), weights = w)
rq6_sensitivityDisclosureSummary <- summary(rq6_sensitivityDisclosureMod)
rq6_sensitivityDisclosureAnova <- Anova(rq6_sensitivityDisclosureMod)

rq6_sensitivityDisclosureBF10 <- 1/exp((BIC(rq6_sensitivityDisclosureMod) - BIC(rq6_sensitivityDisclosureMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
rq6_sensitivityDisclosurePosterior <- rq6_sensitivityDisclosureBF10/(1+rq6_sensitivityDisclosureBF10)
rq6_sensitivityDisclosureBayesOut <- data.frame(BF10 = round(rq6_sensitivityDisclosureBF10, 3), Posterior = round(rq6_sensitivityDisclosurePosterior, 3))

# V akej miere boli obete spokojné s reakciou osôb, u ktorých hľadali pomoc?
rq6_satisfaction_n <- data %>% filter(satisfaction != 0) %>% summarise(meanSatisfaction = mean(satisfaction, na.rm = T),
                   sdSatisfaction = sd(satisfaction, na.rm = T))
rq6_satisfaction_table <- data %>% filter(satisfaction != 0) %$% table(satisfaction)

rq6_disclosureSatisfaction <- do.call(rbind.data.frame, data %>% select(contains("disclosure_") & !contains("whyNo")) %>% map(~cbind("freq" = round(wtd.table(., weights = data$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(., weights = data$w, normwt = T), 0))))) %>%
  rownames_to_column("disclosureWho") %>% filter(!grepl(".0", disclosureWho, fixed = TRUE)) %>% arrange(desc(freq))

# Existuje rozdiel medzi pohlavím obete a mierou spokojnosti s reakciou ľudí, u ktorých obete hľadali pomoc?
rq6_satisfactionGender <- svyttest(satisfaction ~ Rod, design)

# V koľkých percentách prípadov bolo spustené oficiálne konanie voči osobe, ktorá sa dopustila SO?
rq6_investigationN <- data %>%
  filter(harrassed == 1) %>%
  group_by(investigation) %>%
  summarise(n = n(),
            percent = round(100*n()/nrow(.), 2))

# Existuje súvis medzi zažitou formou SO a spustením oficiálneho konania? & Existuje súvis medzi častosťou (frekvenciou) SO a spustením oficiálneho konania?
rq6_investigationMod0 <- data %>% filter(harrassed == 1) %$% glm(investigation ~ 1, family=binomial(), weights = w)
rq6_investigationMod <- data %>% filter(harrassed == 1) %$% glm(investigation ~ harrassedSeverity, family=binomial(), weights = w)
investigationSummary <- summary(rq6_investigationMod)
rq6_investigationAnova <- Anova(rq6_investigationMod)

rq6_investigationBF10 <- 1/exp((BIC(rq6_investigationMod) - BIC(rq6_investigationMod0))/2) # Bayes Factor based on BIC approximation assuming unit information prior
rq6_investigationPosterior <- rq6_investigationBF10/(1+rq6_investigationBF10)
rq6_investigationBayesOut <- data.frame(BF10 = round(rq6_investigationBF10, 3), Posterior = round(rq6_investigationPosterior, 3))

rq6_investigationHarrassed_cor <- data %>% weighted_correlation(investigation, harrassedSeverity, weights = w)
rq6_formsReported <- data %>% filter(investigation == 1) %>% summarize(n = harrassedSeverity)

# Aké sú dôvody, pre ktoré obete nehľadali pomoc

rq6_disclosureWhyNoFreq <- do.call(rbind.data.frame, data %>% select(contains("whyNo") & !contains("_other")) %>% map(~cbind("freq" = round(wtd.table(as.logical(.), weights = data$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(as.logical(.), weights = data$w, normwt = T), 0))*100))) %>% #Turning disclosure_ vars to logical; T = disclosed, F = not disclosed
  rownames_to_column("disclosureWhyNo") %>% filter(grepl(".TRUE", disclosureWhyNo, fixed = TRUE)) %>% arrange(desc(freq))

rq6_disclosureWhyNoFreq[,"disclosureWhyNo"] <- rq6_disclosureWhyNoFreq[,"disclosureWhyNo"] %>% str_remove("whyNoDisclosure_") %>% str_remove(".TRUE")

# Je rozsah uvádzaných dôsledkov podmienený tým, kto je páchateľom?

# compute frequencies of abuses per cluster and perpetrator
data$gmhTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(as.numeric(`Rodovo motivované obťažovanie`) > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$gmhStudentFreq <- data %>% transmute(gmhStudent = ifelse(as.numeric(`Rodovo motivované obťažovanie`) > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$gmhEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(as.numeric(`Rodovo motivované obťažovanie`) > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
#
data$usaTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(as.numeric(`Nechcená sexuálna pozornosť`) > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$usaStudentFreq <- data %>% transmute(gmhStudent = ifelse(as.numeric(`Nechcená sexuálna pozornosť`) > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$usaEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(as.numeric(`Nechcená sexuálna pozornosť`) > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
#
data$sabTeacherFreq <- data %>% transmute(gmhTeacher = ifelse(as.numeric(`Sexuálny nátlak`) > 0, data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$sabStudentFreq <- data %>% transmute(gmhStudent = ifelse(as.numeric(`Sexuálny nátlak`) > 0, data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
data$sabEmployeeFreq <- data %>% transmute(gmhEmployee = ifelse(as.numeric(`Sexuálny nátlak`) > 0, data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T), NA))
#

# determine the most frequent perpetrator per cluster
# GMH
data$mostFreqAbuserGMH <- data %>% select(gmhTeacherFreq, gmhStudentFreq, gmhEmployeeFreq) %$%
  colnames(.)[ifelse(data$`Rodovo motivované obťažovanie` == 0, NA, max.col(., ties.method="random"))]  %>% str_remove("gmh") %>% str_remove("Freq")
# USA
data$mostFreqAbuserUSA <- data %>% select(usaTeacherFreq, usaStudentFreq, usaEmployeeFreq) %$%
  colnames(.)[ifelse(data$`Nechcená sexuálna pozornosť` == 0, NA, max.col(., ties.method="random"))]  %>% str_remove("usa") %>% str_remove("Freq")
# SAB
data$mostFreqAbuserSAB <- data %>% select(sabTeacherFreq, sabStudentFreq, sabEmployeeFreq) %$%
  colnames(.)[ifelse(data$`Sexuálny nátlak` == 0, NA, max.col(., ties.method="random"))]  %>% str_remove("sab") %>% str_remove("Freq")

# identify the most frequent perpetrator committing the most severe type of abuse
data$perpetratorMostSevere <- data %$% ifelse(!is.na(mostFreqAbuserSAB), mostFreqAbuserSAB, ifelse(!is.na(mostFreqAbuserUSA), mostFreqAbuserUSA, ifelse(!is.na(mostFreqAbuserGMH), mostFreqAbuserGMH, NA)))

rq6_severPerpMod <- lm(impactSeverity ~ perpetratorMostSevere, weights = data$w, data)
rq6_severPerpBF <- lmBF(impactSeverity ~ perpetratorMostSevere, data[!is.na(data$perpetratorMostSevere),], rscaleEffects = rScale)
rq6_severPerpSummary <- summary(rq6_severPerpMod)
rq6_severPerpAnova <- Anova(rq6_severPerpMod)

# RQ7 Dôsledky spojené so zverejnením sťažnosti---------------------------------------------------------------------
# Aké dôsledky spojené so zverejnením sťažnosti na pôde VŠ sú uvádzané najčastejšie?
rq7_investigation_n <- do.call(rbind.data.frame, data %>% select(contains("investigation_") & !contains("_other")) %>% map(~cbind("freq" = round(wtd.table(as.logical(.), weights = data$w, normwt = T), 0), "perc" = prop.table(round(wtd.table(as.logical(.), weights = data$w, normwt = T), 0))))) %>% #Turning disclosure_ vars to logical; T = disclosed, F = not disclosed
  rownames_to_column("investigationConsq") %>% filter(grepl(".TRUE", investigationConsq, fixed = TRUE)) %>% arrange(desc(freq))


# RQ8 Vnímanie sexuálneho obťažovania ---------------------------------------------------------------------
# Vnímanie sexuálneho obťažovania (operacionalizovane ako senzitivita)
# Ktoré prejavy správania považujú respondenti/tky za SO?
rq8_attitudes_overall_n <- data %>% select(starts_with("att") & !contains(c("unwanted"))) %>%
  map(~weighted.mean(., data$w, na.rm = T)) %>% as.tibble() %>% gather(key = "perception", value = "mean") %>% arrange(desc(mean))

rq8_attitudes_n <- data %>%
  group_by(factor(Rod)) %>%
  summarise_at(vars(starts_with("att") & !contains("unwanted")),
               funs(n = round(weighted.mean(., w, na.rm = T)*sum(w), 0),
                    perc = weighted.mean(., w, na.rm = T)*20)) %>% na.omit()

rq8_attitudes_n_plotData <- as_tibble(cbind(item = names(rq8_attitudes_n), t(rq8_attitudes_n)))[-1,] %>% filter(!grepl("_n", item, fixed = TRUE)) %>%
  mutate(Chlapci = as.numeric(V2), Dievčatá = as.numeric(V3), item = as.numeric(1:19), V2 = NULL, V3 = NULL) %>% pivot_longer(cols = c(Chlapci, Dievčatá), names_to = "Rod", values_to = "perc")

rq8_attitudes_plot <- ggplotly(rq8_attitudes_n_plotData %>% ggplot(aes(x = reorder(item, -item), y = perc, fill = Rod)) + geom_bar(stat = "identity",  alpha = .6, position = "dodge") +
                                     labs(x = "Prejavy obťažujúceho správania", y = "Priemerná miera súhlasu") + scale_fill_manual("Legenda", values = c("Dievčatá" = cbPalette[1], "Chlapci" = cbPalette[2])) +
                                     coord_flip() + scale_x_discrete(labels = rev(noquote(sprintf("Situácia %d", 1:19)))) + scale_y_continuous(limits = c(0, 100)))
rq8_attitudes_plot
attitudesList <- c("Rozprával príbehy alebo vtipy so sexuálnym podtónom (napr. na hodine/počas praxe/súkromne v kabinete).",
                   "Mal nemiestne sexuálne poznámky (napr. na hodine/počas praxe/súkromne v kabinete).",
                   "Mal útočné poznámky (napr. na vyučovacej hodine/počas výkonu praxe/súkromne v kabinete).",
                   "Mal 'sexistické' poznámky znevažujúce mužov a ženy (napr. ženy sú dobré iba do postele, muži myslia iba penisom).",
                   "Znevýhodňoval študentov/študentky na základe pohlavia/rodu (napr. zhoršil hodnotenie).",
                   "Zvýhodňoval študentov/študentky na základe pohlavia/rodu (napr. zlepšil/a hodnotenie).",
                   "Používal (ukazoval) zjavné sexuálne materiály počas výučby (aj keď sa jej to netýka).",
                   "Komentoval vzhľad študentov/študentiek (napr. hodnotil telo, oblečenie).",
                   "Pokúsil sa diskutovať so študentom/študentkou o sexe aj keď sa to netýka výučby (napr. sa ich pýta na ich sexuálny život).",
                   "Prejavoval študentom / študentkám neželanú sexuálnu pozornosť (napr. snaha o zblíženie).",
                   "Posielal študentom/študentkám nevyžiadané obrázky/fotky so sexuálnym podtónom.",
                   "Zízal na študentov/študentky sexuálne žiadostivo (napr. žmurkal, čumel).",
                   "Pokúšal sa nadviazať so študentmi/študentkami sexuálny vzťah napriek predchádzajúcemu  odmietnutiu.",
                   "Opakoval žiadosti o stretnutie napriek predchádzajúcemu odmietnutiu (napr. žiadosti o drink/večeru).",
                   "Dotýkal sa študentov/študentiek spôsobom, ktorý v nich vyvolával nepríjemné pocity (napr. ruka cez plecia, okolo pása).",
                   "Pokúsil sa študentov/študentiek sexuálne dotýkať/hladiť (napr. potlapkávanie po zadku).",
                   "Naznačoval študentom/študentkám výhody za sexuálne zblíženie.",
                   "Naznačoval študentom/študentkám ohrozenie ak sa s ním/ňou sexuálne nezblížia (napr. spomenul skúškové obdobie).",
                   "Vyvolával dojem, že sa mu/jej študenti/študentky musia podriadiť ak chcú, aby s nimi bolo zaobcházané dobre (napr. vyžadoval sexuále zblíženie).")

attitudesList <- cbind(noquote(sprintf("Situácia %d", 1:19)), "Formulácia položky" = attitudesList)

rq8_attitudes_item_freqs <- data %>% select(starts_with("att") & !contains(c("unwanted"))) %>%
map(~round(prop.table(wtd.table(., weights = data$w, normwt = T, na.rm = T))*100, 2))
rq8_attitudes_item_freqs <- data.frame(t(sapply(rq8_attitudes_item_freqs,c)))
names(rq8_attitudes_item_freqs) <- c("Úplne nesúhlasím",	"Skôr nesúhlasím",	"Neviem",	"Skôr súhlasím",	"Úplne súhlasím")

# Pre dievčatá
rq8_attitudes_item_freqs_females <- data[data$Rod == 1,] %>% select(starts_with("att") & !contains(c("unwanted"))) %>%
  map(~round(prop.table(wtd.table(., weights = data[data$Rod == 1,]$w, normwt = T, na.rm = T))*100, 2))

rq8_attitudes_item_freqs_females <- data.frame(t(sapply(rq8_attitudes_item_freqs_females,c)))
names(rq8_attitudes_item_freqs_females) <- c("Úplne nesúhlasím",	"Skôr nesúhlasím",	"Neviem",	"Skôr súhlasím",	"Úplne súhlasím")

# Pre chlapcov
rq8_attitudes_item_freqs_males <- data[data$Rod == 0,] %>% select(starts_with("att") & !contains(c("unwanted"))) %>%
  map(~round(prop.table(wtd.table(., weights = data[data$Rod == 0,]$w, normwt = T, na.rm = T))*100, 2))
rq8_attitudes_item_freqs_males <- data.frame(t(sapply(rq8_attitudes_item_freqs_males,c)))
names(rq8_attitudes_item_freqs_males) <- c("Úplne nesúhlasím",	"Skôr nesúhlasím",	"Neviem",	"Skôr súhlasím",	"Úplne súhlasím")

rq8_attitudes_item_freqs_wNA <- data %>% select(starts_with("att") & !contains(c("unwanted"))) %>%
  map(~round(prop.table(wtd.table(., weights = data$w, normwt = T, na.rm = F, na.show = T))*100, 2))
rq8_attitudes_item_freqs_wNA

# Existujú rozdiely s ohľadom na klastre foriem SO (1. rodové obťažovanie, 2. nechcená sexuálna pozornosť, 3. sexuálne donútenie/násilie) ?
# Tazko zodpovedatelne. Alternativa: vztah zavaznosti SO a senzitivity na obtazovanie

rq8_harrassedSensitivity_cor <-  data %>% weighted_correlation(harrassedSeverity, sensitivityToHarrasment, weights = w)
rq8_harrassedSensitivity_bf <- data %$% correlationBF(harrassedSeverity, sensitivityToHarrasment, rscale = rScale)

# Existujú rozdiely medzi pohlaviami?
rq8_genderSensitivityMod <- lm(sensitivityToHarrasment ~ Rod, weights = w, data)
rq8_genderSensitivityBF <- lmBF(sensitivityToHarrasment ~ Rod, data[!is.na(data$Rod),], rscaleEffects = rScale)
rq8_genderSensitivitySummary <- summary(rq8_genderSensitivityMod)
rq8_genderSensitivityAnova <- Anova(rq8_genderSensitivityMod)

# Existujú rozdiely v závislosti od veku respondentov/tiek?
rq8_ageSensitivityMod <- lm(sensitivityToHarrasment ~ age, weights = w, data)
rq8_ageSensitivityBF <- lmBF(sensitivityToHarrasment ~ age, data[!is.na(data$age),], rscaleEffects = rScale)
rq8_ageSensitivitySummary <- summary(rq8_ageSensitivityMod)
rq8_ageSensitivityAnova <- Anova(rq8_ageSensitivityMod)

# Existujú rozdiely v závislosti od odboru štúdia respondentov/tiek?
rq8_fieldSensitivityMod <- lm(sensitivityToHarrasment ~ `Odbor štúdia`, weights = w, data)
rq8_fieldSensitivityBF <- lmBF(sensitivityToHarrasment ~ `Odbor štúdia`, data[!is.na(data$`Odbor štúdia`),], rscaleEffects = rScale)
rq8_fieldSensitivitySummary <- summary(rq8_fieldSensitivityMod)
rq8_fieldSensitivityAnova <- Anova(rq8_fieldSensitivityMod)

rq8_fieldSensitivity_means <- data %>% group_by(`Odbor štúdia`) %>% summarise(meanSensitivity = weighted_mean(sensitivityToHarrasment, w),
                                                                          sdSensitivity = weighted_sd(sensitivityToHarrasment, w))
colnames(rq8_fieldSensitivity_means) <- c("Odbor štúdia", "<I>M</I>", "<I>SD</I>")
sensRegTable <- round(rq8_fieldSensitivitySummary$coefficients, 2)
rownames(sensRegTable) <- noquote(rq8_fieldSensitivity_means$`Odbor štúdia`)
rownames(sensRegTable)[1] <- noquote("Intercept (filozofický, humanitný, pedagogický, teologický)")
colnames(sensRegTable) <- c("Regresný odhad", "<I>SE</I>", "<I>t</I> štatistika", "<I>p</I>-hodnota")


# Existujú rozdiely v závislosti od regiónov, z ktorých respondenti/tky pochádzajú?
data$`Región fakulty` <- factor(data$`Región fakulty`)
rq8_regionSensitivityMod <- lm(sensitivityToHarrasment ~ `Región fakulty`, weights = w, data)
rq8_regionSensitivityBF <- anovaBF(sensitivityToHarrasment ~ `Región fakulty`, data[!is.na(data$`Región fakulty`),], rscaleEffects = rScale)
rq8_regionSensitivitySummary <- summary(rq8_regionSensitivityMod)
rq8_regionSensitivityAnova <- Anova(rq8_regionSensitivityMod)

rq8_regionSensitivity_means <- data %>% group_by(`Región fakulty`) %>% summarise(meanSensitivity = weighted_mean(sensitivityToHarrasment, w),
                                                                              sdSensitivity = weighted_sd(sensitivityToHarrasment, w))

# Existujú rozdiely v závislosti od vierovyznania respondentov/tiek?
rq8_believerSensitivityMod <- lm(sensitivityToHarrasment ~ Veriaci, weights = w, data)
rq8_believerSensitivityBF <- anovaBF(sensitivityToHarrasment ~ Veriaci, data[!is.na(data$Veriaci),], rscaleEffects = rScale)
rq8_believerSensitivitySummary <- summary(rq8_believerSensitivityMod)
rq8_believerSensitivityAnova <- Anova(rq8_believerSensitivityMod)

rq8_believerSensitivity_means <- data %>% group_by(Veriaci) %>% summarise(meanSensitivity = weighted_mean(sensitivityToHarrasment, w),
                                                                           sdSensitivity = weighted_sd(sensitivityToHarrasment, w))

# Existujú rozdiely medzi obeťami SO a osobami, ktoré nemajú osobnú skúsenosť s SO? (častý poznatok - osoby, ktoré majú viac skúseností so SO zvyknú aj viac situácií vyhodnocovať ako SO).
rq8_harrassedSensitivityMod <- lm(sensitivityToHarrasment ~ harrassed, weights = w, data)
rq8_harrassedSensitivityBF <- anovaBF(sensitivityToHarrasment ~ harrassed, data[!is.na(data$harrassed),], rscaleEffects = rScale)
rq8_harrassedSensitivitySummary <- summary(rq8_harrassedSensitivityMod)
rq8_harrassedSensitivityAnova <- Anova(rq8_harrassedSensitivityMod)

rq8_harrassedSensitivity_means <- data %>% group_by(harrassed) %>% summarise(meanSensitivity = weighted_mean(sensitivityToHarrasment, w),
                                                                             sdSensitivity = weighted_sd(sensitivityToHarrasment, w))

# Existuje súvis medzi tým, čo respondenti/tky považujú za SO a tým, či majú tendenciou hľadať pomoc, ak sa s niektorou z foriem SO osobne stretli?
# controlling for harrassedSeverity
rq8_disclosureSensitivityMod <- lm(sensitivityToHarrasment ~ disclosureBinary + harrassedSeverity, weights = w, data)
rq8_disclosureSensitivityBF <- lmBF(sensitivityToHarrasment ~ disclosureBinary + harrassedSeverity, data[!is.na(data$disclosureBinary),], rscaleEffects = rScale)
rq8_disclosureSensitivitySummary <- summary(rq8_disclosureSensitivityMod)
rq8_disclosureSensitivityAnova <- Anova(rq8_disclosureSensitivityMod, type = "III")
rq8_disclosureSensitivity_means <- data %>% group_by(disclosureBinary) %>% summarise(meanSensitivity = round(mean(sensitivityToHarrasment, na.rm = T), 1),
                                           sdSensitivity = round(sd(sensitivityToHarrasment, na.rm = T), 1)) %>% .[-1]
colnames(rq8_disclosureSensitivity_means) <- c("M", "SD")
rownames(rq8_disclosureSensitivity_means) <- c("Participant sa nezdôveril", "Participant sa zdôveril", "Participant nebol obeťou obťažovania")

# Aké je právne povedomie o SO?
data <- data %>% mutate(legalAwareness = rowSums(cbind(know1, know2), na.rm = T))

rq8_legalAwareness_mean <- data %>%
  summarise(meanLegalAwareness = weighted_mean(legalAwareness, w),
            sdLegalAwareness = weighted_sd(legalAwareness, w))

# Koľko percent participantov vie o tom, že SO je súčasťou zákona
rq8_legalAwareness_q70_table <- round(prop.table(wtd.table(data$know1, weights = data$w))*100, 2)
rq8_legalAwareness_q70_table_females <- round(prop.table(wtd.table(data[data$Rod == 1,]$know1, weights = data[data$Rod == 1,]$w))*100, 2)
rq8_legalAwareness_q70_table_males <- round(prop.table(wtd.table(data[data$Rod == 0,]$know1, weights = data[data$Rod == 0,]$w))*100, 2)
# Koľko percent participantov vie o tom, že sa môžu obrátiť na súd
rq8_legalAwareness_q71_table <- round(prop.table(wtd.table(data$know2, weights = data$w))*100, 2)
rq8_legalAwareness_q71_table_females <- round(prop.table(wtd.table(data[data$Rod == 1,]$know2, weights = data[data$Rod == 1,]$w))*100, 2)
rq8_legalAwareness_q71_table_males <- round(prop.table(wtd.table(data[data$Rod == 0,]$know2, weights = data[data$Rod == 0,]$w))*100, 2)
# Koľko percent participantov cíti, že či ich škola dostatočne v téme SO vyvzdelala/informovala
rq8_legalAwareness_q72_table <- round(prop.table(wtd.table(data$know3, weights = data$w))*100, 2)
rq8_legalAwareness_q72_table_females <- round(prop.table(wtd.table(data[data$Rod == 1,]$know3, weights = data[data$Rod == 1,]$w))*100, 2)
rq8_legalAwareness_q72_table_males <- round(prop.table(wtd.table(data[data$Rod == 0,]$know3, weights = data[data$Rod == 0,]$w))*100, 2)

# Existujú rozdiely medzi pohlaviami?
rq8_genderLegalAwarenessMod <- lm(legalAwareness ~ Rod, weights = w, data)
rq8_genderLegalAwarenessBF <- lmBF(legalAwareness ~ Rod, data[!is.na(data$Rod) & !is.na(data$legalAwareness),], rscaleEffects = rScale)
rq8_genderLegalAwarenessSummary <- summary(rq8_genderLegalAwarenessMod)
rq8_genderLegalAwarenessAnova <- Anova(rq8_genderLegalAwarenessMod)

rq8_genderLegalAwareness_means <- data %>% group_by(Rod) %>% summarise(meanLegalAwareness = weighted_mean(legalAwareness, w),
                                                                                sdLegalAwareness = weighted_sd(legalAwareness, w))

# Existujú rozdiely v závislosti od regiónov, z ktorých respondenti/tky pochádzajú?
rq8_regionLegalAwarenessMod <- lm(legalAwareness ~ `Región fakulty`, weights = w, data)
rq8_regionLegalAwarenessBF <- anovaBF(legalAwareness ~ `Región fakulty`, data[!is.na(data$`Región fakulty`) & !is.na(data$legalAwareness),], rscaleEffects = rScale)
rq8_regionLegalAwarenessSummary <- summary(rq8_regionLegalAwarenessMod)
rq8_regionLegalAwarenessAnova <- Anova(rq8_regionLegalAwarenessMod)

# Existuje súvis medzi tým, čo respondenti/tky považujú za SO a tým, či majú tendenciou hľadať pomoc, ak sa s niektorou z foriem SO osobne stretli?
# controlling for harrassedSeverity
rq8_disclosureLegalAwarenessMod <- lm(legalAwareness ~ disclosureBinary + harrassedSeverity, weights = w, data)
rq8_disclosureLegalAwarenessBF <- lmBF(legalAwareness ~ disclosureBinary + harrassedSeverity, data[!is.na(data$disclosureBinary),], rscaleEffects = rScale)
rq8_disclosureLegalAwarenessSummary <- summary(rq8_disclosureLegalAwarenessMod)
rq8_disclosureLegalAwarenessAnova <- Anova(rq8_disclosureLegalAwarenessMod, type = "III")

rq8_disclosureLegalAwareness_means <- data %>% group_by(disclosureBinary) %>% summarise(meanLegalAwareness = weighted_mean(legalAwareness, w),
                                                                                        sdLegalAwareness = weighted_sd(legalAwareness, w))


# RQ9 Poskytnutie info školou --------------------------------------------------------------------
# Poskytla im ich vysoká škola dostatok informácií o SO?
# dostatok informacii = odpoved skor ano alebo urcite ano
rq9_dostatokInformacii_n <- data %>%
  summarise(dostatokInformaciiN = svytotal(know3 > 2, design, na.rm = TRUE),
            dostatokInformaciiPerc = svytotal(know3 > 2, design, na.rm = TRUE)*100/sum(w))

# Existujú rozdiely medzi pohlaviami?
rq9_genderInformationMod <- lm(know3 ~ Rod, weights = w, data)
rq9_genderInformationBF <- lmBF(know3 ~ Rod, data[!is.na(data$Rod) & !is.na(data$know3),], rscaleEffects = rScale)
rq9_genderInformationSummary <- summary(rq9_genderInformationMod)
rq9_genderInformationAnova <- Anova(rq9_genderInformationMod)

# Existujú rozdiely v závislosti od regiónov, z ktorých respondenti/tky pochádzajú?
rq9_regionInformationMod <- lm(know3 ~ `Región fakulty`, weights = w, data)
rq9_regionInformationBF <- anovaBF(know3 ~ `Región fakulty`, data[!is.na(data$`Región fakulty`) & !is.na(data$know3),], rscaleEffects = rScale)
rq9_regionInformationSummary <- summary(rq9_regionInformationMod)
rq9_regionInformationAnova <- Anova(rq9_regionInformationMod)

# Existujú rozdiely v závislosti od odboru štúdia respondentov/tiek?
rq9_fieldInformationMod <- lm(know3 ~ `Odbor štúdia`, weights = w, data)
rq9_fieldInformationBF <- lmBF(know3 ~ `Odbor štúdia`, data[!is.na(data$`Odbor štúdia`) & !is.na(data$know3),], rscaleEffects = rScale)
rq9_fieldInformationSummary <- summary(rq9_fieldInformationMod)
rq9_fieldInformationAnova <- Anova(rq9_fieldInformationMod)

# RQ10 Stereotypy --------------------------------------------------------------------

# Tvrdenia / stereotypy /  predsudky o sexuálnom obťažovaní
rq10_misconcept_n <- data %>%
  group_by(factor(Rod)) %>%
  summarise_at(vars(starts_with("misconcept") & !contains("Score")),
               funs(n = round(weighted.mean(., w, na.rm = T)*sum(w), 0),
                    perc = weighted.mean(., w, na.rm = T)*20)) %>% na.omit()

rq10_misconcept_n_plotData <- as_tibble(cbind(item = names(rq10_misconcept_n), t(rq10_misconcept_n)))[-1,] %>% filter(!grepl("_n", item, fixed = TRUE)) %>%
  mutate(Chlapci = as.numeric(V2), Dievčatá = as.numeric(V3), item = as.numeric(1:11), V2 = NULL, V3 = NULL) %>% pivot_longer(cols = c(Chlapci, Dievčatá), names_to = "Rod", values_to = "perc")

rq10_misconcept_plot <- ggplotly(rq10_misconcept_n_plotData %>% ggplot(aes(x = reorder(item, -item), y = perc, fill = Rod)) + geom_bar(stat = "identity",  alpha = .6, position = "dodge") +
                                     labs(x = "Miskoncepty o sexuálnom obťažovaní", y = "Priemerná miera súhlasu") + scale_fill_manual("Legenda", values = c("Dievčatá" = cbPalette[1], "Chlapci" = cbPalette[2])) +
                                     coord_flip() + scale_x_discrete(labels = rev(noquote(sprintf("Miskoncept %d", 1:11)))) + scale_y_continuous(limits = c(0, 100)))

misconceptsList <- c("Ženy si často vymýšľajú obvinenia zo sexuálneho obťažovania.",
                     "Vyučujúci / vyučujúce by nemali mať romantické vzťahy so svojimi študentmi / študentkami.",
                     "Vyučujúci / vyučujúce by nemali mať sexuálne vzťahy so svojimi študentmi / študentkami.",
                     "Sexuálne obťažovanie je zneužitie, ktoré ovplyvňuje kvalitu života osoby, na ktorej je páchané.",
                     "Keď sa bude sexuálne obťažovanie ignorovať, prestane.",
                     "Ľudia, ktorí zažili sexuálne obťažovanie, ale rozhodnú sa ho nahlásiť až po rokoch, nemajú nárok na spravodlivoť (napr. potrestanie vinníka).",
                     "Ženy, ktoré sa obliekajú sexuálne atraktívnym spôsobom si koledujú o sexuálnu pozornosť (napr. komentáre).",
                     "Muži, ktorí sa obliekajú sexuálne atraktívnym spôsobom si koledujú o sexuálnu pozornosť (napr. komentáre).",
                     "Zodpovednosť za sexuálne obťažovanie prináleží v plnej miere osobe, ktorá sa obťažovania dopúšťa, bez ohľadu na vzhľad alebo konanie osoby, ktorá je obťažovaná.",
                     "So sexuálnym obťažovaním sa nedá nič robiť.",
                     "Vedenie školy by malo prijať opatrenia na predchádzanie sexuálneho obťažovania.")

misconceptsList <- cbind(noquote(sprintf("Miskoncept %d", 1:11)), "Formulácia miskonceptu" = misconceptsList)

rq10_misconcept_item_freqs <- data %>% select(starts_with("misconcept")) %>%
  map(~round(prop.table(wtd.table(., weights = data$w, normwt = T, na.rm = T))*100, 2))
rq10_misconcept_item_freqs <- data.frame(t(sapply(rq10_misconcept_item_freqs[-12],c)))
names(rq10_misconcept_item_freqs) <- c("Úplne nesúhlasím",	"Skôr nesúhlasím",	"Neviem",	"Skôr súhlasím",	"Úplne súhlasím")
rownames(rq10_misconcept_item_freqs) <-  noquote(sprintf("Miskoncept %d", 1:11))

# Pre dievčatá
rq10_misconcept_item_freqs_females <- data[data$Rod == 1,] %>% select(starts_with("misconcept")) %>%
  map(~round(prop.table(wtd.table(., weights = data[data$Rod == 1,]$w, normwt = T, na.rm = T))*100, 2))
rq10_misconcept_item_freqs_females <- data.frame(t(sapply(rq10_misconcept_item_freqs_females[-12],c)))
names(rq10_misconcept_item_freqs_females) <- c("Úplne nesúhlasím",	"Skôr nesúhlasím",	"Neviem",	"Skôr súhlasím",	"Úplne súhlasím")
rownames(rq10_misconcept_item_freqs_females) <-  noquote(sprintf("Miskoncept %d", 1:11))

# Pre chlapcov
rq10_misconcept_item_freqs_males  <- data[data$Rod == 0,] %>% select(starts_with("misconcept")) %>%
  map(~round(prop.table(wtd.table(., weights = data[data$Rod == 0,]$w, normwt = T, na.rm = T))*100, 2))
rq10_misconcept_item_freqs_males <- data.frame(t(sapply(rq10_misconcept_item_freqs_males[-12],c)))
names(rq10_misconcept_item_freqs_males) <- c("Úplne nesúhlasím",	"Skôr nesúhlasím",	"Neviem",	"Skôr súhlasím",	"Úplne súhlasím")
rownames(rq10_misconcept_item_freqs_males) <-  noquote(sprintf("Miskoncept %d", 1:11))

rq10_misconcept_item_freqs_wNA <- data %>% select(starts_with("misconcept")) %>%
  map(~round(prop.table(wtd.table(., weights = data$w, normwt = T, na.rm = F, na.show = T))*100, 2))
rq10_misconcept_item_freqs_wNA <- data.frame(t(sapply(rq10_misconcept_item_freqs_wNA[-12],c)))
names(rq10_misconcept_item_freqs_wNA) <- c("Úplne nesúhlasím",	"Skôr nesúhlasím",	"Neviem",	"Skôr súhlasím",	"Úplne súhlasím")

# V akej miere súhlasia respondenti/tky s jednotlivými tvrdeniami?
# min score = 0, max score 55. The higher the score the higher the agreement with misconceptions
data <- data %>% map_at(., .at = vars(c(misconcept2, misconcept3, misconcept4, misconcept9, misconcept11)), .f =~ 4-.) %>% as.tibble() %>% mutate(misconceptScore = rowSums(data %>% select(starts_with("misconcept")), na.rm = T)) # dealing with the inverse scaling of items

rq.10_agreeMisconceptions <- data %>%
  summarise(meanAgreementMisconceptions = mean(misconceptScore, na.rm = TRUE),
            sdAgreementMisconceptions = sd(misconceptScore, na.rm = TRUE))

# Existujú rozdiely medzi pohlaviami?
rq.10_genderMisconceptionsMod <- lm(misconceptScore ~ Rod, weights = w, data)
rq.10_genderMisconceptionsBF <- lmBF(misconceptScore ~ Rod, data[!is.na(data$Rod) & !is.na(data$misconceptScore),], rscaleEffects = rScale)
rq.10_genderMisconceptionsSummary <- summary(rq.10_genderMisconceptionsMod)
rq.10_genderMisconceptionsAnova <- Anova(rq.10_genderMisconceptionsMod)

# Existujú rozdiely v závislosti od veku respondentov/tiek?
rq.10_ageMisconceptionsMod <- lm(misconceptScore ~ age, weights = w, data)
rq.10_ageMisconceptionsBF <- lmBF(misconceptScore ~ age, data[!is.na(data$age),], rscaleEffects = rScale)
rq.10_ageMisconceptionsSummary <- summary(rq.10_ageMisconceptionsMod)
rq.10_ageMisconceptionsAnova <- Anova(rq.10_ageMisconceptionsMod)

# Existujú rozdiely v závislosti od odboru štúdia respondentov/tiek?
rq.10_fieldMisconceptionsMod <- lm(misconceptScore ~ `Odbor štúdia`, weights = w, data)
rq.10_fieldMisconceptionsBF <- lmBF(misconceptScore ~ `Odbor štúdia`, data[!is.na(data$`Odbor štúdia`) & !is.na(data$misconceptScore),], rscaleEffects = rScale)
rq.10_fieldMisconceptionsSummary <- summary(rq.10_fieldMisconceptionsMod)
rq.10_fieldMisconceptionsAnova <- Anova(rq.10_fieldMisconceptionsMod)

# Existujú rozdiely v závislosti od regiónov, z ktorých respondenti/tky pochádzajú?
rq.10_regionMisconceptionsMod <- lm(misconceptScore ~ `Región fakulty`, weights = w, data)
rq.10_regionMisconceptionsBF <- anovaBF(misconceptScore ~ `Región fakulty`, data[!is.na(data$`Región fakulty`) & !is.na(data$misconceptScore),], rscaleEffects = rScale)
rq.10_regionMisconceptionsSummary <- summary(rq.10_regionMisconceptionsMod)
rq.10_regionMisconceptionsAnova <- Anova(rq.10_regionMisconceptionsMod)

# Existujú rozdiely v závislosti od vierovyznania respondentov/tiek?
rq.10_believerMisconceptionsMod <- lm(misconceptScore ~ Veriaci, weights = w, data)
rq.10_believerMisconceptionsBF <- anovaBF(misconceptScore ~ Veriaci, data[!is.na(data$Veriaci),], rscaleEffects = rScale)
rq.10_believerMisconceptionsSummary <- summary(rq.10_believerMisconceptionsMod)
rq.10_believerMisconceptionsAnova <- Anova(rq.10_believerMisconceptionsMod)

rq.10_believerMisconceptions_means <- data %>% group_by(Veriaci) %>% summarise(meanMisconceptions = mean(misconceptScore, na.rm = T),
                                          sdMisconceptions = sd(misconceptScore, na.rm = T))

# Existujú rozdiely medzi obeťami SO a osobami, ktoré nemajú osobnú skúsenosť s SO? (častý poznatok - osoby, ktoré majú viac skúseností so SO zvyknú aj viac situácií vyhodnocovať ako SO).
rq.10_harrassedMisconceptionsMod <- lm(misconceptScore ~ harrassed, weights = w, data)
rq.10_harrassedMisconceptionsBF <- anovaBF(misconceptScore ~ harrassed, data[!is.na(data$harrassed),], rscaleEffects = rScale)
rq.10_harrassedMisconceptionsSummary <- summary(rq.10_harrassedMisconceptionsMod)
rq.10_harrassedMisconceptionsAnova <- Anova(rq.10_harrassedMisconceptionsMod)

rq.10_harrassedMisconceptions_means <- data %>% group_by(harrassed) %>% summarise(meanMisconceptions = mean(misconceptScore, na.rm = T),
                                           sdMisconceptions = sd(misconceptScore, na.rm = T))

# Existuje súvis medzi tým, čo respondenti/tky považujú za SO a tým, či majú tendenciou hľadať pomoc, ak sa s niektorou z foriem SO osobne stretli?
# controlling for harrassedSeverity
rq.10_disclosureMisconceptionsMod <- lm(misconceptScore ~ disclosureBinary + harrassedSeverity, weights = w, data)
rq.10_disclosureMisconceptionsBF <- lmBF(misconceptScore ~ disclosureBinary + harrassedSeverity, data[!is.na(data$disclosureBinary),], rscaleEffects = rScale)
rq.10_disclosureMisconceptionsSummary <- summary(rq.10_disclosureMisconceptionsMod)
rq.10_disclosureMisconceptionsAnova <- Anova(rq.10_disclosureMisconceptionsMod, type = "III")

rq.10_disclosureMisconceptions_means <- data %>% group_by(disclosureBinary) %>% summarise(meanMisconceptions = mean(misconceptScore, na.rm = T),
                                                  sdMisconceptions = sd(misconceptScore, na.rm = T))

sessionInfo()
save.image("workspace.RDS")
