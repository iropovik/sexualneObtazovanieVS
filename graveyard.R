
#########
# Graveyard
#########
#
# # Base rato no of responses per perpetrators
# baseWho <- data %>% select(contains("_who") & !contains("Other")) %>% map(~sum(!is.na(.))) %>% as.numeric(.) %>% .[1:120*6] %>% .[!is.na(.)] #Calculating the base rate no of responses per perpetrators and selecting every 6th element
# freqItems[1:20]

# Weighting
# svyUnweighted <- svydesign(ids=~1, data=data)
#
# facultyRegionDist <- data.frame(facultyRegion = c("Západné Slovensko", "Stredné Slovensko", "Východné Slovensko"),
#                        Freq = nrow(data) * c(0.585736606, 0.192742301, 0.221521093))
#
# fieldStudyDist <- data.frame(fieldStudy = c("filozofický, humanitný, pedagogický, teologický", "mediálny, umelecký", "prírodovedecký", "spoločenský, ekonomický, právny", "technický", "zdravotnícky"),
#                        Freq = nrow(data) * c(0.217237034, 0.042941384, 0.089884582, 0.194224081, 0.316445744, 0.139267174))
#
# svyRake <- rake(design = svyUnweighted,
#                  sample.margins = list(~facultyRegion, ~fieldStudy),
#                  population.margins = list(facultyRegionDist, fieldStudyDist))
#
#
# summary(weights(svyRake))
# svyRakeTrim <- trimWeights(svyRake, lower=0.2, upper=4, strict=TRUE)
#
#
# samplingWeights <- unlist(attributes(svyRakeTrim$postStrata[[1]][[2]]))[(nrow(data) +1):(2*nrow(data))]

# Perpetrators
# whoList <- list()
# for(i in 1:20){
#   whoList[[i]] <- data %>% select(contains("_who") & !contains("Other")) %>% map(~cbind("freq" = table(.), "perc" = prop.table(table(.))))
#   names(whoList[[i]]) == c("teacher_M", "teacher_F", "student_M", "student_F", "employee_M", "employee_F")
# }


# Overall frequencies of perpetrators across all clusters
data$freqAbusesTeacher <- data %>% select(contains("_whoTeacher") & !contains("Other")) %>% rowSums(., na.rm = T)
data$freqAbusesStudent <- data %>% select(contains("_whoStudent") & !contains("Other")) %>% rowSums(., na.rm = T)
data$freqAbusesEmployee <- data %>% select(contains("_whoEmployee") & !contains("Other")) %>% rowSums(., na.rm = T)
