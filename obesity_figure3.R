library(DatabaseConnector)
library(ggplot2)
library(dplyr)
library(interactions)
library(ggpubr)
library(lme4)
library(tibble)
library(sjPlot)
library(nlme)
library(emmeans)

#launchEvidenceExplorer(dataFolder = "C:/Users/hihip/OneDrive - Ajou University/important/study/이카루스obesity/obesity09201/output/shinyData", blind = FALSE, launch.browser = FALSE)

################################
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sql server",
                                                                server = '',
                                                                user = '',
                                                                password = '',
                                                                pathToDriver = 'C:/jdbc_driver')
conn <- DatabaseConnector::connect(connectionDetails)
# #################################
con <- dbConnect(RSQLite::SQLite(), dbname="~/output/cmOutput/CmData_l1_t3316_c3317/file11aa15f63e9c6.sqlite")
data <- dbGetQuery(con, "SELECT * FROM cohorts")
connected_db <- data %>% select(personSeqId, personId)
colnames(connected_db) <- c('personSeqId', 'person_id')

person <- dbGetQuery(conn,"SELECT person_id, birth_datetime, gender_source_value FROM [CDMPv535_ABMI].[dbo].[person]")

StratPop_l1_s1_p1_t3316_c3317_s1_o2580 <- readRDS("~/output/cmOutput/StratPop_l1_s1_p1_t3316_c3317_s1_o2580.rds")
table(StratPop_l1_s1_p1_t3316_c3317_s1_o2580$treatment)
metaDF <- merge(StratPop_l1_s1_p1_t3316_c3317_s1_o2580, connected_db) %>% select(person_id, treatment, cohortStartDate, daysToCohortEnd)
colnames(metaDF) <- c('person_id', 'treatment', 'cohortStartDate', 'cohortEndDate')
metaDF <- metaDF %>% mutate(cohortEndDate = cohortStartDate + cohortEndDate)
target_cohort <- metaDF %>% filter(treatment == 1)
comparator_cohort <- metaDF %>% filter(treatment == 0)
#################################

# IgE(IU/mL) : 42529219 (20, 580) / Triglyceride(mg/dL) : 3022192 (0, 500) / LDL(mg/dL) : 3028437 (0, 250) / HDL(mg/dL) : 3007070 (0, 110) / CRP(mg/L) : 3020460 (-1, 6) / FeNO : 40480257 / HbA1c(%) : 3004410 (5.5, 7) / Blood glucose(mg/dL) : 3040820 / Platelets [#/volume] in Blood : 3007461 (225, 265) / ESR : 3015183 / AST(IU/L) : 3013721 / ALT(IU/L) : 3006923 / Creatinine kinase : 3007220 / Eosinophils/100 leukocytes in sputum : 3021940 (-10, 65) / Neutrophils/100 leukocytes in sputum : 3026514 / Eosinophil/100 leukocytes in blood : 3006504 (2, 4) / Neutrophils/100 leukocytes in blood : 3018010 (52, 65) / FEV1 measured/predicted : 3011708 / FEV1/FVC : 3011505 (82, 88) / BMI : 3038553 (20, 45) / Serum leukocytes (X10³/㎕) : 3010813 / FVC : 3005600

# 모든 측정치 활용 (+ 나이, 성별, 반복측정치 보정)

extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv535_ABMI].[dbo].[measurement] m
                     where m.measurement_concept_id = 3005600 and m.person_id in (", paste(target_cohort$person_id, collapse = ', '), ')')
# extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv535_ABMI].[dbo].[measurement] m
#                      join (select * from [CDMPv535_ABMI].[dbo].[visit_occurrence] where visit_concept_id in (9202)) opv
#                      on m.visit_occurrence_id = opv.visit_occurrence_id
#                      where m.measurement_concept_id = 3010813 and m.person_id in (", paste(target_cohort$person_id, collapse = ', '), ')')

obesity_group <- dbGetQuery(conn, extractcode)
obesity_group <- merge(obesity_group, person, by = 'person_id')
colnames(obesity_group) <- c('person_id', 'measurement_date', 'value_as_number', 'birth', 'gender')
obesity_group <- merge(target_cohort, obesity_group, by = 'person_id')
obesity_group <- obesity_group %>% filter(measurement_date >= cohortStartDate) %>% filter(cohortEndDate >= measurement_date) %>% mutate(followup_years = as.numeric(measurement_date - cohortStartDate)/365) %>% mutate(age = as.numeric(measurement_date - as.Date(substr(as.character(birth),1,10)))/365) %>% mutate(obesity = 'yes')

extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv535_ABMI].[dbo].[measurement] m
                     where m.measurement_concept_id = 3005600 and m.person_id in (", paste(comparator_cohort$person_id, collapse = ', '), ')')
# extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv535_ABMI].[dbo].[measurement] m
#                      join (select * from [CDMPv535_ABMI].[dbo].[visit_occurrence] where visit_concept_id in (9202)) opv
#                      on m.visit_occurrence_id = opv.visit_occurrence_id
#                      where m.measurement_concept_id = 3010813 and m.person_id in (", paste(comparator_cohort$person_id, collapse = ', '), ')')

no_obesity_group <- dbGetQuery(conn, extractcode)
no_obesity_group <- merge(no_obesity_group, person, by = 'person_id')
colnames(no_obesity_group) <- c('person_id', 'measurement_date', 'value_as_number', 'birth', 'gender')
no_obesity_group <- merge(comparator_cohort, no_obesity_group, by = 'person_id')
no_obesity_group <- no_obesity_group %>% filter(measurement_date >= cohortStartDate) %>% filter(cohortEndDate >= measurement_date) %>% mutate(followup_years = as.numeric(measurement_date - cohortStartDate)/365) %>% mutate(age = as.numeric(measurement_date - as.Date(substr(as.character(birth),1,10)))/365) %>% mutate(obesity = 'no')

obesity_group <- obesity_group %>% filter(followup_years<10)
no_obesity_group <- no_obesity_group %>% filter(followup_years<10)
total <- rbind(obesity_group, no_obesity_group)

lmm_model <- lmer(
  value_as_number ~ followup_years*obesity + age + gender
  + (1 | person_id),
  data = total
)
#################################
predicted <- emmeans(lmm_model, specs = ~ obesity, at = list(followup_years = 10), pbkrtest.limit = 7000, lmerTest.limit = 7000)
conf_int <- confint(predicted)

print(conf_int)
print(pairs(predicted))
#################################
sjPlot::plot_model(lmm_model, type = "pred", terms = c("followup_years", 'obesity'), axis.title = c('Years of follow-up', 'FVC'), title = '', alpha = 0) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "grey50")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .7, position = position_dodge(width = 0.2), linetype = 1) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_fill_manual(values=c("white", "black")) +
  scale_color_manual(values=c("black", "black")) +
  scale_y_continuous(limits = c(88, 96)) + 
  aes(linetype=group, color=group) +
  geom_point(size = 4, stroke = .5, shape = 21, position = position_dodge(width = 0.05)) + 
  scale_x_discrete(limits=c(0, 2, 4, 6, 8, 10)) # 0, 1, 2, 3, 4, 5  ## 0, 2, 4, 6, 8, 10
length(unique(obesity_group$person_id))
length(unique(no_obesity_group$person_id))
#sjPlot::tab_model(lmm_model)

# nrow(unique(total %>% filter(treatment == 1) %>% select(person_id)))
# nrow(unique(total %>% filter(treatment == 0) %>% select(person_id)))
