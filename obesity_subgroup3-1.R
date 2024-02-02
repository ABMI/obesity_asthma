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

# connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sql server",
#                                                                 server = '10.5.99.50',
#                                                                 user = 'hihipch',
#                                                                 password = 'dkssudgo@5',
#                                                                 pathToDriver = 'C:/jdbc_driver')
# conn <- DatabaseConnector::connect(connectionDetails)
# #################################
# con <- dbConnect(RSQLite::SQLite(), dbname="C:/Users/hihip/OneDrive - Ajou University/important/study/이카루스obesity/obesity08131/output/cmOutput/CmData_l1_t3316_c3317/file11aa15f63e9c6.sqlite")
# data <- dbGetQuery(con, "SELECT * FROM cohorts")
# connected_db <- data %>% select(personSeqId, personId)
# colnames(connected_db) <- c('personSeqId', 'person_id')
# 
# person <- dbGetQuery(conn,"SELECT person_id, birth_datetime, gender_source_value FROM [CDMPv535_ABMI].[dbo].[person]")
# 
# StratPop_l1_s2_p1_t3316_c3317_s1_o3954 <- readRDS("C:/Users/hihip/OneDrive - Ajou University/important/study/이카루스obesity/obesity09201/output/cmOutput/StratPop_l1_s2_p1_t3316_c3317_s1_o3954.rds")
# Obesity <- StratPop_l1_s2_p1_t3316_c3317_s1_o3954 %>% filter(treatment == 1)
# Nonobesity <- StratPop_l1_s2_p1_t3316_c3317_s1_o3954 %>% filter(treatment == 0)
# 
# Obesity <- merge(Obesity, connected_db) %>% select(person_id, treatment, cohortStartDate, daysToCohortEnd, daysToEvent)
# Nonobesity <- merge(Nonobesity, connected_db) %>% select(person_id, treatment, cohortStartDate, daysToCohortEnd, daysToEvent)
# 
# colnames(Obesity) <- c('person_id', 'treatment', 'cohortStartDate', 'cohortEndDate', 'daysToEvent')
# colnames(Nonobesity) <- c('person_id', 'treatment', 'cohortStartDate', 'cohortEndDate', 'daysToEvent')
# Obesity <- Obesity %>% mutate(cohortEndDate = cohortStartDate + cohortEndDate)
# Nonobesity <- Nonobesity %>% mutate(cohortEndDate = cohortStartDate + cohortEndDate)
# 
# Obesity_WCT <- Obesity %>% filter(is.na(daysToEvent)) %>% select(person_id, cohortStartDate, cohortEndDate)
# Obesity_UCT <- Obesity %>% filter(!is.na(daysToEvent)) %>% select(person_id, cohortStartDate, cohortEndDate)
# Nonobesity_WCT <- Nonobesity %>% filter(is.na(daysToEvent)) %>% select(person_id, cohortStartDate, cohortEndDate)
# Nonobesity_UCT <- Nonobesity %>% filter(!is.na(daysToEvent)) %>% select(person_id, cohortStartDate, cohortEndDate)

### Eosinophil count (cells/mcL) : 3006504 (0, 1000) / Neutrophil count (cells/mcL) : 3018010 (0, 10000)
extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv535_ABMI].[dbo].[measurement] m 
                     join (select * from [CDMPv535_ABMI].[dbo].[visit_occurrence] where visit_concept_id in (9202)) opv
                     on m.visit_occurrence_id = opv.visit_occurrence_id
                     where m.measurement_concept_id = 3010813 and m.person_id in (", paste(Obesity_UCT$person_id, collapse = ', '), ')')
target_leukocyte <- dbGetQuery(conn, extractcode)
target_leukocyte <- merge(target_leukocyte, person, by = 'person_id')
colnames(target_leukocyte) <- c('person_id', 'measurement_date', 'leukocyte', 'birth', 'gender')
target_leukocyte <- merge(Obesity_UCT, target_leukocyte, by = 'person_id')
target_leukocyte <- target_leukocyte %>% filter(measurement_date >= cohortStartDate) %>% filter(cohortEndDate >= measurement_date) %>% mutate(followup_years = as.numeric(measurement_date - cohortStartDate)/365) %>% mutate(age = as.numeric(measurement_date - as.Date(substr(as.character(birth),1,10)))/365) %>% mutate(obesity = 'yes')

extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv535_ABMI].[dbo].[measurement] m 
                     join (select * from [CDMPv535_ABMI].[dbo].[visit_occurrence] where visit_concept_id in (9202)) opv
                     on m.visit_occurrence_id = opv.visit_occurrence_id
                     where m.measurement_concept_id = 3018010 and m.person_id in (", paste(Obesity_UCT$person_id, collapse = ', '), ')')
target_eosinophil_percent <- dbGetQuery(conn, extractcode)
target_eosinophil_percent <- merge(target_eosinophil_percent, person, by = 'person_id')
colnames(target_eosinophil_percent) <- c('person_id', 'measurement_date', 'eosinophil_percent', 'birth', 'gender')
target_eosinophil_percent <- merge(Obesity_UCT, target_eosinophil_percent, by = 'person_id')
target_eosinophil_percent <- target_eosinophil_percent %>% filter(measurement_date >= cohortStartDate) %>% filter(cohortEndDate >= measurement_date) %>% mutate(followup_years = as.numeric(measurement_date - cohortStartDate)/365) %>% mutate(age = as.numeric(measurement_date - as.Date(substr(as.character(birth),1,10)))/365) %>% mutate(obesity = 'yes')

target_eosinophil <- merge(target_leukocyte, target_eosinophil_percent %>% select(person_id, measurement_date, eosinophil_percent), by=c('person_id', 'measurement_date'))
target_eosinophil <- target_eosinophil %>% mutate(total_eosinophil_count = leukocyte * eosinophil_percent * 10)

extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv535_ABMI].[dbo].[measurement] m 
                     join (select * from [CDMPv535_ABMI].[dbo].[visit_occurrence] where visit_concept_id in (9202)) opv
                     on m.visit_occurrence_id = opv.visit_occurrence_id
                     where m.measurement_concept_id = 3010813 and m.person_id in (", paste(Nonobesity_UCT$person_id, collapse = ', '), ')')

comparator_leukocyte <- dbGetQuery(conn, extractcode)
comparator_leukocyte <- merge(comparator_leukocyte, person, by = 'person_id')
colnames(comparator_leukocyte) <- c('person_id', 'measurement_date', 'leukocyte', 'birth', 'gender')
comparator_leukocyte <- merge(Nonobesity_UCT, comparator_leukocyte, by = 'person_id')
comparator_leukocyte <- comparator_leukocyte %>% filter(measurement_date >= cohortStartDate) %>% filter(cohortEndDate >= measurement_date) %>% mutate(followup_years = as.numeric(measurement_date - cohortStartDate)/365) %>% mutate(age = as.numeric(measurement_date - as.Date(substr(as.character(birth),1,10)))/365) %>% mutate(obesity = 'no')

extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv535_ABMI].[dbo].[measurement] m 
                     join (select * from [CDMPv535_ABMI].[dbo].[visit_occurrence] where visit_concept_id in (9202)) opv
                     on m.visit_occurrence_id = opv.visit_occurrence_id
                     where m.measurement_concept_id = 3018010 and m.person_id in (", paste(Nonobesity_UCT$person_id, collapse = ', '), ')')

comparator_eosinophil_percent <- dbGetQuery(conn, extractcode)
comparator_eosinophil_percent <- merge(comparator_eosinophil_percent, person, by = 'person_id')
colnames(comparator_eosinophil_percent) <- c('person_id', 'measurement_date', 'eosinophil_percent', 'birth', 'gender')
comparator_eosinophil_percent <- merge(Nonobesity_UCT, comparator_eosinophil_percent, by = 'person_id')
comparator_eosinophil_percent <- comparator_eosinophil_percent %>% filter(measurement_date >= cohortStartDate) %>% filter(cohortEndDate >= measurement_date) %>% mutate(followup_years = as.numeric(measurement_date - cohortStartDate)/365) %>% mutate(age = as.numeric(measurement_date - as.Date(substr(as.character(birth),1,10)))/365) %>% mutate(obesity = 'no')

comparator_eosinophil <- merge(comparator_leukocyte, comparator_eosinophil_percent %>% select(person_id, measurement_date, eosinophil_percent), by=c('person_id', 'measurement_date'))
comparator_eosinophil <- comparator_eosinophil %>% mutate(total_eosinophil_count = leukocyte * eosinophil_percent * 10)

target_eosinophil <- target_eosinophil %>% filter(followup_years<=10)
comparator_eosinophil <- comparator_eosinophil %>% filter(followup_years<=10)
target_eosinophil <- target_eosinophil %>% filter(total_eosinophil_count != 0)
comparator_eosinophil <- comparator_eosinophil %>% filter(total_eosinophil_count != 0)
total <- rbind(target_eosinophil, comparator_eosinophil)

lmm_model <- lmer(
  total_eosinophil_count ~ followup_years*obesity + age + gender
  + (1 | person_id),
  data = total
)

#################################
predicted <- emmeans(lmm_model, specs = ~ obesity, at = list(followup_years = 10), pbkrtest.limit = 5000, lmerTest.limit = 5000)
conf_int <- confint(predicted)

print(conf_int)
print(pairs(predicted))
#################################
sjPlot::plot_model(lmm_model, type = "pred", terms = c("followup_years", 'obesity'), axis.title = c('Years of follow-up', 'Eosinophil count (cells/mcL)'), title = '', alpha = 0) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "grey50")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .7, position = position_dodge(width = 0.2), linetype = 1) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_fill_manual(values=c("white", "black")) +
  scale_color_manual(values=c("black", "black")) +
  #scale_y_continuous(limits = c(20, 32)) + 
  aes(linetype=group, color=group) +
  geom_point(size = 4, stroke = .5, shape = 21, position = position_dodge(width = 0.05)) + 
  scale_x_discrete(limits=c(0, 2, 4, 6, 8, 10)) # 0, 1, 2, 3, 4, 5  ## 0, 2, 4, 6, 8, 10

#sjPlot::tab_model(lmm_model)

nrow(unique(total %>% filter(treatment == 1) %>% select(person_id)))
nrow(unique(total %>% filter(treatment == 0) %>% select(person_id)))
