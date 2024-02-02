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
#                                                                 password = 'gks1dls!',
#                                                                 pathToDriver = 'C:/jdbc_driver')
# conn <- DatabaseConnector::connect(connectionDetails)
# ###############################
# launchEvidenceExplorer(dataFolder = 'C:/Users/hihip/OneDrive - Ajou University/important/study/이카루스obesity/obesity09201/output/shinyData', blind = FALSE, launch.browser = FALSE)
# ################################
# con <- dbConnect(RSQLite::SQLite(), dbname="C:/Users/hihip/OneDrive - Ajou University/important/study/이카루스obesity/obesity09201/output/cmOutput/CmData_l1_t3316_c3317/filec68f58cec8f2.sqlite")
# data <- dbGetQuery(con, "SELECT * FROM cohorts")
# connected_db <- data %>% select(personSeqId, personId)
# colnames(connected_db) <- c('personSeqId', 'person_id')
# 
# person <- dbGetQuery(conn,"SELECT person_id, birth_datetime, gender_source_value FROM [CDMPv535_ABMI].[dbo].[person]")

StratPop_l1_s2_p1_t3316_c3317_s1_o3954 <- readRDS("C:/Users/hihip/OneDrive - Ajou University/important/study/이카루스obesity/obesity09201/output/cmOutput/StratPop_l1_s2_p1_t3316_c3317_s1_o3263.rds")
Obesity <- StratPop_l1_s2_p1_t3316_c3317_s1_o3954 %>% filter(treatment == 1)
Nonobesity <- StratPop_l1_s2_p1_t3316_c3317_s1_o3954 %>% filter(treatment == 0)

Obesity <- merge(Obesity, connected_db) %>% select(person_id, treatment, cohortStartDate, daysToCohortEnd, daysToEvent)
Nonobesity <- merge(Nonobesity, connected_db) %>% select(person_id, treatment, cohortStartDate, daysToCohortEnd, daysToEvent)

colnames(Obesity) <- c('person_id', 'treatment', 'cohortStartDate', 'cohortEndDate', 'daysToEvent')
colnames(Nonobesity) <- c('person_id', 'treatment', 'cohortStartDate', 'cohortEndDate', 'daysToEvent')
Obesity <- Obesity %>% mutate(cohortEndDate = cohortStartDate + cohortEndDate)
Nonobesity <- Nonobesity %>% mutate(cohortEndDate = cohortStartDate + cohortEndDate)

Obesity_WCT <- Obesity %>% filter(is.na(daysToEvent)) %>% select(person_id, cohortStartDate, cohortEndDate, treatment)
Obesity_UCT <- Obesity %>% filter(!is.na(daysToEvent)) %>% select(person_id, cohortStartDate, cohortEndDate, treatment)
Nonobesity_WCT <- Nonobesity %>% filter(is.na(daysToEvent)) %>% select(person_id, cohortStartDate, cohortEndDate, treatment)
Nonobesity_UCT <- Nonobesity %>% filter(!is.na(daysToEvent)) %>% select(person_id, cohortStartDate, cohortEndDate, treatment)

####################################
# IgE(IU/mL) : 42529219 (20, 580) / Fasting blood glucose (mg/dL) : 3040820 / Serum leukocytes (X10³/㎕) : 3010813 / Expired nitric oxide : 40480257

extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv535_ABMI].[dbo].[measurement] m
                     where m.measurement_concept_id = 3040820 and m.person_id in (", paste(Obesity_UCT$person_id, collapse = ', '), ')')
# extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv535_ABMI].[dbo].[measurement] m
#                      join (select * from [CDMPv535_ABMI].[dbo].[visit_occurrence] where visit_concept_id in (9202)) opv
#                      on m.visit_occurrence_id = opv.visit_occurrence_id
#                      where m.measurement_concept_id = 3010813 and m.person_id in (", paste(Obesity_UCT$person_id, collapse = ', '), ')')

obesity_group <- dbGetQuery(conn, extractcode)
obesity_group <- merge(obesity_group, person, by = 'person_id')
colnames(obesity_group) <- c('person_id', 'measurement_date', 'value_as_number', 'birth', 'gender')
obesity_group <- merge(Obesity_UCT, obesity_group, by = 'person_id')
obesity_group <- obesity_group %>% filter(measurement_date >= cohortStartDate) %>% filter(cohortEndDate >= measurement_date) %>% mutate(followup_years = as.numeric(measurement_date - cohortStartDate)/365) %>% mutate(age = as.numeric(measurement_date - as.Date(substr(as.character(birth),1,10)))/365) %>% mutate(obesity = 'yes')

extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv535_ABMI].[dbo].[measurement] m
                     where m.measurement_concept_id = 3040820 and m.person_id in (", paste(Nonobesity_UCT$person_id, collapse = ', '), ')')
# extractcode <- paste("select m.person_id, m.measurement_date, m.value_as_number from [CDMPv535_ABMI].[dbo].[measurement] m
#                      join (select * from [CDMPv535_ABMI].[dbo].[visit_occurrence] where visit_concept_id in (9202)) opv
#                      on m.visit_occurrence_id = opv.visit_occurrence_id
#                      where m.measurement_concept_id = 3010813 and m.person_id in (", paste(Nonobesity_UCT$person_id, collapse = ', '), ')')

no_obesity_group <- dbGetQuery(conn, extractcode)
no_obesity_group <- merge(no_obesity_group, person, by = 'person_id')
colnames(no_obesity_group) <- c('person_id', 'measurement_date', 'value_as_number', 'birth', 'gender')
no_obesity_group <- merge(Nonobesity_UCT, no_obesity_group, by = 'person_id')
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
predicted <- emmeans(lmm_model, specs = ~ obesity, at = list(followup_years = 10), pbkrtest.limit = 10000, lmerTest.limit = 10000)
conf_int <- confint(predicted)

print(conf_int)
print(pairs(predicted))
#################################
sjPlot::plot_model(lmm_model, type = "pred", terms = c("followup_years", 'obesity'), axis.title = c('Years of follow-up', 'Serum leukocytes (X10³/㎕)'), title = '', alpha = 0) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "grey50")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .7, position = position_dodge(width = 0.2), linetype = 1) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_fill_manual(values=c("white", "black")) +
  scale_color_manual(values=c("black", "black")) +
  #scale_y_continuous(limits = c(150, 650)) + 
  aes(linetype=group, color=group) +
  geom_point(size = 4, stroke = .5, shape = 21, position = position_dodge(width = 0.05)) + 
  scale_x_discrete(limits=c(0, 2, 4, 6, 8, 10)) # 0, 1, 2, 3, 4, 5  ## 0, 2, 4, 6, 8, 10

#sjPlot::tab_model(lmm_model)

nrow(unique(total %>% filter(treatment == 1) %>% select(person_id)))
nrow(unique(total %>% filter(treatment == 0) %>% select(person_id)))
