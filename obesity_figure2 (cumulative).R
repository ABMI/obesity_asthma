library(obesity11121)
launchEvidenceExplorer(dataFolder = "C:/Users/hihip/OneDrive - Ajou University/important/study/이카루스obesity/obesity11121/output/shinyData", blind = FALSE, launch.browser = FALSE)

# Cumulative incidence curve (Figure 2)

#SAE
pop = readRDS("C:/Users/hihip/OneDrive - Ajou University/important/study/이카루스obesity/obesity09201/output/cmOutput/StratPop_l1_s1_p1_t3316_c3317_s1_o3262.rds")
plotKaplanMeier(pop, confidenceIntervals = F, )

#AE
pop = readRDS("C:/Users/hihip/OneDrive - Ajou University/important/study/이카루스obesity/obesity11121/output/cmOutput/StratPop_l1_s1_p1_t3316_c3317_s1_o3997.rds")
plotKaplanMeier(pop, confidenceIntervals = F, )

#IV CS or biologics prescription
pop = readRDS("C:/Users/hihip/OneDrive - Ajou University/important/study/이카루스obesity/obesity09201/output/cmOutput/StratPop_l1_s1_p1_t3316_c3317_s1_o3691.rds")
plotKaplanMeier(pop, confidenceIntervals = F, )

#New-onset T2DM
pop = readRDS("C:/Users/hihip/OneDrive - Ajou University/important/study/이카루스obesity/obesity09201/output/cmOutput/StratPop_l1_s1_p1_t3316_c3317_s1_o3265.rds")
plotKaplanMeier(pop, confidenceIntervals = F, )

#New-onset HTN
pop = readRDS("C:/Users/hihip/OneDrive - Ajou University/important/study/이카루스obesity/obesity09201/output/cmOutput/StratPop_l1_s1_p1_t3316_c3317_s1_o3264.rds")
plotKaplanMeier(pop, confidenceIntervals = F, )

#New-onset hyperlipidemia
pop = readRDS("C:/Users/hihip/OneDrive - Ajou University/important/study/이카루스obesity/obesity09201/output/cmOutput/StratPop_l1_s1_p1_t3316_c3317_s1_o3621.rds")
plotKaplanMeier(pop, confidenceIntervals = F, )
