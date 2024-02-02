library(obesity11121)
launchEvidenceExplorer(dataFolder = "~/output/shinyData", blind = FALSE, launch.browser = FALSE)

# Cumulative incidence curve (Figure 2)

#SAE
pop = readRDS("~/output/cmOutput/StratPop_l1_s1_p1_t3316_c3317_s1_o3262.rds")
plotKaplanMeier(pop, confidenceIntervals = F, )

#AE
pop = readRDS("~/output/cmOutput/StratPop_l1_s1_p1_t3316_c3317_s1_o3997.rds")
plotKaplanMeier(pop, confidenceIntervals = F, )

#IV CS or biologics prescription
pop = readRDS("~/output/cmOutput/StratPop_l1_s1_p1_t3316_c3317_s1_o3691.rds")
plotKaplanMeier(pop, confidenceIntervals = F, )

#New-onset T2DM
pop = readRDS("~/output/cmOutput/StratPop_l1_s1_p1_t3316_c3317_s1_o3265.rds")
plotKaplanMeier(pop, confidenceIntervals = F, )

#New-onset HTN
pop = readRDS("~/output/cmOutput/StratPop_l1_s1_p1_t3316_c3317_s1_o3264.rds")
plotKaplanMeier(pop, confidenceIntervals = F, )

#New-onset hyperlipidemia
pop = readRDS("~/output/cmOutput/StratPop_l1_s1_p1_t3316_c3317_s1_o3621.rds")
plotKaplanMeier(pop, confidenceIntervals = F, )
