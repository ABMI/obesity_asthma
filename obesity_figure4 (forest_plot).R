library(ggplot2)
library(ggthemes)
library(extrafont)

######### SAE ###########

df <- data.frame(study=c('10 years, 1:1 (main setting)', '10 years, 1:2', '10 years, stratification', '1 year, 1:1', '2 years, 1:1', '3 year, 1:1', '5 years, 1:1', 'ITT, 1:1'),
                 index=8:1,
                 effect=c(1.83, 1.80, 1.60, 2.09, 1.87, 1.88, 2.06, 1.83),
                 lower=c(1.05, 1.14, 1.09, 1.04, 1.01, 1.04, 1.15, 1.05),
                 upper=c(3.32, 2.88, 2.35, 4.46, 3.58, 3.52, 3.84, 3.32))

ggplot(data=df, aes(y=index, x=effect, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.2) +
  scale_y_continuous(breaks=1:nrow(df), labels=rev(df$study)) +
  geom_pointrange(shape = 22, fill = "black") +
  geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  labs(x='', y = '') +
  theme_tufte() +
  scale_x_continuous(trans="log10", limits = c(0.25, 5), breaks = c(0.5, 1, 2, 5)) + 
  theme(text=element_text(size=20, family="Serif"))

######### Incompletely controlled asthma ###########

df <- data.frame(study=c('10 years, 1:1 (main setting)', '10 years, 1:2', '10 years, stratification', '1 year, 1:1', '2 years, 1:1', '3 year, 1:1', '5 years, 1:1', 'ITT, 1:1'),
                 index=8:1,
                 effect=c(1.16, 1.10, 1.10, 1.18, 1.17, 1.16, 1.17, 1.16),
                 lower=c(1.02, 0.98, 1.01, 1.02, 1.02, 1.01, 1.02, 1.02),
                 upper=c(1.33, 1.24, 1.21, 1.37, 1.35, 1.33, 1.34, 1.33))

ggplot(data=df, aes(y=index, x=effect, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.2) +
  scale_y_continuous(breaks=1:nrow(df), labels=rev(df$study)) +
  geom_pointrange(shape = 22, fill = "black") +
  geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  labs(x='', y = '') +
  theme_tufte() +
  scale_x_continuous(trans="log10", limits = c(0.25, 2), breaks = c(0.5, 1, 2)) + 
  theme(text=element_text(size=20, family="Serif"))
