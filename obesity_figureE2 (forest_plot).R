library(ggplot2)
library(ggthemes)
library(extrafont)

######### IV corticosteroid or biologics treatment ###########

df <- data.frame(study=c('10 years, 1:1 (main setting)', '10 years, 1:2', '10 years, stratification', '1 year, 1:1', '2 years, 1:1', '3 year, 1:1', '5 years, 1:1', 'ITT, 1:1'),
                 index=8:1,
                 effect=c(1.22, 1.10, 1.17, 1.24, 1.22, 1.26, 1.22, 1.22),
                 lower=c(0.99, 0.92, 1.02, 0.98, 0.98, 1.02, 0.99, 0.99),
                 upper=c(1.50, 1.31, 1.34, 1.58, 1.52, 1.56, 1.50, 1.50))

ggplot(data=df, aes(y=index, x=effect, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.2) +
  scale_y_continuous(breaks=1:nrow(df), labels=rev(df$study)) +
  geom_pointrange(shape = 22, fill = "black") +
  geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  labs(x='', y = '') +
  theme_tufte() +
  scale_x_continuous(trans="log10", limits=c(0.5, 2), breaks = c(0.5, 1, 2)) +
  theme(text=element_text(size=20, family="Serif"))

######### New-onset T2DM ###########

df <- data.frame(study=c('10 years, 1:1 (main setting)', '10 years, 1:2', '10 years, stratification', '1 year, 1:1', '2 years, 1:1', '3 year, 1:1', '5 years, 1:1', 'ITT, 1:1'),
                 index=8:1,
                 effect=c(3.86, 3.97, 3.73, 4.33, 5.33, 6.33, 4.00, 3.86),
                 lower=c(1.78, 2.07, 2.24, 1.40, 1.78, 2.16, 1.74, 1.78),
                 upper=c(9.62, 8.24, 6.42, 18.90, 22.92, 26.95, 10.80, 9.62))

ggplot(data=df, aes(y=index, x=effect, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.2) +
  scale_y_continuous(breaks=1:nrow(df), labels=rev(df$study)) +
  geom_pointrange(shape = 22, fill = "black") +
  geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  labs(x='', y = '') +
  theme_tufte() +
  scale_x_continuous(trans="log10", limits=c(0.5, 32), breaks = c(0.5, 1, 2, 5,  10, 30)) +
  theme(text=element_text(size=20, family="Serif"))

######### New-onset HTN ###########

df <- data.frame(study=c('10 years, 1:1 (main setting)', '10 years, 1:2', '10 years, stratification', '1 year, 1:1', '2 years, 1:1', '3 year, 1:1', '5 years, 1:1', 'ITT, 1:1'),
                 index=8:1,
                 effect=c(2.75, 2.54, 2.11, 3.87, 3.42, 2.59, 3.00, 2.75),
                 lower=c(1.68, 1.67, 1.55, 1.87, 1.85, 1.51, 1.80, 1.68),
                 upper=c(4.70, 3.94, 2.88, 9.05, 6.79, 4.65, 5.26, 4.70))

ggplot(data=df, aes(y=index, x=effect, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.2) +
  scale_y_continuous(breaks=1:nrow(df), labels=rev(df$study)) +
  geom_pointrange(shape = 22, fill = "black") +
  geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  labs(x='', y = '') +
  theme_tufte() +
  scale_x_continuous(trans="log10", limits=c(0.5, 10), breaks = c(0.5, 1, 2, 5,  10)) +
  theme(text=element_text(size=20, family="Serif"))

######### New-onset hyperlipidemia ###########

df <- data.frame(study=c('10 years, 1:1 (main setting)', '10 years, 1:2', '10 years, stratification', '1 year, 1:1', '2 years, 1:1', '3 year, 1:1', '5 years, 1:1', 'ITT, 1:1'),
                 index=8:1,
                 effect=c(2.08, 1.90, 1.44, 1.83, 2.11, 1.75, 2.17, 2.08),
                 lower=c(1.09, 1.11, 1.00, 0.70, 0.98, 0.87, 1.12, 1.09),
                 upper=c(4.16, 3.32, 2.08, 5.32, 4.90, 3.67, 4.45, 4.16))

ggplot(data=df, aes(y=index, x=effect, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.2) +
  scale_y_continuous(breaks=1:nrow(df), labels=rev(df$study)) +
  geom_pointrange(shape = 22, fill = "black") +
  geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  labs(x='', y = '') +
  theme_tufte() +
  scale_x_continuous(trans="log10", limits=c(0.5, 10), breaks = c(0.5, 1, 2, 5,  10)) +
  theme(text=element_text(size=20, family="Serif"))
