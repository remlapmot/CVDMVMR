library(readxl)
library(dplyr)
if (!requireNamespace('ggforestplot')) remotes::install_github("NightingaleHealth/ggforestplot")
library(ggforestplot)
library(ggplot2)
library(ggforce)

# Figure 6 ----

dat <- read_excel('redo-figures.xlsx', sheet = 'fig6')

dat <- dat |> 
  mutate(logor = log(oddsratio),
         logorlow = log(orlowci),
         logorupp = log(oruppci))

dat <- dat |> 
  mutate(logorse = (logorupp - logorlow)/(2 * 1.96))

dat <- dat |> 
  mutate(Estimator = estimator)

# Forestplot

fig6 <- forestplot(
  df = dat,
  name = outcome,
  estimate = logor,
  se = logorse,
  logodds = TRUE,
  colour = Estimator,
  shape = Estimator,
  xlab = "Odds ratio with 95% CI (IVW and MVMR estimates)"
) + 
  ggplot2::theme_minimal() + 
  ggplot2::scale_colour_grey(start = 0, end = .6)
fig6
ggplot2::ggsave('img/fig6.pdf')
ggplot2::ggsave('img/fig6.tiff')

# Figure 5 ----

df <- read_excel('redo-figures.xlsx', sheet = 'fig5')

df <- df |> 
  mutate(logor = log(oddsratio),
         logorlow = log(orlowci),
         logorupp = log(oruppci))

df <- df |> 
  mutate(logorse = (logorupp - logorlow)/(2 * 1.96))

# Forestplot

fig5 <- forestplot(
  df = df,
  name = Exposure,
  estimate = logor,
  se = logorse,
  logodds = TRUE,
  colour = Estimator,
  shape = Estimator,
  xlab = "Odds ratio with 95% CI (IVW and MVMR estimates)"
) + 
  ggplot2::theme_minimal() + 
  ggplot2::scale_colour_grey(start = 0, end = .6) +
  ggforce::facet_col(
    facets = ~Outcome,
    scales = "free_y",
    space = "free"
  )
fig5
ggplot2::ggsave('img/fig5.pdf')
ggplot2::ggsave('img/fig5.tiff')
