library(readxl)
pxrf_toate_curate_primara_v2 <- read_excel("pxrf_toate_curate_primara_v2.xlsx", 
                                             +     sheet = "medii")

df_pxrf <- as.data.frame(pxrf_toate_curate_primara_v2[-1,])
df_pxrf$SITE <- as.factor(df_pxrf$SITE)

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

gr_dens_Si <- ggplot(df_pxrf, aes(x=SiO3, group=SITE, fill=SITE))+
  geom_density(adjust=1.5, alpha=.5)+
  theme_ipsum()
gr_dens_Si
