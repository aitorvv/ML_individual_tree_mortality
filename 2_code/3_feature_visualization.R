#------------------------------------------------------------------------------------------#
####                                   Visualize data                                   ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 02/06/2023                              #
#------------------------------------------------------------------------------------------#

# Variables are manually compared and selected avoiding correlation between variables
# of the same group. Candidate variables are chosen for each group, making on the 
# next step all the variables combinations

# That script is just to visualize!

## Resources

# https://www.youtube.com/watch?v=ffhQil7KhSo&ab_channel=yuzaRDataScience


#### Initial setup ####

library(PerformanceAnalytics)
library(tidyverse)
library(GGally)
library(flextable)
library(dlookr)
#library(ggstatsplot)

setwd('ML_individual_tree_mortality/')

df_original <- read.csv('1_data/tmp_DEN/2_clima/df_complete_r33.csv')


#### Features correlation exploration: tree size ####

#df <- select(df_original, age, dbh, height, ba)
df <- select(df_original, dbh)


#### Features correlation exploration: tree vigour ####

# df <- select(df_original, age, dbh, slenderness, mean_slenderness, dom_slenderness, 
#rel_dbh_Do, rel_h_Ho, rel_dbh_Dg, rel_h_Hm)
df <- select(df_original, site, dbh, slenderness)

#df$rel_slenderness <- df_original$slenderness/df_original$mean_slenderness
#df$rel_dom_slenderness <- df_original$slenderness/df_original$dom_slenderness


#### Features correlation exploration: tree growth ####

df <- select(df_original, site, dbh, i_dbh, i_annual_dbh, i_year, SI_100)


#### Features correlation exploration: competition ####

#df <- select(df_original, age, dbh, bal_subplot, G, G_local, Hegyi)
df <- select(df_original, site, dbh, bal_subplot, G, G_local, Hegyi)
df <- select(df_original, dbh, i_annual_dbh, G, G_local)
df <- select(df_original, dbh, i_annual_dbh, bal_subplot, Hegyi)

df$Hegyi <- ifelse(df$Hegyi == Inf, 0, df$Hegyi)


#### Features correlation exploration: density ####

#df <- select(df_original, age, dbh, G_local, 
#             SDI, N, S)
df <- select(df_original, dbh, i_annual_dbh, bal_subplot, Hegyi,
             N, SDI, S)
df$Hegyi <- ifelse(df$Hegyi == Inf, 0, df$Hegyi)


#### Features correlation exploration: stand size ####

df <- select(df_original, age, dbh, 
             DBHm, Dg, Do, Hm, Ho)#, mean_slenderness, dom_slenderness)
#df <- select(df_original, age, dbh, N,
#             Dg, mean_slenderness, dom_slenderness)

df <- select(df_original, dbh, N,
             Dg, Do, Ho) 


#### Features correlation exploration: mixture ####

#df <- select(df_original, age, dbh, N, Ho, mean_slenderness,
#  Dg_sp_1, DBHm_sp_1, Hm_sp_1, N_sp_1, G_sp_1,
#  Dg_sp_2, DBHm_sp_2, Hm_sp_2, N_sp_2, G_sp_2)

#df <- select(df_original, age, dbh, N, Ho, mean_slenderness,
#             Dg_sp_1, Hm_sp_1, N_sp_1, G_sp_1)
#             Dg_sp_2, Hm_sp_2, N_sp_2, G_sp_2)


#### Features correlation exploration: clima ####

#df <- select(df_original, dbh, M, prec, tmean, SPEI3_aug, SPEI6_aug, SPEI12_aug)

df <- select(df_original, M, SPEI3_aug, SPEI6_aug, SPEI12_aug)


#### Visualize data ####

# how is our data distribution and try to find groups 
chart.Correlation(df)

# plot correlation graph using groups 
ggpairs(df,
        #aes(colour = age),
        lower = list(continuous = 'smooth'))

ggpairs(df,
        aes(colour = as.factor(site)),
        lower = list(continuous = 'smooth'))

# check normal distribution: shapiro-wilk test
# if p-value < 0.05 --> variable NON normal distributed
df %>%
  normality() %>%
  mutate(across(is.numeric, ~round(., 3))) %>%
  regulartable()

df %>%
  group_by(site) %>%  # grouping
  normality() %>%
  mutate(across(is.numeric, ~round(., 3))) %>%
  regulartable()

# correlation selection by groups
grouped_ggcorrmat(
  df,
  type = 'parametric',
  grouping.var = site,
  plotgrid.args = list(nrow = 2)
)




