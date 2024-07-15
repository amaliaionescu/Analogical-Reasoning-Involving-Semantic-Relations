library(readr)
require(brms)
require(rstan)
require(Rcpp)
require(scales)
require(gridExtra)
require(loo)
require(tidyverse)
require(readxl)
library(dplyr)
library(tidyr)

#for viz
library(ggExtra)
require(cowplot)
require(bayesplot)

#partial correlations
require(ppcor)
library(ggcorrplot)

#run stan computations on multiple cores
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

data <- read_csv('antdata_long.csv')

model <- brm(response ~ condition * age + pos + (1 | subj) + (1 | item),  
                          data=data, 
                          family = bernoulli(link = "logit"),
                          warmup = 500, 
                          iter = 2000, 
                          chains = 4, 
                          inits= "0", 
                          cores= 4,
                          seed = 420)

summary(model)
pp_check(model) #checks posterior tracks data
conditional_effects(model) #conditional effects

#group level effects
ranef(model, groups = 'item', probs = .95)
