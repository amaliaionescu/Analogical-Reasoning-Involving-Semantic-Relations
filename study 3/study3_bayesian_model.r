library(readr)
require(brms)
require(rstan)
require(Rcpp)
require(scales)
require(gridExtra)
require(loo)
require(tidyverse)
require(readxl)

#for viz
library(ggExtra)
require(cowplot)
require(bayesplot)

#partial correlations
require(ppcor)
library(ggcorrplot)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

data <- read.csv('study3_data_long.csv')

data$Rating <- ifelse(data$Rating == 0, "no label", "label")
data$Rating <- factor(data$Rating, levels = c("no label", "label"))

# Set "no label" as the reference level
data$Rating <- relevel(data$Rating, ref = "no label")


model <- brm(response ~ Rating * trial_type + pos + age + (1 | Subj) + (1 | item),  
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


#alternative model for dissertation defense
data$Rating <- factor(data$Rating, levels = c("no label", "label"))

model2 <- brm(response ~ Rating * trial_type + Rating * age + pos + (1 | Subj) + (1 | item),  
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

# Plot the posterior distribution
post_plot <- mcmc_areas(posterior_samples, 
                        pars = c('b_Ratinglabel:trial_typesame'),
                        prob = 0.94, 
                        prob_outer = 0.99, 
                        point_est = "mean") +
             ggtitle('Posterior distribution of\nRating:trial_type')

# Print the plot
print(post_plot)

