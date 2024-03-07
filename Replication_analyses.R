# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)

# Replication data loading and prep ---------------------
rep_data <- read_csv("replication_data.csv")
head(rep_data)

# Convert to long dataset

rep_data_long <- rep_data %>%
  gather(key = "condition", value = "max_squat", Placebo, Caffeine)
head(rep_data_long, 3)

# add differences column to wide dataset

rep_data <- rep_data %>% 
  mutate(differences =  Caffeine - Placebo) 

## Replication descriptives  ---------------

rep_desc <- rep_data_long %>%
  group_by(condition) %>%
  summarise(count = n(),
            mean = mean(max_squat),
            sd = sd(max_squat)) %>%
  mutate(mean_diff = mean(rep_data$differences), 
         sd_diff = sd(rep_data$differences))

## Resolving assumptions  ---------------------------------------

## Distribution check 

ggplot(rep_data_long, aes(max_squat)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(rep_data_long, aes(condition, max_squat, color = condition)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

rep_data %>%
  identify_outliers(differences)

### Normality check  

rep_data %>% shapiro_test(differences) 

## Paired t-test  -----------------------------

replication_ttest <- t.test(max_squat ~ condition, rep_data_long, 
                  alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
replication_ttest

### Replication effect size calculation ------

rep_dz <- d.dep.t.diff.t(t = replication_ttest$statistic, n = rep_desc$count[1], a = 0.05)
rep_dz

rep_hedges <- g.ind.t(m1 = rep_desc$mean[1], m2 = rep_desc$mean[2], 
                       sd1 = rep_desc$sd[1], sd2 = rep_desc$sd[2], 
                       n1 = 15, n2 = 15, a = 0.05)
rep_hedges 


# Original study data ------

orig_data <- read_csv("original_data.csv")
head(orig_data)

# Convert to long dataset

orig_data_long <- orig_data %>%
  gather(key = "condition", value = "max_squat", Placebo, Caffeine)
head(orig_data_long, 3)

# add differences column to wide dataset

orig_data <- orig_data %>% 
  mutate(differences =  Caffeine - Placebo) 

## Original descriptives  ---------------

orig_desc <- orig_data_long %>%
  group_by(condition) %>%
  summarise(count = n(),
            mean = mean(max_squat),
            sd = sd(max_squat)) %>%
  mutate(mean_diff = mean(orig_data$differences), 
         sd_diff = sd(orig_data$differences))

## Resolving assumptions  ---------------------------------------

## Distribution check 

ggplot(orig_data_long, aes(max_squat)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(orig_data_long, aes(condition, max_squat, color = condition)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

orig_data %>%
  identify_outliers(differences)

### Normality check  

orig_data %>% shapiro_test(differences) 

## Paired t-test  -----------------------------

original_ttest <- t.test(max_squat ~ condition, orig_data_long, 
                            alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
original_ttest

### Original effect size calculation ------

orig_dz <- d.dep.t.diff.t(t = original_ttest$statistic, n = orig_desc$count[1], a = 0.05)
orig_dz

# Note - the original study reported Hedges g for the paired t test effect size

orig_hedges <- g.ind.t(m1 = orig_desc$mean[1], m2 = orig_desc$mean[2], 
        sd1 = orig_desc$sd[1], sd2 = orig_desc$sd[2], 
        n1 = 15, n2 = 15, a = 0.05)
orig_hedges 

# This calculation does not match the original effect size exactly (0.27)

# Replication analyses - z-test --------

# Compare dz
rep_test <- compare_smd(
  smd2 = rep_dz$d,
  n2 = rep_desc$count[1],
  smd1 = orig_dz$d,
  n1 = orig_desc$count[1],
  paired = TRUE,
  alternative = "greater")
rep_test


# Bootstrap Z-test 

boot_test = boot_compare_smd(x1 = orig_data$differences,
                             x2 = rep_data$differences,
                                 paired = TRUE,
                             alternative = "greater")

boot_test

# Table of bootstrapped CIs
knitr::kable(boot_test$df_ci, digits = 4)
