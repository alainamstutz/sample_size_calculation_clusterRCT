---
title: "sample size calculations, various RCTs and surveys"
author: "A.Amstutz"
date: "2023-10-18"
output:
  html_document:
    keep_md: yes
    toc: yes
    toc_float: yes
    code_folding: hide
  pdf_document:
    toc: yes
---

# Load packages

```r
library(tidyverse)
library(readxl)
library(writexl)
library(here)
library(kableExtra)
library(ggplot2)

library(pwr)
```

# Sample size for a binary outcome / individual randomized trial


# Sample size calculation for a continous outcome / individual randomized trial

```r
# Define the parameters
alpha <- 0.05 # Significance level (usually 0.05 for a 95% confidence interval)
power <- 0.80 # Desired power (usually 0.80 or 80% power)
mean_int <- 5 # Mean of the control group
mean_cont <- 0 # Mean of the intervention group
sd <- 10 # standard deviation of the outcome. We need to make a guess at the population standard deviation. If we have absolutely no idea, one rule of thumb is to take the difference between the maximum and minimum values and divide by 4. Let's say the maximum purchase is $10 and the minimum purchase is $1. Our estimated standard deviation is (10 - 1)/4 = 2.25
  
# Calculate the effect size
effect_size <- (mean_int - mean_cont) / sd

# Calculate the sample size for 1 arm
sample_size_1arm <- pwr.t.test(d = effect_size, 
                          sig.level = alpha, 
                          power = power)
sample_size <- sample_size_1arm$n * 2
# Print
cat("Required sample size:", round(sample_size, 0))
```

```
## Required sample size: 128
```

```r
#####
# Define the parameters for a "manual calculation"
alpha <- 0.05 # Significance level
power <- 0.80 # Desired power
mean_int <- 5 # Mean of the control group
mean_cont <- 0 # Mean of the intervention group
sd <- 10 # Estimated standard deviation

# Calculate the effect size (delta)
delta <- abs(mean_int - mean_cont)

# Calculate the critical values for Z_alpha/2 and Z_beta
Z_alpha_half <- qnorm(1 - alpha / 2)
Z_beta <- qnorm(power)

# Calculate the sample size for 1 arm
sample_size_1arm <- (2 * (sd^2) / delta^2) * ((Z_alpha_half + Z_beta)^2)
sample_size <- sample_size_1arm * 2
# Print the sample size
cat("Required sample size:", round(sample_size, 0))
```

```
## Required sample size: 126
```

# Sample size calculation for a cluster randomized trial // continuous outcome

```r
# Define the parameters
alpha <- 0.05 # Significance level 
power <- 0.80 # Desired power
mean_int <- 5 # Mean of the control group
mean_cont <- 0 # Mean of the intervention group
sd <- 10 # Estimated standard deviation of the outcome
cluster_size <- 50 # Average cluster size
icc <- 0.01 # Intracluster correlation coefficient

# Calculate the effect size
delta <- abs(mean_int - mean_cont)

# Calculate the critical values for Z_alpha/2 and Z_beta
Z_alpha_half <- qnorm(1 - alpha / 2)
Z_beta <- qnorm(power)

# Calculate the design effect (or Variance Inflation Factor (VIF))
design_effect <- 1 + (cluster_size - 1) * icc

# Calculate the total sample size (individual level) by inflating the SS for an individual RCT by the VIF
sample_size_1arm <- (2 * (sd^2) / delta^2) * ((Z_alpha_half + Z_beta)^2)
sample_size <- sample_size_1arm * 2
sample_size_cluster_ind <- sample_size * design_effect
cat("Required sample size:", round(sample_size_cluster_ind, 0))
```

```
## Required sample size: 187
```

```r
# Number of clusters
n_clusters <- sample_size_cluster_ind / cluster_size
cat("Required clusters:", round(n_clusters, 0))
```

```
## Required clusters: 4
```

# Sample size calculation for a cluster randomized trial // binary outcome

```r
# Define the parameters
alpha <- 0.05 # alpha level
power <- 0.80 # power
p_int <- 0.55 # Proportion of the binary outcome in the intervention group
p_cont <- 0.50 # Proportion of the binary outcome in the control group
cluster_size <- 50  # Average cluster size
icc <- 0.01  # Intracluster correlation coefficient

# Calculate the design effect
design_effect <- 1 + (cluster_size - 1) * icc

# Calculate the critical values for Z_alpha/2 and Z_beta
Z_alpha_half <- qnorm(1 - alpha / 2)
Z_beta <- qnorm(power)

# Calculate the total sample size (individual level) by inflating the SS for an individual RCT by the VIF
sample_size_1arm <- ((Z_alpha_half + Z_beta)^2 * (p_int * (1 - p_int) + p_cont * (1 - p_cont))) / (p_int - p_cont)^2
sample_size <- sample_size_1arm * 2

sample_size_cluster_ind <- sample_size * design_effect
cat("Required sample size:", round(sample_size_cluster_ind, 0))
```

```
## Required sample size: 4655
```

```r
# Number of clusters
n_clusters <- sample_size_cluster_ind / cluster_size
cat("Required clusters:", round(n_clusters, 0))
```

```
## Required clusters: 93
```

# Sample size calculation for a clustered survey

```r
# Define the parameters
confidence_level <- 0.95 # confidence level
z <- qnorm(1 - (1 - confidence_level) / 2) # z statistic
d <- 0.05 # margin of error
cluster_size <- 3 # Average cluster size (fixed in our case)
icc <- 0.05 # Intracluster correlation coefficient
prop <- 0.7 # Estimated target proportion/prevalence

# Calculate design effect
deff <- 1 + (cluster_size - 1) * icc # Design Effect

# Calculate sample size
sample_size <- ceiling((z * sqrt(prop * (1 - prop)) / d)^2 / deff)

# Print
cat("Required sample size:", sample_size)
```

```
## Required sample size: 294
```
