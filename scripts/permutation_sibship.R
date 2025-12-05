# Permutation test

# read libraries
library(tidyverse)
library(infer)
library(dplyr)

# Input: data to permute. In my code here, it's a 

sibdata <- read_csv("~/Documents/GitHub/sociality_lifehist/data/permutation_sibdat.csv") |> 
  mutate(fullSib_index = as.factor(fullSib_index))

# --- Step 1: Calculate the Observed Test Statistic ---

# calculate observed chi-seq
obs_stat <- sibdata |>
  specify(formula = fullSib_index ~ treatment_status) |>
  calculate(stat = "Chisq")


# --- Step 2: Establish the Null Distribution ---
## Establish null distribution
null_fits <- sibdata |> 
  specify(fullSib_index ~ treatment_status) |>
  hypothesize(null = "independence") |> 
  generate(reps = 10000,
           type = "permute") |> 
  calculate(stat = "Chisq")

# --- Step 3: Calculate p-value ---
null_fits |> 
  get_p_value(obs_stat = pull(obs_stat, stat),
              direction = "both")

## Plot p-value
null_fits |> 
  ggplot(aes(x = stat)) +
  geom_histogram() +
  geom_vline(xintercept = pull(obs_stat, stat),
             color = "red") +
  theme_classic()+
  ylab("Number of permutations")+
  xlab("Observed Chi-squared")
