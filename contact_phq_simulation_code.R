#############################################
#### Simulating Police Contact & Depressive Symptoms ####
#############################################
### Workspace setup ####


#library(groundhog)
#groundhog.library(dplyr, "2025-11-15")
#groundhog.library(faux, "2025-11-15")
#groundhog.library(summarytools, "2025-11-15")
#groundhog.library(missMethods, "2025-11-15")

library(dplyr)
library(groundhog)
library(faux)
library(summarytools)
library(missMethods)


sessionInfo()

# R version 4.5.1
# groundhog_3.2.3     
# dplyr_1.1.4 
# faux_1.2.3   
# summarytools_1.1.3
# missMethods_0.4.0

#setting seed


set.seed(123)
#sample size
N <- 500

# participant id
participant_id <- 1:N

# race (0 = BIPOC, 1 = White)
race <- rbinom(N, 1, 0.4)   # assume 40% White

# police stop (more common among BIPOC)
pcon <- ifelse(race == 0,
               rbinom(N, 1, 0.35),  # BIPOC
               rbinom(N, 1, 0.15))  # White

# ---- Generate PHQ item-level data ----
# Base depression level by race
race_eff <- ifelse(race == 0, 0.4, 0.2)

# Extra depression effect for police stop
police_eff <- ifelse(pcon == 1, 0.9, 0.4)

# Stronger police-stop effect among BIPOC
interaction_eff <- ifelse(race == 0 & pcon == 1, 0.6, 0.3)

# latent (continuous) depression severity
latent_dep <- 0.5 + race_eff + police_eff + interaction_eff + rnorm(N, 0, 0.6)
# Function to convert latent score to PHQ response categories
assign_phq <- function(x){
  cut(x,
      breaks = c(-Inf, 0.5, 1.2, 1.9, Inf),
      labels = c(0,1,3,4),  # PHQ values
      right = FALSE)
}

# generate 9 phq items
phq_1 <- as.numeric(assign_phq(latent_dep + rnorm(N,0,0.4)))
phq_2 <- as.numeric(assign_phq(latent_dep + rnorm(N,0,0.4)))
phq_3 <- as.numeric(assign_phq(latent_dep + rnorm(N,0,0.4)))
phq_4 <- as.numeric(assign_phq(latent_dep + rnorm(N,0,0.4)))
phq_5 <- as.numeric(assign_phq(latent_dep + rnorm(N,0,0.4)))
phq_6 <- as.numeric(assign_phq(latent_dep + rnorm(N,0,0.4)))
phq_7 <- as.numeric(assign_phq(latent_dep + rnorm(N,0,0.4)))
phq_8 <- as.numeric(assign_phq(latent_dep + rnorm(N,0,0.4)))
phq_9 <- as.numeric(assign_phq(latent_dep + rnorm(N,0,0.4)))

# Combine into a dataset
sim_data <- data.frame(
  participant_id,
  race = factor(race, levels=c(0,1), labels=c("BIPOC","White")),
  pcon = factor(pcon, levels=c(0,1), labels=c("No","Yes")),
  phq_1, phq_2, phq_3, phq_4, phq_5, phq_6, phq_7, phq_8, phq_9
)

#Introducing missingness at random
sim_data <- missMethods::delete_MCAR(sim_data, .02, "phq_1")
sim_data <- missMethods::delete_MCAR(sim_data, .03, "phq_2")
sim_data <- missMethods::delete_MCAR(sim_data, .04, "phq_3")
sim_data <- missMethods::delete_MCAR(sim_data, .03, "phq_4")
sim_data <- missMethods::delete_MCAR(sim_data, .07, "phq_5")
sim_data <- missMethods::delete_MCAR(sim_data, .09, "phq_6")
sim_data <- missMethods::delete_MCAR(sim_data, .02, "phq_7")
sim_data <- missMethods::delete_MCAR(sim_data, .09, "phq_8")
sim_data <- missMethods::delete_MCAR(sim_data, .11, "phq_9")
sim_data <- missMethods::delete_MCAR(sim_data, .09, "pcon")


# PHQ-9 total score
sim_data$phq_total <- rowSums(sim_data[, grep("phq_", names(sim_data))])

head(sim_data)
View(sim_data)
