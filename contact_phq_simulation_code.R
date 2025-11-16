set.seed(123)

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
police_eff <- ifelse(pcon == 1, 0.9, 0)

# Stronger police-stop effect among BIPOC
interaction_eff <- ifelse(race == 0 & pcon == 1, 0.6, 0)

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

# PHQ-9 total score
sim_data$phq_total <- rowSums(sim_data[, grep("phq_", names(sim_data))])

head(sim_data)
View(sim_data)
