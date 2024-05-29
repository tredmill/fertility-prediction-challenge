# Project:   fertility-prediction-challenge
# Objective: Diagnose NAs and imputations
# Author:    Edoardo Costantini
# Created:   2024-05-25
# Modified:  2024-05-25
# Notes:     Run this script with df_2020 processed data from the submission clean_df function

# Explore missing data ---------------------------------------------------------

# Find missing data patterns
mdpats <- mice::md.pattern(df_2020, plot = FALSE)

# Number of missing data patterns
dim(mdpats)

# Missing values per variable
round(mdpats[nrow(mdpats), -ncol(mdpats)]/nrow(df_2020) * 100, 1)

# Store the names of the variables in order of missing propostions
vars_permiss <- names(mdpats[nrow(mdpats), -ncol(mdpats)])

# Get table of possible values in order of missing values
shelf <- lapply(
  vars_permiss,
  function(x) {
    table(df_2020[, x], useNA = "always")
  }
)

# Name them
names(shelf) <- vars_permiss

# Print them
shelf

# Imputation for convergence check ---------------------------------------------

# Define methods
imp_methods <- mice::make.method(df_2020)

# Quick pred
predmat <- mice::quickpred(df_2020)

# Perform imputation in parallel
mice_out <- futuremice(
  n.core = 5,
  data = df_2020,
  m = 5,
  maxit = 1e2,
  method = imp_methods,
  predictorMatrix = predmat
)

# Convergence plots
plot(
  mice_out,
)

# Plot densities
densityplot(mice_out, layout = c(3, 1))

dim(complete(mice_out, 1))