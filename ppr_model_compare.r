#########################################################################
# PPR LA DECISIONS ####
#########################################################################


rm(list=ls()) # remove existing variables

### note on naming convention ###
  # Available Parcels = 0; benefits are counted
  # Conserved (Purchased) Parcels = 1 ; benefits are counted
  # Converted (Lost) Parcels = 2 ; benefits are not counted


##################################################
#      CHOOSE STATIONARITY SCENARIO TO RUN        #####
##################################################
#
# Set this variable to control the input data (variable or constant ecological data)
# Options: "constant" or "variable"
#
benefit_scenario <- "variable"

##################################################
#      CALL REFERENCE SCRIPTS        ####
##################################################

library(MDPtoolbox)
library(graphics)
library(dplyr)

##################################################
#      CALL SOURCE SCRIPTS        ####
##################################################

source('Input_data.r')
source('mdp_builder/mdp_finite_horizon_nonStationary.r')
source('solver_greedy.r')
source('solver_mdp_myopic.R')
source('solver_mdp_nonstationary.r')
source('R/explore_solution_PPR.r')
source('R/dec2binvec.r')
source('R/getSite.r')
source('R/binvec2dec.r')
source('R/getState.r')

##################################################
#      BUILD & SOLVE MDP    ####
#################################################

## Build the MDP
# Generate the transition and reward matrix
PR <- mdp_example_PPR_non_stationary(M,term,Pj)
P <- PR$P   # Probability transitions P(SxSxAxT)
R <- PR$R   # Reward R(SxAxT)
h <- PR$RT  # terminal Reward R(S)

## Solve the MDP
results_optimal <- mdp_finite_horizon_nonStationary(P, R, 1, time_step, h)
policy_optimal <- results_optimal$policy; head(policy_optimal)


## Explore solution
sim_optimal <- explore_solution_PPR(numeric(init_site), policy_optimal, M, P, R,h)
sim_optimal$Treward
sim_optimal$Tsites

# Run the simulation 1000 times and collect sim$Treward from each run
sim_runs_optimal <- lapply(1:1000, function(i) {
  sim <- explore_solution_PPR(numeric(init_site), policy_optimal, M, P, R, h)
  sim$Treward
})



#########################################################
# BUILD & SOLVE Greedy Algorithm ####
#########################################################

## Solve the greedy algorithm
results_greedy <- mdp_greedy_policy_nonStationary(M, P, R, 1, time_step, h, init_site)
policy_greedy <- results_greedy$policy; head(policy_greedy)


## Explore solution
sim_greedy <- explore_solution_PPR(numeric(init_site), policy_greedy, M, P, R,h)
sim_greedy$Treward
sim_greedy$Tsites

# Run the simulation 1000 times and collect sim$Treward from each run
sim_runs_greedy <- lapply(1:1000, function(i) {
  sim <- explore_solution_PPR(numeric(init_site), policy_greedy, M, P, R, h)
  sim$Treward
})


#########################################################
# BUILD & SOLVE Forward Looking Myopic Model ####
#########################################################

## Solve the forward-looking myopic algorithm
results_fl_myopic <- mdp_myopic_forward_look_policy(P, R, 1, time_step, h)
policy_fl_myopic <- results_fl_myopic$policy; head(policy_fl_myopic)


## Explore solution
sim_fl_myopic <- explore_solution_PPR(numeric(init_site), policy_fl_myopic, M, P, R,h)
sim_fl_myopic$Treward
sim_fl_myopic$Tsites

# Run the simulation 1000 times and collect sim$Treward from each run
sim_runs_fl_myopic <- lapply(1:1000, function(i) {
  sim <- explore_solution_PPR(numeric(init_site), policy_fl_myopic, M, P, R, h)
  sim$Treward
})



########### COMPARE AND STORE RESULTS #########

# --- 1. PROCESS SIMULATION RESULTS ---
# Extract 1000 terminal rewards for each model
terminal_rewards_optimal <- do.call(rbind, sim_runs_optimal)[, ncol(M) + 1]
terminal_rewards_greedy <- do.call(rbind, sim_runs_greedy)[, ncol(M) + 1]
terminal_rewards_fl_myopic <- do.call(rbind, sim_runs_fl_myopic)[, ncol(M) + 1]

# --- 2. PERFORM STATISTICAL TESTS ---
# Two-sample t-test

# Greedy vs. non-stationary MDP
ttest_vs_greedy <- t.test(terminal_rewards_optimal, terminal_rewards_greedy)

# Myopic MDP vs Greedy
ttest_vs_fl_myopic <- t.test(terminal_rewards_optimal, terminal_rewards_fl_myopic)



# --- 2. CREATE AND SAVE THE RAW DATA TABLE ---
#combine all raw results into a single dataframe.
raw_results_table <- bind_rows(
  data.frame(TerminalReward = terminal_rewards_optimal, Model = "optimal"),
  data.frame(TerminalReward = terminal_rewards_greedy,  Model = "greedy"),
  data.frame(TerminalReward = terminal_rewards_fl_myopic, Model = "fl_myopic")
)

# Save into csv w/ dynamic file name
raw_output_filename <- paste0("raw_simulation_results_", benefit_scenario, ".csv")

# Save the raw data to a new CSV file
write.csv(raw_results_table, raw_output_filename, row.names = FALSE)
cat("\nRaw simulation data saved to:", raw_output_filename, "\n")


# --- 3. CREATE AND SAVE THE SUMMARY TABLE ---

summary_stats <- raw_results_table %>%
  group_by(Model) %>%
  summarise(
    mean_r = mean(TerminalReward),
    sd_r = sd(TerminalReward)
  )

# Mean of MDP nonstationary (baseline)
mean_optimal <- summary_stats$mean_r[summary_stats$Model == "optimal"]

# Calculate difference in mean between MDP optimal/greedy/FL Myopic
results_sum_enhanced <- summary_stats %>%
  mutate(
    # Add a column for the difference from the optimal model's mean reward
    DifferenceFromOptimal = round(mean_optimal - mean_r, 2),
    
    # Add a column for the p-value from the t-test
    PValue = case_when(
      Model == "optimal"   ~ NA_real_, # No p-value for comparing to itself
      Model == "greedy"    ~ ttest_vs_greedy$p.value,
      Model == "fl_myopic" ~ ttest_vs_fl_myopic$p.value
    ),
    
    # Format the p-value for clean presentation (e.g., showing "< 0.001")
    Significance = case_when(
      is.na(PValue) ~ "--",
      PValue < 0.001 ~ "< 0.001",
      TRUE ~ as.character(round(PValue, 3))
    ),
    
    # Add the scenario information
    data_scenario = benefit_scenario
  ) %>%
  # Select and reorder columns for the final output
  select(
    Model,
    MeanReward = mean_r,
    StDev = sd_r,
    DifferenceFromOptimal,
    Significance,
    Scenario = data_scenario
  )

# Round the numeric columns for a cleaner final table
results_sum_enhanced <- results_sum_enhanced %>%
  mutate(across(where(is.numeric), ~ round(., 2)))


# --- 5. SAVE AND PRINT THE ENHANCED TABLE ---
# Define the dynamic name for the summary object and file
dynamic_name <- paste0("results_sum_enhanced_", benefit_scenario)

# Assign to a dynamically named object
assign(dynamic_name, results_sum_enhanced)

# Save the enhanced summary to a CSV file
write.csv(results_sum_enhanced, paste0(dynamic_name, ".csv"), row.names = FALSE)
cat("Enhanced summary statistics saved to:", paste0(dynamic_name, ".csv"), "\n")

# Print the contents of the newly created enhanced summary object
cat("\n--- Final Enhanced Summary Table (`", dynamic_name, "`) ---\n", sep="")
print(get(dynamic_name))
cat("----------------------------------------------------------\n")








