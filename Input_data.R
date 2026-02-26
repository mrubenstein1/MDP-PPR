# --- BENEFITS: PARCEL VALUES OVER TIME ---

# This script reads a variable 'benefit_scenario' from the main script
# to determine which benefit matrix (M) to generate. This version defines
# all time step values at once, according to scenarios, then separates time steps 1:5 from the
# terminal value (term). 
    #This helps to align with structure of explore_solution_PPR & other scripts.

# Common parameters for both scenarios
init_site <- 6
time_step <- 5 # Main simulation horizon
full_horizon <- time_step + 1 # Total steps including terminal

##################################################
#      DEFINE REWARD & TRANSITION MATRIX         #
#################################################

if (!exists("benefit_scenario")) {
  stop("ERROR: The 'benefit_scenario' variable is not set in the main script.")
}

if (benefit_scenario == "constant") {
  
  # --- Scenario 1: CONSTANT BENEFIT (Reproducible Random Workflow) ---
  set.seed(42)
  
  # Generate a reproducible random starting value for each of the 6 sites.
  initial_values <- sample(1:10, size = init_site, replace = TRUE)
  
  # Create the full benefit matrix by repeating each site's initial value
  benefit_list <- lapply(initial_values, function(val) {
    rep(val, full_horizon)
  })
  
  # Combine the list of vectors into a single matrix.
  dataR_full <- do.call(rbind, benefit_list)
  
} else if (benefit_scenario == "variable") {
  
  # --- Scenario 2: VARIABLE BENEFIT ---
  set.seed(42)
  time_points <- 1:full_horizon
  
  #P1: starts high, declines
  p1_start <- sample(15:22, 1)
  p1 <- round(p1_start * c(1, 0.25, 0.1, 0.05, 0, 0))
  
  #P2: Starts low and grows over time (quadratic growth curve).
  p2 <- round(0.8 * (time_points - 1)^2 + time_points)
  
  #P3: Stable, mid-range value
  p3_val <- sample(3:5, 1)
  p3 <- rep(p3_val, full_horizon)
  
  round(p3_val + sin(time_points))
  
  #P4: Rises slowly then descends
  p4 <- round(-0.6 * (time_points - 3.5)^2 + 8)
  
  #P5: Steady decline (linear decay)
  p5_start <- sample(8:12, 1)
  p5 <- p5_start - (0:(full_horizon-1))
  
  #P6: Stable, low-range value
  p6_val <- sample(2:4, 1)
  p6 <- rep(p6_val, full_horizon)
  
  # Combine the programmatically generated vectors into a matrix.
  dataR_full <- rbind(p1, p2, p3, p4, p5, p6); dataR_full
  
} else {
  stop(paste("Invalid 'benefit_scenario' specified:", benefit_scenario,
             '. Please choose "constant" or "variable".'))
}


# --- Common Code to Split and Finalize Matrices ---

# 1. The main benefit matrix 'M' uses only the first 5 time steps
M <- dataR_full[, 1:time_step]

# 2. The terminal benefit 'term' is the value from the 6th time step
dataRT <- dataR_full[, full_horizon]
term <- matrix(dataRT, nrow = init_site, ncol = 1)


# --- CONVERSION PROBABILITY (Pj) ---
# Kept constant for this example
Pj <- array(rep(0.1, init_site * time_step), c(init_site, time_step))