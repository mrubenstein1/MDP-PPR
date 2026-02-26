# Filename: mdp_myopic_policy_nonStationary.r
#
# Computes a myopic (greedy) policy for a non-stationary MDP.
# This policy selects the action that maximizes the immediate reward at each state and time,
# completely ignoring transition probabilities and future rewards.
#
# This function is designed to be a drop-in replacement for mdp_finite_horizon_nonStationary
# to allow for direct comparison between an optimal and a myopic policy.
#
# INPUTS:
# P - Transition probability array [S x S x A x H]. Note: This is UNSUSED but included for compatibility.
# R - Reward array [S x A x H]. This is the only information the myopic agent uses.
# discount - Discount factor. Note: This is UNUSED but included for compatibility.
# H - The planning horizon (number of time steps).
# h - A vector of terminal rewards for each state [S].
#
# OUTPUTS:
# A list containing:
# policy - An [S x H] matrix where policy[s, t] is the myopic action for state s at time t.
# V      - An [S x (H+1)] "value" matrix. For the myopic policy, V[s, t] is simply the
#          immediate reward R[s, policy[s,t], t]. V at H+1 is the terminal reward h.

mdp_myopic_policy_nonStationary <- function(P, R, discount, H, h) {
  
  # Infer the number of states and actions from the dimensions of the Reward array
  S <- dim(R)[1]
  A <- dim(R)[2]
  
  # Initialize a policy matrix to store the best action for each state and time
  policy <- matrix(0, nrow = S, ncol = H)
  
  # Initialize a "value" matrix for consistency with the MDP solver output.
  V <- matrix(0, nrow = S, ncol = H + 1)
  
  # The "value" at the terminal time step is the terminal reward.
  V[, H + 1] <- h
  
  # Loop backwards through time (from H to 1) for structural consistency.
  # For a myopic policy, the direction doesn't functionally matter.
  for (i in H:1) {
    
    # For each state...
    for (s in 1:S) {
      
      # --- CORE MYOPIC LOGIC ---
      # Find the action (index) that yields the maximum immediate reward R
      # for the current state 's' and time 'i'.
      # The transition probabilities 'P' and future values are deliberately ignored.
      best_action <- which.max(R[s, , i])
      
      # Store this greedy action in our policy table.
      policy[s, i] <- best_action
      
      # For output consistency, the "value" of this state-time under a myopic policy
      # is just the immediate reward we get from our chosen action.
      V[s, i] <- R[s, best_action, i]
    }
  }
  
  return(list("policy" = policy, "V" = V))
}
