# Filename: mdp_myopic_forward_look_policy.R
#
# Computes a "forward-looking" myopic policy for a non-stationary MDP.

# This policy simulates a decision-maker who, at each time step 't', attempts to
# plan for the future (from t+1 to H). However, it makes a critical myopic assumption:
# it believes that the transition probabilities P[,,,t] and reward structure R[,,t]
# from the current time step 't' will remain stationary for all future time steps.
#
# The agent performs the following steps at each time 't':
# 1. Observes the current P and R matrices.
# 2. Solves a new, stationary, finite-horizon MDP for the remaining time steps (t+1 to H)
#    using the values of the current P and R matrices for all future time steps
# 3. It uses this projected future value to calculate the best action for the current
#    time step 't'.
# 4. It takes that action, time moves forward, and the true P and R for t+1 are revealed
#
# INPUTS:
# P - The TRUE non-stationary transition probability array [S x S x A x H].
# R - The TRUE non-stationary reward array [S x A x H].
# discount - Discount factor.
# H - The planning horizon (number of time steps).
# h - A vector of terminal rewards for each state [S].
#
# OUTPUTS:
# A list containing:
# policy - An [S x H] matrix where policy[s, t] is the action for state s at time t.
# V      - An [S x (H+1)] "value" matrix, where V[s,t] is the calculated value.

mdp_myopic_forward_look_policy <- function(P, R, discount, H, h) {
  
  # Infer the number of states and actions from dimensions
  S <- dim(R)[1]
  A <- dim(R)[2]
  
  # Initialize policy and value matrices
  policy <- matrix(0, nrow = S, ncol = H)
  V <- matrix(0, nrow = S, ncol = H + 1)
  V[, H + 1] <- h
  
  # --- CORE LOGIC ---
  # Loop backwards through REAL time (from H to 1)
  for (i in H:1) {
    
    # --- Myopic Forward-Look Step ---
    # The agent at time 'i' assumes the world is stationary, based on
    # the conditions at 'i'. We now calculate the expected future value function
    # under this flawed assumption.
    P_stationary <- P[,,,i]
    R_stationary <- R[,,i]
    
    # V_future_proj represents the value-to-go from the NEXT step (i+1) onwards,
    # as projected by the myopic agent at time 'i'.
    # Initialize with the known terminal rewards.
    V_future_proj <- h
    
    # If we are not at the final time step, calculate the projected values
    # by iterating backwards from H to i+1.
    if (i < H) {
      for (j in H:(i + 1)) {
        # This is a standard Bellman update, but applied using the stationary P and R.
        q_values_proj <- array(0, dim = c(S, A))
        for (s_proj in 1:S) {
          for (a_proj in 1:A) {
            # Expected value = immediate reward + discounted future value
            q_values_proj[s_proj, a_proj] <- R_stationary[s_proj, a_proj] +
              discount * sum(P_stationary[s_proj, , a_proj] * V_future_proj)
          }
        }
        # The value of a state is the value of the best action from that state.
        V_future_proj <- apply(q_values_proj, 1, max)
      }
    }
    # At the end of this loop, V_future_proj holds the agent's calculated value
    # of being in any state at time i+1.
    
    # --- Decision Step ---
    # Now, use this projected future value to make the actual decision for time 'i'.
    for (s in 1:S) {
      q_values_real <- numeric(A)
      for (a in 1:A) {
        # The agent uses the true P and R for the current step 'i', but its
        # expectation about the future value comes from the myopic projection.
        q_values_real[a] <- R[s, a, i] + discount * sum(P[s, , a, i] * V_future_proj)
      }
      
      best_action <- which.max(q_values_real)
      policy[s, i] <- best_action
      V[s, i] <- max(q_values_real)
    }
  }
  
  return(list("policy" = policy, "V" = V))
}