
# Computes a true greedy policy for the PPR problem.
# This policy ignores the R matrix and instead selects the available action (parcel)
# that has the highest individual benefit M[a, t] at each state and time.
#
# INPUTS:
# M      - The Benefit array [J x H]. This is the information the greedy agent uses.
# P      - UNSUSED but included for compatibility.
# R      - UNSUSED but included for compatibility.
# discount - UNSUSED but included for compatibility.
# H      - The planning horizon (number of time steps).
# h      - UNSUSED but included for compatibility.
# J      - The number of sites.
#
# OUTPUTS:
# A list containing:
# policy - An [S x H] matrix where policy[s, t] is the greedy action for state s at time t.
# V      - A placeholder matrix for compatibility.

mdp_greedy_policy_nonStationary <- function(M, P, R, discount, H, h, J) {
  
  S <- 3^J
  
  # Initialize a policy matrix
  policy <- matrix(0, nrow = S, ncol = H)
  
  # Loop through every time step
  for (i in 1:H) {
    # And every possible state...
    for (s in 1:S) {
      
      # --- CORE GREEDY LOGIC ---
      site_config <- getSite(s - 1, J)
      
      # 1. Find which sites are available (state == 0)
      available_sites <- which(site_config == 0)
      
      if (length(available_sites) > 0) {
        # 2. If sites are available, get their benefits at the current time step
        benefits_of_available <- M[available_sites, i]
        
        # 3. Find the index of the best site WITHIN THE SUBSET of available sites
        best_local_index <- which.max(benefits_of_available)
        
        # 4. Map this local index back to the true parcel number
        best_action <- available_sites[best_local_index]
        
        policy[s, i] <- best_action
        
      } else {
        # 5. If no sites are available, the action doesn't matter. Default to 1.
        policy[s, i] <- 1
      }
    }
  }
  
  # Return the policy and a placeholder Value matrix for compatibility
  return(list("policy" = policy, "V" = matrix(0, nrow=S, ncol=H+1)))
}