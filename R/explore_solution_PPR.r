explore_solution_PPR <- function(init_site, policy, M, P, R,h) {
	
	J <- nrow(M) # number of sites
	H <- ncol(M) # timestep
	
	S <- 3^J
	
	horizon <- H+1
	action <- numeric(horizon)
	Tsites <- matrix(0, nrow=J, ncol=horizon)
	Treward <- numeric(horizon)
	
	current_site <- init_site
	
	for (i in 1:(horizon-1)) {
		state <- getState(current_site)+1
		# Tstates[i,] <- state
		Tsites[,i] <- current_site
		action[i] <- policy[state,i]
		
		if (i != 1 ) {
			Treward[i] <- R[state, action[i],i] + Treward[i-1]
		} else {
		  Treward[i] <- R[state, action[i],i]
		}
		
		p_state_new <- runif(1)
		p <- 0
		state_new <- 0
		while(p < p_state_new & state_new < S) {
			state_new <- state_new + 1
			p <- p + P[state, state_new, action[i],i]
		}
		
		current_site <- getSite(state_new-1, J)
	}
	# terminal state
	state <- getState(current_site)+1
	# Tstates[i,] <- state
	Tsites[,horizon] <- current_site
	Treward[horizon] <- h[state]+ Treward[horizon-1] # terminal reward
	
	
	#par(mfrow=c(2,1))
	par(mfrow=c(2,1), mar = c(4, 4, 2, 2))
	
	breaks <- 0:3  # One more break than colors
	colors <- c("white", "grey", "black")
	# Create a numeric matrix of color indices
	Tsites_indices <- Tsites+1
	# Use the numeric matrix and color vector in the image function
	image(t(Tsites_indices), axes = FALSE,xlab="Time horizon", ylab="Sites", breaks = breaks, col=colors)
	box()
	title(main="white=Available, grey=Purchased, black=Converted", font.main=1)
	
	plot(1:horizon, Treward,type="s", xlab="Time horizon", ylab="Sum of benefits")
	
	return(list("Treward"=Treward, "Tsites"=Tsites))
}