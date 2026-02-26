mdp_finite_horizon_nonStationary <- function(P, R, discount, N, h) {
# only works for # R(SxAxT)
  
  
start<-as.POSIXlt(Sys.time())

# VERBOSE STUFF

# check of arguments
if (N < 1) {
	print('--------------------------------------------------------')
	print('MDP Toolbox ERROR: N must be upper than 0')
	print('--------------------------------------------------------')
} else if (discount <= 0 | discount > 1) {
	print('--------------------------------------------------------')
	print('MDP Toolbox ERROR: Discount rate must be in ]0; 1]')
	print('--------------------------------------------------------')
} else {
	# Beware global scope!
	if (is.list(P)) { # is P a list?
		S <- dim(P[[1]])[1] #  If P is a list, then P[[1]] accesses the first element of the list (which should be a matrix), and dim(P[[1]])[1] gets the number of rows in this matrix. 
		A <- length(P) # length of the list P, which is the number of actions in the MDP, and assigns it to A.
		
		print("Sorry cant deal with list just yet")
		
	} else {
		S <- dim(P)[1]
		A <- dim(P)[3]
	}
	V <- matrix(0,S,N+1) # checked done
	policy <- matrix(0,S,N)
	
	if (nargs() == 5) {
		V[,N+1] <- h
	}
	
	for (n in 0:(N-1)) {
	  t1 <-N-n
	  PRt <- mdp_computePR(P[,,,t1],R[,,t1]) # R(SxAxT)
	  Pt <- P[,,,t1] # to check
		bellman <- mdp_bellman_operator(Pt,PRt,discount,V[,N-n+1]) # check
		W <- bellman[[1]]
		X <- bellman[[2]]
		V[,N-n] <- W
		policy[,N-n] <- X
	}
}

end <-as.POSIXlt(Sys.time())

return(list("V"=V, "policy"=policy, "time" = end-start))

}
