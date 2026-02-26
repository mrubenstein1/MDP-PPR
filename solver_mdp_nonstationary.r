# function [P, R] = mdp_example_PPR_non_stationary (M, pj)


getprobafast <- function(aux, availSite, Pj, t1) {
  # Vectorized computation of joint probability
  p <- prod(ifelse(aux == 0, 1 - Pj[availSite, t1], Pj[availSite, t1]))
  return(p)
}

mdp_example_PPR_non_stationary <- function(M,term, Pj=NULL) {
  
  # mdp_example_PPR_non_stationary
  #                       Generate a Markov Decision Process PPR with unit cost of 1 
  #                       example based on
  #                       a simple reserve design problem
  #                       (see the related documentation for more detail)
  # Arguments -------------------------------------------------------------
  #   M(JxH) = benefit function across sites and time
  #        J = number of sites (> 0), optional (default 4)
  #        H = number of time step (>0), optional (default 4)
  #   Pj(JXH) = probability of development occurence, in ]0, 1[, optional (default 0.1)
  # Evaluation -------------------------------------------------------------
  #   P(SxSxAxH) = transition probability matrix
  #   R(SxAxH) = reward matrix
  #   
  
  if ( nargs() >= 1 & dim(M)[1] <= 1 & dim(M)[2] <= 1 ) {
    print('----------------------------------------------------------')
    print('MDP Toolbox ERROR: M is a JxH matrix with Number of sites J and H time 
	      horizon must be greater than 0')
    print('----------------------------------------------------------')
  } else if (nargs() >= 2 & any(Pj < 0 | Pj > 1)) {
    print('-----------------------------------------------------------')
    print('MDP Toolbox ERROR: elements of probability matrix Pj must be in [0; 1]')
    print('-----------------------------------------------------------')
  } else {
    # initialization of optional arguments
    #	if (nargs() < 2 & nargs()>= 1) {
    if (is.null(Pj)) {
      J <- nrow(M)
      H <- ncol(M)
      Pj <- round(array(runif(J*H, min=0, max=0.4), c(J,H))*100)/100
    }
    if (nargs() < 1) {
      J <- 3
      H <- 3
      M <- round(matrix(nrow=J, ncol=H, data=runif(J*H,0,10)))
      term <- round(matrix(runif(init_site, 0, 10), nrow = J, ncol = 1))
      
      Pj <- round(array(runif(J*H, min=0, max=0.4), c(J,H))*100)/100
    }
    
    J <- nrow(M)
    H <- ncol(M)
    
    # Definition of states
    # each site, 0 is available; 1 is reserved; 2 is developed
    S <- 3^J
    # action space
    A <- J
    
    # There are J actions corresponding to the selection of a site for
    # reservation. A site can only be reserved if it is available.
    # By convention we will use a ternary base where state #0 is the state
    # that corresponds to [0,0, ..,0] all sites are available. State #1 is
    # [1,0,0,...,0]; state 2 is [2,0,0 ..,0] and state 3 is [0,1,0, .. 0] and so forth.
    # for example 
    # site = [0,0,1,2] means the first 2 sites are available (site[1:2]=0), site 3 is
    # reserved (site[3]=1) and site 4 is developped (site[4]=2).
    
    # Build P(AxSxSxT)
    # complexity is in SxAx2^navail; with 2^navail<=S
    
    P <- array(0, c(S,S,A,H))
    
    for (t1 in 1:H){
      for (s1 in 1:S) {
        site1 <- getSite(s1-1, J)
        for (a in 1:A) {
          site2 <- site1
          if (site1[a] == 0) { # then it can be purchased
            site2[a] <- 1
          }
          # Can the site2 also transition to developed?
          availSite <- which(site2 == 0) # availSite contains id of sites available.
          # this is wrong ... needs to be updated
          
          if (length(availSite) > 0) { # if some sites are available
            navail <- length(availSite)
            siten <- rep(1,2^navail) %*% t(site2)
            aux <- numeric(navail)
            for (k in 1:2^navail) {
              siten[k,availSite] <- aux*2 # selected site turn developed (2)
              p2 <- getprobafast(aux,availSite,Pj,t1) # joint proba given aux and id availSite
              s2 <- getState(siten[k,])+1 # next state
              P[s1,s2,a,t1] <- p2
              aux <- dec2binvec(binvec2dec(aux)+1, navail) # next sites
            }
          } else {
            s2 <- getState(site2)+1
            P[s1,s2,a,t1] <- 1
          }
        }
      }
    }
    # Build R
    
    R <- array(0, c(S,A,H))
    for (t1 in 1:H) {
      for (s1 in 1:S) { # for all states
        site1 <- getSite(s1-1,J) # get the sites configuration i.e. # -> [0,1,0,2]
        # Vectorized computation of sumb
        sumb <- sum(M[site1 != 2, t1])
        R[s1,,t1] <- array(sumb,c(1,A)) # same benefit for all actions when purchased or avail
        }
    }
    print(R)

    RT <- numeric(S) # terminal reward
      for (s1 in 1:S) { # for all states
        site1 <- getSite(s1-1,J) # get the sites configuration i.e. # -> [0,1,0,2]
        # Vectorized computation of sumb
        sumb <- sum(term[site1 != 2])
        RT[s1] <- sumb 
      }
    print(RT)
    return(list("P"=P, "R"=R, "RT"=RT))
  }
}

