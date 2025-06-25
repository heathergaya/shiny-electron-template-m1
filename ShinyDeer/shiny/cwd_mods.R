### Functions and initial values for CWD Shiny App
### For raster:
#cwd_geojson <- geojson_json(st_read("latlon.geojson"))
#geoj <- fromJSON(cwd_geojson)
#lat_lon_tab <- dget("latlontab.txt")
### Some pre-application objects
init_phi_a <- matrix(c(0.27, 0.880, 0.916,
                       0.0000000, 0.541, 0.358,
                       0.27, 0.838, 0.88,
                       0.0000000, 0.528, 0.257), nrow = 2, byrow = T)
colnames(init_phi_a) <- c("- Fawns", "- Yearlings", "- Adults", 
                          "+ Fawns", "+ Yearlings", "+ Adults")
rownames(init_phi_a) <- c("F", "M")

init_sigma_a <- matrix(c(759.6951, 313.2917, 629.7136,
                         388.8458, 160.3027, 319.3263, 
                         6674.0513, 2588.2623, 5357.5420,
                         3487.4181, 1984.3192, 3726.1874), nrow = 2, byrow = T)
colnames(init_sigma_a) <- c("- Fawns", "- Yearlings", "- Adults", 
                            "+ Fawns", "+ Yearlings", "+ Adults")
rownames(init_sigma_a) <- c("F", "M")


### Create functions now, so they're available for app to use ####

## infection
rmm_infect <- function(n,
                 nn,
                 phi,
                 eta0,
                 nState,
                 sex,
                 PastPos,
                 pos_friends
  ) {
    nState <- nState/2
    #hazard <- exp(eta0 + eta_friends*contacts)
    #eta_d <- 1 - exp(-(hazard*pos_friends+.05)) #6.178 = sq miles per pixel
    #hazard <- exp(eta0)
    #eta_d <- 1-exp(-(hazard*(.75*pos_friends + .25*PastPos)))
    eta_d <- .25*(1-exp(-.974*(.75*pos_friends + .25*PastPos)))
    #eff_intercept <- (-2*(1-PastPos))+(-1.48*PastPos)
    #eta_d <- plogis(eff_intercept + 1.0014*pos_friends/(1-.72+.72*TotDens))
    psi_live <- array(0, c(nState, nState+1))
    
    if(sex == 1){ #female
      
      psi_live[1,2] <- (1-eta_d)*phi[1]
      psi_live[1,5] <- eta_d*phi[1]
      psi_live[2,3]  <- (1-eta_d)*phi[2]
      psi_live[2,6]  <- eta_d*phi[2]
      psi_live[3,3]  <- (1-eta_d)*phi[3]
      psi_live[3,6]  <- eta_d*phi[3]
      psi_live[5,6] <- phi[5]
      psi_live[6,6] <- phi[6]
      
      for(ss in 1:6){
        psi_live[ss,7] <- 1-phi[ss]
      }
    } #end female
    
    if(sex == 0){ #male
      psi_live[1,2] <- (1-eta_d)*phi[7]
      psi_live[1,5] <- eta_d*phi[7]
      psi_live[2,3]  <- (1-eta_d)*phi[8]
      psi_live[2,6]  <- eta_d*phi[8]
      psi_live[3,3]  <- (1-eta_d)*phi[9]
      psi_live[3,6]  <- eta_d*phi[9]
      psi_live[5,6] <- phi[11]
      psi_live[6,6] <- phi[12]
      
      for(ss in 1:6){ #male phis are after all the female ones
        psi_live[ss,7] <- 1-phi[ss+6]
      } 
    } #end male
    
    ans1 <- array(0, dim = c(nState, nState + 1))
    ans <- array(0, dim = nState)
    for(ss in 1:6){
      ans1[ss,] <- rmultinom(1, size = nn[ss], prob = psi_live[ss,1:7])
    }
    for(ss in 1:6){
      ans[ss] <- sum(ans1[1:nState,ss])
    }
    
    return(ans[1:nState])
    
  }

# ### Tab 1, non-spatial female model ####
# superbasic <- function(phi = init_phi, #survival
#                        gamma = init_gam, #reproduction, as in p(success reproduced) not fecundity
#                        n0 = init_pop, #initial population
#                        eta = init_eta, #transmission
#                        nocc = 20, #time steps
#                        q = .5, #fawn sex ratio
#                        H = init_H, #constant hunter harvest
#                        posfawns = FALSE #can fawns be positive
# ){
#   nStates <- length(n0)
#   n <- allfawns <- matrix(NA_integer_, nStates, nocc)
#   n[,1] <- n0
#   N <- Totneg <- Totpos <- lambda_eff <- integer(nocc)
#   N[1] <- sum(n[,1])
#   negs <- rep(c(rep(0, 3), rep(1,3)), 2)
#   Totneg[1] <- n[,1] %*% (1-negs)
#   Totpos[1] <- n[,1] %*% negs
#   #to make it easier for indexing, flip phi/gamma/eta matrices:
#   phi <- t(phi)
#   gamma <- t(gamma)
#   eta <- t(eta)
#   ### This is "post-repro census" formulation; Fecundity=AgeClassSurvival*BirthRate
#   Tmat <- matrix(c(q*phi*gamma, #new f fawns
#                    phi[1]*(1-eta[1]), rep(0, 11), #F Neg Y
#                    0, phi[2]*(1-eta[2]), phi[3]*(1-eta[3]), rep(0, 9), #F neg adult
#                    rep(0, 12), #new pos F fawns
#                    phi[1]*(eta[1]), rep(0, 11), #F pos Y; Assuming fawns can't be positive
#                    0, phi[2]*(eta[2]), phi[3]*(eta[3]), 0, phi[5], phi[6], rep(0, 6), #F pos adult
#                    (1-q)*phi*gamma, #new m fawns
#                    rep(0, 6), phi[7]*(1-eta[7]), rep(0, 5), #m neg y
#                    rep(0, 7), phi[8]*(1-eta[8]), phi[9]*(1-eta[9]), rep(0, 3), #m neg adult
#                    rep(0, 12), #new pos M fawns
#                    rep(0, 6), phi[7]*(eta[7]), rep(0, 5), #M pos Y
#                    rep(0, 7), phi[8]*(eta[8]), phi[9]*(eta[9]), 0, phi[11], phi[12] #M pos adult
#   ),
#   ncol = 12, byrow = T)
#   if(posfawns){
#     Tmat[1,c(5,6)] <- 0 #pos moms can't have neg female fawns
#     Tmat[4,c(5,6)] <- q*phi[5:6]*gamma[5:6] #pos female fawns
#     Tmat[5,4] <- phi[4] #pos Y can come from pos Fawns
#     Tmat[7,c(5,6)] <- 0 #pos moms can't have neg male fawns
#     Tmat[10,c(5,6)] <- (1-q)*phi[5:6]*gamma[5:6] #pos male fawns
#     Tmat[11,10] <- phi[10] #pos Y can come from pos Fawns
#   }
#   
#   for(t in 2:nocc){
#     post_h <- (n[,t-1] - c(H))
#     post_h[post_h <0] <- 0
#     n[,t] <- Tmat %*% (post_h) #this is the N(t+1) = H*A(t) approach 
#     N[t] <- sum(n[,t])
#     Totneg[t] <- n[,t] %*% negs
#     Totpos[t] <- n[,t] %*% (1-negs)
#     lambda_eff[t] <- N[t]/N[t-1]
#     
#   }# end t
#   
#   ### Outputs 
#   ea <- eigen(Tmat)
#   lambda <- Re(ea$values)[1]        ## Growth rate
#   u0 <- Re(ea$vectors[,1])
#   u <- u0/sum(u0)                  ## Stable age distribution
#   #managers might not care to have these split by status; could condense to sex/age
#   u2 <- c(u[1:3] + u[4:6], u[7:9] + u[10:12])
#   
#   v0 <- Re(eigen(t(Tmat))$vectors[,1])
#   v <- v0/v0[1]                     ## Standardized reproductive value
#   
#   ### sensitivity
#   vs <- v%*%t(u0) #standard repro values * inverse stable age 
#   Sens <- vs/as.numeric(v%*%u0) 
#   #or could use popbio package:
#   #sensitivity(Tmat[1:6,1:6])
# 
#   #sensitivity is interesting, but we might not want to display impossible transitions (adults getting younger, etc) 
#   Sens_pos <- Sens
#   Sens_pos[Tmat == 0] <- NA
#   
#   ### elasticity
#   Elas <- (Tmat/lambda)*Sens
#   #elasticity(Tmat[1:6,1:6])
#   E_fec <- sum(Elas[1,]) #fecundity
#   E_fawn_s <- sum(Elas[,1]) #fawn survival
#   E_y_s <- sum(c(Elas[3,2], Elas[6,c(2,5)])) #yearling surv, either from neg y -> neg A, neg y -> pos A, or pos y -> pos A
#   E_a_s <- sum(c(Elas[3,3], Elas[6,c(3,6)])) #adult survival
#   E_props <- c(E_fec, E_fawn_s, E_y_s, E_a_s)
#   
#   ### Effects on lambda from perturbations for vital rates might also be interesting. 
#   #What if phi or eta were a little different?
#   ## Since this is female based, males don't affect lambda, so can skip them
#   templam <- array(NA, c(9, 3))
#   vals <- c(.95, 1.05, 1.1) #5% drop, 5% increase, 10% increase
#   for(x in 1:3){ #low, high and higher perturbation
#     for (y in 1:6) { #phis
#       temp_phi <- phi[1:6]
#       temp_phi[y] <- phi[y]*vals[x]
#       Tempmat <- matrix(c(q*temp_phi*gamma[1:6], #new f fawns
#                    temp_phi[1]*(1-eta[1]), rep(0, 5), #F Neg Y
#                    0, temp_phi[2]*(1-eta[2]), temp_phi[3]*(1-eta[3]), rep(0, 3), #F neg adult
#                    rep(0, 6), #new pos F fawns
#                    temp_phi[1]*(eta[1]), rep(0, 5), #F pos Y; Assuming fawns can't be positive
#                    0, temp_phi[2]*(eta[2]), temp_phi[3]*(eta[3]), 0, temp_phi[5], temp_phi[6] #F pos adult
#       ),
#     ncol = 6, byrow = T)
#       if(posfawns){
#         Tempmat[1,c(5,6)] <- 0 #pos moms can't have neg female fawns
#         Tempmat[4,c(5,6)] <- q*temp_phi[5:6]*gamma[5:6] #pos female fawns
#         Tempmat[5,4] <- temp_phi[4] #pos Y can come from pos Fawns
#       }
#     templam[y,x] <- Re(eigen(Tempmat)$values)[1]-lambda 
#     }
#     for (y in 1:3) { #eta
#       temp_eta <- eta[1:6]
#       temp_eta[y] <- eta[y]*vals[x]
#       Tempmat <- matrix(c(q*phi[1:6]*gamma[1:6], #new f fawns
#                           phi[1]*(1-temp_eta[1]), rep(0, 5), #F Neg Y
#                           0, phi[2]*(1-temp_eta[2]), phi[3]*(1-temp_eta[3]), rep(0, 3), #F neg adult
#                           rep(0, 6), #new pos F fawns
#                           phi[1]*(temp_eta[1]), rep(0, 5), #F pos Y; Assuming fawns can't be positive
#                           0, phi[2]*(temp_eta[2]), phi[3]*(temp_eta[3]), 0, phi[5], phi[6] #F pos adult
#       ),
#       ncol = 6, byrow = T)
#       if(posfawns){
#         Tempmat[1,c(5,6)] <- 0 #pos moms can't have neg female fawns
#         Tempmat[4,c(5,6)] <- q*phi[5:6]*gamma[5:6] #pos female fawns
#         Tempmat[5,4] <- phi[4] #pos Y can come from pos Fawns
#       }
#       templam[(y+6),x] <- Re(eigen(Tempmat)$values)[1]-lambda 
#     }
#   }
#   
#   
#   
#   Totpop <- data.frame(t = 1:nocc,
#                        N = c(N, Totneg, Totpos),
#                        group = rep(c("All", "CWD Negative", "CWD Positive"), each = nocc))
#   agepop <- data.frame(t = rep(1:nocc, each =12),
#                        N = c(n),
#                        group = rep(c("F- Fawns", "F- Yearlings", "F- Adults", 
#                                      "F+ Fawns", "F+ Yearlings", "F+ Adults",
#                                      "M- Fawns", "M- Yearlings", "M- Adults", 
#                                      "M+ Fawns", "M+ Yearlings", "M+ Adults"), nocc))
#   
#   Lambda_calcs <- data.frame(t = rep(2:nocc, 2),
#                              lambda = c(rep(lambda, (nocc-1)), N[2:nocc]/N[1:(nocc-1)]),
#                              Group = rep(c("No Harvest", "With Harvest"), each = nocc-1))
#   return(list(n=n, N=N, Totpop = Totpop, Tmat = Tmat, agepop = agepop, 
#               lambda = lambda, stableage = u, stableage2 = u2, reprovalue = v,
#               Sens = Sens, Elas = Elas, Sens_pos = Sens_pos, E_props = E_props,
#               templam=templam, lambda_calcs = Lambda_calcs))
# }
# 
# ### Tab 2, non-spatial both sex model ####
# basic_etad_2sex <- function(phi = init_phi, #survival
#                             n0 = init_pop, #initial population
#                             eta = -2, #transmission intercept 
#                             alpha = 0, #d-dep in transmission
#                             eta_s = 0, #sex-based transmission beta
#                             eta_a = c(0,0,0), #age based transmission beta
#                             nocc =20, #time steps
#                             q, #fawn sex ratio
#                             H = init_H,
#                             posfawns = FALSE
#                             
# ){
#   nStates <- length(n0)
#   n <- allfawns <- gamma <-eta_d <- matrix(NA_integer_, nStates, nocc)
#   sex <- rep(c(1,0), each =6) #1 = F, 0 = male
#   age <- rep(1:3, 4) #ages of stages
#   n[,1] <- n0
#   N <- Totneg <- Totpos <- nf <- nm <- integer(nocc)
#   N[1] <- sum(n[,1])
#   negs <- rep(c(rep(0, 3), rep(1,3)), 2)
#   Totneg[1] <- n[,1] %*% negs
#   Totpos[1] <- n[,1] %*% (1-negs)
#   #to make it easier for indexing, flip phi/gamma/eta matrices:
#   phi <- t(phi)
#   Tmat <- array(NA, c(nStates, nStates, nocc))
#   
#   #polygyny
#   ho <- 6 #harem size
#   ky <- 1 #litter size yF
#   ko <- 2 #litter size aF
#   for(t in 2:nocc){
#     print(paste("Calculating year", t, "of", nocc))
#     
#     #does it makes sense that this is based off last year's pop? 
#     #probably, b/c the fawns of t are born from (t-1)'s adults/yearlings
#     nf[t-1] <- sum(n[c(2,3,5,6),t-1])
#     nm[t-1] <- sum(n[c(8,9,11,12),t-1])
#     
#     AF <- min(.5*ko, ko*nm[t-1]/(nm[t-1]+nf[t-1]/ho)) #adult female
#     YF <- min(.5*ky, ky*nm[t-1]/(nm[t-1]+nf[t-1]/ho)) #yearling female
#     YM <- AM <- min(.5*(ky*(n[2,t-1]+n[5,t-1]) + ko*(n[3,t-1]+n[6,t-1]))/nm[t-1], 
#               (ky*(n[2,t-1]+n[5,t-1]) + ko*(n[3,t-1]+n[6,t-1]))/(nf[t-1]/ho + nm[t-1])) #each male can't be responsible for more than half the babies
#     gamma[,t-1] <- c(0, YF, AF, 0, YF, AF, 0, YM, AM, 0, YM, AM)
#     eta_d[,t-1] <- plogis(eta + alpha*(sum(n[,t-1] %*% negs)) + eta_s*sex + eta_a[age]) #transmission depends on number of infected, age, and sex
#     Tmat[,,t-1] <- matrix(c(q*phi*gamma[,t-1], #new f fawns
#                             phi[1]*(1-eta_d[1,t-1]), rep(0, 11), #F Neg Y
#                             0, phi[2]*(1-eta_d[2,t-1]), phi[3]*(1-eta_d[3,t-1]), rep(0, 9), #F neg adult
#                             rep(0, 12), #new pos F fawns
#                             phi[1]*(eta_d[1,t-1]), rep(0, 11), #F pos Y; Assuming fawns can't be positive
#                             0, phi[2]*(eta_d[2,t-1]), phi[3]*(eta_d[3,t-1]), 0, phi[5], phi[6], rep(0, 6), #F pos adult
#                             (1-q)*phi*gamma[,t-1], #new m fawns
#                             rep(0, 6), phi[7]*(1-eta_d[7,t-1]), rep(0, 5), #m neg y
#                             rep(0, 7), phi[8]*(1-eta_d[8,t-1]), phi[9]*(1-eta_d[9,t-1]), rep(0, 3), #m neg adult
#                             rep(0, 12), #new pos M fawns
#                             rep(0, 6), phi[7]*(eta_d[7,t-1]), rep(0, 5), #M pos Y
#                             rep(0, 7), phi[8]*(eta_d[8,t-1]), phi[9]*(eta_d[9,t-1]), 0, phi[11], phi[12] #M pos adult
#     ),
#     ncol = 12, byrow = T)
#     if(posfawns){
#       Tmat[1,c(5,6),t-1] <- 0 #pos moms can't have neg female fawns
#       Tmat[1,c(9,12),t-1] <- (q*phi[c(9,12)]*gamma[c(9,12),t-1])*(sum(n[c(2,3),t-1])/nf[t-1]) #only some are neg from fathers
#       Tmat[4,c(5,6),t-1] <- q*phi[5:6]*gamma[5:6,t-1] #pos female fawns
#       Tmat[4,c(9,12),t-1] <- (q*phi[c(9,12)]*gamma[c(9,12),t-1])*(sum(n[c(5,6),t-1])/nf[t-1]) #only some are pos from fathers
#       Tmat[5,4,t-1] <- phi[4] #pos Y can come also come from pos Fawns
#       Tmat[7,c(5,6),t-1] <- 0 #pos moms can't have neg male fawns
#       Tmat[7,c(9,12),t-1] <- ((1-q)*phi[c(9,12)]*gamma[c(9,12),t-1])*(sum(n[c(2,3),t-1])/nf[t-1]) #only some are neg from fathers
#       Tmat[10,c(5,6),t-1] <- (1-q)*phi[5:6]*gamma[5:6,t-1] #pos male fawns
#       Tmat[10,c(9,12),t-1] <- ((1-q)*phi[c(9,12)]*gamma[c(9,12),t-1])*(sum(n[c(5,6),t-1])/nf[t-1]) #only some are pos from fathers
#       Tmat[11,10,t-1] <- phi[10] #pos Y can come from pos Fawns
#     }
#     post_h <- (n[,t-1] - c(H))
#     post_h[post_h <0] <- 0
#     n[,t] <- Tmat[,,t-1] %*% (post_h) #this is the N(t+1) = H*A(t) approach 
#     #n[,t] <- Tmat[,,t-1] %*% n[,t-1] #no harvest
#     N[t] <- sum(n[,t])
#     Totneg[t] <- sum(n[c(1:3, 7:9),t])
#     Totpos[t] <- sum(n[c(4:6, 10:12),t])
#     
#   }# end t
#   
#   lambda <- array(NA, nocc)
#   Sens <- array(NA, c(nStates, nStates, nocc))
#   u0 <- u <- array(NA, c(nStates, nocc))
#   u2 <- array(NA, c(6, nocc))
#   for(t in 1:(nocc-1)){
#     ea <- eigen(Tmat[,,t])
#     lambda[t] <- Re(ea$values)[1]        ## Growth rate
#     u0[,t] <- Re(ea$vectors[,1])
#     u[,t]<- u0[,t]/sum(u0[,t])                  ## Stable age distribution
#     u2[,t] <- c(u[1:3,t] + u[4:6,t], u[7:9,t] + u[10:12,t])
#     v0 <- Re(eigen(t(Tmat[,,t]))$vectors[,1])
#     v <- v0/v0[1]                     ## Standardized reproductive value
#     
#     ### sensitivity
#     vs <- v%*%t(u0[,t]) #standard repro values * inverse stable age 
#     Sens[,,t] <- vs/as.numeric(v%*%u0[,t])
#   }
#   
#   Totpop <- data.frame(t = 1:nocc,
#                        N = c(N, Totneg, Totpos),
#                        group = rep(c("All", "CWD Negative", "CWD Positive"), each = nocc))
#   
#   agepop <- data.frame(t = rep(1:nocc, each =12),
#                        N = c(n),
#                        group = rep(c("F- Fawns", "F- Yearlings", "F- Adults", 
#                                      "F+ Fawns", "F+ Yearlings", "F+ Adults",
#                                      "M- Fawns", "M- Yearlings", "M- Adults", 
#                                      "M+ Fawns", "M+ Yearlings", "M+ Adults"), nocc))
#   #lambda_pop <- data.frame(t = 1:nocc,
#   #                         lambda = lambda)
#   
#   Lambda_calcs <- data.frame(t = rep(2:nocc, 2),
#                              lambda = c(lambda[1:(nocc-1)], N[2:nocc]/N[1:(nocc-1)]),
#                              Group = rep(c("No Harvest", "With Harvest"), each = nocc-1))
#   
#   return(list(n=n, N=N, Totpos = Totpos, Totneg = Totneg, Tmat = Tmat, 
#               gamma = gamma, lambda= lambda, Sens = Sens, eta_d = eta_d,
#               Totpop = Totpop, agepop = agepop, lambda_calcs = Lambda_calcs, u2 = u2))
# }

### Tab 3, spatial both sex model ####

I_2sex <- function(
    n0, #initial population (counts)
    eta, #transmission intercept 
    eta2,
    nocc = 5, #time steps,
    nPixels, #total pixels
    gridDis, #distance between pixels
    pixArea = 6.178, #sq miles per pixel
    sigmas,
    phis,
    scenarios,
    s.r = .45, #sex ratio for fawns
    posfawns = FALSE,
    quickrun = FALSE,
    APR,
    DoeLim,
    BuckLim,
    grid0
){
  
  ## start by sorting out useable vs unusable pixels
  max_dist <- 8 #pixel spacing
  noGo <- which(is.na(n0[3,]))
  mdist <- sqrt(2*max_dist^2)+.1 #allow for diagonal pixel movements too
  
  #minimize movement:
  gdist <- gridDis
  gdist[noGo,] <- NA
  gdist[,noGo] <- NA
  gdist[gdist > mdist] <- NA
  pixcount <- apply(gdist, 1, function(x){sum(!is.na(x))}) #how many pixels available to move to from each other pixel
  pixopts_pre <- stri_list2matrix(apply(gdist, 1, function(x){which(!is.na(x))}), byrow = TRUE) #returns a character matrix
  pixopts <- matrix(as.numeric(pixopts_pre), ncol = ncol(pixopts_pre)) #messy programming
  pixopts[is.na(pixopts)] <- -1 #avoid an annoying warning

  #Prep functions:
  normalize_move <- function(sigma, pixcount, pixopts, nState, nPix, gridDist, noGo){
      probs = array(0, dim = c(nPixels, nPixels, nState)) 
      ## only calculate for reasonably close pixels
      for(q in 1:nPix){
        if(q %in% noGo) next
        for(k in 1:nState){
          if(k != 4){
            if(k != 10){
              for(pp in 1:pixcount[q]){
                probs[q,pixopts[q,pp],k] <- exp(-(gridDist[q,pixopts[q,pp]])^2/(2*sigma[k]^2))
              }
              probs[q, 1:nPix,k] <- probs[q,1:nPix,k]/sum(probs[q,1:nPix, k])
            }
          }
        }#end k
      } #end q
      return(probs[1:nPix, 1:nPix, 1:nState])
    }
  
  mmm_psi = function(X, P){ #psi multis
    p = nrow(P)
    Y <- Reduce("+", lapply(1:p, function(j) {
      rmultinom(1, size = X[j], P[j,])
    }))
    return(as.vector(Y[1:length(X),1]))
  }
  mmm_move = function(X, P){ #movement multis
    p = nrow(P)
    Y <- Reduce("+", lapply(1:p, function(j) {
      rmultinom(1, size = X[j], P[j,])
    }))
    return(as.vector(Y))
  }
  
  mmm_fawn = function(X, P,s.r){ #total fawns
    p = nrow(P)
    Y <- Reduce("+", lapply(1:p, function(j) {
      rmultinom(1, size = X[j], P[j,])
    }))
    m_f <- c(round(s.r*sum(Y[1:3]*1:3)), round(s.r*sum(Y[4:6]*1:3)), 
             sum(Y[1:3]*1:3) - round(s.r*sum(Y[1:3]*1:3)),
             sum(Y[4:6]*1:3) - round(s.r*sum(Y[4:6]*1:3))) #neg F fawns, neg M fawns, pos F fawns, pos M fawns
    return(m_f)
  }
 `%notin%` <- Negate(`%in%`)
 
 repro <- function(gamma, p_neg){
    p_repro_pos <- c(c(gamma*c_repro[4,1]*(p_neg), gamma*c_repro[4,2]*(p_neg), gamma*c_repro[4,3]*(p_neg), 
                       gamma*c_repro[4,1]*(1-p_neg), gamma*c_repro[4,2]*(1-p_neg), 
                       gamma*c_repro[4,3]*(1-p_neg), (gamma*c_repro[4,4] + 1- gamma)), #F pos fawn
                    c(gamma*c_repro[5,1]*(p_neg), gamma*c_repro[5,2]*(p_neg), gamma*c_repro[5,3]*(p_neg),
                      gamma*c_repro[5,1]*(1-p_neg), gamma*c_repro[5,2]*(1-p_neg), gamma*c_repro[5,3]*(1-p_neg),
                      (gamma*c_repro[5,4] + 1- gamma)), #F pos year
                    c(gamma*c_repro[6,1]*(p_neg), gamma*c_repro[6,2]*(p_neg), gamma*c_repro[6,3]*(p_neg),
                      gamma*c_repro[6,1]*(1-p_neg), gamma*c_repro[6,2]*(1-p_neg), gamma*c_repro[6,3]*(1-p_neg),
                      (gamma*c_repro[6,4] + 1- gamma)))
    p_repro <- matrix(c(#1 neg fawn, 2 neg fawn, 3 neg fawn, 1 pos fawn, 2 pos fawn, 3 pos fawn, no fawn
     c(gamma*c_repro[1,1], gamma*c_repro[1,2], gamma*c_repro[1,3], rep(0, 3), (gamma*c_repro[1,4] + 1- gamma)), #F neg fawns
     c(gamma*c_repro[2,1], gamma*c_repro[2,2], gamma*c_repro[2,3], rep(0,3), (gamma*c_repro[2,4] + 1- gamma)), #F neg year
     c(gamma*c_repro[3,1], gamma*c_repro[3,2], gamma*c_repro[3,3], rep(0, 3), (gamma*c_repro[3,4] + 1- gamma)), #F neg adult
     p_repro_pos, #positive females
     c(rep(0,6), 1), #M neg fawns
     c(rep(0,6), 1), #M neg year
     c(rep(0,6), 1), #M neg adult
     c(rep(0,6), 1), #M pos fawns
     c(rep(0,6), 1), #M pos year
     c(rep(0,6), 1)),  #M pos adult
     nc = 7, byrow = T) #last column = p(no babies)
   return(p_repro)
 }
 
  hunting <- function(APR, DoeLim, BuckLim, pixel, ns, npre){
    remove <- array(0, 12)
    mycounty <- grid0$County[pixel]
    if(mycounty %notin% unique(scenarios$county)) mycounty <- 'Marion'
    lims <- subset(scenarios, scenarios$county == mycounty & 
                     scenarios$DoeLimit == DoeLim & 
                     scenarios$BuckLimit == BuckLim &
                     scenarios$APR == APR)
    lims$adj_harv <- lims$harv*(sum(ns)/sum(npre+.01)) #adjust as population fluctuates 
    lims$adj_sd <- lims$sd_harv*(sum(ns)/sum(npre+.01)) #adjust as population fluctuates 
    #print(lims$adj_harv)
    bucks <- round(rnorm(1, lims[1,'adj_harv'], sd = lims[1,'adj_sd'])/lims$Area[1]*pixArea)
    fawns <- round(rnorm(2, lims[2,'adj_harv'], sd = lims[2,'adj_sd'])/lims$Area[1]*pixArea)
    does <- round(rnorm(1, lims[3,'adj_harv'], sd = lims[3,'adj_sd'])/lims$Area[1]*pixArea)
    if(bucks < 0) bucks <- 0
    if(fawns[1] < 0) fawns[1] <- 0
    if(fawns[2] < 0) fawns[2] <- 0
    if(does < 0) does <- 0
    
    if(sum(ns[c(8:9,11:12)]) >0 ){
      if(APR == 0){
        p_bucks <- c(ns[c(8:9,11:12)]*c(1,1.2,1,1.2))/sum(ns[c(8:9,11:12)])
      } else {
        p_bucks <- c(0, ns[9]/sum(ns[c(9,12)]), 0, ns[12]/sum(ns[c(9,12)]))
      }
    if(!any(is.na(p_bucks))){
    remove[c(8:9, 11:12)] <- t(rmultinom(1, size = bucks, p_bucks))
    }
    }
    remove[c(1,7)] <- fawns
    if(sum(ns[c(2:3,5:6)]) >0 ){
    p_does <- c(ns[c(2:3,5:6)]/sum(ns[c(2:3,5:6)]))
    remove[c(2:3,5:6)] <- t(rmultinom(1, size = does, p_does))
    }
    for(j in 1:12){
      remove[j] <- min(remove[j], ns[j])
    }
    return(remove)
  }
  
  nStates <- nState <- 12 
  sex <- rep(1:0, each= 6) #sex of each stage
  age <- rep(1:3, 4) #age of each stage
  infected <- rep(rep(0:1, each = 3), 2) #infection status of each stage
  n_moves <- array(0, c(nStates, nocc, nPixels, nPixels))
  n_psi <- array(0, c(nStates, nStates,nocc,  nPixels)) #note that n_psi is now just nStates not nStates+1
  fawns <- subs <- adults <- array(NA, c(nocc))
  progress_messages <<- NA
  
  n <- n_post_move <- n_pre <- allfawns <- eta_d <- survivors <- array(0, c(nStates, nocc, nPixels))
  move_probs_norm <- array(0, c(nPixels, nPixels, nState))
  n_bigmoves <- array(0, c(nState, nocc, nPixels, nPixels))
  cwd_pix <- gamma <-p_repro <- array(0, c(nPixels, nocc)) #index of how much cwd is present in the landscape at each pixel
  N_ads <- prev_q <- prev_q2 <- array(0, c(nPixels, nocc))
  psi <- array(0, c(nStates, nStates, nocc, nPixels)) #transition matrix (infect + surv + age) 
  n[,1,] <- round(n0)
  cwd_pix[-noGo,1] <- colSums(n[c(4:6, 10:12),1,-noGo]) > 0

  
  N <- Totneg <- Totpos <- TotF_r <- integer(nocc)
  Nq <- Pq <- Hq <- array(0, c(nocc, nPixels)) #Pq = total pos, Hq = total neg
  Nq[1,-noGo] <- colSums(n[,1,-noGo])
  Pq[1,-noGo] <- colSums(n[c(4:6, 10:12),1,-noGo])
  Hq[1,-noGo] <- colSums(n[c(1:3, 7:9),1,-noGo])
  N_ads[-noGo,1] <- colSums(n[c(2,3,5,6,8,9,11,12), 1,-noGo ])
  prev_q[-noGo,1] <- (Pq[1,-noGo]/(Nq[1,-noGo]+.000001))*(Nq[1,-noGo] >0)
  prev_q2[-noGo,1] <- (Pq[1,-noGo]/(colSums(n[c(2,3,5,6,8,9,11,12), 1,-noGo ])+.000001))*(colSums(n[c(2,3,5,6,8,9,11,12), 1,-noGo ]) >0)
  N[1] <- sum(n[,1,-noGo])
  Totneg[1] <- sum(n[c(1:3, 7:9),1,-noGo])
  Totpos[1] <- sum(n[c(5:6, 10:12),1,-noGo])
  TotF_r[1] <- sum(n[c(2:3, 5:6),1,-noGo])/sum(n[c(2:3,5:6,8:9, 11:12),1,-noGo])
  fawns[1] <- sum(n[c(1,4,7,10),1,-noGo])
  subs[1] <- sum(n[c(2,5,8,11),1,-noGo])
  adults[1] <- sum(n[c(3,6,9,12),1,-noGo])
  ### Basing this on herd data from AK state agency
  c_repro <- matrix(c(.04, 0, 0,.96, #neg F Fawns
                      .59, .41,  .0,0, #neg F year
                      .085, .18, .135, 0,#neg F adult
                      .04, 0, 0,.96, #pos F Fawns
                      .59, .41,  .0,0, #pos F year
                      .085, .18, .135, 0,#pos F adult
                      0, 0, 0,1,
                      0, 0, 0,1,
                      0, 0, 0,1,
                      0, 0, 0,1,
                      0, 0, 0,1,
                      0, 0, 0,1), nrow = 12, byrow = T) #given I successfully mate, probability of litter sizes 1-3 or no repro (last column)
  
  #survival unaffected by landscape:
  phi <- phis
  move_probs_norm[1:nPix, 1:nPix, 1:nState] <- normalize_move(sigma =  sigmas[1:12],
                                                              pixcount = pixcount[1:nPix],
                                                              pixopts = pixopts[1:nPix, ],
                                                              nState = nState,
                                                              nPix = nPix,
                                                              gridDist = gridDis[1:nPix, 1:nPix],
                                                              noGo = noGo
  )
  
  for(t in 2:nocc){
    progress_messages <<- paste("Calculating year", t, "of", nocc)
    print(progress_messages)
    # Update the reactive value with the new message
    #sim_messages(progress_messages)
    
    for(q in 1:nPixels){
      if(q %in% noGo){next} #ignore non-boundary pixels
      # if(sum(n[,t-1,q]) == 0){
      #   n_pre[,t,q] <- 0
      #   next
      # }
      
    if(t != 2){
     survivors[1:6, t, q] <- rmm_infect(1, 
                                       nn = n[1:6,t-1,q],
                                       phi = phi,
                                       eta0 = eta,
                                       nState = nState,
                                       sex = 1, #female
                                       PastPos = sum(Pq[1:(t-2),q]),
                                       pos_friends = Pq[t-1,q])

      survivors[7:12, t, q] <- rmm_infect(1, 
                                          nn = n[7:12,t-1,q],
                                          phi = phi,
                                          eta0 = eta,
                                          nState = nState,
                                          sex = 0, #male
                                          PastPos = sum(Pq[1:(t-2),q]),
                                          pos_friends = Pq[t-1,q])
    } else {
      survivors[1:6, t, q] <- rmm_infect(1, 
                                         nn = n[1:6,t-1,q],
                                         phi = phi,
                                         eta0 = eta,
                                         nState = nState,
                                         sex = 1, #female
                                         PastPos = Pq[t-1,q],
                                         pos_friends = Pq[t-1,q])
      
      survivors[7:12, t, q] <- rmm_infect(1, 
                                          nn = n[7:12,t-1,q],
                                          phi = phi,
                                          eta0 = eta,
                                          nState = nState,
                                          sex = 0, #male
                                          PastPos = Pq[t-1,q],
                                          pos_friends = Pq[t-1,q])
    }
    } #end of q
    ## Now they move
    #this could be more efficient in a model, but okay for simulation
    for(m in 1:nStates){ #each state 
      for(q in 1:nPix){
      if(sum(survivors[m,t,-noGo]) == 0) next
        n_bigmoves[m,t, q, 1:nPix] <- rmulti(1, size = survivors[m,t,q], move_probs_norm[q, 1:nPix, m])
      }
    } #end m
    
    for(m in 1:nStates){
      for(q in 1:nPix){
    n[m,t,q] <- sum(n_bigmoves[m,t,1:nPix,q]) #how many ended in this pixel
    if(m != 1 & m != 7){n_post_move[m,t,q] <- n[m,t,q]}
      }
      }
    ### New Fawns 
    ## male contribution is via gamma, requires a male to be in the vicinity (the current or nearby pixels)
     # gamma<- 1 #not used
      
      if(posfawns == FALSE){ 
        p_neg <- 1
      } else{
        p_neg <- .1
      }
      
     for(q in 1:nPix){
       if(q %in% noGo) next 
      gamma[q,t] <- (sum(n[c(8:12),t,pixopts[q, 1:pixcount[q]]]) > 0)*1 #basically are there males nearby
      n[c(1,4,7,10), t, q] <- mmm_fawn(n[,t,q], repro(gamma[q,t], p_neg),s.r)  #all transitions finished
      
      ## Deal with edges by adding in some deer 
       if(q %in% edge_cells){
        fromoutside <- t(rmultinom(1, size = sample(0:2, 1), prob = c(0, .1, .1, 0, .05,.05, 0, .15, .15, 0, .05, .05)))
        n[1:12,t,q] <- n[1:12,t,q] + fromoutside
      }
      
      ### Introduce Hunting ####
      n[1:12,t,q] <- n[1:12, t, q ] - hunting(APR, DoeLim, BuckLim, q, n[1:12, t, q], n[1:12,1,q])
      
      Nq[t,q] <- sum(n[,t,q])
      Pq[t,q] <- sum(n[c(5:6, 11:12),t,q])
      Hq[t,q] <- sum(n[c(1:3, 7:9),t,q])
      cwd_pix[q,t] <- (sum(n[c(4:6, 10:12),t,q]) + cwd_pix[q,t-1]) > 0
      N_ads[q,t] <- sum(n[c(2,3,5,6,8,9,11,12), t, q])
      prev_q[q,t] <- (Pq[t,q]/(Nq[t,q]+.000001))*(Nq[t,q] >0)
      prev_q2[q,t] <- (Pq[t,q]/(sum(n[c(2:3, 5:6, 8:9, 11:12),t,q])+.000001))*(sum(n[c(2:3, 5:6, 8:9, 11:12),t,q]) >0)
      #each pos deer sheds 1 unit of cwd into their pixel per time frame
    } #end q
    
    N[t] <- sum(n[,t,-noGo])
    Totneg[t] <- sum(n[c(1:3, 7:9),t,-noGo])
    Totpos[t] <- sum(n[c(5:6, 11:12),t,-noGo])
    TotF_r[t] <- sum(n[c(2:3, 5:6),t,-noGo])/sum(n[c(2:3,5:6,8:9, 11:12),t,-noGo])
    fawns[t] <- sum(n[c(1,4,7,10),t,-noGo])
    subs[t] <- sum(n[c(2,5,8,11),t,-noGo])
    adults[t] <- sum(n[c(3,6,9,12),t,-noGo])
  }# end t
  
  notfawns <- adults + subs
  popcrash <- ifelse(all(N >0), nocc, min(which(N == 0))-1)
  
  Totpop <- data.frame(t = 1:nocc,
                       N = c(N/(6.18*(nPixels-length(noGo))), Totneg/(6.18*(nPixels-length(noGo))), Totpos/(6.18*(nPixels-length(noGo)))),
                       group = rep(c("All", "CWD Negative", "CWD Positive"), each = nocc))
  Totput <- subset(Totpop, Totpop$t <= popcrash)
  
  Agepop <- data.frame(t = rep(1:nocc, each = 12),
                       N = c(apply(n[,,-noGo], c(1,2), sum)),
                       group = rep(c("F- Fawns", "F- Yearlings", "F- Adults", 
                                     "F+ Fawns", "F+ Yearlings", "F+ Adults",
                                     "M- Fawns", "M- Yearlings", "M- Adults", 
                                     "M+ Fawns", "M+ Yearlings", "M+ Adults"), nocc),
                       group2 = rep(c("F Fawns", "F Yearlings", "F Adults", 
                                      "F Fawns", "F Yearlings", "F Adults",
                                      "M Fawns", "M Yearlings", "M Adults", 
                                      "M Fawns", "M Yearlings", "M Adults"), nocc))
  Agepop <- subset(Agepop, Agepop$t <= popcrash)
  
  Agepop2 <- Agepop %>% group_by(t, group2) %>% summarize(N2 = sum(N/(6.18*(nPixels-length(noGo)))))
  Agepop2 <- as.data.frame(Agepop2)
  
  Ratios <- data.frame(t = rep(1:nocc, 3),
                       Prop = c(TotF_r, Totpos/N, Totpos/(N-fawns)),
                       group = rep(c("Adult Sex Ratio", 'Prevalence', 'Adult Prevalence'), each = nocc))
  Ratios <- subset(Ratios, Ratios$t <= popcrash)
  
  PixPrevs <- data.frame(t =rep(1:nocc, nPixels),
                      Prop = c(Pq/(Nq+.00001)),
                      Gr0 = (c(Pq)>0)*1)
  #PixPrevs <- subset(PixPrevs, PixPrevs$t <= popcrash & PixPrevs$Gr0 == 1)
  
  prev_map1 <- data.frame(prev = c(prev_q2),
                         ns = c(N_ads),
                         time = rep(1:nocc, each = nPixels),
                         pixel = 1:nPixels,
                         county = rep(grid0$County, nocc))
  #prev_map <- subset(prev_map, prevmap$ns >0) #no prevalence if no deer
  prev_map1$prev <- ifelse(prev_map1$pixel %in% noGo, NA, prev_map1$prev)
  prev_map <- as.data.frame(prev_map1 %>%
                  group_by(county, time) %>%
                  mutate(AvgPrev = mean(prev, na.rm = TRUE),
                         prevs = prev) %>%
                  ungroup())
  #prev_map <- subset(prev_map, prev_map$pixel %in% c(1:nPixels)[-noGo])
  
  SimpleAge <- data.frame(t = rep(1:nocc, 3),
                          N = c(fawns/N, subs/N, adults/N),
                          group = c(rep(c('Fawns', 'Yearlings', 'Adults'), each = nocc)))

  SimpleAge <- subset(SimpleAge, SimpleAge$t <= popcrash)
  SimpleAge$group <- factor(SimpleAge$group, levels = c('Adults', 'Yearlings', 'Fawns'))
  
  ## For Richard, some Leslie matrix type outputs
  # f_pos<- array(0, c(6,6))
  # for(i in 1:6){
  #   f_pos[i,1] <- sum(p_repro[i,1:3]*c(1:3)) #negative fawns
  #   f_pos[i,4] <- sum(p_repro[i,4:6]*c(1:3)) #pos fawns
  #   
  # }
  
  #Growth rate vs. prevalence 
  Prev_lam <- data.frame(t = 2:nocc,
                       Prop = Totpos[2:nocc]/N[2:nocc],
                       Lambda = N[2:nocc]/N[1:(nocc-1)])
  Prev_lam <- subset(Prev_lam, Prev_lam$t <= popcrash)
  
  if(quickrun){
    return(list(N = N))
  } else {
  return(list(n=n, N=N, Nq = Nq, Totpos = Totpos, Totneg = Totneg,Ratios =Ratios,
              eta_d = eta_d, psi = psi, Totpop = Totpop, Agepop = Agepop, 
              phi = phi, fawns = fawns, subs= subs, adults = adults,gamma = gamma,
              Pq = Pq, Hq = Hq, n_pre = n_pre, n_post_move = n_post_move, 
              n_moves = n_moves, n_psi = n_psi, survivors = survivors, n0 = n0,
              eta = eta, alpha = alpha,  move_probs_norm = move_probs_norm,
              eta2 = eta2, nocc = nocc, nPix = nPixels, gridDis = gridDis,
              s.r = s.r, posfawns = posfawns, SimpleAge = SimpleAge, noGo = noGo,
              p_repro = p_repro, Prev_lam = Prev_lam, PixPrevs = PixPrevs, prev_map = prev_map,prev_map1 = prev_map1,grid0 = grid0,
              Agepop2 = Agepop2))
  }
}

#### For spatial model, need a surface ####
library(terra)
library(latticeExtra)
library(PoissonMultinomial)
#dem <- scale(rast('AR_DEM_4Kres.tif'))
#dem <- scale(readRDS('AR_DEM-4K_2.rds'))
dem <- rast('cellholder.tif')
na_cells <- is.na(dem)
touching_na <- focal(na_cells, w = 3, fun = max, na.rm = FALSE)
edge_cells <- c(which(terra::values(touching_na) == 1 & !is.na(terra::values(dem))), 122,162, 202, 242, 282, 322)
xlim <- ext(dem)[1:2]
ylim <- ext(dem)[3:4]
delta <- res(dem)[1] #this is the width of pixels in our environment  
grid0 <- as.data.frame(dem, xy= TRUE, na.rm = F)
grid0$County <- NA
grid <- grid0[,1:2]
grid0$County[which(grid0[,3] == 66)] <- 'Marion'
grid0$County[which(grid0[,3] == 19)] <- 'Izard'
grid0$County[which(grid0[,3] == 65)] <- 'Polk'
grid0$County[which(grid0[,3] == 11)] <- 'Van Buren'
grid0$County[which(grid0[,3] == 1)] <- 'Stone'
grid0$County[which(grid0[,3] == 10)] <- 'Carroll'
grid0$County[which(grid0[,3] == 13)] <- 'Searcy'
grid0$County[which(grid0[,3] == 31)] <- 'Newton'
grid0$County[which(grid0[,3] == 40)] <- 'Baxter'
grid0$County[which(grid0[,3] == 42)] <- 'Boone'
grid0$County[which(grid0[,3] == 58)] <- 'Madison'
nPix <- nrow(grid) #count pixels
#pixArea <- delta^2 #area of a square is just width*width
gridDis <- as.matrix(dist(grid))/1000 #distance between each grid point, in km
W <- grid0[,3]
base_rast <- dem
base_rast[] <- 0
names(grid0) <- c('x', 'y', 'Prions', 'County')


