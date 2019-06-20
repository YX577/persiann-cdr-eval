#*[---------------------------------------------------------------------------------------------]*#
#*[ Objective: This function takes a vector of seasonal data (length 136 = 4 seasons * 34 yrs)  ]*#
#*[            and detects changepoints in the data. Return value is the number of changepoints ]*#
#*[            and season each occurs as a numerical value.                                     ]*#
#*[                                                                                             ]*#
#*[            For example, a return value of 2 64 128 means two changepoints occur, one at     ]*#
#*[            season 64 and one at season 128.                                                 ]*#
#*[                                                                                             ]*#
#*[            A return value of 0 means zero changepoints occurred.                            ]*#
#*[ Author: Maria Paquin                                               __       __              ]*#
#*[ Updated: 01/03/2018                                                ) \     / (              ]*#
#*[                                                                   )_  \_V_/  _(             ]*#
#*[                                                                     )__   __(               ]*#
#*[                                                                        '-'                  ]*#
#*[---------------------------------------------------------------------------------------------]*#


changepoints <- function(point) {
  
  #[------------------------------]#
  #[ Generate initial population  ]#
  #[------------------------------]#
  
  
  initPop <- function(maxCP,genSize){
    
    population <- vector("list", genSize)
    
    for (i in 1:genSize){
      k <- sample(0:maxCP, 1)
      population[[i]] <- c(k, sample(1:136, k, replace = FALSE))
    }
    return(population)
  }
  
  
  #[------------------------------]#
  #[     Fitness Evaluation       ]#
  #[------------------------------]#
  
  # Model for log liklihood
  # point <- with(df, df[ Lat == lat & Lon == lon, c(-1,-2)])
  # point <- t(point)
  
  n <- 136
  time <- (1:n)/40
  
  Q1 = rep(c(1, 0, 0, 0), n / 4)
  Q2 = rep(c(0, 1, 0, 0), n / 4)
  Q3 = rep(c(0, 0, 1, 0), n / 4)
  Q4 = rep(c(0, 0, 0, 1), n / 4)
  
  Q <- Q1 + 2*Q2 + 3*Q3 + 4*Q4
  
  # Generate MDLScore for each "chomorosome" in the population
  fitnessEval <- function(population){
    genSize = length(population)
    
    MDLscore <- matrix(nrow = genSize,ncol = 1)
    
    for(index in 1:genSize){
      
      numCP = population[[index]][1]
      
      if(numCP == 0){
        k = 0
      }else{
        config = combine(population[[index]])
        cp = config[-1]
        k = config[1]
      }
      
      if(k != 0){
        
        delta <- matrix(0, nrow=136,ncol=1+k)
        p = penalty(config)
        
        if(k >= 2){
          for (i in 2:k){
            delta[,i] <- c(rep(0, cp[i-1]-1),rep(i-1, cp[i]-cp[i-1]), rep(0, 136-cp[i]+1))
          }
        }
        
        delta[,k+1] <- c(rep(0,cp[k]-1), rep(k, 136-cp[k]+1))
        d <- as.matrix(rowSums(delta))
        fit.ols <- lm(point ~ factor(Q) + time + factor(d))
        
      }else{
        p = 0
        fit.ols <- lm(point ~ factor(Q) + time)
      }
      MDLscore[index] = (-1)*as.numeric(logLik(fit.ols))/log(2) + p
    }
    
    return(MDLscore)
  }
  
  
  
  # Returns sorted, reduced configuration of a chromosome in the format [k, m_1, m_2,...,m_k], 
  # where k is the number of changepoints, |m_i - m_j| < 6+k, |m_i - 0| < 6+k, and |m_i - 136| < 6+k
  combine <- function(config){
    k <- config[1]
    cp <- sort(config[-1])
    
    new_k <- 0
    new_config <- new_k
    
    for (i in 1:k){
      if(cp[i]-new_config[new_k+1] > 6+k){
        new_config <- c(new_config, cp[i])
        new_k = new_k + 1
      }
    }
    
    # Checking endpoint
    if(new_config[new_k+1] > 136-(6+k)){
      new_config <- new_config[-(new_k+1)]
      new_k = new_k - 1
    }
    
    new_config[1] = new_k
    
    return(new_config)
  }
  
  # Computes the penalty for fitness evaluation
  penalty <- function(config){ 
    k <- config[1]
    cp <- sort(config[-1])
    
    cp[k+1] <- 136
    
    j = 2:(k+1)
    i = 2:k
    
    p <- (1/2)*sum(log2(cp[j]-cp[j-1]))+log2(k+1)+sum(log2(cp[i]))
    
    return(p)
  }
  
  
  #[------------------------------]#
  #[        Selection             ]#
  #[------------------------------]#
  
  selection <- function(MDLscore){
    fitnessScore <- rank((-1)*MDLscore)
    total <- sum(fitnessScore)
    selectionProb <- fitnessScore/total
    parents <- sample(1:genSize, 2, replace = FALSE, prob = selectionProb)
    return(parents)
  }
  
  
  #[------------------------------]#
  #[    Offspring Production      ]#
  #[------------------------------]#
  
  # The mother and father chromosomes are combined
  combineParents <- function(motherChrom, fatherChrom){
    
    combinedChrom <- sort(union(fatherChrom, motherChrom)) # length(combinedChrom) > 0
    
    if(length(combinedChrom) > 1){
      if('0' %in% combinedChrom){
        combinedChrom <- combinedChrom[! combinedChrom %in% '0']
      }
    }
    
    return(combinedChrom) # 0 or 1+ changepoints
  }
  
  
  # The set of combined changepoints is retained/thinned
  # independently with 0.5 probability
  thin <- function(combinedChrom){
    
    thinnedChrom <- vector()
    
    if(! 0 %in% combinedChrom){
      
      for (i in 1:length(combinedChrom)){
        keep = sample(c(TRUE,FALSE), 1, prob = c(0.5, 0.5))
        if(keep){
          l = length(thinnedChrom)
          thinnedChrom[l+1] = combinedChrom[i]
        }
      }
    }else{
      thinnedChrom <- 0
    }
    
    if (length(thinnedChrom) == 0){
      thinnedChrom <- 0
    }
    
    return(thinnedChrom) # 0 or 1+ changepoints
    
  }
  
  
  # The changepoint times that remain are allowed to move
  # their location slightly, with probability:
  # 0.3 -> one time smaller
  # 0.4 -> same
  # 0.3 -> one time larger
  shift <- function(thinnedChrom){
    
    if(! 0 %in% thinnedChrom){
      
      for(j in 1:length(thinnedChrom)){
        move = sample(c(-1,0,1), 1, prob = c(0.3, 0.4, 0.3))
        if(move == -1 & thinnedChrom[j] != 1){
          thinnedChrom[j] <- thinnedChrom[j]-1
        }
        if(move == 1 & thinnedChrom[j] != 136){
          thinnedChrom[j] <- thinnedChrom[j]+1
        }
      }
    }
    
    shiftedChrom <- combine(c(length(thinnedChrom), thinnedChrom))
    
    if(!0 %in% shiftedChrom){
      return(shiftedChrom[-1]) # returns 1+ changepoints
    }
    
    return(shiftedChrom) # returns 0
    
  }
  
  # After each child is formed, every non-changepoint time is 
  # independently allowed to become a changepoint time with 
  # probability 0.003
  mutate <- function(childChrom){
    
    CP <- 1:136
    nonCP <- CP[! CP %in% childChrom]
    
    for(i in 1:length(nonCP)){
      addCP <- sample(c(TRUE,FALSE), 1, prob = c(0.003, 0.997))
      
      if(addCP){
        childChrom <- c(childChrom, nonCP[i])
      }
    }
    
    if(length(childChrom) > 1){
      if('0' %in% childChrom){
        childChrom <- childChrom[! childChrom %in% '0']
      }
    }
    
    childChrom <- combine(c(length(childChrom), childChrom))
    
    if(!0 %in% childChrom){
      return(childChrom[-1]) # returns 1+ changepoints
    }
    
    return(childChrom) # returns 0
  }
  
  
  # Generate new child chromosome
  generateChildChrom <- function(motherChrom, fatherChrom){
    
    childChrom <- list()  
    combinedParents <- combineParents(motherChrom, fatherChrom)
    
    thinnedChrom <- thin(combinedParents)
    shiftedChrom <- shift(thinnedChrom)
    mutatedChrom <- mutate(shiftedChrom)
    
    if(!0 %in% mutatedChrom){
      childChrom <- c(length(mutatedChrom), mutatedChrom)
    }else{
      childChrom <- 0
    }
    return(childChrom)
  }

  # Initial parameters
  maxCP <- 5
  genSize <- 30
  n <- 50
  
  population <- vector()
  population <- initPop(maxCP,genSize)
  
  MDLscore <- fitnessEval(population)
  
  for(generationCount in 1:n){
    
    prevPopulation <- population
    
    bestChrom <- which(MDLscore %in% min(MDLscore))[1]
    bestMDL <- MDLscore[bestChrom]
    
    population <- vector("list", genSize)
    
    for(chromCount in 1:genSize){
      
      parents <- selection(MDLscore)
      
      mother <- parents[1] # config of mother chromosome
      father <- parents[2] # config of father chromosome
      
      if('0' %in% prevPopulation[[mother]]){
        motherChrom <- 0
      }else{
        motherChrom <- prevPopulation[[mother]][-1] # config w/o k
      }
      
      if('0' %in% prevPopulation[[father]]){
        fatherChrom <- 0
      }else{
        fatherChrom <- prevPopulation[[father]][-1] # config w/o k
      }
      
      childChrom <- generateChildChrom(motherChrom, fatherChrom)
      
      population[[chromCount]] <- childChrom
      
    }
    
    MDLscore <- fitnessEval(population)
    
    # Replace worst chromosome of current generation with best chromosome
    # of previous generation
    worstChrom <- which(MDLscore %in% max(MDLscore))[1]
    population[[worstChrom]] <- prevPopulation[[bestChrom]]
    MDLscore[worstChrom] <- bestMDL
    
  }
  
  finalBestMDL <- which(MDLscore %in% min(MDLscore))
  population[[finalBestMDL[1]]] # return final changepoint configuration

}

