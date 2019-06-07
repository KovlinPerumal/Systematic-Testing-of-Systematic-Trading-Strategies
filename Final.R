##Kovlin Perumal
##Contact: <kovlinpermal@gmail.com>
##Systematic Testing of Systematic Trading Strategies
##This code was written for the research paper, Systematic Testing of Systematic Trading Strategies.
##Code not referenced was written completely by myself and I take full responsibility for any errors made.

##Preamble------------------------------------------------------------

setwd("C:/Users/kovlin/Desktop/WORK/4th Year/Project/Main Project")


library(readxl)
library(boot)
library(PerformanceAnalytics)
library(xlsx)
library(psych)
library(ggplot2); theme_set(theme_grey())
library(Hmisc)
library(xtable)



##Function For Simulating Market Data---------------------------------
##Adapted from code written by Xavier Wells url:[https://rdrr.io/github/R-Finance/Meucci/man/SimulateJumpDiffusionMerton.html]

SimulateJumpDiffusionMerton = function( m, s, l, a, D, ts, J )
{
  L = length(ts);
  T = ts[ L ];
  
  # simulate number of jumps; 
  N = rpois( J, l * T );
  
  Jumps = matrix( 0, J, L );
  for( j in 1 : J )
  {
    # simulate jump arrival time
    t = T * rnorm(N[ j ]);
    t = sort(t);
    
    # simulate jump size
    S = a + D * rnorm(N[ j ]);
    
    # put things together
    CumS = cumsum(S);
    Jumps_ts = matrix( 0, 1, L);
    for( n in 1 : L )
    {
      Events = sum( t <= ts[ n ]);
      if( Events )
      {
        Jumps_ts[ n ] = CumS[ Events ];
      }
    }
    
    Jumps[ j, ] = Jumps_ts;
  }
  
  D_Diff = matrix( NaN, J, L );
  for( l in 1 : L )
  {
    Dt = ts[ l ];
    if( l > 1 )
    {
      Dt = ts[ l ] - ts[ l - 1 ];
    }
    
    D_Diff[ , l ] = m * Dt + s * sqrt(Dt) * rnorm(J); 
  }
  
  X = cbind( matrix(0, J, 1), apply(D_Diff, 2, cumsum) + Jumps );
  
  return( X );
}

##Function For Generating ATRs------------------------------------
GenerateATRs = function( stratSims, stratLength, pSuccess, returns)
{
 
 strat <- 0
 stratMatrix <- matrix( nrow = stratLength, ncol = stratSims)
 
 for(k in 1:stratSims){
  
   for(i in 1:stratLength){
    
     random = runif(1)
    
     if(random < pSuccess)
     {
       if(returns[i] > 0)
       {
        
         strat[i] = 1
       }
       else
       {
         strat[i] = -1
       }
      
     }
     else
     {
       if(returns[i] < 0)
       {
        
         strat[i] = 1
       }
       else
       {
         strat[i] = -1
       }
     }
    
    
   }
  
   stratMatrix[,k] <- strat
  
 }
 
  return(stratMatrix)
}
##Function For Chopping Returns------------------------------------
ChopReturns = function( stratSims, stratLength, returns){
  
 returnsSim <- NULL
  
 for(k in 1:stratSims){
  
   for(i in 1:stratLength){
     returnsSim[i] <- returns[i]
   } 
 }

 return(returnsSim)
}

##Function For Generating Mean Strategy Returns-------------------
GenerateMeanStratReturn = function( stratSims,stratLength, returnsSim, stratMatrix)
{
 pureStratReturns <- 0

 for(i in 1:stratSims){
   pureStratReturns[i] = returnsSim %*% stratMatrix[,i]
 }
 

 meanStratReturns <- NULL

 for (i in 1:stratSims) {
  
   meanStratReturns[i] = pureStratReturns[i]/stratLength
  
  
 }

 return(meanStratReturns)
}

##Function For Individual T-Tests-------------------

PerformTtests = function( stratSims,stratLength, meanStratReturns, returnsSim)
{
  

  alpha = 0.025
  countSig = 0
  sigStrats <- NULL
  pSigVector <- NULL
  sigSymbol <- NULL
  n <- 1:stratSims
  
  for(i in 1:stratSims)
  {
  
   t.stat <- (meanStratReturns[i] - 0) / ( sqrt(var(returnsSim[,i]))/sqrt(stratLength))
   dof <- stratSims - 1
   t.critical <- qt(1-alpha, df= dof) 
   p.value <- 1 - pt(t.stat, df= dof)
  
   if(t.stat >= t.critical)
   {
    
    countSig = countSig + 1
    sigStrats = c(sigStrats, i)
    pSigVector = c(pSigVector, p.value)
    sigSymbol = c(sigSymbol, '*')
   }
   else
   {
    
    pSigVector = c(pSigVector, p.value)
    sigSymbol = c(sigSymbol, '') 
   }   
  
  
  }
  
 sigMatrixTtests = data.frame( Strategy = n, probability = pSigVector, Significance = sigSymbol)

 return(sigMatrixTtests)
}

##Function For FWER T-Tests-------------------

PerformFWERtests = function( stratSims,stratLength, meanStratReturns, returnsSim)
{

  
  alpha = 0.025/stratSims
  countSig = 0
  n <- 1:stratSims
  sigStrats <- NULL
  pSigVector <- NULL
  sigSymbol <- NULL
  
  for(i in 1:stratSims)
  {
    
    t.stat <- (meanStratReturns[i] - 0) / ( sqrt(var(returnsSim[,i]))/sqrt(stratLength))
    dof <- stratSims - 1
    t.critical <- qt(1-alpha, df= dof) 
    p.value <- 1 - pt(t.stat, df= dof)
    
    if(t.stat >= t.critical)
    {
  
      countSig = countSig + 1
      sigStrats = c(sigStrats, i)
      pSigVector = c(pSigVector, p.value)
      sigSymbol = c(sigSymbol, '*')
    }
    else
    {
      
      pSigVector = c(pSigVector, p.value)
      sigSymbol = c(sigSymbol, '') 
    }   
    
    
  }
  
  sigMatrixFWERtests = data.frame( Strategy = n, probability = pSigVector, Significance = sigSymbol)
  
  return(sigMatrixFWERtests)
}

##Function For FDR T-Tests-----------------------------------
PerformFDRtests = function( stratSims,stratLength, sigMatrix,alpha)
{
 
 sortedSigMatrix = sigMatrix[order(sigMatrix$probability),c(1,2)]

 sortedSigMatrix

 fdrSig = 0
 fdrSigStrats <- NULL
 fdrpSigVector <- NULL
 n <- 1:stratSims
 sigSymbol <- NULL

 for(i in 1:length(sigMatrix$Strategy))
 {
   if( sortedSigMatrix$probability[i] < (i/stratSims * alpha))
   {
     
     fdrSig = fdrSig + 1
     fdrSigStrats = c(fdrSigStrats, sortedSigMatrix$Strategy[i])
     fdrpSigVector = c(fdrpSigVector, sortedSigMatrix$probability[i])
     sigSymbol = c(sigSymbol, '*')
   } else
   {
    
     fdrSigStrats = c(fdrSigStrats, sortedSigMatrix$Strategy[i])
     fdrpSigVector = c(fdrpSigVector, sortedSigMatrix$probability[i])
     sigSymbol = c(sigSymbol, '')
     
   }  
  
 }
 
 sigMatrixFDRtests = data.frame( Strategy = fdrSigStrats, probability = fdrpSigVector,Significance = sigSymbol, NumberOfSigStrats = fdrSig)
 
 return(sigMatrixFDRtests)

}

##Function to Get Monthly Returns------------------------------------------------

GetMonthlyReturns = function(stratSims,stratLength,returnsSim,stratMatrix)
{
  monthlyReturns <- NULL


  
    for(i in 1:stratLength){
    
      monthlyReturns[i] <- stratMatrix[i] * returnsSim[i]
    
    }
  
  
  return(monthlyReturns)
}

##Function to Get Monthly Expected Returns------------------------------------------------

GetExpectedReturns = function(stratSims,stratLength,returnsSim,pSuccess)
{
  pureExpStratReturns = 0
  
  
    for(k in 1:stratLength){
    pureExpStratReturns = pureExpStratReturns + (abs(returnsSim[k]) * pSuccess - abs(returnsSim[k]) * (1- pSuccess)) 
    }
  
  
  
  meanExpectedReturns = 0
  

    
  meanExpectedReturns = pureExpStratReturns/stratLength
    
    
  
  
  return(meanExpectedReturns)
  
}

##Function to Get Data Mining Bias------------------------------------------------

GetDMB = function(stratSims,meanStratReturns,meanExpectedReturns)
{
  dmBias = 0
  
    
  dmBias =  abs(meanStratReturns - meanExpectedReturns)
  
  
 return(dmBias)
  
}


##Function to perform WRC Tests------------------------------------------------
performWRCtests = function(stratSims,stratLength,meanStratReturns,monthlyReturns,stratMatrix)
{
 
 #wrcResults = performWRCtests(100,stratLength[y],maxStratReturns,monthlyReturns)  
  
 resamples = 100
 n <- 1:stratSims

 bootindex <- matrix(nrow = stratLength, ncol = resamples)

 for(i in 1:resamples){
  
    bootindex[,i] <- sample(1:stratLength, replace = T)
  
 }


 bootMean <- matrix(nrow = stratSims, ncol = resamples)

 maxBoot <- NULL

 bootReturns <- matrix(nrow = stratLength, ncol = resamples)


 for(j in 1:resamples){
  
   for(i in 1:stratSims){
    
     for(k in 1:stratLength){
      
       bootReturns[k,i] <- monthlyReturns[bootindex[k,j],i] * stratMatrix[k,i]
      
     }
    
     bootMean[i,j] = mean(bootReturns[,i])
    
   }
  
   maxBoot[j] = max(bootMean[,j]) 
 }


 wrcPvals <- 0

 for (i in 1:stratSims){
   count = 0
  
   for (j in 1:resamples){
     if( maxBoot[j] >= meanStratReturns[i]){
       count = count+1
     }
   }
  
   wrcPvals[i] = count/resamples
 }

 maxBoot
 
 wrcResults = data.frame( Strategy = n, Return = meanStratReturns, Probability = wrcPvals)

 return(wrcResults)
}


##Romano and Wolf StepM method---------------------------------------------------

performStepMtests = function(stratSims,stratLength,meanStratReturns,monthlyReturns,stratMatrix)
{
  
  sig = 1
  minStepMPvals <- NULL
  numbering <- NULL
  originalIndex = c(1:100)
  
   while ( all(is.na(stratMatrix)) == FALSE && sig > 0)
    {
        
        sig = 0
     
        resamples = 100
        n <- 1:stratSims
        
        bootindex <- matrix(nrow = stratLength, ncol = resamples)
        
        for(i in 1:resamples){
          
          bootindex[,i] <- sample(1:stratLength, replace = T)
          
        }
        
        
        bootMean <- matrix(nrow = (stratSims-sig), ncol = resamples)
        
        maxBoot <- NULL
        
        bootReturns <- matrix(nrow = stratLength, ncol = resamples)
        
        
        for(j in 1:resamples){
          
          for(i in 1:stratSims){
            
            for(k in 1:stratLength){
              
              bootReturns[k,i] <- monthlyReturns[bootindex[k,j],i] * stratMatrix[k,i]
              
            }
            
            bootMean[i,j] = mean(bootReturns[,i])
            
          }
          
          maxBoot[j] = max(bootMean[,j]) 
        }

      
        stepMPvals <- 0
        
        for (i in 1:stratSims){
          count = 0
          
          for (j in 1:resamples)
            {
            if( maxBoot[j] >= meanStratReturns[i])
              {
              count = count+1
            }
          }
          
          stepMPvals[i] = count/resamples
        }
        
     if(min(stepMPvals)<0.05)   
     {
    
      while( all(is.na(stratMatrix)) == FALSE && all(is.na(stepMPvals)) == FALSE && min(stepMPvals) < 0.05) 
      {  
           
            sig = sig + 1
            numbering = append(numbering,originalIndex[match(min(stepMPvals),stepMPvals)])
            minStepMPvals = append(minStepMPvals,min(stepMPvals))
            originalIndex <- originalIndex[-match(min(stepMPvals),stepMPvals)]
            stratMatrix <- as.matrix(stratMatrix[,-match(min(stepMPvals),stepMPvals)])
            stepMPvals <- stepMPvals[-match(min(stepMPvals),stepMPvals)]
            
            stratSims = stratSims - 1
             
      }
     }else
     {
       minStepMPvals = append(minStepMPvals,stepMPvals)
       numbering = append(numbering,originalIndex)
       sig = 0
       
     }
          
      
    
   }
  
    stepMResults = data.frame(Strategy = numbering, Probability = minStepMPvals)
   
    return(stepMResults)
  }
  
  
  
  
  ##Function to perform MPC Tests------------------------------------------------
  performMPCtests = function(stratSims,stratLength,meanStratReturns,monthlyReturns,stratMatrix)
  {
   resamples = 100
   n <- 1:stratSims
  
   mcpStratindex <- matrix(nrow = stratLength, ncol = resamples)
   mcpReturnindex <- matrix(nrow = stratLength, ncol = resamples)
  
   MCPReturns <- matrix(nrow = stratLength, ncol = stratSims)
  
   MCPMean <- matrix(nrow = stratSims, ncol = resamples)
  
  
   for(i in 1:resamples){
    
     mcpStratindex[,i] = sample(1:stratLength, replace = F)
     mcpReturnindex[,i] = sample(1:stratLength, replace = F)
    
   }
  
   maxMCP <- NULL
  
   for(j in 1:resamples){
    
     for(i in 1:stratSims){
      
       for(k in 1:stratLength){
         
         MCPReturns[k,i] <- stratMatrix[mcpStratindex[k,j],i] * monthlyReturns[mcpReturnindex[k,j],i]
        
       }
      
       MCPMean[i,j] = mean(MCPReturns[,i])
      
     }
    
     maxMCP[j] = max(MCPMean[,j]) 
   }
  
  
   mcpPvals <- 0
  
   for (i in 1:stratSims){
     count = 0
    
     for (j in 1:resamples){
       if( maxMCP[j] >= meanStratReturns[i]){
         count = count+1
       }
     }
    
     mcpPvals[i] = count/resamples
   }
  
   maxMCP
   
   mcpResults = data.frame( Strategy = n, Return = meanStratReturns, Probability = mcpPvals)

 return(mcpResults)
}

##Function to perform Hansen's test for SPA------------------------------------
performSPAtests = function(stratSims,stratLength,maxStratReturns,monthlyReturns,stratMatrix)
{
 resamples = 100
 n<-1:stratSims


 SPAMean <- matrix(nrow = stratSims, ncol = resamples)

 maxSPA <- NULL

 SPAReturns <- matrix(nrow = stratLength, ncol = resamples)
 SPAindex <- matrix(nrow = stratLength, ncol = resamples)

 w <- 0


 for(i in 1:stratSims){
  
   
   for(j in 1:resamples){
    
     SPAindex[,j] <- sample(1:stratLength, replace = T)
     
     for(k in 1:stratLength){
      
       SPAReturns[k,i] <- monthlyReturns[SPAindex[k,j],i] * stratMatrix[k,i]
      
     }
    
     SPAMean[i,j] = mean(SPAReturns[,i])
    
   }
  
 }

 wsum = 0

 for (i in 1:stratSims) 
 { 
   for(j in 1:resamples)
   {
     wsum = wsum + ((stratLength^(1/2) * SPAMean[i,j] - stratLength^(1/2)*maxStratReturns[i]))^(2) 
    
   }
  
   w[i] = (1/resamples) * wsum
   wsum = 0
 }

 tSPA <- matrix(nrow = stratSims, ncol = resamples)

for(i in 1:stratSims)
{
  for(j in 1:resamples)
  {  
    
     tSPA[i,j] = (stratLength^(1/2) * SPAMean[i,j]) / w[i]
    
   }
  
   maxSPA[i] = max(0,max(tSPA[i,]))
  
 }


 spaPvals <- 0
 stratSPA <- 0

 for (i in 1:stratSims){
  
   count = 0
   stratSPA[i] = (stratLength^(1/2) * maxStratReturns[i]) / w[i]
  
   for (j in 1:resamples){
    
     if( maxSPA[j] >= stratSPA[i]){
       count = count+1
     }
   }
  
   spaPvals[i] = count/resamples
 }

 maxSPA
 
 spaResults = data.frame( Strategy = n, Return = maxStratReturns, Probability = spaPvals)

 return(spaResults)

}

##Corradi & Swansons Test----------------------------------------------------------

performCStests = function(stratSims, stratLength, maxStratReturns, maxReturnsSim, stratMatrix, pSuccess)
{
  
  
  benchmean = rep(0, stratSims)
  benchvar = rep(0, stratSims)
  negstratReturns <- matrix(ncol = stratSims, nrow = stratLength)
  lossbenchmean = 0
  meanStratloss <- NULL
  varStratLoss <- NULL
  csStat <- NULL
  n<-1:stratSims
  
  resamples = 100
  
  for(i in 1:stratSims)
  {
    for(j in 1:stratLength) 
    {  
      negstratReturns[j,i] = maxReturnsSim[j,i] * stratMatrix[j,i] * (- 1)
      
    }
    
    benchmean[i] = mean(maxReturnsSim[,i])
    benchvar[i] = 1/(stratLength^(1/2)) * var(maxReturnsSim[,i])
    lossbenchmean[i] = benchmean[i] * (-1)
    meanStratloss[i] = mean(negstratReturns[,i])
    varStratLoss[i] = 1/(stratLength^(1/2)) * var(negstratReturns[,i])
    d = density(maxReturnsSim[,i])
    plot(d)
    
    csStat[i] = - stratLength^(1/2) * (pnorm(-maxStratReturns[i], lossbenchmean[i], (benchvar[i]^(1/2))) - pnorm(-maxStratReturns[i], meanStratloss[i], (varStratLoss[i]^(1/2))))
    
    
  }
  
  benchvar
  csStat
  resamples = 100
  
  csindex = matrix(ncol = resamples, nrow = stratLength)
  bootnegreturns = matrix(ncol = resamples, nrow = stratLength)
  bootmeans <- NULL
  maxboot <- NULL
  
  
  bootvarStratLoss <- NULL
  csbootStat <- NULL
  
  for (i in 1:stratSims)
  {
    
    csindex = matrix(ncol = resamples, nrow = stratLength)
    
    for(j in 1:resamples)
    {
      
      csindex[,j] <- sample(1:stratLength, replace = T)  
      
      for(k in 1:stratLength) 
      {
        
        bootnegreturns[k,j] =  maxReturnsSim[csindex[k,j],i] * stratMatrix[k,i] * (- 1)
        
      }
      
      bootmeans[j] = mean(bootnegreturns[,j])
      
    }
    
    maxboot[i] = min(bootmeans)
    
    bootvarStratLoss[i] = 1/(stratLength^(1/2)) * var(bootnegreturns[,match( min(bootmeans),bootmeans)])
    
    csbootStat[i] = - stratLength^(1/2) * (pnorm(maxboot[i], lossbenchmean[i], (benchvar[i]^(1/2))) - pnorm(maxboot[i], maxboot[i], (bootvarStratLoss[i]^(1/2))))
  }  
  
  csPvals <- NULL
  
  for (i in 1:stratSims){
    count = 0
    
    for (j in 1:resamples){
      if( csbootStat[j] >= csStat[i]){
        count = count+1
      }
    }
    
    csPvals[i] = count/resamples
  }
  
  
  
  CSResults = data.frame( Strategy = n, Return = maxStratReturns, Probability = csPvals)
  
  return(CSResults)
}


##Get Method Stats-----------------------------------------------------------------

GetMethodStats = function(methodpvals,k,n,j)
{
 sigcount = 0
  
 if(k == 1 | k== 4 | k == 5| k == 6 | k == 7 | k == 8) 
 { 
  
  for (i in 1:length(methodpvals))
  {
    if(methodpvals[i]<0.05)
    {
      
      sigcount = sigcount + 1
      
    }
  }  
}

    
 
 if(k == 2)
 {
   for (i in 1:length(methodpvals))
   {
     if(methodpvals[i]<(0.05/n))
     {
       
       sigcount = sigcount + 1
       
     }
   }   
} 
  
 
 if(k == 3)
 {
    
   sigcount = j[n]
    
 }
    
   
  
  methodstats = data.frame(SignificantCount = sigcount, Min = min(methodpvals), Median = median(methodpvals), Max = max(methodpvals), Mean = mean(methodpvals), 
                           Variance = var(methodpvals), Skewness = skewness(methodpvals), Kurtosis = kurtosis(methodpvals))
  
  return(methodstats)
}  




##---------------------------------------------------------------------------------
##Parameters:
pSuccess = c(0.25,0.4,0.5,0.6,0.75)  #P #for p in 
stratNos = c(1,10,25,50,100) #K #for k in
stratLength = c(12,60,120,180) #y #for t in
outliers = c(0,0.1,0.2) #Lambda #for l in
stdDiffusion = c(0.1,0.3,0.5) #Sigma #for v in
count = 1
z = 1
allparamters = list()
allpvals = list()
allstats = list()
allreturns = list()

max(stratNos)

match(max(stratNos),stratNos)

for(y in 1:4)
{  
  for(k in 1:5)
  {  
    for(p in 1:5)
    { 
      for(l in 1:3)
      {
        for(v in 1:3)
        { 
          
          
          print(stratLength[y])
          print(stratNos[k])
          print(pSuccess[p])
          print(outliers[l])
          print(stdDiffusion[v])
          
          
          stratMatrix = matrix(ncol = 100, nrow = stratLength[y])
          monthlyReturns = matrix(ncol = 100, nrow = stratLength[y])
          maxReturnsSim <- matrix(ncol = 100, nrow = stratLength[y])
          maxStratReturns <- NULL
          meanExpectedReturns <- NULL
          dmBias <- NULL
          
        
          for(j in 1:100)
          {
            
            t <- seq(1, 999, by = 1)
            
            data = SimulateJumpDiffusionMerton( 0, stdDiffusion[v], outliers[l], 0, 0.01, t, 1)
            
            
            t = c(t,1000)
            
            demeaned <- NULL
            
              for(i in 1:1000){
                
                demeaned[i] = data[i] - mean(data)
              }

            
            
            returns = demeaned
          
            dummystratMatrix = GenerateATRs( stratNos[k], stratLength[y], pSuccess[p], returns)
            returnsSim = ChopReturns(stratNos[k],stratLength[y],returns)
            meanStratReturns = GenerateMeanStratReturn(stratNos[k],stratLength[y],returnsSim,dummystratMatrix)
            maxStratReturns[j] =  max(meanStratReturns)
            
            stratMatrix[,j] <- dummystratMatrix[,match(max(meanStratReturns),meanStratReturns)]
            maxReturnsSim[,j] <- returnsSim
            
            
            monthlyReturns[,j] = GetMonthlyReturns(100,stratLength[y],maxReturnsSim[,j],stratMatrix[,j])
            meanExpectedReturns[j] = GetExpectedReturns(100,stratLength[y],maxReturnsSim[,j],pSuccess[p])
            
            dmBias[j] = GetDMB(stratNos[k],maxStratReturns[j],meanExpectedReturns[j])
          
          }
          
          
          
          sigMatrixTtests = PerformTtests(100,stratLength[y], maxStratReturns, monthlyReturns)
          sigMatrixFWERtests = PerformFWERtests(100,stratLength[y], maxStratReturns, monthlyReturns)
          sigMatrixFDRtests = PerformFDRtests(100,stratLength[y],sigMatrixTtests,0.025)
          
          
          
          wrcResults = performWRCtests(100,stratLength[y],maxStratReturns,maxReturnsSim,stratMatrix)
          mpcResults = performMPCtests(100,stratLength[y],maxStratReturns,maxReturnsSim,stratMatrix)
          spaResults = performSPAtests(100,stratLength[y],maxStratReturns,maxReturnsSim,stratMatrix)
          stepMresults = performStepMtests(100,stratLength[y],maxStratReturns,maxReturnsSim,stratMatrix)
          CSResults = performCStests(100,stratLength[y],maxStratReturns,maxReturnsSim,stratMatrix,pSuccess[p])
          
          orderedStepMResults <- stepMresults[order(stepMresults$Strategy),]
          
          options(scipen=999)
          
          parameterNames = c('PSuccess', 'StratSims', 'StratLength', 'Outliers' ,'Variance')
          
          dummyparameters = c(pSuccess[p],stratNos[k],stratLength[y],outliers[l],stdDiffusion[v])
          
          parameters = data.frame(Parameters = dummyparameters,row.names = parameterNames)
          
          numbering <- 1:100
          
          
          allResults = data.frame(StratNo = numbering, MeanReturn = maxStratReturns,
                                  ExpectedReturns = meanExpectedReturns,
                                  DataMiningBias =  dmBias,
                                  TtestsPvals = sigMatrixTtests$probability,
                                  SignificanceT = sigMatrixTtests$Significance,
                                  FWERTestsPvals = sigMatrixFWERtests$probability,
                                  SignificanceFWER = sigMatrixFWERtests$Significance,
                                  FDRTestNo = sigMatrixFDRtests$Strategy,
                                  FDRTestsPvals = sigMatrixFDRtests$probability, 
                                  SignificanceFDR = sigMatrixFDRtests$Significance,
                                  WRCTestsPvals = wrcResults$Probability,
                                  StepMPvals = orderedStepMResults$Probability,
                                  MPCTestsPvals = mpcResults$Probability, 
                                  SPATestsPvals = spaResults$Probability,
                                  CSTestsPvals = CSResults$Probability)
          
         
          
          Ttests = GetMethodStats(sigMatrixTtests$probability,1,0,0)
          FWER = GetMethodStats(sigMatrixFWERtests$probability,2,100,0)
          FDR = GetMethodStats(sigMatrixFDRtests$probability,3,100,sigMatrixFDRtests$NumberOfSigStrats)
          
          sigMatrixFDRtests$numberOfSigStrats
          
          WRC = GetMethodStats(wrcResults$Probability,4,0,0)
          StepM = GetMethodStats(orderedStepMResults$Probability,5,0,0)
          MCP = GetMethodStats(mpcResults$Probability,6,0,0)
          SPA = GetMethodStats(spaResults$Probability,7,0,0)
          CS = GetMethodStats(CSResults$Probability,8,0,0)
          
          statsSet = rbind(Ttests,FWER,FDR,WRC,StepM,MCP,SPA,CS)
          
          methodNames = c("Ttests","FWER","FDR","WRC","StepM","MCP","SPA","CS")
          
          methodstats = data.frame(statsSet,row.names = methodNames)
          
          write.xlsx(x = parameters, file = paste("results P",toString(p),"K",toString(k),"T",toString(y),"L",toString(l),"V",toString(v),".xlsx"),sheetName = "Parameters", row.names = TRUE)
          write.xlsx(x = allResults, paste("results P",toString(p),"K",toString(k),"T",toString(y),"L",toString(l),"V",toString(v),".xlsx"),sheetName = "Pvalsheet", row.names = FALSE,append = TRUE)
          write.xlsx(x = methodstats,paste("results P",toString(p),"K",toString(k),"T",toString(y),"L",toString(l),"V",toString(v),".xlsx"),sheetName = "StatsSheet", row.names = TRUE,append = TRUE)
       
          allparamters[[z]] = parameters
          allpvals[[z]] = data.frame(StratNo = numbering, TtestsPvals = sigMatrixTtests$probability,
                                    WRCTestsPvals = wrcResults$Probability,
                                    MPCTestsPvals = mpcResults$Probability,
                                    SPATestsPvals = spaResults$Probability,
                                    StepMPvals = orderedStepMResults$Probability,
                                    CSTestsPvals = CSResults$Probability)
          allstats[[z]] = methodstats
          
          allreturns[[z]] = data.frame(StratNo = numbering,
                                       MeanReturn = maxStratReturns,
                                       ExpectedReturns = meanExpectedReturns,
                                       DataMiningBias =  dmBias)
                                       
          
          z=z+1
        }
      }  
    }
        
  }    
}



dummystratMatrix[,match(max(meanStratReturns),meanStratReturns)]



maxStratReturns

z = z - 1 

##Plot AverageBias Against Probability Value----------------------------------------------------------------------------------

dummyProbability <- NULL
dummyProbabilities <- NULL
dummyAverageBias <- NULL
dummyallBias <- NULL
dummysd <- NULL

k=1

all

for(i in 1:z) #Number of runs
{
  if(allparamters[[i]]$Parameters[2] == 50) #Number of Strategies
  {  
   if(allparamters[[i]]$Parameters[3] == 60) #Strategy Length
   {  
     if(allparamters[[i]]$Parameters[4] == 0.1) #Outliers
      {
        if(allparamters[[i]]$Parameters[5] == 0.1) #Volatility of Market
        {  
          dummyProbability[k] = allparamters[[i]]$Parameters[1]
          
          for( j in 1:100)
          {  
          dummyProbabilities = append(dummyProbabilities,allparamters[[i]]$Parameters[1])
          }
          
          dummyallBias = append(dummyallBias, allreturns[[i]]$DataMiningBias)
          dummyAverageBias[k] = mean(allreturns[[i]]$DataMiningBias)
          dummysd[k] = sd(allreturns[[i]]$DataMiningBias)
          
          k = k + 1
        
        }
        
      }
    }
  }  
}

dummysd

dummysummary <- summary(dummyallBias)

dummyallprobframe = data.frame(Probability = dummyProbabilities, Bias = dummyallBias)
dummyaverageprobframe = data.frame(Probability = dummyProbability, AverageBias = dummyAverageBias, SD = dummysd)

dummyallprobframe
dummyaverageprobframe


p1 <- ggplot(data = dummyaverageprobframe, aes(x=Probability, y=AverageBias)) +
    ylim(0,0.175) +
    geom_line(data = dummyaverageprobframe, aes(x=Probability, y=AverageBias, colour='red')) +
    geom_errorbar(aes(ymin = AverageBias - SD, ymax = AverageBias + SD), width = 0.05, colour = 'darkgrey') +
    geom_point(data = dummyaverageprobframe, aes(x=Probability, y=AverageBias, colour='red')) +
    scale_colour_manual(name = '', 
                        values =c("red" = "red","darkblue"="darkblue", "green" = "green","yellow" = "yellow", 'black'='black',"orange"="orange"), 
                        labels = c('Average')) +
    theme_bw() 

   

p1 



##Plot AverageBias Against Number of Strategies Tested----------------------------------------------------------------------------------
dummyStrategyNumbers <- NULL
dummyAverageBias <- NULL
dummyvectorStrategyNumbers <- NULL
dummyallBias <- NULL
dummysd <- NULL

k=1

for(i in 1:z) #Number of runs
{
  if(allparamters[[i]]$Parameters[1] == 0.5) #Probability
  {  
    if(allparamters[[i]]$Parameters[3] == 60) #Strategy Length
    {  
      if(allparamters[[i]]$Parameters[4] == 0.1) #Outliers
      {
        if(allparamters[[i]]$Parameters[5] == 0.1) #Volatility of Market
        {  
          dummyStrategyNumbers[k] = allparamters[[i]]$Parameters[2]
          dummyAverageBias[k] = mean(allreturns[[i]]$DataMiningBias)
          
          dummysd[k] = sd(allreturns[[i]]$DataMiningBias)
          
          for( j in 1:100)
          {  
            dummyvectorStrategyNumbers = append(dummyvectorStrategyNumbers,allparamters[[i]]$Parameters[2])
          }
          dummyallBias = append(dummyallBias, allreturns[[i]]$DataMiningBias)
          
          
          k = k + 1
          
        }
        
      }
    }
  }  
}

dummyStratNosframe = data.frame(NumberofStratsTested = dummyStrategyNumbers, AverageBias = dummyAverageBias, SD = dummysd)


dummyallStratNosframe = data.frame(NumberofStratsTested =  dummyvectorStrategyNumbers, Bias = dummyallBias)


p2 <- ggplot(data = dummyStratNosframe, aes(x=NumberofStratsTested, y=AverageBias)) +
  ylim(0,0.175) +
  geom_line(data = dummyStratNosframe, aes(x=NumberofStratsTested, y=AverageBias, colour='red')) +
  geom_errorbar(aes(ymin = AverageBias - SD, ymax = AverageBias + SD),colour = 'grey') +
  geom_point(data = dummyStratNosframe, aes(x=NumberofStratsTested, y=AverageBias, colour='red')) +
  scale_colour_manual(name = '', 
                      values =c("red" = "red","darkblue"="darkblue", "green" = "green","yellow" = "yellow", 'black'='black',"orange"="orange"), 
                      labels = c('Average')) +
  theme_bw() 



p2 

##Plot AverageBias Against Strategy Length---------------------------------------------------------------------------------
dummyStratLength <- NULL
dummyAverageBias <- NULL
dummyvectorStratLength <- NULL
dummyallBias <- NULL
dummysd <- NULL

k=1

for(i in 1:z) #Number of runs
{
  if(allparamters[[i]]$Parameters[1] == 0.5) #Probability
  {  
    if(allparamters[[i]]$Parameters[2] == 50) #Number of Strategies
    {  
      if(allparamters[[i]]$Parameters[4] == 0.1) #Outliers
      {
        if(allparamters[[i]]$Parameters[5] == 0.1) #Volatility of Market
        {  
          dummyStratLength[k] = allparamters[[i]]$Parameters[3]
          dummyAverageBias[k] = mean(allreturns[[i]]$DataMiningBias)
          dummysd[k] = sd(allreturns[[i]]$DataMiningBias)
          
          for( j in 1:100)
          {  
            dummyvectorStratLength = append(dummyvectorStratLength,allparamters[[i]]$Parameters[3])
          }
          dummyallBias = append(dummyallBias, allreturns[[i]]$DataMiningBias)
          
          k = k + 1
          
        }
        
      }
    }
  }  
}

dummyStratLengthframe = data.frame(StrategyLength= dummyStratLength, AverageBias = dummyAverageBias, SD = dummysd)
dummyStratLengthframe

dummyallStratLengthframe = data.frame(StrategyLength =  dummyvectorStratLength, Bias = dummyallBias)


p3 <- ggplot(data = dummyStratLengthframe, aes(x=StrategyLength, y=AverageBias)) +
  ylim(0,0.175) +
  geom_line(data = dummyStratLengthframe, aes(x=StrategyLength, y=AverageBias, colour='red')) +
  geom_errorbar(aes(ymin = AverageBias - SD, ymax = AverageBias + SD), width = 10, colour = 'grey') +
  geom_point(data = dummyStratLengthframe, aes(x=StrategyLength, y=AverageBias, colour='red')) +
  scale_colour_manual(name = '', 
                      values =c("red" = "red","darkblue"="darkblue", "green" = "green","yellow" = "yellow", 'black'='black',"orange"="orange"), 
                      labels = c('Average')) +
  theme_bw() 



p3 

##Plot AverageBias Against Presence of Outliers---------------------------------------------------------------------------------
dummyOutliers <- NULL
dummyAverageBias <- NULL
dummyvectorOutliers <- NULL
dummyallBias <- NULL
dummysd <- NULL

k=1

for(i in 1:z) #Number of runs
{
  if(allparamters[[i]]$Parameters[1] == 0.5) #Probability
  {  
    if(allparamters[[i]]$Parameters[2] == 50) #Number of Strategies
    {  
      if(allparamters[[i]]$Parameters[3] == 60) #Strategy Length
      {
        if(allparamters[[i]]$Parameters[5] == 0.1) #Volatility of Market
        {  
          dummyOutliers[k] = allparamters[[i]]$Parameters[4]
          dummyAverageBias[k] = mean(allreturns[[i]]$DataMiningBias)
          dummysd[k] = sd(allreturns[[i]]$DataMiningBias)
          
          for( j in 1:100)
          {  
            dummyvectorOutliers= append(dummyvectorOutliers,allparamters[[i]]$Parameters[4])
          }
          dummyallBias = append(dummyallBias, allreturns[[i]]$DataMiningBias)
          
          k = k + 1
          
        }
        
      }
    }
  }  
}

dummyOutliersframe = data.frame(Lambda = dummyOutliers, AverageBias = dummyAverageBias, SD = dummysd)
dummyOutliersframe

dummyallOutliersframe = data.frame(Lambda =  dummyvectorOutliers, Bias = dummyallBias)


p4 <- ggplot(data = dummyOutliersframe, aes(x=Lambda, y=AverageBias)) +
  ylim(0,0.175) +
  geom_line(data = dummyOutliersframe, aes(x=Lambda, y=AverageBias, colour='red')) +
  geom_errorbar(aes(ymin = AverageBias - SD, ymax = AverageBias + SD), width = 0.02, colour = 'grey') +
  geom_point(data = dummyOutliersframe, aes(x=Lambda, y=AverageBias, colour='red')) +
  scale_colour_manual(name = '', 
                      values =c("red" = "red","darkblue"="darkblue", "green" = "green","yellow" = "yellow", 'black'='black',"orange"="orange"), 
                      labels = c('Average')) +
  theme_bw() 



p4 

##Plot AverageBias Against Market Volatility---------------------------------------------------------------------------------
dummyVol <- NULL
dummyAverageBias <- NULL
dummyvectorVol <- NULL
dummyallBias <- NULL
dummysd <- NULL

k=1

for(i in 1:z) #Number of runs
{
  if(allparamters[[i]]$Parameters[1] == 0.5) #Probability
  {  
    if(allparamters[[i]]$Parameters[2] == 50) #Number of Strategies
    {  
      if(allparamters[[i]]$Parameters[3] == 60) #Strategy Length
      {
        if(allparamters[[i]]$Parameters[4] == 0.1) #Outliers
        {  
          dummyVol[k] = allparamters[[i]]$Parameters[5]
          dummyAverageBias[k] = mean(allreturns[[i]]$DataMiningBias)
          dummysd[k] = sd(allreturns[[i]]$DataMiningBias)
          
          for( j in 1:100)
          {  
            dummyvectorVol= append(dummyvectorVol,allparamters[[i]]$Parameters[5])
          }
          dummyallBias = append(dummyallBias, allreturns[[i]]$DataMiningBias)
          
          
          k = k + 1
          
        }
        
      }
    }
  }  
}

dummyVolframe = data.frame(Volatility = dummyVol, AverageBias = dummyAverageBias, SD = dummysd)
dummyVolframe

dummyallVolframe = data.frame(Volatility =  dummyvectorVol, Bias = dummyallBias)


p5 <- ggplot(data = dummyVolframe, aes(x=Volatility, y=AverageBias)) +
  ylim(0,0.175) +
  geom_line(data = dummyVolframe, aes(x=Volatility, y=AverageBias, colour='red')) +
  geom_errorbar(aes(ymin = AverageBias - SD, ymax = AverageBias + SD), width = 0.02, colour = 'grey') +
  geom_point(data = dummyVolframe, aes(x=Volatility, y=AverageBias, colour='red')) +
  scale_colour_manual(name = '', 
                      values =c("red" = "red","darkblue"="darkblue", "green" = "green","yellow" = "yellow", 'black'='black',"orange"="orange"), 
                      labels = c('Average')) +
  theme_bw() 



p5 

##Plot Method Sig Strats Against Number of Strats Tested p = 0.5------------------------------------------------------------------------------------------------------
dummytTestsig <- NULL
dummyFWERsig <- NULL
dummyFDRsig <- NULL
dummyWRCsig <- NULL
dummyStepMsig <- NULL
dummyMCPsig <- NULL
dummySPAsig <- NULL
dummyCSsig <- NULL

dummyNumberTested <- NULL

k=1

allstats[[1]]

for(i in 1:z) #Number of runs
{
  if(allparamters[[i]]$Parameters[1] == 0.5) #Probability
  {  
      if(allparamters[[i]]$Parameters[3] == 60) #Strategy Length
      {
        if(allparamters[[i]]$Parameters[4] == 0.1) #Outliers
        {  
          if(allparamters[[i]]$Parameters[5] == 0.1) #Number of Strategies
          {  
          dummytTestsig[k] = allstats[[i]]$SignificantCount[1]
          dummyFWERsig[k] = allstats[[i]]$SignificantCount[2]
          dummyFDRsig[k] = allstats[[i]]$SignificantCount[3]
          dummyWRCsig[k] = allstats[[i]]$SignificantCount[4]
          dummyStepMsig[k] = allstats[[i]]$SignificantCount[5]
          dummyMCPsig[k] = allstats[[i]]$SignificantCount[6]
          dummySPAsig[k] = allstats[[i]]$SignificantCount[7]
          dummyCSsig[k] = allstats[[i]]$SignificantCount[8]
          dummyNumberTested[k] = allparamters[[i]]$Parameters[2]
          k = k + 1
          
        }
        
      }
    }
  }  
}

dummytTestsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummytTestsig)
dummyFWERsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyFWERsig)
dummyFDRsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyFDRsig)
dummyWRCsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyWRCsig)
dummyStepMsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyStepMsig)
dummyMCPsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyMCPsig)
dummySPAsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummySPAsig)
dummyCSsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyCSsig)

dummytTestsigframe
dummyFWERsigframe
dummyFDRsigframe
dummyWRCsigframe
dummyStepMsigframe
dummyMCPsigframe
dummySPAsigframe
dummyCSsigframe


p6 <- ggplot(data = dummytTestsigframe, aes(x= NumberTested), ylim = range(c(0,100))) +
  geom_line(data = dummytTestsigframe, aes(y=NumberOfSigStrats,  colour = "black"))  +
  geom_line(data = dummyFWERsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "darkblue")) +
  geom_line(data = dummyFDRsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "green")) +
  geom_line(data = dummyCSsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "lightblue")) +
  geom_line(data = dummyWRCsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "orange")) +
  geom_line(data = dummyStepMsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "pink")) +
  geom_line(data = dummyMCPsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "red")) +
  geom_line(data = dummySPAsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "yellow")) +
  scale_colour_manual(name = 'Methods', 
                      values =c("red" = "red","darkblue"="darkblue","lightblue" = "lightblue", "green" = "green","yellow" = "yellow", 'black'='black',"orange"="orange","pink" = "pink"), 
                      labels = c('T-Test','FWER','FDR','CS','WRC','StepM','MCP','SPA')) +
  theme_bw()  

p6 

##Plot Method Sig Strats Against Number of Strats Tested p = 0.6------------------------------------------------------------------------------------------------------
dummytTestsig <- NULL
dummyFWERsig <- NULL
dummyFDRsig <- NULL
dummyWRCsig <- NULL
dummyStepMsig <- NULL
dummyMCPsig <- NULL
dummySPAsig <- NULL
dummyCSsig <- NULL

dummyNumberTested <- NULL

k=1

allstats[[1]]

for(i in 1:z) #Number of runs
{
  if(allparamters[[i]]$Parameters[1] == 0.6) #Probability
  { 
    
    if(allparamters[[i]]$Parameters[3] == 60) #Strategy Length
    {
      if(allparamters[[i]]$Parameters[4] == 0.1) #Outliers
      {  
        if(allparamters[[i]]$Parameters[5] == 0.1) #Number of Strategies
        {  
          dummytTestsig[k] = allstats[[i]]$SignificantCount[1]
          dummyFWERsig[k] = allstats[[i]]$SignificantCount[2]
          dummyFDRsig[k] = allstats[[i]]$SignificantCount[3]
          dummyWRCsig[k] = allstats[[i]]$SignificantCount[4]
          dummyStepMsig[k] = allstats[[i]]$SignificantCount[5]
          dummyMCPsig[k] = allstats[[i]]$SignificantCount[6]
          dummySPAsig[k] = allstats[[i]]$SignificantCount[7]
          dummyCSsig[k] = allstats[[i]]$SignificantCount[8]
          dummyNumberTested[k] = allparamters[[i]]$Parameters[2]
          k = k + 1
          
        }
        
      }
    }
  }  
}

dummytTestsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummytTestsig)
dummyFWERsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyFWERsig)
dummyFDRsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyFDRsig)
dummyWRCsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyWRCsig)
dummyStepMsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyStepMsig)
dummyMCPsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyMCPsig)
dummySPAsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummySPAsig)
dummyCSsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyCSsig)

dummytTestsigframe
dummyFWERsigframe
dummyFDRsigframe
dummyWRCsigframe
dummyStepMsigframe
dummyMCPsigframe
dummySPAsigframe
dummyCSsigframe



p7 <- ggplot(data = dummytTestsigframe, aes(x= NumberTested), ylim = range(c(0,100))) +
  geom_line(data = dummytTestsigframe, aes(y=NumberOfSigStrats,  colour = "black"))  +
  geom_line(data = dummyFWERsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "darkblue")) +
  geom_line(data = dummyFDRsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "green")) +
  geom_line(data = dummyCSsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "lightblue")) +
  geom_line(data = dummyWRCsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "orange")) +
  geom_line(data = dummyStepMsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "pink")) +
  geom_line(data = dummyMCPsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "red")) +
  geom_line(data = dummySPAsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "yellow")) +
  scale_colour_manual(name = 'Methods', 
                      values =c("red" = "red","darkblue"="darkblue","lightblue" = "lightblue", "green" = "green","yellow" = "yellow", 'black'='black',"orange"="orange","pink" = "pink"), 
                      labels = c('T-Test','FWER','FDR','CS','WRC','StepM','MCP','SPA')) +
  theme_bw()  

p7 

##Plot Method Sig Strats Against Number of Strats Tested p = 0.75------------------------------------------------------------------------------------------------------
dummytTestsig <- NULL
dummyFWERsig <- NULL
dummyFDRsig <- NULL
dummyWRCsig <- NULL
dummyMCPsig <- NULL
dummySPAsig <- NULL
dummyCSsig <- NULL

dummyNumberTested <- NULL

k=1

allstats[[1]]

for(i in 1:z) #Number of runs
{
  if(allparamters[[i]]$Parameters[1] == 0.6) #Probability
  {  
    if(allparamters[[i]]$Parameters[3] == 60) #Strategy Length
    {
      if(allparamters[[i]]$Parameters[4] == 0.1) #Outliers
      {  
        if(allparamters[[i]]$Parameters[5] == 0.1) #Number of Strategies
        {  
          dummytTestsig[k] = allstats[[i]]$SignificantCount[1]
          dummyFWERsig[k] = allstats[[i]]$SignificantCount[2]
          dummyFDRsig[k] = allstats[[i]]$SignificantCount[3]
          dummyWRCsig[k] = allstats[[i]]$SignificantCount[4]
          dummyStepMsig[k] = allstats[[i]]$SignificantCount[5]
          dummyMCPsig[k] = allstats[[i]]$SignificantCount[6]
          dummySPAsig[k] = allstats[[i]]$SignificantCount[7]
          dummyCSsig[k] = allstats[[i]]$SignificantCount[8]
          dummyNumberTested[k] = allparamters[[i]]$Parameters[2]
          k = k + 1
          
        }
        
      }
    }
  }  
}

dummytTestsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummytTestsig)
dummyFWERsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyFWERsig)
dummyFDRsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyFDRsig)
dummyWRCsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyWRCsig)
dummyStepMsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyStepMsig)
dummyMCPsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyMCPsig)
dummySPAsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummySPAsig)
dummyCSsigframe = data.frame(NumberTested = dummyNumberTested, NumberOfSigStrats = dummyCSsig)

dummytTestsigframe
dummyFWERsigframe
dummyFDRsigframe
dummyWRCsigframe
dummyStepMsigframe
dummyMCPsigframe
dummySPAsigframe
dummyCSsigframe


p8 <- ggplot(data = dummytTestsigframe, aes(x= NumberTested), ylim = range(c(0,100))) +
  geom_line(data = dummytTestsigframe, aes(y=NumberOfSigStrats,  colour = "black"))  +
  geom_line(data = dummyFWERsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "darkblue")) +
  geom_line(data = dummyFDRsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "green")) +
  geom_line(data = dummyCSsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "lightblue")) +
  geom_line(data = dummyWRCsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "orange")) +
  geom_line(data = dummyStepMsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "pink")) +
  geom_line(data = dummyMCPsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "red")) +
  geom_line(data = dummySPAsigframe, aes(x=NumberTested , y=NumberOfSigStrats, colour = "yellow")) +
  scale_colour_manual(name = 'Methods', 
                      values =c("red" = "red","darkblue"="darkblue","lightblue" = "lightblue", "green" = "green","yellow" = "yellow", 'black'='black',"orange"="orange","pink" = "pink"), 
                      labels = c('T-Test','FWER','FDR','CS','WRC','StepM','MCP','SPA')) +
  theme_bw()  

p8 

##Tabulate Sig Strats for each ATR success rate for each method--------------------------------------------------------------
dummytTestsig <- NULL
dummyFWERsig <- NULL
dummyFDRsig <- NULL
dummyWRCsig <- NULL
dummyMCPsig <- NULL
dummySPAsig <- NULL
dummyCSsig <- NULL
dummyStepMsig <- NULL


dummyprob <- NULL

k=1

allstats[[1]]

for(i in 1:z) #Number of runs
{
    
    if(allparamters[[i]]$Parameters[2] == 50) #Number of Strategies
    {  
      if(allparamters[[i]]$Parameters[3] == 60) #Strategy Length
      {
        if(allparamters[[i]]$Parameters[4] == 0.1) #Outliers
        {  
          if(allparamters[[i]]$Parameters[5] == 0.5) #Volatility
          {  
            dummytTestsig[k] = allstats[[i]]$SignificantCount[1]
            dummyFWERsig[k] = allstats[[i]]$SignificantCount[2]
            dummyFDRsig[k] = allstats[[i]]$SignificantCount[3]
            dummyWRCsig[k] = allstats[[i]]$SignificantCount[4]
            dummyStepMsig[k] = allstats[[i]]$SignificantCount[5]
            dummyMCPsig[k] = allstats[[i]]$SignificantCount[6]
            dummySPAsig[k] = allstats[[i]]$SignificantCount[7]
            dummyCSsig[k] = allstats[[i]]$SignificantCount[8]
            dummyprob[k] = allparamters[[i]]$Parameters[1]
            k = k + 1
            
          }
          
        }
      }
    }
}

dummysigframek2 = data.frame(Probability = dummyprob, Ttest =dummytTestsig,
                           FWER = dummyFWERsig,
                           FDR = dummyFDRsig,
                           WRC = dummyWRCsig,
                           StepM = dummyStepMsig,
                           MCP = dummyMCPsig,
                           SPA = dummySPAsig,
                           CS = dummyCSsig)

dummysigframek2

print(xtable(dummysigframek2,digits=c(0,2,0,0,0,0,0,0,0,0)))

##Counting false discoveries------------------------------------------------------------------------------------------------
tTestfd = 0
FWERfd = 0
FDRfd = 0
WRCfd = 0
MCPfd = 0
SPAfd = 0
CSfd = 0
StepMfd = 0

tTestfdx = 0
FWERfdx = 0
FDRfdx = 0
WRCfdx = 0
MCPfdx = 0
SPAfdx = 0
CSfdx = 0
StepMfdx = 0


allstats[[76]]$SignificantCount

run = 0

for(i in 1:z) #Number of runs
{
  
  if(allparamters[[i]]$Parameters[1] == 0.25 | allparamters[[i]]$Parameters[1] == 0.4 | allparamters[[i]]$Parameters[1] == 0.5) #Number of Strategies
  {  
          run = run + 1
          tTestfd = tTestfd + allstats[[i]]$SignificantCount[1]
          FWERfd = FWERfd + allstats[[i]]$SignificantCount[2]
          FDRfd = FDRfd + allstats[[i]]$SignificantCount[3]
          WRCfd = WRCfd + allstats[[i]]$SignificantCount[4]
          StepMfd = StepMfd + allstats[[i]]$SignificantCount[5]
          MCPfd = MCPfd + allstats[[i]]$SignificantCount[6]
          SPAfd = SPAfd + allstats[[i]]$SignificantCount[7]
          CSfd = CSfd + allstats[[i]]$SignificantCount[8]
          
          for(k in 1:8)
          {
            if(allstats[[i]]$SignificantCount[k] > 5)
            {
              if(k == 1)
              {  
               tTestfdx = tTestfdx + (allstats[[i]]$SignificantCount[k] - 5)
              }
              if(k == 2)
              {  
                FWERfdx = FWERfdx + (allstats[[i]]$SignificantCount[k] - 5)
              }
              
              if(k == 3)
              {  
                FDRfdx = FDRfdx+ (allstats[[i]]$SignificantCount[k] - 5)
              }
              if(k == 4)
              {  
                WRCfdx = WRCfdx + (allstats[[i]]$SignificantCount[k] - 5)
              }
              if(k == 5)
              {  
                StepMfdx = StepMfdx + (allstats[[i]]$SignificantCount[k] - 5)
              }
              if(k == 6)
              {  
                MCPfdx = MCPfdx + (allstats[[i]]$SignificantCount[k] - 5)
              }
              if(k == 7)
              {  
                SPAfdx = SPAfdx + (allstats[[i]]$SignificantCount[k] - 5)
              }
              
              if(k == 8)
              {  
                CSfdx = CSfdx + (allstats[[i]]$SignificantCount[k] - 5)
              }
              
            }
          }  
          
  }
}

run

falsemethodNames = c("Ttests","FWER","FDR","WRC","StepM","MCP","SPA","CS")

falseframe = data.frame( Methods = falsemethodNames, 
                         FalseDiscoveries = c(tTestfd, FWERfd, FDRfd, WRCfd, StepMfd, MCPfd, SPAfd, CSfd),
                         UnexFalseDiscoveries = c(tTestfdx, FWERfdx, FDRfdx, WRCfdx, StepMfdx, MCPfdx, SPAfdx, CSfdx))

falseframe$FalseDiscoveries





##Plot AveragePval Against ART Success Rate----------------------------------------------------------------------------------

dummyProbability <- NULL
dummytTestp <- NULL
dummyWRCp <- NULL
dummyMCPp <- NULL
dummySPAp <- NULL
dummyStepMp <- NULL
dummyCSp <- NULL


k=1

allpvals[[1]]$WRCTestsPvals

for(i in 1:z) #Number of runs
{
  if(allparamters[[i]]$Parameters[2] == 50) #Number of Strategies
  {  
    if(allparamters[[i]]$Parameters[3] == 60) #Strategy Length
    {  
      if(allparamters[[i]]$Parameters[4] == 0.1) #Outliers
      {
        if(allparamters[[i]]$Parameters[5] == 0.1) #Volatility of Market
        {  
          dummyProbability[k] = allparamters[[i]]$Parameters[1]
          dummytTestp[k] = mean(allpvals[[i]]$TtestsPvals)
          dummyWRCp[k] = mean(allpvals[[i]]$WRCTestsPvals)
          dummyMCPp[k] = mean(allpvals[[i]]$MPCTestsPvals)
          dummySPAp[k] = mean(allpvals[[i]]$SPATestsPvals)
          dummyStepMp[k] = mean(allpvals[[i]]$StepMPvals)
          dummyCSp[k] = mean(allpvals[[i]]$CSTestsPvals)
          k = k + 1
          
        }
        
      }
    }
  }  
}

dummytTestpframe = data.frame(Probability = dummyProbability, AveragePValue = dummytTestp)
dummyWRCpframe = data.frame(Probability = dummyProbability, AveragePValue = dummyWRCp)
dummyMCPpframe = data.frame(Probability = dummyProbability, AveragePValue = dummyMCPp)
dummySPApframe = data.frame(Probability = dummyProbability, AveragePValue = dummySPAp)
dummyStepMpframe = data.frame(Probability = dummyProbability, AveragePValue = dummyStepMp)
dummyCSpframe = data.frame(Probability = dummyProbability, AveragePValue = dummyCSp)

dummytTestpframe
dummyWRCpframe
dummyMCPpframe
dummySPApframe
dummyStepMpframe
dummyCSpframe

p9 <- ggplot(data = dummytTestpframe, aes(x= Probability), ylim = range(c(0,1))) +
  geom_line(data = dummytTestpframe, aes(y=AveragePValue,  colour = "black"))  +
  geom_line(data = dummyCSpframe, aes(x=Probability , y=AveragePValue,  colour = "lightblue"))  +
  geom_line(data = dummyWRCpframe, aes(x=Probability , y=AveragePValue, colour = "orange")) +
  geom_line(data = dummyStepMpframe, aes(x=Probability , y=AveragePValue, colour = "pink")) +
  geom_line(data = dummyMCPpframe, aes(x=Probability , y=AveragePValue, colour = "red")) +
  geom_line(data = dummySPApframe, aes(x=Probability , y=AveragePValue, colour = "yellow")) +
  scale_colour_manual(name = 'Methods', 
                      values =c("red" = "red","darkblue"="darkblue", "lightblue" = "lightblue" ,"green" = "green","yellow" = "yellow", 'black'='black',"orange"="orange", "pink"="pink"), 
                      labels = c('T-Test','CS','WRC','StepM','MCP','SPA')) +
  theme_bw()  

p9 



##Plot Average Pval Against Number of Strats Tested----------------------------------------------------------------------------------
dummyNoStrats <- NULL
dummytTestp <- NULL
dummyWRCp <- NULL
dummyMCPp <- NULL
dummySPAp <- NULL
dummyStepMp <- NULL
dummyCSp <- NULL

k=1

for(i in 1:z) #Number of runs
{
  if(allparamters[[i]]$Parameters[1] == 0.5) #Probability
  {  
    if(allparamters[[i]]$Parameters[3] == 60) #Strategy Length
    {  
      if(allparamters[[i]]$Parameters[4] == 0.1) #Outliers
      {
        if(allparamters[[i]]$Parameters[5] == 0.1) #Volatility of Market
        {  
          dummyNoStrats[k] = allparamters[[i]]$Parameters[2]
          dummytTestp[k] = mean(allpvals[[i]]$TtestsPvals)
          dummyWRCp[k] = mean(allpvals[[i]]$WRCTestsPvals)
          dummyMCPp[k] = mean(allpvals[[i]]$MPCTestsPvals)
          dummySPAp[k] = mean(allpvals[[i]]$SPATestsPvals)
          dummyStepMp[k] = mean(allpvals[[i]]$StepMPvals)
          dummyCSp[k] = mean(allpvals[[i]]$CSTestsPvals)
          k = k + 1
          
        }
        
      }
    }
  }  
}

dummytTestpframe = data.frame(NumberofStrats = dummyNoStrats, AveragePValue = dummytTestp)
dummyWRCpframe = data.frame(NumberofStrats = dummyNoStrats, AveragePValue = dummyWRCp)
dummyMCPpframe = data.frame(NumberofStrats = dummyNoStrats, AveragePValue = dummyMCPp)
dummySPApframe = data.frame(NumberofStrats = dummyNoStrats, AveragePValue = dummySPAp)
dummyStepMpframe = data.frame(NumberofStrats = dummyNoStrats, AveragePValue = dummyStepMp)
dummyCSpframe = data.frame(NumberofStrats = dummyNoStrats, AveragePValue = dummyCSp)


dummytTestpframe
dummyWRCpframe
dummyMCPpframe
dummySPApframe
dummyStepMpframe
dummyCSpframe


p10 <- ggplot(data = dummytTestpframe, aes(x= NumberofStrats), ylim = range(c(0,1))) +
  geom_line(data = dummytTestpframe, aes(y=AveragePValue,  colour = "black"))  +
  geom_line(data = dummyCSpframe, aes(x=NumberofStrats, y=AveragePValue,  colour = "lightblue"))  +
  geom_line(data = dummyWRCpframe, aes(x=NumberofStrats , y=AveragePValue, colour = "orange")) +
  geom_line(data = dummyStepMpframe, aes(x=NumberofStrats , y=AveragePValue, colour = "pink")) +
  geom_line(data = dummyMCPpframe, aes(x=NumberofStrats , y=AveragePValue, colour = "red")) +
  geom_line(data = dummySPApframe, aes(x=NumberofStrats , y=AveragePValue, colour = "yellow")) +
  scale_colour_manual(name = 'Methods', 
                      values =c("red" = "red","darkblue"="darkblue", "lightblue" = "lightblue" ,"green" = "green","yellow" = "yellow", 'black'='black',"orange"="orange", "pink"="pink"), 
                      labels = c('T-Test','CS','WRC','StepM','MCP','SPA')) +
  theme_bw()  

p10 



##Plot Average Pval Against Strategy Length---------------------------------------------------------------------------------
dummyStratLength <- NULL
dummytTestp <- NULL
dummyWRCp <- NULL
dummyMCPp <- NULL
dummySPAp <- NULL
dummyStepMp <- NULL
dummyCSp <- NULL


k=1

for(i in 1:z) #Number of runs
{
  if(allparamters[[i]]$Parameters[1] == 0.5) #Probability
  {  
    if(allparamters[[i]]$Parameters[2] == 50) #Number of Strategies
    {  
      if(allparamters[[i]]$Parameters[4] == 0.1) #Outliers
      {
        if(allparamters[[i]]$Parameters[5] == 0.1) #Volatility of Market
        {  
          dummyStratLength[k] = allparamters[[i]]$Parameters[3]
          dummytTestp[k] = mean(allpvals[[i]]$TtestsPvals)
          dummyWRCp[k] = mean(allpvals[[i]]$WRCTestsPvals)
          dummyMCPp[k] = mean(allpvals[[i]]$MPCTestsPvals)
          dummySPAp[k] = mean(allpvals[[i]]$SPATestsPvals)
          dummyStepMp[k] = mean(allpvals[[i]]$StepMPvals)
          dummyCSp[k] = mean(allpvals[[i]]$CSTestsPvals)
          k = k + 1
          
        }
        
      }
    }
  }  
}

dummytTestpframe = data.frame(LengthOfStrat = dummyStratLength, AveragePValue = dummytTestp)
dummyWRCpframe = data.frame(LengthOfStrat = dummyStratLength, AveragePValue = dummyWRCp)
dummyMCPpframe = data.frame(LengthOfStrat = dummyStratLength, AveragePValue = dummyMCPp)
dummySPApframe = data.frame(LengthOfStrat = dummyStratLength, AveragePValue = dummySPAp)
dummyStepMpframe = data.frame(LengthOfStrat = dummyStratLength, AveragePValue = dummyStepMp)
dummyCSpframe = data.frame(LengthOfStrat = dummyStratLength, AveragePValue = dummyCSp)


dummytTestpframe
dummyWRCpframe
dummyMCPpframe
dummySPApframe
dummyStepMpframe
dummyCSpframe


p11 <- ggplot(data = dummytTestpframe, aes(x= LengthOfStrat), ylim = range(c(0,1))) +
  geom_line(data = dummytTestpframe, aes(y=AveragePValue,  colour = "black"))  +
  geom_line(data = dummyCSpframe, aes(x=LengthOfStrat, y=AveragePValue,  colour = "lightblue"))  +
  geom_line(data = dummyWRCpframe, aes(x=LengthOfStrat , y=AveragePValue, colour = "orange")) +
  geom_line(data = dummyStepMpframe, aes(x=LengthOfStrat , y=AveragePValue, colour = "pink")) +
  geom_line(data = dummyMCPpframe, aes(x=LengthOfStrat , y=AveragePValue, colour = "red")) +
  geom_line(data = dummySPApframe, aes(x=LengthOfStrat , y=AveragePValue, colour = "yellow")) +
  scale_colour_manual(name = 'Methods', 
                      values =c("red" = "red","darkblue"="darkblue", "lightblue" = "lightblue" ,"green" = "green","yellow" = "yellow", 'black'='black',"orange"="orange", "pink"="pink"), 
                      labels = c('T-Test','CS','WRC','StepM','MCP','SPA')) +
  theme_bw()  

p11 

##Plot Average Pval Against Presence of Outliers---------------------------------------------------------------------------------
dummyOutliers <- NULL
dummytTestp <- NULL
dummyWRCp <- NULL
dummyMCPp <- NULL
dummySPAp <- NULL
dummyStepMp <- NULL
dummyCSp <- NULL

k=1

for(i in 1:z) #Number of runs
{
  if(allparamters[[i]]$Parameters[1] == 0.5) #Probability
  {  
    if(allparamters[[i]]$Parameters[2] == 50) #Number of Strategies
    {  
      if(allparamters[[i]]$Parameters[3] == 60) #Strategy Length
      {
        if(allparamters[[i]]$Parameters[5] == 0.1) #Volatility of Market
        {  
          dummyOutliers[k] = allparamters[[i]]$Parameters[4]
          dummytTestp[k] = mean(allpvals[[i]]$TtestsPvals)
          dummyWRCp[k] = mean(allpvals[[i]]$WRCTestsPvals)
          dummyMCPp[k] = mean(allpvals[[i]]$MPCTestsPvals)
          dummySPAp[k] = mean(allpvals[[i]]$SPATestsPvals)
          dummyStepMp[k] = mean(allpvals[[i]]$StepMPvals)
          dummyCSp[k] = mean(allpvals[[i]]$CSTestsPvals)
          k = k + 1
          
          
        }
        
      }
    }
  }  
}

dummytTestpframe = data.frame(Lambda = dummyOutliers, AveragePValue = dummytTestp)
dummyWRCpframe = data.frame(Lambda = dummyOutliers, AveragePValue = dummyWRCp)
dummyMCPpframe = data.frame(Lambda = dummyOutliers, AveragePValue = dummyMCPp)
dummySPApframe = data.frame(Lambda = dummyOutliers, AveragePValue = dummySPAp)
dummyStepMpframe = data.frame(Lambda = dummyOutliers, AveragePValue = dummyStepMp)
dummyCSpframe = data.frame(Lambda = dummyOutliers, AveragePValue = dummyCSp)


dummytTestpframe
dummyWRCpframe
dummyMCPpframe
dummySPApframe
dummyStepMpframe
dummyCSpframe


p12 <- ggplot(data = dummytTestpframe, aes(x= Lambda), ylim = range(c(0,1))) +
  geom_line(data = dummytTestpframe, aes(y=AveragePValue,  colour = "black"))  +
  geom_line(data = dummyCSpframe, aes(x=Lambda , y=AveragePValue,  colour = "lightblue"))  +
  geom_line(data = dummyWRCpframe, aes(x=Lambda , y=AveragePValue, colour = "orange")) +
  geom_line(data = dummyStepMpframe, aes(x=Lambda , y=AveragePValue, colour = "pink")) +
  geom_line(data = dummyMCPpframe, aes(x=Lambda , y=AveragePValue, colour = "red")) +
  geom_line(data = dummySPApframe, aes(x=Lambda , y=AveragePValue, colour = "yellow")) +
  scale_colour_manual(name = 'Methods', 
                      values =c("red" = "red","darkblue"="darkblue", "lightblue" = "lightblue" ,"green" = "green","yellow" = "yellow", 'black'='black',"orange"="orange", "pink"="pink"), 
                      labels = c('T-Test','CS','WRC','StepM','MCP','SPA')) +
  theme_bw()  

p12 

##Plot Average Pval Against Market Volatility---------------------------------------------------------------------------------
dummyVol <- NULL
dummytTestp <- NULL
dummyWRCp <- NULL
dummyMCPp <- NULL
dummySPAp <- NULL
dummyStepMp <- NULL
dummyCSp <- NULL

allpvals[[1]]

k=1

for(i in 1:z) #Number of runs
{
  if(allparamters[[i]]$Parameters[1] == 0.5) #Probability
  {  
    if(allparamters[[i]]$Parameters[2] == 50) #Number of Strategies
    {  
      if(allparamters[[i]]$Parameters[3] == 60) #Strategy Length
      {
        if(allparamters[[i]]$Parameters[4] == 0.1) #Outliers
        {  
          dummyVol[k] = allparamters[[i]]$Parameters[5]
          dummytTestp[k] = mean(allpvals[[i]]$TtestsPvals)
          dummyWRCp[k] = mean(allpvals[[i]]$WRCTestsPvals)
          dummyMCPp[k] = mean(allpvals[[i]]$MPCTestsPvals)
          dummySPAp[k] = mean(allpvals[[i]]$SPATestsPvals)
          dummyStepMp[k] = mean(allpvals[[i]]$StepMPvals)
          dummyCSp[k] = mean(allpvals[[i]]$CSTestsPvals)
          k = k + 1
          
        }
        
      }
    }
  }  
}

dummytTestpframe = data.frame(Volatility = dummyVol, AveragePValue = dummytTestp)
dummyWRCpframe = data.frame(Volatility = dummyVol, AveragePValue = dummyWRCp)
dummyMCPpframe = data.frame(Volatility = dummyVol, AveragePValue = dummyMCPp)
dummySPApframe = data.frame(Volatility = dummyVol, AveragePValue = dummySPAp)
dummyStepMpframe = data.frame(Volatility = dummyVol, AveragePValue = dummyStepMp)
dummyCSpframe = data.frame(Volatility = dummyVol, AveragePValue = dummyCSp)


dummytTestpframe
dummyWRCpframe
dummyMCPpframe
dummySPApframe
dummyStepMpframe
dummyCSpframe


p13 <- ggplot(data = dummytTestpframe, aes(x= Volatility), ylim = range(c(0,1))) +
  geom_line(data = dummytTestpframe, aes(y=AveragePValue,  colour = "black"))  +
  geom_line(data = dummyCSpframe, aes(x=Volatility  , y=AveragePValue,  colour = "lightblue"))  +
  geom_line(data = dummyWRCpframe, aes(x=Volatility , y=AveragePValue, colour = "orange")) +
  geom_line(data = dummyStepMpframe, aes(x=Volatility , y=AveragePValue, colour = "pink")) +
  geom_line(data = dummyMCPpframe, aes(x=Volatility , y=AveragePValue, colour = "red")) +
  geom_line(data = dummySPApframe, aes(x=Volatility , y=AveragePValue, colour = "yellow")) +
  scale_colour_manual(name = 'Methods', 
                      values =c("red" = "red","darkblue"="darkblue", "lightblue" = "lightblue" ,"green" = "green","yellow" = "yellow", 'black'='black',"orange"="orange", "pink"="pink"), 
                      labels = c('T-Test','CS','WRC','StepM','MCP','SPA')) +
  theme_bw()  

p13 
