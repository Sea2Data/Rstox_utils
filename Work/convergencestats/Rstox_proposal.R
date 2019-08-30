#
# Draft for including the analysis in convergenceDiagonstics.R as a function in Rstox
#
# for testability, we'd like to split it up in several functions
# for dependency management, we'd like to consider if we can reimplement the coda functionality, or find the same functionality in a library already included.
# consider also wrp to other plans for bayesian analysis in Rstox
#


#' Calculate potential Rhat for Gelman-Rubin test of a scalar parameter
#' @param chainCount scalar: the number of chains
#' @param chainLength scalar: the length of the chains
#' @param chainMeans vector: mean of the parameter for each chain
#' @param chainSigmasq vector: variance of the parameter for each chain
#' @internal
calcRhat <- function(chainLength, chainmeans, chainSigmasq){
  if (length(chainmeans)!=length(chainSigmasq)){
    stop("length of chainmeans must equal length of chainSignmaSq")
  }
  if (length(chainmeans)<3){
    stop("Need at least 3 chains")
  }
  chainCount <- lenght(chainmeans)
  overallMean <- mean(chainmeans)
  btwChainVar <- sum((chainmeans-overallMean)**2)*(chainLength/(chainCount)-1)
  winChainVar <- mean(chainSigmasq)
  
  pooledvar <- ((chainlength-1)/chainlength)*winChainVar + ((chainCount+1)/chainCount*chainLength)*btwChainVar
  stop("have not figured out how to do this. Maybe worth including dependency after all")
  var_of_pooledvar <- "?" # have not figured out how to do this. Maybe need dependency after all
  dof <-  2*pooledvar^2/var_of_pooledvar
  
  psrf <- pooledvar/winChainVar
  
  rhat <- sqrt((dof+3)*pooledvar/((dof+1)*winChainVar))
  
  return(rhat)
}


#' Extracts statistics of scalar parameters from an ECA fit object.
#' @param fit eca fit objecy as returned from eca.estimate
#' @param model the model to extract parameter statistics from
#' @param FUN a function mapping a vector of parametert iterations to the desired statistic
#' @value An object formatted as the model objects on the eca fit object, but with a scalar statistic in place of the mcmc replicates.
#' @internal
extractStat <- function(fit, model, FUN=mean){
  
}

#' Performs Gelman-Rubin convergence check on a given Reca configuration.
#' @details 
#'  This convergence check calculates the ratio (Rhat) of the estimated marginal posterior variance of each parameter fitted, 
#'  and the corresponding average within-chain variances. Convergence issues are flagged if this ratio exceeds a given threshold (Rhat_threshol).
#' 
#'  If an eca run is already available in project, an additional replicates-1 runs will be performed
#'  Additional runs will be assinged a random seed, even if seed is set as fixed in the project.
#'  To fix replicate seeds, use the parameter replicate_seeds. These should match the number of replicates,
#'  the first one will be ignored if an eca run is available in the project.
#' @param projectname identifies the stox project for which the convergence check should be performed.
#' @param replicates total number chains to run with the same configuration
#' @param replicate_seeds vector of seeds for pseudorandom number generator for replicates. These must be distinct.
#' @param Rhat_threshold upper limit of Rhat values for parameters that are considered converged.
convergenceCheckReca <- function(projectname, replicates, replicate_seeds=NULL, Rhat_threshold=1.1){
  
  if (length(unique(replicate_seeds))!=length(replicate_seeds)){
    stop("All seeds must be distinct")
  }
  if (length(unique(replicate_seeds))!=replicates){
    stop("Number of seeds must match number of replicates. If a run already exists, the first seed will be ignored.")
  }
  
  
  
  # for each chain
  ## for each model
  ### extract mean and sigmasq and store: extractStat
  
  # for each model
  ## for each parameter
  ### calculate Gelman-Rubin and store
  
  # sort by Rhat
  ## report Rhats larger than threshold in ascending order
  
}