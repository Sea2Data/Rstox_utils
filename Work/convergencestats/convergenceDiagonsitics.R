library(Rstox)
library(Reca)
setJavaMemory(size=10e+09)

projectname <- "ECA_NSSK_sei_2018"


setwd("~/workspace/stox/")

#
# Example for running Reca data prepped from StoX, and controlling parameters like the seed.
#
# If you want to run the stox project with only the parameters specified there, consider RstoX::runproject.
# The code below will use the parameters stored in the StoX project for the stox baseline-model, but will use provided parameters (or function defaults) for the stox r-model
# Stox baseline-model and stox r-model, refer to the workflows that you see in the StoX interface, labelled baseline and R
#


# runs stox baseline and prepares data for Reca
# result is saved to StoX directory rather than returned. Use Rstox::loadProjectData to retrieve them.
prepareRECA(projectname)
projectdata <- loadProjectData(projectname, var="prepareRECA")

# Data and parameters formatted for Reca, consult the documentation for Reca::eca.fit and Reca::eca.predict
AgeLength <- projectdata$prepareRECA$AgeLength
WeightLength <- projectdata$prepareRECA$WeightLength
Landings <- projectdata$prepareRECA$Landings
GlobalParameters <- projectdata$prepareRECA$GlobalParameters

# setting parameters needed for running ECA. Running only a few samples for testing purposes.
GlobalParameters$caa.burnin <- 0
GlobalParameters$burnin <- 20000
GlobalParameters$nSamples <- 5000
GlobalParameters$thin <- 10
GlobalParameters$fitfile <- "fit"
GlobalParameters$predictfile <- "pred"
GlobalParameters$lgamodel <- "log-linear"
GlobalParameters$CC <- F
GlobalParameters$CCerror <- F
GlobalParameters$age.error <- F
GlobalParameters$seed <- 42

#
# Actually running eca. Consult the documentation for Reca::eca.fit and Reca::eca.predict for interpreting the return objects.
#
fit <- eca.estimate(AgeLength,WeightLength,Landings,GlobalParameters)
pred <- eca.predict(AgeLength,WeightLength,Landings,GlobalParameters)

### Save the fitted object
save(fit, file=paste0(getwd(), "/project/", projectname, "/fit_seed", GlobalParameters$seed, ".Rdata"))

# Running mutiple chains? 
run_multiple_chains = TRUE
if (run_multiple_chains == TRUE)
{
  GlobalParameters$seed <- 20
  fit2 <- eca.estimate(AgeLength,WeightLength,Landings,GlobalParameters)
  save(fit2, file=paste0(getwd(), "/project/", projectname, "/fit_seed", GlobalParameters$seed, ".Rdata"))
  GlobalParameters$seed <- 10
  fit3 <- eca.estimate(AgeLength,WeightLength,Landings,GlobalParameters)
  save(fit3, file=paste0(getwd(), "/project/", projectname, "/fit_seed", GlobalParameters$seed, ".Rdata"))
}

##### Now testing a few things to add-on the different diagnostic plots for ECA models
str(fit)

library(coda)
library(dplyr)
All_chains_val <- list(fit$ProportionAtAge$Intercept$cov$constant, 
                       fit2$ProportionAtAge$Intercept$cov$constant, 
                       fit3$ProportionAtAge$Intercept$cov$constant)

test <- mcmc(as.matrix(fit$ProportionAtAge$Intercept$cov$constant[1,,]))
test1 <- mcmc(as.array(fit2$ProportionAtAge$Intercept$cov$constant[1,,]))
test2 <- mcmc(as.array(fit3$ProportionAtAge$Intercept$cov$constant[1,,]))
mclist <- mcmc.list(test,test1,test2)

traceplot(mclist)
gelman.diag(mclist)
gelman.plot(mclist)

####### Need to think about how to make a list for all ages groups
####### Need to think about how to group different fitted objects

## fit  -- eca fit object
## fit1 -- same eca model with different seed
## fit2 -- same eca model with another different seed
# Stox2coda <- function(m1,m2,m3) {
#   if (is.null(dim(m1))) {
#     mclist <- mcmc.list(mcmc(m1),mcmc(m2),mcmc(m3))
#   } else {
#     if (ncol(m1) > 1) mclist <- lapply(1:ncol(m1), function(var) {
#                                       lapply(1:nrow(m1), function(age) mcmc.list(mcmc(as.matrix(m1[age,var,])),mcmc(as.matrix(m2[age,var,])),mcmc(as.matrix(m3[age,var,]))))})
#     if (ncol(m1) == 1 & nrow(m1) > 1) mclist <- lapply(1:nrow(m1), function(age) mcmc.list(mcmc(as.matrix(m1[age,1,])),mcmc(as.matrix(m2[age,1,])),mcmc(as.matrix(m3[age,1,]))))
#     if (ncol(m1) == 1 & nrow(m1) == 1) mclist <- mcmc.list(mcmc(as.matrix(m1)),mcmc(as.matrix(m2)),mcmc(as.matrix(m3)))
#   }
#   return(mclist)
# }
# 
# 
# library(plotrix)  # for the listDepth function to find the maximum depth of a list
# 
# Stox2codaall <- function(m1,m2,m3) {
#   Max_list_depth <- listDepth(m1)
#   if(Max_list_depth == 4){
#     result <- lapply(seq_along(names(m1)), function(level1) {
#                     lapply(seq_along(head(names(m1[[level1]]),-1)), function(level2){
#                           lapply(seq_along(names(m1[[level1]][[level2]])), function(level3){
#                                 if (length(m1[[level1]][[level2]][[level3]]) == 0) list()
#                                 if (length(m1[[level1]][[level2]][[level3]]) == 1){
#                                   Stox2coda(m1[[level1]][[level2]][[level3]][[1]],
#                                             m2[[level1]][[level2]][[level3]][[1]],
#                                             m3[[level1]][[level2]][[level3]][[1]])
#                                 } 
#                                 if (length(m1[[level1]][[level2]][[level3]]) > 1){
#                                   lapply(seq_along(names(m1[[level1]][[level2]][[level3]])), function(level4){
#                                     Stox2coda(m1[[level1]][[level2]][[level3]][[level4]],
#                                                m2[[level1]][[level2]][[level3]][[level4]],
#                                                m3[[level1]][[level2]][[level3]][[level4]])})
#                                 }})})})}
#   return(result)
# }
# 
# 
# All_pars <- Stox2codaall(fit,fit2,fit3)
# 
# Nage = 20
# library(data.table)
# result <- (lapply(1:length(All_pars), function(level1) {
#   lapply(1:length(All_pars[[level1]]), function(level2){
#     lapply(1:length(All_pars[[level1]][[level2]]), function(level3){
#       if (length(All_pars[[level1]][[level2]][[level3]])==0) {
#         list()
#         }else{ 
#           if (TRUE %in% (is(All_pars[[level1]][[level2]][[level3]]) == "mcmc.list")) {
#             print(gelman.diag(All_pars[[level1]][[level2]][[level3]])[[1]][1] )
#           } else {
#             lapply(1:length(All_pars[[level1]][[level2]][[level3]]), function(level4){
#               if (length(All_pars[[level1]][[level2]][[level3]][[level4]]) == 1) {
#                 print(gelman.diag(All_pars[[level1]][[level2]][[level3]][[level4]])[[1]][1])
#               } else {
#                 if (listDepth(All_pars[[level1]][[level2]][[level3]][[level4]]) == 3) lapply(1:length(All_pars[[level1]][[level2]][[level3]][[level4]]), function(var) {
#                 lapply(1:length(All_pars[[level1]][[level2]][[level3]][[level4]][[1]]), function(age) gelman.diag(All_pars[[level1]][[level2]][[level3]][[level4]][[var]][[age]])[[1]][1])})
#                 if (listDepth(All_pars[[level1]][[level2]][[level3]][[level4]]) == 2) lapply(1:length(All_pars[[level1]][[level2]][[level3]][[level4]]), function(age) gelman.diag(All_pars[[level1]][[level2]][[level3]][[level4]][[age]])[[1]][1])
#               }
#       })}}})})}))
# 


### Calculating the Gelman-Rubin diagnostic test and saving the result into a data.frame
#result <- list()
result <- c() 

for (i in seq_along(names(fit)))
{
  #result[[i]] <- list()
  for (j in seq_along(head(names(fit[[i]]),-1)))
  {
    #result[[i]][[j]] <- list()
    for (k in seq_along(names(fit[[i]][[j]])))
    {
      #result[[i]][[j]][[k]] <- list()
      if (length(fit[[i]][[j]][[k]])>0)
      {
        for (l in seq_along(names(fit[[i]][[j]][[k]])))
        {
          #result[[i]][[j]][[k]][[l]] <- list()
          if(is.null(dim(fit[[i]][[j]][[k]][[l]]))) 
          {
            #result[[i]][[j]][[k]][[l]] <- gelman.diag(mcmc.list(mcmc(as.matrix(fit[[i]][[j]][[k]][[l]])),
            #                                          mcmc(as.matrix(fit2[[i]][[j]][[k]][[l]])),
            #                                          mcmc(as.matrix(fit3[[i]][[j]][[k]][[l]]))))[[1]][1]
            val <- gelman.diag(mcmc.list(mcmc(as.matrix(fit[[i]][[j]][[k]][[l]])),
                               mcmc(as.matrix(fit2[[i]][[j]][[k]][[l]])),
                               mcmc(as.matrix(fit3[[i]][[j]][[k]][[l]]))))[[1]][1]
            new <- c(names(fit)[i], head(names(fit[[i]]),-1)[j], names(fit[[i]][[j]])[k], names(fit[[i]][[j]][[k]])[l],
                     NA, NA, val) 
            result <- rbind(result, new)
          } else {  
            if(dim(fit[[i]][[j]][[k]][[l]])[2]==1 & dim(fit[[i]][[j]][[k]][[l]])[1]==1) 
            {
              #result[[i]][[j]][[k]][[l]] <- gelman.diag(mcmc.list(mcmc(as.matrix(fit[[i]][[j]][[k]][[l]])),
              #                                                    mcmc(as.matrix(fit2[[i]][[j]][[k]][[l]])),
              #                                                    mcmc(as.matrix(fit3[[i]][[j]][[k]][[l]]))))[[1]][1]
              val <- gelman.diag(mcmc.list(mcmc(as.matrix(fit[[i]][[j]][[k]][[l]])),
                                 mcmc(as.matrix(fit2[[i]][[j]][[k]][[l]])),
                                 mcmc(as.matrix(fit3[[i]][[j]][[k]][[l]]))))[[1]][1]
              new <- c(names(fit)[i], head(names(fit[[i]]),-1)[j], names(fit[[i]][[j]])[k], names(fit[[i]][[j]][[k]])[l],
                       NA, NA, val)
              result <- rbind(result, new)
            }
            if(dim(fit[[i]][[j]][[k]][[l]])[2]>1) 
            {
              for (m in 1:dim(fit[[i]][[j]][[k]][[l]])[2])
              {
                #result[[i]][[j]][[k]][[l]][[m]] <- list()
                if (dim(fit[[i]][[j]][[k]][[l]])[1]>1)
                {
                  for (n in 1:dim(fit[[i]][[j]][[k]][[l]])[1])
                  {
                    #  result[[i]][[j]][[k]][[l]][[m]][[n]] <- gelman.diag(mcmc.list(mcmc(fit[[i]][[j]][[k]][[l]][n,m,]),
                    #                                                      mcmc(fit2[[i]][[j]][[k]][[l]][n,m,]),
                    #                                                      mcmc(fit3[[i]][[j]][[k]][[l]][n,m,])))[[1]][1]
                    val <- gelman.diag(mcmc.list(mcmc(fit[[i]][[j]][[k]][[l]][n,m,]),
                                       mcmc(fit2[[i]][[j]][[k]][[l]][n,m,]),
                                       mcmc(fit3[[i]][[j]][[k]][[l]][n,m,])))[[1]][1]
                    new <- c(names(fit)[i], head(names(fit[[i]]),-1)[j], names(fit[[i]][[j]])[k], names(fit[[i]][[j]][[k]])[l],
                             m, paste0("age",n), val) 
                    result <- rbind(result, new)
                  }
                } else {
                  #result[[i]][[j]][[k]][[l]][[m]][[1]] <- gelman.diag(mcmc.list(mcmc(fit[[i]][[j]][[k]][[l]][1,m,]),
                  #                                                    mcmc(fit2[[i]][[j]][[k]][[l]][1,m,]),
                  #                                                    mcmc(fit3[[i]][[j]][[k]][[l]][1,m,])))[[1]][1]
                  val <- gelman.diag(mcmc.list(mcmc(fit[[i]][[j]][[k]][[l]][1,m,]),
                                     mcmc(fit2[[i]][[j]][[k]][[l]][1,m,]),
                                     mcmc(fit3[[i]][[j]][[k]][[l]][1,m,])))[[1]][1]
                  new <- c(names(fit)[i], head(names(fit[[i]]),-1)[j], names(fit[[i]][[j]])[k], names(fit[[i]][[j]][[k]])[l],
                           m, paste0("no_age_class"), val)
                  result <- rbind(result, new)
                }
              }  
            }
            if(dim(fit[[i]][[j]][[k]][[l]])[2]==1 & dim(fit[[i]][[j]][[k]][[l]])[1]>1) 
            {
              #result[[i]][[j]][[k]][[l]][[1]] <- list()
              for (n in 1:dim(fit[[i]][[j]][[k]][[l]])[1])
              {
                #result[[i]][[j]][[k]][[l]][[1]][[n]] <- gelman.diag(mcmc.list(mcmc(fit[[i]][[j]][[k]][[l]][n,1,]),
                #                                                    mcmc(fit2[[i]][[j]][[k]][[l]][n,1,]),
                #                                                    mcmc(fit3[[i]][[j]][[k]][[l]][n,1,])))[[1]][1]
                val <- gelman.diag(mcmc.list(mcmc(fit[[i]][[j]][[k]][[l]][n,1,]),
                                   mcmc(fit2[[i]][[j]][[k]][[l]][n,1,]),
                                   mcmc(fit3[[i]][[j]][[k]][[l]][n,1,])))[[1]][1]
                new <- c(names(fit)[i], head(names(fit[[i]]),-1)[j], names(fit[[i]][[j]])[k], names(fit[[i]][[j]][[k]])[l],
                         1, paste0("age",n), val) 
                result <- rbind(result, new)
              }
            }
          }  
        }
      } else {
        val <- NA
        #result[[i]][[j]][[k]][[l]] <- NA
        new <- c(names(fit)[i], head(names(fit[[i]]),-1)[j], names(fit[[i]][[j]])[k], NA, NA, NA, val)
        result <- rbind(result, new)
      }
    }
  }
}


colnames(result) <- c("Model", "Int_or_slope", "Var_type", "Var_name", "Var_level", "Age", "Rhat")
result <- as.data.frame(result)
result$Rhat <- as.numeric(as.character(result$Rhat))

Above1.1 <- which(result$Rhat > 1.1)
if (length(Above1.1)>=1)
{
  nn <- length(Above1.1)
  Most_problematic_Rhat_val <- sort(result$Rhat, decreasing =T)[1:min(9, nn)]
  Most_problematic_Rhat_loc <- order(result$Rhat, decreasing =T)[1:min(9, nn)]
  
  library(tidyr)
  par(mfrow=c(ifelse(nn>6,3,ifelse(nn>2,2,1)),ifelse(nn>4,3,ifelse(nn>1,2,1))), oma=c(3,4,3,1), mar=c(1,1,3,1))
  for (ijk in Most_problematic_Rhat_loc)
  {
    if (!is.na(as.character(result[ijk,5])) & !is.na(as.character(result[ijk,6]))) 
    {
      mclist <- mcmc.list(mcmc(fit[[as.character(result[ijk,1])]][[as.character(result[ijk,2])]][[as.character(result[ijk,3])]][[as.character(result[ijk,4])]][as.numeric(extract_numeric(result[ijk,6])),as.numeric(as.character(result[ijk,5])),]),
                          mcmc(fit2[[as.character(result[ijk,1])]][[as.character(result[ijk,2])]][[as.character(result[ijk,3])]][[as.character(result[ijk,4])]][as.numeric(extract_numeric(result[ijk,6])),as.numeric(as.character(result[ijk,5])),]),
                          mcmc(fit3[[as.character(result[ijk,1])]][[as.character(result[ijk,2])]][[as.character(result[ijk,3])]][[as.character(result[ijk,4])]][as.numeric(extract_numeric(result[ijk,6])),as.numeric(as.character(result[ijk,5])),]))
    }       
    if (is.na(as.character(result[ijk,5])) & !is.na(as.character(result[ijk,6]))) 
        {
          mclist <- mcmc.list(mcmc(fit[[as.character(result[ijk,1])]][[as.character(result[ijk,2])]][[as.character(result[ijk,3])]][[as.character(result[ijk,4])]][as.numeric(extract_numeric(result[ijk,6])),1,]),
                              mcmc(fit2[[as.character(result[ijk,1])]][[as.character(result[ijk,2])]][[as.character(result[ijk,3])]][[as.character(result[ijk,4])]][as.numeric(extract_numeric(result[ijk,6])),1,]),
                              mcmc(fit3[[as.character(result[ijk,1])]][[as.character(result[ijk,2])]][[as.character(result[ijk,3])]][[as.character(result[ijk,4])]][as.numeric(extract_numeric(result[ijk,6])),1,]))
    }       
    if (!is.na(as.character(result[ijk,5])) & is.na(as.character(result[ijk,6]))) 
    {
      mclist <- mcmc.list(mcmc(fit[[as.character(result[ijk,1])]][[as.character(result[ijk,2])]][[as.character(result[ijk,3])]][[as.character(result[ijk,4])]][1,as.numeric(as.character(result[ijk,5])),]),
                          mcmc(fit2[[as.character(result[ijk,1])]][[as.character(result[ijk,2])]][[as.character(result[ijk,3])]][[as.character(result[ijk,4])]][1,as.numeric(as.character(result[ijk,5])),]),
                          mcmc(fit3[[as.character(result[ijk,1])]][[as.character(result[ijk,2])]][[as.character(result[ijk,3])]][[as.character(result[ijk,4])]][1,as.numeric(as.character(result[ijk,5])),]))
    }       
    if (is.na(as.character(result[ijk,5])) & is.na(as.character(result[ijk,6]))) 
    {
      mclist <- mcmc.list(mcmc(fit[[as.character(result[ijk,1])]][[as.character(result[ijk,2])]][[as.character(result[ijk,3])]][[as.character(result[ijk,4])]]),
                          mcmc(fit2[[as.character(result[ijk,1])]][[as.character(result[ijk,2])]][[as.character(result[ijk,3])]][[as.character(result[ijk,4])]]),
                          mcmc(fit3[[as.character(result[ijk,1])]][[as.character(result[ijk,2])]][[as.character(result[ijk,3])]][[as.character(result[ijk,4])]]))
    }       
        
    traceplot(mclist, main=paste(paste(t(apply(result[ijk,-7],1,as.character)), collapse="_"), "Rhat =", round(result[ijk,7],2)))
  }
  mtext(side=1, "Number of iterations", outer=T, line=2)
  mtext(side=2, "Parameter values", outer=T, line=2)
  # mtext(side=3, paste0("STOP!!!! N=", length(Above1.1), " parameters (total=", nrow(result), ") have Rhat>1.1"), outer=T, line=0, cex=1.5, font=2)
  mtext(side=3, paste0("STOP!!!! Convergence problem. N=", length(Above1.1), " parameters have Rhat>1.1"), outer=T, line=0, cex=1.6, font=2)
} else {
  plot(0,0,type="n", axes=F, ann=FALSE)
  text(0,0,"ALL is GOOD! Continue the analysis", cex=2, col="red")
}






