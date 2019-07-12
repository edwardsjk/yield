#' A function to estimate yield and NNT from each strategy
#'
#' This function computes HIV testing yield for a specific strategy
#' @param data Input dataset with one record per individual not previously diagnosed with HIV
#' @param y name of variable with indicator that HIV test is positive
#' @param strategyind name of variable  indicating whether or not a specific person would be tested under a given strategy
#' @param strata name of variable with values of variable to be stratified by (defaults to NULL)
#' @param weight name of variable with respondent weights  (e.g., survey sampling weight), if applicable (defaults to NA)
#' @param cluster name of variable with cluster IDs, if applicable (defaults to NULL)
#' @export
#' @examples
#' yieldstrategy(data = mydata, y = "hiv", strategyind = "s1", strata = "sex", weight = "personweight", cluster = "venue")

yieldstrategy <- function(data, y, strategyind, strata = NULL, weight = "No Weights", cluster = "No Clusters"){
  require(survey)
  dat <- data
  # dat$y <- y
  # dat$str <- strategyind
  # dat$x <- strata
  # if(is.na(weight)) dat$weight <- rep(1, nrow(dat))
  # if(!is.na(weight)) dat$weight <- weight
  # dat$w <- weight
  # dat$cluster <- cluster
  dat$y <- data[,y]
  dat$str <- data[,strategyind]
  if(!is.null(strata)) dat$x <- data[,strata]
  if(weight == "No Weights") dat$weight <- rep(1, nrow(dat))
  if(weight != "No Weights") dat$weight <- data[ ,weight]
  if(!is.null(cluster) == 1) dat$cluster <- data[, cluster]
  dat <- dat[dat$str == 1, ]
 # cl <- ifelse(is.na(cluster), paste0(1), paste0("cluster"))
  if(cluster == "No Clusters") {
    survob <- svydesign(id=~1, weights=~weight, data=dat)
  }
  if(cluster != "No Clusters") {
    survob <- svydesign(id=~cluster, weights=~weight, data=dat)
  }
  svy <- svymean(~y, design = survob)
  pct <- unlist(coef(svy)) * 100
  se <- unlist(SE(svy)) * 100
  lcl <- pct - 1.96*se
  ucl <- pct + 1.96*se
  mean <- pct
  yield <- as.data.frame(cbind(mean, lcl, ucl))
  nnt <- 1/(yield/100)
  names(yield) <- c("yield", "lcl", "ucl")
  names(nnt) <- c("nnt", "nntucl", "nntlcl")
  results <- as.matrix(cbind(yield, nnt))
  results <- results[,c(1,2,3,4,6,5)]
  results
  return(results)
}



#' Estimate HIV testing yield under a number of strategies
#'
#' This function summarizes results across multiple strategies
#' @param data Input dataset
#' @param y indicator that HIV test is positive
#' @param strategyind name of variable indicating whether or not a specific person would be tested under a given strategy
#' @param strata name of variable to be stratified by (defaults to NULL)
#' @param weight name of weight variable, defaults to "No Weights"
#' @param cluster name of variable indicating clusters, defaults to "No Clusters"
#' @param stratlabels optional vector of strategy labels to improve readability
#' @export
#' @examples
#' calcyield(data = mydata, y = "hiv", strategies = c("str1", "str2", "str3"), stratlabels = c("All venues", "Some other strategy", "Another one"), weight="weight", cluster = "sitespotid")


calcyield<- function(data, y, strategies,  weight = "No Weights", cluster = "No Clusters", stratlabels = NULL){

   num <- length(strategies)
   res <- matrix(nrow = num, ncol = 6)
   for(i in 1:num){
     str <- strategies[i]
     #stri <- data[,str]
     res[i,] <- yieldstrategy(data = data, y = y, strategyind = str, weight = weight, cluster = cluster)
   }
  if(is.null(stratlabels)) rownames(res) <- strategies
   if (!is.null(stratlabels)) rownames(res) <- stratlabels
  #paste0("Strategy", row(res)[,1])#stratlabels
  colnames(res) <- c("Yield", "Yield.LCL", "Yield.UCL", "NNT", "NNT.LCL", "NNT.UCL")
  res2 <- as.data.frame(res)
  res2$Strategy.Name <- row.names(res)
  res2 <- res2[, c(7, 1, 2, 3, 4, 5, 6)]
  return(res2)
}


#' A function to plot expeceted HIV testing yield by strategy
#'
#' This function plots the number needed to test to identify 1 new (previously undiagnosed) case of HIV and 95% CI for each strategy
#' @param results Matrix of results from calcyield function
#' @export
#' @examples
#' plotyield(allresults)
#'

plotyield <- function(results){
  require(ggplot2)
  res <- results# as.data.frame(results)
  #res$label <- row.names(results)
  res$nntr <- round(res$NNT)
  fp <- ggplot(data=res, aes(x=Strategy.Name, y=NNT, ymin=NNT.LCL, ymax=NNT.UCL)) +
    geom_pointrange() +
    xlab("Strategy") + ylab("Number needed to test to find 1 new case of HIV (95% CI)") + #theme( legend.position=c(.9,.95), legend.title=element_blank())+
    #coord_cartesian(ylim=c(0, 150))+
    scale_y_continuous(trans='log', breaks = c(20, 40, 80, 160, 320))+
    coord_flip(ylim=c(10, 320)) +  # flip coordinates (puts labels on y axis)
    theme(axis.text.y = element_text(hjust=0)) +
    geom_text(data=res, aes(y=NNT, label = nntr, vjust=-0.75))# use a white background
  return(fp)
}

