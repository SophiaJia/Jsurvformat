#' Create a data frame suitable for plotting survival curves from a 'survfit' object.
#' 
#'createSurvivalFrame Create a data frame suitable for plotting survival curves from a 'survfit' object.         
#'@param Sfit A object from survfit
#'@return A dataframe  suitable for plotting survival curves
#'@examples
#'s1 <- survfit(Surv(as.numeric(D$surdate), D$censor_m) ~ D$isup_m, data = D) 
#'f.frame<-createSurvivalFrame(s1)
#'
#'ggplot(data=f.frame, aes(col = strata, group = strata)) + geom_step(aes(x = time, y = surv), direction = "hv", size = 1.2) + 
#'        geom_point(data = subset(f.frame, n.censor == 1), aes(x = time, y = surv), shape = "|", cex = 7) + theme_bw() +
#'        scale_color_discrete(name="  ",
#'                             breaks = c("D$isup_m=1", "D$isup_m=2","D$isup_m=3", "D$isup_m=4"),
#'                             labels = c("1", "2", "3", "4")) +
#'        scale_x_discrete(breaks = c(seq(0, 30, by=5)), labels = c(seq(0, 30, by=5))) +
#'        coord_cartesian(ylim = c(0, 1.05)) +
#'        theme(axis.text = element_text(size = 18),
#'              axis.title = element_text(size = 16,face = "bold"),
#'              plot.title = element_text(lineheight = .12, face = "bold", size = 15),
#'              legend.text = element_text(size = 18)) +
#'        labs(x = "
#'                  Time (Month)", y = "Survival
#'               ",size = rel(2))+
#'        ggtitle("(a) ISUP Grouping
#'          
#'          
#'
#'       ")
#'
#'@export 
#'@name createSurvivalFrame
#'@references  code written by Ramon Saccilotto
#'



createSurvivalFrame <- function(Sfit){
  # initialise frame variable
  f.frame <- NULL
  # check if more then one strata
  if(length(names(Sfit$strata)) == 0){
    # create data.frame with data from surv???t
    f.frame <- data.frame(time=Sfit$time, n.risk=Sfit$n.risk, n.event=Sfit$n.event, n.censor = Sfit
                          $n.censor, surv=Sfit$surv, upper=Sfit$upper, lower=Sfit$lower)
    # create ???rst two rows (start at 1)
    f.start <- data.frame(time=c(0, f.frame$time[1]), n.risk=c(Sfit$n, Sfit$n), n.event=c(0,0), 
                          n.censor=c(0,0), surv=c(1,1), upper=c(1,1), lower=c(1,1))
    # add ???rst row to dataset
    f.frame <- rbind(f.start, f.frame)
    # remove temporary data
    rm(f.start)
  } 
  else {
    # create vector for strata identi???cation
    f.strata <- NULL
    for(f.i in 1:length(Sfit$strata)){
      # add vector for one strata according to number of rows of strata
      f.strata <- c(f.strata, rep(names(Sfit$strata)[f.i], Sfit$strata[f.i]))
    }
    # create data.frame with data from surv???t (create column for strata)
    f.frame <- data.frame(time=Sfit$time, n.risk=Sfit$n.risk, n.event=Sfit$n.event, n.censor = Sfit
                          $n.censor, surv=Sfit$surv, upper=Sfit$upper, lower=Sfit$lower, strata=factor(f.strata))
    # remove temporary data
    rm(f.strata)
    # create ???rst two rows (start at 1) for each strata
    for(f.i in 1:length(Sfit$strata)){
      # take only subset for this strata from data
      f.subset <- subset(f.frame, strata==names(Sfit$strata)[f.i])
      # create ???rst two rows (time: 0, time of ???rst event)
      f.start <- data.frame(time=c(0, f.subset$time[1]), n.risk=rep(Sfit[f.i]$n, 2), n.event=c(0,0), 
                            n.censor=c(0,0), surv=c(1,1), upper=c(1,1), lower=c(1,1), strata=rep(names(Sfit$strata)[f.i],
                                                                                                 2))  
      # add ???rst two rows to dataset
      f.frame <- rbind(f.start, f.frame)
      # remove temporary data
      rm(f.start, f.subset)
    }
    # reorder data
    f.frame <- f.frame[order(f.frame$strata, f.frame$time), ]
    
    
    #rename row.names
    rownames(f.frame) <- NULL
  }
  # return frame
  return(f.frame)
}




