#' Subset survival analysis 
#' 
#'JS.g3sub output the table with general multivariable survival analysis result with HR (95\% Confidence Interval),P value        
#'@param D A data.frame in which to interpret the variables 
#'@param Event Name of event varaible :the status indicator, normally 0=alive, 1=dead
#'@param Stime Name of the follow up time variable
#'@param Svar  Name of the comparing group variable 
#'@param Group Name of the subset group variable 
#'@param cutoff  value of the cut off   
#'@param nocut logical, if a cutoff is defined or not
#'@return A dataframe of coxph output including  HRs (95\% Confidence Intervals), P values
#'@examples
#'Event <- c("pd_censor")
#'Stime <- c("pd_surv")
#'Svar  <- c("tr_group")
#'Group <- c("age_m")
#'
#'T_age <- JS.g3sub(D, Event, Stime, Svar, "age_m", cutoff = 60, nocut = F)
#'T_BMI <- JS.g3sub(D, Event, Stime, Svar, Group)
#'save(T_age,T_BMI, file='myfile.Rda')
#'
#'Then write this in Rmarkdown 
#'```{r, echo=FALSE,results = 'asis'}
#'load("H:/Projects/p_Smith, Mitchell/Rituxan/Rituxan/myfile.Rda")
#'knitr::kable(T_age, format = "markdown", row.names = F)
#'```
#'
#'the second table 
#'```{r, echo=FALSE,results = 'asis'}
#'knitr::kable(T_BMI, format = "markdown", row.names = F)
#'```
#'@export 
#'@name JS.g3sub
#' 
#'
JS.g3sub <- function(Data, Event, Stime, Svar, Group, cutoff = NA, nocut = T)
{
        #Future
        ## missing data
        ## cutoff more then one 
        ## svar level more than one 
        
        #get variables 
        event <- Data[, match(Event, names(Data))]
        stime <- Data[, match(Stime, names(Data))]
        svar  <- Data[, match(Svar, names(Data))]
        gvar  <- Data[, match(Group, names(Data))]
        
        #check cut off
        #if (length(table(gvar)) > 10 and is.na(cutoff) and nocut == F) {
        #  stop("Cut off is need for continous varaible, or change nocut to TRUE")
        #  
        #}
        
        # Number of event and percent
        .nsvar <- table (svar)
        .nevent <- table(svar, event )[,2]
        nevent <- c(
                      paste(.nevent[1],"(", format((.nevent[1]/.nsvar[1]) * 100, digits = 3),"%)", sep = ""),
                      paste(.nevent[2],"(", format((.nevent[2]/.nsvar[2]) * 100, digits = 3),"%)", sep = ""),
                      " ", 
                      paste(.nevent[3],"(", format((.nevent[3]/.nsvar[3]) * 100, digits = 3),"%)", sep = ""),
                      " "
                   )       
        #univariable analysis 
        r1 <- JS.g3(Surv(as.numeric(stime), event) ~ as.factor(svar) , data = Data, Gname = Svar)
        
        #analysis adjusted by group variable
        .Gname = paste(Svar, "Adjusted by", Group, sep = '' )
        r2 <- JS.g3(Surv(as.numeric(stime), event) ~ as.factor(svar) + gvar , data = Data, Gname = .Gname)
        
        r.all <- rbind(nevent, r1, r2)
        #analysis adjusted by coninous variable with cut off
        if ( is.na(cutoff) == F)
        {
                gvar2 <- (gvar > cutoff) * 1
                .Gname = paste(Svar, "Adjusted by", Group, "(cut off =", cutoff, ")", sep = '' )
                r3 <- JS.g3(Surv(as.numeric(stime), event) ~ as.factor(svar) + gvar2 , data = Data, Gname = .Gname)
                r.all <- rbind(r.all, r3)
        }
        #subsets 
        r.all <- rbind(r.all, 'Subsets:' = '')
        gvar_m = gvar
        if ( is.na(cutoff) == F) {
                gvar_m = gvar2
        }
        
        subsets <- split(Data, gvar_m)
        for (i in 1 : length(subsets))
        {
                Data.sub <- subsets[[i]]
                event <- Data.sub[, match(Event, names(Data.sub))]
                stime <- Data.sub[, match(Stime, names(Data.sub))]
                svar  <- Data.sub[, match(Svar, names(Data.sub))]
                .Gname <- paste (Group, names(subsets)[i], sep = " ")
                rs <- JS.g3(Surv(as.numeric(stime), event) ~ as.factor(svar) , data = Data.sub, Gname = .Gname)
                r.all <- rbind(r.all, rs)          
        }
        
        return (r.all)
}
