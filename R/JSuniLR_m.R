#' A complete table for multiple univariable survival analysis by using log rank test
#' 
#'JS.uniLR_m output the table with general multivariable survival analysis result with Number of total patients,
#'Number of Events, Estimited Survival,P value. This function only change the format of the output table.        
#'@param D A data.frame in which to interpret the variables 
#'@param Event The status indicator, normally 0=alive, 1=dead
#'@param Stime This is the follow up time
#'@param Svars A vector of variables 
#'@param groupns A text vector of the the group names for output 
#'@param month Time for estimiated survival in month
#'@return A dataframe of log rank test output including Number of total patients, Number of Events, Estimated Survival ,P values
#'@examples
#'Event   <- c("pd_censor")
#'Stime   <- c("pd_surv")
#'Svars   <- c("tr_group", "age_m")
#'Groupns <- c("Treatment", "Age")
#'JS.uniLR_m(D,  Event, Stime, Svars, Groupns, 60)
#'
#'@export 
#'@name JS.uniLR_m
#' 
#'

JS.uniLR_m <- function (Data, Event, Stime, Svars, groupns, month){       
        rs.all <- NULL
        for (i in 1:length(Svars))
        {
                rs <- JS.uniLR(Data, Event, Stime, Svars[i] , groupns[i], month)
                rs.all <- rbind(rs.all, rs)
        }
        return(rs.all)
}
