#' A complete table for multiple univariable survival analysis 
#' 
#'JS.uni_m output the table with general multivariable survival analysis result with Number of total patients,
#'Number of Events, HR (95\% Confidence Interval),P value. This function only change the format of the output table.
#'Note: c index and d index are from package survcomp.         
#'@param D A data.frame in which to interpret the variables 
#'@param Event The status indicator, normally 0=alive, 1=dead
#'@param Stime This is the follow up time
#'@param Svars A vector of variables 
#'@param groupns A text vector of the the group names for output 
#'@param Cats  a vector of logical elements indicating whether or not Svar is a categorical varaible 
#'@param EM  a logcial term, include estimated median survival nor not 
#'@return A dataframe of coxph output including Number of total patients, Number of Events, HRs (95\% Confidence Intervals), P values, C index and D index.
#'@examples
#'Event   <- c("pd_censor")
#'Stime   <- c("pd_surv")
#'Svars   <- c("tr_group", "age_m")
#'Cats    <- c(T, F)
#'Groupns <- c("Treatment", "Age")
#'JS.uni_m(D, Event, Stime, Svars, Cats, Groupns)
#'
#'@export 
#'@name JS.uni_m_fdr
#' 
#'
JS.uni_m_fdr <- function(Data , Event, Stime , Svars, Cats, Groupns, EM = F) {    
        rs.all <- NULL
        if (EM == F){
        for (i in 1:length(Svars))
        {
                rs <- JS.uni_fdr(Data, Event, Stime, Svars[i] , Groupns[i],  Cats[i])
                rs.all <- rbind(rs.all, rs)
        }
        }
        if (EM == T){
                for (i in 1:length(Svars))
                {
                        rs <- JS.uni_em(Data, Event, Stime, Svars[i] , Groupns[i],  Cats[i])
                        rs.all <- rbind(rs.all, rs)
                }
        }
        
        
        return(rs.all)
}