#' A complete table for univariable log rank test survival analysis 
#' 
#'JS.uniLR output the table with total number, number of events, estimented survival(Time can be difinded) and p-value.
#'Note: this function can only be used for catagorical analysis.         
#'@param D A data.frame in which to interpret the variables 
#'@param Event The status indicator, normally 0=alive, 1=dead
#'@param Stime This is the follow up time
#'@param Svar A vector of group
#'@param groupn A text vector of the the group name for output 
#'@param month Time for survival estimation 
#'@param Rho a scalar parameter that controls the type of test. With 'rho = 0' this is the log-rank or Mantel-Haenszel test, and with 'rho = 1' it is equivalent to the Peto & Peto modification of the Gehan-Wilcoxon test.
#'@return A dataframe of output including Number of total patients, Number of Events, Estimited survival , P values.
#'@examples Rho 
#' JS.uniLR(Data, Event, Stime, Svars[i] , groupns[i], month, Rho)
#'
#'rtf output
#'rtf<-RTF("Table_survival.doc",width = 8.5, height = 11, font.size = 10, omi = c(1,1,1,1))
#'addHeader(rtf,title="Table , Survival Analysis  ")
#'addTable(rtf,isup_m.surv, font.size = 10, row.names = F, NA.string="-", col.widths = c(rep(0.6, 3), 2, rep(0.6, 3) ) )
#'done(rtf))
#'
#'Rmarkdown output
#'save(isup_m.surv, file='myfile.Rda')
#'
#'Then open at markdown file
#'library(knitr)
#'load("H:/Projects/myfile.Rda")
#'"```{r, echo=FALSE,results = 'asis'}"
#'output <- load("H:/Projects/p_Smith, Mitchell/Rituxan/Rituxan/T_BMI.Rdata")
#'knitr::kable(isup_m.surv, format = "markdown")  
#"```"
#'
#'@export 
#'@name JS.uni
#' 
JS.uniLR <- function (Data, Event, Stime, Svar, groupn, month, Rho = 0){
        #get factors;
        event <- as.numeric(Data[,match(Event, names(Data))])
        stime <- Data[,match(Stime, names(Data))]
        svar  <- Data[,match(Svar, names(Data))]
        .data <- data.frame (event, stime, svar)
        .data <- na.omit(.data)
        fit1     <- survdiff(Surv(stime, event) ~ svar, data = .data, rho = Rho)
        fit.p    <-  JS.p(pchisq(fit1$chisq, df= length(table(svar)) - 1, lower=FALSE))
        fit2     <- survfit(Surv(stime, event) ~ svar, data = .data )
        fit.y5sv <- summary(fit2, time = month)$surv
        .numberoflevel <- unique(svar[!is.na(svar)])
        if (length(fit.y5sv) == length(.numberoflevel)) {
                fit.y5se <- summary(fit2, time = month)$std.err
                fit.5y   <- paste(format(fit.y5sv, digits = 2), '\u00B1', format(fit.y5se, digits = 2))      
        } else if (length(fit.y5sv) != length(.numberoflevel)){
                .get <- cbind(summary(fit2, time = month)$surv, summary(fit2, time = month)$std.err, summary(fit2, time = month)$strata)
                .alltime <- cbind(summary(fit2)$surv, summary(fit2)$strata)
                .misslevel <- tmptime[!.timeall[,2] %in% c(summary(fit2, time = 60)$strata), ]
                .missleveltime <- tapply(.misslevel[,1], .misslevel[,2], max)
                .level <- as.numeric(names(.missleveltime))
                for (i in c(1 : length(.missleveltime))){
                        .time  <- .missleveltime[i]
                        .timealllevel <- cbind(summary(fit2, time = .time)$surv, summary(fit2, time = .time)$std.err, summary(fit2, time = .time)$strata)
                        .timeforlevel <- .timealllevel[.timealllevel[,3] == .level[i],c(1,2)]
                        .get <- rbind(.get, cbind(.timeforlevel[1], .timeforlevel[2], .level[i]))  
                        
                }
                .getsort <- .get[,order(.get[,3])]
                fit.5y <- paste(format(.getsort[,1], digits = 2), '\u00B1', format(.getsort[,2], digits = 2))
                
        }
        out <- cbind(fit1$n, fit1$obs,fit.5y)
        .p  <- c(fit.p, rep('',length(out[,1]) - 1))
        out <- cbind(rownames(out),out,.p)
        colnames(out) <- c("   ", "N", "No.Event", "estimated survival", "P-value")
        Gname <- c(groupn, rep( " ", 4))
        .surv.total <- rbind(Gname, out)
        return (.surv.total) 
}