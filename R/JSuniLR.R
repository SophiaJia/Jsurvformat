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
#'JS.uni(D = D ,"pd_censor", "pd_surv" , "tr_group", "Treatment")
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
        fit.p    <-  JS.p(pchisq(fit1$chisq, df=2, lower=FALSE))
        fit2     <- survfit(Surv(stime, event) ~ svar, data = .data )
        fit.y5sv <- summary(fit2, time = month)$surv
        fit.y5se <- summary(fit2, time = month)$std.err
        fit.5y   <- paste(format(fit.y5sv, digits = 2), '\u00B1', format(fit.y5se, digits = 2))
        out <- cbind(fit1$n, fit1$obs,fit.5y)
        .p  <- c(fit.p, rep('',length(out[,1]) - 1))
        out <- cbind(rownames(out),out,.p)
        colnames(out) <- c("   ", "N", "No.Event", "estimated survival", "P-value")
        Gname <- c(groupn, rep( " ", 4))
        .surv.total <- rbind(Gname, out)
        return (.surv.total) 
}