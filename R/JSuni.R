#' A complete table for univariable survival analysis (C index, D index)
#' 
#'JS.uni output the table with general multivariable survival analysis result with Number of total patients,
#'Number of Events, HR (95\% Confidence Interval),P value, C index and D index. This function only change the format of the output table.
#'Note: c index and d index are from package survcomp.         
#'@param D A data.frame in which to interpret the variables 
#'@param Event The status indicator, normally 0=alive, 1=dead
#'@param Stime This is the follow up time
#'@param Svar A vector of group
#'@param groupn A text vector of the the group name for output 
#'@param Cat  logical, indicating whether or not Svar is a categorical varaible 
#'@param cindex logical, indicating whether or not cindex should be in the table
#'@param dindex logical, indicating whether or not dindex should be in the table
#'@param AIC logical, indicating whether or not AIC should be in the table
#'@return A dataframe of coxph output including Number of total patients, Number of Events, HRs (95\% Confidence Intervals), P values, C index and D index.
#'@examples
#'JS.uni(D = D ,"pd_censor", "pd_surv" , "tr_group", "Treatment" , Cat = T , cindex = F, dindex = F, AIC = F)
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
JS.uni <- function(Data , Event, Stime , Svar, groupn , Cat = F, cindex = F, dindex = F, AIC = F)
{    
        #get factors;
        event <- as.numeric(Data[,match(Event, names(Data))])
        stime <- Data[,match(Stime, names(Data))]
        svar  <- Data[,match(Svar, names(Data))]
        .data <- data.frame (event, stime, svar)
        .data <- na.omit(.data)
        
        # survival HR with 95%CL
        if (Cat == F){
                .fit <- coxph(Surv(as.numeric(stime), event) ~ svar , data = .data)
        }
        if (Cat == T){
                #data[complete.cases(match(Svar, names(Data))), ]
                .fit <- coxph(Surv(as.numeric(stime), event) ~ as.factor(svar) , data = .data)
        }
        #.fitd <- coxph.detail(.fit)
        
        .surv.cl <-summary(.fit)$conf.int
        .surv.p  <-summary(.fit)$coefficients
        
        # concordance index (Concordance = #all concordant pairs/#total pairs ignoring ties.) 
        if(cindex == T) {
                .surv.c       <- concordance.index(svar, surv.time = stime, surv.event = event, method="noether")
                .surv.c.index <- .surv.c $c.index 
        }
        # d index 
        if(dindex == T) {
                .surv.D <- D.index(x = svar, surv.time = stime, surv.event = event)
                .surv.D.index <- .surv.D $d.index 
        }
        
        #AIC
        if(AIC == T){
                .AIC = AIC(.fit)
        } 
        
        # combine into a table 
        .surv.total <- cbind(paste(format(.surv.cl[, 1], digits = 3), '(', format(.surv.cl[, 3], digits = 3), ',', format(.surv.cl[, 4], digits = 3),')' ),
                             .surv.p[, 5])
        .surv.total[, 2] <- JS.p(as.numeric(.surv.total[, 2]))
        if (Cat == T){
                .num <- length(.surv.total[,1])
                reference <- c( 'Reference ' , '')
                .surv.total <- rbind(reference, .surv.total) 
                
                #.surv.total <- cbind(.surv.total, cindex = c(format(.surv.c.index, digits = 3), rep(" ", .num)))
                #.surv.total <- cbind(.surv.total, dindex = c(format(.surv.D.index, digits = 3), rep(" ", .num)))
                num.event   <- table(svar, event)[, 2]
                .surv.total <- cbind( c( data.frame(num.event)[, 1]) , .surv.total)
                .surv.total <- cbind(data.frame(table(svar))$svar, data.frame(table(svar))$Freq, .surv.total)
                colnames(.surv.total) <- c("   ", "N", "No.Event", "HR ( 95%CI )", "P-value")
                Gname <- c(groupn, rep('', 4))
                .surv.total <- rbind(Gname, .surv.total)
        }
        if (Cat == F){
                num.event   <- table(event)[2]
                .surv.total <- cbind( c( data.frame(num.event)[, 1]) , .surv.total)
                .surv.total <- cbind('', as.character(length(svar)), .surv.total)
                colnames(.surv.total) <- c('', 'N', 'No.Event', 'HR ( 95%CI )', 'P-value')
                .surv.total[1] <- groupn
        }
        
        return(.surv.total)
}