#' A complete table for univariable survival analysis (C index, D index)
#' 
#'JS.uni output the table with general multivariable survival analysis result with Number of total patients,
#'Number of Events, HR (95\% Confidence Interval),P value, C index and D index. This function only change the format of the output table.
#'Note: c index and d index are from package survcomp.         
#'@param D A data.frame in which to interpret the variables 
#'@param event The status indicator, normally 0=alive, 1=dead
#'@param stime This is the follow up time
#'@param var A vector of group
#'@param GradeN A text vector of the the group name for output 
#'@return A dataframe of coxph output including Number of total patients, Number of Events, HRs (95\% Confidence Intervals), P values, C index and D index.
#'@examples
#'isup_m.surv <- JS.uni ( D = D , event = D$censor_m , stime = D$surdate , svar = D$isup_m, GradeN = "ISUP")  
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
#'output <- load("H:/Projects/myfile.Rda")
#'kable(output, format = "markdown")
#'
#'@export 
#'@name JS.uni
#' 
#'
JS.uni <- function(D , event, stime , svar, GradeN ) {    
    
    #delete row that has missing svar
    D <- D[complete.cases(as.numeric(D$FurhmanGrade)), ]
    
    # survival HR with 95%CL
    .fit <- coxph(Surv(as.numeric(stime), event) ~ as.factor(svar) , data = D)
    #.fitd <- coxph.detail(.fit)
    
    .surv.cl <-summary(.fit)$conf.int
    .surv.p  <-summary(.fit)$coefficients
    
    # concordance index (Concordance = #all concordant pairs/#total pairs ignoring ties.) 
    .surv.c       <- concordance.index(svar, surv.time = stime, surv.event = event, method="noether")
    .surv.c.index <- .surv.c $c.index 
    
    # d index 
    .surv.D <- D.index(x = svar, surv.time = stime, surv.event = event)
    .surv.D.index <- .surv.D $d.index 
    
    # combine into a table 
    .surv.total <- cbind(paste(format(.surv.cl[, 1], digits = 3), "(", format(.surv.cl[, 3], digits = 3), ",", format(.surv.cl[, 4], digits = 3),")" ),
                            .surv.p[, 5])
    .surv.total[, 2] <- JS.p(as.numeric(.surv.total[, 2]))
    .num <- length(.surv.total[,1])
    reference <- c(" " , " ")
    .surv.total <- rbind(reference, .surv.total) 
    
    .surv.total <- cbind(.surv.total, cindex = c(format(.surv.c.index, digits = 3), rep(" ", .num)))
    .surv.total <- cbind(.surv.total, dindex = c(format(.surv.D.index, digits = 3), rep(" ", .num)))
    num.event   <- table(svar, event)[, 1]
    .surv.total <- cbind( c( data.frame(num.event)[, 1]) , .surv.total)
    .surv.total <- cbind(data.frame(table(svar))$svar, data.frame(table(svar))$Freq, .surv.total)
    colnames(.surv.total) <- c("   ", "N", "No.Event", "HR ( 95%CI )", "P-value", "C index", "D index")
    name.grade <- c(GradeN, rep( " ", 6))
    .surv.total <- rbind(name.grade, .surv.total) 

}

