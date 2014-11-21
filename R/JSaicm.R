#' General survival statistics for multivariable analysis (AIC)
#' 
#'JS.aicm output the table with general multivariable survival analysis result with HR (95\% Confidence Interval),P value and Akaike's An Information Criterion. This function only change the format of the output table.      
#'@param ... arguments will be passed to coxph 
#'@return A dataframe of coxph output including Variable Names, HRs (95\% Confidence Intervals), P values and AIC
#'@examples
#'Model <- JS.aicm (Surv(as.numeric(surdate), scensor) ~ as.factor(isup_m_new) + age + gentder + stage , data = D1)
#'
#'rtf output
#'rtf<-RTF("Table_survival.doc",width = 8.5, height = 11, font.size = 10, omi = c(1,1,1,1))
#'addHeader(rtf,title="Table1, Survival Analysis ")
#'addTable(rtf, Model, font.size = 10, row.names = F, NA.string="-", col.widths = c(rep(1.5, 4) ) )
#'done(rtf)
#'
#'Rmarkdown output
#'save(Model, file='myfile.Rda')
#'
#'Then open at markdown file
#'output <- load("H:/Projects/myfile.Rda")
#'kable(output, format = "markdown")
#'
#'@export 
#'@name JS.aicm
#' 

JS.aicm <- function( ... ) {  
  ### this function only changed the output format 
  .fit <- coxph( ... )
  .surv.cl <-summary(.fit)$conf.int
  .surv.p  <-summary(.fit)$coefficients
  #AIC
  .AIC = AIC(.fit)
  # combine into a table 
  .surv.total <- cbind(paste(format(.surv.cl[, 1], digits = 3), "(", format(.surv.cl[, 3], digits = 3), ",", format(.surv.cl[, 4], digits = 3),")" ),
                       .surv.p[, 5])
   
  #modify the table 
  .surv.total[, 2] <- JS.p(as.numeric(.surv.total[, 2]))
  .num <- length(.surv.total[,1])
  .black <- c(" " , " ")
  .surv.total <- rbind(.black, .surv.total)
  .surv.total <- cbind(.surv.total, AIC = c(format(.AIC, digits = 6), rep(" ", .num)))
  .black <- c(" " , " ", " ")
  .num <- length(.surv.total[,1])
  .surv.total <- rbind(.black, .surv.total)
  .surv.total <- cbind(c("Model", rep(" ", .num)),row.names(.surv.total), .surv.total)
  colnames(.surv.total) <- c("   ", "Variables", "HR ( 95%CI )", "P-value", "AIC")
                       
  out<- .surv.total
}
