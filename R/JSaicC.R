#' General survival statistics for multivariable analysis (AIC & C index)
#' 
#'JS.aicC output the table with general multivariable survival analysis result with HR (95\% Confidence Interval),P value and Akaike's An Information Criterion. This function only change the format of the output table.      
#'@param ... arguments will be passed to coxph 
#'@return A dataframe of coxph output including Variable Names, HRs (95\% Confidence Intervals), P values, AIC and C index
#'@examples
#'Model <- JS.aicC (Surv(as.numeric(surdate), scensor) ~ as.factor(isup_m_new) + age + gentder + stage , data = D1)
#'
#'rtf output
#'rtf<-RTF("Table_survival.doc",width = 8.5, height = 11, font.size = 10, omi = c(1,1,1,1))
#'addHeader(rtf,title="Table1, Survival Analysis ")
#'addTable(rtf, Model, font.size = 10, row.names = F, NA.string="-", col.widths = c(rep(1.5, 6) ) )
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
#'@name JS.aicC
#' 

JS.aicC <- function( ... ) {  
  ### this function only changed the output format 
  .fit <- coxph( ... )
  .surv.cl <-summary(.fit)$conf.int
  .surv.p  <-summary(.fit)$coefficients
  
  # c index
  c_index <- summary(.fit)$concordance[1]
  
  #AIC
  .AIC = AIC(.fit)
  # combine into a table 
  .surv.total <- cbind(paste(J.digit(.surv.cl[, 1], 2), "(", J.digit(.surv.cl[, 3], 2), ",", J.digit(.surv.cl[, 4], 2),")" ),
                       .surv.p[, 5])
  
  #modify the table 
  .surv.total[, 2] <- JS.p(as.numeric(.surv.total[, 2]))
  .num <- length(.surv.total[,1])
  .black <- c(" " , " ")
  .surv.total <- rbind(.black, .surv.total)
  .surv.total <- cbind(.surv.total, AIC = c(J.digit(.AIC, 6), rep(" ", .num)),C_index = c(J.digit(c_index, 3), rep(" ", .num)))
  .black <- c(" " , " ", " ", " ")
  .num <- length(.surv.total[,1])
  .surv.total <- rbind(.black, .surv.total)
  .surv.total <- cbind(c("Model", rep(" ", .num)),row.names(.surv.total), .surv.total)
  colnames(.surv.total) <- c("   ", "Variables", "HR ( 95%CI )", "P-value", "AIC", "C Index")
                       
  out<- .surv.total
}
