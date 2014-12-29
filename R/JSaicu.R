#' General survival statistics for univariable analysis (AIC)
#' 
#'JS.aicu output the table with general survival analysis result with HR(95\% Confidence Interval),P value and Akaike's An Information Criterion. This function only change the format of the output table.      
#'@param ... arguments will be passed to coxph 
#'@return A dataframe of coxph output including HR(95\% Confidence Interval), P value and AIC
#'@examples
#'Model_1 <- JS.aicu (Surv(as.numeric(surdate), scensor) ~ as.factor(isup_m_new) , data = D1)
#'Model_2 <- JS.aicu (Surv(as.numeric(surdate), scensor) ~ as.factor(FurhmanGrade_new), data = D2)
#'...
#'Model_5 <- JS.aicu (Surv(as.numeric(surdate), scensor) ~ as.factor(isup_m_new) + as.factor(Necrosis), data = D1)
#'output_f <- rbind(Model_1, Model_2, Model_3, Model_4, Model_5)
#'row.names(output_f) <- c(1:length(row.names(output_f)))
#'
#'rtf output
#'rtf<-RTF("Table_survival.doc",width = 8.5, height = 11, font.size = 10, omi = c(1,1,1,1))
#'addHeader(rtf,title="Table1, Survival Analysis ")
#'addTable(rtf, output_f, font.size = 10, row.names = F, NA.string="-", col.widths = c(rep(1.5, 4) ) )
#'done(rtf)
#'
#'Rmarkdown output
#'save(out,plot1, file='myfile.Rda')
#'
#'Then open at markdown file
#'library(knitr)
#'output <- load("H:/Projects/myfile.Rda")
#'kable(output, format = "markdown")
#'
#'@export 
#'@name JS.aicu
#'             
JS.aicu <- function( ... ) {  
  ### this function will only change the output format 
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
  reference <- c(" " , " ")
  .surv.total <- rbind(reference, .surv.total)
  .surv.total <- cbind(.surv.total, AIC = c(format(.AIC, digits = 6), rep(" ", .num)))
  colnames(.surv.total) <- c("HR ( 95%CI )", "P-value", "AIC")
  
  
  out<- .surv.total
}
