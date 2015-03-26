#' A output table for multivariable logistic analysis 
#' 
#'JB.logreg_o_multi output the table  with  OR (95\% Confidence Interval),P value         
#'@param outx  polr output object 
#'@return A dataframe of output including Variable names,  ORs (95\% Confidence Intervals), P values
#'@examples
#'E2.aGVHD_tmp <- polr(as.factor(aGVHD) ~ mutation + age_ge60 + diseasestat_m2,, data = D, Hess=TRUE)
#'JB.logreg_o_multi(E2.aGVHD_tmp)
#'@export 
#'@name JB.logreg_o_multi
#' 
#'
JB.logreg_o_multi <- function(outx){  
        OR  <- format(exp(coef(outx)), digits = 2)
        CL  <- exp(confint(outx))
        LCL <- format(CL[ ,1], digits = 2)
        UCL <- format(CL[ ,2], digits = 2)
        OR95CI  <- paste(OR,'(',LCL,',',UCL,')')
        ctable <- coef(summary(outx)) ## store table   
        .P <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values        
        P   <- JS.p(.P[1:length(UCL)])
        out <- cbind(OR95CI, P)
        colnames(out) <- c('Odds Ratio (95% CI)', 'P')
        return(out)
}