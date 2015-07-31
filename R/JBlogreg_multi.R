#' A output table for multivariable logistic analysis 
#' 
#'JB.logreg_multi output the table  with  OR (95\% Confidence Interval),P value         
#'@param outx  polr output object 
#'@return A dataframe of output including Variable names,  ORs (95\% Confidence Intervals), P values
#'@examples
#'Rd <- glm(as.factor(renal_dysfunc) ~ age + gender_m1 + TotalAnthracycline + TotalARAC + IndNoPRBCTrans + IndNoPltTrans, data = D, family=binomial)
#'JB.logreg_multi(Rd)
#'@export 
#'@name JB.logreg_multi
#' 
#'
JB.logreg_multi <- function(outx){  
        OR  <- J.digit(exp(coef(outx)), 2)
        CL  <- exp(confint(outx))
        LCL <- J.digit(CL[ ,1], 2)
        UCL <- J.digit(CL[ ,2], 2)
        OR95CI  <- paste(OR,'(',LCL,',',UCL,')')
        ctable <- coef(summary(outx)) ## store table   
        .P <- ctable[,4]          
        P   <- JS.p(.P[1:length(UCL)])
        .out <- cbind(OR95CI, P)
        out <- .out[-1,]
        colnames(out) <- c('Odds Ratio (95% CI)', 'P')
        return(out)
}