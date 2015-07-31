#' univariable logistic regression ( ordered )
#' 
#'JB.logreg_o output the table with ordered univariable logistic regression result with  OR (95\% Confidence Interval),P value. This function only change the format of the output table.         
#'@param xvar Independent variable 
#'@param yvar Dependent variable range (level > 2)
#'@param name independent variable name 
#'@return A formated output including OR(95\% Confidence Intervals), P value. 
#'@examples
#'JaGVHD_1y <- logreg(D$mutation, D$aGVHD_1Y, 'Graded Acute')
#'@export 
#'@name JB.logreg_o
#' 
JB.logreg_o <- function(xvar, yvar, name){
am.glm <- polr(as.factor(yvar) ~ xvar, Hess=TRUE)    
OR  <- J.digit(exp(coef(am.glm)), 2)
CL  <- exp(confint(am.glm))
LCL <- J.digit(CL[1], 2)
UCL <- J.digit(CL[2], 2)
OR95CI  <- paste(OR,'(',LCL,',',UCL,')')
ctable <- coef(summary(am.glm)) ## store table   
.P <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values        
P   <- JS.p(.P[1])
out <- cbind(name, OR95CI, P)
colnames(out) <- c('','Odds Ratio (95% CI)', 'P')
return(out)
}