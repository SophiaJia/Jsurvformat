#' univariable logistic regression 
#' 
#'JB.logreg output the table with general univariable logistic regression result with  OR (95\% Confidence Interval),P value. This function only change the format of the output table.         
#'@param xvar Independent variable 
#'@param yvar Dependent variable range(0,1)
#'@param name independent variable name 
#'@return A formated output including OR(95\% Confidence Intervals), P value. 
#'@examples
#'JaGVHD_1y <- JB.logreg(D$mutation, D$aGVHD_1Y, 'Graded Acute')
#'@export 
#'@name JB.logreg
#' 
JB.logreg <- function(xvar, yvar, name, factorNY = FALSE){
        if (factorNY == FALSE){
                am.glm = glm(formula = yvar ~ xvar, family=binomial)
                OR  <- J.digit(exp(-coef(am.glm))[2],2)
        }
        if (factorNY == TRUE){
                am.glm = glm(formula = yvar ~ as.factor(xvar), family=binomial)
                OR  <- J.digit(exp(-coef(summary(am.glm)))[c(-1),2],2)
        }
        CL  <- exp(-confint(am.glm))
        LCL <- J.digit(CL[c(-1),2], 2)
        UCL <- J.digit(CL[c(-1),1], 2)
        OR95CI  <- paste(OR,'(',LCL,',',UCL,')')
        P   <- JS.p(summary(am.glm)$coef[(-1),4])
        out <- cbind(name, OR95CI, P)
        colnames(out) <- c('','Odds Ratio (95% CI)','P')
        return(out)
}



