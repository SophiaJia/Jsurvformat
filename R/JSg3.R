#' General survival statistics for variable that has three groups  
#' 
#'JS.g3 output the table with general survival analysis result with HR(95\% Confidence Interval),P value      
#'@param ... arguments will be passed to coxph 
#'@param Gname  Name of the row , required 
#'@return A dataframe of coxph output including HR(95\% Confidence Interval), P value 
#'@examples
#'Model1 <- JS.g3(Surv(as.numeric(pd_surv), pd_censor) ~ as.factor(tr_group) , data = D, Gname = "Treatment")
#'Model2 <- JS.g3(Surv(as.numeric(pd_surv), pd_censor) ~ as.factor(tr_group) + age_m, data = D, Gname = "Treatment adjusted by age")
#'@export 
#'@name JS.g3
#'  


JS.g3 <- function(..., Gname)
{
        .fit <- coxph(...)
        .surv.cl <- summary(.fit)$conf.int
        .surv.p <- summary(.fit)$coefficients
        .surv.total <- cbind(paste(J.digit(.surv.cl[, 1], 2), 
                                   "(", J.digit(.surv.cl[, 3], 2), ",", J.digit(.surv.cl[, 4], 2), ")"), .surv.p[, 5])
        .surv.total[, 2] <- JS.p(as.numeric(.surv.total[, 2]))
        .surv.totaltrans <- cbind( c("1 (Ref)"), .surv.total[1,1], .surv.total[1,2], .surv.total[2,1], .surv.total[2,2])
        colnames(.surv.totaltrans) <- c('', rep( c("HR ( 95%CI )", "P"),2))
        rownames(.surv.totaltrans) <- Gname
        return (.surv.totaltrans)
}  