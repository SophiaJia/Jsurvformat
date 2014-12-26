#' A output table for multivariable survival analysis 
#' 
#'JS.multi output the table with general multivariable survival analysis result with  HR (95\% Confidence Interval),P value, C index, D index and AIC. This function only change the format of the output table.
#'Note: c index and d index are from package survcomp.         
#'@param ... arguments will be passed to coxph 
#'@param cindex logical, required,  indicating whether or not cindex should be in the table
#'@param dindex logical, required, indicating whether or not dindex should be in the table
#'@param AIC logical, required, indicating whether or not AIC should be in the table
#'@return A dataframe of coxph output including Variable names,  HRs (95\% Confidence Intervals), P values,( C index, D index and AIC )
#'@examples
#'JS.survform (Surv(as.numeric(pd_surv), pd_censor) ~ as.factor(tr_group) + as.factor(BMI_c) + BMI, data = D,cindex = F, dindex = F, AIC = F)  
#'
#'
#'@export 
#'@name JS.multi
#' 
#'

JS.multi<- function (..., cindex = F, dindex = F, AIC = F) 
{
        # get input elements
        input <- c(...)
        input.data <- as.data.frame(input[2:length(input)])
        
        # get variables 
        input.var <- reshape2:::parse_formula(input[[1]])[2]
        input.var2 <- unlist(input.var)
        var_n <- length(input.var2)
        vars <- NULL
        for (i in 1:var_n ) {
                vars <- c(vars, c(as.character(input.var2[[i]])))
        }
        # find out which varaible is used as a factor
        isfa <- which(vars%in%"as.factor")
        isfa_2 <- isfa + 1
        var.fa  <- vars[isfa_2]
        var.all <- vars[! vars %in% "as.factor"]
        
        #survival analysis 
        .fit <- coxph(...)
        .surv.cl <- summary(.fit)$conf.int
        .surv.p <- summary(.fit)$coefficients
        .surv.total <- cbind(paste(format(.surv.cl[, 1], digits = 2), 
                                   "(", format(.surv.cl[, 3], digits = 2), ",", format(.surv.cl[,4], digits = 2), ")"), .surv.p[, 5])
        .surv.total[, 2] <- JS.p(as.numeric(.surv.total[, 2]))
        
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
        
        #modify the table by adding reference group     
        .black <- c("Reference",rep(" ", length(.surv.total[1,])-1))
        ##find locations- can I use apply?
        order.fa = isfa
        for ( i in 1:length(isfa)){
                order.fa[i] <- .fit$assign[[isfa[i] - i + 1]][1]
        }
        
        ## insert 
        .surv.total <- JS.insert(order.fa, .black, .surv.total)
        
        ## 1. add variable names
        row.names<- rownames(.surv.total)
        ### modifly raw names
        .surv.total <- cbind( row.names(.surv.total), .surv.total)
        colnames(.surv.total) <- c( "Variables", "HR ( 95%CI )", "P-value")
        return (.surv.total)
}