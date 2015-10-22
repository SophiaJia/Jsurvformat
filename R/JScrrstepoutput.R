#' Transform the result of ccrstep 
#' 
#'     
#'@param result the output result (table) of the ccrstep function  
#'@return output table with HR and P 
#'@examples
#'crrstep.output(result2)
#'@export 
#'@name crrstep.output
#' 
crrstep.output <- function(result,data){
        es <- result$coefficients[,1]
        sd <- result$coefficients[,"std.error"]
        t  <- result$coefficients[,"t-stat"]
        hr <- exp(es)
        low95 <- exp(es - 1.96*sd)
        up95  <- exp(es + 1.96*sd)
        n = length(data[,1])
        p <- 2 * pt(t, df=n,lower = FALSE) 
        
        total <- cbind(paste(J.digit(hr, 2), '(', J.digit(low95, 2),',',J.digit(up95, 2),')' ), JS.p(p) )
        colnames(total) <- c('HR (95%CI)', 'P')
        return(total)
}


