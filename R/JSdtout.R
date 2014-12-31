#' Subset survival analysis 
#' 
#'JS.dtout output an double title html table        
#'@param out A data.frame/matrix 
#'@param Svar  Name of the title group variable 
#'@return A html table
#'@examples
#'Svar  <- c("tr_group")
#'out <- JS.g3sub(D, Event, Stime, Svar, "age_m", cutoff = 60, nocut = F)
#'T_age <- JS.dtout(out, Svar,"Table 1. HR and p ")
#'
#'@export 
#'@name JS.dtout
#' 
#'
JS.dtout <- function(out, svar, titlen)
{
        .nsvar <- table (svar)
        .tname <- names(table(svar))
        colnames(out)[1] <- c( paste(.tname[1], '<br />(n =', .nsvar[1], ')'))
        rownames(out) <- sprintf('<b>%s</b>', rownames(out))
        
        
        ## table column headers (with line breaks (<br />))
        cgroup <- c('',
                    paste(.tname[2], '<br />( n =', .nsvar[2], ')'), 
                    paste(.tname[3], '<br />( n =', .nsvar[3], ')'))
        
        # zzz <- `rownames<-`(out, NULL)
        
        tableout <- htmlTable::htmlTable(out, rowlabel = 'Adjustment<sup>&dagger;</sup>', 
                                         ctable = TRUE, align = 'ccccc',
                                         ## number of columns that each cgroup label spans:
                                         n.cgroup = c(1, 2, 2), cgroup = cgroup,
                                         ## insert two table spanning sections:
                                         #tspanner = c('',''),  # no labels
                                         #n.tspanner = c(3, 5), # number of rows to span (must sum to nrow(out))
                                         #           css.tspanner.sep = "border-bottom: 1px dotted grey;",
                                         caption = titlen, 
                                         tfoot = '<font size=1><sup>&dagger;</sup>Some note.</font>')
        
        return (tableout)
}
