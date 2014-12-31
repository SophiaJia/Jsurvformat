#' Subset survival analysis 
#' 
#'JS.htmlout output an double title html table        
#'@param out A data.frame/matrix 
#'@param Svar  Name of the title group variable 
#'@return A html table
#'@examples
#'T_age <- JS.htmlout(out, "Table 1, HR and p")
#'
#'@export 
#'@name JS.htmlout
#' 
#'

JS.htmlout <- function(out, titlen)
{

tableout <-htmlTable::htmlTable(out, rowlabel = 'Variables <sup>&dagger;</sup>', 
                         ctable = TRUE, align = 'ccccc',
                         ## number of columns that each cgroup label spans:
                         #n.cgroup = c(1, 2, 2), cgroup = cgroup,
                         ## insert two table spanning sections:
                         #tspanner = c('',''),  # no labels
                         #n.tspanner = c(3, 5), # number of rows to span (must sum to nrow(out))
                         #           css.tspanner.sep = "border-bottom: 1px dotted grey;",
                         caption = titlen, 
                         tfoot = '<font size=1><sup>&dagger;</sup>Some note.</font>')
return (tableout)
}
