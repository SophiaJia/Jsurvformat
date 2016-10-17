#' A complete table for multipy logistic regression analysis for 
#' 
#'JB.logreg_m output the table with general logistic regression analysis result with HR (95\% Confidence Interval),P value. This function only change the format of the output.       
#'Note the difference with JS.crisk, however the usage is the same 
#'@param xvar A matrix of independent variable 
#'@param yvar Dependent variable range(0,1)
#'@param name A vertor of independent variable name 
#'@return A formated output including OR(95\% Confidence Intervals), P value. 
#'@examples
#'X      <- cbind(D$mutation,D$group2_1, D$group2_2 , D$age_ge60, D$comor_m, D$transplant_type, D$donor_m2, D$cellsource_m, D$diseasestat_m2)
#'Gnames <- c('SRSF2/U2FA1(WT vs Mutation)', 'Disease(AML vs MPN)', 'Disease(AML vs MDS or MDS/MPN)',
#'            'Age(< 60 vs > 60)', 'Comorbidity(Low vs Intern vs High)','Transplant ( Myeloblative vs Reduced Intensity)',
#'            'Donor(Sib/Related vs Unrelated)','Cell Source(BM vs PSC vs Cord)','Disease status(Relapse/Refrac or Untreated vs CR/NR/PR)')
#'JB.logreg_m(X, D$aGVHD_1Y, Gnames)
#'
#'@export 
#'@name JB.logreg_m
#' 
JB.logreg_m <- function(xvar, yvar, name, factorNY = FALSE) {
        rs.all <- NULL
        for (i in 1:length(xvar[1,]))
        {
                rs <- JB.logreg(xvar[,i], yvar, name[i], factorNY) 
                rs.all <- rbind(rs.all, rs)
        }
        
        
        return(rs.all)
}