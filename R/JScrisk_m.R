#' A complete table for multiple univariable competing risk analysis 
#' 
#'JS.crisk_m output the table with general competing risk analysis result with HR (95\% Confidence Interval),P value. This function only change the format of the output.       
#'Note the difference with JS.crisk, however the usage is the same 
#'@param cevent The status indicator, normally 0 = alive, 1 = event, 2 = other event 
#'@param csurv Follow up time
#'@param cvars A Matrix of groups variables
#'@param gnames A text vector of the the group name for output 
#'@return A dataframed output including HRs (95\% Confidence Intervals), P values.
#'@examples
#'X      <- cbind(D$mutation,D$group2_1, D$group2_2 , D$age_ge60, D$comor_m, D$transplant_type, D$donor_m2, D$cellsource_m, D$diseasestat_m2)
#'Gnames <- c('SRSF2/U2FA1(WT vs Mutation)', 'Disease(AML vs MPN)', 'Disease(AML vs MDS or MDS/MPN)',
#'            'Age(< 60 vs > 60)', 'Comorbidity(Low vs Intern vs High)','Transplant ( Myeloblative vs Reduced Intensity)',
#'            'Donor(Sib/Related vs Unrelated)','Cell Source(BM vs PSC vs Cord)','Disease status(Relapse/Refrac or Untreated vs CR/NR/PR)')
#'B.RRM  <- crisk (D$surv, D$censor_rm, X, Gnames)
#'
#'@export 
#'@name JS.crisk_m
#' 
JS.crisk_m <- function(csurv, cevent, cvars, gnames) {
        rs.all <- NULL
        for (i in 1:length(cvars[1,]))
        {
                rs <- JS.crisk(csurv, cevent, cvars[,i] , gnames[i])
                rs.all <- rbind(rs.all, rs)
        }
        
        
        return(rs.all)
}