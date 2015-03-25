#' A output table for univariable and multivariable competing risk analysis 
#' 
#'JS.crisk output the table with general competing risk analysis result with HR (95\% Confidence Interval),P value. This function only change the format of the output.       
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
#'@name JS.crisk
#' 
JS.crisk <- function (csurv, cevent, cvars, gnames){
        fit  = cmprsk:crr(csurv, cevent, cvars)
        S   <- summary(fit)
        HR  <- format(S$coef[,c(2)], digits = 2)
        LCL <- format(S$conf.int[,c(3)], digits = 2)
        UCL <- format(S$conf.int[,c(4)], digits = 3)
        HR95CI <- paste(HR,'(',LCL,',',UCL,')')
        p   <- JS.p(S$coef[,c(5)])
        out <- cbind(HR95CI, p)
        rownames(out) <- gnames
        colnames(out) <- c('Hazard Ratio (95% CI)', 'P')
        return(out)
}