% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JBlogreg_m.R
\name{JB.logreg_m}
\alias{JB.logreg_m}
\title{A complete table for multipy logistic regression analysis for}
\usage{
JB.logreg_m(xvar, yvar, name, factorNY = FALSE)
}
\arguments{
\item{xvar}{A matrix of independent variable}

\item{yvar}{Dependent variable range(0,1)}

\item{name}{A vertor of independent variable name}
}
\value{
A formated output including OR(95\% Confidence Intervals), P value.
}
\description{
JB.logreg_m output the table with general logistic regression analysis result with HR (95\% Confidence Interval),P value. This function only change the format of the output.       
Note the difference with JS.crisk, however the usage is the same
}
\examples{
X      <- cbind(D$mutation,D$group2_1, D$group2_2 , D$age_ge60, D$comor_m, D$transplant_type, D$donor_m2, D$cellsource_m, D$diseasestat_m2)
Gnames <- c('SRSF2/U2FA1(WT vs Mutation)', 'Disease(AML vs MPN)', 'Disease(AML vs MDS or MDS/MPN)',
           'Age(< 60 vs > 60)', 'Comorbidity(Low vs Intern vs High)','Transplant ( Myeloblative vs Reduced Intensity)',
           'Donor(Sib/Related vs Unrelated)','Cell Source(BM vs PSC vs Cord)','Disease status(Relapse/Refrac or Untreated vs CR/NR/PR)')
JB.logreg_m(X, D$aGVHD_1Y, Gnames)

}
