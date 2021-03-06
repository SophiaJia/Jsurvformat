% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JSg3subout.R
\name{JS.g3subout}
\alias{JS.g3subout}
\title{Subset survival analysis}
\usage{
JS.g3subout(Data, Event, Stime, Svar, Group, cutoff = NA, nocut = T)
}
\arguments{
\item{Event}{Name of event varaible :the status indicator, normally 0=alive, 1=dead}

\item{Stime}{Name of the follow up time variable}

\item{Svar}{Name of the comparing group variable}

\item{Group}{Name of the subset group variable}

\item{cutoff}{value of the cut off}

\item{nocut}{logical, if a cutoff is defined or not}

\item{D}{A data.frame in which to interpret the variables}
}
\value{
A dataframe of coxph output including  HRs (95\% Confidence Intervals), P values
}
\description{
JS.g3subout output an html table with general multivariable survival analysis result with HR (95\% Confidence Interval),P value
}
\examples{
Event <- c("pd_censor")
Stime <- c("pd_surv")
Svar  <- c("tr_group")
Group <- c("age_m")

T_age <- JS.g3sub(D, Event, Stime, Svar, "age_m", cutoff = 60, nocut = F)
T_BMI <- JS.g3sub(D, Event, Stime, Svar, Group)
save(T_age,T_BMI, file='myfile.Rda')

Then write this in Rmarkdown 
```{r, echo=FALSE,results = 'asis'}
load("H:/Projects/p_Smith, Mitchell/Rituxan/Rituxan/myfile.Rda")
knitr::kable(T_age, format = "markdown", row.names = F)
```

the second table 
```{r, echo=FALSE,results = 'asis'}
knitr::kable(T_BMI, format = "markdown", row.names = F)
```
}
