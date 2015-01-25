#' A complete table for univariable survival analysis 
#' 
#'JS.uni_em output the table with general multivariable survival analysis result with Number of total patients,
#'Number of Events, Estimated Median, HR (95\% Confidence Interval),P value. This function only change the format of the output table.        
#'@param D A data.frame in which to interpret the variables 
#'@param Event The status indicator, normally 0=alive, 1=dead
#'@param Stime This is the follow up time
#'@param Svar A vector of group
#'@param groupn A text vector of the the group name for output 
#'@param Cat  logical, indicating whether or not Svar is a categorical varaible 
#'@return A dataframe of coxph output including Number of total patients, Number of Events, HRs (95\% Confidence Intervals), P values,
#'@examples
#'JS.uni_em(D = D ,"pd_censor", "pd_surv" , "tr_group", "Treatment" , Cat = T)
#'
#'rtf output
#'rtf<-RTF("Table_survival.doc",width = 8.5, height = 11, font.size = 10, omi = c(1,1,1,1))
#'addHeader(rtf,title="Table , Survival Analysis  ")
#'addTable(rtf,isup_m.surv, font.size = 10, row.names = F, NA.string="-", col.widths = c(rep(0.6, 3), 2, rep(0.6, 3) ) )
#'done(rtf))
#'
#'Rmarkdown output
#'save(isup_m.surv, file='myfile.Rda')
#'
#'Then open at markdown file
#'library(knitr)
#'output <- load("H:/Projects/myfile.Rda")
#'"```{r, echo=FALSE,results = 'asis'}"
#'output <- load("H:/Projects/p_Smith, Mitchell/Rituxan/Rituxan/T_BMI.Rdata")
#'knitr::kable(isup_m.surv, format = "markdown")  
#"```"
#'
#'@export 
#'@name JS.uni_em
#' 
JS.uni_em <- function(Data , Event, Stime , Svar, groupn, Cat = F)
{    
        #get factors;
        event <- Data[,match(Event, names(Data))]
        stime <- Data[,match(Stime, names(Data))]
        svar  <- Data[,match(Svar, names(Data))]
        
        # survival HR with 95%CL
        if (Cat == F){
                .fit <- coxph(Surv(as.numeric(stime), event) ~ svar , data = Data)
                emsv <- "--"
        }
        else if (Cat == T){
                #data[complete.cases(match(Svar, names(Data))), ]
                .fit <- coxph(Surv(as.numeric(stime), event) ~ as.factor(svar) , data = Data)
                #get median survival
                emsvtmp <- survMisc::median(survfit(Surv(as.numeric(stime), event) ~ as.factor(svar) , data = Data))
                emsv <- format(emsvtmp, digits = 2)
        }
        #.fitd <- coxph.detail(.fit)
        
        .surv.cl <-summary(.fit)$conf.int
        .surv.p  <-summary(.fit)$coefficients
        
        
        
        # combine into a table 
        .surv.total <- cbind(paste(format(.surv.cl[, 1], digits = 3), "(", format(.surv.cl[, 3], digits = 3), ",", format(.surv.cl[, 4], digits = 3),")" ),
                             .surv.p[, 5])
        .surv.total[, 2] <- JS.p(as.numeric(.surv.total[, 2]))
        
        if (Cat == T){
                .num <- length(.surv.total[,1])
                reference <- c(" Reference " , " ")
                .surv.total <- rbind(reference, .surv.total) 
                
                num.event   <- table(svar, event)[, 2]
                .surv.total <- cbind( c( data.frame(num.event)[, 1]) , emsv, .surv.total)
                .surv.total[,4] <- as.factor(.surv.total[,4])
                .surv.total <- cbind(data.frame(table(svar))$svar, data.frame(table(svar))$Freq, .surv.total)
                colnames(.surv.total) <- c("   ", "N", "No.Event", "Estimated Median", "HR ( 95%CI )", "P-value")
                Gname <- c(groupn, rep( " ", 5))
                .surv.total <- apply(.surv.total,2,as.character)
                .surv.total <- rbind(Gname, .surv.total)
        }
        if (Cat == F){
                num.event   <- table(event)[2]
                .surv.total <- cbind( c( data.frame(num.event)[, 1]) , emsv,  .surv.total)
                .surv.total <- cbind("", as.character(length(svar)), .surv.total)
                colnames(.surv.total) <- c("   ", "N", "No.Event","Estimated Median",  "HR ( 95%CI )", "P-value")
                .surv.total <- apply(.surv.total,2,as.character)
                .surv.total[1] <- groupn
        }
        
        return(.surv.total)
}