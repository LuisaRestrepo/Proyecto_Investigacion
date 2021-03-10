#-----------------------------------------------------------------
#3. MULTINOM
#-----------------------------------------------------------------

### MODEL 1 - CATEGORICAL VARIABLES

m1=multinom(Q18.1 ~ Q3+Q8NN+Q16.1+Q16.2+Q16.3+Q16.4+Q16.5+Q17.1+Q17.2+Q22.1+Q22.2+Q22.3+Q22.4+Q22.5+Q22.6+Q27+Q28+Q31.1+
    QB4.1+QB4.2+QB4.3+QB4.4+QB4.5+QB4.6+QB4.7+QB4.8+QB4.9+QB4.10+QB4.11+QB4.12+QB4.13+QB4.14+QB4.15+QB4.16+QB4.17+QB4.18+QB4.19+QB4.20+
              QB5.1+QB5.2+QB5.3+QB5.4+QB5.5+QB5.6+QB5.7+QB5.11+
              QB7.1+QB7.2+QB7.3+QB7.4+QB7.5+QB7.6+QB7.7+QB7.8+QB7.9+QB7.10+QB7.11+QB7.12+
              QB9.1+QB9.2+QB9.3+QB9.4+
              QB11.1+QB11.2+QB11.3+QB11.4+QB11.5+QB11.6+QB11.11+QB11.12+QB11.13+QB11.14+QB11.15+QB11.16+QB11.17+
              QB12.1+QB12.2+QB12.3+QB12.4+QB12.5+QB12.6+QB12.7+
              QB13.1+QB13.2+QB13.3+QB13.4+QB13.5+QB13.6+QB13.7+QB13.8+QB13.9+QB13.11+QB13.12+
              QB15.1+QB15.2+QB15.3+
              QB20.1+QB20.2+QB20.3+QB20.4+QB20.5+QB20.6+QB20.7+QB20.8+QB20.9+QB20.10+QB20.11+QB20.12+QB20.13+
              QB21.1+QB21.2+QB21.3+QB21.4+QB21.5+QB21.6+QB21.7+QB21.8+QB21.9+QB21.10+QB21.11+QB21.12+QB21.13+
              QB23.1+QB23.2+QB23.3+QB23.4+QB23.5+
              QB24.1+QB24.2+
              QB25.1+QB25.2+QB25.3+
              QB30.1+QB30.2+QB30.3,data=dt2,MaxNWts= 1305)

summary(m1) 
install.packages("rminer")
library(rminer)

H=holdout(dt2$respondent_id,ratio=2/3,mode="random")
print(H)

#STEP
m1_step <- step(m1)
summary(m1_step) 
anova_m1_m1_step <- anova(m1_step,m1, test = "Chisq") #Pr(Chi) 1

z=summary(m1_step)$coefficients/summary(m1_step)$standard.errors
z
valor.p <- (1 -pnorm(abs(z),0,1))*2
valor.p

riesgos <-  fitted.values(m1_step)
riesgos
plot(fitted(m1_step))
residuals(m1_step,type='deviance')
residuals

###MODEL 2 - VARIABLES LIKERT AS ORDINAL
m2=multinom(Q18.1 ~ Q3+Q8NN+OQ16.1+OQ16.2+OQ16.3+OQ16.4+OQ16.5+OQ17.1+OQ17.2+OQ22.1+OQ22.2+OQ22.3+OQ22.4+OQ22.5+OQ22.6+OQ27+OQ28+OQ31.1+
              QB4.1+QB4.2+QB4.3+QB4.4+QB4.5+QB4.6+QB4.7+QB4.8+QB4.9+QB4.10+QB4.11+QB4.12+QB4.13+QB4.14+QB4.15+QB4.16+QB4.17+QB4.18+QB4.19+QB4.20+
              QB5.1+QB5.2+QB5.3+QB5.4+QB5.5+QB5.6+QB5.7+QB5.11+
              QB7.1+QB7.2+QB7.3+QB7.4+QB7.5+QB7.6+QB7.7+QB7.8+QB7.9+QB7.10+QB7.11+QB7.12+
              QB9.1+QB9.2+QB9.3+QB9.4+
              QB11.1+QB11.2+QB11.3+QB11.4+QB11.5+QB11.6+QB11.11+QB11.12+QB11.13+QB11.14+QB11.15+QB11.16+QB11.17+
              QB12.1+QB12.2+QB12.3+QB12.4+QB12.5+QB12.6+QB12.7+
              QB13.1+QB13.2+QB13.3+QB13.4+QB13.5+QB13.6+QB13.7+QB13.8+QB13.9+QB13.11+QB13.12+
              QB15.1+QB15.2+QB15.3+
              QB20.1+QB20.2+QB20.3+QB20.4+QB20.5+QB20.6+QB20.7+QB20.8+QB20.9+QB20.10+QB20.11+QB20.12+QB20.13+
              QB21.1+QB21.2+QB21.3+QB21.4+QB21.5+QB21.6+QB21.7+QB21.8+QB21.9+QB21.10+QB21.11+QB21.12+QB21.13+
              QB23.1+QB23.2+QB23.3+QB23.4+QB23.5+
              QB24.1+QB24.2+
              QB25.1+QB25.2+QB25.3+
              QB30.1+QB30.2+QB30.3,MaxNWts= 1305, data=dt3)

summary(m2)

#STEP
m2_step <- step(m2) 
summary(m2_step)
anova_m2_m2_step <- anova(m2_step,m2, test = "Chisq") #Pr(Chi) 1
m2_step$anova

z <- summary(m2_step)$coefficients/summary(m2_step)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


###MODEL 3 - DEPENDET VARIABLE (Q18.1) AS ORDINAL, INDEPENDENT ONES ORDERED
dt4 <- dt3
dt4$q18.1ord=ordered(dt4$Q18.1,levels = c("No success","Limited","Moderate","Substancial","Strong"))
levels(dt4$q18.1ord)


m3=polr(q18.1ord ~ Q3+Q8NN+OQ16.1+OQ16.2+OQ16.3+OQ16.4+OQ16.5+OQ17.1+OQ17.2+OQ22.1+OQ22.2+OQ22.3+OQ22.4+OQ22.5+OQ22.6+OQ27+OQ28+OQ31.1+
    QB4.1+QB4.2+QB4.3+QB4.4+QB4.5+QB4.6+QB4.7+QB4.8+QB4.9+QB4.10+QB4.11+QB4.12+QB4.13+QB4.14+QB4.15+QB4.16+QB4.17+QB4.18+QB4.19+QB4.20+
              QB5.1+QB5.2+QB5.3+QB5.4+QB5.5+QB5.6+QB5.7+QB5.11+
              QB7.1+QB7.2+QB7.3+QB7.4+QB7.5+QB7.6+QB7.7+QB7.8+QB7.9+QB7.10+QB7.11+QB7.12+
              QB9.1+QB9.2+QB9.3+QB9.4+
              QB11.1+QB11.2+QB11.3+QB11.4+QB11.5+QB11.6+QB11.11+QB11.12+QB11.13+QB11.14+QB11.15+QB11.16+QB11.17+
              QB12.1+QB12.2+QB12.3+QB12.4+QB12.5+QB12.6+QB12.7+
              QB13.1+QB13.2+QB13.3+QB13.4+QB13.5+QB13.6+QB13.7+QB13.8+QB13.9+QB13.11+QB13.12+
              QB15.1+QB15.2+QB15.3+
              QB20.1+QB20.2+QB20.3+QB20.4+QB20.5+QB20.6+QB20.7+QB20.8+QB20.9+QB20.10+QB20.11+QB20.12+QB20.13+
              QB21.1+QB21.2+QB21.3+QB21.4+QB21.5+QB21.6+QB21.7+QB21.8+QB21.9+QB21.10+QB21.11+QB21.12+QB21.13+
              QB23.1+QB23.2+QB23.3+QB23.4+QB23.5+
              QB24.1+QB24.2+
              QB25.1+QB25.2+QB25.3+
              QB30.1+QB30.2+QB30.3
        , data=dt4, Hess = TRUE)

summary(m3) 
#STEP
m3_step <- step(m3, k = 5) 
m3_step <- step(m3)
m3_step <- stepAIC(m3)

summary(m3_step)
m3s_anova <- m3_step$anova
m3s_anova
m3s_anova$Deviance

#install.packages("tidyverse")
library(broom)
pvalue <- tidy(m3_step,conf.level = 0.95,p.value = TRUE)
pvalue
glance(m3_step) 


(ctable <- coef(summary(m3_step)))#store table
p <- pt(abs(ctable[, "t value"]), df = 125, lower.tail = FALSE) * 2 ### df = 138, calculate and store p values PNORM, df = n- k -1
p
(ctable <- cbind(ctable, "p value" = p)) ## combined table


###MODEL 4 - DEPENDET VARIABLE (Q18.1) AS ORDINAL, INDEPENDENT VARIABLES UNSORTED

m4=polr(q18.1ord ~ Q3+Q8NN+Q16.1+Q16.2+Q16.3+Q16.4+Q16.5+Q17.1+Q17.2+Q22.1+Q22.2+Q22.3+Q22.4+Q22.5+Q22.6+Q27+Q28+Q31.1+
    QB4.1+QB4.2+QB4.3+QB4.4+QB4.5+QB4.6+QB4.7+QB4.8+QB4.9+QB4.10+QB4.11+QB4.12+QB4.13+QB4.14+QB4.15+QB4.16+QB4.17+QB4.18+QB4.19+QB4.20+
              QB5.1+QB5.2+QB5.3+QB5.4+QB5.5+QB5.6+QB5.7+QB5.11+
              QB7.1+QB7.2+QB7.3+QB7.4+QB7.5+QB7.6+QB7.7+QB7.8+QB7.9+QB7.10+QB7.11+QB7.12+
              QB9.1+QB9.2+QB9.3+QB9.4+
              QB11.1+QB11.2+QB11.3+QB11.4+QB11.5+QB11.6+QB11.11+QB11.12+QB11.13+QB11.14+QB11.15+QB11.16+QB11.17+
              QB12.1+QB12.2+QB12.3+QB12.4+QB12.5+QB12.6+QB12.7+
              QB13.1+QB13.2+QB13.3+QB13.4+QB13.5+QB13.6+QB13.7+QB13.8+QB13.9+QB13.11+QB13.12+
              QB15.1+QB15.2+QB15.3+
              QB20.1+QB20.2+QB20.3+QB20.4+QB20.5+QB20.6+QB20.7+QB20.8+QB20.9+QB20.10+QB20.11+QB20.12+QB20.13+
              QB21.1+QB21.2+QB21.3+QB21.4+QB21.5+QB21.6+QB21.7+QB21.8+QB21.9+QB21.10+QB21.11+QB21.12+QB21.13+
              QB23.1+QB23.2+QB23.3+QB23.4+QB23.5+
              QB24.1+QB24.2+
              QB25.1+QB25.2+QB25.3+
              QB30.1+QB30.2+QB30.3,data=dt4)

summary(m4) 

#STEP
m4_step <- step(m4)
summary(m4_step)
anova_m4_m4_step <- anova(m4_step,m4, test = "Chisq") 
m4_step$anova

library(broom)
pvalue <- tidy(m4_step,conf.level = 0.95,p.value = TRUE)
pvalue
glance(m4_step) #


(ctable <- coef(summary(m4_step)))#store table
p <- pt(abs(ctable[, "t value"]), df = 125, lower.tail = FALSE) * 2 
p
(ctable <- cbind(ctable, "p value" = p))


###REGRESSION BY COMPANY SIZE

library(dplyr)
dt_startups <- filter_at(dt4, vars(Q3), any_vars( . == "Startups")  , .preserve = TRUE)
dt_Microenterprise <- filter_at(dt4, vars(Q3), any_vars( . == "Microenterprise")  , .preserve = TRUE)
dt_Small <- filter_at(dt4, vars(Q3), any_vars( . == "Small")  , .preserve = TRUE)
dt_Medium <- filter_at(dt4, vars(Q3), any_vars( . == "Medium")  , .preserve = TRUE)
dt_Large <- filter_at(dt4, vars(Q3), any_vars( . == "Large")  , .preserve = TRUE)
count(dt_startups, dt_startups$respondent_id) 
count(dt_Microenterprise, dt_Microenterprise$respondent_id) #37
count(dt_Small, dt_Small$respondent_id) 
count(dt_Medium, dt_Medium$respondent_id) 
count(dt_Large, dt_Large$respondent_id) 


SS=multinom(Q18.1~ Q16.1+Q16.2+Q16.3+Q16.4+Q16.5+Q17.1+Q17.2+Q22.1+Q22.2+Q22.3+Q22.4+Q22.5++Q22.6+Q27+Q28+Q31.1+
                    QB4.1+QB4.2+QB4.3+QB4.4+QB4.5+QB4.6+QB4.7+QB4.8+QB4.9+QB4.10+QB4.11+QB4.12+QB4.13+QB4.14+QB4.15+QB4.16+QB4.17+QB4.18+QB4.19+
                    QB5.1+QB5.2+QB5.3+QB5.4+QB5.5+QB5.6+QB5.7+
                    QB7.1+QB7.2+QB7.3+QB7.4+QB7.5+QB7.6+QB7.7+QB7.8+QB7.9+QB7.10+QB7.11+
                    QB9.1+QB9.2+QB9.3+QB9.4+
                    QB11.1+QB11.2+QB11.3+QB11.4+QB11.5+QB11.6+QB11.11+QB11.12+QB11.13+QB11.14+QB11.15+QB11.16+
                    QB12.1+QB12.2+QB12.3+QB12.4+QB12.5+QB12.6+QB12.7+
                    QB13.1+QB13.2+QB13.3+QB13.4+QB13.5+QB13.6+QB13.7+QB13.8+QB13.9+QB13.11+
                    QB15.1+QB15.2+
                    QB20.1+QB20.2+QB20.3+QB20.4+QB20.5+QB20.6+QB20.7+QB20.8+QB20.9+QB20.10+QB20.11+QB20.12+QB20.13+
                    QB21.1+QB21.2+QB21.3+QB21.4+QB21.5+QB21.6+QB21.7+QB21.8+QB21.9+QB21.10+QB21.11+QB21.12+QB21.13+
                    QB23.1+QB23.2+QB23.3+QB23.4+QB23.5+
                    QB24.1+QB24.2+
                    QB25.1+QB25.2+
                    QB30.3
            ,data=dt_startups, Hess = TRUE)

SO=multinom(Q18.1 ~ OQ16.1+OQ16.2+OQ16.3+OQ16.4+OQ16.5+OQ17.1+OQ17.2+OQ22.1+OQ22.2+OQ22.3+OQ22.4+OQ22.5++OQ22.6+OQ27+OQ28+OQ31.1+
                    QB4.1+QB4.2+QB4.3+QB4.4+QB4.5+QB4.6+QB4.7+QB4.8+QB4.9+QB4.10+QB4.11+QB4.12+QB4.13+QB4.14+QB4.15+QB4.16+QB4.17+QB4.18+QB4.19+
                    QB5.1+QB5.2+QB5.3+QB5.4+QB5.5+QB5.6+QB5.7+
                    QB7.1+QB7.2+QB7.3+QB7.4+QB7.5+QB7.6+QB7.7+QB7.8+QB7.9+QB7.10+QB7.11+
                    QB9.1+QB9.2+QB9.3+QB9.4+
                    QB11.1+QB11.2+QB11.3+QB11.4+QB11.5+QB11.6+QB11.11+QB11.12+QB11.13+QB11.14+QB11.15+QB11.16+
                    QB12.1+QB12.2+QB12.3+QB12.4+QB12.5+QB12.6+QB12.7+
                    QB13.1+QB13.2+QB13.3+QB13.4+QB13.5+QB13.6+QB13.7+QB13.8+QB13.9+QB13.11+
                    QB15.1+QB15.2+
                    QB20.1+QB20.2+QB20.3+QB20.4+QB20.5+QB20.6+QB20.7+QB20.8+QB20.9+QB20.10+QB20.11+QB20.12+QB20.13+
                    QB21.1+QB21.2+QB21.3+QB21.4+QB21.5+QB21.6+QB21.7+QB21.8+QB21.9+QB21.10+QB21.11+QB21.12+QB21.13+
                    QB23.1+QB23.2+QB23.3+QB23.4+QB23.5+
                    QB24.1+QB24.2+
                    QB25.1+QB25.2+
                    QB30.3
            ,data=dt_startups, Hess = TRUE)

OO=polr(q18.1ord ~ OQ16.1+OQ16.2+OQ16.3+OQ16.4+OQ16.5+OQ17.1+OQ17.2+OQ22.1+OQ22.2+OQ22.3+OQ22.4+OQ22.5++OQ22.6+OQ27+OQ28+OQ31.1+
                QB4.1+QB4.2+QB4.3+QB4.4+QB4.5+QB4.6+QB4.7+QB4.8+QB4.9+QB4.10+QB4.11+QB4.12+QB4.13+QB4.14+QB4.15+QB4.16+QB4.17+QB4.18+QB4.19+
                QB5.1+QB5.2+QB5.3+QB5.4+QB5.5+QB5.6+QB5.7+
                QB7.1+QB7.2+QB7.3+QB7.4+QB7.5+QB7.6+QB7.7+QB7.8+QB7.9+QB7.10+QB7.11+
                QB9.1+QB9.2+QB9.3+QB9.4+
                QB11.1+QB11.2+QB11.3+QB11.4+QB11.5+QB11.6+QB11.11+QB11.12+QB11.13+QB11.14+QB11.15+QB11.16+
                QB12.1+QB12.2+QB12.3+QB12.4+QB12.5+QB12.6+QB12.7+
                QB13.1+QB13.2+QB13.3+QB13.4+QB13.5+QB13.6+QB13.7+QB13.8+QB13.9+QB13.11+
                QB15.1+QB15.2+
                QB20.1+QB20.2+QB20.3+QB20.4+QB20.5+QB20.6+QB20.7+QB20.8+QB20.9+QB20.10+QB20.11+QB20.12+
                QB21.1+QB21.2+QB21.3+QB21.4+QB21.5+QB21.6+QB21.7+QB21.8+QB21.9+QB21.10+QB21.11+QB21.12+QB21.13+
                QB23.1+QB23.2+QB23.3+QB23.4+QB23.5+
                QB24.1+QB24.2+
                QB25.1+QB25.2+
                QB30.3
        ,data=dt_Small, Hess = TRUE)

OO=polr(q18.1ord ~ OQ16.1+OQ16.2+OQ16.3+OQ16.4+OQ16.5+OQ17.1+OQ17.2+OQ22.1+OQ22.2+OQ22.3+OQ22.4+OQ22.5++OQ22.6+OQ27+OQ28+OQ31.1+
                QB5.1+QB5.2+QB5.3+QB5.4+QB5.5+QB5.6+QB5.7+
                QB9.1+QB9.2+QB9.3+QB9.4+
                QB15.1+QB15.2+
                QB21.1+QB21.2+QB21.3+QB21.4+QB21.5+QB21.6+QB21.7+QB21.8+QB21.9+QB21.10+QB21.11+QB21.12+QB21.13+
                QB23.1+QB23.2+QB23.3+QB23.4+QB23.5+
                QB24.1+QB24.2+
                QB25.1+QB25.2+
                QB30.3
        ,data=dt_Small, Hess = TRUE)

OS=polr(q18.1ord ~ Q16.1+Q16.2+Q16.3+Q16.4+Q16.5+Q17.1+Q17.2+Q22.1+Q22.2+Q22.3+Q22.4+Q22.5++Q22.6+Q27+Q28+Q31.1+
                QB4.1+QB4.2+QB4.3+QB4.4+QB4.5+QB4.6+QB4.7+QB4.8+QB4.9+QB4.10+QB4.11+QB4.12+QB4.13+QB4.14+QB4.15+QB4.16+QB4.17+QB4.18+QB4.19+
                QB5.1+QB5.2+QB5.3+QB5.4+QB5.5+QB5.6+QB5.7+
                QB7.1+QB7.2+QB7.3+QB7.4+QB7.5+QB7.6+QB7.7+QB7.8+QB7.9+QB7.10+QB7.11+
                QB9.1+QB9.2+QB9.3+QB9.4+
                QB11.1+QB11.2+QB11.3+QB11.4+QB11.5+QB11.6+QB11.11+QB11.12+QB11.13+QB11.14+QB11.15+QB11.16+
                QB12.1+QB12.2+QB12.3+QB12.4+QB12.5+QB12.6+QB12.7+
                QB13.1+QB13.2+QB13.3+QB13.4+QB13.5+QB13.6+QB13.7+QB13.8+QB13.9+QB13.11+
                QB15.1+QB15.2+
                QB20.1+QB20.2+QB20.3+QB20.4+QB20.5+QB20.6+QB20.7+QB20.8+QB20.9+QB20.10+QB20.11+QB20.12+QB20.13+
                QB21.1+QB21.2+QB21.3+QB21.4+QB21.5+QB21.6+QB21.7+QB21.8+QB21.9+QB21.10+QB21.11+QB21.12+QB21.13+
                QB23.1+QB23.2+QB23.3+QB23.4+QB23.5+
                QB24.1+QB24.2+
                QB25.1+QB25.2+
                QB30.3
        ,data=dt_startups, Hess = TRUE)


summary(OO)
OO_step <- step(OO)
summary(OO_step)
OO_step$anova

library("broom")
pvaluem3 <- tidy(m3,conf.level = 0.95)
pvaluem3

