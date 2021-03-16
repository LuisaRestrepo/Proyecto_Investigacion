#-----------------------------------------------------------------
# PAQUETES, LIBRERIAS, DATA
#-----------------------------------------------------------------

#Paquetes
#install.packages("packagename")
#install.packages("Rcpp")
#install.packages("ggplot2")
#install.packages("data.table", dependencies=TRUE)
#install.packages("tidyverse")
#install.packages("rlang")
#install.packages("xlsx")
#install.packages("likert")

#Librerias
library(readxl)
library(ggplot2)
library(data.table)
library (MASS)
library (nnet)
library(rlang)
library(xlsx)
library(dplyr)

#Leer data
#HOME
dt <- read_excel("")

#-----------------------------------------------------------------
#1. RECODE DATA
#-----------------------------------------------------------------

#Recode para presentacion de graficas
dt$Q18.1[dt$Q18.1=="N/C"] <- "No success" ##########OJOOOOOOO "No success" 
dt$Q18.1[dt$Q18.1=="1. SIN ÉXITO"] <- "No success"
dt$Q18.1[dt$Q18.1=="2"] <- "No success"
dt$Q18.1[dt$Q18.1=="3"] <- "Limited"
dt$Q18.1[dt$Q18.1=="4"] <- "Limited"
dt$Q18.1[dt$Q18.1=="5"] <- "Moderate"
dt$Q18.1[dt$Q18.1=="6"] <- "Moderate"
dt$Q18.1[dt$Q18.1=="7"] <- "Substancial"
dt$Q18.1[dt$Q18.1=="8"] <- "Substancial"
dt$Q18.1[dt$Q18.1=="9"] <- "Strong"
dt$Q18.1[dt$Q18.1=="10. CON MUCHO ÉXITO"] <- "Strong"

dt$Q3[dt$Q3=="Startups - Entre 1 a 5 trabajadores"] <- "Startups"
dt$Q3[dt$Q3=="Microempresa - Entre 6 a 10 trabajadores"] <- "Microenterprise"
dt$Q3[dt$Q3=="Pequeño - Entre 11 a 50 trabajadores"] <- "Small"
dt$Q3[dt$Q3=="Mediano - Entre 51 a 200 trabajadores"] <- "Medium"
dt$Q3[dt$Q3=="Grande - más de 200 trabajadores"] <- "Large"

dt$Q1.4[dt$Q1.4=="CENTRO ORIENTE"] <- "Middle east"
dt$Q1.4[dt$Q1.4=="CARIBE"] <- "Caribbean"
dt$Q1.4[dt$Q1.4=="CENTRO SUR"] <- "South center"
dt$Q1.4[dt$Q1.4=="EJE CAFETERO"] <- "Coffee belt"
dt$Q1.4[dt$Q1.4=="LLANOS"] <- "LLanos"
dt$Q1.4[dt$Q1.4=="PACIFICO"] <- "Pacific"

#likert en inglés
QLikertAcuerdo <- list("Q16.1","Q16.2","Q16.3","Q16.4","Q16.5","Q27","Q28")
QLikertFrecuencia <- list("Q17.1","Q17.2","Q31.1","Q31.2","Q31.3")
QLikertCantidad <- list("Q22.1","Q22.2","Q22.3","Q22.4","Q22.5","Q22.6")

dt$Q26.1[dt$Q26.1=="Nunca"] <- "Never"
dt$Q26.1[dt$Q26.1=="Casi nunca"] <- "Hardly ever"
dt$Q26.1[dt$Q26.1=="De vez en cuando"] <- "Occasionally"
dt$Q26.1[dt$Q26.1=="A veces"] <- "Sometimes"
dt$Q26.1[dt$Q26.1=="Con frecuencia"] <- "Frequently"
dt$Q26.1[dt$Q26.1=="Muchísimas veces"] <- "Usually"
dt$Q26.1[dt$Q26.1=="Siempre"] <- "Always"

for(o in QLikertAcuerdo){
  dt[[o]][dt[[o]]=="Totalmente en desacuerdo"] <- "Strongly disagree"
  dt[[o]][dt[[o]]=="En desacuerdo"] <- "Disagree"
  dt[[o]][dt[[o]]=="Ni de acuerdo ni en desacuerdo"] <- "Neutral"
  dt[[o]][dt[[o]]=="De acuerdo"] <- "Agree"
  dt[[o]][dt[[o]]=="Totalmente de acuerdo"] <- "Strongly agree"
}

for(o in QLikertFrecuencia){
  dt[[o]][dt[[o]]=="Casi nunca"] <- "Hardly ever"
  dt[[o]][dt[[o]]=="A veces"] <- "Sometimes"
  dt[[o]][dt[[o]]=="Normalmente"] <- "Normally"
  dt[[o]][dt[[o]]=="Casi siempre"] <- "Usually"
  dt[[o]][dt[[o]]=="Siempre"] <- "Always"
}

for(o in QLikertCantidad){
  dt[[o]][dt[[o]]=="Casi nada"] <- "Almost nothing"
  dt[[o]][dt[[o]]=="Solo un poco"] <- "Just a little"
  dt[[o]][dt[[o]]=="La mitad"] <- "Half"
  dt[[o]][dt[[o]]=="Casi totalmente"] <- "Almost totally"
  dt[[o]][dt[[o]]=="Completamente"] <- "Completely"
}

############DELETE UNFILLED SURVEYS Q18.1
dt2 <- dt
dt2 <- dt2[-which(is.na(dt2$Q18.1)),]
#Columna númerica agrupada
dt2$Q8NN <- NA
dt2$Q8NN[dt2$Q8>=1 & dt2$Q8<=3] <- "1-3"
dt2$Q8NN[dt2$Q8>=4 & dt2$Q8<=5] <- "4-5"
dt2$Q8NN[dt2$Q8>=6 & dt2$Q8<=10] <- "6-10"
dt2$Q8NN[dt2$Q8>=11] <- "11+"

#-----------------------------------------------------------------
#2. CONSTRUCT FACTORS Y ORDINALS
#-----------------------------------------------------------------

MixAnswer <- list("Q18.1","Q1.3","Q1.4","Q3","Q8N","Q16.1","Q16.2","Q16.3","Q16.4","Q16.5","Q17.1","Q17.2","Q22.1","Q22.2","Q22.3","Q22.4","Q22.5","Q22.6","Q26.1","Q27","Q28","Q31.1","Q31.2","Q31.3","QB4.1","QB4.2","QB4.3","QB4.4","QB4.5","QB4.6","QB4.7","QB4.8","QB4.9","QB4.10","QB4.11","QB4.12","QB4.13","QB4.14","QB4.15","QB4.16","QB4.17","QB4.18","QB4.19","QB4.20","QB5.1","QB5.2","QB5.3","QB5.4","QB5.5","QB5.6","QB5.7","QB5.8","QB5.9","QB5.10","QB5.11","QB7.1","QB7.2","QB7.3","QB7.4","QB7.5","QB7.6","QB7.7","QB7.8","QB7.9","QB7.10","QB7.11","QB7.12","QB7.13","QB9.1","QB9.2","QB9.3","QB9.4","QB10.1","QB10.2","QB10.3","QB10.4","QB11.1","QB11.2","QB11.3","QB11.4","QB11.5","QB11.6","QB11.7","QB11.8","QB11.9","QB11.10","QB11.11","QB11.12","QB11.13","QB11.14","QB11.15","QB11.16","QB11.17","QB12.1","QB12.2","QB12.3","QB12.4","QB12.5","QB12.6","QB12.7","QB12.8","QB13.1","QB13.2","QB13.3","QB13.4","QB13.5","QB13.6","QB13.7","QB13.8","QB13.9","QB13.10","QB13.11","QB13.12","QB19.1","QB19.2","QB19.3","QB19.4","QB19.5","QB19.6","QB19.7","QB19.8","QB19.9","QB19.10","QB19.11","QB19.12","QB19.13","QB20.1","QB20.2","QB20.3","QB20.4","QB20.5","QB20.6","QB20.7","QB20.8","QB20.9","QB20.10","QB20.11","QB20.12","QB20.13","QB20.14","QB21.1","QB21.2","QB21.3","QB21.4","QB21.5","QB21.6","QB21.7","QB21.8","QB21.9","QB21.10","QB21.11","QB21.12","QB21.13","QB21.14","QB23.1","QB23.2","QB23.3","QB23.4","QB23.5","QB24.1","QB24.2","QB24.3","QB24.4","QB24.5","QB25.1","QB25.2","QB25.3","QB30.1","QB30.2","QB30.3","QB30.4","QB14.1","QB14.2","QB14.3","QB14.4","QB14.5","QB14.6","QB14.7","QB14.8","QB14.9","QB14.10","QB14.11","QB14.12","QB14.13","QB14.14","QB14.15","QB14.16","QB14.17","QB14.18","QB14.19","QB15.1","QB15.2","QB15.3")

for(k in MixAnswer){
  
  if( is_empty(which(is.na(dt2[[k]])))){
    
    dt2[[k]] <- as.factor(dt2[[k]])
  }else
  {
    dt2 <- dt2[-which(is.na(dt2[[k]])),]
    dt2[[k]] <- as.factor(dt2[[k]])
  }
}

levels(dt2$Q18.1)
str(dt2)
str(dt2$Q18.1)

#CREATE LIKERT VARIABLES AS ORDINALS 
dt3 <- dt2
QLikertAcuerdo <- list("Q16.1","Q16.2","Q16.3","Q16.4","Q16.5","Q27","Q28")
QLikertFrecuencia <- list("Q17.1","Q17.2","Q31.1","Q31.2","Q31.3")
QLikertCantidad <- list("Q22.1","Q22.2","Q22.3","Q22.4","Q22.5","Q22.6")

dt3$OQ26.1=ordered(dt3$Q26.1,levels = c("Never","Hardly ever","Occasionally","Sometimes","Frequently","Usually","Always"))# considerar una variable categórica como una ordinal
levels(dt3$OQ26.1)

for(o in QLikertAcuerdo){
  dt3[,paste("O",o,sep="")]=ordered(dt3[[o]],levels = c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"))# considerar una variable categórica como una ordinal
}
str(dt3$OQ16.2)
levels(dt3$OQ16.4)

for(o in QLikertFrecuencia){
  dt3[,paste("O",o,sep="")]=ordered(dt3[[o]],levels = c("Hardly ever","Sometimes","Normally","Usually","Always"))# considerar una variable categórica como una ordinal
}
str(dt3$OQ17.1)
levels(dt3$OQ17.1)

for(o in QLikertCantidad){
  dt3[,paste("O",o,sep="")]=ordered(dt3[[o]],levels = c("Almost nothing","Just a little","Half","Almost totally","Completely"))# considerar una variable categórica como una ordinal
}
str(dt3$OQ22.2)
levels(dt3$OQ22.2)

colnames(dt3)[colSums(is.na(dt3)) > 0]#Revisar las columnas con empty values, solo debe salir 22.7, 14, 14.1, 14.2, 15, 18.2, 26.2, 35, 36, 37, 38, 39


#-----------------------------------------------------------------
#5. STACK SURVEY AND STACK MULTI (UNNESTED)
#-----------------------------------------------------------------

#PRUEBA MULTIRESPUESTA COMO UNNESTED

#install.packages("devtools")
#devtools::install_github("dgrtwo/stacksurveyr")
#install.packages("surveydata")
#install.packages("dplyr")

#library(stacksurveyr)
library(dplyr)

#LEER
stack_survey <- dt2
stack_schema <- read_excel("C:/Users/luisa/Dropbox/Universidad EAFIT - Maestría en Ingeniería - 2018/Trabajo/Contenido del Trabajo/Resultados/Software Reuse_R_Binary_Depurado.xlsx", sheet = "schema")
#stack_schema <- read_excel("C:/Users//Proyecto/Dropbox/Universidad EAFIT - Maestría en Ingeniería - 2018/Trabajo/Contenido del Trabajo/Resultados/Software Reuse_R_Binary_Depurado.xlsx", sheet = "schema")
#stack_schema

#CONOCER MULTI RESPUESTA
#stack_schema %>% filter(stack_schema$type == "multi")

#VER VALORES MULTIRESPUESTA
#stack_survey %>%
#  filter(!is.na(Q32)) %>%
#  select(Q32)

#GUARDAR UNNESTED
#SM <- stack_multi("Q4")
#SM
#stack_multi("Q4") %>% count(tech = answer, sort = TRUE)

#SM_Q5_TOTAL <- stack_multi("Q4") %>% count(tech = answer, sort = TRUE)
#SM_Q5_TOTAL = SM_Q5_TOTAL[-1,]
#SM_Q5_TOTAL

#RELACIONAR DOS VARIABLES
#stack_survey %>%
 # filter(Q18.1 == "Strong") %>%
 # inner_join(stack_multi("Q2"), by = "respondent_id") %>%
 # count(answer, sort = TRUE)

#GRAFICAR CON 3 VARIABLES
#stack_survey %>%
 # inner_join(stack_multi("Q2")) %>%
 # group_by(answer) %>%
 # summarize_each(funs(mean(., na.rm = TRUE)), Q8, Q1.4) %>%
  #ggplot(aes(Q8, Q1.4)) +
 # geom_point() +
  #geom_text(aes(label = answer), vjust = 1, hjust = 1) +
 # xlab("Average age of people using this technology") +
  #ylab("Average salary (USD)") +
  #scale_y_continuous(labels = dollar_format())


#stack_survey$Q2 <- as.factor(stack_survey$Q2)
###funcion
stack_multi <- function(columns = NULL) {
  multi_response_qs <- stack_schema$column[stack_schema$type == "multi"]
  
  if (is.null(columns)) {
    columns <- multi_response_qs
  } else {
    dif <- setdiff(columns, multi_response_qs)
    if (length(dif) > 0) {
      stop(dif[1], " is not a multi-response column in stack_survey")
    }
  }
  
  stack_survey %>%
    dplyr::select(respondent_id, one_of(columns)) %>%
    tidyr::gather(column, answer, -respondent_id) %>%
    dplyr::filter(!is.na(answer)) %>%
    tidyr::unnest(answer = stringr::str_split(answer, ";"))
}




###########