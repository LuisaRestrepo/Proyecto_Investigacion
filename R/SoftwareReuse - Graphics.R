

#-----------------------------------------------------------------
#4. GRAPHICS
#-----------------------------------------------------------------
#Generality for graphics
rs <- factor(dt2$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))
root <- file.path("")



#Tabla de contingencia
with(dt, table(dt2$Q18.1, dt2$Q3))

#-----------------------------------------------------------------
#4.1 OVERALL RESULTS
#-----------------------------------------------------------------

#######REGION
rg <- factor(dt2$Q1.4,levels = c("South center","LLanos","Caribbean","Pacific","Coffee belt","Middle east"))

ggplot(dt2, aes(rg, fill = rs )) + geom_bar() +  coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") + ####LABELS DE LA TABLA
  scale_fill_grey(start = .9, end = .3) + ####COLORES DE LAS BARRAS
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + ####FONDE DEL GRÁFICO
  geom_text(aes(label=..count.., group = rg),stat = "count", position = position_stack(1.05))

with(dt, prop.table(table(dt2$Q1.4,dt2$Q18.1), margin = 1))

#######ROLE
SM_Q2 <- stack_multi("Q2")
SM_Q2
SM_Q2_total <- stack_multi("Q2") %>% count(tech = answer, sort = TRUE)
SM_Q2_total
SM_Q2_total = SM_Q2_total[-1,]
SM_Q2_total$P <- SM_Q2_total$n/ 367 #######OJO CAMBIAR 267
SM_Q2_total

SM_Q2_total$tech[SM_Q2_total$tech=="Director de la compañía"] <- "Chief Information Officer (CIO)"
SM_Q2_total$tech[SM_Q2_total$tech=="Desarrollador de Software Backend"] <- "Backend Software Developer"
SM_Q2_total$tech[SM_Q2_total$tech=="Analista de desarrollo de software"] <- "Software Development Analyst"
SM_Q2_total$tech[SM_Q2_total$tech=="Arquitecto de Software"] <- "Software Architect"
SM_Q2_total$tech[SM_Q2_total$tech=="Desarrollador de Software Frontend"] <- "Frontend Software Developer"
SM_Q2_total$tech[SM_Q2_total$tech=="Director del área de sistemas"] <- "Chief Technology Officer (CTO)"
SM_Q2_total$tech[SM_Q2_total$tech=="Consultor"] <- "Consultant"
SM_Q2_total$tech[SM_Q2_total$tech=="Administrador de bases de datos"] <- "Database Administrator"
SM_Q2_total$tech[SM_Q2_total$tech=="Desarrollador de componentes"] <- "Component's Developer"
SM_Q2_total$tech[SM_Q2_total$tech=="Otro"] <- "Other"
SM_Q2_total$tech[SM_Q2_total$tech=="Analista de pruebas"] <- "Test Analyst"
SM_Q2_total$tech[SM_Q2_total$tech=="Analista del negocio (BA)"] <- "Business Analyst (BA)"
SM_Q2_total$tech[SM_Q2_total$tech=="Ingeniero de Requisitos"] <- "Requirements Engineer"
SM_Q2_total$tech[SM_Q2_total$tech=="Entrenador"] <- "Coach"

install.packages(c("readxl","writexl")) 
library(readxl)
library(writexl)

write.xlsx(SM_Q2_total, root , sheetName = "Rols", append = TRUE)


ggplot(SM_Q2_total, aes( x = reorder(SM_Q2_total$tech, SM_Q2_total$n), y = SM_Q2_total$n), cex.axis=0.5 )+ 
  labs( x="", y="", title="") +
  theme(text = element_text(size=10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + ####FONDE DEL GRÁFICO
  geom_bar(stat="identity") +
  coord_flip() 

stack_survey %>%
  filter(Q3 == "Large") %>%
  inner_join(stack_multi("Q2"), by = "respondent_id") %>%
  count(answer, sort = TRUE)

itcs <- stack_survey %>% 
  dplyr::select(respondent_id,Q3) %>%
  inner_join(stack_multi("Q2"), by = "respondent_id") %>%
  filter(answer != "")
itcs2 <- data.frame(itcs)
with(dt, prop.table(table(itcs2$Q3,itcs2$answer), margin = 1))


#SOFTWARE DOMAINS
SM_Q6 <- stack_multi("Q6")
SM_Q6
SM_Q6_total <- stack_multi("Q6") %>% count(tech = answer, sort = TRUE)
SM_Q6_total = SM_Q6_total[-1,]
SM_Q6_total$P <- SM_Q6_total$n/ 367 ####OJO CAMBIAR NUMEROOOO


SM_Q6_total$tech[SM_Q6_total$tech=="Desarrollo de software a la medida"] <- "Custom software development"
SM_Q6_total$tech[SM_Q6_total$tech=="Desarrollo de nuevos productos"] <- "Development of new products"
SM_Q6_total$tech[SM_Q6_total$tech=="Integraciones de software y aplicaciones"] <- "Software and application integrations"
SM_Q6_total$tech[SM_Q6_total$tech=="Actividades de consultoría informática"] <- "Computer consultancy activities"
SM_Q6_total$tech[SM_Q6_total$tech=="Desarrollo inhouse (Software producido por la empresa con el propósito de usarla dentro de la empresa)."] <- "Inhouse Development"
SM_Q6_total$tech[SM_Q6_total$tech=="Mantenimiento de software"] <- "Software maintenance"
SM_Q6_total$tech[SM_Q6_total$tech=="Pruebas de software"] <- "Software testing"
SM_Q6_total$tech[SM_Q6_total$tech=="Programas de formación TI"] <- "IT training programs"
SM_Q6_total$tech[SM_Q6_total$tech=="Administración de instalaciones informáticas"] <- "Administration of computer facilities"
SM_Q6_total$tech[SM_Q6_total$tech=="Otro"] <- "SaaS (Software as a Service)"

write.xlsx(SM_Q6_total, root , sheetName = "SoftwareDomains", append = TRUE)

#empresa q3, region 1.4
itcs <- stack_survey %>% 
  dplyr::select(respondent_id,Q1.4) %>%
  inner_join(stack_multi("Q6"), by = "respondent_id") %>%
  filter(answer != "")
itcs2 <- data.frame(itcs)
with(dt, prop.table(table(itcs2$answer,itcs2$Q3), margin = 1))
with(dt, prop.table(table(itcs2$Q3,itcs2$answer), margin = 1))

with(dt, prop.table(table(itcs2$Q1.4,itcs2$answer), margin = 1))

#-----------------------------------------------------------------
#4.2 BUSINESS FACTORS
#-----------------------------------------------------------------

#######APPLICATION DOMAIN
SM_Q4 <- stack_multi("Q4")
SM_Q4
SM_Q4_total <- stack_multi("Q4") %>% count(tech = answer, sort = TRUE)
SM_Q4_total = SM_Q4_total[-1,]
SM_Q4_total$P <- SM_Q4_total$n/ 367#sum(SM_Q4_total$n)
SM_Q4_total

SM_Q4_total$tech[SM_Q4_total$tech=="Tecnologías de información y comunicaciones (TIC)"] <- "Information and communications technologies (ICT)"
SM_Q4_total$tech[SM_Q4_total$tech=="Educación"] <- "Education"
SM_Q4_total$tech[SM_Q4_total$tech=="Financiero"] <- "Financial"
SM_Q4_total$tech[SM_Q4_total$tech=="Salud y seguridad social"] <- "Health and social security"
SM_Q4_total$tech[SM_Q4_total$tech=="Servicios de gestión empresarial"] <- "Business management services"
SM_Q4_total$tech[SM_Q4_total$tech=="Logística"] <- "Logistics"
SM_Q4_total$tech[SM_Q4_total$tech=="Comunicaciones"] <- "Communications"
SM_Q4_total$tech[SM_Q4_total$tech=="Gobierno"] <- "Government"
SM_Q4_total$tech[SM_Q4_total$tech=="Telecomunicaciones"] <- "Telecommunications"
SM_Q4_total$tech[SM_Q4_total$tech=="Alimentos y bebidas"] <- "Food and drinks"
SM_Q4_total$tech[SM_Q4_total$tech=="Construcción e ingeniería"] <- "Construction and Engineering"
SM_Q4_total$tech[SM_Q4_total$tech=="Seguros"] <- "Insurance"
SM_Q4_total$tech[SM_Q4_total$tech=="Agroindustria"] <- "Agroindustry"
SM_Q4_total$tech[SM_Q4_total$tech=="Manufactura"] <- "Manufacture"
SM_Q4_total$tech[SM_Q4_total$tech=="Turismo y entretenimiento"] <- "Tourism and entertainment"
SM_Q4_total$tech[SM_Q4_total$tech=="Energía, petróleo y gas"] <- "Energy, oil and gas"
SM_Q4_total$tech[SM_Q4_total$tech=="Finca raíz"] <- "Real estate"
SM_Q4_total$tech[SM_Q4_total$tech=="Legal"] <- "Legal"
SM_Q4_total$tech[SM_Q4_total$tech=="Minería"] <- "Mining"
SM_Q4_total$tech[SM_Q4_total$tech=="Otro"] <- "Other"

write.xlsx(SM_Q4_total, root , sheetName = "ApplicationDomains", append = TRUE)

AD <- stack_survey %>% 
  dplyr::select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q4"), by = "respondent_id") %>%
  filter(answer != "") 

AD$answer[AD$answer=="Tecnologías de información y comunicaciones (TIC)"] <- "Inf. and comms. technologies (ICT)"
AD$answer[AD$answer=="Educación"] <- "Education"
AD$answer[AD$answer=="Financiero"] <- "Financial"
AD$answer[AD$answer=="Salud y seguridad social"] <- "Health and social security"
AD$answer[AD$answer=="Servicios de gestión empresarial"] <- "Business management services"
AD$answer[AD$answer=="Logística"] <- "Logistics"
AD$answer[AD$answer=="Comunicaciones"] <- "Communications"
AD$answer[AD$answer=="Gobierno"] <- "Government"
AD$answer[AD$answer=="Telecomunicaciones"] <- "Telecommunications"
AD$answer[AD$answer=="Alimentos y bebidas"] <- "Food and drinks"
AD$answer[AD$answer=="Construcción e ingeniería"] <- "Construction and Engineering"
AD$answer[AD$answer=="Seguros"] <- "Insurance"
AD$answer[AD$answer=="Agroindustria"] <- "Agroindustry"
AD$answer[AD$answer=="Manufactura"] <- "Manufacture"
AD$answer[AD$answer=="Turismo y entretenimiento"] <- "Tourism and entertainment"
AD$answer[AD$answer=="Energía, petróleo y gas"] <- "Energy, oil and gas"
AD$answer[AD$answer=="Finca raíz"] <- "Real estate"
AD$answer[AD$answer=="Legal"] <- "Legal"
AD$answer[AD$answer=="Minería"] <- "Mining"
AD$answer[AD$answer=="Otro"] <- "Other"

AD2 <- data.frame(AD)

adp <- count(AD2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

rs <- factor(adp$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(adp$answer, adp$n, function(x){ sum(x) }), adp$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_text(aes(label = adp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") #+
  stat_summary(fun.y = sum, aes(label = ..y.., group = adp$answer), size = 3, geom = "text", position = position_nudge(0,3))


with(dt, prop.table(table(AD$Q18.1,AD$answer), margin = 1))
with(dt, prop.table(table(AD$answer,AD$Q18.1), margin = 1))

#######TYPE OF SOFTWARE
SM_Q7 <- stack_multi("Q7")
SM_Q7
SM_Q7_total <- stack_multi("Q7") %>% count(tech = answer, sort = TRUE)
SM_Q7_total 
SM_Q7_total = SM_Q7_total[-1,]
SM_Q7_total$P <- SM_Q7_total$n/ 367
SM_Q7_total

SM_Q7_total$tech[SM_Q7_total$tech=="Aplicaciones Web"] <- "Web applications"
SM_Q7_total$tech[SM_Q7_total$tech=="Aplicaciones para dispositivos móviles"] <- "Applications for mobile devices"
SM_Q7_total$tech[SM_Q7_total$tech=="Sistemas de información para la gestión empresarial"] <- "Information systems for business management"
SM_Q7_total$tech[SM_Q7_total$tech=="Aplicaciones de escritorio"] <- "Desktop applications"
SM_Q7_total$tech[SM_Q7_total$tech=="Aplicaciones  para analítica de datos"] <- "Applications for data analytics"
SM_Q7_total$tech[SM_Q7_total$tech=="Aplicaciones para la inteligencia de negocios (BI)"] <- "Applications for business intelligence (BI)"
SM_Q7_total$tech[SM_Q7_total$tech=="Software integrado en un dispositivo eléctrico"] <- "Software integrated in an electrical device"
SM_Q7_total$tech[SM_Q7_total$tech=="Sistemas IoT"] <- "IoT systems"
SM_Q7_total$tech[SM_Q7_total$tech=="Soluciones basadas en inteligencia artificial"] <- "Solutions based on artificial intelligence"
SM_Q7_total$tech[SM_Q7_total$tech=="Digital Content"] <- "Digital Content"
SM_Q7_total$tech[SM_Q7_total$tech=="Realidad virtual y/o aumentada"] <- "Virtual and/or augmented reality"
SM_Q7_total$tech[SM_Q7_total$tech=="Videojuegos"] <- "Video games"
SM_Q7_total$tech[SM_Q7_total$tech=="Otro"] <- "Other"

write.xlsx(SM_Q7_total, root , sheetName = "SoftwareType", append = TRUE)

TS <- stack_survey %>% 
  dplyr::select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q7"), by = "respondent_id") %>%
  filter(answer != "") 

TS$answer[TS$answer=="Aplicaciones Web"] <- "Web applications"
TS$answer[TS$answer=="Aplicaciones para dispositivos móviles"] <- "Applications for mobile devices"
TS$answer[TS$answer=="Sistemas de información para la gestión empresarial"] <- "Information systems for business management"
TS$answer[TS$answer=="Aplicaciones de escritorio"] <- "Desktop applications"
TS$answer[TS$answer=="Aplicaciones  para analítica de datos"] <- "Applications for data analytics"
TS$answer[TS$answer=="Aplicaciones para la inteligencia de negocios (BI)"] <- "Applications for business intelligence (BI)"
TS$answer[TS$answer=="Software integrado en un dispositivo eléctrico"] <- "Software integrated in an electrical device"
TS$answer[TS$answer=="Sistemas IoT"] <- "IoT systems"
TS$answer[TS$answer=="Soluciones basadas en inteligencia artificial"] <- "Solutions based on artificial intelligence"
TS$answer[TS$answer=="Digital Content"] <- "Digital Content"
TS$answer[TS$answer=="Realidad virtual y/o aumentada"] <- "Virtual and/or augmented reality"
TS$answer[TS$answer=="Videojuegos"] <- "Video games"
TS$answer[TS$answer=="Otro"] <- "Other"

TS2 <- data.frame(TS)

tsp <- count(TS2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

rs <- factor(tsp$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(tsp$answer, tsp$n, function(x){ sum(x) }), tsp$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_text(aes(label = tsp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") #+
  stat_summary(fun.y = sum, aes(label = ..y.., group = tsp$answer), size = 3, geom = "text", position = position_nudge(0,3))

with(dt, prop.table(table(TS$answer,TS$Q18.1), margin = 1))
with(dt, prop.table(table(TS$Q18.1,TS$answer), margin = 1))

#empresa q3, region 1.4
itcs <- stack_survey %>% 
  dplyr::select(respondent_id,Q3) %>%
  inner_join(stack_multi("Q7"), by = "respondent_id") %>%
  filter(answer != "")
itcs2 <- data.frame(itcs)
with(dt, prop.table(table(itcs2$answer,itcs2$Q3), margin = 1))
with(dt, prop.table(table(itcs2$Q3,itcs2$answer), margin = 1))

with(dt, prop.table(table(itcs2$Q1.4,itcs2$answer), margin = 1))

#######PRODUCT FAMILY APPROACH
PAF <- stack_survey %>% 
  dplyr::select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q30"), by = "respondent_id") %>%
  filter(answer != "") 

PAF2 <- data.frame(PAF)

PAF2$answer[PAF2$answer=="Otro"] <- "Aislado"

pafp <- count(PAF2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

rs <- factor(pafp$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(pafp$answer, pafp$n, function(x){ sum(x) }), pafp$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_text(aes(label = pafp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") #+
  stat_summary(fun.y = sum, aes(label = ..y.., group = pafp$answer), size = 3, geom = "text", position = position_nudge(0,3))

with(dt, prop.table(table(PAF2$answer,PAF2$Q18.1), margin = 1))
with(dt, prop.table(table(dt2$Q34,dt2$Q18.1), margin = 1))
install.packages("expss")
library(expss)
count_if("SI",dt3$Q34)

itcs <- stack_survey %>% 
  dplyr::select(respondent_id,Q18.1,Q3) %>%
  inner_join(stack_multi("Q30"), by = "respondent_id") %>%
  filter(answer != "")
itcs2 <- data.frame(itcs)
with(dt, prop.table(table(itcs2$answer,itcs2$Q18.1), margin = 1))

dt_startups <- filter_at(itcs2, vars(Q3), any_vars( . == "Startups")  , .preserve = TRUE)
with(dt, prop.table(table(dt_startups$answer,dt_startups$Q18.1), margin = 1))

dt_Microenterprise <- filter_at(itcs2, vars(Q3), any_vars( . == "Microenterprise")  , .preserve = TRUE)
dt_Small <- filter_at(itcs2, vars(Q3), any_vars( . == "Small")  , .preserve = TRUE)
dt_Medium <- filter_at(itcs2, vars(Q3), any_vars( . == "Medium")  , .preserve = TRUE)
dt_Large <- filter_at(itcs2, vars(Q3), any_vars( . == "Large")  , .preserve = TRUE)



#######DOMAIN ENGINEERING 31.1 MARKET ANALYSIS, 31.2 REQUIREMENTS, 31.3 ARQUITECTURA DIFERENCIA ENTRE PRODUCTOS

library(likert)
sn1 <- data.frame(A = dt3$OQ31.1,B = dt3$OQ31.2, C=dt3$OQ31.3)
sn1$A<- factor(sn1$A, levels=levels(sn1$A)[c(1,2,3,4,5)])
sn1$B <- factor(sn1$B, levels=levels(sn1$B)[c(1,2,3,4,5)])
sn1$C <- factor(sn1$C, levels=levels(sn1$C)[c(1,2,3,4,5)])
#names(sn1) = c("Systematic process","Reuse measurement","Certification process")

likert3 <- likert(items=sn1[,1:3, drop = FALSE], nlevels = 5)

summary(likert3)

plot(likert3,low.color = "#AEAEAE", high.color = "#4D4D4D", group.order=c('A', 'B', 'C'),legend.position = "right")

plot(dt3$OQ16.3)
with(dt, table(dt3$OQ31.1, dt2$Q18.1))
with(dt, prop.table(table(dt3$OQ31.1,dt2$Q18.1), margin = 1))
with(dt, prop.table(table(dt3$OQ31.2,dt2$Q18.1), margin = 1))
with(dt, prop.table(table(dt3$OQ31.3,dt2$Q18.1), margin = 1))



#-----------------------------------------------------------------
#4.3 ORGANIZATIONAL FACTORS
#-----------------------------------------------------------------

#######COMPANY SIZE
  csp <- count(dt2,Q3,Q18.1) %>% 
    group_by(Q3) %>% 
    mutate(n1 = paste0(round(n/sum(n) * 100), "%"))
  
  rs <- factor(csp$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))
  cs <- factor(csp$Q3, levels = c("Large","Medium","Small","Microenterprise","Startups"))
  
  
  ggplot() + aes(cs, csp$n, fill = rs) + geom_col() + 
    labs( x="", y="", title="", fill ="Reuse success level") +
    scale_fill_grey(start = .9, end = .3) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    geom_text(aes(label = csp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
    stat_summary(fun.y = sum, aes(label = ..y.., group = cs), size = 3, geom = "text", position = position_nudge(0,3))
  
  with(dt, prop.table(table(dt2$Q3,dt2$Q18.1), margin = 1)) 
  
#######TEAM SIZE
  tsp <- count(dt2,Q8NN,Q18.1) %>% 
    group_by(Q8NN) %>% 
    mutate(n1 = paste0(round(n/sum(n) * 100), "%"))
  
  tsp
  count(dt2,Q8)
  count(dt2,Q8NN)
  with(dt, table(tsp$Q8NN,tsp$Q18.1), margin = 1)
  ts <- factor(tsp$Q8NN, levels = c("1-3","4-5","6-10","11+"))
  rs <- factor(tsp$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))
  
  
  ggplot() + aes(ts, tsp$n, fill = rs) + geom_col() + 
    labs( x="", y="", title="", fill ="Reuse success level") +
    scale_fill_grey(start = .9, end = .3) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    geom_text(aes(label = tsp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
    stat_summary(fun.y = sum, aes(label = ..y.., group = ts), size = 3, geom = "text", position = position_nudge(0,3))
  
  with(dt, prop.table(table(dt2$Q8NN,dt2$Q18.1), margin = 1)) 

#ggplot(dt2, aes(ts, cs))+ 
#geom_line()+
#geom_point()
  
#######PROJECT SIZE
  DA <- stack_survey %>% 
    dplyr::select(respondent_id,Q18.1) %>%
    inner_join(stack_multi("Q10"), by = "respondent_id") %>%
    filter(answer != "") 
  
  DA2 <- data.frame(DA)
  
  count(DA2, answer)
  #PL2$answer[PL2$answer=="Scala"] <- "Other"
  #PL2$answer[PL2$answer=="Otro"] <- "Other"
  
  dap <- count(DA2,answer,Q18.1) %>% 
    group_by(answer) %>% 
    mutate(n1 = paste0(round(n/sum(n) * 100), "%"))
  
  dap <- data.frame(dap)
  
  rsdap <- factor(dap$Q18.1 ,levels = c("Strong","Substancial","Moderate","Limited","No success"))
  
  ggplot() + aes(reorder(dap$answer, dap$n, function(x){ sum(x) }), dap$n, fill = rsdap) + geom_col() + coord_flip() +
    labs( x="", y="", title="", fill ="Reuse success level") +
    scale_fill_grey(start = .9, end = .3) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    #geom_text(aes(label = dap$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
    stat_summary(fun.y = sum, aes(label = ..y.., group = dap$answer), size = 3, geom = "text", position = position_nudge(0,3))
  
  with(dt, prop.table(table(DA2$answer,DA2$Q18.1), margin = 1))
  with(dt, prop.table(table(DA2$Q18.1,DA2$answer), margin = 1))
  
  dacs <- stack_survey %>% 
    dplyr::select(respondent_id,Q3) %>%
    inner_join(stack_multi("Q10"), by = "respondent_id") %>%
    filter(answer != "")
  dacs2 <- data.frame(dacs)
  with(dacs2, prop.table(table(dacs2$Q3,dacs2$answer), margin = 1))
  with(dacs2, prop.table(table(dacs2$answer,dacs2$Q3), margin = 1))

#######TEAM EXPERIENCE
SM_Q9 <- stack_multi("Q9")
SM_Q9
SM_Q9_total <- stack_multi("Q9") %>% count(tech = answer, sort = TRUE)
SM_Q9_total
SM_Q9_total = SM_Q9_total[-1,]
SM_Q9_total

SM_Q9_total$tech[SM_Q9_total$tech=="Los miembros del equipo tienen más nivel Senior que Junior/sin experiencia"] <- "More Senior than Junior"
SM_Q9_total$tech[SM_Q9_total$tech=="Todos los miembros del equipo de software tienen mínimo nivel Senior"] <- "All Senior"
SM_Q9_total$tech[SM_Q9_total$tech=="Los miembros del equipo tienen más nivel Junior/sin experiencia que Senior"] <- "More Junior than Senior"
SM_Q9_total$tech[SM_Q9_total$tech=="Todos los miembros del equipo de software tienen máximo nivel Junior"] <- "All Junior"

TE <- stack_survey %>% 
  dplyr::select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q9"), by = "respondent_id") %>%
  filter(answer != "") 

TE$answer[TE$answer=="Los miembros del equipo tienen más nivel Senior que Junior/sin experiencia"] <- "More Senior than Junior"
TE$answer[TE$answer=="Todos los miembros del equipo de software tienen mínimo nivel Senior"] <- "All Senior"
TE$answer[TE$answer=="Los miembros del equipo tienen más nivel Junior/sin experiencia que Senior"] <- "More Junior than Senior"
TE$answer[TE$answer=="Todos los miembros del equipo de software tienen máximo nivel Junior"] <- "All Junior"

TE2 <- data.frame(TE)

tep <- count(TE2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

rs <- factor(tep$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(tep$answer, tep$n, function(x){ sum(x) }), tep$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(label = tep$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") #+
  stat_summary(fun.y = sum, aes(label = ..y.., group = tep$answer), size = 3, geom = "text", position = position_nudge(0,3))


TE2 <- stack_survey %>% 
  select(respondent_id,Q3)%>%
inner_join(TE, by = "respondent_id")%>%
  count(Q3, answer, sort = FALSE)

with(TE2, prop.table(table(TE2$answer,TE2$Q18.1), margin = 1))
#######EDUCATION AND MOTIVATION: LIKERT
#######LA ALTA DIRECCION: LIKERT
library(likert)
sn <- data.frame(C = dt3$OQ17.1, D = dt3$OQ17.2)
sn$C <- factor(sn$C, levels=levels(sn$C)[c(1,2,3,4,5)])
sn$D <- factor(sn$D, levels=levels(sn$D)[c(1,2,3,4,5)])
names(sn) = c("M. commitment","Education")
likert1 <- likert(items=sn[,1:2, drop = FALSE], nlevels = 5)
summary(likert1)
plot(likert1,low.color = "#AEAEAE", high.color = "#4D4D4D", legend.position = "right",group.order=c('M. commitment', 'Education'),legend="Response")

sn1 <- data.frame(A = dt3$OQ16.1, B = dt3$OQ16.2)
sn1$A<- factor(sn1$A, levels=levels(sn1$A)[c(1,2,3,4,5)])
sn1$B <- factor(sn1$B, levels=levels(sn1$B)[c(1,2,3,4,5)])
names(sn1) = c("E. Feasibility","Incentives")
likert2 <- likert(items=sn1[,1:2, drop = FALSE], nlevels = 5)
summary(likert2)
plot(likert2,low.color = "#AEAEAE", high.color = "#4D4D4D", legend.position = "right",group.order=c('E. Feasibility', 'Incentives'),legend="Response")

with(dt, table(dt2$Q17.2, dt2$Q18.1))
with(dt, prop.table(table(dt2$Q18.1,dt2$Q17.2), margin = 1))
with(dt, prop.table(table(dt2$Q17.2,dt2$Q18.1), margin = 1))
plot(dt3$OQ17.2,dt3$Q3)

with(dt, prop.table(table(dt2$Q18.1,dt2$Q16.1), margin = 1))
with(dt, prop.table(table(dt2$Q16.1,dt2$Q18.1), margin = 1))
with(dt, prop.table(table(dt2$Q3,dt2$Q16.1), margin = 1))

with(dt, prop.table(table(dt2$Q18.1,dt2$Q16.2), margin = 1))
with(dt, prop.table(table(dt2$Q16.2,dt2$Q18.1), margin = 1))
with(dt, prop.table(table(dt2$Q3,dt2$Q16.2), margin = 1))

count(dt2, Q17.1)
with(dt, prop.table(table(dt2$Q18.1,dt2$Q17.1), margin = 1))
with(dt, prop.table(table(dt2$Q17.1,dt2$Q18.1), margin = 1))
with(dt, prop.table(table(dt2$Q3,dt2$Q17.1), margin = 1))

with(dt, prop.table(table(dt2$Q17.2,dt2$Q18.1), margin = 1))
with(dt, prop.table(table(dt2$Q3,dt2$Q17.2), margin = 1))


#######INDEPENDENT TEAM
IT <- stack_survey %>% 
  dplyr::select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q24"), by = "respondent_id") %>%
  filter(answer != "") 

IT$answer[IT$answer=="La empresa tiene un rol dedicado a la creación de artefactos reutilizables."] <- "ROL"
IT$answer[IT$answer=="La empresa tiene un equipo independiente que desarrolla artefactos reutilizables."] <- "TEAM"
IT$answer[IT$answer=="La empresa contrata un tercero para el desarrollo de artefactos."] <- "THIRD PARTY"
IT$answer[IT$answer=="Ninguna de los anteriores."] <- "None"
IT$answer[IT$answer=="Otro"] <- "None"

IT2 <- data.frame(IT)
count(IT2, answer)

itp <- count(IT2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

rs <- factor(itp$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(itp$answer, itp$n, function(x){ sum(x) }), itp$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(label = itp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  stat_summary(fun.y = sum, aes(label = ..y.., group = itp$answer), size = 3, geom = "text", position = position_nudge(0,3))

with(dt, prop.table(table(IT2$answer,IT2$Q18.1), margin = 1))
with(dt, prop.table(table(IT2$Q18.1,IT2$answer), margin = 1))

itcs <- stack_survey %>% 
  dplyr::select(respondent_id,Q3) %>%
  inner_join(stack_multi("Q24"), by = "respondent_id") %>%
  filter(answer != "")
itcs2 <- data.frame(itcs)
with(dt, prop.table(table(itcs2$Q3,itcs2$answer), margin = 1))

#-----------------------------------------------------------------
#4.4 PROCESS FACTORS
#-----------------------------------------------------------------
#######QUALITY MODELS USAGE
QM <- stack_survey %>% 
  dplyr::select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q5"), by = "respondent_id") %>%
  filter(answer != "") 

QM$answer[QM$answer=="Ninguno"] <- "None"
QM$answer[QM$answer=="ISO 9000/1"] <- "Other"
QM$answer[QM$answer=="ISO 29110"] <- "Other"
QM$answer[QM$answer=="Propia"] <- "Other"
QM$answer[QM$answer=="Otro"] <- "Other"

QM2 <- data.frame(QM)
count(QM2, answer)

qmp <- count(QM2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

rs <- factor(qmp$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(qmp$answer, qmp$n, function(x){ sum(x) }), qmp$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_text(aes(label = qmp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  stat_summary(fun.y = sum, aes(label = ..y.., group = qmp$answer), size = 3, geom = "text", position = position_nudge(0,3))

with(dt, prop.table(table(QM2$answer,QM2$Q18.1), margin = 1))
with(dt, prop.table(table(QM2$Q18.1,QM2$answer), margin = 1))

QMcs <- stack_survey %>% 
  dplyr::select(respondent_id,Q3) %>%
  inner_join(stack_multi("Q5"), by = "respondent_id") %>%
  filter(answer != "")
QMcs2 <- data.frame(QMcs)
with(dt, prop.table(table(QMcs2$Q3,QMcs2$answer), margin = 1))

########## SYSTEMATIC REUSE PROCESS 16.3
########## REUSE MEASURE 16.5
########## CERTIFICAR COMPONENTES 27

library(likert)
sn1 <- data.frame(A = dt3$OQ16.3,B = dt3$OQ16.5, C=dt3$OQ27)
sn1$A<- factor(sn1$A, levels=levels(sn1$A)[c(1,2,3,4,5)])
sn1$B <- factor(sn1$B, levels=levels(sn1$B)[c(1,2,3,4,5)])
sn1$C <- factor(sn1$C, levels=levels(sn1$C)[c(1,2,3,4,5)])
names(sn1) = c("Systematic process","Reuse measurement","Certification process")

likert3 <- likert(items=sn1[,1:3, drop = FALSE], nlevels = 5)

summary(likert3)

plot(likert3,low.color = "#AEAEAE", high.color = "#4D4D4D", group.order=c('Systematic process', 'Reuse measurement', 'Certification process'),legend.position = "right")

count(dt3, dt3$OQ16.3)
with(dt, table(dt3$OQ16.3, dt2$Q18.1))

with(dt, prop.table(table(dt3$OQ16.3,dt3$Q18.1), margin = 1))
with(dt, prop.table(table(dt3$Q3,dt3$OQ16.3), margin = 1))

with(dt, prop.table(table(dt3$OQ16.5,dt3$Q18.1), margin = 1))
with(dt, prop.table(table(dt3$Q3,dt3$OQ16.5), margin = 1))
with(dt, prop.table(table(dt3$OQ16.5,dt3$Q3), margin = 1))

with(dt, prop.table(table(dt3$OQ27,dt3$Q18.1), margin = 1))
with(dt, prop.table(table(dt3$Q3,dt3$OQ27), margin = 1))

########## Development of assets for reuse Q19, Q20
### Q19 artefactos creados en las empresas
QM <- stack_survey %>% 
  dplyr::select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q19"), by = "respondent_id") %>%
  filter(answer != "") 

QM$answer[QM$answer=="Código fuente"] <- "Source code"
QM$answer[QM$answer=="Planes de prueba"] <- "Test plans"
QM$answer[QM$answer=="Casos de prueba"] <- "Test cases"
QM$answer[QM$answer=="Artefacto de despliegue"] <- "Deployment Artifacts"
QM$answer[QM$answer=="Documentación del usuario"] <- "User Documentation"
QM$answer[QM$answer=="Diseño de alto nivel (Arquitectura)"] <- "High level design (Architecture)"
QM$answer[QM$answer=="Diseño detallado"] <- "Detailed design"
QM$answer[QM$answer=="Librerias"] <- "Libraries"
QM$answer[QM$answer=="Componentes de software"] <- "Software components"
QM$answer[QM$answer=="Requisitos"] <- "Requirements"
QM$answer[QM$answer=="Historias de Usuario"] <- "User Stories"
QM$answer[QM$answer=="Otro"] <- "Other"

QM2 <- data.frame(QM)
count(QM2, answer)

Q19 <- data.frame(count(QM2, answer, sort = TRUE))%>%  ######################OJO CAMBIAR EL 267
  mutate(n1 = paste0(round(n/367 * 100), "%"))

qmp <- count(QM2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

rs <- factor(qmp$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(qmp$answer, qmp$n, function(x){ sum(x) }), qmp$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_text(aes(label = qmp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  stat_summary(fun.y = sum, aes(label = ..y.., group = qmp$answer), size = 3, geom = "text", position = position_nudge(0,3))

with(dt, prop.table(table(QM2$answer,QM2$Q18.1), margin = 1))
with(dt, prop.table(table(QM2$Q18.1,QM2$answer), margin = 1))


### Q20 artefactos creados para ser reutilizables
QM <- stack_survey %>% 
  select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q20"), by = "respondent_id") %>%
  filter(answer != "") 

QM$answer[QM$answer=="Código fuente"] <- "Source code"
QM$answer[QM$answer=="Planes de prueba"] <- "Test plans"
QM$answer[QM$answer=="Casos de prueba"] <- "Test cases"
QM$answer[QM$answer=="Artefacto de despliegue"] <- "Deployment Artifacts"
QM$answer[QM$answer=="Documentación del usuario"] <- "User Documentation"
QM$answer[QM$answer=="Diseño de alto nivel (Arquitectura)"] <- "High level design (Architecture)"
QM$answer[QM$answer=="Diseño detallado"] <- "Detailed design"
QM$answer[QM$answer=="Librerias"] <- "Libraries"
QM$answer[QM$answer=="Componentes de software"] <- "Software components"
QM$answer[QM$answer=="Requisitos"] <- "Requirements"
QM$answer[QM$answer=="Historias de Usuario"] <- "User Stories"
QM$answer[QM$answer=="Ninguno"] <- "None"
QM$answer[QM$answer=="Otro"] <- "Other"

QM2 <- data.frame(QM)
Q20 <- data.frame(count(QM2, answer, sort = TRUE))%>%  ######################OJO CAMBIAR EL 267
  mutate(n1 = paste0(round(n/367 * 100), "%"))

qmp <- count(QM2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

rs <- factor(qmp$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(qmp$answer, qmp$n, function(x){ sum(x) }), qmp$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_text(aes(label = qmp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  stat_summary(fun.y = sum, aes(label = ..y.., group = qmp$answer), size = 3, geom = "text", position = position_nudge(0,3))

with(dt, prop.table(table(QM2$answer,QM2$Q18.1), margin = 1))
with(dt, prop.table(table(QM2$Q18.1,QM2$answer), margin = 1))


########## artefactos reutilizables se usan???  Q26.1

library(likert)
sn1 <- data.frame(A = dt3$OQ26.1)
sn1$A<- factor(sn1$A, levels=levels(sn1$A)[c(1,2,3,4,5,6,7)])
names(sn1) = c("Reusable asset")
likert3 <- likert(items=sn1[,1:1, drop = FALSE], nlevels = 7)

summary(likert3)

plot(likert3,low.color = "#9B9B9B", high.color = "#4D4D4D",legend.position = "right", legend="Frequency of use")

Q20Q261 <- stack_survey %>% 
  dplyr::select(respondent_id,Q26.1) %>%
  inner_join(stack_multi("Q20"), by = "respondent_id") %>%
  filter(answer != "")
Q20Q2612 <- data.frame(Q20Q261)

with(dt, prop.table(table(Q20Q2612$Q26.1,Q20Q2612$answer), margin = 1))
with(dt, prop.table(table(Q20Q2612$answer,Q20Q2612$Q26.1), margin = 1))


####Q26.1, union artefactos creados para reutilizar con su uso
count(dt2, Q26.1)
QM <- stack_survey %>% 
  select(respondent_id,Q26.1) %>%
  inner_join(stack_multi("Q20"), by = "respondent_id") %>%
  filter(answer != "") 

QM$answer[QM$answer=="Código fuente"] <- "Source code"
QM$answer[QM$answer=="Planes de prueba"] <- "Test plans"
QM$answer[QM$answer=="Casos de prueba"] <- "Test cases"
QM$answer[QM$answer=="Artefacto de despliegue"] <- "Deployment Artifacts"
QM$answer[QM$answer=="Documentación del usuario"] <- "User Documentation"
QM$answer[QM$answer=="Diseño de alto nivel (Arquitectura)"] <- "High level design (Architecture)"
QM$answer[QM$answer=="Diseño detallado"] <- "Detailed design"
QM$answer[QM$answer=="Librerias"] <- "Libraries"
QM$answer[QM$answer=="Componentes de software"] <- "Software components"
QM$answer[QM$answer=="Requisitos"] <- "Requirements"
QM$answer[QM$answer=="Historias de Usuario"] <- "User Stories"
QM$answer[QM$answer=="Ninguno"] <- "None"
QM$answer[QM$answer=="Otro"] <- "Other"

QM2 <- data.frame(QM)
Q20 <- data.frame(count(QM2, answer, sort = TRUE))%>%  ######################OJO CAMBIAR EL 267
  mutate(n1 = paste0(round(n/367 * 100), "%"))

qmp <- count(QM2,answer,Q26.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

rs <- factor(qmp$Q26.1,levels = c("Always", "Usually","Frequently","Sometimes","Occasionally","Hardly ever","Never"))

ggplot() + aes(reorder(qmp$answer, qmp$n, function(x){ sum(x) }), qmp$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Frequency of use") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_text(aes(label = qmp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  stat_summary(fun.y = sum, aes(label = ..y.., group = qmp$answer), size = 3, geom = "text", position = position_nudge(0,3))

with(QM2, prop.table(table(QM2$answer,QM2$Q18.1), margin = 1))

########## GESTION DE LA CONFIGURACIÓN Q21
QM <- stack_survey %>% 
  select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q21"), by = "respondent_id") %>%
  filter(answer != "") 

QM$answer[QM$answer=="Código fuente"] <- "Source code"
QM$answer[QM$answer=="Planes de prueba"] <- "Test plans"
QM$answer[QM$answer=="Casos de prueba"] <- "Test cases"
QM$answer[QM$answer=="Artefacto de despliegue"] <- "Deployment Artifacts"
QM$answer[QM$answer=="Documentación del usuario"] <- "User Documentation"
QM$answer[QM$answer=="Diseño de alto nivel (Arquitectura)"] <- "High level design (Architecture)"
QM$answer[QM$answer=="Diseño detallado"] <- "Detailed design"
QM$answer[QM$answer=="Librerias"] <- "Libraries"
QM$answer[QM$answer=="Componentes de software"] <- "Software components"
QM$answer[QM$answer=="Requisitos"] <- "Requirements"
QM$answer[QM$answer=="Historias de Usuario"] <- "User Stories"
QM$answer[QM$answer=="Ninguno"] <- "None"
QM$answer[QM$answer=="Otro"] <- "Other"

QM2 <- data.frame(QM)
with(QM2, prop.table(table(QM2$answer,QM2$Q18.1), margin = 1))

Q21 <- data.frame(count(QM2, answer, sort = TRUE))%>%
  mutate(n1 = paste0(round(n/367 * 100), "%"))

qmp <- count(QM2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

rs <- factor(qmp$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(qmp$answer, qmp$n, function(x){ sum(x) }), qmp$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_text(aes(label = qmp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  stat_summary(fun.y = sum, aes(label = ..y.., group = qmp$answer), size = 3, geom = "text", position = position_nudge(0,3))


Q19Q20 <- merge(Q19,Q20,by = "answer", all = TRUE)
Q19Q20Q21 <- merge(Q19Q20,Q21,by = "answer", all = TRUE)

write.xlsx(Q19Q20Q21, root , sheetName = "Assets", append = TRUE)

###
with(dt, prop.table(table(QM2$Q18.1,QM2$answer), margin = 1))
with(dt, prop.table(table(QM2$answer,QM2$Q18.1), margin = 1))

########## KIND OF REUSED ASSETS Q22.1 - Q22.6 - ASSETS CON REUTILIZACIÓN

library(likert)
sn1 <- data.frame(A = dt3$OQ22.1,B =dt3$OQ22.2,C=dt3$OQ22.3,D=dt3$OQ22.4,E=dt3$OQ22.5,F=dt3$OQ22.6)
sn1$A<- factor(sn1$A, levels=levels(sn1$A)[c(1,2,3,4,5)])
sn1$B <- factor(sn1$B, levels=levels(sn1$B)[c(1,2,3,4,5)])
sn1$C <- factor(sn1$C, levels=levels(sn1$C)[c(1,2,3,4,5)])
sn1$D <- factor(sn1$D, levels=levels(sn1$D)[c(1,2,3,4,5)])
sn1$E <- factor(sn1$E, levels=levels(sn1$E)[c(1,2,3,4,5)])
sn1$F <- factor(sn1$F, levels=levels(sn1$F)[c(1,2,3,4,5)])

names(sn1) = c("Requirements", "User stories", "Architecture", "Detailed design", "Software components", "Test cases")
likert3 <- likert(items=sn1[,1:6, drop = FALSE], nlevels = 5)

summary(likert3)

plot(likert3,low.color = "#AEAEAE", high.color = "#4D4D4D", group.order=c('Requirements', 'User stories', 'Architecture', 'Detailed design', 'Software components', 'Test cases'),legend.position = "right", legend="Created with reuse")


sn2 <- data.frame(A = dt3$OQ22.1,B =dt3$OQ22.2,C=dt3$OQ22.3,D=dt3$OQ22.4,E=dt3$OQ22.5,F=dt3$OQ22.6, G =dt3$Q18.1)
tablefiltered <- filter_at(sn2, vars(A,B,C,D,E,F), all_vars(. =="Completely"))
tablefiltered <- filter_at(sn2, vars(A,B,C,D,E,F), all_vars(. =="Almost totally"))
tablefiltered <- filter_at(sn2, vars(A,B,C,D,E,F), all_vars(. =="Almost nothing"))
tablefiltered <- filter_at(sn2, vars(A), all_vars(. =="Almost nothing"))
#with(dt, table(tablefiltered$G,tablefiltered$E))
#with(dt, prop.table(table(tablefiltered$G,tablefiltered$E), margin = 1))

with(dt, prop.table(table(dt3$OQ22.1,dt2$Q18.1), margin = 1))
with(dt, prop.table(table(dt3$OQ22.2,dt2$Q18.1), margin = 1))
with(dt, prop.table(table(dt3$OQ22.3,dt2$Q18.1), margin = 1))
with(dt, prop.table(table(dt3$OQ22.4,dt2$Q18.1), margin = 1))
with(dt, prop.table(table(dt3$OQ22.5,dt2$Q18.1), margin = 1))
with(dt, prop.table(table(dt3$OQ22.6,dt2$Q18.1), margin = 1))
with(dt, prop.table(table(dt2$Q3,dt3$OQ22.1), margin = 1))

########## PREVIOUS DEVELOPMENT Q25
PD <- stack_survey %>% 
  select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q25"), by = "respondent_id") %>%
  filter(answer != "")

PD$answer[PD$answer=="Artefactos reutilizables son desarrollados antes de que un proyecto los necesite."] <- "Before"
PD$answer[PD$answer=="Artefactos reutilizables son desarrollados justo en el momento que el proyecto los necesite"] <- "Just in the moment"
PD$answer[PD$answer=="No se desarrollan artefactos reutilizables."] <- "None"

PD2 <- data.frame(PD)
count(PD2,PD2$answer)
with(dt, prop.table(table(PD2$answer,PD2$Q18.1), margin = 1))
with(dt, prop.table(table(dt2$Q25,dt2$Q18.1), margin = 1))

pdp <- count(PD2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

rs <- factor(pdp$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(pdp$answer, pdp$n, function(x){ sum(x) }), pdp$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(label = pdp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  stat_summary(fun.y = sum, aes(label = ..y.., group = pdp$answer), size = 3, geom = "text", position = position_nudge(0,3))

### ojo debe cuadrar los que no crean artefactos con esta pregunta.


########## ORIGIN REUSABLE ASSETS Q23
PD <- stack_survey %>% 
  select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q23"), by = "respondent_id") %>%
  filter(answer != "")

PD$answer[PD$answer=="Artefactos son desarrollados desde cero"] <- "From scratch"
PD$answer[PD$answer=="Artefactos son copias mejoradas de trabajos existentes."] <- "Enhanced copies of existing projects"
PD$answer[PD$answer=="Artefactos son trabajos de proyectos existentes sin modificar"] <- "Exact copies of existing projects"
PD$answer[PD$answer=="Artefactos son desarrollados desde la reingeniería de productos existentes no necesariamente productos propios."] <- "Reengineering of existing products in the market"
PD$answer[PD$answer=="Artefactos comerciales comprados por la organización(COTS)"] <- "COTS"

PD2 <- data.frame(PD)
count(PD2,PD2$answer)
with(dt, prop.table(table(PD2$answer,PD2$Q18.1), margin = 1))
COUNTSDS <- count(dt2,dt2$Q23)
with(dt, prop.table(table(dt2$Q23,dt2$Q18.1), margin = 1))

pdp <- count(PD2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

rs <- factor(pdp$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(pdp$answer, pdp$n, function(x){ sum(x) }), pdp$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(label = pdp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  stat_summary(fun.y = sum, aes(label = ..y.., group = pdp$answer), size = 3, geom = "text", position = position_nudge(0,3))

with(dt, table(PD2$Q18.1, PD2$answer))
with(dt, prop.table(table(PD2$Q18.1, PD2$answer), margin = 1))
with(dt, prop.table(table(PD2$answer,PD2$Q18.1), margin = 1))

#-----------------------------------------------------------------
#4.5 TECHNOLOGICAL
#-----------------------------------------------------------------
########## Development approach
DA <- stack_survey %>% 
  dplyr::select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q11"), by = "respondent_id") %>%
  filter(answer != "") 

DA2 <- data.frame(DA)
count(DA2, DA2$answer)

#PL2$answer[PL2$answer=="Scala"] <- "Other"
#PL2$answer[PL2$answer=="Otro"] <- "Other"

dap <- count(DA2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

dap <- data.frame(dap)

rsdap <- factor(dap$Q18.1 ,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(dap$answer, dap$n, function(x){ sum(x) }), dap$n, fill = rsdap) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_text(aes(label = dap$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  stat_summary(fun.y = sum, aes(label = ..y.., group = dap$answer), size = 3, geom = "text", position = position_nudge(0,3))

with(dt, prop.table(table(dap$answer,dap$Q18.1), margin = 1))
with(dt, prop.table(table(dap$Q18.1,dap$answer), margin = 1))

dacs <- stack_survey %>% 
  dplyr::select(respondent_id,Q3) %>%
  inner_join(stack_multi("Q11"), by = "respondent_id") %>%
  filter(answer != "")
dacs2 <- data.frame(dacs)
with(dacs2, prop.table(table(dacs2$Q3,dacs2$answer), margin = 1))
with(dacs2, prop.table(table(dacs2$answer,dacs2$Q3), margin = 1))

########## PARADIGM PROGRAMMING Q12
PP <- stack_survey %>% 
  dplyr::select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q12"), by = "respondent_id") %>%
  filter(answer != "") 

PP2 <- data.frame(PP)

PP2$answer[PP2$answer=="Orientado a objetos"] <- "Object-oriented programming"
PP2$answer[PP2$answer=="Orientado a componentes"] <- "Component-oriented programming"
PP2$answer[PP2$answer=="Programación reactiva"] <- "Reactive programming"
PP2$answer[PP2$answer=="Programación funcional"] <- "Functional programming"
PP2$answer[PP2$answer=="Programación procedural"] <- "Procedural programming"
PP2$answer[PP2$answer=="Orientado a aspectos"] <- "Aspect-oriented programming"
PP2$answer[PP2$answer=="Programación basada en restricciones"] <- "Constraint-based programming"
PP2$answer[PP2$answer=="Otro"] <- "Other"

count(PP2, PP2$answer)
with(dt, prop.table(table(PP2$answer,PP2$Q18.1), margin = 1))
PPp <- count(PP2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))


rs <- factor(PPp$Q18.1 ,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(PPp$answer, PPp$n, function(x){ sum(x) }), PPp$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_text(aes(label = dap$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  stat_summary(fun.y = sum, aes(label = ..y.., group = PPp$answer), size = 3, geom = "text", position = position_nudge(0,3))

with(dt, prop.table(table(dap$answer,dap$Q18.1), margin = 1))
with(dt, prop.table(table(dap$Q18.1,dap$answer), margin = 1))

########## Programming language
PL <- stack_survey %>% 
  dplyr::select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q13"), by = "respondent_id") %>%
  filter(answer != "") 

PL2 <- data.frame(PL)

PL2$answer[PL2$answer=="Scala"] <- "Other"
PL2$answer[PL2$answer=="Otro"] <- "Other"

plp <- count(PL2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

rs <- factor(plp$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(plp$answer, plp$n, function(x){ sum(x) }), plp$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_text(aes(label = plp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  stat_summary(fun.y = sum, aes(label = ..y.., group = plp$answer), size = 3, geom = "text", position = position_nudge(0,3))

with(dt, prop.table(table(PL2$answer,PL2$Q18.1), margin = 1))

PLcs <- stack_survey %>% 
  dplyr::select(respondent_id,Q3) %>%
  inner_join(stack_multi("Q13"), by = "respondent_id") %>%
  filter(answer != "")
PLcs2 <- data.frame(PLcs)
with(dt, prop.table(table(PLcs2$Q3,PLcs2$answer), margin = 1))

########## FRAMEWORKS
FR <- stack_survey %>% 
  dplyr::select(respondent_id,Q18.1) %>%
  inner_join(stack_multi("Q14"), by = "respondent_id") %>%
  filter(answer != "") 

FR2 <- data.frame(FR)

frp <- count(FR2,answer,Q18.1) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))

rs <- factor(frp$Q18.1,levels = c("Strong","Substancial","Moderate","Limited","No success"))

ggplot() + aes(reorder(frp$answer, frp$n, function(x){ sum(x) }), frp$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Reuse success level") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_text(aes(label = plp$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  stat_summary(fun.y = sum, aes(label = ..y.., group = frp$answer), size = 3, geom = "text", position = position_nudge(0,3))

with(dt, prop.table(table(FR2$answer,FR2$Q18.1), margin = 1))

PLcs <- stack_survey %>% 
  dplyr::select(respondent_id,Q3) %>%
  inner_join(stack_multi("Q14"), by = "respondent_id") %>%
  filter(answer != "")
PLcs2 <- data.frame(PLcs)
with(dt, prop.table(table(PLcs2$Q3,PLcs2$answer), margin = 1))

plot(with(dt, prop.table(table(PLcs2$answer,PLcs2$Q3), margin = 1)))

########## REPOSITORY SYSTEM USAGE 28
########## CASE TOOLS 16.4

library(likert)
sn1 <- data.frame(A = dt3$OQ28,B = dt3$OQ16.4)
sn1$A<- factor(sn1$A, levels=levels(sn1$A)[c(1,2,3,4,5)])
sn1$B <- factor(sn1$B, levels=levels(sn1$B)[c(1,2,3,4,5)])
names(sn1) = c("Repository", "Case tools")

likert3 <- likert(items=sn1[,1:2, drop = FALSE], nlevels = 5)
summary(likert3)

plot(likert3,low.color = "#AEAEAE", high.color = "#4D4D4D", group.order=c('Repository', 'Case tools'),legend.position = "right", legend="Response")

plot(dt3$OQ28)
with(dt, table(dt3$OQ28, dt2$Q18.1))
with(dt, prop.table(table(dt2$Q18.1,dt3$OQ28), margin = 1))
with(dt, prop.table(table(dt3$OQ28,dt2$Q18.1), margin = 1))

plot(dt3$OQ16.4)
with(dt, table(dt3$OQ16.4, dt2$Q18.1))
with(dt, prop.table(table(dt2$Q18.1,dt3$OQ16.4), margin = 1))
with(dt, prop.table(table(dt3$OQ16.4,dt2$Q18.1), margin = 1))


#-----------------------------------------------------------------
#4.6 BARRERAS Y EXPECTATIVAS
#-----------------------------------------------------------------
#######BARRERAS

SM_Q32 <- stack_multi("Q32")
SM_Q32
SM_Q32_total <- stack_multi("Q32") %>% count(tech = answer, sort = TRUE)
SM_Q32_total

SM_Q32_total$tech[SM_Q32_total$tech=="Los proyectos son muy diversos."] <- "The set of running projects is too diverse"
SM_Q32_total$tech[SM_Q32_total$tech=="La falta de documentaci?n de los procesos."] <- "Lack of documentation of the processes"
SM_Q32_total$tech[SM_Q32_total$tech=="La falta de tiempo y recursos."] <- "Lack of time and resources"
SM_Q32_total$tech[SM_Q32_total$tech=="La falta de comunicaci?n entre empleados."] <- "Lack of communication between employees"
SM_Q32_total$tech[SM_Q32_total$tech=="La falta de expertos en el tema."] <- "Lack of experts in the field"
SM_Q32_total$tech[SM_Q32_total$tech=="El uso de diferentes terminolog?as."] <- "Use of different terminologies"
SM_Q32_total$tech[SM_Q32_total$tech=="La falta de est?ndares estables para la tecnolog?a de componentes."] <- "Lack of stable standards for component technology"
SM_Q32_total$tech[SM_Q32_total$tech=="La tecnolog?a evoluciona y se considera que los artefactos reutilizables pueden volverse obsoletos."] <- "Technology evolves and reusable assets may become obsolete"
SM_Q32_total$tech[SM_Q32_total$tech=="La falta de conocimiento en la integraci?n de componentes."] <- "Lack of knowledge in the integration of components"
SM_Q32_total$tech[SM_Q32_total$tech=="La falta de disciplina en el desarrollo."] <- "Lack of discipline in development."
SM_Q32_total$tech[SM_Q32_total$tech=="Escepticismo inicial sobre la utilidad y ?xito de la iniciativa de reutilizaci?n."] <- "Initial skepticism about reuse initiative"
SM_Q32_total$tech[SM_Q32_total$tech=="La falta de componentes certificados."] <- "Lack of certified components."
SM_Q32_total$tech[SM_Q32_total$tech=="La falta de un m?todo para producir sistemas de calidad a partir de componentes."] <- "Lack of a method to produce quality systems from components"
SM_Q32_total$tech[SM_Q32_total$tech=="La falta de componentes disponibles."] <- "Lack of available components."
SM_Q32_total$tech[SM_Q32_total$tech=="Ninguna"] <- "None"
SM_Q32_total$tech[SM_Q32_total$tech=="Otro"] <- "Other"

ggplot(SM_Q32_total, aes( x = reorder(SM_Q32_total$tech, SM_Q32_total$n), y = SM_Q32_total$n), cex.axis=0.5 )+ 
  labs( x="", y="Quantity", title="") +
  theme(text = element_text(size=10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + ####FONDE DEL GRÁFICO
  geom_bar(stat="identity") +
  coord_flip() 

stack_survey %>%
  filter(Q3 == "Medium") %>%
  inner_join(stack_multi("Q32"), by = "respondent_id") %>%
  count(answer, sort = TRUE)


b <- stack_survey %>% 
  dplyr::select(respondent_id,Q3) %>%
  inner_join(stack_multi("Q32"), by = "respondent_id") %>%
  filter(answer != "")
b2 <- data.frame(b)
with(dt, prop.table(table(b2$answer,b2$Q3), margin = 1))
with(dt, prop.table(table(b2$Q3,b2$answer), margin = 1))


#
A <- stack_survey %>% 
  dplyr::select(respondent_id,Q3) %>%
  inner_join(stack_multi("Q32"), by = "respondent_id") %>%
  filter(answer != "") 

A2 <- data.frame(A)

AP <- count(A2,answer,Q3) %>% 
  group_by(answer) %>% 
  mutate(n1 = paste0(round(n/sum(n) * 100), "%"))


rs <- factor(AP$Q3 ,levels = c("Large","Medium","Small","Microenterprise","Startups"))

ggplot() + aes(reorder(AP$answer, AP$n, function(x){ sum(x) }), AP$n, fill = rs) + geom_col() + coord_flip() +
  labs( x="", y="", title="", fill ="Organization size") +
  scale_fill_grey(start = .9, end = .3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_text(aes(label = dap$n1), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  stat_summary(fun.y = sum, aes(label = ..y.., group = AP$answer), size = 3, geom = "text", position = position_nudge(0,3))


#######BENEFICIOS

SM_Q33 <- stack_multi("Q33")
SM_Q33
SM_Q33_total <- stack_multi("Q33") %>% count(tech = answer, sort = TRUE)
SM_Q33_total

SM_Q33_total$tech[SM_Q33_total$tech=="Disminuci?n de costos"] <- "Cost reduction"
SM_Q33_total$tech[SM_Q33_total$tech=="Disminuci?n de las labores necesarias"] <- "Decrease in labor needs"
SM_Q33_total$tech[SM_Q33_total$tech=="Incremento de la productividad"] <- "Increase in productivity"
SM_Q33_total$tech[SM_Q33_total$tech=="Reutilizaci?n de conocimiento"] <- "Knowledge reuse"
SM_Q33_total$tech[SM_Q33_total$tech=="Incremento de la calidad de los productos de software"] <- "Increase the quality of software products"
SM_Q33_total$tech[SM_Q33_total$tech=="Disminuci?n de riesgos en el producto"] <- "Decrease in product risks"
SM_Q33_total$tech[SM_Q33_total$tech=="Ganancias de productividad a gran escala"] <- "Large-scale productivity gains"
SM_Q33_total$tech[SM_Q33_total$tech=="Disminuci?n del  tiempo de comercializaci?n"] <- "Decrease in time-to-market"
SM_Q33_total$tech[SM_Q33_total$tech=="Gesti?n eficiente de los recursos humanos"] <- "Efficient management of human resources"
SM_Q33_total$tech[SM_Q33_total$tech=="Incremento de la satisfacci?n del cliente"] <- "Increase in customer satisfaction"
SM_Q33_total$tech[SM_Q33_total$tech=="Habilidad de realizar personalizaci?n masiva"] <- "Ability to effect mass customization"
SM_Q33_total$tech[SM_Q33_total$tech=="Habilidad de mantener un crecimiento sin precedentes"] <- "Ability to maintain unprecedented growth"
SM_Q33_total$tech[SM_Q33_total$tech=="Habilidad de moverse a nuevos mercados"] <- "Ability to move into new markets"
SM_Q33_total$tech[SM_Q33_total$tech=="Habilidad de mantener la presencia en el mercado"] <- "Ability to maintain market presence"
SM_Q33_total$tech[SM_Q33_total$tech=="Otro"] <- "Other"

ggplot(SM_Q33_total, aes( x = reorder(SM_Q33_total$tech, SM_Q33_total$n), y = SM_Q33_total$n), cex.axis=0.5 )+ 
  labs( x="", y="Quantity", title="") +
  theme(text = element_text(size=10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + ####FONDE DEL GRÁFICO
  geom_bar(stat="identity") +
  coord_flip() 

b <- stack_survey %>% 
  dplyr::select(respondent_id,Q3) %>%
  inner_join(stack_multi("Q33"), by = "respondent_id") %>%
  filter(answer != "")
b2 <- data.frame(b)
with(dt, prop.table(table(b2$answer,b2$Q3), margin = 1))

stack_survey %>%
  filter(Q3 == "Startups") %>%
  inner_join(stack_multi("Q33"), by = "respondent_id") %>%
  count(answer, sort = TRUE)


