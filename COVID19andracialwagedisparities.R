setwd("C{/Users/Yussuf Schwarz/OneDrive/Desktop/UniWiwi/415G - Labor Relations")

#install.packages("nlme")
#install.packages("cdlTools")
#install.packages("vtable")
#install.packages("usmap")

library(usmap)
library(cdlTools)
library(SciViews)
library(lmtest)
library(nlme)
library(car)
library(vtable)
library(stargazer)
library("writexl")
library(ggplot2)
library(readxl)

###Load data:
data = read.csv("cps_00006.csv.gz")

##############################DATA##############################################

###Prepare data:
data=data[data$INCWAGE>0,]
data=data[data$INCWAGE<99999999,]
data=data[data$WKSWORK1>50,]
data=na.omit(data)

educyears=c()
for (c in 1:length(data$EDUC)){
  if (data$EDUC[c] == 2){
  educyears[c] = 0
  }else if( data$EDUC[c]==10){
  educyears[c] =(4)
  }else if( data$EDUC[c]==20){
  educyears[c] =(6)
  }else if( data$EDUC[c]==30){
  educyears[c] =(8)
  }else if( data$EDUC[c]==40){
  educyears[c] =(9)        
}else if( data$EDUC[c]==50){
  educyears[c] =(10)
}else if( data$EDUC[c]==60){
  educyears[c] =(11)
}else if( data$EDUC[c]==71){
  educyears[c] =(12)
}else if( data$EDUC[c]==73){
  educyears[c] =(13) 
}else if( data$EDUC[c]==81){
  educyears[c] =(14)
}else if( data$EDUC[c]==91){
  educyears[c] =(15)
}else if( data$EDUC[c]==92){
  educyears[c] =(15)        
}else if( data$EDUC[c]==111){
  educyears[c] =(17)
}else if( data$EDUC[c]==123){
  educyears[c] =(19)
}else if( data$EDUC[c]==124){
  educyears[c] =(18)
}else if( data$EDUC[c]==125){
  educyears[c] =(23) 
}else{
  print("no value")}
}


data$EDUCYEARS=educyears
data$SEX=data$SEX-1 #1 for females

college=c()
for (c in 1:length(data$EDUC)){
  if (data$EDUC[c] >= 111){
    college[c] = "College degree"
  }else{
    college[c] ="No College degree"}
}
data$COLLEGE=college

race=c()
for (c in 1:length(data$RACE)){
  if (data$RACE[c] == 100){
    race[c] = "White"
  }else{
    race[c] ="Non-White"}
}
data$RACE=race

data$STATE = fips(data$STATEFIP,to="Name")

metro=c()
for (c in 1:length(data$METRO)){
  if (data$METRO[c] == 0){
    metro[c] = NA
  }else if( data$METRO[c]==9){
    metro[c] =NA
  }else if( data$METRO[c]==2){
    metro[c] ="Metro"
  }else if( data$METRO[c]==3){
    metro[c] ="Metro"
  }else if( data$METRO[c]==4){
    metro[c] ="Metro"
  }else if( data$METRO[c]==1){
    metro[c] ="Not in Metro"
  }else{
    print("error")}
}
    
data$METRO=metro
    
marst=c()
for (c in 1:length(data$MARST)){
  if (data$MARST[c] == 9){
    marst[c] = NA
  }else if( data$MARST[c]==1){
    marst[c] = "Married" 
  }else if( data$MARST[c]==2){
    marst[c] = "Married"  
  }else if( data$MARST[c]==3){
    marst[c] = "Not Married"  
  }else if( data$MARST[c]==4){
    marst[c] = "Not Married"  
  }else if( data$MARST[c]==5){
    marst[c] = "Not Married"  
  }else if( data$MARST[c]==6){
    marst[c] = "Not Married"  
  }else if( data$MARST[c]==7){
    marst[c] = "Not Married"  
  }else{
    print("error")}
}

data$MARST=marst

cit=c()
for (c in 1:length(data$CITIZEN)){
  if (data$CITIZEN[c] == 1){
    cit[c] = "Citizen"
  }else if( data$CITIZEN[c]==2){
    cit[c] = "Citizen" 
  }else if( data$CITIZEN[c]==3){
    cit[c] = "Citizen" 
  }else if( data$CITIZEN[c]==4){
    cit[c] = "Citizen" 
  }else if( data$CITIZEN[c]==5){
    cit[c] = "Non-Citizen" 
  }else if( data$CITIZEN[c]==9){
    cit[c] = NA
  }else{
    print("error")}
}
data$CITIZEN=cit

data=na.omit(data)

data2019 = data[data$YEAR==2019,]
data2022 = data[data$YEAR==2022,]

################################################################################

#############################SUMMARY############################################

data2019sum=data2019[,-c(1:4,6,13,14,17,20)]
data2022sum=data2022[,-c(1:4,6,13,14,17,20)]
colnames(data2019sum)=c("Metro","Age","Sex","Race","Marital Status","N Children","US Citizen","Weeks Worked","Wage","Years of Education","College")
colnames(data2022sum)=c("Metro","Age","Sex","Race","Marital Status","N Children","US Citizen","Weeks Worked","Wage","Years of Education","College")

metrosum=c()
for (c in 1:length(data2019sum$Metro)){
  if (data2019sum$Metro[c] == "Not in Metro"){
    metrosum[c] = 0
  }else{
    metrosum[c] =1}
}
data2019sum$Metro=metrosum

metrosum=c()
for (c in 1:length(data2022sum$Metro)){
  if (data2022sum$Metro[c] == "Not in Metro"){
    metrosum[c] = 0
  }else{
    metrosum[c] =1}
}
data2022sum$Metro=metrosum

racesum=c()
for (c in 1:length(data2019sum$Race)){
  if (data2019sum$Race[c] == "White"){
    racesum[c] = 0
  }else{
    racesum[c] =1}
}
data2019sum$Race=racesum 

racesum=c()
for (c in 1:length(data2022sum$Race)){
  if (data2022sum$Race[c] == "White"){
    racesum[c] = 0
  }else{
    racesum[c] =1}
}
data2022sum$Race=racesum 


marsum=c()
for (c in 1:length(data2019sum$`Marital Status`)){
  if (data2019sum$`Marital Status`[c] == "Not Married"){
    marsum[c] = 0
  }else{
    marsum[c] =1}
}
data2019sum$`Marital Status`=marsum 

marsum=c()
for (c in 1:length(data2022sum$`Marital Status`)){
  if (data2022sum$`Marital Status`[c] == "Not Married"){
    marsum[c] = 0
  }else{
    marsum[c] =1}
}
data2022sum$`Marital Status`=marsum 

citsum=c()
for (c in 1:length(data2019sum$`US Citizen`)){
  if (data2019sum$`US Citizen`[c] == "Citizen"){
    citsum[c] = 0
  }else{
    citsum[c] =1}
}
data2019sum$`US Citizen`=citsum 

citsum=c()
for (c in 1:length(data2022sum$`US Citizen`)){
  if (data2022sum$`US Citizen`[c] == "Citizen"){
    citsum[c] = 0
  }else{
    citsum[c] =1}
}
data2022sum$`US Citizen`=citsum 

colsum=c()
for (c in 1:length(data2019sum$College)){
  if (data2019sum$College[c] == "No College degree"){
    colsum[c] = 0
  }else{
    colsum[c] =1}
}
data2019sum$College=colsum 

colsum=c()
for (c in 1:length(data2022sum$College)){
  if (data2022sum$College[c] == "No College degree"){
    colsum[c] = 0
  }else{
    colsum[c] =1}
}
data2022sum$College=colsum 

stargazer(data2019sum,out = "Summary2019.html",title = "Summary Statistics 2019",digits = 2)
stargazer(data2022sum,out = "Summary2022.html",title = "Summary Statistics 2022",digits = 2)

################################################################################

############################ESTIMATION##########################################

########################DUMMIES 2019############################################

unique(data2019$STATE)
M_i = matrix(ncol=51,nrow=67466)

#for (x in 1:length(data2019$STATE)){
#  for (i in 1:length(unique(data2019$STATE))){
#    if (unique(data2019$STATE)[i] == data2019$STATE[x]){
#      M_i[x,i] = 1
#    } else {
#      M_i[x,i] = 0
#    }
#  }
#} #Used to create dummies19 once

dummies19=data.frame(M_i)
names(dummies19)=unique(data2019$STATE)
names(dummies19)=gsub(" ","",x=names(dummies19))
#write_xlsx(dummies19,"statedummies.xlsx")

dummies19=read_xlsx("statedummies.xlsx",col_names = T)

########################DUMMIES 2022############################################

unique(data2022$STATE)
M_i_22 = matrix(ncol=51,nrow=55135)

#for (x in 1:length(data2022$STATE)){
#  for (i in 1:length(unique(data2022$STATE))){
#    if (unique(data2022$STATE)[i] == data2022$STATE[x]){
#      M_i_22[x,i] = 1
#    } else {
#      M_i_22[x,i] = 0
#    }
#  }
#} #Used to create dummies22 once

dummies22=data.frame(M_i_22)
names(dummies22)=unique(data2022$STATE)
names(dummies22)=gsub(" ","",x=names(dummies22))
#write_xlsx(dummies22,"statedummies22.xlsx")
dummies22=read_xlsx("statedummies22.xlsx",col_names = T)

#########################GLS ESTIMATION#########################################

###############################2019#############################################

Reg2019=gls(ln(INCWAGE)~AGE+I(AGE^2)+NCHILD+CITIZEN+INDLY+WKSWORK1+DISABWRK+EDUCYEARS+MARST+METRO+SEX+RACE+METRO*RACE,data=data2019)
summary(Reg2019)
stargazer(Reg2019,title = "GLS Regression 2019")
###############################2022#############################################

Reg2022=gls(ln(INCWAGE)~AGE+I(AGE^2)+NCHILD+CITIZEN+INDLY+WKSWORK1+DISABWRK+EDUCYEARS+MARST+METRO+SEX+RACE+METRO*RACE,data=data2022)
summary(Reg2022)
stargazer(Reg2022,title = "GLS Regression 2022")

################################################################################

###############################2019#############################################

coeffRace19 = data.frame(Reg2019$coefficients[13])
coeffRaceMetro19 = data.frame(Reg2019$coefficients[14])

gap19=coeffRace19[1,]*(unclass(factor(data2019$RACE))-1)+coeffRaceMetro19[1,]*(unclass(factor(data2019$RACE))-1)*(unclass(factor(data2019$METRO))-1)

###############################2022#############################################

coeffRace22 = data.frame(Reg2022$coefficients[13])
coeffRaceMetro22 = data.frame(Reg2022$coefficients[14])

gap22=coeffRace22[1,]*(unclass(factor(data2022$RACE))-1)+coeffRaceMetro22[1,]*(unclass(factor(data2022$RACE))-1)*(unclass(factor(data2022$METRO))-1)

################################################################################

##############################DENSITY###########################################


den1 = data.frame(gap19[1:55135],gap22)
names(den1)=c("gap19","gap22")
dichte1 = ggplot(data=den1) +
  stat_density(aes(x=gap19,fill="black"),adjust=7, alpha=.3)+
  stat_density(aes(x=gap22,fill="red"),adjust=7, alpha=.2) +
  scale_fill_identity(name = NULL, 
                      labels = c(black = "Conditional Racial Wage Gap 2019", 
                                 red = "Conditional Racial Wage Gap 2022"
                      ),
                      guide = "legend")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Density of Individual Conditional Racial Wage Gaps 2019 and 2022",
       y     = "Density",
       x     = "x")

dichte1

ggsave("Anhang2.png",plot = dichte1,scale = 1,width = 12,height = 5,device='png', dpi=500)

################################################################################


###############################2019#############################################

dummies19es=data.frame(cbind(gap19,dummies19))
names(dummies19es)=c("gap19",names(dummies19))
names(dummies19es)=gsub(" ","",names(dummies19es))

gap19states=gls(gap19~Maine+`NewHampshire`+Vermont+Massachusetts+
      `RhodeIsland`+Connecticut+`NewYork`+`NewJersey`+
      Pennsylvania+Ohio+Indiana+Indiana+Illinois+
      Michigan+Wisconsin+Minnesota+Iowa+Missouri+
      `NorthDakota`+`SouthDakota`+Nebraska+Kansas+
      Delaware+Maryland+`DistrictofColumbia`+Virginia+
      `WestVirginia`+`NorthCarolina`+`SouthCarolina`+Georgia+
      Florida+Kentucky+Tennessee+Alabama+Mississippi+
      Arkansas+Louisiana+Oklahoma+Texas+Montana+
      Idaho+Wyoming+Colorado+`NewMexico`+Arizona+
      Utah+Nevada+Washington+Oregon+California+
      Alaska+Hawaii-1,data = dummies19es)
summary(gap19states)
stargazer(gap19states,title = "GLS Regression Regional Dispersion 2019",out="Anhang3.html")

coeff19 = data.frame(gap19states$coefficients)
states=unique(data2019$STATE)
coeff19$state=states
x=coeff19$gap19states.coefficients
us_map <- usmap::us_map() # used to add map scale

usmap19=plot_usmap(data = coeff19, values = "gap19states.coefficients", labels = F)+
  labs(fill = 'Conditional Racial Wage Gap 2019') + 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90",
                       guide = guide_colourbar(barwidth = 25, barheight = 1,
                                               title.position = "top"))+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12), 
        legend.text=element_text(size=10))

ggsave("map19.png",plot = usmap19,scale = 1,width = 12,height = 5,device='png', dpi=500)

###############################2022#############################################

dummies22es=data.frame(cbind(gap22,dummies22))
names(dummies22es)=c("gap22",names(dummies22))
names(dummies22es)=gsub(" ","",names(dummies22es))

gap22states=gls(gap22~Maine+`NewHampshire`+Vermont+Massachusetts+
              `RhodeIsland`+Connecticut+`NewYork`+`NewJersey`+
              Pennsylvania+Ohio+Indiana+Indiana+Illinois+
              Michigan+Wisconsin+Minnesota+Iowa+Missouri+
              `NorthDakota`+`SouthDakota`+Nebraska+Kansas+
              Delaware+Maryland+`DistrictofColumbia`+Virginia+
              `WestVirginia`+`NorthCarolina`+`SouthCarolina`+Georgia+
              Florida+Kentucky+Tennessee+Alabama+Mississippi+
              Arkansas+Louisiana+Oklahoma+Texas+Montana+
              Idaho+Wyoming+Colorado+`NewMexico`+Arizona+
              Utah+Nevada+Washington+Oregon+California+
              Alaska+Hawaii-1,data = dummies22es)
summary(gap22states)
stargazer(gap22states,title = "GLS Regression Regional Dispersion 2022",out="Anhang4.html")

coeff22 = data.frame(gap22states$coefficients)
states=unique(data2022$STATE)
coeff22$state=states
x=coeff22$gap22states.coefficients
us_map <- usmap::us_map()

usmap22=plot_usmap(data = coeff22, values = "gap22states.coefficients", labels = F)+
  labs(fill = 'Conditional Racial Wage Gap 2022') + 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90",
                       guide = guide_colourbar(barwidth = 25, barheight = 1,
                                               title.position = "top"))+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12), 
        legend.text=element_text(size=10))

ggsave("map22.png",plot = usmap22,scale = 1,width = 12,height = 5,device='png', dpi=500)

################################################################################

gapchange=(gap22states$coefficients-gap19states$coefficients)*100
gapchange=sort(gapchange)
stargazer(gapchange,out = "gapchange.html",flip = T,title = "Percentage Change of Conditional Wage Gap 2019 to 2022")
