#libraries
library(tidyverse)
library(caret)
library(plumber)

#read in data
diabetes_binary_health_indicators_BRFSS2015_csv <- read_csv("C:/Users/beard/Downloads/diabetes_binary_health_indicators_BRFSS2015.csv.zip")

#select predictors and convert factors
diabetes<-diabetes_binary_health_indicators_BRFSS2015_csv |>
  select(c(Diabetes_binary,HighBP,HighChol,Smoker,Stroke,PhysActivity,HvyAlcoholConsump,Sex,Age,Income,BMI)) |>
  mutate( , Diabetes_binary=factor(Diabetes_binary, levels=c(0,1), labels= c("No","Pre")),
          HighBP=factor(HighBP, levels= c(0,1), labels=c("No","Yes")),
          HighChol=factor(HighChol, levels= c(0,1), labels=c("No","Yes")),
          Smoker=factor(Smoker, levels= c(0,1), labels=c("No","Yes")),
          Stroke=factor(Stroke, levels= c(0,1), labels=c("No","Yes")),
          PhysActivity=factor(PhysActivity, levels= c(0,1), labels=c("No","Yes")),
          HvyAlcoholConsump=factor(HvyAlcoholConsump, levels= c(0,1), labels=c("No","Yes")),
          Sex=factor(Sex, levels= c(0,1), labels=c("Female","Male")),
          Age=factor(Age, levels= 1:14, labels=c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+","Unkown/Missing")),
          Income=factor(Income, levels= c(0:8,77,99, NA_real_), labels=c(">$10,000","$10,000-15,000","$15,000-20,000","$20,000-25,000","$25,000-35,000","$35,000-50,000","$50,000-75,000","$75,000+","Unkown","Refused","Missing")))

#model
defaults<-list(
  BMI<-mean(diabetes$BMI, na.rm=TRUE),
  Diabetes_binary<-names(sort(table(diabetes$Diabetes_binary), decreasing = TRUE))[1],
  HighBP<-names(sort(table(diabetes$HighBP), decreasing = TRUE))[1],
  HighChol<-names(sort(table(diabetes$HighChol), decreasing = TRUE))[1],
  Smoker<-names(sort(table(diabetes$Smoker), decreasing = TRUE))[1],
  Stroke<-names(sort(table(diabetes$Stroke), decreasing = TRUE))[1],
  PhysActivity<-names(sort(table(diabetes$PhysActivity), decreasing = TRUE))[1],
  HvyAlcoholConsump<-names(sort(table(diabetes$HvyAlcoholConsump), decreasing = TRUE))[1],
  Sex<-names(sort(table(diabetes$Sex), decreasing = TRUE))[1],
  Age<-names(sort(table(diabetes$Age), decreasing = TRUE))[1],
  Income<-names(sort(table(diabetes$Income), decreasing = TRUE))[1]
)


#API
#* @param defaults 
#* @get /pred

function(defaults){
  list(defaults=defaults)
}

#* @get /info

function() {
  list(
    name = "Emma Beardsley",
    page = "https://beardsleye.github.io/Project-Final/EDA.html"
  )
}

#Call API
api<-plumb("myAPI.R")
api$run(port=8000)