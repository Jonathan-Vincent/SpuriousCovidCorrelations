require(ggplot2)
library(scales)
library(readr)
library(tidyr)
library(dplyr)
library(cowplot)

cols = cols(
  .default = col_double(),
  iso_code = col_character(),
  continent = col_character(),
  location = col_character(),
  date = col_date(format = ""),
  icu_patients = col_double(),
  icu_patients_per_million = col_double(),
  hosp_patients = col_double(),
  hosp_patients_per_million = col_double(),
  weekly_icu_admissions = col_double(),
  weekly_icu_admissions_per_million = col_double(),
  weekly_hosp_admissions = col_double(),
  weekly_hosp_admissions_per_million = col_double(),
  tests_units = col_character()
)

#covid <- read_csv("Documents/CovidCorrelations/UntilPresent.csv",col_types = cols)
#wdiBase <- read_csv("Documents/CovidWaves/WDI_csv/WDIeuro.csv")

wdi <- wdiBase

wdi$Country[32] = 'Slovakia'
wdi$Country[8] = 'Czechia'
wdi$Country[5] = 'Bosnia'


covidEurope <- covid[covid$continent == 'Europe' & (covid$date == '2021-03-21')
                            & covid$population > 300000,]




covidEurope <- covidEurope[!(is.na(covidEurope$iso_code)),]

covidEurope <- covidEurope[ , colSums(is.na(covidEurope)) == 0]

covidEurope <- covidEurope %>% 
  rename(
    Country = location
  )

covidEurope$Country[5] = 'Bosnia'

covidEurope$date <- NULL
covidEurope$continent <- NULL

n=40


Eurodata <- merge(x=wdi, y=covidEurope, by='Country')



EurodataVar <- Eurodata[ - as.numeric(which(apply(Eurodata, 2, sd) == 0))][,-1]

i1 <- sapply(EurodataVar, is.numeric)
y1 <- "total_deaths_per_million" #change it to actual column name
x1 <- setdiff(names(EurodataVar)[i1], y1)
x1 <- names(EurodataVar)[i1]

descending <- order(cor(EurodataVar[x1], EurodataVar[[y1]],use="complete.obs"))

#print(cor(EurodataVar[x1], EurodataVar[[y1]],use="complete.obs")[descending])

for(i in 1:ncol(Eurodata)) {       # for-loop over columns
  if(is.numeric(Eurodata[ , i])){
    corr = cor(Eurodata[ , i],Eurodata$total_deaths_per_million)
    if(!is.na(corr)){
      if(abs(corr) >0.5){
        name = colnames(Eurodata)[i]
        print(paste0(name,corr))
      }
    }
    corr2 = cor(log(Eurodata[ , i]),log(Eurodata$total_deaths_per_million))
    if(!is.na(corr2)){
      if(abs(corr2) >0.5){
        name = paste0('Log ',colnames(Eurodata)[i])
        print(paste0(name,corr2))
      }
    }
  }
}
p <- ggplot(data=Eurodata,aes(x=`Agricultural land (% of land area)`,
                                 y=total_deaths_per_million,label=Country))+
  geom_point()+
  geom_label()+
  scale_y_log10()+
  scale_x_log10()+
  #xlab('Livestock production index 2020 (2005 = 100)')+
  ylab('Total Covid-19 Deaths per Million')

print(p)