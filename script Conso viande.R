
library(ggplot2)
library(dplyr)
library(readxl)
library(sandwich)
library(lmtest)
library(car)


#import des données
data <- read_excel("CV_USA.xlsx", sheet = 1)
View(data)

#transformation logarithme et conso/personne (en pounds) et PIB/personne

data <- data %>%
  mutate(CV_percap = (CV/Population)*(10^9),
         PIB_percap = PIB/Population,
         log_CVpc = log(CV_percap),
         log_PIBpc = log(PIB_percap),
         log_Beef = log(Beef_price),
         log_CPI = log(CPI))

#création d'une série temporelle
startyear = min(data$Year)
ts_CVpc <- ts(data$log_CVpc, start = startyear, frequency = 1)

#Exploratory Data Analysis avant modèle

ggplot(data, aes(Year, CV_percap)) + geom_line() + labs(title="Consommation viande per capita")
ggplot(data, aes(Year, log_CVpc)) + geom_line() + labs(title="log(CV per capita)")


#régression linéaire log-log

model1 <- lm(log_CVpc ~ log_PIBpc + log_Beef + log_CPI, data = data )
summary(model1)

# test hétéroscédasticité

bptest(model1) # Breusch-Pagan

bptest(model1, ~ fitted(model1) + I(fitted(model1)^2)) # test de white approximé

# test autocorrélation 

dwtest(model1) # Durbin-Watson
bgtest(model1, order = 2) # Breusch-Godfrey, ajuster l'ordre






















