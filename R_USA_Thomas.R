#lets go pour une bonne tranche de fun

install.packages("readxl")
install.packages("lmtest")
install.packages("tseries")
install.packages("glmnet")

library(readxl)
library(lmtest)
library(tseries)
library(glmnet)
library(strucchange)

data <- read_excel("CV_USA.xlsx")
head(data)

# Convertir Year en variable numérique (au cas où)
data$Year <- as.numeric(data$Year)

#on fout tout le monde en log car c'est plus joli... et pratique
data_log <- data
data_log$CV <- log(data$CV)
data_log$PIB <- log(data$PIB)
data_log$Population <- log(data$Population)
data_log$CPI <- log(data$CPI)
data_log$Beef_price <- log(data$Beef_price)

#test de la multicolinéarité (VIF)

# ---- 2. Création du modèle de régression multiple ----
modele <- lm(CV ~ PIB + Population + CPI + Beef_price, data = data)
modele_log <- lm(CV ~ PIB + Population + CPI + Beef_price, data = data_log)


# Résumé du modèle
summary(modele)
#tous les paramètres sont pertinents sauf le prix un peu mois (Student)
#Le modèle en général est super p-value giga faible (Fisher)

summary(modele_log)
#ça change des trucs et plusieurs paramètres perdent en importance

#Bon ba c'est bien bô mais la partie en dessous ne marche pas pk on a de la multicolinéarité dans les données, aller à la partie 2 plus bas
#test du modèle par la méthode des moindres carrés généralisées (ccl chap 2)
#ATTEBNTION ça marche que si il n'y a pas de multicolinéarité dans les données, à voir avec l'autre team (intro chap 3)
#autocorrélation d'ordre 1 (Durbin-Watson)
dwtest(modele)
dwtest(modele_log)
#y'a de l'autocorrélation starfoula !! car p-value < 0.05, et dans les 2 cas

#on test l'hétéroscédasticité
bptest(modele)
bptest(modele_log)
#bonne nouvelle on est homoscédastique (Breusch-Pagan)

#visualisation de tout ça 
plot(modele, which = 1)
plot(modele_log, which = 1)
#on observe la belle tendance à la courbure des résidut qui fait chier !!

#vérifions la normalité des résidus ensuite
jarque.bera.test(residuals(modele))
#les résidu ne sont pas normaux mais de peux car p-value = 0.01367 < 0.05 (Jarque-Bera)
#visualisation graphique 
qqnorm(residuals(modele))
qqline(residuals(modele))
#graphiquement ils semble quand même bien normaux ces cons de résidus
#si on considère qu'ils ne sont pas normaux faut changer de modèle ou bien transformer la variable je crois...

jarque.bera.test(residuals(modele_log))
#en log le test dit que les résidus sont normaux, c'est une super nouvelle, continuons donc avec des log car c'est cool
qqnorm(residuals(modele_log))
qqline(residuals(modele_log))

#Attention, je poursuis désormais que avec les log car ils sont mieux, cf : juste au dessus

#On test ensuite la stabilité temporelle (Cusum)
cusum   <- efp(CV ~ PIB + Population + CPI + Beef_price, data = data_log, type = "Rec-CUSUM")
plot(cusum)
#Test (Cusum Square) plus fiable d'après le cours
#rr <- (recresid(data_log$CV, modele_log))
#rr <- -rr^2 
#cumrr <- -cumsum(rr)/scr
#le prof le fait avec des matrice et pas des dataset et je suis quand même tjs un brêle en R qu'on ne l'oublie pas donc si qql arrive à le faire je suis chaud patate !!

#RESUME :
#On a de l'autocorrélation mais pas d'hétéroscédasticité, les résidus sont normaux avec le log et il semble qu'on soit stable temporellement mais à préciser avec un vrai cusum square
#Malheureusement on a de la multicolinéarité et donc il faut faire une regression Ridge, c'est ce qui est fait en dessous

#Partie 2
#on attaque une petite régression ridge des familles

#mettons tout ce joli monde dans une matrice
X_log <- model.matrix(CV ~ PIB + Population + CPI + Beef_price, data = data_log)[, -1]
CV_log <- data_log$CV

set.seed(123)
modele_ridge <- cv.glmnet(
  X_log, CV_log,
  alpha = 0,        # Ridge
  nfolds = 5, #sinon pas assez de données par folds
  standardize = TRUE
)
#message d'avis mettant standardize = False car on a moins de 3 obs par fold, mais normalement non donc je suis pas sur de tout

plot(modele_ridge)
#le plot est joli mais jsp ce que ça signifie

#Validation croisée
#les meilleurs coef ridge, en fait on a plein de coef possible variant en fonction de lambda et ici se sont les 2 lambda qui présentent les meilleurs caractéristiques de stabilioté
modele_ridge$lambda.min #minimise l'erreur moyenne de validation croisée, variance néanmoins plus importante
modele_ridge$lambda.1se #l'erreur est à moins d'un écart type du minimum, coef plus petit, plus faible variance

#Et nvoici les coefs
coef(modele_ridge, s = "lambda.1se")

#voici comment utiliser le modèle pour faire une prédiction:
CV_pred <- predict(modele_ridge, newx = X, s = "lambda.1se")

#Partie 3 quelques test possible sur notre petite regression ridge
rmse <- sqrt(mean((CV_log - CV_pred)^2))
rmse # RMSE = 0.348  pas dégueu

r2 <- 1 - sum((CV_log - CV_pred)^2) / sum((CV_log - mean(CV_log))^2)Message d'avis :
Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold i
r2 #le R²=0.3485 est nul à chier on explique rien du tout avec le modèle, starfula c'est la sauce !!

#Déterminons maintenant un intervalle de confince pour notre joli modèle, par une méthode de bootstrap percentile-t avec un intervalle de confiance basé sur une statistique pivot (je cite le prof)

#c'est galère et j'ai mal à la tête je m'y attèle plus tard 






