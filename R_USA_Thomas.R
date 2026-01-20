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

# setwd("/Users/luciedeseguinspazzis/Desktop/EDDE/Cours/S1/Econométrie-Lantz/Projet/Projet_OB1_2025-2026") # set working directory
# getwd() # vérifie le working directory
# list.files() # regarde les différents fichiers
# data <- read_excel("CV_USA.xlsx") # charge le fichier excel et nouveau nom cv_usa
# names(cv_usa)

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

#variable reel plutôt que nominal car c'est encore mieux
data_reel <- data
data_reel$CV <- log(data$CV/(data$CPI/100))
data_reel$PIB <- log(data$PIB/(data$Population*(data$CPI/100)))
data_reel$Beef_price <- log(data$Beef_price/(data$CPI/100))

#mettons tout ce joli monde dans une matrice
X_reel <- model.matrix(CV ~ PIB +  Beef_price, data = data_reel)[, -1]
CV_reel <- data_reel$CV

# ---- 2. Création du modèle de régression multiple ----
modele <- lm(CV ~ PIB + Population + CPI + Beef_price, data = data)
modele_log <- lm(CV ~ PIB + Population + CPI + Beef_price, data = data_log)
modele_reel <- lm(CV ~ PIB + Beef_price, data = data_reel)


# Résumé du modèle
summary(modele)
#tous les paramètres sont pertinents sauf le prix un peu mois (Student)
#Le modèle en général est super p-value giga faible (Fisher)

summary(modele_log)
#ça change des trucs et plusieurs paramètres perdent en importance

summary(modele_reel)
CV_pred_reel <- predict(modele_reel, newx = X_reel)
r2 <- 1 - sum((CV_reel - CV_pred_reel)^2) / sum((CV_reel - mean(CV_reel))^2)
r2

#Bon ba c'est bien bô mais la partie en dessous ne marche pas pk on a de la multicolinéarité dans les données, aller à la partie 2 plus bas
#test du modèle par la méthode des moindres carrés généralisées (ccl chap 2)
#ATTEBNTION ça marche que si il n'y a pas de multicolinéarité dans les données, à voir avec l'autre team (intro chap 3)
#autocorrélation d'ordre 1 (Durbin-Watson)
dwtest(modele)
dwtest(modele_log)
#y'a de l'autocorrélation starfoula !! car p-value < 0.05, et dans les 2 cas

dwtest(modele_reel)
#y'a pas tant d'utocorrélation que ça ...

#on test l'hétéroscédasticité
bptest(modele)
bptest(modele_log)
bptest(modele_reel)
#bonne nouvelle on est homoscédastique (Breusch-Pagan)

#visualisation de tout ça 
plot(modele, which = 1)
plot(modele_log, which = 1)
plot(modele_reel, wich = 1)

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

jarque.bera.test(residuals(modele_reel))
#en log le test dit que les résidus sont normaux, c'est une super nouvelle, continuons donc avec des log car c'est cool
qqnorm(residuals(modele_reel))
qqline(residuals(modele_reel))

#Attention, je poursuis désormais que avec les log car ils sont mieux, voir même les log en valeur reel, cf : juste au dessus

#On test ensuite la stabilité temporelle (Cusum)
cusum <- efp(CV ~ PIB + Beef_price, data = data_reel, type = "Rec-CUSUM")

plot(cusum)
#on observe une forte rupture de tendance 
#on fait donc un test de Chow pour 2011, date de la rupture
sctest(CV_reel ~ X_reel, type="Chow", point = 7)
#p-value = 8.44e-06, il y a donc bien une rupture temporelle (mais aussi en 2006 et 2005, genre ttes les dates avant 2007 donc à voir quoi en dire et surtout à voir le Cusumsquare)

# Test (Cusum Square) plus fiable d'après le cours
rr <- (recresid(data_log$CV, modele_log))
rr <- -rr^2
cumrr <- -cumsum(rr)/scr
# le prof le fait avec des matrice et pas des dataset et je suis quand même tjs un brêle en R qu'on ne l'oublie pas donc si qql arrive à le faire je suis chaud patate !!

# Lucie vas-y je test
# CUMSUMSQ - test stabilité des coefficients du modèle dans le temps
y <- as.matrix(data_log$CV) # variable expliquée 
X <- cbind(
  1,                       # constante
  data_log$PIB,            # on ajoute les variables explicatives 
  data_log$Population,
  data_log$CPI,
  data_log$Beef_price
)
w <- recresid(y, X)       # résidus récursifs
CUSUMSQ <- cumsum(w^2) / sum(w^2) # construction du cusumsq
nrow(X) == length(y) # check de sécurité --> TRUE donc c'est bon pas de pb de longueur ou de NA 
X <- as.matrix(X)
y <- as.matrix(y)

# visualisation graphique 
plot(CUSUMSQ,
     type = "l",
     lwd = 2,
     ylab = "CUSUM of Squares",
     xlab = "Temps",
     main = "Test CUSUMSQ – Stabilité du modèle")

abline(h = c(0.05, 0.95), lty = 2, col = "red")
# la courbe ne reste en pas entre les bornes - ça veut dire pas de stabilité du modèle 
# --> rupture structurelle --> au moins un coefficient change dans le temps

#RESUME :
#On a de l'autocorrélation mais pas d'hétéroscédasticité, les résidus sont normaux avec le log et il semble y avoir une rupture temporelle vers 2007 mais je sais pas trop quoi faire. 
#Malheureusement on a de la multicolinéarité et donc il faut faire une regression Ridge, c'est ce qui est fait en dessous

#Partie 2
#on attaque une petite régression ridge des familles

set.seed(123)
modele_ridge <- cv.glmnet(
  X_reel, CV_reel,
  alpha = 0,        # Ridge
  nfolds = 5, #sinon pas assez de données par folds
  standardize = TRUE
)
#message d'avis mettant standardize = False car on a moins de 3 obs par fold, mais normalement non donc je suis pas sur de tout
# pourquoi tu veux pas mettre Population et CPI comme variables ? 

plot(modele_ridge)
#le plot est joli mais jsp ce que ça signifie
coef(modele_ridge, s = "lambda.1se") # on utilise lambda.1se car le + utilisée en économie 
# Attention les coefficients Ridge sont biaisés à cause de la pénalisation.

#Validation croisée
#les meilleurs coef ridge, en fait on a plein de coef possible variant en fonction de lambda et ici se sont les 2 lambda qui présentent les meilleurs caractéristiques de stabilioté
modele_ridge$lambda.min #minimise l'erreur moyenne de validation croisée, variance néanmoins plus importante
modele_ridge$lambda.1se #l'erreur est à moins d'un écart type du minimum, coef plus petit, plus faible variance

#Et nvoici les coefs
coefs <- coef(modele_ridge, s = "lambda.1se")
coefs
#voici comment utiliser le modèle pour faire une prédiction:
CV_pred <- predict(modele_ridge, newx = X_reel, s = "lambda.1se")

#Partie 3 quelques test possible sur notre petite regression ridge
rmse <- sqrt(mean((CV_reel - CV_pred)^2))
rmse # RMSE = 0.348  pas dégueu
# 0.091415 moi j'ai ça bizarre ...

r2 <- 1 - sum((CV_reel - CV_pred)^2) / sum((CV_reel - mean(CV_reel))^2)
r2 #0.536 pas dégueu y'a des chose à dire

#Déterminons maintenant un intervalle de confince pour notre joli modèle, par une méthode de bootstrap percentile-t avec un intervalle de confiance basé sur une statistique pivot (je cite le prof)

# Bootstrap
B <- 1000
pred_boot <- matrix(NA, nrow = B, ncol = nrow(X_reel))  # on stocke la prédiction pour chaque obs

set.seed(123)
for(b in 1:B){
  # Tirage bootstrap
  idx <- sample(1:nrow(X_log), replace = TRUE)
  Xb <- X_reel[idx, ]
  yb <- CV_reel[idx]
  
  # Ajustement Ridge
  cvb <- cv.glmnet(Xb, yb, alpha = 0, nfolds = 5)
  
  # Prédiction pour les mêmes observations (jeu original)
  pred_boot[b, ] <- predict(cvb, newx = X_reel, s = "lambda.1se")
}

# Calcul IC 95% (percentile)
ci_lower <- apply(pred_boot, 2, quantile, probs = 0.025)
ci_upper <- apply(pred_boot, 2, quantile, probs = 0.975)

# Résultat final
result <- data.frame(
  CV_orig = CV_reel,
  pred_mean = apply(pred_boot, 2, mean),
  ci_lower = ci_lower,
  ci_upper = ci_upper
)
result

#la moyenne pour avoir un intervalle de confiance pour notre modèle
#y'a bcp de Chat GPT dans cette partie, il faut être vigilant mais je galère avec le R pr tout relire et être sur de moi
pred_mean_global <- mean(result$pred_mean)
ci_lower_global <- mean(result$ci_lower)
ci_upper_global <- mean(result$ci_upper)

pred_mean_global
ci_lower_global
ci_upper_global

#Partie 4
#Réussir à trouver comment obtenir des prévisions jusqu'à 2030 pour pouvoir faire nos prévision avec ce super modèle au R² dégueu !
# Lucie - je pense que peut-être on peut regarder au moins des prévisions de l'état américain 
#sur qqs variables qui vont nous permettre de dire au modèle dans quel direction il faut aller
 






