# Conso-Viande-USA
Ici on peut écrire ce qu'on veut ! 
Bla bla bla

#import des données -----
data <- read_excel("CV_USA.xlsx", sheet = 1)
View(data)

#transformation logarithme et conso/personne (en pounds) et PIB/personne -----

data <- data %>%
  mutate(CV_percap = (CV/Population)*(10^9),
         PIB_percap = PIB/Population,
         log_CVpc = log(CV_percap),
         log_PIBpc = log(PIB_percap),
         log_Beef = log(Beef_price),
         log_CPI = log(CPI))

#création d'une série temporelle -----
startyear = min(data$Year)
ts_CVpc <- ts(data$log_CVpc, start = startyear, frequency = 1)

#Exploratory Data Analysis avant modèle ----

ggplot(data, aes(Year, CV_percap)) + geom_line() + labs(title="Consommation viande per capita")
ggplot(data, aes(Year, log_CVpc)) + geom_line() + labs(title="log(CV per capita)")


#régression linéaire log-log ----

model1 <- lm(log_CVpc ~ log_PIBpc + log_Beef + log_CPI, data = data )
summary(model1)

# test hétéroscédasticité -----

##  Breusch-Pagan -----
bptest(model1) 

## test de white approximé ------

bptest(model1, ~ fitted(model1) + I(fitted(model1)^2)) 

# Test stabilité intertemporelle -----



# CUSUM -----
cusum_test <- efp(model1, data = data, type = "Rec-CUSUM")
plot(cusum_test)

# CUSUM Square -----

## CUSUM of squares bizarre MOSUM ----
cusumsq_test <- efp(
  log_CVpc ~ log_PIBpc + log_Beef + log_CPI,
  data = data,
  type = "Rec-MOSUM"
)

plot(cusumsq_test)

## Cusum square comme le prof ----
n <- nrow(data)   # 21
k <- 3            # nombre de variables explicatives
alpha <- 0.05

c0 <- sqrt(0.5 * log(1/alpha) / (n - k))
c0


model1 <- lm(log_CVpc ~ log_PIBpc + log_Beef + log_CPI, data = data)

### Résidus récursifs -----
rr <- recresid(model1)
rr2 <- rr^2
scr <- sum(rr2)
cumrr <- cumsum(rr2) / scr

nk1 <- n - k - 1
kp2 <- k + 2
t1 <- 1:nk1
t2 <- kp2:n

smin <- ((t2 - k)/(n - k)) - c0
smax <- ((t2 - k)/(n - k)) + c0

### Regrouper -----
vec2 <- cbind(smin, cumrr, smax)

matplot(t1, vec2, type = "l", lty = c(2,1,2),
        col = c("red","blue","red"),
        xlab = "Observation", ylab = "CUSUM of Squares",
        main = "Test CUSUM of Squares - modèle log-log viande US")
legend("topleft", legend=c("Seuil inférieur","CUSUM","Seuil supérieur"),
       col=c("red","blue","red"), lty=c(2,1,2), bty="n")


cusum_sq <- efp(log_CVpc ~ log_PIBpc + log_Beef + log_CPI,
                data = data,
                type = "OLS-CUSUM")

### Tracer -----
plot(cusum_sq,
     main = "CUSUM - modèle log-log viande US",
     xlab = "Observation",
     ylab = "CUSUM")

# test multicolinéarité avec test VIF (pas ouf) ------

vif_values <- vif(model1)
vif_values_df <- data.frame(Variable = names(vif_values), VIF = vif_values)
vif_values_df

# test autocorrélation -----

dwtest(model1) # Durbin-Watson
bgtest(model1, order = 2) # Breusch-Godfrey, ajuster l'ordre


# Correction de l'autocorrélation avec gls ----



model_gls <- gls(
  log_CVpc ~ log_PIBpc + log_Beef + log_CPI,
  correlation = corAR1(form = ~ Year),
  data = data
)

summary(model_gls)


# Correction de la multicolinéarité A VOIR SI AJOUT DE VARIABLES -----

## régression ridge (modèle pas utilisable car coef pas significatifs) -----



### Préparer X et y ------
y <- data$log_CVpc
X <- as.matrix(data %>% select(log_PIBpc, log_Beef, log_CPI))

### Standardiser les variables (glmnet le fait par défaut) ------
ridge_model <- cv.glmnet(
  X, y,
  alpha = 0,            # alpha=0 → Ridge
  nfolds = 5             # 5-fold cross-validation pour choisir lambda
)

### Valeur de lambda choisie par CV ------
lambda_best <- ridge_model$lambda.min
lambda_best

### Coefficients Ridge ------
coef(ridge_model, s = "lambda.min")

#ajout prix poulet (prix moyen dans les ville américaine)-----
## Créer un data frame pour le prix moyen annuel----
chicken_price <- data.frame(
  Year = 2004:2024,
  chicken_avg_price = c(1.069583333, 1.05575, 1.049333333, 1.115166667,
                        1.206916667, 1.278166667, 1.263166667, 1.291333333,
                        1.422166667, 1.496166667, 1.53325, 1.48825,
                        1.463666667, 1.471916667, 1.497083333, 1.495,
                        1.562909091, 1.526583333, 1.800666667, 1.914166667, 1.998416667)
)

## Joindre au dataset principal----
data <- left_join(data, chicken_price, by = "Year")

## Vérifier ----
head(data)

##passage en log ----
data <- data %>%
  mutate(log_chicken = log(chicken_avg_price))

## nouveau modèle ----
model2 <- lm(log_CVpc ~ log_PIBpc + log_Beef + log_CPI + log_chicken, data = data)
summary(model2)

## multicolinéartité avec poulet -----
vif(model2)


# Prix réels avec poulet en log -----
data <- data %>%
  mutate(Beef_price_real = Beef_price / CPI,
         chicken_price_real = chicken_avg_price / CPI,
         PIB_percap_real = PIB_percap / CPI)

## Logarithmes pour le modèle log-log -----
data <- data %>%
  mutate(log_Beef_real = log(Beef_price_real),
         log_chicken_real = log(chicken_price_real),
         log_PIBpc_real = log(PIB_percap_real))

## nouveau modèle réel + poulet en log -----
model_real_log <- lm(log_CVpc ~ log_PIBpc_real + log_Beef_real + log_chicken_real, data = data)
summary(model_real_log)

# multicolinéarité ---
vif(model_real_log)

# --- Test hétéroscédasticité ---
## Breusch-Pagan
bptest(model_real_log)

## Test de White approximé
bptest(model_real_log, ~ fitted(model_real_log) + I(fitted(model_real_log)^2))

# --- Test multicolinéarité ---
vif(model_real_log)

# --- Test autocorrélation ---
dwtest(model_real_log)  # Durbin-Watson
bgtest(model_real_log, order = 2)  # Breusch-Godfrey

# --- Correction de l'autocorrélation si nécessaire avec GLS ---
model_gls2 <- gls(
  log_CVpc ~ log_PIBpc_real + log_Beef_real + log_chicken_real,
  correlation = corAR1(form = ~ Year),
  data = data
)
summary(model_gls2)
summary(model_gls)

# ajout prix soja ----
soy_data <- data.frame(
  Year = 2004:2024,
  soybeans = c(
    276.6329947, 223.1503433, 217.4539026, 317.3197308,
    452.9425192, 378.5455512, 384.9452543, 484.2459131,
    537.7618367, 517.2037784, 457.8135186, 347.3555637,
    362.7068209, 358.8242022, 342.5282471, 327.0020754,
    349.8774914, 505.1009044, 569.6917629, 520.4768147,
    405.2099965
  )
)

data <- data %>%
  left_join(soy_data, by = "Year")
## passage en log ----
data <- data %>%
  mutate(
    log_soy = log(soybeans)
  )

plot(data$Year, data$soybeans, type = "l",
     main = "Prix mondial du soja",
     ylab = "Prix", xlab = "Année")

plot(data$Year, data$log_soy, type = "l",
     main = "Log du prix du soja",
     ylab = "log(prix)", xlab = "Année")

## nouveeau modèle poulet soja en log----

model3 <- lm(
  log_CVpc ~ log_PIBpc + log_Beef + log_chicken + log_soy + log_CPI,
  data = data
)

summary(model3)

# passage en réel ----
data <- data %>%
  mutate(Beef_price_real = Beef_price / CPI,
         chicken_price_real = chicken_avg_price / CPI,
         PIB_percap_real = PIB_percap / CPI,
         soja_price_real = soybeans / CPI)

## passage en réel + log 

data <- data %>%
  mutate(log_Beef_real = log(Beef_price_real),
         log_chicken_real = log(chicken_price_real),
         log_PIBpc_real = log(PIB_percap_real),
         log_soja_price_real = log(soja_price_real))


## nouveau modèle réel + poulet + soja en log -----
model_real_log2 <- lm(log_CVpc ~ log_PIBpc_real + log_Beef_real + log_chicken_real + log_soja_price_real, data = data)
summary(model_real_log2)

### multicolinéarité 

vif(model_real_log2)

# --- Test hétéroscédasticité ---
## Breusch-Pagan
bptest(model_real_log2)

## Test de White approximé
bptest(model_real_log2, ~ fitted(model_real_log2) + I(fitted(model_real_log2)^2))

# --- Test multicolinéarité ---
vif(model_real_log2)

# --- Test autocorrélation ---
dwtest(model_real_log2)  # Durbin-Watson
bgtest(model_real_log2, order = 2)  # Breusch-Godfrey


# --- Correction de l'autocorrélation avec GLS  -> pas fou --- 
model_gls3 <- gls(
  log_CVpc ~ log_PIBpc_real + log_Beef_real + log_chicken_real,
  correlation = corAR1(form = ~ Year),
  data = data
)
summary(model_gls3)

# modèle dynamique ----

data$log_CVpc_lag <- lag(data$log_CVpc)

model_dyn <- lm(
  log_CVpc ~ log_CVpc_lag + log_PIBpc_real +
    log_Beef_real + log_chicken_real + log_soja_price_real,
  data = data
)

summary(model_dyn)

## autocorrélation ----
dwtest(model_dyn)
acf(resid(model_dyn))

### multicolinéarité 

vif(model_dyn)

# --- Test hétéroscédasticité ---
## Breusch-Pagan
bptest(model_dyn)

## Test de White approximé
bptest(model_dyn, ~ fitted(model_dyn) + I(fitted(model_dyn)^2))

# --- Test multicolinéarité ---
vif(model_dyn)

# --- Test autocorrélation ---
dwtest(model_dyn)  # Durbin-Watson
bgtest(model_dyn, order = 2)  # Breusch-Godfrey
