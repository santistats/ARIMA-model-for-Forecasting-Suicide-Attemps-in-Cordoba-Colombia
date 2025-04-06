# Importamos las librerías  -----------------------------------------------
library(tseries)
library(forecast)
library(readxl)
library(RcmdrMisc)
library(ggplot2)
library(ggfortify)
library(TSstudio)
library(urca)
library(dplyr)
library(xts)
library(ggplot2)
library(scales)
library(lubridate)
library(reshape2)
library(lmtest)
library(car)
library(TSA)
library(FinTS)
library(nortest)
######

# Creamos la serie  -------------------------------------------------------
tms = ts(s2$confirmados, start = c(2016,1), end = c(2023,12), frequency = 12);tms
tms_c = tsclean(tms);tms_c
n = length(tms);n

# Graficamos la serie  ----------------------------------------------------
df = data.frame(
  Fecha = seq(as.Date("2016-01-01"), by = "month", length.out = length(tms_c)),
  Intentos = tms_c
)
#Extraemos los índices de año y mes
df$Año = year(df$Fecha)  
df$Mes = month(df$Fecha)

ggplot(df, aes(x = Fecha, y = Intentos)) +
  geom_line(color = "royalblue", size = 0.75, linetype = "solid", alpha = 0.7) +  
  geom_point(color = "royalblue", size = 1.2, shape = 16, alpha = 0.8) +        
  labs(
    title = "Intentos de suicidio en Córdoba (2016 - 2023)",
    x = "Año",
    y = "Cantidad de intentos"
  ) +
  scale_x_date(date_labels = "%Y", breaks = date_breaks("1 year")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    text = element_text(size = 12),
    panel.grid.major.y = element_line(size = 0.3, linetype = 'dashed', color = "gray80"),  
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),  
    axis.line = element_line(color = "gray70")  
  )

#Boxplot para la tendencia
ggplot(df, aes(x = factor(Año), y = Intentos, fill = factor(Año))) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16, outlier.size = 1.5, alpha = 0.7) +
  labs(
    title = "Tendencia de los intentos de suicidio (2016 - 2023)",
    x = "Año",
    y = "Intentos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    text = element_text(size = 12),
    legend.position = "none",  # Oculta la leyenda de colores
    panel.grid.major = element_line(size = 0.3, linetype = 'dashed', color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_brewer(palette = "Set3")

#Graficamos la estacionalidad 
ts_seasonal(tms_c, type = "box", title = "Estacionalidad de los intentos de suicidio")

# Prueba de estacionareidad  ---------------------------------------------
#Hacemos la prueba de KPSS
kpss = kpss.test(tms_c);kpss
if(kpss$p.value < 0.05){
  print("La serie no es estacionaria")
} else {
  print("La serie es estacionaria")
}

# Transformación y diferenciación  ----------------------------------------
###Control de la variabilidad 
lambda = BoxCox.lambda(tms_c);lambda
tms_bc = BoxCox(tms_c, lambda = lambda);tms_bc ##Nueva variable transformada
plot(tms_bc) ##Esta es la serie que vamos a dividir (validación cruzada) 

adf = adf.test(tms_bc);adf
if(adf$p.value < 0.05){
  print("La serie  es estacionaria")
} else {
  print("La serie no es estacionaria")
}

# Validación cruzada ------------------------------------------------------
h = 12
val_cruz = ts_split(tms_bc, sample.out = h);val_cruz
train = val_cruz$train;train
test = val_cruz$test;test
#Verificamos 
ts_info(train)
ts_info(test)
### Control de la media. Diferenciación regular 
d = ndiffs(train);d
tms_bcdiff = diff(train, differences = 1);tms_bcdiff
adf = adf.test(tms_bcdiff);adf
if(adf$p.value < 0.05){
  print("La serie  es estacionaria")
} else {
  print("La serie no es estacionaria")
}
###Gráficamos la nueva serie
df_trans = data.frame(
  Fecha = seq(as.Date("2016-01-01"), by = "month", length.out = length(tms_bcdiff)),
  Intentos = tms_bcdiff
)

ggplot(df_trans, aes(x = Fecha, y = Intentos)) +
  geom_line(color = "royalblue", size = 0.75, linetype = "solid", alpha = 0.7) +  
  geom_point(color = "royalblue", size = 1.2, shape = 16, alpha = 0.8) +        
  labs(
    title = "Serie diferenciada usando h = 12",
    x = "Año",
    y = "Cantidad de intentos"
  ) +
  scale_x_date(date_labels = "%Y", breaks = date_breaks("1 year")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    text = element_text(size = 12),
    panel.grid.major.y = element_line(size = 0.3, linetype = 'dashed', color = "gray80"),  
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),  
    axis.line = element_line(color = "gray70")  
  )

##############
orden = eacf(tms_bcdiff, ar.max = 10, ma.max = 10);orden
round(orden$eacf,4)

umbral1 = 2/sqrt(96-1-1);umbral1;umbral2 = 2/sqrt(96-2-1);umbral2

# Modelos ARIMA para h = 12 -----------------------------------------------
tms_bcfit1 = Arima(train, order = c(1,1,1), method = 'ML', include.constant = T, include.drift = T);tms_bcfit1 

coeftest(tms_bcfit1)
#Sin intercepto
tms_bcfit1n = Arima(train, order = c(1,1,1), method = 'M');tms_bcfit1n

coeftest(tms_bcfit1n)


# Diagnostico del modelo para h = 12 --------------------------------------------------
##Para (1,1,1) con intercepto
ei = residuals(tms_bcfit1);ei
ad.test(ei) ##Prueba de normalidad. H0: Los datos son normales  
ArchTest(ei) ##Prueba de homogeneidad. H0: Los datos son homogeneos
t.test(ei, mu = 0) ###Prueba para centrado en 0
Box.test(ei, lag = 5, type = "Ljung-Box") 


##Para (1,1,1) sin intercepto

ein = residuals(tms_bcfit1n);ein
ad.test(ein) ##Prueba de normalidad. H0: Los datos son normales  
ArchTest(ein) ##Prueba de homogeneidad. H0: Los datos son homogeneos
t.test(ein, mu = 0) ###Prueba para centrado en 0
Box.test(ein, lag = 5, type = "Ljung-Box")

# Intervalos de predicción  -----------------------------------------------
##Para (1,1,1) con intercepto 
pronos_test1 = forecast(tms_bcfit1, h);pronos_test1 
pronos_test1$mean ##Valores pronosticados en test
pronos_test1$fitted ##Valores pronosticados en train
autoplot(pronos_test1)
##Para (1,1,1) sin intercepto 
pronos_test1n = forecast(tms_bcfit1n, h);pronos_test1n
pronos_test1n$mean
pronos_test1n$fitted

pronos_original_mean = InvBoxCox(pronos_test1n$mean, lambda);pronos_original_mean

pronos_original_fitted = InvBoxCox(pronos_test1n$fitted, lambda);pronos_original_fitted



df_pronosticos = data.frame(
  Time = time(pronos_test1n$mean),  
  Pronosticos = as.numeric(pronos_original_mean)
)

df_ajustes = data.frame(
  Time = time(pronos_test1n$fitted),  
  Ajustes = as.numeric(pronos_original_fitted)
)

ggplot() +
  geom_line(data = df_ajustes, size = 0.75, aes(x = Time, y = Ajustes, color = "Valores ajustados")) +
  geom_line(data = df_pronosticos, size = 0.8,aes(x = Time, y = Pronosticos, color = "Pronósticos")) +
  scale_color_manual(name = "Leyenda", values = c("Valores ajustados" = "royalblue", "Pronósticos" = "yellow")) +
  ggtitle("Pronósticos") +
  xlab("Años") +
  ylab("Intentos") +
  theme_minimal()

# Métricas del modelo ARIMA para h = 12 ----------------------------------------------------
##Para los datos originales: (1,1,1) con intercepto
met_train1 = accuracy(InvBoxCox(pronos_test1$fitted, lambda), InvBoxCox(train, lambda));met_train1
met_test1 = accuracy(InvBoxCox(pronos_test1$mean, lambda), InvBoxCox(test, lambda = lambda));met_test1
#(1,1,1) Sin intercepto
met_train1n_2 = accuracy(InvBoxCox(pronos_test1n$fitted, lambda), InvBoxCox(train, lambda));met_train1n_2
met_test1n_2 = accuracy(InvBoxCox(pronos_test1n$mean, lambda), InvBoxCox(test, lambda));met_test1n_2


# Modelo SARIMA para h = 12  ----------------------------------------------------------
#Usamos los datos de entrenamiento 
D = nsdiffs(train);D #Si D = 0, lag = 1. Si D = 1, lag = S
train2 = diff(diff(train, lag = 1), d = 1);train2
adf.test(train2)
ts_cor(train2, lag.max = 60)
coeftest(tms_bcfit2)
orden2 = eacf(train2, ar.max = 10, ma.max = 10);orden2
#Modelo candidato 1 
m1 = Arima(train, order = c(0,1,1), 
           seasonal = list(order = c(0,0,0), period = 12), 
           method = 'ML');m1
pronos_m1 = forecast(m1, h);pronos_m1 

# Diagnostico del modelo SARIMA para h = 12 -------------------------------
ei1_s = residuals(m1);ei1_s
ad.test(ei1_s) ##Prueba de normalidad. H0: Los datos son normales 
ArchTest(ei1_s) ##Prueba de homogeneidad. H0: Los datos son homogeneos
t.test(ei1_s, mu = 0) ###Prueba para centrado en 0
Box.test(ei1_s,lag = 5,type = 'Ljung-Box')
ts_cor(ei1_s, lag.max = 50)

# Métricas del modelo SARIMA para h = 12 ----------------------------------------------------
pronos_s1 = forecast(m1, h);pronos_s1 
met_train_s1 = accuracy(InvBoxCox(pronos_s1$fitted, lambda), InvBoxCox(train, lambda));met_train_s1
met_test_s1 = accuracy(InvBoxCox(pronos_s1$mean, lambda), InvBoxCox(test, lambda));met_test_s1



# Pronostico con h = 6  ---------------------------------------------------
##Validación cruzada 
h2 = 6
val_cruz2 = ts_split(tms_bc, sample.out = h2);val_cruz2
trainh2 = val_cruz2$train;trainh2
testh2 = val_cruz2$test;testh2
#Verificamos 
ts_info(trainh2)
ts_info(testh2)
### Control de la media. Diferenciación regular 
d2 = ndiffs(trainh2);d2
tms_bcdiffh2 = diff(trainh2, differences = 1);tms_bcdiffh2
plot(tms_bcdiffh2)
adfh2 = adf.test(tms_bcdiffh2);adfh2
if(adfh2$p.value < 0.05){
  print("La serie  es estacionaria")
} else {
  print("La serie no es estacionaria")
}
########Gráficamos la nueva serie
df_trans2 = data.frame(
  Fecha = seq(as.Date("2016-01-01"), by = "month", length.out = length(tms_bcdiffh2)),
  Intentos = tms_bcdiffh2
)

ggplot(df_trans2, aes(x = Fecha, y = Intentos)) +
  geom_line(color = "royalblue", size = 0.75, linetype = "solid", alpha = 0.7) +  
  geom_point(color = "royalblue", size = 1.2, shape = 16, alpha = 0.8) +        
  labs(
    title = "Serie diferenciada tomando h = 6",
    x = "Año",
    y = "Cantidad de intentos"
  ) +
  scale_x_date(date_labels = "%Y", breaks = date_breaks("1 year")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    text = element_text(size = 12),
    panel.grid.major.y = element_line(size = 0.3, linetype = 'dashed', color = "gray80"),  
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank(),  
    axis.line = element_line(color = "gray70")  
  )


###########
ordenh2 = eacf(tms_bcdiffh2, ar.max = 10, ma.max = 10);ordenh2
round(orden$eacf,4)

# Modelo ARIMA candidato para h = 6  ------------------------------------
tms_h2 = Arima(trainh2, order = c(1,1,1), method = 'ML', include.constant = T, include.drift = T);tms_h2
coeftest(tms_h2)

pronos_h2_1 = forecast(tms_h2, h2);pronos_h2_1

met_train_h2_1 = accuracy(InvBoxCox(pronos_h2_1$fitted, lambda),
                          InvBoxCox(trainh2, lambda));met_train_h2_1

met_test_h2_1 = accuracy(InvBoxCox(pronos_h2_1$mean, lambda), InvBoxCox(testh2, lambda));met_test_h2_1

# Diagnostico del modelo ARIMA PARA h = 6 ---------------------------------
ei_1_h2 = residuals(tms_h2);ei_1_h2
ad.test(ei_1_h2) ##Prueba de normalidad. H0: Los datos son normales 
ArchTest(ei_1_h2) ##Prueba de homogeneidad. H0: Los datos son homogeneos
t.test(ei_1_h2, mu = 0) ###Prueba para centrado en 0
Box.test(ei_1_h2,lag = 5,type = 'Ljung-Box')

# Modelo SARIMA para h = 6  -----------------------------------------------
#Usamos los datos de entrenamiento 
D2 = nsdiffs(trainh2);D2 #Si D = 0, lag = 1. Si D = 1, lag = S
train2_h2 = diff(diff(trainh2, lag = 1), d = 1);train2_h2
adf.test(train2_h2)
ts_plot(train2)
ts_cor(train2_h2, lag.max = 60)

orden2_h2 = eacf(train2_h2, ar.max = 10, ma.max = 10);orden2_h2
#Modelo candidato 1 
m1_h2 = Arima(train2_h2, order = c(0,1,1), 
              seasonal = list(order = c(0,0,0), period = 12), 
              method = 'ML');m1_h2
coeftest(m1_h2)

pronos_m1_h2 = forecast(m1_h2, h2);pronos_m1_h2 
##métricas
met_train_m1_h2 = accuracy(InvBoxCox(pronos_m1_h2$fitted, lambda), InvBoxCox(trainh2, lambda));met_train_m1_h2

met_test_m1_h2 = accuracy(InvBoxCox(pronos_m1_h2$mean, lambda), InvBoxCox(test, lambda = lambda));met_test_m1_h2


# Diagnóstico del modelo SARIMA para h = 6 -----------------------------------
eis_2 = residuals(m1_h2);eis_2
ad.test(eis_2) ##Prueba de normalidad. H0: Los datos son normales 
ArchTest(eis_2) ##Prueba de homogeneidad. H0: Los datos son homogeneos
t.test(eis_2, mu = 0) ###Prueba para centrado en 0
Box.test(eis_2,lag = 5,type = 'Ljung-Box')

# Modelos automaticos  ----------------------------------------------------
#Para h = 12
auto_model = auto.arima(train);auto_model

ei_auto1 = residuals(auto_model);ei_auto1
ad.test(ei_auto1) ##Prueba de normalidad. H0: Los datos son normales 
ArchTest(ei_auto1) ##Prueba de homogeneidad. H0: Los datos son homogeneos
t.test(ei_auto1, mu = 0) ###Prueba para centrado en 0
Box.test(ei_auto1,lag = 5,type = 'Ljung-Box')

pronos_auto = forecast(auto_model, h);pronos_auto
pronos_auto$mean ##Valores pronosticados en test
pronos_auto$fitted ##Valores pronosticados en train
autoplot(pronos_auto)

met_train_auto = accuracy(InvBoxCox(pronos_auto$fitted, lambda), InvBoxCox(train, lambda));met_train_auto
met_test_auto = accuracy(InvBoxCox(pronos_auto$mean, lambda), InvBoxCox(test, lambda = lambda));met_test_auto


#Para h = 6
auto_modelh2 = auto.arima(trainh2);auto_modelh2
pronos_auto2 = forecast(auto_modelh2, h2);pronos_auto2
pronos_auto2$mean ##Valores pronosticados en test
pronos_auto2$fitted ##Valores pronosticados en train
autoplot(pronos_auto)

met_train_auto2 = accuracy(InvBoxCox(pronos_auto2$fitted, lambda), InvBoxCox(trainh2, lambda));met_train_auto2

met_test_auto2 = accuracy(InvBoxCox(pronos_auto2$mean, lambda), InvBoxCox(testh2, lambda = lambda));met_test_auto2


ei_auto2 = residuals(auto_modelh2);ei_auto2
ad.test(ei_auto2) ##Prueba de normalidad. H0: Los datos son normales 
ArchTest(ei_auto2) ##Prueba de homogeneidad. H0: Los datos son homogeneos
t.test(ei_auto2, mu = 0) ###Prueba para centrado en 0
Box.test(ei_auto2,lag = 5,type = 'Ljung-Box')



