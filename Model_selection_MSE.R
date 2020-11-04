n = 1000
p = 20

#Generando las betas del modelo Y=X\beta+\epsilon
beta = round(runif(p+1, -100, 100),2)
beta[3] = 0
beta[9] = 0
beta[17] = 0

#Generando las variables predictoras independientes como uniformes (0,1)
X = cbind(rep(1,1),matrix(round(runif(n*p, 0, 1),3), ncol=20))

#Generando los errores como normales con media 0 y sigma^2 = 1
epsilon = rnorm(n, mean=0, sd=1)

#Calculando la variable respuesta a partir del modelo Y=X\beta+\epsilon
y = X %*% t(t(beta)) + epsilon

#Dividiendo el conjunto de datos en entrenamiento y prueba
set.seed(10)
ind = sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.9, 0.1))
X_train = X[ind, ]
X_test = X[!ind, ]
y_train = y[ind]
y_test = y[!ind, ]

print(c(dim(X_train),dim(X_test)))
print(c(length(y_train),length(y_test)))

#Haciendo un data frame para usar regsubsets y lm
df_train=as.data.frame(cbind(X_train,y_train))
df_test=as.data.frame(cbind(X_test,y_test))
library(plyr)
df_train = rename(df_train, c('V1'='X0','V2'='X1','V3'='X2','V4'='X3','V5'='X4','V6'='X5','V7'='X6','V8'='X7','V9'='X8','V10'='X9','V11'='X10','V12'='X11','V13'='X12','V14'='X13','V15'='X14','V16'='X15','V17'='X16','V18'='X17','V19'='X18','V20'='X19','V21'='X20','y_train'='y'))
df_test = rename(df_test, c('V1'='X0','V2'='X1','V3'='X2','V4'='X3','V5'='X4','V6'='X5','V7'='X6','V8'='X7','V9'='X8','V10'='X9','V11'='X10','V12'='X11','V13'='X12','V14'='X13','V15'='X14','V16'='X15','V17'='X16','V18'='X17','V19'='X18','V20'='X19','V21'='X20','y_test'='y'))
df_train = df_train[]
df_test = df_test[]
head(df_train)


#Para seleccionar los mejores submodelos usando regsubsets()
library(leaps)
a = regsubsets(y_train~.,data=df_train[,2:21],nvmax=p)
summary(a)

#Para graficar los MSE asociado al mejor modelo con n número de variables
library(ggplot2)
No_Variables=c(1:p)
MSE=summary(a)$rss/No_Variables
MSE_df = as.data.frame(cbind(MSE,No_Variables))
ggplot(MSE_df, aes(x=No_Variables, y=MSE)) + geom_point()

modelos = summary(a)$which
modelos[1,]
var_names = c('X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','X13','X14','X15','X16','X17','X18','X19','X20')

#El for anidado es para extraer automáticamente las fórmulas de los mejores modelos que devuelve regsubsets() 
MSE_test=c()
for (i in c(1:20)){
  aux = c(var_names[modelos[i,2:21]])
  aux2="y ~"
  for (j in 1:(length(aux))){
    if (j == length(aux)){aux2 = paste(aux2, as.name(aux[j]))}
    else{
      aux2 = paste(aux2, as.name(aux[j]), SEP="+")}
  }
  #En cada iteración de obtiene un modelo y un MSE
  model = lm(as.formula(aux2), data = df_train)
  MSE_test = c(MSE_test,mean((df_test[,22] - predict(model,df_test[,2:21]))^2))
}

#Para graficar
No_Variables=c(1:p)
MSE_df_test = as.data.frame(cbind(MSE_test,No_Variables))
ggplot(MSE_df_test, aes(x=No_Variables, y=MSE_test)) + geom_point() + ggtitle("MSE de Prueba") 

#Podemos observar que al evaluar los modelos sobre el conjunto de prueba el punto de inflección 
#del MSE se encuentra en las 13 variables, a diferencia del plot sobre el conjunto de entrenamiento, 
#esto nos habla de que para hacer buenas predicciones con el modelo quizá sí hacen falta más variables 
#que las que observábamos sobre el conjunto de prueba.
#De modo que podemos proponer el modelo con 13 variables como el mejor modelo. Comparando las betas originales con 
#las estimadas por este modelo, como se muestra abajo, se puede observar que en efecto se tienen muy buenas estimaciones 
#de estos coeficientes, difiriendo en su mayoría solo por una o dos unidades.


model = lm(y ~ X1 + X3 + X5 + X6 + X7 + X9 + X10 + X11 + X12 + X13 + X15 + 
             X17 + X18, data = df_train)
round(model$coef,2)
beta