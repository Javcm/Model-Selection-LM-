n = 100
p = 1

#Generando las variables predictoras independientes como uniformes (0,1)
X = matrix(round(runif(n*p, 0, 1),3), ncol=p)

#Generando los errores como normales con media 0 y sigma^2 = 1
epsilon = round(rnorm(n, mean=0, sd=.5),3)

#Generando las betas del modelo Y=X\beta+\epsilon
beta = round(runif(4, -10, 10),2)

#Calculando la variable respuesta a partir del modelo Y=X\beta+\epsilon
y = beta[1] + beta[2]*X + beta[3]*X^2 + beta[4]*X^3 + epsilon

df = data.frame(X,X^2,X^3,X^4,X^5,X^6,X^7,X^8,X^9,X^10,y)
a = regsubsets(y~.,data=df[,1:10],nvmax=10)

modelos = summary(a)$which
var_names = c('X','X.2','X.3','X.4','X.5','X.6','X.7','X.8','X.9','X.10')

library(olsrr)

#El for anidado es para extraer automáticamente las fórmulas de los mejores modelos que devuelve regsubsets() 
MSE=c()
BIC=c()
R2=c()
Cp=c()
for (i in c(1:10)){
  aux = c(var_names[modelos[i,2:11]])
  aux2="y ~"
  for (j in 1:(length(aux))){
    if (j == length(aux)){aux2 = paste(aux2, as.name(aux[j]))}
    else{
      aux2 = paste(aux2, as.name(aux[j]), SEP="+")}
  }
  #En cada iteración de obtiene un modelo y cada criterio
  model = lm(as.formula(aux2), data = df)
  full_model = lm(y ~ ., data = df)
  Cp = c(Cp,ols_mallows_cp(model, full_model))
  R2 = c(R2,summary(model)$r.squared)
  MSE = c(MSE,mean((df[,11] - predict(model,df[,1:10]))^2))
  BIC = c(BIC,BIC(model))
}

#Para graficar
No_Variables=c(1:10)

BIC_df = as.data.frame(cbind(BIC,No_Variables))
Cp_df = as.data.frame(cbind(Cp,No_Variables))
MSE_df = as.data.frame(cbind(MSE,No_Variables))
R2_df = as.data.frame(cbind(R2,No_Variables))


criterios_df = as.data.frame(cbind(BIC,Cp,MSE,R2,No_Variables))
criterios1_df = as.data.frame(cbind(BIC,Cp,No_Variables))
criterios2_df = as.data.frame(cbind(MSE,R2,No_Variables))

#ggplot(BIC_df, aes(x=No_Variables, y=BIC)) + geom_point() + ggtitle("BIC") 
#ggplot(Cp_df, aes(x=No_Variables, y=Cp)) + geom_point() + ggtitle("Cp") 
#ggplot(R2_df, aes(x=No_Variables, y=R2)) + geom_point() + ggtitle("R2") 
#ggplot(MSE_df, aes(x=No_Variables, y=MSE)) + geom_point() + ggtitle("MSE") 

library(reshape2)
meltdf = melt(criterios1_df,id="No_Variables")
ggplot(meltdf,aes(x=No_Variables,y=value,colour=variable,group=variable)) + geom_line()
meltdf = melt(criterios2_df,id="No_Variables")
ggplot(meltdf,aes(x=No_Variables,y=value,colour=variable,group=variable)) + geom_line()


#Se puede observar en todas los gráficos el punto de inflección para decidir qué modelo 
#es el mejor está entre 2 y 3 variables, con dos ya se podría predecir de forma muy adecuada, 
#tal vez esto se deba a que los números simulados de $X$ son valores entre 0 y 1, de modo que al 
#tomar X^3 estos valores se hacen muy pequeños, haciendo que $y$ dependa principalmente de $X$ y $X^2$, 
#pero con 3 se logra predecir casi de forma perfecta, lo cuál nos habla bien de la metodología. 

df = data.frame(X,X^2,X^3,X^4,X^5,X^6,X^7,X^8,X^9,X^10,y)
full_model = lm(y ~ ., data = df)

library(MASS)  # Para poder usar la funcion stepAIC
modback <- stepAIC(full_model, trace=TRUE, direction="backward")

modelos = summary(a)$which
var_names = c('X','X.2','X.3','X.4','X.5','X.6','X.7','X.8','X.9','X.10')

library(olsrr)

#El for anidado es para extraer automáticamente las fórmulas de los mejores modelos que devuelve regsubsets() 
MSE=c()
BIC=c()
R2=c()
Cp=c()
for (i in c(1:10)){
  aux = c(var_names[modelos[i,2:11]])
  aux2="y ~"
  for (j in 1:(length(aux))){
    if (j == length(aux)){aux2 = paste(aux2, as.name(aux[j]))}
    else{
      aux2 = paste(aux2, as.name(aux[j]), SEP="+")}
  }
  #En cada iteración de obtiene un modelo y cada criterio
  model = lm(as.formula(aux2), data = df)
  full_model = lm(y ~ ., data = df)
  Cp = c(Cp,ols_mallows_cp(model, full_model))
  R2 = c(R2,summary(model)$r.squared)
  MSE = c(MSE,mean((df[,11] - predict(model,df[,1:10]))^2))
  BIC = c(BIC,BIC(model))
}

#Para graficar
No_Variables=c(1:10)

BIC_df = as.data.frame(cbind(BIC,No_Variables))
Cp_df = as.data.frame(cbind(Cp,No_Variables))
MSE_df = as.data.frame(cbind(MSE,No_Variables))
R2_df = as.data.frame(cbind(R2,No_Variables))


criterios_df = as.data.frame(cbind(BIC,Cp,MSE,R2,No_Variables))
criterios1_df = as.data.frame(cbind(BIC,Cp,No_Variables))
criterios2_df = as.data.frame(cbind(MSE,R2,No_Variables))

#ggplot(BIC_df, aes(x=No_Variables, y=BIC)) + geom_point() + ggtitle("BIC") 
#ggplot(Cp_df, aes(x=No_Variables, y=Cp)) + geom_point() + ggtitle("Cp") 
#ggplot(R2_df, aes(x=No_Variables, y=R2)) + geom_point() + ggtitle("R2") 
#ggplot(MSE_df, aes(x=No_Variables, y=MSE)) + geom_point() + ggtitle("MSE") 

library(reshape2)
meltdf = melt(criterios1_df,id="No_Variables")
ggplot(meltdf,aes(x=No_Variables,y=value,colour=variable,group=variable)) + geom_line()
meltdf = melt(criterios2_df,id="No_Variables")
ggplot(meltdf,aes(x=No_Variables,y=value,colour=variable,group=variable)) + geom_line()


df = data.frame(X,X^2,X^3,X^4,X^5,X^6,X^7,X^8,X^9,X^10,y)
full_model = lm(y ~ ., data = df)

library(MASS)  # Para poder usar la funcion stepAIC
modback = stepAIC(full_model, trace=TRUE, direction="backward")

summary(modback)


empty.model = lm(y ~ 1, data=df)
modforw = stepAIC(empty.model, trace=FALSE, direction="forward", scope=y ~ X + X.2 + X.4 + X.5 + X.6 + X.7 + X.8 + X.9 + X.10)
summary(modforw)

#Del método backward usando como criterio de selección el criterio de información de Akaike podemos observar que el mejor modelo 
#contempla 4 variables, que curiosamente no son las variables con las que se generó el modelo, por lo que quizá no sea la mejor 
#idea usar este criterio para la selección del modelo.