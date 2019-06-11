

###########################
### Lectura de datos ######
###########################

setwd("titanic/")
df <- read.csv("gender_submission.csv", header = TRUE)

df <- read.csv("train.csv", header = TRUE)
head(df[,1:5]) 



###########################
### Estudio el dato  ######
###########################


sapply(df, function(x) class(x))
summary(df)
sapply(df, function(x) sum(is.na(x)))

## CORRECCI?N DEL DATO
#trace(utils:::unpackPkgZip, edit=TRUE)

library(DMwR)
library(VIM)

df$Age<- kNN(df)$Age
sapply(df, function(x) sum(is.na(x)))

## VALORES EXTREMOS
par(mfrow=c(2,3))

boxplot(df$Pclass)
boxplot(df$Age)
boxplot(df$SibSp)
boxplot(df$Parch)
boxplot(df$Fare)

boxplot.stats(df$Age)$out
boxplot.stats(df$SibSp)$out
boxplot.stats(df$Parch)$out
boxplot.stats(df$Fare)$out
boxplot.stats(df$Pclass)$out

# Exportaci?n de los datos limpios en .csv 
write.csv(df, "titanic_clean.csv")


###########################
### Selecci?n del dato ####
###########################

# Agrupaci?n por tipo de pclass 

substrRight <- function(x, n){
     sapply(x, function(xx)
         substr(xx, (nchar(xx)-n+1), nchar(xx))
     )
}

df$Sex<-as.numeric(df$Sex)
df$Embarked<-as.numeric(df$Embarked)

#DELETE VAR OF df$Cabin -> NO EXISTEN VALORES REPETIDOS. 
#<- cbind(df, dummy(df$Cabin))


df$Cabin<-as.character(df$Cabin)
df$Cabin<-substrRight((df$Cabin),2)
df$Cabin<-as.numeric(df$Cabin)

df$Ticket<-as.character(df$Ticket)
df$Ticket<-substrRight((df$Ticket),2)
df$Ticket<-as.numeric(df$Ticket)


df$PassengerId <- NULL
df$Name <- NULL


sapply(df, function(x) class(x))
summary(df)
sapply(df, function(x) sum(is.na(x)))

###########################
### Normalidad   ##########
###########################


library(nortest)
alpha = 0.05 
col.names = colnames(df)
for (i in 1:ncol(df)) { 
  if (i == 1) 
    cat("Variables que no siguen una distribuci?n normal:\n") 
  if (is.integer(df[,i]) | is.numeric(df[,i])) { 
    p_val = ad.test(df[,i])$p.value 
    if (p_val < alpha) { cat(col.names[i])
  # Format output 
      if (i < ncol(df) - 1) cat(", ") 
      if (i %% 3 == 0) cat("\n")
    }
  }
} 

colnames(df)

for(i in 2:ncol(df)){
  a<-fligner.test(Survived ~ df[,i], data = df)
  if( (a$p.value) > 0.05){
    b<-"variancia homog?neas"
  } else{
    b<-"variancia heterogenea"
  }
  print(c(colnames(df[i]), (a$p.value),b ))
}



###########################
### ##### test ###### #####
###########################


## OPTION 1: SPERMAN

corr_matrix <- matrix(nc = 2, nr = 0) 
colnames(corr_matrix) <- c("estimate", "p-value")
# Calcular el coeficiente de correlaci?n para cada variable cuantitativa 
# con respecto al campo "precio" 
for (i in 2:(ncol(df))) { 
  if (is.integer(df[,i]) | is.numeric(df[,i])) { 
    spearman_test = cor.test(df[,i], df[,1], method = "spearman") 
    corr_coef = spearman_test$estimate 
    p_val = spearman_test$p.value
# Add row to matrix 
    pair = matrix(ncol = 2, nrow = 1) 
    pair[1][1] = corr_coef 
    pair[2][1] = p_val 
    corr_matrix <- rbind(corr_matrix, pair) 
    rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(df)[i]
}
}
print(corr_matrix)

## OPTION 2: contraste de hip?tesis de dos muestras sobre la diferencia de medias

df.1.Survived <-  df[df$Pclass == 1,]$Survived  
df.2.Survived <-  df[df$Pclass == 2 | df$Pclass == 3,]$Survived 

t.test(df.1.Survived, df.2.Survived, alternative = "less") 


# OPTION 3: Regresion lineal

# Variable a predecir 
Survived = df$Survived

# Generaci?n de varios modelos 
modelo1 <- lm(Survived ~ Pclass + Sex , data = df)
modelo2 <- lm(Survived ~ Pclass + Sex + Parch + Fare , data = df) 
modelo3 <- lm(Survived ~ Pclass + Sex + Parch + Fare + Embarked, data = df) 
modelo4 <- lm(Survived ~ Pclass + Sex + Parch + Fare + Embarked + Parch, data = df) 
modelo5 <- lm(Survived ~ ., data = df)

# Tabla con los coeficientes de determinaci?n de cada modelo 
tabla.coeficientes <- matrix(c(1, summary(modelo1)$r.squared, 2, 
                               summary(modelo2)$r.squared, 3, 
                               summary(modelo3)$r.squared, 4,
                               summary(modelo4)$r.squared, 5, 
                               summary(modelo5)$r.squared), ncol = 2, byrow = TRUE)
colnames(tabla.coeficientes) <- c("Modelo", "R^2") 
tabla.coeficientes



##En caso de querer predecir el modelo...
newdata <- data.frame( Pclass = 1, 
                       Sex = 2, 
                       Parch = 3, 
                       Fare = 20, 
                       Embarked = 20 )
# Predecir el precio 
predict(modelo3, newdata)





#################################
### Representation results ######
#################################



## OPTION 1: 
library("PerformanceAnalytics")
chart.Correlation(df, histogram=TRUE, pch=19)



## OPTION 2:

# Get some colors
my_data<-df
my_data$Age<-NULL
my_data$Ticket<-NULL
my_data$Cabin<-NULL
res <- cor(my_data)
round(res, 2)

 
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

library("Hmisc")
res2 <- rcorr(as.matrix(df))
res2
par(mfrow=c(2,2))


## OPTION 3: 
plot(modelo3)

