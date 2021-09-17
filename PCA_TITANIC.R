

titanic <- read.csv("~/MAEA/Materias/Analisis Multivariado/R/Datos/titanic.csv")
str(titanic)
#tabla de supervivencia
table(titanic$Survived, titanic$Pclass)

#

library(tidyverse)
library(psych)
str(titanic)
#Para el ejercicio, se tomaron únicamente las variables Survived, 
#Pclass, Sex, Age, Sibso, Parch y Fare. Con #esto, se partió la base
#de datos en dos: el 80% de los datos para entrenamiento (train),
#y el restante para #evaluación (test).
df<- titanic %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare)
df$Sex<-recode(df$Sex, 'male'=0, 'female'=1)

set.seed(0)
ind <- sample(2,nrow(df),
              replace=TRUE,
              prob = c(0.8,0.2))

train <- df[ind==1,]
test  <- df[ind==2,]
#Para estudiar las correlaciones, se acomoda la base de datos
#como matriz y se obtiene la correlación absoluta entre vairables.
#Se omitieron las filas donde no existía registro de edad.
train<- na.omit(train)



train_cor<- cor(train, method = "pearson")
train_cor <- data.frame(cor=train_cor[1:7,7])

###  
train_cor <- train_cor %>% 
  mutate(cor_abs = abs(cor))

train_cor$Var<- rownames(train_cor)

train_cor<- train_cor %>%
  mutate(Var= replace(Var,Var == 1, "Survived"),
         Var= replace(Var,Var == 2, "Pclass"),
         Var= replace(Var,Var == 3, "Sex"),
         Var= replace(Var,Var == 4, "Age"),
         Var= replace(Var,Var == 5, "SibSp"),
         Var= replace(Var,Var == 6, "Parch"),
         Var= replace(Var,Var == 7, "Fare")
  )
train_cor
ggplot(train_cor,aes(x=Var, y=cor_abs))+
  geom_bar(stat = "identity", color = "steelblue", fill="white")


#Se filtraron las variables para considerar solamente aquellas 
#con correlaciones absolutas mayores que 0.2.
#Posteriormente se graficaron de nuevo las correlaciones.
train_cor <- train_cor %>% filter(cor_abs>0.2)

train_filtrado <- train %>% 
  select(Survived,one_of(as.character(train_cor$Var)))

library(GGally)
ggpairs(df, mapping = aes(col = Survived)) +
  theme_bw()

pairs.panels(train_filtrado,
             gap=0,
             bg= c("blue","yellow","red")[train$Survived],
             pch=21)


### PCA
pca <- select(train_filtrado, -1) %>%
  prcomp(center = TRUE, scale = TRUE)
plot(pca)
summary(pca)
pca$scale

map_dfc(1:3, ~pca$rotation[, .] * sqrt(pca$sdev ^ 2)[.])

#install.packages("factoextra")
library(factoextra)
pcaDat <- get_pca(pca)
fviz_pca_biplot(pca, label = "var")
fviz_pca_var(pca)
fviz_screeplot(pca, addlabels = TRUE, choice = "eigenvalue")
fviz_screeplot(pca, addlabels = TRUE, choice = "variance")

####


titanic_Pca <- train_filtrado %>%
  mutate(PCA1 = pca$x[, 1], PCA2 = pca$x[, 2])
ggplot(titanic_Pca, aes(PCA1, PCA2, col = Survived)) +
  geom_point() +
  theme_bw()





library(nnet)

pred<- predict(pca, train)
training_set<- data.frame(pred, train[1])


modelo<- glm(Survived ~ PC1 + PC2 + PC3, data=training_set, family = binomial)

summary(modelo)

glm.probs=predict(modelo, type = "response")
#Con el modelo anterior, se revisará el % de aciertos del
#modelo de predicción mediante una matriz de 
#confusión. 
#Para ello, mediante la librería ROCR se buscará cuál es el
#valor de probabilidad más adecuado para hacer la clasificación.


library(ROCR)

ROCRpred<- prediction(glm.probs, training_set$Survived)

ROCRperf= performance(ROCRpred, "tpr", "fpr")

plot(ROCRperf, colorize=T, print.cutoffs.at= seq(0, 1, 0.05),
     text.adj=c(-0.2, 1.7))
glm.pred=rep("Died", 580)
glm.pred[glm.probs>0.47]= "Survived"

tabla<- table(glm.pred, training_set$Survived)

aciertos<- sum(diag(tabla)/sum(tabla))*100; aciertos
errores<- (100-aciertos); errores
##########
pcaUnscaled <- select(titanic_Pca, -Survived) %>%
  prcomp(center = TRUE, scale = FALSE)
pcaUnscaled

fviz_pca_biplot(pcaUnscaled, label = "var")
fviz_pca_var(pcaUnscaled)
fviz_screeplot(pcaUnscaled, addlabels = TRUE, choice = "variance")


##   lo mismo que en el ejercicio 2 nuevamente, pero esta vez
## establezca los argumentos center = FALSE y escala = VERDADERO.

pcaUncentered <- select(titanic_Pca, -Survived) %>%
  prcomp(center = FALSE, scale = TRUE)
pcaUncentered


fviz_pca_biplot(pcaUncentered, label = "var")
fviz_pca_var(pcaUncentered)
fviz_screeplot(pcaUncentered, addlabels = TRUE, choice = "variance")

