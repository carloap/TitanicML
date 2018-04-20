## Titanic: Machine Learning from Disaster

# limpar memória do R
rm(list=ls(all=TRUE))

# Leitura dos arquivos Treino e Teste
titanic.train <- read.csv("/home/carloap/github/R/TitanicML/train.csv", sep=",")
titanic.test <- read.csv("/home/carloap/github/R/TitanicML/test.csv", sep=",")

# Criar coluna faltante, e variavel dummy para controle
titanic.train$IsTrain <- TRUE
titanic.test$IsTrain <- FALSE
titanic.test$Survived <- NA
titanic.full <- rbind(titanic.train, titanic.test)

# Criar coluna Sex como variável contínua
titanic.full$newSex <- as.numeric(titanic.full$Sex)

## Manipular variavel de nomes, para extrair a saudação
titanic.full$Name <- as.character(titanic.full$Name)
aux <- sapply( strsplit(titanic.full$Name, split=', ', fixed=TRUE), 
               function(s)(
                 sapply(
                   strsplit(s[2], split='. ', fixed=TRUE), 
                   function(s1) (s1[1])
                 )
               )
             )

# Adicionar coluna nova e converter para texto, se for o caso
titanic.full$Treatment <- c(aux)
# titanic.full$Treatment <- as.factor(titanic.full$Treatment)
titanic.full$Treatment <- as.character(titanic.full$Treatment)

# Identificar campos com idade vazia (TRUE = NA)
titanic.full$dummy_Age <- is.na(titanic.full$Age)

# Pega todos os valores de idade dos "Mr" que não estejam vazios
AgeMr <- titanic.full[titanic.full$Treatment=="Mr" & !is.na(titanic.full$Age), ]$Age

# Histograma: Analisar a Idade dos "Mr" como exemplo
hist(AgeMr)
median(AgeMr) # Utilizando esse valor

# Manipula somente as Idades vazias = NA e as saudações "Mr" para inserir como estimativa, a idade mediana 
titanic.full[ titanic.full$Treatment=="Mr" & titanic.full$dummy_Age==TRUE ,]$Age <- median(AgeMr)

# Verificar quantas classificações ainda tem e aplicar a regra da idade (acima) para cada um que sobrou
unique(factor(titanic.full$Treatment))

# Começando a aplicar agora
# Mrs
AgeAux <- titanic.full[titanic.full$Treatment=="Mrs" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Mrs" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Miss
AgeAux <- titanic.full[titanic.full$Treatment=="Miss" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Miss" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Master
AgeAux <- titanic.full[titanic.full$Treatment=="Master" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Master" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Don
AgeAux <- titanic.full[titanic.full$Treatment=="Don" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Don" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Rev
AgeAux <- titanic.full[titanic.full$Treatment=="Rev" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Rev" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Dr
AgeAux <- titanic.full[titanic.full$Treatment=="Dr" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Dr" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Mme
AgeAux <- titanic.full[titanic.full$Treatment=="Mme" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Mme" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Ms
AgeAux <- titanic.full[titanic.full$Treatment=="Ms" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Ms" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Major
AgeAux <- titanic.full[titanic.full$Treatment=="Major" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Major" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Lady
AgeAux <- titanic.full[titanic.full$Treatment=="Lady" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Lady" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Sir
AgeAux <- titanic.full[titanic.full$Treatment=="Sir" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Sir" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Mlle
AgeAux <- titanic.full[titanic.full$Treatment=="Mlle" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Mlle" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Col
AgeAux <- titanic.full[titanic.full$Treatment=="Col" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Col" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Capt
AgeAux <- titanic.full[titanic.full$Treatment=="Capt" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Capt" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# the Countess
AgeAux <- titanic.full[titanic.full$Treatment=="the Countess" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="the Countess" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Jonkheer
AgeAux <- titanic.full[titanic.full$Treatment=="Jonkheer" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Jonkheer" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Dona
AgeAux <- titanic.full[titanic.full$Treatment=="Dona" & titanic.full$dummy_Age==FALSE, ]$Age
titanic.full[titanic.full$Treatment=="Dona" & titanic.full$dummy_Age==TRUE, ]$Age <- median(AgeAux)
# Algumas inserções podem dar erro, pois não há valores vazios de Idade, logo, não precisa inserir o que já existe

# Visualizar
str(titanic.train)
tail(titanic.full)

# Cria um novo data frame com variáveis contínuas para o modelo
attach(titanic.full)
titanic.correlation <- data.frame(
  PassengerId = PassengerId,
#  Survived = Survived,
  Pclass = Pclass,
  Age = Age,
  newSex = newSex,
  SibSp = SibSp,
#  Fare = Fare,
  Parch = Parch
)
# Matriz de correlação: somente com variáveis contínuas
cortitanic <- cor(titanic.correlation)
# Ainda precisa tratar as variáveis nulas

# Instalar a biblioteca corrplot para visualizar a matriz de correlação
# install.packages("corrplot")
library(corrplot)
# Matriz de correlação: gráfico
corrplot(cortitanic, type = "full", order = "hclust", method="pie",
         tl.col = "black", tl.srt = 45)


## Devolve os dados limpos para o treino e teste
titanic.train <- titanic.full[titanic.full$IsTrain==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrain==FALSE,]
str(titanic.train)

# Execução do modelo
# Regressão Linear simples
m1 <- lm(Survived ~ Age + Pclass + SibSp + Parch, data=titanic.train)
m1 <- lm(Survived ~ log(Age) + Pclass + SibSp + Parch, data=titanic.train)
summary(m1)

# Modelo 2 (razoavel)
m2 <- lm(Survived ~ Pclass + Age + SibSp + Parch + newSex , data=titanic.train)
m2 <- lm(Survived ~ Pclass + log(Age) + SibSp + Parch + newSex , data=titanic.train)
summary(m2)


# Previsão dos dados
p<-predict(m2, newdata=titanic.test)
p
table(!is.na(p))
hist(p)

# Classificação de Survived
pr <- ifelse(p<0.5, 0, 1)

# Montagem do dataframe para saída de dados
pr_data <- cbind(titanic.test$PassengerId, pr)
pr_data <- as.data.frame(pr_data)

# Nomes das colunas
names(pr_data) <- c("PassengerId", "Survived")

# Gravação em disco do arquivo a ser submetido no site Kaggle
write.csv(pr_data, file="/home/carloap/github/R/TitanicML/predict.csv", row.names = FALSE)
