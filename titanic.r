## Titanic: Machine Learning from Disaster

install.packages("MASS")
library(MASS)


# limpar memória do R
rm(list=ls(all=TRUE))


# Leitura dos arquivos
titanic.train <- read.csv("/home/carloap/github/R/TitanicML/train.csv")
titanic.test <- read.csv("/home/carloap/github/R/TitanicML/test.csv")

## Criar base completa! Mas também, criar a coluna faltante "Survived"
# Valores para manipulação
titanic.train$IsTrain <- TRUE
titanic.test$IsTrain <- FALSE
# Coluna Faltante
titanic.test$Survived <- NA
# Base Titanic Completa!
titanic.full <- rbind(titanic.train, titanic.test)
str(titanic.train)
tail(titanic.full)


## Manipular variavel de nomes
titanic.full$Name <- as.character(titanic.full$Name)
titanic.full$Treatment <- unlist(strsplit(titanic.full$Name, split=', '))
x <- sapply(titanic.full$Name, function(x1) strsplit(x1, ", ")[[1]])
x[2,]
y <- sapply(x[2,], function(x2) strsplit(x2, ".")[[1]])
y[1,]

## Devolve os dados limpos para o treino e teste
titanic.train <- titanic.full[titanic.full$IsTrain==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrain==FALSE,]
str(titanic.train)

# Execução do modelo
m1 <- lm(Survived~Embarked, data=train)
summary(m1)

# Modelo 2 (razoavel)
m2 <- lm(Survived~ Pclass + Age + SibSp + Parch + newSex , data=titanic.train)
summary(m2)


# Previsão dos dados
p<-predict(m2, newdata=titanic.test)
p
table(is.na(p))
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
