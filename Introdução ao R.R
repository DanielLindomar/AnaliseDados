# Introdução ao R
# Lindomar Pegorini Daniel

# Mude a pasta de trabalho para onde está o arquivo de vendas de limonada
setwd("C:/Users/lindo/Google Drive/Lindomar/UNEMAT/ENSINO/SEMESTRES LETIVOS/SEMESTRE LETIVO 2020.3/Econometria I/Unidade 1- Introdução ao modelo de regressão linear/Tópico 1.2 Introdução à econometria/Aula 8 - Introdução ao R/")

# Leia o arquivo de dados de limonada
library(readxl)
Limonada <- read_excel("Limonada.xls")
View(Limonada)
attach(Limonada)

# Listando as variáveis
names(Limonada)

# Mostre as primeiras linhas dos dados
head(Limonada)
Limonada[1:10,]

# Estatísiticas descritivas
summary(Vendas)
sd(Vendas)
length(Vendas)
summary(Panfletos)
sd(Panfletos)

# Tabelas de frequência
table(Preço)
table(Vendas, Preço)

# Histograma
hist(Vendas)

# Correlação entre variáveis
cor(Vendas, Panfletos)

# Teste-t para teste de igualdade de média
t.test(Vendas, mu=29.9)

# Teste  de igualdade de média entre grupos
anova(lm(Vendas ~ factor(Preço)))
anova(lm(Vendas ~ factor(Dia)))

# Regressão de MQO - Vendas (variável dependente) e Temperatura, Preço e Panfletos (variáveis independentes)
reg_vendas <- lm(Vendas ~ Temperatura + Preço + Panfletos)
summary(reg_vendas)

# Gráficos
plot (Vendas ~ Temperatura)
reg_vendas1 <- lm(Vendas ~ Temperatura)
abline(reg_vendas1)

# Redefinindo variáveis 
Y <- cbind(Vendas)
X <- cbind(Temperatura, Preço, Panfletos)
summary(Y)
summary(X)
reg_vendas <- lm(Y ~ X)
summary(reg_vendas) 

# Install and use packages 
# install.packages("nome do pacote")
# library(nome do pacote)
