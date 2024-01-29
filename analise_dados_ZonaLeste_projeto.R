##############################################
### PROJETO ANALISE DE DADOS SOBRE A COVID ###
##############################################

#CARREGAR OS PACOTES
library(dplyr)
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)

#BUSCAR O DIRETORIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/gilbe/OneDrive/Documentos/Estudo Linguagem R/dados-covid-sp-master/data")

#ABRIR O ARQUIVO
covid_sp_tratado <- read.csv('covid_sp_tratado.csv', sep = ",")
View(covid_sp_tratado)

covid_sp_tratado$data <- as.Date(covid_sp_tratado$data, format ='%Y-%m-%d')
glimpse(covid_sp_tratado)

covid_sp_tratado$idoso <- as.numeric(covid_sp_tratado$idoso)
glimpse(covid_sp_tratado)

# Excluir coluna idoso(%)
covid_sp_tratado <- select(covid_sp_tratado, -c(18))

# Renomeando a coluna idoso
covid_sp_tratado <- rename(covid_sp_tratado, porcentagem_idoso = idoso)


# Filtro por linha (cidade)

# Poá
covid_poa <- covid_sp_tratado %>% filter(municipio=="Poá")
covid_poa["dens_demografica"] <- covid_poa$pop/covid_poa$area
covid_poa["area"] <- covid_poa$area/100
View(covid_poa)

#suzano
covid_suzano <- covid_sp_tratado %>% filter(municipio=="Suzano")
covid_suzano["dens_demografica"] <- covid_suzano$pop/covid_suzano$area
covid_suzano["area"] <- covid_suzano$area/100
View(covid_suzano)

# Ferraz de vasconcelos
covid_ferraz <- covid_sp_tratado %>% filter(municipio=="Ferraz de Vasconcelos")
covid_ferraz["dens_demografica"] <- covid_ferraz$pop/covid_ferraz$area
covid_ferraz["area"] <- covid_ferraz$area/100
View(covid_ferraz)



#ANALISE ESTATISTICAs

#Media
summarise_at(covid_poa, vars(obitos_novos, casos_novos), mean)
summarise_at(covid_suzano, vars(obitos_novos, casos_novos), mean)
summarise_at(covid_ferraz, vars(obitos_novos, casos_novos), mean)


#Media Movel
plot(covid_poa$data,covid_poa$casos_mm7d, title("MEDIA MOVEL"), col = "red")
plot(covid_poa$data,covid_poa$obitos_mm7d, title("MEDIA MOVEL"), col = "purple")

plot(covid_suzano$data,covid_suzano$casos_mm7d, title("MEDIA MOVEL"), col = "red")
plot(covid_suzano$data,covid_suzano$obitos_mm7d, title("MEDIA MOVEL"), col = "purple")

plot(covid_ferraz$data,covid_ferraz$casos_mm7d, title("MEDIA MOVEL"), col = "red")
plot(covid_ferraz$data,covid_ferraz$obitos_mm7d, title("MEDIA MOVEL"), col = "purple")

# Mediana
summarise_at(covid_poa, vars(obitos_novos, casos_novos), median)
summarise_at(covid_suzano, vars(obitos_novos, casos_novos), median)
summarise_at(covid_ferraz, vars(obitos_novos, casos_novos), median)

# Moda
# Criando uma fun??o
moda <- function(m) {
  valor_unico <- unique(m) #Busca o valor ?nico para a coluna.
  valor_unico[which.max(tabulate(match(m, valor_unico)))] #tabular (contabilizar quantas vezes o valor ?nico aparece) e buscar o maior valor
}
#Obtendo Moda
summarise_at(covid_poa, vars(obitos_novos, casos_novos), moda)
summarise_at(covid_suzano, vars(obitos_novos, casos_novos), moda)
summarise_at(covid_ferraz, vars(obitos_novos, casos_novos), moda)

#filtrando em um mes expecifico 
covid_ferraz_setembro <- covid_ferraz %>% filter(mes==9)

summarise_at(covid_ferraz_setembro, vars(obitos_novos, casos_novos), moda)

summarise_at(covid_ferraz_setembro, vars(obitos_novos, casos_novos), mean)

# Histograma

hist(covid_poa$obitos_novos, col = "blue")
hist(covid_poa$casos_novos, col = "purple")

hist(covid_suzano$obitos_novos, col = "green")
hist(covid_suzano$casos_novos, col = "yellow")

hist(covid_ferraz$obitos_novos, col = "red")
hist(covid_ferraz$casos_novos, col = "brown")

#Medidas de Posição

# M?nimo
summarise_at(covid_poa, vars(obitos_novos, casos_novos), min)
summarise_at(covid_suzano, vars(obitos_novos, casos_novos), min)
summarise_at(covid_ferraz, vars(obitos_novos, casos_novos), min)

# M?ximo
summarise_at(covid_poa, vars(obitos_novos, casos_novos), max)
summarise_at(covid_suzano, vars(obitos_novos, casos_novos), max)
summarise_at(covid_ferraz, vars(obitos_novos, casos_novos), max)

# Amplitude Total
summarise_at(covid_poa, vars(obitos_novos, casos_novos), range)
summarise_at(covid_suzano, vars(obitos_novos, casos_novos), range)
summarise_at(covid_ferraz, vars(obitos_novos, casos_novos), range)

# Quartis
summarise_at(covid_poa, vars(obitos_novos, casos_novos), quantile)
summarise_at(covid_suzano, vars(obitos_novos, casos_novos), quantile)
summarise_at(covid_ferraz, vars(obitos_novos, casos_novos), quantile)

# Amplitude Interquartil
summarise_at(covid_poa, vars(obitos_novos, casos_novos), IQR)
summarise_at(covid_suzano, vars(obitos_novos, casos_novos), IQR)
summarise_at(covid_ferraz, vars(obitos_novos, casos_novos), IQR)

# O summary resulta em resumo estat?stico
# de todas as variaveis numericas/inteiras
summary(covid_poa)
summary(covid_suzano)
summary(covid_ferraz)

# Box Plot
summary(covid_poa$obitos_novos)
boxplot(covid_poa$obitos_novos)

summary(covid_poa$casos_novos)
boxplot(covid_poa$casos_novos)

summary(covid_suzano$obitos_novos)
boxplot(covid_suzano$obitos_novos)

summary(covid_suzano$casos_novos)
boxplot(covid_suzano$casos_novos)

summary(covid_ferraz$obitos_novos)
boxplot(covid_ferraz$obitos_novos)

summary(covid_suzano$casos_novos)
boxplot(covid_suzano$casos_novos)

# Tratando os outliers
# Identificando e excluindo todos os outliers

#POA
covid_poa %>% identify_outliers(casos_novos)
outliers = c(boxplot.stats(covid_poa$casos_novos)$out)
covid_poa_sem_outliers <- covid_poa[-c(which(covid_poa$casos_novos %in% outliers)),  ]
boxplot(covid_poa_sem_outliers$casos_novos)

#SUZANO
covid_suzano %>% identify_outliers(casos_novos)
outliers = c(boxplot.stats(covid_suzano$casos_novos)$out)
covid_suzano_sem_outliers <- covid_suzano[-c(which(covid_suzano$casos_novos %in% outliers)),  ]
boxplot(covid_suzano_sem_outliers$casos_novos)

#FERRAZ
covid_ferraz %>% identify_outliers(casos_novos)
outliers = c(boxplot.stats(covid_ferraz$casos_novos)$out)
covid_ferraz_sem_outliers <- covid_ferraz[-c(which(covid_ferraz$casos_novos %in% outliers)),  ]
boxplot(covid_ferraz_sem_outliers$casos_novos)


# Excluindo alguns outliers

#POA
covid_poa_sem_outliers %>% identify_outliers(casos_novos)
data_poa_excluir <- c('2020-07-02','2020-08-12','2021-01-15','2021-02-24')
covid_poa_sem_outliers<-covid_poa_sem_outliers %>% filter(!data %in% data_poa_excluir)
boxplot(covid_poa_sem_outliers$casos_novos)

#SUZANO
covid_suzano_sem_outliers %>% identify_outliers(casos_novos)
mes_suzano_excluir <- c('2020-07-24','2020-07-31','2020-08-22','2020-10-16','2020-12-18','2021-01-06','2021-02-12')
covid_suzano_sem_outliers<-covid_suzano_sem_outliers %>% filter(!mes %in% mes_suzano_excluir)
boxplot(covid_suzano_sem_outliers$casos_novos)

#FERRAZ
covid_ferraz_sem_outliers %>% identify_outliers(casos_novos)
mes_ferraz_excluir <- c('7','8','9','10','2')
covid_suzano_sem_outliers<-covid_ferraz_sem_outliers %>% filter(!mes %in% mes_ferraz_excluir)
boxplot(covid_poa_sem_outliers$casos_novos)


# Medidas de Dispersao

# Variancia
var(covid_poa$obitos_novos)
var(covid_poa$casos_novos)

var(covid_suzano$obitos_novos)
var(covid_suzano$casos_novos)

var(covid_ferraz$obitos_novos)
var(covid_ferraz$casos_novos)

# Desvio padr?o
sd(covid_poa$obitos_novos)
sd(covid_poa$casos_novos)

sd(covid_suzano$obitos_novos)
sd(covid_suzano$casos_novos)

sd(covid_ferraz$obitos_novos)
sd(covid_ferraz$casos_novos)


# TESTES DE NORMALIDADE

# Existem 4 testes de normalidade principais (numericos) e dois testes graficos:
# Shapiro-Wilk (limite de 5000 amostras)
# Anderson-Darling
# Kolmogorov_Smirnov
# Cramer-Von Mises
# Histograma
# QQplot

# Nivel de significancia DE 0,05(5%) ou nivel de confiança de 95%(MAIS UTILIZADO):
# Quando o parametro p > 0,05 (distribuição normal).


if(!require(nortest)) install.packages("nortest")
library(nortest)

#Histograma
hist(covid_poa$casos_novos, probability=T, col="blue")
lines(density(covid_poa$casos_novos) , col="red")

# QQPLOT (GRAFICO DE DISTRIBUIÇÃO NORMAL)
qqnorm(covid_poa$casos_novos)
qqline(covid_poa$casos_novos)

# Shapiro-Wilk
shapiro.test(covid_poa$casos_novos)

# Anderson-Darling
ad.test(covid_poa$casos_novos)

# Kolmogorov_Smirnov
lillie.test(covid_poa$casos_novos)

#Cramer-Von Mises
cvm.test(covid_poa$casos_novos)



# CORRELAÇÃO LINEAR
# method: "pearson" para dados paramétricos(normalidade e homocedasticidade))
#         "spearman" (volume grande de dados não paramétricos)
#         "kendall" (volume pequeno de dados não paramétricos)

plot(covid_poa$casos,covid_poa$obitos)
cor(covid_poa$casos,covid_poa$obitos,method = "spearman")

regressao <- lm(formula= obitos ~ casos, data=covid_poa) #modelo de regressão
regressao$coefficients
summary(regressao)

### Equa??o: obitos=16.83+0,0415*casos

### Coeficiente de determinação (ajustado): 0.9518 



### GR?FICO DE LINHA COM AJUSTE DE RETA  COM GGPLOT2

if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(ggpubr)) install.packages("ggpubr") #equa??o da reta no gr?fico
library(ggpubr)

ggplot(data = covid_poa, mapping = aes(x = casos, y = obitos)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
                                          sep = "*plain(\" , \")~~")), label.x = 1500,
                        label.y = 200) +
  theme_gray()


if(!require(corrplot)) install.packages("corrplot")                               
library(corrplot) # gr?fico de correla??o 

matriz_corr <- cor(covid_poa[5:13], method = "spearman")
View(matriz_corr)

corrplot(matriz_corr, method = "color")
corrplot(matriz_corr, method="color", 
         type="full", order="original", 
         addCoef.col = "black", # adiciona o coeficiente é matriz
         tl.col="black", tl.srt=45, # cor e rotação do nome das variáveis
)



# GR?FICOS LINEARES POR CIDADES

covid_cidades<-covid_sp_tratado %>% filter (municipio  %in% c("Poá", "Ferraz de Vasconcelos", "Suzano"))
View(covid_cidades)

ggplot(covid_cidades, aes(x = casos, y = obitos, color = municipio)) +
  geom_line() +
  labs(title = "Evolução dos obitos em função dos casos de COVID",
       x = "Casos",
       y = "Obitos") +
  theme_classic()





