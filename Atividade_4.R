###########################

## Projeto_Integrador
## Exercício_4
## Warley_Gomes 
## Ciências de Dados

###########################

install.packages("readr")
install.packages("descr")
install.packages("dplyr")

library(readr)
library(descr)
library(dplyr)

enade14 <- read_csv2("https://raw.githubusercontent.com/neylsoncrepalde/introducao_ao_r/master/dados/enade_2014_amostra.csv")

# 1) Extraia a média, a mediana, mínimo, máximo,
# variância e desvio padrão da idade para todos os alunos da nossa amostra aleatória.

mean(enade14$nu_idade)
median(enade14$nu_idade)
min(enade14$nu_idade)
max(enade14$nu_idade)
var(enade14$nu_idade)
sd(enade14$nu_idade)

# 2) Elabore uma tabela de frequência da quantidade de alunos por sexo. 
# Corrija a variável caso alguma categoria esteja “sobrando”.

enade14$tp_sexo[enade14$tp_sexo == "N"] <- NA 

freq(enade14$tp_sexo, plot=F)


# 3) Agora extraia a média, a mediana, mínimo, máximo, variância
# e desvio padrão da idade para cada categoria de sexo. Exiba os mesmos resultados com um gráfico.


mean(enade14$nu_idade[enade14$tp_sexo == "F"], na.rm = T)
mean(enade14$nu_idade[enade14$tp_sexo == "M"], na.rm = T)

median(enade14$nu_idade[enade14$tp_sexo == "F"], na.rm = T)
median(enade14$nu_idade[enade14$tp_sexo == "M"], na.rm = T)

min(enade14$nu_idade[enade14$tp_sexo == "F"], na.rm = T)
min(enade14$nu_idade[enade14$tp_sexo == "M"], na.rm = T)

max(enade14$nu_idade[enade14$tp_sexo == "F"], na.rm = T)
max(enade14$nu_idade[enade14$tp_sexo == "M"], na.rm = T)

var (enade14$nu_idade[enade14$tp_sexo == "F"], na.rm = T)
var (enade14$nu_idade[enade14$tp_sexo == "M"], na.rm = T)

sd (enade14$nu_idade[enade14$tp_sexo == "F"], na.rm = T)
sd (enade14$nu_idade[enade14$tp_sexo == "M"], na.rm = T)

boxplot(enade14$nu_idade~enade14$tp_sexo)

# 4)Agora extraia a média, a mediana, mínimo, máximo, 
# variância e desvio padrão da idade para cada categoria de cor/raça. Exiba os mesmos resultados com um gráfico.


mean(enade14$nu_idade[enade14$qe_i2 == "a"], na.rm = T)
median(enade14$nu_idade[enade14$qe_i2 == "a"], na.rm = T)
min(enade14$nu_idade[enade14$qe_i2 == "a"], na.rm = T)
max(enade14$nu_idade[enade14$qe_i2 == "a"], na.rm = T)
var(enade14$nu_idade[enade14$qe_i2 == "a"], na.rm = T)
sd(enade14$nu_idade[enade14$qe_i2 == "a"], na.rm = T)

mean(enade14$nu_idade[enade14$qe_i2 == "b"], na.rm = T)
median(enade14$nu_idade[enade14$qe_i2 == "b"], na.rm = T)
min(enade14$nu_idade[enade14$qe_i2 == "b"], na.rm = T)
max(enade14$nu_idade[enade14$qe_i2 == "b"], na.rm = T)
var(enade14$nu_idade[enade14$qe_i2 == "b"], na.rm = T)
sd(enade14$nu_idade[enade14$qe_i2 == "b"], na.rm = T)

mean(enade14$nu_idade[enade14$qe_i2 == "c"], na.rm = T)
median(enade14$nu_idade[enade14$qe_i2 == "c"], na.rm = T)
min(enade14$nu_idade[enade14$qe_i2 == "c"], na.rm = T)
max(enade14$nu_idade[enade14$qe_i2 == "c"], na.rm = T)
var(enade14$nu_idade[enade14$qe_i2 == "c"], na.rm = T)
sd(enade14$nu_idade[enade14$qe_i2 == "c"], na.rm = T)

mean(enade14$nu_idade[enade14$qe_i2 == "d"], na.rm = T)
median(enade14$nu_idade[enade14$qe_i2 == "d"], na.rm = T)
min(enade14$nu_idade[enade14$qe_i2 == "d"], na.rm = T)
max(enade14$nu_idade[enade14$qe_i2 == "d"], na.rm = T)
var(enade14$nu_idade[enade14$qe_i2 == "d"], na.rm = T)
sd(enade14$nu_idade[enade14$qe_i2 == "d"], na.rm = T)


mean(enade14$nu_idade[enade14$qe_i2 == "e"], na.rm = T)
median(enade14$nu_idade[enade14$qe_i2 == "e"], na.rm = T)
min(enade14$nu_idade[enade14$qe_i2 == "e"], na.rm = T)
max(enade14$nu_idade[enade14$qe_i2 == "e"], na.rm = T)
var(enade14$nu_idade[enade14$qe_i2 == "e"], na.rm = T)
sd(enade14$nu_idade[enade14$qe_i2 == "e"], na.rm = T)


mean(enade14$nu_idade~enade14$tp_sexo, na.rm = T) 
boxplot(enade14$nu_idade~enade14$qe_i2, main="Resultados", col="orange")

# 5) Verifique a distribuição de alunos por região do país. Exiba uma tabela de frequências e um gráfico.

enade14$distrib_regiao <- enade14$co_regiao_curso

enade14$distrib_regiao [enade14$distrib_regiao == 1] = "1-Norte"
enade14$distrib_regiao [enade14$distrib_regiao == 2] ="2-Nordeste"
enade14$distrib_regiao [enade14$distrib_regiao == 3] ="3-Sudeste"
enade14$distrib_regiao [enade14$distrib_regiao == 4] ="4-Sul"
enade14$distrib_regiao [enade14$distrib_regiao == 5] ="5-Centro-Oeste"

freq(enade14$distrib_regiao, plot = T, main="Distribuição de alunos por região", col="pink", col.main="darkgray")

# 6) Vamos investigar a associação entre a renda e a cor. 
# Faça uma tabela cruzada entre essas duas variáveis.
# criação das variáveis auxiliares
enade14$cor_raca <- enade14$qe_i2
enade14$distribuicao_renda <- enade14$qe_i8

# preenchimento da variavel cor_raca
enade14$cor_raca[enade14$cor_raca == "a"] <- "Brancos"
enade14$cor_raca[enade14$cor_raca == "b"] <- "Negros"
enade14$cor_raca[enade14$cor_raca == "c"] <- "Pardos"
enade14$cor_raca[enade14$cor_raca == "d"] <- "Amarelos"
enade14$cor_raca[enade14$cor_raca == "e"] <- "Indigenas"

# preenchimento da variavel distribuicao de renda
enade14$distribuicao_renda <- enade14$qe_i8
enade14$distribuicao_renda[enade14$distribuicao_renda == "a"] <- "[até 1,5]"
enade14$distribuicao_renda[enade14$distribuicao_renda == "b"] <- "[de 1,5 a 3]"
enade14$distribuicao_renda[enade14$distribuicao_renda == "c"] <- "[de 3 a 4,5]"
enade14$distribuicao_renda[enade14$distribuicao_renda == "d"] <- "[de 4,5 a 6]"
enade14$distribuicao_renda[enade14$distribuicao_renda == "e"] <- "[de 6 a 10]"
enade14$distribuicao_renda[enade14$distribuicao_renda == "f"] <- "[de 10 a 30]"
enade14$distribuicao_renda[enade14$distribuicao_renda == "g"] <- "[acima de 30]"

# cria uma tabela comparando as variaveis renda e cor
comparacao<- table(enade14$cor_raca, enade14$distribuicao_renda)
print(comparacao)




