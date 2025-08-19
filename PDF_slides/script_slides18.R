####################################################
###             Exemplos dos slides 18           ###
####################################################

# Exemplo 18.1

# Fixando a semente
set.seed(123)

# Pacotes
library(tidyverse) # manipulação de dados
library(ggplot2)   # gráficos
library(boot)      # para usar bootstrap

# Simular os diâmetros da população de árvores
# Os dados serão armazenados em um data frame para
# fazer gráficos com ggplot
pop <- data.frame(
  diametros = rnorm(10000, mean = 50, sd = 10)
)

# Histograma da distribuição dos diâmetros
pop %>%
  ggplot(aes(x = diametros)) +
  geom_histogram(alpha = 0.3, fill = "darkgreen") +
  labs(
    title = "Distribuição de Diâmetros da População",
    subtitle = paste0("Media: ", round(mean(pop$diametros),1), 
                      ". Desvio-padrão: ", round(sd(pop$diametros), 2))
  ) +
  theme_light()

# Coletando uma amostra de tamanho 100
amostra <- data.frame(
  diametros = sample(pop$diametros, size = 100)
)

amostra  %>%
  ggplot(aes(x = diametros)) +
  geom_histogram(alpha = 0.3, fill = "red") +
  labs(
    title = "Distribuição de Diâmetros da Amostra",
    subtitle = paste0("Media: ", round(mean(amostra$diametros),1), 
                      ". Desvio-padrão: ",round(sd(amostra$diametros), 2))
  ) +
  theme_light()

### Intervalo de confiança tradicional
t.test(x = amostra$diametros)

### Intervalo de confiança bootstrap
# função que calcula a média para usar em 'boot()'
media.boot <- function(x, i){
  return(mean(x[i]))
}

# bootstrap com 10.000 réplicas
m.b <- boot(amostra$diametros,
            statistic = media.boot,
            R = 10000)
m.b
boot.ci(m.b, conf = .95, type = "perc")

#***
# Usando o laço for
m <- vector()                        # vetor de médias bootstrap
for (b in 1:10000) {                 # laço boostrap
  m[b] <- mean(sample(amostra$diametros,   # obtenção da amostra
                   size = nrow(amostra), # artificial e cálculo
                   replace = T))           # da média
}                                    # fim do laço bootstrap

# intervalo de confiança de 95%
quantile(x = m, probs = c(.025, .975))
hist(m)
qqnorm(m);qqline(m)

#***************************************************

# Exemplo 18.2

# função que calcula a mediana para usar em 'boot()'
mediana.boot <- function(x, i){
  return(median(x[i]))
}

# bootstrap com 10.000 réplicas
md.b <- boot(amostra$diametros, 
            statistic = mediana.boot,
            R = 10000)
md.b
plot(md.b)
boot.ci(md.b, conf = .95, type = "all")


#***************************************************

# Exemplo 18.3

# tamanho da amostra
nrow(mtcars)

# Fnção que calcula R2
r2 <- function(formula, dados, i) {
  val <- dados[i,] # seleciona a amostra para a função boot
  fit <- lm(formula, data = val) # ajusta o modelo
  return(summary(fit)$r.square)  # retorna o r2
}

# Bootstrap com 1500 réplicas
r2.boot <- boot(data = mtcars, statistic = r2, 
                R = 1500, formula = mpg~wt+disp)
r2.boot
plot(r2.boot)

# Intervalo com 95% de confiança
boot.ci(r2.boot, type="bca")

#***************************************************

# Exemplo 18.4
diametro_a <- c(51.58, 41.66, 42.54, 43.84, 46.84, 61.99,
            42.56, 48.05, 55.97, 51.34, 50.29, 51.04,
            54.67, 48.36, 60.41, 45.63, 54.54, 47.93,
            55.66, 53.63, 39.97, 50.51, 45.57, 44.60,
            41.31, 50.75, 45.03, 49.79, 48.25, 55.51)
diametro_b <- c(49.59, 36.08, 40.01, 38.67, 42.00, 42.82,
            38.87, 39.30, 49.10, 35.05, 39.74, 32.45,
            34.52, 32.01, 38.68, 39.62, 46.61, 44.93,
            44.34, 35.21, 45.07, 38.11, 39.14, 38.98,
            40.49, 38.16, 33.61, 38.14, 33.80, 33.31)
dados <- data.frame(
  area = rep(c("A","B"), each = 30),
  diametros = c(diametro_a, diametro_b)
)

# Gráfico de boxplot para comparar as distribuições
dados %>%
  ggplot(aes(y = diametros, x = area, fill = area, col = area)) +
  geom_boxplot(alpha = 0.3, position = "identity") +
  labs(
    title = "Comparação de diâmetros entre áreas A e B",
    subtitle = paste0("Media area A: ", round(mean(diametro_a),1), 
                      ", média area B: ", round(mean(diametro_b),2))
  ) +
  theme_light() +
  theme(legend.position = "none")

### Teste t para comparação
t.test(diametro_a, diametro_b)

shapiro.test(diametro_a)
shapiro.test(diametro_b)

### Versão bootstrap
# Função que retorna a estatística:
diferenca <- function(x, i) {
  dif = mean(x$diametro_a[i]) - mean(x$diametro_b[i]) # Calcula a diferença de médias
  return(dif)
}

# Bootstrapping com 10.000 replicações
teste_ab = boot(data = data.frame(diametro_a, diametro_b),
                statistic = diferenca, R = 10000)
teste_ab
plot(teste_ab)
boot.ci(teste_ab, conf = .95)

# p-valor bootstrap
minimo <- min(sum(teste_ab$t<0),sum(teste_ab$t>0))
p.valor <- (2*minimo+1) / (teste_ab$R+1)
p.valor


#***************************************************

# Exemplo 18.5

# H0: a velocidade média da luz é 33.02 milionésimos de segundo
# H1: a velocidade média da luz não é 33.02 milionésimos de segundo

# Dados
velocidade <- c(28, 44, 29, 30, 26, 27, 22, 23, 33, 16, 24, 29, 24,
40 , 21, 31, 34, 2, 25, 19)

luz <- data.frame(velocidade)

# Histograma
luz %>%
  ggplot(aes(x = velocidade)) +
  geom_histogram(alpha = 0.3, fill = "darkblue") +
  labs(
    title = "Distribuição de velocidade registrada",
    subtitle = paste0("Media: ", round(mean(luz$velocidade),1), 
                      ". Desvio-padrão: ", round(sd(luz$velocidade), 2))
  ) +
  theme_light()

# Verificando a normalidade dos dados
shapiro.test(velocidade)
#qqplot
luz %>% ggplot(aes(sample = velocidade)) +
  stat_qq() +
  stat_qq_line()

# Como os dados não são normais
# vamos usar o teste bootstrap
media.b <- function(x, i){
  mean(x[i])
}

# 2000 réplicas bootstrap
medias.b <- boot(velocidade, statistic = media.b,
                 R = 2000)
plot(medias.b)

# Intervalo de confiança bootstrap
boot.ci(medias.b, conf = .95, type = "bca")

# p-valor bootstrap
minimo <- min(sum(medias.b$t < 33.02), sum(medias.b$t > 33.02))
p.valor <- (2*minimo+1) / (teste_ab$R+1)
p.valor
