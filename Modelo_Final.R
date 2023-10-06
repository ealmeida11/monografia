##### POLAND ####

# Pacotes

library(vars)
library(tidyverse)
library(plotly)
library(zoo)
library(glue)
library(lubridate)
library(readxl)
library(ggplot2)
library(gridExtra)
library(forecast)
library(MASS)
library(patchwork)
library(devtools)
library(svars)

### País

país <- "Poland"

# Carregar Arquivos

dados <- read_excel(path = "R:\\Economics\\Ealmeida\\Monografia\\Base de Dados2.xlsx", sheet = país)


# Informando o formato da data

dados$Data <- lubridate::ymd(dados$Data)


modelo_quebra <- breakpoints(Headline ~ 1, data = dados)

# select minimum BIC

dados.bp <- breakpoints(modelo_quebra)

# add segments
dados <- add_column(dados, regime = breakfactor(dados.bp))

# modelos
fm0 <- lm(Headline ~ 1, data = dados)
fm1 <- lm(Headline ~ regime, data = dados)

# add back model fit
dados <-
  add_column(dados, fit0 = fitted(fm0), fit1 = fitted(fm1))

p1_pln <- dados %>%
  ggplot() +
  geom_line(aes(x = Data, y = Headline)) +
  geom_line(aes(x = Data, y = fit0, group = 'sample'),
            color = 'blue',
            size = 1) +
  geom_line(aes(x = Data, y = fit1), color = 'red', size = 1) +
  geom_vline(xintercept = as.Date(dados[dados.bp$breakpoints, 'Data']$Data),
             lty = 'dashed') +
  geom_rect(
    data = dados,
    aes(
      xmin = min(Data),
      xmax = max(Data),
      ymin = min(Headline),
      ymax = max(Headline)
    ),
    alpha = 0.001
  ) +
  scale_x_date(breaks = scales::breaks_pretty(n = 15),
               labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 10)) +
  labs(title = paste(país, ": Headline Inflation"),  # Concatenar país com o título
       y = "% QoQ",
       x = NULL) +
  theme_bw() +  # Fundo branco
  theme(panel.background = element_rect(fill = "white"))

p1_pln

# Função para calcular Headline_ajustado
calcular_headline_ajustado <- function(dados) {
  dados <- dados %>%
    group_by(regime) %>%
    mutate(Headline_ajustado = Headline + (first(fit0) - first(fit1))) %>%
    ungroup()
  
  return(dados)
}

# Chame a função com seus dados
dados <- calcular_headline_ajustado(dados)


# Matriz de restrição

sign_restr1 <- matrix(c(
  1, 0, 0, 0,  # Restrições para a OutputGap 
  NA, 1, 0, 0,  # Restrições para a Inflation
  NA, NA, 1, 0,  # Restrições para a InterestRate
  NA, NA, NA, 1   # Restrições para a ExchangeRate
), nrow = 4, byrow = TRUE)


# Criando o VAR Estrutural


var_data_Estrutural <- dados %>% select(GDP , Headline, `Interest Rate`, `Exchange Rate`)

colnames(var_data_Estrutural) <- cbind( "OutputGap", "Inflation", "InterestRate","ExchangeRate")

Lagselection <- VARselect(var_data_Estrutural, lag.max = 8, type = "const")

Lagselection$selection

var_model_estrutural <- VAR(var_data_Estrutural, p = 4, type = "const")


# Estimando o SVAR e incluindo a matriz de restrição

SVARmodel <- SVAR(var_model_estrutural, Amat = sign_restr1, Bmat = NULL, hessian =TRUE, estmethod = c("direct"))

summary(SVARmodel)

#Choques do Câmbio nas variáveis sem ser cumulativo

Choque_Ex_out <- irf(SVARmodel, impulse = "ExchangeRate", response = "OutputGap", n.ahead = 18, cumulative = F, ci = 0.90, runs = 999)

Choque_Ex_inf <-irf(SVARmodel, impulse = "ExchangeRate", response = "Inflation", n.ahead = 18, cumulative = F,  ci = 0.90, runs = 999)

Choque_Ex_ir <-irf(SVARmodel, impulse = "ExchangeRate", response = "InterestRate", n.ahead = 18, cumulative = F, ci = 0.90, runs = 999)

Choque_Ex_Ex <-irf(SVARmodel, impulse = "ExchangeRate", response = "ExchangeRate", n.ahead = 18, cumulative = F, ci = 0.90, runs = 999)


# Criando uma função para retirar os dados do IRF

extrair_variaveis_df <- function(irf_objeto) {
  irf <- data.frame(irf_objeto$irf)
  Upper <- data.frame(irf_objeto$Upper)
  Lower <- data.frame(irf_objeto$Lower)
  
  resultado_final <- cbind(irf, Upper, Lower)
  colnames(resultado_final) <- cbind("irf", "Upper", "Lower")
  
  return(resultado_final)
}

# Extraindo as variáveis do IRF

Choque_Ex_out_df <- extrair_variaveis_df(Choque_Ex_out)
Choque_Ex_inf_df <- extrair_variaveis_df(Choque_Ex_inf)
Choque_Ex_ir_df <- extrair_variaveis_df(Choque_Ex_ir)
Choque_Ex_Ex_df <- extrair_variaveis_df(Choque_Ex_Ex)

# Combina os data frames e adiciona a coluna "Choque"
Choque_Ex_geral_df <- bind_rows(
  mutate(Choque_Ex_out_df, Choque = "Ex -> OutputGap"),
  mutate(Choque_Ex_inf_df, Choque = "Ex -> Inflação"),
  mutate(Choque_Ex_ir_df, Choque = "Ex -> InterestRate"),
  mutate(Choque_Ex_Ex_df, Choque = "Ex -> ExchangeRate")
)

# Adicionar a coluna Sequencia
Choque_Ex_geral_df <- Choque_Ex_geral_df %>%
  group_by(Choque) %>%
  mutate(Sequencia = row_number()) %>%
  ungroup()

# Criando os gráficos para os impulsos respostas


criar_grafico_irf_facet <- function(data_frame) {
  grafico <- ggplot(data = data_frame, aes(x = Sequencia, y = irf)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray70", alpha = 0.3) +
    geom_ribbon(aes(ymin = Upper, ymax = Lower), fill = "gray70", alpha = 0.3) +
    geom_line(color = "black", size = 1.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 0.8) +
    theme_bw() +
    labs(x = "Observation Time", y = "Response",
         title = paste(país, ": Impulse response of 1std dev of the exchange rate")) + # Adiciona o título com o país
    scale_x_continuous(breaks = seq(1, nrow(data_frame), by = 4)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(clip = 'off') +
    facet_wrap(~ Choque, scales = "free_y") # Adiciona a facet_wrap para diferenciar por fonte
  
  return(grafico)
}


# Gráfico_ExChoque

Gráfico_ExChoque_pln <- criar_grafico_irf_facet(Choque_Ex_geral_df)

#Choques do Câmbio nas variáveis cumulativo

Choque_Ex_out_cum <- irf(SVARmodel, impulse = "ExchangeRate", response = "OutputGap", n.ahead = 18, cumulative = T, ci = 0.90, runs = 999)

Choque_Ex_inf_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "Inflation", n.ahead = 18, cumulative = T,  ci = 0.90, runs = 999)

Choque_Ex_ir_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "InterestRate", n.ahead = 18, cumulative = T, ci = 0.90, runs = 999)

Choque_Ex_Ex_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "ExchangeRate", n.ahead = 18, cumulative = T, ci = 0.90, runs = 999)

# Extraindo as variáveis do IRF

Choque_Ex_out_df_cum <- extrair_variaveis_df(Choque_Ex_out_cum)
Choque_Ex_inf_df_cum <- extrair_variaveis_df(Choque_Ex_inf_cum)
Choque_Ex_ir_df_cum <- extrair_variaveis_df(Choque_Ex_ir_cum)
Choque_Ex_Ex_df_cum <- extrair_variaveis_df(Choque_Ex_Ex_cum)

# Combina os data frames e adiciona a coluna "Fonte"
Choque_Ex_geral_df_cum <- bind_rows(
  mutate(Choque_Ex_out_df_cum, Choque = "Ex -> OutputGap"),
  mutate(Choque_Ex_inf_df_cum, Choque = "Ex -> Inflação"),
  mutate(Choque_Ex_ir_df_cum, Choque = "Ex -> InterestRate"),
  mutate(Choque_Ex_Ex_df_cum, Choque = "Ex -> ExchangeRate")
)

# Adicionar a coluna Sequencia
Choque_Ex_geral_df_cum <- Choque_Ex_geral_df_cum %>%
  group_by(Choque) %>%
  mutate(Sequencia = row_number()) %>%
  ungroup()


# Gráfico_ExChoque

criar_grafico_irf_facet2 <- function(data_frame) {
  grafico <- ggplot(data = data_frame, aes(x = Sequencia, y = irf)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray70", alpha = 0.3) +
    geom_ribbon(aes(ymin = Upper, ymax = Lower), fill = "gray70", alpha = 0.3) +
    geom_line(color = "black", size = 1.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 0.8) +
    theme_bw() +
    labs(x = "Observation Time", y = "Response",
         title = paste(país, ": Cumulative impulse response of 1std dev of the exchange rate")) + # Adiciona o título com o país
    scale_x_continuous(breaks = seq(1, nrow(data_frame), by = 4)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(clip = 'off') +
    facet_wrap(~ Choque, scales = "free_y") # Adiciona a facet_wrap para diferenciar por fonte
  
  return(grafico)
}

Gráfico_ExChoque_pln_cum <- criar_grafico_irf_facet2(Choque_Ex_geral_df_cum)


# Salvar os gráficos

ggsave(filename = paste(país, "nível_infla.png"), plot = p1_pln)
ggsave(filename = paste(país, "Choque.png"), plot = Gráfico_ExChoque_pln)
ggsave(filename = paste(país, "Choque_acumulado.png"), plot = Gráfico_ExChoque_pln_cum)




##### Czech Republic  ####

# Pacotes

library(vars)
library(tidyverse)
library(plotly)
library(zoo)
library(glue)
library(lubridate)
library(readxl)
library(ggplot2)
library(gridExtra)
library(forecast)
library(MASS)
library(patchwork)
library(devtools)
library(svars)

### País

país <- "Czech Republic"

# Carregar Arquivos

dados <- read_excel(path = "R:\\Economics\\Ealmeida\\Monografia\\Base de Dados2.xlsx", sheet = país)


# Informando o formato da data

dados$Data <- lubridate::ymd(dados$Data)


modelo_quebra <- breakpoints(Headline ~ 1, data = dados)

# select minimum BIC

dados.bp <- breakpoints(modelo_quebra)

# add segments
dados <- add_column(dados, regime = breakfactor(dados.bp))

# modelos
fm0 <- lm(Headline ~ 1, data = dados)
fm1 <- lm(Headline ~ regime, data = dados)

# add back model fit
dados <-
  add_column(dados, fit0 = fitted(fm0), fit1 = fitted(fm1))

p1_czk <- dados %>%
  ggplot() +
  geom_line(aes(x = Data, y = Headline)) +
  geom_line(aes(x = Data, y = fit0, group = 'sample'),
            color = 'blue',
            size = 1) +
  geom_line(aes(x = Data, y = fit1), color = 'red', size = 1) +
  geom_vline(xintercept = as.Date(dados[dados.bp$breakpoints, 'Data']$Data),
             lty = 'dashed') +
  geom_rect(
    data = dados,
    aes(
      xmin = min(Data),
      xmax = max(Data),
      ymin = min(Headline),
      ymax = max(Headline)
    ),
    alpha = 0.001
  ) +
  scale_x_date(breaks = scales::breaks_pretty(n = 15),
               labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 10)) +
  labs(title = paste(país, ": Headline Inflation"),  # Concatenar país com o título
       y = "% QoQ",
       x = NULL) +
  theme_bw() +  # Fundo branco
  theme(panel.background = element_rect(fill = "white"))

p1_czk

# Função para calcular Headline_ajustado
calcular_headline_ajustado <- function(dados) {
  dados <- dados %>%
    group_by(regime) %>%
    mutate(Headline_ajustado = Headline + (first(fit0) - first(fit1))) %>%
    ungroup()
  
  return(dados)
}

# Chame a função com seus dados
dados <- calcular_headline_ajustado(dados)


# Matriz de restrição

sign_restr1 <- matrix(c(
  1, 0, 0, 0,  # Restrições para a OutputGap 
  NA, 1, 0, 0,  # Restrições para a Inflation
  NA, NA, 1, 0,  # Restrições para a InterestRate
  NA, NA, NA, 1   # Restrições para a ExchangeRate
), nrow = 4, byrow = TRUE)


# Criando o VAR Estrutural


var_data_Estrutural <- dados %>% select(GDP , Headline, `Interest Rate`, `Exchange Rate`)

colnames(var_data_Estrutural) <- cbind( "OutputGap", "Inflation", "InterestRate","ExchangeRate")

Lagselection <- VARselect(var_data_Estrutural, lag.max = 8, type = "const")

Lagselection$selection

var_model_estrutural <- VAR(var_data_Estrutural, p = 4, type = "const")


# Estimando o SVAR e incluindo a matriz de restrição

SVARmodel <- SVAR(var_model_estrutural, Amat = sign_restr1, Bmat = NULL, hessian =TRUE, estmethod = c("direct"))

summary(SVARmodel)

#Choques do Câmbio nas variáveis sem ser cumulativo

Choque_Ex_out <- irf(SVARmodel, impulse = "ExchangeRate", response = "OutputGap", n.ahead = 18, cumulative = F, ci = 0.90, runs = 999)

Choque_Ex_inf <-irf(SVARmodel, impulse = "ExchangeRate", response = "Inflation", n.ahead = 18, cumulative = F,  ci = 0.90, runs = 999)

Choque_Ex_ir <-irf(SVARmodel, impulse = "ExchangeRate", response = "InterestRate", n.ahead = 18, cumulative = F, ci = 0.90, runs = 999)

Choque_Ex_Ex <-irf(SVARmodel, impulse = "ExchangeRate", response = "ExchangeRate", n.ahead = 18, cumulative = F, ci = 0.90, runs = 999)


# Criando uma função para retirar os dados do IRF

extrair_variaveis_df <- function(irf_objeto) {
  irf <- data.frame(irf_objeto$irf)
  Upper <- data.frame(irf_objeto$Upper)
  Lower <- data.frame(irf_objeto$Lower)
  
  resultado_final <- cbind(irf, Upper, Lower)
  colnames(resultado_final) <- cbind("irf", "Upper", "Lower")
  
  return(resultado_final)
}

# Extraindo as variáveis do IRF

Choque_Ex_out_df <- extrair_variaveis_df(Choque_Ex_out)
Choque_Ex_inf_df <- extrair_variaveis_df(Choque_Ex_inf)
Choque_Ex_ir_df <- extrair_variaveis_df(Choque_Ex_ir)
Choque_Ex_Ex_df <- extrair_variaveis_df(Choque_Ex_Ex)

# Combina os data frames e adiciona a coluna "Choque"
Choque_Ex_geral_df <- bind_rows(
  mutate(Choque_Ex_out_df, Choque = "Ex -> OutputGap"),
  mutate(Choque_Ex_inf_df, Choque = "Ex -> Inflação"),
  mutate(Choque_Ex_ir_df, Choque = "Ex -> InterestRate"),
  mutate(Choque_Ex_Ex_df, Choque = "Ex -> ExchangeRate")
)

# Adicionar a coluna Sequencia
Choque_Ex_geral_df <- Choque_Ex_geral_df %>%
  group_by(Choque) %>%
  mutate(Sequencia = row_number()) %>%
  ungroup()

# Criando os gráficos para os impulsos respostas


criar_grafico_irf_facet <- function(data_frame) {
  grafico <- ggplot(data = data_frame, aes(x = Sequencia, y = irf)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray70", alpha = 0.3) +
    geom_ribbon(aes(ymin = Upper, ymax = Lower), fill = "gray70", alpha = 0.3) +
    geom_line(color = "black", size = 1.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 0.8) +
    theme_bw() +
    labs(x = "Observation Time", y = "Response",
         title = paste(país, ": Impulse response of 1std dev of the exchange rate")) + # Adiciona o título com o país
    scale_x_continuous(breaks = seq(1, nrow(data_frame), by = 4)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(clip = 'off') +
    facet_wrap(~ Choque, scales = "free_y") # Adiciona a facet_wrap para diferenciar por fonte
  
  return(grafico)
}


# Gráfico_ExChoque

Gráfico_ExChoque_czk <- criar_grafico_irf_facet(Choque_Ex_geral_df)

#Choques do Câmbio nas variáveis cumulativo

Choque_Ex_out_cum <- irf(SVARmodel, impulse = "ExchangeRate", response = "OutputGap", n.ahead = 18, cumulative = T, ci = 0.90, runs = 999)

Choque_Ex_inf_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "Inflation", n.ahead = 18, cumulative = T,  ci = 0.90, runs = 999)

Choque_Ex_ir_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "InterestRate", n.ahead = 18, cumulative = T, ci = 0.90, runs = 999)

Choque_Ex_Ex_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "ExchangeRate", n.ahead = 18, cumulative = T, ci = 0.90, runs = 999)

# Extraindo as variáveis do IRF

Choque_Ex_out_df_cum <- extrair_variaveis_df(Choque_Ex_out_cum)
Choque_Ex_inf_df_cum <- extrair_variaveis_df(Choque_Ex_inf_cum)
Choque_Ex_ir_df_cum <- extrair_variaveis_df(Choque_Ex_ir_cum)
Choque_Ex_Ex_df_cum <- extrair_variaveis_df(Choque_Ex_Ex_cum)

# Combina os data frames e adiciona a coluna "Fonte"
Choque_Ex_geral_df_cum <- bind_rows(
  mutate(Choque_Ex_out_df_cum, Choque = "Ex -> OutputGap"),
  mutate(Choque_Ex_inf_df_cum, Choque = "Ex -> Inflação"),
  mutate(Choque_Ex_ir_df_cum, Choque = "Ex -> InterestRate"),
  mutate(Choque_Ex_Ex_df_cum, Choque = "Ex -> ExchangeRate")
)

# Adicionar a coluna Sequencia
Choque_Ex_geral_df_cum <- Choque_Ex_geral_df_cum %>%
  group_by(Choque) %>%
  mutate(Sequencia = row_number()) %>%
  ungroup()


# Gráfico_ExChoque

criar_grafico_irf_facet2 <- function(data_frame) {
  grafico <- ggplot(data = data_frame, aes(x = Sequencia, y = irf)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray70", alpha = 0.3) +
    geom_ribbon(aes(ymin = Upper, ymax = Lower), fill = "gray70", alpha = 0.3) +
    geom_line(color = "black", size = 1.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 0.8) +
    theme_bw() +
    labs(x = "Observation Time", y = "Response",
         title = paste(país, ": Cumulative impulse response of 1std dev of the exchange rate")) + # Adiciona o título com o país
    scale_x_continuous(breaks = seq(1, nrow(data_frame), by = 4)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(clip = 'off') +
    facet_wrap(~ Choque, scales = "free_y") # Adiciona a facet_wrap para diferenciar por fonte
  
  return(grafico)
}

Gráfico_ExChoque_czk_cum <- criar_grafico_irf_facet2(Choque_Ex_geral_df_cum)

ggsave(filename = paste(país, "nível_infla.png"), plot = p1_czk)
ggsave(filename = paste(país, "Choque.png"), plot = Gráfico_ExChoque_czk)
ggsave(filename = paste(país, "Choque_acumulado.png"), plot = Gráfico_ExChoque_czk_cum)

##### Brasil ####

# Pacotes

library(vars)
library(tidyverse)
library(plotly)
library(zoo)
library(glue)
library(lubridate)
library(readxl)
library(ggplot2)
library(gridExtra)
library(forecast)
library(MASS)
library(patchwork)
library(devtools)
library(svars)

### País

país <- "Brasil"

# Carregar Arquivos

dados <- read_excel(path = "R:\\Economics\\Ealmeida\\Monografia\\Base de Dados2.xlsx", sheet = país)


# Informando o formato da data

dados$Data <- lubridate::ymd(dados$Data)


modelo_quebra <- breakpoints(Headline ~ 1, data = dados)

# select minimum BIC

dados.bp <- breakpoints(modelo_quebra)

# add segments
dados <- add_column(dados, regime = breakfactor(dados.bp))

# modelos
fm0 <- lm(Headline ~ 1, data = dados)
fm1 <- lm(Headline ~ regime, data = dados)

# add back model fit
dados <-
  add_column(dados, fit0 = fitted(fm0), fit1 = fitted(fm1))

p1_brl <- dados %>%
  ggplot() +
  geom_line(aes(x = Data, y = Headline)) +
  geom_line(aes(x = Data, y = fit0, group = 'sample'),
            color = 'blue',
            size = 1) +
  geom_line(aes(x = Data, y = fit1), color = 'red', size = 1) +
  geom_vline(xintercept = as.Date(dados[dados.bp$breakpoints, 'Data']$Data),
             lty = 'dashed') +
  geom_rect(
    data = dados,
    aes(
      xmin = min(Data),
      xmax = max(Data),
      ymin = min(Headline),
      ymax = max(Headline)
    ),
    alpha = 0.001
  ) +
  scale_x_date(breaks = scales::breaks_pretty(n = 15),
               labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 10)) +
  labs(title = paste(país, ": Headline Inflation"),  # Concatenar país com o título
       y = "% QoQ",
       x = NULL) +
  theme_bw() +  # Fundo branco
  theme(panel.background = element_rect(fill = "white"))

p1_brl

# Função para calcular Headline_ajustado
calcular_headline_ajustado <- function(dados) {
  dados <- dados %>%
    group_by(regime) %>%
    mutate(Headline_ajustado = Headline + (first(fit0) - first(fit1))) %>%
    ungroup()
  
  return(dados)
}

# Chame a função com seus dados
dados <- calcular_headline_ajustado(dados)


# Matriz de restrição

sign_restr1 <- matrix(c(
  1, 0, 0, 0,  # Restrições para a OutputGap 
  NA, 1, 0, 0,  # Restrições para a Inflation
  NA, NA, 1, 0,  # Restrições para a InterestRate
  NA, NA, NA, 1   # Restrições para a ExchangeRate
), nrow = 4, byrow = TRUE)


# Criando o VAR Estrutural


var_data_Estrutural <- dados %>% select(GDP , Headline, `Interest Rate`, `Exchange Rate`)

colnames(var_data_Estrutural) <- cbind( "OutputGap", "Inflation", "InterestRate","ExchangeRate")

Lagselection <- VARselect(var_data_Estrutural, lag.max = 8, type = "const")

Lagselection$selection

var_model_estrutural <- VAR(var_data_Estrutural, p = 4, type = "const")


# Estimando o SVAR e incluindo a matriz de restrição

SVARmodel <- SVAR(var_model_estrutural, Amat = sign_restr1, Bmat = NULL, hessian =TRUE, estmethod = c("direct"))

summary(SVARmodel)

#Choques do Câmbio nas variáveis sem ser cumulativo

Choque_Ex_out <- irf(SVARmodel, impulse = "ExchangeRate", response = "OutputGap", n.ahead = 18, cumulative = F, ci = 0.90, runs = 999)

Choque_Ex_inf <-irf(SVARmodel, impulse = "ExchangeRate", response = "Inflation", n.ahead = 18, cumulative = F,  ci = 0.90, runs = 999)

Choque_Ex_ir <-irf(SVARmodel, impulse = "ExchangeRate", response = "InterestRate", n.ahead = 18, cumulative = F, ci = 0.90, runs = 999)

Choque_Ex_Ex <-irf(SVARmodel, impulse = "ExchangeRate", response = "ExchangeRate", n.ahead = 18, cumulative = F, ci = 0.90, runs = 999)


# Criando uma função para retirar os dados do IRF

extrair_variaveis_df <- function(irf_objeto) {
  irf <- data.frame(irf_objeto$irf)
  Upper <- data.frame(irf_objeto$Upper)
  Lower <- data.frame(irf_objeto$Lower)
  
  resultado_final <- cbind(irf, Upper, Lower)
  colnames(resultado_final) <- cbind("irf", "Upper", "Lower")
  
  return(resultado_final)
}

# Extraindo as variáveis do IRF

Choque_Ex_out_df <- extrair_variaveis_df(Choque_Ex_out)
Choque_Ex_inf_df <- extrair_variaveis_df(Choque_Ex_inf)
Choque_Ex_ir_df <- extrair_variaveis_df(Choque_Ex_ir)
Choque_Ex_Ex_df <- extrair_variaveis_df(Choque_Ex_Ex)

# Combina os data frames e adiciona a coluna "Choque"
Choque_Ex_geral_df <- bind_rows(
  mutate(Choque_Ex_out_df, Choque = "Ex -> OutputGap"),
  mutate(Choque_Ex_inf_df, Choque = "Ex -> Inflação"),
  mutate(Choque_Ex_ir_df, Choque = "Ex -> InterestRate"),
  mutate(Choque_Ex_Ex_df, Choque = "Ex -> ExchangeRate")
)

# Adicionar a coluna Sequencia
Choque_Ex_geral_df <- Choque_Ex_geral_df %>%
  group_by(Choque) %>%
  mutate(Sequencia = row_number()) %>%
  ungroup()

# Criando os gráficos para os impulsos respostas


criar_grafico_irf_facet <- function(data_frame) {
  grafico <- ggplot(data = data_frame, aes(x = Sequencia, y = irf)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray70", alpha = 0.3) +
    geom_ribbon(aes(ymin = Upper, ymax = Lower), fill = "gray70", alpha = 0.3) +
    geom_line(color = "black", size = 1.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 0.8) +
    theme_bw() +
    labs(x = "Observation Time", y = "Response",
         title = paste(país, ": Impulse response of 1std dev of the exchange rate")) + # Adiciona o título com o país
    scale_x_continuous(breaks = seq(1, nrow(data_frame), by = 4)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(clip = 'off') +
    facet_wrap(~ Choque, scales = "free_y") # Adiciona a facet_wrap para diferenciar por fonte
  
  return(grafico)
}


# Gráfico_ExChoque

Gráfico_ExChoque_brl <- criar_grafico_irf_facet(Choque_Ex_geral_df)

#Choques do Câmbio nas variáveis cumulativo

Choque_Ex_out_cum <- irf(SVARmodel, impulse = "ExchangeRate", response = "OutputGap", n.ahead = 18, cumulative = T, ci = 0.90, runs = 999)

Choque_Ex_inf_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "Inflation", n.ahead = 18, cumulative = T,  ci = 0.90, runs = 999)

Choque_Ex_ir_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "InterestRate", n.ahead = 18, cumulative = T, ci = 0.90, runs = 999)

Choque_Ex_Ex_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "ExchangeRate", n.ahead = 18, cumulative = T, ci = 0.90, runs = 999)

# Extraindo as variáveis do IRF

Choque_Ex_out_df_cum <- extrair_variaveis_df(Choque_Ex_out_cum)
Choque_Ex_inf_df_cum <- extrair_variaveis_df(Choque_Ex_inf_cum)
Choque_Ex_ir_df_cum <- extrair_variaveis_df(Choque_Ex_ir_cum)
Choque_Ex_Ex_df_cum <- extrair_variaveis_df(Choque_Ex_Ex_cum)

# Combina os data frames e adiciona a coluna "Fonte"
Choque_Ex_geral_df_cum <- bind_rows(
  mutate(Choque_Ex_out_df_cum, Choque = "Ex -> OutputGap"),
  mutate(Choque_Ex_inf_df_cum, Choque = "Ex -> Inflação"),
  mutate(Choque_Ex_ir_df_cum, Choque = "Ex -> InterestRate"),
  mutate(Choque_Ex_Ex_df_cum, Choque = "Ex -> ExchangeRate")
)

# Adicionar a coluna Sequencia
Choque_Ex_geral_df_cum <- Choque_Ex_geral_df_cum %>%
  group_by(Choque) %>%
  mutate(Sequencia = row_number()) %>%
  ungroup()


# Gráfico_ExChoque

criar_grafico_irf_facet2 <- function(data_frame) {
  grafico <- ggplot(data = data_frame, aes(x = Sequencia, y = irf)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray70", alpha = 0.3) +
    geom_ribbon(aes(ymin = Upper, ymax = Lower), fill = "gray70", alpha = 0.3) +
    geom_line(color = "black", size = 1.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 0.8) +
    theme_bw() +
    labs(x = "Observation Time", y = "Response",
         title = paste(país, ": Cumulative impulse response of 1std dev of the exchange rate")) + # Adiciona o título com o país
    scale_x_continuous(breaks = seq(1, nrow(data_frame), by = 4)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(clip = 'off') +
    facet_wrap(~ Choque, scales = "free_y") # Adiciona a facet_wrap para diferenciar por fonte
  
  return(grafico)
}

Gráfico_ExChoque_brl_cum <- criar_grafico_irf_facet2(Choque_Ex_geral_df_cum)

ggsave(filename = paste(país, "nível_infla.png"), plot = p1_brl)
ggsave(filename = paste(país, "Choque.png"), plot = Gráfico_ExChoque_brl)
ggsave(filename = paste(país, "Choque_acumulado.png"), plot = Gráfico_ExChoque_brl_cum)

#### Chile ####

# Pacotes

library(vars)
library(tidyverse)
library(plotly)
library(zoo)
library(glue)
library(lubridate)
library(readxl)
library(ggplot2)
library(gridExtra)
library(forecast)
library(MASS)
library(patchwork)
library(devtools)
library(svars)

### País

país <- "Chile"

# Carregar Arquivos

dados <- read_excel(path = "R:\\Economics\\Ealmeida\\Monografia\\Base de Dados2.xlsx", sheet = país)


# Informando o formato da data

dados$Data <- lubridate::ymd(dados$Data)

dados <- dados %>% filter(Data > "2012-01-01" & Data < "2019-04-01")

dados <- dados %>% mutate(Headline_ajustado = Headline)

# modelo_quebra <- breakpoints(Headline ~ 1, data = dados)
# 
# # select minimum BIC
# 
# dados.bp <- breakpoints(modelo_quebra)
# 
# # add segments
# dados <- add_column(dados, regime = breakfactor(dados.bp))
# 
# 
# # modelos
# fm0 <- lm(Headline ~ 1, data = dados)
# fm1 <- lm(Headline ~ regime, data = dados)
# 
# # add back model fit
# dados <-
#   add_column(dados, fit0 = fitted(fm0), fit1 = fitted(fm1))
# 
# p1_clp <- dados %>%
#   ggplot() +
#   geom_line(aes(x = Data, y = Headline)) +
#   geom_line(aes(x = Data, y = fit0, group = 'sample'),
#             color = 'blue',
#             size = 1) +
#   geom_line(aes(x = Data, y = fit1), color = 'red', size = 1) +
#   geom_vline(xintercept = as.Date(dados[dados.bp$breakpoints, 'Data']$Data),
#              lty = 'dashed') +
#   geom_rect(
#     data = dados,
#     aes(
#       xmin = min(Data),
#       xmax = max(Data),
#       ymin = min(Headline),
#       ymax = max(Headline)
#     ),
#     alpha = 0.001
#   ) +
#   scale_x_date(breaks = scales::breaks_pretty(n = 15),
#                labels = scales::label_date_short()) +
#   scale_y_continuous(breaks = scales::breaks_extended(n = 10)) +
#   labs(title = paste(país, ": Headline Inflation"),  # Concatenar país com o título
#        y = "% QoQ",
#        x = NULL) +
#   theme_bw() +  # Fundo branco
#   theme(panel.background = element_rect(fill = "white"))
# 
# p1_clp
# 
# # Função para calcular Headline_ajustado
# calcular_headline_ajustado <- function(dados) {
#   dados <- dados %>%
#     group_by(regime) %>%
#     mutate(Headline_ajustado = Headline + (first(fit0) - first(fit1))) %>%
#     ungroup()
# 
#   return(dados)
# }
# 
# # Chame a função com seus dados
# dados <- calcular_headline_ajustado(dados)


# Matriz de restrição

sign_restr1 <- matrix(c(
  1, 0, 0, 0,  # Restrições para a OutputGap 
  NA, 1, 0, 0,  # Restrições para a Inflation
  NA, NA, 1, 0,  # Restrições para a InterestRate
  NA, NA, NA, 1   # Restrições para a ExchangeRate
), nrow = 4, byrow = TRUE)


# Criando o VAR Estrutural


var_data_Estrutural <- dados %>% select(GDP , Headline, `Interest Rate`, `Exchange Rate`)

colnames(var_data_Estrutural) <- cbind( "OutputGap", "Inflation", "InterestRate","ExchangeRate")

Lagselection <- VARselect(var_data_Estrutural, lag.max = 8, type = "const")

Lagselection$selection

var_model_estrutural <- VAR(var_data_Estrutural, p = 4, type = "const")


# Estimando o SVAR e incluindo a matriz de restrição

SVARmodel <- SVAR(var_model_estrutural, Amat = sign_restr1, Bmat = NULL, hessian =TRUE, estmethod = c("direct"))

summary(SVARmodel)


#Choques do Câmbio nas variáveis sem ser cumulativo

Choque_Ex_out <- irf(SVARmodel, impulse = "ExchangeRate", response = "OutputGap", n.ahead = 18, cumulative = F, ci = 0.90, runs = 100)

Choque_Ex_inf <-irf(SVARmodel, impulse = "ExchangeRate", response = "Inflation", n.ahead = 18, cumulative = F,  ci = 0.90, runs = 100)

Choque_Ex_ir <-irf(SVARmodel, impulse = "ExchangeRate", response = "InterestRate", n.ahead = 18, cumulative = F, ci = 0.90, runs = 100)

Choque_Ex_Ex <-irf(SVARmodel, impulse = "ExchangeRate", response = "ExchangeRate", n.ahead = 18, cumulative = F, ci = 0.90, runs = 100)


# Criando uma função para retirar os dados do IRF

extrair_variaveis_df <- function(irf_objeto) {
  irf <- data.frame(irf_objeto$irf)
  Upper <- data.frame(irf_objeto$Upper)
  Lower <- data.frame(irf_objeto$Lower)
  
  resultado_final <- cbind(irf, Upper, Lower)
  colnames(resultado_final) <- cbind("irf", "Upper", "Lower")
  
  return(resultado_final)
}

# Extraindo as variáveis do IRF

Choque_Ex_out_df <- extrair_variaveis_df(Choque_Ex_out)
Choque_Ex_inf_df <- extrair_variaveis_df(Choque_Ex_inf)
Choque_Ex_ir_df <- extrair_variaveis_df(Choque_Ex_ir)
Choque_Ex_Ex_df <- extrair_variaveis_df(Choque_Ex_Ex)

# Combina os data frames e adiciona a coluna "Choque"
Choque_Ex_geral_df <- bind_rows(
  mutate(Choque_Ex_out_df, Choque = "Ex -> OutputGap"),
  mutate(Choque_Ex_inf_df, Choque = "Ex -> Inflação"),
  mutate(Choque_Ex_ir_df, Choque = "Ex -> InterestRate"),
  mutate(Choque_Ex_Ex_df, Choque = "Ex -> ExchangeRate")
)

# Adicionar a coluna Sequencia
Choque_Ex_geral_df <- Choque_Ex_geral_df %>%
  group_by(Choque) %>%
  mutate(Sequencia = row_number()) %>%
  ungroup()

# Criando os gráficos para os impulsos respostas


criar_grafico_irf_facet <- function(data_frame) {
  grafico <- ggplot(data = data_frame, aes(x = Sequencia, y = irf)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray70", alpha = 0.3) +
    geom_ribbon(aes(ymin = Upper, ymax = Lower), fill = "gray70", alpha = 0.3) +
    geom_line(color = "black", size = 1.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 0.8) +
    theme_bw() +
    labs(x = "Observation Time", y = "Response",
         title = paste(país, ": Impulse response of 1std dev of the exchange rate")) + # Adiciona o título com o país
    scale_x_continuous(breaks = seq(1, nrow(data_frame), by = 4)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(clip = 'off') +
    facet_wrap(~ Choque, scales = "free_y") # Adiciona a facet_wrap para diferenciar por fonte
  
  return(grafico)
}


# Gráfico_ExChoque

Gráfico_ExChoque_clp <- criar_grafico_irf_facet(Choque_Ex_geral_df)

#Choques do Câmbio nas variáveis cumulativo

Choque_Ex_out_cum <- irf(SVARmodel, impulse = "ExchangeRate", response = "OutputGap", n.ahead = 18, cumulative = T, ci = 0.90, runs = 100)

Choque_Ex_inf_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "Inflation", n.ahead = 18, cumulative = T,  ci = 0.90, runs = 100)

Choque_Ex_ir_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "InterestRate", n.ahead = 18, cumulative = T, ci = 0.90, runs = 100)

Choque_Ex_Ex_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "ExchangeRate", n.ahead = 18, cumulative = T, ci = 0.90, runs = 100)

# Extraindo as variáveis do IRF

Choque_Ex_out_df_cum <- extrair_variaveis_df(Choque_Ex_out_cum)
Choque_Ex_inf_df_cum <- extrair_variaveis_df(Choque_Ex_inf_cum)
Choque_Ex_ir_df_cum <- extrair_variaveis_df(Choque_Ex_ir_cum)
Choque_Ex_Ex_df_cum <- extrair_variaveis_df(Choque_Ex_Ex_cum)

# Combina os data frames e adiciona a coluna "Fonte"
Choque_Ex_geral_df_cum <- bind_rows(
  mutate(Choque_Ex_out_df_cum, Choque = "Ex -> OutputGap"),
  mutate(Choque_Ex_inf_df_cum, Choque = "Ex -> Inflação"),
  mutate(Choque_Ex_ir_df_cum, Choque = "Ex -> InterestRate"),
  mutate(Choque_Ex_Ex_df_cum, Choque = "Ex -> ExchangeRate")
)

# Adicionar a coluna Sequencia
Choque_Ex_geral_df_cum <- Choque_Ex_geral_df_cum %>%
  group_by(Choque) %>%
  mutate(Sequencia = row_number()) %>%
  ungroup()


# Gráfico_ExChoque

criar_grafico_irf_facet2 <- function(data_frame) {
  grafico <- ggplot(data = data_frame, aes(x = Sequencia, y = irf)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray70", alpha = 0.3) +
    geom_ribbon(aes(ymin = Upper, ymax = Lower), fill = "gray70", alpha = 0.3) +
    geom_line(color = "black", size = 1.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 0.8) +
    theme_bw() +
    labs(x = "Observation Time", y = "Response",
         title = paste(país, ": Cumulative impulse response of 1std dev of the exchange rate")) + # Adiciona o título com o país
    scale_x_continuous(breaks = seq(1, nrow(data_frame), by = 4)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(clip = 'off') +
    facet_wrap(~ Choque, scales = "free_y") # Adiciona a facet_wrap para diferenciar por fonte
  
  return(grafico)
}

Gráfico_ExChoque_clp_cum <- criar_grafico_irf_facet2(Choque_Ex_geral_df_cum)

ggsave(filename = paste(país, "nível_infla.png"), plot = p1_clp)
ggsave(filename = paste(país, "Choque.png"), plot = Gráfico_ExChoque_clp)
ggsave(filename = paste(país, "Choque_acumulado.png"), plot = Gráfico_ExChoque_clp_cum)

#### South Africa ####

# Pacotes

library(vars)
library(tidyverse)
library(plotly)
library(zoo)
library(glue)
library(lubridate)
library(readxl)
library(ggplot2)
library(gridExtra)
library(forecast)
library(MASS)
library(patchwork)
library(devtools)
library(svars)

### País

país <- "South Africa"

# Carregar Arquivos

dados <- read_excel(path = "R:\\Economics\\Ealmeida\\Monografia\\Base de Dados2.xlsx", sheet = país)


# Informando o formato da data

dados$Data <- lubridate::ymd(dados$Data)


modelo_quebra <- breakpoints(Headline ~ 1, data = dados)

# select minimum BIC

dados.bp <- breakpoints(modelo_quebra)

# add segments
dados <- add_column(dados, regime = breakfactor(dados.bp))

# modelos
fm0 <- lm(Headline ~ 1, data = dados)
fm1 <- lm(Headline ~ regime, data = dados)

# add back model fit
dados <-
  add_column(dados, fit0 = fitted(fm0), fit1 = fitted(fm1))

p1_zar <- dados %>%
  ggplot() +
  geom_line(aes(x = Data, y = Headline)) +
  geom_line(aes(x = Data, y = fit0, group = 'sample'),
            color = 'blue',
            size = 1) +
  geom_line(aes(x = Data, y = fit1), color = 'red', size = 1) +
  geom_vline(xintercept = as.Date(dados[dados.bp$breakpoints, 'Data']$Data),
             lty = 'dashed') +
  geom_rect(
    data = dados,
    aes(
      xmin = min(Data),
      xmax = max(Data),
      ymin = min(Headline),
      ymax = max(Headline)
    ),
    alpha = 0.001
  ) +
  scale_x_date(breaks = scales::breaks_pretty(n = 15),
               labels = scales::label_date_short()) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 10)) +
  labs(title = paste(país, ": Headline Inflation"),  # Concatenar país com o título
       y = "% QoQ",
       x = NULL) +
  theme_bw() +  # Fundo branco
  theme(panel.background = element_rect(fill = "white"))

p1_zar

# Função para calcular Headline_ajustado
calcular_headline_ajustado <- function(dados) {
  dados <- dados %>%
    group_by(regime) %>%
    mutate(Headline_ajustado = Headline + (first(fit0) - first(fit1))) %>%
    ungroup()
  
  return(dados)
}

# Chame a função com seus dados
dados <- calcular_headline_ajustado(dados)


# Matriz de restrição

sign_restr1 <- matrix(c(
  1, 0, 0, 0,  # Restrições para a OutputGap 
  NA, 1, 0, 0,  # Restrições para a Inflation
  NA, NA, 1, 0,  # Restrições para a InterestRate
  NA, NA, NA, 1   # Restrições para a ExchangeRate
), nrow = 4, byrow = TRUE)


# Criando o VAR Estrutural


var_data_Estrutural <- dados %>% select(GDP , Headline, `Interest Rate`, `Exchange Rate`)

colnames(var_data_Estrutural) <- cbind( "OutputGap", "Inflation", "InterestRate","ExchangeRate")

Lagselection <- VARselect(var_data_Estrutural, lag.max = 8, type = "const")

Lagselection$selection

var_model_estrutural <- VAR(var_data_Estrutural, p = 4, type = "const")


# Estimando o SVAR e incluindo a matriz de restrição

SVARmodel <- SVAR(var_model_estrutural, Amat = sign_restr1, Bmat = NULL, hessian =TRUE, estmethod = c("direct"))

summary(SVARmodel)

#Choques do Câmbio nas variáveis sem ser cumulativo

Choque_Ex_out <- irf(SVARmodel, impulse = "ExchangeRate", response = "OutputGap", n.ahead = 18, cumulative = F, ci = 0.90, runs = 999)

Choque_Ex_inf <-irf(SVARmodel, impulse = "ExchangeRate", response = "Inflation", n.ahead = 18, cumulative = F,  ci = 0.90, runs = 999)

Choque_Ex_ir <-irf(SVARmodel, impulse = "ExchangeRate", response = "InterestRate", n.ahead = 18, cumulative = F, ci = 0.90, runs = 999)

Choque_Ex_Ex <-irf(SVARmodel, impulse = "ExchangeRate", response = "ExchangeRate", n.ahead = 18, cumulative = F, ci = 0.90, runs = 999)


# Criando uma função para retirar os dados do IRF

extrair_variaveis_df <- function(irf_objeto) {
  irf <- data.frame(irf_objeto$irf)
  Upper <- data.frame(irf_objeto$Upper)
  Lower <- data.frame(irf_objeto$Lower)
  
  resultado_final <- cbind(irf, Upper, Lower)
  colnames(resultado_final) <- cbind("irf", "Upper", "Lower")
  
  return(resultado_final)
}

# Extraindo as variáveis do IRF

Choque_Ex_out_df <- extrair_variaveis_df(Choque_Ex_out)
Choque_Ex_inf_df <- extrair_variaveis_df(Choque_Ex_inf)
Choque_Ex_ir_df <- extrair_variaveis_df(Choque_Ex_ir)
Choque_Ex_Ex_df <- extrair_variaveis_df(Choque_Ex_Ex)

# Combina os data frames e adiciona a coluna "Choque"
Choque_Ex_geral_df <- bind_rows(
  mutate(Choque_Ex_out_df, Choque = "Ex -> OutputGap"),
  mutate(Choque_Ex_inf_df, Choque = "Ex -> Inflação"),
  mutate(Choque_Ex_ir_df, Choque = "Ex -> InterestRate"),
  mutate(Choque_Ex_Ex_df, Choque = "Ex -> ExchangeRate")
)

# Adicionar a coluna Sequencia
Choque_Ex_geral_df <- Choque_Ex_geral_df %>%
  group_by(Choque) %>%
  mutate(Sequencia = row_number()) %>%
  ungroup()

# Criando os gráficos para os impulsos respostas


criar_grafico_irf_facet <- function(data_frame) {
  grafico <- ggplot(data = data_frame, aes(x = Sequencia, y = irf)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray70", alpha = 0.3) +
    geom_ribbon(aes(ymin = Upper, ymax = Lower), fill = "gray70", alpha = 0.3) +
    geom_line(color = "black", size = 1.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 0.8) +
    theme_bw() +
    labs(x = "Observation Time", y = "Response",
         title = paste(país, ": Impulse response of 1std dev of the exchange rate")) + # Adiciona o título com o país
    scale_x_continuous(breaks = seq(1, nrow(data_frame), by = 4)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(clip = 'off') +
    facet_wrap(~ Choque, scales = "free_y") # Adiciona a facet_wrap para diferenciar por fonte
  
  return(grafico)
}


# Gráfico_ExChoque

Gráfico_ExChoque_zar <- criar_grafico_irf_facet(Choque_Ex_geral_df)

#Choques do Câmbio nas variáveis cumulativo

Choque_Ex_out_cum <- irf(SVARmodel, impulse = "ExchangeRate", response = "OutputGap", n.ahead = 18, cumulative = T, ci = 0.90, runs = 999)

Choque_Ex_inf_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "Inflation", n.ahead = 18, cumulative = T,  ci = 0.90, runs = 999)

Choque_Ex_ir_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "InterestRate", n.ahead = 18, cumulative = T, ci = 0.90, runs = 999)

Choque_Ex_Ex_cum <-irf(SVARmodel, impulse = "ExchangeRate", response = "ExchangeRate", n.ahead = 18, cumulative = T, ci = 0.90, runs = 999)

# Extraindo as variáveis do IRF

Choque_Ex_out_df_cum <- extrair_variaveis_df(Choque_Ex_out_cum)
Choque_Ex_inf_df_cum <- extrair_variaveis_df(Choque_Ex_inf_cum)
Choque_Ex_ir_df_cum <- extrair_variaveis_df(Choque_Ex_ir_cum)
Choque_Ex_Ex_df_cum <- extrair_variaveis_df(Choque_Ex_Ex_cum)

# Combina os data frames e adiciona a coluna "Fonte"
Choque_Ex_geral_df_cum <- bind_rows(
  mutate(Choque_Ex_out_df_cum, Choque = "Ex -> OutputGap"),
  mutate(Choque_Ex_inf_df_cum, Choque = "Ex -> Inflação"),
  mutate(Choque_Ex_ir_df_cum, Choque = "Ex -> InterestRate"),
  mutate(Choque_Ex_Ex_df_cum, Choque = "Ex -> ExchangeRate")
)

# Adicionar a coluna Sequencia
Choque_Ex_geral_df_cum <- Choque_Ex_geral_df_cum %>%
  group_by(Choque) %>%
  mutate(Sequencia = row_number()) %>%
  ungroup()


# Gráfico_ExChoque

criar_grafico_irf_facet2 <- function(data_frame) {
  grafico <- ggplot(data = data_frame, aes(x = Sequencia, y = irf)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray70", alpha = 0.3) +
    geom_ribbon(aes(ymin = Upper, ymax = Lower), fill = "gray70", alpha = 0.3) +
    geom_line(color = "black", size = 1.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 0.8) +
    theme_bw() +
    labs(x = "Observation Time", y = "Response",
         title = paste(país, ": Cumulative impulse response of 1std dev of the exchange rate")) + # Adiciona o título com o país
    scale_x_continuous(breaks = seq(1, nrow(data_frame), by = 4)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(clip = 'off') +
    facet_wrap(~ Choque, scales = "free_y") # Adiciona a facet_wrap para diferenciar por fonte
  
  return(grafico)
}

Gráfico_ExChoque_zar_cum <- criar_grafico_irf_facet2(Choque_Ex_geral_df_cum)

ggsave(filename = paste(país, "nível_infla.png"), plot = p1_zar)
ggsave(filename = paste(país, "Choque.png"), plot = Gráfico_ExChoque_zar)
ggsave(filename = paste(país, "Choque_acumulado.png"), plot = Gráfico_ExChoque_zar_cum)



