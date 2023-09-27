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

# Carregar Arquivos

dados <- read_excel("C:\\Users\\erica\\OneDrive\\Documentos\\PUC RIO\\Monografia\\Base de Dados.xlsx", sheet = "Polonia")

# Código

# Informando o formato da data

dados$Data <- lubridate::ymd(dados$Data)

#calculando o MoM

dados$Cambio_MoM <- NaN
dados$Headline_MoM <- NaN
dados$Food_MoM <- NaN
dados$Energy_MoM <- NaN
dados$Goods_MoM <- NaN
dados$Services_MoM <- NaN
dados$Cambio_MoM [2: nrow(dados)] <- log(dados$Cambio [2:nrow(dados)] / dados$Cambio [1:nrow(dados)-1])
dados$Headline_MoM [2: nrow(dados)] <- log(dados$Headline [2:nrow(dados)] / dados$Headline [1:nrow(dados)-1])
dados$Food_MoM [2: nrow(dados)] <- log(dados$Food [2:nrow(dados)] / dados$Food [1:nrow(dados)-1])
dados$Energy_MoM [2: nrow(dados)] <- log(dados$Energy [2:nrow(dados)] / dados$Energy [1:nrow(dados)-1])
dados$Goods_MoM [2: nrow(dados)] <- log(dados$Goods [2:nrow(dados)] / dados$Goods [1:nrow(dados)-1])
dados$Services_MoM [2: nrow(dados)] <- log(dados$Services [2:nrow(dados)] / dados$Services [1:nrow(dados)-1])

# Come?ando em 2010 e Anualizando o dado mensal
var_data <- dados %>% filter(Data > "2009-12-01") %>% dplyr::select(Cambio_MoM,Headline_MoM) %>%
  mutate(
    Cambio_MoM = (Cambio_MoM + 1)^12 - 1,
    Headline_MoM = (Headline_MoM + 1)^12 - 1
  )

# var_data <- dados %>% filter(Data > "2009-12-02") %>% dplyr::select(Cambio_MoM,Headline_MoM,Food_MoM,Food_MoM,Energy_MoM,Goods_MoM, Services_MoM) * 12 * 100
plot(dados$Headline_MoM, type = "l")
# Criando o VAR para o Headline

lag_selection <- VARselect(var_data, lag.max = 24)$selection

var_model <-  VAR(var_data, p = lag_selection[1])

shock_size_pos2009 = summary(var_model)$varresult$Headline_MoM$sigma

# Impulso Resposta do VAR

n.ahead = 50
irf_ts_post2009 <- irf(var_model,
                       impulse = 'Cambio_MoM',
                       response = 'Headline_MoM',
                       ortho = TRUE,
                       n.ahead = n.ahead,
                       boot = TRUE,
                       runs=100)



# Passando os impulsos respostas para acumulado 12m
yoy_irf_pos2009 <- rep(0, n.ahead+12)
yoy_irf_pos2009[12:length(yoy_irf_pos2009)] <- irf_ts_post2009$irf$Cambio_MoM

# Como os valores do MoM est?o anualizados, a m?dia dos ?ltimos 12 meses ? uma boa aproxima??o para o YoY
yoy_irf_pos2009 <- zoo::rollmean(yoy_irf_pos2009, 12)


# Plotando o impulso acumulado 12m
yoy_irf_df <- data.frame(yoy_irf_pos2009
                         # , yoy_irf_pos2014, yoy_irf_pos2019
                         ) * 100


# Crie o gráfico de barras com as três séries
ggplot(yoy_irf_df, aes(x = 1:51)) +
  geom_bar(aes(y = yoy_irf_pos2009, fill = "2010"), stat = "identity", width = 0.2) +
  # geom_bar(aes(y = yoy_irf_pos2014, fill = "2014"), stat = "identity", width = 0.2, position = position_nudge(x = 0.2)) +
  # geom_bar(aes(y = yoy_irf_pos2019, fill = "2019"), stat = "identity", width = 0.2, position = position_nudge(x = 0.4)) +
  labs(title = "Impacto Acumulado em 12 meses no CPI Headline de um choque de um desvio padrão no Câmbio",
       x = "Meses",
       y = "Impacto em bps",
       fill = "Anos") +  # Personalize o título da legenda aqui
  scale_fill_manual(values = c("2010" = "red", "2014" = "blue", "2019" = "green")) +
  scale_x_continuous(breaks = seq(1, 51, by = 5)) +  # Define os intervalos do eixo X
  scale_y_continuous(breaks = seq(-100, 100, by = 5)) +  # Define os intervalos do eixo Y
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, color = "black", size = 0.5),
    panel.background = element_rect(fill = NA, color = NA),
    axis.line = element_line(color = "black")
  )

### decomp var
x1 <- svars::id.chol(var_model)
x2 <- svars::hd(x1, series = 2)
x3 <- svars::cf(x1, series = 2)

obs_series <- var_data$Headline_MoM[lag_selection[1]+1: nrow(var_data)]
decomp_df <- x2$hidec
decomp_df$obs_series <- var_data$Headline_MoM[(lag_selection[1]+1): nrow(var_data)]
decomp_df$dates <- (dados %>% filter(Data > "2009-12-01"))$Data[(lag_selection[1]+1): nrow(var_data)]


# Grafico
x <- decomp_df$dates[1: nrow(decomp_df)]
line1 <-x3$counter[1: nrow(decomp_df)]
line2 <- x3$actual$`Demeaned series  Headline_MoM`[1: nrow(decomp_df)]
bar <- x3$actual$`Demeaned series  Headline_MoM`[1: nrow(decomp_df)] - x3$counter[1: nrow(decomp_df)]
df <- data.frame(x = x, line1 = line1, line2 = line2, bar = bar)


# Crie o gráfico de linhas
plot_lines <- ggplot(df, aes(x = x)) +
  geom_line(aes(y = line1, color = "Contrafactual CPI Headline"), size = 1.5) +
  geom_line(aes(y = line2, color = "CPI Headline"), size = 1.5, linetype = "solid") +
  labs(x = "Date", y = "YoY", title = "CPI Headline: Contrafactual Ex Choques de Câmbio") +
  scale_color_manual(values = c("Contrafactual CPI Headline" = "Green", "CPI Headline" = "black"), name = "Legendas") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(
    panel.border = element_rect(fill = NA, color = "black", size = 0.5),
    panel.background = element_rect(fill = NA, color = NA),
    axis.line = element_line(color = "black")
  )

# Crie o gráfico de barras
plot_bars <- ggplot(df, aes(x = x)) +
  geom_bar(aes(y = bar, fill = "Efeito do Choque em bps"), stat = "identity", width = 5) +
  labs(x = "Date", y = "YoY") +
  scale_fill_manual(values = c("blue")) +
  theme_minimal() +
  labs(fill = NULL) +
  labs(y = "bps") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme(
      panel.border = element_rect(fill = NA, color = "black", size = 0.5),
      panel.background = element_rect(fill = NA, color = NA),
      axis.line = element_line(color = "black")
    )
  

# Combine os gráficos verticalmente
grid.arrange(plot_lines, plot_bars, ncol = 1)



# yoy_irf_pos2014 <- rep(0, n.ahead+12)
# yoy_irf_pos2014[12:length(yoy_irf_pos2014)] <- irf_ts_post2014$irf$Cambio_MoM
# yoy_irf_pos2014 <- zoo::rollmean(yoy_irf_pos2014, 12)
# 
# yoy_irf_pos2019 <- rep(0, n.ahead+12)
# yoy_irf_pos2019[12:length(yoy_irf_pos2019)] <- irf_ts_post2019$irf$Cambio_MoM
# yoy_irf_pos2019 <- zoo::rollmean(yoy_irf_pos2019, 12)

# # Come?ando em 2015 e Anualizando o dado mensal
# var_data <- dados %>% filter(Data > "2014-12-01") %>% dplyr::select(Cambio_MoM,Headline_MoM) * 12 * 100
# 
# # var_data <- dados %>% filter(Data > "2009-12-02") %>% dplyr::select(Cambio_MoM,Headline_MoM,Food_MoM,Food_MoM,Energy_MoM,Goods_MoM, Services_MoM) * 12 * 100
# 
# # Criando o VAR para o Headline
# 
# lag_selection <- VARselect(var_data, lag.max = 24)$selection
# 
# var_model <-  VAR(var_data, p = lag_selection[2])
# 
# shock_size_pos2014 = summary(var_model)$varresult$Headline_MoM$sigma
# 
# # Impulso Resposta do VAR
# 
# n.ahead = 50
# irf_ts_post2014 <- irf(var_model,
#                        impulse = 'Cambio_MoM',
#                        response = 'Headline_MoM',
#                        ortho = TRUE,
#                        n.ahead = n.ahead,
#                        boot = TRUE,
#                        runs=100)
# 
# # Plotando IRF Full Sample
# plot(irf_ts_post2014)
# 
# 
# # Come?ando em 2020 e Anualizando o dado mensal
# var_data <- dados %>% filter(Data > "2019-12-01") %>% dplyr::select(Cambio_MoM,Headline_MoM) * 12 * 100
# 
# # var_data <- dados %>% filter(Data > "2009-12-02") %>% dplyr::select(Cambio_MoM,Headline_MoM,Food_MoM,Food_MoM,Energy_MoM,Goods_MoM, Services_MoM) * 12 * 100
# 
# # Criando o VAR para o Headline
# 
# lag_selection <- VARselect(var_data, lag.max = 24)$selection
# 
# var_model <-  VAR(var_data, p = lag_selection[1])
# 
# shock_size_pos2019 = summary(var_model)$varresult$Headline_MoM$sigma
# 
# # Impulso Resposta do VAR
# 
# n.ahead = 50
# irf_ts_post2019 <- irf(var_model,
#                        impulse = 'Cambio_MoM',
#                        response = 'Headline_MoM',
#                        ortho = TRUE,
#                        n.ahead = n.ahead,
#                        boot = TRUE,
#                        runs=100)
# 
# # Plotando IRF Full Sample
# plot(irf_ts_post2019)

