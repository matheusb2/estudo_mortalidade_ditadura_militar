# =============================================================================
# Estratégia metodológica para estimação do excesso de mortalidade infantil e
# geral durante o regime militar brasileiro (1964-1985)
# =============================================================================

# Carregar pacotes
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(aTSA)
library(ipeaplot)
library(forecast)

#=============================================================================
# Correção de erros censitários
#=============================================================================

# Dados de subenumeração por idade e sexo (p)
p_men <- c(0.060, 0.044, 0.015, 0.017, 0.025, 0.064, 0.049, 0.0385, 0.0025, -0.0125, -0.040)
p_women <- c(0.056, 0.044, 0.009, 0.010, 0.015, 0.024, 0.012, 0.0105, 0.0025, -0.010, -0.030)
age_groups <- c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos",
                "20 a 24 anos", "25 a 29 anos", "30 a 39 anos", "40 a 49 anos",
                "50 a 59 anos", "60 a 69 anos", "70 ou mais")

# Taxas de omissão líquida por ano (para os anos censitários usados)
omissao <- data.frame(
  ano = c(1930, 1940, 1950, 1960, 1970, 1980, 1991),
  omissao_liquida = c(0.036, 0.036, 0.034, 0.033, 0.035, 0.023, 0.024)
)

# Função para corrigir um data.frame de um único ano censitário
corrigir_censo <- function(df_ano, ano) {
  # Extrair a taxa de omissão para o ano
  omissao_ano <- omissao$omissao_liquida[omissao$ano == ano]
  if (length(omissao_ano) == 0) stop("Ano sem taxa de omissão")
  
  C_total <- 1 / (1 - omissao_ano)
  
  # Construir nomes das colunas para homens e mulheres
  col_homens <- paste0("homens ", age_groups)
  col_mulheres <- paste0("mulheres ", age_groups)
  
  # Obter contagens brutas (já numéricas)
  pop_homens <- as.numeric(df_ano[col_homens])
  pop_mulheres <- as.numeric(df_ano[col_mulheres])
  
  # Fatores iniciais
  f0_homens <- 1 / (1 - p_men)
  f0_mulheres <- 1 / (1 - p_women)
  
  # População corrigida preliminar
  pop_corr_homens <- pop_homens * f0_homens
  pop_corr_mulheres <- pop_mulheres * f0_mulheres
  
  # Totais
  total_bruto <- sum(pop_homens) + sum(pop_mulheres)
  total_corr_prelim <- sum(pop_corr_homens) + sum(pop_corr_mulheres)
  
  # Fator de escala
  s <- (total_bruto * C_total) / total_corr_prelim
  
  # Fatores finais
  f_homens <- f0_homens * s
  f_mulheres <- f0_mulheres * s
  
  # Aplicar correção (usando listas para atribuição em uma única linha)
  df_ano[col_homens] <- as.list(pop_homens * f_homens)
  df_ano[col_mulheres] <- as.list(pop_mulheres * f_mulheres)
  
  # Atualizar a coluna "população" (total)
  df_ano$população <- sum(df_ano[col_homens]) + sum(df_ano[col_mulheres])
  
  return(df_ano)
}

# =============================================================================
# 1. PREPARAÇÃO ROBUSTA DOS DADOS
# =============================================================================

arquivo <- "~/Documentos/IPEA/modelos/demografia social da ditadura militar/censos_mortes_bruto.xlsx"

df_raw <- read_excel(
  path = arquivo,
  sheet = "BR_1950_2022"
)

names(df_raw)

# Função que converte qualquer coluna para numérico, tratando caracteres
to_numeric <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    # Remove pontos de milhar e substitui vírgula decimal por ponto
    x <- gsub("\\.", "", x)  # remove pontos (milhar)
    x <- gsub(",", ".", x)   # vírgula para ponto decimal
  }
  as.numeric(x)
}

# Lista de colunas que devem ser numéricas
colunas_populacionais <- c(
  paste0("homens ", c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24",
                      "25 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69",
                      "70 ou mais"), " anos"),
  paste0("mulheres ", c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24",
                        "25 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69",
                        "70 ou mais"), " anos"),
  "população", "mortalidade_infantil", "mort_infant", "fecundidade",
  "nascidos_vivos", "mortos e desaparecidos documentados", "mortes violentas SP"
)

# Converter todas as colunas existentes no df_raw
df_raw <- df_raw %>%
  mutate(across(any_of(colunas_populacionais), to_numeric))

# Garantir que 'ano' seja numérico
df_raw$ano <- as.numeric(df_raw$ano)

# =============================================================================
# 2. EXTRAÇÃO DE SÉRIES BÁSICAS
# =============================================================================

census_years <- c(1930, 1940, 1950, 1960, 1970, 1980, 1991)

census_years_urban <- c(1940, 1950, 1960, 1970, 1980, 1991)


df_census <- df_raw %>%
  filter(ano %in% census_years) %>%
  arrange(ano)


for (ano in census_years) {
  idx <- which(df_raw$ano == ano)
  if (length(idx) == 1) {
    df_raw[idx, ] <- corrigir_censo(df_raw[idx, ], ano)
  }
}

pop_census <- df_census$população   # já numérico

# Interpolação linear da população total 1940-1991
all_years <- 1930:1991

# Extrai os valores observados nos anos censitários
pop_census <- df_census$população   # vetor numérico (tamanho 7)
urb_census <- df_census$pop_urbana  # vetor numérico (tamanho 7)

# Define a sequência completa de anos para interpolação
all_years_urban <- 1940:1991


# Interpolação log-linear (exponencial) para obter séries contínuas
pop_total <- exp(approx(census_years, log(pop_census), xout = all_years)$y)
pop_urbana <- exp(approx(census_years, log(urb_census), xout = all_years)$y)



# Filtra os anos onde PIB é conhecido (remove NAs)
df_pib_known <- df_raw %>%
  filter(!is.na(PIB)) %>%
  arrange(ano)

# Extrai os vetores de anos e valores conhecidos
anos_pib <- df_pib_known$ano
pib_known <- df_pib_known$PIB

# Interpolação log-linear para todos os anos do período
PIB_interp <- exp(approx(anos_pib, log(pib_known), xout = all_years)$y)

PIB_per_capita <- 

# Grupos etários femininos para estimativa de nascimentos
fem_groups <- c("mulheres 15 a 19 anos", "mulheres 20 a 24 anos",
                "mulheres 25 a 29 anos", "mulheres 30 a 39 anos",
                "mulheres 40 a 49 anos")

# Amplitudes de cada grupo (em anos)
widths <- c(5, 5, 5, 10, 10)

# Pesos de fecundidade típicos (ajustáveis)
pesos_fec <- c(0.10, 0.25, 0.25, 0.15, 0.05)  # soma 0.80, restante em 10-14 e 45-49
pesos_fec <- pesos_fec / sum(pesos_fec)  # agora somam 1

# Interpolação da população feminina por grupo etário
fem_pop_interp <- matrix(NA, nrow = length(all_years), ncol = length(fem_groups))
colnames(fem_pop_interp) <- fem_groups



for (i in seq_along(fem_groups)) {
  vals <- df_census[[fem_groups[i]]]
  # Interpolar para todos os anos
  fem_pop_interp[, i] <- exp(approx(census_years, log(vals), xout = all_years)$y)
}

# Taxa de fecundidade total (TFT) anual
TFT_anual <- df_raw %>%
  filter(ano %in% all_years) %>%
  pull(fecundidade)

# Estimar nascimentos anuais
# Cálculo correto dos nascimentos anuais
nascimentos <- numeric(length(all_years))
for (yr in seq_along(all_years)) {
  nascimentos[yr] <- TFT_anual[yr] * sum(pesos_fec * fem_pop_interp[yr, ] / widths)
}

df_raw$mortalidade_infantil
# Mortalidade infantil (IMR) – interpolação anual 1930-1990
imr_raw <- df_raw |>
  dplyr::select(ano, mortalidade_infantil) |>
  filter(!is.na(mortalidade_infantil)) |>
  arrange(ano)

imr_years_all <- 1930:2022
imr_interp <- spline(imr_raw$ano, imr_raw$mortalidade_infantil,
                     xout = imr_years_all, method = "fmm")$y

plot(imr_interp, type = "l")

# =============================================================================
# 3. MIGRAÇÃO LÍQUIDA POR DÉCADA
# =============================================================================

# Participação do saldo migratório no crescimento populacional (%)
mig_part <- data.frame(
  periodo_inicio = c(1940, 1950, 1960, 1970, 1980),
  periodo_fim    = c(1950, 1960, 1970, 1980, 1991),
  participacao   = c(1.0, 3.4, 1.32, 0.9, 0.52) / 100
)

# =============================================================================
# 4. ÓBITOS TOTAIS POR PERÍODO INTERCENSITÁRIO
# =============================================================================

periodos <- list(
  "1940-1950" = c(1940,1950),
  "1950-1960" = c(1950,1960),
  "1960-1970" = c(1960,1970),
  "1970-1980" = c(1970,1980),
  "1980-1991" = c(1980,1991)
)

resultados <- data.frame(periodo = character(),
                         pop_inicio = numeric(), pop_fim = numeric(),
                         nasc = numeric(), migracao = numeric(),
                         obitos = numeric())

for (p in names(periodos)) {
  t0 <- periodos[[p]][1]
  t1 <- periodos[[p]][2]
  
  P0 <- pop_total[all_years == t0]
  P1 <- pop_total[all_years == t1]
  
  anos_periodo <- t0:(t1 - 1)
  B <- sum(nascimentos[all_years %in% anos_periodo])
  
  linha_mig <- mig_part[mig_part$periodo_inicio == t0, ]
  M <- linha_mig$participacao * (P1 - P0)
  
  D <- P0 - P1 + B + M
  
  resultados <- rbind(resultados,
                      data.frame(periodo = p, pop_inicio = P0, pop_fim = P1,
                                 nasc = B, migracao = M, obitos = D))
}

cat("Óbitos reconstruídos por década:\n")
print(resultados %>% mutate(across(where(is.numeric), ~ round(., 0))))

# =============================================================================
# 5. ANUALIZAÇÃO DOS ÓBITOS (USANDO IMR)
# =============================================================================

obitos_anuais <- numeric(length(all_years))

for (i in 1:nrow(resultados)) {
  t0 <- periodos[[resultados$periodo[i]]][1]
  t1 <- periodos[[resultados$periodo[i]]][2]
  anos <- t0:(t1 - 1)
  
  # IMR para cada ano do período
  imr_periodo <- imr_interp[imr_years_all %in% anos]
  
  # Se a soma das IMR for zero (improvável), distribuir uniformemente
  if (sum(imr_periodo) == 0) {
    pesos <- rep(1/length(anos), length(anos))
  } else {
    pesos <- imr_periodo / sum(imr_periodo)
  }
  
  obitos_anuais[all_years %in% anos] <- resultados$obitos[i] * pesos
}

# Taxa bruta de mortalidade observada
CDR_obs <- obitos_anuais / pop_total

# Verificação rápida
if (any(is.na(obitos_anuais[1:30]))) {
  warning("obitos_anuais contém NAs nos primeiros anos. Verifique a conversão dos dados.")
}

# =============================================================================
# 6. RELAÇÃO CDR ~ IMR NO PERÍODO PRÉ-1964
# =============================================================================

# definição de anos prévios
anos_pre <- 1940:1963
ind_pre <- all_years %in% anos_pre

dados_pre <- data.frame(
  ano = anos_pre,
  CDR = CDR_obs[ind_pre],
  IMR = imr_interp[imr_years_all %in% anos_pre]
)

#teste de cointegração
coint.test(dados_pre$CDR, dados_pre$ano + dados_pre$CDR)

# como a cointegração não funcionou, optei por usar um arima automático mantendo 
# a mortalidade infantil como preditora exógena
modelo_cdr_imr <- auto.arima(dados_pre$CDR, xreg = dados_pre$IMR)

summary(modelo_cdr_imr)


# =============================================================================
# 7. CENÁRIOS CONTRAFACTUAIS DE MORTALIDADE: MODELO LOG-LINEAR
# =============================================================================

# 7.1 Estimativa do contrafactual log-linear de mortalidade infantil

# IMR histórica 1930-1960 para modelo log-linear
imr_hist <- data.frame(
  ano = seq(1944, 1963, 1),
  IMR = imr_interp[imr_years_all %in% seq(from = 1944, to = 1963, by = 1)]
)

# Testes de cointegração
coint.test(imr_hist$IMR, imr_hist$ano)
coint.test(imr_hist$IMR, imr_hist$ano + imr_hist$ano^2+ imr_hist$ano^3)


# modelo log-linear
modelo_log <- lm(log(IMR) ~ ano, data = imr_hist)

summary(modelo_log)

# período de projeção

anos_regime <- 1964:1985

# modelo preditivo de tendência de mortalidade

imr_model <- exp(predict(modelo_log, interval = "prediction", level = 0.95,
                           newdata = data.frame(ano = anos_regime)))
summary(imr_model)

# Intervalo de confiança e estimativa pontual

imr_central <- imr_model[, "fit"]
imr_max     <- imr_model[, "lwr"]
imr_min     <- imr_model[, "upr"]

# Verificar
plot(anos_regime, imr_central, type = "l", ylim = range(imr_min, imr_min),
     main = "IMR contrafactual (1964-1985)", xlab = "Ano", ylab = "IMR")
lines(anos_regime, imr_min, lty = 2)
lines(anos_regime, imr_max, lty = 2)

#-----------------------------------------------------------------------------
# 7.2 VISUALIZAÇÃO DA MORTALIDADE INFANTIL – OBSERVADA E CONTRAFACTUAL
#-----------------------------------------------------------------------------

# 1. Preparar dados observados (até 1985)
dados_obs <- data.frame(
  ano = 1930:1985,
  IMR = imr_interp[1:56]  # índices correspondentes a 1930:1985
) %>%
  filter(!is.na(IMR))

# 2. Preparar dados contrafactuais (1964‑1985)
# (supondo que você já calculou imr_central, imr_min, imr_max para 1964‑1985)
dados_contra <- data.frame(
  ano = 1964:1985,
  IMR_central = as.numeric(imr_central),
  IMR_min     = as.numeric(imr_min),
  IMR_max     = as.numeric(imr_max)
)

# 3. Combinar para o gráfico
# Vamos criar uma coluna que identifica se é observado ou contrafactual
dados_obs$tipo <- "Observado"
dados_contra$tipo <- "Contrafactual (central)"

# Para plotar a faixa de incerteza, usamos geom_ribbon
# e para a linha central, geom_line

ggplot() +
  # Faixa de incerteza da projeção contrafactual
  geom_ribbon(data = dados_contra,
              aes(x = ano, ymin = IMR_min, ymax = IMR_max),
              fill = "lightblue", alpha = 0.4) +
  # Linha central da projeção contrafactual
  geom_line(data = dados_contra,
            aes(x = ano, y = IMR_central, color = "Contrafactual"),
            size = 1.2, linetype = "dashed") +
  # Linha da série observada
  geom_line(data = dados_obs,
            aes(x = ano, y = IMR, color = "Observado"),
            size = 1) +
  # Marcador do início do regime militar
  geom_vline(xintercept = 1964, linetype = "dotted", color = "red", size = 1) +
  annotate("text", x = 1964, y = max(dados_obs$IMR, na.rm = TRUE) * 0.9,
           label = "Início do regime militar", color = "red", hjust = -0.1) +
  # Escalas e temas
  scale_color_manual(name = "Série",
                     values = c("Observado" = "black", "Contrafactual" = "blue")) +
  labs(
    title = "Mortalidade infantil (IMR) – observada e contrafactual",
    subtitle = "Projeção log-linear com base na tendência 1945-1963",
    x = "Ano",
    y = "IMR (óbitos por 1.000 nascidos vivos)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#-----------------------------------------------------------------------------
# 7.3  EXCESSO DE MORTALIDADE INFANTIL – MODELO LOG‑LINEAR
#-----------------------------------------------------------------------------
# 1. Vetor de anos do regime (já definido)
anos_regime <- 1964:1985

# 2. Extrair a IMR observada para o mesmo período
ind_obs <- which(imr_years_all %in% anos_regime)
imr_observado <- imr_interp[ind_obs]

# 3. As projeções já estão em imr_central, imr_min, imr_max (vetores de tamanho 22)

# 4. Calcular excesso anual (observado - projetado)
excesso_imr_central <- imr_observado - imr_central
excesso_imr_min     <- imr_observado - imr_min
excesso_imr_max     <- imr_observado - imr_max

# 5. Excesso acumulado (soma simples, pode ser negativa)
excesso_acum_central <- sum(excesso_imr_central)
excesso_acum_min     <- sum(excesso_imr_min)
excesso_acum_max     <- sum(excesso_imr_max)

cat("\n========== EXCESSO DE MORTALIDADE INFANTIL (IMR) – LOG‑LINEAR ==========\n")
cat(sprintf("Excesso acumulado (central): %0.2f óbitos por 1.000 NV\n", excesso_acum_central))
cat(sprintf("Excesso acumulado (mínimo):   %0.2f óbitos por 1.000 NV\n", excesso_acum_min))
cat(sprintf("Excesso acumulado (máximo):   %0.2f óbitos por 1.000 NV\n", excesso_acum_max))

# 6. Excesso truncado em zero (apenas anos com IMR observada > projetada)
excesso_pos_central <- pmax(excesso_imr_central, 0)
excesso_pos_min     <- pmax(excesso_imr_min, 0)
excesso_pos_max     <- pmax(excesso_imr_max, 0)

excesso_pos_acum_central <- sum(excesso_pos_central)
excesso_pos_acum_min     <- sum(excesso_pos_min)
excesso_pos_acum_max     <- sum(excesso_pos_max)

cat("\n--- Excesso truncado em zero (apenas anos com excesso positivo) ---\n")
cat(sprintf("Excesso positivo acumulado (central): %0.2f óbitos por 1.000 NV\n", excesso_pos_acum_central))
cat(sprintf("Excesso positivo acumulado (mínimo):   %0.2f óbitos por 1.000 NV\n", excesso_pos_acum_min))
cat(sprintf("Excesso positivo acumulado (máximo):   %0.2f óbitos por 1.000 NV\n", excesso_pos_acum_max))


tibble( c("Excesso acumulado central", 
          "excesso acumulado mínimo", 
          "excesso acumulado máximo"),
        c(sum(nascimentos[35:56]*(excesso_pos_central/100)), 
          sum(nascimentos[35:56]*(excesso_pos_min/100)), 
          sum(nascimentos[35:56]*(excesso_pos_max/100))))

# 7. Tabela anual para inspeção
tabela_imr_excesso <- data.frame(
  Ano = anos_regime,
  IMR_Obs = round(imr_observado, 2),
  IMR_Proj_Central = round(imr_central, 2),
  IMR_Proj_Min = round(imr_min, 2),
  IMR_Proj_Max = round(imr_max, 2),
  Excesso_Central = round(excesso_imr_central, 2),
  Excesso_Min = round(excesso_imr_min, 2),
  Excesso_Max = round(excesso_imr_max, 2)
)

cat("\n--- Tabela anual do excesso de IMR (apenas central) ---\n")
print(tabela_imr_excesso[, c("Ano", "IMR_Obs", "IMR_Proj_Central", "Excesso_Central")])

# 8. Gráfico do excesso anual (central)
library(ggplot2)
ggplot(tabela_imr_excesso, aes(x = Ano, y = Excesso_Central)) +
  geom_col(fill = ifelse(tabela_imr_excesso$Excesso_Central > 0, "red", "blue"), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Excesso de mortalidade infantil – modelo log‑linear",
    subtitle = "Diferença entre IMR observada e projetada (1964‑1985)",
    x = "Ano", y = "Excesso de IMR (óbitos por 1.000 NV)"
  ) +
  theme_minimal()

#-----------------------------------------------------------------------------
# 7.4 CÁLCULO DO EXCESSO DE MORTALIDADE GERAL
#-----------------------------------------------------------------------------


ind_regime <- all_years %in% anos_regime
pop_regime <- pop_total[ind_regime]
obitos_regime <- obitos_anuais[ind_regime]

#função para alcular o excesso 
calcular_excesso <- function(imr_contra) {
  CDR_contra <- forecast(modelo_cdr_imr,  xreg = imr_contra) 
  obitos_esperados <- CDR_contra$mean * pop_regime
  excesso <- obitos_regime - obitos_esperados
  total <- sum(excesso)
  list(excesso_anual = excesso, total = total)
}

excesso_max <- calcular_excesso(imr_max)   # queda máxima de mortalidade 
excesso_central <- calcular_excesso(imr_central) #tendência central
excesso_min <- calcular_excesso(imr_min)   # queda mínima de mortalidade

# ----------------------------------------------------------------------------
# 7.5 RESULTADOS
# ----------------------------------------------------------------------------

cat("\n========================== RESULTADOS ==========================\n")
cat(sprintf("Excesso de mortalidade 1964–1985 (cenário máximo): %0.0f óbitos\n",
            excesso_max$total))
cat(sprintf("Excesso de mortalidade 1964–1985 (cenário central): %0.0f óbitos\n",
            excesso_central$total))
cat(sprintf("Excesso de mortalidade 1964–1985 (cenário mínimo): %0.0f óbitos\n",
            excesso_min$total))

pred_min <-  forecast(modelo_cdr_imr,  xreg = imr_min)
pred_central <- forecast(modelo_cdr_imr,  xreg = imr_central)
pred_max <- forecast(modelo_cdr_imr,  xreg = imr_max)

# Tabela anual do excesso
resultado_anual <- data.frame(
  Ano = anos_regime,
  Observado = round(obitos_regime, 0),
  Esperado_Min = round(pred_max$mean * pop_regime, 0),
  Esperado_Max = round(pred_min$mean * pop_regime, 0),
  Esperado_Central = round(pred_central$mean * pop_regime, 0),
  Excesso_Max = round(excesso_max$excesso_anual, 0),
  Excesso_Central = round(excesso_central$excesso_anual, 0),
  Excesso_Min = round(excesso_min$excesso_anual, 0)
)

print(resultado_anual)

# Visualização
dat_plot <- resultado_anual %>%
  dplyr::select(Ano, Observado, Esperado_Min, Esperado_Central, Esperado_Max) %>%
  pivot_longer(-Ano, names_to = "Cenário", values_to = "Óbitos") %>%
  mutate(`Óbitos (mil)` = `Óbitos` / 1000)

grafico_coorte_componente <- ggplot(dat_plot, aes(x = Ano, y = `Óbitos (mil)`, color = Cenário)) +
  geom_line(size = 1) +
  labs(title = "Óbitos anuais observados e contrafactuais (1964–1985)", 
       subtitle = "Coorte-componente com projeção log-linear (tendência 46-63)",
       y = "Óbitos (milhares)") +
  theme_minimal()

print(grafico_coorte_componente)

save_ipeaplot(grafico_coorte_componente, "Gráfico de coorte-componente contrafactual",
              format = c("eps", "png"))

# =============================================================================
# 8. PROJEÇÃO ARIMA PRÉ-1964 – EXCESSO DE MORTALIDADE INFANTIL
# Ajuste do modelo com dados de 1945-1963 e projeção para 1964-1985
# =============================================================================

library(forecast)
library(ggplot2)
library(dplyr)

# 1. Preparar a série histórica (1945–1963)
anos_hist <- 1944:1963
imr_hist <- imr_interp[imr_years_all %in% anos_hist]  # IMR observada no período pré-regime

# Criar um objeto ts
serie_hist <- ts(imr_hist, start = 1945, frequency = 1)

# 2. Ajustar modelo ARIMA automaticamente (seleção por AICc)
modelo_arima_pre <- auto.arima(serie_hist, 
                               stepwise = FALSE, 
                               approximation = FALSE,
                               seasonal = FALSE)  # dados anuais, sem sazonalidade

# Resumo do modelo
cat("Modelo ARIMA ajustado ao período 1945-1963:\n")
print(summary(modelo_arima_pre))

# 3. Projetar para 1964–1985 (22 anos) com intervalo de 95%
h <- 22
# Para obter dois níveis (ex: 80% e 95%), especifique level = c(80, 95)
# Se quiser apenas 95%, use level = 95, mas acesse [,1]
projecao <- forecast(modelo_arima_pre,  
                     bootstrap = TRUE, h = h, level = c(80, 95))

# Extrair a projeção (valores centrais)
imr_projetado <- as.numeric(projecao$mean)

# Intervalo de 95% (segunda coluna, pois a primeira é 80%)
imr_projetado_inf <- as.numeric(projecao$lower[, 2])   # 95% inferior
imr_projetado_sup <- as.numeric(projecao$upper[, 2])   # 95% superior

# (Caso tenha usado level = 95 apenas, use [,1])
# imr_projetado_inf <- as.numeric(projecao$lower[, 1])
# imr_projetado_sup <- as.numeric(projecao$upper[, 1])

anos_proj <- 1964:1985

# 4. Preparar dados observados para o mesmo período
ind_obs <- imr_years_all %in% anos_proj
imr_observado <- imr_interp[ind_obs]

# 5. Calcular o excesso de IMR (observado - projetado) ano a ano
excesso_imr <- imr_observado - imr_projetado
excesso_imr_inf <- imr_observado - imr_projetado_sup   # inversão (inferior do excesso)
excesso_imr_sup <- imr_observado - imr_projetado_inf   # superior do excesso

# Excesso acumulado (soma das diferenças)
excesso_acumulado_imr <- sum(excesso_imr)

cat("\n======================== RESULTADOS ========================\n")
cat(sprintf("Excesso acumulado de IMR (1964–1985): %0.2f óbitos por 1.000 NV\n", 
            excesso_acumulado_imr))

# 6. Converter o excesso de IMR em excesso de óbitos usando a relação CDR ~ IMR
#    (modelo_cdr_imr já foi estimado com dados de 1940-1963)
#    e a população real do período.

# Para cada ano, prever a CDR a partir da IMR projetada e observada
ind_regime <- all_years %in% anos_proj
pop_regime <- pop_total[ind_regime]

# CDR esperada (projetada)
CDR_projetado <- forecast(modelo_cdr_imr, xreg = imr_projetado)

# CDR observada (real)
CDR_observado <- forecast(modelo_cdr_imr, xreg = imr_observado)

# Óbitos esperados (projetados)
obitos_esperados <- CDR_projetado$mean * pop_regime

# Óbitos observados (reais)
obitos_observados <- CDR_observado$mean * pop_regime

# Excesso de óbitos anual
excesso_obitos <- obitos_observados - obitos_esperados

# Excesso total de óbitos
excesso_total_obitos <- sum(excesso_obitos)

cat(sprintf("\nExcesso total de óbitos (1964–1985): %0.0f\n", excesso_total_obitos))
cat("=============================================================\n")

# 7. Tabela anual com os resultados
tabela_resultados <- data.frame(
  Ano = anos_proj,
  IMR_Observado = round(imr_observado, 2),
  IMR_Projetado = round(imr_projetado, 2),
  IMR_Projetado_inf = round(imr_projetado_inf, 2),
  IMR_Projetado_sup = round(imr_projetado_sup, 2),
  Excesso_IMR = round(excesso_imr, 2),
  Excesso_IMR_inf = round(excesso_imr_inf, 2),
  Excesso_IMR_sup = round(excesso_imr_sup, 2),
  Obitos_Observados = round(obitos_observados, 0),
  Obitos_Esperados = round(obitos_esperados, 0),
  Excesso_Obitos = round(excesso_obitos, 0)
)

print(tabela_resultados)

# 8. Gráficos

# a) IMR observada vs. projetada (com intervalo)
dados_grafico <- data.frame(
  Ano = c(anos_hist, anos_proj),
  IMR = c(imr_hist, imr_observado),
  Tipo = c(rep("Histórico", length(anos_hist)), rep("Observado", length(anos_proj)))
)

dados_proj <- data.frame(
  Ano = anos_proj,
  IMR_proj = imr_projetado,
  inf = imr_projetado_inf,
  sup = imr_projetado_sup
)

ggplot() +
  geom_line(data = dados_grafico, aes(x = Ano, y = IMR, color = Tipo), size = 1) +
  geom_ribbon(data = dados_proj, aes(x = Ano, ymin = inf, ymax = sup),
              fill = "lightblue", alpha = 0.4) +
  geom_line(data = dados_proj, aes(x = Ano, y = IMR_proj, color = "Projetado (ARIMA)"),
            size = 1, linetype = "dashed") +
  geom_vline(xintercept = 1964, linetype = "dotted", color = "red") +
  annotate("text", x = 1964, y = max(imr_hist) * 0.9,
           label = "Início do regime militar", color = "red", hjust = -0.1) +
  scale_color_manual(values = c("Histórico" = "black",
                                "Observado" = "darkgreen",
                                "Projetado (ARIMA)" = "blue")) +
  labs(title = "Mortalidade infantil – observada vs. projetada (ARIMA pré-1964)",
       subtitle = "Projeção baseada na tendência 1945-1963",
       x = "Ano", y = "IMR (óbitos por 1.000 nascidos vivos)") +
  theme_minimal()

# b) Excesso anual de óbitos
ggplot(tabela_resultados, aes(x = Ano, y = Excesso_Obitos / 1000)) +
  geom_col(fill = ifelse(tabela_resultados$Excesso_Obitos > 0, "red", "blue"), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Excesso anual de óbitos (ARIMA pré-1964)",
       subtitle = "Diferença entre óbitos observados e esperados (1964-1985)",
       x = "Ano", y = "Excesso (milhares de óbitos)") +
  theme_minimal()

# 9. Diagnóstico do modelo ARIMA
cat("\nDiagnóstico do modelo ARIMA:\n")
checkresiduals(modelo_arima_pre)

# Teste de Ljung-Box para autocorrelação residual
print(Box.test(residuals(modelo_arima_pre), lag = 10, type = "Ljung-Box"))

cat("Excesso acumulado de mortalidade infantil: ", sum(pmax(tabela_resultados$Excesso_Obitos, 0)))

# -----------------------------------------------------------------------------
# 8.1. ESTIMAÇÃO DO EXCESSO DE MORTALIDADE GERAL USANDO ARIMAX (CONTRAFACTUAL)
# ----------------------------------------------------------------------------

library(forecast)
library(dplyr)

# 1. Preparar a série histórica da IMR pré-regime (1930–1963) para projeção contrafactual
anos_hist_arimax <- 1944:1963
imr_hist_ts <- ts(imr_interp[imr_years_all %in% anos_hist_arimax], frequency = 1)

# 2. Ajustar um modelo ARIMA univariado na série histórica (tendência contrafactual pura)
modelo_arima_contra <- auto.arima(imr_hist_ts, stepwise = FALSE, approximation = FALSE)

# 3. Projetar a IMR contrafactual para o período do regime militar (1964–1985)
# Número de períodos a projetar: 1985 - 1964 + 1 = 22 anos
# Utilizando forecast::forecast explicitamente para evitar conflitos de namespace
proj_arimax <- forecast::forecast(modelo_arima_contra, h = 22, level = 80, bootstrap = TRUE)

imr_central_arimax <- as.numeric(proj_arimax$mean)
imr_min_arimax     <- as.numeric(proj_arimax$lower) # Limite inferior da projeção
imr_max_arimax     <- as.numeric(proj_arimax$upper) # Limite superior da projeção

# 4. Função adaptada para calcular o excesso de mortalidade geral via ARIMAX
calcular_excesso_arimax <- function(imr_contra_vec) {
  CDR_contra <- forecast(modelo_cdr_imr, xreg = imr_contra_vec)
  obitos_esperados <- CDR_contra$mean * pop_regime
  excesso <- obitos_regime - obitos_esperados
  total <- sum(excesso)
  list(excesso_anual = excesso, total = total)
}

excesso_min_arimax     <- calcular_excesso_arimax(imr_max_arimax)
excesso_central_arimax <- calcular_excesso_arimax(imr_central_arimax)
excesso_max_arimax     <- calcular_excesso_arimax(imr_min_arimax)

# ----------------------------------------------------------------------------
# 8.2. RESULTADOS DO MODELO ARIMAX
# ----------------------------------------------------------------------------

cat("\n=================== RESULTADOS: ESTIMATIVA ARIMAX ===================\n")
cat(sprintf("Excesso de mortalidade geral 1964–1985 (cenário máximo): %0.0f óbitos\n",
            excesso_max_arimax$total))
cat(sprintf("Excesso de mortalidade geral 1964–1985 (cenário central): %0.0f óbitos\n",
            excesso_central_arimax$total))
cat(sprintf("Excesso de mortalidade geral 1964–1985 (cenário mínimo): %0.0f óbitos\n",
            excesso_min_arimax$total))

pred_arima_min <- forecast(modelo_cdr_imr, xreg = imr_min_arimax)
pred_arima_central <- forecast(modelo_cdr_imr, xreg = imr_central_arimax)
pred_arima_max <- forecast(modelo_cdr_imr, xreg = imr_max_arimax)

# Tabela anual do excesso com ARIMAX
resultado_anual_arimax <- data.frame(
  Ano = anos_regime,
  Observado = round(obitos_regime, 0),
  Esperado_Max = round(pred_arima_max$mean * pop_regime, 0),
  Esperado_Min = round(pred_arima_min$mean * pop_regime, 0),
  Esperado_Central = round(pred_arima_central$mean * pop_regime, 0),
  Excesso_Max = round(excesso_max_arimax$excesso_anual, 0),
  Excesso_Central = round(excesso_central_arimax$excesso_anual, 0),
  Excesso_Min = round(excesso_min_arimax$excesso_anual, 0)
)

print(resultado_anual_arimax)

# Visualização gráfica do excesso com ARIMAX
dat_plot_arimax <- resultado_anual_arimax %>%
  dplyr::select(Ano, Observado, Esperado_Min, Esperado_Central, Esperado_Max) %>%
  pivot_longer(-Ano, names_to = "Cenário", values_to = "Óbitos") %>%
  mutate(`Óbitos (mil)` = `Óbitos` / 1000)

grafico_arimax_geral <- ggplot(dat_plot_arimax, aes(x = Ano, y = `Óbitos (mil)`, color = Cenário)) +
  geom_line(size = 1) +
  labs(title = "Óbitos anuais observados e contrafactuais (1964–1985)", 
       subtitle = "Estimativa de excesso de mortalidade geral via projeção ARIMAX",
       y = "Óbitos (milhares)",
       x = "Ano") +
  theme_minimal()

print(grafico_arimax_geral)


# =============================================================================
# 9. ESTUDO DE SÉRIE TEMPORAL INTERROMPIDA (ITS) – MORTALIDADE INFANTIL
# Interrupção em 1964 – início do regime militar
# =============================================================================

library(tidyverse)
library(lmtest)
library(sandwich)
library(forecast)
library(tseries)

# 1. Preparar os dados (1930–1985)
# Supondo que 'imr_interp' seja a série interpolada de mortalidade infantil
dados_its <- data.frame(
  ano = 1944:1985,
  imr = imr_interp[15:56]  # ajuste o índice conforme sua série
) %>%

  filter(!is.na(imr))

# 2. Criar variáveis para o modelo ITS
#    - Tempo (centrado para reduzir multicolinearidade)
#    - Interrupção (0 antes de 1964, 1 a partir de 1964)
#    - Tempo após interrupção (0 antes de 1964, (ano - 1963) a partir de 1964)

ano_interrupcao <- 1964

dados_its <- dados_its %>%
  mutate(
    tempo = ano - 1943,                    # variável contínua (centrado)
    interrupcao = ifelse(ano >= ano_interrupcao, 1, 0),
    tempo_pos = ifelse(ano >= ano_interrupcao, ano - ano_interrupcao, 0)
  )

# 3. Ajuste do modelo ITS por mínimos quadrados (OLS)
modelo_its <- lm(imr ~ tempo + interrupcao + tempo_pos, data = dados_its)

summary(modelo_its)

# 4. Verificar autocorrelação dos resíduos (teste Durbin-Watson)
dwtest(modelo_its)  # se p < 0.05, há autocorrelação

# 5. Se houver autocorrelação, estimar erros robustos (Newey-West)
#    ou usar um modelo ARIMA para corrigir.
# Opção 1: Erros padrão robustos para autocorrelação e heterocedasticidade
coeftest(modelo_its, vcov = NeweyWest(modelo_its, lag = 1, prewhite = FALSE))

# Opção 2: Ajustar um modelo ARIMA com a estrutura ITS como regressores
# (Recomendado para séries com autocorrelação forte)
serie_ts <- ts(dados_its$imr, start = 1930, frequency = 1)

# Criar matriz de regressores (tempo, interrupcao, tempo_pos)
xreg <- cbind(
  tempo = dados_its$tempo,
  interrupcao = dados_its$interrupcao,
  tempo_pos = dados_its$tempo_pos
)

# Ajustar ARIMA com regressores (ARIMAX) – seleção automática
modelo_arimax <- auto.arima(serie_ts, xreg = xreg, stepwise = FALSE, approximation = FALSE)

# Resumo do modelo
summary(modelo_arimax)

# Coeficientes e significância
coeftest(modelo_arimax)

# Gráfico

# Série observada e valores ajustados pelo modelo ARIMAX
dados_its$ajustado_arimax <- fitted(modelo_arimax)

ggplot(dados_its, aes(x = ano)) +
  geom_line(aes(y = imr, color = "Observado"), size = 1) +
  geom_line(aes(y = ajustado_arimax, color = "Ajustado (ARIMAX)"), size = 1, linetype = "dashed") +
  geom_vline(xintercept = 1964, linetype = "dotted", color = "red", size = 1) +
  annotate("text", x = 1964, y = max(dados_its$imr, na.rm = TRUE) * 0.9,
           label = "1964 – Início do regime militar", color = "red", hjust = -0.1) +
  labs(
    title = "Mortalidade infantil – ITS com ARIMAX",
    subtitle = "Ajuste do modelo com interrupção em 1964",
    x = "Ano", y = "IMR (óbitos por 1.000 nascidos vivos)"
  ) +
  scale_color_manual(values = c("Observado" = "black", "Ajustado (ARIMAX)" = "blue")) +
  theme_minimal()


# 6. Calcular efeitos
# Extrair coeficientes do modelo ARIMAX (ou do OLS com erros robustos)
coefs <- coef(modelo_arimax)
coef(modelo_arimax)
names(coefs)
# Os coeficientes estão na ordem: regressores (tempo, interrupcao, tempo_pos) e AR/MA
# Identificar os índices corretos:

beta_tempo <- coefs["tempo"]
beta_interrupcao <- coefs["interrupcao"]
beta_tempo_pos <- coefs["tempo_pos"]

# Efeito imediato (mudança de nível em 1964): beta_interrupcao
# Efeito gradual (mudança na inclinação): beta_tempo_pos
# Efeito acumulado até 1985 (22 anos): beta_interrupcao + beta_tempo_pos * 22

efeito_imediato <- beta_interrupcao
efeito_tendencia <- beta_tempo_pos
efeito_acumulado <- (beta_interrupcao + beta_tempo_pos * 22)

cat("Efeito imediato (1964):", round(efeito_imediato, 3), "óbitos por 1000 NV\n")
cat("Mudança na tendência anual:", round(efeito_tendencia, 3), "óbitos por 1000 NV/ano\n")
cat("Efeito acumulado 1964–1985:", round(efeito_acumulado, 3), "óbitos por 1000 NV\n")


# 8. Comparação com os métodos anteriores (log-linear e ARIMA)
# Você pode calcular o excesso de mortes a partir do ITS:
# Para cada ano 1964-1985, a diferença entre a projeção contrafactual e a observada
# é o efeito da interrupção.

# Supondo que você tenha o vetor 'nascimentos' para 1930‑1991
nascimentos_regime <- sum(nascimentos[all_years %in% 1964:1985])

# Efeito acumulado do ARIMAX (em óbitos por 1.000 NV)
efeito_acumulado_imr <- pmax(round(efeito_acumulado, 2), 0)  # (valor obtido do modelo)

# ----------------------------------------------------------------------------
# 9.1. CÁLCULO DO EXCESSO DE MORTALIDADE INFANTIL ANUAL – ABORDAGEM DOS COEFICIENTES
# Usando os betas da interrupção e tempo_pos, truncando negativos
# ----------------------------------------------------------------------------

# 1. Extrair coeficientes do modelo ARIMAX (sem PIB e urbanização)
# (Certifique-se de que 'modelo_arimax' é o modelo sem covariáveis)
coefs <- coef(modelo_arimax)
beta_interrupcao <- coefs["interrupcao"]   # 0.05383
beta_tempo_pos   <- coefs["tempo_pos"]     # -0.01977

# 2. Preparar vetores para os anos do regime (1964–1985)
anos_regime <- 1964:1985
nascimentos_regime <- nascimentos[all_years %in% anos_regime]  # já calculado

# 3. Calcular o efeito anual (em óbitos por 1000 NV)
efeito_anual <- beta_interrupcao + beta_tempo_pos * (0:21)  # t = 0 a 21

# 4. Truncar valores negativos para zero (se desejar apenas excesso)
efeito_excesso <- pmax(efeito_anual, 0)   # pmax = máximo paralelo (trunca em 0)

# 5. Calcular o excesso absoluto de óbitos por ano
excesso_absoluto_anual <- (efeito_excesso * nascimentos_regime) / 1000

# 6. Excesso total (soma)
excesso_total <- sum(excesso_absoluto_anual)

# 7. (Opcional) Calcular também a redução (valores negativos)
efeito_reducao <- pmin(efeito_anual, 0)   # valores negativos
reducao_absoluta_anual <- (abs(efeito_reducao) * nascimentos_regime) / 1000
reducao_total <- sum(reducao_absoluta_anual)

# 8. Exibir resultados
cat("===== RESULTADOS – EXCESSO POR COEFICIENTES (TRUNCADO) =====\n")
cat(sprintf("Excesso total de óbitos (1964–1985): %0.0f\n", excesso_total))
cat(sprintf("Redução total de óbitos (1964–1985): %0.0f\n", reducao_total))
cat(sprintf("Efeito líquido (excesso - redução): %0.0f\n", excesso_total - reducao_total))

# 9. Tabela anual
tabela_excesso <- data.frame(
  Ano = anos_regime,
  Nascimentos = nascimentos_regime,
  Efeito_IMR = round(efeito_anual, 4),
  Excesso_IMR = round(efeito_excesso, 4),
  Excesso_Obitos = round(excesso_absoluto_anual, 0),
  Reducao_Obitos = round(reducao_absoluta_anual, 0)
)
print(tabela_excesso)

# 10. Gráfico do excesso anual
library(ggplot2)
ggplot(tabela_excesso, aes(x = Ano, y = Excesso_Obitos / 1000)) +
  geom_col(fill = "red", alpha = 0.6) +
  labs(title = "Annual excess of newborn deaths under Brazilian Military Dictatorship 1964-1985",
       subtitle = "Interrupted time series model",
       x = "Ano", y = "excess (thousands of deaths)") +
  theme_minimal()

# =============================================================================
# ANÁLISE DE SENSIBILIDADE – EXCESSO DE MORTALIDADE NO REGIME MILITAR (1964-1985)
# =============================================================================
# Parâmetros variados:
#   - Método de interpolação da IMR: "fmm" ou "natural"
#   - Início do período de treino: 1940, 1944, 1949
#   - Fim do período de treino: 1960, 1963
#   - Multiplicador da correção migratória: 1,0, 1,5, 2,0
# =============================================================================

corrigir_censo_var <- function(df_ano, ano, 
                               omissao_liquida = NULL,   # se NULL, usa a tabela fixa
                               p_men_var = p_men, 
                               p_women_var = p_women) {
  # Se não fornecida, usa a taxa da tabela global
  if (is.null(omissao_liquida)) {
    omissao_ano <- omissao$omissao_liquida[omissao$ano == ano]
  } else {
    omissao_ano <- omissao_liquida
  }
  if (length(omissao_ano) == 0) stop("Ano sem taxa de omissão")
  
  C_total <- 1 / (1 - omissao_ano)
  
  col_homens <- paste0("homens ", age_groups)
  col_mulheres <- paste0("mulheres ", age_groups)
  
  pop_homens <- as.numeric(df_ano[col_homens])
  pop_mulheres <- as.numeric(df_ano[col_mulheres])
  
  f0_homens <- 1 / (1 - p_men_var)
  f0_mulheres <- 1 / (1 - p_women_var)
  
  pop_corr_homens <- pop_homens * f0_homens
  pop_corr_mulheres <- pop_mulheres * f0_mulheres
  
  total_bruto <- sum(pop_homens) + sum(pop_mulheres)
  total_corr_prelim <- sum(pop_corr_homens) + sum(pop_corr_mulheres)
  
  s <- (total_bruto * C_total) / total_corr_prelim
  
  f_homens <- f0_homens * s
  f_mulheres <- f0_mulheres * s
  
  df_ano[col_homens] <- as.list(pop_homens * f_homens)
  df_ano[col_mulheres] <- as.list(pop_mulheres * f_mulheres)
  df_ano$população <- sum(df_ano[col_homens]) + sum(df_ano[col_mulheres])
  
  return(df_ano)
}

calcular_excesso_completo <- function(imr_obs, imr_contra, nascimentos, modelo_cdr, obitos_obs, pop_regime) {
  # Excesso de IMR (óbitos infantis)
  excesso_imr <- imr_obs - imr_contra$central
  excesso_pos_imr <- pmax(excesso_imr, 0)
  excesso_infantil <- sum(excesso_pos_imr * nascimentos / 1000)  # em óbitos absolutos
  
  # Excesso de mortalidade geral (usando modelo CDR)
  CDR_contra <- forecast(modelo_cdr, xreg = imr_contra$central)
  obitos_esperados <- CDR_contra$mean * pop_regime
  excesso_geral <- sum(obitos_obs - obitos_esperados)
  
  return(list(excesso_infantil = excesso_infantil,
              excesso_geral = excesso_geral))
}

# =============================================================================
# ANÁLISE DE SENSIBILIDADE COM CORREÇÃO CENSITÁRIA VARIÁVEL
# =============================================================================

# Carregar pacotes adicionais (já carregados, mas por segurança)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(forecast)
library(splines)
library(lmtest)
library(sandwich)

# -----------------------------------------------------------------------------
# 0. DEFINIR CENÁRIOS DE CORREÇÃO
# -----------------------------------------------------------------------------
# Dados de omissão e erro total (fornecidos)
omissao_base <- data.frame(
  ano = c(1930, 1940, 1950, 1960, 1970, 1980, 1991),
  omissao_liquida = c(0.036, 0.036, 0.034, 0.033, 0.035, 0.023, 0.024),
  erro_total = c(0.076, 0.071, 0.067, 0.073, 0.043, 0.047, 0.058) # para os anos disponíveis, ajuste os que faltam
)

# Para anos sem erro_total (1930), usamos o mesmo de 1940
omissao_base$erro_total[omissao_base$ano == 1930] <- 0.076

# Definir cenários:
# - Baixo: usa omissao_liquida (menor correção)
# - Central: usa omissao_liquida (já é o central)
# - Alto: usa erro_total (maior correção)
cenarios_correcao <- list(
  baixo  = omissao_base$omissao_liquida,
  central = omissao_base$omissao_liquida * 1.25,
  alto   = omissao_base$omissao_liquida * 1.5
)

# Para variar também o padrão etário, poderíamos criar fatores multiplicadores:
# fator_idade <- c(0.8, 1.0, 1.2) e ajustar p_men e p_women.

# -----------------------------------------------------------------------------
# 1. FUNÇÃO PARA PREPARAR DADOS CORRIGIDOS COM DADO CENÁRIO
# -----------------------------------------------------------------------------
preparar_dados_corrigidos <- function(omissao_vec, p_men_use = p_men, p_women_use = p_women) {
  # Lê os dados brutos
  df_temp <- read_excel(path = arquivo, sheet = "BR_1950_2022")
  
  # Converte para numérico (usando a mesma função to_numeric)
  df_temp <- df_temp %>%
    mutate(across(any_of(colunas_populacionais), to_numeric)) %>%
    mutate(ano = as.numeric(ano))
  
  # Aplica correção com os parâmetros fornecidos para cada ano censitário
  for (ano in census_years) {
    idx <- which(df_temp$ano == ano)
    if (length(idx) == 1) {
      # Encontra a taxa de omissão correspondente a este ano
      omissao_ano <- omissao_vec[omissao_base$ano == ano]
      if (length(omissao_ano) == 0) stop("Ano sem taxa")
      df_temp[idx, ] <- corrigir_censo_var(df_temp[idx, ], ano, 
                                           omissao_liquida = omissao_ano,
                                           p_men_var = p_men_use,
                                           p_women_var = p_women_use)
    }
  }
  
  # Extrai os dados censitários corrigidos
  df_census_temp <- df_temp %>% filter(ano %in% census_years) %>% arrange(ano)
  
  # Interpolações (pop_total, pop_urbana, fem_pop, etc.)
  pop_census_temp <- df_census_temp$população
  urb_census_temp <- df_census_temp$pop_urbana
  pop_total_temp <- exp(approx(census_years, log(pop_census_temp), xout = all_years)$y)
  pop_urbana_temp <- exp(approx(census_years, log(urb_census_temp), xout = all_years)$y)
  
  # Interpolação feminina
  fem_pop_interp_temp <- matrix(NA, nrow = length(all_years), ncol = length(fem_groups))
  colnames(fem_pop_interp_temp) <- fem_groups
  for (i in seq_along(fem_groups)) {
    vals <- df_census_temp[[fem_groups[i]]]
    fem_pop_interp_temp[, i] <- exp(approx(census_years, log(vals), xout = all_years)$y)
  }
  
  # TFT e nascimentos
  TFT_anual_temp <- df_temp %>% filter(ano %in% all_years) %>% pull(fecundidade)
  nascimentos_temp <- numeric(length(all_years))
  for (yr in seq_along(all_years)) {
    nascimentos_temp[yr] <- TFT_anual_temp[yr] * sum(pesos_fec * fem_pop_interp_temp[yr, ] / widths)
  }
  
  # IMR (usando spline, mas pode ser função do método; aqui fixamos "fmm")
  imr_raw_temp <- df_temp %>%
    dplyr::select(ano, mortalidade_infantil) %>%
    filter(!is.na(mortalidade_infantil)) %>%
    arrange(ano)
  imr_interp_temp <- spline(imr_raw_temp$ano, imr_raw_temp$mortalidade_infantil,
                            xout = 1930:2022, method = "fmm")$y
  
  # Retorna uma lista com tudo que as funções auxiliares precisam
  return(list(
    df_raw = df_temp,
    df_census = df_census_temp,
    pop_total = pop_total_temp,
    pop_urbana = pop_urbana_temp,
    fem_pop_interp = fem_pop_interp_temp,
    nascimentos = nascimentos_temp,
    imr_interp = imr_interp_temp
  ))
}

# -----------------------------------------------------------------------------
# 2. AJUSTAR AS FUNÇÕES AUXILIARES PARA RECEBER OS DADOS PREPARADOS
# -----------------------------------------------------------------------------
# As funções interpolar_imr, calcular_obitos_anuais, etc., atualmente usam 
# objetos globais (df_raw, pop_total, nascimentos). Vamos reescrevê-las para 
# receber os dados como argumentos.


# (Aqui você pode manter as funções originais e simplesmente sobrescrever os 
# objetos globais dentro do loop, mas isso é arriscado. O mais seguro é 
# criar versões que recebam os dados.)

# Para simplificar, vou mostrar como modificar o loop para reatribuir os 
# objetos globais a cada iteração (menos elegante, mas funcional).

# -----------------------------------------------------------------------------
# 3. EXECUÇÃO DA ANÁLISE DE SENSIBILIDADE COM CENÁRIOS DE CORREÇÃO
# -----------------------------------------------------------------------------

# Parâmetros fixos (além dos que já variam)
param_grid <- expand.grid(
  imr_method = c("fmm", "natural"),
  train_start = c(1940, 1944),
  train_end   = c(1960, 1963),
  mig_mult    = c(1.0, 1.5, 2.0),
  correcao_cenario = c("baixo", "central", "alto"),  # novo parâmetro
  stringsAsFactors = FALSE
) %>% filter(train_start < train_end)

resultados_sens <- data.frame()

for (i in 1:nrow(param_grid)) {
  params <- param_grid[i, ]
  cat(sprintf("\nProcessando combinação %d/%d: method=%s, train=%d-%d, mig=%.1f, correção=%s\n",
              i, nrow(param_grid), params$imr_method, params$train_start, 
              params$train_end, params$mig_mult, params$correcao_cenario))
  
  resultado_linha <- tryCatch({
    # 1. Obter a taxa de omissão para o cenário
    omissao_vec <- cenarios_correcao[[params$correcao_cenario]]
    
    # 2. Preparar dados corrigidos (substitui os objetos globais)
    dados_corrigidos <- preparar_dados_corrigidos(omissao_vec)
    
    # 3. Sobrescrever objetos globais com os corrigidos (para usar as funções existentes)
    #    Note: isso é necessário porque as funções auxiliares foram escritas 
    #    usando variáveis globais. Em uma versão final, refatore-as.
    df_raw <- dados_corrigidos$df_raw
    df_census <- dados_corrigidos$df_census
    pop_total <- dados_corrigidos$pop_total
    pop_urbana <- dados_corrigidos$pop_urbana
    fem_pop_interp <- dados_corrigidos$fem_pop_interp
    nascimentos <- dados_corrigidos$nascimentos
    imr_interp <- dados_corrigidos$imr_interp
    
    # 4. Executar as etapas de cálculo (usando as funções auxiliares já definidas)
    #    Nota: as funções interpolar_imr, calcular_obitos_anuais, etc. usam 
    #    as variáveis globais recém-atualizadas.
    
    # Interpolação da IMR (se necessário, mas já temos imr_interp)
    # Aqui, se quisermos variar o método, precisamos reinterpolar.
    if (params$imr_method != "fmm") {
      imr_interp <- interpolar_imr(params$imr_method)  # esta função usa df_raw global
    }
    
    # Óbitos anuais
    obitos_anuais <- calcular_obitos_anuais(params$mig_mult, imr_interp)
    
    # Modelo CDR
    modelo_cdr <- ajustar_modelo_cdr(obitos_anuais, imr_interp)
    
    # Projeções contrafactuais
    anos_regime <- 1964:1985
    imr_obs <- imr_interp[1930:2022 %in% anos_regime]
    pop_regime <- pop_total[all_years %in% anos_regime]
    obitos_obs <- obitos_anuais[all_years %in% anos_regime]
    nasc_regime <- nascimentos[all_years %in% anos_regime]
    
    contra_log <- projetar_imr_contrafactual(imr_interp, params$train_start, params$train_end, "loglinear")
    contra_arima <- projetar_imr_contrafactual(imr_interp, params$train_start, params$train_end, "arima")
    
    exc_log <- calcular_excesso_completo(imr_obs, contra_log, nasc_regime, modelo_cdr, obitos_obs, pop_regime)
    exc_arima <- calcular_excesso_completo(imr_obs, contra_arima, nasc_regime, modelo_cdr, obitos_obs, pop_regime)
    
    data.frame(
      imr_method = params$imr_method,
      train_start = params$train_start,
      train_end = params$train_end,
      mig_mult = params$mig_mult,
      correcao_cenario = params$correcao_cenario,
      excesso_infantil_log = exc_log$excesso_infantil,
      excesso_geral_log = exc_log$excesso_geral,
      excesso_infantil_arima = exc_arima$excesso_infantil,
      excesso_geral_arima = exc_arima$excesso_geral
    )
  }, error = function(e) {
    cat(sprintf("  ERRO: %s\n", e$message))
    data.frame(
      imr_method = params$imr_method,
      train_start = params$train_start,
      train_end = params$train_end,
      mig_mult = params$mig_mult,
      correcao_cenario = params$correcao_cenario,
      excesso_infantil_log = NA,
      excesso_geral_log = NA,
      excesso_infantil_arima = NA,
      excesso_geral_arima = NA
    )
  })
  
  resultados_sens <- rbind(resultados_sens, resultado_linha)
}

# -----------------------------------------------------------------------------
# 4. ANÁLISE DOS RESULTADOS (INCLUINDO O EFEITO DA CORREÇÃO)
# -----------------------------------------------------------------------------

# Agora você pode agrupar por correcao_cenario para ver o impacto.
summary_correcao <- resultados_sens %>%
  group_by(correcao_cenario) %>%
  summarise(
    media_geral_log = mean(excesso_geral_log, na.rm = TRUE),
    sd_geral_log = sd(excesso_geral_log, na.rm = TRUE),
    .groups = "drop"
  )
print(summary_correcao)

# Gráfico de barras do excesso geral por cenário de correção
ggplot(summary_correcao, aes(x = correcao_cenario, y = media_geral_log, fill = correcao_cenario)) +
  geom_col() +
  geom_errorbar(aes(ymin = media_geral_log - sd_geral_log, 
                    ymax = media_geral_log + sd_geral_log), width = 0.2) +
  labs(title = "Excesso de mortalidade geral por cenário de correção censitária",
       y = "Excesso (óbitos)", x = "Cenário de correção") +
  theme_minimal()

# Você também pode refazer os gráficos de tornado e boxplots incluindo correcao_cenario como fator.

# -----------------------------------------------------------------------------
# 5. SALVAR RESULTADOS
# -----------------------------------------------------------------------------
write.csv(resultados_sens, "resultados_sensibilidade_com_correcao.csv", row.names = FALSE)
saveRDS(resultados_sens, "resultados_sensibilidade_com_correcao.rds")

# =============================================================================
# VISUALIZAÇÃO COMPLETA DOS RESULTADOS DA ANÁLISE DE SENSIBILIDADE (CORRIGIDA)
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(viridis)
library(ggpubr)

# Certifique-se de que 'resultados_sens' existe
# resultados_sens <- readRDS("resultados_sensibilidade_com_correcao.rds")

# -----------------------------------------------------------------------------
# 1. TABELAS RESUMO (já funcionam)
# -----------------------------------------------------------------------------

summary_geral <- resultados_sens %>%
  summarise(
    n = n(),
    media_infantil = mean(excesso_infantil_log, na.rm = TRUE),
    sd_infantil = sd(excesso_infantil_log, na.rm = TRUE),
    min_infantil = min(excesso_infantil_log, na.rm = TRUE),
    max_infantil = max(excesso_infantil_log, na.rm = TRUE),
    media_geral = mean(excesso_geral_log, na.rm = TRUE),
    sd_geral = sd(excesso_geral_log, na.rm = TRUE),
    min_geral = min(excesso_geral_log, na.rm = TRUE),
    max_geral = max(excesso_geral_log, na.rm = TRUE)
  )
print("Resumo geral:")
print(summary_geral)

resumo_por_param <- bind_rows(
  resultados_sens %>% group_by(imr_method) %>% summarise(media = mean(excesso_geral_log, na.rm = TRUE), sd = sd(excesso_geral_log, na.rm = TRUE), .groups = "drop") %>% mutate(param = "imr_method", level = imr_method),
  resultados_sens %>% group_by(train_start) %>% summarise(media = mean(excesso_geral_log, na.rm = TRUE), sd = sd(excesso_geral_log, na.rm = TRUE), .groups = "drop") %>% mutate(param = "train_start", level = as.character(train_start)),
  resultados_sens %>% group_by(train_end) %>% summarise(media = mean(excesso_geral_log, na.rm = TRUE), sd = sd(excesso_geral_log, na.rm = TRUE), .groups = "drop") %>% mutate(param = "train_end", level = as.character(train_end)),
  resultados_sens %>% group_by(mig_mult) %>% summarise(media = mean(excesso_geral_log, na.rm = TRUE), sd = sd(excesso_geral_log, na.rm = TRUE), .groups = "drop") %>% mutate(param = "mig_mult", level = as.character(mig_mult)),
  resultados_sens %>% group_by(correcao_cenario) %>% summarise(media = mean(excesso_geral_log, na.rm = TRUE), sd = sd(excesso_geral_log, na.rm = TRUE), .groups = "drop") %>% mutate(param = "correcao_cenario", level = correcao_cenario)
)
print("Resumo por parâmetro:")
print(resumo_por_param)

# Top e bottom 10 (funciona)
resultados_ordenados <- resultados_sens %>% arrange(desc(excesso_geral_log)) %>% mutate(rank = row_number())
top10 <- head(resultados_ordenados, 10)
bottom10 <- tail(resultados_ordenados, 10)
print("Top 10:")
print(top10[, c("imr_method", "train_start", "train_end", "mig_mult", "correcao_cenario", "excesso_geral_log")])
print("Bottom 10:")
print(bottom10[, c("imr_method", "train_start", "train_end", "mig_mult", "correcao_cenario", "excesso_geral_log")])

# -----------------------------------------------------------------------------
# 2. GRÁFICOS DE EFEITO PRINCIPAL (funciona)
# -----------------------------------------------------------------------------

plot_efeito_principal <- function(data, param_col, label) {
  data %>%
    group_by({{ param_col }}) %>%
    summarise(media = mean(excesso_geral_log, na.rm = TRUE),
              sd = sd(excesso_geral_log, na.rm = TRUE),
              .groups = "drop") %>%
    ggplot(aes(x = as.factor({{ param_col }}), y = media, fill = as.factor({{ param_col }}))) +
    geom_col(alpha = 0.7) +
    geom_errorbar(aes(ymin = media - sd, ymax = media + sd), width = 0.2) +
    labs(title = paste("Efeito de", label, "no excesso geral"),
         x = label, y = "Excesso geral médio (óbitos)") +
    theme_minimal() +
    theme(legend.position = "none")
}

p_imr <- plot_efeito_principal(resultados_sens, imr_method, "Método IMR")
p_start <- plot_efeito_principal(resultados_sens, train_start, "Início do treino")
p_end <- plot_efeito_principal(resultados_sens, train_end, "Fim do treino")
p_mig <- plot_efeito_principal(resultados_sens, mig_mult, "Multiplicador migratório")
p_correcao <- plot_efeito_principal(resultados_sens, correcao_cenario, "Cenário de correção")

ggarrange(p_imr, p_start, p_end, p_mig, p_correcao, ncol = 3, nrow = 2, common.legend = FALSE)
ggsave("efeitos_principais.png", width = 12, height = 8)

# -----------------------------------------------------------------------------
# 3. BOXPLOTS CORRIGIDOS (com conversão para character)
# -----------------------------------------------------------------------------

resultados_long <- resultados_sens %>%
  mutate(across(c(imr_method, train_start, train_end, mig_mult, correcao_cenario), as.character)) %>%
  pivot_longer(cols = c(imr_method, train_start, train_end, mig_mult, correcao_cenario),
               names_to = "parametro", values_to = "valor") %>%
  mutate(valor = as.factor(valor))

ggplot(resultados_long, aes(x = valor, y = excesso_geral_log / 1e6, fill = valor)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~ parametro, scales = "free_x") +
  labs(title = "Distribuição do excesso geral (milhões) por parâmetro",
       x = "Nível do parâmetro", y = "Excesso geral (milhões de óbitos)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("boxplots_por_parametro.png", width = 10, height = 6)

# -----------------------------------------------------------------------------
# 4. GRÁFICOS ADICIONAIS PARA EXPLORAR O EFEITO DA CORREÇÃO
# -----------------------------------------------------------------------------

# Boxplot apenas por correcao_cenario (com jitter)
p_box_correcao <- ggplot(resultados_sens, aes(x = correcao_cenario, y = excesso_geral_log / 1e6, fill = correcao_cenario)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(title = "Excesso geral por cenário de correção censitária",
       x = "Cenário de correção", y = "Excesso geral (milhões de óbitos)") +
  theme_minimal()
print(p_box_correcao)
ggsave("boxplot_correcao.png", width = 6, height = 5)

# Interação correcao vs mig_mult
interacao_mig_cor <- resultados_sens %>%
  group_by(correcao_cenario, mig_mult) %>%
  summarise(media = mean(excesso_geral_log), .groups = "drop") %>%
  ggplot(aes(x = as.factor(mig_mult), y = media / 1e6, color = correcao_cenario, group = correcao_cenario)) +
  geom_line(size = 1.2) + geom_point(size = 3) +
  labs(title = "Interação entre correção censitária e multiplicador migratório",
       x = "Multiplicador migratório", y = "Excesso geral médio (milhões)") +
  theme_minimal()
print(interacao_mig_cor)
ggsave("interacao_mig_correcao.png", width = 7, height = 5)

# Interação correcao vs train_start
interacao_start_cor <- resultados_sens %>%
  group_by(correcao_cenario, train_start) %>%
  summarise(media = mean(excesso_geral_log), .groups = "drop") %>%
  ggplot(aes(x = as.factor(train_start), y = media / 1e6, color = correcao_cenario, group = correcao_cenario)) +
  geom_line(size = 1.2) + geom_point(size = 3) +
  labs(title = "Interação entre correção censitária e início do treino",
       x = "Início do treino", y = "Excesso geral médio (milhões)") +
  theme_minimal()
print(interacao_start_cor)
ggsave("interacao_start_correcao.png", width = 7, height = 5)

# -----------------------------------------------------------------------------
# 5. HEATMAPS para cada cenário de correção
# -----------------------------------------------------------------------------

for (cen in unique(resultados_sens$correcao_cenario)) {
  p <- resultados_sens %>%
    filter(correcao_cenario == cen) %>%
    group_by(imr_method, train_start, train_end) %>%
    summarise(media = mean(excesso_geral_log), .groups = "drop") %>%
    ggplot(aes(x = as.factor(train_start), y = as.factor(train_end), fill = media / 1e6)) +
    geom_tile() +
    geom_text(aes(label = round(media / 1e6, 2)), size = 3) +
    facet_wrap(~ imr_method) +
    scale_fill_viridis_c(option = "plasma") +
    labs(title = paste("Excesso geral (milhões) - Cenário", cen),
         x = "Início do treino", y = "Fim do treino") +
    theme_minimal()
  print(p)
  ggsave(paste0("heatmap_", cen, ".png"), width = 8, height = 5)
}

# -----------------------------------------------------------------------------
# 6. ANÁLISE DE VARIÂNCIA COM INTERAÇÕES (para quantificar influência)
# -----------------------------------------------------------------------------

resultados_sens_fact <- resultados_sens %>%
  mutate(imr_method = as.factor(imr_method),
         train_start = as.factor(train_start),
         train_end = as.factor(train_end),
         mig_mult = as.factor(mig_mult),
         correcao_cenario = as.factor(correcao_cenario))

# Modelo com interações de primeira ordem
modelo_interacao <- lm(excesso_geral_log ~ (imr_method + train_start + train_end + mig_mult) * correcao_cenario,
                       data = resultados_sens_fact)
anova_interacao <- anova(modelo_interacao)
print("Tabela ANOVA com interações:")
print(anova_interacao)

# Contribuição relativa (soma de quadrados)
sq <- anova_interacao$`Sum Sq`
contrib <- sq / sum(sq) * 100
names(contrib) <- rownames(anova_interacao)
contrib_df <- data.frame(Fator = names(contrib), Contribuicao = contrib)
print("Contribuição relativa de cada termo (%):")
print(contrib_df)

# Gráfico da contribuição
ggplot(contrib_df, aes(x = reorder(Fator, -Contribuicao), y = Contribuicao, fill = Fator)) +
  geom_col(alpha = 0.7) +
  labs(title = "Contribuição relativa dos termos (incluindo interações)",
       x = "Termo do modelo", y = "Contribuição (%)") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("contribuicao_interacoes.png", width = 10, height = 6)

# -----------------------------------------------------------------------------
# 7. SALVAR TODOS OS GRÁFICOS EM UM PDF
# -----------------------------------------------------------------------------

pdf("sensibilidade_graficos_completos.pdf", width = 10, height = 7)
print(p_imr)
print(p_start)
print(p_end)
print(p_mig)
print(p_correcao)
print(ggplot(resultados_long, aes(x = valor, y = excesso_geral_log / 1e6, fill = valor)) +
        geom_boxplot(alpha = 0.6) + facet_wrap(~ parametro, scales = "free_x") +
        labs(title = "Boxplots por parâmetro", y = "Excesso geral (milhões)") +
        theme_minimal() + theme(legend.position = "none"))
print(p_box_correcao)
print(interacao_mig_cor)
print(interacao_start_cor)
# Repetir heatmaps (já foram impressos, mas podemos incluir novamente)
for (cen in unique(resultados_sens$correcao_cenario)) {
  p <- resultados_sens %>%
    filter(correcao_cenario == cen) %>%
    group_by(imr_method, train_start, train_end) %>%
    summarise(media = mean(excesso_geral_log), .groups = "drop") %>%
    ggplot(aes(x = as.factor(train_start), y = as.factor(train_end), fill = media / 1e6)) +
    geom_tile() + geom_text(aes(label = round(media / 1e6, 2)), size = 3) +
    facet_wrap(~ imr_method) + scale_fill_viridis_c(option = "plasma") +
    labs(title = paste("Excesso geral (milhões) - Cenário", cen),
         x = "Início do treino", y = "Fim do treino") + theme_minimal()
  print(p)
}
print(ggplot(contrib_df, aes(x = reorder(Fator, -Contribuicao), y = Contribuicao, fill = Fator)) +
        geom_col(alpha = 0.7) +
        labs(title = "Contribuição relativa dos termos (incluindo interações)") +
        theme_minimal() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)))
dev.off()
cat("Todos os gráficos salvos em 'sensibilidade_graficos_completos.pdf'\n")
