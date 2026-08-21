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
library(tidyverse)
library(lmtest)
library(sandwich)
library(tseries)


# =============================================================================
# Correção de erros censitários com decaimento exponencial do erro por idade
# =============================================================================

age_groups <- c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos",
                "20 a 24 anos", "25 a 29 anos", "30 a 39 anos", "40 a 49 anos",
                "50 a 59 anos", "60 a 69 anos", "70 ou mais")

ajuste_ref <- c(0.058, 0.036, 0.083)

mat_p_men <- cbind(
  c( 0.042,  0.021,  0.005, -0.002, -0.005, -0.008, -0.010, -0.005,  0.000,  0.000,  0.000),
  c( 0.108,  0.032,  0.005,  0.000, -0.005, -0.012, -0.018, -0.008, -0.002,  0.000,  0.002),
  c( 0.125,  0.065,  0.038,  0.021,  0.008, -0.005, -0.022, -0.015, -0.008, -0.002,  0.000)
)

mat_p_women <- cbind(
  c( 0.035,  0.018,  0.002, -0.005, -0.012, -0.015, -0.010, -0.005,  0.000,  0.000,  0.005),
  c( 0.051,  0.020,  0.005,  0.000, -0.008, -0.015, -0.012, -0.005, -0.002,  0.000,  0.002),
  c( 0.032, -0.018, -0.031, -0.042, -0.061, -0.048, -0.035, -0.012, -0.005, -0.002,  0.000)
)

omissao <- data.frame(
  ano = c(1930, 1940, 1950, 1960, 1970, 1980, 1991, 2000, 2010, 2022),
  ajuste_pop = c(0.087, 0.081, 0.075, 0.069, 0.073, 0.043, 0.047, 0.058, 0.036, 0.083
  )
)

# Função auxiliar usando decaimento exponencial modelado com taxa de decaimento (lambda)
obter_padrao_exponencial <- function(ajuste_alvo) {
  p_m <- numeric(length(age_groups))
  p_w <- numeric(length(age_groups))
  
  for(i in seq_along(age_groups)) {
    # Suavização baseada em relação exponencial/linear mista em função do ajuste macro
    fit_m <- lm(mat_p_men[i, ] ~ ajuste_ref)
    fit_w <- lm(mat_p_women[i, ] ~ ajuste_ref)
    
    val_m <- predict(fit_m, newdata = data.frame(ajuste_ref = ajuste_alvo))
    val_w <- predict(fit_w, newdata = data.frame(ajuste_ref = ajuste_alvo))
    
    # Aplicando fator de amortecimento/decaimento exponencial nas pontas (ex: idade 0-4 e 70+)
    # para garantir que o erro decaia suavemente rumo às assíntotas sem saltos bruscos
    if(i <= 3) {
      # Fator de decaimento exponencial estrito para as primeiras idades infantis
      fator_exp <- exp(-0.1 * (i - 1))
      p_m[i] <- val_m * fator_exp
      p_w[i] <- val_w * fator_exp
    } else if(i >= 9) {
      # Amortecimento assintótico para idades avançadas
      p_m[i] <- val_m * exp(-0.05 * (i - 9))
      p_w[i] <- val_w * exp(-0.05 * (i - 9))
    } else {
      p_m[i] <- val_m
      p_w[i] <- val_w
    }
  }
  
  return(list(p_men = p_m, p_women = p_w))
}

# Função principal de correção atualizada
corrigir_censo <- function(df_ano, ano) {
  omissao_ano <- omissao$ajuste_pop[omissao$ano == ano]
  if (length(omissao_ano) == 0) stop("Ano sem taxa de omissão cadastrada.")
  
  C_total <- 1 / (1 - omissao_ano)
  
  padroes <- obter_padrao_exponencial(omissao_ano)
  p_men_ano <- padroes$p_men
  p_women_ano <- padroes$p_women
  
  col_homens <- paste0("homens ", age_groups)
  col_mulheres <- paste0("mulheres ", age_groups)
  
  pop_homens <- as.numeric(df_ano[col_homens])
  pop_mulheres <- as.numeric(df_ano[col_mulheres])
  
  f0_homens <- 1 / (1 - p_men_ano)
  f0_mulheres <- 1 / (1 - p_women_ano)
  
  pop_corr_homens <- pop_homens * f0_homens
  pop_corr_mulheres <- pop_mulheres * f0_mulheres
  
  total_bruto <- sum(pop_homens) + sum(pop_mulheres)
  total_corr_prelim <- sum(pop_corr_homens) + sum(pop_corr_mulheres)
  
  s <- (total_bruto * C_total) / total_corr_prelim
  
  f_homens <- f0_homens * s
  f_mulheres <- f0_mulheres * s
  
  df_ano[col_homens] <- as.list(pop_homens * f_homens)
  df_ano[col_mulheres] <- as.list(pop_mulheres * f_mulheres)
  df_ano$população <- sum(as.numeric(df_ano[col_homens])) + sum(as.numeric(df_ano[col_mulheres]))
  
  corrigido <- df_ano
  return(corrigido)
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

# reconstruir óbitos

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
coint.test(dados_pre$CDR, dados_pre$IMR + dados_pre$ano)

plot(y = dados_pre$CDR, x = dados_pre$IMR)
plot(y = dados_pre$CDR, x = dados_pre$ano, type = "l")
plot(x = dados_pre$CDR, y = dados_pre$IMR)

# como a cointegração não funcionou, optei por usar um arima automático mantendo 
# a mortalidade infantil como preditora exógena
modelo_cdr_imr <- auto.arima(dados_pre$CDR, xreg = dados_pre$IMR)

summary(modelo_cdr_imr)

coeftest(modelo_cdr_imr)

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
coint.test(log(imr_hist$IMR), imr_hist$ano)
coint.test(log(imr_hist$IMR), imr_hist$ano + imr_hist$ano^2 + imr_hist$ano^3 + imr_hist$ano^4 + imr_hist$ano^5)


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
# 9. ESTUDO DE SÉRIE TEMPORAL INTERROMPIDA (ITS) – MORTALIDADE INFANTIL
# Interrupção em 1964 – início do regime militar
# =============================================================================


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
    tempo = ano - 1944,                    # variável contínua (centrado)
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
serie_ts <- ts(dados_its$imr, start = 1944, frequency = 1)

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
dados_its$ajustado_arimax <- modelo_arimax$fitted

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
efeito_acumulado <- beta_interrupcao + beta_tempo_pos * 22

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
# 9.1. CÁLCULO DO EXCESSO DE MORTALIDADE INFANTIL ANUAL (COM TESTE DE SIGNIFICÂNCIA)
# ----------------------------------------------------------------------------

library(lmtest)

# 1. Obter a tabela de coeficientes com erros padrão e p-valores do ARIMAX
tabela_coefs <- coeftest(modelo_arimax)

# Extrair estimativas e p-valores
beta_interrupcao <- tabela_coefs["interrupcao", "Estimate"]
p_interrupcao    <- tabela_coefs["interrupcao", "Pr(>|z|)"]

beta_tempo_pos   <- tabela_coefs["tempo_pos", "Estimate"]
p_tempo_pos      <- tabela_coefs["tempo_pos", "Pr(>|z|)"]

# 2. Definir nível de significância (ex: alfa = 0.05)
alpha <- 0.05

# Aplicar a condição: se não for significativo (p >= alpha), o efeito vira 0
if (p_interrupcao >= alpha) {
  cat(sprintf("Aviso: Coeficiente 'interrupcao' não significativo (p = %.3f). Tratado como 0.\n", p_interrupcao))
  beta_interrupcao <- 0
} else {
  cat(sprintf("Coeficiente 'interrupcao' significativo (p = %.3f).\n", p_interrupcao))
}

if (p_tempo_pos >= alpha) {
  cat(sprintf("Aviso: Coeficiente 'tempo_pos' não significativo (p = %.3f). Tratado como 0.\n", p_tempo_pos))
  beta_tempo_pos <- 0
} else {
  cat(sprintf("Coeficiente 'tempo_pos' significativo (p = %.3f).\n", p_tempo_pos))
}

# 3. Preparar vetores para os anos do regime (1964–1985)
anos_regime <- 1964:1985
nascimentos_regime <- nascimentos[all_years %in% anos_regime] 

# 4. Calcular o efeito anual (em óbitos por 1000 NV) usando apenas os coeficientes válidos
efeito_anual <- beta_interrupcao + beta_tempo_pos * (0:21)  # t = 0 a 21

# 5. Truncar valores negativos para zero (se desejar apenas excesso)
efeito_excesso <- pmax(efeito_anual, 0)   

# 6. Calcular o excesso absoluto de óbitos por ano
excesso_absoluto_anual <- (efeito_excesso * nascimentos_regime) / 1000

# 7. Excesso total (soma)
excesso_total <- sum(excesso_absoluto_anual)

# 8. (Opcional) Calcular também a redução (valores negativos)
efeito_reducao <- pmin(efeito_anual, 0)   
reducao_absoluta_anual <- (abs(efeito_reducao) * nascimentos_regime) / 1000
reducao_total <- sum(reducao_absoluta_anual)

# 9. Exibir resultados
cat("===== RESULTADOS – EXCESSO POR COEFICIENTES (COM FILTRO DE SIGNIFICÂNCIA) =====\n")
cat(sprintf("Excesso total de óbitos (1964–1985): %0.0f\n", excesso_total))
cat(sprintf("Redução total de óbitos (1964–1985): %0.0f\n", reducao_total))
cat(sprintf("Efeito líquido (excesso - redução): %0.0f\n", excesso_total - reducao_total))

# 10. Tabela anual
tabela_excesso <- data.frame(
  Ano = anos_regime,
  Nascimentos = nascimentos_regime,
  Mortalidade_infantil_obs = imr_interp[ind_obs],
  Efeito_IMR = round(efeito_anual, 4),
  Excesso_IMR = round(efeito_excesso, 4),
  Excesso_Obitos = round(excesso_absoluto_anual, 0),
  Reducao_Obitos = round(reducao_absoluta_anual, 0)
)
print(tabela_excesso)

# 10. Gráfico do excesso anual
library(ggplot2)
its_excess <- ggplot(tabela_excesso, aes(x = Ano, y = Excesso_Obitos / 1000)) +
  geom_col(alpha = 0.6) +
  labs(title = "Annual excess of newborn deaths under Brazilian Military Dictatorship 1964-1985",
       subtitle = "Interrupted time series model",
       x = "Ano", y = "excess (thousands of deaths)") +
  theme_minimal()

print(its_excess)

ggsave("ITS_excess_plot.png", plot = its_excess)

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
serie_hist <- ts(imr_hist, start = 1944, frequency = 1)

# 2. Ajustar modelo ARIMA automaticamente (seleção por AICc)
modelo_arima_pre <- auto.arima(serie_hist)  # dados anuais, sem sazonalidade

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
proj_arimax <- forecast::forecast(modelo_arima_contra, h = 22, level = 95, bootstrap = TRUE)

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
# VISUALIZAÇÃO COMPLETA DA ANÁLISE PRINCIPAL (PRÉ-SENSIBILIDADE)
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(knitr)
library(scales)

# -----------------------------------------------------------------------------
# 1. TABELA RESUMO DOS EXCESSOS (INFANTIL E GERAL) – POR MÉTODO
# -----------------------------------------------------------------------------

# Excesso infantil (log-linear) – já calculado em 7.3
# Temos: excesso_pos_acum_central, excesso_pos_acum_min, excesso_pos_acum_max (em IMR)
# E também a conversão para óbitos absolutos (usando nascimentos)
# O código original já fez: tibble(..., c(sum(nascimentos[35:56]*(excesso_pos_central/100)), ...))

# Vamos extrair esses valores do ambiente
# Se não estiverem salvos, recalcular:
if (!exists("excesso_infantil_abs_central")) {
  # Recalcular a partir dos dados existentes
  ind_regime <- all_years %in% 1964:1985
  nasc_regime <- nascimentos[ind_regime]
  
  excesso_infantil_abs_central <- sum(nasc_regime * (excesso_pos_central / 1000))  # já está em óbitos
  excesso_infantil_abs_min <- sum(nasc_regime * (excesso_pos_min / 1000))
  excesso_infantil_abs_max <- sum(nasc_regime * (excesso_pos_max / 1000))
}

# Excesso geral (log-linear) – já calculado em 7.5
# excesso_central$total, excesso_min$total, excesso_max$total

# Excesso infantil e geral pelo método ARIMA (seção 8)
# Temos: excesso_acumulado_imr (em IMR) e excesso_total_obitos (geral)
# Também temos tabela_resultados com excesso de IMR e óbitos por ano
# Para o ARIMA, o excesso infantil absoluto pode ser calculado a partir da tabela
if (exists("tabela_resultados")) {
  excesso_infantil_abs_arima <- sum(tabela_resultados$Excesso_Obitos, na.rm = TRUE)  # já é óbitos
} else {
  excesso_infantil_abs_arima <- NA
}

# Tabela de resumo
tabela_resumo <- data.frame(
  Método = c("Log-linear (central)", "Log-linear (mínimo)", "Log-linear (máximo)",
             "ARIMA (central)"),
  `Excesso infantil (óbitos)` = c(excesso_infantil_abs_central,
                                  excesso_infantil_abs_min,
                                  excesso_infantil_abs_max,
                                  excesso_infantil_abs_arima),
  `Excesso geral (óbitos)` = c(excesso_central$total,
                               excesso_min$total,
                               excesso_max$total,
                               if (exists("excesso_total_obitos")) excesso_total_obitos else NA)
)

# Arredondar
tabela_resumo <- tabela_resumo %>%
  mutate(across(where(is.numeric), ~ round(., 0)))

# Exibir no console
cat("\n========== TABELA RESUMO DOS EXCESSOS ==========\n")
print(tabela_resumo)

# -----------------------------------------------------------------------------
# 2. GRÁFICO DA IMR OBSERVADA E CONTRAFACTUAL (LOG-LINEAR)
# -----------------------------------------------------------------------------

# Dados observados (1930–1985)
dados_obs <- data.frame(
  ano = 1930:1985,
  IMR = imr_interp[1:56]  # índices 1:56 correspondem a 1930:1985
) %>% filter(!is.na(IMR))

# Dados contrafactuais (1964–1985)
dados_contra <- data.frame(
  ano = 1964:1985,
  IMR_central = as.numeric(imr_central),
  IMR_min     = as.numeric(imr_min),
  IMR_max     = as.numeric(imr_max)
)

# Combinar para ggplot
p_imr <- ggplot() +
  geom_ribbon(data = dados_contra,
              aes(x = ano, ymin = IMR_min, ymax = IMR_max),
              fill = "lightblue", alpha = 0.4) +
  geom_line(data = dados_contra,
            aes(x = ano, y = IMR_central, color = "Contrafactual"),
            size = 1.2, linetype = "dashed") +
  geom_line(data = dados_obs,
            aes(x = ano, y = IMR, color = "Observado"),
            size = 1) +
  geom_vline(xintercept = 1964, linetype = "dotted", color = "red", size = 1) +
  annotate("text", x = 1964, y = max(dados_obs$IMR, na.rm = TRUE) * 0.9,
           label = "Início do regime militar", color = "red", hjust = -0.1) +
  scale_color_manual(name = "Série",
                     values = c("Observado" = "black", "Contrafactual" = "blue")) +
  labs(title = "Mortalidade infantil (IMR) – observada e contrafactual (log-linear)",
       subtitle = "Projeção baseada na tendência 1944-1963",
       x = "Ano", y = "IMR (óbitos por 1.000 nascidos vivos)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_imr)
ggsave("imr_obs_contra_loglinear.png", p_imr, width = 8, height = 5)

# -----------------------------------------------------------------------------
# 3. GRÁFICO DO EXCESSO ANUAL DE IMR (LOG-LINEAR)
# -----------------------------------------------------------------------------

# Usar a tabela já criada: tabela_imr_excesso
p_excesso_imr <- ggplot(tabela_imr_excesso, aes(x = Ano, y = Excesso_Central)) +
  geom_col(fill = ifelse(tabela_imr_excesso$Excesso_Central > 0, "red", "blue"), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Excesso anual de mortalidade infantil (log-linear)",
       subtitle = "Diferença entre IMR observada e projetada (1964–1985)",
       x = "Ano", y = "Excesso de IMR (óbitos por 1.000 NV)") +
  theme_minimal()

print(p_excesso_imr)
ggsave("excesso_anual_imr_loglinear.png", p_excesso_imr, width = 8, height = 5)

# -----------------------------------------------------------------------------
# 4. GRÁFICO DE ÓBITOS OBSERVADOS E ESPERADOS (LOG-LINEAR)
# -----------------------------------------------------------------------------

# Dados do resultado_anual (já calculado)
dat_plot <- resultado_anual %>%
  dplyr::select(Ano, Observado, Esperado_Min, Esperado_Central, Esperado_Max) %>%
  pivot_longer(-Ano, names_to = "Cenário", values_to = "Óbitos") %>%
  mutate(`Óbitos (mil)` = `Óbitos` / 1000)

p_obitos <- ggplot(dat_plot, aes(x = Ano, y = `Óbitos (mil)`, color = Cenário, linetype = Cenário)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Observado" = "black",
                                "Esperado_Min" = "blue",
                                "Esperado_Central" = "darkgreen",
                                "Esperado_Max" = "red")) +
  scale_linetype_manual(values = c("Observado" = "solid",
                                   "Esperado_Min" = "dashed",
                                   "Esperado_Central" = "dashed",
                                   "Esperado_Max" = "dashed")) +
  labs(title = "Óbitos anuais observados e contrafactuais (log-linear)",
       subtitle = "Coorte-componente com projeção log-linear (tendência 1944-1963)",
       y = "Óbitos (milhares)",
       x = "Ano") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_obitos)
ggsave("obitos_obs_esperados_loglinear.png", p_obitos, width = 8, height = 5)

# -----------------------------------------------------------------------------
# 5. GRÁFICO DO EXCESSO ANUAL DE ÓBITOS GERAIS (LOG-LINEAR)
# -----------------------------------------------------------------------------

# Extrair excesso anual do resultado_anual
excesso_anual_geral <- resultado_anual %>%
  dplyr::select(Ano, Excesso_Central) %>%
  rename(Excesso = Excesso_Central)

p_excesso_geral <- ggplot(excesso_anual_geral, aes(x = Ano, y = Excesso / 1000)) +
  geom_col(fill = ifelse(excesso_anual_geral$Excesso > 0, "red", "blue"), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Excesso anual de mortalidade geral (log-linear)",
       subtitle = "Diferença entre óbitos observados e esperados (1964–1985)",
       x = "Ano", y = "Excesso (milhares de óbitos)") +
  theme_minimal()

print(p_excesso_geral)
ggsave("excesso_anual_geral_loglinear.png", p_excesso_geral, width = 8, height = 5)

# -----------------------------------------------------------------------------
# 6. TABELA ANUAL DETALHADA (OBSERVADO, ESPERADO, EXCESSO) - LOG-LINEAR
# -----------------------------------------------------------------------------

# Usar resultado_anual
tabela_anual_log <- resultado_anual %>%
  dplyr::select(Ano, Observado, Esperado_Central, Excesso_Central) %>%
  rename(Esperado = Esperado_Central, Excesso = Excesso_Central)

# Exibir como tabela formatada (no console)
cat("\n========== TABELA ANUAL (LOG-LINEAR) ==========\n")
print(tabela_anual_log)

# Salvar em CSV
write.csv(tabela_anual_log, "tabela_anual_loglinear.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 7. GRÁFICOS PARA O MÉTODO ARIMA (COMPARAÇÃO)
# -----------------------------------------------------------------------------

# Se existir resultado_anual_arimax, plotar
if (exists("resultado_anual_arimax")) {
  dat_plot_arimax <- resultado_anual_arimax %>%
    dplyr::select(Ano, Observado, Esperado_Min, Esperado_Central, Esperado_Max) %>%
    pivot_longer(-Ano, names_to = "Cenário", values_to = "Óbitos") %>%
    mutate(`Óbitos (mil)` = `Óbitos` / 1000)
  
  p_arimax <- ggplot(dat_plot_arimax, aes(x = Ano, y = `Óbitos (mil)`, color = Cenário, linetype = Cenário)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = c("Observado" = "black",
                                  "Esperado_Min" = "blue",
                                  "Esperado_Central" = "darkgreen",
                                  "Esperado_Max" = "red")) +
    scale_linetype_manual(values = c("Observado" = "solid",
                                     "Esperado_Min" = "dashed",
                                     "Esperado_Central" = "dashed",
                                     "Esperado_Max" = "dashed")) +
    labs(title = "Óbitos anuais observados e contrafactuais (ARIMA)",
         subtitle = "Projeção ARIMA univariada da IMR (1944-1963)",
         y = "Óbitos (milhares)",
         x = "Ano") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p_arimax)
  ggsave("obitos_obs_esperados_arima.png", p_arimax, width = 8, height = 5)
  
  # Excesso anual ARIMA
  excesso_anual_arima <- resultado_anual_arimax %>%
    dplyr::select(Ano, Excesso_Central) %>%
    rename(Excesso = Excesso_Central)
  
  p_excesso_arima <- ggplot(excesso_anual_arima, aes(x = Ano, y = Excesso / 1000)) +
    geom_col(fill = ifelse(excesso_anual_arima$Excesso > 0, "red", "blue"), alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Excesso anual de mortalidade geral (ARIMA)",
         subtitle = "Diferença entre óbitos observados e esperados (1964–1985)",
         x = "Ano", y = "Excesso (milhares de óbitos)") +
    theme_minimal()
  
  print(p_excesso_arima)
  ggsave("excesso_anual_geral_arima.png", p_excesso_arima, width = 8, height = 5)
}

# -----------------------------------------------------------------------------
# 8. COMPARAÇÃO ENTRE MÉTODOS (LOG-LINEAR vs ARIMA) – GRÁFICO DE BARRAS
# -----------------------------------------------------------------------------

# Juntar os resultados centrais de ambos os métodos
comparacao <- data.frame(
  Metodo = c("Log-linear", "ARIMA"),
  Excesso_infantil = c(excesso_infantil_abs_central,
                       if (exists("excesso_infantil_abs_arima")) excesso_infantil_abs_arima else NA),
  Excesso_geral = c(excesso_central$total,
                    if (exists("excesso_total_obitos")) excesso_total_obitos else NA)
)

# Exibir tabela
cat("\n========== COMPARAÇÃO ENTRE MÉTODOS ==========\n")
print(comparacao)

# Gráfico de barras comparativo (excesso geral)
comparacao_long <- comparacao %>%
  pivot_longer(-Metodo, names_to = "Tipo", values_to = "Excesso") %>%
  filter(!is.na(Excesso))

p_comparacao <- ggplot(comparacao_long, aes(x = Metodo, y = Excesso / 1e6, fill = Tipo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = round(Excesso / 1e6, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  labs(title = "Comparação do excesso total entre métodos",
       x = "Método", y = "Excesso (milhões de óbitos)",
       fill = "Tipo de excesso") +
  theme_minimal()

print(p_comparacao)
ggsave("comparacao_metodos.png", p_comparacao, width = 6, height = 5)

# -----------------------------------------------------------------------------
# 9. GRÁFICO DA SÉRIE DA IMR COM PROJEÇÃO ARIMA (SE DISPONÍVEL)
# -----------------------------------------------------------------------------

if (exists("projecao") && exists("imr_projetado")) {
  # Dados históricos e observados
  dados_hist <- data.frame(
    ano = 1944:1963,
    IMR = imr_interp[15:34]  # ajuste conforme necessidade
  )
  dados_obs_arima <- data.frame(
    ano = 1964:1985,
    IMR = imr_observado
  )
  dados_proj_arima <- data.frame(
    ano = 1964:1985,
    IMR = imr_projetado,
    inf = imr_projetado_inf,
    sup = imr_projetado_sup
  )
  
  p_arima_imr <- ggplot() +
    geom_line(data = dados_hist, aes(x = ano, y = IMR, color = "Histórico"), size = 1) +
    geom_line(data = dados_obs_arima, aes(x = ano, y = IMR, color = "Observado"), size = 1) +
    geom_ribbon(data = dados_proj_arima,
                aes(x = ano, ymin = inf, ymax = sup),
                fill = "lightblue", alpha = 0.4) +
    geom_line(data = dados_proj_arima,
              aes(x = ano, y = IMR, color = "Projetado (ARIMA)"),
              size = 1, linetype = "dashed") +
    geom_vline(xintercept = 1964, linetype = "dotted", color = "red") +
    annotate("text", x = 1964, y = max(dados_hist$IMR) * 0.9,
             label = "Início do regime", color = "red", hjust = -0.1) +
    scale_color_manual(values = c("Histórico" = "black",
                                  "Observado" = "darkgreen",
                                  "Projetado (ARIMA)" = "blue")) +
    labs(title = "Mortalidade infantil – ARIMA pré-1964",
         subtitle = "Projeção baseada na tendência 1944-1963",
         x = "Ano", y = "IMR (óbitos por 1.000 NV)") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p_arima_imr)
  ggsave("imr_arima_obs_contra.png", p_arima_imr, width = 8, height = 5)
}

# -----------------------------------------------------------------------------
# 10. SALVAR TODOS OS GRÁFICOS EM UM ÚNICO PDF
# -----------------------------------------------------------------------------

pdf("analise_principal_graficos_completos.pdf", width = 10, height = 7)

print(p_imr)
print(p_excesso_imr)
print(p_obitos)
print(p_excesso_geral)
if (exists("p_arimax")) print(p_arimax)
if (exists("p_excesso_arima")) print(p_excesso_arima)
if (exists("p_arima_imr")) print(p_arima_imr)
print(p_comparacao)

dev.off()
cat("\nTodos os gráficos principais foram salvos em 'analise_principal_graficos_completos.pdf'\n")

# -----------------------------------------------------------------------------
# FIM DA VISUALIZAÇÃO
# -----------------------------------------------------------------------------


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
  df_ano[col_mulheres] <- as.list(pop_mulher
