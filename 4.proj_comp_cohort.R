# =============================================================================
# Estratégia metodológica para estimação do excesso de mortalidade
# durante o regime militar brasileiro (1964-1985)
# Versão corrigida - 2026-07-05
# =============================================================================

# Carregar pacotes
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(aTSA)
library(ipeaplot)

# =============================================================================
# 1. PREPARAÇÃO ROBUSTA DOS DADOS
# =============================================================================

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

df_census <- df_raw %>%
  filter(ano %in% census_years) %>%
  arrange(ano)

pop_census <- df_census$população   # já numérico

# Interpolação linear da população total 1940-1991
all_years <- 1930:1991
pop_total <- exp(approx(census_years, log(pop_census), xout = all_years)$y)



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
imr_interp <- spline(imr_raw$ano, log(imr_raw$mortalidade_infantil),
                     xout = imr_years_all, method = "natural")$y

plot(imr_interp)

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

anos_pre <- 1940:1963
ind_pre <- all_years %in% anos_pre

dados_pre <- data.frame(
  ano = anos_pre,
  CDR = CDR_obs[ind_pre],
  IMR = imr_interp[imr_years_all %in% anos_pre]
)

modelo_cdr_imr <- lm(CDR ~ IMR, data = dados_pre)

# =============================================================================
# 7. CENÁRIOS CONTRAFACTUAIS DE IMR
# =============================================================================

# IMR histórica 1930-1960 para modelo log-linear
imr_hist <- data.frame(
  ano = seq(1945, 1963, 1),
  IMR = imr_interp[imr_years_all %in% seq(from = 1945, to = 1963, by = 1)]
)

coint.test(imr_hist$IMR, imr_hist$ano)
coint.test(imr_hist$IMR, imr_hist$ano + imr_hist$ano^2+ imr_hist$ano^3)



modelo_log <- lm(log(IMR) ~ ano, data = imr_hist)

summary(modelo_log)

anos_regime <- 1964:1985

# modelo preditivo de tendência de mortalidade

imr_model <- exp(predict(modelo_log, interval = "prediction", level = 0.95,
                           newdata = data.frame(ano = anos_regime)))
summary(imr_model)


imr_central <- imr_model[, "fit"]
imr_max     <- imr_model[, "lwr"]
imr_min     <- imr_model[, "upr"]

# Verificar
plot(anos_regime, imr_central, type = "l", ylim = range(imr_min, imr_min),
     main = "IMR contrafactual (1964-1985)", xlab = "Ano", ylab = "IMR")
lines(anos_regime, imr_min, lty = 2)
lines(anos_regime, imr_max, lty = 2)

# =============================================================================
# 8. VISUALIZAÇÃO DA MORTALIDADE INFANTIL – OBSERVADA E CONTRAFACTUAL
# =============================================================================

library(ggplot2)
library(dplyr)

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
  IMR_central = imr_central,
  IMR_min     = imr_min,
  IMR_max     = imr_max
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



# =============================================================================
# 9. CÁLCULO DO EXCESSO DE MORTALIDADE
# =============================================================================

ind_regime <- all_years %in% anos_regime
pop_regime <- pop_total[ind_regime]
obitos_regime <- obitos_anuais[ind_regime]

calcular_excesso <- function(imr_contra) {
  CDR_contra <- predict(modelo_cdr_imr, newdata = data.frame(IMR = imr_contra))
  obitos_esperados <- CDR_contra * pop_regime
  excesso <- obitos_regime - obitos_esperados
  total <- sum(excesso)
  list(excesso_anual = excesso, total = total)
}

excesso_max <- calcular_excesso(imr_max)   # queda máxima de mortalidade 
excesso_central <- calcular_excesso((imr_central)) #tendência central
excesso_min <- calcular_excesso(imr_min)   # queda mínima de mortalidade

# =============================================================================
# 10. RESULTADOS
# =============================================================================

cat("\n========================== RESULTADOS ==========================\n")
cat(sprintf("Excesso de mortalidade 1964–1985 (cenário máximo): %0.0f óbitos\n",
            excesso_max$total))
cat(sprintf("Excesso de mortalidade 1964–1985 (cenário central): %0.0f óbitos\n",
            excesso_central$total))
cat(sprintf("Excesso de mortalidade 1964–1985 (cenário mínimo): %0.0f óbitos\n",
            excesso_min$total))

# Tabela anual do excesso
resultado_anual <- data.frame(
  Ano = anos_regime,
  Observado = round(obitos_regime, 0),
  Esperado_Min = round(predict(modelo_cdr_imr, newdata = data.frame(IMR = imr_max)) * pop_regime, 0),
  Esperado_Max = round(predict(modelo_cdr_imr, newdata = data.frame(IMR = imr_min)) * pop_regime, 0),
  Esperado_Central = round(predict(modelo_cdr_imr, newdata = data.frame(IMR = imr_central))*pop_regime, 0),
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
# 11. ESTUDO DE SÉRIE TEMPORAL INTERROMPIDA (ITS) – MORTALIDADE INFANTIL
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
  ano = 1930:1985,
  imr = imr_interp[1:56]  # ajuste o índice conforme sua série
) %>%
  filter(!is.na(imr))

# 2. Criar variáveis para o modelo ITS
#    - Tempo (centrado para reduzir multicolinearidade)
#    - Interrupção (0 antes de 1964, 1 a partir de 1964)
#    - Tempo após interrupção (0 antes de 1964, (ano - 1963) a partir de 1964)

dados_its <- dados_its %>%
  mutate(
    tempo = ano - 1945,                    # variável contínua (centrado)
    interrupcao = ifelse(ano >= 1964, 1, 0),
    tempo_pos = ifelse(ano >= 1964, ano - 1963, 0)
  )

# 3. Ajuste do modelo ITS por mínimos quadrados (OLS)
modelo_its <- lm(imr ~ tempo + interrupcao + tempo_pos, data = dados_its)

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

# 6. Calcular efeitos
# Extrair coeficientes do modelo ARIMAX (ou do OLS com erros robustos)
coefs <- coef(modelo_arimax)
# Os coeficientes estão na ordem: regressores (tempo, interrupcao, tempo_pos) e AR/MA
# Identificar os índices corretos:
idx_reg <- 1:3
beta_tempo <- coefs[1]
beta_interrupcao <- coefs[2]
beta_tempo_pos <- coefs[3]

# Efeito imediato (mudança de nível em 1964): beta_interrupcao
# Efeito gradual (mudança na inclinação): beta_tempo_pos
# Efeito acumulado até 1985 (22 anos): beta_interrupcao + beta_tempo_pos * 22

efeito_imediato <- beta_interrupcao
efeito_tendencia <- beta_tempo_pos
efeito_acumulado <- beta_interrupcao + beta_tempo_pos * 22

cat("Efeito imediato (1964):", round(efeito_imediato, 2), "óbitos por 1000 NV\n")
cat("Mudança na tendência anual:", round(efeito_tendencia, 2), "óbitos por 1000 NV/ano\n")
cat("Efeito acumulado 1964–1985:", round(efeito_acumulado, 2), "óbitos por 1000 NV\n")

# 7. Gráficos

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

# 8. Comparação com os métodos anteriores (log-linear e ARIMA)
# Você pode calcular o excesso de mortes a partir do ITS:
# Para cada ano 1964-1985, a diferença entre a projeção contrafactual e a observada
# é o efeito da interrupção.

# Supondo que você tenha o vetor 'nascimentos' para 1930‑1991
nascimentos_regime <- sum(nascimentos[all_years %in% 1964:1985])

# Efeito acumulado do ARIMAX (em óbitos por 1.000 NV)
efeito_acumulado_imr <- round(efeito_acumulado, 2)  # (valor obtido do modelo)

# Excesso absoluto de óbitos
excesso_absoluto <- (efeito_acumulado_imr / 1000) * nascimentos_regime

cat(sprintf("Total de nascidos vivos 1964‑1985: %0.0f\n", nascimentos_regime))
cat(sprintf("Excesso de óbitos estimado (ARIMAX): %0.0f\n", excesso_absoluto))

# =============================================================================
# ESTIMAÇÃO DO EXCESSO DE MORTALIDADE GERAL USANDO ARIMAX (CONTRAFACTUAL)
# =============================================================================

library(forecast)
library(dplyr)

# 1. Preparar a série histórica da IMR pré-regime (1930–1963) para projeção contrafactual
anos_hist_arimax <- 1930:1963
imr_hist_ts <- ts(imr_interp[imr_years_all %in% anos_hist_arimax], start = 1930, frequency = 1)

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
  CDR_contra <- predict(modelo_cdr_imr, newdata = data.frame(IMR = imr_contra_vec))
  obitos_esperados <- CDR_contra * pop_regime
  excesso <- obitos_regime - obitos_esperados
  total <- sum(excesso)
  list(excesso_anual = excesso, total = total)
}

excesso_min_arimax     <- calcular_excesso_arimax(imr_max_arimax)
excesso_central_arimax <- calcular_excesso_arimax(imr_central_arimax)
excesso_max_arimax     <- calcular_excesso_arimax(imr_min_arimax)

# =============================================================================
# RESULTADOS DO MODELO ARIMAX
# =============================================================================

cat("\n=================== RESULTADOS: ESTIMATIVA ARIMAX ===================\n")
cat(sprintf("Excesso de mortalidade geral 1964–1985 (cenário máximo): %0.0f óbitos\n",
            excesso_max_arimax$total))
cat(sprintf("Excesso de mortalidade geral 1964–1985 (cenário central): %0.0f óbitos\n",
            excesso_central_arimax$total))
cat(sprintf("Excesso de mortalidade geral 1964–1985 (cenário mínimo): %0.0f óbitos\n",
            excesso_min_arimax$total))

# Tabela anual do excesso com ARIMAX
resultado_anual_arimax <- data.frame(
  Ano = anos_regime,
  Observado = round(obitos_regime, 0),
  Esperado_Max = round(predict(modelo_cdr_imr, newdata = data.frame(IMR = imr_max_arimax)) * pop_regime, 0),
  Esperado_Min = round(predict(modelo_cdr_imr, newdata = data.frame(IMR = imr_min_arimax)) * pop_regime, 0),
  Esperado_Central = round(predict(modelo_cdr_imr, newdata = data.frame(IMR = imr_central_arimax)) * pop_regime, 0),
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
