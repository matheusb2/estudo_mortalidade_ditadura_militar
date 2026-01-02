

# ==============================================================================
# 0. PACOTES
# ==============================================================================

library(readxl)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ipeaplot)
library(strucchange)
library(broom)
library(scales)
library(patchwork)


# ==============================================================================
# 1. DADOS
# ==============================================================================
#ATENÇÃO,SUBSTITUA ABAIXO COM O ENDEREÇO ONDE SALVOU A PLANILHA E IRÁ SALVAR A TABELA DE RESULTADOS

arquivo <- "pasta onde salvou a planilha/censos interpolados.xlsx"
arquivo_saida <- "pasta onde será salvo a tabela de resultados/análise coortes/estudo_coorte.csv"

df_raw <- read_excel(
  path = arquivo,
  sheet = "BR_1950_2022"
)

names(df_raw)

#Os dados dos Censos por sexo e faixa etária foram interpolados ano a ano
#Censos de 1950, 1960, 1970, 1980, 1991, 2000, 2010 e 2022
#Tendência observada entre 1950 e 1960 foi extrapolada em décadas depois como contrafactual
#Déficit populacional é a diferença entre contrafactual e observado (nos Censos e interpolações)
#Dados de homicídios e mortes violentas (homicídios+suicídios+acidentes+confrontos) nacionais de 1979 a 2022
#Número de mortes violentas de 1960 a 1978 são projetados dos dados do município de SP
#números paulistas e nacionais de mortes violentas e homicídios tem R=0,97 entre 1979 e 1999
#mortes violentas no município de São Paulo e homicídios no Brasil tem 96,76% de correlação linear entre 1979 e 1999
#mortes violentas no município de São Paulo e  no Brasil tem 98,24%  de correlação linear entre 1979 e 1999
#Observações paulistas só de 1960, 1965, 1970 e 1975, depois de 1979 em diante
#Anos faltantes foram interpolados, depois projetados ao país todo por MQO simples
#Excessos de homicídios e demais mortes violentas foram calculados por diferença com tendências
#Ou seja, a lógica é: se tivesse seguido esta tendência, quantas mortes a menos haveria?
#Tendência 1: tendência linear da taxa de homicídios de 1960-1965 projetada até 1985
#Tendência 2: interpolação linear dos anos de 1966 a 1978
#Tendência 3: interpolação linear de 1966 a 1984
#Excesso de mortes=(taxa observada - taxa tendência)*população/100000

t1979_1999 <- df_raw |>
  filter(
    ano >= 1979, ano <= 1999)

cor(x = t1979_1999$`mortes violentas SP`, y = t1979_1999$`homicídios projetados`)
cor(x = t1979_1999$`mortes violentas SP`, y = t1979_1999$`mortes por causas externas projetadas`)


# ==============================================================================
# 2. CONVERTER OS DÉFICITS POPULACIONAIS EM FORMATO LONGO
# ==============================================================================

dados_long <- df_raw |>
  select(ano, matches("^déficit populacional")) |>
  pivot_longer(-ano, names_to = "variavel", values_to = "deficit") |>
  mutate(
    sexo = if_else(str_detect(variavel, "homens"), "homens", "mulheres"),
    faixa = str_extract(variavel, "\\d+ a \\d+|\\d+ ou mais")
  )

# ==============================================================================
# 3. MORTALIDADE ADULTA (25–59)
# ==============================================================================

mortes_long <- dados_long |>
  arrange(sexo, faixa, ano) |>
  group_by(sexo, faixa) |>
  mutate(mortes_incrementais = pmax(deficit - lag(deficit, default = first(deficit)), 0)) |>
  ungroup()

summary(mortes_long)

mortes_adultos <- mortes_long |>
  filter(
    ano >= 1964, ano <= 1985,
    faixa %in% c("25 a 29", "30 a 39", "40 a 49", "50 a 59")
  )

summary(mortes_adultos)

mortes_adultos_total_valor <- mortes_adultos |>
  summarise(excesso_adultos = sum(mortes_incrementais, na.rm = TRUE)) |>
  pull(excesso_adultos)

print(mortes_adultos_total_valor)

mortes_adultos_homens <- mortes_adultos |>
  filter(sexo == "homens") |>
  summarise(total = sum(mortes_incrementais, na.rm = TRUE)) |>
  pull(total)

print(mortes_adultos_homens)

desaparecimento_mulheres_adultas <- mortes_adultos_total_valor - mortes_adultos_homens

razao_homem_mulher_adultos <- mortes_adultos_homens/desaparecimento_mulheres_adultas
print(razao_homem_mulher_adultos)

print(desaparecimento_mulheres_adultas)

mortes_documentadas <- df_raw |>
  filter(ano >= 1964, ano <= 1985) |>
  summarise(total = sum(`mortos e desaparecidos documentados`, na.rm = TRUE)) |>
  pull(total)

print(mortes_documentadas)

#===============================================================================
#3.1 TESTES FORMAIS DA TRAJETÓRIA DA MORTALIDADE ADULTA
#==============================================================================

#arrumar os dados

adultos_25_59 <- df_raw |>
  transmute(
    ano,
    deficit =
      `déficit populacional homens 25 a 29 anos` +
      `déficit populacional mulheres 25 a 29 anos` +
      `déficit populacional homens 30 a 39 anos` +
      `déficit populacional mulheres 30 a 39 anos` +
      `déficit populacional homens 40 a 49 anos` +
      `déficit populacional mulheres 40 a 49 anos` +
      `déficit populacional homens 50 a 59 anos` +
      `déficit populacional mulheres 50 a 59 anos`
  )


#variável da mudança de regime de 1964 a 1985 e tempo no regime
adultos_25_59 <- adultos_25_59 |>
  mutate(
    regime = as.integer(ano >= 1964 & ano <= 1985),
    t_regime = if_else(regime == 1, ano - 1963, 0)
  )


#Quebras estruturais

modelo_adultos <- lm(deficit ~ ano, data = adultos_25_59)

summary(modelo_adultos)

quebras_adultos <- breakpoints(deficit ~ ano, data = adultos_25_59)

summary(quebras_adultos)

plot(quebras_adultos)

plot(Fstats(deficit ~ ano, data = adultos_25_59))

plot(efp(deficit ~ ano, data = adultos_25_59, type = "Rec-CUSUM"))
plot(efp(deficit ~ ano, data = adultos_25_59, type = "OLS-CUSUM"))
plot(efp(deficit ~ ano, data = adultos_25_59, type = "Rec-MOSUM"))
plot(efp(deficit ~ ano, data = adultos_25_59, type = "OLS-MOSUM"))

#As quebras estruturais serão relevantes!

#modelo de intervenção temporária
modelo_interv_adultos <- lm(
  deficit ~ ano + regime + t_regime,
  data = adultos_25_59
)

summary(modelo_interv_adultos)

#modelo de quebra dupla

adultos_25_59 <- adultos_25_59 |>
  mutate(
    t_pos_1964 = pmax(ano - 1963, 0),
    t_pos_1985 = pmax(ano - 1985, 0)
  )

modelo_dupla_quebra_adultos <- lm(
  deficit ~ ano + t_pos_1964 - t_pos_1985,
  data = adultos_25_59
)

summary(modelo_dupla_quebra_adultos)


#regressão segmentada

#Regressão segmentada pé e pós 1964


modelo_segmentado_adultos <- lm(
  deficit ~ ano * regime,
  data = adultos_25_59
)

summary(modelo_segmentado_adultos)

#teste de Bai-Perron
bp_adultos <- breakpoints(deficit ~ ano, data = adultos_25_59)

summary(bp_adultos)
plot(bp_adultos)

#teste placebo (1950-1963)
summary(
  lm(
    deficit ~ ano,
    data = subset(adultos_25_59, ano < 1964)
  )
)

summary(
  lm(
    deficit ~ ano,
    data = subset(adultos_25_59, ano > 1985)
  )
)

#Visualização

adultos_25_59 <- adultos_25_59 |>
  mutate(regime_f = factor(regime, labels = c("Fora do regime", "Durante o regime")))

ggplot(adultos_25_59, aes(x = ano, y = deficit)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    aes(color = regime_f, group = regime_f),
    method = "lm",
    se = FALSE,
    linewidth = 1.2
  ) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed") +
  scale_color_manual(values = c("grey50", "#D62728")) +
  labs(
    title = "Déficit populacional adulto (25–59) e regime autoritário",
    x = "Ano",
    y = "Déficit populacional",
    color = "Período"
  ) +
  theme_ipea()



# ==============================================================================
# 4. MORTALIDADE INFANTIL (0–4) — DIAGNÓSTICO SEM CONTABILIZAR
# ==============================================================================
#os dados do Censo não são adequados pra mensurar a mortalidade infantil
#para isso existem outros estudos da época, como:
#PAIM, Jairnison Silva; COSTA, Maria da Conceição Nascimento. Variação da mortalidade infantil em diferentes capitais brasileiras (1960-1979). Revista Baiana de Saúde Pública, v. 9, n. 3/4, p. 125-125, 1982.
#YUNES, João; RONCHEZEL, Vera Shirley Carvalho. Evolução da mortalidade geral, infantil e proporcional no Brasil. Revista de Saúde Pública, v. 8, p. 3-48, 1974.
#só vamos verificar se o choque causado pela mudança de políticas públicas devido ao golpe se confirma

infantil <- df_raw |>
  transmute(ano, deficit = `déficit populacional crianças 0 a 4 anos`)

infantil <- infantil |> mutate(
  regime = as.integer(ano >= 1964 & ano <= 1985),
  t_regime = if_else(regime == 1, ano - 1963, 0)
)

modelo_linear <- lm(deficit ~ ano, data = infantil)
quebras <- breakpoints(deficit ~ ano, data = infantil)

summary(modelo_linear)
summary(quebras)
plot(y = modelo_linear$residuals, x = df_raw$ano)

# ==============================================================================
# 4.1 DIFERENÇAS-EM-DIFERENÇAS (0–4) DA MORTALIDADE INFANTIL
# ==============================================================================

infantil_sexo <- df_raw |>
  transmute(
    ano,
    homens = `déficit populacional homens 0 a 4 anos`,
    mulheres = `déficit populacional mulheres 0 a 4 anos`
  ) |>
  pivot_longer(-ano, names_to = "sexo", values_to = "deficit") |>
  mutate(pos_1964 = as.integer(ano >= 1964))

infantil_sexo <- infantil_sexo |>
  mutate(
    regime = as.integer(ano >= 1964 & ano <= 1985),
    t_regime = if_else(regime == 1, ano - 1963, 0)
  )

modelo_did <- lm(deficit ~ sexo * regime * ano, data = infantil_sexo)

summary(modelo_did)


# ==============================================================================
# 4.2 PLACEBO (20–24) JOVENS
# ==============================================================================

adultos_placebo <- df_raw |>
  transmute(
    ano,
    homens = `déficit populacional homens 20 a 24 anos`,
    mulheres = `déficit populacional mulheres 20 a 24 anos`
  ) |>
  pivot_longer(-ano, names_to = "sexo", values_to = "deficit") |>
  mutate(pos_1964 = as.integer(ano >= 1964))

adultos_placebo <- adultos_placebo |>
  mutate(
    regime = as.integer(ano >= 1964 & ano <= 1985),
    t_regime = if_else(regime == 1, ano - 1963, 0)
  )

modelo_placebo <- lm(deficit ~ sexo * regime + ano, data = adultos_placebo)

summary(modelo_placebo)

# ==============================================================================
# 5. MORTALIDADE NA INFÂNCIA (5–9) — ESTIMÁVEL
# ==============================================================================

fecundidade_decada <- tibble::tribble( ~ano_decada, ~tft, 1940, 6.16, 1950, 6.21,
                                       1960, 6.28, 1970, 5.76, 1980, 4.35, 1991, 
                                       2.89, 2000, 2.38, 2010, 1.90, 2022, 1.55)
#Fonte: IBGE, Censo Demográfico 1940/2022. 
#Link: https://educa.ibge.gov.br/professores/educa-atividades/17658-fecundidade-no-brasil-1940-a-2010.html 
#https://agenciadenoticias.ibge.gov.br/agencia-noticias/2012-agencia-de-noticias/noticias/43837-censo-2022-mostra-um-pais-com-menos-filhos-e-menos-maes
#Acesso 26/12/2025.
#interpolação entre os anos 
fecundidade_ano <- tibble( ano = seq( min(fecundidade_decada$ano_decada), 
                                      max(fecundidade_decada$ano_decada), by = 1 ) ) |> 
  mutate( tft = approx( x = fecundidade_decada$ano_decada, y = fecundidade_decada$tft, xout = ano, rule = 2 )$y ) 

#Construir o fator de ajuste de fecundidade
#1960 é o ano de referência pré-ditadura 
tft_ref <- fecundidade_ano |> filter(ano == 1960) |> pull(tft)
fecundidade_ano <- fecundidade_ano |> mutate( fator_fecundidade = tft / tft_ref )

# Média de fecundidade no período de nascimento da coorte 5–9
fecundidade_coorte_5_9 <- fecundidade_ano |>
  mutate(
    ano_obs = ano + 7  # centro aproximado da coorte
  ) |>
  group_by(ano_obs) |>
  summarise(
    tft_media_coorte = mean(tft, na.rm = TRUE),
    .groups = "drop"
  )
summary(fecundidade_coorte_5_9)

# Referência pré-1964 (ex.: média 1955–1960)
tft_ref_5_9 <- fecundidade_ano |>
  filter(ano >= 1955, ano <= 1960) |>
  summarise(tft_ref = mean(tft)) |>
  pull(tft_ref)

fecundidade_coorte_5_9 <- fecundidade_coorte_5_9 |>
  mutate(
    fator_fecundidade_5_9 = tft_media_coorte / tft_ref
  )
summary(fecundidade_coorte_5_9)

dados_5_9_aj <- df_raw |>
  left_join(
    fecundidade_coorte_5_9,
    by = c("ano" = "ano_obs")
  ) |>
  mutate(
    `contrafactual homens 5 a 9 anos` =
      `contrafactual homens 5 a 9 anos` * fator_fecundidade_5_9,
    
    `contrafactual mulheres 5 a 9 anos` =
      `contrafactual mulheres 5 a 9 anos` * fator_fecundidade_5_9
  ) |>
  mutate(
    `déficit populacional homens 5 a 9 anos` =
      `contrafactual homens 5 a 9 anos` - `homens 5 a 9 anos`,
    
    `déficit populacional mulheres 5 a 9 anos` =
      `contrafactual mulheres 5 a 9 anos` - `mulheres 5 a 9 anos`
  )



infancia_5_9 <- dados_5_9_aj |>
  select(
    ano,
    `déficit populacional homens 5 a 9 anos`,
    `déficit populacional mulheres 5 a 9 anos`
  ) |>
  pivot_longer(-ano, names_to = "sexo", values_to = "deficit")

mortes_infancia <- infancia_5_9 |>
  arrange(sexo, ano) |>
  group_by(sexo) |>
  mutate(mortes_incrementais = pmax(deficit - lag(deficit, default = first(deficit)), 0)) |>
  ungroup()

summary(mortes_infancia)

excesso_mortalidade_infancia <- mortes_infancia |>
  filter(ano >= 1964, ano <= 1985) |>
  summarise(total = sum(mortes_incrementais, na.rm = TRUE)) |>
  pull(total)

print(excesso_mortalidade_infancia)

# criar variável de regime

#variável da mudança de regime de 1964 a 1985 e tempo no regime
mortes_infancia <- mortes_infancia |>
  mutate(
    regime = as.integer(ano >= 1964 & ano <= 1985),
    t_regime = if_else(regime == 1, ano - 1963, 0)
  )


#arrumar para a visualização

mortes_infancia <- mortes_infancia |>
  mutate(regime_f = factor(regime, labels = c("Fora do regime", "Durante o regime")))

#visualização
ggplot(mortes_infancia, aes(x = ano, y = deficit)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    aes(color = regime_f, group = regime_f),
    method = "lm",
    se = FALSE,
    linewidth = 1.2
  ) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed") +
  scale_color_manual(values = c("grey50", "#D62728")) +
  labs(
    title = "Déficit populacional na infância (5-9 anos) e regime autoritário",
    x = "Ano",
    y = "Déficit populacional",
    color = "Período"
  ) +
  theme_ipea()



#==============================================================================
# 6. MORTALIDADE DE JOVENS (15-24)
#==============================================================================

# Preparar dados (15–19 e 20–24)

excesso_15_24_long <- df_raw |>
  select(
    ano,
    `déficit populacional homens 15 a 19 anos`,
    `déficit populacional mulheres 15 a 19 anos`,
    `déficit populacional homens 20 a 24 anos`,
    `déficit populacional mulheres 20 a 24 anos`
  ) |>
  pivot_longer(
    -ano,
    names_to = "variavel",
    values_to = "deficit"
  ) |>
  mutate(
    sexo = if_else(str_detect(variavel, "homens"), "homens", "mulheres"),
    faixa = case_when(
      str_detect(variavel, "15 a 19") ~ "15–19",
      str_detect(variavel, "20 a 24") ~ "20–24"
    )
  )

#Mortes incrementais (estoque → fluxo)
mortes_15_24 <- excesso_15_24_long |>
  arrange(sexo, faixa, ano) |>
  group_by(sexo, faixa) |>
  mutate(
    mortes_incrementais = pmax(deficit - lag(deficit, default = first(deficit)), 0)
  ) |>
  ungroup()

summary(mortes_15_24)

#Restringir ao período do regime

mortes_15_24_evento <- mortes_15_24 |>
  filter(ano >= 1964, ano <= 1985)

#Agregar resultados
excesso_15_24_resumo <- mortes_15_24_evento |>
  group_by(sexo, faixa) |>
  summarise(
    excesso_mortalidade = sum(mortes_incrementais, na.rm = TRUE),
    .groups = "drop"
  )

print(excesso_15_24_resumo
)
excesso_15_24_masculino <- excesso_15_24_resumo |> filter(sexo == "homens")

print(excesso_15_24_masculino)

excesso_15_24_masc_total <-  sum(excesso_15_24_masculino$excesso_mortalidade)

print(excesso_15_24_masc_total)

excesso_15_24_total <- excesso_15_24_resumo |>
  summarise(
    excesso_15_24 = sum(excesso_mortalidade)
  ) |>
  pull(excesso_15_24)

print(excesso_15_24_total)

# Testes formais (fortemente recomendados)
#Quebra estrutural (Bai–Perron)

teste_quebra_15_24 <- lm(
  deficit ~ ano,
  data = excesso_15_24_long
)
summary(teste_quebra_15_24)
plot(y = teste_quebra_15_24$residuals, x = excesso_15_24_long$ano)
plot(y = teste_quebra_15_24$fitted.values, x = excesso_15_24_long$ano)


#Você deve observar quebras nos anos 1960/70, não antes.

#Placebo (10–14 anos)
placebo_10_14 <- df_raw |>
  select(
    ano,
    `déficit populacional homens 10 a 14 anos`,
    `déficit populacional mulheres 10 a 14 anos`
  ) |>
  pivot_longer(-ano, values_to = "deficit")

summary(lm(deficit ~ ano, data = placebo_10_14))



# ==============================================================================
# 7. MORTALIDADE EM IDOSOS (60+)
# ==============================================================================

deficit_idosos <- df_raw |>
  select(ano, matches("déficit populacional (homens|mulheres) (60|70)")) |>
  pivot_longer(-ano, names_to = "variavel", values_to = "deficit")

mortes_idosos <- deficit_idosos |>
  arrange(variavel, ano) |>
  group_by(variavel) |>
  mutate(mortes_incrementais = pmax(deficit - lag(deficit, default = first(deficit)), 0)) |>
  ungroup()

summary(mortes_idosos)

excesso_idosos_total <- mortes_idosos |>
  filter(ano >= 1964, ano <= 1985) |>
  summarise(total = sum(mortes_incrementais, na.rm = TRUE)) |>
  pull(total)

print(excesso_idosos_total)

#===============================================================================
#7.1 Testes formais da mortalidade de idosos
#===============================================================================

#arrumar os dados para os testes

idosos_60mais <- df_raw |>
  transmute(
    ano,
    deficit =
      `déficit populacional homens 60 a 69 anos` +
      `déficit populacional mulheres 60 a 69 anos` +
      `déficit populacional homens 70 ou mais` +
      `déficit populacional mulheres 70 ou mais`
  )

idosos_60mais <- idosos_60mais |>
  mutate(
    regime = as.integer(ano >= 1964 & ano <= 1985),
    t_regime = if_else(regime == 1, ano - 1963, 0)
  )

#Teste de quebra estrutural endógena de Bai-Perron

modelo_idosos <- lm(deficit ~ ano, data = idosos_60mais)

quebras_idosos <- breakpoints(deficit ~ ano, data = idosos_60mais)

summary(quebras_idosos)
plot(quebras_idosos)

#Regressão segmentada pé e pós 1964

idosos_60mais <- idosos_60mais |>
  mutate(pos_1964 = as.integer(ano >= 1964))

modelo_segmentado_idosos <- lm(
  deficit ~ ano * regime,
  data = idosos_60mais
)

summary(modelo_segmentado_idosos)


#intervenção temporária

modelo_interv_idosos <- lm(
  deficit ~ ano + regime + t_regime,
  data = idosos_60mais
)

summary(modelo_interv_idosos)


#modelo de dupla quebra

idosos_60mais <- idosos_60mais |>
  mutate(
    t_pos_1964 = pmax(ano - 1963, 0),
    t_pos_1985 = pmax(ano - 1985, 0)
  )

modelo_dupla_quebra_idosos <- lm(
  deficit ~ ano + t_pos_1964 - t_pos_1985,
  data = idosos_60mais
)

summary(modelo_dupla_quebra_idosos)


#Teste placeboo
placebo_idosos <- idosos_60mais |>
  filter(ano < 1964)

summary(lm(deficit ~ ano, data = placebo_idosos))

#Visualização


idosos_60mais <- idosos_60mais |>
  mutate(regime_f = factor(regime, labels = c("Fora do regime", "Durante o regime")))

ggplot(idosos_60mais, aes(x = ano, y = deficit)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    aes(color = regime_f, group = regime_f),
    method = "lm",
    se = FALSE,
    linewidth = 1.2
  ) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed") +
  scale_color_manual(values = c("grey50", "#D62728")) +
  labs(
    title = "Déficit populacional 60 ou mais anos  e regime autoritário",
    x = "Ano",
    y = "Déficit populacional",
    color = "Período"
  ) +
  theme_ipea()

# ==============================================================================
# 8. INTERVALOS FINAIS
# ==============================================================================

limite_inferior <- mortes_documentadas + excesso_15_24_masc_total + mortes_adultos_homens 
estimativa_central <- mortes_documentadas  + excesso_15_24_total + mortes_adultos_total_valor
limite_superior <- mortes_documentadas + 
  excesso_15_24_total +
  mortes_adultos_total_valor +
  excesso_mortalidade_infancia +
  excesso_idosos_total

# ==============================================================================
# 9. TABELA FINAL
# ==============================================================================

tabela_componentes <- tibble(
  Componente = c(
    "Mortes documentadas",
    'Excesso jovens (15-24)',
    "Excesso adultos (25–59)",
    "Excesso infância (5–9)",
    "Excesso idosos (60+)",
    "TOTAL"
  ),
  Inferior = c(mortes_documentadas, excesso_15_24_masc_total, mortes_adultos_homens, 0, 0, limite_inferior),
  Central  = c(mortes_documentadas, excesso_15_24_total, mortes_adultos_total_valor, 0, 0, estimativa_central),
  Superior = c(mortes_documentadas, excesso_15_24_total, mortes_adultos_total_valor,
               excesso_mortalidade_infancia, excesso_idosos_total, limite_superior)
) |>
  mutate(across(-Componente, ~ comma(round(.x))))

print(tabela_componentes)


write_csv(tabela_componentes, 
          file = arquivo_saida)

# ==============================================================================
# 10. DISPERSÃO: DOCUMENTADAS × ESTIMATIVAS
# ==============================================================================

dados_disp <- df_raw |>
  filter(ano >= 1964, ano <= 1985) |>
  transmute(
    documentadas = `mortos e desaparecidos documentados`,
    hom_t1 = `homicídios excesso (projetado - tendência1)`,
    hom_t2 = `homicídios excesso (projetado - tendência2)`,
    hom_t3 = `homicídios excesso (projetado - tendência3)`,
    viol_t1 = `excesso de mortes violentas (projetado - tendência 1)`,
    viol_t2 = `excesso de mortes violentas (projetado - tendência 2)`,
    viol_t3 = `excesso de mortes violentas (projetado - tendência 3)`
  )

dados_long_disp <- dados_disp |>
  pivot_longer(-documentadas, names_to = "cenario", values_to = "excesso")

#criação de alguns modelos
summary(lm(hom_t1 ~ documentadas, data = dados_disp))
summary(lm(hom_t2 ~ documentadas, data = dados_disp))
summary(lm(hom_t3 ~ documentadas, data = dados_disp))
summary(lm(viol_t1 ~ documentadas, data = dados_disp))
summary(lm(viol_t2 ~ documentadas, data = dados_disp))
summary(lm(viol_t3 ~ documentadas, data = dados_disp))

#Enfim, o gráfico
ggplot(dados_long_disp, aes(x = documentadas, y = excesso)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  facet_wrap(~ cenario, scales = "free_y") +
  labs(
    title = "Associação entre mortes documentadas e estimativas de excesso",
    subtitle = "Homicídios e mortes violentas – diferentes tendências (1964–1985)",
    x = "Mortes documentadas (por ano)",
    y = "Excesso estimado de mortes"
  ) +
  theme_ipea() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

labels_eq <- dados_long_disp |>
  group_by(cenario) |>
  do({
    modelo <- lm(excesso ~ documentadas, data = .)
    s <- summary(modelo)
    
    tibble(
      intercepto = coef(modelo)[1],
      incl = coef(modelo)[2],
      r2 = s$r.squared,
      p = coef(s)[2, 4]
    )
  }) |>
  ungroup() |>
  mutate(
    label = paste0(
      "y = ",
      round(intercepto, 1),
      " + ",
      round(incl, 2),
      "x\n",
      "R² = ",
      round(r2, 3),
      "\n",
      "p = ",
      format.pval(p, digits = 2)
    )
  )

posicoes <- dados_long_disp |>
  group_by(cenario) |>
  summarise(
    x = min(documentadas, na.rm = TRUE),
    y = max(excesso, na.rm = TRUE)
  )

labels_eq <- labels_eq |>
  left_join(posicoes, by = "cenario")

ggplot(dados_long_disp, aes(x = documentadas, y = excesso)) +
  geom_point(alpha = 0.7, size = 2, shape = 4) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  geom_text(
    data = labels_eq,
    aes(x = x, y = y, label = label),
    hjust = -0.9,
    vjust = 2.55,
    size = 3,
    color = "red"
  ) +
  facet_wrap(~ cenario, scales = "free_y") +
  labs(
    title = "Associação entre mortes documentadas e estimativas de excesso",
    subtitle = "Equações de regressão estimadas separadamente por cenário (1964–1985)",
    x = "Mortes documentadas (por ano)",
    y = "Excesso estimado de mortes"
  ) +
  theme_ipea() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

#===================================================================
#11. TRIANGULAÇÃO DE DÉFICIT DEMOGRÁFICO E MORTES VIOLENTAS
#===================================================================

#ORGANIZAR OS DADOS
triang <- df_raw |>
  filter(ano >= 1964, ano <= 1985) |>
  transmute(
    ano,
    deficit_demografico =
      `déficit populacional homens 15 a 19 anos` +
      `déficit populacional mulheres 15 a 19 anos` +
      `déficit populacional homens 25 a 29 anos` +
      `déficit populacional homens 30 a 39 anos` +
      `déficit populacional homens 40 a 49 anos` +
      `déficit populacional homens 50 a 59 anos`,
    
    documentadas = `mortos e desaparecidos documentados`,
    
    homic_excesso_t1 = `homicídios excesso (projetado - tendência1)`,
    homic_excesso_t2 = `homicídios excesso (projetado - tendência2)`,
    homic_excesso_t3 = `homicídios excesso (projetado - tendência3)`,
    
    viol_excesso_t1 = `excesso de mortes violentas (projetado - tendência 1)`,
    viol_excesso_t2 = `excesso de mortes violentas (projetado - tendência 2)`,
    viol_excesso_t3 = `excesso de mortes violentas (projetado - tendência 3)`
  )

#regressões trianguladas

summary(lm(deficit_demografico ~ homic_excesso_t1 + ano, data = triang))

summary(lm(deficit_demografico ~ homic_excesso_t2 + ano, data = triang))

summary(lm(deficit_demografico ~ homic_excesso_t3 + ano, data = triang))

summary(lm(deficit_demografico ~ viol_excesso_t1 + ano, data = triang))

summary(lm(deficit_demografico ~ viol_excesso_t2 + ano, data = triang))

summary(lm(deficit_demografico ~ viol_excesso_t3 + ano, data = triang))

summary(lm(deficit_demografico ~ documentadas +  ano, data = triang))

summary(lm(deficit_demografico ~ documentadas + homic_excesso_t1 + ano, data = triang))

summary(lm(deficit_demografico ~ documentadas + homic_excesso_t2 + ano, data = triang))

summary(lm(deficit_demografico ~ documentadas + homic_excesso_t3 + ano, data = triang))

summary(lm(deficit_demografico ~ documentadas + viol_excesso_t1 + ano, data = triang))

summary(lm(deficit_demografico ~ documentadas + viol_excesso_t2 + ano, data = triang))

summary(lm(deficit_demografico ~ documentadas + viol_excesso_t3 + ano, data = triang))



#visualização: excesso de homicídios trend 1 => déficit populacional

ggplot(triang, aes(x = homic_excesso_t1, y = deficit_demografico)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Triangulação: homicídios e déficit demográfico",
    subtitle = "Brasil, 1964–1985",
    x = "Excesso de homicídios (tendência 1)",
    y = "Déficit populacional implícito"
  ) +
  theme_ipea()

#visualização: excesso de homicídios trend1 => déficit populacional

ggplot(triang, aes(x = homic_excesso_t2, y = deficit_demografico)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Triangulação: homicídios e déficit demográfico",
    subtitle = "Brasil, 1964–1985",
    x = "Excesso de homicídios (tendência 2)",
    y = "Déficit populacional implícito"
  ) +
  theme_ipea()

#visualização: excesso de homicídios trend 3 => déficit populacional

ggplot(triang, aes(x = homic_excesso_t3, y = deficit_demografico)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Triangulação: homicídios e déficit demográfico",
    subtitle = "Brasil, 1964–1985",
    x = "Excesso de homicídios (tendência 3)",
    y = "Déficit populacional implícito"
  ) +
  theme_ipea()

#visualização: mortes documentadas => déficit populacional; controle pelo ano

tempdef <- lm(deficit_demografico ~ ano, data = triang)
triang$residuos_ano <- tempdef$residuals
docdef <- lm(residuos_ano ~ documentadas, data = triang)
intercep_docdef <- coef(docdef)[["(Intercept)"]]
slope_docdef <- coef(docdef)[[2]]
txtdocdef <- sprintf('y = %.2f + %.2fx, r² = %.2f', intercep_docdef, slope_docdef, 
                     summary(docdef)$r.squared)

ggplot(triang, aes(x = documentadas, y = residuos_ano)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = T, linewidth = 1) +
  labs(
    title = "mortos e desaparecidos e déficit demográfico, controlado por trajetória temporal",
    subtitle = "Brasil, 1964–1985",
    x = "mortos e desaparecidos políticos documentados",
    y = "Déficit populacional implícito controlado por ano"
  ) +
  theme_ipea() + scale_color_ipea() +
  geom_text(x = Inf, y = -Inf, label = txtdocdef, color="black", size=5, hjust=1.1, vjust=-1.1)


#visualização: mortes violentas 1=> déficit populacional

ggplot(triang, aes(x = viol_excesso_t1, y = deficit_demografico)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Triangulação: excesso de mortes violentas e déficit demográfico",
    subtitle = "Brasil, 1964–1985",
    x = "excesso de mortes violentas (tendência 1)",
    y = "Déficit populacional implícito"
  ) +
  theme_ipea()

#visualização: mortes violentas 2=> déficit populacional

ggplot(triang, aes(x = viol_excesso_t2, y = deficit_demografico)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Triangulação: excesso de mortes violentas e déficit demográfico",
    subtitle = "Brasil, 1964–1985",
    x = "excesso de mortes violentas (tendência 2)",
    y = "Déficit populacional implícito"
  ) +
  theme_ipea()

#visualização: mortes violentas 3=> déficit populacional

ggplot(triang, aes(x = viol_excesso_t3, y = deficit_demografico)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Triangulação: excesso de mortes violentas e déficit demográfico",
    subtitle = "Brasil, 1964–1985",
    x = "excesso de mortes violentas (tendência 3)",
    y = "Déficit populacional implícito"
  ) +
  theme_ipea()


#Sincronia: magnitudes diferentes, tendências convergentes?

triang <- df_raw |>
  filter(ano >= 1964, ano <= 1985) |>
  transmute(
    ano,
    
    # Déficit demográfico implícito (jovens + adultos)
    deficit_demografico =
      `déficit populacional homens 15 a 19 anos` +
      `déficit populacional mulheres 15 a 19 anos` +
      `déficit populacional homens 25 a 29 anos` +
      `déficit populacional mulheres 25 a 29 anos` +
      `déficit populacional homens 30 a 39 anos` +
      `déficit populacional mulheres 30 a 39 anos` +
      `déficit populacional homens 40 a 49 anos` +
      `déficit populacional mulheres 40 a 49 anos` +
      `déficit populacional homens 50 a 59 anos` +
      `déficit populacional mulheres 50 a 59 anos`,
    
    documentadas = `mortos e desaparecidos documentados`,
    homic_excesso1 = `homicídios excesso (projetado - tendência1)`,
    homic_excesso2 = `homicídios excesso (projetado - tendência2)`,
    homic_excesso3 = `homicídios excesso (projetado - tendência3)`,
    viol_excesso1  = `excesso de mortes violentas (projetado - tendência 1)`,
    viol_excesso2  = `excesso de mortes violentas (projetado - tendência 2)`,
    viol_excesso3  = `excesso de mortes violentas (projetado - tendência 3)`
    
    
  )

triang_long <- triang |>
  pivot_longer(
    -ano,
    names_to = "serie",
    values_to = "valor"
  ) |>
  group_by(serie) |>
  mutate(valor_z = as.numeric(scale(valor))) |>
  ungroup()

ggplot(triang_long, aes(x = ano, y = valor_z, color = serie)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed") +
  labs(
    title = "Tendências temporais de diversas estimativas",
    subtitle = "Séries padronizadas (z-score)",
    x = NULL,
    y = "Desvio-padrão",
    color = NULL
  ) +
  scale_color_ipea(palette = "Orange-Blue") +
  theme_ipea()

#===============================================================================
#dados defasados
#===============================================================================

#lags

names(triang)

triang_lag <- triang |>
  arrange(ano) |>
  mutate(
    homic_excesso1_lag1 = lag(homic_excesso1, 1),
    homic_excesso1_lag2 = lag(homic_excesso1, 2)
  )

summary(lm(deficit_demografico ~ homic_excesso1 + homic_excesso1_lag1 + homic_excesso1_lag2, data = triang_lag))


#lags

triang_lag <- triang |>
  arrange(ano) |>
  mutate(
    homic_excesso2_lag1 = lag(homic_excesso2, 1),
    homic_excesso2_lag2 = lag(homic_excesso2, 2)
  )

summary(lm(deficit_demografico ~ homic_excesso2 + homic_excesso2_lag1 + homic_excesso2_lag2, data = triang_lag))


#lags

triang_lag <- triang |>
  arrange(ano) |>
  mutate(
    homic_excesso3_lag1 = lag(homic_excesso3, 1),
    homic_excesso3_lag2 = lag(homic_excesso3, 2)
  )

summary(lm(deficit_demografico ~ homic_excesso3 + homic_excesso3_lag1 + homic_excesso3_lag2, data = triang_lag))
#==============================================================================
#Gráfico de triangulação entre défict demográfico e excessos de mortes violentas
#==============================================================================

#Preparar os dados em formato long (passo-chave)
triang_long_disp <- triang |>
  select(
    deficit_demografico,
    homic_excesso1,
    homic_excesso2,
    homic_excesso3,
    viol_excesso1,
    viol_excesso2,
    viol_excesso3
  ) |>
  pivot_longer(
    -deficit_demografico,
    names_to = "cenario",
    values_to = "excesso"
  )

#Estimar regressões separadas por cenário (para as equações)
labels_eq_triang <- triang_long_disp |>
  group_by(cenario) |>
  do({
    m <- lm(deficit_demografico ~ excesso, data = .)
    s <- summary(m)
    
    tibble(
      intercepto = coef(m)[1],
      incl = coef(m)[2],
      r2 = s$r.squared,
      p = coef(s)[2, 4]
    )
  }) |>
  ungroup() |>
  mutate(
    label = paste0(
      "y = ",
      round(intercepto, 1),
      " + ",
      round(incl, 2),
      "x\n",
      "R² = ",
      round(r2, 3),
      "\n",
      "p = ",
      format.pval(p, digits = 2)
    )
  )

#Definir posições dos rótulos em cada facet
posicoes_triang <- triang_long_disp |>
  group_by(cenario) |>
  summarise(
    x = min(excesso, na.rm = TRUE),
    y = max(deficit_demografico, na.rm = TRUE)
  )

labels_eq_triang <- labels_eq_triang |>
  left_join(posicoes_triang, by = "cenario")

#GRÁFICO FINAL (síntese, 6 em 1)
ggplot(triang_long_disp, aes(x = excesso, y = deficit_demografico)) +
  geom_point(alpha = 0.7, size = 2, shape = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  geom_text(
    data = labels_eq_triang,
    aes(x = x, y = y, label = label),
    hjust = -0.1,
    vjust = 1.1,
    size = 3,
    color = "red"
  ) +
  facet_wrap(~ cenario, scales = "free_x") +
  labs(
    title = "Triangulação entre déficit demográfico e violência letal",
    subtitle = "Excesso de homicídios e mortes violentas segundo diferentes tendências (1964–1985)",
    x = "Excesso estimado de mortes",
    y = "Déficit demográfico implícito"
  ) +
  theme_ipea() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

#==============================================================================
#EXPLICAÇÃO ALTERNATIVA: A MIGRAÇÃO
#==============================================================================
#Fonte: Adas, Melhem.Panorama geográfico brasileiro. São Paulo: Moderna, 2004, p. 286.
migracao_participacao <- tibble::tribble(
  ~periodo,        ~percentual,
  "1808–1890",      8.0,
  "1891–1900",     29.0,
  "1901–1920",      7.1,
  "1921–1940",      8.1,
  "1941–1950",      1.0,
  "1951–1960",      3.4,
  "1961–1970",      0.9
)

#LEVY, M. S. F.. O papel da migração internacional na evolução da população brasileira (1872 a 1972). Revista de Saúde Pública, v. 8, p. 49–90, jun. 1974. https://doi.org/10.1590/S0034-89101974000500003 
entradas_imigrantes_decada <- tibble::tribble(
  ~periodo,        ~imigrantes, ~percentual_total,
  "1872–1879",      176337,       3.10,
  "1880–1889",      448622,       7.66,
  "1890–1899",     1198327,      20.47,
  "1900–1909",      622407,      10.63,
  "1910–1919",      815453,      13.93,
  "1920–1929",      846647,      14.46,
  "1930–1939",      332768,       5.68,
  "1940–1949",      114085,       1.94,
  "1950–1959",      583068,       9.95,
  "1960–1969",      197587,       3.37,
  "1970–2004",      520877,       8.80
)

#IBGE. Estatísticas históricas do Brasil. Séries econômicas, Demográficas e Sociais de 1550 a 1985. V. 3, p. 28-33, 1986.
pop_imigrante_censos <- tibble::tribble(
  ~ano, ~pop_total,   ~pop_imigrante, ~percentual,
  1872, 10112061,       389459,         3.85,
  1890, 14333915,       351618,         2.45,
  1900, 17318565,      1074311,         6.20,
  1920, 30635605,      1565961,         5.11,
  1940, 41236315,      1406342,         3.41,
  1950, 51944397,      1213974,         2.33,
  1960, 70992343,      1400245,         1.97,
  1970, 93134846,      1229128,         1.32,
  1980,119011052,      1110910,         0.93,
  1991,146825475,       767773,         0.52,
  2000,169590693,       683982,         0.40
)


# ============================================================
# DADOS DE MIGRAÇÃO (CENSOS – % da população)
# ============================================================

migracao_censo <- tibble::tribble(
  ~ano, ~perc_imigrantes,
  1950, 2.33,
  1960, 1.97,
  1970, 1.32,
  1980, 0.93
)

# interpolar para anos intercensitários
migracao_ano <- tibble(ano = 1950:1985) |>
  left_join(migracao_censo, by = "ano") |>
  mutate(
    perc_imigrantes = approx(
      x = migracao_censo$ano,
      y = migracao_censo$perc_imigrantes,
      xout = ano,
      rule = 2
    )$y
  )

# ============================================================
# DÉFICIT DEMOGRÁFICO (jovens + adultos, como no seu modelo)
# ============================================================

deficit_demo <- df_raw |>
  filter(ano >= 1950, ano <= 1985) |>
  transmute(
    ano,
    deficit =
      `déficit populacional homens 15 a 19 anos` +
      `déficit populacional mulheres 15 a 19 anos` +
      `déficit populacional homens 25 a 29 anos` +
      `déficit populacional mulheres 25 a 29 anos` +
      `déficit populacional homens 30 a 39 anos` +
      `déficit populacional mulheres 30 a 39 anos` +
      `déficit populacional homens 40 a 49 anos` +
      `déficit populacional mulheres 40 a 49 anos` +
      `déficit populacional homens 50 a 59 anos` +
      `déficit populacional mulheres 50 a 59 anos`
  )

#========================================================================================
#TESTE FORMAL
#========================================================================================
sintese <- deficit_demo |>
  left_join(migracao_ano, by = "ano") |>
  mutate(
    deficit_z   = as.numeric(scale(deficit)),
    migracao_z  = as.numeric(scale(perc_imigrantes)),
    regime = ano >= 1964 & ano <= 1985
  )


summary(lm(deficit ~ perc_imigrantes + ano, data = sintese))


#========================================================================================
#SALDO MIGRATÓRIO ANUAL
#========================================================================================

# População total por ano
pop_ano <- df_raw |>
  select(ano, populacao = população)

# Participação da migração no crescimento (%)
migracao_part <- tibble::tribble(
  ~decada, ~participacao_migracao,
  1950, 0.0233,
  1960, 0.0197,
  1970, 0.0132,
  1980, 0.0093
)

#Crescimento populacional por década
crescimento_decada <- pop_ano |>
  filter(ano %% 10 == 0) |>
  arrange(ano) |>
  mutate(
    decada = ano - 10,
    pop_inicial = lag(populacao),
    crescimento = populacao - pop_inicial
  ) |>
  filter(!is.na(crescimento))

#Saldo migratório da década
saldo_migratorio_decada <- crescimento_decada |>
  left_join(migracao_part, by = "decada") |>
  mutate(
    saldo_migratorio = crescimento * participacao_migracao
  )

#saldo migratório anual
saldo_migratorio_anual <- pop_ano |>
  mutate(
    decada = floor(ano / 10) * 10
  ) |>
  left_join(
    saldo_migratorio_decada |> 
      select(decada, saldo_migratorio),
    by = "decada"
  ) |>
  group_by(decada) |>
  mutate(
    crescimento_anual = populacao - lag(populacao),
    peso = crescimento_anual / sum(crescimento_anual, na.rm = TRUE),
    saldo_migratorio_ano = saldo_migratorio * peso
  ) |>
  ungroup()

#distribuir linearmente o saldo migratório anual da década por ano

saldo_migratorio_anual_linear <- df_raw |>
  select(ano) |>
  mutate(
    decada = floor(ano / 10) * 10
  ) |>
  left_join(
    saldo_migratorio_decada,
    by = "decada"
  )



saldo_migratorio_anual_linear <- saldo_migratorio_anual_linear |>
  group_by(decada) |>
  mutate(
    n_anos = n(),
    saldo_migratorio_ano = saldo_migratorio / n_anos
  ) |>
  ungroup()


saldo_migratorio_anual_linear$ano <- saldo_migratorio_anual_linear$ano.x

triang_migracao <- triang |>
  left_join(
    saldo_migratorio_anual_linear |> select(ano, saldo_migratorio_ano),
    by = "ano"
  )

summary(
  lm(
    deficit_demografico ~ saldo_migratorio_ano + homic_excesso1 + ano,
    data = triang_migracao
  )
)

summary(
  lm(
    deficit_demografico ~ saldo_migratorio_ano + homic_excesso2 + ano,
    data = triang_migracao
  )
)

summary(
  lm(
    deficit_demografico ~ saldo_migratorio_ano + homic_excesso3 + ano,
    data = triang_migracao
  )
)

#visualização

anodef <- lm(deficit_demografico ~ ano, data = triang_migracao)
summary(anodef)
triang_migracao$residuos <- anodef$residuals
defic_migra <- lm(residuos ~ saldo_migratorio_ano, data = triang_migracao)
summary(defic_migra)
intercept_defmig <- coef(defic_migra)[["(Intercept)"]]
slope_defmig <- coef(defic_migra)[[2]]
txt_lm_dm <- sprintf('y = %.2f + %.2fx, r² = %.2f', intercept_defmig, slope_defmig, 
                     summary(defic_migra)$r.squared)
ggplot(triang_migracao, aes(x = saldo_migratorio_ano, y = residuos)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, formula = y ~x, show.legend = T) +
  labs(title = "Déficit populacional controlado por ano e saldo migratório",
       x = "ano", y = "saldo migratório controlado por ano") +
  theme_ipea() + scale_color_ipea() +
  geom_text(x = Inf, y = -Inf, label = txt_lm_dm, color="black", size=5, hjust=1.1, vjust=-1.1)
