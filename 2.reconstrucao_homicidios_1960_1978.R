#==============================================================================
# PACOTES
#==============================================================================

library(tidyverse)
library(ggplot2)
library(ipeaplot)
library(readxl)
library(strucchange)
library(broom)
library(patchwork)
library(urca)
library(tseries)

salvar_reconstrucao <- "/home/matheus/Documentos/IPEA/modelos/reconstrução homicídios/reconstrucao_homicidios_desde_1960"


#===============================================
#Agregar por ano
#===============================================

colunas_numericas <- sapply(dados_mortes, is.numeric)
print(colunas_numericas)

# Certificar-se de que 'ano' não está incluída nas colunas numéricas para a soma
# (pois será usada para agrupar)
colunas_numericas["ano"] <- FALSE

# Listar colunas numéricas
colunas_para_somar <- names(dados_mortes)[colunas_numericas]

cat("Colunas numéricas identificadas para soma:\n")
print(colunas_para_somar)

# Criar dataframe com soma por ano usando base R
dados_mortes_ano <- aggregate(
  dados_mortes[colunas_para_somar], 
  by = list(ano = dados_mortes$ano), 
  FUN = sum, 
  na.rm = TRUE
)

# Verificar o resultado
cat("\nDataFrame com soma por ano (primeiras linhas):\n")
print(head(dados_mortes_ano))


dados_mortes_ano$tx_agressoes <- dados_mortes_ano$agressoes_e_confrontos *100000/dados_mortes_ano$populacao

dados_mortes_ano$tx_homicid_ajprop <- dados_mortes_ano$homicidios_intencionais_prop_media*100000/dados_mortes_ano$populacao

dados_mortes_ano$tx_homicid_ajrand <- dados_mortes_ano$homicidios_intencionais_rand_media*100000/dados_mortes_ano$populacao

#opcional, se não quiser atualizar os dados do DATASUS
dados_mortes_ano <- dados_mortes_ano %>%
  filter(ano < 2023)

dados_mortes_ano$anos <- as.numeric(dados_mortes_ano$ano)

ggplot(dados_mortes_ano, aes(x = anos)) +
  geom_line(aes(y = tx_agressoes, colour =  "agressões"))+
  scale_x_continuous()+
  geom_line(aes(y = tx_homicid_ajprop, colour = "homicídios aj prop")) +
  geom_line(aes(y = tx_homicid_ajrand, colour = "homicídios aj rand")) +
  labs(title = "estimativas de taxa de homicídios", y = "taxa por 100 mil habitantes", colour = "legenda") +
  theme_ipea() + scale_color_ipea(pallete = "viridis") + scale_fill_ipea(pallete = "viridis")

write.csv(dados_mortes_ano, file.path(salvar_reconstrucao, "estimativa_nacional.csv"), 
          row.names = FALSE)


#==============================================
#reconstrução do número e taxas desde 1960
#==============================================
#o SIM-DATASUS só publica dados de mortlaidade desde 1979
#antes disso, só achei dados de mortes por causas externas (MCE) no município de São Paulo, em 1960, 1965, 1970 e 1975
#São Paulo manteve cerca de 5%-6% da população nacional no período, sendo o principal centro econômico e demográfico
#poderia as MCE se São Paulo servir de proxy para os homicídios do Brasil todo?
#para averiguar, utilizamos correlação linear com os homicídios ajustados pelo método anterior
#testamos dois cenários de correlação: dados de 1979 a 1999, ou de 1979 a 2022
#por algum motivo, a correlação entre SP e Brasil é mais forte no primeiro cenário, chegando a 95% ou mais


#carregar os dados
dados_mortes_SP <- read_excel("~/Documentos/IPEA/modelos/demografia social da ditadura militar/mortes_violentas_SP.xlsx", 
                              sheet = "Planilha1")

#verifique o nome das variáveis
names(dados_mortes_ano)
names(dados_mortes_SP)

#aumentar o tamanho da série temporal
anos_completos <- tibble(
  ano = 1960:2022
)

dados_mortes_ano_ext <- anos_completos |>
  left_join(dados_mortes_ano, by = "ano")

#juntar os dados nacionais da reconstrução anterior com os dados do município de São Paulo
dados_1960_2022 <- dados_mortes_ano_ext |>
  left_join(dados_mortes_SP, by = "ano")

#restringir a 79-99
dados_79_99 <- dados_1960_2022 |>
  filter(ano >= 1979, ano <= 1999)

#calcular correlações
correlacoes <- tibble(
  variavel = c(
    "agressoes_e_confrontos",
    "homicidios_intencionais_prop_media",
    "homicidios_intencionais_rand_media",
    "MCE"
  ),
  correlacao = c(
    cor(dados_79_99$MCE_SP,
        dados_79_99$agressoes_e_confrontos,
        use = "complete.obs"),
    cor(dados_79_99$MCE_SP,
        dados_79_99$homicidios_intencionais_prop_media,
        use = "complete.obs"),
    cor(dados_79_99$MCE_SP,
        dados_79_99$homicidios_intencionais_rand_media,
        use = "complete.obs"),
    cor(dados_79_99$MCE_SP,
        dados_79_99$MCE,
        use = "complete.obs")
  )
)

correlacoes

#homicídios reconstruídos pelo método 2 terão correlação um pouco maior
#por sensibilidade, vamos manter as demais medidas

# ===========================================================
# testes de raiz unitária e cointegração
# ===========================================================

library(aTSA)

# Teste em nível (H0: Possui raiz unitária / Não é estacionária)
adf.test(dados_79_99$homicidios_intencionais_rand_media)
adf.test(dados_79_99$MCE_SP)

# Teste na primeira diferença (H0: Possui raiz unitária)
adf.test(diff(dados_79_99$homicidios_intencionais_rand_media))
adf.test(diff(dados_79_99$MCE_SP))

coint.test(dados_79_99$homicidios_intencionais_rand_media, dados_79_99$MCE_SP)

coint.test(log(dados_79_99$homicidios_intencionais_rand_media), dados_79_99$MCE_SP)

coint.test(dados_79_99$homicidios_intencionais_rand_media, poly(dados_79_99$MCE_SP, 2))

coint.test(log(dados_79_99$homicidios_intencionais_rand_media), log(dados_79_99$MCE_SP))


coint.test(log(dados_79_99$homicidios_intencionais_rand_media), poly(dados_79_99$MCE_SP, 2))

# Teste em nível (H0: Possui raiz unitária / Não é estacionária)
adf.test(dados_79_99$MCE)
adf.test(dados_79_99$MCE_SP)

# Teste na primeira diferença (H0: Possui raiz unitária)
adf.test(diff(dados_79_99$MCE))
adf.test(diff(dados_79_99$MCE_SP))


coint.test(dados_79_99$MCE, dados_79_99$MCE_SP)

coint.test(dados_79_99$MCE, poly(dados_79_99$MCE_SP, 2))

coint.test(log(dados_79_99$MCE), log(dados_79_99$MCE_SP))


coint.test(log(dados_79_99$MCE), poly(dados_79_99$MCE_SP, 2))


library(dplyr)

#==============================================================================
# PASSO 1: Interpolação da variável preditora (MCE_SP) para escala anual
#==============================================================================

# Garantir que os dados estejam ordenados por ano
dados_1960_2022 <- dados_1960_2022 |> arrange(ano)

# Criamos a série anual contínua da MCE_SP usando Spline Cúbico Natural
# O R vai usar os pontos conhecidos (60, 65, 70, 75 e 79+) para criar a curva anual
MCE_SP_anualizada <- spline(
  x = dados_1960_2022$ano,
  y = dados_1960_2022$MCE_SP,
  xout = dados_1960_2022$ano,
  method = "natural"
)$y

# Inserimos a MCE_SP anualizada de volta no dataset para os anos anteriores a 1979
dados_1960_2022 <- dados_1960_2022 |>
  mutate(
    MCE_SP = if_else(ano < 1979, MCE_SP_anualizada, MCE_SP)
  )

#==============================================================================
# PASSO 2: Modelagem (Treino com base nos dados observados de 1979 a 1999)
#==============================================================================

mod_agressoes <- lm(
  agressoes_e_confrontos ~ poly(MCE_SP, 2),
  data = dados_79_99
)

mod_prop_media <- lm(
  homicidios_intencionais_prop_media ~ poly(MCE_SP, 2),
  data = dados_79_99
)

mod_rand_media <- lm(
  homicidios_intencionais_rand_media ~ poly(MCE_SP, 2),
  data = dados_79_99
)

mod_MCE <- lm(
  MCE ~ poly(MCE_SP, 2),
  data = dados_79_99
)

#==============================================================================
# PASSO 3: Backcasting (Predição anual baseada na MCE_SP anualizada)
#==============================================================================

# Filtramos todos os anos do passado que precisam de imputação (1960 a 1978)
anos_para_imputar <- dados_1960_2022 |>
  filter(ano >= 1960 & ano <= 1978)

# Geramos as predições ANUAIS completas
pred_agressoes <- round(predict(mod_agressoes, newdata = anos_para_imputar), 0)
pred_prop_media <- round(predict(mod_prop_media, newdata = anos_para_imputar), 0)
pred_rand_media <- round(predict(mod_rand_media, newdata = anos_para_imputar), 0)
pred_MCE        <- round(predict(mod_MCE, newdata = anos_para_imputar), 0)

#==============================================================================
# PASSO 4: Atribuição dos valores preditos ao banco principal
#==============================================================================

dados_1960_2022 <- dados_1960_2022 |>
  mutate(
    agressoes_e_confrontos = if_else(
      ano >= 1960 & ano <= 1978,
      pred_agressoes[match(ano, anos_para_imputar$ano)],
      agressoes_e_confrontos
    ),
    homicidios_intencionais_prop_media = if_else(
      ano >= 1960 & ano <= 1978,
      pred_prop_media[match(ano, anos_para_imputar$ano)],
      homicidios_intencionais_prop_media
    ),
    homicidios_intencionais_rand_media = if_else(
      ano >= 1960 & ano <= 1978,
      pred_rand_media[match(ano, anos_para_imputar$ano)],
      homicidios_intencionais_rand_media
    ),
    MCE = if_else(
      ano >= 1960 & ano <= 1978,
      pred_MCE[match(ano, anos_para_imputar$ano)],
      MCE
    )
  ) |>
  # Forçar arredondamento final de segurança
  mutate(
    homicidios_intencionais_rand_media = round(homicidios_intencionais_rand_media, 0),
    MCE = round(MCE, 0)
  )

#==============================================================================
# PASSO 5: Checagem Visual e Estrutural
#==============================================================================

# Plot para conferir a suavidade e aderência da série histórica gerada
plot(x = dados_1960_2022$ano, y = dados_1960_2022$homicidios_intencionais_rand_media, 
     type = "l", col = "darkred", xlab = "Ano", ylab = "Homicídios Rand Média",
     main = "Resultado do Backcasting com MCE_SP Interpolada")

# Visualização da tabela gerada até 1985
dados_1960_2022 |>
  filter(ano <= 1985) |>
  dplyr::select(
    ano,
    MCE_SP,
    homicidios_intencionais_rand_media,
    MCE
  )

#visualização da série completa
#homicidios
numero_homicidios <- ggplot(dados_1960_2022, aes(x = ano)) +
  geom_line(
    aes(y = homicidios_intencionais_rand_media),
    color = "steelblue"
  ) +
  geom_point(
    aes(y = homicidios_intencionais_rand_media),
    color = "black",
    size = 2
  ) +
  labs(
    y = "número de homicídios intencionais"
  ) + theme_ipea() + scale_color_ipea(palette = "Red-Blue-White")

numero_homicidios

save_ipeaplot(numero_homicidios, file.name = "numero_reconstruido_homicidios_desde_1960", format = c("eps", "png"))


dados_1960_2022$mortes_violentas <- dados_1960_2022$homicidios_intencionais_rand_media +
  dados_1960_2022$MCE


#mortes violentas (homicídios, acidentes e suicídios)
numero_MCE <- ggplot(dados_1960_2022, aes(x = ano)) +
  scale_y_continuous() +
  geom_line(
    aes(y = mortes_violentas, colour = "mortes violentas (total)")
  ) +
  geom_point(
    aes(y = mortes_violentas, colour = "mortes violentas (total)"),
    size = 2
  ) +
  geom_line(
    aes(y = homicidios_intencionais_rand_media, colour = "homicídios")
  ) +
  geom_point(
    aes(y = homicidios_intencionais_rand_media, colour = "homicídios"),
    size = 2
  ) +
  labs(
    y = "número de mortes",
    colour = "tipo:"
  ) + theme_ipea(legend.position = "bottom") + 
  geom_vline(xintercept = 1979, linetype="dotted")+
  scale_color_ipea(palette = "Red-Blue-White") +
  scale_fill_ipea(palette = "Inferno")

numero_MCE

save_ipeaplot(numero_MCE, file.name = "numero_reconstruido_mce_desde_1960", format = c("eps", "png"))

#padronizar os dados estimados, interpolados e observados integrados

dados_1960_2022$população

dados_1960_2022$tx_homicidios <- dados_1960_2022$homicidios_intencionais_rand_media*100000/dados_1960_2022$população

summary(dados_1960_2022$tx_homicidios)

taxa_reconstruida <-ggplot(dados_1960_2022, aes(x = ano)) +
  scale_x_continuous(
    breaks = seq(1960, 2022, by = 10),
    minor_breaks = seq(1960, 2022, by = 5)
  )+
  geom_line(
    aes(y = tx_homicidios),
    color = "steelblue"
  )+
  geom_point(
    aes(y = tx_homicidios),
    color = "black",
    size = 2
  ) +
  labs(
    y = "taxa de homicídios intencionais (por 100 mil hab.)",
  ) +
  theme_ipea(x_text_angle = 0) +
  scale_color_ipea(palette = "Red-Blue-White")

taxa_reconstruida

save_ipeaplot(taxa_reconstruida, "taxa reconstruída", format = c("eps", "png"))

#taxa de mortes por causas externas

dados_1960_2022$tx_mce <- (dados_1960_2022$MCE)*1e5/ dados_1960_2022$população

#taxa de mortes violentas


dados_1960_2022$tx_mortes_violentas <- dados_1960_2022$mortes_violentas*1e5/dados_1960_2022$população

summary(dados_1960_2022$tx_mce)

taxa_reconstruida_mce <-ggplot(dados_1960_2022, aes(x = ano)) +
  scale_x_continuous(
    breaks = seq(1960, 2022, by = 10),
    minor_breaks = seq(1960, 2022, by = 5)
  )+
  scale_y_continuous()+
  geom_line(
    aes(y = tx_mortes_violentas, colour = "taxa de mortes violentas (total)")
  )+
  geom_point(
    aes(y = tx_mortes_violentas, colour = "taxa de mortes violentas (total)"),
    size = 2
  ) +
  geom_line(
    aes(y = tx_homicidios, colour = "taxa de homicídios"),
    color = "steelblue"
  )+
  geom_point(
    aes(y = tx_homicidios, colour = "taxa de homicídios"),
    size = 2
  ) +
  labs(
    y = "taxa de mortes (por 100 mil hab.)",
    colour = "tipo:"
  ) +
  theme_ipea(x_text_angle = 0, legend.position = "bottom") +
  geom_vline(xintercept = 1979, linetype="dotted")+
  scale_color_ipea(palette = "Red-Blue-White") +
  scale_fill_ipea(palette = "Red-Blue-White")

taxa_reconstruida_mce

save_ipeaplot(taxa_reconstruida_mce, "taxa reconstruída causas externas", format = c("eps", "png"))

#Mortes violentas (homicídios+mce)


dados_1960_2022$mortes_violentas <- dados_1960_2022$homicidios_intencionais_rand_media +
  dados_1960_2022$MCE

# ================================================================
#  GRÁFICO DE ÁREA EMPILHADA PARA NÚMERO (homicídios + MCE)
# ================================================================

# Preparar dados para área empilhada
dados_empilhados_numero <- dados_1960_2022 %>%
  select(ano, 
         Homicídios = homicidios_intencionais_rand_media, 
         `Causas Externas` = MCE) %>%
  pivot_longer(cols = c(Homicídios, `Causas Externas`), 
               names_to = "Tipo", 
               values_to = "Valor")

# Ordenar fatores para empilhamento (homicídios embaixo, causas externas em cima)
dados_empilhados_numero$Tipo <- factor(dados_empilhados_numero$Tipo, 
                                       levels = c("Causas Externas", "Homicídios"))

# Gráfico de área empilhada
numero_MCE_empilhado <- ggplot(dados_empilhados_numero, aes(x = ano, y = Valor, fill = Tipo)) +
  geom_area(alpha = 0.8) +
  labs(
    title = "Mortes Violentas no Brasil (1960-2022)",
    subtitle = "Homicídios e demais causas externas",
    y = "Número de mortes",
    x = "Ano",
    fill = "Tipo de morte"
  ) +
  theme_ipea(legend.position = "bottom") +
  scale_fill_ipea(palette = "Red-Blue-White") +
  scale_color_ipea(palette = "Red-Blue-White") +
  scale_x_continuous(
    breaks = seq(1960, 2022, by = 10),
    minor_breaks = seq(1960, 2022, by = 5)
  ) +
  geom_vline(xintercept = 1979, linetype = "dotted", color = "#666666") +
  annotate("text", x = 1979, y = max(dados_1960_2022$mortes_violentas), 
           label = "Início SIM", hjust = -0.1, vjust = 1, size = 3, color = "#666666")

numero_MCE_empilhado

# ================================================================
# 2. GRÁFICO DE ÁREA EMPILHADA PARA TAXA
# ================================================================

# Preparar dados para área empilhada de taxas
dados_empilhados_taxa <- dados_1960_2022 %>%
  select(ano, 
         `Taxa Homicídios` = tx_homicidios, 
         `Taxa Causas Externas` = tx_mce,
         'tx_mortes_violentas' = tx_mortes_violentas) %>%
  pivot_longer(cols = c(`Taxa Homicídios`, `Taxa Causas Externas`), 
               names_to = "Tipo", 
               values_to = "Taxa")

# Ordenar fatores
dados_empilhados_taxa$Tipo <- factor(dados_empilhados_taxa$Tipo, 
                                     levels = c("Taxa Causas Externas", "Taxa Homicídios"))

# Gráfico de área empilhada para taxas
taxa_reconstruida_mce_empilhado <- ggplot(dados_empilhados_taxa, aes(x = ano, y = Taxa, fill = Tipo)) +
  geom_area(alpha = 0.8) +
  labs(
    title = "Taxa de Mortes Violentas no Brasil (1960-2022)",
    subtitle = "Por 100 mil habitantes",
    y = "Taxa por 100 mil habitantes",
    x = "Ano",
    fill = "Tipo de morte"
  ) +
  theme_ipea(legend.position = "bottom") +
  scale_fill_ipea(palette = "Red-Blue-White") +
  scale_color_ipea(palette = "Red-Blue-White") +
  scale_x_continuous(
    breaks = seq(1960, 2022, by = 10),
    minor_breaks = seq(1960, 2022, by = 5)
  ) +
  geom_vline(xintercept = 1979, linetype = "dotted", color = "#666666") +
  annotate("text", x = 1979, y = max(dados_1960_2022$tx_mortes_violentas), 
           label = "Início SIM", hjust = -0.1, vjust = 1, size = 3, color = "#666666")

taxa_reconstruida_mce_empilhado


#======================================================================
#CONTRAFACTUAIS DOS HOMICÍDIOS
#======================================================================

#Agora passamos a uma análise dos contrafactuais, não só de mensuração
#a ideia é definir 3 contrafactuais, com propósitos diversos
#tratam-se de taxas contrafactuais de homicídios
#como as taxas de 60-78 são imputadas e interpoladas, deve ser corroborada por...
#uma análise demográfica de coorte com contrafactual sobre população jovem masculina


#contrafactual 1 define efeito amplo do regime, extrapolando a tendência 60-63

#1. Contrafctual de efeito máximo (extrapolação da tendência de 1960 a 1963)
#tendência pré-evento projetada para frente
#supõe que a tendência de 1960-1963 prosseguiria linearmente
#então projeta esta tendência para todo o período
#é a hipótese mais forte, que supõe um legado violento de longo prazo

dados_pre <- dados_1960_2022 |>
  filter(ano >= 1960, ano <= 1963)

mod_cf1 <- lm(
  tx_homicidios ~ ano,
  data = dados_pre
)
summary(mod_cf1)


#extrapolação para frente
dados_1960_2022 <- dados_1960_2022 |>
  mutate(
    cf_efeito_max = predict(mod_cf1, newdata = cur_data())
  )

plot(x = dados_1960_2022$ano, y = dados_1960_2022$cf_efeito_max)

#2. contrafactual se efeito só durante o regime
#este contrafactual supõe um efeito restrito à duração do regime
#supõe que a dinâmica da violência foi condicionada pela duração do regime
#ou seja, cessado o regime, o efeito cessa, a taxa convergiria para a observada imediatamente
#supõe que o país chegaria "naturalmente" à taxa observada em 1986
#pode capturar o impacto da violência política e polícia específica do regime militar
#para dados pré e pós regime, os valores são iguais aos dados
#mas é mais limitada para capturar o legado de médio e longo prazo da ditadura militar

dados_cf2_base <- dados_1960_2022 |>
  filter(ano %in% c(1963, 1985))

ano_inicial <- 1963
ano_final   <- 1985

tx_inicial <- dados_cf2_base |>
  dplyr::filter(ano == ano_inicial) |>
  dplyr::pull(tx_homicidios)

tx_final <- dados_cf2_base |>
  dplyr::filter(ano == ano_final) |>
  dplyr::pull(tx_homicidios)

T <- ano_final - ano_inicial
r <- (log(tx_final) - log(tx_inicial)) / T

cf_duracao_evento_raw <- dados_1960_2022 |>
  mutate(
    cf_exp = tx_inicial * exp(r * (ano - ano_inicial))
  ) |>
  pull(cf_exp)

dados_1960_2022 <- dados_1960_2022 |>
  mutate(
    cf_duracao_evento = if_else(
      ano >= 1963 & ano <= 1985,
      cf_duracao_evento_raw,
      tx_homicidios
    )
  )

dados_1960_2022$cf_duracao_evento


#3. contrafactual de duração ampliada
#este contrafactual é um meio termo entre os anteriores
#inicialmente a tendência seria a mesma de 60-63, mas depois convergiria com os dados observados
#o marco definido da mudança é o fim do AI-5, em 1978, encerrando os "Anos de Chumbo"...
#mas depois disso persistiu alguma violência política documentada e transição lenta
#manteve-se o aparelho policial-militar formado sob a ditadura militar
#a violência política decaiu, mas ascendeu a violência privada e policial no contexto de mercados ilícitos
#assim, conjuga legado violento institucional e a expansão internacional do narcotráfico
#para dados pré e pós regime, os valores são iguais aos dados

#Contrafactual com persistência e decaimento paramétrico

#CF3t′=λt​⋅CF1t​+(1−λt​)⋅Yt​
#λt​=exp(−τt−1978)

#parâmetro de decaimento
tau <- 10

dados_1960_2022 <- dados_1960_2022 |>
  mutate(
    lambda = case_when(
      ano <= 1978 ~ 1,
      ano > 1978  ~ exp(-(ano - 1978) / tau)
    ),
    cf_duracao_evento_amplo = lambda * cf_efeito_max +
      (1 - lambda) * tx_homicidios
  )

summary(dados_1960_2022$cf_duracao_evento_amplo)

range(dados_1960_2022$lambda)



#comparar contrafactuais

taxa_e_cenarios <- ggplot(dados_1960_2022, aes(x = ano ))+
  geom_line(aes(y = tx_homicidios, colour = "taxa de homicídios"), linewidth = 1.5)+
  geom_line(aes(y = cf_efeito_max, colour =  "tendência pré-golpe"), linetype = "dashed")+
  geom_line(aes(y = cf_duracao_evento, colour = "contrafactual 1963-1986"), linetype = "dashed")+
  geom_line(aes(y = cf_duracao_evento_amplo, colour = "tend. pré-golpe decai após-78"), linetype = "dashed") + 
  geom_vline(xintercept = 1964, linetype="dotted")+
  geom_vline(xintercept = 1985, linetype = "dotted")+
  labs(
    title = "taxa de homicídios e tendências contrafactuais",
    subtitle = "imputado (1960-1978), observado (1979-2022)",
    x = "ano",
    y = "taxa de homicídios (por 100 mil hab.)"
  ) + theme(legend.title = element_blank()) +
  theme_ipea(legend.position = "bottom") +  scale_color_ipea(palette = "Red-Blue-White")

taxa_e_cenarios

save_ipeaplot(taxa_e_cenarios, "taxa e cenários", format = c("eps", "png"))

#análise descritiva dos contrafactuais

dados_1960_2022$cf_efeito_max
summary(dados_1960_2022$cf_efeito_max)

dados_1960_2022$cf_duracao_evento
summary(dados_1960_2022$cf_duracao_evento)

summary(dados_1960_2022$cf_duracao_evento_amplo)
dados_1960_2022$cf_duracao_evento_amplo


#resultado de interesse é o número de mortes excedentes, não a taxa, 
#por isso invertemos a diferença entre as taxas
#a taxa é um nível de violência

dados_1960_2022$excesso_homicidios_cf1 <- pmax(0, (dados_1960_2022$tx_homicidios - dados_1960_2022$cf_efeito_max) *
                                                 dados_1960_2022$população/100000)

sum(dados_1960_2022$excesso_homicidios_cf1)

dados_1960_2022$excesso_homicidios_cf2 <- pmax(0,(dados_1960_2022$tx_homicidios - dados_1960_2022$cf_duracao_evento) *
                                                 dados_1960_2022$população/100000)

sum(dados_1960_2022$excesso_homicidios_cf2)

dados_1960_2022$excesso_homicidios_cf3 <- pmax(0,(dados_1960_2022$tx_homicidios - dados_1960_2022$cf_duracao_evento_amplo)*
                                                 dados_1960_2022$população/100000)

sum(dados_1960_2022$excesso_homicidios_cf3)

#======================================================================
#CONTRAFACTUAIS DAS CAUSAS EXTERNAS DE MORTALIDADE NO BRASIL
#======================================================================

#Agora passamos a uma análise dos contrafactuais, não só de mensuração
#a ideia é definir 3 contrafactuais, com propósitos diversos
#tratam-se de taxas contrafactuais de mortes por causas externas
#os contrafactuais são baseados em diferentes suposições sobre o legado do regime
#militar e sua influência na violência letal no Brasil
#como as taxas de 60-78 são imputadas e interpoladas, deve ser corroborada por...
#uma análise demográfica de coorte com contrafactual sobre população jovem e adulta
#no caso das mortes por causas externas, elas incluem homicídios, acidentes e suicídios
#incluir os suicídios e acidentes pode ser uma maneira de capturar...
#efeitos indiretos do endurecimento econômico e político
#em especial, temos evidências de aumento dos óbitos no trabalho,
#e acidentes de trânsito no longo prazo, com a difusão do automóvel
#dos suicídios não temos evidências, mas pode ter sido ocultado pelo regime militar




#contrafactual 1 define efeito amplo do regime, extrapolando a tendência 60-63

#1. Contrafctual de efeito máximo (extrapolação da tendência de 1960 a 1963
#tendência pré-evento projetada para frente
#supõe que a tendência de 1960-1965 prosseguiria linearmente
#então projeta esta tendência para todo o período
#é a hipótese mais forte, que supõe um legado violento de longo prazo

dados_pre <- dados_1960_2022 |>
  filter(ano >= 1960, ano <= 1963)



mod_cf1_mce <- lm(
  tx_mce ~ ano,
  data = dados_pre
)
summary(mod_cf1_mce)

#extrapolação para frente
dados_1960_2022 <- dados_1960_2022 |>
  mutate(
    cf_efeito_max_mce = predict(mod_cf1_mce, newdata = cur_data())
  )

plot(x = dados_1960_2022$ano, y = dados_1960_2022$cf_efeito_max_mce)

#2. contrafactual se efeito só durante o regime
#este contrafactual supõe um efeito restrito à duração do regime
#supõe que a dinâmica da violência foi condicionada pela duração do regime
#ou seja, cessado o regime, o efeito cessa
#supõe que o país chegaria "naturalmente" à taxa observada em 1986
#pode capturar o impacto da violência política do regime militar, no sentido estrito
#inclusive impacto indireto, pela deterioração das condições de segurança no trabalho
#para dados pré e pós regime, os valores são iguais aos dados
#interpolação mediante taxa exṕonencial
#O contrafactual exponencial assume crescimento contínuo da taxa de homicídios entre 1963 e 1986, produzindo uma trajetória suavizada que independe de incrementos discretos anuais

#Base com anos inicial e final
dados_cf2_base <- dados_1960_2022 |>
  dplyr::filter(ano %in% c(1963, 1985))

#Parâmetros demográficos
ano_inicial <- 1963
ano_final   <- 1985

tx_inicial_mce <- dados_cf2_base |>
  dplyr::filter(ano == ano_inicial) |>
  dplyr::pull(tx_mce)

tx_final_mce <- dados_cf2_base |>
  dplyr::filter(ano == ano_final) |>
  dplyr::pull(tx_mce)

T <- ano_final - ano_inicial

#Taxa exponencial de crescimento
r_mce <- (log(tx_final_mce) - log(tx_inicial_mce)) / T


#Série contrafactual exponencial completa
cf_duracao_evento_raw_mce <- dados_1960_2022 |>
  dplyr::mutate(
    cf_exp_mce = tx_inicial_mce * exp(r_mce * (ano - ano_inicial))) |>
  pull(cf_exp_mce)

#Aplicando sua regra institucional (efeito só durante o regime)
dados_1960_2022 <- dados_1960_2022 |>
  dplyr::mutate(
    cf_duracao_evento_mce = dplyr::if_else(
      ano >= 1963 & ano <= 1985,
      cf_duracao_evento_raw_mce,
      tx_mce
    )
  )

#Resultado final
dados_1960_2022$cf_duracao_evento_mce
plot(dados_1960_2022$cf_duracao_evento_mce)

#3. contrafactual de duração ampliada
#este contrafactual é um meio termo entre os anteriores
#inicialmente a tendência seria a mesma de 60-63, mas depois convergiria com os dados observados
#o marco da mudança é o fim do AI-5, em 1978, encerrando o terror total de Estado
#mas depois disso persistiu alguma violência política documentada e transição lenta
#a violência política decaiu, mas ascendeu a violência no contexto de mercados ilícitos
#assim, conjuga legado violento com processos internacionais de difusão do automóvel e do narcotráfico
# e a persistência de condições perigosas de trabalho
#para dados pré e pós regime, os valores são iguais aos dados

#Contrafactual com persistência e decaimento paramétrico

#CF3t′​=λt​⋅CF2t​+(1−λt​)⋅Yt​
#λt​=exp(−τt−1978​)

#parâmetro de decaimento
tau <- 10

dados_1960_2022 <- dados_1960_2022 |>
  mutate(
    lambda = case_when(
      ano <= 1978 ~ 1,
      ano > 1978  ~ exp(-(ano - 1978) / tau)
    ),
    cf_duracao_evento_amplo_mce = lambda * cf_efeito_max_mce +
      (1 - lambda) * tx_mce
  )

summary(dados_1960_2022$cf_duracao_evento_amplo_mce)
range(dados_1960_2022$lambda)




#comparar contrafactuais

taxa_e_cenarios_mce <- ggplot(dados_1960_2022, aes(x = ano ))+
  geom_line(aes(y = tx_mce, colour = "taxa de causas externas"), linewidth = 1.5)+
  geom_line(aes(y = cf_efeito_max_mce, colour =  "tendência pré-golpe"), linetype = "dashed")+
  geom_line(aes(y = cf_duracao_evento_mce, colour = "contrafactual 1963-1985"), linetype = "dashed")+
  geom_line(aes(y = cf_duracao_evento_amplo_mce, colour = "tend. pré-golpe decai após-78"), linetype = "dashed") + 
  geom_vline(xintercept = 1964, linetype="dotted")+
  geom_vline(xintercept = 1985, linetype = "dotted")+
  labs(
    title = "taxas de outras mortes violentas e contrafactuais",
    subtitle = "imputado (1960-1978), observado (1979-2022)",
    x = "ano",
    y = "taxa de causas externas (por 100 mil hab.)"
  ) + theme(legend.title = element_blank()) +
  theme_ipea(legend.position = "bottom") +  scale_color_ipea(palette = "Red-Blue-White")

taxa_e_cenarios_mce

save_ipeaplot(taxa_e_cenarios_mce, "taxas e cenários de mortalidade por causas externas",
              width = 8, height = 5,format = c("eps", "png"))

#análise descritiva dos contrafactuais

dados_1960_2022$cf_efeito_max_mce
summary(dados_1960_2022$cf_efeito_max_mce)

dados_1960_2022$cf_duracao_evento_mce
summary(dados_1960_2022$cf_duracao_evento_mce)

dados_1960_2022$cf_duracao_evento_amplo_mce
summary(dados_1960_2022$cf_duracao_evento_amplo_mce)


#resultado de interesse é o número de mortes excedentes, não a taxa, 
#por isso invertemos a diferença entre as taxas
#a taxa é um nível de violência

dados_1960_2022$excesso_mce_cf1 <- pmax(0,(dados_1960_2022$tx_mce - dados_1960_2022$cf_efeito_max_mce) *
                                          dados_1960_2022$população/100000)

sum(dados_1960_2022$excesso_mce_cf1)

dados_1960_2022$excesso_mce_cf2 <- pmax(0,(dados_1960_2022$tx_mce - dados_1960_2022$cf_duracao_evento_mce) *
                                          dados_1960_2022$população/100000)

sum(dados_1960_2022$excesso_mce_cf2)

dados_1960_2022$excesso_mce_cf3 <- pmax(0,(dados_1960_2022$tx_mce - dados_1960_2022$cf_duracao_evento_amplo_mce)*
                                          dados_1960_2022$população/100000)

sum(dados_1960_2022$excesso_mce_cf3)

#mortes violentas

dados_1960_2022$excesso_mortes_violentascf1 <- dados_1960_2022$excesso_homicidios_cf1 +
  dados_1960_2022$excesso_mce_cf1

dados_1960_2022$excesso_mortes_violentascf2 <- dados_1960_2022$excesso_homicidios_cf2 +
  dados_1960_2022$excesso_mce_cf2

dados_1960_2022$excesso_mortes_violentascf3<- dados_1960_2022$excesso_homicidios_cf3 +
  dados_1960_2022$excesso_mce_cf3

#==============================================================================
#contrafactual extremo
#==============================================================================

#homicídios
dados_1960_2022$homicidio_cf_extremo <- dados_1960_2022$tx_homicidios[4]

dados_1960_2022$homicidio_cf_extremo

dados_1960_2022$excesso_homicidios_cfex <- pmax(0,
                                                (dados_1960_2022$tx_homicidios-dados_1960_2022$homicidio_cf_extremo) * dados_1960_2022$população/100000)

dados_1960_2022$excesso_homicidios_cfex

summary(dados_1960_2022$excesso_homicidios_cfex)

sum(dados_1960_2022$excesso_homicidios_cfex)


# outras mortes por causas externas

dados_1960_2022$mce_cf_extremo <- dados_1960_2022$tx_mce[4]

dados_1960_2022$mce_cf_extremo

dados_1960_2022$excesso_mce_cfex <- pmax(0,
                                         (dados_1960_2022$tx_mce-dados_1960_2022$mce_cf_extremo) * dados_1960_2022$população/100000)

dados_1960_2022$excesso_mce_cfex

summary(dados_1960_2022$excesso_mce_cfex)

sum(dados_1960_2022$excesso_mce_cfex)


# mortes violentas

dados_1960_2022$excesso_mortes_violentas_cfex <- dados_1960_2022$excesso_homicidios_cfex +
  dados_1960_2022$excesso_mce_cfex

summary(dados_1960_2022$excesso_mortes_violentas_cfex)

#totais de 1960 a 1990

anos_interesse <- dados_1960_2022$ano >= 1960 &
  dados_1960_2022$ano <= 1990

#homicídios
sum(
  dados_1960_2022$excesso_homicidios_cf1[anos_interesse],
  na.rm = TRUE
)

sum(
  dados_1960_2022$excesso_homicidios_cf2[anos_interesse],
  na.rm = TRUE
)

sum(
  dados_1960_2022$excesso_homicidios_cf3[anos_interesse],
  na.rm = TRUE
)

sum(
  dados_1960_2022$excesso_homicidios_cfex[anos_interesse],
  na.rm = TRUE
)

#acidentes e suicídios
sum(
  dados_1960_2022$excesso_mce_cf1[anos_interesse],
  na.rm = TRUE
)

sum(
  dados_1960_2022$excesso_mce_cf2[anos_interesse],
  na.rm = TRUE
)

sum(
  dados_1960_2022$excesso_mce_cf3[anos_interesse],
  na.rm = TRUE
)

sum(
  dados_1960_2022$excesso_mce_cfex[anos_interesse],
  na.rm = TRUE
)

#mortes violentas

sum(
  dados_1960_2022$excesso_mortes_violentascf1[anos_interesse],
  na.rm = TRUE
)
sum(
  dados_1960_2022$excesso_mortes_violentascf2[anos_interesse],
  na.rm = TRUE
)

sum(
  dados_1960_2022$excesso_mortes_violentascf3[anos_interesse],
  na.rm = TRUE
)

sum(
  dados_1960_2022$excesso_mortes_violentas_cfex[anos_interesse],
  na.rm = TRUE
)

#Tabela resumo: 1964-1985

totais_1960_1990_wide <- tibble(
  tipo = c(
    "Homicídios",
    "Acidentes e suicídios",
    "Mortes violentas"
  ),
  cf1 = c(
    sum(dados_1960_2022$excesso_homicidios_cf1[anos_interesse], na.rm = TRUE),
    sum(dados_1960_2022$excesso_mce_cf1[anos_interesse], na.rm = TRUE),
    sum(dados_1960_2022$excesso_mortes_violentascf1[anos_interesse], na.rm = TRUE)
  ),
  cf2 = c(
    sum(dados_1960_2022$excesso_homicidios_cf2[anos_interesse], na.rm = TRUE),
    sum(dados_1960_2022$excesso_mce_cf2[anos_interesse], na.rm = TRUE),
    sum(dados_1960_2022$excesso_mortes_violentascf2[anos_interesse], na.rm = TRUE)
  ),
  cf3 = c(
    sum(dados_1960_2022$excesso_homicidios_cf3[anos_interesse], na.rm = TRUE),
    sum(dados_1960_2022$excesso_mce_cf3[anos_interesse], na.rm = TRUE),
    sum(dados_1960_2022$excesso_mortes_violentascf3[anos_interesse], na.rm = TRUE)
  ),
  cf_extremo = c(
    sum(dados_1960_2022$excesso_homicidios_cfex[anos_interesse], na.rm = TRUE),
    sum(dados_1960_2022$excesso_mce_cfex[anos_interesse], na.rm = TRUE),
    sum(dados_1960_2022$excesso_mortes_violentas_cfex[anos_interesse], na.rm = TRUE)
  ),
  total = c(
    sum(dados_1960_2022$homicidios_intencionais_rand_media[anos_interesse], n.rm = TRUE),
        sum(dados_1960_2022$MCE[anos_interesse], na.rm = TRUE),
        sum(dados_1960_2022$homicidios_intencionais_rand_media[anos_interesse], n.rm = TRUE)+
      sum(dados_1960_2022$MCE[anos_interesse], na.rm = TRUE)
    )
)

print(totais_1960_1990_wide)

write_excel_csv2(totais_1960_1990_wide, file.path(salvar_reconstrucao, "excessos_mortes_1960_1990.csv"))

#================================================================
#salvar resultados
#================================================================
stopifnot(
  !anyDuplicated(dados_1960_2022$ano),
  !any(is.na(dados_1960_2022$ano))
)

write.csv(dados_1960_2022, file.path(salvar_reconstrucao, "serie_historica_1960_em_diante.csv"), 
          row.names = FALSE)


# ==============================================================================
# DISPERSÃO: DOCUMENTADAS × ESTIMATIVAS
# ==============================================================================

dados_disp <- df_raw |>
  filter(ano >= 1960, ano <= 1990) |>
  transmute(
    documentadas = `mortos e desaparecidos documentados`,
    hmcd_exces1 = excesso_homicidios_cf1,
    hmcd_exces2 = excesso_homicidios_cf2,
    hmcd_exces3 = excesso_homicidios_cf3,
    mce_exces1 = excesso_mce_cf1,
    mce_exces2 = excesso_mce_cf2,
    mce_exces3 = excesso_mce_cf3
  )

dados_long_disp <- dados_disp |>
  pivot_longer(-documentadas, names_to = "cenario", values_to = "excesso")

# Criação de modelos de regressão
summary(lm(hmcd_exces1 ~ documentadas, data = dados_disp))
summary(lm(hmcd_exces2 ~ documentadas, data = dados_disp))
summary(lm(hmcd_exces3 ~ documentadas, data = dados_disp))
summary(lm(mce_exces1 ~ documentadas, data = dados_disp))
summary(lm(mce_exces2 ~ documentadas, data = dados_disp))
summary(lm(mce_exces3 ~ documentadas, data = dados_disp))

# Preparar labels para as equações
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

# Gráfico de dispersão
vitimas_oficiais_excesso_mortes <- ggplot(dados_long_disp, aes(x = documentadas, y = excesso)) +
  geom_point(
    alpha = 0.6,
    size = 2,
    shape = 4
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1,
    color = "steelblue"
  ) +
  geom_text(
    data = labels_eq,
    aes(
      x = Inf,
      y = -Inf,
      label = label
    ),
    hjust = 1.1,
    vjust = -0.5,
    size = 3,
    color = "red"
  ) +
  geom_label(
    data = labels_eq,
    aes(
      x = Inf,
      y = -Inf,
      label = label
    ),
    hjust = 1.05,
    vjust = -0.4,
    size = 3,
    color = "red",
    fill = "white",
    label.size = 0,
    alpha = 0.85
  ) +
  facet_wrap(~ cenario, scales = "free_y") +
  labs(
    title = "Mortes documentadas vs. estimativas de excesso de violência",
    subtitle = "Equações de regressão por cenário (1964–1985)",
    x = "Mortes documentadas (por ano)",
    y = "Excesso estimado de mortes violentas"
  ) +
  theme_ipea() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

vitimas_oficiais_excesso_mortes

save_ipeaplot(vitimas_oficiais_excesso_mortes, "vítimas oficiais e excesso de mortes violentas",
              path = pasta_saida, width = 8, height = 5,
              format = c("eps", "png"))
