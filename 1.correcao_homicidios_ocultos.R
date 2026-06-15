#reconstrução homicídios
#==============================================================

#pacotes necessários

library(tidyverse)
library(readxl)
library(ggplot2)
library(ipeaplot)
library(strucchange)
library(broom)
library(patchwork)

#arquivos

#ATENÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#RENOMEIE O ENDEREÇO ABAIXO COM A LOCALIZAÇÃO DO ARQUIVO dados_mortes_1979_2022

arquivo_base_homicidios_ocultos <- "/home/matheus/Documentos/IPEA/modelos/reconstrução homicídios/dados_mortes_1979_2022.xlsx"

dados_mortes <- read_excel("~/Documentos/IPEA/modelos/reconstrução homicídios/dados_mortes_1979_2022.xlsx", 
                           sheet = "Planilha1", col_types = c("numeric", "text", "text", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric"))

#ATENÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#RENOMEIE O ENDEREÇO ABAIXO COM A PASTA ONDE QUER SALVAR OS RESULTADOS

salvar_reconstrucao <- "/home/matheus/Documentos/IPEA/modelos/reconstrução homicídios/reconstrucao_homicidios_desde_1960"

str(dados_mortes)

dados_mortes <- subset(dados_mortes, !is.na(dados_mortes$agressoes_e_confrontos))
dados_mortes$MCE_total <- dados_mortes$MCE

dados_mortes$prop_hopmmd <- (dados_mortes$agressoes_e_confrontos+1) /((dados_mortes$MG - dados_mortes$MCMD)+1)

dados_mortes$prop_hopmvii <- (dados_mortes$agressoes_e_confrontos+1) /((dados_mortes$MCE_total - dados_mortes$MVII)+1)

dados_mortes$prop_MVII_ocultos_CMD <- (dados_mortes$MVII+1)/ (((dados_mortes$MG - dados_mortes$MCMD))+1)

#==================================================================
#simulação de dados
#==================================================================
#"Soares, Adauto Martins; Cortez-Escalante, Juan José; França, Elisabeth. Revisão dos métodos de correção de óbitos e dimensões de qualidade da causa básica por acidentes e violências no Brasil. Ciência & Saúde Coletiva [online]. 2016, v. 21, n. 12 [Accessed 15 May 2026] , pp. 3803-3818. Available from: <https://doi.org/10.1590/1413-812320152112.13682015>. ISSN 1678-4561. https://doi.org/10.1590/1413-812320152112.13682015.:
# Soares, Cortes-Escalante e França (2016) concluem que as mortes por causas externas ocultas por mal definidas...
#... não é significativamente diferente das demais causas. Logo, pode-se considerar que tende a ser proporcional.
#Cerqueira, D. R. de C., & Lins, G. de O. A.. (2025). Mapa dos homicídios ocultos no Brasil entre 1996 e 2021. Ciência & Saúde Coletiva, 30(10), e16612025. https://doi.org/10.1590/1413-812320253010.16612025
#Cerqueira e Lins (2025) identificam 43,6% de homicídios ocultos entre MVII.
#Ou seja, a proporção parece praticamente aleatória, próxima de 50%.
#como a correção que pretendo é probabilística, não basta um sorteio
#o método 1 é híbrido, combina proporções deterministas com incerteza dos sorteios
#o método 1 é aplicado às mortes por causas mal definidas (MCMD)
#inclusive supomos que parte das MCMD seriam MVII, não só agressões e confrontos
#a literatura médica aponta que MCMD contém mortes violentas (MCE_total) mal classificadas
#porém, uma análise usando-o também para mortes violentas por intenção indeterminada (MVII)
#o método 2 supõe aleatoriedade total:
#a proporção de MVII que poderiam ser homicídios ocultos é aleatória, e esta proporção...
#é usada numa binomial invertida com probabilidade aleatória
#assim, o método 2 pressupõe incerteza radical, pois as MVII podem ter vários motivos:
#erro administrativo, erro policial-judiciário e ocultação deliberada da letalidade policial
#enquanto no método 1 a proporção é função das razões entre números do ano e Estado,
#no método 2 cada simulação de cada observação deve ter uma proporção aleatória

# Número de simulações
n_simulacoes <- 3000

# Função segura para qbinom que lida com NAs
safe_qbinom <- function(p, size, prob) {
  result <- rep(NA, length(p))
  valid <- !is.na(size) & !is.na(prob) & prob >= 0 & prob <= 1 & size >= 0
  if (any(valid)) {
    result[valid] <- qbinom(p[valid], size[valid], prob[valid])
  }
  return(result)
}

# Função auxiliar: amostra p de uma distribuição Beta
# mu    = proporção estimada (média da Beta)
# n_obs = tamanho do denominador usado para estimar mu
#         (concentração: quanto maior, mais confiante em mu)
# cap   = concentração máxima (evita Beta quase-pontual em amostras grandes)

calibrar_beta <- function(mu, n_obs, cap = 100) {
  phi   <- pmin(pmax(n_obs, 2), cap)   # concentração entre 2 e cap
  alpha <- mu * phi
  beta  <- (1 - mu) * phi
  rbeta(length(mu),
        pmax(alpha, 0.01),
        pmax(beta,  0.01))
}

# Função para calcular as estimativas em cada simulação
# Agora retorna uma lista com dois resultados
simular_estimativas <- function(seed) {
  set.seed(seed)
  dados_temp <- dados_mortes
  n_rows <- nrow(dados_temp)
  
  # ============================================================
  # 1. MCMD → homicídios ocultos
  # MUDANÇA (Opção A): p amostrado de Beta, não fixo
  # ============================================================
  
  denom_cmd <- dados_temp$MG - (dados_temp$MCMD + dados_temp$MVII)
  
  prop_CMD <- dados_temp$agressoes_e_confrontos / denom_cmd
  prop_CMD <- pmin(pmax(prop_CMD, 0), 1)
  
  # >>> ANTES: safe_qbinom(runif(n_rows), dados_temp$MCMD, prop_CMD)
  p_CMD <- calibrar_beta(prop_CMD, n_obs = denom_cmd)
  homicidios_ocultos_CMD <- safe_qbinom(
    runif(n_rows),
    dados_temp$MCMD,
    p_CMD                   # p agora tem incerteza Beta
  )
  
  # ============================================================
  # 2. MVII → homicídios ocultos
  # ============================================================
  
  denom_mvi <- dados_temp$MCE_total - dados_temp$MVII
  
  prop_MVII <- dados_temp$agressoes_e_confrontos / denom_mvi
  prop_MVII <- pmin(pmax(prop_MVII, 0), 1)
  
  # ------------------
  # MÉTODO 1: Beta-Binomial (Opção A)
  # >>> ANTES: safe_qbinom(runif(n_rows), dados_temp$MVII, prop_MVII)
  # ------------------
  
  p_MVII <- calibrar_beta(prop_MVII, n_obs = denom_mvi)
  homicidios_ocultos_MVII_prop <- safe_qbinom(
    runif(n_rows),
    dados_temp$MVII,
    p_MVII                  # p agora tem incerteza Beta
  )
  
  round(homicidios_intencionais_prop <- 
          dados_temp$agressoes_e_confrontos +
          homicidios_ocultos_CMD +
          homicidios_ocultos_MVII_prop, 0)
  
  # ------------------
  # MÉTODO 2: Beta(5, 5) centrada em 0,5 (Opção B)
  # >>> ANTES: runif(n_rows)   [equivale a Beta(1,1)]
  # ------------------
  
  # phi = 10 → Beta(5, 5): média 0,5, desvio-padrão ≈ 0,15
  # Aumente phi para concentrar mais em 0,5; diminua para mais incerteza
  phi_prior <- 10
  p_rand    <- rbeta(n_rows,
                     shape1 = 0.5 * phi_prior,   # = 5
                     shape2 = 0.5 * phi_prior)   # = 5
  
  homicidios_ocultos_MVII_rand <- safe_qbinom(
    runif(n_rows),
    dados_temp$MVII,
    p_rand
  )
  
  homicidios_intencionais_rand <- 
    round(dados_temp$agressoes_e_confrontos +
            homicidios_ocultos_CMD +
            homicidios_ocultos_MVII_rand, 0)
  
  # Robustez
  homicidios_ocultos_CMD[is.na(homicidios_ocultos_CMD)]             <- 0
  homicidios_ocultos_MVII_prop[is.na(homicidios_ocultos_MVII_prop)] <- 0
  homicidios_ocultos_MVII_rand[is.na(homicidios_ocultos_MVII_rand)] <- 0
  homicidios_intencionais_prop[is.na(homicidios_intencionais_prop)] <- 0
  homicidios_intencionais_rand[is.na(homicidios_intencionais_rand)] <- 0
  
  return(list(
    homicidios_CMD       = homicidios_ocultos_CMD,
    homicidios_MVII_prop = homicidios_ocultos_MVII_prop,
    homicidios_MVII_rand = homicidios_ocultos_MVII_rand,
    total_prop           = homicidios_intencionais_prop,
    total_rand           = homicidios_intencionais_rand
  ))
}

# Verificar estrutura dos dados
str(dados_mortes)

# Verificar valores problemáticos

summary(dados_mortes[, c("MG", "MCMD", "MCE_total", "MVII","agressoes_e_confrontos")])

# Verificar se há valores negativos ou zeros problemáticos
cat("\nVerificando problemas:\n")
cat("Linhas com MG - MCMD <= 0:", sum((dados_mortes$MG - (dados_mortes$MCMD + dados_mortes$MVII)) <= 0, na.rm = TRUE), "\n")
cat("Linhas com MCE_total - MVII <= 0:", sum((dados_mortes$MCE_total - dados_mortes$MVII) <= 0, na.rm = TRUE), "\n")

# Se houver muitos problemas, ajustar os dados
problemas <- (dados_mortes$MG - (dados_mortes$MCMD + dados_mortes$MVII)) <= 0
if (any(problemas, na.rm = TRUE)) {
  cat("Ajustando valores problemáticos...\n")
  # Adicionar um pequeno valor para evitar divisão por zero
  dados_mortes$MG[problemas] <- dados_mortes$MG[problemas] + 1
}

problemas2 <- (dados_mortes$MCE_total - dados_mortes$MVII) <= 0
if (any(problemas2, na.rm = TRUE)) {
  cat("Ajustando valores problemáticos (MCE_total-MVII)...\n")
  dados_mortes$MCE_total[problemas2] <- dados_mortes$MCE_total[problemas2] + 1
}

# Criar matrizes para armazenar resultados de ambos os métodos
resultados_prop <- matrix(0, nrow = nrow(dados_mortes), ncol = n_simulacoes)
resultados_rand <- matrix(0, nrow = nrow(dados_mortes), ncol = n_simulacoes)

cat("\nIniciando simulações...\n")
pb <- txtProgressBar(min = 0, max = n_simulacoes, style = 3)

res_CMD        <- matrix(0, nrow = nrow(dados_mortes), ncol = n_simulacoes)
res_MVII_prop  <- matrix(0, nrow = nrow(dados_mortes), ncol = n_simulacoes)
res_MVII_rand  <- matrix(0, nrow = nrow(dados_mortes), ncol = n_simulacoes)
res_total_prop <- matrix(0, nrow = nrow(dados_mortes), ncol = n_simulacoes)
res_total_rand <- matrix(0, nrow = nrow(dados_mortes), ncol = n_simulacoes)


for (i in 1:n_simulacoes) {
  r <- simular_estimativas(i)
  
  res_CMD[, i]        <- r$homicidios_CMD
  res_MVII_prop[, i]  <- r$homicidios_MVII_prop
  res_MVII_rand[, i]  <- r$homicidios_MVII_rand
  res_total_prop[, i] <- r$total_prop
  res_total_rand[, i] <- r$total_rand
  
  setTxtProgressBar(pb, i)
}

close(pb)

# Função segura para quantil
safe_quantile <- function(x, probs) {
  if (all(is.na(x)) || length(x) == 0) {
    return(NA)
  }
  quantile(x, probs = probs, na.rm = TRUE)
}

# MCMD → homicídios ocultos
dados_mortes$homicidios_ocultos_CMD_media <- rowMeans(res_CMD, na.rm = TRUE)

# MVII → homicídios ocultos
dados_mortes$homicidios_ocultos_MVII_prop_media <- rowMeans(res_MVII_prop, na.rm = TRUE)
dados_mortes$homicidios_ocultos_MVII_rand_media <- rowMeans(res_MVII_rand, na.rm = TRUE)

# Totais
dados_mortes$homicidios_intencionais_prop_media <-
  round(rowMeans(res_total_prop, na.rm = TRUE), 0)

dados_mortes$homicidios_intencionais_rand_media <-
  round(rowMeans(res_total_rand, na.rm = TRUE), 0)

dados_mortes$homicidios_intencionais_prop_sd <- apply(res_total_prop, 1, sd, na.rm = TRUE)

dados_mortes$homicidios_intencionais_rand_sd <- apply(res_total_rand, 1, sd, na.rm = TRUE)

#intervalos de confiança:

dados_mortes$homicidios_intencionais_prop_li <-
  round(apply(res_total_prop, 1, safe_quantile, probs = 0.025), 0)

dados_mortes$homicidios_intencionais_prop_ls <-
  round(apply(res_total_prop, 1, safe_quantile, probs = 0.975), 0)

dados_mortes$homicidios_intencionais_rand_li <-
  round(apply(res_total_rand, 1, safe_quantile, probs = 0.025), 0)

dados_mortes$homicidios_intencionais_rand_ls <-
  round(apply(res_total_rand, 1, safe_quantile, probs = 0.975), 0)


# Visualizar resultados
cat("\n==============================================\n")
cat("RESULTADOS - MÉTODO 1 (proporção variável)\n")
cat("==============================================\n")
print(head(dados_mortes[, c(
  "homicidios_intencionais_prop_media",
  "homicidios_intencionais_prop_li",
  "homicidios_intencionais_prop_ls",
  "homicidios_intencionais_prop_sd"
)], 10))


cat("\n==============================================\n")
cat("RESULTADOS - MÉTODO 2 (proporção aleatória  )\n")
cat("==============================================\n")
print(head(dados_mortes[, c(
  "homicidios_intencionais_rand_media",
  "homicidios_intencionais_rand_li",
  "homicidios_intencionais_rand_ls",
  "homicidios_intencionais_rand_sd"
)], 10))

# Resumo estatístico

# Criar gráficos comparativos
if (nrow(dados_mortes) > 1) {
  cat("\nCriando visualizações comparativas...\n")
  
  # Configurar área de plotagem para 2 gráficos lado a lado
  par(mfrow = c(1, 2))
  
  # Gráfico para Método 1
  plot(dados_mortes$homicidios_intencionais_prop_media, 
       type = "l", 
       main = "Método 1: Proporção Variável",
       xlab = "Observação", 
       ylab = "Média Estimada",
       ylim = range(c(dados_mortes$homicidios_intencionais_prop_li, 
                      dados_mortes$homicidios_intencionais_prop_ls),
                    na.rm = TRUE))
  lines(dados_mortes$homicidios_intencionais_prop_li, col = "red", lty = 2)
  lines(dados_mortes$homicidios_intencionais_prop_ls, col = "red", lty = 2)
  legend("topright", 
         legend = c("Média", "IC 95%"), 
         col = c("black", "red"), 
         lty = c(1, 2),
         bty = "n")
  
  # Gráfico para Método 2
  plot(dados_mortes$homicidios_intencionais_rand_media, 
       type = "l", 
       main = "Método 2: proporção aleatória ( )",
       xlab = "Observação", 
       ylab = "Média Estimada",
       ylim = range(c(dados_mortes$homicidios_intencionais_rand_li, 
                      dados_mortes$homicidios_intencionais_rand_ls),
                    na.rm = TRUE))
  lines(dados_mortes$homicidios_intencionais_rand_li, col = "blue", lty = 2)
  lines(dados_mortes$homicidios_intencionais_rand_ls, col = "blue", lty = 2)
  legend("topright", 
         legend = c("Média", "IC 95%"), 
         col = c("black", "blue"), 
         lty = c(1, 2),
         bty = "n")
  
  # Resetar área de plotagem
  par(mfrow = c(1, 1))
  
  # Gráfico comparativo das médias
  plot(dados_mortes$homicidios_intencionais_prop_media,
       dados_mortes$homicidios_intencionais_rand_media,
       main = "Comparação entre Métodos",
       xlab = "Método 1 (Proporção Variável)",
       ylab = "Método 2 (proporção aleatória  )")
  abline(0, 1, col = "red", lty = 2)  # Linha de igualdade
  text(mean(dados_mortes$homicidios_intencionais_prop_media, na.rm = TRUE),
       mean(dados_mortes$homicidios_intencionais_rand_media, na.rm = TRUE),
       paste("r =", round(cor(dados_mortes$homicidios_intencionais_prop_media,
                              dados_mortes$homicidios_intencionais_rand_media,
                              use = "complete.obs"), 4)),
       pos = 2.2, col = "blue")
}

# Criar resumo final simplificado
cat("\n==============================================\n")
cat("RESUMO FINAL SIMPLIFICADO\n")
cat("==============================================\n")

dados_mortes$diferenca_metodos <-
  dados_mortes$homicidios_intencionais_prop_media -
  dados_mortes$homicidios_intencionais_rand_media


# Criar um dataframe resumo
resumo_final <- data.frame(
  Observação = 1:nrow(dados_mortes),
  Agressões = dados_mortes$agressoes_e_confrontos,
  Método1_Prop_Variável = round(dados_mortes$homicidios_intencionais_prop_media, 0),
  Método2_Prop_Rand    = round(dados_mortes$homicidios_intencionais_rand_media, 0),
  Diferença            = round(dados_mortes$diferenca_metodos, 0),
  Percentual_Aumento = ifelse(dados_mortes$agressoes_e_confrontos > 0,
                              round(100 * (dados_mortes$homicidios_intencionais_prop_media - dados_mortes$agressoes_e_confrontos) / dados_mortes$agressoes_e_confrontos, 1),
                              NA)
)

colnames(resumo_final) <- c("Obs", "Agressões", "Método1", "Método2", "Dif", "% Aumento vs Agressões")

print(head(resumo_final, 15))

# Salvar resultados se necessário


dados_mortes$tx_agressoes <- dados_mortes$agressoes_e_confrontos*1e5/dados_mortes$populacao

dados_mortes$tx_homicid_ajprop <- dados_mortes$homicidios_intencionais_prop_media*100000/dados_mortes$populacao

dados_mortes$tx_homicid_ajrand <- dados_mortes$homicidios_intencionais_rand_media*100000/dados_mortes$populacao



str(dados_mortes)

write.csv2(   dados_mortes,   file.path(salvar_reconstrucao, "painel_nacional_1979_2022.csv") )


#===============================================
#comparação com o Mapa da Violência
################################################
#Os homicídios ocultos do Mapa da Violência foram estimados por Lins e Cerqueira
#O método foi de regressão logística multinomial, dados de 2012 a 2022
#restringem-se à estimação de homicídios ocultos por MVII
#por isso, serão usados como validação externa e seleção do melhor modelo
#critérios: maior correlação linear e menor raiz do erro quadrático médio

#homicídios mais mortes violentas indeterminadas por ajuste proporcional

dados_teste <- subset(dados_mortes, !is.na(dados_mortes$homicidios_ocultos_Mapa_da_Violencia))

cor_prop_x_MdV <- cor(dados_mortes$homicidios_ocultos_MVII_prop_media,
                      dados_mortes$homicidios_ocultos_Mapa_da_Violencia, use = "complete.obs")

lm_prop_x_MdV <- lm(dados_mortes$homicidios_ocultos_Mapa_da_Violencia ~ dados_mortes$homicidios_ocultos_MVII_prop_media)

eqm_lm_prop_x_MdV <- sqrt(mean(lm_prop_x_MdV$residuals^2))

cor_rand_x_MdV <- cor(x = dados_mortes$homicidios_ocultos_MVII_rand_media,
                      y =  dados_mortes$homicidios_ocultos_Mapa_da_Violencia, use = "complete.obs")

lm_rand_x_MdV <- lm(dados_mortes$homicidios_ocultos_Mapa_da_Violencia ~ dados_mortes$homicidios_ocultos_MVII_rand_media)

eqm_rand_x_MdV <- sqrt(mean(lm_rand_x_MdV$residuals^2))

# Tabela de resultados da validação
resultados_validacao <- tibble(
  Métrica = c("Correlação", "Raiz do Erro Quadrático Médio"),
  Método_Proporção = c(round(cor_prop_x_MdV, 4), round(eqm_lm_prop_x_MdV, 2)),
  Método_Aleatório = c(round(cor_rand_x_MdV, 4), round(eqm_rand_x_MdV, 2))
)

print(resultados_validacao)

write.csv2(resultados_validacao, "valida_ext_mdv.csv")

dados_mortes$MCE <- dados_mortes$MCE_total - (dados_mortes$homicidios_ocultos_MVII_rand_media + 
                                                dados_mortes$agressoes_e_confrontos)

dados_mortes$mortes_violentas <- dados_mortes$MCE + dados_mortes$homicidios_intencionais_rand_media

summary(dados_mortes$MCE)

#####################################################
# ESTIMATIVA NACIONAL

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


dados_mortes_ano$anos <- as.numeric(dados_mortes_ano$ano)

homicidios_nacional <- ggplot(dados_mortes_ano, aes(x = anos)) +
  geom_line(aes(y = tx_agressoes, colour =  "agressões"))+
  scale_x_continuous()+
  geom_line(aes(y = tx_homicid_ajrand, colour = "homicídios aj rand")) +
  labs(title = "estimativas de taxa de homicídios", y = "taxa por 100 mil habitantes", colour = "legenda") +
  theme_ipea() + scale_color_ipea(pallete = "Red-Blue-White") + scale_fill_ipea(pallete = "Red-Blue-White")

print(homicidios_nacional)

save_ipeaplot(homicidios_nacional, file.name = "taxa_nacional", 
              path = salvar_reconstrucao, format = c("png", "eps"))

write.csv(dados_mortes_ano, file.path(salvar_reconstrucao, "estimativa_nacional.csv"), 
          row.names = FALSE)

