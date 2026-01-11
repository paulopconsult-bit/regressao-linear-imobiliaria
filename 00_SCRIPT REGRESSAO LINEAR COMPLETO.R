############################################################
## SCRIPT GENÉRICO — REGRESSÃO LINEAR SIMPLES
# Os valores preditos pelo modelo são estimativas da média da população para aqueles valores de X, 
# obtidas a partir da amostra usada no ajuste.
############################################################


############################################################
## 0. SETUP INICIAL
## Objetivo: preparar o ambiente, carregar pacotes e definir
##           configurações globais antes da análise.
############################################################

# Limpeza do ambiente
rm(list = ls())
while (dev.cur() > 1) dev.off() # Fecha todos os gráficos e configurações de aberturas de gráficos

# Carregamento das bibliotecas necessárias
library(readxl)      # Importação de arquivos Excel
library(ggplot2)     # Visualizações
library(skimr)       # Inspeção descritiva
library(moments)     # Assimetria e curtose
library(dplyr)       # Manipulação de dados
library(rstatix)     # Testes estatísticos
library(rmarkdown)   # Relatórios Rmd
library(car)         # Diagnósticos de regressão
library(knitr)       # Formatação de tabelas
library(Metrics)     # Métricas de erro (MAPE, SSE etc.)
library(GGally)      # Correlogramas (ggpairs)
library(reshape2)    # Para melt() mapa de calor

# Configurações globais
options(scipen = 999)   # Evita notação científica
# options(scipen = 0)   # (Opcional) Retorna notação científica

# Diretório de trabalho (ajuste conforme necessário)
setwd("G:\\Meu Drive\\R\\Regressao-Linear-Imobiliario")


############################################################
## 1. DEFINA A BASE
############################################################

base = read.table("base_lm_imoveis.txt", header = TRUE, 
                  sep = ";", dec = ",")
# header = TRUE > quer dizer que a primeira linha contem titulo
# separador "\t" significa que é um TAB
# dec = ".", avisamos para o R que o separador decimal é o PONTO


# base <- read_excel("Regressao linear simples.xls",
#                    sheet="Imobiliario" ) 


############################################################
## 1.1 DEFINA AS VARIÁVEIS AQUI
############################################################


names(base)
skim(base)

id <- "Id" # Não obrigatório / Estuda duplicidades
anyDuplicated(base[[id]])
# valor =  0 → não há duplicados / campo não é essencial para analise.
# 1. Quantidade de registros e variáveis
n_registros <- nrow(base)
n_variaveis <- ncol(base)
# 2. Verificar duplicidades no campo Id
duplicados_id <- sum(duplicated(base$Id))
# 3. Verificar duplicidades de linhas ignorando o Id
duplicatas_linhas <- base[duplicated(base[, setdiff(names(base), "Id")]), ]
n_registros_duplicatas_linhas <- nrow(duplicatas_linhas)
# 4. Verificar se o Id é sequencial
id_sequencial <- all(diff(sort(base$Id)) == 1)
# 5. Verificar mínimo e máximo do Id (o único resumo que faz sentido)
id_min <- min(base$Id)
id_max <- max(base$Id)
# 6. IDs faltantes (buracos na sequência)
ids_faltantes <- setdiff(id_min:id_max, base$Id)

############################################################
## 1.2 DEFINIR O MODELO E VARIAVEIS
############################################################
# Defina a variável dependente (Y) 
var_y <- "Reais_m2"

# Defina as variáveis explicativas (X) para SIMPLES OU COMPOSTA:

# Escolha aqui: "simples" ou "multipla"
modo_modelo <- "multipla"

if (modo_modelo == "simples") {
  
  var_x <- c("Deslocamento_metro_Km")
  
} else if (modo_modelo == "multipla") {
  
  var_x <- c("Deslocamento_metro_Km",
             "Tempo_Construcao",
             "Comercios_quadra")
} else {
  stop("ERRO: modo_modelo deve ser 'simples' ou 'multipla'.")
}

# Verificação 
var_x
class(var_x)

############################################################
## 2. INSPEÇÃO INICIAL DA BASE
############################################################


str(base)           # avaliar variáveis de texto
summary(base)       # quantidade de observações é "obs."


############################################################
## 3. DESCRITIVAS DAS VARIÁVEIS
############################################################

summary(base[[var_y]])

quantile(base[[var_y]], probs = c(0.01, 0.99))
# 1% dos valores estão ≤ 2.5600 e 99% ≤ 14.0104

skewness(base[[var_y]])
# [1] 0.1737147 -> A distribuição é quase simétrica mas esxites uma Assimetria à direita, à Média > Mediana
#
# Distribuição aproximadamente Simétrica quando skewness = 0 media ≈ mediana.
# Assimetria/Distorção à esquerda quando skewness < 0; geralmente Média < Mediana 
# Assimetria à direita, se skewness > 0; geralmente Média > Mediana

# Descritivas da variável explicativa (ou selecionar)
summary(base)

hist(base[[var_y]],
     col="darkturquoise",
     main=paste("Histograma de", var_y),
     xlab=var_y)

############################################################
## 4. CORRELAÇÃO DE PEARSON
#
# Interpretação dos limites: 
# +1 -> correlação positiva perfeita 
# 0 -> ausência de correlação linear 
# -1 -> correlação negativa perfeita
#
#   |------|----------------|
#   | 0.00–0.09 | Muito fraca | RUIDO PURO: Correlação fraca NÃO significa erro alto. Significa independência entre as variáveis.
#   | 0.10–0.19 | Muito fraca | RUIDO LEVE: Correlação fraca NÃO significa erro alto. Significa independência entre as variáveis.
#   | 0.20–0.29 | Fraca | quase RUIDO: Correlação fraca NÃO significa erro alto. Significa independência entre as variáveis.
#   | 0.30–0.39 | Fraca | Meio Termo: Existe uma tendência leve entre as variáveis, mas ela não é forte o suficiente para ser considerada uma relação sólida.
#   | 0.40–0.59 | Moderada | Aqui existe relação, mas não é forte o suficiente para causar multicolinearidade.
#   | 0.60–0.79 | Forte | risco de MULTICOLINEARIDADE: variáveis explicativas se explicam entre si
#   | 0.80–1.00 | Muito forte | MULTICOLINEARIDADE severa: variáveis explicativas se explicam entre si
############################################################

# Correlação negativa forte (no exemplo original)
for (x in var_x) { 
  r <- cor(base[[var_y]], base[[x]]) 
  cat(paste0("Correlação entre ", var_y, " e ", x, ": ", round(r, 4), "\n")) 
}

# Correlação entre Reais_m2 e Deslocamento_metro_Km: -0.756 é forte com tendência de queda de preço.
# Correlação entre Reais_m2 e Tempo_Construcao: -0.211 é fraca com tendência de queda de preço. 
# Correlação entre Reais_m2 e Comercios_quadra: 0.6126 é ligeiramente forte com tendência de aumento de preço.


# Correlograma (Y e a X escolhida) 
var_x
var_x[1] # Escolhido Deslocamento_metro_Km porque tem a maior |correlação|
ggpairs(base[, c(var_y, var_x[1])], title = "Correlograma")
# As curvas são construídas a partir dos valores reais da variável(Distancia_metro_Km e Mil_reais_m2)
# Essas curvas são kernel density plots para suavizar a imagem


# Correlograma Completo (Y + Todas as X) 
variaveis_modelo <- c(var_y, var_x) 
GGally::ggpairs( 
  base[, variaveis_modelo], 
  title = "Correlograma — Variáveis do Modelo" 
)

############################################################
## 4.1 MAPA DE CALOR — Heatmap da Matriz de Correlação (Cores Invertidas)
############################################################

# Seleciona apenas as variáveis do modelo
vars_cor <- base[, c(var_y, var_x)]
class(vars_cor)


# Matriz de correlação
mat_cor <- round(cor(vars_cor), 3)

# Converte para formato longo
mat_cor_melt <- melt(mat_cor)

# Heatmap com vermelho = positivo e azul = negativo (tons suaves)
ggplot(mat_cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#6BAED6",      # azul suave (negativo)
    mid = "white",        # neutro
    high = "#FB6A4A",     # vermelho mais forte, porém elegante (positivo)
    midpoint = 0,
    limit = c(-1, 1),
    name = "Correlação"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Mapa de Calor — Matriz de Correlação",
    x = "",
    y = ""
  )


############################################################
## 5. GRÁFICO DE DISPERSÃO
############################################################
base[["Deslocamento_metro_Km"]]

ggplot(base, 
       aes(x = .data[[var_x[1]]], 
           y = .data[[var_y]], 
           colour = .data[[var_y]])) + geom_point(alpha = 0.3) + 
  # geom_smooth(method = "lm", se = TRUE) # Mostra a reta da regressão linear no gráfico
  # OU
  geom_smooth()
  

############################################################
## 5.1 GRÁFICOS DE DISPERSÃO PARA OUTRAS VARIÁVEIS X
############################################################
# Neste exercicio, temos 03 variáveis X
var_x

ggplot(base, 
       aes(x = .data[[var_x[2]]], 
           y = .data[[var_y]], 
           colour = .data[[var_y]])) + geom_point(alpha = 0.4) + 
  # geom_smooth(method = "lm", se = TRUE) # Mostra a reta da regressão linear no gráfico
  # OU
  geom_smooth()


ggplot(base, 
       aes(x = .data[[var_x[3]]], 
           y = .data[[var_y]], 
           colour = .data[[var_y]])) + geom_point(alpha = 0.4) + 
  # geom_smooth(method = "lm", se = TRUE) # Mostra a reta da regressão linear no gráfico
  # OU
  geom_smooth()


############################################################
## 6. MODELO DE REGRESSÃO LINEAR
# 𝑌=𝛽0+𝛽1𝑋...+𝛽3𝑋3...
# Y = Mil_reais_m2
# X = Distancia_metro_Km
# β₀ = intercepto # Valor de Y quando X = 0.  
# β₁ = inclinação # Para cada "1 unidade" que X aumenta, Y cai em média "B1" unidades.
############################################################


# Monta a fórmula automaticamente 
formula_lm <- as.formula( 
  paste(var_y, "~", paste(var_x, collapse = " + ")) 
)

# Ajusta o modelo
modelo <- lm(formula_lm, data = base)


# 𝑌=𝛽0+𝛽1𝑋1...+𝛽3𝑋3...
# 𝑌=16.493840 - 5.739872 * 𝑋1 - 0.041448 * 𝑋2 + 0.183508 * 𝑋3
summary(modelo)

# Multiple R-squared (R2)
R2 <- summary(modelo)$r.squared  
R2
# Para regressão simples, um R² acima de 62% já costuma ser considerado bom.
# O coeficiente de determinação (explicação) diz que 62% da variação de Mil_reais_m2 é explicada pelas 3 variáveis

# Adjusted R-squared (R2adj)
R2adj <- summary(modelo)$adj.r.squared
R2adj
# 0.6168737
# Mesmo após a penalização do modelo devido ao número de variáveis, o modelo ainda explica cerca de 61,7% da variação do preço por m².
## Em regressão linear simples "R²" e "R² ajustado" são praticamente iguais. 
## Em Reg. Multipla usar "R² ajustado".


# F-statistic
# estatística F, Avaliando o  p-value: < 0.00000000000000022 (métrica usada para quando a lm é composta)
F_statistic <- summary(modelo)$fstatistic
F_statistic
# Ela testa se todas os betas são zero ao mesmo tempo: ou seja “Nenhuma variável explica Y.”
#𝐻0:𝛽1=𝛽2=𝛽3=⋯=0  Nenhuma variável explica Y. O modelo não tem poder preditivo.
# Se a Estatística F for alta e o p‑valor for muito pequeno: então rejeita H0 (é o que queremos, a hipotese alternativa H1)
# H1:𝛽1=𝛽2=𝛽3=⋯<>0 Pelo menos uma variável explica Y. O modelo tem poder preditivo real.


# Avalia-se o p-valor de todas as variáveis do seu modelo, se são altamente significativas.


# DICA:
# MODELO COM AS VARIAVEIS REMOVIDAS PODE-SE ANALISAR O MAPE para comparação entre o antes e o depois.
# Quando tiver variaveis que são removidas devido alto p-valor, mas o R2 permanece "o mesmo ou bem próximo"

# CASO PRECISE DE AJUSTE DAS VARIAVEIS EXPLICATIVAS, ajustar "var_x"
# var_x <- c("Distancia_metro_Km",
#            "Idade_imovel",
#            "Comercios_proximos")

############################################################
## 7. TESTE DE HIPÓTESE PARA β1 / β0
############################################################

# TESTE DE HIPOTESE t (Bilateral) β₁ / AVALIADO PELO p-valor
# Queremos rejeitar H₀ e aceitar que β₁ é diferente de zero.
# H₀: β₁ = 0 Não há relação linear entre X e Y.
# H₁: β₁ ≠ 0 Existe relação linear entre X e Y. #OBJETIVO#

## Pr(>|t|) O p-valor do teste associado a B1 é de 2e-16
## A regressão linear sempre usa a tabela t (que assume que a variância populacional é desconhecida)
## Com n GRANDE usa a t, porque se aproxima da Z
## Se escolhermos a CONFIANÇA de 90%, ALPHA 10% então o p-valor=2e-16 < 0,1 -> rejeita H0
## Mesmo aumentando CONFIANÇA para 99,9%, ainda rejeitaríamos H0 porque p-valor=22e-17 < 0,01 (ALPHA)
## Se o teste de hipótese rejeita H0 -> Então Distancia_metro_Km explica Mil_reais_m2.
# Como o p-valor é extremamente baixo, concluímos que existe relação linear significativa entre Distancia_metro_Km e Mil_reais_m2.
# Teste e avalie a hipotese com p-valor para todas as variáveis.

## TESTE DE HIPOTESE t (Bilateral) β0 | Casos PArticulares ##
## Se o p‑valor do intercepto for BAIXO, você NÃO remove B0
## 03 Motivos para quando devemos retirar o intercepto (B0)
# 01 O p‑valor do intercepto é alto. muito maior que ALPHA, Logo, não há evidência estatística de que B0 ≠ 0.
# 02 Se X = 0 não existe na variável no mundo real, então o intercepto representa uma situação impossível e vira uma extrapolação pura.
# 03 O intercepto não faz sentido no contexto do problema
## só remove B0 quando isso faz sentido estatístico + conceitual
## AVISAR o R que não queremos o coeficiente B0 na equação


############################################################
# 7.1 MODELO SEM INTERCEPTO (B0 = 0)
############################################################

# Fórmula sem intercepto
formula_lm_2 <- as.formula(
  paste(var_y, "~", paste(var_x, collapse = " + "), "- 1")
)

# Ajuste do modelo sem intercepto
modelo_lm_2 <- lm(formula_lm_2, data = base)

# Resumo do modelo sem B0
summary(modelo_lm_2)


############################################################
## 8. PREDIÇÕES E RESÍDUOS
############################################################

# 𝑌=𝛽0+𝛽1𝑋1...+𝛽3𝑋3...
# 𝑌=16.493840 - 5.739872 * 𝑋1 - 0.041448 * 𝑋2 + 0.183508 * 𝑋3


base$Predito <- round(fitted(modelo), 2)
base$Residuo <- round(residuals(modelo), 2)

ic <- predict(modelo, interval="confidence")[, c("lwr", "upr")] # Gera fit, IC_inferior e IC_superior da média do predito
base <- cbind(base, ic)

View(base)

############################################################
## 9. DISTRIBUIÇÃO DOS RESÍDUOS — ANÁLISE DESCRITIVA DO ERRO
############################################################

# Conceito Média Descritiva
# Assimetria com distribuição assimétrica à esquerda → média < mediana
# Assimetria com distribuição assimétrica à direita → média > mediana

# Distribuição Normal: Média = Mediana = Moda = ZERO
# Modelo > Observado → Modelo SuperEstima o Observado
# Modelo < Observado → Modelo SubsEstima o Observado
# media_erro > mediana_erro → Modelo SuperEstima o Observado
# media_erro < mediana_erro → Modelo SubsEstima o Observado

# Média dos resíduos
media_erro <- mean(base$Residuo)
media_erro <- as.data.frame(media_erro)

# Mediana dos resíduos
mediana_erro <- median(base$Residuo)
mediana_erro <- as.data.frame(mediana_erro)

# Juntando colunas lado a lado
ErrosMediaMediana <- cbind(media_erro, mediana_erro)

# Atribuição lógica da distribuição do erro
ErrosMediaMediana$AtribuicaoErro <-
  if (media_erro > mediana_erro) {
    print("Modelo SuperEstima o Observado")
  } else if (media_erro < mediana_erro) {
    print("Modelo SubsEstima o Observado")
  } else {
    "Distribuição Normal dos Erros: Ótimo" # é praticamente uma utopia em dados reais.
  }

print(ErrosMediaMediana)


############################################################
## 10. GRÁFICOS DE RESÍDUOS
############################################################

par(mfrow=c(2,2))

# Histograma dos resíduos
hist(base$Residuo, col="darkturquoise", main="Histograma dos Resíduos")
# Esse gráfico avalia se os resíduos parecem vir de uma distribuição normal, que é uma das suposições da regressão linear.

# QQ-Plot dos resíduos
qqnorm(base$Residuo, col="darkturquoise")
qqline(base$Residuo, col="steelblue")
# Os pontinhos são os residuos, a reta é a distribuição hipotetica dos residuos
# Pequenos desvios nas pontas são normais.
# Outliers: valores > 5 ou < -5: Resp. Não há outliers extremos (>5 ou <–5)
# Normalidade dos resíduos está OK.


## 10.1 RESÍDUOS VS AJUSTADO E ANÁLISE DE LINEARIDADE DA BASE

# Resíduos vs Ajustado (Predito) # O gráfico Resíduos vs Ajustado NÃO deve mostrar qualquer tipo de tendência.
plot(base$Predito, base$Residuo,
     main="Resíduos vs Ajustado (Predito)",
     ylab="Resíduos", col="darkturquoise")


# Comparativo: o gráfico Resíduos x Ajustados NUNCA deve se parecer com o gráfico de Linearidade da base.
# Em caso de multiplo ou comparar  y com cada x individualmente, ou nem precisa comparar (não obirgatório).
# Linearidade entre X e Y
plot(base[[var_x[1]]], base[[var_y]],
     col="darkturquoise",
     main="Linearidade",
     xlab=var_x[1], 
     ylab=var_y)
abline(lm(base[[var_y]] ~ base[[var_x[1]]]), col="blue")


############################################################
## 11. MÉTRICAS DE ERRO: MAPE, SSE e RSE | Diagnostico Rsiduos e VIF
# Quanto menor MAPE, SSE e RSE, melhor o modelo. 
# Cada métrica mede um aspecto diferente do erro: 
# MAPE → erro percentual médio 
# SSE → erro total acumulado 
# RSE → erro típico (desvio-padrão dos resíduos)
############################################################

# MAPE
MAPE <- mape(base[[var_y]], base$Predito)
MAPE
# 0.1746516
# MAPE ≈ 17% → O modelo erra em média 17% no valor previsto por m2. (interpreta qualquer tipo de regressão)

# Como interpretar 17% na prática: 
# Para modelos imobiliários com apenas 1 variável explicativa, 
# um MAPE entre 15% e 25% é comum, razoável, esperado e coerente com o R² ≈ 57%.
# Para modelos imobiliários, sim — 17% é um MAPE excelente, mesmo sendo múltipla.
# preço de imovel tem: alta variabilidade, ruído natural, fatores não observados (andar, vista, reforma, vizinhança, barulho, segurança, insolação, etc.)

# RSE: erro residual padrão (desvio-padrão dos resíduos) 
RSE <- summary(modelo)$sigma 
RSE
# O RSE (Residual Standard Error), e ele não substitui MAPE ou SSE — ele complementa.
# Exemplo: 1.615034
# O RSE indica que o erro típico (desvio‑padrão da variabilidade dos resíduos) é de aproximadamente ±1,61 mil/m².
# RSE ≈ 1.61 → o erro típico (desvio‑padrão dos resíduos) é de aproximadamente ±1.61 mil/m².


# Relação entre MAPE e RSE: 
# MAPE aproximado pode ser estimado por: RSE / média(Y) 
media_y <- mean(base$Reais_m2)
MAPE_calc <- RSE / media_y
MAPE_calc

# Quando: RSE ≈ 1.61 e média(Y) ≈ 7.55753 → MAPE ≈ 21% 
# O valor real (≈17%) é coerente com essa aproximação. 
# Isso bate bem com o desvio padrão dos resíduos (RSE ≈ 1.71), que representa o erro típico absoluto do modelo.
# Isso é coerente: se o imóvel custa ~10 mil/m², 1.61 representa ~19%, # muito próximo do MAPE real (~17%).


# Conclusão: 
# Para um modelo imobiliário com apenas 3 variáveis.
# MAPE ≈ 17%, RSE ≈ 1.61 e R² ≈ 62% — são totalmente coerentes entre si e indicam um modelo estatisticamente sólido.


# SSE: Serve para comparar modelos que fazem a MESMA previsão. 
# Quanto menor o SSE, melhor o ajuste relativo entre modelos concorrentes.
# mede o erro total acumulado (soma dos quadrados dos resíduos).
SSE <- sse(base[[var_y]], base$Predito)
SSE
# 1066.674

# RSE = raiz quad de SSE / (n=413 - K=3 -1)


# 04 gráficos do Diagnóstico dos resíduos (recomendado)
par(mfrow=c(2,2)) 
plot(modelo)

# Residuals vs Fitted
# A dispersão é relativamente constante em relação a linha horizontal quase reta → homocedasticidade aceitável
# 
# Normal Q-Q Plot
# Os pontos devem seguir a linha diagonal.
# Pequenos desvios nas pontas são normais.
# Os resíduos são aproximadamente normais, o que é suficiente para regressão.
# 
# Scale-Location (Spread-Location)
# Testar: Homocedasticidade (variância constante dos resíduos)
# A dispersão é relativamente constante em relação a linha horizontal quase reta → homocedasticidade OK, sem grandes problemas
# 
# Residuals vs Leverage
# Testar Observações que podem distorcer o modelo


# VIF — Variance Inflation Factor (multicolinearidade)
# VIF = o quanto o erro padrão do coeficiente daquela variável aumentou porque ela está correlacionada com outras variáveis.
# VIF mede o quanto a variância do coeficiente (Beta) daquela variável X está inflada por causa da multicolinearidade.
# VIF < 5 → excelente
# VIF < 10 → aceitável
# VIF > 10 → problema sério
VIF <- vif(modelo)
VIF


############################################################
## 12. OUTLIERS DOS RESÍDUOS
# Outlier não é “erro”, Outlier é um sinal.

# Outliers: resíduos > 5 ou < -5 # Arbitrário ±2 resíduos moderados; ±3 resíduos grandes, ±4 ou ±5 resíduos extremos (outliers)
# 5 é um corte empírico.

# Temos: Residual standard error: RSE ≈ x.xx (desvio padrão dos Residuo do modelo); corte de 5 (5 / x.xx = y.yy desvio padrões)
# Seguir a teoria de desvios‑padrão da distribuição teórica dos resíduos studentizados, cortes [+-2] ou [+-3]
# |resíduo studentizado| > 2 → ponto suspeito
# |resíduo studentizado| > 3 → outlier forte

# Um resíduo de ±5 está apenas 1.32 desvios‑padrão acima do erro típico. Só tem 4 resíduos que são maiores que 1.32 RSE#
############################################################

outliers <- subset(base, Residuo > 5 | Residuo < -5)
View(outliers)


# Select com condições em duas colunas / Removendo residuos
outliers_consult <- subset(base, base$Residuo < -6 | base$Residuo > 4)
View(outliers_consult)

# Menor e Maior resíduo
MaiorResiduo <- max(base$Residuo)
MaiorResiduo <- as.data.frame(MaiorResiduo)
print(MaiorResiduo)

MenorResiduo <- min(base$Residuo)
MenorResiduo <- as.data.frame(MenorResiduo)
print(MenorResiduo)

# Juntando colunas lado a lado
MenorMaiorResiduo <- cbind(MenorResiduo, MaiorResiduo)
print(MenorMaiorResiduo)


# Se tivessemos muitos erros acima de +/-5 poderiamos aplicar log ao y
# reg_log <- lm(log(Y2016) ~ X2015, data = captacao)
# a forma de analisar é exatamente a mesma.O que muda são os valores, não o processo.

############################################################
## 13 . Opcionais
############################################################


# Gráfico Residuals vs Leverage com Cook destacado
# É mais prático analisar outliers — e na maioria dos casos, eles coincidem com os pontos influentes Cook's.
par(mfrow = c(1,1))

plot(modelo, which = 5, 
     cex = 1.3,              # aumenta tamanho dos pontos
     pch = 19,               # pontos sólidos
     col = "black")          # cor dos pontos

# Adiciona a linha de Cook mais grossa
cook <- cooks.distance(modelo)
lev  <- hatvalues(modelo)
res  <- rstandard(modelo)

# Desenha a curva de Cook com destaque
cook.level <- 4 / (nrow(base) - length(coef(modelo)) - 1)

# Linha superior
curve(sqrt(cook.level * (1 - x) / x), 
      from = min(lev), to = max(lev),
      add = TRUE, lty = 2, lwd = 3, col = "red")

# Linha inferior
curve(-sqrt(cook.level * (1 - x) / x), 
      from = min(lev), to = max(lev),
      add = TRUE, lty = 2, lwd = 3, col = "red")

# Rótulos maiores
text(lev, res, labels = ifelse(abs(res) > 2, names(res), ""),
     pos = 3, cex = 1.2, col = "blue")



############################################################
## PROMPT ATUAL PARA EVOLUIR O PROJETO
############################################################

# Copilot, estou retomando o projeto de regressão linear.  
# Use este contexto para continuar exatamente de onde paramos:
#   
#   1. O script .R contém explicações teóricas completas, comentários longos e notas pedagógicas.  
# → Não quero perder nada disso.  
# → O script continua sendo meu arquivo mestre (lm simples e composta), totalmente comentado.
# 
# 2. Agora quero criar:
# a) um modelo de portfólio em .Rmd, resumido e executivo, seja customizavel e mais como uma amostra de trabalho  
# quero um .Rmd elegante, enxuto, com personalidade para publicar no meu github, que:
# Mostra maturidade técnica sem despejar código demais
# Demonstra domínio estatístico sem virar aula
# Passa confiança para recrutadores e seja atratativo visualmente e de fácul conexão com o cliente
# Gera respeito entre pares
# Tem aquele ar de profissional experiente que sabe o que mostrar e o que deixar nos bastidores
###########################################################
## FIM DO SCRIPT GENÉRICO
############################################################