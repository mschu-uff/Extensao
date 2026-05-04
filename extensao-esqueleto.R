# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# A ALTERAÇÃO DO SCRIPT ESQUELETO - ETAPA 1 - DEVERÁ SER FEITA DENTRO DA BRANCH SINASC

# Tarefa 1. Leitura do banco de dados do SINASC 2015  com 3017668 linhas e 61 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sinasc

dados_sinasc <- read.csv("dados/SINASC_2015.csv", header=TRUE, sep=";")
cod_uf <- "41" # Paraná
colunas_desejadas <- c("CONTADOR", "CODMUNNASC", "LOCNASC", "IDADEMAE",
                       "ESTCIVMAE", "CODMUNRES", "GESTACAO", "GRAVIDEZ",
                       "PARTO", "SEXO", "APGAR5", "RACACOR", "PESO", "IDANOMAL",
                       "ESCMAE2010", "RACACORMAE", "SEMAGESTAC", "CONSPRENAT",
                       "TPAPRESENT", "TPROBSON", "PARIDADE", "KOTELCHUCK")

# verificando se os dados foram carregados corretamente
stopifnot(nrow(dados_sinasc) == 3017668)
stopifnot(ncol(dados_sinasc) == 61)
stopifnot(
  names(dados_sinasc)[c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35,
                        38, 44, 46, 48, 59, 60, 61)] == colunas_desejadas
)

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

dados_sinasc_1 <- dados_sinasc[,colunas_desejadas]

# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

dados_sinasc_2 <- dados_sinasc_1[substr(dados_sinasc_1$CODMUNRES, 1, 2)==cod_uf,]

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11409     15: 143657    16: 15750      17: 25110
# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026     
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122 

stopifnot(nrow(dados_sinasc_2)==160947)

# Exportar o arquivo com o nome dados_sinasc_2.csv

write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names=FALSE)
dados_sinasc_2 <- read.csv("dados_sinasc_2.csv")

# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

for (col in c("LOCNASC", "ESTCIVMAE", "GESTACAO", "GRAVIDEZ", "PARTO", "SEXO",
              "APGAR5", "RACACOR", "IDANOMAL", "ESCMAE2010", "RACACORMAE",
              "TPAPRESENT", "TPROBSON", "PARIDADE", "KOTELCHUCK")) {
  freq <- table(dados_sinasc_2[[col]], useNA="always")
  cat(col)
  print(freq)
  cat("\n")
  assign(paste0(col, "_f"), freq)
}

# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável

summary(dados_sinasc_2)
dados_sinasc_2 <- within(dados_sinasc_2, {
  LOCNASC[LOCNASC==9] <- NA
  ESTCIVMAE[ESTCIVMAE==9] <- NA
  GESTACAO[GESTACAO==9] <- NA
  GRAVIDEZ[GRAVIDEZ==9] <- NA
  PARTO[PARTO==9] <- NA
  SEXO[SEXO==0] <- NA
  APGAR5[APGAR5==99] <- NA
  IDANOMAL[IDANOMAL==9] <- NA
  ESCMAE2010[ESCMAE2010==9] <- NA
  TPAPRESENT[TPAPRESENT==9] <- NA
  TPROBSON[TPROBSON==11] <- NA
  KOTELCHUCK[KOTELCHUCK==9] <- NA
  IDADEMAE[IDADEMAE==99] <- NA
  CONSPRENAT[CONSPRENAT==99] <- NA
})
summary(dados_sinasc_2)

# Tarefa 6. Atribuir legendas para as categorias das variáveis investigadas na etapa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

dados_sinasc_2 <- within(dados_sinasc_2, {
  LOCNASC <- factor(LOCNASC, 1:5, c("Hospital",
                                    "Outros estabelecimentos de saúde",
                                    "Domicílio",
                                    "Outros",
                                    "Aldeia Indígena"))
  ESTCIVMAE <- factor(ESTCIVMAE, 1:5, c("Solteira",
                                        "Casada",
                                        "Viúva",
                                        "Separada judicialmente/divorciada",
                                        "União estável"))
  GESTACAO <- factor(GESTACAO, 1:6, c("Menos de 22 semanas",
                                      "22 a 27 semanas",
                                      "28 a 31 semanas",
                                      "32 a 36 semanas",
                                      "37 a 41 semanas",
                                      "42 semanas e mais"))
  GRAVIDEZ <- factor(GRAVIDEZ, 1:3, c("Única",
                                      "Dupla",
                                      "Tripla ou mais"))
  PARTO <- factor(PARTO, 1:2, c("Vaginal", "Cesário"))
  SEXO <- factor(SEXO, 1:2, c("Masculino", "Feminino"))
  RACACOR <- factor(RACACOR, 1:5, c("Branca",
                                    "Preta",
                                    "Amarela",
                                    "Parda",
                                    "Indígena"))
  IDANOMAL <- factor(IDANOMAL, 1:2, c("Sim", "Não"))
  ESCMAE2010 <- factor(ESCMAE2010, 0:5, c("Sem escolaridade",
                                          "Fundamental I",
                                          "Fundamental II",
                                          "Médio",
                                          "Superior incompleto",
                                          "Superior completo"))
  RACACORMAE <- factor(RACACORMAE, 1:5, c("Branca",
                                          "Preta",
                                          "Amarela",
                                          "Parda",
                                          "Indígena"))
  TPAPRESENT <- factor(TPAPRESENT, 1:3, c("Cefálico",
                                          "Pélvica ou podálica",
                                          "Transversa"))
  PARIDADE <- factor(PARIDADE, 0:1, c("Nulípara", "Multípara"))
  KOTELCHUCK <- factor(KOTELCHUCK, 1:5, c("Não realizou pré-natal",
                                          "Inadequado",
                                          "Intermediário",
                                          "Adequado",
                                          "Mais que adequado"))
  TPROBSON <- factor(TPROBSON, 1:10, paste0("Grupo ", 1:10))
})

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados


# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator
# criar nova variável referente ao deslocamento materno para realizar o parto, chamado de peregrinação
# nova variável: dados_sinasc_2$PERIG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES

dados_sinasc_2 <- within(dados_sinasc_2, {
  F_PESO <- cut(PESO,
                c(0, 2500, 4000, max(PESO, na.rm=TRUE)),
                c("Baixo peso", "Peso normal", "Macrossomia"),
                right=FALSE, include.lowest=TRUE)
  F_IDADE <- cut(IDADEMAE,
                 c(0, 5*(3:10), max(IDADEMAE, na.rm=TRUE)),
                 c("<15", paste0(5*(3:9), "-", 5*(3:9)+4), "50+"),
                 right=FALSE, include.lowest=TRUE)
  F_APGAR5 <- cut(APGAR5, c(0, 7, 10), c("Baixo", "Normal"), right=FALSE,
                  include.lowest=TRUE)
  PERIG <- factor(ifelse(CODMUNNASC == CODMUNRES, "Não", "Sim"))
  ESTCIV <- factor(c(1, 2, 1, 1, 2), 1:2,
                   c("Sem companheiro", "Com companheiro"))[ESTCIVMAE]
})

# Script - tarefas 1 a 7 - ETAPA 1

# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.

tabela_pig <- read.csv("Tabela_PIG_Brasil.csv", sep=";", header=TRUE)
tabela_pig$SEXO <- factor(tabela_pig$SEXO)

# Verificando que a variável CONTADOR pode ser usada para ordenar o resultado
stopifnot(dados_sinasc_2$CONTADOR == 1:nrow(dados_sinasc_2))

aux <- merge(dados_sinasc_2[,c("CONTADOR", "SEMAGESTAC", "PESO", "SEXO")],
             tabela_pig, all.x=TRUE)
aux <- aux[order(aux$CONTADOR),]
rownames(aux) <- aux$CONTADOR
dados_sinasc_2$F_PIG <- with(
  aux,
  factor(c("GIG", "AIG", "PIG"))[(PESO<PESO_P10) + (PESO<PESO_P90) + 1]
)
dados_sinasc_2$F_PIG[dados_sinasc_2$GRAVIDEZ!="Única"] <- NA

# Tarefas 9 e 10 (reformulada)
# Crie um banco de dados contendo as 103 variáveis listadas no arquivo
# "Variáveis - Projeto - Tarefas 9 e 10 da Etapa 1.pdf"
# O banco final deverá possuir:
# - 103 colunas, correspondentes às variáveis especificadas;
# - n + 1 linhas, onde:
#   - n corresponde ao número de municípios distintos da UF em análise
#   - a primeira linha corresponde aos valores agregados para a UF como
# um todo;
#   - as demais linhas correspondem aos municípios da UF.
# As variáveis devem ser construídas a partir dos microdados do SINASC,
# respeitando os nomes e a ordem especificados.

obs.completas <- tapply(complete.cases(dados_sinasc), dados_sinasc$CODMUNRES, sum)
sinasc_pr <- do.call(rbind, by(
  dados_sinasc_2,
  dados_sinasc_2$CODMUNRES,
  analise <- function(df) {
    data.frame(
      ANO = 2015,
      NIVEL = "MUNICIPIO",
      TN = nrow(df),
      TNRCR = sum(complete.cases(df)),
      TGI_15 = sum(df$F_IDADE == "<15", na.rm=TRUE),
      TGI_15_19 = sum(df$F_IDADE == "15-19", na.rm=TRUE),
      TGI_20_24 = sum(df$F_IDADE == "20-24", na.rm=TRUE),
      TGI_25_29 = sum(df$F_IDADE == "25-29", na.rm=TRUE),
      TGI_30_34 = sum(df$F_IDADE == "30-34", na.rm=TRUE),
      TGI_35_39 = sum(df$F_IDADE == "35-39", na.rm=TRUE),
      TGI_40_44 = sum(df$F_IDADE == "40-44", na.rm=TRUE),
      TGI_45_49 = sum(df$F_IDADE == "45-49", na.rm=TRUE),
      TGI_50 = sum(df$F_IDADE == "50+", na.rm=TRUE),
      TGIF = sum(df$IDADEMAE >= 15 & df$IDADEMAE <= 49, na.rm=TRUE),
      IM_P25 = quantile(df$IDADEMAE, 0.25, na.rm=TRUE, names=FALSE),
      IM_P50 = quantile(df$IDADEMAE, 0.5, na.rm=TRUE, names=FALSE),
      IM_P75 = quantile(df$IDADEMAE, 0.75, na.rm=TRUE, names=FALSE),
      IM_MD = mean(df$IDADEMAE, na.rm=TRUE),
      IM_DP = sd(df$IDADEMAE, na.rm=TRUE),
      EM_S = sum(df$ESCMAE2010 == "Sem escolaridade", na.rm=TRUE),
      EM_FI = sum(df$ESCMAE2010 == "Fundamental I", na.rm=TRUE),
      EM_FII = sum(df$ESCMAE2010 == "Fundamental II", na.rm=TRUE),
      EM_M = sum(df$ESCMAE2010 == "Médio", na.rm=TRUE),
      EM_SI = sum(df$ESCMAE2010 == "Superior incompleto", na.rm=TRUE),
      EM_SC = sum(df$ESCMAE2010 == "Superior completo", na.rm=TRUE),
      TGRC_B = sum(df$RACACORMAE == "Branca", na.rm=TRUE),
      TGRC_PT = sum(df$RACACORMAE == "Preta", na.rm=TRUE),
      TGRC_A = sum(df$RACACORMAE == "Amarela", na.rm=TRUE),
      TGRC_PD = sum(df$RACACORMAE == "Parda", na.rm=TRUE),
      TGRC_I = sum(df$RACACORMAE == "Indígena", na.rm=TRUE),
      TGSC = sum(df$ESTCIV == "Sem companheiro", na.rm=TRUE),
      TGCC = sum(df$ESTCIV == "Com companheiro", na.rm=TRUE),
      TGPRI = sum(df$PARIDADE == "Nulípara", na.rm=TRUE),
      TGNPRI = sum(df$PARIDADE == "Multípara", na.rm=TRUE),
      TGU = sum(df$GRAVIDEZ == "Única", na.rm=TRUE),
      TGG = sum(df$GRAVIDEZ %in% c("Dupla", "Tripla ou mais"), na.rm=TRUE),
      TGD_22 = sum(df$SEMAGESTAC < 22, na.rm=TRUE),
      TGD_22_27 = sum(df$SEMAGESTAC >= 22 & df$SEMAGESTAC <= 27, na.rm=TRUE),
      TGD_28_31 = sum(df$SEMAGESTAC >= 28 & df$SEMAGESTAC <= 31, na.rm=TRUE),
      TGD_32_36 = sum(df$SEMAGESTAC >= 32 & df$SEMAGESTAC <= 36, na.rm=TRUE),
      TGD_37_41 = sum(df$SEMAGESTAC >= 37 & df$SEMAGESTAC <= 41, na.rm=TRUE),
      TGD_42 = sum(df$SEMAGESTAC >= 42, na.rm=TRUE),
      TGD_PRT = sum(df$SEMAGESTAC < 37, na.rm=TRUE),
      TGD_AT = sum(df$SEMAGESTAC >= 37 & df$SEMAGESTAC <= 41, na.rm=TRUE),
      TGD_PST = sum(df$SEMAGESTAC >= 42, na.rm=TRUE),
      DG_P25 = quantile(df$SEMAGESTAC, 0.25, na.rm=TRUE, names=FALSE),
      DG_P50 = quantile(df$SEMAGESTAC, 0.5, na.rm=TRUE, names=FALSE),
      DG_P75 = quantile(df$SEMAGESTAC, 0.75, na.rm=TRUE, names=FALSE),
      DG_MD = mean(df$SEMAGESTAC, na.rm=TRUE),
      DG_DP = sd(df$SEMAGESTAC, na.rm=TRUE),
      TKC_NR = sum(df$KOTELCHUCK == "Não realizou pré-natal", na.rm=TRUE),
      TKC_ID = sum(df$KOTELCHUCK == "Inadequado", na.rm=TRUE),
      TKC_IT = sum(df$KOTELCHUCK == "Intermediário", na.rm=TRUE),
      TKC_AD = sum(df$KOTELCHUCK == "Adequado", na.rm=TRUE),
      TKC_MAD = sum(df$KOTELCHUCK == "Mais que adequado", na.rm=TRUE),
      TGPRG_S = sum(df$PERIG == "Sim", na.rm=TRUE),
      TGPRG_N = sum(df$PERIG == "Não", na.rm=TRUE),
      TPV = sum(df$PARTO == "Vaginal", na.rm=TRUE),
      TPC = sum(df$PARTO == "Cesário", na.rm=TRUE),
      TRAP_C = sum(df$TPAPRESENT == "Cefálico", na.rm=TRUE),
      TRAP_P = sum(df$TPAPRESENT == "Pélvica ou podálica", na.rm=TRUE),
      TRAP_T = sum(df$TPAPRESENT == "Transversa", na.rm=TRUE),
      TGROB_1 = sum(df$TPROBSON == "Grupo 1", na.rm=TRUE),
      TGROB_2 = sum(df$TPROBSON == "Grupo 2", na.rm=TRUE),
      TGROB_3 = sum(df$TPROBSON == "Grupo 3", na.rm=TRUE),
      TGROB_4 = sum(df$TPROBSON == "Grupo 4", na.rm=TRUE),
      TGROB_5 = sum(df$TPROBSON == "Grupo 5", na.rm=TRUE),
      TGROB_6 = sum(df$TPROBSON == "Grupo 6", na.rm=TRUE),
      TGROB_7 = sum(df$TPROBSON == "Grupo 7", na.rm=TRUE),
      TGROB_8 = sum(df$TPROBSON == "Grupo 8", na.rm=TRUE),
      TGROB_9 = sum(df$TPROBSON == "Grupo 9", na.rm=TRUE),
      TGROB_10 = sum(df$TPROBSON == "Grupo 10", na.rm=TRUE),
      TNLOC_H = sum(df$LOCNASC == "Hospital", na.rm=TRUE),
      TNLOC_ES = sum(df$LOCNASC == "Outros estabelecimentos de saúde", na.rm=TRUE),
      TNLOC_D = sum(df$LOCNASC == "Domicílio", na.rm=TRUE),
      TNLOC_O = sum(df$LOCNASC == "Outros", na.rm=TRUE),
      TNLOC_AI = sum(df$LOCNASC == "Aldeia Indígena", na.rm=TRUE),
      TRS_M = sum(df$SEXO == "Masculino", na.rm=TRUE),
      TRS_F = sum(df$SEXO == "Feminino", na.rm=TRUE),
      TRRC_B = sum(df$RACACOR == "Branca", na.rm=TRUE),
      TRRC_PT = sum(df$RACACOR == "Preta", na.rm=TRUE),
      TRRC_A = sum(df$RACACOR == "Amarela", na.rm=TRUE),
      TRRC_PD = sum(df$RACACOR == "Parda", na.rm=TRUE),
      TRRC_I = sum(df$RACACOR == "Indígena", na.rm=TRUE),
      TRP_BP = sum(df$PESO < 2500, na.rm=TRUE),
      TRP_N = sum(df$PESO >= 2500 & df$PESO < 4000, na.rm=TRUE),
      TRP_M = sum(df$PESO >= 4000, na.rm=TRUE),
      PESO_P25 = quantile(df$PESO, 0.25, na.rm=TRUE, names=FALSE),
      PESO_P50 = quantile(df$PESO, 0.5, na.rm=TRUE, names=FALSE),
      PESO_P75 = quantile(df$PESO, 0.75, na.rm=TRUE, names=FALSE),
      PESO_MD = mean(df$PESO, na.rm=TRUE),
      PESO_DP = sd(df$PESO, na.rm=TRUE),
      TRPIG_P = sum(df$F_PIG == "PIG", na.rm=TRUE),
      TRPIG_A = sum(df$F_PIG == "AIG", na.rm=TRUE),
      TRPIG_G = sum(df$F_PIG == "GIG", na.rm=TRUE),
      TRAPG5_B = sum(df$APGAR5 < 7, na.rm=TRUE),
      TRAPG5_N = sum(df$APGAR5 >= 7, na.rm=TRUE),
      APG5_MD = mean(df$APGAR5, na.rm=TRUE),
      APG5_DP = sd(df$APGAR5, na.rm=TRUE),
      TRAC = sum(df$IDANOMAL == "Sim", na.rm=TRUE),
      TRSAC = sum(df$IDANOMAL == "Não", na.rm=TRUE)
    )
  }))
sinasc_pr$CODMUNRES <- rownames(sinasc_pr)
rownames(sinasc_pr) <- NULL
sinasc_pr$TNRC <- obs.completas[sinasc_pr$CODMUNRES]

linha_estado <- analise(dados_sinasc_2)
linha_estado$NIVEL <- "UF"
linha_estado$CODMUNRES <- 41
linha_estado$TNRC <- sum(sinasc_pr$TNRC)

sinasc_pr <- rbind(linha_estado, sinasc_pr)

variaveis <- read.csv("Variaveis.csv", header=TRUE, sep=",")
sinasc_pr <- sinasc_pr[,variaveis$Variável]

View(sinasc_pr)

# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv

write.csv(sinasc_pr, "SINASC_PR.csv", row.names=FALSE)

# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"
# Faça um merge de script de SINASC para main



##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# Altere o script esqueleto nas partes que se refere a ETAPA 2 e envie para o repositório Extensao tendo feito o commite "Esqueleto atualizado na Etapa 2"
# A partir de main crie a branch SIM
# ESTANDO NA BRANCH SIM, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 1 e só insira comandos na ETAPA 2
# Para realizar as tarefas da ETAPA 2, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1216475 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, 
# OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 7948      12: 3517      13: 16675     14: 2091      15: 37365     16: 2946       17: 7402
# 21: 33666     22: 19366     23: 55258     24: 20153     25: 26422     26: 62556      27: 19756     28: 13453     29: 87083
# 31: 131274    32: 22332     33: 127714    35: 287645     
# 41: 70839     42: 37984     43: 82349
# 50: 15457     51: 17095     52: 38854     53: 11975

# Exportar o arquivo com o nome dados_sim_2.csv


# Ao concluir a Tarefa 3 da Etapa 2 commite e envie para o repositório REMOTO o script e dados_sim_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis: TIPOBITO, SEXO, RACACOR, 
# TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO


# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 99 para NA


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), 
# labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados


# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da Etapa 2.pdf”
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE


# Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv

# Ao terminar a ETAPA 2 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 2"
# Faça um merge de script de SIM para main


#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 3

# Tarefa 1. Acesso aos bancos de dados e obtenção da informação



#####################################################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO, BASEADO NAS ANÁLISES DE SINASC, SIM, IBGE, SNIS,...
######################################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 4

# Cada aluno gerar um dataframe de uma única linha (referente ao seu estado) com as variáveis na ordem indicada pela professora



############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes
