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

dados_sim <- read.csv("dados/Mortalidade_Geral_2015.csv",
                      header=TRUE, sep=";")
stopifnot(dim(dados_sim) == c(1264175, 87))

# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, 
# OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

cols_SIM <- c("CONTADOR", "TIPOBITO", "DTOBITO", "DTNASC",
              "IDADE", "SEXO", "RACACOR", "ESC2010",
              "CODMUNRES", "TPMORTEOCO", "OBITOGRAV",
              "OBITOPUERP", "CAUSABAS", "TPOBITOCOR",
              "MORTEPARTO")
stopifnot(
  names(dados_sim[, c(1, 3, 4, 8, 9, 10, 11, 14, 17,
                      35, 36, 37, 47, 77, 84)]) == cols_SIM
)
dados_sim_1 <- dados_sim[, cols_SIM]

# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

cod_uf <- "41" # Paraná
dados_sim_2 <- dados_sim_1[
  substr(dados_sim_1$CODMUNRES, 1, 2) == cod_uf,
]

# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 7948      12: 3517      13: 16675     14: 2091      15: 37365     16: 2946       17: 7402
# 21: 33666     22: 19366     23: 55258     24: 20153     25: 26422     26: 62556      27: 19756     28: 13453     29: 87083
# 31: 131274    32: 22332     33: 127714    35: 287645     
# 41: 70839     42: 37984     43: 82349
# 50: 15457     51: 17095     52: 38854     53: 11975

stopifnot(dim(dados_sim_2) == c(70839, 15))

# Exportar o arquivo com o nome dados_sim_2.csv

write.csv(dados_sim_2, "dados_sim_2.csv", row.names=FALSE)

# Ao concluir a Tarefa 3 da Etapa 2 commite e envie para o repositório REMOTO o script e dados_sim_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis: TIPOBITO, SEXO, RACACOR, 
# TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

for (col in c("TIPOBITO", "SEXO", "RACACOR", "TPMORTEOCO",
              "OBITOGRAV", "OBITOPUERP", "CAUSABAS",
              "TPOBITOCOR", "MORTEPARTO", "ESC2010")) {
  freq <- table(dados_sim_2[[col]], useNA="always")
  cat(col)
  print(freq)
  cat("\n")
}

# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 99 para NA

summary(dados_sim_2)
dados_sim_2 <- within(dados_sim_2, {
  IDADE[IDADE==999] <- NA
  SEXO[SEXO==0] <- NA
  ESC2010[ESC2010==9] <- NA
  TPMORTEOCO[TPMORTEOCO==9] <- NA
  OBITOGRAV[OBITOGRAV==9] <- NA
  OBITOPUERP[OBITOPUERP==9] <- NA
  MORTEPARTO[MORTEPARTO==9] <- NA
})

# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), 
# labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

dados_sim_2 <- within(dados_sim_2, {
  TIPOBITO <- factor(TIPOBITO, 1:2, c("Fetal", "Não fetal"))

  SEXO <- factor(SEXO, 1:2, c("Masculino", "Feminino"))
  
  RACACOR <-
    factor(RACACOR, 1:5,
           c("Branca",
             "Preta",
             "Amarela",
             "Parda",
             "Indígena"))
  
  TPMORTEOCO <-
    factor(TPMORTEOCO, c(1:5, 8),
           c("Na gravidez",
             "No parto",
             "No abortamento",
             "Até 42 dias após o término do parto",
             "De 43 dias a 1 ano após o término da gestação",
             "Não ocorreu nestes períodos"))
  
  OBITOGRAV <- factor(OBITOGRAV, 1:2, c("Sim", "Não"))
  
  OBITOPUERP <-
    factor(OBITOPUERP, 1:3,
           c("Sim, até 42 dias após o parto",
             "Sim, de 43 dias a 1 ano",
             "Não"))
  
  CAUSABAS <- factor(CAUSABAS)
  
  TPOBITOCOR <-
    factor(TPOBITOCOR, 1:9,
           c("Durante a gestação",
             "Durante o abortamento",
             "Após o abortamento",
             "No parto ou até 1 hora após o parto",
             "No puerpério",
             "Entre 43 dias e até 1 ano após o parto",
             "A investigação não identificou o momento do óbito",
             "Mais de um ano após o parto",
             "O óbito não ocorreu nas circunstancias anteriores"))
  
  MORTEPARTO <-
    factor(MORTEPARTO, 1:3, c("Antes", "Durante", "Após"))
  
  ESC2010 <-
    factor(ESC2010, 0:5,
           c("Sem escolaridade",
             "Fundamental I",
             "Fundamental II",
             "Médio",
             "Superior incompleto",
             "Superior completo"))
})

# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da Etapa 2.pdf”
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE

obs.completas <- tapply(complete.cases(dados_sim), dados_sim$CODMUNRES, sum)
obs.completas["41"] <- sum(obs.completas)
sim_pr <- do.call(rbind, by(
  dados_sim_2,
  dados_sim_2$CODMUNRES,
  analise <- function(df) {
    INICIALCAUSABAS <- substr(df$CAUSABAS, 1, 1)
    F_CAUSABAS <- table(INICIALCAUSABAS)
    PRECOCE <- df$TPMORTEOCO %in% c("Na gravidez",
                                    "No parto",
                                    "No abortamento",
                                    "Até 42 dias após o término do parto")
    NEONATAL <- df$TIPOBITO!="Fetal" & df$IDADE<=227
    data.frame(
      ANO = 2015,
      NIVEL = "MUNICIPIO",
      CODMUNRES = df$CODMUNRES[1],
      TO = nrow(df),
      TORC = obs.completas[as.character(df$CODMUNRES[1])],
      TORCR = sum(complete.cases(df)),
      TO_NN = sum(F_CAUSABAS[c("V", "W", "X", "Y")], na.rm=TRUE),
      TO_N = sum(F_CAUSABAS[!names(F_CAUSABAS) %in% c("V", "W", "X", "Y")], na.rm=TRUE),
      TO_CB_I = sum(F_CAUSABAS[c("A", "B")], na.rm=TRUE),
      TO_CB_N = sum(F_CAUSABAS[c("C", "D")], na.rm=TRUE),
      TO_CB_C = sum(F_CAUSABAS["I"], na.rm=TRUE),
      TO_CB_R = sum(F_CAUSABAS["J"], na.rm=TRUE),
      TO_CB_O = sum(F_CAUSABAS[!names(F_CAUSABAS) %in% c("A", "B", "C", "D", "I", "J", "V", "W", "X", "Y")], na.rm=TRUE),
      TO_M = sum(df$SEXO=="Masculino", na.rm=TRUE),
      TO_F = sum(df$SEXO=="Feminino", na.rm=TRUE),
      TO_F_IF = sum(df$SEXO=="Feminino" & df$IDADE>=415 & df$IDADE<=449, na.rm=TRUE),
      TO_FT = sum(df$TIPOBITO=="Fetal", na.rm=TRUE),
      TO_NT = sum(NEONATAL, na.rm=TRUE),
      TO_NT_P = sum(NEONATAL & df$IDADE<=206, na.rm=TRUE),
      TO_NT_T = sum(df$IDADE>=207 & df$IDADE<=227, na.rm=TRUE),
      TO_PNT = sum(df$IDADE>=228 & df$IDADE<=311, na.rm=TRUE),
      TO_MT_G = sum(df$MORTEPARTO=="Antes", na.rm=TRUE),
      TONT_B = sum(NEONATAL & df$RACACOR=="Branca", na.rm=TRUE),
      TONT_PT = sum(NEONATAL & df$RACACOR=="Preta", na.rm=TRUE),
      TONT_A = sum(NEONATAL & df$RACACOR=="Amarela", na.rm=TRUE),
      TONT_PD = sum(NEONATAL & df$RACACOR=="Parda", na.rm=TRUE),
      TONT_I = sum(NEONATAL & df$RACACOR=="Indígena", na.rm=TRUE),
      TO_MT = sum(!is.na(df$MORTEPARTO)),
      TO_MT_DG = sum(df$TPMORTEOCO=="Na gravidez", na.rm=TRUE),
      TO_MT_PT = sum(df$TPMORTEOCO=="No parto", na.rm=TRUE),
      TO_MT_AB = sum(df$TPMORTEOCO=="No abortamento", na.rm=TRUE),
      TO_MT_42 = sum(df$TPMORTEOCO=="Até 42 dias após o término do parto", na.rm=TRUE),
      TO_MT_43 = sum(df$TPMORTEOCO=="De 43 dias a 1 ano após o término da gestação", na.rm=TRUE),
      TO_MT_P = sum(PRECOCE, na.rm=TRUE),
      TO_MT_P_I = sum(PRECOCE & df$IDADE>=415 & df$IDADE<=449, na.rm=TRUE),
      TO_MT_P_ES = sum(PRECOCE & df$ESC2010=="Sem escolaridade", na.rm=TRUE),
      TO_MT_P_EFI = sum(PRECOCE & df$ESC2010=="Fundamental I", na.rm=TRUE),
      TO_MT_P_EFII = sum(PRECOCE & df$ESC2010=="Fundamental II", na.rm=TRUE),
      TO_MT_P_EM = sum(PRECOCE & df$ESC2010=="Médio", na.rm=TRUE),
      TO_MT_P_ESI = sum(PRECOCE & df$ESC2010=="Superior incompleto", na.rm=TRUE),
      TO_MT_P_ESC = sum(PRECOCE & df$ESC2010=="Superior completo", na.rm=TRUE)
    )
  }))
rownames(sim_pr) <- NULL

linha_estado <- analise(dados_sim_2)
linha_estado$NIVEL <- "UF"
linha_estado$CODMUNRES <- 41

sim_pr <- rbind(linha_estado, sim_pr)

# Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv

write.csv(sim_pr, "SIM_PR.csv", row.names=FALSE)

# Ao terminar a ETAPA 2 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 2"
# Faça um merge de script de SIM para main


#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# Ao terminar a ETAPA 2 faça um merge de SIM para main
# Altere as orientações do script e commit (em main) "Script com orientações ETAPA 3 - SIDRA"
# Abra um branch OUTROS
# Na branch OUTROS escreva os comandos da Tarefa 1 abaixo

# Tarefa 1. Acesso aos bancos de dados do SIDRA e obtenção da informação
# Leia os arquivos:
# 1. população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv  
# 2. população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv  
# 3. população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv
# 4. população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv

# A partir dos arquivos acima gere o banco de dados de nome SIDRA_UF com as seguintes variáveis:
# 1    ANO:    
# 2    NIVEL
# 3    CODMUNRES
# 4 POPRE_T
# 5 POPRC_T
# 6 POPRC_M
# 7 POPRC_F
# 8 POPRC_15
# 9 POPRC_15_49
# 10 POPRC_50
# 11 POPRC_F_15
# 12 POPRC_F_15_49
# 13 POPRC_F_50

pop_idade_uf <- read.csv("população residente censo 2010 - por faixa etária - UF - SIDRA - tabela_1552.csv", header=TRUE, sep=";")
pop_idade_sexo <- read.csv("população residente censo 2010 - por faixa etária e sexo - municípios - SIDRA - tabela_1552.csv", header=TRUE, sep=";")
pop_sexo <- read.csv("população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv", header=TRUE, sep=";")
pop_estimada_2015 <- read.csv("população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv", header=TRUE, sep=";")

IDADE_15 <- c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")
IDADE_15_49 <- c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos",
                 "30 a 34 anos", "35 a 39 anos", "40 a 44 anos",
                 "45 a 49 anos")
IDADE_50 <- c("50 a 54 anos", "55 a 59 anos", "60 a 64 anos",
              "65 a 69 anos", "70 a 74 anos", "75 a 79 anos",
              "80 a 89 anos", "90 a 99 anos", "100 anos ou mais")

pop_idade_sexo <- within(pop_idade_sexo, {
  F_IDADE[F_IDADE %in% IDADE_15] <- "0 a 14 anos"
  F_IDADE[F_IDADE %in% IDADE_15_49] <- "15 a 49 anos"
  F_IDADE[F_IDADE %in% IDADE_50] <- "50 anos ou mais"
})

pop_idade_uf <- pop_idade_uf[-514,] # linha vazia
pop_idade_uf <- within(pop_idade_uf, {
  F_IDADE[F_IDADE %in% IDADE_15] <- "0 a 14 anos"
  F_IDADE[F_IDADE %in% IDADE_15_49] <- "15 a 49 anos"
  F_IDADE[F_IDADE %in% IDADE_50] <- "50 anos ou mais"
  ESTADO <- NULL
})

pop_idade_sexo <- rbind(pop_idade_sexo, pop_idade_uf)

pop_tot <- as.data.frame.matrix(xtabs(POP ~ (CODMUNRES + F_IDADE), pop_idade_sexo))
names(pop_tot) <- c("POPRC_15", "POPRC_15_49", "POPRC_50")
pop_tot$CODMUNRES <- rownames(pop_tot)
rownames(pop_tot) <- NULL

pop_fem <- as.data.frame.matrix(xtabs(POPF ~ (CODMUNRES + F_IDADE), pop_idade_sexo))
names(pop_fem) <- c("POPRC_F_15", "POPRC_F_15_49", "POPRC_F_50")
pop_fem$CODMUNRES <- rownames(pop_fem)
rownames(pop_fem) <- NULL

sidra_pr <- pop_estimada_2015
sidra_pr <- merge(sidra_pr, pop_sexo, all.x=TRUE)
sidra_pr <- merge(sidra_pr, pop_tot, all.x=TRUE)
sidra_pr <- merge(sidra_pr, pop_fem, all.x=TRUE)

sidra_pr$ANO <- 2015
sidra_pr$NIVEL <- ifelse(nchar(sidra_pr$CODMUNRES)==2, "UF", "MUNICIPIO")

sidra_pr <- sidra_pr[,c("ANO",    
                        "NIVEL",
                        "CODMUNRES",
                        "POPRE_T",
                        "POPRC_T",
                        "POPRC_M",
                        "POPRC_F",
                        "POPRC_15",
                        "POPRC_15_49",
                        "POPRC_50",
                        "POPRC_F_15",
                        "POPRC_F_15_49",
                        "POPRC_F_50")]

sidra_pr <- sidra_pr[substr(sidra_pr$CODMUNRES, 1, 2)=="41",]

# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados TAREFA 3 - SIDRA"

write.csv(sidra_pr, "SIDRA_PR.csv", row.names=FALSE)

# Tarefa 2: Acesso aos bancos de dados do SINISA e obtenção da informação
# Escreva os comandos da Tarefa 2 estando na branch OUTROS# Leia o arquivo agua e esgoto - município - 2015.csv 
# A partir do arquivo acima gere o banco de dados de nome SINISA_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 POPR_RA
# 5 POPR_RE

sinisa_pr <- read.csv("agua e esgoto - município - 2015.csv", header=TRUE, sep=";")
sinisa_pr <- sinisa_pr[sinisa_pr$Estado=="PR",]

sinisa_pr$POPR_RA <- as.integer(gsub("\\.", "", sinisa_pr$POPR_RA))
sinisa_pr$POPR_RE <- as.integer(gsub("\\.", "", sinisa_pr$POPR_RE))
sinisa_pr$ANO <- 2015
sinisa_pr$NIVEL <- "MUNICIPIO"

sinisa_pr <- sinisa_pr[,c("ANO", "NIVEL", "CODMUNRES", "POPR_RA", "POPR_RE")]

sinisa_pr <- rbind(data.frame(
  ANO = 2015,
  NIVEL = "UF",
  CODMUNRES = 41,
  POPR_RA = sum(sinisa_pr$POPR_RA, na.rm=TRUE),
  POPR_RE = sum(sinisa_pr$POPR_RE, na.rm=TRUE)
), sinisa_pr)

# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados TAREFA 3 - SINISA"

write.csv(sinisa_pr, "SINISA_PR.csv", row.names=FALSE)

# Tarefa 3: Acesso aos bancos de dados do ATLAS  e obtenção da informação
# Escreva os comandos da Tarefa 3 estando na branch OUTROS
# Leia os arquivos:
# 1. códigos dos municípios - 2010.csv      
# 2. IDHM - 2010 (CENSO) e 2015 (PNAD) - total e por sexo - UF - Atlas Brasil.csv
# 3. IDHM - 2010 - municípios - Atlas Brasil.csv
# A partir do arquivo acima gere o banco de dados de nome ATLAS_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 IDHM_A
# 5 IDHM_CA
# 6 IDHM_CA_M
# 7 IDHM_CA_F

cod_mun <- read.csv("códigos dos municípios - 2010.csv", header=TRUE, sep=";")[,1:2]
idhm_mun <- read.csv("IDHM - 2010 - municípios - Atlas Brasil.csv", header=TRUE, sep=";")[,1:2]
idhm_uf <- read.csv("IDHM - 2010 (CENSO) e 2015 (PNAD) - total e por sexo - UF - Atlas Brasil.csv", header=TRUE, sep=";")[,1:7]

cod_mun <- cod_mun[substr(cod_mun$CODMUNRES, 1, 2)=="41",]

idhm_mun <- idhm_mun[endsWith(idhm_mun$município, "PR)"),]
idhm_mun$município <- substr(idhm_mun$município, 1,
                             nchar(idhm_mun$município)-5)
idhm_mun$NIVEL <- "MUNICIPIO"

idhm_uf <- idhm_uf[idhm_uf$UF=="Paraná",
                   c("IDHM_2010", "IDHM_2015",
                     "IDHM_2010_M", "IDHM_2010_F")]
idhm_uf$CODMUNRES <- "41"
idhm_uf$NIVEL <- "UF"

idhm_mun <- merge(idhm_mun, cod_mun, all.x=TRUE)
idhm_mun <- idhm_mun[,c("CODMUNRES", "NIVEL", "IDHM_2010")]
idhm_mun$IDHM_2015 <- NA
idhm_mun$IDHM_2010_M <- NA
idhm_mun$IDHM_2010_F <- NA

atlas_pr <- rbind(idhm_uf, idhm_mun)
atlas_pr$ANO <- 2015
atlas_pr <- atlas_pr[,c("ANO", "NIVEL", "CODMUNRES",
                        "IDHM_2015", "IDHM_2010",
                        "IDHM_2010_M", "IDHM_2010_F")]
names(atlas_pr) <- c("ANO", "NIVEL", "CODMUNRES",
                     "IDHM_A", "IDHM_CA",
                     "IDHM_CA_M", "IDHM_CA_F")
atlas_pr$IDHM_A <- as.numeric(sub(",", ".", atlas_pr$IDHM_A))
atlas_pr$IDHM_CA <- as.numeric(sub(",", ".", atlas_pr$IDHM_CA))
atlas_pr$IDHM_CA_M <- as.numeric(sub(",", ".", atlas_pr$IDHM_CA_M))
atlas_pr$IDHM_CA_F <- as.numeric(sub(",", ".", atlas_pr$IDHM_CA_F))

# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados TAREFA 3 - ATLAS"

write.csv(atlas_pr, "ATLAS_PR.csv", row.names=FALSE)

################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO COM DADOS DO SIDRA, ATLAS, SINASC, SIM, SINISA E INDICADORES
################################################################


# Tarefa 1: Fazer o merge dos bancos de dados criados nas etapas anteriores (SIDRA_UF, ATLAS_ UF,  SINASC_UF, SIM_UF e SINISA_UF), 
# sendo que as variáveis deverão seguir a ordem

SIDRA_PR <- read.csv("SIDRA_PR.csv", header=TRUE, sep=",")
ATLAS_PR <- read.csv("ATLAS_PR.csv", header=TRUE, sep=",")
SINASC_PR <- read.csv("SINASC_PR.csv", header=TRUE, sep=",")
SIM_PR <- read.csv("SIM_PR.csv", header=TRUE, sep=",")
SINISA_PR <- read.csv("SINISA_PR.csv", header=TRUE, sep=",")

codmun_6_7 <- SIDRA_PR$CODMUNRES
names(codmun_6_7) <- substr(SIDRA_PR$CODMUNRES, 1, 6)
codmun_6_7["410000"] <- 410000

SINASC_PR$CODMUNRES <- codmun_6_7[as.character(SINASC_PR$CODMUNRES)]
SIM_PR$CODMUNRES <- codmun_6_7[as.character(SIM_PR$CODMUNRES)]
SINISA_PR$CODMUNRES <- codmun_6_7[as.character(SINISA_PR$CODMUNRES)]

DA_PR <- merge(SIDRA_PR, ATLAS_PR, all=TRUE)
DA_PR <- merge(DA_PR, SINASC_PR, all=TRUE)
DA_PR <- merge(DA_PR, SIM_PR, all=TRUE)
DA_PR <- merge(DA_PR, SINISA_PR, all=TRUE)

# ANO, NIVEL, CODMUNRES (uma única vez), variáveis do SIDRA, do ATLAS, do SINASC, do SIM e da SINISA. No merge deve constar qualquer município que esteja em pelo menos um dos bancos
# Chamar o banco de dados de DA_UF

# Após o merge dos bancos, fazer commit “Script e dados agregados da UF”


# Tarefa 2: Acrescentar no banco DA_UF os indicadores TFG, TMG, RMM, TMM, TMM_P, TMN, TMN_P, TMN_T e TMI e chamar o banco de BDEM_UF_2015

# Após a criação do banco, fazer commit “Script e dados BDEM_UF_2015”

# Exporte o arquivo em formato CSV# Faça o commit com a mensagem "Script e dados TAREFA 3 - ATLAS"


############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes
