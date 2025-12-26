###################################################################
################CORRELAçãO#########################################
###################################################################
#TESTE DE NORMALIDADE
#Prioridade teste de shapiro-wilk, por se tratar de uma amostra grande
getwd()

library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=T, sep=";", dec=",", fileEncoding = "latin1")
dadosCONV=read.table("bov_mais_CONV.csv", header=T, sep=";", dec=",", fileEncoding = "latin1")
dadosDOP=read.table("bov_mais_DOP.csv", header=T, sep=";", dec=",", fileEncoding = "latin1")

dados$Dest_abate2<-as.numeric(factor(dados$Dest_abate))-1
table(dados$Dest_abate)
table(dados$Dest_abate2)
library(fBasics)
dados$custoaliment = dados$custo_concentrado+dados$custo_palha
dadosDOP$custoaliment = dadosDOP$custo_concentrado+dadosDOP$custo_palha
dadosCONV$custoaliment = dadosCONV$custo_concentrado+dadosCONV$custo_palha


#Teste normalidade geral Shapiro Wilk
shapiro.test(dados$peso_saida) #p-value = 8.322e-06
shapiro.test(dados$Idadesaida_meses) #p-value < 2.2e-16
shapiro.test(dados$GMD) #p-value = 2.28e-12
shapiro.test(dados$peso_ent) #p-value = 0.001504
shapiro.test(dados$Idadeent_meses) #p-value = 1.975e-14
shapiro.test(dados$Diasengorda) #p-value = 0.000262

shapiro.test(dados$P210) #p-value = 0.6191

shapiro.test(dados$pesodacarcaca) #p-value = 1.594e-06
shapiro.test(dados$preco_kg_carcaca) #p-value < 2.2e-16
shapiro.test(dados$Rendimento_carcaca) #p-value = 0.001462
shapiro.test(dados$valor_carcaca) #p-value = 3.876e-09

shapiro.test(dados$liquido_recria) #p-value = 0.01595

shapiro.test(dados$custos_func) #p-value = 0.000262
shapiro.test(dados$custoaliment) #p-value = 1.721e-07
shapiro.test(dados$custo_individual_transporte) #p-value < 2.2e-16

shapiro.test(dados$outros_custos) #p-value = 0.01043

shapiro.test(dados$total_custos) #p-value = 7.707e-07
shapiro.test(dados$VG_capacidade_maternal) #p-value = 3.719e-08
shapiro.test(dados$VG_cap_crescimento) #p-value = 0.002431
shapiro.test(dados$VG_gmd_estacao) #p-value = 6.051e-16
shapiro.test(dados$VG_carcaca_dia_idade) #p-value < 2.2e-16
shapiro.test(dados$VG_Intervalo_Entre_partos) #p-value = 0.0001717
shapiro.test(dados$VG_indice_conversao) #p-value = 9.861e-12
shapiro.test(dados$VG_longevidade_produtiva) #p-value = 3.783e-08
shapiro.test(dados$VG_consumo_alim_residual) #p-value < 2.2e-16

#"NãO FAZ SENTIDO TESTAR A NORMALIDADE DE VARIáVEIS FIXAS COMO profilaxia servico_Promert

#Teste normalidade geral kolmogorov

lillieTest(dados$peso_ent) #0.001798
lillieTest(dados$Idadeent_meses) #1.463e-15
lillieTest(dados$peso_saida) #0.0001271
lillieTest(dados$Idadesaida_meses) #< 2.2e-16
lillieTest(dados$Diasengorda) #0.0004424
lillieTest(dados$GMD) #1.214e-05
lillieTest(dados$pesodacarcaca) #0.008476
lillieTest(dados$preco_kg_carcaca) #< 2.2e-16 

lillieTest(dados$Rendimento_carcaca) #0.124

lillieTest(dados$valor_carcaca) #5.674e-08 

lillieTest(dados$liquido_recria) #0.02608

lillieTest(dados$custos_func) #0.0004424

lillieTest(dados$custoaliment) #0.03687

lillieTest(dados$custo_individual_transporte) # 1.411e-10 
lillieTest(dados$outros_custos) # 0.006248
lillieTest(dados$total_custos)# 0.001406 

lillieTest(dados$P210) # 0.6492

lillieTest(dados$VG_capacidade_maternal) #  0.0007115
lillieTest(dados$VG_cap_crescimento) #0.004446 
lillieTest(dados$VG_gmd_estacao) #3.406e-16 
lillieTest(dados$VG_carcaca_dia_idade) #< 2.2e-16 
lillieTest(dados$VG_Intervalo_Entre_partos) #0.0005361
lillieTest(dados$VG_indice_conversao) #4.202e-14 
lillieTest(dados$VG_longevidade_produtiva) #4.024e-06 
lillieTest(dados$VG_consumo_alim_residual) #2.2e-16 


#Para os que falharam na normalidade realizou-se o teste de simetria
#Teste de simetria
library(moments)
agostino.test(dados$Idadeent_meses)  #p-value = 2.731e-10
agostino.test(dados$peso_ent) #p-value = 0.0006071
agostino.test(dados$Idadesaida_meses) #0.07151
agostino.test(dados$peso_saida) #p-value = 0.001024
agostino.test(dados$Diasengorda)  #p-value = 0.6301
agostino.test(dados$pesodacarcaca) #p-value = 4.805e-05
agostino.test(dados$preco_kg_carcaca) #p-value = 0.4029
agostino.test(dados$valor_carcaca) #p-value = 3.419e-05
agostino.test(dados$profilaxia) #p-value < 2.2e-16
agostino.test(dados$servico_Promert) #p-value = 6.079e-06
agostino.test(dados$custos_func) # p-value = 0.6301
agostino.test(dados$custoaliment)  #p-value = 1.815e-06
agostino.test(dados$custo_individual_transporte) #p-value < 2.2e-16
agostino.test(dados$total_custos) #p-value = 9.136e-05
agostino.test(dados$VG_capacidade_maternal)  #p-value = 5.98e-09
agostino.test(dados$VG_cap_crescimento) #p-value = 0.0134
agostino.test(dados$VG_gmd_estacao) # p-value < 2.2e-16
agostino.test(dados$VG_carcaca_dia_idade) #p-value < 2.2e-16
agostino.test(dados$VG_Intervalo_Entre_partos) #p-value = 0.5823
agostino.test(dados$VG_indice_conversao)  #p-value = 0.4268
agostino.test(dados$VG_longevidade_produtiva) #p-value = 7.893e-06
agostino.test(dados$VG_consumo_alim_residual) #p-value < 2.2e-16

anscombe.test(dados$Idadeent_meses, alternative = "two.sided") #p-value = 0.9342
anscombe.test(dados$peso_ent, alternative = "two.sided") #p-value = 0.5691
anscombe.test(dados$Idadesaida_meses, alternative = "two.sided") #p-value < 2.2e-16
anscombe.test(dados$peso_saida, alternative = "two.sided") #p-value = 0.02602
anscombe.test(dados$Diasengorda, alternative = "two.sided") #p-value = 0.00155
anscombe.test(dados$pesodacarcaca, alternative = "two.sided") #p-value = 0.1228
anscombe.test(dados$preco_kg_carcaca, alternative = "two.sided") #p-value = 0.0006139
anscombe.test(dados$valor_carcaca, alternative = "two.sided") #p-value = 0.0003419
anscombe.test(dados$profilaxia, alternative = "two.sided") #p-value = 0.2681
anscombe.test(dados$servico_Promert, alternative = "two.sided") #0.0003203
anscombe.test(dados$custos_func, alternative = "two.sided") #p-value = 0.00155
anscombe.test(dados$custoaliment, alternative = "two.sided") #p-value = 0.7791
anscombe.test(dados$custo_individual_transporte, alternative = "two.sided") #p-value = 6.139e-08
anscombe.test(dados$total_custos, alternative = "two.sided") #p-value = 0.3557
anscombe.test(dados$VG_capacidade_maternal, alternative = "two.sided") #p-value = 0.003933
anscombe.test(dados$VG_cap_crescimento, alternative = "two.sided") #p-value = 0.8666
anscombe.test(dados$VG_gmd_estacao, alternative = "two.sided") # p-value = 7.032e-09
anscombe.test(dados$VG_carcaca_dia_idade, alternative = "two.sided") #p-value = 1.8e-07
anscombe.test(dados$VG_Intervalo_Entre_partos, alternative = "two.sided") #p-value = 6.153e-07
anscombe.test(dados$VG_indice_conversao, alternative = "two.sided")  #p-value = 3.944e-08
anscombe.test(dados$VG_longevidade_produtiva, alternative = "two.sided") #p-value = 0.0007349
anscombe.test(dados$VG_consumo_alim_residual, alternative = "two.sided") #p-value = 3.89e-15

#Correlação de Spearman
cor.test(dados$Idadeent_meses, dados$total_custos, method = "spearman") #-0.1922325
cor.test(dados$peso_ent, dados$total_custos, method = "spearman") #-0.3612817
cor.test(dados$Idadesaida_meses, dados$total_custos, method = "spearman") #0.727437
cor.test(dados$peso_saida, dados$total_custos, method = "spearman") #0.8145924
cor.test(dados$Diasengorda, dados$total_custos, method = "spearman") #0.9539992
cor.test(dados$GMD, dados$total_custos, method = "spearman") #p-value = 0.9213 e rho=-0.003700789
cor.test(dados$P210, dados$total_custos, method = "spearman") #-0.2148655
cor.test(dados$pesodacarcaca, dados$total_custos, method = "spearman") #0.8395829
cor.test(dados$preco_kg_carcaca, dados$total_custos, method = "spearman")#0.5878373 
cor.test(dados$valor_carcaca, dados$total_custos, method = "spearman") #0.8370752
cor.test(dados$profilaxia, dados$total_custos, method = "spearman") #0.158499 
cor.test(dados$servico_Promert, dados$total_custos, method = "spearman") #0.5700741
cor.test(dados$custos_func, dados$total_custos, method = "spearman") #0.9539992 
cor.test(dados$custoaliment, dados$total_custos, method = "spearman") #0.9965282
cor.test(dados$custo_individual_transporte, dados$total_custos, method = "spearman") #0.6842176
cor.test(dados$outros_custos, dados$total_custos, method = "spearman")#-0.3586762
cor.test(dados$liquido_recria, dados$total_custos, method = "spearman")#-0.2098954
cor.test(dados$Rendimento_carcaca, dados$total_custos, method = "spearman") #0.5011986
cor.test(dados$VG_capacidade_maternal, dados$total_custos, method = "spearman") #0.1029688  
cor.test(dados$VG_cap_crescimento, dados$total_custos, method = "spearman") #-0.1044027
cor.test(dados$VG_gmd_estacao, dados$total_custos, method = "spearman") #p-value = 0.06901 rho = -0.06799558
cor.test(dados$VG_carcaca_dia_idade, dados$total_custos, method = "spearman") #p-value = 0.1068 rho = 0.06032034 
cor.test(dados$VG_Intervalo_Entre_partos, dados$total_custos, method = "spearman") #p-value = 0.01198 rho= 0.09385902 
cor.test(dados$VG_indice_conversao, dados$total_custos, method = "spearman") #p-value = 0.01467  rho = 0.09117089
cor.test(dados$VG_longevidade_produtiva, dados$total_custos, method = "spearman") #p-value = 0.5914 rho = -0.02009484
cor.test(dados$VG_consumo_alim_residual, dados$total_custos, method = "spearman") #0.1056657 

#LUCRO LIQUIDO RECRIA  ###Correlação de pearson
cor.test(dados$Rendimento_carcaca, dados$liquido_recria, method = "pearson") #0.1404289 
cor.test(dados$P210, dados$liquido_recria, method = "pearson") #0.4876612
cor.test(dados$GMD, dados$liquido_recria, method = "pearson") #0.3505898
cor.test(dados$VG_cap_crescimento, dados$liquido_recria, method = "pearson") #0.1661854

#LUCRO LIQUIDO RECRIA  ###Correlação de spearman
cor.test(dados$peso_ent, dados$liquido_recria, method = "spearman") #0.6542276
cor.test(dados$Idadeent_meses, dados$liquido_recria, method = "spearman") #0.3426948 
cor.test(dados$peso_saida, dados$liquido_recria, method = "spearman") #0.2876761 
cor.test(dados$Idadesaida_meses, dados$liquido_recria, method = "spearman") #-0.1139932
cor.test(dados$Diasengorda, dados$liquido_recria, method = "spearman") #-0.3861441 
cor.test(dados$pesodacarcaca, dados$liquido_recria, method = "spearman") #0.2878313
cor.test(dados$preco_kg_carcaca, dados$liquido_recria, method = "spearman") #0.337654
cor.test(dados$valor_carcaca, dados$liquido_recria, method = "spearman") #0.3178401 
cor.test(dados$profilaxia, dados$liquido_recria, method = "spearman") #0.05249391 
cor.test(dados$servico_Promert, dados$liquido_recria, method = "spearman") #-0.07193132
cor.test(dados$custos_func, dados$liquido_recria, method = "spearman") #-0.3861441
cor.test(dados$custoaliment, dados$liquido_recria, method = "spearman") #-0.1889857
cor.test(dados$custo_individual_transporte, dados$liquido_recria, method = "spearman") #0.05602249
cor.test(dados$outros_custos, dados$liquido_recria, method = "spearman") #0.648383
cor.test(dados$total_custos, dados$liquido_recria, method = "spearman") #-0.2098954
cor.test(dados$VG_capacidade_maternal, dados$liquido_recria, method = "spearman") #0.05326315   
cor.test(dados$VG_gmd_estacao, dados$liquido_recria, method = "spearman")  #0.03578925 
cor.test(dados$VG_carcaca_dia_idade, dados$liquido_recria, method = "spearman") # 0.210022
cor.test(dados$VG_Intervalo_Entre_partos, dados$liquido_recria, method = "spearman") #-0.06920802
cor.test(dados$VG_indice_conversao, dados$liquido_recria, method = "spearman") #-0.08931179
cor.test(dados$VG_longevidade_produtiva, dados$liquido_recria, method = "spearman") #0.06103473
cor.test(dados$VG_consumo_alim_residual, dados$liquido_recria, method = "spearman") #0.005070204
