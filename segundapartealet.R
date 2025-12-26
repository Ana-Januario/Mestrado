###########################################################################
############Inser��o vari�vel aleat�ria###################################

library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=T, sep=";", dec=",")


##########Modelo dadosd a entrada
dados2<-dados[-c(2,3,264,265,266,267,268,333,337,338,348,360,363,703,710),]

modcustodiaENTB2<-lm(custototal.dia~peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade, data = dados2)
summary(modcustodiaENTB2)
dados[2,]
# esse animal apresentava-se como outlier inferior do custototal.dia = 1,644
summary(dados$custototal.dia) #o menor valor nos dados 1 e 2 s�o diferentes
summary(dados2$custototal.dia) #� a� que est�o os outliers        

dados[3,]   #outlier inferior custototal.dia = 1,57


table(dados2$criador_origem)

library(lme4)
mixed.lmer <- lmer(custototal.dia ~ peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade + VG_Intervalo_Entre_partos+ (1|criador_origem), data = dados2)
summary(mixed.lmer)
#Repara na variancia quase nula dos efeitos aleatorias e na % de explica��o que tem.
#o efeito do criador � nulo

table(dados2$classpeso)

mixed.lmep <- lmer(custototal.dia ~ peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade + (1|classpeso), data = dados2)
summary(mixed.lmep)
