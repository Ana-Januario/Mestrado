getwd()

library(readxl)
dadosCONV=read.table("bov_mais_CONV.csv", header=T, sep=";", dec=",")
library(fBasics)
hist(dadosCONV$custototal.dia, main = "Histograma custo total convencional/dia na engorda", xlab = "Custo total/dia", ylab = "Frequ�ncia")
boxplot(dadosCONV$custototal.dia)

#NORMALIDADE
shapiro.test(dadosCONV$custototal.dia) #A variavel passa na normalidade
lillieTest(dadosCONV$custototal.dia)

##MODELO SIMPLES####
modcusto1<-lm(dadosCONV$custototal.dia~dadosCONV$peso_ent)
summary(modcusto1) 
#Mantendo tudo constante, quando aumento em 1 Kg o peso do animal na entrada, o custo por dia aumenta em 0,004 euros

modcusto2<-lm(dadosCONV$custototal.dia~dadosCONV$Idadeent_meses)
summary(modcusto2) #significativo

modcusto3<-lm(dadosCONV$custototal.dia~dadosCONV$P210)
summary(modcusto3) 
#Mantendo tudo constante, quando aumento em 1 unidade o P210, o custo aumenta em 0,005 euros

#n�o entrou por apresentar muito NA
#modcusto<-lm(dadosCONV$custototal.dia~dadosCONV$Pai_p210)
#summary(modcusto) 
#a vari�vel n�o apresentou significancia

#n�o entrou por apresentar muito NA
#modcusto<-lm(dadosCONV$custototal.dia~dadosCONV$mae_p210)
#summary(modcusto) 
#marginalmente significativo

modcusto4<-lm(dadosCONV$custototal.dia~dadosCONV$VG_capacidade_maternal)
summary(modcusto4) 
#Mantendo tudo constante, o aumento de um ponto na capacidade maternal aumenta o custo em 0,007 euros

modcusto5<-lm(dadosCONV$custototal.dia~dadosCONV$VG_cap_crescimento)
summary(modcusto5)
#n�o apresentou significancia

modcusto6<-lm(dadosCONV$custototal.dia~dadosCONV$VG_gmd_estacao)
summary(modcusto6) 
#n�o apresentou significancia

modcusto7<-lm(dadosCONV$custototal.dia~dadosCONV$VG_carcaca_dia_idade)
summary(modcusto7) 
#Mantendo tudo constante, quando aumento em 1 unidade o VG da carc dia idade, o custo por dia aumenta em 0,003euros

modcusto8<-lm(dadosCONV$custototal.dia~dadosCONV$VG_Intervalo_Entre_partos)
summary(modcusto8) 
#n�o apresentou significancia

modcusto9<-lm(dadosCONV$custototal.dia~dadosCONV$VG_indice_conversao)
summary(modcusto9) 
#N�o apresentou significancia

modcusto10<-lm(dadosCONV$custototal.dia~dadosCONV$VG_longevidade_produtiva)
summary(modcusto10) 
#n�o apresentou significancia

modcusto11<-lm(dadosCONV$custototal.dia~dadosCONV$VG_consumo_alim_residual)
summary(modcusto11) 
#n�o apresentou significancia

modcusto14<-lm(dadosCONV$custototal.dia~dadosCONV$GMD)
summary(modcusto14)
#n�o apresentou significancia


########################################################################################
#################RLM custo total de produ��o CONV/dia na engorda############################

modcustodiaM1<-lm(custototal.dia~peso_ent+Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade, data = dadosCONV)
summary(modcustodiaM1) #O peso a entrada n�o se mostrou significativo

modcustodiaM2<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade, data = dadosCONV)
summary(modcustodiaM2)
##Todas as vari�veis s�o significativas e R�=0,57

#ADD as vari�veis que n�o apresentaram significancia na fase univariada levando em considera��o
#Conversa professor Gon�alo Jacinto Skype
#vari�veis importantes VG_cap_crescimento, P210, GMD e VG_carcaca_dia_idade

modcustodiaM3<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_cap_crescimento, data = dadosCONV)
summary(modcustodiaM3) #n�o

modcustodiaM4<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data = dadosCONV)
summary(modcustodiaM4) #n�o

modcustodiaM5<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD, data = dadosCONV)
summary(modcustodiaM5) #sig

#at� o momento modelo obtido � modcustodiaM5 e R� = 60,59%
##################################TESTANDO AS INTERA��ES#########################

modcustodiaMi1<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+Idadeent_meses*P210, data = dadosCONV)
summary(modcustodiaMi1) #n�o

modcustodiaMi2<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+Idadeent_meses*VG_capacidade_maternal, data = dadosCONV)
summary(modcustodiaMi2) #significativa e R�=62,%

modcustodiaMi3<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+Idadeent_meses*VG_carcaca_dia_idade, data = dadosCONV)
summary(modcustodiaMi3) #p-valor > 0,001 n�o entra

modcustodiaMi4<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+Idadeent_meses*GMD, data = dadosCONV)
summary(modcustodiaMi4) #n�o

modcustodiaMi5<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+P210*VG_capacidade_maternal, data = dadosCONV)
summary(modcustodiaMi5)#n�o

modcustodiaMi6<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+P210*VG_carcaca_dia_idade, data = dadosCONV)
summary(modcustodiaMi6) #n�o

modcustodiaMi7<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+P210*GMD, data = dadosCONV)
summary(modcustodiaMi7) #p-valor maior que 0,001

modcustodiaMi8<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+VG_capacidade_maternal*VG_carcaca_dia_idade, data = dadosCONV)
summary(modcustodiaMi8) #p-valor>0,001

modcustodiaMi9<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+VG_capacidade_maternal*GMD, data = dadosCONV)
summary(modcustodiaMi9) #p-valor>0,001

modcustodiaMi10<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+VG_carcaca_dia_idade*GMD, data = dadosCONV)
summary(modcustodiaMi10) #n�o

#O modelo obtido foi modcustodiaMi2
####ANALISE DOS RESIDUOS DO MODELO########################

#Analise dos res�duos
resid<-resid(modcustodiaMi2) # residuos do Modelo multivariado
pred<-fitted(modcustodiaMi2) # Valores ajustados pelo modelo
resid.std <- rstandard(modcustodiaMi2)
plot(resid~pred)

library(nortest)
lillie.test(resid) #n�o admite a normalidade
shapiro.test(resid)
qqnorm(resid)
identify(qqnorm(resid))
qqline(resid)#muitos pontos outliers
# Histograma dos residuos
hist(resid)  #n�o aparentam ter distr Normal
# Avaliar Achatamento
library(moments)
# teste para a curtose
anscombe.test(resid) #os res�duos n�o s�o mesocurticos
# teste para a Assimetria
agostino.test(resid)  #Memso considerando a simetria, falha a normalidade


library(car)
##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustodiaMi2)
#N�o possui independencia

library(lmtest)
# Breusch- Pagan test
bptest(modcustodiaMi2) 
#apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaMi2) # variance inflation factors 
sqrt(vif(modcustodiaMi2)) > 2 # Se verdade existe multicolinearidade

#N�O PASSA NOS PRESSUPOSTOS
plot(modcustodiaMi2) #presen�a de alguns individuos outliers
plot(resid.std)

######################################################################
###########VERIFICA��O DOS VALORES INFLUENTES#########################
#Modelo modcustodiaMi2 
#teste T para observa��es outliers
pvalue<-2*(1-pt(abs(resid.std),length(resid.std)))

# Devolve os outliers a 10%
names(pvalue)<-1:length(resid.std)
pvalue[pvalue<0.1]


#nenhum ultrapassa o limite indicado
dcook<-cooks.distance(modcustodiaMi2)
names(dcook)<-1:length(dcook)
plot(dcook,ylim=c(0,1.1))
abline(h=1.0,col="red")
identify(dcook)
#nenhum ultrapassa o limite sugerido pela distancia de cooks

#ser� retirado os animais que apresentaram-se outliers 
dadosCONV2<-dadosCONV[-c(15,26,31,33,34,174,175,176,177,178,179,215,216,226,230,231,232,233,234,235),]

modcustodiaMi21<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+Idadeent_meses*VG_capacidade_maternal, data = dadosCONV2)
summary(modcustodiaMi21)
#quando retira os outliersa o R�=61%

#Analise dos res�duos
resid<-resid(modcustodiaMi21)
pred<-fitted(modcustodiaMi21)
resid.std <- rstandard(modcustodiaMi21)

lillie.test(resid) #admite a normalidade
shapiro.test(resid)

qqnorm(resid)
identify(qqnorm(resid))
qqline(resid)#pontos outliers
# Histograma dos residuos
hist(resid)  #n�o aparentam ter distr Normal
# Avaliar Achatamento

# teste para a curtose
anscombe.test(resid) 
# teste para a Assimetria
agostino.test(resid)  

##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustodiaMi21)
#N�o possui independencia

# Breusch- Pagan test
bptest(modcustodiaMi21) 
#apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaMi21) # variance inflation factors 
sqrt(vif(modcustodiaMi21)) > 2 
#O modelo n�o fica bom
#####################################################################
#####################################################################



#Modelo com as vari�veis da entrada

modcustoMENT<-lm(custototal.dia~peso_ent+Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade,data = dadosCONV)
summary(modcustoMENT) #peso n�o � significativa


modcustoMENT1<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade,data = dadosCONV)
summary(modcustoMENT1) 

#Add capacidade de crescimento
modcustoMENT2<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_cap_crescimento,data = dadosCONV)
summary(modcustoMENT2) #n�o

#add VG GMD esta��o
modcustoMENT3<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_gmd_estacao,data = dadosCONV)
summary(modcustoMENT3) #n�o

#add VG_consumo_alim_residual
modcustoMENT4<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_consumo_alim_residual,data = dadosCONV)
summary(modcustoMENT4) #n�o

#add VG_indice_conversao
modcustoMENT5<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_indice_conversao,data = dadosCONV)
summary(modcustoMENT5) #n�o

#add VG_Intervalo_Entre_partos
modcustoMENT6<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos,data = dadosCONV)
summary(modcustoMENT6) #n�o

#add VG_longevidade_produtiva
modcustoMENT7<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_longevidade_produtiva,data = dadosCONV)
summary(modcustoMENT7) #n�o

#####################################################################################
#modcustoMENT1
#testando intera��o
modcustoMENI1<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+Idadeent_meses*P210,data = dadosCONV)
summary(modcustoMENI1) #n�o

modcustoMENI2<-lm(custototal.dia~Idadeent_meses+VG_capacidade_maternal+VG_carcaca_dia_idade+Idadeent_meses*VG_capacidade_maternal,data = dadosCONV)
summary(modcustoMENI2) #ajuda a explicar aumenta R� em 3%

modcustoMENI3<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+Idadeent_meses*VG_carcaca_dia_idade,data = dadosCONV)
summary(modcustoMENI3) #n�o ajuda a explicar

modcustoMENI4<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+P210*VG_capacidade_maternal, data = dadosCONV)
summary(modcustoMENI4) #n�o

modcustoMENI5<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+P210*VG_carcaca_dia_idade, data = dadosCONV)
summary(modcustoMENI5) #n�o

modcustoMENI6<-lm(custototal.dia~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_capacidade_maternal*VG_carcaca_dia_idade, data = dadosCONV)
summary(modcustoMENI6) #n�o
######################################################################################
#Testando os res�duos dos modelos
#modcustoMENT1 = sem intera��o #n�o apresenta normalidade nos residuos
#modcustoMENI2 = com intera��o

#Analise dos res�duos
resid<-resid(modcustoMENI2)
pred<-fitted(modcustoMENI2)
resid.std <- rstandard(modcustoMENI2)

lillie.test(resid) #admite a normalidade
shapiro.test(resid)

qqnorm(resid)
identify(qqnorm(resid))
qqline(resid)#pontos outliers
# Histograma dos residuos
hist(resid)  #n�o aparentam ter distr Normal
# Avaliar Achatamento

# teste para a curtose
anscombe.test(resid) 
# teste para a Assimetria
agostino.test(resid)  

##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustoMENI2)
#N�o possui independencia

# Breusch- Pagan test
bptest(modcustoMENI2) 
#apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustoMENI2) # variance inflation factors 
sqrt(vif(modcustoMENI2)) > 2 

###########VERIFICA��O DOS VALORES INFLUENTES#########################
#Modelo modcustodiaMi2 
#teste T para observa��es outliers
pvalue<-2*(1-pt(abs(resid.std),length(resid.std)))

# Devolve os outliers a 10%
names(pvalue)<-1:length(resid.std)
pvalue[pvalue<0.01]

#ser� retirado os animais que apresentaram-se outliers 
dadosCONV2<-dadosCONV[-c(1, 7, 332, 333, 336, 337, 338),]

modcustodiaMi21<-lm(custototal.dia~Idadeent_meses+VG_capacidade_maternal+VG_carcaca_dia_idade+Idadeent_meses*VG_capacidade_maternal, data = dadosCONV2)
summary(modcustodiaMi21)
#quando retira os outliersa o R�=34%

#Analise dos res�duos
resid<-resid(modcustodiaMi21)
pred<-fitted(modcustodiaMi21)
resid.std <- rstandard(modcustodiaMi21)

lillie.test(resid) 
shapiro.test(resid)

qqnorm(resid)
identify(qqnorm(resid))
qqline(resid)#pontos outliers
# Histograma dos residuos
hist(resid)  #n�o aparentam ter distr Normal
# Avaliar Achatamento

# teste para a curtose
anscombe.test(resid) 
# teste para a Assimetria
agostino.test(resid)  

##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustodiaMi21)
#N�o possui independencia

# Breusch- Pagan test
bptest(modcustodiaMi21) 
#apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaMi21) # variance inflation factors 
sqrt(vif(modcustodiaMi21)) > 2 
#O modelo n�o fica bom





####################################################################################
#####################################################################################

#transforma��o
#log, ra�s, inversa, ao quadrado
dadosCONV$custodialog<-log(dadosCONV$custototal.dia)
##MODELO SIMPLES####
modcustolog1<-lm(dadosCONV$custodialog~dadosCONV$peso_ent)
summary(modcustolog1) #significativo

modcustolog2<-lm(dadosCONV$custodialog~dadosCONV$Idadeent_meses)
summary(modcustolog2) #significativo

modcustolog3<-lm(dadosCONV$custodialog~dadosCONV$P210)
summary(modcustolog3) #signif

modcustolog4<-lm(dadosCONV$custodialog~dadosCONV$VG_capacidade_maternal)
summary(modcustolog4) #signif

modcustolog5<-lm(dadosCONV$custodialog~dadosCONV$VG_cap_crescimento)
summary(modcustolog5)
#n�o apresentou significancia

modcustolog6<-lm(dadosCONV$custodialog~dadosCONV$VG_gmd_estacao)
summary(modcustolog6) 
#n�o apresentou significancia

modcustolog7<-lm(dadosCONV$custodialog~dadosCONV$VG_carcaca_dia_idade)
summary(modcustolog7) #signif

modcustolog8<-lm(dadosCONV$custodialog~dadosCONV$VG_Intervalo_Entre_partos)
summary(modcustolog8) 
#n�o apresentou significancia

modcustolog9<-lm(dadosCONV$custodialog~dadosCONV$VG_indice_conversao)
summary(modcustolog9) 
#N�o apresentou significancia

modcustolog10<-lm(dadosCONV$custodialog~dadosCONV$VG_longevidade_produtiva)
summary(modcustolog10) 
#n�o apresentou significancia

modcustolog11<-lm(dadosCONV$custodialog~dadosCONV$VG_consumo_alim_residual)
summary(modcustolog11) 
#n�o apresentou significancia

modcustolog12<-lm(dadosCONV$custodialog~dadosCONV$GMD)
summary(modcustolog12)
#n�o apresentou significancia

#######Multivariado transformada

modcustologM1<-lm(custodialog~peso_ent+Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade, data=dadosCONV)
summary(modcustologM1)
#o peso e o intercepto n�o apresentam sig

#retirando o intercepto
modcustologM2<-lm(custodialog~-1+peso_ent+Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade, data=dadosCONV)
summary(modcustologM2)
#retirando o peso
modcustologM2<-lm(custodialog~-1+Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade, data=dadosCONV)
summary(modcustologM2) #R�=99%

#teste
modcustologM2<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade, data=dadosCONV)
summary(modcustologM2) 

#ADD vari�veis 
#VG_cap_crescimento
modcustologM3<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_cap_crescimento, data=dadosCONV)
summary(modcustologM3) #n�o

#VG_gmd_estacao
modcustologM4<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_gmd_estacao, data=dadosCONV)
summary(modcustologM4)#n�o

#VG_Intervalo_Entre_partos
modcustologM5<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dadosCONV)
summary(modcustologM5) #n�o

modcustologM6<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_indice_conversao, data=dadosCONV)
summary(modcustologM6) #n�o

modcustologM7<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_longevidade_produtiva, data=dadosCONV)
summary(modcustologM7) #n�o

modcustologM8<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_consumo_alim_residual, data=dadosCONV)
summary(modcustologM8) #n�o

modcustologM9<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD, data=dadosCONV)
summary(modcustologM9) #significante


resid<-resid(modcustologM9)
pred<-fitted(modcustologM9)
resid.std <- rstandard(modcustologM9)

lillie.test(resid) 
shapiro.test(resid)
qqnorm(resid)
identify(qqnorm(resid))
qqline(resid)#pontos outliers
# Histograma dos residuos
hist(resid)  #n�o aparentam ter distr Normal
# Avaliar Achatamento

# teste para a curtose
anscombe.test(resid) 
# teste para a Assimetria
agostino.test(resid)  #simetria

##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustologM9)
#N�o possui independencia

# Breusch- Pagan test
bptest(modcustologM9) 
#apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustologM9) # variance inflation factors 
sqrt(vif(modcustologM9)) > 2 

#Modelo modcustologM9 Simetrico, homocedastico e com ausencia de multicolinearidade

###############Intera��o

modcustologMI1<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+Idadeent_meses*P210, data=dadosCONV)
summary(modcustologMI1) #n�o

modcustologMI2<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+Idadeent_meses*VG_capacidade_maternal, data=dadosCONV)
summary(modcustologMI2)

modcustologMI3<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+Idadeent_meses*VG_carcaca_dia_idade, data=dadosCONV)
summary(modcustologMI3)

modcustologMI4<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+Idadeent_meses*GMD, data=dadosCONV)
summary(modcustologMI4) #n�o

modcustologMI5<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+P210*VG_capacidade_maternal, data=dadosCONV)
summary(modcustologMI5) #n�o

modcustologMI6<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+P210*VG_carcaca_dia_idade, data=dadosCONV)
summary(modcustologMI6) #n�o

modcustologMI7<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+P210*GMD, data=dadosCONV)
summary(modcustologMI7)

modcustologMI8<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+VG_capacidade_maternal*VG_carcaca_dia_idade, data=dadosCONV)
summary(modcustologMI8) #n�o

modcustologMI9<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+VG_capacidade_maternal*GMD, data=dadosCONV)
summary(modcustologMI9) #n�o

modcustologMI10<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD+VG_carcaca_dia_idade*GMD, data=dadosCONV)
summary(modcustologMI10) #n�o
####################N�o tem intera��o


###########VERIFICA��O DOS VALORES INFLUENTES#########################
#Modelo modcustodiaMi2 n�o passa nos pressupostos
#portanto ser� usado o modelo sem intera��o para retirar os outliers
#teste T para observa��es outliers
pvalue<-2*(1-pt(abs(resid.std),length(resid.std)))

# Devolve os outliers a 10%
names(pvalue)<-1:length(resid.std)
pvalue[pvalue<0.01]

#ser� retirado os animais que apresentaram-se outliers 
dadosCONV2<-dadosCONV[-c(15,26,175,176,177,178,179,230,232,233,234,235),]

modcustodiaM91<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade+GMD, data = dadosCONV2)
summary(modcustodiaMi21)
#quando retira os outliersa o R�=34%

#Analise dos res�duos
resid<-resid(modcustodiaMi21)
pred<-fitted(modcustodiaMi21)
resid.std <- rstandard(modcustodiaMi21)

lillie.test(resid)
shapiro.test(resid)

qqnorm(resid)
identify(qqnorm(resid))
qqline(resid)#pontos outliers
# Histograma dos residuos
hist(resid)  #n�o aparentam ter distr Normal
# Avaliar Achatamento

# teste para a curtose
anscombe.test(resid) 
# teste para a Assimetria
agostino.test(resid)  #simetria

##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustodiaMi21)
#N�o possui independencia

# Breusch- Pagan test
bptest(modcustodiaMi21) 
#apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaMi21) # variance inflation factors 
sqrt(vif(modcustodiaMi21)) > 2 

#apresenta simetria, homocedasticidade e multicolinearidade
#########################################################################################3
###########################################################################3
#########################################################################3


#Modelo dados a entrada
modcustologM1<-lm(custodialog~peso_ent+Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade, data=dadosCONV)
summary(modcustologM1)

modcustologM1<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade, data=dadosCONV)
summary(modcustologM1)
anova(modcustologM1)

#Analise dos res�duos
resid<-resid(modcustologM1)
pred<-fitted(modcustologM1)
resid.std <- rstandard(modcustologM1)

lillie.test(resid)
shapiro.test(resid)

qqnorm(resid)
qqline(resid)#pontos outliers
# Histograma dos residuos
hist(resid)  #n�o aparentam ter distr Normal
# Avaliar Achatamento

# teste para a curtose
anscombe.test(resid) 
# teste para a Assimetria
agostino.test(resid)  #simetria

##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustologM1)
#N�o possui independencia

# Breusch- Pagan test
bptest(modcustologM1) 
#apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustologM1) # variance inflation factors 
sqrt(vif(modcustologM1)) > 2 
plot(resid)
#aqui passa na simetria, homocedasticidade e multicolinearidade



###########VERIFICA��O DOS VALORES INFLUENTES#########################
#teste T para observa��es outliers
pvalue<-2*(1-pt(abs(resid.std),length(resid.std)))

# Devolve os outliers a 1%
names(pvalue)<-1:length(resid.std)
pvalue[pvalue<0.01]

#ser� retirado os animais que apresentaram-se outliers 
dadosCONV2<-dadosCONV[-c(1, 3, 4, 5, 6, 7, 8, 231, 232, 233, 234, 235),]

modcustodiaM21<-lm(custodialog~Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade, data = dadosCONV2)
summary(modcustodiaM21)
#quando retira os outliers o R�=60,7%

#Analise dos res�duos
resid<-resid(modcustodiaM21)
pred<-fitted(modcustodiaM21)
resid.std <- rstandard(modcustodiaM21)

lillie.test(resid)
shapiro.test(resid) #retirar os valores influentes n�o alterou em nada, portanto decidi ficar com eles
# Avaliar Achatamento
# teste para a curtose
anscombe.test(resid) 
# teste para a Assimetria
agostino.test(resid)  #simetria

##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustodiaM21)
#N�o possui independencia

# Breusch- Pagan test
bptest(modcustodiaM21) 
#apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaM21) # variance inflation factors 
sqrt(vif(modcustodiaM21)) > 2 

#teste retirar o intercepto
modcustologM1<-lm(custodialog~-1+Idadeent_meses+P210+VG_capacidade_maternal+VG_carcaca_dia_idade, data=dadosCONV)
summary(modcustologM1)
AIC(modcustologM1)
#n�o faz sentido retirar o intercepto, no caso em quest�o n�o faz sentido custo negativo

#Analise dos res�duos
resid<-resid(modcustologM1)
pred<-fitted(modcustologM1)
resid.std <- rstandard(modcustologM1)

lillie.test(resid)
shapiro.test(resid) #retirar os valores influentes n�o alterou em nada, portanto decidi ficar com eles
# Avaliar Achatamento
# teste para a curtose
anscombe.test(resid) 
# teste para a Assimetria
agostino.test(resid)  #simetria

##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustologM1)
#N�o possui independencia

# Breusch- Pagan test
bptest(modcustologM1) 
#apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustologM1) # variance inflation factors 
sqrt(vif(modcustologM1)) > 2 
#d� problema de multicolinearidade

#portanto, modelo selecionado modcustologM1 sem retirar os outliers
par(mfrow = c(2,2))
plot(modcustologM1)
exp(modcustologM1$coefficients)



###############
##############Testando glm
MULTGAMMAIDENT<-glm(custodialog~Idadeent_meses + P210+VG_capacidade_maternal+VG_carcaca_dia_idade, data=dadosCONV,family=Gamma(link = identity), na.action = na.exclude)
summary(MULTGAMMAIDENT)
library(car)
vif(MULTGAMMAIDENT)
#R�
(n<-length(dadosCONV$custodialog)) 
(R2N<-(1-exp((MULTGAMMAIDENT$dev-MULTGAMMAIDENT$null)/n))/(1-exp(-MULTGAMMAIDENT$null/n)))  #R� = 0,457

#varia��o explicada do modelo com resposta continua R�
1-((deviance(MULTGAMMAIDENT)/(df.residual(MULTGAMMAIDENT))))/(MULTGAMMAIDENT$null.deviance/MULTGAMMAIDENT$df.null)
#



