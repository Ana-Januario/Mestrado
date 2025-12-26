###############RLM CUSTO POR DIA GERAL###############################
getwd()

library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=T, sep=";", dec=",")


library(fBasics)
hist(dados$custototal.dia, main = "Histograma custo total/dia na engorda", xlab = "Custo total/dia", ylab = "Frequ�ncia")
boxplot(dados$custototal.dia)

#NORMALIDADE
shapiro.test(sqrt(dados$custototal.dia)) #A variavel passa na normalidade
lillieTest(sqrt(dados$custototal.dia))

shapiro.test(dadosCONV$custototal.dia) #A variavel passa na normalidade
lillieTest(dadosCONV$custototal.dia)

shapiro.test(dadosDOP$custototal.dia) #A variavel passa na normalidade
lillieTest(dadosDOP$custototal.dia)

#Grafico para apresentar na tese
grafico<-hist(dados$custototal.dia, main = "Custo total por dia na engorda",xlab = "Custo total/dia na engorda", ylab = "Frequencias")
min(dados$custototal.dia)
seq(1,5,length=22)
xajust<-seq(min(dados$custototal.dia, na.rm=T),  
            max(dados$custototal.dia, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$custototal.dia,na.rm=T),
              sd=sd(dados$custototal.dia,na.rm=T))
hist(dados$custototal.dia, freq = F, main = "Histograma Custo total por dia na engorda", xlab = "Dias na engorda", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)


##MODELO SIMPLES####
modcusto1<-lm(dados$custototal.dia~dados$peso_ent)
summary(modcusto1) #R�adj=0,409

modcusto2<-lm(dados$custototal.dia~dados$Idadeent_meses)
summary(modcusto2) #R�adj=0,03
#Mantendo tudo constante, quando aumento em 1 mes da idade na entrada, o custo por dia aumenta em 0,03 euros

modcusto3<-lm(dados$custototal.dia~dados$P210)
summary(modcusto3) #R�adj=0,15
#Mantendo tudo constante, quando aumento em 1 unidade o P210, o custo aumenta em 0,003 euros

#n�o entrou por apresentar muito NA
#modcusto4<-lm(dados$custototal.dia~dados$Pai_p210)
#summary(modcusto4) #R�adj=0,00
#a vari�vel n�o apresentou significancia

#n�o entrou por apresentar muito NA
#modcusto5<-lm(dados$custototal.dia~dados$mae_p210)
#summary(modcusto5) #R�adj=0,01
#n�o apresentou significancia

modcusto6<-lm(dados$custototal.dia~dados$VG_capacidade_maternal)
summary(modcusto6) #R�adj=0,004
#Mantendo tudo constante, o aumento de um ponto na capacidade maternal aumenta o custo em 0,006 euros

modcusto7<-lm(dados$custototal.dia~dados$VG_cap_crescimento)
summary(modcusto7) #R�adj=0,007
#Mantendo tudo constante o aumento de 1 ponto nessa VG aumenta o custo em 0,001

modcusto8<-lm(dados$custototal.dia~dados$VG_gmd_estacao)
summary(modcusto8) #R�adj=0,0000
#n�o apresentou significancia

modcusto9<-lm(dados$custototal.dia~dados$VG_carcaca_dia_idade)
summary(modcusto9) #R�adj=0,11
#Mantendo tudo constante, quando aumento em 1 unidade o VG da carc dia idade, o custo por dia aumenta em 0,003euros

modcusto10<-lm(dados$custototal.dia~dados$VG_Intervalo_Entre_partos)
summary(modcusto10) #R�adj=0,00
#n�o apresentou significancia

modcusto11<-lm(dados$custototal.dia~dados$VG_indice_conversao)
summary(modcusto11) #R�adj=0,001
#N�o apresentou significancia

modcusto12<-lm(dados$custototal.dia~dados$VG_longevidade_produtiva)
summary(modcusto12) #R�adj=0,001
#n�o apresentou significancia

modcusto13<-lm(dados$custototal.dia~dados$VG_consumo_alim_residual)
summary(modcusto13) #R�adj=0,001
#n�o apresentou significancia

#N�o entra por n�o ser informa��o disponivel no inicio da recria
modcusto14<-lm(dados$custototal.dia~dados$GMD)
summary(modcusto14) #R�adj=0,001
#n�o apresentou significancia

modcusto15<-lm(dados$custototal.dia~dados$Dest_abate)
summary(modcusto15)

########################################################################################
#################RLM custo total de produ��o/dia na engorda############################
modcustodiaM<-lm(custototal.dia~peso_ent+Idadeent_meses+P210+GMD+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade+Dest_abate,  data = dados)
summary(modcustodiaM)

#Retira p210 maior p-valor
modcustodiaM1<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade+Dest_abate,  data = dados)
summary(modcustodiaM1)
#REtira carca�a dia idade
modcustodiaM2<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_cap_crescimento+VG_capacidade_maternal+Dest_abate,  data = dados)
summary(modcustodiaM2)
#retira capacidade de crescimento
modcustodiaM3<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_capacidade_maternal+Dest_abate,  data = dados)
summary(modcustodiaM3)
#cheguei a esse modelo, testei os residuos e n�o passa na normalidade

############################################################################################
#INTERA��O
modcustodiaMI1<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_capacidade_maternal+Dest_abate+peso_ent*Idadeent_meses,  data = dados)
summary(modcustodiaMI1)
#n�o
modcustodiaMI2<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_capacidade_maternal+Dest_abate+peso_ent*GMD,  data = dados)
summary(modcustodiaMI2)
#n�o
modcustodiaMI3<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_capacidade_maternal+Dest_abate+peso_ent*VG_capacidade_maternal,  data = dados)
summary(modcustodiaMI3)
#Marginalmente significativa, mas n�o explica nada
modcustodiaMI4<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_capacidade_maternal+Dest_abate+peso_ent*Dest_abate,  data = dados)
summary(modcustodiaMI4)
#tem alguma significancia mas n�o ajuda a explicar o modelo
modcustodiaMI5<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_capacidade_maternal+Dest_abate+Idadeent_meses*GMD,  data = dados)
summary(modcustodiaMI5)
#tem alguma significancia mas n�o ajuda a explicar o modelo
modcustodiaMI6<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_capacidade_maternal+Dest_abate+Idadeent_meses*VG_capacidade_maternal,  data = dados)
summary(modcustodiaMI6)
#tem alguma significancia mas n�o ajuda a explicar o modelo
modcustodiaMI7<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_capacidade_maternal+Dest_abate+Idadeent_meses*Dest_abate,  data = dados)
summary(modcustodiaMI7)
#tem alguma significancia mas n�o ajuda a explicar o modelo
modcustodiaMI8<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_capacidade_maternal+Dest_abate+GMD*VG_capacidade_maternal,  data = dados)
summary(modcustodiaMI8)
#tem alguma significancia mas n�o ajuda a explicar o modelo
modcustodiaMI9<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_capacidade_maternal+Dest_abate+GMD*Dest_abate,  data = dados)
summary(modcustodiaMI9)
#n�o
modcustodiaMI10<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_capacidade_maternal+Dest_abate+VG_capacidade_maternal*Dest_abate,  data = dados)
summary(modcustodiaMI10)
#n�o

#nenhuma intera��o se mostrou significativa o suficiente a ponto de explicar o modelo

####ANALISE DOS RESIDUOS DO MODELO########################

#modelo modcustodiaM3 sem intera��o
#Analise dos res�duos
resid<-resid(modcustodiaM3) # residuos do Modelo multivariado
pred<-fitted(modcustodiaM3) # Valores ajustados pelo modelo
resid.std <- rstandard(modcustodiaM3)

library(nortest)
lillie.test(resid) #n�o admite a normalidade
qqnorm(resid)
qqline(resid)#muitos pontos outliers
identify(qqnorm(resid))
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
durbinWatsonTest(modcustodiaM3)
#N�o possui independencia

library(lmtest)
# Breusch- Pagan test
bptest(modcustodiaM3) 
#Apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaM3) # variance inflation factors 
sqrt(vif(modcustodiaM3)) > 2 # Se verdade existe multicolinearidade

###########VERIFICA��O DOS VALORES INFLUENTES#########################
# vamos fazer um teste T para ver quais as observacoes sao consideradas outliers
pvalue<-2*(1-pt(abs(resid.std),length(resid.std)))

# Devolve os outliers ao nivel 1%
names(pvalue)<-1:length(resid.std)
pvalue[pvalue<0.01]

lev<-hatvalues(modcustodiaM3)
lev
#como s�o muitos valores influentes
names(lev)<-1:length(lev)
plot(pred,lev,abline(h=0.2))
plot(pred,lev,ylim=c(0,0.2),abline(h=0.2))

#nenhum ultrapassa o limite indicado
dcook<-cooks.distance(modcustodiaM3)
names(dcook)<-1:length(dcook)
plot(dcook,ylim=c(0,1.1))
abline(h=1.0,col="red")
identify(dcook)
#nenhum ultrapassa o limite sugerido pela distancia de cooks

#ser� retirado os animais que apresentaram-se outliers no test t
dados2<-dados[-c(2,3,59,263,264,265,266,267,268,325,326,331,332,333,334,335,336,337,338,348,360,608,621,703,710),]

modcustodiaM3B2<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_capacidade_maternal+Dest_abate, data = dados2)
summary(modcustodiaM3B2)

#Analise dos res�duos
resid<-resid(modcustodiaM3B2)
pred<-fitted(modcustodiaM3B2)
resid.std <- rstandard(modcustodiaM3B2)

lillie.test(resid) #n�o admite a normalidade
shapiro.test(resid)
qqnorm(resid)
qqline(resid)#muitos pontos outliers
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
durbinWatsonTest(modcustodiaM3B2)
#N�o possui independencia

# Breusch- Pagan test
bptest(modcustodiaM3B2) 
#n�o apresenta homocedasticidade
bptest(modcustodiaM3B2, varformula = ~ fitted.values(modcustodiaM3B2), studentize = TRUE)

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaM3B2) # variance inflation factors 
sqrt(vif(modcustodiaM3B2)) > 2 # Se verdade existe multicolinearidade

#n�o vale a pena retirar os valores influentes.


###########################################################
#########################################################
###########################################################
#RLM Backward
#A entrada

modcustodiaM0<- lm(dados$custototal.dia~dados$peso_ent+dados$Idadeent_meses+dados$P210+dados$VG_gmd_estacao+dados$VG_longevidade_produtiva+dados$VG_consumo_alim_residual+dados$VG_indice_conversao+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos,  data = dados)
summary(modcustodiaM0)
AIC(modcustodiaM0)

modcustodiaM<-lm(custototal.dia~peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos,  data = dados)
summary(modcustodiaM)
anova(modcustodiaM)
AIC(modcustodiaM)
#VG_gmd_estacao
#VG_longevidade_produtiva
#VG_consumo_alim_residual
#VG_indice_conversao -> era sig

#Analise dos res�duos
resid<-resid(modcustodiaM) # residuos do Modelo multivariado
pred<-fitted(modcustodiaM) # Valores ajustados pelo modelo
resid.std <- rstandard(modcustodiaM)

library(nortest)
lillie.test(resid) #n�o admite a normalidade
qqnorm(resid)
qqline(resid)#muitos pontos outliers
# Histograma dos residuos
hist(resid)  #n�o aparentam ter distr Normal
# Avaliar Achatamento
library(moments)
# teste para a curtose
anscombe.test(resid) 
# teste para a Assimetria
agostino.test(resid)


library(car)
##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustodiaM)
#N�o possui independencia

library(lmtest)
# Breusch- Pagan test
bptest(modcustodiaM) 
#apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaM) # variance inflation factors 
sqrt(vif(modcustodiaM)) > 2 # Se verdade existe multicolinearidade


library(hnp)
hnp(modcustodiaM$residuals, sim = 99,resid.type ='deviance',how.many.out=T ,
    conf = 0.95,scale = T, main = 'Gr�fico normal com envelope', xlab = 'Quartis te�ricos', ylab = 'Res�duos')

############################VALORES INFLUENTES##########################################
# vamos fazer um teste T para ver quais as observacoes sao consideradas outliers
pvalue<-2*(1-pt(abs(resid.std),length(resid.std)))

# Devolve os outliers ao nivel 1%
names(pvalue)<-1:length(resid.std)
pvalue[pvalue<0.01]

lev<-hatvalues(modcustodiaM)
lev
#como s�o muitos valores influentes
names(lev)<-1:length(lev)
plot(pred,lev,abline(h=0.2))
plot(pred,lev,ylim=c(0,0.2),abline(h=0.2))

#nenhum ultrapassa o limite indicado
dcook<-cooks.distance(modcustodiaM)
names(dcook)<-1:length(dcook)
plot(dcook,ylim=c(0,1.1))
abline(h=1.0,col="red")
identify(dcook)
#nenhum ultrapassa o limite sugerido pela distancia de cooks

#ser� retirado os animais que apresentaram-se outliers no test t
dados2<-dados[-c(2,3,263,264,265,266,267,268,333,337,338,348,360,363,710),]
modcustodiaMB2<-lm(custototal.dia~peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data = dados2)
summary(modcustodiaMB2)
AIC(modcustodiaMB2)
anova(modcustodiaMB2)
#Analise dos res�duos
resid<-resid(modcustodiaMB2)
pred<-fitted(modcustodiaMB2)
resid.std <- rstandard(modcustodiaMB2)

lillie.test(resid) #n�o admite a normalidade
shapiro.test(resid)
qqnorm(resid)
qqline(resid)#muitos pontos outliers
# Histograma dos residuos
hist(resid)  #n�o aparentam ter distr Normal

# Avaliar Achatamento
# teste para a curtose 
anscombe.test(resid) #atende curtose
# teste para a Assimetria
agostino.test(resid)  


##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustodiaMB2)
#N�o possui independencia

# Breusch- Pagan test
bptest(modcustodiaMB2) 
bptest(modcustodiaMB2, varformula = ~ fitted.values(modcustodiaMB2), studentize = TRUE)
vif(modcustodiaMB2) # variance inflation factors 
sqrt(vif(modcustodiaMB2)) > 2 # Se verdade existe multicolinearidade


par(mfrow = c(1,2))
plot(modcustodiaMB2)

library(hnp)
hnp(modcustodiaM$residuals, sim = 99,resid.type ='deviance',how.many.out=T ,
    conf = 0.95,scale = T, main = 'Gr�fico normal envelope com outliers', xlab = 'Quartis te�ricos', ylab = 'Res�duos')

hnp(modcustodiaMB2$residuals, sim = 99,resid.type ='deviance',how.many.out=T ,
    conf = 0.95,scale = T, main = 'Gr�fico normal envelope sem outliers', xlab = 'Quartis te�ricos', ylab = 'Res�duos')

