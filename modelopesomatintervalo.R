#Modelo geral caracteristicas de entrada
getwd()

library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=T, sep=";", dec=",")

modcustodiaENT<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade,  data = dados)
summary(modcustodiaENT)
#idade
modcustodiaENT<-lm(custototal.dia~peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade,  data = dados)
summary(modcustodiaENT)


#####################Teste de intera��es############################################

modcustodiaENT1<-lm(custototal.dia~peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade+peso_ent*VG_cap_crescimento,  data = dados)
summary(modcustodiaENT1)
#A intera��o n�o apresentou significancia

modcustodiaMI2<-lm(custototal.dia~peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade+peso_ent*VG_capacidade_maternal,  data = dados)
summary(modcustodiaMI2)
#n�o

modcustodiaMI3<-lm(custototal.dia~peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade+peso_ent*VG_carcaca_dia_idade,  data = dados)
summary(modcustodiaMI3)
#n�o

modcustodiaMI4<-lm(custototal.dia~peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_cap_crescimento*VG_capacidade_maternal,  data = dados)
summary(modcustodiaMI4)

modcustodiaMI5<-lm(custototal.dia~peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_cap_crescimento*VG_carcaca_dia_idade,  data = dados)
summary(modcustodiaMI5)

modcustodiaMI6<-lm(custototal.dia~peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade+VG_capacidade_maternal*VG_carcaca_dia_idade,  data = dados)
summary(modcustodiaMI6)

#nenhuma intera��o significativa
###########################################################################################

#Analise dos res�duos
resid<-resid(modcustodiaENT) # residuos do Modelo multivariado
pred<-fitted(modcustodiaENT) # Valores ajustados pelo modelo
resid.std <- rstandard(modcustodiaENT)

library(nortest)
lillie.test(resid) #n�o admite a normalidade
qqnorm(resid)
qqline(resid)#muitos pontos outliers
# Histograma dos residuos
hist(resid)  #n�o aparentam ter distr Normal
# Avaliar Achatamento
library(moments)
# teste para a curtose
anscombe.test(resid) #os res�duos s�o mesocurticos
# teste para a Assimetria
agostino.test(resid)


library(car)
##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustodiaENT)
#N�o possui independencia

library(lmtest)
# Breusch- Pagan test
bptest(modcustodiaENT) 
#apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaENT) # variance inflation factors 
sqrt(vif(modcustodiaENT)) > 2 # Se verdade existe multicolinearidade

############################VALORES INFLUENTES##########################################
# vamos fazer um teste T para ver quais as observacoes sao consideradas outliers
pvalue<-2*(1-pt(abs(resid.std),length(resid.std)))

# Devolve os outliers ao nivel 1%
names(pvalue)<-1:length(resid.std)
pvalue[pvalue<0.01]

lev<-hatvalues(modcustodiaM8)
lev
#como s�o muitos valores influentes
names(lev)<-1:length(lev)
plot(pred,lev,abline(h=0.2))
plot(pred,lev,ylim=c(0,0.2),abline(h=0.2))

#nenhum ultrapassa o limite indicado
dcook<-cooks.distance(modcustodiaM8)
names(dcook)<-1:length(dcook)
plot(dcook,ylim=c(0,1.1))
abline(h=1.0,col="red")
identify(dcook)
#nenhum ultrapassa o limite sugerido pela distancia de cooks

#ser� retirado os animais que apresentaram-se outliers no test t
dados2<-dados[-c(2,3,264,265,266,267,268,333,337,338,348,360,363,703,710),]

modcustodiaENTB2<-lm(custototal.dia~peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade, data = dados2)
summary(modcustodiaENTB2)
anova(modcustodiaENTB2)


#Analise dos res�duos
resid<-resid(modcustodiaENTB2)
pred<-fitted(modcustodiaENTB2)
resid.std <- rstandard(modcustodiaENTB2)

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
durbinWatsonTest(modcustodiaENTB2)
#N�o possui independencia

# Breusch- Pagan test
bptest(modcustodiaENTB2) 

bptest(modcustodiaENTB2, varformula = ~ fitted.values(modcustodiaENTB2), studentize = TRUE)
#n�o apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaENTB2) # variance inflation factors 
sqrt(vif(modcustodiaENTB2)) > 2 # Se verdade existe multicolinearidade

