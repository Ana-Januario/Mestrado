###############RLM CUSTO POR DIA DOP###############################
getwd()

library(readxl)
dadosDOP=read.table("bov_mais_DOP.csv", header=T, sep=";", dec=",")
library(fBasics)
hist(dadosDOP$custototal.dia, main = "Histograma custo total DOP/dia na engorda", xlab = "Custo total/dia", ylab = "Frequ�ncia")
boxplot(dadosDOP$custototal.dia)

#NORMALIDADE
shapiro.test(dadosDOP$custototal.dia) #A variavel passa na normalidade
lillieTest(dadosDOP$custototal.dia)

##MODELO SIMPLES####
modcusto1<-lm(dadosDOP$custototal.dia~dadosDOP$peso_ent)
summary(modcusto1) #R�adj=0,52
#Mantendo tudo constante, quando aumento em 1 Kg o peso do animal na entrada, o custo por dia aumenta em 0,004 euros

modcusto2<-lm(dadosDOP$custototal.dia~dadosDOP$Idadeent_meses)
summary(modcusto2) #n�o significativo

modcusto3<-lm(dadosDOP$custototal.dia~dadosDOP$P210)
summary(modcusto3) #R�adj=0,15
#Mantendo tudo constante, quando aumento em 1 unidade o P210, o custo aumenta em 0,003 euros

#n�o entrou por apresentar muito NA
#modcusto<-lm(dadosDOP$custototal.dia~dadosDOP$Pai_p210)
#summary(modcusto) #R�adj=0,00
#a vari�vel n�o apresentou significancia

#modcusto<-lm(dadosDOP$custototal.dia~dadosDOP$mae_p210)
#summary(modcusto) #R�adj=0,01
#n�o apresentou significancia


modcusto4<-lm(dadosDOP$custototal.dia~dadosDOP$VG_capacidade_maternal)
summary(modcusto4) #R�adj=0,03
#Mantendo tudo constante, o aumento de um ponto na capacidade maternal aumenta o custo em 0,005 euros

modcusto5<-lm(dadosDOP$custototal.dia~dadosDOP$VG_cap_crescimento)
summary(modcusto5) #R�adj=0,02
#Mantendo tudo constante o aumento de 1 ponto nessa VG aumenta o custo em 0,003

modcusto6<-lm(dadosDOP$custototal.dia~dadosDOP$VG_gmd_estacao)
summary(modcusto6) #R�adj=0,0000
#n�o apresentou significancia

modcusto7<-lm(dadosDOP$custototal.dia~dadosDOP$VG_carcaca_dia_idade)
summary(modcusto7) #R�adj=0,23
#Mantendo tudo constante, quando aumento em 1 unidade o VG da carc dia idade, o custo por dia aumenta em 0,004euros

modcusto8<-lm(dadosDOP$custototal.dia~dadosDOP$VG_Intervalo_Entre_partos)
summary(modcusto8) #R�adj=0,00
#n�o apresentou significancia

modcusto9<-lm(dadosDOP$custototal.dia~dadosDOP$VG_indice_conversao)
summary(modcusto9) #R�adj=0,001
#N�o apresentou significancia

modcusto10<-lm(dadosDOP$custototal.dia~dadosDOP$VG_longevidade_produtiva)
summary(modcusto10) #R�adj=0,001
#n�o apresentou significancia

modcusto11<-lm(dadosDOP$custototal.dia~dadosDOP$VG_consumo_alim_residual)
summary(modcusto11) #R�adj=0,001
#n�o apresentou significancia

modcusto14<-lm(dadosDOP$custototal.dia~dadosDOP$GMD)
summary(modcusto14) #R�adj=0,03

########################################################################################
#################RLM custo total de produ��oDOP/dia na engorda############################
#Adicionando todas variaveis que foram significativas no modelo simples

modcustodiaM1<-lm(custototal.dia~peso_ent+P210+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+GMD, data = dadosDOP)
summary(modcustodiaM1)

#Retirando carca�a dia idade, apresentou maior p-value no teste t
modcustodiaM2<-lm(custototal.dia~peso_ent+P210+VG_capacidade_maternal+VG_cap_crescimento+GMD, data = dadosDOP)
summary(modcustodiaM2)

#Retirando capacidade maternal
modcustodiaM2<-lm(custototal.dia~peso_ent+P210+VG_cap_crescimento+GMD, data = dadosDOP)
summary(modcustodiaM2)

#Retirando P210
modcustodiaM2<-lm(custototal.dia~peso_ent+VG_cap_crescimento+GMD, data = dadosDOP)
summary(modcustodiaM2)

modcustodiaM2<-lm(custototal.dia~peso_ent+GMD, data = dadosDOP)
summary(modcustodiaM2)

#ADD as vari�veis que n�o apresentaram significancia na fase univariada

#idade a entrada
modcustodiaM3<-lm(custototal.dia~peso_ent+GMD+Idadeent_meses, data = dadosDOP)
summary(modcustodiaM3) #significativo e R�=0,66

#VG GMD esta��o
modcustodiaM4<-lm(custototal.dia~peso_ent+GMD+Idadeent_meses+VG_gmd_estacao, data = dadosDOP)
summary(modcustodiaM4) #n�o significativo

#VG_Intervalo_Entre_partos
modcustodiaM5<-lm(custototal.dia~peso_ent+GMD+Idadeent_meses+VG_Intervalo_Entre_partos, data = dadosDOP)
summary(modcustodiaM5) #n�o significativo

#VG_consumo_alim_residual
modcustodiaM6<-lm(custototal.dia~peso_ent+GMD+Idadeent_meses+VG_consumo_alim_residual, data = dadosDOP)
summary(modcustodiaM6) #significativo mas n�o altera o R�

#VG_longevidade_produtiva
modcustodiaM7<-lm(custototal.dia~peso_ent+GMD+Idadeent_meses+VG_longevidade_produtiva, data = dadosDOP)
summary(modcustodiaM7) #n�o significativa

#VG_indice_conversao
modcustodiaM8<-lm(custototal.dia~peso_ent+GMD+Idadeent_meses+VG_indice_conversao, data = dadosDOP)
summary(modcustodiaM8)#n�o significativa 

#modelo obtido modcustodiaM3
#modcustodiaM3<-lm(custototal.dia~peso_ent+GMD+Idadeent_meses, data = dadosDOP)
#summary(modcustodiaM3) 

#############################################################
##################################TESTANDO AS INTERA��ES

modcustodiaMi1<-lm(custototal.dia~peso_ent+GMD+Idadeent_meses+peso_ent*GMD, data = dadosDOP)
summary(modcustodiaMi1)#n�o

modcustodiaMi2<-lm(custototal.dia~peso_ent+GMD+Idadeent_meses+peso_ent*Idadeent_meses, data = dadosDOP)
summary(modcustodiaMi2) #n�o explica R� altera em 1%

modcustodiaMi3<-lm(custototal.dia~peso_ent+GMD+Idadeent_meses+GMD*Idadeent_meses, data = dadosDOP)
summary(modcustodiaMi3) #n�o explica R� altera em 1%
anova(modcustodiaMi3)
#modelo sem intera��o selecionado

####ANALISE DOS RESIDUOS DO MODELO########################
#modcustodiaM6
#Analise dos res�duos
resid<-resid(modcustodiaM3) # residuos do Modelo multivariado
pred<-fitted(modcustodiaM3) # Valores ajustados pelo modelo
resid.std <- rstandard(modcustodiaM3)

library(nortest)
lillie.test(resid) #n�o admite a normalidade
shapiro.test(resid)
qqnorm(resid)
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
durbinWatsonTest(modcustodiaM3)
#N�o possui independencia

library(lmtest)
# Breusch- Pagan test
bptest(modcustodiaM3) 
bptest(modcustodiaM3, varformula = ~ fitted.values(modcustodiaM3), studentize = TRUE)

#n�o apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaM3) # variance inflation factors 
sqrt(vif(modcustodiaM3)) > 2 # Se verdade existe multicolinearidade

#� UM MODELO FULEIRO


######################################################################
###########VERIFICA��O DOS VALORES INFLUENTES#########################

# vamos fazer um teste T para ver quais as observacoes sao consideradas outliers
pvalue<-2*(1-pt(abs(resid.std),length(resid.std)))

# Devolve os outliers ao nivel de 1%
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

#ser� retirado os animais que apresentaram-se outliers 
dadosDOP2<-dadosDOP[-c(6,295,374,375,377),]

modcustodiaM31<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD, data = dadosDOP2)
summary(modcustodiaM31) #quando retira os outliersa o R�=74

#Analise dos res�duos
resid<-resid(modcustodiaM31)
pred<-fitted(modcustodiaM31)
resid.std <- rstandard(modcustodiaM31)

lillie.test(resid) #admite a normalidade
shapiro.test(resid)
qqnorm(resid)
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
durbinWatsonTest(modcustodiaM31)
#N�o possui independencia

# Breusch- Pagan test
bptest(modcustodiaM31) 
#apresenta homocedasticidade
bptest(modcustodiaM31, varformula = ~ fitted.values(modcustodiaM31), studentize = TRUE)

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaM31) # variance inflation factors 
sqrt(vif(modcustodiaM31)) > 2 # Se verdade existe multicolinearidade

#O modelo selecionado foi modcustodiaM31 Mesocurtico, homoscedastico ausencia de multicolinearidade





##################################################################################
#MODELO COM AS VARI�VEIS A ENTRADA

modcustodiaM1<-lm(custototal.dia~peso_ent+P210+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade, data = dadosDOP)
summary(modcustodiaM1)
#retiro P210

modcustodiaM1<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade, data = dadosDOP)
summary(modcustodiaM1)

#add idade a entrada
modcustodiaM2<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+Idadeent_meses, data = dadosDOP)
summary(modcustodiaM2) #carca�a dia idade deixa de fazer sentido
#aumenta em 3% o R�

#retiro carca�a dia idade
modcustodiaM2<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento, data = dadosDOP)
summary(modcustodiaM2) 

modcustodiaM3<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_gmd_estacao, data = dadosDOP)
summary(modcustodiaM3) #n�o

modcustodiaM3<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_Intervalo_Entre_partos, data = dadosDOP)
summary(modcustodiaM3) #Marginalmente significante, mas explica o modelo em 1%

modcustodiaM3<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_indice_conversao, data = dadosDOP)
summary(modcustodiaM3) #Significante

modcustodiaM4<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_indice_conversao+VG_longevidade_produtiva, data = dadosDOP)
summary(modcustodiaM4) #n�o

modcustodiaM4<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_indice_conversao+VG_consumo_alim_residual, data = dadosDOP)
summary(modcustodiaM4) #n�o explica

#Intera��o

modcustodiaMI1<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_indice_conversao+peso_ent*Idadeent_meses, data = dadosDOP)
summary(modcustodiaMI1) #n�o

modcustodiaMI2<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_indice_conversao+peso_ent*VG_capacidade_maternal, data = dadosDOP)
summary(modcustodiaMI2) #n�o

modcustodiaMI3<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_indice_conversao+peso_ent*VG_cap_crescimento, data = dadosDOP)
summary(modcustodiaMI3) #n�o

modcustodiaMI4<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_indice_conversao+peso_ent*VG_indice_conversao, data = dadosDOP)
summary(modcustodiaMI4) #n�o

modcustodiaMI5<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_indice_conversao+Idadeent_meses*VG_capacidade_maternal, data = dadosDOP)
summary(modcustodiaMI5) #n�o

modcustodiaMI6<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_indice_conversao+Idadeent_meses*VG_cap_crescimento, data = dadosDOP)
summary(modcustodiaMI6) #n�o

modcustodiaMI7<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_indice_conversao+Idadeent_meses*VG_indice_conversao, data = dadosDOP)
summary(modcustodiaMI7) #n�o

modcustodiaMI8<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_indice_conversao+VG_capacidade_maternal*VG_cap_crescimento, data = dadosDOP)
summary(modcustodiaMI8) #n�o

modcustodiaMI9<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_indice_conversao+VG_capacidade_maternal*VG_indice_conversao, data = dadosDOP)
summary(modcustodiaMI9) #n�o

modcustodiaMI10<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_indice_conversao+VG_cap_crescimento*VG_indice_conversao, data = dadosDOP)
summary(modcustodiaMI10) #n�o

#modelo sem intera��o selecionado modcustodiaM3
modcustodiaM3<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento, data = dadosDOP)
summary(modcustodiaM3)


#Analise dos res�duos
resid<-resid(modcustodiaM3)
pred<-fitted(modcustodiaM3)
resid.std <- rstandard(modcustodiaM3)

lillie.test(resid) #nao admite normalidade
shapiro.test(resid)
qqnorm(resid)
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
durbinWatsonTest(modcustodiaM3)
#N�o possui independencia

# Breusch- Pagan test
bptest(modcustodiaM3) 
#apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaM3) # variance inflation factors 
sqrt(vif(modcustodiaM3)) > 2 # Se verdade existe multicolinearidade

###########VERIFICA��O DOS VALORES INFLUENTES#########################

# vamos fazer um teste T para ver quais as observacoes sao consideradas outliers
pvalue<-2*(1-pt(abs(resid.std),length(resid.std)))
# Devolve os outliers ao nivel de 1%
names(pvalue)<-1:length(resid.std)
pvalue[pvalue<0.01]
#ser� retirado os animais que apresentaram-se outliers 
dadosDOP2<-dadosDOP[-c(4,295,374,375,377),]
modcustodiaM32<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento, data = dadosDOP2)
summary(modcustodiaM32) #R� = 69%
AIC(modcustodiaM32)
anova(modcustodiaM32)

#Analise dos res�duos do modelo sem outliers
resid<-resid(modcustodiaM32)
pred<-fitted(modcustodiaM32)
resid.std <- rstandard(modcustodiaM32)

lillie.test(resid) #admite a normalidade
shapiro.test(resid)
qqnorm(resid)
qqline(resid)#pontos outliers
# Histograma dos residuos
hist(resid)  #n�o aparentam ter distr Normal
# Avaliar Achatamento


##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustodiaM32)
#N�o possui independencia

# Breusch- Pagan test
bptest(modcustodiaM32) 
bptest(modcustodiaM32, varformula = ~ fitted.values(modcustodiaM32), studentize = TRUE)
#apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaM32) # variance inflation factors 
sqrt(vif(modcustodiaM32)) > 2 # Se verdade existe multicolinearidade

#o modelo atende normalidade, homocedasticidade e multicolinearidade
par(mfrow = c(2,2))
plot(modcustodiaM32)
