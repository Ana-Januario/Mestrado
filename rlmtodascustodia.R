#Modelo geral TODAS AS VARI�VEIS

getwd()

library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=T, sep=";", dec=",")


#no modelo simples, das variaveis:peso a entrada, idade a entrada, P210, GMD, 
#VG capacidade maternal, VG capacidade de crescimento, VG GMD esta��o, 
#VG carca�a por dia de idade,VG intervalo entre partos, VG indice de convers�o, 
#VG longevidade produtiva, VG consumo alimentar residual e destino de abate
#apenas essas foram compor o modelo multivariado

modcustodiaM3<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_capacidade_maternal+Dest_abate,  data = dados)
summary(modcustodiaM3)

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
hist(resid, main = "Histograma dos res�duos do modelo geral 
                      com todos os dados", ylab="Frequencia", xlab="Res�duos")  #parentam ter distr Normal
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


#O modelo escolhido �
modcustodiaM3<-lm(custototal.dia~peso_ent+Idadeent_meses+GMD+VG_capacidade_maternal+Dest_abate,  data = dados)
summary(modcustodiaM3)
#atende simetria, homocedasticidade e multicolinearidade




###################################################################################3
####################################################################################
####################################################################################
######################################################################################
##########################################################################################


#Modelo geral caracter�sticas de entrada
modcustodiaENT<-lm(custototal.dia~peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade,  data = dados)
summary(modcustodiaENT)
anova(modcustodiaENT)

par(mfrow = c(2,2))
plot(modcustodiaENT)

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
anscombe.test(resid) 
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

lev<-hatvalues(modcustodiaENT)
lev
#como s�o muitos valores influentes
names(lev)<-1:length(lev)
plot(pred,lev,abline(h=0.2))
plot(pred,lev,ylim=c(0,0.2),abline(h=0.2))

#nenhum ultrapassa o limite indicado
dcook<-cooks.distance(modcustodiaENT)
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

par(mfrow = c(2,2))
plot(modcustodiaENTB2)
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
anscombe.test(resid) #atende curtose
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
#apresenta homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaENTB2) # variance inflation factors 
sqrt(vif(modcustodiaENTB2)) > 2 # Se verdade existe multicolinearidade


#modelo selecionado para dados a entrada modcustodiaENTB2
#Mesocurtico, homocedastico e ausencia de multicolinearidade
#






resettest(modcustodiaENTB2, power = 2:2, type = "regressor", data = dados2)
#Aparentemente a forma funcional n�o est� correta
#Em termos gerais podemos ter diversas possibilidades de
#n�o linearidades no contexto da regress�o linear
#Uma explica��o � que a forma funcional pode estar incorreta (Verbek 2017)
#H0 forma funcional correta
#teste para autocorrela��o com regressores ex�genos: Teste t e durbin watson
#Com regressores ex�genos ou end�genos: teste de breusch godfrey
bgtest (modcustodiaENTB2, order = 1, order.by = NULL, type = c ("Chisq"), data = dados2)
#LM teste
#(n-1)*R�
#698*0.5193 = 362,47
#tabela qui quadrado, valor calculado maior que o observado
#Tamb�m n�o passa no teste de breusch godfrey

cov(dados2$peso_ent, dados2$custototal.dia, method = "pearson")
cov(dados2$VG_capacidade_maternal, dados2$custototal.dia, method = "pearson")
cov(dados2$VG_cap_crescimento, dados2$custototal.dia, method = "pearson")
cov(dados2$VG_carcaca_dia_idade, dados2$custototal.dia, method = "pearson")
#O peso e a carca�a por dia de dade apresentam as maiores COV