#####################################################################
###########################Modelo GLM################################
#####################################################################
#O objetivou foi verificar se o modelo obtido pelo RLM, com outra
#fun��o de distribui��o e outro link, apresentaria resultado melhor
#Segundo Agresti ajustar um modelo de regress�o cl�ssica � o mesmo que 
#ajustar um GLM usando a normal (Gaussian family) e as componentes alet�rias
#com a fun��o de liga��o identidade (link = identity)

#MODELO NORMAL E LINK IDENTIDADE
setwd()

library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=T, sep=";", dec=",")

library(fBasics)
library(mfp)
library(car)
library(rms)

#Modelo nulo
fit0 <- glm(dados$custototal.dia ~ 1,        # Y ~ lista de variaveis explicativas. ~1 indicia que so se considera a constante (modelo nulo) 
            data=dados,                     # nome do conjunto de dados
            family=gaussian(link = "identity"))    # distribuicao de Y e funcao de ligacao
# -- coeficientes do modelo e sua significancia
summary(fit0)   

# modelo obtido foi
MULTGAUS<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=gaussian(link = "identity"), na.action = na.exclude)
summary(MULTGAUS) 
anova(MULTGAUS)
#Os pressupostos da linearidade das covari�veis cont�nuas foram
#testados pelo m�todo dos quartis, o m�todo de Lowess e os
#Polinomios fracion�rios.
#o resultado est� no codigo geral e n�o quis colocar aqui para 
#n�o ficar muita coisa, mas todas passaram na linearidade.


vif(MULTGAUS) #Pode-se observar ausencia de multicolinearidade

#R�
#varia��o explicada do modelo com resposta continua R�
1-((deviance(MULTGAUS)/(df.residual(MULTGAUS))))/(MULTGAUS$null.deviance/MULTGAUS$df.null)

BIC(MULTGAUS)

#TESTE DA BONDADE DO AJUSTAMENTO
## Teste de Cessie-van Houwelingen ##
fit1 <- lrm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados, x=TRUE, y=TRUE)
resid(fit1, 'gof')
print(fit1)
#OBS: eu peguei esse teste do material de categorica.
#Mas pelo o que li, n�o sei se � o teste mais indicado para verifica��o
#da bondade de ajustamento de Y continuo


#Comparando com modelo nulo pela verossimilhan�a
fit0a <- glm(custototal.dia ~ 1,  
             data=MULTGAUS$model,           
             family=gaussian(link=identity))    
summary(fit0a)
anova(fit0a, MULTGAUS, test="Chisq")

##..........................................##
##### Analise de residuos via individuos #####

## residuos deviance ##
rd<-residuals (MULTGAUS, type="deviance") # residuos deviance
plot(fitted(MULTGAUS), rd, xlab= "Probabilidades estimadas", ylab="Residuos Deviance") 

# identificar as possiveis observacoes influentes
temp<-influence.measures(MULTGAUS)
(lista <- which(apply(temp$is.inf, 1, any)))	 # lista as candidatas a observacoes influentes
summary(temp) 
#Pode-se verificar que os 15 individuos que foram retirados do modelo
#RLM por serem outliers, apresentaram-se como outliers tamb�m no GLM.
#Entretanto, tamb�m foram acrescidos os animais: 273, 274, 325, 331,
#336, 351, 361, 362, 368, 403, 404, 446, 447, 459, 489, 550, 558, 612,
#621, 626, 635, 703 e 711.

## distancia de Cook ##
plot(cooks.distance(MULTGAUS), xlab="Individuos", ylab="Distancia de Cook")
identify(cooks.distance(MULTGAUS), labels=rownames(dados)) # identificar os pontos que se destacam
#Nenhum animal ultrapassa o valor indicado

#adequabilidade da fun��o de liga��o gaussiana e link identidade
mu <-predict(MULTGAUS, type="response")
z <-predict(MULTGAUS)+(dados$custototal.dia-mu)
plot(z~predict(MULTGAUS,type="link"), 
     main = "Adequabilidade da fun��o gaussiana e link identidade", xlab=expression(hat(eta)), dados, ylab="Resposta linearizada")
#Se ajusta a uma recta, mas est� disperso

#avalia��o variancia
ra<-resid(MULTGAUS,type="response")
tr<-2*log(predict(MULTGAUS,type="response"))
plot(ra ~ tr, xlab=expression(2*log(hat(mu))),ylab="Res�duos Absolutos")
lines (lowess (ra~tr), col="red")
# n�o deve apresentar uma tend�ncia

#Os resultados s�o semelhantes ao obtido no modelo geral sem 
#retirar os outliers

#########################################################################
#########################################################################
##########################################################################
######################################################################
######################################################################
#GAUSSIANA INVERSA
#LINK inverse
#LIGA��O CANONICA

#Modelo nulo
fit0 <- glm(dados$custototal.dia ~ 1,        # Y ~ lista de variaveis explicativas. ~1 indicia que so se considera a constante (modelo nulo) 
            data=dados,                     # nome do conjunto de dados
            family=inverse.gaussian(link = "inverse"))    # distribuicao de Y e funcao de ligacao
# -- coeficientes do modelo e sua significancia
summary(fit0)     


#modelo
MULTGINV<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=inverse.gaussian(link = "inverse"), na.action = na.exclude)
summary(MULTGINV)


vif(MULTGINV)

#R�
#varia��o explicada do modelo com resposta continua R�
1-((deviance(MULTGINV)/(df.residual(MULTGINV))))/(MULTGINV$null.deviance/MULTGINV$df.null)


BIC(MULTGINV)

#TESTES DA BONDADE DO AJUSTAMENTO
fit1 <- lrm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados, x=TRUE, y=TRUE)
resid(fit1, 'gof')
print(fit1)

#Comparando com modelo nulo pela verossimilhan�a
fit0 <- glm(custototal.dia ~ 1,        
            data=MULTGINV$model,           
            family=inverse.gaussian(link=inverse))    
summary(fit0)
anova(fit0, MULTGINV, test="Chisq")

##..........................................##
##### Analise de residuos via individuos #####

## residuos deviance ##
rd<-residuals (MULTGINV, type="deviance") # residuos deviance
plot(fitted(MULTGINV), rd, xlab= "Probabilidades estimadas", ylab="Residuos Deviance") 

## distancia de Cook ##
plot(cooks.distance(MULTGINV), xlab="Individuos", ylab="Distancia de Cook")
identify(cooks.distance(MULTGINV), labels=rownames(dados)) # identificar os pontos que se destacam


####################################################################
#adequabilidade da fun��o de liga��o inverse gaussian e link inversa
mu <-predict(MULTGINV, type="response")
z <-predict(MULTGINV)-2*(dados$custototal.dia-mu)/mu^3
plot(z~predict(MULTGINV,type="link"), xlab=expression(hat(eta)), dados, ylab="Resposta linearizada",
     main = "Adequabilidade da fun��o gaussiana inversa e liga��o inversa")
# n�o se ajustou bem a uma reta, muitos individuos dispersos


#avalia��o variancia
ra<-resid(MULTGINV,type="response")
tr<-2*log(predict(MULTGINV,type="response"))
plot(ra ~ tr, xlab=expression(2*log(hat(mu))),ylab="Res�duos Absolutos")
lines (lowess (ra~tr), col="red")
# 

#################################################################
####################################################################
######################################################################
#####################################################################
#GAMMA
#LINK inverse
#LIGA��O CANONICA


#Modelo nulo
fit0 <- glm(dados$custototal.dia ~ 1,        # Y ~ lista de variaveis explicativas. ~1 indicia que so se considera a constante (modelo nulo) 
            data=dados,                     # nome do conjunto de dados
            family=Gamma(link="inverse"))    # distribuicao de Y e funcao de ligacao
# -- coeficientes do modelo e sua significancia
summary(fit0)   



#modelo
MULTGAMMAINV<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=Gamma(link="inverse"), na.action = na.exclude)
summary(MULTGAMMAINV) 


vif(MULTGAMMAINV)     #N�o apresenta problemas de multicolinearidade

#R�
#varia��o explicada do modelo com resposta continua R�
1-((deviance(MULTGAMMAINV)/(df.residual(MULTGAMMAINV))))/(MULTGAMMAINV$null.deviance/MULTGAMMAINV$df.null)


BIC(MULTGAMMAINV)

#Bondade ajustamento
fit1 <- lrm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados, x=TRUE, y=TRUE)
resid(fit1, 'gof')
print(fit1)

#Comparando com modelo nulo pela verossimilhan�a
fit0a <- glm(custototal.dia ~ 1,        
             data=MULTGAMMAINV$model,           
             family=Gamma(link="inverse"))    
summary(fit0a)
anova(fit0a, MULTGAMMAINV, test="Chisq")



#adequabilidade da fun��o de liga��o GAMMA e link inverso
mu <-predict(MULTGAMMAINV, type="response")
z <-predict(MULTGAMMAINV)-(dados$custototal.dia-mu)/(mu^2)
plot(z~predict(MULTGAMMAINV,type="link"), xlab=expression(hat(eta)), dados, ylab="Resposta linearizada",
     main = "Adequabilidade da fun��o Gamma e liga��o inversa")
#Ficou parecido com o modelo da Gaussiana Inversa

#avalia��o variancia
ra<-resid(MULTGAMMAINV,type="response")
tr<-2*log(predict(MULTGAMMAINV,type="response"))
plot(ra ~ tr, xlab=expression(2*log(hat(mu))),ylab="Res�duos Absolutos")
lines (lowess (ra~tr), col="red")


###################################################################
#####################################################################
###################################################################
####################################################################
##########################################################################3
##GAMMA
#LINK log


#Modelo nulo
fit0 <- glm(dados$custototal.dia ~ 1,        # Y ~ lista de variaveis explicativas. ~1 indicia que so se considera a constante (modelo nulo) 
            data=dados,                     # nome do conjunto de dados
            family=Gamma(link=log))    # distribuicao de Y e funcao de ligacao
# -- coeficientes do modelo e sua significancia
summary(fit0)        


#modelo
MULTGAMMALOG<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=Gamma(link=log), na.action = na.exclude)
summary(MULTGAMMALOG) 


vif(MULTGAMMALOG)


#R�
#varia��o explicada do modelo com resposta continua R�
1-((deviance(MULTGAMMALOG)/(df.residual(MULTGAMMALOG))))/(MULTGAMMALOG$null.deviance/MULTGAMMALOG$df.null)

BIC(MULTGAMMALOG)


#TESTES DA BONDADE DO AJUSTAMENTO
fit1 <- lrm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados, x=TRUE, y=TRUE)
resid(fit1, 'gof')
print(fit1)


#Comparando com modelo nulo pela verossimilhan�a
fit0 <- glm(custototal.dia ~ 1,        
            data=MULTGAMMALOG$model,           
            family=Gamma(link=log))    
summary(fit0)
anova(fit0, MULTGAMMALOG, test="Chisq")



#adequabilidade da fun��o de liga��o Gamma e link log
mu <-predict(MULTGAMMALOG, type="response")
z <-predict(MULTGAMMALOG)+(dados$custototal.dia-mu)/mu
plot(z~predict(MULTGAMMALOG,type="link"), xlab=expression(hat(eta)), dados, ylab="Resposta linearizada",
     main = "Adequabilidade da fun��o gama e liga��o log")
#existe uma tendencia, mas tamb�m me parece disperso

#avalia��o variancia
ra<-resid(MULTGAMMALOG,type="response")
tr<-2*log(predict(MULTGAMMALOG,type="response"))
plot(ra ~ tr, xlab=expression(2*log(hat(mu))),ylab="Res�duos Absolutos")
lines (lowess (ra~tr), col="red")
# n�o deve apresentar uma tend�ncia


######################################################################
####################################################################
###################################################################
##GAMMA
#LINK identidade


#Modelo nulo
fit0 <- glm(dados$custototal.dia ~ 1,        # Y ~ lista de variaveis explicativas. ~1 indicia que so se considera a constante (modelo nulo) 
            data=dados,                     # nome do conjunto de dados
            family=Gamma(link="identity"))    # distribuicao de Y e funcao de ligacao
# -- coeficientes do modelo e sua significancia
summary(fit0)        


MULTGAMMAIDENT<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=Gamma(link="identity"), na.action = na.exclude)
summary(MULTGAMMAIDENT)


vif(MULTGAMMAIDENT)

#R�
#varia��o explicada do modelo com resposta continua R�
1-((deviance(MULTGAMMAIDENT)/(df.residual(MULTGAMMAIDENT))))/(MULTGAMMAIDENT$null.deviance/MULTGAMMAIDENT$df.null)


BIC(MULTGAMMAIDENT)

#TESTES DA BONDADE DO AJUSTAMENTO
fit1 <- lrm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados, x=TRUE, y=TRUE)
resid(fit1, 'gof')
print(fit1)


#Comparando com modelo nulo pela verossimilhan�a
fit0 <- glm(custototal.dia ~ 1,        
            data=MULTGAMMAIDENT$model,           
            family=Gamma(link=identity))    
summary(fit0)
anova(fit0, MULTGAMMAIDENT, test="Chisq")



#adequabilidade da fun��o de liga��o Gamma e link identity
mu <-predict(MULTGAMMAIDENT, type="response")
z <-predict(MULTGAMMAIDENT)+(dados$custototal.dia-mu)
plot(z~predict(MULTGAMMAIDENT,type="link"), xlab=expression(hat(eta)), dados, ylab="Resposta linearizada",
     main = "Adequabilidade da fun��o Gamma e liga��o identidade")
# se ajustou a uma reta

#avalia��o variancia
ra<-resid(MULTGAMMAIDENT,type="response")
tr<-2*log(predict(MULTGAMMAIDENT,type="response"))
plot(ra ~ tr, xlab=expression(2*log(hat(mu))),ylab="Res�duos Absolutos")
lines (lowess (ra~tr), col="red")
# n�o deve apresentar uma tend�ncia

#Conforme os professores podem verificar na tabela que enviei
#Embora O deviance da fun��o Gaussiana esteja muito alto
#Os melhores valores de AIC, BIC e R� s�o desse tipo de distribui��o
#Portanto, penso que os dados se ajustam melhor a distribui��o gaussiana  
#com Link identidade.
#Seguido da Gamma com link identidade.