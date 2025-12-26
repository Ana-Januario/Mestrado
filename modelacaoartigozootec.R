#-----------------------------------------------
###################Modelação####################
#-----------------------------------------------

getwd()

library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=T, sep=";", dec=",")
library(fBasics)

#Modelo nulo
fit0 <- glm(dados$custototal.dia ~ 1,        # Y ~ lista de variaveis explicativas. ~1 indicia que so se considera a constante (modelo nulo) 
            data=dados,                     # nome do conjunto de dados
            family=Gamma(link="inverse"))    # distribuicao de Y e funcao de ligacao
# -- coeficientes do modelo e sua significancia
summary(fit0)        


#Idade a entrada
fit1 <- glm(dados$custototal.dia ~ dados$Idadeent_meses, 
            data=dados,family=Gamma(link="inverse"))
summary(fit1) 
anova(fit1, test="Chisq") 
exp(fit1$coef[2])


#Peso a entrada
fit2 <- glm(dados$custototal.dia ~ dados$peso_ent, 
            data=dados,family=Gamma(link="inverse"))
summary(fit2) 
anova(fit2, test="Chisq") 
exp(fit2$coef[2])

#VG Capacidade maternal
fit3 <- glm(dados$custototal.dia ~ dados$VG_capacidade_maternal, 
            data=dados,family=Gamma(link="inverse"))
summary(fit3) 
anova(fit3, test="Chisq") 
exp(fit3$coef[2])

#VG capacidade de crescimento
fit4 <- glm(dados$custototal.dia ~ dados$VG_cap_crescimento, 
            data=dados,family=Gamma(link="inverse"))
summary(fit4) 
anova(fit4, test="Chisq") 
exp(fit4$coef[2])


#VG_gmd_estacao
fit5 <- glm(dados$custototal.dia ~ dados$VG_gmd_estacao, 
            data=dados,family=Gamma(link="inverse"))
summary(fit5) 
anova(fit5, test="Chisq") 

#VG_carcaca_dia_idade
fit6 <- glm(dados$custototal.dia ~ dados$VG_carcaca_dia_idade, 
            data=dados,family=Gamma(link="inverse"))
summary(fit6) 
anova(fit6, test="Chisq") 

#VG_Intervalo_Entre_partos
fit7 <- glm(dados$custototal.dia ~ dados$VG_Intervalo_Entre_partos, 
            data=dados,family=Gamma(link="inverse"))
summary(fit7)
anova(fit7, test="Chisq") 


#VG_indice_conversao
fit8 <- glm(dados$custototal.dia ~ dados$VG_indice_conversao, 
            data=dados,family=Gamma(link="inverse"))
summary(fit8) 
anova(fit8, test="Chisq") 


#VG_longevidade_produtiva
fit9 <- glm(dados$custototal.dia ~ dados$VG_longevidade_produtiva, 
            data=dados,family=Gamma(link="inverse"))
summary(fit9) 
anova(fit9, test="Chisq") 


#VG_consumo_alim_residual
fit10 <- glm(dados$custototal.dia ~ dados$VG_consumo_alim_residual, 
            data=dados,family=Gamma(link="inverse"))
summary(fit10)
anova(fit10, test="Chisq")


#############GLM gama

MULTGAMMAINV<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=Gamma(link="inverse"), na.action = na.exclude)
summary(MULTGAMMAINV) 
#De acordo com o p-valor retira 
#VG_gmd_estacao
#VG_consumo_alim_residual
#Idadeent_meses
require(MASS)
gamma.shape(MULTGAMMAINV)

# ------ avaliar pressuposto de linearidade (para covariaveis continuas) -----

# -- Metodo dos quartis
Qis <- as.numeric(quantile(dados$peso_ent, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$pesoCAT<- cut(dados$peso_ent,            # variavel a categorizar
                    breaks=Qis,            # pontos de corte das classes (nos quartis)
                    right=FALSE,           # classes abertas a direita
                    include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$pesoCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMAINV1a <-glm(custototal.dia~pesoCAT+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=Gamma("inverse"),
             data=dados)
# coeficientes estimados
summary(MULTGAMMAINV1a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMAINV1a$coef[2:4]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de peso categorizada gama e link inversa")  # titulo do grafico
lines(lowess(x,y))                            # adiciona uma linha ao grafico a unir os pontos
#para peso tem aspecto linear

###---------- M�todo lowess ----------- ###
#
plot(lowess(predict(MULTGAMMAINV)~dados$peso_ent), type="l", main="Linearidade pelo m�todo de Lowess",xlab="Peso � entrada" ,ylab="logOdds")
##Gr�fico linear

#______VG_capacidade_maternal

Qis <- as.numeric(quantile(dados$VG_capacidade_maternal, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$capmaternoCAT<- cut(dados$VG_capacidade_maternal,            # variavel a categorizar
                          breaks=Qis,            # pontos de corte das classes (nos quartis)
                          right=FALSE,           # classes abertas a direita
                          include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$capmaternoCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMAINV2a <-glm(custototal.dia~peso_ent+capmaternoCAT+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=Gamma("inverse"),
             data=dados)
summary(MULTGAMMAINV2a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMAINV2a$coef[3:5]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de capacidade maternal categorizada")  # titulo do grafico
lines(lowess(x,y))

###---------- M�todo lowess ----------- ###
#
plot(lowess(predict(MULTGAMMAINV)~dados$VG_capacidade_maternal), type="l", main="Linearidade pelo m�todo de Lowess",xlab="Valor gen�tico da capacidade maternal" ,ylab="logOdds")
#Sem aspecto linear

# -- Metodo dos polinomios fraccionarios 
library(mfp)
MULTGAMMAINV2p <-mfp(custototal.dia~peso_ent+fp(VG_capacidade_maternal)+
               VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao,
             family = Gamma(link = "inverse"), 
             data=dados)
print(MULTGAMMAINV2p)
summary(MULTGAMMAINV2p)
#N�o sugere transforma��o para a vari�vel considera linear



#__________VG_cap_crescimento

Qis <- as.numeric(quantile(dados$VG_cap_crescimento, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$capcresciCAT<- cut(dados$VG_cap_crescimento,            # variavel a categorizar
                          breaks=Qis,            # pontos de corte das classes (nos quartis)
                          right=FALSE,           # classes abertas a direita
                          include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$capcresciCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMAINV3a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+capcresciCAT+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=Gamma("inverse"),
             data=dados)
summary(MULTGAMMAINV3a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMAINV3a$coef[4:6]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de capacidade de crescimento categorizada")  # titulo do grafico
lines(lowess(x,y))
# aspecto linear
###---------- M�todo lowess ----------- ###
#
plot(lowess(predict(MULTGAMMAINV)~dados$VG_cap_crescimento), type="l", main="Linearidade pelo m�todo de Lowess",xlab="Valor gen�tico da capacidade de crescimento" ,ylab="logOdds")

# -- Metodo dos polinomios fraccionarios 

MULTGAMMAINV3p <-mfp(custototal.dia~peso_ent+VG_capacidade_maternal+
               fp(VG_cap_crescimento)+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos,
             family = Gamma(link = inverse), 
             data=dados)
print(MULTGAMMAINV3p)
summary(MULTGAMMAINV3p)
#n�o sugere transforma��o considero linear


#__________________VG_carcaca_dia_idade
Qis <- as.numeric(quantile(dados$VG_carcaca_dia_idade, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$carcporidadeCAT<- cut(dados$VG_carcaca_dia_idade,            # variavel a categorizar
                         breaks=Qis,            # pontos de corte das classes (nos quartis)
                         right=FALSE,           # classes abertas a direita
                         include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$carcporidadeCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMAINV4a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+carcporidadeCAT+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=Gamma("inverse"),
             data=dados)
summary(MULTGAMMAINV4a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMAINV4a$coef[5:7]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade VG carca�a por dia de idade categorizada")  # titulo do grafico
lines(lowess(x,y))
# Sem aspecto linear

plot(lowess(predict(MULTGAMMAINV)~dados$VG_carcaca_dia_idade), type="l", main="Linearidade pelo m�todo de Lowess",xlab="Valor gen�tico carca�a por dia de idade" ,ylab="logOdds")
#apresenta aspecto linear



#__________________VG_Intervalo_Entre_partos
Qis <- as.numeric(quantile(dados$VG_Intervalo_Entre_partos, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$intentrepartosCAT<- cut(dados$VG_Intervalo_Entre_partos,            # variavel a categorizar
                            breaks=Qis,            # pontos de corte das classes (nos quartis)
                            right=FALSE,           # classes abertas a direita
                            include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$intentrepartosCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMAINV5a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+intentrepartosCAT, 
             na.action = na.exclude, 
             family=Gamma("inverse"),
             data=dados)
summary(MULTGAMMAINV5a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMAINV5a$coef[6:8]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade VG Intervalo entre partos categorizada")  # titulo do grafico
lines(lowess(x,y))
#aspecto linear





###################                             ###################
########-------------------------------------------------------------------########


######---------------------Multicolinariedade---------------------######

library(car)
vif(MULTGAMMAINV)     #N�o apresenta problemas de multicolinearidade
   


(n<-length(dados$custototal.dia)) 
(R2N<-(1-exp((MULTGAMMAINV$dev-MULTGAMMAINV$null)/n))/(1-exp(-MULTGAMMAINV$null/n)))  #R� = 0,4428

#varia��o explicada do modelo com resposta continua R�
1-((deviance(MULTGAMMAINV)/(df.residual(MULTGAMMAINV))))/(MULTGAMMAINV$null.deviance/MULTGAMMAINV$df.null)
#A respota do RLM � melhor

BIC(MULTGAMMAINV)



###################Bondade do ajustamento e capacidade discriminativa###################
########-----------------------------------------------------------------------------------########
## N�O USA PARA VARI�VEIS Y CONTINUA Teste de Hosmer e Lemeshow  
#(fithl<-hoslem.test(dados$custototal.dia, fitted(MULT), g = 10))
#fithl$expected
#p-valor = 1... Est� estranho

## Teste de Cessie-van Houwelingen ##
library(rms)
fit1 <- lrm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados, x=TRUE, y=TRUE)
resid(fit1, 'gof')
print(fit1)
# Brier=0,17

#Comparando com modelo nulo pela verossimilhan�a
fit0a <- glm(custototal.dia ~ 1,        
             data=MULTGAMMAINV$model,           
             family=Gamma(link="inverse"))    
summary(fit0a)
anova(fit0a, MULTGAMMAINV, test="Chisq")



#adequabilidade da fun��o de liga��o GAMMA e link inverso
mu <-predict(MULTGAMMAINV, type="response")
z <-predict(MULTGAMMAINV)-(dados$custototal.dia-mu)/(mu^2)
plot(z~predict(MULTGAMMAINV,type="link"), xlab=expression(hat(eta)), dados,main = 'Adequabilidade da fun��o gama e liga��o inversa', ylab="Resposta linearizada")
# se se ajustar a uma recta, est� ok

#gaussiana inversa
#family=inverse.gaussian(link = "identity")

#avalia��o variancia
ra<-resid(MULTGAMMAINV,type="response")
tr<-2*log(predict(MULTGAMMAINV,type="response"))
plot(ra ~ tr, xlab=expression(2*log(hat(mu))),ylab="Res�duos Absolutos")
lines (lowess (ra~tr), col="red")
# n�o deve apresentar uma tend�ncia





##..........................................##
##### Analise de residuos via individuos #####
# Nota: quando existem variaveis continuas no modelo quase sempre havera quase tantos padroes como covariaveis e esta abordagem pode ser usada

## residuos deviance ##
rd<-residuals (MULTGAMMAINV, type="deviance") # residuos deviance
plot(fitted(MULTGAMMAINV), rd, xlab= "Probabilidades estimadas", ylab="Residuos Deviance") 

## distancia de Cook ##
plot(cooks.distance(MULTGAMMAINV), xlab="Individuos", ylab="Distancia de Cook")
identify(cooks.distance(MULTGAMMAINV), labels=rownames(dados)) # identificar os pontos que se destacam
halfnorm(cooks.distance(MULTGAMMAINV)) # grafico seminormal 
# Nota: nenhum valor com uma distancia de Cook>0.5
#os animais que mais se destoam tamb�m apareceram no RLM 2, 268, 338, 348

## residuos DfBeta ##
rdf<-dfbetas(MULTGAMMAINV) # residuos dfbeta
# Representação dos dfbeta vs preditores #
plot(dados$peso_ent, rdf[, 2], xlab="Peso a entrada", ylab="Dfbeta")
plot(dados$VG_capacidade_maternal, rdf[, 3], xlab="VG capacidade maternal", ylab="Dfbeta")
plot(dados$VG_cap_crescimento, rdf[, 4], xlab="VG capacidade de crescimento", ylab="Dfbeta")
plot(dados$VG_carcaca_dia_idade, rdf[, 5], xlab="VG caraca�a por dia de idade", ylab="Dfbeta")
plot(dados$VG_Intervalo_Entre_partos, rdf[, 6], xlab="VG intervalo entre partos", ylab="Dfbeta")
plot(dados$VG_indice_conversao, rdf[, 7], xlab="VG indice de convers�o alimentar", ylab="Dfbeta")

# Nota: nenhum valor com um residuo excessivamente elevado (>1) ou que se destaque

# Representa��o dos dfbeta vs obs #
dfbetasPlots (MULTGAMMAINV)

#Leverage
h<-influence (MULTGAMMAINV)
plot(h$hat) 
# Nota: apesar de existirem muitos pontos acima do limite n�o ha nenhum excessivamente elevado nem que se destaque 

#############################################################################################







##########################################################################3
###############GAMMA COM LINK LOG##################################



#Modelo nulo
fit0 <- glm(dados$custototal.dia ~ 1,        # Y ~ lista de variaveis explicativas. ~1 indicia que so se considera a constante (modelo nulo) 
            data=dados,                     # nome do conjunto de dados
            family=Gamma(link=log))    # distribuicao de Y e funcao de ligacao
# -- coeficientes do modelo e sua significancia
summary(fit0)        


#Idade a entrada
fit1 <- glm(dados$custototal.dia ~ dados$Idadeent_meses, 
            data=dados,family=Gamma(link=log))
summary(fit1) 
anova(fit1, test="Chisq") 
exp(fit1$coef[2])


#Peso a entrada
fit2 <- glm(dados$custototal.dia ~ dados$peso_ent, 
            data=dados,family=Gamma(link=log))
summary(fit2) 
anova(fit2, test="Chisq") 
exp(fit2$coef[2])

#VG Capacidade maternal
fit3 <- glm(dados$custototal.dia ~ dados$VG_capacidade_maternal, 
            data=dados,family=Gamma(link=log))
summary(fit3) 
anova(fit3, test="Chisq") 
exp(fit3$coef[2])

#VG capacidade de crescimento
fit4 <- glm(dados$custototal.dia ~ dados$VG_cap_crescimento, 
            data=dados,family=Gamma(link=log))
summary(fit4) 
anova(fit4, test="Chisq") 
exp(fit4$coef[2])


#VG_gmd_estacao
fit5 <- glm(dados$custototal.dia ~ dados$VG_gmd_estacao, 
            data=dados,family=Gamma(link=log))
summary(fit5) 
anova(fit5, test="Chisq") 

#VG_carcaca_dia_idade
fit6 <- glm(dados$custototal.dia ~ dados$VG_carcaca_dia_idade, 
            data=dados,family=Gamma(link=log))
summary(fit6) 
anova(fit6, test="Chisq") 

#VG_Intervalo_Entre_partos
fit7 <- glm(dados$custototal.dia ~ dados$VG_Intervalo_Entre_partos, 
            data=dados,family=Gamma(link=log))
summary(fit7)
anova(fit7, test="Chisq") 


#VG_indice_conversao
fit8 <- glm(dados$custototal.dia ~ dados$VG_indice_conversao, 
            data=dados,family=Gamma(link=log))
summary(fit8) 
anova(fit8, test="Chisq") 


#VG_longevidade_produtiva
fit9 <- glm(dados$custototal.dia ~ dados$VG_longevidade_produtiva, 
            data=dados,family=Gamma(link=log))
summary(fit9) 
anova(fit9, test="Chisq") 


#VG_consumo_alim_residual
fit10 <- glm(dados$custototal.dia ~ dados$VG_consumo_alim_residual, 
             data=dados,family=Gamma(link=log))
summary(fit10)
anova(fit10, test="Chisq")


################MULTIVARIADO

MULTGAMMALOG<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=Gamma(link=log), na.action = na.exclude)
summary(MULTGAMMALOG) 
gamma.shape(MULTGAMMALOG)

# ------ avaliar pressuposto de linearidade (para covariaveis continuas) -----

# -- Metodo dos quartis
Qis <- as.numeric(quantile(dados$peso_ent, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$pesoCAT<- cut(dados$peso_ent,            # variavel a categorizar
                    breaks=Qis,            # pontos de corte das classes (nos quartis)
                    right=FALSE,           # classes abertas a direita
                    include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$pesoCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMALOG1a <-glm(custototal.dia~pesoCAT+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
                     na.action = na.exclude, 
                     family=Gamma(log),
                     data=dados)
# coeficientes estimados
summary(MULTGAMMALOG1a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMALOG1a$coef[2:4]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de peso categorizada gama link log")  # titulo do grafico
lines(lowess(x,y))                            # adiciona uma linha ao grafico a unir os pontos
#para peso n�o tem aspecto linear

###---------- M�todo lowess ----------- ###
#
plot(lowess(predict(MULTGAMMALOG)~dados$peso_ent), type="l", main="Linearidade pelo m�todo de Lowess",xlab="Peso a entrada" ,ylab="logOdds")
##Gr�fico linear

#______VG_capacidade_maternal

Qis <- as.numeric(quantile(dados$VG_capacidade_maternal, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$capmaternoCAT<- cut(dados$VG_capacidade_maternal,            # variavel a categorizar
                          breaks=Qis,            # pontos de corte das classes (nos quartis)
                          right=FALSE,           # classes abertas a direita
                          include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$capmaternoCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMALOG2a <-glm(custototal.dia~peso_ent+capmaternoCAT+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
                     na.action = na.exclude, 
                     family=Gamma(log),
                     data=dados)
summary(MULTGAMMALOG2a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMALOG2a$coef[3:5]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de capacidade maternal categorizada")  # titulo do grafico
lines(lowess(x,y))

###---------- M�todo lowess ----------- ###
#
plot(lowess(predict(MULTGAMMALOG)~dados$VG_capacidade_maternal), type="l", main="Linearidade pelo m�todo de Lowess",xlab="Valor gen�tico da capacidade maternal" ,ylab="logOdds")
#Sem aspecto linear

# -- Metodo dos polinomios fraccionarios 
library(mfp)
MULTGAMMALOG2p <-mfp(custototal.dia~peso_ent+fp(VG_capacidade_maternal)+
                             VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao,
                     family = Gamma(link = log), 
                     data=dados)
print(MULTGAMMALOG2p)
summary(MULTGAMMALOG2p)
#N�o sugere transforma��o para a vari�vel considera linear

#__________VG_cap_crescimento

Qis <- as.numeric(quantile(dados$VG_cap_crescimento, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$capcresciCAT<- cut(dados$VG_cap_crescimento,            # variavel a categorizar
                         breaks=Qis,            # pontos de corte das classes (nos quartis)
                         right=FALSE,           # classes abertas a direita
                         include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$capcresciCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMALOG3a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+capcresciCAT+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
                     na.action = na.exclude, 
                     family=Gamma(log),
                     data=dados)
summary(MULTGAMMALOG3a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMALOG3a$coef[4:6]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de capacidade de crescimento categorizada")  # titulo do grafico
lines(lowess(x,y))
# aspecto linear
###---------- M�todo lowess ----------- ###
#
plot(lowess(predict(MULTGAMMALOG)~dados$VG_cap_crescimento), type="l", main="Linearidade pelo m�todo de Lowess",xlab="Valor gen�tico da capacidade de crescimento" ,ylab="logOdds")

# -- Metodo dos polinomios fraccionarios 

MULTGAMMALOG3p <-mfp(custototal.dia~peso_ent+VG_capacidade_maternal+
                             fp(VG_cap_crescimento)+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos,
                     family = Gamma(link = log), 
                     data=dados)
print(MULTGAMMALOG3p)
summary(MULTGAMMALOG3p)
#n�o sugere transforma��o considero linear


#__________________VG_carcaca_dia_idade
Qis <- as.numeric(quantile(dados$VG_carcaca_dia_idade, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$carcporidadeCAT<- cut(dados$VG_carcaca_dia_idade,            # variavel a categorizar
                            breaks=Qis,            # pontos de corte das classes (nos quartis)
                            right=FALSE,           # classes abertas a direita
                            include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$carcporidadeCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMALOG4a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+carcporidadeCAT+VG_Intervalo_Entre_partos, 
                     na.action = na.exclude, 
                     family=Gamma(log),
                     data=dados)
summary(MULTGAMMALOG4a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMALOG4a$coef[5:7]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade VG carca�a por dia de idade categorizada")  # titulo do grafico
lines(lowess(x,y))
# Sem aspecto linear

plot(lowess(predict(MULTGAMMALOG)~dados$VG_carcaca_dia_idade), type="l", main="Linearidade pelo m�todo de Lowess",xlab="Valor gen�tico carca�a por dia de idade" ,ylab="logOdds")
#apresenta aspecto linear


#__________________VG_Intervalo_Entre_partos
Qis <- as.numeric(quantile(dados$VG_Intervalo_Entre_partos, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$intentrepartosCAT<- cut(dados$VG_Intervalo_Entre_partos,            # variavel a categorizar
                              breaks=Qis,            # pontos de corte das classes (nos quartis)
                              right=FALSE,           # classes abertas a direita
                              include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$intentrepartosCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMALOG5a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+intentrepartosCAT, 
                     na.action = na.exclude, 
                     family=Gamma(log),
                     data=dados)
summary(MULTGAMMALOG5a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMALOG5a$coef[6:8]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade VG Intervalo entre partos categorizada")  # titulo do grafico
lines(lowess(x,y))
#aspecto linear


########-------------------------------------------------------------------########
###################                              ###################
########-------------------------------------------------------------------########


MULTGAMMALOG<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=Gamma(link = log), na.action = na.exclude)
summary(MULTGAMMALOG)


library(car)
vif(MULTGAMMALOG)


#modelo sem intera��o
(n<-length(dados$custototal.dia)) 
(R2N<-(1-exp((MULTGAMMALOG$dev-MULTGAMMALOG$null)/n))/(1-exp(-MULTGAMMALOG$null/n)))  #R� = 0,45

#varia��o explicada do modelo com resposta continua R�
1-((deviance(MULTGAMMALOG)/(df.residual(MULTGAMMALOG))))/(MULTGAMMALOG$null.deviance/MULTGAMMALOG$df.null)
#

BIC(MULTGAMMALOG)

#TESTES DA BONDADE DO AJUSTAMENTO
library(rms)
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
plot(z~predict(MULTGAMMALOG,type="link"), xlab=expression(hat(eta)), dados, ylab="Resposta linearizada")
# se ajustou bem a uma reta

#avalia��o variancia
ra<-resid(MULTGAMMALOG,type="response")
tr<-2*log(predict(MULTGAMMALOG,type="response"))
plot(ra ~ tr, xlab=expression(2*log(hat(mu))),ylab="Res�duos Absolutos")
lines (lowess (ra~tr), col="red")
# n�o deve apresentar uma tend�ncia


########################################################################
########################################################################
#############GAMMA FUN��O DE LIGA��O IDENTIDADE########################



#Modelo nulo
fit0 <- glm(dados$custototal.dia ~ 1,        # Y ~ lista de variaveis explicativas. ~1 indicia que so se considera a constante (modelo nulo) 
            data=dados,                     # nome do conjunto de dados
            family=Gamma(link="identity"))    # distribuicao de Y e funcao de ligacao
# -- coeficientes do modelo e sua significancia
summary(fit0)        


MULTGAMMAIDENT<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=Gamma(link="identity"), na.action = na.exclude)
summary(MULTGAMMAIDENT)


# ------ avaliar pressuposto de linearidade (para covariaveis continuas) -----

# -- Metodo dos quartis
Qis <- as.numeric(quantile(dados$peso_ent, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$pesoCAT<- cut(dados$peso_ent,            # variavel a categorizar
                    breaks=Qis,            # pontos de corte das classes (nos quartis)
                    right=FALSE,           # classes abertas a direita
                    include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$pesoCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMAIDENT1a <-glm(custototal.dia~pesoCAT+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
                     na.action = na.exclude, 
                     family=Gamma(identity),
                     data=dados)
# coeficientes estimados
summary(MULTGAMMAIDENT1a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMAIDENT1a$coef[2:4]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de peso categorizada")  # titulo do grafico
lines(lowess(x,y))                            # adiciona uma linha ao grafico a unir os pontos
#para peso n�o tem aspecto linear

###---------- M�todo lowess ----------- ###
#
plot(lowess(predict(MULTGAMMAIDENT)~dados$peso_ent), type="l", main="Linearidade pelo m�todo de Lowess",xlab="Peso a entrada" ,ylab="logOdds")
##Gr�fico linear

#______VG_capacidade_maternal

Qis <- as.numeric(quantile(dados$VG_capacidade_maternal, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$capmaternoCAT<- cut(dados$VG_capacidade_maternal,            # variavel a categorizar
                          breaks=Qis,            # pontos de corte das classes (nos quartis)
                          right=FALSE,           # classes abertas a direita
                          include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$capmaternoCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMAIDENT2a <-glm(custototal.dia~peso_ent+capmaternoCAT+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
                     na.action = na.exclude, 
                     family=Gamma(identity),
                     data=dados)
summary(MULTGAMMAIDENT2a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMAIDENT2a$coef[3:5]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de capacidade maternal categorizada")  # titulo do grafico
lines(lowess(x,y))

###---------- M�todo lowess ----------- ###
#
plot(lowess(predict(MULTGAMMAIDENT)~dados$VG_capacidade_maternal), type="l", main="Linearidade pelo m�todo de Lowess",xlab="Valor gen�tico da capacidade maternal" ,ylab="logOdds")
#Sem aspecto linear

# -- Metodo dos polinomios fraccionarios 
library(mfp)
MULTGAMMAIDENT2p <-mfp(custototal.dia~peso_ent+fp(VG_capacidade_maternal)+
                             VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao,
                     family = Gamma(link = identity), 
                     data=dados)
print(MULTGAMMAIDENT2p)
summary(MULTGAMMAIDENT2p)
#N�o sugere transforma��o para a vari�vel considera linear

#__________VG_cap_crescimento

Qis <- as.numeric(quantile(dados$VG_cap_crescimento, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$capcresciCAT<- cut(dados$VG_cap_crescimento,            # variavel a categorizar
                         breaks=Qis,            # pontos de corte das classes (nos quartis)
                         right=FALSE,           # classes abertas a direita
                         include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$capcresciCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMAIDENT3a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+capcresciCAT+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
                     na.action = na.exclude, 
                     family=Gamma(identity),
                     data=dados)
summary(MULTGAMMAIDENT3a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMAIDENT3a$coef[4:6]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de capacidade de crescimento categorizada")  # titulo do grafico
lines(lowess(x,y))
# aspecto linear
###---------- M�todo lowess ----------- ###
#
plot(lowess(predict(MULTGAMMAIDENT)~dados$VG_cap_crescimento), type="l", main="Linearidade pelo m�todo de Lowess",xlab="Valor gen�tico da capacidade de crescimento" ,ylab="logOdds")

# -- Metodo dos polinomios fraccionarios 

MULTGAMMAIDENT3p <-mfp(custototal.dia~peso_ent+VG_capacidade_maternal+
                             fp(VG_cap_crescimento)+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos,
                     family = Gamma(link = identity), 
                     data=dados)
print(MULTGAMMAIDENT3p)
summary(MULTGAMMAIDENT3p)
#n�o sugere transforma��o considero linear


#__________________VG_carcaca_dia_idade
Qis <- as.numeric(quantile(dados$VG_carcaca_dia_idade, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$carcporidadeCAT<- cut(dados$VG_carcaca_dia_idade,            # variavel a categorizar
                            breaks=Qis,            # pontos de corte das classes (nos quartis)
                            right=FALSE,           # classes abertas a direita
                            include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$carcporidadeCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMAIDENT4a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+carcporidadeCAT+VG_Intervalo_Entre_partos, 
                     na.action = na.exclude, 
                     family=Gamma(identity),
                     data=dados)
summary(MULTGAMMAIDENT4a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMAIDENT4a$coef[5:7]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade VG carca�a por dia de idade categorizada")  # titulo do grafico
lines(lowess(x,y))
# Sem aspecto linear

plot(lowess(predict(MULTGAMMAIDENT)~dados$VG_carcaca_dia_idade), type="l", main="Linearidade pelo m�todo de Lowess",xlab="Valor gen�tico carca�a por dia de idade" ,ylab="logOdds")
#apresenta aspecto linear


#__________________VG_Intervalo_Entre_partos
Qis <- as.numeric(quantile(dados$VG_Intervalo_Entre_partos, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$intentrepartosCAT<- cut(dados$VG_Intervalo_Entre_partos,            # variavel a categorizar
                              breaks=Qis,            # pontos de corte das classes (nos quartis)
                              right=FALSE,           # classes abertas a direita
                              include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$intentrepartosCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAMMAIDENT5a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+intentrepartosCAT, 
                     na.action = na.exclude, 
                     family=Gamma(identity),
                     data=dados)
summary(MULTGAMMAIDENT5a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAMMAIDENT5a$coef[6:8]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade VG Intervalo entre partos categorizada")  # titulo do grafico
lines(lowess(x,y))
#aspecto linear


########-------------------------------------------------------------------########
###################                              ###################
########-------------------------------------------------------------------########


MULTGAMMAIDENT<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=Gamma(link = identity), na.action = na.exclude)
summary(MULTGAMMAIDENT)
gamma.shape(MULTGAMMAIDENT)

library(car)
vif(MULTGAMMAIDENT)

#R�
(n<-length(dados$custototal.dia)) 
(R2N<-(1-exp((MULTGAMMAIDENT$dev-MULTGAMMAIDENT$null)/n))/(1-exp(-MULTGAMMAIDENT$null/n)))  #R� = 0,457

#varia��o explicada do modelo com resposta continua R�
1-((deviance(MULTGAMMAIDENT)/(df.residual(MULTGAMMAIDENT))))/(MULTGAMMAIDENT$null.deviance/MULTGAMMAIDENT$df.null)
#


BIC(MULTGAMMAIDENT)

resid<-resid(MULTGAMMAIDENT)
qqnorm(resid, ylab= 'Residuo Studentizado')
qqline(resid)

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
plot(z~predict(MULTGAMMAIDENT,type="link"), xlab=expression(hat(eta)), dados, ylab="Resposta linearizada", main = 'Adequabilidade da fun��o gama e liga��o identidade')
# se ajustou bem a uma reta

#avalia��o variancia
ra<-resid(MULTGAMMAIDENT,type="response")
tr<-2*log(predict(MULTGAMMAIDENT,type="response"))
plot(ra ~ tr, xlab=expression(2*log(hat(mu))),ylab="Res�duos Absolutos")
lines (lowess (ra~tr), col="red")
# n�o deve apresentar uma tend�ncia

hnp(MULTGAMMAIDENT$residuals, sim = 99,resid.type ='deviance',how.many.out=T ,
    conf = 0.95,scale = T)
