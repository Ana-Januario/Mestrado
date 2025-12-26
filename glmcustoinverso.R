#################################################################
#######MODELO GLM PARA O CUSTO POR DIA DE PRODUçãO###############
############PARA COMPARAçãO COM O MODELO RLM####################

setwd()

library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=T, sep=";", dec=",", fileEncoding = "latin1")

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




################MULTIVARIADO

MULT<-glm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_consumo_alim_residual+VG_gmd_estacao+VG_indice_conversao+VG_longevidade_produtiva, data=dados,family=gaussian(link = "identity"), na.action = na.exclude)
summary(MULT) 
#De acordo com o p-valor retira 
#VG_gmd_estacao
#VG_longevidade_produtiva
#VG_consumo_alim_residual
#Idadeent_meses
#VG_indice_conversao

 
MULTGAUS<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=gaussian(link = "identity"), na.action = na.exclude)
summary(MULTGAUS) 

vif(MULTGAUS) 

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
MULTGAUS1a <-glm(custototal.dia~pesoCAT+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=gaussian("identity"),
             data=dados)
# coeficientes estimados
summary(MULTGAUS1a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAUS1a$coef[2:4]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de peso categorizada")  # titulo do grafico
lines(lowess(x,y))                            # adiciona uma linha ao grafico a unir os pontos
#para peso tem aspecto linear

# -- VG capacidade maternal
Qis <- as.numeric(quantile(dados$VG_capacidade_maternal, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$VGCAPMATCAT<- cut(dados$VG_capacidade_maternal,            # variavel a categorizar
                    breaks=Qis,            # pontos de corte das classes (nos quartis)
                    right=FALSE,           # classes abertas a direita
                    include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$VGCAPMATCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAUS2a <-glm(custototal.dia~peso_ent+VGCAPMATCAT+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=gaussian("identity"),
             data=dados)
# coeficientes estimados
summary(MULTGAUS2a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAUS2a$coef[3:5]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de VG capacidade maternal categorizada")  # titulo do grafico
lines(lowess(x,y))                            # adiciona uma linha ao grafico a unir os pontos

## __ VG capacidade de crescimento
Qis <- as.numeric(quantile(dados$VG_cap_crescimento, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$VGCAPCRESC<- cut(dados$VG_cap_crescimento,            # variavel a categorizar
                        breaks=Qis,            # pontos de corte das classes (nos quartis)
                        right=FALSE,           # classes abertas a direita
                        include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$VGCAPCRESC)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAUS3a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VGCAPCRESC+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=gaussian("identity"),
             data=dados)
# coeficientes estimados
summary(MULTGAUS3a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAUS3a$coef[4:6]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de VG capacidade crescimento categorizada")  # titulo do grafico
lines(lowess(x,y)) 



## __ VG carcaça por dia de idade
Qis <- as.numeric(quantile(dados$VG_carcaca_dia_idade, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$VGCARCDIAIDADECAT<- cut(dados$VG_cap_crescimento,            # variavel a categorizar
                       breaks=Qis,            # pontos de corte das classes (nos quartis)
                       right=FALSE,           # classes abertas a direita
                       include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$VGCARCDIAIDADECAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAUS4a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VGCARCDIAIDADECAT+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=gaussian("identity"),
             data=dados)
# coeficientes estimados
summary(MULTGAUS4a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAUS4a$coef[5:7]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de VG carcaça por dia de idade categorizada")  # titulo do grafico
lines(lowess(x,y)) 

MULTGAUS4p <-mfp(custototal.dia~peso_ent+VG_capacidade_maternal+
               VG_cap_crescimento+fp(VG_carcaca_dia_idade)+VG_Intervalo_Entre_partos+VG_indice_conversao,
             family = gaussian(link = identity), 
             data=dados)
print(MULTGAUS4p)
#não sugere transformação

###---------- Método lowess ----------- ###
#
plot(lowess(predict(MULTGAUS)~dados$VG_carcaca_dia_idade), type="l", main="Linearidade pelo método de Lowess",xlab="Valor genético da carcaça por dia de idade" ,ylab="logOdds")


## __ VG intervalo entre partos
Qis <- as.numeric(quantile(dados$VG_Intervalo_Entre_partos, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$VGINTENTREPARTOSCAT<- cut(dados$VG_Intervalo_Entre_partos,            # variavel a categorizar
                              breaks=Qis,            # pontos de corte das classes (nos quartis)
                              right=FALSE,           # classes abertas a direita
                              include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$VGINTENTREPARTOSCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGAUS5a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VGINTENTREPARTOSCAT, 
             na.action = na.exclude, 
             family=gaussian("identity"),
             data=dados)
# coeficientes estimados
summary(MULTGAUS5a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGAUS5a$coef[6:8]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de VG intervalo entre partos categorizada")  # titulo do grafico
lines(lowess(x,y)) 


##############################################################

MULTGAUS<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=gaussian(link = "identity"), na.action = na.exclude)
summary(MULTGAUS)


#######################R2
#modelo sem interação
(n<-length(dados$custototal.dia)) 
(R2N<-(1-exp((MULTGAUS$dev-MULTGAUS$null)/n))/(1-exp(-MULTGAUS$null/n)))  #R2 = 0,46

#variação explicada do modelo com resposta continua R2
1-((deviance(MULTGAUS)/(df.residual(MULTGAUS))))/(MULTGAUS$null.deviance/MULTGAUS$df.null)
#A resposta do RLM sem outlier é melhor
BIC(MULTGAUS)

resid<-resid(MULTGAUS)
qqnorm(resid, ylab= 'Resíduo Studentizado')
qqline(resid)


#TESTES DA BONDADE DO AJUSTAMENTO
#modelo 
fit1 <- lrm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados, x=TRUE, y=TRUE)
resid(fit1, 'gof')
print(fit1)


#Comparando com modelo nulo pela verossimilhança
fit0a <- glm(custototal.dia ~ 1,  
             data=MULTGAUS$model,           
             family=gaussian(link=identity))    
summary(fit0a)
anova(fit0a, MULTGAUS, test="Chisq")


##..........................................##
##### Analise de residuos via individuos #####
# Nota: quando existem variaveis continuas no modelo quase sempre havera quase tantos padroes como covariaveis e esta abordagem pode ser usada

## residuos deviance ##
rd<-residuals (MULTGAUS, type="deviance") # residuos deviance
plot(fitted(MULTGAUS), rd, xlab= "Probabilidades estimadas", ylab="Residuos Deviance") 


## distancia de Cook ##
plot(cooks.distance(MULTGAUS), xlab="Individuos", ylab="Distancia de Cook")
identify(cooks.distance(MULTGAUS), labels=rownames(dados)) # identificar os pontos que se destacam

#os individuos que se apresentaram como outliers nessa analise também foram outlier no RLM
# 2   3 263 268 337 348 363

# Representação dos dfbeta vs obs #
dfbetasPlots(MULTGAUS)

#Leverage
h<-influence (MULTGAUS)
plot(h$hat) 
#apesar de existirem muitos pontos acima do limite não há nenhum excessivamente elevado nem que se destaque 
rd<-residuals (MULTGAUS, type="deviance") # residuos deviance
plot(fitted(MULTGAUS), rd, xlab= "Probabilidades estimadas", ylab="Resíduos Deviance") 

# identificar as possiveis observacoes influentes
temp<-influence.measures(MULTGAUS)
(lista <- which(apply(temp$is.inf, 1, any)))	 # lista as candidatas a observacoes influentes
summary(temp) 

#adequabilidade da função de ligação gaussiana e link identidade
mu <-predict(MULTGAUS, type="response")
z <-predict(MULTGAUS)+(dados$custototal.dia-mu)
plot(z~predict(MULTGAUS,type="link"), 
main = "Adequabilidade da função gaussiana e link identidade", xlab=expression(hat(eta)), dados, ylab="Resposta linearizada")
# se se ajustar a uma recta, está ok


#avaliação variancia
ra<-resid(MULTGAUS,type="response")
tr<-2*log(predict(MULTGAUS,type="response"))
plot(ra ~ tr, xlab=expression(2*log(hat(mu))),ylab="Resíduos Absolutos")
lines (lowess (ra~tr), col="red")
# não deve apresentar uma tendência

#gráfico (meio) normal de um objeto de modelo ajustado para uma variedade de modelos diferentes
#https://www.rpubs.com/TiagoCosta/502155
library(hnp)
hnp(MULTGAUS$residuals, sim = 99,resid.type ='deviance',how.many.out=T ,
    conf = 0.95,scale = T, main = 'Gráfico normal com envelope', xlab = 'Quartis teóricos', ylab = 'Resíduos')

####################################################################
######################################################################
######################################################################
#GAUSSIANA INVERSA
#LINK = "1/mu^2




#Modelo nulo
fit0 <- glm(dados$custototal.dia ~ 1,        # Y ~ lista de variaveis explicativas. ~1 indicia que so se considera a constante (modelo nulo) 
            data=dados,                     # nome do conjunto de dados
            family=inverse.gaussian(link = "inverse"))    # distribuicao de Y e funcao de ligacao
# -- coeficientes do modelo e sua significancia
summary(fit0)        


#Idade a entrada
fit1 <- glm(dados$custototal.dia ~ dados$Idadeent_meses, 
            data=dados,family=inverse.gaussian(link = "inverse"))
summary(fit1) #
anova(fit1, test="Chisq") 

#Peso a entrada
fit2 <- glm(dados$custototal.dia ~ dados$peso_ent, 
            data=dados,family=inverse.gaussian(link = "inverse"))
summary(fit2) #
anova(fit2, test="Chisq") 
exp(fit2$coef[2])

#VG Capacidade maternal
fit3 <- glm(dados$custototal.dia ~ dados$VG_capacidade_maternal, 
            data=dados,family=inverse.gaussian(link = "inverse"))
summary(fit3) #
anova(fit3, test="Chisq") 
exp(fit3$coef[2])

#VG capacidade de crescimento
fit4 <- glm(dados$custototal.dia ~ dados$VG_cap_crescimento, 
            data=dados,family=inverse.gaussian(link = "inverse"))
summary(fit4) #< 2.2e-16 ***
anova(fit4, test="Chisq") 
exp(fit4$coef[2])

#VG_gmd_estacao
fit5 <- glm(dados$custototal.dia ~ dados$VG_gmd_estacao, 
            data=dados,family=inverse.gaussian(link = "inverse"))
summary(fit5) #< 2.2e-16 ***
anova(fit5, test="Chisq") 

#VG_carcaca_dia_idade
fit6 <- glm(dados$custototal.dia ~ dados$VG_carcaca_dia_idade, 
            data=dados,family=inverse.gaussian(link="inverse"))
summary(fit6) #< 2.2e-16 ***
anova(fit6, test="Chisq") 

#VG_Intervalo_Entre_partos
fit7 <- glm(dados$custototal.dia ~ dados$VG_Intervalo_Entre_partos, 
            data=dados,family=inverse.gaussian(link = "inverse"))
summary(fit7) #< 2.2e-16 ***
anova(fit7, test="Chisq") 


#VG_indice_conversao
fit8 <- glm(dados$custototal.dia ~ dados$VG_indice_conversao, 
            data=dados,family=inverse.gaussian(link = "inverse"))
summary(fit8) #< 2.2e-16 ***
anova(fit8, test="Chisq") 


#VG_longevidade_produtiva
fit9 <- glm(dados$custototal.dia ~ dados$VG_longevidade_produtiva, 
            data=dados,family=inverse.gaussian(link = "inverse"))
summary(fit9) #< 2.2e-16 ***
anova(fit9, test="Chisq") 


#VG_consumo_alim_residual
fit10 <- glm(dados$custototal.dia ~ dados$VG_consumo_alim_residual, 
             data=dados,family=inverse.gaussian(link = "inverse"))
summary(fit10) #< 2.2e-16 ***
anova(fit10, test="Chisq")


################MULTIVARIADO

MULT<-glm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade, data=dados,family=inverse.gaussian(link = "inverse"), na.action = na.exclude)
summary(MULT) 
#idade deixa de fazer sentido

MULTGINV<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade, data=dados,family=inverse.gaussian(link = "inverse"), na.action = na.exclude)
summary(MULTGINV)
#teste add Intervalo entre partos
MULTGINV<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=inverse.gaussian(link = "inverse"), na.action = na.exclude)
summary(MULTGINV)


# ------ avaliar pressuposto de linearidade (para covariaveis continuas) -----

# -- Metodo dos quartis
#Peso
Qis <- as.numeric(quantile(dados$peso_ent, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$pesoCAT<- cut(dados$peso_ent,            # variavel a categorizar
                    breaks=Qis,            # pontos de corte das classes (nos quartis)
                    right=FALSE,           # classes abertas a direita
                    include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$pesoCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGINV1a <-glm(custototal.dia~pesoCAT+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=inverse.gaussian("1/mu^2"),
             data=dados)
# coeficientes estimados
summary(MULTGINV1a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGINV1a$coef[2:4]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de peso categorizada gaussiana inversa")  # titulo do grafico
lines(lowess(x,y))                            # adiciona uma linha ao grafico a unir os pontos
#para peso tem aspecto linear


# -- VG capacidade maternal
Qis <- as.numeric(quantile(dados$VG_capacidade_maternal, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$VGCAPMATCAT<- cut(dados$VG_capacidade_maternal,            # variavel a categorizar
                    breaks=Qis,            # pontos de corte das classes (nos quartis)
                    right=FALSE,           # classes abertas a direita
                    include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$VGCAPMATCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGINV2a <-glm(custototal.dia~peso_ent+VGCAPMATCAT+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=inverse.gaussian("1/mu^2"),
             data=dados)
# coeficientes estimados
summary(MULTGINV2a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGINV2a$coef[3:5]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de VG capac maternal categorizada")  # titulo do grafico
lines(lowess(x,y))                            # adiciona uma linha ao grafico a unir os pontos
#

# -- Metodo dos polinomios fraccionarios 
MULTGINV2p <-mfp(custototal.dia~peso_ent+fp(VG_capacidade_maternal)+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos,
             family = inverse.gaussian(link = 1/mu^2), 
             data=dados)
print(MULTGINV2p)
summary(MULTGINV2p)
#não sugere transformação

# -- VG capacidade de crescimento
Qis <- as.numeric(quantile(dados$VG_cap_crescimento, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$VGCAPCRESCCAT<- cut(dados$VG_cap_crescimento,            # variavel a categorizar
                        breaks=Qis,            # pontos de corte das classes (nos quartis)
                        right=FALSE,           # classes abertas a direita
                        include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$VGCAPCRESCCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGINV3a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VGCAPCRESCCAT+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=inverse.gaussian("1/mu^2"),
             data=dados)
# coeficientes estimados
summary(MULTGINV3a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGINV3a$coef[4:6]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de VG capac crescimento categorizada")  # titulo do grafico
lines(lowess(x,y))                            # adiciona uma linha ao grafico a unir os pontos
#
# -- Metodo dos polinomios fraccionarios 
MULTGINV3p <-mfp(custototal.dia~peso_ent+VG_capacidade_maternal+fp(VG_cap_crescimento)+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos,
             family = inverse.gaussian(link = 1/mu^2), 
             data=dados)
print(MULTGINV3p)
summary(MULTGINV3p)
#não sugere transformação

# -- VG carcaça por dia de idade
Qis <- as.numeric(quantile(dados$VG_carcaca_dia_idade, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$VGCARCDIAIDADECAT<- cut(dados$VG_carcaca_dia_idade,            # variavel a categorizar
                          breaks=Qis,            # pontos de corte das classes (nos quartis)
                          right=FALSE,           # classes abertas a direita
                          include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$VGCARCDIAIDADECAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGINV4a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VGCARCDIAIDADECAT+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=inverse.gaussian("1/mu^2"),
             data=dados)
# coeficientes estimados
summary(MULTGINV4a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGINV4a$coef[5:7]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de VG carcaça dia de idade categorizada")  # titulo do grafico
lines(lowess(x,y))   

###---------- Método lowess ----------- ###
#
plot(lowess(predict(MULTGINV)~dados$VG_carcaca_dia_idade), type="l", main="Linearidade pelo método de Lowess",xlab="Valor genético da carcaça por dia de idade" ,ylab="logOdds")
#Aspecto linear


# -- Metodo dos polinomios fraccionarios 
MULTGINV4p <-mfp(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+fp(VG_carcaca_dia_idade)+VG_Intervalo_Entre_partos,
             family = inverse.gaussian(link = 1/mu^2), 
             data=dados)
print(MULTGINV4p)
summary(MULTGINV4p)
#não sugere transformação

# -- VG intervalo entre partos
Qis <- as.numeric(quantile(dados$VG_Intervalo_Entre_partos, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$VGINTENTRPARTOS<- cut(dados$VG_Intervalo_Entre_partos,            # variavel a categorizar
                              breaks=Qis,            # pontos de corte das classes (nos quartis)
                              right=FALSE,           # classes abertas a direita
                              include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$VGCARCDIAIDADECAT)
# ajustar modelo com a variavel quantitativa categorizada
MULTGINV5a <-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VGCARCDIAIDADECAT+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=inverse.gaussian("1/mu^2"),
             data=dados)
# coeficientes estimados
summary(MULTGINV5a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULTGINV5a$coef[6:8]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de VG intervalo entre partos categorizada")  # titulo do grafico
lines(lowess(x,y))   

###---------- Método lowess ----------- ###
#
plot(lowess(predict(MULTGINV)~dados$VG_Intervalo_Entre_partos), type="l", main="Linearidade pelo método de Lowess",xlab="Valor genético Intervalo entre partos" ,ylab="logOdds")
#Sem aspecto linear

# -- Metodo dos polinomios fraccionarios 
MULTGINV5p <-mfp(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+fp(VG_Intervalo_Entre_partos),
             family = inverse.gaussian(link = 1/mu^2), 
             data=dados)
print(MULTGINV5p)
summary(MULTGINV5p)
#não sugere transformação



MULTGINV<-glm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=inverse.gaussian(link = "inverse"), na.action = na.exclude)
summary(MULTGINV)



vif(MULTGINV)


#######################R2
#R2
(n<-length(dados$custototal.dia)) 
(R2N<-(1-exp((MULTGINV$dev-MULTGINV$null)/n))/(1-exp(-MULTGINV$null/n)))  #R2 = 0,42

#variação explicada do modelo com resposta continua R2
1-((deviance(MULTGINV)/(df.residual(MULTGINV))))/(MULTGINV$null.deviance/MULTGINV$df.null)
#


BIC(MULTGINV)

#TESTES DA BONDADE DO AJUSTAMENTO
fit1 <- lrm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados, x=TRUE, y=TRUE)
resid(fit1, 'gof')
print(fit1)


#Comparando com modelo nulo pela verossimilhança
fit0 <- glm(custototal.dia ~ 1,        
              data=MULTGINV$model,           
              family=inverse.gaussian(link=inverse))    
summary(fit0)
anova(fit0, MULTGINV, test="Chisq")

resid<-resid(MULTGINV)
qqnorm(resid, ylab= 'Residuo Studentizado')
qqline(resid)

##..........................................##
##### Analise de residuos via individuos #####
# Nota: quando existem variaveis continuas no modelo quase sempre havera quase tantos padroes como covariaveis e esta abordagem pode ser usada

## residuos deviance ##
rd<-residuals (MULTGINV, type="deviance") # residuos deviance
plot(fitted(MULTGINV), rd, xlab= "Probabilidades estimadas", ylab="Residuos Deviance") 


## distancia de Cook ##
plot(cooks.distance(MULTGINV), xlab="Individuos", ylab="Distancia de Cook")
identify(cooks.distance(MULTGINV), labels=rownames(dados)) # identificar os pontos que se destacam

#os individuos que se apresentaram como outliers nessa analise também foram outlier no RLM
# 2   3 348 

# Representação dos dfbeta vs obs #
dfbetasPlots (MULTGINV)

#Leverage
h<-influence (MULTGINV)
plot(h$hat) 
#apesar de existirem muitos pontos acima do limite não ha nenhum excessivamente elevado nem que se destaque 
rd<-residuals (MULTGINV, type="deviance") # residuos deviance
plot(fitted(MULTGINV), rd, xlab= "Probabilidades estimadas", ylab="Resíduos Deviance") 

# identificar as possiveis observacoes influentes
temp<-influence.measures(MULTGINV)
(lista <- which(apply(temp$is.inf, 1, any)))	 # lista as candidatas a observacoes influentes
summary(temp) 
#os valores influentes são diferentes do RLM

#Adequabilidade da função de ligação inverse gaussian e link 1/mu^2
mu <-predict(MULTGINV, type="response")
z <-predict(MULTGINV)-2*(dados$custototal.dia-mu)/mu^3
plot(z~predict(MULTGINV,type="link"), xlab=expression(hat(eta)), dados, ylab="Resposta linearizada",
     main = "Adequabilidade da função gaussiana inversa e ligação inversa")
#Ajustou a uma reta, porém mais disperso


#Avaliação variancia
ra<-resid(MULTGINV,type="response")
tr<-2*log(predict(MULTGINV,type="response"))
plot(ra ~ tr, xlab=expression(2*log(hat(mu))),ylab="Resíduos Absolutos")
lines (lowess (ra~tr), col="red")
# não deve apresentar uma tendência

hnp(MULTGINV$residuals, sim = 99,resid.type ='deviance',how.many.out=T ,
    conf = 0.95,scale = T)
