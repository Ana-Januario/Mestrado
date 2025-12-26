#############################################################################
###########MODELO LOGISTICO#################################################

getwd()

library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=T, sep=";", dec=",", fileEncoding = "latin1")
dados$Dest_abate2<-as.numeric(factor(dados$Dest_abate))-1 #Sem o -1 ficaria 1 e 2, com -1 0 e 1
table(dados$Dest_abate)
table(dados$Dest_abate2)
376/338

library(rms)
library(aod)
library(Hmisc)
library(gam)
library(mfp)
library(faraway)
library(car)
library(generalhoslem)
library(ResourceSelection)
library(MASS)
library(Epi)
library(ggplot2)

library(epiR)
library(DAAG)

library(ltm)
#CORRELAÇÃO PONTO BISERIAL
biserial.cor(dados$peso_ent, dados$Dest_abate2) #0,10
biserial.cor(dados$Idadeent_meses, dados$Dest_abate2) #-0,32
biserial.cor(dados$P210, dados$Dest_abate2, use = c("complete.obs"), level = 1) #muito NA 0,17
biserial.cor(dados$VG_capacidade_maternal, dados$Dest_abate2) #-0,019
biserial.cor(dados$VG_cap_crescimento, dados$Dest_abate2)#0.08
biserial.cor(dados$VG_gmd_estacao, dados$Dest_abate2)#0,04
biserial.cor(dados$VG_carcaca_dia_idade, dados$Dest_abate2) #0,11
biserial.cor(dados$VG_Intervalo_Entre_partos, dados$Dest_abate2) #- 0,14
biserial.cor(dados$VG_indice_conversao, dados$Dest_abate2) #-0,038
biserial.cor(dados$VG_longevidade_produtiva, dados$Dest_abate2) #-0,03
biserial.cor(dados$VG_consumo_alim_residual, dados$Dest_abate2) #0,0049
biserial.cor(dados$GMD, dados$Dest_abate2) #-0,048
biserial.cor(dados$Diasengorda, dados$Dest_abate2) #-0,56
biserial.cor(dados$custototal.dia, dados$Dest_abate2) #-0,29
biserial.cor(dados$liquido_recria, dados$Dest_abate2) #-0,07



#Modelo nulo
fit0 <- glm(dados$Dest_abate2 ~ 1,        # Y ~ lista de variaveis explicativas. ~1 indicia que so se considera a constante (modelo nulo) 
            data=dados,                     # nome do conjunto de dados
            family=binomial(link=logit))    # distribuicao de Y e funcao de ligacao
# -- coeficientes do modelo e sua significancia
summary(fit0)  #AIC: 987,79         
exp(fit0$coefficients) #confirmado que meu DOP é meu sucesso nessa modelação


#Modelo simples

fit1 <- glm(dados$Dest_abate2 ~ dados$peso_ent, 
            data=dados,family=binomial(link=logit))

# -- coeficientes do modelo e sua significancia
summary(fit1) # Nota: valor p (Wald) de beta1 = 0,00415 -> variavel significativa ao nivel alfa de 5% 
anova(fit1, test="Chisq") #0,003848 
exp(fit1$coef[1])
exp(fit1$coef[2])


fit2 <- glm(dados$Dest_abate2 ~ dados$Idadeent_meses, 
            data=dados,family=binomial(link=logit))

summary(fit2) # Nota: valor p (Wald) de beta1 = < 2e-16 -> variavel significativa ao nivel alfa de 5% 
anova(fit2, test="Chisq") #<0,001
exp(fit2$coef[1])
exp(fit2$coef[2])


fit3 <- glm(dados$Dest_abate2 ~ dados$P210, 
            data=dados,family=binomial(link=logit))
summary(fit3) # Nota: valor p (Wald) de beta1 = 0,00077 -> variavel significativa ao nivel alfa de 5% 
anova(fit3, test="Chisq") #<0,001
exp(fit3$coef[1])
exp(fit3$coef[2])


fit4 <- glm(dados$Dest_abate2 ~ dados$VG_capacidade_maternal, 
            data=dados,family=binomial(link=logit))
summary(fit4) # Nota: valor p (Wald) de beta1 = 0,593 -> variavel significativa ao nivel alfa de 5% 
anova(fit4, test="Chisq") #0,59
exp(fit4$coef[1])
exp(fit4$coef[2])


fit5 <- glm(dados$Dest_abate2 ~ dados$VG_cap_crescimento, 
            data=dados,family=binomial(link=logit))
summary(fit5) # Nota: valor p (Wald) de beta1 = 0,0178 -> variavel significativa ao nivel alfa de 5% 
anova(fit5, test="Chisq") #0,017
exp(fit5$coef[1])
exp(fit5$coef[2])


fit6 <- glm(dados$Dest_abate2 ~ dados$VG_gmd_estacao, 
            data=dados,family=binomial(link=logit))
summary(fit6) # Nota: valor p (Wald) de beta1 = 0,26-> variavel significativa ao nivel alfa de 5% 
anova(fit6, test="Chisq") #0,26


fit7 <- glm(dados$Dest_abate2 ~ dados$VG_carcaca_dia_idade, 
            data=dados,family=binomial(link=logit))
summary(fit7) # Nota: valor p (Wald) de beta1 = 0,00209-> variavel significativa ao nivel alfa de 5% 
anova(fit7, test="Chisq") #0,0018


fit8 <- glm(dados$Dest_abate2 ~ dados$VG_Intervalo_Entre_partos, 
            data=dados,family=binomial(link=logit))
summary(fit8) # Nota: valor p (Wald) de beta1 = 0,00013-> variavel significativa ao nivel alfa de 5% 
anova(fit8, test="Chisq") #0.0001066


fit9 <- glm(dados$Dest_abate2 ~ dados$VG_indice_conversao, 
            data=dados,family=binomial(link=logit))
summary(fit9) # Nota: valor p (Wald) de beta1 = 0,00013-> variavel significativa ao nivel alfa de 5% 
anova(fit9, test="Chisq")


fit10 <- glm(dados$Dest_abate2 ~ dados$VG_longevidade_produtiva, 
            data=dados,family=binomial(link=logit))
summary(fit10) # Nota: valor p (Wald) de beta1 = 0,365-> variavel significativa ao nivel alfa de 5% 
anova(fit10, test="Chisq")


fit11 <- glm(dados$Dest_abate2 ~ dados$VG_consumo_alim_residual, 
             data=dados,family=binomial(link=logit))
summary(fit11) # Nota: valor p (Wald) de beta1 = 0,89-> variavel significativa ao nivel alfa de 5% 
anova(fit11, test="Chisq")


fit12 <- glm(dados$Dest_abate2 ~ dados$GMD, 
             data=dados,family=binomial(link=logit))
summary(fit12) # Nota: valor p (Wald) de beta1 = 0,196-> variavel significativa ao nivel alfa de 5% 
anova(fit12, test="Chisq")


fit13 <- glm(dados$Dest_abate2 ~ dados$Diasengorda, 
             data=dados,family=binomial(link=logit))
summary(fit13) # Nota: valor p (Wald) de beta1 <0,001 -> variavel significativa ao nivel alfa de 5% 
anova(fit13, test="Chisq") #< 2.2e-16


fit14 <- glm(dados$Dest_abate2 ~ dados$custototal.dia, 
             data=dados,family=binomial(link=logit))
summary(fit14) # Nota: valor p (Wald) de beta1 <0,001 -> variavel significativa ao nivel alfa de 5% 
anova(fit14, test="Chisq") #< 2.2e-16


fit15 <- glm(dados$Dest_abate2 ~ dados$liquido_recria, 
             data=dados,family=binomial(link=logit))
summary(fit15) # Nota: valor p (Wald) de beta1 0,06 -> variavel significativa ao nivel alfa de 5% 
anova(fit15, test="Chisq") #0,06



#####____________________Modelo Multivariado_____________________________#######
#foram add as variáveis que tiveram p-valor<0,25
#dados a entrada
MULT1<-glm(Dest_abate2~peso_ent+Idadeent_meses+P210+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULT1) 
library(car)
vif(MULT1) #peso, idade e P210 problemas de multicolinearidade

#P210 tem maior p-valor
MULT2<-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULT2)


#Retira VG_cap_crescimento
MULT3<-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULT3)
#AIC: 842.66

#Verificação das variáveis que não entraram 
#VG_capacidade_maternal
MULT4<-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_capacidade_maternal, data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULT4)
vif(MULT4) #não tem problema de multicolinearidade
#significativo
#Reduz o AIC para 840,26

#VG_gmd_estacao
MULT5<-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_capacidade_maternal+VG_gmd_estacao, data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULT5)
vif(MULT5) #peso e idade apresentam problemas de multicolinearidade
#significativo
#Reduz AIC 834,65

#VG_indice_conversao
MULT6<-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_capacidade_maternal+VG_gmd_estacao+VG_indice_conversao, data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULT6)
#não é significativo

#VG_longevidade_produtiva
MULT7<-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_capacidade_maternal+VG_gmd_estacao+VG_longevidade_produtiva, data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULT7)
#não é significativo

#VG_consumo_alim_residual
MULT8<-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_capacidade_maternal+VG_gmd_estacao+VG_consumo_alim_residual, data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULT8)
#não é significativo



#conversa com professor 
#fazer com todas as variaveis de uma vez
MULT<-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_gmd_estacao+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULT) 
#retirei pela ordem de maior p-valor
#P210
#VG_cap_crescimento
#VG_indice_conversao
#VG_longevidade_produtiva
#VG_consumo_alim_residual

#o modelo obtido foi o mesmo do MULT 5

#modelo obtido
MULT<-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_gmd_estacao+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULT)
library(car)
vif(MULT) #Idade>2 talvez teria algum problema


#pseudo coeficiente de determinação 
(n<-length(dados$Dest_abate2)) # número observações da variavel resposta
(R2N<-(1-exp((MULT$dev-MULT$null)/n))/(1-exp(-MULT$null/n)))  #R²=27,8%


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
MULT1a <-glm(Dest_abate2~pesoCAT+Idadeent_meses+VG_capacidade_maternal+VG_gmd_estacao+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
               na.action = na.exclude, 
               family=binomial("logit"),
               data=dados)
# coeficientes estimados
summary(MULT1a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULT1a$coef[2:4]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de peso categorizada")  # titulo do grafico
lines(lowess(x,y))                            # adiciona uma linha ao grafico a unir os pontos
#para peso n�o tem aspecto linear

## -- Metodo lowess
# grafico de dispersao dos valores preditos vs variavel peso

plot(lowess(predict(MULT)~dados$peso_ent), 
     type="l", main = "Linearidade peso", xlab="Peso a entrada", ylab="logOdds")
#aspecto linear



# -- Metodo dos polinomios fraccionarios 
library(mfp)
MULT1p <-mfp(Dest_abate2~fp(peso_ent)+Idadeent_meses+VG_capacidade_maternal+
                 VG_gmd_estacao+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos,
               family = binomial(link = logit), 
               data=dados)
print(MULT1p)
summary(MULT1p)

#não sugere uma transformação mas apenas uma troca de escala: (peso/100)
#mantem-se a variável do jeito que está


#______________Idade a entrada
#######################################
Qis <- as.numeric(quantile(dados$Idadeent_meses, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$idadeCAT<- cut(dados$Idadeent_meses,            # variavel a categorizar
                    breaks=Qis,            # pontos de corte das classes (nos quartis)
                    right=FALSE,           # classes abertas a direita
                    include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$idadeCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULT2a <-glm(Dest_abate2~peso_ent+idadeCAT+VG_capacidade_maternal+VG_gmd_estacao+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=binomial("logit"),
             data=dados)
# coeficientes estimados
summary(MULT2a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULT2a$coef[3:5]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade da idade categorizada", xlab = "Idade a entrada")  # titulo do grafico
lines(lowess(x,y))    

#tem aspecto linear


#________________________________________________
#------------------------------VG_capacidade_maternal

Qis <- as.numeric(quantile(dados$VG_capacidade_maternal, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$capmaternoCAT<- cut(dados$VG_capacidade_maternal,            # variavel a categorizar
                     breaks=Qis,            # pontos de corte das classes (nos quartis)
                     right=FALSE,           # classes abertas a direita
                     include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$capmaternoCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULT3a <-glm(Dest_abate2~peso_ent+dados$Idadeent_meses+capmaternoCAT+VG_gmd_estacao+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+peso_ent*VG_carcaca_dia_idade, 
               na.action = na.exclude, 
               family=binomial("logit"),
               data=dados)
summary(MULT3a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULT3a$coef[4:6]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de capacidade maternal categorizada")  # titulo do grafico
lines(lowess(x,y))

## -- Metodo lowess
# grafico de dispersao dos valores preditos vs variavel capacidade maternal

plot(lowess(predict(MULT)~dados$VG_capacidade_maternal), 
     type="l", xlab="x", ylab="logOdds")
#

# -- Metodo dos polinomios fraccionarios 
library(mfp)
MULT3p <-mfp(Dest_abate2~peso_ent+Idadeent_meses+fp(VG_capacidade_maternal)+
                 VG_gmd_estacao+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos,
            family = binomial(link = logit), 
            data=dados)
print(MULT3p)
summary(MULT3p)


#----------------------------------------------------------------------
#________________________________________________VG_gmd_estacao

Qis <- as.numeric(quantile(dados$VG_gmd_estacao, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$gmdtesteCAT<- cut(dados$VG_gmd_estacao,            # variavel a categorizar
                          breaks=Qis,            # pontos de corte das classes (nos quartis)
                          right=FALSE,           # classes abertas a direita
                          include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$gmdtesteCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULT4a <-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+gmdtesteCAT+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, 
               na.action = na.exclude, 
               family=binomial("logit"),
               data=dados)
summary(MULT4a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULT4a$coef[5:7]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de GMD teste esta��o categorizada")  # titulo do grafico
lines(lowess(x,y))
#

## -- Metodo lowess
# grafico de dispersao dos valores preditos vs variavel VG gmd em teste de esta��o
plot(lowess(predict(MULT)~dados$VG_gmd_estacao), 
     type="l", main = "Linearidade VG GMD em teste de esta��o",xlab="VG GMD em teste de esta��o", ylab="logOdds")
#

# -- Metodo dos polinomios fraccionarios 
MULT4p <-mfp(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+fp(VG_gmd_estacao)+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos,
             family = quasibinomial(link = logit), 
             data=dados)
warnings()
print(MULT4p)
summary(MULT4p)
# APARECEU PROBLEMAS DE CONVERGENCIA foi resolvido pelo método da
#Mquasiverossimilhança

#----------------------------------------------------------------------
#_________________________VG_carcaça por dia de idade

Qis <- as.numeric(quantile(dados$VG_carcaca_dia_idade, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$carcidadeCAT<- cut(dados$VG_carcaca_dia_idade,            # variavel a categorizar
                        breaks=Qis,            # pontos de corte das classes (nos quartis)
                        right=FALSE,           # classes abertas a direita
                        include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$carcidadeCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULT5a <-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_gmd_estacao+carcidadeCAT+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=binomial("logit"),
             data=dados)
summary(MULT5a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULT5a$coef[6:8]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade VG carcaça dia de idade categorizada")  # titulo do grafico
lines(lowess(x,y))
#não tem aspecto linear

## -- Metodo lowess
# grafico de dispersao dos valores preditos vs variavel peso
plot(lowess(predict(MULT)~dados$VG_carcaca_dia_idade), 
     type="l", main = "Linearidade VG carcaça por dia de idade",xlab="VG carcaça por dia de idade", ylab="logOdds")
#também sem aspecto linear

# -- Metodo dos polinomios fraccionarios 
MULT5p <-mfp(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_gmd_estacao+fp(VG_carcaca_dia_idade)+VG_Intervalo_Entre_partos,
             family = binomial(link = logit), 
             data=dados)
print(MULT5p)
summary(MULT5p)

#transformação da variável conforme polinomio fracionários
dados$VG_carcaca_dia_idadetransf<-(dados$VG_carcaca_dia_idade)^3

#verificação da linearidade da variável transformada
Qis <- as.numeric(quantile(dados$VG_carcaca_dia_idadetransf, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$VG_carcaca_dia_idadetransfcat<- cut(dados$VG_carcaca_dia_idadetransf,            # variavel a categorizar
                        breaks=Qis,            # pontos de corte das classes (nos quartis)
                        right=FALSE,           # classes abertas a direita
                        include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$VG_carcaca_dia_idadetransfcat)
# ajustar modelo com a variavel quantitativa categorizada
MULT5a1 <-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_gmd_estacao+VG_carcaca_dia_idadetransfcat+VG_Intervalo_Entre_partos, 
             na.action = na.exclude, 
             family=binomial("logit"),
             data=dados)
summary(MULT5a1)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULT5a1$coef[6:8]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade de carcaça por dia
         de idade transformada categorizada")  # titulo do grafico
lines(lowess(x,y))

#----------------------------------------------------------------------
#Modelo sem interação com VG_carcaca_dia_idadetransf
MULTI1<-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+
                VG_gmd_estacao+VG_carcaca_dia_idadetransf+VG_Intervalo_Entre_partos, 
            data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULTI1)


# grafico de dispersao dos valores preditos vs variavel carcaça por dia de idade
plot(lowess(predict(MULTI1)~dados$VG_carcaca_dia_idadetransf), 
     type="l", main = "Linearidade VG carcaça por dia de idade",xlab="VG carcaça por dia de idade transformada", ylab="logOdds")
#aspecto linear


#----------------------------------------------------------------------
#_________________________VG_intervalo entre partos

Qis <- as.numeric(quantile(dados$VG_Intervalo_Entre_partos, probs=seq(0, 1, 0.25)))  # Calcula os quartis da variavel quantitativa (inclui max e min)
# categorizar a variavel quantitativa com base nos quartis
dados$intervalorentrepartosCAT<- cut(dados$VG_Intervalo_Entre_partos,            # variavel a categorizar
                         breaks=Qis,            # pontos de corte das classes (nos quartis)
                         right=FALSE,           # classes abertas a direita
                         include.lowest=TRUE)   # a ultima classe e fechada a direita
table(dados$intervalorentrepartosCAT)
# ajustar modelo com a variavel quantitativa categorizada
MULT6a <-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_gmd_estacao+VG_carcaca_dia_idadetransf+intervalorentrepartosCAT, 
             na.action = na.exclude, 
             family=binomial("logit"),
             data=dados)
summary(MULT6a)
# grafico com dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
k <- 5
x <- (Qis[1:(k-1)]+Qis[2:k])/2                # pontos medios das classes
y <- c(0, as.numeric(MULT6a$coef[7:9]))        # valor dos betas da variavel quantitativa categorizada, sendo o 1 beta=0
plot(x, y,                                    # grafico de dispersao
     main="Linearidade VG intervalo entre partos categorizada")  # titulo do grafico
lines(lowess(x,y))

## -- Metodo lowess
# grafico de dispersao dos valores preditos vs variavel intervalo entre partos
plot(lowess(predict(MULTI1)~dados$VG_Intervalo_Entre_partos), 
     type="l", main = "Linearidade VG intervalo entre partos",xlab="VG intervalor entre partos", ylab="logOdds")
#tem aspecto linear


########-------------------------------------------------------------------########
###################               Interações                    ###################
########-------------------------------------------------------------------########

#Foram apenas testadas as interações que fazem sentido no contexto
#Sem interação com VG_carcaca_dia_idadetransf
MULTI1<-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+
                    VG_gmd_estacao+VG_carcaca_dia_idadetransf+VG_Intervalo_Entre_partos, 
            data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULTI1)


## Peso e VG_Intervalo_Entre_partos ##
MULTI2<-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+
VG_gmd_estacao+VG_carcaca_dia_idadetransf+VG_Intervalo_Entre_partos+
        VG_Intervalo_Entre_partos*peso_ent, 
            data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULTI2)
anova(MULTI2, MULTI1, test="Chisq")
#significativa# 

## Peso e  VG_carcaca_dia_idade##
MULTI3<-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+
                    VG_gmd_estacao+VG_carcaca_dia_idadetransf+VG_Intervalo_Entre_partos+
                    VG_carcaca_dia_idadetransf*peso_ent, 
            data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULTI3)
anova(MULTI3, MULTI1, test="Chisq")

#Essa interação é bem significativa
#



######---------------------Multicolinariedade---------------------######

vif(MULTI1)   #apresenta problemas de multicolinearidade
vif(MULTI2)
vif(MULTI3)   #Apresenta problema de multicolinearidade


########-----------------------------------------------------------------------------------########
###################     Bondade do ajustamento e capacidade discriminativa      ###################
########-----------------------------------------------------------------------------------########

(n<-length(dados$Dest_abate2)) 
(R2N<-(1-exp((MULT$dev-MULT$null)/n))/(1-exp(-MULT$null/n)))  #R2 = 30,53


#modelo com interação
(n<-length(dados$Dest_abate2)) 
(R2N<-(1-exp((MULTI2$dev-MULTI2$null)/n))/(1-exp(-MULTI2$null/n)))  #R2 = 31,278

(n<-length(dados$Dest_abate2)) 
(R2N<-(1-exp((MULTI3$dev-MULTI3$null)/n))/(1-exp(-MULTI3$null/n)))  #R2 = 31,86 

## Teste de Hosmer e Lemeshow ## 
library(ResourceSelection)
(hl<-hoslem.test(dados$Dest_abate2, fitted(MULT), g = 10))
hl$expected
hl$observed
#como valor p=0,97 podemos concluir que o modelo se ajusta aos dados



#Teste HL para modelo com interação
(hl<-hoslem.test(dados$Dest_abate2, fitted(MULTI2), g = 10))
hl$expected
hl$observed
#p-valor de 0,37 tb se ajusta aos dados

(hl<-hoslem.test(dados$Dest_abate2, fitted(MULTI3), g = 10))
hl$expected
hl$observed
#p-valor de 0,87 o modelo se ajusta aos dados



#### Capacidade discriminativa: curva ROC ####
library(Epi)
#modelo Sem interação
ROC(form=Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+
            VG_gmd_estacao+VG_carcaca_dia_idadetransf+VG_Intervalo_Entre_partos,
    data=dados, plot="ROC",PV=T,MX=T,AUC=T)
# Modelo com capacidade discriminativa aceitavel AUC=0,778
# Para um ponto de corte igual a 0,46 com sensibilidade de 75,8% e uma especificidade de 65,7%
table(dados$Dest_abate2)
table(dados$Dest_abate)

#matriz de confundimento
Yest<-MULTI1$fitted.values>0.31
table(Yest) #modelo estima
Yobs<-MULTI1$model$Dest_abate2
table(Yobs)  #o que foi observado na pratica
table(Yobs,Yest)
#specificity = 
129/(129+209)
#sensibility= 
343/(343+33)
#Buscar melhor ponto de corte para aumentar a sensibilidade
#ter mais acerto na estimação do animal DOP
#com ponto de corte de 0.31 a sensibilidade vai para 91,22%

perf = function(cut, mod, y)
{
        yhat = (mod$fit>cut)
        w = which(y==1)
        sensitivity = mean( yhat[w] == 1 ) 
        specificity = mean( yhat[-w] == 0 ) 
        c.rate = mean( y==yhat ) 
        out = t(as.matrix(c(sensitivity, specificity)))
        colnames(out) = c("sensibilidade", "especificidade")
        return(out)
}
perf(0.310,MULTI1,dados$Dest_abate2)
perf(0.10,MULTI1,dados$Dest_abate2)
perf(0.50,MULTI1,dados$Dest_abate2)

s = seq(.01,.99,length=1000)
OUT = matrix(0,1000,2)
for(i in 1:1000) OUT[i,]=perf(s[i],MULTI1,dados$Dest_abate2)
plot(s,OUT[,1],xlab="Ponto de corte",ylab="Valor", ylim=c(0,1),type="l", col=2, lwd=2)
lines(s,OUT[,2],col="darkgreen",lwd=2)
legend(0,.25,col=c(2,"darkgreen"),lwd=c(2,2),c("Sensibilidade","Especificdade"), bty="n")


previstos2 <- data.frame(probabilidade=MULTI3$fitted.values, Dest_abate2=dados$Dest_abate2)
# ordenar por ordem crescente de probabilidade o data.frame anterior
previstos2 <- previstos2[order(previstos2$probabilidade, decreasing=FALSE),]
# criar uma coluna que identifica o numero da linha
previstos2$rank <- 1:nrow(previstos2)

# grafico
library(ggplot2)
ggplot(data=previstos2,                                  # conjunto de dados
       aes(x=rank, y=probabilidade)) +                  # variaveis a representar no eixo x e y
        geom_point(aes(color=Dest_abate2),                            # variavel y
                   alpha=1, shape=4, stroke=2) +              # definir o simbolo a usar, e outras caracteristicas do simbolo 
        xlab("Indice")+                                       # titulo do eixo x
        ylab("Prob prevista de ter ser DOP") 
#aparenta ter um aspecto linear, deve ter aspecto logístico


##..........................................##
##### Analise de residuos via individuos #####
# Nota: quando existem variaveis continuas no modelo quase sempre havera quase tantos padroes como covariaveis e esta abordagem pode ser usada

## residuos deviance ##
rd<-residuals (MULTI3, type="deviance") # residuos deviance
plot(fitted(MULTI3), rd, xlab= "Probabilidades estimadas", ylab="Residuos Deviance") 

# distancia de Cook ##
plot(cooks.distance(MULTI3), xlab="Individuos", ylab="Distancia de Cook")
identify(cooks.distance(MULTI3), labels=rownames(dados)) # identificar os pontos que se destacam
library(faraway)
halfnorm(cooks.distance(MULTI3)) 
# Nota: nenhum valor com uma distancia de Cook>0.5

## residuos DfBeta ##
rdf<-dfbetas(MULTI3) # residuos dfbeta
# Representação dos dfbeta vs preditores #
plot(dados$peso_ent, rdf[, 2], xlab="Peso a entrada", ylab="Dfbeta")
plot(dados$Idadeent_meses, rdf[, 3], xlab="Idade a entrada", ylab="Dfbeta")
plot(dados$VG_capacidade_maternal, rdf[, 4], xlab="Capacidade maternal", ylab="Dfbeta")
plot(dados$VG_gmd_estacao, rdf[, 5], xlab="GMD em teste de estação", ylab="Dfbeta")
plot(dados$VG_carcaca_dia_idadetransf, rdf[, 6], xlab="Carcaça por dia de idade transformada", ylab="Dfbeta")
plot(dados$VG_Intervalo_Entre_partos, rdf[, 7], xlab="Intervalo entre partos", ylab="Dfbeta")
# Nota: nenhum valor com um resíduo excessivamente elevado (>1) ou que se destaque

# Representação dos dfbeta vs obs #
dfbetasPlots (MULTI3)

#Leverage
h<-influence (MULTI3)
plot(h$hat) 
abline(h=2*6/189, lty=2)
# Apenas 1 ponto acima do limite não há nenhum excessivamente elevado nem que se destaque 

##########--------------------------------------------------------########
###################            Validação             ###################
########--------------------------------------------------------########

library(rms)
mod<-lrm(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_gmd_estacao+
             VG_carcaca_dia_idadetransf+VG_Intervalo_Entre_partos, 
         data=dados, x=TRUE, y=TRUE )
#aqui ele pede para retirar a interação
print(mod)
resid(mod,"gof")

    ## bootstrap ##
(val.boot<-validate(mod, B=100, bw=TRUE))

## validação cruzada  ##
(val.cross<-validate(mod, method="crossvalidation", B=10))
#retirar gmd comparar o modelo obtido
#AUC
#hosmer
#cessie
#R2
#se não mudar muito os coeficientes retira
#A variável VG_carcaca_dia_idadetransf não se mostrou sig


#modelo obtido 
MULTFINAL<-glm(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+
                VG_Intervalo_Entre_partos, 
            data=dados,family=binomial(link=logit), na.action = na.exclude)
summary(MULTFINAL)
1/0.99
vif(MULTFINAL)
(hl<-hoslem.test(dados$Dest_abate2, fitted(MULTFINAL), g = 10))
rd<-residuals (MULTFINAL, type="deviance") # residuos deviance
plot(fitted(MULTFINAL), rd, xlab= "Probabilidades estimadas", ylab="Resíduos Deviance") 



# distancia de Cook
plot(cooks.distance(MULTFINAL), 
     xlab="Individuos", 
     ylab="Distancia de Cook")
identify(cooks.distance(MULTFINAL)) # identificar os pontos que se destacam
halfnorm(cooks.distance(MULTFINAL))    # grafico seminormal 
# Nota: nenhum valor com uma distancia de Cook>0.5
# representa os residuos studentizados vs leverage e ainda as distâncias de Cook
library(car)   # ativar pacote necessario
influencePlot(MULTFINAL,                                             # modelo ajustado
              id.n = 2,                                         # id.n = n: identifica os n pontos mais influentes em cada uma das 3 medidas
              xlab = "leverage",                                # titulo eixo x
              main = "Pontos influentes",                       # titulo
              sub = "Circulo proporcional da distancia de Cook")	# subtitulo

# identificar as possiveis observacoes influentes
temp<-influence.measures(MULTFINAL)
(lista <- which(apply(temp$is.inf, 1, any)))	 # lista as candidatas a observacoes influentes
summary(temp)  




ROC(form=Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+
        VG_Intervalo_Entre_partos, 
    data=dados, plot="ROC",PV=T,MX=T,AUC=T)
# Modelo com capacidade discriminativa aceitavel AUC=0,75
# Para um ponto de corte igual a 0,49 com sensibilidade de 71,5% e uma especificidade de 69,8%


#matriz de confundimento
Yest<-MULTFINAL$fitted.values>0.35
table(Yest) #modelo estima
Yobs<-MULTFINAL$model$Dest_abate2
table(Yobs)  #o que foi observado na pratica
table(Yobs,Yest)
#specificity = 
212/(212+126)
#sensibility= 
324/(324+52)
# se eu diminuir o ponto de corte para 0,35 eu alcan�o uma sensibilidade de 86%
#E especificidade de 62%

# calibração #
mod<-lrm(Dest_abate2~peso_ent+Idadeent_meses+VG_capacidade_maternal+
             VG_Intervalo_Entre_partos, 
         data=dados, x=TRUE, y=TRUE )
#aqui ele pede para retirar a interação
print(mod)
resid(mod,"gof")
a<-calibrate(mod, method="boot", B=150)
plot(a)



########------------------------------------------------------------########
###################            Interpretação             ###################
########------------------------------------------------------------########


# OR e intervalos de confiança baseados na verosimilhança de perfil #
cbind(exp(coef(MULTFINAL)), exp(confint(MULTFINAL)))
exp(coef(MULTFINAL))
exp(confint(MULTFINAL))

# Representação dos OR com IC: so deve ser usada em modelos sem interação #
source("sjPlotOdds.R")
plotOdds(MULTFINAL, axisLimits=c(0, 3.5), gridBreaksAt=2)

library(fBasics)
basicStats(dados$peso_ent)
basicStats(dados$Idadeent_meses)
basicStats(dados$VG_capacidade_maternal)
basicStats(dados$VG_Intervalo_Entre_partos)
mean(dados$Diasengorda)
mean(dados$Idadeent_meses)
mean(dados$VG_capacidade_maternal)
mean(dados$VG_Intervalo_Entre_partos)


#######################################################################
############Gráfico probabilidades##################################

MULTFINAL


library(ggplot2)
grafico <- ggplot(dados, aes(x=dados$Idadeent_meses, y=dados$Dest_abate2)) + geom_point() + 
    geom_line(aes(x=dados$Idadeent_meses, y=MULTFINAL$fitted))
grafico

grafico2 <- ggplot(dados, aes(x=dados$peso_ent, y=dados$Dest_abate2)) + geom_point() + 
    geom_line(aes(x=dados$peso_ent, y=MULTFINAL$fitted), col="red")
grafico2


idade <- seq (6, 13, 1) 
pred1 <- predict (MULTFINAL, 
                  data.frame (peso_ent=174.6, 
                              Idadeent_meses=idade, VG_capacidade_maternal=-0.38,
                              VG_Intervalo_Entre_partos=-5.5,  type="response"))

plot (idade, pred1, type="l", main="", xlab="x", ylim=c(0,1), 
      ylab="Probabilidade estimada ", col="black")      

summary(MULTFINAL$model$peso_ent)
summary(MULTFINAL$model$Idadeent_meses)
summary(MULTFINAL$model$VG_capacidade_maternal)
summary(MULTFINAL$model$VG_Intervalo_Entre_partos)

peso <- seq (180, 240, 20) 
pred1 <- predict (MULTFINAL, 
                  data.frame (peso_ent=peso, 
                              Idadeent_meses=7.94, VG_capacidade_maternal=-0.38,
                              VG_Intervalo_Entre_partos=-5.6,  type="response"))

plot (peso, pred1, type="l", main="", xlab="x", ylim=c(0,1), 
      ylab="Probabilidade estimada ", col="black")   





(prob.est <- as.vector(tapply(fitted(MULTFINAL))))
plot(prob.est,                                     # variavel a representar 
     main = "Probabilidade estimada por padrão",   # titulo geral
     xlab = "Padrão",                              # titulo eixo x
     ylab = "Probabilidade estimada")  

 min(dados$peso_ent)
# grafico para aumentos de peso #
(a <- 70:175)
OR_DOP <- exp(a*coef(MULTFINAL)[2])       # OR para um aumentos de a libras em lwt
ICbeta <- confint.default(MULTFINAL)[2,]  # IC beta4
liOR_DOP <- exp(a*ICbeta[1])        # limite inferior do IC a 95% para OR para aumentos de a libras em lwt
lsOR_DOP <- exp(a*ICbeta[2])        # limite inferior do IC a 95% para OR para aumentos de a libras em lwt
plot(a,                               # valores do eixo x (os aumentos)
     OR_DOP,                         # valores do eixo y (os OR)
     main="",                         # titulo do grafico
     xlab="Aumento do peso",          # legenda do eixo x
     ylab="OR",                       # legenda do eixo y
     lty=1,                           # tipo de linha (1=continua)

         type="l") 

coef(MULTFINAL)[2]
coef(MULTFINAL)[1]
coef(MULTFINAL)[3]
exp(coef(MULTFINAL)[1])
exp(coef(MULTFINAL)[2])
ICbeta[1]
ICbeta[2]


(a <- 100:240)
OR_DOP <- exp(a*coef(MULTFINAL)[2])       # OR para um aumentos de a peso em y
ICbeta <- confint.default(MULTFINAL)[2,]  # IC beta2
liOR_DOP <- exp(a*ICbeta[1])        # limite inferior do IC a 95% para OR para aumentos de a libras em lwt
lsOR_DOP <- exp(a*ICbeta[2])        # limite superior do IC a 95% para OR para aumentos de a libras em lwt

plot(a,                               # valores do eixo x (os aumentos)
     OR_DOP,                         # valores do eixo y (os OR)
     main="",                         # titulo do grafico
     xlab="Aumento do peso",          # legenda do eixo x
     ylab="OR",                       # legenda do eixo y
     lty=1,                           # tipo de linha (1=continua)
     type="l") 


min(dados$Idadeent_meses)
(a <- 6:15)
OR_DOP <- exp(a*coef(MULTFINAL)[3])       # OR para um aumentos da idade
ICbeta <- confint.default(MULTFINAL)[3,]  # IC beta2
liOR_DOP <- exp(a*ICbeta[1])        # limite inferior do IC a 95% para OR para aumentos de a libras em lwt
lsOR_DOP <- exp(a*ICbeta[2])        # limite superior do IC a 95% para OR para aumentos de a libras em lwt

plot(a,                               # valores do eixo x (os aumentos)
     OR_DOP,                         # valores do eixo y (os OR)
     main="",                         # titulo do grafico
     xlab="Aumento da dade",          # legenda do eixo x
     ylab="OR",                       # legenda do eixo y
     lty=1,                           # tipo de linha (1=continua)
     type="l") 



# -- Estimação da probabilidade para um dado perfil
 
library(faraway)
x0 <- c(1, 175, 7, -0.38, -5.55)           # valores dos coeficientes para o perfil pretendido
eta0 <- sum(x0*coef(MULTFINAL))         # calculo do logit
ilogit(eta0)                       # probabilidade estimada
(cm <- summary(MULTFINAL)$cov.unscaled) # matriz de variancias e covariancias
se <- sqrt(t(x0) %*% cm %*% x0)    # desvio padrao associado aos dados do perfil
# IC a 95% para a probabilidade estimada
ilogit(c(eta0-qnorm(0.975)*se,     # limite inferior
         eta0+qnorm(0.975)*se))    # limite superior

x0 <- c(1, 175, 9, -0.38, -5.55)           # valores dos coeficientes para o perfil pretendido
eta0 <- sum(x0*coef(MULTFINAL))         # calculo do logit
ilogit(eta0)                       # probabilidade estimada
(cm <- summary(MULTFINAL)$cov.unscaled) # matriz de variancias e covariancias
se <- sqrt(t(x0) %*% cm %*% x0)    # desvio padrao associado aos dados do perfil
# IC a 95% para a probabilidade estimada
ilogit(c(eta0-qnorm(0.975)*se,     # limite inferior
         eta0+qnorm(0.975)*se))    # limite superior

x0 <- c(1, 200, 7, -0.38, -5.55)           # valores dos coeficientes para o perfil pretendido
eta0 <- sum(x0*coef(MULTFINAL))         # calculo do logit
ilogit(eta0)                       # probabilidade estimada
(cm <- summary(MULTFINAL)$cov.unscaled) # matriz de variancias e covariancias
se <- sqrt(t(x0) %*% cm %*% x0)    # desvio padrao associado aos dados do perfil
# IC a 95% para a probabilidade estimada
ilogit(c(eta0-qnorm(0.975)*se,     # limite inferior
         eta0+qnorm(0.975)*se))    # limite superior

table(dados$Dest_abate2)
# Extrair os padroes de covariaveis
(modelo.cp <- epi.cp(dados[c("peso_ent","Idadeent_meses","VG_capacidade_maternal","VG_Intervalo_Entre_partos")]))
str(modelo.cp)
# Neste modelo temos 132 padroes e 189 individuos
(prob.est <- as.vector(tapply(fitted(MULTFINAL), as.factor(modelo.cp$id), min)))
plot(prob.est,                                     # variavel a representar 
     main = "Probabilidade estimada por padrão",   # titulo geral
     xlab = "Padrão",                              # titulo eixo x
     ylab = "Probabilidade estimada")  

# vamos considerar: fumadora vs. nao fumadora, sendo mulher branca, para pesos peso entre 80 e 250
pesograf <- seq(170, 250, 1)                                             # valores do peso
# probabilidade prevista 
pred.no <- predict(MULTFINAL, 
                   data.frame(peso_ent=pesograf, Idadeent_meses=7, VG_capacidade_maternal=-0.38, VG_Intervalo_Entre_partos = -5.55),  # conjunto de dados
                   type="response")                                 # probabilidade prevista
plot (pesograf, pred.no, 
      type="l", ylim=c(0,1), 
      xlab="Weight at entry", 
      ylab="Estimate probability", 
      col="chocolate")

idadegraf <- seq(7, 15, 1)                                             # valores do peso
# probabilidade prevista 
pred.no <- predict(MULTFINAL, 
                   data.frame(peso_ent=175, Idadeent_meses=idadegraf, VG_capacidade_maternal=-0.38, VG_Intervalo_Entre_partos = -5.55),  # conjunto de dados
                   type="response")                                 # probabilidade prevista
plot (idadegraf, pred.no, 
      type="l", ylim=c(0,1), 
      xlab="Age at entry", 
      ylab="Estimate probability", 
      col="chocolate")

# dados com o perfil pretendido
newdata1 <- with(dados, 
                 data.frame(peso_ent=170, 
                            Idadeent_meses=rep(seq(from = 7, to = 15, length.out = 15),2), VG_capacidade_maternal=-0.38, VG_Intervalo_Entre_partos=5.55))
# juntar aos dados anteriores as probabilidades previstas
newdata2 <- cbind(newdata1, 
                  predict(MULTFINAL, newdata = newdata1, type = "link",se = TRUE))
newdata3 <- within(newdata2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - qnorm(0.975) * se.fit)
  UL <- plogis(fit + qnorm(0.975) * se.fit)
})
MULTFINAL
ggplot(newdata3, aes(x = Idadeent_meses, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL,ymax = UL, fill = peso_ent), alpha = 0.2) + 
  geom_line(aes(colour = VG_capacidade_maternal ), size = 1) +
   xlab("Age at entry") +            # titulo do eixo x
  ylab("Expected probability")      # titulo do eixo y

