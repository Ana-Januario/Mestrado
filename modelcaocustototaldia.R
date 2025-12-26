###########################################################################
#########MODELAçaO RLM CUSTO TOTAL POR DIA################################
###########################################################################
getwd()
library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=T, sep=";", dec=",")
dados$totalpordia<-(dados$total_custos)/(dados$Diasengorda)
mean(dados$Custototal_dia)
boxplot(dados$Custototal_dia)
library(fBasics)
basicStats(dados$Custototal_dia)
hist(dados$Custototal_dia)

dadosDOP=read.table("bov_mais_DOP.csv", header=T, sep=";", dec=",")
mean(dadosDOP$Custototal_dia)
sd(dadosDOP$Custototal_dia)

dadosCONV=read.table("bov_mais_CONV.csv", header=T, sep=";", dec=",")
mean(dadosCONV$Custototal_dia)
sd(dadosCONV$Custototal_dia)

library(fBasics)

shapiro.test(dados$custototal.dia)
lillieTest(dados$custototal.dia)
#a variavel passa na normalidade


#custo fixos e custos variaveis

dados$custosfixos<-(dados$profilaxia+dados$servico_Promert+dados$custo_individual_transporte)
dados$custoalimentacaopordia<-(dados$custodepalha.dia+dados$custodeconcentrado.dia)
dados$custosvariaveis<-(dados$custoalimentacaopordia+dados$custofunc_dia+dados$outroscustos.dia)

modcustopordia1<- lm(dados$custototal.dia~dados$custosfixos+dados$custosvariaveis)
summary(modcustopordia1)

##MODELO SIMPLES####
modcusto1<-lm(dados$custototal.dia~dados$peso_ent)
summary(modcusto1) #R2adj=0,41
#Mantendo tudo constante, quando aumento em 1 Kg o peso do animal na entrada, o custo por dia aumenta em 0,004 euros

modcusto2<-lm(dados$custototal.dia~dados$Idadeent_meses)
summary(modcusto2) #R2adj=0,03
#Mantendo tudo constante, quando aumento em 1 mes da idade na entrada, o custo por dia aumenta em 0,03 euros

modcusto3<-lm(dados$custototal.dia~dados$P210)
summary(modcusto3) #R2adj=0,15
#Mantendo tudo constante, quando aumento em 1 unidade on P210, o custo aumenta em 0,003 euros

modcusto4<-lm(dados$custototal.dia~dados$Pai_p210)
summary(modcusto4) #R2adj=0,00
#a variavel nao apresentou significancia

modcusto5<-lm(dados$custototal.dia~dados$mae_p210)
summary(modcusto5) #R?adj=0,01
#n?o apresentou significancia

modcusto6<-lm(dados$custototal.dia~dados$VG_capacidade_maternal)
summary(modcusto6) #R2adj=0,04
#significancia

modcusto7<-lm(dados$custototal.dia~dados$VG_cap_crescimento)
summary(modcusto7) #R2adj=0,007
#significancia a 1%

modcusto8<-lm(dados$custototal.dia~dados$VG_gmd_estacao)
summary(modcusto8) #R2adj=0,0000
#n?o apresentou significancia

modcusto9<-lm(dados$custototal.dia~dados$VG_carcaca_dia_idade)
summary(modcusto9) #R2adj=0,03
#Mantendo tudo constante, quando aumento em 1 unidade o VG da carc dia idade, o custo por dia aumenta em 0,005euros

modcusto10<-lm(dados$custototal.dia~dados$VG_Intervalo_Entre_partos)
summary(modcusto10) #R2adj=0,00
#nao apresentou significancia

modcusto11<-lm(dados$custototal.dia~dados$VG_indice_conversao)
summary(modcusto11) #R2adj=0,001
#Nao apresentou significancia

modcusto12<-lm(dados$custototal.dia~dados$VG_longevidade_produtiva)
summary(modcusto12) #R2adj=0,001
#nao apresentou significancia

modcusto13<-lm(dados$custototal.dia~dados$VG_consumo_alim_residual)
summary(modcusto13) #R2adj=0,001
#nao apresentou significancia

modcusto14<-lm(dados$custototal.dia~dados$GMD)
summary(modcusto14)


########################################################################################
#################RLM custo total de producao/dia na engorda############################

modcustodiaM1<-lm(custototal.dia~peso_ent+Idadeent_meses+P210+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade, data = dados)
summary(modcustodiaM1)
anova(modcustodiaM1)

#Retiro p210
modcustodiaM2<-lm(custototal.dia~peso_ent+Idadeent_meses+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade, data = dados)
summary(modcustodiaM2)
#idade deixou de apresentar significancia

#refazendo o modelo com as demais variaveis sem idade a entrada
modcustodiaM3<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade, data = dados)
summary(modcustodiaM3)

#Colocando as variaveis que n?o apresentaram significancia na fase univariada
modcustodiaM5<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+Pai_p210, data = dados)
summary(modcustodiaM5) #nao

modcustodiaM6<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+mae_p210, data = dados)
summary(modcustodiaM6) #nao

modcustodiaM8<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_gmd_estacao, data = dados)
summary(modcustodiaM8) #nao altera o R2

modcustodiaM9<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data = dados)
summary(modcustodiaM9)

modcustodiaM10<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_gmd_estacao+VG_Intervalo_Entre_partos+VG_indice_conversao, data = dados)
summary(modcustodiaM10)
modcustodiaM10<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao, data = dados)
summary(modcustodiaM10)

modcustodiaM11<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+VG_longevidade_produtiva, data = dados)
summary(modcustodiaM11) #nao

modcustodiaM12<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+VG_consumo_alim_residual, data = dados)
summary(modcustodiaM12) #nao


#####################TESTANDO AS INTERAcoES
modcustodiaI1<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+peso_ent*VG_capacidade_maternal, data = dados)
summary(modcustodiaI1)
#Nao apresentou significancia

modcustodiaI2<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+peso_ent*VG_cap_crescimento, data = dados)
summary(modcustodiaI2)
#n?o

modcustodiaI3<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+peso_ent*VG_carcaca_dia_idade, data = dados)
summary(modcustodiaI3)
#nao

modcustodiaI4<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+peso_ent*VG_Intervalo_Entre_partos, data = dados)
summary(modcustodiaI4)
#embora seja muito signif altera em apenas 1% o R2

modcustodiaI5<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+peso_ent*VG_indice_conversao, data = dados)
summary(modcustodiaI5)
#nao

modcustodiaI6<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+VG_capacidade_maternal*VG_cap_crescimento, data = dados)
summary(modcustodiaI6)
#nao

modcustodiaI7<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+VG_capacidade_maternal*VG_cap_crescimento, data = dados)
summary(modcustodiaI7)
#nao

modcustodiaI8<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+VG_capacidade_maternal*VG_carcaca_dia_idade, data = dados)
summary(modcustodiaI8)
#nao

modcustodiaI9<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+VG_capacidade_maternal*VG_Intervalo_Entre_partos, data = dados)
summary(modcustodiaI9)#nao

modcustodiaI10<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+VG_capacidade_maternal*VG_indice_conversao, data = dados)
summary(modcustodiaI10)#nao

modcustodiaI11<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+VG_cap_crescimento*VG_carcaca_dia_idade, data = dados)
summary(modcustodiaI11)
#nao
modcustodiaI12<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+VG_cap_crescimento*VG_Intervalo_Entre_partos, data = dados)
summary(modcustodiaI12) #nao


modcustodiaI13<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+VG_cap_crescimento*VG_indice_conversao, data = dados)
summary(modcustodiaI13)
#nao explica

modcustodiaI14<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+VG_carcaca_dia_idade*VG_Intervalo_Entre_partos, data = dados)
summary(modcustodiaI14)
#nao

modcustodiaI15<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+VG_carcaca_dia_idade*VG_indice_conversao, data = dados)
summary(modcustodiaI15)
#nao explica

modcustodiaI16<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao+VG_Intervalo_Entre_partos*VG_indice_conversao, data = dados)
summary(modcustodiaI16)
#embora seja muito significativa nao altera R2

#modelo sem interacao selecionado
plot(modcustodiaM10) 
#e possivel notar alguns outliers: 2,3,348

####ANALISE DOS RESIDUOS DO MODELO

#Analise dos resíduos
resid<-resid(modcustodiaM10) # residuos do Modelo multivariado
pred<-fitted(modcustodiaM10) # Valores ajustados pelo modelo
resid.std <- rstandard(modcustodiaM10)

library(nortest)
lillie.test(resid) #admite-se a normalidade
qqnorm(resid)
qqline(resid)#muitos pontos outliers
identify(qqnorm(resid)) #2,3,265,267,268,348
# Histograma dos residuos
hist(resid)  #nao aparentam ter distr Normal

# Avaliar Achatamento
library(moments)
# teste para a curtose
anscombe.test(resid) #os residuos nao sao mesocurticos
# teste para a Assimetria
agostino.test(resid)  #Assimetricos

library(car)
##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustodiaM10)
#

library(lmtest)
# Breusch- Pagan test
bptest(modcustodiaM10) #p-value = 0.06
#pode-se assumir a homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaM10) # variance inflation factors 
sqrt(vif(modcustodiaM10)) > 2 # Se verdade existe multicolinearidade
#nao apresenta problema de multicolinearidade

################################################################
dcook<-cooks.distance(modcustodiaM10)
names(dcook)<-1:length(dcook)
plot(dcook,ylim=c(0,1.1))
abline(h=1.0,col="red")
identify(dcook)

plot(resid.std, ylab = "res?duos standartizados", main = "Análise dos resíduos")
identify(resid.std)
plot(pred,resid)
identify(pred,resid)

#antes de verificar os valores influentes, vou verificar se pelo metodo stepwise consigo um modelo melhor



######################################################################
###########VERIFICAçãO DOS VALORES INFLUENTES#########################

# vamos fazer um teste T para ver quais as observacoes sao consideradas outliers
pvalue<-2*(1-pt(abs(resid.std),length(resid.std)))

# Devolve os outliers ao nivel 1%
names(pvalue)<-1:length(resid.std)
pvalue[pvalue<0.01]

#antes de retirar esses outlier vou verificar os valores influentes
#### Valores Influentes
lev<-hatvalues(modcustodiaM10)
lev
#como sao muitos valores influentes
names(lev)<-1:length(lev)
plot(pred,lev,abline(h=0.2))
plot(pred,lev,ylim=c(0,0.2),abline(h=0.2))

#nenhum ultrapassa o limite indicado
dcook<-cooks.distance(modcustodiaM10)
names(dcook)<-1:length(dcook)
plot(dcook,ylim=c(0,1.1))
abline(h=1.0,col="red")
identify(dcook)
#nenhum ultrapassa o limite sugerido pela distancia de cooks
#sera retirado os animais que apresentaram-se outliers na analise grafica
dados2<-dados[-c(2,3,263,264,265,266,267,268,333,338,348,363,710),]
plot(dados2$custototal.dia)
hist(dados2$custototal.dia,main = "Histograma custo total/dia na engorda sem outliers", xlab = "Custo total/dia", ylab = "Frequncia") #apresentou um histogram mais bem comportado

modcustodiaM10<-lm(custototal.dia~peso_ent+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos+VG_indice_conversao, data = dados2)
summary(modcustodiaM10)

#Analise dos residuos
resid<-resid(modcustodiaM10) # residuos do Modelo multivariado
pred<-fitted(modcustodiaM10) # Valores ajustados pelo modelo
resid.std <- rstandard(modcustodiaM10)

lillie.test(resid) #admite-se a normalidade
qqnorm(resid)
qqline(resid)#muitos pontos outliers
# Histograma dos residuos
hist(resid)  #n?o aparentam ter distr Normal

# Avaliar Achatamento
# teste para a curtose
anscombe.test(resid) #os res?duos s?o mesocurticos
# teste para a Assimetria
agostino.test(resid)  #Assimetricos

library(car)
##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modcustodiaM10)
#

# Breusch- Pagan test
bptest(modcustodiaM10) #p-value = 0.06
bptest(modcustodiaM10, varformula = ~ fitted.values(modcustodiaM10), studentize = TRUE)

#pode-se assumir a homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modcustodiaM10) # variance inflation factors 
sqrt(vif(modcustodiaM10)) > 2 # Se verdade existe multicolinearidade
#n?o apresenta problema de multicolinearidade
