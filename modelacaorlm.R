######################RLM####################################
###################MODELO LUCRO LIQUIDO######################


getwd()

library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=T, sep=";", dec=",")

library(fBasics)

lillieTest(dados$liquido_recria)
shapiro.test(dados$liquido_recria)

##MODELO SIMPLES####
modlucro1<-lm(dados$liquido_recria~dados$peso_ent)
summary(modlucro1) #R2adj=0,39
modlucro1$coefficients
#Mantendo tudo constante, quando aumento em 1 Kg o peso do animal na entrada, o liquido da recria aumenta em 1,31 euros
plot(dados$liquido_recria, dados$peso_ent, abline(reg=modlucro1,col="blue"),col="red")

modlucro2<-lm(dados$liquido_recria~dados$Idadeent_meses)
summary(modlucro2) #R2adj=0,10
modlucro2$coefficients
#Mantendo tudo constante, quando aumento em 1 mes da idade na entrada, o liquido da recria aumenta em 19,30 euros
plot(dados$liquido_recria, dados$Idadeent_meses, abline(reg=modlucro2,col="blue"),col="red")


modlucro3<-lm(dados$liquido_recria~dados$P210)
summary(modlucro3) #R2adj=0.1541
modlucro3$coefficients
#Mantendo tudo constante, quando aumento em 1 unidade on P210, o liquido da recria aumenta em 1,19 euros

modlucro4<-lm(dados$liquido_recria~dados$Pai_p210)
summary(modlucro4) #R2adj=0,00
#a variavel não apresentou significancia

modlucro5<-lm(dados$liquido_recria~dados$mae_p210)
summary(modlucro5) #R2adj=0,01
#nao apresentou significancia

modlucro6<-lm(dados$liquido_recria~dados$VG_capacidade_maternal)
summary(modlucro6) #R2adj=0,01
#nao apresentou significancia

modlucro7<-lm(dados$liquido_recria~dados$VG_cap_crescimento)
summary(modlucro7) #R2adj=0,02
modlucro7$coefficients
#Mantendo tudo constante, quando aumento em 1 unidade o VG da cap crescimento, o liquido da recria aumenta em 1,25 euros
plot(dados$liquido_recria, dados$VG_cap_crescimento, abline(reg=modlucro7,col="blue"),col="red", xlab="Liquido recria", ylab="Valor gen�tico")

modlucro8<-lm(dados$liquido_recria~dados$VG_gmd_estacao)
summary(modlucro8) #R2adj=0,0000
#nao apresentou significancia

modlucro9<-lm(dados$liquido_recria~dados$VG_carcaca_dia_idade)
summary(modlucro9) #R2adj=0,08
modlucro9$coefficients
#Mantendo tudo constante, quando aumento em 1 unidade o VG da carc dia idade, o liquido da recria aumenta em 1,08 euros
plot(dados$liquido_recria, dados$VG_carcaca_dia_idade, abline(reg=modlucro9,col="blue"),col="red")


modlucro10<-lm(dados$liquido_recria~dados$VG_Intervalo_Entre_partos)
summary(modlucro10) #R2adj=0,003
modlucro10$coefficients
#nao apresentou significancia

modlucro11<-lm(dados$liquido_recria~dados$VG_indice_conversao)
summary(modlucro11) #R2adj=0,004
modlucro11$coefficients
#Mantendo tudo constante, quando aumento em 1 unidade o VG do indice de conversao, o liquido da recria aumenta reduz em 36 euros
plot(dados$liquido_recria, dados$VG_indice_conversao, abline(reg=modlucro11,col="blue"),col="red",xlab="Liquido recria", ylab="Valor gen�tico")

modlucro12<-lm(dados$liquido_recria~dados$VG_longevidade_produtiva)
summary(modlucro12) #R2adj=0,001
#nao apresentou significancia

modlucro13<-lm(dados$liquido_recria~dados$VG_consumo_alim_residual)
summary(modlucro13) #R2adj=0,001
#nao apresentou significancia

modlucro14<-lm(dados$liquido_recria~dados$Dest_abate)
summary(modlucro14)

#####################MULTIVARIADO####################3
modlucroM1<-lm(liquido_recria~peso_ent+Idadeent_meses+P210+VG_cap_crescimento+VG_carcaca_dia_idade+VG_indice_conversao+Dest_abate, data = dados)
anova(modlucroM1)
summary(modlucroM1)

#novo modelo RLM vou retirar idade a entrada, pois já era necessario retirar
#pu fica com o peso oiu fica com a idade
#e o VG do indice de conversão alimentar, pois não apresentou significancia
modlucroM2<-lm(liquido_recria~peso_ent+P210+VG_cap_crescimento+VG_carcaca_dia_idade+Dest_abate, data = dados)
summary(modlucroM2)
anova(modlucroM2)
#Como P210 não apresentou significancia na anova será realizado um terceiro modelo sem P210
modlucroM3<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+Dest_abate, data = dados)
summary(modlucroM3)
anova(modlucroM3)
#Esse modelo se mostrou bom

#Verificando as variáveis que não apresentaram significancia no RLM simples
modlucroM3a<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+Dest_abate+Pai_p210, data = dados)
summary(modlucroM3a)
anova(modlucroM3a)
#P210_pai não é significativo
modlucroM3b<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+mae_p210, data = dados)
summary(modlucroM3b)
anova(modlucroM3b)
#P210_mae não é significativo

modlucroM3c<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_capacidade_maternal, data = dados)
summary(modlucroM3c)
anova(modlucroM3c)
#VG capacidade maternal, não é significativo

modlucroM3d<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_gmd_estacao, data = dados)
summary(modlucroM3d)
anova(modlucroM3d)
#VG gmd em teste de estação não é significativo

modlucroM3e<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_Intervalo_Entre_partos, data = dados)
summary(modlucroM3e)
anova(modlucroM3e)
#VG intervalo entre partos não é significativo


modlucroM3g<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_consumo_alim_residual, data = dados)
summary(modlucroM3g)
anova(modlucroM3g)
#não se apresentou significativo



modlucroM3f<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate, data = dados)
summary(modlucroM3f)
anova(modlucroM3f)
coef(modlucroM3f)
# No modelo final o destino de abate se apresentou significativo

############################
#Verificando as interações
modlucroM3fint1<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate+peso_ent*VG_cap_crescimento, data = dados)
summary(modlucroM3fint1)
anova(modlucroM3fint1)

modlucroM3fint2<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate+peso_ent*VG_carcaca_dia_idade, data = dados)
summary(modlucroM3fint2)
anova(modlucroM3fint2)

modlucroM3fint3<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate+peso_ent*VG_longevidade_produtiva, data = dados)
summary(modlucroM3fint3)
anova(modlucroM3fint3)

modlucroM3fint3.1<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate+peso_ent*Dest_abate, data = dados)
summary(modlucroM3fint3.1)
anova(modlucroM3fint3.1)

modlucroM3fint4<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate+VG_cap_crescimento*VG_carcaca_dia_idade, data = dados)
summary(modlucroM3fint4)
anova(modlucroM3fint4)
#VG cap crescimento e VG carcaça por dia de idade apresentaram interação com significancia

modlucroM3fint5<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate+VG_cap_crescimento*VG_longevidade_produtiva, data = dados)
summary(modlucroM3fint5)
anova(modlucroM3fint5)

modlucroM3fint5.1<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate+VG_cap_crescimento*Dest_abate, data = dados)
summary(modlucroM3fint5.1)
anova(modlucroM3fint5.1)
#Capacidade de crescimento e destino de abate apresentaram significancia

modlucroM3fint6<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate+VG_carcaca_dia_idade*VG_longevidade_produtiva, data = dados)
summary(modlucroM3fint6)
anova(modlucroM3fint6)

modlucroM3fint7<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate+VG_longevidade_produtiva*Dest_abate, data = dados)
summary(modlucroM3fint7)
anova(modlucroM3fint7)


modlucroM3fint8<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate+VG_cap_crescimento*VG_carcaca_dia_idade+VG_cap_crescimento*Dest_abate, data = dados)
summary(modlucroM3fint8)
anova(modlucroM3fint8)

#Duas interações foram significativas
#Entre = VG_cap_crescimento*Dest_abate
#VG_cap_crescimento*VG_carcaca_dia_idade

########################################################################
#Analise dos resíduos
resid<-resid(modlucroM3fint8) # residuos do Modelo multivariado
pred<-fitted(modlucroM3fint8) # Valores ajustados pelo modelo
resid.std <- rstandard(modlucroM3fint8)

library(nortest)
lillie.test(resid) #não admite-se a normalidade
shapiro.test(resid)
qqnorm(resid)
qqline(resid) #muitos pontos outliers
# Histograma dos residuos
hist(resid)  # aparentam ter distr Normal

# Avaliar Achatamento
library(moments)
# teste para a curtose
anscombe.test(resid) #os resíduos não são mesocurticos
# teste para a Assimetria
agostino.test(resid)  #Memso considerando a simetria, falha a normalidade

library(car)
##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modlucroM3fint8)
#Não se pode admitir independencia dos resíduos

install.packages("lmtest")
library(lmtest)
# Breusch- Pagan test
bptest(modlucroM3fint8) #p-value = 1.672e-15
#Não se pode assumir a homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modlucroM3fint8) # variance inflation factors 
sqrt(vif(modlucroM3fint8)) > 2 # Se verdade existe multicolinearidade
#não apresenta problema de multicolinearidade
#Mas não passa na normalidade

#transformações
y<-log(dados$liquido_recria)
hist(y)
modlucroM3fint8a<-lm(y~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate+VG_cap_crescimento*VG_carcaca_dia_idade+VG_cap_crescimento*Dest_abate, data = dados)
#summary(modlucroM3fint8a)
#anova(modlucroM3fint8a)
#resida<-resid(modlucroM3fint8a)
#lillie.test(resida) #não passou na normalidade

y<-sqrt(dados$liquido_recria)
hist(y)
modlucroM3fint8b<-lm(y~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate+VG_cap_crescimento*VG_carcaca_dia_idade+VG_cap_crescimento*Dest_abate, data = dados)
#summary(modlucroM3fint8b)
#anova(modlucroM3fint8b)
#residb<-resid(modlucroM3fint8b)
#lillie.test(residb)

y<-(1/dados$liquido_recria)
hist(y)
modlucroM3fint8c<-lm(y~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate+VG_cap_crescimento*VG_carcaca_dia_idade+VG_cap_crescimento*Dest_abate, data = dados)
#summary(modlucroM3fint8c)
#anova(modlucroM3fint8c)
#residc<-resid(modlucroM3fint8c)
#lillie.test(residc)

#nenhuma transformação se mostrou valida para tornar os resíduos normais

################################################################
################################################################
#PARA TENTAR VERIFICAR SE EXISTE ALGUM MODELO MELHOR, TENTOU-SE 
#o metodo stepwise
library(MASS)
modlucrototal<-lm(liquido_recria~peso_ent+Idadeent_meses+P210+Pai_p210+mae_p210+VG_capacidade_maternal+VG_cap_crescimento+VG_carcaca_dia_idade+VG_gmd_estacao+VG_consumo_alim_residual+VG_indice_conversao+VG_Intervalo_Entre_partos+VG_longevidade_produtiva+Dest_abate, data = dados)
step <- stepAIC(modlucrototal, direction="forward") #metodo de selecao das variaveis backward, forward, both com base no AIC
step$anova
anova(modlucrototal)
summary(modlucrototal)

modlucroM6<-lm(liquido_recria~Idadeent_meses+P210+VG_cap_crescimento+VG_gmd_estacao+VG_Intervalo_Entre_partos+VG_carcaca_dia_idade+Dest_abate, data = dados)
summary(modlucroM6)
anova(modlucroM6)
#O destino de abate e a capacidade de crescimento não são significativos aqui

modlucroM6a<-lm(liquido_recria~Idadeent_meses+P210+VG_gmd_estacao+VG_Intervalo_Entre_partos+VG_carcaca_dia_idade, data = dados)
summary(modlucroM6a)
anova(modlucroM6a)

#Verificação das interações
modlucroM7<-lm(liquido_recria~Idadeent_meses+P210+VG_gmd_estacao+VG_Intervalo_Entre_partos+VG_carcaca_dia_idade+Idadeent_meses*P210, data = dados)
summary(modlucroM7)
anova(modlucroM7)
#a idade a entrada e o P210 apresentam interação significativa
modlucroM8<-lm(liquido_recria~Idadeent_meses+P210+VG_gmd_estacao+VG_Intervalo_Entre_partos+VG_carcaca_dia_idade+Idadeent_meses*P210+Idadeent_meses*VG_gmd_estacao, data = dados)
summary(modlucroM8)
anova(modlucroM8)
#A idade a entrada e o VG GMD estão não são uma interação significativa

modlucroM9<-lm(liquido_recria~Idadeent_meses+P210+VG_gmd_estacao+VG_Intervalo_Entre_partos+VG_carcaca_dia_idade+Idadeent_meses*P210+Idadeent_meses*VG_Intervalo_Entre_partos, data = dados)
summary(modlucroM9)
anova(modlucroM9)
# a idade a entrada e o VG intervalo entre partos tb não apresentam interação significativa

modlucroM10<-lm(liquido_recria~Idadeent_meses+P210+VG_gmd_estacao+VG_Intervalo_Entre_partos+VG_carcaca_dia_idade+Idadeent_meses*P210+Idadeent_meses*VG_carcaca_dia_idade, data = dados)
summary(modlucroM10)
anova(modlucroM10)
#Idade a entrada e o VG_carcaca_dia_idade apresentam interação significativa

modlucroM11<-lm(liquido_recria~Idadeent_meses+P210+VG_gmd_estacao+VG_Intervalo_Entre_partos+VG_carcaca_dia_idade+Idadeent_meses*P210+Idadeent_meses*VG_carcaca_dia_idade+P210*VG_gmd_estacao, data = dados)
summary(modlucroM11)
anova(modlucroM11)
#P210 e VG GMD estão é significativo

modlucroM12<-lm(liquido_recria~Idadeent_meses+P210+VG_gmd_estacao+VG_Intervalo_Entre_partos+VG_carcaca_dia_idade+Idadeent_meses*P210+Idadeent_meses*VG_carcaca_dia_idade+P210*VG_gmd_estacao+P210*VG_Intervalo_Entre_partos, data = dados)
summary(modlucroM12)
anova(modlucroM12)
#P210 e VG_Intervalo_Entre_partos não tem significancia

modlucroM13<-lm(liquido_recria~Idadeent_meses+P210+VG_gmd_estacao+VG_Intervalo_Entre_partos+VG_carcaca_dia_idade+Idadeent_meses*P210+Idadeent_meses*VG_carcaca_dia_idade+P210*VG_gmd_estacao+P210*VG_carcaca_dia_idade, data = dados)
summary(modlucroM13)
anova(modlucroM13)


modlucroM14<-lm(liquido_recria~Idadeent_meses+P210+VG_gmd_estacao+VG_Intervalo_Entre_partos+VG_carcaca_dia_idade+Idadeent_meses*P210+Idadeent_meses*VG_carcaca_dia_idade+P210*VG_gmd_estacao+P210*VG_carcaca_dia_idade+VG_gmd_estacao*VG_Intervalo_Entre_partos, data = dados)
summary(modlucroM14)
anova(modlucroM14)
#N�o significativa

modlucroM15<-lm(liquido_recria~Idadeent_meses+P210+VG_gmd_estacao+VG_Intervalo_Entre_partos+VG_carcaca_dia_idade+Idadeent_meses*P210+Idadeent_meses*VG_carcaca_dia_idade+P210*VG_gmd_estacao+P210*VG_carcaca_dia_idade+VG_gmd_estacao*VG_carcaca_dia_idade, data = dados)
summary(modlucroM15)
anova(modlucroM15)


###VERIFICANDO OS RESIDUOS OBTIDOS PELO METODO STEPWISE
residsw<-resid(modlucroM15) # residuos do Modelo multivariado
predsw<-fitted(modlucroM15) # Valores ajustados pelo modelo
resid.stdsw <- rstandard(modlucroM15)

library(nortest)
lillie.test(residsw) #admite-se a normalidade a 1%
qqnorm(residsw)
qqline(residsw) #muitos pontos outliers
# Histograma dos residuos
hist(residsw)  # aparentam ter distr Normal

library(car)
##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modlucroM15)
#P-valor = 0,03, pode-se admitir independencia dos res�duos

#install.packages("lmtest")
#library(lmtest)
# Breusch- Pagan test
bptest(modlucroM15) #p-value < 0,0001
#N�o pode-se assumir a homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modlucroM15) # variance inflation factors 
sqrt(vif(modlucroM15)) > 2 # Se verdade existe multicolinearidade
#Apresenta problema de multicolinearidade

#APESAR DE PASSAR NA NORMALIDADE, NÃO PASSA NOS DEMAIS PRESSUPOSTOS


#VERIFICAÇÃO DE OUTLIERS
# Existencia de Outliers
####  Outliers:
plot(resid.std, ylab = "resíduos standartizados", main = "Análise dos resíduos")
identify(resid.std)
plot(pred,resid)

# vamos fazer um teste T para ver quais as observacoes sao consideradas outliers
pvalue<-2*(1-pt(abs(resid.std),length(resid.std)))

# Devolve os outliers ao nivel 5% e 10%
names(pvalue)<-1:length(resid.std)
pvalue[pvalue<0.01]
pvalue[pvalue<0.05]
#9, 14, 20, 21, 26, 38, 45, 46, 58, 70, 79, 97, 124, 127, 145, 187, 199, 296, 547
#antes de retirar esses outlier vou verificar os valores influentes
#### Valores Influentes
lev<-hatvalues(modlucroM3fint8)
lev
#como são muitos valores influentes, preferiu-se tentar retirar apenas os animais a 1% de significancia
names(lev)<-1:length(lev)
plot(pred,lev,abline(h=0.2))
identify(pred,lev)
plot(pred,lev,ylim=c(0,0.2),abline(h=0.2))
#nenhum ultrapassa o limite indicado
dcook<-cooks.distance(modlucroM3)
names(dcook)<-1:length(dcook)
plot(dcook,ylim=c(0,0.8))
abline(h=1.0,col="red")
identify(dcook)
#nenhum ultrapassa o limite sugerido pela distancia de cooks
#será retirado os animais que apresentaram-se outliers com significancia a 1%
dados2<-dados[-c(9,14,20,21,26,30,37,38,42,45,46,49,58,69,70,77,79,82,90,97,100,101,124,127,129,132,134,145,146,162,171,173,187,194,199,206,211,231,237,270,284,296,335,355,462,547,553,672,716),]
dados[14,]
dados[38,]
dados[70,]
dados[79,]
dados[124,]
dados[127,]
dados[145,]
 
#voltar a ajustar o modelo multivariado

modlucroM3fint8.2<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+VG_longevidade_produtiva+Dest_abate+VG_cap_crescimento*VG_carcaca_dia_idade+VG_cap_crescimento*Dest_abate, data = dados2)
summary(modlucroM3fint8.2)
anova(modlucroM3fint8.2)

modlucroM3fint8.3<-lm(liquido_recria~peso_ent+VG_cap_crescimento+VG_carcaca_dia_idade+Dest_abate+VG_cap_crescimento*VG_carcaca_dia_idade+VG_cap_crescimento*Dest_abate, data = dados2)
summary(modlucroM3fint8.3)
anova(modlucroM3fint8.3)

#Analise dos resíduos
resid.3<-resid(modlucroM3fint8.3) 
pred.3<-fitted(modlucroM3fint8.3)
resid.std.3<- rstandard(modlucroM3fint8.3)

library(nortest)
lillie.test(resid.3) #admite-se a normalidade
qqnorm(resid.3)
qqline(resid.3) #muitos pontos outliers
# Histograma dos residuos
hist(resid.3)  # aparentam ter distr Normal
xajust<-seq(min(resid.3, na.rm=T),  
            max(resid.3, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(resid.3,na.rm=T),
              sd=sd(resid.3,na.rm=T))
hist(resid.3, freq = F, main = "Histograma dos resíduos do modelo", xlab = "Resíduos", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)
# Avaliar Achatamento

library(car)
##Independencia dos residuos
# teste durbin.Watson 
# de 1.5 < D <2.5 sao independentes
durbinWatsonTest(modlucroM3fint8.3)
#POde-se admitir independencia dos res�duos

install.packages("lmtest")
library(lmtest)
# Breusch- Pagan test
bptest(modlucroM3fint8.3) 
#Não se pode assumir a homocedasticidade

#Existencia de Colinearidade/Multicolinearidade
vif(modlucroM3fint8.3) # variance inflation factors 
sqrt(vif(modlucroM3fint8.3)) > 2 # Se verdade existe multicolinearidade
#não apresenta problema de multicolinearidade

##############Verificando se a variável destino de abate se apresenta significativa
