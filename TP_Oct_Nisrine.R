### iii- Importation des données: 
TPretinol<- read.csv2("presentationTPretinol.csv")

#### - déclaration des variables qualitatives
TPretinol$sexe <- as.factor(TPretinol$sexe)
TPretinol$tabac <- as.factor(TPretinol$tabac) 
TPretinol$vitamine <- as.factor(TPretinol$vitamine)

##### DESCRIPTION DES VARIABLES QUANTITATIVES
library(prettyR)
describe(TPretinol,num.desc=c("mean","median","var","sd","valid.n","min","max"))
##Quantile age
quantile(TPretinol$age,na.rm = TRUE)
##Quantile bmi
quantile(TPretinol$bmi,na.rm = TRUE)
##Quantile calories
quantile(TPretinol$calories,na.rm = TRUE)
##Quantile graisses
quantile(TPretinol$graisses,na.rm = TRUE)
##Quantile cholesterol
quantile(TPretinol$cholesterol,na.rm = TRUE)
##Quantile fibres
quantile(TPretinol$fibres,na.rm = TRUE)
##Quantile alcool
quantile(TPretinol$alcool,na.rm = TRUE)
##Quantile betadiet
quantile(TPretinol$betadiet,na.rm = TRUE)
##Quantile retdiet
quantile(TPretinol$retdiet,na.rm = TRUE)
##Quantile betaplasma
quantile(TPretinol$betaplasma,na.rm = TRUE)
##Quantile retplasma
quantile(TPretinol$retplasma,na.rm = TRUE)


###Histogramme

hist(TPretinol$age,main="Histogramme de l'âge",xlab="Age (en années)",ylab="Effectif", col="cadetblue2", las=1)
hist(TPretinol$bmi,main="Histogramme du BMI",xlab="BMI (poids/(taille^2))",ylab="Effectif",col="bisque2", las=1)
hist(TPretinol$alcool,main="Histogramme d'ALCOOL",xlab="Nombre de verres d'alcool consommés par semaine",ylab="Effectif",col="darkgoldenrod1", las=1)
table(TPretinol$alcool == 203)
hist(TPretinol$alcool[TPretinol$alcool<203],nclass=30,main="",xlab="Nombre de  verres d'alcool par semaine",ylab="Effectif",col="darkgoldenrod1", las=1)
hist(TPretinol$cholesterol,main="Histogramme du CHOLESTEROL",xlab="Cholesterol consommé (mg par jour)",ylab="Effectif",col="darkgoldenrod1", las=1)
hist(TPretinol$retdiet,main="Histogramme de RETINOL (RETDIET)",xlab="retinol consommé (mcg par jour)",ylab="Effectif",col="blueviolet", las=1)
hist(TPretinol$retplasma,main="Histogramme de Concentration en rétinol plasmatique (RETPLASMA)" ,xlab="Retinol plasmatique (ng/ml)",ylab="Effectif",col="darkolivegreen3", las=1)


#### DESCRIPTION DES VARIABLES QUALITATIVES 
SEXE<-table(factor(TPretinol$sexe, levels = c(1,2), labels = c("Homme","Femme")))
SEXE
prop.table(SEXE) * 100
barplot(SEXE, main = "diagramme en batôns des hommes et des femmes",xlab="Sexe",ylab="Fréquence", las=1, col = "khaki1")


par(mfrow=c(1,2))
## Tabac 
TABAC<-table(factor(TPretinol$tabac, levels = c(1,2,3), labels =c("Jamais","Autrefois","Actuellement")))
TABAC
prop.table(TABAC) * 100
##Vitamine
VITAMINE<-table(factor(TPretinol$vitamine, levels = c(1,2,3), labels =c("souvent","pas souvent","non")))
VITAMINE
prop.table(VITAMINE) * 100

barplot(TABAC, main = "consommation de tabac",xlab="Tabac",ylab="Fréquence", ylim = c(0,200), las=1, col = "lavenderblush1")
barplot(VITAMINE, main = "Consommation de vitamines",xlab="Vitamine",ylab="Fréquence", las=1,ylim = c(0,150), col = "lightcoral")

Tab_Var <- read.csv2("tableau_Variable.csv")
print(Tab_Var)




#### CROISEMENTS DES VARIABLES:
##### 1- Variables qualitative / qualitative
##Sexe/Tabac

table(TPretinol$sexe,TPretinol$tabac,deparse.level = 2) 
chisq.test(TPretinol$sexe,TPretinol$tabac,correct=F)

##Sexe/Vitamine
table(TPretinol$sexe,TPretinol$vitamine, deparse.level = 2) 
chisq.test(TPretinol$sexe,TPretinol$vitamine,correct=FALSE)

##Vitamine/Tabac
table(TPretinol$vitamine,TPretinol$tabac,deparse.level = 2) 
chisq.test(TPretinol$vitamine,TPretinol$tabac,correct=FALSE)

##### 2- Variables quantitative / qualitative
##Sexe/Age 
by(TPretinol$age,TPretinol$sexe,sd,na.rm=TRUE) #égalité var
t.test(TPretinol$age~TPretinol$sexe, var.equal=TRUE) # différence significative  


##Boxplot
library(gplots)
boxplot(age~sexe,TPretinol)

##OU  

library(ggplot2)
ggplot(TPretinol, aes(x=sexe, y=age, fill=sexe)) +
  geom_boxplot()+theme_grey()+
  ggtitle("L'age en fonction du sexe")

## Alcool/Sexe 
by(TPretinol$alcool,TPretinol$sexe,sd,na.rm=TRUE) #pas d'égalité var
t.test(TPretinol$alcool~TPretinol$sexe, var.equal=TRUE) # différence significative

ggplot(TPretinol, aes(x=sexe, y=alcool, fill=sexe)) +
  geom_boxplot()+scale_fill_manual(values=c("#999999", "#E69F00"))+ylim(0,40)+theme_grey()+ ggtitle("Consommation quotidienne d'alcool en fonction du sexe")


## Sexe/Cholesterol 
by(TPretinol$cholesterol,TPretinol$sexe,sd,na.rm=TRUE) #pas d'égalité var
t.test(TPretinol$cholesterol~TPretinol$sexe, var.equal=TRUE)# différence sgnificative

ggplot(TPretinol, aes(x=sexe, y=cholesterol, fill=sexe)) +
  geom_boxplot()+scale_fill_manual(values=c("#994499", "#E69F22"))+theme_grey()+ ggtitle("Taux de cholesterol en fonction du sexe")

## Sexe/BMI 
by(TPretinol$bmi,TPretinol$sexe,sd,na.rm=TRUE) #pas d'égalité var
t.test(TPretinol$bmi~TPretinol$sexe, var.equal=TRUE)# différence non sgnificative 

ggplot(TPretinol, aes(x=sexe, y=bmi, fill=sexe)) +
  geom_boxplot()+scale_fill_manual(values=c("#999974", "#E69F88"))+theme_grey()+ ggtitle("BMI en fonction du sexe")

## Sexe/RETDIET 
by(TPretinol$retdiet,TPretinol$sexe,sd,na.rm=TRUE) #pas d'égalité var
t.test(TPretinol$retdiet~TPretinol$sexe, var.equal=TRUE)# différence non sgnificative 

ggplot(TPretinol, aes(x=sexe, y=retdiet, fill=sexe)) +
  geom_boxplot()+scale_fill_manual(values=c("#999823", "#E69D88"))+theme_grey()+ ggtitle("Retinol consommé(mcg par jour) en fonction du sexe")

## Sexe/RETPLASMA
by(TPretinol$retplasma,TPretinol$sexe,sd,na.rm=TRUE) #pas d'égalité var
t.test(TPretinol$retplasma~TPretinol$sexe, var.equal=TRUE)# différence sgnificative

ggplot(TPretinol, aes(x=sexe, y=retplasma, fill=sexe)) +
  geom_boxplot()+scale_fill_manual(values=c("#998774", "#E27F48"))+theme_grey()+ ggtitle("Retinol plasmatique(ng/ml) en fonction du sexe")

#Tabac/Age 
T1<- lm(age~tabac,data=TPretinol)
summary(T1)
drop1(T1,.~., test="F")#significative
boxplot(TPretinol$age~TPretinol$tabac,varwidth =TRUE, notch = TRUE, outline = TRUE,
        col=(c("yellow4","yellow3","yellow1")),
        main="Consommation du tabac en fonction de l'âge", xlab="Tabac",ylab="Age")
legend("bottomright", inset=.02, title="Consommation du tabac",
       c("Jamais","Autrefois","actuellement"), fill=c("yellow4","yellow3","yellow1"), horiz=TRUE, cex=0.8)


#Tabac/BMI
T2<- lm(bmi~tabac,data=TPretinol)
summary(T2)
drop1(T2,.~., test="F")# non significative
boxplot(TPretinol$bmi~TPretinol$tabac,varwidth =TRUE, notch = TRUE, outline = TRUE,
        col=(c("yellow4","yellow3","yellow1")),
        main="Consommation du tabac en fonction du bmi", xlab="Tabac",ylab="BMI")
legend("bottomright", inset=.02, title="Consommation du tabac",
       c("Jamais","Autrefois","actuellement"), fill=c("yellow4","yellow3","yellow1"), horiz=TRUE, cex=0.8)


#Tabac/Alcool
T3<- lm(alcool~tabac,data=TPretinol)
summary(T3)
drop1(T3,.~., test="F")#significative
boxplot(TPretinol$alcool~TPretinol$tabac,varwidth =TRUE, notch = TRUE, outline = TRUE,
        col=(c("yellow4","yellow3","yellow1")),
        main="Consommation du tabac par rapport à la consommation de l'alcool  par semaine ", xlab="Tabac",ylab="Alcool")
legend("topleft", inset=.02, title="Consommation du tabac",
       c("Jamais","Autrefois","actuellement"), fill=c("yellow4","yellow3","yellow1"), horiz=TRUE, cex=0.8)

#Tabac/Cholesterol
T4<- lm(cholesterol~tabac,data=TPretinol)
summary(T4)
drop1(T4,.~., test="F")# non significative
boxplot(TPretinol$cholesterol~TPretinol$tabac,varwidth =TRUE, notch = TRUE, outline = TRUE,
        col=(c("yellow4","yellow3","yellow1")),
        main="Consommation du tabac par rapport au cholesterol (mg par jour)", xlab="Tabac",ylab="Cholesterol (mg par jour)")
legend("topright", inset=.02, title="Consommation du tabac",
       c("Jamais","Autrefois","actuellement"), fill=c("yellow4","yellow3","yellow1"), horiz=TRUE, cex=0.8)

#Tabac/RETDIET
T5<- lm(retdiet~tabac,data=TPretinol)
summary(T5)
drop1(T5,.~., test="F")#non significative
boxplot(TPretinol$retdiet~TPretinol$tabac,varwidth =TRUE, notch = TRUE, outline = TRUE,
        col=(c("yellow4","yellow3","yellow1")),
        main="Consommation du tabac en fonction du rétinol", xlab="Tabac",ylab="Retinol consommé (mcg par jour)")
legend("topright", inset=.02, title="Consommation du tabac",
       c("Jamais","Autrefois","actuellement"), fill=c("yellow4","yellow3","yellow1"), horiz=TRUE, cex=0.8)

#Tabac/RETPLASMA
T5<- lm(retplasma~tabac,data=TPretinol)
summary(T5)
drop1(T5,.~., test="F")#significative
boxplot(TPretinol$retplasma~TPretinol$tabac,varwidth =TRUE, notch = TRUE, outline = TRUE,
        col=(c("yellow4","yellow3","yellow1")),
        main="Consommation du tabac en fonction du retinol plasmatique (ng/ml) ", xlab="Tabac",ylab="Retinol plasmatique (ng/ml) ")
legend("bottomright", inset=.02, title="Consommation du tabac",
       c("Jamais","Autrefois","actuellement"), fill=c("yellow4","yellow3","yellow1"), horiz=TRUE, cex=0.8)


#Vitamine/Age 
T6<- lm(age~vitamine,data=TPretinol)
summary(T6)
drop1(T6,.~., test="F")#significative
boxplot(TPretinol$age~TPretinol$vitamine,varwidth =TRUE, notch = TRUE, outline = TRUE,
        col=(c("steelblue4","steelblue1","lightsteelblue1")),
        main="Vitamine en fonction de l'âge", xlab="vitamine",ylab="Age")
legend("bottomright", inset=.02, title="Vitamine",
       c("souvent","pas souvent","non"), fill=c("steelblue4","steelblue1","lightsteelblue1"), horiz=TRUE, cex=0.8)


#Vitamine/BMI
T7<- lm(bmi~vitamine,data=TPretinol)
summary(T7)
drop1(T7,.~., test="F")#non significative
boxplot(TPretinol$bmi~TPretinol$vitamine,varwidth =TRUE, notch = TRUE, outline = TRUE,
        col=(c("steelblue4","steelblue1","lightsteelblue1")),
        main="Vitamine en fonction du bmi", xlab="Vitamine",ylab="BMI")
legend("bottomright", inset=.02, title="Vitamine",
       c("souvent","pas souvent","non"), fill=c("steelblue4","steelblue1","lightsteelblue1"), horiz=TRUE, cex=0.8)


#Vitamine/Alcool
T8<- lm(alcool~vitamine,data=TPretinol)
summary(T8)
drop1(T8,.~., test="F")#non significative
boxplot(TPretinol$alcool~TPretinol$vitamine,varwidth =TRUE, notch = TRUE, outline = TRUE,
        col=(c("steelblue4","steelblue1","lightsteelblue1")),
        main="Vitamine par rapport à la consommation de l'alcool  par semaine ", xlab="Vitamine",ylab="Alcool")
legend("topleft", inset=.02, title="Vitamine",
       c("souvent","pas souvent","non"), fill=c("steelblue4","steelblue1","lightsteelblue1"), horiz=TRUE, cex=0.8)


#Vitamine/Cholesterol
T9<- lm(cholesterol~vitamine,data=TPretinol)
summary(T9)
drop1(T9,.~., test="F")#non significative
boxplot(TPretinol$cholesterol~TPretinol$vitamine,varwidth =TRUE, notch = TRUE, outline = TRUE,
        col=(c("steelblue4","steelblue1","lightsteelblue1")),
        main="Vitamine par rapport au cholesterol (mg par jour)", xlab="Vitamine",ylab="Cholesterol (mg par jour)")
legend("topright", inset=.02, title="Vitamine",
       c("souvent","pas souvent","non"), fill=c("steelblue4","steelblue1","lightsteelblue1"), horiz=TRUE, cex=0.8)

#Vitamine/RETDIET
T10<- lm(retdiet~vitamine,data=TPretinol)
summary(T10)
drop1(T10,.~., test="F")#non significative
boxplot(TPretinol$retdiet~TPretinol$vitamine,varwidth =TRUE, notch = TRUE, outline = TRUE,
        col=(c("steelblue4","steelblue1","lightsteelblue1")),
        main="Vitamine en fonction du  rétinol", xlab="Vitamine",ylab="Retinol consommé (mcg par jour)")
legend("topright", inset=.02, title="Vitamine",
       c("souvent","pas souvent","non"), fill=c("steelblue4","steelblue1","lightsteelblue1"), horiz=TRUE, cex=0.8)

#Vitamine/RETPLASMA
T11<- lm(retplasma~vitamine,data=TPretinol)
summary(T11)
drop1(T11,.~., test="F")#non significative
boxplot(TPretinol$retplasma~TPretinol$vitamine,varwidth =TRUE, notch = TRUE, outline = TRUE,
        col=(c("steelblue4","steelblue1","lightsteelblue1")),
        main="Vitamine en fonction du retinol plasmatique (ng/ml) ", xlab="Vitamine",ylab="Retinol plasmatique (ng/ml) ")
legend("bottomright", inset=.02, title="Vitamine",
       c("souvent","pas souvent","non"), fill=c("steelblue4","steelblue1","lightsteelblue1"), horiz=TRUE, cex=0.8)


#### 3- Variables quantitatives / quantitatives

## Calcul de matrice de corrélation
library(corrplot)
TPretinol_cut<- TPretinol[, c(1,4,9,10,12,14)]
head(TPretinol_cut)
corrplot(cor(TPretinol_cut,use="complete.obs"),method = "circle")
#OU
corrplot(cor(TPretinol_cut,use="complete.obs"),method = "number")
##OU
corrplot(cor(TPretinol_cut,use="complete.obs"),method = "color")

### Autre manière 


library(Hmisc)
rcorr(as.matrix(TPretinol_cut))

names(TPretinol)
Retcor <- cor(TPretinol_cut)
Retcor
library(corrplot)
corrplot(Retcor, type="full", order="original", tl.col="blue", tl.srt=45)


##Age/BMI
cor.test(TPretinol$age,TPretinol$bmi)## non significative
plot(TPretinol$age,TPretinol$bmi,ylab='BMI',xlab="Age (en années)", main="BMI en fonction de l'age", col="blue")

##Age/Alcool
cor.test(TPretinol$age,TPretinol$alcool)## non significative
plot(TPretinol$age,TPretinol$alcool,ylab='Cons.Alcool par semaine',xlab="Age (en années)", main="Consommation d'alcool par semaine en fonction de l'age", col="blue")

##Age/Cholesterol
cor.test(TPretinol$age,TPretinol$cholesterol)## significative
plot(TPretinol$age,TPretinol$cholesterol,ylab='Cholesterol (mg par jour)',xlab="Age (en années)", main="Cholesterol en fonction de l'age", col="blue")


##Age/Retdiet
cor.test(TPretinol$age,TPretinol$retdiet)## non significative
plot(TPretinol$age,TPretinol$retdiet,ylab='retinol consommé (mg par jour)',xlab="Age (en années)", main="Retinol consommé en fonction de l'age",col="blue")


##Age/Retplasma
cor.test(TPretinol$age,TPretinol$retplasma)## significative
plot(TPretinol$age,TPretinol$retplasma,ylab='Retinol plasmatique (ng/ml)',xlab="Age (en années)", main="Retinol plasmatique  en fonction de l'age", col="blue")

##BMI/Alcool
cor.test(TPretinol$bmi,TPretinol$alcool)## non significative
plot(TPretinol$bmi,TPretinol$alcool,ylab='Cons.Alcool par semaine',xlab="BMI", main="Consommation d'alcool par semaine en fonction du BMI", col="green")

##BMI/Cholesterol
cor.test(TPretinol$bmi,TPretinol$cholesterol)## non significative
plot(TPretinol$bmi,TPretinol$cholesterol,ylab='Cholesterol (mg par jour)',xlab="BMI", main="Cholesterol en fonction du BMI", col="green")


##BMI/Retdiet
cor.test(TPretinol$bmi,TPretinol$retdiet)## non significative
plot(TPretinol$bmi,TPretinol$retdiet,ylab='retinol consommé (mg par jour)',xlab="BMI", main="Retinol consommé en fonction du BMI",col="green")

##BMI/Retplasma
cor.test(TPretinol$bmi,TPretinol$retplasma)## non significative
plot(TPretinol$bmi,TPretinol$retplasma,ylab='Retinol plasmatique (ng/ml)',xlab="BMI", main="Retinol plasmatique  en fonction du BMI", col="green")


##Alcool/Cholesterol
cor.test(TPretinol$alcool,TPretinol$cholesterol)## significative
plot(TPretinol$alcool,TPretinol$cholesterol,ylab='Cholesterol (mg par jour)',xlab="Alcool", main="Cholesterol en fonction de Alcool", col="red")


##Alcol/Retdiet
cor.test(TPretinol$alcool,TPretinol$retdiet)## non significative
plot(TPretinol$alcool,TPretinol$retdiet,ylab='retinol consommé (mg par jour)',xlab="Alcool", main="Retinol consommé en fonction de Alcool",col="red")


##Alcool/Retplasma
cor.test(TPretinol$alcool,TPretinol$retplasma)## non significative
plot(TPretinol$alcool,TPretinol$retplasma,ylab='Retinol plasmatique (ng/ml)',xlab="Alcool", main="Retinol plasmatique  en fonction de Alcool", col="red")

##Cholesterol/Retdiet
cor.test(TPretinol$cholesterol,TPretinol$retdiet)##  significative
plot(TPretinol$cholesterol,TPretinol$retdiet,ylab='retinol consommé (mg par jour)',xlab="Cholesterol", main="Retinol consommé en fonction du Cholesterol",col="deeppink4")


##Cholesterol/Retplasma
cor.test(TPretinol$cholesterol,TPretinol$retplasma)## non significative
plot(TPretinol$cholesterol,TPretinol$retplasma,ylab='Retinol plasmatique (ng/ml)',xlab="Cholesterol", main="Retinol plasmatique  en fonction du Cholesterol", col="deeppink4")

##Retdiet/Retplasma
cor.test(TPretinol$retdiet,TPretinol$retplasma)## non significative
plot(TPretinol$retdiet,TPretinol$retplasma,ylab='Retinol plasmatique (ng/ml)',xlab="Retinol consommé", main="Retinol plasmatique  en fonction du retinol consommé", col="gray20")


### 4- Régression linéaire multiple 

str(TPretinol)
T12 <- lm(retplasma~age+sexe+tabac+bmi+vitamine+alcool+cholesterol+retdiet,data=TPretinol)
summary(T12)
drop1(T12,.~.,test="F")


#### Régression linéaire multiple avec interaction

T12 <- lm(retplasma~age+sexe+tabac+bmi+vitamine+alcool+cholesterol+retdiet,data=TPretinol)
add1(T12,.~.^2,test="F")

par(mfrow=c(1,3))
hist(resid(T12), col = "grey")
qqnorm(resid(T12))
qqline(resid(T12))


### 5- Régression logistique
median(TPretinol$retplasma) ## 566
TPretinol$retplasma_Recode <- ifelse(TPretinol$retplasma < 566,0,1)
class(TPretinol$retplasma_Recode)
TPretinol$retplasma_Recode<-as.factor(TPretinol$retplasma_Recode)
str(TPretinol$retplasma_Recode)
table(TPretinol$retplasma_Recode)

T14<- glm(retplasma_Recode~age+sexe+tabac+vitamine+bmi+alcool+cholesterol+retdiet, data=TPretinol, family="binomial")
summary(T14)
drop1(T14,.~.,test="Chisq")

add1(T14,.~.^2,test="C")


