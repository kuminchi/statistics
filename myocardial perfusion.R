####Global
##load CVS document into R service

bdgl <- read.csv("C:/Users/DELL/Desktop/R/BDglobal.csv")

##Make the avariables as factors
Strain=factor(bdgl$Strain)

## ANOVA Analysis--Diastole
Result.bdgld=aov(bdgl$Diastole~Strain)
summary(Result.bdgld)
TukeyHSD(Result.bdgld)

## ANOVA Analysis--Systole
Result.bdgls=aov(bdgl$Systole~Strain)
summary(Result.bdgls)
TukeyHSD(Result.bdgls)


##Make the avariables as factors
Strain=factor(bdgl$Strain)
Sex=factor(bdgl$Sex)

## ANOVA Analysis
Result.bdgl2=aov(bdgl$Diastole~Strain*Sex)
summary(Result.bdgl2)
TukeyHSD(Result.bdgl2)

##This case would have undesirable comparison


####Septum
##load CVS document into R service

bdsp <- read.csv("C:/Users/DELL/Desktop/R/BDseptum.csv")

##Make the avariables as factors
Strain=factor(bdsp$Strain)

## ANOVA Analysis--Diastole
Result.bdspd=aov(bdsp$Diastole~Strain)
summary(Result.bdspd)
TukeyHSD(Result.bdspd)

## ANOVA Analysis--Systole
Result.bdsps=aov(bdsp$Systole~Strain)
summary(Result.bdsps)
TukeyHSD(Result.bdsps)

####Anterior
##load CVS document into R service

bdan <- read.csv("C:/Users/DELL/Desktop/R/BDanterior.csv")

##Make the avariables as factors
Strain=factor(bdan$Strain)

## ANOVA Analysis--Diastole
Result.bdand=aov(bdan$Diastole~Strain)
summary(Result.bdand)
TukeyHSD(Result.bdand)

## ANOVA Analysis--Systole
Result.bdans=aov(bdan$Systole~Strain)
summary(Result.bdans)
TukeyHSD(Result.bdans)


####Lateral
##load CVS document into R service

bdla <- read.csv("C:/Users/DELL/Desktop/R/BDlateral.csv")

##Make the avariables as factors
Strain=factor(bdla$Strain)

## ANOVA Analysis--Diastole
Result.bdlad=aov(bdla$Diastole~Strain)
summary(Result.bdlad)
TukeyHSD(Result.bdlad)

## ANOVA Analysis--Systole
Result.bdlas=aov(bdla$Systole~Strain)
summary(Result.bdlas)
TukeyHSD(Result.bdlas)


####Inferior
##load CVS document into R service

bdind <- read.csv("C:/Users/DELL/Desktop/R/BDinferiord.csv")

##Make the avariables as factors
Strain=factor(bdind$Strain)

## ANOVA Analysis--Diastole
Result.bdind=aov(bdind$Diastole~Strain)
summary(Result.bdind)
TukeyHSD(Result.bdind)

##load CVS document into R service

bdins <- read.csv("C:/Users/DELL/Desktop/R/BDinferiors.csv")

##Make the avariables as factors
Strain=factor(bdins$Strain)

## ANOVA Analysis--Systole
Result.bdins=aov(bdins$Systole~Strain)
summary(Result.bdins)
TukeyHSD(Result.bdins)


######Sex comparison--B6
###Global
##load CVS document into R service

b6gl <- read.csv("C:/Users/DELL/Desktop/R/B6global.csv")

##Make the avariables as factors
Sex=factor(b6gl$Sex)

## ANOVA Analysis--Diastole
Result.b6gld=aov(b6gl$Diastole~Sex)
summary(Result.b6gld)
TukeyHSD(Result.b6gld)

## ANOVA Analysis--Systole
Result.b6gls=aov(b6gl$Systole~Sex)
summary(Result.b6gls)
TukeyHSD(Result.b6gls)


####Septum
##load CVS document into R service

b6sp <- read.csv("C:/Users/DELL/Desktop/R/B6septum.csv")

##Make the avariables as factors
Sex=factor(b6sp$Sex)

## ANOVA Analysis--Diastole
Result.b6spd=aov(b6sp$Diastole~Sex)
summary(Result.b6spd)
TukeyHSD(Result.b6spd)

## ANOVA Analysis--Systole
Result.b6sps=aov(b6sp$Systole~Sex)
summary(Result.b6sps)
TukeyHSD(Result.b6sps)

####Anterior
##load CVS document into R service

b6an <- read.csv("C:/Users/DELL/Desktop/R/B6anterior.csv")

##Make the avariables as factors
Sex=factor(b6an$Sex)

## ANOVA Analysis--Diastole
Result.b6and=aov(b6an$Diastole~Sex)
summary(Result.b6and)
TukeyHSD(Result.b6and)

## ANOVA Analysis--Systole
Result.b6ans=aov(b6an$Systole~Sex)
summary(Result.b6ans)
TukeyHSD(Result.b6ans)


####Lateral
##load CVS document into R service

b6la <- read.csv("C:/Users/DELL/Desktop/R/B6lateral.csv")

##Make the avariables as factors
Sex=factor(b6la$Sex)

## ANOVA Analysis--Diastole
Result.b6lad=aov(b6la$Diastole~Sex)
summary(Result.b6lad)
TukeyHSD(Result.b6lad)

## ANOVA Analysis--Systole
Result.b6las=aov(b6la$Systole~Sex)
summary(Result.b6las)
TukeyHSD(Result.b6las)


####Inferior
##load CVS document into R service

b6ind <- read.csv("C:/Users/DELL/Desktop/R/B6inferiord.csv")

##Make the avariables as factors
Sex=factor(b6ind$Sex)

## ANOVA Analysis--Diastole
Result.b6ind=aov(b6ind$Diastole~Sex)
summary(Result.b6ind)
TukeyHSD(Result.b6ind)

##load CVS document into R service

b6ins <- read.csv("C:/Users/DELL/Desktop/R/B6inferiors.csv")

##Make the avariables as factors
Sex=factor(b6ins$Sex)

## ANOVA Analysis--Systole
Result.b6ins=aov(b6ins$Systole~Sex)
summary(Result.b6ins)
TukeyHSD(Result.b6ins)


#######Sex comparison--D2
####Global
##load CVS document into R service

d2gl <- read.csv("C:/Users/DELL/Desktop/R/D2global.csv")

##Make the avariables as factors
Sex=factor(d2gl$Sex)

## ANOVA Analysis--Diastole
Result.d2gld=aov(d2gl$Diastole~Sex)
summary(Result.d2gld)
TukeyHSD(Result.d2gld)

## ANOVA Analysis--Systole
Result.d2gls=aov(d2gl$Systole~Sex)
summary(Result.d2gls)
TukeyHSD(Result.d2gls)

####Septum
##load CVS document into R service

d2sp <- read.csv("C:/Users/DELL/Desktop/R/D2septum.csv")

##Make the avariables as factors
Sex=factor(d2sp$Sex)

## ANOVA Analysis--Diastole
Result.d2spd=aov(d2sp$Diastole~Sex)
summary(Result.d2spd)
TukeyHSD(Result.d2spd)

## ANOVA Analysis--Systole
Result.d2sps=aov(d2sp$Systole~Sex)
summary(Result.d2sps)
TukeyHSD(Result.d2sps)

####Anterior
##load CVS document into R service

d2an <- read.csv("C:/Users/DELL/Desktop/R/D2anterior.csv")

##Make the avariables as factors
Sex=factor(d2an$Sex)

## ANOVA Analysis--Diastole
Result.d2and=aov(d2an$Diastole~Sex)
summary(Result.d2and)
TukeyHSD(Result.d2and)

## ANOVA Analysis--Systole
Result.d2ans=aov(d2an$Systole~Sex)
summary(Result.d2ans)
TukeyHSD(Result.d2ans)


####Lateral
##load CVS document into R service

d2la <- read.csv("C:/Users/DELL/Desktop/R/D2lateral.csv")

##Make the avariables as factors
Sex=factor(d2la$Sex)

## ANOVA Analysis--Diastole
Result.d2lad=aov(d2la$Diastole~Sex)
summary(Result.d2lad)
TukeyHSD(Result.d2lad)

## ANOVA Analysis--Systole
Result.d2las=aov(d2la$Systole~Sex)
summary(Result.d2las)
TukeyHSD(Result.d2las)


####Inferior
##load CVS document into R service

d2ind <- read.csv("C:/Users/DELL/Desktop/R/D2inferiord.csv")

##Make the avariables as factors
Sex=factor(d2ind$Sex)

## ANOVA Analysis--Diastole
Result.d2ind=aov(d2ind$Diastole~Sex)
summary(Result.d2ind)
TukeyHSD(Result.d2ind)

##load CVS document into R service

d2ins <- read.csv("C:/Users/DELL/Desktop/R/D2inferiors.csv")

##Make the avariables as factors
Sex=factor(d2ins$Sex)

## ANOVA Analysis--Systole
Result.d2ins=aov(d2ins$Systole~Sex)
summary(Result.d2ins)
TukeyHSD(Result.d2ins)

