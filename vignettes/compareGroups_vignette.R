### R code from vignette source 'compareGroups_vignette.rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: compareGroups_vignette.rnw:46-48
###################################################
rm(list=ls())
options(warn = -1)


###################################################
### code chunk number 2: compareGroups_vignette.rnw:85-87
###################################################
dir.create("./univar",showWarnings = FALSE)
dir.create("./bivar",showWarnings = FALSE)


###################################################
### code chunk number 3: compareGroups_vignette.rnw:104-105
###################################################
library(compareGroups)


###################################################
### code chunk number 4: compareGroups_vignette.rnw:149-151
###################################################
data(regicor)
head(regicor)


###################################################
### code chunk number 5: compareGroups_vignette.rnw:156-163
###################################################
dicc<-data.frame(
"Name"=I(names(regicor)),
"Label"=I(unlist(lapply(regicor,label))),
"Codes"=I(unlist(lapply(regicor,function(x) paste(levels(x),collapse="; "))))
)
dicc$Codes<-sub(">=","$\\\\geq$",dicc$Codes)  
print(xtable(dicc,align=rep("l",4)),include.rownames=FALSE,size="small",tabular.environment="longtable", sanitize.text.function=function(x) x)


###################################################
### code chunk number 6: compareGroups_vignette.rnw:191-196
###################################################
regicor$tdeath <- with(regicor, Surv(todeath, death == 'Yes'))
label(regicor$tdeath) <- "Mortality"

regicor$tcv <- with(regicor, Surv(tocv, cv == 'Yes'))
label(regicor$tcv) <- "Cardiovascular"


###################################################
### code chunk number 7: compareGroups_vignette.rnw:215-216
###################################################
compareGroups(year ~ . , data=regicor)


###################################################
### code chunk number 8: compareGroups_vignette.rnw:227-228
###################################################
compareGroups(year ~ . -id, data=regicor)


###################################################
### code chunk number 9: compareGroups_vignette.rnw:235-237
###################################################
res<-compareGroups(year ~ age + sex + smoker + triglyc , data=regicor)
res


###################################################
### code chunk number 10: compareGroups_vignette.rnw:258-259
###################################################
compareGroups(year ~ age + smoker + triglyc, data=regicor, subset = sex=='Male')


###################################################
### code chunk number 11: compareGroups_vignette.rnw:266-267
###################################################
options(width=80)


###################################################
### code chunk number 12: compareGroups_vignette.rnw:269-271
###################################################
compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
            selec = list(triglyc=txchol=='No', sbp=txhtn=='No'))


###################################################
### code chunk number 13: compareGroups_vignette.rnw:273-275
###################################################
compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
            selec = list(triglyc=txchol=='No', sbp=txhtn=='No'))


###################################################
### code chunk number 14: compareGroups_vignette.rnw:280-281
###################################################
options(width=80)


###################################################
### code chunk number 15: compareGroups_vignette.rnw:283-286
###################################################
compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
    selec = list(triglyc=txchol=='No', sbp=txhtn=='No'), 
    subset = sex=='Male')


###################################################
### code chunk number 16: compareGroups_vignette.rnw:289-293
###################################################
options(width=80)
compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
    selec = list(triglyc=txchol=='No', sbp=txhtn=='No'), 
    subset = sex=='Male')


###################################################
### code chunk number 17: compareGroups_vignette.rnw:298-299
###################################################
options(width=80)


###################################################
### code chunk number 18: compareGroups_vignette.rnw:301-303
###################################################
compareGroups(year ~ age + smoker + triglyc + triglyc, data=regicor, 
	selec = list(triglyc.1=txchol=='No'))


###################################################
### code chunk number 19: compareGroups_vignette.rnw:317-318
###################################################
options(width=80)


###################################################
### code chunk number 20: compareGroups_vignette.rnw:320-321
###################################################
compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, method = c(triglyc=2))


###################################################
### code chunk number 21: compareGroups_vignette.rnw:323-324
###################################################
options(width=120)


###################################################
### code chunk number 22: compareGroups_vignette.rnw:339-340
###################################################
options(width=80)


###################################################
### code chunk number 23: compareGroups_vignette.rnw:342-344
###################################################
compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
              method = c(triglyc=NA), alpha= 0.01)


###################################################
### code chunk number 24: compareGroups_vignette.rnw:346-347
###################################################
options(width=120)


###################################################
### code chunk number 25: compareGroups_vignette.rnw:356-360
###################################################
cuts<-"lo:40=1; 41:45=2; 46:50=3; 51:55=4; 56:60=5; 61:65=6; 66:hi=7"
regicor$age7gr<-car::recode(regicor$age, cuts)
compareGroups(year ~ age7gr, data=regicor, method = c(age7gr=NA))
compareGroups(year ~ age7gr, data=regicor, method = c(age7gr=NA), min.dis=8)


###################################################
### code chunk number 26: compareGroups_vignette.rnw:376-377
###################################################
compareGroups(age7gr ~ smoker + triglyc + sbp, data=regicor, max.ylev=7)


###################################################
### code chunk number 27: compareGroups_vignette.rnw:382-383
###################################################
compareGroups(year ~ smoker + age7gr, method= (age7gr=3), data=regicor, max.xlev=5)


###################################################
### code chunk number 28: compareGroups_vignette.rnw:402-403
###################################################
compareGroups(year ~ smoker + triglyc + sbp, data=regicor, include.label= FALSE)


###################################################
### code chunk number 29: compareGroups_vignette.rnw:410-411
###################################################
options(width=80)


###################################################
### code chunk number 30: compareGroups_vignette.rnw:413-416
###################################################
resu1<-compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
                     method = c(triglyc=2))
createTable(resu1)


###################################################
### code chunk number 31: compareGroups_vignette.rnw:423-426
###################################################
resu2<-compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
                     method = c(triglyc=2), Q1=0.025, Q3=0.975)
createTable(resu2)


###################################################
### code chunk number 32: compareGroups_vignette.rnw:433-434
###################################################
options(width=80)


###################################################
### code chunk number 33: compareGroups_vignette.rnw:436-438
###################################################
compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
              method = c(triglyc=2), Q1=0, Q3=1)


###################################################
### code chunk number 34: compareGroups_vignette.rnw:440-441
###################################################
options(width=120)


###################################################
### code chunk number 35: compareGroups_vignette.rnw:448-452
###################################################
regicor$smk<-regicor$smoker
levels(regicor$smk)<- c("Never smoker", "Current or former < 1y", 
                        "Never or former >= 1y", "Unknown")
cbind(table(regicor$smk))


###################################################
### code chunk number 36: compareGroups_vignette.rnw:484-485
###################################################
compareGroups(year ~ age + smk + sbp, data=regicor, simplify=TRUE)


###################################################
### code chunk number 37: compareGroups_vignette.rnw:497-500
###################################################
res<-compareGroups(year ~ age + sex + smoker + txchol + triglyc + sbp, 
                   method = c(triglyc=2), data=regicor)
summary(res[c(1, 3, 4)])


###################################################
### code chunk number 38: compareGroups_vignette.rnw:513-514
###################################################
plot(res[c(1,2)], file="./univar/")


###################################################
### code chunk number 39: compareGroups_vignette.rnw:532-533
###################################################
plot(res[c(1,2)], bivar=TRUE, file="./bivar/")


###################################################
### code chunk number 40: compareGroups_vignette.rnw:557-559
###################################################
res<-compareGroups(year ~ age + sex + triglyc, data=regicor)
res


###################################################
### code chunk number 41: compareGroups_vignette.rnw:564-565
###################################################
options(width=80)


###################################################
### code chunk number 42: compareGroups_vignette.rnw:567-570
###################################################
res<-update(res, . ~. - sex + smoker +  chol, subset = sex=='Male', 
                method = c(triglyc=2, chol = 2), 
                selec = list(triglyc=txchol=='No', sbp=txhtn=='No'))


###################################################
### code chunk number 43: compareGroups_vignette.rnw:572-573
###################################################
options(width=120)


###################################################
### code chunk number 44: compareGroups_vignette.rnw:575-576
###################################################
res


###################################################
### code chunk number 45: compareGroups_vignette.rnw:592-594
###################################################
res1<-compareGroups(sex ~ age + smoker + txchol + sbp, data=regicor, ref=1)
createTable(res1, show.ratio=TRUE, show.p.overall=FALSE)


###################################################
### code chunk number 46: compareGroups_vignette.rnw:599-600
###################################################
options(width=80)


###################################################
### code chunk number 47: compareGroups_vignette.rnw:602-605
###################################################
res2<-compareGroups(sex ~ age + smoker + txchol + sbp, data=regicor, 
                    ref=c(smoker=1, txchol=2))
createTable(res2, show.ratio=TRUE, show.p.overall=FALSE)


###################################################
### code chunk number 48: compareGroups_vignette.rnw:610-611
###################################################
options(width=120)


###################################################
### code chunk number 49: compareGroups_vignette.rnw:618-620
###################################################
res<-compareGroups(sex ~ age + smoker + txchol + sbp, data=regicor, ref.no='NO')
createTable(res, show.ratio=TRUE, show.p.overall=FALSE)


###################################################
### code chunk number 50: compareGroups_vignette.rnw:629-631
###################################################
res<-compareGroups(sex ~ age + txchol + sbp, data=regicor)
createTable(res, show.ratio=TRUE, show.p.overall=FALSE)


###################################################
### code chunk number 51: compareGroups_vignette.rnw:636-637
###################################################
options(width=80)


###################################################
### code chunk number 52: compareGroups_vignette.rnw:639-641
###################################################
res<-compareGroups(sex ~ age + txchol + sbp, data=regicor, fact.ratio= c(age=10, sbp=100))
createTable(res, show.ratio=TRUE, show.p.overall=FALSE)


###################################################
### code chunk number 53: compareGroups_vignette.rnw:643-644
###################################################
options(width=120)


###################################################
### code chunk number 54: compareGroups_vignette.rnw:653-655
###################################################
res<-compareGroups(sex ~ histchol + txchol + sbp, data=regicor)
createTable(res, show.ratio=TRUE, show.p.overall=FALSE)


###################################################
### code chunk number 55: compareGroups_vignette.rnw:660-662
###################################################
res<-compareGroups(sex ~ histchol + txchol + sbp, data=regicor, ref.y=2)
createTable(res, show.ratio=TRUE, show.p.overall=FALSE)


###################################################
### code chunk number 56: compareGroups_vignette.rnw:675-677
###################################################
plot(compareGroups(tcv ~ txchol, data=regicor), bivar=TRUE, file="./bivar/")
plot(compareGroups(tcv ~ sbp, data=regicor), bivar=TRUE, file="./bivar/")


###################################################
### code chunk number 57: compareGroups_vignette.rnw:704-705
###################################################
options(width=80)


###################################################
### code chunk number 58: compareGroups_vignette.rnw:707-710
###################################################
res<-compareGroups(sex ~  age + tdeath + tcv, timemax=c(tcv=365.25*5, tdeath=365.25*3), 
                   data=regicor)
res


###################################################
### code chunk number 59: compareGroups_vignette.rnw:712-713
###################################################
options(width=120)


###################################################
### code chunk number 60: compareGroups_vignette.rnw:722-724
###################################################
plot(res[2], file="./univar/")
plot(res[2], bivar=TRUE, file="./bivar/")


###################################################
### code chunk number 61: compareGroups_vignette.rnw:749-750
###################################################
options(width=100)


###################################################
### code chunk number 62: compareGroups_vignette.rnw:753-756
###################################################
res<-compareGroups(year ~ age + sex + smoker + triglyc , method= c(triglyc=2), 
       data=regicor)
restab<-createTable(res)


###################################################
### code chunk number 63: compareGroups_vignette.rnw:761-762
###################################################
print(restab,which.table='descr')


###################################################
### code chunk number 64: compareGroups_vignette.rnw:767-768
###################################################
print(restab,which.table='avail')


###################################################
### code chunk number 65: compareGroups_vignette.rnw:784-785
###################################################
update(restab, hide = c(sex='Male'))


###################################################
### code chunk number 66: compareGroups_vignette.rnw:794-796
###################################################
res<-compareGroups(year ~ age + sex + histhtn + txhtn, data=regicor)
createTable(res, hide.no='no')


###################################################
### code chunk number 67: compareGroups_vignette.rnw:805-806
###################################################
createTable(res, digits= c(age=2, sex = 3))


###################################################
### code chunk number 68: compareGroups_vignette.rnw:815-816
###################################################
createTable(res, type=1)


###################################################
### code chunk number 69: compareGroups_vignette.rnw:821-822
###################################################
createTable(res, type=3)


###################################################
### code chunk number 70: compareGroups_vignette.rnw:834-835
###################################################
createTable(res, show.n=TRUE)


###################################################
### code chunk number 71: compareGroups_vignette.rnw:842-843
###################################################
createTable(res, show.descr=FALSE)


###################################################
### code chunk number 72: compareGroups_vignette.rnw:850-851
###################################################
createTable(res, show.all=TRUE)


###################################################
### code chunk number 73: compareGroups_vignette.rnw:857-858
###################################################
createTable(res, show.p.overall=FALSE)


###################################################
### code chunk number 74: compareGroups_vignette.rnw:865-866
###################################################
createTable(res, show.p.trend=TRUE)


###################################################
### code chunk number 75: compareGroups_vignette.rnw:876-877
###################################################
createTable(res, show.p.mul=TRUE)


###################################################
### code chunk number 76: compareGroups_vignette.rnw:886-887
###################################################
createTable(update(res, subset= year!=2005), show.ratio=TRUE, show.p.overall=FALSE)


###################################################
### code chunk number 77: compareGroups_vignette.rnw:892-894
###################################################
createTable(compareGroups(tcv ~  age + sex, data=regicor), show.ratio=TRUE, 
              show.p.overall=FALSE)


###################################################
### code chunk number 78: compareGroups_vignette.rnw:903-905
###################################################
createTable(compareGroups(tcv ~  age + sex, data=regicor), show.ratio=TRUE, 
             digits.ratio= 3)


###################################################
### code chunk number 79: compareGroups_vignette.rnw:916-919
###################################################
restab1 <- createTable(compareGroups(year~ age + sex, data=regicor))
restab2 <- createTable(compareGroups(year~ smoker + triglyc, data=regicor))
rbind("Epidemiological"=restab1,"History"=restab2)


###################################################
### code chunk number 80: compareGroups_vignette.rnw:933-934
###################################################
rbind("Epidemiological"=restab1,"History"=restab2)[c(1,4)]


###################################################
### code chunk number 81: compareGroups_vignette.rnw:939-940
###################################################
rbind("Epidemiological"=restab1,"History"=restab2)[c(4,3,2,1)]


###################################################
### code chunk number 82: compareGroups_vignette.rnw:952-957
###################################################
res<-compareGroups(year ~ age +  smoker + sbp + histhtn +triglyc , data=regicor)
alltab <- createTable(res,  show.p.overall = FALSE)
femaletab <- createTable(update(res,subset=sex=='Female'), show.p.overall = FALSE)
maletab <- createTable(update(res,subset=sex=='Male'), show.p.overall = FALSE)
cbind("ALL"=alltab,"FEMALE"=femaletab,"MALE"=maletab)


###################################################
### code chunk number 83: compareGroups_vignette.rnw:962-963
###################################################
cbind(alltab,femaletab,maletab,caption=NULL)


###################################################
### code chunk number 84: compareGroups_vignette.rnw:968-969
###################################################
cbind(alltab,femaletab,maletab)


###################################################
### code chunk number 85: compareGroups_vignette.rnw:984-986
###################################################
print(createTable(compareGroups(year ~ age +  smoker + sbp, data=regicor)), 
       which.table='both')


###################################################
### code chunk number 86: compareGroups_vignette.rnw:991-992
###################################################
print(createTable(compareGroups(year ~ age +  smoker + sbp, data=regicor)), nmax=FALSE)


###################################################
### code chunk number 87: compareGroups_vignette.rnw:997-998
###################################################
summary(createTable(compareGroups(year ~ age +  smoker + sbp, data=regicor)))


###################################################
### code chunk number 88: compareGroups_vignette.rnw:1003-1007
###################################################
res<-compareGroups(year ~ age +  smoker + sbp + txhtn, data=regicor)
restab<-createTable(res, type=1, show.ratio=TRUE, show.p.overall=FALSE)
restab
update(restab, show.n=TRUE)


###################################################
### code chunk number 89: compareGroups_vignette.rnw:1012-1013
###################################################
update(restab, x = update(res, subset=c(sex=='Male')), show.n=TRUE)


###################################################
### code chunk number 90: compareGroups_vignette.rnw:1020-1021
###################################################
createTable(compareGroups(year~ age + sex + smoker +sbp, data=regicor))


###################################################
### code chunk number 91: compareGroups_vignette.rnw:1023-1024
###################################################
createTable(compareGroups(year~ age + sex + smoker +sbp, data=regicor))[1:2]


###################################################
### code chunk number 92: compareGroups_vignette.rnw:1071-1073
###################################################
restab<-createTable(compareGroups(year~ age + sex, data=regicor))
export2latex(restab)


