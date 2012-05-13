### R code from vignette source 'compareGroups_vignette.rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: compareGroups_vignette.rnw:46-48
###################################################
rm(list=ls())
options(warn = -1)


###################################################
### code chunk number 2: compareGroups_vignette.rnw:84-86
###################################################
dir.create("./univar",showWarnings = FALSE)
dir.create("./bivar",showWarnings = FALSE)


###################################################
### code chunk number 3: compareGroups_vignette.rnw:103-104
###################################################
library(compareGroups)


###################################################
### code chunk number 4: compareGroups_vignette.rnw:148-150
###################################################
data(regicor)
head(regicor)


###################################################
### code chunk number 5: compareGroups_vignette.rnw:155-162
###################################################
dicc<-data.frame(
"Name"=I(names(regicor)),
"Label"=I(unlist(lapply(regicor,label))),
"Codes"=I(unlist(lapply(regicor,function(x) paste(levels(x),collapse="; "))))
)
dicc$Codes<-sub(">=","$\\\\geq$",dicc$Codes)  
print(xtable(dicc,align=rep("l",4)),include.rownames=FALSE,size="small",tabular.environment="longtable", sanitize.text.function=function(x) x)


###################################################
### code chunk number 6: compareGroups_vignette.rnw:190-195
###################################################
regicor$tdeath <- with(regicor, Surv(todeath, death == 'Yes'))
label(regicor$tdeath) <- "Mortality"

regicor$tcv <- with(regicor, Surv(tocv, cv == 'Yes'))
label(regicor$tcv) <- "Cardiovascular"


###################################################
### code chunk number 7: compareGroups_vignette.rnw:214-215
###################################################
compareGroups(year ~ . , data=regicor)


###################################################
### code chunk number 8: compareGroups_vignette.rnw:226-227
###################################################
compareGroups(year ~ . -id, data=regicor)


###################################################
### code chunk number 9: compareGroups_vignette.rnw:234-236
###################################################
res<-compareGroups(year ~ age + sex + smoker + triglyc , data=regicor)
res


###################################################
### code chunk number 10: compareGroups_vignette.rnw:257-258
###################################################
compareGroups(year ~ age + smoker + triglyc, data=regicor, subset = sex=='Male')


###################################################
### code chunk number 11: compareGroups_vignette.rnw:265-266
###################################################
options(width=80)


###################################################
### code chunk number 12: compareGroups_vignette.rnw:268-270
###################################################
compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
            selec = list(triglyc=txchol=='No', sbp=txhtn=='No'))


###################################################
### code chunk number 13: compareGroups_vignette.rnw:272-274
###################################################
compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
            selec = list(triglyc=txchol=='No', sbp=txhtn=='No'))


###################################################
### code chunk number 14: compareGroups_vignette.rnw:279-280
###################################################
options(width=80)


###################################################
### code chunk number 15: compareGroups_vignette.rnw:282-285
###################################################
compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
    selec = list(triglyc=txchol=='No', sbp=txhtn=='No'), 
    subset = sex=='Male')


###################################################
### code chunk number 16: compareGroups_vignette.rnw:288-292
###################################################
options(width=80)
compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
    selec = list(triglyc=txchol=='No', sbp=txhtn=='No'), 
    subset = sex=='Male')


###################################################
### code chunk number 17: compareGroups_vignette.rnw:297-298
###################################################
options(width=80)


###################################################
### code chunk number 18: compareGroups_vignette.rnw:300-302
###################################################
compareGroups(year ~ age + smoker + triglyc + triglyc, data=regicor, 
	selec = list(triglyc.1=txchol=='No'))


###################################################
### code chunk number 19: compareGroups_vignette.rnw:316-317
###################################################
options(width=80)


###################################################
### code chunk number 20: compareGroups_vignette.rnw:319-320
###################################################
compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, method = c(triglyc=2))


###################################################
### code chunk number 21: compareGroups_vignette.rnw:322-323
###################################################
options(width=120)


###################################################
### code chunk number 22: compareGroups_vignette.rnw:338-339
###################################################
options(width=80)


###################################################
### code chunk number 23: compareGroups_vignette.rnw:341-343
###################################################
compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
              method = c(triglyc=NA), alpha= 0.01)


###################################################
### code chunk number 24: compareGroups_vignette.rnw:345-346
###################################################
options(width=120)


###################################################
### code chunk number 25: compareGroups_vignette.rnw:355-359
###################################################
cuts<-"lo:40=1; 41:45=2; 46:50=3; 51:55=4; 56:60=5; 61:65=6; 66:hi=7"
regicor$age7gr<-car::recode(regicor$age, cuts)
compareGroups(year ~ age7gr, data=regicor, method = c(age7gr=NA))
compareGroups(year ~ age7gr, data=regicor, method = c(age7gr=NA), min.dis=8)


###################################################
### code chunk number 26: compareGroups_vignette.rnw:375-376
###################################################
compareGroups(age7gr ~ smoker + triglyc + sbp, data=regicor, max.ylev=7)


###################################################
### code chunk number 27: compareGroups_vignette.rnw:381-382
###################################################
compareGroups(year ~ smoker + age7gr, method= (age7gr=3), data=regicor, max.xlev=5)


###################################################
### code chunk number 28: compareGroups_vignette.rnw:401-402
###################################################
compareGroups(year ~ smoker + triglyc + sbp, data=regicor, include.label= FALSE)


###################################################
### code chunk number 29: compareGroups_vignette.rnw:409-410
###################################################
options(width=80)


###################################################
### code chunk number 30: compareGroups_vignette.rnw:412-415
###################################################
resu1<-compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
                     method = c(triglyc=2))
createTable(resu1)


###################################################
### code chunk number 31: compareGroups_vignette.rnw:422-425
###################################################
resu2<-compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
                     method = c(triglyc=2), Q1=0.025, Q3=0.975)
createTable(resu2)


###################################################
### code chunk number 32: compareGroups_vignette.rnw:432-433
###################################################
options(width=80)


###################################################
### code chunk number 33: compareGroups_vignette.rnw:435-437
###################################################
compareGroups(year ~ age + smoker + triglyc + sbp, data=regicor, 
              method = c(triglyc=2), Q1=0, Q3=1)


###################################################
### code chunk number 34: compareGroups_vignette.rnw:439-440
###################################################
options(width=120)


###################################################
### code chunk number 35: compareGroups_vignette.rnw:447-451
###################################################
regicor$smk<-regicor$smoker
levels(regicor$smk)<- c("Never smoker", "Current or former < 1y", 
                        "Never or former >= 1y", "Unknown")
cbind(table(regicor$smk))


###################################################
### code chunk number 36: compareGroups_vignette.rnw:483-484
###################################################
compareGroups(year ~ age + smk + sbp, data=regicor, simplify=TRUE)


###################################################
### code chunk number 37: compareGroups_vignette.rnw:496-499
###################################################
res<-compareGroups(year ~ age + sex + smoker + txchol + triglyc + sbp, 
                   method = c(triglyc=2), data=regicor)
summary(res[c(1, 3, 4)])


###################################################
### code chunk number 38: compareGroups_vignette.rnw:512-513
###################################################
plot(res[c(1,2)], file="./univar/")


###################################################
### code chunk number 39: compareGroups_vignette.rnw:531-532
###################################################
plot(res[c(1,2)], bivar=TRUE, file="./bivar/")


###################################################
### code chunk number 40: compareGroups_vignette.rnw:556-558
###################################################
res<-compareGroups(year ~ age + sex + triglyc, data=regicor)
res


###################################################
### code chunk number 41: compareGroups_vignette.rnw:563-564
###################################################
options(width=80)


###################################################
### code chunk number 42: compareGroups_vignette.rnw:566-569
###################################################
res<-update(res, . ~. - sex + smoker +  chol, subset = sex=='Male', 
                method = c(triglyc=2, chol = 2), 
                selec = list(triglyc=txchol=='No', sbp=txhtn=='No'))


###################################################
### code chunk number 43: compareGroups_vignette.rnw:571-572
###################################################
options(width=120)


###################################################
### code chunk number 44: compareGroups_vignette.rnw:574-575
###################################################
res


###################################################
### code chunk number 45: compareGroups_vignette.rnw:591-593
###################################################
res1<-compareGroups(sex ~ age + smoker + txchol + sbp, data=regicor, ref=1)
createTable(res1, show.ratio=TRUE)


###################################################
### code chunk number 46: compareGroups_vignette.rnw:598-599
###################################################
options(width=80)


###################################################
### code chunk number 47: compareGroups_vignette.rnw:601-604
###################################################
res2<-compareGroups(sex ~ age + smoker + txchol + sbp, data=regicor, 
                    ref=c(smoker=1, txchol=2))
createTable(res2, show.ratio=TRUE)


###################################################
### code chunk number 48: compareGroups_vignette.rnw:609-610
###################################################
options(width=120)


###################################################
### code chunk number 49: compareGroups_vignette.rnw:617-619
###################################################
res<-compareGroups(sex ~ age + smoker + txchol + sbp, data=regicor, ref.no='NO')
createTable(res, show.ratio=TRUE)


###################################################
### code chunk number 50: compareGroups_vignette.rnw:628-630
###################################################
res<-compareGroups(sex ~ age + txchol + sbp, data=regicor)
createTable(res, show.ratio=TRUE)


###################################################
### code chunk number 51: compareGroups_vignette.rnw:635-636
###################################################
options(width=80)


###################################################
### code chunk number 52: compareGroups_vignette.rnw:638-640
###################################################
res<-compareGroups(sex ~ age + txchol + sbp, data=regicor, fact.ratio= c(age=10, sbp=100))
createTable(res, show.ratio=TRUE)


###################################################
### code chunk number 53: compareGroups_vignette.rnw:642-643
###################################################
options(width=120)


###################################################
### code chunk number 54: compareGroups_vignette.rnw:652-654
###################################################
res<-compareGroups(sex ~ histchol + txchol + sbp, data=regicor)
createTable(res, show.ratio=TRUE)


###################################################
### code chunk number 55: compareGroups_vignette.rnw:659-661
###################################################
res<-compareGroups(sex ~ histchol + txchol + sbp, data=regicor, ref.y=2)
createTable(res, show.ratio=TRUE)


###################################################
### code chunk number 56: compareGroups_vignette.rnw:674-676
###################################################
plot(compareGroups(tcv ~ txchol, data=regicor), bivar=TRUE, file="./bivar/")
plot(compareGroups(tcv ~ sbp, data=regicor), bivar=TRUE, file="./bivar/")


###################################################
### code chunk number 57: compareGroups_vignette.rnw:703-704
###################################################
options(width=80)


###################################################
### code chunk number 58: compareGroups_vignette.rnw:706-709
###################################################
res<-compareGroups(sex ~  age + tdeath + tcv, timemax=c(tcv=365.25*5, tdeath=365.25*3), 
                   data=regicor)
res


###################################################
### code chunk number 59: compareGroups_vignette.rnw:711-712
###################################################
options(width=120)


###################################################
### code chunk number 60: compareGroups_vignette.rnw:721-723
###################################################
plot(res[2], file="./univar/")
plot(res[2], bivar=TRUE, file="./bivar/")


###################################################
### code chunk number 61: compareGroups_vignette.rnw:748-749
###################################################
options(width=100)


###################################################
### code chunk number 62: compareGroups_vignette.rnw:752-755
###################################################
res<-compareGroups(year ~ age + sex + smoker + triglyc , method= c(triglyc=2), 
       data=regicor)
restab<-createTable(res)


###################################################
### code chunk number 63: compareGroups_vignette.rnw:760-761
###################################################
print(restab,which.table='descr')


###################################################
### code chunk number 64: compareGroups_vignette.rnw:766-767
###################################################
print(restab,which.table='avail')


###################################################
### code chunk number 65: compareGroups_vignette.rnw:783-784
###################################################
update(restab, hide = c(sex='Male'))


###################################################
### code chunk number 66: compareGroups_vignette.rnw:793-795
###################################################
res<-compareGroups(year ~ age + sex + histhtn + txhtn, data=regicor)
createTable(res, hide.no='no')


###################################################
### code chunk number 67: compareGroups_vignette.rnw:804-805
###################################################
createTable(res, digits= c(age=2, sex = 3))


###################################################
### code chunk number 68: compareGroups_vignette.rnw:814-815
###################################################
createTable(res, type=1)


###################################################
### code chunk number 69: compareGroups_vignette.rnw:820-821
###################################################
createTable(res, type=3)


###################################################
### code chunk number 70: compareGroups_vignette.rnw:833-834
###################################################
createTable(res, show.n=TRUE)


###################################################
### code chunk number 71: compareGroups_vignette.rnw:841-842
###################################################
createTable(res, show.descr=FALSE)


###################################################
### code chunk number 72: compareGroups_vignette.rnw:849-850
###################################################
createTable(res, show.all=TRUE)


###################################################
### code chunk number 73: compareGroups_vignette.rnw:856-857
###################################################
createTable(res, show.p.overall=FALSE)


###################################################
### code chunk number 74: compareGroups_vignette.rnw:864-865
###################################################
createTable(res, show.p.trend=TRUE)


###################################################
### code chunk number 75: compareGroups_vignette.rnw:875-876
###################################################
createTable(res, show.p.mul=TRUE)


###################################################
### code chunk number 76: compareGroups_vignette.rnw:885-886
###################################################
createTable(update(res, subset= year!=2005), show.ratio=TRUE)


###################################################
### code chunk number 77: compareGroups_vignette.rnw:891-892
###################################################
createTable(compareGroups(tcv ~  age + sex, data=regicor), show.ratio=TRUE)


###################################################
### code chunk number 78: compareGroups_vignette.rnw:901-903
###################################################
createTable(compareGroups(tcv ~  age + sex, data=regicor), show.ratio=TRUE, 
             digits.ratio= 3)


###################################################
### code chunk number 79: compareGroups_vignette.rnw:914-917
###################################################
restab1 <- createTable(compareGroups(year~ age + sex, data=regicor))
restab2 <- createTable(compareGroups(year~ smoker + triglyc, data=regicor))
rbind("Epidemiological"=restab1,"History"=restab2)


###################################################
### code chunk number 80: compareGroups_vignette.rnw:931-932
###################################################
rbind("Epidemiological"=restab1,"History"=restab2)[c(1,4)]


###################################################
### code chunk number 81: compareGroups_vignette.rnw:937-938
###################################################
rbind("Epidemiological"=restab1,"History"=restab2)[c(4,3,2,1)]


###################################################
### code chunk number 82: compareGroups_vignette.rnw:950-955
###################################################
res<-compareGroups(year ~ age +  smoker + sbp + histhtn +triglyc , data=regicor)
alltab <- createTable(res,  show.p.overall = FALSE)
femaletab <- createTable(update(res,subset=sex=='Female'), show.p.overall = FALSE)
maletab <- createTable(update(res,subset=sex=='Male'), show.p.overall = FALSE)
cbind("ALL"=alltab,"FEMALE"=femaletab,"MALE"=maletab)


###################################################
### code chunk number 83: compareGroups_vignette.rnw:960-961
###################################################
cbind(alltab,femaletab,maletab,caption=NULL)


###################################################
### code chunk number 84: compareGroups_vignette.rnw:966-967
###################################################
cbind(alltab,femaletab,maletab)


###################################################
### code chunk number 85: compareGroups_vignette.rnw:982-984
###################################################
print(createTable(compareGroups(year ~ age +  smoker + sbp, data=regicor)), 
       which.table='both')


###################################################
### code chunk number 86: compareGroups_vignette.rnw:989-990
###################################################
print(createTable(compareGroups(year ~ age +  smoker + sbp, data=regicor)), nmax=FALSE)


###################################################
### code chunk number 87: compareGroups_vignette.rnw:995-996
###################################################
summary(createTable(compareGroups(year ~ age +  smoker + sbp, data=regicor)))


###################################################
### code chunk number 88: compareGroups_vignette.rnw:1001-1005
###################################################
res<-compareGroups(year ~ age +  smoker + sbp + txhtn, data=regicor)
restab<-createTable(res, type=1, show.ratio=TRUE )
restab
update(restab, show.n=TRUE)


###################################################
### code chunk number 89: compareGroups_vignette.rnw:1010-1011
###################################################
update(restab, x = update(res, subset=c(sex=='Male')), show.n=TRUE)


###################################################
### code chunk number 90: compareGroups_vignette.rnw:1018-1019
###################################################
createTable(compareGroups(year~ age + sex + smoker +sbp, data=regicor))


###################################################
### code chunk number 91: compareGroups_vignette.rnw:1021-1022
###################################################
createTable(compareGroups(year~ age + sex + smoker +sbp, data=regicor))[1:2, ]


###################################################
### code chunk number 92: compareGroups_vignette.rnw:1069-1071
###################################################
restab<-createTable(compareGroups(year~ age + sex, data=regicor))
export2latex(restab)


