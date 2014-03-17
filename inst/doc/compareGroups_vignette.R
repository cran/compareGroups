### R code from vignette source 'compareGroups_vignette.rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: compareGroups_vignette.rnw:104-105
###################################################
library(compareGroups)


###################################################
### code chunk number 2: compareGroups_vignette.rnw:150-151
###################################################
data(predimed)


###################################################
### code chunk number 3: compareGroups_vignette.rnw:158-166
###################################################
dicc<-data.frame(
"Name"=I(names(predimed)),
"Label"=I(unlist(lapply(predimed,label))),
"Codes"=I(unlist(lapply(predimed,function(x) paste(levels(x),collapse="; "))))
)
dicc$Codes<-sub(">=","$\\\\geq$",dicc$Codes)  
#print(xtable(dicc,align=rep("l",4)),include.rownames=FALSE,size="small",tabular.environment="longtable", sanitize.text.function=function(x) x)
print(xtable(dicc,align=rep("l",4)),include.rownames=FALSE,size="small", sanitize.text.function=function(x) x)


###################################################
### code chunk number 4: compareGroups_vignette.rnw:191-193
###################################################
predimed$tmain <- with(predimed, Surv(toevent, event == 'Yes'))
label(predimed$tmain) <- "AMI, stroke, or CV Death"


###################################################
### code chunk number 5: compareGroups_vignette.rnw:213-214
###################################################
compareGroups(group ~ . , data=predimed)


###################################################
### code chunk number 6: compareGroups_vignette.rnw:225-226
###################################################
compareGroups(group ~ . -toevent - event, data=predimed)


###################################################
### code chunk number 7: compareGroups_vignette.rnw:233-235
###################################################
res<-compareGroups(group ~ age + sex + smoke + waist + hormo, data=predimed)
res


###################################################
### code chunk number 8: compareGroups_vignette.rnw:254-256
###################################################
compareGroups(group ~ age + smoke + waist + hormo, data=predimed, 
              subset = sex=='Female')


###################################################
### code chunk number 9: compareGroups_vignette.rnw:263-264
###################################################
options(width=80,keep.source=FALSE)


###################################################
### code chunk number 10: compareGroups_vignette.rnw:267-269
###################################################
compareGroups(group ~ age + sex + smoke + waist + hormo, data=predimed, 
              selec = list(hormo= sex=="Female", waist = waist>20 ))


###################################################
### code chunk number 11: compareGroups_vignette.rnw:274-275
###################################################
options(width=120,keep.source=FALSE)


###################################################
### code chunk number 12: compareGroups_vignette.rnw:278-280
###################################################
compareGroups(group ~ age + smoke + waist + hormo, data=predimed, 
              selec = list(waist= !is.na(hormo)), subset = sex=="Female")


###################################################
### code chunk number 13: compareGroups_vignette.rnw:286-287
###################################################
options(width=80,keep.source=TRUE)


###################################################
### code chunk number 14: compareGroups_vignette.rnw:289-291
###################################################
compareGroups(group ~ age + sex + bmi + bmi + waist + hormo, data=predimed, 
              selec = list(bmi.1=!is.na(hormo)))


###################################################
### code chunk number 15: compareGroups_vignette.rnw:304-305
###################################################
options(width=80,keep.source=FALSE)


###################################################
### code chunk number 16: compareGroups_vignette.rnw:307-309
###################################################
compareGroups(group ~ age + smoke + waist + hormo, data=predimed, 
              method = c(waist=2))


###################################################
### code chunk number 17: compareGroups_vignette.rnw:311-312
###################################################
options(width=120,keep.source=FALSE)


###################################################
### code chunk number 18: compareGroups_vignette.rnw:327-328
###################################################
options(width=80,keep.source=FALSE)


###################################################
### code chunk number 19: compareGroups_vignette.rnw:330-332
###################################################
compareGroups(group ~ age + smoke + waist + hormo, data=predimed, 
              method = c(waist=NA), alpha= 0.01)


###################################################
### code chunk number 20: compareGroups_vignette.rnw:334-335
###################################################
options(width=120,keep.source=FALSE)


###################################################
### code chunk number 21: compareGroups_vignette.rnw:344-348
###################################################
cuts<-"lo:55=1; 56:60=2; 61:65=3; 66:70=4; 71:75=5; 76:80=6; 81:hi=7"
predimed$age7gr<-car::recode(predimed$age, cuts)
compareGroups(group ~ age7gr, data=predimed, method = c(age7gr=NA))
compareGroups(group ~ age7gr, data=predimed, method = c(age7gr=NA), min.dis=8)


###################################################
### code chunk number 22: compareGroups_vignette.rnw:364-365
###################################################
compareGroups(age7gr ~ sex + bmi + waist, data=predimed, max.ylev=7)


###################################################
### code chunk number 23: compareGroups_vignette.rnw:370-371
###################################################
compareGroups(group ~ sex + age7gr, method= (age7gr=3), data=predimed, max.xlev=5)


###################################################
### code chunk number 24: compareGroups_vignette.rnw:389-391
###################################################
compareGroups(group ~ age + smoke + waist + hormo, data=predimed, 
              include.label= FALSE)


###################################################
### code chunk number 25: compareGroups_vignette.rnw:398-399
###################################################
options(width=80, keep.source=FALSE)


###################################################
### code chunk number 26: compareGroups_vignette.rnw:402-405
###################################################
resu1<-compareGroups(group ~ age + waist, data=predimed, 
                       method = c(waist=2))
createTable(resu1)


###################################################
### code chunk number 27: compareGroups_vignette.rnw:412-415
###################################################
resu2<-compareGroups(group ~ age + smoke + waist + hormo, data=predimed, 
                       method = c(waist=2), Q1=0.025, Q3=0.975)
createTable(resu2)


###################################################
### code chunk number 28: compareGroups_vignette.rnw:422-423
###################################################
options(width=120,keep.source=FALSE)


###################################################
### code chunk number 29: compareGroups_vignette.rnw:427-428
###################################################
options(width=80,keep.source=FALSE)


###################################################
### code chunk number 30: compareGroups_vignette.rnw:430-432
###################################################
compareGroups(group ~ age + smoke + waist + hormo, data=predimed, 
                method = c(waist=2), Q1=0, Q3=1)


###################################################
### code chunk number 31: compareGroups_vignette.rnw:434-435
###################################################
options(width=120,keep.source=FALSE)


###################################################
### code chunk number 32: compareGroups_vignette.rnw:442-446
###################################################
predimed$smk<-predimed$smoke
levels(predimed$smk)<- c("Never smoker", "Current or former < 1y", "Never or former >= 1y", "Unknown")
label(predimed$smk)<-"Smoking 4 cat."
cbind(table(predimed$smk))


###################################################
### code chunk number 33: compareGroups_vignette.rnw:479-480
###################################################
compareGroups(group ~ age + smk, data=predimed, simplify=FALSE)


###################################################
### code chunk number 34: compareGroups_vignette.rnw:509-512
###################################################
res<-compareGroups(group ~ age + sex + smoke + waist + hormo, method = c(waist=2),
                   data=predimed)
summary(res[c(1, 2, 4)])


###################################################
### code chunk number 35: compareGroups_vignette.rnw:525-526
###################################################
plot(res[c(1,2)], file="./figures/univar/")


###################################################
### code chunk number 36: compareGroups_vignette.rnw:544-545
###################################################
plot(res[c(1,2)], bivar=TRUE, file="./figures/bivar/")


###################################################
### code chunk number 37: compareGroups_vignette.rnw:569-571
###################################################
res<-compareGroups(group ~ age + sex + smoke + waist + hormo, data=predimed)
res


###################################################
### code chunk number 38: compareGroups_vignette.rnw:576-577
###################################################
options(width=80,keep.source=FALSE)


###################################################
### code chunk number 39: compareGroups_vignette.rnw:579-581
###################################################
res<-update(res, . ~. - sex +  bmi + toevent, subset = sex=='Female', 
              method = c(waist=2, tovent=2), selec = list(bmi=!is.na(hormo)))


###################################################
### code chunk number 40: compareGroups_vignette.rnw:583-584
###################################################
options(width=120,keep.source=FALSE)


###################################################
### code chunk number 41: compareGroups_vignette.rnw:586-587
###################################################
res


###################################################
### code chunk number 42: compareGroups_vignette.rnw:603-605
###################################################
res1<-compareGroups(htn ~ age + sex + bmi + smoke, data=predimed, ref=1)
createTable(res1, show.ratio=TRUE)


###################################################
### code chunk number 43: compareGroups_vignette.rnw:610-611
###################################################
options(width=80,keep.source=FALSE)


###################################################
### code chunk number 44: compareGroups_vignette.rnw:613-616
###################################################
res2<-compareGroups(htn ~ age + sex + bmi + smoke, data=predimed, 
                    ref=c(smoke=1, sex=2))
createTable(res2, show.ratio=TRUE)


###################################################
### code chunk number 45: compareGroups_vignette.rnw:621-622
###################################################
options(width=120,keep.source=FALSE)


###################################################
### code chunk number 46: compareGroups_vignette.rnw:629-632
###################################################
res<-compareGroups(htn ~ age + sex + bmi + hormo + hyperchol, data=predimed, 
                   ref.no='NO')
createTable(res, show.ratio=TRUE)


###################################################
### code chunk number 47: compareGroups_vignette.rnw:641-643
###################################################
res<-compareGroups(htn ~ age + bmi, data=predimed)
createTable(res, show.ratio=TRUE)


###################################################
### code chunk number 48: compareGroups_vignette.rnw:648-649
###################################################
options(width=80,keep.source=FALSE)


###################################################
### code chunk number 49: compareGroups_vignette.rnw:651-654
###################################################
res<-compareGroups(htn ~ age + bmi, data=predimed, 
                   fact.ratio= c(age=10, bmi=2))
createTable(res, show.ratio=TRUE)


###################################################
### code chunk number 50: compareGroups_vignette.rnw:656-657
###################################################
options(width=120,keep.source=FALSE)


###################################################
### code chunk number 51: compareGroups_vignette.rnw:666-668
###################################################
res<-compareGroups(htn ~ age + sex + bmi + hyperchol, data=predimed)
createTable(res, show.ratio=TRUE)


###################################################
### code chunk number 52: compareGroups_vignette.rnw:674-676
###################################################
res<-compareGroups(htn ~ age + sex + bmi + hyperchol, data=predimed, ref.y=2)
createTable(res, show.ratio=TRUE)


###################################################
### code chunk number 53: compareGroups_vignette.rnw:690-692
###################################################
plot(compareGroups(tmain ~ sex, data=predimed), bivar=TRUE, file="./figures/bivar/")
plot(compareGroups(tmain ~ age, data=predimed), bivar=TRUE, file="./figures/bivar/")


###################################################
### code chunk number 54: compareGroups_vignette.rnw:722-723
###################################################
options(width=80,keep.source=FALSE)


###################################################
### code chunk number 55: compareGroups_vignette.rnw:725-728
###################################################
res<-compareGroups(sex ~  age + tmain, timemax=c(tmain=3),
                   data=predimed)
res


###################################################
### code chunk number 56: compareGroups_vignette.rnw:730-731
###################################################
options(width=120,keep.source=FALSE)


###################################################
### code chunk number 57: compareGroups_vignette.rnw:740-742
###################################################
plot(res[2], file="./figures/univar/")
plot(res[2], bivar=TRUE, file="./figures/bivar/")


###################################################
### code chunk number 58: compareGroups_vignette.rnw:766-767
###################################################
options(width=100,keep.source=FALSE)


###################################################
### code chunk number 59: compareGroups_vignette.rnw:770-773
###################################################
res<-compareGroups(group ~ age + sex + smoke + waist + hormo, data=predimed, 
              selec = list(hormo=sex=="Female"))
restab<-createTable(res)


###################################################
### code chunk number 60: compareGroups_vignette.rnw:778-779
###################################################
print(restab,which.table='descr')


###################################################
### code chunk number 61: compareGroups_vignette.rnw:784-785
###################################################
print(restab,which.table='avail')


###################################################
### code chunk number 62: compareGroups_vignette.rnw:800-801
###################################################
update(restab, hide = c(sex="Male"))


###################################################
### code chunk number 63: compareGroups_vignette.rnw:810-812
###################################################
res<-compareGroups(group ~ age + sex + htn + diab, data=predimed)
createTable(res, hide.no='no', hide = c(sex="Male"))


###################################################
### code chunk number 64: compareGroups_vignette.rnw:821-822
###################################################
createTable(res, digits= c(age=2, sex = 3))


###################################################
### code chunk number 65: compareGroups_vignette.rnw:831-832
###################################################
createTable(res, type=1)


###################################################
### code chunk number 66: compareGroups_vignette.rnw:837-838
###################################################
createTable(res, type=3)


###################################################
### code chunk number 67: compareGroups_vignette.rnw:850-851
###################################################
createTable(res, show.n=TRUE)


###################################################
### code chunk number 68: compareGroups_vignette.rnw:858-859
###################################################
createTable(res, show.descr=FALSE)


###################################################
### code chunk number 69: compareGroups_vignette.rnw:866-867
###################################################
createTable(res, show.all=TRUE)


###################################################
### code chunk number 70: compareGroups_vignette.rnw:873-874
###################################################
createTable(res, show.p.overall=FALSE)


###################################################
### code chunk number 71: compareGroups_vignette.rnw:881-882
###################################################
createTable(res, show.p.trend=TRUE)


###################################################
### code chunk number 72: compareGroups_vignette.rnw:892-893
###################################################
createTable(res, show.p.mul=TRUE)


###################################################
### code chunk number 73: compareGroups_vignette.rnw:899-900
###################################################
createTable(update(res, subset= group!="Control diet"), show.ratio=TRUE)


###################################################
### code chunk number 74: compareGroups_vignette.rnw:905-907
###################################################
createTable(compareGroups(tmain ~  group + age + sex, data=predimed), 
            show.ratio=TRUE)


###################################################
### code chunk number 75: compareGroups_vignette.rnw:916-918
###################################################
createTable(compareGroups(tmain ~  group + age + sex, data=predimed), 
            show.ratio=TRUE, digits.ratio= 3)


###################################################
### code chunk number 76: compareGroups_vignette.rnw:928-931
###################################################
restab1 <- createTable(compareGroups(group ~ age + sex, data=predimed))
restab2 <- createTable(compareGroups(group ~ bmi + smoke, data=predimed))
rbind("Non-modifiable risk factors"=restab1, "Modifiable risk factors"=restab2)


###################################################
### code chunk number 77: compareGroups_vignette.rnw:944-945
###################################################
rbind("Non-modifiable"=restab1,"Modifiable"=restab2)[c(1,4)]


###################################################
### code chunk number 78: compareGroups_vignette.rnw:950-951
###################################################
rbind("Modifiable"=restab1,"Non-modifiable"=restab2)[c(4,3,2,1)]


###################################################
### code chunk number 79: compareGroups_vignette.rnw:961-966
###################################################
res<-compareGroups(group ~ age +  smoke + bmi + htn , data=predimed)
alltab <- createTable(res,  show.p.overall = FALSE)
femaletab <- createTable(update(res,subset=sex=='Female'), show.p.overall = FALSE)
maletab <- createTable(update(res,subset=sex=='Male'), show.p.overall = FALSE)
cbind("ALL"=alltab,"FEMALE"=femaletab,"MALE"=maletab)


###################################################
### code chunk number 80: compareGroups_vignette.rnw:971-972
###################################################
cbind(alltab,femaletab,maletab,caption=NULL)


###################################################
### code chunk number 81: compareGroups_vignette.rnw:977-978
###################################################
cbind(alltab,femaletab,maletab)


###################################################
### code chunk number 82: compareGroups_vignette.rnw:991-993
###################################################
print(createTable(compareGroups(group ~ age + sex + smoke + waist + hormo,
                                data=predimed)), which.table='both')


###################################################
### code chunk number 83: compareGroups_vignette.rnw:996-998
###################################################
print(createTable(compareGroups(group ~ age + sex + smoke + waist + hormo,
                                data=predimed)),  nmax=FALSE)


###################################################
### code chunk number 84: compareGroups_vignette.rnw:1004-1006
###################################################
summary(createTable(compareGroups(group ~ age + sex + smoke + waist + hormo, 
                                  data=predimed)))


###################################################
### code chunk number 85: compareGroups_vignette.rnw:1011-1015
###################################################
res<-compareGroups(group ~ age + sex + smoke + waist + hormo, data=predimed)
restab<-createTable(res, type=1, show.ratio=TRUE )
restab
update(restab, show.n=TRUE)


###################################################
### code chunk number 86: compareGroups_vignette.rnw:1021-1022
###################################################
update(restab, x = update(res, subset=c(sex=='Female')), show.n=TRUE)


###################################################
### code chunk number 87: compareGroups_vignette.rnw:1033-1034
###################################################
createTable(compareGroups(group ~ age + sex + smoke + waist + hormo, data=predimed))


###################################################
### code chunk number 88: compareGroups_vignette.rnw:1036-1037
###################################################
createTable(compareGroups(group ~ age + sex + bmi, data=predimed))[1:2, ]


###################################################
### code chunk number 89: compareGroups_vignette.rnw:1082-1085
###################################################
restab<-createTable(compareGroups(group ~ age + sex + smoke + waist + hormo, 
                                  data=predimed))
export2latex(restab)


###################################################
### code chunk number 90: compareGroups_vignette.rnw:1122-1124 (eval = FALSE)
###################################################
## ?report   # to know more about report function
## ?regicor  # info about REGICOR data set


###################################################
### code chunk number 91: compareGroups_vignette.rnw:1137-1141
###################################################
# from a compareGroups object
data(regicor)
res <- compareGroups(year ~ .-id, regicor)
missingTable(res)


###################################################
### code chunk number 92: compareGroups_vignette.rnw:1144-1147 (eval = FALSE)
###################################################
## # or from createTable objects
## restab <- createTable(res, hide.no = 'no')
## missingTable(restab)


###################################################
### code chunk number 93: compareGroups_vignette.rnw:1153-1159
###################################################
# first create time-to-cardiovascular event
regicor$tcv<-with(regicor,Surv(tocv,cv=='Yes'))
# create the table
res <- compareGroups(tcv ~ . -id-tocv-cv-todeath-death, regicor, include.miss = TRUE)
restab <- createTable(res, hide.no = 'no')
restab


###################################################
### code chunk number 94: compareGroups_vignette.rnw:1175-1177
###################################################
data(SNPs)
head(SNPs)


###################################################
### code chunk number 95: compareGroups_vignette.rnw:1182-1184
###################################################
res<-compareSNPs(casco ~ snp10001 + snp10002 + snp10003, data=SNPs)
res


###################################################
### code chunk number 96: compareGroups_vignette.rnw:1192-1194
###################################################
res<-compareSNPs(~ snp10001 + snp10002 + snp10003, data=SNPs)
res


###################################################
### code chunk number 97: compareGroups_vignette.rnw:1215-1218
###################################################
export2latex(createTable(compareGroups(group ~  age + sex + smoke + bmi + waist + 
wth + htn + diab + hyperchol + famhist + hormo + p14 + toevent + event,
                          data=predimed), hide.no="No",hide = c(sex="Male")))


###################################################
### code chunk number 98: compareGroups_vignette.rnw:1280-1285
###################################################
export2latex(createTable(compareGroups(htn ~  age + sex + smoke + bmi + waist + 
                              wth + diab + hyperchol + famhist + hormo + 
                            p14 + toevent + event,
                            data=predimed), hide.no="No",hide = c(sex="Male"),
            show.ratio=TRUE, show.descr=FALSE))


###################################################
### code chunk number 99: compareGroups_vignette.rnw:1298-1300
###################################################
export2latex(createTable(compareGroups(tmain ~  group + age + sex, data=predimed), 
            show.ratio=TRUE))


