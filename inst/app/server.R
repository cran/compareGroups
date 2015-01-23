library(shiny, quietly=TRUE)

options(shiny.maxRequestSize = 10e6) # ~10 Mb
.cGroupsWUIEnv <- new.env(parent=emptyenv())

loadhelp <- function(){
  help <- gsub("\t","",readLines("help"))
  starthelp <- which(help=="<cghelptext>") + 1
  endhelp <- which(help=="</cghelptext>") - 1
  helpvar <- help[starthelp - 2]
  hlp <- sapply(1:length(helpvar), function(a) paste(help[starthelp[a]:endhelp[a]],collapse=""))
  names(hlp) <- helpvar
  return(hlp)
}

require(compareGroups)
require(foreign)

wd<-getwd()
setwd(system.file("app", package = "compareGroups"))

shinyServer(function(input, output) {
  ###############
  ## read data ##
  ###############
  dataset<-reactive({
    if (input$exampledata!='Own data'){ # read examples...
      datasetname<-input$exampledata
      if (input$exampledata=='REGICOR'){
        data(regicor)
        dataset <- regicor
      }      
      if (input$exampledata=='PREDIMED'){
        data(predimed)
        dataset <- predimed
      }     
      if (input$exampledata=='SNPS'){
        data(SNPs,package="SNPassoc")
        dataset <- SNPs
      }    
    } else { # read own data
      inFile<-input$files
      if (is.null(inFile)){
        return(invisible(NULL))
      }
      # read TXT
      if (input$datatype=='*.txt'){
        if (input$sep=='o')
          sepchar<-input$sepother
        else
          sepchar<-input$sep      
        if (input$encoding=='default')
          dataset<- try(read.table(inFile$datapath,header=input$header,sep=sepchar,quote=input$quote,dec=input$dechar,na.strings=input$missvalue),silent=TRUE)
        else
          dataset<- try(read.table(inFile$datapath,header=input$header,sep=sepchar,quote=input$quote,dec=input$dechar,na.strings=input$missvalue,encoding=input$encoding),silent=TRUE)        
        if (inherits(dataset,"try-error")){
          cat("Error in reading data\n")
          return(invisible(NULL))      
        }
        if (!is.data.frame(dataset)){
          cat("Data is not a data frame\n")
          return(invisible(NULL))      
        }          
      }
      # read SPSS
      if (input$datatype=='*.sav'){
        if (input$encoding=='default')
          dataset<-try(read.spss(inFile$datapath,to.data.frame=TRUE),silent=TRUE)
        else
          dataset<-try(read.spss(inFile$datapath,to.data.frame=TRUE,reencode=input$encoding),silent=TRUE)
        if (inherits(dataset,"try-error")){
          cat("Error in reading data\n")
          return(invisible(NULL))      
        }
        if (!is.data.frame(dataset)){
          cat("Data is not a data frame\n")
          return(invisible(NULL))      
        }
        vl<-attr(dataset,"variable.labels")
        for (i in 1:ncol(dataset))
          label(dataset[,i])<-vl[i]
      }
      # read R
      if (input$datatype=='*.rda'){
        datasetname <- try(load(inFile$datapath),silent=TRUE)
        if (inherits(datasetname,"try-error")){
          cat("Error in reading data\n")
          return(invisible(NULL))      
        }
        dataset <- get(datasetname)
        if (!is.data.frame(dataset)){
          cat("Data is not a data frame\n")
          return(invisible(NULL))      
        }      
      }
      # read EXCEL
      if (input$datatype=='*.xls'){
        if (is.null(input$tablenames))
          return(invisible(NULL)) 
        library(XLConnect, quietly=TRUE)     
        chn <- try(loadWorkbook(inFile$datapath),silent=TRUE)
        dataset<-try(readWorksheet(chn,sheet=input$tablenames),silent=TRUE)
        if (inherits(dataset,"try-error"))
          return(invisible(NULL))
      }
    }
    if (!is.data.frame(dataset) || nrow(dataset)==0)
      return(invisible(NULL))
    # select variables
    if (exists("selevars",envir=.cGroupsWUIEnv))
      rm(selevars,envir=.cGroupsWUIEnv)
    assign("selevars",names(dataset),envir=.cGroupsWUIEnv)
    # discarted variables
    if (exists("discvars",envir=.cGroupsWUIEnv))
      rm(discvars,envir=.cGroupsWUIEnv)
    assign("discvars",character(),envir=.cGroupsWUIEnv)      
    # store method
    if (exists("method",envir=.cGroupsWUIEnv))
      rm(method,envir=.cGroupsWUIEnv)
    res<-compareGroups(~.,dataset,max.xlev=Inf,max.ylev=Inf,method=NA)
    method<-sapply(res,function(x) paste(attr(x,"method"),collapse=" "))
    method<-ifelse(method=="continuous normal",1,
                   ifelse(method=="continuous non-normal",2,3))
    names(method)<-attr(res,"varnames.orig")
    assign("method",method,envir=.cGroupsWUIEnv)
    # store descdigits
    if (exists("descdigits",envir=.cGroupsWUIEnv))
      rm(descdigits,envir=.cGroupsWUIEnv)
    res<-compareGroups(~.,dataset,max.xlev=Inf,max.ylev=Inf,method=NA)
    descdigits<-rep(NA,length(res))
    names(descdigits)<-attr(res,"varnames.orig")
    assign("descdigits",descdigits,envir=.cGroupsWUIEnv) 
    # store ratiodigits
    if (exists("ratiodigits",envir=.cGroupsWUIEnv))
      rm(ratiodigits,envir=.cGroupsWUIEnv)
    res<-compareGroups(~.,dataset,max.xlev=Inf,max.ylev=Inf,method=NA)
    ratiodigits<-rep(NA,length(res))
    names(ratiodigits)<-attr(res,"varnames.orig")
    assign("ratiodigits",ratiodigits,envir=.cGroupsWUIEnv)     
    # reference category for OR/HR of categorical row-variables
    if (exists("refratiocat",envir=.cGroupsWUIEnv))
      rm(refratiocat,envir=.cGroupsWUIEnv)
    res<-compareGroups(~.,dataset,max.xlev=Inf,max.ylev=Inf,method=NA)
    refratiocat<-rep(1,length(res))
    names(refratiocat)<-attr(res,"varnames.orig")
    assign("refratiocat",refratiocat,envir=.cGroupsWUIEnv) 
    # store factor to be multiplied for continuous variables in computing OR/HR
    if (exists("factratio",envir=.cGroupsWUIEnv))
      rm(factratio,envir=.cGroupsWUIEnv)
    res<-compareGroups(~.,dataset,max.xlev=Inf,max.ylev=Inf,method=NA)
    factratio<-rep(1,length(res))
    names(factratio)<-attr(res,"varnames.orig")
    assign("factratio",factratio,envir=.cGroupsWUIEnv)        
    # store hide
    if (exists("hide",envir=.cGroupsWUIEnv))
      rm(hide,envir=.cGroupsWUIEnv)
    nn<-names(dataset)
    hide<-rep(NA,length(nn))
    names(hide)<-nn
    assign("hide",hide,envir=.cGroupsWUIEnv)
    # store variable subset
    if (exists("varsubset",envir=.cGroupsWUIEnv))
      rm(varsubset,envir=.cGroupsWUIEnv)
    nn<-names(dataset)
    varsubset<-rep(NA,length(nn))
    names(varsubset)<-nn
    assign("varsubset",varsubset,envir=.cGroupsWUIEnv)    
    # return data
    return(dataset)
  })
  
  
  ###############################
  #### check if data is read ####
  ###############################
  
  output$initial <- renderUI({
    if (is.null(dataset()))
      initial <- FALSE
    else
      initial <- TRUE
    checkboxInput("initial",HTML("<font size=2px color='grey'>Data loaded / Reset</font>"),initial)
  })  
  
  ###############################
  #### LOAD OPTIONS #############
  ###############################
  
  output$loadoptions<-renderUI({   
    inFile<-input$files
    if (is.null(input$datatype))
      return(invisible(NULL))
    if (input$datatype!='*.xls' && input$datatype!='*.txt'){   
      return(invisible(NULL))
    } else {
      # EXCELL
      if (input$datatype=='*.xls'){
        if (is.null(inFile))
          return(invisible(NULL))
        chn <- try(loadWorkbook(inFile$datapath),silent=TRUE)
        if (inherits(chn,"try-error"))
          return(invisible(NULL))
        tablenames <- try(getSheets(chn),silent=TRUE)
        if (inherits(tablenames,"try-error") || length(tablenames)==0)
          return(invisible(NULL))
        names(tablenames)<-tablenames
        selectInput("tablenames", "Choose the table to read:", choices = tablenames, selectize=FALSE)
      } else {
        # TXT
        if (input$datatype=='*.txt'){
          div(
            HTML("<hr>"),
            h5("TEXT Options:"),
            checkboxInput('header', 'Has column headers', TRUE),
            div(class="row-fluid",
                HTML('<input class="span2" type="text" id="missvalue" value="" /> Missing Data String (e.g. <i>NA</i>)')
            ),
            div(class="row-fluid",
                div(class="well span5",
                    radioButtons('sep', 'Column Separator', c(Comma=',', Semicolon=';', Tab='\t', Other='o'), 'Comma'),
                    conditionalPanel(
                      condition = "input.sep == 'o'",
                      HTML('<input class="span4" type="text" id="sepother" value="" />')
                    ),
                    radioButtons('dechar', 'Decimal point character', c(Comma=',', Dot='.'), 'Dot')                
                ),
                div(class="well span5",
                    radioButtons('quote', 'Values in Quotes?', c(None='', 'Double'='"', 'Single'="'"), 'Double')
                )
            )
          )
        }
      }
    }
  }) 
  
  
  ###################
  ### create table ##
  ###################
  
  create<-reactive({
    dd<-dataset()
    if (is.null(dd)){
      cat("Data not loaded\n")
      return(invisible(NULL))
    }
    # global subset
    input$changeglobalsubset  
    isolate({
      dd2<-dd
      for (i in 1:ncol(dd2))
        if (is.factor(dd2[,i]))
          dd2[,i]<-as.integer(dd2[,i])
      if (!is.null(input$globalsubset))
        dd2<-try(eval(parse(text=paste("subset(dd2,",input$globalsubset,")",sep=""))),silent=TRUE)
      if (inherits(dd2,"try-error")){
        cat("Subset not correct\n")
        return(invisible(NULL))      
      }  
      if (nrow(dd2)==0){
        cat("No individuals selected\n")
        return(invisible(NULL))      
      }
      dd<-dd[rownames(dd2),]
    })    
    input$changeselevars
    if (!exists("selevars",envir=.cGroupsWUIEnv))
      return(NULL)
    selevars<-get("selevars",envir=.cGroupsWUIEnv,inherits=FALSE) 
    if (is.null(selevars) || length(selevars)==0){
      cat("No variables selected\n")
      return(invisible(NULL)) 
    }
    if (input$resptype=='None'){
      form<-as.formula(paste("~",paste(selevars,collapse="+"),sep=""))
    } else {
      if (input$resptype=='Survival'){
        statusval<-as.numeric(strsplit(input$statuscat,":")[[1]][1])
        cens<-as.integer(dd[,input$varselestatus])==statusval 
        times<-dd[,input$varseletime]
        dd$"respsurv"<-Surv(times,cens)
        label(dd$"respsurv")<-paste("[ ",input$varseletime,"; ",input$varselestatus,"=", levels(as.factor(dd[,input$varselestatus]))[statusval],"]")
        form<-as.formula(paste("respsurv~",paste(selevars,collapse="+"),sep=""))  
      } else {
        form<-as.formula(paste(input$gvar,"~",paste(selevars,collapse="+"),sep=""))
      }
    }
    input$changemethod
    method<-get("method",envir=.cGroupsWUIEnv,inherits=FALSE)
    isolate({
      if (exists("method",envir=.cGroupsWUIEnv) && !is.null(input$method)){
        if (!is.null(input$varselemethodALL) && input$varselemethodALL)
          method[1:length(method)]<-ifelse(input$method=='Normal',1,
                                           ifelse(input$method=='Non-normal',2,
                                                  ifelse(input$method=='Categorical',3,NA)))        
        else
          if (length(input$varselemethod)>0)
            method[input$varselemethod]<-ifelse(input$method=='Normal',1,
                                                ifelse(input$method=='Non-normal',2,
                                                       ifelse(input$method=='Categorical',3,NA)))
        assign("method",method,envir=.cGroupsWUIEnv)
      }
    })
    input$changehide
    hide<-get("hide",envir=.cGroupsWUIEnv,inherits=FALSE)
    isolate({
      if (length(input$varselehide)>0 && exists("hide",envir=.cGroupsWUIEnv) && !is.null(input$hidecat)){
        catval<-as.numeric(strsplit(input$hidecat,":")[[1]][1])
        hide[input$varselehide]<-catval
        assign("hide",hide,envir=.cGroupsWUIEnv)
      }
    })
    input$changedescdigits
    descdigits<-get("descdigits",envir=.cGroupsWUIEnv,inherits=FALSE)
    isolate({
      if (exists("descdigits",envir=.cGroupsWUIEnv) && !is.null(input$descdigits)){
        if (!is.null(input$varseledescdigitsALL) && input$varseledescdigitsALL)
          descdigits[1:length(descdigits)]<-ifelse(input$descdigits==-1,NA,input$descdigits) 
        else
          if (length(input$varseledescdigits)>0)
            descdigits[input$varseledescdigits]<-ifelse(input$descdigits==-1,NA,input$descdigits)
        assign("descdigits",descdigits,envir=.cGroupsWUIEnv)
      }
    }) 
    input$changeratiodigits
    ratiodigits<-get("ratiodigits",envir=.cGroupsWUIEnv,inherits=FALSE)
    isolate({
      if (exists("ratiodigits",envir=.cGroupsWUIEnv) && !is.null(input$ratiodigits)){
        if (!is.null(input$varseleratiodigitsALL) && input$varseleratiodigitsALL)
          ratiodigits[1:length(ratiodigits)]<-ifelse(input$ratiodigits==-1,NA,input$ratiodigits) 
        else
          if (length(input$varseleratiodigits)>0)
            ratiodigits[input$varseleratiodigits]<-ifelse(input$ratiodigits==-1,NA,input$ratiodigits)
        assign("ratiodigits",ratiodigits,envir=.cGroupsWUIEnv)
      }
    })     
    
    input$changeratiocat
    refratiocat<-get("refratiocat",envir=.cGroupsWUIEnv,inherits=FALSE)
    isolate({
      if (length(input$varselerefratio)>0 && exists("refratiocat",envir=.cGroupsWUIEnv) && !is.null(input$refratiocat)){
        catval<-as.numeric(strsplit(input$refratiocat,":")[[1]][1])
        refratiocat[input$varselerefratio]<-catval
        assign("refratiocat",refratiocat,envir=.cGroupsWUIEnv)
      }      
    })  
    input$changefactratio
    factratio<-get("factratio",envir=.cGroupsWUIEnv,inherits=FALSE)
    isolate({
      if (exists("factratio",envir=.cGroupsWUIEnv) && !is.null(input$factratio)){
        if (!is.null(input$varselefactratioALL) && input$varselefactratioALL)
          factratio[1:length(factratio)]<-input$factratio 
        else
          if (length(input$varselefactratio)>0)
            factratio[input$varselefactratio]<-input$factratio
        assign("factratio",factratio,envir=.cGroupsWUIEnv)
      }    
    })
    input$changevarsubset
    varsubset<-get("varsubset",envir=.cGroupsWUIEnv,inherits=FALSE)
    isolate({
      if (exists("varsubset",envir=.cGroupsWUIEnv) && !is.null(input$varsubset)){
        if (!is.null(input$varselevarsubsetALL) && input$varselevarsubsetALL)
          varsubset[1:length(varsubset)]<-input$varsubset 
        else
          if (length(input$varselevarsubset)>0)
            varsubset[input$varselevarsubset]<-input$varsubset
        varsubset<-ifelse(varsubset=='',NA,varsubset)
        assign("varsubset",varsubset,envir=.cGroupsWUIEnv)
      }
    })
    if (any(!is.na(varsubset))){
      dd2<-dd
      for (i in 1:ncol(dd2))
        if (is.factor(dd2[,i]))
          dd2[,i]<-as.integer(dd2[,i])
      for (i in seq_along(varsubset)){
        if (!is.na(varsubset[i])){
          if (is.factor(dd2[,names(varsubset)[i]]))
            dd2[,i]<-as.integer(dd2[,names(varsubset)[i]])
          kk<-!eval(parse(text=paste("with(dd2,",varsubset[i],")",sep="")))
          dd[kk,names(varsubset)[i]]<-NA
        }
      }
    }    
    if (length(input$hideno)==0 || input$hideno=='')
      hideno<-NA
    else
      hideno<-unlist(strsplit(input$hideno,","))
    refno<-hideno
    refy<-if (is.null(input$gvarcat)) 1 else as.numeric(strsplit(input$gvarcat,":")[[1]][1])
    res<-compareGroups(form,dd,max.xlev=Inf,max.ylev=Inf,method=method,compute.ratio=FALSE)
    refratiocat<-as.vector(refratiocat[attr(res,"varnames.orig")])
    factratio<-as.vector(factratio[attr(res,"varnames.orig")])
    method<-as.vector(method[attr(res,"varnames.orig")])
    hide<-as.vector(hide[attr(res,"varnames.orig")])
    descdigits<-as.vector(descdigits[attr(res,"varnames.orig")])
    ratiodigits<-as.vector(ratiodigits[attr(res,"varnames.orig")])
    alpha<-if (is.null(input$alpha)) 0.05 else input$alpha
    mindis<-if (is.null(input$mindis)) 0.05 else input$mindis
    pcorrected<-if (is.null(input$pcorrected)) 0.05 else input$pcorrected
    showpmul<-if (is.null(input$showpmul)) 0.05 else input$showpmul
    input$changeformat
    isolate({
      Q1<-if (is.null(input$Q1)) 25 else input$Q1   
      Q3<-if (is.null(input$Q3)) 75 else input$Q3
      qtype1<-if (is.null(input$qtype1)) 1 else input$qtype1
      qtype2<-if (is.null(input$qtype2)) 1 else input$qtype2
      type<-if (is.null(input$type)) NA else input$type
      sdtype<-if (is.null(input$sdtype)) 1 else input$sdtype
    })
    computeratio<-if (is.null(input$computeratio) || input$resptype=='Survival') TRUE else input$computeratio 
    includemiss<-if (is.null(input$includemiss)) FALSE else input$includemiss
    simplify<-if (is.null(input$simplify)) TRUE else input$simplify
    showpoverall<-if (is.null(input$showpoverall)) TRUE else input$showpoverall
    showptrend<-if (is.null(input$showptrend)) FALSE else input$showptrend
    showratio<-if (is.null(input$showratio)) FALSE else input$showratio
    showpratio<-if (is.null(input$showpratio)) showratio else input$showpratio
    showall<-if (is.null(input$showall)) TRUE else input$showall
    shown<-if (is.null(input$shown)) FALSE else input$shown
    showdesc<-if (is.null(input$showdesc)) TRUE else input$showdesc
    pvaldigits<-if (is.null(input$pvaldigits)) 3 else input$pvaldigits
    showpmul<-if (is.null(input$showpmul)) FALSE else input$showpmul
    # compareGroups
    res<-compareGroups(form,dd,max.xlev=Inf,max.ylev=Inf,method=method,include.miss=includemiss,ref.no="no",ref=refratiocat,Q1=Q1/100,Q3=Q3/100,simplify=simplify,compute.ratio=computeratio,fact.ratio=factratio,ref.y=refy,min.dis=mindis,alpha=alpha,p.corrected=pcorrected)    
    # createTable
    restab<-createTable(res,show.p.overall=showpoverall,show.p.trend=showptrend,show.ratio=showratio,show.p.ratio=showpratio,show.all=showall,show.n=shown,show.desc=showdesc,hide.no=hideno,hide=hide,type=type,sd.type=sdtype,q.type=c(qtype1,qtype2),digits=descdigits,digits.ratio=ratiodigits,digits.p=pvaldigits,show.p.mul=showpmul)
    # return
    return(restab)  
  })  
  
  #########################
  ### create compareSNPs ##
  #########################
  
  createSNPs<-reactive({
    dd<-dataset()
    if (is.null(dd)){
      cat("Data not loaded\n")
      return(invisible(NULL))
    }
    # global subset
    input$changeglobalsubset  
    isolate({
      dd2<-dd
      for (i in 1:ncol(dd2))
        if (is.factor(dd2[,i]))
          dd2[,i]<-as.integer(dd2[,i])
      if (!is.null(input$globalsubset))
        dd2<-try(eval(parse(text=paste("subset(dd2,",input$globalsubset,")",sep=""))),silent=TRUE)
      if (inherits(dd2,"try-error")){
        cat("Subset not correct\n")
        return(invisible(NULL))      
      }  
      if (nrow(dd2)==0){
        cat("No individuals selected\n")
        return(invisible(NULL))      
      }
      dd<-dd[rownames(dd2),]
    })    
    input$changeselevars
    if (!exists("selevars",envir=.cGroupsWUIEnv))
      return(NULL)    
    selevars<-get("selevars",envir=.cGroupsWUIEnv,inherits=FALSE) 
    if (is.null(selevars) || length(selevars)==0){
      cat("No variables selected\n")
      return(invisible(NULL)) 
    }   
    if (input$resptype=='None')
      form<-as.formula(paste("~",paste(selevars,collapse="+"),sep=""))
    else {
      if (input$resptype=='Survival'){
        return(invisible(NULL))
      } else
        form<-as.formula(paste(input$gvar,"~",paste(selevars,collapse="+"),sep=""))
    }
    restabSNPs<-compareSNPs(form, dd, sep = input$sepSNPs) 
    return(restabSNPs)  
  })   
  
  ####################
  ### values table ###
  ####################
  
  ## values summary
  output$valuestable <- renderText({
    dd<-dataset()
    if (is.null(dd)){
      cat("Data not loaded\n")
      return(invisible(NULL))
    }
    input$changemethod
    if (!exists("selevars",envir=.cGroupsWUIEnv))
      return(NULL)    
    selevars<-get("selevars",envir=.cGroupsWUIEnv,inherits=FALSE)
    if (is.null(selevars))
      return(NULL)
    if (length(selevars)==0){
      cat("No variables selected\n")
      return(invisible(NULL))
    }
    method<-get("method",envir=.cGroupsWUIEnv,inherits=FALSE)
    res<-compareGroups(~.,dd,max.xlev=Inf,max.ylev=Inf,method=method,min.dis=if (is.null(input$mindis)) 5 else input$mindis,alpha=if (is.null(input$alpha)) 0.05 else input$alpha)
    method.temp<-sapply(res,function(x) paste(attr(x,"method"),collapse=" "))
    varnames.orig<-attr(res,"varnames.orig")
    res<-compareGroups(~.,dd,max.xlev=Inf,max.ylev=Inf,method=NA,min.dis=if (is.null(input$mindis)) 5 else input$mindis,alpha=if (is.null(input$alpha)) 0.05 else input$alpha)
    values<-n<-NULL
    for (i in 1:length(res)){
      x.i<-attr(res[[i]],"x")
      n<-c(n,sum(!is.na(x.i)))
      if (is.factor(x.i)){
        if (nlevels(x.i)>input$maxvalues){
          vv<-paste("'",levels(x.i),"'",sep="")
          cc<-1:nlevels(x.i)
          vv<-c(paste("-",vv[1:(input$maxvalues-1)],sep=""),"...",paste("-",vv[length(vv)],sep=""))
          cc<-c(cc[1:(input$maxvalues-1)],"",cc[length(cc)])
          values<-c(values,paste(paste(cc,vv,sep=""),collapse="<br/> "))
        }else
          values<-c(values,paste(paste(1:nlevels(x.i),paste("'",levels(x.i),"'",sep=""),sep="-"),collapse="<br/> "))
      } else
        values<-c(values,paste(compareGroups:::format2(range(x.i)),collapse="; "))
    }
    ans<-data.frame("Name"=varnames.orig,"Label"=names(res),"Method"=sub("continuous ","",method.temp),"N"=n,"Values"=values)
    ans<-as.matrix(ans)
    ans<-print(xtable(ans),type="html",include.rownames=FALSE, sanitize.text.function=function(x) x)
    ans<-gsub("<TD align=\"center\">",paste("<TD align=\"center\" style=\"font-size:",input$htmlsizeinfotab,"em\">",sep=""),ans)
    ans<-gsub("<TD>",paste("<TD style=\"font-size:",input$htmlsizeinfotab,"em\">",sep=""),ans)
    ans<-gsub("<TH>",paste("<TH style=\"font-size:",input$htmlsizeinfotab,"em\">",sep=""),ans)
    ans
    
  })
  
  ## values extended
  output$valuesext <- renderDataTable(dataset(), options=list(iDisplayLength = 10))
  
  ############################
  ##### print createTable ####
  ############################
  
  output$restab <- renderPrint({
    restab<-create()
    if (is.null(restab))
      return(invisible(NULL))
    input$changeLabels
    isolate({header.labels<-c(input$alllabel,input$poveralllabel,input$ptrendlabel,input$pratiolabel,input$Nlabel)})
    isolate({print(restab,header.labels=header.labels)})
  })
  
  ############################
  ##### html createTable #####
  ############################
  
  output$htmltab <- renderText({
    restab<-create()
    if (is.null(restab))
      return(invisible(NULL))
    input$changeLabels
    isolate({header.labels<-c(input$alllabel,input$poveralllabel,input$ptrendlabel,input$pratiolabel,input$Nlabel)})
    export2html(restab,"tableHTML.html",header.labels=header.labels)      
    ans<-scan(file="tableHTML.html",what="character",sep="\n")
    file.remove("tableHTML.html")  
    ans<-gsub("<TD align=\"center\">",paste("<TD align=\"center\" style=\"font-size:",input$htmlsizerestab,"em\">",sep=""),ans)
    ans<-gsub("<TD>",paste("<TD style=\"font-size:",input$htmlsizerestab,"em\">",sep=""),ans)
    ans<-gsub("<TH>",paste("<TH style=\"font-size:",input$htmlsizerestab,"em\">",sep=""),ans)
    ans
  })
  
  #############################
  ##### PDF createTable #######
  #############################
  
  output$pdftab<-renderUI({
    restab<-create()
    if (is.null(restab))
      return(invisible(NULL))
    sizenum<-if (is.null(input$sizepdftab)) 5 else input$sizepdftab
    #filename<-paste(system.file("app", package = "compareGroups"),"/www/tablePDF",sample(1:10000,1),".pdf",sep="")
    filename<-paste("./www/tablePDF",sample(1:10000,1),".pdf",sep="")
    input$changeLabels
    isolate({
      header.labels<-c(input$alllabel,input$poveralllabel,input$ptrendlabel,input$pratiolabel,input$Nlabel)
      captionlabel<-input$captionlabel
      if (!is.null(captionlabel) && captionlabel=='NULL')
        captionlabel<-NULL 
    })
    export2pdf(restab,filename,size=c("tiny","scriptsize","footnotesize","small","normalsize","large","Large","LARGE","huge","Huge")[sizenum],open=FALSE, margin=c(0.5,0,0,0),header.labels=header.labels,caption=captionlabel)
    ff<-list.files(dirname(filename),full.names=TRUE) 
    sapply(ff[ff!=filename],file.remove)
    print(getwd())
    tags$iframe(src=basename(filename), width="800", height="700")
  })
  
  ############################
  ##### print compareSNPs ####
  ############################
  
  output$restabSNPs <- renderPrint({
    restabSNPs<-createSNPs()
    if (is.null(restabSNPs))
      return(invisible(NULL))
    return(restabSNPs)
  })  
  
  ##############################
  ##### summary createTable ####
  ##############################
  
  output$sumtab <- renderText({
    restab<-create()
    if (is.null(restab))
      return(invisible(NULL))
    cg<-attr(restab,"x")[[1]]
    varsubset<-get("varsubset",envir=.cGroupsWUIEnv,inherits=FALSE)
    for (i in 1:length(cg)){
      nn<-which(names(varsubset)==attr(cg,"varnames.orig")[i])
      if ((!is.null(input$globalsubset) && input$globalsubset!='') && (!is.na(varsubset[nn]) && varsubset[nn]!=''))
        selec<-paste(input$globalsubset," & (",varsubset[nn],")",sep="")
      if ((!is.null(input$globalsubset) && input$globalsubset!='') && (is.na(varsubset[nn]) || varsubset[nn]==''))
        selec<-input$globalsubset
      if ((is.null(input$globalsubset) || input$globalsubset=='') && (!is.na(varsubset[nn]) && varsubset[nn]!=''))
        selec<-varsubset[nn]      
      if ((is.null(input$globalsubset) || input$globalsubset=='') && (is.na(varsubset[nn]) || varsubset[nn]==''))
        selec<-"ALL"         
      attr(cg[[i]],"selec")<-selec
    }
    export2html(createTable(cg),file="tablesummaryHTML.html",which.table="avail")
    ans<-scan(file="tablesummaryHTML_appendix.html",what="character",sep="\n")
    file.remove("tablesummaryHTML_appendix.html")  
    ans<-gsub("<TD align=\"center\">",paste("<TD align=\"center\" style=\"font-size:",input$htmlsizesumtab,"em\">",sep=""),ans)
    ans<-gsub("<TD>",paste("<TD style=\"font-size:",input$htmlsizesumtab,"em\">",sep=""),ans)
    ans<-gsub("<TH>",paste("<TH style=\"font-size:",input$htmlsizesumtab,"em\">",sep=""),ans)   
    ans    
  })
  
  ##########################################
  ##### select variables to be analyzed ####
  ##########################################
  
  output$selevarslist<-renderUI({
    if (is.null(input$initial) || !input$initial)
      return(invisible(NULL))
    dd<-dataset()
    if (is.null(dd)){
      cat("Data not loaded\n")
      return(invisible(NULL))
    }
    input$changeselevars
    if (!exists("discvars",envir=.cGroupsWUIEnv))
      return(NULL)    
    discvars<-get("discvars",envir=.cGroupsWUIEnv,inherits=FALSE)
    if (!exists("selevars",envir=.cGroupsWUIEnv))
      return(NULL)
    selevars<-get("selevars",envir=.cGroupsWUIEnv,inherits=FALSE)
    isolate({
      discvars<-c(discvars,input$selevars)
      if (length(input$selevars)>0)
        selevars<-selevars[-which(selevars%in%input$selevars)]
      selevars<-c(selevars,input$discvars)
      if (length(input$discvars)>0)
        discvars<-discvars[-which(discvars%in%input$discvars)]        
      assign("selevars",selevars,envir=.cGroupsWUIEnv)
      assign("discvars",discvars,envir=.cGroupsWUIEnv)
    })
    nn<-names(dd)
    div(
      div(selectInput("selevars","I want these variables",selevars,multiple=TRUE,selectize=FALSE),tags$style(type='text/css', paste("#selevars { height: ",ifelse(length(selevars)==0,20,ifelse(length(selevars)>20,300,18*length(selevars)+5)),"px;}",sep=""))),
      actionButton("changeselevars","",icon = icon("fa fa-sort")),
      div(selectInput("discvars","I don't want these variables",if (length(discvars)==0) discvars else nn[which(nn%in%discvars)], multiple=TRUE,selectize=FALSE),tags$style(type='text/css', paste("#discvars { height: ",ifelse(length(discvars)==0,20,ifelse(length(discvars)>20,300,18*length(discvars)+5)),"px;}",sep="")))
    )
  })
  
  
  ################################
  ##### select group variable ####
  ################################
  
  # select variable
  output$vargroup <- renderUI({
    dd<-dataset()
    if (is.null(dd)){
      cat("Data not loaded\n")
      return(invisible(NULL))
    }  
    input$changemethod
    method<-get("method",envir=.cGroupsWUIEnv,inherits=FALSE)
    res<-compareGroups(~.,max.xlev=Inf,max.ylev=Inf,dd,method=method,min.dis=if (is.null(input$mindis)) 5 else input$mindis,alpha=if (is.null(input$alpha)) 0.05 else input$alpha)
    method.temp<-sapply(res,function(x) paste(attr(x,"method"),collapse=" "))
    method.temp<-ifelse(method.temp=="continuous normal",1,
                        ifelse(method.temp=="continuous non-normal",2,3))
    names(method.temp)<-attr(res,"varnames.orig")
    vlist<-names(method.temp)
    vlist<-vlist[method.temp==3]
    vlist<-vlist[sapply(dd[vlist],function(x) nlevels(as.factor(x))<=input$maxgroups)]
    vlist<-vlist
    if (length(vlist)==0){
      return(invisible(NULL))
    }
    names(vlist)<-vlist
    selectInput("gvar", "Choose the grouping variable:", choices = vlist, selectize=FALSE)    
  })
  
  # select category for OR reference (only when two categories).
  output$vargroupcat <- renderUI({
    dd<-dataset()
    if (is.null(dd)){
      cat("Data not loaded\n")
      return(invisible(NULL))
    }
    if (is.null(input$gvar))
      return(invisible(NULL))
    vv<-dd[,input$gvar]
    if (nlevels(vv)!=2)
      return(NULL)
    vlist<-paste(1:nlevels(vv),levels(vv),sep=":")
    names(vlist)<-vlist
    conditionalPanel(
      condition = "input.computeratio == true",
      selectInput("gvarcat", "OR ref. cat:", choices = vlist, selectize=FALSE)    
    )
  })  
  
  ########################
  ##### select method ####
  ########################
  
  output$selemethod <- renderUI({
    if (is.null(input$initial) || !input$initial)
      return("Data not loaded")
    input$changeselevars
    if (!exists("selevars",envir=.cGroupsWUIEnv))
      return(NULL)
    selevars<-get("selevars",envir=.cGroupsWUIEnv,inherits=FALSE)
    if (is.null(selevars) || length(selevars)==0)
      return(NULL)
    div(
      div(class="row-fluid",                    
          div(class="span4",selectInput("varselemethod", "variable", choices = selevars, multiple = TRUE, selected = isolate({ input$varselemethod}),selectize=FALSE),
              tags$style(type='text/css', paste("#varselemethod { height: ",ifelse(length(selevars)==0,20,ifelse(length(selevars)>20,300,18*length(selevars)+5)),"px;}",sep=""))),
          div(class="span1 offset0",checkboxInput('varselemethodALL', 'ALL', isolate({input$varselemethodALL})))
      ),
      div(class="row-fluid",
        div(class="span3",selectInput("method", "type", c("Normal","Non-normal","Categorical","NA"),isolate({input$method}),selectize=FALSE)),   
        div(HTML("<br>"),class="span1 offset0",actionButton("changemethod","Update"))
      )
    )
  })
  
  output$selemethodNA <- renderUI({
    if (is.null(input$initial) || !input$initial)
      return(invisible(NULL))
    if (is.null(input$method) || input$method!='NA')    
      return(NULL)
    div(
      numericInput("alpha","alpha",value=0.05,min=0,max=1,step=0.005),
      numericInput("mindis","min categories",value=5,min=1,max=10)
    )
  })  
  
  ###################################
  ##### select response #############
  ###################################
  
  output$response <- renderUI({
    if (is.null(input$initial) || !input$initial)
      return("Data not loaded")
    if (input$resptype == 'Group'){
      div(
        numericInput('maxgroups',"Maximum number of groups:",value=5,min=2,max=10),
        uiOutput("vargroup"),
        checkboxInput('computeratio', 'Compute OR:', FALSE)
      )    
    } else {
      if (input$resptype=='Survival'){
        div(
          uiOutput("timevar"),
          uiOutput("censvar"),
          uiOutput("censcat")
        )
      } else {
        return(invisible(NULL))
      } 
    }
  })
  
  ####################################
  ##### select descriptive digits ####
  ####################################
  
  output$seledescdigits <- renderUI({
    if (!exists("selevars",envir=.cGroupsWUIEnv))
      return(NULL)  
    selevars<-get("selevars",envir=.cGroupsWUIEnv,inherits=FALSE)
    if (is.null(selevars))
      return(NULL)
    div(
      div(class="row-fluid",
          div(class="span4",selectInput("varseledescdigits", "variable", choices = selevars, multiple = TRUE, selected = isolate({input$varseledescdigits}),selectize=FALSE)),
          div(class="span2 offset5",checkboxInput('varseledescdigitsALL', 'ALL', isolate({input$varseledescdigitsALL})))
      ),
      numericInput("descdigits", label="Number of decimals (-1: default)", value = -1, min=-1, max=10),
      actionButton("changedescdigits","Update")
    )
  })
  
  ##############################
  ##### select ratio digits ####
  ##############################
  
  output$seleratiodigits <- renderUI({
    if (!exists("selevars",envir=.cGroupsWUIEnv))
      return(NULL)  
    selevars<-get("selevars",envir=.cGroupsWUIEnv,inherits=FALSE)
    if (is.null(selevars))
      return(NULL)
    div(
      div(class="row-fluid",
          div(class="span4",selectInput("varseleratiodigits", "variable", choices = selevars, multiple = TRUE, selected = isolate({input$varseleratiodigits}),selectize=FALSE)),
          div(class="span2 offset5",checkboxInput('varseleratiodigitsALL', 'ALL', isolate({input$varseleratiodigitsALL})))
      ),
      numericInput("ratiodigits", label="Number of decimals (-1: default)", value = -1, min=-1, max=10),
      actionButton("changeratiodigits","Update")
    )
  })      
  
  ##########################
  ##### variable subset ####
  ##########################
  
  output$selevarsubset <- renderUI({
    if (is.null(input$initial) || !input$initial)
      return("Data not loaded")
    if (!exists("selevars",envir=.cGroupsWUIEnv))
      return(NULL)  
    selevars<-get("selevars",envir=.cGroupsWUIEnv,inherits=FALSE)
    if (is.null(selevars))
      return(NULL)
    div(
      h5("Global subset"),
      textInput('globalsubset', 'Write subset expression', ''),
      actionButton("changeglobalsubset","Apply"),
      h5("Variable subset"),
      div(class="row-fluid",
          div(class="span4",selectInput("varselevarsubset", "variable", choices = selevars, multiple = TRUE, selected = isolate({input$varselevarsubset}),selectize=FALSE)),
          div(class="span2 offset5",checkboxInput('varselevarsubsetALL', 'ALL', isolate({input$varselevarsubsetALL})))
      ),
      textInput("varsubset", label="Write subset expression", value = ""),
      actionButton("changevarsubset","Apply")
    )
  })     
  
  ###############################################################
  ##### select reference category in OR/HR for row-variables ####
  ###############################################################
  
  ## ratio 
  output$ratio <- renderUI({
    if (is.null(input$initial) || !input$initial) 
      return("Data not loaded")
    if (input$resptype!='None'){
      div(
        # reference category
        div(
          uiOutput("selerefvar"),
          uiOutput("selerefcat")
        ),
        # factor 
        uiOutput("selefactratio")
      )
    } else {
      return("No response variable selected")
    }
  })
  
  ## select variable
  output$selerefvar <- renderUI({
    dd<-dataset()
    if (is.null(dd)){
      cat("Data not loaded\n")
      return(invisible(NULL))
    }  
    if (!exists("selevars",envir=.cGroupsWUIEnv))
      return(NULL)    
    selevars<-get("selevars",envir=.cGroupsWUIEnv,inherits=FALSE)
    input$changemethod
    method<-get("method",envir=.cGroupsWUIEnv,inherits=FALSE)
    res<-compareGroups(~.,max.xlev=Inf,max.ylev=Inf,dd,method=method,min.dis=if (is.null(input$mindis)) 5 else input$mindis,alpha=if (is.null(input$alpha)) 0.05 else input$alpha)
    method.temp<-sapply(res,function(x) paste(attr(x,"method"),collapse=" "))
    method.temp<-ifelse(method.temp=="continuous normal",1,
                        ifelse(method.temp=="continuous non-normal",2,3))
    names(method.temp)<-attr(res,"varnames.orig")
    vlist<-names(method.temp)
    vlist<-vlist[method.temp==3]  
    names(vlist)<-vlist
    vlist<-intersect(vlist,selevars)
    if (length(vlist)==0){
      return(invisible(NULL))
    }
    div(
      h5("Reference category:"), 
      selectInput("varselerefratio", "variable", choices = vlist, multiple = FALSE, selectize=FALSE)
    )
  })
  
  ## select category
  output$selerefcat <- renderUI({
    dd<-dataset()
    if (is.null(dd)){
      cat("Data not loaded\n")
      return(invisible(NULL))
    }
    if (!exists("selevars",envir=.cGroupsWUIEnv))
      return(NULL)    
    selevars<-get("selevars",envir=.cGroupsWUIEnv,inherits=FALSE)
    if (is.null(selevars))
      return(invisible(NULL))  
    if (is.null(input$varselerefratio) || input$varselerefratio=="No categorical variables")
      return(invisible(NULL))
    vv<-as.factor(dd[,input$varselerefratio])
    vlist<-1:nlevels(vv)
    names(vlist)<-paste(vlist,levels(vv),sep=":")  
    div(
      selectInput("refratiocat", "category", vlist, vlist[1],selectize=FALSE),
      actionButton("changeratiocat","Update")
    )
  }) 
  
  #########################################
  ##### select factor to compute OR/HR ####
  #########################################
  
  output$selefactratio <- renderUI({
    dd<-dataset()
    if (is.null(dd)){
      cat("Data not loaded\n")
      return(invisible(NULL))
    }  
    input$changemethod
    if (!exists("selevars",envir=.cGroupsWUIEnv))
      return(NULL)    
    selevars<-get("selevars",envir=.cGroupsWUIEnv,inherits=FALSE)
    method<-get("method",envir=.cGroupsWUIEnv,inherits=FALSE)
    res<-compareGroups(~.,max.xlev=Inf,max.ylev=Inf,dd,method=method,min.dis=if (is.null(input$mindis)) 5 else input$mindis,alpha=if (is.null(input$alpha)) 0.05 else input$alpha)
    method.temp<-sapply(res,function(x) paste(attr(x,"method"),collapse=" "))
    method.temp<-ifelse(method.temp=="continuous normal",1,
                        ifelse(method.temp=="continuous non-normal",2,3))
    names(method.temp)<-attr(res,"varnames.orig")
    vlist<-names(method.temp)
    vlist<-vlist[method.temp!=3] 
    names(vlist)<-vlist
    vlist<-intersect(vlist,selevars) 
    if (length(vlist)==0){
      return(invisible(NULL))
    }    
    div(
      h5("Multiplying factor:"),
      div(class="row-fluid",
          div(class="span5",selectInput("varselefactratio", "variable", choices = vlist, multiple = TRUE, selected = isolate({input$varselefactratio}),selectize=FALSE)),
          div(class="span2 offset5",div(class="span2",checkboxInput('varselefactratioALL', 'ALL', isolate({input$varselefactratioALL}))))
      ),
      numericInput("factratio", label="factor", value = 1, min=1, max=100),
      actionButton("changefactratio","Update")
    )
  })    
  
  #################################
  ##### select hide category ######
  #################################
  
  ## select variable
  output$selehidevar <- renderUI({
    if (is.null(input$initial) || !input$initial)
      return("Data not loaded")
    dd<-dataset()
    if (is.null(dd)){
      cat("Data not loaded\n")
      return(invisible(NULL))
    }  
    input$changemethod
    if (!exists("selevars",envir=.cGroupsWUIEnv))
      return(NULL)    
    selevars<-get("selevars",envir=.cGroupsWUIEnv,inherits=FALSE)
    method<-get("method",envir=.cGroupsWUIEnv,inherits=FALSE)
    res<-compareGroups(~.,max.xlev=Inf,max.ylev=Inf,dd,method=method,min.dis=if (is.null(input$mindis)) 5 else input$mindis,alpha=if (is.null(input$alpha)) 0.05 else input$alpha)
    method.temp<-sapply(res,function(x) paste(attr(x,"method"),collapse=" "))
    method.temp<-ifelse(method.temp=="continuous normal",1,
                        ifelse(method.temp=="continuous non-normal",2,3))
    names(method.temp)<-attr(res,"varnames.orig")
    vlist<-names(method.temp)
    vlist<-vlist[method.temp==3]  
    names(vlist)<-vlist 
    vlist<-intersect(vlist,selevars) 
    if (length(vlist)==0){
      return(invisible(NULL))
    }
    selectInput("varselehide", "variable", choices = vlist, multiple = FALSE, selectize=FALSE)
  })
  
  ## select category
  output$selehidecat <- renderUI({
    dd<-dataset()
    if (is.null(dd)){
      cat("Data not loaded\n")
      return(invisible(NULL))
    }
    if (!exists("selevars",envir=.cGroupsWUIEnv))
      return(NULL)    
    selevars<-get("selevars",envir=.cGroupsWUIEnv,inherits=FALSE)
    if (is.null(selevars) || length(selevars)==0)
      return(invisible(NULL))  
    if (is.null(input$varselehide))
      return(invisible(NULL))             
    vv<-as.factor(dd[,input$varselehide])
    vlist<-c(NA,1:nlevels(vv))
    names(vlist)<-paste(vlist,c("<<None>>",levels(vv)),sep=":")
    div(
      selectInput("hidecat", "category", vlist, "<<None>>", selectize=FALSE),
      actionButton("changehide","Update")    
    )
  }) 
  
  #################################
  ##### select time variable ######
  #################################
  
  output$timevar <- renderUI({
    dd<-dataset()
    if (is.null(dd)){
      cat("Data not loaded\n")
      return(invisible(NULL))
    }  
    input$changemethod
    method<-get("method",envir=.cGroupsWUIEnv,inherits=FALSE)
    res<-compareGroups(~.,max.xlev=Inf,max.ylev=Inf,dd,method=method,min.dis=if (is.null(input$mindis)) 5 else input$mindis,alpha=if (is.null(input$alpha)) 0.05 else input$alpha)
    method.temp<-sapply(res,function(x) paste(attr(x,"method"),collapse=" "))
    method.temp<-ifelse(method.temp=="continuous normal",1,
                        ifelse(method.temp=="continuous non-normal",2,3))
    names(method.temp)<-attr(res,"varnames.orig")
    vlist<-names(method.temp)
    vlist<-vlist[method.temp!=3] 
    if (length(vlist)==0){
      return(invisible(NULL))
    }
    names(vlist)<-vlist  
    selectInput("varseletime", "Select time-to-event variable", choices = vlist, multiple = FALSE, selectize=FALSE)
  })   
  
  #################################
  ##### select status variable ####
  #################################
  
  output$censvar <- renderUI({
    dd<-dataset()
    if (is.null(dd)){
      cat("Data not loaded\n")
      return(invisible(NULL))
    }  
    input$changemethod
    method<-get("method",envir=.cGroupsWUIEnv,inherits=FALSE)
    res<-compareGroups(~.,max.xlev=Inf,max.ylev=Inf,dd,method=method,min.dis=if (is.null(input$mindis)) 5 else input$mindis,alpha=if (is.null(input$alpha)) 0.05 else input$alpha)
    method.temp<-sapply(res,function(x) paste(attr(x,"method"),collapse=" "))
    method.temp<-ifelse(method.temp=="continuous normal",1,
                        ifelse(method.temp=="continuous non-normal",2,3))
    names(method.temp)<-attr(res,"varnames.orig")
    vlist<-names(method.temp)
    vlist<-vlist[method.temp==3]  
    if (length(vlist)==0){
      return(invisible(NULL))
    }
    names(vlist)<-vlist  
    selectInput("varselestatus", "Select status variable", choices = vlist, multiple = FALSE, selectize=FALSE)
  })
  
  ######################################
  ##### select death category/ies ######
  ######################################
  
  output$censcat <- renderUI({
    dd<-dataset()
    if (is.null(dd)){
      cat("Data not loaded\n")
      return(invisible(NULL))
    }  
    input$changemethod
    method<-get("method",envir=.cGroupsWUIEnv,inherits=FALSE)
    res<-compareGroups(~.,max.xlev=Inf,max.ylev=Inf,dd,method=method,min.dis=if (is.null(input$mindis)) 5 else input$mindis,alpha=if (is.null(input$alpha)) 0.05 else input$alpha)
    method.temp<-sapply(res,function(x) paste(attr(x,"method"),collapse=" "))
    method.temp<-ifelse(method.temp=="continuous normal",1,
                        ifelse(method.temp=="continuous non-normal",2,3))
    names(method.temp)<-attr(res,"varnames.orig")
    vlist<-names(method.temp)
    vlist<-vlist[method.temp==3]  
    if (length(vlist)==0){
      return(invisible(NULL))
    }
    vv<-as.factor(dd[,input$varselestatus])
    vlist<-1:nlevels(vv)
    names(vlist)<-paste(vlist,levels(vv),sep=":")
    selectInput("statuscat", "Select event category", vlist, multiple = FALSE, selectize=FALSE)
  })   
  
  ######################################
  ####### show #########################
  ######################################
  
  output$show <- renderUI({
    if (is.null(input$initial) || !input$initial)
      return("Data not loaded")
    div(
      div(class="row-fluid",
          div(class="span6",checkboxInput('showall', 'ALL', TRUE)),
          div(class="span6",checkboxInput('showpoverall', 'p-overall', TRUE))
      ),
      div(class="row-fluid",
          div(class="span6",checkboxInput('showdesc', 'Descriptives', TRUE)),
          div(class="span6",checkboxInput('showptrend', 'p-trend', FALSE))
      ),
      div(class="row-fluid",
          div(class="span6",checkboxInput('showratio', 'OR/HR', FALSE)),
          div(class="span6",                    
              conditionalPanel(
                condition = "input.showratio == true",
                checkboxInput('showpratio', 'OR/HR p-value', FALSE)
              )                    
          )                    
      ),
      div(class="row-fluid",
          div(class="span6",checkboxInput('shown', 'Available', FALSE)),
          div(class="span6",checkboxInput('includemiss', "NA category", FALSE))
      ),
      div(class="row-fluid",
          div(class="span6",checkboxInput('showpmul', 'Pairwise p-value', FALSE)),
          div(class="span6",
              conditionalPanel(
                condition = "input.showpmul == true",
                checkboxInput('pcorrected', 'Correct pairwise p-values', FALSE)
              )
          )
      ),
      div(class="row-fluid",
          div(class="span6",checkboxInput('simplify', 'Simplify', FALSE)),
          div(class="span6","")
      )
    )                         
  })
  
  ##################################
  ######### format #################
  ##################################
  
  output$format <- renderUI({
    if (is.null(input$initial) || !input$initial)
      return("Data not loaded")
    div(
      div(class="row-fluid",
          div(class="span6",h5("Frequencies:")),                                                         
          div(class="span6",radioButtons("type", "", c("%" = 1, "n (%)" = 2, "n"=3), selected="n (%)",inline = TRUE))
      ),
      div(class="row-fluid",
          div(class="span4",h5("Means:")),
          div(class="span8",radioButtons("sdtype", "", c("Mean (SD)"=1,"Mean+-SD"=2), selected="Mean (SD)",inline = TRUE))
      ),
      h5("Quantiles: Median [a, b]:"),
      div(class="row-fluid",
          div(class="span2",h6("[a, ]:")),
          div(class="span2",numericInput("Q1", label="", value = 25, min=0, max=49))
      ),                                          
      div(class="row-fluid",
          div(class="span2",h6("[ , b]:")),
          div(class="span2",numericInput("Q3", label="", value = 75, min=51, max=100))
      ),
      div(class="row-fluid",
          div(class="span5",radioButtons("qtype1", "brackets", c("Squared"=1,"Rounded"=2), selected="Squared")),
          div(class="span5",radioButtons("qtype2", "separator", c("Semicolon"=1,"Comma"=2,"Slash"=3), selected="Semicolon"))
      ),
      actionButton("changeformat","","Update")
    )
  })
  
  ########################
  ##### decimals #########
  ########################  
  
  output$decimals <- renderUI({  
    if (is.null(input$initial) || !input$initial)
      return("Data not loaded")
    div(
      div(
        h5("p-values:"),
        numericInput("pvaldigits", label="Number of decimals", value = 3, min=1, max=20)
      ),
      div(
        h5("Descriptives:"),
        uiOutput("seledescdigits")
      ),
      div(
        h5("OR/HR:"),
        uiOutput("seleratiodigits")
      )                  
    )
  })
  
  ########################
  ##### labels ###########
  ########################  
  
  output$labels <- renderUI({  
    if (is.null(input$initial) || !input$initial) 
      return("Data not loaded") 
    div(
      div(class="row-fluid",
          div(class="span2",textInput("alllabel", label="All:", value="[ALL]")),
          div(class="span2 offset7",actionButton("changeLabels","Apply")) 
      ),
      textInput("poveralllabel", label="overall p-value:", value="p.overall"),
      textInput("ptrendlabel", label="p-value for trend:", value="p.trend"),
      textInput("pratiolabel", label="OR/HR p-value:", value="p.ratio"),
      textInput("Nlabel", label="Available data:", value="N"), 
      textInput("captionlabel", label="Caption (only for PDF):", value="NULL")
    )
  })
  
  ########################
  ##### save #############
  ########################  
  
  output$save <- renderUI({  
    if (is.null(input$initial) || !input$initial) 
      return("Data not loaded")
    if (input$results=='TABLE'){  # save table
      div(
        selectInput("downloadtabletype", "Select format", choices = c("PDF","CSV","HTML","TXT"),selectize=FALSE),
        downloadButton('actiondownloadtable', 'Download'),
        conditionalPanel(
          condition="input.downloadtabletype == 'PDF'",
          wellPanel(
            selectInput('sizepdf', 'Resize', c("tiny","scriptsize","footnotesize","small","normalsize","large","Large","LARGE","huge","Huge"),"normalsize", selectize=FALSE),
            h4(""),
            checkboxInput('landscape', 'Landscape', FALSE)
          )
        ),
        conditionalPanel(        
          condition="input.downloadtabletype == 'CSV'",
          wellPanel(
            radioButtons('sepcsv', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), 'Comma')
          )
        )
      )    
    } else {
      if (input$results=='PLOT'){  # save plot
        div(
          selectInput("downloadplottype", "Select format", choices = c('pdf','bmp','jpg','png','tif'),selectize=FALSE),
          downloadButton('actiondownloadplot', 'Download')      
        )      
      } else {
        if (input$results=='SNPs'){  # save SNPs table
          div(
            condition = "input.initial == true && input.results == 'SNPs'",          
            downloadButton('actiondownloadSNPtable', 'Download')          
          )
        }
      }
    }
  })
  
  ########################
  ####### values #########
  ########################
  
  output$values <- renderUI({  
    if (is.null(input$initial) || !input$initial) 
      return("Data not loaded")  
    div(
      div(class="row-fluid",
          div(class="span1",h6("Resize:")),
          div(class="span10",sliderInput("htmlsizeinfotab", "", min=0.5, max=2, value=1, step=0.1))
      ),
      div(class="row-fluid",
          div(class="span4",h6("Maximum number of categories to display:")),
          div(class="span1",numericInput("maxvalues", "", min=3, max=100, value=10, step=1))
      ),
      htmlOutput('valuestable')
    )
  })
  
  ########################
  ####### info ###########
  ########################
  
  output$info <- renderUI({  
    if (is.null(input$initial) || !input$initial) 
      return("Data not loaded")  
    div(
      div(class="row-fluid",
          div(class="span1",h6("Resize:")),
          div(class="span10",sliderInput("htmlsizesumtab", "", min=0.5, max=2, value=1, step=0.1))
      ),
      htmlOutput('sumtab')
    )
  })
  
  ########################
  ####### table ##########
  ########################
  
  output$table <- renderUI({  
    if (is.null(input$initial) || !input$initial) 
      return("Data not loaded")  
    if (input$tabletype=='R console'){
      div(
        condition = "input.tabletype == 'R console'",
        verbatimTextOutput('restab')
      )
    } else {
      if (input$tabletype == 'PDF'){
        div(
          div(class="row-fluid",
              div(class="span1",h6("Resize:")),
              div(class="span10",sliderInput("sizepdftab", "", min=1, max=10, value=5, step=1))
          ),           
          uiOutput('pdftab')
        )              
      } else {
        if (input$tabletype == 'HTML'){
          div(
            div(class="row-fluid",
                div(class="span1",h6("Resize:")),
                div(class="span10",sliderInput("htmlsizerestab", "", min=0.5, max=2, value=1, step=0.1))
            ),           
            htmlOutput('htmltab')
          )
        }
      }
    }
  })
  
  ########################
  ###### ui plot #########
  ########################
  
  output$uiplot <- renderUI({  
    if (is.null(input$initial) || !input$initial) 
      return("Data not loaded")    
    div(  
      uiOutput("varPlot"), 
      imageOutput('plot',width = "100%", height = "500px")
    )
  })
  
  ########################
  ######## snps ##########
  ########################
  
  output$snps <- renderUI({  
    if (is.null(input$initial) || !input$initial) 
      return("Data not loaded") 
    div(
      div(class="row-fluid",
          div(class="span3","Allele separator character"),
          HTML('<input class="span1" type="text" id="sepSNPs" value="" />')
      ),
      verbatimTextOutput('restabSNPs')
    )
  })
  
  ########################
  ##### plot #############
  ########################
  
  output$varPlot <- renderUI({
    if (input$exampledata=='Own data'){
      inFile<-input$files
      if (is.null(inFile))
        return(invisible(NULL))  
    }
    if (!exists("selevars",envir=.cGroupsWUIEnv))
      return(NULL)      
    selevars<-get("selevars",envir=.cGroupsWUIEnv,inherits=FALSE)
    if (is.null(selevars) || length(selevars)==0)
      return(invisible(NULL))
    input$changeselevars
    div(class="row-fluid",  
        div(class="span4",selectInput("varPlot", "", choices = selevars, selectize=FALSE)),
        conditionalPanel(
          condition = "input.resptype != null && input.resptype != 'None'",
          div(class="span2",checkboxInput('bivar', 'Bivariate', FALSE))
        )
    )  
  })
  
  output$plot <- renderImage({
    if (is.null(create()))
      return(list(src = "./figure.png", alt = "Error in performing the table"))
    bivar<-if (is.null(input$bivar)) FALSE else input$bivar
    plot(create(),type="png",file="./fig",bivar=bivar)
    file.rename(paste("./fig",input$varPlot,".png",sep=""),"./figure.png")
    ff<-list.files(pattern="\\.png$")
    ff<-ff[-which(ff=="figure.png")]
    sapply(ff,file.remove)
    list(src = "./figure.png", alt = "No figure found")
  }, deleteFile = TRUE)
  
  
  ####################################
  ############  HELP  ################
  ####################################
  
  
  ## about
  output$helpabout <- renderUI({
    hlp <- loadhelp()
    if (input$about=='compareGroups'){
      div(HTML(hlp["HELPCG"]))
    } else {
      if (input$about=='WUI'){
        div(HTML(hlp["HELPWUI"]))
      } else {
          div(HTML(hlp["DATASECURITY"]))
      }
    }
  })
  
  ## help screens
  output$helpscreens <- renderUI({
    hlp <- loadhelp()
    if (input$screens=='Results'){
      div(
        tabsetPanel(id="respanelhelp",
                      tabPanel(HTML("<font size=2px color='blue'> INFO	</font>"), HTML(hlp["INFO"])),
                      navbarMenu(HTML("<font size=2px color='blue'> VALUES </font>"),
                                 tabPanel(HTML("<font size=2px color='blue'> Summary </font>"), HTML(hlp["Summary"])),
                                 tabPanel(HTML("<font size=2px color='blue'> Extended </font>"), HTML(hlp["Extended"]))
                      ),                                     
                      tabPanel(HTML("<font size=2px color='blue'> TABLE </font>"), HTML(hlp["TABLE"])),                       
                      tabPanel(HTML("<font size=2px color='blue'> PLOT	</font>"), HTML(hlp["PLOT"])),                       
                      tabPanel(HTML("<font size=2px color='blue'> SNPs	</font>"), HTML(hlp["SNPs"])),
                      tabPanel(HTML("<font size=2px color='blue'> HELP	</font>"), HTML(hlp["Help"]))
        ),
        br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()        
      )       
    } else {
      if (input$screens=='Control'){
        div(
          tabsetPanel(id="contpanelhelp",
                      tabPanel(HTML("<font size=2px color='blue'> START	</font>"), HTML(hlp["LOAD"])),
                      navbarMenu(HTML("<font size=2px color='blue'> SETTINGS </font>"),
                                 tabPanel(HTML("<font size=2px color='blue'> Type </font>"), HTML(hlp["Type"])),
                                 tabPanel(HTML("<font size=2px color='blue'> Response </font>"), HTML(hlp["Response"])),
                                 tabPanel(HTML("<font size=2px color='blue'> Hide </font>"), HTML(hlp["Hide"])),
                                 tabPanel(HTML("<font size=2px color='blue'> Subset </font>"), HTML(hlp["Subset"])),                                                                               
                                 tabPanel(HTML("<font size=2px color='blue'> OR/HR </font>"), HTML(hlp["OR/HR"]))
                      ),         
                      navbarMenu(HTML("<font size=2px color='blue'> DISPLAY </font>"),
                                 tabPanel(HTML("<font size=2px color='blue'> Show </font>"), HTML(hlp["Show"])),
                                 tabPanel(HTML("<font size=2px color='blue'> Format </font>"), HTML(hlp["Format"])),
                                 tabPanel(HTML("<font size=2px color='blue'> Decimals </font>"), HTML(hlp["Decimals"])),
                                 tabPanel(HTML("<font size=2px color='blue'> Label </font>"), HTML(hlp["Label"]))                                                                              
                      ), 
                      tabPanel(HTML("<font size=2px color='blue'> SAVE	</font>"), HTML(hlp["SAVE"]))
          ),
          br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br() 
        )    
      }
    }
  })  
  
  ####################################
  ##### DOWNLOAD RESULTS #############
  ####################################
  
  ####### table #########
  output$actiondownloadtable <- downloadHandler(
    filename = function() paste("tableOuput",tolower(input$downloadtabletype),sep="."),
    content = function(ff) {
      input$changeLabels
      isolate({
        header.labels<-c(input$alllabel,input$poveralllabel,input$ptrendlabel,input$pratiolabel,input$Nlabel)
        captionlabel<-input$captionlabel
        if (!is.null(captionlabel) && captionlabel=='NULL')
          captionlabel<-NULL 
      })    
      restab<-create()
      if (is.null(restab))
        return(invisible(NULL))
      if (input$downloadtabletype=='CSV'){
        export2csv(restab,file=ff,sep=input$sepcsv,header.labels=header.labels)
      }
      if (input$downloadtabletype=='PDF'){
        export2pdf(restab,file="tableTemp.pdf",openfile=FALSE,size=input$sizepdf,landscape=input$landscape,header.labels=header.labels,caption=captionlabel)
        file.rename("tableTemp.pdf",ff)
        file.remove("tableTemp.aux")
        file.remove("tableTemp.log")
        file.remove("tableTemp.tex")
      }
      if (input$downloadtabletype=='HTML')
        export2html(restab,file=ff,header.labels=header.labels)
      if (input$downloadtabletype=='TXT'){
        sink(ff)
        print(restab,header.labels=header.labels)
        sink()
      }              
    }
  )
  
  ####### SNPs table #########
  output$actiondownloadSNPtable <- downloadHandler(
    filename = function() "tableSNPOuput.txt",
    content = function(ff) {
      restabSNPs<-createSNPs()
      if (is.null(restabSNPs))
        return(invisible(NULL))
      sink(ff)
      print(restabSNPs)
      sink()
    }
  )  
  
  ####### plot #########
  output$actiondownloadplot <- downloadHandler(
    filename = function() paste("figure",tolower(input$downloadplottype),sep="."),
    content = function(ff) {
      if (is.null(create()))
        return(NULL)
      ext<-input$downloadplottype
      bivar<-if (is.null(input$bivar)) FALSE else input$bivar
      plot(create(),type=ext,file="fig",bivar=bivar)
      file.rename(paste("fig",input$varPlot,".",ext,sep=""),ff)
      ffremove<-list.files(pattern=paste("\\.",ext,"$",sep=""))
      ww<-which(ffremove==paste("figure",ext,sep="."))  
      if (length(ww)>0)
        ffremove<-ffremove[-which(ffremove==paste("figure",ext,sep="."))]
      sapply(ffremove,file.remove)
    }
  )  
  
  
  
})

setwd(wd)
