cGroupsGUI <- function(X){

  call<-match.call()

  if (!missing(X)){
         datatemp.name<-as.character(call)[2]
         if (!is.matrix(X) & !is.data.frame(X))  
            stop("X must be a matrix or a data.frame")
         if (is.matrix(X))    
            x <- as.data.frame(X)
         if (is.data.frame(X))    
            x <- X
  }
  
  if (missing(X)){ 
    datatemp.name<-"regicor"
    data(regicor,package="compareGroups")
    X <- regicor
    x <- X
  }
  
  XX <- x
  
  assign(datatemp.name, X, envir = .GlobalEnv)
  
  next.panel.function <- function(){

    var.names <- as.character(tkget(tlist.var.selection, 0, "end"))
    factor.selected <- as.character(tkget(tlist.factor.selection, 0, "end"))

    if (length(var.names)==0) return(tkmessageBox(message = "Select at least one variable to report", icon = "info", type = "ok"))
    if (length(factor.selected)==0) return(tkmessageBox(message = "Select one factor to report", icon = "info", type = "ok"))

    if (length(as.character(tkwinfo("children",tt)))==4){
          child <- as.character(tkwinfo("children",tt))[4]
          tt2 <- list()
          tt2$ID <- child
          tt2$env <- tt$env
          class(tt2) <- "tkwin"
          tkdestroy(tt2)}
    if (length(as.character(tkwinfo("children",tt)))<4)  tt2 <- tktoplevel(tt, class="children")
    tktitle(tt2) <- paste("Export options:",datatemp.name)
    tkwm.resizable(tt2,0,0)
    topframe2 <- tkframe(tt2)
    tkgrid(topframe2, column=0, row=0, sticky="nwes", ipadx=5,ipady=5,padx=12,pady=12)
    frame8 <- tkwidget(topframe2, "labelframe", text ="Variable to report", fg="blue",padx=10,pady=10)
    frame9 <- tkwidget(topframe2, "labelframe", text ="General options", fg="blue",padx=10,pady=10)
    frame10 <- tkwidget(topframe2, "labelframe", text ="Options for (CAT) variables", fg="blue",padx=10,pady=10)

    type.vars <- as.character()
    for (i in 1:length(var.names)){
      type.vars[i] <- strsplit(var.names,"-")[[i]][2]
    }

    export.scr <- tkscrollbar(frame8, repeatinterval=5,command=function(...)tkyview(tlist.export,...))
    tlist.export <- tklistbox(frame8,height=15,selectmode="extended", yscrollcommand=function(...)tkset(export.scr,...),
                    background="white",exportselection=FALSE, height =21, width =15)
    for (i in (1:length(var.names))){
        if (is.na(type.vars[i])){
            var.names[i] <- paste(var.names[i],"-(NORM)",sep="")
        }
        tkinsert(tlist.export,"end",var.names[i])
    }
    tkbind(tlist.export)
    tkgrid(tlist.export,export.scr)
    tkgrid.configure(export.scr,rowspan=4,sticky="nsw")
    tkgrid(frame8, column=1, row=1, sticky="nse", rowspan=18, padx=10)

    type.vars <- as.character()
    var.names <- as.character(tkget(tlist.export, 0, "end"))
    for (i in 1:length(var.names)){
      type.vars[i] <- strsplit(var.names,"-")[[i]][2]
    }
    
    decimals <- rep(4,length(var.names))
    decimals.function <- function(){
        dec.val <- as.character(tclvalue(dec.value))
        pos <- as.numeric(tkcurselection(tlist.export))+1
        if (length(pos)==0)   return(tkmessageBox(message = "No variable selected to change decimals digits", icon = "info", type = "ok"))
        if(dec.val=="1dec") tmp <- 1
        if(dec.val=="2dec") tmp <- 2
        if(dec.val=="3dec") tmp <- 3
        if(dec.val=="defdec") tmp <- 4
        decimals[pos] <<- tmp
        return(decimals)
    }

    dec1 <- tkradiobutton(frame9)
    dec2 <- tkradiobutton(frame9)
    dec3 <- tkradiobutton(frame9)
    dec4 <- tkradiobutton(frame9)
    dec.value <- tclVar("defdec")
    tkconfigure(dec1, variable = dec.value, value = "1dec", command = decimals.function)
    tkconfigure(dec2, variable = dec.value, value = "2dec", command = decimals.function)
    tkconfigure(dec3, variable = dec.value, value = "3dec", command = decimals.function)
    tkconfigure(dec4, variable = dec.value, value = "defdec", command = decimals.function)
    tkgrid(tklabel(frame9,text=""))
    tkgrid(tklabel(frame9,text="Decimals digits",fg="blue"), sticky="nw")
    tkgrid(tklabel(frame9,text="1 dec"),dec1)
    tkgrid(tklabel(frame9,text="2 dec"),dec2)
    tkgrid(tklabel(frame9,text="3 dec"),dec3)
    tkgrid(tklabel(frame9,text="Default dec"),dec4)

    if (sum(unique(x[,factor.selected])%nin%NA) <3){
        results.show2 <- tkcheckbutton(frame9)
        results.show3 <- tkcheckbutton(frame9)
        results.value2 <- tclVar("0")
        results.value3 <- tclVar("0")
        tkconfigure(results.show2,variable=results.value2, state= "disabled")
        tkconfigure(results.show3,variable=results.value3, state= "disabled")
        tkgrid(tklabel(frame9,text="P-values in report",fg="blue"), sticky="nw")
        tkgrid(tklabel(frame9,text="P trend"),results.show2)
        tkgrid(tklabel(frame9,text="Multiple comparisons"),results.show3)
    }

    if (sum(unique(x[,factor.selected])%nin%NA) >=3){
        results.show2 <- tkcheckbutton(frame9)
        results.show3 <- tkcheckbutton(frame9)
        results.value2 <- tclVar("1")
        results.value3 <- tclVar("1")
        tkconfigure(results.show2,variable=results.value2)
        tkconfigure(results.show3,variable=results.value3)
        tkgrid(tklabel(frame9,text="P-values in report",fg="blue"), sticky="nw")
        tkgrid(tklabel(frame9,text="P trend"),results.show2)
        tkgrid(tklabel(frame9,text="Multiple comparisons"),results.show3)
    }
  
  
    type.csv1 <- tkradiobutton(frame9)
    type.csv2 <- tkradiobutton(frame9)
    csv.value <-  tclVar("comma")
    tkconfigure(type.csv1, variable = csv.value, value = "comma", command = I)
    tkconfigure(type.csv2, variable = csv.value, value = "pcomma", command = I)
    tkgrid(tklabel(frame9,text="Separator for .csv export",fg="blue"), sticky="nw")
    tkgrid(tklabel(frame9,text="comma (,)"),type.csv1)
    tkgrid(tklabel(frame9,text="semicolon (;)"),type.csv2)
    
    results.n <- tkcheckbutton(frame9)
    results.n.value <- tclVar("1")
    tkconfigure(results.n,variable=results.n.value)
    tkgrid(tklabel(frame9,text="Show N in each variable",fg="blue"),results.n, sticky="nw")
    
    results.all <- tkcheckbutton(frame9)
    results.value1 <- tclVar("1")
    tkconfigure(results.all ,variable=results.value1)
    tkgrid(tklabel(frame9,text="Show 'ALL' column",fg="blue"),results.all, sticky="nw")
    tkgrid(frame9, column=2, row=1,padx=10, sticky="nse", rowspan=18)

    elim.cat <- rep(3,length(var.names))
    elim.cat.function <- function(){
        cat.val <- as.character(tclvalue(cat.value))
        pos <- as.numeric(tkcurselection(tlist.export))+1
        if (length(pos)==0) return(tkmessageBox(message = "No variable selected to hide category", icon = "info", type = "ok"))
        alarm <- 0
        if (length(pos)==1 & all(type.vars[pos]=="(CAT)")){
              if(cat.val=="1cat") elim.cat[pos] <<-  1
              if(cat.val=="2cat") elim.cat[pos] <<-  2
              if(cat.val=="Nocat") elim.cat[pos] <<-  3
              if(cat.val=="Check" & (var.names[pos]%in%names(x)))  plot(factor(x[,var.names[pos]]), main =paste("Plot of ",strsplit(var.names[pos],"-")[[1]][1]), xlab="")
              if(cat.val=="Check" & (var.names[pos]%nin%names(x)))  plot(factor(x[,strsplit(var.names[pos],"-")[[1]][1]]), main =paste("Plot of ",strsplit(var.names[pos],"-")[[1]][1]), xlab="")
        }
        if (length(pos)==1 & any(type.vars[pos]!="(CAT)")) return(tkmessageBox(message = "Only hide category for (CAT) variables", icon = "info", type = "ok"))
        if (length(pos)>1){
              alarm <- 0
              if(cat.val=="Check")  return(tkmessageBox(message = "Only check possible for one (CAT) variable", icon = "info", type = "ok"))
              for (i in 1:length(pos)){
                    if (type.vars[pos[i]]!="(CAT)") alarm <- 1
                    if (type.vars[pos[i]]=="(CAT)"){
                          if(cat.val=="1cat") elim.cat[pos[i]] <<-  1
                          if(cat.val=="2cat") elim.cat[pos[i]] <<-  2
                          if(cat.val=="Nocat") elim.cat[pos[i]] <<-  3
                    }
              }
              if (alarm==1){
                  tkmessageBox(message = "Only apply change to (CAT) variables", icon = "info", type = "ok")
                  return(elim.cat)
              }
        return(elim.cat)
        }

    }
    cat1 <- tkradiobutton(frame10)
    cat2 <- tkradiobutton(frame10)
    cat3 <- tkradiobutton(frame10)
    cat4 <- tkradiobutton(frame10)
    cat.value <-  tclVar("Nocat")
    tkconfigure(cat1, variable = cat.value, value = "1cat", command = elim.cat.function)
    tkconfigure(cat2, variable = cat.value, value = "2cat", command = elim.cat.function)
    tkconfigure(cat3, variable = cat.value, value = "Nocat", command = elim.cat.function)
    tkconfigure(cat4, variable = cat.value, value = "Check", command = elim.cat.function)
    tkgrid(tklabel(frame10,text=""))
    tkgrid(tklabel(frame10,text="Hide category",fg="blue"),sticky="nw")
    tkgrid(tklabel(frame10,text="First Category"),cat1)
    tkgrid(tklabel(frame10,text="Last Category"),cat2)
    tkgrid(tklabel(frame10,text="No Category"),cat3)
    tkgrid(tklabel(frame10,text="Check Category"),cat4)

    type.cat1 <- tkradiobutton(frame10)
    type.cat2 <- tkradiobutton(frame10)
    type.cat.value <-  tclVar("nperc")
    tkconfigure(type.cat1, variable = type.cat.value, value = "nperc", command = I)
    tkconfigure(type.cat2, variable = type.cat.value, value = "perc", command = I)
    tkgrid(tklabel(frame10,text="Type",fg="blue"), sticky="nw")
    tkgrid(tklabel(frame10,text="n (%)"),type.cat1)
    tkgrid(tklabel(frame10,text="(%)"),type.cat2)
    
    tkgrid(frame10, column=3, row=1,padx=10, sticky="nse", rowspan=18)


    check.function <- function(){
        tmp.dec <- factor(decimals, levels = c(1,2,3,4), labels = c("1","2", "3", "Default"))
        tmp.cat <-  factor(elim.cat, levels = c(1,2,3,4), labels = c("First","Last", "No hide category", "Check"))
        matrix.inputs <- data.frame(var.names,tmp.dec, subset.selection, tmp.cat)
        names(matrix.inputs) <- c("Report variables (method)", "Decimals digits", "Subset", "Hide a category")
        View(matrix.inputs, title = "Summary of Variables Parameters")
    }


    check.panel <- tkbutton(topframe2,text="  >> CHECKING >>   ",command = check.function, fg="red", height=3, width=20)
    tkbind(check.panel)
    tkgrid(check.panel, padx=10, pady=1,column=4, row=1,sticky="s")
    
    create.function <- function(){
        name.vars <- as.character()
        for (i in 1:length(var.names)){
            name.vars[i] <- strsplit(var.names,"-")[[i]][1]
        }
        XX <- get(datatemp.name, envir = .GlobalEnv)[,name.vars, drop=FALSE]
        y.name<-strsplit(factor.selected,"-")[[1]][1]
        y <- get(datatemp.name, envir = .GlobalEnv)[,y.name]
        
        if (label(y)=="") 
          label(y)<-y.name
        
        subs <- ifelse(subset.selection=="",NA,paste("with(",datatemp.name,",",subset.selection,")",sep=""))
        method <- ifelse(type.vars=="(NORM)",1,
                  ifelse(type.vars=="(NNORM)",2,
                  ifelse(type.vars=="(CAT)",3,NA)))
        alpha <-  as.numeric(tkget(alpha.slider))
        type <-   as.character(tclvalue(type.cat.value))
        type <- ifelse(type=="nperc",2,1)
        digits <- ifelse(decimals==4,NA,decimals)
        show.all <- as.numeric(tclvalue(results.value1))==1
        show.p.trend <- as.numeric(tclvalue(results.value2))==1
        show.p.mul <-  as.numeric(tclvalue(results.value3))==1        
        show.n <-  as.numeric(tclvalue(results.n.value))==1
        hide <- ifelse(elim.cat==3,NA,elim.cat)
        hide <- ifelse(elim.cat==2,Inf,hide)
        sep.csv <- ifelse(as.character(tclvalue(csv.value))=="comma",",",";")
        GetDir <- function(){
            directory <- tclvalue(tkgetSaveFile(parent = tt2,initialfile=paste("CompareGroups",datatemp.name,sep="_"),
                          title = "Select path to save the table"))
            directory <- as.character(directory)
            if (directory=="") return()
            return(directory)
        }
        directory <- GetDir()

        if(!inherits(directory,"NULL")){
            ans <- try(compareGroups(X = XX, y = y, selec = subs, method = method, alpha = alpha, min.dis = 5, max.ylev = 2000))
            if(inherits(ans,"try-error")) return(tkmessageBox(message = "Error occurred in 'compareGroups' function and not created", icon = "info", type="ok"))
            if(!inherits(ans,"try-error")) ans <- try(createTable(x=ans,hide=hide, digits=digits,type=type, show.all=show.all, show.p.trend=show.p.trend,show.p.mul=show.p.mul,show.n=show.n))
            if(inherits(ans,"try-error")) return(tkmessageBox(message = "Error occurred in 'createTable' function and not created", icon = "info", type="ok"))
            if(!inherits(ans,"try-error")){
               export2latex(ans,file=directory)
               export2csv(ans, file=directory, sep=sep.csv)
               return(tkmessageBox(message = "Compare Groups Table has been created", icon = "info", type="ok"))
            }
        }

    }
    
    create.panel <- tkbutton(topframe2,text="  >> CREATE >>   ",command = create.function, fg="red", height=3, width=20)
    tkbind(create.panel)
    tkgrid(create.panel, padx=10, pady=1,column=4, row=2,sticky="s")

  }

  tt <- tktoplevel()
  tktitle(tt) <- paste("Compare Groups:",datatemp.name)
  tkwm.resizable(tt,0,0)
  topframe <- tkframe(tt)
  tkgrid(topframe, column=0, row=0, sticky="nwes", ipadx=3,ipady=3,padx=12,pady=12)

  frame1 <- tkwidget(topframe, "labelframe", text ="Variables in data frame", fg="blue",padx=10,pady=10)
  frame2 <- tkwidget(topframe, "labelframe", text ="Factor to report", fg="blue",padx=10,pady=10)
  frame3 <- tkwidget(topframe, "labelframe", text ="Variables to report", fg="blue",padx=10,pady=10)
  frame4 <- tkwidget(topframe, "labelframe", text ="Significance level for S-W test", fg="blue",padx=5,pady=5)
  frame5 <- tkwidget(topframe, "labelframe", text ="Method", fg="blue",padx=1,pady=1)
  frame6 <- tkwidget(topframe, "labelframe", text ="Plot", fg="blue",padx=5,pady=5)
  frame7 <- tkwidget(topframe, "labelframe", text ="Subset of global data frame", fg="blue",padx=5,pady=5)

  GetFile <- function(){
      name <- tclvalue(tkgetOpenFile(parent = tt, title = "Load R/SPSS Data",
                filetypes = paste("{{R Data Files} {.RData .RData .rdata .rda .RDA}}","{{SPSS Files} {.sav}}")))
      if (name=="") return(" ")
      if (length(name)>1)  stop(paste("More than one object in",name))
      data.type <-  strsplit(name,".", fixed=TRUE)[[1]][2]
      if (data.type%in%c("RData","RData","rdata","rda","RDA")){
              datatemp.name <- try(load(file = name, envir = .GlobalEnv))
              if (inherits(datatemp.name,"try-error")) {
                return("Problems loading .RData")
              }else{
               locate <- substring(as.character(tkwm.geometry(tt)),first = 8,last = 50)
               eval(parse(text=paste("cGroupsGUI(",datatemp.name,")",sep="")))
               subset.selection <<-""
               tkwm.geometry(tt,locate)
               tkdestroy(tt)
              }

      }
      if (data.type%in%c("sav")){
               CGData <- try(spss.get(file = name))
               if (inherits(CGData,"try-error")){
                  return("Problems loading .sav")
               } else{
                 datatemp.name<-sub(".sav$","",basename(name),ignore.case=TRUE)
                 assign(datatemp.name,CGData,envir=.GlobalEnv)
                 eval(parse(text=paste("cGroupsGUI(",datatemp.name,")",sep="")))
                 subset.selection <<-""
                 tkdestroy(tt)
               }
     }
     datatemp.name<<-datatemp.name
  }


  topMenu <- tkmenu(tt)
  tkconfigure(tt,menu=topMenu)
  fileMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(topMenu,"command",label="Load Data",command=function() GetFile())


  var.scr  <- tkscrollbar(frame1, repeatinterval=5,command=function(...)tkyview(tlist.var,...))
  tlist.var <- tklistbox(frame1,height=5,selectmode="extended", yscrollcommand=function(...)tkset(var.scr,...),
            background="white",exportselection=FALSE, height =20, width =15)

  for (i in (1:length(x))){
      aux <- ifelse(is.factor(x[,names(x)[i]]), as.character(paste(names(x)[i],"-(CAT)",sep="")),as.character(names(x)[i]))
      aux <- ifelse(is.character(x[,names(x)[i]]), as.character(paste(names(x)[i],"-(CAT)",sep="")),aux)
      if(is.factor(x[,names(x)[i]])) x[,names(x)[i]] <- factor(x[,names(x)[i]])
      if(is.character(x[,names(x)[i]])) x[,names(x)[i]] <- factor(x[,names(x)[i]])
      #aux <- ifelse(length(unique(x[,names(x)[i]]))<6, as.character(paste(names(x)[i],"-(CAT)",sep="")),aux)
      #if(length(unique(x[,names(x)[i]]))<6)  x[,names(x)[i]] <- factor(x[,names(x)[i]])
      tkinsert(tlist.var,"end",aux)
      if(!is.numeric(x[,names(x)[i]]) &
         !is.integer(x[,names(x)[i]]) &
         !is.double(x[,names(x)[i]]) &
         !is.character(x[,names(x)[i]]) &
         !is.factor(x[,names(x)[i]]))   tkdelete(tlist.var,"end")
      

  }
  tkbind(tlist.var)
  tkgrid(tlist.var,var.scr)
  tkgrid.configure(var.scr,rowspan=4,sticky="nsw")
  tkgrid(frame1, padx=5,pady=5, column=1, row=1)
  for (i in 0:(ncol(x)-1)){
      names(x)[i+1] <- as.character(tkget(tlist.var,i))
  }
  xori <- x
  

  var.select <- function(){
                    selection <- as.character(names(x)[as.numeric(tkcurselection(tlist.var))+1])
                    return(selection)}
  var.export <- function(){
                    export.sel <- as.numeric(tkcurselection(tlist.var.selection))
                    return(export.sel)}
  select.factor <- function(){
      	var.name <- var.select()
      	if( length(var.name)==0) 
           tkmessageBox(message = "No factor selected", icon = "info", type="ok")
        if( length(var.name)==1){
                  #if(!is.factor(x[,var.name]))  
                  #    tkmessageBox(message = "Only for (CAT) variables", icon = "info", type="ok")
                  #if(is.factor(x[,var.name])){
                      tkdelete(tlist.factor.selection,0,"end")
                      tkinsert(tlist.factor.selection,"active",var.name)
                  #}
        }
        if(length(var.name)>1)
           tkmessageBox(message = "Select only one variable", icon = "info", type="ok")
  }

  factor.plot <- function(){
        factor.sel <- as.character(tkget(tlist.factor.selection,0,"end"))
        if(length(factor.sel)==0) tkmessageBox(message = "No factor selected to plot", icon = "info", type = "ok")
        if(length(factor.sel)>0){
          name.plot <- strsplit(factor.sel,"-")[[1]][1]
          plot(factor(x[,factor.sel]),main=paste("Plot of",name.plot,sep=" "),xlab="")}
  }

  select.report <- function(){
        var.name <- var.select()
      	n <- length(tkget(tlist.var.selection,0,"end"))
      	if(length(var.name)==0) tkmessageBox(message = "No variable selected", icon = "info", type="ok")
        if(length(var.name)>0){
            for (i in 1:length(var.name)){
            tkinsert(tlist.var.selection,"end",var.name[i])
            }
        }
        tksee(tlist.var.selection,"end")
  }
  extract.report <- function(){
      var.name <- var.export()
      if (length(var.name)==0) tkmessageBox(message = "No variable selected to exclude", icon = "info", type = "ok")
      if (length(var.name)>0){
      for (i in 1:length(var.name)){
          var.name <- var.export()
          tkdelete(tlist.var.selection,var.name[1])
          }
      }
  }


  tlist.factor.selection <- tklistbox(frame2, height=1, width=18, selectmode="single",background="white", exportselection=FALSE)
  tkbind(tlist.factor.selection)
  tkgrid(tlist.factor.selection,column=2,row=1, padx=10)
  
  select.report.factor <- tkbutton(frame2,text="     Select factor -->    ",command = select.factor,height=1, width=20)
  tkgrid(select.report.factor,column=1,row=1, padx=10,sticky="e")
  tkbind(tlist.factor.selection, select.report.factor)
  
  select.plot.factor <- tkbutton(frame2,text="    Factor  Plot       ",command = factor.plot,height=1, width=20)
  tkbind(factor.plot, select.plot.factor)
  tkgrid(select.plot.factor,sticky = "w",column=3,row=1, padx=10)
  tkgrid(frame2,  padx=5,pady=5, column=2, row=1, sticky="ne",columnspan=3)


  tlist.var.selection <- tklistbox(frame3,height=10,selectmode="extended", yscrollcommand=function(...)tkset(var.scr.selection,...),
            background="white", exportselection=FALSE,width =18)
  var.scr.selection <- tkscrollbar(frame3, repeatinterval=5,command=function(...)tkyview(tlist.var.selection,...))

  tkbind(tlist.var.selection)
  tkbind(var.scr.selection)
  tkgrid(tlist.var.selection,var.scr.selection,column=2,row=1, padx=10)
  tkgrid.configure(var.scr.selection,rowspan=4,sticky="nse",column=2)


  select.report.var <-tkbutton(frame3,text="     Select Variable -->   ",command = select.report,height=1, width=20)
  tkbind(tlist.var.selection,select.report.var)
  tkgrid(select.report.var, rowspan=1,column=1,row=1,sticky="ne",padx=10)


  select.extract.var <-tkbutton(frame3,text="   <-- Exclude Variable  ",command = extract.report,height=1, width=20)
  tkbind(tlist.var.selection,select.extract.var)
  tkgrid(select.extract.var, rowspan=1,column=1,row=1,sticky="se",padx=10)

  tkgrid(frame3,padx=5,pady=5, column=2, row=1, sticky="ew",columnspan=3)


  subset.nul <- tclVar("")
  subset.selection <- tclvalue(subset.nul)
  text.subset <- tkentry(frame7, width= 30,textvariable = subset.nul)
  tkbind(text.subset)

  send.subset <- function(){
      subset.select <- tclvalue(subset.nul)
      subset.dataframe <- try(eval(parse(text=paste("subset(",datatemp.name,",subset=",subset.select,")",sep=""))), silent=TRUE)
      if(inherits(subset.dataframe, "try-error"))  
          return(tkmessageBox(message = "Syntax error. Try again", icon = "info", type = "ok"))
      if(nrow(subset.dataframe)==0) 
          return(tkmessageBox(message = paste("0 people will be analyzed. No applying subset"), icon = "info", type = "ok"))
      if(nrow(subset.dataframe)>0){
            tkmessageBox(message = paste("Correct syntax"), icon = "info", type = "ok")
            subset.selection <<- subset.select
            return(x <<- xori[row.names((subset.dataframe)),])
      }
            
  }

  tkgrid.configure(text.subset,rowspan=4,sticky="se")
  tkbind(send.subset)
  subset.but <-tkbutton(frame7,text="   Apply subset   ",command=send.subset)
  tkbind(subset.but, "<Return>",send.subset)
  tkgrid(frame7,padx=5,pady=5, column=2, row=1, sticky="ws",columnspan=3)
  tkgrid(subset.but, row=1,column=2, sticky="ws")
  

  type.var.function <- function(){
      type.var.val <- as.character(tclvalue(type.var.value))
      var.names <- as.character(tkget(tlist.var.selection,0,"end"))
      var.export <- var.export()
      alpha <- as.numeric(tkget(alpha.slider))
      if(length(var.export)==0) return(tkmessageBox(message = "No variable selected to change method", icon = "info", type = "ok"))
      alarm <- 0
      if(length(var.export)>0) {
            var.names <- var.names[var.export+1]
            for (i in 1:length(var.export)){
                var.names.ori <- strsplit(var.names[i],"-")[[1]][1]
                if (var.names.ori%in%names(x)) var.names[i] <- var.names.ori 
                if (!is.factor(x[,var.names[i]]) & type.var.val=="non.param") nvar <- paste(var.names[i],"-(NNORM)",sep="")
                if (!is.factor(x[,var.names[i]]) & type.var.val=="yes.param") nvar <- paste(var.names[i],"-(NORM)",sep="")
                if (!is.factor(x[,var.names[i]]) & type.var.val=="cat") nvar <- paste(var.names[i],"-(CAT)",sep="")
                if (!is.factor(x[,var.names[i]]) & type.var.val=="test.param"){
                       if (length(unique(x[,var.names[i]]))>=6){
                            SW <- shapiro.test(x[,var.names[i]])$p.value
                            if (SW<alpha)   nvar <- paste(var.names[i],"-(NNORM)",sep="")
                            if (SW>=alpha)   nvar <- paste(var.names[i],"-(NORM)",sep="")
                       }else{
                            nvar <- paste(var.names[i],"-(CAT)",sep="")
                            tkmessageBox(message = paste("Less than 6 different values for '",var.names[i],"'",". Not performs S-W test",sep=""), icon = "info", type = "ok")
                       }
                }
                if (is.factor(x[,var.names[i]]) & type.var.val!="cat") alarm <- 1
                if(!is.factor(x[,var.names[i]])) {
                     tkinsert(tlist.var.selection,var.export[i],nvar)
                     tkdelete(tlist.var.selection,var.export[i]+1)
                     tkselection.set(tlist.var.selection,var.export[i],var.export[i])
                     tkactivate(tlist.var.selection,var.export[i])
                }

            }
          if (alarm==1) tkmessageBox(message = "Only change method to no(CAT) variables", icon = "info", type = "ok")
       }

  }
  type.var1 <- tkradiobutton(frame5)
  type.var2 <- tkradiobutton(frame5)
  type.var3 <- tkradiobutton(frame5)
  type.var4 <- tkradiobutton(frame5)
  type.var.value <-  tclVar("test.param")
  tkconfigure(type.var1, variable = type.var.value, value = "cat", command = type.var.function)
  tkconfigure(type.var2, variable = type.var.value, value = "non.param", command = type.var.function)
  tkconfigure(type.var3, variable = type.var.value, value = "yes.param", command = type.var.function)
  tkconfigure(type.var4, variable = type.var.value, value = "test.param", command = type.var.function)
  tkgrid(tklabel(frame5,text="Categorical"),type.var1)
  tkgrid(tklabel(frame5,text="Non Normal"),type.var2)
  tkgrid(tklabel(frame5,text="Normal"),type.var3)
  tkgrid(tklabel(frame5,text="Test (S-W)"),type.var4)
  tkgrid(frame5, row=1, column=4,columnspan=2, sticky="w")


  cplot <-  function(var1, var2, selected.v, selected.f){
       at <- table(var2, var1)
       barplot(at, beside=TRUE, main = paste("Barplot of",selected.v,"by",selected.f, sep=" "))
  }
  qplot <- function(var1, var2, selected.v, selected.f){
      boxplot(var1 ~ var2, main = paste("Boxplot of",selected.v,"by",selected.f, sep=" "))
  }

  AlphaValue <- tclVar("0.05")
  AlphaValueLabel <- tklabel(frame4,text=as.character(tclvalue(AlphaValue)))
  tkgrid(tklabel(frame4,text=""),AlphaValueLabel,tklabel(frame4,text=""))
  tkconfigure(AlphaValueLabel,textvariable=AlphaValue)
  alpha.slider <- tkscale(frame4, from=1, to=0,showvalue=F, variable=AlphaValue,resolution=0.01, orient="horizontal")
  tkgrid(frame4, column=3, row=1, sticky="se",columnspan=2,rowspan=3, padx=5)
  tkgrid(alpha.slider,padx=1,pady=1, column=3, row=1, sticky="se")


  type.plot.function <- function(){
      type.var.val <- as.character(tclvalue(type.plot.value))
      sel.vars <- as.character(tkget(tlist.var.selection,0,"end"))
      f.var <- as.character(tkget(tlist.factor.selection,0,"end"))
      tmp <- var.export()
      if(length(tmp)==0) return(tkmessageBox(message = "No variable selected to plot", icon = "info", type = "ok"))
      if(length(tmp)>1) return(tkmessageBox(message = "Select only one variable to plot", icon = "info", type = "ok"))
      sel.vars <- sel.vars[tmp+1]
      if(length(tmp)==1){
          sel.vars2 <- strsplit(sel.vars,"-")[[1]][1]
          sel.vars3 <- strsplit(sel.vars,"-")[[1]][2]
          if (sel.vars2%in%names(x)) sel.vars <- strsplit(sel.vars,"-")[[1]][1]
      }
      if(length(tmp)==1 & type.var.val=="univariate" & is.factor(x[,sel.vars])) 
          return(plot(factor(x[,sel.vars]), main =paste("Plot of ",sel.vars2), xlab=""))
      if(length(tmp)==1 & type.var.val=="univariate" & !is.factor(x[,sel.vars]) & (!is.na(sel.vars3) & sel.vars3=="(CAT)")) 
          return(plot(factor(x[,sel.vars]),main =paste("Plot of ",sel.vars2),xlab=""))
      if(length(tmp)==1 & type.var.val=="univariate" & !is.factor(x[,sel.vars]) & ((!is.na(sel.vars3) & sel.vars3!="(CAT)") | is.na(sel.vars3))){ 
          old.par <- par(no.readonly = TRUE) 
          norm.plot(as.numeric(x[,sel.vars]),sel.vars,z=1.5 ,n.breaks="Sturges",plot.new=FALSE)
          par(old.par)
          return(invisible(NULL))
      }
      if(length(f.var)==0 & type.var.val=="bivariate") 
          return(tkmessageBox(message = "No factor selected to plot", icon = "info", type = "ok"))
      if(length(tmp)==1 & type.var.val=="bivariate" & is.factor(x[,sel.vars])) 
         return(cplot(factor(x[,f.var]),factor(x[,sel.vars]), strsplit(sel.vars,"-")[[1]][1], strsplit(f.var,"-")[[1]][1]))
      if(length(tmp)==1 & type.var.val=="bivariate" & !is.factor(x[,sel.vars])  & (!is.na(sel.vars3) & sel.vars3=="(CAT)")) 
          return(cplot(factor(x[,f.var]),factor(x[,sel.vars]), strsplit(sel.vars,"-")[[1]][1], strsplit(f.var,"-")[[1]][1]))
      if(length(tmp)==1 & type.var.val=="bivariate" & !is.factor(x[,sel.vars])  & ((!is.na(sel.vars3) & sel.vars3!="(CAT)") | is.na(sel.vars3))) 
         return(qplot(as.numeric(x[,sel.vars]), factor(x[,f.var]), sel.vars, strsplit(f.var,"-")[[1]][1]))
   }      
                      
  type.plot1 <- tkradiobutton(frame6)
  type.plot2 <- tkradiobutton(frame6)
  type.plot.value <-  tclVar("univariate")
  tkconfigure(type.plot1, variable = type.plot.value, value = "univariate", command = type.plot.function)
  tkconfigure(type.plot2, variable = type.plot.value, value = "bivariate", command = type.plot.function)
  tkgrid(tklabel(frame6,text="Univariate"),type.plot1)
  tkgrid(tklabel(frame6,text="Bivariate"),type.plot2)
  tkgrid(frame6, column=5, row=1, sticky="e")

  next.panel <- tkbutton(topframe,text="  >> NEXT >>   ",command = next.panel.function, fg="red", height=4)
  tkbind(next.panel)
  tkgrid(next.panel,row=1,column=5, sticky="se",columnspan=4)
  tkfocus(tt) 
   
}

