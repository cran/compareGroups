cGroupsGUI <- function(X){
      call <- match.call()
      if (!missing(X)){
         if (!is.matrix(X) & !is.data.frame(X))  stop("X must be a matrix or a data.frame")
         if (is.matrix(X))    x <- as.data.frame(X)
         if (is.data.frame(X))    x <- X
        datatemp.name <- as.character(call)[2]
      } else {
         x <- data(regicor)
         datatemp.name <- x
         x <- get(x)
      }
      assign(".p.mult","0", envir = .GlobalEnv)                 
      assign(".p.trend","0", envir = .GlobalEnv)              
      assign(".show.all","1", envir = .GlobalEnv) 
      assign(".show.n","1", envir = .GlobalEnv)              
      assign(".type.cat.value","nperc", envir = .GlobalEnv) 
      assign(datatemp.name, x, envir = .GlobalEnv)
      matrix.info <- data.frame(c(NA,NA))
      matrix.info[,1:7] <- NA
      names(matrix.info) <- c("name","type","digits","hide","subset.all","subset.part","label")
      matrix.info[,2] <- 0
      for (i in 1:ncol(x)){
         matrix.info[i,1] <- names(x)[i]
         if(is.factor(x[,i])) matrix.info[i,2] <- 3
         if(is.character(x[,i])) matrix.info[i,2] <- 3
         if(is.numeric(x[,i])) matrix.info[i,2] <- 1
         if(inherits(x[,i], "Surv")) matrix.info[i,2] <- 0
         if(length(unique(x[,i]))<6)  matrix.info[i,2] <- 3
         if(is.na(matrix.info[i,2])) matrix.info[i,2] <- 1
         matrix.info[i,7] <- label(x[,names(x)[i]])
      }
      matrix.info[,3] <- 1
      matrix.info[,4] <- 0
      matrix.info <- matrix.info[matrix.info$type!=0,]
      matrix.info$subset.part <- ""
      matrix.info$subset.all <- ""
      matrix.info$type[matrix.info$type==1] <- "Normal"
      matrix.info$type[matrix.info$type==2] <- "Non-Normal"
      matrix.info$type[matrix.info$type==3] <- "Categorical"
      matrix.info$type[matrix.info$type==4] <- "Survival"
      matrix.info$hide <- "No"
      read.list <- function(){
         sel1 <- cbind(as.character(tkget(report.list,0,"end")))
         mat <- data.frame(NA,NA)
         mat[,1:6] <- NA
         names(mat) <- c("names","type","digits","hide","subs","label")
         for (i in 1:length(sel1)){
             size <- length(strsplit(sel1," ")[[i]])
             sel2 <- strsplit(sel1," ")
             nam <- sel2[[i]][1]
             typ <- sel2[[i]][2]
             dig <- sel2[[i]][3]
             hid <- sel2[[i]][4]
             tmp <- grep("{",sel2[[i]],fixed=TRUE)
             tmp2 <- grep("}",sel2[[i]],fixed=TRUE)
             if(length(tmp)==0){
                 subs <- sel2[[i]][5]
                 labl <- sel2[[i]][6]
             }
             if(length(tmp)==1){
                 if(length(grep("{",sel2[[i]][5],fixed=TRUE))==1){
                      subs <-  paste(sel2[[i]][tmp[1]:tmp2[1]], collapse=" ")
                      subs <- substring(subs,2,nchar(subs)-1)
                      labl <- sel2[[i]][size]
                  }
                  if(length(grep("{",sel2[[i]][6],fixed=TRUE))==1){
                      labl <- paste(sel2[[i]][tmp[1]:tmp2[1]], collapse=" ")
                      labl <- substring(labl,2,nchar(labl)-1)
                      subs <- sel2[[i]][5]
                  }
              }
              if (length(tmp)==2){
                  if (tmp[1]==tmp2[1]) subs <- ""
                  if (tmp[1]!=tmp2[1]){
                      subs <-  paste(sel2[[i]][tmp[1]:tmp2[1]], collapse=" ")
                      subs <- substring(subs,2,nchar(subs)-1)
                  }
                  if (tmp[2]==tmp2[2]) labl <- ""
                  if (tmp[2]!=tmp2[2]){
                        labl <-  paste(sel2[[i]][tmp[2]:tmp2[2]], collapse=" ")
                        labl <- substring(labl,2,nchar(labl)-1)
                  }
              }
              mat[i,] <- c(nam, typ, dig, hid,subs, labl)
            }
         return(mat)
      }
      tt <- tktoplevel()
      tktitle(tt) <- paste("Compare Groups : ",datatemp.name,sep="")
      tkwm.resizable(tt,0,0)
      topframe <- tkframe(tt)
      tkgrid(topframe, column=0, row=0, sticky="nwes", ipadx=3,ipady=3,padx=12,pady=20)
      frame1 <- tkwidget(topframe, "labelframe", text ="Variables in data frame", fg="blue",padx=10,pady=10)
      var.scr  <- tkscrollbar(frame1, repeatinterval=5,command=function(...)tkyview(tlist.var,...))
      tlist.var <- tklistbox(frame1,height=5,selectmode="extended", yscrollcommand=function(...)tkset(var.scr,...),
                    background="white",exportselection=FALSE, height =37, width =15)
      for (i in 1:nrow(matrix.info)){
          tkinsert(tlist.var,"end",matrix.info[i,1])
      }
      tkbind(tlist.var)
      tkgrid(tlist.var,var.scr)
      tkgrid.configure(var.scr,sticky="nsw")
      tkgrid(frame1, padx=5,pady=5, column=1,rowspan=40,sticky="ns")
      tk2tip(tlist.var,"List of variables in data frame")
      frame2 <- tkwidget(topframe, "labelframe", text ="Factor to report", fg="blue")
      disable1 <- function(){
        tk2state.set(tlist.factor.selection, state='disabled')
        tk2state.set(tlist.timeto.selection, state='disabled')
        tk2state.set(tlist.status.selection, state='disabled')
        tk2state.set(entry4, state='disabled')
        tk2state.set(entry.factor, state='disabled')
        tk2state.set(select.timeto1, state='disabled')
        tk2state.set(select.status1, state='disabled')
        tk2state.set(select.factor1, state='disabled')
     }
    disable2 <- function(){
      tk2state.set(tlist.factor.selection, state='normal')
      tk2state.set(entry.factor, state='readonly')
      tk2state.set(tlist.timeto.selection, state='disabled')
      tk2state.set(tlist.status.selection, state='disabled')
      tk2state.set(entry4, state='disabled')
      tk2state.set(select.timeto1, state='disabled')
      tk2state.set(select.status1, state='disabled')
      tk2state.set(select.factor1, state='normal')
    }  
    disable3 <- function(){
      tk2state.set(tlist.factor.selection, state='disabled')
      tk2state.set(entry.factor, state='disabled')
      tk2state.set(tlist.timeto.selection, state='normal')
      tk2state.set(tlist.status.selection, state='normal')
      tk2state.set(entry4, state='readonly')
      tk2state.set(select.timeto1, state='normal')
      tk2state.set(select.status1, state='normal')
      tk2state.set(select.factor1, state='disabled')
    }
    type.var1 <- tkradiobutton(frame2)
    type.var2 <- tkradiobutton(frame2)
    type.var3 <- tkradiobutton(frame2)
    type.var.valuex <-  tclVar("none")
    tkconfigure(type.var1, variable = type.var.valuex, value = "none", command = disable1)
    tkconfigure(type.var2, variable = type.var.valuex, value = "factor", command = disable2)
    tkconfigure(type.var3, variable = type.var.valuex, value = "surv", command = disable3)
    tkgrid(type.var1, tklabel(frame2,text="None"), sticky="w")
    tkgrid(type.var2, tklabel(frame2,text="Factor"), sticky="w")
      select.factor <- function(){
           var.name <- as.numeric(tkcurselection(tlist.var))
           if(length(var.name)==0) return(tkmessageBox(message = "No factor selected", icon = "info", type="ok"))
           if(length(var.name)==1){
                if(matrix.info[var.name+1,]$type!="Categorical")  tkmessageBox(message = "Only for categorical variables", icon = "info", type="ok")
                if(matrix.info[var.name+1,]$type=="Categorical"){
                     tkdelete(tlist.factor.selection,0,"end")
                     tkinsert(tlist.factor.selection,"active",matrix.info[var.name+1,]$name)
                     tmp.data <- eval(parse(text=paste("subset(x,subset=",.global.subset.selection,")",sep="")))
                     tmp.data <- factor(tmp.data[,var.name+1])
                     if(length(levels(tmp.data))==2) tkconfigure(entry.factor,value= levels(tmp.data), textvariable=levels(tmp.data)[1])
                     if(length(levels(tmp.data))!=2) tkconfigure(entry.factor,value= "--", textvariable="--")
                     
                }
           }
           if(length(var.name)>1){
                 tkmessageBox(message = "Select only one categorical variable", icon = "info", type="ok")
           }
      }
    tlist.factor.selection <- tklistbox(frame2, height=1, width=18, selectmode="single",background= "white", exportselection=FALSE,state='normal')
    select.factor1 <- tkbutton(frame2,text="u",command = select.factor ,height=1, width=2, font="{Wingdings 3} {8}" )
    tkgrid(select.factor1,column=2, padx=5,pady=5,sticky="e")
    tkgrid(tlist.factor.selection,column=3,row=2,padx=5,pady=5,sticky="w")
    entry.factor <- ttkcombobox(frame2)
    tkconfigure(entry.factor,value=c(" "),state="readonly",text=" ")
    tkgrid(entry.factor,column=5,row=2,padx=5,pady=5,sticky="w", columnspan=4)
    tkgrid(tklabel(frame2,text="Reference"),column=5,row=1)
    tkgrid(tklabel(frame2,text="Variable"),column=3,row=1)
    tkgrid(type.var3, tklabel(frame2,text="Survival"), sticky="w")
    select.timeto <- function(){
           var.name <- as.numeric(tkcurselection(tlist.var))
           if(length(var.name)==0) return(tkmessageBox(message = "No 'time to' selected", icon = "info", type="ok"))
           if(length(var.name)==1){
                if(matrix.info[var.name+1,]$type=="Categorical")  tkmessageBox(message = "Only for numerical variables", icon = "info", type="ok")
                if(matrix.info[var.name+1,]$type!="Categorical"){
                     tkdelete(tlist.timeto.selection,0,"end")
                     tkinsert(tlist.timeto.selection,"active",matrix.info[var.name+1,]$name)
                }
           }
           if(length(var.name)>1){
                 tkmessageBox(message = "Select only one numerical variable", icon = "info", type="ok")
           }
      }
      
    select.status <- function(){
           var.name <- as.numeric(tkcurselection(tlist.var))
           if(length(var.name)==0) return(tkmessageBox(message = "No 'status' selected", icon = "info", type="ok"))
           if(length(var.name)==1){
                if(matrix.info[var.name+1,]$type!="Categorical")  tkmessageBox(message = "Only for categorical variables", icon = "info", type="ok")
                if(matrix.info[var.name+1,]$type=="Categorical"){
                     tkdelete(tlist.status.selection,0,"end")
                     tkinsert(tlist.status.selection,"active",matrix.info[var.name+1,]$name)
                    tmp.data <- eval(parse(text=paste("subset(x,subset=",.global.subset.selection,")",sep="")))
                    tmp.data <- factor(tmp.data[,var.name+1])
                    tkconfigure(entry4,value= levels(tmp.data), textvariable=levels(tmp.data)[1])
                }
           }
           if(length(var.name)>1){
                 tkmessageBox(message = "Select only one categorical variable", icon = "info", type="ok")
           }
    } 
    tlist.timeto.selection <- tklistbox(frame2, height=1, width=18, selectmode="single",background= "white", exportselection=FALSE,state='normal')
    select.timeto1 <- tkbutton(frame2,text="u",command = select.timeto ,height=1, width=2, font="{Wingdings 3} {8}" )
    tkgrid(select.timeto1,column=2,padx=5,pady=5,sticky="e")
    tkgrid(tlist.timeto.selection,column=3,row=4,padx=5,pady=5,sticky="w")
    tlist.status.selection <- tklistbox(frame2, height=1, width=18, selectmode="single",background= "white", exportselection=FALSE,state='normal')
    select.status1 <- tkbutton(frame2,text="u",command = select.status ,height=1, width=2, font="{Wingdings 3} {8}" )
    tkgrid(select.status1,column=4,row=4, padx=5,pady=5,sticky="e")
    tkgrid(tlist.status.selection,column=5,row=4,padx=5,pady=5,sticky="w")
    tkgrid(tklabel(frame2,text="--"),column=6,row=4, padx=5,pady=5,sticky="e")
    entry4 <- ttkcombobox(frame2)
    tkconfigure(entry4,value=c(" "),state="readonly",text=" ")
    tkgrid(entry4,column=7,row=4,padx=5,pady=5,sticky="w")
    tkgrid(tklabel(frame2,text="Time to event"),column=3,row=3)
    tkgrid(tklabel(frame2,text="Status"),column=5,row=3)
    tkgrid(tklabel(frame2,text="Event"),column=7,row=3)
    plots.factor <- function(){
          var.name <- as.character(tkget(tlist.factor.selection,0,"end"))
          if( length(var.name)==0) return(tkmessageBox(message = "No factor selected", icon = "info", type="ok"))
          tmp.data <- eval(parse(text=paste("subset(x,subset=",.global.subset.selection,")",sep="")))
          tmp.data <- factor(tmp.data[,var.name])
          coor <- barplot(table(tmp.data) ,main=paste("Plot of",var.name,sep=" "),xlab="", beside=FALSE)
          percent <- paste(round(prop.table(table(tmp.data))*100),"%",sep="")
          text(coor,mean(table(tmp.data))/2,percent)
      }   
    plots.factor.uni <- function(){
          var.name <- as.character(tkget(tlist.timeto.selection,0,"end"))
          if( length(var.name)==0) return(tkmessageBox(message = "No 'time to' selected", icon = "info", type="ok"))
          var.name2 <- as.character(tkget(tlist.status.selection,0,"end"))
          if( length(var.name2)==0) return(tkmessageBox(message = "No 'status' selected", icon = "info", type="ok"))
          event <- as.character(tkget(entry4))
          if( length(event)==0) return(tkmessageBox(message = "No 'event' selected", icon = "info", type="ok"))
          tmp.data <- eval(parse(text=paste("subset(x,subset=",.global.subset.selection,")",sep="")))
          timeto <- tmp.data[,var.name]
          eventto <- as.integer(tmp.data[, var.name2]==event)
          variableF <- Surv(timeto, eventto)
          label(variableF)<-label(tmp.data[,var.name2])
          KM.plot(x = variableF, file = NULL, var.label.x = label(x[, var.name2]))
    }  
    plot.uni.surv <- function(){
      if (tclvalue(type.var.valuex)=='none') return()
      if (tclvalue(type.var.valuex)=='factor')  plots.factor()
      if (tclvalue(type.var.valuex)=='surv')  plots.factor.uni()
    } 
    factor.plot <- tkbutton(frame2,text="Plot",command = plot.uni.surv ,height=1, width=10)
    tkgrid(factor.plot,row=2,column=8,padx=5,pady=5)
    tkgrid(frame2,column=3,columnspan=5, rowspan=5, row=0,padx=5,pady=5,sticky="nw")
    tk2state.set(tlist.factor.selection, state='disabled')
    tk2state.set(tlist.timeto.selection, state='disabled')
    tk2state.set(tlist.status.selection, state='disabled')
    tk2state.set(entry4, state='disabled')
    tk2state.set(entry.factor, state='disabled')
    tk2state.set(select.timeto1, state='disabled')
    tk2state.set(select.status1, state='disabled')
    tk2state.set(select.factor1, state='disabled')
    matrix.report <- matrix.info
      matrix.ini <- ""
      frame3 <- tkwidget(topframe, "labelframe", text ="Variable to report", fg="blue",padx=5,pady=5)
      var.scry  <- tkscrollbar(frame3, repeatinterval=5,command=function(...)tkyview(report.list ,...))
      var.scrx  <- tkscrollbar(frame3, orient="horizontal",command=function(...)tkxview(report.list ,...))
      report.list <- tk2mclistbox(frame3, width = 99, height=25,resizablecolumns = TRUE,selectmode="extended",
                                  yscrollcommand=function(...)tkset(var.scry,...),
                                  xscrollcommand=function(...)tkset(var.scrx,...))
      tk2column(report.list , "add", "var", label = "Name", width = 25)
      tk2column(report.list , "add", "type",label = "Type", width = 13)
      tk2column(report.list , "add", "dig", label = "Digits", width = 13)
      tk2column(report.list , "add", "hid", label = "Hide (Ref.)", width = 13)
      tk2column(report.list , "add", "sub", label = "Subset", width = 20)
      tk2column(report.list , "add", "lab", label = "Label", width = 100)
      tkgrid(report.list,var.scry)
      tkgrid(report.list,var.scrx)
      tkgrid.configure(var.scry,sticky="nse")
      tkgrid.configure(var.scrx,sticky="sew")
      tkgrid(frame3,column=3,columnspan=5, rowspan=25, row=6,padx=5,pady=5,sticky="ew")
    insert.var <- function(){
          var.name <- as.numeric(tkcurselection(tlist.var))
          if(length(var.name)==0) tkmessageBox(message = "No variable selected", icon = "info", type="ok")
          if(length(var.name)>0) tk2insert.multi(report.list , "end", matrix.info[var.name+1,c("name","type","digits","hide","subset.part","label")])
      }
    exclude.var <- function(){
          var.name <- as.numeric(tkcurselection(report.list))
          if(length(var.name)==0) tkmessageBox(message = "No variable selected", icon = "info", type="ok")
          if(length(var.name)>0){
                  tkselection.clear(tlist.var,0,"end")
                  sel1 <- cbind(as.character(tkget(report.list,0,"end")))
                  sel2 <- strsplit(sel1," ")
                  lvar <- as.character()
                  for(i in 1:nrow(sel1)){
                     lvar[i] <- sel2[[i]][1]
                  }
                  sel3 <- as.numeric(tkcurselection(report.list))+1
                  lvar <- lvar[sel3]
                  for (i in 1:length(var.name)){
                      lvar[i] <- as.numeric(rownames(matrix.info[matrix.info$name%in%lvar[i],]))
                  }
                  lvar <- as.numeric(lvar)
                  for(i in 1:length(var.name)){
                     tkselection.set(tlist.var,lvar[i]-1)
                     tkdelete(report.list , as.numeric(tkcurselection(report.list))[1])
                  }
          }
      }
      select.report.var <- tkbutton(topframe,text="u",command = insert.var,height=1, width=4, font="{Wingdings 3} {10}" )
      tkbind(report.list , select.report.var)
      tkgrid(select.report.var,column=2,row=11,padx=5,padx=10,sticky="s")
      exclude.report.var <- tkbutton(topframe,text="t",command =exclude.var,height=1, width=4, font="{Wingdings 3} {10}" )
      tkbind(report.list , select.report.var)
      tkgrid(exclude.report.var,column=2,row=12,padx=5,padx=10,sticky="n")
      frame4 <- tkwidget(topframe, "labelframe", text ="Global subset", fg="blue",padx=5,pady=5)
      subset.glob <- tclVar("")
      subset.glob.sel <- tclvalue(subset.glob)
      text.subset.glob <- tkentry(frame4, width= 40,textvariable = subset.glob)
      tkbind(text.subset.glob)
      assign(".global.subset.selection","", envir = .GlobalEnv)
      send.subset.glob <- function(){
          subset.select <- tclvalue(subset.glob)
          subset.dataframe <- try(eval(parse(text=paste("subset(x,subset=",subset.select,")",sep=""))), silent=TRUE)
          if(inherits(subset.dataframe, "try-error"))
                  return(tkmessageBox(message = "Syntax error. Try again", icon = "info", type = "ok"))
          if(nrow(subset.dataframe)==0)
                  return(tkmessageBox(message = paste("0 people will be analyzed. No applying subset"), icon = "info", type = "ok"))
          if(nrow(subset.dataframe)>0){
                   assign(".global.subset.selection", subset.select, envir = .GlobalEnv)
                   return(tkmessageBox(message = paste("Correct syntax. Subset applied to dataframe"), icon = "info", type = "ok"))
          }
      }
      tkgrid.configure(text.subset.glob)
      tkbind(send.subset.glob)
      subset.but.glob <-tkbutton(frame4,text="Apply subset",command=send.subset.glob, width=15)
      tkbind(subset.but.glob, "<Return>",send.subset.glob)
      tkgrid(frame4,column=3,columnspan=3, rowspan=2, row=40,padx=5,pady=5,sticky="ew")
      tkgrid(subset.but.glob, column=3,columnspan=3,row=0,rowspan=2)
      tk2tip(frame4, "Subset for dataframe")
      frame5 <- tkwidget(topframe, "labelframe", text ="'Variable' subset", fg="blue",padx=5,pady=5)
      subset.part <- tclVar("")
      subset.part.sel <- tclvalue(subset.part)
      text.subset.part <- tkentry(frame5, width= 40,textvariable = subset.part)
      tkbind(text.subset.part)
      send.subset.part <- function(){
          var.name <- (as.numeric(tkcurselection(report.list)))+1
          if( length(var.name)==0) return(tkmessageBox(message = "No variable selected", icon = "info", type="ok"))
          subset.select <- tclvalue(subset.part)
          subset.dataframe <- try(eval(parse(text=paste("subset(x,subset=",subset.select,")",sep=""))), silent=TRUE)
          if(inherits(subset.dataframe, "try-error"))
                  return(tkmessageBox(message = "Syntax error. Try again", icon = "info", type = "ok"))
          if(nrow(subset.dataframe)==0)
                  return(tkmessageBox(message = paste("0 people will be analyzed. No applying subset"), icon = "info", type = "ok"))
          if(nrow(subset.dataframe)>0){
                  new.mat <- read.list()
                  new.mat[var.name,5] <- subset.select
                  tkdelete(report.list,0,"end")
                  tk2insert.multi(report.list , "end", new.mat)
                  for(i in 1:length(var.name)){
                        tkselection.set(report.list,(var.name[i]-1))
                  }
                  return(tkmessageBox(message = paste("Correct syntax"), icon = "info", type = "ok"))
          }
      }
      tkgrid.configure(text.subset.part)
      tkbind(send.subset.part)
      subset.but.part <-tkbutton(frame5,text="Apply subset",command=send.subset.part, width=15)
      tkbind(subset.but.part, "<Return>",send.subset.part)
      tkgrid(frame5,column=3,columnspan=3, rowspan=2, row=45,padx=5,pady=5,sticky="ew")
      tkgrid(subset.but.part,column=3,columnspan=3,row=0,rowspan=2)
      tk2tip(frame5, "Subset for a selected report variable")
      frame6 <- tkwidget(topframe, "labelframe", text ="Method", fg="blue",padx=5,pady=5)
      type.var.function <- function(){
          var.name <- (as.numeric(tkcurselection(report.list)))+1
          if( length(var.name)==0) return(tkmessageBox(message = "No variable selected", icon = "info", type="ok"))
          sw.selection <- as.numeric(as.character(tclvalue(swtest)))
          new.mat <- read.list()
          type.var.val <- as.character(tclvalue(type.var.value))
          total <- as.integer(sum(as.integer(matrix.info[matrix.info$name%in%new.mat[var.name,1],2]%in%'Categorical'))==length(var.name))
          total2 <- sum(as.integer(matrix.info[matrix.info$name%in%new.mat[var.name,1],2]%in%'Categorical'))
          if(total==1){
                 if(type.var.val!="cat") return(tkmessageBox(message = "Only for  numerical variables", icon = "info", type="ok"))
          }
          if(total==0){
                 if (total2>=1)  tkmessageBox(message = "Applied changes in original non factor class", icon = "info", type="ok")
                 for (i in 1:length(var.name)){
                      if(matrix.info[matrix.info$name==new.mat[var.name[i],1],2]%nin%c('Categorical','Survival')){
                          if(type.var.val=="cat") new.mat[var.name[i],2] <- "Categorical"
                          if(type.var.val=="non.param") new.mat[var.name[i],2] <- "Non-Normal"
                          if(type.var.val=="yes.param") new.mat[var.name[i],2] <- "Normal"
                          if(type.var.val=="test.param"){
                             tmp <- ifelse(is.na(new.mat[var.name[i],5]),"",new.mat[var.name[i],5])
                             tmp.data <- eval(parse(text=paste("subset(x,subset=",.global.subset.selection,")",sep="")))
                             tmp.data <- eval(parse(text=paste("subset(tmp.data,subset=",tmp,")",sep="")))
                             if (length(unique(tmp.data[,new.mat[var.name[i],1]]))<6){
                                    tkmessageBox(message = paste("Less than 6 different values for '",new.mat[var.name[i],1],"'",". Not performs S-W test",sep=""), icon = "info", type = "ok")
                                   new.mat[var.name[i],2] <- "Categorical"
                                } else{
                                    if(length(tmp.data[,new.mat[var.name[i],1]])>5000) return(tkmessageBox(message = "Error in shapiro.test: sample size must be between 3 and 5000", icon = "info", type="ok"))
                                    SW <- shapiro.test(tmp.data[,new.mat[var.name[i],1]])$p.value
                                    if (SW<sw.selection)   new.mat[var.name[i],2] <- "Non-Normal"
                                    if (SW>=sw.selection)   new.mat[var.name[i],2] <- "Normal"
                                  }
                          }
                      }
                 }
          }
          tkdelete(report.list,0,"end")
          tk2insert.multi(report.list , "end", new.mat)
          for(i in 1:length(var.name)){
             if(matrix.info[matrix.info$name==new.mat[var.name[i],1],2]!='Categorical') tkselection.set(report.list,(var.name[i]-1))
          }
      }
      type.var1 <- tkradiobutton(frame6)
      type.var2 <- tkradiobutton(frame6)
      type.var3 <- tkradiobutton(frame6)
      type.var4 <- tkradiobutton(frame6)
      type.var.value <-  tclVar("test.param")
      tkconfigure(type.var1, variable = type.var.value, value = "cat", command = type.var.function)
      tkconfigure(type.var2, variable = type.var.value, value = "non.param", command = type.var.function)
      tkconfigure(type.var3, variable = type.var.value, value = "yes.param", command = type.var.function)
      tkconfigure(type.var4, variable = type.var.value, value = "test.param", command = type.var.function)
      tkgrid(tklabel(frame6,text="Categorical"),type.var1)
      tkgrid(tklabel(frame6,text="Non Normal"),type.var2)
      tkgrid(tklabel(frame6,text="Normal"),type.var3)
      tkgrid(tklabel(frame6,text="Test (S-W)"),type.var4)
      swtest <- tclVar("0.05")
      tspin <- tk2spinbox(frame6, from = 0, to = 1, increment = 0.01,state="readonly", width=4,readonlybackground="white",textvariable=swtest)
      tkgrid(tspin,row=3,column=2)
      tk2tip(tspin, "Significance level for Shapiro-Wilks test")
      tk2tip(frame6, "Select method for analyze each variable")
      tkgrid(frame6,column=15, rowspan=5, row=5,padx=5,pady=5,sticky="nw")
      frame8 <- tkwidget(topframe, "labelframe", text ="Plots", fg="blue",padx=5,pady=5)
      plots.uni <- function(){
          var.name <- (as.numeric(tkcurselection(report.list)))+1
         	if( length(var.name)==0) return(tkmessageBox(message = "No variable selected", icon = "info", type="ok"))
         	if(length(var.name)>1)  return(tkmessageBox(message = "Select only one variable", icon = "info", type="ok"))
          mat <- read.list()
          variable <- mat[var.name,]
          tmp <- ifelse(is.na(variable[5]),"",variable[5])[[1]]
          tmp.data <- eval(parse(text=paste("subset(x,subset=",.global.subset.selection,")",sep="")))
          tmp.data <- eval(parse(text=paste("subset(tmp.data,subset=",tmp,")",sep="")))
          if(variable[,2]=='Categorical'){
            tmp.data <- factor(tmp.data[,as.character(variable[1])])
            coor <- barplot(table(tmp.data) ,main=paste("Plot of",as.character(variable[1]),sep=" "),xlab="", beside=FALSE)
            percent <- paste(round(prop.table(table(tmp.data))*100),"%",sep="")
            text(coor,mean(table(tmp.data))/2,percent)
          }
          if(variable[,2]%in%c('Normal', 'Non-Normal')){
            tmp.data <- tmp.data[,as.character(variable[1])]
            norm.plot(x=tmp.data, file=NULL,var.label.x = as.character(variable[1]), z=1.5,n.breaks="Sturges")
          }
          if(variable[,2]=='Survival'){
            tmp.data <- tmp.data[,as.character(variable[1])]
            KM.plot(x = tmp.data, file = NULL, var.label.x = as.character(variable[1]))
          }
      }
      plots.bi <- function(){
          if (tclvalue(type.var.valuex)=='factor'){
            var.name <- (as.numeric(tkcurselection(report.list)))+1
            var.plot <- as.character(tkget(tlist.factor.selection,0,"end"))
           	if( length(var.name)==0) return(tkmessageBox(message = "No variable selected", icon = "info", type="ok"))
           	if(length(var.name)>1)  return(tkmessageBox(message = "Select only one variable", icon = "info", type="ok"))
         	  if( length(var.plot)==0) return(tkmessageBox(message = "No factor selected", icon = "info", type="ok"))
            mat <- read.list()
            variable <- mat[var.name,]
            tmp <- ifelse(is.na(variable[5]),"",variable[5])[[1]]
            tmp.data <- eval(parse(text=paste("subset(x,subset=",.global.subset.selection,")",sep="")))
            tmp.data <- eval(parse(text=paste("subset(tmp.data,subset=",tmp,")",sep="")))
            if(variable[,2]=='Categorical'){
              at <- table(factor(tmp.data[,as.character(variable[1])]), factor(tmp.data[,var.plot]))
              barplot(at, beside=TRUE, main = paste("Barplot of",as.character(variable[1]),"by",var.plot, sep=" "),
              legend.text=TRUE,col=rainbow(nrow(at)),args.legend = list(bg = "white"))
            }
            if(variable[,2]%nin%c('Categorical','Survival')){
              boxplot(tmp.data[,as.character(variable[1])] ~ factor(tmp.data[,var.plot]),
              main = paste("Boxplot of",as.character(variable[1]),"by",var.plot, sep=" "), ylab=as.character(variable[1]), xlab=var.plot,
              col="bisque")
            }
          }
          if(tclvalue(type.var.valuex)=='surv'){
            var.name <- as.character(tkget(tlist.timeto.selection,0,"end"))
            if( length(var.name)==0) return(tkmessageBox(message = "No 'time to' selected", icon = "info", type="ok"))
            var.name2 <- as.character(tkget(tlist.status.selection,0,"end"))
            if( length(var.name2)==0) return(tkmessageBox(message = "No 'status' selected", icon = "info", type="ok"))
            event <- as.character(tkget(entry4))
            if( length(event)==0) return(tkmessageBox(message = "No 'event' selected", icon = "info", type="ok"))
          
            var.report <- (as.numeric(tkcurselection(report.list)))+1
            if( length(var.report)==0) return(tkmessageBox(message = "No variable selected", icon = "info", type="ok"))
           	if(length(var.report)>1)  return(tkmessageBox(message = "Select only one variable", icon = "info", type="ok"))
            mat <- read.list()
            variable <- mat[var.report,]
            tmp <- ifelse(is.na(variable[5]),"",variable[5])[[1]]
            tmp.data <- eval(parse(text=paste("subset(x,subset=",.global.subset.selection,")",sep="")))
            tmp.data <- eval(parse(text=paste("subset(tmp.data,subset=",tmp,")",sep="")))
            timeto <- tmp.data[,var.name]
            eventto <- as.integer(tmp.data[, var.name2]==event)
            variableF <- Surv(timeto, eventto)
            label(variableF)<-label(tmp.data[,var.name2])
            if(variable[,2]=='Categorical'){ 
              KMg.plot(x = tmp.data[,as.character(variable[1])], y= variableF, file=NULL, var.label.x=label(x[,as.character(variable[1])]), var.label.y = label(x[,var.name2]))
            }
            if(variable[,2]%in%c('Normal','Non-Normal')){
                Cox.plot(x = tmp.data[,as.character(variable[1])], y= variableF, file=NULL, var.label.x=label(x[,as.character(variable[1])]), var.label.y = label(x[,var.name2]))
            
            }
         }
      }
      uni.plot <- tkbutton(frame8,text="Univariate",command = plots.uni,height=1, width=20)
      bi.plot <- tkbutton(frame8,text="Bivariate",command = plots.bi, height=1, width=20)
      tkbind(uni.plot, plots.uni)
      tkbind(bi.plot, plots.bi)
      tkgrid(uni.plot)
      tkgrid(bi.plot)
      tkgrid(frame8,column=15, rowspan=5, row=10,padx=5,pady=5,sticky="new")
      tk2tip(frame8, "Plot the selected variable/s") 
      frame7 <- tkwidget(topframe, "labelframe", text ="Decimals digits", fg="blue",padx=5,pady=5)
      decimals.function <- function(){
          var.name <- (as.numeric(tkcurselection(report.list)))+1
         	if( length(var.name)==0) return(tkmessageBox(message = "No variable selected", icon = "info", type="ok"))
          dec.select <- tclvalue(dec.value)
          new.mat <- read.list()
          if(dec.select=='defdec') new.mat[var.name,3] <- "Default"
          if(dec.select=='1dec') new.mat[var.name,3] <- "1"
          if(dec.select=='2dec') new.mat[var.name,3] <- "2"
          if(dec.select=='3dec') new.mat[var.name,3] <- "3"
          tkdelete(report.list,0,"end")
          tk2insert.multi(report.list , "end", new.mat)
          for(i in 1:length(var.name)){
              tkselection.set(report.list,(var.name[i]-1))
          }
      }
      dec1 <- tkradiobutton(frame7)
      dec2 <- tkradiobutton(frame7)
      dec3 <- tkradiobutton(frame7)
      dec4 <- tkradiobutton(frame7)
      dec.value <- tclVar("defdec")
      tkconfigure(dec1, variable = dec.value, value = "1dec", command = decimals.function)
      tkconfigure(dec2, variable = dec.value, value = "2dec", command = decimals.function)
      tkconfigure(dec3, variable = dec.value, value = "3dec", command = decimals.function)
      tkconfigure(dec4, variable = dec.value, value = "defdec", command = decimals.function)
      tkgrid(tklabel(frame7,text="1 dec"),dec1)
      tkgrid(tklabel(frame7,text="2 dec"),dec2)
      tkgrid(tklabel(frame7,text="3 dec"),dec3)
      tkgrid(tklabel(frame7,text="Default dec"),dec4)
      tk2tip(frame7, "Number of decimals in report")
      tkgrid(frame7,column=15,row=15,padx=5,pady=5,sticky="s")
      frame9 <- tkwidget(topframe, "labelframe", text ="Hide (Ref.) category", fg="blue",padx=5,pady=5)
      hide.function <- function(){
              var.name <- (as.numeric(tkcurselection(report.list)))+1
             	if( length(var.name)==0) return(tkmessageBox(message = "No variable selected", icon = "info", type="ok"))
             	hide.select <- tclvalue(hide.value)
              new.mat <- read.list()
              res <- sum(as.integer(new.mat[var.name,"type"]%in%'Categorical'))
              if (res==0) return(tkmessageBox(message = "No 'Categorical' variable selected", icon = "info", type="ok"))
              if (length(var.name)>res) tkmessageBox(message = "Applied changes in 'Categorical' variables", icon = "info", type="ok")
              var.name <- var.name[new.mat[var.name,"type"]%in%'Categorical']
              if(hide.select=='1cat') new.mat[var.name,"hide"] <- "First"
              if(hide.select=='2cat') new.mat[var.name,"hide"] <- "Last"
              if(hide.select=='nocat') new.mat[var.name,"hide"] <- "No"
              tkdelete(report.list,0,"end")
              tk2insert.multi(report.list , "end", new.mat)
              for(i in 1:length(var.name)){
                  tkselection.set(report.list,(var.name[i]-1))
              }
      }
      hide1 <- tkradiobutton(frame9)
      hide2 <- tkradiobutton(frame9)
      hide3 <- tkradiobutton(frame9)
      hide.value <- tclVar("nocat")
      tkconfigure(hide1, variable = hide.value, value = "1cat", command = hide.function)
      tkconfigure(hide2, variable = hide.value, value = "2cat", command = hide.function)
      tkconfigure(hide3, variable = hide.value, value = "nocat", command = hide.function)
      tkgrid(tklabel(frame9,text="First"),hide1)
      tkgrid(tklabel(frame9,text="Last"),hide2)
      tkgrid(tklabel(frame9,text="No category"),hide3)
      tk2tip(frame9, "Hide a category in categorical variable")
      tkgrid(frame9,column=15,row=20,padx=5,pady=5,sticky="s")
      spssLoad <- function(){
              name <- tclvalue(tkgetOpenFile(parent = tt, title = "SPSS Data",filetypes = "{{SPSS Files} {.sav}}"))
              if (name=="") return(" ")
              if (length(name)>1)  stop(paste("More than one object in",name))
              setwd(dirname(name))
              load.data <- try(spss.get(file = name))
              if (inherits(load.data,"try-error")){
                  return("Problems loading .sav")
              } else{
                  datatemp.name <- sub(".sav$", "", basename(name), ignore.case = TRUE)
                   datatemp.name <- gsub(" ", ".",datatemp.name)
                   assign(datatemp.name, load.data, envir = .GlobalEnv)
                   eval(parse(text = paste("cGroupsGUI(", datatemp.name,")", sep = "")))
                    tkdestroy(tt)
                }
      }
      RWspace <- function(){
        all.ls <- ls(envir =.GlobalEnv)
        n <- length(all.ls)
        j <- 1
        num <- NULL
        for (i in 1:n){
            if (inherits(get(all.ls[i]),"data.frame")){
                num[j] <- i
                j <- j+1
            }
        }
        if (is.null(num)) return(tkmessageBox(message = "No dataframes in workspace", icon = "info", type="ok"))
        names.data <- all.ls[num]
        special <- tktoplevel(parent=topframe)
        tktitle(special) <- "Dataframes in Workspace"
        pos <- as.character(tkwm.geometry(tt))
        pos <- paste("+",paste(strsplit(pos,"+", fixed=TRUE)[[1]][2:3],collapse="+"),sep="")
        pos <- paste("259x177", pos, sep="")
        tkwm.resizable(special,0,0)
        tkwm.geometry(special,pos)
        tkgrab(special)
        topspecial <- tkframe(special)
        tkfocus(special)
        tkgrid(topspecial, column=0, row=0, sticky="nwes", ipadx=3,ipady=3,padx=40,pady=15)
        frame0 <- tkwidget(topspecial, "labelframe", text ="", fg="blue",padx=10,pady=10)
        var.scr0  <- tkscrollbar(frame0, repeatinterval=5,command=function(...)tkyview(tlist.var0,...))
        tlist.var0 <- tklistbox(frame0,height=5,selectmode="single", yscrollcommand=function(...)tkset(var.scr0,...),
                    background="white",exportselection=FALSE, height =6, width =20)
        for (i in 1:length(num)){
             tkinsert(tlist.var0,"end",names.data[i])
        }
        tkbind(tlist.var0)
        tkgrid(tlist.var0,var.scr0)
        tkgrid.configure(var.scr0,sticky="nsw")
        tkgrid(frame0, padx=5,pady=5)
        select.rdata <- function(){
             var.name <- (as.numeric(tkcurselection(tlist.var0)))+1
            	if( length(var.name)==0){
                  tkmessageBox(message = "No dataframe selected", icon = "info", type="ok")
                  return(tkdestroy(special))
              }
              eval(parse(text = paste("cGroupsGUI(", names.data[var.name],")", sep = "")))
              tkdestroy(tt)
        }
      cancel.rdata <- function(){
          tkdestroy(special)
      }
      selection <- tkbutton(frame0,text="Load",command = select.rdata, height=1, width=8,fg='blue' )
      tkgrid(selection,sticky="se",row=2)
      cancel <- tkbutton(frame0,text="Cancel",command = cancel.rdata, height=1, width=8,fg='red' )
      tkgrid(cancel,sticky="sw",row=2)
      }
      RData <- function(){
       name <- tclvalue(tkgetOpenFile(parent = tt, title = "R Data",
                        filetypes = "{{Rdata Files} {.rdata .rda .Rdata .RData}}"))
              if (name=="") return(" ")
              if (length(name)>1)  stop(paste("More than one object in",name))
              setwd(dirname(name))
              load.data <- try(load(file = name,envir = .GlobalEnv))
              if (inherits(load.data,"try-error")){
                  return("Problems loading .Rdata")
              } else{
                   tkmessageBox(message = paste(basename(name),"loaded in workspace"), icon = "info", type="ok")
                  }
      }
      plaincomma <- function(){
              name <- tclvalue(tkgetOpenFile(parent = tt, title = "Plain Data",filetypes = "{{Plain Files} {.csv}}"))
              if (name=="") return(" ")
              if (length(name)>1)  stop(paste("More than one object in",name))
              setwd(dirname(name))
              datatemp.name <- sub(".csv$", "", basename(name), ignore.case = TRUE)
              datatemp.name <- gsub(" ", ".",datatemp.name)
              load.data <- try(read.csv(file = name))
              if (inherits(load.data,"try-error")){
                  return("Problems loading .csv")
              } else{
                   tkdestroy(tt)
                   assign(datatemp.name, load.data, envir = .GlobalEnv)
                      eval(parse(text = paste("cGroupsGUI(", datatemp.name,")", sep = "")))
              }
      }
     plaincolon   <- function(){
              name <- tclvalue(tkgetOpenFile(parent = tt, title = "Plain Data",filetypes = "{{Plain Files} {.csv}}"))
              if (name=="") return(" ")
              if (length(name)>1)  stop(paste("More than one object in",name))
              setwd(dirname(name))
              datatemp.name <- sub(".csv$", "", basename(name), ignore.case = TRUE)
              datatemp.name <- gsub(" ", ".",datatemp.name)
              load.data <- try(read.csv2(file = name))
              if (inherits(load.data,"try-error")){
                  return("Problems loading .csv")
              } else{
                   tkdestroy(tt)
                   assign(datatemp.name, load.data, envir = .GlobalEnv)
                      eval(parse(text = paste("cGroupsGUI(", datatemp.name,")", sep = "")))
              }
      }
     report.options <- function(){
             var.plot <- as.character(tkget(tlist.factor.selection,0,"end"))
             tmp.data <- eval(parse(text=paste("subset(x,subset=",.global.subset.selection,")",sep="")))
             aux <- as.integer(length(var.plot)==1)
             if (aux!=0) nx <- length(levels(factor(tmp.data[,var.plot])))
             if (aux==0) nx <- 0
             stat <- "disabled"
             stat.mult <- "disabled"
             if (!exists(".p.overall", envir = .GlobalEnv))  assign(".p.overall","0", envir = .GlobalEnv)
             if (!exists(".p.trend", envir = .GlobalEnv))  assign(".p.trend","0", envir = .GlobalEnv)
             if (!exists(".p.mult", envir = .GlobalEnv))   assign(".p.mult","0", envir = .GlobalEnv)
             if (exists(".p.overall", envir = .GlobalEnv) & aux==0)  assign(".p.overall","0", envir = .GlobalEnv)
             if (exists(".p.trend", envir = .GlobalEnv) & aux==0)  assign(".p.trend","0", envir = .GlobalEnv)
             if (exists(".p.mult", envir = .GlobalEnv) & aux==0)  assign(".p.mult","0", envir = .GlobalEnv)
             if (!exists(".show.all", envir = .GlobalEnv))  assign(".show.all","1", envir = .GlobalEnv)
             if (!exists(".show.desc", envir = .GlobalEnv))  assign(".show.desc","1", envir = .GlobalEnv)
             if (!exists(".show.haz", envir = .GlobalEnv))  assign(".show.haz","0", envir = .GlobalEnv)
             if (!exists(".show.n", envir = .GlobalEnv))   assign(".show.n","1", envir = .GlobalEnv)
             if (!exists(".type.cat.value", envir = .GlobalEnv))  assign(".type.cat.value","nperc", envir = .GlobalEnv)
             if (aux==1 & nx>=3 & tclvalue(type.var.valuex)=='factor'){
                  stat.mult <- "normal"
                  stat <- "normal"
                  if (exists(".p.trend", envir = .GlobalEnv))    assign(".p.trend" ,.p.trend , envir = .GlobalEnv)
                  if (exists(".p.mult", envir = .GlobalEnv))   assign( ".p.mult" , .p.mult, envir = .GlobalEnv)
             }
              if (aux==1 & nx==2 & tclvalue(type.var.valuex)=='factor'){
                  stat <- "normal"
                  if (exists(".p.overall", envir = .GlobalEnv))    assign(".p.overall" ,.p.overall , envir = .GlobalEnv)
             }
              if (tclvalue(type.var.valuex)=='surv'){
                  stat <- "normal"
                  if (exists(".p.overall", envir = .GlobalEnv))    assign(".p.overall" ,.p.overall , envir = .GlobalEnv)
             }
             if (exists(".show.all", envir = .GlobalEnv)) assign( ".show.all" , .show.all, envir = .GlobalEnv)
             if (exists(".show.desc", envir = .GlobalEnv)) assign( ".show.desc" , .show.desc, envir = .GlobalEnv)
             if (exists(".show.haz", envir = .GlobalEnv)) assign( ".show.haz" , .show.haz, envir = .GlobalEnv)
             if (exists(".show.n", envir = .GlobalEnv))   assign( ".show.n"  , .show.n , envir = .GlobalEnv)
             if (exists(".type.cat.value", envir = .GlobalEnv))  assign( ".type.cat.value"  , .type.cat.value, envir = .GlobalEnv)
             pos <- as.character(tkwm.geometry(tt))
             pos <- paste("+",paste(strsplit(pos,"+", fixed=TRUE)[[1]][2:3],collapse="+"),sep="")
             pos <- paste("220x322", pos, sep="")
             report <- tktoplevel(parent=topframe)
             tktitle(report) <- "Options for report"
             tkwm.resizable(report,0,0)
             tkwm.geometry(report,pos)
             tkgrab(report)
             tkfocus(report)
             topreport <- tkframe(report)
             tkgrid(topreport,columnspan=8)
             topreportframe <- tkwidget(topreport, "labelframe", text ="", fg="blue",padx=10,pady=10)
             results.show <- tkcheckbutton(topreportframe)
             results.show2 <- tkcheckbutton(topreportframe)
             results.show3 <- tkcheckbutton(topreportframe)
             results.value <- tclVar(.p.trend)
             results.value2 <- tclVar(.p.mult)
             results.value3 <- tclVar(.p.overall)
             tkconfigure(results.show,variable=results.value, state= stat.mult)
             tkconfigure(results.show2,variable=results.value2, state= stat.mult)
             tkconfigure(results.show3,variable=results.value3, state=stat)
             tkgrid(tklabel(topreportframe,text="P-values in report",fg="blue"))
             tkgrid(tklabel(topreportframe,text="P overall"),results.show3)
             tkgrid(tklabel(topreportframe,text="P trend"),results.show)
             tkgrid(tklabel(topreportframe,text="Multiple comparisons"),results.show2)
             results.n <- tkcheckbutton(topreportframe)
             results.n.value <- tclVar(.show.n)
             tkconfigure(results.n,variable=results.n.value)
             tkgrid(tklabel(topreportframe,text="Show N in each variable",fg="blue"),results.n, sticky="nw")
             results.all <- tkcheckbutton(topreportframe)
             results.value.all <- tclVar(.show.all)
             tkconfigure(results.all ,variable=results.value.all)
             tkgrid(tklabel(topreportframe,text="Show 'ALL' column",fg="blue"),results.all, sticky="nw")
             results.desc <- tkcheckbutton(topreportframe)
             results.value.desc <- tclVar(.show.desc)
             tkconfigure(results.desc ,variable=results.value.desc)
             tkgrid(tklabel(topreportframe,text="Show descriptives",fg="blue"),results.desc, sticky="nw")
             results.haz <- tkcheckbutton(topreportframe)
             results.value.haz <- tclVar(.show.haz)
             tkconfigure(results.haz ,variable=results.value.haz)
             tkgrid(tklabel(topreportframe,text="Show odds/hazard ratio",fg="blue"),results.haz, sticky="nw")
             type.cat1 <- tkradiobutton(topreportframe)
             type.cat2 <- tkradiobutton(topreportframe)
             type.cat3 <- tkradiobutton(topreportframe)
             type.cat.valuex <-  tclVar(.type.cat.value)
             tkconfigure(type.cat1, variable = type.cat.valuex, value = "nperc", command = I)
             tkconfigure(type.cat2, variable = type.cat.valuex, value = "perc", command = I)
             tkconfigure(type.cat3, variable = type.cat.valuex, value = "nsample", command = I)
             tkgrid(tklabel(topreportframe,text="Type for categorical",fg="blue"), sticky="nw")
             tkgrid(tklabel(topreportframe,text="n (%)"),type.cat1)
             tkgrid(tklabel(topreportframe,text="(%)"),type.cat2)
             tkgrid(tklabel(topreportframe,text="n"),type.cat3)
             tkgrid(tklabel(topreportframe,text="",fg="blue"),results.all, sticky="nw")
                 accept.fun <- function(){
                  assign(".p.overall" ,tclvalue(results.value3) , envir = .GlobalEnv)
                  assign(".p.trend" ,tclvalue(results.value) , envir = .GlobalEnv)
                  assign(".p.mult" ,tclvalue(results.value2) , envir = .GlobalEnv)
                  assign(".show.all" ,tclvalue(results.value.all) , envir = .GlobalEnv)
                  assign(".show.desc" ,tclvalue(results.value.desc) , envir = .GlobalEnv)
                  assign(".show.haz" ,tclvalue(results.value.haz) , envir = .GlobalEnv)
                  assign(".show.n"  ,tclvalue(results.n.value) , envir = .GlobalEnv)
                  assign(".type.cat.value"  ,tclvalue(type.cat.valuex) , envir = .GlobalEnv)
                 tkdestroy(report)
            }
            cancel.fun <- function(){
                tkdestroy(report)
            }
            accept <- tkbutton(topreportframe,text="Accept",command = accept.fun, height=1, width=8,fg='blue')
            cancel <- tkbutton(topreportframe,text="Cancel",command = cancel.fun , height=1, width=8,fg='red')
            tkgrid(cancel,row=12,column=0,sticky="w")
            tkgrid(accept,row=12,column=0,sticky="e")
            tkgrid(topreportframe, padx=25,pady=25, columnspan=8)
      }
      prepare.input <- function(){
          tmp.data <- x
          if(tclvalue(type.var.valuex)=='factor'){  
              var.plot <- as.character(tkget(tlist.factor.selection,0,"end"))
              label.aux <- label(tmp.data[,var.plot])
              variableF <- factor(tmp.data[,var.plot])
              if (label.aux=="")   label(variableF) <- var.plot
              if (label.aux!="")   label(variableF) <- label.aux
          }
          if(tclvalue(type.var.valuex)=='none'){  
              variableF <- NULL
              ref.y <- 1
          }
          if(tclvalue(type.var.valuex)=='surv'){
              var.name <- as.character(tkget(tlist.timeto.selection,0,"end"))
              if( length(var.name)==0) return(tkmessageBox(message = "No 'time to' selected", icon = "info", type="ok"))
              var.name2 <- as.character(tkget(tlist.status.selection,0,"end"))
              if( length(var.name2)==0) return(tkmessageBox(message = "No 'status' selected", icon = "info", type="ok"))
              event <- as.character(tkget(entry4))
              if( length(event)==0) return(tkmessageBox(message = "No 'event' selected", icon = "info", type="ok"))
              timeto <- tmp.data[,var.name]
              eventto <- as.integer(tmp.data[, var.name2]==event)
              variableF <- Surv(timeto, eventto)
              label(variableF)<-label(tmp.data[,var.name2])
              ref.y <- 1
          } 
    
            if (length(as.character(tkget(report.list,0,"end")))==0) return(tkmessageBox(message = "Select at least one variables to report", icon = "info", type="ok"))
            mat <- read.list()
            variables <- tmp.data[,mat$names, drop=FALSE]
            method <- mat$type
            subs <- mat$subs
            hide <- mat$hide
            decimals <- mat$digits
            subs.aux <- NULL
            for (i in 1:nrow(mat)){
                if(method[i]=='Categorical') method[i] <- 3
                if(method[i]=='Non-Normal') method[i] <- 2
                if(method[i]=='Normal') method[i] <- 1
                if(subs[i]=="") subs[i] <- NA
                if(hide[i]=="First") hide[i] <- 1
                if(hide[i]=="Last") hide[i] <- Inf
                if(hide[i]=="No") hide[i] <- NA
                if(decimals[i]=="Default") decimals[i] <- NA
                if(.global.subset.selection==""){
                      if(is.na(subs[i]))  subs.aux[i] <- ""
                      if(!is.na(subs[i])) subs.aux[i] <- paste(mat$names[i],"=",subs[i],sep="")
               }
               if(.global.subset.selection!=""){
                     if(is.na(subs[i]))  subs.aux[i] <- paste(mat$names[i],"=",.global.subset.selection,sep="")
                     if(!is.na(subs[i])) subs.aux[i] <- paste(mat$names[i],"=",.global.subset.selection," & ",subs[i],sep="")
                }
            }
              subs.aux[subs.aux==""] <- NA
              subs <- subs.aux
              if(all(is.na(subs))) subs <- NA
              if(any(!is.na(subs))){ 
                  subs <- subs[!is.na(subs)]
                  subs <- paste("list(",paste(subs, collapse=","),")",sep="")
              }
            method <- as.numeric(method)
            hide <- as.numeric(hide)
            decimals <- as.numeric(decimals)
            if (!exists(".p.trend", envir = .GlobalEnv)) .p.trend <- 0
            if (!exists(".p.mult", envir = .GlobalEnv)) .p.mult <- 0
            if (!exists(".show.all", envir = .GlobalEnv)) .show.all <- 1
            if (!exists(".show.n", envir = .GlobalEnv)) .show.n <- 1
            if (!exists(".p.overall", envir = .GlobalEnv)) .p.overall <- 1
            if (!exists(".show.haz", envir = .GlobalEnv)) .show.haz <- 0
            if (!exists(".show.desc", envir = .GlobalEnv)) .show.desc <- 1
            if(tclvalue(type.var.valuex)=='factor'){
              ref.y <- as.character(tkget(entry.factor))
              if(length(ref.y)==0) ref.y <- 1
              if(ref.y=='--') ref.y <- 1
              if(ref.y!=1){
                     tmp.data <- eval(parse(text=paste("subset(x,subset=",.global.subset.selection,")",sep="")))
                      pos <- levels(factor(tmp.data[,var.plot]))==ref.y
                     if(pos[1]==TRUE) ref.y <- 1
                     if(pos[2]==TRUE) ref.y <- 2
              }
           } 
           nlev <- as.vector(unlist(lapply(variables,function(x) nlevels(x))))
           ref <- hide
           ref[is.na(ref)] <- 1
           ref[is.infinite(ref)] <- nlev[is.infinite(ref)] 
           p.trend <- as.numeric(.p.trend)
            p.mult <- as.numeric(.p.mult)
            show.all <- as.numeric(.show.all)
            show.n <- as.numeric(.show.n)
            if(.p.overall=='1') show.p.overall <- TRUE
            if(.p.overall=='0') show.p.overall <- FALSE
            if(.show.haz=='1') show.ratio <- TRUE
            if(.show.haz=='0') show.ratio <- FALSE
            if(.show.desc=='1') show.descr <- TRUE
            if(.show.desc=='0') show.descr <- FALSE       
            if(.type.cat.value=="nsample") typecat <- 3
            if(.type.cat.value=="nperc") typecat <- 2
            if(.type.cat.value=="perc") typecat <- 1
            alpha <- as.numeric(as.character(tclvalue(swtest)))
            res <- list(X = variables, y = variableF, Xext = x, selec = subs, method = method, alpha = alpha, hide = hide, digits = decimals, type = typecat, show.all = show.all, show.p.trend = p.trend, show.p.mul = p.mult, show.n=show.n, show.ratio=show.ratio, show.descr = show.descr, show.p.overall= show.p.overall, ref.y = ref.y, ref=ref)
      }
      print.aux <- function(){
            res <- prepare.input()
            if(!inherits(res,"list")) return()
            ans <- try(eval(parse(text= paste("compareGroups(X = res$X, y = res$y, Xext=res$Xext, selec =", res$selec,", method = res$method, alpha = res$alpha,min.dis = 5, max.ylev = 5, ref.y = res$ref.y, ref= res$ref)"))))
            if(inherits(ans,"try-error")) return(tkmessageBox(message = "Error occurred in 'compareGroups' function", icon = "info", type="ok"))
            print(ans)
            if(!inherits(ans,"try-error")) ans <- try(createTable(x=ans,hide=res$hide, digits=res$digits,type=res$type, show.all= res$show.all, show.p.trend= res$show.p.trend,show.p.mul= res$show.p.mul,show.n=res$show.n, show.p.overall = res$show.p.overall, show.ratio = res$show.ratio, show.descr = res$show.descr))
            if(inherits(ans,"try-error")) return(tkmessageBox(message = "Error occurred in 'createTable' function", icon = "info", type="ok"))
            print(ans, , which.table = 'both')
      }
      export2latex.aux <- function(){
            res <- prepare.input()
            if(!inherits(res,"list")) return()
            ans <- try(eval(parse(text= paste("compareGroups(X = res$X, y = res$y, Xext=res$Xext, selec =", res$selec,", method = res$method, alpha = res$alpha,min.dis = 5, max.ylev = 5, ref.y = res$ref.y, ref= res$ref)"))))
            if(inherits(ans,"try-error")) return(tkmessageBox(message = "Error occurred in 'compareGroups' function", icon = "info", type="ok"))
            if(!inherits(ans,"try-error")) ans <- try(createTable(x=ans,hide=res$hide, digits=res$digits,type=res$type, show.all= res$show.all, show.p.trend= res$show.p.trend,show.p.mul= res$show.p.mul,show.n=res$show.n, show.p.overall = res$show.p.overall, show.ratio = res$show.ratio, show.descr = res$show.descr))
            if(inherits(ans,"try-error")) return(tkmessageBox(message = "Error occurred in 'createTable' function", icon = "info", type="ok"))
            GetDir <- function(){
                   directory <- tclvalue(tkgetSaveFile(parent = tt,initialfile="CompareGroupsTable",title = "Select where to save table"))
                   directory <- as.character(directory)
                   if (directory=="") return()
                   return(directory)
             }
            directory <- GetDir()
            if(is.null(directory)) return()
            setwd(dirname(directory))
            ans <- try(export2latex(ans, file = directory, which.table = 'both'))
            if(inherits(ans,"try-error")) return(tkmessageBox(message = "Error occurred in 'export2latex' function", icon = "info", type="ok"))
            if(!inherits(ans,"try-error")) return(tkmessageBox(message = "Correct 'export2latex'", icon = "info", type="ok"))
      }
     export2csv.aux1 <- function(){
        res <- prepare.input()
        if(!inherits(res,"list")) return()
        ans <- try(eval(parse(text= paste("compareGroups(X = res$X, y = res$y, Xext=res$Xext, selec =", res$selec,", method = res$method, alpha = res$alpha,min.dis = 5, max.ylev = 5, ref.y = res$ref.y, ref= res$ref)"))))
        if(inherits(ans,"try-error")) return(tkmessageBox(message = "Error occurred in 'compareGroups' function", icon = "info", type="ok"))
            if(!inherits(ans,"try-error")) ans <- try(createTable(x=ans,hide=res$hide, digits=res$digits,type=res$type, show.all= res$show.all, show.p.trend= res$show.p.trend,show.p.mul= res$show.p.mul,show.n=res$show.n, show.p.overall = res$show.p.overall, show.ratio = res$show.ratio, show.descr = res$show.descr))
        if(inherits(ans,"try-error")) return(tkmessageBox(message = "Error occurred in 'createTable' function", icon = "info", type="ok"))
        GetDir <- function(){
               directory <- tclvalue(tkgetSaveFile(parent = tt,initialfile="CompareGroupsTable",title = "Select where to save table"))
               directory <- as.character(directory)
               if (directory=="") return()
               return(directory)
        }
        directory <- GetDir()
        if(is.null(directory)) return()
        setwd(dirname(directory))
        ans <- try(export2csv(ans, file = directory, sep=",", which.table="both"))
        if(inherits(ans,"try-error")) return(tkmessageBox(message = "Error occurred in 'export2csv' function", icon = "info", type="ok"))
        if(!inherits(ans,"try-error")) return(tkmessageBox(message = "Correct 'export2csv'", icon = "info", type="ok"))
     }
     export2csv.aux2 <- function(){
          res <- prepare.input()
          if(!inherits(res,"list")) return()
        ans <- try(eval(parse(text= paste("compareGroups(X = res$X, y = res$y, Xext=res$Xext, selec =", res$selec,", method = res$method, alpha = res$alpha,min.dis = 5, max.ylev = 5, ref.y = res$ref.y, ref= res$ref)"))))
          if(inherits(ans,"try-error")) return(tkmessageBox(message = "Error occurred in 'compareGroups' function", icon = "info", type="ok"))
            if(!inherits(ans,"try-error")) ans <- try(createTable(x=ans,hide=res$hide, digits=res$digits,type=res$type, show.all= res$show.all, show.p.trend= res$show.p.trend,show.p.mul= res$show.p.mul,show.n=res$show.n, show.p.overall = res$show.p.overall, show.ratio = res$show.ratio, show.descr = res$show.descr))
          if(inherits(ans,"try-error")) return(tkmessageBox(message = "Error occurred in 'createTable' function", icon = "info", type="ok"))
          GetDir <- function(){
               directory <- tclvalue(tkgetSaveFile(parent = tt,initialfile="CompareGroupsTable",title = "Select where to save table"))
               directory <- as.character(directory)
               if (directory=="") return()
               return(directory)
           }
           directory <- GetDir()
           if(is.null(directory)) return()
           setwd(dirname(directory))
           ans <- try(export2csv(ans, file = directory, sep=";", which.table="both"))
          if(inherits(ans,"try-error")) return(tkmessageBox(message = "Error occurred in 'export2csv' function", icon = "info", type="ok"))
          if(!inherits(ans,"try-error")) return(tkmessageBox(message = "Correct 'export2csv'", icon = "info", type="ok"))
     }
     topMenu <- tkmenu(tt)
     tkconfigure(tt,menu=topMenu)
     fileMenu <- tkmenu(topMenu,tearoff=FALSE)
     tkadd(topMenu,"cascade",label="Load File",menu=fileMenu)
     tkadd(fileMenu,"command",label="SPSS",command=spssLoad)
     tkadd(fileMenu,"command",label="Rdata",command=RData)
     tkadd(fileMenu,"command",label="Workspace",command=RWspace)
     openMenu <- tkmenu(topMenu,tearoff=FALSE)
     tkadd(openMenu,"command",label="Sep: comma (,)", command=plaincomma)
     tkadd(openMenu,"command",label="Sep: semicolon (;)", command=plaincolon)
     tkadd(fileMenu,"cascade",label="Plain text",menu=openMenu)
     fileMenu1 <- tkmenu(topMenu,tearoff=FALSE)
     tkadd(topMenu,"command",label="Report options",command=report.options)
     fileMenu2 <- tkmenu(topMenu,tearoff=FALSE)
     tkadd(topMenu,"cascade",label="Export",menu=fileMenu2)
     tkadd(fileMenu2,"command",label="Print",command=print.aux)
     tkadd(fileMenu2,"command",label="LaTeX",command=export2latex.aux)
      tkadd(fileMenu2,"command",label="HTML",command=I)
     openRecentMenu <- tkmenu(topMenu,tearoff=FALSE)
     tkadd(openRecentMenu,"command",label="Sep: comma (,)", command=export2csv.aux1)
     tkadd(openRecentMenu,"command",label="Sep: semicolon (;)", command=export2csv.aux2)
     tkadd(fileMenu2,"cascade",label="CSV",menu=openRecentMenu)
}

