# Read contextual help file 

shinyUI(fluidPage(
    
    #headerPanel(HTML('<h2>compareGroups: descriptive statistics by goups</h2>')),   
    headerPanel("",windowTitle="compareGroups | Explore and Summarise Epidemiological Data in R"),

    HTML('<style type="text/css"> #inputpanel{ background-color: rgb(200,200,200); border: 2px solid grey; box-shadow: 2px 2px 1px #888888; } </style>'),
    HTML('<style type="text/css"> #inputpanel .well{ background-color: rgb(200,200,200); border: 2px solid grey;} </style>'),
    HTML('<style type="text/css"> #outpanel { background-color: rgb(250,250,250); border: 2px solid grey; box-shadow: 2px 2px 1px #888888;} </style>'),
    HTML('<style type="text/css"> #showload { background-color: rgb(250,250,250); border: 2px solid grey;} </style>'),
    HTML('<style type="text/css"> #selevars { width: 120px;} </style>'), 
    HTML('<style type="text/css"> #discvars { width: 120px;} </style>'),
    HTML('<style type="text/css"> #changeselevars { font-size: 15px; border: 2px solid grey;} </style>'),
    HTML('<style type="text/css"> #varPlot { width: 120px;} </style>'),
    HTML('<style type="text/css"> #maxvalues { width: 100px;} </style>'),
     


#    tags$head(
#      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
#      tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');")),
#      tags$style(HTML(".span4{width: 27%; scroll:auto;};")),
#      tags$style(HTML(".span4 .well{background-color:#E0E0E0; border:1px solid grey}")),
#      tags$style(type = "text/css", ".navbar {background-color: blue;}")
#    ),


    fluidRow(

    ##################################################     
    ################ CONTROL PANEL ###################
    ##################################################
    
    column(4,

      wellPanel(id="inputpanel",   
      
      tabsetPanel(id="menu", type="pills",
        #### Loads data and lists variables in the data set to be selected####
        tabPanel("START",
          ## load
          div(class="row-fluid",
            wellPanel(
              fluidRow(
                column(6,HTML("<button id='showload' type='button' class='btn action-button' style='border:2px solid grey'><b>Toggle Load Panel</b></button>")),
                column(6,uiOutput("initial"))
              ),
              conditionalPanel(
                condition = "input.showload != null && input.showload % 2 == 0",
                div(  
                  #h5("File data"), 
                  br(),
                  selectInput("exampledata", "Choose data", choices = c("Own data","REGICOR","PREDIMED","SNPS"), selectize=FALSE),
                  conditionalPanel(
                    condition = "input.exampledata == 'Own data'",
                    div(
                      fileInput("files", "", accept=c('', 'sav', 'csv', 'dat', 'txt', 'xls', 'xlsx', 'mdb')),
                      radioButtons("datatype", "Data format", c('SPSS'='*.sav', 'TEXT'='*.txt','EXCEL'='*.xls','R'='*.rda'),'*.sav',inline = TRUE),
                      radioButtons("encoding", "Encoding", c('default'='default','latin1'='latin1','utf8'='utf8'),'default',inline = TRUE),
                      uiOutput("loadoptions")
                    )
                  )
                )
              )
            )
          ),
          ## vars list
          uiOutput("selevarslist")
        ),    
        navbarMenu("SETTINGS",
          ## methods
          tabPanel("Type",
             uiOutput("selemethod"),
             uiOutput("selemethodNA")
          ),
          ## response
          tabPanel("Response",
            hr(),
            radioButtons('resptype', 'Response type',  c("None","Group","Survival"),'None'),                 
            uiOutput("response")
          ),            
          ## hide
          tabPanel("Hide",
            wellPanel(
              fluidRow(
                column(6,uiOutput("selehidevar")),
                column(6,uiOutput("selehidecat"))
              )
            ),
            textInput('hideno', "Hide 'no' category", '')
          ),
          ## subset
          tabPanel("Subset",    
             uiOutput("selevarsubset")
          ),
          ## OR/HR ratio for row-variables
          tabPanel("OR/HR",
            uiOutput("ratio")
          )
        ),
        navbarMenu("DISPLAY",
          ## show
          tabPanel("Show",
            uiOutput("show")
          ),
          ## Format display
          tabPanel("Format",
            uiOutput("format")
          ),
          ## number of decimals
          tabPanel("Decimals",
            uiOutput("decimals")
          ),
          ## header labels
          tabPanel("Labels",
            uiOutput("labels")
          )             
        ),  
        tabPanel("SAVE",
          uiOutput("save")
        )
      )
    )),
    
    #################################
    #######  RESULTS PANEL ##########
    #################################

    column(8,
      wellPanel(id="outpanel",
    
          tabsetPanel(id="results",type="pills",selected="TABLE",
                      tabPanel("INFO",
                               uiOutput("info")
                      ),
                      navbarMenu("VALUES",
                        # summary
                        tabPanel("Summary",
                          uiOutput("values")
                        ),
                        # extended
                        tabPanel("Extended",
                          sliderInput("valueextsize", "Resize (%):", min=10, max=300, value=100, step=10),
                          uiOutput("valuesext")
                        )
                      ),                      
                      tabPanel("TABLE",
                               radioButtons("tabletype", "", c("HTML","PDF","R console"), selected="HTML",inline = TRUE),
                               uiOutput("table")
                      ),
                      tabPanel("PLOT",
                               uiOutput("uiplot")
                      ),
                      tabPanel("SNPs",
                               uiOutput("snps")
                      ),
                      navbarMenu("HELP",
                        # summary
                        tabPanel("About",
                          radioButtons("about", "", c("compareGroups","WUI","Security"), selected="compareGroups",inline = TRUE),
                          wellPanel(uiOutput("helpabout"))
                        ),
                        # extended
                        tabPanel("Panels",
                          radioButtons("screens", "", c("Control","Results"), selected="Control",inline = TRUE),
                          br(),
                          wellPanel(uiOutput("helpscreens"))
                        )
                      )                      
          )
    ))
  )

))                               
