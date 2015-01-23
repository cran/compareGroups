# Read contextual help file 

shinyUI(pageWithSidebar(
    
    #headerPanel(HTML('<h2>compareGroups: descriptive statistics by goups</h2>')),   
    headerPanel("",windowTitle="compareGroups | Explore and Summarise Epidemiological Data in R"),
    sidebarPanel(   
  
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
        tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');")),
        tags$style(HTML(".span4{width: 27%; scroll:auto;};")),
        tags$style(HTML(".span4 .well{background-color:#E0E0E0; border:1px solid grey}")),
        tags$style(type = "text/css", ".navbar {background-color: blue;}")
      ),


           # tags$head(
           #     tags$style("body {background-color: lightgreen; }")
           # ),
           

      tabsetPanel(id="menu", type="pills",
        #### Loads data and lists variables in the data set to be selected####
        tabPanel("START",
          ## load
          div(class="row-fluid",
            div(class="span6",HTML("<button id='showload' type='button' class='btn action-button' style='border:2px solid grey'><b>Toggle Load Panel</b></button>")),
            div(class="span5 offset0",uiOutput("initial")),
            wellPanel(
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
            radioButtons('resptype', 'Response type',  c("None","Group","Survival"),'None'),                 
            uiOutput("response")
          ),            
          ## hide
          tabPanel("Hide",
            wellPanel(
              uiOutput("selehidevar"),
              uiOutput("selehidecat")
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
    ),

    ###########################
    #######  RESULTS ##########
    ###########################

    mainPanel(
    
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
                          dataTableOutput('valuesext')
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
    )
)

)                               
