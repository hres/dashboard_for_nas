renderName <- function() { 
  
  ( htmlOutput('HCtablename') )
  
} 

aboutAuthors <- function() {list(
  tags$h4("Development Team:"),
  fluidRow(
    box(
      "Daniel Buijs, MSc", br(),
      "Data Scientist, Health Products and Food Branch", br(),
      "Health Canada / Government of Canada", br(),
      "daniel.buijs@hc-sc.gc.ca",
      width = 4
    ),
    box(
      "Nanqing Zhu, MSc (in progress)", br(),
      "Jr. Data Scientist, Health Products and Food Branch", br(),
      "Health Canada / Government of Canada", br(),
      "nanqing.zhu@mail.mcgill.ca",
      width = 4
    ),
    box(
      "Jason , BSc (in progress)", br(),
      "Jr. Data Scientist Co-op, Health Products and Food Branch", br(),
      "Health Canada / Government of Canada", br(),
      "haiyang.jiang@canada.ca or haiyangj@sfu.ca",
      width = 4
    )
  )
)}

#####functions for the about page:
makeapplinks <- function(){
  mynames<-c('DA','DISP','D','E', 'P', 'Z', 'LRE', 'LR','FDA')
  
  labels <- vector(mode='character', length = length(mynames))
  names(labels)<-mynames
  labels['DA'] <- '<h5>Drug Apps</h4><b>Dashboard-</b> Overview of reports for a drug'
  labels['FDA']<-'<b>Dashboard (FDA version)-</b> Overview of reports from FAERS'
  labels['DISP']<-'<b>Disproportioanlity Analysis for a Drug-Event-</b> Calculate Information Component for Common Events for a drug'
  labels['D'] <- '<b>PRR for a Drug-</b> Calculate Proportional Reporting Rates for Common Events for a drug'
  labels['E'] <-  '<b>PRR for an Event-</b> Calculate Proportional Reporting Rates for Common Drugs that have a specified event'
  labels['P'] <-  '<b>Dynamic PRR-</b> Calculate Proportional Reporting Rates for a drug-event pair over time'
  labels['Z'] <- '<b>Change Point Analysis-</b> Change point analysis for a drug-event pair over time'
  labels['LR'] <- '<b>Likelihood Ratio Test for Drug-</b> Calculate Likelihood Ratio Tests for Common Events for a drug'
  labels['LRE'] <-'<b>Likelihood Ratio Test for Event-</b> Calculate Likelihood Ratio Tests for Common Drugs for an event'
  
  link<-vector(mode='character', length = length(mynames))
  names(link)<-mynames
  link['DA']<-'https://shiny.hres.ca/CVShiny'
  link['FDA']<-'https://shiny.hres.ca/shinyfaers'
  link['DISP']<-'https://shiny.hres.ca/shinydisp_test2'
  link['D']<-'https://shiny.hres.ca/OpenFDA_CV/prrD_test'
  link['E']<-'https://shiny.hres.ca/OpenFDA_CV/prrE_test'
  link['P']<-'https://shiny.hres.ca/OpenFDA_CV/dynprr_test'
  link['Z']<-'https://shiny.hres.ca/OpenFDA_CV/ChangePoint_test'
  link['LR']<-'https://shiny.hres.ca/OpenFDA_CV/LRTestD_test'
  link['LRE']<-'https://shiny.hres.ca/OpenFDA_CV/LRTestE_test'
  
  out<-vector()
  for (i in seq_along(mynames)){
    out[i] <- paste0('<a href=',link[i],' target=_blank >',labels[i],'</a><br>')
  }
  return(out)
}
# Define UI for application that draws a histogram
ui<-dashboardPage(
  dashboardHeader(title="A Brief Report for Approved New Active Substances(NAS) in Canada since 2016-01-01",
                  titleWidth = 850
  ),
  dashboardSidebar(
    width = 280,
    sidebarMenu(id='menu',
      menuItem("Overview",tabName="overview",icon=icon("eye")),
      menuItem("ADE Reports",tabName="ade",icon=icon("flag")),
      menuItem("Pubmed Records",tabName="pubmed",icon=icon("book")),
      menuItem("Issued Advisory",tabName="advise",icon=icon("podcast")),
      menuItem("Notice of Compliance",tabName = 'NOC',icon=icon("bullhorn")),
      menuItem("Product Monograph Updates",tabName = 'labelchange',icon=icon("sticky-note")),
      menuItem("Safety review",tabName = 'safereview',icon=icon('comment')),
      menuItem('Summary Basis of Decision',tabName="sbd",icon=icon('archive')),
      menuItem('Regulatory Decision Summary',tabName = 'rds',icon=icon('database')),
      menuItem("About",tabName="about",icon=icon("info"))
    ),
    
      
    selectizeInput("name_type",
                     "Drug name type",
                     choices=c("Active Ingredient","Brand Name")
      ),
      
    conditionalPanel(
        condition="input.name_type=='Brand Name'",
        selectizeInput("brand",
                       "Brand Name",
                       choices=c("Start typing to search..." = "",drugchoice)
    )
  ),
  
  conditionalPanel(
    condition="input.name_type=='Active Ingredient'",
    selectizeInput("ing",
                   "Active Ingredient",
                   choices=c("Start typing to search..." = "",ingchoice,"CLOZAPINE","INFLIXIMAB")
    )
  )
),
  dashboardBody(
    tabItems(
      tabItem(tabName="overview",
              fluidRow(
                box(plotlyOutput('company'),width=8)),
              
              fluidRow(box(plotlyOutput('status'),width=5),
                       box(plotlyOutput('ATC'))),
              
              fluidRow(box(DT::dataTableOutput('drugtable'),width=12))
              ),
      
      
      tabItem(tabName="ade",
              fluidRow(
                box(plotlyOutput('cv'),
                    HTML("<b>Since 2015-01-01</b><br>")),
                box(plotlyOutput('fda')),
                div(style="display:inline-block",
                radioButtons(inputId = "time_selection",label=NULL,choices=c("Since 2016-01-01","Since on market"),inline=TRUE)),
                box(DT::dataTableOutput('cv_event')),
                box(DT::dataTableOutput('fda_event'))
              )),
      
      tabItem(tabName="pubmed",
              fluidRow(
                box(plotlyOutput('pubtime')),
                box(plotlyOutput('pubcountry'))
              ),
              
              fluidRow(
                box(plotlyOutput('pubjournal'),width=10)
              ),
              
              fluidRow(
                box(DT::dataTableOutput('pubtable'),width=10)
              )
              
              ),
      tabItem(tabName="advise",
              fluidRow(
                box(width=12,
                    renderName(),
                  DT::dataTableOutput('HCadvise'))
                ),
              fluidRow(
                box(
                  htmlOutput('FDAtablename'),
                  DT::dataTableOutput('FDAadvise'),width=12)
              )),
      tabItem(tabName='NOC',
              
              box(
                width=12,
                htmlOutput('NOCtitle'),
                DT::dataTableOutput("NOCtable")
                )
              ),
      tabItem(tabName='labelchange',
              box(
                width=12,
                htmlOutput('labelchangetitle'),
                DT::dataTableOutput('labelchangetable')
              )),
      tabItem(tabName='safereview',
              box(
                width=12,
                htmlOutput('safereviewtitle'),
                DT::dataTableOutput('safereviewtable')
              )),
      tabItem(tabName='sbd',
              
              box(
                width=12,
                htmlOutput('sbdtitle'),
                DT::dataTableOutput("sbdtable")
              )
      ),
      tabItem(tabName='rds',
              
              box(
                width=12,
                htmlOutput('rdstitle'),
                DT::dataTableOutput("rdstable")
              )
      ),
      tabItem(tabName = "about",
              box(
                width = 12,
                h2("About"),
                
                HTML(paste(
                  
                  "",
                  "Before using, please read over the Canada Vigilance Adverse Reaction caveat document.",
                  
                  "<p>",
                  "This is a proof-of-concept product, showing analysis for new active substances approved in Canada since 2016-01-01 ",
                  "</p>",
                  "<p>",
                  "This is a prototype experiment that utilizes publically available data from",
                  "<a href ='https://www.canada.ca/en/health-canada/services/drugs-health-products/medeffect-canada/adverse-reaction-database/canada-vigilance-online-database-data-extract.html' target=_blank> Canada Vigilance Adverse Reaction Online Database</a> ",
                  "and <a href='https://open.fda.gov' target=_blank>OpenFDA API</a>",
                  "providing visualizations in an interactive format.This app allows users to effortlessly interact with the reports", 
                  "database, conduct searches and view results in highly interactive dashboards.",
                  "</p>",
                  "<br>",
                  "<p>",
                  "For more detailed analysis, please refer to other Apps developed by the Data Science group ","</p>")),
                
                  HTML(makeapplinks()),
                  div(style="display:inline-block",
                  aboutAuthors())))
      
      
    )
  )
)
  
    
   
