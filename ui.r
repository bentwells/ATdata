## Main UI function for AT Data app
shinyUI(fluidPage(title="Appalachian Trail Elevation Profiles",

  ## Apply HTML styles to various parts of the page
  tags$head(tags$style(HTML("#heading-panel { background-color:#80E0FF; padding-left:10px; }"))),
  tags$head(tags$style(HTML("#control-panel { background-color:#E0E0E0; padding:10px; }"))),
  tags$head(tags$style(HTML("#display-intro { background-color:#AAFFAA; padding:10px; 
    height:700px; width:960px; }"))),

  ## Header panel
  absolutePanel(id="heading-panel",h1("Appalachian Trail Elevation Profiles"),
    top="0px",left="0px",height="80px",width="1280px"),
  
  ## Control panel with menus, buttons, and download links (left side)
  absolutePanel(id="control-panel",
    selectInput(inputId="start.state",label="Start State:",
      choices=at.states,selected="Georgia"),
    uiOutput("start.location"),
    selectInput(inputId="end.state",label="End State:",
      choices=at.states,selected="Maine"),
    uiOutput("end.location"),
    radioButtons(inputId="mile.count",label="Mileage Labels:",
      choices=c("Start from 0","Start from North/South End"),selected="Start from 0"),
    sliderInput(inputId="mi.per.panel",label="Miles per Panel:",
      min=20,max=40,value=30,step=5,width="320px"),
    actionButton("go.button","Go!",width="100px"),
    br(),br(),
    conditionalPanel(condition="input['go.button'] > 0",
      downloadLink("download.data","Download this Information (PDF)")),
    top="80px",left="0px",height="700px",width="320px"),

  ## Main panel displaying elevation profiles
  absolutePanel(id="display-panel",
    conditionalPanel(id="display-intro",condition="input['go.button'] == 0",
      p(strong("Welcome!")),
      p("Heading out for a day hike or section hike on the Appalachian Trail? With a few
         clicks, this app will",br(),"provide you with information you need to prepare for 
         your hike."),
      p("Use the menus on the left to select your starting and ending points on the trail,
         then click 'Go' to get",br(),"customized elevation profiles and data tables with
         basic information on distances, elevation changes,",br(),"and amenities such as
         water, shelters and campsites for points of interest on your section of the AT."),
      p("Use the remaining buttons to get the profiles looking the way you want, then click
         on the link that appears",br(),"at the bottom of the menu panel to download a free
         PDF copy of the elevation profiles and data table."),
      p("Hiking SOBO? No problem. Enter your northern entry point as your starting location,
         and the app figures",br(),"everything else out for you. No more having to read charts
         backwards or subtract numbers in your head."),
      p("Planning a thru-hike? Check out the latest edition of",a("WhiteBlaze Pages,",
         href="https://whiteblazepages.com/"),"and get up-to-date information",br(),
         "on hostels, trail shuttles, and current trail conditions at",a("whiteblaze.net.",
         href="https://whiteblaze.net/")),
      p("Like this page? Please consider donating to keep it running.",a("Donate",
         href="https://www.paypal.me/bentwells/")),
      p("The mileage and elevation data are based on the official 2023 mileages and GPS data
         from the",br(),a("Appalachian Trail Conservancy.",href="https://appalachiantrail.org"))),
    conditionalPanel(condition="input['go.button'] > 0",
      plotOutput("display.plot",height="600px",width="960px")),
    top="80px",left="320px",height="700px",width="960px"),
  
  ## Slider bar beneath image display
  absolutePanel(id="slider-bar",
    conditionalPanel(condition="input['go.button'] > 0",uiOutput("panel.num")),
    top="680px",left="320px",height="100px",width="960px"),
  
  ## Legend panel beneath slider bar
  absolutePanel(id="legend-panel",
    conditionalPanel(condition="input['go.button'] > 0",
    plotOutput("legend.panel",height="300px",width="960px")),
    top="780px",left="320px",height="300px",widh="960px"),

  ## Data table beneath legend panel
  absolutePanel(id="display-table",
    conditionalPanel(condition="input['go.button'] > 0",
    DT::dataTableOutput("display.table")),
    top="1080px",left="320px",height="600px",width="960px")
))
