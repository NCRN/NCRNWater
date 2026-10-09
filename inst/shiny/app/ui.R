library(shiny)
library(leaflet)
library(plotly)


shinyUI(
  fluidPage(
    theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css"
    ,style="padding: 0px"
    ,title=paste0(NETWORK, " Water Quality")
    ,column(
      12
      ,id="NPSBanner"
      ,style="margin: 0px"
      ,tags$head(includeScript ("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"))
      ,tags$head(
        tags$script(
        'type = "text/javascript"'
        ,'
        var ss = document.createElement("link"); ss.type="text/css"; ss.rel="stylesheet"; 
        ss.href = window.self === window.top ? "NCRN.css" : "NCRNframe.css"; document.getElementsByTagName("head")[0].appendChild(ss);
        var dimension = [0, 0];
        $(document).on("shiny:connected", function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
        });
        $(window).resize(function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
        });
        '
        )
      )
      # ,tags$head(HTML( '<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />'))
      ,div(
        h1(
          style="background-color: black; color: white; height: 125px; padding: 10px; margin: 0px"
          ,HTML(NETWORK_LONG, '<br>', VIZ_NAME)
          )
      )
      ,tags$head(tags$style(HTML("
                                 .leaflet-tooltip {
                                 background-color: transparent !important;
                                 padding: 0 !important;
                                 border: none !important;
                                 box-shadow: none !important;
                                 }
                                 .leaflet-tooltip .label-text {
                                 background-color: white;
                                 padding: 1px 5px;
                                 border-radius: 4px;
                                 display: inline-block;
                                 }
                                 ")))
      ,tags$head(
      #custom styling for loading message
      tags$style(HTML("
                    .typing-text {
                    font-family: monospace;
                    #overflow: hidden;
                    #white-space: nowrap;
                    #width: fit-content;
                    font-size: 24px;
                    color: #333;
                    text-align: center;
                    position: relative;
                    margin-top: 50px;
                    margin: 40px;
                    display: none;
                    }
                    .loading-image {
                    max-width: 700px;
                    max-height: 500px;
                    margin: 0 auto;
                    display: none;
                    }
                    .caption-text {
                    margin-top: 12px; font-size: 18px; color: #333; display: none
                    }
                    .loading-bar-container {
                    margin-top: 20px;
                    height: 12px;
                    width: 40%;
                    background-color: #e0e0e0;
                    overflow: hidden;
                    border-radius: 5px;
                    display: none;
                    }
                    .loading-bar {
                    height: 100%;
                    width: 0%;
                    background-color: #333;
                   # transition: width 0.5s ease;
                    }
                    #' @keyframes loading-progress {
                    #' 0% { transform: translateX(-100%); }
                    #' 100% { transform: translateX(200%); }
                    #' }
                    # .Spinner {
                    # color: $4CAF50;
                    # font-size: 60px;
                    # text-indent: -9999em;
                    # overflow: hidden;
                    # width: 1em;
                    # height: 1em;
                    # border-radius: 50%;
                    # margin-top: 40px;
                    # margin: 72 auto;
                    # display: none;
                    # position: center;
                    # -webkit-transform: translateZ(0);
                    # -ms-transform: translateZ(0);
                    # transform: translateZ(0);
                    # -webkit-animation: load6 1.7s infinite ease, round 1.7s infinite ease;
                    # animation: load6 1.7s infinite ease, round 1.7s infinite ease;
                    # }
                    #' @-webkit-keyframes load6 {
                    #'   0% {
                    #'     box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;  }
                    #'   5%, 95% {
                    #'     box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;  }
                    #'   10%, 59% {
                    #'     box-shadow: 0 -0.83em 0 -0.4em, -0.087em -0.825em 0 -0.42em, -0.173em -0.812em 0 -0.44em, -0.256em -0.789em 0 -0.46em, -0.297em -0.775em 0 -0.477em;  }
                    #'   20% {
                    #'     box-shadow: 0 -0.83em 0 -0.4em, -0.338em -0.758em 0 -0.42em, -0.555em -0.617em 0 -0.44em, -0.671em -0.488em 0 -0.46em, -0.749em -0.34em 0 -0.477em; }
                    #'   38% {
                    #'     box-shadow: 0 -0.83em 0 -0.4em, -0.377em -0.74em 0 -0.42em, -0.645em -0.522em 0 -0.44em, -0.775em -0.297em 0 -0.46em, -0.82em -0.09em 0 -0.477em; }
                    #'   100% {
                    #'     box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;  }
                    #' }
                    #' @keyframes load6 {
                    #'   0% {
                    #'     box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;  }
                    #'   5%,
                    #'   95% {
                    #'     box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;  }
                    #'   10%,
                    #'   59% {
                    #'     box-shadow: 0 -0.83em 0 -0.4em, -0.087em -0.825em 0 -0.42em, -0.173em -0.812em 0 -0.44em, -0.256em -0.789em 0 -0.46em, -0.297em -0.775em 0 -0.477em;  }
                    #'   20% {
                    #'     box-shadow: 0 -0.83em 0 -0.4em, -0.338em -0.758em 0 -0.42em, -0.555em -0.617em 0 -0.44em, -0.671em -0.488em 0 -0.46em, -0.749em -0.34em 0 -0.477em; }
                    #'   38% {
                    #'     box-shadow: 0 -0.83em 0 -0.4em, -0.377em -0.74em 0 -0.42em, -0.645em -0.522em 0 -0.44em, -0.775em -0.297em 0 -0.46em, -0.82em -0.09em 0 -0.477em; }
                    #'   100% {
                    #'     box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;  }
                    #' }
                    #' @-webkit-keyframes round {
                    #'   0% {
                    #'     -webkit-transform: rotate(0deg);
                    #'     transform: rotate(0deg);
                    #'   }
                    #'   100% {
                    #'     -webkit-transform: rotate(360deg);
                    #'     transform: rotate(360deg);
                    #'   }
                    #' }
                    #' @keyframes round {
                    #'   0% {
                    #'     -webkit-transform: rotate(0deg);
                    #'     transform: rotate(0deg);
                    #'   }
                    #'   100% {
                    #'     -webkit-transform: rotate(360deg);
                    #'     transform: rotate(360deg);
                    #'   }
                    #' }
                    ")),
      
      #Loading screen HTML div (initially visible)
      div(id = "loading_screen", #"Loading, please wait...",
          style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: white; opacity: 0.8; 
          z-index: 9999; display: flex; align-items: center; justify-content: center; flex-direction: column;",
          
          tags$div(id =  "typingText", class = "typing-text"),
          tags$img(id = "loadingImage", class = "loading-image", alt = "Water monitoring Image"),
          tags$div(id = "captionText"),
          tags$div(id = "spinner", class = "loading-bar-container",
                   tags$div(id = "progressBar", class = "loading-bar"))),
      
      tags$script(HTML(sprintf("
                     const facts = %s;
                     const images = %s;
                     
              setTimeout(() => {
                     if (!window.shinyAppLoaded) {
                     const fact_index = Math.floor(Math.random() * facts.length);
                     const image_index = Math.floor(Math.random() * images.length);
                   
                    const fact1 = document.getElementById('typingText'); 
                      fact1.textContent = facts[fact_index];
                      fact1.style.display = 'block';
                    const img1 = document.getElementById('loadingImage')
                      img1.src = images[image_index].src;
                      img1.style.display = 'block';
                    const caption1 = document.getElementById('captionText')
                      caption1.textContent = images[image_index].location + ' - ' + images[image_index].date;
                      caption1.style.display = 'block';  
                    const spinner1 = document.getElementById('spinner');
                    const progressBar = document.getElementById('progressBar');
                      spinner1.style.display = 'block';
                         
                    let progress = 0;
                    const progressInterval = setInterval(() => {
                    if (progress < 100) {
                    progress += 0.7;
                    
                    document.getElementById('progressBar').style.width = progress + '%%';
                    } else {
                    clearInterval(progressInterval);
                      }
                     }, 60);
                    }
                  }, 2000);",
                               jsonlite::toJSON('Application loading...', auto_unbox = TRUE),
                               jsonlite::toJSON(LOADING_IMAGES[[1]]$src, auto_unbox = TRUE)))))
    ),
    
  
 #mainPanel(
  tabsetPanel(

      tabPanel(
        h4("Time Series")
        ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,fluidRow(
            column(width=4, parkChooserUI("TimePark"))
            ,column(width=4, siteChooserUI("TimeSite"))
            ,column(width=4, paramChooserUI("TimeParam"))
          )
          ,fluidRow(
            column(width=4, yearChooserUI("TimeYears"))
            ,column(width=2,checkboxInput("ShowHidePoint", "Show points", TRUE))
            ,column(width=2, checkboxInput("SeriesThreshLine","Show Water Quality Threshold Line", TRUE))
            ,column(width=2, actionButton(inputId="GraphicsModal2", label='Graphics Options', class="btn btn-primary",style="margin-top: 15px"))
            ,column(width=2, actionButton(inputId="AboutTimeSeries", label="About this Graph...", class="btn btn-primary",style="margin-top: 15px"))
          )
          ,fluidRow(
            column(width=7)
            ,column(width=5, htmlOutput("SeriesThresholdSummaryMultiple"))
          )
        )
        ,fluidRow(
          column(width=12, plotlyOutput("SeriesPlot"))
        )
      ),  

      tabPanel(
        h4("Boxplot")
        ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,fluidRow(
            column(width=4, parkChooserUI("BoxPark"))
            ,column(width=4, siteChooserUI("BoxSite"))
            ,column(width=4, paramChooserUI("BoxParam"))
          )
          ,fluidRow(
            column(width=4, yearChooserUI("BoxYears"))
            ,column(width=2, radioButtons(inputId="BoxBy", label="Compare by:", choices=c("year", "month", "site"), selected = "year", inline = F))
            ,column(width=2, checkboxInput("BoxThreshLine","Show Water Quality Threshold Line", TRUE))
            ,column(width=2, actionButton(inputId="GraphicsModal2", label='Graphics Options', class="btn btn-primary",style="margin-top: 15px"))
            ,column(width=2, actionButton(inputId="AboutComparisons", label="About this Graph...", class="btn btn-primary",style="margin-top: 15px"))
          )
          ,fluidRow(
              column(width=7)
              ,column(width=5, htmlOutput("BoxThresholdSummaryMultiple"))
          )
        )
        ,fluidRow(
          column(width=12, plotlyOutput("BoxPlot"))
        )
      ),  

      tabPanel(
        h4("Correlations")
        ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,fluidRow(
            column(width=4, parkChooserUI("CorrPark"))
            ,column(width=4, siteChooserUI("CorrSite"))
            ,column(width=4, yearChooserUI("CorrYears"))
          )
          ,fluidRow(
            column(width=4, paramChooserUI("CorrParam1"))
            ,column(width=4, paramChooserUI2("CorrParam2"))
            ,column(width=2, actionButton(inputId="GraphicsModal2", label='Graphics Options', class="btn btn-primary",style="margin-top: 15px"))
            ,column(width=2, actionButton(inputId="AboutCorrelations", label="About this Graph...", class="btn btn-primary",style="margin-top: 15px"))
          )
        )
        ,fluidRow(
          column(width=12, plotlyOutput("CorrPlot"))
        )
      ),  

      tabPanel(h4("Map"),
        column(2, div(style='padding: 5px 10px',class="panel panel-default",
          uiOutput("MapChars"),

         div(style = "display: block", 
            #  checkboxInput(inputId="InactiveSites", label="Display inactive sites", value=F), 
             checkboxGroupInput(inputId="MapIn",label="Park(s)" , choices=NULL, inline = FALSE),
             actionButton("refreshParks", "Reset park selections", class = "btn btn-primary", style = "margin-top: 10px;")),
              
          # to hide the "US Geological Survey Stream Gages" checkbox, we'll make it only conditionally-visible
          # this approach leaves all of the downstream code intact to avoid breaking dependencies
          # take the contents out of the conditionalPanel and reload the app to restore functionality
          conditionalPanel( 
            condition = "1===2", # a condition that only ever evaluates to FALSE
            HTML('<hr >'),
            br(),
            strong("US Geological Survey Stream Gages"),
            checkboxInput(inputId="MapUSGS", label="Map USGS Gaging Stations (slow)", value=F)
          ),
          actionButton(inputId="AboutMap", label="About this Map...", class="btn btn-primary",style="margin-top: 15px")
          
        )),
        column(10, style="padding: 0",
               leafletOutput("WaterMap",width = "100%", height="900px")
        ) 
      ),
      tabPanel(
        h4("Data")
        ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,uiOutput('DatasetURL')
          ,fluidRow(
            column(width=4, parkChooserUI("DataPark"))
            ,column(width=4, siteChooserUI("DataSite"))
            ,column(width=4, paramChooserUI("DataParam"))
          )
        )
        ,DT::dataTableOutput("WaterTable")
      ),
      tabPanel(
        h4("Summary")
        ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,fluidRow(
            column(width=4, parkChooserUI("SummaryPark"))
            ,column(width=4, siteChooserUI("SummarySite"))
            ,column(width=4, paramChooserUI("SummaryParam"))
          )
          ,fluidRow(
            column(width=4, yearChooserUI("SummaryYears"))
            ,column(width=4, radioButtons(inputId="SummaryBoxBy", label="Compare by:", choices=c("year", "month", "site"), selected = "year", inline = T))
            ,column(width=4, actionButton(inputId="AboutSummary", label="About this Table...", class="btn btn-primary",style="margin-top: 15px"))
          )
        )
        ,uiOutput("summary_box_ui")
        ,tags$head(
        tags$style(HTML("
          .summary-box {
          background-color: #f5f3e5;
          padding: 10px;
          border-radius: 5px;
          margin-bottom: 15px;
          }
          .dt-buttons {
          float: right !important;
          }
          .shiny-notification {
          background-color: #D0342C;
          color: white;
          font-size: 18px;
          border-radius: 10px;
          padding: 15px;
          }
          .shiny-notification-close {
           color: white;
           } 
           ")))
      ),
      
  tabPanel(
    h4("Exceedances")
    ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,fluidRow(
            column(width=4, parkChooserUI("DataParkExceedances"))
            ,column(width=4, siteChooserUI("DataSiteExceedances"))
            ,column(width=4, paramChooserUI("DataParamExceedances"))
          ),fluidRow(
            column(width=8, actionButton("hist_button", "Hide Figure"))
            ,column(width=4, actionButton(inputId="AboutExceedances", label="About Exceedances...", class="btn btn-primary"))
          )
        )
    ,uiOutput("mytabs")
     
    ),
    tabPanel(
      h4("Photos")
      ,tabsetPanel(
        tabPanel(
          h4("First Photo")
          ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,fluidRow(
            column(width=4, parkChooserUI("PhotoPark"))
            ,column(width=4, siteChooserUI("PhotoSite"))
            ,column(width=4, yearChooserUI("PhotoYears"))
          )
          ,fluidRow(
            column(width=4, siteVisitChooserUI("PhotoSiteVisit"))
            ,column(width=4, photoChooserUI("PhotoPhoto"))
            ,column(width=4, uiOutput('NoPhotos'))
          )
        )
        ,imageOutput("image_plot")      
        )
        ,tabPanel(
          h4("Second Photo")
          ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,fluidRow(
            column(width=4, parkChooserUI2("PhotoPark2"))
            ,column(width=4, siteChooserUI2("PhotoSite2"))
            ,column(width=4, yearChooserUI2("PhotoYears2"))
          )
          ,fluidRow(
            column(width=4, siteVisitChooserUI2("PhotoSiteVisit2"))
            ,column(width=4, photoChooserUI2("PhotoPhoto2"))
            ,column(width=4, uiOutput('NoPhotos2'))
          )
        )  ,imageOutput("image_plot2")      
      )
    )
    ),
    
    tabPanel(
      h4("About")
      ,shiny::includeHTML("www/projectintro.html")
      )
    )
  )
)
