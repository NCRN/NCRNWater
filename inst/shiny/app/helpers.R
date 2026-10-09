
#### Years Module ####
yearChooserUI<-function(id){
  ns<-NS(id)
  sliderInput(inputId=ns("YearsShow"), label= "Years", min=MINYR, max=MAXYR, step=1, value=c(MINYR,MAXYR),sep="",ticks=F)
}


yearChooser<-function(input,output,session,data,chosen)  {
  
  observe({
    req( data() )
    if(class(data()$Date)=="Date"){
      YrMax<-reactive(max(year(data()$Date), na.rm=T))
      YrMin<-reactive(min(year(data()$Date), na.rm=T))
      
      updateSliderInput(session, inputId="YearsShow", min=YrMin(),max=YrMax(),val=chosen())
    }
  })
  
  return(reactive(input$YearsShow))
}

yearChooserUI2<-function(id){
  ns<-NS(id)
  sliderInput(inputId=ns("YearsShow2"), label= "Years", min=MINYR, max=MAXYR, step=1, value=c(MINYR,MAXYR),sep="",ticks=F)
}


yearChooser2<-function(input,output,session,data,chosen)  {
  
  observe({
    req( data() )
    if(class(data()$Date)=="Date"){
      YrMax<-reactive(max(year(data()$Date), na.rm=T))
      YrMin<-reactive(min(year(data()$Date), na.rm=T))
      
      updateSliderInput(session, inputId="YearsShow2", min=YrMin(),max=YrMax(),val=chosen())
    }
  })
  
  return(reactive(input$YearsShow2))
}

#### Date Module ####
# daterangeChooserUI <-function(id){
#   ns<-NS(id)
#   dateRangeInput(inputId=ns("DateRangeIn"), label="4. Date Range (optional)", start = 2005-01-01, end = NULL)
# }
# 
# daterangeChooser<-function(input,output,session,data, chosen)  {
#   observe({
#     req(data())
#     MinDate<-min(data()$Date, na.rm=T)
#     MaxDate<-max(data()$Date, na.rm=T)
#     updateDateRangeInput(session, "DateRangeIn", start = MinDate, end = MaxDate)
#     })
# 
#   return(reactive(input$DateRangeIn))
# }

#### Park Module ####

parkChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("ParkIn"),label="Park" , choices=NULL)
}

parkChooser<-function(input,output,session, data, chosen){
  observe({updateSelectizeInput(session, "ParkIn", selected=chosen(),
                                choices=c("Choose a Park"="", c(`names<-`(getParkInfo(data, info="ParkCode"), getParkInfo(data, info="ParkShortName"))))
  )})
  return(reactive(input$ParkIn))
}

parkChooserUI2<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("ParkIn2"),label="Park" , choices=NULL)
}

parkChooser2<-function(input,output,session, data, chosen){
  observe({updateSelectizeInput(session, "ParkIn2", selected=chosen(),
                                choices=c("Choose a Park"="", c(`names<-`(getParkInfo(data, info="ParkCode"), getParkInfo(data, info="ParkShortName"))))
  )})
  return(reactive(input$ParkIn2))
}


#### Site Module ####

siteChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId = ns("SiteIn"),
                 label="Site",
                 choices=NULL,
                 multiple = TRUE,
                 options = list(plugins = list("remove_button"))
  )
}

siteChooser<-function(input, output, session, data, park, chosen){
  SiteIn <- shiny::reactive(input$SiteIn)
  
  observe({
    updateSelectizeInput(session, inputId = "SiteIn", selected=chosen(), 
                         choices=c("Choose a Site"="",
                                   c("Select All" = "ALL", setNames(getSiteInfo(data, parkcode=park(), info="SiteCode"), 
                                                                    getSiteInfo(data, parkcode=park(), info="SiteName") )))
    )
  })
  return(reactive({
    if ("ALL" %in% SiteIn()){
      getSiteInfo(data, parkcode = park(), info = "SiteCode")
    } else {
      SiteIn()
    }
  }))
}

siteChooserUI2<-function(id){
  ns<-NS(id)
  selectizeInput(inputId = ns("SiteIn2"),
                 label="Site",
                 choices=NULL,
                 multiple = TRUE,
                 options = list(plugins = list("remove_button"))
  )
}

siteChooser2<-function(input, output, session, data, park, chosen){
  SiteIn2 <- shiny::reactive(input$SiteIn2)
  
  observe({
    updateSelectizeInput(session, inputId = "SiteIn2", selected=chosen(), 
                         choices=c("Choose a Site"="",
                                   c("Select All" = "ALL", setNames(getSiteInfo(data, parkcode=park(), info="SiteCode"), 
                                                                    getSiteInfo(data, parkcode=park(), info="SiteName") )))
    )
  })
  return(reactive({
    if ("ALL" %in% SiteIn2()){
      getSiteInfo(data, parkcode = park(), info = "SiteCode")
    } else {
      SiteIn2()
    }
  }))
}

### Parameter Module ####

paramChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("ParamIn"), label="Parameter", choices=NULL)
}


paramChooser<-function(input, output, session, data, park, chosen){
  PChoices<-reactive({
    req(park())
    Choice<-getCharInfo(data, parkcode=park(), info="CharName")
    ChoiceName<-paste0(getCharInfo(data, parkcode=park(), info="DisplayName"), " (",
                       getCharInfo(data, parkcode=park(), info="Units") %>% 
                         iconv("","UTF-8"), ")")#%>% iconv("","UTF-8"))
    if(isTruthy(Choice) & isTruthy(ChoiceName)) { names(Choice)<-ChoiceName }
    return(Choice)
  })
  
  observe(
    updateSelectizeInput(session, inputId="ParamIn",selected=chosen(), 
                         choices=c("Choose a Parameter"="", as.list(sort(PChoices()))))
  )
  
  return(reactive(input$ParamIn))
}

paramChooserUI2<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("ParamIn2"), label="Second Parameter", choices=NULL)
}


paramChooser2<-function(input, output, session, data, park, chosen){
  PChoices<-reactive({
    req(park())
    Choice<-getCharInfo(data, parkcode=park(), info="CharName")
    ChoiceName<-paste0(getCharInfo(data, parkcode=park(), info="DisplayName"), " (",
                       getCharInfo(data, parkcode=park(), info="Units") %>% 
                         iconv("","UTF-8"), ")")#%>% iconv("","UTF-8"))
    if(isTruthy(Choice) & isTruthy(ChoiceName)) { names(Choice)<-ChoiceName }
    return(Choice)
  })
  
  observe(
    updateSelectizeInput(session, inputId="ParamIn2",selected=chosen(), 
                         choices=c("Choose a Parameter"="", as.list(sort(PChoices()))))
  )
  
  return(reactive(input$ParamIn2))
}

### Site Visit Module ####

siteVisitChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("siteVisitIn"), label="Site visit", choices=NULL)
}


siteVisitChooser<-function(input, output, session, data, park, site, years, imgs, chosen){
  PChoices<-reactive({
    req(park(), site(), years())
    park <- park()
    sites <- site()
    years <- years()
    imgs <- imgs()
    
    sitevisits <- c()
    if (park %in% names(imgs)){
      for (site in sites){
        if (site %in% names(imgs[[park]])){
          for (yr in names(imgs[[park]][[site]])){
            if (as.numeric(yr) >= as.numeric(years[1]) & as.numeric(yr) <= as.numeric(years[2])){
              for (sitevisit in names(imgs[[park]][[site]][[yr]])){
                sitevisits <- c(sitevisits, sitevisit)
              }
            }
          }
        }
      }
    }
    
    return(sitevisits)
  })
  
  observe(
    updateSelectizeInput(session, inputId="siteVisitIn",selected=chosen(), 
                         choices=c("Choose a site visit"="", as.list(sort(PChoices(), decreasing=T))))
  )
  
  return(reactive(input$siteVisitIn))
}

siteVisitChooserUI2<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("siteVisitIn2"), label="Site visit", choices=NULL)
}


siteVisitChooser2<-function(input, output, session, data, park, site, years, imgs, chosen){
  PChoices<-reactive({
    req(park(), site(), years())
    park <- park()
    sites <- site()
    years <- years()
    imgs <- imgs()
    
    sitevisits <- c()
    if (park %in% names(imgs)){
      for (site in sites){
        if (site %in% names(imgs[[park]])){
          for (yr in names(imgs[[park]][[site]])){
            if (as.numeric(yr) >= as.numeric(years[1]) & as.numeric(yr) <= as.numeric(years[2])){
              for (sitevisit in names(imgs[[park]][[site]][[yr]])){
                sitevisits <- c(sitevisits, sitevisit)
              }
            }
          }
        }
      }
    }
    
    return(sitevisits)
  })
  
  observe(
    updateSelectizeInput(session, inputId="siteVisitIn2",selected=chosen(), 
                         choices=c("Choose a site visit"="", as.list(sort(PChoices(), decreasing=T))))
  )
  
  return(reactive(input$siteVisitIn2))
}

#### Photo module
photoChooserUI<-function(id){
  ns<-NS(id)
  sliderInput(inputId = ns("PhotoPhoto"), label = "Photo", min = 1, max = 8, value = 1, ticks=F)
}

photoChooser<-function(input, output, session, data, park, site, years, imgs, sitevisit){
  PhotoChoices<-reactive({
    req(park(), site(), years(), imgs(), sitevisit())
    park <- park()
    sites <- site()
    years <- years()
    imgs <- imgs()
    sitevisit <- sitevisit()
    
    filenames <- c()
    if (park %in% names(imgs)){
      for (site in sites){
        if (site %in% names(imgs[[park]])){
          for (yr in names(imgs[[park]][[site]])){
            if (as.numeric(yr) >= as.numeric(years[1]) & as.numeric(yr) <= as.numeric(years[2])){
              for (visit in names(imgs[[park]][[site]][[yr]])){
                if (visit == sitevisit){
                  for (f in names(imgs[[park]][[site]][[yr]][[sitevisit]])){
                    fname <- imgs[[park]][[site]][[yr]][[sitevisit]][[f]]$rel_fpath
                    filenames <- c(filenames, fname)
                  }
                }
              }
            }
          }
        }
      }
    }
    
    return(filenames)
  })
  
  observe(
    updateSliderInput(session, inputId="PhotoPhoto", min=1,max=length(PhotoChoices()))
  )
  
  return(reactive(input$PhotoPhoto))
}

photoChooserUI2<-function(id){
  ns<-NS(id)
  sliderInput(inputId = ns("PhotoPhoto2"), label = "Photo", min = 1, max = 8, value = 1, ticks=F)
}

photoChooser2<-function(input, output, session, data, park, site, years, imgs, sitevisit){
  PhotoChoices<-reactive({
    req(park(), site(), years(), imgs(), sitevisit())
    park <- park()
    sites <- site()
    years <- years()
    imgs <- imgs()
    sitevisit <- sitevisit()
    
    filenames <- c()
    if (park %in% names(imgs)){
      for (site in sites){
        if (site %in% names(imgs[[park]])){
          for (yr in names(imgs[[park]][[site]])){
            if (as.numeric(yr) >= as.numeric(years[1]) & as.numeric(yr) <= as.numeric(years[2])){
              for (visit in names(imgs[[park]][[site]][[yr]])){
                if (visit == sitevisit){
                  for (f in names(imgs[[park]][[site]][[yr]][[sitevisit]])){
                    fname <- imgs[[park]][[site]][[yr]][[sitevisit]][[f]]$rel_fpath
                    filenames <- c(filenames, fname)
                  }
                }
              }
            }
          }
        }
      }
    }
    
    return(filenames)
  })
  
  observe(
    updateSliderInput(session, inputId="PhotoPhoto2", min=1,max=length(PhotoChoices()))
  )
  
  return(reactive(input$PhotoPhoto2))
}


resolve_tokens <- function(url, token_keys) {
  # token_keys example: list(API_KEY = "ENV:NPS_MAP_API_KEY")
  if (is.null(token_keys) || !length(token_keys)) return(url)
  out <- url
  for (nm in names(token_keys)) {
    v <- token_keys[[nm]]
    # Allow ENV:VAR or raw string
    val <- if (is.character(v) && startsWith(v, "ENV:")) Sys.getenv(sub("^ENV:", "", v), "")
    else v
    out <- gsub(paste0("\\{", nm, "\\}"), val, out, fixed = FALSE)
  }
  out
}

# --- Basemap helpers ----------------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Replace token placeholders in a tile URL template using ENV values or raw strings.
# Example: token_keys = list(API_KEY = "ENV:NPS_MAP_API_KEY")   -> reads Sys.getenv("NPS_MAP_API_KEY")
resolve_tokens <- function(url, token_keys) {
  if (is.null(token_keys) || !length(token_keys)) return(url)
  out <- url
  for (nm in names(token_keys)) {
    src <- token_keys[[nm]]
    val <- if (is.character(src) && startsWith(src, "ENV:")) Sys.getenv(sub("^ENV:", "", src), "")
    else src
    out <- gsub(paste0("\\{", nm, "\\}"), val, out, fixed = FALSE)
  }
  out
}

# Add configured basemaps to a Leaflet map object from cfg$map$basemaps.
# Supports either providers (e.g., "OpenStreetMap") or raw XYZ templates (url + attribution).
add_configured_basemaps <- function(m, cfg) {
  basemaps <- tryCatch(cfg$map$basemaps, error = function(e) NULL)
  default_provider <- tryCatch(cfg$map$default_provider, error = function(e) "OpenStreetMap")
  
  # Fallback: just OpenStreetMap if no basemaps were specified
  if (is.null(basemaps) || !length(basemaps)) {
    return(m %>% leaflet::addProviderTiles(providers[[default_provider %||% "OpenStreetMap"]], group = default_provider))
  }
  
  for (bm in basemaps) {
    grp  <- bm$group %||% bm$name %||% "Basemap"
    attr <- bm$attribution %||% ""
    
    if (!is.null(bm$provider)) {
      # Use Leaflet provider catalog (e.g., providers$OpenStreetMap, providers$OpenTopoMap)
      prov_obj <- providers[[bm$provider]]
      m <- m %>% leaflet::addProviderTiles(prov_obj, group = grp)
    } else if (!is.null(bm$url)) {
      # Raw XYZ tiles with optional tokens and subdomains
      url <- resolve_tokens(bm$url, bm$token_keys)
      subdomains <- if (!is.null(bm$subdomains)) strsplit(bm$subdomains, ",")[[1]] else NULL
      tile_opts  <- if (is.null(subdomains)) leaflet::tileOptions() else leaflet::tileOptions(subdomains = subdomains)
      m <- m %>% leaflet::addTiles(group = grp, urlTemplate = url, attribution = attr, options = tile_opts)
    }
  }
  
  m
}
