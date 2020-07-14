library(shiny)
library(shinyauthr)
library(shinyjs)
library(datasets)
library(tidyverse)
library(sf)
library(sp)
library(tmaptools)
library(noncensus)
library(leaflet)
library(tools)
library(raster)
library(grid)
library(DT)
library(devtools)
library(noncensus)
data("zip_codes")
library(shinycssloaders)

library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)

library(dashboardthemes)
library(leaflet)
library(leaflet.extras)
library(RMySQL)
library(plotly)
library(heatmaply)
library(shinyHeatmaply)
library(shinyWidgets)


#changes life exp
library("gplots")
library(HMDHFDplus)
library(reshape2)
library(RColorBrewer)


options(HMD_user = "bh2722@columbia.edu")
options(HMD_password = "password999")

logotitle <- shinyDashboardLogoDIY(boldText = "", mainText = "MortalityViz", textSize = 14, badgeText = "BETA",  
                                    badgeTextColor = "black", badgeTextSize = 2, badgeBackColor = "#DCDCDC", badgeBorderRadius = 2)

sysfonts::font_add_google("Roboto Condensed", regular.wt = 400, bold.wt = 700)

theme_custom <- shinyDashboardThemeDIY(
    
    ### general
    appFontFamily = "Roboto Condensed"
    ,appFontColor = "rgb(0,0,0)"
    ,primaryFontColor = "rgb(0,0,0)"
    ,infoFontColor = "rgb(0,0,0)"
    ,successFontColor = "rgb(0,0,0)"
    ,warningFontColor = "rgb(0,0,0)"
    ,dangerFontColor = "rgb(0,0,0)"
    ,bodyBackColor = "rgb(236,242,247)"
    
    ### header
    ,logoBackColor = "rgb(53,64,83)" #mortalityviz background
    
    ,headerButtonBackColor = "rgb(255,255,255)" #white background for lines button 
    ,headerButtonIconColor = "rgb(125,132,145)" #grey three lines 
    ,headerButtonBackColorHover = "rgb(21, 154, 251)" #bright blue
    ,headerButtonIconColorHover = "rgb(0,0,0)" #black hover three lines = black lines
    
    ,headerBackColor = "rgb(255,255,255)"  #rest of header back color = white
    ,headerBoxShadowColor = "rgb(232,232,232)" 
    ,headerBoxShadowSize = "2px 2px 2px"
    
    ### sidebar
    ,sidebarBackColor = "rgb(53,64,83)"
    ,sidebarPadding = 0 #left side selected tab
    
    ,sidebarMenuBackColor = "rgb(53,64,83)" #"transparent" for sidebar collapse menu
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 20
    
    ,sidebarShadowRadius = "2px 2px 2px" #sidebar shadow
    ,sidebarShadowColor = "#aaaaaa"
    
    ,sidebarUserTextColor = "rgb(0,0,0)"
    
    ,sidebarSearchBackColor = "rgb(55,72,80)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "rgb(55,72,80)"
    
    #for unselected sidebars = dark blue shading and grey text
    ,sidebarTabTextColor = "rgb(125,132,145)" #grey text color
    ,sidebarTabTextSize = 14
    ,sidebarTabBorderStyle = "none none none none" #border clockwise from top
    ,sidebarTabBorderColor = ""
    ,sidebarTabBorderWidth = 0
    
    #selected sidebar = darker blue shading, w/ white text
    ,sidebarTabBackColorSelected = "rgb(46,58,72)" #selected even darker blue
    ,sidebarTabTextColorSelected = "rgb(255,255,255)" #white text
    ,sidebarTabRadiusSelected = "0px 0px 0px 0px" #rectangular
    
    #hovering sidebar = darker blue shading w/ grey text
    ,sidebarTabBackColorHover = "rgb(46,58,72)" #hover same as selected = even darker blue
    ,sidebarTabTextColorHover = "rgb(125,132,145)" #but hover not selected = grey text
    ,sidebarTabBorderStyleHover = "none none none solid"
    ,sidebarTabBorderColorHover = "rgb(21, 154, 251)" #bright blue hovering left padding
    ,sidebarTabBorderWidthHover = 5
    ,sidebarTabRadiusHover = "0px 0px 0px 0px"
    
    ### boxes
    ,boxBackColor = "rgb(255,255,255)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 1px 1px"
    ,boxShadowColor = "rgba(0,0,0,.1)"
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(111,125,150)" #grey
    ,boxPrimaryColor = "rgb(71,186,193)" #teal
    ,boxInfoColor = "rgb(21, 154, 251)" #bright blue
    ,boxSuccessColor = "rgb(67, 138, 250)"
    ,boxWarningColor = "rgb(244,156,104)"
    ,boxDangerColor = "rgb(111,125,150)" #grey
    
    ,tabBoxTabColor = "rgb(255,255,255)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(0,0,0)"
    ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
    ,tabBoxBackColor = "rgb(255,255,255)" 
    ,tabBoxHighlightColor = "rgb(21, 154, 251)" #"rgb(111,125,150)" #rim of tab boxes of non seelected
    ,tabBoxBorderRadius = 0
    
    ### inputs
    ,buttonBackColor = "rgb(245,245,245)"
    ,buttonTextColor = "rgb(0,0,0)"
    ,buttonBorderColor = "rgb(200,200,200)"
    ,buttonBorderRadius = 5
    
    ,buttonBackColorHover = "rgb(235,235,235)"
    ,buttonTextColorHover = "rgb(100,100,100)"
    ,buttonBorderColorHover = "rgb(200,200,200)"
    
    ,textboxBackColor = "rgb(255,255,255)"
    ,textboxBorderColor = "rgb(200,200,200)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(245,245,245)"
    ,textboxBorderColorSelect = "rgb(200,200,200)"
    
    ### tables
    ,tableBackColor = "rgb(255,255,255)"
    ,tableBorderColor = "rgb(240,240,240)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
)

convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
        mi$attribs$class=NULL
    }
    mi
}

dbHeader <- dashboardHeaderPlus(title = tagList(
                                        a(href = "javascript:void(window.open('https://google.com', '_blank'))",
                                                         tags$img(src='MortalityViz.png', class = 'logo-lg',
                                                                  style = "float: center",height='45x',width='160px')),
                                        img(class = "logo-mini", src = "https://image.flaticon.com/icons/svg/204/204074.svg")),
                                tags$li(a(href = "javascript:void(window.open('https://github.com/bhsu4', '_blank'))",
                                          icon("github", "fa-1.5x"),
                                          title = "Visit my Github"),
                                          class = "dropdown", 
                                          style="color: #000; background-color: #b3b3b3; 
                                                      font-size:135% ;"),
                                tags$li(a(href = "javascript:void(window.open('https://sps.columbia.edu/academics/masters/actuarial-science', '_blank'))",
                                          img(src = 'ColumbiaLogo.png',
                                              title = "Contact Us", height = "20px", width = "20px")),
                                        class = 'dropdown') 
                            )


ui = dashboardPagePlus(
    
    
    title = "MortalityViz",
    
    # Dashboard Page Setup ----------------------------------------------------
    dbHeader,
    #dashboardHeader(
    ### changing logo
    # title = logo_blue_gradient),
    
    # Dashboard Sidebar -------------------------------------------------------
    dashboardSidebar(

        sidebarUserPanel("Mushu",
                         subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                         # Image file should be in www/ subdir
                         image = "https://fiverr-res.cloudinary.com/t_profile_original,q_auto,f_auto/attachments/profile/photo/a8ece8a3bb5951b6a9ffe9a19063327a-1537916496823/Mushu%20Glasses.jpg"
        ),
        sidebarMenu(
           # id = "tabs",
            #convertMenuItem(
            menuItem("MORTALITY DECOMP", #tabName = "tab_le", 
                         icon = icon("desktop"), startExpanded = TRUE,
                menuSubItem("Overview", tabName = "tab_le", icon = icon("clipboard-list")),
                menuSubItem("Decomposition by Age", tabName = "DecAge", icon = icon("chevron-right")), #icon("hand-holding-heart")),
                menuSubItem("Decomposition by Age/COD", tabName = "DecAgeCOD", icon = icon("chevron-right"))), #icon("hand-holding-usd"))), #, "tab_le"),
            menuItem("ABOUT", tabName = "tab_about", icon = icon("gear")) #info
            
           
        )
    ),
    
    # Dashboard Body ----------------------------------------------------------
    dashboardBody(
    
        tags$script(HTML("$('body').addClass('sidebar-mini');")),
        
        theme_custom,
        
        #tabItems(
        tags$head(tags$style(HTML('.main-sidebar { font-size: 20px; font-weight: bold;}
                        .skin-blue .main-header .logo:hover {  background-color: #47b2ff; }
                        .skin-blue .main-header .navbar {  background-color: "rgb(255,255,255)"; }   ')),
                  #'.main-sidebar {font-weight: bold; font-family: Source Sans Pro
                     # font-size: 30px; ')),
                  
                  tags$link(rel = "shortcut icon", 
                            href = "https://www.columbia.edu/content/themes/custom/columbia/favicon-crown.png"), 
                  ),
        
    tabItems(
        # About - tab_life exp -------------------------------------------------------
        tabItem(
            "tab_le", 
            fluidRow(
                # overview start ------------------------------------------------
                box(
                    title = tags$strong(HTML("<br>&nbsp;&nbsp;&nbsp;DECOMPOSITION DESCRIPTION<br><br>")),
                    status = "danger",
                    width = 6, solidHeader = TRUE,
                    tags$style(HTML('color: #ffffff')),
                    tags$br(),
                    tags$p(
                        class = "text-left",
                        HTML('&nbsp;'), HTML('&nbsp;'), tags$strong(HTML("FUNCTION TITLE")), 
                        HTML('<span style="padding: 0 20px">&nbsp;</span>'), "Decomposition of Mortality by Age", HTML('<hr/>')
                    ),
                    tags$p(
                        class = "text-left",
                        HTML('&nbsp;'), HTML('&nbsp;'), tags$strong(HTML("METRIC")), 
                        HTML('<span style="padding: 0 44px">&nbsp;</span>'), "Change in Life Expectancy, Life Preparancy", HTML('<hr/>')
                    ), 
                    tags$p(
                        class = "text-left",
                        HTML('&nbsp;'), HTML('&nbsp;'), tags$strong(HTML("EXPERIENCE")), 
                        HTML('<span style="padding: 0 30px">&nbsp;</span>'), "Click functionality in barchart", HTML('<hr/>')
                    ),
                    tags$p(
                        HTML('<div style="display:flex; justify-content:space-around; content-align: left; ">
                        <p><b>&nbsp;&nbsp;&nbsp;&nbsp;DESCRIPTION</b><p>
                        <span style = "padding: 0 65px; display:inline-block; word-wrap:break-word;"> The Decomposition of Mortality by Age is responsible of observing the life expectancy changes over a time interval for a select country.</span>
                        </div><hr/>')
                    ),
                    tags$p(
                        HTML('<div style="display:flex; justify-content:space-around; content-align: left; ">
                        <p><b>&nbsp;&nbsp;&nbsp;&nbsp;ERRORS</b><p>

                        <span style = " padding: 0 95px; display:inline-block; word-wrap:break-word;">Australia is working country for all years. Need to figure out why conditional panels dont work for other countries. </span>
                        
                        </div><hr/>'
                        )
                    ),
                    tags$head(tags$style(HTML('.box-header h3.box-title { font-weight: bold; font-size: 18px; color: rgb(0,144,197);} 
                                              p {color: #5e697d}'))) 
                    
                ),
                box(
                    title = tags$strong(HTML("<br>&nbsp;&nbsp;&nbsp;WHAT IS LIFE PREPARANCY?<br><br>")),
                    status = "info",
                    width = 6, solidHeader = TRUE,
                    tags$style(HTML('color: #ffffff')),
                    tags$br(),
                    tags$p(
                        class = "text-left",
                        "A life preparancy age might be commonly defined as the age to which 10 percent of a population 
                        that has already reached age 65 is expected to live in the future.", 
                        blockQuote("[There is] a growing need for the actuarial profession around the world to
                                   promote retirement plans that include lifetime income options....
                                   Longevity risk, however, is a much more delicate issue than simply noting the 
                                   average ages that might be obtained across a population. First, actuaries need 
                                   to change the public's vocabulary to switch from 'expectancy from birth' 
                                   to 'retirement preparedness.'",     
                                   HTML("<br><p style = text-align:right>-<b>Dale Hall, Managing Director of Research at the SOA </p></b>"))
                    ),
                    tags$head(tags$style(HTML('.box-header h3.box-title { font-weight: bold; font-size: 18px; color: rgb(0,144,197);} 
                                              p {color: #5e697d}'))) 
                )
            )
        ),
        tabItem(
            "DecAge", 
            fluidRow(
                column(width = 12,
                           boxPlus(title = "Choose Parameters", closable = FALSE, width = NULL, 
                                   status = "danger", collapsible = TRUE, solidHeader = TRUE, 
                                   enable_dropdown = TRUE, dropdown_icon = "sticky-note", 
                                   dropdown_menu = dropdownItemList(dropdownItem(url = "https://www.demographic-research.org/volumes/vol7/14/7-14.pdf", name = "Decomposition Source"), 
                                                                    dropdownDivider()
                                                                    ), 
                           fluidRow(
                               column(width = 3,
                                   fluidRow(
                                       column(width = 10,
                                        selectInput("heatCountry", "Selected Country", c(Choose = ""), selectize = TRUE)
                                        ), 
                                       column(width = 2, 
                                       actionButton(
                                           inputId = "heatQA",
                                           label = "",
                                           icon = icon("question-circle"),
                                           style="color: #fff; background-color: #b3b3b3; 
                                                  border-color: #b3b3b3; padding:2px; font-size:78% ; 
                                                  width: 20px; height: 20px"
                                       ))
                                       
                                   ),
                                   fluidRow(
                                       column(width = 12,
                                          checkboxGroupButtons(
                                              inputId = "heatGender", label = "Gender", 
                                              choices = c("Male", "Female"), selected = "Male", 
                                              justified = TRUE, status = "primary"))
                                   )
                               ), 
                              column(width = 9, 
                                  # Input: Specification of range within an interval ----
                                   wellPanel( 
                                       conditionalPanel(
                                           condition = "input.heatCountry == 'Australia'",
                                           sliderInput("range_t",
                                                       label = "Years Selected",
                                                       min = 1921, max = 2018, value = c(1921, 2018))
                                       )
                                   ) # wellPanel
                              ) # column
                           
                       

                         ) #fluidrow
                         
                       )
                     ), 
                column(width = 12,
                       
                       tabBox(width = NULL, title = tagList(shiny::icon("calculator"), "By Age"), 
                              tabPanel(title = "Change in Life Expectancy", id = "tabset1", 
                                              
                                       fluidRow(
                                           column(width = 6,
                                                  plotlyOutput("BarplotLE", height = 300)
                                           ), 
                                           column(width = 6, 
                                                  plotlyOutput("BarplotLE_specific", height = 300)
                                           )
                                       ), 
                                       tags$br(), tags$br(),
                                       fluidRow(
                                           column(width = 12, 
                                                  withSpinner(plotlyOutput("HeatMap", height = 600), proxy.height = "20px")
                                           )
                                       )
                              ),
                              tabPanel(title = "Change in Life Preparancy",
                                       
                                        fluidRow(
                                           column(width = 10,
                                                  withSpinner(plotlyOutput("LineLP", height = 400)), 
                                           ), 
                                           column(width = 2, 
                                                  numericInput("z", "Percentile", value = 0.9, min = 0, max = 1, step = 0.1)
                                                  )
                                        
                                        ), 
                                       tags$br(), tags$br(),
                                       fluidRow(
                                           column(width = 12, 
                                                  withSpinner(plotlyOutput("HeatMapLP", height = 600), proxy.height = "20px")
                                           )
                                       )
                              ),
                              tabPanel(title = "Contribution to Gender Gap",
                                       
                                       fluidRow(
                                           column(width = 8,
                                                  withSpinner(plotlyOutput("GapAge", height = 600)) 
                                           ), 
                                           column(width = 4, 
                                                  withSpinner(DT::dataTableOutput("GapAgeTable", width = "100%", height = 400))
                                           )
                                           
                                       ), 
                              )
                       )
                )
            )

        ),
    ##################### -- Start of Decomp. Age + COD -- #########################
        tabItem(
            "DecAgeCOD", 
            fluidRow(
                column(width = 12,
                       boxPlus(title = "Choose Parameters", closable = FALSE, width = NULL, 
                               status = "danger", collapsible = TRUE, solidHeader = TRUE, 
                               enable_dropdown = TRUE, dropdown_icon = "sticky-note", 
                               dropdown_menu = dropdownItemList(dropdownItem(url = "https://www.demographic-research.org/volumes/vol7/14/7-14.pdf", name = "Decomposition Source"), 
                                                                dropdownDivider()
                               ), 
                               fluidRow(
                                   column(width = 3,
                                          fluidRow(
                                              column(width = 6,
                                                     selectInput("CODCountry", "Selected Country", c(Choose = ""), selectize = TRUE)
                                              ), 
                                              column(width = 4, 
                                                     numericInput("CODAge", "Input Age", value = 0, min = 0, max = 105, step = 5)
                                              ),
                                              column(width = 2, 
                                                     actionButton(
                                                         inputId = "CODQA",
                                                         label = "",
                                                         icon = icon("question-circle"),
                                                         style="color: #fff; background-color: #b3b3b3; 
                                                  border-color: #b3b3b3; padding:2px; font-size:78% ; 
                                                  width: 20px; height: 20px"
                                                     ))
                                              
                                          ),
                                          fluidRow(
                                              column(width = 8,
                                                     checkboxGroupButtons(
                                                         inputId = "CODGender", label = "Gender", 
                                                         choices = c("Male", "Female"), selected = "Male", 
                                                         justified = TRUE, status = "primary")
                                              ),
                                              column(width = 4, 
                                                       materialSwitch(
                                                          inputId = "CODAggregate", label = div(style = "margin-top:0px; font-weight: bold ; margin-bottom: -10px", 
                                                                                                 HTML("All Countries<br><br>")), 
                                                          right = FALSE, value = FALSE, status = "primary")
                                              )
                                          )
                                   ), 
                                   column(width = 9, 
                                          # Input: Specification of range within an interval ----
                                          wellPanel( 
                                              conditionalPanel(
                                                  condition = "input.CODAggregate",
                                                  numericInput("tcod_year",
                                                              label = "Year Selected (1950-2015)",
                                                              min = 1950, max = 2015, 
                                                              value = 2000, step = 1)
                                              ),
                                              conditionalPanel(
                                                  condition = "!input.CODAggregate && input.CODCountry == 'Canada'",
                                                  sliderInput("range_tcod",
                                                              label = "Years Selected",
                                                              min = 1950, max = 2009, value = c(1950, 2009))
                                              ),
                                              conditionalPanel(
                                                  condition = "!input.CODAggregate && input.CODCountry == 'Czech Republic'",
                                                  sliderInput("range_tcod",
                                                              label = "Years Selected",
                                                              min = 1950, max = 2013, value = c(1950, 2013))
                                              ),
                                              conditionalPanel(
                                                  condition = "!input.CODAggregate && input.CODCountry == 'France'",
                                                  sliderInput("range_tcod",
                                                              label = "Years Selected",
                                                              min = 1958, max = 2013, value = c(1958, 2013))
                                              ),
                                              conditionalPanel(
                                                  condition = "!input.CODAggregate && input.CODCountry == 'United Kingdom'",
                                                  sliderInput("range_tcod",
                                                              label = "Years Selected",
                                                              min = 1950, max = 2014, value = c(1950, 2014))
                                              ),
                                              conditionalPanel(
                                                  condition = "!input.CODAggregate && input.CODCountry == 'Japan'",
                                                  sliderInput("range_tcod",
                                                              label = "Years Selected",
                                                              min = 1950, max = 2013, value = c(1950, 2013))
                                              ),
                                              conditionalPanel(
                                                  condition = "!input.CODAggregate && input.CODCountry == 'Norway'",
                                                  sliderInput("range_tcod",
                                                              label = "Years Selected",
                                                              min = 1951, max = 2012, value = c(1951, 2012))
                                              ),
                                              conditionalPanel(
                                                  condition = "!input.CODAggregate && input.CODCountry == 'Sweden'",
                                                  sliderInput("range_tcod",
                                                              label = "Years Selected",
                                                              min = 1952, max = 2012, value = c(1952, 2012))
                                              ),
                                              conditionalPanel(
                                                  condition = "!input.CODAggregate && input.CODCountry == 'USA'",
                                                  sliderInput("range_tcod",
                                                              label = "Years Selected",
                                                              min = 1959, max = 2015, value = c(1959, 2015))
                                              )
                                          ) # wellPanel
                                   ) # column
                              ) #fluidrow
                       )
                ), 
                column(width = 12,
                       
                       tabBox(width = NULL, title = tagList(shiny::icon("hand-holding-heart"), "By Age and COD"), 
                              tabPanel(title = "Mortality Chapters by Country", id = "tabset1", 
                                       
                                       fluidRow(
                                           column(width = 12,
                                                 withSpinner(plotlyOutput("Animate_MCBar", height = 500))
                                           
                                       )
                                   )
                              ),
                              tabPanel(title = "Change in Life Expectancy", id = "tabset2", 
                                       
                                       fluidRow(
                                           column(width = 6,
                                                  plotlyOutput("BarplotLE_AgeCOD", height = 300)
                                           ), 
                                           column(width = 6, 
                                                  plotlyOutput("BarplotLE_specificAgeCOD", height = 300)
                                           )
                                       ), 
                                       tags$br(), tags$br(),
                                       fluidRow(
                                           column(width = 12, 
                                                  withSpinner(plotlyOutput("HeatMap_AgeCOD", height = 600), proxy.height = "20px")
                                           )
                                       )
                              )
                       )
                )
            )
            
        ),
                    
        
        # About - tab_about -------------------------------------------------------
        tabItem(
            "tab_about",
            
            
            fluidRow(
                # About - About Me - start ------------------------------------------------
                widgetUserBox(
                    title = "Benjamin Hsu",
                    subtitle = "M.S. in Actuarial Science",
                    type = NULL,
                    width = 6,
                    src = "me.png",
                    background = TRUE,
                    backgroundUrl = "https://cdn.hipwallpaper.com/i/55/63/hYxHpB.jpg",
                    closable = FALSE,
                    HTML("<br>"), HTML("<br>"),
                    tags$strong("Hi! I'm Ben!"), 
                    HTML(paste0("Get in touch with me on LinkedIn (", tags$a(href = "https://www.linkedin.com/in/benjamin-hsu-10b33a97/", "Benjamin Hsu"), "),")),  
                    "online at", HTML(paste0(tags$a(href = "https://benjaminhsu.netlify.com", "benjaminhsu.com", target = "a()"), ",")), 
                    "or by email at bh2722@columbia.edu.",
                    footer = "I am a Master of Science student in Actuarial Science at Columbia University. 
                             I graduated from the University of Rochester with a Bachelor's degree in Statistics 
                             with a Certificate in Actuarial Science. I like to work on projects with statistical 
                             and machine learning. You can find more of my work on my website, or get in touch with 
                             me via LinkedIn or my email above.")
                
                
               
            
                #box(
                #    title = "About me",
                #    status = "danger",
                #    width = 3,
                #    tags$p(
                #        class = "text-center",
                #        tags$img(class = "img-responsive img-rounded center-block", src = "me.png", style = "max-width: 150px;")
                #    ),
                #    tags$p(
                #        class = "text-center",
                #        tags$strong("Hi! I'm Ben!"),
                #        HTML(paste0("(", tags$a(href = "https://www.linkedin.com/in/benjamin-hsu-10b33a97/", "Benjamin Hsu"), ")"))
                #    ),
                #    tags$p(
                #        "I am a Master of Science student in Actuarial Science at Columbia University.",
                #        "I graduated from the University of Rochester with a Bachelor's degree in Statistics",
                #        "with a Certificate in Actuarial Science."
                #    ),
                #    tags$p(
                #        "I like to work on projects with",
                #        "statistical and machine learning. You can find more of my work on my website",
                #        HTML(paste0(tags$a(href = "https://benjaminhsu.netlify.com", "benjaminhsu.com", target = "a()"), "."))
                #    ),
                #    tags$p(
                #        "Get in touch with me on LinkedIn at",
                #        HTML(paste0("(", tags$a(href = "https://www.linkedin.com/in/benjamin-hsu-10b33a97/", "Benjamin Hsu", target = "a()"), "),")),
                #        "online at",
                #        HTML(paste0(tags$a(href = "https://benjaminhsu.netlify.com", "benjaminhsu.com", target = "a()"), ",")),
                #        "or by email at",
                #        HTML(paste0(tags$a(href = "mailto:bh2722@columbia.edu", "bh2722@columbia.edu"), "."))
                #    )
                #)
                )
            )
        )
        
    )    
        
)
    
change5x1 <- function(cntry, t1, t2){
    
    Males_5x1 <- readHMDweb(CNTRY = cntry, item = "mltper_5x1", username = getOption("HMD_user"), password = getOption("HMD_password"))
    Females_5x1 <- readHMDweb(CNTRY = cntry, item = "fltper_5x1", username = getOption("HMD_user"), password = getOption("HMD_password"))
    labels <- levels(cut(unique(Males_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Males_5x1$Age), 5)), right = FALSE))
    labels_m <- paste0(levels(cut(unique(Males_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Males_5x1$Age), 5)), right = FALSE)), "M")
    labels_f <- paste0(levels(cut(unique(Females_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Females_5x1$Age), 5)), right = FALSE)), "F")
    
    #create heatmap labels
    age_interest <- gsub(",.*$", "", gsub("\\[|\\)", "", labels)) #gsub the brackets out, and then take lower value before comma
    age_contribution <- paste("Contribution Age", age_interest)
    age_le <- paste("LE Age", age_interest)
    
    #barplot labels
    age_range <- gsub(",", "-", gsub("\\[|\\)", "", labels)) #dash range labels
    
    #start of calculation
    lx_matrix_M<-dcast(Males_5x1,Year~Age,value.var = "lx")
    ex_matrix_M<-dcast(Males_5x1,Year~Age,value.var = "ex")
    lx_matrix_F<-dcast(Females_5x1,Year~Age,value.var = "lx")
    ex_matrix_F<-dcast(Females_5x1,Year~Age,value.var = "ex")
    age_groups<-dim(lx_matrix_M)[2]-1
    
    ## Definition of vectors to be used
    changes_per_age_M<-matrix(0,nrow=age_groups-1,ncol=age_groups-1)
    changes_per_age_F<-matrix(0,nrow=age_groups-1,ncol=age_groups-1)
    changes_per_age_MF <- matrix(0, nrow = age_groups, ncol = age_groups)
    
    for(k in 1:(age_groups-1)){
        d_x_12_M<-matrix(0,nrow=age_groups,ncol=1)
        d_x_21_M<-matrix(0,nrow=age_groups,ncol=1)
        d_x_12_F<-matrix(0,nrow=age_groups,ncol=1)
        d_x_21_F<-matrix(0,nrow=age_groups,ncol=1)
        ##we redefine the vectors lx by dividing them by the respective l0
        ##Case of males
        lx_t1_M<-lx_matrix_M[lx_matrix_M$Year==t1,-1]/lx_matrix_M[lx_matrix_M$Year==t1,k+1]
        lx_t2_M<-lx_matrix_M[lx_matrix_M$Year==t2,-1]/lx_matrix_M[lx_matrix_M$Year==t2,k+1]
        ex_t1_M<-ex_matrix_M[ex_matrix_M$Year==t1,-1]
        ex_t2_M<-ex_matrix_M[ex_matrix_M$Year==t2,-1]
        ##Case of females
        lx_t1_F<-lx_matrix_F[lx_matrix_F$Year==t1,-1]/lx_matrix_F[lx_matrix_F$Year==t1,k+1]
        lx_t2_F<-lx_matrix_F[lx_matrix_F$Year==t2,-1]/lx_matrix_F[lx_matrix_F$Year==t2,k+1]
        ex_t1_F<-ex_matrix_F[ex_matrix_F$Year==t1,-1]
        ex_t2_F<-ex_matrix_F[ex_matrix_F$Year==t2,-1]
        
        
        #<-dim(lx_t1)[2]
        for(x in k:(age_groups-1)){##
            ##Calculation for males
            d_x_12_M[x]<-as.double(lx_t1_M[x])*(as.double(ex_t1_M[x])-as.double(ex_t2_M[x]))-as.double(lx_t1_M[x+1])*(as.double(ex_t1_M[x+1])-as.double(ex_t2_M[x+1]))
            d_x_21_M[x]<-as.double(lx_t2_M[x])*(as.double(ex_t2_M[x])-as.double(ex_t1_M[x]))-as.double(lx_t2_M[x+1])*(as.double(ex_t2_M[x+1])-as.double(ex_t1_M[x+1]))  
            changes_per_age_M[x,k]<-0.5*(d_x_21_M[x]-d_x_12_M[x])
            ##calculation for females
            d_x_12_F[x]<-as.double(lx_t1_F[x])*(as.double(ex_t1_F[x])-as.double(ex_t2_F[x]))-as.double(lx_t1_F[x+1])*(as.double(ex_t1_F[x+1])-as.double(ex_t2_F[x+1]))
            d_x_21_F[x]<-as.double(lx_t2_F[x])*(as.double(ex_t2_F[x])-as.double(ex_t1_F[x]))-as.double(lx_t2_F[x+1])*(as.double(ex_t2_F[x+1])-as.double(ex_t1_F[x+1]))  
            changes_per_age_F[x,k]<-0.5*(d_x_21_F[x]-d_x_12_F[x])
        }
    }

    rownames(changes_per_age_F) <- age_contribution
    colnames(changes_per_age_F) <- age_le
    rownames(changes_per_age_M) <- age_contribution
    colnames(changes_per_age_M) <- age_le
    
    both_genders = cbind(changes_per_age_M, changes_per_age_F)
    total_change_in_age <- apply(both_genders, 2, sum)
    res <- rbind(both_genders, total_change_in_age)
    #add labels
    colnames(res) <- c(labels_m, labels_f)
    row.names(res) <- c(labels, "Total")
    ##The following two quantities are used for a plot
    total_male <- apply(changes_per_age_M, 2, sum)
    total_female <- apply(changes_per_age_F, 2, sum)
    #creating a matrix w/ M/F
    changes_per_age_MF[lower.tri(changes_per_age_MF)] <- changes_per_age_M[lower.tri(changes_per_age_M, diag = TRUE)]
    changes_per_age_MF[upper.tri(changes_per_age_MF)] <- t(changes_per_age_F)[upper.tri(t(changes_per_age_F), diag = TRUE)]
    diag(changes_per_age_MF) <- NA
    rownames(changes_per_age_MF) <- c("", age_contribution)
    colnames(changes_per_age_MF) <- c(age_le, " ")
    
    
    ##Removal of the upper part of matrix since it is always zero
    changes_per_age_M[upper.tri(changes_per_age_M)] <- NA
    changes_per_age_F[upper.tri(changes_per_age_F)] <- NA
    
    
    return(list(changes_per_age_M, changes_per_age_F, changes_per_age_MF))
}

create_hover_MF <- function(x){
    my_rep <- vector() ; my_rep2 <- vector()
    obs_length <- length(x)
    for(k in obs_length:1){
        times <- obs_length - k
        if(k == obs_length){ curr_rep <- c(rep(rev(x)[k], k)) }
        else{ curr_rep <- c(rep(rev(x)[k+1], times), rep(rev(x)[k], k)) }
        my_rep = c(my_rep, curr_rep)
    }
    return(my_rep)
}

change5x1_LP <- function(cntry, t1, t2, z){
    #CLP calculation
    
    Males_5x1 <- readHMDweb(CNTRY = cntry, item = "mltper_5x1", username = getOption("HMD_user"), password = getOption("HMD_password"))
    Females_5x1 <- readHMDweb(CNTRY = cntry, item = "fltper_5x1", username = getOption("HMD_user"), password = getOption("HMD_password"))
    labels <- levels(cut(unique(Males_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Males_5x1$Age), 5)), right = FALSE))
    labels_m <- paste0(levels(cut(unique(Males_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Males_5x1$Age), 5)), right = FALSE)), "M")
    labels_f <- paste0(levels(cut(unique(Females_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Females_5x1$Age), 5)), right = FALSE)), "F")
    
    #age group equivalence
    age_interest <- gsub(",.*$", "", gsub("\\[|\\)", "", labels)) #gsub the brackets out, and then take lower value before comma
    age_contribution <- paste("Contribution Age", age_interest)
    age_lp <- paste("LP Age", age_interest)
    age_range <- gsub(",", "-", gsub("\\[|\\)", "", labels)) #dash range labels
    age_group <- 1:length(age_interest)
    age_group_equiv <- data.frame(Age_Label = age_range, Age_Group = age_group, Initial_Age = age_interest)
    
    lx_matrix_M<-dcast(Males_5x1,Year~Age,value.var = "lx")
    lx_matrix_F<-dcast(Females_5x1,Year~Age,value.var = "lx")
    
    initial_ages <- as.matrix(as.numeric(as.character(age_group_equiv$Initial_Age)))
    ages<-(dim(lx_matrix_M[,-1])[2])-1##ages is the number of ages present in the life table, per year
    life_preparancy_female_t1<-matrix(0,nrow=ages,ncol=1)
    life_preparancy_female_t2<-matrix(0,nrow=ages,ncol=1)
    life_preparancy_male_t1<-matrix(0,nrow=ages,ncol=1)
    life_preparancy_male_t2<-matrix(0,nrow=ages,ncol=1)
    survivors_male_t1<-matrix(0,nrow=1,ncol=ages)
    survivors_male_t2<-matrix(0,nrow=1,ncol=ages)
    survivors_female_t1<-matrix(0,nrow=1,ncol=ages)
    survivors_female_t2<-matrix(0,nrow=1,ncol=ages)
    d_x_12_M<-matrix(0,nrow=ages,ncol=1)
    d_x_21_M<-matrix(0,nrow=ages,ncol=1)
    d_x_12_F<-matrix(0,nrow=ages,ncol=1)
    d_x_21_F<-matrix(0,nrow=ages,ncol=1)
    changes_per_age_M<-matrix(0,nrow=ages,ncol=1)
    changes_per_age_F<-matrix(0,nrow=ages,ncol=1)
    
    ##we redine the vectors lx for the years of interest by dividing them by the respective l0
    ##Case of males
    lx_t1_M<-lx_matrix_M[lx_matrix_M$Year==t1,-1]/lx_matrix_M[lx_matrix_M$Year==t1,2]
    lx_t2_M<-lx_matrix_M[lx_matrix_M$Year==t2,-1]/lx_matrix_M[lx_matrix_M$Year==t2,2]
    ##no need for group 110+ since causes of death ends with 105+
    lx_t1_M<-as.matrix(lx_t1_M[,-ncol(lx_t1_M)])
    lx_t2_M<-as.matrix(lx_t2_M[,-ncol(lx_t2_M)])
    ##Case of females
    lx_t1_F<-lx_matrix_F[lx_matrix_F$Year==t1,-1]/lx_matrix_F[lx_matrix_F$Year==t1,2]
    lx_t2_F<-lx_matrix_F[lx_matrix_F$Year==t2,-1]/lx_matrix_F[lx_matrix_F$Year==t2,2]
    ##no need for group 110+ since causes of death ends with 105+
    lx_t1_F<-as.matrix(lx_t1_F[,-ncol(lx_t1_F)])
    lx_t2_F<-as.matrix(lx_t2_F[,-ncol(lx_t2_F)])
    
    ##We'll compute the amount of survivors considered necessary to define the life preparancy per age
    survivors_male_t1[1,]<-as.matrix((1-z)*lx_t1_M)##survivors used as reference for life preparancy males
    survivors_male_t2[1,]<-as.matrix((1-z)*lx_t2_M)
    survivors_female_t1[1,]<-as.matrix((1-z)*lx_t1_F)##survivors used as reference for life preparanacy female
    survivors_female_t2[1,]<-as.matrix((1-z)*lx_t2_F)
    ##We now compute the lifepreparancy per age for every age and the years of interest
    for(x in 1:(ages)){
        #This counts the last position of the survivor vector that exceeds the required number of 
        #survivors based on the percentile
        cm_t1<-lx_t1_M<survivors_male_t1[x]
        cm_t2<-lx_t2_M<survivors_male_t2[x]
        cf_t1<-lx_t1_F<survivors_female_t1[x]
        cf_t2<-lx_t2_F<survivors_female_t2[x]
        ###Interpolation to obtain the life preparancy year t1 males
        t1_d1_M <- lx_t1_M[length(which(cm_t1 == FALSE))] - survivors_male_t1[x]
        t1_d2_M <- survivors_male_t1[x] - lx_t1_M[min(length(which(cm_t1 == FALSE))+1, ages)]
        t1_d_M <- t1_d1_M + t1_d2_M
        life_preparancy_male_t1[x] <- as.double((t1_d2_M/t1_d_M)*initial_ages[length(which(cm_t1 == FALSE))] + (t1_d1_M/t1_d_M)*initial_ages[min(length(which(cm_t1 == FALSE))+1, ages)])
        ###Interpolation to obtain the life preparancy t1 females
        t1_d1_F<-lx_t1_F[(length(which(cf_t1=="FALSE")))]-survivors_female_t1[x]
        t1_d2_F<-survivors_female_t1[x]-lx_t1_F[min((length(which(cf_t1=="FALSE")))+1,ages)]
        t1_d_F<-t1_d1_F+t1_d2_F
        life_preparancy_female_t1[x]<-as.double(( t1_d2_F/t1_d_F)*initial_ages[((length(which(cf_t1=="FALSE"))))]+(t1_d1_F/t1_d_F)*initial_ages[min(((length(which(cf_t1=="FALSE"))))+1,ages)])
        ###Interpolation to obtain the life preparancy year t2 males
        t2_d1_M<-lx_t2_M[(length(which(cm_t2=="FALSE")))]-survivors_male_t2[x]
        t2_d2_M<-survivors_male_t2[x]-lx_t2_M[min((length(which(cm_t2=="FALSE")))+1,ages)]
        t2_d_M<-t2_d1_M+t2_d2_M
        life_preparancy_male_t2[x]<-as.double((t2_d2_M/t2_d_M)*initial_ages[(length(which(cm_t2=="FALSE")))]+(t2_d1_M/t2_d_M)*initial_ages[min(((length(which(cm_t2=="FALSE"))))+1,ages)])
        ###Interpolation to obtain the life preparancy year t2 females
        t2_d1_F<-lx_t2_F[(length(which(cf_t2=="FALSE")))]-survivors_female_t2[x]
        t2_d2_F<-survivors_female_t2[x]-lx_t2_F[min((length(which(cf_t2=="FALSE")))+1,ages)]
        t2_d_F<-t2_d1_F+t2_d2_F
        life_preparancy_female_t2[x]<-as.double((t2_d2_F/t2_d_F)*initial_ages[((length(which(cf_t2=="FALSE"))))]+(t2_d1_F/t2_d_F)*initial_ages[min(((length(which(cf_t2=="FALSE"))))+1,ages)])
    }
    ##When estimating life preparancy requires the number of survivors at x=111, formula returns na
    ##We assign the maximum age in this case, since no one is assumed to survive beyond age 110
    life_preparancy_male_t1[is.na(life_preparancy_male_t1)]<-initial_ages[nrow(initial_ages)]
    life_preparancy_male_t2[is.na(life_preparancy_male_t2)]<-initial_ages[nrow(initial_ages)]
    life_preparancy_female_t1[is.na(life_preparancy_female_t1)]<-initial_ages[nrow(initial_ages)]
    life_preparancy_female_t2[is.na(life_preparancy_female_t2)]<-initial_ages[nrow(initial_ages)]
    
    result <- data.frame(initial = initial_ages, 
                         malet1 = life_preparancy_male_t1, femalet1 = life_preparancy_female_t1, 
                         malet2 = life_preparancy_male_t2, femalet2 = life_preparancy_female_t2)
    row.names(result) <- age_range
    
    #start of change in life preparancy
    
    age_groups <- length(age_group)
    ## Definition of vectors to be used
    changes_per_age_M<-matrix(0,nrow=age_groups-1,ncol=age_groups-1)
    changes_per_age_F<-matrix(0,nrow=age_groups-1,ncol=age_groups-1)
    changes_per_age_MF <- matrix(0, nrow = age_groups, ncol = age_groups)
    
    for(k in 0:(age_groups-1)){
        d_x_12_M<-matrix(0,nrow=age_groups,ncol=1)
        d_x_21_M<-matrix(0,nrow=age_groups,ncol=1)
        d_x_12_F<-matrix(0,nrow=age_groups,ncol=1)
        d_x_21_F<-matrix(0,nrow=age_groups,ncol=1)
        ##we redefine the vectors lx by dividing them by the respective l0
        ##Case of males
        lx_t1_M<-lx_matrix_M[lx_matrix_M$Year==t1,-1]/lx_matrix_M[lx_matrix_M$Year==t1,k+1]
        lx_t2_M<-lx_matrix_M[lx_matrix_M$Year==t2,-1]/lx_matrix_M[lx_matrix_M$Year==t2,k+1]
        ##Case of females
        lx_t1_F<-lx_matrix_F[lx_matrix_F$Year==t1,-1]/lx_matrix_F[lx_matrix_F$Year==t1,k+1]
        lx_t2_F<-lx_matrix_F[lx_matrix_F$Year==t2,-1]/lx_matrix_F[lx_matrix_F$Year==t2,k+1]
        
        for(x in k:(age_groups-1)){##
            ##Calculation for males
            d_x_12_M[x]<-as.double(lx_t1_M[x])*(as.double(result[x,1])-as.double(result[x,3]))-as.double(lx_t1_M[x+1])*(as.double(result[x+1,1])-as.double(result[x+1,3]))
            d_x_21_M[x]<-as.double(lx_t2_M[x])*(as.double(result[x,3])-as.double(result[x,1]))-as.double(lx_t2_M[x+1])*(as.double(result[x+1,3])-as.double(result[x+1,1]))  
            changes_per_age_M[x,k]<-0.5*(d_x_21_M[x]-d_x_12_M[x])
            ##calculation for females
            d_x_12_F[x]<-as.double(lx_t1_F[x])*(as.double(result[x,2])-as.double(result[x,4]))-as.double(lx_t1_F[x+1])*(as.double(result[x+1,2])-as.double(result[x+1,4]))
            d_x_21_F[x]<-as.double(lx_t2_F[x])*(as.double(result[x,4])-as.double(result[x,2]))-as.double(lx_t2_F[x+1])*(as.double(result[x+1,4])-as.double(result[x+1,2]))  
            changes_per_age_F[x,k]<-0.5*(d_x_21_F[x]-d_x_12_F[x])
        }
    }
    
    rownames(changes_per_age_F)<-age_contribution[-length(age_contribution)]
    colnames(changes_per_age_F)<-age_lp[-length(age_lp)]
    rownames(changes_per_age_M)<-age_contribution[-length(age_contribution)]
    colnames(changes_per_age_M)<-age_lp[-length(age_lp)]
    
    both_genders<-cbind(changes_per_age_M,changes_per_age_F)
    total_change_in_Age <- apply(both_genders,2,sum)
    res <- rbind(both_genders,total_change_in_Age)
    colnames(res) <- c(labels_m[-length(labels_m)], labels_f[-length(labels_f)])
    row.names(res) <- c(labels[-length(labels)], "Total")
    
    #creating a matrix w/ M/F
    changes_per_age_MF[lower.tri(changes_per_age_MF)] <- changes_per_age_M[lower.tri(changes_per_age_M, diag = TRUE)]
    changes_per_age_MF[upper.tri(changes_per_age_MF)] <- t(changes_per_age_F)[upper.tri(t(changes_per_age_F), diag = TRUE)]
    diag(changes_per_age_MF) <- NA
    rownames(changes_per_age_MF) <- c("", age_contribution[-length(age_contribution)])
    colnames(changes_per_age_MF) <- c(age_lp[-length(age_lp)], " ")
    
    ##Removal of the upper part of matrix since it is always zero
    changes_per_age_M[upper.tri(changes_per_age_M)] <- NA
    changes_per_age_F[upper.tri(changes_per_age_F)] <- NA
    
    return(list(result, changes_per_age_M, changes_per_age_F, changes_per_age_MF))
}


GenGap_Age <-function(cntry, t){
    ##########
    
    Males_5x1 <- readHMDweb(CNTRY = cntry, item = "mltper_5x1", username = getOption("HMD_user"), password = getOption("HMD_password"))
    Females_5x1 <- readHMDweb(CNTRY = cntry, item = "fltper_5x1", username = getOption("HMD_user"), password = getOption("HMD_password"))
    labels <- levels(cut(unique(Males_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Males_5x1$Age), 5)), right = FALSE))
    labels_m <- paste0(levels(cut(unique(Males_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Males_5x1$Age), 5)), right = FALSE)), "M")
    labels_f <- paste0(levels(cut(unique(Females_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Females_5x1$Age), 5)), right = FALSE)), "F")
    
    #create heatmap labels
    age_interest <- gsub(",.*$", "", gsub("\\[|\\)", "", labels)) #gsub the brackets out, and then take lower value before comma
    age_range <- gsub(",", "-", gsub("\\[|\\)", "", labels)) #dash range labels
    
    ##In here, F stands for Females, M for males
    lx_matrix_M<<-dcast(Males_5x1,Year~Age,value.var = "lx")
    ex_matrix_M<<-dcast(Males_5x1,Year~Age,value.var = "ex")
    lx_matrix_F<<-dcast(Females_5x1,Year~Age,value.var = "lx")
    ex_matrix_F<<-dcast(Females_5x1,Year~Age,value.var = "ex")
    age_groups<-dim(lx_matrix_M[,-1])[2]-1
    ## Definition of vectors to be used
    Total_Change_per_age<-matrix(0,nrow=age_groups,ncol=age_groups)
    #Extraction of information associated to variables of interest for year t
    #First column contains the year, thus it is removed
    lx_M_general<-lx_matrix_M[lx_matrix_M$Year==t,-1]
    ex_M_general<-ex_matrix_M[ex_matrix_M$Year==t,-1]
    lx_F_general<-lx_matrix_F[lx_matrix_F$Year==t,-1]
    ex_F_general<-ex_matrix_F[ex_matrix_F$Year==t,-1]
    ######
    for(k in 0:(age_groups-1)){
        d_x_12<-matrix(0,nrow=age_groups,ncol=1)
        d_x_21<-matrix(0,nrow=age_groups,ncol=1)
        ##we redefine the vectors lx by dividing them by the respective l0
        ##Case of males
        lx_M<-lx_M_general/lx_matrix_M[lx_matrix_M$Year==t,k+1]
        ex_M<-ex_M_general
        ##Case of females
        lx_F<-lx_F_general/lx_matrix_F[lx_matrix_F$Year==t,k+1]
        ex_F<-ex_F_general
        
        for(x in k:(age_groups-1)){##
            ##Calculation algorithm From Andreev
            d_x_12[x]<-as.double(lx_M[x])*(as.double(ex_M[x])-as.double(ex_F[x]))-as.double(lx_M[x+1])*(as.double(ex_M[x+1])-as.double(ex_F[x+1]))
            d_x_21[x]<-as.double(lx_F[x])*(as.double(ex_F[x])-as.double(ex_M[x]))-as.double(lx_F[x+1])*(as.double(ex_F[x+1])-as.double(ex_M[x+1]))
            Total_Change_per_age[x,k]<-0.5*(d_x_21[x]-d_x_12[x])
        }
    }      
    #######
    
    Total_Gap<-apply(Total_Change_per_age,2,sum)
    result_WT<-rbind(Total_Change_per_age,Total_Gap)
    rownames(result_WT)<-c(age_interest, "Total")
    colnames(result_WT)<-age_interest
    
    Total_Change_per_age_df <- data.frame(age_interest, t(Total_Change_per_age))
    colnames(Total_Change_per_age_df) <- c("age_interest", age_interest)
    Total_Change_per_age_melt <- melt(Total_Change_per_age_df, id.vars = c("age_interest"))
    
    
    # create table for this
    Total_Change_per_age_df2 <- data.frame(Total_Change_per_age)
    colnames(Total_Change_per_age_df2) <- paste("Age", age_interest)
    rownames(Total_Change_per_age_df2) <- age_range
    Total_Change_per_age_df2 <- Total_Change_per_age_df2 %>% rownames_to_column() %>% rename(Contribution = rowname)
    Total_Change_per_age_melt2 <- melt(Total_Change_per_age_df2, id.vars = c("Contribution"))
    table_change <- Total_Change_per_age_melt2[!(Total_Change_per_age_melt2$value == 0),] %>% 
                        mutate_if(is.numeric, round, 5) %>% rename("Age Group" = variable, Value = value) %>% 
                        select("Age Group", Contribution, Value) %>% mutate_at(vars(Contribution), factor)
    
    #final output
    return(list(table_change, Total_Change_per_age_melt))
}

change5x1_AgeCOD <- function(cntry, t1, t2){
    
    Males_5x1 <- readHMDweb(CNTRY = cntry, item = "mltper_5x1", username = getOption("HMD_user"), password = getOption("HMD_password"))
    Females_5x1 <- readHMDweb(CNTRY = cntry, item = "fltper_5x1", username = getOption("HMD_user"), password = getOption("HMD_password"))
    labels <- levels(cut(unique(Males_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Males_5x1$Age), 5)), right = FALSE))
    labels_m <- paste0(levels(cut(unique(Males_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Males_5x1$Age), 5)), right = FALSE)), "M")
    labels_f <- paste0(levels(cut(unique(Females_5x1$Age), breaks = c(0, 1, 5, seq(10, max(Females_5x1$Age), 5)), right = FALSE)), "F")
    
    #create heatmap labels
    age_interest <- gsub(",.*$", "", gsub("\\[|\\)", "", labels)) #gsub the brackets out, and then take lower value before comma
    age_range <- gsub(",", "-", gsub("\\[|\\)", "", labels)) #dash range labels
    age_equivalence <- data.frame(initial = age_interest, range = age_range)
    
    ###We reshape the lx, ex information downloaded in a more convinient way
    lx_matrix_M<-dcast(Males_5x1,Year~Age,value.var = "lx")
    ex_matrix_M<-dcast(Males_5x1,Year~Age,value.var = "ex")
    lx_matrix_F<-dcast(Females_5x1,Year~Age,value.var = "lx")
    ex_matrix_F<-dcast(Females_5x1,Year~Age,value.var = "ex")
    
    ####We reshape the rates per disease info in a more convenient way
    COD_Info <- read.csv(paste0("COD_5x1_chapters_", cntry, ".csv"))
    rates_per_chapter_males<-dcast(COD_Info,Year+COD.chap~Age,value.var = "Rates.M")
    rates_per_chapter_females<-dcast(COD_Info,Year+COD.chap~Age,value.var = "Rates.F")
    
    ##We'll obtain the amounts corresponding to exposed to risk for males and females (all ages)
    ##in the years of interest
    rates_per_chapter_males_t1<-as.matrix(rates_per_chapter_males[rates_per_chapter_males$Year==t1,-(1:2)])/1000
    rates_per_chapter_males_t2<-as.matrix(rates_per_chapter_males[rates_per_chapter_males$Year==t2,-(1:2)])/1000
    rates_per_chapter_females_t1<-as.matrix(rates_per_chapter_females[rates_per_chapter_females$Year==t1,-(1:2)])/1000
    rates_per_chapter_females_t2<-as.matrix(rates_per_chapter_females[rates_per_chapter_females$Year==t2,-(1:2)])/1000
    
    mortality_chapters<-nrow(rates_per_chapter_males_t1)-1
    age_groups<- length(age_interest) #dim(rates_per_chapter_males[,-(1:2)])[2]
    
    #20 mortality chapters, and 23 age groups -- 21 is the total 
    
    for(k in 1:(mortality_chapters+1)){
        ##Detection of last column with non NA values. "lcgi" stands for last column gender year i 
        lcm1 <- dim(rates_per_chapter_males_t1)[2]-length(rates_per_chapter_males_t1[k,which(is.na(rates_per_chapter_males_t1[k,]))])
        lcm2<-dim(rates_per_chapter_males_t2)[2]-length(rates_per_chapter_males_t2[k,which(is.na(rates_per_chapter_males_t2[k,]))])
        lcf1<-dim(rates_per_chapter_females_t1)[2]-length(rates_per_chapter_females_t1[k,which(is.na(rates_per_chapter_females_t1[k,]))])
        lcf2<-dim(rates_per_chapter_females_t2)[2]-length(rates_per_chapter_females_t2[k,which(is.na(rates_per_chapter_females_t2[k,]))])
        ###replacement of na 
        rates_per_chapter_males_t1[k,is.na(rates_per_chapter_males_t1[k,])]<-rates_per_chapter_males_t1[k,lcm1]
        rates_per_chapter_males_t2[k,is.na(rates_per_chapter_males_t2[k,])]<-rates_per_chapter_males_t2[k,lcm2]
        rates_per_chapter_females_t1[k,is.na(rates_per_chapter_females_t1[k,])]<-rates_per_chapter_females_t1[k,lcf1]
        rates_per_chapter_females_t2[k,is.na(rates_per_chapter_females_t2[k,])]<-rates_per_chapter_females_t2[k,lcf2]
    }
    
    ##Every column of matrix will represent an age group and every row a chapter of cause of death
    Proportion_Change_rate_of_mortality_t1_t2_male<-matrix(0,nrow=20,ncol=age_groups)
    Proportion_Change_rate_of_mortality_t1_t2_female<-matrix(0,nrow=20,ncol=age_groups)
    Contribution_chapter_male<-matrix(0,nrow=20,ncol=age_groups)
    Contribution_chapter_female<-matrix(0,nrow=20,ncol=age_groups)
    Change_rate_of_mortality_t1_t2_male<-as.matrix(rates_per_chapter_males_t1[21,])-as.matrix(rates_per_chapter_males_t2[21,])
    Change_rate_of_mortality_t1_t2_female<-as.matrix(rates_per_chapter_females_t1[21,])-as.matrix(rates_per_chapter_females_t2[21,])
    
    ##we compute the respective PROPORTIONAL changes in mortality
    for(i in 1:mortality_chapters){
        Proportion_Change_rate_of_mortality_t1_t2_male[i,]<-(rates_per_chapter_males_t1[i,]-rates_per_chapter_males_t2[i,])/t(Change_rate_of_mortality_t1_t2_male)
        Proportion_Change_rate_of_mortality_t1_t2_female[i,]<-(rates_per_chapter_females_t1[i,]-rates_per_chapter_females_t2[i,])/t(Change_rate_of_mortality_t1_t2_female)
    }
    ###some NA may be generated in the results when dividing by a rate that is 0. We remove them 
    Proportion_Change_rate_of_mortality_t1_t2_male[is.na(Proportion_Change_rate_of_mortality_t1_t2_male)]<-0
    Proportion_Change_rate_of_mortality_t1_t2_female[is.na(Proportion_Change_rate_of_mortality_t1_t2_female)]<-0
    
    #output
    return(list(Proportion_Change_rate_of_mortality_t1_t2_male, Proportion_Change_rate_of_mortality_t1_t2_female))
}


server <- shinyServer(function(input, output, session){ 
    
    
        #options(warn = -1) 
    
        observeEvent(input$heatQA, {
            show_alert(
                title = NULL,
                text = tags$span(
                    tags$h3("Decomposition of Mortality by Age",
                            style = "color: steelblue;"), tags$br(),
                    "Select country and time period along with gender", 
                    tags$br(), tags$br(),
                    "If Aggregate", tags$b("OFF"), "then male and female decomposition will be performed separately",
                    tags$br(), tags$br(),
                    "If Aggregate", tags$b("ON"), "both genders then population (male + female) decomposition will be performed",
                    tags$br(), tags$br(),
                    icon("far fa-smile")
                ),
                html = TRUE
            )
        })
    
        country_info <- reactive({
            read.csv("List_Countries.csv")
        })
    
        res <- reactive({
            req(input$heatCountry)
            chosen_country <- as.character(country_info()$Code[which(country_info()$Country == input$heatCountry)])
            res <- change5x1(chosen_country, input$range_t[1], input$range_t[2])
            return(res)
        })
        
        observe({
            updateSelectInput(session, inputId = 'heatCountry', label = 'Selected Country',
                              choices = unique(country_info()$Country), 
                              selected = input$heatCountry)
        })
        
        country_years <- reactive({
            chosen_country <- as.character(country_info()$Code[which(country_info()$Country == input$heatCountry)])
            males5x1 <- readHMDweb(CNTRY = chosen_country, item = "mltper_5x1", username = getOption("HMD_user"), password = getOption("HMD_password"))
            return(males5x1$Year)
        })
        
        #heatmap
        output$HeatMap <- renderPlotly({
            chosen_country <- as.character(country_info()$Code[which(country_info()$Country == input$heatCountry)])
            scale_colors <- brewer.pal(n=9, name = "YlOrRd") #selection of
            
            if (length(input$heatGender) == 1){
                if (input$heatGender == "Male" ){ res_gender <- res()[[1]] }
                else if (input$heatGender == "Female" ){ res_gender <- res()[[2]]}

                par(mar=c(5.1,2.1,1,2.1))
                dim1 <- dim(res_gender)[[1]]
                hover_text <- matrix(paste0((sapply(colnames(res_gender), function(x) rep(x, dim1))), "<br>", 
                                            rep(rownames(res_gender), dim1), "<br>", 
                                            rep(input$heatGender, dim1*2), "<br>"),
                                     byrow = FALSE, ncol = dim1)
                hover_text2 <- matrix(paste(hover_text, round(res_gender, 4), sep="Change: "), dim1, dim1)
                
                heatmaply(res_gender, dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
                          cexRow = 0.9, cexCol = 0.9, col = scale_colors,  
                          plot_method = c("plotly"), main = paste0("Changes in Life Expectancy (", 
                                                                    input$range_t[1], "-", input$range_t[2], ", ",
                                                                    input$heatGender, ", ", input$heatCountry, ")"), 
                          font = list(size = 8), custom_hovertext = hover_text2, 
                          key.title = "Changes in Years", colorbar_xpos = 30, colorbar_ypos = 10) %>% 
                    layout(xaxis = list(ticktext = as.numeric(gsub("LE Age ", "", colnames(res_gender))), title = "Age", 
                                        showgrid = F, tickangle = 0, showticklabels = TRUE), 
                           yaxis = list(ticktext = as.numeric(gsub("Contribution Age ", "", rev(rownames(res_gender)))),
                                        title = "Contribution", showgrid = F, showticklabels = TRUE))
            
            }
            else{ 
                #if (input$heatAggregate == "FALSE"){ 
                    res_gender <- res()[[3]]
                    par(mar=c(5.1,2.1,1,2.1))
                    dim1 <- dim(res_gender)[[1]]
                 
                    #create hover text 
                    row_contr <- create_hover_MF(c(rownames(res_gender)[-1], " ")) #byrow
                    col_le <- create_hover_MF(colnames(res_gender)) #bycol
                  
                    mat1 <- matrix(row_contr, byrow = TRUE, ncol = dim1)
                    mat2 <- matrix(col_le, byrow = FALSE, ncol = dim1)
                    mat3 <- ifelse(lower.tri(mat2), "Male", "Female")
                  
                    mat4 <- matrix(paste(paste0(mat1, "<br>", mat2, "<br>", mat3, "<br>"), 
                                       round(res_gender, 4), sep = "Change: "), dim1, dim1)
                    diag(mat4) <- NA #remove diag hover text
                  
                    heatmaply(res_gender, dendrogram = "none", Rowv = FALSE, Colv = FALSE, #sepwidth=c(1.5, 1.5),
                            cexRow = 0.65, cexCol = 0.65, 
                            col = scale_colors, xlab = "", ylab = "", plot_method = c("plotly"),
                            main = paste0("Changes in Life Expectancy (", 
                                          input$range_t[1], "-", input$range_t[2], ", ",
                                          paste(input$heatGender, collapse = "/"), ", ", input$heatCountry, ")"),
                            font = list(size = 8), custom_hovertext = mat4,
                            key.title = "Changes in Years", 
                            colorbar_xpos = 30, colorbar_ypos = 10) %>% 
                            layout(shapes = list(type = 'line', x0 = 0, x1 = 25, y0 =25, y1 = 0, line = list(width = 1.5)),
                                   xaxis = list(title = "Age", showgrid = F, showticklabels = FALSE), 
                                   yaxis = list(title = "Contribution", showgrid = F, showticklabels = FALSE))
                #}
               # else {
               #    res_gender <- res()[[1]] + res()[[2]] 
               #    par(mar=c(5.1,2.1,1,2.1))
               #    dim1 <- dim(res_gender)[[1]]
               #    hover_text <- matrix(paste0((sapply(colnames(res_gender), function(x) rep(x, dim1))), "<br>", 
               #                                 rep(rownames(res_gender), dim1), "<br>", 
               #                                 rep("Aggregate", dim1*2), "<br>"),
               #                          byrow = FALSE, ncol = dim1)
               #    hover_text2 <- matrix(paste(hover_text, round(res_gender, 4), sep="Change: "), dim1, dim1)
               #     
               #    heatmaply(res_gender, dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
               #              cexRow = 0.9, cexCol = 0.9, col = scale_colors,  
               #              plot_method = c("plotly"), main = paste0("Changes in Life Expectancy (", 
               #                                                        input$range_t[1], "-", input$range_t[2], ", ",
               #                                                        input$heatGender, ", ", input$heatCountry, ")"), 
               #               font = list(size = 8), custom_hovertext = hover_text2, 
               #               key.title = "Changes in Years", colorbar_xpos = 30, colorbar_ypos = 10) %>% 
               #              layout(xaxis = list(ticktext = as.numeric(gsub("LE Age ", "", colnames(res_gender))), title = "Age", 
               #                                 showgrid = F, tickangle = 0, showticklabels = TRUE), 
               #                    yaxis = list(ticktext = as.numeric(gsub("Contribution Age ", "", rev(rownames(res_gender)))),
               #                                 title = "Contribution", showgrid = F, showticklabels = TRUE))
               # }
            }
        })
        
        
        # for maintaining the state of drill-down variables
        BarplotLE <- reactiveVal()
        BarplotLE_specific <- reactiveVal()

        # when clicking on a category, 
        observeEvent(event_data("plotly_click", source = "BarplotLE"), {
            BarplotLE(event_data("plotly_click", source = "BarplotLE")$x)
            BarplotLE_specific(NULL)
            #updateSelectInput(session, inputId = 'heatAge', selected = gsub("Age ", "", BarplotLE()))
        })
        
        
        output$BarplotLE <- renderPlotly({
            #gender select
            if (length(input$heatGender) == 1){
                if (input$heatGender == "Male"){ res_gender <- res()[[1]] }
                else if (input$heatGender == "Female"){ res_gender <- res()[[2]] }
                else if (input$heatGender == "Population"){ res_gender <- res()[[1]] + res()[[2]] }
            
            
            #ecalculation of total
            print(BarplotLE())
            if (is.null(BarplotLE())){
                Total_male <- data.frame(le = apply(res_gender, 2, sum, na.rm = TRUE), row.names = colnames(res_gender)) %>% 
                    rownames_to_column() %>% mutate(curr_col = "#FDB863")
            }
            else{
                Total_male <- data.frame(le = apply(res_gender, 2, sum, na.rm = TRUE), row.names = colnames(res_gender)) %>% 
                    rownames_to_column() %>% mutate(curr_col = if_else(gsub("LE Age ", "", rowname) %in% BarplotLE(), "#8073AC", "#FDB863"))
            }
                
                p <- plot_ly(Total_male, x = ~gsub("LE Age ", "", rowname), y = ~le, type = "bar", 
                             source = "BarplotLE", marker = list(color = ~curr_col)) %>% 
                    config(displayModeBar = FALSE)
                p <- layout(p, barmode="overlay",
                            title = paste0("Changes in Life Expectancy (", 
                                    input$range_t[1], "-", input$range_t[2], ", ",
                                    input$heatGender, ", ", input$heatCountry, ")"),
                            font = list(size = 8),
                            xaxis = list(title = "Age", categoryarray = names(Total_male), 
                                         categoryorder = "array", size = 8, tickangle = 0), 
                            yaxis = list(title = "Change (Years)"))
                p 
                
            }
            else{ 
                #if (input$heatAggregate == "FALSE"){ 
                    res_gender1 = res()[[1]]
                    res_gender2 = res()[[2]]
                    
                    print(BarplotLE())
                    if (is.null(BarplotLE())){
                        Total_male <- data.frame(le = apply(res_gender1, 2, sum, na.rm = TRUE), row.names = colnames(res_gender1)) %>% 
                            rownames_to_column() %>% mutate(curr_col = "#FDB863")
                        Total_female <- data.frame(le = apply(res_gender2, 2, sum, na.rm = TRUE), row.names = colnames(res_gender2)) %>% 
                            rownames_to_column() %>% mutate(curr_col = "#FD6363")
                    }
                    else{
                        Total_male <- data.frame(le = apply(res_gender1, 2, sum, na.rm = TRUE), row.names = colnames(res_gender1)) %>% 
                            rownames_to_column() %>% mutate(curr_col = if_else(gsub("LE Age ", "", rowname) %in% BarplotLE(), "#8073AC", "#FDB863"))
                        Total_female <- data.frame(le = apply(res_gender2, 2, sum, na.rm = TRUE), row.names = colnames(res_gender2)) %>% 
                            rownames_to_column() %>% mutate(curr_col = if_else(gsub("LE Age ", "", rowname) %in% BarplotLE(), "#92CCDE", "#FD6363"))
                    }
                    
                    p <- plot_ly(Total_male, x = ~gsub("LE Age ", "", rowname), y = ~le, type = "bar", name = 'Male',
                                 source = "BarplotLE", marker = list(color = ~curr_col)) %>%  config(displayModeBar = FALSE) %>% 
                          add_trace(data = Total_female, x = ~gsub("LE Age ", "", rowname), y = ~le, type = "bar", 
                                     name = 'Female', marker = list(color = ~curr_col)) %>% 
                          layout(barmode="group", title = paste0("Changes in Life Expectancy (", 
                                                                 input$range_t[1], "-", input$range_t[2], ", ",
                                                                 paste(input$heatGender, collapse = "/"), ", ", input$heatCountry, ")"), 
                                 font = list(size = 8),
                                 xaxis = list(title = "Age", categoryarray = names(Total_male), 
                                              categoryorder = "array", size = 8, tickangle = 0), 
                                 yaxis = list(title = "Change (Years)"))
                    p
                #}
             #   else{
             #       res_gender = res()[[1]] + res()[[2]]
             #       print(BarplotLE())
             #       if (is.null(BarplotLE())){
             #           Total_male <- data.frame(le = apply(res_gender, 2, sum, na.rm = TRUE), row.names = colnames(res_gender)) %>% 
             #               rownames_to_column() %>% mutate(curr_col = "#FDB863")
             #       }
             #       else{
             #           Total_male <- data.frame(le = apply(res_gender, 2, sum, na.rm = TRUE), row.names = colnames(res_gender)) %>% 
             #               rownames_to_column() %>% mutate(curr_col = if_else(gsub("LE Age ", "", rowname) %in% BarplotLE(), "#8073AC", "#FDB863"))
             #       }
             #       
             #       p <- plot_ly(Total_male, x = ~gsub("LE Age ", "", rowname), y = ~le, type = "bar", 
             #                    source = "BarplotLE", marker = list(color = ~curr_col)) %>% 
             #           config(displayModeBar = FALSE)
             #       p <- layout(p, barmode="overlay",
             #                   title = paste0("Changes in Life Expectancy (", 
             #                                  input$range_t[1], "-", input$range_t[2], ", ",
             #                                  "Aggregate", ", ", input$heatCountry, ")"),
             #                   font = list(size = 8),
             #                   xaxis = list(title = "Age", categoryarray = names(Total_male), 
             #                                categoryorder = "array", size = 8, tickangle = 0), 
             #                   yaxis = list(title = "Change (Years)"))
             #       p 
             #   }
            }
        })
        
        
        
        output$BarplotLE_specific <- renderPlotly({
            if (length(input$heatGender) == 1){
                if (input$heatGender == "Male"){ res_gender <- res()[[1]] }
                else if (input$heatGender == "Female"){ res_gender <- res()[[2]]}
                else if (input$heatGender == "Population") { res_gender <- res()[[1]] + res()[[2]] }
                
                if (is.null(BarplotLE())){  
                    p <- plotly_empty(type = "scatter", mode = "markers") %>%
                          config(displayModeBar = FALSE) %>%
                          layout(title = list(text = "Click on Each Bar for Decomposition Details", yref = "paper", y = 0.5))
                    return(p)
                }
                
                data.frame(contribution = res_gender[, paste("LE Age", BarplotLE())]) %>% 
                    rownames_to_column() %>% mutate(curr_color = "#8073AC") %>%
                    plot_ly(x = ~gsub("Contribution Age", "", rowname), y = ~contribution, source = "BarplotLE_specific", 
                            type = "bar", marker = list(color = ~curr_color)) %>% 
                    config(displayModeBar = FALSE) %>% 
                    layout(barmode="overlay",
                           title = paste0("Contribution of Change in Life Expectancy (", 
                                          input$range_t[1], "-", input$range_t[2], ", ",
                                          input$heatGender, ", ", BarplotLE(), ", ", 
                                          input$heatCountry, ")"), 
                           font = list(size = 8),
                           xaxis = list(title = "Contribution Age", categoryarray = ~rowname, 
                                        categoryorder = "array", size = 8, tickangle = 0),
                           yaxis = list(title = "Contribution (Years)"))
                }
            else{
                #if (input$heatAggregate == "FALSE"){ 
                    if (is.null(BarplotLE())){
                        p <- plotly_empty(type = "scatter", mode = "markers") %>%
                            config(displayModeBar = FALSE) %>%
                            layout(title = list(text = "Click on Each Bar for Decomposition Details", yref = "paper", y = 0.5))
                        return(p)
                    } 
                    
                    res_gender1 = res()[[1]]
                    res_gender2 = res()[[2]]
                    
                    d1 <- data.frame(contribution = res_gender1[, paste("LE Age", BarplotLE())]) %>% 
                        rownames_to_column() %>% mutate(curr_color = "#8073AC") 
                    d2 <- data.frame(contribution = res_gender2[, paste("LE Age", BarplotLE())]) %>% 
                        rownames_to_column() %>% mutate(curr_col = "#92CCDE") 
                    
                    p <- plot_ly(data = d1, x = ~gsub("Contribution Age", "", rowname), y = ~contribution, source = "BarplotLE_specific", 
                                 type = "bar", marker = list(color = ~curr_color), name = "Male") %>%  config(displayModeBar = FALSE)  %>% 
                         add_trace(data = d2, x = ~gsub("Contribution Age", "", rowname),
                                  y = ~contribution, type = "bar", name = "Female", marker = list(color = ~curr_col)) %>% 
                         layout(barmode="group",
                                title = paste0("Contribution of Change in Life Expectancy (", 
                                               input$range_t[1], "-", input$range_t[2], ", ",
                                               paste0(input$heatGender, collapse = "/"), ", ", BarplotLE(), ", ", 
                                               input$heatCountry, ")"), 
                                font = list(size = 8),
                                xaxis = list(title = "Contribution Age", categoryarray = ~rowname, 
                                             categoryorder = "array", size = 8, tickangle = 0),
                                yaxis = list(title = "Life Expectancy Change (Years)"))
                    p
                #}
                #else{
                #    if (is.null(BarplotLE())){  
                #        p <- plotly_empty(type = "scatter", mode = "markers") %>%
                #            config(displayModeBar = FALSE) %>%
                #            layout(title = list(text = "Click on Each Bar for Decomposition Details", yref = "paper", y = 0.5))
                #        return(p)
                #    }
                #    res_gender <- res()[[1]] + res()[[2]]
                #    
                #    data.frame(contribution = res_gender[, paste("LE Age", BarplotLE())]) %>% 
                #        rownames_to_column() %>% mutate(curr_color = "#8073AC") %>%
                #        plot_ly(x = ~gsub("Contribution Age", "", rowname), y = ~contribution, source = "BarplotLE_specific", 
                #                type = "bar", marker = list(color = ~curr_color)) %>% 
                #        config(displayModeBar = FALSE) %>% 
                #        layout(barmode="overlay",
                #               title = paste0("Contribution of Change in Life Expectancy (", 
                #                              input$range_t[1], "-", input$range_t[2], ", ",
                #                              "Aggregate", ", ", BarplotLE(), ", ", 
                #                              input$heatCountry, ")"), 
                #               font = list(size = 8),
                #               xaxis = list(title = "Contribution Age", categoryarray = ~rowname, 
                #                            categoryorder = "array", size = 8, tickangle = 0),
                #               yaxis = list(title = "Contribution (Years)"))
                #}
            }
                
         })
        
        
        
        
       # second tab
        
        LP_res <- reactive({
            req(input$heatCountry)
            chosen_country <- as.character(country_info()$Code[which(country_info()$Country == input$heatCountry)])
            return(change5x1_LP(chosen_country, input$range_t[1], input$range_t[2], input$z))
        })
        
        output$LineLP <- renderPlotly(
            plot_ly(LP_res()[[1]], x = ~initial, y = ~malet1, name = paste("Male", input$range_t[1]), 
                    type = 'scatter', mode = 'lines+markers',  
                    line = list(color = "#FDB863", dash = "dot"), 
                    marker = list(color = "#FDB863", symbol = "square", size = 10)) %>% 
                add_trace(y = ~femalet1, name = paste("Female", input$range_t[1]), mode = 'lines+markers',
                          line = list(color = "#FD6363", dash = "dot"), 
                          marker = list(color = "#FD6363", symbol = "square", size = 10)) %>% 
                add_trace(y = ~malet2, name = paste("Male", input$range_t[2]), mode = 'lines+markers',
                          line = list(color = "#8073AC", dash = "dash"), 
                          marker = list(color = "#8073AC", symbol = "circle", size = 10)) %>% 
                add_trace(y = ~femalet2, name = paste("Female", input$range_t[2]), mode = 'lines+markers',
                          line = list(color = "#92CCDE", dash = "dash"), 
                          marker = list(color = "#92CCDE", symbol = "circle", size = 10)) %>% 
                layout(title = paste0("Life Preparancy Per Age Group at ", scales::ordinal(input$z*100), " Percentile ", 
                                      "(", input$range_t[1], "-", input$range_t[2], ", ", input$heatCountry, ")"),
                       legend = list(orientation = "v", xanchor = "left", x = 0.10), 
                       font = list(size = 8), 
                       xaxis = list(title = "Age", size = 8, tickangle = 0), 
                       yaxis = list(title = "Life Preparancy (Years)", size = 8, tickangle = 0)) %>% 
                config(displayModeBar = FALSE)
        )
        
        
        output$HeatMapLP <- renderPlotly({
            
            chosen_country <- as.character(country_info()$Code[which(country_info()$Country == input$heatCountry)])
            scale_colors <- brewer.pal(n=9, name = "YlOrRd") #selection of
            
            if (length(input$heatGender) == 1){
                if (input$heatGender == "Male" ){ res_gender <- LP_res()[[2]] }
                else if (input$heatGender == "Female" ){ res_gender <- LP_res()[[3]]}
                
                par(mar=c(5.1,2.1,1,2.1))
                dim1 <- dim(res_gender)[[1]]
                hover_text <- matrix(paste0((sapply(colnames(res_gender), function(x) rep(x, dim1))), "<br>", 
                                            rep(rownames(res_gender), dim1), "<br>", 
                                            rep(input$heatGender, dim1*2), "<br>"),
                                     byrow = FALSE, ncol = dim1)
                hover_text2 <- matrix(paste(hover_text, round(res_gender, 4), sep="Change: "), dim1, dim1)
                
                heatmaply(res_gender, dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
                          cexRow = 0.9, cexCol = 0.9, col = scale_colors,  
                          plot_method = c("plotly"), main = paste0("Changes in Life Preparancy (", 
                                                                   input$range_t[1], "-", input$range_t[2], ", ",
                                                                   input$heatGender, ", ", input$heatCountry, ")"), 
                          font = list(size = 8), custom_hovertext = hover_text2, 
                          key.title = "Changes in Years", colorbar_xpos = 30, colorbar_ypos = 10) %>% 
                    layout(xaxis = list(ticktext = as.numeric(gsub("LP Age ", "", colnames(res_gender))), title = "Age", 
                                        showgrid = F, tickangle = 0, showticklabels = TRUE), 
                           yaxis = list(ticktext = as.numeric(gsub("Contribution Age ", "", rev(rownames(res_gender)))),
                                        title = "Contribution", showgrid = F, showticklabels = TRUE))
                
            }
            else{ 
                #if (input$heatAggregate == "FALSE"){ 
                res_gender <- LP_res()[[4]]
                par(mar=c(5.1,2.1,1,2.1))
                dim1 <- dim(res_gender)[[1]]
                
                #create hover text 
                row_contr <- create_hover_MF(c(rownames(res_gender)[-1], " ")) #byrow
                col_le <- create_hover_MF(colnames(res_gender)) #bycol
                
                mat1 <- matrix(row_contr, byrow = TRUE, ncol = dim1)
                mat2 <- matrix(col_le, byrow = FALSE, ncol = dim1)
                mat3 <- ifelse(lower.tri(mat2), "Male", "Female")
                
                mat4 <- matrix(paste(paste0(mat1, "<br>", mat2, "<br>", mat3, "<br>"), 
                                     round(res_gender, 4), sep = "Change: "), dim1, dim1)
                diag(mat4) <- NA #remove diag hover text
                
                heatmaply(res_gender, dendrogram = "none", Rowv = FALSE, Colv = FALSE, #sepwidth=c(1.5, 1.5),
                          cexRow = 0.65, cexCol = 0.65, 
                          col = scale_colors, xlab = "", ylab = "", plot_method = c("plotly"),
                          main = paste0("Changes in Life Preparancy (", 
                                        input$range_t[1], "-", input$range_t[2], ", ",
                                        paste(input$heatGender, collapse = "/"), ", ", input$heatCountry, ")"),
                          font = list(size = 8), custom_hovertext = mat4,
                          key.title = "Changes in Years", 
                          colorbar_xpos = 30, colorbar_ypos = 10) %>% 
                    layout(shapes = list(type = 'line', x0 = 0, x1 = 24, y0 =24, y1 = 0, line = list(width = 1.5)),
                           xaxis = list(title = "Age", showgrid = F, showticklabels = FALSE), 
                           yaxis = list(title = "Contribution", showgrid = F, showticklabels = FALSE))
                }
            
        })
        
        # third tab
        
        gap_age <- reactive({
            req(input$heatCountry)
            chosen_country <- as.character(country_info()$Code[which(country_info()$Country == input$heatCountry)])
            return(GenGap_Age(chosen_country, input$range_t[1]))
        })
        
        output$GapAge <- renderPlotly({
            #my_col <- colorRampPalette(brewer.pal(8, "YlOrRd"))(length(age_interest))
            
            plot_ly(gap_age()[[2]], x = ~age_interest, y = ~value, color = ~variable, type= 'bar', 
                    hoverinfo = "text", text = ~paste0("Age Group: ", age_interest, '</br></br>', 
                                                       "Contribution: ", variable, '</br>',
                                                       "Value: ", round(value, 4)))  %>%  
                layout(title = paste0("Contributions to Gender Gap in Life Expectancy (", input$range_t[1], ")" ), 
                       yaxis = list(title = 'Contribution (Male - Female)'), barmode = 'stack', 
                       xaxis = list(title = "Age", categoryarray = names(gap_age()[[2]]), 
                                    categoryorder = "array", size = 8, tickangle = 0), 
                       showlegend = FALSE) %>% config(displayModeBar = FALSE)
        })
        
        output$GapAgeTable <- renderDataTable(gap_age()[[1]], options = list(searching = FALSE, lengthMenu = c(15, 25, 50)))
        

        
        
        
        ### - start of decomposition by age and COD - ###
        
        observe({
            updateSelectInput(session, inputId = 'CODCountry', label = 'Selected Country',
                              choices = c("Canada", "Czech Republic", "France", 
                                          "United Kingdom", "Japan", "Norway", "Sweden", "USA"), 
                              selected = input$CODCountry)
        })
        
        COD_countries <- reactive({
            COD_countries <- data.frame(country = c("Canada", "Czech Republic", "France", "United Kingdom", "Japan", "Norway", "Sweden", "USA"),
                                        code = c("CAN", "CZEC", "FRATNP", "GBRTENW", "JPN", "NOR", "SWE", "USA"))
        })
        
        chapters20 <- reactive({
            diagn<-c("Infectious","Cancer","Benign tumor","Blood","Endocrine/Nutrition",
                     "Mental Disorder","Nervous System","Heart Disease","Cerebrovascular","Circulatory",
                     "Respiratory","Digestive","Skin","Musculoskeletal","Genitourinary",
                     "Pregnancy/childbirth","Perinatal Conditions","Birth Defects","Unknown","External")
            data.frame(chapter = 1:length(diagn), diagn)
        })
        
        animate_res <- reactive({
            
            if(isTRUE(input$CODAggregate)){
                
                num_countries <- length(COD_countries())
                rates_males_ff <- data.frame()
                for (countries in COD_countries()$code){
                    COD_Info <- read.csv(paste0("COD_5x1_chapters_", countries, ".csv"))
                    rates_per_chapter_males2<-dcast(COD_Info,Year+COD.chap+Country~Age,value.var = "Rates.M")
                    rates_males_ff <- rbind(rates_males_ff, rates_per_chapter_males2)
                }
                rates_males_ff$Country <- plyr::mapvalues(rates_males_ff$Country, levels(rates_males_ff$Country), levels(COD_countries()$country))
                #cod chapter not 21
                rates_males_ff[rates_males_ff$COD.chap == "All",]$COD.chap <- 21
                #picking your age group 
                age_initial <- colnames(rates_males_ff[, -c(1:3)])
                selected_agegrp <- max(which((input$CODAge >= as.numeric(age_initial)) == TRUE))
                
                #find data for sunburst plot
                chosen_rates = rates_males_ff[,c(1:3, 3+selected_agegrp)]
                chosen_rates_year = chosen_rates[which(chosen_rates$Year == input$tcod_year),] #input$range_tcod[1]), ]
                
                ##breakdown mortality chapters
                
                chosen_rates_year <- merge(chosen_rates_year, chapters20(), by.x = "COD.chap", by.y = "chapter")
                chosen_rates_year$ids = paste(chosen_rates_year$Country, chosen_rates_year$diagn, sep = "-")
                names(chosen_rates_year)[4] <- "value"
                chosen_rates_year <- chosen_rates_year[-which(chosen_rates_year$value == 0), ]
                #parent pies in sunburst plot
                parent_rates <- data.frame(Country = rep("", length(as.character(unique(chosen_rates_year$Country)))), 
                                           value = rep(0, length(as.character(unique(chosen_rates_year$Country)))), 
                                           ids = as.character(unique(chosen_rates_year$Country)), 
                                           diagn = as.character(unique(chosen_rates_year$Country)))
                final_sunburst <- rbind(parent_rates, chosen_rates_year[, c("Country", "value", "ids", "diagn")])
                final_sunburst$text <- ifelse(final_sunburst$value == 0, "", round(final_sunburst$value, 3))
                return(final_sunburst)
            }
            else{ #(isFALSE(input$CODAggregate)){
                
                    print(input$CODAge)
                    print(input$CODAggregate)
                    req(input$CODCountry)
                    #import the file
                    COD_Info <- read.csv(paste0("COD_5x1_chapters_", COD_countries()$code[which(COD_countries()$country == input$CODCountry)], ".csv"))
                    rates_per_chapter_males <- dcast(COD_Info,Year+COD.chap~Age,value.var = paste0("Rates.", substring(input$CODGender[1], 1, 1)))
                    #parameters
                    mortality_chapters<-max(rates_per_chapter_males$COD.chap)-1 #20 mortality chapters, 21st is total
                    age_groups <- length(unique(COD_Info$Age)) #number of age groups
                    t1 = input$range_tcod[1] ; t2 = input$range_tcod[2]
                    span_years <- t2 - t1 + 1
                    #picking your age group 
                    age_initial <- colnames(rates_per_chapter_males[, -c(1:2)])
                    selected_agegrp <- max(which((input$CODAge >= as.numeric(age_initial)) == TRUE))
                    #animation rates
                    rates_animate <- as.matrix(rates_per_chapter_males[rates_per_chapter_males$Year>=t1 & rates_per_chapter_males$Year <= t2,-(1:2)])/1000
                    rates_animate <- data.frame(chapter = rep(1:(mortality_chapters+1), span_years), 
                                                year = rep(t1:t2, each = mortality_chapters+1), 
                                                rate = rates_animate[,selected_agegrp]) #age
                    #find prop percent
                    rates_animate$perct = rates_animate$rate/rep(rates_animate[rates_animate$chapter == 21,]$rate, each = 21)
                    rates_animate$chapter <- as.factor(rates_animate$chapter)
                    rates_animate <- droplevels(rates_animate[-which(rates_animate$chapter == 21),])
                    
                    ##breakdown mortality chapters
                    rates_animate_df <- merge(rates_animate, chapters20(), by = "chapter")
                    return(rates_animate_df)
                    
            }
            
        })
        
        output$Animate_MCBar <- renderPlotly({
            #if(input$CODAggregate == FALSE){
                
            
            if(input$CODAggregate == TRUE){
                animate_res() %>%
                    plot_ly(hovertext = ~paste0(diagn, '</br></br>', text, '</br>'), hoverinfo = "text") %>%
                    add_trace(
                        ids = ~ids, labels = ~diagn, parents = ~Country, values = ~value, 
                        type = 'sunburst', maxdepth = 2, insidetextorientation='radial',
                        domain = list(column = 1)
                    ) %>% 
                    layout(title = paste0("Rates by Country and Mortality Chapters (", input$tcod_year,  
                                          ", ", input$CODGender, ", ", input$CODAge, ")" ),
                           font = list(size = 8),
                           yaxis = list(title = 'Proportion'), xaxis = list(title = "Mortality Chapter", size = 8, tickangle = 0), 
                           showlegend = FALSE) %>% config(displayModeBar = FALSE)
            }
            else{
                animate_res() %>%
                    plot_ly(x = ~chapter, y = ~perct, color = ~diagn, frame = ~year, 
                            text = ~paste0("Chapter: ", diagn, '</br></br>', 
                                           "Year: ", year, '</br>',
                                           "Value: ", round(rate,4), '</br>',
                                           "Proportion: ", paste0(round(perct*100, 4), "%")), 
                            hoverinfo = "text", type = 'bar'
                    ) %>% layout(title = paste0("Proportional Changes in Mortality Chapters (", input$range_tcod[1], "-", input$range_tcod[2], 
                                                ", ", input$CODGender, ", ", input$CODAge, ", ", input$CODCountry, ")" ),
                                 font = list(size = 8),
                                 yaxis = list(title = 'Proportion'), xaxis = list(title = "Mortality Chapter", size = 8, tickangle = 0), 
                                 showlegend = FALSE) %>% config(displayModeBar = FALSE) %>% 
                    animation_slider(
                        currentvalue = list(prefix = "Year ")
                    )
            }
        })
        
        # for maintaining the state of drill-down variables
        BarplotLE_AgeCOD <- reactiveVal()
        BarplotLE_specificAgeCOD <- reactiveVal()
        
        # when clicking on a category, 
        observeEvent(event_data("plotly_click", source = "BarplotLE_AgeCOD"), {
            BarplotLE_AgeCOD(event_data("plotly_click", source = "BarplotLE_AgeCOD")$x)
            BarplotLE_specificAgeCOD(NULL)
        })
        
        
        # output
        Changes_age_cause <- reactive({
            #life expectancy
            req(input$CODCountry)
            chosen_country <- as.character(COD_countries()$code[which(COD_countries()$country == input$CODCountry)])
            #life expectancy
            res <- change5x1(chosen_country, input$range_tcod[1], input$range_tcod[2])
            #mortality chapters
            res_COD <- change5x1_AgeCOD(chosen_country, input$range_tcod[1], input$range_tcod[2])
            #age group 
            mortality_chapters = 20
            age_initial <- gsub("LE Age ", "", colnames(res[[1]]))
            age_groups <- dim(res_COD[[1]])[2]
            selected_agegrp <- max(which((input$CODAge >= as.numeric(age_initial)) == TRUE))
            
            #final output
            Changes_age_cause_male<-matrix(0,nrow=mortality_chapters,ncol=age_groups)
            Changes_age_cause_female<-matrix(0,nrow=mortality_chapters,ncol=age_groups)
            for(x in 1:age_groups){
                Changes_age_cause_male[,x]<-res[[1]][x,selected_agegrp]*res_COD[[1]][,x]
                Changes_age_cause_female[,x]<-res[[2]][x,selected_agegrp]*res_COD[[2]][,x]
            }
            colnames(Changes_age_cause_male) <- age_initial
            colnames(Changes_age_cause_female) <- age_initial
            rownames(Changes_age_cause_male) <- 1:mortality_chapters
            rownames(Changes_age_cause_female) <- 1:mortality_chapters
            return(list(Changes_age_cause_male, Changes_age_cause_female))
        })
        
        output$BarplotLE_AgeCOD <- renderPlotly({
            #gender select
            if (length(input$CODGender) == 1){
                if (input$CODGender == "Male"){ res_gender <- Changes_age_cause()[[1]] }
                else if (input$CODGender == "Female"){ res_gender <- Changes_age_cause()[[2]] }
                #calculation of total
                print(BarplotLE_AgeCOD())
                if (is.null(BarplotLE_AgeCOD())){
                    Total_age_cause <- data.frame(le = apply(res_gender, 2, sum, na.rm = TRUE), 
                                                       row.names = colnames(res_gender)) %>% 
                                                rownames_to_column() %>% mutate(curr_col = "#FDB863")
                    selected_agegrp <- max(which((input$CODAge >= as.numeric(Total_age_cause$rowname)) == TRUE))
                }
                else{
                    Total_age_cause <- data.frame(le = apply(res_gender, 2, sum, na.rm = TRUE), 
                                                       row.names = colnames(res_gender)) %>% 
                                                rownames_to_column() %>% mutate(curr_col = if_else(rowname %in% BarplotLE_AgeCOD(), "#8073AC", "#FDB863"))
                    selected_agegrp <- max(which((input$CODAge >= as.numeric(Total_age_cause$rowname)) == TRUE))
                }
                
                plot_ly(Total_age_cause[c(selected_agegrp:23),], x = ~rowname, y = ~le, type = "bar", 
                        source = "BarplotLE_AgeCOD", marker = list(color = ~curr_col)) %>% 
                    config(displayModeBar = FALSE) %>% 
                    layout(p, barmode="overlay", title = paste0("Changes in Life Expectancy (",
                                                                input$range_tcod[1], "-", input$range_tcod[2], ", ", 
                                                                input$CODGender, ", ", input$CODCountry, ")"),      
                           font = list(size = 8), xaxis = list(title = "Age", 
                                                               categoryarray = names(Total_age_cause[c(selected_agegrp:23),]), 
                                                               categoryorder = "array", size = 8, tickangle = 0), 
                           yaxis = list(title = "Change (Years)"))
            }
            else{ 
                res_gender1 = Changes_age_cause()[[1]]
                res_gender2 = Changes_age_cause()[[2]]
                
                print(BarplotLE_AgeCOD())
                if (is.null(BarplotLE_AgeCOD())){
                    
                    #male
                    Total_male_age_cause <- data.frame(le = apply(res_gender1, 2, sum, na.rm = TRUE), 
                                                       row.names = colnames(res_gender1)) %>% 
                        rownames_to_column() %>% mutate(curr_col = "#FDB863")
                    selected_agegrp <- max(which((input$CODAge >= as.numeric(Total_male_age_cause$rowname)) == TRUE))
        
                    #female
                    Total_female_age_cause <- data.frame(le = apply(res_gender2, 2, sum, na.rm = TRUE), 
                                                       row.names = colnames(res_gender2)) %>% 
                        rownames_to_column() %>% mutate(curr_col = "#FD6363")
                    selected_agegrp <- max(which((input$CODAge >= as.numeric(Total_female_age_cause$rowname)) == TRUE))
                    
                }
                else{
                    #male
                    Total_male_age_cause <- data.frame(le = apply(res_gender1, 2, sum, na.rm = TRUE), 
                                                       row.names = colnames(res_gender1)) %>% 
                        rownames_to_column() %>% mutate(curr_col = if_else(rowname %in% BarplotLE_AgeCOD(), "#8073AC", "#FDB863"))
                    selected_agegrp <- max(which((input$CODAge >= as.numeric(Total_male_age_cause$rowname)) == TRUE))
                    
                    #female
                    Total_female_age_cause <- data.frame(le = apply(res_gender2, 2, sum, na.rm = TRUE), 
                                                       row.names = colnames(res_gender2)) %>% 
                        rownames_to_column() %>% mutate(curr_col = if_else(rowname %in% BarplotLE_AgeCOD(), "#92CCDE", "#FD6363"))
                    selected_agegrp <- max(which((input$CODAge >= as.numeric(Total_male_age_cause$rowname)) == TRUE))
                    
                }
                
                p <- plot_ly(Total_male_age_cause[c(selected_agegrp:23),], x = ~rowname, y = ~le, type = "bar", name = 'Male',
                             source = "BarplotLE_AgeCOD", marker = list(color = ~curr_col)) %>%  config(displayModeBar = FALSE) %>% 
                    add_trace(data = Total_female_age_cause[c(selected_agegrp:23),], x = ~rowname, y = ~le, type = "bar", 
                              name = 'Female', marker = list(color = ~curr_col)) %>% 
                    layout(barmode="group", title = paste0("Changes in Life Expectancy (", 
                                                           input$range_tcod[1], "-", input$range_tcod[2], ", ",
                                                           paste(input$CODGender, collapse = "/"), ", ", input$CODCountry, ")"), 
                           font = list(size = 8),
                           xaxis = list(title = "Age", categoryarray = names(Total_male_age_cause[c(selected_agegrp:23),]), 
                                        categoryorder = "array", size = 8, tickangle = 0), 
                           yaxis = list(title = "Change (Years)"))
                p
            }
        })
        
        
        
        output$BarplotLE_specificAgeCOD <- renderPlotly({
            if (length(input$CODGender) == 1){
                if (input$CODGender == "Male"){ res_gender <- Changes_age_cause()[[1]] }
                else if (input$CODGender == "Female"){ res_gender <- Changes_age_cause()[[2]]}

                if (is.null(BarplotLE_AgeCOD())){  
                    p <- plotly_empty(type = "scatter", mode = "markers") %>%
                        config(displayModeBar = FALSE) %>%
                        layout(title = list(text = "Click on Each Bar for Decomposition Details", yref = "paper", y = 0.5))
                    return(p)
                }
                
                print(BarplotLE_AgeCOD())
                data.frame(contribution = res_gender[, BarplotLE_AgeCOD()]) %>% 
                    rownames_to_column() %>% mutate(curr_color = "#8073AC") %>%
                    mutate(rowname = as.integer(rowname)) %>% 
                    inner_join(chapters20(), by = c("rowname" = "chapter")) %>% 
                    plot_ly(x = ~rowname, y = ~contribution, source = "BarplotLE_specificAgeCOD", 
                            type = "bar", marker = list(color = ~curr_color), 
                            hoverinfo = "text", text = ~paste0("Mortality Chapter: ", diagn, '</br></br>', 
                                                               "Contribution: ", round(contribution, 4), '</br>')) %>% 
                    config(displayModeBar = FALSE) %>% 
                    layout(barmode="overlay",
                           title = paste0("Contribution of Change in Life Expectancy (", 
                                          input$range_tcod[1], "-", input$range_tcod[2], ", ",
                                          input$CODGender, ", ", BarplotLE_AgeCOD(), ", ", 
                                          input$CODCountry, ")"), 
                           font = list(size = 8),
                           xaxis = list(title = "Contribution Mortality Chapter", categoryarray = ~rowname, 
                                        categoryorder = "array", size = 8, tickangle = 0),
                           yaxis = list(title = "Contribution (Years)"))
            }
            else{
                
                print(BarplotLE_AgeCOD())
                if (is.null(BarplotLE_AgeCOD())){
                    p <- plotly_empty(type = "scatter", mode = "markers") %>%
                        config(displayModeBar = FALSE) %>%
                        layout(title = list(text = "Click on Each Bar for Decomposition Details", yref = "paper", y = 0.5))
                    return(p)
                } 
                
                res_gender1 = Changes_age_cause()[[1]]
                res_gender2 = Changes_age_cause()[[2]]
                
                d1 <- data.frame(contribution = res_gender1[, BarplotLE_AgeCOD()]) %>% 
                          rownames_to_column() %>% mutate(curr_color = "#8073AC") 
                d2 <- data.frame(contribution = res_gender2[, BarplotLE_AgeCOD()]) %>% 
                          rownames_to_column() %>% mutate(curr_col = "#92CCDE") 
                
                p <- plot_ly(data = d1, x = ~rowname, y = ~contribution, source = "BarplotLE_specificAgeCOD", 
                             type = "bar", marker = list(color = ~curr_color), name = "Male") %>%  config(displayModeBar = FALSE)  %>% 
                    add_trace(data = d2, x = ~rowname, y = ~contribution, type = "bar", 
                              name = "Female", marker = list(color = ~curr_col)) %>% 
                    layout(barmode="group",
                           title = paste0("Contribution of Change in Life Expectancy (", 
                                          input$range_tcod[1], "-", input$range_tcod[2], ", ",
                                          paste0(input$CODGender, collapse = "/"), ", ", BarplotLE_AgeCOD(), ", ", 
                                          input$CODCountry, ")"), 
                           font = list(size = 8),
                           xaxis = list(title = "Contribution Age", categoryarray = ~rowname, 
                                        categoryorder = "array", size = 8, tickangle = 0),
                           yaxis = list(title = "Life Expectancy Change (Years)"))
                p
            }
            
        })
        
       ###heatmap COD
        output$HeatMap_AgeCOD <- renderPlotly({
            chosen_country <- as.character(country_info()$Code[which(country_info()$Country == input$heatCountry)])
            scale_colors <- brewer.pal(n=9, name = "YlOrRd") #selection of
            
            if (length(input$CODGender) == 1){
                if (input$CODGender == "Male" ){ res_gender <- Changes_age_cause()[[1]] }
                else if (input$CODGender == "Female" ){ res_gender <- Changes_age_cause()[[2]]}
                
                par(mar=c(5.1,2.1,1,2.1))
                dim1 <- dim(res_gender)[[1]]
                dim2 <- dim(res_gender)[[2]]
                hover_text <- matrix(paste0((sapply(colnames(res_gender), function(x) rep(x, dim1))), "<br>", 
                                     rep(rownames(res_gender), dim2), "<br>", 
                                     rep(input$CODGender, dim1*dim2), "<br>"),
                                     byrow = FALSE, ncol = dim2)
                hover_text2 <- matrix(paste(hover_text, round(res_gender, 4), sep="Change: "), dim1, dim2)
                
                heatmaply(res_gender, dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
                          cexRow = 0.9, cexCol = 0.9, col = scale_colors,  
                          plot_method = c("plotly"), main = paste0("Changes in Life Expectancy (", 
                                                                   input$range_tcod[1], "-", input$range_tcod[2], ", ",
                                                                   input$CODGender, ", ", input$CODCountry, ")"), 
                          font = list(size = 8), custom_hovertext = hover_text2, 
                          key.title = "Changes in Years", colorbar_xpos = 30, colorbar_ypos = 10) %>% 
                    layout(xaxis = list(ticktext = as.numeric(colnames(res_gender)), title = "Age", 
                                        showgrid = F, tickangle = 0, showticklabels = TRUE), 
                           yaxis = list(ticktext = as.numeric(rev(rownames(res_gender))),
                                        title = "Contribution", showgrid = F, showticklabels = TRUE))
                
            }
            else{ 
                #if (input$heatAggregate == "FALSE"){ 
                res_gender <- Changes_age_cause()[[2]] - Changes_age_cause()[[1]]
                
                par(mar=c(5.1,2.1,1,2.1))
                dim1 <- dim(res_gender)[[1]]
                dim2 <- dim(res_gender)[[2]]
                hover_text <- matrix(paste0((sapply(colnames(res_gender), function(x) rep(x, dim1))), "<br>", 
                                            rep(rownames(res_gender), dim2), "<br>", 
                                            rep("Male-Female", dim1*dim2), "<br>"),
                                     byrow = FALSE, ncol = dim2)
                hover_text2 <- matrix(paste(hover_text, round(res_gender, 4), sep="Change: "), dim1, dim2)
                
                heatmaply(res_gender, dendrogram = "none", Rowv = FALSE, Colv = FALSE, #sepwidth=c(1.5, 1.5),
                          cexRow = 0.65, cexCol = 0.65, 
                          col = scale_colors, xlab = "", ylab = "", plot_method = c("plotly"),
                          main = paste0("Changes in Life Expectancy (", 
                                        input$range_tcod[1], "-", input$range_tcod[2], ", ",
                                        "Female - Male, ", input$CODCountry, ")"),
                          font = list(size = 8), custom_hovertext = mat4,
                          key.title = "Changes in Years", 
                          colorbar_xpos = 30, colorbar_ypos = 10) %>% 
                    layout(shapes = list(type = 'line', x0 = 0, x1 = 25, y0 =25, y1 = 0, line = list(width = 1.5)),
                           xaxis = list(title = "Age", showgrid = F, showticklabels = FALSE), 
                           yaxis = list(title = "Contribution", showgrid = F, showticklabels = FALSE))
            }
        })
        
        
        
        
        
        
        
        

})


shinyApp(ui, server)

