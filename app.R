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
    ,sidebarMenuBorderRadius = 5
    
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
    ,sidebarTabBorderWidth = 20
    
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
                                        class = 'dropdown'), titleWidth = 300
                            )

META <- list(
    # Name of the app, used in the browser/tab title
    name        = "rstudio::conf(\'tweets\')",
    # A description of the app, used in social media cards
    description = "A Shiny Dashboard, rstudio::conf #FOMO reducer, tweet explorer by @grrrck",
    # Link to the app, used in social media cards
    app_url     = "https://apps.garrickadenbuie.com/rstudioconf-2019/",
    # Link to app icon image, used in social media cards
    app_icon    = "https://garrickadenbuie.com/images/2019/rstudioconf-2019-icon.png",
    # The name of the conference or organization
    conf_org    = "rstudio::conf",
    # App title, long, shown when sidebar is open, HTML is valid
    logo_lg     = "<em>rstudio</em>::<strong>conf</strong>(2019)",
    # App title, short, shown when sidebar is collapsed, HTML is valid
    logo_mini   = "<em>rs</em><strong>c</strong>",
    # Icon for box with count of conference-related tweets
    topic_icon  = "comments",
    # Icon for box with count of "community"-related tweets
    topic_icon_full = "r-project",
    # AdminLTE skin color for the dashboard
    skin_color  = "blue-light",
    # AdminLTE theme CSS files
    theme_css   = c("ocean-next/AdminLTE.css", "ocean-next/_all-skins.css")
)


ui = dashboardPagePlus(
    
   # skin = "blue-light",
    title = 'MortalityViz',
    #skin  = META$skin_color,
    #theme = c(META$theme_css, "custom.css"),
    #sidebar_mini = TRUE,

    
    # Dashboard Page Setup ----------------------------------------------------
    dbHeader,
    #dashboardHeader(
    ### changing logo
    # title = logo_blue_gradient),
    
    # Dashboard Sidebar -------------------------------------------------------
    dashboardSidebar(
        width = 300,
        sidebarUserPanel("Mushu",
                         subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                         # Image file should be in www/ subdir
                         image = "https://fiverr-res.cloudinary.com/t_profile_original,q_auto,f_auto/attachments/profile/photo/a8ece8a3bb5951b6a9ffe9a19063327a-1537916496823/Mushu%20Glasses.jpg"
        ),
        sidebarMenu(id = "tabs_all",
            menuItem("HOME", tabName = "tab_home", icon = icon("home")), 
            menuItem("LEARN MORE", tabName = "tab_learnmore", icon = icon("lightbulb"), startExpanded = FALSE, 
                     menuSubItem("Explore", tabName = "tab_explore", icon = icon("compass")), 
                     menuSubItem("Documentation", tabName = "tab_docu", icon = icon("book")), 
                     menuSubItem("Community", tabName = "tab_community", icon = icon("users"))),
            menuItem("LATEST NEWS", tabName = "tab_news", icon = icon("file-alt")),
            menuItem("DECOMPOSITION", #tabName = "tab_le", 
                         icon = icon("desktop"), startExpanded = TRUE,
                menuSubItem("Overview", tabName = "tab_le", icon = icon("clipboard-list")),
                menuSubItem("By Age", tabName = "DecAge", icon = icon("chevron-right")), #icon("hand-holding-heart")),
                menuSubItem("By Age/COD", tabName = "DecAgeCOD", icon = icon("chevron-right"))), #icon("hand-holding-usd"))), #, "tab_le"),
            menuItem("ABOUT", tabName = "tab_about", icon = icon("gear")), #info
            menuItem("Q & A", tabName = "tab_qa", icon = icon("question-circle"))#, 
            #HTML(paste0(
            #    "<br><br><br><br><br><br><br><br><br>",
            #    "<table style='margin-left:auto; margin-right:auto;'>",
            ##    "<tr>",
            #    "<td style='padding: 5px;'><a href='https://www.facebook.com/SocietyofActuaries' target='_blank'><i class='fab fa-facebook-square fa-lg'></i></a></td>",
            #    "<td style='padding: 5px;'><a href='https://www.twitter.com/soactuaries' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
            #    "<td style='padding: 5px;'><a href='https://www.instagram.com/soactuaries' target='_blank'><i class='fab fa-instagram fa-lg'></i></a></td>",
            #    "</tr>",
            #   "</table>"
            #    ),
            #    HTML(paste0(
            #        "<script>",
            #        "var today = new Date();",
            #        "var yyyy = today.getFullYear();",
            #        "</script>",
            #        "<p style = 'text-align: center;'><small>&copy; - <a href='https://benjaminhsu.netlify.com' target='_blank'>Benjamin Hsu</a> - </small></p>")
            #    ))
           
        )
    ),
    
    # Dashboard Body ----------------------------------------------------------
    dashboardBody(
    
        tags$script(HTML("$('body').addClass('sidebar-mini');")),
        theme_custom,
        
        
        #tabItems(
        tags$head(tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=News+Cycle');
                                    h1 {font-family:'News Cycle',sans-serif;
                                        font-size: 48px;
                                        font-weight: 1000;
                                        line-height: 1.1;
                                        color: 'slategrey';
                                    }")),
        
                      #  '.main-sidebar { font-size: 20px; font-weight: bold;}
                      #  .skin-blue .main-header .logo:hover {  background-color: #47b2ff; }
                      #  .skin-blue .main-header .navbar {  background-color: "rgb(255,255,255)"; }')),
                  #'.main-sidebar {font-weight: bold; font-family: Source Sans Pro
                     # font-size: 30px; ')),
                  
                  tags$link(rel = "shortcut icon", 
                            href = "https://www.columbia.edu/content/themes/custom/columbia/favicon-crown.png"), 
                  ),
        
    tabItems(
        # About - tab_introduction of the web application -------------------------------------------------------
        tabItem(
            "tab_home", HTML("<b><h3>Mortality Analysis Calculator</h3></b>"), hr(),
            fluidRow(
                    box(
                        title = "", 
                        status = "primary",
                        width = 4, height = 425,
                        column(width = 12, align = "center",
                                        img(src = "compass-icon.png", width=100),
                        HTML(paste0("<br>" ,"<b><h2><p style = 'text-align: center ; color: black'>Explore</p></h2></b>",
                        "<p style = 'text-align: center; line-height: 25px; vertical-align: center; padding: 15px 35px; font-size: 17px'>
                        Guided learning with explanation on the depths of mortality decomposition analysis. 
                        Follow step by step process to explore the technical details of mortality decomposition. </p>")), 
                        #column(12, align = "center", actionButton("explore_more", "Learn more")), 
                        div(style="display:inline-block; width:100%; text-align: center;", 
                            actionButton(inputId='explore_more', label="Let's Explore", icon = icon("plus")))
                        )
                    ), 
                    box(
                        title = "",
                        status = "warning",
                        width = 4, height = 425, 
                        column(width = 12, align = "center",
                               img(src = "book-icon.png", width=100),
                               HTML(paste0("<br>" ,"<b><h2><p style = 'text-align: center ; color: black'>Documentation</p></h2></b>",
                               "<p style = 'text-align: center; line-height: 25px; vertical-align: center; padding: 15px 35px; font-size: 17px'>
                               Find documentation and walkthrough of code for different decomposition functions. 
                               Work your way towards understanding decomposition analysis inside and out. </p>")), 
                               div(style="display:inline-block; width:100%; text-align: center;", 
                                   actionButton(inputId='docu_more', label="See Documentation", icon = icon("plus")))
                        )
                    ), 
                    box(
                        title = "",
                        status = "success",
                        width = 4, height = 425, 
                        column(width = 12, align = "center",
                               img(src = "users-icon.png", width=100),
                               HTML(paste0("<br>" ,"<b><h2><p style = 'text-align: center ; color: black'>Community</p></h2></b>",
                               "<p style = 'text-align: center; line-height: 25px; vertical-align: center; padding: 15px 35px; font-size: 17px'>
                               Get your questions answered by our community of actuaries and developers. 
                               Connect with us to build your network, and see the latest projects being worked on. </p>")), 
                               div(style="display:inline-block; width:100%; text-align: center;", 
                                   actionButton(inputId='community_more', label="Meet Us", icon = icon("plus")))
                        )
                    ), 
                    box(
                        title = "",
                        status = "success",
                        width = 12, height = 425, 
                        slickROutput("slickr", width = '500px')
                    )
                    
            )
        ),
        
        # About - tab_introduction of the web application -------------------------------------------------------
        tabItem(
            "tab_explore", "Explore!",
        ),
        tabItem(
            "tab_docu", "Documentation!",
        ),
        tabItem(
            "tab_community", "people!",
        ),
        
        # About - tab_introduction of the web application -------------------------------------------------------
        tabItem(
            "tab_news", 
            fluidRow(
                box(
                    title = ,
                    status = "primary",
                    width = 6,
                    userPost(
                        id = 1,
                        src = "https://fiverr-res.cloudinary.com/t_profile_original,q_auto,f_auto/attachments/profile/photo/a8ece8a3bb5951b6a9ffe9a19063327a-1537916496823/Mushu%20Glasses.jpg",
                        author = "The SOA Explores the Role of Actuaries in the Face of the COVID-19 Situation",
                        description = "R. Dale Hall",
                        "R. Dale Hall, FSA, CERA, MAAA, CFA managing director of Research for the Society of Actuaries (SOA) explains 
                        the many roles actuaries can have as COVID-19 spreads around the world.", HTML("</br></br>"),
                        "Schaumburg, Ill., March 10, 2020 - The past few weeks have seen the emergence and spread of a novel coronavirus named 
                        'SARS-CoV2' that causes the respiratory disease named 'coronavirus disease 2019' (COVID-19).  While original detection 
                        occurred in China, the U.S. Centers for Disease Control and Prevention (CDC) note that the disease is now detected in 60 
                        locations internationally, including the United States.", HTML("</br>"),
                        div(style="display:inline-block; width:180%; text-align: center;", actionButton(inputId='ab1', label="Learn More", 
                                            icon = icon("plus"), 
                                            onclick ="window.open('https://www.soa.org/resources/announcements/press-releases/2020/2020-covid-19-situation/', '_blank')"))
                    ),
                    userPost(
                        id = 2,
                        src = "https://fiverr-res.cloudinary.com/t_profile_original,q_auto,f_auto/attachments/profile/photo/a8ece8a3bb5951b6a9ffe9a19063327a-1537916496823/Mushu%20Glasses.jpg",
                        author = "Test Post 2",
                        description = "User Name",
                        "lalalallala",
                        userPostMedia(src = "https://adminlte.io/themes/AdminLTE/dist/img/photo2.png"),
                        
                    )
                ), 
                box(
                    title = ,
                    status = "warning",
                    width = 6,
                    userPost(
                        id = 1,
                        src = "https://fiverr-res.cloudinary.com/t_profile_original,q_auto,f_auto/attachments/profile/photo/a8ece8a3bb5951b6a9ffe9a19063327a-1537916496823/Mushu%20Glasses.jpg",
                        author = "Latest Updates on the Web Application",
                        description = "Benjamin Hsu",
                        "bippity boppity boo fixed this n that", 
                    )
                )
            )
        ),
        # About - tab_life exp -------------------------------------------------------
        tabItem(
            "tab_le", 
            fluidRow(
                # overview start ------------------------------------------------######################################product list here, user post in tab before
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
                        <span style = "padding: 0 60px; display:inline-block; word-wrap:break-word;"> The Decomposition of Mortality by Age is responsible of observing the life expectancy changes over a time interval for a select country.</span>
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
                ),
                
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
                                      column(width = 12,
                                             checkboxGroupButtons(
                                                 inputId = "CODGender", label = "Gender", 
                                                 choices = c("Male", "Female"), selected = "Male", 
                                                 justified = TRUE, status = "primary")
                                      )
                                 )
                           ), 
                           column(width = 9, 
                                  # Input: Specification of range within an interval ----
                                  wellPanel( 
                                      conditionalPanel(
                                          condition = "input.CODCountry == 'Canada'" ,
                                          sliderInput("range_tcod",
                                                      label = "Years Selected",
                                                      min = 1950, max = 2009, value = c(1950, 2009))
                                      ),
                                      conditionalPanel(
                                          condition = "input.CODCountry == 'Czech Republic'",
                                          sliderInput("range_tcod",
                                                      label = "Years Selected",
                                                      min = 1950, max = 2013, value = c(1950, 2013))
                                      ),
                                      conditionalPanel(
                                          condition = "input.CODCountry == 'France'",
                                          sliderInput("range_tcod",
                                                      label = "Years Selected",
                                                      min = 1958, max = 2013, value = c(1958, 2013))
                                      ),
                                      conditionalPanel(
                                          condition = "input.CODCountry == 'United Kingdom'",
                                          sliderInput("range_tcod",
                                                      label = "Years Selected",
                                                      min = 1950, max = 2014, value = c(1950, 2014))
                                      ),
                                      conditionalPanel(
                                          condition = "input.CODCountry == 'Japan'",
                                          sliderInput("range_tcod",
                                                      label = "Years Selected",
                                                      min = 1950, max = 2013, value = c(1950, 2013))
                                      ),
                                      conditionalPanel(
                                          condition = "input.CODCountry == 'Norway'",
                                          sliderInput("range_tcod",
                                                      label = "Years Selected",
                                                      min = 1951, max = 2012, value = c(1951, 2012))
                                      ),
                                      conditionalPanel(
                                          condition = "input.CODCountry == 'Sweden'",
                                          sliderInput("range_tcod",
                                                      label = "Years Selected",
                                                      min = 1952, max = 2012, value = c(1952, 2012))
                                      ),
                                      conditionalPanel(
                                          condition = "input.CODCountry == 'USA'",
                                          sliderInput("range_tcod",
                                                      label = "Years Selected",
                                                      min = 1959, max = 2015, value = c(1959, 2015))
                                      )
                                  ) # wellPanel
                           ) # column
                       ) #fluidrow
                   )
                )
            ),
            
            fluidRow(
                
                column(width = 12,
                       
                       tabBox(width = NULL, title = tagList(shiny::icon("hand-holding-heart"), "By Age and COD"), 
                                   tabPanel(title = "Mortality Chapters by Country", id = "tabset22", 
                                            
                                            fluidRow(
                                                column(width = 6,
                                                       withSpinner(plotlyOutput("Animate_MCBar", height = 800))
                                                ), 
                                                column(width = 6, 
                                                       withSpinner(DT::dataTableOutput("Animate_Table"))
                                                )
                                            )
                                   ),
                                   tabPanel(title = "Change in Life Expectancy", id = "tabset33", 
                                            
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
                                   ), 
                                   tabPanel(title = "Change in Life Preparancy",
                                           
                                            fluidRow(
                                                column(width = 10,
                                                       withSpinner(plotlyOutput("HeatMapLP_COD", height = 600)), 
                                                ), 
                                                column(width = 2, 
                                                       numericInput("CODz", "Percentile", value = 0.9, min = 0, max = 1, step = 0.1)
                                                )
                                               
                                            )
                                  ),
                                  tabPanel(title = "Contribution to Gender Gap",
                                           
                                           fluidRow(
                                               column(width = 10,
                                                      withSpinner(plotlyOutput("GapAgeCOD", height = 600)), 
                                               )
                                               
                                           )
                                  )
                       )
                )
                
                
                
                
            )
            
        ),
                    
        
        # About - tab_about -------------------------------------------------------##########################timeline?
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
                )
            ), 
    
        tabItem( 
            "tab_qa",
            fluidRow(
                box(
                    title = "Frequently Asked Questions",
                    width = 12,
                    accordion(
                        accordionItem(
                            id = 1,
                            title = "Decomposition by Age",
                            color = "danger",
                            collapsed = FALSE,
                            "This is some text! I'm basicallly wondering how long this text can go and whether or not it will just be nice and automatically wrap to the next line or is it just goinng to be ike the other stuff and not wrap at all and be a bummer function. I guess if we continue to keep typing like this into a big paragraph we can be hopeful that it will actullay become some sort of stuff and wrap like a nce one"
                        ),
                        accordionItem(
                            id = 2,
                            title = "Accordion Item 2",
                            color = "warning",
                            collapsed = FALSE,
                            "This is some text!"
                        ),
                        accordionItem(
                            id = 3,
                            title = "Accordion Item 3",
                            color = "info",
                            collapsed = FALSE,
                            "This is some text!"
                        )
                    )
                )
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
    return(list(table_change, Total_Change_per_age_melt, result_WT))
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

mortality_chapter_rates <- function(cntry, t1, t2){
    #import the file
    COD_Info <- read.csv(paste0("COD_5x1_chapters_", cntry, ".csv"))
    rates_per_chapter_males <- dcast(COD_Info,Year+COD.chap~Age,value.var = paste0("Rates.M"))
    rates_per_chapter_females <- dcast(COD_Info,Year+COD.chap~Age,value.var = paste0("Rates.F"))
    
    #parameters
    mortality_chapters <- max(rates_per_chapter_males$COD.chap)-1 #20 mortality chapters, 21st is total
    age_groups <- length(unique(COD_Info$Age)) #number of age groups
    
    #animation rates
    rates_animate_M <- as.matrix(rates_per_chapter_males[rates_per_chapter_males$Year == t1 | rates_per_chapter_males$Year == t2, -(1:2)])/1000
    rates_animate_F <- as.matrix(rates_per_chapter_females[rates_per_chapter_females$Year == t1 | rates_per_chapter_females$Year == t2, -(1:2)])/1000
    rates_animate_labels <- data.frame(chapter = rep(1:(mortality_chapters+1), 2), 
                                       year = rep(c(t1, t2), each = mortality_chapters+1)) 
    #final rates
    rates_animate_Mf <- cbind(rates_animate_labels, data.frame(Gender = "Male"), rates_animate_M) 
    rates_animate_Ff <- cbind(rates_animate_labels, data.frame(Gender = "Female"), rates_animate_F)
    rates_animate_final <- rbind(rates_animate_Mf, rates_animate_Ff)
    
    return(rates_animate_final)
}

GenGap_AgeCOD <- function(cntry, t){
    #import the file
    COD_Info <- read.csv(paste0("COD_5x1_chapters_", cntry, ".csv"))
    
    #fractional mortality rates
    Cause_Mortality_Fractions_M<-dcast(COD_Info,Year+COD.chap~Age,value.var = "COD.frac.M")
    Cause_Mortality_Fractions_F<-dcast(COD_Info,Year+COD.chap~Age,value.var = "COD.frac.F")
    rates_per_chapter_males<-dcast(COD_Info,Year+COD.chap~Age,value.var = "Rates.M")
    rates_per_chapter_females<-dcast(COD_Info,Year+COD.chap~Age,value.var = "Rates.F")
    
    ##Extraction of information for year t
    Cause_Mortality_Fractions_F_t<-as.matrix(Cause_Mortality_Fractions_F[Cause_Mortality_Fractions_F$Year==t,-(1:2)])#Just the specific year required and no need for descriptive columns
    Cause_Mortality_Fractions_M_t<-as.matrix(Cause_Mortality_Fractions_M[Cause_Mortality_Fractions_M$Year==t,-(1:2)])
    
    #parameters
    mortality_chapters <- max(rates_per_chapter_males$COD.chap)-1 #20 mortality chapters, 21st is total
    age_groups <- length(unique(COD_Info$Age)) #number of age groups
    
    ###This section completes the matrix for cases of countries with years missing information about certain advanced
    ###ages. For example, the case of CZE for ages 85-90,90-95,95-100,100-105 before year 1986. In such a case,
    #These ages are filled with the value in age 85 (which corresponds to 85+)
    for(k in 1:(mortality_chapters+1)){
        ##Detection of last column with non NA values. "lcgt" stands for last column with info gender year i 
        lcft<-dim(Cause_Mortality_Fractions_F_t)[2]-length(Cause_Mortality_Fractions_F_t[k,which(is.na(Cause_Mortality_Fractions_F_t[k,]))])
        lcmt<-dim(Cause_Mortality_Fractions_M_t)[2]-length(Cause_Mortality_Fractions_M_t[k,which(is.na(Cause_Mortality_Fractions_M_t[k,]))])
        ###replacement of na 
        Cause_Mortality_Fractions_F_t[k,is.na(Cause_Mortality_Fractions_F_t[k,])]<-Cause_Mortality_Fractions_F_t[k,lcft]
        Cause_Mortality_Fractions_M_t[k,is.na(Cause_Mortality_Fractions_M_t[k,])]<-Cause_Mortality_Fractions_M_t[k,lcmt]
    }
    #extract only rates per chapter for year t
    rates_per_chapter_males_t<-as.matrix(rates_per_chapter_males[rates_per_chapter_males$Year==t,-(1:2)])
    rates_per_chapter_females_t<-as.matrix(rates_per_chapter_females[rates_per_chapter_females$Year==t,-(1:2)])
    
    ###This section completes the matrix for cases of countries with years missing information about certain advanced
    ###ages. For example, the case of CZE for ages 85-90,90-95,95-100,100-105 before year 1986. In such a case,
    #These ages are filled with the value in age 85 (which corresponds to 85+)
    for(k in 1:(mortality_chapters+1)){
        ##Detection of last column with non NA values. "lcgi" stands for last column gender year i 
        lcmt<-dim(rates_per_chapter_males_t)[2]-length(rates_per_chapter_males_t[k,which(is.na(rates_per_chapter_males_t[k,]))])
        lcft<-dim(rates_per_chapter_females_t)[2]-length(rates_per_chapter_females_t[k,which(is.na(rates_per_chapter_females_t[k,]))])
        ###replacement of na 
        rates_per_chapter_males_t[k,is.na(rates_per_chapter_males_t[k,])]<-rates_per_chapter_males_t[k,lcmt]
        rates_per_chapter_females_t[k,is.na(rates_per_chapter_females_t[k,])]<-rates_per_chapter_females_t[k,lcft]
    }
    
    return(list(Cause_Mortality_Fractions_M_t, Cause_Mortality_Fractions_F_t, rates_per_chapter_males_t, rates_per_chapter_females_t))
}


server <- shinyServer(function(input, output, session){ 

        #homepage reactie action buttons
        observeEvent(input$explore_more, {
            newtab <- switch(input$tabs_all, "tab_home" = "tab_explore")
            updateTabItems(session, "tabs_all", newtab)
        })
        observeEvent(input$docu_more, {
            newtab <- switch(input$tabs_all, "tab_home" = "tab_docu")
            updateTabItems(session, "tabs_all", newtab)
        })
        observeEvent(input$community_more, {
            newtab <- switch(input$tabs_all, "tab_home" = "tab_community")
            updateTabItems(session, "tabs_all", newtab)
        })
        
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
        
        #extract from mortality_chapter_rates for specific age group
        chapter_rates_select <- reactive({
            req(input$CODCountry)
            #import the file
            chosen_country <- as.character(COD_countries()$code[which(COD_countries()$country == input$CODCountry)])
            chapter_rates <- mortality_chapter_rates(chosen_country, input$range_tcod[1], input$range_tcod[2])
            
            #picking your age group 
            age_initial <- colnames(chapter_rates[, -c(1:3)])
            selected_agegrp <- max(which((input$CODAge >= as.numeric(age_initial)) == TRUE)) + 3
            #find prop percent
            chapter_rates_select <- data.frame(chapter_rates[,c(1:3)], rate = chapter_rates[,selected_agegrp])
            
            #calculating proportion
            chapter_rates_select$perct = chapter_rates_select$rate/rep(chapter_rates_select[chapter_rates_select$chapter == 21,]$rate, each = 21)
            chapter_rates_select$chapter <- as.factor(chapter_rates_select$chapter)
            chapter_rates_select <- droplevels(chapter_rates_select[-which(chapter_rates_select$chapter == 21),])
            return(chapter_rates_select)
        })
        
        # for maintaining the state of drill-down variables
        Animate_MCBar <- reactiveVal()

        # when clicking on a category, 
        observeEvent(event_data("plotly_click", source = "Animate_MCBar"), {
            Animate_MCBar(event_data("plotly_click", source = "Animate_MCBar")$y)
        })
        
        
        output$Animate_MCBar <- renderPlotly({
            #gender select
            if (length(input$CODGender) == 1){
                
                chapter_rates_select_t1 <- chapter_rates_select()[which(chapter_rates_select()$Gender == input$CODGender & 
                                                                            chapter_rates_select()$year == input$range_tcod[1]),] 
                chapter_rates_select_t2 <- chapter_rates_select()[which(chapter_rates_select()$Gender == input$CODGender & 
                                                                            chapter_rates_select()$year == input$range_tcod[2]),] 
                chapter_rates_diff <- data.frame(chapter = rep(1:(max(chapters20()$chapter))),
                                                 chapter_rates_select_t2[,4:5] - chapter_rates_select_t1[,4:5])
                chapter_rates_diff <- merge(chapter_rates_diff, chapters20(), by = "chapter")
                
                #if no click
                if (is.null(Animate_MCBar())){
                    chapter_rates_select_diff <- chapter_rates_diff %>% mutate(curr_col = "#FDB863")
                }
                else{
                    chapter_rates_select_diff <- chapter_rates_diff %>% mutate(curr_col = if_else(diagn %in% Animate_MCBar(), "#8073AC", "#FDB863"))
                }
                #plot barplot
                plot_ly(chapter_rates_select_diff, x = ~rate, y = ~diagn, type = "bar", marker = list(color = ~curr_col),
                        source = "Animate_MCBar", text = ~paste0("Chapter: ", diagn, '</br></br>', 
                                                                 "Change: ", round(rate, 4), '</br>',
                                                                 "Proportion: ", paste0(round(perct*100, 4), "%")), 
                        hoverinfo = "text", orientation = 'h') %>% 
                    config(displayModeBar = FALSE) %>% 
                    layout(barmode="overlay", title = paste0("Changes in Rate per 100,000 (",
                                                             input$range_tcod[1], "-", input$range_tcod[2], ", ", 
                                                             input$CODGender, ", ", input$CODCountry, ")"),      
                           font = list(size = 8), xaxis = list(title = "Change in Rate (per 100,000)", 
                               categoryarray = ~diagn, 
                               categoryorder = "array", size = 8, tickangle = 0,
                               showgrid = FALSE), 
                           yaxis = list(title = "", autorange="reversed", showgrid = TRUE))
            }
            else{ 
                chapter_rates_select_t1_g1 <- chapter_rates_select()[which(chapter_rates_select()$Gender == input$CODGender[1] & 
                                                                               chapter_rates_select()$year == input$range_tcod[1]),] 
                chapter_rates_select_t2_g1 <- chapter_rates_select()[which(chapter_rates_select()$Gender == input$CODGender[1] & 
                                                                               chapter_rates_select()$year == input$range_tcod[2]),] 
                chapter_rates_select_t1_g2 <- chapter_rates_select()[which(chapter_rates_select()$Gender == input$CODGender[2] & 
                                                                               chapter_rates_select()$year == input$range_tcod[1]),] 
                chapter_rates_select_t2_g2 <- chapter_rates_select()[which(chapter_rates_select()$Gender == input$CODGender[2] & 
                                                                               chapter_rates_select()$year == input$range_tcod[2]),] 
                chapter_rates_diff1 <- data.frame(chapter = rep(1:(max(chapters20()$chapter))),
                                                  chapter_rates_select_t2_g1[,4:5] - chapter_rates_select_t1_g1[,4:5])
                chapter_rates_diff1 <- merge(chapter_rates_diff1, chapters20(), by = "chapter")
                chapter_rates_diff2 <- data.frame(chapter = rep(1:(max(chapters20()$chapter))),
                                                  chapter_rates_select_t2_g2[,4:5] - chapter_rates_select_t1_g2[,4:5])
                chapter_rates_diff2 <- merge(chapter_rates_diff2, chapters20(), by = "chapter")
                
                print(Animate_MCBar())
                if (is.null(Animate_MCBar())){
                    #male/female -- gender 1
                    chapter_rates_select_diff1 <- chapter_rates_diff1 %>% mutate(curr_col = "#FDB863")
                    #male/female -- gender 2
                    chapter_rates_select_diff2 <- chapter_rates_diff2 %>% mutate(curr_col = "#FD6363")
                }
                else{
                    #male/female -- gender 1
                    chapter_rates_select_diff1 <- chapter_rates_diff1 %>% mutate(curr_col = if_else(diagn %in% Animate_MCBar(), "#8073AC", "#FDB863"))
                    #male/female -- gender 2
                    chapter_rates_select_diff2 <- chapter_rates_diff2 %>% mutate(curr_col = if_else(diagn %in% Animate_MCBar(), "#92CCDE", "#FD6363"))
                }
                #plot 
                plot_ly(data = chapter_rates_select_diff1, x = ~rate, y = ~diagn, type = "bar", 
                        name = input$CODGender[1], marker = list(color = ~curr_col),
                        source = "Animate_MCBar", text = ~paste0("Chapter: ", diagn, '</br></br>', 
                                                                 "Gender: ", input$CODGender[1], '</br>', 
                                                                 "Year: ", paste0(input$range_tcod[1], "-", input$range_tcod[2]), '</br>',  
                                                                 "Change: ", round(rate, 4), '</br>',
                                                                 "Proportion: ", paste0(round(perct*100, 4), "%")), 
                        hoverinfo = "text", orientation = 'h') %>% 
                    config(displayModeBar = FALSE) %>% 
                    add_trace(data = chapter_rates_select_diff2, x = ~rate, y = ~diagn, type = "bar", 
                              name = input$CODGender[2], marker = list(color = ~curr_col), 
                              text = ~paste0("Chapter: ", diagn, '</br></br>', 
                                             "Gender: ", input$CODGender[2], '</br>', 
                                             "Year: ", paste0(input$range_tcod[1], "-", input$range_tcod[2]), '</br>', 
                                             "Change: ", round(rate, 4), '</br>',
                                             "Proportion: ", paste0(round(perct*100, 4), "%")), 
                              hoverinfo = "text", orientation = 'h') %>% 
                    layout(barmode="group", title = paste0("Changes in Death Rate per 100,000 (",
                                                           input$range_tcod[1], "-", input$range_tcod[2], ", ", 
                                                           paste(input$heatGender, collapse = "/"), ", ", input$CODCountry, ")"),      
                           font = list(size = 8), xaxis = list(title = "Change in Rate (per 100,000)", 
                                                               categoryarray = ~diagn, 
                                                               categoryorder = "array", size = 10, tickangle = 0, showgrid = FALSE), 
                           yaxis = list(title = "", autorange="reversed", showgrid = TRUE))
            }
        })
        
        output$Animate_Table <- DT::renderDataTable({
            if (length(input$CODGender) == 1){
                if (is.null(Animate_MCBar())){
                    chapter_rates_select_ord <- merge(chapter_rates_select()[which(chapter_rates_select()$Gender == input$CODGender),], 
                                                      chapters20(), by = "chapter")[, c("diagn", "year", "Gender", "rate", "perct")]
                    chapter_rates_select_ord <- chapter_rates_select_ord[order(chapter_rates_select_ord$diagn, chapter_rates_select_ord$year),]
                }
                else{
                    chapter_rates_select_ord1 <- merge(chapter_rates_select()[which(chapter_rates_select()$chapter == 
                                                                                           which(chapters20()$diagn %in% Animate_MCBar()) &
                                                                                     chapter_rates_select()$Gender == input$CODGender),],
                                                             chapters20(), by = "chapter")[, c("diagn", "year", "Gender", "rate", "perct")]
                    chapter_rates_select_ord2 <- merge(chapter_rates_select()[-which(chapter_rates_select()$chapter == 
                                                                                           which(chapters20()$diagn %in% Animate_MCBar()) &
                                                                                     chapter_rates_select()$Gender == input$CODGender),], 
                                                             chapters20(), by = "chapter")[, c("diagn", "year", "Gender", "rate", "perct")]
                    chapter_rates_select_ord1 <- chapter_rates_select_ord1[order(chapter_rates_select_ord1$diagn, chapter_rates_select_ord1$year),]
                    chapter_rates_select_ord2 <- chapter_rates_select_ord2[order(chapter_rates_select_ord2$diagn, chapter_rates_select_ord2$year),]
                    #change column names
                    chapter_rates_select_ord <- rbind(chapter_rates_select_ord1, chapter_rates_select_ord2)
                }
            }
            else{
                if (is.null(Animate_MCBar())){
                    chapter_rates_select_ord <- merge(chapter_rates_select()[which(chapter_rates_select()$Gender == input$CODGender),], 
                                                      chapters20(), by = "chapter")[, c("diagn", "year", "Gender", "rate", "perct")]
                    chapter_rates_select_ord <- chapter_rates_select_ord[order(chapter_rates_select_ord$diagn, chapter_rates_select_ord$year),]
                }
                else{
                    chapter_rates_select_ord1 <- merge(chapter_rates_select()[which(chapter_rates_select()$chapter == 
                                                                                        which(chapters20()$diagn %in% Animate_MCBar())),],
                                                       chapters20(), by = "chapter")[, c("diagn", "year", "Gender", "rate", "perct")]
                    chapter_rates_select_ord2 <- merge(chapter_rates_select()[-which(chapter_rates_select()$chapter == 
                                                                                        which(chapters20()$diagn %in% Animate_MCBar())),], 
                                                       chapters20(), by = "chapter")[, c("diagn", "year", "Gender", "rate", "perct")]
                    chapter_rates_select_ord1 <- chapter_rates_select_ord1[order(chapter_rates_select_ord1$diagn, chapter_rates_select_ord1$year),]
                    chapter_rates_select_ord2 <- chapter_rates_select_ord2[order(chapter_rates_select_ord2$diagn, chapter_rates_select_ord2$year),]
                    #rbind two columns
                    chapter_rates_select_ord <- rbind(chapter_rates_select_ord1, chapter_rates_select_ord2)
               }
            }
            colnames(chapter_rates_select_ord) <- c("Mortality Chapter", "Year", "Gender", "Death Rate/100000", "Proportion (%)")
            chapter_rates_select_ord[,"Proportion (%)"] <- chapter_rates_select_ord[,"Proportion (%)"]*100
            chapter_rates_select_ord[,4:5] <- round(chapter_rates_select_ord[,4:5], 4)
            chapter_rates_select_ord}, 
            options = list(searching = FALSE, lengthMenu = c(20, 40)), rownames = FALSE
        )
        
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
        
        # for maintaining the state of drill-down variables
        BarplotLE_AgeCOD <- reactiveVal()
        BarplotLE_specificAgeCOD <- reactiveVal()
        
        # when clicking on a category, 
        observeEvent(event_data("plotly_click", source = "BarplotLE_AgeCOD"), {
            BarplotLE_AgeCOD(event_data("plotly_click", source = "BarplotLE_AgeCOD")$x)
            BarplotLE_specificAgeCOD(NULL)
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
                hover_text <- matrix(paste0("Age: ", sapply(colnames(res_gender), function(x) rep(x, dim1)), "<br>", 
                                     "Mortality Chapter: ", rep(chapters20()$diagn, dim2), "<br>", 
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
                           yaxis = list(ticktext = rev(chapters20()$diagn), title = "",
                                       # title = paste0(c(rep("&nbsp;", 20), "Contribution",
                                       #                  rep("&nbsp;", 20), rep("\n&nbsp;", 1)), collapse = ""),
                                        showgrid = F, showticklabels = TRUE))
                
            }
            else{ 
                #if (input$heatAggregate == "FALSE"){ 
                res_gender <- Changes_age_cause()[[2]] - Changes_age_cause()[[1]]
                
                #par(mar=c(5.1,5.1,1,2.1))
                dim1 <- dim(res_gender)[[1]]
                dim2 <- dim(res_gender)[[2]]
                hover_text <- matrix(paste0("Age: ", sapply(colnames(res_gender), function(x) rep(x, dim1)), "<br>", 
                                            "Mortality Chapter: ", rep(chapters20()$diagn, dim2), "<br>", 
                                            rep("Female - Male", dim1*dim2), "<br>"),
                                     byrow = FALSE, ncol = dim2)
                hover_text2 <- matrix(paste(hover_text, round(res_gender, 4), sep="Change: "), dim1, dim2)
                
                heatmaply(res_gender, dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
                          cexRow = 0.9, cexCol = 0.9, col = scale_colors,  
                          plot_method = c("plotly"), main = paste0("Changes in Life Expectancy (", 
                                                                   input$range_tcod[1], "-", input$range_tcod[2], ", ",
                                                                   "Female - Male", ", ", input$CODCountry, ")"), 
                          font = list(size = 8), custom_hovertext = hover_text2, 
                          key.title = "Changes in Years", colorbar_xpos = 30, colorbar_ypos = 10) %>% 
                    layout(xaxis = list(ticktext = as.numeric(colnames(res_gender)), title = "Age", 
                                        showgrid = F, tickangle = 0, showticklabels = TRUE), 
                           yaxis = list(ticktext = rev(chapters20()$diagn), title = "",
                                       # title = paste0(c(rep("&nbsp;", 20), "Contribution",
                                       #                  rep("&nbsp;", 20), rep("\n&nbsp;", 1)), collapse = ""),
                                        showgrid = F, showticklabels = TRUE))
            }
        })
        
       ### life preparancy for COD 
        
        # output
        Changes_age_cause_LP <- reactive({
            #life preparancy chosen country
            req(input$CODCountry)
            chosen_country <- as.character(COD_countries()$code[which(COD_countries()$country == input$CODCountry)])
            #life preparancy
            res <- change5x1_LP(chosen_country, input$range_tcod[1], input$range_tcod[2], input$CODz)
            res[[2]][is.na(res[[2]])]<-0 #LP change males set to 0 for NAs
            res[[3]][is.na(res[[3]])]<-0 #LP change females set to 0 for NAs
            #mortality chapters
            res_COD <- change5x1_AgeCOD(chosen_country, input$range_tcod[1], input$range_tcod[2])
            #age group 
            mortality_chapters = 20
            age_initial <- gsub("LP Age ", "", colnames(res[[2]]))
            age_groups <- dim(res_COD[[1]])[2]
            selected_agegrp <- max(which((input$CODAge >= as.numeric(age_initial)) == TRUE))
            
            #final output
            Changes_age_cause_male<-matrix(0,nrow=mortality_chapters,ncol=age_groups-1)
            Changes_age_cause_female<-matrix(0,nrow=mortality_chapters,ncol=age_groups-1)
            for(x in 1:age_groups-1){
                Changes_age_cause_male[,x]<-res[[2]][x,selected_agegrp]*res_COD[[1]][,x] #res[[2]] for male
                Changes_age_cause_female[,x]<-res[[3]][x,selected_agegrp]*res_COD[[2]][,x] #res[[3]] for female
            }
            colnames(Changes_age_cause_male) <- age_initial
            colnames(Changes_age_cause_female) <- age_initial
            rownames(Changes_age_cause_male) <- 1:mortality_chapters
            rownames(Changes_age_cause_female) <- 1:mortality_chapters
            return(list(Changes_age_cause_male, Changes_age_cause_female))
        })
        
        output$HeatMapLP_COD <- renderPlotly({
            
            chosen_country <- as.character(COD_countries()$code[which(COD_countries()$country == input$CODCountry)])
            scale_colors <- brewer.pal(n=9, name = "YlOrRd") #selection of
            
            if (length(input$CODGender) == 1){
                if (input$CODGender == "Male" ){ res_gender <- Changes_age_cause_LP()[[1]] }
                else if (input$CODGender == "Female" ){ res_gender <- Changes_age_cause_LP()[[2]]}
                
                dim1 <- dim(res_gender)[[1]]
                dim2 <- dim(res_gender)[[2]]
                hover_text <- matrix(paste0("Age: ", sapply(colnames(res_gender), function(x) rep(x, dim1)), "<br>", 
                                            "Mortality Chapter: ", rep(chapters20()$diagn, dim2), "<br>", 
                                            rep(input$CODGender, dim1*dim2), "<br>"),
                                     byrow = FALSE, ncol = dim2)
                hover_text2 <- matrix(paste(hover_text, round(res_gender, 4), sep="Change: "), dim1, dim2)
                
                heatmaply(res_gender, dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
                          cexRow = 0.9, cexCol = 0.9, col = scale_colors,  
                          plot_method = c("plotly"), main = paste0("Changes in Life Preparancy (", 
                                                                   input$range_tcod[1], "-", input$range_tcod[2], ", ",
                                                                   input$CODGender, ", ", input$CODCountry, ")"), 
                          font = list(size = 8), custom_hovertext = hover_text2, 
                          key.title = "Changes in Years", colorbar_xpos = 30, colorbar_ypos = 10) %>% 
                    layout(xaxis = list(ticktext = as.numeric(colnames(res_gender)), title = "Age", 
                                        showgrid = F, tickangle = 0, showticklabels = TRUE), 
                           yaxis = list(ticktext = rev(chapters20()$diagn), title = "",
                                        showgrid = F, showticklabels = TRUE))
                
            }
            else{ 
                res_gender <- Changes_age_cause_LP()[[2]] - Changes_age_cause_LP()[[1]]
                #dimensions
                dim1 <- dim(res_gender)[[1]]
                dim2 <- dim(res_gender)[[2]]
                hover_text <- matrix(paste0("Age: ", sapply(colnames(res_gender), function(x) rep(x, dim1)), "<br>", 
                                            "Mortality Chapter: ", rep(chapters20()$diagn, dim2), "<br>", 
                                            rep("Female - Male", dim1*dim2), "<br>"),
                                     byrow = FALSE, ncol = dim2)
                hover_text2 <- matrix(paste(hover_text, round(res_gender, 4), sep="Change: "), dim1, dim2)
                
                heatmaply(res_gender, dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
                          cexRow = 0.9, cexCol = 0.9, col = scale_colors,  
                          plot_method = c("plotly"), main = paste0("Changes in Life Preparancy (", 
                                                                   input$range_tcod[1], "-", input$range_tcod[2], ", ",
                                                                   "Female - Male", ", ", input$CODCountry, ")"), 
                          font = list(size = 8), custom_hovertext = hover_text2, 
                          key.title = "Changes in Years", colorbar_xpos = 30, colorbar_ypos = 10) %>% 
                    layout(xaxis = list(ticktext = as.numeric(colnames(res_gender)), title = "Age", 
                                        showgrid = F, tickangle = 0, showticklabels = TRUE), 
                           yaxis = list(ticktext = rev(chapters20()$diagn), title = "",
                                        showgrid = F, showticklabels = TRUE))
            }
        })
        
        ### start of gender gap tab
        
        Changes_age_cause_gap <- reactive({
            
            #life expectancy
            req(input$CODCountry)
            chosen_country <- as.character(COD_countries()$code[which(COD_countries()$country == input$CODCountry)])
            
            #GenGap function for per age outputs: change per age (unmelt, melt), change per age lower triangle
            Gender_Gap_Age <- GenGap_Age(chosen_country, input$range_tcod[1])
            #GenGap function outputs: mortality fractions M,F and mortality rates per chapter M,F
            res <- GenGap_AgeCOD(chosen_country, input$range_tcod[1])
            
            #parameters
            mortality_chapters = max(chapters20()$chapter) #number of chapters
            age_initial <- colnames(res[[1]])
            age_groups <- dim(res[[1]])[2] #number of agegroups
            selected_agegrp <- max(which((input$CODAge >= as.numeric(age_initial)) == TRUE))
            
            #total rates
            Total_rate_M <- res[[3]][mortality_chapters+1,]
            Total_rate_F <- res[[4]][mortality_chapters+1,] 
            
            #gender gap distribution
            Gender_Gap_Distribution <- Gender_Gap_Age[[3]][-nrow(Gender_Gap_Age[[3]]), selected_agegrp]
            Contribution_per_cause<-matrix(0,nrow=mortality_chapters,ncol=age_groups)
            ##difference in ratios to calc gender gap by mortality chapter
            Ratio_of_Differences<-matrix(0,nrow=mortality_chapters,ncol=age_groups)
            for(i in 1:mortality_chapters){
                Ratio_of_Differences[i,] <- (res[[2]][i,]*Total_rate_F-res[[1]][i,]*Total_rate_M)/(Total_rate_F-Total_rate_M)    
                Contribution_per_cause[i,] <- Ratio_of_Differences[i,]*Gender_Gap_Distribution                           
            }
            Contribution_per_cause[is.na(Contribution_per_cause)] <- 0 ##NAs may be generated if rate in denominator is zero for an age(happens for 105+)
            Total_Cause<-apply(Contribution_per_cause,1,sum)
            result<-cbind(Contribution_per_cause,Total_Cause)
            colnames(result) = c(age_initial, "Total")
            return(result)
            
        })
        
        output$GapAgeCOD <- renderPlotly({
            
            scale_colors <- brewer.pal(n=9, name = "YlOrRd") #selection of
            
            res_gender <- Changes_age_cause_gap()[,-ncol(Changes_age_cause_gap())]
            dim1 <- dim(res_gender)[[1]]
            dim2 <- dim(res_gender)[[2]]
            hover_text <- matrix(paste0("Age: ", sapply(colnames(res_gender), function(x) rep(x, dim1)), "<br>", 
                                        "Mortality Chapter: ", rep(chapters20()$diagn, dim2), "<br>", 
                                        rep("Female - Male", dim1*dim2), "<br>"),
                                 byrow = FALSE, ncol = dim2)
            hover_text2 <- matrix(paste(hover_text, round(res_gender, 4), sep="Change: "), dim1, dim2)
            
            heatmaply(res_gender, dendrogram = "none", Rowv = FALSE, Colv = FALSE, 
                      cexRow = 0.9, cexCol = 0.9, col = scale_colors,  
                      plot_method = c("plotly"), main = paste0("Gender Gap in Life Expectancy (", 
                                                               input$range_tcod[1], ", ",
                                                               "Female - Male", ", ", input$CODCountry, ")"), 
                      font = list(size = 8), custom_hovertext = hover_text2, 
                      key.title = "Changes in Years", colorbar_xpos = 30, colorbar_ypos = 10) %>% 
                layout(xaxis = list(ticktext = as.numeric(colnames(res_gender)), title = "Age", 
                                    showgrid = F, tickangle = 0, showticklabels = TRUE), 
                       yaxis = list(ticktext = rev(chapters20()$diagn), title = "",
                                    showgrid = F, showticklabels = TRUE))
        })
        

})


shinyApp(ui, server)

