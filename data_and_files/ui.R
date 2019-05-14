library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)

header <- dashboardHeader(title= "Migrant Stock and Industrial Growth", 
                          titleWidth = 350)

body <- dashboardBody(
  fluidRow(
    tags$head(
      tags$style(HTML(".leaflet-container {background: #B5DEFA;}
                      .content-wrapper, .right-side {background-color: #ffffff;}"))
    )),
  
  #overview paragraph
  fluidRow(
    column(offset = 2, width = 8, align = "left", HTML(paste("<br/>",
                                                             "<b><font size=4>Overview</font></b><br/>",
                                                             "<p align=justify>We are interested in looking at migration stock and industry growth patterns across countries. 
                                                             We will explore the following questions:<br/>",
                                                             "&nbsp &nbsp - What is the distribution of migrant flow across countries?<br/>",
                                                             "&nbsp &nbsp - How does migration flow evolve over time for each country?<br/>",
                                                             "&nbsp &nbsp - How does the value added of different industries evolve over time for each country?<br/>",
                                                             "&nbsp &nbsp - What is the relationship between migration flow and value added for each industry?<br/>",
                                                             "<b><i><font size=2>*Note: We use value added to measure the development of industry here. Value added equals 
                                                             the difference between an industry’s gross output (consisting of sales or receipts and other operating income, 
                                                             commodity taxes, and inventory change) and the cost of its intermediate inputs (including energy, raw materials, semi-finished goods, 
                                                             and services that are purchased from all sources). <a href=https://www.bea.gov/help/faq/184>See the definition by U.S. Bureau of Economic Analysis (BEA).</a></font></i></b><br/>",
                                                             
                                                             "<p align=justify>The visualisations below are built using data obtained from the following sources: <br/>",
                                                             "&nbsp &nbsp - UN data on <a href=https://www.un.org/en/development/desa/population/migration/data/estimates2/estimates17.asp>total international migrant stock</a> <br/>",
                                                             "&nbsp &nbsp - UN data on <a href=https://www.un.org/en/development/desa/population/migration/data/estimates2/estimates17.asp>international migrant stock by destination
                                                             and origin</a><br/>",
                                                             "&nbsp &nbsp - UN data on <a href=https://www.un.org/en/development/desa/population/migration/data/estimates2/estimates17.asp> total population and international migrant stock by gender</a><br/>",                                                             
                                                             "&nbsp &nbsp - UN data on <a href=http://data.un.org/Data.aspx?d=SNAAMA&f=grID%3A201%3BcurrID%3ANCU%3BpcFlag%3A0> value added by activity, 1970 - 2017</a><br/>",
                                                             "&nbsp &nbsp - UN 2008 ISIC industry standard <a href=https://unstats.un.org/unsd/publication/seriesM/seriesm_4rev4e.pdf> international standard industrial classification of all economic activities (ISIC), rev.4</a><br/>"
    )))
  ),
  
  #paragraph about first visualisation
  fluidRow(
    column(offset = 2, width = 8, align = "left", HTML(paste("<br/><p align=justify><b><font size=4>Migrant stock data at country level over time (1990 - 2017)</font></b><br/>
                                                             Interact with the map visualisation below by choosing a year of interest. The land area of each country is coloured according 
                                                             to the percentage of international migrant stock out of the country's total population. Clicking on a country will reveal
                                                             information box containing additional details about the migrant stock in the selected year. The details include the total number
                                                             of migrants, the top 3 migrant sources, etc.</p>",
                                                             "<p align=justify> From the leaftlet map, we see that there is an overall North-South divide in the number of migrants -- countries
                                                             in the North seem to generally have a darker orange/red tone while countries in the South seem to generally have a lighter green/yellow
                                                             tone. Specifically, America, Canada and Russia consistently had high percentages of migrant stock relative to the country's population across the years.
                                                             Comparatively, South America, China and India consistently had lower percentages of migrant stock relative to the 
                                                             country's population across the years. This trend persisted across the time period of 1990-2017. </p>",
                                                             "<br/>")))
  ),
  
  # inputwidget
  fluidRow(
    column(offset = 3, width = 2, align = "right", HTML(paste("<strong>Select a year:</strong>")))
  ),
  
  fluidRow(
    column(width = 12, align = "center", radioButtons(inputId = "Year", label = NA, inline = TRUE,
                                                      choices = c("1990", "1995", "2000", "2005", "2010", "2015", "2017")))
  ),
  
  #legend bar
  fluidRow(
    column(offset = 3, width = 8, align = "left", img(img(src = 'legendbar.png', height = '50px', width = '600px')))
  ),
  
  #visualisation1 -- MAP
  fluidRow(
    column(offset = 2, width = 8, align = "left", leafletOutput("mig_map", width = 810))
  ),
  
  #paragraph about second visualisation
  fluidRow(
    column(offset = 2, width = 8, align = "left", HTML(paste("<br/><br/><p align=justify><b><font size=4>Migration trend across time (1990-2017)</font></b><br/>
                                                             Interact with the plotly visualisation below by choosing a country of interest. The graph would show you
                                                             the change in total population and total migrant count of the selected country over the period of 1990 to 
                                                             2017. Feel free to play around with the legend box. <i>Click on the variable names to select or deselect
                                                             them.</i> This might allow you to make comparisons of population and migrant count of a specific gender over
                                                             time. You can also choose to focus on only population (total and by gender) over time OR focus only on 
                                                             migrant count (total and by gender) over time.</p>",
                                                             "<p align=justify> We can see that the population and migrant stock of USA increase in tandem across
                                                             the period of 1990 to 2017. Comparatively, China experienced an influx of immigrants in 2000 and this
                                                             influx was mainly made up of male immigrants. You will be able to see this trend more clear if you 
                                                             deselect the three population variables by clicking on each of them once. The female migrant stock in
                                                             China grew at a relatively stable pace from 1990 to 2017 while the male migrant stock, and consequently,
                                                             the total migrant stock, showed a notable pop in 2000.</p>",
                                                             "<br/>")))
  ),
  # inputwidget
  fluidRow(
    column(offset = 3, width = 3, align = "left", uiOutput("countrySelect"))
  ),
  
  #visualisation2 -- plotly1: migration flow
  fluidRow(
    column(offset = 2, width = 8, align = "left", plotlyOutput("plotly", width = 810, height = 400))
  ),
  
  
  #paragraph about third visualisation
  fluidRow(
    column(offset = 2, width = 8, align = "left", HTML(paste("<br/>",
                                                             "<b>* For the industry analysis below, we use <a href=http://data.un.org/Data.aspx?d=SNAAMA&f=grID%3A201%3BcurrID%3ANCU%3BpcFlag%3A0> UN value added by activity data, 1970 - 2017</a>.
                                                              It has used the <a href=https://unstats.un.org/unsd/publication/seriesM/seriesm_4rev4e.pdf> UN 2008 ISIC industry standard, rev.4</a> 
                                                             to break down the total value added by sector. *</b><br/>")))
  ),
  
  #industryinfochart
  fluidRow(
    column(offset = 2, width = 8, align = "left", img(img(src = 'industrychart.png', height = '350px', width = '820px')))
  ),
  
  #paragraph about third visualisation
  fluidRow(
    column(offset = 2, width = 8, align = "left", HTML(paste("<br/>",
                                                             "<br/><p align=justify><b><font size=4>Industry value added across time (1990-2017)</font></b><br/>",
                                                             "This visualization enables a selection of hierarchical levels. You could choose a region first, and then
                                                             choose a country in that region (if not, the default, data of all countries in the region would be selected). 
                                                             After choosing an industry of interest as well (if not, the default is the value added of all industries), 
                                                             the graph and the table would automatically present the relevant result for the country/region over time.
                                                             To make the comparison easier, country-level trends are accompanied with the regional average one. By selecting
                                                             <i>All industries of the Country/Area</i>, we are able to compare different industries within a country/area.</p>",
                                                             "From the graphs we could see that for most countries/areas in the world, <i>Other Activities (ISIC J-P)</i> contributes most to
                                                             the total value added. It includes the following industries:</p>",
                                                             "&nbsp &nbsp - J. Information and communication<br/>",
                                                             "&nbsp &nbsp - K. Financial and insurance activities<br/>",
                                                             "&nbsp &nbsp - L. Real estate activities<br/>",
                                                             "&nbsp &nbsp - M. Professional, scientific and technical activities<br/>",
                                                             "&nbsp &nbsp - N. Administrative and support service activities<br/>",
                                                             "&nbsp &nbsp - O. Public administration and defence; compulsory social security<br/>",
                                                             "&nbsp &nbsp - P. Education<br/>",
                                                             "<br/>")))
  ),
  
  
  #visualisation3 -- plot2: industry level
  fluidRow(
    # selection
    column(offset = 2, width = 2, uiOutput("regionSelection0")),
    column(width = 3, uiOutput("secondSelection0")),
    column(width = 3, uiOutput("industrySelection0"))
  ),
  fluidRow(
    # selection
    column(offset = 2, width = 8, uiOutput("allindustrySelection0"))
  ),
  
  # Main panel
  fluidRow(
    column(offset = 2, width = 8, plotlyOutput("linePlot", width = 810, height = 400))
    ),


fluidRow(
    column(offset = 2, width = 8, align = "left", HTML(paste("<br/>",
                                                             "<i><font size=1.5>&nbsp &nbsp * Value added equals the difference between an 
                                                                 industry’s gross output and the cost of its intermediate inputs. 
                                                                 <a href=https://www.bea.gov/help/faq/184>See the definition by U.S. Bureau of Economic Analysis (BEA).</a></font></i><br/>",
                                                             "<br/>")))),
  
  
  fluidRow(column(offset = 2, width = 8,
                  fluidRow(dataTableOutput("dataTable2")))),
  
  #paragraph about forth visualisation
  fluidRow(
    column(offset = 2, width = 8, align = "left", HTML(paste("<br/>",
                                                             "<br/><p align=justify><b><font size=4>Describing the relationship between migrant population and industry value added</font></b><br/>
                                                               Finally, we are able to describe some relationships. The visualization below provides a selection with more hierarchical levels. 
                                                               In addition to region and country selections, now you are able to select world level data as well. Also, options of population type are provided. 
                                                               By clicking '5-year Growth rate' options, data of 5-year growth rate for both variables are presented. It could provide more insight into the relationship between migrant stock and industrial growth. 
                                                               For instance, since both <i>Migrant Stock as % of Population</i> and <i>value added of Wholesale, retail trade, restaurants and hotels (ISIC G-H) industry</i> are 
                                                               increasing over time in Australia, the plot shows a strong positive correlation between two variables in absolute value. But when we view the relationship of 
                                                               their 5-year growth rates, the positive relationship becomes less significant. The service industry shows a different growth rate with the migrant population.
                                                               Given more available data, we could further explore whether there is a time lap between migration increment and service industry development, and 
                                                               whether there exists a causal effect.</p>",
                                                             "Regarding the region and world level data, strong heteroscedasticity may exist. You could zoom in the graphs to see more detail.</p>",
                                                             "<br/>")))),
  
  
  # side panel
  fluidRow(
    column(offset = 2, width = 2, uiOutput("regionSelection1")),
    column(width = 2, uiOutput("secondSelection1")),
    column(width = 2, uiOutput("industrySelection1"))
    ),
  
  # Main panel selections
  fluidRow(
    column(offset = 2, width = 5, uiOutput("genderSelection"))
    ),
  
  fluidRow(  
    column(offset = 2, width = 5,
           uiOutput("populationSelection"))
    ),
  
  # main panel graphs
  fluidRow(
    column(offset = 2, width = 6,
           fluidRow(plotlyOutput("scatterPlot", width = 610, height = 400))),
    column(width = 2, uiOutput("growthSelection"))
    ),
    
  fluidRow(
      column(offset = 2, width = 8, align = "left", HTML(paste("<br/>",
                                                               "<i><font size=1.5>&nbsp &nbsp * Value added equals the difference between an 
                                                                 industry’s gross output and the cost of its intermediate inputs. 
                                                                 <a href=https://www.bea.gov/help/faq/184>See the definition by U.S. Bureau of Economic Analysis (BEA).</a></font></i><br/>",
                                                               "<br/>")))
    ),
    
  fluidRow(column(offset = 2, width = 8,
                  fluidRow(dataTableOutput("dataTable3")))
           )
  
)



dashboardPage(
  header,
  dashboardSidebar(disable= TRUE),
  body
)