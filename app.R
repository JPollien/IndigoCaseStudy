library(sp)
library(rgeos)
library(rgdal)
library(dplyr)
library(leaflet)
library(scales)
library(spatialEco)
library(DT)
library(shinythemes)
library(shiny)
library(httr)
library(jsonlite)

#setwd('C:\\Users\\jpoll\\Documents\\GitHub\\IndigoCaseStudy')

# Calculation for change between years
pct <- function(x) {x/lag(x)}

##### Adding a special function to format the legend differently ####
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft", 
                                                    "topleft"), pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins	
      
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
      
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
      
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, 
                       na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}
#### end special function ####

#### Begin data prep ####
# For use in debugging
#dat <- read.csv("usda_crops_5yr.csv", sep=",", stringsAsFactors=FALSE)

# Basic format of getting JSON in R:
resp <- fromJSON("https://indigocasestudyjtp.herokuapp.com/data")

# Figure out how many pages we're gonna go through
page = range(1, sapply(strsplit(resp$links$last, "="), "[", 2))

# Preallocate DF
data <- NULL

for (i in 1:sapply(strsplit(resp$links$last, "="), "[", 2)){
  # Make it as a data frame to get around row.names issues
  response <- as.data.frame(fromJSON(paste("https://indigocasestudyjtp.herokuapp.com/data?page%5Bnumber%5D=", i, sep=""), flatten=TRUE)$data, row.names=NULL)
  if(i==1){
    data <- response
  }
  if(i>1){
    data <- rbind(data, response)
  }
  
}

data2 <- data[, -which(names(data) %in% c("type", "id", "links.self"))]
names(data2) <- sub("attributes.", "", names(data2))
dat <- data2

# Colnames tolower
names(dat) <- tolower(names(dat))
#names(dat)[1] <- "crop" # fix crop naming
names(dat)[which(names(dat)=="fips_code")] <- "GEOID" # for use merging with TIGER data
dat <- dat[, -which(names(dat) %in% "county_name")] # Remove this, we'll merge by FIPS
#dat$county_name <- tolower(dat$county_name)

# Wide data set, subset only what we need.
#county_dat <- subset(dat, measureid == "296", 
#                     select = c("reportyear","countyfips","statename", "countyname", "value", "unitname")) %>%
#  subset(reportyear==2011, select = c("countyfips", "value"))

# Rename columns to make for a clean df merge later.
#colnames(county_dat) <- c("GEOID", "airqlty")

county_dat <- dat

# Have to add leading zeos to any FIPS code that's less than 5 digits long to get a good match.
# I'm cheating by using C code. sprintf will work as well.
county_dat$GEOID <- formatC(county_dat$GEOID, width = 5, format = "d", flag = "0")

### Special handling for cotton
county_dat[which(county_dat$crop=="COTTON"),]$total_yield <- 1

county_dat$total_production <- county_dat$total_harvested_acres * county_dat$total_yield
### TEMP
#county_dat <- county_dat[which(county_dat$year=="2018"),]

county_dat2 <- county_dat[
  with(county_dat, order(GEOID, crop, year)),
]

county_dat_pct_change <- county_dat2 %>% group_by(GEOID, crop) %>% mutate_each(funs(pct), c(total_harvested_acres, total_yield, total_production))

#names(county_dat_pct_change)[c(4,6,7)] <- c("pct_change_total_harvested_acres", "pct_change_total_yield", "pct_change_total_production")
names(county_dat_pct_change)[which(names(county_dat_pct_change)=="total_harvested_acres")] <- "pct_change_total_harvested_acres"
names(county_dat_pct_change)[which(names(county_dat_pct_change)=="total_yield")] <- "pct_change_total_yield"
names(county_dat_pct_change)[which(names(county_dat_pct_change)=="total_production")] <- "pct_change_total_production"

dataset <- merge(county_dat2, county_dat_pct_change, by=c("GEOID", "crop", "year", "state_code"))

dataset$pct_change_total_harvested_acres = ifelse(!is.na(dataset$pct_change_total_harvested_acres), paste(round((dataset$pct_change_total_harvested_acres - 1)*100,1), "%"), dataset$pct_change_total_harvested_acres)

dataset$pct_change_total_yield = ifelse(!is.na(dataset$pct_change_total_yield), paste(round((dataset$pct_change_total_yield - 1)*100,1), "%"), dataset$pct_change_total_yield)

dataset$pct_change_total_production = ifelse(!is.na(dataset$pct_change_total_production), paste(round((dataset$pct_change_total_production - 1)*100,1), "%"), dataset$pct_change_total_production)

dataset2 <- reshape(dataset, idvar = c("GEOID", "year", "state_code"), timevar="crop", direction="wide")

####
# Download CB county shape file from Tiger.
# CB 20m is the smallest and will load the fastest.
us.map <- readOGR(dsn = ".", layer = "cb_2018_us_county_20m", stringsAsFactors = FALSE)


# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
#  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]
# Make sure other outling islands are removed.
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]

bigtable <- merge(us.map, dataset, by=c("GEOID"), duplicateGeoms=TRUE)
bigtable$total_production.fancy <- format(round(bigtable$total_production,0), big.mark=",")
bigtable$total_harvested_acres.fancy <- format(round(bigtable$total_harvested_acres,0), big.mark=",")
bigtable$total_yield.fancy <- format(round(bigtable$total_yield,0), big.mark=",")

pal <- colorNumeric("Spectral", NULL, n = 13)

# UI
ui <- shinyUI(fluidPage(theme = shinytheme("united"),
                        titlePanel(HTML("<h1><center><font size=10> 
                                        Tracking Crop Yields Over the Last Five Years</font></center></h1>")), 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("yearInput", label = h3("Year"),
                                        choices = sort(unique(bigtable$year)[!is.na(unique(bigtable$year))], decreasing=TRUE)),
                                        #choices = c("2014",
                                        #            "2015",
                                        #            "2016",
                                        #            "2017",
                                        #           "2018")),
                          selectInput("cropInput", label = h3("Crop"),
                                      choices = c("CORN",
                                                  "WHEAT",
                                                  "SOYBEANS",
                                                  "RICE",
                                                  "COTTON"))),
                          mainPanel(leafletOutput(outputId = 'map', height = 
                                                    800),
                          ))
))

# SERVER
server <- shinyServer(function(input, output, session) {
  
  observe({
    
    isolate({
      year_input = input$yearInput
      crop_input = input$cropInput
    })
    pal_rev <- colorNumeric(c("#d7191c","#fdae61","#ffffbf","#abd9e9", "#2c7bb6"), bigtable[!is.na(bigtable$total_production) & bigtable$year==input$yearInput & bigtable$crop==input$cropInput,]$total_production, reverse = TRUE)
    output$map <- renderLeaflet({
      leaflet(data = bigtable[!is.na(bigtable$total_production) & bigtable$year==input$yearInput & bigtable$crop==input$cropInput,]) %>% 
        addTiles() %>%
        addPolygons(fillColor = ~pal_rev(total_production),
                    fillOpacity = 0.8,
                    color = "#BDBDC3",
                    weight = 1,
                    popup = paste0("<strong>County name: </strong>",
                                   bigtable[!is.na(bigtable$total_production) & bigtable$year==input$yearInput & bigtable$crop==input$cropInput,]$NAME,
                                   "<br><strong>State: </strong>",
                                   bigtable[!is.na(bigtable$total_production) & bigtable$year==input$yearInput & bigtable$crop==input$cropInput,]$state_code,
                                   "<br><strong>Year: </strong>",
                                   bigtable[!is.na(bigtable$total_production) & bigtable$year==input$yearInput & bigtable$crop==input$cropInput,]$year,
                                   "<br><br><strong>Total production : </strong>",
                                   bigtable[!is.na(bigtable$total_production) & bigtable$year==input$yearInput & bigtable$crop==input$cropInput,]$total_production.fancy,
                                   "<br><strong>Total production (% change): </strong>",
                                   bigtable[!is.na(bigtable$total_production) & bigtable$year==input$yearInput & bigtable$crop==input$cropInput,]$pct_change_total_production,
                                   "<br><br><strong>Total acres harvested : </strong>",
                                   bigtable[!is.na(bigtable$total_production) & bigtable$year==input$yearInput & bigtable$crop==input$cropInput,]$total_harvested_acres.fancy,
                                   "<br><strong>Total acres harvested (% change) : </strong>",
                                   bigtable[!is.na(bigtable$total_production) & bigtable$year==input$yearInput & bigtable$crop==input$cropInput,]$pct_change_total_harvested_acres,
                                   "<br><br><strong>Total yield : </strong>",
                                   bigtable[!is.na(bigtable$total_production) & bigtable$year==input$yearInput & bigtable$crop==input$cropInput,]$total_yield.fancy,
                                   "<br><strong>Total yield (% change) : </strong>",
                                   bigtable[!is.na(bigtable$total_production) & bigtable$year==input$yearInput & bigtable$crop==input$cropInput,]$pct_change_total_yield)) %>%
        addLegend_decreasing("bottomright", pal = pal, values = ~total_production,
                             title = paste(input$cropInput," (total bushels per county)", sep=""),
                             opacity = 1)
      
    })
  })
})

shinyApp(ui = ui, server = server)
###############