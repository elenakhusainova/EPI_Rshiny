rm(list=ls())
library(shiny)
library(shinydashboard)
library(shinyBS)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggvis)
# devtools::install_github("elenakhusainova/EPI_Rpackage", force = TRUE)
library(EPI)

load("alldata_red.RData")
countries <- alldata$y2018raw[[1]]$country
aux <- alldata$aux
full2abbr <- c("raw" = "raw",  "indicator" = "ind")
abbr2full <- c("raw" = "raw", "ind" = "indicator")
abbr2desc <- alldata$info$Abbreviation[alldata$info$Type %in% c("Indicator", "Blend")]
names(abbr2desc) <- 
 paste0(abbr2desc, ": ", alldata$info$Description[alldata$info$Type %in% c("Indicator", "Blend")])
abbr2desc <- sort(abbr2desc)

create_alldata_red <- function(y2018raw = "../../../Work/Raw",
                               y2018ind = "../../../Work/P5_Indicator",
                               information = "master_variable_list.csv",
                               scores = "../../../Work/P6_Aggregation/EPIscores.csv", 
                               ranks = "../../../Work/P6_Aggregation/EPIranks.csv",
                               allow.na = TRUE) {
 
 # Auxiliary function to create a list of data frames for the given type 
 # and year. Essentially it reads all the .csv files from the folder which names
 # matches the pattern.
 create_alldata_aux <- function(path, pattern = NULL, neededVars){
  before <- dir(path) # List of files in the folder before I started
  make.na(path)
  new <- setdiff(dir(path), before) # Files I added, will need to clean later
  out <- lapply(files <- dir(path, pattern = pattern, full.names = TRUE),
                function(z){
                 temp <- read.csv.epi(z, allow.na = allow.na)
                 names(temp) <- gsub("^L14.|^L16.", "", names(temp)) # Legacy variables
                 temp
                })
  # Get the abbreviation (capital letters with numbers):
  names(out) <- gsub("^.*[^A-Z0-9]([A-Z0-9]+)_.*$", "\\1", files) 
  out <- out[names(out) %in% neededVars]
  file.remove(paste0(path, "/", new))
  return(out)
 }
 
 ranks <- read.csv(ranks, as.is = TRUE)
 scores <- read.csv(scores, as.is = TRUE)
 info <- read.csv(information, as.is = TRUE)
 info <- info[, c("Type", "Abbreviation", "Description", "RawPolarity", "Normalization",
                  "Transformation", "Target_Good", "Target_Bad")]
 neededVars <- info$Abbreviation[info$Type %in% c("Indicator", "Blend")]
 neededVars <- neededVars[neededVars != "CXN"]
 
 allRaw2018 <- create_alldata_aux(y2018raw, pattern = ".*_na.csv$", neededVars)
 allInd2018 <- create_alldata_aux(y2018ind, pattern = ".*_na.csv$", neededVars)
 
 rawNames2018 <- names(allRaw2018)
 indNames2018 <- names(allInd2018)
 
 var_name = c(rawNames2018, indNames2018)
 type <- c(rep("raw", length(rawNames2018)), rep("ind", length(indNames2018)))
 
 aux <- data.frame(name = var_name,
                   year = 2018,
                   type = type, stringsAsFactors = FALSE)
 aux <- aux[!duplicated(aux),]
 alldata <- list("y2018raw" = allRaw2018,
                 "y2018names_raw" = rawNames2018,
                 "y2018ind" = allInd2018,
                 "y2018names_ind" = indNames2018,
                 "info" = info,
                 "aux" = aux,
                 "scores" = scores,
                 "ranks" = ranks)
 save(alldata, file = "alldata_red.RData", compress='xz')
}


# Get the most recent available data from the data frame:
get_MRD <- function(a, b) {
 out <- data.frame(country = countries,
                   var1 = apply(a[, -c(1:3), drop = FALSE], 1,
                                function(x) {rev(x[!is.na(x)])[1]}),
                   var2 = apply(b[, -c(1:3), drop = FALSE], 1,
                                function(x) {rev(x[!is.na(x)])[1]}))
 return(out)
}


plot_ts_countries_ggplot <- function(input_country, input_vars,
                                     input_luckyNum, input_lucky = FALSE, 
                                     input_add = FALSE, saveplot = FALSE) {
 lucky_countries <- c()
 if(input_lucky) lucky_countries <- sample(setdiff(countries, input_country), input_luckyNum)
 
 inds <- alldata$y2018names_ind
 if (!"all" %in% input_vars) {inds <- input_vars}
 
 out_data <- lapply(inds,
                    function(x){
                     add_countries <- c()
                     curr_var <- alldata$y2018ind[[x]]
                     temp <- curr_var[, ncol(curr_var)]
                     if (all(is.na(temp))) temp <- curr_var[, ncol(curr_var) - 1]
                     if (!all(is.na(temp)) & input_add){
                      these_max <- which(temp == max(temp, na.rm = TRUE))
                      these_min <- which(temp == min(temp, na.rm = TRUE))
                      add_countries <- c(add_countries, 
                                         countries[c(these_max[1], these_min[1])])
                     }
                     curr_var[curr_var$country %in%
                               c(input_country, add_countries, lucky_countries), ]
                    })
 names(out_data) <- inds
 
 
 plots <- lapply(names(out_data), function(ind) {
  
  years <- gsub(".*([0-9]{4}).*", "\\1", names(out_data[[ind]])[-c(1:3)])
  pl <- melt(out_data[[ind]][, -c(1:2)])
  pl$year <- as.numeric(gsub(".*([0-9]{4}).*", "\\1", pl$variable))
  labels <- years
  labels[seq(2, length(labels) - 1, by = 5)] <- "" 
  
  ggplot(pl) +
   geom_line(aes(x = year, y = value, col = as.factor(country))) +
   scale_x_continuous(breaks = as.numeric(years)[seq(1, length(years), by = 4)]) +
   xlab("") + ylab("") +
   ggtitle(ind) +
   labs(color = 'countries') +
   ylim(c(0, 100)) +
   theme_bw()
 })
 
 if (saveplot) return(marrangeGrob(grobs = plots, ncol = 2, nrow = 2))
 else return (grid.arrange(grobs = plots, ncol = 2,
                           heights = rep(unit(3, "in"), ((length(plots) + 1) %/% 2))))
}

get_Data <- function(x, lab = names(x)) {
 paste(paste0(lab[1], ": ", x[, 1]),
       paste0(lab[2], ": ", x[, 2]),
       paste0(lab[3], ": ", x[, 3]),
       sep = "<br />")
}

plot_compare_ggvis <- function(x) {
 x_title <- names(x)[2]
 y_title <- names(x)[3]
 names(x) <- c("country", "var1", "var2")
 return (x %>%
          ggvis(~var1, ~var2, key := ~country) %>%
          add_tooltip(get_Data) %>%
          set_options(width = "auto", height  = "auto") %>% 
          add_axis("x", title = x_title, title_offset = 40) %>%
          add_axis("y", title = y_title, title_offset = 50))
}

get_DataForComparison <- function(a, b, year1, year2) {
 
 root_a <- gsub(".[0-9]{4}", "", names(a)[4])
 root_b <- gsub(".[0-9]{4}", "", names(b)[4])
 years_a <- gsub(".*([0-9]{4})$", "\\1", names(a)[-c(1:3)])
 years_b <- gsub(".*([0-9]{4})$", "\\1", names(b)[-c(1:3)])
 
 # Create data frame:
 out <- get_MRD(a, b)
 if (year2 == "most recent" & year1 %in% years_a) {
  out$var1 <- a[, paste(root_a, year1, sep = ".")]
 }
 else if (year1 == "most recent" & year2 %in% years_b) {
  out$var2 <- b[, paste(root_b, year2, sep = ".")]
 }
 else if (year1 %in% years_a & year2 %in% years_b) {
  out$var1 <- a[, paste(root_a, year1, sep = ".")]
  out$var2 <- b[, paste(root_b, year2, sep = ".")]
 }
 if (all(is.na(out$var1))) {out$var1 <- -8888}
 if (all(is.na(out$var2))) {out$var2 <- -8888}
 names(out) <- c('country', paste(root_a, year1, sep = "_"), paste(root_b, year2, sep = "_"))
 
 return(out)
}

server <- function(input, output, session) {
 
 #################################
 ########## First tab ###########
 
 observe({
  input$var
  updateSelectInput(session, "type", label = "Select variable type",
                    choices = full2abbr[full2abbr == aux$type[aux$name == input$var]])
 })
 
 var <- reactive({
  input$var
  input$type
  alldata[[paste0("y2018", input$type)]][[input$var]]
 })
 
 output$hist <- renderPlot({
  if(!is.null(var())){
   z <- var()
   temp <- data.frame(country = countries,
                      var1 = apply(z[, -c(1:3)], 1, function(x) {rev(x[!is.na(x)])[1]}))
  }
  names(temp) <- c("country", paste0("Last available data for ", isolate(input$var)))
  
  ggplot(temp, aes(temp[, ncol(temp)])) +
   geom_histogram(color="black", fill="lightblue") +
   xlab(paste0("Variable: ", isolate(input$var), "; ",
               "EPI year: 2018; ",
               "Type: ", isolate(input$type))) +
   ylab("") +
   theme_bw()
 })
 
 output$table <- renderDataTable({
  var()
  if(!(is.null(var()))){
   out <- data.frame(country = countries,
                     var1 = apply(var()[, -c(1:3)], 1,
                                  function(x) {rev(x[!is.na(x)])[1]}))
   out$var1 <- round(as.numeric(out$var1), 2)
   names(out) <- c("country", paste0("Last available data for ", isolate(input$var)))
   out
  }
 })
 
 #################################
 ########## Second tab ###########
 
 country <- reactive({
  input$country
  ranks <- alldata$ranks[alldata$ranks$country == isolate(input$country), ]
  scores <- alldata$scores[alldata$scores$country == isolate(input$country), ]
  inds <- unique(gsub("^([A-Z0-9]*)[^A-Z0-9].*", "\\1", names(ranks)))
  inds <- inds[inds != ""]
  
  df <- data.frame(ind = inds, score_new = NA, rank_new = NA, score_old = NA, rank_old = NA)
  
  if (nrow(scores) != 0) {    
   df$score_new <- round(as.numeric(unlist(scores[, paste0(inds, ".new")])), 2)
   df$score_old <- round(as.numeric(unlist(scores[, paste0(inds, ".old")])), 2)
  }
  if (nrow(ranks) != 0) {
   df$rank_new <- round(as.numeric(unlist(ranks[, paste0(inds, ".rnk.new")])), 2)
   df$rank_old <- round(as.numeric(unlist(ranks[, paste0(inds, ".rnk.old")])), 2)
  }

  names(df) <- c("Var", "2018 score current", "2018 rank current", "2018 score baseline", "2018 rank baseline")
  df
 })
 
 output$table2 <- renderDataTable({
  country()
 })
 
 #################################
 ########## Third tab ############
 
 observeEvent(input$do, {
  validate(need(length(input$vars) > 0, "No data!"))
  output$plot3 <- renderPlot({
   plot_ts_countries_ggplot(isolate(input$countries), isolate(input$vars),
                            isolate(input$luckyNum), isolate(input$lucky),
                            isolate(input$add))
  })
 })
 
 observeEvent(input$do, {
  output$plot3_fin <- renderUI({
   height <- ifelse(!"all" %in% isolate(input$vars),
                    220 * ((length(isolate(input$vars)) + 1) %/% 2), 
                    220 * (nrow(aux[aux$year == 2018 & aux$type == "ind",])) %/% 2)
   plotOutput("plot3", height = height)
  })
 })
 
 output$downloadPlot <- downloadHandler(
  filename = "Plot.pdf",
  content = function(file) {
   ggsave(file, plot_ts_countries_ggplot(input$countries, input$vars,
                                         input$luckyNum, input$lucky, 
                                         input$add, saveplot = TRUE),
          width = 11, height = 7)
  })
 
}


ui <- 
 dashboardPage(title = "EPI visualization tools",
               dashboardHeader(title = 
                                tags$div(tags$a(href='http://epi.yale.edu/',
                                                tags$img(src='https://epi.envirocenter.yale.edu/sites/default/files/epi_lockup_52.png',
                                                         height='20', width='20')), "EPI vis")
               ),
               dashboardSidebar(
                sidebarMenu(
                 menuItem("Explore an indicator or variable", tabName = "onevar"),
                 menuItem("Explore a country", tabName = "onecountry"),
                 menuItem("Compare countries", tabName = "manycountries"))),
               dashboardBody(tags$head(tags$style(HTML('.skin-blue .main-header .logo {background-color: #1b1b38;} .skin-blue .main-header .logo:hover {background-color: #1b1b38;} .skin-blue .main-header .navbar {background-color: #06357a;} .skin-blue .main-sidebar {background-color: #1b1b38;} .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #1b1b38;} .skin-blue .main-sidebar .sidebar .sidebar-menu a{background-color: #1b1b38;color: #ffffff;} .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{background-color: #06357a;} .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: #06357a;}'))),
                             tabItems(
                              tabItem(tabName = "onevar",
                                      fluidRow(column(width = 12,
                                                      selectInput('var', 
                                                                  'Select an indicator or variable:',
                                                                  abbr2desc, width = "100%"),
                                                      selectInput('type',
                                                                  'Select variable type:',
                                                                  abbr2full))),
                                      fluidRow(
                                       tabBox(width = 12, id = "tabset2",
                                              tabPanel("Histogram", plotOutput('hist')),
                                              tabPanel("Table", dataTableOutput('table'))
                                       )
                                      )
                              ),
                              tabItem(tabName = "onecountry",
                                      fluidRow(column(width = 10,
                                                      selectInput('country', 'Select a country:',
                                                                  countries, selected = countries[1]))),
                                      fluidRow(column(width = 12,
                                                      dataTableOutput("table2")))),
                              tabItem(tabName = "manycountries",
                                      fluidRow(column(width = 12,
                                                      selectInput('countries', 'Select countries:',
                                                                  countries, selected = countries[1],
                                                                  multiple = TRUE, width = "100%"))),
                                      fluidRow(column(width = 12,
                                                      selectInput('vars', 'Select indicators:',
                                                                  c("all", 
                                                                    abbr2desc[abbr2desc == 
                                                                               aux$name[aux$type == "ind"]]), 
                                                                  selected = "all", multiple = TRUE,
                                                                  width = "100%"))),
                                      fluidRow(column(width = 4,
                                                      checkboxInput("lucky",
                                                                    label = 
                                                                     p("I'm feeling lucky",
                                                                       bsButton("q2", label = "", 
                                                                                icon = icon("question"),
                                                                                style = "info", 
                                                                                size = "extra-small")),
                                                                    value = FALSE),
                                                      bsPopover(id = "q2", title = "", 
                                                                content = "Add the selected number of random countries to the plots"),
                                                      conditionalPanel("input.lucky == true",
                                                                       sliderInput("luckyNum", 
                                                                                   label = "",
                                                                                   min = 1,
                                                                                   max = 10, 
                                                                                   value = 5))),
                                               column(width = 4,
                                                      checkboxInput("add",
                                                                    label = 
                                                                     p("Add max/min",
                                                                       bsButton("q4", label = "", 
                                                                                icon = icon("question"), 
                                                                                style = "info", 
                                                                                size = "extra-small")),
                                                                    value = FALSE),
                                                      bsPopover(id = "q4", title = "", 
                                                                content = "Add the countries with the largest/smallest value in the last year.")),
                                               column(width = 4,
                                                      downloadButton('downloadPlot', 
                                                                     'Download plots'))),
                                      
                                      fluidRow(column(width = 12, 
                                                      actionButton("do", "GO!", width = '100%',
                                                                   style="color: #ffffff; background-color: #94a0b8; border-color: #1b1b38"))),
                                      conditionalPanel("input.do != 0", 
                                                       fluidRow(box(width = 12, uiOutput('plot3_fin')))))
                              
                             ))
 )


shinyApp(ui = ui, server = server)
