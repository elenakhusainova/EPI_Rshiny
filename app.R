rm(list=ls())
library(shiny)
library(shinydashboard)
library(shinyBS)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggvis)
# devtools::install_github("elenakhusainova/epi_rpackage", force = TRUE)
library(EPI)
load("alldata.RData")
countries <- alldata$y2018raw[[1]]$country
aux <- alldata$aux
full2abbr <- c("raw" = "raw", "normalized" = "nrm", "transformed" = "trf", 
               "resized" = "rsz", "imputed" = "imp", "indicator" = "ind")
abbr2full <- c("raw" = "raw", "nrm" = "normalized", "trf" = "transformed", 
               "rsz" = "resized", "imp" = "imputed", "ind" = "indicator")


create_alldata <- function(y2014raw = "../../../Work/LRaw",
                           y2016raw = "../../../Work/LRaw",
                           y2018raw = "../../../Work/Raw",
                           y2018nrm = "../../../Work/P1_Normalization",
                           y2018trf = "../../../Work/P2_Transformation",
                           y2018rsz = "../../../Work/P3_Resizing",
                           y2018imp = "../../../Work/P4_Imputation",
                           y2018ind = "../../../Work/P5_Indicator",
                           information = "../../../Work/Inputs/master_variable_list.csv",
                           allow.na = TRUE) {
 
 # Auxiliary function to create a list of data frames for the given type 
 # and year. Essentially it reads all the .csv files from the folder which names
 # matches the pattern.
 create_alldata_aux <- function(path, pattern = NULL){
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
  file.remove(paste0(path, "/", new))
  return(out)
 }
 
 allRaw2014 <- create_alldata_aux(y2014raw, pattern = "^L14.*_na.csv$")
 allRaw2016 <- create_alldata_aux(y2016raw, pattern = "^L16.*_na.csv$")
 allRaw2018 <- create_alldata_aux(y2018raw, pattern = ".*_na.csv$")
 
 allNrm2018 <- create_alldata_aux(y2018nrm, pattern = ".*_na.csv$")
 allTrf2018 <- create_alldata_aux(y2018trf, pattern = ".*_na.csv$")
 allRsz2018 <- create_alldata_aux(y2018rsz, pattern = ".*_na.csv$")
 allImp2018 <- create_alldata_aux(y2018imp, pattern = ".*_na.csv$")
 allInd2018 <- create_alldata_aux(y2018ind, pattern = ".*_na.csv$")
 
 rawNames2018 <- names(allRaw2018)
 rawNames2016 <- names(allRaw2016)
 rawNames2014 <- names(allRaw2014)
 
 nrmNames2018 <- names(allNrm2018)
 trfNames2018 <- names(allTrf2018)
 rszNames2018 <- names(allRsz2018)
 impNames2018 <- names(allImp2018)
 indNames2018 <- names(allInd2018)
 
 var_name = c(rawNames2018, rawNames2016, rawNames2014,
              nrmNames2018, trfNames2018, rszNames2018,
              impNames2018, indNames2018)
 year <- c(rep("2018", length(rawNames2018)), rep("2016", length(rawNames2016)),
           rep("2014", length(rawNames2014)), rep("2018", length(nrmNames2018)),
           rep("2018", length(trfNames2018)), rep("2018", length(rszNames2018)),
           rep("2018", length(impNames2018)), rep("2018", length(indNames2018)))
 type <- c(rep("raw", length(rawNames2018) + length(rawNames2016) + length(rawNames2014)), 
           rep("nrm", length(nrmNames2018)), rep("trf", length(trfNames2018)),
           rep("rsz", length(rszNames2018)), rep("imp", length(impNames2018)),
           rep("ind", length(indNames2018)))
 
 info <- read.csv(information, as.is = TRUE)
 info <- info[, c("Abbreviation", "Description", "RawPolarity", "Normalization",
                  "Transformation", "Target_Good", "Target_Bad")]
 aux <- data.frame(name = var_name,
                   year = year,
                   type = type, stringsAsFactors = FALSE)
 aux <- aux[!duplicated(aux),]
 # aux <- aux[aux$name %in% info$Abbreviation,] 
 alldata <- list("y2018raw" = allRaw2018,
                 "y2016raw" = allRaw2016,
                 "y2014raw" = allRaw2014,
                 "y2018names_raw" = rawNames2018,
                 "y2016names_raw" = rawNames2016,
                 "y2014names_raw" = rawNames2014,
                 "y2018nrm" = allNrm2018,
                 "y2018names_nrm" = nrmNames2018,
                 "y2018trf" = allTrf2018,
                 "y2018names_trf" = trfNames2018,
                 "y2018rsz" = allRsz2018,
                 "y2018names_rsz" = rszNames2018,
                 "y2018imp" = allImp2018,
                 "y2018names_imp" = impNames2018,
                 "y2018ind" = allInd2018,
                 "y2018names_ind" = indNames2018,
                 "info" = info,
                 "aux" = aux)
 save(alldata, file = "alldata.RData", compress='xz')
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


plot_ts_countries_ggplot <- function(input_country, input_vars, input_type_3,
                                     input_luckyNum, input_lucky = FALSE, 
                                     input_add = FALSE, input_fullRange = FALSE,
                                     saveplot = FALSE) {
 lucky_countries <- c()
 if(input_lucky) lucky_countries <- sample(setdiff(countries, input_country), input_luckyNum)
 
 inds <- alldata[[paste0("y2018names_", input_type_3)]]
 if (!"all" %in% input_vars) {inds <- input_vars}
 
 out_data <- lapply(inds,
                    function(x){
                     add_countries <- c()
                     curr_var <- alldata[[paste0("y2018", input_type_3)]][[x]]
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
  
  ggplot(pl) +
   geom_line(aes(x = year, y = value, col = as.factor(country))) +
   ylim(range(pl$value, na.rm = TRUE)) +
   scale_x_continuous(breaks = as.numeric(years), labels = years) +
   xlab("") + ylab("") +
   ggtitle(ind) +
   labs(color = 'countries') +
   theme_bw()
 })
 
 if (input_fullRange){
  ranges <- lapply(inds, function(x){range(alldata[[paste0("y2018", input_type_3)]][[x]][,-c(1:3)], 
                                           na.rm = TRUE)})
  plots <- lapply(1:length(plots), function(i) {plots[[i]] + ylim(ranges[[i]])})
 }
 
 if (saveplot) return(marrangeGrob(grobs = plots, ncol = 2, nrow = 2))
 else return (grid.arrange(grobs = plots, ncol = 2,
                           heights = rep(unit(3, "in"), ((length(plots) + 1) %/% 2))))
}


maxres <- function(x) {
 temp <- t(x[, -c(1:3)])
 lms <- apply(temp, 2, function(z) {
  if (all(is.na(z))) {return (NA)}
  lm0 <- lm(z ~ c(1:nrow(temp)))
  # plot(z~c(1:nrow(temp)))
  max(abs(lm0$residuals))/summary(lm0)$sigma
 })
 return(lms)
}


plot_ts_ggvis <- function(x, threshold, input_var, input_type, input_datayear, 
                          input_transform) {
 # Calculate residuals:
 lms <- maxres(x)
 
 # Create data frame for the plot:
 these <- order(lms)[threshold[1]:threshold[2]]
 out_data <- melt(x[these, -c(1:2)]) 
 out_data$year <- as.numeric(gsub(".*([0-9]{4})$", "\\1", out_data$variable))
 out_data <- out_data[!is.na(out_data$value),]
 out_data$label <- rep("", nrow(out_data))
 last_year <- (out_data$year == max(out_data$year))
 out_data$label[last_year] <- sapply(out_data$country[last_year], 
                                     function(z){x$iso[x$country == z]})
 
 # The chunk below is to make labels better:
 rn <- (range(out_data$value)[2] - range(out_data$value)[1]) / 50
 out_data$crowded <- sapply(out_data$value, 
                            function(x) {sum(abs(out_data$value[last_year] - x) < rn)})
 out_data$crowded[!last_year] <- 0
 out_data$dx <- 5
 out_data$dx[out_data$crowded == 2] <- 
  rep(c(5, 30), length.out = length(out_data$dx[out_data$crowded == 2]))
 out_data$dx[out_data$crowded == 3] <- 
  rep(c(5, 30, 55), length.out = length(out_data$dx[out_data$crowded == 3]))
 out_data$dx[out_data$crowded > 3] <- 
  rep(c(5, 30, 55, 80, 105, 130), length.out = length(out_data$dx[out_data$crowded > 3]))
 out_data$dy <- 0
 
 # Plot:
 return (out_data %>%
          ggvis(~year, ~value, stroke = ~country) %>%
          add_axis("x", title="year",  format="####") %>%
          add_axis("y", title=paste0("Variable: ", input_var, "; ",
                                     "Type: ", input_type, "; ",
                                     "EPI version: ", input_datayear, "; ",
                                     "Transformation: ", input_transform),
                   title_offset = 50) %>%
          layer_lines() %>%
          layer_points(size = 1, fill = ~country) %>% 
          layer_text(text := ~label, fontSize := 10, 
                     dx := ~dx,
                     dy := ~dy) %>%
          set_options(width = "auto") %>%
          add_tooltip(get_Data) %>%
          hide_legend("fill") %>%
          hide_legend("stroke"))
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
 
 output$text <- renderText({
  varname1 <- input$var1
  if (input$datayear1 == 'Jay (2014)') {varname1 <- paste0("L14.", varname1)}
  if (input$datayear1 == 'Angel (2016)') {varname1 <- paste0("L16.", varname1)}
  thisrow1 <- alldata$info[alldata$info$Abbreviation == varname1, ]
  out <- paste0("<br/><br/><br/><br/><br/><br/><font color=\"BCC3D8\"><b><p style=\"margin-left: 1em;\">",
                input$var1, " from ", input$datayear1, " EPI version:<br/>",
                thisrow1[[2]], "<br/>", 
                "Polarity: ", thisrow1[[3]], "<br/>", 
                "Normalization: ", thisrow1[[4]], "<br/>",
                "Recommended transformation: ", thisrow1[[5]], "<br/>",
                "Bad target: ", thisrow1[[7]], "<br/>",
                "Good target: ", thisrow1[[6]], "<br/>",
                "</p></b></font><br/>")
  
  varname2 <- input$var2
  if(varname2 != varname1){
   if (input$datayear2 == 'Jay (2014)') {varname2 <- paste0("L14.", varname2)}
   if (input$datayear2 == 'Angel (2016)') {varname2 <- paste0("L16.", varname2)}
   thisrow2 <- alldata$info[alldata$info$Abbreviation == varname2, ]
   out <- paste0(out,
                 "<font color=\"BCC3D8\"><b><p style=\"margin-left: 1em;\">",
                 input$var2, " from ", input$datayear2, " EPI version:<br/>",
                 thisrow2[[2]], "<br/>",
                 "Polarity: ", thisrow2[[3]], "<br/>", 
                 "Normalization: ", thisrow2[[4]], "<br/>",
                 "Recommended transformation: ", thisrow2[[5]], "<br/>",
                 "Bad target: ", thisrow2[[7]], "<br/>",
                 "Good target: ", thisrow2[[6]], "<br/>",
                 "</p></b></font><br/>")
  }
  
  varname <- input$var
  if (!varname %in% c(varname1, varname2)){
   if (input$datayear == 'Jay (2014)') {varname <- paste0("L14.", varname)}
   if (input$datayear == 'Angel (2016)') {varname <- paste0("L16.", varname)}
   thisrow <- alldata$info[alldata$info$Abbreviation == varname, ]
   out <- paste0(out,
                 "<font color=\"BCC3D8\"><b><p style=\"margin-left: 1em;\">",
                 input$var, " from ", input$datayear, " EPI version:<br/>",
                 thisrow[[2]], "<br/>", 
                 "Polarity: ", thisrow[[3]], "<br/>",
                 "Normalization: ", thisrow[[4]], "<br/>",
                 "Recommended transformation: ", thisrow[[5]], "<br/>",
                 "Bad target: ", thisrow[[7]], "<br/>",
                 "Good target: ", thisrow[[6]], "<br/>",
                 "</p></b></font><br/>")
  }
  out
 })
 
 ################################
 ########## First tab ###########
 
 # X-axis:
 observe({ 
  input$datayear1
  updateSelectInput(session, "var1", label = "X-axis: select variable",
                    choices = sort(unique(aux[aux$year == input$datayear1,]$name)))
 })
 
 observe({ 
  input$var1
  input$datayear1
  updateSelectInput(session, "type1", label = "X-axis: select variable type",
                    choices = full2abbr[full2abbr == aux$type[aux$year == input$datayear1 & 
                                                               aux$name == input$var1]])
 })
 
 observe({
  input$datayear1
  input$var1
  input$type1
  years1 <- as.numeric(gsub(".*([0-9]{4})$", "\\1", 
                            names(alldata[[paste0("y", input$datayear1, input$type1)]]
                                  [[input$var1]])[-c(1:3)]))
  updateSelectInput(session, "year1",
                    label = "X-axis: select year:",
                    choices = c("most recent", rev(years1)))
 })
 
 # Y-axis:
 observe({ 
  input$datayear2
  updateSelectInput(session, "var2", label = "Y-axis: select variable",
                    choices = sort(unique(aux[aux$year == input$datayear2,]$name)))
 })
 
 observe({ 
  input$var2
  input$datayear2
  updateSelectInput(session, "type2", label = "Y-axis: select variable type",
                    choices = full2abbr[full2abbr == aux$type[aux$year == input$datayear2 & 
                                                               aux$name == input$var2]])
 })
 
 observe({ 
  input$datayear2
  input$var2
  input$type2 
  years2 <- as.numeric(gsub(".*([0-9]{4})$", "\\1", 
                            names(alldata[[paste0("y", input$datayear2, input$type2)]]
                                  [[input$var2]])[-c(1:3)]))
  updateSelectInput(session, "year2",
                    label = "Y-axis: select year:",
                    choices = c("most recent", rev(years2)))
 })
 
 
 # Data frame for outputs:
 z <- reactive({
  c(input$datayear1, input$datayear2, input$type1, input$type2, input$var1, 
    input$var2, input$year1, input$year2) # Just all
  X <- alldata[[paste0("y", input$datayear1, input$type1)]][[input$var1]]
  Y <- alldata[[paste0("y", input$datayear2, input$type2)]][[input$var2]]
  if (!any(c(is.null(X), is.null(Y)))) get_DataForComparison(X, Y, input$year1, input$year2)
  else NULL
 })
 
 
 # Outputs:
 observe({ 
  z()
  if(!is.null(z())){
   temp <- z()[complete.cases(z()), ]
   lb <- linked_brush(keys = temp$country, "red")
   selected <- reactive({temp[lb$selected(), ]})
   plot_compare_ggvis(temp) %>%
    layer_points(fill.brush := "red") %>% lb$input() %>%
    bind_shiny("plot_compare")
  }
  
  #plot_compare_ggvis(selected, isolate(input$year1), isolate(input$year2),
  #                   isolate(input$var1), isolate(input$var2),
  #                   isolate(input$datayear1), isolate(input$datayear2)) %>%
  #  layer_points() %>%
  #  bind_shiny("plot_compare_zoom")
 }, priority = -1)
 
 output$table12 <- renderDataTable({
  z()
 })
 
 #################################
 ########## Second tab ###########
 observe({
  input$datayear
  updateSelectInput(session, "var", label = "Select variable",
                    choices = sort(unique(aux$name[aux$year == input$datayear])))
 })
 
 observe({
  input$datayear
  input$var
  updateSelectInput(session, "type", label = "Select variable type",
                    choices = full2abbr[full2abbr == aux$type[aux$year == input$datayear & 
                                                               aux$name == input$var]])
 })
 
 var <- reactive({
  input$datayear
  input$var
  input$type
  alldata[[paste0("y", input$datayear, input$type)]][[input$var]]
 })
 
 output$hist <- renderPlot({
  
  if(!is.null(var())){
   if(input$transform == "None") z <- var()
   else tryCatch({z <- logit(var())$x}, error = function(e) e, finally = {z <- var()})
   temp <- data.frame(country = countries,
                      var1 = apply(z[, -c(1:3)], 1, function(x) {rev(x[!is.na(x)])[1]}))
  }
  names(temp) <- c("country", isolate(input$var))
  
  ggplot(temp, aes(temp[, ncol(temp)])) +
   geom_histogram(color="black", fill="lightblue") +
   xlab(paste0("Indicator: ", isolate(input$var), "; ",
               "EPI version: ", isolate(input$datayear), "; ",
               "Transformation: ", isolate(input$transform), "; ",
               "Type: ", isolate(input$type))) +
   ylab("") +
   theme_bw()
 })
 
 observe({
  if(!is.null(var())){
   if(input$transform == "None") z <- var()
   else tryCatch({z <- logit(var())$x}, error = function(e) e, finally = {z <- var()})
   plot_ts_ggvis(z, input$threshold, isolate(input$var),
                 isolate(input$type), isolate(input$datayear),
                 isolate(input$transform)) %>% bind_shiny("ts_ggvis")
  }
 })
 
 output$table <- renderDataTable({
  var()
  if(!(is.null(var()))){
   out <- data.frame(country = countries,
                     var1 = apply(var()[, -c(1:3)], 1,
                                  function(x) {rev(x[!is.na(x)])[1]}),
                     var2 = apply(logit(var())$x[, -c(1:3)], 1,
                                  function(x) {rev(x[!is.na(x)])[1]}))
   names(out) <- c("country", isolate(input$var), 
                   paste(isolate(input$var), "log"))
   out
  }
 })
 
 
 
 #################################
 ########## Third tab ############
 
 observe({ 
  input$type_3
  updateSelectInput(session, "vars", 
                    label = "Select variables:",
                    choices = c("all", sort(aux$name[aux$year == 2018 & aux$type == input$type_3])),
                    selected = "all")
 })
 
 observeEvent(input$do, {
  validate(need(length(input$vars) > 0, "No data!"))
  output$plot3 <- renderPlot({
   plot_ts_countries_ggplot(isolate(input$country), isolate(input$vars),
                            isolate(input$type_3),
                            isolate(input$luckyNum), isolate(input$lucky),
                            isolate(input$add), isolate(input$fullRange))
  })
 })
 
 observeEvent(input$do, {
  output$plot3_fin <- renderUI({
   height <- ifelse(!"all" %in% isolate(input$vars),
                    220 * ((length(isolate(input$vars)) + 1) %/% 2), 
                    220 * (nrow(aux[aux$year == 2018 & aux$type == isolate(input$type_3),])) %/% 2)
   plotOutput("plot3", height = height)
  })
 })
 
 output$downloadPlot <- downloadHandler(
  filename = "Plot.pdf",
  content = function(file) {
   ggsave(file, plot_ts_countries_ggplot(input$country, input$vars,
                                         input$type_3, input$luckyNum,
                                         input$lucky, input$add,
                                         input$fullRange, saveplot = TRUE),
          width = 11, height = 7)
  })
 
}


ui <- 
 dashboardPage(title = "EPI visualization tools",
               dashboardHeader(title = 
                                tags$div(tags$a(href='http://epi.yale.edu/',
                                                tags$img(src='http://epi.yale.edu/sites/all/themes/epizen/favicon.ico',
                                                         height='20', width='20')), "EPI vis")
               ),
               dashboardSidebar(
                sidebarMenu(
                 menuItem("Compare two variables", tabName = "two_vars"),
                 menuItem("Look at one variable separately", tabName = "one_var"),
                 menuItem(text = tags$p("Compare countries with current", tags$br(), "EPI data"), 
                          tabName = "country")),
                htmlOutput("text")),
               dashboardBody(tags$head(tags$style(HTML('.skin-blue .main-header .logo {background-color: #1b1b38;} .skin-blue .main-header .logo:hover {background-color: #1b1b38;} .skin-blue .main-header .navbar {background-color: #06357a;} .skin-blue .main-sidebar {background-color: #1b1b38;} .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #1b1b38;} .skin-blue .main-sidebar .sidebar .sidebar-menu a{background-color: #1b1b38;color: #ffffff;} .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{background-color: #06357a;} .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: #06357a;}'))),
                             tabItems(
                              tabItem(tabName = "two_vars", 
                                      fluidRow(
                                       column(width = 6,
                                              title = "Controls",
                                              selectInput('datayear1',
                                                          'X-axis: select EPI version:',
                                                          c("Jay (2014)" = 2014, 
                                                            "Angel (2016)" = 2016, 
                                                            "Current (2018)" = 2018),
                                                          selected = 2018),
                                              selectInput('var1', 'X-axis: select indicator:',
                                                          alldata$y2018names_raw,
                                                          selected=alldata$y2018names_raw[1]),
                                              selectInput('type1',
                                                          'X-axis: select variable type:',
                                                          abbr2full, selected = 'raw'),
                                              selectInput('year1',
                                                          'X-axis: select year:', c(""))),
                                       column(width = 6,
                                              selectInput('datayear2',
                                                          'Y-axis: select EPI version:',
                                                          c("Jay (2014)" = 2014, 
                                                            "Angel (2016)" = 2016, 
                                                            "Current (2018)" = 2018),
                                                          selected = 2018),
                                              selectInput('var2', 'Y-axis: select indicator:',
                                                          alldata$y2018names_raw,
                                                          selected=alldata$y2018names_raw[1]),
                                              selectInput('type2',
                                                          'Y-axis: select variable type:',
                                                          abbr2full, selected = 'raw'),
                                              selectInput('year2',
                                                          'Y-axis: select year:', c("")))),
                                      fluidRow(
                                       tabBox(width = 12, id = "tabset1",
                                              tabPanel("Scatterplot", ggvisOutput('plot_compare'), 
                                                       ggvisOutput('plot_compare_zoom')),
                                              tabPanel("Table", dataTableOutput('table12'))
                                       )
                                      )
                                      
                              ),
                              tabItem(tabName = "one_var",
                                      fluidRow(column(width = 6,
                                                      selectInput('datayear', 'Select EPI version:',
                                                                  c("Jay (2014)" = 2014, 
                                                                    "Angel (2016)" = 2016, 
                                                                    "Current (2018)" = 2018),
                                                                  selected = 2018),
                                                      selectInput('var', 'Select indicator:',
                                                                  aux$name[aux$year == 2018 & 
                                                                            aux$type == "raw"]),
                                                      selectInput('type',
                                                                  'Select variable type:',
                                                                  abbr2full, selected = 'raw')),
                                               column(width = 6,
                                                      selectInput('transform', 'Select transformation:',
                                                                  c("None", "log"), selected = "None"),
                                                      sliderInput("threshold", 
                                                                  label = 
                                                                   p("Threshold",
                                                                     bsButton("q1", 
                                                                              label = "", 
                                                                              icon = icon("question"),
                                                                              style = "info", 
                                                                              size = "extra-small")), 
                                                                  min = 0, 
                                                                  max = length(countries), 
                                                                  value = c(0, length(countries))),
                                                      bsPopover(id = "q1", title = "",
                                                                content = "For each country the linear model was fitted and the ratio of largest residuals to residual standart error calculated. Then countries were sorted according to this value. Choose the range of countries to display.")
                                               )),
                                      fluidRow(
                                       tabBox(width = 12, id = "tabset2",
                                              tabPanel("Time series", ggvisOutput('ts_ggvis')),
                                              tabPanel("Histogram", plotOutput('hist')),
                                              tabPanel("Table", dataTableOutput('table'))
                                       )
                                      )
                              ),
                              tabItem(tabName = "country",
                                      fluidRow(column(width = 4,
                                                      selectInput('country', 'Select countries:',
                                                                  countries, selected = countries[1],
                                                                  multiple = TRUE),
                                                      selectInput('type_3', 'Select variable type:',
                                                                  full2abbr, selected = "raw"),
                                                      selectInput('vars', 'Select variables:',
                                                                  c("all", alldata$y2018names_raw), 
                                                                  selected = "all",
                                                                  multiple = TRUE)),
                                               column(width = 4,
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
                                                                                   value = 5)),
                                                      checkboxInput("fullRange",
                                                                    label = 
                                                                     p("Use full range",
                                                                       bsButton("q3", label = "", 
                                                                                icon = icon("question"),
                                                                                style = "info", 
                                                                                size = "extra-small")),
                                                                    value = FALSE),
                                                      bsPopover(id = "q3", title = "", 
                                                                content = "Use the full range for y-axis"),
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
                                                       fluidRow(box(width = 12, uiOutput('plot3_fin'))))
                              )))
 )

shinyApp(ui = ui, server = server)
