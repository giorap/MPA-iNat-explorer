######################################################
##### -- MPA iNaturalist Explorer R Shiny App -- #####
######################################################
########################
##### -- Server -- #####
########################

##### -- Set things up -- #####
##### Load packages
library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)
library(ggplot2)
library(rinat)
library(shinyLP)
library(highcharter)
library(plotly)
library(RColorBrewer)
library(collapsibleTree)

##### Load data
MPA_boundaries <- readRDS("mpa_boundaries_edited.rds")
MPA_iNat_observations <- readRDS("MPA_iNat_observations_withTaxonomy.rds")
names(MPA_iNat_observations) <- as.character(MPA_boundaries$OBJECTID)[match(names(MPA_iNat_observations), MPA_boundaries@data$iNat_project_name)]
MPA_iNat_observations <- c(MPA_iNat_observations, vector("list", length(setdiff(as.character(MPA_boundaries$OBJECTID), names(MPA_iNat_observations)))))
names(MPA_iNat_observations)[which(names(MPA_iNat_observations) == "")] <- setdiff(as.character(MPA_boundaries$OBJECTID), names(MPA_iNat_observations))

##### Edit data
#### Add a field including iNat observation counts
MPA_boundaries@data$iNat_observations_count <- NA
MPA_boundaries@data$iNat_observations_count[match(names(MPA_iNat_observations), MPA_boundaries@data$OBJECTID)] <- as.numeric(unlist(lapply(MPA_iNat_observations, function(x) ifelse(is.null(x), 0, nrow(x)))))
#### Add a taxonomic_ID field
MPA_iNat_observations <- lapply(MPA_iNat_observations, function(x) {
  x$taxonomic_ID <- ifelse(!(is.na(x$Phylum)), x$Phylum, ifelse(!(is.na(x$Kingdom)), x$Kingdom, ifelse(!(is.na(x$iconic_taxon_name)), x$iconic_taxon_name, NA)))
  x
})
##### Create objects
#### Color palette for MPA polygons
col_pal <- colorQuantile("Reds", MPA_boundaries@data$iNat_observations_count, n = 5)

function(input, output, session) {

##### -- Map -- #####
  
  output$mpa_map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(mean(bbox(MPA_boundaries)[1,]),
              mean(bbox(MPA_boundaries)[2,]),
              zoom=6 # set to 10 as 9 is a bit too zoomed out
      ) %>%   
      clearShapes() %>%
      addPolygons(data = MPA_boundaries,
                  layerId = ~OBJECTID,
                  color = "#444444",
                  fillColor = ~col_pal(iNat_observations_count),
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillOpacity = 0.8, 
                  weight = 1,
                  popup = paste0(MPA_boundaries$FULLNAME,
                                 "<br>",
                                 "<a target = '_blank' href = 'https://www.inaturalist.org/projects/california-mpas-", MPA_boundaries$iNat_project_name, "'>", "Go to iNaturalist project</a>"
                                 ),
                  label = MPA_boundaries$FULLNAME,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE) 
                  ) %>%
      addLegend("topleft",
                #pal = col_pal, values = MPA_boundaries$iNat_observations_count,
                colors = c(col_pal(0), col_pal(1), col_pal(11), col_pal(99), col_pal(678)),
                opacity = 1, title = "iNaturalist <br> observations",
                labels = c("0", "", "", "", "678 or more")
      )
  })

##### -- Overview -- #####

  #### "selected_mpa"
  output$selected_mpa <- renderText({
    as.character(MPA_boundaries$FULLNAME[MPA_boundaries$OBJECTID == input$mpa_map_shape_click$id])
    }) 
  
  #### "observations_donut"
  output$observations_donut <- renderHighchart({
    
    plot_data <- MPA_iNat_observations[[input$mpa_map_shape_click$id]] %>%
      group_by(quality_grade) %>%
      summarize(count = n())
    
    if (length(which(plot_data$quality_grade == "research")) > 0) plot_data$quality_grade[plot_data$quality_grade == "research"] <- "research grade"
    if (length(which(plot_data$quality_grade == "needs_id")) > 0) plot_data$quality_grade[plot_data$quality_grade == "needs_id"] <- "needs ID"
    if (length(which(plot_data$quality_grade == "casual")) > 0) plot_data$quality_grade[plot_data$quality_grade == "casual"] <- "casual"
    
    highchart() %>%
      hc_title(text = paste0(sum(plot_data$count), " Observations"),
               verticalAlign = "middle",
               margin = 20,
               style = list(color = "#144746", fontSize = "17px", fontFamily = "Helvetica", useHTML = TRUE)) %>%
      hc_chart(type = "pie") %>%
      hc_add_series_labels_values(labels = plot_data$quality_grade,
                                  dataLabels = list(enabled = FALSE),
                                  values = plot_data$count, 
                                  name = "Observations", color = c(grey(0.5), "gold", "greenyellow")[1:length(unique(plot_data$quality_grade))], 
                                  innerSize = "80%") %>%
      hc_plotOptions(series = list(showInLegend = TRUE)) %>%
      hc_legend(itemStyle = list(fontSize = "12px", fontFamily = "Helvetica"))
    
  })
  
  #### "observers_donut"
  output$observers_donut <- renderHighchart({
    
    plot_data <- MPA_iNat_observations[[input$mpa_map_shape_click$id]] %>%
      group_by(user_login) %>%
      summarize(count = n()) %>%
      mutate(type = ifelse(count >= 100, "frequent", 
                                  ifelse(count >= 10, "occasional", "rare"
                                         ))) %>%
      group_by(type) %>%
      summarize(count = n())
    
    highchart() %>%
      hc_title(text = paste0(sum(plot_data$count), " Observers"),
               verticalAlign = "middle",
               style = list(color = "#144746", fontSize = "17px", fontFamily = "Helvetica", useHTML = TRUE)) %>%
      hc_chart(type = "pie") %>%
      hc_add_series_labels_values(dataLabels = list(enabled = FALSE), 
                                  labels = plot_data$type, 
                                  values = plot_data$count, 
                                  name = "Observers", 
                                  color = c("plum", "tomato", "deepskyblue")[1:length(unique(plot_data$type))], 
                                  innerSize = "80%") %>%
      hc_plotOptions(series = list(showInLegend = TRUE)) %>%
      hc_legend(itemStyle = list(fontSize = "12px", fontFamily = "Helvetica"))
  })

  #### "taxa_donut"
  output$taxa_donut <- renderHighchart({
    
    unique_species <- MPA_iNat_observations[[input$mpa_map_shape_click$id]] %>%
      dplyr::filter(taxon.rank == "species" & !is.na(taxon.rank)) %>%
      dplyr::filter(complete.cases(taxon.name)) %>%
      dplyr::filter(!duplicated(taxon.name)) 
    
    plot_data <- unique_species %>%
      group_by(taxonomic_ID) %>%
      summarize(count = n())
    
    plot_data$taxonomic_ID[is.na(plot_data$taxonomic_ID)] <- "Other"
    
    highchart() %>%
      hc_title(text = paste0(length(unique(unique_species$taxon.name)), " Species"),
               verticalAlign = "middle",
               margin = 20,
               style = list(color = "#144746", fontSize = "17px", fontFamily = "Helvetica", useHTML = TRUE)) %>%
      hc_chart(type = "pie") %>%
      hc_add_series_labels_values(labels = plot_data$taxonomic_ID, 
                                  dataLabels = list(style = list(fontSize = "12px", fontFamily = "Helvetica")),
                                  values = plot_data$count, 
                                  name = "Species", 
                                  innerSize = "80%") 
  })

##### -- Temporal trends -- #####
  
  #### "iNat_observations_plot"
  output$iNat_observations_plot <- renderPlotly({
    
    focal_mpa <- MPA_iNat_observations[[input$mpa_map_shape_click$id]]
    
    if (!is.null(focal_mpa)){
    
      iNat_research_observations_by_date <- focal_mpa %>%
        group_by(observed_on, quality_grade) %>%
        summarise(count = n(), na.rm = TRUE) %>%
        dplyr::select(observed_on, quality_grade, count) %>%
        as.data.frame()
        
      iNat_research_observations_by_date$observed_on <- iNat_research_observations_by_date$observed_on %>% as.Date()
      names(iNat_research_observations_by_date)[1] <- "Date"
      iNat_research_observations_by_date$quality_grade[iNat_research_observations_by_date$quality_grade == "research"] <- "Research grade"
      iNat_research_observations_by_date$quality_grade[iNat_research_observations_by_date$quality_grade == "needs_id"] <- "Needs ID"
      iNat_research_observations_by_date$quality_grade[iNat_research_observations_by_date$quality_grade == "casual"] <- "Casual"
      
      iNat_research_observations_by_date <- iNat_research_observations_by_date %>% dplyr::filter(!is.na(Date) & Date >= "2000-01-01" & Date <= "2018-12-31")
      
      p <- plot_ly(iNat_research_observations_by_date, x = ~Date) %>%
        add_bars(y = ~count, color = ~quality_grade, marker = list(size = length(unique(iNat_research_observations_by_date$quality_grade)), line = list(width = 12)), text= ~count, hoverinfo = 'text') %>%
        config(displayModeBar = FALSE) %>%
        layout(
          xaxis = list(
            barmode = "stack",
            range = c(as.Date("2000-01-01"), as.Date("2018-12-31")),
            rangeselector = list(enabled = FALSE),
            rangeslider = list(type = "date", 
                               font = list(family = "Helvetica", size = 13),
                               bgcolor = grey(0.9)),
            tickfont = list(family = "Helvetica", size = 14),
            ticklen = 8,
            tickcolor = "white",
            title = ""
          ),
          yaxis = list(title = "Number of observations", 
                       titlefont = list(family = "Helvetica", size = 14), 
                       tickfont = list(family = "Helvetica", size = 14)),
          legend = list(x = 0.1, 
                        y = 0.9, 
                        font = list(family = "Helvetica", size = 13)
          )
          
        )
      
      p
      
    } 
    
    })

  #### "iNat_species_plot"
  output$iNat_species_plot <- renderPlotly({
    
    focal_mpa <- MPA_iNat_observations[[input$mpa_map_shape_click$id]]
    
    if (!is.null(focal_mpa)){
      
      iNat_species_by_date <- focal_mpa %>%
        dplyr::filter(taxon.rank == "species") %>%
        dplyr::select(taxon.name, observed_on, taxonomic_ID) %>%
        distinct(taxon.name, observed_on, .keep_all = TRUE) %>%
        group_by(observed_on, taxonomic_ID) %>%
        summarise(count = n(), na.rm = TRUE) %>%
        dplyr::select(observed_on, taxonomic_ID, count) %>%
        as.data.frame()
      
      iNat_species_by_date$observed_on <- iNat_species_by_date$observed_on %>% as.Date()
      iNat_species_by_date$taxonomic_ID[which(is.na(iNat_species_by_date$taxonomic_ID))] <- "Other"

      names(iNat_species_by_date)[1] <- "Date"
      iNat_species_by_date <- iNat_species_by_date %>% dplyr::filter(!is.na(Date) & Date >= "2000-01-01" & Date <= "2018-12-31")
      
      p <- plot_ly(iNat_species_by_date, x = ~Date) %>%
        add_bars(y = ~count, color = ~taxonomic_ID, marker = list(size = length(unique(iNat_species_by_date$taxonomic_ID)), line = list(width = 12)), text= ~count, hoverinfo = 'text') %>%
        config(displayModeBar = FALSE) %>%
        layout(
          xaxis = list(
            barmode = "stack",
            range = c(as.Date("2000-01-01"), as.Date("2018-12-31")),
            rangeselector = list(enabled = FALSE),
            rangeslider = list(type = "date", 
                               font = list(family = "Helvetica", size = 13),
                               bgcolor = grey(0.9)),
            
            tickfont = list(family = "Helvetica", size = 14),
            ticklen = 8,
            tickcolor = "white",
            title = ""
          ),
          
          yaxis = list(title = "Number of species observed", 
                       titlefont = list(family = "Helvetica", size = 14), 
                       tickfont = list(family = "Helvetica", size = 14)),
          legend = list(y = -0.7, 
                        font = list(family = "Helvetica", size = 13),
                        orientation = "h"
          )
          
        )
      
      p
    }
    
  })
  
  ##### -- Taxonomic breakdown -- #####
  
  #### "iNat_species_plot"
  output$taxon_tree <- renderCollapsibleTree({
    
    focal_mpa <- MPA_iNat_observations[[input$mpa_map_shape_click$id]]
    
    if (!is.null(focal_mpa)){
      
      plot_dat <- focal_mpa %>% dplyr::filter(taxon.rank == "species" & !is.na(taxon.rank) & quality_grade == "research") 
        
      collapsibleTree(
        plot_dat,
        hierarchy = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "taxon.name"),
        width = "100%",
        zoomable = FALSE
        )
      
    }
    
  })
  
}
