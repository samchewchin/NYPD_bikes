library(shiny)
library(leaflet)
library(leaflet.extras)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinyWidgets)
library(thematic)

# ---- THEMES ----
dark_theme <- bs_theme(
  bootswatch = "darkly",
  base_font = font_google("Inter"),
  bg = "#222222", fg = "#FFFFAA",
  primary = "#FFFFAA", secondary = "#FFFFAA"
)

mets_theme <- bs_theme(
  bootswatch = "flatly",
  base_font = font_google("Inter"),
  bg = "#f2f2f2", fg = "#002D72",
  primary = "#002D72", secondary = "#FF5910"
)

yankees_theme <- bs_theme(
  bootswatch = "flatly",
  base_font = font_google("Inter"),
  bg = "#f0f0f0", fg = "#001C43",
  primary = "#001C43", secondary = "#E8002F"
)

# ---- HELPER FUNCTION FOR BARPLOTS ----
make_barplot <- function(df, value_col, fill_color, plot_mode, title_label, dark_mode = TRUE) {
  if (plot_mode == "totals") {
    df_plot <- df %>%
      group_by(month) %>%
      summarise(value = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  } else {
    df_plot <- df %>%
      mutate(year = as.integer(format(date, "%Y"))) %>%
      group_by(year, month) %>%
      summarise(month_total = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
      group_by(month) %>%
      summarise(value = mean(month_total, na.rm = TRUE), .groups = "drop")
  }
  
  df_plot <- df_plot %>% complete(month = 1:12, fill = list(value = 0))
  
  font_col <- if (dark_mode) "#FFFFAA" else "#222222"
  bg_col <- if (dark_mode) "#222222" else "#FFFFFF"
  
  ggplot(df_plot, aes(x = factor(month, levels = 1:12, labels = month.abb), y = value)) +
    geom_col(fill = fill_color, alpha = 0.9) +
    theme_minimal(base_family = "Inter") +
    theme(
      text = element_text(color = font_col, size = 11),
      axis.text = element_text(color = font_col, size = 10),
      panel.background = element_rect(fill = bg_col, color = NA),
      plot.background = element_rect(fill = bg_col, color = NA),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(size = 12, hjust = 0.5, color = font_col, margin = margin(b = 5))
    ) +
    labs(title = paste0(title_label, ifelse(plot_mode == "totals", " (Totals)", " (Averages)")))
}

# ---- UI ----
ui <- page_navbar(
  title = div(style = "color:#FFFFAA; font-weight:bold;", "Where the Going Gets Rough: Cycling Collisions in NYC"),
  theme = dark_theme,
  
  nav_panel(
    "",
    layout_sidebar(
      sidebar = sidebar(
        width = 240,
        open = TRUE,
        radioButtons("layer_type", "Map type:",
                     choices = c("Raw points" = "points", "Heatmap" = "density"),
                     selected = "density"),
        checkboxGroupInput("data_type", "Show data for:",
                           choices = c("Injuries" = "injuries", "Fatalities" = "fatalities"),
                           selected = c("injuries", "fatalities")),
        radioButtons("plot_mode", "Stat mode:",
                     choices = c("Totals" = "totals", "Averages" = "averages"),
                     selected = "totals", inline = TRUE),
        sliderInput("date_range", "Select date range:",
                    min = as.Date("2012-01-01"),
                    max = as.Date("2024-12-31"),
                    value = c(as.Date("2012-01-01"), as.Date("2024-12-31")),
                    step = 30,
                    animate = animationOptions(interval = 1000, loop = FALSE)),
        dateInput("date_start", "Start date:", value = as.Date("2012-01-01")),
        dateInput("date_end", "End date:", value = as.Date("2024-12-31")),
        sliderInput("month_range", "Select months within year:",
                    min = 1, max = 12, value = c(1, 12), step = 1),
        div(
          style = "margin-top: 0.75rem;",
          actionButton("step_back", "← 30 days"),
          actionButton("step_forward", "30 days →"),
          actionButton("reset_range", "Full Range")
        ),
        hr(),
        selectInput("theme_choice", "Page Theme:",
                    choices = c("Dark", "Mets", "Yankees"), selected = "Dark")
      ),
      
      # ---- Main content ----
      div(
        style = "display: flex; flex-direction: column; align-items: center; gap: 1rem; width: 100%;",
        
        leafletOutput("map", height = "50vh", width = "100%"),
        
        textOutput("range_label"),
        
        div(
          style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 1rem; width: 100%; max-width: 1100px;",
          card(
            card_header(uiOutput("injured_header")),
            card_body(
              uiOutput("injured_means"),
              plotOutput("monthly_injured", height = "200px")
            ),
            style="flex:1; min-width:200px; max-width:500px;"
          ),
          card(
            card_header(uiOutput("fatal_header")),
            card_body(
              uiOutput("fatal_means"),
              plotOutput("monthly_killed", height = "200px")
            ),
            style="flex:1; min-width:200px; max-width:500px;"
          )
        ),
        
        card(
          style = "width: auto; max-width: 800px;",
          card_body(
            uiOutput("data_source")
          )
        ),
        # Dedication line
        div(
          style = "margin-top: 0.5rem; text-align: center;",
          uiOutput("dedication")
        )
        
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  thematic_shiny()  # enable thematic for auto theming
  
  # ---- Data ----
  df <- read.csv("appdata.csv")
  df$date <- as.Date(df$date)
  df$month <- as.numeric(format(df$date, "%m"))
  
  min_date <- min(df$date, na.rm = TRUE)
  max_date <- max(df$date, na.rm = TRUE)
  updateSliderInput(session, "date_range", min = min_date, max = max_date, value = c(min_date, max_date))
  updateDateInput(session, "date_start", value = min_date)
  updateDateInput(session, "date_end", value = max_date)
  
  # ---- Tie the date inputs and slider together ----
  observeEvent(input$date_start, {
    new_end <- max(input$date_end, input$date_start)
    updateDateInput(session, "date_end", value = new_end)
    updateSliderInput(session, "date_range", value = c(input$date_start, new_end))
  }, ignoreInit = TRUE)
  
  observeEvent(input$date_end, {
    new_start <- min(input$date_start, input$date_end)
    updateDateInput(session, "date_start", value = new_start)
    updateSliderInput(session, "date_range", value = c(new_start, input$date_end))
  }, ignoreInit = TRUE)
  
  observeEvent(input$date_range, {
    updateDateInput(session, "date_start", value = input$date_range[1])
    updateDateInput(session, "date_end", value = input$date_range[2])
  }, ignoreInit = TRUE)
  
  # ---- Reactive filtering ----
  filtered_data <- reactive({
    req(input$date_range, input$month_range)
    df %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(month >= input$month_range[1], month <= input$month_range[2])
  })
  
  # ---- Dynamic color palette ----
  theme_colors <- reactive({
    switch(input$theme_choice,
           "Dark" = list(
             injury = "limegreen",
             fatal = "hotpink",
             text = "#FFFFAA",
             map_tile = providers$CartoDB.DarkMatter,
             injury_gradient = viridis::viridis(6, option = "D"),
             fatal_gradient  = viridis::viridis(6, option = "F")
           ),
           "Mets" = list(
             injury = "#002D72",
             fatal = "#FF5910",
             text = "#333333",
             map_tile = providers$CartoDB.Positron,
             injury_gradient = colorRampPalette(c("#D0E1F9", "#002D72"))(6),
             fatal_gradient = colorRampPalette(c("#FFE6D0", "#FF5910"))(6)
           ),
           "Yankees" = list(
             injury = "#001C43",
             fatal = "#E8002F",
             text = "#333333",
             map_tile = providers$CartoDB.Positron,
             injury_gradient = colorRampPalette(c("#D9E1F2", "#001C43"))(6),
             fatal_gradient = colorRampPalette(c("#FFD1D6", "#E8002F"))(6)
           )
    )
  })
  
  # ---- Apply theme dynamically ----
  observeEvent(input$theme_choice, {
    theme_selected <- switch(
      input$theme_choice,
      "Dark" = dark_theme,
      "Mets" = mets_theme,
      "Yankees" = yankees_theme
    )
    session$setCurrentTheme(theme_selected)
  })
  
  # ---- Map ----
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(theme_colors()$map_tile) %>%
      setView(lng = -73.916, lat = 40.74081, zoom = 11)
  })
  ## --- Static (all points) Observe block ----
  # observe({
  #   df_filtered <- filtered_data()
  #   colors <- theme_colors()
  #   leafletProxy("map", data = df_filtered) %>%
  #     clearMarkers() %>%
  #     clearHeatmap() %>%
  #     addProviderTiles(colors$map_tile)
  #   
  #   if ("points" %in% input$layer_type) {
  #     if ("injuries" %in% input$data_type) {
  #       leafletProxy("map", data = df_filtered[df_filtered$injured > 0, ]) %>%
  #         addCircleMarkers(~lon, ~lat, radius = 1, color = colors$injury,
  #                          fillOpacity = 0.5, popup = ~popup_label)
  #     }
  #     if ("fatalities" %in% input$data_type) {
  #       leafletProxy("map", data = df_filtered[df_filtered$killed > 0, ]) %>%
  #         addCircleMarkers(~lon, ~lat, radius = 2, color = colors$fatal,
  #                          fillOpacity = 1, popup = ~popup_label)
  #     }
  #   }
  #   
  #   if ("density" %in% input$layer_type) {
  #     if ("injuries" %in% input$data_type) {
  #       leafletProxy("map", data = df_filtered[df_filtered$injured > 0, ]) %>%
  #         addHeatmap(lng = ~lon, lat = ~lat, intensity = ~injured,
  #                    radius = 20, blur = 10, gradient = colors$injury_gradient)
  #     }
  #     if ("fatalities" %in% input$data_type) {
  #       leafletProxy("map", data = df_filtered[df_filtered$killed > 0, ]) %>%
  #         addHeatmap(lng = ~lon, lat = ~lat, intensity = ~killed * 80,
  #                    radius = 25, blur = 15, gradient = colors$fatal_gradient)
  #     }
  #   }
  # })
  
  ## ---- Viewport aware observe ----
  observe({
    # Get filtered data and theme colors
    df_filtered <- filtered_data()
    colors <- theme_colors()
    
    # Clear previous markers and heatmaps
    leafletProxy("map", data = df_filtered) %>%
      clearMarkers() %>%
      clearHeatmap() %>%
      addProviderTiles(colors$map_tile)
    
    # ---- Ensure map bounds exist ----
    bounds <- input$map_bounds
    zoom <- input$map_zoom
    if (!is.null(bounds) && !is.null(zoom)) {
      
      # Filter both injuries and fatalities by viewport
      df_in_view <- df_filtered %>%
        filter(lon >= bounds$west & lon <= bounds$east,
               lat >= bounds$south & lat <= bounds$north)
      
      # ---- Adaptive thinning for Injuries only ----
      df_injuries <- df_in_view %>% filter(injured > 0)
      if (nrow(df_injuries) > 0) {
        # Determine grid resolution based on zoom (adjust as desired)
        n_grid <- min(max(50 * zoom / 10, 50), 300)
        lng_range <- bounds$east - bounds$west
        lat_range <- bounds$north - bounds$south
        
        df_injuries <- df_injuries %>%
          mutate(
            grid_x = floor((lon - bounds$west)/lng_range * n_grid),
            grid_y = floor((lat - bounds$south)/lat_range * n_grid),
            grid_id = paste0(grid_x, "_", grid_y)
          ) %>%
          group_by(grid_id) %>%
          slice_sample(n = 8) %>%  # Keep one injury per grid cell
          ungroup()
      }
      
      # ---- Fatalities: viewport-filtered only, no thinning ----
      df_fatal <- df_in_view %>% 
        filter(killed > 0)
      
      # ---- Plot Injuries ----
      if ("points" %in% input$layer_type && "injuries" %in% input$data_type && nrow(df_injuries) > 0) {
        leafletProxy("map", data = df_injuries) %>%
          addCircleMarkers(
            ~lon, ~lat, radius = 1, color = colors$injury,
            fillOpacity = 0.2, popup = ~popup_label
          )
      }
      
      # ---- Plot Fatalities ----
      if ("points" %in% input$layer_type && "fatalities" %in% input$data_type && nrow(df_fatal) > 0) {
        leafletProxy("map", data = df_fatal) %>%
          addCircleMarkers(
            ~lon, ~lat, radius = 2, color = colors$fatal,
            fillOpacity = 1, popup = ~popup_label
          )
      }
      
      # ---- Density Heatmaps ----
      if ("density" %in% input$layer_type) {
        if ("injuries" %in% input$data_type && nrow(df_in_view[df_in_view$injured > 0, ]) > 0) {
          leafletProxy("map", data = df_in_view[df_in_view$injured > 0, ]) %>%
            addHeatmap(
              lng = ~lon, lat = ~lat, intensity = ~injured,
              radius = 15, blur = 9, gradient = colors$injury_gradient
            )
        }
        if ("fatalities" %in% input$data_type && nrow(df_in_view[df_in_view$killed > 0, ]) > 0) {
          leafletProxy("map", data = df_in_view[df_in_view$killed > 0, ]) %>%
            addHeatmap(
              lng = ~lon, lat = ~lat, intensity = ~killed * 80,
              radius = 20, blur = 12, gradient = colors$fatal_gradient
            )
        }
      }
    }
  })
  
  
  
  
  # ---- Date label ----
  output$range_label <- renderText({
    paste0(format(input$date_range[1], "%b %d, %Y"), " – ", format(input$date_range[2], "%b %d, %Y"))
  })
  
  # ---- Step buttons ----
  observeEvent(input$step_back, {
    new_start <- input$date_range[1] - 30
    new_end <- input$date_range[2] - 30
    if (new_start < min(df$date)) new_start <- min(df$date)
    updateSliderInput(session, "date_range", value = c(new_start, new_end))
  })
  observeEvent(input$step_forward, {
    new_start <- input$date_range[1] + 30
    new_end <- input$date_range[2] + 30
    if (new_end > max(df$date)) new_end <- max(df$date)
    updateSliderInput(session, "date_range", value = c(new_start, new_end))
  })
  observeEvent(input$reset_range, {
    updateSliderInput(session, "date_range", value = c(min(df$date), max(df$date)))
  })
  
  # ---- Headers and Means ----
  output$injured_header <- renderUI({
    total <- sum(filtered_data()$injured, na.rm = TRUE)
    div(style = paste0("color:", theme_colors()$injury, "; font-weight:bold;"), paste0("Cyclists Injured: ", format(total, big.mark=",")))
  })
  output$fatal_header <- renderUI({
    total <- sum(filtered_data()$killed, na.rm = TRUE)
    div(style = paste0("color:", theme_colors()$fatal, "; font-weight:bold;"), paste0("Cyclists Killed: ", format(total, big.mark=",")))
  })
  
  mean_fun <- function(col) {
    total <- sum(col, na.rm = TRUE)
    years <- as.numeric(difftime(max(filtered_data()$date), min(filtered_data()$date), units = "days")) / 365.25
    months <- as.numeric(difftime(max(filtered_data()$date), min(filtered_data()$date), units = "days")) / 30.44
    list(annual = round(total / years, 1), monthly = round(total / months, 1))
  }
  
  output$injured_means <- renderUI({
    means <- mean_fun(filtered_data()$injured)
    tags$div(style=paste0("font-size:0.8rem; color:", theme_colors()$text, ";"),
             "Annual mean: ", span(style=paste0("color:", theme_colors()$injury, ";"), format(means$annual, big.mark=",")), br(),
             "Monthly mean: ", span(style=paste0("color:", theme_colors()$injury, ";"), format(means$monthly, big.mark=",")))
  })
  output$fatal_means <- renderUI({
    means <- mean_fun(filtered_data()$killed)
    tags$div(style=paste0("font-size:0.8rem; color:", theme_colors()$text, ";"),
             "Annual mean: ", span(style=paste0("color:", theme_colors()$fatal, ";"), format(means$annual, big.mark=",")), br(),
             "Monthly mean: ", span(style=paste0("color:", theme_colors()$fatal, ";"), format(means$monthly, big.mark=",")))
  })
  
  # ---- Barplots ----
  output$monthly_injured <- renderPlot({
    make_barplot(filtered_data(), "injured", theme_colors()$injury, input$plot_mode, "Cyclists Injured",
                 dark_mode = input$theme_choice == "Dark")
  })
  output$monthly_killed <- renderPlot({
    make_barplot(filtered_data(), "killed", theme_colors()$fatal, input$plot_mode, "Cyclists Killed",
                 dark_mode = input$theme_choice == "Dark")
  })
  
  # ---- Data source text ----
  output$data_source <- renderUI({
    tags$small(
      style = paste0("color:", theme_colors()$text, ";"),
      "Data source: ",
      tags$a(
        href = "https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95/about_data",  # Replace with your actual URL
        "NYPD Open Data",
        target = "_blank",  # Opens in a new tab
        style = "color: inherit; text-decoration: underline;"
      ),
      ". This dashboard is not an exhaustive representation. Thousands of records in the database lacked coordinates and are not summarized here."
    )
  })
  # ---- dedication text ----
  output$dedication <- renderUI({
    tags$small(
      style = paste0(
        "color:", theme_colors()$text, ";",
        "font-style: italic;",
        "opacity: 0.8;"
      ),
      "This page is dedicated to the bicycle riders of New York City."
    )
  })
  
}


shinyApp(ui, server)
