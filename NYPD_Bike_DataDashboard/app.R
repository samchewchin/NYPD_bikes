# refined app version with cleaner plots, smaller text, fixed buttons, and sidebar toggle
library(shiny)
library(leaflet)
library(leaflet.extras)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinyWidgets)

# ---- Helper plotting function ----
make_barplot <- function(df, value_col, fill_color, plot_mode, title_label) {
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
  
  ggplot(df_plot, aes(x = factor(month, levels = 1:12, labels = month.abb), y = value)) +
    geom_col(fill = fill_color, alpha = 0.9, width = 0.75) +
    theme_minimal(base_family = "Inter") +
    theme(
      text = element_text(color = "#FFFFAA", size = 12),
      axis.text = element_text(color = "#FFFFAA", size = 10),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#222222", color = NA),
      plot.background = element_rect(fill = "#222222", color = NA),
      plot.title = element_text(size = 14, hjust = 0.5, color = "#FFFFAA", margin = margin(b = 3))
    ) +
    labs(title = paste0(title_label, ifelse(plot_mode == "totals", " (Totals)", " (Averages)")))
}

# ---- UI ----
ui <- page_navbar(
  title = div(style = "color:#FFAA33; font-weight:bold;", "Where the Going Gets Rough: Cycling Collisions in NYC"),
  theme = bs_theme(
    bootswatch = "darkly",
    base_font = font_google("Inter"),
    bg = "#222222", fg = "#FFFFAA",
    primary = "#FFFFAA", secondary = "#FFFFAA"
  ),
  
  nav_panel(
    "",
    layout_sidebar(
      sidebar = sidebar(
        width = 220,
        open = TRUE,
        radioButtons("layer_type", "Display type:",
                     choices = c("Raw points" = "points", "Heatmap" = "density"),
                     selected = "density"),
        checkboxGroupInput("data_type", "Plot data:",
                           choices = c("Injuries" = "injuries", "Fatalities" = "fatalities"),
                           selected = c("injuries", "fatalities")),
        div(
          style = "margin-bottom: 0.5rem;",
          tags$label("Month stats mode:", style="font-size:1rem; margin-right: 6px;"),
          switchInput(
            inputId = "plot_mode",
            label = NULL,
            onLabel = "Totals",
            offLabel = "Avg",
            size = "mini",
            handleWidth = 30,
            onStatus = "success",
            offStatus = "warning",
            value = TRUE
          )
        ),
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
          style = "margin-top: 1rem;",
          actionButton("step_back", "← 30 days"),
          actionButton("step_forward", "30 days →"),
          actionButton("reset_range", "Full Range")
        )
      ),
      
      # ---- Main content ----
      div(
        style = "display: flex; flex-direction: column; align-items: center; gap: 1rem; width: 100%;",
        
        leafletOutput("map", height = "48vh", width = "100%"),
        textOutput("range_label"),
        
        div(
          style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 1rem; width: 100%; max-width: 1100px;",
          card(
            card_header(uiOutput("injured_header")),
            card_body(
              tags$div(style="font-size:0.8rem; color:#CCCC77;",
                       "Annual mean: ", span(style="color:limegreen;", textOutput("injured_annual_mean", inline=TRUE)), br(),
                       "Monthly mean: ", span(style="color:limegreen;", textOutput("injured_monthly_mean", inline=TRUE))
              ),
              plotOutput("monthly_injured", height = "230px")   # enlarged plot
            ),
            style="flex:1; min-width:200px; max-width:500px;"
          ),
          card(
            card_header(uiOutput("fatal_header")),
            card_body(
              tags$div(style="font-size:0.8rem; color:#CCCC77;",
                       "Annual mean: ", span(style="color:hotpink;", textOutput("fatal_annual_mean", inline=TRUE)), br(),
                       "Monthly mean: ", span(style="color:hotpink;", textOutput("fatal_monthly_mean", inline=TRUE))
              ),
              plotOutput("monthly_killed", height = "230px")   # enlarged plot
            ),
            style="flex:1; min-width:200px; max-width:500px;"
          )
        ),
        
        card(
          style = "width: auto; max-width: 800px;",
          card_body(
            tags$small(
              style = "color:#FFDD55;",
              "Data source: NYPD open data. This visualization includes only incidents where cyclist coordinates were available.
              Data are incomplete and not authoritative, but may highlight spatial or temporal hotspots."
            )
          )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  df <- read.csv("appdata.csv")
  df$date <- as.Date(df$date)
  df$month <- as.numeric(format(df$date, "%m"))
  
  min_date <- min(df$date, na.rm = TRUE)
  max_date <- max(df$date, na.rm = TRUE)
  updateSliderInput(session, "date_range", min = min_date, max = max_date, value = c(min_date, max_date))
  
  filtered_data <- reactive({
    req(input$date_range, input$month_range)
    df %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(month >= input$month_range[1], month <= input$month_range[2])
  })
  
  output$range_label <- renderText({
    paste0(
           format(input$date_range[1], "%b %d, %Y"), " – ",
           format(input$date_range[2], "%b %d, %Y"))
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
  
  # ---- Headers ----
  output$injured_header <- renderUI({
    total <- sum(filtered_data()$injured, na.rm = TRUE)
    div(style = "color:limegreen;", paste0("Cyclists Injured: ", format(total, big.mark=",")))
  })
  output$fatal_header <- renderUI({
    total <- sum(filtered_data()$killed, na.rm = TRUE)
    div(style = "color:hotpink;", paste0("Cyclists Killed: ", format(total, big.mark=",")))
  })
  
  # ---- Means ----
  mean_fun <- function(col) {
    total <- sum(col, na.rm = TRUE)
    years <- as.numeric(difftime(max(filtered_data()$date), min(filtered_data()$date), units = "days")) / 365.25
    months <- as.numeric(difftime(max(filtered_data()$date), min(filtered_data()$date), units = "days")) / 30.44
    list(annual = round(total / years, 1), monthly = round(total / months, 1))
  }
  
  output$injured_annual_mean <- renderText({ format(mean_fun(filtered_data()$injured)$annual, big.mark = ",") })
  output$injured_monthly_mean <- renderText({ format(mean_fun(filtered_data()$injured)$monthly, big.mark = ",") })
  output$fatal_annual_mean <- renderText({ format(mean_fun(filtered_data()$killed)$annual, big.mark = ",") })
  output$fatal_monthly_mean <- renderText({ format(mean_fun(filtered_data()$killed)$monthly, big.mark = ",") })
  
  # ---- Map ----
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = -73.916, lat = 40.74081, zoom = 11)
  })
  
  observe({
    df_filtered <- filtered_data()
    leafletProxy("map", data = df_filtered) %>%
      clearMarkers() %>%
      clearHeatmap()
    
    if ("points" %in% input$layer_type) {
      if ("injuries" %in% input$data_type) {
        leafletProxy("map", data = df_filtered[df_filtered$injured > 0, ]) %>%
          addCircleMarkers(~lon, ~lat, radius = 0.5, color = "limegreen",
                           fillOpacity = 0.5, popup = ~popup_label)
      }
      if ("fatalities" %in% input$data_type) {
        leafletProxy("map", data = df_filtered[df_filtered$killed > 0, ]) %>%
          addCircleMarkers(~lon, ~lat, radius = 2, color = "hotpink",
                           fillOpacity = 1, popup = ~popup_label)
      }
    }
    
    if ("density" %in% input$layer_type) {
      if ("injuries" %in% input$data_type) {
        leafletProxy("map", data = df_filtered[df_filtered$injured > 0, ]) %>%
          addHeatmap(lng = ~lon, lat = ~lat, intensity = ~injured,
                     radius = 20, blur = 10, gradient = viridis::viridis(6, option = "D"))
      }
      if ("fatalities" %in% input$data_type) {
        leafletProxy("map", data = df_filtered[df_filtered$killed > 0, ]) %>%
          addHeatmap(lng = ~lon, lat = ~lat, intensity = ~killed * 80,
                     radius = 25, blur = 15, gradient = viridis::viridis(5, option = "F"))
      }
    }
  })
  
  # ---- Barplots ----
  output$monthly_injured <- renderPlot({
    make_barplot(filtered_data(), "injured", "limegreen",
                 if (isTRUE(input$plot_mode)) "totals" else "averages", "Cyclists Injured")
  })
  output$monthly_killed <- renderPlot({
    make_barplot(filtered_data(), "killed", "hotpink",
                 if (isTRUE(input$plot_mode)) "totals" else "averages", "Cyclists Killed")
  })
}

shinyApp(ui, server)
