library(shiny)
library(leaflet)
library(leaflet.extras)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)

# ---- UI ----
ui <- page_navbar(
  title = div(
    style = "color:#FFAA33; font-weight:bold;",
    "Where the Going Gets Rough: Cycling Collisions in NYC"
  ),
  
  theme = bs_theme(
    bootswatch = "darkly",
    base_font = font_google("Inter"),
    bg = "#222222",       # dark background
    fg = "#FFFFAA",       # pale yellow default font
    primary = "#FFFFAA",
    secondary = "#FFFFAA"
  ),
  
  nav_panel(
    "",
    layout_sidebar(
      sidebar = sidebar(
        width = 200,
        open = TRUE,
        
        # 1. Display type first
        radioButtons("layer_type", "Display type:",
                     choices = c("Raw points" = "points",
                                 "Density/Heatmap" = "density"),
                     selected = "points"),
        
        # 2. Injuries/Fatalities checkboxes
        checkboxGroupInput("data_type", "Show data for:",
                           choices = c("Injuries" = "injuries",
                                       "Fatalities" = "fatalities"),
                           selected = c("injuries", "fatalities")),
        
        # 3. Date range slider (animated)
        sliderInput("date_range", "Select date range:",
                    min = as.Date("2012-01-01"),
                    max = as.Date("2024-12-31"),
                    value = c(as.Date("2012-01-01"), as.Date("2024-12-31")),
                    step = 30,
                    animate = animationOptions(interval = 1000, loop = FALSE)),
        
        # 4. Manual date entry
        dateInput("date_start", "Start date:", value = as.Date("2012-01-01")),
        dateInput("date_end", "End date:", value = as.Date("2024-12-31")),
        
        # 5. Months within year
        sliderInput("month_range", "Select months within year:",
                    min = 1, max = 12, value = c(1, 12), step = 1),
        
        # 6. Step forward/back buttons
        div(
          style = "margin-top: 1rem;",
          actionButton("step_back", "← 30 days"),
          actionButton("step_forward", "30 days →"),
          actionButton("reset_range", "Full Range")
        )
      ),
      
      # ---- Main content: left map + footer, right cards ----
      layout_column_wrap(
        width = 24,
        gap = "1rem",
        
        # Left column: map + footer
        layout_column_wrap(
          width = 18,
          gap = "1rem",
          div(
            style = "display:flex; flex-direction: column; gap:0.5rem;",
            
            card(
              full_screen = TRUE,
              leafletOutput("map", height = "85vh")
            ),
            
            card(
              style = "padding:0.5rem;",
              tags$small(
                style = "color:#FFDD55;",
                "Data source: NYPD open data. This visualization includes only incidents where cyclist coordinates were available.
                Data are incomplete and not authoritative, but may highlight spatial or temporal hotspots."
              )
            )
          )
        ),
        
        # Right column: totals stacked
        layout_column_wrap(
          width = 6,
          gap = "1rem",
          div(
            style = "display:flex; flex-direction:column; gap:1rem;",
            
            # Date range card
            card(
              card_header("Data displayed:"),
              card_body(h4(textOutput("range_label")))
            ),
            
            # Cyclists Injured card
            card(
              card_header(uiOutput("injured_header")),  # Total displayed in header
              card_body(
                tags$div(
                  style = "margin-top:0.5rem; font-size:0.9rem;",
                  # Annual mean label and value
                  tags$span(style="color:#CCCC77; font-weight:bold;", "Annual mean: "),
                  tags$span(style="color:limegreen;", textOutput("injured_annual_mean", inline=TRUE)),
                  br(),
                  # Monthly mean label and value
                  tags$span(style="color:#CCCC77; font-weight:bold;", "Monthly mean: "),
                  tags$span(style="color:limegreen;", textOutput("injured_monthly_mean", inline=TRUE)),
                  plotOutput("monthly_injured", height = "120px")
                )
              )
            ),
            
            # Cyclists Killed card
            card(
              card_header(uiOutput("fatal_header")),  # Total displayed in header
              card_body(
                tags$div(
                  style = "margin-top:0.5rem; font-size:0.9rem;",
                  # Annual mean label and value
                  tags$span(style="color:#CCCC77; font-weight:bold;", "Annual mean: "),
                  tags$span(style="color:hotpink;", textOutput("fatal_annual_mean", inline=TRUE)),
                  br(),
                  # Monthly mean label and value
                  tags$span(style="color:#CCCC77; font-weight:bold;", "Monthly mean: "),
                  tags$span(style="color:hotpink;", textOutput("fatal_monthly_mean", inline=TRUE)),
                  plotOutput("monthly_killed", height = "120px"),
                )
              )
            )
          )
        )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  df <- read.csv("appdata.csv")
  df$date <- as.Date(df$date)
  df$month <- as.numeric(format(df$date, "%m"))
  
  # ---- Filtered data ----
  filtered_data <- reactive({
    req(input$date_range, input$month_range)
    df %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(month >= input$month_range[1], month <= input$month_range[2])
  })
  
  # ---- Step buttons ----
  observeEvent(input$step_forward, {
    step_days <- 30
    new_start <- input$date_range[1] + step_days
    new_end   <- input$date_range[2] + step_days
    updateSliderInput(session, "date_range", value = c(new_start, new_end))
  })
  
  observeEvent(input$step_back, {
    step_days <- 30
    new_start <- input$date_range[1] - step_days
    new_end   <- input$date_range[2] - step_days
    updateSliderInput(session, "date_range", value = c(new_start, new_end))
  })
  
  observeEvent(input$reset_range, {
    updateSliderInput(session, "date_range",
                      value = c(min(df$date), max(df$date)))
  })
  
  # ---- Range label ----
  output$range_label <- renderText({
    rng <- input$date_range
    paste0(format(rng[1], "%b %d, %Y"), " – ", format(rng[2], "%b %d, %Y"))
  })
  
  # ---- Dynamic card headers for totals ----
  output$injured_header <- renderUI({
    total <- sum(filtered_data()$injured, na.rm = TRUE)
    div(style="color:limegreen;", paste0("Cyclists Injured: ", format(total, big.mark=",")))
  })
  
  output$fatal_header <- renderUI({
    total <- sum(filtered_data()$killed, na.rm = TRUE)
    div(style="color:hotpink;", paste0("Cyclists Killed: ", format(total, big.mark=",")))
  })
  
  # ---- Annual / Monthly means for Injured ----
  output$injured_annual_mean <- renderText({
    total <- sum(filtered_data()$injured, na.rm = TRUE)
    years <- as.numeric(difftime(max(filtered_data()$date), min(filtered_data()$date), units="days"))/365.25
    format(round(total/years, 1), big.mark=",")
  })
  
  output$injured_monthly_mean <- renderText({
    total <- sum(filtered_data()$injured, na.rm = TRUE)
    months <- as.numeric(difftime(max(filtered_data()$date), min(filtered_data()$date), units="days"))/30.44
    format(round(total/months, 1), big.mark=",")
  })
  
  # ---- Annual / Monthly means for Killed ----
  output$fatal_annual_mean <- renderText({
    total <- sum(filtered_data()$killed, na.rm = TRUE)
    years <- as.numeric(difftime(max(filtered_data()$date), min(filtered_data()$date), units="days"))/365.25
    format(round(total/years, 1), big.mark=",")
  })
  
  output$fatal_monthly_mean <- renderText({
    total <- sum(filtered_data()$killed, na.rm = TRUE)
    months <- as.numeric(difftime(max(filtered_data()$date), min(filtered_data()$date), units="days"))/30.44
    format(round(total/months, 1), big.mark=",")
  })
  
  
  # ---- Leaflet map ----
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = -74.00, lat = 40.7128, zoom = 11)
  })
  
  observe({
    df_filtered <- filtered_data()
    
    leafletProxy("map", data = df_filtered) %>%
      clearMarkers() %>%
      clearHeatmap()
    
    if ("points" %in% input$layer_type) {
      if ("injuries" %in% input$data_type) {
        leafletProxy("map", data = df_filtered[df_filtered$injured > 0,]) %>%
          addCircleMarkers(~lon, ~lat,
                           radius = 0.5, color = "limegreen",
                           fillOpacity = 0.5,
                           popup = ~paste0("Date: ", date,
                                           "<br>Injured: ", injured,
                                           "<br>Killed: ", killed))
      }
      if ("fatalities" %in% input$data_type) {
        leafletProxy("map", data = df_filtered[df_filtered$killed > 0,]) %>%
          addCircleMarkers(~lon, ~lat,
                           radius = 2, color = "hotpink",
                           fillOpacity = 1,
                           popup = ~paste0("Date: ", date,
                                           "<br>Injured: ", injured,
                                           "<br>Killed: ", killed))
      }
    }
    
    if ("density" %in% input$layer_type) {
      if ("injuries" %in% input$data_type) {
        leafletProxy("map", data = df_filtered[df_filtered$injured > 0,]) %>%
          addHeatmap(lng = ~lon, lat = ~lat, intensity = ~injured,
                     radius = 20, blur = 10,
                     gradient = viridis::viridis(6, option="D"))
      }
      if ("fatalities" %in% input$data_type) {
        leafletProxy("map", data = df_filtered[df_filtered$killed > 0,]) %>%
          addHeatmap(lng = ~lon, lat = ~lat, intensity = ~killed*80,
                     radius = 25, blur = 15,
                     gradient = viridis::viridis(5, option="F"))
      }
    }
  })
  
  # ---- Monthly bar plots ----
  output$monthly_injured <- renderPlot({
    df_bar <- filtered_data() %>%
      group_by(month) %>%
      summarise(Injured = sum(injured, na.rm=TRUE))
    
    ggplot(df_bar, aes(x = factor(month), y = Injured)) +
      geom_col(fill="limegreen") +
      scale_x_discrete(labels = substr(month.abb,1,1)) +  # single-letter labels
      theme_minimal(base_family="Inter") +
      theme(
        text = element_text(color="#FFFFAA"),
        axis.text = element_text(color="#FFFFAA"),
        panel.background = element_rect(fill="#222222", color=NA),
        plot.background = element_rect(fill="#222222", color=NA),
        axis.title = element_blank(),
        axis.ticks = element_blank()
      )
  })
  
  output$monthly_killed <- renderPlot({
    df_bar <- filtered_data() %>%
      group_by(month) %>%
      summarise(Killed = sum(killed, na.rm=TRUE))
    
    ggplot(df_bar, aes(x = factor(month), y = Killed)) +
      geom_col(fill="hotpink") +
      scale_x_discrete(labels = substr(month.abb,1,1)) +  # single-letter labels
      theme_minimal(base_family="Inter") +
      theme(
        text = element_text(color="#FFFFAA"),
        axis.text = element_text(color="#FFFFAA"),
        panel.background = element_rect(fill="#222222", color=NA),
        plot.background = element_rect(fill="#222222", color=NA),
        axis.title = element_blank(),
        axis.ticks = element_blank()
      )
  })
}

shinyApp(ui, server)
