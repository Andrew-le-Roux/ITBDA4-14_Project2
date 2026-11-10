library(shiny); library(DBI); library(RPostgres); library(leaflet)

db <- dbConnect(Postgres(), dbname = "postgres", host = "localhost", port = 5432, user = "postgres",
                password = if (rstudioapi::isAvailable()) rstudioapi::askForPassword("PostgreSQL password")
                else Sys.getenv("PGPASSWORD"))
onStop(function() dbDisconnect(db))

places     <- dbGetQuery(db, "SELECT place, latitude, longitude FROM places ORDER BY place")
categories <- dbGetQuery(db, "SELECT DISTINCT category FROM events ORDER BY category")$category

events_sql <- "
  SELECT e.*, distance_km(p.latitude, p.longitude, e.latitude, e.longitude) AS distance_km
  FROM events e
  JOIN places p ON p.place = $1
  WHERE ($2::numeric = 0 OR distance_km(p.latitude, p.longitude, e.latitude, e.longitude) <= $2::numeric)
    AND ($3 = 'All' OR e.category = $3)
    AND (NOT $4::boolean OR COALESCE(e.end_date, e.start_date) >= CURRENT_DATE OR e.event_type = 'Recurring')
  ORDER BY e.start_date, distance_km"

price_label <- function(d) ifelse(d$is_free, "Free",
                                  ifelse(d$price_imputed, paste0("approx. R", d$price_rand), paste0("R", d$price_rand)))


ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background: #f6f7f9; }
    .card { background: #fff; border-radius: 8px; padding: 12px 14px; margin-bottom: 10px;
            box-shadow: 0 1px 3px rgba(0,0,0,.08); }
    .card h4 { margin: 0 0 4px; font-size: 16px; }
    .meta { color: #555; font-size: 13px; margin-bottom: 6px; }
    .tag  { display: inline-block; background: #e8eef7; color: #1f3b57; border-radius: 10px;
            padding: 1px 8px; font-size: 12px; margin-right: 4px; }
    .free { background: #e3f4e8; color: #1d6b3a; }"))),
  titlePanel("What's on near me? - Western Cape events and activities"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput("place", "My location", places$place, selected = "Cape Town CBD"),
                 sliderInput("radius", "Radius (km) - 0 = everything", min = 0, max = 100, value = 15, step = 5),
                 selectInput("category", "Category", c("All", categories)),
                 checkboxInput("upcoming", "Upcoming only", TRUE),
                 hr(),
                 textOutput("count"),
                 tableOutput("by_category")
    ),
    mainPanel(width = 9,
              leafletOutput("map", height = 380),
              br(),
              uiOutput("cards")
    )
  )
)


server <- function(input, output, session) {
  
  events <- reactive({
    dbGetQuery(db, events_sql, params = list(input$place, input$radius, input$category, input$upcoming))
  })
  me <- reactive(places[places$place == input$place, ])
  
  output$count <- renderText({
    d <- events()
    paste0(nrow(d), " activities found, ", sum(d$is_free), " of them free.")
  })
  
  output$by_category <- renderTable({
    d <- events()
    if (nrow(d) == 0) return(NULL)
    t <- as.data.frame(table(Category = d$category))
    t[order(-t$Freq), ]
  }, colnames = FALSE)
  
  output$map <- renderLeaflet({
    d <- events()
    m <- leaflet() %>% addTiles() %>%
      setView(me()$longitude, me()$latitude, zoom = if (input$radius == 0) 9 else 12) %>%
      addCircleMarkers(lng = me()$longitude, lat = me()$latitude, radius = 8, color = "#e07a2e",
                       fillOpacity = 1, label = paste("You:", input$place))
    if (input$radius > 0)
      m <- addCircles(m, me()$longitude, me()$latitude, radius = input$radius * 1000,
                      color = "#e07a2e", fill = FALSE, weight = 1)
    if (nrow(d) > 0)
      m <- addCircleMarkers(m, lng = d$longitude, lat = d$latitude, radius = 7, color = "#2f5d86", fillOpacity = 0.8,
                            popup = paste0("<b>", htmltools::htmlEscape(d$event_name), "</b><br>", d$start_date, " | ",
                                           price_label(d), "<br><a href='", d$url, "' target='_blank'>More info</a>"),
                            clusterOptions = markerClusterOptions())
    m
  })
  
  output$cards <- renderUI({
    d <- head(events(), 100)
    if (nrow(d) == 0) return(p("No activities match these filters - try a bigger radius."))
    lapply(seq_len(nrow(d)), function(i) {
      e <- d[i, ]
      div(class = "card",
          h4(a(e$event_name, href = e$url, target = "_blank")),
          div(class = "meta",
              paste0(format(e$start_date, "%a %d %b %Y"),
                     if (!is.na(e$start_time)) paste0(" | ", substr(e$start_time, 1, 5)) else "",
                     " | ", e$place, " (", e$distance_km, " km)")),
          span(class = "tag", e$category),
          span(class = if (e$is_free) "tag free" else "tag", price_label(e)),
          span(class = "tag", e$event_type),
          p(style = "margin:8px 0 0", ifelse(is.na(e$summary), "(no description)", e$summary)))
    })
  })
}

shinyApp(ui, server)