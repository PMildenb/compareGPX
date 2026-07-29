library(shiny)      # Shiny and UI
library(shinydashboard)
library(leaflet)    # Mapping
library(sf)          # Spatial data
library(dplyr)
library(data.table)  # Efficient data manipulation


ui <- dashboardPage(
  dashboardHeader(title = "compareGPX"),
  
  dashboardSidebar(
    fileInput(
      "gpx_files",
      "Upload GPX File(s)",
      multiple = TRUE,
      accept = c(".gpx")
    ),
    actionButton("clear_map", "Clear Tracks", icon = icon("trash"))
  ),
  
  dashboardBody(
    fluidRow(
      box(width = 12, leafletOutput("map", height = "400px"))
    ),
    fluidRow(
      box(width = 12, title = "Marked Trackpoints", tableOutput("marked_table"))
    )
  )
)

server <- function(input, output, session) {
  source("helper.R")
  
  # Reactive data stores
  tracks       <- reactiveVal(list())
  track_points <- reactiveVal(list())
  marked_points <- reactiveVal(data.table(idx = integer()))
  
  # Color palette for tracks
  track_colors <- reactive({
    trks <- tracks()
    if (length(trks)) colorFactor(rainbow(length(trks)), domain = seq_along(trks))
  })

  # Initialize empty map
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      setView(lng = 8.8, lat = 49.8, zoom = 8)
  })

  # Process uploaded GPX files
  observeEvent(input$gpx_files, {
    req(input$gpx_files)
    current_tracks  <- tracks()
    current_points <- track_points()
    n_files       <- nrow(input$gpx_files)

    for (i in seq_len(n_files)) {
      file_path  <- input$gpx_files$datapath[i]
      file_name  <- input$gpx_files$name[i]
      id_prefix  <- paste0(file_name, "_", as.numeric(Sys.time()), "_", i)

      track_layer <- tryCatch(
        st_read(file_path, layer = "tracks", quiet = TRUE),
        error = function(e) NULL
      )
      point_layer <- tryCatch(
        st_read(file_path, layer = "track_points", quiet = TRUE),
        error = function(e) NULL
      )

      if (!is.null(track_layer) && nrow(track_layer))
        current_tracks[[id_prefix]] <- list(data = track_layer, name = file_name)

      if (!is.null(point_layer) && nrow(point_layer)) {
        point_layer <- st_transform(point_layer, 4326)
        time_col <- intersect(
          c("time", "Time", "timestamp", "Timestamp"), names(point_layer)
        )
        point_layer$track_timestamp <- if (length(time_col))
          as.character(point_layer[[time_col[1]]]) else NA_character_
        point_layer$file_name <- file_name
        point_layer$point_id <- paste0(id_prefix, "_", seq_len(nrow(point_layer)))
        current_points[[id_prefix]] <- point_layer
      }
      cat(point_layer$track_timestamp)
    }
    tracks(current_tracks)
    track_points(current_points)
  })

  # Render tracks and points on map
  observe({
    trks <- tracks()
    pts  <- track_points()
    
    leafletProxy("map") |> clearShapes() |> clearMarkers() |> clearControls()

    if (length(trks)) {
      colors <- track_colors()
      for (i in seq_along(trks)) {
        leafletProxy("map") |>
          addPolylines(
            data  = st_transform(trks[[i]]$data, 4326),
            color = colors(i), weight = 4, opacity = 0.9,
            label = trks[[i]]$name, group = trks[[i]]$name
          )
      }
      for (i in seq_along(pts)) {
        leafletProxy("map") |>
          addCircleMarkers(
            data       = pts[[i]],
            color      = colors(i), radius = 4,
            weight     = 1, fillOpacity = 0.5,
            layerId    = ~point_id
          )
      }
    }
  })

  # Handle marker clicks for trackpoint selection
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    req(click$id)
    pts <- track_points()
    if (!length(pts)) return()

    all_pts <- rbindlist(Filter(NROW, pts), fill = TRUE)
    selected <- all_pts[point_id == click$id]
    if (!nrow(selected)) return()

    track_names <- names(pts)
    track_idx <- which(sapply(
      pts, function(x) !is.null(x$point_id) &&
        selected$point_id[1] %in% x$point_id
    ))
    if (!length(track_idx)) return()

    col_name  <- paste0("track_", track_idx)
    current   <- marked_points()

    if (is.null(current) || !nrow(current)) {
      current <- data.table(idx = integer())
      for (i in seq_along(track_names))
        current[[paste0("track_", i)]] <- character()
    }

    non_empty <- !is.na(current[[col_name]]) & current[[col_name]] != ""
    local_idx <- sum(non_empty) + 1
    
    if (local_idx > nrow(current)) {
      new_idx <- (nrow(current) + 1):local_idx
      new_rows <- data.table(idx = new_idx)
      for (i in seq_along(track_names))
        new_rows[[paste0("track_", i)]] <- NA_character_
      current <- rbindlist(list(current, new_rows), fill = TRUE)
    }
    
    current[local_idx, (col_name) := selected$track_timestamp[1]]
    marked_points(current)
  })

  # Render table of marked points with time differences
output$marked_table <- renderTable({
  input$map_marker_click
  tbl <- data.table::copy(marked_points())
  if (!nrow(tbl))
    return(as.data.frame(tbl))
  track_cols <- grep("^track_", names(tbl), value = TRUE)
  for (col in track_cols) {
    x <- tbl[[col]]
    valid <- !(is.na(x) | x == "")
    if (any(valid)) {
      formatted <- sort_n_format(x[valid])
      tbl[[col]] <- NA_character_
      tbl[[col]][seq_along(formatted)] <- formatted
    }
  }
  as.data.frame(tbl)
})

  # Clear all map data
  observeEvent(input$clear_map, {
    tracks(list())
    track_points(list())
    marked_points(data.table(idx = integer()))
    leafletProxy("map") |> clearShapes() |> clearMarkers() |> clearControls()
  })
}

shinyApp(ui, server)
