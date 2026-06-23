library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "compareGPX"),

  dashboardSidebar(
    fileInput(
      "gpx_files",
      "Upload GPX File(s)",
      multiple = TRUE,
      accept = c(".gpx")
    ),

    actionButton("clear_map",
                 "Clear Tracks",
                 icon = icon("trash")
    )
  ),

  dashboardBody(
    fluidRow(
      box(width = 12, leafletOutput("map", height = "400px") )
    ),
    fluidRow(
      box(width = 12, title = "Marked Trackpoints",
          tableOutput("marked_table")
      )
    )
  )
)

server <- function(input, output, session) {

  tracks <- reactiveVal(list())
  track_points <- reactiveVal(list())

  marked_points <- reactiveVal(
    data.frame(
      timestamp = character(),
      file = character(),
      stringsAsFactors = FALSE
    )
  )

  # Reactive color palette for tracks – recomputed only when the track list changes
  track_colors <- reactive({
    trks <- tracks()
    if (length(trks) == 0) {
      NULL
    } else {
      colorFactor(rainbow(length(trks)), domain = seq_along(trks))
    }
  })

  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      setView(
        lng = 10.4515,
        lat = 51.1657,
        zoom = 6
      )
  })

  observeEvent(input$gpx_files, {

    req(input$gpx_files)

    current_tracks <- tracks()
    current_points <- track_points()

    for (i in seq_len(nrow(input$gpx_files))) {

      file_path <- input$gpx_files$datapath[i]
      file_name <- input$gpx_files$name[i]
      id_prefix <- paste0(file_name, "_", as.numeric(Sys.time()), "_", i)

      track_layer <- tryCatch(
        st_read(file_path, layer = "tracks", quiet = TRUE),
        error = function(e) NULL
      )

      point_layer <- tryCatch(
        st_read(file_path, layer = "track_points", quiet = TRUE),
        error = function(e) NULL
      )

      if (!is.null(track_layer) && nrow(track_layer) > 0) {
        current_tracks[[id_prefix]] <- list(
          data = track_layer,
          name = file_name
        )
      }

      if (!is.null(point_layer) && nrow(point_layer) > 0) {

        point_layer <- st_transform(point_layer, 4326)

        time_col <- intersect(
          c("time", "Time", "timestamp", "Timestamp"),
          names(point_layer)
        )

        if (length(time_col) == 0) {
          point_layer$track_timestamp <- NA_character_
        } else {
          point_layer$track_timestamp <- as.character(point_layer[[time_col[1]]])
        }

        point_layer$file_name <- file_name
        point_layer$point_id <- paste0(id_prefix, "_", seq_len(nrow(point_layer)))

        current_points[[id_prefix]] <- point_layer
      }
    }

    tracks(current_tracks)
    track_points(current_points)
  })

  observe({

    trks <- tracks()
    pts <- track_points()

    leafletProxy("map") |>
      clearShapes() |>
      clearMarkers() |>
      clearControls()

    if (length(trks) > 0) {

      colors <- track_colors()

      for (i in seq_along(trks)) {

        trk <- trks[[i]]
        track_sf <- st_transform(trk$data, 4326)

        leafletProxy("map") |>
          addPolylines(
            data = track_sf,
            color = colors(i),
            weight = 4,
            opacity = 0.9,
            label = trk$name,
            group = trk$name
          )
      }

      for (i in seq_along(pts)) {

        pt <- pts[[i]]
        
        leafletProxy("map") |>
          addCircleMarkers(
            data = pt,
            color = colors(i),
            radius = 4,
            weight = 1,
            fillOpacity = 0.5,
            layerId = ~point_id,
            popup = ~paste(
              "<b>File:</b>", file_name,
              "<br><b>Time:</b>", track_timestamp
            )
          )
      } 
    }
  })

  observeEvent(input$map_marker_click, {

    click <- input$map_marker_click
    req(click$id)

    pts <- track_points()
    if (length(pts) == 0) return()

    all_pts <- do.call(rbind, pts)

    selected <- all_pts |>
      filter(point_id == click$id)

    if (nrow(selected) == 0) return()

    current     <- marked_points()
    track_names <- names(pts)

    track_idx <- which(vapply(
      pts,
      function(x) selected$point_id[1] %in% x$point_id,
      logical(1)
    ))

    if (length(track_idx) == 0) return()

    col_name <- paste0("track_", track_idx)
    current  <- marked_points()

    if (is.null(current) || nrow(current) == 0) {

      current <- data.frame(idx = integer())

      for (i in seq_along(track_names)) {
        current[[paste0("track_", i)]] <- character()
      }
    }

    local_idx <- sum(!is.na(current[[col_name]]) & current[[col_name]] != "") + 1

    while (nrow(current) < local_idx) {

      new_row <- data.frame(idx = nrow(current) + 1)

      for (i in seq_along(track_names)) {
        new_row[[paste0("track_", i)]] <- NA_character_
      }

      current <- bind_rows(current, new_row)
    }

    current[local_idx, col_name] <- selected$track_timestamp[1]

    marked_points(current)
  })

output$marked_table <- renderTable({

  pts <- track_points()
  n_tracks <- length(pts)

  tbl <- marked_points()

  if (is.null(tbl) || nrow(tbl) == 0) {
    tbl <- data.frame(idx = integer())
    for (i in seq_len(n_tracks)) {
      tbl[[paste0("track_", i)]] <- character()
    }
  }

  # Compute time differences for rows > 1 per track column
  if (nrow(tbl) > 1) {
    for (i in seq_len(n_tracks)) {
      col_name <- paste0("track_", i)
      times <- tbl[[col_name]]
      # Preserve empty strings as NA for conversion
      posix <- as.POSIXct(times, tz = "UTC")
      diffs <- c(NA, difftime(posix[-1], posix[-length(posix)], units = "secs"))
      # Format diffs as HH:MM:SS, keep first row original timestamp
      diff_formatted <- sapply(seq_along(diffs), function(j) {
        if (j == 1) {
          return(times[1])
        }
        d <- diffs[j]
        if (is.na(d) || is.na(times[j]) || times[j] == "") {
          return("")
        }
        secs <- as.integer(d)
        sprintf("%02d:%02d:%02d", secs %/% 3600, (secs %% 3600) %/% 60, secs %% 60)
      })
      tbl[[col_name]] <- diff_formatted
    }
  }

  tbl
})

  observeEvent(input$clear_map, {

    tracks(list())
    track_points(list())

    marked_points(
      data.frame(
        timestamp = character(),
        file = character(),
        stringsAsFactors = FALSE
      )
    )

    leafletProxy("map") |>
      clearShapes() |>
      clearMarkers() |>
      clearControls()
  })
}

shinyApp(ui, server)