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

      if (!is.null(track_layer) && nrow(track_layer) > 0) {
        current_tracks[[id_prefix]] <- list(
          data = track_layer,
          name = file_name
        )
      }

      # Read track points - for GPX files, we need to extract points from the tracks
      point_layer <- tryCatch({
        # First try reading the GPX with layer specification
        # GPX files typically have layers: tracks, track_points, waypoints, routes
        
        # Check available layers
        available_layers <- st_layers(file_path)
        
        # Try different layer names for track points
        point_layer_names <- c("track_points", "points", "waypoints")
        found_layer <- FALSE
        
        for (layer_name in point_layer_names) {
          if (layer_name %in% available_layers$name) {
            pt_data <- st_read(file_path, layer = layer_name, quiet = TRUE)
            if (!is.null(pt_data) && nrow(pt_data) > 0) {
              point_layer <- pt_data
              found_layer <- TRUE
              break
            }
          }
        }
        
        if (!found_layer && !is.null(track_layer) && nrow(track_layer) > 0) {
          # If no track_points layer found, extract points from the tracks layer
          # GPX tracks are stored as LINESTRING, we need to extract individual points
          track_geom <- track_layer$geometry
          if (!is.null(track_geom)) {
            # Convert LINESTRING to POINTs
            points_list <- lapply(st_geometry(track_geom), function(geom) {
              if (st_geometry_type(geom) == "MULTILINESTRING") {
                coords <- st_coordinates(st_line_merge(geom))
              } else if (st_geometry_type(geom) == "LINESTRING") {
                coords <- st_coordinates(geom)
              } else {
                return(NULL)
              }
              if (!is.null(coords) && nrow(coords) > 0) {
                # coords from st_coordinates is already a matrix with X,Y columns
                st_sf(geometry = st_points(coords), crs = st_crs(track_layer))
              } else {
                NULL
              }
            })
            
            # Filter out NULL entries and combine
            points_list <- points_list[!sapply(points_list, is.null)]
            if (length(points_list) > 0) {
              point_layer <- do.call(rbind, points_list)
              found_layer <- TRUE
            }
          }
        }
        
        if (!found_layer) {
          NULL
        } else {
          point_layer
        }
        
      }, error = function(e) NULL)

      if (!is.null(point_layer) && nrow(point_layer) > 0) {
        # Ensure point_layer is in EPSG:4326
        if (st_crs(point_layer)$epsg != 4326) {
          point_layer <- st_transform(point_layer, 4326)
        }
        
        # Extract time information
        time_col <- intersect(
          c("time", "Time", "timestamp", "Timestamp"),
          names(point_layer)
        )

        if (length(time_col) == 0) {
          point_layer$track_timestamp <- NA_character_
        } else {
          point_layer$track_timestamp <- as.character(point_layer[[time_col[1]]])
        }

        point_layer$gpx_file_name <- file_name
        point_layer$point_id <- paste0(id_prefix, "_", seq_len(nrow(point_layer)))

        current_points[[id_prefix]] <- point_layer
      }
    }

    tracks(current_tracks)
    track_points(current_points)
  })

  # Separate observers to avoid full map redraw on every change
  # 1) Draw / update tracks when `tracks` changes
  observeEvent(tracks(), {
    trks <- tracks()
    colors <- track_colors()
    leafletProxy("map") |> clearGroup("track")
    if (length(trks) > 0) {
      for (i in seq_along(trks)) {
        trk <- trks[[i]]
        track_sf <- trk$data
        # Only transform if not already in EPSG:4326 (GPX files are typically already in 4326)
        if (st_crs(track_sf)$epsg != 4326) {
          track_sf <- st_transform(track_sf, 4326)
        }
        leafletProxy("map") |>
          addPolylines(
            data = track_sf,
            color = colors(i),
            weight = 4,
            opacity = 0.9,
            label = trk$name,
            group = "track"
          )
      }
    }
  })

  # 2) Draw / update points when `track_points` changes
  observeEvent(track_points(), {
    pts <- track_points()
    colors <- track_colors()
    leafletProxy("map") |> clearGroup("point")
    if (length(pts) > 0) {
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
            group = "point",
            popup = ~paste(
              "<b>File:</b>", gpx_file_name,
              "<br><b>Time:</b>", track_timestamp
            )
          )
      }
    }
  })

  observeEvent(input$map_click, {

    click <- input$map_click
    req(click)

    pts <- track_points()
    if (length(pts) == 0) return()

    current <- marked_points()
    track_names <- names(pts)

    if (is.null(current) || nrow(current) == 0) {

      current <- data.frame(idx = integer())

      for (i in seq_along(track_names)) {
        current[[paste0("track_", i)]] <- NA_character_
      }
    }

    click_sf <- st_point(c(click$lng, click$lat)) |> st_sf(crs = 4326)

    for (track_idx in seq_along(pts)) {

      col_name <- paste0("track_", track_idx)
      track_pts <- pts[[track_idx]]
      
      # Only transform if not already in EPSG:4326 (GPX files are typically already in 4326)
      if (st_crs(track_pts)$epsg != 4326) {
        track_pts <- st_transform(track_pts, 4326)
      }

      if (nrow(track_pts) > 0) {

        dists <- st_distance(click_sf, track_pts)
        closest_idx <- which.min(dists)

        closest_point <- track_pts[closest_idx, ]
        timestamp <- closest_point$track_timestamp[1]

        local_idx <- sum(!is.na(current[[col_name]]) & current[[col_name]] != "") + 1

        while (nrow(current) < local_idx) {

          new_row <- data.frame(idx = nrow(current) + 1)

          for (i in seq_along(track_names)) {
            new_row[[paste0("track_", i)]] <- NA_character_
          }

          current <- bind_rows(current, new_row)
        }

        current[local_idx, col_name] <- timestamp
      }
    }

    for (track_idx in seq_along(track_names)) {

      col_name <- paste0("track_", track_idx)
      track_times <- current[[col_name]]
      non_na_times <- track_times[!is.na(track_times) & track_times != ""]
      if (length(non_na_times) > 0) {
        tryCatch({
          sorted_times <- sort(as.POSIXct(non_na_times, tz = "UTC"))
          sorted_times_char <- as.character(sorted_times)
          current[1:length(sorted_times_char), col_name] <- sorted_times_char
          if (nrow(current) > length(sorted_times_char)) {
            current[(length(sorted_times_char)+1):nrow(current), col_name] <- NA_character_
          }
        }, error = function(e) {
          warning("Failed to sort timestamps for track ", track_idx, ": ", e$message)
        })
      }
    }

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