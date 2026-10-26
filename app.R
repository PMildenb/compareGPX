library(shiny)       # Shiny and UI
library(shinydashboard)
library(leaflet)     # Mapping
library(sf)          # Spatial data
library(data.table)  # Efficient data manipulation
source("helper.R")

ui <- dashboardPage(
  dashboardHeader(title = "compareGPX"),
  
  dashboardSidebar(
    fileInput("gpx_files",
      "Upload GPX File(s)",
      multiple = TRUE,
      accept = ".gpx"
    ),
    actionButton( "clear_map"   , "Clear Tracks"     , icon = icon("trash") ), 
    actionButton( "clear_points", "Clear Trackpoints", icon = icon("trash") )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        #map_wrapper { height: calc(100vh - 160px); min-height: 250px; }
        #map_wrapper .html-widget { height: 100% !important; }
        #drag_handle {
          height: 8px; margin: 4px 0; border-radius: 4px;
          background: #b8c2ce; cursor: row-resize; user-select: none;
        }
      ")),
      tags$script(HTML("
        $(function() {
          var wrapper = document.getElementById('map_wrapper');
          var handle  = document.getElementById('drag_handle');
          var dragging = false, startY = 0, startH = 0;
          function resizeMap() {
            var map = HTMLWidgets.find(document.getElementById('map'));
            if (map) map.invalidateSize();
          }
          $(handle).on('mousedown', function(e) {
            dragging = true;
            startY = e.pageY;
            startH = wrapper.getBoundingClientRect().height;
            e.preventDefault();
          });
          $(document).on('mousemove', function(e) {
            if (!dragging) return;
            wrapper.style.height = Math.max(250, startH + (e.pageY - startY)) + 'px';
            resizeMap();
          });
          $(document).on('mouseup', function() { dragging = false; });
          $(handle).on('dblclick', function() {
            wrapper.style.height = '';
            resizeMap();
          });
          $(window).on('resize', resizeMap);
        });
      "))
    ),
    fluidRow( box(width=12,
      div(id = "map_wrapper", leafletOutput("map", height = "100%")),
      div(id = "drag_handle", title = "Drag to resize the map, double-click to reset")
    )),
    fluidRow( box(width=12, title = "Marked Trackpoints", tableOutput("marked_table")) )
  )
)

server <- function(input, output, session) {
  
  # Reactive data stores
  tracks       <- reactiveVal(list())
  track_points <- reactiveVal(list())
  marked_ids   <- reactiveVal(character())
  redraw_map   <- reactiveVal(0L)  # bump to force a full map redraw
  
  # Color palette for tracks
  track_colors <- reactive({
    trks <- tracks()
    if (length(trks)) colorFactor(rainbow(length(trks)), domain = seq_along(trks))
  })

  # Initialize empty map
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      setView(lng = 8.5, lat = 49.6, zoom = 11)
  })

  # Draw trackpoints as circle markers; Radius according to marking state
  draw_points <- function(data, color, id) {
    radius <- ifelse(id %in% isolate(marked_ids()), 8, 3)
    leafletProxy("map") |>
      addCircleMarkers(
        data        = data,
        color       = color,
        radius      = radius,
        weight      = 1,
        fillOpacity = 0.5,
        layerId     = ~point_id
      )
  }

  # Process uploaded GPX files
  observeEvent(input$gpx_files, {
    req(input$gpx_files)
    current_tracks  <- tracks()
    current_points  <- track_points()

    for (i in 1:nrow(input$gpx_files)) {
      file_path  <- input$gpx_files$datapath[i]
      file_name  <- input$gpx_files$name[i]
      id_prefix  <- paste0(file_name, "_", i)

      track_layer <- read_gpx_layer(file_path, "tracks")
      point_layer <- read_gpx_layer(file_path, "track_points")

      if (!is.null(track_layer) && nrow(track_layer))
        current_tracks[[id_prefix]] <- list(data = track_layer, name = file_name)

      if (!is.null(point_layer) && nrow(point_layer)) {
        point_layer$track_timestamp <- as.character(point_layer$time)
        point_layer$point_id        <- paste0(id_prefix, "_", 1:nrow(point_layer) )
        current_points[[id_prefix]] <- point_layer
      }
    }
    tracks(current_tracks)
    track_points(current_points)
  })

  # Render tracks and points on map ; full redraw only on data changes or redraw_map()
  observe({
    redraw_map()
    trks <- tracks()
    pts  <- track_points()

    leafletProxy("map") |> clearShapes() |> clearMarkers()

    if (length(trks)) {
      colors <- track_colors()
      for (i in seq_along(trks)) {
        leafletProxy("map") |>
          addPolylines(
            data  = trks[[i]]$data,
            color = colors(i), weight = 4, opacity = 0.9,
            label = trks[[i]]$name, group = trks[[i]]$name
          )
      }
      for (i in seq_along(pts)) {
        draw_points(pts[[i]], colors(i), pts[[i]]$point_id)
      }
    }
  })

  # Toggle marking of the clicked trackpoint; update only that marker
  observeEvent(input$map_marker_click, {
    id <- input$map_marker_click$id
    req(id)

    pts <- track_points()
    track_idx <- which(sapply(pts, function(x) id %in% x$point_id))
    req(track_idx)
    track_idx <- track_idx[1]

    ids <- marked_ids()
    marked_ids(if (id %in% ids) setdiff(ids, id) else c(ids, id))

    pt_layer <- pts[[track_idx]]
    draw_points(pt_layer[pt_layer$point_id == id, ], track_colors()(track_idx), id)
  })

  # Toggle the closest trackpoint of every track at a clicked map location.
  # Group semantics: if all nearest points are already marked, unmark them;
  # otherwise mark them all. Tracks whose nearest point is more than 50 m
  # from the click are ignored; if no track is within 50 m, nothing happens.
  ## TODO: Clean up. Toggle doesnt really work
  toggle_nearest_at <- function(lat, lng) {
    pts <- track_points()
    colors <- track_colors()
    if (!length(pts) || is.null(colors)) return(invisible())

    click_pt <- st_sfc(st_point(c(lng, lat)), crs = 4326)
    nearest <- lapply(pts, function(p) p[st_nearest_feature(click_pt, p), ])
    dists <- vapply(nearest, function(p) as.numeric(st_distance(click_pt, p))[1], numeric(1))
    keep <- dists <= 50
    if (!any(keep)) return(invisible())

    nearest_ids <- vapply(nearest, function(p) p$point_id[1], character(1))[keep]

    ids <- marked_ids()
    all_marked <- all(nearest_ids %in% ids)
    marked_ids(if (all_marked) setdiff(ids, nearest_ids)
               else unique(c(ids, nearest_ids)))

    for (i in which(keep)) draw_points(nearest[[i]], colors(i), nearest[[i]]$point_id)
    invisible()
  }

  # Click on empty map area
  observeEvent(input$map_click, {
    toggle_nearest_at(input$map_click$lat, input$map_click$lng)
  })

  # Click on a track polyline
  ## TODO: This works only sometimes (?)
  observeEvent(input$map_shape_click, {
    toggle_nearest_at(input$map_shape_click$lat, input$map_shape_click$lng)
  })

  # Render table of marked points with time differences
  output$marked_table <- renderTable({
    ids <- marked_ids()
    pts <- track_points()
    if (!length(ids) || !length(pts)) return(data.frame(idx = integer()))

    track_cols <- lapply(pts, function(p) {
      sort_n_format(p$track_timestamp[p$point_id %in% ids])
    })
    n          <- max(lengths(track_cols))
    track_cols <- lapply(track_cols, `length<-`, n)  # pad with NA to common length
    tbl        <- data.table(idx = seq_len(n))
    for (i in seq_along(track_cols)) tbl[[paste0("track_", i)]] <- track_cols[[i]]
    as.data.frame(tbl)
  })

  # Clear all map data
  observeEvent(input$clear_map, {
    tracks(list())
    track_points(list())
    marked_ids(character())
  })

  # Clear marked points only
  observeEvent(input$clear_points, {
    marked_ids(character())
    redraw_map(redraw_map() + 1L)
  })
}

shinyApp(ui, server)

