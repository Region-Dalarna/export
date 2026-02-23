library(shiny)
library(leaflet)
library(DT)
library(plotly)

ui <- fluidPage(
  shiny::tags$head(
    shiny::tags$style(HTML(
      "#karta_kommun .leaflet-pane.dimPane {pointer-events: none !important;}
      .dt-filter-pop {
        position: absolute; z-index: 9999; min-width: 260px; background: #fff;
        border: 1px solid #ddd; border-radius: 6px; box-shadow: 0 4px 16px rgba(0,0,0,0.12);
        padding: 10px;
      }
      .dt-filter-pop .title { font-size: 12px; font-weight: 600; margin-bottom: 8px; color: #000000; }
      .dt-filter-select { width: 100%; }
      .select2-container { width: 100% !important; }
      .dt-header-text {
        cursor: pointer; text-decoration: underline; text-decoration-style: dotted; text-underline-offset: 2px;
        color: #000000;
      }
      .dt-filter-actions { display: flex; gap: 6px; margin-top: 10px; justify-content: flex-end; }
      #stapel .main-svg,
      #stapel .hoverlayer, #stapel .modebar, #stapel .cursor-crosshair {
      cursor: default !important;}
      ")),
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"),
    shiny::tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/select2@4.1.0-rc.0/dist/css/select2.min.css"),
    shiny::tags$script(src = "https://cdn.jsdelivr.net/npm/select2@4.1.0-rc.0/dist/js/select2.min.js"),
    shiny::tags$script(HTML("
    (function(){
      function onGlobalPointerDown(evt){
        var t = evt.target;

        // 1) Ignore clicks inside UI elements we shouldn't react to
        if (t.closest && (
              // Sidofiltret
              t.closest('.sidTabell') ||

              // Nedladdningsknappar
              t.closest('#downloadFilter') ||
              t.closest('#downloadAll') ||

              // Sammanställningstabell
              t.closest('#tabell') ||
              t.closest('.dataTables_wrapper') ||

              // Selectize controls and dropdowns (Shiny selectInput default)
              t.closest('.selectize-control') ||
              t.closest('.selectize-dropdown') ||

              // Leaflet built-in controls (zoom, layers, attribution)
              t.closest('.leaflet-control')
            )) return;

        // 2) If the click hits a geometry AND that geometry (or its parent) is tagged .no-reset -> skip
        var hitGeom = t.closest && (t.closest('.leaflet-interactive') || t.closest('.leaflet-marker-icon'));
        if (hitGeom && t.closest('.kommunerPane')) return;

        // 3) Otherwise trigger reset (covers:
        //    - clicks on non-tagged geometries
        //    - clicks on blank map areas/tiles
        //    - clicks elsewhere in the app)
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue('resetKommunOutside', Date.now(), {priority:'event'});
        }
      }

      // Use capture phase so we see the event before Leaflet stops propagation
      document.addEventListener('pointerdown', onGlobalPointerDown, true);
    })();
    ")
    )
  ),

  # 1. Header-bild
 # fluidRow(
 #   column(
 #     width = 12,
 #     shiny::tags$img(src = 'DalarnaExport.png', style = "display: block; max-width:100%; height:200px; margin-left: auto; margin-right: auto;") # put header.png in /www
 #   )
 # ),

  # Filterkolumn (2), Kommunkarta (3), Stapeldiagram (4), Världskartor (5,6)
  fluidRow(
    # 2. Filterkolumn
    column(
      width = 2,
      fluidRow(
        div(
          class = "sidTabell",
          style = "padding: 15px; background-color: #f8f8f8; border: 1px solid #ddd;",
          h4("Filter"),
          selectInput("kommun", "Kommun", choices = NULL),
          selectInput("anstallda", "Storleksklass", choices = NULL),
          selectInput("juridisk", "Bolagsform", choices = NULL),
     #    selectInput("kluster", "Kluster", choices = c("Alla"), selected = "Alla"),
          selectInput("exportVolym", "Exportvolym", choices = NULL),
          selectInput("exportRegion", "Exportregion", choices = NULL),
          selectInput("importVolym", "Importvolym", choices = NULL),
          selectInput("importRegion", "Importregion", choices = NULL)
        )
      ),
     br(),
     fluidRow(
     downloadButton('downloadFilter',"Ladda ner filtrerad data till excel")
      ),
     fluidRow(
       downloadButton('downloadAll',"Ladda ner all data till excel")
     )
  ),

    # 3. Kommunkarta
    column(
      width = 3,
      h4("Kommuner"),
      fluidRow(leafletOutput("karta_kommun"),
               style = "height:500px")
    ),

    # 4. Stapeldiagram
    column(
      width = 4,
      h4("Fördelning"),
      plotlyOutput("stapel", height = "500px")
    ),

    # 5 and 6. Export och importkartor
    column(
      width = 2,
      fluidRow(
        h4("Export"),
        leafletOutput("exportKarta"),
        style = "height:225px"
      ),
      fluidRow(
        h4("Import"),
        leafletOutput("importKarta"),
        style = "height:225px"
      )
    )
  ),

  # 7. Nedre tabell
  fluidRow(
    column(
      width = 10, offset = 2,
      h4("Företagsstatistik"),
      DTOutput("tabell")
    )
  )
)
