library(shiny)
library(leaflet)
library(DT)
library(plotly)

foretag_ui <- fluidPage(
  shiny::tags$head(
    
    # Om du vill ladda Roboto från Google Fonts kan du avkommentera denna.
    # Om rapporten ska fungera helt utan internet bör du i stället lägga
    # Roboto-filer lokalt i www/ och definiera dem i styles.css.
    #
    # shiny::tags$link(
    #   rel = "stylesheet",
    #   href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&display=swap"
    # ),
    
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    ),
    
    shiny::tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/select2@4.1.0-rc.0/dist/css/select2.min.css"
    ),
    
    shiny::tags$script(
      src = "https://cdn.jsdelivr.net/npm/select2@4.1.0-rc.0/dist/js/select2.min.js"
    ),
    
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

          // 2) If the click hits a geometry AND that geometry (or its parent)
          // is tagged .kommunerPane -> skip
          var hitGeom = t.closest && (
            t.closest('.leaflet-interactive') ||
            t.closest('.leaflet-marker-icon')
          );

          if (hitGeom && t.closest('.kommunerPane')) return;

          // 3) Otherwise trigger reset
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue(
              'resetKommunOutside',
              Date.now(),
              { priority: 'event' }
            );
          }
        }

        // Use capture phase so we see the event before Leaflet stops propagation
        document.addEventListener('pointerdown', onGlobalPointerDown, true);
      })();
    "))
  ),
  
  # 1. Header-bild
  # fluidRow(
  #   column(
  #     width = 12,
  #     shiny::tags$img(
  #       src = "DalarnaExport.png",
  #       style = "
  #         display: block;
  #         max-width: 100%;
  #         height: 200px;
  #         margin-left: auto;
  #         margin-right: auto;
  #       "
  #     )
  #   )
  # ),
  
  # Filterkolumn, kommunkarta, stapeldiagram, export- och importkartor
  fluidRow(
    
    # 2. Filterkolumn
    column(
      width = 2,
      
      fluidRow(
        div(
          class = "sidTabell",
          
          h4("Filter"),
          
          selectInput("kommun", "Kommun", choices = NULL),
          selectInput("anstallda", "Storleksklass", choices = NULL),
          selectInput("juridisk", "Branschgrupp", choices = NULL),
          
          # selectInput("kluster", "Kluster", choices = c("Alla"), selected = "Alla"),
          
          selectInput("exportVolym", "Exportvolym", choices = NULL),
          selectInput("exportRegion", "Exportregion", choices = NULL),
          selectInput("importVolym", "Importvolym", choices = NULL),
          selectInput("importRegion", "Importregion", choices = NULL)
        )
      ),
      
      br(),
      
      fluidRow(
        downloadButton(
          outputId = "downloadFilter",
          label = "Ladda ner filtrerad data till Excel"
        )
      ),
      
      fluidRow(
        downloadButton(
          outputId = "downloadAll",
          label = "Ladda ner all data till Excel"
        )
      )
    ),
    
    # 3. Kommunkarta
    column(
      width = 3,
      
      h4("Kommuner"),
      
      fluidRow(
        leafletOutput("karta_kommun"),
        style = "height: 500px;"
      )
    ),
    
    # 4. Stapeldiagram
    column(
      width = 4,
      
      h4("Branschgrupper"),
      
      plotlyOutput("stapel", height = "500px")
    ),
    
    # 5 och 6. Export- och importkartor
    column(
      width = 2,
      
      fluidRow(
        h4("Export"),
        leafletOutput("exportKarta", height = "180px"),
        style = "height: 225px;"
      ),
      
      fluidRow(
        h4("Import"),
        leafletOutput("importKarta", height = "180px"),
        style = "height: 225px;"
      )
    )
  ),
  
  # 7. Nedre tabell
  fluidRow(
    column(
      width = 10,
      offset = 2,
      
      h4("Företagsstatistik"),
      
      DTOutput("tabell")
    )
  )
)