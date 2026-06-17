library(shiny)
library(leaflet)
library(DT)
library(htmltools)
library(plotly)

handel_ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        /* Typsnitt - lagg motsvarande .otf/.ttf i www/ */
        @font-face {
          font-family: 'Fieldwork Geo Demibold';
          src: url('FieldworkGeo-Bold-Fixed.otf') format('opentype');
          font-weight: 700; font-style: normal; font-display: swap;
        }
        @font-face {
          font-family: 'Roboto';
          src: url('Roboto-Regular.ttf') format('truetype');
          font-weight: 400; font-style: normal; font-display: swap;
        }
        @font-face {
          font-family: 'Roboto';
          src: url('Roboto-Medium.ttf') format('truetype');
          font-weight: 500; font-style: normal; font-display: swap;
        }
        @font-face {
          font-family: 'Roboto';
          src: url('Roboto-Bold.ttf') format('truetype');
          font-weight: 700; font-style: normal; font-display: swap;
        }

        body { font-family: 'Roboto', Arial, sans-serif; color: #00374e; }

        /* Brandpalett: bla #00374e, beige #f3e8d9, gron #00a064 */
        #lanText { font-family:'Fieldwork Geo Demibold',Arial,sans-serif; font-size:42px; font-weight:700; color:#00374e; }
        #importExportText { font-family:'Fieldwork Geo Demibold',Arial,sans-serif; font-size: clamp(18px, 2vw, 20px); font-weight:700; color:#00374e; }
        #sumExport { font-family:'Roboto',Arial,sans-serif; font-size: clamp(22px, 2.2vw, 28px); font-weight:700; color:#00a064; }
        #sumImport { font-family:'Roboto',Arial,sans-serif; font-size: clamp(22px, 2.2vw, 28px); font-weight:700; color:#00374e; }
        #exportPercText { font-family:'Roboto',Arial,sans-serif; font-size: clamp(22px, 2.2vw, 28px); font-weight:700; color:#00a064; }
        #netBalansText { font-family:'Roboto',Arial,sans-serif; font-size: clamp(22px, 2.2vw, 28px); font-weight:700; color:#00374e; }
        #exportText { font-family:'Fieldwork Geo Demibold',Arial,sans-serif; font-size: clamp(16px, 1.6vw, 18px); font-weight:700; color:#00374e; }
        #importText { font-family:'Fieldwork Geo Demibold',Arial,sans-serif; font-size: clamp(16px, 1.6vw, 18px); font-weight:700; color:#00374e; }
        #brodtext1, #brodtext2, #brodtext3, #brodtext4 { font-family:'Roboto',Arial,sans-serif; font-size: clamp(13px, 1.5vw, 15px); color:#6c6c6c; }

        /* OBS: ingen height-regel pa .leaflet-container - hojden styrs av leafletOutput(height=...) */
        .leaflet-container { background: transparent !important; }
        .leaflet-control-attribution { display: none !important; }
        .js-plotly-plot .plotly .cursor-crosshair { cursor: default !important; }
           "))
  ),
  fluidRow(
    column(
      width = 4,
      fluidRow(
        column(12,  textOutput("lanText"), style = "margin-bottom: 4px;")
      ),
      fluidRow(
        column(12, textOutput("importExportText"), style = "margin-bottom: 8px;")
      ),
      br(),
      fluidRow(
        column(6, textOutput("sumExport"), align = "center"),
        column(6, textOutput("sumImport"), align = "center")
      ),
      fluidRow(
        column(6, textOutput("brodtext1"), align = "center"),
        column(6, textOutput("brodtext2"), align = "center")
      ),
      br(),
      fluidRow(
        column(6, textOutput("exportPercText"), align = "center"),
        column(6, textOutput("netBalansText"), align = "center")
        ),
      fluidRow(
        column(6, textOutput("brodtext3"), align = "center"),
        column(6, textOutput("brodtext4"), align = "center")
      ),
      fluidRow(
        leafletOutput("lansKarta", height = "800px")
      )
    ),
    column(
      width = 4,
      br(),
      fluidRow(
        column(
          width = 12,
          plotlyOutput("year_graph", height = "300px")
        )
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          plotlyOutput("bar_graph", height = "700px")
        )
      )
    ),
    column(
      width = 4,
      fluidRow(
        br(),
        column(
          width = 12,
          textOutput("importText")
        )
      ),
      fluidRow(
        column(
          width = 12,
          plotlyOutput("importStapel", height = "350px")
        )
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          textOutput("exportText")
        )
      ),
      fluidRow(
        column(
          width = 12,
          plotlyOutput("exportStapel", height = "350px")
        )
      )
    )
  )
)