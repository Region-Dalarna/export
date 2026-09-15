# ui.R - två flikar: Företagsstatistik (flik 1) och Handel (flik 2)
# foretag_ui och handel_ui är fullständiga fluidPage-objekt som definieras i global.R.

navbarPage(
  title = "Exportarena Dalarna",
  id = "huvudflik",
  collapsible = TRUE,

  header = tagList(
    tags$head(
      tags$link(
        rel = "icon",
        type = "image/x-icon",
        href = "favicon.ico"
      )
    ),
    shiny.telemetry::use_telemetry()
  ),

  tabPanel("Företagsstatistik", foretag_ui),
  tabPanel("Export",            handel_ui)
)
