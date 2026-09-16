# ui.R - två flikar: Företagsstatistik (flik 1) och Handel (flik 2)
# foretag_ui och handel_ui är fullständiga fluidPage-objekt som definieras i global.R.

navbarPage(
  title = "Exportarena Dalarna – varuexport",
  id = "huvudflik",
  collapsible = TRUE,

  header = tagList(
    tags$head(
      tags$link(
        rel = "icon",
        type = "image/x-icon",
        href = "favicon.ico"
      ),
      telemetri_ui(telemetry)  # telemetri_ui(NULL) förväntas vara ofarligt (inga head-taggar läggs till)
    )
  ),

  tabPanel("Exportstatistik Företag", foretag_ui),
  tabPanel("Exportstatistik Regional",  handel_ui)
)
