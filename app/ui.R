# ui.R - två flikar: Företagsstatistik (flik 1) och Handel (flik 2)
# foretag_ui och handel_ui är fullständiga fluidPage-objekt som definieras i global.R.

navbarPage(
  title = "Exportarena Dalarna",
  id = "huvudflik",
  collapsible = TRUE,
  tabPanel("Företagsstatistik", foretag_ui),
  tabPanel("Handel",            handel_ui)
)