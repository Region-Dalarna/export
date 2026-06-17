library(shiny)
library(tidyverse)
library(readxl)
library(leaflet)
library(sf)
library(DT)
library(shinyWidgets)
library(scales)  # for comma()
library(plotly)
library(here)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R", encoding = "utf-8", echo = FALSE)

# Inputdata
#source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
#source("C:/Users/SE1C3T/Documents/Uppdrag/Dalarna/Arbetsmapp/main/script/api_r.r", encoding = "utf-8", echo = FALSE)
#source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

handel_projektmapp <- "."

handel_datafil <- list.files(path = file.path(handel_projektmapp, "data"), pattern = "*.xlsx", full.names = TRUE)
# Exkludera mappningsfilen som flik 1 använder
handel_datafil <- handel_datafil[!grepl("sni_branschgrupp", basename(handel_datafil))]

system.time({
  handel_con_geo <- shiny_uppkoppling_las("geodata")
  # Hämtar länsgeometrin direkt från karta.lan_scb
  lan_db <- st_read(handel_con_geo, query = "SELECT * FROM karta.lan_scb")
  DBI::dbDisconnect(handel_con_geo)
})

if (length(handel_datafil) == 0) {
  stop("❌ Ingen Excel-fil hittades i mappen: ", file.path(handel_projektmapp, "data"))
}

to_num <- function(x) {
  if (is.numeric(x)) return(x)
  if (inherits(x, "Date")) return(as.numeric(x))  # Excel dates become numeric days
  readr::parse_number(as.character(x))
}

tryCatch({
  handel_df <- readxl::read_xlsx(handel_datafil) %>%
    mutate(across(1:2, as.character)) %>%
    # Convert all other columns to numeric, parsing any embedded text
    rename(ImportVolym = Import,
           ExportVolym = Export,
           MetallExport = gruppExport1,
           TjansterExport = gruppExport2,
           BearbetatExport = gruppExport3,
           FordonExport = gruppExport4,
           LivsmedelExport = gruppExport5,
           MetallImport = gruppImport1,
           TjansterImport = gruppImport2,
           BearbetatImport = gruppImport3,
           FordonImport = gruppImport4,
           LivsmedelImport = gruppImport5,
           NordAmExport = NordamerikaExport,
           NordAmImport = NordamreikaImport,
           lanskod = Lankod
    )
})

# Längeometrier - lan_scb har redan en rad per län.
# Kodkolumnen heter "lnkod" i tabellen; döps om till "lanskod" för join mot handelsdatan.
lan_sf <- lan_db %>%
  dplyr::rename(lanskod = lnkod) %>%
  st_transform(4326)

#Filterar för senaste året
maxYear <- max(handel_df$Ar)
tradeData <- handel_df %>%
  dplyr::filter(Ar == maxYear) %>%
  mutate(NettoHandel = ExportVolym - ImportVolym)

handel_server <- function(input, output, session) {
  
  rv <- reactiveValues(sel_lan = NULL,
                       sel_lanskod = NULL,
                       data_filt = tradeData)
  
  mergedData <- reactive({
    lan_sf %>%
      left_join(
        tradeData, 
        by = "lanskod"
      ) %>%
      mutate(NettoHandel = replace_na(NettoHandel, 0))
  })
  
  # Helper for nice number formatting (svensk: mellanslag som tusental, komma som decimal)
  fmt_mdkr <- function(x) format(round(x, 1), big.mark = " ", decimal.mark = ",", nsmall = 1, trim = TRUE)
  
  # Filtered dataset based on selected län ("Alla" = unfiltered)
  filteredData <- reactive({
    req(tradeData)
    if (is.null(rv$sel_lan) || rv$sel_lan == "Alla") {
      tradeData
    } else {
      tradeData %>% filter(Lan == rv$sel_lan)
    }
  })
  
  output$lansKarta <- renderLeaflet({
    md <- mergedData()
    
    # Divergerande skala centrerad pa noll: bla (underskott) -> beige -> gron (overskott)
    dom <- max(abs(md$NettoHandel), na.rm = TRUE)
    if (!is.finite(dom) || dom == 0) dom <- 1
    pal_lan <- colorNumeric(c("#00374e", "#f3e8d9", "#00a064"), domain = c(-dom, dom))
    
    leaflet(md, options = leafletOptions(
      zoomControl = FALSE,
      dragging = FALSE,
      scrollWheelZoom = FALSE,
      doubleClickZoom = FALSE,
      touchZoom = FALSE
    )) %>%
      addMapPane("lanPane", zIndex = 405) %>%
      addMapPane("dimPane", zIndex =410) %>%
      addMapPane("selectedPane", zIndex = 415) %>%
      
      addPolygons(
        data = md,
        fillColor = ~pal_lan(NettoHandel),
        fillOpacity = 0.7,
        color = "white",
        weight = 0.6,
        label = ~paste0("Län: ", Lan, 
                        "
                        
                        Nettohandel: ", fmt_mdkr(NettoHandel), " mdkr"),
        layerId = ~lanskod,
        group = "lan",
        options = pathOptions(pane = "lanPane")
      )
  })
  
  observeEvent(input$lansKarta_shape_click, {
    clicked_id <- input$lansKarta_shape_click$id
    if (is.null(clicked_id) || length(clicked_id) == 0) return()
    md <- mergedData()
    lan_namn <- md %>% dplyr::filter(lanskod == clicked_id) %>% dplyr::pull(Lan) %>% dplyr::first()
    
    if (!is.null(rv$sel_lanskod) && identical(rv$sel_lanskod, clicked_id)) {
      # Klick på redan valt län -> avvälj
      rv$sel_lan <- NULL
      rv$sel_lanskod <- NULL
    } else {
      rv$sel_lan <- lan_namn
      rv$sel_lanskod <- clicked_id
    }
  })
  
  # Markeringsöverlägg: ritar en tydlig ram runt valt län (utan att ladda om kartan)
  observe({
    proxy <- leafletProxy("lansKarta")
    proxy %>% clearGroup("valt_lan")
    
    if (!is.null(rv$sel_lanskod)) {
      sel <- mergedData() %>% dplyr::filter(lanskod == rv$sel_lanskod)
      if (nrow(sel) > 0) {
        proxy %>% addPolygons(
          data = sel,
          fillColor = "#00374e",
          fillOpacity = 0.12,
          color = "#00374e",
          weight = 4,
          opacity = 1,
          group = "valt_lan",
          options = pathOptions(pane = "selectedPane", interactive = FALSE)
        )
      }
    }
  })
  
  observeEvent(input$reset_sel, {
    rv$sel_lan <- NULL
    rv$sel_lanskod <- NULL
  })
  
  output$lanText <- renderText({
    if (is.null(rv$sel_lan))  "Hela Sverige" else rv$sel_lan
  })
  
  # Statiska texter
  output$importExportText <- renderText({ "Import och export"})
  output$brodtext1 <- renderText({"Export (mdkr)"})
  output$brodtext2 <- renderText({"Import (mdkr)"})
  output$brodtext3 <- renderText({"av Sveriges export"})
  output$brodtext4 <- renderText({"Handelsbalans (mdkr)"})
  
  output$sumExport <- renderText({
    fmt_mdkr(sum(filteredData()$ExportVolym, na.rm = TRUE))
  })
  
  output$sumImport <- renderText({
    fmt_mdkr(sum(filteredData()$ImportVolym, na.rm = TRUE))
  })
  
  output$exportPercText <- renderText({
    df_sel <- sum(filteredData()$ExportVolym, na.rm = TRUE)
    df_all <- sum(tradeData$ExportVolym, na.rm = TRUE)
    
    if (isTRUE(df_all > 0)) {
      if (is.null(rv$sel_lan)) "100.0%" else paste0(scales::number(100 * df_sel / df_all, accuracy = 0.1), "%")
    } else {
      "0.0%"
    }
  })
  
  output$netBalansText <- renderText({
    fmt_mdkr(sum(filteredData()$ExportVolym - filteredData()$ImportVolym, na.rm = TRUE))
  })
  
  ts_data <- reactive({
    req(handel_df)
    
    base <- if (is.null(rv$sel_lan) || rv$sel_lan == "Alla") {
      handel_df
    } else {
      handel_df %>% dplyr::filter(Lan == rv$sel_lan)
    }
    
    base %>%
      dplyr::group_by(Ar) %>%
      dplyr::summarise(
        ExportVolym = sum(ExportVolym, na.rm = TRUE),
        ImportVolym = sum(ImportVolym, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(Ar)
  })
  
  
  ## Linjegraf av utveckling över tid
  output$year_graph <- renderPlotly({
    df <- ts_data() %>%
      tidyr::pivot_longer(
        cols = c(ExportVolym, ImportVolym),
        names_to = "Typ",
        values_to = "Volym"
      ) %>%
      dplyr::mutate(
        Typ = dplyr::recode(Typ, ExportVolym = "Export", ImportVolym = "Import"),
        Typ = factor(Typ, levels = c("Export", "Import"))
      )
    
    req(nrow(df) > 0)
    
    p <- ggplot(df, aes(x = Ar, y = Volym, color = Typ)) + 
      geom_line(linewidth = 0.7) +
      scale_color_manual(
        values = c(Export = "#00a064", Import = "#00374e"),
        labels = c(ExportVolym = "Export", ImportVolym = "Import")
      ) + 
      scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
      scale_x_continuous(breaks = c(2000, 2010, 2020),
                         labels = c(2000, 2010, 2020)
      ) +
      labs(
        x = NULL,
        y = "Volym (mdkr)",
        color = NULL,
        title = if (is.null(rv$sel_lan) || rv$sel_lan == "Alla") {
          "Utveckling över tid - Hela Sverige"
        } else {
          paste0("Utveckling över tid - ", rv$sel_lan)
        }
      ) + 
      theme_minimal(base_size=12) + 
      theme(
        text = element_text(family = "Roboto"),
        axis.title.y = element_text(
          face = "bold", 
          family = "Fieldwork Geo Demibold",
          size = 14, 
          color = "#00374e"
        ),
        plot.title = element_text(
          face = "bold", 
          family = "Fieldwork Geo Demibold",
          size = 16, 
          color = "#00374e", 
          hjust = 0, 
          margin = margin(b = 6)
        ), 
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(
          color = "#ABABAB",
          linewidth = 0.5,
          linetype = "dotted"
        )
      )
    gp <- plotly::ggplotly(p, tooltip = NULL) %>%
      layout(
        dragmode = FALSE,
        hovermode = "x unified",
        xaxis = list(
          showspikes = TRUE, spikemode = "across", spikesnap = "data",
          hoverformat = ".0f"
        )
      ) %>%
      plotly::config(displayModeBar = FALSE, 
                     displaylogo = FALSE,
                     scrollZoom = FALSE,
                     doubleClick = FALSE,
                     editable = FALSE,
                     showAxisDragHandles = FALSE)
    
    # Custom hover labels per trace: "Import:" and "Export:" with Swedish thousands separator
    # Pre-compute formatted values for hover
    for (i in seq_along(gp$x$data)) {
      nm <- gp$x$data[[i]]$name
      vals <- gp$x$data[[i]]$y
      formatted <- format(vals, big.mark = " ", scientific = FALSE, trim = TRUE)
      gp$x$data[[i]]$customdata <- formatted
      label <- if (grepl("Import", nm)) "Import" else "Export"
      gp$x$data[[i]]$hovertemplate <- paste0(label, ": %{customdata}<extra></extra>")
    }
    
    gp
  })
  
  katData <- reactive({
    base <- if (is.null(rv$sel_lan) || rv$sel_lan == "Alla") {
      handel_df
    } else {
      handel_df %>% dplyr::filter(Lan == rv$sel_lan)
    }
    
    df_year <- base %>% dplyr::filter(Ar == maxYear)
    
    vals <- df_year %>% dplyr::summarise(
      MetallExport     = sum(MetallExport,     na.rm = TRUE),
      TjansterExport   = sum(TjansterExport,   na.rm = TRUE),
      BearbetatExport  = sum(BearbetatExport,  na.rm = TRUE),
      FordonExport     = sum(FordonExport,     na.rm = TRUE),
      LivsmedelExport  = sum(LivsmedelExport,  na.rm = TRUE),
      MetallImport     = sum(MetallImport,     na.rm = TRUE),
      TjansterImport   = sum(TjansterImport,   na.rm = TRUE),
      BearbetatImport  = sum(BearbetatImport,  na.rm = TRUE),
      FordonImport     = sum(FordonImport,     na.rm = TRUE),
      LivsmedelImport  = sum(LivsmedelImport,  na.rm = TRUE)
    )
    
    tibble::tibble(
      Kategori = c("Metall", "Tjänster", "Bearbetat", "Fordon", "Livsmedel"),
      Export   = c(vals$MetallExport, vals$TjansterExport, vals$BearbetatExport, vals$FordonExport, vals$LivsmedelExport),
      Import   = c(vals$MetallImport, vals$TjansterImport, vals$BearbetatImport, vals$FordonImport, vals$LivsmedelImport)
    ) %>%
      tidyr::pivot_longer(cols = c(Export, Import), names_to = "Typ", values_to = "Volym")
  })
  
  
  # Stapeldiagram
  output$bar_graph <- renderPlotly({
    df <- katData()
    req(nrow(df) > 0)
    
    pl <- ggplot(df, aes(y = Kategori, x = Volym, fill = Typ)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) + 
      scale_fill_manual(
        values = c(Export = "#00a064", Import = "#00374e"),
        labels = c(Export = "Export", Import = "Import")
      ) +
      scale_x_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
      labs(
        x = NULL, y = NULL,
        title = paste0(
          "Produktgrupper ", maxYear, " - ",
          ifelse(is.null(rv$sel_lan) || rv$sel_lan == "Alla", "Hela Sverige", rv$sel_lan)
        )
      ) +
      theme_minimal(base_size = 12) + 
      theme(
        text = element_text(family = "Roboto"),
        plot.title = element_text(
          face = "bold", 
          family = "Fieldwork Geo Demibold",
          size = 16, 
          color = "#00374e", 
          hjust = 0, 
          margin = margin(b = 6)
        ), 
        plot.title.position = "plot",
        legend.position = "right", 
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(
          color = "#ABABAB",
          linewidth = 0.5,
          linetype = "dotted"
        ),
        axis.text.y = element_text(size = 16)
      )
    
    plt <- plotly::ggplotly(pl, tooltip = NULL) %>%
      plotly::layout(
        dragmode = FALSE,
        hovermode = "y unified",
        xaxis = list(
          showspikes = FALSE,
          hoverformat = ".0f"
        ),
        yaxis = list(showspikes = FALSE)
      ) %>%
      plotly::config(displayModeBar = FALSE, 
                     displaylogo = FALSE,
                     scrollZoom = FALSE,
                     doubleClick = FALSE,
                     editable = FALSE,
                     showAxisDragHandles = FALSE)
    
    
    
    # Customize hover per trace: show "[symbol] Export: (value) mdkr" and "[symbol] Import: (value) mdkr"
    for (i in seq_along(plt$x$data)) {
      nm <- plt$x$data[[i]]$name
      # Horizontal bars -> values are in x
      vals <- plt$x$data[[i]]$y
      formatted <- format(vals, big.mark = " ", scientific = FALSE, trim = TRUE)
      plt$x$data[[i]]$customdata <- formatted
      label <- if (grepl("Import", nm)) "Import" else "Export"
      plt$x$data[[i]]$hovertemplate <- paste0(label, ": (%{customdata}) mdkr<extra></extra>")
    }
    
    plt
  })
  
  
  # Regiondata (export/import per världsdel) - ingen geometri behövs för staplar
  region_data <- reactive({
    df <- filteredData()
    
    df %>%
      tidyr::pivot_longer(
        cols = tidyselect::matches("^(Afrika|SydAm|NordAm|Europa|Asien)(Export|Import)$"),
        names_to = c("varldsdel", "Typ"),
        names_pattern = "^(.+?)(Export|Import)$",
        values_to = "Volym"
      ) %>%
      dplyr::mutate(
        varldsdel = dplyr::recode(varldsdel,
                                  "SydAm" = "Sydamerika",
                                  "NordAm" = "Nordamerika")
      ) %>%
      dplyr::group_by(varldsdel, Typ) %>%
      dplyr::summarise(Volym = sum(Volym, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = Typ, values_from = Volym,
        values_fill = list(Volym = 0)
      ) %>%
      dplyr::rename(export_vol = Export, import_vol = Import)
  })
  
  ## Exportstapel (vart exporten går, per världsdel) - grön
  output$exportStapel <- renderPlotly({
    d <- region_data()
    req(nrow(d) > 0)
    d <- d[order(d$export_vol), ]
    plotly::plot_ly(
      d,
      x = ~export_vol,
      y = ~factor(varldsdel, levels = varldsdel),
      type = "bar", orientation = "h",
      marker = list(color = "#00a064"),
      hovertext = ~paste0(varldsdel, ": ", fmt_mdkr(export_vol), " mdkr"),
      hovertemplate = "%{hovertext}<extra></extra>"
    ) |>
      plotly::layout(
        xaxis = list(title = list(text = "mdkr",
                                  font = list(family = "Fieldwork Geo Demibold, Arial, sans-serif", size = 12, color = "#00374e")),
                     zeroline = FALSE, showgrid = TRUE, gridcolor = "#e6e6e6",
                     tickfont = list(family = "Roboto, Arial, sans-serif", size = 10, color = "#00374e")),
        yaxis = list(title = "",
                     tickfont = list(family = "Roboto, Arial, sans-serif", size = 12, color = "#00374e")),
        margin = list(l = 0, r = 10, t = 5, b = 30),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        dragmode = FALSE
      ) |>
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE,
                     scrollZoom = FALSE, doubleClick = FALSE,
                     editable = FALSE, showAxisDragHandles = FALSE)
  })
  
  ## Importstapel (varifrån importen kommer, per världsdel) - blå
  output$importStapel <- renderPlotly({
    d <- region_data()
    req(nrow(d) > 0)
    d <- d[order(d$import_vol), ]
    plotly::plot_ly(
      d,
      x = ~import_vol,
      y = ~factor(varldsdel, levels = varldsdel),
      type = "bar", orientation = "h",
      marker = list(color = "#00374e"),
      hovertext = ~paste0(varldsdel, ": ", fmt_mdkr(import_vol), " mdkr"),
      hovertemplate = "%{hovertext}<extra></extra>"
    ) |>
      plotly::layout(
        xaxis = list(title = list(text = "mdkr",
                                  font = list(family = "Fieldwork Geo Demibold, Arial, sans-serif", size = 12, color = "#00374e")),
                     zeroline = FALSE, showgrid = TRUE, gridcolor = "#e6e6e6",
                     tickfont = list(family = "Roboto, Arial, sans-serif", size = 10, color = "#00374e")),
        yaxis = list(title = "",
                     tickfont = list(family = "Roboto, Arial, sans-serif", size = 12, color = "#00374e")),
        margin = list(l = 0, r = 10, t = 5, b = 30),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        dragmode = FALSE
      ) |>
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE,
                     scrollZoom = FALSE, doubleClick = FALSE,
                     editable = FALSE, showAxisDragHandles = FALSE)
  })
  
  output$importText <- renderText({ "Varifrån kommer importen"})
  
  output$exportText <- renderText({ "Var går exporten"})
  
}