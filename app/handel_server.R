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

# ============================================================================
#  Datakälla - VERIFIERA dessa innan körning
# ============================================================================
# Schemat där handelstabellerna ligger (ftg_df i flik 1 ligger t.ex. i "scb").
oppna_data_schema <- "mikro_db"          # <-- ÄNDRA vid behov

# Tabellnamn i databasen
tab_tidsserie <- "export_tidsserie"     # export/import/handelsbalans per år och region
tab_lander    <- "export_lander"        # per världsdel
tab_varor     <- "export_varor"         # per produktgrupp

# Värdena antas vara i KRONOR och räknas om till mdkr (miljarder). Sätt till 1
# om datan redan är i mdkr, eller 1e6 om den är i tkr.
kr_per_mdkr <- 1e9
# ============================================================================

# Plockar en kolumn ur df utifrån en lista av möjliga namn (robust mot
# stavning/accenter som "Exportvärde" vs "Exportvarde").
pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    stop("Hittade ingen av kolumnerna [", paste(candidates, collapse = ", "),
         "] i tabellen. Tillgängliga kolumner: ", paste(names(df), collapse = ", "))
  }
  df[[hit[1]]]
}

# --- Geometrier -------------------------------------------------------------
system.time({
  handel_con_geo <- shiny_uppkoppling_las("geodata")
  lan_db <- st_read(handel_con_geo, query = "SELECT * FROM karta.lan_scb")
  DBI::dbDisconnect(handel_con_geo)
})

# Längeometrier - en rad per län. Kodkolumnen heter "lnkod"; döps om till
# "lanskod" för join mot handelsdatan (regionkod).
lan_sf <- lan_db %>%
  dplyr::rename(lanskod = lnkod) %>%
  st_transform(4326)

# --- Handelsdata från databasen --------------------------------------------
system.time({
  handel_con <- shiny_uppkoppling_las("oppna_data")
  tidsserie_raw <- dplyr::tbl(handel_con, dbplyr::in_schema(oppna_data_schema, tab_tidsserie)) %>% dplyr::collect()
  lander_raw    <- dplyr::tbl(handel_con, dbplyr::in_schema(oppna_data_schema, tab_lander))    %>% dplyr::collect()
  varor_raw     <- dplyr::tbl(handel_con, dbplyr::in_schema(oppna_data_schema, tab_varor))     %>% dplyr::collect()
  DBI::dbDisconnect(handel_con)
})

RIKET <- "00"

# Standardiserade tabeller med enhetliga kolumnnamn och värden i mdkr
tidsserie_df <- tibble::tibble(
  Ar          = as.integer(pick_col(tidsserie_raw, c("Ar", "år", "År", "ar"))),
  regionkod   = stringr::str_pad(as.character(pick_col(tidsserie_raw, c("regionkod", "Regionkod"))), 2, "left", "0"),
  region      = as.character(pick_col(tidsserie_raw, c("region", "Region"))),
  export_mdkr = as.numeric(pick_col(tidsserie_raw, c("Exportvärde", "Exportvarde", "exportvärde", "exportvarde"))) / kr_per_mdkr,
  import_mdkr = as.numeric(pick_col(tidsserie_raw, c("Importvärde", "Importvarde", "importvärde", "importvarde"))) / kr_per_mdkr,
  netto_mdkr  = as.numeric(pick_col(tidsserie_raw, c("Handelsbalans", "handelsbalans"))) / kr_per_mdkr
)

lander_df <- tibble::tibble(
  Ar          = as.integer(pick_col(lander_raw, c("Ar", "år", "År", "ar"))),
  regionkod   = stringr::str_pad(as.character(pick_col(lander_raw, c("regionkod", "Regionkod"))), 2, "left", "0"),
  region      = as.character(pick_col(lander_raw, c("region", "Region"))),
  varldsdel   = as.character(pick_col(lander_raw, c("Världsdel", "Varldsdel", "världsdel", "varldsdel"))),
  export_mdkr = as.numeric(pick_col(lander_raw, c("Exportvärde", "Exportvarde", "exportvärde", "exportvarde"))) / kr_per_mdkr,
  import_mdkr = as.numeric(pick_col(lander_raw, c("Importvärde", "Importvarde", "importvärde", "importvarde"))) / kr_per_mdkr
) %>%
  dplyr::filter(!is.na(varldsdel), varldsdel != "NA")

varor_df <- tibble::tibble(
  Ar              = as.integer(pick_col(varor_raw, c("Ar", "år", "År", "ar"))),
  regionkod       = stringr::str_pad(as.character(pick_col(varor_raw, c("regionkod", "Regionkod"))), 2, "left", "0"),
  region          = as.character(pick_col(varor_raw, c("region", "Region"))),
  produktgrupp_id = suppressWarnings(as.integer(pick_col(varor_raw, c("Produktgrupp_id", "produktgrupp_id")))),
  produktgrupp    = as.character(pick_col(varor_raw, c("Produktgrupp", "produktgrupp"))),
  export_mdkr     = as.numeric(pick_col(varor_raw, c("Exportvärde", "Exportvarde", "exportvärde", "exportvarde"))) / kr_per_mdkr,
  import_mdkr     = as.numeric(pick_col(varor_raw, c("Importvärde", "Importvarde", "importvärde", "importvarde"))) / kr_per_mdkr
) %>%
  dplyr::filter(!is.na(produktgrupp), produktgrupp != "Produktgrupp okänd")

maxYear <- max(tidsserie_df$Ar, na.rm = TRUE)

# Länsdata för senaste året (exkl. Riket) - underlag till kartan
karta_data_bas <- tidsserie_df %>%
  dplyr::filter(Ar == maxYear, regionkod != RIKET) %>%
  dplyr::transmute(lanskod = regionkod, Lan = region,
                   ExportVolym = export_mdkr, ImportVolym = import_mdkr,
                   NettoHandel = netto_mdkr)

# Nationell export (Riket) för procentberäkning
nationell_export_mdkr <- tidsserie_df %>%
  dplyr::filter(Ar == maxYear, regionkod == RIKET) %>%
  dplyr::summarise(s = sum(export_mdkr, na.rm = TRUE)) %>%
  dplyr::pull(s)

handel_server <- function(input, output, session) {

  rv <- reactiveValues(sel_region = NULL, sel_regionkod = NULL)

  # Vald regionkod ("00" = Riket/Alla när inget län är valt)
  valt_regionkod <- reactive(if (is.null(rv$sel_regionkod)) RIKET else rv$sel_regionkod)

  mergedData <- reactive({
    lan_sf %>%
      left_join(karta_data_bas, by = "lanskod") %>%
      mutate(NettoHandel = replace_na(NettoHandel, 0))
  })

  # Svensk talformatering (mellanslag tusental, komma decimal)
  fmt_mdkr <- function(x) format(round(x, 1), big.mark = " ", decimal.mark = ",", nsmall = 1, trim = TRUE)

  # KPI-rad: vald region (Riket om inget valt), senaste året
  filteredData <- reactive({
    tidsserie_df %>%
      dplyr::filter(Ar == maxYear, regionkod == valt_regionkod()) %>%
      dplyr::transmute(Lan = region, ExportVolym = export_mdkr,
                       ImportVolym = import_mdkr, NettoHandel = netto_mdkr)
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
      ) %>%
      addControl(
        html = "<div class='kart-tips-wrap'><svg class='kart-tips-arrow' width='64' height='54' viewBox='0 0 64 54' xmlns='http://www.w3.org/2000/svg'><path d='M58 48 C 30 50, 10 42, 8 8' fill='none' stroke='#00374e' stroke-width='2.2' stroke-linecap='round'/><path d='M8 8 L 6 22 M8 8 L 22 11' fill='none' stroke='#00374e' stroke-width='2.2' stroke-linecap='round' stroke-linejoin='round'/></svg><div class='kart-tips'>Tips!<br>Klicka för att se<br>ett specifikt län</div></div>",
        position = "bottomright",
        className = "kart-tips-control"
      )
  })

  observeEvent(input$lansKarta_shape_click, {
    clicked_id <- input$lansKarta_shape_click$id
    if (is.null(clicked_id) || length(clicked_id) == 0) return()
    lan_namn <- karta_data_bas %>% dplyr::filter(lanskod == clicked_id) %>% dplyr::pull(Lan) %>% dplyr::first()

    if (!is.null(rv$sel_regionkod) && identical(rv$sel_regionkod, clicked_id)) {
      # Klick på redan valt län -> avvälj
      rv$sel_region <- NULL
      rv$sel_regionkod <- NULL
    } else {
      rv$sel_region <- lan_namn
      rv$sel_regionkod <- clicked_id
    }
  })

  # Markeringsöverlägg: ritar en tydlig ram runt valt län (utan att ladda om kartan)
  observe({
    proxy <- leafletProxy("lansKarta")
    proxy %>% clearGroup("valt_lan")

    if (!is.null(rv$sel_regionkod)) {
      sel <- mergedData() %>% dplyr::filter(lanskod == rv$sel_regionkod)
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
    rv$sel_region <- NULL
    rv$sel_regionkod <- NULL
  })

  output$lanText <- renderText({
    if (is.null(rv$sel_region)) "Hela Sverige" else rv$sel_region
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
    df_all <- nationell_export_mdkr

    if (isTRUE(df_all > 0)) {
      if (is.null(rv$sel_regionkod)) "100,0%" else paste0(scales::number(100 * df_sel / df_all, accuracy = 0.1, decimal.mark = ","), "%")
    } else {
      "0,0%"
    }
  })

  output$netBalansText <- renderText({
    fmt_mdkr(sum(filteredData()$ExportVolym - filteredData()$ImportVolym, na.rm = TRUE))
  })

  ts_data <- reactive({
    tidsserie_df %>%
      dplyr::filter(regionkod == valt_regionkod()) %>%
      dplyr::group_by(Ar) %>%
      dplyr::summarise(
        ExportVolym = sum(export_mdkr, na.rm = TRUE),
        ImportVolym = sum(import_mdkr, na.rm = TRUE),
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
        labels = c(Export = "Export", Import = "Import")
      ) +
      scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
      scale_x_continuous(breaks = c(2000, 2010, 2020),
                         labels = c(2000, 2010, 2020)
      ) +
      labs(
        x = NULL,
        y = "Volym (mdkr)",
        color = NULL,
        title = if (is.null(rv$sel_region)) {
          "Utveckling över tid - Hela Sverige"
        } else {
          paste0("Utveckling över tid - ", rv$sel_region)
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

    # Custom hover labels per trace
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

  # Produktgrupper (varor_df) för vald region, senaste året
  katData <- reactive({
    d <- varor_df %>%
      dplyr::filter(Ar == maxYear, regionkod == valt_regionkod()) %>%
      dplyr::group_by(produktgrupp_id, produktgrupp) %>%
      dplyr::summarise(
        Export = sum(export_mdkr, na.rm = TRUE),
        Import = sum(import_mdkr, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(produktgrupp_id)

    # Radbryt långa namn och behåll ordning (id stigande, störst id överst i stapeln)
    lev <- stringr::str_wrap(d$produktgrupp, 30)
    d %>%
      dplyr::mutate(Kategori = factor(stringr::str_wrap(produktgrupp, 30), levels = rev(lev))) %>%
      tidyr::pivot_longer(cols = c(Export, Import), names_to = "Typ", values_to = "Volym")
  })

  # Stapeldiagram produktgrupper
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
          ifelse(is.null(rv$sel_region), "Hela Sverige", rv$sel_region)
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
        axis.text.y = element_text(size = 10)
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

    for (i in seq_along(plt$x$data)) {
      nm <- plt$x$data[[i]]$name
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
    lander_df %>%
      dplyr::filter(Ar == maxYear, regionkod == valt_regionkod()) %>%
      dplyr::group_by(varldsdel) %>%
      dplyr::summarise(
        export_vol = sum(export_mdkr, na.rm = TRUE),
        import_vol = sum(import_mdkr, na.rm = TRUE),
        .groups = "drop"
      )
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
