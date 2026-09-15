library(shiny)
library(tidyverse)
library(readxl)
library(leaflet)
library(sf)
library(DT)
library(shinyWidgets)
library(openxlsx)
library(plotly)

# Inputdata
# func_shinyappar.R laddas nu via library(rdshinyappar) i global.R.
#source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
#source("C:/Users/SE1C3T/Documents/Uppdrag/Dalarna/Arbetsmapp/main/script/api_r.r", encoding = "utf-8", echo = FALSE)
# source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

#projektmapp <- sokvag_for_skript_hitta()

# hjälpfunktion
lagg_ihop_expimp_lander <- function(...) {
  vals <- list(...)
  har_export <- purrr::some(vals, ~ .x %in% c("Export", "Både export och import"))
  har_import <- purrr::some(vals, ~ .x %in% c("Import", "Både export och import"))
  dplyr::case_when(
    har_export & har_import ~ "Både export och import",
    har_export              ~ "Export",
    har_import              ~ "Import",
    TRUE                    ~ NA_character_
  )
}

#datafil <- list.files(path = paste0(projektmapp, "data"), pattern = "*.xlsx", full.names = TRUE)
system.time({
  con <- shiny_uppkoppling_las("oppna_data")                                                  # skapa anslutning
  ftg_df <- tbl(con, dbplyr::in_schema("scb", "foretag")) %>%
    filter(!`stkl export, kod` %in% c("0", "") | !`stkl import, kod` %in% c("0", ""),
           `säteslän, kod` == "20") %>%
    collect()
  DBI::dbDisconnect(con)                                                                   # stäng anslutning
})

system.time({
  con_geo <- shiny_uppkoppling_las("geodata")
  karta <- st_read(con_geo, layer = Id(schema = "karta", table = "varldsdelar"))
  kommuner_db <- st_read(con_geo, layer = Id(schema = "karta", table = "kommun_scb"))
  #karta <- st_read("C:/Users/SE1C3T/Documents/Uppdrag/Dalarna/Arbetsmapp/main/dev/data/varldsdelar.gpkg")
  #RegSO <- st_read("C:/Users/SE1C3T/Documents/Uppdrag/Dalarna/Arbetsmapp/main/dev/data/RegSO_2025.gpkg")
  DBI::dbDisconnect(con_geo)
})


# if (length(datafil) == 0) {
#   stop("❌ Ingen Excel-fil hittades i mappen: ", file.path(projektmapp, "data"))
# }

#tryCatch({
#dataset_df <- read_xlsx(datafil) %>%
system.time({
  dataset_df <- ftg_df %>%
    mutate(
      Kluster = "Alla",
      `stkl export, kod` = readr::parse_number(`stkl export, kod`),
      `stkl import, kod` = readr::parse_number(`stkl import, kod`),
      stkl_num = storleksklass %>% parse_number(),
      storleksklass = fct_reorder(storleksklass, stkl_num),
      `stkl export` = if_else(`stkl export` == "", NA_character_, `stkl export`),
      `stkl import` = if_else(`stkl import` == "", NA_character_, `stkl import`),
      `stkl export` = fct_reorder(`stkl export`, `stkl export, kod`, .na_rm = TRUE),
      `stkl import` = fct_reorder(`stkl import`, `stkl import, kod`, .na_rm = TRUE)
    ) %>%
    rename(Kommun = säteskommun,
           Kommunkod = `säteskommun, kod`,
           Storleksklass = storleksklass,
           ExpStklText = `stkl export`,
           ImpStklText = `stkl import`,
           Bolagsform = `juridisk form`,
           `Huvudsaklig bransch` = bransch_1,
           SNI_kod = `bransch_1, kod`,
           Nordamerika = `nord- och centralamerika`,
           Sydamerika = sydamerika,
           Afrika = afrika,
           Företagsnamn = företagsnamn,
           Telefon = telefon,
           Postnummer = postnr
    ) %>%
    mutate(
      Europa = purrr::pmap_chr(
        list(norden, eu, `övriga europa`),
        ~ lagg_ihop_expimp_lander(..1, ..2, ..3)
      ),
      Asien = purrr::pmap_chr(
        list(`asien, ej fjärran östern`, `fjärran östern`),
        ~ lagg_ihop_expimp_lander(..1, ..2)
      )
    ) %>%
    select(-c(norden, eu, `övriga europa`, `fjärran östern`, `asien, ej fjärran östern`))
})

# Punkt 2: Omsättningsklass. Råkolumnen heter "storleksklass fin, oms" i scb.foretag.
# Justera namnet nedan om det skiljer sig i databasen - då blir Omsättningsklass NA
# och en varning skrivs ut i konsolen i stället för att appen kraschar.
if ("storleksklass fin, oms" %in% names(dataset_df)) {
  dataset_df <- dplyr::rename(dataset_df, Omsättningsklass = `storleksklass fin, oms`)
} else {
  warning("Kolumnen 'storleksklass fin, oms' hittades inte - Omsättningsklass blir NA. ",
          "Justera namnet i foretag_server.R.")
  dataset_df$Omsättningsklass <- NA_character_
}

# Visa bara aktiebolag, handels-/kommanditbolag och enskilda företag (fysiska personer).
# "Övriga aktiebolag" döps om till "Aktiebolag".
.behall_bolagsform <- c("Övriga aktiebolag", "Handelsbolag, kommanditbolag", "Fysiska personer")
if (any(trimws(dataset_df$Bolagsform) %in% .behall_bolagsform)) {
  dataset_df <- dataset_df %>%
    dplyr::filter(trimws(Bolagsform) %in% .behall_bolagsform) %>%
    dplyr::mutate(Bolagsform = dplyr::recode(trimws(Bolagsform),
                                             "Övriga aktiebolag" = "Aktiebolag",
                                             "Fysiska personer" = "Enskilda firmor"))
} else {
  warning("Inga av bolagsformerna (", paste(.behall_bolagsform, collapse = ", "),
          ") hittades i Bolagsform - behåller alla. Kontrollera värdena i 'juridisk form'.")
}
rm(.behall_bolagsform)

# Branschgrupp: joinar in Gunillas 20 grupper utifrån SNI-koden (bransch_1, kod).
# Mappningen ligger temporärt i data/sni_branschgrupp.xlsx - flyttas till databasen
# när den lagts upp där, då byts inläsningen mot ett dbReadTable-anrop.
.mapping_path <- "data/sni_branschgrupp.xlsx"
if (file.exists(.mapping_path)) {
  .sni_map <- readxl::read_xlsx(.mapping_path, sheet = "Mappning") %>%
    dplyr::transmute(
      SNI_kod = as.character(`SNI-kod`),
      Branschgrupp = `Branschgrupp`
    ) %>%
    dplyr::distinct(SNI_kod, .keep_all = TRUE)

  dataset_df <- dataset_df %>%
    dplyr::mutate(SNI_kod = as.character(SNI_kod)) %>%
    dplyr::left_join(.sni_map, by = "SNI_kod") %>%
    dplyr::mutate(Branschgrupp = tidyr::replace_na(Branschgrupp, "Övrigt"))

  rm(.sni_map)
} else {
  warning("Mappningsfilen hittades inte: ", .mapping_path,
          " - alla rader får Branschgrupp = 'Övrigt'.")
  dataset_df$Branschgrupp <- "Övrigt"
}
rm(.mapping_path)

# }, error = function(e) {
#   stop("❌ Fel vid inläsning av Excel-filen: ", conditionMessage(e))
# })

# Kommuner i Dalarna
system.time({
  kommuner_sf <- kommuner_db %>%
    filter(str_sub(knkod, 1, 2) == "20") %>%
    group_by(kommunnamn = knnamn, kommunkod = knkod) %>%
    summarise(.groups = "drop") %>%
    st_transform(4326)
})

system.time({
  # Omkretsande kommuner
  kommuner_bg <- kommuner_db %>%
    st_transform(4326) %>%
    st_filter(kommuner_sf, .predicate = st_touches) %>%
    filter(!str_sub(knkod, 1, 2) == "20") %>%
    group_by(kommunnamn = knnamn, kommunkod = knkod) %>%
    summarise(.groups = "drop") %>%
    st_simplify(dTolerance = 0.02, preserveTopology = TRUE)
})

system.time({
  kommuner_sf <- kommuner_sf %>%
    st_simplify(dTolerance = 0.01, preserveTopology = TRUE)
})

system.time({
  sf_use_s2(FALSE)
  ## Ändra till dynamisk istället för till gpkg innan prod
  varlden_sf <- karta %>%
    dplyr::mutate(varldsdel = ifelse(varldsdel == "Oceanien", "Asien", varldsdel)) %>%
    dplyr::filter(varldsdel != "Antarktis") %>%
    sf::st_wrap_dateline(options = c("WRAPDATELINE=YES"), quiet = TRUE) %>%
    dplyr::group_by(varldsdel) %>%
    dplyr::summarise(.groups = "drop")
})


foretag_server <- function(input, output, session) {

  ## Filtermeny-bearbetning
  observe({
    req(dataset_df)
    updateSelectInput(session, "juridisk",
                      choices = c("Alla", sort(unique(trimws(dataset_df$Branschgrupp)))),
                      selected = "Alla")

    updateSelectInput(session, "bolagsform",
                      choices = c("Alla", sort(unique(trimws(dataset_df$Bolagsform)))),
                      selected = "Alla")

    updateSelectInput(session, "anstallda",
                      choices = c("Alla", levels(dataset_df$Storleksklass)),
                      selected = "Alla")

    updateSelectizeInput(session, "kommun",
                         choices = c("Alla" = "Alla", setNames(
                           unique(trimws(dataset_df$Kommunkod)),
                           unique(trimws(dataset_df$Kommun)))),
                         selected = "Alla")

    updateSelectInput(session, "exportVolym",
                      choices = c("Alla" = "Alla", levels(dataset_df$ExpStklText)),
                      selected = "Alla")

    updateSelectInput(session, "exportRegion",
                      choices = c("Alla" = "Alla", "Europa", "Afrika", "Asien", "Nordamerika", "Sydamerika"),
                      selected = "Alla")

    updateSelectInput(session, "importVolym",
                      choices = c("Alla" = "Alla", levels(dataset_df$ImpStklText)),
                      selected = "Alla")

    updateSelectInput(session, "importRegion",
                      choices = c("Alla" = "Alla", "Europa", "Afrika", "Asien", "Nordamerika", "Sydamerika"),
                      selected = "Alla")
  })

  # Filtrera företagsdata
  data_filt <- reactive({
    req(input$juridisk, input$anstallda, input$bolagsform)
    df <- dataset_df

    if (!is.null(input$kommun) && input$kommun != "Alla") {
      df <- df %>% dplyr::filter(Kommunkod == input$kommun)
    }
    if (input$juridisk != "Alla") {
      df <- df %>% dplyr::filter(Branschgrupp == input$juridisk)
    }
    if (input$bolagsform != "Alla") {
      df <- df %>% dplyr::filter(Bolagsform == input$bolagsform)
    }
    if (input$anstallda != "Alla") {
      df <- df %>% dplyr::filter(Storleksklass == input$anstallda)
    }
    if (input$exportVolym != "Alla"){
      df <- df %>% dplyr::filter(ExpStklText == input$exportVolym)
    }
    if (input$importVolym != "Alla"){
      df <- df %>% dplyr::filter(ImpStklText == input$importVolym)
    }
    if (input$exportRegion != "Alla"){
      df <- df %>% dplyr::filter(.data[[input$exportRegion]] %in% c("Export", "Både export och import"))
    }
    if (input$importRegion != "Alla"){
      df <- df %>% dplyr::filter(.data[[input$importRegion]] %in% c("Import", "Både export och import"))
    }
    df
  })

  data_filt_no_kommun <- reactive({
    req(input$juridisk, input$anstallda, input$bolagsform)
    df <- dataset_df

    # DO NOT apply the kommun filter here
    if (input$juridisk != "Alla") {
      df <- df %>% dplyr::filter(Branschgrupp == input$juridisk)
    }
    if (input$bolagsform != "Alla") {
      df <- df %>% dplyr::filter(Bolagsform == input$bolagsform)
    }
    if (input$anstallda != "Alla") {
      df <- df %>% dplyr::filter(Storleksklass == input$anstallda)
    }
    if (input$exportVolym != "Alla"){
      df <- df %>% dplyr::filter(ExpStklText == input$exportVolym)
    }
    if (input$importVolym != "Alla"){
      df <- df %>% dplyr::filter(ImpStklText == input$importVolym)
    }
    if (input$exportRegion != "Alla"){
      df <- df %>% dplyr::filter(.data[[input$exportRegion]] %in% c("1", "3"))
    }
    if (input$importRegion != "Alla"){
      df <- df %>% dplyr::filter(.data[[input$importRegion]] %in% c("2", "3"))
    }
    df
  })

  ## Tabelldata
  # tabell

  ## Sammanställningstabell
  output$tabell <- renderDT({
    tabell_df <- data_filt() |>
      dplyr::arrange(dplyr::desc(stkl_num)) |>
      dplyr::select(
        Företagsnamn,
        Kommun,
        Omsättningsklass,
        Storleksklass,
        Branschgrupp,
        "Huvudsaklig bransch",
        ExpStklText,
        ImpStklText,
        Bolagsform
      ) |>
      dplyr::rename(
        Exportvolym = ExpStklText,
        Importvolym = ImpStklText
      )

    datatable(
      tabell_df,
      rownames = FALSE,
      extensions = "Scroller",
      options = list(
        language = list(
          info = "Visar _START_ till _END_ av _TOTAL_ företag",
          infoEmpty = "Visar 0 till 0 av 0 poster",
          infoFiltered = "(filtrerat från totalt _MAX_ företag)",
          lengthMenu = "Visa _MENU_ poster",
          search = "Sök:",
          zeroRecords = "Inga matchande poster"),
        deferRender = TRUE,
        scrollY = 350,
        scrollX = TRUE,
        scroller = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(
            width = '50px',
            targets = c(1,3)),
          list(
            width = '100px',
            targets = c(0,2,4,5,6,7,8)))
      )
    )
    #paste(nrow(data_filt()), "matchande företag")
  }, server = TRUE)

  ## Stapeldiagram
  output$stapel <- renderPlotly({

    .bas_df <- dataset_df
    .filt_df <- data_filt()
    if (isTRUE(filtrera_bort_ovrigt)) {
      .bas_df  <- .bas_df  |> dplyr::filter(Branschgrupp != "Övrigt")
      .filt_df <- .filt_df |> dplyr::filter(Branschgrupp != "Övrigt")
    }

    tot_data <- .bas_df |>
      dplyr::count(Branschgrupp, name = "n_total") |>
      dplyr::mutate(Branschgrupp = as.character(Branschgrupp)) |>
      dplyr::arrange(n_total)

    # Punkt: när en specifik branschgrupp är vald ska övriga staplar tonas ner
    # kraftigt så den valda branschgruppen syns tydligt.
    sel_branschgrupp <- if (!is.null(input$juridisk) && input$juridisk != "Alla") {
      input$juridisk
    } else {
      NA_character_
    }
    tot_data <- tot_data |>
      dplyr::mutate(
        tot_opacity = if (!is.na(sel_branschgrupp)) {
          ifelse(Branschgrupp == sel_branschgrupp, 0.25, 0.06)
        } else {
          0.25
        }
      )

    # Punkt 6: "Totalt"-staplarna byggs alltid från ALLA branschgrupper (oavsett
    # vald branschgrupp/kommun-filter). Det gör att y-axeln alltid har samma
    # antal kategoriplatser, så varje stapel får samma tjocklek - även när bara
    # en branschgrupp matchar filtret.
    full_cat_order <- tot_data$Branschgrupp

    # Punkt 5: tydligare hovertext, särskilt vid kommunfilter
    kommun_vald <- !is.null(input$kommun) && input$kommun != "Alla"
    kommun_namn <- if (kommun_vald) {
      kommuner_sf$kommunnamn[kommuner_sf$kommunkod == input$kommun][1]
    } else {
      NA_character_
    }

    stapel_filt <- .filt_df |>
      dplyr::count(Branschgrupp, name = "n_filt") |>
      dplyr::mutate(Branschgrupp = as.character(Branschgrupp))

    stapel_df <- dplyr::left_join(tot_data, stapel_filt, by = "Branschgrupp") |>
      dplyr::mutate(
        n_filt = tidyr::replace_na(n_filt, 0),
        tooltip = if (kommun_vald && !is.na(kommun_namn)) {
          paste0(
            "<b>", Branschgrupp, "</b><br>",
            n_filt, " företag i ", kommun_namn,
            " av ", n_total, " företag i hela Dalarna"
          )
        } else {
          paste0(
            "<b>", Branschgrupp, "</b><br>",
            n_filt, " av ", n_total, " företag i hela Dalarna"
          )
        }
      )

    if (nrow(tot_data) == 0 || sum(stapel_df$n_filt) == 0) {
      return(
        plotly::plot_ly() |>
          plotly::layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "Inga företag matchar aktuellt filter",
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(
                  family = "Roboto, Arial, sans-serif",
                  size = 12,
                  color = "#00374e"
                )
              )
            ),
            margin = list(l = 0, r = 0, t = 0, b = 0),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)"
          ) |>
          plotly::config(
            displayModeBar = FALSE,
            displaylogo = FALSE
          )
      )
    }

    max_x <- max(tot_data$n_total, na.rm = TRUE)

    if (!is.finite(max_x) || max_x == 0) {
      max_x <- 1
    }

    etiketter <- lapply(seq_len(nrow(tot_data)), function(i) {
      is_sel <- is.na(sel_branschgrupp) || tot_data$Branschgrupp[i] == sel_branschgrupp
      list(
        x = tot_data$n_total[i],
        y = tot_data$Branschgrupp[i],
        text = tot_data$Branschgrupp[i],
        xref = "x",
        yref = "y",
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "middle",
        xshift = 8,
        opacity = if (is_sel) 1 else 0.35,
        font = list(
          family = "Roboto, Arial, sans-serif",
          size = 11,
          color = "#00374e"
        )
      )
    })

    stapel_filt_df <- stapel_df |> dplyr::filter(n_filt > 0)

    plotly::plot_ly() |>
      plotly::add_bars(
        data = tot_data,
        x = ~n_total,
        y = ~Branschgrupp,
        orientation = "h",
        marker = list(
          color = "#00374e",
          opacity = tot_data$tot_opacity
        ),
        hoverinfo = "skip",
        showlegend = FALSE,
        name = "Totalt"
      ) |>
      plotly::add_bars(
        data = stapel_filt_df,
        x = ~n_filt,
        y = ~Branschgrupp,
        orientation = "h",
        marker = list(
          color = "#00374e",
          opacity = 1
        ),
        # I() tvingar fram en vektor (array) i JSON-serialiseringen även när
        # det bara finns en rad - annars visas ibland literalen "%{hovertext}"
        # i stället för den faktiska texten när endast en branschgrupp matchar.
        hovertext = I(stapel_filt_df$tooltip),
        hovertemplate = "%{hovertext}<extra></extra>",
        showlegend = FALSE,
        name = "Filtrerat"
      ) |>
      plotly::layout(
        barmode = "overlay",

        xaxis = list(
          title = list(
            text = "Antal företag",
            font = list(
              family = "Fieldwork Geo Demibold, Arial, sans-serif",
              size = 13,
              color = "#00374e"
            )
          ),
          range = c(0, max_x * 1.35),
          zeroline = FALSE,
          showgrid = FALSE,
          tickfont = list(
            family = "Roboto, Arial, sans-serif",
            size = 10,
            color = "#00374e"
          )
        ),

        yaxis = list(
          title = "",
          showticklabels = FALSE,
          showgrid = FALSE,
          zeroline = FALSE,
          categoryorder = "array",
          categoryarray = full_cat_order
        ),

        annotations = etiketter,

        margin = list(
          l = 0,
          r = 90,
          t = 5,
          b = 45
        ),

        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",

        font = list(
          family = "Roboto, Arial, sans-serif",
          color = "#00374e"
        ),

        dragmode = FALSE
      ) |>
      plotly::config(
        displayModeBar = FALSE,
        displaylogo = FALSE,
        scrollZoom = FALSE,
        doubleClick = FALSE,
        editable = FALSE,
        showAxisDragHandles = FALSE
      )
  })

  ## Kommunvalskarta
  # Joinar kommungeometrier med scb-data
  karta_data <- reactive({
    base_df <- if (!is.null(input$kommun) && input$kommun != "Alla") {
      data_filt_no_kommun()
    } else {
      data_filt()
    }

    df_counts <- base_df %>%
      group_by(Kommunkod) %>%
      summarise(antal = n(), .groups = "drop")
    kommuner_sf %>%
      left_join(
        df_counts, by = c("kommunkod" = "Kommunkod")) %>%
      mutate(antal = replace_na(antal, 0))
  })

  # Palett
  fargvektor_karta <- reactive({
    dmap <- karta_data()
    dom <- range(dmap$antal, na.rm = TRUE)
    if (!is.finite(dom[1]) || !is.finite(dom[2])) dom <- c(0,1)
    colorNumeric("YlGnBu", domain = dom)
  })
  output$karta_kommun <- renderLeaflet({
    kd <- karta_data()
    bb <- sf::st_bbox(sf::st_transform(kd, 4326))   # utbredning för Dalarna
    leaflet(kd, options = leafletOptions(
      zoomControl = FALSE,
      dragging = FALSE,
      scrollWheelZoom = FALSE,
      doubleClickZoom = FALSE,
      touchZoom = FALSE,
      preferCanvas = TRUE,
      zoomSnap = 0,
      zoomDelta = 0.1
    )) %>%
      # Passa in kartan på länets faktiska utbredning (tightare än fast zoom)
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) %>%
      # Ritningsordning
      addMapPane("kommunerPane", zIndex = 410) %>%
      addMapPane("dimPane",  zIndex = 410) %>%
      addMapPane("selectedPane", zIndex = 410) %>%

      # Basfärgsättning. Ska göras dynamisk till aktivt filter
      addPolygons(
        data = karta_data(),
        fillColor = ~fargvektor_karta()(antal),
        fillOpacity = 0.7,
        color = "grey",
        weight = 0.5,
        layerId = ~kommunkod,
        group = "kommuner",
        label = ~paste0(kommunnamn, ": ", antal, " företag"),
        options = pathOptions(pane = "kommunerPane")
      )
  })

  observeEvent(input$karta_kommun_shape_click, ignoreInit = TRUE, {
    clicked_id <- input$karta_kommun_shape_click$id
    current_id <- input$kommun

    if (identical(clicked_id, current_id)) {
      updateSelectizeInput(session, "kommun", selected = "Alla")
    } else {
      updateSelectizeInput(session, "kommun", selected = clicked_id)
    }
  })

  # inställningar för kartans interaktivitet
  observeEvent(list(karta_data(), input$kommun), ignoreInit = TRUE, {
    # Redraw base polygons with current filtered counts and palette
    leafletProxy("karta_kommun") %>%
      clearGroup("kommuner") %>%
      addPolygons(
        data = karta_data(),
        fillColor = ~fargvektor_karta()(antal),
        fillOpacity = 0.7,
        color = "grey",
        weight = 0.5,
        layerId = ~kommunkod,
        group = "kommuner",
        label = ~paste0(kommunnamn, ": ", antal, " företag"),
        options = pathOptions(pane = "kommunerPane")
      )

    # Apply dimming + red highlight depending on selected municipality
    if (!is.null(input$kommun) && input$kommun != "Alla") {
      leafletProxy("karta_kommun") %>%
        #    clearGroup("dim") %>%
        #    clearGroup("markerad") %>%
        addPolygons(
          data = dplyr::filter(kommuner_sf, kommunkod != input$kommun),
          fillColor = "#808080",
          fillOpacity = 0.25,
          color = NA,
          weight = 0,
          group = "dim",
          options = pathOptions(pane = "dimPane", interactive = FALSE)
        ) %>%
        addPolygons(
          data = dplyr::filter(kommuner_sf, kommunkod == input$kommun),
          fillColor = "transparent",
          color = "red",
          weight = 2,
          layerId = "markerad",
          group = "markerad",
          options = pathOptions(pane = "selectedPane")
        )
    } else {
      leafletProxy("karta_kommun") %>%
        clearGroup("dim") %>%
        clearGroup("markerad")
    }
  })

  ## Sidotabell
  output$sidTabell <- renderDT({
    data_filt() %>%
      select(Företagsnamn, Kommun, Storleksklass, Bolagsform, `Huvudsaklig bransch`) %>%
      arrange(desc(Storleksklass))
  },
  options = list(
    dom = 'ftip',
    pageLength = nrow(data_filt()),
    scrollY = "400px",
    scrollCollapse = TRUE,
    language = list(searchPlaceholder = 'Sök företag', search = '')
  ),
  rownames = FALSE,
  #filter = "top",
  selection = "none",
  server = TRUE
  )

  # Icke dynamiskt val av världsdelar att visa
  varldsdel_choices <- c("Afrika", "Asien", "Europa", "Nordamerika", "Sydamerika")

  liten_karta_data <- reactive({
    df <- data_filt()

    # Joinar geometrier mot datatabell
    result <- varlden_sf %>%
      dplyr::left_join(
        df %>%
          tidyr::pivot_longer(
            cols = tidyselect::all_of(varldsdel_choices),
            names_to = "varldsdel",
            values_to = "imp_exp_typ"
          ) %>%
          dplyr::group_by(varldsdel) %>%
          dplyr::summarize(
            exportforetag = sum(imp_exp_typ %in% c("Export", "Både export och import"), na.rm = TRUE),
            importforetag = sum(imp_exp_typ %in% c("Import", "Både export och import"), na.rm = TRUE),
            .groups = "drop"
          ),
        by = "varldsdel"
      ) %>%
      tidyr::replace_na(list(exportforetag = 0, importforetag = 0))
    result

  })

  observeEvent(input$resetKommunOutside, ignoreInit = TRUE, {
    updateSelectizeInput(session, "kommun", selected = "Alla")
  })

  # Byter text till "Matchande företag x med export från" vid aktivt filter
  any_filter_active <- reactive({
    any(c(input$exportRegion, input$importRegion) != "Alla", na.rm = TRUE)
  })

  # Paletter
  fargvektor_liten_export <- reactive({
    d <- liten_karta_data()
    is_sel <- if (!is.null(input$exportRegion) && input$exportRegion != "Alla") d$varldsdel == input$exportRegion else rep(TRUE, nrow(d))
    sel_vals <- ifelse(is_sel, d$exportforetag, NA_real_)
    dom <- range(sel_vals, na.rm = TRUE)
    if (!is.finite(dom[1]) || !is.finite(dom[2])) dom <- range(d$exportforetag, na.rm = TRUE)
    colorNumeric(palette = "Blues", domain = dom, na.color = "transparent")
  })
  fargvektor_liten_import <- reactive({
    d <- liten_karta_data()
    is_sel <- if (!is.null(input$importRegion) && input$importRegion != "Alla") d$varldsdel == input$importRegion else rep(TRUE, nrow(d))
    sel_vals <- ifelse(is_sel, d$importforetag, NA_real_)
    dom <- range(sel_vals, na.rm = TRUE)
    if (!is.finite(dom[1]) || !is.finite(dom[2])) dom <- range(d$importforetag, na.rm = TRUE)
    colorNumeric(palette = "Reds", domain = dom, na.color = "transparent")
  })

  ## Exportkarta
  output$exportKarta <- renderLeaflet({
    d <- liten_karta_data()

    # Endast exportRegion påverkar exportkarta
    is_selected <- if (!is.null(input$exportRegion) && input$exportRegion != "Alla") d$varldsdel == input$exportRegion else rep(TRUE, nrow(d))
    label_prefix <- if (any_filter_active()) "matchande företag med export till " else "företag med export till "

    leaflet(d,
            options = leafletOptions(
              zoomControl = FALSE, dragging = FALSE, scrollWheelZoom = FALSE,
              doubleClickZoom = FALSE, touchZoom = FALSE, preferCanvas = TRUE,
              zoomSnap = 0, zoomDelta = 0.1
            )
    ) %>%
      addMapPane("importBasePane", zIndex = 410) %>%
      addMapPane("importDimPane",  zIndex = 412) %>%
      addMapPane("importSelectedPane", zIndex = 415) %>%
      addPolygons(
        data = d,
        fillColor = ~ ifelse(is_selected, fargvektor_liten_export()(exportforetag), "#d9d9d9"),
        fillOpacity = 0.7,
        color = "grey",
        weight = 0.5,
        layerId = ~varldsdel,
        group = "export",
        label = ~paste0(exportforetag, " ", label_prefix, varldsdel)
      )
  })

  ## Importkarta
  output$importKarta <- renderLeaflet({
    d <- liten_karta_data()

    # Endast importRegion påverkar importKarta
    is_selected <- if (!is.null(input$importRegion) && input$importRegion != "Alla") d$varldsdel == input$importRegion else rep(TRUE, nrow(d))
    label_prefix <- if (any_filter_active()) "matchande företag med import från " else "företag med import från "

    leaflet(d,
            options = leafletOptions(
              zoomControl = FALSE, dragging = FALSE, scrollWheelZoom = FALSE,
              doubleClickZoom = FALSE, touchZoom = FALSE, preferCanvas = TRUE,
              zoomSnap = 0, zoomDelta = 0.1
            )
    ) %>%
      addMapPane("exportBasePane", zIndex = 410) %>%
      addMapPane("exportDimPane",  zIndex = 412) %>%
      addMapPane("exportSelectedPane", zIndex = 415) %>%
      addPolygons(
        data = d,
        fillColor = ~ ifelse(is_selected, fargvektor_liten_import()(importforetag), "#d9d9d9"),
        fillOpacity = 0.7,
        color = "grey",
        weight = 0.5,
        layerId = ~varldsdel,
        group = "import",
        label = ~paste0(importforetag, " ", label_prefix, varldsdel),
        options = pathOptions(pane = "exportBasePane")
      )
  })

  ## Bearbetning nedladdad excel
  export_df <- reactive({
    df <- data_filt()
    req(nrow(df) > 0)
    df %>%
      arrange(desc(stkl_num)) %>%
      dplyr::select(
        Företagsnamn, Telefon, Kommun, Postnummer,
        Storleksklass, Omsättningsklass, ExpStklText, ImpStklText,
        Kluster, `Huvudsaklig bransch`, Branschgrupp, Bolagsform
      )
  })

  ## Nedladdningsfunktion: filtrerad data
  output$downloadFilter <- downloadHandler(
    filename = function() {
      stamp <- format(Sys.time(), "%Y-%m-%d_%H%M")
      paste0("foretagsstatistik_filtrerad_", stamp, ".xlsx")
    },

    content = function(dl) {
      df <- export_df()

      wb <- openxlsx::createWorkbook()

      openxlsx::addWorksheet(wb, "Data")

      header_style <- openxlsx::createStyle(
        fgFill = "#00374e",
        fontColour = "#FFFFFF",
        textDecoration = "bold",
        border = "bottom",
        borderColour = "#f3e8d9"
      )

      openxlsx::writeData(
        wb,
        sheet = "Data",
        x = df,
        headerStyle = header_style
      )

      openxlsx::setColWidths(
        wb,
        sheet = "Data",
        cols = seq_len(ncol(df)),
        widths = "auto"
      )

      openxlsx::addFilter(
        wb,
        sheet = "Data",
        row = 1,
        cols = seq_len(ncol(df))
      )

      openxlsx::freezePane(
        wb,
        sheet = "Data",
        firstActiveRow = 2
      )

      openxlsx::saveWorkbook(
        wb,
        file = dl,
        overwrite = TRUE
      )
    }
  )


  ## Nedladdningsfunktion: all data
  output$downloadAll <- downloadHandler(
    filename = function() {
      stamp <- format(Sys.time(), "%Y-%m-%d_%H%M")
      paste0("foretagsstatistik_all_data_", stamp, ".xlsx")
    },

    content = function(dlAll) {
      df <- dataset_df

      wb <- openxlsx::createWorkbook()

      openxlsx::addWorksheet(wb, "Data")

      header_style <- openxlsx::createStyle(
        fgFill = "#00374e",
        fontColour = "#FFFFFF",
        textDecoration = "bold",
        border = "bottom",
        borderColour = "#f3e8d9"
      )

      openxlsx::writeData(
        wb,
        sheet = "Data",
        x = df,
        headerStyle = header_style
      )

      openxlsx::setColWidths(
        wb,
        sheet = "Data",
        cols = seq_len(ncol(df)),
        widths = "auto"
      )

      openxlsx::addFilter(
        wb,
        sheet = "Data",
        row = 1,
        cols = seq_len(ncol(df))
      )

      openxlsx::freezePane(
        wb,
        sheet = "Data",
        firstActiveRow = 2
      )

      openxlsx::saveWorkbook(
        wb,
        file = dlAll,
        overwrite = TRUE
      )
    }
  )

}
