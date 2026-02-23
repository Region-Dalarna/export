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
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R")
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
  filter(!`stkl export, kod` %in% c("0", "") | !`stkl import, kod` %in% c("0", "")) %>%
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


server <- function(input, output, session) {

  ## Filtermeny-bearbetning
  observe({
    req(dataset_df)
    updateSelectInput(session, "juridisk",
                      choices = c("Alla", sort(unique(trimws(dataset_df$Bolagsform)))),
                      selected = "Alla")

    updateSelectInput(session, "anstallda",
                      choices = c("Alla", levels(dataset_df$Storleksklass)),
                      selected = "Alla")

    updateSelectizeInput(session, "kommun",
                         choices = c("Alla" = "Alla", setNames(
                           unique(trimws(dataset_df$Kommunkod)),
                           unique(trimws(dataset_df$Kommun)))),
                         selected = "Alla",
                         server = TRUE)

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
    req(input$juridisk, input$anstallda)
    df <- dataset_df

    if (!is.null(input$kommun) && input$kommun != "Alla") {
      df <- df %>% dplyr::filter(Kommunkod == input$kommun)
    }
    if (input$juridisk != "Alla") {
      df <- df %>% dplyr::filter(Bolagsform == input$juridisk)
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
    req(input$juridisk, input$anstallda)
    df <- dataset_df

    # DO NOT apply the kommun filter here
    if (input$juridisk != "Alla") {
      df <- df %>% dplyr::filter(Bolagsform == input$juridisk)
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
    tabell_df <- data_filt() %>%
      arrange(desc(stkl_num)) %>%
      select(
        Företagsnamn,
        Postnummer,
        Kommun,
        Storleksklass,
        "Huvudsaklig bransch",
        Bolagsform,
        ExpStklText,
        ImpStklText,
        Kluster
      ) %>%
      rename(
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
        scroller = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(
            width = '50px',
            targets = c(1:3,8)),
          list(
            width = '100px',
            targets = c(0,4:7))),
        initComplete = JS("
          function(settings, json) {
            var api = this.api();                            // API-instans för tabellen
            var tableId = settings.sTableId;

            function escRegex(s){
              return String(s).replace(/[.*+?^${}()|[\\]\\\\]/g, '\\\\$&'); }
            function closeAll(){
              $('.dt-filter-pop[data-table=\"' + tableId + '\"]').remove(); }

            function positionPop($trigger, $pop) {                // Placerar popup under event
              var rect = $trigger[0].getBoundingClientRect();     // Triggers position relativt till viewport
              var top  = window.scrollY + rect.bottom + 4;        // Beräknar absolut-höjd
              var left = window.scrollX + rect.left;
              $pop.css({ top: top + 'px', left: left + 'px' });   // Applicerar CSS-position till popup
            }

            // Wrap header text med klickbart spann
            api.columns().every(function(){                  // Itererar alla kolumner
              var $h = $(this.header());                     // jQuery av kolumners header
              if (!$h.find('.dt-header-text').length) {
                var txt = $h.text();
                $h.empty().append($('<span class=\"dt-header-text\"></span>').text(txt));
              }
            });

            // Stäng på interaktioner utanför
            $(document).on('click.dtfilters-' + tableId, function(e){           // Global klick handler namngett per tabell
              if ($(e.target).closest('.dt-filter-pop').length) return;         // Ignorera klick inuti popup
              closeAll();                                                       // Else stäng öppna popup
            });

            // Stäng på scroll utanför popup
            $(window).on('scroll.dtfilters-' + tableId + ' resize.dtfilters-' + tableId, function(){ closeAll(); });
            $(api.table().container()).on('scroll.dtfilters-' + tableId, function(){ closeAll(); });

            api.columns().every(function(colIdx){            // Filter UI per kolumn
              var column = this;                             // Kolumn-API
              var $header = $(column.header());              // Header
              var headerText = $header.text().trim();        // Header text för labels/keys
              var colKey = headerText;                       // Default kolumn-key = header text
              var keyMap = {                                 // Valfri mapping från header labels till backend keys
                'Exportvolym': 'ExpStklText',
                'Importvolym': 'ImpStklText',
                'Huvudsaklig bransch': 'Huvudsaklig bransch',
                'Företagsnamn': 'Företagsnamn',
                'Postnummer': 'Postnummer',
                'Kommun': 'Kommun',
                'Storleksklass': 'Storleksklass',
                'Bolagsform': 'Bolagsform',
                'Kluster': 'Kluster'
              };
              if (keyMap[headerText]) colKey = keyMap[headerText];
              var $trigger = $header.find('.dt-header-text');                   // Klickbara ytan i headern

              $trigger.off('click').on('click', function(ev){                   // Klick handler för att öppna popup
                ev.stopPropagation(); // don't sort when opening filter         // Förhindra header klick från att trigga sortering
                closeAll();

                // Få unika värden från klientsidsdata
                var uniq = column.data().unique().toArray()
                  .filter(function(v){ return v !== null && v !== undefined && v !== ''; })   // Filtrerar null/tomma
                  .sort(function(a,b){
                    var an = parseFloat(a), bn = parseFloat(b);
                    if(!isNaN(an) && !isNaN(bn)) return an - bn;
                    return (''+a).localeCompare(''+b);
                  });

                var popId = 'dt-pop-' + tableId + '-' + colIdx;
                var $pop = $('<div class=\"dt-filter-pop\" id=\"' + popId + '\" data-table=\"' + tableId + '\"></div>');
                var $title = $('<div class=\"title\"></div>').text('Filter: ' + $header.text());

                // Multival för värden
                var $select = $('<select multiple class=\"dt-filter-select\"></select>');
                $select.append($('<option value=\"__ALL__\" selected>All</option>'));                   //Default =Alla
                uniq.forEach(function(v){ $select.append($('<option>').attr('value', v).text(v)); });   // Lägg till unika värden

                var toggleId = 'excl_' + popId;                 // Unika värden för exkludering
                var $toggle = $('<div class=\"form-check mt-2\"></div>')
                  .append($('<input type=\"checkbox\" class=\"form-check-input\" id=\"' + toggleId + '\">'))
                  .append($('<label class=\"form-check-label\" for=\"' + toggleId + '\">Exkludera val</label>'));

                var $actions = $('<div class=\"dt-filter-actions\"></div>')
                  .append($('<button type=\"button\" class=\"btn btn-sm btn-secondary\">Rensa</button>').on('click', function(){
                      column.search('', true, false).draw();
                      if (window.Shiny) {                          // Säger till servern att filtret är rensat
                        Shiny.setInputValue(tableId + '_filter', {
                          table: tableId,
                          colIdx: colIdx,
                          colHeader: headerText,
                          colKey: colKey,
                          values: null,         // null => rensar den här kolumnens app-wide filter
                          exclude: false
                        }, {priority: 'event'});
                      }
                      closeAll();
                  }))

                  .append($('<button type=\"button\" class=\"btn btn-sm btn-primary\">Använd</button>').on('click', function(){
                    var vals = $select.val() || [];
                    var exclude = $toggle.find('input').is(':checked');         //Kollar efter exkluderings-check
                    if (vals.length === 0 || vals.indexOf('__ALL__') !== -1) {      //Om inget eller Alla är valt
                      column.search('', true, false).draw(); closeAll(); return;    //Rensa filter och stäng
                    }
                    var pattern = vals.map(function(v){ return '^' + escRegex(v) + '$'; }).join('|');   //Bygg regex för valda värden
                    if (exclude) {
                      column.search('^(?!(' + pattern + ')).*$', true, false).draw();   //Negativ lookahead för att exkludera värden
                    } else {
                      column.search('(' + pattern + ')', true, false).draw();   //Matchra valda värden
                    }
                    closeAll();
                  }));

                $pop.append($title).append($select).append($toggle).append($actions);   //Bygger popup
                $('body').append($pop);
                positionPop($trigger, $pop);        // Placera relativt till header

                var $container = $(api.table().container());
                var reposition = function(){ positionPop($trigger, $pop); };    //Byter plats vid resize
                $container.on('scroll.' + popId, reposition);
                $(window).on('resize.' + popId, reposition);      // Byter plats vid fönster resize
                $pop.on('remove', function(){     //Cleanup handlers när popup är borta
                  $container.off('scroll.' + popId, reposition);
                  $(window).off('resize.' + popId, reposition);
                });

                // Initiera sök
                $select.select2({
                  dropdownParent: $pop,     // Håll dropdown inuti popup
                  width: '100%',
                  placeholder: 'Sök och välj...',
                  closeOnSelect: false,
                  allowClear: true,
                  language: 'sv'
                });

                // Hantera 'Alla' vs specifika val
                $select.on('change', function(){
                  var cur = $select.val() || [];      // Nuvarande val
                  if (cur.length > 1 && cur.indexOf('__ALL__') !== -1) {
                    // Tar bort Alla om andra väljs
                    $select.val(cur.filter(function(v){ return v !== '__ALL__'; })).trigger('change.select2');
                  } else if (cur.length === 0) {
                    // reset till Alla när inget är valt
                    $select.val(['__ALL__']).trigger('change.select2');
                  }
                });
              });
            });
          }
        ")
      )
    )
    #paste(nrow(data_filt()), "matchande företag")
  }, server = TRUE)

  ## Stapeldiagram
  output$stapel <- renderPlotly({
    tot_data <- dataset_df %>%
      count(Bolagsform, name = "n_total")
    stapel_filt <- data_filt() %>%
      dplyr::count(Bolagsform, name = "n_filt") %>%
      dplyr::filter(n_filt > 0)

    stapel_df <- dplyr::left_join(stapel_filt, tot_data, by = "Bolagsform") %>%
      dplyr::mutate(
        tooltip = paste0(
          "Bolagsform: ", as.character(Bolagsform),
          "
          Antal matchningar: ", n_filt,
          " av ", n_total, " företag"
        )
      )

    # Stapelinställningar
    plotP <- ggplot(stapel_df, aes(x = reorder(Bolagsform, n_filt))) +
      geom_col(aes(y = n_total, text = tooltip), fill = "dodgerblue4", width = 0.8, alpha = 0.5) +
      geom_col(aes(y = n_filt, text = tooltip), fill = "dodgerblue4", width = 0.8) +
      coord_flip() +
      labs(x = NULL, y = "Antal företag") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme(panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent')
      )
    p0 <- plotly::ggplotly(plotP, tooltip = "text") %>%
      plotly::config(displayModeBar = FALSE,
                     displaylogo = FALSE,
                     scrollZoom = FALSE,
                     doubleClick = FALSE,
                     editable = FALSE,
                     showAxisDragHandles = FALSE) %>%
      plotly::layout(dragmode = FALSE)

    pb <- plotly::plotly_build(p0)
    pb$x$data <- lapply(pb$x$data, function(tr){
      tr$mode <- NULL
      # ensure every trace carries the tooltip we built
      if (!is.null(tr$text)) {
        tr$customdata <- tr$text
      } else if (!is.null(tr$hovertext)) {
        tr$customdata <- tr$hovertext
      } else {
        # last-resort fallback using x/y/name tokens
        tr$customdata <- paste0(
          (tr$name %||% ""), if (!is.null(tr$name)) "<br>" else "",
          "Kategori: %{x}<br>Antal: %{y}"
        )
      }
      tr$hovertemplate <- "<extra></extra>"
      tr$hoverinfo <- "skip"
      tr
    })
    htmlwidgets::onRender(pb, "
        function(el,x){

          // Gömmer plotly grafik
          var style = document.createElement('style');
          style.textContent = '.hoverlayer .hovertext path, .hoverlayer .hovertext rect { display:none !important; }';
          document.head.appendChild(style);

          var tip = document.createElement('div');
          Object.assign(tip.style, {
            position:'fixed', pointerEvents:'none',
            background:'white', border:'1px solid #1e90ff',
            padding:'6px 8px', borderRadius:'4px',
            font:'12px sans-serif', zIndex:9999, display:'none'
          });
          document.body.appendChild(tip);

          var gd = document.getElementById(el.id);

          gd.on('plotly_hover', function(e){
            var pt = e.points[0];
            var cd = pt.customdata;
            var txt = (Array.isArray(cd) ? cd[pt.pointNumber] : cd) ||
                      pt.text || pt.hovertext || '';
            tip.innerHTML = txt;
            tip.style.left = (e.event.clientX + 12) + 'px';
            tip.style.top  = (e.event.clientY + 12) + 'px';
            tip.style.display = 'block';
          });

          gd.on('plotly_unhover', function(){
            tip.style.display = 'none';
          });

          gd.addEventListener('mousemove', function(e){
            if (tip.style.display === 'block') {
              tip.style.left = (e.clientX + 12) + 'px';
              tip.style.top  = (e.clientY + 12) + 'px';
            }
          });
        }
      ")
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
  leaflet(karta_data(), options = leafletOptions(
    zoomControl = FALSE,
    dragging = FALSE,
    scrollWheelZoom = FALSE,
    doubleClickZoom = FALSE,
    touchZoom = FALSE,
    preferCanvas = TRUE
  )) %>%
    # Fönsterinställningar start view (centerpunkt och inzoomning)
    setView(14.36, 61.1, zoom = 6.5) %>%
    # Ritningsordning
    addMapPane("bgPane", zIndex = 405) %>%
    addMapPane("kommunerPane", zIndex = 410) %>%
    addMapPane("dimPane",  zIndex = 410) %>%
    addMapPane("selectedPane", zIndex = 410) %>%

    # Omkringliggande kommuner (gråa, icke-interaktiva)
    addPolygons(
      data = kommuner_bg,
      fillColor = "#808080",
      fillOpacity = 0.25,
      color = "grey",
      weight = 0.5,
      group = "bg",
      options = pathOptions(pane = "bgPane", interactive = FALSE)
    ) %>%
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
      updateSelectInput(session, "kommun", selected = "Alla")
    } else {
      updateSelectInput(session, "kommun", selected = clicked_id)
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
              doubleClickZoom = FALSE, touchZoom = FALSE, preferCanvas = TRUE
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
              doubleClickZoom = FALSE, touchZoom = FALSE, preferCanvas = TRUE
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
        Storleksklass,ExpStklText, ImpStklText,
        Kluster, `Huvudsaklig bransch`, Bolagsform
      )
  })

  ## Nedladdningsfunktion
  output$downloadFilter <- downloadHandler(
    filename = function(){
      stamp = format(Sys.time(), "%Y-%m-%d_%H%M")
      paste('testdata_', stamp, '.xlsx', sep='')
      },
    content = function (dl){
      df <- export_df()
      wb <- createWorkbook()
      addWorksheet(wb, "Data")
      header_style <- createStyle(
        fgFill = "#E6F0FF",
        fontColour="#00000F",
        textDecoration = "bold",
        border = "bottom",
        borderColour = "#AAAAAA"
      )
      writeData(wb, "Data", df, headerStyle = header_style)
      setColWidths(wb, "Data", cols = 1:ncol(df), widths = "auto")
      addFilter(wb, sheet = "Data", row = 1, cols =1:ncol(df))
      freezePane(wb, "Data", firstActiveRow = 2)
      saveWorkbook(wb, dl, overwrite = TRUE)
    }
  )

  output$downloadAll <- downloadHandler(
    filename = function(){
      stamp = format(Sys.time(), "%Y-%m-%d_%H%M")
      paste('testdataAllt_', stamp, '.xlsx', sep='')
    },
    content = function(dlAll){
      df <- dataset_df
      wb <- createWorkbook()
      addWorksheet(wb, "Data")
      header_style <- createStyle(
        fgFill = "#E6F0FF",
        fontColour="#00000F",
        textDecoration = "bold",
        border = "bottom",
        borderColour = "#AAAAAA"
      )
      writeData(wb, "Data", df, headerStyle = header_style)
      setColWidths(wb, "Data", cols = 1:ncol(df), widths = "auto")
      addFilter(wb, sheet = "Data", row = 1, cols =1:ncol(df))
      freezePane(wb, "Data", firstActiveRow = 2)
      saveWorkbook(wb, dlAll, overwrite = TRUE)
    }
  )

}
