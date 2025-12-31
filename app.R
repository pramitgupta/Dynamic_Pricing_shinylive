library(shiny)
library(htmltools)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------- Pricing rules ----------------
price_for_row <- function(row, temp_c) {
  base <- as.numeric(row$base_price)
  cat  <- tolower(trimws((row$category %||% "")))
  
  final <- base
  label <- ""
  
  is_icecream <- grepl("ice", cat) || gsub("_", " ", cat) == "ice cream"
  
  if (is_icecream) {
    if (temp_c >= 30 && temp_c <= 35) {
      final <- round(base * 0.70, 2)
      label <- "🍦 30% OFF (30–35°C)"
    } else if (temp_c >= 25 && temp_c < 30) {
      final <- round(base * 0.80, 2)
      label <- "🍦 20% OFF (25–30°C)"
    }
  } else if (cat == "coffee") {
    if (temp_c >= 10 && temp_c < 20) {
      final <- round(base * 0.70, 2)
      label <- "☕ 30% OFF (10–20°C)"
    } else if (temp_c >= 20 && temp_c < 25) {
      final <- round(base * 0.80, 2)
      label <- "☕ 20% OFF (20–25°C)"
    }
  } else if (cat == "beer") {
    if (temp_c >= 30 && temp_c <= 35) {
      final <- round(base * 0.70, 2)
      label <- "🍺 30% OFF (30–35°C)"
    } else if (temp_c >= 20 && temp_c < 25) {
      final <- round(base * 0.90, 2)
      label <- "🍺 10% OFF (20–25°C)"
    }
  } else if (cat == "food") {
    if (temp_c >= 30 && temp_c <= 35) {
      final <- round(base * 0.70, 2)
      label <- "🍽️ 30% OFF (30–35°C)"
    }
  }
  
  list(final_price = final, discount_label = label)
}

menu_cards_ui <- function(rows) {
  if (is.null(rows) || length(rows) == 0) {
    return(div(class = "muted", "No products to show."))
  }
  
  div(
    class = "menu-grid",
    lapply(rows, function(r) {
      name  <- (r$name %||% "(unnamed product)")
      img   <- (r$images %||% "")
      img   <- if (!nzchar(trimws(img))) "https://via.placeholder.com/400x260?text=No+Image" else trimws(img)
      
      base  <- as.numeric(r$base_price)
      final <- as.numeric(r$final_price %||% base)
      label <- (r$discount_label %||% "")
      
      div(
        class = "menu-card",
        tags$img(src = img, alt = "product", class = "menu-img"),
        div(class = "menu-name", htmlEscape(name)),
        div(
          class = "menu-price",
          tags$strong(sprintf("₹%.2f", final)),
          if (nzchar(label)) tags$span(
            style = "text-decoration: line-through; color:#4da3ff; margin-left:8px;",
            sprintf("₹%.2f", base)
          )
        ),
        div(class = "menu-discount", label)
      )
    })
  )
}

# ---------------- UI ----------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background:#070707; color:#fff; }
      .panel { background:#0b0b0b; border:1px solid #222; border-radius:14px; padding:16px; margin-bottom:14px;}
      .menu-grid { display:grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); gap:18px; margin-top:16px; }
      .menu-card { border:1px solid #222; background:#0b0b0b; border-radius:14px; padding:14px; text-align:center; }
      .menu-img { width:100%; height:160px; object-fit:cover; border-radius:10px; margin-bottom:10px; }
      .menu-name { font-weight:600; margin:6px 0 8px; }
      .menu-price { font-size:15px; }
      .menu-discount { color:#e63946; font-size:13px; margin-top:6px; min-height:18px; }
      .muted { color:#bdbdbd; }
      .ok { color:#7CFC98; }
      .bad { color:#ff6b6b; }
    ")),
    tags$script(HTML("
      (function(){
        function el(id){ return document.getElementById(id); }

        async function fetchJSON(url, opts){
          const res = await fetch(url, opts);
          const txt = await res.text();
          let data = null;
          try { data = txt ? JSON.parse(txt) : null; } catch(e) {}
          if(!res.ok){
            throw new Error((data && (data.message || data.error)) ? (data.message || data.error) : (txt || ('HTTP ' + res.status)));
          }
          return data;
        }

        async function runFetch(){
          const ow = (el('ow_key')?.value || '').trim();
          const sbUrlRaw = (el('sb_url')?.value || '').trim();
          const sbUrl = sbUrlRaw.replace(/\\/$/, '');
          const sbKey = (el('sb_anon')?.value || '').trim();
          const city = (el('city')?.value || '').trim();

          const missing = [];
          if(!ow) missing.push('OpenWeather API Key');
          if(!sbUrl) missing.push('Supabase URL');
          if(!sbKey) missing.push('Supabase Anon Key');
          if(!city) missing.push('City');

          if(missing.length){
            Shiny.setInputValue('js_error', '❌ Missing: ' + missing.join(', '), {priority:'event'});
            return;
          }

          Shiny.setInputValue('js_error', '', {priority:'event'});
          Shiny.setInputValue('js_status', 'Fetching weather + products…', {priority:'event'});

          try{
            const geo = await fetchJSON(
              'https://api.openweathermap.org/geo/1.0/direct?q=' + encodeURIComponent(city) + '&limit=1&appid=' + encodeURIComponent(ow)
            );

            if(!geo || !geo.length){
              Shiny.setInputValue('js_error', `❌ City '${city}' not found.`, {priority:'event'});
              Shiny.setInputValue('js_status', '', {priority:'event'});
              return;
            }

            const lat = geo[0].lat, lon = geo[0].lon;
            const loc = geo[0].name;

            const w = await fetchJSON(
              'https://api.openweathermap.org/data/2.5/weather?lat=' + lat + '&lon=' + lon + '&units=metric&appid=' + encodeURIComponent(ow)
            );

            const weather = {
              location_name: loc,
              temp: w.main.temp,
              condition: w.weather[0].description,
              icon_url: 'https://openweathermap.org/img/wn/' + w.weather[0].icon + '@2x.png'
            };

            const selectUrl = sbUrl + '/rest/v1/Prod?select=' + encodeURIComponent('product_id,name,base_price,category,images,final_price,discount_label');

            const rows = await fetchJSON(selectUrl, {
              headers: {
                'apikey': sbKey,
                'Authorization': 'Bearer ' + sbKey,
                'Content-Type': 'application/json'
              }
            });

            Shiny.setInputValue('js_payload', { sbUrl, sbKey, weather, rows }, {priority:'event'});
          } catch(err){
            Shiny.setInputValue('js_error', '❌ Error: ' + err.message, {priority:'event'});
          } finally {
            Shiny.setInputValue('js_status', '', {priority:'event'});
          }
        }

        document.addEventListener('DOMContentLoaded', function(){
          const btn = el('run');
          if(btn){
            btn.addEventListener('click', function(){
              runFetch();
            });
          }
        });

        Shiny.addCustomMessageHandler('applyUpdates', async function(msg){
          const sbUrl = msg.sbUrl;
          const sbKey = msg.sbKey;
          const updates = msg.updates || [];

          let ok = 0, fail = 0;

          for(const u of updates){
            const patchUrl = sbUrl + '/rest/v1/Prod?product_id=eq.' + encodeURIComponent(u.product_id);

            try{
              await fetchJSON(patchUrl, {
                method: 'PATCH',
                headers: {
                  'apikey': sbKey,
                  'Authorization': 'Bearer ' + sbKey,
                  'Content-Type': 'application/json',
                  'Prefer': 'return=minimal'
                },
                body: JSON.stringify(u.fields)
              });
              ok++;
            } catch(e){
              fail++;
            }
          }

          Shiny.setInputValue('js_save_result', {ok, fail, total: updates.length}, {priority:'event'});
        });
      })();
    "))
  ),
  
  titlePanel("🍽️ Smart Restaurant — shinylive"),
  
  div(
    class = "panel",
    tags$p(class = "muted",
           "Note: On shinylive/GitHub Pages, API keys are visible to users (client-side). Use for demos only."
    ),
    fluidRow(
      column(4, passwordInput("ow_key", "OpenWeather API Key")),
      column(4, textInput("sb_url", "Supabase URL", placeholder = "https://xxxx.supabase.co")),
      column(4, passwordInput("sb_anon", "Supabase Anon Key"))
    ),
    fluidRow(
      column(8, textInput("city", "📍 City", placeholder = "e.g., Mumbai")),
      column(4, br(), actionButton("run", "Check Weather & Update Menu", class = "btn btn-primary"))
    ),
    uiOutput("msg_block")
  ),
  
  div(class = "panel", uiOutput("weather_block")),
  div(class = "panel", uiOutput("menu_block"))
)

# ---------------- Server ----------------
server <- function(input, output, session) {
  
  state <- reactiveValues(
    weather = NULL,
    rows = NULL,
    sbUrl = NULL,
    sbKey = NULL,
    error = "",
    status = "",
    save = NULL
  )
  
  observeEvent(input$js_error, {
    state$error <- input$js_error %||% ""
  }, ignoreNULL = FALSE)
  
  observeEvent(input$js_status, {
    state$status <- input$js_status %||% ""
  }, ignoreNULL = FALSE)
  
  observeEvent(input$js_payload, {
    p <- input$js_payload
    if (is.null(p)) return()
    
    state$sbUrl   <- p$sbUrl
    state$sbKey   <- p$sbKey
    state$weather <- p$weather
    state$rows    <- p$rows
    state$save    <- NULL
    
    if (is.null(state$rows) || length(state$rows) == 0) return()
    
    temp <- as.numeric(state$weather$temp)
    cond <- state$weather$condition
    now_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    
    updates <- list()
    
    for (i in seq_along(state$rows)) {
      r  <- state$rows[[i]]
      pr <- price_for_row(r, temp)
      
      # update UI state
      state$rows[[i]]$final_price    <- pr$final_price
      state$rows[[i]]$discount_label <- pr$discount_label
      
      updates[[length(updates) + 1]] <- list(
        product_id = r$product_id,
        fields = list(
          final_price = pr$final_price,
          discount_label = pr$discount_label,
          last_weather_temp = temp,
          last_weather_condition = cond,
          last_city = state$weather$location_name,
          last_updated_at = now_iso
        )
      )
    }
    
    session$sendCustomMessage("applyUpdates", list(
      sbUrl = state$sbUrl,
      sbKey = state$sbKey,
      updates = updates
    ))
  })
  
  observeEvent(input$js_save_result, {
    state$save <- input$js_save_result
  })
  
  output$msg_block <- renderUI({
    div(
      if (nzchar(state$error)) div(class = "bad", state$error),
      if (nzchar(state$status)) div(class = "muted", state$status),
      if (!is.null(state$save)) {
        if (state$save$fail == 0) {
          div(class = "ok", sprintf("✅ Saved %d/%d updates to Supabase.", state$save$ok, state$save$total))
        } else {
          div(class = "bad", sprintf("⚠️ Saved %d/%d updates. Failed: %d.", state$save$ok, state$save$total, state$save$fail))
        }
      }
    )
  })
  
  output$weather_block <- renderUI({
    w <- state$weather
    if (is.null(w)) return(div(class = "muted", "🌤️ Weather info will appear here…"))
    
    div(
      tags$h3(paste0("🌤️ Weather in ", w$location_name)),
      tags$ul(
        tags$li(tags$strong("Temperature: "), paste0(w$temp, "°C")),
        tags$li(tags$strong("Condition: "), tools::toTitleCase(w$condition))
      ),
      tags$img(src = w$icon_url, width = 80, height = 80)
    )
  })
  
  output$menu_block <- renderUI({
    if (is.null(state$rows)) return(div(class = "muted", "🍽️ Menu will appear here…"))
    menu_cards_ui(state$rows)
  })
}

shinyApp(ui, server)
