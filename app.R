# app.R
install.packages(c("shiny", "httr2", "jsonlite", "htmltools", "markdown"))
library(shiny)
library(httr2)
library(jsonlite)
library(htmltools)

TABLE_NAME <- "Prod"

# ---------- Helpers ----------
sb_headers <- function(sb_key) {
  # For anon/public key, Supabase accepts apikey + Authorization Bearer
  list(
    apikey = sb_key,
    Authorization = paste("Bearer", sb_key),
    "Content-Type" = "application/json"
  )
}

sb_select_products <- function(sb_url, sb_key) {
  req <- request(paste0(sb_url, "/rest/v1/", TABLE_NAME)) |>
    req_headers(!!!sb_headers(sb_key)) |>
    req_url_query(
      select = "product_id,name,base_price,category,images,final_price,discount_label"
    )
  
  resp <- req_perform(req)
  if (resp_status(resp) >= 300) {
    stop("Supabase select failed: ", resp_status(resp), "\n", resp_body_string(resp))
  }
  
  txt <- resp_body_string(resp)
  if (!nzchar(txt)) return(list())
  fromJSON(txt, simplifyVector = TRUE)
}

sb_update_product <- function(sb_url, sb_key, product_id, updates) {
  req <- request(paste0(sb_url, "/rest/v1/", TABLE_NAME)) |>
    req_headers(!!!sb_headers(sb_key)) |>
    req_url_query(product_id = paste0("eq.", product_id)) |>
    req_method("PATCH") |>
    req_body_json(updates)
  
  resp <- req_perform(req)
  if (resp_status(resp) >= 300) {
    stop("Supabase update failed: ", resp_status(resp), "\n", resp_body_string(resp))
  }
  invisible(TRUE)
}

get_weather <- function(city, ow_key) {
  # Geocoding
  geo_req <- request("https://api.openweathermap.org/geo/1.0/direct") |>
    req_url_query(q = city, limit = 1, appid = ow_key)
  
  geo_resp <- req_perform(geo_req)
  if (resp_status(geo_resp) >= 300) {
    stop("OpenWeather geo failed: ", resp_status(geo_resp), "\n", resp_body_string(geo_resp))
  }
  
  geo <- fromJSON(resp_body_string(geo_resp), simplifyVector = TRUE)
  if (length(geo) == 0) return(NULL)
  
  lat <- geo$lat[1]
  lon <- geo$lon[1]
  loc <- geo$name[1]
  
  w_req <- request("https://api.openweathermap.org/data/2.5/weather") |>
    req_url_query(lat = lat, lon = lon, appid = ow_key, units = "metric")
  
  w_resp <- req_perform(w_req)
  if (resp_status(w_resp) >= 300) {
    stop("OpenWeather weather failed: ", resp_status(w_resp), "\n", resp_body_string(w_resp))
  }
  
  w <- fromJSON(resp_body_string(w_resp), simplifyVector = TRUE)
  
  list(
    location_name = loc,
    temp = w$main$temp,
    condition = w$weather[[1]]$description,
    icon_url = paste0("https://openweathermap.org/img/wn/", w$weather[[1]]$icon, "@2x.png")
  )
}

price_for_row <- function(row, temp_c) {
  base <- as.numeric(row$base_price)
  cat <- tolower(trimws(ifelse(is.null(row$category) || is.na(row$category), "", row$category)))
  
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
  if (length(rows) == 0) return(div())
  
  # rows might be a data.frame or list; normalize to list of rows
  row_list <- if (is.data.frame(rows)) split(rows, seq_len(nrow(rows))) else rows
  
  div(
    class = "menu-grid",
    lapply(row_list, function(r) {
      # r could be list with columns
      name <- ifelse(is.null(r$name) || is.na(r$name), "(unnamed product)", r$name)
      img  <- ifelse(is.null(r$images) || is.na(r$images) || !nzchar(trimws(r$images)),
                     "https://via.placeholder.com/400x260?text=No+Image",
                     trimws(r$images))
      base <- as.numeric(r$base_price)
      final <- ifelse(is.null(r$final_price) || is.na(r$final_price), base, as.numeric(r$final_price))
      label <- ifelse(is.null(r$discount_label) || is.na(r$discount_label), "", r$discount_label)
      
      price_line <- tags$div(
        tags$strong(paste0("₹", format(final, nsmall = 2))),
        if (nzchar(label)) tags$span(
          style = "text-decoration: line-through; color:#4da3ff; margin-left:8px;",
          paste0("₹", format(base, nsmall = 2))
        )
      )
      
      div(
        class = "menu-card",
        tags$img(src = img, alt = "product", class = "menu-img"),
        div(class = "menu-name", htmlEscape(name)),
        div(class = "menu-price", price_line),
        div(class = "menu-discount", label)
      )
    })
  )
}

# ---------- UI ----------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background:#070707; color:#fff; }
      .panel { background:#0b0b0b; border:1px solid #222; border-radius:14px; padding:16px; }
      .menu-grid { display:grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); gap:18px; margin-top:16px; }
      .menu-card { border:1px solid #222; background:#0b0b0b; border-radius:14px; padding:14px; text-align:center; }
      .menu-img { width:100%; height:160px; object-fit:cover; border-radius:10px; margin-bottom:10px; }
      .menu-name { font-weight:600; margin:6px 0 8px; }
      .menu-price { font-size:15px; }
      .menu-discount { color:#e63946; font-size:13px; margin-top:6px; min-height:18px; }
      .btn-primary { border-radius:12px; }
      .muted { color:#bdbdbd; }
    "))
  ),
  
  titlePanel("🍽️ Smart Restaurant — Dynamic Menu Pricing (Shiny / shinylive)"),
  
  fluidRow(
    column(
      6,
      div(
        class = "panel",
        tags$p(class="muted",
               "Enter your keys/URL below, then choose a city to fetch weather, update prices in Supabase, and display the live menu with images."
        ),
        passwordInput("ow_key", "OpenWeather API Key", placeholder = "Paste your OpenWeather API key"),
        textInput("sb_url", "Supabase URL", placeholder = "https://xxxxxxxx.supabase.co"),
        passwordInput("sb_anon", "Supabase Anon Key", placeholder = "Paste your anon/public key"),
        textInput("city", "📍 Enter City", placeholder = "e.g., Mumbai, Delhi, London"),
        actionButton("run", "Check Weather & Update Menu", class = "btn btn-primary")
      )
    ),
    column(
      6,
      div(
        class = "panel",
        uiOutput("weather_block")
      )
    )
  ),
  
  div(class="panel", uiOutput("menu"))
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  state <- reactiveValues(
    status = NULL,
    weather = NULL,
    rows = list()
  )
  
  observeEvent(input$run, {
    state$status <- NULL
    state$weather <- NULL
    state$rows <- list()
    
    # Validate
    missing <- c()
    if (!nzchar(input$ow_key)) missing <- c(missing, "OpenWeather API Key")
    if (!nzchar(input$sb_url)) missing <- c(missing, "Supabase URL")
    if (!nzchar(input$sb_anon)) missing <- c(missing, "Supabase Anon Key")
    if (!nzchar(input$city)) missing <- c(missing, "City")
    
    if (length(missing) > 0) {
      state$status <- paste("❌ Missing:", paste(missing, collapse = ", "))
      return()
    }
    
    tryCatch({
      w <- get_weather(input$city, input$ow_key)
      if (is.null(w)) {
        state$status <- paste0("❌ City '", input$city, "' not found.")
        return()
      }
      
      rows <- sb_select_products(input$sb_url, input$sb_anon)
      count <- if (is.data.frame(rows)) nrow(rows) else length(rows)
      
      if (count == 0) {
        state$weather <- w
        state$status <- "**Fetched 0 product(s) from `Prod`.**"
        state$rows <- list()
        return()
      }
      
      temp <- w$temp
      cond <- w$condition
      now_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      
      any_discount <- FALSE
      
      # Update each row in Supabase
      if (is.data.frame(rows)) {
        for (i in seq_len(nrow(rows))) {
          r <- rows[i, , drop = FALSE]
          pr <- price_for_row(r, temp)
          if (nzchar(pr$discount_label)) any_discount <- TRUE
          
          updates <- list(
            final_price = pr$final_price,
            discount_label = pr$discount_label,
            last_weather_temp = temp,
            last_weather_condition = cond,
            last_city = w$location_name,
            last_updated_at = now_iso
          )
          
          ok <- TRUE
          tryCatch({
            sb_update_product(input$sb_url, input$sb_anon, r$product_id[[1]], updates)
          }, error = function(e) {
            ok <<- FALSE
          })
          
          # Reflect in UI (and mark save failure)
          rows$final_price[i] <- pr$final_price
          rows$discount_label[i] <- if (!ok && nzchar(pr$discount_label)) {
            paste("(⚠️ save failed)", pr$discount_label)
          } else if (!ok) {
            "(⚠️ save failed)"
          } else {
            pr$discount_label
          }
        }
      }
      
      state$weather <- w
      state$status <- paste0("**Fetched ", count, " product(s) from `Prod`.**",
                             "\n\n- **Dynamic Pricing Active**: ", ifelse(any_discount, "Yes", "No"))
      state$rows <- rows
      
    }, error = function(e) {
      state$status <- paste0("❌ Error: ", conditionMessage(e))
    })
  })
  
  output$weather_block <- renderUI({
    if (!is.null(state$status) && is.null(state$weather)) {
      return(div(state$status))
    }
    
    w <- state$weather
    if (is.null(w)) {
      return(div(class="muted", "🌤️ Weather info will appear here…"))
    }
    
    div(
      tags$h3(paste0("🌤️ Weather in ", w$location_name)),
      tags$ul(
        tags$li(strong("Temperature: "), paste0(w$temp, "°C")),
        tags$li(strong("Condition: "), tools::toTitleCase(w$condition))
      ),
      tags$img(src = w$icon_url, width = 80, height = 80),
      tags$hr(),
      div(HTML(markdown::markdownToHTML(text = state$status, fragment.only = TRUE)))
    )
  })
  
  output$menu <- renderUI({
    if (is.null(state$weather) && is.null(state$status)) {
      return(div(class="muted", "🍽️ Today’s Dynamic Menu will appear here…"))
    }
    menu_cards_ui(state$rows)
  })
}

shinyApp(ui, server)
