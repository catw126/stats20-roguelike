
# ---------- UI ----------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#1b1b2f; color:#eee; font-family: Georgia, serif; }
    .card { background:#2b2b4a; border-radius:10px; padding:15px; margin:8px;
            text-align:center; }
    .bar-outer { position:relative; background:#444; border-radius:6px;
                 height:22px; margin:4px 0 10px 0; }
    .bar-inner { height:100%; border-radius:6px; }
    .bar-label { position:absolute; top:0; left:0; right:0; text-align:center;
                 font-size:13px; line-height:22px; }
    .log { background:#111; border-radius:8px; padding:10px; height:200px;
           overflow-y:auto; font-family: monospace; font-size:13px; }
    .btn-move { margin:4px; }
    h1, h2, h3 { text-align:center; }
  "))),
  titlePanel("Hero Quest"),
  uiOutput("screen")
)

# ---------- SERVER ----------

server <- function(input, output, session) {
  
  rv <- do.call(reactiveValues, initial_state())
    
  source("server/battle.R", local = TRUE)
  source("server/outputs.R", local = TRUE)
}

shinyApp(ui, server)