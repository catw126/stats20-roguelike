observeEvent(input$start, start_game(input$hero_pick))
lapply(moves$id, function(mid) {
  observeEvent(input[[paste0("move_", mid)]], play_round(mid), ignoreInit = TRUE)
})
observeEvent(input$guard,   play_round("guard"))
observeEvent(input$restart, rv$screen <- "select")

# --- Screens ---
output$hero_preview <- renderUI({
  h <- hero_template[[input$hero_pick]]
  req(h)
  div(class = "card",
      h3(h$name), p(em(h$blurb)),
      p("HP: ", h$hp), p("Attack: ", h$attack),
      p("Intellect: ", h$intellect),
      p("Defense: ", h$defense), p("Speed: ", h$speed))
})

output$move_buttons <- renderUI({
  m <- get_moves(rv$hero_key)
  lapply(seq_len(nrow(m)), function(i) {
    actionButton(paste0("move_", m$id[i]), m$label[i], class = "btn-move btn-primary")
  })
})

output$screen <- renderUI({
  
  if (rv$screen == "select") {
    return(tagList(
      h2("Choose your hero"),
      div(style = "text-align:center;",
          selectInput("hero_pick", "Choose your hero:",
                      choices = names(hero_template)),
          uiOutput("hero_preview"),
          actionButton("start", "Start quest", class = "btn-primary")
      )
    ))
  }
  
  if (rv$screen == "victory") {
    return(div(class = "card",
               h2("Victory!"),
               p("The ", strong(rv$wave$name[nrow(rv$wave)]), " has fallen."),
               h3(reward_text),
               p("The quest is complete. Thanks for playing!"),
               actionButton("restart", "Play again", class = "btn-success")
    ))
  }
  
  if (rv$screen == "defeat") {
    return(div(class = "card",
               h2("Defeated..."),
               p("Your hero has fallen on the quest. Game over."),
               actionButton("restart", "Try again", class = "btn-danger")
    ))
  }
  hero  <- rv$hero
  enemy <- rv$wave[rv$enemy_idx, ]
  fluidRow(
    column(6, div(class = "card",
                  h3(hero$name), hp_bar(rv$hp, hero$hp, "#4caf50")
    )),
    column(6, div(class = "card",
                  h3(enemy$name),
                  hp_bar(rv$enemy_hp, enemy$hp, "#e53935"),
                  p("Enemy ", rv$enemy_idx, " of ", nrow(rv$wave))
    )),
    column(12, div(style = "text-align:center;",
                   uiOutput("move_buttons"),
                   actionButton("guard", "Guard", class = "btn-move btn-info")
    )),
    column(12, h4("Battle log"),
           div(class = "log", lapply(rv$log, function(m) div(m))))
  )
})