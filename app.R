# ============================================================================
# Word Prediction Shiny Application
# A simple yet elegant application for next-word prediction
# ============================================================================

library(shiny)
library(data.table)
library(stringr)
library(shinycssloaders)
library(bslib) # Modern UI components

# ============================================================================
# LOAD PRE-TRAINED MODELS
# ============================================================================

MODEL_DIR <- "model_data"

# Load models into a named list for modular access
# We map them to their 'n' value for easy looping
load_models <- function() {
  list(
    "4" = readRDS(file.path(MODEL_DIR, "quadgram.rds")),
    "3" = readRDS(file.path(MODEL_DIR, "trigram.rds")),
    "2" = readRDS(file.path(MODEL_DIR, "bigram.rds")),
    "1" = readRDS(file.path(MODEL_DIR, "unigram.rds"))
  )
}

models <- load_models()
# Ensure keys are set for binary search speed
lapply(models, function(x) setkey(x, prefix))

# ============================================================================
# PREDICTION FUNCTIONS
# ============================================================================

predict_word_modular <- function(input_text, models, n_results = 5) {
  # Clean and tokenize
  clean_text <- tolower(input_text) %>% 
    str_replace_all("[^a-z' ]", "") %>% 
    str_trim() %>% 
    str_split("\\s+") %>% 
    unlist()
  
  if (length(clean_text) == 0 || input_text == "") {
    return(list(res = head(models[["1"]][order(-prob)], n_results), method = "Top Unigrams"))
  }
  
  # Iterative Backoff (Try 4-gram, then 3-gram, etc.)
  for (i in 4:2) {
    if (length(clean_text) >= (i - 1)) {
      # Grab the last (i-1) words as the prefix
      current_prefix <- paste(tail(clean_text, i - 1), collapse = " ")
      matches <- models[[as.character(i)]][prefix == current_prefix][order(-prob)]
      
      if (nrow(matches) > 0) {
        return(list(res = head(matches, n_results), method = paste0(i, "-gram Model")))
      }
    }
  }
  
  # Final Fallback to Unigram
  return(list(res = head(models[["1"]][order(-prob)], n_results), method = "Unigram Fallback"))
}

# ============================================================================
# USER INTERFACE
# ============================================================================

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  # Custom CSS for the "Floating Card" look
  tags$head(
    tags$style(HTML("
      body { background: #f0f2f5; }
      .main-card { 
        max-width: 800px; margin: 50px auto; padding: 30px; 
        background: white; border-radius: 15px; box-shadow: 0 10px 30px rgba(0,0,0,0.1); 
      }
      .prediction-btn { 
        margin: 5px; border-radius: 20px; text-transform: lowercase; 
        font-weight: 600; padding: 10px 20px;
      }
      .method-badge { font-size: 0.7em; vertical-align: middle; margin-left: 10px; }
    "))
  ),
  
  div(class = "main-card",
      h2("NextWord N-Gram Prediction App", style="font-weight: 800; color: #2c3e50;"),
      p("Start typing, and the model will suggest the next word.", class="text-muted"),
      
      textAreaInput("user_input", label = NULL, placeholder = "Type here...", 
                    rows = 3, width = "100%"),
      
      uiOutput("prediction_buttons"),
      
      hr(),
      
      # Technical Details
      accordion(
        accordion_panel(
          "Technical Metadata",
          icon = icon("gears"),
          tableOutput("debug_table"),
          uiOutput("method_info")
        )
      )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Debounce input to prevent lag while typing fast (300ms delay)
  debounced_input <- reactive({ input$user_input }) %>% debounce(300)
  
  # Reactive core prediction
  prediction_data <- reactive({
    predict_word_modular(debounced_input(), models)
  })
  
  # Render clickable buttons for top 5 predictions
  output$prediction_buttons <- renderUI({
    res <- prediction_data()$res
    
    # Create a list of buttons
    btns <- lapply(1:nrow(res), function(i) {
      actionButton(
        inputId = paste0("btn_", res$prediction[i]),
        label = res$prediction[i],
        class = "btn-primary prediction-btn",
        onclick = sprintf("Shiny.setInputValue('selected_word', '%s', {priority: 'event'})", res$prediction[i])
      )
    })
    
    div(style = "margin-top: 20px;", btns)
  })
  
  # Insert clicked word into text area
  observeEvent(input$selected_word, {
    current <- input$user_input
    # Add a space if the input doesn't end in one
    new_text <- if(grepl("\\s$", current) || current == "") {
      paste0(current, input$selected_word, " ")
    } else {
      paste0(current, " ", input$selected_word, " ")
    }
    updateTextAreaInput(session, "user_input", value = new_text)
  })
  
  # Render technical table
  output$debug_table <- renderTable({
    req(debounced_input())
    prediction_data()$res[, .(Word = prediction, Probability = sprintf("%.2f%%", prob * 100))]
  })
  
  output$method_info <- renderUI({
    span(class="badge bg-info", paste("Logic Path:", prediction_data()$method))
  })
}

# ============================================================================
# RUN APPLICATION
# ============================================================================

shinyApp(ui, server)