# ============================================================================
# Word Prediction Shiny Application
# ============================================================================

library(shiny)
library(data.table)
library(stringr)
library(fst)
library(bslib)

# ============================================================================
# LOAD PRE-TRAINED MODELS
# ============================================================================

MODEL_DIR <- "model_data"

load_models <- function() {
  cat("Loading models...\n")
  mods <- list(
    "5" = read_fst(file.path(MODEL_DIR, "ngram_5.fst"), as.data.table = TRUE),
    "4" = read_fst(file.path(MODEL_DIR, "ngram_4.fst"), as.data.table = TRUE),
    "3" = read_fst(file.path(MODEL_DIR, "ngram_3.fst"), as.data.table = TRUE),
    "2" = read_fst(file.path(MODEL_DIR, "ngram_2.fst"), as.data.table = TRUE),
    "1" = read_fst(file.path(MODEL_DIR, "ngram_1.fst"), as.data.table = TRUE)
  )

  for (i in 2:5) {
    setkey(mods[[as.character(i)]], prefix)
  }
  
  return(mods)
}

models <- load_models()

# ============================================================================
# PREDICTION FUNCTIONS
# ============================================================================

predict_word_modular <- function(input_text, models, n_results = 3) {
  # 1. Clean and tokenize input
  clean_text <- tolower(input_text) %>% 
    str_replace_all("[^a-z' ]", "") %>% 
    str_trim() %>% 
    str_split("\\s+") %>% 
    unlist()
  
  # Handle empty input
  if (length(clean_text) == 0 || input_text == "") {
    return(list(
      res = head(models[["1"]], n_results), 
      method = "Top Unigrams"
    ))
  }
  
  # 2. Iterative Backoff (Starts at 5-gram)
  # i=5 looks for 4 previous words, i=2 looks for 1 previous word
  for (i in 5:2) {
    if (length(clean_text) >= (i - 1)) {
      current_prefix <- paste(tail(clean_text, i - 1), collapse = " ")
      
      # Binary search lookup 
      matches <- models[[as.character(i)]][prefix == current_prefix]
      
      if (nrow(matches) > 0) {
        return(list(
          res = head(matches[order(-prob)], n_results), 
          method = paste0(i, "-gram Model")
        ))
      }
    }
  }
  
  # 3. Final Fallback to Unigram
  # We use the most frequent words in the English language
  return(list(
    res = head(models[["1"]], n_results), 
    method = "Unigram Fallback"
  ))
}

# ============================================================================
# UI DEFINITION
# ============================================================================
library(shiny)
library(bslib)
library(stringr)
library(dplyr)

# ============================================================================
# UI DEFINITION
# ============================================================================
ui <- fluidPage(
  # Using the Minty theme as the base
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  tags$head(
    tags$style(HTML("
      body { 
        background-color: #f0f7f4; /* Soft Minty Grey/Green */
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 100vh;
        font-family: 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
      }
      .main-card { 
        max-width: 750px; 
        width: 100%; 
        padding: 50px; 
        background: #ffffff; 
        border-radius: 30px; 
        box-shadow: 0 20px 40px rgba(120, 150, 140, 0.1);
        border: 1px solid #e0eee8;
      }
      .prediction-btn {
        background: #f8fffb;
        border: 2px solid #d1eadd;
        color: #5a7d6c;
        padding: 10px 22px;
        border-radius: 15px;
        font-weight: 600;
        margin: 6px;
        transition: all 0.2s ease-in-out;
        box-shadow: 0 2px 4px rgba(0,0,0,0.02);
      }
      .prediction-btn:hover {
        background: #78c2ad; /* Minty primary color */
        color: white;
        border-color: #78c2ad;
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(120, 194, 173, 0.3);
      }
      .model-label {
        font-size: 0.75em;
        color: #a3b8ae;
        margin-top: 30px;
        font-style: italic;
      }
      textarea {
        border-radius: 20px !important;
        border: 2px solid #e9f2ee !important;
        padding: 20px !important;
        resize: none;
        background-color: #fafdfc !important;
      }
      textarea:focus {
        border-color: #78c2ad !important;
        box-shadow: 0 0 0 0.25rem rgba(120, 194, 173, 0.15) !important;
      }
    "))
  ),
  
  div(class = "main-card",
      div(style = "text-align: center; margin-bottom: 35px;",
          h2("NextWord N-Gram Text Predictor", style = "font-weight: 800; color: #4a6358; letter-spacing: -0.5px;"),
          p("Start typing to receive text recommendations.", style = "color: #8da99c;")
      ),
      
      textAreaInput("user_input", 
                    label = NULL, 
                    placeholder = "Start your story here...", 
                    rows = 5, 
                    width = "100%"),
      
      div(style = "text-align: center; margin-top: 25px;",
          uiOutput("prediction_buttons")
      ),
      
      div(style = "text-align: center;",
          uiOutput("method_info")
      )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================
server <- function(input, output, session) {
  
  prediction_data <- reactive({
    req(input$user_input)
    
    clean_text <- tolower(input$user_input) %>% 
      str_replace_all("[^a-z' ]", "") %>% 
      str_trim() %>% 
      str_split("\\s+") %>% 
      unlist()
    
    result <- NULL
    method_used <- ""
    
    # Back-off Logic searching for 4 results
    for (i in 5:2) {
      if (length(clean_text) >= (i - 1)) {
        current_prefix <- paste(tail(clean_text, i - 1), collapse = " ")
        matches <- models[[as.character(i)]][prefix == current_prefix]
        if (nrow(matches) > 0) {
          result <- head(matches[order(-prob)], 4) # Updated to 4
          method_used <- paste0("Sourced from ", i, "-gram context")
          break
        }
      }
    }
    
    if (is.null(result)) {
      result <- head(models[["1"]][order(-freq)], 4) # Updated to 4
      method_used <- "Sourced from common vocabulary"
    }
    
    list(res = result, method = method_used)
  })
  
  # Render 4 Word Chips
  output$prediction_buttons <- renderUI({
    data <- prediction_data()
    if (is.null(data$res) || nrow(data$res) == 0) return(NULL)
    
    tagList(
      lapply(1:nrow(data$res), function(i) {
        word <- data$res$prediction[i]
        actionButton(
          inputId = paste0("btn_", word, "_", i), 
          label = word,
          class = "prediction-btn",
          onclick = sprintf("Shiny.setInputValue('insert_word', '%s', {priority: 'event'});", word)
        )
      })
    )
  })
  
  output$method_info <- renderUI({
    div(class = "model-label", prediction_data()$method)
  })
  
  observeEvent(input$insert_word, {
    # Appends word and adds a space for the next word automatically
    new_text <- paste0(str_trim(input$user_input), " ", input$insert_word, " ")
    updateTextAreaInput(session, "user_input", value = new_text)
  })
}

shinyApp(ui, server)