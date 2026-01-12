# ============================================================================
# Word Prediction Model Training Script
# ============================================================================

library(tokenizers)  
library(data.table)
library(stringr)

set.seed(123)

# ============================================================================
# 1. CONFIGURATION
# ============================================================================

# File paths
corpus_files <- c(
  "en_US.blogs.txt",
  "en_US.news.txt",
  "en_US.twitter.txt"
)

# Sampling rate (use 0.05-0.1 for faster training, 1.0 for full corpus)
SAMPLE_RATE <- 0.5

# Output directory for model files
MODEL_DIR <- "model_data"
dir.create(MODEL_DIR, showWarnings = FALSE)

# ============================================================================
# 2. LOADING DATA AND PREPROCESSING
# ============================================================================

cat("Loading and sampling corpus files...\n")

load_and_sample <- function(filepath, sample_rate) {
  if (!file.exists(filepath)) {
    cat(sprintf("Warning: %s not found, skipping...\n", filepath))
    return(character(0))
  }
  
  con <- file(filepath, "r")
  lines <- readLines(con, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
  close(con)
  
  # Sample lines
  if (sample_rate < 1.0) {
    n_sample <- max(1, floor(length(lines) * sample_rate))
    sampled <- sample(lines, n_sample)
  } else {
    sampled <- lines
  }
  
  cat(sprintf("  %s: %s lines sampled\n", basename(filepath), 
              format(length(sampled), big.mark = ",")))
  
  sampled
}

# Load all corpora
all_lines <- unlist(lapply(corpus_files, load_and_sample, SAMPLE_RATE))
cat(sprintf("\nTotal lines loaded: %s\n\n", format(length(all_lines), big.mark = ",")))

# ============================================================================
# 3. BUILD N-Gram Tables
# ============================================================================

build_ngram_table <- function(text_vector, n, min_freq = 3) {
  cat(sprintf("Generating %d-grams...\n", n))
  
  # Generate n-grams (returns a list of character vectors)
  ng_list <- tokenize_ngrams(text_vector, n = n, n_min = n, lowercase = TRUE)
  
  # Flatten and count using data.table (extremely memory efficient)
  dt <- data.table(feature = unlist(ng_list))
  dt <- dt[, .(frequency = .N), by = feature][!is.na(feature)]
  
  # Prune low frequencies
  dt <- dt[frequency >= min_freq]
  
  # Split into Prefix and Prediction
  if (n > 1) {
    # Uses fast base R sub() to split at the LAST space
    dt[, prefix := sub("\\s\\S+$", "", feature)]
    dt[, prediction := sub("^.*\\s", "", feature)]
    
    # Calculate Probabilities within each prefix group
    dt[, prob := frequency / sum(frequency), by = prefix]
  } else {
    dt[, prefix := ""]
    dt[, prediction := feature]
    dt[, prob := frequency / sum(frequency)]
  }
  
  # Set key for fast binary search in your prediction function
  setkey(dt, prefix)
  
  return(dt[, .(prefix, prediction, prob)])
}

# Saving the names of the models as a vector
n_gram_models <- list()

for (i in 1:4) {
  dt <- build_ngram_table(toks, n = i)
  n_gram_models[[i]] <- build_ngram_table(all_lines, n = i, min_freq = 2)
}

names(n_gram_models) <- c("unigram", "bigram", "trigram", "quadgram")

# ============================================================================
# 4. SAVE MODELS
# ============================================================================

for (name in names(n_gram_models)) {
  saveRDS(n_gram_models[[name]], file = paste0("model_data/", name, ".rds"))
}
