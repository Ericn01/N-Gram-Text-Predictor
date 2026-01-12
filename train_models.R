# ============================================================================
# Word Prediction Model Training Script
# ============================================================================

library(tokenizers)  
library(data.table)
library(stringr)
library(fst)

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
SAMPLE_RATE <- 0.34

# Output directory for model files
output_dir <- "model_data/"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ============================================================================
# 2. LOADING DATA AND PREPROCESSING
# ============================================================================

print("Loading and cleaning data...")

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

all_lines <- str_to_lower(all_lines)
all_lines <- str_replace_all(all_lines, "[^a-z' ]", "") 
all_lines <- str_squish(all_lines)
# ============================================================================
# 3. BUILD N-Gram Tables
# ============================================================================

build_ngram_table <- function(text_data, n) {
  print(paste0("Processing ", n, "-grams..."))
  
  # Tokenize
  tokens <- tokenize_ngrams(text_data, n = n, n_min = n)
  tokens <- unlist(tokens)
  
  # Create frequency table
  dt <- as.data.table(tokens)
  setnames(dt, "tokens", "phrase")
  dt <- dt[, .N, by = phrase]
  setnames(dt, "N", "freq")
  
  # Pruning: Removing ultra-rare combinations to save memory
  if (n > 1) dt <- dt[freq > 1] 
  if (n > 3) dt <- dt[freq > 2]
  
  if (n == 1) {
    # Unigrams are just a sorted list of common words
    dt <- dt[order(-freq)]
    write_fst(dt, paste0(output_dir, "ngram_1.fst"), compress = 100)
  } else {
    # Split into Prefix and Prediction
    # Example for 3-gram: "i love you" -> prefix="i love", prediction="you"
    dt[, prefix := word(phrase, 1, n - 1)]
    dt[, prediction := word(phrase, -1)]
    dt[, phrase := NULL] # Drop full phrase to save space
    
    # only keeping the top 4 predictions for every prefix
    dt <- dt[order(-freq), head(.SD, 4), by = prefix]
    
    # Calculate simple probability
    dt[, prob := freq / sum(freq), by = prefix]
    
    # Pre-Keying: Physically sorts data and marks it for binary search
    setkey(dt, prefix)
    
    # Save as .fst 
    write_fst(dt, paste0(output_dir, "ngram_", n, ".fst"), compress = 100)
  }
  # Cleanup RAM after every loop
  rm(dt, tokens)
  gc()
}

# ============================================================================
# 4. EXECUTION LOOP
# ============================================================================

for (i in 5:5) {
  build_ngram_table(all_lines, i)
}

print("Training Complete! .fst files are in /model_data")
