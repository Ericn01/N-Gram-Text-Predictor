## NextWord N-Gram Predictor: Intelligent Text Completion

This is a simple predictive text engine designed to simulate the autocomplete features found in modern mobile keyboards. By analyzing large-scale text corpora (blogs, news, and twitter data), the model estimates the probability of the next word based on the user's current context.

**Features**

  1. Modular Pipeline: Separate training and inference scripts for better memory management.

  2. Context-Aware: Utilizes up to 5-gram models to capture long-range linguistic dependencies.

  3. Stupid Backoff Algorithm: A fast, efficient fallback logic that ensures a prediction is always provided, even for unseen phrases.

  4. Performance Optimized: Powered by data.table for lightning-fast binary searches and tokenizers for stable, Matrix-free tokenization.

  5. Interactive UI: A modern Shiny interface with "Click-to-Insert" functionality and real-time confidence metrics.

---

**Tech Stack**

  1. Language: R
  
  2. Data Processing: data.table, stringr, tokenizers

  3. Web Framework: shiny, bslib

---

**Project Structure**
```
.
├── training_script.R     # Script to clean data and build n-gram tables
├── app.R                 # Main Shiny application
├── model_data/           # Directory containing pre-trained .fst files
│   ├── unigram.fst
│   ├── bigram.fst
│   ├── trigram.fst
│   └── quadgram.fst
│   └── pentagram.fst
└── README.md             # Project documentation
```
---

**How It Works**

- Preprocessing: Text is converted to lowercase, stripped of numbers/special characters, and tokenized into sequences.

- Frequency Analysis: The model counts occurrences of n-word sequences to calculate Maximum Likelihood Estimates (MLE).

- Prediction: When a user types, the app takes the last n−1 words and searches for the most frequent completion.

- Fallback: If no match is found in the 4-gram model, it "backs off" to the 3-gram, then 2-gram, and finally defaults to the most common unigrams.

---
