
# ================================================================
# Korean Lexicon Analysis
# snowText
#
# Dictionary-based Korean text analysis with:
#   - exact matching
#   - trailing * prefix matching
#   - multi-word expressions
#   - Korean-oriented text normalization
#   - optional negation detection
#   - raw / negated / adjusted counts
#   - document-level evidence
#   - jamovi data-set output
# ================================================================


# ================================================================
# Helper functions
# ================================================================


# ------------------------------------------------
# Trim character safely
# ------------------------------------------------

.klex_trim <- function(x) {
  
  if (is.null(x))
    return(character())
  
  x <- as.character(x)
  trimws(x)
}


# ------------------------------------------------
# Escape regular-expression metacharacters
# ------------------------------------------------

.klex_regex_escape <- function(x) {
  
  if (length(x) == 0L)
    return(character())
  
  gsub(
    "([][{}()+?.^$|\\\\])",
    "\\\\\\1",
    x,
    perl = TRUE
  )
}


# ------------------------------------------------
# Sanitize category name for saved jamovi columns
# ------------------------------------------------

.klex_sanitize_name <- function(x) {
  
  x <- as.character(x)
  
  # Keep Hangul, Latin letters and digits.
  x <- gsub(
    "[^0-9A-Za-z가-힣]+",
    "_",
    x,
    perl = TRUE
  )
  
  x <- gsub("^_+|_+$", "", x)
  
  if (!nzchar(x))
    x <- "category"
  
  x
}


# ------------------------------------------------
# Make category-derived names unique
# ------------------------------------------------

.klex_unique_names <- function(categories) {
  
  base <- vapply(
    categories,
    .klex_sanitize_name,
    character(1)
  )
  
  make.unique(base, sep = "_")
}


# ------------------------------------------------
# Korean-oriented normalization
#
# This intentionally does NOT perform morphological analysis.
# It:
#   - converts NA to empty text
#   - optionally lower-cases Latin text
#   - converts punctuation/symbols to spaces
#   - collapses repeated whitespace
#
# Prefix matching then handles Korean inflection-like surface forms
# such as 만족*, 행복*, 불안*.
# ------------------------------------------------

.klex_normalize_text <- function(x, normalize = TRUE) {
  
  if (length(x) == 0L)
    return(character())
  
  x <- as.character(x)
  x[is.na(x)] <- ""
  
  if (!isTRUE(normalize))
    return(trimws(x))
  
  # Latin case normalization; Korean is unaffected.
  x <- tolower(x)
  
  # Preserve Korean syllables, Latin letters and numbers.
  # Everything else becomes a space.
  x <- gsub(
    "[^0-9A-Za-z가-힣]+",
    " ",
    x,
    perl = TRUE
  )
  
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}


# ------------------------------------------------
# Tokenize one document
# ------------------------------------------------

.klex_tokenize <- function(text, normalize = TRUE) {
  
  text <- .klex_normalize_text(text, normalize)
  
  if (length(text) == 0L ||
      is.na(text) ||
      !nzchar(text))
    return(character())
  
  tokens <- strsplit(
    text,
    "[[:space:]]+",
    perl = TRUE
  )[[1]]
  
  tokens[nzchar(tokens)]
}


# ------------------------------------------------
# Parse comma-separated terms
#
# exact:
#   만족
#
# prefix:
#   만족*
#
# phrase:
#   매우 만족
#
# Multi-word phrases can also contain a trailing wildcard
# on the final token, e.g. "매우 만족*".
# ------------------------------------------------

.klex_parse_terms <- function(x) {
  
  if (is.null(x) ||
      length(x) == 0L ||
      is.na(x) ||
      !nzchar(trimws(x))) {
    
    return(list(
      exact = character(),
      prefix = character(),
      phrases = list(),
      raw = character()
    ))
  }
  
  parts <- unlist(
    strsplit(as.character(x), ",", fixed = TRUE),
    use.names = FALSE
  )
  
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  
  # Remove duplicated dictionary entries while retaining order.
  parts <- parts[!duplicated(parts)]
  
  exact <- character()
  prefix <- character()
  phrases <- list()
  
  for (term in parts) {
    
    term2 <- trimws(term)
    
    has_star <- grepl("\\*$", term2)
    
    if (has_star)
      term2 <- sub("\\*$", "", term2)
    
    term2 <- trimws(term2)
    
    if (!nzchar(term2))
      next
    
    norm_term <- .klex_normalize_text(
      term2,
      normalize = TRUE
    )
    
    if (!nzchar(norm_term))
      next
    
    term_tokens <- strsplit(
      norm_term,
      "[[:space:]]+",
      perl = TRUE
    )[[1]]
    
    # Exact / prefix single term
    if (length(term_tokens) == 1L) {
      
      if (has_star)
        prefix <- c(prefix, term_tokens)
      else
        exact <- c(exact, term_tokens)
      
    } else {
      
      phrases[[length(phrases) + 1L]] <- list(
        tokens = term_tokens,
        prefix_last = has_star,
        label = term
      )
    }
  }
  
  list(
    exact = unique(exact),
    prefix = unique(prefix),
    phrases = phrases,
    raw = parts
  )
}


# ------------------------------------------------
# Parse lexicon Array from jamovi
# ------------------------------------------------

.klex_parse_lexicon <- function(lexicon) {
  
  categories <- character()
  entries <- list()
  
  if (is.null(lexicon))
    return(list(categories = categories, entries = entries))
  
  for (row in lexicon) {
    
    category <- NULL
    terms <- NULL
    
    if (is.list(row)) {
      category <- row$category
      terms <- row$terms
    }
    
    if (is.null(category) ||
        is.null(terms))
      next
    
    category <- trimws(as.character(category))
    terms <- trimws(as.character(terms))
    
    if (!nzchar(category) ||
        !nzchar(terms))
      next
    
    parsed <- .klex_parse_terms(terms)
    
    n_terms <-
      length(parsed$exact) +
      length(parsed$prefix) +
      length(parsed$phrases)
    
    if (n_terms == 0L)
      next
    
    categories <- c(categories, category)
    entries[[length(entries) + 1L]] <- parsed
  }
  
  # Category names should be unique because they are row keys
  # and become variable-name prefixes.
  if (anyDuplicated(categories)) {
    
    jmvcore::reject(
      paste0(
        "Category names must be unique. ",
        "Please rename duplicated lexicon categories."
      )
    )
  }
  
  list(
    categories = categories,
    entries = entries
  )
}


# ------------------------------------------------
# Parse Korean negation cues
# ------------------------------------------------

.klex_parse_negation_cues <- function(x) {
  
  parsed <- .klex_parse_terms(x)
  
  # Negation cues should normally be one-token expressions.
  # Multi-word expressions are retained for future compatibility,
  # although current local-window matching is token oriented.
  parsed
}


# ------------------------------------------------
# Match one token to exact/prefix term
# ------------------------------------------------

.klex_token_matches <- function(token, exact, prefix) {
  
  if (!nzchar(token))
    return(FALSE)
  
  if (length(exact) > 0L &&
      token %in% exact)
    return(TRUE)
  
  if (length(prefix) > 0L) {
    
    hit <- vapply(
      prefix,
      function(p) startsWith(token, p),
      logical(1)
    )
    
    if (any(hit))
      return(TRUE)
  }
  
  FALSE
}


# ------------------------------------------------
# Find single-token matches
#
# Returns data frame:
#   start
#   end
#   term
#   surface
# ------------------------------------------------

.klex_find_single_matches <- function(tokens, parsed) {
  
  out <- list()
  
  if (length(tokens) == 0L)
    return(data.frame(
      start = integer(),
      end = integer(),
      term = character(),
      surface = character(),
      stringsAsFactors = FALSE
    ))
  
  # Exact terms
  if (length(parsed$exact) > 0L) {
    
    for (term in parsed$exact) {
      
      idx <- which(tokens == term)
      
      if (length(idx) > 0L) {
        for (i in idx) {
          out[[length(out) + 1L]] <- data.frame(
            start = i,
            end = i,
            term = term,
            surface = tokens[i],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  # Prefix terms
  if (length(parsed$prefix) > 0L) {
    
    for (term in parsed$prefix) {
      
      idx <- which(startsWith(tokens, term))
      
      if (length(idx) > 0L) {
        for (i in idx) {
          out[[length(out) + 1L]] <- data.frame(
            start = i,
            end = i,
            term = paste0(term, "*"),
            surface = tokens[i],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  if (length(out) == 0L)
    return(data.frame(
      start = integer(),
      end = integer(),
      term = character(),
      surface = character(),
      stringsAsFactors = FALSE
    ))
  
  do.call(rbind, out)
}


# ------------------------------------------------
# Find multi-word phrase matches
# ------------------------------------------------

.klex_find_phrase_matches <- function(tokens, parsed) {
  
  out <- list()
  
  if (length(tokens) == 0L ||
      length(parsed$phrases) == 0L) {
    
    return(data.frame(
      start = integer(),
      end = integer(),
      term = character(),
      surface = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  n_tokens <- length(tokens)
  
  for (phrase in parsed$phrases) {
    
    p <- phrase$tokens
    m <- length(p)
    
    if (m == 0L || m > n_tokens)
      next
    
    for (i in seq_len(n_tokens - m + 1L)) {
      
      segment <- tokens[i:(i + m - 1L)]
      
      if (m == 1L) {
        
        ok <- if (isTRUE(phrase$prefix_last))
          startsWith(segment[1], p[1])
        else
          identical(segment[1], p[1])
        
      } else {
        
        first_ok <- all(
          segment[seq_len(m - 1L)] ==
            p[seq_len(m - 1L)]
        )
        
        last_ok <- if (isTRUE(phrase$prefix_last))
          startsWith(segment[m], p[m])
        else
          segment[m] == p[m]
        
        ok <- first_ok && last_ok
      }
      
      if (isTRUE(ok)) {
        
        out[[length(out) + 1L]] <- data.frame(
          start = i,
          end = i + m - 1L,
          term = phrase$label,
          surface = paste(segment, collapse = " "),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(out) == 0L)
    return(data.frame(
      start = integer(),
      end = integer(),
      term = character(),
      surface = character(),
      stringsAsFactors = FALSE
    ))
  
  do.call(rbind, out)
}


# ------------------------------------------------
# Find all matches for one category
#
# Duplicate spans caused by overlapping identical dictionary rules
# are removed to avoid accidental double counting.
# ------------------------------------------------

.klex_find_matches <- function(tokens, parsed) {
  
  a <- .klex_find_single_matches(tokens, parsed)
  b <- .klex_find_phrase_matches(tokens, parsed)
  
  out <- rbind(a, b)
  
  if (nrow(out) == 0L)
    return(out)
  
  key <- paste(
    out$start,
    out$end,
    out$surface,
    sep = "\r"
  )
  
  out <- out[!duplicated(key), , drop = FALSE]
  rownames(out) <- NULL
  
  out
}


# ------------------------------------------------
# Test whether a token matches one negation cue
# ------------------------------------------------

.klex_is_negation_token <- function(token, cues) {
  
  .klex_token_matches(
    token,
    exact = cues$exact,
    prefix = cues$prefix
  )
}


# ------------------------------------------------
# Find negation around one matched expression
#
# Korean negation may occur:
#   before:  "안 좋다", "못 했다"
#   after:   "좋지 않았다", "만족하지 못했다"
#
# A symmetric local window is therefore used. The function only
# FLAGS the match; negationMode determines whether it is excluded.
# ------------------------------------------------

.klex_find_negation <- function(tokens,
                                start,
                                end,
                                cues,
                                window = 2L) {
  
  n <- length(tokens)
  
  if (n == 0L)
    return(list(
      negated = FALSE,
      cue = "",
      cueIndex = NA_integer_,
      context = ""
    ))
  
  window <- suppressWarnings(as.integer(window))
  
  if (is.na(window) || window < 1L)
    window <- 1L
  
  left <- max(1L, start - window)
  right <- min(n, end + window)
  
  candidate_idx <- seq.int(left, right)
  
  # Do not treat the matched token itself as a negation cue.
  matched_idx <- seq.int(start, end)
  candidate_idx <- setdiff(candidate_idx, matched_idx)
  
  found <- integer()
  
  if (length(candidate_idx) > 0L) {
    
    is_neg <- vapply(
      candidate_idx,
      function(i)
        .klex_is_negation_token(tokens[i], cues),
      logical(1)
    )
    
    found <- candidate_idx[is_neg]
  }
  
  # Optional support for multi-word negation cues
  # inside the local context.
  phrase_found <- NULL
  
  if (length(found) == 0L &&
      length(cues$phrases) > 0L) {
    
    local_tokens <- tokens[left:right]
    
    phrase_matches <-
      .klex_find_phrase_matches(local_tokens, cues)
    
    if (nrow(phrase_matches) > 0L) {
      
      phrase_start_global <-
        phrase_matches$start[1] + left - 1L
      
      phrase_found <- list(
        index = phrase_start_global,
        cue = phrase_matches$surface[1]
      )
    }
  }
  
  if (length(found) == 0L && is.null(phrase_found)) {
    
    return(list(
      negated = FALSE,
      cue = "",
      cueIndex = NA_integer_,
      context = paste(tokens[left:right], collapse = " ")
    ))
  }
  
  if (length(found) > 0L) {
    
    # Use nearest negation cue to the matched expression.
    distance <- pmin(
      abs(found - start),
      abs(found - end)
    )
    
    cue_index <- found[which.min(distance)]
    cue <- tokens[cue_index]
    
  } else {
    
    cue_index <- phrase_found$index
    cue <- phrase_found$cue
  }
  
  context_left <- max(1L, left)
  context_right <- min(n, right)
  
  list(
    negated = TRUE,
    cue = cue,
    cueIndex = cue_index,
    context = paste(
      tokens[context_left:context_right],
      collapse = " "
    )
  )
}


# ------------------------------------------------
# Resolve dictionary and validate user input
# ------------------------------------------------

.klex_resolve_dictionary <- function(lexicon) {
  
  dict <- .klex_parse_lexicon(lexicon)
  
  if (length(dict$categories) == 0L) {
    
    jmvcore::reject(
      paste0(
        "The lexicon has no complete category. ",
        "Add at least one category with one or more terms."
      )
    )
  }
  
  dict
}


# ------------------------------------------------
# Dictionary summary data
# ------------------------------------------------

.klex_dictionary_summary <- function(dict) {
  
  rows <- vector(
    "list",
    length(dict$categories)
  )
  
  for (i in seq_along(dict$categories)) {
    
    p <- dict$entries[[i]]
    
    rows[[i]] <- data.frame(
      category = dict$categories[i],
      exactTerms = length(p$exact),
      wildcardPrefixes = length(p$prefix),
      multiWordTerms = length(p$phrases),
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, rows)
}


# ================================================================
# jamovi analysis class
# ================================================================

#' @export
klexiconClass <- if (requireNamespace(
  "jmvcore",
  quietly = TRUE
)) R6::R6Class(
  
  "klexiconClass",
  
  inherit = klexiconBase,
  
  private = list(
    
    .htmlwidget = NULL,
    
    
    # ========================================================
    # Initialization
    # ========================================================
    
    .init = function() {
      
      private$.htmlwidget <- HTMLWidget$new()
      
      # ----------------------------------------------------
      # Instructions
      # ----------------------------------------------------
      
      self$results$instructions$setVisible(
        visible = TRUE
      )
      
      self$results$instructions$setContent(
        
        private$.htmlwidget$generate_accordion(
          
          title = "Instructions",
          
          content = paste(
            
            '<div style="border: 2px solid #e6f4fe;',
            'border-radius: 15px;',
            'padding: 15px;',
            'background-color: #e6f4fe;',
            'margin-top: 10px;">',
            
            '<div style="text-align:justify;">',
            
            '<p><strong>Korean Lexicon Analysis</strong> ',
            'converts Korean text into reproducible ',
            'dictionary-based category scores.</p>',
            
            '<ul>',
            
            '<li><strong>Text Variable</strong>: select one ',
            'text column containing the documents or responses ',
            'to be analysed.</li>',
            
            '<li><strong>Lexicon</strong>: four example ',
            'categories are provided by default. ',
            'The categories and terms are examples only and ',
            'may be edited, removed, or expanded for the ',
            'research purpose.</li>',
            
            '<li>Enter terms as a ',
            '<strong>comma-separated list</strong>. ',
            'A trailing <strong>*</strong> indicates prefix ',
            'matching. For example, <strong>만족*</strong> can ',
            'match forms such as 만족했다 and 만족스럽다.</li>',
            
            '<li><strong>Multi-word expressions</strong> are ',
            'also supported.</li>',
            
            '<li><strong>Korean text normalization</strong> ',
            'standardizes punctuation, repeated spaces, and ',
            'Latin-letter case before matching. ',
            'It does not perform morphological analysis.</li>',
            
            '<li><strong>Negation handling</strong> examines ',
            'nearby Korean negation cues. ',
            '<em>No adjustment</em> retains all matches; ',
            '<em>Flag only</em> reports negated matches without ',
            'removing them; <em>Exclude negated matches</em> ',
            'removes flagged matches from the adjusted count.</li>',
            
            '<li><strong>Negation window</strong> specifies ',
            'how many neighbouring tokens on each side of a ',
            'matched expression are inspected for negation cues.</li>',
            
            '<li>The negation procedure is a transparent ',
            'rule-based heuristic rather than semantic or AI ',
            'interpretation. Researchers should inspect ',
            '<strong>Negation Evidence</strong> when negation ',
            'adjustment is used.</li>',
            
            '<li><strong>Detected terms</strong> can be saved ',
            'with the derived scores, and ',
            '<strong>Append results to the data set</strong> ',
            'adds the document-level variables directly to ',
            'the jamovi data set for subsequent statistical ',
            'analysis.</li>',
            
            '</ul>',
            
            '<p><strong>Important:</strong> dictionary results ',
            'depend on the researcher-defined categories and ',
            'terms. The supplied categories are examples and ',
            'should not be interpreted as a validated ',
            'psychological lexicon.</p>',
            
            '</div></div>'
          )
        )
      )
      
      
      # ----------------------------------------------------
      # Create dictionary result rows
      # ----------------------------------------------------
      
      dict <- tryCatch(
        
        .klex_resolve_dictionary(
          self$options$lexicon
        ),
        
        error = function(e)
          NULL
      )
      
      if (is.null(dict))
        return()
      
      for (category in dict$categories) {
        
        self$results$dictSummary$addRow(
          rowKey = category,
          values = NULL
        )
        
        self$results$catSummary$addRow(
          rowKey = category,
          values = NULL
        )
      }
    },
    
    
    # ========================================================
    # Main analysis
    # ========================================================
    
    .run = function() {
      
      # ----------------------------------------------------
      # Dictionary
      # ----------------------------------------------------
      
      dict <- .klex_resolve_dictionary(
        self$options$lexicon
      )
      
      # ----------------------------------------------------
      # Dictionary Summary
      # ----------------------------------------------------
      
      dict_table <- self$results$dictSummary
      dict_table$deleteRows()
      
      dict_summary <-
        .klex_dictionary_summary(dict)
      
      for (i in seq_len(nrow(dict_summary))) {
        
        category <- dict_summary$category[i]
        
        dict_table$addRow(
          rowKey = category,
          values = list(
            category =
              category,
            exactTerms =
              dict_summary$exactTerms[i],
            wildcardPrefixes =
              dict_summary$wildcardPrefixes[i],
            multiWordTerms =
              dict_summary$multiWordTerms[i]
          )
        )
      }
      
      
      # ----------------------------------------------------
      # Korean Matching Settings
      # ----------------------------------------------------
      
      settings_table <-
        self$results$matchingSummary
      
      settings_table$deleteRows()
      
      mode_label <- switch(
        self$options$negationMode,
        none = "No adjustment",
        flag = "Flag only",
        exclude = "Exclude negated matches",
        as.character(self$options$negationMode)
      )
      
      settings_table$addRow(
        rowKey = "normalization",
        values = list(
          setting = "Korean text normalization",
          value = if (isTRUE(
            self$options$normalizeKorean
          )) "Yes" else "No"
        )
      )
      
      settings_table$addRow(
        rowKey = "negationMode",
        values = list(
          setting = "Negation handling",
          value = mode_label
        )
      )
      
      settings_table$addRow(
        rowKey = "negationWindow",
        values = list(
          setting = "Negation window",
          value = as.character(
            self$options$negationWindow
          )
        )
      )
      
      settings_table$addRow(
        rowKey = "negationCues",
        values = list(
          setting = "Negation cues",
          value = as.character(
            self$options$negationCues
          )
        )
      )
      
      
      # ----------------------------------------------------
      # No text variable yet:
      # show dictionary/settings only
      # ----------------------------------------------------
      
      if (is.null(self$options$textVar))
        return()
      
      
      # ----------------------------------------------------
      # Text data
      # ----------------------------------------------------
      
      texts <- self$data[[
        self$options$textVar
      ]]
      
      texts <- as.character(texts)
      texts[is.na(texts)] <- ""
      
      n_docs <- length(texts)
      
      if (n_docs == 0L)
        return()
      
      
      # ----------------------------------------------------
      # Negation dictionary
      # ----------------------------------------------------
      
      neg_cues <- .klex_parse_negation_cues(
        self$options$negationCues
      )
      
      use_negation <-
        !identical(
          self$options$negationMode,
          "none"
        )
      
      exclusion_mode <-
        identical(
          self$options$negationMode,
          "exclude"
        )
      
      window <- suppressWarnings(
        as.integer(
          self$options$negationWindow
        )
      )
      
      if (is.na(window) || window < 1L)
        window <- 1L
      
      
      # ----------------------------------------------------
      # Allocate document-level result vectors
      # ----------------------------------------------------
      
      n_tokens <- integer(n_docs)
      n_types <- integer(n_docs)
      
      safe_names <-
        .klex_unique_names(dict$categories)
      
      raw_counts <- list()
      neg_counts <- list()
      adjusted_counts <- list()
      percentages <- list()
      detected_terms <- list()
      
      for (i in seq_along(dict$categories)) {
        
        sn <- safe_names[i]
        
        raw_counts[[sn]] <-
          integer(n_docs)
        
        neg_counts[[sn]] <-
          integer(n_docs)
        
        adjusted_counts[[sn]] <-
          integer(n_docs)
        
        percentages[[sn]] <-
          numeric(n_docs)
        
        detected_terms[[sn]] <-
          character(n_docs)
      }
      
      
      # ----------------------------------------------------
      # Detailed evidence containers
      # ----------------------------------------------------
      
      detail_rows <- list()
      evidence_rows <- list()
      
      detail_index <- 0L
      evidence_index <- 0L
      
      
      # ----------------------------------------------------
      # Analyse document by document
      # ----------------------------------------------------
      
      for (doc_i in seq_len(n_docs)) {
        
        tokens <- .klex_tokenize(
          texts[doc_i],
          normalize =
            isTRUE(
              self$options$normalizeKorean
            )
        )
        
        n_tokens[doc_i] <- length(tokens)
        n_types[doc_i] <-
          length(unique(tokens))
        
        
        for (cat_i in seq_along(
          dict$categories
        )) {
          
          category <-
            dict$categories[cat_i]
          
          parsed <-
            dict$entries[[cat_i]]
          
          sn <- safe_names[cat_i]
          
          matches <-
            .klex_find_matches(
              tokens,
              parsed
            )
          
          raw_n <- nrow(matches)
          
          neg_n <- 0L
          adjusted_n <- raw_n
          
          matched_surface <- character()
          
          
          if (raw_n > 0L) {
            
            matched_surface <-
              matches$surface
            
            negated <-
              rep(FALSE, raw_n)
            
            for (m in seq_len(raw_n)) {
              
              neg_info <- list(
                negated = FALSE,
                cue = "",
                cueIndex = NA_integer_,
                context = ""
              )
              
              if (use_negation) {
                
                neg_info <-
                  .klex_find_negation(
                    tokens = tokens,
                    start =
                      matches$start[m],
                    end =
                      matches$end[m],
                    cues = neg_cues,
                    window = window
                  )
              }
              
              negated[m] <-
                isTRUE(
                  neg_info$negated
                )
              
              
              # ------------------------------------
              # Negation evidence
              # ------------------------------------
              
              if (isTRUE(
                neg_info$negated
              )) {
                
                evidence_index <-
                  evidence_index + 1L
                
                treatment <-
                  if (exclusion_mode)
                    "Excluded"
                else
                  "Flagged"
                
                evidence_rows[[
                  evidence_index
                ]] <- list(
                  document = doc_i,
                  category = category,
                  matchedTerm =
                    matches$surface[m],
                  negationCue =
                    neg_info$cue,
                  context =
                    neg_info$context,
                  action =
                    treatment
                )
              }
            }
            
            
            neg_n <- sum(negated)
            
            if (exclusion_mode)
              adjusted_n <-
              raw_n - neg_n
            else
              adjusted_n <- raw_n
          }
          
          
          raw_counts[[sn]][doc_i] <-
            raw_n
          
          neg_counts[[sn]][doc_i] <-
            neg_n
          
          adjusted_counts[[sn]][doc_i] <-
            adjusted_n
          
          percentages[[sn]][doc_i] <-
            if (n_tokens[doc_i] > 0L)
              adjusted_n /
            n_tokens[doc_i]
          else
            0
          
          
          if (length(
            matched_surface
          ) > 0L) {
            
            detected_terms[[sn]][doc_i] <-
              paste(
                unique(
                  matched_surface
                ),
                collapse = ", "
              )
            
          } else {
            
            detected_terms[[sn]][doc_i] <-
              ""
          }
          
          
          # --------------------------------------------
          # Document-level details
          # --------------------------------------------
          
          if (isTRUE(self$options$documentDetails) && raw_n > 0L) {
            
            detail_index <- detail_index + 1L
            
            doc_percentage <- if (n_tokens[doc_i] > 0L) {
              adjusted_n / n_tokens[doc_i]
            } else {
              0
            }
            
            detail_rows[[detail_index]] <- list(
              document = doc_i,
              category = category,
              tokens = n_tokens[doc_i],
              rawCount = raw_n,
              adjustedCount = adjusted_n,
              percentage = doc_percentage,
              detectedTerms = detected_terms[[sn]][doc_i]
            )
          }
          
          
        }
      }
      
      
      # ----------------------------------------------------
      # Category Counts Summary
      # ----------------------------------------------------
      
      cat_table <-
        self$results$catSummary
      
      cat_table$deleteRows()
      
      total_adjusted_all <- 0L
      
      for (cat_i in seq_along(
        dict$categories
      )) {
        
        category <-
          dict$categories[cat_i]
        
        sn <- safe_names[cat_i]
        
        rc <- raw_counts[[sn]]
        nc <- neg_counts[[sn]]
        ac <- adjusted_counts[[sn]]
        pc <- percentages[[sn]]
        
        docs_with_matches <-
          sum(ac > 0L)
        
        total_adjusted_all <-
          total_adjusted_all +
          sum(ac)
        
        cat_table$addRow(
          rowKey = category,
          values = list(
            category =
              category,
            rawCount =
              sum(rc),
            negatedCount =
              sum(nc),
            adjustedCount =
              sum(ac),
            meanCount =
              if (n_docs > 0L)
                mean(ac)
            else
              0,
            meanPerc =
              if (n_docs > 0L)
                mean(pc)
            else
              0,
            docCount =
              docs_with_matches,
            docPerc =
              if (n_docs > 0L)
                docs_with_matches /
              n_docs
            else
              0
          )
        )
      }
      
      
      # ----------------------------------------------------
      # Category Match Plot
      # ----------------------------------------------------
      
      if (isTRUE(self$options$categoryPlot)) {
        
        adjusted_totals <- vapply(
          seq_along(dict$categories),
          function(cat_i) {
            sn <- safe_names[cat_i]
            sum(adjusted_counts[[sn]])
          },
          numeric(1)
        )
        
        self$results$categoryPlot$setState(
          list(
            categories = dict$categories,
            adjustedMatches = adjusted_totals
          )
        )
      }
      
      # ----------------------------------------------------
      # Raw vs Adjusted Match Plot
      # ----------------------------------------------------
      
      if (isTRUE(self$options$negationImpactPlot)) {
        
        raw_totals <- vapply(
          seq_along(dict$categories),
          function(cat_i) {
            sn <- safe_names[cat_i]
            sum(raw_counts[[sn]])
          },
          numeric(1)
        )
        
        adjusted_totals <- vapply(
          seq_along(dict$categories),
          function(cat_i) {
            sn <- safe_names[cat_i]
            sum(adjusted_counts[[sn]])
          },
          numeric(1)
        )
        
        self$results$negationImpactPlot$setState(
          list(
            categories = dict$categories,
            rawMatches = raw_totals,
            adjustedMatches = adjusted_totals
          )
        )
      }      
      
      # ----------------------------------------------------
      # Notes
      # ----------------------------------------------------
      
      total_tokens <- sum(n_tokens)
      
      if (total_tokens == 0L) {
        
        cat_table$setNote(
          "noTokens",
          paste0(
            "No tokens were found in the ",
            "selected text variable."
          )
        )
        
      } else if (total_adjusted_all == 0L) {
        
        cat_table$setNote(
          "noHits",
          paste0(
            "The dictionary produced no ",
            "adjusted matches in the ",
            "selected texts."
          )
        )
      }
      
      
      # ----------------------------------------------------
      # Document-level table
      # ----------------------------------------------------
      
      if (isTRUE(
        self$options$documentDetails
      )) {
        
        detail_table <-
          self$results$documentDetails
        
        detail_table$deleteRows()
        
        if (length(detail_rows) > 0L) {
          
          for (i in seq_along(
            detail_rows
          )) {
            
            row <- detail_rows[[i]]
            
            detail_table$addRow(
              rowKey =
                paste0(
                  "doc_",
                  row$document,
                  "_",
                  i
                ),
              values = row
            )
          }
        }
      }
      
      
      # ----------------------------------------------------
      # Negation Evidence table
      # ----------------------------------------------------
      
      if (isTRUE(
        self$options$negationEvidence
      )) {
        
        ev_table <-
          self$results$negationEvidence
        
        ev_table$deleteRows()
        
        if (length(evidence_rows) > 0L) {
          
          for (i in seq_along(
            evidence_rows
          )) {
            
            row <-
              evidence_rows[[i]]
            
            ev_table$addRow(
              rowKey =
                paste0(
                  "neg_",
                  i
                ),
              values = row
            )
          }
          
        } else {
          
          ev_table$setNote(
            "noNegation",
            paste0(
              "No lexicon matches were ",
              "flagged by the current ",
              "negation rules."
            )
          )
        }
      }
      
      
      # ----------------------------------------------------
      # Status
      # ----------------------------------------------------
      
      total_raw <-
        sum(
          vapply(
            raw_counts,
            sum,
            numeric(1)
          )
        )
      
      total_negated <-
        sum(
          vapply(
            neg_counts,
            sum,
            numeric(1)
          )
        )
      
      total_adjusted <-
        sum(
          vapply(
            adjusted_counts,
            sum,
            numeric(1)
          )
        )
      
      status <- sprintf(
        paste0(
          "%d documents analysed, ",
          "%d categories, ",
          "%d total tokens, ",
          "%d raw matches, ",
          "%d negated matches, ",
          "%d adjusted matches."
        ),
        n_docs,
        length(dict$categories),
        total_tokens,
        total_raw,
        total_negated,
        total_adjusted
      )
      
      self$results$statusNote$setVisible(
        TRUE
      )
      
      self$results$statusNote$setContent(
        status
      )
      
      
      # ----------------------------------------------------
      # Save results to jamovi data set
      # ----------------------------------------------------
      
      if (isTRUE(
        self$options$saveResults
      ) &&
      self$results$saveResults$
      isNotFilled()) {
        
        
        output <- data.frame(
          n_tokens = n_tokens,
          n_types = n_types,
          stringsAsFactors = FALSE
        )
        
        
        for (cat_i in seq_along(
          dict$categories
        )) {
          
          sn <- safe_names[cat_i]
          
          output[[
            paste0(
              sn,
              "_raw_count"
            )
          ]] <- raw_counts[[sn]]
          
          output[[
            paste0(
              sn,
              "_negated_count"
            )
          ]] <- neg_counts[[sn]]
          
          output[[
            paste0(
              sn,
              "_adjusted_count"
            )
          ]] <- adjusted_counts[[sn]]
          
          output[[
            paste0(
              sn,
              "_adjusted_prop"
            )
          ]] <- percentages[[sn]]
          
          if (isTRUE(
            self$options$detectedWords
          )) {
            
            output[[
              paste0(
                sn,
                "_detected_terms"
              )
            ]] <-
              detected_terms[[sn]]
          }
        }
        
        
        keys <- names(output)
        titles <- keys
        
        
        descriptions <- vapply(
          
          keys,
          
          function(k) {
            
            if (k == "n_tokens") {
              
              return(
                paste0(
                  "Total number of tokens ",
                  "in the document"
                )
              )
            }
            
            if (k == "n_types") {
              
              return(
                paste0(
                  "Number of distinct tokens ",
                  "in the document"
                )
              )
            }
            
            if (grepl(
              "_raw_count$",
              k
            )) {
              
              return(
                paste0(
                  "Raw dictionary matches ",
                  "before negation adjustment"
                )
              )
            }
            
            if (grepl(
              "_negated_count$",
              k
            )) {
              
              return(
                paste0(
                  "Dictionary matches flagged ",
                  "by the negation rule"
                )
              )
            }
            
            if (grepl(
              "_adjusted_count$",
              k
            )) {
              
              return(
                paste0(
                  "Dictionary matches after ",
                  "the selected negation ",
                  "handling rule"
                )
              )
            }
            
            if (grepl(
              "_adjusted_perc$",
              k
            )) {
              
              return(
                paste0(
                  "Adjusted matches divided ",
                  "by document token count"
                )
              )
            }
            
            if (grepl(
              "_detected_terms$",
              k
            )) {
              
              return(
                paste0(
                  "Comma-separated surface ",
                  "forms matched for the ",
                  "dictionary category"
                )
              )
            }
            
            "Korean Lexicon Analysis result"
          },
          
          character(1)
        )
        
        
        measure_types <- ifelse(
          
          grepl(
            "_detected_terms$",
            keys
          ),
          
          "nominal",
          "continuous"
        )
        
        
        self$results$saveResults$set(
          keys,
          titles,
          descriptions,
          measure_types
        )
        
        
        self$results$saveResults$
          setRowNums(
            rownames(self$data)
          )
        
        
        for (k in keys) {
          
          self$results$saveResults$
            setValues(
              output[[k]],
              key = k
            )
        }
      }
      
      
      TRUE
    },
    
    
    # ================================================================
    # Category Match Plot
    # ================================================================
    
    .plotCategoryMatches = function(
    image,
    ggtheme,
    theme,
    ...
    ) {
      
      state <- image$state
      
      if (is.null(state) ||
          is.null(state$categories) ||
          is.null(state$adjustedMatches))
        return(FALSE)
      
      categories <- as.character(state$categories)
      adjustedMatches <- as.numeric(state$adjustedMatches)
      
      if (length(categories) == 0L ||
          length(categories) != length(adjustedMatches))
        return(FALSE)
      
      valid <- is.finite(adjustedMatches)
      
      categories <- categories[valid]
      adjustedMatches <- adjustedMatches[valid]
      
      if (length(categories) == 0L)
        return(FALSE)
      
      plotData <- data.frame(
        Category = factor(
          categories,
          levels = rev(categories)
        ),
        AdjustedMatches = adjustedMatches,
        stringsAsFactors = FALSE
      )
      
      palette <- grDevices::hcl.colors(
        max(3, length(categories)),
        palette = 'Dark 3'
      )[seq_along(categories)]
      
      names(palette) <- categories
      
      plot <- ggplot2::ggplot(
        plotData,
        ggplot2::aes(
          x = Category,
          y = AdjustedMatches,
          fill = Category
        )
      ) +
        ggplot2::geom_col(
          width = 0.72,
          show.legend = FALSE
        ) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = AdjustedMatches
          ),
          hjust = -0.15,
          size = 3.5
        ) +
        ggplot2::scale_fill_manual(
          values = palette
        ) +
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(
            mult = c(0, 0.12)
          )
        ) +
        ggplot2::coord_flip(
          clip = 'off'
        ) +
        ggplot2::labs(
          x = NULL,
          y = 'Adjusted Matches'
        ) +
        ggtheme +
        ggplot2::theme(
          legend.position = 'none',
          panel.grid.major.y = ggplot2::element_blank()
        )
      
      print(plot)
      TRUE
    },
    
    # ================================================================
    # Raw vs Adjusted Match Plot
    # ================================================================
    
    .plotNegationImpact = function(
    image,
    ggtheme,
    theme,
    ...
    ) {
      
      state <- image$state
      
      if (is.null(state) ||
          is.null(state$categories) ||
          is.null(state$rawMatches) ||
          is.null(state$adjustedMatches))
        return(FALSE)
      
      categories <- as.character(
        state$categories
      )
      
      rawMatches <- as.numeric(
        state$rawMatches
      )
      
      adjustedMatches <- as.numeric(
        state$adjustedMatches
      )
      
      if (length(categories) == 0L ||
          length(categories) != length(rawMatches) ||
          length(categories) != length(adjustedMatches))
        return(FALSE)
      
      valid <- is.finite(rawMatches) &
        is.finite(adjustedMatches)
      
      categories <- categories[valid]
      rawMatches <- rawMatches[valid]
      adjustedMatches <- adjustedMatches[valid]
      
      if (length(categories) == 0L)
        return(FALSE)
      
      plotData <- data.frame(
        Category = rep(
          categories,
          each = 2
        ),
        MatchType = factor(
          rep(
            c(
              "Raw",
              "Adjusted"
            ),
            times = length(categories)
          ),
          levels = c(
            "Raw",
            "Adjusted"
          )
        ),
        Matches = as.numeric(
          rbind(
            rawMatches,
            adjustedMatches
          )
        ),
        stringsAsFactors = FALSE
      )
      
      plotData$Category <- factor(
        plotData$Category,
        levels = rev(categories)
      )
      
      plot <- ggplot2::ggplot(
        plotData,
        ggplot2::aes(
          x = Category,
          y = Matches,
          fill = MatchType
        )
      ) +
        ggplot2::geom_col(
          position = ggplot2::position_dodge(
            width = 0.78
          ),
          width = 0.68
        ) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = Matches
          ),
          position = ggplot2::position_dodge(
            width = 0.78
          ),
          hjust = -0.15,
          size = 3.4
        ) +
        ggplot2::scale_fill_manual(
          values = c(
            "Raw" = "#7F8C8D",
            "Adjusted" = "#2F80ED"
          )
        ) +
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(
            mult = c(0, 0.15)
          )
        ) +
        ggplot2::coord_flip(
          clip = "off"
        ) +
        ggplot2::labs(
          x = NULL,
          y = "Matches",
          fill = NULL
        ) +
        ggtheme +
        ggplot2::theme(
          legend.position = "right",
          panel.grid.major.y =
            ggplot2::element_blank()
        )
      
      print(plot)
      
      TRUE
    }
  )
)
