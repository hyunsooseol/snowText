
lexicalClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "lexicalClass",
  inherit = lexicalBase,
  
  private = list(
    
    .htmlwidget = NULL,
    
    
    # ============================================================
    # Initialization
    # ============================================================
    
    .init = function() {
      
      private$.htmlwidget <- HTMLWidget$new()
      
      if (is.null(self$data) ||
          is.null(self$options$words) ||
          is.null(self$options$freq)) {
        
        self$results$instructions$setVisible(visible = TRUE)
      }
      
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
            
            '<p><strong>Lexical Distribution</strong> evaluates the ',
            'statistical structure of a word-frequency distribution.</p>',
            
            '<ul>',
            
            '<li><strong>Words</strong>: a word or lexical-item variable.</li>',
            
            '<li><strong>Frequency</strong>: a positive integer count ',
            'representing the number of occurrences of each word.</li>',
            
            '<li>If the same word appears in multiple rows, its frequencies ',
            'are summed before analysis.</li>',
            
            '<li>For Web Text Mining exports, ',
            '<strong>word_frequency_overall.csv</strong> is recommended ',
            'for overall lexical-distribution analysis.</li>',
            
            '<li>Feature requests and bug reports can be made on my ',
            '<a href="https://github.com/hyunsooseol/snowText/issues" ',
            'target="_blank">GitHub</a>.</li>',
            
            '</ul></div></div>'
          )
        )
      )
    },
    
    
    # ============================================================
    # Main analysis
    # ============================================================
    
    .run = function() {
      
      if (is.null(self$options$words) ||
          is.null(self$options$freq))
        return()
      
      dat <- private$.prepareData()
      
      if (is.null(dat) || nrow(dat) == 0)
        return()
      
      
      # ========================================================
      # 1. Vocabulary Structure
      # ========================================================
      
      if (isTRUE(self$options$vocabularyStructure)) {
        
        table <- self$results$vocabularyStructure
        table$deleteRows()
        
        V <- nrow(dat)
        N <- sum(dat$frequency)
        
        hapaxCount <- sum(dat$frequency == 1)
        disCount   <- sum(dat$frequency == 2)
        
        hapaxRatio <- if (V > 0)
          hapaxCount / V
        else
          NA_real_
        
        disRatio <- if (V > 0)
          disCount / V
        else
          NA_real_
        
        table$addRow(
          rowKey = "summary",
          values = list(
            vocabularySize = V,
            totalTokens    = N,
            hapaxCount     = hapaxCount,
            hapaxRatio     = hapaxRatio,
            disCount       = disCount,
            disRatio       = disRatio
          )
        )
        
        table$setNote(
          key = "vocabulary",
          note = paste(
            "Vocabulary Size is the number of unique words after",
            "duplicate word rows are combined."
          )
        )
        
        table$setNote(
          key = "hapax",
          note = paste(
            "Hapax counts words occurring exactly once.",
            "Hapax Ratio = Hapax / Vocabulary Size."
          )
        )
        
        table$setNote(
          key = "dis",
          note = paste(
            "Dis Legomena counts words occurring exactly twice.",
            "Dis Ratio = Dis Legomena / Vocabulary Size."
          )
        )
      }
      
      
      # ========================================================
      # 2. Distribution Concentration
      # ========================================================
      
      if (isTRUE(self$options$concentration)) {
        
        table <- self$results$concentration
        table$deleteRows()
        
        V <- nrow(dat)
        N <- sum(dat$frequency)
        
        p <- dat$frequency / N
        
        shannon <- -sum(p * log(p))
        
        normalizedEntropy <- if (V > 1)
          shannon / log(V)
        else
          0
        
        gini <- private$.gini(dat$frequency)
        
        simpson <- sum(p^2)
        
        table$addRow(
          rowKey = "summary",
          values = list(
            shannon           = shannon,
            normalizedEntropy = normalizedEntropy,
            gini              = gini,
            simpson           = simpson
          )
        )
        
        table$setNote(
          key = "entropy",
          note = paste(
            "Shannon H = -Σ pᵢ ln(pᵢ), where pᵢ is the relative",
            "frequency of word i. Natural logarithms are used."
          )
        )
        
        table$setNote(
          key = "normalizedEntropy",
          note = paste(
            "Normalized Entropy = Shannon H / ln(V), where V is",
            "Vocabulary Size. Values range from 0 to 1;",
            "larger values indicate a more even distribution."
          )
        )
        
        table$setNote(
          key = "gini",
          note = paste(
            "Gini ranges from 0 toward 1.",
            "Larger values indicate greater inequality in word frequencies."
          )
        )
        
        table$setNote(
          key = "simpson",
          note = paste(
            "Simpson Concentration = Σ pᵢ².",
            "Larger values indicate stronger concentration in",
            "a smaller number of words."
          )
        )
      }
      
      
      # ========================================================
      # 3. Coverage Analysis
      # ========================================================
      
      if (isTRUE(self$options$coverage)) {
        
        table <- self$results$coverage
        table$deleteRows()
        
        freq <- sort(
          dat$frequency,
          decreasing = TRUE
        )
        
        total <- sum(freq)
        
        cumulative <- cumsum(freq) / total
        
        top10N <- min(10L, length(freq))
        top20N <- min(20L, length(freq))
        
        top10 <- sum(freq[seq_len(top10N)]) / total * 100
        top20 <- sum(freq[seq_len(top20N)]) / total * 100
        
        coverage50 <- private$.coverageWords(
          cumulative,
          0.50
        )
        
        coverage80 <- private$.coverageWords(
          cumulative,
          0.80
        )
        
        coverage90 <- private$.coverageWords(
          cumulative,
          0.90
        )
        
        table$addRow(
          rowKey = "summary",
          values = list(
            top10      = top10,
            top20      = top20,
            coverage50 = coverage50,
            coverage80 = coverage80,
            coverage90 = coverage90
          )
        )
        
        table$setNote(
          key = "top",
          note = paste(
            "Top 10 and Top 20 are the percentages of all word tokens",
            "accounted for by the 10 and 20 most frequent words,",
            "respectively."
          )
        )
        
        table$setNote(
          key = "coverage",
          note = paste(
            "Words for 50%, 80%, and 90% are the minimum numbers",
            "of highest-frequency words required to reach the",
            "corresponding cumulative proportion of all tokens."
          )
        )
      }
      
      
      # ========================================================
      # 4. Zipf Diagnostics
      # ========================================================
      
      if (isTRUE(self$options$zipf)) {
        
        table <- self$results$zipf
        table$deleteRows()
        
        zipf <- private$.zipfStatistics(dat)
        
        table$addRow(
          rowKey = "summary",
          values = list(
            n         = zipf$n,
            slope     = zipf$slope,
            intercept = zipf$intercept,
            r2        = zipf$r2
          )
        )
        
        table$setNote(
          key = "model",
          note = paste(
            "Zipf diagnostics are based on the linear regression",
            "ln(frequency) = intercept + slope × ln(rank)."
          )
        )
        
        table$setNote(
          key = "interpretation",
          note = paste(
            "A slope near -1 is often associated with a Zipf-like",
            "rank-frequency pattern, but no fixed goodness or",
            "normality threshold is imposed by this analysis."
          )
        )
        
        table$setNote(
          key = "r2",
          note = paste(
            "R² describes how closely the observed log rank-frequency",
            "relationship follows the fitted straight line."
          )
        )
      }
      
      
      # ========================================================
      # Plot states
      # ========================================================
      
      # --------------------------------------------------------
      # Rank-Frequency Plot
      # --------------------------------------------------------
      
      if (isTRUE(self$options$rankFrequencyPlot)) {
        
        rankData <- dat
        
        rankData$rank <- seq_len(nrow(rankData))
        
        zipf <- private$.zipfStatistics(rankData)
        
        fittedFrequency <- rep(
          NA_real_,
          nrow(rankData)
        )
        
        if (is.finite(zipf$slope) &&
            is.finite(zipf$intercept)) {
          
          fittedFrequency <- exp(
            zipf$intercept +
              zipf$slope * log(rankData$rank)
          )
        }
        
        self$results$rankFrequencyPlot$setState(
          list(
            rank = rankData$rank,
            frequency = rankData$frequency,
            fitted = fittedFrequency
          )
        )
      }
      
      
      # --------------------------------------------------------
      # Lorenz Curve
      # --------------------------------------------------------
      
      if (isTRUE(self$options$lorenzPlot)) {
        
        lorenzFreq <- sort(
          dat$frequency,
          decreasing = FALSE
        )
        
        nLorenz <- length(lorenzFreq)
        totalLorenz <- sum(lorenzFreq)
        
        cumulativeWords <- c(
          0,
          seq_len(nLorenz) / nLorenz
        )
        
        cumulativeFrequency <- c(
          0,
          cumsum(lorenzFreq) / totalLorenz
        )
        
        self$results$lorenzPlot$setState(
          list(
            cumulativeWords = cumulativeWords,
            cumulativeFrequency = cumulativeFrequency,
            gini = private$.gini(dat$frequency)
          )
        )
      }
      
      
      # --------------------------------------------------------
      # Frequency Spectrum
      # --------------------------------------------------------
      
      if (isTRUE(self$options$frequencySpectrumPlot)) {
        
        frequencies <- sort(
          unique(dat$frequency)
        )
        
        wordTypes <- vapply(
          frequencies,
          function(f) {
            sum(dat$frequency == f)
          },
          numeric(1)
        )
        
        self$results$frequencySpectrumPlot$setState(
          list(
            frequency = frequencies,
            wordTypes = wordTypes
          )
        )
      }
    },
    
    
    # ============================================================
    # Data preparation
    # ============================================================
    
    .prepareData = function() {
      
      wordsVar <- self$options$words
      freqVar  <- self$options$freq
      
      data <- self$data
      
      if (is.null(data) ||
          is.null(wordsVar) ||
          is.null(freqVar))
        return(NULL)
      
      if (! wordsVar %in% names(data) ||
          ! freqVar %in% names(data))
        return(NULL)
      
      
      words <- as.character(
        data[[wordsVar]]
      )
      
      freq <- suppressWarnings(
        as.numeric(
          as.character(
            data[[freqVar]]
          )
        )
      )
      
      
      valid <- !is.na(words) &
        nzchar(trimws(words)) &
        is.finite(freq)
      
      words <- trimws(
        words[valid]
      )
      
      freq <- freq[valid]
      
      
      if (length(words) == 0)
        return(NULL)
      
      
      integerTolerance <- sqrt(
        .Machine$double.eps
      )
      
      nonInteger <- abs(
        freq - round(freq)
      ) > integerTolerance
      
      
      if (any(nonInteger)) {
        
        stop(
          paste(
            "Frequency must contain positive integer counts.",
            "Decimal frequency values were detected."
          )
        )
      }
      
      
      if (any(freq <= 0)) {
        
        stop(
          paste(
            "Frequency must contain positive integer counts.",
            "Zero or negative values were detected."
          )
        )
      }
      
      
      freq <- as.numeric(
        round(freq)
      )
      
      
      raw <- data.frame(
        word = words,
        frequency = freq,
        stringsAsFactors = FALSE
      )
      
      
      aggregated <- stats::aggregate(
        frequency ~ word,
        data = raw,
        FUN = sum
      )
      
      
      aggregated$frequency <- as.numeric(
        aggregated$frequency
      )
      
      
      aggregated <- aggregated[
        order(
          -aggregated$frequency,
          aggregated$word
        ),
        ,
        drop = FALSE
      ]
      
      
      rownames(aggregated) <- NULL
      
      aggregated
    },
    
    
    # ============================================================
    # Gini coefficient
    # ============================================================
    
    .gini = function(x) {
      
      x <- as.numeric(x)
      
      x <- x[
        is.finite(x) &
          x >= 0
      ]
      
      
      n <- length(x)
      
      if (n == 0)
        return(NA_real_)
      
      
      total <- sum(x)
      
      if (! is.finite(total) ||
          total <= 0)
        return(NA_real_)
      
      
      if (n == 1)
        return(0)
      
      
      x <- sort(
        x,
        decreasing = FALSE
      )
      
      
      i <- seq_len(n)
      
      
      g <- sum(
        (2 * i - n - 1) * x
      ) / (n * total)
      
      
      g <- max(
        0,
        min(
          1,
          g
        )
      )
      
      
      g
    },
    
    
    # ============================================================
    # Coverage threshold
    # ============================================================
    
    .coverageWords = function(
    cumulative,
    threshold
    ) {
      
      index <- which(
        cumulative >= threshold
      )
      
      
      if (length(index) == 0)
        return(NA_integer_)
      
      
      as.integer(
        index[1]
      )
    },
    
    
    # ============================================================
    # Zipf statistics
    # ============================================================
    
    .zipfStatistics = function(dat) {
      
      freq <- sort(
        dat$frequency,
        decreasing = TRUE
      )
      
      
      n <- length(freq)
      
      
      result <- list(
        n = n,
        slope = NA_real_,
        intercept = NA_real_,
        r2 = NA_real_
      )
      
      
      if (n < 2)
        return(result)
      
      
      rank <- seq_len(n)
      
      logRank <- log(rank)
      logFreq <- log(freq)
      
      
      if (length(unique(logFreq)) < 2)
        return(result)
      
      
      fit <- tryCatch(
        stats::lm(
          logFreq ~ logRank
        ),
        error = function(e) NULL
      )
      
      
      if (is.null(fit))
        return(result)
      
      
      coefficients <- stats::coef(fit)
      
      
      if (length(coefficients) >= 2) {
        
        result$intercept <- unname(
          coefficients[1]
        )
        
        result$slope <- unname(
          coefficients[2]
        )
      }
      
      
      fitSummary <- summary(fit)
      
      r2 <- fitSummary$r.squared
      
      
      if (is.finite(r2))
        result$r2 <- r2
      
      
      result
    },
    
    
    # ============================================================
    # Rank-Frequency Plot
    # ============================================================
    
    .plotRankFrequency = function(
    image,
    ggtheme,
    theme,
    ...
    ) {
      
      state <- image$state
      
      if (is.null(state))
        return(FALSE)
      
      if (is.null(state$rank) ||
          is.null(state$frequency))
        return(FALSE)
      
      
      rank <- as.numeric(
        state$rank
      )
      
      frequency <- as.numeric(
        state$frequency
      )
      
      
      valid <- is.finite(rank) &
        is.finite(frequency) &
        rank > 0 &
        frequency > 0
      
      
      rank <- rank[valid]
      frequency <- frequency[valid]
      
      
      if (length(rank) == 0)
        return(FALSE)
      
      
      plotData <- data.frame(
        Rank = rank,
        Frequency = frequency
      )
      
      
      plot <- ggplot2::ggplot(
        plotData,
        ggplot2::aes(
          x = Rank,
          y = Frequency
        )
      ) +
        ggplot2::geom_point(
          size = 2,
          alpha = 0.75
        ) +
        ggplot2::scale_x_log10() +
        ggplot2::scale_y_log10() +
        ggplot2::labs(
          x = "Word Rank (log scale)",
          y = "Frequency (log scale)"
        )
      
      
      if (!is.null(state$fitted)) {
        
        fitted <- as.numeric(
          state$fitted
        )
        
        fitted <- fitted[valid]
        
        fitValid <- is.finite(fitted) &
          fitted > 0
        
        
        if (any(fitValid)) {
          
          fitData <- data.frame(
            Rank = rank[fitValid],
            Frequency = fitted[fitValid]
          )
          
          
          plot <- plot +
            ggplot2::geom_line(
              data = fitData,
              ggplot2::aes(
                x = Rank,
                y = Frequency
              ),
              inherit.aes = FALSE,
              linewidth = 0.8
            )
        }
      }
      
      
      plot <- plot + ggtheme
      
      
      print(plot)
      
      TRUE
    },
    
    
    # ============================================================
    # Lorenz Curve
    # ============================================================
    
    .plotLorenz = function(
    image,
    ggtheme,
    theme,
    ...
    ) {
      
      state <- image$state
      
      
      if (is.null(state))
        return(FALSE)
      
      
      if (is.null(state$cumulativeWords) ||
          is.null(state$cumulativeFrequency))
        return(FALSE)
      
      
      cumulativeWords <- as.numeric(
        state$cumulativeWords
      )
      
      cumulativeFrequency <- as.numeric(
        state$cumulativeFrequency
      )
      
      
      
      gini <- if (!is.null(state$gini))
        as.numeric(state$gini)
      else
        NA_real_
      
      valid <- is.finite(cumulativeWords) &
        is.finite(cumulativeFrequency)
      
      
      cumulativeWords <- cumulativeWords[valid]
      cumulativeFrequency <- cumulativeFrequency[valid]
      
      
      if (length(cumulativeWords) < 2)
        return(FALSE)
      
      
      plotData <- data.frame(
        Words = cumulativeWords,
        Frequency = cumulativeFrequency
      )
      
      
      plot <- ggplot2::ggplot(
        plotData,
        ggplot2::aes(
          x = Words,
          y = Frequency
        )
      ) +
        ggplot2::geom_abline(
          intercept = 0,
          slope = 1,
          linetype = "dashed"
        ) +
        ggplot2::geom_line(
          linewidth = 1
        ) +
        ggplot2::labs(
          x = "Cumulative Proportion of Words",
          y = "Cumulative Proportion of Frequency",
          subtitle = if (is.finite(gini))
            sprintf("Gini = %.3f", gini)
          else
            NULL
        ) +
        ggplot2::coord_equal(
          xlim = c(0, 1),
          ylim = c(0, 1)
        )
      
      
      plot <- plot + ggtheme
      
      
      print(plot)
      
      TRUE
    },
    
    
    # ============================================================
    # Frequency Spectrum
    # ============================================================
    
    .plotFrequencySpectrum = function(
    image,
    ggtheme,
    theme,
    ...
    ) {
      
      state <- image$state
      
      
      if (is.null(state))
        return(FALSE)
      
      
      if (is.null(state$frequency) ||
          is.null(state$wordTypes))
        return(FALSE)
      
      
      frequency <- as.numeric(
        state$frequency
      )
      
      wordTypes <- as.numeric(
        state$wordTypes
      )
      
      
      valid <- is.finite(frequency) &
        is.finite(wordTypes)
      
      
      frequency <- frequency[valid]
      wordTypes <- wordTypes[valid]
      
      
      if (length(frequency) == 0)
        return(FALSE)
      
      
      spectrum <- data.frame(
        Frequency = factor(
          as.character(frequency),
          levels = as.character(frequency)
        ),
        WordTypes = wordTypes
      )
      
      
      plot <- ggplot2::ggplot(
        spectrum,
        ggplot2::aes(
          x = Frequency,
          y = WordTypes
        )
      ) +
        ggplot2::geom_col() +
        ggplot2::labs(
          x = "Frequency",
          y = "Number of Word Types"
        )
      
      
      plot <- plot + ggtheme
      
      
      print(plot)
      
      TRUE
    }
    
  )
)