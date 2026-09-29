
lexicalComparisonClass <- if (
  requireNamespace('jmvcore', quietly = TRUE)
) R6::R6Class(
  
  "lexicalComparisonClass",
  
  inherit = lexicalComparisonBase,
  
  private = list(
    
    .htmlwidget = NULL,
    
    
    # ============================================================
    # Initialization
    # ============================================================
    
    .init = function() {
      
      private$.htmlwidget <- HTMLWidget$new()
      
      if (is.null(self$data) ||
          is.null(self$options$words) ||
          is.null(self$options$freq) ||
          is.null(self$options$group)) {
        
        self$results$instructions$setVisible(
          visible = TRUE
        )
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
            
            '<p><strong>Lexical Distribution Comparison</strong> ',
            'compares word-frequency distributions across two ',
            'or more groups.</p>',
            
            '<ul>',
            
            '<li><strong>Words</strong>: a word or lexical-item ',
            'variable.</li>',
            
            '<li><strong>Frequency</strong>: a positive integer ',
            'count representing the number of occurrences of ',
            'each word.</li>',
            
            '<li><strong>Group</strong>: a categorical variable ',
            'identifying the corpus or group to which each ',
            'word-frequency row belongs.</li>',
            
            '<li>If the same word occurs more than once within ',
            'the same group, frequencies are summed before ',
            'analysis.</li>',
            
            '<li>Words that do not occur in one of the compared ',
            'groups are treated as having frequency zero in ',
            'that group.</li>',
            
            '<li>For Web Text Mining exports, ',
            '<strong>word_frequency_group.csv</strong> is ',
            'recommended.</li>',
            
            '<li>Feature requests and bug reports can be made on ',
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
          is.null(self$options$freq) ||
          is.null(self$options$group))
        return()
      
      
      dat <- private$.prepareData()
      
      
      if (is.null(dat) ||
          nrow(dat) == 0)
        return()
      
      
      groups <- unique(
        dat$group
      )
      
      
      if (length(groups) < 2) {
        
        stop(
          paste(
            "Group must contain at least two valid groups.",
            "Lexical Distribution Comparison requires",
            "two or more groups."
          )
        )
      }
      
      
      # ========================================================
      # Prepare all pairwise comparisons once
      # ========================================================
      
      groupPairs <- utils::combn(
        groups,
        2,
        simplify = FALSE
      )
      
      
      pairResults <- lapply(
        
        groupPairs,
        
        function(pair) {
          
          private$.pairStatistics(
            dat = dat,
            group1 = pair[1],
            group2 = pair[2]
          )
        }
      )
      
      
      # ========================================================
      # 1. Distribution Difference
      # ========================================================
      
      if (isTRUE(
        self$options$distributionDifference
      )) {
        
        table <- self$results$distributionDifference
        
        table$deleteRows()
        
        
        for (i in seq_along(pairResults)) {
          
          result <- pairResults[[i]]
          
          table$addRow(
            
            rowKey = paste0(
              "pair_",
              i
            ),
            
            values = list(
              
              group1 = result$group1,
              
              group2 = result$group2,
              
              jsd = result$jsd,
              
              hellinger = result$hellinger,
              
              brayCurtis = result$brayCurtis
            )
          )
        }
        
        
        table$setNote(
          
          key = "jsd",
          
          note = paste(
            "Jensen-Shannon divergence compares the",
            "complete relative-frequency distributions.",
            "Base-2 logarithms are used, so values range",
            "from 0 to 1. A value of 0 indicates identical",
            "distributions."
          )
        )
        
        
        table$setNote(
          
          key = "hellinger",
          
          note = paste(
            "Hellinger distance ranges from 0 to 1.",
            "A value of 0 indicates identical",
            "relative-frequency distributions;",
            "larger values indicate greater separation."
          )
        )
        
        
        table$setNote(
          
          key = "bray",
          
          note = paste(
            "Bray-Curtis dissimilarity is calculated from",
            "normalized word proportions rather than raw",
            "token counts, so differences in corpus size",
            "alone do not determine the result.",
            "Values range from 0 to 1."
          )
        )
      }
      
      
      # ========================================================
      # 2. Vocabulary Overlap
      # ========================================================
      
      if (isTRUE(
        self$options$vocabularyOverlap
      )) {
        
        table <- self$results$vocabularyOverlap
        
        table$deleteRows()
        
        
        for (i in seq_along(pairResults)) {
          
          result <- pairResults[[i]]
          
          table$addRow(
            
            rowKey = paste0(
              "overlap_",
              i
            ),
            
            values = list(
              
              group1 = result$group1,
              
              group2 = result$group2,
              
              vocabulary1 = result$vocabulary1,
              
              vocabulary2 = result$vocabulary2,
              
              sharedWords = result$sharedWords,
              
              unionWords = result$unionWords,
              
              jaccard = result$jaccard,
              
              sharedPct = result$sharedPct
            )
          )
        }
        
        
        table$setNote(
          
          key = "jaccard",
          
          note = paste(
            "Jaccard Similarity = Shared Words / Union Words.",
            "Values range from 0 to 1, with larger values",
            "indicating greater vocabulary overlap."
          )
        )
        
        
        table$setNote(
          
          key = "shared",
          
          note = paste(
            "Shared Vocabulary (%) is the percentage of",
            "the smaller vocabulary that is also present",
            "in the other group:",
            "Shared Words / min(Vocabulary 1, Vocabulary 2) × 100."
          )
        )
      }
      
      
      # ========================================================
      # 3. Concentration Difference
      # ========================================================
      
      if (isTRUE(
        self$options$concentrationDifference
      )) {
        
        table <- self$results$concentrationDifference
        
        table$deleteRows()
        
        
        for (i in seq_along(groups)) {
          
          groupName <- groups[i]
          
          groupData <- dat[
            dat$group == groupName,
            ,
            drop = FALSE
          ]
          
          
          stats <- private$.distributionStatistics(
            groupData$frequency
          )
          
          
          table$addRow(
            
            rowKey = paste0(
              "group_",
              i
            ),
            
            values = list(
              
              group = groupName,
              
              vocabulary = stats$vocabulary,
              
              tokens = stats$tokens,
              
              entropy = stats$entropy,
              
              normalizedEntropy =
                stats$normalizedEntropy,
              
              gini = stats$gini,
              
              simpson = stats$simpson
            )
          )
        }
        
        
        table$setNote(
          
          key = "concentration",
          
          note = paste(
            "These statistics describe the lexical",
            "distribution within each group separately.",
            "They allow researchers to compare vocabulary",
            "size, diversity, evenness, and concentration",
            "across groups."
          )
        )
        
        
        table$setNote(
          
          key = "entropy",
          
          note = paste(
            "Shannon H = -Σ pᵢ log(pᵢ), using natural",
            "logarithms. Normalized Entropy = H / ln(V)."
          )
        )
        
        
        table$setNote(
          
          key = "gini",
          
          note = paste(
            "Gini ranges from 0 toward 1.",
            "Larger values indicate greater inequality",
            "in word frequencies."
          )
        )
        
        
        table$setNote(
          
          key = "simpson",
          
          note = paste(
            "Simpson Concentration = Σ pᵢ².",
            "Larger values indicate stronger concentration",
            "in a smaller number of words."
          )
        )
      }
      
      
      # ========================================================
      # 4. Difference Breakdown
      # ========================================================
      
      if (isTRUE(
        self$options$contribution
      )) {
        
        table <- self$results$contribution
        
        table$deleteRows()
        
        
        for (i in seq_along(pairResults)) {
          
          result <- pairResults[[i]]
          
          contributionData <- result$contribution
          
          group1Only <-
            contributionData$probability1 > 0 &
            contributionData$probability2 == 0
          
          group2Only <-
            contributionData$probability1 == 0 &
            contributionData$probability2 > 0
          
          shared <-
            contributionData$probability1 > 0 &
            contributionData$probability2 > 0
          
          categories <- list(
            group1Only = group1Only,
            group2Only = group2Only,
            shared = shared
          )
          
          categoryLabels <- c(
            group1Only = "Group 1 only",
            group2Only = "Group 2 only",
            shared = "Shared"
          )
          
          totalContribution <- sum(
            contributionData$contribution
          )
          
          for (categoryKey in names(categories)) {
            
            selected <- categories[[categoryKey]]
            
            categoryContribution <- sum(
              contributionData$contribution[selected]
            )
            
            contributionPct <- if (
              totalContribution > 0
            ) {
              100 * categoryContribution /
                totalContribution
            } else {
              NA_real_
            }
            
            table$addRow(
              
              rowKey = paste0(
                "breakdown_",
                i,
                "_",
                categoryKey
              ),
              
              values = list(
                group1 = result$group1,
                group2 = result$group2,
                category = categoryLabels[[categoryKey]],
                wordCount = sum(selected),
                jsContribution = categoryContribution,
                contributionPct = contributionPct
              )
            )
          }
        }
        
        table$setNote(
          key = "jsContribution",
          note = paste(
            "JS Contribution is the sum of the word-level",
            "contributions within each category. The three",
            "categories partition the full vocabulary, so",
            "their values sum to the reported Jensen-Shannon",
            "divergence for each group pair."
          )
        )
        
        table$setNote(
          key = "contributionPct",
          note = paste(
            "Share of JS (%) is the category contribution",
            "divided by the total Jensen-Shannon divergence",
            "for the group pair, multiplied by 100.",
            "When the distributions are identical and",
            "the divergence is zero, shares are undefined."
          )
        )
      }
      
      
      # ========================================================
      # Plot states
      # ========================================================
      
      
      # --------------------------------------------------------
      # Distribution Distance Plot
      # --------------------------------------------------------
      
      if (isTRUE(
        self$options$distancePlot
      )) {
        
        pairLabels <- vapply(
          
          pairResults,
          
          function(x) {
            
            paste(
              x$group1,
              "vs",
              x$group2
            )
          },
          
          character(1)
        )
        
        
        jsd <- vapply(
          pairResults,
          function(x) x$jsd,
          numeric(1)
        )
        
        
        hellinger <- vapply(
          pairResults,
          function(x) x$hellinger,
          numeric(1)
        )
        
        
        brayCurtis <- vapply(
          pairResults,
          function(x) x$brayCurtis,
          numeric(1)
        )
        
        
        self$results$distancePlot$setState(
          
          list(
            
            pair = pairLabels,
            
            jsd = jsd,
            
            hellinger = hellinger,
            
            brayCurtis = brayCurtis
          )
        )
      }
      
      
      # --------------------------------------------------------
      # Contribution Plot
      # --------------------------------------------------------
      
      if (isTRUE(
        self$options$contributionPlot
      )) {
        
        plotFrames <- list()
        
        
        for (i in seq_along(pairResults)) {
          
          result <- pairResults[[i]]
          
          temp <- result$contribution
          
          
          temp <- temp[
            order(
              -temp$contribution,
              temp$word
            ),
            ,
            drop = FALSE
          ]
          
          
          topN <- min(
            10L,
            nrow(temp)
          )
          
          
          if (topN > 0) {
            
            temp <- temp[
              seq_len(topN),
              ,
              drop = FALSE
            ]
            
            
            temp$pair <- paste(
              result$group1,
              "vs",
              result$group2
            )
            
            
            plotFrames[[length(plotFrames) + 1L]] <-
              temp
          }
        }
        
        
        if (length(plotFrames) > 0) {
          
          plotData <- do.call(
            rbind,
            plotFrames
          )
          
          
          self$results$contributionPlot$setState(
            
            list(
              
              pair = plotData$pair,
              
              word = plotData$word,
              
              contribution =
                plotData$contribution
            )
          )
        }
      }
      
      
      # --------------------------------------------------------
      # Differential Vocabulary Map
      # --------------------------------------------------------
      
      if (isTRUE(
        self$options$differentialVocabularyPlot
      )) {
        
        plotFrames <- list()
        
        
        for (i in seq_along(pairResults)) {
          
          result <- pairResults[[i]]
          
          temp <- result$contribution
          
          
          temp$absDifference <- abs(
            temp$difference
          )
          
          
          temp <- temp[
            order(
              -temp$absDifference,
              temp$word
            ),
            ,
            drop = FALSE
          ]
          
          
          topN <- min(
            10L,
            nrow(temp)
          )
          
          
          if (topN > 0) {
            
            temp <- temp[
              seq_len(topN),
              ,
              drop = FALSE
            ]
            
            
            temp$pair <- paste(
              result$group1,
              "vs",
              result$group2
            )
            
            
            temp$direction <- ifelse(
              temp$difference >= 0,
              result$group1,
              result$group2
            )
            
            
            plotFrames[[length(plotFrames) + 1L]] <-
              temp
          }
        }
        
        
        if (length(plotFrames) > 0) {
          
          plotData <- do.call(
            rbind,
            plotFrames
          )
          
          
          self$results$differentialVocabularyPlot$setState(
            
            list(
              
              pair = plotData$pair,
              
              word = plotData$word,
              
              difference = plotData$difference,
              
              direction = plotData$direction
            )
          )
        }
      }
      
    },
    
    
    # ============================================================
    # Data preparation
    # ============================================================
    
    .prepareData = function() {
      
      wordsVar <- self$options$words
      
      freqVar <- self$options$freq
      
      groupVar <- self$options$group
      
      data <- self$data
      
      
      if (is.null(data) ||
          is.null(wordsVar) ||
          is.null(freqVar) ||
          is.null(groupVar))
        return(NULL)
      
      
      if (! wordsVar %in% names(data) ||
          ! freqVar %in% names(data) ||
          ! groupVar %in% names(data))
        return(NULL)
      
      
      words <- as.character(
        data[[wordsVar]]
      )
      
      
      groups <- as.character(
        data[[groupVar]]
      )
      
      
      freq <- suppressWarnings(
        
        as.numeric(
          
          as.character(
            data[[freqVar]]
          )
        )
      )
      
      
      valid <-
        
        !is.na(words) &
        
        nzchar(
          trimws(words)
        ) &
        
        !is.na(groups) &
        
        nzchar(
          trimws(groups)
        ) &
        
        is.finite(freq)
      
      
      words <- trimws(
        words[valid]
      )
      
      
      groups <- trimws(
        groups[valid]
      )
      
      
      freq <- freq[valid]
      
      
      if (length(words) == 0)
        return(NULL)
      
      
      # --------------------------------------------------------
      # Frequency must be positive integer count
      # --------------------------------------------------------
      
      integerTolerance <-
        sqrt(.Machine$double.eps)
      
      
      nonInteger <-
        
        abs(
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
        
        group = groups,
        
        frequency = freq,
        
        stringsAsFactors = FALSE
      )
      
      
      # --------------------------------------------------------
      # Combine duplicate word rows within each group
      # --------------------------------------------------------
      
      aggregated <- stats::aggregate(
        
        frequency ~ group + word,
        
        data = raw,
        
        FUN = sum
      )
      
      
      aggregated$frequency <-
        as.numeric(
          aggregated$frequency
        )
      
      
      aggregated <- aggregated[
        order(
          aggregated$group,
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
    # Pairwise lexical statistics
    # ============================================================
    
    .pairStatistics = function(
    dat,
    group1,
    group2
    ) {
      
      dat1 <- dat[
        dat$group == group1,
        ,
        drop = FALSE
      ]
      
      
      dat2 <- dat[
        dat$group == group2,
        ,
        drop = FALSE
      ]
      
      
      words1 <- dat1$word
      
      words2 <- dat2$word
      
      
      unionWords <- sort(
        union(
          words1,
          words2
        )
      )
      
      
      frequency1 <- rep(
        0,
        length(unionWords)
      )
      
      
      frequency2 <- rep(
        0,
        length(unionWords)
      )
      
      
      index1 <- match(
        dat1$word,
        unionWords
      )
      
      
      index2 <- match(
        dat2$word,
        unionWords
      )
      
      
      frequency1[index1] <-
        dat1$frequency
      
      
      frequency2[index2] <-
        dat2$frequency
      
      
      total1 <- sum(
        frequency1
      )
      
      
      total2 <- sum(
        frequency2
      )
      
      
      p <- frequency1 / total1
      
      q <- frequency2 / total2
      
      
      # --------------------------------------------------------
      # Jensen-Shannon divergence
      # --------------------------------------------------------
      
      m <- (p + q) / 2
      
      
      contributionP <- rep(
        0,
        length(p)
      )
      
      
      contributionQ <- rep(
        0,
        length(q)
      )
      
      
      validP <- p > 0 & m > 0
      
      validQ <- q > 0 & m > 0
      
      
      contributionP[validP] <-
        
        0.5 *
        p[validP] *
        log2(
          p[validP] /
            m[validP]
        )
      
      
      contributionQ[validQ] <-
        
        0.5 *
        q[validQ] *
        log2(
          q[validQ] /
            m[validQ]
        )
      
      
      jsContribution <-
        
        contributionP +
        contributionQ
      
      
      jsd <- sum(
        jsContribution
      )
      
      
      # numerical protection
      jsd <- max(
        0,
        min(
          1,
          jsd
        )
      )
      
      
      # --------------------------------------------------------
      # Hellinger distance
      # --------------------------------------------------------
      
      hellinger <-
        
        sqrt(
          sum(
            (
              sqrt(p) -
                sqrt(q)
            )^2
          )
        ) /
        sqrt(2)
      
      
      hellinger <- max(
        0,
        min(
          1,
          hellinger
        )
      )
      
      
      # --------------------------------------------------------
      # Bray-Curtis dissimilarity
      #
      # Relative-frequency distributions are used.
      # Since sum(p)=sum(q)=1:
      #
      # BC = sum(|p-q|) / sum(p+q)
      #
      # --------------------------------------------------------
      
      denominator <-
        sum(p + q)
      
      
      brayCurtis <- if (
        denominator > 0
      ) {
        
        sum(
          abs(p - q)
        ) /
          denominator
        
      } else {
        
        NA_real_
      }
      
      
      if (is.finite(brayCurtis)) {
        
        brayCurtis <- max(
          0,
          min(
            1,
            brayCurtis
          )
        )
      }
      
      
      # --------------------------------------------------------
      # Vocabulary overlap
      # --------------------------------------------------------
      
      shared <- intersect(
        words1,
        words2
      )
      
      
      vocabulary1 <-
        length(
          unique(words1)
        )
      
      
      vocabulary2 <-
        length(
          unique(words2)
        )
      
      
      sharedWords <-
        length(shared)
      
      
      unionCount <-
        length(unionWords)
      
      
      jaccard <- if (
        unionCount > 0
      ) {
        
        sharedWords /
          unionCount
        
      } else {
        
        NA_real_
      }
      
      
      smallerVocabulary <- min(
        vocabulary1,
        vocabulary2
      )
      
      
      sharedPct <- if (
        smallerVocabulary > 0
      ) {
        
        sharedWords /
          smallerVocabulary *
          100
        
      } else {
        
        NA_real_
      }
      
      
      # --------------------------------------------------------
      # Per-word contribution table
      # --------------------------------------------------------
      
      contributionData <- data.frame(
        
        word = unionWords,
        
        probability1 = p,
        
        probability2 = q,
        
        difference = p - q,
        
        contribution = jsContribution,
        
        stringsAsFactors = FALSE
      )
      
      
      list(
        
        group1 = group1,
        
        group2 = group2,
        
        jsd = jsd,
        
        hellinger = hellinger,
        
        brayCurtis = brayCurtis,
        
        vocabulary1 = vocabulary1,
        
        vocabulary2 = vocabulary2,
        
        sharedWords = sharedWords,
        
        unionWords = unionCount,
        
        jaccard = jaccard,
        
        sharedPct = sharedPct,
        
        contribution =
          contributionData
      )
    },
    
    
    # ============================================================
    # Distribution statistics for one group
    # ============================================================
    
    .distributionStatistics = function(
    frequency
    ) {
      
      frequency <- as.numeric(
        frequency
      )
      
      
      frequency <- frequency[
        is.finite(frequency) &
          frequency > 0
      ]
      
      
      V <- length(
        frequency
      )
      
      
      N <- sum(
        frequency
      )
      
      
      if (V == 0 ||
          N <= 0) {
        
        return(
          list(
            
            vocabulary = 0L,
            
            tokens = 0,
            
            entropy = NA_real_,
            
            normalizedEntropy =
              NA_real_,
            
            gini = NA_real_,
            
            simpson = NA_real_
          )
        )
      }
      
      
      p <- frequency / N
      
      
      entropy <-
        
        -sum(
          p * log(p)
        )
      
      
      normalizedEntropy <- if (
        V > 1
      ) {
        
        entropy /
          log(V)
        
      } else {
        
        0
      }
      
      
      gini <- private$.gini(
        frequency
      )
      
      
      simpson <- sum(
        p^2
      )
      
      
      list(
        
        vocabulary = V,
        
        tokens = N,
        
        entropy = entropy,
        
        normalizedEntropy =
          normalizedEntropy,
        
        gini = gini,
        
        simpson = simpson
      )
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
        return(
          NA_real_
        )
      
      
      total <- sum(x)
      
      
      if (!is.finite(total) ||
          total <= 0)
        return(
          NA_real_
        )
      
      
      if (n == 1)
        return(0)
      
      
      x <- sort(
        x,
        decreasing = FALSE
      )
      
      
      i <- seq_len(n)
      
      
      g <- sum(
        (
          2 * i -
            n -
            1
        ) *
          x
      ) /
        (
          n *
            total
        )
      
      
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
    # Distribution Distance Plot
    # ============================================================
    
    .plotDistance = function(
    image,
    ggtheme,
    theme,
    ...
    ) {
      
      state <- image$state
      
      
      if (is.null(state))
        return(FALSE)
      
      
      if (is.null(state$pair) ||
          is.null(state$jsd) ||
          is.null(state$hellinger) ||
          is.null(state$brayCurtis))
        return(FALSE)
      
      
      pair <- as.character(
        state$pair
      )
      
      
      jsd <- as.numeric(
        state$jsd
      )
      
      
      hellinger <- as.numeric(
        state$hellinger
      )
      
      
      brayCurtis <- as.numeric(
        state$brayCurtis
      )
      
      
      n <- length(pair)
      
      
      if (n == 0)
        return(FALSE)
      
      
      plotData <- data.frame(
        
        Pair = rep(
          pair,
          times = 3
        ),
        
        Measure = factor(
          
          rep(
            c(
              "Jensen-Shannon",
              "Hellinger",
              "Bray-Curtis"
            ),
            each = n
          ),
          
          levels = c(
            "Jensen-Shannon",
            "Hellinger",
            "Bray-Curtis"
          )
        ),
        
        Value = c(
          jsd,
          hellinger,
          brayCurtis
        ),
        
        stringsAsFactors = FALSE
      )
      
      
      plotData <- plotData[
        is.finite(
          plotData$Value
        ),
        ,
        drop = FALSE
      ]
      
      
      if (nrow(plotData) == 0)
        return(FALSE)
      
      
      plot <- ggplot2::ggplot(
        
        plotData,
        
        ggplot2::aes(
          x = Pair,
          y = Value,
          fill = Measure
        )
        
      ) +
        
        ggplot2::geom_col(
          position =
            ggplot2::position_dodge(
              width = 0.8
            ),
          width = 0.72
        ) +
        
        ggplot2::scale_y_continuous(
          limits = c(0, 1)
        ) +
        
        ggplot2::labs(
          x = "Group Comparison",
          y = "Distance / Dissimilarity",
          fill = "Measure"
        )
      
      
      plot <- plot + ggtheme
      
      
      print(plot)
      
      
      TRUE
    },
    
    
    # ============================================================
    # Contribution Plot
    # ============================================================
    
    .plotContribution = function(
    image,
    ggtheme,
    theme,
    ...
    ) {
      
      state <- image$state
      
      
      if (is.null(state))
        return(FALSE)
      
      
      if (is.null(state$pair) ||
          is.null(state$word) ||
          is.null(state$contribution))
        return(FALSE)
      
      
      pair <- as.character(
        state$pair
      )
      
      
      word <- as.character(
        state$word
      )
      
      
      contribution <- as.numeric(
        state$contribution
      )
      
      
      valid <-
        
        !is.na(pair) &
        
        !is.na(word) &
        
        nzchar(word) &
        
        is.finite(contribution)
      
      
      pair <- pair[valid]
      
      word <- word[valid]
      
      contribution <-
        contribution[valid]
      
      
      if (length(word) == 0)
        return(FALSE)
      
      
      # --------------------------------------------------------
      # Pair-specific factor labels are used so that the
      # ordering of words can differ across facets.
      # --------------------------------------------------------
      
      uniqueLabel <- paste(
        
        word,
        
        pair,
        
        sep = "___"
      )
      
      
      plotData <- data.frame(
        
        Pair = pair,
        
        Word = word,
        
        Label = uniqueLabel,
        
        Contribution =
          contribution,
        
        stringsAsFactors = FALSE
      )
      
      
      plotData <- plotData[
        order(
          plotData$Pair,
          plotData$Contribution
        ),
        ,
        drop = FALSE
      ]
      
      
      plotData$Label <- factor(
        
        plotData$Label,
        
        levels = unique(
          plotData$Label
        )
      )
      
      
      plot <- ggplot2::ggplot(
        
        plotData,
        
        ggplot2::aes(
          x = Contribution,
          y = Label,
          fill = Pair
        )
        
      ) +
        
        ggplot2::geom_col() +
        
        ggplot2::scale_fill_manual(
          values = stats::setNames(
            grDevices::hcl.colors(
              length(unique(plotData$Pair)),
              palette = "Dark 3"
            ),
            unique(plotData$Pair)
          ),
          guide = "none"
        ) +
        
        ggplot2::facet_wrap(
          ~ Pair,
          scales = "free_y"
        ) +
        
        ggplot2::scale_y_discrete(
          
          labels = function(x) {
            
            sub(
              "___.*$",
              "",
              x
            )
          }
        ) +
        
        ggplot2::labs(
          x = "JS Contribution",
          y = "Word"
        )
      
      
      plot <- plot + ggtheme
      
      
      if (self$options$angle > 0) {
        plot <- plot +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = self$options$angle,
              hjust = 1
            )
          )
      }
      
      print(plot)
      
      
      TRUE
    },
    
    
    
    # ============================================================
    # Differential Vocabulary Map
    # ============================================================
    
    .plotDifferentialVocabulary = function(
    image,
    ggtheme,
    theme,
    ...
    ) {
      
      state <- image$state
      
      
      if (is.null(state))
        return(FALSE)
      
      
      if (is.null(state$pair) ||
          is.null(state$word) ||
          is.null(state$difference) ||
          is.null(state$direction))
        return(FALSE)
      
      
      pair <- as.character(
        state$pair
      )
      
      
      word <- as.character(
        state$word
      )
      
      
      difference <- as.numeric(
        state$difference
      )
      
      
      direction <- as.character(
        state$direction
      )
      
      
      valid <-
        
        !is.na(pair) &
        
        !is.na(word) &
        
        nzchar(word) &
        
        is.finite(difference) &
        
        !is.na(direction)
      
      
      pair <- pair[valid]
      
      word <- word[valid]
      
      difference <- difference[valid]
      
      direction <- direction[valid]
      
      
      if (length(word) == 0)
        return(FALSE)
      
      
      # --------------------------------------------------------
      # Pair-specific factor labels are used so that the
      # ordering of words can differ across facets.
      # --------------------------------------------------------
      
      uniqueLabel <- paste(
        
        word,
        
        pair,
        
        sep = "___"
      )
      
      
      plotData <- data.frame(
        
        Pair = pair,
        
        Word = word,
        
        Label = uniqueLabel,
        
        Difference = difference,
        
        Direction = direction,
        
        stringsAsFactors = FALSE
      )
      
      
      plotData <- plotData[
        order(
          plotData$Pair,
          plotData$Difference
        ),
        ,
        drop = FALSE
      ]
      
      
      plotData$Label <- factor(
        
        plotData$Label,
        
        levels = unique(
          plotData$Label
        )
      )
      
      
      directionLevels <- unique(
        plotData$Direction
      )
      
      
      palette <- grDevices::hcl.colors(
        max(
          3,
          length(directionLevels)
        ),
        palette = "Dark 3"
      )[seq_along(directionLevels)]
      
      
      plot <- ggplot2::ggplot(
        
        plotData,
        
        ggplot2::aes(
          x = Difference,
          y = Label,
          fill = Direction
        )
        
      ) +
        
        ggplot2::geom_vline(
          xintercept = 0,
          colour = "#7F8C8D",
          linewidth = 0.6
        ) +
        
        ggplot2::geom_col() +
        
        ggplot2::scale_x_continuous(
          breaks = function(x) pretty(x, n = 3),
          labels = function(x) sprintf("%.1f", x * 100)
        ) +
        
        ggplot2::scale_fill_manual(
          values = stats::setNames(
            palette,
            directionLevels
          )
        ) +
        
        ggplot2::facet_wrap(
          ~ Pair,
          scales = "free_y"
        ) +
        
        ggplot2::scale_y_discrete(
          
          labels = function(x) {
            
            sub(
              "___.*$",
              "",
              x
            )
          }
        ) +
        
        ggplot2::labs(
          x = "Relative Frequency Difference (%)",
          y = "Word",
          fill = "Higher in"
        )
      
      
      plot <- plot + ggtheme
      
      
      print(plot)
      
      
      TRUE
    }
    
  )
)