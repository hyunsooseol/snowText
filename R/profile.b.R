
profileClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "profileClass",
  inherit = profileBase,
  
  private = list(
    .htmlwidget = NULL,
    # Add instance for HTMLWidget
    
    .init = function() {
      private$.htmlwidget <- HTMLWidget$new()
      
      if (is.null(self$data) || is.null(self$options$vars)) {
        self$results$instructions$setVisible(visible = TRUE)
      }
      
      self$results$instructions$setContent(
        private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe;',
            'border-radius: 15px; padding: 15px;',
            'background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align: justify;">',
            
            '<p><strong>Text Profile</strong> examines the quality',
            'and relationships of document-level text measures.</p>',
            
            '<ul>',
            '<li><strong>Text Variables:</strong> select numeric',
            'measures, with one document per row.</li>',
            '<li><strong>Length Variable:</strong> select a numeric',
            'document length measure for length diagnostics.</li>',
            '<li><strong>Grouping Variable:</strong> select a category',
            'to compare profiles across groups.</li>',
            '<li><strong>Document ID:</strong> optionally identify',
            'documents flagged as outliers.</li>',
            '</ul>',
            
            '<p>Use <strong>Profile Diagnostics</strong> to examine',
            'data quality, length effects, adjusted relationships,',
            'group differences, outliers, and redundancy.</p>',
            
            '<p>For Web Text Mining exports, use',
            '<strong>core/document_analysis.csv</strong>.</p>',
            
            '<p>Feature requests and bug reports:',
            '<a href="https://github.com/hyunsooseol/snowText/issues"',
            'target="_blank">GitHub</a>.</p>',
            
            '</div></div>'
          )
        )
      )
    },
    
    
    # ================================================================
    # Main analysis
    # ================================================================
    
    .run = function() {
      
      # ------------------------------------------------------------
      # 0. Basic setup
      # ------------------------------------------------------------
      
      vars <- self$options$vars
      data <- self$data
      
      if (is.null(vars) || length(vars) == 0)
        return()
      
      vars <- vars[vars %in% names(data)]
      
      if (length(vars) == 0)
        return()
      
      
      # ============================================================
      # 1. PROFILE DATA QUALITY
      # ============================================================
      
      if (isTRUE(self$options$dataQuality)) {
        
        qualityTable <- self$results$dataQuality
        summaryTable <- self$results$dataQualitySummary
        
        # The variable-level table uses rows: (vars), so setRow() is used.
        # The completeness table is dynamic and contains one summary row.
        summaryTable$deleteRows()
        
        X <- private$.numericMatrix(data, vars)
        
        totalN <- nrow(X)
        
        for (var in vars) {
          
          x <- private$.numeric(data[[var]])
          
          valid <- is.finite(x)
          validN <- sum(valid)
          missing <- length(x) - validN
          
          missingPct <- if (length(x) > 0)
            missing / length(x) * 100
          else
            NA_real_
          
          uniqueN <- if (validN > 0)
            length(unique(x[valid]))
          else
            0L
          
          variance <- NA_real_
          
          if (validN >= 2)
            variance <- stats::var(x[valid])
          
          status <- 'OK'
          
          # Status precedence is intentional: a measure that cannot be
          # analysed should not merely be labelled as highly missing.
          if (validN < 3) {
            
            status <- 'Insufficient'
            
          } else if (is.finite(variance) && variance <= 0) {
            
            status <- 'Constant'
            
          } else if (is.finite(missingPct) && missingPct >= 20) {
            
            status <- 'High missingness'
          }
          
          qualityTable$setRow(
            rowKey = var,
            values = list(
              validN     = validN,
              missing    = missing,
              missingPct = missingPct,
              unique     = uniqueN,
              variance   = variance,
              status     = status
            )
          )
        }
        
        complete <- stats::complete.cases(X)
        completeN <- sum(complete)
        
        completePct <- if (totalN > 0)
          completeN / totalN * 100
        else
          NA_real_
        
        summaryTable$addRow(
          rowKey = 'summary',
          values = list(
            totalN      = totalN,
            completeN   = completeN,
            completePct = completePct
          )
        )
        
        qualityTable$setNote(
          key  = 'valid',
          note = 'Valid N counts finite numeric observations. Missing includes non-finite or missing observations after numeric conversion.'
        )
        
        qualityTable$setNote(
          key  = 'status',
          note = 'Status is diagnostic: Insufficient = fewer than 3 valid observations; Constant = zero variance; High missingness = at least 20% missing; otherwise OK.'
        )
        
        summaryTable$setNote(
          key  = 'complete',
          note = 'Complete Rows are observations with valid values on all selected Text Variables. Multivariate analyses may use this common complete-case subset.'
        )
      }
      
      
      # ============================================================
      # 2. LENGTH BIAS DIAGNOSTICS
      # ============================================================
      
      if (isTRUE(self$options$lengthBias)) {
        
        table <- self$results$lengthBias
        
        lengthVar <- self$options$lengthVar
        
        # --------------------------------------------------------
        # No length variable selected
        # --------------------------------------------------------
        
        if (is.null(lengthVar) ||
            length(lengthVar) != 1 ||
            lengthVar == '' ||
            ! lengthVar %in% names(data)) {
          
          for (var in vars) {
            
            table$setRow(
              rowKey = var,
              values = list(
                n           = NA,
                correlation = NA,
                slope       = NA,
                r2          = NA,
                p           = NA,
                effect      = ''
              )
            )
          }
          
          table$setNote(
            key  = 'lengthVar',
            note = 'Select a continuous Length Variable to evaluate length-related bias.'
          )
          
        } else {
          
          lengthX <- private$.numeric(data[[lengthVar]])
          
          for (var in vars) {
            
            y <- private$.numeric(data[[var]])
            
            # The length variable itself should not be tested
            # against itself.
            if (identical(var, lengthVar)) {
              
              table$setRow(
                rowKey = var,
                values = list(
                  n           = sum(is.finite(y)),
                  correlation = NA,
                  slope       = NA,
                  r2          = NA,
                  p           = NA,
                  effect      = 'Length variable'
                )
              )
              
              next
            }
            
            
            valid <- is.finite(lengthX) & is.finite(y)
            
            x <- lengthX[valid]
            yy <- y[valid]
            
            n <- length(yy)
            
            r     <- NA_real_
            slope <- NA_real_
            r2    <- NA_real_
            p     <- NA_real_
            effect <- ''
            
            
            # Need variation in both x and y
            if (n >= 3 &&
                length(unique(x)) > 1 &&
                length(unique(yy)) > 1) {
              
              # Pearson association with document length
              r <- suppressWarnings(
                stats::cor(
                  x,
                  yy,
                  method = 'pearson'
                )
              )
              
              # Simple regression:
              # text measure = b0 + b1 * length
              fit <- tryCatch(
                stats::lm(yy ~ x),
                error = function(e) NULL
              )
              
              if (! is.null(fit)) {
                
                fitSummary <- summary(fit)
                
                if (length(stats::coef(fit)) >= 2)
                  slope <- unname(stats::coef(fit)[2])
                
                r2 <- fitSummary$r.squared
                
                coefTable <- fitSummary$coefficients
                
                if (nrow(coefTable) >= 2)
                  p <- coefTable[2, 4]
              }
              
              
              if (! is.finite(r))
                r <- NA_real_
              
              if (! is.finite(slope))
                slope <- NA_real_
              
              if (! is.finite(r2))
                r2 <- NA_real_
              
              if (! is.finite(p))
                p <- NA_real_
              
              
              # Diagnostic classification based on explained
              # variance. These are descriptive thresholds,
              # not formal hypothesis-test cut-offs.
              effect <- private$.lengthEffect(r2)
            }
            
            
            table$setRow(
              rowKey = var,
              values = list(
                n           = n,
                correlation = r,
                slope       = slope,
                r2          = r2,
                p           = p,
                effect      = effect
              )
            )
          }
          
          
          table$setNote(
            key  = 'model',
            note = 'Length bias is evaluated using Pearson correlation and simple linear regression of each text measure on the selected length variable.'
          )
          
          table$setNote(
            key  = 'effect',
            note = 'Length Effect is a descriptive diagnostic based on R²: < .01 negligible, .01–.09 small, .09–.25 moderate, and ≥ .25 large.'
          )
          
          table$setNote(
            key  = 'missing',
            note = 'Missing values are excluded separately for each text measure.'
          )
        }
      }
      
      
      # ============================================================
      # 3. LENGTH-ADJUSTED RELATIONSHIPS
      # ============================================================
      
      if (isTRUE(self$options$adjustedRelationships)) {
        
        table <- self$results$adjustedRelationships
        table$deleteRows()
        
        lengthVar <- self$options$lengthVar
        
        if (is.null(lengthVar) ||
            length(lengthVar) != 1 ||
            lengthVar == '' ||
            ! lengthVar %in% names(data)) {
          
          table$setNote(
            key  = 'lengthVar',
            note = 'Select a continuous Length Variable to estimate length-adjusted relationships.'
          )
          
        } else if (length(vars) < 2) {
          
          table$setNote(
            key  = 'vars',
            note = 'At least two Text Variables are required to estimate length-adjusted relationships.'
          )
          
        } else {
          
          zAll <- private$.numeric(data[[lengthVar]])
          
          for (i in seq_len(length(vars) - 1)) {
            
            for (j in seq.int(i + 1, length(vars))) {
              
              var1 <- vars[i]
              var2 <- vars[j]
              
              xAll <- private$.numeric(data[[var1]])
              yAll <- private$.numeric(data[[var2]])
              
              valid <- is.finite(xAll) &
                is.finite(yAll) &
                is.finite(zAll)
              
              x <- xAll[valid]
              y <- yAll[valid]
              z <- zAll[valid]
              
              n <- length(x)
              
              partialR <- NA_real_
              df <- NA_real_
              p <- NA_real_
              
              # One control variable -> df = N - 3. At least four
              # observations are therefore required for an inferential test.
              if (n >= 4 &&
                  length(unique(x)) > 1 &&
                  length(unique(y)) > 1 &&
                  length(unique(z)) > 1) {
                
                fitX <- tryCatch(
                  stats::lm(x ~ z),
                  error = function(e) NULL
                )
                
                fitY <- tryCatch(
                  stats::lm(y ~ z),
                  error = function(e) NULL
                )
                
                if (! is.null(fitX) && ! is.null(fitY)) {
                  
                  rx <- stats::residuals(fitX)
                  ry <- stats::residuals(fitY)
                  
                  if (length(unique(rx)) > 1 &&
                      length(unique(ry)) > 1) {
                    
                    partialR <- suppressWarnings(
                      stats::cor(rx, ry, method = 'pearson')
                    )
                    
                    if (! is.finite(partialR))
                      partialR <- NA_real_
                    
                    df <- n - 3
                    
                    if (is.finite(partialR) && df > 0) {
                      
                      # Guard against harmless floating-point excursions
                      # beyond [-1, 1].
                      partialR <- max(-1, min(1, partialR))
                      
                      if (abs(partialR) >= 1) {
                        
                        p <- 0
                        
                      } else {
                        
                        tValue <- partialR * sqrt(
                          df / (1 - partialR^2)
                        )
                        
                        p <- 2 * stats::pt(
                          -abs(tValue),
                          df = df
                        )
                      }
                    }
                  }
                }
              }
              
              table$addRow(
                rowKey = paste(var1, var2, sep = '::'),
                values = list(
                  variable1   = var1,
                  variable2   = var2,
                  n           = n,
                  correlation = partialR,
                  df          = df,
                  p           = p
                )
              )
            }
          }
          
          table$setNote(
            key  = 'method',
            note = 'Partial correlations are Pearson correlations between residuals after regressing each text measure on the selected Length Variable.'
          )
          
          table$setNote(
            key  = 'df',
            note = 'With one control variable, the significance test uses df = N - 3. Missing values are excluded separately for each measure pair and the Length Variable.'
          )
        }
      }
      
      
      # ============================================================
      # 4. MULTIVARIATE PROFILE TEST
      # ============================================================
      
      if (isTRUE(self$options$multivariate)) {
        
        manovaTable <- self$results$multivariateTest
        contributionTable <- self$results$profileContribution
        
        # These are dynamic tables
        manovaTable$deleteRows()
        contributionTable$deleteRows()
        
        groupVar <- self$options$group
        
        
        # --------------------------------------------------------
        # Grouping variable not supplied
        # --------------------------------------------------------
        
        if (is.null(groupVar) ||
            length(groupVar) != 1 ||
            groupVar == '' ||
            ! groupVar %in% names(data)) {
          
          manovaTable$setNote(
            key  = 'group',
            note = 'Select a Grouping Variable to perform the multivariate profile test.'
          )
          
        } else if (length(vars) < 2) {
          
          manovaTable$setNote(
            key  = 'vars',
            note = 'At least two Text Variables are required for the multivariate profile test.'
          )
          
        } else {
          
          # ----------------------------------------------------
          # Build complete-case multivariate matrix
          # ----------------------------------------------------
          
          X <- private$.numericMatrix(data, vars)
          
          g <- data[[groupVar]]
          
          if (! is.factor(g))
            g <- factor(g)
          
          valid <- stats::complete.cases(X) & !is.na(g)
          
          Xcc <- X[valid, , drop = FALSE]
          gcc <- droplevels(g[valid])
          
          
          # ----------------------------------------------------
          # Remove zero-variance variables for MANOVA
          # ----------------------------------------------------
          
          varOK <- apply(
            Xcc,
            2,
            function(z) {
              length(z) >= 2 &&
                is.finite(stats::var(z)) &&
                stats::var(z) > 0
            }
          )
          
          Xman <- Xcc[, varOK, drop = FALSE]
          manVars <- colnames(Xman)
          
          
          if (nlevels(gcc) < 2) {
            
            manovaTable$setNote(
              key  = 'levels',
              note = 'The Grouping Variable must contain at least two observed groups.'
            )
            
          } else if (ncol(Xman) < 2) {
            
            manovaTable$setNote(
              key  = 'variance',
              note = 'At least two non-constant Text Variables are required for the multivariate profile test.'
            )
            
          } else if (nrow(Xman) <= nlevels(gcc)) {
            
            manovaTable$setNote(
              key  = 'sample',
              note = 'There are too few complete observations for the requested multivariate profile test.'
            )
            
          } else {
            
            # ------------------------------------------------
            # MANOVA with Pillai's Trace
            # ------------------------------------------------
            
            manovaFit <- tryCatch(
              stats::manova(Xman ~ gcc),
              error = function(e) NULL
            )
            
            
            pillaiOK <- FALSE
            
            if (! is.null(manovaFit)) {
              
              manSummary <- tryCatch(
                summary(
                  manovaFit,
                  test = 'Pillai'
                ),
                error = function(e) NULL
              )
              
              if (! is.null(manSummary) &&
                  ! is.null(manSummary$stats)) {
                
                statsMat <- manSummary$stats
                
                if (nrow(statsMat) >= 1) {
                  
                  value <- suppressWarnings(
                    as.numeric(
                      statsMat[1, 'Pillai']
                    )
                  )
                  
                  approxF <- suppressWarnings(
                    as.numeric(
                      statsMat[1, 'approx F']
                    )
                  )
                  
                  df1 <- suppressWarnings(
                    as.numeric(
                      statsMat[1, 'num Df']
                    )
                  )
                  
                  df2 <- suppressWarnings(
                    as.numeric(
                      statsMat[1, 'den Df']
                    )
                  )
                  
                  pValue <- suppressWarnings(
                    as.numeric(
                      statsMat[1, 'Pr(>F)']
                    )
                  )
                  
                  
                  if (is.finite(value) &&
                      is.finite(approxF) &&
                      is.finite(df1) &&
                      is.finite(df2) &&
                      is.finite(pValue)) {
                    
                    manovaTable$addRow(
                      rowKey = 'pillai',
                      values = list(
                        test    = "Pillai's Trace",
                        value   = value,
                        approxF = approxF,
                        df1     = df1,
                        df2     = df2,
                        p       = pValue
                      )
                    )
                    
                    pillaiOK <- TRUE
                  }
                }
              }
            }
            
            
            if (! pillaiOK) {
              
              manovaTable$setNote(
                key  = 'manovaFail',
                note = 'The multivariate test could not be estimated. This can occur when the selected measures are linearly dependent or the sample size is insufficient.'
              )
            }
            
            
            # ------------------------------------------------
            # Follow-up univariate contributions
            #
            # Classical one-way ANOVA is used here as a
            # descriptive follow-up to the multivariate test.
            #
            # Partial eta squared:
            #
            # SS_group / (SS_group + SS_error)
            # ------------------------------------------------
            
            for (var in manVars) {
              
              y <- Xcc[, var]
              
              validUni <- is.finite(y) & !is.na(gcc)
              
              yy <- y[validUni]
              gg <- droplevels(gcc[validUni])
              
              
              if (length(yy) < 3 ||
                  nlevels(gg) < 2 ||
                  length(unique(yy)) < 2)
                next
              
              
              fit <- tryCatch(
                stats::lm(yy ~ gg),
                error = function(e) NULL
              )
              
              if (is.null(fit))
                next
              
              
              aovTable <- tryCatch(
                stats::anova(fit),
                error = function(e) NULL
              )
              
              if (is.null(aovTable) ||
                  nrow(aovTable) < 2)
                next
              
              
              fValue <- suppressWarnings(
                as.numeric(aovTable$`F value`[1])
              )
              
              df1 <- suppressWarnings(
                as.numeric(aovTable$Df[1])
              )
              
              df2 <- suppressWarnings(
                as.numeric(aovTable$Df[2])
              )
              
              pValue <- suppressWarnings(
                as.numeric(aovTable$`Pr(>F)`[1])
              )
              
              ssGroup <- suppressWarnings(
                as.numeric(aovTable$`Sum Sq`[1])
              )
              
              ssError <- suppressWarnings(
                as.numeric(aovTable$`Sum Sq`[2])
              )
              
              partialEta2 <- NA_real_
              
              denom <- ssGroup + ssError
              
              if (is.finite(denom) && denom > 0)
                partialEta2 <- ssGroup / denom
              
              
              contributionTable$addRow(
                rowKey = var,
                values = list(
                  variable    = var,
                  f           = fValue,
                  df1         = df1,
                  df2         = df2,
                  p           = pValue,
                  partialEta2 = partialEta2
                )
              )
            }
            
            
            manovaTable$setNote(
              key  = 'pillai',
              note = "Pillai's Trace is reported as the primary multivariate test because it is comparatively robust to departures from multivariate assumptions."
            )
            
            contributionTable$setNote(
              key  = 'followup',
              note = 'Contribution results are follow-up one-way ANOVAs for the individual text measures. Partial η² is calculated as SSgroup / (SSgroup + SSerror).'
            )
            
            contributionTable$setNote(
              key  = 'interpretation',
              note = 'Follow-up univariate results should be interpreted in the context of the overall multivariate profile test.'
            )
          }
        }
      }
      
      
      # ============================================================
      # 5. PROFILE OUTLIERS
      # ============================================================
      
      if (isTRUE(self$options$outliers)) {
        
        table <- self$results$outliers
        
        table$deleteRows()
        
        idVar <- self$options$idVar
        
        
        if (length(vars) < 2) {
          
          table$setNote(
            key  = 'vars',
            note = 'At least two Text Variables are required for multivariate profile outlier detection.'
          )
          
        } else {
          
          X <- private$.numericMatrix(data, vars)
          
          complete <- stats::complete.cases(X)
          
          Xcc <- X[complete, , drop = FALSE]
          
          sourceRows <- which(complete)
          
          
          # Remove constant variables
          keep <- apply(
            Xcc,
            2,
            function(z) {
              length(z) >= 2 &&
                is.finite(stats::var(z)) &&
                stats::var(z) > 0
            }
          )
          
          Xuse <- Xcc[, keep, drop = FALSE]
          
          
          if (ncol(Xuse) < 2) {
            
            table$setNote(
              key  = 'variance',
              note = 'At least two non-constant Text Variables are required for multivariate profile outlier detection.'
            )
            
          } else if (nrow(Xuse) <= 2) {
            
            table$setNote(
              key  = 'sample',
              note = 'There are too few complete observations to estimate multivariate profile outliers.'
            )
            
          } else {
            
            center <- colMeans(Xuse)
            
            S <- tryCatch(
              stats::cov(Xuse),
              error = function(e) NULL
            )
            
            
            if (! is.null(S) &&
                all(is.finite(S))) {
              
              # ------------------------------------------------
              # Eigen-based generalized inverse
              #
              # This safely handles singular covariance
              # matrices caused by redundant profile measures.
              #
              # The chi-square df is the effective covariance
              # rank rather than blindly using the number of
              # selected measures.
              # ------------------------------------------------
              
              eig <- tryCatch(
                eigen(
                  S,
                  symmetric = TRUE
                ),
                error = function(e) NULL
              )
              
              
              if (! is.null(eig)) {
                
                values <- eig$values
                vectors <- eig$vectors
                
                maxEig <- max(abs(values))
                
                tol <- max(
                  dim(S)
                ) * .Machine$double.eps * maxEig
                
                
                positive <- values > tol
                
                rankS <- sum(positive)
                
                
                if (rankS >= 1) {
                  
                  invS <- vectors[, positive, drop = FALSE] %*%
                    diag(
                      1 / values[positive],
                      nrow = rankS
                    ) %*%
                    t(
                      vectors[, positive, drop = FALSE]
                    )
                  
                  
                  centered <- sweep(
                    Xuse,
                    2,
                    center,
                    FUN = '-'
                  )
                  
                  
                  d2 <- rowSums(
                    (centered %*% invS) * centered
                  )
                  
                  
                  pValues <- stats::pchisq(
                    d2,
                    df = rankS,
                    lower.tail = FALSE
                  )
                  
                  
                  # Sort most unusual documents first
                  orderIndex <- order(
                    pValues,
                    d2,
                    decreasing = FALSE,
                    na.last = TRUE
                  )
                  
                  
                  for (k in orderIndex) {
                    
                    rowIndex <- sourceRows[k]
                    
                    document <- as.character(rowIndex)
                    
                    if (! is.null(idVar) &&
                        length(idVar) == 1 &&
                        idVar != '' &&
                        idVar %in% names(data)) {
                      
                      idValue <- data[[idVar]][rowIndex]
                      
                      if (! is.na(idValue))
                        document <- as.character(idValue)
                    }
                    
                    
                    flag <- ''
                    
                    # Conservative threshold for
                    # multivariate profile screening
                    if (is.finite(pValues[k]) &&
                        pValues[k] < .001) {
                      
                      flag <- 'Unusual'
                    }
                    
                    
                    table$addRow(
                      rowKey = paste0(
                        'row_',
                        rowIndex
                      ),
                      values = list(
                        document = document,
                        row      = rowIndex,
                        distance = d2[k],
                        df       = rankS,
                        p        = pValues[k],
                        flag     = flag
                      )
                    )
                  }
                  
                  
                  table$setNote(
                    key  = 'method',
                    note = 'Profile outliers are evaluated using squared Mahalanobis distance across complete observations.'
                  )
                  
                  table$setNote(
                    key  = 'threshold',
                    note = 'Documents with p < .001 are flagged as Unusual. The threshold is intentionally conservative for exploratory multivariate screening.'
                  )
                  
                  
                  if (rankS < ncol(Xuse)) {
                    
                    table$setNote(
                      key  = 'rank',
                      note = 'The covariance matrix was rank-deficient. A generalized inverse was used and chi-square degrees of freedom were based on the effective covariance rank.'
                    )
                  }
                }
              }
            }
          }
        }
      }
      
      
      # ============================================================
      # 6. PROFILE REDUNDANCY
      # ============================================================
      
      if (isTRUE(self$options$redundancy)) {
        
        table <- self$results$redundancy
        
        
        # Need at least two measures to assess association
        if (length(vars) < 2) {
          
          for (var in vars) {
            
            table$setRow(
              rowKey = var,
              values = list(
                strongest      = '',
                maxCorrelation = NA,
                vif            = NA,
                status         = ''
              )
            )
          }
          
          table$setNote(
            key  = 'vars',
            note = 'At least two Text Variables are required to evaluate profile redundancy.'
          )
          
        } else {
          
          X <- private$.numericMatrix(data, vars)
          
          
          # ----------------------------------------------------
          # Pairwise correlation matrix
          # ----------------------------------------------------
          
          R <- suppressWarnings(
            stats::cor(
              X,
              use = 'pairwise.complete.obs',
              method = 'pearson'
            )
          )
          
          
          # ----------------------------------------------------
          # VIF uses common complete cases across all measures
          # to ensure that the regressions are comparable.
          # ----------------------------------------------------
          
          complete <- stats::complete.cases(X)
          Xcomplete <- X[complete, , drop = FALSE]
          
          
          for (i in seq_along(vars)) {
            
            var <- vars[i]
            
            strongest <- ''
            maxR <- NA_real_
            vif <- NA_real_
            status <- ''
            
            
            # ------------------------------------------------
            # Strongest pairwise association
            # ------------------------------------------------
            
            otherIndex <- setdiff(
              seq_along(vars),
              i
            )
            
            if (length(otherIndex) > 0 &&
                ! is.null(dim(R))) {
              
              rValues <- abs(
                R[i, otherIndex]
              )
              
              finiteR <- is.finite(rValues)
              
              if (any(finiteR)) {
                
                candidates <- otherIndex[finiteR]
                candidateR <- rValues[finiteR]
                
                best <- which.max(candidateR)
                
                bestIndex <- candidates[best]
                
                strongest <- vars[bestIndex]
                maxR <- candidateR[best]
              }
            }
            
            
            # ------------------------------------------------
            # Variance Inflation Factor
            #
            # VIF_j = 1 / (1 - R_j^2)
            #
            # where R_j^2 is obtained by regressing measure j
            # on all remaining selected profile measures.
            # ------------------------------------------------
            
            if (nrow(Xcomplete) >= 3) {
              
              y <- Xcomplete[, i]
              
              predictors <- Xcomplete[
                ,
                otherIndex,
                drop = FALSE
              ]
              
              
              if (length(unique(y)) > 1 &&
                  ncol(predictors) >= 1) {
                
                # Remove predictor columns with zero variance
                predictorOK <- apply(
                  predictors,
                  2,
                  function(z) {
                    is.finite(stats::var(z)) &&
                      stats::var(z) > 0
                  }
                )
                
                predictors <- predictors[
                  ,
                  predictorOK,
                  drop = FALSE
                ]
                
                
                if (ncol(predictors) >= 1 &&
                    nrow(predictors) >
                    ncol(predictors) + 1) {
                  
                  vifData <- data.frame(
                    y = y,
                    predictors,
                    check.names = FALSE
                  )
                  
                  fit <- tryCatch(
                    stats::lm(
                      y ~ .,
                      data = vifData
                    ),
                    error = function(e) NULL
                  )
                  
                  
                  if (! is.null(fit)) {
                    
                    r2 <- summary(fit)$r.squared
                    
                    if (is.finite(r2)) {
                      
                      if (r2 >= 1 - sqrt(.Machine$double.eps)) {
                        
                        vif <- Inf
                        
                      } else {
                        
                        vif <- 1 / (1 - r2)
                      }
                    }
                  }
                }
              }
            }
            
            
            # ------------------------------------------------
            # Redundancy classification
            # ------------------------------------------------
            
            if (is.infinite(vif)) {
              
              status <- 'High'
              
            } else if (is.finite(vif)) {
              
              if (vif >= 10) {
                
                status <- 'High'
                
              } else if (vif >= 5) {
                
                status <- 'Moderate'
                
              } else {
                
                status <- 'Low'
              }
            }
            
            
            table$setRow(
              rowKey = var,
              values = list(
                strongest      = strongest,
                maxCorrelation = maxR,
                vif            = vif,
                status         = status
              )
            )
          }
          
          
          table$setNote(
            key  = 'correlation',
            note = 'Max |r| is the largest absolute Pearson correlation between the measure and any other selected profile measure.'
          )
          
          table$setNote(
            key  = 'vif',
            note = 'VIF is calculated by regressing each measure on all other selected measures using common complete observations.'
          )
          
          table$setNote(
            key  = 'threshold',
            note = 'Redundancy is classified descriptively as Low (VIF < 5), Moderate (5 ≤ VIF < 10), or High (VIF ≥ 10).'
          )
        }
      }
      
      
      # ============================================================
      # 7. PLOT STATES
      # ============================================================
      
      # ------------------------------------------------------------
      # Text Profile Plot
      # ------------------------------------------------------------
      
      if (isTRUE(self$options$profilePlot)) {
        
        groupVar <- self$options$group
        
        state <- list(
          valid = FALSE,
          message = 'Select a Grouping Variable to display group text profiles.'
        )
        
        if (! is.null(groupVar) &&
            length(groupVar) == 1 &&
            groupVar != '' &&
            groupVar %in% names(data) &&
            length(vars) >= 2) {
          
          X <- private$.numericMatrix(data, vars)
          g <- data[[groupVar]]
          
          if (! is.factor(g))
            g <- factor(g)
          
          complete <- stats::complete.cases(X) & !is.na(g)
          Xcc <- X[complete, , drop = FALSE]
          gcc <- droplevels(g[complete])
          
          if (nrow(Xcc) >= 3 && nlevels(gcc) >= 2) {
            
            sdValues <- apply(Xcc, 2, stats::sd)
            keep <- is.finite(sdValues) & sdValues > 0
            
            Xuse <- Xcc[, keep, drop = FALSE]
            plotVars <- colnames(Xuse)
            
            if (ncol(Xuse) >= 2) {
              
              means <- colMeans(Xuse)
              sds <- apply(Xuse, 2, stats::sd)
              
              Z <- sweep(Xuse, 2, means, FUN = '-')
              Z <- sweep(Z, 2, sds, FUN = '/')
              
              groupLevels <- levels(gcc)
              
              profileMeans <- lapply(
                groupLevels,
                function(level) {
                  rows <- gcc == level
                  colMeans(Z[rows, , drop = FALSE], na.rm = TRUE)
                }
              )
              
              profileMeans <- do.call(rbind, profileMeans)
              rownames(profileMeans) <- groupLevels
              
              state <- list(
                valid = TRUE,
                variables = plotVars,
                groups = groupLevels,
                means = profileMeans
              )
              
            } else {
              
              state$message <- paste(
                'At least two non-constant Text Variables are required',
                'for the Text Profile Plot.'
              )
            }
            
          } else {
            
            state$message <- paste(
              'At least two observed groups and sufficient complete cases',
              'are required for the Text Profile Plot.'
            )
          }
          
        } else if (length(vars) < 2) {
          
          state$message <- paste(
            'At least two Text Variables are required',
            'for the Text Profile Plot.'
          )
        }
        
        self$results$profilePlot$setState(state)
      }
      
      
      # ------------------------------------------------------------
      # Profile Diagnostic Map
      # ------------------------------------------------------------
      
      if (isTRUE(self$options$diagnosticMap)) {
        
        diagnostics <- c('Length Bias', 'Redundancy', 'Data Quality')
        
        statusMatrix <- matrix(
          'Not available',
          nrow = length(vars),
          ncol = length(diagnostics),
          dimnames = list(vars, diagnostics)
        )
        
        severityMatrix <- matrix(
          NA_real_,
          nrow = length(vars),
          ncol = length(diagnostics),
          dimnames = list(vars, diagnostics)
        )
        
        # Length-bias classification
        lengthVar <- self$options$lengthVar
        
        if (! is.null(lengthVar) &&
            length(lengthVar) == 1 &&
            lengthVar != '' &&
            lengthVar %in% names(data)) {
          
          lengthX <- private$.numeric(data[[lengthVar]])
          
          for (i in seq_along(vars)) {
            var <- vars[i]
            y <- private$.numeric(data[[var]])
            
            if (identical(var, lengthVar)) {
              statusMatrix[i, 'Length Bias'] <- 'Length variable'
              next
            }
            
            valid <- is.finite(lengthX) & is.finite(y)
            x <- lengthX[valid]
            yy <- y[valid]
            
            if (length(yy) >= 3 &&
                length(unique(x)) > 1 &&
                length(unique(yy)) > 1) {
              
              fit <- tryCatch(stats::lm(yy ~ x), error = function(e) NULL)
              
              if (! is.null(fit)) {
                r2 <- summary(fit)$r.squared
                effect <- private$.lengthEffect(r2)
                statusMatrix[i, 'Length Bias'] <- effect
                severityMatrix[i, 'Length Bias'] <- switch(
                  effect,
                  'Negligible' = 0,
                  'Small' = 1,
                  'Moderate' = 2,
                  'Large' = 3,
                  NA_real_
                )
              }
            }
          }
        }
        
        # Redundancy classification
        if (length(vars) >= 2) {
          
          X <- private$.numericMatrix(data, vars)
          complete <- stats::complete.cases(X)
          Xcomplete <- X[complete, , drop = FALSE]
          
          for (i in seq_along(vars)) {
            otherIndex <- setdiff(seq_along(vars), i)
            vif <- NA_real_
            
            if (nrow(Xcomplete) >= 3) {
              y <- Xcomplete[, i]
              predictors <- Xcomplete[, otherIndex, drop = FALSE]
              
              if (length(unique(y)) > 1 && ncol(predictors) >= 1) {
                predictorOK <- apply(
                  predictors,
                  2,
                  function(z) is.finite(stats::var(z)) && stats::var(z) > 0
                )
                
                predictors <- predictors[, predictorOK, drop = FALSE]
                
                if (ncol(predictors) >= 1 &&
                    nrow(predictors) > ncol(predictors) + 1) {
                  
                  vifData <- data.frame(
                    y = y,
                    predictors,
                    check.names = FALSE
                  )
                  
                  fit <- tryCatch(
                    stats::lm(y ~ ., data = vifData),
                    error = function(e) NULL
                  )
                  
                  if (! is.null(fit)) {
                    r2 <- summary(fit)$r.squared
                    
                    if (is.finite(r2)) {
                      if (r2 >= 1 - sqrt(.Machine$double.eps))
                        vif <- Inf
                      else
                        vif <- 1 / (1 - r2)
                    }
                  }
                }
              }
            }
            
            if (is.infinite(vif)) {
              statusMatrix[i, 'Redundancy'] <- 'High'
              severityMatrix[i, 'Redundancy'] <- 3
            } else if (is.finite(vif)) {
              if (vif >= 10) {
                statusMatrix[i, 'Redundancy'] <- 'High'
                severityMatrix[i, 'Redundancy'] <- 3
              } else if (vif >= 5) {
                statusMatrix[i, 'Redundancy'] <- 'Moderate'
                severityMatrix[i, 'Redundancy'] <- 2
              } else {
                statusMatrix[i, 'Redundancy'] <- 'Low'
                severityMatrix[i, 'Redundancy'] <- 0
              }
            }
          }
        }
        
        # Data-quality classification
        for (i in seq_along(vars)) {
          var <- vars[i]
          x <- private$.numeric(data[[var]])
          
          valid <- is.finite(x)
          validN <- sum(valid)
          missing <- length(x) - validN
          
          missingPct <- if (length(x) > 0)
            missing / length(x) * 100
          else
            NA_real_
          
          variance <- NA_real_
          if (validN >= 2)
            variance <- stats::var(x[valid])
          
          status <- 'OK'
          severity <- 0
          
          if (validN < 3) {
            status <- 'Insufficient'
            severity <- 3
          } else if (is.finite(variance) && variance <= 0) {
            status <- 'Constant'
            severity <- 3
          } else if (is.finite(missingPct) && missingPct >= 20) {
            status <- 'High missingness'
            severity <- 2
          }
          
          statusMatrix[i, 'Data Quality'] <- status
          severityMatrix[i, 'Data Quality'] <- severity
        }
        
        self$results$diagnosticMap$setState(
          list(
            variables = vars,
            diagnostics = diagnostics,
            status = statusMatrix,
            severity = severityMatrix
          )
        )
      }
      
      # ------------------------------------------------------------
      # Profile Contrast Map
      # ------------------------------------------------------------
      
      if (isTRUE(self$options$contrastMap)) {
        
        groupVar <- self$options$group
        
        state <- list(
          valid = FALSE,
          message = 'Select a Grouping Variable to display profile contrasts.'
        )
        
        if (! is.null(groupVar) &&
            length(groupVar) == 1 &&
            groupVar != '' &&
            groupVar %in% names(data) &&
            length(vars) >= 2) {
          
          X <- private$.numericMatrix(data, vars)
          g <- data[[groupVar]]
          
          if (! is.factor(g))
            g <- factor(g)
          
          complete <- stats::complete.cases(X) & !is.na(g)
          
          Xcc <- X[complete, , drop = FALSE]
          gcc <- droplevels(g[complete])
          
          if (nrow(Xcc) >= 3 && nlevels(gcc) >= 2) {
            
            # Remove constant measures before standardization
            sdValues <- apply(Xcc, 2, stats::sd)
            keep <- is.finite(sdValues) & sdValues > 0
            
            Xuse <- Xcc[, keep, drop = FALSE]
            plotVars <- colnames(Xuse)
            
            if (ncol(Xuse) >= 2) {
              
              # Use the same pooled standardization as Text Profile Plot
              means <- colMeans(Xuse)
              sds <- apply(Xuse, 2, stats::sd)
              
              Z <- sweep(Xuse, 2, means, FUN = '-')
              Z <- sweep(Z, 2, sds, FUN = '/')
              
              groupLevels <- levels(gcc)
              
              profileMeans <- lapply(
                groupLevels,
                function(level) {
                  
                  rows <- gcc == level
                  
                  colMeans(
                    Z[rows, , drop = FALSE],
                    na.rm = TRUE
                  )
                }
              )
              
              profileMeans <- do.call(rbind, profileMeans)
              rownames(profileMeans) <- groupLevels
              colnames(profileMeans) <- plotVars
              
              # ----------------------------------------------------
              # Pairwise profile contrasts
              #
              # Delta z = mean z of first group
              #           - mean z of second group
              # ----------------------------------------------------
              
              pairIndex <- utils::combn(
                seq_along(groupLevels),
                2
              )
              
              contrastLabels <- character(ncol(pairIndex))
              
              contrastMatrix <- matrix(
                NA_real_,
                nrow = ncol(pairIndex),
                ncol = length(plotVars),
                dimnames = list(
                  NULL,
                  plotVars
                )
              )
              
              for (k in seq_len(ncol(pairIndex))) {
                
                i <- pairIndex[1, k]
                j <- pairIndex[2, k]
                
                contrastLabels[k] <- paste0(
                  groupLevels[i],
                  ' \u2212 ',
                  groupLevels[j]
                )
                
                contrastMatrix[k, ] <-
                  profileMeans[i, ] -
                  profileMeans[j, ]
              }
              
              rownames(contrastMatrix) <- contrastLabels
              
              state <- list(
                valid = TRUE,
                variables = plotVars,
                contrasts = contrastLabels,
                delta = contrastMatrix
              )
              
            } else {
              
              state$message <- paste(
                'At least two non-constant Text Variables are required',
                'for the Profile Contrast Map.'
              )
            }
            
          } else {
            
            state$message <- paste(
              'At least two observed groups and sufficient complete cases',
              'are required for the Profile Contrast Map.'
            )
          }
          
        } else if (length(vars) < 2) {
          
          state$message <- paste(
            'At least two Text Variables are required',
            'for the Profile Contrast Map.'
          )
        }
        
        self$results$contrastMap$setState(state)
      }
      
    },
    
    
    
    # ================================================================
    # Text Profile Plot
    # ================================================================
    .plotProfile = function(
    image,
    ggtheme,
    theme,
    ...
    ) {
      
      state <- image$state
      
      if (is.null(state))
        return(FALSE)
      
      if (isFALSE(state$valid)) {
        
        message <- if (!is.null(state$message))
          as.character(state$message)
        else
          'The Text Profile Plot is not available for the current selections.'
        
        plot <- ggplot2::ggplot() +
          ggplot2::annotate(
            'text',
            x = 0,
            y = 0,
            label = message,
            size = 4
          ) +
          ggplot2::xlim(-1, 1) +
          ggplot2::ylim(-1, 1) +
          ggplot2::labs(x = NULL, y = NULL) +
          ggtheme +
          ggplot2::theme(
            axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank()
          )
        
        print(plot)
        return(TRUE)
      }
      
      if (is.null(state$variables) ||
          is.null(state$groups) ||
          is.null(state$means))
        return(FALSE)
      
      variables <- as.character(state$variables)
      groups <- as.character(state$groups)
      means <- as.matrix(state$means)
      
      if (length(variables) < 2 ||
          length(groups) < 2 ||
          nrow(means) != length(groups) ||
          ncol(means) != length(variables))
        return(FALSE)
      
      plotData <- data.frame(
        Variable = factor(
          rep(variables, times = length(groups)),
          levels = variables
        ),
        Group = factor(
          rep(groups, each = length(variables)),
          levels = groups
        ),
        MeanZ = as.vector(t(means)),
        stringsAsFactors = FALSE
      )
      
      plotData <- plotData[
        is.finite(plotData$MeanZ),
        ,
        drop = FALSE
      ]
      
      if (nrow(plotData) == 0)
        return(FALSE)
      
      palette <- grDevices::hcl.colors(
        max(3, length(groups)),
        palette = 'Dark 3'
      )[seq_along(groups)]
      
      plot <- ggplot2::ggplot(
        plotData,
        ggplot2::aes(
          x = Variable,
          y = MeanZ,
          group = Group,
          colour = Group
        )
      ) +
        ggplot2::geom_hline(
          yintercept = 0,
          linetype = 'dashed',
          linewidth = 0.5,
          colour = '#777777'
        ) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 2.8) +
        ggplot2::scale_colour_manual(values = palette) +
        ggplot2::labs(
          x = 'Text Measure',
          y = 'Mean Standardized Score (z)',
          colour = 'Group'
        ) +
        ggtheme +
        ggplot2::theme(
          legend.position = 'right'
        )
      
      # plot <- plot +
      #   ggplot2::theme(
      #     axis.text.x = ggplot2::element_text(
      #       angle = self$options$angle,
      #       hjust = if (self$options$angle == 0) 0.5 else 1
      #     )
      #   )
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
    
    
    # ================================================================
    # Profile Diagnostic Map
    # ================================================================
    
    .plotDiagnosticMap = function(
    image,
    ggtheme,
    theme,
    ...
    ) {
      
      state <- image$state
      
      if (is.null(state))
        return(FALSE)
      
      if (is.null(state$variables) ||
          is.null(state$diagnostics) ||
          is.null(state$status) ||
          is.null(state$severity))
        return(FALSE)
      
      variables <- as.character(state$variables)
      diagnostics <- as.character(state$diagnostics)
      status <- as.matrix(state$status)
      severity <- as.matrix(state$severity)
      
      if (length(variables) == 0 ||
          nrow(status) != length(variables) ||
          ncol(status) != length(diagnostics) ||
          !all(dim(status) == dim(severity)))
        return(FALSE)
      
      plotData <- expand.grid(
        Variable = variables,
        Diagnostic = diagnostics,
        stringsAsFactors = FALSE
      )
      
      plotData$Variable <- factor(
        plotData$Variable,
        levels = rev(variables)
      )
      
      plotData$Diagnostic <- factor(
        plotData$Diagnostic,
        levels = diagnostics
      )
      
      plotData$Status <- as.vector(status)
      plotData$Severity <- as.vector(severity)
      
      plotData$Level <- ifelse(
        is.na(plotData$Severity),
        'Not available',
        ifelse(
          plotData$Severity >= 3,
          'High concern',
          ifelse(
            plotData$Severity >= 2,
            'Moderate concern',
            ifelse(
              plotData$Severity >= 1,
              'Low concern',
              'No/low concern'
            )
          )
        )
      )
      
      plotData$Level <- factor(
        plotData$Level,
        levels = c(
          'No/low concern',
          'Low concern',
          'Moderate concern',
          'High concern',
          'Not available'
        )
      )
      
      fillValues <- c(
        'No/low concern' = '#2E7D32',
        'Low concern' = '#F9A825',
        'Moderate concern' = '#EF6C00',
        'High concern' = '#C62828',
        'Not available' = '#8A8A8A'
      )
      
      textColour <- ifelse(
        plotData$Level == 'Low concern',
        '#222222',
        '#FFFFFF'
      )
      
      plot <- ggplot2::ggplot(
        plotData,
        ggplot2::aes(
          x = Diagnostic,
          y = Variable,
          fill = Level
        )
      ) +
        ggplot2::geom_tile(
          colour = '#FFFFFF',
          linewidth = 0.8
        ) +
        ggplot2::geom_text(
          ggplot2::aes(label = Status),
          colour = textColour,
          size = 3.4
        ) +
        ggplot2::scale_fill_manual(
          values = fillValues,
          drop = FALSE
        ) +
        ggplot2::labs(
          x = NULL,
          y = NULL,
          fill = 'Diagnostic level'
        ) +
        ggtheme +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.position = 'right'
        )
      
      print(plot)
      TRUE
    },
    
    # ================================================================
    # Profile Contrast Map
    # ================================================================
    
    # ================================================================
    # Profile Contrast Map
    # ================================================================
    
    .plotContrastMap = function(
    image,
    ggtheme,
    theme,
    ...
    ) {
      
      state <- image$state
      
      if (is.null(state))
        return(FALSE)
      
      # ------------------------------------------------------------
      # Invalid state
      # ------------------------------------------------------------
      
      if (isFALSE(state$valid)) {
        
        message <- if (!is.null(state$message))
          as.character(state$message)
        else
          'The Profile Contrast Map is not available for the current selections.'
        
        plot <- ggplot2::ggplot() +
          ggplot2::annotate(
            'text',
            x = 0,
            y = 0,
            label = message,
            size = 4
          ) +
          ggplot2::xlim(-1, 1) +
          ggplot2::ylim(-1, 1) +
          ggplot2::labs(
            x = NULL,
            y = NULL
          ) +
          ggtheme +
          ggplot2::theme(
            axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank()
          )
        
        print(plot)
        
        return(TRUE)
      }
      
      if (is.null(state$variables) ||
          is.null(state$contrasts) ||
          is.null(state$delta))
        return(FALSE)
      
      variables <- as.character(state$variables)
      contrasts <- as.character(state$contrasts)
      delta <- as.matrix(state$delta)
      
      if (length(variables) < 2 ||
          length(contrasts) < 1 ||
          nrow(delta) != length(contrasts) ||
          ncol(delta) != length(variables))
        return(FALSE)
      
      # ------------------------------------------------------------
      # Plot data
      # ------------------------------------------------------------
      
      plotData <- expand.grid(
        Variable = variables,
        Contrast = contrasts,
        stringsAsFactors = FALSE
      )
      
      plotData$DeltaZ <- as.vector(t(delta))
      
      plotData <- plotData[
        is.finite(plotData$DeltaZ),
        ,
        drop = FALSE
      ]
      
      if (nrow(plotData) == 0)
        return(FALSE)
      
      plotData$Variable <- factor(
        plotData$Variable,
        levels = rev(variables)
      )
      
      plotData$Contrast <- factor(
        plotData$Contrast,
        levels = contrasts
      )
      
      # ------------------------------------------------------------
      # Largest profile contrast
      # ------------------------------------------------------------
      
      largestIndex <- which.max(
        abs(plotData$DeltaZ)
      )
      
      largestContrast <- as.character(
        plotData$Contrast[largestIndex]
      )
      
      largestVariable <- as.character(
        plotData$Variable[largestIndex]
      )
      
      largestDelta <- plotData$DeltaZ[largestIndex]
      
      summaryText <- sprintf(
        'Largest: %s | %s | \u0394z = %+.2f',
        largestContrast,
        largestVariable,
        largestDelta
      )
      
      # Automatically rotate labels when many contrasts are displayed
      contrastAngle <- if (length(contrasts) >= 5) 45 else 0
      
      # ------------------------------------------------------------
      # Symmetric colour scale centred at zero
      # ------------------------------------------------------------
      
      maxAbs <- max(
        abs(plotData$DeltaZ),
        na.rm = TRUE
      )
      
      if (!is.finite(maxAbs) ||
          maxAbs <= sqrt(.Machine$double.eps))
        maxAbs <- 1
      
      textColour <- ifelse(
        abs(plotData$DeltaZ) >= 0.60 * maxAbs,
        '#FFFFFF',
        '#222222'
      )
      
      # ------------------------------------------------------------
      # Plot
      # ------------------------------------------------------------
      
      plot <- ggplot2::ggplot(
        plotData,
        ggplot2::aes(
          x = Contrast,
          y = Variable,
          fill = DeltaZ
        )
      ) +
        ggplot2::geom_tile(
          colour = '#FFFFFF',
          linewidth = 0.8
        ) +
        ggplot2::geom_text(
          label = sprintf(
            '%+.2f',
            plotData$DeltaZ
          ),
          colour = textColour,
          size = 3.5
        ) +
        
        ggtheme +
        
        ggplot2::scale_fill_gradient2(
          low = '#C62828',
          mid = '#FFFFFF',
          high = '#1565C0',
          midpoint = 0,
          limits = c(
            -maxAbs,
            maxAbs
          ),
          name = expression(Delta * z)
        ) +
        
        ggplot2::labs(
          subtitle = summaryText,
          x = 'Group Contrast',
          y = NULL
        ) +
        
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.position = 'right',
          axis.text.x = ggplot2::element_text(
            angle = contrastAngle,
            hjust = if (contrastAngle == 0) 0.5 else 1
          )
        )
      
      print(plot)
      
      TRUE
    },
    
    # ================================================================
    # Helper: safe numeric conversion
    # ================================================================
    
    .numeric = function(x) {
      
      result <- suppressWarnings(
        jmvcore::toNumeric(x)
      )
      
      as.numeric(result)
    },
    
    
    # ================================================================
    # Helper: create numeric matrix from selected variables
    # ================================================================
    
    .numericMatrix = function(data, vars) {
      
      columns <- lapply(
        vars,
        function(var) {
          private$.numeric(
            data[[var]]
          )
        }
      )
      
      X <- do.call(
        cbind,
        columns
      )
      
      colnames(X) <- vars
      
      X
    },
    
    
    # ================================================================
    # Helper: classify length-related explained variance
    # ================================================================
    
    .lengthEffect = function(r2) {
      
      if (! is.finite(r2))
        return('')
      
      if (r2 < .01)
        return('Negligible')
      
      if (r2 < .09)
        return('Small')
      
      if (r2 < .25)
        return('Moderate')
      
      'Large'
    }
    
  )
)