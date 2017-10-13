#' A RC class for ridge regression models
#'@field formula A formula
#'@field data A dataset which we apply the formula (data.frame)
#'@field lambda A numeric parameter
#'@export ridgereg
#'@export
ridgereg <- setRefClass(
  "ridgereg",
  fields = list(
    formula = "formula",
    data = "data.frame",
    call = "vector",
    beta_hat = "matrix",
    y_hat = "matrix",
    degf = "numeric",
    res_var = "matrix",
    var_beta = "vector",
    t_vals = "numeric",
    lambda = "numeric",
    x_norm = "matrix",
    beta_hat_reg = "matrix"
  ),
  methods = list(
    initialize = function(formula, data, lambda) {
      # Intercept formula and data before assignment
      call <<- c(
        "ridgereg(formula = ",
        Reduce(paste, deparse(formula)),
        ", data = ",
        deparse(substitute(data)),
        ", lambda = ",
        lambda,
        ")"
      )

      formula <<- formula
      data <<- data
      lambda <<- lambda

      X <- model.matrix(formula, data)
      X <- X[,-1] # Removes (Intercept)
      x_norm <<- scale(X)
      label <- all.vars(formula)[1]
      y <- data[[label]]
      qr_mat <- qr(x_norm)
      Q <- qr.Q(qr_mat)
      R <- qr.R(qr_mat)

      # Ridge regression coefficients
      beta_hat_reg <<-
        solve(t(R) %*% R + lambda * diag(dim(t(R) %*% R)[1])) %*% t(x_norm) %*% y

      # Fitted values
      y_hat <<- x_norm %*% beta_hat_reg # To be calculated

    },
    print = function() {
      "Prints the call of the class and ridge regression coefficients in named vector"
      # Print call
      cat("\nCall:\n")
      lapply(call, cat)

      # Print coefficients
      cat("\n\n")
      cat("Coefficients:\n")

      # Ackowledgement to Erik Herwin and Albin Västerlund
      # - since my print with return didn't pass the tests (which is bullshit)
      beta <- as.numeric(beta_hat_reg)
      namn <- row.names(beta_hat_reg)
      names(beta) <- NULL
      beta <- round(beta, 4)

      for (i in 2:length(beta)) {
        beta[i] <-
          format(beta[i],
                 width = max(nchar(beta[i]), nchar(namn[i])),
                 justify = c("right"))
        namn[i] <-
          format(namn[i],
                 width = max(nchar(beta[i]), nchar(namn[i])),
                 justify = c("right"))
      }

      beta[1] <-
        format(beta[1],
               width = max(nchar(beta[1]), nchar(namn[1]), nchar("Coefficients")),
               justify = c("right"))
      namn[1] <-
        format(namn[1],
               width = max(nchar(beta[1]), nchar(namn[1]), nchar("Coefficients")),
               justify = c("right"))

      beta[1] <- paste(beta[1], " ", sep = "")
      namn[1] <- paste(namn[1], " ", sep = "")

      cat(paste(namn, collapse = "  "),
          sep = "",
          collapse = "\n")
      cat(paste(beta, collapse = "  "),
          sep = "",
          collapse = "\n")
    },
    resid = function() {
      "Returns residual values"
      return(e_hat)
    },
    pred = function(values = NULL) {
      "Returns fitted values"
      if (!is.null(values)) {
        predict<-as.numeric(t(as.matrix(values)%*%beta_hat_reg))
        return(predict)
      } else {
        return(y_hat)
      }
    },
    coef = function() {
      "Returns the regression coefficients"
      dummy <- c(t(beta_hat_reg))
      names(dummy) <- row.names(beta_hat_reg)
      return(dummy)
    }
  )
)

