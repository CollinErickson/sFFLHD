#' @export
sFFLHDmm <- R6::R6Class(classname="sFFLHDmm",
    public = list(
      s = NULL, # keep a sFFLHD
      b = 0,
      have_choices = FALSE,
      Xchoices = list(),
      X = NULL,
      D = NULL,
      L = NULL,
      initialize = function(D, L, ...) {
        self$s <- sFFLHD::sFFLHD(D=D, L=L, ...)
        self$X <- matrix(NA, nrow=0, ncol=D)
        self$D <- D
        self$L <- L
      },
      get.batch = function() {#browser()
        if (self$b == 0) {
          bat <- self$s$get.batch()
        } else {
          if (length(self$Xchoices) == 0) {
            Xnew <- self$s$get.batches.to.golden()
            Xnewchoices <- split_matrix(mat=Xnew, rowspergroup = self$s$L, shuffle = FALSE)
            self$Xchoices <- Xnewchoices #c(self$Xchoices, )
            #self$s$get.batches.to.golden()
            #self$have_choices <- TRUE
          }
          #choices <- (nrow(s$Xb)) / s$L - self$b
          if (length(self$Xchoices) == 1) {
            selection <- 1
          } else {
          # now we have choices, do maximin
            mindists <- sapply(self$Xchoices, function(xx) {self$mindist.1(self$X, xx)})
            selection <- which.max(mindists)
          }
          bat <- self$Xchoices[[selection]]
          self$Xchoices[[selection]] <- NULL
        }
        self$X <- rbind(self$X, bat)
        self$b <- self$b + 1
        bat
      },
      maximin = function() {
        for (i in 1:choices) {
          mindists[i] <- self$mindist.1(self$s$Xb[1:(self$b * self$s$L)], self$s$Xb)
        }
      },
      #mindist.all = function(Xlist, X1) {
      #  min(sapply(Xlist, function(aa){self$mindist.1(aa, X1)}))
      #}
      mindist.1 = function(a,b) {#browser()
        # a and b are matrices, points along row
        min(outer(1:nrow(a), 1:nrow(b), Vectorize(function(i,j) {sum((a[i,] - b[j,])^2)})))
      },

      get.batches = function(num) { # get multiple batches at once
        out <- matrix(NA, nrow=0,ncol=self$D)
        for (i in 1:num) {out <- rbind(out,self$get.batch())}
        return(out)
      } # end get.batches function
    )
)
