#' sFFLHD R6 object that gives a batch of points at a time using maximin.
#' To do this it takes all batches for stage at beginning of stage
#' and then reorders them. Not that great in practice.
#'
#' @field D numeric. The number of dimensions for the design. Must be set.
#' @field L numeric. The number of points in each batch, also the number of
#'  levels of each dimension. Must be set.
#' @field maximin logical. Should maximin distance be used to space out points?
#' @field a numeric. A root of L that determines the intermediate stages.
#' Is automatically set to smallest possible value, which is recommended.
#' @field b integer. The batch number.
#' @field nb integer. The number of points selected so far.
#' @field lb numeric. Current levels of the small grid.
#' @field Lb numeric. Current levels of the intermediate grid.
#' @field Xb matrix. Current design matrix, continuous from 0 to 1.
#' @field Vb matrix. Small grid design.
#' @field Mb matrix. Intermediate grid design.
#' @field Wb matrix. Big grid design.
#' @field A1 matrix. The first OA slice.
#' @field r integer. Used to keep track of loop index.
#' @field p integer. Used to keep track of loop index.
#' @field Ar matrix. Current Ar.
#' @field stage integer. Current stage.
#' @field vii integer. Used to keep track of location in stage 2.
#' @field Fslices list. A list of slices.
#' @field FF1.1 matrix. Temporary matrix used to generate slices.
#' @field Mb.store matrix. Temporary storage of Mb.
#' @field v.shuffle integer. A storage value for storing order.
#' Requires extra optimization.
#'
#' @return A sFFLHDmm object
#'
#' @export
#'
#' @importFrom stats runif
#' @importFrom methods new
#' @importFrom conf.design factorize
#' @importFrom DoE.base oa.design
#'
#' @examples
#' s <- sFFLHDmm$new(D=2,L=3)
#' s$get.batch()
#' s <- sFFLHDmm$new(D=2,L=4)
#' s$get.batch()
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
