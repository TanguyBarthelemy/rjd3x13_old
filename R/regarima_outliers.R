#' @include utils.R
NULL

#' Title
#'
#' @param y
#' @param order
#' @param seasonal
#' @param mean
#' @param X
#' @param X.td
#' @param ao
#' @param ls
#' @param so
#' @param tc
#' @param cv
#'
#' @return
#' @export
#'
#' @examples
regarima.outliers<-function(y, order=c(0L,1L,1L), seasonal=c(0L,1L,1L), mean=F,
                        X=NULL, X.td=NULL, ao=T, ls=T, tc=F, so=F, cv=0){
  if (!is.ts(y)){
    stop("y must be a time series")
  }
  if (! is.null(X.td)){
    sy<-start(y)
    td<-rjd3modelling::td.forTs(y, X.td)
    X<-cbind(X, td)
  }


  jregarima<-.jcall("demetra/x13/r/RegArimaOutliersDetection", "Ldemetra/x13/r/RegArimaOutliersDetection$Results;", "process",
                    rjd3toolkit::ts_r2jd(y), as.integer(order), as.integer(seasonal), mean, rjd3toolkit::matrix_r2jd(X),
                 ao, ls, tc, so, cv)
  model<-list(
    y=as.numeric(y),
    variables=rjd3toolkit::proc_vector(jregarima, "variables"),
    X=rjd3toolkit::proc_matrix(jregarima, "regressors"),
    b=rjd3toolkit::proc_vector(jregarima, "b"),
    bcov=rjd3toolkit::proc_matrix(jregarima, "bvar"),
    linearized=rjd3toolkit::proc_vector(jregarima, "linearized")
  )

  ll0<-rjd3toolkit::proc_likelihood(jregarima, "initiallikelihood.")
  ll1<-rjd3toolkit::proc_likelihood(jregarima, "finallikelihood.")

  return(structure(list(
    model=model,
    likelihood=list(initial=ll0, final=ll1)),
    class="JDSTS"))
}
