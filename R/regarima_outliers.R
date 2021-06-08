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
                    .JD3_ENV$ts_r2jd(y), as.integer(order), as.integer(seasonal), mean, .JD3_ENV$matrix_r2jd(X),
                 ao, ls, tc, so, cv)
  model<-list(
    y=as.numeric(y),
    variables=.JD3_ENV$proc_vector(jregarima, "variables"),
    X=.JD3_ENV$proc_matrix(jregarima, "regressors"),
    b=.JD3_ENV$proc_vector(jregarima, "b"),
    bcov=.JD3_ENV$proc_matrix(jregarima, "bvar"),
    linearized=.JD3_ENV$proc_vector(jregarima, "linearized")
  )

  ll0<-.JD3_ENV$proc_likelihood(jregarima, "initiallikelihood.")
  ll1<-.JD3_ENV$proc_likelihood(jregarima, "finallikelihood.")

  return(structure(list(
    model=model,
    likelihood=list(initial=ll0, final=ll1)),
    class="JDSTS"))
}
