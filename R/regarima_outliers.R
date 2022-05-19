#' @include utils.R
NULL

#' Detect Outliers in RegARIMA Model
#'
#' @param y the dependent variable (`ts` object).
#' @param order,seasonal the orders of the ARIMA model
#' @param mean boolean to include or not the mean.
#' @param X explanatory varibales.
#' @param X.td trading days regressors.
#' @param ao,ls,so,tc boolean to indicate which outliers are detected
#' @param cv  `numeric`. The entered critical value for the outliers' detection procedure.
#' If equal to 0 the critical value for the outliers' detection procedure is automatically determined
#' by the number of observations.
#'
#' @return a `"JDSTS"` object.
#'
#' @examples
#' regarima.outliers(rjd3toolkit::ABS$X0.2.09.10.M)
#' @export
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
