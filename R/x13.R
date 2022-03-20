#' @include utils.R x13_spec.R x13_rslts.R
NULL

#' RegARIMA model, pre-adjustment in X13
#'
#' @param ts a univariate time series
#' @param spec the model specification. Can be either the name of a predifined specification (`"rg0"`, `"rg1"`, `"rg2c"`, `"rg3"`, `"rg4"` or `"rg5c"`, see details) or a user-defined specification.
#' @param context the dictionnary of variables.
#' @details
#' The available predefined 'JDemetra+' model specifications are described in the table below:
#'
#' \tabular{rrrrrrr}{
#' \strong{Identifier} |\tab \strong{Log/level detection} |\tab \strong{Outliers detection} |\tab \strong{Calendar effects} |\tab \strong{ARIMA}\cr
#' RG0 |\tab \emph{NA} |\tab \emph{NA} |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RG1 |\tab automatic |\tab AO/LS/TC  |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RG2c |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab Airline(+mean)\cr
#' RG3 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab automatic\cr
#' RG4c |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab automatic\cr
#' RG5c |\tab automatic |\tab AO/LS/TC |\tab 7 td vars + Easter |\tab automatic
#' }
#' @md
#' @return
#' @export
#'
#' @examples
regarima<-function(ts, spec="rg4", context=NULL){
  jts<-rjd3toolkit:::ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/x13/r/RegArima", "Ljdplus/x13/regarima/RegArimaOutput;", "fullProcess", jts, spec)
  }else{
    jspec<-r2jd_spec_regarima(spec)
    if (is.null(context)){
      context<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/x13/r/RegArima", "Ljdplus/x13/regarima/RegArimaOutput;", "fullProcess", jts, jspec, context)
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (regarima_output(jrslt))
  }
}

#' @export
#' @rdname regarima
fast.regarima<-function(ts, spec="rg4", context=NULL){
  jts<-rjd3toolkit:::ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/x13/r/RegArima", "Ljdplus/regsarima/regular/RegSarimaModel;", "process", jts, spec)
  }else{
    jspec<-r2jd_spec_regarima(spec)
    if (is.null(context)){
      context<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/x13/r/RegArima", "Ljdplus/regsarima/regular/RegSarimaModel;", "process", jts, jspec, context )
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (regarima_rslts(jrslt))
  }
}

regarima_output<-function(jq){
  if (is.jnull(jq))
    return (NULL)
  q<-.jcall("demetra/x13/r/RegArima", "[B", "toBuffer", jq)
  p<-RProtoBuf::read(x13.RegArimaOutput, q)
  return (structure(list(
    result=rjd3modelling:::p2r_regarima_rslts(p$result),
    estimation_spec=p2r_spec_regarima(p$estimation_spec),
    result_spec=p2r_spec_regarima(p$result_spec)
  ),
  class="JD3_REGARIMA_OUTPUT")
  )
}

#' Seasonal Adjustment with  X13-ARIMA
#'
#' @inheritParams regarima
#' @param spec the model specification. Can be either the name of a predifined specification (`"rsa0"`, `"rsa1"`, `"rsa2c"`, `"rsa3"`, `"rsa4"` or `"rsa5c"`, see details) or a user-defined specification.
#'
#' The available predefined 'JDemetra+' model specifications are described in the table below:
#'
#' \tabular{rrrrrrr}{
#' \strong{Identifier} |\tab \strong{Log/level detection} |\tab \strong{Outliers detection} |\tab \strong{Calendar effects} |\tab \strong{ARIMA}\cr
#' RSA0 |\tab \emph{NA} |\tab \emph{NA} |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RSA1 |\tab automatic |\tab AO/LS/TC  |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RSA2c |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab Airline(+mean)\cr
#' RSA3 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab automatic\cr
#' RSA4c |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab automatic\cr
#' RSA5c |\tab automatic |\tab AO/LS/TC |\tab 7 td vars + Easter |\tab automatic\cr
#' }
#' @export
#'
#' @examples
x13<-function(ts, spec="rsa4", context=NULL){
  jts<-rjd3toolkit:::ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/x13/r/X13", "Ldemetra/x13/io/protobuf/X13Output;", "fullProcess", jts, spec)
  }else{
    jspec<-r2jd_spec_x13(spec)
    if (is.null(context)){
      context<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/x13/r/X13", "Ldemetra/x13/io/protobuf/X13Output;", "fullProcess", jts, jspec, context )
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (x13_output(jrslt))
  }
}


#' @export
#' @rdname x13
fast.x13<-function(ts, spec="rsa4", context=NULL){
  jts<-rjd3toolkit:::ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/x13/r/X13", "Ljdplus/x13/X13Results;", "process", jts, spec)
  }else{
    jspec<-r2jd_spec_x13(spec)
    if (is.null(context)){
      context<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/x13/r/X13", "Ljdplus/x13/X13Results;", "process", jts, jspec, context)
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (x13_rslts(jrslt))
  }
}

x13_output<-function(jq){
  if (is.jnull(jq))
    return (NULL)
  q<-.jcall("demetra/x13/r/X13", "[B", "toBuffer", jq)
  p<-RProtoBuf::read(x13.X13Output, q)
  return (structure(list(
    result=p2r_x13_rslts(p$result),
    estimation_spec=p2r_spec_x13(p$estimation_spec),
    result_spec=p2r_spec_x13(p$result_spec)
  ),
  class="JD3_X13_OUTPUT")
  )

}

#' X11 Decomposition Algorithm
#'
#' @inheritParams x13
#' @param spec the specification
#'
#' @return
#' @export
#'
#' @examples
x11<-function(ts, spec){
  jts<-rjd3toolkit:::ts_r2jd(ts)
  jspec<-r2jd_spec_x11(spec)
  jrslt<-.jcall("demetra/x13/r/X11", "Ljdplus/x11/X11Results;", "process", jts, jspec)
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    return (x11_rslts(jrslt))
  }
}

#' Title
#'
#' @param spec
#' @param refspec
#' @param policy
#' @param period
#' @param start
#' @param end
#'
#' @return
#' @export
#'
#' @examples
regarima.refresh<-function(spec, refspec=NULL, policy=c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers", "FixedParameters", "FixedAutoRegressiveParameters", "Fixed"), period=0, start=NULL, end=NULL){
  policy=match.arg(policy)
  if (class(spec) != "JD3_REGARIMA_SPEC") stop("Invalid specification type")
  jspec<-r2jd_spec_regarima(spec)
  if (is.null(refspec)){
    jrefspec<-.jcall("demetra/regarima/RegArimaSpec", "Ldemetra/regarima/RegArimaSpec;", "fromString", "rg4")

  }else{
    if (class(refspec) != "JD3_REGARIMA_SPEC") stop("Invalid specification type")
    jrefspec<-r2jd_spec_regarima(refspec)
  }
  jdom<-rjd3toolkit:::jdomain(period, start, end)
  jnspec<-.jcall("demetra/x13/r/RegArima", "Ldemetra/regarima/RegArimaSpec;", "refreshSpec", jspec, jrefspec, jdom, policy)
  return (jd2r_spec_regarima(jnspec))
}

#' Title
#'
#' @param spec
#' @param refspec
#' @param policy
#' @param period
#' @param start
#' @param end
#'
#' @return
#' @export
#'
#' @examples
x13.refresh<-function(spec, refspec=NULL, policy=c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers", "FixedParameters", "FixedAutoRegressiveParameters", "Fixed", "Current"), period=0, start=NULL, end=NULL){
  policy=match.arg(policy)
  if (class(spec) != "JD3_X13_SPEC") stop("Invalid specification type")
  jspec<-r2jd_spec_x13(spec)
  if (is.null(refspec)){
    jrefspec<-.jcall("demetra/x13/X13Spec", "Ldemetra/x13/X13Spec;", "fromString", "rsa4")

  }else{
    if (class(refspec) != "JD3_X13_SPEC") stop("Invalid specification type")
    jrefspec<-r2jd_spec_x13(refspec)
  }
  jdom<-rjd3toolkit:::jdomain(period, start, end)
  jnspec<-.jcall("demetra/x13/r/X13", "Ldemetra/x13/X13Spec;", "refreshSpec", jspec, jrefspec, jdom, policy)
  return (jd2r_spec_x13(jnspec))

}

