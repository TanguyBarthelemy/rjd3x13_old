#' @include utils.R x13_spec.R x13_rslts.R
NULL

#' RegARIMA model, pre-adjustment in X13
#'
#' @param ts a univariate time series.
#' @param spec the model specification. Can be either the name of a predefined specification or a user-defined specification.
#' @param context the dictionnary of variables.
#' @param userdefined a vector containing the additional output variables.
#'
#' @return the `regarima()` function returns a list with the results (`"JD3_REGARIMA_RSLTS"` object), the estimation specification and the result specification, while `fast.regarima()` is a faster function that only returns the results.
#'
#' @examples
#' y = rjd3toolkit::ABS$X0.2.09.10.M
#' sp = spec_regarima_default("rg5c")
#' sp = rjd3toolkit::add_outlier(sp,
#'                  type = c("AO"), c("2015-01-01", "2010-01-01"))
#' fast.regarima(y, spec = sp)
#' sp = rjd3toolkit::set_transform(
#'    rjd3toolkit::set_tradingdays(
#'      rjd3toolkit::set_easter(sp, enabled = FALSE),
#'     option = "workingdays"
#'   ),
#'   fun = "None"
#' )
#' fast.regarima(y, spec = sp)
#' sp =  rjd3toolkit::set_outlier(sp, outliers.type = c("AO"))
#' fast.regarima(y, spec = sp)
#' @export
regarima<-function(ts, spec=c("rg4", "rg0", "rg1", "rg2c", "rg3","rg5c"), context=NULL, userdefined = NULL){
  jts<-rjd3toolkit::ts_r2jd(ts)
  if (is.character(spec)){
    spec = gsub("sa", "g", tolower(spec), fixed = TRUE)
    spec = match.arg(spec[1],
                     choices = c("rg0", "rg1", "rg2c", "rg3","rg4", "rg5c")
    )
    jrslt<-.jcall("demetra/x13/r/RegArima", "Ljdplus/x13/regarima/RegArimaOutput;", "fullProcess", jts, spec)
  }else{
    jspec<-r2jd_spec_regarima(spec)
    if (is.null(context)){
      jcontext <- .jnull("demetra/timeseries/regression/ModellingContext")
    } else {
      jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
    }
    jrslt<-.jcall("demetra/x13/r/RegArima", "Ljdplus/x13/regarima/RegArimaOutput;", "fullProcess", jts, jspec, jcontext)
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = regarima_output(jrslt)
    return (add_ud_var(res, jrslt, userdefined = userdefined))
  }
}
#' @export
#' @rdname regarima
fast.regarima<-function(ts, spec= c("rg4", "rg0", "rg1", "rg2c", "rg3","rg5c"), context=NULL, userdefined = NULL){
  jts<-rjd3toolkit::ts_r2jd(ts)
  if (is.character(spec)){
    spec = gsub("sa", "g", tolower(spec), fixed = TRUE)
    spec = match.arg(spec[1],
                     choices = c("rg0", "rg1", "rg2c", "rg3","rg4", "rg5c")
    )
    jrslt<-.jcall("demetra/x13/r/RegArima", "Ljdplus/regsarima/regular/RegSarimaModel;", "process", jts, spec)
  }else{
    jspec<-r2jd_spec_regarima(spec)
    if (is.null(context)){
      jcontext <- .jnull("demetra/timeseries/regression/ModellingContext")
    } else {
      jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
    }
    jrslt<-.jcall("demetra/x13/r/RegArima", "Ljdplus/regsarima/regular/RegSarimaModel;", "process", jts, jspec, jcontext)
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = regarima_rslts(jrslt)
    return (add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
  }
}

regarima_output<-function(jq){
  if (is.jnull(jq))
    return (NULL)
  q<-.jcall("demetra/x13/r/RegArima", "[B", "toBuffer", jq)
  p<-RProtoBuf::read(x13.RegArimaOutput, q)
  return (structure(list(
    result=rjd3toolkit::p2r_regarima_rslts(p$result),
    estimation_spec=p2r_spec_regarima(p$estimation_spec),
    result_spec=p2r_spec_regarima(p$result_spec)
  ),
  class="JD3_REGARIMA_OUTPUT")
  )
}

#' Seasonal Adjustment with  X13-ARIMA
#'
#' @inheritParams regarima
#'
#' @examples
#' sp = spec_x13_default("rg5c")
#' y = rjd3toolkit::ABS$X0.2.09.10.M
#' fast.x13(y, spec = sp)
#' sp = rjd3toolkit::add_outlier(sp,
#'                  type = c("AO"), c("2015-01-01", "2010-01-01"))
#' sp =  rjd3toolkit::set_transform(
#'    rjd3toolkit::set_tradingdays(
#'      rjd3toolkit::set_easter(sp, enabled = FALSE),
#'     option = "workingdays"
#'   ),
#'   fun = "None"
#' )
#' sp = set_x11(sp,
#'              henderson.filter = 13)
#' fast.x13(y, spec = sp)
#'
#' @return the `x13()` function returns a list with the results, the estimation specification and the result specification, while `fast.x13()` is a faster function that only returns the results.
#' The `jx13()` functions only results the java object to custom outputs in other packages (use [rjd3toolkit::dictionary()] to
#' get the list of variables and [rjd3toolkit::result()] to get a specific variable).
#' @export
x13<-function(ts, spec=c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"), context=NULL, userdefined = NULL){
  jts<-rjd3toolkit::ts_r2jd(ts)
  if (is.character(spec)){
    spec = gsub("g", "sa", tolower(spec), fixed = TRUE)
    spec = match.arg(spec[1],
                     choices = c("rsa0", "rsa1", "rsa2c", "rsa3","rsa4", "rsa5c")
    )
    jrslt<-.jcall("demetra/x13/r/X13", "Ljdplus/x13/X13Output;", "fullProcess", jts, spec)
  }else{
    jspec<-r2jd_spec_x13(spec)
    if (is.null(context)){
      jcontext <- .jnull("demetra/timeseries/regression/ModellingContext")
    } else {
      jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
    }
    jrslt<-.jcall("demetra/x13/r/X13", "Ljdplus/x13/X13Output;", "fullProcess", jts, jspec, jcontext)
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = x13_output(jrslt)
    return (add_ud_var(res, jrslt, userdefined = userdefined, out_class = "Ljdplus/x13/X13Results;"))
  }
}


#' @export
#' @rdname x13
fast.x13<-function(ts, spec=c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"), context=NULL, userdefined = NULL){
  jts<-rjd3toolkit::ts_r2jd(ts)
  if (is.character(spec)){
    spec = gsub("g", "sa", tolower(spec), fixed = TRUE)
    spec = match.arg(spec[1],
                     choices = c("rsa0", "rsa1", "rsa2c", "rsa3","rsa4", "rsa5c")
    )
    jrslt<-.jcall("demetra/x13/r/X13", "Ljdplus/x13/X13Results;", "process", jts, spec)
  }else{
    jspec<-r2jd_spec_x13(spec)
    if (is.null(context)){
      jcontext <- .jnull("demetra/timeseries/regression/ModellingContext")
    } else {
      jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
    }
    jrslt<-.jcall("demetra/x13/r/X13", "Ljdplus/x13/X13Results;", "process", jts, jspec, jcontext)
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = x13_rslts(jrslt)
    return (add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
  }
}

#' @export
#' @rdname x13
jx13<-function(ts, spec=c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"), context=NULL, userdefined = NULL){
  jts<-rjd3toolkit::ts_r2jd(ts)
  if (is.character(spec)){
    spec = gsub("g", "sa", tolower(spec), fixed = TRUE)
    spec = match.arg(spec[1],
                     choices = c("rsa0", "rsa1", "rsa2c", "rsa3","rsa4", "rsa5c")
    )
    jrslt<-.jcall("demetra/x13/r/X13", "Ljdplus/x13/X13Results;", "process", jts, spec)
  }else{
    jspec<-r2jd_spec_x13(spec)
    if (is.null(context)){
      jcontext <- .jnull("demetra/timeseries/regression/ModellingContext")
    } else {
      jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
    }
    jrslt<-.jcall("demetra/x13/r/X13", "Ljdplus/x13/X13Results;", "process", jts, jspec, jcontext)
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = rjd3toolkit::jd3Object(jrslt, result = TRUE)
    return (res)
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

#' X-11 Decomposition Algorithm
#'
#' @inheritParams x13
#' @param spec the specification.
#'
#' @examples
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' x11_spec <- spec_x11_default()
#' x11(y, x11_spec)
#' x11_spec <- set_x11(x11_spec, henderson.filter = 13)
#' x11(y, x11_spec)
#' @export
x11 <- function(ts, spec = spec_x11_default(), userdefined = NULL){
  jts<-rjd3toolkit::ts_r2jd(ts)
  jspec<-r2jd_spec_x11(spec)
  jrslt<-.jcall("demetra/x13/r/X11", "Ljdplus/x11/X11Results;", "process", jts, jspec)
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = x11_rslts(jrslt)
    return (add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
  }
}

#' Refresh Policy
#'
#' @param spec the current specification
#' @param refspec the reference specification (used to defined the set of models considered).
#' By default this is the `"RG4"` or `"RSA4"` specification.
#' @param policy the refresh policy
#' @param period,start,end to specify the frozen domain when `policy` equals to `"Outliers"` or `"Outliers_StochasticComponent"`.
#'
#' @return a new specification.
#'
#' @examples
#' y = rjd3toolkit::ABS$X0.2.08.10.M
#' y_anc = window(y,end = 2009)
#' mod_anc = regarima(y_anc, spec_regarima_default())
#' res_spec = mod_anc$result_spec
#' mod_anc
#' # ARIMA parameters fixed
#' fast.regarima(y,
#'               regarima.refresh(res_spec,
#'                                mod_anc$estimation_spec,
#'                                policy = "FixedParameters"))
#' # Outlier detection
#' fast.regarima(y,
#'               regarima.refresh(res_spec,
#'                                policy = "Outliers"))
#'
#' @name refresh
#' @rdname refresh
#' @export
regarima.refresh<-function(spec, refspec=NULL, policy=c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers", "FixedParameters", "FixedAutoRegressiveParameters", "Fixed"), period=0, start=NULL, end=NULL){
  policy=match.arg(policy)
  if (!inherits(spec, "JD3_REGARIMA_SPEC"))
    stop("Invalid specification type")
  jspec<-r2jd_spec_regarima(spec)
  if (is.null(refspec)){
    jrefspec<-.jcall("demetra/regarima/RegArimaSpec", "Ldemetra/regarima/RegArimaSpec;", "fromString", "rg4")

  }else{
    if (!inherits(refspec, "JD3_REGARIMA_SPEC"))
      stop("Invalid specification type")
    jrefspec<-r2jd_spec_regarima(refspec)
  }
  jdom<-rjd3toolkit::jdomain(period, start, end)
  jnspec<-.jcall("demetra/x13/r/RegArima", "Ldemetra/regarima/RegArimaSpec;", "refreshSpec", jspec, jrefspec, jdom, policy)
  return (jd2r_spec_regarima(jnspec))
}

#' @rdname refresh
#' @export
x13.refresh<-function(spec, refspec=NULL, policy=c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers", "FixedParameters", "FixedAutoRegressiveParameters", "Fixed", "Current"), period=0, start=NULL, end=NULL){
  policy=match.arg(policy)
  if (!inherits(spec, "JD3_X13_SPEC"))
    stop("Invalid specification type")
  jspec<-r2jd_spec_x13(spec)
  if (is.null(refspec)){
    jrefspec<-.jcall("demetra/x13/X13Spec", "Ldemetra/x13/X13Spec;", "fromString", "rsa4")

  }else{
    if (!inherits(refspec, "JD3_X13_SPEC"))
      stop("Invalid specification type")
    jrefspec<-r2jd_spec_x13(refspec)
  }
  jdom<-rjd3toolkit::jdomain(period, start, end)
  jnspec<-.jcall("demetra/x13/r/X13", "Ldemetra/x13/X13Spec;", "refreshSpec", jspec, jrefspec, jdom, policy)
  return (jd2r_spec_x13(jnspec))
}

