add_ud_var <- function(x, jx, userdefined = NULL, out_class = NULL, result = FALSE){
  if (is.null(userdefined)) {
    x$user_defined = rjd3toolkit::user_defined(x, NULL)
  } else {
    if (result) {
      res = jx
    } else {
      if (is.null(out_class)) {
        res = jx$getResult()
      } else {
        res = .jcall(jx, out_class, "getResult")
      }
    }
    res = rjd3toolkit::jd3Object(res, result = TRUE)
    x$user_defined = rjd3toolkit::user_defined(res, userdefined = userdefined)
  }
  x
}

#' Retrieve the user-defined variable names
#'
#' Function to retrieve the names of the additional output variables that can be defined in `x13()`, `regarima()` and `x11()` functions.
#'
#'@param x a character.
#'
#' @examples
#' userdefined_variables_x13("x13")
#' userdefined_variables_x13("regarima")
#' userdefined_variables_x13("x11")
#' @export
userdefined_variables_x13 <- function(x = c("X-13","RegArima","X-11")){
  x <- match.arg(gsub("-", "", tolower(x)),
                 choices = c("x13", "regarima", "x11"))

  # jts<-rjd3toolkit::ts_r2jd(rjd3toolkit::ABS$X0.2.09.10.M)
  # jrslt<- rJava::.jcall("demetra/x13/r/X13", "Ljdplus/x13/X13Results;", "process", jts, "RSA3")
  # rjd3toolkit::dictionary(rjd3toolkit::jd3Object(jrslt, result = TRUE)) |>
  #   dput()

  sa_x13 = structure(c("preadjustment.a1", "preadjustment.a1a", "preadjustment.a1b",
                       "preadjustment.a6", "preadjustment.a7", "preadjustment.a8", "preadjustment.a8i",
                       "preadjustment.a8s", "preadjustment.a8t", "preadjustment.a9",
                       "preadjustment.a9sa", "preadjustment.a9u", "preadjustment.a9ser",
                       "s", "s_f", "sa", "sa_f", "t", "t_f", "i", "decomposition.y_cmp",
                       "decomposition.y_cmp_f", "decomposition.y_cmp_b", "decomposition.s_cmp",
                       "decomposition.sa_cmp", "decomposition.t_cmp", "decomposition.i_cmp",
                       "finals.d11", "finals.d12", "finals.d13", "finals.d16", "finals.d18",
                       "finals.d11a", "finals.d12a", "finals.d16a", "finals.d18a", "finals.e1",
                       "finals.e2", "finals.e3", "finals.e11", "arima.phi(*)", "arima.bphi(*)",
                       "arima.theta(*)", "arima.btheta(*)", "y_f(?)", "y_b(?)", "y_ef(?)",
                       "y_eb(?)", "yc", "l", "ycal", "det", "det_f(?)", "det_b(?)",
                       "cal", "cal_f(?)", "cal_b(?)", "tde", "tde_f(?)", "tde_b(?)",
                       "mhe", "mhe_f(?)", "mhe_b(?)", "ee", "ee_f(?)", "ee_b(?)", "omhe",
                       "omhe_f(?)", "omhe_b(?)", "out", "out_f(?)", "out_b(?)", "regression.mu",
                       "regression.lp", "regression.easter", "regression.outlier(*)",
                       "regression.td(*)", "regression.user(*)", "regression.out(*)",
                       "regression.missing(*)", "regression.ml.pcorr", "residuals.ser",
                       "residuals.res", "residuals.tsres", "arima.p", "arima.d", "arima.q",
                       "arima.bp", "arima.bd", "arima.bq", "y", "period", "span.start",
                       "span.end", "span.n", "span.missing", "log", "adjust", "regression.espan.start",
                       "regression.espan.end", "regression.espan.n", "regression.espan.missing",
                       "regression.mean", "regression.nlp", "regression.ntd", "regression.nmh",
                       "regression.nout", "regression.nao", "regression.nls", "regression.ntc",
                       "regression.nso", "regression.nusers", "regression.leaster",
                       "regression.description", "regression.type", "regression.details.coefficients",
                       "regression.details.covar", "regression.details.covar-ml", "regression.ml.parameters",
                       "regression.ml.pcovar", "regression.ml.pcovar-ml", "regression.ml.pscore",
                       "likelihood.ll", "likelihood.adjustedll", "likelihood.ssqerr",
                       "likelihood.nparams", "likelihood.nobs", "likelihood.neffectiveobs",
                       "likelihood.df", "likelihood.aic", "likelihood.aicc", "likelihood.bic",
                       "likelihood.bicc", "likelihood.bic2", "likelihood.hannanquinn",
                       "residuals.type", "residuals.mean", "residuals.doornikhansen",
                       "residuals.skewness", "residuals.kurtosis", "residuals.lb", "residuals.bp",
                       "residuals.seaslb", "residuals.seasbp", "residuals.lb2", "residuals.bp2",
                       "residuals.nruns", "residuals.lruns", "residuals.nudruns", "residuals.ludruns",
                       "out_i", "out_i_f(?)", "out_i_b(?)", "out_t", "out_t_f(?)", "out_t_b(?)",
                       "out_s", "out_s_f(?)", "out_s_b(?)", "reg_i", "reg_i_f(?)", "reg_i_b(?)",
                       "reg_t", "reg_t_f(?)", "reg_t_b(?)", "reg_s", "reg_s_f(?)", "reg_s_b(?)",
                       "reg_sa", "reg_sa_f(?)", "reg_sa_b(?)", "reg_y", "reg_y_f(?)",
                       "reg_y_b(?)", "reg_u", "reg_u_f(?)", "reg_u_b(?)", "det_i", "det_i_f(?)",
                       "det_i_b(?)", "det_t", "det_t_f(?)", "det_t_b(?)", "det_s", "det_s_f(?)",
                       "det_s_b(?)", "decomposition.b1", "decomposition.b2", "decomposition.b3",
                       "decomposition.b4", "decomposition.b5", "decomposition.b6", "decomposition.b7",
                       "decomposition.b8", "decomposition.b9", "decomposition.b10",
                       "decomposition.b11", "decomposition.b13", "decomposition.b17",
                       "decomposition.b20", "decomposition.c1", "decomposition.c2",
                       "decomposition.c4", "decomposition.c5", "decomposition.c6", "decomposition.c7",
                       "decomposition.c9", "decomposition.c10", "decomposition.c11",
                       "decomposition.c13", "decomposition.c17", "decomposition.C20",
                       "decomposition.d1", "decomposition.d2", "decomposition.d4", "decomposition.d5",
                       "decomposition.d6", "decomposition.d7", "decomposition.d8", "decomposition.d9",
                       "decomposition.d10", "decomposition.d10a", "decomposition.d11",
                       "decomposition.d11a", "decomposition.d12", "decomposition.d12a",
                       "decomposition.d13", "decomposition.trend-filter", "decomposition.seasonal-filters",
                       "decomposition.d9-global-msr", "decomposition.d9-msr", "decomposition.d9-msr-table",
                       "decomposition.x11-all", "diagnostics.seas-res-f", "diagnostics.seas-res-qs",
                       "diagnostics.seas-res-kw", "diagnostics.seas-res-friedman", "diagnostics.seas-res-periodogram",
                       "diagnostics.seas-res-spectralpeaks", "diagnostics.seas-i-f",
                       "diagnostics.seas-i-qs", "diagnostics.seas-i-kw", "diagnostics.seas-i-periodogram",
                       "diagnostics.seas-i-friedman", "diagnostics.seas-i-spectralpeaks",
                       "diagnostics.seas-sa-f", "diagnostics.seas-sa-qs", "diagnostics.seas-sa-kw",
                       "diagnostics.seas-sa-friedman", "diagnostics.seas-sa-periodogram",
                       "diagnostics.seas-sa-spectralpeaks", "diagnostics.seas-lin-f",
                       "diagnostics.seas-lin-qs", "diagnostics.seas-lin-kw", "diagnostics.seas-lin-friedman",
                       "diagnostics.seas-lin-periodogram", "diagnostics.seas-lin-spectralpeaks",
                       "diagnostics.seas-sa-ac1", "diagnostics.td-sa-all", "diagnostics.td-sa-last",
                       "diagnostics.td-i-all", "diagnostics.td-i-last", "diagnostics.td-res-all",
                       "diagnostics.td-res-last", "diagnostics.seas-lin-combined", "diagnostics.seas-lin-evolutive",
                       "diagnostics.seas-lin-stable", "diagnostics.seas-si-combined",
                       "diagnostics.seas-si-combined3", "diagnostics.seas-si-evolutive",
                       "diagnostics.seas-si-stable", "diagnostics.seas-res-combined",
                       "diagnostics.seas-res-combined3", "diagnostics.seas-res-evolutive",
                       "diagnostics.seas-res-stable", "diagnostics.seas-i-combined",
                       "diagnostics.seas-i-combined3", "diagnostics.seas-i-evolutive",
                       "diagnostics.seas-i-stable", "diagnostics.seas-sa-combined",
                       "diagnostics.seas-sa-combined3", "diagnostics.seas-sa-evolutive",
                       "diagnostics.seas-sa-stable", "diagnostics.fcast-insample-mean",
                       "diagnostics.fcast-outsample-mean", "diagnostics.fcast-outsample-variance",
                       "variancedecomposition.cycle", "variancedecomposition.seasonality",
                       "variancedecomposition.irregular", "variancedecomposition.tdh",
                       "variancedecomposition.others", "variancedecomposition.total",
                       "m-statistics.m1", "m-statistics.m2", "m-statistics.m3", "m-statistics.m4",
                       "m-statistics.m5", "m-statistics.m6", "m-statistics.m7", "m-statistics.m8",
                       "m-statistics.m9", "m-statistics.m10", "m-statistics.m11", "m-statistics.q",
                       "m-statistics.q-m2"), dim = 305L)

  # jts<-rjd3toolkit::ts_r2jd(rjd3toolkit::ABS$X0.2.09.10.M)
  # jrslt<- rJava::.jcall("demetra/x13/r/RegArima", "Ljdplus/regsarima/regular/RegSarimaModel;", "process", jts, "RG3")
  # rjd3toolkit::dictionary(rjd3toolkit::jd3Object(jrslt, result = TRUE)) |>
  #   dput()

  sa_regarima = structure(c("arima.phi(*)", "arima.bphi(*)", "arima.theta(*)",
                            "arima.btheta(*)", "y_f(?)", "y_b(?)", "y_ef(?)", "y_eb(?)",
                            "yc", "l", "ycal", "det", "det_f(?)", "det_b(?)", "cal", "cal_f(?)",
                            "cal_b(?)", "tde", "tde_f(?)", "tde_b(?)", "mhe", "mhe_f(?)",
                            "mhe_b(?)", "ee", "ee_f(?)", "ee_b(?)", "omhe", "omhe_f(?)",
                            "omhe_b(?)", "out", "out_f(?)", "out_b(?)", "regression.mu",
                            "regression.lp", "regression.easter", "regression.outlier(*)",
                            "regression.td(*)", "regression.user(*)", "regression.out(*)",
                            "regression.missing(*)", "regression.ml.pcorr", "residuals.ser",
                            "residuals.res", "residuals.tsres", "arima.p", "arima.d", "arima.q",
                            "arima.bp", "arima.bd", "arima.bq", "y", "period", "span.start",
                            "span.end", "span.n", "span.missing", "log", "adjust", "regression.espan.start",
                            "regression.espan.end", "regression.espan.n", "regression.espan.missing",
                            "regression.mean", "regression.nlp", "regression.ntd", "regression.nmh",
                            "regression.nout", "regression.nao", "regression.nls", "regression.ntc",
                            "regression.nso", "regression.nusers", "regression.leaster",
                            "regression.description", "regression.type", "regression.details.coefficients",
                            "regression.details.covar", "regression.details.covar-ml", "regression.ml.parameters",
                            "regression.ml.pcovar", "regression.ml.pcovar-ml", "regression.ml.pscore",
                            "likelihood.ll", "likelihood.adjustedll", "likelihood.ssqerr",
                            "likelihood.nparams", "likelihood.nobs", "likelihood.neffectiveobs",
                            "likelihood.df", "likelihood.aic", "likelihood.aicc", "likelihood.bic",
                            "likelihood.bicc", "likelihood.bic2", "likelihood.hannanquinn",
                            "residuals.type", "residuals.mean", "residuals.doornikhansen",
                            "residuals.skewness", "residuals.kurtosis", "residuals.lb", "residuals.bp",
                            "residuals.seaslb", "residuals.seasbp", "residuals.lb2", "residuals.bp2",
                            "residuals.nruns", "residuals.lruns", "residuals.nudruns", "residuals.ludruns",
                            "out_i", "out_i_f(?)", "out_i_b(?)", "out_t", "out_t_f(?)", "out_t_b(?)",
                            "out_s", "out_s_f(?)", "out_s_b(?)", "reg_i", "reg_i_f(?)", "reg_i_b(?)",
                            "reg_t", "reg_t_f(?)", "reg_t_b(?)", "reg_s", "reg_s_f(?)", "reg_s_b(?)",
                            "reg_sa", "reg_sa_f(?)", "reg_sa_b(?)", "reg_y", "reg_y_f(?)",
                            "reg_y_b(?)", "reg_u", "reg_u_f(?)", "reg_u_b(?)", "det_i", "det_i_f(?)",
                            "det_i_b(?)", "det_t", "det_t_f(?)", "det_t_b(?)", "det_s", "det_s_f(?)",
                            "det_s_b(?)"), dim = 146L)

  # jts<-rjd3toolkit::ts_r2jd(rjd3toolkit::ABS$X0.2.09.10.M)
  # jrslt<- rJava::.jcall("demetra/x13/r/X11", "Ljdplus/x11/X11Results;",
  #                       "process", jts,
  #                       rjd3x13::r2jd_spec_x11(rjd3x13::spec_x11_default()))
  # rjd3toolkit::dictionary(rjd3toolkit::jd3Object(jrslt, result = TRUE)) |>
  #   dput()

  sa_x11 = structure(c("b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9",
                       "b10", "b11", "b13", "b17", "b20", "c1", "c2", "c4", "c5", "c6",
                       "c7", "c9", "c10", "c11", "c13", "c17", "C20", "d1", "d2", "d4",
                       "d5", "d6", "d7", "d8", "d9", "d10", "d10a", "d11", "d11a", "d12",
                       "d12a", "d13", "trend-filter", "seasonal-filters", "d9-global-msr",
                       "d9-msr", "d9-msr-table", "x11-all"), dim = 47L)
  switch (x,
          x13 = sa_x13,
          regarima = sa_regarima,
          x11 = sa_x11
  )
}
