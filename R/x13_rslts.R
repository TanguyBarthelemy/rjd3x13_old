#' @include utils.R
NULL


x13_rslts<-function(jrslts){
  if (is.jnull(jrslts))
    return (NULL)
  q<-.jcall(jrslts, "[B", "buffer")
  rq<-RProtoBuf::read(x13.X13Results, q)
  return (p2r_x13_rslts(rq))
}

x11_rslts<-function(jrslts){
  if (is.jnull(jrslts))
    return (NULL)
  q<-.jcall(jrslts, "[B", "buffer")
  rq<-RProtoBuf::read(x13.X11Results, q)
  return (p2r_x11_rslts(rq))
}

p2r_x13_rslts<-function(p){

  return (structure(
    list(
      preprocessing=p2r_regarima_rslts(p$preprocessing),
      preadjust=p2r_x13_preadjust(p$final),
      decomposition=p2r_x11_rslts(p$decomposition),
      final=p2r_x13_final(p$final),
      mstats=p$diagnostics_x13$mstatistics$as.list(),
      diagnostics=p2r_sa_diagnostics(p$diagnostics_sa)
      )
    ,
    class= "JD3X13_RSLTS"))
}

p2r_x11_rslts<-function(p){
  return (structure(
    list(
      d1=p2r_ts(p$d1),
      d2=p2r_ts(p$d2),
      d4=p2r_ts(p$d4),
      d5=p2r_ts(p$d5),
      d6=p2r_ts(p$d6),
      d7=p2r_ts(p$d7),
      d8=p2r_ts(p$d8),
      d9=p2r_ts(p$d9),
      d10=p2r_ts(p$d10),
      d11=p2r_ts(p$d11),
      d12=p2r_ts(p$d12),
      d13=p2r_ts(p$d13),
      final_henderson=p$final_henderson_filter
    ),
    class= "JD3X11"))
}


p2r_x13_final<-function(p){
  return (list(
      d10final=p2r_ts(p$d10final),
      d11final=p2r_ts(p$d11final),
      d12final=p2r_ts(p$d12final),
      d13final=p2r_ts(p$d13final),
      d16=p2r_ts(p$d16),
      d18=p2r_ts(p$d18),
      d10a=p2r_ts(p$d10a),
      d11a=p2r_ts(p$d11a),
      d12a=p2r_ts(p$d12a),
      d16a=p2r_ts(p$d16a),
      d18a=p2r_ts(p$d18a)
    ))
}

p2r_x13_preadjust<-function(p){
  return (list(
      a1=p2r_ts(p$a1),
      a1a=p2r_ts(p$a1a),
      a6=p2r_ts(p$a6),
      a7=p2r_ts(p$a7),
      a8=p2r_ts(p$a8),
      a9=p2r_ts(p$a9)
    ))
}


############################# Generics

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
sa.decomposition.JD3X13_RSLTS<-function(x){
  if (is.null(x)) return (NULL)
  return (rjd3sa::sadecomposition(x$preadjust$a1,
                                  x$final$d11final,
                                  x$final$d12final,
                                  x$final$d10final,
                                  x$final$d13final,
                                  x$preprocessing$description$log
  ))

}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
sa.decomposition.JD3X13_OUTPUT<-function(x){
  return (sa.decomposition(x$result))
}


