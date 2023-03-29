
#' rchisdd() function to create a fake variable from the info of the chis dummy data dictionary
#'
#' @param info_dd a data.frame that contains c('VALUE','FREQ') if categorical or c('N','MIN','MAX','MEAN') if count/continuous
#' @param lgl_floor TRUE/FALSE if you want the output values rounded to their integer floor
#'
#' @return a numeric vector of values generated according to the user supplied `info_dd`
#' @export
#'
#' @examples
#' 
#' info_dd = data.frame(VALUE = c(1,2,3,4,5,8),
#' FREQ = c(22606, 1116,129,201,396,5))
#' 
#' fake_1 = rchisdd(info_dd)
#' table(fake_1)
#' 
#' # N MIN MAX MEAN from data dictionary
#' info_dd_2 = data.frame(N = 24453,
#'                        MIN = 18,
#'                        MAX = 117,
#'                        MEAN = 53.52)
#' 
#' fake_2 = rchisdd(info_dd_2)
#' summary(fake_2)
#' hist(fake_2)

rchisdd = function(info_dd,lgl_floor=TRUE){
  
  require(dplyr)
  require(purrr)
  
  # only continuous vars have MEAN
  lgl_cnt = ("MEAN" %in% names(info_dd))
  
  
  rcat = function(info_dd){
    
    lst_value_freq = info_dd %>% 
      rename(x=VALUE,times=FREQ) %>% 
      purrr::pmap(.f=rep)  # rep(x=VALUE,times=FREQ)
    
    # lapply(lst_value_freq,length)
    
    # appends them into vector
    vec_val_freq = unlist(lst_value_freq)
    return(vec_val_freq)
  }
  
  rcont_mmm = function(info_dd,lgl_floor){
    
    # the main workhorse helper function to stretch/squeeze
    scaleMMM <- function(x, 
                         min.target=-1, max.target=1,
                         mean.target=0,
                         p.range=c(0.01,10)) {
      
      
      # https://stats.stackexchange.com/a/316189
      
      tmpfun <- function(p) {
        x2 <- x-min(x)
        x2 <- x2/max(x2)
        x2 <- x2^p
        x2 <- x2*(max.target-min.target) + min.target
        x2
      }
      
      p <- uniroot(function(p){mean(tmpfun(p))-mean.target}, p.range)$root
      
      out <- tmpfun(p)
      attr(out, 'p') <- p
      out
    }
    
    # step 0 rgamma seems to be a good initial appx
    # continuous vars are always >= 0 based on ctrl+f data dictionary
    
    draw0 = info_dd %>% 
      transmute(n=N,shape=MEAN) %>% # rename and keep only
      purrr::pmap(rgamma,rate=1) %>% 
      unlist
    
    # summary(draw0); hist(draw0)
    
    # preserve mean and stretch/squeeze to [MIN,MAX]
    
    draw1 = scaleMMM(x=draw0,  # result of rgamma() initial appx
                     mean.target=info_dd$MEAN,
                     min.target=info_dd$MIN,
                     max.target=info_dd$MAX,
                     p.range=c(0.01,100)) 
    
    # draw1 %>% summary(); length(draw1)
    # hist(draw1)
    # plot(draw0,draw1)
    
    if(isTRUE(lgl_floor)){
      # optional integers round floor()
      message("Rounding to the floor since lgl_floor=TRUE")
      return(floor(draw1))
    }else{
      return(draw1)
    }
  }
  
  if(isTRUE(lgl_cnt)){
    # continuous
    out_draw = rcont_mmm(info_dd,lgl_floor)
  }else{
    # categorical
    out_draw = rcat(info_dd)
  }
  
  return(out_draw)
}




