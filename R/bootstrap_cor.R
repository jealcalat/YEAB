source("~/Dropbox/mult_IF_aco_exp/v3/wrapper.R")

df_IF_tr_xs <- bind_all_Ss(326:333,30,0,0) %>% as.data.frame()

rhos <- lapply(unique(df_IF_tr_xs$cde),function(c){
  df1 <- df_IF_tr_xs[df_IF_tr_xs$cde == c,c("cum_dt","sesion","evento")]
  rhos_int <- lapply(unique(df1$sesion),function(s){
    df2 <- df1[df1$sesion == s,]
    df2 <- matrix(c(df2$cum_dt,df2$evento),ncol = 2)
    if(c == 1) {
      r <- 1
      R <- 2
    } else {
      r <- 11
      R <- 21
    }
    rho_hat <- rR_corr_boot(df2,r,R,1000,200,B =1000)
    df_rho <- data.frame(rho = rho_hat[1],
                         low = rho_hat[2],
                         upp = rho_hat[3],
                         ses = s,
                         ccde = c)
    df_rho
  }) %>% bind_rows()
  rhos_int
}) %>% bind_rows()


library(plotrix)

plot(rhos$ses[rhos$ccde == 2],
     rhos$rho[rhos$ccde == 2],
     ylim = c(0,1),
     type = "l")
lines(rhos$ses[rhos$ccde == 1],
     rhos$rho[rhos$ccde == 1],
     ylim = c(0,1),
     type = "l")

plotCI(rhos$ses[rhos$ccde == 2],
       rhos$rho[rhos$ccde == 2],
       uiw = rhos$upp[rhos$ccde == 2],
       liw = rhos$low[rhos$ccde == 2])
