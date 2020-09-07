## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  eval = nzchar(Sys.getenv("bmscstan_eval"))
)

## ---- fig.height= 6, fig.width= 6---------------------------------------------
library(bmscstan)

data(BSE)

str(data.pt)

str(data.ctrl)

ggplot(data.pt, aes(y = RT, x = Body.District:Side , fill = Congruency))+
  geom_boxplot()

ggplot(data.ctrl, aes(y = RT, x = Body.District:Side , fill = Congruency))+
  geom_boxplot()+
  facet_wrap( ~ ID , ncol = 4)

## ---- fig.show='hold'---------------------------------------------------------
qqnorm(data.ctrl$RT, main = "Controls")
qqline(data.ctrl$RT)

qqnorm(data.pt$RT, main = "Single Case")
qqline(data.pt$RT)

## ---- fig.show='hold'---------------------------------------------------------
out <- boxplot.stats( data.ctrl$RT )$out
data.ctrl <- droplevels( data.ctrl[ !data.ctrl$RT %in% out , ] )

out <- boxplot.stats( data.pt$RT )$out
data.pt <- droplevels( data.pt[ !data.pt$RT %in% out , ] )

qqnorm(data.ctrl$RT, main = "Controls")
qqline(data.ctrl$RT)

qqnorm(data.pt$RT, main = "Single Case")
qqline(data.pt$RT)

## -----------------------------------------------------------------------------
contrasts( data.ctrl$Side )          <- contr.treatment( n = 2 )
contrasts( data.ctrl$Congruency )    <- contr.treatment( n = 2 )
contrasts( data.ctrl$Body.District ) <- contr.treatment( n = 2 )

contrasts( data.pt$Side )            <- contr.treatment( n = 2 )
contrasts( data.pt$Congruency )      <- contr.treatment( n = 2 )
contrasts( data.pt$Body.District )   <- contr.treatment( n = 2 )

## -----------------------------------------------------------------------------
data.ctrl$BD_ID <- interaction( data.ctrl$Body.District , data.ctrl$ID )

## ---- warning = FALSE, message = FALSE----------------------------------------
mdl <- BMSC(formula = RT ~ Body.District * Congruency * Side +
             (Congruency * Side | ID) + (Congruency * Side | BD_ID),
             data_ctrl = data.ctrl,
             data_sc = data.pt,
             cores = 4,
             seed = 2020)

## ---- fig.width=6, fig.height=6-----------------------------------------------
pp_check( mdl )

## -----------------------------------------------------------------------------
print( summary( mdl ) , digits = 3 )

## ---- fig.height=6, fig.width=6-----------------------------------------------
plot( mdl , who = "control" )

## -----------------------------------------------------------------------------
pp <- pairwise.BMSC(mdl , contrast = "Body.District2:Congruency2" , who = "control")

print( pp , digits = 3 )

## ---- fig.height=6, fig.width=6-----------------------------------------------
plot( pp )

## ---- fig.show = "hold"-------------------------------------------------------
p1 <- pairwise.BMSC(mdl , contrast = "Body.District2" ,  who = "control" )

plot( p1 )[[1]] +
  ggtitle("Body District" , subtitle = "Marginal effects") 

plot( p1 )[[2]] +
  ggtitle("Body District" , subtitle = "Contrasts") 

p2 <- pairwise.BMSC(mdl , contrast = "Congruency2" ,  who = "control" )

plot( p2 )[[1]] +
  ggtitle("Congruency" , subtitle = "Marginal effects")

plot( p2 )[[2]] +
  ggtitle("Congruency" , subtitle = "Contrasts")

p3 <- pairwise.BMSC(mdl , contrast = "Side2" ,  who = "control" )

plot( p3 )[[1]] +
  ggtitle("Side" , subtitle = "Marginal effects")

plot( p3 )[[2]] +
  ggtitle("Side" , subtitle = "Contrasts")

## ---- fig.width=6, fig.height=6-----------------------------------------------
plot( mdl ) +
  theme_bw( base_size = 18 )+
  theme( legend.position = "bottom",
         legend.direction = "horizontal")

## ---- fig.width=6, fig.height=6-----------------------------------------------
plot( mdl ,who = "delta" ) +
  theme_bw( base_size = 18 )

## -----------------------------------------------------------------------------
p4 <- pairwise.BMSC(mdl , contrast = "Body.District2:Congruency2" , who = "delta")

print( p4 , digits = 3 )

## ---- fig.show="hold"---------------------------------------------------------
plot( p4 , type = "interval")

plot( p4 , type = "area")

plot( p4 , type = "hist")

## ---- fig.height=6, fig.width=6-----------------------------------------------
p5 <- pairwise.BMSC(mdl , contrast = "Body.District2:Congruency2" ,
                    who = "singlecase")

plot( p5 , type = "hist")[[1]]

## ---- fig.height=6, fig.width=6-----------------------------------------------
p6 <- pairwise.BMSC(mdl , contrast = "Body.District2:Side2" , who = "delta")

print( p6 , digits = 3 )

plot( p6 , type = "hist")[[1]] +
  theme_bw( base_size = 18)+
  theme( strip.text.y = element_text( angle = 0 ) )

## ---- fig.height=6, fig.width=6-----------------------------------------------
p7 <- pairwise.BMSC(mdl ,
                    contrast = "Body.District2:Congruency2:Side2" ,
                    who = "delta")

print( p7 , digits = 3 )

plot( p7 , type = "hist")[[1]] +
  theme_bw( base_size = 18)+
  theme( strip.text.y = element_text( angle = 0 ) )

