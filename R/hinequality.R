hinequality <-function (rates, pop, type = c("hineq","rd","rr","bgv","idisp","mld", "theil", "gini")) {
  
  require(laeken)
  require(tibble)
  
  # Range Difference (RD)
  rd <- function(r){
    diff(range(r))
  }
  
  # Range Ratio (RR)
  rr <- function(r){
    max(r)/min(r)
  }
  
  # Between-Group Variance (BGV)
  bgv <- function(r,p){
    p=p/sum(p)
    mu=sum(r*p)
    b=sum(p*(r-mu)^2)
    b
  }
  
  # Index of Disparity (IDisp)
  idisp <- function(r){
    100*(sum((r-min(r))/(length(r)-1))/min(r))
  }
  
  # Mean Log Deviation (MLD)
  mld <- function(r,p){
    p=p/sum(p)
    mu=sum(r*p)
    r=r/mu
    m=sum(p*(-log(r)))
    m
  }
  
  # Theil Index (T) 
  theil <- function(r,p){
    p=p/sum(p)
    mu=sum(r*p)
    r=r/mu
    t=sum(p*r*log(r))
    t
  }
  
  # Gini Coefficient
  gini2 <- function(r,p){
    
    p= p/sum(p)
    g = laeken::gini(r,p)
    unlist(g)
  }
  
  # wrap up of all the measures
  hineq <- function(r,p){
    
    disp <- c(rd(r=rates),bgv(r=rates,p=pop),rr(r=rates),idisp(r=rates),mld(r=rates,p=pop), theil(r=rates,p=pop), gini2(r=rates,p=pop))
    
    measures <- c("Range Difference (RD)","Between-Group Variance (BGV)",
                  "Range Ratio (RR)","Index of Disparity (IDisp)","Mean Log Deviation (MLD)","Theil Index (T)", "Gini Coefficient")
    types <- c("Absolute","Absolute","Relative","Relative","Relative","Relative", "Relative")
    tab <- tibble::tibble(Inequality_Measure = measures, Value = disp,Type=types )
    tab
  }
  
  switch(match.arg(type),hineq=hineq(rates,pop),rd=rd(rates),bgv=bgv(rates,pop),rr=rr(rates),idisp=idisp(rates),mld=mld(rates,pop), theil=theil(rates,pop), gini = gini2(rates, pop))
  
}

