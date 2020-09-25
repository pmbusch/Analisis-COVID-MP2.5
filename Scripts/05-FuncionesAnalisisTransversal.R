### Analisis-COVID-MP2.5
## Funciones para generar Figuras/Tablas de un modelo Transversal ajustado
## PBH Julio 2020


foot_note <- "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"

f_tableCoef <- function(model){
  # est <- cbind(est=coef(mod), confint(mod))
  est <- summary(model)$coefficients[,1:4] %>% as.data.frame() %>% 
    as_tibble(rownames = "parametro")
  names(est) <- c("parametro","coef","sd","z_value","p_value")
  
  ## Add codes
  est <- est %>% mutate(codes=case_when(
    p_value<0.001 ~ "***",
    p_value<0.01 ~ "**",
    p_value<0.05 ~ "*",
    p_value<0.1 ~ ".",
    T ~ ""))
  
  ## Tabla coeficientes
  table <- est %>% 
    mutate(parametro=parametro %>% 
             str_remove_all("scale|\\(|\\)|log") %>% 
             f_replaceVar()) %>% 
    rename(Parametro=parametro, `Coef.`=coef, `Desv.`=sd,
           `Valor-z`=z_value,`Valor-p`=p_value,`Sign.`=codes) %>% 
    flextable() %>% 
    colformat_num(big.mark=" ", digits=4, j=2:5,
                  na_str="s/i") %>% 
    bold(bold=T, part="header") %>% bold(j=1, bold=T) %>% 
    autofit(add_w = 0.1, add_h = 0.3) %>%
    align(j=1, align = "left", part="all") %>% 
    footnote(j=6, value=as_paragraph(foot_note), part="header", inline=T)
  
  return(table)
}

f_tableMRR <- function(model){
  # est <- cbind(est=coef(mod), confint(mod))
  est <- summary(model)$coefficients[,1:4] %>% as.data.frame() %>% 
    as_tibble(rownames = "parametro")
  names(est) <- c("parametro","coef","sd","z_value","p_value")
  
  ## Add codes
  est <- est %>% mutate(codes=case_when(
    p_value<0.001 ~ "***",
    p_value<0.01 ~ "**",
    p_value<0.05 ~ "*",
    p_value<0.1 ~ ".",
    T ~ ""))
  
  # Calculate MRR
  est <- est %>% mutate(low=coef-1.96*sd, high=coef+1.96*sd)
  est_mrr <- est[-1,] %>% mutate(coef=exp(coef) %>% round(2), 
                                 low=exp(low) %>% round(2), 
                                 high=exp(high) %>% round(2),
                                 ci=paste("(",low,", ",high,")",sep = ""),
                                 p_value=round(p_value,4))
  
  # Tabla MRR
  table <- est_mrr %>% 
    dplyr::select(parametro, coef, ci, p_value, codes) %>% 
    mutate(parametro=parametro %>% 
             str_remove_all("scale|\\(|\\)|log") %>% 
             f_replaceVar()) %>% 
    rename(Variable=parametro, MRR=coef, `95% I.C.`=ci,
           `Valor-p`=p_value,`Sign.`=codes) %>% 
    flextable() %>% 
    bold(bold=T, part="header") %>% bold(j=1, bold=T) %>% 
    autofit(add_w = 0.1, add_h = 0.3) %>%
    align(j=1, align = "left", part="all") %>% 
    footnote(j=5, value=as_paragraph(foot_note), part="header", inline=T)
  
  return(table)
}

f_figMRR <- function(model){
  # est <- cbind(est=coef(mod), confint(mod))
  est <- summary(model)$coefficients[,1:4] %>% as.data.frame() %>% 
    as_tibble(rownames = "parametro")
  names(est) <- c("parametro","coef","sd","z_value","p_value")
  
  ## Add codes
  est <- est %>% mutate(codes=case_when(
    p_value<0.001 ~ "***",
    p_value<0.01 ~ "**",
    p_value<0.05 ~ "*",
    p_value<0.1 ~ ".",
    T ~ ""))
  
  # Calculate MRR
  est <- est %>% mutate(low=coef-1.96*sd, high=coef+1.96*sd)
  est_mrr <- est[-1,] %>% mutate(coef=exp(coef) %>% round(2), 
                                 low=exp(low) %>% round(2), 
                                 high=exp(high) %>% round(2),
                                 ci=paste("(",low,", ",high,")",sep = ""),
                                 p_value=round(p_value,4))
  
  ## Figure MRR
  p <- est_mrr %>% 
    rowid_to_column() %>% 
    mutate(parametro=parametro %>% 
             str_remove_all("scale|\\(|\\)|log") %>% 
             f_replaceVar()) %>% 
    ggplot(aes(x=reorder(parametro,desc(rowid)), y=coef))+
    geom_point()+
    geom_errorbar(aes(ymin=low, ymax=high))+
    geom_hline(yintercept = 1, linetype = "dashed")+
    labs(x="",y="MRR")+
    coord_flip()
  
  return(p)
}

## Para calcular MRR del MP2.5 con su intervalo de confianza
f_MRR_mp25 <- function(mod, param="mp25"){
  # Get coefficients
  est <- summary(mod)$coefficients[,1:4] %>% as.data.frame() %>% 
    as_tibble(rownames = "parametro") %>% 
    filter(parametro==param)
  names(est) <- c("parametro","coef","sd","z_value","p_value")
  
  # Calculate MRR
  est <- est %>% mutate(low=coef-1.96*sd, high=coef+1.96*sd)
  est_mrr <- est %>% mutate(RR=exp(coef) %>% round(2), 
                            lower_CI=exp(low) %>% round(2), 
                            upper_CI=exp(high) %>% round(2)) %>% 
    dplyr::select(RR, lower_CI, upper_CI)
  return(est_mrr)
}



## EoF