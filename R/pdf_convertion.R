
#' @export
formatos <- function(orgao = '8', tr = '26'){
  paste0(
    #formato CNJ - 2008
    '[0-9]{7}\\-[0-9]{2}\\.[0-9]{4}\\.',orgao,'\\.',tr,'\\.[0-9]{4}|',
    #nro saj
    '[0-9]{3}\\.[0-9]{2}\\.[0-9]{6}\\-[0-9]{1}|',
    #nro que aparece no tjal 1
    '[0-9]{5}\\-[0-9]{1}\\.[0-9]{4}\\.[0-9]{3}'
  )
}

#' @export
pdf2text <- function(a, first_pg = NA, last_pg = NA, raw = F, keep_file = F, new_file = "repo.txt"){
  bash = ifelse(raw,'pdftotext -raw','pdftotext')
  bash = ifelse(is.na(first_pg),bash,paste(bash,'-f',first_pg))
  bash = ifelse(is.na(last_pg),bash,paste(bash,'-l',last_pg))
  bash = paste(bash,a,new_file)
  
  if(!file.exists(a)){return('')}
  
  if(file.size(a) > 5000 & stringi::stri_detect(a,fixed = '.pdf')){
    system(bash)
    texto = readr::read_file(new_file)
    ifelse(keep_file,NA,file.remove(new_file))
  } else {
    texto = ''
  }
  return(texto)
}


encontra_processos <- function(texto, orgao = '8', tr = '26', tj = 'TJSP'){
  texto %>%
    tira_header_pagina(tj) %>% 
    stringi::stri_extract_all(regex = formatos(orgao,tr)) %>%
    dplyr::first() %>%
    dplyr::data_frame() %>%
    setNames('n_processo') %>% 
    dplyr::distinct(n_processo)
}

tira_header_pagina <- function(texto, tj = 'TJSP'){
  texto %<>% 
    stringi::stri_replace_all(regex = '\n', replacement = '')
  if(tj == 'TJSC'){
    texto %<>% 
      stringi::stri_replace_all(regex = '\f[0-9].*?de2016|[\f\r]', replacement = '')
  } else if (tj %in% c('TJSP','TJAL')){
    texto %<>% 
      stringi::stri_replace_all(regex = '[\f ]', replacement = '') %>% 
      stringi::stri_replace_all(regex = 'PublicaçãoOficialdoTribunal.*?Edição[0-9]{3,6}', replacement = '')
  } 
  return(texto)
}

extrai_processos_arq <- function(a,orgao = '8', tr = '26', r = F, tj= 'TJSP', save_file = T){
  d = a %>%
    pdf2text(raw = r) %>%
    encontra_processos(orgao, tr, tj)
  
  if(save_file){saveRDS(d,file = stringi::stri_replace(a, fixed = '.pdf', replacement = '.rds'))}
  
  return(d)
}

extrai_processos <- function(lista_arquivos, orgao = '8', tr = '26', raw = F, tj = 'TJSP'){
  dplyr::data_frame(arq = lista_arquivos) %>%
    dplyr::group_by(arq) %>%
    dplyr::do(extrai_processos_arq(.$arq, orgao,tr,raw)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(n_processo)
}

tipo_n_procesos <- function(d){
  d %>% 
    dplyr::select(n_processo) %>% 
    dplyr::first() %>% 
    stringi::stri_length() %>% 
    table()
}