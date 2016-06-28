#' Formatos encontrados no TJSP e TJAL
#' 
#' @export
formats <- function(cnj = T, prodesp = T, saj = T, tjal = T, orgao = 8, tr = 26){
  sprintf('%s%s%s%s',
          #formato CNJ - 2008
          ifelse(cnj,sprintf('[0-9]{7}\\-[0-9]{2}\\.[0-9]{4}\\.%s\\.%s\\.[0-9]{4}',orgao,tr),''),
          #nro saj
          ifelse(saj,'|[0-9]{3}\\.[0-9]{2}\\.[0-9]{6}\\-?[0-9]{1}?',''),
          #nro que aparece no tjal 1
          ifelse(tjal,'|[0-9]{5}\\-[0-9]{1}\\.[0-9]{4}\\.[0-9]{3}',''),
          #prodesp
          ifelse(prodesp,'|[0-9]{3}\\.[0-9]{2}\\.[0-9]{4}\\.[0-9]{6}\\-?[0-9]?','')
  )
}


#' PDF to TEXT
#' 
#' @export
pdf_to_text <- function(from, 
                        first_pg = NA, 
                        last_pg = NA, 
                        raw = FALSE, 
                        keep_file = FALSE, 
                        new_file = 'repo.txt', 
                        overwrite = FALSE) {
  if (!file.exists(from)) {
    return('origem nao existe')
  } else if (!overwrite && file.exists(new_file)) {
    return('destino existe e overwrite = FALSE')
  } else if (file.size(from) > 5000 & stringi::stri_detect(from, fixed = ".pdf")) {
    sprintf("pdftotext %s %s%s%s%s", from, 
            ifelse(raw, "-raw ", " "), 
            ifelse(!is.na(first_pg), paste("-f", first_pg, ""), " "), 
            ifelse(!is.na(last_pg), paste("-l", last_pg, ""), " "), 
            new_file) %>% system()
    # texto <- readr::read_file(new_file)
    texto <- 'OK'
    if(!keep_file) file.remove(new_file)
  } else {
    texto <- 'erro'
    cat(texto, '\n', file = new_file)
    if(!keep_file) file.remove(new_file)
  }
  return(texto)
}


#' PDF to TEXT (vetor de arqs)
#' 
#' @export
conv_pdf2txt <- function(arqs, dest_dir, raw = FALSE, overwrite = TRUE) {
  dest_arqs <- sprintf('%s/%s.txt', dest_dir, tools::file_path_sans_ext(basename(arqs)))
  dplyr::data_frame(origem = arqs, destino = dest_arqs) %>% 
    dplyr::distinct(origem, destino, .keep_all = TRUE) %>% 
    dplyr::group_by(origem, destino) %>% 
    do({
      r <- pdf_to_text(.$origem, raw = raw, keep_file = TRUE, new_file = .$destino, overwrite = overwrite)
      dplyr::data_frame(result = r)
    }) %>% 
    dplyr::ungroup()
}


#' Tira header da pagina do DJE
#' 
#' @export
tira_header_pag <- function(texto, tj = "TJSP") {
  texto %<>% stringi::stri_replace_all(regex = "\n", replacement = " ")
  if (tj == "TJSC") {
    texto %<>% 
      stringi::stri_replace_all(regex = "\f[0-9].*?de *2016|[\f \r]", replacement = "")
  } else if (tj %in% c("TJSP", "TJAL")) {
    texto %<>% 
      stringi::stri_replace_all(regex = "\f", replacement = "") %>%
      stringi::stri_replace_all(regex = "Publicação *Oficial *do *Tribunal *.*?Edição *[0-9]+ *[0-9]+", 
                                replacement = "") %>% 
      stringi::stri_replace_all(regex = " ", replacement = "")
  }
  return(texto)
}

#' Encontra processos
#' 
#' @export
encontra_proc <- function (texto, orgao = "8", tr = "26", tj = "TJSP") {
  texto %>% 
    tira_header_pag(tj) %>%
    stringi::stri_extract_all(regex = formats(orgao = orgao, tr = tr)) %>% 
    dplyr::first() %>% 
    dplyr::data_frame() %>% 
    setNames("n_processo") %>% 
    dplyr::distinct(n_processo)
}

#' Converte txt para rds com processos
#' 
#' @export
conv_txt2rds <- function(arqs, dest_dir = NULL, orgao = "8", tr = "26", tj = "TJSP") {
  if (!is.null(dest_dir)) {
    dest_file <- sprintf('%s/%s.rds', dest_dir, tools::file_path_sans_ext(basename(arqs)))
    ff <- dplyr::failwith(dplyr::data_frame(result = 'erro'), encontra_processos2)
    d_res <- dplyr::data_frame(arq = arqs, dest = dest_file) %>% 
      dplyr::group_by(arq, dest) %>% 
      dplyr::do({
        if (file.exists(.$dest)) return(dplyr::data_frame(result = 'ja existe'))
        txt <- readr::read_file(.$arq)
        proc <- ff(txt, orgao = orgao, tr = tr, tj = tj)
        saveRDS(proc, .$dest)
        dplyr::data_frame(result = 'OK')
      }) %>% 
      dplyr::ungroup()
  } else {
    d_res <- dplyr::data_frame(arq = arqs) %>% 
      dplyr::group_by(arq) %>% 
      dplyr::do({
        txt <- readr::read_file(.$arq)
        proc <- ff(txt, orgao = orgao, tr = tr, tj = tj)
        proc
      }) %>% 
      dplyr::ungroup()
  }
  d_res
}

#' Carrega pasta de arquivos RDS
#' 
#' @export
carrega_rds <- function(path) {
  p_dje <- path %>% 
    dir(recursive = TRUE, full.names = TRUE) %>% 
    plyr::llply(function(.x) {
      .x %>% 
        readRDS() %>% 
        dplyr::mutate(arq = .x) %>% 
        dplyr::select(arq, dplyr::everything())
    }, .progress = 'text') %>% 
    dplyr::bind_rows()
}

#' Encontra a merda do digito verificador de um numero cnj
#'
#' @export
calcula_dig <- function(num, monta = FALSE) {
  NNNNNNN <- substr(num,  1L,  7L)
  AAAA <-    substr(num,  8L, 11L)
  JTR <-     substr(num, 12L, 14L)
  OOOO <-    substr(num, 15L, 18L)
  n1 <- sprintf('%02d', as.numeric(NNNNNNN) %% 97)
  n2 <- sprintf('%02d', as.numeric(sprintf('%s%s%s', n1, AAAA, JTR)) %% 97)
  n3 <- sprintf('%02d', 98 - ((as.numeric(sprintf('%s%s', n2, OOOO)) * 100) %% 97))
  dig <- n3
  if(monta) {
    return(sprintf('%s%s%s', substr(num, 1, 7), dig, substr(num, 8, 18)))
  }
  return(dig)
}

#' Converte para numero CNJ (vetorizado)
#' 
#' @export
to_cnj <- function (n_proc, orgao, tr) {
  n_processo <- stringi::stri_replace_all_regex(n_proc, '[^0-9]', '')
  nchar <- stringi::stri_length(n_processo)
  n1 <- stringi::stri_sub(n_processo, 1, 3)
  n1 <- ifelse(n1 == '583' & orgao == '8' & tr == '26', '001', n1)
  n2 <- ifelse(nchar > 14,
               stringi::stri_sub(n_processo, 6, 9),
               ifelse(as.numeric(stringi::stri_sub(n_processo, 4, 5)) < 17,
                      paste0("20", stringi::stri_sub(n_processo, 4, 5)),
                      paste0("19", stringi::stri_sub(n_processo, 4, 5))))
  
  n3 <- ifelse(nchar > 14, stringi::stri_sub(n_processo, 10, 15), stringi::stri_sub(n_processo, 6, 11))
  n_processo <- ifelse(nchar == 20, n_processo,
                       calcula_dig(sprintf("0%s%s%s%s0%s", n3, n2, orgao, tr, n1), T))
  n_processo
}

