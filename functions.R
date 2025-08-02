
autenticar <- function() {
  
  app_name <- 'jneuer' # chosen by user
  app_client_id  <- Sys.getenv("STRAVA_KEY")
  app_secret <- Sys.getenv("STRAVA_SECRET")

  if (!file.exists('.httr-oauth')) {
    # criar e colocar no cache
    stoken <- httr::config(
      token = strava_oauth(
        app_name, 
        app_client_id, 
        app_secret, 
        app_scope="activity:read_all",
        cache = TRUE
      )
    )
  } else {
    # Ler do cache
    stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
    message('Token lida do cache.')
  }

  stoken
  
}


baixar_atividades <- function(stoken, after = NULL) {
  
  get_activity_list(stoken, after = after) %>% 
    compile_activities() 
  
}


limpar_df_atividades <- function(df, types = 'Run') {
  
  pt_br_locale <- locale(decimal_mark = ',', grouping_mark = '')
  
  df %>% 
    filter(type %in% types) %>% 
    # Data e fuso horário
    mutate(
      start_date = as_datetime(start_date),
      start_date_local = as_datetime(start_date_local)
    ) %>% 
    mutate(
      start_date_local =
        case_when(
          str_detect(timezone, 'America/Sao_Paulo$') 
            ~ force_tz(start_date_local, 'America/Sao_Paulo'),
          str_detect(timezone, 'GMT$') 
            ~ with_tz(start_date_local, 'America/Sao_Paulo')
        )
    ) %>% 
    mutate(
      timezone = '(GMT-03:00) America/Sao_Paulo'
    ) %>% 
    # Outros campos
    mutate(
      average_heartrate = parse_double(
        average_heartrate,
        locale = pt_br_locale
      ),
      commute = as.logical(commute),
      display_hide_heartrate_option = 
        as.logical(display_hide_heartrate_option),
      flagged = as.logical(flagged),
      from_accepted_tag = as.logical(from_accepted_tag),
      has_heartrate = as.logical(has_heartrate),
      has_kudoed = as.logical(has_kudoed),
      heartrate_opt_out = as.logical(heartrate_opt_out),
      manual = as.logical(manual),
      max_heartrate = parse_double(max_heartrate),
      pr_count = parse_double(pr_count),
      private = as.logical(private),
      trainer = as.logical(trainer),
      upload_id = as.double(upload_id),
      upload_id_str = as.character(upload_id_str),
      utc_offset = as.double(utc_offset),
      workout_type = parse_double(workout_type),
      average_cadence = as.double(average_cadence),
      suffer_score = as.double(suffer_score)
    ) %>% 
    arrange(start_date_local)

}


ler_arquivo_atividades <- function(nome_arquivo) {
  
  if (file.exists(nome_arquivo)) {
    
    read_csv(
      nome_arquivo,
      col_types = cols(
        achievement_count = col_double(),
        athlete_count = col_double(),
        athlete.id = col_character(),
        athlete.resource_state = col_double(),
        average_heartrate = col_double(),
        average_speed = col_double(),
        comment_count = col_double(),
        commute = col_logical(),
        display_hide_heartrate_option = col_logical(),
        distance = col_double(),
        elapsed_time = col_double(),
        elev_high = col_double(),
        elev_low = col_double(),
        end_latlng1 = col_logical(),
        end_latlng2 = col_logical(),
        external_id = col_character(),
        flagged = col_logical(),
        from_accepted_tag = col_logical(),
        gear_id = col_character(),
        has_heartrate = col_logical(),
        has_kudoed = col_logical(),
        heartrate_opt_out = col_logical(),
        id = col_character(),
        kudos_count = col_double(),
        manual = col_logical(),
        map.id = col_character(),
        map.resource_state = col_double(),
        map.summary_polyline = col_character(),
        max_heartrate = col_double(),
        max_speed = col_double(),
        moving_time = col_double(),
        name = col_character(),
        photo_count = col_double(),
        pr_count = col_double(),
        private = col_logical(),
        resource_state = col_double(),
        sport_type = col_character(),
        start_date = col_datetime(format = "%FT%TZ"),
        start_date_local = col_datetime(format = "%FT%TZ"),
        start_latlng1 = col_double(),
        start_latlng2 = col_double(),
        timezone = col_character(),
        total_elevation_gain = col_double(),
        total_photo_count = col_double(),
        trainer = col_logical(),
        type = col_character(),
        upload_id = col_double(),
        upload_id_str = col_character(),
        utc_offset = col_double(),
        visibility = col_character(),
        workout_type = col_double(),
        average_cadence = col_double(),
        suffer_score = col_double()
      )
    )
    
  } else {
    
    NULL
    
  }
  
}


data_hora_ultima_atividade <- function(df_atividades) {
  
  if (!is.null(df_atividades)) {
    
    df_atividades %>% 
      slice_max(
        order_by = start_date_local,
        n = 1,
        with_ties = FALSE
      ) %>% 
      pull(start_date_local) %>% 
      as_date()
    
  } else {
    
    NULL
    
  }
  
}


gravar_atividades <- function(df_atividades, nome_arquivo) {
  
  df_atividades %>% 
    write_csv(nome_arquivo)
  
}


baixar_streams_uma_atividade <- function(
  stoken,
  id,
  types = c(
    'time', 
    'distance', 
    'latlng', 
    'velocity_smooth',
    'heartrate', 
    'cadence'
  ),
  resolution = 'high',
  series_type = 'time'
) {

  lista <- get_streams(
    stoken,
    id,
    types = types,
    resolution = resolution,
    series_type = series_type
  )

  df <- lista %>%
    map(
      \(x) {
        if (x$type == 'latlng') {
          map(x$data, unlist) %>% as_tibble_col(column_name = x$type)
        } else {
          map_dbl(x$data, 1) %>% as_tibble_col(column_name = x$type)
        }
      }
    ) %>%
    bind_cols()
  
  if ('latlng' %in% names(df)) {
    
    df <- df %>% 
      hoist(
        latlng,
        lat = 1,
        lng = 2
      )
    
  }
  
  df

}


baixar_streams_diversas_atividades <- function(
  stoken,
  ids,
  types = c(
    'time', 
    'distance', 
    'latlng', 
    'velocity_smooth',
    'heartrate', 
    'cadence'
  ),
  resolution = 'high',
  series_type = 'time'
) {
  
  for (id in ids) {
    
    message('Baixando atividade id ', id)
    df <- baixar_streams_uma_atividade(stoken, id, types)

    message('Gravando atividade id ', id)
    nome_arq <- paste0(id, '.csv')
    write_csv(df, nome_arq)

  }
  
}
