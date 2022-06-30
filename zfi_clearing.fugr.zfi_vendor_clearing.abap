FUNCTION ZFI_VENDOR_CLEARING.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_BELGELER) TYPE  ZFI_TT_DENK_BELGE
*"     REFERENCE(DENKTAR) TYPE  DATUM DEFAULT SY-DATUM
*"     REFERENCE(BLDAT) TYPE  DATUM DEFAULT SY-DATUM
*"     REFERENCE(BLART) TYPE  BLART DEFAULT 'AB'
*"     REFERENCE(AGUMS) TYPE  AGUMS DEFAULT ''
*"     REFERENCE(WAERS) TYPE  WAERS DEFAULT 'TRY'
*"     REFERENCE(MODE) TYPE  MODE DEFAULT 'N'
*"  EXPORTING
*"     REFERENCE(AWKEY) TYPE  AWKEY
*"     REFERENCE(MESSAGE) TYPE  BAPI_MSG
*"     REFERENCE(ES_RET2) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DEFINE rellena_ftpost.
    l_t_ftpost-stype = &1.
    l_t_ftpost-count = &2.
    l_t_ftpost-fnam  = &3.
    l_t_ftpost-fval  = &4.
    append l_t_ftpost.
  END-OF-DEFINITION.

  CLEAR : l_t_blntab , l_t_blntab[] , l_t_ftclear , l_t_ftclear[] ,
  l_t_ftpost , l_t_ftpost[] , l_t_fttax , l_t_fttax[] ,
  wa_belgeler , bsik , bkpf , tutar , budat_txt , bldat_txt ,
  denktutar , bakiye , zfbdt_txt , es_ret2.

  l_group = sy-tcode .

  if mode is not INITIAL .
    p_mode = mode .
  endif .

  CALL FUNCTION 'ZPOSTING_INTERFACE_START'
    EXPORTING
      i_function         = 'C'
      i_group            = l_group
      i_mode             = p_mode
      i_update           = 'S'
      i_user             = sy-uname
      i_xbdcc            = 'X'
    EXCEPTIONS
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      OTHERS             = 6.

  LOOP AT it_belgeler INTO wa_belgeler.
    l_t_ftclear-agbuk = wa_belgeler-bukrs  .
    l_t_ftclear-agkoa = 'K' .
    l_t_ftclear-selfd = 'BELNR'.
    l_t_ftclear-xnops = 'X' .
    l_t_ftclear-agums = agums .
    l_t_ftclear-selvon(10) = wa_belgeler-belnr .
    l_t_ftclear-selvon+10(4) = wa_belgeler-gjahr .
    l_t_ftclear-selvon+14(3) = wa_belgeler-buzei .
    APPEND l_t_ftclear .
    IF wa_belgeler-kalantutar IS NOT INITIAL .
      denktutar = wa_belgeler-kalantutar .
      SELECT SINGLE * FROM bsik WHERE bukrs EQ wa_belgeler-bukrs AND
      belnr EQ wa_belgeler-belnr AND
      gjahr EQ wa_belgeler-gjahr AND
      buzei EQ wa_belgeler-buzei .

      SELECT SINGLE * FROM bkpf WHERE bukrs EQ wa_belgeler-bukrs AND
      belnr EQ wa_belgeler-belnr AND
      gjahr EQ wa_belgeler-gjahr .
    ENDIF .
  ENDLOOP .

  WRITE : denktar TO budat_txt DD/MM/YYYY ,
  BLDAT TO bldat_txt DD/MM/YYYY .
  IF bsik IS NOT INITIAL .
    WRITE bsik-zfbdt TO zfbdt_txt DD/MM/YYYY .

    rellena_ftpost 'K' 1 'BKPF-BUKRS' bkpf-bukrs .
    rellena_ftpost 'K' 1 'BKPF-BLART' blart . "bkpf-blart .
    rellena_ftpost 'K' 1 'BKPF-BLDAT' bldat_txt .
    rellena_ftpost 'K' 1 'BKPF-BUDAT' budat_txt .
    rellena_ftpost 'K' 1 'BKPF-WAERS' waers .
    rellena_ftpost 'K' 1 'BKPF-BKTXT' bkpf-bktxt .

    WRITE denktutar TO tutar DECIMALS 2 .
    CONDENSE tutar .
    IF bsik-shkzg EQ 'S' .
      if bsik-umskz is INITIAL .
        rellena_ftpost 'P' 2 'RF05A-NEWBS' '21'.
      else .
        rellena_ftpost 'P' 2 'RF05A-NEWBS' '29'.
      endif .
    ELSE .
      IF  bsik-umskz  IS NOT INITIAL.
        rellena_ftpost 'P' 2 'RF05A-NEWBS' '39'.
      ELSE.
        rellena_ftpost 'P' 2 'RF05A-NEWBS' '31'.
      ENDIF.
    ENDIF .
    rellena_ftpost 'P' 2 'RF05A-NEWKO' bsik-lifnr .
    rellena_ftpost 'P' 2 'BSEG-WRBTR'  tutar(15).
*    rellena_ftpost 'P' 2 'BSEG-SGTXT'  'Denkleştirme Testi' .
    rellena_ftpost 'P' 2 'BSEG-ZFBDT'  zfbdt_txt .
    rellena_ftpost 'P' 2 'BSEG-ZTERM'  bsik-zterm .
*    rellena_ftpost 'P' 2 'BSEG-GSBER'  bsik-gsber .
*    rellena_ftpost 'P' 2 'BSEG-GSBER'  '9001' .
*    CONCATENATE bkpf-belnr 'Denkleştirmesinden kalan'
*    INTO bsik-sgtxt SEPARATED BY space .
    rellena_ftpost 'P' 2 'BSEG-SGTXT'  bsik-sgtxt .
*    rellena_ftpost 'P' 2 'RF05A-NEWUM' bsik-umskz .

    CONCATENATE bsik-belnr bsik-gjahr bsik-buzei into bsik-zuonr .
    rellena_ftpost 'P' 2 'BSEG-ZUONR' bsik-zuonr .

    zbd1t = bsik-zbd1t.
    zbd2t = bsik-zbd2t.
    zbd3t = bsik-zbd3t.
    rellena_ftpost 'P' 2 'BSEG-ZBD1T' zbd1t.
    rellena_ftpost 'P' 2 'BSEG-ZBD2T' zbd2t.
    rellena_ftpost 'P' 2 'BSEG-ZBD3T' zbd3t.

    bakiye = 'X' .

  ELSE .
    rellena_ftpost 'K' 1 'BKPF-BUKRS' wa_belgeler-bukrs .
    rellena_ftpost 'K' 1 'BKPF-BLART' blart .
    rellena_ftpost 'K' 1 'BKPF-BLDAT' budat_txt .
    rellena_ftpost 'K' 1 'BKPF-BUDAT' bldat_txt .
    rellena_ftpost 'K' 1 'BKPF-WAERS' wa_belgeler-waers .
    rellena_ftpost 'K' 1 'BKPF-BKTXT' 'Denkleştirme Belgesi' .

    rellena_ftpost 'P' 2 'BSEG-WRBTR' '' .

  ENDIF .


  CALL FUNCTION 'ZPOSTING_INTERFACE_CLEARING'
    EXPORTING
      i_auglv                    = 'EINGZAHL'
      i_tcode                    = 'FB05'
      i_sgfunct                  = 'C'
      bakiye                     = bakiye
    IMPORTING
      E_MSGID                    = es_ret2-id
      E_MSGNO                    = es_ret2-number
      E_MSGTY                    = es_ret2-type
      E_MSGV1                    = es_ret2-message_v1
      E_MSGV2                    = es_ret2-message_v2
      E_MSGV3                    = es_ret2-message_v3
      E_MSGV4                    = es_ret2-message_v4
      E_MESSAGE                  = MESSAGE
    TABLES
      t_blntab                   = l_t_blntab[]
      t_ftclear                  = l_t_ftclear[]
      t_ftpost                   = l_t_ftpost[]
      t_fttax                    = l_t_fttax[]
    EXCEPTIONS
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      OTHERS                     = 10.

  CALL FUNCTION 'FORMAT_MESSAGE'
    EXPORTING
      id        = sy-msgid
      lang      = sy-langu
      no        = sy-msgno
      v1        = sy-msgv1
      v2        = sy-msgv2
      v3        = sy-msgv3
      v4        = sy-msgv4
    IMPORTING
      msg       = msg
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR l_t_blntab .
  READ TABLE l_t_blntab INDEX 1 .
  IF sy-subrc IS INITIAL .
    awkey = l_t_blntab-gjahr . CONDENSE awkey .
    CONCATENATE l_t_blntab-belnr l_t_blntab-bukrs awkey
    INTO awkey .
  ENDIF .

  CALL FUNCTION 'ZPOSTING_INTERFACE_END'
    EXCEPTIONS
      session_not_processable = 1
      OTHERS                  = 2.


ENDFUNCTION.
