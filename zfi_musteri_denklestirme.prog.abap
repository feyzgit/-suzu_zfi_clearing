*&---------------------------------------------------------------------*
*& Report  ZFI_MUSTERI_DENKLESTIRME
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zfi_musteri_denklestirme.

TABLES : kna1 , bkpf .

INCLUDE zalv_global .

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001 .
PARAMETERS : p_bukrs LIKE t001-bukrs MODIF ID BUK OBLIGATORY .
SELECT-OPTIONS : s_kunnr FOR kna1-kunnr ,
                 s_ktokd FOR kna1-ktokd DEFAULT 'ZF01' ,
                 s_budat FOR bkpf-budat OBLIGATORY ,
                 s_blart FOR bkpf-blart ,
                 s_belnr FOR bkpf-belnr .
PARAMETERS : p_waers TYPE waers OBLIGATORY DEFAULT 'TRY' .
SELECTION-SCREEN END OF BLOCK b1 .

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002 .
PARAMETERS : p_dnktar LIKE bkpf-budat DEFAULT sy-datum ,
             p_bldat like bkpf-bldat DEFAULT sy-datum ,
             p_blart LIKE bkpf-blart DEFAULT 'AB' ,
             p_mode TYPE mode DEFAULT 'N' .
SELECTION-SCREEN END OF BLOCK b2 .

DATA : BEGIN OF it_kna1 OCCURS 0 ,
        kunnr LIKE kna1-kunnr ,
        name1 LIKE kna1-name1 ,
       END OF it_kna1 .

DATA : BEGIN OF it_bsid OCCURS 0 ,
        bukrs LIKE bsid-bukrs ,
        belnr LIKE bsid-belnr ,
        gjahr LIKE bsid-gjahr ,
        budat LIKE bsid-budat ,
        bldat LIKE bsid-bldat ,
        buzei LIKE bsid-buzei ,
        shkzg LIKE bsid-shkzg ,
        wrbtr LIKE bsid-wrbtr ,
        sgtxt LIKE bsid-sgtxt ,
        zfbdt LIKE bsid-zfbdt ,
        zbd1t LIKE bsid-zbd1t ,
        zbd2t LIKE bsid-zbd2t ,
        zbd3t LIKE bsid-zbd3t ,
        rebzg LIKE bsid-rebzg ,
        kunnr LIKE bsid-kunnr ,
        name1 LIKE kna1-name1 ,
        kalan LIKE bsid-wrbtr ,
       END OF it_bsid .

DATA : BEGIN OF it_bsid_disp OCCURS 0 ,
        baicon(4) ,
        rowcolor(4) ,
        kunnr LIKE kna1-kunnr ,
        name1 LIKE kna1-name1 ,
        zfbdt LIKE bsid-zfbdt ,
        belnr LIKE bsid-belnr ,
        budat LIKE bsid-budat ,
        bldat LIKE bsid-bldat ,
        buzei LIKE bsid-buzei ,
        sgtxt LIKE bsid-sgtxt ,
        wrbtr LIKE bsid-wrbtr ,
        kalan LIKE bsid-wrbtr ,
       END OF it_bsid_disp .

DATA : BEGIN OF it_report OCCURS 0 ,
        selkz ,
        kunnr LIKE kna1-kunnr ,
        name1 LIKE kna1-name1 ,
        fattop LIKE bsid-wrbtr ,
        alacaktop LIKE bsid-wrbtr ,
        bakiye LIKE bsid-wrbtr ,
        awkey LIKE bkpf-awkey ,
*        kalanbelge like bkpf-belnr ,
*        kalantutar like bsid-WRBTR ,
       END OF it_report .

DATA : l_kalan TYPE wrbtr ,
       l_wrbtr TYPE wrbtr .

FIELD-SYMBOLS : <fs_bsid> LIKE it_bsid .

at SELECTION-SCREEN .
  clear : s_budat .
  read table s_budat index 1 .
  if sy-subrc is INITIAL .
    p_bldat = s_budat-high .
    if p_bldat is INITIAL .
      p_bldat = s_budat-low .
    endif .
  endif .

START-OF-SELECTION .

  SELECT * FROM bsid JOIN kna1 ON bsid~kunnr EQ kna1~kunnr
    INTO CORRESPONDING FIELDS OF TABLE it_bsid
    WHERE bsid~bukrs EQ p_bukrs AND
          bsid~umskz EQ '' AND
          bsid~kunnr IN s_kunnr AND
          kna1~ktokd IN s_ktokd AND
          bsid~budat IN s_budat AND
          bsid~blart IN s_blart AND
          bsid~belnr IN s_belnr AND
          bsid~waers EQ p_waers .

  LOOP AT it_bsid ASSIGNING <fs_bsid> .
    CALL FUNCTION 'NET_DUE_DATE_GET'
      EXPORTING
        i_zfbdt = <fs_bsid>-zfbdt
        i_zbd1t = <fs_bsid>-zbd1t
        i_zbd2t = <fs_bsid>-zbd2t
        i_zbd3t = <fs_bsid>-zbd3t
        i_shkzg = <fs_bsid>-shkzg
        i_rebzg = <fs_bsid>-rebzg
*       I_KOART = 'D'
      IMPORTING
        e_faedt = <fs_bsid>-zfbdt.
  ENDLOOP .

  SORT it_bsid BY zfbdt DESCENDING wrbtr DESCENDING belnr
                        DESCENDING buzei DESCENDING.
*SORT it_bsid by kunnr zfbdt WRBTR buzei .

  LOOP AT it_bsid ASSIGNING <fs_bsid> .
    CLEAR : it_kna1 .
    MOVE-CORRESPONDING <fs_bsid> TO it_kna1 .
    COLLECT it_kna1 .

    CLEAR : it_report .
    it_report-kunnr = <fs_bsid>-kunnr .
    it_report-name1 = <fs_bsid>-name1 .

    IF <fs_bsid>-shkzg EQ 'H' .
      it_report-alacaktop = <fs_bsid>-wrbtr .
    ELSE .
      it_report-fattop = <fs_bsid>-wrbtr .
    ENDIF .
    it_report-bakiye = it_report-fattop - it_report-alacaktop .

    COLLECT it_report .
  ENDLOOP .

  LOOP AT it_report .

    l_kalan = it_report-bakiye .
    IF l_kalan LT 0 .
      MULTIPLY l_kalan BY -1 .
    ENDIF .

    LOOP AT it_bsid ASSIGNING <fs_bsid>
                WHERE kunnr EQ it_report-kunnr .

      IF it_report-bakiye GT 0 .
        IF <fs_bsid>-shkzg EQ 'H' .
          <fs_bsid>-kalan = 0 .
          CONTINUE .
        ENDIF .
*  CHECK <fs_bsid>-shkzg eq 'S' .
      ELSE .
        IF <fs_bsid>-shkzg EQ 'S' .
          <fs_bsid>-kalan = 0 .
          CONTINUE .
        ENDIF .
*  CHECK <fs_bsid>-shkzg eq 'H' .
      ENDIF .

      l_wrbtr = <fs_bsid>-wrbtr .

      IF l_kalan IS NOT INITIAL .
        IF l_wrbtr LT l_kalan .
          SUBTRACT l_wrbtr FROM l_kalan .
          <fs_bsid>-kalan =  <fs_bsid>-wrbtr .
        ELSE .
          <fs_bsid>-kalan = l_kalan .
          IF <fs_bsid>-shkzg EQ 'H' .
            MULTIPLY <fs_bsid>-kalan BY -1  .
          ENDIF .
          CLEAR : l_kalan .
*  it_report-kalanbelge = <fs_bsid>-belnr .
*  it_report-kalantutar = <fs_bsid>-kalan .
        ENDIF .
      ELSE .
        <fs_bsid>-kalan = 0 .
      ENDIF .

    ENDLOOP .

    MODIFY it_report .

  ENDLOOP .

END-OF-SELECTION .


  CLEAR : lt_t_fieldcatalog , lt_t_fieldcatalog[] .
  v_default_recname = 'IT_REPORT' .
  v_default_report_name = sy-repid .
  PERFORM set_report_fcat.
  PERFORM show_report_fcat_lvc TABLES it_report
                      USING  ''"P_VARI
                             gs_variant
                             v_default_report_name
                             v_default_recname.

*&---------------------------------------------------------------------*
*&      Form  get_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FV_REPORT  text
*----------------------------------------------------------------------*
FORM get_detail USING fv_report LIKE LINE OF it_report .

  CLEAR : it_bsid_disp , it_bsid_disp[] .
  LOOP AT it_bsid WHERE kunnr EQ fv_report-kunnr .
    CLEAR : it_bsid_disp .
    MOVE-CORRESPONDING it_bsid TO it_bsid_disp .
    IF it_bsid-shkzg EQ 'H' .
      MULTIPLY it_bsid_disp-wrbtr BY -1 .
      it_bsid_disp-baicon = icon_negative .
    ELSE .
      it_bsid_disp-baicon = icon_positive .
    ENDIF .
    IF it_bsid_disp-kalan EQ 0 .
      it_bsid_disp-rowcolor = color_light_green .
    ELSEIF it_bsid_disp-kalan EQ it_bsid_disp-wrbtr .
      it_bsid_disp-rowcolor = color_light_red .
    ELSEIF it_bsid_disp-kalan NE it_bsid_disp-wrbtr .
      it_bsid_disp-rowcolor = color_light_yellow .
    ENDIF .
    APPEND it_bsid_disp .
  ENDLOOP .
  SORT it_bsid_disp BY rowcolor ASCENDING baicon ASCENDING
        zfbdt DESCENDING wrbtr DESCENDING belnr
              DESCENDING buzei DESCENDING.

ENDFORM .                    "get_detail

*&---------------------------------------------------------------------*
*&      Form  clearing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM clearing .

  DATA : lt_belgeler TYPE zfi_tt_denk_belge WITH HEADER LINE .

  LOOP AT it_report WHERE selkz IS NOT INITIAL .


    CLEAR : lt_belgeler , lt_belgeler[] .

    PERFORM get_detail USING it_report .

    loop at it_bsid_disp where
      ( rowcolor eq color_light_green or
        rowcolor eq color_light_yellow ).
      lt_belgeler-bukrs = p_bukrs .
      lt_belgeler-belnr = it_bsid_disp-belnr .
      lt_belgeler-gjahr = it_bsid_disp-budat(4) .
      lt_belgeler-buzei = it_bsid_disp-buzei .
      lt_belgeler-waers = p_waers .
      lt_belgeler-KALANTUTAR = it_bsid_disp-kalan .
      if lt_belgeler-KALANTUTAR lt 0 .
        MULTIPLY lt_belgeler-KALANTUTAR by -1 .
      endif .
      append lt_belgeler .
    endloop .

    CALL FUNCTION 'ZFI_CUST_CLEARING'
      EXPORTING
        it_belgeler = lt_belgeler[]
        denktar     = p_dnktar
        BLDAT       = p_bldat
        blart       = p_blart
*       AGUMS       = ''
        waers       = p_waers
        mode        = p_mode
      IMPORTING
        awkey       = it_report-awkey.

    modify it_report .
  ENDLOOP .

ENDFORM .                    "clearing

*&---------------------------------------------------------------------*
*&      Form  SET_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_top_of_page.

  CLEAR: gt_list_top_of_page,gt_list_top_of_page[].
  PERFORM comment_build USING gt_list_top_of_page[].

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = 'ENJOYSAP_LOGO'
      it_list_commentary = gt_list_top_of_page.

ENDFORM.                    "set_top_of_page
*---------------------------------------------------------------------*
*       FORM COMMENT_BUILD                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  LT_TOP_OF_PAGE                                                *
*---------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.


ENDFORM.                    "comment_build
*---------------------------------------------------------------------*
*  FORM f01_user_command
*---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_user_command USING r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield. "#EC CALLED
  CASE r_ucomm .
    WHEN '&IC1' .
      IF v_default_recname EQ 'IT_REPORT' .
        if rs_selfield-fieldname eq 'AWKEY' .
           set PARAMETER ID 'BUK' FIELD rs_selfield-value+10(4) .
           set PARAMETER ID 'BLN' FIELD rs_selfield-value(10) .
           set PARAMETER ID 'GJR' FIELD rs_selfield-value+14(4) .
           call TRANSACTION 'FB03' and SKIP FIRST SCREEN .
        else .
        CLEAR : it_report .
        READ TABLE it_report INDEX rs_selfield-tabindex .
        IF sy-subrc IS INITIAL .
          PERFORM get_detail USING it_report .
          CLEAR : lt_t_fieldcatalog , lt_t_fieldcatalog[] .
          v_default_recname = 'IT_BSID_DISP' .
          v_default_report_name = sy-repid .
          PERFORM set_report_fcat.
          PERFORM show_report_fcat_lvc TABLES it_bsid_disp
                              USING  ''"P_VARI
                                     gs_variant
                                     v_default_report_name
                                     v_default_recname.

          v_default_recname = 'IT_REPORT' .
          v_default_report_name = sy-repid .

        ENDIF .
       ENDIF .
      ENDIF .
    WHEN '&CLEAR' .
      PERFORM clearing .
  ENDCASE .

  rs_selfield-refresh = 'X'.
ENDFORM.                    "f01_user_command

*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM set_pf_status_set USING rt_extab TYPE slis_t_extab .   "#EC CALLED
  PERFORM set_excluding_tab TABLES rt_extab.
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab[].
ENDFORM.                    "f01_set_status
*&---------------------------------------------------------------------*
*&      Form  excluding_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excluding_events.

  PERFORM exclude_events TABLES ex_events USING 'CALLER_EXIT'.
*  PERFORM exclude_events TABLES ex_events USING 'USER_COMMAND'.
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_PAGE'.
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_COVERPAGE'.
  PERFORM exclude_events TABLES ex_events USING 'END_OF_COVERPAGE'.
  PERFORM exclude_events TABLES ex_events USING 'FOREIGN_TOP_OF_PAGE'.
  PERFORM exclude_events TABLES ex_events USING 'FOREIGN_END_OF_PAGE'.
*  PERFORM exclude_events TABLES ex_events USING 'PF_STATUS_SET'.
  PERFORM exclude_events TABLES ex_events USING 'LIST_MODIFY'.
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_LIST'.
  PERFORM exclude_events TABLES ex_events USING 'END_OF_PAGE'.
  PERFORM exclude_events TABLES ex_events USING 'END_OF_LIST'.
  PERFORM exclude_events TABLES ex_events USING 'AFTER_LINE_OUTPUT'.
  PERFORM exclude_events TABLES ex_events USING 'BEFORE_LINE_OUTPUT'.
  PERFORM exclude_events TABLES ex_events USING 'REPREP_SEL_MODIFY'.
  PERFORM exclude_events TABLES ex_events USING 'SUBTOTAL_TEXT'.
  PERFORM exclude_events TABLES ex_events USING 'GROUPLEVEL_CHANGE'.

*  PERFORM APPEND_EVENTS  TABLES AP_EVENTS USING 'DATA_CHANGED'.
*  PERFORM APPEND_EVENTS  TABLES AP_EVENTS USING 'ITEM_DATA_EXPAND'.
*  PERFORM APPEND_EVENTS  TABLES AP_EVENTS USING 'GROUPLEVEL_CHANGE'.
ENDFORM.                    " excluding_events

*&---------------------------------------------------------------------*
*&      Form  SET_EXCLUDING_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->EXTAB      text
*----------------------------------------------------------------------*
FORM set_excluding_tab TABLES extab.
  REFRESH extab.
*  EXTAB = '&ABC'.      APPEND EXTAB.
*  extab = '&UMC'.      append extab.
*  extab = '%SL' .      append extab.
*  extab = '&SUM'.      append extab.
*  extab = '&OL0'.      append extab.
*  extab = '&OAD'.      append extab.
*  extab = '&AVE'.      append extab.
*  extab = '&ILT'.      append extab.
*  extab = '&ETA'.      append extab.
*  extab = '%PC' .      append extab.
*  extab = '&ALL'.      append extab.
*  extab = '&SAL'.      append extab.
*  EXTAB = '&EB9'.      APPEND EXTAB.
*  EXTAB = '&REFRESH'.  APPEND EXTAB.
*  extab = '&OUP'.      append extab.
*  extab = '&ODN'.      append extab.
*  extab = '&RNT_PREV'. append extab.
*  extab = '&VEXCEL'.   append extab.
*  extab = '&AOW'.      append extab.
*  EXTAB = '&GRAPH'.    APPEND EXTAB.
*  EXTAB = '&INFO'.     APPEND EXTAB.
*  EXTAB = '&DET'.     APPEND EXTAB.

if v_default_recname ne 'IT_REPORT' .
  EXTAB = '&CLEAR'.     APPEND EXTAB.
endif .

ENDFORM.                    " set_excluding_tab

*&---------------------------------------------------------------------*
*&      Form  SET_REPORT_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_report_fcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = v_default_report_name
      i_internal_tabname     = v_default_recname
      i_inclname             = v_default_report_name
      i_client_never_display = 'X'
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = lt_t_fieldcatalog[]
    EXCEPTIONS
      OTHERS                 = 3.

  PERFORM set_field_cat_user_exit.

ENDFORM.                    " set_report_fcat

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CAT_USER_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_field_cat_user_exit .
  DATA: recname TYPE slis_tabname.
  DATA : v_title(42) TYPE c.
  MOVE: 'SELTEXT_L/SELTEXT_M/SELTEXT_S/REPTEXT_DDIC' TO v_title.
  recname = v_default_recname .

  LOOP AT lt_t_fieldcatalog WHERE key IS NOT INITIAL .
    CLEAR : lt_t_fieldcatalog-key .
    MODIFY lt_t_fieldcatalog .
  ENDLOOP .

  PERFORM
    set_line_field_cat TABLES lt_t_fieldcatalog USING :
    recname 'FATTOP'   v_title 'Borç Toplamı' ,
    recname 'ALACAKTOP'   v_title 'Alacak Toplamı' ,
    recname 'BAKIYE'   v_title 'Bakiye' ,
    recname 'KALANBELGE'   v_title 'Kismi Denkleşen Belge' ,
    recname 'KALANTUTAR'   v_title 'Kalan Tutar(Denkleştirmeden)' ,
    recname 'KALAN' v_title 'Denkleştirmeden Kalan' ,
    recname 'WRBTR' v_title 'Belge Tutarı' ,
    recname 'AWKEY' v_title 'Denkleştirme Belgesi'.

  DELETE lt_t_fieldcatalog WHERE fieldname EQ 'ROWCOLOR' OR
                                 fieldname EQ 'SELKZ' .

ENDFORM.                    " set_field_cat_user_exit

*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT_USER_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PVARI    text
*      -->P_DRNAME   text
*----------------------------------------------------------------------*
FORM set_layout_user_exit USING    p_pvari
                                   p_drname.
*  GS_GRID_SET-EDT_CLL_CB = 'X'.

*  GS_LAYOUT-GET_SELINFOS       = 'X'.
*  GS_LAYOUT-COLTAB_FIELDNAME   = 'COLOR'.
  IF v_default_recname EQ 'IT_REPORT' .
    CLEAR : gs_layout-info_fieldname .
    gs_layout-box_fieldname = 'SELKZ' .
  ELSE .
    gs_layout-info_fieldname     = 'ROWCOLOR' .
    CLEAR : gs_layout-box_fieldname .
  ENDIF .
*  gs_layout-coltab_fieldname   = 'COLOR'.
*  gs_layout-expand_fieldname  = 'BUKRS'.

  gs_layout-colwidth_optimize = 'X' .
ENDFORM.                    " set_layout_user_exi
