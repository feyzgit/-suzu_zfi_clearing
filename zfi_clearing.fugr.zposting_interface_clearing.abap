FUNCTION ZPOSTING_INTERFACE_CLEARING.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_AUGLV) LIKE  T041A-AUGLV
*"     VALUE(I_TCODE) LIKE  SY-TCODE
*"     VALUE(I_SGFUNCT) LIKE  RFIPI-SGFUNCT DEFAULT SPACE
*"     VALUE(I_NO_AUTH) DEFAULT SPACE
*"     VALUE(BAKIYE) TYPE  CHAR1 DEFAULT ''
*"  EXPORTING
*"     VALUE(E_MSGID) LIKE  SY-MSGID
*"     VALUE(E_MSGNO) LIKE  SY-MSGNO
*"     VALUE(E_MSGTY) LIKE  SY-MSGTY
*"     VALUE(E_MSGV1) LIKE  SY-MSGV1
*"     VALUE(E_MSGV2) LIKE  SY-MSGV2
*"     VALUE(E_MSGV3) LIKE  SY-MSGV3
*"     VALUE(E_MSGV4) LIKE  SY-MSGV4
*"     VALUE(E_SUBRC) LIKE  SY-SUBRC
*"     VALUE(E_MESSAGE) TYPE  BAPI_MSG
*"  TABLES
*"      T_BLNTAB STRUCTURE  BLNTAB
*"      T_FTCLEAR STRUCTURE  FTCLEAR
*"      T_FTPOST STRUCTURE  FTPOST
*"      T_FTTAX STRUCTURE  FTTAX
*"  EXCEPTIONS
*"      CLEARING_PROCEDURE_INVALID
*"      CLEARING_PROCEDURE_MISSING
*"      TABLE_T041A_EMPTY
*"      TRANSACTION_CODE_INVALID
*"      AMOUNT_FORMAT_ERROR
*"      TOO_MANY_LINE_ITEMS
*"      COMPANY_CODE_INVALID
*"      SCREEN_NOT_FOUND
*"      NO_AUTHORIZATION
*"----------------------------------------------------------------------


*------- Belegdaten initialisieren -------------------------------------

  TCODE = I_TCODE.
  AUGLV = I_AUGLV.
  SGFUNCT = I_SGFUNCT.
  PERFORM INIT_POSTING.
  CLEAR:   XFTCLEAR, DEFSIZE.
  REFRESH: XFTCLEAR.
  CLEAR:   YFTCLEAR.                                      "31i

*------- Transactionscode prüfen ---------------------------------------
  IF TCODE NE 'FB05' and TCODE NE 'FB05L'. "1527033
    MESSAGE E006 WITH TCODE RAISING TRANSACTION_CODE_INVALID.
  ENDIF.

*------- Reportname setzen  -----------------------------------------
  REP_NAME = REP_NAME_A.                        " Belegvorerfassung

*------- Tabellendaten übertragen --------------------------------------
  LOOP AT T_FTPOST.
    XFTPOST = T_FTPOST.
    APPEND XFTPOST.
  ENDLOOP.
  LOOP AT T_FTCLEAR.
*------- Data without selection field to be appended after sort --------
*------- Only one such item is allowed and it must be last      --------
*------- Additionally, the all-inclusive selection must be last also --
   IF T_FTCLEAR-SELFD = SPACE.                          "31i
    YFTCLEAR = T_FTCLEAR.
   ELSEIF T_FTCLEAR-SELFD   = 'BELNR'         "all other o/i via lbox
       AND T_FTCLEAR-SELVON = SPACE
       AND T_FTCLEAR-SELBIS = 'ZZZZZZZZZZ'.
    YFTCLEAR = T_FTCLEAR.
   ELSE.
    XFTCLEAR = T_FTCLEAR.
    APPEND XFTCLEAR.
   ENDIF.
  ENDLOOP.

  SORT XFTCLEAR BY AGKOA AGKON AGBUK XNOPS AGUMS.
  IF NOT YFTCLEAR = SPACE.                                "31i
    APPEND YFTCLEAR TO XFTCLEAR.
  ENDIF.

  PERFORM AUGLV_TABIX_ERMITTELN.

  LOOP AT T_FTTAX.
    XFTTAX = T_FTTAX.
    APPEND XFTTAX.
  ENDLOOP.
  DESCRIBE TABLE XFTTAX LINES TFILL_XFTTAX.

*------- Buchungsdatentabelle (XFTPOST) abarbeiten im Loop -------------
  PERFORM XFTPOST_LOOP.

  if BAKIYE eq 'X' .
    clear ft .
    ft-fnam = 'BDC_OKCODE' .
    ft-fval = '/06' .
    append ft .
  endif .

*------- Letzte Belegzeile übertragen ----------------------------------
  PERFORM POSITION_UEBERTRAGEN.

*------- Ausgleichsdaten (XFTCLEAR) abarbeiten -------------------------
  CLEAR DYNNR.
  LOOP AT XFTCLEAR.
    AT NEW AGKOA.
      PERFORM FCODE_F06_F07.
    ENDAT.

    AT NEW AGKON.
      PERFORM FCODE_F06_F07.
    ENDAT.

    AT NEW AGBUK.
      PERFORM FCODE_F06_F07.
    ENDAT.

    AT NEW XNOPS.
      PERFORM FCODE_F06_F07.
    ENDAT.

    AT NEW AGUMS.
      PERFORM FCODE_F06_F07.
    ENDAT.

    IF DYNNR = '0710'.
      PERFORM BSELK_UEBERGEBEN.
      CLEAR DYNNR.
    ENDIF.

    LOOPC = ( LOOPC + 1 ) MOD 18.
    IF LOOPC = 0.
      LOOPC = 18.
    ENDIF.
    IF LOOPC = 1.
      PERFORM FCODE_F05.
    ENDIF.
    PERFORM BSELP_UEBERGEBEN.

    DESCRIBE TABLE FT LINES INDEX.
  ENDLOOP.

*------- Transaktion abschließen ---------------------------------------
  PERFORM FCODE_F11.
  PERFORM TRANSAKTION_BEENDEN USING I_NO_AUTH
                CHANGING E_MESSAGE .

*------- Exportparameter zurückgeben (bei Call Transaction .. Using ..)-
  IF FUNCT   = 'C'
  OR SGFUNCT = 'C'.
    E_SUBRC = SUBRC.
    E_MSGTY = MSGTY.
    E_MSGID = MSGID.
    E_MSGNO = MSGNO.
    E_MSGV1 = MSGV1.
    E_MSGV2 = MSGV2.
    E_MSGV3 = MSGV3.
    E_MSGV4 = MSGV4.
    LOOP AT XBLTAB.
      T_BLNTAB = XBLTAB.
      APPEND T_BLNTAB.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
