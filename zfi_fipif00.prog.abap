*&---------------------------------------------------------------------*
*&  Include           ZFI_FIPIF00
*&---------------------------------------------------------------------*

***INCLUDE LFIPIF00 .

*eject
*----------------------------------------------------------------------*
*       Form  ABWEICHENDE_ZENTRALE
*----------------------------------------------------------------------*
form abweichende_zentrale.

  select single * from t001 where bukrs = bukrs.
  if sy-subrc ne 0.
    message a016 with bukrs.
  endif.
  select single * from t019w where mpool = 'SAPMF05A'
                               and winfk = 'FIL'
                               and buvar = t001-buvar.
  if sy-subrc ne 0
  and t001-buvar ne space.
    select single * from t019w where mpool = 'SAPMF05A'
                                 and winfk = 'FIL'
                                 and buvar = space.
  endif.
  if sy-subrc ne 0.
    message a018 with 'T019W' 'SAPMF05A' 'FIL' t001-buvar.
  endif.

  clear ft.
  ft-program  = rep_name.
  ft-dynpro   = t019w-winnr.
  ft-dynbegin = 'X'.
  append ft.

  clear ft.
* FT-FNAM = 'RF05A-NEWKO'.                                     "30F
  ft-fnam = fnam_konto.                                     "30F
  ft-fval = knrze.
  append ft.
  clear ft.

endform.                               " ABWEICHENDE_ZENTRALE

*eject
*-----------------------------------------------------------------------
*        Form  AUGLV_TABIX_ERMITTELN
*-----------------------------------------------------------------------
*        Tabellenindex für Ausgleichsvorgang aus T041A ermitteln
*-----------------------------------------------------------------------
form auglv_tabix_ermitteln.
  if auglv = space.
    message e009 with tcode raising clearing_procedure_missing.
  endif.

*------- interne Tabelle für Ausgleichsvorgänge füllen -----------------
  if tfill_041a = 0.
    select * from t041a.
      xt041a-auglv = t041a-auglv.
      append xt041a.
    endselect.
    describe table xt041a lines tfill_041a.
    if tfill_041a = 0.
      message a010 raising table_t041a_empty.
    endif.
  endif.

*------- Tabix für Ausgleichsvorgang merken ----------------------------
  tabix_041a = 0.
  loop at xt041a where auglv = auglv.
    tabix_041a = sy-tabix.
    exit.
  endloop.
  if tabix_041a = 0.
    message e011 with auglv raising clearing_procedure_invalid.
  endif.
endform.                    "auglv_tabix_ermitteln

*eject
*-----------------------------------------------------------------------
*        Form  BSELK_UEBERGEBEN
*-----------------------------------------------------------------------
*        Selektionskopfdaten aus FTCLEAR auf Dynpro 710 übergeben.
*-----------------------------------------------------------------------
form bselk_uebergeben.
  if xftclear-agkoa ne space.
    clear ft.
    ft-fnam = 'RF05A-AGKOA'.
    ft-fval = xftclear-agkoa.
    append ft.
  endif.

  clear ft.
  ft-fnam = 'RF05A-AGKON'.
  ft-fval = xftclear-agkon.
  append ft.

  clear ft.
  ft-fnam = 'RF05A-AGBUK'.
  ft-fval = xftclear-agbuk.
  append ft.

  clear ft.
  ft-fnam = 'RF05A-XNOPS'.
  ft-fval = xftclear-xnops.
  append ft.

  clear ft.
  ft-fnam = 'RF05A-AGUMS'.
  ft-fval = xftclear-agums.
  append ft.

  if not xftclear-xfifo is initial.
    clear ft.
    ft-fnam = 'RF05A-XFIFO'.
    ft-fval = xftclear-xfifo.
    append ft.
  endif.

  if not xftclear-avsid is initial.
    clear ft.
    ft-fnam = 'RF05A-AVSID'.
    ft-fval = xftclear-avsid.
    append ft.
  endif.

*   Cursor setzen, sonst funktioniert Matchcode Eingabe für Konto nicht
  clear ft.
  ft-fnam = 'BDC_CURSOR'.
  ft-fval = 'RF05A-AGKON'.
  append ft.
endform.                    "bselk_uebergeben

*eject
*-----------------------------------------------------------------------
*        Form  BSELP_UEBERGEBEN
*-----------------------------------------------------------------------
*        Selektionspositionsdaten aus FTCLEAR auf Dynpro 733 übergeben.
*-----------------------------------------------------------------------
form bselp_uebergeben.
*------- falls keine Selektionsdaten Daten nicht senden -----------
  check not xftclear-selfd is initial.

  clear ft.
  ft-fnam(12)    = 'RF05A-FELDN('.
  ft-fnam+12(02) = loopc.
  ft-fnam+14(01) = ')'.
  condense ft-fnam no-gaps.
  ft-fval = xftclear-selfd.
  append ft.

  clear ft.
  ft-fnam(12)    = 'RF05A-SEL01('.
  ft-fnam+12(02) = loopc.
  ft-fnam+14(01) = ')'.
  condense ft-fnam no-gaps.
  ft-fval = xftclear-selvon.
  append ft.

  clear ft.
  ft-fnam(12)    = 'RF05A-SEL02('.
  ft-fnam+12(02) = loopc.
  ft-fnam+14(01) = ')'.
  condense ft-fnam no-gaps.
  ft-fval = xftclear-selbis.
  append ft.

* DESCRIBE TABLE FT LINES INDEX.
endform.                    "bselp_uebergeben

*eject
*-----------------------------------------------------------------------
*        Form  DYNPRO_ERMITTELN
*-----------------------------------------------------------------------
*        Nummer des Standardbildes und Zusatzbildes ermitteln.
*-----------------------------------------------------------------------
form dynpro_ermitteln.
  data: konto_n(10)   type n.
  data: ld_tcode      like tcode. "1527033

*------- Tabelle TBSL  lesen: (1. interne Tabelle, 2. ATAB-Tabelle) ----
  loop at xtbsl where bschl = bschl.
    exit.
  endloop.
  if sy-subrc ne 0.
    select single * from tbsl where bschl = bschl.
    if sy-subrc = 0.
      xtbsl = tbsl.
      append xtbsl.
    else.
*     exit, wg  FB05 ohne Buchungszeilen
      exit.
*     MESSAGE E008 WITH BSCHL RAISING POSTING_KEY_INVALID.
    endif.
  endif.

*------- Windowfunktion setzen -----------------------------------------
  clear winfk.
  case xtbsl-koart.
    when 'D'.
      winfk = 'ZKOD'.
    when 'K'.
      winfk = 'ZKOK'.
    when 'S'.
      winfk = 'ZKOS'.
    when 'A'.
      winfk = 'ZKOA'.
  endcase.

*------- Steuerkategorie ermitteln -------------------------------------
  clear mwskzs.
  if xtbsl-koart = 'S'.
    if konto co ' 0123456789'.
      konto_n = konto.
      konto   = konto_n.
    endif.
    select single * from skb1 where bukrs = bukrs
                                and saknr = konto.
    if sy-subrc eq 0.
      mwskzs = skb1-mwskz.
    endif.
  endif.

  if tcode = 'FB05L'.  "1527033
    ld_tcode = 'FB05'. "1527033
  ELSEIF tcode = 'FBCB'.                                    "1562986
    ld_tcode = 'FBB1'.                                      "1562986
  else.                "1527033
    ld_tcode = tcode.  "1527033
  endif.               "1527033

*------- Dynpronummern ermitteln ---------------------------------------
  call function 'NEXT_DYNPRO_SEARCH'
    exporting
      i_bschl  = bschl
      i_bukrs  = bukrs
      i_mwskzs = mwskzs
      i_tcode  = ld_tcode
      i_umskz  = umskz
      i_winfk  = winfk
    importing
      e_dynnra = dynnr
      e_mpool  = mpool
      e_winnrz = winnr
    exceptions  "nur noch nicht bereits geprueften Ausnahmen
      bukrs_nf = 1
      dynnr_nf = 2
      tcodd_nf = 3
      tcodm_nf = 4
      winnr_nf = 5
      others   = 6.

  if sy-subrc <> 0.
    case sy-subrc.
      when '1'.
        message id sy-msgid type 'E' number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                raising company_code_invalid.
      when '2' or '5' or '6'.
        message id sy-msgid type 'E' number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                raising screen_not_found.
      when '3' or '4'.
        message id sy-msgid type 'E' number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                raising transaction_code_invalid.
    endcase.
  endif.

* Dynpro ändern bei der Umbuchung der Steuerlast
  IF ( tcode = 'FB41' or tcode = 'FBCB' ) AND dynnr = '312'.
    dynnr = '300'.
  endif.
endform.                    "dynpro_ermitteln

*eject
*----------------------------------------------------------------------
*        Form  DYNPRO_SENDEN_VV_BER_BESTAND.
*----------------------------------------------------------------------
*        Dieses Dynpro ist für Vermögensverwaltung und soll vor dem
*        Anlagedynpro gesendet werden, um LFVI9-SBERI zu übergeben.
*        LFVI9-SBERI ist in FTVK, da es auch über den Kontierungsblock
*        der Vermögensverwaltung eingegeben werden kann.
*----------------------------------------------------------------------
form dynpro_senden_vv_ber_bestand.
  clear ft.
  ft-program  = rep_name_vk.
  ft-dynpro   = '0200'.
  ft-dynbegin = 'X'.
  append ft.
  loop at ftvk.
    ft = ftvk.
    append ft.
  endloop.
  refresh ftvk.
  describe table ftvk lines tfill_ftvk.
endform.                    "dynpro_senden_vv_ber_bestand

*eject
*-----------------------------------------------------------------------
*        Form  DYNPRO_SENDEN_FTA
*-----------------------------------------------------------------------
*        Daten für Standardbild aus FTA in FT übertragen
*        Für Übertragung des BSCHL/KONTO/.. aus nächster Position
*        muß der Index gemerkt werden.
*-----------------------------------------------------------------------
form dynpro_senden_fta using a_dynnr.
  clear ft.
  ft-program  = rep_name.
  ft-dynpro   = a_dynnr.
  ft-dynbegin = 'X'.
  append ft.

* Falls es sich um das Dynpro 312 (Steuerdynpro) handelt, existiert
* das Flag BKPF-XMWST nicht und muß deswegen auf das nächste
* Hauptdynpro verschoben werden.  Dazu merkt man sich mittels XMWST_SET
* fuer die naechste Belegzeile, ob BKPF-XMWST noch angekreuzt werden
* muss.

  if ( a_dynnr = '0304' or a_dynnr = '0312' or
       a_dynnr = '2320' or a_dynnr = '0320' )
     and xmwst = 'X'.
    if a_dynnr = '0304'.
      clear ft.
      ft-fnam = 'RF05A-XMWST'.
      ft-fval = 'X'.
      append ft.
    endif.
    delete fta where fnam = 'BKPF-XMWST'.
    if sy-subrc = 0.
      xmwst_set = 'X'.
    endif.
  endif.

  loop at fta.
    if fta-fnam = 'BKPF-XMWST'.
      clear xmwst_set.
    endif.
    if a_dynnr = '0304'.
      if fta-fnam = 'BKPF-XMWST'
      or fta-fnam = 'BSEG-VORNR'.
        shift fta-fnam right.
        fta-fnam(5) = 'RF05A'.
      endif.
    endif.
    ft = fta.
    append ft.
  endloop.

  if a_dynnr <> '0304' and
     a_dynnr <> '0312' and
     a_dynnr <> '0320' and
     a_dynnr <> '2320' and
     xmwst_set = 'X'.
    clear ft.
    ft-fnam = 'BKPF-XMWST'.
    ft-fval = 'X'.
    append ft.
    clear xmwst_set.
  endif.

  if a_dynnr = '0300'
  or a_dynnr = '0301'
  or a_dynnr = '0302'
  or a_dynnr = '0312'
  or a_dynnr = '0304'
  or a_dynnr = '0305'.
    clear ft.
    ft-fnam = 'BDC_CURSOR'.
    ft-fval = fnam_konto.                                   "30F
*   IF REP_NAME = REP_NAME_BV.                              "30F
*     Bei Belegvorerfassung                                 "30F
*     FT-FVAL = 'RF05V-NEWKO'.                              "30F
*   ELSE.                                                   "30F
*     FT-FVAL = 'RF05A-NEWKO'.                              "30F
*   ENDIF.                                                  "30F
    append ft.
  endif.

  describe table ft lines index.
endform.                    "dynpro_senden_fta

*eject
*-----------------------------------------------------------------------
*        Form  DYNPRO_SENDEN_FTC
*-----------------------------------------------------------------------
*        CPD-Daten aus FTC in FT übertragen
*-----------------------------------------------------------------------
form dynpro_senden_ftc.

  data: move_to_iban(1) type c.

  field-symbols <ftc> type BDCDATA.

  clear ft.
  ft-program  = rep_name_c.
  ft-dynpro   = '0100'.
  ft-dynbegin = 'X'.
  append ft.

* IBAN without bank account number
* if BANKN is available: move BANKL and BANKS to CPD screen
* if BANKN is empty:     move BANKL and BANKS to IBAN popup
  clear move_to_iban.
  if tfill_ftiban ne 0.
    read table ftc with key FNAM = 'BSEC-BANKN' assigning <ftc>.
    if sy-subrc ne 0 or <ftc>-fval is initial.
      move_to_iban = 'X'.
    endif.
  endif.

  loop at ftc.
    if ( ftc-fnam = 'BSEC-BANKL' or ftc-fnam = 'BSEC-BANKS' )
       and move_to_iban = 'X'.
       clear ftiban.
       concatenate 'TIBAN-' ftc-fnam+5 into ftiban-fnam.
       ftiban-fval = ftc-fval.
       append ftiban.
       continue.
    endif.

    ft = ftc.
    append ft.
  endloop.
endform.                    "dynpro_senden_ftc

*eject
*-----------------------------------------------------------------------
*        Form  DYNPRO_SENDEN_FTK
*-----------------------------------------------------------------------
*        Kontierungsblockdaten aus FTK in FT übertragen
*        Dynpro '0002' enthält alle Daten des Kontierungsblocks
*        außerdem:
*        Daten des VV-Kontierungsblock-Dynpros aus aus FTVK in FT
*        übertragen (nur dann, wenn COBL-IMKEY nicht übergeben wurde)
*-----------------------------------------------------------------------
form dynpro_senden_ftk.
  data: imkey like bdcdata-fnam value 'COBL-IMKEY'.
  data: ld_kontl type kontl_fi,                            "Note 604733
        ld_kontt type kontt_fi.                            "Note 604733
  data: lt_bukrs type fagl_t_bukrs,                        "Note1605537
        ls_bukrs type fagl_s_bukrs,                        "Note1605537
        ld_splt_active type xfeld.                         "Note1605537

*------- tax screen: only with NewGL/doc.split active ----- Note1137272*
  if  dynnr = '0312'.                                      "Note1137272
    check: not glflex_active is initial.                   "Note1605537
*                                                               "
    clear: ld_splt_active.                                      "
    refresh: lt_bukrs.                                          "
    move bukrs to ls_bukrs.                                     "
    append ls_bukrs to lt_bukrs.                                "
    call method cl_fagl_split_services=>check_activity          "
      exporting                                                 "
        it_bukrs  = lt_bukrs                                    "
      receiving                                                 "
        rb_active = ld_splt_active.                             "
    check: not ld_splt_active is initial.                  "Note1605537
  endif.                                                   "Note1137272

  clear ft.
  ft-program  = rep_name_k.
  ft-dynpro   = '0002'.
  ft-dynbegin = 'X'.
  append ft.
  perform fcode_enter.
  clear in_cobl.

  loop at ftk.

    if tfill_ftcopa > 0.
      perform fill_in_cobl using ftk-fnam+5(127) ftk-fval.
    endif.

    ft = ftk.
    append ft.
  endloop.

* Check if Insurance screen needs to be sent.              Note 604733
* Maybe more details screens from other components.
  if tfill_ft_generic_kontl > 0.
    read table ft_generic_kontl with key fnam = 'BSEG-KONTL'.
    ld_kontl = ft_generic_kontl-fval.

    read table ft_generic_kontl with key fnam = 'BSEG-KONTT'.
    ld_kontt = ft_generic_kontl-fval.

    clear ft.

    case ld_kontt.
      WHEN 'VV' or 'VX'.    " account assignment type of IS insurance
* set 'details' flag
        ft-fnam = 'DKACB-XINSUR'.
        ft-fval = 'X'.
        append ft.
      when others.
*  call whatever field here, similar to example above
*  See routine generic_kontl_data.
    endcase.
  endif.
* End of insertion                                         Note 604733

* Prüfen, ob zusätzl. Kontierungsblockbild der VermögVerw. zu senden ist
  describe table ftvk lines tfill_ftvk.
  if tfill_ftvk = 0.
    exit.
  endif.

* nicht senden, wenn COBL-IMKEY gefüllt ist (FTVK löschen)
  loop at ftk where fnam = imkey.
    refresh ftvk.
  endloop.

* Übertragen in FT
  describe table ftvk lines tfill_ftvk.
  if tfill_ftvk > 0.
*   'Weiter-Flag' setzen auf Dynpro '0002'
    clear ft.
    ft-fnam     = 'DKACB-XIMKY'.
    ft-fval     = 'X'.
    append ft.
    clear ft.

*   VV-Kontierungsblock-Dynpro
    ft-program  = rep_name_vk.
    ft-dynpro   = '0100'.
    ft-dynbegin = 'X'.
    append ft.
    loop at ftvk.
      ft = ftvk.
      append ft.
    endloop.

*   zurück zum Standard-Kontierungsblock-Dynpro
    clear ft.
    ft-program  = rep_name_k.
    ft-dynpro   = '0002'.
    ft-dynbegin = 'X'.
    append ft.
  endif.

endform.                    "dynpro_senden_ftk

*eject
*-----------------------------------------------------------------------
*        Form  DYNPRO_SENDEN_FTZ
*-----------------------------------------------------------------------
*        Zusatzdaten aus FTZ in FT übertragen
*-----------------------------------------------------------------------
form dynpro_senden_ftz using z_dynnr.
  clear ft.
  ft-program  = rep_name.
  ft-dynpro   = z_dynnr.
  ft-dynbegin = 'X'.
  append ft.
  loop at ftz.
    ft = ftz.
    append ft.
  endloop.
* Index für Fußzeile hochsetzen
  describe table ft lines index.
endform.                    "dynpro_senden_ftz

*eject
*-----------------------------------------------------------------------
*        Form  DYNPRO_SENDEN_FTVV
*-----------------------------------------------------------------------
*        Daten für Vermögensverwalter aus FTVV in FT übertragen
*        vorher muß noch auf dem alten Bild der OK_Code 'SOPT' gesendet
*        werden.
*-----------------------------------------------------------------------
form dynpro_senden_ftvv.
  clear ft.
  ft-fnam     = 'BDC_OKCODE'.
  ft-fval     = 'SOPT'.
  index = index + 1.
  insert ft index index.
  clear ft.
  ft-program  = rep_name_vv.
  ft-dynpro   = '0100'.
  ft-dynbegin = 'X'.
  append ft.
  loop at ftvv.
    ft = ftvv.
    append ft.
  endloop.
  clear ft.
  ft-fnam     = 'BDC_OKCODE'.
  ft-fval     = '/11'.
  append ft.

endform.                    "dynpro_senden_ftvv


*eject
*-----------------------------------------------------------------------
*        Form  DYNPRO_SETZEN_EINSTIEG
*-----------------------------------------------------------------------
*        Einstiegsbild setzen in Tabelle FT.
*-----------------------------------------------------------------------
form dynpro_setzen_einstieg.
  data ld_tcode          LIKE sy-tcode.             "1562986
* CASE TCODE.
*   WHEN 'ABF1'.
*     DYNNR = '0100'.
*   WHEN 'FB01'.
*     DYNNR = '0100'.
*   WHEN 'FB41'.
*     DYNNR = '0100'.
*   WHEN 'FBB1'.
*     DYNNR = '0100'.
*   WHEN 'FB05'.
*     DYNNR = '0122'.
*   WHEN 'FBVB'.
*     DYNNR = '0100'.
*   WHEN 'FBD5'.
*     DYNNR = '0125'.
* ENDCASE.
  IF tcode = 'FBCB'.                                "1562986
    ld_tcode = 'FBB1'.                              "1562986
  ELSE.                                             "1562986
    ld_tcode = tcode.                               "1562986
  ENDIF.                                            "1562986
  SELECT SINGLE * FROM tstc WHERE tcode = ld_tcode. "1562986
  if sy-subrc = 0.
    dynnr = tstc-dypno.
  else.
    MESSAGE e018 WITH 'TSTC' ld_tcode.              "1562986
  endif.

  clear ft.
  ft-program  = rep_name.
  ft-dynpro   = dynnr.
  ft-dynbegin = 'X'.
  append ft.
  index = index + 1.

*-------- set fieldname for new account -------------------------------
  if rep_name = rep_name_bv.                                "30F
*   preliminary posting                                   "30F
    fnam_konto = 'RF05V-NEWKO'.                             "30F
  else.                                                     "30F
    fnam_konto = 'RF05A-NEWKO'.                             "30F
  endif.                                                    "30F

*-------- Cursor setzen, wg. MatchCode eingabe -------------------------
  clear ft.
  ft-fnam = 'BDC_CURSOR'.
  ft-fval = fnam_konto.                                     "30F
* IF REP_NAME = REP_NAME_BV.                              "30F
*   Bei Belegvorerfassung                                 "30F
*   FT-FVAL = 'RF05V-NEWKO'.                              "30F
* ELSE.                                                   "30F
*   FT-FVAL = 'RF05A-NEWKO'.                              "30F
* ENDIF.                                                  "30F
  append ft.
  index = index + 1.


*------- Ausgleichsvorgang im Loop ankreuzen (bei FB05 etc.) -----------
  if auglv ne space.
    clear ft.
    ft-fnam(12)   = 'RF05A-XPOS1('.
    ft-fnam+12(2) = tabix_041a.
    ft-fnam+14(1) = ')'.
    condense ft-fnam no-gaps.
    ft-fval       = 'X'.
    append ft.
    index = index + 1.
  endif.
endform.                    "dynpro_setzen_einstieg

*eject
*-----------------------------------------------------------------------
*        Form  FCODE_ENTER
*-----------------------------------------------------------------------
*        Fcode /00 setzen um zu verhindern, dass ein leeres
*        Kontierungsblock übersprungen wird. Es hat zum fehlerhaften
*        Einmischen der Daten der nächsten Position in die aktuellen
*        Daten.
*-----------------------------------------------------------------------
form fcode_enter.
  clear ft.
  ft-fnam = 'BDC_OKCODE'.
  ft-fval = '/00'.
  append ft.
endform.                    "fcode_enter

*eject
*-----------------------------------------------------------------------
*        Form  FCODE_F05
*-----------------------------------------------------------------------
*        Fcode /05 für 'Batch-Input Selektion' übergeben
*        Dynpro 733 setzen
*-----------------------------------------------------------------------
form fcode_f05.
*------- falls keine Selektionsdaten Dynpro 733 nicht senden -----------
  check not xftclear-selfd is initial.

*------- Absprung auf Dynpro 733 --------------------------------------
  clear ft.
  ft-fnam = 'BDC_OKCODE'.
  ft-fval = '/05'.
  append ft.

  clear ft.
  ft-program  = rep_name_a.
  ft-dynpro   = '0733'.
  ft-dynbegin = 'X'.
  append ft.
  dynnr = '0733'.
endform.                                                    "fcode_f05

*eject
*-----------------------------------------------------------------------
*        Form  fcode_f06_f07
*-----------------------------------------------------------------------
*        Fcode /06 für 'OP auswählen' bzw. 'Anderes Konto' übergeben
*        Dynpro 710 setzen
*-----------------------------------------------------------------------
form fcode_f06_f07.
  if dynnr ne '0710'.
    clear ft.
    ft-fnam = 'BDC_OKCODE'.

    if dynnr = '0733'.
      ft-fval = '/07'.
      append ft.
    else.
      ft-fval = '/06'.
      index = index + 1.
      insert ft index index.
    endif.

    clear ft.
    ft-program  = rep_name_a.
    ft-dynpro   = '0710'.
    ft-dynbegin = 'X'.
    append ft.

    dynnr = '0710'.
    clear loopc.
  endif.
endform.                    "fcode_f06_f07

*eject
*-----------------------------------------------------------------------
*        Form  FCODE_F07
*-----------------------------------------------------------------------
*        Fcode /07 für 'Zusatzdaten' übergeben
*-----------------------------------------------------------------------
form fcode_f07.
  clear ft.
  ft-fnam = 'BDC_OKCODE'.
  ft-fval = '/07'.
  append ft.
endform.                                                    "fcode_f07

*eject
*-----------------------------------------------------------------------
*        Form  FCODE_F11
*-----------------------------------------------------------------------
*        Fcode /11 für 'Sichern' auf dem richtigen Dynpro setzen
*-----------------------------------------------------------------------
form fcode_f11.

  if tfill_ftvv ne 0.
*   Dynpro für Vermögensverwaltung wurde gesendet.
*   Rücksprung auf das vorige Standad-Dynpro
    clear ft.
    ft-program  = rep_name.
    ft-dynpro   = dynnr.
    ft-dynbegin = 'X'.
    append ft.
    if dynnr = 300 or dynnr = 305.
      perform leeres_cobl_to_ft.
    endif.
  endif.
* Steuerkurs auch bei Steuerrechnen übernehmen            "N960639
  IF tfill_xfttax NE 0. " AND xmwst NE 'X'.               "N960639
    perform tax_dynpro.
* begin of note 1023317
    LOOP AT ft WHERE fnam CS 'VATDATE'.
      APPEND ft.
      EXIT.
    ENDLOOP.
    IF sy-subrc IS INITIAL.
      LOOP AT ft WHERE fnam CS 'VATDATE'.
        DELETE ft.
        EXIT.
      ENDLOOP.
    ENDIF.
* end of note 1023317
    DESCRIBE TABLE ft LINES index.
  endif.

* PF11- Sichern
  clear ft.
  ft-fnam = 'BDC_OKCODE'.
  ft-fval = '/11'.
  index = index + 1.
  insert ft index index.

endform.                                                    "fcode_f11

*eject
*-----------------------------------------------------------------------
*        FORM TAX_DYNPRO.
*-----------------------------------------------------------------------
*        Steuern-Dynpro
*-----------------------------------------------------------------------
form tax_dynpro.
* Funktionscode setzten
* CLEAR FT.                                                 "STEG
* FT-FNAM     = 'BDC_OKCODE'.                               "STEG
* FT-FVAL     = 'STEB'.                                     "STEG
* INDEX = INDEX + 1.                                        "STEG
* INSERT FT INDEX INDEX.                                    "STEG

  select single * from t001 where bukrs = bkpf_bukrs.      "Note 641889
  if sy-subrc ne 0.
    message a016 with bukrs.
  endif.
  select single * from t005 where land1 = t001-land1.
  if sy-subrc ne 0.
    message a017 with t001-land1.
  endif.

*-------- XFTTAX komprimieren vor Aufruf der Tax-Dynpros----------------
  perform xfttax_komprimieren.

  select single * from ttxd where kalsm = t005-kalsm.
  if sy-subrc = 0.
    perform tax_dynpro_us.
  else.
    perform tax_dynpro_not_us.
  endif.
endform.                    "tax_dynpro


*eject
*-----------------------------------------------------------------------
*        FORM TAX_DYNPRO_US.
*-----------------------------------------------------------------------
*        Steuern-Dynpro für USA
*-----------------------------------------------------------------------
form tax_dynpro_us.
  data: tax_screen     like tstc-dypno." Screen Number      "STEG
  perform find_us_tax_screen_number using tax_screen.           "STEG
  clear ft.
  ft-program  = rep_name_t.
  ft-dynpro   = tax_screen.            "STEG
  ft-dynbegin = 'X'.
  append ft.
  if tax_screen = '0450'.              "STEG
    perform set_function_code_steb.    "STEG
    clear loopc.
    loop at xfttax.
      loopc = loopc + 1.

      if loopc = 1.
      perform tax_exchange_rate.                           " Note 564235
        if xfttax-bschl is initial.                        "N960639
          exit.                                            "N960639
        endif.                                             "N960639
      ENDIF.

      if loopc > 16.
        defsize = 'X'.             "N849676 set default screen size
        clear ft.
        ft-fnam     = 'BDC_OKCODE'.
        ft-fval     = 'P+'.
        append ft.

        clear ft.
        ft-program  = rep_name_t.
        ft-dynpro   = tax_screen.
        ft-dynbegin = 'X'.
        append ft.

        loopc = 1.
      endif.

      perform append_taxline_to_ft using 'BSET-FWSTE'  xfttax-fwste.
      perform append_taxline_to_ft using 'BSET-MWSKZ'  xfttax-mwskz.
      perform append_taxline_to_ft using 'BSEG-BSCHL'  xfttax-bschl.
      perform append_taxline_to_ft using 'BSET-TXJCD'  xfttax-txjcd.
      perform append_taxline_to_ft using 'BSET-KSCHL'  xfttax-kschl.
    endloop.
  else.                                "STEG
    perform set_function_code_steg.    "STEG
    clear loopc.
    loop at xfttax.
      loopc = loopc + 1.
      if loopc > 15.
        defsize = 'X'.             "N849676 set default screen size
        clear ft.
        ft-fnam     = 'BDC_OKCODE'.
        ft-fval     = 'P+'.
        append ft.

        clear ft.
        ft-program  = rep_name_t.
        ft-dynpro   = tax_screen.
        ft-dynbegin = 'X'.
        append ft.

        loopc = 1.
      endif.

      perform append_taxline_to_ft using 'BSET-FWSTE'  xfttax-fwste.
      perform append_taxline_to_ft using 'BSET-KSCHL'  xfttax-kschl.
    endloop.
  endif.                               "STEG

endform.                    "tax_dynpro_us


*eject
*-----------------------------------------------------------------------
*        FORM TAX_DYNPRO_NOT_US.
*-----------------------------------------------------------------------
*        Steuern-Dynpro für alle Länder außer USA
*-----------------------------------------------------------------------
form tax_dynpro_not_us.

  perform set_function_code_steb.      "STEG
  clear ft.
  ft-program  = rep_name_t.
  ft-dynpro   = '0300'.
  ft-dynbegin = 'X'.
  append ft.

  sort xfttax by mwskz bschl.
  clear loopc.
  loop at xfttax.
    loopc = loopc + 1.

    if loopc = 1.
      perform tax_exchange_rate.                           " Note 564235
      if xfttax-bschl is initial.                          "N960639
        exit.                                              "N960639
      endif.                                               "N960639
    ENDIF.

    if loopc > 15.
      defsize = 'X'.             "N849676 set default screen size
      clear ft.
      ft-fnam     = 'BDC_OKCODE'.
      ft-fval     = 'P+'.
      append ft.

      clear ft.
      ft-program  = rep_name_t.
      ft-dynpro   = '0300'.
      ft-dynbegin = 'X'.
      append ft.

      loopc = 1.
    endif.
    perform append_taxline_to_ft using 'BSET-FWSTE'  xfttax-fwste.
    perform append_taxline_to_ft using 'BSET-MWSKZ'  xfttax-mwskz.
    perform append_taxline_to_ft using 'BSEG-BSCHL'  xfttax-bschl.
    perform append_taxline_to_ft using 'BSET-TXJCD'  xfttax-txjcd.
    perform append_taxline_to_ft using 'BSET-HWSTE'  xfttax-hwste.
  endloop.

endform.                    "tax_dynpro_not_us

*---------------------------------------------------------------------*
*       FORM APPEND_TAXLINE_TO_FT                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FIELDNAME                                                     *
*  -->  FIELDVALUE                                                    *
*---------------------------------------------------------------------*
form append_taxline_to_ft using fieldname fieldvalue.       " XBETRAG.
  check not fieldvalue is initial.

  clear ft.
  ft-fnam(14)    = fieldname.

  ft-fnam+14(01) = '('.
  ft-fnam+15(02) = loopc.
  ft-fnam+17(01) = ')'.

  condense ft-fnam no-gaps.
  ft-fval = fieldvalue.
  append ft.

endform.                    "append_taxline_to_ft


*---------------------------------------------------------------------*
*       FORM FCODE_F11_OLD                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form fcode_f11_old.
  if dynnr = 300.
    perform leeres_cobl_to_ft.
  endif.
  if tfill_ftvv eq 0.
    if tfill_xfttax eq 0.
      clear ft.
      ft-fnam = 'BDC_OKCODE'.
      ft-fval = '/11'.
      index = index + 1.
      insert ft index index.
    endif.

  else.
*   Dynpro für Vermögensverwaltung wurde gesendet.
*   Rücksprung auf das vorige Standad-Dynpro und dann /11
    clear ft.
    ft-program  = rep_name.
    ft-dynpro   = dynnr.
    ft-dynbegin = 'X'.
    append ft.
    clear ft.
    ft-fnam = 'BDC_OKCODE'.
    ft-fval = '/11'.
    append ft.
  endif.

endform.                    "fcode_f11_old

*eject
*-----------------------------------------------------------------------
*        FORM  LEERES_COBL_TO_FT.
*-----------------------------------------------------------------------
*        leeres Kontierungsblock-Dynpro senden
*-----------------------------------------------------------------------
form leeres_cobl_to_ft.
  clear ft.
  ft-program  = rep_name_k.
  ft-dynpro   = '0002'.
  ft-dynbegin = 'X'.
  append ft.
endform.                    "leeres_cobl_to_ft

*eject
*-----------------------------------------------------------------------
*        Form  INIT_POSTING
*-----------------------------------------------------------------------
*        Belegdaten initialisieren
*-----------------------------------------------------------------------
form init_posting.
  clear:   umskz.
  clear:   bukrs, waers, xmwst, xmwst_set, send_ok17.
  clear:   blart, budat, budat_int.

  clear:   ft, fta, ftc, ftf, ftz, ftk, ftvk, ftvv, xftpost, xfttax.
  clear:   ftps.                                            "30F
  clear:   ftab.                                              "KJV
  clear:   ftw.
  clear:   ftsplt.
  clear:   ftsplt_wt.
  clear:   ftcopa.                                          "1414479
  refresh: ft, fta, ftc, ftf, ftz, ftk, ftvk, ftvv, xftpost, xfttax.
  refresh: ftfkto, ftps.                                    "30F
  refresh: ftw.
  refresh: ftab.                                             "KJV
  refresh: ftsplt.
  refresh: ftsplt_wt.
  refresh: ftcopa.                                          "1414479
endform.                    "init_posting

*eject
*-----------------------------------------------------------------------
*        Form  MAPPE_ABSPIELEN_IM_BATCH.
*-----------------------------------------------------------------------
form mappe_abspielen_im_batch.
  get run time field runtime.
  jobname    = 'RSBDCSUB-FIPI'.
  jobname+14 = runtime.

  call function 'JOB_OPEN'
    exporting
      jobname          = jobname
      jobgroup         = 'FIPI'
    importing
      jobcount         = jobcount
    exceptions
      cant_create_job  = 01
      invalid_job_data = 02
      jobname_missing  = 03.

  if sy-subrc ne 0.
    message e015  raising session_not_processable.
  endif.


  submit rsbdcsub and return
                  user sy-uname
                  via job jobname number jobcount
                  with queue_id =  queue_id
                  with z_verarb =  'X'.


  call function 'JOB_CLOSE'
    exporting
      jobname              = jobname
      jobcount             = jobcount
      strtimmed            = bdcimmed
      sdlstrtdt            = bdcstrtdt
      sdlstrttm            = bdcstrttm
    exceptions
      cant_start_immediate = 01
      jobname_missing      = 02
      job_close_failed     = 03
      job_nosteps          = 04
      job_notex            = 05
      lock_failed          = 06
      invalid_startdate    = 07
      others               = 99.

  if sy-subrc ne 0.
    message e015  raising session_not_processable.
  endif.

  clear  bdcimmed.
  bdcstrtdt = space.
  bdcstrttm = space.
endform.                    "mappe_abspielen_im_batch

*eject
*-----------------------------------------------------------------------
*        Form  MAPPE_OEFFNEN
*-----------------------------------------------------------------------
*        Öffnen der BDC-Queue für Datentransfer
*-----------------------------------------------------------------------
form mappe_oeffnen.
  clear queue_id.
  call function 'BDC_OPEN_GROUP'
    exporting
      client   = mandt
      group    = group
      holddate = holdd
      keep     = xkeep
      user     = usnam
    importing
      qid      = queue_id.
  group_open = 'X'.
endform.                    "mappe_oeffnen

*eject
*-----------------------------------------------------------------------
*        Form  MAPPE_SCHLIESSEN
*-----------------------------------------------------------------------
form mappe_schliessen.
  if group_open = 'X'.
    call function 'BDC_CLOSE_GROUP'.
    clear group_open.
  endif.
endform.                    "mappe_schliessen

*eject
*-----------------------------------------------------------------------
*        Form  POSITION_UEBERTRAGEN
*-----------------------------------------------------------------------
*        Gruppenwechsel bei Belegposition:
*        Die in den internen Tabellen FTA, FTF, etc. gesammelten
*        Daten einer Belegposition werden in der richtigen
*        Reihenfolge in Tabelle FT übertragen.
*        Vor der Übertragung erfolgt die Dynproermittlung.
*-----------------------------------------------------------------------
form position_uebertragen.
  data: h_wt_acco like with_item-wt_acco.
  data :h_lfb1 like lfb1 OCCURS 1 WITH HEADER LINE.         "878993
  data :h_knb1 like knb1 OCCURS 1 WITH HEADER LINE.         "878993
  data :l_lifnr type lifnr.                                 "878993
  data :l_kunnr type kunnr.                                 "878993
  DATA: cmp_str  TYPE string.                               "870828
  cmp_str = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                   "870828

* prüfen, ob konkurrierende Felder für NEWKO vorhanden.
  describe table ftfkto lines tfill_ftfkto.
  if tfill_ftfkto = 1.
*   alles klar FTF ergänzen
    loop at ftfkto.
      ftfkto-fnam = fnam_konto.                             "30F
      clear ftf.
      ftf = ftfkto.
      append ftf.
      konto = ftf-fval.
    endloop.
  elseif tfill_ftfkto = 2.
*   der erste Treffer wird als Mitbuchkonto angesehen
*   danach darf nur noch genau ein Konto da sein, das ungleich
*    BSEG-HKONT sein muß
    loop at ftfkto where fnam = 'BSEG-HKONT'.
      ftfkto_indx = sy-tabix.
      exit.
    endloop.
    if sy-subrc = 0.
      clear fta.
      fta = ftfkto.
      append fta.
      delete ftfkto index ftfkto_indx.
      loop at ftfkto.
        if ftfkto-fnam = 'BSEG-HKONT'.
          message e019.
        else.
          ftfkto-fnam = fnam_konto.                         "30F
          clear ftf.
          ftf = ftfkto.
          append ftf.
          konto = ftf-fval.
        endif.
      endloop.
    else.
      message e020.
    endif.
  elseif tfill_ftfkto > 2.
    message e021.
  elseif tfill_ftfkto = 0.
    exit.
  endif.
  refresh ftfkto.

*------- Fußzeile vollständig? -----------------------------------------
  if bukrs = space.
    message e012 raising company_code_missing.
  endif.

  if bschl = space.
    message e013 with xftpost-count raising posting_key_missing.
  endif.
  if konto = space.
    message e014 with xftpost-count raising account_missing.
  endif.


*------- Fußzeiledaten übertragen --------------------------------------
  loop at ftf.
    ft = ftf.
    index = index + 1.
    insert ft index index.
  endloop.

*------- Übertragen einer abweichenden Zentrale ------------------------
  if konto(1) = '+'.
    perform abweichende_zentrale.
  endif.

*------- nächste DYNNR ermitteln (Standardbild + Zusatzdaten) ----------
  perform dynpro_ermitteln.

*------- PublicSector-Daten bei Debitor/Kreditor auf Zusatz-Daten   ----
*-------                    bei Anlagen auf Haupt-Dynpro            ----
  loop at ftps.                                             "30F
    if dynnr = '0303' or dynnr = '0304' or
      dynnr = '0305' or dynnr = '0312'.                     "Note885510
      fta = ftps.                                           "30F
      append fta.                                           "30F
    else.                                                   "30F
      ftz = ftps.                                           "30F
      append ftz.                                           "30F
    endif.                                                  "30F
  endloop.                                                  "30F

*------ Falls es sich um das Dynpro "Belegposition Kreditor handelt,
*------ muessen die Felder BSEG-VERTN und BSEG-VERTT auf das
*------ Zusatzdaten-Dynpro

  if dynnr = '0302'.
    loop at fta.
      if fta-fnam = 'BSEG-VERTT' or
         fta-fnam = 'BSEG-VERTN' or
         fta-fnam = 'BSEG-VBEWA'.
        ftz = fta.
        append ftz.
        delete fta.
      endif.
    endloop.
  endif.

*------ Falls es sich um das Dynpro "Direkte Steuerbuchung" handelt,
*------ müssen die Felder FISTL,FIPOS,GRANT_NBR,FKBER,GEBER  aus
*------ dem Kontierungsblock auf das allgemeine Dynpro
*------ Tabelle ist dann von COBL auf BSEG umzusetzen
*------ Felder KOSTL,AUFNR,PS_PSP_PNR,RECID   sind auf Joint Venture
*------ Subscreen SAPLGJTS 0001 dort aber als COBL-Felder und müssen
*------ daher nicht auf BSEG umgesetzt werden
*------ Neu mit Hinweis 1137272
  if dynnr = '0312'.
    loop at ftk.
      if ftk-fnam = 'COBL-FISTL'          or
         ftk-fnam = 'COBL-FIPOS'          or
         ftk-fnam = 'COBL-GRANT_NBR'      or
         ftk-fnam = 'COBL-GEBER'          or
         ftk-fnam = 'COBL-GSBER'          or            "Note1630382
         ftk-fnam = 'COBL-BUDGET_PD'.                   "Note1630382
         fta = ftk.
         fta-fnam(4) = 'BSEG'.
         append fta.
         delete ftk.
      endif.
      if ftk-fnam = 'COBL-KOSTL'          or
         ftk-fnam = 'COBL-AUFNR'          or
         ftk-fnam = 'COBL-PS_PSP_PNR'     or
         ftk-fnam = 'COBL-RECID'.
         fta = ftk.
         append fta.
         delete ftk.
      endif.
      if ftk-fnam = 'COBL-FKBER'.
         fta = ftk.
         fta-fnam = 'BSEG-FKBER_LONG'.
         append fta.
         delete ftk.
      endif.
    endloop.
  endif.


*------ ALC Payment supplement on cash relevant gl items on detail
*------ screen 330
  if dynnr = '0300'.
    loop at fta where fnam = 'BSEG-UZAWE'.
      ftz = fta.
      append ftz.
      delete fta.
      exit.
    endloop.
    loop at fta where fnam = 'BSEG-KIDNO'.
       ftz = fta.
       append ftz.
       delete fta.
       exit.
    endloop.
  endif.
*------ Auskommentiert, da ab 4.0C das Feld nicht mehr auf dem Dynpro
*------ SAPMF05A/0305 vorhanden ist  ---> Kontierungsblock.
*------ Bei Anlagen (Dynpro 305) kann man auf dem Hauptbild auch
*------ das Kontierungsblockfeld COBL-PRCTR eingeben.
* LOOP AT FTK WHERE FNAM = 'COBL-PRCTR'.
*   IF DYNNR = '0305' AND REP_NAME = REP_NAME_A.
*     REPLACE 'COBL' WITH 'BSEG' INTO FTK-FNAM.
*     FTA = FTK.
*     APPEND FTA.
*     DELETE FTK.
*   ENDIF.
* ENDLOOP.

*------- Daten für Sonder-Dynpros vorhanden? ---------------------------
  describe table ftc       lines  tfill_ftc.
  describe table ftiban    lines  tfill_ftiban.
  describe table ftk       lines  tfill_ftk.
  describe table ftz       lines  tfill_ftz.
  describe table ftvv      lines  tfill_ftvv.
  describe table ftvk      lines  tfill_ftvk.
  describe table ftcopa    lines  tfill_ftcopa.
  describe table ftisis    lines  tfill_ftisis.
  describe table ft_generic_kontl lines  tfill_ft_generic_kontl.
  describe table ftab      lines  tfill_ftab  .                   "KJV
*---------------------------------------------- extended withholding tax
  describe table ftw       lines  tfill_ftw.
  describe table ftsplt    lines  tfill_ftsplt.

  perform wechseldaten_frankreich.

*------- CpD-Daten senden? ---------------------------------------------
  if  tfill_ftc   ne  0
  and regul       ne 'X'.
    perform dynpro_senden_ftc.
*------- IBAN-Daten senden?
    if tfill_ftiban ne 0.
      perform iban_data.
    endif.
  endif.

*------ Vermögensverwaltung Vordynpro vor Anlage -----------------------
  INCLUDE ifre_begin_of_re_classic.
  if  xtbsl-koart = 'A'
  and tfill_ftvk  > 0.
    perform dynpro_senden_vv_ber_bestand.
  endif.
  INCLUDE ifre_end_of_re_classic.

*------- Standardbild senden (+F07) ------------------------------------
  perform dynpro_senden_fta using dynnr.
  if tfill_ftz ne 0.                                        "i
    perform fcode_f07.                                      "i
  endif.                                                    "i
*---------------------------------------------- extended withholding tax
* send popup for withholding tax data
  if tcode ne 'FB05'                                       "note 526316
     and ( xtbsl-koart eq 'K' or xtbsl-koart eq 'D' ).      "642392
*  if tfill_ftw ne 0.
    call function 'FI_CHECK_EXTENDED_WT'
      exporting
        i_bukrs              = bukrs
      exceptions
        component_not_active = 1
        not_found            = 2
        others               = 3.
    if sy-subrc = 0.
*   h_wt_acco = konto(10).                                  "508747
    IF xtbsl-koart = 'K'.                          "start of 878993
      l_lifnr = KONTO.
      CALL FUNCTION 'FI_WT_READ_LFB1'
        EXPORTING
          I_LIFNR         = l_lifnr
          I_BUKRS         = BUKRS
        TABLES
          T_LFB1          = h_lfb1
       EXCEPTIONS
         NOT_FOUND       = 1
         OTHERS          = 2.
      IF SY-SUBRC = 0.
        READ TABLE h_lfb1 WITH KEY LIFNR = h_lfb1-lifnr
                                   BUKRS = bukrs.
        IF NOT h_lfb1-lnrze IS INITIAL.
          KONTO = h_lfb1-lnrze.
        ENDIF.
      ENDIF.
    ELSEIF xtbsl-koart = 'D' .
       l_kunnr = KONTO.
       CALL FUNCTION 'FI_WT_READ_KNB1'
         EXPORTING
           I_KUNNR         = l_kunnr
           I_BUKRS         = BUKRS
         TABLES
           T_KNB1          = h_knb1
        EXCEPTIONS
          NOT_FOUND       = 1
          OTHERS          = 2.
       IF SY-SUBRC = 0.
         READ TABLE h_knb1 WITH KEY KUNNR = h_knb1-kunnr
                                    BUKRS = bukrs.
         IF NOT h_knb1-knrze IS INITIAL.
           KONTO = h_knb1-knrze.
         ENDIF.
       ENDIF.
     ENDIF.                                           "end of 878993

      if konto NA cmp_str.                                  "870828
        SHIFT konto LEFT DELETING LEADING '0'.              "642392
      endif.                                                "870828

      if konto(1) = '='.                                    "673286
        if konto ca '.'.                                    "673286
          shift konto up to '.'.                            "673286
        endif.                                              "673286
        shift konto left.                                   "673286
      endif.                                                "673286

      call function 'CONVERSION_EXIT_ALPHA_INPUT'           "508747
        exporting                                           "508747
          input         = konto(10)           "673286 "624989 "508747
        importing                                           "508747
          output        = h_wt_acco.                        "508747
      translate h_wt_acco to upper case.                 "#EC TRANSLANG
      call function 'FI_WT_FIPI_FILL_FT_TAB'
           exporting
                i_dynnr            = dynnr
                i_index            = index
                i_koart            = xtbsl-koart
                i_wt_acco          = h_wt_acco
                i_bukrs            = bukrs
                i_budat            = budat_wt
                i_bschl            = xtbsl-bschl
                i_umskz            = umskz
*       IMPORTING
*         e_index            = index
           tables
                i_ftw              = ftw
                i_ft               = ft
           exceptions
                dynpro_not_correct = 1
                others             = 2.
*
*--- Dynpro setzen für Fußzeile
*         clear ft.
*         ft-program  = rep_name.
*         ft-dynpro   = dynnr.
*         ft-dynbegin = 'X'.
*         append ft.
*         if dynnr = '0300' or
*            dynnr = '0301' or
*            dynnr = '0302' or
*            dynnr = '0312'.
*            clear ft.
*            ft-fnam = 'BDC_CURSOR'.
*            ft-fval = fnam_konto.
*            append ft.
*         endif.
*         describe table ft lines index.
*
*    endif.
    endif.
 endif.                                                     "note 526316
*  if tfill_ftz ne 0.
*    perform fcode_f07.
*  endif.

*------- Popup für Betragssplitt (FTSPLT)-------------------------------
  if tfill_ftsplt ne 0.
    call function 'AC_APAR_SPLIT_FILL_FT'
      exporting
        i_dynnr       = dynnr
      tables
        t_ft_split    = ftsplt
        t_ft          = ft
        t_ft_split_wt = ftsplt_wt.

  endif.

*------- Kontierungsblock-Dynpro senden, falls Sachkontenbild ----------
  if dynnr = 300 or dynnr = 305 OR dynnr = 312.             "Note1137272
    perform dynpro_senden_ftk.

*------- Send all additional screens here:

* Profitability analysis (CO-PA) screen
    perform copa_daten.
* Insurance screen (IS Insurance)
    perform isis_daten.
* Also Insurance screen, maybe screen from other industry solutions
    perform generic_kontl_data.      " field KONTL filled ?
  endif.


*------- Kontierungblock Full Screen von VV, wenn Anzahlung -----------
  if tfill_ftvk > 0
  and (    xtbsl-koart = 'D'
       or  xtbsl-koart = 'K' ).
    perform dynpro_senden_vv_anzahlungen.
  endif.

*------- Daten für abweich. Zahlungsempf. senden ? ---------------------
  if  tfill_ftc   ne  0
  and regul       eq 'X'.
    perform dynpro_senden_ftc.
*------- IBAN-Daten senden?
    if tfill_ftiban ne 0.
      perform iban_data.
    endif.
  endif.

*------- Daten für Anlagen anteilige Ab/Zuschreibungen--------"KJV------
  if  tfill_ftab  ne  0.                                      "KJV
    perform dynpro_senden_ftab.                               "KJV
  endif.                                                      "KJV


*------- Zusatzbild senden ? -------------------------------------------
  if tfill_ftz ne 0.
    perform dynpro_senden_ftz using winnr.
    if tfill_ftab ne 0.                                        "KJV
      perform dynpro_senden_ftab.                              "KJV
    endif.                                                     "KJV
  endif.

*------- Vermögensverwaltungsbild senden ? -----------------------------
  if  tfill_ftvv ne 0
  and tfill_ftz  eq 0
  and tfill_ftc  eq 0.
    perform dynpro_senden_ftvv.
  endif.

*------- Positionsdaten initialisieren ---------------------------------
  refresh: fta, ftc, ftf, ftz, ftk, ftvk, ftvv, ftcopa, ftiban.
  refresh: ftisis, ftps, ftw, ftab, ftsplt, ftsplt_wt, ft_generic_kontl.

  clear:   anbwa, bschl, konto, umskz, regul, knrze.
  clear:   tfill_ftc, tfill_ftz, tfill_ftcopa, tfill_ft_generic_kontl.
  clear:   tfill_ftisis, tfill_ftw, tfill_ftab, ftsplt, ftsplt_wt.
  clear:   tfill_ftiban.

endform.                    "position_uebertragen


**eject
**-----------------------------------------------------------------------
**        Form  RESET_CLEARING.
**-----------------------------------------------------------------------
**        Rücknahme eines Ausgleichs
**-----------------------------------------------------------------------
*form reset_clearing using p_no_auth type c.
*  clear ft.
*  ft-program  = rep_name_r.
*  ft-dynpro   = '0100'.
*  ft-dynbegin = 'X'.
*  append ft.
*  if not augbl is initial.
*    clear ft.
*    ft-fnam     = 'RF05R-AUGBL'.
*    ft-fval     = augbl.
*    append ft.
*  endif.
*  if not bukrs is initial.
*    clear ft.
*    ft-fnam     = 'RF05R-BUKRS'.
*    ft-fval     = bukrs.
*    append ft.
*  endif.
*  if not gjahr is initial.
*    clear ft.
*    ft-fnam     = 'RF05R-GJAHR'.
*    ft-fval     = gjahr.
*    append ft.
*  endif.
*
*  clear ft.
*  ft-fnam     = 'BDC_OKCODE'.
*  ft-fval     = '/11'.
*  append ft.
*
**  perform transaktion_beenden using p_no_auth e_message.
*
*endform.                    "reset_clearing
*
*
**eject
**-----------------------------------------------------------------------
**        Form  REVERSE_DOCUMENT.
**-----------------------------------------------------------------------
**        Beleg stornieren
**-----------------------------------------------------------------------
*form reverse_document using p_no_auth.
*  clear ft.
*  ft-program  = rep_name_a.
*  ft-dynpro   = '0105'.
*  ft-dynbegin = 'X'.
*  append ft.
*  if not belns is initial.
*    clear ft.
*    ft-fnam     = 'RF05A-BELNS'.
*    ft-fval     = belns.
*    append ft.
*  endif.
*  if not bukrs is initial.
*    clear ft.
*    ft-fnam     = 'BKPF-BUKRS'.
*    ft-fval     = bukrs.
*    append ft.
*  endif.
*  if not gjahs is initial.
*    clear ft.
*    ft-fnam     = 'RF05A-GJAHS'.
*    ft-fval     = gjahs.
*    append ft.
*  endif.
*  if not budat is initial.
*    clear ft.
*    ft-fnam     = 'BSIS-BUDAT'.
*    ft-fval     = budat.
*    append ft.
*  endif.
*  if not monat is initial.
*    clear ft.
*    ft-fnam     = 'BSIS-MONAT'.
*    ft-fval     = monat.
*    append ft.
*  endif.
*
*  if not stgrd is initial.
*    clear ft.
*    ft-fnam     = 'UF05A-STGRD'.
*    ft-fval     = stgrd.
*    append ft.
*  endif.
*
*  if not voidr is initial.
*    clear ft.
*    ft-fnam     = 'RF05A-VOIDR'.
*    ft-fval     = voidr.
*    append ft.
*  endif.
*
*  clear ft.
*  ft-fnam     = 'BDC_OKCODE'.
*  ft-fval     = '/11'.
*  append ft.
*
*  perform transaktion_beenden using p_no_auth.
*endform.                    "reverse_document


*eject
*-----------------------------------------------------------------------
*        Form  TRANSAKTION_BEENDEN
*-----------------------------------------------------------------------
form transaktion_beenden using p_no_auth type c
                  CHANGING e_msg type BAPI_MSG .
  data: local_funct like rfipi-funct,
        tab_msg like bdcmsgcoll occurs 0 with header line,
        _subrc like sy-subrc,
        no_auth type c.
  data: wa_opt type ctu_params.                             "N811562

  if sgfunct is initial.
    local_funct = funct.
  else.
    local_funct = sgfunct.
  endif.

  case local_funct.
*------- Funktion: Batch-Input -----------------------------------------
    when 'B'.
      call function 'BDC_INSERT'
        exporting
          tcode     = tcode
        tables
          dynprotab = ft.

*------- Funktion: Call Transaction ... Using ... ----------------------
    when 'C'.
      refresh xbltab.
      clear xbltab.
      export xbltab to memory id 'FI_XBLTAB'.

      if p_no_auth = space.
        call function 'AUTHORITY_CHECK_TCODE'
          exporting
            tcode  = tcode
          exceptions
            ok     = 0
            not_ok = 1
            others = 2.
      endif.
      if sy-subrc ne 0.
        if xbdcc ne 'X'.
          message e172(00) with tcode raising no_authorization.
        else.
          message s172(00) with tcode.
        endif.
        no_auth = 'X'.
      else.

* Bei IDOC Verarbeitung muss Sperre von der Datenbank genommen
* werden
        call function 'IDOC_INVOIC_UNLOCK'.
        refresh tab_msg.
        wa_opt-dismode = mode.                              "N811562
        wa_opt-updmode = update.                            "N811562
        wa_opt-defsize = defsize.                           "N849676

        call transaction tcode using  ft
                             options from wa_opt            "N811562
                             messages into tab_msg.
      endif.

      data : l_msg type string .

      clear : e_msg .
      loop at tab_msg .
        clear : l_msg .
        CALL FUNCTION 'FORMAT_MESSAGE'
         EXPORTING
           ID              = tab_msg-MSGID
           LANG            = sy-langu
           NO              = tab_msg-MSGNR
           V1              = tab_msg-MSGV1
           V2              = tab_msg-MSGV2
           V3              = tab_msg-MSGV3
           V4              = tab_msg-MSGV4
         IMPORTING
           MSG             = l_msg
         EXCEPTIONS
           NOT_FOUND       = 1
           OTHERS          = 2
                  .
        CONCATENATE e_msg l_msg ';' into e_msg .
      ENDLOOP .

      subrc = sy-subrc.
      if no_auth eq 'X'.
        msgty = 'E'.
      else.
        msgty = sy-msgty.
      endif.
      msgid = sy-msgid.
      msgno = sy-msgno.
      msgv1 = sy-msgv1.
      msgv2 = sy-msgv2.
      msgv3 = sy-msgv3.
      msgv4 = sy-msgv4.

* overwrite msg-fields when error occured
      if sy-subrc ne 0 and no_auth ne 'X'.
        _subrc = sy-subrc.
        read table tab_msg with key msgtyp = 'E'.
        if sy-subrc eq 0.
          msgty = tab_msg-msgtyp.
          msgid = tab_msg-msgid.
          msgno = tab_msg-msgnr.
          msgv1 = tab_msg-msgv1.
          msgv2 = tab_msg-msgv2.
          msgv3 = tab_msg-msgv3.
          msgv4 = tab_msg-msgv4.
        endif.
        sy-subrc = _subrc.
      endif.

      if sy-subrc = 0.
        import xbltab from memory id 'FI_XBLTAB'.
      endif.

*------- bei Fehlern und XBDCC = 'X': Batch Input erzeugen
      if subrc ne 0 and xbdcc = 'X'.
        if group_open ne 'X'.
          perform mappe_oeffnen.
        endif.
        call function 'BDC_INSERT'
          exporting
            tcode     = tcode
          tables
            dynprotab = ft.
        if sy-subrc = 0.
          message s008(fb) with '' group.
        endif.
      endif.


*------- Funktion: Interaktive Buchungsschnittstelle
    when 'I'.
  endcase.
endform.                    "transaktion_beenden

*eject
*-----------------------------------------------------------------------
*        Form  WECHSELDATEN_FRANKREICH
*-----------------------------------------------------------------------
*        Auf dem Wechselbild für Frankreich (2320) werden einige Daten
*        in BSEC-Feldern übergeben. Diese BSEC-Daten müssen nicht
*        auf das CpD-Bild, sondern das Wechselbild übergeben werden.
*-----------------------------------------------------------------------
form wechseldaten_frankreich.
  check tfill_ftc ne 0.

  if xtbsl-xsonu ne space.
    select single * from  t074u
        where  koart       = xtbsl-koart
        and    umskz       = umskz.
  endif.

  if t074u-umsks = 'W'.
    loop at ftc where fnam = 'BSEC-NAME1'.
      exit.
    endloop.
    if sy-subrc ne 0.
      loop at ftc.
        fta = ftc.
        append fta.
      endloop.
      refresh ftc.
      clear tfill_ftc.
    endif.
  endif.
endform.                    "wechseldaten_frankreich

*eject
*-----------------------------------------------------------------------
*        Form  XFTPOST_ANALYSIEREN
*-----------------------------------------------------------------------
*        Buchungsdaten aus FTPOST (Feldname,Feldwert) analysieren:
*        Daten werden aus der Schnittstellentabelle FTPOST in
*        dynprobezogenen Feldtabellen gesammelt.
*        - Kopfdaten:            direkt in Tabelle FT stellen
*        - Fußzeiledaten:        in FTF       sammeln
*        - Fußzeiledaten (Konto) in FTFKTO    sammeln
*        - Standarddynpro:       in FTA       sammeln
*        - CpD-Daten:            in FTC       sammeln
*        - Zusatzdaten:          in FTZ       sammeln
*        - Kontierungsblockdaten in FTK       sammeln
*        - Vermögensverwaltung   in FTV       sammeln
*        - VV-Kont.Blockdaten    in FTVK      sammeln
*        - COPA-DATEN            in FTCOPA    sammeln
*        - Quellensteuerdaten    in FTW       sammeln
*        - Betragssplitt         in FTSPLT    sammeln
*        - Betragssplitt WT      in FTSPLT_WT sammeln

*-----------------------------------------------------------------------
form xftpost_analysieren.

*------- Daten von ISIS (insurance)
  STATICS: s_vlvz_in_bbseg type c.
  DATA: lv_length type i.

*----- Falls BKPF-XPRFG gefuellt ist, Flag setzen, das sicherstellt,
*      dass okcode /17 gesendet wird.
  if xftpost-fnam   = 'BKPF-XPRFG' and
     xftpost-fval   ne space       and
     tcode          = 'FBV1'.
    send_ok17      = 'X'.
    exit.
  endif.

*------- VBUND eingabe auf Kopfebene-----------------------------------
*        verarbeiten bei AT NEW SYTPE
  if xftpost-fnam    = 'BKPF-VBUND'
  or xftpost-fnam    = 'RF014-VBUND'.
    vbund = xftpost-fval.
    exit.
  endif.

*------- Kopfdaten übertragen (direkt in Tabelle FT stellen) -----------
  if xftpost-fnam(4) = 'BKPF'
  or xftpost-fnam    = 'RF05A-AUGTX'   "neues Feld Ausgl.-text 45A-HP
  or xftpost-fnam    = 'RF05A-PARGB'   "jetzt COBL
  or xftpost-fnam    = 'VBKPF-PARGB'   "Belegvorerfassung
  or xftpost-fnam    = 'VBKPF-XBWAE'   "Belegvorerfassung
  or xftpost-fnam    = 'FS006-DOCID'   "BarCodeübernahme
  or xftpost-fnam    = 'FS006-BARCD'.  "BarCodeübernahme
    if xftpost-fnam  = 'BKPF-XMWST'
    and not xftpost-fval  is initial.
      xmwst = 'X'.
*     XMWST muß in FTA damit es auf nächstes Hauptdynpro muß
      clear fta.
      move-corresponding xftpost to fta.
      append fta.
      exit.
    endif.
    if xftpost-fnam = 'BKPF-BUKRS'.
      bukrs = xftpost-fval.
      bkpf_bukrs = bukrs.                                  "Note 641889
    endif.
    if xftpost-fnam = 'BKPF-WAERS'.
      waers = xftpost-fval.
    endif.
    if xftpost-fnam = 'BKPF-BLART'.
      blart = xftpost-fval.
    endif.
    if xftpost-fnam = 'BKPF-BUDAT'.
      budat = xftpost-fval.
      call function 'CONVERT_DATE_TO_INTERNAL'
        exporting
          date_external            = xftpost-fval
        importing
          date_internal            = budat_wt
        exceptions
          date_external_is_invalid = 1
          others                   = 2.
    endif.
    clear ft.
    move-corresponding xftpost to ft.
    append ft.
    index = index + 1.
    exit.
  endif.

*------- Fußzeiledaten analysieren / merken (FTF) ohne Konto -----------
  if xftpost-fnam = 'BSEG-BSCHL'
  or xftpost-fnam = 'BSEG-UMSKZ'
  or xftpost-fnam = 'BSEG-ANBWA'
  or xftpost-fnam = 'BSEG-BUKRS'

  or xftpost-fnam = 'RF05A-NEWBS'
  or xftpost-fnam = 'RF05A-NEWUM'
  or xftpost-fnam = 'RF05A-NEWBW'
  or xftpost-fnam = 'RF05A-NEWBK'

  or xftpost-fnam = 'RF05V-NEWBS'
  or xftpost-fnam = 'RF05V-NEWUM'
  or xftpost-fnam = 'RF05V-NEWBK'
  or xftpost-fnam = 'RF05V-NEWBW'.                                 .

*------- Fußzeiledaten in Hilfsfeldern speichern / Feldname anpassen ---
    if xftpost-fnam = 'BSEG-BUKRS'
    or xftpost-fnam = 'RF05A-NEWBK'
    or xftpost-fnam = 'RF05V-NEWBK'.
      if rep_name = rep_name_bv.
        xftpost-fnam = 'RF05V-NEWBK'.
      else.
        xftpost-fnam = 'RF05A-NEWBK'.
      endif.
      bukrs       = xftpost-fval.
    endif.

    if xftpost-fnam = 'BSEG-BSCHL'
    or xftpost-fnam = 'RF05A-NEWBS'
    or xftpost-fnam = 'RF05V-NEWBS'.
      if rep_name = rep_name_bv.
        xftpost-fnam = 'RF05V-NEWBS'.
      else.
        xftpost-fnam = 'RF05A-NEWBS'.
      endif.
      bschl       = xftpost-fval.
    endif.


    if xftpost-fnam = 'BSEG-UMSKZ'
    or xftpost-fnam = 'RF05A-NEWUM'
    or xftpost-fnam = 'RF05V-NEWUM'.
      if rep_name = rep_name_bv.
        xftpost-fnam = 'RF05V-NEWUM'.
      else.
        xftpost-fnam = 'RF05A-NEWUM'.
      endif.
      umskz       = xftpost-fval.
    endif.

    if xftpost-fnam = 'BSEG-ANBWA'
    or xftpost-fnam = 'RF05A-NEWBW'
    or xftpost-fnam = 'RF05V-NEWBW'.
      if rep_name = rep_name_bv.
        xftpost-fnam = 'RF05V-NEWBW'.
      else.
        xftpost-fnam = 'RF05A-NEWBW'.
      endif.
      anbwa       = xftpost-fval.
    endif.

    clear ftf.
    move-corresponding xftpost to ftf.
    append ftf.
    exit.

  endif.

*------- Fußzeiledaten analysieren / merken (FTF) nur  Konto ---------
*------- Spezialbehandlung Konto                      -----------------
  if xftpost-fnam = 'BSEG-KONTO'
  or xftpost-fnam = 'BSEG-KUNNR'
  or xftpost-fnam = 'BSEG-LIFNR'
  or xftpost-fnam = 'BSEG-HKONT'
  or xftpost-fnam = 'RF05A-NEWKO'
  or xftpost-fnam = 'RF05V-NEWKO'.
    clear ftfkto.
    move-corresponding xftpost to ftfkto.
    append ftfkto.
    exit.
  endif.

*------- CpD-Daten analysieren / merken (FTC); Feldname anpassen -------
  if xftpost-fnam(4) = 'BSEC'
  or xftpost-fnam    = 'BSEG-STCEG'.
    if xftpost-fnam = 'BSEC-STCEG'.
      xftpost-fnam = 'BSEG-STCEG'.
    endif.
    clear ftc.
    move-corresponding xftpost to ftc.
    append ftc.
    exit.
  endif.

*------- IBAN Daten merken (FTIBAN).

  if xftpost-fnam(5) = 'TIBAN'.
    clear ftiban.
    move-corresponding xftpost to ftiban.
    append ftiban.
    exit.
  endif.

*------- Daten vom Kontierungsblock analysieren / merken (FTK) ---------
  if xftpost-fnam(4) = 'COBL'.
    clear ftk.
    move-corresponding xftpost to ftk.
    append ftk.
    exit.
  endif.

*------- Daten von Vermögensverwaltung analysieren / merken (FTVV, FTVK)
*begin of insertion note 1115584
  if xftpost-fnam(5) = 'LFVI9'
  or xftpost-fnam(21) = 'REIT_TAX_CORRECTION_S'.
*end of insertion note 1115584
*     Felder für Kontierungsblockdynpro der Vermögensverwaltung
*     ( SAPLFVI8 0100) in Tab FTVK sammeln
    clear ftvk.
    move-corresponding xftpost to ftvk.
    append ftvk.
    exit.
  endif.

*------- Daten von COPA merken für die Übergabe an FB
  if xftpost-fnam(9) = 'BSEG-RKE_'.
    clear ftcopa.
    move-corresponding xftpost to ftcopa.
    append ftcopa.
    exit.
  endif.

*------- Daten von ISIS (insurance) merken für die Übergabe an FB
  IF s_vlvz_in_bbseg IS INITIAL.
    PERFORM CHECK_BBSEG_FOR_VZK CHANGING s_vlvz_in_bbseg.
    TRANSLATE s_vlvz_in_bbseg USING ' O'.
  ENDIF.

  IF s_vlvz_in_bbseg = 'X'.
    lv_length = numofchar( xftpost-fnam ) - 5 .
    IF xftpost-fnam(10) = 'BSEG-ISCD_' OR
       ( xftpost-fnam(5) = 'BSEG-' AND xftpost-fnam+lv_length(5) = '_ISCD' ).
    clear ftisis.
    move-corresponding xftpost to ftisis.
    append ftisis.
    exit.
    ENDIF.
  endif.

* ----------------------------------------------------------------------
* Note 499049: Support Posting of parked documents of
* other compoments (IS Insurance etc: ) which
* use field VBSEG-KONTL (KONTT) for a generic storage of their
* accounting assignments.
* ----------------------------------------------------------------------
  if xftpost-fnam = 'BSEG-KONTL' or xftpost-fnam = 'BSEG-KONTT'.
    clear ft_generic_kontl.
    move-corresponding xftpost to ft_generic_kontl.
    append ft_generic_kontl.
    exit.
  endif.

*------- Extended Withholding tax (FTW) --------------------------------
  if xftpost-fnam(4) = 'WITH'.
    call function 'FI_CHECK_EXTENDED_WT'
      exporting
        i_bukrs              = bukrs
      exceptions
        component_not_active = 1
        not_found            = 2
        others               = 3.
    if sy-subrc = 0.
      call function 'FI_WT_FIPI_FILL_FTW_TAB'
        exporting
          i_ftpost = xftpost
        tables
          i_ftw    = ftw
        exceptions
          others   = 0.
      exit.
    else.
      exit.
    endif.
  endif.

*------- Daten für Betragssplitt (FTSPLT) ---------
  if xftpost-fnam(6) = 'ACSPLT'.
    clear ftsplt.
    move-corresponding xftpost to ftsplt.
    append ftsplt.
    exit.
  endif.

  if xftpost-fnam(9) = 'ACWT_ITEM'.
    clear ftsplt_wt.
    move-corresponding xftpost to ftsplt_wt.
    append ftsplt_wt.
    exit.
  endif.

*------- Zusatzdaten analysieren / merken (FTZ) ------------------------
*-----------------------------------------------------------------------
*        Neue Zusatzdaten-Felder müssen hier ebenfalls
*        aufgenommen werden !!!
*-----------------------------------------------------------------------

  if xftpost-fnam    = 'BSEG-DMBE2'
  or xftpost-fnam    = 'BSEG-DMBE3'
  or xftpost-fnam    = 'BSEG-ZOLLT'
  or xftpost-fnam    = 'BSEG-EGRUP'
  or xftpost-fnam    = 'BSEG-BTYPE'
  or xftpost-fnam    = 'BSEG-VNAME'
* OR XFTPOST-FNAM    = 'BSEG-FIPOS'                           "30F
* OR XFTPOST-FNAM    = 'BSEG-GEBER'                           "30F
* OR XFTPOST-FNAM    = 'BSEG-FISTL'                           "30F
  or xftpost-fnam    = 'BSEG-VBUND'
  or xftpost-fnam    = 'BSEG-ABPER'
  or xftpost-fnam    = 'BSEG-GBETR'
  or xftpost-fnam    = 'BSEG-KURSR'
  or xftpost-fnam    = 'BSEG-RSTGR'                         "30F

  or xftpost-fnam    = 'BSEG-MANSP'
  or xftpost-fnam    = 'BSEG-MSCHL'
  or xftpost-fnam    = 'BSEG-HBKID'
  or xftpost-fnam    = 'BSEG-HKTID'                                 "RE
  or xftpost-fnam    = 'BSEG-BVTYP'
  or xftpost-fnam    = 'BSEZ-EGMLD'
  or xftpost-fnam    = '*KNA1-KUNNR'   "Warenempfg. EG-Dreiecksgeschaeft
  or xftpost-fnam    = 'BSEG-EGMLD'
* OR XFTPOST-FNAM    = 'BSEG-EGBLD'
  or xftpost-fnam    = 'BSEG-XEGDR'
  or xftpost-fnam    = 'BSEZ-XEGDR_HU'                     "Note1009677
  or xftpost-fnam    = 'BSEG-ANFBN'
  or xftpost-fnam    = 'BSEG-ANFBU'
  or xftpost-fnam    = 'BSEG-ANFBJ'
  or xftpost-fnam    = 'BSEG-LZBKZ'
  or xftpost-fnam    = 'BSEG-LANDL'
  or xftpost-fnam    = 'BSEG-DIEKZ'
  or xftpost-fnam    = 'BSEG-ZOLLD'
  or xftpost-fnam    = 'BSEG-ZOLLT'
  or xftpost-fnam    = 'BSEG-FDTAG'
  or xftpost-fnam    = 'BSEG-VRSDT'
  or xftpost-fnam    = 'BSEG-FDLEV'
  or xftpost-fnam    = 'BSEG-VRSKZ'
  or xftpost-fnam    = 'BSEG-ZINKZ'
  or xftpost-fnam    = 'BSEG-HZUON'
  or xftpost-fnam    = 'BSEG-XREF1'
  or xftpost-fnam    = 'BSEG-XREF2'
  or xftpost-fnam    = 'BSEG-CCBTC'
  or xftpost-fnam    = 'BSEG-XNEGP'    "4.0
  or xftpost-fnam    = 'BSEG-IDXSP'    "4.0
  or xftpost-fnam    = 'BSEG-KKBER'    "4.0
  or xftpost-fnam    = 'BSEG-XREF3'    "4.0
  or xftpost-fnam    = 'BSEG-DTWS1'    "4.0
  or xftpost-fnam    = 'BSEG-DTWS2'    "4.0
  or xftpost-fnam    = 'BSEG-DTWS3'    "4.0
  or xftpost-fnam    = 'BSEG-DTWS4'    "4.0
  or xftpost-fnam    = 'BSEG-BLNBT'                         "4.0C
  or xftpost-fnam    = 'BSEG-BLNPZ'                         "4.0C
  or xftpost-fnam    = 'BSEG-BLNKZ'                         "4.0C
  or xftpost-fnam    = 'BSEG-CESSION_KZ'
  or xftpost-fnam    = 'BSEG-BEWAR'                        "note 658991
*  or xftpost-fnam    = 'BSEG-FKBER_LONG'                  "note 607502
*  or xftpost-fnam    = 'BSEG-GRANT_NBR'                   "note 607502
  or xftpost-fnam    = 'BSEG-PENRC'.    "PromptPaymentAct (Note 571833)
    if xftpost-fnam = 'BSEG-EGMLD'.
      xftpost-fnam = 'BSEZ-EGMLD'.
    endif.
    clear ftz.
    move-corresponding xftpost to ftz.
    append ftz.
    exit.
  endif.

*------- Funds Management Felder parken in FTFM --------------------
  if xftpost-fnam    = 'BSEG-FIPOS'                         "30F
  or xftpost-fnam    = 'BSEG-GEBER'                         "30F
* or xftpost-fnam    = 'BSEG-FISTL'.                       "note 607502
  or xftpost-fnam    = 'BSEG-FISTL'                        "note 607502
  or xftpost-fnam    = 'BSEG-FKBER_LONG'                   "note 607502
  or xftpost-fnam    = 'BSEG-GRANT_NBR'                    "note 607502
  or xftpost-fnam    = 'BSEG-KBLNR'                        "note 607502
  or xftpost-fnam    = 'BSEG-KBLPOS'                       "note 607502
  or xftpost-fnam    = 'BSEZ-ERLKZ'                        "note 607502
  or xftpost-fnam    = 'BSEG-ERLKZ'.                       "note 607502
    if xftpost-fnam = 'BSEG-ERLKZ'.                        "note 607502
      xftpost-fnam = 'BSEZ-ERLKZ'.                         "note 607502
    endif.                                                 "note 607502
    clear ftps.                                             "30F
    move-corresponding xftpost to ftps.                     "30F
    append ftps.                                            "30F
    exit.                                                   "30F
  endif.                                                    "30F

*------- Abweichende Zentrale  ---------------------------------------
  if xftpost-fnam = 'BSEG-KNRZE'.
    knrze = xftpost-fval.
    exit.
  endif.

*------- Daten für AfA-Bereiche Anteilswerte-------------------"KJV----
  if xftpost-fnam(4) = 'ANEA'.                                 "KJV
    move-corresponding xftpost to ftab.                        "KJV
    append ftab.                                               "KJV
    exit.                                                      "KJV
  endif.                                                       "KJV

*------- Daten für Standardbilder (FTA) --------------------------------
*------- alle bisher noch nicht verarbeiteten Felder -------------------
  if xftpost-fnam = 'BSEG-REGUL'
  or xftpost-fnam = 'RF05A-REGUL'
  or xftpost-fnam = 'RF05V-REGUL'.
    if rep_name = rep_name_bv.
      xftpost-fnam = 'RF05V-REGUL'.
    else.
      xftpost-fnam = 'RF05A-REGUL'.
    endif.
    regul        = xftpost-fval.
  endif.
  clear fta.
  move-corresponding xftpost to fta.
  append fta.
endform.                    "xftpost_analysieren


*eject
*-----------------------------------------------------------------------
*        Form  XFTPOST_LOOP
*-----------------------------------------------------------------------
*        Buchungsdatentabelle abarbeiten
*-----------------------------------------------------------------------
form xftpost_loop.

  loop at xftpost.
    at first.
      clear index.
      perform dynpro_setzen_einstieg.
    endat.

*------- Gruppenwechsel Satztyp (K=Kopfsatz, P=Position) ---------------
    at new stype.
      if xftpost-stype cn 'KP'.
        message e007 with xftpost-stype raising record_type_invalid.
      endif.
      if not vbund is initial.
*       Aufruf beim ersten mal unterdrücken
        perform vbund_auf_kopf_uebertragen.
      endif.
    endat.

*------- Gruppenwechsel Satzzähler -------------------------------------
    at new count.
      if xftpost-stype = 'P'.
        if tcode ne 'FBVB' and xftpost-count > 950.
          message e023 with '950' raising too_many_line_items.
        elseif tcode eq 'FBVB' and xftpost-count > 999.
          message e023 with '999' raising too_many_line_items.
        endif.
        if xftpost-count > 1.
          perform position_uebertragen.
        endif.
      endif.
    endat.

*------- FTPOST-Daten in interne Tabellen übertragen/sortieren ------
    perform xftpost_analysieren.
  endloop.
endform.                    "xftpost_loop

*&---------------------------------------------------------------------*
*&      Form  DYNPRO_SENDEN_VV_ANZAHLUNGEN
*&---------------------------------------------------------------------*
*       Das Dynpro wird bei Anzahlungen auf Immobilienobjekte          *
*       gesendet. (Vermögensverwaltung Kontierungsblock Fullscreen)    *
*----------------------------------------------------------------------*
form dynpro_senden_vv_anzahlungen.

* 'Weiter-Flag' setzen für RE               "MPR
  clear ft.                                                 "MPR
  ft-fnam     = 'RF05A-XIMKO'.                              "MPR
  ft-fval     = 'X'.                                        "MPR
  append ft.                                                "MPR

*   VV-Kontierungsblock-Dynpro
  clear ft.
  ft-program  = rep_name_vk.
  ft-dynpro   = '0100'.
  ft-dynbegin = 'X'.
  append ft.
  loop at ftvk.
    ft = ftvk.
    append ft.
  endloop.
  refresh ftvk.
endform.                               " DYNPRO_SENDEN_VV_ANZAHLUNGEN


*eject
*&-------------------------------------------------------------------
*&      Form  VBUND_AUF_KOPF_UEBERTRAGEN
*&-------------------------------------------------------------------
*&      Routine komplett neu durch QHA941207
*&-------------------------------------------------------------------
*&      INDEX nicht hochsetzen, da Fußzeile vor VBUND
*&      kommen muß
*&      CLEAR VBUND  sonst wird das Bild mehrfach gesendet
*&------------------------------------------------------------------
form vbund_auf_kopf_uebertragen.
* OK-Code auf Kopf-Dynpro setzen
  clear ft.
  ft-fnam = 'BDC_OKCODE'.
  ft-fval = 'PG'.
  append ft.

* VBUND-Dynpro
  clear ft.
  ft-program  = 'SAPLF014'.
  ft-dynpro   = '0100'.
  ft-dynbegin = 'X'.
  append ft.

  clear ft.
  ft-fnam = 'RF014-VBUND'.
  ft-fval = vbund.
  append ft.

  clear vbund.
endform.                    "vbund_auf_kopf_uebertragen


*&---------------------------------------------------------------------*
*&      Form  XFTTAX_KOMPRIMIEREN
*&---------------------------------------------------------------------*
*&      Diese Routine komprimiert die FTTAX-Zeilen
*&      Zeilen mit gleichem MWSKZ, BSCHL, TXJCD und KSCHL können
*&      zusammengefaßt werden
*&---------------------------------------------------------------------*
form xfttax_komprimieren.
* begin of note 794969
* check whether jurisdiction codes are active
* and whether then 'calculate tax line by line' is active
  data: jurcode_active,
        external_system_aktive,
        tax_linewise.

  clear: jurcode_active,
         tax_linewise,
         external_system_aktive,
         txkrs.                          " note 1257048

  call function 'CHECK_JURISDICTION_ACTIVE'
    exporting
      i_bukrs    = bkpf_bukrs
    importing
      e_isactive = jurcode_active
      e_external = external_system_aktive
      e_xtxit    = tax_linewise.
* end of note 794969

  refresh cxfttax.
  loop at xfttax.
* der erste Wert, der in XFTTAX-TXKRS übergeben wird, in TXKRS sichern (1257048)
    if txkrs is INITIAL.                 " note 1257048
      txkrs = xfttax-txkrs.              " note 1257048
    endif.                               " note 1257048
    clear xfttax-txkrs.                  " note 1257048
    clear cxfttax.
    move-corresponding xfttax to cxfttax.
    if not xfttax-fwste is initial.
*-------  Waers enthält die Belegwährung  ----------------------------
* ------  WAERS IST INITIAL, WENN ERFASSUNG NUR IN HAUSWÄHRUNG (FB00) -
      if waers is initial.
        waers = t001-waers.
      endif.

      perform betrag_aufbereiten using xfttax-fwste
                                       waers
                                       cxfttax-pfwste
                                       rcode.
      if rcode ne 0.
        message e022 with text-001 xfttax-fwste
                raising amount_format_error.
      endif.
    endif.
    if not xfttax-hwste is initial.
*     T001-Waers enthält die Buchungskreiswährung
      perform betrag_aufbereiten using xfttax-hwste
                                       t001-waers
                                       cxfttax-phwste
                                       rcode.
      if rcode ne 0.
        message e022 with text-001 xfttax-fwste
                raising amount_format_error.
      endif.
    endif.
    if tax_linewise is initial.                             "N794969
      collect cxfttax.
    else.                                                   "N794969
      append cxfttax.                                       "N794969
    endif.                                                  "N794969
  endloop.

  refresh xfttax.

  loop at cxfttax.
    clear xfttax.
    move-corresponding cxfttax to xfttax.
    if not cxfttax-pfwste is initial.
*     Waers enthält die Belegwährung
      write cxfttax-pfwste to xfttax-fwste currency waers.
    endif.
    if not cxfttax-phwste is initial.
*     T001-Waers enthält die Buchungskreiswährung
      write cxfttax-phwste to xfttax-hwste currency t001-waers.
    endif.
    append xfttax.
  endloop.
endform.                               " XFTTAX_KOMPRIMIEREN



*eject
*---------------------------------------------------------------------*
*       FORM BETRAG_AUFBEREITEN                                       *
*---------------------------------------------------------------------*
*       Die Betrag wird konvertiert in ein betragsfeld                *
*---------------------------------------------------------------------*
*  -->  BETRAG    aufzubereitender Betrag.                            *
*  -->  waers     Währung                                             *
*  <--  BETRAG_OUT aufbereiteter Betrag.                              *
*  <--  RC        Return Code                                         *
*---------------------------------------------------------------------*
form betrag_aufbereiten using betrag waers betrag_out rc.
  data:  addez               type i.   " zu addierende Dezst.
  data:  anzdez              type i.   " Anzahl Dezimalstellen
  data:  betrag_in(16)       type c.
  data:  betrag_n(18)        type n.
  data:  betrag_pd2(15)      type p decimals 2.
  data:  blen                type i.   " Länge
  data:  char4(4)            type c.   " Hilfsfeld
  data:  char18(18)          type c.   " Hilfsfeld (allg.)
  data:  char30(30)          type c.   " Hilfsfeld (allg.)
  data:  deznul(4)           type c.   " Hilfsfeld
  data:  eins(1)             type c value '1'.              " Char. 1
  data:  refe(16)            type p.   " Rechenfeld

  clear rc.
  betrag_in = betrag.
  condense betrag_in no-gaps.

* ------ Dezimalstellen der Währung ------------------------------------
  if waers ne waers_old.
    select single * from tcurx where currkey = waers.
    if sy-subrc = 0.
      dezstellen = tcurx-currdec.
    else.
      dezstellen = 2.
    endif.
    waers_old = waers.

  endif.

  if dezzeichen is initial.
* ------ Dezimalzeichen: Komma oder Punkt ? ----------------------------
    clear char4.
    refe = 12.
    write refe to char4 currency eins.
    dezzeichen = char4+1(1).

* ------ Fixpunktarithmetik akiv ? ------------------------------------
    select single * from trdir where name = sy-repid.
    fixpt = trdir-fixpt.
  endif.

* ------ Unzulässige Zeichen? ------------------------------------------
  if betrag_in cn '0123456789,.- '.
    rc = 4.
    exit.
  endif.

* ------ Tausenderpunkte eliminieren -----------------------------------
  if dezzeichen = ','.
    translate betrag_in using '. '.
  else.
    translate betrag_in using ', '.
  endif.
* ------ Vorzeichen eliminieren ----------------------------------------
  translate betrag_in using '- '.
  condense betrag_in no-gaps.

* ------ Dezimalzeichen in Punkt ändern -------------------------------
  if dezzeichen = ','.
    translate betrag_in using ',.'.
  endif.

* ------ Prüfen, ob mehrere Dezimalzeichen übergeben wurden -----------
  if betrag_in cs '.'.
    char18 = betrag_in.
    shift char18 left by sy-fdpos places.
    shift char18 left.
    if char18 cs '.'.
      rc = 4.
      exit.
    endif.
  endif.

* ------ evtl. Dezimalstellen ergänzen --------------------------------
  blen = strlen( betrag_in ).
  if betrag_in cs '.'.
    anzdez = blen - sy-fdpos - 1.
  else.
    anzdez = 0.
  endif.

  addez = dezstellen - anzdez.
  if addez >= 0.
    do addez times.
      deznul+0(1) = '0'.
      shift deznul right.
    enddo.
  else.
    rc = 4.
    exit.
  endif.

  char30 = betrag_in.
  char30+20 = deznul.
  translate char30 using '. '.
  condense char30 no-gaps.
  betrag_in  = char30.
  betrag_n   = betrag_in.
  betrag_pd2 = betrag_n.

  if fixpt = 'X'.
    betrag_pd2 = betrag_pd2 / 100.
  endif.

  betrag_out = betrag_pd2.
  rc = 0.
endform.                    "betrag_aufbereiten

*eject
*&---------------------------------------------------------------------*
*&      Form  DIRECT_TAX_POSTING
*&---------------------------------------------------------------------*
*&     Beim direkten Bebuchen von Steuerkonten kann der Geschäftsbereich
*&     als COBL-GSBER angeliefert werden und ist deshalb in FTK
*&     Auf Dynpro 312 muß er aber als BSEG-GSBER ohne Kontierungsblock
*&     übertragen werden.
*&---------------------------------------------------------------------*
form direct_tax_posting.
  loop at ftk.
* Felder KOSTL, AUFNR und COBL-PS_PSP_PNR werden wegen dem
* Kontierungsblock SAPLGJTS/0001 auf dem Steuerbild (Joint-Venture)
* NICHT umbenannt (CSP-Hinweis 81413).
    if not ( ftk-fnam = 'COBL-KOSTL'
            or ftk-fnam = 'COBL-AUFNR'
            or ftk-fnam = 'COBL-RECID'
            or ftk-fnam = 'COBL-PS_PSP_PNR' ).
      replace 'COBL' with 'BSEG' into ftk-fnam.
    endif.
    ft = ftk.
    append ft.
  endloop.

  describe table ft lines index.
endform.                               " DIRECT_TAX_POSTING

*eject
*&---------------------------------------------------------------------*
*&      Form  COPA_DATEN
*&---------------------------------------------------------------------*
*       falls FTCOPA Daten enthält COPA FB aufrufen und                *
*       anschließend FT füllen                                         *
*----------------------------------------------------------------------*
form copa_daten.
  check tfill_ftcopa > 0.
  data: begin of ft_bdc occurs 0.
          include structure bdcdata.
  data: end of ft_bdc.

* Wegen User-Exit im RKE_FILL_BDCDATA_WITH_CRITERIA
* werden wichtige Daten im IN_COBL uebergeben.
* HKONT wird bei Anlagen nicht gefüllt, da in der Fusszeile
* (Fusszeile-Konto gespeichert in 'konto')
* das Anlagekonto und nicht das Sachkonto der Hauptbuchhaltung
* uebergeben wird.
  if tcode = 'FB01' or tcode =  'FBVB'.
    perform fill_in_cobl using 'VORGN' 'RFBU'.
  endif.

  perform fill_in_cobl using 'BLART' blart.
  perform fill_in_cobl using 'BUKRS' bukrs.
  if xtbsl-koart = 'S'.
    perform fill_in_cobl using 'HKONT' konto.
  endif.
  call function 'CONVERSION_EXIT_IDATE_INPUT'
    exporting
      input  = budat
    importing
      output = budat_int.
  if budat_int = space.            "error case
    budat_int = budat.
  endif.
  perform fill_in_cobl using 'BUDAT' budat_int.

  call function 'RKE_FILL_BDCDATA_WITH_CRITERIA'
    exporting
      i_cobl         = in_cobl
    tables
      i_copadata     = ftcopa
      i_bdcdata      = ft_bdc
    exceptions
      no_bukrs_found = 1
      no_erkrs_found = 2
      others         = 3.

  describe table ft_bdc lines  tfill_ftcopa.
  if tfill_ftcopa > 0.
*--------- Flag setzen für Aufruf von Detailbild -----------------------
    clear ft.
    ft-fnam = 'DKACB-XERGO'.
    ft-fval = 'X'.
    append ft.
    clear ft.
*--------- COPA Daten --------------------------------------------------
    loop at ft_bdc.
      ft = ft_bdc.
      append ft.
    endloop.
  endif.
*--------- Rücksprung zum Kontierungsblockbild -------------------------
  if sy-subrc = 0.
    clear ft.
    ft-program  = rep_name_k.
    ft-dynpro   = '0002'.
    ft-dynbegin = 'X'.
    append ft.
  endif.
endform.                               " COPA_DATEN

*&---------------------------------------------------------------------*
*&      Form  FIND_US_TAX_SCREEN_NUMBER           "new with 30E
*&---------------------------------------------------------------------*
*       decide if detailed screen 450 or general screen 300            *
*       if there is a jurisdiction that has only space and zero        *
*       after the prefix (leng1) the data are detailed -> screen 0450  *
*----------------------------------------------------------------------*
form find_us_tax_screen_number using screen.
  data: detail_screen(1) type c.
  loop at xfttax.
    if xfttax-txjcd is initial
* if tax entered on detail level then kschl must be filled as well
    or xfttax-kschl is initial.                                "N864459
      exit.
    endif.
    shift xfttax-txjcd left by ttxd-leng1 places.
    if  xfttax-txjcd co ' 0'.
      detail_screen = 'X'.
      exit.
    endif.
  endloop.
  if detail_screen = 'X'.
    screen = '0450'.
  else.
    screen = '0300'.
  endif.
endform.                               " FIND_US_TAX_SCREEN_NUMBER

*&---------------------------------------------------------------------*
*&      Form  SET_FUNCTION_CODE_STEB
*&---------------------------------------------------------------------*
*       Set function code STEB (Detail Tax Batch Input)                *
*----------------------------------------------------------------------*
form set_function_code_steb.
  clear ft.                            "STEG
  ft-fnam     = 'BDC_OKCODE'.          "STEG
  ft-fval     = 'STEB'.                "STEG
  index = index + 1.                   "STEG
  insert ft index index.               "STEG

endform.                               " SET_FUNCTION_CODE_STEB

*&---------------------------------------------------------------------*
*&      Form  SET_FUNCTION_CODE_STEG
*&---------------------------------------------------------------------*
*       Set function code STEG (Summary tax with Jurisdiction Code)    *
*----------------------------------------------------------------------*
form set_function_code_steg.
  clear ft.                            "STEG
  ft-fnam     = 'BDC_OKCODE'.          "STEG
  ft-fval     = 'STEG'.                "STEG
  index = index + 1.                   "STEG
  insert ft index index.               "STEG

endform.                               " SET_FUNCTION_CODE_STEB
*&---------------------------------------------------------------------*
*&      Form  FILL_IN_COBL
*&---------------------------------------------------------------------*
*       Der Header IN_COBL wird mit den COBL-Feldern gefüllt, die in
*       der BBSEG übergeben wurden.
*----------------------------------------------------------------------*
*  -->  I_FIELDNAME        Feldname
*  -->  I_FIELDVALUE       Feldinhalt
*----------------------------------------------------------------------*
form fill_in_cobl using i_fieldname
                        i_fieldvalue.

* Füllen des Export-Parameters I_COBL des FBs
* RKE_FILL_BDCDATA_WITH_CRITERIA  mit den COBL-Feldern.

  data: begin of fieldname,
          fix(8) value 'IN_COBL-',
          var like dd03p-fieldname,
        end of fieldname.
  field-symbols: <cobl-field>.

* Die Namensungleichheiten zwischen BBSEG und COBL wurden schon in
* RFBIBL02 ausgeglichen

  fieldname-var = i_fieldname.
  assign (fieldname) to <cobl-field>.
  <cobl-field> = i_fieldvalue.
endform.                               " FILL_IN_COBL
*&---------------------------------------------------------------"KJV -*
*&      Form  DYNPRO_SENDEN_FTAB                                 "KJV
*&---------------------------------------------------------------"KJV -*
*       text                                                     "KJV
*----------------------------------------------------------------"KJV -*
*  -->    ANEP-Daten aus FTAB in FT übertragen                   "KJV
*  <--  p2        text                                           "KJV
* JVA: Additional screen for assets wipe-ups
*----------------------------------------------------------------"KJV -*
form dynpro_senden_ftab.                                         "KJV
  data: begin of ftab_fields occurs 0,                         "KJV
          fnam  like ftab-fnam,                                "KJV
        end of ftab_fields .                                   "KJV
  check dynnr = 305.                                           "KJV
  refresh ftab_fields.                                         "KJV
  clear ft.                                                    "KJV
  ft-program  = rep_name_ab.                                   "KJV
  ft-dynpro   = '0275'.                                        "KJV
  ft-dynbegin = 'X'.                                           "KJV
  append ft.                                                   "KJV
  loop at ftab.                                                "KJV
    loop at ftab_fields where fnam = ftab-fnam.                "KJV
    endloop.                                                   "KJV
    if sy-subrc = 0. "field found --> new dynpro               "KJV
      ft-program  = rep_name_ab.                               "KJV
      ft-dynpro   = '0275'.                                    "KJV
      ft-dynbegin = 'X'.                                        "KJV
      clear: ft-fnam, ft-fval.                                 "KJV
      append ft.                                               "KJV
      refresh ftab_fields.                                     "KJV
    endif.                                                     "KJV
    ft = ftab.                                                 "KJV
    append ft.                                                 "KJV
    ftab_fields-fnam = ftab-fnam.                             "KJV
    append ftab_fields.                                        "KJV
  endloop.                                                     "KJV
endform.                    " DYNPRO_SENDEN_FTAB                "KJV

*&---------------------------------------------------------------------*
*&      Form  isis_daten
*&---------------------------------------------------------------------*
*       falls FTISIS Daten enthält ISIS FB aufrufen und                *
*       anschließend FT füllen
*----------------------------------------------------------------------*
form isis_daten.

  check tfill_ftisis > 0.
  data: begin of ft_bdc occurs 0.
          include structure bdcdata.
  data: end of ft_bdc.

*-----Falls Versicherungsdaten da sind, muss auf das Versicherungsdynpro
*-----gesprungen werden. Dazu auf dem Kontierungsblock-Dynpro das
*-----Ankreuzfeld setzen.

  clear ft.
  ft-fnam = 'DKACB-XINSUR' .
  ft-fval = 'X'.
  append ft.

  call function 'ISIS_FILL_BDCDATA'
    exporting
      i_form     = 'X'
    tables
      t_copadata = ftisis
      t_bdcdata  = ft_bdc.                                  "#EC *

  loop at ft_bdc.
    clear ft.
    ft = ft_bdc.
    append ft.
  endloop.

*--------- Rücksprung zum Kontierungsblockbild -------------------------
  if sy-subrc = 0.
    clear ft.
    ft-program  = rep_name_k.
    ft-dynpro   = '0002'.
    ft-dynbegin = 'X'.
    append ft.
  endif.


endform.                    " isis_daten
*&---------------------------------------------------------------------*
*&      Form  FCODE_PBBP
*&---------------------------------------------------------------------*
*       Falls Transaktion FBV1 und "vollständig parken" gewählt
*       wurde mit =PBBP abschliessen.
*----------------------------------------------------------------------*
form fcode_pbbp.

  if tfill_ftvv ne 0.
*   Dynpro für Vermögensverwaltung wurde gesendet.
*   Rücksprung auf das vorige Standad-Dynpro
    clear ft.
    ft-program  = rep_name.
    ft-dynpro   = dynnr.
    ft-dynbegin = 'X'.
    append ft.
    if dynnr = 300 or dynnr = 305.
      perform leeres_cobl_to_ft.
    endif.
  endif.
  if tfill_xfttax ne 0 and xmwst ne 'X'.
    perform tax_dynpro.
*   back to standard-dynpro
    describe table ft lines index.
  endif.

  clear ft.
  ft-fnam = 'BDC_OKCODE'.
  ft-fval = '=PBBP'.
  index = index + 1.
  insert ft index index.
endform.                                                    " FCODE_PBBP
*&---------------------------------------------------------------------*
*&      Form  generic_kontl_data
*&---------------------------------------------------------------------*
* Created by Note 499049
*-----------------------------------------------------------------------
* Support handling of field KONTL which can be used by several IS.
*
* We decode KONTL at this point by calling the appropriate
* decode function which has to be provided by the industry solution
*----------------------------------------------------------------------*
form generic_kontl_data .

  check tfill_ft_generic_kontl > 0.

  data: begin of ft_bdc occurs 0.
          include structure bdcdata.
  data: end of ft_bdc.
  data: ld_kontl type kontl_fi,
        ld_kontt type kontt_fi,
        ld_program type repid,
        ld_screen type dynnr,
        ld_leave_code type tcode.

  read table ft_generic_kontl with key fnam = 'BSEG-KONTL'.
  if sy-subrc = 0.
    ld_kontl = ft_generic_kontl-fval.
  endif.

  read table ft_generic_kontl with key fnam = 'BSEG-KONTT'.
  if sy-subrc = 0.
    ld_kontt = ft_generic_kontl-fval.
  endif.

*-----------------------------------------------------------------------
* As of now, only IS Insurance uses field KONTL; therefore
* we don't call a BADI or BTE here.
* Please also refer to similar source code in function modules
* FI_DOC_TO_ACCFI_TRANSFORM
* FI_DOC_TO_ACC_TRANSFORM
* in which a similar decoding is performed....
*-----------------------------------------------------------------------
  clear ft.

  case ld_kontt.
    WHEN 'VV' or 'VX'.    " account assignment type of IS insurance

* The following coding moved to routine dynpro_senden_ftk. Note 604733
* set 'details' flag
*    ft-fnam = 'DKACB-XINSUR'.
*    ft-fval = 'X'.
*    append ft.

* call appropriate decode function
      call function 'FI_DECODE_KONTL'
        exporting
          i_kontt      = ld_kontt
          i_kontl      = ld_kontl
        importing
          i_program    = ld_program
          i_screen     = ld_screen
          i_leave_code = ld_leave_code
        tables
          t_bdcft      = ft_bdc.                            "#EC *

* call appropriate program and screen
      clear ft.
      ft-program  = ld_program.
      ft-dynpro   = ld_screen.
      ft-dynbegin = 'X'.
      append ft.

* transfer decoded fields to ft structure
      loop at ft_bdc.
        clear ft.
        ft = ft_bdc.
        append ft.
      endloop.

* leave screen
      clear ft.
      if not ld_leave_code is initial.
        ft-fnam = 'BDC_OKCODE'.
        ft-fval = ld_leave_code.
        append ft.
      endif.

    when others.
*  call whatever kind of decode function here, similar to example above
  endcase.

*--------- Back to COBL screen SAPLKACB 0002

  if sy-subrc = 0.
    clear ft.
    ft-program  = rep_name_k.
    ft-dynpro   = '0002'.
    ft-dynbegin = 'X'.
    append ft.
  endif.

endform.                    " generic_kontl_data.
*&---------------------------------------------------------------------*
*&      Form  tax_exchange_rate
*&---------------------------------------------------------------------*
*      Created by Note  564235
*----------------------------------------------------------------------*

form tax_exchange_rate.

  data: ld_data(10) type c.

* ----- Provide the exchange rate for tax calculations if necessary----
  check not txkrs is initial.

  write txkrs to ld_data using edit mask '==EXCRT'.
  clear ft.
  ft-fnam(14)    = 'RTAX1-KURSF'.
  ft-fval = ld_data.
  append ft.

endform.                    " tax_exchange_rate
*&---------------------------------------------------------------------*
*&      Form  iban_data
*&---------------------------------------------------------------------*
*       Dynpro mit Iban Daten aufrufen
*----------------------------------------------------------------------*
form iban_data .

  data: ld_index type n,
        ld_index2 type sy-index,
        ld_iban  type iban.

  field-symbols <ftc> type BDCDATA.

  read table ftc with key FNAM = 'BSEC-BANKN' assigning <ftc>.
  if sy-subrc ne 0 or <ftc>-fval is initial.
* iban without bank account: SAPLIBMA screen 0200

*   okcode fuer IBAN
    clear ft.
    ft-fnam = 'BDC_OKCODE'.
    ft-fval = 'IBAN'.
    append ft.
*   Daten uebertragen
    clear ft.
    ft-program  = rep_name_iban.
    ft-dynpro   = '0200'.
    ft-dynbegin = 'X'.
    append ft.
    clear ft.
    ft-fnam  = 'BDC_OKCODE'.
    ft-fval  = '=ENTR'.
    append ft.
    ft-fnam  = 'BDC_CURSOR'.
    ft-fval  = 'IBAN01'.
    append ft.
    loop at ftiban.
      if ftiban-fnam ne 'TIBAN-IBAN'.
         if ftiban-fnam = 'TIBAN-BANKS' or
            ftiban-fnam = 'TIBAN-BANKL'.
             ft = ftiban.
             append ft.
         endif.
      else.
        condense ft-fval no-gaps.
        ld_iban = ft-fval(34).
* IBAN Daten aufspalten.
* Es werden alle neun Felder gefuellt, um zu verhindern
* dass generierte Daten ausversehen uebernommen werden
        do 9 times.
          ld_index = sy-index.
          concatenate 'IBAN' '0' ld_index into ft-fnam.
          ld_index2 = ( sy-index - 1 ) * 4 .
          if sy-index le 8.
            ft-fval = ftiban-fval+ld_index2(4).
          else.
            ft-fval = ftiban-fval+ld_index2(2).
          endif.
          append ft.
        enddo.
      endif.
    endloop.
    clear ft.
    ft-program  = rep_name_iban.
    ft-dynpro   = '0200'.
    ft-dynbegin = 'X'.
    append ft.
    clear ft.
    ft-fnam  = 'BDC_OKCODE'.
    ft-fval  = '=ENTR'.
    append ft.

  else.
* iban with bank account: SAPLIBMA screen 0100

*   okcode fuer IBAN
  clear ft.
  ft-fnam = 'BDC_OKCODE'.
  ft-fval = 'IBAN'.
  append ft.
*   Daten uebertragen
  clear ft.
  ft-program  = rep_name_iban.
  ft-dynpro   = '0100'.
  ft-dynbegin = 'X'.
  append ft.
  clear ft.
  ft-fnam  = 'BDC_OKCODE'.
  ft-fval  = '=ENTR'.
  append ft.
  ft-fnam  = 'BDC_CURSOR'.
  ft-fval  = 'IBAN01'.
  append ft.
  loop at ftiban.
    if ftiban-fnam ne 'TIBAN-IBAN'.
      ft = ftiban.
      append ft.
    else.
      condense ft-fval no-gaps.
      ld_iban = ft-fval(34).
* IBAN Daten aufspalten.
* Es werden alle neun Felder gefuellt, um zu verhindern
* dass generierte Daten ausversehen uebernommen werden
      do 9 times.
        ld_index = sy-index.
        concatenate 'IBAN' '0' ld_index into ft-fnam.
        ld_index2 = ( sy-index - 1 ) * 4 .
        if sy-index le 8.
          ft-fval = ftiban-fval+ld_index2(4).
        else.
          ft-fval = ftiban-fval+ld_index2(2).
        endif.
        append ft.
      enddo.
    endif.
  endloop.

  endif.


* Ruecksprung zum CPD Bild
  clear ft.
  ft-program  = rep_name_c.
  ft-dynpro   = '0100'.
  ft-dynbegin = 'X'.
  append ft.

endform.                    " iban_data
*&---------------------------------------------------------------------*
*&      Form  CHECK_BBSEG_FOR_VZK
*&---------------------------------------------------------------------*
*       VZK in BBSEG => IS-IS (insurance) active
*----------------------------------------------------------------------*
FORM CHECK_BBSEG_FOR_VZK  CHANGING vzk_active type c.

  DATA: lv_descr  TYPE REF TO cl_abap_structdescr,
        lv_length TYPE i.
  FIELD-SYMBOLS: <comp_descr> TYPE abap_compdescr.

  lv_descr ?= cl_abap_structdescr=>describe_by_name( 'BBSEG' ).
  CLEAR vzk_active.

  LOOP AT lv_descr->components ASSIGNING <comp_descr>.
    lv_length = numofchar( <comp_descr>-name ) .
    IF lv_length >= 5.
      SUBTRACT 5 FROM lv_length.
      IF <comp_descr>-name(5) = 'ISCD_' OR
       <comp_descr>-name+lv_length(5) = '_ISCD' .
        vzk_active = 'X'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHECK_BBSEG_FOR_VZK
