FUNCTION-POOL ZFI_CLEARING MESSAGE-ID F8."MESSAGE-ID ..

* INCLUDE LZFI_CLEARINGD...                  " Local class definition


* INCLUDE LZFI_CLEARINGD..." Local class definition
*-----------------------------------------------------------------------
*        Tabellen / Strukturen
*-----------------------------------------------------------------------

TABLES:  RFIPI.                        " Arbeits- Schnittstellenfelder
TABLES:  T001,                         " Buchungkreistabelle
         T005,                         " Ländertabelle
         T019W,                        " Window-Auswahl Buchhaltung
         T041A.                        " Ausgleichsvorgänge

TABLES:  TSTC,                         " SAP-Transaktions-Codes
         TTXD,                         " Struktur des Steuerstandortcode
         TBSL.                         " Buchungsschlüssel

TABLES:  SKB1.                         " Sachkontenstamm (Buchungskreis)

TABLES:  TCURX,          " Dezimalstellen der Währungen  " QHA950512
         TRDIR.          " Systemtabelle TRDIR           " QHA950512

TABLES:  T074U.

tables : bsid , bkpf , BSIK .

*------- Feldtabelle gesamt --------------------------------------------
DATA:    BEGIN OF FT OCCURS 10.
           INCLUDE STRUCTURE BDCDATA.
DATA:    END OF FT.

*------- Feldtabelle Standard-Dynpros ----------------------------------
DATA:    BEGIN OF FTA OCCURS 10.
           INCLUDE STRUCTURE BDCDATA.
DATA:    END OF FTA.

*------- Feldtabelle CPD-Dynpro ----------------------------------------
DATA:    BEGIN OF FTC OCCURS 10.
           INCLUDE STRUCTURE BDCDATA.
DATA:    END OF FTC.

*------- Feldtabelle IBAN-Daten ----------------------------------------
DATA:    BEGIN OF FTIBAN OCCURS 0.
           INCLUDE STRUCTURE BDCDATA.
DATA:    END OF FTIBAN.

*------- Feldtabelle COPA-Daten ----------------------------------------
DATA:    BEGIN OF FTCOPA OCCURS 0.
           INCLUDE STRUCTURE COPADATA.
DATA:    END OF FTCOPA.

*------- Feldtabelle ISIS-Daten ----------------------------------------
DATA:    BEGIN OF FTISIS OCCURS 0.
           INCLUDE STRUCTURE COPADATA.
DATA:    END OF FTISIS.

*------- Fieldtable for IS data, stored in generic string KONTL
DATA:    BEGIN OF FT_GENERIC_KONTL OCCURS 0.
           INCLUDE STRUCTURE COPADATA.
DATA:    END OF FT_GENERIC_KONTL.

*------- Feldtabelle Fußzeiledaten -------------------------------------
DATA:    BEGIN OF FTF OCCURS 10.
           INCLUDE STRUCTURE BDCDATA.
DATA:    END OF FTF.

*------- Feldtabelle Fußzeiledaten (nur für Konten) --------------------
DATA:    BEGIN OF FTFKTO OCCURS 4.
           INCLUDE STRUCTURE BDCDATA.
DATA:    END OF FTFKTO.

*------- Feldtabelle Felder Public Sector ------------------------------
DATA:    BEGIN OF FTPS   OCCURS 3.                        "30F
           INCLUDE STRUCTURE BDCDATA.                     "30F
DATA:    END OF FTPS.                                     "30F

*------- Feldtabelle Kontierungsblock-Dynpro ---------------------------
DATA:    BEGIN OF FTK OCCURS 10.
           INCLUDE STRUCTURE BDCDATA.
DATA:    END OF FTK.

*------- Feldtabelle Steuern-Dynpro ------------------------------------
DATA:    BEGIN OF FTT OCCURS 10.
           INCLUDE STRUCTURE BDCDATA.
DATA:    END OF FTT.

*------- Feldtabelle Zusatz-Dynpro der Vermögensverwaltung -------------
DATA:    BEGIN OF FTVV OCCURS 10.
           INCLUDE STRUCTURE BDCDATA.
DATA:    END OF FTVV.

*------- Feldtabelle Kontierungsbl.Zusatz-Dynpro der Vermögensverwaltung
DATA:    BEGIN OF FTVK OCCURS 10.
           INCLUDE STRUCTURE BDCDATA.
DATA:    END OF FTVK.

*------- Feldtabelle Anlagen AfA-Anteile --------------------"KJV------
DATA:    BEGIN OF FTAB OCCURS 10.                            "KJV
           INCLUDE STRUCTURE BDCDATA.                        "KJV
DATA:    END OF FTAB.                                        "KJV

*------- Field table for extended withholding tax ----------------------
DATA:    BEGIN OF FTW OCCURS 10.
           INCLUDE STRUCTURE BDCDATA.
DATA:    END OF FTW.

*------- Feldtabelle Zusatz-Dynpros ------------------------------------
DATA:    BEGIN OF FTZ OCCURS 10.
           INCLUDE STRUCTURE BDCDATA.
DATA:    END OF FTZ.

*------- Feldtabelle Betragssplitt------------------------------------
DATA:    BEGIN OF FTSPLT OCCURS 10.
           INCLUDE STRUCTURE BDCDATA.
DATA:    END OF FTSPLT.

*------- Feldtabelle Betragssplitt WT ---------------------------------
DATA:    BEGIN OF FTSPLT_WT OCCURS 10.
           INCLUDE STRUCTURE BDCDATA.
DATA:    END OF FTSPLT_WT.

*------- Belegnummerntabelle -------------------------------------------
DATA:    BEGIN OF XBLTAB OCCURS 2.
           INCLUDE STRUCTURE BLNTAB.
DATA:    END OF XBLTAB.

*------- Feldtabelle mit Ausgleichsdaten aus Schnittstelle -------------
DATA:    BEGIN OF XFTCLEAR OCCURS 50.
           INCLUDE STRUCTURE FTCLEAR.
DATA:    END OF XFTCLEAR.

*------- Temporary Storage for data records without SFELD  -------------
DATA:    BEGIN OF YFTCLEAR .                                 "31i
           INCLUDE STRUCTURE FTCLEAR.
DATA:    END OF YFTCLEAR.

*------- Feldtabelle mit BKPF- und BSEG-Daten aus Schnittstelle --------
DATA:    BEGIN OF XFTPOST OCCURS 50.
           INCLUDE STRUCTURE FTPOST.
DATA:    END OF XFTPOST.

*------- Feldtabelle mit den Steuern -----------------------------------
DATA:    BEGIN OF XFTTAX OCCURS 10.
           INCLUDE STRUCTURE FTTAX.
DATA:    END OF XFTTAX.

*------- Feldtabelle mit den komprimierten Steuern --------------
DATA:    BEGIN OF CXFTTAX OCCURS 10,
           MWSKZ   LIKE FTTAX-MWSKZ,
           BSCHL   LIKE FTTAX-BSCHL,
           TXJCD   LIKE FTTAX-TXJCD,
           KSCHL   LIKE FTTAX-KSCHL,
           PFWSTE  LIKE BSET-FWSTE,
           PHWSTE  LIKE BSET-HWSTE,
           TXKRS   TYPE TXKRS_BKPF,                       " Note 564235
         END OF CXFTTAX.

*------- Tabelle XTBSL (Buchungsschlüssel) -----------------------------
DATA:    BEGIN OF XTBSL OCCURS 10.
           INCLUDE STRUCTURE TBSL.
DATA:    END OF XTBSL.

*------- Tabelle XT041A (Ausgleichsvorgänge) ---------------------------
DATA:    BEGIN OF XT041A OCCURS 5,
           AUGLV        LIKE T041A-AUGLV,
         END OF XT041A.

*------- Feldleiste mit den Steuern ------------------------------------
DATA:    BEGIN OF YFTTAX.
           INCLUDE STRUCTURE FTTAX.
DATA:    END OF YFTTAX.
*eject

*------- Hilfsheader          ------------------------------------------

* Header IN_COBL beinhaltet die übergebenen COBL-Felder.
* Wird im FB RKE_FILL_BDCDATA_WITH_CRITERIA benötigt um zu verhindern,
* daß Kontierungsmerkmale auf das Dynpro SAPLKACB/0002 doppelt
* gesendet werden, falls man diese doppelt übergibt (in FTK und FTCOPA)
* Bsp. BBSEG-PRCTR und BBSEG-RKE_PRCTR wird gefüllt.
* (Falls man ein Feld doppelt auf ein Dynpro sendet, wird dieses Feld
* nicht mehr eingabebereit.
DATA IN_COBL LIKE COBL.


*-----------------------------------------------------------------------
*        Einzelfelder
*-----------------------------------------------------------------------

*------- Einzelfelder Schnittstelle ------------------------------------
DATA:    AUGLV          LIKE T041A-AUGLV,      " Ausgleichsvorgang
         AUGBL          LIKE RF05R-AUGBL,      " Ausgleichsbelegnummer
         BELNS          LIKE RF05A-BELNS,      " Belnr zu storn. Beleg
         BDCIMMED       LIKE RFIPI-BDCIMMED,   " nur BDC: sof. Abspielen
         BDCSTRTDT      LIKE TBTCJOB-SDLSTRTDT,  "nur BDC: Startdatum
         BDCSTRTTM      LIKE TBTCJOB-SDLSTRTTM,  "nur BDC: Startzeit
         BUDAT          LIKE BSIS-BUDAT,       " Budat Stornobeleg
         FTFKTO_INDX    LIKE SY-TABIX,  "QHA   " Index merken für FTFKTO
         FUNCT          LIKE RFIPI-FUNCT,      " Funktion
         GROUP          LIKE APQI-GROUPID,     " Mappenname
         GJAHR          LIKE RF05R-GJAHR,      " Geschäftsjahr
         GJAHS          LIKE RF05A-GJAHS,      " Gjahr zu storn. Beleg
         HOLDD          LIKE APQI-STARTDATE,   " Startdateum
         MANDT          LIKE SY-MANDT,         " Mandant
         MODE(1)        TYPE C,                " Anzeigemodus
         MONAT          LIKE BSIS-MONAT,       " Buchungsper Stornobeleg

         MSGID          LIKE SY-MSGID,         " Message-ID
         MSGNO          LIKE SY-MSGNO,         " Message-Nummer
         MSGTY          LIKE SY-MSGTY,         " Message-Typ
         MSGV1          LIKE SY-MSGV1,         " Message-Variable 1
         MSGV2          LIKE SY-MSGV2,         " Message-Variable 2
         MSGV3          LIKE SY-MSGV3,         " Message-Variable 3
         MSGV4          LIKE SY-MSGV4,         " Message-Variable 4

         MWSKZS         LIKE SKB1-MWSKZ,       " Steuerkategorie Sako

         QUEUE_ID       LIKE APQI-QID,         " BDC Unique Key

         SGFUNCT        LIKE RFIPI-SGFUNCT,    " Single function
         SUBRC          LIKE SY-SUBRC,         " Returncode
         TCODE          LIKE SY-TCODE,         " Transakt.Code
         UPDATE(1)      TYPE C,                " Updatemodus
         USNAM          LIKE APQI-USERID,      " Username
         STGRD          LIKE UF05A-STGRD,      " Stornogrund
         VOIDR          LIKE RF05A-VOIDR,      " Ungültigkeitsgrund
         XBDCC          LIKE RFIPI-XBDCC,      " X=BDC bei Error in C
         XKEEP          LIKE APQI-QERASE.      " X=Mappe halten

*------- Hilfsfelder ---------------------------------------------------
DATA:    ANBWA          LIKE BSEG-ANBWA.    " Anlagenbewegungsart

DATA:    BSCHL          LIKE BSEG-BSCHL,    " Buchungsschlüssel
         BUKRS          LIKE BKPF-BUKRS,    " Buchungskreis
         BLART          LIKE BKPF-BLART,    " Belegart
         BUDAT_INT      LIKE COBL-BUDAT,    " date in internal format
         BUDAT_WT       LIKE BSIS-BUDAT.

DATA:    CHAR20(20)     TYPE C.                                " QHATAX

DATA:    DEFSIZE        TYPE C,         " N849676 X=Dynpro-Standardgröße
         DEZZEICHEN(1)  TYPE C,          " Dezimalzeichen
         DEZSTELLEN(1)  TYPE N,          " Dezimalstellen
         DYNNR          LIKE TSTC-DYPNO.    " Standard-Dynpronummer

DATA:    FIXPT          LIKE TRDIR-FIXPT.   "Fixp.arithmetik
DATA:    FNAM_KONTO     LIKE BDCDATA-FNAM.    "field name      "30E

DATA:    GLFLEX_ACTIVE  TYPE XFELD,                        "Note1605537
         GROUP_OPEN(1)  TYPE C.             " X=Mappe schon geöffnet

DATA:    INDEX          LIKE SY-TFILL   .   " Tabellenindex

DATA:    JOBCOUNT       LIKE TBTCO-JOBCOUNT, " Jobnummer
         JOBNAME        LIKE TBTCO-JOBNAME. " Jobname

DATA:    KONTO          LIKE RF05A-NEWKO,   " Kontonummer (17-stellig)
         KNRZE          LIKE BBSEG-KNRZE.   " Abweichende Zentrale

DATA:    RCODE(1)       TYPE C,             " Return Code  " QHA950512
         REGUL          LIKE RF05A-REGUL.   " abweich. Regul. in Beleg

DATA:    LOOPC          LIKE SY-LOOPC.      " Loop-Zähler

DATA:    MPOOL          LIKE T019W-MPOOL.   " Modulpoolname

DATA:    RUNTIME        TYPE I.             "Runtime

DATA:    TABIX_041A      TYPE I,            " Index T041A
         TFILL_FTAB      TYPE I,            " Anz. Einträge FTAB  "KJV
         TFILL_FTC       TYPE I,            " Anz. Einträge in FTC
         TFILL_FTIBAN    TYPE I,            " Anz. Einträge in FTIBAN.
         TFILL_FTCOPA    TYPE I,            " Anz. Einträge in FTCopa
         TFILL_FTISIS    TYPE I,            " Anz. Einträge in FTISIS
         TFILL_FT_GENERIC_KONTL TYPE I,     " no. of entries ......
         TFILL_FTFKTO    TYPE I,            " Anz. Einträge in FTFKTO
         TFILL_FTK       TYPE I,            " Anz. Einträge in FTK
         TFILL_XFTTAX    TYPE I,            " Anz. Einträge in FTTAX
         TFILL_FTVK      TYPE I,            " Anz. Einträge in FTVK
         TFILL_FTVV      TYPE I,            " Anz. Einträge in FTVV
         TFILL_FTW       TYPE I,            " Anz. Einträge in FTW
         TFILL_FTZ       TYPE I,            " Anz. Einträge in FTZ
         TFILL_041A      TYPE I,            " Anz. Einträge in XT041A
         TFILL_FTSPLT    TYPE I.            " Anz. Eintrage in FTSPLT

DATA:    UMSKZ          LIKE BSEG-UMSKZ.    " Sonderumsatzkennzeichen

DATA:    WAERS          LIKE BKPF-WAERS,    " Währung
         WAERS_OLD      LIKE BKPF-WAERS,    " Währung      " QHA950512
         WINFK          LIKE T019W-WINFK,   " Window-Funktion (T019W)
         WINNR          LIKE T019W-WINNR.   " Window-Nummer

*        speichern von VBUND bei Eingabe auf Kopfebene
DATA:    VBUND          LIKE BSEG-VBUND.              "QHA941207

DATA:    XMWST          LIKE BKPF-XMWST.    " Steuer rechnen
DATA:    XMWST_SET(1)   type c.             " set BKPF-XMWST again
*---- send okcode /17 with FBV1
DATA:    SEND_OK17      TYPE FLAG.

*------- Konstanten ----------------------------------------------------
DATA:    REP_NAME(8)    TYPE C.
DATA:    REP_NAME_A(8)  TYPE C VALUE 'SAPMF05A'. " Mpool SAPMF05A
DATA:    REP_NAME_BV(8) TYPE C VALUE 'SAPLF040'. " Mpool SAPLF040 BVorEr
DATA:    REP_NAME_C(8)  TYPE C VALUE 'SAPLFCPD'. " Mpool SAPLFCPD (CPD)
DATA:    REP_NAME_IBAN(8) TYPE C VALUE 'SAPLIBMA'. " Mpool SAPLIBMA
DATA:    REP_NAME_K(8)  TYPE C VALUE 'SAPLKACB'. " Mpool SAPLKACB (CoBl)
DATA:    REP_NAME_T(8)  TYPE C VALUE 'SAPLTAX1'. " Mpool SAPLTAX1 (Taxes
DATA:    REP_NAME_R(8)  TYPE C VALUE 'SAPMF05R'. " Mpool SAPMF05R (FBRA)
DATA:    REP_NAME_V(8)  TYPE C VALUE 'SAPLF014'. " Mpool SAPLF014(VBUND)
DATA:    REP_NAME_VK(8) TYPE C VALUE 'SAPLFVI8'. " Mpool SAPLFVI8(Vermög
DATA:    REP_NAME_VV(8) TYPE C VALUE 'SAPLFVI9'. " Mpool SAPLFVI9(Vermög
DATA:    REP_NAME_AB(8) TYPE C VALUE 'SAPLAINT'. " Mpool Anl.ant. "KJV


DATA:    NO_DATE              LIKE  SY-DATUM        VALUE '        ',
         NO_TIME              LIKE  SY-UZEIT        VALUE '      '.

DATA:    BKPF_BUKRS      LIKE BKPF-BUKRS.                  "Note 641889
data:    TXKRS           like FTTAX-TXKRS.                 "note 1257048

data : l_group LIKE apqi-groupid  ,
       l_t_blntab  TYPE blntab OCCURS 0 WITH HEADER LINE ,
       l_t_ftclear TYPE ftclear OCCURS 0 WITH HEADER LINE,
       l_t_ftpost  TYPE ftpost OCCURS 0 WITH HEADER LINE,
       l_t_fttax   TYPE fttax OCCURS 0 WITH HEADER LINE,
       wa_belgeler like ZFI_ST_DENK_BELGE ,
       budat_txt(10) , bldat_txt(10) , zfbdt_txt(10) , tutar(30) ,
       zbd1t(3),
       zbd2t(3),
       zbd3t(3),
       denktutar type dmbtr ,
       bakiye .
data : p_mode type mode VALUE 'N' .
DATA : MSG type string .
