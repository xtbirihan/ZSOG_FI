REPORT /saptr/fi_r_kdv_b_raporu
       LINE-SIZE 255
       LINE-COUNT 65
       MESSAGE-ID /saptr/fi.

INCLUDE zsaptr_fi_i_kdv_b_raporu.

SELECTION-SCREEN BEGIN OF BLOCK mainsel WITH FRAME.
SELECT-OPTIONS : s_bukrs FOR bkpf-bukrs OBLIGATORY MODIF ID msl
                        MEMORY ID buk,
                 s_belnr FOR bkpf-belnr,
                 s_gjahr FOR bkpf-gjahr NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK mainsel.

SELECTION-SCREEN: PUSHBUTTON /1(79) pushb_o2
                  USER-COMMAND ucomm_o2 MODIF ID mo2,
                  PUSHBUTTON /1(79) pushb_c2
                  USER-COMMAND ucomm_c2 MODIF ID mc2.

SELECTION-SCREEN BEGIN OF BLOCK furtsel WITH FRAME.
PARAMETERS     : p_mnbvol TYPE /saptr/mnbvol
                      DEFAULT 8000 OBLIGATORY MODIF ID mc2.
SELECT-OPTIONS : s_budat FOR bkpf-budat MODIF ID mc2,
                 s_bldat FOR bkpf-bldat MODIF ID mc2,
                 s_monat FOR bkpf-monat MODIF ID mc2 NO-EXTENSION.
SELECT-OPTIONS : s_mwskz FOR bset-mwskz MODIF ID mc2,
                 s_ktosl FOR sktosl MODIF ID mc2,
                 s_kunnr FOR kna1-kunnr MODIF ID mc2,
                 s_lifnr FOR lfa1-lifnr MODIF ID mc2,
                 s_blart FOR bkpf-blart MODIF ID mc2.
PARAMETERS     : p_outtx TYPE /saptr/outtx MODIF ID mc2,
                 p_inptx TYPE /saptr/inptx MODIF ID mc2.
SELECTION-SCREEN END OF BLOCK furtsel.

SELECTION-SCREEN: PUSHBUTTON /1(79) pushb_o1
                  USER-COMMAND ucomm_o1 MODIF ID mo1,
                  PUSHBUTTON /1(79) pushb_c1
                  USER-COMMAND ucomm_c1 MODIF ID mc1.

SELECTION-SCREEN BEGIN OF BLOCK outpsel WITH FRAME.
PARAMETERS     : p_stcd2 TYPE /saptr/rbstcd RADIOBUTTON GROUP taxf
                  DEFAULT 'X' MODIF ID mc1,
                 p_stcd1 TYPE /saptr/rbstcd
                  RADIOBUTTON GROUP taxf MODIF ID mc1.

SELECT-OPTIONS : s_fzcust FOR fzcust MATCHCODE OBJECT
                          debi MODIF ID mc1,
                 s_fzvend FOR fzvend MATCHCODE OBJECT
                          kred_c MODIF ID mc1.
PARAMETERS     : c_saveba AS CHECKBOX MODIF ID mc1,
                 p_fnamba LIKE rlgrap-filename MODIF ID mc1,
                 c_savebs AS CHECKBOX MODIF ID mc1,
                 p_fnambs LIKE rlgrap-filename MODIF ID mc1.
SELECTION-SCREEN END OF BLOCK outpsel.

SELECTION-SCREEN: PUSHBUTTON /1(79) pushb_o4
                  USER-COMMAND ucomm_o4 MODIF ID mo4,
                  PUSHBUTTON /1(79) pushb_c4
                  USER-COMMAND ucomm_c4 MODIF ID mc4.

SELECTION-SCREEN BEGIN OF BLOCK outplst WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-030 MODIF ID mc4.
PARAMETERS pbsdchk AS CHECKBOX DEFAULT 'X' MODIF ID mc4.
SELECTION-SCREEN COMMENT 35(1) text-029 MODIF ID mc4.
PARAMETERS pvaribsd LIKE disvariant-variant MODIF ID mc4.
SELECTION-SCREEN COMMENT 50(1) text-029 MODIF ID mc4.
SELECTION-SCREEN PUSHBUTTON 52(15) text-055
 USER-COMMAND pbsdbut MODIF ID mc4.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-031 MODIF ID mc4.
PARAMETERS pbshchk AS CHECKBOX DEFAULT 'X' MODIF ID mc4.
SELECTION-SCREEN COMMENT 35(1) text-029 MODIF ID mc4.
PARAMETERS pvaribsh LIKE disvariant-variant MODIF ID mc4.
SELECTION-SCREEN COMMENT 50(1) text-029 MODIF ID mc4.
SELECTION-SCREEN PUSHBUTTON 52(15) text-055
 USER-COMMAND pbshbut MODIF ID mc4.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-032 MODIF ID mc4.
PARAMETERS pbadchk AS CHECKBOX DEFAULT 'X' MODIF ID mc4.
SELECTION-SCREEN COMMENT 35(1) text-029 MODIF ID mc4.
PARAMETERS pvaribad LIKE disvariant-variant MODIF ID mc4.
SELECTION-SCREEN COMMENT 50(1) text-029 MODIF ID mc4.
SELECTION-SCREEN PUSHBUTTON 52(15) text-055
 USER-COMMAND pbadbut MODIF ID mc4.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-033 MODIF ID mc4.
PARAMETERS pbahchk AS CHECKBOX DEFAULT 'X' MODIF ID mc4.
SELECTION-SCREEN COMMENT 35(1) text-029 MODIF ID mc4.
PARAMETERS pvaribah LIKE disvariant-variant MODIF ID mc4.
SELECTION-SCREEN COMMENT 50(1) text-029 MODIF ID mc4.
SELECTION-SCREEN PUSHBUTTON 52(15) text-055
 USER-COMMAND pbahbut MODIF ID mc4.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-034 MODIF ID mc4.
PARAMETERS pbsdchko AS CHECKBOX DEFAULT 'X' MODIF ID mc4.
SELECTION-SCREEN COMMENT 35(1) text-029 MODIF ID mc4.
PARAMETERS pvarbsdo LIKE disvariant-variant MODIF ID mc4.
SELECTION-SCREEN COMMENT 50(1) text-029 MODIF ID mc4.
SELECTION-SCREEN PUSHBUTTON 52(15) text-055
 USER-COMMAND pbsdbuto MODIF ID mc4.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-035 MODIF ID mc4.
PARAMETERS pbshchko AS CHECKBOX DEFAULT 'X' MODIF ID mc4.
SELECTION-SCREEN COMMENT 35(1) text-029 MODIF ID mc4.
PARAMETERS pvarbsho LIKE disvariant-variant MODIF ID mc4.
SELECTION-SCREEN COMMENT 50(1) text-029 MODIF ID mc4.
SELECTION-SCREEN PUSHBUTTON 52(15) text-055
 USER-COMMAND pbshbuto MODIF ID mc4.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-036 MODIF ID mc4.
PARAMETERS pbadchko AS CHECKBOX DEFAULT 'X' MODIF ID mc4.
SELECTION-SCREEN COMMENT 35(1) text-029 MODIF ID mc4.
PARAMETERS pvarbado LIKE disvariant-variant MODIF ID mc4.
SELECTION-SCREEN COMMENT 50(1) text-029 MODIF ID mc4.
SELECTION-SCREEN PUSHBUTTON 52(15) text-055
 USER-COMMAND pbadbuto MODIF ID mc4.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-037 MODIF ID mc4.
PARAMETERS pbahchko AS CHECKBOX DEFAULT 'X' MODIF ID mc4.
SELECTION-SCREEN COMMENT 35(1) text-029 MODIF ID mc4.
PARAMETERS pvarbaho LIKE disvariant-variant MODIF ID mc4.
SELECTION-SCREEN COMMENT 50(1) text-029 MODIF ID mc4.
SELECTION-SCREEN PUSHBUTTON 52(15) text-055
 USER-COMMAND pbahbuto MODIF ID mc4.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK outplst.

SELECTION-SCREEN: PUSHBUTTON /1(79) pushb_o3
                  USER-COMMAND ucomm_o3 MODIF ID mo3,
                  PUSHBUTTON /1(79) pushb_c3
                  USER-COMMAND ucomm_c3 MODIF ID mc3.
SELECTION-SCREEN BEGIN OF BLOCK taxpayer WITH FRAME.
PARAMETERS : tpitoc TYPE /saptr/tpitoc MODIF ID mc3,
             tpitno TYPE /saptr/tpitno MODIF ID mc3,
             tpisnm TYPE /saptr/tpisnm MODIF ID mc3,
             tpinam TYPE /saptr/tpinam MODIF ID mc3,
             tpitrn TYPE /saptr/tpitrn MODIF ID mc3,
             tpiema TYPE /saptr/tpiema MODIF ID mc3,
             tpitac TYPE /saptr/tpitac MODIF ID mc3,
             tpitel TYPE /saptr/tpitel MODIF ID mc3.
SELECTION-SCREEN END OF BLOCK taxpayer.

PARAMETERS : par_cb1(1) TYPE c NO-DISPLAY,
             par_cb2(1) TYPE c NO-DISPLAY,
             par_cb3(1) TYPE c NO-DISPLAY,
             par_cb4(1) TYPE c NO-DISPLAY,
             par_cb5(1) TYPE c NO-DISPLAY,
             par_avpn(1) TYPE c NO-DISPLAY.

INITIALIZATION.
  p_inptx = 'X'.
  p_outtx = 'X'.

  s_ktosl-sign = 'I'.
  s_ktosl-option = 'EQ'.
  s_ktosl-low = 'MWS'.
  APPEND s_ktosl.
  s_ktosl-low = 'VST'.
  APPEND s_ktosl.

  gs_variant-report = sy-repid.
  par_cb1 = 'X'.
  par_cb3 = 'X'.
  par_cb4 = 'X'.


  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_expand
      text   = text-051
    IMPORTING
      result = pushb_o1
    EXCEPTIONS
      OTHERS = 3.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_collapse
      text   = text-051
    IMPORTING
      result = pushb_c1
    EXCEPTIONS
      OTHERS = 3.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_expand
      text   = text-052
    IMPORTING
      result = pushb_o2
    EXCEPTIONS
      OTHERS = 3.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_collapse
      text   = text-052
    IMPORTING
      result = pushb_c2
    EXCEPTIONS
      OTHERS = 3.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_expand
      text   = text-053
    IMPORTING
      result = pushb_o3
    EXCEPTIONS
      OTHERS = 3.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_collapse
      text   = text-053
    IMPORTING
      result = pushb_c3
    EXCEPTIONS
      OTHERS = 3.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_expand
      text   = text-054
    IMPORTING
      result = pushb_o4
    EXCEPTIONS
      OTHERS = 3.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_collapse
      text   = text-054
    IMPORTING
      result = pushb_c4
    EXCEPTIONS
      OTHERS = 3.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR pvaribsd.
  PERFORM alv_variante_f4 USING 'BSD'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pvaribsh.
  PERFORM alv_variante_f4 USING 'BSH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pvaribad.
  PERFORM alv_variante_f4 USING 'BAD'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pvaribah.
  PERFORM alv_variante_f4 USING 'BAH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pvarbsdo.
  PERFORM alv_variante_f4 USING 'BSDO'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pvarbsho.
  PERFORM alv_variante_f4 USING 'BSHO'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pvarbado.
  PERFORM alv_variante_f4 USING 'BADO'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pvarbaho.
  PERFORM alv_variante_f4 USING 'BAHO'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fnamba.
  PERFORM get_filename_f4 USING p_fnamba.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fnambs.
  PERFORM get_filename_f4 USING p_fnambs.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    PERFORM close_block USING: par_cb1 'MC1' space,
                               par_cb1 'MO1' 'X',
                               par_cb2 'MC2' space,
                               par_cb2 'MO2' 'X',
                               par_cb3 'MC3' space,
                               par_cb3 'MO3' 'X',
                               par_cb4 'MC4' space,
                               par_cb4 'MO4' 'X'.
  ENDLOOP.

AT SELECTION-SCREEN.

  g_sscr_ucomm = sscrfields-ucomm.
  CASE sscrfields-ucomm.
    WHEN 'UCOMM_O1'. CLEAR par_cb1.
    WHEN 'UCOMM_C1'. par_cb1 = 'X'.
    WHEN 'UCOMM_O2'. CLEAR par_cb2.
    WHEN 'UCOMM_C2'. par_cb2 = 'X'.
    WHEN 'UCOMM_O3'. CLEAR par_cb3.
    WHEN 'UCOMM_C3'. par_cb3 = 'X'.
    WHEN 'UCOMM_O4'. CLEAR par_cb4.
    WHEN 'UCOMM_C4'. par_cb4 = 'X'.

    WHEN 'PBSDBUT'. PERFORM cfg_variant USING '1'.
    WHEN 'PBSHBUT'. PERFORM cfg_variant USING '2'.
    WHEN 'PBADBUT'. PERFORM cfg_variant USING '3'.
    WHEN 'PBAHBUT'. PERFORM cfg_variant USING '4'.
    WHEN 'PBSDBUTO'. PERFORM cfg_variant USING '5'.
    WHEN 'PBSHBUTO'. PERFORM cfg_variant USING '6'.
    WHEN 'PBADBUTO'. PERFORM cfg_variant USING '7'.
    WHEN 'PBAHBUTO'. PERFORM cfg_variant USING '8'.
  ENDCASE.
  PERFORM chk_selfields.

START-OF-SELECTION.
  COMMIT WORK AND WAIT.
  PERFORM set_initial_values.
  PERFORM get_bset_lines.
  PERFORM del_bset_revers.
  PERFORM process_bset.
  PERFORM badi_modify.
  PERFORM del_min_volumes.
  PERFORM chk_err.
  PERFORM dsp_forms.

*&--------------------------------------------------------------------*
*&      Form  chk_selfields
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM chk_selfields.
  DATA: lv_waers TYPE waers,
        lv_periv TYPE periv.

* Check currency, fiscal year variant and country of company code
  CLEAR: lv_waers, lv_periv.
  SELECT * FROM t001 WHERE bukrs IN s_bukrs.

    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
                ID 'BUKRS' FIELD t001-bukrs
                ID 'ACTVT' FIELD '03'.
    IF sy-subrc NE 0.
      MESSAGE e800(fr) WITH t001-bukrs RAISING not_authorized.
    ENDIF.
    IF t001-land1 <> 'TR'.
      MESSAGE e012.
      EXIT.
    ENDIF.
    IF lv_waers IS INITIAL.
      lv_waers = t001-waers.
    ELSE.
      IF t001-waers <> lv_waers.
        MESSAGE e013.
        EXIT.
      ENDIF.
    ENDIF.
    IF lv_periv IS INITIAL.
      lv_periv = t001-periv.
    ELSE.
      IF t001-periv <> lv_periv.
        MESSAGE e014.
        EXIT.
      ENDIF.
    ENDIF.
  ENDSELECT.

  IF s_budat-low(4) <> s_budat-high(4) AND s_budat-high NE 0.
    SET CURSOR FIELD 'S_BUDAT-LOW'.
    MESSAGE e017.
  ENDIF.

  IF NOT s_budat IS INITIAL AND NOT s_gjahr IS INITIAL AND
    s_budat-low(4) <> s_gjahr-low.
    MESSAGE e019.
  ENDIF.

  IF s_monat-low <> 0 OR s_monat-high <> 0.
    IF s_gjahr-low = 0.
      SET CURSOR FIELD 'S_GJAHR-LOW'.
      MESSAGE e015.
    ENDIF.
  ENDIF.

  IF s_gjahr-low >= '2008' AND s_monat-low <> s_monat-high AND s_monat-high NE 0.
    SET CURSOR FIELD 'S_MONAT-LOW'.
    MESSAGE e018.
  ENDIF.

  IF s_budat-low(4) >= '2008' AND s_budat-low+4(2) <> s_budat-high+4(2) AND s_budat-high NE 0.
    SET CURSOR FIELD 'S_BUDAT-LOW'.
    MESSAGE e018.
  ENDIF.

  IF s_monat-low EQ 0 AND s_monat-high EQ 0 AND
     s_bldat-low EQ 0 AND s_bldat-high EQ 0 AND
     s_budat-low EQ 0 AND s_budat-high EQ 0.
    MESSAGE e009.
  ENDIF.

ENDFORM.                    "chk_selfields

*&--------------------------------------------------------------------*
*&      Form  PROCESS_BSET
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM process_bset.
  DATA: BEGIN OF ibp OCCURS 0,
          lifnr LIKE lfa1-lifnr,
          kunnr LIKE kna1-kunnr,
          name1 LIKE lfa1-name1,
          name2 LIKE lfa1-name2,
          stcd1 LIKE lfa1-stcd1,
          stcd2 LIKE lfa1-stcd2,
          stcd3 LIKE lfa1-stcd3,
          stkzn LIKE lfa1-stkzn,
          land1 LIKE lfa1-land1,
        END OF ibp.
  DATA : name1 LIKE lfa1-name1,
         name2 LIKE lfa1-name2,
         stcd1 LIKE lfa1-stcd1,
         stcd2 LIKE lfa1-stcd2,
         stcd3 LIKE lfa1-stcd3,
         stkzn LIKE lfa1-stkzn,
         land1 LIKE lfa1-land1,
         ktokk LIKE lfa1-ktokk,
         ktokd LIKE kna1-ktokd,
         xcpds LIKE t077k-xcpds,
         landa LIKE t005-landa,
         landx LIKE t005t-landx,
         xnof  LIKE vbapf-anzfkp_iv,
         i TYPE i.


  LOOP AT ibset INTO wbset.
    CLEAR xnof.
    ON CHANGE OF wbset-bukrs OR wbset-belnr OR wbset-gjahr.
      xnof = 1.
    ENDON.
    READ TABLE it007a WITH KEY mwskz = wbset-mwskz.
    IF sy-subrc NE 0 OR
      ( it007a-mwart = 'A' AND p_outtx = space ) OR
      ( it007a-mwart = 'V' AND p_inptx = space ).
      DELETE TABLE ibset WITH TABLE KEY bukrs = wbset-bukrs
                                        belnr = wbset-belnr
                                        gjahr = wbset-gjahr
                                        buzei = wbset-buzei.
      CONTINUE.
    ENDIF.
    CLEAR: ibp[], ibp.
    LOOP AT ibsik INTO wbsik
         WHERE bukrs EQ wbset-bukrs AND
               belnr EQ wbset-belnr AND
               gjahr EQ wbset-gjahr.
      CLEAR: name1, name2, stcd1, stcd2, stcd3, stkzn, land1, ktokk.
      SELECT SINGLE name1 name2 stcd1 stcd2 stcd3 stkzn land1 ktokk
        INTO (name1, name2, stcd1, stcd2, stcd3, stkzn, land1, ktokk)
        FROM lfa1
             WHERE lifnr = wbsik-lifnr.
      IF sy-subrc = 0.
        CLEAR xcpds.
        SELECT SINGLE xcpds
          INTO xcpds
          FROM t077k
               WHERE ktokk EQ ktokk.
        IF xcpds = 'X'.
          SELECT SINGLE name1 name2 stcd1 stcd2 stcd3 stkzn land1
          INTO (name1, name2, stcd1, stcd2, stcd3, stkzn, land1)
            FROM bsec
                 WHERE bukrs = wbsik-bukrs
                   AND belnr = wbsik-belnr
                   AND gjahr = wbsik-gjahr
                   AND buzei = wbsik-buzei.
        ENDIF.
        ibp-lifnr = wbsik-lifnr.
        ibp-kunnr = ''.
        ibp-name1 = name1.
        ibp-name2 = name2.
        ibp-stcd1 = stcd1.
        ibp-stcd2 = stcd2.
        ibp-stcd3 = stcd3.
        ibp-stkzn = stkzn.
        ibp-land1 = land1.
        APPEND ibp.
      ENDIF.
    ENDLOOP.
    LOOP AT ibsid INTO wbsid
         WHERE bukrs EQ wbset-bukrs AND
               belnr EQ wbset-belnr AND
               gjahr EQ wbset-gjahr.
      CLEAR: name1, name2, stcd1, stcd2, stcd3, stkzn, land1, ktokd.
      SELECT SINGLE name1 name2 stcd1 stcd2 stcd3 stkzn land1 ktokd
        INTO (name1, name2, stcd1, stcd2, stcd3, stkzn, land1, ktokd)
        FROM kna1
             WHERE kunnr = wbsid-kunnr.
      IF sy-subrc = 0.
        SELECT SINGLE xcpds
          INTO xcpds
          FROM t077d
               WHERE ktokd EQ ktokd.
        IF xcpds = 'X'.
          SELECT SINGLE name1 name2 stcd1 stcd2 stcd3 stkzn land1
            INTO (name1, name2, stcd1, stcd2, stcd3, stkzn, land1)
            FROM bsec
                 WHERE bukrs = wbsid-bukrs
                   AND belnr = wbsid-belnr
                   AND gjahr = wbsid-gjahr
                   AND buzei = wbsid-buzei.
        ENDIF.
        CLEAR ibp.
        ibp-kunnr = wbsid-kunnr.
        ibp-lifnr = ''.
        ibp-name1 = name1.
        ibp-name2 = name2.
        ibp-stcd1 = stcd1.
        ibp-stcd2 = stcd2.
        ibp-stcd3 = stcd3.
        ibp-stkzn = stkzn.
        ibp-land1 = land1.
        APPEND ibp.
      ENDIF.
    ENDLOOP.
    SORT ibp.
    DELETE ADJACENT DUPLICATES FROM ibp.


*{   ->>> Added by Prodea Ozan Şahin - 07.08.2019 15:21:27
    LOOP AT ibp.
      SELECT SINGLE COUNT( * )
      FROM lfa1
      WHERE ktokk = 'SGYD'
        AND lifnr = ibp-lifnr.
      IF sy-subrc = 0.

        DELETE ibp WHERE lifnr = ibp-lifnr.
      ENDIF.
    ENDLOOP.
*}     <<<- End of  Added - 07.08.2019 15:21:27


    DESCRIBE TABLE ibp LINES i.
    IF i > 1.
      MOVE 'MULTI_BP' TO wbset-name1.
      APPEND wbset TO ebset.
      DELETE TABLE ibset WITH TABLE KEY bukrs = wbset-bukrs
                                        belnr = wbset-belnr
                                        gjahr = wbset-gjahr
                                        buzei = wbset-buzei.
      CONTINUE.
*    ELSEIF i = 0.
*      MOVE 'NO_BP' TO wbset-name1.
*      APPEND wbset TO ebset.
*      DELETE TABLE ibset WITH TABLE KEY bukrs = wbset-bukrs
*                                        belnr = wbset-belnr
*                                        gjahr = wbset-gjahr
*                                        buzei = wbset-buzei.
*      CONTINUE.



    ELSE.
      CLEAR ibp.
      LOOP AT ibp.
        MOVE: ibp-lifnr TO wbset-lifnr,
              ibp-kunnr TO wbset-kunnr,
              ibp-name1 TO wbset-name1,
              ibp-name2 TO wbset-name2,
              ibp-land1 TO wbset-land1,
              ibp-stcd1 TO wbset-stcd1,
              ibp-stcd2 TO wbset-stcd2,
              ibp-stcd3 TO wbset-stcd3,
              ibp-stkzn TO wbset-stkzn.
      ENDLOOP.

    ENDIF.
    IF wbset-shkzg EQ 'H'.
      wbset-hwbas = - wbset-hwbas.
      wbset-hwste = - wbset-hwste.
    ENDIF.
    CLEAR: landa, landx.
    IF ( wbset-lifnr IN s_fzvend AND NOT s_fzvend[] IS INITIAL
         AND NOT wbset-lifnr IS INITIAL ) OR
       ( wbset-kunnr IN s_fzcust AND NOT s_fzcust[] IS INITIAL
         AND NOT wbset-kunnr IS INITIAL ).
      landa = '960'.
    ELSE.
      SELECT SINGLE landa INTO landa
               FROM t005
              WHERE land1 EQ wbset-land1.
    ENDIF.
    SELECT SINGLE landx INTO landx
             FROM t005t
            WHERE spras EQ sy-langu AND
                  land1 EQ wbset-land1.
    IF it007a-mwart = 'A'.
      IF ( wbset-hwbas GT 0 AND wbset-hwste LT 0 ) OR
         ( wbset-hwbas LT 0 AND wbset-hwste GT 0 ).
        wbset-hwbas = - wbset-hwbas.
      ENDIF.
      wbset-hwbas = - wbset-hwbas.
      wbset-hwste = - wbset-hwste.
      CLEAR itabbsd.
      MOVE-CORRESPONDING wbset TO itabbsd.
      itabbsd-landa = landa.
      itabbsd-landx = landx.
      IF wbset-land1 NE 'TR'.
        itabbsd-stcd1 = '2222222222'.
        itabbsd-stcd3 = '2222222222'.
      ELSEIF p_stcd2 EQ 'X'.
        itabbsd-stcd1 = wbset-stcd2.
      ENDIF.
      IF wbset-stkzn = 'X'.
        itabbsd-stcd3 = itabbsd-stcd1.
        CLEAR itabbsd-stcd1.
      ENDIF.
      COLLECT itabbsd.
      MOVE-CORRESPONDING itabbsd TO itabbsh.
      itabbsh-nofdc = xnof.
      COLLECT itabbsh.
    ELSEIF it007a-mwart = 'V'.
      IF ( wbset-hwbas GT 0 AND wbset-hwste LT 0 ) OR
         ( wbset-hwbas LT 0 AND wbset-hwste GT 0 ).
        wbset-hwbas = - wbset-hwbas.
      ENDIF.
      MOVE-CORRESPONDING wbset TO itabbad.
      itabbad-landa = landa.
      itabbad-landx = landx.
      IF wbset-land1 NE 'TR'.
        itabbad-stcd1 = '1111111111'.
        itabbad-stcd3 = '1111111111'.
      ELSEIF p_stcd2 EQ 'X'.
        itabbad-stcd1 = wbset-stcd2.
      ENDIF.
      IF wbset-stkzn = 'X'.
        itabbad-stcd3 = itabbad-stcd1.
        CLEAR itabbad-stcd1.
      ENDIF.
      COLLECT itabbad.
      MOVE-CORRESPONDING itabbad TO itabbah.
      itabbah-nofdc = xnof.
      COLLECT itabbah.
    ENDIF.
  ENDLOOP.
  REFRESH ibset.
  FREE ibset.
  COMMIT WORK.
ENDFORM.                    "PROCESS_BSET
*---------------------------------------------------------------------*
*       FORM del_min_volumes                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM del_min_volumes.
  DATA : cou LIKE itabbsh-sortn,
         cou_o LIKE itabbsh-sortn.
  cou = 1.
  cou_o = 1.
  SORT itabbsh BY hwbas DESCENDING.
  SORT itabbah BY hwbas DESCENDING.
  CLEAR: itabbsh_o, itabbah_o, itabbsd_o, itabbad_o.
  REFRESH: itabbsh_o, itabbah_o, itabbsd_o, itabbad_o.
  LOOP AT itabbsh.
    IF itabbsh-hwbas LT p_mnbvol.
      cou_o = cou_o + 1.
      itabbsh_o-sortn = cou_o.
      MOVE itabbsh TO itabbsh_o.
      APPEND itabbsh_o.
      LOOP AT itabbsd WHERE name1 EQ itabbsh-name1
                      AND stcd1 EQ itabbsh-stcd1
                      AND landa EQ itabbsh-landa.
        MOVE itabbsd TO itabbsd_o.
        APPEND itabbsd_o.
        DELETE itabbsd.
      ENDLOOP.
      DELETE itabbsh.
    ELSE.
      itabbsh-sortn = cou.
      MODIFY itabbsh TRANSPORTING sortn.
      cou = cou + 1.
    ENDIF.
  ENDLOOP.
  cou = 1.
  cou_o = 1.
  LOOP AT itabbah.
    IF itabbah-hwbas LT p_mnbvol.
      cou_o = cou_o + 1.
      itabbah_o-sortn = cou_o.
      MOVE itabbah TO itabbah_o.
      APPEND itabbah_o.
      LOOP AT itabbad WHERE name1 EQ itabbah-name1
                      AND stcd1 EQ itabbah-stcd1
                      AND landa EQ itabbah-landa.
        MOVE itabbad TO itabbad_o.
        APPEND itabbad_o.
        DELETE itabbad.
      ENDLOOP.
      DELETE itabbah.
    ELSE.
      itabbah-sortn = cou.
      MODIFY itabbah TRANSPORTING sortn.
      cou = cou + 1.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "del_min_volumes

*&--------------------------------------------------------------------*
*&      Form  dsp_forms
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM dsp_forms.
  PERFORM create_gt_excluding.
  IF sy-batch EQ 'X'.
    PERFORM list_error.
  ENDIF.
  SORT itabbsd[] BY lifnr kunnr bukrs belnr gjahr.
  SORT itabbad[] BY kunnr lifnr bukrs belnr gjahr.
  SORT itabbsd_o[] BY lifnr kunnr bukrs belnr gjahr.
  SORT itabbad_o[] BY kunnr lifnr bukrs belnr gjahr.
  IF flg_notp = 'X'.
    PERFORM dsp_all.
  ELSEIF var_avp1 = 'X'.
    REFRESH gt_alv.
    APPEND gt_alv.
    READ TABLE gt_alv INDEX 1 ASSIGNING <gt_alv>.
    PERFORM dsp_bsd.
  ELSEIF var_avp2 = 'X'.
    REFRESH gt_alv.
    APPEND gt_alv.
    READ TABLE gt_alv INDEX 1 ASSIGNING <gt_alv>.
    PERFORM dsp_bsh.
  ELSEIF var_avp3 = 'X'.
    REFRESH gt_alv.
    APPEND gt_alv.
    READ TABLE gt_alv INDEX 1 ASSIGNING <gt_alv>.
    PERFORM dsp_bad.
  ELSEIF var_avp4 = 'X'.
    REFRESH gt_alv.
    APPEND gt_alv.
    READ TABLE gt_alv INDEX 1 ASSIGNING <gt_alv>.
    PERFORM dsp_bah.
  ELSEIF var_avp5 = 'X'.
    REFRESH gt_alv.
    APPEND gt_alv.
    READ TABLE gt_alv INDEX 1 ASSIGNING <gt_alv>.
    PERFORM dsp_bsd_o.
  ELSEIF var_avp6 = 'X'.
    REFRESH gt_alv.
    APPEND gt_alv.
    READ TABLE gt_alv INDEX 1 ASSIGNING <gt_alv>.
    PERFORM dsp_bsh_o.
  ELSEIF var_avp7 = 'X'.
    REFRESH gt_alv.
    APPEND gt_alv.
    READ TABLE gt_alv INDEX 1 ASSIGNING <gt_alv>.
    PERFORM dsp_bad_o.
  ELSEIF var_avp8 = 'X'.
    REFRESH gt_alv.
    APPEND gt_alv.
    READ TABLE gt_alv INDEX 1 ASSIGNING <gt_alv>.
    PERFORM dsp_bah_o.
  ENDIF.
  IF sy-batch EQ 'X'.
    IF c_savebs = 'X'.
      PERFORM cnv_bs_to_xml.
    ENDIF.
    IF c_saveba = 'X'.
      PERFORM cnv_ba_to_xml.
    ENDIF.
  ENDIF.
ENDFORM.                    "dsp_forms

*&--------------------------------------------------------------------*
*&      Form  dsp_all
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM dsp_all.
  STATICS: s_index TYPE sy-index VALUE '1'.   "loop counter
  REFRESH gt_alv.
  gt_alv-itabbsd[] = itabbsd[].
  gt_alv-itabbsh[] = itabbsh[].
  gt_alv-itabbad[] = itabbad[].
  gt_alv-itabbah[] = itabbah[].
  gt_alv-itabbsd_o[] = itabbsd_o[].
  gt_alv-itabbsh_o[] = itabbsh_o[].
  gt_alv-itabbad_o[] = itabbad_o[].
  gt_alv-itabbah_o[] = itabbah_o[].
  APPEND gt_alv.
  flg_notp = 'X'.
  READ TABLE gt_alv INTO wt_alv INDEX s_index.
  IF sy-subrc = 0.                     "entry found
    PERFORM new-section(rsbtchh0).     "separate list
    s_index = s_index + 1.
    PERFORM dsp_bsd.
  ELSEIF s_index > 1.                  "there were entreis
    s_index = 1.                       "reset for ALV
  ENDIF.
ENDFORM.                    "dsp_all

*&--------------------------------------------------------------------*
*&      Form  dsp_bsd
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM dsp_bsd.
  flg_summ = 'A'.
  PERFORM print_table
            TABLES
               wt_alv-itabbsd
            USING
               pbsdchk
               var_avp1
               c_bsd
               pvaribsd
               space
               space
               'DSP_BSH'
               'TOP_OF_PAGE_BSD'
               'CREATE_FIELDCAT_BSD'.

ENDFORM.                    "dsp_bsd

*&--------------------------------------------------------------------*
*&      Form  dsp_bsh
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM dsp_bsh.
  flg_summ = 'B'.
  PERFORM print_table
            TABLES
               wt_alv-itabbsh
            USING
               pbshchk
               var_avp2
               c_bsh
               pvaribsh
               space
               space
               'DSP_BAD'
               'TOP_OF_PAGE_BSH'
               'CREATE_FIELDCAT_BSH'.

ENDFORM.                    "dsp_bsh

*&--------------------------------------------------------------------*
*&      Form  dsp_bad
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM dsp_bad.
  flg_summ = 'C'.
  PERFORM print_table
            TABLES
               wt_alv-itabbad
            USING
               pbadchk
               var_avp3
               c_bad
               pvaribad
               space
               space
               'DSP_BAH'
               'TOP_OF_PAGE_BAD'
               'CREATE_FIELDCAT_BAD'.

ENDFORM.                    "dsp_bad

*&--------------------------------------------------------------------*
*&      Form  dsp_bah
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM dsp_bah.
  flg_summ = 'D'.
  PERFORM print_table
            TABLES
               wt_alv-itabbah
            USING
               pbahchk
               var_avp4
               c_bah
               pvaribah
               space
               space
               'DSP_BSD_O'
               'TOP_OF_PAGE_BAH'
               'CREATE_FIELDCAT_BAH'.

ENDFORM.                    "dsp_bah

*&--------------------------------------------------------------------*
*&      Form  dsp_bsd_o
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM dsp_bsd_o.
  flg_summ = 'E'.
  PERFORM print_table
            TABLES
               wt_alv-itabbsd_o
            USING
               pbsdchko
               var_avp5
               c_bsdo
               pvarbsdo
               space
               space
               'DSP_BSH_O'
               'TOP_OF_PAGE_BSD_O'
               'CREATE_FIELDCAT_BSD'.

ENDFORM.                    "dsp_bsd_o

*&--------------------------------------------------------------------*
*&      Form  dsp_bsh_o
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM dsp_bsh_o.
  flg_summ = 'F'.
  PERFORM print_table
            TABLES
               wt_alv-itabbsh_o
            USING
               pbshchko
               var_avp6
               c_bsho
               pvarbsho
               space
               space
               'DSP_BAD_O'
               'TOP_OF_PAGE_BSH_O'
               'CREATE_FIELDCAT_BSH'.

ENDFORM.                    "dsp_bsh_o

*&--------------------------------------------------------------------*
*&      Form  dsp_bad_o
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM dsp_bad_o.
  flg_summ = 'G'.
  PERFORM print_table
            TABLES
               wt_alv-itabbad_o
            USING
               pbadchko
               var_avp7
               c_bado
               pvarbado
               space
               space
               'DSP_BAH_O'
               'TOP_OF_PAGE_BAD_O'
               'CREATE_FIELDCAT_BAD'.

ENDFORM.                    "dsp_bad_o

*&--------------------------------------------------------------------*
*&      Form  dsp_bah_o
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM dsp_bah_o.
  flg_summ = 'H'.
  PERFORM print_table
            TABLES
               wt_alv-itabbah_o
            USING
               pbahchko
               var_avp8
               c_baho
               pvarbaho
               space
               space
               'DSP_ALL'
               'TOP_OF_PAGE_BAH_O'
               'CREATE_FIELDCAT_BAH'.

ENDFORM.                    "dsp_bah_o

*&--------------------------------------------------------------------*
*&      Form  cfg_variant
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->CH         text
*---------------------------------------------------------------------*
FORM cfg_variant USING ch.
  SUBMIT (g_repid) WITH s_bukrs  EQ s_bukrs
                   WITH s_lifnr  EQ 'Z'
                   WITH s_kunnr  EQ 'Z'
                   WITH s_umskz  EQ 'Z'
                   WITH s_gjahr  EQ '1955'
                   WITH s_monat  EQ '01'
                   WITH s_budat  EQ '99991201'
                   WITH s_bldat  EQ '99991201'
                   WITH pbsdchk  EQ 'X'
                   WITH pvaribsd EQ pvaribsd
                   WITH par_avpn EQ ch
                   AND RETURN.
ENDFORM.                    "cfg_variant

*&--------------------------------------------------------------------*
*&      Form  chk_ibel_err
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM chk_err.
  LOOP AT ebset INTO wbset.
    IF wbset-name1 = 'MULTI_BP'.
      PERFORM add_mess USING 005 'E' wbset-bukrs wbset-belnr wbset-gjahr ''.
    ELSEIF wbset-name1 = 'NO_BP'.
      PERFORM add_mess USING 016 'E' wbset-bukrs wbset-belnr wbset-gjahr ''.
    ENDIF.
  ENDLOOP.
  IF sy-subrc = 0.
    MESSAGE i000.
  ENDIF.
ENDFORM.                    "chk_ibel_err

*&--------------------------------------------------------------------*
*&      Form  add_mess
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->MSGNO      text
*      -->MSGTY      text
*      -->V1         text
*      -->V2         text
*      -->V3         text
*      -->V4         text
*---------------------------------------------------------------------*
FORM add_mess USING msgno msgty v1 v2 v3 v4.
  DATA: ls_msg TYPE bal_s_msg.
  ls_msg-msgid     = '/SAPTR/FI'.
  ls_msg-msgno     = msgno.
  ls_msg-msgv1     = v1.
  ls_msg-msgv2     = v2.
  ls_msg-msgv3     = v3.
  ls_msg-msgv4     = v4.
  ls_msg-probclass = 2.
  ls_msg-msgty     = msgty.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_s_msg       = ls_msg
    EXCEPTIONS
      log_not_found = 0
      OTHERS        = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
               sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "ADD_MESS


*&--------------------------------------------------------------------*
*&      Form  del_bset_revers
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM del_bset_revers.
  DATA: bukrs LIKE bkpf-bukrs,
        belnr LIKE bkpf-belnr,
        gjahr LIKE bkpf-gjahr,
        ybset TYPE tybset.

  LOOP AT ibset INTO wbset WHERE awtyp = 'VBRK' AND stblg NE space.
    SELECT SINGLE bukrs belnr gjahr
      INTO (bukrs,belnr,gjahr)
      FROM bkpf
           WHERE awtyp = 'VBRK'
             AND awkey = wbset-stblg.
    IF sy-subrc = 0.
      READ TABLE ibset INTO ybset WITH KEY bukrs = bukrs
                                           belnr = belnr
                                           gjahr = gjahr.
      IF sy-subrc = 0.
        ybset-stblg = wbset-stblg.
        MODIFY TABLE ibset FROM ybset TRANSPORTING stblg.
      ENDIF.
    ENDIF.
  ENDLOOP.
  LOOP AT ibset INTO wbset WHERE NOT stblg IS INITIAL.
    wbset-hwbas = 0.
    MODIFY TABLE ibset FROM wbset TRANSPORTING hwbas.
    IF sy-subrc EQ 0.
      MODIFY ibset FROM wbset TRANSPORTING hwbas
          WHERE bukrs = wbset-bukrs AND
                belnr = wbset-belnr AND
                gjahr = wbset-gjahr AND
                buzei = wbset-buzei.
    ENDIF.
  ENDLOOP.
  DELETE ibset WHERE hwbas = 0.
  COMMIT WORK.
ENDFORM.                    "del_bset_revers

*---------------------------------------------------------------------*
*       FORM get_bset_lines                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_bset_lines.
  DATA: mwskz LIKE bset-mwskz,
        shkzg LIKE bset-shkzg,
        hwbas LIKE bset-hwbas,
        hwste LIKE bset-hwste,
        buzei LIKE bset-buzei,
        xblnr LIKE bkpf-xblnr,
        budat LIKE bkpf-budat,
        bldat LIKE bkpf-bldat,
        awkey LIKE bkpf-awkey,
        awtyp LIKE bkpf-awtyp,
        stblg LIKE rbkp-stblg,
        stjah LIKE rbkp-stjah.

  DATA : lt_bseg  TYPE TABLE OF bseg,
         ls_bseg  TYPE bseg,
         ls_bseg1 TYPE bseg,
         ls_bseg2 TYPE bseg,
         ls_bseg3 TYPE bseg.

  REFRESH: ibset, ibkpf, ibsid, ibsik.
  FREE: ibset, ibkpf, ibsid, ibsik.

  SELECT bukrs belnr gjahr buzei kunnr
    INTO TABLE ibsid
    FROM bsid
         WHERE  bukrs IN s_bukrs
         AND    gjahr IN s_gjahr
         AND    belnr IN s_belnr
         AND    blart IN s_blart
         AND    bldat IN s_bldat
         AND    budat IN s_budat
         AND    monat IN s_monat
         AND    kunnr IN s_kunnr.

  SELECT bukrs belnr gjahr buzei kunnr
    APPENDING TABLE ibsid
    FROM bsad
         WHERE  bukrs IN s_bukrs
         AND    gjahr IN s_gjahr
         AND    belnr IN s_belnr
         AND    blart IN s_blart
         AND    bldat IN s_bldat
         AND    budat IN s_budat
         AND    monat IN s_monat
         AND    kunnr IN s_kunnr.

  SELECT bukrs belnr gjahr buzei lifnr
    INTO TABLE ibsik
    FROM bsik
         WHERE  bukrs IN s_bukrs
         AND    gjahr IN s_gjahr
         AND    belnr IN s_belnr
         AND    blart IN s_blart
         AND    bldat IN s_bldat
         AND    budat IN s_budat
         AND    monat IN s_monat
         AND    lifnr IN s_lifnr.

  SELECT bukrs belnr gjahr buzei lifnr
    APPENDING TABLE ibsik
    FROM bsak
         WHERE  bukrs IN s_bukrs
         AND    gjahr IN s_gjahr
         AND    belnr IN s_belnr
         AND    blart IN s_blart
         AND    bldat IN s_bldat
         AND    budat IN s_budat
         AND    monat IN s_monat
         AND    lifnr IN s_lifnr.

  LOOP AT ibsid INTO wbsid.
    wbkpf-bukrs = wbsid-bukrs.
    wbkpf-belnr = wbsid-belnr.
    wbkpf-gjahr = wbsid-gjahr.
    APPEND wbkpf TO ibkpf.
  ENDLOOP.

  LOOP AT ibsik INTO wbsik.
    wbkpf-bukrs = wbsik-bukrs.
    wbkpf-belnr = wbsik-belnr.
    wbkpf-gjahr = wbsik-gjahr.
    APPEND wbkpf TO ibkpf.
  ENDLOOP.

  SORT ibkpf BY bukrs belnr gjahr.
  DELETE ADJACENT DUPLICATES FROM ibkpf.

  LOOP AT ibkpf INTO wbkpf.
    CLEAR: stblg, stjah, xblnr, awkey, awtyp, budat, bldat, buzei, mwskz, shkzg, hwbas, hwste.
    SELECT SINGLE stblg stjah xblnr awkey awtyp budat bldat
      INTO (stblg, stjah, xblnr, awkey, awtyp, budat, bldat)
      FROM bkpf
           WHERE bukrs EQ wbkpf-bukrs
             AND gjahr EQ wbkpf-gjahr
             AND belnr EQ wbkpf-belnr.
    wbset-bukrs = wbkpf-bukrs.
    wbset-belnr = wbkpf-belnr.
    wbset-gjahr = wbkpf-gjahr.
    wbset-xblnr = xblnr.
    wbset-budat = budat.
    wbset-bldat = bldat.
    wbset-stblg = stblg.
    wbset-stjah = stjah.
    IF awtyp = 'RMRP' AND stblg EQ space.
      SELECT SINGLE stblg stjah
        INTO (stblg, stjah)
        FROM rbkp
             WHERE  belnr  = awkey(10)
             AND    gjahr  = awkey+10(4).
      IF sy-subrc = 0.
        IF NOT stblg IS INITIAL.
          wbset-stblg = stblg.
          wbset-stjah = stjah.
        ENDIF.
      ENDIF.
    ELSEIF awtyp = 'VBRK' AND stblg EQ space.
      SELECT SINGLE sfakn
        INTO stblg
        FROM vbrk
             WHERE  vbeln  = awkey.
      IF sy-subrc = 0.
        IF NOT stblg IS INITIAL.
          wbset-stblg = stblg.
          wbset-awtyp = awtyp.
        ENDIF.
      ENDIF.
    ENDIF.
    IF wbset-bukrs = '2425' .

      SELECT SINGLE buzei mwskz shkzg hwbas hwste
      INTO (buzei, mwskz, shkzg, hwbas, hwste)
      FROM bset
       WHERE bukrs EQ wbkpf-bukrs
         AND gjahr EQ wbkpf-gjahr
         AND belnr EQ wbkpf-belnr
         AND ktosl = ''.
      IF sy-subrc NE 0.

        IF s_belnr IS INITIAL.
          SELECT SINGLE *
           FROM bseg
           INTO  ls_bseg
           WHERE belnr = wbkpf-belnr
           AND gjahr IN s_gjahr
           AND bukrs = '2425'
           AND hkont LIKE '159%'.

          IF sy-subrc = 0 .
            SELECT SINGLE *
             FROM bseg
             INTO  ls_bseg1
             WHERE belnr = wbkpf-belnr
               AND gjahr IN s_gjahr
               AND bukrs = '2425'
               AND hkont LIKE '320%'
               AND kunnr IN s_kunnr
               AND lifnr IN s_lifnr.

            IF sy-subrc = 0.
              wbset-buzei = ls_bseg1-buzei.
              wbset-shkzg = ls_bseg1-shkzg.

              IF wbset-mwskz IS INITIAL.
                wbset-mwskz = 'V0'.
              ENDIF.

              IF wbset-shkzg EQ 'H'.
                wbset-hwbas = - ls_bseg1-dmbtr.

              ELSE.
                wbset-hwbas = ls_bseg1-dmbtr.
              ENDIF.

              CLEAR : ls_bseg.
              CLEAR : ls_bseg1.
              CLEAR : ls_bseg2.
            ENDIF.
            SELECT SINGLE *
             FROM bseg
             INTO  ls_bseg2
             WHERE belnr = wbkpf-belnr
              AND gjahr IN s_gjahr
              AND bukrs = '2425'
              AND hkont LIKE '191%'.         .
            IF sy-subrc = 0.

              IF wbset-mwskz IS INITIAL.
                wbset-mwskz = 'V0'.
              ENDIF.

              wbset-buzei = ls_bseg2-buzei.
              wbset-shkzg = ls_bseg2-shkzg.
              IF wbset-shkzg EQ 'H'.
                wbset-hwste = ls_bseg2-dmbtr.
              ELSE.
                wbset-hwste = ls_bseg2-dmbtr.
              ENDIF.

            ENDIF.

            INSERT wbset INTO TABLE ibset.
          ENDIF.

        ENDIF.


      ENDIF.

      SELECT buzei mwskz shkzg hwbas hwste
  INTO (buzei, mwskz, shkzg, hwbas, hwste)
  FROM bset
       WHERE bukrs EQ wbkpf-bukrs
         AND gjahr EQ wbkpf-gjahr
         AND belnr EQ wbkpf-belnr
         AND ktosl IN s_ktosl.
        wbset-buzei = buzei.
        wbset-mwskz = mwskz.
        wbset-shkzg = shkzg.
        wbset-hwbas = hwbas.
        wbset-hwste = hwste.
        INSERT wbset INTO TABLE ibset.
      ENDSELECT.

    ELSE.

      SELECT buzei mwskz shkzg hwbas hwste
       INTO (buzei, mwskz, shkzg, hwbas, hwste)
       FROM bset
       WHERE bukrs EQ wbkpf-bukrs
         AND gjahr EQ wbkpf-gjahr
         AND belnr EQ wbkpf-belnr
         AND ktosl IN s_ktosl.
        wbset-buzei = buzei.
        wbset-mwskz = mwskz.
        wbset-shkzg = shkzg.
        wbset-hwbas = hwbas.
        wbset-hwste = hwste.
        INSERT wbset INTO TABLE ibset.
      ENDSELECT.

    ENDIF.
    CLEAR wbset.
  ENDLOOP.
  FREE: ibkpf.
  COMMIT WORK.
ENDFORM.                    "get_bset_lines


*---------------------------------------------------------------------*
*       FORM set_initial_values                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_initial_values.
  DATA : monlow LIKE t009b-poper,
         monhgh LIKE t009b-poper.

  CASE par_avpn.
    WHEN '1'. var_avp1 = 'X'.
    WHEN '2'. var_avp2 = 'X'.
    WHEN '3'. var_avp3 = 'X'.
    WHEN '4'. var_avp4 = 'X'.
    WHEN '5'. var_avp5 = 'X'.
    WHEN '6'. var_avp6 = 'X'.
    WHEN '7'. var_avp7 = 'X'.
    WHEN '8'. var_avp8 = 'X'.
    WHEN OTHERS. flg_notp = 'X'.
  ENDCASE.

  SELECT SINGLE * FROM t001 WHERE bukrs EQ s_bukrs-low.
  SELECT SINGLE * FROM t005 WHERE land1 EQ t001-land1.

  REFRESH it007a.
  SELECT *
    INTO TABLE it007a
    FROM t007a
         WHERE  kalsm  = t005-kalsm
         AND    mwskz IN s_mwskz.

  REFRESH : itabbsd, itabbad, itabbsh, itabbah.
  FREE : itabbsd, itabbad, itabbsh, itabbah.

  CLEAR g_s_log.
  g_s_log-extnumber = 'Logs'.
  g_s_log-aldate  = sy-datum.
  g_s_log-altime  = sy-uzeit.
  g_s_log-aluser  = sy-uname.
  g_s_log-altcode = sy-tcode.
  g_s_log-alprog  = sy-repid.
  g_s_log-almode  = 'I'.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log = g_s_log
    EXCEPTIONS
      OTHERS  = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
               sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "set_initial_values

*---------------------------------------------------------------------*
*       FORM get_filename_f4                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FNAM                                                          *
*---------------------------------------------------------------------*
FORM get_filename_f4 CHANGING fnam.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask          = '*.xml'
      static        = 'X'
    CHANGING
      file_name     = fnam
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.

  IF sy-subrc > 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " get_filename_f4

*&--------------------------------------------------------------------*
*&      Form  list_error
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM list_error.
  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_amodal = ' '
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
               sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "list_error


*---------------------------------------------------------------------*
*       FORM comment_build                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  LT_TOP_OF_PAGE                                                *
*  -->  TITLE                                                         *
*---------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader title.
  DATA: ls_line TYPE slis_listheader.
  DATA: tarh(10) TYPE c,
        saat(8)  TYPE c,
        sdatum LIKE sy-datum,
        suzeit LIKE sy-uzeit,
        butxt LIKE t001-butxt,
        trh1(10),
        trh2(10).
  sdatum = sy-datum.
  suzeit = sy-uzeit.
  REFRESH lt_top_of_page.

  WRITE :  sdatum TO tarh DD/MM/YYYY.
  WRITE :  suzeit TO saat USING EDIT MASK '__:__:__'.
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = title.
  APPEND ls_line TO lt_top_of_page.
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key = 'Rapor Tarihi - Saati : '.
  CONCATENATE tarh '-' saat INTO ls_line-info.
  APPEND ls_line TO lt_top_of_page.
  ls_line-key = 'Şirket               : '.
  CONCATENATE t001-bukrs '-' t001-butxt INTO ls_line-info.
  APPEND ls_line TO lt_top_of_page.
  CLEAR ls_line.
  ls_line-typ  = 'A'.
ENDFORM.                               "comment_build

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_list_top_of_page.
ENDFORM.                    "set_top_of_page


*&--------------------------------------------------------------------*
*&      Form  close_block
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->VALUE(U_CLOtextLOCK)
*      -->U_MODIFY_IDtext
*      -->U_CONVERT  text
*---------------------------------------------------------------------*
FORM close_block USING    value(u_close_block) LIKE par_cb1
                          u_modify_id LIKE screen-group1
                          u_convert.

  IF NOT u_convert IS INITIAL.
    IF u_close_block IS INITIAL.
      u_close_block = 'X'.
    ELSE.
      CLEAR u_close_block.
    ENDIF.
  ENDIF.

  IF ( screen-group1 = u_modify_id )
  AND ( NOT u_close_block IS INITIAL ).
    screen-active = '0'.
    MODIFY SCREEN.
  ENDIF.

ENDFORM.                               " CLOSE_BLOCK

*&--------------------------------------------------------------------*
*&      Form  print_table
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->LT_OUTTAB  text
*      -->L_CHECK_APPtext
*      -->L_CHECK_AVPtext
*      -->L_VARIANT_HtextE
*      -->L_VARIANT_Ntext
*      -->L_FIELDNAMEtext
*      -->L_FIELDNAMEtext
*      -->L_NEXT_FORMtext
*      -->L_TOP_OF_PAtextORM
*      -->L_CREATE_FItextAT_FORM
*---------------------------------------------------------------------*
FORM print_table
  TABLES lt_outtab
  USING
    l_check_append         TYPE c
    l_check_avp            TYPE c
    l_variant_handle       TYPE disvariant-handle
    l_variant_name         TYPE disvariant-variant
    l_fieldname_1          TYPE slis_sortinfo_alv-fieldname
    l_fieldname_2          TYPE slis_sortinfo_alv-fieldname
    l_next_form            TYPE slis_alv_event-form
    l_top_of_page_form     TYPE slis_alv_event-form
    l_create_fieldcat_form TYPE slis_alv_event-form.

  DATA:    lt_fieldcat  TYPE slis_t_fieldcat_alv,     "Field catalog
           lt_sortinfo  TYPE slis_t_sortinfo_alv,     "Sort information
           lt_alv_event TYPE slis_t_event,            "Events by ALV
           l_layout     TYPE slis_layout_alv,         "Layout info.
           l_variant    TYPE disvariant.              "Display variant
  STATICS: s_flg_fiti   TYPE c VALUE 'X'.

  CLEAR flg_print_header.
  IF ( l_check_append = 'X' ) OR ( l_check_avp = 'X' ).

    flg_print_header = 'X'.
    l_variant-handle  = l_variant_handle.
    l_variant-variant = l_variant_name.
    l_variant-report  = g_repid.

    IF flg_notp = 'X'.                 "No variant customizing
      PERFORM append_event   USING    slis_ev_end_of_list
                                      l_next_form
                             CHANGING lt_alv_event.
    ELSE.
      PERFORM fill_table TABLES lt_outtab.
    ENDIF.
    IF s_flg_fiti = 'X'.               "First List
      PERFORM append_event   USING    slis_ev_top_of_list
                                      'TOP_OF_LIST'
                             CHANGING lt_alv_event.
    ENDIF.
    PERFORM append_event     USING    slis_ev_top_of_page
                                      l_top_of_page_form
                             CHANGING lt_alv_event.

    PERFORM (l_create_fieldcat_form) IN PROGRAM (g_repid)
                             USING    l_variant_handle
                             CHANGING lt_fieldcat.

    IF ( NOT l_fieldname_1 IS INITIAL ).
      PERFORM append_sortinfo  USING    l_fieldname_1 'X' 'X' 'UL'
                               CHANGING lt_sortinfo.
    ENDIF.
    IF ( NOT l_fieldname_2 IS INITIAL ).
      PERFORM append_sortinfo  USING    l_fieldname_2 'X' 'X' 'UL'
                               CHANGING lt_sortinfo.
    ENDIF.

    PERFORM create_layout USING    s_flg_fiti
                          CHANGING l_layout.

    IF g_refresh_round = 'X'.          "All lists were displayed
      CLEAR g_refresh_round.
      flg_summ = ' '.
    ELSE.
      IF s_flg_fiti = 'X'.             "Display the first list
        CLEAR s_flg_fiti.
      ENDIF.
      CLEAR flg_statistikdruck.                             "OP-08
      CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
        EXPORTING
          i_callback_program       = g_repid
          is_layout                = l_layout
          i_callback_pf_status_set = 'SET_PF_STATUS_SET'
          i_callback_user_command  = 'SET_USER_COMMAND'
          it_fieldcat              = lt_fieldcat
          it_excluding             = gt_excluding
          it_sort                  = lt_sortinfo
          i_save                   = 'A'
          is_variant               = l_variant
          it_events                = lt_alv_event
          is_print                 = flg_statistikdruck     "OP-08
        TABLES
          t_outtab                 = lt_outtab.
    ENDIF.
  ELSE.                                "LT_OUTTAB is not displayed
    PERFORM (l_next_form) IN PROGRAM (g_repid).
  ENDIF.
ENDFORM.                    "print_table

*&--------------------------------------------------------------------*
*&      Form  append_event
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->U_NAME     text
*      -->U_FORM     text
*      -->CT_EVENT   text
*---------------------------------------------------------------------*
FORM append_event USING    u_name TYPE slis_alv_event-name
                           u_form TYPE slis_alv_event-form
                  CHANGING ct_event     TYPE slis_t_event.
  DATA: l_alv_event TYPE slis_alv_event.

  l_alv_event-name = u_name.
  l_alv_event-form = u_form.
  APPEND l_alv_event TO ct_event.
ENDFORM.                    "append_event

*&--------------------------------------------------------------------*
*&      Form  create_layout
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->L_FLG_FITI text
*      -->L_LAYOUT   text
*---------------------------------------------------------------------*
FORM create_layout USING    l_flg_fiti TYPE c
                   CHANGING l_layout TYPE slis_layout_alv.

  IF l_flg_fiti = 'X'.
    l_layout-min_linesize = 250.       "Place for the following lists
    l_layout-get_selinfos = 'X'.       "Print report selections
  ELSE.
    l_layout-list_append = 'X'.        "This list is a Append-List
  ENDIF.
  IF flg_notp = 'X'.
    l_layout-no_hotspot = 'X'.         "Headings not as a Hotspot
  ELSE.
    l_layout-group_change_edit = 'X'.  "User can enter format options
  ENDIF.
ENDFORM.                    "create_layout

*&--------------------------------------------------------------------*
*&      Form  append_sortinfo
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->L_FIELDNAMEtext
*      -->L_UP       text
*      -->L_SUBTOT   text
*      -->L_GROUP    text
*      -->L_LT_SORTINtext
*---------------------------------------------------------------------*
FORM append_sortinfo USING l_fieldname TYPE slis_sortinfo_alv-fieldname
                           l_up        TYPE slis_sortinfo_alv-up
                           l_subtot    TYPE slis_sortinfo_alv-subtot
                           l_group     TYPE slis_sortinfo_alv-group
                     CHANGING l_lt_sortinfo TYPE slis_t_sortinfo_alv.
  DATA: l_sortinfo TYPE slis_sortinfo_alv.

  l_sortinfo-fieldname = l_fieldname.
  l_sortinfo-up        = l_up.
  l_sortinfo-subtot    = l_subtot.
  l_sortinfo-group     = l_group.
  APPEND l_sortinfo TO l_lt_sortinfo.
ENDFORM.                    "append_sortinfo

*&--------------------------------------------------------------------*
*&      Form  fill_table
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->LT_TABLE   text
*---------------------------------------------------------------------*
FORM fill_table TABLES lt_table.
  DO 3 TIMES.
    CALL FUNCTION 'INITIALIZE_STRUCTURE'
      EXPORTING
        i_n_fill   = 1
      CHANGING
        c_workarea = lt_table.
    APPEND lt_table.
  ENDDO.
ENDFORM.                               " FILL_TABLE

*&--------------------------------------------------------------------*
*&      Form  create_fieldcat_ep
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->L_HANDLE   text
*      -->P_LT_FIELDCtext
*---------------------------------------------------------------------*
FORM create_fieldcat_bsd USING  l_handle TYPE slis_handl
                        CHANGING p_lt_fieldcat TYPE slis_t_fieldcat_alv.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = g_repid
      i_internal_tabname = 'ITABBSD'
      i_structure_name   = '/SAPTR/VATBBSD'
    CHANGING
      ct_fieldcat        = p_lt_fieldcat[].
  LOOP AT p_lt_fieldcat INTO lt_fieldcat.
    IF lt_fieldcat-datatype EQ 'CURR'.
      lt_fieldcat-do_sum = 'X'.
      MODIFY p_lt_fieldcat FROM lt_fieldcat.
    ENDIF.
    IF lt_fieldcat-fieldname EQ 'USER_FIELD_1'.
      lt_fieldcat-no_out = 'X'.
      MODIFY p_lt_fieldcat FROM lt_fieldcat.
    ENDIF.
    IF lt_fieldcat-fieldname EQ 'USER_FIELD_2'.
      lt_fieldcat-no_out = 'X'.
      MODIFY p_lt_fieldcat FROM lt_fieldcat.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "create_fieldcat_bsd

*&--------------------------------------------------------------------*
*&      Form  create_fieldcat_bsh
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->L_HANDLE   text
*      -->P_LT_FIELDCtext
*---------------------------------------------------------------------*
FORM create_fieldcat_bsh USING  l_handle TYPE slis_handl
                        CHANGING p_lt_fieldcat TYPE slis_t_fieldcat_alv.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = g_repid
      i_internal_tabname = 'ITABBSH'
      i_structure_name   = '/SAPTR/VATBBSH'
    CHANGING
      ct_fieldcat        = p_lt_fieldcat[].
  LOOP AT p_lt_fieldcat INTO lt_fieldcat.
    IF lt_fieldcat-datatype EQ 'CURR'.
      lt_fieldcat-do_sum = 'X'.
      MODIFY p_lt_fieldcat FROM lt_fieldcat.
    ENDIF.
    IF lt_fieldcat-fieldname EQ 'LANDX'.
      lt_fieldcat-no_out = 'X'.
      MODIFY p_lt_fieldcat FROM lt_fieldcat.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "create_fieldcat_bsh

*&--------------------------------------------------------------------*
*&      Form  create_fieldcat_bad
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->L_HANDLE   text
*      -->P_LT_FIELDCtext
*---------------------------------------------------------------------*
FORM create_fieldcat_bad USING  l_handle TYPE slis_handl
                        CHANGING p_lt_fieldcat TYPE slis_t_fieldcat_alv.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = g_repid
      i_internal_tabname = 'ITABBAD'
      i_structure_name   = '/SAPTR/VATBBAD'
    CHANGING
      ct_fieldcat        = p_lt_fieldcat[].
  LOOP AT p_lt_fieldcat INTO lt_fieldcat.
    IF lt_fieldcat-datatype EQ 'CURR'.
      lt_fieldcat-do_sum = 'X'.
      MODIFY p_lt_fieldcat FROM lt_fieldcat.
    ENDIF.
    IF lt_fieldcat-fieldname EQ 'USER_FIELD_1'.
      lt_fieldcat-no_out = 'X'.
      MODIFY p_lt_fieldcat FROM lt_fieldcat.
    ENDIF.
    IF lt_fieldcat-fieldname EQ 'USER_FIELD_2'.
      lt_fieldcat-no_out = 'X'.
      MODIFY p_lt_fieldcat FROM lt_fieldcat.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "create_fieldcat_bad

*&--------------------------------------------------------------------*
*&      Form  create_fieldcat_bah
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->L_HANDLE   text
*      -->P_LT_FIELDCtext
*---------------------------------------------------------------------*
FORM create_fieldcat_bah USING  l_handle TYPE slis_handl
                        CHANGING p_lt_fieldcat TYPE slis_t_fieldcat_alv.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = g_repid
      i_internal_tabname = 'ITABBAH'
      i_structure_name   = '/SAPTR/VATBBAH'
    CHANGING
      ct_fieldcat        = p_lt_fieldcat[].

  LOOP AT p_lt_fieldcat INTO lt_fieldcat.
    IF lt_fieldcat-datatype EQ 'CURR'.
      lt_fieldcat-do_sum = 'X'.
      MODIFY p_lt_fieldcat FROM lt_fieldcat.
    ENDIF.
    IF lt_fieldcat-fieldname EQ 'LANDX'.
      lt_fieldcat-no_out = 'X'.
      MODIFY p_lt_fieldcat FROM lt_fieldcat.
    ENDIF.

  ENDLOOP.
ENDFORM.                    "create_fieldcat_bah

*&--------------------------------------------------------------------*
*&      Form  top_of_page_bsd
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM top_of_page_bsd.
  FORMAT COLOR COL_NEGATIVE INVERSE.
  WRITE: / text-300.
  FORMAT RESET.
ENDFORM.                    "top_of_page_bsd

*&--------------------------------------------------------------------*
*&      Form  top_of_page_bsh
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM top_of_page_bsh.
  FORMAT COLOR COL_NEGATIVE INVERSE.
  WRITE: / text-301.
  FORMAT RESET.
ENDFORM.                    "top_of_page_bsh

*&--------------------------------------------------------------------*
*&      Form  top_of_page_bad
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM top_of_page_bad.
  FORMAT COLOR COL_NEGATIVE INVERSE.
  WRITE: / text-302.
  FORMAT RESET.
ENDFORM.                    "top_of_page_bad

*&--------------------------------------------------------------------*
*&      Form  top_of_page_bah
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM top_of_page_bah.
  FORMAT COLOR COL_NEGATIVE INVERSE.
  WRITE: / text-303.
  FORMAT RESET.
ENDFORM.                    "top_of_page_bah

*&--------------------------------------------------------------------*
*&      Form  top_of_page_bsd
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM top_of_page_bsd_o.
  FORMAT COLOR COL_NEGATIVE INVERSE.
  WRITE: / text-304.
  FORMAT RESET.
ENDFORM.                    "top_of_page_bsd

*&--------------------------------------------------------------------*
*&      Form  top_of_page_bsh
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM top_of_page_bsh_o.
  FORMAT COLOR COL_NEGATIVE INVERSE.
  WRITE: / text-305.
  FORMAT RESET.
ENDFORM.                    "top_of_page_bsh

*&--------------------------------------------------------------------*
*&      Form  top_of_page_bad
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM top_of_page_bad_o.
  FORMAT COLOR COL_NEGATIVE INVERSE.
  WRITE: / text-306.
  FORMAT RESET.
ENDFORM.                    "top_of_page_bad

*&--------------------------------------------------------------------*
*&      Form  top_of_page_bah
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM top_of_page_bah_o.
  FORMAT COLOR COL_NEGATIVE INVERSE.
  WRITE: / text-307.
  FORMAT RESET.
ENDFORM.                    "top_of_page_bah


*&--------------------------------------------------------------------*
*&      Form  create_gt_excluding
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM create_gt_excluding.
  REFRESH gt_excluding.
  IF flg_notp = 'X'.                   "No variant customizing
    PERFORM append_fcode USING: '&SUM',"Zwischensummen
                                '&ODN',"Sortieren absteigend
                                '&OUP',"Sortieren aufsteigen
                                '&OAD',"Variante auswählen
                                '&AVE',"Variante sichern
                                '&UMC',"Summe
                                '&OMP',"Nur Summ.
                                '&XPA',"Sum. aufr.
                                '&OL0',"Akt. Anze.
                                '&ETA',"Detail

                                '&OPT',"Spalte op.
                                '&OLX',"Anz. def.
                                '&ERW',"Anz. Verw.
                                '&KOM',"Sum. ausw.
                                '&AUF',"Aufriss
                                '&CDF',"Fixi. auf.
                                '&CFI',"Spalte fi.
                                '&DAU',"Tren. aut.
                                '&DOF',"Tren. aus
                                '&LFO',"Liststatus
                                '&XXL',"Tabellenkal.
                                '&AQW',"Textverarb.
                                '&DON',"Tren. ein
                                '&LIS'."Grundliste
  ENDIF.
  PERFORM append_fcode USING:   '&ABC',"ABC Analye
                                '&ILT',"Filter
                                '&ILD'."Filter lö
ENDFORM.                               "create_gt_excluding

*&--------------------------------------------------------------------*
*&      Form  append_fcode
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->U_FCODE    text
*---------------------------------------------------------------------*
FORM append_fcode USING u_fcode TYPE rsmpe-func.
  APPEND u_fcode TO gt_excluding.
ENDFORM.                    "append_fcode

*&--------------------------------------------------------------------*
*&      Form  alv_variante_f4
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->L_HANDLE   text
*---------------------------------------------------------------------*
FORM alv_variante_f4 USING l_handle TYPE slis_handl.
  DATA: l_variant_help TYPE disvariant,
        l_variant      TYPE disvariant.
  DATA: l_exit TYPE c.

  DATA nof4 TYPE c.

  CLEAR nof4.
  LOOP AT SCREEN.
    CASE l_handle.
      WHEN 'BSD'.
        IF screen-name = 'PVARIBSD'.
          IF screen-input = 0.
            nof4 = 'X'.
          ENDIF.
        ENDIF.
      WHEN 'BSH'.
        IF screen-name = 'PVARIBSH'.
          IF screen-input = 0.
            nof4 = 'X'.
          ENDIF.
        ENDIF.
      WHEN 'BAD'.
        IF screen-name = 'PVARIBAD'.
          IF screen-input = 0.
            nof4 = 'X'.
          ENDIF.
        ENDIF.
      WHEN 'BAH'.
        IF screen-name = 'PVARIBAH'.
          IF screen-input = 0.
            nof4 = 'X'.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDLOOP.               "end note 522575

  l_variant-handle = l_handle.
  l_variant-report = g_repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = l_variant
      i_save     = 'A'
    IMPORTING
      e_exit     = l_exit
      es_variant = l_variant_help.

  IF l_exit = space.                   "No User-Exit
    CASE l_handle.
      WHEN 'BSD'.
        pvaribsd = l_variant_help-variant.
      WHEN 'BSH'.
        pvaribsh = l_variant_help-variant.
      WHEN 'BAD'.
        pvaribad = l_variant_help-variant.
      WHEN 'BAH'.
        pvaribah = l_variant_help-variant.
    ENDCASE.
  ENDIF.

ENDFORM.                    "alv_variante_f4

*&--------------------------------------------------------------------*
*&      Form  set_user_command
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->R_UCOMM    text
*      -->RS_SELFIELDtext
*---------------------------------------------------------------------*
FORM set_user_command USING r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.
  DATA: belnr LIKE bsid-belnr,
        gjahr LIKE bsid-gjahr,
        bukrs LIKE bsid-bukrs.
  gs_selfield = rs_selfield.
  CASE gs_selfield-tabname.
    WHEN 'ITABBSD'.
      READ TABLE itabbsd INDEX gs_selfield-tabindex.
      belnr = itabbsd-belnr.
      bukrs = itabbsd-bukrs.
      gjahr = itabbsd-gjahr.
    WHEN 'ITABBSH'. READ TABLE itabbsh INDEX gs_selfield-tabindex.
    WHEN 'ITABBAD'.
      READ TABLE itabbad INDEX gs_selfield-tabindex.
      belnr = itabbad-belnr.
      bukrs = itabbad-bukrs.
      gjahr = itabbad-gjahr.
    WHEN 'ITABBAH'. READ TABLE itabbah INDEX gs_selfield-tabindex.
  ENDCASE.

  CASE r_ucomm.
    WHEN 'HTML'.
      MESSAGE i000(0k) WITH 'STANDART-HTML'.
    WHEN '&IC1' OR '&ETA'.             "Double Click
      IF gs_selfield-tabname EQ 'ITABBSD' OR
         gs_selfield-tabname EQ 'ITABBAD'.
        PERFORM call_fb03 USING belnr bukrs gjahr.
      ENDIF.
    WHEN 'LOGS'. PERFORM list_error.
    WHEN 'DXMLS'. PERFORM cnv_bs_to_xml.
    WHEN 'DXMLA'. PERFORM cnv_ba_to_xml.
    WHEN OTHERS.

  ENDCASE.

ENDFORM.                               "f01_user_command

*&--------------------------------------------------------------------*
*&      Form  call_fb03
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->BELNR      text
*---------------------------------------------------------------------*
FORM call_fb03 USING belnr bukrs gjahr.
  CHECK NOT belnr IS INITIAL.
  SET PARAMETER ID 'BLN' FIELD belnr.
  SET PARAMETER ID 'BUK' FIELD bukrs.
  SET PARAMETER ID 'GJR' FIELD gjahr.
  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
ENDFORM.                                                    "call_fb03

*&--------------------------------------------------------------------*
*&      Form  set_pf_status_set
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->RT_EXTAB   text
*---------------------------------------------------------------------*
FORM set_pf_status_set USING rt_extab TYPE slis_t_extab .
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab[].
*Function Code        LOGS
*Function Text        Logs
*Icon Name            ICON_PROTOCOL
*Icon Text            Logs
*Info. Text           Logs
*Fastpath

*Function Code        DXML
*Function Text        Export XML
*Icon Name            ICON_XML_DOC
*Icon Text            Export XML
*Info. Text           Export XML
*Fastpath


ENDFORM.                               "f01_set_status


*&--------------------------------------------------------------------*
*&      Form  down_xtable
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->FILENAME   text
*      -->XML_TABLE  text
*---------------------------------------------------------------------*
FORM down_xtable USING fnam
                      xml_table TYPE table
                      fsize fty.
  DATA: : rc LIKE sy-subrc,
          filesize TYPE i,
          htext(80),
          filename  TYPE string.
  filename = fnam.
  filesize = fsize.
  IF sy-batch NE 'X'.
    IF filename IS INITIAL.
      IF fty EQ 'A'.
        htext = text-607.
      ELSE.
        htext = text-608.
      ENDIF.
      CALL FUNCTION 'WS_FILENAME_GET'
        EXPORTING
          def_filename     = filename
          title            = htext
        IMPORTING
          filename         = filename
          rc               = rc
        EXCEPTIONS
          inv_winsys       = 1
          no_batch         = 2
          selection_cancel = 3
          selection_error  = 4
          OTHERS           = 5.
      IF filename IS INITIAL.
        IF fty EQ 'A'.
          MESSAGE i006 WITH text-609.
        ELSE.
          MESSAGE i006 WITH text-610.
        ENDIF.
        EXIT.
      ENDIF.
    ENDIF.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize = filesize
        filename     = filename
        filetype     = 'BIN'
      TABLES
        data_tab     = xml_table
      EXCEPTIONS
        OTHERS       = 1.

  ELSE.
    OPEN DATASET filename FOR OUTPUT IN BINARY MODE.
    LOOP AT xml_table INTO strxml.
      TRANSFER strxml TO filename.
    ENDLOOP.
    CLOSE DATASET filename.
  ENDIF.

ENDFORM.     " down_table

*---------------------------------------------------------------------*
*       FORM cnv_bs_to_xml                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM cnv_bs_to_xml.
  DATA: l_ixml            TYPE REF TO if_ixml,
        l_streamfactory   TYPE REF TO if_ixml_stream_factory,
        l_ostream         TYPE REF TO if_ixml_ostream,
        l_renderer        TYPE REF TO if_ixml_renderer,
        l_document        TYPE REF TO if_ixml_document.

  DATA: l_element_beyanname      TYPE REF TO if_ixml_element,
        l_element_genel          TYPE REF TO if_ixml_element,
        l_element_idari          TYPE REF TO if_ixml_element,
        l_element_vdkodu         TYPE REF TO if_ixml_element,
        l_element_donem          TYPE REF TO if_ixml_element,
        l_element_dummy          TYPE REF TO if_ixml_element,
        l_element_mukellef       TYPE REF TO if_ixml_element,
        l_element_hsv            TYPE REF TO if_ixml_element,
        l_element_duzenleyen     TYPE REF TO if_ixml_element,
        l_element_ozel           TYPE REF TO if_ixml_element,
        l_element_satisbildirimi TYPE REF TO if_ixml_element,
        l_element_bs             TYPE REF TO if_ixml_element,
        l_value                  TYPE string.

  DATA: l_xml_table       TYPE TABLE OF xml_line,
        l_xml_size        TYPE i,
        l_rc              TYPE i,
        toplam            TYPE p DECIMALS 2,
        toplam_o          TYPE p DECIMALS 2,
        toplam_gen        TYPE p DECIMALS 2,
        tutar             TYPE p DECIMALS 2,
        toplamstr(30),
        toplamstr_o(30),
        toplamstr_gen(30),
        tutarstr(30),
        l_year(4),
        l_monat TYPE i,
        l_monat_str(2),
        sortn             TYPE i,
        s_encoding_type   TYPE string,
        encoding          TYPE REF TO if_ixml_encoding.

  CHECK sy-batch NE 'X' OR c_savebs EQ 'X'.
  IF NOT s_gjahr IS INITIAL.
    l_year = s_gjahr-low.
  ELSEIF NOT s_budat IS INITIAL.
    l_year = s_budat-low(4).
  ELSEIF NOT s_bldat IS INITIAL.
    l_year = s_bldat-low(4).
  ENDIF.
  IF NOT s_monat IS INITIAL.
    l_monat = s_monat-low.
  ELSEIF NOT s_budat IS INITIAL.
    l_monat = s_budat-low+4(2).
  ELSEIF NOT s_bldat IS INITIAL.
    l_monat = s_bldat-low+4(2).
  ENDIF.
  WRITE l_monat TO l_monat_str.
  CONDENSE l_monat_str.

  toplam = 0.
  toplam_o = 0.
  toplam_gen = 0.
  LOOP AT itabbsh.
    toplam = toplam + trunc( itabbsh-hwbas ).
  ENDLOOP.
  WRITE toplam TO toplamstr DECIMALS 0 NO-GROUPING.

  LOOP AT itabbsh_o.
    toplam_o = toplam_o + trunc( itabbsh_o-hwbas ).
  ENDLOOP.
  WRITE toplam_o TO toplamstr_o DECIMALS 0 NO-GROUPING.

  toplam_gen = toplam + toplam_o.
  WRITE toplam_gen TO toplamstr_gen DECIMALS 0 NO-GROUPING.

  CHECK toplam_gen NE 0.

  l_ixml = cl_ixml=>create( ).
  l_document = l_ixml->create_document( ).

  s_encoding_type = 'ISO-8859-9'.
  encoding =  l_ixml->create_encoding(
             character_set = s_encoding_type
             byte_order = 0 ).

  l_document->set_encoding( encoding ).
  l_element_beyanname  = l_document->create_simple_element(
                  name = 'beyanname'
                parent = l_document ).

  IF l_year < '2008'.
    l_value = 'FORMBS_1'.
  ELSE.
    l_value = 'FORMBS_2'.
  ENDIF.
  l_rc = l_element_beyanname->set_attribute( name = 'kodVer'
                              value = l_value ).

  IF l_year < '2009'.
    l_value = 'YTL'.
  ELSE.
    l_value = 'TL'.
  ENDIF.
  l_rc = l_element_beyanname->set_attribute( name = 'paraBirimi'
                              value = l_value ).

  l_value = 'http://www.w3.org/2001/XMLSchema-instance'.
  l_rc = l_element_beyanname->set_attribute(
      name = 'xmlns:xsi'
     value = l_value ).

  IF l_year < '2008'.
    l_value = 'FORMBS_1.xsd'.
  ELSE.
    l_value = 'FORMBS_2.xsd'.
  ENDIF.
  l_rc = l_element_beyanname->set_attribute(
      name = 'xsi:noNamespaceSchemaLocation'
     value = l_value ).

  l_element_genel  = l_document->create_simple_element(
              name = 'genel'
              parent = l_element_beyanname  ).

  l_element_idari  = l_document->create_simple_element(
              name = 'idari'
              parent = l_element_genel  ).
  l_value = tpitoc.
  l_element_dummy  = l_document->create_simple_element(
              name = 'vdKodu'
              value = l_value
              parent = l_element_idari  ).
  l_element_donem  = l_document->create_simple_element(
              name = 'donem'
              parent = l_element_idari  ).

  IF l_year < '2008'.
    l_value = 'yillik'.
  ELSE.
    l_value = 'aylik'.
  ENDIF.
  l_element_dummy  = l_document->create_simple_element(
             name = 'tip'
             value = l_value
             parent = l_element_donem  ).
  l_value = l_year.
  l_element_dummy = l_document->create_simple_element(
             name = 'yil'
             value = l_value
             parent = l_element_donem  ).
  IF l_year >= '2008'.
    l_value = l_monat_str.
    l_element_dummy = l_document->create_simple_element(
               name = 'ay'
               value = l_value
               parent = l_element_donem  ).
  ENDIF.

  l_element_mukellef  = l_document->create_simple_element(
              name = 'mukellef'
              parent = l_element_genel  ).
  l_value = tpitno.
  l_element_dummy  = l_document->create_simple_element(
              name = 'vergiNo'
              value = l_value
              parent = l_element_mukellef  ).
  l_value = tpisnm.
  l_element_dummy = l_document->create_simple_element(
              name = 'soyadi'
              value = l_value
              parent = l_element_mukellef  ).
  l_value = tpinam.
  l_element_dummy = l_document->create_simple_element(
              name = 'adi'
              value = l_value
              parent = l_element_mukellef  ).
  l_value = tpitrn.
  l_element_dummy = l_document->create_simple_element(
              name = 'ticSicilNo'
              value = l_value
              parent = l_element_mukellef  ).
  l_value = tpiema.
  l_element_dummy = l_document->create_simple_element(
              name = 'eposta'
              value = l_value
              parent = l_element_mukellef  ).
  l_value = tpitac.
  l_element_dummy = l_document->create_simple_element(
              name = 'alanKodu'
              value = l_value
              parent = l_element_mukellef  ).
  l_value = tpitel.
  l_element_dummy = l_document->create_simple_element(
              name = 'telNo'
              value = l_value
              parent = l_element_mukellef  ).
  l_element_hsv  = l_document->create_simple_element(
              name = 'hsv'
              parent = l_element_genel  ).
  l_value = 'kendisi'.
  l_rc = l_element_hsv->set_attribute( name = 'sifat'
                              value = l_value ).
  l_value = tpitno.
  l_element_dummy  = l_document->create_simple_element(
              name = 'vergiNo'
              value = l_value
              parent = l_element_hsv  ).
  l_value = tpisnm.
  l_element_dummy = l_document->create_simple_element(
              name = 'soyadi'
              value = l_value
              parent = l_element_hsv  ).
  l_value = tpinam.
  l_element_dummy = l_document->create_simple_element(
              name = 'adi'
              value = l_value
              parent = l_element_hsv  ).
  l_value = tpitrn.
  l_element_dummy = l_document->create_simple_element(
              name = 'ticSicilNo'
              value = l_value
              parent = l_element_hsv  ).
  l_value = tpiema.
  l_element_dummy = l_document->create_simple_element(
              name = 'eposta'
              value = l_value
              parent = l_element_hsv  ).
  l_value = tpitac.
  l_element_dummy = l_document->create_simple_element(
              name = 'alanKodu'
              value = l_value
              parent = l_element_hsv  ).
  l_value = tpitel.
  l_element_dummy = l_document->create_simple_element(
              name = 'telNo'
              value = l_value
              parent = l_element_hsv  ).
  l_element_duzenleyen  = l_document->create_simple_element(
              name = 'duzenleyen'
              parent = l_element_genel  ).
  l_value = tpitno.
  l_element_dummy  = l_document->create_simple_element(
              name = 'vergiNo'
              value = l_value
              parent = l_element_duzenleyen  ).
  l_value = tpisnm.
  l_element_dummy = l_document->create_simple_element(
              name = 'soyadi'
              value = l_value
              parent = l_element_duzenleyen  ).
  l_value = tpinam.
  l_element_dummy = l_document->create_simple_element(
              name = 'adi'
              value = l_value
              parent = l_element_duzenleyen  ).
  l_value = tpitrn.
  l_element_dummy = l_document->create_simple_element(
              name = 'ticSicilNo'
              value = l_value
              parent = l_element_duzenleyen  ).
  l_value = tpiema.
  l_element_dummy = l_document->create_simple_element(
              name = 'eposta'
              value = l_value
              parent = l_element_duzenleyen  ).
  l_value = tpitac.
  l_element_dummy = l_document->create_simple_element(
              name = 'alanKodu'
              value = l_value
              parent = l_element_duzenleyen  ).
  l_value = tpitel.
  l_element_dummy = l_document->create_simple_element(
              name = 'telNo'
              value = l_value
              parent = l_element_duzenleyen  ).
  l_element_ozel  = l_document->create_simple_element(
              name = 'ozel'
              parent = l_element_beyanname  ).

  IF toplam NE 0.
    l_element_satisbildirimi = l_document->create_simple_element(
                name = 'satisBildirimi'
                parent = l_element_ozel  ).
    LOOP AT itabbsh.
      l_element_bs = l_document->create_simple_element(
                  name = 'bs'
                  parent = l_element_satisbildirimi  ).
      sortn = itabbsh-sortn.
      l_value = sortn.
      l_element_dummy = l_document->create_simple_element(
                  name = 'siraNo'
                  value = l_value
                  parent = l_element_bs  ).
      l_value = itabbsh-name1.
      l_element_dummy = l_document->create_simple_element(
                  name = 'unvan'
                  value = l_value
                  parent = l_element_bs  ).
      l_value = itabbsh-landa.
      l_element_dummy = l_document->create_simple_element(
                  name = 'ulke'
                  value = l_value
                  parent = l_element_bs  ).
      l_value = itabbsh-stcd1.
      l_element_dummy = l_document->create_simple_element(
                  name = 'vkno'
                  value = l_value
                  parent = l_element_bs  ).
      l_value = itabbsh-stcd3.
      l_element_dummy = l_document->create_simple_element(
                  name = 'tckimlikno'
                  value = l_value
                  parent = l_element_bs  ).
      l_value = itabbsh-nofdc.
      l_element_dummy = l_document->create_simple_element(
                  name = 'belgeSayisi'
                  value = l_value
                  parent = l_element_bs  ).
      tutar = trunc( itabbsh-hwbas ).
      WRITE tutar TO tutarstr DECIMALS 0 NO-GROUPING.
      CONDENSE tutarstr.
      l_value = tutarstr.
      l_element_dummy = l_document->create_simple_element(
                  name = 'malHizmetBedeli'
                  value = l_value
                  parent = l_element_bs  ).
    ENDLOOP.
    CONDENSE toplamstr.
    l_value = toplamstr.
    l_element_dummy = l_document->create_simple_element(
                name = 'toplamSatisBedeli'
                value = l_value
                parent = l_element_ozel  ).
  ENDIF.
  IF toplam_o NE 0.
    CONDENSE toplamstr_o.
    l_value = toplamstr_o.
    l_element_dummy = l_document->create_simple_element(
                name = 'digerMalHizBedelTop'
                value = l_value
                parent = l_element_ozel  ).
  ENDIF.
  CONDENSE toplamstr_gen.
  l_value = toplamstr_gen.
  l_element_dummy = l_document->create_simple_element(
              name = 'genelToplam'
              value = l_value
              parent = l_element_ozel  ).

  l_streamfactory = l_ixml->create_stream_factory( ).
  l_ostream = l_streamfactory->create_ostream_itable(
              table = l_xml_table ).
  l_renderer = l_ixml->create_renderer( ostream  = l_ostream
                                        document = l_document ).
  l_rc = l_renderer->render( ).
  l_xml_size = l_ostream->get_num_written_raw( ).

  PERFORM down_xtable USING p_fnambs l_xml_table[] l_xml_size 'S'.

ENDFORM.                    "cnv_bs_to_xml

*---------------------------------------------------------------------*
*       FORM cnv_ba_to_xml                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM cnv_ba_to_xml.
  DATA: l_ixml            TYPE REF TO if_ixml,
        l_streamfactory   TYPE REF TO if_ixml_stream_factory,
        l_ostream         TYPE REF TO if_ixml_ostream,
        l_renderer        TYPE REF TO if_ixml_renderer,
        l_document        TYPE REF TO if_ixml_document.

  DATA: l_element_beyanname      TYPE REF TO if_ixml_element,
        l_element_genel          TYPE REF TO if_ixml_element,
        l_element_idari          TYPE REF TO if_ixml_element,
        l_element_vdkodu         TYPE REF TO if_ixml_element,
        l_element_donem          TYPE REF TO if_ixml_element,
        l_element_dummy          TYPE REF TO if_ixml_element,
        l_element_mukellef       TYPE REF TO if_ixml_element,
        l_element_hsv            TYPE REF TO if_ixml_element,
        l_element_duzenleyen     TYPE REF TO if_ixml_element,
        l_element_ozel           TYPE REF TO if_ixml_element,
        l_element_alisbildirimi  TYPE REF TO if_ixml_element,
        l_element_ba             TYPE REF TO if_ixml_element,
        l_value                  TYPE string.

  DATA: l_xml_table       TYPE TABLE OF xml_line,
        l_xml_size        TYPE i,
        l_rc              TYPE i,
        toplam            TYPE p DECIMALS 2,
        toplam_o          TYPE p DECIMALS 2,
        toplam_gen        TYPE p DECIMALS 2,
        tutar             TYPE p DECIMALS 2,
        toplamstr(30),
        toplamstr_o(30),
        toplamstr_gen(30),
        tutarstr(30),
        l_year(4),
        l_monat TYPE i,
        l_monat_str(2),
        sortn             TYPE i,
        s_encoding_type   TYPE string,
        encoding          TYPE REF TO if_ixml_encoding.

  CHECK sy-batch NE 'X' OR c_saveba EQ 'X'.
  IF NOT s_gjahr IS INITIAL.
    l_year = s_gjahr-low.
  ELSEIF NOT s_budat IS INITIAL.
    l_year = s_budat-low(4).
  ELSEIF NOT s_bldat IS INITIAL.
    l_year = s_bldat-low(4).
  ENDIF.
  IF NOT s_monat IS INITIAL.
    l_monat = s_monat-low.
  ELSEIF NOT s_budat IS INITIAL.
    l_monat = s_budat-low+4(2).
  ELSEIF NOT s_bldat IS INITIAL.
    l_monat = s_bldat-low+4(2).
  ENDIF.
  WRITE l_monat TO l_monat_str.
  CONDENSE l_monat_str.

  toplam = 0.
  toplam_o = 0.
  toplam_gen = 0.
  LOOP AT itabbah.
    toplam = toplam + trunc( itabbah-hwbas ).
  ENDLOOP.
  WRITE toplam TO toplamstr DECIMALS 0 NO-GROUPING.

  LOOP AT itabbah_o.
    toplam_o = toplam_o + trunc( itabbah_o-hwbas ).
  ENDLOOP.
  WRITE toplam_o TO toplamstr_o DECIMALS 0 NO-GROUPING.

  toplam_gen = toplam + toplam_o.
  WRITE toplam_gen TO toplamstr_gen DECIMALS 0 NO-GROUPING.

  CHECK toplam_gen NE 0.

  l_ixml = cl_ixml=>create( ).
  l_document = l_ixml->create_document( ).

  s_encoding_type = 'ISO-8859-9'.
  encoding =  l_ixml->create_encoding(
             character_set = s_encoding_type
             byte_order = 0 ).

  l_document->set_encoding( encoding ).
  l_element_beyanname  = l_document->create_simple_element(
                  name = 'beyanname'
                parent = l_document ).

  IF l_year < '2008'.
    l_value = 'FORMBA_1'.
  ELSE.
    l_value = 'FORMBA_2'.
  ENDIF.
  l_rc = l_element_beyanname->set_attribute( name = 'kodVer'
                              value = l_value ).

  IF l_year < '2009'.
    l_value = 'YTL'.
  ELSE.
    l_value = 'TL'.
  ENDIF.
  l_rc = l_element_beyanname->set_attribute( name = 'paraBirimi'
                              value = l_value ).

  l_value = 'http://www.w3.org/2001/XMLSchema-instance'.
  l_rc = l_element_beyanname->set_attribute(
      name = 'xmlns:xsi'
     value = l_value ).

  IF l_year < '2008'.
    l_value = 'FORMBA_1.xsd'.
  ELSE.
    l_value = 'FORMBA_2.xsd'.
  ENDIF.
  l_rc = l_element_beyanname->set_attribute(
      name = 'xsi:noNamespaceSchemaLocation'
     value = l_value ).

  l_element_genel  = l_document->create_simple_element(
              name = 'genel'
              parent = l_element_beyanname  ).

  l_element_idari  = l_document->create_simple_element(
              name = 'idari'
              parent = l_element_genel  ).
  l_value = tpitoc.
  l_element_dummy  = l_document->create_simple_element(
              name = 'vdKodu'
              value = l_value
              parent = l_element_idari  ).
  l_element_donem  = l_document->create_simple_element(
              name = 'donem'
              parent = l_element_idari  ).

  IF l_year < '2008'.
    l_value = 'yillik'.
  ELSE.
    l_value = 'aylik'.
  ENDIF.
  l_element_dummy  = l_document->create_simple_element(
             name = 'tip'
             value = l_value
             parent = l_element_donem  ).
  l_value = l_year.
  l_element_dummy = l_document->create_simple_element(
             name = 'yil'
             value = l_value
             parent = l_element_donem  ).
  IF l_year >= '2008'.
    l_value = l_monat_str.
    l_element_dummy = l_document->create_simple_element(
               name = 'ay'
               value = l_value
               parent = l_element_donem  ).
  ENDIF.

  l_element_mukellef  = l_document->create_simple_element(
              name = 'mukellef'
              parent = l_element_genel  ).
  l_value = tpitno.
  l_element_dummy  = l_document->create_simple_element(
              name = 'vergiNo'
              value = l_value
              parent = l_element_mukellef  ).
  l_value = tpisnm.
  l_element_dummy = l_document->create_simple_element(
              name = 'soyadi'
              value = l_value
              parent = l_element_mukellef  ).
  l_value = tpinam.
  l_element_dummy = l_document->create_simple_element(
              name = 'adi'
              value = l_value
              parent = l_element_mukellef  ).
  l_value = tpitrn.
  l_element_dummy = l_document->create_simple_element(
              name = 'ticSicilNo'
              value = l_value
              parent = l_element_mukellef  ).
  l_value = tpiema.
  l_element_dummy = l_document->create_simple_element(
              name = 'eposta'
              value = l_value
              parent = l_element_mukellef  ).
  l_value = tpitac.
  l_element_dummy = l_document->create_simple_element(
              name = 'alanKodu'
              value = l_value
              parent = l_element_mukellef  ).
  l_value = tpitel.
  l_element_dummy = l_document->create_simple_element(
              name = 'telNo'
              value = l_value
              parent = l_element_mukellef  ).
  l_element_hsv  = l_document->create_simple_element(
              name = 'hsv'
              parent = l_element_genel  ).
  l_value = 'kendisi'.
  l_rc = l_element_hsv->set_attribute( name = 'sifat'
                              value = l_value ).
  l_value = tpitno.
  l_element_dummy  = l_document->create_simple_element(
              name = 'vergiNo'
              value = l_value
              parent = l_element_hsv  ).
  l_value = tpisnm.
  l_element_dummy = l_document->create_simple_element(
              name = 'soyadi'
              value = l_value
              parent = l_element_hsv  ).
  l_value = tpinam.
  l_element_dummy = l_document->create_simple_element(
              name = 'adi'
              value = l_value
              parent = l_element_hsv  ).
  l_value = tpitrn.
  l_element_dummy = l_document->create_simple_element(
              name = 'ticSicilNo'
              value = l_value
              parent = l_element_hsv  ).
  l_value = tpiema.
  l_element_dummy = l_document->create_simple_element(
              name = 'eposta'
              value = l_value
              parent = l_element_hsv  ).
  l_value = tpitac.
  l_element_dummy = l_document->create_simple_element(
              name = 'alanKodu'
              value = l_value
              parent = l_element_hsv  ).
  l_value = tpitel.
  l_element_dummy = l_document->create_simple_element(
              name = 'telNo'
              value = l_value
              parent = l_element_hsv  ).
  l_element_duzenleyen  = l_document->create_simple_element(
              name = 'duzenleyen'
              parent = l_element_genel  ).
  l_value = tpitno.
  l_element_dummy  = l_document->create_simple_element(
              name = 'vergiNo'
              value = l_value
              parent = l_element_duzenleyen  ).
  l_value = tpisnm.
  l_element_dummy = l_document->create_simple_element(
              name = 'soyadi'
              value = l_value
              parent = l_element_duzenleyen  ).
  l_value = tpinam.
  l_element_dummy = l_document->create_simple_element(
              name = 'adi'
              value = l_value
              parent = l_element_duzenleyen  ).
  l_value = tpitrn.
  l_element_dummy = l_document->create_simple_element(
              name = 'ticSicilNo'
              value = l_value
              parent = l_element_duzenleyen  ).
  l_value = tpiema.
  l_element_dummy = l_document->create_simple_element(
              name = 'eposta'
              value = l_value
              parent = l_element_duzenleyen  ).
  l_value = tpitac.
  l_element_dummy = l_document->create_simple_element(
              name = 'alanKodu'
              value = l_value
              parent = l_element_duzenleyen  ).
  l_value = tpitel.
  l_element_dummy = l_document->create_simple_element(
              name = 'telNo'
              value = l_value
              parent = l_element_duzenleyen  ).
  l_element_ozel  = l_document->create_simple_element(
              name = 'ozel'
              parent = l_element_beyanname  ).

  IF toplam NE 0.
    l_element_alisbildirimi = l_document->create_simple_element(
                name = 'alisBildirimi'
                parent = l_element_ozel  ).
    LOOP AT itabbah.
      l_element_ba = l_document->create_simple_element(
                  name = 'ba'
                  parent = l_element_alisbildirimi  ).
      sortn = itabbah-sortn.
      l_value = sortn.
      l_element_dummy = l_document->create_simple_element(
                  name = 'siraNo'
                  value = l_value
                  parent = l_element_ba  ).
      l_value = itabbah-name1.
      l_element_dummy = l_document->create_simple_element(
                  name = 'unvan'
                  value = l_value
                  parent = l_element_ba  ).
      l_value = itabbah-landa.
      l_element_dummy = l_document->create_simple_element(
                  name = 'ulke'
                  value = l_value
                  parent = l_element_ba  ).
      l_value = itabbah-stcd1.
      l_element_dummy = l_document->create_simple_element(
                  name = 'vkno'
                  value = l_value
                  parent = l_element_ba  ).
      l_value = itabbah-stcd3.
      l_element_dummy = l_document->create_simple_element(
                  name = 'tckimlikno'
                  value = l_value
                  parent = l_element_ba  ).
      l_value = itabbah-nofdc.
      l_element_dummy = l_document->create_simple_element(
                  name = 'belgeSayisi'
                  value = l_value
                  parent = l_element_ba  ).
      tutar = trunc( itabbah-hwbas ).
      WRITE tutar TO tutarstr DECIMALS 0 NO-GROUPING.
      CONDENSE tutarstr.
      l_value = tutarstr.
      l_element_dummy = l_document->create_simple_element(
                  name = 'malHizmetBedeli'
                  value = l_value
                  parent = l_element_ba  ).
    ENDLOOP.
    CONDENSE toplamstr.
    l_value = toplamstr.
    l_element_dummy = l_document->create_simple_element(
                name = 'toplamSatisBedeli'
                value = l_value
                parent = l_element_ozel  ).
  ENDIF.
  IF toplam_o NE 0.
    CONDENSE toplamstr_o.
    l_value = toplamstr_o.
    l_element_dummy = l_document->create_simple_element(
                name = 'digerMalHizBedelTop'
                value = l_value
                parent = l_element_ozel  ).
  ENDIF.
  CONDENSE toplamstr_gen.
  l_value = toplamstr_gen.
  l_element_dummy = l_document->create_simple_element(
              name = 'genelToplam'
              value = l_value
              parent = l_element_ozel  ).

  l_streamfactory = l_ixml->create_stream_factory( ).
  l_ostream = l_streamfactory->create_ostream_itable(
              table = l_xml_table ).
  l_renderer = l_ixml->create_renderer( ostream  = l_ostream
                                        document = l_document ).
  l_rc = l_renderer->render( ).
  l_xml_size = l_ostream->get_num_written_raw( ).

  PERFORM down_xtable USING p_fnamba l_xml_table[] l_xml_size 'A'.


ENDFORM.                    "cnv_ba_to_xml
*&---------------------------------------------------------------------*
*&      Form  badi_modify
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM badi_modify .
  DATA: tbukrs TYPE /saptr/bukrs,
        tgjahr TYPE /saptr/gjahr,
        tbudat TYPE /saptr/budat,
        tbldat TYPE /saptr/bldat,
        tmonat TYPE /saptr/monat,
        pouttx TYPE /saptr/outtx,
        pinptx TYPE /saptr/inptx.

* BADI initialization - change dynamical selections
* -------------------------------------------------
  CALL METHOD cl_exithandler=>get_instance
    CHANGING
      instance = exit.

  tbukrs[] = s_bukrs[].
  tgjahr[] = s_gjahr[].
  tmonat[] = s_monat[].
  tbudat[] = s_budat[].
  tbldat[] = s_bldat[].
  pouttx   = p_outtx.
  pinptx   = p_inptx.

* Call methode.
  CALL METHOD exit->modify_forms
    EXPORTING
      t_bukrs = tbukrs
      t_monat = tmonat
      t_budat = tbudat
      t_bldat = tbldat
      t_gjahr = tgjahr
    CHANGING
      itabbsd = itabbsd[]
      itabbad = itabbad[]
      itabbsh = itabbsh[]
      itabbah = itabbah[].

ENDFORM.                    " badi_modify
