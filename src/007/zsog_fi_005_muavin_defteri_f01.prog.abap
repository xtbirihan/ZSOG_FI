*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_005_MUAVIN_DEFTERI_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  prepare_alv_table
*&---------------------------------------------------------------------*
FORM prepare_alv_table.
  IF itab[] IS INITIAL.
    MESSAGE s886(f4).
    SUBMIT (sy-repid) VIA SELECTION-SCREEN.
  ENDIF.
* SORT ITAB BY HKONT WAERS TYPE BUDAT BLDAT XBLNR.
  SORT itab BY hkont saknr budat."budat ."belnr .
  CLEAR devir.
  LOOP AT itab.

***      Ters Kayıt Kontrolü eklentileri
    IF p_trs IS INITIAL AND itab-stblg IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR rtab.
    MOVE : sy-tabix   TO rtab-tabix   ,
           t001-waers TO rtab-hwaer   .

    IF p_bpb = 'X'.
      itab-tutar = itab-wrbtr .
    ELSE.
      itab-tutar = itab-dmbtr .
*      itab-waers = t001-waers.
    ENDIF.
*    IF itab-shkzg EQ 'H' .
*      itab-dmbtr = itab-dmbtr * -1 .
*      itab-wrbtr = itab-wrbtr * -1 .
*    ENDIF.

    rtab-name = name.
*    RTAB-DEVIR = DEVIR-DMBTR.
    MOVE-CORRESPONDING itab TO rtab.
    IF p_gl = 'X'.
      IF itab-hkont NE devir-hkont.
        PERFORM header_dvr.
        cumule = cumule + devir-dmbtr.
        PERFORM item_line.
      ELSE.
        PERFORM item_line.
      ENDIF.
    ELSE.
      IF itab-hkont NE devir-mu_sa.
        PERFORM header_dvr.
        cumule = cumule + devir-dmbtr.
        PERFORM item_line.
      ELSE.
        IF itab-saknr NE devir-hkont .
          PERFORM header_dvr.
          cumule = cumule + devir-dmbtr.
          PERFORM item_line.
        ELSE.
          PERFORM item_line.
        ENDIF.
      ENDIF.
    ENDIF.
    PERFORM read_yevmiye_no.
    IF NOT itab-umskz EQ ''.
      IF NOT itab-lifnr IS INITIAL.
        SELECT SINGLE ltext INTO rtab-ltext
          FROM t074t
         WHERE spras = sy-langu
           AND shbkz = itab-umskz
           AND koart = 'K'.
      ELSEIF NOT itab-kunnr IS INITIAL.
        SELECT SINGLE ltext INTO rtab-ltext
          FROM t074t
         WHERE spras = sy-langu
           AND shbkz = itab-umskz
           AND koart = 'D'.

      ENDIF.
    ENDIF.
    IF itab-lifnr NE ''.
      SELECT SINGLE stcd2 INTO rtab-stcd2
        FROM lfa1
       WHERE lifnr = itab-lifnr.
    ENDIF.
    IF itab-kunnr NE ''.
      SELECT SINGLE stcd2 INTO rtab-stcd2
        FROM kna1
       WHERE kunnr = itab-kunnr.
    ENDIF.

    APPEND rtab.
  ENDLOOP.
  IF p_dvr = 'X'.
    LOOP AT devir WHERE islendi <> 'X'.
      PERFORM add_devir.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " prepare_alv_table

*&---------------------------------------------------------------------*
*&      Form  get_initial_GL
*&---------------------------------------------------------------------*
FORM get_initial_gl.

  DATA: lv_ldgrp TYPE bkpf-ldgrp,
        lv_wrbtr TYPE bsis-wrbtr ,
        lt_bkpf LIKE bkpf OCCURS 0 WITH HEADER LINE ,
        lv_bstat TYPE bkpf-bstat.
*  IF p_rldnr = '02' .
*    CLEAR : lt_bkpf, lt_bkpf[].
**  select * from bkpf into corresponding fields of table lt_bkpf
**           where bukrs = p_bukrs
**             and budat lt S_BLDAT-LOW
**             and
*    SELECT p~racct p~hsl p~rtcur p~wsl c~ldgrp c~bstat
*       INTO (devir-hkont,devir-dmbtr,devir-waers,lv_wrbtr,
*             lv_ldgrp, lv_bstat)
*         FROM bkpf AS c
*         INNER JOIN faglflexa AS p ON     c~bukrs = p~rbukrs
*                                AND c~gjahr = p~ryear
*                                AND  c~belnr = p~docnr
*           WHERE p~rbukrs =  p_bukrs AND
*                 p~racct IN p_hkont AND
**               P~ZUONR IN S_ZUONR AND
*                 c~budat LT s_bldat-low AND
*                 c~blart IN s_blart AND
**               p~prctr IN s_prctr AND
**               p~prctr IN s_prctr AND
*                 p~rbusa IN s_gsber AND
**               P~WERKS IN S_WERKS AND
*                 p~cost_elem IN s_kostl AND
**               P~AUFNR IN S_AUFNR AND
**               P~XREF3 IN S_XREF3 AND
*                ( ( c~ldgrp = p_rldnr AND c~bstat = 'L' ) OR
*                 ( c~ldgrp = '' AND c~bstat = '' ) ) AND
*                 p~rldnr = p_rldnr AND
*                 c~usnam IN s_usnam AND
*                 c~xref1_hd IN s_xref1h AND
*                 c~xref2_hd IN s_xref2h
*                    .
**
*"YT27072006
*
***--------------------------------------------------------------------*
**    CHECK ( LV_LDGRP EQ P_RLDNR AND LV_BSTAT = 'L' ) OR
**          ( LV_LDGRP EQ '' AND LV_BSTAT = '' ).
***--------------------------------------------------------------------*
*      IF p_bpb = 'X'.
*        devir-dmbtr  = lv_wrbtr .
*      ELSE.
*        devir-waers  = t001-waers.
*      ENDIF.
*      IF devir-dmbtr < 0 .
**      DEVIR-DMBTR = DEVIR-DMBTR * -1.
*** alacak borc durumu
*        devir-borc  = devir-dmbtr .
*      ELSE.
*        devir-alck  = devir-dmbtr .
*      ENDIF.
*      COLLECT devir.CLEAR devir.
*    ENDSELECT.
***********
*
*  ELSE .
*    SELECT p~hkont p~shkzg p~dmbtr p~waers p~wrbtr c~ldgrp c~bstat
*       INTO (devir-hkont,bsis-shkzg,devir-dmbtr,devir-waers,bsis-wrbtr,
*             lv_ldgrp, lv_bstat)
*         FROM bsis AS p
*         INNER JOIN bkpf AS c ON    p~bukrs   = c~bukrs
*                                AND p~gjahr   = c~gjahr
*                                AND p~belnr   = c~belnr
*           WHERE p~bukrs =  p_bukrs AND
*                 p~hkont IN p_hkont AND
*                 p~zuonr IN s_zuonr AND
*                 p~budat LT s_bldat-low AND
*                 p~blart IN s_blart AND
**               p~prctr IN s_prctr AND
**               p~prctr IN s_prctr AND
*                 p~gsber IN s_gsber AND
*                 p~werks IN s_werks AND
*                 p~kostl IN s_kostl AND
*                 p~aufnr IN s_aufnr AND
*                 p~xref3 IN s_xref3 AND
**              ( ( C~LDGRP = P_RLDNR AND C~BSTAT = 'L' ) OR
**               ( C~LDGRP = '' AND C~BSTAT = '' ) ) AND
*               ( ( c~rldnr = p_rldnr AND c~bstat = 'L' ) OR
*                 ( c~rldnr = '' AND c~bstat = '' ) ) AND
*                 c~usnam IN s_usnam AND
*                 c~xref1_hd IN s_xref1h AND
*                 c~xref2_hd IN s_xref2h.
*                                                            "YT27072006
*
***--------------------------------------------------------------------*
**
**    CHECK ( LV_LDGRP EQ P_RLDNR AND LV_BSTAT = 'L' ) OR
**          ( LV_LDGRP EQ '' AND LV_BSTAT = '' ).
***--------------------------------------------------------------------*
*      IF p_bpb = 'X'.
*        devir-dmbtr  = bsis-wrbtr .
*      ELSE.
*        devir-waers  = t001-waers.
*      ENDIF.
*      IF bsis-shkzg = 'H'.
*        devir-dmbtr = devir-dmbtr * -1.
*** alacak borc durumu
*        devir-borc  = devir-dmbtr .
*      ELSE.
*        devir-alck  = devir-dmbtr .
*      ENDIF.
*      COLLECT devir.CLEAR devir.
*    ENDSELECT.
***********
*
*    SELECT p~hkont p~shkzg p~dmbtr p~waers p~wrbtr
*       INTO (devir-hkont,bsas-shkzg,devir-dmbtr,devir-waers,bsas-wrbtr)
*         FROM bsas AS p
*         INNER JOIN bkpf AS c ON    p~bukrs   = c~bukrs
*                                AND p~gjahr   = c~gjahr
*                                AND p~belnr   = c~belnr
*           WHERE p~bukrs =  p_bukrs AND
*                 p~hkont IN p_hkont AND
*                 p~zuonr IN s_zuonr AND
*                 p~budat LT s_bldat-low AND
*                 p~blart IN s_blart AND
**               p~prctr IN s_prctr AND
**               p~prctr IN s_prctr AND
*                 p~gsber IN s_gsber AND
*                 p~werks IN s_werks AND
*                 p~kostl IN s_kostl AND
*                 p~aufnr IN s_aufnr AND
*                 p~xref3 IN s_xref3 AND
*                 c~usnam IN s_usnam AND
*                     c~xref1_hd IN s_xref1h AND
*                 c~xref2_hd IN s_xref2h.
*                                                            "YT27072006
*      IF p_bpb = 'X'.
*        devir-dmbtr  = bsas-wrbtr .
*      ELSE.
*        devir-waers  = t001-waers.
*      ENDIF.
*      IF bsas-shkzg = 'H'.
*        devir-dmbtr = devir-dmbtr * -1.
*** alacak borc durumu
*        devir-borc  = devir-dmbtr .
*      ELSE.
*        devir-alck  = devir-dmbtr .
*      ENDIF.
*      COLLECT devir.CLEAR devir.
*    ENDSELECT.
*  ENDIF.
ENDFORM.                    " get_initial_gl
*&---------------------------------------------------------------------*
*&      Form  get_data_gl
*&---------------------------------------------------------------------*
FORM get_data_gl.
  DATA: ls_bseg LIKE bseg.

  SELECT * FROM bsis
           WHERE bukrs = p_bukrs  AND
                 hkont IN p_hkont AND
                 zuonr IN s_zuonr AND
                 budat IN s_bldat AND
                 blart IN s_blart AND
*                 prctr IN s_prctr AND
*                 prctr IN s_prctr AND
                 gsber IN s_gsber AND
                 werks IN s_werks AND
                 aufnr IN s_aufnr AND
                 xref3 IN s_xref3 AND
                 kostl IN s_kostl .

    MOVE-CORRESPONDING bsis TO itab.
**   barise 09.07.2013 PYP kodu yanına açıklaması da gelsin.
*    DATA ls_objnr TYPE j_objnr.
*    CONCATENATE 'PR' itab-projk INTO ls_objnr.
*    SELECT SINGLE post1 INTO itab-post1
*    FROM prps
*    WHERE objnr EQ ls_objnr.
**   barise 09.07.2013 PYP kodu yanına açıklaması da gelsin.

    itab-valor = itab-valut - itab-bldat.
    itab-saknr = itab-hkont. " YT20061218.
    SELECT SINGLE ktext INTO itab-aufnr_text FROM coas WHERE
                            aufnr = bsis-aufnr.

    CLEAR: ls_bseg.
    SELECT SINGLE * FROM bseg INTO ls_bseg
     WHERE belnr = bsis-belnr
       AND gjahr = bsis-gjahr
       AND bukrs = bsis-bukrs
       AND buzei = bsis-buzei.
    itab-xref1 = ls_bseg-xref1          .
    itab-xref2 = ls_bseg-xref2          .
    itab-zuonr  = ls_bseg-zuonr         .
    IF itab-menge > 0.
      itab-bfiyat = itab-dmbtr / itab-menge.
    ENDIF.


    IF p_chk NE 'X' AND itab-valut NE '00000000'.
      itab-gecik = sy-datum   - itab-valut.
    ENDIF.

    PERFORM find_bwwrt USING bsis CHANGING itab-bwwrt.

    SELECT SINGLE * FROM bkpf INTO CORRESPONDING FIELDS OF itab
         WHERE bukrs = p_bukrs    AND
               belnr = itab-belnr AND
               gjahr = itab-gjahr AND
*               usnam IN s_usnam AND
               xref1_hd IN s_xref1h AND
               xref2_hd IN s_xref2h.
    IF sy-subrc EQ 0.
      PERFORM fill_appended_fields USING itab.
****      IF itab-menge > 0.
****        itab-bfiyat = itab-dmbtr / itab-menge.
****      ENDIF.
* defteri kebir görünümü kar merkezi
****      CLEAR faglflexa . "damlap açılacak
****      SELECT * FROM faglflexa WHERE ryear = itab-gjahr AND
****                                    docnr = itab-belnr AND
****                                   rbukrs = itab-bukrs AND
****                                   buzei  = itab-buzei ."AND
*****                                   rldnr  = p_rldnr.
****
****        itab-dmbtr = faglflexa-hsl.
*****        itab-wrbtr = faglflexa-tsl.
****        itab-wrbtr = faglflexa-wsl.
*****        itab-prctr = faglflexa-prctr.
****        itab-gsber  = faglflexa-rbusa.
        APPEND itab.
        s_belnr-option = 'EQ' .
        s_belnr-sign   = 'I'.
        s_belnr-low = itab-belnr.
        APPEND s_belnr.
****
****      ENDSELECT.
****      CLEAR itab.
    ENDIF.
  ENDSELECT.


*--------------------------------------------------------------------*
*--------------------------------------------------------------------*

  DATA: lss_bseg LIKE bseg_add.
  DATA: lv_line LIKE sy-index .


  IF s_belnr[] IS NOT INITIAL.
    DESCRIBE TABLE s_belnr LINES lv_line .

    IF lv_line < 2000 .
      SELECT * FROM faglflexa
               WHERE rbukrs = p_bukrs  AND
                     racct IN p_hkont AND
                     docnr NOT IN s_belnr AND
*                 ZUONR IN S_ZUONR AND
                     budat IN s_bldat ."AND
*                 BLART IN S_BLART AND
*                 GSBER IN S_GSBER AND
*                 WERKS IN S_WERKS AND
*                 AUFNR IN S_AUFNR AND
*                 XREF3 IN S_XREF3 AND
*                 KOSTL IN S_KOSTL
*                      rldnr  = p_rldnr.


        CLEAR: lss_bseg.
        SELECT SINGLE * FROM bseg_add INTO lss_bseg
         WHERE belnr = faglflexa-belnr
           AND gjahr = faglflexa-gjahr
           AND bukrs = faglflexa-rbukrs
           AND buzei = faglflexa-buzei
           AND werks IN s_werks
           AND gsber IN s_gsber
           AND aufnr IN s_aufnr
           AND zuonr IN s_zuonr
           AND xref3 IN s_xref3
           AND kostl IN s_kostl .

        MOVE-CORRESPONDING faglflexa TO itab.
        MOVE-CORRESPONDING faglflexa TO bsis.
        MOVE-CORRESPONDING bsis TO itab.
        MOVE-CORRESPONDING lss_bseg TO itab.

        itab-valor = itab-valut - itab-bldat.
        itab-saknr = itab-hkont. " YT20061218.
        SELECT SINGLE ktext INTO itab-aufnr_text FROM coas WHERE
                                aufnr = bsis-aufnr.


        itab-xref1 = lss_bseg-xref1          .
        itab-xref2 = lss_bseg-xref2          .
        itab-zuonr = lss_bseg-zuonr         .
        IF itab-menge > 0.
          itab-bfiyat = itab-dmbtr / itab-menge.
        ENDIF.


        IF p_chk NE 'X' AND itab-valut NE '00000000'.
          itab-gecik = sy-datum   - itab-valut.
        ENDIF.

        PERFORM find_bwwrt USING bsis CHANGING itab-bwwrt.

        SELECT SINGLE * FROM bkpf INTO CORRESPONDING FIELDS OF itab
             WHERE bukrs = p_bukrs    AND
                   belnr = itab-belnr AND
                   gjahr = itab-gjahr AND
                   usnam IN s_usnam AND
                   blart IN s_blart AND
                   xref1_hd IN s_xref1h AND
                   xref2_hd IN s_xref2h.
        IF sy-subrc EQ 0.
          PERFORM fill_appended_fields USING itab.
          IF itab-menge > 0.
            itab-bfiyat = itab-dmbtr / itab-menge.
          ENDIF.
          itab-dmbtr = faglflexa-hsl.
          itab-wrbtr = faglflexa-wsl.
          itab-gsber  = faglflexa-rbusa.
          APPEND itab.
          CLEAR itab.
        ENDIF.
      ENDSELECT.
    ELSE .
      SELECT * FROM faglflexa
               WHERE rbukrs = p_bukrs  AND
                     racct IN p_hkont AND
*                 ZUONR IN S_ZUONR AND
                     budat IN s_bldat ."AND
*                 BLART IN S_BLART AND
*                 GSBER IN S_GSBER AND
*                 WERKS IN S_WERKS AND
*                 AUFNR IN S_AUFNR AND
*                 XREF3 IN S_XREF3 AND
*                 KOSTL IN S_KOSTL
*                      rldnr  = p_rldnr.

        IF faglflexa-docnr IN s_belnr .
          CONTINUE .
        ENDIF.
        CLEAR: lss_bseg.
        SELECT SINGLE * FROM bseg_add INTO lss_bseg
         WHERE belnr = faglflexa-belnr
           AND gjahr = faglflexa-gjahr
           AND bukrs = faglflexa-rbukrs
           AND buzei = faglflexa-buzei
           AND werks IN s_werks
           AND gsber IN s_gsber
           AND aufnr IN s_aufnr
           AND zuonr IN s_zuonr
           AND xref3 IN s_xref3
           AND kostl IN s_kostl .

        MOVE-CORRESPONDING faglflexa TO itab.
        MOVE-CORRESPONDING faglflexa TO bsis.
        MOVE-CORRESPONDING bsis TO itab.
        MOVE-CORRESPONDING lss_bseg TO itab.

        itab-valor = itab-valut - itab-bldat.
        itab-saknr = itab-hkont. " YT20061218.
        SELECT SINGLE ktext INTO itab-aufnr_text FROM coas WHERE
                                aufnr = bsis-aufnr.


        itab-xref1 = lss_bseg-xref1          .
        itab-xref2 = lss_bseg-xref2          .
        itab-zuonr = lss_bseg-zuonr         .
        IF itab-menge > 0.
          itab-bfiyat = itab-dmbtr / itab-menge.
        ENDIF.


        IF p_chk NE 'X' AND itab-valut NE '00000000'.
          itab-gecik = sy-datum   - itab-valut.
        ENDIF.

        PERFORM find_bwwrt USING bsis CHANGING itab-bwwrt.

        SELECT SINGLE * FROM bkpf INTO CORRESPONDING FIELDS OF itab
             WHERE bukrs = p_bukrs    AND
                   belnr = itab-belnr AND
                   gjahr = itab-gjahr AND
                   usnam IN s_usnam AND
                   blart IN s_blart AND
                   xref1_hd IN s_xref1h AND
                   xref2_hd IN s_xref2h.
        IF sy-subrc EQ 0.
          PERFORM fill_appended_fields USING itab.
          IF itab-menge > 0.
            itab-bfiyat = itab-dmbtr / itab-menge.
          ENDIF.
          itab-dmbtr = faglflexa-hsl.
          itab-wrbtr = faglflexa-wsl.
          itab-gsber  = faglflexa-rbusa.
          APPEND itab.
          CLEAR itab.
        ENDIF.
      ENDSELECT.
    ENDIF .
  ELSE.

    SELECT * FROM faglflexa
             WHERE rbukrs = p_bukrs  AND
                   racct IN p_hkont AND
*                 ZUONR IN S_ZUONR AND
                   budat IN s_bldat ."AND
*                 BLART IN S_BLART AND
*                 GSBER IN S_GSBER AND
*                 WERKS IN S_WERKS AND
*                 AUFNR IN S_AUFNR AND
*                 XREF3 IN S_XREF3 AND
*                 KOSTL IN S_KOSTL
*                    rldnr  = p_rldnr.


      CLEAR: lss_bseg.
      SELECT SINGLE * FROM bseg_add INTO lss_bseg
       WHERE belnr = faglflexa-belnr
         AND gjahr = faglflexa-gjahr
         AND bukrs = faglflexa-rbukrs
         AND buzei = faglflexa-buzei
         AND werks IN s_werks
         AND gsber IN s_gsber
         AND aufnr IN s_aufnr
         AND zuonr IN s_zuonr
         AND xref3 IN s_xref3
         AND kostl IN s_kostl .

      MOVE-CORRESPONDING faglflexa TO itab.
      MOVE-CORRESPONDING faglflexa TO bsis.
      MOVE-CORRESPONDING bsis TO itab.
      MOVE-CORRESPONDING lss_bseg TO itab.

      itab-valor = itab-valut - itab-bldat.
      itab-saknr = itab-hkont. " YT20061218.
      SELECT SINGLE ktext INTO itab-aufnr_text FROM coas WHERE
                              aufnr = bsis-aufnr.


      itab-xref1 = lss_bseg-xref1          .
      itab-xref2 = lss_bseg-xref2          .
      itab-zuonr = lss_bseg-zuonr         .
      IF itab-menge > 0.
        itab-bfiyat = itab-dmbtr / itab-menge.
      ENDIF.


      IF p_chk NE 'X' AND itab-valut NE '00000000'.
        itab-gecik = sy-datum   - itab-valut.
      ENDIF.

      PERFORM find_bwwrt USING bsis CHANGING itab-bwwrt.

      SELECT SINGLE * FROM bkpf INTO CORRESPONDING FIELDS OF itab
           WHERE bukrs = p_bukrs    AND
                 belnr = itab-belnr AND
                 gjahr = itab-gjahr AND
                 usnam IN s_usnam AND
                 blart IN s_blart AND
                 xref1_hd IN s_xref1h AND
                 xref2_hd IN s_xref2h.
      IF sy-subrc EQ 0.
        PERFORM fill_appended_fields USING itab.
        IF itab-menge > 0.
          itab-bfiyat = itab-dmbtr / itab-menge.
        ENDIF.
        itab-dmbtr = faglflexa-hsl.
        itab-wrbtr = faglflexa-wsl.
        itab-gsber  = faglflexa-rbusa.
        APPEND itab.
        CLEAR itab.
      ENDIF.
    ENDSELECT.

  ENDIF.

ENDFORM.                    " get_data_gl



*&---------------------------------------------------------------------*
*&      Form  get_data_gl_closed
*&---------------------------------------------------------------------*
FORM get_data_gl_closed.
  CHECK p_cls = 'X'.
  SELECT * FROM bsas
           WHERE bukrs = p_bukrs  AND
                 hkont IN p_hkont AND
                 zuonr IN s_zuonr AND
                 budat IN s_bldat AND
                 blart IN s_blart AND
*                 prctr IN s_prctr AND
*                 prctr IN s_prctr AND
                 gsber IN s_gsber AND
                 aufnr IN s_aufnr AND
                 xref3 IN s_xref3 AND
                 kostl IN s_kostl .

    MOVE-CORRESPONDING bsas TO itab.
    itab-valor = itab-valut - itab-bldat.
    itab-saknr = itab-hkont ."YT20061218.
    IF p_chk NE 'X' AND itab-valut NE '00000000'.
      itab-gecik = sy-datum   - itab-valut.
    ENDIF.

    SELECT SINGLE ktext INTO itab-aufnr_text FROM coas WHERE
                            aufnr = bsas-aufnr.

    PERFORM find_bwwrt USING bsas CHANGING itab-bwwrt.

    SELECT SINGLE * FROM bkpf INTO CORRESPONDING FIELDS OF itab
       WHERE bukrs = p_bukrs AND
             belnr = itab-belnr AND
             gjahr = itab-gjahr AND
*             usnam IN s_usnam AND
            xref1_hd IN s_xref1h AND
               xref2_hd IN s_xref2h.
    IF sy-subrc EQ 0.

      PERFORM fill_appended_fields USING itab.
****      IF itab-menge > 0.
****        itab-bfiyat = itab-dmbtr / itab-menge.
****      ENDIF.
* defteri kebir görünümü kar merkezi
****      CLEAR faglflexa .
****      SELECT * FROM faglflexa WHERE ryear = itab-gjahr AND
****                                    docnr = itab-belnr AND
****                                   rbukrs = itab-bukrs AND
****                                   buzei  = itab-buzei ."AND
*****                                   rldnr  = p_rldnr .
****
****        itab-dmbtr = faglflexa-hsl.
*****        itab-wrbtr = faglflexa-tsl.
****        itab-wrbtr = faglflexa-wsl.
*****        itab-prctr = faglflexa-prctr.
****        itab-gsber  = faglflexa-rbusa.
        APPEND itab.
*        CLEAR itab.
****      ENDSELECT.
    ENDIF.
  ENDSELECT.

ENDFORM.                    " get_data_gl_closed

*&---------------------------------------------------------------------*
*&      Form  header
*&---------------------------------------------------------------------*
FORM header_dvr.
  DATA: lv_tabix LIKE sy-tabix.
  CLEAR : cumule , s_top, h_top.

  CLEAR: devir   ,
         lv_tabix.
  IF p_gl = 'X'.
    READ TABLE devir WITH KEY hkont = itab-saknr.
*                              waers = itab-waers.
  ELSE.
    READ TABLE devir WITH KEY mu_sa = itab-hkont
                              hkont = itab-saknr.
*                              waers = itab-waers.
  ENDIF.
  lv_tabix = sy-tabix.
  IF sy-subrc = 0.
    PERFORM add_devir.
    devir-islendi = 'X'.
    MODIFY devir INDEX lv_tabix.
  ELSE.
    PERFORM add_bos_devir.
  ENDIF.
  MOVE : itab-saknr TO devir-hkont ,
         itab-waers TO devir-waers .
  rtab-devir = devir-dmbtr.



***Get name
  IF p_gl = 'X'.
    CLEAR: lv_ktopl.
    SELECT SINGLE ktopl FROM t001
                        INTO lv_ktopl
                       WHERE bukrs = p_bukrs.
    SELECT SINGLE txt20 INTO name
         FROM skat WHERE spras = sy-langu
                     AND saknr = itab-hkont
                     AND ktopl = lv_ktopl.
  ELSEIF p_mu = 'X'.
    SELECT SINGLE name1 INTO name
         FROM kna1 WHERE kunnr = itab-hkont .
  ELSE.
    SELECT SINGLE name1 INTO name
         FROM lfa1 WHERE lifnr = itab-hkont .
  ENDIF.
  rtab-name = name.
ENDFORM.                    " header
*&---------------------------------------------------------------------*
*&      Form  item_line
*&---------------------------------------------------------------------*
FORM item_line.
  CASE itab-shkzg.
    WHEN 'S'.
      rtab-dmbtr1 = itab-tutar .
      cumule = cumule + itab-tutar.
      s_top = s_top + itab-tutar.
    WHEN 'H'.
      rtab-dmbtr2 = itab-tutar .
      cumule = cumule - itab-tutar." + YI - YAPTIK
      h_top = h_top + itab-tutar.
  ENDCASE.
  IF cumule > 0 .
    rtab-dmbtr3 = cumule.
  ELSEIF cumule < 0 .
    rtab-dmbtr4 = cumule.
  ELSE.
    rtab-dmbtr3 = cumule.
    rtab-dmbtr4 = cumule.
  ENDIF.
*-
  IF itab-shkzg EQ 'H' .
    rtab-dmbtr5 = itab-tutar  * ( -1 ).
  ELSE.
    rtab-dmbtr5 = itab-tutar  .
  ENDIF.
  MOVE cumule TO  rtab-dmbtr6  .
ENDFORM.                    " item_line
*&---------------------------------------------------------------------*
*&      Form  disable_fields
*&---------------------------------------------------------------------*
FORM disable_fields .
  LOOP AT SCREEN.
*- open close block
    CASE screen-group1.
      WHEN 'FB' .
        IF p_gl   NE space.
          screen-input  = 0 .
          screen-active = 0 .
        ENDIF.
      WHEN 'FB2' .
        IF p_gl   NE space.
          screen-input  = 0 .
          screen-active = 0 .
        ENDIF.
      WHEN 'FB3' .
        IF p_mu   NE space.
          screen-input  = 0 .
          screen-active = 0 .
        ENDIF.
      WHEN 'FGL' .
        IF p_gl   EQ space.
          screen-input  = 0 .
          screen-active = 0 .
        ENDIF.
      WHEN 'FMU' .
        IF p_mu   EQ space.
          screen-input  = 0 .
          screen-active = 0 .
        ENDIF.
      WHEN 'FSA' .
        IF p_sa   EQ space.
          screen-input  = 0 .
          screen-active = 0 .
        ENDIF.

    ENDCASE.
    IF gv_flg10 EQ space.
      IF screen-group1 EQ 'FB1' OR
         screen-group1 EQ 'FB2' OR
         screen-group1 EQ 'FB3' .
        screen-input  = 0 .
        screen-active = 0 .
      ENDIF.
    ENDIF.

    IF screen-name EQ 'P_TOP'.
      screen-invisible = '1'.
    ENDIF.

    IF screen-name CS 'P_BUKRS'.
      IF gv_sistem_yon EQ 'X'.
        screen-active = 1.
        screen-input  = 1.
      ELSE.
        screen-active = 1.
        screen-input  = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

  IF gv_sistem_yon EQ 'X'.
    LOOP AT SCREEN.
      IF ( screen-name CS 'S_MSRKET').
        screen-active = 1.
        screen-input  = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF ( screen-name CS 'S_MSRKET').
        screen-active = 1.
        screen-input  = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " disable_fields
*&---------------------------------------------------------------------*
*&      Form  get_initial_mu
*&---------------------------------------------------------------------*
FORM get_initial_mu.
  DATA: lv_belnr LIKE bsid-belnr,
        lv_gjahr LIKE bsid-gjahr.
  DATA : lv_count TYPE i.

  IF p_dvr = 'X'.
    SELECT p~kunnr p~hkont p~umskz p~shkzg p~dmbtr p~waers p~wrbtr
           p~belnr p~gjahr
       INTO (devir-mu_sa, devir-hkont, bsid-umskz, bsid-shkzg,
            devir-dmbtr , devir-waers, bsid-wrbtr,
            lv_belnr, lv_gjahr )
         FROM bsid AS p
         INNER JOIN bkpf AS c ON    p~bukrs   = c~bukrs
                                AND p~gjahr   = c~gjahr
                                AND p~belnr   = c~belnr
         INNER JOIN kna1 AS d ON p~kunnr = d~kunnr
           WHERE p~bukrs =  p_bukrs     AND
                 p~kunnr IN p_kunnr     AND
*                 D~STCD2 IN S_STCD2     AND
*                 P~ZUONR IN S_ZUONR     AND
                 p~budat LT s_bldat-low     AND
                 p~blart IN s_blart     AND
**               p~prctr IN s_prctr AND
**               p~prctr IN s_prctr AND
                 p~gsber   IN s_gsber   "AND
*                 c~rldnr = p_rldnr  "AND
*                 P~KOSTL IN S_KOSTL     AND
*                 P~AUFNR IN S_AUFNR     AND
*                 P~XREF1 IN S_XREF1     AND
*                 P~XREF2 IN S_XREF2     AND
*                 P~XREF3 IN S_XREF3     AND
*                 C~USNAM IN S_USNAM     AND
*                 C~BSTAT NE 'S'         AND
*                 D~NAME3 IN S_NAME3     AND
*                 D~KTOKD IN S_KTOKD
      .

*    DEVIR-HKONT = BSID-KUNNR .
*    DEVIR-DMBTR = BSID-DMBTR .
      IF x_shbv NE 'X' AND     "General Ledger Kontrol
         ( bsid-umskz NE space ) .
        CONTINUE.
      ENDIF.

      IF p_bpb = 'X'.
        devir-dmbtr  = bsid-wrbtr .
      ELSE.
        devir-waers  = t001-waers.
      ENDIF.
      IF bsid-shkzg = 'H'.
        devir-dmbtr = devir-dmbtr * -1.
** alacak borc durumu
        devir-borc  = devir-dmbtr .
      ELSE.
        devir-alck  = devir-dmbtr .

      ENDIF.
      COLLECT devir.CLEAR devir.
      ADD : 1 TO lv_count.
    ENDSELECT.

*******

    SELECT p~kunnr p~hkont p~umskz p~shkzg p~dmbtr p~waers p~wrbtr
           p~belnr p~gjahr
       INTO (devir-mu_sa, devir-hkont, bsad-umskz, bsad-shkzg,
            devir-dmbtr , devir-waers, bsad-wrbtr,
            lv_belnr, lv_gjahr)
         FROM bsad AS p
         INNER JOIN bkpf AS c ON    p~bukrs   = c~bukrs
                                AND p~gjahr   = c~gjahr
                                AND p~belnr   = c~belnr
         INNER JOIN kna1 AS d ON p~kunnr = d~kunnr
           WHERE p~bukrs =  p_bukrs     AND
                 p~kunnr IN p_kunnr     AND
*                 D~STCD2 IN S_STCD2     AND
*                 P~ZUONR IN S_ZUONR     AND
                 p~budat LT s_bldat-low AND
                 p~blart IN s_blart     AND
*               p~prctr IN s_prctr AND
*               p~prctr IN s_prctr AND
                 p~gsber   IN s_gsber  " AND
*                 P~KOSTL IN S_KOSTL     AND
*                 P~AUFNR IN S_AUFNR     AND
*                 P~XREF1 IN S_XREF1     AND
*                 P~XREF2 IN S_XREF2     AND
*                 P~XREF3 IN S_XREF3     AND
*                 C~USNAM IN S_USNAM     AND
*                 C~BSTAT NE 'S'         AND
*                 D~NAME3 IN S_NAME3     AND
*                 D~KTOKD IN S_KTOKD
      .

*    DEVIR-HKONT = BSID-KUNNR .
*    DEVIR-DMBTR = BSID-DMBTR .
      IF x_shbv NE 'X' AND     "General Ledger Kontrol
         ( bsad-umskz NE space ) .
        CONTINUE.
      ENDIF.

      IF p_bpb = 'X'.
        devir-dmbtr  = bsad-wrbtr .
      ELSE.
        devir-waers  = t001-waers.
      ENDIF.
      IF bsad-shkzg = 'H'.
        devir-dmbtr = devir-dmbtr * -1.
** alacak borc durumu
        devir-borc  = devir-dmbtr .
      ELSE.
        devir-alck  = devir-dmbtr .
      ENDIF.
      COLLECT devir.CLEAR devir.
      ADD : 1 TO lv_count.
    ENDSELECT.
*******

  ELSE.

    SELECT p~kunnr p~umskz p~shkzg p~dmbtr p~waers p~wrbtr
       INTO (devir-hkont,bsid-umskz,bsid-shkzg,devir-dmbtr,devir-waers,
             bsid-wrbtr)
         FROM bsid AS p
         INNER JOIN bkpf AS c ON    p~bukrs   = c~bukrs
                                AND p~gjahr   = c~gjahr
                                AND p~belnr   = c~belnr
         INNER JOIN kna1 AS d ON p~kunnr = d~kunnr
           WHERE p~bukrs =  p_bukrs     AND
                 p~kunnr IN p_kunnr     AND
*                 D~STCD2 IN S_STCD2     AND
*                 P~ZUONR IN S_ZUONR     AND
                 p~budat IN s_bldat AND
                 p~blart IN s_blart     AND
*               p~prctr IN s_prctr AND
*               p~prctr IN s_prctr AND
                 gsber   IN s_gsber    " AND
*                 P~KOSTL IN S_KOSTL     AND
*                 P~AUFNR IN S_AUFNR     AND
*                 P~XREF1 IN S_XREF1     AND
*                 P~XREF2 IN S_XREF2     AND
*                 P~XREF3 IN S_XREF3     AND
*                 C~USNAM IN S_USNAM     AND
*                 D~NAME3 IN S_NAME3     AND
*                 D~KTOKD IN S_KTOKD
      .
*    DEVIR-HKONT = BSID-KUNNR .
*    DEVIR-DMBTR = BSID-DMBTR .
      IF x_shbv NE 'X' AND     "General Ledger Kontrol
         ( bsid-umskz NE space ) .
        CONTINUE.
      ENDIF.

***      Ters Kayıt Kontrolü eklentileri
      IF p_trs IS INITIAL.
        CONTINUE.
      ENDIF.
      IF p_bpb = 'X'.
        devir-dmbtr  = bsid-wrbtr .
      ELSE.
        devir-waers  = t001-waers.
      ENDIF.
      IF bsid-shkzg = 'H'.
        devir-dmbtr = devir-dmbtr * -1.
** alacak borc durumu
        devir-borc  = devir-dmbtr .
      ELSE.
        devir-alck  = devir-dmbtr .

      ENDIF.
      COLLECT devir.CLEAR devir.
      ADD : 1 TO lv_count.
    ENDSELECT.


*********
    SELECT p~kunnr p~umskz p~shkzg p~dmbtr p~waers p~wrbtr
       INTO (devir-hkont,bsad-umskz,bsad-shkzg,devir-dmbtr,devir-waers,
             bsad-wrbtr)
         FROM bsad AS p
         INNER JOIN bkpf AS c ON    p~bukrs   = c~bukrs
                                AND p~gjahr   = c~gjahr
                                AND p~belnr   = c~belnr
         INNER JOIN kna1 AS d ON p~kunnr = d~kunnr
           WHERE p~bukrs =  p_bukrs     AND
                 p~kunnr IN p_kunnr     AND
*                 D~STCD2 IN S_STCD2     AND
*                 P~ZUONR IN S_ZUONR     AND
                 p~budat IN s_bldat AND
                 p~blart IN s_blart     AND
*               p~prctr IN s_prctr AND
*               p~prctr IN s_prctr AND
                 gsber   IN s_gsber   "  AND
*                 P~KOSTL IN S_KOSTL     AND
*                 P~AUFNR IN S_AUFNR     AND
*                 P~XREF1 IN S_XREF1     AND
*                 P~XREF2 IN S_XREF2     AND
*                 P~XREF3 IN S_XREF3     AND
*                 C~USNAM IN S_USNAM     AND
*                 D~NAME3 IN S_NAME3     AND
*                 D~KTOKD IN S_KTOKD
      .
*    DEVIR-HKONT = BSID-KUNNR .
*    DEVIR-DMBTR = BSID-DMBTR .
      IF x_shbv NE 'X' AND     "General Ledger Kontrol
         ( bsad-umskz NE space ) .
        CONTINUE.
      ENDIF.

      IF p_bpb = 'X'.
        devir-dmbtr  = bsad-wrbtr .
      ELSE.
        devir-waers  = t001-waers.
      ENDIF.
      IF bsad-shkzg = 'H'.
        devir-dmbtr = devir-dmbtr * -1.
** alacak borc durumu
        devir-borc  = devir-dmbtr .
      ELSE.
        devir-alck  = devir-dmbtr .

      ENDIF.
      COLLECT devir.CLEAR devir.
      ADD : 1 TO lv_count.
    ENDSELECT.
*********
  ENDIF.

ENDFORM.                    " get_initial_mu

*&---------------------------------------------------------------------*
*&      Form  get_data_mu
*&---------------------------------------------------------------------*
FORM get_data_mu.
  SELECT * FROM bsid
           WHERE bukrs = p_bukrs  AND
                 kunnr IN p_kunnr AND
*                 ZUONR IN S_ZUONR AND
                 budat IN s_bldat AND
                 blart IN s_blart AND
*                 prctr IN s_prctr AND
*                 prctr IN s_prctr AND
                 gsber IN s_gsber" AND
*                 KOSTL IN S_KOSTL AND
*                 AUFNR IN S_AUFNR AND
*                 XREF1 IN S_XREF1 AND
*                 XREF2 IN S_XREF2 AND
*                 XREF3 IN S_XREF3
    .
    IF x_shbv NE 'X' AND     "General Ledger Kontrol
       ( bsid-umskz NE space ) AND
       ( bsid-bstat NE 'S' ) AND
       ( bsid-bstat NE 'V' ) AND
       ( bsid-bstat NE 'W' ) AND
       ( bsid-bstat NE 'Z' ) .
      CONTINUE.
    ENDIF.
** vergi
    SELECT SINGLE name3 INTO itab-name3
      FROM kna1
     WHERE kunnr = bsid-kunnr
       AND stcd2 IN s_stcd2
       AND name3 IN s_name3
       AND ktokd IN s_ktokd.
    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING bsid TO itab.

    itab-saknr = bsid-hkont.
    itab-hkont = bsid-hkont.
    itab-valut = bsid-zfbdt + bsid-zbd1t.
    itab-valor = bsid-zbd1t.
    itab-kunnr = bsid-kunnr.
    SELECT SINGLE ktext INTO itab-aufnr_text FROM coas WHERE
                            aufnr = bsid-aufnr.
    itab-name3 = itab-name3(4).
    IF p_chk NE 'X' AND itab-valut NE '00000000'.
      itab-gecik = sy-datum   - itab-valut.
    ENDIF.
    SELECT SINGLE * FROM bkpf INTO CORRESPONDING FIELDS OF itab
         WHERE bukrs = p_bukrs AND
               belnr = itab-belnr AND
               gjahr = itab-gjahr AND
*               usnam IN s_usnam AND
                     xref1_hd IN s_xref1h AND
               xref2_hd IN s_xref2h.
    IF sy-subrc EQ 0.

      IF logic = '2'.   "Is it couple turn?
        itab-type = 'M'.
        READ TABLE couples WITH KEY kunnr = itab-kunnr.
        itab-hkont = couples-lifnr.
      ENDIF.

      PERFORM fill_appended_fields USING itab.

* defteri kebir görünümü kar merkezi
*      CLEAR faglflexa .
*      SELECT * FROM faglflexa WHERE ryear = itab-gjahr AND
*                                    docnr = itab-belnr AND
*                                   rbukrs = itab-bukrs AND
*                                   buzei  = itab-buzei." AND
**                                   rldnr  = p_rldnr,.
*
*        itab-dmbtr = faglflexa-hsl.
**        itab-wrbtr = faglflexa-tsl.
*        itab-wrbtr = faglflexa-wsl.
**        itab-prctr = faglflexa-prctr.
*        itab-gsber  = faglflexa-rbusa.
        APPEND itab.
*      ENDSELECT.
*      CLEAR itab.

    ENDIF.
  ENDSELECT.

ENDFORM.                    " get_data_mu
*&---------------------------------------------------------------------*
*&      Form  get_initial_sa
*&---------------------------------------------------------------------*
FORM get_initial_sa.

  IF p_dvr = 'X'.

    SELECT p~lifnr p~hkont p~umskz p~shkzg p~dmbtr p~waers p~wrbtr
       INTO (devir-mu_sa, devir-hkont, bsik-umskz, bsik-shkzg,
             devir-dmbtr, devir-waers, bsik-wrbtr)
         FROM bsik AS p
         INNER JOIN bkpf AS c ON    p~bukrs   = c~bukrs
                                AND p~gjahr   = c~gjahr
                                AND p~belnr   = c~belnr
         INNER JOIN lfa1 AS d ON p~lifnr = d~lifnr
           WHERE p~bukrs =  p_bukrs     AND
                 p~lifnr IN p_lifnr     AND
                 d~stcd2 IN s_stcd2     AND
                 p~zuonr IN s_zuonr     AND
                 p~budat LT s_bldat-low AND
                 p~blart IN s_blart     AND
*               p~prctr IN s_prctr AND
*               p~prctr IN s_prctr AND
                 p~gsber IN s_gsber     AND
                 p~kostl IN s_kostl     AND
                 p~aufnr IN s_aufnr     AND
                 p~xref1 IN s_xref1     AND
                 p~xref2 IN s_xref2     AND
                 p~xref3 IN s_xref3     AND
                 c~usnam IN s_usnam     AND
                 c~bstat NE 'S'         AND
*                 c~rldnr = p_rldnr      AND
                 c~xref1_hd IN s_xref1h AND
                 c~xref2_hd IN s_xref2h AND
                  d~ktokk IN s_ktokd.
      IF x_shbv NE 'X' AND
         ( bsik-umskz NE space ).
        CONTINUE.
      ENDIF.

      IF p_bpb = 'X'.
        devir-dmbtr  = bsik-wrbtr .
      ELSE.
        devir-waers  = t001-waers.
      ENDIF.
      IF bsik-shkzg = 'H'.
        devir-dmbtr = devir-dmbtr * -1.
** alacak borc durumu
        devir-borc  = devir-dmbtr .
      ELSE.
        devir-alck  = devir-dmbtr .

      ENDIF.
      COLLECT devir.CLEAR devir.
    ENDSELECT.
*******
    SELECT p~lifnr p~hkont p~umskz p~shkzg p~dmbtr p~waers p~wrbtr
       INTO (devir-mu_sa, devir-hkont, bsak-umskz, bsak-shkzg,
             devir-dmbtr, devir-waers, bsak-wrbtr)
         FROM bsak AS p
         INNER JOIN bkpf AS c ON    p~bukrs   = c~bukrs
                                AND p~gjahr   = c~gjahr
                                AND p~belnr   = c~belnr
         INNER JOIN lfa1 AS d ON p~lifnr = d~lifnr
           WHERE p~bukrs =  p_bukrs     AND
                 p~lifnr IN p_lifnr     AND
                 d~stcd2 IN s_stcd2     AND
                 p~zuonr IN s_zuonr     AND
                 p~budat LT s_bldat-low AND
                 p~blart IN s_blart     AND
*               p~prctr IN s_prctr AND
*               p~prctr IN s_prctr AND
                 p~gsber IN s_gsber     AND
                 p~kostl IN s_kostl     AND
                 p~aufnr IN s_aufnr     AND
                 p~xref1 IN s_xref1     AND
                 p~xref2 IN s_xref2     AND
                 p~xref3 IN s_xref3     AND
                 c~usnam IN s_usnam     AND
                 c~bstat NE 'S'         AND
                     c~xref1_hd IN s_xref1h AND
                 c~xref2_hd IN s_xref2h AND
                  d~ktokk IN s_ktokd.
      IF x_shbv NE 'X' AND
         ( bsak-umskz NE space ).
        CONTINUE.
      ENDIF.

      IF p_bpb = 'X'.
        devir-dmbtr  = bsak-wrbtr .
      ELSE.
        devir-waers  = t001-waers.
      ENDIF.
      IF bsak-shkzg = 'H'.
        devir-dmbtr = devir-dmbtr * -1.
** alacak borc durumu
        devir-borc  = devir-dmbtr .
      ELSE.
        devir-alck  = devir-dmbtr .
      ENDIF.
      COLLECT devir.CLEAR devir.
    ENDSELECT.

*******
  ELSE.

    SELECT p~lifnr p~umskz p~shkzg p~dmbtr p~waers p~wrbtr
       INTO (devir-hkont,bsik-umskz,bsik-shkzg,devir-dmbtr,devir-waers,
  bsik-wrbtr)
         FROM bsik AS p
         INNER JOIN bkpf AS c ON    p~bukrs   = c~bukrs
                                AND p~gjahr   = c~gjahr
                                AND p~belnr   = c~belnr
         INNER JOIN lfa1 AS d ON p~lifnr = d~lifnr
           WHERE p~bukrs =  p_bukrs     AND
                 p~lifnr IN p_lifnr     AND
                 d~stcd2 IN s_stcd2     AND
                 p~zuonr IN s_zuonr     AND
                 p~budat LT s_bldat-low AND
                 p~blart IN s_blart     AND
*               p~prctr IN s_prctr AND
*               p~prctr IN s_prctr AND
                 p~gsber IN s_gsber     AND
                 p~kostl IN s_kostl     AND
                 p~aufnr IN s_aufnr     AND
                 p~xref1 IN s_xref1     AND
                 p~xref2 IN s_xref2     AND
                 p~xref3 IN s_xref3     AND
                 c~usnam IN s_usnam     AND
                                                            "YT27072006
                     c~xref1_hd IN s_xref1h AND
                 c~xref2_hd IN s_xref2h.
      IF x_shbv NE 'X' AND
         ( bsik-umskz NE space ).
        CONTINUE.
      ENDIF.

      IF p_bpb = 'X'.
        devir-dmbtr  = bsik-wrbtr .
      ELSE.
        devir-waers  = t001-waers.
      ENDIF.
      IF bsik-shkzg = 'H'.
        devir-dmbtr = devir-dmbtr * -1.
** alacak borc durumu
        devir-alck  = devir-dmbtr .
      ELSE.
        devir-borc  = devir-dmbtr .

      ENDIF.
      COLLECT devir.CLEAR devir.
    ENDSELECT.
*********

    SELECT p~lifnr p~umskz p~shkzg p~dmbtr p~waers p~wrbtr
       INTO (devir-hkont,bsik-umskz,bsak-shkzg,devir-dmbtr,devir-waers,
  bsak-wrbtr)
         FROM bsak AS p
         INNER JOIN bkpf AS c ON    p~bukrs   = c~bukrs
                                AND p~gjahr   = c~gjahr
                                AND p~belnr   = c~belnr
         INNER JOIN lfa1 AS d ON p~lifnr = d~lifnr
           WHERE p~bukrs =  p_bukrs     AND
                 p~lifnr IN p_lifnr     AND
                 d~stcd2 IN s_stcd2     AND
                 p~zuonr IN s_zuonr     AND
                 p~budat LT s_bldat-low AND
                 p~blart IN s_blart     AND
*               p~prctr IN s_prctr AND
*               p~prctr IN s_prctr AND
                 p~gsber IN s_gsber     AND
                 p~kostl IN s_kostl     AND
                 p~aufnr IN s_aufnr     AND
                 p~xref1 IN s_xref1     AND
                 p~xref2 IN s_xref2     AND
                 p~xref3 IN s_xref3     AND
                 c~usnam IN s_usnam     AND
                                                            "YT27072006
                     c~xref1_hd IN s_xref1h AND
                 c~xref2_hd IN s_xref2h.
      IF x_shbv NE 'X' AND
         ( bsak-umskz NE space ).
        CONTINUE.
      ENDIF.

      IF p_bpb = 'X'.
        devir-dmbtr  = bsak-wrbtr .
      ELSE.
        devir-waers  = t001-waers.
      ENDIF.
      IF bsak-shkzg = 'H'.
        devir-dmbtr = devir-dmbtr * -1.
** alacak borc durumu
        devir-alck  = devir-dmbtr .
      ELSE.
        devir-borc  = devir-dmbtr .
      ENDIF.
      COLLECT devir.CLEAR devir.
    ENDSELECT.
*********
  ENDIF.
ENDFORM.                    " get_initial_sa
*&---------------------------------------------------------------------*
*&      Form  get_data_sa
*&---------------------------------------------------------------------*
FORM get_data_sa.
  SELECT * FROM bsik
           WHERE bukrs = p_bukrs  AND
                 lifnr IN p_lifnr AND
                 zuonr IN s_zuonr AND
                 budat IN s_bldat AND
                 blart IN s_blart AND
*                 prctr IN s_prctr AND
                 gsber IN s_gsber AND
                 kostl IN s_kostl AND
                 aufnr IN s_aufnr AND
                 xref1 IN s_xref1 AND
                 xref2 IN s_xref2 AND
                 xref3 IN s_xref3 .
    IF x_shbv NE 'X' AND
       ( bsik-umskz NE space ) AND
       ( bsik-bstat NE 'S' ) AND
       ( bsik-bstat NE 'V' ) AND
       ( bsik-bstat NE 'W' ) AND
       ( bsik-bstat NE 'Z' ) .
      CONTINUE.
    ENDIF.
** vergi
    SELECT SINGLE *
      FROM lfa1
     WHERE lifnr = bsik-lifnr
       AND stcd2 IN s_stcd2
       AND ktokk IN s_ktokk.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING bsik TO itab.

    itab-saknr = bsik-hkont.
    itab-hkont = itab-lifnr.
    itab-valut = bsik-zfbdt + bsik-zbd1t.
    itab-valor = bsik-zbd1t.
    itab-lifnr = bsik-lifnr.
    SELECT SINGLE ktext INTO itab-aufnr_text FROM coas WHERE
                            aufnr = bsik-aufnr.
    IF p_chk NE 'X' AND itab-valut NE '00000000'.
      itab-gecik = sy-datum   - itab-valut.
    ENDIF.
    SELECT SINGLE * FROM bkpf INTO CORRESPONDING FIELDS OF itab
         WHERE bukrs = p_bukrs AND
               belnr = itab-belnr AND
               gjahr = itab-gjahr AND
*               usnam IN s_usnam AND
                     xref1_hd IN s_xref1h AND
               xref2_hd IN s_xref2h.
    CHECK sy-subrc EQ 0.

    IF NOT s_werks IS INITIAL.
      SELECT SINGLE belnr INTO itab-belnr FROM bseg
              WHERE belnr =  itab-belnr AND
                    werks IN s_werks    AND
                    werks NE space        .
      CHECK sy-subrc = 0 .
    ENDIF.
    IF logic = '1'.   "Is it couple turn?
      itab-type = 'S'.
      READ TABLE couples WITH KEY lifnr = itab-lifnr.
      itab-hkont = couples-kunnr.
    ENDIF.

    PERFORM fill_appended_fields USING itab.

* defteri kebir görünümü kar merkezi
    CLEAR faglflexa .
*    SELECT * FROM faglflexa WHERE ryear = itab-gjahr AND
*                                  docnr = itab-belnr AND
*                                 rbukrs = itab-bukrs AND
*                                 buzei  = itab-buzei ."AND
**                                   rldnr  = p_rldnr.
*
*      itab-dmbtr = faglflexa-hsl.
**      itab-wrbtr = faglflexa-tsl.
*      itab-wrbtr = faglflexa-wsl.
**      itab-prctr = faglflexa-prctr.
*      itab-gsber  = faglflexa-rbusa.
      APPEND itab.
*    ENDSELECT.
*    CLEAR itab.

  ENDSELECT.

ENDFORM.                    " get_data_sa
*&---------------------------------------------------------------------*
*&      Form  get_data_mu_closed
*&---------------------------------------------------------------------*
FORM get_data_mu_closed.
  CHECK p_cls = 'X'.
  SELECT * FROM bsad
           WHERE bukrs = p_bukrs  AND
                 kunnr IN p_kunnr AND
                 zuonr IN s_zuonr AND
                 budat IN s_bldat AND
                 blart IN s_blart AND
*                 prctr IN s_prctr AND
*                 prctr IN s_prctr AND
                 gsber IN s_gsber AND
                 kostl IN s_kostl AND
                 aufnr IN s_aufnr AND
                 xref1 IN s_xref1 AND
                 xref2 IN s_xref2 AND
                 xref3 IN s_xref3 .

    IF x_shbv NE 'X' AND     "General Ledger Kontrol
       ( bsad-umskz NE space ) AND
       ( bsad-bstat NE 'S' ) AND
       ( bsad-bstat NE 'V' ) AND
       ( bsad-bstat NE 'W' ) AND
       ( bsad-bstat NE 'Z' ) .
      CONTINUE.
    ENDIF.
** vergi
    SELECT SINGLE name3 INTO itab-name3
      FROM kna1
     WHERE kunnr = bsad-kunnr
       AND stcd2 IN s_stcd2
       AND name3 IN s_name3
       AND ktokd IN s_ktokd.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING bsad TO itab.

    itab-saknr = bsad-hkont.
    itab-hkont = itab-kunnr.
    itab-valut = bsad-zfbdt + bsad-zbd1t.
    itab-valor = bsad-zbd1t.
    itab-kunnr = bsad-kunnr.
    SELECT SINGLE ktext INTO itab-aufnr_text FROM coas WHERE
                            aufnr = bsad-aufnr.
    itab-name3 = itab-name3(4).
    IF p_chk NE 'X' AND itab-valut NE '00000000'.
      itab-gecik = sy-datum   - itab-valut.
    ENDIF.
    SELECT SINGLE * FROM bkpf INTO CORRESPONDING FIELDS OF itab
         WHERE bukrs = p_bukrs AND
               belnr = itab-belnr AND
               gjahr = itab-gjahr AND
*               usnam IN s_usnam AND
                     xref1_hd IN s_xref1h AND
               xref2_hd IN s_xref2h.
    IF sy-subrc EQ 0.

      IF logic = '2'.   "Is it couple turn?
        itab-type = 'M'.
        READ TABLE couples WITH KEY kunnr = itab-kunnr.
        itab-hkont = couples-lifnr.
      ENDIF.

      PERFORM fill_appended_fields USING itab.

* defteri kebir görünümü kar merkezi
*      CLEAR faglflexa .
*      SELECT * FROM faglflexa WHERE ryear = itab-gjahr AND
*                                    docnr = itab-belnr AND
*                                   rbukrs = itab-bukrs AND
*                                   buzei  = itab-buzei ."AND
**                                   rldnr  = p_rldnr.
*
*        itab-dmbtr = faglflexa-hsl.
**        itab-wrbtr = faglflexa-tsl.
*        itab-wrbtr = faglflexa-wsl.
**        itab-prctr = faglflexa-prctr.
*        itab-gsber  = faglflexa-rbusa.
        APPEND itab.
*      ENDSELECT.
*      CLEAR itab.
    ENDIF.

  ENDSELECT.

ENDFORM.                    " get_data_mu_closed
*&---------------------------------------------------------------------*
*&      Form  get_data_sa_closed
*&---------------------------------------------------------------------*
FORM get_data_sa_closed.
  CHECK p_cls = 'X'.
  SELECT * FROM bsak
           WHERE bukrs = p_bukrs  AND
                 lifnr IN p_lifnr AND
                 zuonr IN s_zuonr AND
                 budat IN s_bldat AND
                 blart IN s_blart AND
*                 prctr IN s_prctr AND
                 gsber IN s_gsber AND
                 kostl IN s_kostl AND
                 aufnr IN s_aufnr AND
                 xref1 IN s_xref1 AND
                 xref2 IN s_xref2 AND
                 xref3 IN s_xref3 .


    IF x_shbv NE 'X' AND
       ( bsak-umskz NE space ) AND
       ( bsak-bstat NE 'S' ) AND
       ( bsak-bstat NE 'V' ) AND
       ( bsak-bstat NE 'W' ) AND
       ( bsak-bstat NE 'Z' ) .
      CONTINUE.
    ENDIF.
** vergi
    SELECT SINGLE *
      FROM lfa1
     WHERE lifnr = bsak-lifnr
       AND stcd2 IN s_stcd2
        AND ktokk IN s_ktokk.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING bsak TO itab.

    itab-saknr = bsak-hkont.
    itab-hkont = itab-lifnr.
    itab-valut = bsak-zfbdt + bsak-zbd1t.
    itab-valor = bsak-zbd1t.
    itab-lifnr = bsak-lifnr.
    SELECT SINGLE ktext INTO itab-aufnr_text FROM coas WHERE
                            aufnr = bsak-aufnr.
    IF p_chk NE 'X' AND itab-valut NE '00000000'.
      itab-gecik = sy-datum   - itab-valut.
    ENDIF.
    SELECT SINGLE * FROM bkpf INTO CORRESPONDING FIELDS OF itab
         WHERE bukrs = p_bukrs AND
               belnr = itab-belnr AND
               gjahr = itab-gjahr AND
*               usnam IN s_usnam AND
                     xref1_hd IN s_xref1h AND
               xref2_hd IN s_xref2h.
    CHECK sy-subrc EQ 0.

    IF NOT s_werks IS INITIAL.
      SELECT SINGLE belnr INTO itab-belnr FROM bseg
              WHERE belnr =  itab-belnr AND
                    werks IN s_werks    AND
                    werks NE space        .
      CHECK sy-subrc = 0 .
    ENDIF.
    IF logic = '1'.   "Is it couple turn?
      itab-type = 'S'.
      READ TABLE couples WITH KEY lifnr = itab-lifnr.
      itab-hkont = couples-kunnr.
    ENDIF.

    PERFORM fill_appended_fields USING itab.

* defteri kebir görünümü kar merkezi
*    CLEAR faglflexa .
*    SELECT * FROM faglflexa WHERE ryear = itab-gjahr AND
*                                  docnr = itab-belnr AND
*                                 rbukrs = itab-bukrs AND
*                                 buzei  = itab-buzei ."AND
**                                   rldnr  = p_rldnr.
*
*      itab-dmbtr = faglflexa-hsl.
**      itab-wrbtr = faglflexa-tsl.
*      itab-wrbtr = faglflexa-wsl.
**      itab-prctr = faglflexa-prctr.
*      itab-gsber  = faglflexa-rbusa.
      APPEND itab.
*    ENDSELECT.
*    CLEAR itab.

  ENDSELECT.

ENDFORM.                    " get_data_sa_closed
*&---------------------------------------------------------------------*
*&      Form  get_data_mu_satici
*&---------------------------------------------------------------------*
FORM get_data_mu_satici.
  CHECK x_apar = 'X'.            "Read Couples selected?
  CHECK NOT p_lifnr IS INITIAL.  "Any Couples Existing?
  logic = '1'.
  PERFORM get_data_sa.
  PERFORM get_data_sa_closed.
ENDFORM.                    " get_data_mu_satici
*&---------------------------------------------------------------------*
*&      Form  get_data_sa_musteri
*&---------------------------------------------------------------------*
FORM get_data_sa_musteri.
  CHECK x_apar = 'X'.
  CHECK NOT p_kunnr IS INITIAL.  "Any Couples Existing?
  logic = '2'.
  PERFORM get_data_mu.
  PERFORM get_data_mu_closed.
ENDFORM.                    " get_data_sa_musteri
*&---------------------------------------------------------------------*
*&      Form  find_couples
*&---------------------------------------------------------------------*
FORM find_couples.
  IF p_mu = 'X'.
    p_lifnr-sign   = 'I'.
    p_lifnr-option = 'EQ'.
    SELECT kunnr lifnr  INTO  (kna1-kunnr,kna1-lifnr) FROM kna1 WHERE
           kunnr IN p_kunnr  AND
           lifnr NE space      .

      MOVE-CORRESPONDING kna1 TO couples.
      p_lifnr-low = kna1-lifnr .
      APPEND : p_lifnr,couples.

    ENDSELECT.
    IF sy-subrc NE 0 .
      CLEAR p_lifnr.
    ENDIF.
  ELSEIF p_sa = 'X'.
    p_kunnr-sign   = 'I'.
    p_kunnr-option = 'EQ'.
    SELECT lifnr kunnr INTO (lfa1-lifnr,lfa1-kunnr) FROM lfa1 WHERE
           lifnr IN p_lifnr  AND
           kunnr NE space      .
      MOVE-CORRESPONDING lfa1 TO couples.
      p_kunnr-low = lfa1-kunnr .
      APPEND : p_kunnr,couples.
    ENDSELECT.
    IF sy-subrc NE 0 .
      CLEAR p_kunnr.
    ENDIF.
  ENDIF.
ENDFORM.                    " find_couples
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                  rs_selfield TYPE slis_selfield.
  break kadrig.
  CASE r_ucomm.
    WHEN '&IC1'.                             "dobbleclick
      READ TABLE rtab INTO rtab INDEX rs_selfield-tabindex.
      SET PARAMETER ID 'BLN' FIELD rtab-belnr.
      SET PARAMETER ID 'BUK' FIELD p_bukrs.
      SET PARAMETER ID 'GJR' FIELD rtab-gjahr.

      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDCASE.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------
*&      Form  read_yevmiye_no
*- 04.04.2007 tarihinde eklendi.Yahyat
*----------------------------------------------------------------------
FORM read_yevmiye_no.
***  READ TABLE gt_yvm WITH KEY bukrs = itab-bukrs
***                             gjahr = itab-gjahr
***                             belnr = itab-belnr.
  SELECT SINGLE yevno INTO rtab-belnr_alt FROM ZSOG_FIT_YEVMIYE
    WHERE bukrs = itab-bukrs
    AND gjahr = itab-gjahr
    AND belnr = itab-belnr.
***  IF sy-subrc EQ 0.
***    rtab-belnr_alt = gt_yvm-belnr_alt.
***    EXIT.
***  ENDIF.
  CONCATENATE sy-mandt itab-bukrs itab-belnr itab-gjahr INTO bkpf_key .
  CLEAR x .
  CALL FUNCTION 'OPEN_FI_PERFORM_00003320_P'
    EXPORTING
      i_bkpf_key   = bkpf_key
      i_land1      = 'TR'
    IMPORTING
      e_bkpf_addon = x.
  CLEAR gt_yvm.
  MOVE : itab-bukrs       TO gt_yvm-bukrs ,
         itab-gjahr       TO gt_yvm-gjahr ,
         itab-belnr       TO gt_yvm-belnr ,
         x-belnr_alt TO gt_yvm-belnr_alt.
  APPEND gt_yvm.

***  rtab-belnr_alt = x-belnr_alt.

ENDFORM.                    " read_yevmiye_no
*&---------------------------------------------------------------------*
*&      Form  change_flg
*&---------------------------------------------------------------------*
FORM change_flg  CHANGING p_flg p_button p_txt.
  IF p_flg NE space.
    p_flg = space.
    CONCATENATE icon_data_area_expand p_txt
                INTO p_button SEPARATED BY space.
  ELSE.
    CONCATENATE icon_data_area_collapse p_txt
                INTO p_button SEPARATED BY space.
    p_flg = 'X' .
  ENDIF.
*ICON_2 ICON_DATA_AREA_EXPAND          '@K1@'."  Stop
*ICON_2 ICON_DATA_AREA_COLLAPSE        '@K2@'."  Stop

ENDFORM.                    " change_flg
*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV
*&---------------------------------------------------------------------*
FORM show_alv .
  PERFORM initialization_for_alv USING text-h01     .
  PERFORM alv_get_events USING      'ALV_EVENTS[]'  .
  PERFORM alv_fieldcat_merge USING  'ALV_FIELDCAT[]'  'RTAB' .
  PERFORM set_alv_fieldcat_rtab USING 'RTAB'.

  LOOP AT alv_fieldcat WHERE fieldname EQ 'DEVIR'.
    alv_fieldcat-no_out = 'X'.
    MODIFY alv_fieldcat.
  ENDLOOP.
  PERFORM fill_t_sort TABLES  alv_sort
          USING  : 'RTAB' 'HKONT' 'X' ' ' 'X' ,
                   'RTAB' 'WAERS' 'X' ' ' 'X' .

* ALV_LAYOUT-BOX_FIELDNAME     = 'MARK'  .
* alv_layout-info_fieldname    = 'LINE_COLOR' .
  PERFORM call_alv_function USING 'RTAB[]'
                                    'ALV_FIELDCAT[]' 'ALV_LAYOUT' .

ENDFORM.                    " SHOW_ALV
*&------------------------------------------------------------*
*&      Form  call_alv_function
*&------------------------------------------------------------*
FORM call_alv_function USING tabname fcatname layname.
  ASSIGN : (tabname)  TO <vout>   ,
           (fcatname) TO <fcat>   ,
           (layname)  TO <layout> .
  CLEAR : alv_layout-no_subchoice.

  DATA : t_exclude TYPE slis_t_extab ,
         s_exclude TYPE slis_extab   .

  s_exclude-fcode = '&ODN' .  APPEND s_exclude TO t_exclude .
  s_exclude-fcode = '&OUP' .  APPEND s_exclude TO t_exclude .
  s_exclude-fcode = '&SUM' .  APPEND s_exclude TO t_exclude .
  s_exclude-fcode = '&UMC' .  APPEND s_exclude TO t_exclude .
  s_exclude-fcode = '&OUP' .  APPEND s_exclude TO t_exclude .
  s_exclude-fcode = '&OUP' .  APPEND s_exclude TO t_exclude .
  s_exclude-fcode = '&OUP' .  APPEND s_exclude TO t_exclude .

  alv_layout-info_fieldname   = 'LINE_COLOR'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = alv_repid
      i_callback_pf_status_set = 'SET_MENU'
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      i_background_id          = 'ALV_BACKGROUND'
      is_layout                = alv_layout
      it_fieldcat              = <fcat>
**      it_excluding            = t_exclude
      i_grid_title             = gs_grid
*     IT_SORT                  = ALV_SORT[]
      i_save                   = 'A'
      is_variant               = h_variant
      it_events                = alv_events[]
    TABLES
      t_outtab                 = <vout>.

ENDFORM.                    " call_alv_function
*&---------------------------------------------------------------*
*&      Form  set_alv_fieldcat_merge
*&---------------------------------------------------------------*
FORM set_alv_fieldcat_rtab USING  tabname   TYPE slis_tabname.

  DATA : lv_temp(45) .
  MOVE : 'SELTEXT_L/SELTEXT_M/SELTEXT_S/REPTEXT_DDIC' TO lv_temp.
  PERFORM list_set_attribute TABLES alv_fieldcat
  USING : tabname   'DMBTR'      'CFIELDNAME'   '' , " 'HWAER'
          tabname   'DEVIR'      'CFIELDNAME'   '' , " 'WAERS'
          tabname   'DMBTR1'     'CFIELDNAME'   '' , " 'WAERS'
          tabname   'DMBTR2'     'CFIELDNAME'   '' , " 'WAERS'
          tabname   'DMBTR3'     'CFIELDNAME'   '' , " 'WAERS'
          tabname   'DMBTR4'     'CFIELDNAME'   '' , " 'WAERS'
          tabname   'DMBTR5'     'CFIELDNAME'   '' , " 'WAERS'
          tabname   'DMBTR6'     'CFIELDNAME'   '' , " 'WAERS'
          tabname   'DEVIR'       'DO_SUM'        ''           ,
          tabname   'DMBTR'       'DO_SUM'        ''           ,
          tabname   'DMBTR1'      'DO_SUM'        ''           ,
          tabname   'DMBTR2'      'DO_SUM'        ''           ,
          tabname   'DMBTR3'      'DO_SUM'        ''           ,
          tabname   'DMBTR4'      'DO_SUM'        ''           ,
          tabname   'DMBTR5'      'DO_SUM'        ''           ,
          tabname   'DMBTR6'      'DO_SUM'        ''           ,
          tabname   'DMBTR'       'NO_ZERO'        'X'         ,
          tabname   'DMBTR1'      'NO_ZERO'        'X'         ,
          tabname   'DMBTR2'      'NO_ZERO'        'X'         ,
          tabname   'DMBTR3'      'NO_ZERO'        'X'         ,
          tabname   'DMBTR4'      'NO_ZERO'        'X'         ,
          tabname   'DMBTR5'      'NO_ZERO'        'X'         ,
          tabname   'DMBTR6'      'NO_ZERO'        'X'         ,
          tabname   'DEVIR'       'NO_ZERO'       'X'          ,
          tabname   'TYPE'        'NO_OUT'        'X'          ,
          tabname   'DMBTR5'      'NO_OUT'        'X'          ,
          tabname   'DMBTR6'      'NO_OUT'        'X'          ,
          tabname   'DMBTR'       'NO_OUT'        'X'          ,
          tabname   'BELNR'      'HOTSPOT'       'X'           ,
          tabname   'WAERS'       lv_temp         'PB'  ,
          tabname   'PBIRIMI'     lv_temp      '(BP)Para Birimi',
          tabname   'VALOR'       lv_temp        'Valör Tarihi',
          tabname   'GECIK'       lv_temp        'Geçikme'     ,
          tabname   'DEVIR'       lv_temp        'Devreden'    ,
          tabname   'AUFNR_TEXT'  lv_temp   'İç Sipariş Tanımı',
          tabname   'NAME3'  lv_temp        'S.Böl',
          tabname   'XREF1_HD'  lv_temp     'Baş.Ref.Anahtar 1',
          tabname   'XREF2_HD'  lv_temp        'Arşiv',
          tabname   'DMBE2'  lv_temp        'UPB2(USD)' ,
          tabname   'DMBE3'  lv_temp        'UPB3(EUR)',
          tabname   'BFIYAT'  lv_temp        'Birim Fiyat',
*          tabname   'PRCTR'       lv_temp         'Kar Merkezi',
*          tabname   'NAME_LFA1'       lv_temp         'Satıcı Adı',
*          tabname   'NAME_KNA1'       lv_temp         'Müşteri Adı',
          tabname   'GSBER'       lv_temp         'İş Alanı'    ,
          tabname   'ZLSPR'       lv_temp       'Ödeme Blokajı',
          tabname   'LTEXT'       lv_temp       'ÖDK Tanımı'   ,
          tabname   'STCD2'       lv_temp       'Vergi Numarası',
          tabname   'HKONT'       lv_temp       'Hesap Kodu'   ,
          tabname   'NAME'        lv_temp       'Hesap Adı'    ,
          tabname   'DMBTR1'      lv_temp       'Borç'         ,
          tabname   'DMBTR2'      lv_temp       'Alacak'       ,
          tabname   'DMBTR3'      lv_temp       'Bakiye Borç'  ,
          tabname   'DMBTR4'      lv_temp       'Bakiye Alacak',
          tabname   'DMBTR5'      lv_temp       'Bakiye'       ,
          tabname   'DMBTR6'      lv_temp       'Kümüle Bakiye',
          tabname   'TABIX'       lv_temp       'İşlem No'     ,
          tabname   'LINE'        lv_temp       'Matbu No'     ,
          tabname   'BELNR_ALT'   lv_temp       'Yevmiye No'   ,
          tabname   'VALUT'       lv_temp       'Vade Tarihi' .

  alv_fieldcat-key = space .
  MODIFY alv_fieldcat TRANSPORTING key WHERE key = 'X' .
ENDFORM.                    " set_alv_fieldcat_merge
*&---------------------------------------------------------------------*
*&      Form  me_INITIALIZATION
*&---------------------------------------------------------------------*
FORM me_initialization .
  CONCATENATE icon_data_area_expand gv_flt10
              INTO gv_but10 SEPARATED BY space.

  p_gl = 'X'.
  s_bldat-sign = 'I'.
  s_bldat-option = 'BT'.
  s_bldat-low = s_bldat-high = sy-datum.
  s_bldat-low+4(4) = '0101'.
  APPEND s_bldat.

  SELECT SINGLE * FROM t001 INTO it_h_t001
     WHERE bukrs = p_bukrs.
  INSERT table it_h_t001.

ENDFORM.                    " me_INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  set_initial_value
*&---------------------------------------------------------------------*
FORM set_initial_value .
  SELECT SINGLE       * FROM  t001
         WHERE  bukrs  = p_bukrs.

  SELECT SINGLE  * FROM adrc WHERE addrnumber = t001-adrnr .
*  select single * from zallgugent002 where bukrs = p_bukrs .

*    if zallgugent002-company = 'AYGAZ' .
*       alv_logo = 'ZAYGAZ_LOGO' .
*    elseif zallgugent002-company = 'ADG' .
*       alv_logo = 'ZADG_LOGO' .
*    elseif zallgugent002-company = 'AKPA' .
*       alv_logo = 'ZAKPA_LOGO' .
*    endif.

  alv_info = adrc-name1 .

ENDFORM.                    " set_initial_value
*&---------------------------------------------------------------------*
*&      Form  MODIFY_TABLE
*&---------------------------------------------------------------------*
FORM modify_table .
  DATA: lv_line      TYPE i,
        lt_line      LIKE tline OCCURS 1 WITH HEADER LINE,
        ls_line      LIKE tline ,
        lv_langu     LIKE sy-langu,
        lv_name      LIKE thead-tdname,
        lv_ba_durum  LIKE gs_rtab2-dmbtr3.
  lv_langu = 'TR'.
  LOOP AT rtab.
    gs_rtab2-devir  = gs_rtab2-devir  + rtab-devir.
    gs_rtab2-dmbtr1 = gs_rtab2-dmbtr1 + rtab-dmbtr1.
    gs_rtab2-dmbtr2 = gs_rtab2-dmbtr2 + rtab-dmbtr2.
****  sd de faturada girilen matbu numarasının okunması
****  sadece 9 ile başlayan belgelere bakılacak
    IF rtab-awkey+0(1) EQ 9.
      CLEAR: lv_name, ls_line.
      REFRESH lt_line.
      lv_name = rtab-awkey.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                  = sy-mandt
          id                      = '0002'
          language                = lv_langu
          name                    = lv_name
          object                  = 'VBBK'
        TABLES
          lines                   = lt_line
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
      ENDIF.
      READ TABLE lt_line INTO ls_line INDEX 1.
      rtab-line = ls_line-tdline.
    ENDIF.
    MODIFY rtab.
  ENDLOOP.

*  DESCRIBE TABLE rtab LINES lv_line.
*
*  READ TABLE rtab INTO gs_rtab INDEX lv_line.
*  IF NOT gs_rtab-dmbtr3 IS INITIAL.
*    gs_rtab2-dmbtr3 = gs_rtab-dmbtr3.
*  ELSEIF NOT gs_rtab-dmbtr4 IS INITIAL.
*    gs_rtab2-dmbtr4 = gs_rtab-dmbtr4.
*  ENDIF.
  lv_ba_durum = ( gs_rtab2-devir + gs_rtab2-dmbtr1 )
                - gs_rtab2-dmbtr2.

  IF lv_ba_durum < 0.
    gs_rtab2-dmbtr4 = lv_ba_durum.
  ELSEIF lv_ba_durum > 0.
    gs_rtab2-dmbtr3 = lv_ba_durum.
  ENDIF.
  gs_rtab2-name = 'TOPLAM'.
  MOVE-CORRESPONDING gs_rtab2 TO rtab.
  rtab-line_color = 'C310'.
  APPEND rtab.

ENDFORM.                    " MODIFY_TABLE

*&--------------------------------------------------------------------*
*& Form SET_MENU
*&--------------------------------------------------------------------*
FORM set_menu USING rt_extab TYPE slis_t_extab.
*  select single * from zaygfigent017 where uname = sy-uname.
*  if sy-subrc = 0.
  SET PF-STATUS 'MENU2'.
*  else.
*    set pf-status 'MENU3'.
*  endif.

ENDFORM. "SET_MENU

*&---------------------------------------------------------------------*
*&      Form  fill_appended_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_appended_fields CHANGING ln_itab LIKE itab.
**değerleme alanları
*  SELECT SINGLE bwshb
*   INTO itab-bwshb3
*   FROM bsbw
*  WHERE bukrs EQ itab-bukrs
*    AND belnr EQ itab-belnr
*    AND gjahr EQ itab-gjahr
*    AND buzei EQ itab-buzei
*    AND curtp EQ '10'
*    AND bwber EQ 'Y3'
*    AND methd EQ '1' .

*  SELECT SINGLE bwshb
*    INTO itab-bwshb4
*    FROM bsbw
*   WHERE bukrs EQ itab-bukrs
*     AND belnr EQ itab-belnr
*     AND gjahr EQ itab-gjahr
*     AND buzei EQ itab-buzei
*     AND curtp EQ '10'
*     AND bwber EQ 'Y4'
*     AND methd EQ '1' .

*  SELECT SINGLE bwshb
*    INTO itab-bwshb5
*    FROM bsbw
*   WHERE bukrs EQ itab-bukrs
*     AND belnr EQ itab-belnr
*     AND gjahr EQ itab-gjahr
*     AND buzei EQ itab-buzei
*     AND curtp EQ '10'
*     AND bwber EQ 'Y5'
*     AND methd EQ '1' .

**karşı hesap ve türü
  CALL FUNCTION 'GET_GKONT'
    EXPORTING
      belnr           = itab-belnr
      bukrs           = itab-bukrs
      buzei           = itab-buzei
      gjahr           = itab-gjahr
      gknkz           = '3'
    IMPORTING
      gkont           = itab-gkont
      koart           = itab-gkart
    EXCEPTIONS
      belnr_not_found = 1
      buzei_not_found = 2
      gknkz_not_found = 3
      OTHERS          = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

**malzeme
*****  SELECT SINGLE matnr menge meins absbt bdiff zlspr
*****   INTO (itab-matnr,itab-menge,itab-meins,itab-absbt,
*****            itab-bdiff,itab-zlspr)
*****   FROM bseg
*****  WHERE bukrs EQ itab-bukrs
*****    AND belnr EQ itab-belnr
*****    AND gjahr EQ itab-gjahr
*****    AND buzei EQ itab-buzei.
*****
*****  IF itab-matnr IS NOT INITIAL.
*****    SELECT SINGLE maktx FROM makt INTO itab-maktx
*****      WHERE matnr EQ itab-matnr.
*****
*****  ENDIF.
ENDFORM.                    "fill_appended_fields

*&---------------------------------------------------------------------*
*&      Form  find_bwwrt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM find_bwwrt USING p_bsis LIKE bsis lv_bwwrt.

  DATA: wa_bsegp  LIKE bsegp,
        wa_pos    LIKE rfposxext,
        p_keydate LIKE sy-datum.

  p_keydate      = '99991231'. "sy-datum.
  wa_pos-koart   = 'S'. "c_koart_gl.
  wa_pos-konto   = p_bsis-hkont.
  wa_pos-dmshb   = p_bsis-dmbtr.
  wa_pos-wrshb   = p_bsis-wrbtr.
  wa_pos-vbewa   = p_bsis-bewar.                            "944067
  wa_bsegp-bdiff = p_bsis-bdiff.
  wa_bsegp-bdif2 = p_bsis-bdif2.
  wa_bsegp-bdif3 = p_bsis-bdif3.
* derive nontrivial rfpos fields:
  CALL FUNCTION 'ITEM_DERIVE_FIELDS'
    EXPORTING
      s_t001    = it_h_t001
      s_bsegp   = wa_bsegp
      key_date  = p_keydate
      xopvw     = p_bsis-xopvw
    CHANGING
      s_item    = wa_pos
    EXCEPTIONS
      bad_input = 1
      OTHERS    = 2.
  IF sy-subrc NE 0.
*    MESSAGE a022.
  ENDIF.

  lv_bwwrt = wa_pos-bwwrt.

ENDFORM.                    "find_bwwrt
*&---------------------------------------------------------------------*
*&      Form  MODIFY_RTAB
*&---------------------------------------------------------------------*
FORM modify_rtab .
  DATA: ls_rtab   LIKE rtab,
        ls_append LIKE rtab.
  DATA:lt_rtab LIKE rtab OCCURS 1 WITH HEADER LINE.

  DATA: BEGIN OF lt_temp OCCURS 1,
        hkont   LIKE bsis-hkont  ,
        saknr   LIKE bseg-saknr  ,
        END OF lt_temp.
*  break sinae.
  lt_rtab[] = rtab[].

  CLEAR: rtab[], rtab .

*  LOOP AT lt_rtab.
*    CLEAR: lt_temp.
*    lt_temp-hkont = lt_rtab-hkont.
*    lt_temp-saknr = lt_rtab-saknr.
*    COLLECT lt_temp.
*  ENDLOOP.

*LOOP AT lt_temp.
*  LOOP AT lt_rtab WHERE hkont = lt_temp-hkont
*                    and saknr = lt_temp-saknr.
*
*  ENDLOOP.
*ENDLOOP.

  LOOP AT lt_rtab INTO ls_rtab.
    CLEAR: ls_append.
    ON CHANGE OF ls_rtab-hkont.
      ls_append-hkont = ls_rtab-hkont.
      ls_append-tabix = 'DEVİR'.
      ls_append-sgtxt = 'DEVİR'.
      IF ls_rtab-devir >= 0.
        ls_append-dmbtr1 = ls_rtab-devir.
      ELSE.
        ls_append-dmbtr2 = ls_rtab-devir.
      ENDIF.
      ls_append-line_color = 'C410'.
      APPEND ls_append TO rtab.
    ENDON.
    MOVE-CORRESPONDING ls_rtab TO rtab.
    APPEND rtab.
  ENDLOOP.
ENDFORM.                    " MODIFY_RTAB
*&---------------------------------------------------------------------*
*&      Form  ADD_DEVIR
*&---------------------------------------------------------------------*
FORM add_devir .
  DATA: ls_rtab   LIKE rtab,
        ls_append LIKE rtab.
  CLEAR: ls_append.
  IF p_gl = 'X'.
    ls_append-hkont = devir-hkont.
    ls_append-saknr = devir-hkont.
  ELSE.
    ls_append-hkont = devir-mu_sa.
    ls_append-saknr = devir-hkont.
  ENDIF.
  ls_append-tabix = 'DEVİR'.
  ls_append-sgtxt = 'DEVİR'.
*  IF devir-dmbtr >= 0.
*    ls_append-dmbtr1 = devir-dmbtr.
*  ELSE.
*    ls_append-dmbtr2 = devir-dmbtr.
*  ENDIF.
  ls_append-dmbtr1 = devir-alck.
  ls_append-dmbtr2 = devir-borc.

  IF devir-dmbtr > 0 .
    ls_append-dmbtr3 = devir-dmbtr.
  ELSE.
    ls_append-dmbtr4 = devir-dmbtr.
  ENDIF.


  ls_append-line_color = 'C410'.
  APPEND ls_append TO rtab.
ENDFORM.                    " ADD_DEVIR
*&---------------------------------------------------------------------*
*&      Form  ADD_BOS_DEVIR
*&---------------------------------------------------------------------*
FORM add_bos_devir .
  DATA: ls_rtab   LIKE rtab,
        ls_append LIKE rtab.
  CLEAR: ls_append.
  ls_append-hkont = itab-hkont.
  ls_append-saknr = itab-saknr.
  ls_append-tabix = 'DEVİR'.
  ls_append-sgtxt = 'DEVİR'.
  ls_append-line_color = 'C410'.
  APPEND ls_append TO rtab.
  devir-mu_sa = itab-hkont.
  devir-hkont = itab-saknr.
ENDFORM.                    " ADD_BOS_DEVIR
*&---------------------------------------------------------------------*
*&      Form  PREPARE_ALV_TABLE2
*&---------------------------------------------------------------------*
FORM prepare_alv_table2 .
  IF itab[] IS INITIAL.
    MESSAGE s886(f4).
    SUBMIT (sy-repid) VIA SELECTION-SCREEN.
  ENDIF.
* SORT ITAB BY HKONT WAERS TYPE BUDAT BLDAT XBLNR.
  SORT itab BY hkont budat."budat ."belnr .
  CLEAR devir.
  LOOP AT itab.

    CLEAR rtab.
    MOVE : sy-tabix   TO rtab-tabix   ,
           t001-waers TO rtab-hwaer   .

    IF p_bpb = 'X'.
      itab-tutar = itab-wrbtr .
    ELSE.
      itab-tutar = itab-dmbtr .
*      itab-waers = t001-waers.
    ENDIF.
*    IF itab-shkzg EQ 'H' .
*      itab-dmbtr = itab-dmbtr * -1 .
*      itab-wrbtr = itab-wrbtr * -1 .
*    ENDIF.

    rtab-name = name.
*    RTAB-DEVIR = DEVIR-DMBTR.
    MOVE-CORRESPONDING itab TO rtab.
    IF itab-hkont NE devir-hkont.  "OR itab-waers NE devir-waers .
      PERFORM header.
      IF p_top = 'X'.
        cumule = cumule + devir-dmbtr.
      ENDIF.
      PERFORM item_line.
    ELSE.
      PERFORM item_line.
    ENDIF.
    PERFORM read_yevmiye_no.
    IF NOT itab-umskz EQ ''.
      IF NOT itab-lifnr IS INITIAL.
        SELECT SINGLE ltext INTO rtab-ltext
          FROM t074t
         WHERE spras = sy-langu
           AND shbkz = itab-umskz
           AND koart = 'K'.
      ELSEIF NOT itab-kunnr IS INITIAL.
        SELECT SINGLE ltext INTO rtab-ltext
          FROM t074t
         WHERE spras = sy-langu
           AND shbkz = itab-umskz
           AND koart = 'D'.

      ENDIF.
    ENDIF.
    IF itab-lifnr NE ''.
      SELECT SINGLE stcd2 INTO rtab-stcd2
        FROM lfa1
       WHERE lifnr = itab-lifnr.
    ENDIF.
    IF itab-kunnr NE ''.
      SELECT SINGLE stcd2 INTO rtab-stcd2
        FROM kna1
       WHERE kunnr = itab-kunnr.
    ENDIF.
    APPEND rtab.
  ENDLOOP.

ENDFORM.                    " PREPARE_ALV_TABLE2
*&---------------------------------------------------------------------*
*&      Form  HEADER
*&---------------------------------------------------------------------*
FORM header .
  CLEAR : cumule , s_top, h_top.

  CLEAR devir.
  READ TABLE devir WITH KEY hkont = itab-hkont
                            waers = itab-waers.
  MOVE : itab-hkont TO devir-hkont ,
         itab-waers TO devir-waers .
  rtab-devir = devir-dmbtr.

***Get name
  IF p_gl = 'X'.
    CLEAR: lv_ktopl.

    SELECT SINGLE ktopl FROM t001
                        INTO lv_ktopl
                       WHERE bukrs = p_bukrs.
    SELECT SINGLE txt20 INTO name
         FROM skat WHERE spras = sy-langu
                     AND saknr = itab-hkont
                     AND ktopl = lv_ktopl.
  ELSEIF p_mu = 'X'.
    SELECT SINGLE name1 INTO name
         FROM kna1 WHERE kunnr = itab-hkont .
  ELSE.
    SELECT SINGLE name1 INTO name
         FROM lfa1 WHERE lifnr = itab-hkont .
  ENDIF.
  rtab-name = name.
ENDFORM.                    " HEADER
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DEVIR
*&---------------------------------------------------------------------*
FORM modify_devir .
  DATA: lv_tanim  LIKE skat-txt20,
        lv_name1  LIKE kna1-name1,
        lv_lifnr  LIKE lfa1-lifnr,
        lv_kunnr  LIKE kna1-kunnr.



  DATA: lv_ktopl TYPE t001-ktopl.

  SELECT SINGLE ktopl FROM t001
                      INTO lv_ktopl
                      WHERE bukrs = p_bukrs.


  LOOP AT rtab WHERE sgtxt = 'DEVİR'.
    CLEAR: lv_tanim.
    IF p_gl = 'X'.
      rtab-belnr = 'DEVİR'.
      SELECT SINGLE txt20 INTO lv_tanim
        FROM skat
       WHERE spras = 'TR'
         AND ktopl = lv_ktopl
         AND saknr = rtab-saknr.
      CONCATENATE rtab-hkont(10) '-' lv_tanim(20)
             INTO rtab-sgtxt.
    ELSEIF p_mu = 'X'.
      rtab-belnr = 'DEVİR'.
      SELECT SINGLE txt20 INTO lv_tanim
        FROM skat
       WHERE spras = 'TR'
         AND ktopl = lv_ktopl
         AND saknr = rtab-saknr.
      SELECT SINGLE name1 INTO lv_name1
        FROM kna1
       WHERE kunnr = rtab-hkont.
      PACK rtab-hkont TO lv_kunnr.
      CONDENSE lv_kunnr.
      CONCATENATE rtab-saknr(10) '-' lv_tanim(15) '/'
                  lv_kunnr(7)    '-' lv_name1(15)
             INTO rtab-sgtxt.
    ELSEIF p_sa = 'X'.
      rtab-belnr = 'DEVİR'.
      SELECT SINGLE txt20 INTO lv_tanim
        FROM skat
       WHERE spras = 'TR'
         AND ktopl = lv_ktopl
         AND saknr = rtab-saknr.
      SELECT SINGLE name1 INTO lv_name1
        FROM lfa1
       WHERE lifnr = rtab-hkont.
*      PACK rtab-hkont TO lv_lifnr."deleted by EmreK.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = rtab-hkont
        IMPORTING
          output = lv_lifnr
        EXCEPTIONS
          OTHERS = 1.
      CONDENSE lv_lifnr.
      CONCATENATE rtab-saknr(10) '-' lv_tanim(15) '/'
                  lv_lifnr(7)    '-' lv_name1(15)
             INTO rtab-sgtxt.
    ENDIF.
    MODIFY rtab.
  ENDLOOP.
ENDFORM.                    " MODIFY_DEVIR

*&---------------------------------------------------------------------*
* Form END_OF_LIST
*
* Clear flags in case the user choses to reprint the report
*
*-------------------------------------------------------------------
FORM end_of_list.
ENDFORM.                    "END_OF_LIST
*&---------------------------------------------------------------------*
*&      Form  MUST_YETKI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM satbol_yetki .
*  DATA: LV_SATBOL TYPE ZAYGFIMOD_SATIS_BOLGESI.
*
*  LOOP AT ITAB.
*
*    LV_SATBOL = ITAB-NAME3.
*    AUTHORITY-CHECK OBJECT 'ZSATBOL'
*           FOR USER SY-UNAME
*           ID 'ZSATBOL'  FIELD LV_SATBOL.
*    IF SY-SUBRC <> 0.
*      DELETE ITAB.
*    ENDIF.
*
*  ENDLOOP.

ENDFORM.                    " MUST_YETKI


*&---------------------------------------------------------------------*
*&      Form  DYNAMIC_SH_NAME3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dynamic_sh_name3 USING p_fieldname.
*
*  data: return_tab like ddshretval occurs 0 with header line.
*  data: begin of val_tab1 occurs 0.
*          include structure zaygfimodt020.
*  data  end of val_tab1.
*  clear: val_tab1, val_tab1[].
*
*  select * into corresponding fields of table
*                              val_tab1 from zaygfimodt020.
*
*  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
*    exporting
*      retfield    = 'REGIOGROUP'
*      dynpprog    = sy-repid
*      dynpnr      = sy-dynnr
*      dynprofield = p_fieldname
*      value_org   = 'S'
*    tables
*      value_tab   = val_tab1
*      return_tab  = return_tab.
*
ENDFORM.                    " DYNAMIC_SH_NAME3
*&---------------------------------------------------------------------*
*&      Form  YETKI_KONTROL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM yetki_kontrol .

*  CLEAR: gv_sistem_yon.
**  SELECT SINGLE *
**    FROM zmuh_liste_yetki
**    INTO gs_liste_yetki
**   WHERE danisman EQ sy-uname.
*
*  IF sy-subrc EQ 0.
*    IF gs_liste_yetki-prodea_cb EQ 'X' AND
*       gs_liste_yetki-inera_cb  EQ 'X' AND
*       gs_liste_yetki-fiz_cb    EQ 'X' AND
*       gs_liste_yetki-easy_cb   EQ 'X'.
*      gv_sistem_yon = 'X'.
*    ELSE.
*      CLEAR gv_sistem_yon.
*      IF gs_liste_yetki-prodea_cb EQ 'X'.
*        p_bukrs = 'PRO'.
*      ELSEIF gs_liste_yetki-inera_cb EQ 'X'.
*        p_bukrs = 'INRA'.
*      ELSEIF gs_liste_yetki-fiz_cb EQ 'X'.
*        p_bukrs = 'FIZ'.
**    ELSEIF gs_liste_yetki-prodea_cb EQ 'X'.
*      ENDIF.
*    ENDIF.
*  ELSE.
*    CLEAR p_bukrs.
*    MESSAGE 'Hiçbir şirket koduna yetkiniz bulunmamaktadır. Lütfen ' &
*    'yöneticinizle iletişime geçiniz' TYPE 'I' DISPLAY LIKE 'E'.
*  ENDIF.

ENDFORM.                    " YETKI_KONTROL
