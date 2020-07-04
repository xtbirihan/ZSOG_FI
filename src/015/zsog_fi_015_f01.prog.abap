*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_015_F01
*&---------------------------------------------------------------------*
*&-----FORMS-----------------------------------------------------------*
FORM select_vendors .

  SELECT SINGLE * FROM t001 WHERE bukrs = p_bukrs.

  SELECT lfa1~lifnr AS kunnr lfa1~name1 lfb1~akont
           FROM lfb1 INNER JOIN lfa1
             ON lfb1~lifnr = lfa1~lifnr
        APPENDING CORRESPONDING FIELDS OF TABLE tb_knb1
        WHERE lfb1~bukrs = p_bukrs
          AND lfb1~akont IN s_hkont
          AND lfa1~lifnr IN s_lifnr
          AND lfa1~regio IN s_regio.

ENDFORM.                    " select_vendors
*----------------------------------------------------------------------*
FORM select_customers .
  SELECT SINGLE * FROM t001 WHERE bukrs = p_bukrs.

  SELECT kna1~kunnr AS kunnr kna1~name1 knb1~akont
           FROM knb1 INNER JOIN kna1
             ON knb1~kunnr = kna1~kunnr
        INTO CORRESPONDING FIELDS OF TABLE tb_knb1
        WHERE kna1~regio IN s_regio
          AND knb1~bukrs = p_bukrs
          AND knb1~akont IN s_hkont
          AND kna1~kunnr IN s_kunnr.
ENDFORM.                    " select_vendors
*&---------------------------------------------------------------------*
*&      Form  select_customers2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_customers2 .
  SELECT SINGLE * FROM t001 WHERE bukrs = p_bukrs.

  SELECT lfa1~lifnr AS kunnr kna1~name1 knb1~akont
           FROM knb1 INNER JOIN kna1
             ON knb1~kunnr = kna1~kunnr
           INNER JOIN lfa1
             ON kna1~kunnr = lfa1~kunnr
        INTO CORRESPONDING FIELDS OF TABLE tb_knb1
        WHERE kna1~regio IN s_regio
          AND knb1~bukrs = p_bukrs
          AND knb1~akont IN s_hkont
          AND kna1~kunnr IN s_kunnr.
ENDFORM.                    " select_vendors
*&---------------------------------------------------------------------*
FORM select_vendors_open_items .

  LOOP AT tb_knb1.
    FREE  : tb_bsid, tb_bsad.

    IF s_gsber IS INITIAL.
      SELECT bsik~lifnr AS kunnr
          bsik~belnr bsik~xblnr bsik~budat bsik~shkzg bsik~zfbdt
          bsik~zbd1t bsik~dmbtr bsik~dmbe2 bsik~umskz
          bsik~gsber
          APPENDING CORRESPONDING FIELDS OF TABLE tb_bsid
        FROM bsik INNER JOIN bkpf ON bkpf~bukrs = bsik~bukrs
                             AND bkpf~belnr = bsik~belnr
                             AND bkpf~gjahr = bsik~gjahr
        WHERE bsik~bukrs =  p_bukrs
          AND bsik~lifnr =  tb_knb1-kunnr
          AND bsik~budat LE p_keydt
          AND bsik~umskz =  space
          AND bkpf~stblg =  space.
    ELSE.
      SELECT bsik~lifnr AS kunnr
          bsik~belnr bsik~xblnr bsik~budat bsik~shkzg bsik~zfbdt
          bsik~zbd1t bsik~dmbtr bsik~dmbe2 bsik~umskz
          bsik~gsber
          APPENDING CORRESPONDING FIELDS OF TABLE tb_bsid
        FROM bsik INNER JOIN bkpf ON bkpf~bukrs = bsik~bukrs
                             AND bkpf~belnr = bsik~belnr
                             AND bkpf~gjahr = bsik~gjahr
        WHERE bsik~bukrs =  p_bukrs
          AND bsik~gsber =  s_gsber
          AND bsik~lifnr =  tb_knb1-kunnr
          AND bsik~budat LE p_keydt
          AND bsik~umskz =  space
          AND bkpf~stblg =  space.
    ENDIF.


    IF p_keydt < sy-datum.
      IF s_gsber IS INITIAL.
        SELECT bsak~lifnr AS kunnr
             bsak~belnr bsak~xblnr bsak~budat bsak~shkzg bsak~zfbdt
             bsak~zbd1t bsak~dmbtr bsak~dmbe2 bsak~umskz
             bsak~gsber
           APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad
           FROM bsak INNER JOIN bkpf ON bkpf~bukrs = bsak~bukrs
                                AND bkpf~belnr = bsak~belnr
                                AND bkpf~gjahr = bsak~gjahr
           WHERE bsak~bukrs =  p_bukrs
             AND bsak~lifnr =  tb_knb1-kunnr
             AND bsak~budat LE p_keydt
             AND bsak~augdt GT p_keydt
             AND bsak~umskz =  space
             AND bkpf~stblg =  space.
      ELSE.
        SELECT bsak~lifnr AS kunnr
             bsak~belnr bsak~xblnr bsak~budat bsak~shkzg bsak~zfbdt
             bsak~zbd1t bsak~dmbtr bsak~dmbe2 bsak~umskz
             bsak~gsber
           APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad
           FROM bsak INNER JOIN bkpf ON bkpf~bukrs = bsak~bukrs
                                AND bkpf~belnr = bsak~belnr
                                AND bkpf~gjahr = bsak~gjahr
           WHERE bsak~bukrs =  p_bukrs
             AND bsak~gsber = s_gsber
             AND bsak~lifnr =  tb_knb1-kunnr
             AND bsak~budat LE p_keydt
             AND bsak~augdt GT p_keydt
             AND bsak~umskz =  space
             AND bkpf~stblg =  space.
      ENDIF.



      APPEND LINES OF tb_bsad TO tb_bsid.
    ENDIF.

    IF tb_bsid[] IS INITIAL.
      DELETE tb_knb1.
      CONTINUE.
    ENDIF.
    PERFORM build_borc_alac_tables.
  ENDLOOP.
ENDFORM.                    " select_open_items
*&---------------------------------------------------------------------*
FORM select_customers_open_items .

  LOOP AT tb_knb1.
    FREE  : tb_bsid, tb_bsad.

    IF s_gsber IS INITIAL.
      SELECT bsid~kunnr bsid~belnr bsid~xblnr bsid~budat bsid~shkzg
           bsid~zfbdt bsid~zbd1t bsid~dmbtr bsid~dmbe2 bsid~umskz
           bsid~gsber bsid~waers bsid~wrbtr
           INTO CORRESPONDING FIELDS OF TABLE tb_bsid
         FROM bsid INNER JOIN bkpf ON bkpf~bukrs = bsid~bukrs
                                  AND bkpf~belnr = bsid~belnr
                                  AND bkpf~gjahr = bsid~gjahr
         WHERE bsid~bukrs =  p_bukrs
           AND bsid~kunnr =  tb_knb1-kunnr
           AND bsid~budat LE p_keydt
           AND bsid~umskz =  space
           AND bkpf~stblg =  space.
    ELSE.
      SELECT bsid~kunnr bsid~belnr bsid~xblnr bsid~budat bsid~shkzg
           bsid~zfbdt bsid~zbd1t bsid~dmbtr bsid~dmbe2 bsid~umskz
           bsid~gsber bsid~waers bsid~wrbtr
           INTO CORRESPONDING FIELDS OF TABLE tb_bsid
         FROM bsid INNER JOIN bkpf ON bkpf~bukrs = bsid~bukrs
                                  AND bkpf~belnr = bsid~belnr
                                  AND bkpf~gjahr = bsid~gjahr
         WHERE bsid~bukrs =  p_bukrs
           AND bsid~gsber = s_gsber
           AND bsid~kunnr =  tb_knb1-kunnr
           AND bsid~budat LE p_keydt
           AND bsid~umskz =  space
           AND bkpf~stblg =  space.
    ENDIF.


    IF tb_knb1-kunnr IS INITIAL.

      IF s_gsber IS INITIAL.

        SELECT bsid~kunnr bsid~belnr bsid~xblnr bsid~budat bsid~shkzg
               bsid~zfbdt bsid~zbd1t bsid~dmbtr bsid~dmbe2 bsid~umskz
               bsid~gsber bsid~waers bsid~wrbtr
     APPENDING CORRESPONDING FIELDS OF TABLE tb_bsid
         FROM bsid INNER JOIN bkpf ON bkpf~bukrs = bsid~bukrs
                                  AND bkpf~belnr = bsid~belnr
                                  AND bkpf~gjahr = bsid~gjahr
         WHERE bsid~bukrs =  p_bukrs
*         and bsid~kunnr =  tb_knb1-kunnr
           AND bsid~budat LE p_keydt
           AND bsid~umskz =  space
           AND bkpf~stblg =  space
           AND bsid~filkd NE space.
      ELSE.
        SELECT bsid~kunnr bsid~belnr bsid~xblnr bsid~budat bsid~shkzg
          bsid~zfbdt bsid~zbd1t bsid~dmbtr bsid~dmbe2 bsid~umskz
          bsid~gsber bsid~waers bsid~wrbtr" aylint 04.09.2014
          APPENDING CORRESPONDING FIELDS OF TABLE tb_bsid
          FROM bsid INNER JOIN bkpf ON bkpf~bukrs = bsid~bukrs
                         AND bkpf~belnr = bsid~belnr
                         AND bkpf~gjahr = bsid~gjahr
          WHERE bsid~bukrs =  p_bukrs
          AND bsid~gsber = s_gsber
*         and bsid~kunnr =  tb_knb1-kunnr
          AND bsid~budat LE p_keydt
          AND bsid~umskz =  space
          AND bkpf~stblg =  space
          AND bsid~filkd NE space.
      ENDIF.


    ELSE.

      IF s_gsber IS INITIAL.
        SELECT bsid~kunnr bsid~belnr bsid~xblnr bsid~budat bsid~shkzg
               bsid~zfbdt bsid~zbd1t bsid~dmbtr bsid~dmbe2 bsid~umskz
               bsid~gsber bsid~waers bsid~wrbtr
       APPENDING CORRESPONDING FIELDS OF TABLE tb_bsid
           FROM bsid INNER JOIN bkpf ON bkpf~bukrs = bsid~bukrs
                                    AND bkpf~belnr = bsid~belnr
                                    AND bkpf~gjahr = bsid~gjahr
           WHERE bsid~bukrs =  p_bukrs
*         and bsid~kunnr =  tb_knb1-kunnr
             AND bsid~budat LE p_keydt
             AND bsid~umskz =  space
             AND bkpf~stblg =  space
             AND bsid~filkd EQ tb_knb1-kunnr.
      ELSE.
        SELECT bsid~kunnr bsid~belnr bsid~xblnr bsid~budat bsid~shkzg
      bsid~zfbdt bsid~zbd1t bsid~dmbtr bsid~dmbe2 bsid~umskz
      bsid~gsber bsid~waers bsid~wrbtr
APPENDING CORRESPONDING FIELDS OF TABLE tb_bsid
  FROM bsid INNER JOIN bkpf ON bkpf~bukrs = bsid~bukrs
                           AND bkpf~belnr = bsid~belnr
                           AND bkpf~gjahr = bsid~gjahr
  WHERE bsid~bukrs =  p_bukrs
    AND bsid~gsber = s_gsber
*         and bsid~kunnr =  tb_knb1-kunnr
    AND bsid~budat LE p_keydt
    AND bsid~umskz =  space
    AND bkpf~stblg =  space
    AND bsid~filkd EQ tb_knb1-kunnr.
      ENDIF.
    ENDIF.

    IF p_keydt < sy-datum.

      IF s_gsber IS INITIAL.
        SELECT bsad~kunnr bsad~belnr bsad~xblnr bsad~budat bsad~shkzg
             bsad~zfbdt bsad~zbd1t bsad~dmbtr bsad~dmbe2 bsad~umskz
             bsad~gsber bsad~waers bsad~wrbtr
           APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad
           FROM bsad INNER JOIN bkpf ON bkpf~bukrs = bsad~bukrs
                                    AND bkpf~belnr = bsad~belnr
                                    AND bkpf~gjahr = bsad~gjahr
           WHERE bsad~bukrs =  p_bukrs
             AND bsad~kunnr =  tb_knb1-kunnr
             AND bsad~budat LE p_keydt
             AND bsad~augdt GT p_keydt
             AND bsad~umskz =  space
             AND bkpf~stblg =  space.
      ELSE.
        SELECT bsad~kunnr bsad~belnr bsad~xblnr bsad~budat bsad~shkzg
             bsad~zfbdt bsad~zbd1t bsad~dmbtr bsad~dmbe2 bsad~umskz
             bsad~gsber bsad~waers bsad~wrbtr
           APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad
           FROM bsad INNER JOIN bkpf ON bkpf~bukrs = bsad~bukrs
                                    AND bkpf~belnr = bsad~belnr
                                    AND bkpf~gjahr = bsad~gjahr
           WHERE bsad~bukrs =  p_bukrs
             AND bsad~gsber = s_gsber
             AND bsad~kunnr =  tb_knb1-kunnr
             AND bsad~budat LE p_keydt
             AND bsad~augdt GT p_keydt
             AND bsad~umskz =  space
             AND bkpf~stblg =  space.
      ENDIF.


      IF tb_knb1-kunnr IS INITIAL.
        IF s_gsber IS INITIAL.
          SELECT bsad~kunnr bsad~belnr bsad~xblnr bsad~budat bsad~shkzg
               bsad~zfbdt bsad~zbd1t bsad~dmbtr bsad~dmbe2 bsad~umskz
               bsad~gsber bsad~waers bsad~wrbtr
        APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad
        FROM bsad INNER JOIN bkpf ON bkpf~bukrs = bsad~bukrs
                                 AND bkpf~belnr = bsad~belnr
                                 AND bkpf~gjahr = bsad~gjahr
        WHERE bsad~bukrs =  p_bukrs
*             and bsad~kunnr =  tb_knb1-kunnr
          AND bsad~budat LE p_keydt
               AND bsad~augdt GT p_keydt
          AND bsad~umskz =  space
          AND bkpf~stblg =  space
          AND bsad~filkd NE space.
        ELSE.
          SELECT bsad~kunnr bsad~belnr bsad~xblnr bsad~budat bsad~shkzg
               bsad~zfbdt bsad~zbd1t bsad~dmbtr bsad~dmbe2 bsad~umskz
               bsad~gsber bsad~waers bsad~wrbtr
        APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad
        FROM bsad INNER JOIN bkpf ON bkpf~bukrs = bsad~bukrs
                                 AND bkpf~belnr = bsad~belnr
                                 AND bkpf~gjahr = bsad~gjahr
        WHERE bsad~bukrs =  p_bukrs
          AND bsad~gsber = s_gsber
*             and bsad~kunnr =  tb_knb1-kunnr
          AND bsad~budat LE p_keydt
               AND bsad~augdt GT p_keydt
          AND bsad~umskz =  space
          AND bkpf~stblg =  space
          AND bsad~filkd NE space.
        ENDIF.


      ELSE.
        IF s_gsber IS INITIAL.
          SELECT bsad~kunnr bsad~belnr bsad~xblnr bsad~budat bsad~shkzg
              bsad~zfbdt bsad~zbd1t bsad~dmbtr bsad~dmbe2 bsad~umskz
              bsad~gsber bsad~waers bsad~wrbtr
            APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad
            FROM bsad INNER JOIN bkpf ON bkpf~bukrs = bsad~bukrs
                                     AND bkpf~belnr = bsad~belnr
                                     AND bkpf~gjahr = bsad~gjahr
            WHERE bsad~bukrs =  p_bukrs
*             and bsad~kunnr =  tb_knb1-kunnr
              AND bsad~budat LE p_keydt
              AND bsad~augdt GT p_keydt
              AND bsad~umskz =  space
              AND bkpf~stblg =  space
              AND bsad~filkd EQ tb_knb1-kunnr.
        ELSE.
          SELECT bsad~kunnr bsad~belnr bsad~xblnr bsad~budat bsad~shkzg
              bsad~zfbdt bsad~zbd1t bsad~dmbtr bsad~dmbe2 bsad~umskz
              bsad~gsber bsad~waers bsad~wrbtr
            APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad
            FROM bsad INNER JOIN bkpf ON bkpf~bukrs = bsad~bukrs
                                     AND bkpf~belnr = bsad~belnr
                                     AND bkpf~gjahr = bsad~gjahr
            WHERE bsad~bukrs =  p_bukrs
              AND bsad~gsber = s_gsber
*             and bsad~kunnr =  tb_knb1-kunnr
              AND bsad~budat LE p_keydt
              AND bsad~augdt GT p_keydt
              AND bsad~umskz =  space
              AND bkpf~stblg =  space
              AND bsad~filkd EQ tb_knb1-kunnr.
        ENDIF.


      ENDIF.
      APPEND LINES OF tb_bsad TO tb_bsid.

    ENDIF.

    LOOP AT tb_bsid WHERE filkd NE space.
      tb_bsid-kunnr = tb_bsid-filkd.
      MODIFY tb_bsid.
    ENDLOOP.

    LOOP AT tb_bsad WHERE filkd NE space.
      tb_bsad-kunnr = tb_bsad-filkd.
      MODIFY tb_bsad.
    ENDLOOP.

    IF tb_bsid[] IS INITIAL.
      DELETE tb_knb1.
      CONTINUE.
    ENDIF.
    PERFORM build_borc_alac_tables.
  ENDLOOP.
ENDFORM.                    " select_open_items
*&---------------------------------------------------------------------*
FORM match_alac_borc
          TABLES i_borc LIKE tb_borc[]
                 i_alac LIKE tb_alac[].

  SORT i_borc BY kunnr ASCENDING zfbdt ASCENDING belnr ASCENDING.
  SORT i_alac BY kunnr ASCENDING zfbdt ASCENDING belnr ASCENDING.

  LOOP AT i_borc.

    LOOP AT i_alac WHERE kunnr = i_borc-kunnr.
      IF i_alac-kalan = i_borc-kalan.
        i_alac-kalan = 0.
        i_borc-kalan = 0.
*        delete i_alac.
        EXIT.
      ELSEIF i_alac-kalan < i_borc-kalan.
        SUBTRACT i_alac-kalan FROM i_borc-kalan.
*        delete i_alac.
        CONTINUE.
      ELSE.   "i_alac-kalan > i_borc-kalan
        SUBTRACT i_borc-kalan FROM i_alac-kalan.
        MODIFY i_alac TRANSPORTING kalan.
        i_borc-kalan = 0.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF i_borc-kalan = 0 .
*      delete i_borc.
      CONTINUE.
    ENDIF.
    MODIFY i_borc TRANSPORTING kalan.
  ENDLOOP.


ENDFORM.                    "match_alac_borc
*&---------------------------------------------------------------------*
FORM prepare_list .
  DATA : lv_arti10 LIKE sy-datum,
         wa        LIKE tb_borc.

  LOOP AT tb_alac.
    MOVE tb_alac TO tb_borc.
    tb_borc-dmbtr = tb_borc-dmbtr * -1.
    tb_borc-kalan = tb_borc-kalan * -1.
    APPEND tb_borc.
  ENDLOOP.


  SORT tb_borc BY kunnr zfbdt.

  LOOP AT tb_borc.
    tb_borc-gun    = p_keydt - tb_borc-zfbdt.
    tb_borc-carpim = tb_borc-kalan * tb_borc-gun.
    MODIFY tb_borc.
  ENDLOOP.

  DATA : i_topx   TYPE bapicurr_d, "çarpım
         gcm_topx TYPE bapicurr_d,
         ilr_topx TYPE bapicurr_d.

  SORT tb_borc BY kunnr belnr budat.

  LOOP AT tb_borc.
    wa = tb_borc.

***    at new kunnr.
    AT NEW gsber.
      CLEAR: tb_list,i_topx,gcm_topx,ilr_topx.
      MOVE-CORRESPONDING wa TO tb_list.
      READ TABLE tb_knb1 WITH KEY kunnr = tb_borc-kunnr.
      tb_list-name1 =  tb_knb1-name1.
      tb_list-hkont =  tb_knb1-akont.
      tb_list-alan  =  tb_borc-alan.

      CLEAR skat.
      SELECT SINGLE * FROM skat WHERE spras = sy-langu
                                  AND ktopl = 'ZIGZ'
                                  AND saknr = tb_list-hkont.
      tb_list-txt50 = skat-txt50.
      IF rb_try = 'X'.
        tb_list-waers = 'TRY'.
      ELSE.
        tb_list-waers = 'EUR'.
      ENDIF.

    ENDAT.


    IF tb_borc-zfbdt IN r_gvb01.
      ADD tb_borc-dmbtr TO tb_list-gvb01.
    ELSEIF tb_borc-zfbdt IN r_gvb02.
      ADD tb_borc-dmbtr TO tb_list-gvb02.
    ELSEIF tb_borc-zfbdt IN r_gvb03.
      ADD tb_borc-dmbtr TO tb_list-gvb03.
    ELSEIF tb_borc-zfbdt IN r_gvb04.
      ADD tb_borc-dmbtr TO tb_list-gvb04.
    ELSEIF tb_borc-zfbdt IN r_gvb05.
      ADD tb_borc-dmbtr TO tb_list-gvb05.
    ELSEIF tb_borc-zfbdt IN r_gvb06.
      ADD tb_borc-dmbtr TO tb_list-gvb06.

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:24:08
    ELSEIF tb_borc-zfbdt IN r_gvb07.
      ADD tb_borc-dmbtr TO tb_list-gvb07.
    ELSEIF tb_borc-zfbdt IN r_gvb08.
      ADD tb_borc-dmbtr TO tb_list-gvb08.
*      }    <<<- End of  Added - 07.10.2019 10:24:08

    ELSEIF tb_borc-zfbdt IN r_gvb99.
      ADD tb_borc-dmbtr TO tb_list-gvb99.
    ELSEIF tb_borc-zfbdt IN r_ivb01.
      ADD tb_borc-dmbtr TO tb_list-ivb01.
    ELSEIF tb_borc-zfbdt IN r_ivb02.
      ADD tb_borc-dmbtr TO tb_list-ivb02.
    ELSEIF tb_borc-zfbdt IN r_ivb03.
      ADD tb_borc-dmbtr TO tb_list-ivb03.
    ELSEIF tb_borc-zfbdt IN r_ivb04.
      ADD tb_borc-dmbtr TO tb_list-ivb04.
    ELSEIF tb_borc-zfbdt IN r_ivb05.
      ADD tb_borc-dmbtr TO tb_list-ivb05.
    ELSEIF tb_borc-zfbdt IN r_ivb06.
      ADD tb_borc-dmbtr TO tb_list-ivb06.

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:27:13
    ELSEIF tb_borc-zfbdt IN r_ivb07.
      ADD tb_borc-dmbtr TO tb_list-ivb07.
    ELSEIF tb_borc-zfbdt IN r_ivb08.
      ADD tb_borc-dmbtr TO tb_list-ivb08.
*      }    <<<- End of  Added - 07.10.2019 10:27:13

    ELSEIF tb_borc-zfbdt IN r_ivb99.
      ADD tb_borc-dmbtr TO tb_list-ivb99.
    ELSE.
      BREAK-POINT.  "kontrol için
    ENDIF.


    IF tb_borc-zfbdt < p_keydt. " geçmiş vade
      ADD tb_borc-dmbtr  TO tb_list-gvbtop.
      ADD tb_borc-carpim TO gcm_topx.
    ELSE. "ileri vade
      ADD tb_borc-dmbtr  TO tb_list-ivbtop.
      ADD tb_borc-carpim TO ilr_topx.
    ENDIF.

****    add tb_borc-kalan  to tb_list-bakiy.
    ADD tb_borc-dmbtr  TO tb_list-bakiy.
    ADD tb_borc-carpim TO i_topx.



***    at end of kunnr.
    AT END OF gsber.

      IF tb_list-bakiy NE 0.
        tb_list-ortgn = i_topx / tb_list-bakiy .
      ENDIF.

      IF tb_list-gvbtop NE 0.
        tb_list-gvbgn = gcm_topx / tb_list-gvbtop .
      ENDIF.
      IF tb_list-ivbtop NE 0.
        tb_list-ivbgn = ilr_topx / tb_list-ivbtop .
      ENDIF.
      APPEND tb_list.
    ENDAT.
    COLLECT tb_list INTO seats_tab.

  ENDLOOP.

  CLEAR tb_list.
  REFRESH tb_list.

  tb_list[] = seats_tab[].

  SORT tb_list BY hkont kunnr.


ENDFORM.                    " prepare_list
*&---------------------------------------------------------------------*
FORM display_list .
  va_repid = sy-repid.
  DATA:s_print TYPE slis_print_alv.

  PERFORM fcat_fill CHANGING lt_fcat.

  ls_layout-colwidth_optimize = 'X'.
  ls_layout-zebra             = 'X'.

  lt_sort-up = 'X'.
  lt_sort-fieldname = 'ALAN'.
  lt_sort-subtot = 'X'.
  APPEND lt_sort.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = va_repid
      i_callback_top_of_page = 'BASLIK_OLUSTUR'
      is_layout              = ls_layout
      it_fieldcat            = lt_fcat
*     it_sort                = lt_sort[]
      i_save                 = 'A'
      i_bypassing_buffer     = 'X'
      i_default              = space
      it_events              = gt_events
    TABLES
      t_outtab               = tb_list.
ENDFORM.                    " display_list
*----------------------------------------------------------------------*
FORM fcat_fill CHANGING ct_fcat TYPE slis_t_fieldcat_alv.

  DATA: ls_fcat TYPE slis_fieldcat_alv.
  DATA: lv_txt1 TYPE char10,
        lv_txt2 TYPE char10,
        lv_text TYPE char25,
        lv_txt3 TYPE char40.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSOG_FI_015_S_01'
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = ct_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  DEFINE text_change.
    clear ls_fcat.
    ls_fcat-seltext_l    = &1.
    ls_fcat-seltext_m    = &2.
    ls_fcat-seltext_s    = ls_fcat-seltext_m.
    ls_fcat-reptext_ddic = ls_fcat-seltext_m.
    ls_fcat-outputlen    = 25.
    ls_fcat-ddictxt      = 'M'.
    modify ct_fcat from ls_fcat
           transporting seltext_s seltext_m seltext_l reptext_ddic
outputlen
           where fieldname = &3.
  END-OF-DEFINITION.

  DEFINE no_out.
    clear ls_fcat.
    ls_fcat-no_out = 'X'.
    modify ct_fcat from ls_fcat
           transporting no_out where fieldname = &1.
  END-OF-DEFINITION.
  DEFINE do_sum.
    clear ls_fcat.
    ls_fcat-do_sum = 'X'.
    modify ct_fcat from ls_fcat
           transporting do_sum where fieldname = &1.
  END-OF-DEFINITION.
  DEFINE key_color.
    clear ls_fcat.
    ls_fcat-key = 'X'.
    modify ct_fcat from ls_fcat
           transporting key where fieldname = &1.
  END-OF-DEFINITION  .
  IF rb_mus = 'X'.
    text_change 'Müşteri Adı'             'Müşteri'          'NAME1'.
    text_change 'Müşteri No'              'Müşteri'          'KUNNR'.
  ELSE.
    text_change 'Satıcı Adı'              'Satıcı'           'NAME1'.
    text_change 'Satıcı No'               'Satıcı'           'KUNNR'.
  ENDIF.

  text_change 'Geçmiş Vadeli Bakiye'   'Geçmiş Vadeli'     'GVBTOP'.
  text_change 'İleri Vadeli Bakiye'    'İleri Vadeli'      'IVBTOP'.
  text_change 'Bakiye'                  'Bakiye'           'BAKIY'.
  text_change 'Ort.Vade(Gün)'     'Gün'     'ORTGN'.
  text_change 'Ort.Vade(Gün)'     'Gün'     'GVBGN'.
  text_change 'Ort.Vade(Gün)'     'Gün'     'IVBGN'.

  DATA : i_ilk(3) TYPE c,
         i_son(3) TYPE c.

  DEFINE vadeler.

    clear : lv_txt3,lv_text.

    if rb_gun = 'X'.
      if &2 = '+'.
        i_ilk = &1-low  - p_keydt.
        i_son = &1-high - p_keydt.
      else.
        i_ilk = p_keydt - &1-low.
        i_son = p_keydt - &1-high.
      endif.
      concatenate &2 i_ilk '-' i_son into lv_txt3.
    elseif rb_trh = 'X'.
      write &1-low  to lv_txt1.
      write &1-high to lv_txt2.

*{   ->>> Added by Prodea Sefa Taşkent - 03.10.2019 12:19:18
      lv_txt1 = lv_txt1+0(5).
      lv_txt2 = lv_txt2+0(5).
*      }    <<<- End of  Added - 03.10.2019 12:19:18

*      lv_txt1 = lv_txt1.
*      lv_txt2 = lv_txt2.
      concatenate lv_txt1 lv_txt2 into lv_text separated by '-'.
      concatenate lv_txt3 '(' lv_text ')' into lv_txt3.
    endif.

*    write &1-low  to lv_txt1.
*    write &1-high to lv_txt2.
*    concatenate lv_txt1 lv_txt2 into lv_text separated by '-'.
*    if &2 = '+'.
*      i_ilk = &1-low  - p_keydt.
*      i_son = &1-high - p_keydt.
*    else.
*      i_ilk = p_keydt - &1-low.
*      i_son = p_keydt - &1-high.
*    endif.
*    concatenate &2 i_ilk '-' i_son into lv_txt3.
*    concatenate lv_txt3 '(' lv_text ')' into lv_txt3."burcua
* excele aktarımda iki tarih arası yerine gün aralığı değiştirmesi
    text_change lv_txt3  lv_txt3  &3.
*    text_change lv_text  lv_txt3  &3.
* excele aktarımda iki tarih arası yerine gün aralığı değiştirmesi
  END-OF-DEFINITION  .


*{   ->>> Added by Prodea Sefa Taşkent - 03.10.2019 15:39:48
  no_out 'GSBER'.
*}    <<<- End of  Added - 03.10.2019 15:39:48

  vadeler r_ivb01 '+' 'IVB01' .
  vadeler r_ivb02 '+' 'IVB02' .
  vadeler r_ivb03 '+' 'IVB03' .
  vadeler r_ivb04 '+' 'IVB04' .
  vadeler r_ivb05 '+' 'IVB05' .
  vadeler r_ivb06 '+' 'IVB06' .
*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:42:59
  vadeler r_ivb07 '+' 'IVB07' .
  vadeler r_ivb08 '+' 'IVB08' .
*  }    <<<- End of  Added - 07.10.2019 10:42:59

  vadeler r_gvb01 '-' 'GVB01' .
  vadeler r_gvb02 '-' 'GVB02' .
  vadeler r_gvb03 '-' 'GVB03' .
  vadeler r_gvb04 '-' 'GVB04' .
  vadeler r_gvb05 '-' 'GVB05' .
  vadeler r_gvb06 '-' 'GVB06' .
*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:42:44
  vadeler r_gvb07 '-' 'GVB07' .
  vadeler r_gvb08 '-' 'GVB08' .
*  }    <<<- End of  Added - 07.10.2019 10:42:44




  CLEAR: lv_txt3,lv_text.

  IF rb_gun = 'X'.
    i_ilk = p_keydt - r_gvb99-low.
    CONCATENATE '-' i_ilk '>' INTO lv_txt3.
  ELSEIF rb_trh = 'X'.
    WRITE r_gvb99-low  TO lv_txt1.
*    CONCATENATE '*'  lv_txt1 INTO lv_text SEPARATED BY '-'.
*{   ->>> Added by Prodea Sefa Taşkent - 03.10.2019 12:18:56
    lv_text = lv_txt1.
    CONCATENATE lv_txt3 '( * -' lv_text(5) ' )' INTO lv_txt3.

*    }    <<<- End of  Added - 03.10.2019 12:18:56


  ENDIF.

*  WRITE r_gvb99-low  TO lv_txt1.
*  CONCATENATE '*'  lv_txt1 INTO lv_text SEPARATED BY '-'.
*  i_ilk = p_keydt - r_gvb99-low.
*  CONCATENATE '-' i_ilk '>' INTO lv_txt3."burcua
*  CONCATENATE lv_txt3 '(' lv_text ')' INTO lv_txt3."burcua
*< excele aktarımda iki tarih arası yerine gün aralığı değiştirmesi
  text_change lv_txt3  lv_txt3  'GVB99'.
* excele aktarımda iki tarih arası yerine gün aralığı değiştirmesi>


  CLEAR: lv_txt3,lv_text.

  IF rb_gun = 'X'.
    i_ilk = r_ivb99-low - p_keydt.
    CONCATENATE '+' i_ilk '>' INTO lv_txt3.
  ELSEIF rb_trh = 'X'.
    WRITE r_ivb99-low  TO lv_txt1.

*{   ->>> Added by Prodea Sefa Taşkent - 03.10.2019 12:18:34
    CONCATENATE lv_txt1 ' -*' INTO lv_text SEPARATED BY '-'.
    CONCATENATE lv_txt3 '(  ' lv_text(5) ' - * )' INTO lv_txt3.

*    }    <<<- End of  Added - 03.10.2019 12:18:34


*    CONCATENATE lv_txt1 ' *' INTO lv_text SEPARATED BY '-'.
*    CONCATENATE lv_txt3 '(' lv_text(5) ')' INTO lv_txt3.
  ENDIF.
  DATA : lv_datum(40) TYPE c.
  DATA : lv_datum1(40) TYPE c.
  DATA : lv_datum2(40) TYPE c.
  DATA : lv_datum3(40) TYPE c.
  DATA : lv_datum4(40) TYPE c.
  DATA : lv_datum5(40) TYPE c.
  DATA : lv_datum6(40) TYPE c.
  DATA : lv_datum7(40) TYPE c.
*  LOOP AT ct_fcat INTO gs_fcat.

*    CASE gs_fcat-fieldname.
*      WHEN 'IVB01'.
*        lv_datum = gs_fcat-seltext_l.
*        SPLIT lv_datum AT '.' INTO lv_datum1 lv_datum2 lv_datum3
*lv_datum4 lv_datum5.
*        SPLIT lv_datum3 AT '-' INTO lv_datum6 lv_datum7.
*
*        CONCATENATE  lv_datum1 lv_datum2  INTO gs_fcat-seltext_l
*SEPARATED BY '.'.
*        CONCATENATE lv_datum7 lv_datum4 INTO gs_fcat-seltext_l
*SEPARATED BY '.'.
*    ENDCASE.
*
*  ENDLOOP.

*  WRITE r_ivb99-low  TO lv_txt1.
*  CONCATENATE lv_txt1 ' *' INTO lv_text SEPARATED BY '-'.
*  i_ilk = r_ivb99-low - p_keydt.
*  CONCATENATE '+' i_ilk '>' INTO lv_txt3."burcua
*< excele aktarımda iki tarih arası yerine gün aralığı değiştirmesi
*  CONCATENATE lv_txt3 '(' lv_text ')' INTO lv_txt3."burcua
  text_change lv_txt3  lv_txt3  'IVB99'.
* excele aktarımda iki tarih arası yerine gün aralığı değiştirmesi>

  do_sum 'BAKIY'.
  do_sum 'GVB99'.
  do_sum 'GVB01'.
  do_sum 'GVB02'.
  do_sum 'GVB03'.
  do_sum 'GVB04'.
  do_sum 'GVB05'.
  do_sum 'GVB06'.
*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:50:34
  do_sum 'GVB07'.
  do_sum 'GVB08'.
*  }    <<<- End of  Added - 07.10.2019 10:50:34
  do_sum 'GVBTOP'.
  do_sum 'IVB01'.
  do_sum 'IVB02'.
  do_sum 'IVB03'.
  do_sum 'IVB04'.
  do_sum 'IVB05'.
  do_sum 'IVB06'.
*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:50:46
  do_sum 'IVB07'.
  do_sum 'IVB08'.
*  }    <<<- End of  Added - 07.10.2019 10:50:46
  do_sum 'IVB99'.
  do_sum 'IVBTOP'.

  key_color 'HKONT'.
  key_color 'TXT50'.
  key_color 'KUNNR'.
  key_color 'NAME1'.

  DATA: lv_tar TYPE char10.

  CLEAR: ls_fcat, lv_tar.

  CONCATENATE p_keydt+6(2)
              p_keydt+4(2)
              p_keydt(4)
         INTO lv_tar
      SEPARATED BY '.'.

*{   ->>> Added by Prodea Sefa Taşkent - 03.10.2019 12:17:58
  CONCATENATE 'Bakiye' '(' lv_tar+0(5) ')'
         INTO ls_fcat-seltext_m.
*}    <<<- End of  Added - 03.10.2019 12:17:58
*  CONCATENATE 'Bakiye' '(' lv_tar ')'
*         INTO ls_fcat-seltext_m.

  ls_fcat-seltext_l    = ls_fcat-seltext_m.
  ls_fcat-seltext_s    = ls_fcat-seltext_m.
  ls_fcat-reptext_ddic = ls_fcat-seltext_m.
  ls_fcat-outputlen    = 25.
  ls_fcat-ddictxt      = 'M'.
  MODIFY ct_fcat FROM ls_fcat
         TRANSPORTING seltext_s seltext_m seltext_l reptext_ddic
outputlen
         WHERE fieldname = 'BAKIY'.


  ls_fcat-hotspot = 'X'.
  MODIFY ct_fcat FROM ls_fcat
         TRANSPORTING hotspot
         WHERE fieldname = 'KUNNR'.

ENDFORM.                    " FCAT_FILL
*----------------------------------------------------------------------*
FORM baslik_olustur.
  DATA : lv_ilktr TYPE char10,
         lv_sontr TYPE char10,
         ls_adrc LIKE adrc.

  CLEAR: lt_header, lt_header[].
  lt_header-typ = 'H'.
  IF rb_mus = 'X'.
    lt_header-info = 'Müşteri Yaşlandırma Raporu'.
  ELSE.
    lt_header-info = 'Satıcı Yaşlandırma Raporu'.
  ENDIF.
  APPEND lt_header.

  lt_header-typ = 'S'.
  lt_header-key = 'Şirket Kodu : '.

  SELECT SINGLE * FROM adrc  INTO ls_adrc WHERE addrnumber = t001-adrnr.
*  concatenate t001-bukrs ' - ' t001-butxt into lt_header-info.

  CONCATENATE t001-bukrs ' - ' ls_adrc-name1 INTO lt_header-info.
  APPEND lt_header.

  CLEAR lt_header.
  lt_header-typ = 'S'.
  lt_header-key = 'Anahtar Tarih : '.
  WRITE p_keydt TO lt_header-info.
  APPEND lt_header.

  CLEAR lt_header.
  lt_header-typ = 'S'.
  lt_header-key = 'Rapor Para Birimi : '.
  IF rb_try = 'X'.
    lt_header-info = 'TRY'.
  ELSE.
    lt_header-info = 'EUR'.
  ENDIF.
  APPEND lt_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header[].

ENDFORM.                    "baslik_olustur
*----------------------------------------------------------------------*
FORM f01_user_command USING r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield. "#EC CALLED

  IF r_ucomm ='&IC1'.
    READ TABLE tb_list INDEX rs_selfield-tabindex.
    IF sy-subrc = 0.
      SET PARAMETER ID 'BUK' FIELD p_bukrs.
      IF rb_mus = 'X'.
        SET PARAMETER ID 'KUN' FIELD tb_list-kunnr.
        SUBMIT rfitemar AND RETURN
                WITH pa_stida     = p_keydt
                WITH x_opsel      = 'X'
                WITH x_shbv       = ''.
      ELSE.
        SET PARAMETER ID 'LIF' FIELD tb_list-kunnr.
        SUBMIT rfitemap AND RETURN
                WITH pa_stida     = p_keydt
                WITH x_opsel      = 'X'
                WITH x_shbv       = ''.
      ENDIF.
    ENDIF.
  ELSEIF r_ucomm = '&F03'
      OR r_ucomm = '&F15'
      OR r_ucomm = '&F12'.
    SET SCREEN 0.
  ENDIF.

ENDFORM.                    "f01_user_command
*----------------------------------------------------------------------*
FORM f01_set_status USING rt_extab TYPE slis_t_extab .
  SET PF-STATUS 'BBBB'.
ENDFORM.                    "F01_SET_STATUS
*----------------------------------------------------------------------*
FORM modify_screen .
  LOOP AT SCREEN.

    IF rb_mus = 'X'.
      IF screen-name CS 'S_LIFNR'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      CLEAR : s_lifnr[],s_lifnr.
    ELSE.
      IF screen-name CS 'S_KUNNR'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      CLEAR : s_kunnr[],s_kunnr.
    ENDIF.

    IF rb_gun = 'X'.
      CASE screen-group1.
        WHEN 'GUN'.
          screen-invisible = 0.
          screen-active = 1.
          MODIFY SCREEN.
      ENDCASE.
    ELSEIF rb_trh = 'X'.
      CASE screen-group1.
        WHEN 'TRH'.
          screen-invisible = 0.
          screen-active = 1.
          MODIFY SCREEN.
      ENDCASE.
    ENDIF.


*    IF gv_sistem_yon EQ 'X'.
*      IF ( screen-name CS 'P_BUKRS').
*        screen-active = 1.
*        screen-input  = 1.
*        MODIFY SCREEN.
*      ENDIF.
*    ELSE.
*      IF ( screen-name CS 'P_BUKRS').
*        screen-active = 1.
*        screen-input  = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
  ENDLOOP.
ENDFORM.                    " modify_screen
*----------------------------------------------------------------------*
FORM build_borc_alac_tables .

  DATA : temp LIKE tb_bsid-dmbtr.
  DATA : convers(10) TYPE i.
  DATA : cons(10) TYPE c.

  CLEAR: temp , convers , cons.

  CONCATENATE sy-datum+6(2) sy-datum+4(2)  sy-datum+0(4)
  INTO cons.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = cons
    IMPORTING
      output = convers.


  LOOP AT tb_bsid.
    IF tb_bsid-zfbdt IS INITIAL.
      tb_bsid-zfbdt = tb_bsid-budat.
    ENDIF.
    tb_bsid-zfbdt = tb_bsid-zfbdt + tb_bsid-zbd1t.

    IF tb_bsid-shkzg = 'H'.
      CLEAR tb_alac.
      MOVE tb_bsid-kunnr TO tb_alac-alan.
      MOVE tb_bsid-kunnr TO tb_alac-kunnr.
      MOVE tb_bsid-belnr TO tb_alac-belnr.
      MOVE tb_bsid-budat TO tb_alac-budat.
      MOVE tb_bsid-zfbdt TO tb_alac-zfbdt.
      MOVE tb_bsid-xblnr TO tb_alac-xblnr.
      MOVE tb_bsid-gsber TO tb_alac-gsber.
      MOVE tb_bsid-waers TO tb_alac-waers.
      IF rb_try = 'X'.
        MOVE tb_bsid-dmbtr TO tb_alac-dmbtr.
        MOVE tb_bsid-dmbtr TO tb_alac-kalan.
      ELSEIF rb_bpb EQ 'X'.
        IF tb_bsid-waers EQ 'TRY'.
          MOVE tb_bsid-dmbtr TO tb_alac-dmbtr.
          MOVE tb_bsid-dmbtr TO tb_alac-kalan.
        ELSEIF tb_bsid-waers EQ 'USD'.
          MOVE tb_bsid-wrbtr TO tb_alac-dmbtr.
          MOVE tb_bsid-wrbtr TO tb_alac-kalan.
        ELSE.
          MOVE tb_bsid-dmbe2 TO tb_alac-dmbtr.
          MOVE tb_bsid-dmbe2 TO tb_alac-kalan.
        ENDIF.
*      ELSEIF rb_date EQ 'X' AND rb_eur EQ 'X'.
*        SELECT SINGLE  * FROM tcurr WHERE kurst = 'S'
*      AND fcurr = 'EUR' AND tcurr = 'TRY' AND
*          gdatu EQ convers.
*        IF sy-subrc = 0.
*          temp = tb_bsid-dmbtr / tcurr-ukurs.
*          MOVE temp TO tb_alac-dmbtr.
*          MOVE temp TO tb_alac-kalan.
*        ELSE.
*          MESSAGE i003(zfi).
*          EXIT.
*        ENDIF.
      ELSE.
        MOVE tb_bsid-dmbe2 TO tb_alac-dmbtr.
        MOVE tb_bsid-dmbe2 TO tb_alac-kalan.
      ENDIF.
      APPEND tb_alac.
    ELSE .
      CLEAR tb_borc.
      MOVE tb_bsid-kunnr TO tb_borc-alan.
      MOVE tb_bsid-kunnr TO tb_borc-kunnr.
      MOVE tb_bsid-belnr TO tb_borc-belnr.
      MOVE tb_bsid-budat TO tb_borc-budat.
      MOVE tb_bsid-zfbdt TO tb_borc-zfbdt.
      MOVE tb_bsid-xblnr TO tb_borc-xblnr.
      MOVE tb_bsid-gsber TO tb_borc-gsber.
      MOVE tb_bsid-waers TO tb_borc-waers.
      IF rb_try = 'X'.
        MOVE tb_bsid-dmbtr TO tb_borc-dmbtr.
        MOVE tb_bsid-dmbtr TO tb_borc-kalan.
      ELSEIF rb_bpb EQ 'X'.
        IF tb_bsid-waers EQ 'TRY'.
          MOVE tb_bsid-dmbtr TO tb_borc-dmbtr.
          MOVE tb_bsid-dmbtr TO tb_borc-kalan.
        ELSEIF tb_bsid-waers EQ 'USD'.
          MOVE tb_bsid-wrbtr TO tb_borc-dmbtr.
          MOVE tb_bsid-wrbtr TO tb_borc-kalan.
        ELSE.
          MOVE tb_bsid-dmbe2 TO tb_borc-dmbtr.
          MOVE tb_bsid-dmbe2 TO tb_borc-kalan.
        ENDIF.
      ELSEIF rb_date EQ 'X'.
        SELECT SINGLE * FROM tcurr WHERE kurst = 'S'
      AND fcurr = 'EUR' AND tcurr = 'TRY' AND
          gdatu EQ convers.
        IF sy-subrc EQ 0 .
          temp = tb_bsid-dmbtr / tcurr-ukurs.
          MOVE temp TO tb_borc-dmbtr.
          MOVE temp TO tb_borc-kalan.
        ELSE.
          MESSAGE i003(zfi).
          EXIT.
        ENDIF.



      ELSE.
        MOVE tb_bsid-dmbe2 TO tb_borc-dmbtr.
        MOVE tb_bsid-dmbe2 TO tb_borc-kalan.
      ENDIF.
      APPEND tb_borc.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " build_borc_alac_tables
*----------------------------------------------------------------------*
FORM fill_ranges .
  REFRESH : r_gvb99, r_gvb01, r_gvb02, r_gvb03,
            r_gvb04, r_gvb05, r_gvb06 ,r_gvb07,
            r_ivb01, r_ivb02, r_ivb03, r_ivb04,
            r_ivb05, r_ivb06, r_ivb07, r_ivb99.
*-geçmiş vadeliler

  r_gvb01-sign   = 'I'.
  r_gvb01-option = 'BT'.
  r_gvb01-low   = p_keydt - p_gcm1.
  r_gvb01-high   = p_keydt - 1.
  APPEND r_gvb01.

  r_gvb02-sign   = 'I'.
  r_gvb02-option = 'BT'.
  r_gvb02-low    = p_keydt - p_gcm2.
  r_gvb02-high   = p_keydt - p_gcm1 - 1.
  APPEND r_gvb02.

  r_gvb03-sign   = 'I'.
  r_gvb03-option = 'BT'.
  r_gvb03-low    = p_keydt - p_gcm3.
  r_gvb03-high   = p_keydt - p_gcm2 - 1.
  APPEND r_gvb03.

  r_gvb04-sign   = 'I'.
  r_gvb04-option = 'BT'.
  r_gvb04-low    = p_keydt - p_gcm4.
  r_gvb04-high  = p_keydt - p_gcm3 - 1.
  APPEND r_gvb04.

  r_gvb05-sign   = 'I'.
  r_gvb05-option = 'BT'.
  r_gvb05-low    = p_keydt - p_gcm5.
  r_gvb05-high   = p_keydt - p_gcm4 - 1.
  APPEND r_gvb05.

  r_gvb06-sign   = 'I'.
  r_gvb06-option = 'BT'.
  r_gvb06-low    = p_keydt - p_gcm6.
  r_gvb06-high   = p_keydt - p_gcm5 - 1.
  APPEND r_gvb06.

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:19:45
  r_gvb07-sign   = 'I'.
  r_gvb07-option = 'BT'.
  r_gvb07-low    = p_keydt - p_gcm7.
  r_gvb07-high    = p_keydt - p_gcm6 - 1.
  APPEND r_gvb07.

  r_gvb08-sign   = 'I'.
  r_gvb08-option = 'BT'.
  r_gvb08-low    = p_keydt - p_gcm8.
  r_gvb08-high    = p_keydt - p_gcm7 - 1.
  APPEND r_gvb08.

  r_gvb99-sign   = 'I'.
  r_gvb99-option = 'LE'.
  r_gvb99-low    = p_keydt - p_gcm8 - 1 .
  APPEND r_gvb99.
*}    <<<- End of  Added - 07.10.2019 10:19:45

*{   ->>> Commented by Prodea Sefa Taşkent - 07.10.2019 10:19:23
*  r_gvb99-sign   = 'I'.
*  r_gvb99-option = 'LE'.
*  r_gvb99-low    = p_keydt - p_gcm6 - 1.
*  APPEND r_gvb99.
*}    <<<- End of  Commented - 07.10.2019 10:19:23

*-ileri vadeliler
  r_ivb01-sign   = 'I'.
  r_ivb01-option = 'BT'.
  r_ivb01-low    = p_keydt.
  r_ivb01-high   = p_keydt + p_ilr1.
  APPEND r_ivb01.

  r_ivb02-sign   = 'I'.
  r_ivb02-option = 'BT'.
  r_ivb02-low    = p_keydt + p_ilr1 + 1.
  r_ivb02-high   = p_keydt + p_ilr2.
  APPEND r_ivb02.

  r_ivb03-sign   = 'I'.
  r_ivb03-option = 'BT'.
  r_ivb03-low    = p_keydt + p_ilr2 + 1.
  r_ivb03-high   = p_keydt + p_ilr3.
  APPEND r_ivb03.

  r_ivb04-sign   = 'I'.
  r_ivb04-option = 'BT'.
  r_ivb04-low   = p_keydt + p_ilr3 + 1.
  r_ivb04-high   = p_keydt + p_ilr4.
  APPEND r_ivb04.

  r_ivb05-sign   = 'I'.
  r_ivb05-option = 'BT'.
  r_ivb05-low    = p_keydt + p_ilr4 + 1.
  r_ivb05-high   = p_keydt + p_ilr5.
  APPEND r_ivb05.

  r_ivb06-sign   = 'I'.
  r_ivb06-option = 'BT'.
  r_ivb06-low    = p_keydt + p_ilr5 + 1.
  r_ivb06-high   = p_keydt + p_ilr6.
  APPEND r_ivb06.

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:21:59

  r_ivb07-sign   = 'I'.
  r_ivb07-option = 'BT'.
  r_ivb07-low    = p_keydt + p_ilr6 + 1.
  r_ivb07-high   = p_keydt + p_ilr7.
  APPEND r_ivb07.

  r_ivb08-sign   = 'I'.
  r_ivb08-option = 'BT'.
  r_ivb08-low    = p_keydt + p_ilr7 + 1.
  r_ivb08-high   = p_keydt + p_ilr8.
  APPEND r_ivb08.

  r_ivb99-sign   = 'I'.
  r_ivb99-option = 'GE'.
  r_ivb99-low    = p_keydt + p_ilr8 + 1.
  APPEND r_ivb99.

*  }    <<<- End of  Added - 07.10.2019 10:21:59

*  r_ivb99-sign   = 'I'.
*  r_ivb99-option = 'GE'.
*  r_ivb99-low    = p_keydt + p_ilr6 + 1.
*  APPEND r_ivb99.

*{   ->>> Commented by Prodea Sefa Taşkent - 07.10.2019 10:21:49
*  r_ivb99-sign   = 'I'.
*  r_ivb99-option = 'GE'.
*  r_ivb99-low    = p_keydt + p_ilr6 + 1.
*  APPEND r_ivb99.
*}    <<<- End of  Commented - 07.10.2019 10:21:49


ENDFORM.                    " fill_ranges
*&---------------------------------------------------------------------*
*&      Form  SELECT_VENDORS_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_vendors_2 .

  SELECT SINGLE * FROM t001 WHERE bukrs = p_bukrs.

  SELECT lfa1~lifnr AS kunnr lfa1~name1 lfb1~akont
           FROM lfb1 INNER JOIN lfa1
             ON lfb1~lifnr = lfa1~lifnr
        APPENDING CORRESPONDING FIELDS OF TABLE tb_knb2
        WHERE lfb1~bukrs = p_bukrs
          AND lfb1~akont IN s_hkont
          AND lfa1~lifnr IN s_lifnr
          AND lfa1~regio IN s_regio.
ENDFORM.                    " SELECT_VENDORS_2
*&---------------------------------------------------------------------*
*&      Form  SELECT_VENDORS_OPEN_ITEMS_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_vendors_open_items_2 .
  LOOP AT tb_knb2.
    FREE  : tb_bsid2, tb_bsad2.

    SELECT bsik~lifnr AS kunnr
           bsik~belnr bsik~xblnr bsik~budat bsik~shkzg bsik~zfbdt
           bsik~zbd1t bsik~dmbtr bsik~dmbe2 bsik~umskz
           APPENDING CORRESPONDING FIELDS OF TABLE tb_bsid2
         FROM bsik INNER JOIN bkpf ON bkpf~bukrs = bsik~bukrs
                              AND bkpf~belnr = bsik~belnr
                              AND bkpf~gjahr = bsik~gjahr
         WHERE bsik~bukrs =  p_bukrs
           AND bsik~lifnr =  tb_knb2-kunnr
           AND bsik~budat LE p_keydt
           AND bsik~umskz =  space
           AND bkpf~stblg =  space.

    IF p_keydt < sy-datum.
      SELECT bsak~lifnr AS kunnr
             bsak~belnr bsak~xblnr bsak~budat bsak~shkzg bsak~zfbdt
             bsak~zbd1t bsak~dmbtr bsak~dmbe2 bsak~umskz
           APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad2
           FROM bsak INNER JOIN bkpf ON bkpf~bukrs = bsak~bukrs
                                AND bkpf~belnr = bsak~belnr
                                AND bkpf~gjahr = bsak~gjahr
           WHERE bsak~bukrs =  p_bukrs
             AND bsak~lifnr =  tb_knb2-kunnr
             AND bsak~budat LE p_keydt
             AND bsak~augdt GT p_keydt
             AND bsak~umskz =  space
             AND bkpf~stblg =  space.

      APPEND LINES OF tb_bsad2 TO tb_bsid2.
    ENDIF.

    IF tb_bsid2[] IS INITIAL.
      DELETE tb_knb2.
      CONTINUE.
    ENDIF.
    PERFORM build_borc_alac_tables_2.
  ENDLOOP.
ENDFORM.                    " SELECT_VENDORS_OPEN_ITEMS_2
*&---------------------------------------------------------------------*
*&      Form  BUILD_BORC_ALAC_TABLES_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_borc_alac_tables_2 .
  LOOP AT tb_bsid2.
    IF tb_bsid2-zfbdt IS INITIAL.
      tb_bsid2-zfbdt = tb_bsid2-budat.
    ENDIF.
    tb_bsid2-zfbdt = tb_bsid2-zfbdt + tb_bsid2-zbd1t.

    IF tb_bsid2-shkzg = 'H'.
      CLEAR tb_alac2.
      READ TABLE gt_kna2 WITH KEY lifnr = tb_bsid2-kunnr.
      IF sy-subrc EQ 0.
        MOVE tb_bsid2-kunnr TO tb_alac2-kunnr.
        MOVE gt_kna2-kunnr  TO tb_alac2-alan.
      ELSE.
        MOVE tb_bsid2-kunnr TO tb_alac2-kunnr.
      ENDIF.
      MOVE tb_bsid2-belnr TO tb_alac2-belnr.
      MOVE tb_bsid2-budat TO tb_alac2-budat.
      MOVE tb_bsid2-zfbdt TO tb_alac2-zfbdt.
      MOVE tb_bsid2-xblnr TO tb_alac2-xblnr.
      MOVE tb_bsid2-gsber TO tb_alac2-gsber.
      IF rb_try = 'X'.
        MOVE tb_bsid2-dmbtr TO tb_alac2-dmbtr.
        MOVE tb_bsid2-dmbtr TO tb_alac2-kalan.
      ELSE.
        MOVE tb_bsid2-dmbe2 TO tb_alac2-dmbtr.
        MOVE tb_bsid2-dmbe2 TO tb_alac2-kalan.
      ENDIF.
      APPEND tb_alac2.
    ELSE .
      CLEAR tb_borc2.
      READ TABLE gt_kna2 WITH KEY lifnr = tb_bsid2-kunnr.
      IF sy-subrc EQ 0.
        MOVE tb_bsid2-kunnr TO tb_borc2-kunnr.
        MOVE gt_kna2-kunnr  TO tb_borc2-alan.
      ELSE.
        MOVE tb_bsid2-kunnr TO tb_borc2-kunnr.
      ENDIF.
      MOVE tb_bsid2-kunnr TO tb_borc2-kunnr.
      MOVE tb_bsid2-belnr TO tb_borc2-belnr.
      MOVE tb_bsid2-budat TO tb_borc2-budat.
      MOVE tb_bsid2-zfbdt TO tb_borc2-zfbdt.
      MOVE tb_bsid2-xblnr TO tb_borc2-xblnr.
      MOVE tb_bsid2-gsber TO tb_borc2-gsber.
      IF rb_try = 'X'.
        MOVE tb_bsid2-dmbtr TO tb_borc2-dmbtr.
        MOVE tb_bsid2-dmbtr TO tb_borc2-kalan.
      ELSE.
        MOVE tb_bsid2-dmbe2 TO tb_borc2-dmbtr.
        MOVE tb_bsid2-dmbe2 TO tb_borc2-kalan.
      ENDIF.
      APPEND tb_borc2.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " BUILD_BORC_ALAC_TABLES_2
*&---------------------------------------------------------------------*
*&      Form  SELECT_CUSTOMERS_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_customers_2 .
  SELECT SINGLE * FROM t001 WHERE bukrs = p_bukrs.

  SELECT kna1~kunnr AS kunnr kna1~name1 knb1~akont
           FROM knb1 INNER JOIN kna1
             ON knb1~kunnr = kna1~kunnr
        INTO CORRESPONDING FIELDS OF TABLE tb_knb2
        WHERE kna1~regio IN s_regio
          AND knb1~bukrs = p_bukrs
          AND knb1~akont IN s_hkont
          AND kna1~kunnr IN s_kunnr.
ENDFORM.                    " SELECT_CUSTOMERS_2
*&---------------------------------------------------------------------*
*&      Form  SELECT_CUSTOMERS_OPEN_ITEMS_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_customers_open_items_2 .
  LOOP AT tb_knb2.
    FREE  : tb_bsid2, tb_bsad2.

    IF s_gsber IS INITIAL.
      SELECT bsid~kunnr bsid~belnr bsid~xblnr bsid~budat bsid~shkzg
           bsid~zfbdt bsid~zbd1t bsid~dmbtr bsid~dmbe2 bsid~umskz
           bsid~gsber
           INTO CORRESPONDING FIELDS OF TABLE tb_bsid2
         FROM bsid INNER JOIN bkpf ON bkpf~bukrs = bsid~bukrs
                                  AND bkpf~belnr = bsid~belnr
                                  AND bkpf~gjahr = bsid~gjahr
         WHERE bsid~bukrs =  p_bukrs
           AND bsid~kunnr =  tb_knb2-kunnr
           AND bsid~budat LE p_keydt
           AND bsid~umskz =  space
           AND bkpf~stblg =  space.
    ELSE.
      SELECT bsid~kunnr bsid~belnr bsid~xblnr bsid~budat bsid~shkzg
           bsid~zfbdt bsid~zbd1t bsid~dmbtr bsid~dmbe2 bsid~umskz
           bsid~gsber
           INTO CORRESPONDING FIELDS OF TABLE tb_bsid2
         FROM bsid INNER JOIN bkpf ON bkpf~bukrs = bsid~bukrs
                                  AND bkpf~belnr = bsid~belnr
                                  AND bkpf~gjahr = bsid~gjahr
         WHERE bsid~bukrs =  p_bukrs
           AND bsid~gsber = s_gsber
           AND bsid~kunnr =  tb_knb2-kunnr
           AND bsid~budat LE p_keydt
           AND bsid~umskz =  space
           AND bkpf~stblg =  space.
    ENDIF.


    IF tb_knb1-kunnr IS INITIAL.
      IF s_gsber IS INITIAL.
        SELECT bsid~kunnr bsid~belnr bsid~xblnr bsid~budat bsid~shkzg
       bsid~zfbdt bsid~zbd1t bsid~dmbtr bsid~dmbe2 bsid~umskz
          bsid~gsber
APPENDING CORRESPONDING FIELDS OF TABLE tb_bsid2
 FROM bsid INNER JOIN bkpf ON bkpf~bukrs = bsid~bukrs
                          AND bkpf~belnr = bsid~belnr
                          AND bkpf~gjahr = bsid~gjahr
 WHERE bsid~bukrs =  p_bukrs
*         and bsid~kunnr =  tb_knb1-kunnr
   AND bsid~budat LE p_keydt
   AND bsid~umskz =  space
   AND bkpf~stblg =  space
   AND bsid~filkd NE space.
      ELSE.
        SELECT bsid~kunnr bsid~belnr bsid~xblnr bsid~budat bsid~shkzg
       bsid~zfbdt bsid~zbd1t bsid~dmbtr bsid~dmbe2 bsid~umskz
          bsid~gsber
APPENDING CORRESPONDING FIELDS OF TABLE tb_bsid2
 FROM bsid INNER JOIN bkpf ON bkpf~bukrs = bsid~bukrs
                          AND bkpf~belnr = bsid~belnr
                          AND bkpf~gjahr = bsid~gjahr
 WHERE bsid~bukrs =  p_bukrs
   AND bsid~gsber = s_gsber
*         and bsid~kunnr =  tb_knb1-kunnr
   AND bsid~budat LE p_keydt
   AND bsid~umskz =  space
   AND bkpf~stblg =  space
   AND bsid~filkd NE space.
      ENDIF.


    ELSE.

      IF s_gsber IS INITIAL.
        SELECT bsid~kunnr bsid~belnr bsid~xblnr bsid~budat bsid~shkzg
             bsid~zfbdt bsid~zbd1t bsid~dmbtr bsid~dmbe2 bsid~umskz
          bsid~gsber
     APPENDING CORRESPONDING FIELDS OF TABLE tb_bsid2
         FROM bsid INNER JOIN bkpf ON bkpf~bukrs = bsid~bukrs
                                  AND bkpf~belnr = bsid~belnr
                                  AND bkpf~gjahr = bsid~gjahr
         WHERE bsid~bukrs =  p_bukrs
*         and bsid~kunnr =  tb_knb1-kunnr
           AND bsid~budat LE p_keydt
           AND bsid~umskz =  space
           AND bkpf~stblg =  space
           AND bsid~filkd EQ tb_knb2-kunnr.
      ELSE.
        SELECT bsid~kunnr bsid~belnr bsid~xblnr bsid~budat bsid~shkzg
             bsid~zfbdt bsid~zbd1t bsid~dmbtr bsid~dmbe2 bsid~umskz
          bsid~gsber
     APPENDING CORRESPONDING FIELDS OF TABLE tb_bsid2
         FROM bsid INNER JOIN bkpf ON bkpf~bukrs = bsid~bukrs
                                  AND bkpf~belnr = bsid~belnr
                                  AND bkpf~gjahr = bsid~gjahr
         WHERE bsid~bukrs =  p_bukrs
          AND bsid~gsber = s_gsber
*         and bsid~kunnr =  tb_knb1-kunnr
           AND bsid~budat LE p_keydt
           AND bsid~umskz =  space
           AND bkpf~stblg =  space
           AND bsid~filkd EQ tb_knb2-kunnr.
      ENDIF.


    ENDIF.

    IF p_keydt < sy-datum.
      IF s_gsber IS INITIAL.
        SELECT bsad~kunnr bsad~belnr bsad~xblnr bsad~budat bsad~shkzg
       bsad~zfbdt bsad~zbd1t bsad~dmbtr bsad~dmbe2 bsad~umskz
       bsad~gsber
     APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad2
     FROM bsad INNER JOIN bkpf ON bkpf~bukrs = bsad~bukrs
                              AND bkpf~belnr = bsad~belnr
                              AND bkpf~gjahr = bsad~gjahr
     WHERE bsad~bukrs =  p_bukrs
       AND bsad~kunnr =  tb_knb2-kunnr
       AND bsad~budat LE p_keydt
       AND bsad~augdt GT p_keydt
       AND bsad~umskz =  space
       AND bkpf~stblg =  space.
      ELSE.
        SELECT bsad~kunnr bsad~belnr bsad~xblnr bsad~budat bsad~shkzg
       bsad~zfbdt bsad~zbd1t bsad~dmbtr bsad~dmbe2 bsad~umskz
       bsad~gsber
     APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad2
     FROM bsad INNER JOIN bkpf ON bkpf~bukrs = bsad~bukrs
                              AND bkpf~belnr = bsad~belnr
                              AND bkpf~gjahr = bsad~gjahr
     WHERE bsad~bukrs =  p_bukrs
       AND bsad~gsber = s_gsber
       AND bsad~kunnr =  tb_knb2-kunnr
       AND bsad~budat LE p_keydt
       AND bsad~augdt GT p_keydt
       AND bsad~umskz =  space
       AND bkpf~stblg =  space.
      ENDIF.


      IF tb_knb1-kunnr IS INITIAL.
        IF s_gsber IS INITIAL.
          SELECT bsad~kunnr bsad~belnr bsad~xblnr bsad~budat bsad~shkzg
       bsad~zfbdt bsad~zbd1t bsad~dmbtr bsad~dmbe2 bsad~umskz
       bsad~gsber
APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad2
FROM bsad INNER JOIN bkpf ON bkpf~bukrs = bsad~bukrs
                         AND bkpf~belnr = bsad~belnr
                         AND bkpf~gjahr = bsad~gjahr
WHERE bsad~bukrs =  p_bukrs
*             and bsad~kunnr =  tb_knb1-kunnr
  AND bsad~budat LE p_keydt
       AND bsad~augdt GT p_keydt
  AND bsad~umskz =  space
  AND bkpf~stblg =  space
  AND bsad~filkd NE space.
        ELSE.
          SELECT bsad~kunnr bsad~belnr bsad~xblnr bsad~budat bsad~shkzg
       bsad~zfbdt bsad~zbd1t bsad~dmbtr bsad~dmbe2 bsad~umskz
       bsad~gsber
APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad2
FROM bsad INNER JOIN bkpf ON bkpf~bukrs = bsad~bukrs
                         AND bkpf~belnr = bsad~belnr
                         AND bkpf~gjahr = bsad~gjahr
WHERE bsad~bukrs =  p_bukrs
  AND bsad~gsber = s_gsber
*             and bsad~kunnr =  tb_knb1-kunnr
  AND bsad~budat LE p_keydt
       AND bsad~augdt GT p_keydt
  AND bsad~umskz =  space
  AND bkpf~stblg =  space
  AND bsad~filkd NE space.
        ENDIF.


      ELSE.

        IF s_gsber IS INITIAL.
          SELECT bsad~kunnr bsad~belnr bsad~xblnr bsad~budat bsad~shkzg
               bsad~zfbdt bsad~zbd1t bsad~dmbtr bsad~dmbe2 bsad~umskz
               bsad~gsber
             APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad2
             FROM bsad INNER JOIN bkpf ON bkpf~bukrs = bsad~bukrs
                                      AND bkpf~belnr = bsad~belnr
                                      AND bkpf~gjahr = bsad~gjahr
             WHERE bsad~bukrs =  p_bukrs
*             and bsad~kunnr =  tb_knb1-kunnr
               AND bsad~budat LE p_keydt
               AND bsad~augdt GT p_keydt
               AND bsad~umskz =  space
               AND bkpf~stblg =  space
               AND bsad~filkd EQ tb_knb2-kunnr.
        ELSE.
          SELECT bsad~kunnr bsad~belnr bsad~xblnr bsad~budat bsad~shkzg
               bsad~zfbdt bsad~zbd1t bsad~dmbtr bsad~dmbe2 bsad~umskz
               bsad~gsber
             APPENDING CORRESPONDING FIELDS OF TABLE tb_bsad2
             FROM bsad INNER JOIN bkpf ON bkpf~bukrs = bsad~bukrs
                                      AND bkpf~belnr = bsad~belnr
                                      AND bkpf~gjahr = bsad~gjahr
             WHERE bsad~bukrs =  p_bukrs
               AND bsad~gsber = s_gsber
*             and bsad~kunnr =  tb_knb1-kunnr
               AND bsad~budat LE p_keydt
               AND bsad~augdt GT p_keydt
               AND bsad~umskz =  space
               AND bkpf~stblg =  space
               AND bsad~filkd EQ tb_knb2-kunnr.
        ENDIF.

      ENDIF.
      APPEND LINES OF tb_bsad2 TO tb_bsid2.

    ENDIF.

    LOOP AT tb_bsid2 WHERE filkd NE space.
      tb_bsid2-kunnr = tb_bsid2-filkd.
      MODIFY tb_bsid2.
    ENDLOOP.

    LOOP AT tb_bsad2 WHERE filkd NE space.
      tb_bsad2-kunnr = tb_bsad2-filkd.
      MODIFY tb_bsad2.
    ENDLOOP.

    IF tb_bsid2[] IS INITIAL.
      DELETE tb_knb2.
      CONTINUE.
    ENDIF.
    PERFORM build_borc_alac_tables_3.
  ENDLOOP.
ENDFORM.                    " SELECT_CUSTOMERS_OPEN_ITEMS_2
*&---------------------------------------------------------------------*
*&      Form  BUILD_BORC_ALAC_TABLES_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_borc_alac_tables_3 .
  LOOP AT tb_bsid2.
    IF tb_bsid2-zfbdt IS INITIAL.
      tb_bsid2-zfbdt = tb_bsid2-budat.
    ENDIF.
    tb_bsid2-zfbdt = tb_bsid2-zfbdt + tb_bsid2-zbd1t.

    IF tb_bsid2-shkzg = 'H'.
      CLEAR tb_alac2.
      READ TABLE gt_lfa2 WITH KEY kunnr = tb_bsid2-kunnr.
      IF sy-subrc EQ 0.
        MOVE tb_bsid2-kunnr TO tb_alac2-kunnr.
        MOVE gt_lfa2-lifnr TO tb_alac2-alan.
      ELSE.
        MOVE tb_bsid2-kunnr TO tb_alac2-kunnr.
      ENDIF.

      MOVE tb_bsid2-belnr TO tb_alac2-belnr.
      MOVE tb_bsid2-budat TO tb_alac2-budat.
      MOVE tb_bsid2-zfbdt TO tb_alac2-zfbdt.
      MOVE tb_bsid2-xblnr TO tb_alac2-xblnr.
      MOVE tb_bsid2-gsber TO tb_alac2-gsber.
      IF rb_try = 'X'.
        MOVE tb_bsid2-dmbtr TO tb_alac2-dmbtr.
        MOVE tb_bsid2-dmbtr TO tb_alac2-kalan.
      ELSE.
        MOVE tb_bsid2-dmbe2 TO tb_alac2-dmbtr.
        MOVE tb_bsid2-dmbe2 TO tb_alac2-kalan.
      ENDIF.
      APPEND tb_alac2.
    ELSE .
      CLEAR tb_borc2.
      READ TABLE gt_lfa2 WITH KEY kunnr = tb_bsid2-kunnr.
      IF sy-subrc EQ 0.
        MOVE tb_bsid2-kunnr TO tb_borc2-kunnr.
        MOVE gt_lfa2-lifnr TO tb_borc2-alan.
      ELSE.
        MOVE tb_bsid2-kunnr TO tb_borc2-kunnr.
      ENDIF.

      MOVE tb_bsid2-kunnr TO tb_borc2-kunnr.
      MOVE tb_bsid2-belnr TO tb_borc2-belnr.
      MOVE tb_bsid2-budat TO tb_borc2-budat.
      MOVE tb_bsid2-zfbdt TO tb_borc2-zfbdt.
      MOVE tb_bsid2-xblnr TO tb_borc2-xblnr.
      MOVE tb_bsid2-gsber TO tb_borc2-gsber.
      IF rb_try = 'X'.
        MOVE tb_bsid2-dmbtr TO tb_borc2-dmbtr.
        MOVE tb_bsid2-dmbtr TO tb_borc2-kalan.
      ELSE.
        MOVE tb_bsid2-dmbe2 TO tb_borc2-dmbtr.
        MOVE tb_bsid2-dmbe2 TO tb_borc2-kalan.
      ENDIF.
      APPEND tb_borc2.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " BUILD_BORC_ALAC_TABLES_3
*&---------------------------------------------------------------------*
*&      Form  PREPARE_LIST2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_list2 .
  DATA : lv_arti10 LIKE sy-datum,
           wa        LIKE tb_borc.

  LOOP AT tb_alac2.
    MOVE tb_alac2 TO tb_borc2.
    tb_borc2-dmbtr = tb_borc2-dmbtr * -1.
    tb_borc2-kalan = tb_borc2-kalan * -1.
    APPEND tb_borc2.
  ENDLOOP.



  SORT tb_borc2 BY kunnr zfbdt.

  LOOP AT tb_borc2.
    tb_borc2-gun    = p_keydt - tb_borc2-zfbdt.
    tb_borc2-carpim = tb_borc2-kalan * tb_borc2-gun.
    MODIFY tb_borc2.
  ENDLOOP.

  DATA : i_topx   TYPE bapicurr_d, "çarpım
         gcm_topx TYPE bapicurr_d,
         ilr_topx TYPE bapicurr_d.

  SORT tb_borc2 BY kunnr belnr budat.

  LOOP AT tb_borc2.
    wa = tb_borc2.

***    at new kunnr.
    AT NEW gsber.
      CLEAR: tb_list,i_topx,gcm_topx,ilr_topx.
      MOVE-CORRESPONDING wa TO tb_list.
      READ TABLE tb_knb2 WITH KEY kunnr = tb_borc2-kunnr.
      tb_list-name1 =  tb_knb2-name1.
      tb_list-hkont =  tb_knb2-akont.
      tb_list-alan  =  tb_borc2-alan.
      CLEAR skat.
      SELECT SINGLE * FROM skat WHERE spras = sy-langu
                                  AND ktopl = 'ZIGZ'
                                  AND saknr = tb_list-hkont.
      tb_list-txt50 = skat-txt50.
      IF rb_try = 'X'.
        tb_list-waers = 'TRY'.
      ELSE.
        tb_list-waers = 'EUR'.
      ENDIF.
    ENDAT.

***    if tb_borc2-zfbdt in r_gvb01.
***      add tb_borc2-kalan to tb_list-gvb01.
***    elseif tb_borc2-zfbdt in r_gvb02.
***      add tb_borc2-kalan to tb_list-gvb02.
***    elseif tb_borc2-zfbdt in r_gvb03.
***      add tb_borc2-kalan to tb_list-gvb03.
***    elseif tb_borc2-zfbdt in r_gvb04.
***      add tb_borc2-kalan to tb_list-gvb04.
***    elseif tb_borc2-zfbdt in r_gvb99.
***      add tb_borc2-kalan to tb_list-gvb99.
***    elseif tb_borc2-zfbdt in r_ivb01.
***      add tb_borc2-kalan to tb_list-ivb01.
***    elseif tb_borc2-zfbdt in r_ivb02.
***      add tb_borc2-kalan to tb_list-ivb02.
***    elseif tb_borc2-zfbdt in r_ivb03.
***      add tb_borc2-kalan to tb_list-ivb03.
***    elseif tb_borc2-zfbdt in r_ivb04.
***      add tb_borc2-kalan to tb_list-ivb04.
***    elseif tb_borc2-zfbdt in r_ivb99.
***      add tb_borc2-kalan to tb_list-ivb99.
***    else.
***      break-point.  "kontrol için
***    endif.

    IF tb_borc2-zfbdt IN r_gvb01.
      ADD tb_borc2-dmbtr TO tb_list-gvb01.
    ELSEIF tb_borc2-zfbdt IN r_gvb02.
      ADD tb_borc2-dmbtr TO tb_list-gvb02.
    ELSEIF tb_borc2-zfbdt IN r_gvb03.
      ADD tb_borc2-dmbtr TO tb_list-gvb03.
    ELSEIF tb_borc2-zfbdt IN r_gvb04.
      ADD tb_borc2-dmbtr TO tb_list-gvb04.
    ELSEIF tb_borc2-zfbdt IN r_gvb05.
      ADD tb_borc2-dmbtr TO tb_list-gvb05.
    ELSEIF tb_borc2-zfbdt IN r_gvb06.
      ADD tb_borc2-dmbtr TO tb_list-gvb06.

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:36:02
    ELSEIF tb_borc2-zfbdt IN r_gvb07.
      ADD tb_borc2-dmbtr TO tb_list-gvb07.
    ELSEIF tb_borc2-zfbdt IN r_gvb08.
      ADD tb_borc2-dmbtr TO tb_list-gvb08.
*      }    <<<- End of  Added - 07.10.2019 10:36:02

    ELSEIF tb_borc2-zfbdt IN r_gvb99.
      ADD tb_borc2-dmbtr TO tb_list-gvb99.
    ELSEIF tb_borc2-zfbdt IN r_ivb01.
      ADD tb_borc2-dmbtr TO tb_list-ivb01.
    ELSEIF tb_borc2-zfbdt IN r_ivb02.
      ADD tb_borc2-dmbtr TO tb_list-ivb02.
    ELSEIF tb_borc2-zfbdt IN r_ivb03.
      ADD tb_borc2-dmbtr TO tb_list-ivb03.
    ELSEIF tb_borc2-zfbdt IN r_ivb04.
      ADD tb_borc2-dmbtr TO tb_list-ivb04.
    ELSEIF tb_borc2-zfbdt IN r_ivb05.
      ADD tb_borc2-dmbtr TO tb_list-ivb05.
    ELSEIF tb_borc2-zfbdt IN r_ivb06.
      ADD tb_borc2-dmbtr TO tb_list-ivb06.

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:35:37
    ELSEIF tb_borc2-zfbdt IN r_ivb07.
      ADD tb_borc2-dmbtr TO tb_list-ivb07.
    ELSEIF tb_borc2-zfbdt IN r_ivb08.
      ADD tb_borc2-dmbtr TO tb_list-ivb08.
*      }    <<<- End of  Added - 07.10.2019 10:35:37

    ELSEIF tb_borc2-zfbdt IN r_ivb99.
      ADD tb_borc2-dmbtr TO tb_list-ivb99.
    ELSE.
      BREAK-POINT.  "kontrol için
    ENDIF.

    IF tb_borc2-zfbdt < p_keydt. " geçmiş vade
      ADD tb_borc2-dmbtr  TO tb_list-gvbtop.
      ADD tb_borc2-carpim TO gcm_topx.
    ELSE. "ileri vade
      ADD tb_borc2-dmbtr  TO tb_list-ivbtop.
      ADD tb_borc2-carpim TO ilr_topx.
    ENDIF.

    ADD tb_borc2-dmbtr  TO tb_list-bakiy.
    ADD tb_borc2-carpim TO i_topx.

***    at end of kunnr.
    AT END OF gsber.
      tb_list-ortgn = i_topx / tb_list-bakiy .
      IF tb_list-gvbtop NE 0.
        tb_list-gvbgn = gcm_topx / tb_list-gvbtop .
      ENDIF.
      IF tb_list-ivbtop NE 0.
        tb_list-ivbgn = ilr_topx / tb_list-ivbtop .
      ENDIF.
      APPEND tb_list.
    ENDAT.
  ENDLOOP.

  SORT tb_list BY alan hkont kunnr.
ENDFORM.                    " PREPARE_LIST2
*&---------------------------------------------------------------------*
*&      Form  PREPARE_LIST_NO_IS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_list_no_is .
  DATA : lv_arti10 LIKE sy-datum,
         wa        LIKE tb_borc.

  LOOP AT tb_alac.
    MOVE tb_alac TO tb_borc.
    tb_borc-dmbtr = tb_borc-dmbtr * -1.
    tb_borc-kalan = tb_borc-kalan * -1.
    APPEND tb_borc.
  ENDLOOP.


  SORT tb_borc BY kunnr zfbdt.

  LOOP AT tb_borc.
    tb_borc-gun    = p_keydt - tb_borc-zfbdt.
    tb_borc-carpim = tb_borc-kalan * tb_borc-gun.
    MODIFY tb_borc.
  ENDLOOP.

  DATA : i_topx   TYPE bapicurr_d, "çarpım
         gcm_topx TYPE bapicurr_d,
         ilr_topx TYPE bapicurr_d.

  SORT tb_borc BY kunnr belnr budat.

  LOOP AT tb_borc.
    wa = tb_borc.

    AT NEW kunnr.
***   at new gsber.
      CLEAR: tb_list,i_topx,gcm_topx,ilr_topx.
      MOVE-CORRESPONDING wa TO tb_list.
      READ TABLE tb_knb1 WITH KEY kunnr = tb_borc-kunnr.
      tb_list-name1 =  tb_knb1-name1.
      tb_list-hkont =  tb_knb1-akont.
      tb_list-alan  =  tb_borc-alan.

      CLEAR skat.
      SELECT SINGLE * FROM skat WHERE spras = sy-langu
                                  AND ktopl = 'ZIGZ'
                                  AND saknr = tb_list-hkont.
      tb_list-txt50 = skat-txt50.
      IF rb_try = 'X'.
        tb_list-waers = 'TRY'.
      ELSE.
        tb_list-waers = 'EUR'.
      ENDIF.

    ENDAT.

    IF tb_borc-zfbdt IN r_gvb01.
      ADD tb_borc-dmbtr TO tb_list-gvb01.
    ELSEIF tb_borc-zfbdt IN r_gvb02.
      ADD tb_borc-dmbtr TO tb_list-gvb02.
    ELSEIF tb_borc-zfbdt IN r_gvb03.
      ADD tb_borc-dmbtr TO tb_list-gvb03.
    ELSEIF tb_borc-zfbdt IN r_gvb04.
      ADD tb_borc-dmbtr TO tb_list-gvb04.
    ELSEIF tb_borc-zfbdt IN r_gvb05.
      ADD tb_borc-dmbtr TO tb_list-gvb05.
    ELSEIF tb_borc-zfbdt IN r_gvb06.
      ADD tb_borc-dmbtr TO tb_list-gvb06.

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:36:40
    ELSEIF tb_borc-zfbdt IN r_gvb07.
      ADD tb_borc-dmbtr TO tb_list-gvb07.
    ELSEIF tb_borc-zfbdt IN r_gvb08.
      ADD tb_borc-dmbtr TO tb_list-gvb08.
*      }    <<<- End of  Added - 07.10.2019 10:36:40

    ELSEIF tb_borc-zfbdt IN r_gvb99.
      ADD tb_borc-dmbtr TO tb_list-gvb99.
    ELSEIF tb_borc-zfbdt IN r_ivb01.
      ADD tb_borc-dmbtr TO tb_list-ivb01.
    ELSEIF tb_borc-zfbdt IN r_ivb02.
      ADD tb_borc-dmbtr TO tb_list-ivb02.
    ELSEIF tb_borc-zfbdt IN r_ivb03.
      ADD tb_borc-dmbtr TO tb_list-ivb03.
    ELSEIF tb_borc-zfbdt IN r_ivb04.
      ADD tb_borc-dmbtr TO tb_list-ivb04.
    ELSEIF tb_borc-zfbdt IN r_ivb05.
      ADD tb_borc-dmbtr TO tb_list-ivb05.
    ELSEIF tb_borc-zfbdt IN r_ivb06.
      ADD tb_borc-dmbtr TO tb_list-ivb06.

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:36:56
    ELSEIF tb_borc-zfbdt IN r_ivb07.
      ADD tb_borc-dmbtr TO tb_list-ivb07.
    ELSEIF tb_borc-zfbdt IN r_ivb08.
      ADD tb_borc-dmbtr TO tb_list-ivb08.
*      }    <<<- End of  Added - 07.10.2019 10:36:56

    ELSEIF tb_borc-zfbdt IN r_ivb99.
      ADD tb_borc-dmbtr TO tb_list-ivb99.
    ELSE.
      BREAK-POINT.  "kontrol için
    ENDIF.

    IF tb_borc-zfbdt < p_keydt. " geçmiş vade
      ADD tb_borc-dmbtr  TO tb_list-gvbtop.
      ADD tb_borc-carpim TO gcm_topx.
    ELSE. "ileri vade
      ADD tb_borc-dmbtr  TO tb_list-ivbtop.
      ADD tb_borc-carpim TO ilr_topx.
    ENDIF.

***    add tb_borc-kalan  to tb_list-bakiy.
    ADD tb_borc-dmbtr  TO tb_list-bakiy.
    ADD tb_borc-carpim TO i_topx.



    AT END OF kunnr.
***      at end of gsber.
      IF tb_list-bakiy NE 0.
        tb_list-ortgn = i_topx / tb_list-bakiy .
      ENDIF.
      IF tb_list-gvbtop NE 0.
        tb_list-gvbgn = gcm_topx / tb_list-gvbtop .
      ENDIF.
      IF tb_list-ivbtop NE 0.
        tb_list-ivbgn = ilr_topx / tb_list-ivbtop .
      ENDIF.
      APPEND tb_list.
      COLLECT tb_list INTO seats_tab_2.

    ENDAT.

  ENDLOOP.

  tb_list[] = seats_tab_2[].

  SORT tb_list BY hkont kunnr.




ENDFORM.                    " PREPARE_LIST_NO_IS
*&---------------------------------------------------------------------*
*&      Form  PREPARE_LIST_NO_IS_PB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_list_no_is_pb .
  DATA : lv_arti10 LIKE sy-datum,
         wa        LIKE tb_borc.

  LOOP AT tb_alac.
    MOVE tb_alac TO tb_borc.
    tb_borc-dmbtr = tb_borc-dmbtr * -1.
    tb_borc-kalan = tb_borc-kalan * -1.
    APPEND tb_borc.
  ENDLOOP.


  SORT tb_borc BY kunnr zfbdt.

  LOOP AT tb_borc.
    tb_borc-gun    = p_keydt - tb_borc-zfbdt.
    tb_borc-carpim = tb_borc-kalan * tb_borc-gun.
    MODIFY tb_borc.
  ENDLOOP.

  DATA : i_topx   TYPE bapicurr_d, "çarpım
         gcm_topx TYPE bapicurr_d,
         ilr_topx TYPE bapicurr_d.

  SORT tb_borc BY kunnr belnr budat.

  LOOP AT tb_borc.
    wa = tb_borc.

    AT NEW waers.
      CLEAR: tb_list,i_topx,gcm_topx,ilr_topx.
      MOVE-CORRESPONDING wa TO tb_list.
      READ TABLE tb_knb1 WITH KEY kunnr = tb_borc-kunnr.
      tb_list-name1 =  tb_knb1-name1.
      tb_list-hkont =  tb_knb1-akont.
      tb_list-alan  =  tb_borc-alan.

      CLEAR skat.
      SELECT SINGLE * FROM skat WHERE spras = sy-langu
                                  AND ktopl = 'ZIGZ'
                                  AND saknr = tb_list-hkont.
      tb_list-txt50 = skat-txt50.
      tb_list-waers = tb_borc-waers.

    ENDAT.


    IF tb_borc-zfbdt IN r_gvb01.
      ADD tb_borc-dmbtr TO tb_list-gvb01.
    ELSEIF tb_borc-zfbdt IN r_gvb02.
      ADD tb_borc-dmbtr TO tb_list-gvb02.
    ELSEIF tb_borc-zfbdt IN r_gvb03.
      ADD tb_borc-dmbtr TO tb_list-gvb03.
    ELSEIF tb_borc-zfbdt IN r_gvb04.
      ADD tb_borc-dmbtr TO tb_list-gvb04.
    ELSEIF tb_borc-zfbdt IN r_gvb05.
      ADD tb_borc-dmbtr TO tb_list-gvb05.
    ELSEIF tb_borc-zfbdt IN r_gvb06.
      ADD tb_borc-dmbtr TO tb_list-gvb06.
*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:38:10
    ELSEIF tb_borc-zfbdt IN r_gvb07.
      ADD tb_borc-dmbtr TO tb_list-gvb07.
    ELSEIF tb_borc-zfbdt IN r_gvb08.
      ADD tb_borc-dmbtr TO tb_list-gvb08.
*      }    <<<- End of  Added - 07.10.2019 10:38:10
    ELSEIF tb_borc-zfbdt IN r_gvb99.
      ADD tb_borc-dmbtr TO tb_list-gvb99.
    ELSEIF tb_borc-zfbdt IN r_ivb01.
      ADD tb_borc-dmbtr TO tb_list-ivb01.
    ELSEIF tb_borc-zfbdt IN r_ivb02.
      ADD tb_borc-dmbtr TO tb_list-ivb02.
    ELSEIF tb_borc-zfbdt IN r_ivb03.
      ADD tb_borc-dmbtr TO tb_list-ivb03.
    ELSEIF tb_borc-zfbdt IN r_ivb04.
      ADD tb_borc-dmbtr TO tb_list-ivb04.
    ELSEIF tb_borc-zfbdt IN r_ivb05.
      ADD tb_borc-dmbtr TO tb_list-ivb05.
    ELSEIF tb_borc-zfbdt IN r_ivb06.
      ADD tb_borc-dmbtr TO tb_list-ivb06.
*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:37:37
    ELSEIF tb_borc-zfbdt IN r_ivb07.
      ADD tb_borc-dmbtr TO tb_list-ivb07.
    ELSEIF tb_borc-zfbdt IN r_ivb08.
      ADD tb_borc-dmbtr TO tb_list-ivb08.
*      }    <<<- End of  Added - 07.10.2019 10:37:37
    ELSEIF tb_borc-zfbdt IN r_ivb99.
      ADD tb_borc-dmbtr TO tb_list-ivb99.
    ELSE.
      BREAK-POINT.  "kontrol için
    ENDIF.


    IF tb_borc-zfbdt < p_keydt. " geçmiş vade
      ADD tb_borc-dmbtr  TO tb_list-gvbtop.
      ADD tb_borc-carpim TO gcm_topx.
    ELSE. "ileri vade
      ADD tb_borc-dmbtr  TO tb_list-ivbtop.
      ADD tb_borc-carpim TO ilr_topx.
    ENDIF.

****    add tb_borc-kalan  to tb_list-bakiy.
    ADD tb_borc-dmbtr  TO tb_list-bakiy.
    ADD tb_borc-carpim TO i_topx.




    AT END OF waers.
      IF tb_list-bakiy NE 0.
        tb_list-ortgn = i_topx / tb_list-bakiy .
      ENDIF.
      IF tb_list-gvbtop NE 0.
        tb_list-gvbgn = gcm_topx / tb_list-gvbtop .
      ENDIF.
      IF tb_list-ivbtop NE 0.
        tb_list-ivbgn = ilr_topx / tb_list-ivbtop .
      ENDIF.
      APPEND tb_list.
    ENDAT.
    COLLECT tb_list INTO seats_tab.

  ENDLOOP.

  CLEAR tb_list.
  REFRESH tb_list.

  tb_list[] = seats_tab[].

  SORT tb_list BY hkont kunnr.


ENDFORM.                    " PREPARE_LIST_NO_IS_PB
*&---------------------------------------------------------------------*
*&      Form  PREPARE_LIST_BP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_list_bp .
  DATA : lv_arti10 LIKE sy-datum,
       wa        LIKE tb_borc.

  LOOP AT tb_alac.
    MOVE tb_alac TO tb_borc.
    tb_borc-dmbtr = tb_borc-dmbtr * -1.
    tb_borc-kalan = tb_borc-kalan * -1.
    APPEND tb_borc.
  ENDLOOP.


  SORT tb_borc BY kunnr zfbdt.

  LOOP AT tb_borc.
    tb_borc-gun    = p_keydt - tb_borc-zfbdt.
    tb_borc-carpim = tb_borc-kalan * tb_borc-gun.
    CLEAR tb_borc-gsber.
    MODIFY tb_borc.
  ENDLOOP.

  DATA : i_topx   TYPE bapicurr_d, "çarpım
         gcm_topx TYPE bapicurr_d,
         ilr_topx TYPE bapicurr_d.

  SORT tb_borc BY kunnr waers.

  LOOP AT tb_borc.
    wa = tb_borc.

    AT NEW waers.

      CLEAR: tb_list,i_topx,gcm_topx,ilr_topx.
      MOVE-CORRESPONDING wa TO tb_list.
      READ TABLE tb_knb1 WITH KEY kunnr = tb_borc-kunnr.
      tb_list-name1 =  tb_knb1-name1.
      tb_list-hkont =  tb_knb1-akont.
      tb_list-alan  =  tb_borc-alan.

      CLEAR skat.
      SELECT SINGLE * FROM skat WHERE spras = sy-langu
                                  AND ktopl = 'ZIGZ'
                                  AND saknr = tb_list-hkont.
      tb_list-txt50 = skat-txt50.
      tb_list-waers = tb_borc-waers.

    ENDAT.
    IF tb_borc-zfbdt IN r_gvb01.
      ADD tb_borc-dmbtr TO tb_list-gvb01.
    ELSEIF tb_borc-zfbdt IN r_gvb02.
      ADD tb_borc-dmbtr TO tb_list-gvb02.
    ELSEIF tb_borc-zfbdt IN r_gvb03.
      ADD tb_borc-dmbtr TO tb_list-gvb03.
    ELSEIF tb_borc-zfbdt IN r_gvb04.
      ADD tb_borc-dmbtr TO tb_list-gvb04.
    ELSEIF tb_borc-zfbdt IN r_gvb05.
      ADD tb_borc-dmbtr TO tb_list-gvb05.
    ELSEIF tb_borc-zfbdt IN r_gvb06.
      ADD tb_borc-dmbtr TO tb_list-gvb06.

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:43:49
    ELSEIF tb_borc-zfbdt IN r_gvb07.
      ADD tb_borc-dmbtr TO tb_list-gvb07.
    ELSEIF tb_borc-zfbdt IN r_gvb08.
      ADD tb_borc-dmbtr TO tb_list-gvb08.
*      }    <<<- End of  Added - 07.10.2019 10:43:49

    ELSEIF tb_borc-zfbdt IN r_gvb99.
      ADD tb_borc-dmbtr TO tb_list-gvb99.
    ELSEIF tb_borc-zfbdt IN r_ivb01.
      ADD tb_borc-dmbtr TO tb_list-ivb01.
    ELSEIF tb_borc-zfbdt IN r_ivb02.
      ADD tb_borc-dmbtr TO tb_list-ivb02.
    ELSEIF tb_borc-zfbdt IN r_ivb03.
      ADD tb_borc-dmbtr TO tb_list-ivb03.
    ELSEIF tb_borc-zfbdt IN r_ivb04.
      ADD tb_borc-dmbtr TO tb_list-ivb04.
    ELSEIF tb_borc-zfbdt IN r_ivb05.
      ADD tb_borc-dmbtr TO tb_list-ivb05.
    ELSEIF tb_borc-zfbdt IN r_ivb06.
      ADD tb_borc-dmbtr TO tb_list-ivb06.

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:44:14
    ELSEIF tb_borc-zfbdt IN r_ivb07.
      ADD tb_borc-dmbtr TO tb_list-ivb07.
    ELSEIF tb_borc-zfbdt IN r_ivb08.
      ADD tb_borc-dmbtr TO tb_list-ivb08.
*      }    <<<- End of  Added - 07.10.2019 10:44:14

    ELSEIF tb_borc-zfbdt IN r_ivb99.
      ADD tb_borc-dmbtr TO tb_list-ivb99.
    ELSE.
      BREAK-POINT.  "kontrol için
    ENDIF.

    IF tb_borc-zfbdt < p_keydt. " geçmiş vade
      ADD tb_borc-dmbtr  TO tb_list-gvbtop.
      ADD tb_borc-carpim TO gcm_topx.
    ELSE. "ileri vade
      ADD tb_borc-dmbtr  TO tb_list-ivbtop.
      ADD tb_borc-carpim TO ilr_topx.
    ENDIF.

***    add tb_borc-kalan  to tb_list-bakiy.
    ADD tb_borc-dmbtr  TO tb_list-bakiy.
    ADD tb_borc-carpim TO i_topx.




    AT END OF waers.
      IF tb_list-bakiy NE 0.
        tb_list-ortgn = i_topx / tb_list-bakiy .
      ENDIF.
      IF tb_list-gvbtop NE 0.
        tb_list-gvbgn = gcm_topx / tb_list-gvbtop .
      ENDIF.
      IF tb_list-ivbtop NE 0.
        tb_list-ivbgn = ilr_topx / tb_list-ivbtop .
      ENDIF.
      APPEND tb_list.
      COLLECT tb_list INTO seats_tab_3.

    ENDAT.

  ENDLOOP.

  tb_list[] = seats_tab_3[].

  SORT tb_list BY hkont kunnr.

ENDFORM.                    " PREPARE_LIST_BP
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
*  SELECT SINGLE *
*    FROM zmuh_liste_yetki
*    INTO gs_liste_yetki
*   WHERE danisman EQ sy-uname.

*  IF sy-subrc EQ 0.
*    IF gs_liste_yetki-prodea_cb EQ 'X' AND
*       gs_liste_yetki-inera_cb  EQ 'X' AND
*       gs_liste_yetki-fiz_cb    EQ 'X' AND
*       gs_liste_yetki-easy_cb   EQ 'X'.
*      gv_sistem_yon = 'X'.
*      p_bukrs = 'PRO'.
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
*    gv_hata = 'X'.
*    CLEAR p_bukrs.
*  ENDIF.

ENDFORM.                    " YETKI_KONTROL
