*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_016_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data CHANGING cv_error.

  DATA: lv_bas_tar         TYPE sy-datum,
        lv_bit_tar         TYPE sy-datum,
        lv_mes             TYPE char100 ,
        lv_times           TYPE sy-tabix,
        lv_fark_g          TYPE sy-tabix,
        lv_surec_tipi_temp    TYPE zsog_fi_011-surec_tipi,
        lv_surec_tipi_temp_f  TYPE zsog_fi_011-surec_tipi,
        lv_surec_tipi_temp_th TYPE zsog_fi_011-surec_tipi.

  RANGES :lr_surec_tipi_temp FOR zsog_fi_011-surec_tipi.

  DATA: lv_wrbtr TYPE bseg-wrbtr.
  DATA: lt_mwdat TYPE rtax1u15 OCCURS 0 WITH HEADER LINE.
  DATA: lv_mwskz TYPE bseg-mwskz.

  CLEAR: gt_out, gt_out[], gs_out, cv_error,
         gt_011, gt_011[].

  IF r1 = 'X'.

    SELECT * FROM zsog_fi_011
      INTO CORRESPONDING FIELDS OF TABLE gt_011
      WHERE accounting_period_no = gs_009-accounting_period_no
        AND week_identifier      = gs_009-week_identifier
        AND date_from            = gs_009-date_from
        AND date_to              = gs_009-date_to
        AND file_date            = s_tarih-low
        AND surec_tipi           = '10'.
    IF sy-subrc NE 0.
      SELECT file_date
         SUM( gross_sales_refunds ) AS gross_sales_refunds
         SUM( vat ) AS vat
        FROM zsg_t_021
        INTO CORRESPONDING FIELDS OF TABLE gt_out
        WHERE file_date = s_tarih-low
        GROUP BY file_date.
      IF sy-subrc EQ 0.

        LOOP AT gt_out.
          gt_out-accounting_period_no = gs_009-accounting_period_no.
          gt_out-week_identifier      = gs_009-week_identifier.
          gt_out-date_from            = gs_009-date_from.
          gt_out-date_to              = gs_009-date_to.

          gt_out-net_amount = gt_out-gross_sales_refunds - gt_out-vat.

          IF gt_out-net_amount GT 0.
            gt_out-doc_amount = gt_out-net_amount * 2 / 1000.
          ENDIF.

          gt_out-icon = icon_yellow_light.


          MODIFY gt_out.

        ENDLOOP.

      ENDIF.

    ELSE.

      cv_error = 'X'.
      CONCATENATE s_tarih-low+6(2) '.'
                  s_tarih-low+4(2) '.'
                  s_tarih-low+0(4)
                  ' tarihli tahakkuk belgesi oluşturulduğundan'
                  ' tekrar oluşturulamaz.'
             INTO lv_mes.
      MESSAGE lv_mes TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.

    ENDIF.
  ELSEIF r2 = 'X' OR r6 = 'X'.

    CLEAR: lv_surec_tipi_temp, lv_surec_tipi_temp_f , lv_surec_tipi_temp_th,
    lr_surec_tipi_temp, lr_surec_tipi_temp[].

    lr_surec_tipi_temp-sign   = 'I'.
    lr_surec_tipi_temp-option = 'EQ'.

    IF r2 = 'X'.
      lv_surec_tipi_temp    = '10'.
      lv_surec_tipi_temp_th = '15'.
      lv_surec_tipi_temp_f  = '20'.

      lr_surec_tipi_temp-low = '10'.
      APPEND lr_surec_tipi_temp.
      lr_surec_tipi_temp-low = '15'.
      APPEND lr_surec_tipi_temp.
      lr_surec_tipi_temp-low = '20'.
      APPEND lr_surec_tipi_temp.

    ELSEIF r6 = 'X'.

      lv_surec_tipi_temp    = '30'.
      lv_surec_tipi_temp_th = '35'.
      lv_surec_tipi_temp_f  = '40'.

      lr_surec_tipi_temp-low = '30'.
      APPEND lr_surec_tipi_temp.
      lr_surec_tipi_temp-low = '35'.
      APPEND lr_surec_tipi_temp.
      lr_surec_tipi_temp-low = '40'.
      APPEND lr_surec_tipi_temp.

    ENDIF.

    SELECT * FROM zsog_fi_011
      INTO CORRESPONDING FIELDS OF TABLE gt_011
      WHERE accounting_period_no EQ gs_009-accounting_period_no
        AND week_identifier      EQ gs_009-week_identifier
        AND date_from            EQ gs_009-date_from
        AND date_to              EQ gs_009-date_to
        AND surec_tipi           IN lr_surec_tipi_temp.
    IF sy-subrc EQ 0.
      READ TABLE gt_011 WITH KEY surec_tipi = lv_surec_tipi_temp_f.
      IF sy-subrc NE 0.
        READ TABLE gt_011 WITH KEY surec_tipi = lv_surec_tipi_temp_th.
        IF sy-subrc NE 0.
          SELECT file_date gjahr belnr bukrs doc_amount fatura th
            FROM zsog_fi_011
            INTO CORRESPONDING FIELDS OF TABLE gt_out
           WHERE accounting_period_no = gs_009-accounting_period_no
             AND week_identifier      = gs_009-week_identifier
             AND date_from            = gs_009-date_from
             AND date_to              = gs_009-date_to
             AND file_date            = s_tarih-low
             AND surec_tipi           = lv_surec_tipi_temp.
          IF sy-subrc EQ 0.
            LOOP AT gt_out.

              gt_out-accounting_period_no = gs_009-accounting_period_no.
              gt_out-week_identifier      = gs_009-week_identifier.
              gt_out-date_from            = gs_009-date_from.
              gt_out-date_to              = gs_009-date_to.

              gt_out-icon = icon_yellow_light.
              MODIFY gt_out.
            ENDLOOP.
          ENDIF.
        ELSE.
          cv_error = 'X'.
          CONCATENATE gs_009-accounting_period_no ' / '
                      gs_009-week_identifier
                      ' dönem ters hareket belgesi önceden oluşturulduğundan'
                      ' ters kayıt alınamaz.'
                 INTO lv_mes.
          MESSAGE lv_mes TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ELSE.
        cv_error = 'X'.
        CONCATENATE gs_009-accounting_period_no ' / '
                    gs_009-week_identifier
                    ' dönem faturası önceden oluşturulduğundan'
                    ' ters kayıt alınamaz.'
               INTO lv_mes.
        MESSAGE lv_mes TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.

  ELSEIF ra = 'X' OR rc = 'X'.

    CLEAR: lv_surec_tipi_temp, lr_surec_tipi_temp, lr_surec_tipi_temp[].
    lr_surec_tipi_temp-sign   = 'I'.
    lr_surec_tipi_temp-option = 'EQ'.
    IF ra = 'X'.
      lv_surec_tipi_temp = '20'.
      lr_surec_tipi_temp-low = '10'.
      APPEND lr_surec_tipi_temp.
      lr_surec_tipi_temp-low = '15'.
      APPEND lr_surec_tipi_temp.
      lr_surec_tipi_temp-low = '20'.
      APPEND lr_surec_tipi_temp.
    ELSEIF rc = 'X'.
      lv_surec_tipi_temp = '40'.
      lr_surec_tipi_temp-low = '30'.
      APPEND lr_surec_tipi_temp.
      lr_surec_tipi_temp-low = '35'.
      APPEND lr_surec_tipi_temp.
      lr_surec_tipi_temp-low = '40'.
      APPEND lr_surec_tipi_temp.
    ENDIF.

    SELECT * FROM zsog_fi_011
      INTO CORRESPONDING FIELDS OF TABLE gt_011
      WHERE accounting_period_no EQ gs_009-accounting_period_no
        AND week_identifier      EQ gs_009-week_identifier
        AND date_from            EQ gs_009-date_from
        AND date_to              EQ gs_009-date_to
        AND surec_tipi           IN lr_surec_tipi_temp.

    READ TABLE gt_011 WITH KEY surec_tipi = lv_surec_tipi_temp.
    IF sy-subrc EQ 0.
      cv_error = 'X'.
      CONCATENATE gs_009-accounting_period_no ' / '
                  gs_009-week_identifier
                  ' dönem faturası önceden oluşturulduğundan'
                  ' tekrar fatura oluşturulamaz.'
             INTO lv_mes.
      MESSAGE lv_mes TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      READ TABLE gt_011 WITH KEY surec_tipi = gv_surec_tipi.
      IF sy-subrc EQ 0.
        cv_error = 'X'.
        CONCATENATE gs_009-accounting_period_no ' / '
                    gs_009-week_identifier
                    ' dönem ters hareket belgesi önceden oluşturulduğundan'
                    ' tekrar belge oluşturulamaz.'
               INTO lv_mes.
        MESSAGE lv_mes TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ELSE.
        CLEAR: lv_times, lv_bas_tar, lv_bit_tar, gt_out, lv_fark_g.

        gt_out-accounting_period_no = gs_009-accounting_period_no.
        gt_out-week_identifier      = gs_009-week_identifier.
        gt_out-date_from            = gs_009-date_from.
        gt_out-date_to              = gs_009-date_to.

        lv_bas_tar = gs_009-date_from.
        lv_bit_tar = gs_009-date_to .

        lv_fark_g = lv_bit_tar - lv_bas_tar.

        WHILE lv_bas_tar LT lv_bit_tar.
          CLEAR: gt_011.
          READ TABLE gt_011 WITH KEY file_date = lv_bas_tar.
          IF sy-subrc NE 0.
            cv_error = 'X'.
            CONCATENATE lv_bas_tar+6(2) '.'
                        lv_bas_tar+4(2) '.'
                        lv_bas_tar+0(4)
                        ' tarihli tahakkuk belgesi oluşturulmadığından'
                        ' ters hareket belgesi oluşturulamaz.'
                   INTO lv_mes.
            MESSAGE lv_mes TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ELSE.
            gt_out-doc_amount = gt_out-doc_amount + gt_011-doc_amount.
            lv_times = lv_times + 1.
          ENDIF.
          lv_bas_tar = lv_bas_tar + 1 .

        ENDWHILE.

        CHECK cv_error = space.

        IF lv_times = lv_fark_g.

          gt_out-icon = icon_yellow_light.
          APPEND gt_out.

        ELSE.
          cv_error = 'X'.
          MESSAGE 'Belge sayılarında tutarsızlık mevcut.'
             TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

      ENDIF.
    ENDIF.

  ELSEIF rb = 'X' OR rd = 'X'.

    CLEAR: lv_surec_tipi_temp, lr_surec_tipi_temp, lr_surec_tipi_temp[].

    lr_surec_tipi_temp-sign   = 'I'.
    lr_surec_tipi_temp-option = 'EQ'.

    IF rb = 'X'.
      lv_surec_tipi_temp = '15'.

*      lr_surec_tipi_temp-low = '15'.
*      APPEND lr_surec_tipi_temp.
      lr_surec_tipi_temp-low = '20'.
      APPEND lr_surec_tipi_temp.

    ELSEIF rd = 'X'.
      lv_surec_tipi_temp = '35'.

*      lr_surec_tipi_temp-low = '35'.
*      APPEND lr_surec_tipi_temp.
      lr_surec_tipi_temp-low = '40'.
      APPEND lr_surec_tipi_temp.

    ENDIF.

    SELECT * FROM zsog_fi_011
      INTO CORRESPONDING FIELDS OF TABLE gt_011
      WHERE accounting_period_no EQ gs_009-accounting_period_no
        AND week_identifier      EQ gs_009-week_identifier
        AND date_from            EQ gs_009-date_from
        AND date_to              EQ gs_009-date_to
        AND surec_tipi           IN lr_surec_tipi_temp.
    IF sy-subrc NE 0.
      SELECT file_date gjahr belnr bukrs doc_amount fatura th
        FROM zsog_fi_011
        INTO CORRESPONDING FIELDS OF TABLE gt_out
       WHERE accounting_period_no EQ gs_009-accounting_period_no
         AND week_identifier      EQ gs_009-week_identifier
         AND date_from            EQ gs_009-date_from
         AND date_to              EQ gs_009-date_to
         AND surec_tipi           EQ lv_surec_tipi_temp.
      IF sy-subrc EQ 0.
        LOOP AT gt_out.

          gt_out-accounting_period_no = gs_009-accounting_period_no.
          gt_out-week_identifier      = gs_009-week_identifier.
          gt_out-date_from            = gs_009-date_from.
          gt_out-date_to              = gs_009-date_to.

          gt_out-icon = icon_yellow_light.
          MODIFY gt_out.
        ENDLOOP.

      ENDIF.
    ELSE.
      cv_error = 'X'.
      CONCATENATE gs_009-accounting_period_no ' / '
                  gs_009-week_identifier
                  ' dönem faturası oluşturulduğundan'
                  ' ters hareket ters kaydı oluşturulamaz.'
             INTO lv_mes.
      MESSAGE lv_mes TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.



  ELSEIF r3 = 'X'.

    CLEAR: lv_surec_tipi_temp, lr_surec_tipi_temp, lr_surec_tipi_temp[].


    lv_surec_tipi_temp = '15'.

    lr_surec_tipi_temp-sign   = 'I'.
    lr_surec_tipi_temp-option = 'EQ'.

    lr_surec_tipi_temp-low = '10'.
    APPEND lr_surec_tipi_temp.
    lr_surec_tipi_temp-low = '15'.
    APPEND lr_surec_tipi_temp.
    lr_surec_tipi_temp-low = '20'.
    APPEND lr_surec_tipi_temp.


    SELECT * FROM zsog_fi_011
      INTO CORRESPONDING FIELDS OF TABLE gt_011
      WHERE accounting_period_no EQ gs_009-accounting_period_no
        AND week_identifier      EQ gs_009-week_identifier
        AND date_from            EQ gs_009-date_from
        AND date_to              EQ gs_009-date_to
        AND surec_tipi           IN lr_surec_tipi_temp.

    READ TABLE gt_011 WITH KEY surec_tipi = gv_surec_tipi.
    IF sy-subrc EQ 0.
      cv_error = 'X'.
      CONCATENATE gs_009-accounting_period_no ' / '
                  gs_009-week_identifier
                  ' dönem faturası önceden oluşturulduğundan'
                  ' tekrar fatura oluşturulamaz.'
             INTO lv_mes.
      MESSAGE lv_mes TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      READ TABLE gt_011 WITH KEY surec_tipi = lv_surec_tipi_temp.
      IF sy-subrc NE 0.
        cv_error = 'X'.
        CONCATENATE gs_009-accounting_period_no ' / '
                    gs_009-week_identifier
                    ' dönem ters hareket belgesi oluşturulmadığından'
                    ' fatura oluşturulamaz.'
               INTO lv_mes.
        MESSAGE lv_mes TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ELSE.
        CLEAR: lv_times, lv_bas_tar, lv_bit_tar, gt_out, lv_fark_g.

        gt_out-accounting_period_no = gs_009-accounting_period_no.
        gt_out-week_identifier      = gs_009-week_identifier.
        gt_out-date_from            = gs_009-date_from.
        gt_out-date_to              = gs_009-date_to.

        gt_out-net_amount = gt_011-doc_amount.

        CLEAR: lv_wrbtr, lt_mwdat, lt_mwdat[].
        lv_wrbtr = gt_out-net_amount.
        CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
          EXPORTING
            i_bukrs           = gc_bukrs
            i_mwskz           = gs_010-mwskz
            i_waers           = 'TRY'
            i_wrbtr           = lv_wrbtr
          TABLES
            t_mwdat           = lt_mwdat
          EXCEPTIONS
            bukrs_not_found   = 1
            country_not_found = 2
            mwskz_not_defined = 3
            mwskz_not_valid   = 4
            ktosl_not_found   = 5
            kalsm_not_found   = 6
            parameter_error   = 7
            knumh_not_found   = 8
            kschl_not_found   = 9
            unknown_error     = 10
            account_not_found = 11
            txjcd_not_valid   = 12
            OTHERS            = 13.
        IF sy-subrc EQ 0.
          READ TABLE lt_mwdat INDEX 1.
          IF sy-subrc EQ 0.
            gt_out-doc_amount = lt_mwdat-kawrt + lt_mwdat-wmwst.
            gt_out-vat        = lt_mwdat-wmwst.
            gt_out-icon       = icon_yellow_light.
            APPEND gt_out.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.

  ELSEIF r4 = 'X'.

    CLEAR: lv_surec_tipi_temp.


    lv_surec_tipi_temp = '20'.

    CLEAR: lv_mwskz.
    SELECT SINGLE mwskz FROM zsog_fi_010 INTO lv_mwskz
            WHERE surec_tipi = lv_surec_tipi_temp.

    SELECT file_date gjahr belnr bukrs doc_amount
      FROM zsog_fi_011
      INTO CORRESPONDING FIELDS OF TABLE gt_out
     WHERE accounting_period_no = gs_009-accounting_period_no
       AND week_identifier      = gs_009-week_identifier
       AND date_from            = gs_009-date_from
       AND date_to              = gs_009-date_to
       AND surec_tipi           = lv_surec_tipi_temp.
    IF sy-subrc EQ 0.
      LOOP AT gt_out.

        CLEAR: lv_wrbtr, lt_mwdat, lt_mwdat[].

        lv_wrbtr = gt_out-doc_amount.

        CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
          EXPORTING
            i_bukrs                 = gc_bukrs
            i_mwskz                 = lv_mwskz
            i_waers                 = 'TRY'
            i_wrbtr                 = lv_wrbtr
          TABLES
            t_mwdat                 = lt_mwdat
          EXCEPTIONS
            bukrs_not_found         = 1
            country_not_found       = 2
            mwskz_not_defined       = 3
            mwskz_not_valid         = 4
            account_not_found       = 5
            different_discount_base = 6
            different_tax_base      = 7
            txjcd_not_valid         = 8
            not_found               = 9
            ktosl_not_found         = 10
            kalsm_not_found         = 11
            parameter_error         = 12
            knumh_not_found         = 13
            kschl_not_found         = 14
            unknown_error           = 15
            OTHERS                  = 16.
        IF sy-subrc EQ 0 AND lt_mwdat[] IS NOT INITIAL.
          READ TABLE lt_mwdat INDEX 1.
          IF sy-subrc EQ 0.
            gt_out-net_amount = lt_mwdat-kawrt.
            gt_out-vat        = lt_mwdat-wmwst.

            gt_out-accounting_period_no = gs_009-accounting_period_no.
            gt_out-week_identifier      = gs_009-week_identifier.
            gt_out-date_from            = gs_009-date_from.
            gt_out-date_to              = gs_009-date_to.

            gt_out-icon = icon_yellow_light.
            MODIFY gt_out.
          ENDIF.
        ENDIF.



      ENDLOOP.
    ENDIF.

  ELSEIF r5 = 'X'.

    SELECT * FROM zsog_fi_011
      INTO CORRESPONDING FIELDS OF TABLE gt_011
      WHERE accounting_period_no = gs_009-accounting_period_no
        AND week_identifier      = gs_009-week_identifier
        AND date_from            = gs_009-date_from
        AND date_to              = gs_009-date_to
        AND file_date            = s_tarih-low
        AND surec_tipi           = '30'.
    IF sy-subrc NE 0.
      SELECT file_date
         SUM( sans_commmission ) AS doc_amount
        FROM zsg_t_026
        INTO CORRESPONDING FIELDS OF TABLE gt_out
        WHERE file_date EQ s_tarih-low
*          AND debt_comm GT 0 "commented by xosahin 02.12.2019
        GROUP BY file_date.
      IF sy-subrc EQ 0.

        LOOP AT gt_out.
          gt_out-accounting_period_no = gs_009-accounting_period_no.
          gt_out-week_identifier      = gs_009-week_identifier.
          gt_out-date_from            = gs_009-date_from.
          gt_out-date_to              = gs_009-date_to.

          gt_out-icon = icon_yellow_light.
          MODIFY gt_out.

        ENDLOOP.

      ENDIF.
    ELSE.
      cv_error = 'X'.
      CONCATENATE s_tarih-low+6(2) '.'
                  s_tarih-low+4(2) '.'
                  s_tarih-low+0(4)
                  ' tarihli tahakkuk belgesi oluşturulduğundan'
                  ' tekrar oluşturulamaz.'
             INTO lv_mes.
      MESSAGE lv_mes TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ELSEIF r7 = 'X'.

    CLEAR: lv_surec_tipi_temp, lr_surec_tipi_temp, lr_surec_tipi_temp[].

    lv_surec_tipi_temp = '35'.

    lr_surec_tipi_temp-sign   = 'I'.
    lr_surec_tipi_temp-option = 'EQ'.

    lr_surec_tipi_temp-low = '30'.
    APPEND lr_surec_tipi_temp.
    lr_surec_tipi_temp-low = '35'.
    APPEND lr_surec_tipi_temp.
    lr_surec_tipi_temp-low = '40'.
    APPEND lr_surec_tipi_temp.

    SELECT * FROM zsog_fi_011
         INTO CORRESPONDING FIELDS OF TABLE gt_011
         WHERE accounting_period_no EQ gs_009-accounting_period_no
           AND week_identifier      EQ gs_009-week_identifier
           AND date_from            EQ gs_009-date_from
           AND date_to              EQ gs_009-date_to
           AND surec_tipi           IN lr_surec_tipi_temp.

    READ TABLE gt_011 WITH KEY surec_tipi = lv_surec_tipi_temp.
    IF sy-subrc NE 0.
      cv_error = 'X'.
      CONCATENATE gs_009-accounting_period_no ' / '
                  gs_009-week_identifier
                  ' dönem ters hareket belgesi oluşturulmadığından'
                  ' fatura oluşturulamaz.'
             INTO lv_mes.
      MESSAGE lv_mes TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ELSE.

      SELECT retailer_no
         SUM( sans_commmission )        AS net_amount
         SUM( vat_on_sans_commmission ) AS vat
         SUM( debt_comm )               AS doc_amount
        FROM zsg_t_026
        INTO CORRESPONDING FIELDS OF TABLE gt_out
        WHERE file_date GE gs_009-date_from
          AND file_date LT gs_009-date_to
*          AND debt_comm GT 0 " commented by xosahin 02.12.2019
        GROUP BY  retailer_no.
      IF sy-subrc EQ 0.

        LOOP AT gt_out.
          gt_out-accounting_period_no = gs_009-accounting_period_no.
          gt_out-week_identifier      = gs_009-week_identifier.
          gt_out-date_from            = gs_009-date_from.
          gt_out-date_to              = gs_009-date_to.


          READ TABLE gt_011
            WITH KEY accounting_period_no = gt_out-accounting_period_no
                     week_identifier      = gt_out-week_identifier
                     file_date            = '00000000'
                     date_from            = gt_out-date_from
                     date_to              = gt_out-date_to
                     surec_tipi           = gv_surec_tipi
                     retailer_no          = gt_out-retailer_no.
          IF sy-subrc EQ 0.
            DELETE gt_out.
            CONTINUE.
          ELSE.

            gt_out-icon = icon_yellow_light.
            MODIFY gt_out.
          ENDIF.
        ENDLOOP.

      ENDIF.

    ENDIF.

    SORT gt_out BY retailer_no.

  ELSEIF r8 = 'X'.

    CLEAR: lv_surec_tipi_temp.

    lv_surec_tipi_temp = '40'.

    SELECT file_date retailer_no gjahr belnr bukrs doc_amount fatura th
         FROM zsog_fi_011
         INTO CORRESPONDING FIELDS OF TABLE gt_out
        WHERE accounting_period_no = gs_009-accounting_period_no
          AND week_identifier      = gs_009-week_identifier
          AND date_from            = gs_009-date_from
          AND date_to              = gs_009-date_to
          AND surec_tipi           = lv_surec_tipi_temp.
    IF sy-subrc EQ 0.

      LOOP AT gt_out.

        gt_out-accounting_period_no = gs_009-accounting_period_no.
        gt_out-week_identifier      = gs_009-week_identifier.
        gt_out-date_from            = gs_009-date_from.
        gt_out-date_to              = gs_009-date_to.

        gt_out-icon = icon_yellow_light.
        MODIFY gt_out.
      ENDLOOP.

    ENDIF.

    SORT gt_out BY retailer_no.

  ENDIF.

  CHECK cv_error = space.

  IF gt_out[] IS INITIAL.
    cv_error = 'X'.
    MESSAGE 'Veri bulunamadı.' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  show_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_data .
  CALL SCREEN 100.
ENDFORM.                    " show_data
*&---------------------------------------------------------------------*
*&      Form  show_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_alv .

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.
    CREATE OBJECT grid
      EXPORTING
        i_parent = g_custom_container.


    gs_layo100-zebra      = 'X'.
    gs_layo100-cwidth_opt = 'X'.
    gs_layo100-sel_mode   = 'A'.
    gs_layo100-info_fname = 'COLOR'.
    gs_layo100-stylefname = 'CELLTAB'.
    gs_layo100-ctab_fname = 'CELLCOLOR'.
    gs_variant-report     = sy-repid .

    PERFORM build_fcat .
    PERFORM set_cell_colours.
    PERFORM set_cell_styles.
    PERFORM exclude_button CHANGING gt_toolbar_excluding .

    CALL METHOD grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layo100
        it_toolbar_excluding = gt_toolbar_excluding
        is_variant           = gs_variant
        i_save               = 'A'
      CHANGING
        it_outtab            = gt_out[]
        it_sort              = gt_sort[]
        it_fieldcatalog      = gt_fcat[].

    CREATE OBJECT event_receiver_0100.

*   register handler for events
    SET HANDLER event_receiver_0100->handle_right_click                FOR grid.
    SET HANDLER event_receiver_0100->handle_left_click_design          FOR grid.
    SET HANDLER event_receiver_0100->handle_move_control               FOR grid.
    SET HANDLER event_receiver_0100->handle_size_control               FOR grid.
    SET HANDLER event_receiver_0100->handle_left_click_run             FOR grid.
    SET HANDLER event_receiver_0100->handle_onf1                       FOR grid.
    SET HANDLER event_receiver_0100->handle_onf4                       FOR grid.
    SET HANDLER event_receiver_0100->handle_data_changed               FOR grid.
    SET HANDLER event_receiver_0100->handle_ondropgetflavor            FOR grid.
    SET HANDLER event_receiver_0100->handle_ondrag                     FOR grid.
    SET HANDLER event_receiver_0100->handle_ondrop                     FOR grid.
    SET HANDLER event_receiver_0100->handle_ondropcomplete             FOR grid.
    SET HANDLER event_receiver_0100->handle_subtotal_text              FOR grid.
    SET HANDLER event_receiver_0100->handle_before_user_command        FOR grid.
    SET HANDLER event_receiver_0100->handle_user_command               FOR grid.
    SET HANDLER event_receiver_0100->handle_after_user_command         FOR grid.
    SET HANDLER event_receiver_0100->handle_double_click               FOR grid.
    SET HANDLER event_receiver_0100->handle_delayed_callback           FOR grid.
    SET HANDLER event_receiver_0100->handle_delayed_changed_sel_cal    FOR grid.
    SET HANDLER event_receiver_0100->handle_print_top_of_page          FOR grid.
    SET HANDLER event_receiver_0100->handle_print_top_of_list          FOR grid.
    SET HANDLER event_receiver_0100->handle_print_end_of_page          FOR grid.
    SET HANDLER event_receiver_0100->handle_print_end_of_list          FOR grid.
    SET HANDLER event_receiver_0100->handle_top_of_page                FOR grid.
    SET HANDLER event_receiver_0100->handle_context_menu_request       FOR grid.
    SET HANDLER event_receiver_0100->handle_menu_button                FOR grid.
    SET HANDLER event_receiver_0100->handle_toolbar                    FOR grid.
    SET HANDLER event_receiver_0100->handle_hotspot_click              FOR grid.
    SET HANDLER event_receiver_0100->handle_end_of_list                FOR grid.
    SET HANDLER event_receiver_0100->handle_after_refresh              FOR grid.
    SET HANDLER event_receiver_0100->handle_button_click               FOR grid.
    SET HANDLER event_receiver_0100->handle_data_changed_finished      FOR grid.

*   register F4 fields
    PERFORM field_f4_register.

    CALL METHOD grid->set_toolbar_interactive.

*   ENTER key is pressed or
    CALL METHOD grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*   data is changed and cursor is moved from the cell
    CALL METHOD grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  ELSE .
    PERFORM check_changed_data USING grid .
    PERFORM refresh_table_display USING grid .
  ENDIF .

ENDFORM.                    " show_alv

*&---------------------------------------------------------------------*
*&      form  variant_f4
*&---------------------------------------------------------------------*
FORM variant_f4 USING p_variant.

  DATA : h_exit.

  CLEAR gx_variant.

  CALL FUNCTION 'LVC_VARIANT_F4'
    EXPORTING
      is_variant    = gs_variant
      i_save        = gv_save
    IMPORTING
      e_exit        = h_exit
      es_variant    = gx_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE i000(0k) WITH 'Varyant bulunmamaktadır.'.
  ENDIF.

  IF h_exit IS INITIAL.
    gs_variant-variant = gx_variant-variant.
    p_variant          = gx_variant-variant.
  ENDIF.
ENDFORM.                                                    "variant_f4
*&---------------------------------------------------------------------*
*&      form  variant_default
*&---------------------------------------------------------------------*
FORM variant_default USING p_variant.

  gs_variant-report     = sy-repid.
  gs_variant-username   = sy-uname.
  gx_variant            = gs_variant.

  IF NOT p_variant IS INITIAL.
    gx_variant-variant = p_variant.
  ENDIF.

  CALL FUNCTION 'LVC_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = gv_save
    CHANGING
      cs_variant    = gx_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  CASE sy-subrc.
    WHEN 0.
      p_variant = gx_variant-variant.
    WHEN 2.
      CLEAR p_variant.
  ENDCASE.

ENDFORM.                    "variant_default
*&---------------------------------------------------------------------*
*&      Form  build_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fcat .
  DATA : lv_progname LIKE sy-repid.

  REFRESH : gt_flcat , gt_fcat.

  lv_progname = sy-repid .

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = lv_progname
      i_internal_tabname     = 'GT_OUT'
      i_client_never_display = 'X'
      i_inclname             = lv_progname
    CHANGING
      ct_fieldcat            = gt_flcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
*       accounting_period_no LIKE zsog_fi_009-accounting_period_no,
*       week_identifier      LIKE zsog_fi_009-week_identifier,
*       file_date            LIKE zsg_t_021-file_date,
*       date_from            LIKE zsog_fi_009-date_from,
*       date_to              LIKE zsog_fi_009-date_to,
*       gross_sales_refunds  LIKE zsg_t_021-gross_sales_refunds,
*       vat                  LIKE zsg_t_021-vat,
*       net_amount           LIKE zsg_t_021-gross_sales_refunds,
*       doc_amount           LIKE zsg_t_021-gross_sales_refunds,
  LOOP AT gt_flcat INTO gwa_flcat .
    FREE : gwa_flcat-key.
    MOVE-CORRESPONDING  gwa_flcat TO gs_fcat .
    gs_fcat-fieldname        = gwa_flcat-fieldname.
    gs_fcat-ref_table        = gwa_flcat-ref_tabname.
    gs_fcat-ref_field        = gwa_flcat-ref_fieldname.
    gs_fcat-coltext          = gwa_flcat-seltext_l.
    CASE gs_fcat-fieldname.
      WHEN 'ACCOUNTING_PERIOD_NO'.
        gs_fcat-coltext = 'Acc.Per.No.'.
      WHEN 'NET_AMOUNT'.
        gs_fcat-coltext = 'Net tutar'.
      WHEN 'DOC_AMOUNT'.
        IF r3 = 'X' OR r4 = 'X' OR r7 = 'X' OR r8 = 'X'.
          gs_fcat-coltext = 'Fatura tutarı'.
        ELSE.
          gs_fcat-coltext = 'Tahakkuk edilecek tutar'.
        ENDIF.

      WHEN 'FATURA' OR 'TH'.
        CONTINUE.
    ENDCASE.
    gs_fcat-seltext          = gs_fcat-coltext.
    gs_fcat-reptext          = gs_fcat-coltext.
    APPEND gs_fcat TO gt_fcat.
    CLEAR  gs_fcat.

  ENDLOOP.

  IF r1 = 'X'.
    DELETE gt_fcat WHERE fieldname  = 'RETAILER_NO'.
  ELSEIF r2 = 'X' OR r5 = 'X' OR r6 = 'X'.
    DELETE gt_fcat WHERE fieldname  = 'GROSS_SALES_REFUNDS'
                      OR fieldname  = 'VAT'
                      OR fieldname  = 'NET_AMOUNT'
                      OR fieldname  = 'RETAILER_NO'.
  ELSEIF r3 = 'X' OR r4 = 'X' OR ra = 'X' OR rb = 'X' OR rc = 'X' OR rd = 'X' OR r7 = 'X' OR r8 = 'X'.
    IF r3 = 'X' OR r4 = 'X'.
      DELETE gt_fcat WHERE fieldname  = 'FILE_DATE'
                        OR fieldname  = 'GROSS_SALES_REFUNDS'
                        OR fieldname  = 'RETAILER_NO'.
    ELSEIF ra = 'X' OR rb = 'X' OR rc = 'X' OR rd = 'X'.
      DELETE gt_fcat WHERE fieldname  = 'FILE_DATE'
                        OR fieldname  = 'GROSS_SALES_REFUNDS'
                        OR fieldname  = 'VAT'
                        OR fieldname  = 'NET_AMOUNT'
                        OR fieldname  = 'RETAILER_NO'.
    ELSEIF r7 = 'X' OR r8 = 'X'.
      DELETE gt_fcat WHERE fieldname  = 'FILE_DATE'
                        OR fieldname  = 'GROSS_SALES_REFUNDS'.
    ENDIF.

  ENDIF.

  DATA: ls_fcat TYPE lvc_s_fcat.

* hotspot fields
  ls_fcat-hotspot = 'X'.
  MODIFY gt_fcat FROM ls_fcat
    TRANSPORTING hotspot
    WHERE fieldname = 'ABC'.

* editable column
  ls_fcat-edit = 'X'.
  MODIFY gt_fcat FROM ls_fcat
    TRANSPORTING edit
    WHERE fieldname = 'ABC'.

* F4 list
  ls_fcat-f4availabl = 'X'.
  MODIFY gt_fcat FROM ls_fcat
    TRANSPORTING f4availabl
    WHERE fieldname = 'ABC'.

* dropdown list
  ls_fcat-drdn_hndl = '1'.
  MODIFY gt_fcat FROM ls_fcat
    TRANSPORTING drdn_hndl
    WHERE fieldname = 'ABC'.
*{   ->>> Added by Prodea Ozan Şahin - 03.12.2019 13:47:43


  IF r3 = abap_true. " BAŞ BAYİ -  Hesap Dönemi Fatura
    ls_fcat-edit = abap_true.
    MODIFY gt_fcat FROM ls_fcat
      TRANSPORTING edit
      WHERE fieldname = 'DOC_AMOUNT'.
  ENDIF.
*}     <<<- End of  Added - 03.12.2019 13:47:43
ENDFORM.                    " build_fcat
*&---------------------------------------------------------------------*
*&      Form  exclude_button
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_EXCLUDE text
*----------------------------------------------------------------------*
FORM exclude_button  CHANGING pt_exclude TYPE ui_functions.

*  APPEND CL_GUI_ALV_GRID=>MC_MB_SUM               TO PT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_MB_SUBTOT            TO PT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_GRAPH             TO PT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_INFO              TO PT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_PRINT_BACK        TO PT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_FILTER            TO PT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_FIND_MORE         TO PT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_FIND              TO PT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_MB_EXPORT            TO PT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_MB_VARIANT           TO PT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_DETAIL            TO PT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_MB_VIEW              TO PT_EXCLUDE.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_move_row      TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO pt_exclude.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_SORT              TO PT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_SORT_ASC          TO PT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_SORT_DSC          TO PT_EXCLUDE.
  APPEND cl_gui_alv_grid=>mc_fc_refresh           TO pt_exclude.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_CHECK             TO PT_EXCLUDE.
ENDFORM.                    " exclude_button
*&---------------------------------------------------------------------*
*&      Form  REFRESH_TABLE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GRID  text
*----------------------------------------------------------------------*
FORM refresh_table_display USING p_grid TYPE REF TO cl_gui_alv_grid.

  DATA : ls_stable TYPE lvc_s_stbl .

  ls_stable-row = 'X'.
  ls_stable-col = 'X'.
  CALL METHOD p_grid->refresh_table_display
    EXPORTING
      is_stable      = ls_stable
      i_soft_refresh = 'X'
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2.
ENDFORM.                    " REFRESH_TABLE_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CHECK_CHANGED_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GRID  text
*----------------------------------------------------------------------*
FORM check_changed_data USING p_grid TYPE REF TO cl_gui_alv_grid .
  DATA: l_valid TYPE c.

  CALL METHOD p_grid->check_changed_data
    IMPORTING
      e_valid = l_valid.
ENDFORM.                    " CHECK_CHANGED_DATA
*&---------------------------------------------------------------------*
*&      Form  field_f4_register
*&---------------------------------------------------------------------*
FORM field_f4_register.

  DATA:
    lt_f4 TYPE lvc_t_f4,
    ls_f4 TYPE lvc_s_f4.

  ls_f4-fieldname  = 'ABC'.
  ls_f4-register   = 'X'.
* ls_f4-getbefore  = 'X'.
* ls_f4-chngeafter = 'X'.
  INSERT ls_f4 INTO TABLE lt_f4.

  CALL METHOD grid->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4.
ENDFORM.                    "field_f4_register
*&---------------------------------------------------------------------*
*&      Form  SET_CELL_COLOURS
*&---------------------------------------------------------------------*
*       Set colour of individual ALV cell, field
*----------------------------------------------------------------------*
FORM set_cell_colours .
*  DATA: wa_cellcolor TYPE lvc_s_scol.
*  DATA: lv_index     TYPE sy-tabix.
*
*  LOOP AT gt_out INTO gs_out.
*    lv_index = sy-tabix.
*
**   Set colour of EBELN field to various colors based on sy-tabix value
*    wa_cellcolor-fname = 'LGNUM'.
*    wa_cellcolor-color-col = sy-tabix.  "color code 1-7, if outside rage defaults to 7
*    wa_cellcolor-color-int = '1'.  "1 = Intensified on, 0 = Intensified off
*    wa_cellcolor-color-inv = '0'.  "1 = text colour, 0 = background colour
*    APPEND wa_cellcolor TO gs_out-cellcolor.
*    MODIFY gt_out FROM gs_out INDEX lv_index TRANSPORTING cellcolor.
*
*    IF gs_out-vsola GT 1.
*      wa_cellcolor-fname = 'VSOLA'.
*      wa_cellcolor-color-col = 4.  "color code 1-7, if outside rage defaults to 7
*      wa_cellcolor-color-int = '0'.  "1 = Intensified on, 0 = Intensified off
*      wa_cellcolor-color-inv = '0'.  "1 = text colour, 0 = background colour
*      APPEND wa_cellcolor TO gs_out-cellcolor.
*      MODIFY gt_out FROM gs_out INDEX lv_index TRANSPORTING cellcolor.
*    ENDIF.
*
*    wa_cellcolor-fname = 'TANUM'.
*    wa_cellcolor-color-col = 6.  "color code 1-7, if outside rage defaults to 7
*    wa_cellcolor-color-int = '0'.  "1 = Intensified on, 0 = Intensified off
*    wa_cellcolor-color-inv = '1'.  "1 = text colour, 0 = background colour
*    APPEND wa_cellcolor TO gs_out-cellcolor.
*    MODIFY gt_out FROM gs_out INDEX lv_index TRANSPORTING cellcolor.
*  ENDLOOP.

ENDFORM.                    " SET_CELL_COLOURS
*&---------------------------------------------------------------------*
*&      Form  set_cell_styles
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_cell_styles .

*  DATA: wa_style TYPE lvc_s_styl.
*  DATA: lv_index TYPE sy-tabix.
*
*  DATA: lv_kalan TYPE i.
*
*  LOOP AT gt_out INTO gs_out.
*    CLEAR: lv_index, lv_kalan.
*
*    lv_index = sy-tabix.
*    lv_kalan = lv_index MOD ( 2 ).
*
*
*    CLEAR: wa_style.
*    wa_style-fieldname = 'VSOLA'.
*    wa_style-style     = cl_gui_alv_grid=>mc_style_button .
*
*    APPEND wa_style TO gs_out-celltab.
*    MODIFY gt_out FROM gs_out INDEX lv_index TRANSPORTING celltab.
*
*    IF lv_kalan = 1.
*      CLEAR: wa_style.
*      wa_style-fieldname = 'VSOLM'.
*      wa_style-style     = cl_gui_alv_grid=>mc_style_disabled .
*
*      APPEND wa_style TO gs_out-celltab.
*      MODIFY gt_out FROM gs_out INDEX lv_index TRANSPORTING celltab.
*
*    ENDIF.
*
*  ENDLOOP.

ENDFORM.                    " set_cell_styles
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SECILI  text
*----------------------------------------------------------------------*
FORM get_selected_rows  TABLES it_secili STRUCTURE gt_out.

  DATA: lt_rows TYPE lvc_t_row.
  DATA: l_row   TYPE lvc_s_row.

  CLEAR: it_secili, it_secili[].

  CALL METHOD grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.
  LOOP AT lt_rows INTO l_row.
    READ TABLE gt_out INDEX l_row-index.
    IF sy-subrc EQ 0.
      CLEAR: it_secili.
      MOVE-CORRESPONDING gt_out TO it_secili.
      APPEND it_secili.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  YOU_SURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_QUEST  text
*      <--P_GV_ERROR  text
*----------------------------------------------------------------------*
FORM you_sure  USING    iv_quest
               CHANGING cv_answer.

  DATA: lv_title(50) TYPE c VALUE 'İşlem onayı'.

  CLEAR: cv_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = lv_title
      text_question  = iv_quest
    IMPORTING
      answer         = cv_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

ENDFORM.                    " YOU_SURE
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_ERROR  text
*----------------------------------------------------------------------*
FORM check_data  CHANGING cv_error.

  CLEAR: gs_010, gv_surec_tipi, cv_error, gs_009.

  IF r1 = 'X' OR r2 = 'X' OR r5 = 'X' OR r6 = 'X'.
    IF s_tarih[] IS INITIAL.
      cv_error = 'X'.
      MESSAGE 'Lütfen tarih giriniz.'
         TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ELSEIF r3 = 'X' OR r4 = 'X' OR r7 = 'X' OR r8 = 'X'
      OR ra = 'X' OR rb = 'X' OR rc = 'X' OR rd = 'X'.
    IF s_accper[] IS INITIAL.
      cv_error = 'X'.
      MESSAGE 'Lütfen period giriniz.'
         TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ELSEIF s_week[] IS INITIAL.
      cv_error = 'X'.
      MESSAGE 'Lütfen hafta giriniz.'
         TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.


  IF r1 = 'X'.
    gv_surec_tipi = 10.
  ELSEIF ra = 'X'.
    gv_surec_tipi = 15.
  ELSEIF r3 = 'X'.
    gv_surec_tipi = 20.
  ELSEIF r5 = 'X'.
    gv_surec_tipi = 30.
  ELSEIF rc = 'X'.
    gv_surec_tipi = 35.
  ELSEIF r7 = 'X'.
    gv_surec_tipi = 40.
  ELSEIF r8 = 'X'.
    gv_surec_tipi = 45.
  ENDIF.


  IF NOT ( r2 = 'X' OR rb = 'X' OR r4 = 'X'
        OR r6 = 'X' OR rd = 'X' OR r8 = 'X').
    SELECT SINGLE * FROM zsog_fi_010
      INTO CORRESPONDING FIELDS OF gs_010
     WHERE surec_tipi = gv_surec_tipi.
    IF sy-subrc NE 0.
      cv_error = 'X'.
      MESSAGE s019(zsg) WITH 'ZSGFI17' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDIF.

  CHECK cv_error = space.

  IF r1 = 'X' OR r2 = 'X' OR r5 = 'X' OR r6 = 'X'.
    SELECT SINGLE * FROM zsog_fi_009
      INTO CORRESPONDING FIELDS OF gs_009
     WHERE date_from LE s_tarih-low
       AND date_to   GT s_tarih-low.
  ELSEIF r3 = 'X' OR r4 = 'X' OR r7 = 'X' OR r8 = 'X'
      OR ra = 'X' OR rb = 'X' OR rc = 'X' OR rd = 'X'.
    SELECT SINGLE * FROM zsog_fi_009
      INTO CORRESPONDING FIELDS OF gs_009
     WHERE accounting_period_no EQ s_accper-low
       AND week_identifier      EQ s_week-low.
  ENDIF.
  IF gs_009 IS INITIAL.
    cv_error = 'X'.
    MESSAGE s019(zsg) WITH 'ZSGFI16' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*  SELECT * FROM ZSOG_FI_009
*    INTO CORRESPONDING FIELDS OF TABLE gt_009
*   WHERE ( date_from >= s_tarih-low  AND date_to <= s_tarih-high OR
*           date_from <= s_tarih-low  AND date_to >= s_tarih-low  OR
*           date_from <= s_tarih-high AND date_to >= s_tarih-high ).



ENDFORM.                    " CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SECILI  text
*      <--P_GV_ERROR  text
*      -->P_LV_OKCODE  text
*----------------------------------------------------------------------*
FORM check_selected_rows  TABLES it_secili STRUCTURE gt_secili
                           USING iv_okcode
                        CHANGING cv_error.

  CLEAR: cv_error.
  IF it_secili[] IS INITIAL.
    cv_error = 'X'.
    MESSAGE 'Satır seçmediniz.'
       TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CASE iv_okcode.
    WHEN 'BB_GUNLUK' OR 'BB_HAFTA' OR 'BB_TH' OR
         'SB_GUNLUK' OR 'SB_HAFTA' OR 'SB_TH' .

      LOOP AT it_secili WHERE belnr IS NOT INITIAL.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        cv_error = 'X'.
        CLEAR: it_secili, it_secili[].
        MESSAGE 'Daha önceden oluşturulan satırlar seçilemez.'
           TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    WHEN 'BB_GUN_REV' OR 'BB_HAFTA_R' OR 'BB_TH_R' OR
         'SB_GUN_REV' OR 'SB_HAFTA_R' OR 'SB_TH_R'.
      LOOP AT it_secili WHERE belnr IS INITIAL.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        cv_error = 'X'.
        CLEAR: it_secili, it_secili[].
        MESSAGE 'Belge numarası mevcut değil.'
           TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
*  	WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " CHECK_SELECTED_ROWS
*&---------------------------------------------------------------------*
*&      Form  DOC_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SECILI  text
*----------------------------------------------------------------------*
FORM doc_create  TABLES it_secili STRUCTURE gt_secili
                  USING iv_okcode .

  DATA: ls_documentheader TYPE bapiache09 ,
        lt_accountgl      TYPE TABLE OF  bapiacgl09 WITH HEADER LINE,
        lt_accountpayable TYPE TABLE OF  bapiacap09 WITH HEADER LINE,
        lt_accounttax     TYPE TABLE OF  bapiactx09 WITH HEADER LINE,
        lt_currencyamount TYPE TABLE OF  bapiaccr09 WITH HEADER LINE,
        lv_obj_type       TYPE  bapiache09-obj_type,
        lv_obj_key        TYPE  bapiache09-obj_key,
        lv_obj_sys        TYPE  bapiache09-obj_sys,
        ls_tax            TYPE rtax1u15,
        lt_tax            TYPE TABLE OF rtax1u15,
        lv_itemno         TYPE int4,
        lt_return         TYPE TABLE OF bapiret2    WITH HEADER LINE,
        lv_tabix          TYPE sy-tabix,
        ls_011            TYPE zsog_fi_011,
        lt_011            TYPE zsog_fi_011 OCCURS 0 WITH HEADER LINE ,
        lv_uname          TYPE sy-uname,
        lv_udate          TYPE sy-datum,
        lv_uzeit          TYPE sy-uzeit,
        lv_tarih          TYPE sy-datum,
        lv_surec_tipi_gun TYPE zsog_fi_011-surec_tipi.


  lv_uname = sy-uname.
  lv_udate = sy-datum.
  lv_uzeit = sy-uzeit.

  CLEAR: lv_tarih.
  IF iv_okcode = 'BB_GUNLUK' OR
     iv_okcode = 'SB_GUNLUK'.
    lv_tarih = s_tarih-low.
  ELSEIF iv_okcode = 'BB_TH' OR
         iv_okcode = 'SB_TH'.
    lv_tarih = gs_009-date_to.
    lv_tarih = lv_tarih - 1.
  ENDIF.


  LOOP AT it_secili.
    CLEAR: gs_out, lv_tabix.
    READ TABLE gt_out INTO gs_out
                  WITH KEY accounting_period_no = it_secili-accounting_period_no
                           week_identifier      = it_secili-week_identifier
                           retailer_no          = it_secili-retailer_no
                           file_date            = it_secili-file_date
                           date_from            = it_secili-date_from
                           date_to              = it_secili-date_to.
    IF sy-subrc EQ 0.
      lv_tabix = sy-tabix.
      CLEAR: ls_documentheader,
             lt_accountgl, lt_accountgl[],
             lt_accountpayable, lt_accountpayable[],
             lt_accounttax, lt_accounttax[],
             lt_currencyamount, lt_currencyamount[],
             lv_obj_type, lv_obj_key, lv_obj_sys,
             ls_tax, lt_tax, lt_tax[], lv_itemno,
             lt_return, lt_return[], lv_itemno.

      CLEAR ls_documentheader.
      ls_documentheader-bus_act     = 'RFBU'.
      ls_documentheader-username    = sy-uname.
      ls_documentheader-header_txt  = 'DIJ.PLAT.KOM.TAH'.
      ls_documentheader-comp_code   = gc_bukrs.
      ls_documentheader-doc_date    = lv_tarih.
      ls_documentheader-pstng_date  = lv_tarih.
      ls_documentheader-fisc_year   = lv_tarih(4).
      ls_documentheader-fis_period  = lv_tarih+4(2).
      ls_documentheader-doc_type    = gs_010-belge_turu.
*      ls_documentheader-ref_doc_no  = 'Ref belge'.

      DO 2 TIMES.
        CLEAR: lt_accountgl.
        lv_itemno = lv_itemno + 1.
        lt_accountgl-itemno_acc        = lv_itemno.
        IF sy-index = 1.
          lt_accountgl-gl_account        = gs_010-borc_hesap .
          IF iv_okcode = 'BB_TH' OR iv_okcode = 'SB_TH'.
*            lt_accountgl-profit_ctr        = '2425000001'.
            lt_accountgl-profit_ctr        = gs_010-prctr.
*            lt_accountgl-tax_code          = 'L0'.
            lt_accountgl-tax_code          = gs_010-mwskz.
          ENDIF.
        ELSEIF sy-index = 2.
          lt_accountgl-gl_account        = gs_010-alacak_hesap .
          IF iv_okcode = 'BB_GUNLUK' OR iv_okcode = 'SB_GUNLUK'.
*            lt_accountgl-profit_ctr        = '2425000001'.
            lt_accountgl-profit_ctr        = gs_010-prctr.
*            lt_accountgl-tax_code          = 'L0'.
            lt_accountgl-tax_code          = gs_010-mwskz.
          ENDIF.
        ENDIF.

        IF iv_okcode = 'BB_GUNLUK' OR iv_okcode = 'SB_GUNLUK'.
          lt_accountgl-item_text         = 'DIJITAL PLATFORM KOMISYONU GUNLUK GELIR TAHAKKUKU' .
        ELSEIF iv_okcode = 'BB_TH' OR iv_okcode = 'SB_TH'.
          lt_accountgl-item_text         = 'DIJITAL PLATFORM KOMISYONU HAFTALIK TAH. KAYDI' .
        ENDIF.

        APPEND lt_accountgl.

        CLEAR: lt_currencyamount.
        lt_currencyamount-itemno_acc  = lv_itemno.
        lt_currencyamount-currency    = 'TRY'.
        IF sy-index = 1.
          lt_currencyamount-amt_doccur  = it_secili-doc_amount .
        ELSEIF sy-index = 2.
          lt_currencyamount-amt_doccur  = it_secili-doc_amount * -1.
        ENDIF.
        APPEND lt_currencyamount.
      ENDDO.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader = ls_documentheader
        IMPORTING
          obj_type       = lv_obj_type
          obj_key        = lv_obj_key
          obj_sys        = lv_obj_sys
        TABLES
          accountgl      = lt_accountgl
*         accountpayable = lt_accountpayable
*         accounttax     = lt_accounttax
          currencyamount = lt_currencyamount
          return         = lt_return.
      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        CLEAR: ls_011, lt_011, lt_011[].

        IF iv_okcode = 'BB_TH' OR iv_okcode = 'SB_TH'.

          CLEAR: lv_surec_tipi_gun.
          IF iv_okcode = 'BB_TH'.
            lv_surec_tipi_gun = '10'.
          ELSEIF iv_okcode = 'SB_TH'.
            lv_surec_tipi_gun = '30'.
          ENDIF.

          CLEAR: ls_011, lt_011, lt_011[].
          SELECT * FROM zsog_fi_011
            INTO CORRESPONDING FIELDS OF TABLE lt_011
           WHERE accounting_period_no = gs_out-accounting_period_no
             AND week_identifier      = gs_out-week_identifier
             AND date_from            = gs_out-date_from
             AND date_to              = gs_out-date_to
             AND surec_tipi           = lv_surec_tipi_gun.
          IF sy-subrc EQ 0.
            LOOP AT lt_011 INTO ls_011.
              ls_011-th           = 'X'.
              ls_011-th_belge_no  = lv_obj_key+0(10).
              ls_011-th_belge_yil = lv_obj_key+14(4).
              ls_011-tname        = lv_uname.
              ls_011-tdate        = lv_udate.
              ls_011-tzeit        = lv_uzeit.
              MODIFY lt_011 FROM ls_011.
            ENDLOOP.
          ENDIF.
        ENDIF.

        CLEAR: ls_011.
        ls_011-file_date             = gs_out-file_date.
        ls_011-accounting_period_no  = gs_009-accounting_period_no.
        ls_011-week_identifier       = gs_009-week_identifier.
        ls_011-date_from             = gs_009-date_from.
        ls_011-date_to               = gs_009-date_to.
        ls_011-surec_tipi            = gv_surec_tipi.
        ls_011-bukrs                 = lv_obj_key+10(4).
        ls_011-belnr                 = lv_obj_key+0(10).
        ls_011-gjahr                 = lv_obj_key+14(4).
        ls_011-doc_amount            = gs_out-doc_amount.
        ls_011-uname                 = lv_uname.
        ls_011-udate                 = lv_udate.
        ls_011-uzeit                 = lv_uzeit.
        APPEND ls_011 TO lt_011.

        MODIFY zsog_fi_011 FROM TABLE lt_011.


        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        MESSAGE s255(zfi) WITH lv_obj_key+0(10)
                               lv_obj_key+10(4)
                               lv_obj_key+14(4)
                          INTO gv_dummy.

        PERFORM sys_add_bapiret2 TABLES gt_message.

        gs_out-icon  = icon_green_light.
        gs_out-bukrs = lv_obj_key+10(4).
        gs_out-gjahr = lv_obj_key+14(4).
        gs_out-belnr = lv_obj_key+0(10).
        MODIFY gt_out FROM gs_out INDEX lv_tabix.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        LOOP AT lt_return WHERE type CA 'EAX'.
          MESSAGE ID lt_return-id
                TYPE lt_return-type
              NUMBER lt_return-number
                WITH lt_return-message_v1
                     lt_return-message_v2
                     lt_return-message_v3
                     lt_return-message_v4
                INTO gv_dummy.
          PERFORM sys_add_bapiret2 TABLES gt_message.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " DOC_CREATE
*&---------------------------------------------------------------------*
*&      Form  SYS_ADD_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sys_add_bapiret2 TABLES pt_message STRUCTURE bapiret2 .

  DATA : ls_return TYPE bapiret2.

  CALL FUNCTION 'FS_BAPI_BAPIRET2_FILL'
    EXPORTING
      type   = sy-msgty
      cl     = sy-msgid
      number = sy-msgno
      par1   = sy-msgv1
      par2   = sy-msgv2
      par3   = sy-msgv3
      par4   = sy-msgv4
    IMPORTING
      return = ls_return.
  APPEND ls_return TO pt_message.
ENDFORM.                    " SYS_ADD_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  BAPIRET_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bapiret_display  TABLES  pt_message STRUCTURE bapiret2 .

  CHECK pt_message[] IS NOT INITIAL .

  CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
    EXPORTING
      it_message = pt_message[].

  CLEAR: pt_message, pt_message[].

ENDFORM.                    " BAPIRET_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  DOC_REVERSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SECILI  text
*      -->P_LV_OKCODE  text
*----------------------------------------------------------------------*
FORM doc_reverse  TABLES it_secili STRUCTURE gt_secili
                   USING iv_okcode.

  DATA: ls_reversal TYPE bapiacrev,
        lv_bus_act  TYPE bapiache09-bus_act,
        lv_obj_type TYPE bapiacrev-obj_type,
        lv_obj_key  TYPE bapiacrev-obj_key,
        lv_obj_sys  TYPE bapiacrev-obj_sys,
        lt_return   TYPE bapiret2 OCCURS 0 WITH HEADER LINE,
        lv_tabix    TYPE sy-tabix,
        lv_rev_date TYPE d,
        ls_011      TYPE zsog_fi_011,
        lt_011      TYPE zsog_fi_011 OCCURS 0 WITH HEADER LINE,
        ls_011_log  TYPE zsog_fi_011_log,
        lv_rname    TYPE sy-uname,
        lv_rdate    TYPE sy-datum,
        lv_rzeit    TYPE sy-uzeit.

  RANGES: lr_surec_tip FOR zsog_fi_011-surec_tipi.

  lv_rname = sy-uname.
  lv_rdate = sy-datum.
  lv_rzeit = sy-uzeit.

  PERFORM pop_up_rev_date CHANGING lv_rev_date.
  IF lv_rev_date IS INITIAL.
    MESSAGE 'Ters kayıt tarihi girmediniz.'
       TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT it_secili.
    CLEAR: gs_out, lv_tabix.
    READ TABLE gt_out INTO gs_out
                  WITH KEY accounting_period_no = it_secili-accounting_period_no
                           week_identifier      = it_secili-week_identifier
                           retailer_no          = it_secili-retailer_no
                           file_date            = it_secili-file_date
                           date_from            = it_secili-date_from
                           date_to              = it_secili-date_to.
    IF sy-subrc EQ 0.
      lv_tabix = sy-tabix.
      CLEAR: ls_reversal, lv_bus_act, lv_obj_type, lv_obj_key, lv_obj_sys,
      lt_return, lt_return[].

      ls_reversal-obj_type      = 'BKPFF'.
      CONCATENATE gs_out-belnr
                  gs_out-bukrs
                  gs_out-gjahr
             INTO ls_reversal-obj_key .
      ls_reversal-obj_sys      = sy-mandt.
      ls_reversal-obj_key_r    = ls_reversal-obj_key.
      ls_reversal-pstng_date   = lv_rev_date.
      ls_reversal-comp_code    = gs_out-bukrs.
      ls_reversal-reason_rev   = '01'.

      lv_bus_act = 'RFBU'.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
        EXPORTING
          reversal = ls_reversal
          bus_act  = lv_bus_act
        IMPORTING
          obj_type = lv_obj_type
          obj_key  = lv_obj_key
          obj_sys  = lv_obj_sys
        TABLES
          return   = lt_return.
      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.



        CLEAR: lr_surec_tip, lr_surec_tip[].

        IF iv_okcode = 'BB_GUN_REV' OR iv_okcode = 'SB_GUN_REV'.

          CLEAR: lr_surec_tip.
          lr_surec_tip-sign = 'I'.
          lr_surec_tip-option = 'EQ'.
          IF iv_okcode = 'BB_GUN_REV'.
            lr_surec_tip-low    = '10'.
          ELSEIF iv_okcode = 'SB_GUN_REV'.
            lr_surec_tip-low    = '30'.
          ENDIF.
          APPEND lr_surec_tip.

          CLEAR: ls_011, ls_011_log.
          SELECT SINGLE * FROM zsog_fi_011
            INTO CORRESPONDING FIELDS OF ls_011
           WHERE accounting_period_no EQ gs_out-accounting_period_no
             AND week_identifier      EQ gs_out-week_identifier
             AND date_from            EQ gs_out-date_from
             AND date_to              EQ gs_out-date_to
             AND file_date            EQ gs_out-file_date
             AND surec_tipi           IN lr_surec_tip.
          IF sy-subrc EQ 0.
            DELETE zsog_fi_011 FROM ls_011.
            MOVE-CORRESPONDING ls_011 TO ls_011_log.
            ls_011_log-stblg      = lv_obj_key+0(10).
            ls_011_log-stjah      = lv_obj_key+14(4).
            ls_011_log-ters       = 'X'.
            ls_011_log-rname      = lv_rname.
            ls_011_log-rdate      = lv_rdate.
            ls_011_log-rzeit      = lv_rzeit.
            MODIFY zsog_fi_011_log FROM ls_011_log.
          ENDIF.
        ELSEIF iv_okcode = 'BB_TH_R' OR iv_okcode = 'SB_TH_R'.

          CLEAR: lr_surec_tip.
          lr_surec_tip-sign = 'I'.
          lr_surec_tip-option = 'EQ'.
          IF iv_okcode = 'BB_TH_R'.
            lr_surec_tip-low    = '10'.
            APPEND lr_surec_tip.
            lr_surec_tip-low    = '15'.
            APPEND lr_surec_tip.
          ELSEIF iv_okcode = 'SB_TH_R'.
            lr_surec_tip-low    = '30'.
            APPEND lr_surec_tip.
            lr_surec_tip-low    = '35'.
            APPEND lr_surec_tip.
          ENDIF.

          CLEAR: ls_011, ls_011_log, lt_011, lt_011[].
          SELECT * FROM zsog_fi_011
            INTO CORRESPONDING FIELDS OF TABLE lt_011
           WHERE accounting_period_no EQ gs_out-accounting_period_no
             AND week_identifier      EQ gs_out-week_identifier
             AND date_from            EQ gs_out-date_from
             AND date_to              EQ gs_out-date_to
             AND surec_tipi           IN lr_surec_tip.
          IF sy-subrc EQ 0.
            LOOP AT lt_011 INTO ls_011.
              IF ls_011-surec_tipi = '15' OR ls_011-surec_tipi = '35'.
                DELETE zsog_fi_011 FROM ls_011.
                MOVE-CORRESPONDING ls_011 TO ls_011_log.
                ls_011_log-stblg      = lv_obj_key+0(10).
                ls_011_log-stjah      = lv_obj_key+14(4).
                ls_011_log-ters       = 'X'.
                ls_011_log-rname      = lv_rname.
                ls_011_log-rdate      = lv_rdate.
                ls_011_log-rzeit      = lv_rzeit.
                MODIFY zsog_fi_011_log FROM ls_011_log.
              ELSEIF ls_011-surec_tipi = '10' OR ls_011-surec_tipi = '30'.
                CLEAR: ls_011-th,
                       ls_011-th_belge_no,
                       ls_011-th_belge_yil,
                       ls_011-tdate,
                       ls_011-tname,
                       ls_011-tzeit.
                MODIFY lt_011 FROM ls_011.
              ENDIF.
            ENDLOOP.

            DELETE lt_011 WHERE surec_tipi = '15' OR surec_tipi = '35'.
            MODIFY zsog_fi_011 FROM TABLE lt_011.

          ENDIF.

        ELSEIF iv_okcode = 'BB_HAFTA_R' OR iv_okcode = 'SB_HAFTA_R'.

          CLEAR: lr_surec_tip.
          lr_surec_tip-sign = 'I'.
          lr_surec_tip-option = 'EQ'.
          IF iv_okcode = 'BB_HAFTA_R'.

            lr_surec_tip-low    = '10'.
            APPEND lr_surec_tip.
            lr_surec_tip-low    = '15'.
            APPEND lr_surec_tip.
            lr_surec_tip-low    = '20'.
            APPEND lr_surec_tip.

            CLEAR: ls_011, ls_011_log, lt_011, lt_011[].
            SELECT * FROM zsog_fi_011
              INTO CORRESPONDING FIELDS OF TABLE lt_011
             WHERE accounting_period_no EQ gs_out-accounting_period_no
               AND week_identifier      EQ gs_out-week_identifier
               AND date_from            EQ gs_out-date_from
               AND date_to              EQ gs_out-date_to
               AND surec_tipi           IN lr_surec_tip.
            IF sy-subrc EQ 0.
              LOOP AT lt_011 INTO ls_011.
                IF ls_011-surec_tipi = '20'.
                  DELETE zsog_fi_011 FROM ls_011.
                  MOVE-CORRESPONDING ls_011 TO ls_011_log.
                  ls_011_log-stblg      = lv_obj_key+0(10).
                  ls_011_log-stjah      = lv_obj_key+14(4).
                  ls_011_log-ters       = 'X'.
                  ls_011_log-rname      = lv_rname.
                  ls_011_log-rdate      = lv_rdate.
                  ls_011_log-rzeit      = lv_rzeit.
                  MODIFY zsog_fi_011_log FROM ls_011_log.
                ELSEIF ls_011-surec_tipi = '10' OR ls_011-surec_tipi = '15'.
                  CLEAR: ls_011-fatura,
                         ls_011-fatura_belge_no,
                         ls_011-fatura_belge_yil,
                         ls_011-fdate,
                         ls_011-fname,
                         ls_011-fzeit.
                  MODIFY lt_011 FROM ls_011.
                ENDIF.
              ENDLOOP.

              DELETE lt_011 WHERE surec_tipi = '20'.
              MODIFY zsog_fi_011 FROM TABLE lt_011.

            ENDIF.

          ELSEIF iv_okcode = 'SB_HAFTA_R'.

            CLEAR: ls_011, ls_011_log.
            SELECT SINGLE * FROM zsog_fi_011
              INTO CORRESPONDING FIELDS OF ls_011
             WHERE accounting_period_no EQ gs_out-accounting_period_no
               AND week_identifier      EQ gs_out-week_identifier
               AND date_from            EQ gs_out-date_from
               AND date_to              EQ gs_out-date_to
               AND surec_tipi           EQ '40'
               AND retailer_no          EQ gs_out-retailer_no.
            IF sy-subrc EQ 0.

              DELETE zsog_fi_011 FROM ls_011.
              MOVE-CORRESPONDING ls_011 TO ls_011_log.
              ls_011_log-stblg      = lv_obj_key+0(10).
              ls_011_log-stjah      = lv_obj_key+14(4).
              ls_011_log-ters       = 'X'.
              ls_011_log-rname      = lv_rname.
              ls_011_log-rdate      = lv_rdate.
              ls_011_log-rzeit      = lv_rzeit.
              MODIFY zsog_fi_011_log FROM ls_011_log.

            ENDIF.
          ENDIF.

        ENDIF.


        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        MESSAGE s258(zfi) WITH gs_out-belnr
                               gs_out-gjahr
                               gs_out-bukrs
                               lv_obj_key+0(10)
                          INTO gv_dummy.

        PERFORM sys_add_bapiret2 TABLES gt_message.

        gs_out-icon  = icon_green_light.
        CLEAR: gs_out-bukrs, gs_out-gjahr, gs_out-belnr.

        MODIFY gt_out FROM gs_out INDEX lv_tabix.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        LOOP AT lt_return WHERE type CA 'EAX'.
          MESSAGE ID lt_return-id
                TYPE lt_return-type
              NUMBER lt_return-number
                WITH lt_return-message_v1
                     lt_return-message_v2
                     lt_return-message_v3
                     lt_return-message_v4
                INTO gv_dummy.
          PERFORM sys_add_bapiret2 TABLES gt_message.
        ENDLOOP.
      ENDIF.


    ENDIF.
  ENDLOOP.


ENDFORM.                    " DOC_REVERSE
*&---------------------------------------------------------------------*
*&      Form  POP_UP_REV_DATE
*&---------------------------------------------------------------------*
FORM pop_up_rev_date  CHANGING cv_pstng_date.

  DATA: lv_retcode TYPE c,
        lv_in_date TYPE bkpf-budat,
        lt_fields  TYPE sval OCCURS 0 WITH HEADER LINE.

  CLEAR: cv_pstng_date, lv_in_date.

  IF r2 = 'X' OR r6 = 'X'.
    lv_in_date = s_tarih-low.
  ELSEIF r4 = 'X' OR rb = 'X' OR r8 = 'X' OR rd = 'X'.
    lv_in_date = gs_009-date_to.
    lv_in_date = lv_in_date - 1.
  ENDIF.


  CLEAR: lt_fields.
  lt_fields-tabname    = 'BKPF'.
  lt_fields-fieldname  = 'BUDAT'.
  lt_fields-value      = lv_in_date.
  lt_fields-field_attr = '01'.
  lt_fields-field_obl  = 'X'.
  lt_fields-fieldtext  = 'Ters kayıt tarihi'.
  APPEND lt_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Detayları giriniz.'
    IMPORTING
      returncode      = lv_retcode
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF sy-subrc EQ 0.

    CHECK lv_retcode IS INITIAL.

    READ TABLE lt_fields WITH KEY fieldname = 'BUDAT'.
    IF sy-subrc EQ 0.
      cv_pstng_date = lt_fields-value.
    ENDIF.

  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1
               sy-msgv2
               sy-msgv3
               sy-msgv4.
    EXIT.
  ENDIF.

ENDFORM.                    " POP_UP_REV_DATE
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen .

  IF r1 = 'X' OR r2 = 'X' OR r5 = 'X' OR r6 = 'X'.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'M1'.
          screen-active = 1.
          MODIFY SCREEN.
        WHEN 'M2'.
          screen-active = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
    SET CURSOR FIELD 'S_TARIH-LOW'.
  ELSEIF r3 = 'X' OR r4 = 'X' OR r7 = 'X' OR r8 = 'X'
      OR ra = 'X' OR rb = 'X' OR rc = 'X' OR rd = 'X'.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'M1'.
          screen-active = 0.
          MODIFY SCREEN.
        WHEN 'M2'.
          screen-active = 1.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
    SET CURSOR FIELD 'S_ACCPER-LOW'.
  ENDIF.

ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  INIT_ACCPER_WEEK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_accper_week USING iv_fieldname.

  DATA: lv_retfield        TYPE dfies-fieldname,
        lt_dynpfld_mapping LIKE TABLE OF  dselc WITH HEADER LINE,
        lv_dynprofield     TYPE help_info-dynprofld,
        BEGIN OF lt_value_tab OCCURS 0,
        accounting_period_no TYPE zsog_fi_009-accounting_period_no,
        week_identifier      TYPE zsog_fi_009-week_identifier,
        date_from            TYPE zsog_fi_009-date_from,
        date_to              TYPE zsog_fi_009-date_to  ,
        END OF lt_value_tab.

  CLEAR: lv_retfield, lv_dynprofield,
         lt_dynpfld_mapping, lt_dynpfld_mapping[].

  lv_dynprofield = iv_fieldname.
  IF iv_fieldname = 'S_ACCPER-LOW'.
    lv_retfield = 'ACCOUNTING_PERIOD_NO'.
  ELSEIF iv_fieldname = 'S_WEEK-LOW'.
    lv_retfield = 'WEEK_IDENTIFIER'.
  ENDIF.

  CLEAR: lt_dynpfld_mapping.
  lt_dynpfld_mapping-fldname   = 'F0001'.
  lt_dynpfld_mapping-dyfldname = 'S_ACCPER-LOW'.
  APPEND lt_dynpfld_mapping.

  CLEAR: lt_dynpfld_mapping.
  lt_dynpfld_mapping-fldname   = 'F0002'.
  lt_dynpfld_mapping-dyfldname = 'S_WEEK-LOW'.
  APPEND lt_dynpfld_mapping.

  SELECT DISTINCT
         accounting_period_no
         week_identifier
         date_from
         date_to
    FROM zsog_fi_009
    INTO CORRESPONDING FIELDS OF TABLE lt_value_tab.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = lv_retfield
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = lv_dynprofield
      value_org       = 'S'
    TABLES
      value_tab       = lt_value_tab
      dynpfld_mapping = lt_dynpfld_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.


ENDFORM.                    " INIT_ACCPER_WEEK
*&---------------------------------------------------------------------*
*&      Form  INV_DOC_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SECILI  text
*----------------------------------------------------------------------*
FORM inv_doc_create  TABLES it_secili STRUCTURE gt_secili
                      USING iv_okcode .

  DATA: ls_documentheader    TYPE bapiache09 ,
        lt_accountgl         TYPE TABLE OF  bapiacgl09 WITH HEADER LINE,
        lt_accountpayable    TYPE TABLE OF  bapiacap09 WITH HEADER LINE,
        lt_accountreceivable TYPE TABLE OF  bapiacar09 WITH HEADER LINE,
        lt_accounttax        TYPE TABLE OF  bapiactx09 WITH HEADER LINE,
        lt_currencyamount    TYPE TABLE OF  bapiaccr09 WITH HEADER LINE,
        lv_obj_type          TYPE  bapiache09-obj_type,
        lv_obj_key           TYPE  bapiache09-obj_key,
        lv_obj_sys           TYPE  bapiache09-obj_sys,
        ls_tax               TYPE rtax1u15,
        lt_tax               TYPE TABLE OF rtax1u15,
        lv_itemno            TYPE int4,
        lt_return            TYPE TABLE OF bapiret2    WITH HEADER LINE,
        lv_tabix             TYPE sy-tabix,
        ls_011               TYPE zsog_fi_011,
        lt_011               TYPE zsog_fi_011 OCCURS 0 WITH HEADER LINE,
        lv_uname             TYPE sy-uname,
        lv_udate             TYPE sy-datum,
        lv_uzeit             TYPE sy-uzeit,
        lv_tar_bit           TYPE sy-datum,
        lv_wrbtr             TYPE bseg-wrbtr.

  RANGES: lr_surec_tip_temp FOR zsog_fi_011-surec_tipi.

  lv_uname = sy-uname.
  lv_udate = sy-datum.
  lv_uzeit = sy-uzeit.

  lv_tar_bit = gs_009-date_to.
  lv_tar_bit = lv_tar_bit - 1.

  LOOP AT it_secili.
    CLEAR: gs_out, lv_tabix.
    READ TABLE gt_out INTO gs_out
                  WITH KEY accounting_period_no = it_secili-accounting_period_no
                           week_identifier      = it_secili-week_identifier
                           retailer_no          = it_secili-retailer_no
                           file_date            = it_secili-file_date
                           date_from            = it_secili-date_from
                           date_to              = it_secili-date_to.
    IF sy-subrc EQ 0.
      lv_tabix = sy-tabix.
      CLEAR: ls_documentheader,
             lt_accountgl, lt_accountgl[],
             lt_accountpayable, lt_accountpayable[],
             lt_accountreceivable, lt_accountreceivable[],
             lt_accounttax, lt_accounttax[],
             lt_currencyamount, lt_currencyamount[],
             lv_obj_type, lv_obj_key, lv_obj_sys,
             ls_tax, lt_tax, lt_tax[], lv_itemno,
             lt_return, lt_return[], lv_itemno.

      CLEAR ls_documentheader.
      ls_documentheader-bus_act     = 'RFBU'.
      ls_documentheader-username    = sy-uname.
      IF iv_okcode = 'BB_HAFTA'.
        ls_documentheader-header_txt  = 'BAŞ BAYİ KOMİSYONU'.
      ELSEIF iv_okcode = 'SB_HAFTA'.
        ls_documentheader-header_txt  = 'ELEKTRONIK PLATFORM KOM.'.
      ENDIF.

      ls_documentheader-comp_code   = gc_bukrs.
      ls_documentheader-doc_date    = lv_tar_bit.
      ls_documentheader-pstng_date  = lv_tar_bit.
      ls_documentheader-fisc_year   = ls_documentheader-doc_date(4).
      ls_documentheader-fis_period  = ls_documentheader-doc_date+4(2).
      ls_documentheader-doc_type    = gs_010-belge_turu.
*      ls_documentheader-ref_doc_no  = ' '.

      CLEAR: lv_wrbtr, lt_tax, lt_tax[], ls_tax.

      lv_wrbtr = it_secili-doc_amount.

      CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
        EXPORTING
          i_bukrs                 = gc_bukrs
          i_mwskz                 = gs_010-mwskz
          i_waers                 = 'TRY'
          i_wrbtr                 = lv_wrbtr
        TABLES
          t_mwdat                 = lt_tax
        EXCEPTIONS
          bukrs_not_found         = 1
          country_not_found       = 2
          mwskz_not_defined       = 3
          mwskz_not_valid         = 4
          account_not_found       = 5
          different_discount_base = 6
          different_tax_base      = 7
          txjcd_not_valid         = 8
          not_found               = 9
          ktosl_not_found         = 10
          kalsm_not_found         = 11
          parameter_error         = 12
          knumh_not_found         = 13
          kschl_not_found         = 14
          unknown_error           = 15
          OTHERS                  = 16.

      CHECK lt_tax[] IS NOT INITIAL.

      READ TABLE lt_tax INTO ls_tax INDEX 1.

      CLEAR: lt_accountreceivable.
      lv_itemno = lv_itemno + 1.
      lt_accountreceivable-itemno_acc = lv_itemno.
      IF iv_okcode = 'BB_HAFTA'.
        lt_accountreceivable-customer   = gs_010-kunnr.
      ELSEIF iv_okcode = 'SB_HAFTA'.
        lt_accountreceivable-customer   = gs_out-retailer_no.
      ENDIF.

      lt_accountreceivable-comp_code  = gc_bukrs.
      SELECT SINGLE zterm FROM knb1
               INTO lt_accountreceivable-pmnttrms
              WHERE bukrs = gc_bukrs
                AND kunnr = lt_accountreceivable-customer.
      APPEND lt_accountreceivable.

      CLEAR: lt_currencyamount.
      lt_currencyamount-itemno_acc = lv_itemno.
      lt_currencyamount-currency   = 'TRY'.
      lt_currencyamount-amt_doccur = gs_out-doc_amount.
      lt_currencyamount-amt_base   = gs_out-doc_amount.
      APPEND lt_currencyamount.

      CLEAR: lt_accountgl.
      lv_itemno = lv_itemno + 1.
      lt_accountgl-itemno_acc = lv_itemno.
      lt_accountgl-gl_account = gs_010-alacak_hesap.
      IF iv_okcode = 'BB_HAFTA'.
        CONCATENATE `BAŞ BAYİ KOMİSYONU `
                    gs_009-date_from+6(2) '.'
                    gs_009-date_from+4(2) '.'
                    gs_009-date_from+0(4) ` - `
                    lv_tar_bit+6(2)   '.'
                    lv_tar_bit+4(2)   '.'
                    lv_tar_bit+0(4)
               INTO lt_accountgl-item_text.

      ELSEIF iv_okcode = 'SB_HAFTA'.
        CONCATENATE `ELEKTRONIK PLATFORM KOM. `
                    gs_009-date_from+6(2) '.'
                    gs_009-date_from+4(2) '.'
                    gs_009-date_from+0(4) ` - `
                    lv_tar_bit+6(2)   '.'
                    lv_tar_bit+4(2)   '.'
                    lv_tar_bit+0(4)
               INTO lt_accountgl-item_text.

      ENDIF.

*      lt_accountgl-tax_code   = 'L3'.
*      lt_accountgl-profit_ctr = '2425000001'.
      lt_accountgl-profit_ctr        = gs_010-prctr.
      lt_accountgl-tax_code          = gs_010-mwskz.

      APPEND lt_accountgl.

      CLEAR: lt_currencyamount.
      lt_currencyamount-itemno_acc = lv_itemno.
      lt_currencyamount-currency   = 'TRY'.
      lt_currencyamount-amt_doccur = ls_tax-kawrt * -1.
      lt_currencyamount-amt_base   = ls_tax-kawrt * -1.
      APPEND lt_currencyamount.


      CLEAR: lt_accounttax.
      lv_itemno                = lv_itemno + 1.
      lt_accounttax-itemno_acc = lv_itemno.
*      SELECT SINGLE ktosl konts
*              FROM t030k
*              INTO (lt_accounttax-acct_key ,
*                    lt_accounttax-gl_account )
*             WHERE ktopl = lv_ktopl
*               AND ktosl = ls_tax-ktosl
*               AND mwskz = gt_item_fi-mwskz.
*      lt_accounttax-tax_code = 'L3'.
      lt_accounttax-tax_code = gs_010-mwskz.
      APPEND lt_accounttax.

      CLEAR: lt_currencyamount.
      lt_currencyamount-itemno_acc = lv_itemno.
      lt_currencyamount-currency   = 'TRY'.
      lt_currencyamount-amt_doccur = ls_tax-wmwst * -1.
      lt_currencyamount-amt_base   = ls_tax-kawrt * -1.
      APPEND lt_currencyamount.


      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader    = ls_documentheader
        IMPORTING
          obj_type          = lv_obj_type
          obj_key           = lv_obj_key
          obj_sys           = lv_obj_sys
        TABLES
          accountgl         = lt_accountgl
          accountreceivable = lt_accountreceivable
          accounttax        = lt_accounttax
          currencyamount    = lt_currencyamount
          return            = lt_return.
      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.

        CLEAR: ls_011, lt_011, lt_011[],
               lr_surec_tip_temp, lr_surec_tip_temp[].

        lr_surec_tip_temp-sign = 'I'.
        lr_surec_tip_temp-option = 'EQ'.

        IF iv_okcode = 'BB_HAFTA'.
          lr_surec_tip_temp-low = '10'.
          APPEND lr_surec_tip_temp.
          lr_surec_tip_temp-low = '15'.
          APPEND lr_surec_tip_temp.

          SELECT * FROM zsog_fi_011
            INTO CORRESPONDING FIELDS OF TABLE lt_011
           WHERE accounting_period_no EQ gs_out-accounting_period_no
             AND week_identifier      EQ gs_out-week_identifier
             AND date_from            EQ gs_out-date_from
             AND date_to              EQ gs_out-date_to
             AND surec_tipi           IN lr_surec_tip_temp.
          IF sy-subrc EQ 0.
            LOOP AT lt_011 INTO ls_011.
              ls_011-fatura           = 'X'.
              ls_011-fatura_belge_no  = lv_obj_key+0(10).
              ls_011-fatura_belge_yil = lv_obj_key+14(4).
              ls_011-fname            = lv_uname.
              ls_011-fdate            = lv_udate.
              ls_011-fzeit            = lv_uzeit.
              MODIFY lt_011 FROM ls_011.
            ENDLOOP.
          ENDIF.

        ENDIF.

        CLEAR: ls_011.
        ls_011-file_date             = gs_out-file_date.
        ls_011-accounting_period_no  = gs_009-accounting_period_no.
        ls_011-week_identifier       = gs_009-week_identifier.
        ls_011-date_from             = gs_009-date_from.
        ls_011-date_to               = gs_009-date_to.
        ls_011-surec_tipi            = gv_surec_tipi.
        ls_011-retailer_no           = gs_out-retailer_no.
        ls_011-bukrs                 = lv_obj_key+10(4).
        ls_011-belnr                 = lv_obj_key+0(10).
        ls_011-gjahr                 = lv_obj_key+14(4).
        ls_011-doc_amount            = gs_out-doc_amount.
        ls_011-uname                 = lv_uname.
        ls_011-udate                 = lv_udate.
        ls_011-uzeit                 = lv_uzeit.
        APPEND ls_011 TO lt_011.

        MODIFY zsog_fi_011 FROM TABLE lt_011.


        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        MESSAGE s255(zfi) WITH lv_obj_key+0(10)
                               lv_obj_key+10(4)
                               lv_obj_key+14(4)
                          INTO gv_dummy.

        PERFORM sys_add_bapiret2 TABLES gt_message.

        gs_out-icon  = icon_green_light.
        gs_out-bukrs = lv_obj_key+10(4).
        gs_out-gjahr = lv_obj_key+14(4).
        gs_out-belnr = lv_obj_key+0(10).
        MODIFY gt_out FROM gs_out INDEX lv_tabix.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        LOOP AT lt_return WHERE type CA 'EAX'.
          MESSAGE ID lt_return-id
                TYPE lt_return-type
              NUMBER lt_return-number
                WITH lt_return-message_v1
                     lt_return-message_v2
                     lt_return-message_v3
                     lt_return-message_v4
                INTO gv_dummy.
          PERFORM sys_add_bapiret2 TABLES gt_message.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " INV_DOC_CREATE
*&---------------------------------------------------------------------*
*&      Form  CHECK_INV_VALUE
*&---------------------------------------------------------------------*
FORM check_inv_value  TABLES it_secili STRUCTURE gt_secili
                       USING iv_okcode
                    CHANGING cv_error.
  CASE iv_okcode.
    WHEN 'BB_HAFTA' OR 'SB_HAFTA'.
      LOOP AT it_secili WHERE doc_amount LT 0.
      ENDLOOP.
      IF sy-subrc = 0.
        cv_error = 'X'.
        CLEAR: it_secili, it_secili[].
        MESSAGE 'Fatura tutarı sıfırdan küçük satır içermektedir!'
           TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
  ENDCASE.
ENDFORM.                    " CHECK_INV_VALUE
*&---------------------------------------------------------------------*
*&      Form  F_HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM f_handle_data_changed
  USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  DATA: ls_good        TYPE lvc_s_modi, "C
        lv_error       TYPE c,
        lv_doc_amount  LIKE gt_out-doc_amount,
        lv_type        TYPE dd01v-datatype. " numeric control.

  "C
  CLEAR lv_error.
  IF r3 <> abap_true.
    lv_error = 'X'.
  ENDIF.

  CHECK lv_error IS INITIAL.

  LOOP AT er_data_changed->mt_mod_cells INTO ls_good.
    CLEAR lv_error.

    CASE ls_good-fieldname.
      WHEN 'DOC_AMOUNT'.
        CLEAR: gs_out.
        READ TABLE gt_out INTO gs_out INDEX ls_good-row_id.
        IF sy-subrc = 0.
          "C
          CLEAR lv_doc_amount.

          CALL METHOD er_data_changed->get_cell_value
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
            IMPORTING
              e_value     = lv_doc_amount.

          gs_out-doc_amount = lv_doc_amount.

          MODIFY gt_out FROM gs_out INDEX ls_good-row_id TRANSPORTING
doc_amount.
          IF lv_error = 'X'.
            EXIT.
          ENDIF.
        ENDIF.


    ENDCASE.
  ENDLOOP.
  IF lv_error = 'X'.
    CALL METHOD er_data_changed->display_protocol.
  ENDIF.
  PERFORM refresh_table_display USING grid .

ENDFORM.                    "f_handle_data_changed
