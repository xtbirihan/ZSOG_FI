*&---------------------------------------------------------------------*
*&  Include           ZFI_I_PESIN_ODENEN_GID_FRM
*&---------------------------------------------------------------------*


FORM ws_filename_get USING filename LIKE rlgrap-filename .

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_path         = 'C:\'
      mask             = text-180
      mode             = 'O'
      title            = sy-title
    IMPORTING
      filename         = filename
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.


ENDFORM.                    " WS_FILENAME_GET

*&---------------------------------------------------------------------*
*&      Form  UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ITAB       text
*      -->FILENAME   text
*----------------------------------------------------------------------*
FORM upload USING itab TYPE STANDARD TABLE
                  filename LIKE rlgrap-filename .

  DATA f_name TYPE string.
  CLEAR f_name .
  f_name = filename .

  CLEAR itab .
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = f_name
*     FILETYPE                = 'ASC'
      has_field_separator     = 'X'
    TABLES
      data_tab                = itab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc IS NOT INITIAL .
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
  ENDIF .
ENDFORM.                    "UPLOAD

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_Exc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ITAB         text
*      -->FV_FILENAME  text
*----------------------------------------------------------------------*
FORM upload_exc USING itab TYPE STANDARD TABLE
                  fv_filename LIKE rlgrap-filename .

  DATA lv_fname TYPE localfile.
  DATA lt_raw TYPE truxs_t_text_data.
  DATA i_seperator TYPE char1 VALUE ''.

  lv_fname = fv_filename .

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_field_seperator    = i_seperator
      i_line_header        = 'X'
      i_tab_raw_data       = lt_raw
      i_filename           = lv_fname
    TABLES
      i_tab_converted_data = itab
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

ENDFORM.                    "UPLOAD_Exc

*&---------------------------------------------------------------------*
*&      Form  save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM save .

  PERFORM taksit_tablosu_olustur .

  LOOP AT selrows .

    LOOP AT gt_data FROM selrows-index .
      IF gt_data-awkey IS NOT INITIAL .
        MESSAGE 'Kayıt önceden yaratıldı' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT .
      ENDIF .
      PERFORM post_document CHANGING gt_data-awkey gt_data-message .
      IF gt_data-awkey IS INITIAL .
        gt_data-rowcolor = color_light_red .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      ELSE .
        IF virman_tutari IS NOT INITIAL .
          " tfrs için z1 defterine 280 --> 180 virmanı
*          PERFORM virman_kaydi USING virman_tutari
*                               CHANGING gt_data-virman_awkey
*                                        gt_data-virman_message .
*          IF gt_data-virman_awkey IS INITIAL .
*            gt_data-rowcolor = color_light_red .
*            CLEAR : gt_data-awkey .
*            gt_data-message =
*            'Virman Kaydı yaratılamadığından geri alındı'.
*            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
**          ELSE .
*            gt_data-rowcolor = color_light_green .
*            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*              EXPORTING
*                wait = 'X'.
*          ENDIF .
        ELSE .
          gt_data-rowcolor = color_light_green .
          gt_data-virman_message = 'Virman için tutar oluşmadı' .
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF .
      ENDIF .

      IF gt_data-kursf IS INITIAL.
        SELECT SINGLE kursf FROM bkpf
          INTO gt_data-kursf
          WHERE awkey EQ gt_data-awkey.
      ENDIF.

      it_log-sayac = gt_data-counter .
      it_log-lifnr = gt_data-lifnr .
      it_log-kunnr = gt_data-kunnr.
      it_log-budat = gt_data-tarih .
      it_log-awkey = gt_data-awkey .
      it_log-message = gt_data-message .
      it_log-virman_awkey   = gt_data-virman_awkey   .
      it_log-virman_message = gt_data-virman_message .
      APPEND it_log .
      IF gt_data-awkey IS NOT INITIAL .
        IF gt_data-belgeno IS INITIAL .
          PERFORM number_next CHANGING gt_data-belgeno .
        ENDIF .
      ENDIF .
      MODIFY gt_data .

      IF gt_data-awkey IS NOT INITIAL .
        CLEAR : zfi_pes_gid_bas .
        MOVE-CORRESPONDING gt_data TO zfi_pes_gid_bas .
        zfi_pes_gid_bas-bukrs = bkpf-bukrs .
        zfi_pes_gid_bas-waers = bkpf-waers .
        zfi_pes_gid_bas-aufnr = gt_data-aufnr.
*{   ->>> Inserted by Prodea Ozan Şahin - 20.04.2020 10:57:14
        IF bkpf-blart = 'KG'.
          zfi_pes_gid_bas-tutar = -1 * zfi_pes_gid_bas-tutar.
        ENDIF.
*}     <<<- End of   Inserted - 20.04.2020 10:57:14
*        zfi_pes_gid_bas-aufnr = gt_data-license_num .
*        IF gt_data-license_num IS NOT INITIAL AND
*           gt_data-license_num(2) NE '00' .
*          CONCATENATE '00' zfi_pes_gid_bas-aufnr
*            INTO zfi_pes_gid_bas-aufnr .
*        ENDIF .

        MODIFY zfi_pes_gid_bas .
        IF sy-subrc IS NOT INITIAL .
          MESSAGE i020(zfi) DISPLAY LIKE 'E' .
        ELSE .
          DELETE FROM zfi_pes_gid_kal WHERE belgeno EQ gt_data-belgeno .
          COMMIT WORK AND WAIT .
          LOOP AT gt_taksit_tum WHERE sayac EQ gt_data-counter AND
                                      kalem_tipi EQ 'S'.
            CLEAR : zfi_pes_gid_kal .
            zfi_pes_gid_kal-belgeno   = zfi_pes_gid_bas-belgeno .
            zfi_pes_gid_kal-spmon     = gt_taksit_tum-valor(6) .
            zfi_pes_gid_kal-taksitno  = gt_taksit_tum-buzei .
            zfi_pes_gid_kal-hkont     = gt_taksit_tum-hesap .
            zfi_pes_gid_kal-kostl     = gt_taksit_tum-kostl .
            zfi_pes_gid_kal-gsber     = gt_taksit_tum-gsber .
            zfi_pes_gid_kal-aufnr     = gt_taksit_tum-aufnr .
            zfi_pes_gid_kal-gun       = gt_taksit_tum-gun .
            zfi_pes_gid_kal-tutar     = gt_taksit_tum-tutar .
            zfi_pes_gid_kal-valor     = gt_taksit_tum-valor .

*{   ->>> Inserted by Prodea Ozan Şahin - 20.04.2020 10:57:14
            IF bkpf-blart = 'KG'.
              zfi_pes_gid_kal-tutar = -1 * zfi_pes_gid_kal-tutar.
            ENDIF.
*}     <<<- End of   Inserted - 20.04.2020 10:57:14

            MODIFY zfi_pes_gid_kal .
            COMMIT WORK AND WAIT .
          ENDLOOP .
        ENDIF .
      ENDIF .

      EXIT .
    ENDLOOP .
  ENDLOOP .

ENDFORM .                    "save

*&---------------------------------------------------------------------*
*&      Form  number_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FV_BELGENO text
*----------------------------------------------------------------------*
FORM number_next CHANGING fv_belgeno LIKE zfi_pes_gid_bas-belgeno .

  DATA : lv_number TYPE num9 .

  CLEAR : lv_number .
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZPESING'
    IMPORTING
      number                  = lv_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  fv_belgeno = lv_number . CONDENSE fv_belgeno .
  CONCATENATE 'P' fv_belgeno INTO fv_belgeno .

ENDFORM .                    "number_next

*&---------------------------------------------------------------------*
*&      Form  virman_kaydi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FV_VIRMAN_TUTARI  text
*      -->FV_AWKEY          text
*      -->FV_MESSAGE        text
*----------------------------------------------------------------------*
FORM virman_kaydi USING fv_virman_tutari TYPE dmbtr
                  CHANGING fv_awkey LIKE bkpf-awkey
                           fv_message LIKE bapiret2-message .

  CLEAR : it_return[] , it_return , wa_bkpf , it_bseg , it_bseg[] .

  wa_bkpf-bukrs = bkpf-bukrs .
  wa_bkpf-ldgrp = 'Z1' .
  wa_bkpf-usnam = sy-uname .
  wa_bkpf-budat = gt_data-tarih .
  wa_bkpf-bldat = gt_data-tarih .
  wa_bkpf-wwert = gt_data-tarih .
  wa_bkpf-blart = bkpf-blart .
  wa_bkpf-xblnr = gt_data-policeno .
  wa_bkpf-kursf = gt_data-kursf .
  wa_bkpf-waers = bkpf-waers .

  it_bseg-hkont = gt_data-hkont .
  it_bseg-wrbtr = fv_virman_tutari .
  it_bseg-gsber = gt_data-gsber .
  APPEND it_bseg .

  CLEAR it_bseg .
  it_bseg-hkont = gt_data-hkont_uzun .
  it_bseg-wrbtr = fv_virman_tutari * -1 .
  it_bseg-gsber = gt_data-gsber .
  APPEND it_bseg .

  CALL FUNCTION 'ZFI_DOCUMENT_POST'
    EXPORTING
      bkpf           = wa_bkpf
      commit         = ''
    IMPORTING
      e_key          = fv_awkey
    TABLES
      bseg           = it_bseg[]
      return         = it_return[]
    EXCEPTIONS
      dogrudan_kayit = 1
      OTHERS         = 2.
  IF fv_awkey IS INITIAL OR fv_awkey EQ '$' .
    CLEAR fv_awkey .
  ENDIF .

  LOOP AT it_return WHERE type EQ 'A' OR type EQ 'E' .
    CHECK NOT ( it_return-type EQ 'E' AND it_return-id EQ 'RW'
                AND it_return-number EQ '609' ) .
    CONCATENATE fv_message it_return-message ',' INTO fv_message.
  ENDLOOP .

ENDFORM .                    "virman_kaydi
*&---------------------------------------------------------------------*
*&      Form  post_document
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FV_AWKEY   text
*      -->FV_MESSAGE text
*----------------------------------------------------------------------*
FORM post_document CHANGING fv_awkey LIKE bkpf-awkey
                            fv_message LIKE bapiret2-message .

  DATA : it_accountreceivable LIKE TABLE OF bapiacar09 WITH HEADER LINE.

  CLEAR : documentheader , it_currencyamount , it_currencyamount[] ,
          it_accountgl , it_accountgl[] , it_accountpayable ,
          it_accountpayable[] , it_accounttax , it_accounttax[] ,
          it_return , it_return[] , ktosl , virman_tutari ,
          it_accountreceivable[] , it_accountwt[] , it_accountwt,
          it_accountreceivable.

*  IF gt_data-mwskz IS NOT INITIAL .
*
*    CALL FUNCTION 'RE_GET_TAXCODE_INFO_KTOSL'
*      EXPORTING
*        i_mwskz = gt_data-mwskz
*        i_bukrs = bkpf-bukrs
*      IMPORTING
*        e_ktosl = ktosl.
*  ENDIF .

  documentheader-comp_code = bkpf-bukrs .
  documentheader-username = sy-uname .
  documentheader-doc_date = gt_data-tarih .
  documentheader-pstng_date = gt_data-tarih.
  documentheader-trans_date = gt_data-tarih.
  documentheader-fisc_year  = gt_data-tarih(4) .
  documentheader-fis_period = gt_data-tarih+4(2) .
  documentheader-doc_type = bkpf-blart .
  documentheader-ref_doc_no = gt_data-policeno .

  LOOP AT gt_taksit_tum WHERE policeno EQ gt_data-policeno .
    CASE gt_taksit_tum-kalem_tipi .
      WHEN 'S' . " Ana hesap kalemi
        CLEAR : it_accountgl , it_currencyamount .
*{   ->>> Inserted by Prodea Ozan Şahin - 20.04.2020 10:57:14
        IF bkpf-blart = 'KG'.
          gt_taksit_tum-tutar = -1 * gt_taksit_tum-tutar.
        ENDIF.
*}     <<<- End of   Inserted - 20.04.2020 10:57:14
        IF gt_data-mwskz EQ 'S3' AND ( gt_taksit_tum-hesap+0(3) EQ '191'
                                  OR gt_taksit_tum-hesap+0(3) EQ '360' )
                                  .
          it_accounttax-itemno_acc = gt_taksit_tum-buzei .
          it_accounttax-gl_account = gt_taksit_tum-hesap .
          IF gt_taksit_tum-hesap+0(3) = '191'.
            it_accounttax-acct_key = 'VST' .
          ELSEIF gt_taksit_tum-hesap+0(3) = '360'.
            it_accounttax-acct_key = 'ZST' .
          ELSE.
            it_accounttax-acct_key = ktosl .
          ENDIF.
          it_accounttax-tax_code = gt_data-mwskz .

          APPEND it_accounttax .

          it_currencyamount-itemno_acc = gt_taksit_tum-buzei .
          it_currencyamount-curr_type  = '00'  .
          it_currencyamount-currency   = bkpf-waers .
          it_currencyamount-exch_rate = gt_data-kursf .
          it_currencyamount-amt_doccur = gt_taksit_tum-tutar .
          IF gt_data-mwskz EQ 'S3' AND  ( gt_taksit_tum-hesap+0(3) EQ
          '191'
                                  OR gt_taksit_tum-hesap+0(3) EQ '360' )
                                  .
            IF  gt_taksit_tum-hesap+0(3) = '360'.
              it_currencyamount-amt_doccur = gt_taksit_tum-tutar  * -1.
            ELSE.
              it_currencyamount-amt_doccur = gt_taksit_tum-tutar  .
            ENDIF.
            it_currencyamount-amt_base = gt_taksit_tum-tutar * 100 / 18.
          ENDIF.
          APPEND it_currencyamount .

          IF gt_taksit_tum-buzei LE 12 AND
             gt_taksit_tum-hesap EQ gt_data-hkont_uzun .
            ADD gt_taksit_tum-tutar TO virman_tutari .
          ENDIF .
        ELSE.
          it_accountgl-itemno_acc = gt_taksit_tum-buzei .
          it_accountgl-gl_account = gt_taksit_tum-hesap .
          it_accountgl-costcenter = gt_taksit_tum-kostl .
          it_accountgl-bus_area   = gt_taksit_tum-gsber .
          it_accountgl-material = gt_data-matnr .
          it_accountgl-base_uom = gt_data-meins .
          it_accountgl-quantity = gt_data-menge .
*          it_accountgl-ref_key_3  = gt_data-chassis_num .
          it_accountgl-orderid = gt_taksit_tum-aufnr .
          it_accountgl-value_date = gt_taksit_tum-valor .
          it_accountgl-item_text  = gt_data-sgtxt .
*       it_accountgl-tax_code   = GT_DATA-MWSKZ .
*IF gt_data-mwskz EQ 'S3' AND gt_taksit_tum-hesap+0(3) EQ '191'.
          it_accountgl-tax_code = gt_data-mwskz .
*          ENDIF.
* begin of insert Ali Y. Abbasgil 14.06.2013 15:34:52
*          IF gt_data-mwskz EQ 'S3'.
*            it_accountgl-tax_code = gt_data-mwskz .
*          ENDIF.
* end of insert
          APPEND it_accountgl .

          it_currencyamount-itemno_acc = gt_taksit_tum-buzei .
          it_currencyamount-curr_type  = '00' .
          it_currencyamount-currency   = bkpf-waers .
          it_currencyamount-exch_rate = gt_data-kursf .
          it_currencyamount-amt_doccur = gt_taksit_tum-tutar .
*IF gt_data-mwskz EQ 'S3' AND  gt_taksit_tum-hesap+0(3) EQ '191' .
*            it_currencyamount-amt_doccur = gt_taksit_tum-tutar * -1 .
**          it_currencyamount-AMT_BASE = gt_taksit_tum-tutar * 100 / 18.
*          ENDIF.
          APPEND it_currencyamount .

          IF gt_taksit_tum-buzei LE 12 AND
             gt_taksit_tum-hesap EQ gt_data-hkont_uzun .
            ADD gt_taksit_tum-tutar TO virman_tutari .
          ENDIF .
        ENDIF.
*        IF GT_DATA-MWSKZ eq 'S3' and gt_taksit_tum-hesap+0(3) eq '191'.
*          it_accounttax-itemno_acc = gt_taksit_tum-buzei .
*          it_accounttax-tax_code   = gt_data-mwskz .
*          it_accounttax-gl_account = gt_taksit_tum-hesap .
*          it_accounttax-acct_key = 'VST'."ktosl .
*          APPEND it_accounttax .

*    it_accountreceivable-itemno_acc = gt_taksit_tum-buzei .
*    it_accountreceivable-customer = gt_taksit_tum-hesap .
**    it_accountreceivable-item_text = 'Vade farkı faturası' .
*    it_accountreceivable-bus_area = gt_taksit_tum-gsber .
*    it_accountreceivable-tax_code = ktosl .
*    APPEND it_accountreceivable .
*        endif.

      WHEN 'V' . " vergi kalemi
        CLEAR : it_accounttax , it_currencyamount  .

        it_accounttax-itemno_acc = gt_taksit_tum-buzei .
        it_accounttax-acct_key = gt_taksit_tum-ktosl .
        it_accounttax-tax_code = gt_data-mwskz .

        APPEND it_accounttax .

        it_currencyamount-itemno_acc = gt_taksit_tum-buzei .
        it_currencyamount-curr_type  = '00'.
        it_currencyamount-currency   = bkpf-waers .
        it_currencyamount-exch_rate = gt_data-kursf .
        it_currencyamount-amt_doccur = gt_taksit_tum-tutar .
        it_currencyamount-amt_base = gt_taksit_tum-matrah .
        APPEND it_currencyamount .

      WHEN 'K' . " satıcı kalemi
*{   ->>> Inserted by Prodea Ozan Şahin - 20.04.2020 10:57:14
        IF bkpf-blart = 'KG'.
          gt_taksit_tum-tutar = -1 * gt_taksit_tum-tutar.
        ENDIF.
*}     <<<- End of   Inserted - 20.04.2020 10:57:14
        IF gt_data-tarih EQ '20140331'.
          CLEAR : it_accountgl , it_currencyamount .

          it_accountgl-itemno_acc = gt_taksit_tum-buzei .
*          it_accountgl-gl_account = gt_taksit_tum-hesap .
          it_accountgl-gl_account = '8999000001' .
          it_accountgl-costcenter = gt_taksit_tum-kostl .
          it_accountgl-item_text  = gt_data-sgtxt .

          APPEND it_accountgl .

        ELSE.

          CLEAR : it_accountpayable , it_currencyamount .
          READ TABLE it_accountpayable TRANSPORTING
           NO FIELDS WITH KEY  vendor_no = gt_data-lifnr.
          IF sy-subrc <> 0.
            it_accountpayable-itemno_acc = gt_taksit_tum-buzei .
            it_accountpayable-vendor_no = gt_data-lifnr .
            it_accountpayable-bus_area = gt_taksit_tum-gsber .
            it_accountpayable-item_text = gt_data-sgtxt .
            it_accountpayable-bline_date = gt_taksit_tum-valor .

            CLEAR gt_data-zterm.
            SELECT SINGLE zterm FROM lfb1
                INTO gt_data-zterm
              WHERE lifnr EQ gt_data-lifnr .
            it_accountpayable-pmnttrms   = gt_data-zterm.

*        it_accountpayable-ref_key_3  = gt_data-chassis_num .

            APPEND it_accountpayable .
          ENDIF.
        ENDIF .

        it_currencyamount-itemno_acc = gt_taksit_tum-buzei .
        it_currencyamount-curr_type = '00'.
        it_currencyamount-currency = bkpf-waers .
        it_currencyamount-exch_rate = gt_data-kursf .
        it_currencyamount-amt_doccur = gt_taksit_tum-tutar .
        APPEND it_currencyamount .
        READ TABLE it_accountpayable TRANSPORTING
          NO FIELDS WITH KEY  vendor_no = gt_data-lifnr.
        IF sy-subrc <> 0.

          CLEAR : it_lfbw , it_lfbw[] .
          SELECT * FROM lfbw INTO TABLE it_lfbw
              WHERE bukrs EQ bkpf-bukrs AND
                    lifnr EQ gt_data-lifnr .
          LOOP AT it_lfbw .
            CLEAR : it_accountwt .
            it_accountwt-itemno_acc = gt_taksit_tum-buzei .
            it_accountwt-wt_type = it_lfbw-witht .
            it_accountwt-wt_code = it_lfbw-wt_withcd .
            APPEND it_accountwt .
          ENDLOOP .
        ENDIF.

      WHEN 'D' . " müşteri kalemi
*{   ->>> Inserted by Prodea Ozan Şahin - 20.04.2020 10:57:14
        IF bkpf-blart = 'KG'.
          gt_taksit_tum-tutar = -1 * gt_taksit_tum-tutar.
        ENDIF.
*}     <<<- End of   Inserted - 20.04.2020 10:57:14
        IF gt_data-tarih EQ '20140331'.
          CLEAR : it_accountgl , it_currencyamount .

          it_accountgl-itemno_acc = gt_taksit_tum-buzei .
*          it_accountgl-gl_account = gt_taksit_tum-hesap .
          it_accountgl-gl_account = '8999000001' .
          it_accountgl-costcenter = gt_taksit_tum-kostl .
          it_accountgl-item_text  = gt_data-sgtxt .

          APPEND it_accountgl .

        ELSE.

          CLEAR :  it_accountreceivable , it_currencyamount .
          READ TABLE  it_accountreceivable TRANSPORTING
           NO FIELDS WITH KEY  customer = gt_data-kunnr.
          IF sy-subrc <> 0.
            it_accountreceivable-itemno_acc = gt_taksit_tum-buzei .
            it_accountreceivable-customer = gt_data-kunnr .
            it_accountreceivable-bus_area = gt_taksit_tum-gsber .
            it_accountreceivable-item_text = gt_data-sgtxt .
            it_accountreceivable-bline_date = gt_taksit_tum-valor .
*        it_accountpayable-ref_key_3  = gt_data-chassis_num .

            APPEND it_accountreceivable.
          ENDIF.
        ENDIF .

        it_currencyamount-itemno_acc = gt_taksit_tum-buzei .
        it_currencyamount-curr_type = '00'.
        it_currencyamount-currency = bkpf-waers .
        it_currencyamount-exch_rate = gt_data-kursf .
        it_currencyamount-amt_doccur = gt_taksit_tum-tutar .
        APPEND it_currencyamount .



      WHEN 'A'.

*         IF gt_data-tarih EQ '20140331'.
*          CLEAR : it_accountgl , it_currencyamount .
*
*          it_accountgl-itemno_acc = gt_taksit_tum-buzei .
**          it_accountgl-gl_account = gt_taksit_tum-hesap .
*          it_accountgl-gl_account = '8999000001' .
*          it_accountgl-costcenter = gt_taksit_tum-kostl .
*          it_accountgl-item_text  = gt_data-sgtxt .
*
*          APPEND it_accountgl .
*
*        ELSE.

        CLEAR : it_accountpayable , it_currencyamount .

        it_accountpayable-itemno_acc = gt_taksit_tum-buzei .
        it_accountpayable-vendor_no = gt_data-lifnr .
        it_accountpayable-gl_account = gt_data-anahesap.
        it_accountpayable-bus_area = gt_taksit_tum-gsber .
        it_accountpayable-item_text = gt_data-sgtxt .
        it_accountpayable-bline_date = gt_taksit_tum-valor .

*        it_accountpayable-ref_key_3  = gt_data-chassis_num .

        APPEND it_accountpayable .
*        ENDIF .

        it_currencyamount-itemno_acc = gt_taksit_tum-buzei .
        it_currencyamount-curr_type = '00'.
        it_currencyamount-currency = bkpf-waers .
        it_currencyamount-exch_rate = gt_data-kursf .
        it_currencyamount-amt_doccur = gt_taksit_tum-tutar .
        APPEND it_currencyamount .

*        CLEAR : it_lfbw , it_lfbw[] .
*        SELECT * FROM lfbw INTO TABLE it_lfbw
*            WHERE bukrs EQ bkpf-bukrs AND
*                  lifnr EQ gt_data-lifnr .
*        LOOP AT it_lfbw .
*          CLEAR : it_accountwt .
*          it_accountwt-itemno_acc = gt_taksit_tum-buzei .
*          it_accountwt-wt_type = it_lfbw-witht .
*          it_accountwt-wt_code = it_lfbw-wt_withcd .
*          APPEND it_accountwt .
*        ENDLOOP .


    ENDCASE .
  ENDLOOP .

  CLEAR : fv_awkey , fv_message .
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader    = documentheader
    IMPORTING
      obj_key           = fv_awkey
    TABLES
      accountgl         = it_accountgl[]
      accountpayable    = it_accountpayable[]
      accountreceivable = it_accountreceivable
      accounttax        = it_accounttax[]
      currencyamount    = it_currencyamount[]
      accountwt         = it_accountwt[]
      return            = it_return[].
  IF fv_awkey EQ '$' .
    CLEAR fv_awkey .
  ENDIF .
  IF fv_awkey IS NOT INITIAL .
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*     EXPORTING
*       WAIT          = 'X' .
*    fv_message = 'Belge başarıyla yaratıldı' .
  ELSE .
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    LOOP AT it_return WHERE type EQ 'A' OR type EQ 'E' .
      CHECK NOT ( it_return-type EQ 'E' AND it_return-id EQ 'RW'
                  AND it_return-number EQ '609' ) .
      CONCATENATE fv_message it_return-message ',' INTO fv_message.
    ENDLOOP .
  ENDIF .
ENDFORM .                    "post_document

*&---------------------------------------------------------------------*
*&      Form  taksit_tablosu_olustur
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM taksit_tablosu_olustur .

  CLEAR : lv_index ,
"gt_taksit_tum , gt_taksit_tum[] ,

         gt_taksit_tum , gt_taksit_tum[] ,"added by ozans 21.04.2020
          selrows ,
          selrows[] , it_log , it_log[] .

  CALL METHOD alvgrid->check_changed_data
    IMPORTING
      e_valid = e_valid.

  CALL METHOD alvgrid->get_selected_rows
    IMPORTING
      et_index_rows = selrows[].
*     et_row_no     = .

  PERFORM get_masraf_hesap. " Added By YigitY/PRODEA 15.02.2016

  LOOP AT selrows .

    LOOP AT gt_data FROM selrows-index .

      " Added By YigitY/PRODEA 15.02.2016
      PERFORM masraf_siparis_kontrol USING gt_data-hkont_gider
                                           gt_data-kostl
                                           gt_data-aufnr
                                           gt_data-gsber.

      IF gt_data-bktxt IS INITIAL .
        MESSAGE e032(zco).
      ENDIF.

      "WHERE vade_bas is not INITIAL and
      "  vade_bit is not INITIAL .
      ADD 1 TO lv_index .
      CLEAR : ls_taksit , gt_taksit , gt_taksit[] , lv_begda , lv_endda
      ,
              itemno , begyear , toplamgun , kalantutar , vergihesabi ,
              vergitutari , matrah , it_vergi , it_vergi[] .

      IF gt_data-policeno IS INITIAL .
        MESSAGE 'Simülasyon çalıştırmak için poliçe numarası giriniz'
          TYPE 'I' DISPLAY LIKE 'E' .
        EXIT .
      ENDIF .
*******ergin:masraf yeri kontrolü
*      IF gt_data-kostl IS INITIAL .
*        MESSAGE 'Poliçe girişlerinde Masraf Yeri zorunludur.'
*          TYPE 'I' DISPLAY LIKE 'E' .
*        EXIT .
*      ENDIF .
*******ergin:masraf yeri kontrolü

*******abtaydin anahesap/satıcı kontrolü
      IF  gt_data-anahesap IS NOT INITIAL AND
          gt_data-lifnr    IS NOT INITIAL AND
          gt_data-kunnr    IS NOT INITIAL.
        MESSAGE 'Müşteri,Satıcı ve anahesap aynı anda girilemez!'
           TYPE 'I' DISPLAY LIKE 'E' .
        EXIT .
      ENDIF.
*******abtaydin anahesap/satıcı kontrolü

      CLEAR : gt_taksit_tum .
      READ TABLE gt_taksit_tum WITH KEY policeno = gt_data-policeno .
      IF sy-subrc IS INITIAL .
        EXIT .
      ENDIF .

      lv_begda = gt_data-vade_bas .
      lv_endda = gt_data-vade_bit .
      begyear = lv_begda(4) .
      kalantutar = gt_data-tutar .

      PERFORM get_last_day CHANGING lv_begda .
      PERFORM get_last_day CHANGING lv_endda .
      PERFORM get_tax USING gt_data-mwskz
                      CHANGING kalantutar vergitutari vergihesabi.
      matrah = kalantutar .
      WHILE lv_begda LE lv_endda .
        CLEAR : gt_taksit .
        ADD 1 TO itemno .
        gt_taksit-buzei = itemno .
        gt_taksit-hesap = gt_data-hkont .
        gt_taksit-gsber = gt_data-gsber .
        gt_taksit-aufnr = gt_data-aufnr .


        IF lv_begda(4) NE begyear .
          gt_taksit-hesap = gt_data-hkont_uzun .
        ENDIF .

        IF  itemno >= 0 AND itemno =< 99.
          gt_taksit-tkstx = itemno+1(2) .
        ELSE.
          gt_taksit-tkstx = itemno.
        ENDIF.
        CONDENSE gt_taksit-tkstx .
        CONCATENATE gt_taksit-tkstx '.' ' Taksit' INTO gt_taksit-tkstx.

        CLEAR : it_skat .
        READ TABLE it_skat WITH KEY saknr = gt_taksit-hesap BINARY
        SEARCH.
        IF sy-subrc IS INITIAL .
          gt_taksit-hestx = it_skat-txt20 .
        ENDIF .

        IF lv_begda(6) EQ gt_data-vade_bas(6) .
          gt_taksit-gun = lv_begda - gt_data-vade_bas + 1 .
        ELSEIF lv_begda EQ lv_endda .
          gt_taksit-gun = gt_data-vade_bit+6(2) .
        ELSE .
          gt_taksit-gun = lv_begda+6(2) .
        ENDIF .
        gt_taksit-kostl = gt_data-kostl .
        gt_taksit-valor = lv_begda .
        ADD gt_taksit-gun TO toplamgun .
        gt_taksit-kalem_tipi = 'S' . " ana hesap kalemi
        APPEND gt_taksit .
        PERFORM add_month CHANGING lv_begda .
      ENDWHILE .

      LOOP AT gt_taksit .
        IF gt_taksit-buzei NE itemno .
          gt_taksit-tutar = matrah * gt_taksit-gun / toplamgun .
          SUBTRACT gt_taksit-tutar FROM kalantutar .
        ELSE .
          gt_taksit-tutar = kalantutar .
        ENDIF .
        MODIFY gt_taksit TRANSPORTING tutar .
      ENDLOOP .

      " vergi kalemi
      IF gt_data-mwskz IS NOT INITIAL .
        "" tevkifat düzenleme
        LOOP AT it_vergi .
          ADD 1 TO itemno .
          CLEAR : gt_taksit .
          gt_taksit-buzei = itemno .
          gt_taksit-hesap = it_vergi-vergihesabi .
          CLEAR : it_skat .
          READ TABLE it_skat WITH KEY saknr = gt_taksit-hesap
                                                BINARY SEARCH.
          IF sy-subrc IS INITIAL .
            gt_taksit-hestx = it_skat-txt20 .
          ENDIF .
          gt_taksit-tutar = it_vergi-vergitutari .
          gt_taksit-kalem_tipi = 'V' . " Vergi Kalemi
          gt_taksit-matrah = matrah  .
          gt_taksit-ktosl = it_vergi-ktosl .
          APPEND gt_taksit .
        ENDLOOP .

*        ADD 1 TO itemno .
*        CLEAR : gt_taksit .
*        gt_taksit-buzei = itemno .
*        gt_taksit-hesap = vergihesabi .
*        CLEAR : it_skat .
*        READ TABLE it_skat WITH KEY saknr = gt_taksit-hesap BINARY
*        SEARCH.
*        IF sy-subrc IS INITIAL .
*          gt_taksit-hestx = it_skat-txt20 .
*        ENDIF .
*        gt_taksit-tutar = vergitutari .
*        gt_taksit-kalem_tipi = 'V' . " Vergi Kalemi
*        gt_taksit-matrah = matrah  .
*        APPEND gt_taksit .
      ENDIF .


      CLEAR : gt_taksit .

      " satıcı kalemi
      IF gt_data-lifnr IS NOT INITIAL.
        gt_taksit-hesap = gt_data-lifnr .
        gt_taksit-gsber = gt_data-gsber .
        SELECT SINGLE name1 FROM lfa1
          INTO gt_taksit-hestx
          WHERE lifnr EQ gt_data-lifnr .
        gt_taksit-kalem_tipi = 'K' . " satıcı kalemi

        IF gt_data-taksit IS INITIAL .
          gt_data-taksit = 1 .
        ENDIF .
        saticitutar = gt_data-tutar .
        DO gt_data-taksit TIMES .
          ADD 1 TO itemno .
          gt_taksit-buzei = itemno .
          IF gt_data-taksit EQ sy-index .
            gt_taksit-tutar = -1 * saticitutar .
            APPEND gt_taksit .
            CONTINUE .
          ENDIF .
          gt_taksit-tutar = ( gt_data-tutar * -1 ) / gt_data-taksit.
          ADD gt_taksit-tutar TO saticitutar .
          APPEND gt_taksit .
        ENDDO .


      ELSEIF gt_data-kunnr    IS NOT INITIAL.

        " müşteri kalemi
        CLEAR : gt_taksit .

        gt_taksit-hesap = gt_data-kunnr .
        gt_taksit-gsber = gt_data-gsber .
        SELECT SINGLE name1 FROM kna1
          INTO gt_taksit-hestx
          WHERE kunnr EQ gt_data-kunnr .
        gt_taksit-kalem_tipi = 'D' . " satıcı kalemi

        IF gt_data-taksit IS INITIAL .
          gt_data-taksit = 1 .
        ENDIF .
        musteritutar = gt_data-tutar .
        DO gt_data-taksit TIMES .
          ADD 1 TO itemno .
          gt_taksit-buzei = itemno .
          IF gt_data-taksit EQ sy-index .
            gt_taksit-tutar = -1 * musteritutar .
            gt_taksit-policeno = gt_data-policeno .
            APPEND gt_taksit .
            CONTINUE .
          ENDIF .
          gt_taksit-tutar = ( gt_data-tutar * -1 ) / gt_data-taksit.
          ADD gt_taksit-tutar TO musteritutar .
          gt_taksit-policeno = gt_data-policeno .
          APPEND gt_taksit .
        ENDDO .
        "anahesap kalemi eklendi
      ELSEIF gt_data-anahesap IS NOT INITIAL.
        gt_taksit-hesap = gt_data-anahesap .
        gt_taksit-gsber = gt_data-gsber .

        SELECT SINGLE txt20
            INTO gt_taksit-hestx
         FROM skat
         INNER JOIN t001 ON
         t001~ktopl = skat~ktopl
         WHERE skat~spras EQ sy-langu   AND
               t001~bukrs EQ bkpf-bukrs AND
               skat~saknr EQ gt_data-anahesap.

        gt_taksit-kalem_tipi = 'A' .

        IF gt_data-taksit IS INITIAL .
          gt_data-taksit = 1 .
        ENDIF .
        anahesaptutar = gt_data-tutar .
        DO gt_data-taksit TIMES .
          ADD 1 TO itemno .
          gt_taksit-buzei = itemno .
          IF gt_data-taksit EQ sy-index .
            gt_taksit-tutar = -1 * anahesaptutar .
            APPEND gt_taksit .
            CONTINUE .
          ENDIF .
          gt_taksit-tutar = ( gt_data-tutar * -1 ) / gt_data-taksit.
          ADD gt_taksit-tutar TO anahesaptutar .
          APPEND gt_taksit .
        ENDDO .
      ENDIF.
      "anahesap kalemi eklendi

      "borc & alacak *Mehmet Sertçelik
      IF gt_data-mwskz EQ 'S3'.
        LOOP AT t_mwdat.
          CLEAR : gt_taksit .
          ADD 1 TO itemno .
          gt_taksit-buzei = itemno .
          gt_taksit-hesap = t_mwdat-hkont.
          gt_taksit-tutar = t_mwdat-kawrt * ( 18 / 100 ).
          gt_taksit-kalem_tipi = 'S' .
          APPEND gt_taksit .
        ENDLOOP.
      ENDIF.

      LOOP AT gt_taksit .
        gt_taksit_tum = gt_taksit .
        gt_taksit_tum-sayac = lv_index .
        gt_taksit_tum-policeno = gt_data-policeno .
        APPEND gt_taksit_tum .
      ENDLOOP .
      gt_data-counter = lv_index .
      MODIFY gt_data .
      EXIT .
    ENDLOOP .

  ENDLOOP .

  LOOP AT gt_taksit WHERE kalem_tipi NE 'K' .
    gt_taksit-field_style = it_style_kapali[] .
    MODIFY gt_taksit.
  ENDLOOP .

  LOOP AT gt_taksit_tum WHERE kalem_tipi NE 'K' .
    gt_taksit_tum-field_style = it_style_kapali[] .
    MODIFY gt_taksit_tum .
  ENDLOOP .

ENDFORM .                    "taksit_tablosu_olustur

*&---------------------------------------------------------------------*
*&      Form  refresh_taksit_tablo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM refresh_taksit_tablo .

  CLEAR : gt_taksit , gt_taksit[] .

  LOOP AT it_policeno .

    LOOP AT gt_data WHERE policeno = it_policeno-policeno .

      ADD 1 TO lv_index .
      CLEAR : ls_taksit  , lv_begda , lv_endda ,
              itemno , begyear , toplamgun , kalantutar , vergihesabi ,
              vergitutari , matrah , it_vergi , it_vergi[] .

      DELETE gt_taksit_tum WHERE policeno = gt_data-policeno .

      lv_begda = gt_data-vade_bas .
      lv_endda = gt_data-vade_bit .
      begyear = lv_begda(4) .
      kalantutar = gt_data-tutar .

      PERFORM get_last_day CHANGING lv_begda .
      PERFORM get_last_day CHANGING lv_endda .
      PERFORM get_tax USING gt_data-mwskz
                      CHANGING kalantutar vergitutari vergihesabi.
      matrah = kalantutar .
      WHILE lv_begda LE lv_endda .
        CLEAR : gt_taksit .
        ADD 1 TO itemno .
        gt_taksit-buzei = itemno .
        gt_taksit-hesap = gt_data-hkont .

        gt_taksit-gsber = gt_data-gsber .
        IF lv_begda(4) NE begyear .
          gt_taksit-hesap = gt_data-hkont_uzun .
        ENDIF .

        IF  itemno >= 0 AND itemno =< 99.
          gt_taksit-tkstx = itemno+1(2) .
        ELSE.
          gt_taksit-tkstx = itemno.
        ENDIF.
        CONDENSE gt_taksit-tkstx .
        CONCATENATE gt_taksit-tkstx '.' ' Taksit' INTO gt_taksit-tkstx.

        CLEAR : it_skat .
        READ TABLE it_skat WITH KEY saknr = gt_taksit-hesap BINARY SEARCH.
        IF sy-subrc IS INITIAL .
          gt_taksit-hestx = it_skat-txt20 .
        ENDIF .

        IF lv_begda(6) EQ gt_data-vade_bas(6) .
          gt_taksit-gun = lv_begda - gt_data-vade_bas + 1 .
        ELSEIF lv_begda EQ lv_endda .
          gt_taksit-gun = gt_data-vade_bit+6(2) .
        ELSE .
          gt_taksit-gun = lv_begda+6(2) .
        ENDIF .
        gt_taksit-kostl = gt_data-kostl .
        gt_taksit-valor = lv_begda .
        ADD gt_taksit-gun TO toplamgun .
        gt_taksit-kalem_tipi = 'S' . " ana hesap kalemi
        gt_taksit-policeno = gt_data-policeno .

        APPEND gt_taksit .
        PERFORM add_month CHANGING lv_begda .
      ENDWHILE .

      LOOP AT gt_taksit WHERE policeno EQ gt_data-policeno.
        IF gt_taksit-buzei NE itemno .
          gt_taksit-tutar = matrah * gt_taksit-gun / toplamgun .
          SUBTRACT gt_taksit-tutar FROM kalantutar .
        ELSE .
          gt_taksit-tutar = kalantutar .
        ENDIF .
*{   ->>> Inserted by Prodea Ozan Şahin - 21.04.2020 10:53:51
        IF bkpf-blart = 'KG'.
          gt_taksit-tutar = -1 * gt_taksit-tutar.
        ENDIF.
*}     <<<- End of   Inserted - 21.04.2020 10:53:51
        MODIFY gt_taksit TRANSPORTING tutar .
      ENDLOOP .

      " vergi kalemi
      IF gt_data-mwskz IS NOT INITIAL .
        LOOP AT it_vergi .
          ADD 1 TO itemno .
          CLEAR : gt_taksit .
          gt_taksit-buzei = itemno .
          gt_taksit-hesap = it_vergi-vergihesabi .
          CLEAR : it_skat .
          READ TABLE it_skat WITH KEY saknr = gt_taksit-hesap
                                                BINARY SEARCH.
          IF sy-subrc IS INITIAL .
            gt_taksit-hestx = it_skat-txt20 .
          ENDIF .
          gt_taksit-tutar = it_vergi-vergitutari .
          gt_taksit-kalem_tipi = 'V' . " Vergi Kalemi
          gt_taksit-matrah = matrah  .
          gt_taksit-ktosl = it_vergi-ktosl .
          APPEND gt_taksit .
        ENDLOOP .
      ENDIF .
      IF gt_data-lifnr IS NOT INITIAL.
        " satıcı kalemi
        CLEAR : gt_taksit .

        gt_taksit-hesap = gt_data-lifnr .
        gt_taksit-gsber = gt_data-gsber .
        SELECT SINGLE name1 FROM lfa1
          INTO gt_taksit-hestx
          WHERE lifnr EQ gt_data-lifnr .



        gt_taksit-kalem_tipi = 'K' . " satıcı kalemi

        IF gt_data-taksit IS INITIAL .
          gt_data-taksit = 1 .
        ENDIF .
        saticitutar = gt_data-tutar .
        DO gt_data-taksit TIMES .
          ADD 1 TO itemno .
          gt_taksit-buzei = itemno .
          IF gt_data-taksit EQ sy-index .
            gt_taksit-tutar = -1 * saticitutar .
            gt_taksit-policeno = gt_data-policeno .
*{   ->>> Inserted by Prodea Ozan Şahin - 21.04.2020 10:53:51
            IF bkpf-blart = 'KG'.
              gt_taksit-tutar = -1 * gt_taksit-tutar.
            ENDIF.
*}     <<<- End of   Inserted - 21.04.2020 10:53:51
            APPEND gt_taksit .
            CONTINUE .
          ENDIF .
          gt_taksit-tutar = ( gt_data-tutar * -1 ) / gt_data-taksit.
          ADD gt_taksit-tutar TO saticitutar .
          gt_taksit-policeno = gt_data-policeno .
*{   ->>> Inserted by Prodea Ozan Şahin - 21.04.2020 10:53:51
          IF bkpf-blart = 'KG'.
            gt_taksit-tutar = -1 * gt_taksit-tutar.
          ENDIF.
*}     <<<- End of   Inserted - 21.04.2020 10:53:51
          APPEND gt_taksit .
        ENDDO .
      ELSEIF gt_data-kunnr    IS NOT INITIAL.

        " müşteri kalemi
        CLEAR : gt_taksit .

        gt_taksit-hesap = gt_data-kunnr .
        gt_taksit-gsber = gt_data-gsber .
        SELECT SINGLE name1 FROM kna1
          INTO gt_taksit-hestx
          WHERE kunnr EQ gt_data-kunnr .
        gt_taksit-kalem_tipi = 'D' . " satıcı kalemi

        IF gt_data-taksit IS INITIAL .
          gt_data-taksit = 1 .
        ENDIF .
        musteritutar = gt_data-tutar .
        DO gt_data-taksit TIMES .
          ADD 1 TO itemno .
          gt_taksit-buzei = itemno .
          IF gt_data-taksit EQ sy-index .
            gt_taksit-tutar = -1 * musteritutar .
            gt_taksit-policeno = gt_data-policeno .
*{   ->>> Inserted by Prodea Ozan Şahin - 21.04.2020 10:53:51
            IF bkpf-blart = 'KG'.
              gt_taksit-tutar = -1 * gt_taksit-tutar.
            ENDIF.
*}     <<<- End of   Inserted - 21.04.2020 10:53:51
            APPEND gt_taksit .
            CONTINUE .
          ENDIF .
          gt_taksit-tutar = ( gt_data-tutar * -1 ) / gt_data-taksit.
          ADD gt_taksit-tutar TO musteritutar .
          gt_taksit-policeno = gt_data-policeno .
*{   ->>> Inserted by Prodea Ozan Şahin - 21.04.2020 10:53:51
          IF bkpf-blart = 'KG'.
            gt_taksit-tutar = -1 * gt_taksit-tutar.
          ENDIF.
*}     <<<- End of   Inserted - 21.04.2020 10:53:51
          APPEND gt_taksit .
        ENDDO .


      ELSEIF gt_data-anahesap IS NOT INITIAL.
        CLEAR : gt_taksit .

        gt_taksit-hesap = gt_data-anahesap .
        gt_taksit-gsber = gt_data-gsber .
        SELECT SINGLE txt20
        INTO gt_taksit-hestx
     FROM skat
     INNER JOIN t001 ON
     t001~ktopl = skat~ktopl
     WHERE skat~spras EQ sy-langu   AND
           t001~bukrs EQ bkpf-bukrs AND
           skat~saknr EQ gt_data-anahesap.

        gt_taksit-kalem_tipi = 'A' . " anahesap kalemi

        IF gt_data-taksit IS INITIAL .
          gt_data-taksit = 1 .
        ENDIF .
        anahesaptutar = gt_data-tutar .
        DO gt_data-taksit TIMES .
          ADD 1 TO itemno .
          gt_taksit-buzei = itemno .
          IF gt_data-taksit EQ sy-index .
            gt_taksit-tutar = -1 *  anahesaptutar.
            gt_taksit-policeno = gt_data-policeno .
            APPEND gt_taksit .
            CONTINUE .
          ENDIF .
          gt_taksit-tutar = ( gt_data-tutar * -1 ) / gt_data-taksit.
          ADD gt_taksit-tutar TO  anahesaptutar .
          gt_taksit-policeno = gt_data-policeno .
          APPEND gt_taksit .
        ENDDO .

      ENDIF.


      "borc & alacak *Mehmet Sertçelik
      IF gt_data-mwskz EQ 'S3'.
        LOOP AT t_mwdat.
          CLEAR : gt_taksit .
          ADD 1 TO itemno .
          gt_taksit-buzei = itemno .
          gt_taksit-hesap = t_mwdat-hkont.
          gt_taksit-tutar = t_mwdat-kawrt * ( 18 / 100 ).
          gt_taksit-kalem_tipi = 'S' .
          gt_taksit-policeno = gt_data-policeno .
          APPEND gt_taksit .
        ENDLOOP.
      ENDIF.

*  LOOP AT gt_taksit .
*    gt_taksit_tum = gt_taksit .
*    gt_taksit_tum-sayac = lv_index .
*    gt_taksit_tum-policeno = gt_data-policeno .
*    APPEND gt_taksit_tum .
*  ENDLOOP .
      gt_data-counter = lv_index .
      MODIFY gt_data .
    ENDLOOP .

    LOOP AT gt_taksit WHERE kalem_tipi NE 'K' .
      gt_taksit-field_style = it_style_kapali[] .
      MODIFY gt_taksit .
    ENDLOOP .

  ENDLOOP .

*loop at gt_taksit_tum where kalem_tipi ne 'K' .
*  gt_taksit_tum-field_style = it_style_kapali[] .
*  modify gt_taksit_tum .
*endloop .

*clear : gt_taksit , gt_taksit[] .
*loop at it_policeno .
*  loop at gt_taksit_tum where policeno = it_policeno-policeno .
*    clear : gt_taksit .
*    gt_taksit = gt_taksit_tum .
*    append gt_taksit .
*  endloop .
*endloop .

ENDFORM .                    "refresh_taksit_tablo

*&---------------------------------------------------------------------*
*&      Form  simulate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM simulate .

  CLEAR : selrows , selrows[] , gt_taksit , gt_taksit[] .

  CALL METHOD alvgrid->get_selected_rows
    IMPORTING
      et_index_rows = selrows[].

  LOOP AT selrows .
    LOOP AT gt_data FROM selrows-index .
      LOOP AT gt_taksit_tum WHERE policeno = gt_data-policeno .
        MOVE-CORRESPONDING gt_taksit_tum TO gt_taksit .
*{   ->>> Inserted by Prodea Ozan Şahin - 20.04.2020 10:57:14
        IF bkpf-blart = 'KG'.
          gt_taksit-tutar = -1 * gt_taksit-tutar.
        ENDIF.
*}     <<<- End of   Inserted - 20.04.2020 10:57:14
        APPEND gt_taksit .
        DELETE gt_taksit_tum .
      ENDLOOP .
      EXIT .
    ENDLOOP .
  ENDLOOP .

  CLEAR : lt_t_fieldcatalog , lt_t_fieldcatalog[] .
  v_default_recname = 'GT_TAKSIT' .
  v_default_report_name = sy-repid .
  PERFORM set_report_fcat.
  PERFORM show_report_fcat_pop TABLES gt_taksit
                      USING  '' "P_VARI
                             gs_variant
                             v_default_report_name
                             v_default_recname.
  LOOP AT gt_taksit .
    CLEAR : gt_taksit_tum .
    MOVE-CORRESPONDING gt_taksit TO gt_taksit_tum .
    APPEND gt_taksit_tum .
  ENDLOOP .

*break-point .
  v_default_recname = 'GT_DATA' .

ENDFORM .                    "simulate

*&---------------------------------------------------------------------*
*&      Form  get_last_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FV_DATUM   text
*----------------------------------------------------------------------*
FORM get_last_day CHANGING fv_datum LIKE sy-datum .

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = fv_datum
    IMPORTING
      last_day_of_month = fv_datum
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
ENDFORM .                    "get_last_day

*&---------------------------------------------------------------------*
*&      Form  add_month
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FV_DATUM   text
*----------------------------------------------------------------------*
FORM add_month CHANGING fv_datum .

  fv_datum+6(2) = '01' .

  CALL FUNCTION 'FKK_DTE_ADD_MONTH'
    EXPORTING
      i_datum               = fv_datum
      i_nr_of_months_to_add = 1
    IMPORTING
      e_result              = fv_datum
    EXCEPTIONS
      no_date               = 1
      OTHERS                = 2.

  PERFORM get_last_day CHANGING fv_datum .

ENDFORM .                    "add_month

*&---------------------------------------------------------------------*
*&      Form  addlz
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FV_HKONT   text
*----------------------------------------------------------------------*
FORM addlz CHANGING fv_hkont .

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = fv_hkont
    IMPORTING
      output = fv_hkont.

ENDFORM .                    "addlz

*&---------------------------------------------------------------------*
*&      Form  get_tax
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FV_MWSKZ     text
*      -->KALANTUTAR   text
*      -->VERGITUTARI  text
*      -->HKONT        text
*----------------------------------------------------------------------*
FORM get_tax USING fv_mwskz CHANGING kalantutar vergitutari hkont.
  "it_vergi[].

*data : T_MWDAT like RTAX1U15 OCCURS 0 WITH HEADER LINE .

  CHECK fv_mwskz IS NOT INITIAL .

  CLEAR : t_mwdat , t_mwdat[] , vergitutari , hkont .

  CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
    EXPORTING
      i_bukrs                 = bkpf-bukrs
      i_mwskz                 = fv_mwskz
      i_waers                 = bkpf-waers
      i_wrbtr                 = kalantutar
    IMPORTING
      e_fwste                 = vergitutari
    TABLES
      t_mwdat                 = t_mwdat[]
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

  CLEAR t_mwdat .
  READ TABLE t_mwdat INDEX 1 .
  IF sy-subrc IS INITIAL .
    hkont = t_mwdat-hkont .
  ENDIF .

  SUBTRACT vergitutari FROM kalantutar .

  LOOP AT t_mwdat .
    CLEAR : it_vergi .
    it_vergi-vergihesabi = t_mwdat-hkont .
    it_vergi-vergitutari = t_mwdat-wmwst .
    it_vergi-ktosl = t_mwdat-ktosl .
    APPEND it_vergi .
  ENDLOOP .

ENDFORM .                    "get_tax


"""" ALV FORMS HERE """"
*&---------------------------------------------------------------------*
*&      Form  SET_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_top_of_page.
*  PERFORM COMMENT_BUILD USING GT_LIST_TOP_OF_PAGE[].
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*       EXPORTING
**            i_logo             = 'ENJOYSAP_LOGO'
*            IT_LIST_COMMENTARY = GT_LIST_TOP_OF_PAGE.
ENDFORM.                    "set_top_of_page
*---------------------------------------------------------------------*
*       FORM COMMENT_BUILD                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  LT_TOP_OF_PAGE                                                *
*---------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE
                                        slis_t_listheader.
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
      IF rs_selfield-fieldname EQ 'AWKEY' .
        SET PARAMETER ID 'BUK' FIELD rs_selfield-value+10(4) .
        SET PARAMETER ID 'BLN' FIELD rs_selfield-value(10) .
        SET PARAMETER ID 'GJR' FIELD rs_selfield-value+14(4) .
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
      ENDIF .
    WHEN '&RFRSH' .
      CLEAR : it_policeno , it_policeno[] .
      LOOP AT gt_taksit .
        CLEAR : it_policeno .
        it_policeno = gt_taksit-policeno .
        COLLECT it_policeno .
      ENDLOOP .
      PERFORM refresh_taksit_tablo .
    WHEN '&OK' .
      DATA : lr_grid TYPE REF TO  cl_gui_alv_grid,
             e_valid VALUE 'X'.
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = lr_grid.
      CALL METHOD lr_grid->check_changed_data
        IMPORTING
          e_valid = e_valid.

      SET SCREEN 0 .
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

  CLEAR : ex_events , ex_events[] , ap_events , ap_events[] .

  PERFORM exclude_events TABLES ex_events USING 'CALLER_EXIT'.
*  PERFORM exclude_events TABLES ex_events USING 'USER_COMMAND'.
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_PAGE'.
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_COVERPAGE'.
  PERFORM exclude_events TABLES ex_events USING 'END_OF_COVERPAGE'.
  PERFORM exclude_events TABLES ex_events USING 'FOREIGN_TOP_OF_PAGE'.
  PERFORM exclude_events TABLES ex_events USING 'FOREIGN_END_OF_PAGE'.
  IF v_default_recname NE 'GT_TAKSIT' .
    PERFORM exclude_events TABLES ex_events USING 'PF_STATUS_SET'.
  ENDIF .
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
  DATA : v_title(53) TYPE c.
  MOVE: 'SELTEXT_L/SELTEXT_M/SELTEXT_S/REPTEXT_DDIC/F4AVAILABL' TO
*  MOVE: 'SCRTEXT_L/SCRTEXT_M/SCRTEXT_S/REPTEXT/F4AVAILABL' TO
  v_title.
  recname = v_default_recname .

  PERFORM
   set_line_field_cat TABLES lt_t_fieldcatalog USING :
    recname 'TARIH' v_title 'Tarih' ,
    recname 'VADE_BAS' v_title 'Başlangıç Vade Tarihi' ,
    recname 'VADE_BIT' v_title 'Bitiş Vade Tarihi' ,
    recname 'TUTAR' v_title 'Tutar' ,
    recname 'GIDERHES' v_title 'GiderHesabı' ,
    recname 'HKONT' v_title 'Hesap' ,
    recname 'AWKEY' v_title 'Muhasebe Belgesi' ,
    recname 'MESSAGE' v_title 'İleti Metni' ,
    recname 'VIRMAN_AWKEY' v_title 'Ufrs Virman Kaydı' ,
*    recname 'VIRMAN_MESSAGE' v_title '' ,
    recname 'HKONT' v_title '(180) Kısa Vade Hesabı' ,
    recname 'HKONT_UZUN' v_title '(280) Uzun Vade Hesabı' ,
    recname 'HKONT_GIDER' v_title '(7XX) Gider Hesabı' ,
    recname 'VALOR' 'EDIT' 'X' ,
    recname 'TUTAR' 'EDIT' 'X' ,
    recname 'TAKSIT' v_title 'Taksit' ,
    recname 'MUAF_TUTAR' v_title 'Poliçe Muafiyet Tutarı' ,
    recname 'TEMIN_TUTAR' v_title 'Poliçe Teminat Tutarı' ,
    recname 'POLICE_TURU' v_title 'Poliçe Türü' ,
    recname 'POLICE_KONU' v_title 'Poliçe Konusu' .

  DELETE lt_t_fieldcatalog WHERE fieldname EQ 'COUNTER' OR

                                 fieldname EQ 'SAYAC' OR
                                 fieldname EQ 'KALEM_TIPI' OR
                                 fieldname EQ 'MATRAH' OR
                                 fieldname EQ 'ROWCOLOR' OR
                                 fieldname EQ 'BELGENO'OR
                                 fieldname EQ 'MUAF_TUTAR' OR "ABMIRTEM
                                 fieldname EQ 'TEMIN_TUTAR' OR
                                 fieldname EQ 'POLICE_TURU' OR
                                 fieldname EQ 'POLICE_KONU' OR
                                 fieldname EQ 'MATNR' OR
                                 fieldname EQ 'MENGE' OR
                                 fieldname EQ 'MEINS'. """"""

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
*  gs_layout-coltab_fieldname   = 'COLOR'.
*  gs_layout-expand_fieldname  = 'BUKRS'.
*    GS_LAYOUT-BOX_FIELDNAME = 'SELKZ'.

  CLEAR : gs_variant .
  gs_variant-report = sy-repid .
  gs_variant-username = sy-uname .

  IF v_default_recname EQ 'GT_DATA' .
    gs_variant-handle = '1' .
  ELSEIF v_default_recname EQ 'GT_TAKSIT' .
    gs_variant-handle = '2' .
  ELSEIF v_default_recname EQ 'IT_LOG' .
    gs_variant-handle = '3' .
  ENDIF .

  gs_layout-colwidth_optimize  = 'X' .
  gs_layout-zebra = 'X' .
  IF v_default_recname EQ 'GT_DATA' .
    gs_layout-edit = 'X' .
    gs_layout-info_fieldname = 'ROWCOLOR'.
  ELSEIF v_default_recname EQ 'GT_TAKSIT' .
    CLEAR : gs_layout-edit , gs_layout-info_fieldname .
  ENDIF .

ENDFORM.                    " set_layout_user_exit

*&---------------------------------------------------------------------*
*&      Form  SHOW_REPORT_FCAT_POP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_REPORT            text
*      -->PVARI                text
*      -->GS_VARIANT           text
*      -->DEFAULT_REPORT_NAME  text
*      -->DEFAULT_RECNAME      text
*----------------------------------------------------------------------*
FORM show_report_fcat_pop TABLES it_report
               USING    pvari
                        gs_variant
                        default_report_name
                        default_recname.
  PERFORM layout_init USING gs_layout.
  PERFORM excluding_events.
  PERFORM eventtab_build USING gt_events[].
  PERFORM set_layout USING pvari default_report_name.

  DATA : lvc_fieldcat TYPE lvc_t_fcat WITH HEADER LINE,
         lvc_layout   TYPE lvc_s_layo.

  CLEAR : lvc_fieldcat , lvc_fieldcat[] , lvc_layout .

  PERFORM convert_to_lvc USING lt_t_fieldcatalog[] gs_layout
                         CHANGING lvc_fieldcat[] lvc_layout .

  IF v_default_recname EQ 'GT_TAKSIT' .
    lvc_layout-stylefname = 'FIELD_STYLE' .
  ELSE .
    CLEAR : lvc_layout-stylefname.
  ENDIF .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_BACKGROUND_ID         = 'ALV_BACKGROUND'
      i_buffer_active         = 'X'
      i_bypassing_buffer      = 'X'
      i_callback_program      = default_report_name
*     i_structure_name        = default_tab_name
      i_grid_settings         = gs_grid_set
      is_layout_lvc           = lvc_layout
      it_fieldcat_lvc         = lvc_fieldcat[]
      i_save                  = g_save
      is_variant              = gs_variant
      it_events               = gt_events[]
      it_excluding            = lt_excluding
*     it_fieldcat             = lt_t_fieldcatalog[]
      i_screen_start_column   = 5
      i_screen_start_line     = 2
      i_screen_end_column     = 150
      i_screen_end_line       = 18
    IMPORTING
      e_exit_caused_by_caller = g_exit_caused_by_caller
      es_exit_caused_by_user  = gs_exit_caused_by_user
    TABLES
      t_outtab                = it_report[]
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
ENDFORM.                    " show_report_fcat
*&---------------------------------------------------------------------*
*&      Form  MASRAF_YERI_KONTROLU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_DATA_HKONT_GIDER  text
*      -->P_GT_DATA_KOSTL  text
*----------------------------------------------------------------------*
FORM masraf_siparis_kontrol USING iv_hkont_gider
                                  iv_kostl
                                  iv_aufnr
                                  iv_gsber.

  DATA: lv_kosar     TYPE csks-kosar .
  FIELD-SYMBOLS <fs> TYPE any.

  IF gt_zmasr_hesap[] IS NOT INITIAL.

    CLEAR: lv_kosar.
    SELECT SINGLE kosar
                  FROM csks
                  INTO lv_kosar
                  WHERE kokrs EQ '1000'
                    AND kostl EQ iv_kostl.

    IF sy-subrc EQ 0.
      CLEAR: gs_zmasr_hesap.
      READ TABLE gt_zmasr_hesap INTO gs_zmasr_hesap
                                WITH KEY hkont = iv_hkont_gider(3)
                                         kosar = lv_kosar .
      IF sy-subrc IS NOT INITIAL.
        MESSAGE e022(zco) WITH lv_kosar iv_kostl.
      ENDIF.
    ENDIF.

    IF iv_aufnr IS NOT INITIAL AND
       gt_aufk[] IS NOT INITIAL.

      CLEAR: gs_aufk.
      READ TABLE gt_aufk INTO gs_aufk WITH KEY aufnr = iv_aufnr.
      IF sy-subrc EQ 0.
        CLEAR: lv_kosar.
        SELECT SINGLE kosar
                      FROM csks
                      INTO lv_kosar
                      WHERE kokrs EQ '1000'
                        AND kostl EQ gs_aufk-cycle.
        IF sy-subrc EQ 0.
          CLEAR: gs_zmasr_hesap.
          READ TABLE gt_zmasr_hesap INTO gs_zmasr_hesap
                                    WITH KEY hkont = iv_hkont_gider(3)
                                             kosar = lv_kosar .
          IF sy-subrc IS NOT INITIAL.
            MESSAGE e028(zco) WITH iv_hkont_gider iv_aufnr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


  CLEAR: gs_skb1.
  READ TABLE gt_skb1 INTO gs_skb1 WITH KEY bukrs = bkpf-bukrs
                                           saknr = iv_hkont_gider.
  IF sy-subrc EQ 0.
    CASE gs_skb1-fstag.
      WHEN 'Z003'.
        IF iv_aufnr IS INITIAL.
          MESSAGE e025(zco) WITH iv_hkont_gider.
        ENDIF.
      WHEN 'Z004'.
        IF iv_kostl IS INITIAL.
          MESSAGE e026(zco) WITH iv_hkont_gider.
        ENDIF.
        IF iv_gsber IS INITIAL.
          MESSAGE e026(zco) WITH iv_hkont_gider.
        ENDIF.
      WHEN 'Z005'.
        IF iv_gsber IS INITIAL.
          MESSAGE e027(zco) WITH iv_hkont_gider.
        ENDIF.
      WHEN 'Z007'.
        IF iv_kostl IS INITIAL.
          MESSAGE e026(zco) WITH iv_hkont_gider.
        ENDIF.
    ENDCASE.
  ENDIF.

ENDFORM.                    "masraf_siparis_kontrol
*&---------------------------------------------------------------------*
*&      Form  GET_MASRAF_HESAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_masraf_hesap .

  CLEAR: gt_zmasr_hesap[].
  SELECT hkont kosar
         FROM zfi_masr_hesap
         INTO TABLE gt_zmasr_hesap.

  IF gt_data[] IS NOT INITIAL.

    CLEAR: gt_aufk[] .
    SELECT aufnr bukrs kokrs cycle
           FROM aufk
           INTO TABLE gt_aufk
           FOR ALL ENTRIES IN gt_data
           WHERE aufnr EQ gt_data-aufnr.

    CLEAR: gt_skb1[] .
    SELECT bukrs saknr fstag
           FROM skb1
           INTO TABLE gt_skb1
           FOR ALL ENTRIES IN gt_data
           WHERE bukrs EQ bkpf-bukrs
             AND saknr EQ gt_data-hkont_gider.
  ENDIF.

ENDFORM.                    "get_masraf_hesap
*&---------------------------------------------------------------------*
*&      Form  POPUP_GET_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM popup_get_value CHANGING ev_belgeno.

  DATA: ls_fields LIKE sval .
  DATA: lt_fields LIKE TABLE OF sval .
  DATA: return_code TYPE c.

  CLEAR: ls_fields, ev_belgeno.
  ls_fields-tabname   = 'ZFI_PES_GID_BAS'.
  ls_fields-fieldname = 'BELGENO'.
  ls_fields-fieldtext = 'Belge Numarası'.
  ls_fields-field_obl = 'X'.
*  ls_fields-novaluehlp  = 'X'.
  APPEND ls_fields TO lt_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
*     NO_VALUE_CHECK  = ' '
      popup_title     = 'Lütfen Satır İçin Gerekli Bilgileri Girin.'
      start_column    = '5'
      start_row       = '5'
    IMPORTING
      returncode      = return_code
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  READ TABLE lt_fields INTO ls_fields INDEX 1.
  CLEAR: ev_belgeno. CONDENSE ls_fields-value.
  ev_belgeno = ls_fields-value.


ENDFORM.                    "popup_get_value
*&---------------------------------------------------------------------*
*&      Form  GET_HEADER_ITEMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_BELGENO  text
*----------------------------------------------------------------------*
FORM get_header_items  USING iv_belgeno.

  DATA: ls_zfi_pes_gid_bas TYPE zfi_pes_gid_bas,
        lt_zfi_pes_gid_bas TYPE TABLE OF zfi_pes_gid_bas.
  DATA: ls_zfi_pes_gid_kal TYPE zfi_pes_gid_kal,
        lt_zfi_pes_gid_kal TYPE TABLE OF zfi_pes_gid_kal,
        lv_toplam TYPE zfi_pes_gid_bas-tutar.

  DATA: lv_blart TYPE bkpf-blart."added by ozans 21.04.2020
*        lv_buzid_t TYPE zfi_pes_gid_bas-tutar.

  CLEAR: gt_200, gt_200[], gt_200_kalem, gt_200_kalem[], lv_toplam,
         gt_200_belge, gt_200_belge[], gs_200_baslik,
         gt_bseg, gt_bseg[], lv_blart.
*         lv_buzid_t.

  IF iv_belgeno IS NOT INITIAL.

    CLEAR: ls_zfi_pes_gid_bas.
    SELECT SINGLE * FROM zfi_pes_gid_bas
                    INTO ls_zfi_pes_gid_bas
                    WHERE belgeno EQ iv_belgeno.

    IF sy-subrc EQ 0.
      IF ls_zfi_pes_gid_bas-durum = 'I'.
        MESSAGE e055(zco) WITH iv_belgeno.
      ELSE.
        CLEAR: lt_zfi_pes_gid_kal[], lt_zfi_pes_gid_kal.
        SELECT * FROM zfi_pes_gid_kal
                 INTO TABLE lt_zfi_pes_gid_kal
                 WHERE belgeno EQ iv_belgeno.

        IF sy-subrc EQ 0.
          SELECT * FROM bseg INTO TABLE gt_bseg
                  WHERE bukrs = ls_zfi_pes_gid_bas-awkey+10(4)
                    AND belnr = ls_zfi_pes_gid_bas-awkey(10)
                    AND gjahr = ls_zfi_pes_gid_bas-awkey+14(4).

*{   ->>> Inserted by Prodea Ozan Şahin - 21.04.2020 16:45:00
          LOOP AT lt_zfi_pes_gid_kal TRANSPORTING NO FIELDS WHERE tutar < 0..
            EXIT.
          ENDLOOP.
          IF sy-subrc = 0.
            lv_blart = 'KG'.
          ENDIF.
*}     <<<- End of   Inserted - 21.04.2020 16:45:00

          CLEAR: ls_zfi_pes_gid_kal.
          LOOP AT lt_zfi_pes_gid_kal INTO ls_zfi_pes_gid_kal.
*{   ->>> Inserted by Prodea Ozan Şahin - 21.04.2020 16:19:50
            IF lv_blart = 'KG'.
              ls_zfi_pes_gid_kal-tutar = -1 * ls_zfi_pes_gid_kal-tutar.
            ENDIF.
*}     <<<- End of   Inserted - 21.04.2020 16:19:50
            CLEAR: gt_200_kalem, gt_200_belge.
            MOVE-CORRESPONDING ls_zfi_pes_gid_kal TO gt_200_kalem.
            MOVE-CORRESPONDING ls_zfi_pes_gid_kal TO gt_200_belge.
            gt_200_belge-tutar_son = gt_200_belge-tutar.
*            gt_200_belge-shkzg_son = 'H'."commented by ozans 21.04.2020
*{   ->>> Inserted by Prodea Ozan Şahin - 21.04.2020 16:19:50
            IF lv_blart = 'KG'.
              gt_200_belge-shkzg_son = 'S'.
            ELSE.
              gt_200_belge-shkzg_son = 'H'.
            ENDIF.
*}     <<<- End of   Inserted - 21.04.2020 16:19:50
            APPEND gt_200_kalem.
            IF ls_zfi_pes_gid_kal-awkey_vuk IS INITIAL.
              gt_200_belge-waers = ls_zfi_pes_gid_bas-waers.
              gt_200_belge-kalem = 'A'.
              APPEND gt_200_belge.
              lv_toplam = lv_toplam + gt_200_belge-tutar .
            ENDIF.

          ENDLOOP.

          CLEAR: gt_200, gs_200_baslik.
*{   ->>> Inserted by Prodea Ozan Şahin - 21.04.2020 16:19:50
          IF lv_blart = 'KG'.
            ls_zfi_pes_gid_bas-tutar = -1 * ls_zfi_pes_gid_bas-tutar.
          ENDIF.
*}     <<<- End of   Inserted - 21.04.2020 16:19:50
          MOVE-CORRESPONDING ls_zfi_pes_gid_bas TO gt_200.
          MOVE-CORRESPONDING ls_zfi_pes_gid_bas TO gs_200_baslik.
          APPEND gt_200.

*        CLEAR :lv_buzid_t.
*        LOOP AT gt_bseg WHERE buzid IS NOT INITIAL.
*          CLEAR: gt_200_belge.
*          gt_200_belge-hkont     = gt_bseg-hkont.
*          gt_200_belge-tutar_son = gt_bseg-wrbtr.
*          gt_200_belge-shkzg_son = 'H'.
*          gt_200_belge-waers     = ls_zfi_pes_gid_bas-waers.
*          gt_200_belge-kalem     = 'B'.
*          APPEND gt_200_belge.
*          lv_buzid_t = gt_bseg-wrbtr.
*          EXIT.
*        ENDLOOP.
*{   ->>> Inserted by Prodea Ozan Şahin - 21.04.2020 18:10:45
          IF lv_blart = 'KG'.
            READ TABLE gt_bseg WITH KEY shkzg = 'S'.
            IF sy-subrc EQ 0.
              CLEAR: gt_200_belge.
              IF gt_bseg-lifnr IS NOT INITIAL.
                gt_200_belge-hkont    = gt_bseg-lifnr.
                gt_200_belge-tip      = 'L'.
              ELSEIF gt_bseg-kunnr IS NOT INITIAL.
                gt_200_belge-hkont    = gt_bseg-kunnr.
                gt_200_belge-tip      = 'K'.
              ELSE.
                gt_200_belge-hkont     = gt_bseg-hkont.
                gt_200_belge-tip      = 'A'.
              ENDIF.
              gt_200_belge-tutar_son = lv_toplam." + lv_buzid_T.
              gt_200_belge-shkzg_son   = 'H'.
              gt_200_belge-waers     = ls_zfi_pes_gid_bas-waers.
              gt_200_belge-kalem     = 'B'.
              APPEND gt_200_belge.
            ENDIF.
          ELSE.
*}     <<<- End of   Inserted - 21.04.2020 18:10:45
            READ TABLE gt_bseg WITH KEY shkzg = 'H'.
            IF sy-subrc EQ 0.
              CLEAR: gt_200_belge.
              IF gt_bseg-lifnr IS NOT INITIAL.
                gt_200_belge-hkont    = gt_bseg-lifnr.
                gt_200_belge-tip      = 'L'.
              ELSEIF gt_bseg-kunnr IS NOT INITIAL.
                gt_200_belge-hkont    = gt_bseg-kunnr.
                gt_200_belge-tip      = 'K'.
              ELSE.
                gt_200_belge-hkont     = gt_bseg-hkont.
                gt_200_belge-tip      = 'A'.
              ENDIF.
              gt_200_belge-tutar_son = lv_toplam." + lv_buzid_T.
              gt_200_belge-shkzg_son = 'S'.
              gt_200_belge-waers     = ls_zfi_pes_gid_bas-waers.
              gt_200_belge-kalem     = 'B'.
              APPEND gt_200_belge.
            ENDIF.
          ENDIF.

          CLEAR: gt_200_belge.
          gt_200_belge-hkont = ls_zfi_pes_gid_bas-hkont_gider.
          gt_200_belge-waers = ls_zfi_pes_gid_bas-waers.
          gt_200_belge-kalem = 'C'.
          APPEND gt_200_belge.

        ELSE.
          MESSAGE e055(zco) WITH iv_belgeno.
        ENDIF.
      ENDIF.
*      CALL METHOD alvgrid->refresh_table_display
*        EXCEPTIONS
*          finished = 1
*          OTHERS   = 2.
    ELSE.
      MESSAGE e055(zco) WITH iv_belgeno.
    ENDIF.

  ENDIF.

ENDFORM.                    "get_header_items
*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV
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
*    PERFORM set_cell_colours.
*    PERFORM set_cell_styles.
    PERFORM exclude_button CHANGING gt_toolbar_excluding .

    CALL METHOD grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layo100
        it_toolbar_excluding = gt_toolbar_excluding
        is_variant           = gs_variant
        i_save               = 'A'
      CHANGING
        it_outtab            = gt_200[]
        it_sort              = gt_sort[]
        it_fieldcatalog      = gt_fcat[].


*    CREATE OBJECT o_event_handler .
**
****   register handler for events
***    SET HANDLER event_receiver_0100->handle_right_click
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_left_click_design
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_move_control
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_size_control
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_left_click_run
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_onf1
** FOR grid.
*    SET HANDLER o_event_handler->on_f4
* FOR grid.
***    SET HANDLER event_receiver_0100->handle_data_changed
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_ondropgetflavor
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_ondrag
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_ondrop
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_ondropcomplete
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_subtotal_text
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_before_user_command
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_user_command
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_after_user_command
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_double_click
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_delayed_callback
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_delayed_changed_sel_cal
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_print_top_of_page
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_print_top_of_list
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_print_end_of_page
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_print_end_of_list
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_top_of_page
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_context_menu_request
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_menu_button
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_toolbar
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_hotspot_click
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_end_of_list
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_after_refresh
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_button_click
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_data_changed_finished
** FOR grid.
***
**   register F4 fields
*    PERFORM field_f4_register.
*
*    CALL METHOD grid->set_toolbar_interactive.
*
**   ENTER key is pressed or
*    CALL METHOD grid->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
**   data is changed and cursor is moved from the cell
*    CALL METHOD grid->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*


  ELSE .
    PERFORM check_changed_data USING grid .
    PERFORM refresh_table_display USING grid .
  ENDIF .


ENDFORM.                    "show_alv
*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_alv2 .

  IF g_custom_container2 IS INITIAL.
    CREATE OBJECT g_custom_container2
      EXPORTING
        container_name = g_container2.
    CREATE OBJECT grid2
      EXPORTING
        i_parent = g_custom_container2.


    gs_layo1002-zebra      = 'X'.
    gs_layo1002-cwidth_opt = 'X'.
*    gs_layo1002-sel_mode   = 'A'.
*    gs_layo1002-info_fname = 'COLOR'.
*    gs_layo1002-stylefname = 'CELLTAB'.
*    gs_layo1002-ctab_fname = 'CELLCOLOR'.
*    gs_variant-report     = sy-repid .

    PERFORM build_fcat2 .
*    PERFORM set_cell_colours.
*    PERFORM set_cell_styles.
    PERFORM exclude_button CHANGING gt_toolbar_excluding2 .

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layo1002
        it_toolbar_excluding = gt_toolbar_excluding2
*       is_variant           = gs_variant2
*       i_save               = 'A'
      CHANGING
        it_outtab            = gt_200_belge[]
        it_sort              = gt_sort2[]
        it_fieldcatalog      = gt_fcat2[].


*
**   register handler for events
***    SET HANDLER event_receiver_0100->handle_right_click
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_left_click_design
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_move_control
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_size_control
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_left_click_run
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_onf1
** FOR grid.

***    SET HANDLER event_receiver_0100->handle_data_changed
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_ondropgetflavor
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_ondrag
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_ondrop
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_ondropcomplete
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_subtotal_text
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_before_user_command
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_user_command
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_after_user_command
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_double_click
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_delayed_callback
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_delayed_changed_sel_cal
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_print_top_of_page
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_print_top_of_list
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_print_end_of_page
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_print_end_of_list
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_top_of_page
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_context_menu_request
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_menu_button
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_toolbar
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_hotspot_click
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_end_of_list
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_after_refresh
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_button_click
** FOR grid.
***    SET HANDLER event_receiver_0100->handle_data_changed_finished
** FOR grid.
*
**   register F4 fields
*    PERFORM field_f4_register.
*
*    CALL METHOD grid->set_toolbar_interactive.
*
**   ENTER key is pressed or
*    CALL METHOD grid->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
**   data is changed and cursor is moved from the cell
*    CALL METHOD grid->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*

*egister the field for which the custom F4 has to be displayed
    gs_f4-fieldname  = 'HKONT'.
    gs_f4-register   = 'X'.
    gs_f4-getbefore  = space.
    gs_f4-chngeafter = space.
    APPEND gs_f4 TO gt_f4.
    CALL METHOD grid2->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4.

  ELSE .
    PERFORM check_changed_data USING grid2 .
    PERFORM refresh_table_display USING grid2.
  ENDIF .


ENDFORM.                    "show_alv2
*&---------------------------------------------------------------------*
*&      Form  BUILD_FCAT
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
      i_internal_tabname     = 'GT_DATA'
      i_client_never_display = 'X'
      i_inclname             = lv_progname
    CHANGING
      ct_fieldcat            = gt_flcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT gt_flcat INTO gwa_flcat .
    FREE : gwa_flcat-key.
    MOVE-CORRESPONDING  gwa_flcat TO gs_fcat .
    gs_fcat-fieldname        = gwa_flcat-fieldname.
    gs_fcat-ref_table        = gwa_flcat-ref_tabname.
    gs_fcat-ref_field        = gwa_flcat-ref_fieldname.
    gs_fcat-coltext          = gwa_flcat-seltext_l.
    CASE gs_fcat-fieldname.
      WHEN 'ABC'.
*        gs_fcat-coltext = 'Tanım'.
      WHEN 'VSOLM'.
        gs_fcat-edit = 'X'.
    ENDCASE.
    gs_fcat-seltext          = gs_fcat-coltext.
    gs_fcat-reptext          = gs_fcat-coltext.
    APPEND gs_fcat TO gt_fcat.
    CLEAR  gs_fcat.

  ENDLOOP.

  DELETE gt_fcat WHERE fieldname EQ 'COUNTER' OR
                                 fieldname EQ 'SAYAC' OR
                                 fieldname EQ 'KALEM_TIPI' OR
                                 fieldname EQ 'MATRAH' OR
                                 fieldname EQ 'ROWCOLOR' OR
                                 fieldname EQ 'BELGENO'OR
                                 fieldname EQ 'MUAF_TUTAR' OR "ABMIRTEM
                                 fieldname EQ 'TEMIN_TUTAR' OR
                                 fieldname EQ 'POLICE_TURU' OR
                                 fieldname EQ 'POLICE_KONU' OR
                                 fieldname EQ 'MATNR' OR
                                 fieldname EQ 'MENGE' OR
                                 fieldname EQ 'MEINS'.

*  DATA: ls_fcat TYPE lvc_s_fcat.
*
** hotspot fields
*  ls_fcat-hotspot = 'X'.
*  MODIFY gt_fcat FROM ls_fcat
*    TRANSPORTING hotspot
*    WHERE fieldname = 'ABC'.
*
** editable column
*  ls_fcat-edit = 'X'.
*  MODIFY gt_fcat FROM ls_fcat
*    TRANSPORTING edit
*    WHERE fieldname = 'ABC'.
*
** F4 list
*  ls_fcat-f4availabl = 'X'.
*  MODIFY gt_fcat FROM ls_fcat
*    TRANSPORTING f4availabl
*    WHERE fieldname = 'ABC'.
*
** dropdown list
*  ls_fcat-drdn_hndl = '1'.
*  MODIFY gt_fcat FROM ls_fcat
*    TRANSPORTING drdn_hndl
*    WHERE fieldname = 'ABC'.
ENDFORM.                    "build_fcat
*&---------------------------------------------------------------------*
*&      Form  BUILD_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fcat2 .
  DATA : lv_progname LIKE sy-repid.

  REFRESH : gt_flcat2 , gt_fcat2.

  lv_progname = sy-repid .

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = lv_progname
      i_internal_tabname     = 'GT_200_BELGE'
      i_client_never_display = 'X'
      i_inclname             = lv_progname
    CHANGING
      ct_fieldcat            = gt_flcat2
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT gt_flcat2 INTO gwa_flcat2 .
    FREE : gwa_flcat2-key.
    MOVE-CORRESPONDING  gwa_flcat2 TO gs_fcat2 .
    gs_fcat2-fieldname        = gwa_flcat2-fieldname.
    gs_fcat2-ref_table        = gwa_flcat2-ref_tabname.
    gs_fcat2-ref_field        = gwa_flcat2-ref_fieldname.
    gs_fcat2-coltext          = gwa_flcat2-seltext_l.
    CASE gs_fcat2-fieldname.
*      WHEN 'AWKEY_VUK'  OR 'MESSAGE_VUK'  OR
*           'AWKEY_TFRS' OR 'MESSAGE_TFRS' OR
*           'AWKEY_VRM'  OR 'MESSAGE_VRM'  OR
*           'WAERS'.
*        CONTINUE.
*        gs_fcat-coltext = 'Tanım'.
      WHEN 'TUTAR_SON'.
        gs_fcat2-edit = 'X'.
*        gs_fcat2-ref_table        = 'BSEG'.
*        gs_fcat2-ref_field        = 'WAERS'.
        gs_fcat2-cfieldname         = 'WAERS'.
      WHEN 'SHKZG_SON'.
        gs_fcat2-edit = 'X'.
        gs_fcat2-ref_field = space.
        gs_fcat2-ref_table = space.
      WHEN 'WAERS'.
        gs_fcat2-no_out = 'X'.
    ENDCASE.
    gs_fcat2-seltext          = gs_fcat2-coltext.
    gs_fcat2-reptext          = gs_fcat2-coltext.
    APPEND gs_fcat2 TO gt_fcat2.
    CLEAR  gs_fcat2.

  ENDLOOP.
  DELETE gt_fcat2 WHERE fieldname EQ 'AWKEY_VUK'
                     OR fieldname EQ 'MESSAGE_VUK'
                     OR fieldname EQ 'AWKEY_TFRS'
                     OR fieldname EQ 'MESSAGE_TFRS'
                     OR fieldname EQ 'AWKEY_VRM'
                     OR fieldname EQ 'MESSAGE_VRM'
                     OR fieldname EQ 'KALEM'
                     OR fieldname EQ 'TIP'.
*                     OR fieldname EQ 'WAERS'.

*  DELETE gt_fcat2 WHERE fieldname EQ 'COUNTER' OR
*                        fieldname EQ 'SAYAC' OR
*                        fieldname EQ 'KALEM_TIPI' OR
*                        fieldname EQ 'MATRAH' OR
*                        fieldname EQ 'ROWCOLOR' OR
*                        fieldname EQ 'BELGENO'OR
*                        fieldname EQ 'MUAF_TUTAR' OR "ABMIRTEM
*                        fieldname EQ 'TEMIN_TUTAR' OR
*                        fieldname EQ 'POLICE_TURU' OR
*                        fieldname EQ 'POLICE_KONU' OR
*                        fieldname EQ 'MATNR' OR
*                        fieldname EQ 'MENGE' OR
*                        fieldname EQ 'MEINS'.

*  DATA: ls_fcat TYPE lvc_s_fcat.
*
** hotspot fields
*  ls_fcat-hotspot = 'X'.
*  MODIFY gt_fcat FROM ls_fcat
*    TRANSPORTING hotspot
*    WHERE fieldname = 'ABC'.
*
** editable column
*  ls_fcat-edit = 'X'.
*  MODIFY gt_fcat FROM ls_fcat
*    TRANSPORTING edit
*    WHERE fieldname = 'ABC'.
*
** F4 list
*  ls_fcat-f4availabl = 'X'.
*  MODIFY gt_fcat FROM ls_fcat
*    TRANSPORTING f4availabl
*    WHERE fieldname = 'ABC'.
*
** dropdown list
*  ls_fcat-drdn_hndl = '1'.
*  MODIFY gt_fcat FROM ls_fcat
*    TRANSPORTING drdn_hndl
*    WHERE fieldname = 'ABC'.
ENDFORM.                    "build_fcat2
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
*&      Form  YOU_SURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_ANSWER  text
*----------------------------------------------------------------------*
FORM you_sure  USING iv_title
            CHANGING cv_answer.

  CLEAR: cv_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = iv_title
      text_question  = 'Ekrandan çıkmak istediğinize emin misiniz?'
    IMPORTING
      answer         = cv_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    "you_sure
*&---------------------------------------------------------------------*
*&      Form  CONTROL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_ERROR  text
*----------------------------------------------------------------------*
FORM control_data  CHANGING cv_error   TYPE c.

  CLEAR: cv_error.

  DATA: BEGIN OF ls_control,
          tutar_h LIKE gt_200_belge-tutar_son,
          tutar_s LIKE gt_200_belge-tutar_son,
        END OF ls_control.



  LOOP AT gt_200_belge WHERE tutar_son IS NOT INITIAL AND
                             shkzg_son IS INITIAL.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    cv_error = 'X'.
    MESSAGE 'Borç alacak göstergesi girilmemiş satırlar mevcut.'
       TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT gt_200_belge WHERE tutar_son IS INITIAL AND
                             shkzg_son IS NOT INITIAL .
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    cv_error = 'X'.
    MESSAGE 'Tutarı girilmemiş satırlar mevcut.'
       TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT gt_200_belge WHERE tutar_son LT 0 .
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    cv_error = 'X'.
    MESSAGE 'Tutarı 0dan küçük girilemez.'
       TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT gt_200_belge
    WHERE shkzg_son NE 'S'
      AND shkzg_son NE 'H'
      AND shkzg_son IS NOT INITIAL.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    cv_error = 'X'.
    MESSAGE 'Borç alacak göstergesi yanlış satırlar mevcut.'
       TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR: ls_control.
  LOOP AT gt_200_belge.
    IF gt_200_belge-shkzg_son = 'H'.
      ls_control-tutar_h = ls_control-tutar_h + gt_200_belge-tutar_son.
    ELSE.
      ls_control-tutar_s = ls_control-tutar_s + gt_200_belge-tutar_son.
    ENDIF.
  ENDLOOP.

  IF ls_control-tutar_h NE ls_control-tutar_s.
    cv_error = 'X'.
    MESSAGE 'Borç alacak tutarları eşit değildir.'
       TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
ENDFORM.                    "control_data
*&---------------------------------------------------------------------*
*&      Form  TERS_KAYIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_ERROR  text
*----------------------------------------------------------------------*
FORM ters_kayit  CHANGING cv_error.

  DATA : it_accountreceivable LIKE TABLE OF bapiacar09 WITH HEADER LINE.
  DATA : v_itemno_acc       TYPE posnr_acc .
  CLEAR : documentheader , it_currencyamount , it_currencyamount[] ,
          it_accountgl , it_accountgl[] , it_accountpayable ,
          it_accountpayable[] , it_accounttax , it_accounttax[] ,
          it_return , it_return[] , ktosl , virman_tutari ,
          it_accountreceivable[] , it_accountwt[] , it_accountwt,
          it_accountreceivable.
  DATA: lv_top_tutar TYPE bseg-dmbtr.
  DATA: lv_blart     TYPE bkpf-blart."added by ozans 21.04.2020
  CLEAR: cv_error, lv_top_tutar.

  SELECT SINGLE COUNT( * )
    FROM zfi_pes_gid_kal
    WHERE belgeno EQ gs_200_baslik-belgeno
      AND tutar   LT 0.
  IF sy-subrc = 0.
    lv_blart = 'KG'.
  ENDIF.

  documentheader-username   = sy-uname.
*  documentheader-bus_act    = 'RFBU'.
  documentheader-header_txt = gs_200_baslik-bktxt.
  documentheader-comp_code  = gs_200_baslik-bukrs.
  documentheader-doc_date   = gv_tarih_200.
  documentheader-pstng_date = gv_tarih_200_post.
*  documentheader-doc_type   = 'KR'."commented by xosahin
*{   ->>> Inserted by Prodea Ozan Şahin - 21.04.2020 18:32:52
  IF lv_blart = 'KG'.
    documentheader-doc_type   = 'KG'.
  ELSE.
    documentheader-doc_type   = 'KR'.
  ENDIF.
*}     <<<- End of   Inserted - 21.04.2020 18:32:52

  documentheader-ref_doc_no = gs_200_baslik-policeno.


  LOOP AT gt_200_belge WHERE kalem = 'A' AND tutar_son IS NOT INITIAL.


    READ TABLE gt_bseg WITH KEY buzei = gt_200_belge-taksitno.
    IF sy-subrc EQ 0.
      ADD 1 TO v_itemno_acc.
      it_accountgl-itemno_acc = v_itemno_acc.
      it_accountgl-item_text  = gt_bseg-sgtxt.
      it_accountgl-tax_code   = gt_bseg-mwskz.
      it_accountgl-costcenter = gt_bseg-kostl.
      it_accountgl-gl_account = gt_bseg-hkont.
      it_accountgl-bus_area   = gt_bseg-gsber.
      it_accountgl-value_date = gt_bseg-valut .
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = it_accountgl-gl_account
        IMPORTING
          output = it_accountgl-gl_account.
      APPEND it_accountgl.
      CLEAR it_accountgl.


      it_currencyamount-itemno_acc = v_itemno_acc.
      it_currencyamount-currency   = gs_200_baslik-waers .
      it_currencyamount-amt_doccur = gt_200_belge-tutar_son.
      it_currencyamount-exch_rate = gs_200_baslik-kursf .
      IF gt_200_belge-shkzg_son = 'H'.
        it_currencyamount-amt_doccur = it_currencyamount-amt_doccur * -1.
      ENDIF.
      lv_top_tutar = lv_top_tutar + it_currencyamount-amt_doccur.
      APPEND it_currencyamount .
      CLEAR : it_currencyamount.
    ENDIF.

  ENDLOOP.

*LOOP AT gt_bseg WHERE buzid IS NOT INITIAL .
*
*ENDLOOP.

  LOOP AT gt_200_belge WHERE kalem = 'C' AND tutar_son IS NOT INITIAL .
    IF gt_200_belge-shkzg_son = 'H'.
      lv_top_tutar = lv_top_tutar - gt_200_belge-tutar_son.
    ELSE.
      lv_top_tutar = lv_top_tutar + gt_200_belge-tutar_son.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_200_belge WHERE kalem = 'B' AND tutar_son IS NOT INITIAL.
    ADD 1 TO v_itemno_acc.
    READ TABLE gt_bseg WITH KEY shkzg = 'H'.
    CHECK sy-subrc EQ 0.
*{   ->>> Inserted by Prodea Ozan Şahin - 21.04.2020 18:35:15
    IF lv_blart = 'KG'.
      READ TABLE gt_bseg WITH KEY shkzg = 'S'.
      CHECK sy-subrc EQ 0.
    ELSE.
      READ TABLE gt_bseg WITH KEY shkzg = 'H'.
      CHECK sy-subrc EQ 0.
    ENDIF.
*}     <<<- End of   Inserted - 21.04.2020 18:35:15
    IF gt_200_belge-tip = 'L'.
      CLEAR : it_accountpayable .

      it_accountpayable-itemno_acc = v_itemno_acc .
      it_accountpayable-vendor_no  = gt_bseg-lifnr .
      it_accountpayable-bus_area   = gt_bseg-gsber .
      it_accountpayable-item_text  = gt_bseg-sgtxt .
      it_accountpayable-bline_date = gt_bseg-zfbdt .

      APPEND it_accountpayable .
    ELSEIF gt_200_belge-tip = 'K'.
      CLEAR :  it_accountreceivable .
      it_accountreceivable-itemno_acc = v_itemno_acc .
      it_accountreceivable-customer   = gt_bseg-kunnr .
      it_accountreceivable-bus_area   = gt_bseg-gsber .
      it_accountreceivable-item_text  = gt_bseg-sgtxt .
      it_accountreceivable-bline_date = gt_bseg-zfbdt .

      APPEND it_accountreceivable.
    ELSE.
      CLEAR : it_accountpayable .

      it_accountpayable-itemno_acc = v_itemno_acc .
*      it_accountpayable-vendor_no  = gt_bseg-lifnr .
      it_accountpayable-gl_account = gt_bseg-hkont.
      it_accountpayable-bus_area   = gt_bseg-gsber .
      it_accountpayable-item_text  = gt_bseg-sgtxt .
      it_accountpayable-bline_date = gt_bseg-zfbdt .

      APPEND it_accountpayable .
    ENDIF.

    CLEAR: it_currencyamount.
    it_currencyamount-itemno_acc = v_itemno_acc .
    it_currencyamount-curr_type = '00'.
    it_currencyamount-currency = gs_200_baslik-waers .
    it_currencyamount-exch_rate = gs_200_baslik-kursf .
    it_currencyamount-amt_doccur = lv_top_tutar * -1 .
    APPEND it_currencyamount .
  ENDLOOP.


  LOOP AT gt_200_belge WHERE kalem = 'C' AND tutar_son IS NOT INITIAL.

    ADD 1 TO v_itemno_acc.
    it_accountgl-itemno_acc = v_itemno_acc.
*      it_accountgl-item_text  = gt_bseg-sgtxt.
*      it_accountgl-tax_code   = gt_bseg-mwskz.
    it_accountgl-costcenter = gs_200_baslik-kostl.
    it_accountgl-gl_account = gt_200_belge-hkont.
    it_accountgl-bus_area   = gs_200_baslik-gsber.
    it_accountgl-value_date = gv_tarih_200 .
    it_accountgl-orderid    = gs_200_baslik-aufnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = it_accountgl-gl_account
      IMPORTING
        output = it_accountgl-gl_account.
    APPEND it_accountgl.
    CLEAR it_accountgl.


    it_currencyamount-itemno_acc = v_itemno_acc.
    it_currencyamount-currency   = gs_200_baslik-waers .
    it_currencyamount-amt_doccur = gt_200_belge-tutar_son.
    IF gt_200_belge-shkzg_son = 'H'.
      it_currencyamount-amt_doccur = it_currencyamount-amt_doccur * -1.
    ENDIF.
    lv_top_tutar = lv_top_tutar + it_currencyamount-amt_doccur.
    APPEND it_currencyamount .
    CLEAR : it_currencyamount.

  ENDLOOP.

  DATA: lv_obj    TYPE bapiache09-obj_key,
        lv_msg    TYPE string.
  break xtozcelik.
  CHECK it_currencyamount[] IS NOT INITIAL.
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader    = documentheader
    IMPORTING
      obj_key           = lv_obj
    TABLES
      accountgl         = it_accountgl[]
      accountpayable    = it_accountpayable[]
      accountreceivable = it_accountreceivable
*     accounttax        = it_accounttax[]
      currencyamount    = it_currencyamount[]
*     accountwt         = it_accountwt[]
      return            = it_return[].
  LOOP AT it_return WHERE type CA 'EAX'.
    MESSAGE ID it_return-id
            TYPE it_return-type
            NUMBER it_return-number
              WITH it_return-message_v1
                   it_return-message_v2
                   it_return-message_v3
                   it_return-message_v4
              INTO gv_dummy.
    PERFORM sys_add_bapiret2 TABLES gt_message.
  ENDLOOP.
  IF sy-subrc NE 0.
    CONCATENATE lv_obj(10) 'No''''lu belgenin ters kaydı alınmıştır.'
               INTO lv_msg SEPARATED BY space.
    MESSAGE lv_msg TYPE 'I'.
*    MESSAGE s031(zco) WITH
*    lv_obj(10) lv_obj+10(4) lv_obj+14(4) INTO gv_dummy.
*    PERFORM sys_add_bapiret2 TABLES gt_message.
    gs_200_baslik-durum = 'I'.
    MODIFY zfi_pes_gid_bas FROM gs_200_baslik.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    cv_error = 'X'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .

  ENDIF.
*--------------------------------------------------------------------*
*--------------------------------------------------------------------*
*--------------------------------------------------------------------*
*  DATA: lv_ktosl TYPE bseg-ktosl.
*  DATA: lv_wrbtr TYPE bseg-wrbtr.
*  DATA: lv_toplam TYPE bseg-wrbtr.
*  READ TABLE gt_200 INDEX 1.
*  DATA: lv_item LIKE it_accounttax-itemno_acc.
*  CLEAR: lv_ktosl, lv_wrbtr, lv_toplam.
*  IF gt_200-mwskz IS NOT INITIAL.
*    SELECT SINGLE wrbtr ktosl FROM bseg INTO lv_ktosl
*                             WHERE bukrs = gt_200-awkey+10(4)
*                               AND belnr = gt_200-awkey(10)
*                               AND gjahr = gt_200-awkey+14(4)
*                               AND ktosl NE space
*                               AND buzid EQ 'T'.
*  ENDIF.
*
*
*  documentheader-comp_code = gs_200_baslik-bukrs .
*  documentheader-username = sy-uname .
*  documentheader-doc_date = gv_tarih_200 .
*  documentheader-pstng_date = gv_tarih_200.
*  documentheader-trans_date = gv_tarih_200.
*  documentheader-fisc_year  = gv_tarih_200(4) .
*  documentheader-fis_period = gv_tarih_200+4(2) .
*  documentheader-doc_type = 'PO' .
*  documentheader-ref_doc_no = gs_200_baslik-policeno .
*
*  CLEAR: lv_item, lv_toplam.
*  LOOP AT gt_200_belge WHERE kalem = 'A'.
*
*    CLEAR : it_accountgl , it_currencyamount .
*    lv_item = lv_item + 1.
*   IF gs_200_baslik-mwskz EQ 'S3' AND ( gt_200_belge-hkont+0(3) EQ
*'191'
*                             OR gt_200_belge-hkont+0(3) EQ '360' )
*                             .
*      it_accounttax-itemno_acc = lv_item .
*      it_accounttax-gl_account = gs_200_baslik-hkont .
*      IF gt_200_belge-hkont+0(3) = '191'.
*        it_accounttax-acct_key = 'VST' .
*      ELSEIF gt_200_belge-hkont+0(3) = '360'.
*        it_accounttax-acct_key = 'ZST' .
*      ELSE.
*        it_accounttax-acct_key = ktosl .
*      ENDIF.
*      it_accounttax-tax_code = gs_200_baslik-mwskz .
*
*      APPEND it_accounttax .
*
*      it_currencyamount-itemno_acc = lv_item .
*      it_currencyamount-curr_type  = '00'  .
*      it_currencyamount-currency   = gs_200_baslik-waers .
*      it_currencyamount-exch_rate = gs_200_baslik-kursf .
*      it_currencyamount-amt_doccur = gt_200_belge-tutar_son .
*      IF gt_200_belge-shkzg_son EQ 'H'.
*       it_currencyamount-amt_doccur = it_currencyamount-amt_doccur *
*-1.
*      ENDIF.
**          IF gt_data-mwskz EQ 'S3' AND  ( gt_taksit_tum-hesap+0(3) EQ
**          '191'
**                                  OR gt_taksit_tum-hesap+0(3) EQ
*'360' )
**                                  .
**            IF  gt_200_belge-hkont+0(3) = '360'.
**              it_currencyamount-amt_doccur = gt_200-tutar_son  * -1.
**            ELSE.
**              it_currencyamount-amt_doccur = gt_200-tutar_son  .
**            ENDIF.
*      it_currencyamount-amt_base =
*      it_currencyamount-amt_doccur * 100 / 18.
**          ENDIF.
*      APPEND it_currencyamount .
*
*    ELSE.
*      it_accountgl-itemno_acc = lv_item .
*      it_accountgl-gl_account = gt_200_belge-hkont .
*      it_accountgl-costcenter = lv_ktosl .
*      it_accountgl-bus_area   = gs_200_baslik-gsber .
**          it_accountgl-material = gt_data-matnr .
**          it_accountgl-base_uom = gt_data-meins .
**          it_accountgl-quantity = gt_data-menge .
*      it_accountgl-orderid = gs_200_baslik-aufnr .
*      it_accountgl-value_date = gv_tarih_200 .
*      it_accountgl-item_text  = gs_200_baslik-sgtxt .
*      it_accountgl-tax_code = gs_200_baslik-mwskz .
*
*      APPEND it_accountgl .
*
*      it_currencyamount-itemno_acc = lv_item .
*      it_currencyamount-curr_type  = '00' .
*      it_currencyamount-currency   = gs_200_baslik-waers .
*      it_currencyamount-exch_rate = gs_200_baslik-kursf .
*      it_currencyamount-amt_doccur = gt_200_belge-tutar_son .
*      IF gt_200_belge-shkzg_son EQ 'H'.
*       it_currencyamount-amt_doccur = it_currencyamount-amt_doccur *
*-1.
*      ENDIF.
*      APPEND it_currencyamount .
*
*
*    ENDIF.
*    lv_toplam = lv_toplam + gt_200_belge-tutar_son.
*  ENDLOOP.
*
*  IF NOT ( gs_200_baslik-mwskz EQ 'S3' AND
*         ( gt_200_belge-hkont+0(3) EQ '191'
*        OR gt_200_belge-hkont+0(3) EQ '360' ) ).
*    lv_item = lv_item + 1.
*    CLEAR : it_accounttax , it_currencyamount  .
*
*    it_accounttax-itemno_acc = lv_item .
*    it_accounttax-acct_key = lv_ktosl .
*    it_accounttax-tax_code = gs_200_baslik-mwskz .
*
*    APPEND it_accounttax .
*
*    it_currencyamount-itemno_acc = lv_item .
*    it_currencyamount-curr_type  = '00'.
*    it_currencyamount-currency   = gs_200_baslik-waers .
*    it_currencyamount-exch_rate = gs_200_baslik-kursf .
*    it_currencyamount-amt_doccur = lv_wrbtr .
*    it_currencyamount-amt_base = lv_toplam .
*    APPEND it_currencyamount .
*  ENDIF.
*
*
*  IF gs_200_baslik-lifnr IS NOT INITIAL.
*    READ TABLE gt_200_belge WITH KEY kalem = 'B'.
*    lv_item = lv_item + 1 .
*    it_accountpayable-itemno_acc = lv_item .
*    it_accountpayable-vendor_no = gs_200_baslik-lifnr .
*    it_accountpayable-bus_area = gs_200_baslik-gsber .
*    it_accountpayable-item_text = gs_200_baslik-sgtxt .
*    it_accountpayable-bline_date = gv_tarih_200 .
*
*    APPEND it_accountpayable .
*
*    it_currencyamount-itemno_acc = lv_item .
*    it_currencyamount-curr_type = '00'.
*    it_currencyamount-currency = gs_200_baslik-waers .
*    it_currencyamount-exch_rate = gs_200_baslik-kursf .
*    it_currencyamount-amt_doccur = gt_200_belge-tutar_son .
*    APPEND it_currencyamount .
*
*    CLEAR : it_lfbw , it_lfbw[] .
*    SELECT * FROM lfbw INTO TABLE it_lfbw
*        WHERE bukrs EQ gs_200_baslik-bukrs AND
*              lifnr EQ gs_200_baslik-lifnr .
*    LOOP AT it_lfbw .
*      CLEAR : it_accountwt .
*      it_accountwt-itemno_acc = gt_taksit_tum-buzei .
*      it_accountwt-wt_type = it_lfbw-witht .
*      it_accountwt-wt_code = it_lfbw-wt_withcd .
*      APPEND it_accountwt .
*    ENDLOOP .
*
*
*  ELSE.
*
*  ENDIF.
ENDFORM.                    "ters_kayit
*&---------------------------------------------------------------------*
*&      Form  GET_TARIH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_TARIH_200  text
*----------------------------------------------------------------------*
FORM get_tarih  CHANGING cv_tarih_200
                         cv_tarih_200_post.

  CLEAR: cv_tarih_200, cv_tarih_200_post.


  DATA: ls_fields LIKE sval .
  DATA: lt_fields LIKE TABLE OF sval .
  DATA: return_code TYPE c.

  CLEAR: ls_fields.
  ls_fields-tabname = 'SYST'.
  ls_fields-fieldname = 'DATUM'.
  ls_fields-fieldtext = 'Belge Tarihi'.
  ls_fields-field_obl  = 'X'.
*  ls_fields-novaluehlp  = 'X'.
  APPEND ls_fields TO lt_fields.

  CLEAR: ls_fields.
  ls_fields-tabname = 'BKPF'.
  ls_fields-fieldname = 'BUDAT'.
  ls_fields-fieldtext = 'Kayıt Tarihi'.
  ls_fields-field_obl  = 'X'.
*  ls_fields-novaluehlp  = 'X'.
  APPEND ls_fields TO lt_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
*     NO_VALUE_CHECK  = ' '
      popup_title     = 'Tarih Bilgilerini Giriniz.'
      start_column    = '5'
      start_row       = '5'
    IMPORTING
      returncode      = return_code
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  READ TABLE lt_fields INTO ls_fields INDEX 1.
  CONDENSE ls_fields-value.
  cv_tarih_200 = ls_fields-value.

  READ TABLE lt_fields INTO ls_fields INDEX 2.
  CONDENSE ls_fields-value.
  cv_tarih_200_post = ls_fields-value.
ENDFORM.                    "get_tarih

*&---------------------------------------------------------------------*
*&      Form  SYS_ADD_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sys_add_bapiret2 TABLES pt_message STRUCTURE bapiret2 .

  DATA : ls_return TYPE bapiret2  .

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
