*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_DOCUMENT
*&---------------------------------------------------------------------*

*- BADI : BADI_ACC_DOCUMENT / ACC_DOCUMENT
*&---------------------------------------------------------------------*
*&  Include           ZFI_DOCUMENT
*&---------------------------------------------------------------------*
DATA : gs_bkpf TYPE bkpf ,
       gs_bseg TYPE bseg .

DATA : bs_header     TYPE bapiache09                           ,
       obj_type      TYPE bapiache09-obj_type                  ,
       obj_key       TYPE bapiache09-obj_key                   ,
       obj_sys       TYPE bapiache09-obj_sys                   ,
       bt_account    TYPE TABLE OF bapiacgl09 WITH HEADER LINE ,
       bt_amount     TYPE TABLE OF bapiaccr09 WITH HEADER LINE ,
       bt_payable    TYPE TABLE OF bapiacap09 WITH HEADER LINE ,
       bt_tax        TYPE TABLE OF bapiactx09 WITH HEADER LINE ,
       bt_return     TYPE TABLE OF bapiret2   WITH HEADER LINE ,
       bt_receivable TYPE TABLE OF bapiacar09 WITH HEADER LINE .

DATA : gv_buzei TYPE bseg-buzei .

*------------------ CONVERSION_EXIT_ALPHA_INPUT -----------------------*
DEFINE conversion_exit_alpha_input.
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = &1
    importing
      output = &1.
END-OF-DEFINITION.

DEFINE conversion_exit_alpha_two.
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = &1
    importing
      output = &2.
END-OF-DEFINITION.

*------------------------------ MACRO ---------------------------------*
DEFINE change_dot              .
  replace ',' with '*' into &1 .
  replace '.' with ',' into &1 .
  replace '*' with '.' into &1 .
END-OF-DEFINITION              .
*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_HEADER
*&---------------------------------------------------------------------*
*       Başlık Bilgileri
*----------------------------------------------------------------------*
FORM document_header USING p_header LIKE bkpf.

  CLEAR   : bs_header , obj_type  , obj_key , obj_sys  , bt_account ,
            bt_amount , bt_payable,
            bt_tax    , bt_return , bt_receivable  .

  REFRESH : bt_account, bt_amount, bt_payable, bt_tax  , bt_return ,
            bt_receivable.


  bs_header-bus_act    = 'RFBU'              .
  bs_header-username   = sy-uname            .
  bs_header-comp_code  = p_header-bukrs      .
  bs_header-doc_date   = p_header-bldat      .
  bs_header-pstng_date = p_header-budat      .
  bs_header-trans_date = p_header-budat      .
  bs_header-fisc_year  = p_header-budat+0(4) .
  bs_header-fis_period = p_header-budat+4(2) .
  bs_header-doc_type   = p_header-blart      .
  bs_header-header_txt = p_header-bktxt      .
  bs_header-ref_doc_no = p_header-xblnr      .
*-------------------------------------------------------
  bs_header-acc_principle  = p_header-ldgrp      .
*-------------------------------------------------------


ENDFORM.                    " DOCUMENT_HEADER
*&---------------------------------------------------------------------*
*&      Form  ADD_ACCOUNTGL
*&---------------------------------------------------------------------*
*       Ana Hesap
*----------------------------------------------------------------------*
FORM add_accountgl  USING p_item   TYPE bseg
                          p_header TYPE bkpf .

  DATA: lv_pyp(24).

  bt_account-itemno_acc = p_item-buzei   .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_item-hkont
    IMPORTING
      output = p_item-hkont.
  bt_account-gl_account = p_item-hkont   .
  bt_account-value_date = p_header-bldat .
  bt_account-doc_type   = p_header-blart .
  bt_account-item_text  = p_item-sgtxt   .
  bt_account-ref_key_1  = p_item-xref1   .
  bt_account-ref_key_2  = p_item-xref2   .
  bt_account-ref_key_3  = p_item-xref3   .
  bt_account-profit_ctr = p_item-prctr   .
  bt_account-costcenter = p_item-kostl   .
  bt_account-value_date = p_item-valut   .
  bt_account-orderid    = p_item-aufnr   .
  bt_account-comp_code  = p_item-bukrs   .
  bt_account-tax_code   = p_item-mwskz   .
  bt_account-alloc_nmbr = p_item-zuonr   .
  bt_account-material   = p_item-matnr   .
  bt_account-bus_area   = p_item-gsber.

  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
    EXPORTING
      input     = p_item-projk
    IMPORTING
      output    = lv_pyp
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  bt_account-wbs_element = lv_pyp.
  APPEND bt_account.
ENDFORM.                    " ADD_ACCOUNTGL
*&---------------------------------------------------------------------*
*&      Form  ADD_ACCOUNTPAYABLE
*&---------------------------------------------------------------------*
*       Satıcı
*----------------------------------------------------------------------*
FORM add_accountpayable  USING p_item   TYPE bseg
                               p_header TYPE bkpf .
  conversion_exit_alpha_input : p_item-hkont .
  bt_payable-itemno_acc = p_item-buzei  .
  bt_payable-bline_date = p_header-budat.
  bt_payable-alloc_nmbr = p_item-zuonr.
  bt_payable-item_text  = p_item-sgtxt  .
  bt_payable-sp_gl_ind  = p_item-umskz  .
  bt_payable-vendor_no  = p_item-lifnr  .
  bt_payable-ref_key_1  = p_item-xref1  .
  bt_payable-ref_key_2  = p_item-xref2  .
  bt_payable-ref_key_3  = p_item-xref3  .
  bt_payable-comp_code  = p_item-bukrs  .
  bt_payable-sp_gl_ind  = p_item-umskz.
  bt_payable-bline_date = p_item-zfbdt.
  bt_payable-pmnttrms   = p_item-zterm.
  bt_payable-bus_area   = p_item-gsber.
  APPEND bt_payable.
ENDFORM.                    " ADD_ACCOUNTPAYABLE
*&---------------------------------------------------------------------*
*&      Form  ADD_ACCOUNTRECEIVABLE
*&---------------------------------------------------------------------*
*       Müşteri
*----------------------------------------------------------------------*
FORM add_accountreceivable USING p_item   TYPE bseg
                                 p_header TYPE bkpf .
  conversion_exit_alpha_input : p_item-hkont.
  bt_receivable-itemno_acc = p_item-buzei  .
  bt_receivable-bline_date = p_header-budat.
  bt_receivable-item_text  = p_item-sgtxt  .
  bt_receivable-sp_gl_ind  = p_item-umskz  .
  bt_receivable-customer   = p_item-kunnr  .
  bt_receivable-ref_key_1  = p_item-xref1  .
  bt_receivable-ref_key_2  = p_item-xref2  .
  bt_receivable-ref_key_3  = p_item-xref3  .
  bt_receivable-comp_code  = p_item-bukrs  .
  bt_receivable-alloc_nmbr = p_item-zuonr.
  bt_receivable-sp_gl_ind  = p_item-umskz.
  bt_receivable-bline_date = p_item-zfbdt.
  bt_receivable-pmnttrms   = p_item-zterm.
  bt_receivable-bus_area   = p_item-gsber.
  APPEND bt_receivable.
ENDFORM.                    " ADD_ACCOUNTRECEIVABLE
*&---------------------------------------------------------------------*
*&      Form  ADD_CURRENCYAMOUNT
*&---------------------------------------------------------------------*
*       Tutar Bilgisi
*----------------------------------------------------------------------*
FORM add_currencyamount USING p_item   TYPE bseg
                              p_header TYPE bkpf .

  bt_amount-itemno_acc = p_item-buzei  .
  bt_amount-curr_type  = '00'          . " Belge para birimi
  bt_amount-currency   = p_header-waers.
  bt_amount-amt_doccur = p_item-wrbtr  .
  bt_amount-exch_rate  = p_header-kursf.

  IF p_item-fwbas IS NOT INITIAL .
    bt_amount-amt_base   = p_item-fwbas  .
  ELSE.
    bt_amount-amt_base   = p_item-navfw  .
  ENDIF.
  APPEND bt_amount.

  IF p_item-dmbtr IS NOT INITIAL.
    bt_amount-itemno_acc = p_item-buzei.
    bt_amount-curr_type  = '10'         . " Şirket para birimi
    bt_amount-currency   = 'TRY'        .
    bt_amount-amt_doccur = p_item-dmbtr .
    APPEND bt_amount.
  ENDIF.

  IF p_item-dmbe2 IS NOT INITIAL.
    bt_amount-itemno_acc = p_item-buzei.
    bt_amount-curr_type  = '40'         . " Para birimi USD
    bt_amount-currency   = 'USD'        .
    bt_amount-amt_doccur = p_item-dmbe2 .
    APPEND bt_amount.
  ENDIF.

  IF p_item-dmbe3 IS NOT INITIAL.
    bt_amount-itemno_acc = p_item-buzei.
    bt_amount-curr_type  = '50'         . " Para birimi EUR
    bt_amount-currency   = 'EUR'        .
    bt_amount-amt_doccur = p_item-dmbe3 .
    APPEND bt_amount.
  ENDIF.

ENDFORM.                    " ADD_CURRENCYAMOUNT

*&---------------------------------------------------------------------*
*&      Form  ADD_ACCOUNTTAX
*&---------------------------------------------------------------------*
*       Vergi
*----------------------------------------------------------------------*
FORM add_accounttax USING p_item   TYPE bseg
                          p_header TYPE bkpf.


  CHECK p_item-mwskz IS NOT INITIAL.

  bt_tax-tax_code   = p_item-mwskz.
*  bt_tax-itemno_tax = p_item-buzei.
  gv_buzei          = gv_buzei + 1.
  p_item-buzei      = gv_buzei    .
  bt_tax-itemno_acc = p_item-buzei.
  APPEND bt_tax.

  DATA : lt_mwdat TYPE STANDARD TABLE OF rtax1u15 WITH HEADER LINE .


  bt_amount-itemno_acc = p_item-buzei  .
  bt_amount-curr_type  = '00'          . " Belge para birimi
  bt_amount-currency   = p_header-waers.
  bt_amount-exch_rate  = p_header-kursf.
  PERFORM calculate_tax_from_grossamount TABLES lt_mwdat
                                         USING p_item-bukrs
                                               p_item-mwskz
                                               p_header-waers
                                               p_item-wrbtr .
  READ TABLE lt_mwdat INDEX 1 .
  bt_amount-amt_base   = p_item-wrbtr - lt_mwdat-wmwst  .
  bt_amount-amt_doccur = lt_mwdat-wmwst                 .
  APPEND bt_amount.



  IF p_item-dmbtr IS NOT INITIAL.
    bt_amount-itemno_acc = p_item-buzei.
    bt_amount-curr_type  = '10'         . " Şirket para birimi
    bt_amount-currency   = 'TRY'        .

    PERFORM calculate_tax_from_grossamount TABLES lt_mwdat
                                         USING p_item-bukrs
                                               p_item-mwskz
                                               bt_amount-currency
                                               p_item-dmbtr .

    bt_amount-amt_base   = p_item-dmbtr - lt_mwdat-wmwst  .
    bt_amount-amt_doccur = lt_mwdat-wmwst                 .
    APPEND bt_amount.
  ENDIF.

  IF p_item-dmbe2 IS NOT INITIAL.
    bt_amount-itemno_acc = p_item-buzei.
    bt_amount-curr_type  = '40'         . " Para birimi USD
    bt_amount-currency   = 'USD'        .
    PERFORM calculate_tax_from_grossamount TABLES lt_mwdat
                                         USING p_item-bukrs
                                               p_item-mwskz
                                               bt_amount-currency
                                               p_item-dmbe2 .

    bt_amount-amt_base   = p_item-dmbe2 - lt_mwdat-wmwst  .
    bt_amount-amt_doccur = lt_mwdat-wmwst                 .
    APPEND bt_amount.
  ENDIF.

  IF p_item-dmbe3 IS NOT INITIAL.
    bt_amount-itemno_acc = p_item-buzei.
    bt_amount-curr_type  = '50'         . " Para birimi EUR
    bt_amount-currency   = 'EUR'        .
    PERFORM calculate_tax_from_grossamount TABLES lt_mwdat
                                         USING p_item-bukrs
                                               p_item-mwskz
                                               bt_amount-currency
                                               p_item-dmbe3 .

    bt_amount-amt_base   = p_item-dmbe3 - lt_mwdat-wmwst  .
    bt_amount-amt_doccur = lt_mwdat-wmwst                 .
    APPEND bt_amount.
  ENDIF.



ENDFORM.                    " ADD_ACCOUNTTAX
*&---------------------------------------------------------------------*
*&      Form  BAPI_ACC_DOCUMENT_POST
*&---------------------------------------------------------------------*
*       Muhasebe Verisini Kaydet
*----------------------------------------------------------------------*
FORM bapi_acc_document_post .

  CLEAR : bt_account    , bt_amount, bt_payable, bt_tax  , bt_return ,
          bt_receivable , obj_type , obj_key   , obj_sys .

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader    = bs_header
    IMPORTING
      obj_type          = obj_type
      obj_key           = obj_key
      obj_sys           = obj_sys
    TABLES
      accountgl         = bt_account
      accountreceivable = bt_receivable
      currencyamount    = bt_amount
      accountpayable    = bt_payable
      accounttax        = bt_tax
      return            = bt_return.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFORM.                    " BAPI_ACC_DOCUMENT_POST
*&---------------------------------------------------------------------*
*&      Form  BAPI_ACC_DOCUMENT_POST
*&---------------------------------------------------------------------*
*       Muhasebe Verisini Kaydet
*----------------------------------------------------------------------*
FORM bapi_acc_document_check .

  CLEAR : bt_account    , bt_amount, bt_payable, bt_tax  , bt_return ,
          bt_receivable , obj_type , obj_key   , obj_sys .

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
    EXPORTING
      documentheader    = bs_header
    TABLES
      accountgl         = bt_account
      accountreceivable = bt_receivable
      currencyamount    = bt_amount
      accountpayable    = bt_payable
      accounttax        = bt_tax
      return            = bt_return.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.


  READ TABLE bt_return INDEX 1.
  obj_type = bt_return-message_v1.
  obj_key  = bt_return-message_v2.
  obj_sys  = bt_return-message_v3.
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_DISPLAY'
    EXPORTING
      obj_type = obj_type
      obj_key  = obj_key
      obj_sys  = obj_sys
    IMPORTING
      return   = bt_return.


ENDFORM.                    " BAPI_ACC_DOCUMENT_POST
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_TAX_FROM_NET_AMOUNT
*&---------------------------------------------------------------------*
*       Netten Vergiyi Hesapla
*----------------------------------------------------------------------*
FORM calculate_tax_from_net_amount TABLES t_mwdat STRUCTURE rtax1u15
                                    USING p_bukrs TYPE bkpf-bukrs
                                          p_mwskz TYPE bseg-mwskz
                                          p_waers TYPE bkpf-waers
                                          p_wrbtr TYPE bseg-wrbtr .

  CLEAR : t_mwdat[] , t_mwdat .

  CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
    EXPORTING
      i_bukrs = p_bukrs
      i_mwskz = p_mwskz
      i_waers = p_waers
      i_wrbtr = p_wrbtr
    TABLES
      t_mwdat = t_mwdat.

  READ TABLE t_mwdat INDEX 1 .

ENDFORM.                    " CALCULATE_TAX_FROM_NET_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_TAX_FROM_GROSSAMOUNT
*&---------------------------------------------------------------------*
*       Brütten Vergiyi Hesapla
*----------------------------------------------------------------------*
FORM calculate_tax_from_grossamount TABLES t_mwdat STRUCTURE rtax1u15
                                     USING p_bukrs TYPE bkpf-bukrs
                                           p_mwskz TYPE bseg-mwskz
                                           p_waers TYPE bkpf-waers
                                           p_wrbtr TYPE bseg-wrbtr .

  CLEAR : t_mwdat[] , t_mwdat .

  CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
    EXPORTING
      i_bukrs = p_bukrs
      i_mwskz = p_mwskz
      i_waers = p_waers
      i_wrbtr = p_wrbtr
    TABLES
      t_mwdat = t_mwdat.

  READ TABLE t_mwdat INDEX 1 .

ENDFORM.                    " CALCULATE_TAX_FROM_GROSSAMOUNT
*&---------------------------------------------------------------------*
*&      Form  READ_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*       Kur Bilgisini Oku
*----------------------------------------------------------------------*
FORM read_exchange_rate USING p_datum TYPE sy-datum
                              p_waers TYPE fcurr_curr
                              p_local TYPE tcurr_curr
                              p_ukurs TYPE ukurs_curr.

  CHECK p_waers NE p_local.

  CALL FUNCTION 'READ_EXCHANGE_RATE'
    EXPORTING
      date             = p_datum
      foreign_currency = p_waers
      local_currency   = p_local
    IMPORTING
      exchange_rate    = p_ukurs
    EXCEPTIONS
      no_rate_found    = 1
      no_factors_found = 2
      no_spread_found  = 3
      derived_2_times  = 4
      overflow         = 5
      zero_rate        = 6
      OTHERS           = 7.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*&      Form  BAPI_ACC_DOCUMENT_REVERSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bapi_acc_document_reversal USING p_header TYPE bkpf
                                      p_reason TYPE stgrd .

  DATA : ls_reversal LIKE bapiacrev .

  CLEAR : bt_return , bt_return[] ,obj_type , obj_key , obj_sys .

  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system = ls_reversal-obj_sys.

  ls_reversal-obj_type   = p_header-awtyp.
  ls_reversal-obj_key    = p_header-awkey.
  ls_reversal-obj_key_r  = p_header-awkey.
  ls_reversal-pstng_date = p_header-budat.
  ls_reversal-fis_period = p_header-monat.
  ls_reversal-comp_code  = p_header-bukrs.
  ls_reversal-ac_doc_no  = p_header-belnr.
  ls_reversal-reason_rev = p_reason .


  CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
    EXPORTING
      reversal = ls_reversal
      bus_act  = p_header-glvor
    IMPORTING
      obj_type = obj_type
      obj_key  = obj_key
      obj_sys  = obj_sys
    TABLES
      return   = bt_return.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFORM.                    " BAPI_ACC_DOCUMENT_REVERSAL
*&---------------------------------------------------------------------*
*&      Form  ADD_ACCOUNTTAX
*&---------------------------------------------------------------------*
*       Vergi
*----------------------------------------------------------------------*
FORM add_accounttax_brut USING p_item   TYPE bseg
                               p_header TYPE bkpf .

  CHECK p_item-mwskz IS NOT INITIAL.

  bt_tax-tax_code   = p_item-mwskz.
  bt_tax-itemno_tax = p_item-buzei.
  gv_buzei          = gv_buzei + 1.
  p_item-buzei      = gv_buzei    .
  bt_tax-itemno_acc = p_item-buzei.
  APPEND bt_tax.

  DATA : lt_mwdat TYPE STANDARD TABLE OF rtax1u15 WITH HEADER LINE .


  bt_amount-itemno_acc = p_item-buzei  .
  bt_amount-curr_type  = '00'          . " Belge para birimi
  bt_amount-currency   = p_header-waers.
  bt_amount-exch_rate  = p_header-kursf.
  PERFORM calculate_tax_from_grossamount TABLES lt_mwdat
                                          USING p_item-bukrs
                                                p_item-mwskz
                                                p_header-waers
                                                p_item-wrbtr .
  READ TABLE lt_mwdat INDEX 1 .
  bt_amount-amt_base   = p_item-wrbtr - lt_mwdat-wmwst  .
  bt_amount-amt_doccur = lt_mwdat-wmwst                 .
  APPEND bt_amount.

  p_item-wrbtr = p_item-wrbtr - lt_mwdat-wmwst  .

  IF p_item-dmbtr IS NOT INITIAL.
    bt_amount-itemno_acc = p_item-buzei.
    bt_amount-curr_type  = '10'         . " Şirket para birimi
    bt_amount-currency   = 'YTL'        .

    PERFORM calculate_tax_from_grossamount TABLES lt_mwdat
                                         USING p_item-bukrs
                                               p_item-mwskz
                                               bt_amount-currency
                                               p_item-dmbtr .

    bt_amount-amt_base   = p_item-dmbtr - lt_mwdat-wmwst  .
    bt_amount-amt_doccur = lt_mwdat-wmwst                 .
    APPEND bt_amount.
  ENDIF.

  IF p_item-dmbe2 IS NOT INITIAL.
    bt_amount-itemno_acc = p_item-buzei.
    bt_amount-curr_type  = '40'         . " Para birimi USD
    bt_amount-currency   = 'USD'        .
    PERFORM calculate_tax_from_grossamount TABLES lt_mwdat
                                         USING p_item-bukrs
                                               p_item-mwskz
                                               bt_amount-currency
                                               p_item-dmbe2 .

    bt_amount-amt_base   = p_item-dmbe2 - lt_mwdat-wmwst  .
    bt_amount-amt_doccur = lt_mwdat-wmwst                 .
    APPEND bt_amount.
  ENDIF.

  IF p_item-dmbe3 IS NOT INITIAL.
    bt_amount-itemno_acc = p_item-buzei.
    bt_amount-curr_type  = '50'         . " Para birimi EUR
    bt_amount-currency   = 'EUR'        .
    PERFORM calculate_tax_from_grossamount TABLES lt_mwdat
                                         USING p_item-bukrs
                                               p_item-mwskz
                                               bt_amount-currency
                                               p_item-dmbe3 .

    bt_amount-amt_base   = p_item-dmbe3 - lt_mwdat-wmwst  .
    bt_amount-amt_doccur = lt_mwdat-wmwst                 .
    APPEND bt_amount.
  ENDIF.



ENDFORM.                    " ADD_ACCOUNTTAX
