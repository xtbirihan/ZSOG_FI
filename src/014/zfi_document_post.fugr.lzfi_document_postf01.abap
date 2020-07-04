*----------------------------------------------------------------------*
***INCLUDE LZFI_DOCUMENT_POSTF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_data .
 REFRESH: t_bseg, it_return ,
  it_criteria , it_valuefield .

  CLEAR : l_type, l_key, l_sys, lv_itemno, lv_land1, lv_waer1,
  lv_waer3, lv_waer4, s_bkpf, gd_documentheader,
  t_bseg , it_return,
  it_criteria , it_valuefield , it_accounttax.

  CLEAR : it_currencyamount , it_currencyamount[],
  it_accountreceivable , it_accountreceivable[] ,
  it_accountgl , it_accountgl[] , it_accounttax ,
  it_accounttax[] , it_accountpayable , it_accountpayable[].
ENDFORM.                    " CLEAR_DATA

FORM fill_header .

  IF s_bkpf-usnam IS INITIAL .
    s_bkpf-usnam = sy-uname .
  ENDIF .

  IF s_bkpf-waers IS INITIAL .
    s_bkpf-waers = 'TRY' .
  ENDIF .

  IF s_bkpf-budat IS INITIAL .
    s_bkpf-budat = sy-datum .
  ENDIF .

  IF s_bkpf-bldat IS INITIAL .
    s_bkpf-bldat = sy-datum .
  ENDIF .

  IF s_bkpf-gjahr IS INITIAL .
    s_bkpf-gjahr = s_bkpf-budat(4) .
  ENDIF .

  IF s_bkpf-monat IS INITIAL .
    s_bkpf-monat = s_bkpf-budat+4(2) .
  ENDIF .

  gd_documentheader-username   = s_bkpf-usnam.
  gd_documentheader-header_txt = s_bkpf-bktxt.
  gd_documentheader-comp_code  = s_bkpf-bukrs.
  gd_documentheader-fisc_year  = s_bkpf-gjahr.
  gd_documentheader-doc_date   = s_bkpf-bldat.
  gd_documentheader-pstng_date = s_bkpf-budat.
  gd_documentheader-trans_date = s_bkpf-wwert.
  gd_documentheader-fis_period = s_bkpf-monat.
  gd_documentheader-doc_type   = s_bkpf-blart.
  gd_documentheader-ref_doc_no = s_bkpf-xblnr.
  gd_documentheader-bus_act    = s_bkpf-glvor.

*  IF gv_onkayit IS NOT INITIAL .
*    gd_documentheader-ref_doc_no_long = 'Ön Kayıt' .
*  ENDIF .

  IF s_bkpf-ldgrp IS NOT INITIAL.

    SELECT SINGLE acc_principle INTO gd_documentheader-acc_principle
    FROM tacc_trgt_ldgr WHERE ldgrp_gl EQ s_bkpf-ldgrp.
  ENDIF.

  SELECT SINGLE waers INTO lv_waer1 FROM t001
  WHERE bukrs EQ s_bkpf-bukrs.

  SELECT SINGLE mwaer INTO lv_waer3 FROM t000 CLIENT SPECIFIED
  WHERE mandt EQ sy-mandt.

  SELECT SINGLE curha INTO lv_waer4 FROM t005
  WHERE land1 EQ lv_land1.

  select single ktopl from t001
         into gv_ktopl
         where bukrs eq s_bkpf-bukrs .

ENDFORM.                    " fill_header
*&--------------------------------------------------------------------*
*&      Form  fill_item
*&--------------------------------------------------------------------*
FORM fill_item .

  DATA : lv_ktosl TYPE accit-ktosl.
  DATA : lv_kbetr TYPE kbetr.

  LOOP AT t_bseg .

    ADD 1 TO lv_itemno. CLEAR it_accountgl.

    IF gv_onkayit IS NOT INITIAL .
      t_bseg-xref3 = 'Ön Kayıt' .
    ENDIF .

************************************************************* Account
    IF t_bseg-kunnr IS NOT INITIAL .
      it_accountreceivable-itemno_acc  = lv_itemno.
      it_accountreceivable-customer       = t_bseg-kunnr.
      it_accountreceivable-item_text   = t_bseg-sgtxt.
      it_accountreceivable-bus_area    = t_bseg-gsber.
      it_accountreceivable-alloc_nmbr  = t_bseg-zuonr.
      it_accountreceivable-ref_key_3   = t_bseg-xref3 .
      if not t_bseg-zfbdt is initial .
        it_accountreceivable-BLINE_DATE = t_bseg-zfbdt .
      endif .
      APPEND it_accountreceivable.
    ELSEIF t_bseg-lifnr IS NOT INITIAL .
      it_accountpayable-itemno_acc  = lv_itemno.
      it_accountpayable-vendor_no   = t_bseg-lifnr.
      it_accountpayable-item_text   = t_bseg-sgtxt.
      it_accountpayable-bus_area    = t_bseg-gsber.
      it_accountpayable-alloc_nmbr  = t_bseg-zuonr.
      it_accountpayable-ref_key_3   = t_bseg-xref3 .
      if not t_bseg-zfbdt is initial .
        it_accountpayable-BLINE_DATE = t_bseg-zfbdt .
      endif .
      APPEND it_accountpayable.
    ELSE .
      if t_bseg-anln1 is not initial .
        it_accountgl-ASSET_NO = t_bseg-anln1 .
        it_accountgl-SUB_NUMBER = t_bseg-anln2 .
        it_accountgl-acct_type = 'A' .

        select single ktansw
           from t095 join anla on t095~ktogr eq anla~ktogr
        into it_accountgl-gl_account
        where anla~anln1 eq t_bseg-anln1 and
              anla~anln2 eq t_bseg-anln2 and
              anla~bukrs eq s_bkpf-bukrs and
              t095~ktopl eq gv_ktopl.


      else .
        it_accountgl-gl_account  = t_bseg-hkont.
      endif .
        it_accountgl-itemno_acc  = lv_itemno.
        it_accountgl-item_text   = t_bseg-sgtxt.
        it_accountgl-costcenter  = t_bseg-kostl.
        it_accountgl-bus_area    = t_bseg-gsber.
        it_accountgl-plant       = t_bseg-werks.
        it_accountgl-material    = t_bseg-matnr.
        it_accountgl-alloc_nmbr  = t_bseg-zuonr.
        it_accountgl-quantity    = t_bseg-menge.
        it_accountgl-base_uom    = t_bseg-meins.
        it_accountgl-trade_id    = t_bseg-vbund.
        it_accountgl-cmmt_item   = t_bseg-fipos.
        it_accountgl-ref_key_3   = t_bseg-xref3 .
        it_accountgl-ref_key_2   = t_bseg-xref2 .
        it_accountgl-orderid     = t_bseg-aufnr .
*        it_accountgl-hbkid       = t_bseg-hbkid . "20131226:fergins
*        it_accountgl-hktid       = t_bseg-hktid . "20131226:fergins
        if not t_bseg-valut is initial .
          it_accountgl-VALUE_DATE  = t_bseg-valut .
        endif .

      APPEND it_accountgl.

*      CLEAR skb1 .
*      SELECT SINGLE * FROM skb1 WHERE bukrs EQ s_bkpf-bukrs AND
*      saknr EQ t_bseg-hkont .
*      IF sy-subrc IS INITIAL .
*        IF skb1-mitkz IS NOT INITIAL .
*          RAISE dogrudan_kayit .
*        ENDIF .
*      ENDIF .

    ENDIF .

************************************************************** Tax
*    IF t_bseg-mwskz IS NOT INITIAL.
*      add 1 to lv_itemno .
*      PERFORM determine_tax_values USING    t_bseg-mwskz
*                                   CHANGING lv_kbetr
*                                            lv_ktosl.
*      CLEAR it_accounttax.
*      it_accounttax-itemno_acc = lv_itemno.
*      it_accounttax-acct_key   = lv_ktosl.
*      it_accounttax-tax_code   = t_bseg-mwskz.
*      APPEND it_accounttax.
*
*    ENDIF.

************************************************************** Currency
*    CLEAR it_currencyamount.
*    it_currencyamount-itemno_acc   = lv_itemno.
*    it_currencyamount-curr_type    = '00'.
*    it_currencyamount-currency     = s_bkpf-waers.
*    IF t_bseg-mwskz IS NOT INITIAL.
*      it_currencyamount-tax_amt = ( t_bseg-wrbtr * lv_kbetr / 1000 ).
*      it_currencyamount-amt_base =
*              t_bseg-wrbtr - ( t_bseg-wrbtr * lv_kbetr / 1000 ).
*      IF it_currencyamount-tax_amt = 0.
*        it_currencyamount-amt_doccur = it_currencyamount-amt_base.
*      ENDIF.
*    ELSE.
*      it_currencyamount-amt_doccur   = t_bseg-wrbtr.
*    ENDIF.
*    APPEND it_currencyamount.
*if t_bseg-wrbtr <> 0 .
    CLEAR it_currencyamount.
    it_currencyamount-itemno_acc   = lv_itemno.
    it_currencyamount-curr_type    = '00'.
    it_currencyamount-currency     = s_bkpf-waers.
    it_currencyamount-exch_rate    = s_bkpf-kursf .
    IF t_bseg-mwskz IS NOT INITIAL .

      PERFORM determine_tax_values USING  t_bseg-mwskz
      CHANGING lv_kbetr
        lv_ktosl.

*      if t_bseg-wrbtr is not initial .
        it_currencyamount-amt_doccur =
        t_bseg-wrbtr - ( t_bseg-wrbtr * lv_kbetr / 1000 ).
        APPEND it_currencyamount .
        ADD 1 TO lv_itemno .
        it_currencyamount-itemno_acc   = lv_itemno.
        it_currencyamount-curr_type    = '00'.
        it_currencyamount-currency     = s_bkpf-waers.
        it_currencyamount-exch_rate    = s_bkpf-kursf .
        it_currencyamount-tax_amt = ( t_bseg-wrbtr * lv_kbetr / 1000 ).
        it_currencyamount-amt_doccur = it_currencyamount-tax_amt .
        it_currencyamount-amt_base =
        t_bseg-wrbtr - ( t_bseg-wrbtr * lv_kbetr / 1000 ).

        IF it_currencyamount-tax_amt = 0.
          it_currencyamount-amt_doccur = it_currencyamount-amt_base.
        ENDIF.

*      elseif t_bseg-dmbtr is not initial .
*        it_currencyamount-amt_doccur =
*        t_bseg-dmbtr - ( t_bseg-dmbtr * lv_kbetr / 1000 ).
*        APPEND it_currencyamount .
*        ADD 1 TO lv_itemno .
*        it_currencyamount-itemno_acc   = lv_itemno.
*        it_currencyamount-curr_type    = '10'.
*        it_currencyamount-currency     = s_bkpf-waers.
*        it_currencyamount-tax_amt = ( t_bseg-dmbtr * lv_kbetr / 1000 ).
*        it_currencyamount-amt_doccur = it_currencyamount-tax_amt .
*        it_currencyamount-amt_base =
*        t_bseg-wrbtr - ( t_bseg-dmbtr * lv_kbetr / 1000 ).
*
*        IF it_currencyamount-tax_amt = 0.
*          it_currencyamount-amt_doccur = it_currencyamount-amt_base.
*        ENDIF.
*
*      endif .
      APPEND it_currencyamount .

      CLEAR it_accounttax.
      it_accounttax-itemno_acc = lv_itemno.
      it_accounttax-acct_key   = lv_ktosl.
      it_accounttax-tax_code   = t_bseg-mwskz.
      APPEND it_accounttax.
    ELSE .
      it_currencyamount-amt_doccur   = t_bseg-wrbtr.
      APPEND it_currencyamount .
    ENDIF .
*endif .
*    ENDIF.
*    APPEND it_currencyamount.

************************************************************** PB 1
    IF t_bseg-dmbtr <> 0.
      CLEAR it_currencyamount.

      it_currencyamount-itemno_acc   = lv_itemno.
      it_currencyamount-curr_type    = '10'.
      it_currencyamount-currency     = lv_waer1.
      it_currencyamount-amt_doccur   = t_bseg-dmbtr.
      APPEND it_currencyamount .
    ENDIF.

************************************************************** PB 2
    IF t_bseg-dmbe2 <> 0.
      CLEAR it_currencyamount.

      it_currencyamount-itemno_acc   = lv_itemno.
      it_currencyamount-curr_type    = '30'.
      it_currencyamount-currency     = lv_waer3.
      it_currencyamount-amt_doccur   = t_bseg-dmbe2.
      APPEND it_currencyamount.
    ENDIF.

************************************************************** PB 3
    IF t_bseg-dmbe3 <> 0.
      CLEAR it_currencyamount.

      it_currencyamount-itemno_acc   = lv_itemno.
      it_currencyamount-curr_type    = '40'.
      it_currencyamount-currency     = lv_waer4.
      it_currencyamount-amt_doccur   = t_bseg-dmbe3.
      APPEND it_currencyamount.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " fill_item
*&---------------------------------------------------------------------*
*&      Form  call_bapi
*&---------------------------------------------------------------------*
FORM call_bapi .

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader    = gd_documentheader
    IMPORTING
      obj_type          = l_type
      obj_key           = l_key
      obj_sys           = l_sys
    TABLES
      accountgl         = it_accountgl
      accountreceivable = it_accountreceivable
      accountpayable    = it_accountpayable
      currencyamount    = it_currencyamount
      accounttax        = it_accounttax
      criteria          = it_criteria
      valuefield        = it_valuefield
*     extension         = it_extension
      return            = it_return.

  READ TABLE it_return WITH KEY type = 'E'.
  IF sy-subrc NE 0.
    IF gv_auto_commit = 'X'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.                    " call_bapi

FORM determine_tax_values  USING    ip_kdv
CHANGING cp_kbetr
  cp_ktosl.

  DATA: lt_ftaxp LIKE ftaxp OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'GET_TAX_PERCENTAGE'
    EXPORTING
      aland   = 'TR'
      datab   = sy-datum
      mwskz   = ip_kdv
      txjcd   = 'X'
    TABLES
      t_ftaxp = lt_ftaxp.

  READ TABLE lt_ftaxp INDEX 1.
  MOVE lt_ftaxp-kbetr TO cp_kbetr.

  CALL FUNCTION 'RE_GET_TAXCODE_INFO_KTOSL'
    EXPORTING
      i_mwskz = ip_kdv
      i_bukrs = gd_documentheader-comp_code
    IMPORTING
      e_ktosl = cp_ktosl.

ENDFORM.                    " DETERMINE_TAX_VALUES
*&---------------------------------------------------------------------*
