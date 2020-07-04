FUNCTION-POOL ZFI_DOCUMENT_POST.            "MESSAGE-ID ..

* INCLUDE LZFI_DOCUMENT_POSTD...             " Local class definition


TABLES : skb1 , bsid , bkpf.

DATA: s_bkpf LIKE bkpf,
      gd_documentheader LIKE bapiache09,
      t_bseg            LIKE TABLE OF bseg WITH HEADER LINE,
      it_accountgl      LIKE TABLE OF bapiacgl09 WITH HEADER LINE,
      it_accountreceivable LIKE TABLE OF bapiacar09  WITH HEADER LINE,
      it_accountpayable LIKE TABLE OF bapiacap09 WITH HEADER LINE ,
      it_currencyamount LIKE TABLE OF bapiaccr09 WITH HEADER LINE,
      it_accounttax     LIKE TABLE OF bapiactx09 WITH HEADER LINE,
      it_return         LIKE TABLE OF bapiret2   WITH HEADER LINE,
      it_criteria       LIKE TABLE OF bapiackec9 WITH HEADER LINE,
      it_valuefield     LIKE TABLE OF bapiackev9 WITH HEADER LINE,
      gv_ktopl          like t001-ktopl ,
*      it_extension      like TABLE OF BAPIPAREX  WITH HEADER LINE,
      p_mode TYPE MODE VALUE 'N' .

DATA: l_type LIKE gd_documentheader-obj_type,
      l_key  LIKE gd_documentheader-obj_key,
      l_sys  LIKE gd_documentheader-obj_sys.
DATA : gv_auto_commit ,
      gv_onkayit .

DATA: lv_itemno LIKE bapiacgl09-itemno_acc,
      lv_land1 LIKE t005-land1,
      lv_waer1 LIKE bkpf-waers,
      lv_waer3 LIKE bkpf-waers,
      lv_waer4 LIKE bkpf-waers.

data : gv_vergi_hesap_tipi type char1 .

DATA : it_bkpf LIKE bkpf OCCURS 0 WITH HEADER LINE ,
      it_bseg LIKE bseg OCCURS 0 WITH HEADER LINE .
