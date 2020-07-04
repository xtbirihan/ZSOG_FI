*&---------------------------------------------------------------------*
*  ABAP Name     : ZSOG_FI_015_MUS_SAT_YASLAN
*  Job-Name      : Müşteri-Satıcı Yaşlandırma Programı
*  Autor         : Burcu Hilal Altunbaş
*  GMP relevant  : Mert Kaya
*  Date          : 02.10.2019
*  Description   : Açıklama satırı.
*&---------------------------------------------------------------------*
REPORT zsog_fi_015_mus_sat_yaslan.

INCLUDE zsog_fi_015_top.
INCLUDE zsog_fi_015_f01.

INITIALIZATION.
*  PERFORM yetki_kontrol .

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

START-OF-SELECTION.
  IF gv_hata EQ 'X'.
    MESSAGE 'Hiçbir şirket koduna yetkiniz bulunmamaktadır. Lütfen ' &
       'yöneticinizle iletişime geçiniz.' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
  CHECK gv_hata NE 'X'.
  FREE:gt_kna1[],gt_kna2[],tb_borc[],tb_alac[],gt_lfa1[],
       gt_lfa2[],tb_list[],tb_bsid2[],tb_bsad2[],
       tb_bsid[],tb_bsad[],tb_borc2[],tb_alac2[],tb_list[],tb_knb1[],
       tb_knb2[].
*  CLEAR:p_check.

  IF rb_mus = 'X'.
    PERFORM select_customers.
***    if p_is eq 'X'.
    PERFORM select_customers_open_items.
***    else.
***    endif.
    PERFORM match_alac_borc TABLES tb_borc tb_alac.

    IF p_check EQ 'X'.
      CLEAR gt_kna2[].
      SELECT lifnr kunnr FROM  kna1
        INTO CORRESPONDING FIELDS OF TABLE gt_kna2
             WHERE  kunnr  IN s_kunnr.
      DELETE gt_kna2 WHERE lifnr EQ space.

      CLEAR s_kunnr[].
      LOOP AT gt_kna2.
        CLEAR s_kunnr.
        s_lifnr-sign   = 'I'.
        s_lifnr-option = 'EQ'.
        s_lifnr-low    = gt_kna2-lifnr.
        COLLECT s_lifnr.
      ENDLOOP.

      IF s_lifnr[] IS NOT INITIAL.
        FREE:tb_knb2[].
        PERFORM select_vendors_2.
        PERFORM select_vendors_open_items_2.
        PERFORM match_alac_borc TABLES tb_borc2 tb_alac2.
      ENDIF.

    ENDIF.
  ELSE."Satıcılar için
    PERFORM select_vendors.
    PERFORM select_vendors_open_items.
    PERFORM match_alac_borc TABLES tb_borc tb_alac.

    IF p_check EQ 'X'.

      CLEAR gt_lfa2[].
      SELECT lifnr kunnr FROM  lfa1
        INTO CORRESPONDING FIELDS OF TABLE gt_lfa2
             WHERE  lifnr  IN s_lifnr.
      DELETE gt_lfa2 WHERE kunnr EQ space.
**
      CLEAR s_lifnr[].
      LOOP AT gt_lfa2.
        CLEAR s_lifnr.
        s_kunnr-sign   = 'I'.
        s_kunnr-option = 'EQ'.
        s_kunnr-low    = gt_lfa2-kunnr.
        COLLECT s_kunnr.
      ENDLOOP.
**
      IF s_kunnr[] IS NOT INITIAL.
        FREE:tb_knb2[].
        PERFORM select_customers_2.
        PERFORM select_customers_open_items_2.
        PERFORM match_alac_borc TABLES tb_borc2 tb_alac2.
      ENDIF.

    ENDIF.
  ENDIF.

  PERFORM fill_ranges.
  IF p_is EQ 'X'.
    IF rb_bpb EQ 'X'.
      PERFORM prepare_list_no_is_pb.
    ELSE.
      PERFORM prepare_list.
    ENDIF.

  ELSE.
    IF rb_bpb EQ 'X'.
      PERFORM  prepare_list_bp.

    ELSE.
      PERFORM prepare_list_no_is.
    ENDIF.
  ENDIF.
  IF p_check EQ 'X'.
    PERFORM prepare_list2.
  ENDIF.

*  IF rb_detay EQ 'X'.
*    PERFORM detay.
*  ENDIF.

*  IF rb_detay NE 'X'.
  PERFORM e03_eventtab_build USING gt_events[].
  PERFORM display_list.
*  ENDIF.


  DATA:destination LIKE pri_params-pdest.

*at selection-screen.
*  destination = 'T1_M'.
*  call function 'SET_PRINT_PARAMETERS'
*    exporting
*      destination = destination
*      layout      = 'X_65_255'.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  E03_EVENTTAB_BUILD
*&---------------------------------------------------------------------*
FORM e03_eventtab_build USING e03_lt_events TYPE slis_t_event.
  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = e03_lt_events.
  READ TABLE e03_lt_events WITH KEY name =  slis_ev_user_command
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE 'USER_COMMAND' TO ls_event-form.
    APPEND ls_event TO e03_lt_events.
  ENDIF.
  READ TABLE e03_lt_events WITH KEY name =  slis_ev_pf_status_set
                         INTO ls_event.
  IF sy-subrc = 0.
    MOVE 'F01_SET_STATUS' TO ls_event-form.
    APPEND ls_event TO e03_lt_events.
  ENDIF.
ENDFORM.                    "E03_EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                    rs_selfield TYPE slis_selfield.
  DATA:BEGIN OF it_detay OCCURS 0,
    kunnr LIKE bsid-kunnr,
    belnr LIKE bsid-belnr,
    budat LIKE bsid-budat,
    zfbdt LIKE bsid-zfbdt,
    dmbtr LIKE bsid-dmbtr,
    END OF it_detay.
  DATA:ts_list LIKE tb_list.
  RANGES:gv_date FOR sy-datum.

  IF r_ucomm EQ '&IC1'. " AND rb_detay NE 'X'.
*    break luzuner.
    CLEAR ts_list.
    READ TABLE tb_list INTO ts_list INDEX rs_selfield-tabindex.
    FREE gv_date.
    IF rs_selfield-fieldname EQ 'GVB99'.
      gv_date[] = r_gvb99[].
    ELSEIF rs_selfield-fieldname EQ 'GVB01'.
      gv_date[] = r_gvb01[].
    ELSEIF rs_selfield-fieldname EQ 'GVB02'.
      gv_date[] = r_gvb02[].
    ELSEIF rs_selfield-fieldname EQ 'GVB03'.
      gv_date[] = r_gvb03[].
    ELSEIF rs_selfield-fieldname EQ 'GVB04'.
      gv_date[] = r_gvb04[].
    ELSEIF rs_selfield-fieldname EQ 'GVB05'.
      gv_date[] = r_gvb05[].
    ELSEIF rs_selfield-fieldname EQ 'GVB06'.
      gv_date[] = r_gvb06[].

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:40:29
    ELSEIF rs_selfield-fieldname EQ 'GVB07'.
      gv_date[] = r_gvb07[].
    ELSEIF rs_selfield-fieldname EQ 'GVB08'.
      gv_date[] = r_gvb08[].
*      }    <<<- End of  Added - 07.10.2019 10:40:29

    ELSEIF rs_selfield-fieldname EQ 'IVB01'.
      gv_date[] = r_ivb01[].
    ELSEIF rs_selfield-fieldname EQ 'IVB02'.
      gv_date[] = r_ivb02[].
    ELSEIF rs_selfield-fieldname EQ 'IVB03'.
      gv_date[] = r_ivb03[].
    ELSEIF rs_selfield-fieldname EQ 'IVB04'.
      gv_date[] = r_ivb04[].
    ELSEIF rs_selfield-fieldname EQ 'IVB05'.
      gv_date[] = r_ivb05[].
    ELSEIF rs_selfield-fieldname EQ 'IVB06'.
      gv_date[] = r_ivb06[].

*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:40:39
    ELSEIF rs_selfield-fieldname EQ 'IVB07'.
      gv_date[] = r_ivb07[].
    ELSEIF rs_selfield-fieldname EQ 'IVB08'.
      gv_date[] = r_ivb08[].
*      }    <<<- End of  Added - 07.10.2019 10:40:39

    ELSEIF rs_selfield-fieldname EQ 'IVB99'.
      gv_date[] = r_ivb99[].
    ENDIF.
    READ TABLE gv_date INDEX 1.
    IF sy-subrc EQ 0.
      LOOP AT tb_borc WHERE kunnr EQ ts_list-kunnr
                        AND zfbdt IN gv_date.
        it_detay-kunnr = tb_borc-kunnr.
        it_detay-belnr = tb_borc-belnr.
        it_detay-budat = tb_borc-budat.
        it_detay-zfbdt = tb_borc-zfbdt.
        it_detay-dmbtr = tb_borc-kalan.
        APPEND it_detay.
      ENDLOOP.
      IF it_detay[] IS INITIAL.
        LOOP AT tb_borc2 WHERE kunnr EQ ts_list-kunnr
                           AND zfbdt IN gv_date.
          it_detay-kunnr = tb_borc2-kunnr.
          it_detay-belnr = tb_borc2-belnr.
          it_detay-budat = tb_borc2-budat.
          it_detay-zfbdt = tb_borc2-zfbdt.
          it_detay-dmbtr = tb_borc2-kalan.
          APPEND it_detay.
        ENDLOOP.
      ENDIF.
      FREE MEMORY ID 'ZFIR0004A'.
      EXPORT it_detay TO MEMORY ID 'ZFIR0004A'.
      SUBMIT zfir0004a AND RETURN.
    ENDIF.
  ENDIF.
ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  DETAY
*&---------------------------------------------------------------------*
FORM detay .
  FREE it_tdetay.
  LOOP AT tb_borc.
    CLEAR it_tdetay.
    ON CHANGE OF tb_borc-kunnr.
      CLEAR tb_list.
      READ TABLE tb_list WITH KEY kunnr = tb_borc-kunnr.
      MOVE-CORRESPONDING tb_list TO it_tdetay.
      it_tdetay-bakiy = tb_list-bakiy.
      APPEND it_tdetay.
    ENDON.
    CLEAR it_tdetay.
    it_tdetay-kunnr = tb_borc-kunnr.
    it_tdetay-belnr = tb_borc-belnr.
    it_tdetay-txt50 = tb_list-txt50.
    it_tdetay-xblnr = tb_borc-xblnr.
    it_tdetay-budat = tb_borc-budat.
    it_tdetay-zfbdt = tb_borc-zfbdt.
    it_tdetay-bakiy = tb_borc-kalan.
    APPEND it_tdetay.
  ENDLOOP.

  LOOP AT tb_borc2.
    CLEAR it_tdetay.
    ON CHANGE OF tb_borc2-kunnr.
      CLEAR tb_list.
      READ TABLE tb_list WITH KEY kunnr = tb_borc2-kunnr.
      MOVE-CORRESPONDING tb_list TO it_tdetay.
      it_tdetay-bakiy = tb_list-bakiy.
      APPEND it_tdetay.
    ENDON.
    CLEAR it_tdetay.
    it_tdetay-kunnr = tb_borc2-kunnr.
    it_tdetay-belnr = tb_borc2-belnr.
    it_tdetay-txt50 = tb_list-txt50.
    it_tdetay-xblnr = tb_borc2-xblnr.
    it_tdetay-budat = tb_borc2-budat.
    it_tdetay-zfbdt = tb_borc2-zfbdt.
    it_tdetay-bakiy = tb_borc2-kalan.
    APPEND it_tdetay.
  ENDLOOP.



  FREE MEMORY ID 'ZFIR0004B'.
*{   ->>> Added by Prodea Sefa Taşkent - 07.10.2019 10:16:28
  EXPORT it_tdetay p_gcm1 p_gcm2 p_gcm3 p_gcm4 p_gcm5 p_gcm6 p_gcm7
  p_gcm8
p_ilr1
         p_ilr2 p_ilr3 p_ilr4 p_ilr5 p_ilr6 p_ilr7 p_ilr8 rb_mus rb_sat
         TO MEMORY ID 'ZFIR0004B'.
*}    <<<- End of  Added - 07.10.2019 10:16:28
  SUBMIT zyfir0007b AND RETURN.
ENDFORM.                    " DETAY
