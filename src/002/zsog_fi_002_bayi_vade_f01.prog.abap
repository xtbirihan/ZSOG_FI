*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_002_BAYI_VADE_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data .
  CLEAR: gt_blart.

  SELECT blart FROM zsog_fi_002_c001 INTO TABLE gt_blart
  WHERE blart IN s_blart.
  IF sy-subrc NE 0 AND s_blart[] IS INITIAL.
    gv_error = abap_true.
    MESSAGE e014(zsg) INTO gv_msg.
    PERFORM fill_msg.

  ELSEIF sy-subrc NE 0 AND s_blart[] IS NOT INITIAL.
    gv_error = abap_true.
    MESSAGE e015(zsg) INTO gv_msg.
    PERFORM fill_msg.

  ENDIF.

  SELECT * FROM zsog_fi_002_c002 INTO TABLE gt_rate.
  IF sy-subrc NE 0.
    gv_error = abap_true.
    MESSAGE e016(zsg) INTO gv_msg.
    PERFORM fill_msg.

  ELSEIF sy-subrc = 0 AND lines( gt_rate ) NE 1.
    gv_error = abap_true.
    MESSAGE e017(zsg) INTO gv_msg.
    PERFORM fill_msg.

  ENDIF.

  IF gv_error = abap_true.
    PERFORM display_msg.
  ENDIF.
ENDFORM.                    " CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_msg .
  MOVE-CORRESPONDING sy TO gs_msg.
  APPEND gs_msg TO gt_msg.

  CLEAR gs_msg.
ENDFORM.                    " FILL_MSG
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_msg .
  DATA: lv_text(50).

  lv_text = text-t01.

  CALL FUNCTION 'RSDC_SHOW_MESSAGES_POPUP'
    EXPORTING
      i_t_msg           = gt_msg
      i_txt             = lv_text
      i_with_s_on_empty = ''
      i_one_msg_direct  = ''
      i_one_msg_type_s  = ''.

  CLEAR gt_msg.
ENDFORM.                    " DISPLAY_MSG
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  DATA: BEGIN OF ls_bsid,
    bukrs TYPE bsid-bukrs,
    kunnr TYPE bsid-kunnr,
    umsks TYPE bsid-umsks,
    umskz TYPE bsid-umskz,
    augdt TYPE bsid-augdt,
    augbl TYPE bsid-augbl,
    zuonr TYPE bsid-zuonr,
    gjahr TYPE bsid-gjahr,
    belnr TYPE bsid-belnr,
    buzei TYPE bsid-buzei,
    budat TYPE bsid-budat,
    bldat TYPE bsid-bldat,
    cpudt TYPE bsid-cpudt,
    waers TYPE bsid-waers,
    xblnr TYPE bsid-xblnr,
    blart TYPE bsid-blart,
    monat TYPE bsid-monat,
    bschl TYPE bsid-bschl,
    shkzg TYPE bsid-shkzg,
    gsber TYPE bsid-gsber,
    mwskz TYPE bsid-mwskz,
    dmbtr TYPE bsid-dmbtr,
    wrbtr TYPE bsid-wrbtr,
    sgtxt TYPE bsid-sgtxt,
  END OF ls_bsid,
  lt_bsid LIKE TABLE OF ls_bsid.

  DATA: lr_blart TYPE RANGE OF blart WITH HEADER LINE.

  LOOP AT gt_blart INTO gs_blart.
    lr_blart     = 'IEQ'.
    lr_blart-low = gs_blart-blart.
    APPEND lr_blart.
    CLEAR gs_blart.
  ENDLOOP.

  SELECT bukrs
         kunnr
         umsks
         umskz
         augdt
         augbl
         zuonr
         gjahr
         belnr
         buzei
         budat
         bldat
         cpudt
         waers
         xblnr
         blart
         monat
         bschl
         shkzg
         gsber
         mwskz
         dmbtr
         wrbtr
         sgtxt
    FROM bsid INTO TABLE lt_bsid
   WHERE bukrs EQ p_bukrs
     AND kunnr IN s_kunnr
     AND blart IN lr_blart.

ENDFORM.                    " GET_DATA
