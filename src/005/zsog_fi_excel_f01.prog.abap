*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_EXCEL_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT_PROGRAM
*&---------------------------------------------------------------------*
FORM init_program .


ENDFORM.                    " INIT_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  FILL_INTERNAL_TABLE
*&---------------------------------------------------------------------*
FORM fill_internal_table .

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = 'X'
      i_tab_raw_data       = it_raw
      i_filename           = p_file
    TABLES
      i_tab_converted_data = it_datatab[]
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  CLEAR : gt_belge, gt_belge[] ,
          gt_data, gt_data[] .

  LOOP AT it_datatab INTO wa_datatab .
    IF wa_datatab-wrbtr IS NOT INITIAL .

      REPLACE '.' WITH ' ' INTO wa_datatab-wrbtr .
      CONDENSE wa_datatab-wrbtr NO-GAPS .

      REPLACE ',' WITH '.' INTO wa_datatab-wrbtr .

      REPLACE '.' WITH ' ' INTO wa_datatab-kursf .
      CONDENSE wa_datatab-kursf NO-GAPS .

      REPLACE ',' WITH '.' INTO wa_datatab-kursf .

      REPLACE '.' WITH ' ' INTO wa_datatab-dmbtr .
      CONDENSE wa_datatab-dmbtr NO-GAPS .

      REPLACE ',' WITH '.' INTO wa_datatab-dmbtr .

      MOVE-CORRESPONDING wa_datatab TO gt_data .
      CONCATENATE wa_datatab-bldat+6(4) wa_datatab-bldat+3(2)
                  wa_datatab-bldat+0(2) INTO gt_data-bldat .
      CONCATENATE wa_datatab-budat+6(4) wa_datatab-budat+3(2)
                  wa_datatab-budat+0(2) INTO gt_data-budat .
      CONCATENATE wa_datatab-valut+6(4) wa_datatab-valut+3(2)
            wa_datatab-valut+0(2) INTO gt_data-valut .
      CONCATENATE wa_datatab-zfbdt+6(4)
      wa_datatab-zfbdt+3(2)
      wa_datatab-zfbdt+0(2) INTO gt_data-zfbdt .
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gt_data-kostl
        IMPORTING
          output = gt_data-kostl.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gt_data-aufnr
        IMPORTING
          output = gt_data-aufnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gt_data-prctr
        IMPORTING
          output = gt_data-prctr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gt_data-zterm
        IMPORTING
          output = gt_data-zterm.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*           EXPORTING
*             INPUT         = gt_data-projk
*          IMPORTING
*            OUTPUT        = gt_data-projk
*                   .
*
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          input     = wa_datatab-projk
        IMPORTING
          output    = gt_data-projk
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gt_data-projk
        IMPORTING
          output = gt_data-projk.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = gt_data-ldgrp
        IMPORTING
          output = gt_data-ldgrp.
*                   .
      APPEND gt_data.

    ENDIF.
  ENDLOOP .



ENDFORM.                    " FILL_INTERNAL_TABLE
*&---------------------------------------------------------------------*
*&      Form  WRITE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_alv .
  IF grid IS  INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.
    CREATE OBJECT grid
      EXPORTING
        i_parent = g_custom_container.
  ENDIF.
  PERFORM merge_fieldcat USING 'GT_DATA'.
  PERFORM change_fieldcat.
  PERFORM fill_alv_layout.
  gs_layout-ctab_fname  = 'LINE_COLOR'.
  PERFORM exclude_functions CHANGING gt_exclude.
*  PERFORM adjust_editables.
  PERFORM set_event_receiver USING grid.
  PERFORM set_table_for_first_display TABLES gt_data
                                      USING  grid ''.
ENDFORM.                    " WRITE_ALV
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_screen .
  CALL SCREEN 100.
ENDFORM.                    " DISPLAY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM handle_data_changed  USING    p_er_data_changed.

ENDFORM.                    " HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*----------------------------------------------------------------------*
FORM handle_hotspot_click  USING    p_row_id
                                    p_column_id.
  DATA: ls_data LIKE LINE OF gt_data.
  READ TABLE gt_data INTO ls_data INDEX p_row_id.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  CASE p_column_id.
    WHEN 'BELNR'.
      SET PARAMETER ID 'BLN' FIELD ls_data-belnr.
      SET PARAMETER ID 'BUK' FIELD ls_data-bukrs.
      SET PARAMETER ID 'GJR' FIELD ls_data-gjahr.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_FIELDNAME  text
*      -->P_E_FIELDVALUE  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_ET_BAD_CELLS  text
*      -->P_E_DISPLAY  text
*----------------------------------------------------------------------*
FORM on_f4  USING    p_e_fieldname
                     p_e_fieldvalue
                     p_es_row_no
                     p_er_event_data
                     p_et_bad_cells
                     p_e_display.

ENDFORM.                                                    " ON_F4
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
FORM change_fieldcat .
  DATA: ls_fcat TYPE lvc_s_fcat.
  DATA: lt_fcat TYPE lvc_t_fcat.

  lt_fcat[] = gt_fieldcat[].
  CLEAR: gt_fieldcat[],gt_fieldcat.

  LOOP AT lt_fcat INTO ls_fcat.
    MOVE-CORRESPONDING ls_fcat TO gs_fieldcat.

    CASE ls_fcat-fieldname.
      WHEN 'COLOR_LINE'.
        create_fcat 'Durum'
                    ls_fcat-fieldname '6' 'X' '' '' 'X' '1' '' .
      WHEN 'BELGE_NO'.
        create_fcat 'No'
                     ls_fcat-fieldname '15' '' '' '' '' '2' '' .
      WHEN 'BLDAT'.
        create_fcat 'belge tarihi'
                     ls_fcat-fieldname '15' '' '' '' '' '3' '' .
      WHEN 'BUDAT'.
        create_fcat 'kayıt tarihi'
                     ls_fcat-fieldname '15' '' '' '' '' '4' ' '.
      WHEN 'BLART'.
        create_fcat 'bege türü'
                     ls_fcat-fieldname '15' '' '' '' '' '5' ' '.
      WHEN 'BUKRS'.
        create_fcat 'şirket kodu'
                     ls_fcat-fieldname '15' '' '' '' '' '6' ' '.
      WHEN 'WAERS'.
        create_fcat 'para birimi'
                     ls_fcat-fieldname '15' '' '' '' '' '7' '' .
      WHEN 'KURSF'.
        create_fcat 'Kur'
                     ls_fcat-fieldname '15' '' '' '' '' '8' '' .

      WHEN 'XBLNR'.
        create_fcat 'referans'
                     ls_fcat-fieldname '15' '' '' '' '' '9' '' .
      WHEN 'WAERS'.
        create_fcat 'P.B'
                     ls_fcat-fieldname '15' '' '' '' '' '10' ' '.
      WHEN 'BKTXT'.
        create_fcat 'belge başlık metni'
                     ls_fcat-fieldname '15' '' '' '' '' '11' '' .
      WHEN 'SHKZG'.
        create_fcat 'kayıt anahtarı'
                     ls_fcat-fieldname '15' '' '' '' '' '12' '' .
      WHEN 'KOART'.
        create_fcat 'Hesap türü'
                     ls_fcat-fieldname '15' '' '' '' '' '13' '' .
      WHEN 'HKONT'.
        create_fcat 'hesap'
                     ls_fcat-fieldname '15' '' '' '' '' '14' ' '.
      WHEN 'UMSKZ'.
        create_fcat 'ÖDK'
                     ls_fcat-fieldname '15' '' '' '' '' '15' ' '.
      WHEN 'WRBTR'.
        create_fcat 'tutar'
                     ls_fcat-fieldname '15' '' '' '' '' '16' ' '.
      WHEN 'DMBTR'.
        create_fcat 'TL tutar'
                     ls_fcat-fieldname '15' '' '' '' '' '17' ' '.
      WHEN 'MWSKZ'.
        create_fcat 'vergi göstergesi'
                     ls_fcat-fieldname '15' '' '' '' '' '18' '' .
      WHEN 'ZFBDT'.
        create_fcat 'temel tarih'
                     ls_fcat-fieldname '15' '' '' '' '' '19' '' .
      WHEN 'ZTERM'.
        create_fcat 'ödeme koşulu'
                     ls_fcat-fieldname '15' '' '' '' '' '20' ' '.
      WHEN 'VALUT'.
        create_fcat 'valör tarihi'
                     ls_fcat-fieldname '15' '' '' '' '' '21' ' '.
      WHEN 'KOSTL'.
        create_fcat 'masraf yeri'
                     ls_fcat-fieldname '15' '' '' '' '' '22' '' .
      WHEN 'PRCTR'.
        create_fcat 'kar merkezi'
                     ls_fcat-fieldname '15' '' '' '' '' '23' '' .
      WHEN 'AUFNR'.
        create_fcat 'iç sipariş'
                     ls_fcat-fieldname '15' '' '' '' '' '24' ' '.
      WHEN 'PROJK'.
        create_fcat 'PYP öğesi'
                     ls_fcat-fieldname '15' '' '' '' '' '25' ' '.
      WHEN 'GSBER'.
        create_fcat 'iş alanı'
                     ls_fcat-fieldname '15' '' '' '' '' '26' ' '.
      WHEN 'ZUONR'.
        create_fcat 'tayin'
                     ls_fcat-fieldname '15' '' '' '' '' '27' '' .
      WHEN 'SGTXT'.
        create_fcat 'kalem metin'
                     ls_fcat-fieldname '15' '' '' '' '' '28' '' .
      WHEN 'ABPER'.
        create_fcat 'hesaplaşma dönemi'
                     ls_fcat-fieldname '15' '' '' '' '' '29' ' '.
      WHEN 'XREF1'.
        create_fcat 'referans 1'
                     ls_fcat-fieldname '15' '' '' '' '' '30' '' .
      WHEN 'XREF2'.
        create_fcat 'referans 2'
                     ls_fcat-fieldname '15' '' '' '' '' '31' '' .
      WHEN 'XREF3'.
        create_fcat 'referans 3'
                     ls_fcat-fieldname '15' '' '' '' '' '32' ' '.
      WHEN 'LDGRP'.
        create_fcat 'Defter Grubu'
                     ls_fcat-fieldname '15' '' '' '' '' '33' ' '.
      WHEN 'BELNR'.
        create_fcat 'Oluşan Belge No'
                     ls_fcat-fieldname '15' '' '' 'X' '' '34' ' '.
      WHEN 'GJAHR'.
        create_fcat 'Belge Yılı'
                     ls_fcat-fieldname '15' '' '' '' 'X' '35' ' '.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
FORM exclude_functions  CHANGING pt_exclude TYPE ui_functions.
  DATA ls_exclude TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO pt_exclude.
ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  ADJUST_EDITABLES
*&---------------------------------------------------------------------*
FORM adjust_editables .


  DATA: ls_data    LIKE LINE OF gt_data.
  DATA: lv_type(1)  TYPE c.
  DATA: t_edit      TYPE lvc_t_styl,
        fs_editcell LIKE LINE OF t_edit,
        wa_cellcolor TYPE lvc_s_scol,
        lv_color     TYPE lvc_col.

*  wa_cellcolor-fname =  'FNAME' .


  LOOP AT gt_data INTO ls_data.

*    AT NEW fname.
*      CLEAR gv_color.
*      lv_color = lv_color + 1.
*      lv_color = lv_color MOD 2.
*
*    ENDAT.
*
*    wa_cellcolor-color-col = lv_color.
*    wa_cellcolor-color-int = '1'.
*    wa_cellcolor-color-inv = '0'.
*    APPEND wa_cellcolor TO ls_kalem-line_color.


    CLEAR: ls_data-edit_line, t_edit, t_edit[].
    IF  ls_data-color_line = icon_led_red.
      lv_type = 'R'.
    ELSEIF ls_data-color_line = icon_led_yellow.
      lv_type = 'Y'.
    ELSE.
      lv_type = 'G'.
    ENDIF.

    PERFORM make_editable_cell USING    lv_type 'AUFNR'
                               CHANGING t_edit .

    PERFORM make_editable_cell USING    lv_type 'BLART'
                               CHANGING t_edit .

    PERFORM make_editable_cell USING    lv_type 'BSCHL'
                               CHANGING t_edit .

    PERFORM make_editable_cell USING    lv_type 'GRUPLAMA'
                               CHANGING t_edit .

    PERFORM make_editable_cell USING    lv_type 'KOSTL'
                               CHANGING t_edit .

    PERFORM make_editable_cell USING    lv_type 'K_HKONT'
                               CHANGING t_edit .

    PERFORM make_editable_cell USING    lv_type 'NAKIT_TURU'
                               CHANGING t_edit .

    PERFORM make_editable_cell USING    lv_type 'SGTXT'
                               CHANGING t_edit .

    PERFORM make_editable_cell USING    lv_type 'KSGTXT'
                               CHANGING t_edit .

    PERFORM make_editable_cell USING    lv_type 'UMSKZ'
                               CHANGING t_edit .

    PERFORM make_editable_cell USING    lv_type 'ZUONR'
                               CHANGING t_edit .

    PERFORM make_editable_cell USING    lv_type 'VALUT'
                               CHANGING t_edit .

    PERFORM make_editable_cell USING    lv_type 'MWSKZ'
                               CHANGING t_edit .

    PERFORM make_editable_cell USING    lv_type 'KKURS'
                               CHANGING t_edit .

    INSERT LINES OF t_edit INTO TABLE ls_data-edit_line.
    MODIFY gt_data FROM ls_data.
  ENDLOOP.
ENDFORM.                    " ADJUST_EDITABLES
*&---------------------------------------------------------------------*
*&      Form  MAKE_EDITABLE_CELL
*&---------------------------------------------------------------------*
FORM make_editable_cell  USING    pv_type       TYPE c
                                  pv_field      TYPE  lvc_fname
                         CHANGING pt_editcell   TYPE  lvc_t_styl .

  DATA : wa_cellstyle TYPE lvc_s_styl.
  wa_cellstyle-fieldname =  pv_field .

  CASE pv_type.
    WHEN 'R'.
      wa_cellstyle-style     = '00000027'.
    WHEN 'Y'.
      wa_cellstyle-style     = '00000029'.
    WHEN OTHERS.
      wa_cellstyle-style     = '00000028'.
  ENDCASE.

  INSERT wa_cellstyle INTO TABLE pt_editcell.
ENDFORM.                    " MAKE_EDITABLE_CELL
*&---------------------------------------------------------------------*
*&      Form  SAVE_DOCUMENT
*&---------------------------------------------------------------------*
FORM save_document .
  DATA: ls_selrow TYPE lvc_s_row.
  DATA: ls_msg    TYPE ty_msg.


  DATA: lv_dmbtr TYPE bseg-dmbtr,
        lv_wrbtr TYPE bseg-dmbtr,
        lv_accgl  TYPE bseg-dmbtr,
        lv_accgl2 TYPE bseg-dmbtr,
        lv_rectr TYPE bseg-dmbtr,
        ls_tbsl  TYPE tbsl      .


  PERFORM check_selected_data.
  CHECK gt_selrows IS NOT INITIAL.
  CLEAR gt_belge[].
  LOOP AT gt_selrows INTO ls_selrow.
    CLEAR gs_data.
    READ TABLE gt_data INTO gs_data INDEX ls_selrow-index.
    CHECK  gs_data-belnr IS INITIAL.
    MOVE-CORRESPONDING gs_data TO gt_belge .
    COLLECT gt_belge .
  ENDLOOP.

  LOOP AT gt_belge.
    CLEAR gv_buzei.
    LOOP AT gt_data INTO gs_data
      WHERE belge_no = gt_belge-belge_no
        AND belnr IS INITIAL.

      CLEAR : gs_bkpf , gs_bseg , gv_buzei .

      gs_bkpf-bukrs = gs_data-bukrs .
      gs_bkpf-bldat = gs_data-bldat .
      gs_bkpf-budat = gs_data-budat.
      gs_bkpf-blart = gs_data-blart .
      gs_bkpf-bktxt = gs_data-bktxt.
      gs_bkpf-waers = gs_data-waers.
      gs_bkpf-xblnr = gs_data-xblnr.
      gs_bkpf-kursf = gs_data-kursf.
      gs_bkpf-ldgrp = gs_data-ldgrp.

      PERFORM document_header USING gs_bkpf .
      EXIT.
    ENDLOOP.
    LOOP AT gt_data INTO gs_data
    WHERE belge_no = gt_belge-belge_no.
*- Kalem Bilgileri
      IF gs_data-koart = 'S'.
*- Anahesap Kalem Bilgileri
        gv_buzei         = gv_buzei + 1 .
        gs_bseg-buzei    = gv_buzei     .
        MOVE-CORRESPONDING gs_data TO gs_bseg.
        gs_bseg-bukrs    = gs_bkpf-bukrs.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_bseg-hkont
          IMPORTING
            output = gs_bseg-hkont.
        IF gs_bseg-shkzg EQ 'H'.
          gs_bseg-wrbtr  = gs_bseg-wrbtr * -1.
        ELSE.
          gs_bseg-wrbtr  = gs_bseg-wrbtr.
        ENDIF.
        IF gs_bseg-shkzg EQ 'H'.
          gs_bseg-dmbtr  = gs_bseg-dmbtr * -1.
        ELSE.
          gs_bseg-dmbtr  = gs_bseg-dmbtr.
        ENDIF.
        PERFORM add_accountgl      USING gs_bseg gs_bkpf .
        PERFORM add_currencyamount USING gs_bseg gs_bkpf .


      ELSEIF gs_data-koart = 'D'.
        gv_buzei         = gv_buzei + 1  .
        gs_bseg-buzei    = gv_buzei      .
        MOVE-CORRESPONDING gs_data TO gs_bseg.
        CLEAR gs_bseg-hkont.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_data-hkont
          IMPORTING
            output = gs_data-hkont.
        gs_bseg-kunnr    = gs_data-hkont.
        IF gs_bseg-shkzg EQ 'H'.
          gs_bseg-wrbtr  = gs_bseg-wrbtr * -1.
        ELSE.
          gs_bseg-wrbtr  = gs_bseg-wrbtr.
        ENDIF.
        IF gs_bseg-shkzg EQ 'H'.
          gs_bseg-dmbtr  = gs_bseg-dmbtr * -1.
        ELSE.
          gs_bseg-dmbtr  = gs_bseg-dmbtr.
        ENDIF.
        PERFORM add_accountreceivable USING gs_bseg gs_bkpf .
        PERFORM add_currencyamount    USING gs_bseg gs_bkpf .


      ELSEIF gs_data-koart = 'K'.
        gv_buzei         = gv_buzei + 1  .
        gs_bseg-buzei    = gv_buzei      .
        MOVE-CORRESPONDING gs_data TO gs_bseg.
        CLEAR gs_bseg-hkont.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_data-hkont
          IMPORTING
            output = gs_data-hkont.
        gs_bseg-lifnr    = gs_data-hkont.
        IF gs_bseg-shkzg EQ 'H'.
          gs_bseg-wrbtr  = gs_bseg-wrbtr * -1.
        ELSE.
          gs_bseg-wrbtr  = gs_bseg-wrbtr.
        ENDIF.
        IF gs_bseg-shkzg EQ 'H'.
          gs_bseg-dmbtr  = gs_bseg-dmbtr * -1.
        ELSE.
          gs_bseg-dmbtr  = gs_bseg-dmbtr.
        ENDIF.
        PERFORM add_accountpayable USING gs_bseg gs_bkpf .
        PERFORM add_currencyamount USING gs_bseg gs_bkpf .

      ENDIF.
      IF gs_bseg-mwskz IS NOT INITIAL.
        CLEAR lv_accgl.
        CLEAR lv_accgl2.

        lv_accgl  = gs_bseg-wrbtr.
        lv_accgl2 = gs_bseg-dmbtr.

        PERFORM add_accounttax     USING gs_bseg gs_bkpf.

        CLEAR lv_dmbtr.
        CLEAR lv_wrbtr.
        READ TABLE bt_amount WITH KEY itemno_acc = gs_bseg-buzei
                                      curr_type  = '00'.
        lv_wrbtr = bt_amount-amt_doccur.
        READ TABLE bt_amount WITH KEY itemno_acc = gs_bseg-buzei
                                      curr_type  = '10'.
        lv_dmbtr = bt_amount-amt_doccur.
        gv_buzei = gs_bseg-buzei - 1.
        LOOP AT bt_amount WHERE itemno_acc = gv_buzei.
          IF bt_amount-curr_type  = '00'.
            bt_amount-amt_doccur = lv_accgl + ( lv_wrbtr * -1 ).
          ELSE.
            bt_amount-amt_doccur = lv_accgl2 + ( lv_dmbtr * -1 ).
          ENDIF.

          MODIFY bt_amount.
        ENDLOOP.
        gv_buzei = gv_buzei + 1.
      ENDIF.

    ENDLOOP.
*- Kayıt
    PERFORM bapi_acc_document_post .
    CLEAR: ls_msg.

    LOOP AT bt_return WHERE type EQ 'E'
                         OR type EQ 'A'
                         .
*      MOVE-CORRESPONDING bt_return TO gt_msg.
*      APPEND gt_msg.
*      CLEAR:gt_msg.

    ENDLOOP.
    IF sy-subrc NE 0.
      LOOP AT bt_return WHERE type EQ 'S'.
        LOOP AT gt_data INTO gs_data
         WHERE belge_no = gt_belge-belge_no
           AND belnr IS INITIAL.
          gs_data-belnr = bt_return-message_v2+0(10).
          gs_data-bukrs = bt_return-message_v2+10(4).
          gs_data-gjahr = bt_return-message_v2+14(4).
          MODIFY gt_data FROM gs_data.
        ENDLOOP.
        EXIT.
      ENDLOOP.
    ENDIF.
    LOOP AT bt_return .
      MOVE-CORRESPONDING bt_return TO gt_return .
      APPEND gt_return .
    ENDLOOP .

  ENDLOOP.


ENDFORM.                    " SAVE_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECTED_DATA
*&---------------------------------------------------------------------*
FORM check_selected_data .
  CLEAR: gt_selrows,gt_selrows[].
  DATA: ls_selrow TYPE lvc_s_row.

  CALL METHOD grid->check_changed_data .

  CALL METHOD grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_selrows.
  READ TABLE gt_selrows INTO ls_selrow INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e253(zfi).
  ENDIF.
ENDFORM.                    " CHECK_SELECTED_DATA
*&---------------------------------------------------------------------*
*&      Form  SHOW_LOGS
*&---------------------------------------------------------------------*
FORM show_logs .
  CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
    TABLES
      it_return = gt_return[].
ENDFORM.                    " SHOW_LOGS
*&---------------------------------------------------------------------*
*&      Form  REFRESH_GRID
*&---------------------------------------------------------------------*
FORM refresh_grid .
  CALL METHOD grid->refresh_table_display .

ENDFORM.                    " REFRESH_GRID
*&---------------------------------------------------------------------*
*&      Form  EXCEL_DOWNLOAD_SAMPLE
*&---------------------------------------------------------------------*
FORM excel_download_sample .
  REFRESH : it_tab1.

  CREATE OBJECT w_excel 'EXCEL.APPLICATION'.
  SET PROPERTY OF w_excel  'VISIBLE' = 1.

  CALL METHOD OF
      w_excel
      'WORKBOOKS' = w_workbook.

  CALL METHOD OF
      w_workbook
      'ADD'.

  SET PROPERTY OF w_excel 'SheetsInNewWorkbook' = 3.

  ASSIGN w_deli TO <fs> TYPE 'X'.
  w_hex = wl_c09.
  <fs> = w_hex.

  CONCATENATE 'Başlık Kalem'
              'Belge Tarihi'
              'Kayıt Tarihi'
              'Belge Türü'
              'Şirket Kodu'
              'Para Birimi'
              'Kur'
              'Referans'
              'Belge Başlık Metni'
              'Borç(S) Alacak(H)'
              'Müşteri(D) Satıcı(K) AnaHesap(S)'
              'Hesap'
              'Ödk'
              'Belge Tutar'
              'TL Tutar'
              'Vergi Göstergesi'
              'Temel Tarih'
              'Ödeme Koşulu'
              'Valör Tarihi'
              'Masraf Yeri'
              'Kar Merkezi'
              'İç Sipariş'
              'PYP Ögesi'
              'İş Alanı'
              'Tayin'
              'Metin'
              'Referans1'
              'Referans2'
              'Referans3'
              '0L/02'
              INTO wa_tab SEPARATED BY w_deli.
  APPEND wa_tab TO it_tab1.

  CLEAR wa_tab.

  CONCATENATE '1'
              '29.07.2019'
              '29.07.2019'
              'KR'
              '2425'
              'TRY'
              '1'
              'DENEME12345'
              '4'
              'H'
              'K'
              '7069102'
              ''
              '884,60'
              '884,60'
              ''
              ''
              'Z000'
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              'Deneme1'
              ''
              ''
              ''
              ''
              INTO wa_tab SEPARATED BY w_deli.
  APPEND wa_tab TO it_tab1.
    CLEAR wa_tab.

    CONCATENATE '1'
              '29.07.2019'
              '29.07.2019'
              'KR'
              '2425'
              'TRY'
              '1'
              'DENEME12345'
              '4'
              'S'
              'S'
              '7700400003'
              ''
              '884,60'
              '884,60'
              'V0'
              ''
              'Z000'
              ''
              '2425000001'
              ''
              ''
              ''
              ''
              ''
              'Deneme1'
              ''
              ''
              ''
              ''
              INTO wa_tab SEPARATED BY w_deli.
  APPEND wa_tab TO it_tab1.
    CLEAR wa_tab.

    CONCATENATE '2'
              '29.07.2019'
              '29.07.2019'
              'DR'
              '2425'
              'USD'
              '1'
              'DENEME12345'
              '4'
              'S'
              'D'
              '9142180'
              ''
              '1.000,00'
              '1.000,00'
              ''
              ''
              'Z000'
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              INTO wa_tab SEPARATED BY w_deli.
  APPEND wa_tab TO it_tab1.
    CLEAR wa_tab.

     CONCATENATE '2'
              '29.07.2019'
              '29.07.2019'
              'DR'
              '2425'
              'USD'
              '1'
              'DENEME12345'
              '4'
              'H'
              'S'
              '6000000000'
              ''
              '1.000,00'
              '1.000,00'
              'V4'
              ''
              'Z000'
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              INTO wa_tab SEPARATED BY w_deli.
  APPEND wa_tab TO it_tab1.
    CLEAR wa_tab.

* Downloading header details to first sheet
  PERFORM download_sheet TABLES it_tab1 USING 1 'Header Details'.

  GET PROPERTY OF w_excel 'ActiveSheet' = w_worksheet.

ENDFORM.                    " EXCEL_DOWNLOAD_SAMPLE
*&---------------------------------------------------------------------*
*&      Form  EXCEL_CONTROL
*&---------------------------------------------------------------------*
FORM excel_control .
  DATA lv_extention(10) TYPE c.
  CASE sy-ucomm.
    WHEN 'ONLI'.
      IF p_file IS INITIAL.
        MESSAGE e121(zmm) WITH 'Lütfen dosya yolu giriniz.' DISPLAY LIKE 'E'.
      ENDIF.
      CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
        EXPORTING
          filename  = p_file
        IMPORTING
          extension = lv_extention.
      IF lv_extention = 'XLS' OR lv_extention = 'XLSX'.
      ELSE.
        MESSAGE e122(zmm) WITH 'XLS uzantılı bir dosya yükleyiniz' DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.
ENDFORM.                    " EXCEL_CONTROL
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_SHEET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB1  text
*      -->P_1      text
*      -->P_1737   text
*----------------------------------------------------------------------*
FORM download_sheet  TABLES p_tab
                     USING p_sheet TYPE i
                           p_name TYPE string.

  CALL METHOD OF
      w_excel
      'WORKSHEETS' = w_worksheet
    EXPORTING
      #1           = p_sheet.

  CALL METHOD OF
      w_worksheet
      'ACTIVATE'.
  SET PROPERTY OF w_worksheet 'NAME' = p_name.

  CALL METHOD OF
      w_excel
      'Range' = w_range
    EXPORTING
      #1      = 'A1'
      #2      = 'AF1'.

  CALL METHOD OF
      w_range
      'INTERIOR' = w_int.
  SET PROPERTY OF w_int 'ColorIndex' = 6.
  SET PROPERTY OF w_int 'Pattern' = 1.

* Initially unlock all the columns( by default all the columns are
*  LOCKED )
  CALL METHOD OF
      w_excel
      'Columns' = w_columns.
  SET PROPERTY OF w_columns 'Locked' = 0.

* Locking and formatting first column
  CALL METHOD OF
      w_excel
      'Columns' = w_columns
    EXPORTING
      #1        = 1.

  SET PROPERTY OF w_columns  'Locked' = 1.
  SET PROPERTY OF w_columns  'NumberFormat' = '@'.

* Export the contents in the internal table to the clipboard
  CALL METHOD cl_gui_frontend_services=>clipboard_export
    IMPORTING
      data                 = p_tab[]
    CHANGING
      rc                   = w_rc
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

* Paste the contents in the clipboard to the worksheet
  CALL METHOD OF
      w_worksheet
      'Paste'.

* Autofit the columns according to the contents
  CALL METHOD OF
      w_excel
      'Columns' = w_columns.
  CALL METHOD OF
      w_columns
      'AutoFit'.

  FREE OBJECT: w_columns, w_range.
ENDFORM.                    " DOWNLOAD_SHEET
