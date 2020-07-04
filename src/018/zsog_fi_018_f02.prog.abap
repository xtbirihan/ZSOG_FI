*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_018_F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form STATUS_1903
*&---------------------------------------------------------------------*
FORM status_1903 .
  PERFORM set_screen.

  PERFORM set_fieldcat.
  PERFORM change_fieldcat.
  PERFORM exclude_tb_functions CHANGING gs_scr_1903-t_exclude.
  PERFORM display_alv_grid.
ENDFORM.                    "status_1903
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN
*&---------------------------------------------------------------------*
FORM set_screen .
  DATA: lt_fcode TYPE TABLE OF sy-ucomm.
  DATA: lv_text(50).
  DATA: lv_curr TYPE c LENGTH 18.
  PERFORM set_icons.
  CASE gs_scr_1903-auth-statu.
    WHEN '01'.
      PERFORM set_liste_hazirlayicisi_screen TABLES lt_fcode.
    WHEN '02'.
      PERFORM set_category_screen TABLES lt_fcode.
    WHEN '03'.
      PERFORM set_onayci_screen TABLES lt_fcode.
    WHEN OTHERS.
  ENDCASE.
  WRITE p_oneri TO lv_curr.
  CONDENSE lv_curr.
  lv_text = |/ Öneri Tutarı | && |= | && lv_curr.
*  CONDENSE
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING lt_fcode.
  SET TITLEBAR  'TITLE_100' WITH lv_text.
ENDFORM.                    "set_screen
*&---------------------------------------------------------------------*
*&      Form  set_liste_hazirlayicisi_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_FCODE   text
*----------------------------------------------------------------------*
FORM set_liste_hazirlayicisi_screen TABLES lt_fcode.
  DATA: ls_fcode TYPE sy-ucomm.
  IF p_r1 EQ 'X'.

    IF gs_scr_1903-onay_durum-onay_durum IS INITIAL OR
       gs_scr_1903-onay_durum-onay_durum EQ '01'.
      "liste hazırlayıcısı ilk defa onay veriyor ya da verdiği onayı geri almıştır.
      "bu durumda 1903 ekranında hiçbir buton görmesin.


    ELSEIF gs_scr_1903-onay_durum-onay_durum EQ '02'.

    ELSEIF gs_scr_1903-onay_durum-onay_durum EQ '03'.


    ENDIF.
    " Liste hazırlamada ilk buton seçiliyse liste hazırlayıcısı buton görmesin.
    ls_fcode = 'TUMU'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'KALDIR'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'SATIRSEC'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'SATIRKALD'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'KAYDET'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'SIL'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'BLOKAJ'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'ONAY'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'REDDET'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'GERIAL'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'HEPSDAGIT'."inserted by ozans - 13.06.2020
    APPEND ls_fcode TO lt_fcode.


  ELSEIF p_r2 EQ 'X'.

    " Liste hazırlamada ilk buton seçiliyse liste hazırlayıcısı sil hariç buton görmesin.
    ls_fcode = 'TUMU'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'KALDIR'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'SATIRSEC'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'SATIRKALD'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'KAYDET'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'BLOKAJ'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'ONAY'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'REDDET'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'GERIAL'.
    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'SIL'.
    APPEND ls_fcode TO lt_fcode.
*    ls_fcode = 'SATIRSIL'. "inserted by ozans - 13.06.2020
*    APPEND ls_fcode TO lt_fcode.
    ls_fcode = 'HEPSDAGIT'."inserted by ozans - 13.06.2020
    APPEND ls_fcode TO lt_fcode.
  ENDIF.

  "Ekran onay durumundan farklı olarak screen group G2 olan her şeyi
  "Liste hazırlayıcısından kapat.
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'G2'.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    "set_liste_hazirlayicisi_screen
*&---------------------------------------------------------------------*
*&      Form  set_category_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_FCODE   text
*----------------------------------------------------------------------*
FORM set_category_screen TABLES lt_fcode.

  "kategori ekibi ikinci ekranda sadece onaylama işlemi yapabilir
  DATA: ls_fcode TYPE sy-ucomm.
  ls_fcode = 'TUMU'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'KALDIR'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'SATIRSEC'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'SATIRKALD'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'KAYDET'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'SIL'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'BLOKAJ'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'REDDET'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'GERIAL'.
  APPEND ls_fcode TO lt_fcode.
*  ls_fcode = 'SATIRSIL'. "inserted by ozans - 13.06.2020
*  APPEND ls_fcode TO lt_fcode.

  "eğer belgeler onaycıya gönderilmişse ekranda G2 olan her şeyi ve onayla butonunu kapat.
  IF gs_scr_1903-onay_durum-onay_durum NE '02'.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'G2'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
    ls_fcode = 'ONAY'.
    APPEND ls_fcode TO lt_fcode.
  ENDIF.
ENDFORM.                    "set_category_screen
*&---------------------------------------------------------------------*
*&      Form  set_onayci_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_FCODE   text
*----------------------------------------------------------------------*
FORM set_onayci_screen TABLES lt_fcode.
  "kategori ekibi ikinci ekranda sadece onaylama işlemi yapabilir
  DATA: ls_fcode TYPE sy-ucomm.
  ls_fcode = 'TUMU'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'KALDIR'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'SATIRSEC'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'SATIRKALD'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'KAYDET'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'SIL'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'BLOKAJ'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'REDDET'.
  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'GERIAL'.
  APPEND ls_fcode TO lt_fcode.
*  ls_fcode = 'SATIRSIL'. "inserted by ozans - 13.06.2020
*  APPEND ls_fcode TO lt_fcode.
  ls_fcode = 'HEPSDAGIT'."inserted by ozans - 13.06.2020
  APPEND ls_fcode TO lt_fcode.

  "eğer belgeler onaycıya gönderilmişse ekranda G2 olan her şeyi ve onayla butonunu kapat.
  IF gs_scr_1903-onay_durum-onay_durum NE '03'.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'G2'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
    ls_fcode = 'ONAY'.
    APPEND ls_fcode TO lt_fcode.
  ENDIF.
ENDFORM.                    "set_onayci_screen
*&---------------------------------------------------------------------*
*&      Form  SET_ICONS
*&---------------------------------------------------------------------*
FORM set_icons .
  DATA: status_icon TYPE icons-text.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = gs_scr_1903-onay_durum-icon_name
      text                  = gs_scr_1903-onay_durum-icon_text
      info                  = 'Status'
      add_stdinf            = 'X'
    IMPORTING
      result                = gs_scr_1903-onay_durum-icon_name
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.
ENDFORM.                    "set_icons
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT
*&---------------------------------------------------------------------*
FORM set_fieldcat .
  DATA: ls_fieldcat TYPE lvc_s_fcat.
  CHECK gs_scr_1903-t_fieldcat[] IS INITIAL.

  CLEAR: gs_scr_1903-t_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSOG_FI_018_S_02'
      i_client_never_display = 'X'
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = gs_scr_1903-t_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  CLEAR: ls_fieldcat .
  ls_fieldcat-hotspot = 'X'.
  MODIFY  gs_scr_1903-t_fieldcat FROM ls_fieldcat TRANSPORTING hotspot WHERE fieldname = 'BELNR'.

  "ozanss
  CLEAR: ls_fieldcat .
  ls_fieldcat-tech = 'X'.
  MODIFY  gs_scr_1903-t_fieldcat FROM ls_fieldcat TRANSPORTING tech    WHERE fieldname = 'KDVLI_TUT'
                                                                          OR fieldname = 'KDVSIZ_TUT'
                                                                          OR fieldname = 'KDV_TUT'
                                                                          OR fieldname = 'CAT_DEVLET_ODEME'
                                                                          OR fieldname = 'ONY_DEVLET_ODEME'.



ENDFORM.                    "set_fieldcat
*&---------------------------------------------------------------------*
*&      Form  change_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM change_fieldcat.
  CASE gs_scr_1903-auth-statu.
    WHEN '01'.
      PERFORM set_liste_hazirlayicisi_fcat TABLES gs_scr_1903-t_fieldcat.
    WHEN '02'.
      PERFORM set_category_fcat TABLES gs_scr_1903-t_fieldcat.
    WHEN '03'.
      PERFORM set_onayci_fcat TABLES gs_scr_1903-t_fieldcat.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "change_fieldcat
*&---------------------------------------------------------------------*
*&      Form  set_liste_hazirlayicisi_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM set_liste_hazirlayicisi_fcat TABLES pt_fieldcat STRUCTURE lvc_s_fcat.

  DATA: ls_fieldcat TYPE lvc_s_fcat.
  ls_fieldcat-tech  = 'X'.
  IF gs_scr_1903-onay_durum-onay_durum NE '04'.
    MODIFY  gs_scr_1903-t_fieldcat FROM ls_fieldcat TRANSPORTING tech  WHERE fieldname = 'SECIM'
                                                                         OR  fieldname = 'CAT_SATICI_ODEME'
                                                                         OR  fieldname = 'CAT_DEVLET_ODEME'
                                                                         OR  fieldname = 'CAT_ONAY'
                                                                         OR  fieldname = 'ONY_SATICI_ODEME'
                                                                         OR  fieldname = 'ONY_DEVLET_ODEME'
                                                                         OR  fieldname = 'ONY_ONAY'.
  ELSE.
    MODIFY  gs_scr_1903-t_fieldcat FROM ls_fieldcat TRANSPORTING tech  WHERE fieldname = 'SECIM'.
  ENDIF.

ENDFORM.                    "set_liste_hazirlayicisi_fcat
*&---------------------------------------------------------------------*
*&      Form  set_category_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM set_category_fcat TABLES pt_fieldcat STRUCTURE lvc_s_fcat.

  DATA: ls_fieldcat TYPE lvc_s_fcat.
  ls_fieldcat-checkbox = 'X'.
  MODIFY  gs_scr_1903-t_fieldcat FROM ls_fieldcat TRANSPORTING checkbox  WHERE fieldname = 'SECIM'.
  CLEAR: ls_fieldcat.
  ls_fieldcat-tech  = 'X'.
  MODIFY  gs_scr_1903-t_fieldcat FROM ls_fieldcat TRANSPORTING tech  WHERE fieldname = 'SECIM'
                                                                       OR  fieldname = 'ONY_SATICI_ODEME'
                                                                       OR  fieldname = 'ONY_DEVLET_ODEME'
                                                                       OR  fieldname = 'ONY_ONAY'
                                                                       OR  fieldname = 'CAT_DEVLET_ODEME'."ozans
ENDFORM.                    "set_category_fcat
*&---------------------------------------------------------------------*
*&      Form  set_onayci_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM set_onayci_fcat TABLES pt_fieldcat STRUCTURE lvc_s_fcat.
  DATA: ls_fieldcat TYPE lvc_s_fcat.
  ls_fieldcat-tech  = 'X'.
  MODIFY  gs_scr_1903-t_fieldcat FROM ls_fieldcat TRANSPORTING tech  WHERE fieldname = 'ONY_DEVLET_ODEME'
                                                                        OR fieldname = 'CAT_DEVLET_ODEME'."ozans

ENDFORM.                    "set_onayci_fcat
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
FORM exclude_tb_functions  CHANGING pt_exclude TYPE ui_functions.
  CLEAR: pt_exclude,pt_exclude[].
  DATA ls_exclude TYPE ui_func.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude =   cl_gui_alv_grid=>mc_fc_refresh .
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_maintain_variant.
  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_sum.
*  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_to_office.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_help.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_print.
  APPEND ls_exclude TO pt_exclude.

ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_GRID
*&---------------------------------------------------------------------*
FORM display_alv_grid .
  DATA: ls_variant TYPE disvariant.

  IF gs_scr_1903-r_custom_container IS INITIAL.
    CREATE OBJECT gs_scr_1903-r_custom_container
      EXPORTING
        container_name = gv_container.

    CREATE OBJECT gs_scr_1903-r_grid1
      EXPORTING
        i_parent = gs_scr_1903-r_custom_container.

    gs_scr_1903-s_layout-zebra      = 'X'.
    gs_scr_1903-s_layout-cwidth_opt = 'X'.
    gs_scr_1903-s_layout-sel_mode   = 'D'.
*    gs_scr_1903-s_layout-stylefname = 'CELLSTYLES'.
*    gs_scr_1903-s_layout-info_fname  = 'INFO_FNAME'.
*    gs_scr_1903-s_layout-no_f4      = 'X'.

    ls_variant-report       = sy-repid.
    ls_variant-username     = sy-uname.
    CREATE OBJECT gr_alv_event_ref.
    SET HANDLER gr_alv_event_ref->handle_hotspot_click FOR gs_scr_1903-r_grid1.
    SET HANDLER gr_alv_event_ref->handle_toolbar       FOR gs_scr_1903-r_grid1.
    SET HANDLER gr_alv_event_ref->handle_user_command  FOR gs_scr_1903-r_grid1.
    SET HANDLER gr_alv_event_ref->handle_data_changed  FOR gs_scr_1903-r_grid1.
*    SET HANDLER gr_alv_event_ref->refresh_changed_data FOR gr_grid1.
    CALL METHOD gs_scr_1903-r_grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = gs_scr_1903-t_exclude
        i_structure_name     = 'ZSOG_FI_018_S_02'
        is_layout            = gs_scr_1903-s_layout
        is_variant           = ls_variant
        i_save               = 'A'
      CHANGING
        it_outtab            = gs_scr_1903-filter[]
        it_fieldcatalog      = gs_scr_1903-t_fieldcat.

    CALL METHOD gs_scr_1903-r_grid1->set_toolbar_interactive.

    CALL METHOD gs_scr_1903-r_grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD gs_scr_1903-r_grid1->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ELSE.
    PERFORM refresh_table_display.
  ENDIF.

ENDFORM.                    "display_alv_grid
*&---------------------------------------------------------------------*
*& Form REFRESH_TABLE_DISPLAY
*&---------------------------------------------------------------------*
FORM refresh_table_display .

  DATA: ls_stable TYPE lvc_s_stbl.
  ls_stable-row = 'X'.
  ls_stable-col = 'X'.

  CALL METHOD gs_scr_1903-r_grid1->refresh_table_display
    EXPORTING
      is_stable = ls_stable   " With Stable Rows/Columns
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.
ENDFORM.                    "refresh_table_display
*&---------------------------------------------------------------------*
*& Form 1903_EXIT_CODES
*&---------------------------------------------------------------------*
FORM 1903_exit_codes .
  CLEAR: gs_scr_1903-ucomm.
  LEAVE TO SCREEN 0.
ENDFORM.                    "1903_exit_codes
*&---------------------------------------------------------------------*
*&      Form  CHECK_CHAIN
*&---------------------------------------------------------------------*
FORM check_chain .
  DATA: lv_tutar     TYPE bsik-wrbtr,
        lv_text(18),
        lv_text2(18).

  IF gs_scr_1903-category-odenecek_tutar < 0.
    CLEAR: gs_scr_1903-ucomm.
    MESSAGE e007. "WITH 'Ödenecek tutar negatif olamaz!'. "& & & &
  ENDIF.

  IF gs_scr_1903-category-odenecek_tutar > abs( gs_scr_1903-category-toplam_tutar ).
    CLEAR: gs_scr_1903-ucomm.
    MESSAGE e008." WITH 'Kalan tutardan daha az değer giriniz!'.
  ENDIF.
  IF gs_scr_1903-category-odenecek_tutar > p_oneri.
    CLEAR: gs_scr_1903-ucomm.
    MESSAGE e017.
  ENDIF.

  FIELD-SYMBOLS: <fs_detail> LIKE LINE OF gs_scr_1903-detail.

  LOOP AT gs_scr_1903-detail ASSIGNING <fs_detail> WHERE lifnr NE gs_scr_1903-category-lifnr.
    lv_tutar = lv_tutar + <fs_detail>-odenecek_tutar.
  ENDLOOP.
  lv_tutar = lv_tutar + gs_scr_1903-category-odenecek_tutar.
  IF lv_tutar > p_oneri.
    CLEAR: gs_scr_1903-ucomm.
    WRITE p_oneri TO lv_text .
    lv_tutar = lv_tutar - p_oneri.
    WRITE lv_tutar TO lv_text2 .
    MESSAGE i020 WITH p_oneri lv_tutar .
    MESSAGE e017.
  ENDIF.
ENDFORM.                    "check_chain
*&---------------------------------------------------------------------*
*& Form USER_COMMAND_1903
*&---------------------------------------------------------------------*
FORM user_command_1903 .

  CALL METHOD gs_scr_1903-r_grid1->check_changed_data.
  CASE gs_scr_1903-ucomm.
    WHEN 'TUMU'.
      PERFORM select_all.
    WHEN 'KALDIR'.
      PERFORM deselect_all.
    WHEN 'SATIRSEC'.
      PERFORM select_rows.
    WHEN 'SATIRKALD'.
      PERFORM deselect_rows.
    WHEN 'KAYDET'.
      PERFORM save.
    WHEN 'SIL'.
      PERFORM sil.
    WHEN '&F03' OR '&F15' OR '&F12'.
      PERFORM back_buttons.
    WHEN 'HESAP'.
      PERFORM hesapla.
    WHEN 'RESET'.
      PERFORM resetle.
    WHEN 'ONAY'.
      PERFORM onay.
  ENDCASE.
  CLEAR: gs_scr_1903-ucomm.
ENDFORM.                    "user_command_1903
*&---------------------------------------------------------------------*
*&      Form  back_buttons
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM back_buttons.
  DATA: lv_answer(1)  TYPE c.
  FIELD-SYMBOLS : <fs_filter> LIKE LINE OF gs_scr_1903-filter.
  CASE gs_scr_1903-auth-statu.
    WHEN '01'.
      CLEAR: gs_scr_1903-ucomm.
      LEAVE TO SCREEN 0.
    WHEN '02'.
      LOOP AT gs_scr_1903-filter ASSIGNING <fs_filter> WHERE cat_onay = '@0V@'.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        PERFORM ask_save_to_category CHANGING lv_answer.
        IF lv_answer NE '1'.
          CLEAR: gs_scr_1903-ucomm.
          RETURN.
        ENDIF.
        CLEAR: gs_scr_1903-ucomm.
        LEAVE TO SCREEN 0.
      ELSE."ozanss
        CLEAR: gs_scr_1903-ucomm.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN '03'.
      CLEAR: gs_scr_1903-ucomm.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "back_buttons
*&---------------------------------------------------------------------*
*& Form CHOOSE_ALL
*&---------------------------------------------------------------------*
FORM select_all .

ENDFORM.                    "select_all
*&---------------------------------------------------------------------*
*& Form DESELECT_ALL
*&---------------------------------------------------------------------*
FORM deselect_all .

ENDFORM.                    "deselect_all
*&---------------------------------------------------------------------*
*& Form SELECT_ROWS
*&---------------------------------------------------------------------*
FORM select_rows .


ENDFORM.                    "select_rows
*&---------------------------------------------------------------------*
*& Form DESELECT_ROWS
*&---------------------------------------------------------------------*
FORM deselect_rows .

ENDFORM.                    "deselect_rows
*&---------------------------------------------------------------------*
*& Form SAVE
*&---------------------------------------------------------------------*
FORM save .

ENDFORM.                    "save
*&---------------------------------------------------------------------*
*&      Form  HESAPLA
*&---------------------------------------------------------------------*
FORM hesapla .
  CASE gs_scr_1903-auth-statu.
    WHEN '01'.

    WHEN '02'.
      PERFORM category_calculate.
    WHEN '03'.
      PERFORM approval_calculate.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "hesapla
*&---------------------------------------------------------------------*
*&      Form  resetle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM resetle.

ENDFORM.                    "resetle
*&---------------------------------------------------------------------*
*&      Form  sil
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sil.

ENDFORM.                    "sil
*&---------------------------------------------------------------------*
*&      Form  ask_save_to_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_ANSWER  text
*----------------------------------------------------------------------*
FORM ask_save_to_category CHANGING lv_answer.
  PERFORM pop_up_confirm USING text-013 text-039  text-009 text-010 text-011
                               text-012 CHANGING lv_answer .
ENDFORM.                    "ask_save_to_category
*&---------------------------------------------------------------------*
*&      Form  category_calculate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM category_calculate.
  DATA: ls_reset    TYPE zsog_fi_018_s_02.
  "kategori ekibi hesapla butonuna bastığı zaman.
  "0-Belge tutarı toplam belge tutarlarından, kısmi ödeme belgelerinin çıkartılmış tutarı olmalı.


  "1-belge tutarını tam karışılıyor mu?
  "belge tutarını tam karşılıyorsa, KDV'siz kısmı kategori satıcı yaz,
  "KDV tutarını kategori devlete yaz.

  "2-Belge tutarını tam karşılamıyorsa vergisi var mı yok mu?
  " vergisi varsa ödenecek tutardan vergi hesapla, vergili tutarı kategori devlete yaz,
  "girilen tutar'dan vergiyi çıkartıp kategory satıcıya yaz.

  ls_reset-secim = ''.
  ls_reset-cat_devlet_odeme = 0.
  ls_reset-cat_satici_odeme = 0.
  ls_reset-cat_onay = ''.

  DATA: ls_filter_read LIKE LINE OF gs_scr_1903-filter.
  DATA: lv_tutar LIKE gs_scr_1903-category-odenecek_tutar.
  DATA: ls_filter LIKE LINE OF gs_scr_1903-filter.
  DATA: lv_tabix TYPE sy-tabix.
  FIELD-SYMBOLS: <fs_kismi> LIKE LINE OF gs_scr_1903-kismi.

  MODIFY gs_scr_1903-filter FROM ls_reset TRANSPORTING secim cat_devlet_odeme cat_satici_odeme cat_onay WHERE secim IS NOT INITIAL .
  CLEAR: ls_filter_read.
  READ TABLE gs_scr_1903-filter INTO ls_filter_read INDEX 1.
*  DATA(ls_filter_read) = gs_scr_1903-filter[ 1 ].

  CLEAR: lv_tutar.
  lv_tutar = gs_scr_1903-category-odenecek_tutar.
  LOOP AT gs_scr_1903-filter INTO ls_filter WHERE kalan_odeme > 0.
*    DATA(lv_tabix) = sy-tabix.
    CLEAR :lv_tabix.
    lv_tabix = sy-tabix.

    IF lv_tutar EQ 0.
      EXIT.
    ENDIF.
    IF lv_tutar > ls_filter-kalan_odeme. " Burada tam ödeme senaryosu gerçekleşiyor.
*      ls_filter-cat_devlet_odeme = ls_filter-kdv_tut.
*      ls_filter-cat_satici_odeme = ls_filter-kdvsiz_tut.
      IF ls_filter-mwskz IS INITIAL.
        ls_filter-mwskz = 'V0'.
      ENDIF.
*      PERFORM get_tax_percentage USING ls_filter-mwskz ls_filter-kalan_odeme CHANGING ls_filter-cat_devlet_odeme ls_filter-cat_satici_odeme."ozans
      ls_filter-cat_satici_odeme = ls_filter-kalan_odeme."ozans
*      ls_filter-cat_satici_odeme = ls_filter-belge_tutar."ozans
*      ls_filter-odenecek_tutar   = ls_filter-cat_devlet_odeme + ls_filter-cat_satici_odeme."ozans
      ls_filter-odenecek_tutar   =  ls_filter-cat_satici_odeme."ozans
*      ls_filter-belgeden_kalan   = ls_filter-belge_tutar - ( ls_filter-cat_devlet_odeme + ls_filter-cat_satici_odeme ).

      ls_filter-belgeden_kalan    = ls_filter-belge_tutar - ls_filter-kalan_odeme.
      lv_tutar = lv_tutar - ls_filter-kalan_odeme.
*      lv_tutar = lv_tutar - ls_filter-belge_tutar."ozans
      LOOP AT gs_scr_1903-kismi ASSIGNING <fs_kismi> WHERE bukrs = ls_filter-bukrs
                                                       AND rebzg = ls_filter-belnr
                                                       AND rebzj = ls_filter-gjahr.

        ls_filter-belgeden_kalan = ls_filter-belgeden_kalan - <fs_kismi>-wrbtr.
      ENDLOOP.
    ELSEIF lv_tutar > 0. "burada kısmi ödeme olayı gerçekleşiyor.
      IF ls_filter-mwskz IS INITIAL.
        ls_filter-mwskz = 'V0'.
      ENDIF.
*      PERFORM get_tax_percentage USING ls_filter-mwskz lv_tutar CHANGING ls_filter-cat_devlet_odeme ls_filter-cat_satici_odeme."ozans
      ls_filter-cat_satici_odeme = lv_tutar.
      lv_tutar = 0.
*      ls_filter-odenecek_tutar   = ls_filter-cat_devlet_odeme + ls_filter-cat_satici_odeme."ozans
      ls_filter-odenecek_tutar   =  ls_filter-cat_satici_odeme."ozans
*      ls_filter-belgeden_kalan   = ls_filter-belge_tutar - ( ls_filter-cat_devlet_odeme + ls_filter-cat_satici_odeme )."ozans
      ls_filter-belgeden_kalan   = ls_filter-belge_tutar -  ls_filter-cat_satici_odeme ."ozans
      LOOP AT gs_scr_1903-kismi ASSIGNING <fs_kismi> WHERE bukrs = ls_filter-bukrs
                                                       AND rebzg = ls_filter-belnr
                                                       AND rebzj = ls_filter-gjahr.

        ls_filter-belgeden_kalan = ls_filter-belgeden_kalan - <fs_kismi>-wrbtr.
      ENDLOOP.
    ENDIF.
    ls_filter-secim = 'X'.
    MODIFY gs_scr_1903-filter FROM ls_filter INDEX lv_tabix TRANSPORTING cat_satici_odeme cat_devlet_odeme secim odenecek_tutar belgeden_kalan.
  ENDLOOP.

ENDFORM.                    "category_calculate
*&---------------------------------------------------------------------*
*&      Form  approval_calculate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM approval_calculate.
  DATA: ls_reset    TYPE zsog_fi_018_s_02.
  "kategori ekibi hesapla butonuna bastığı zaman.
  "0-Belge tutarı toplam belge tutarlarından, kısmi ödeme belgelerinin çıkartılmış tutarı olmalı.


  "1-belge tutarını tam karışılıyor mu?
  "belge tutarını tam karşılıyorsa, KDV'siz kısmı kategori satıcı yaz,
  "KDV tutarını kategori devlete yaz.

  "2-Belge tutarını tam karşılamıyorsa vergisi var mı yok mu?
  " vergisi varsa ödenecek tutardan vergi hesapla, vergili tutarı kategori devlete yaz,
  "girilen tutar'dan vergiyi çıkartıp kategory satıcıya yaz.

  ls_reset-secim = ''.
  ls_reset-ony_devlet_odeme = 0.
  ls_reset-ony_satici_odeme = 0.
  ls_reset-ony_onay = ''.

  DATA: ls_filter_read LIKE LINE OF gs_scr_1903-filter.
  FIELD-SYMBOLS: <fs_sum_alv> LIKE LINE OF gs_scr_1903-sum_alv.
  FIELD-SYMBOLS: <fs_kismi>   LIKE LINE OF gs_scr_1903-kismi.
  DATA: lv_tutar LIKE gs_scr_1903-category-odenecek_tutar.
  DATA: ls_filter LIKE LINE OF gs_scr_1903-filter.
  DATA: lv_index TYPE sy-tabix.

  MODIFY gs_scr_1903-filter FROM ls_reset TRANSPORTING secim ony_devlet_odeme ony_satici_odeme ony_onay WHERE secim IS NOT INITIAL .
*  DATA(ls_filter_read) = gs_scr_1903-filter[ 1 ].
  CLEAR: ls_filter_read.
  READ TABLE gs_scr_1903-filter INTO ls_filter_read INDEX 1.

  READ TABLE gs_scr_1903-sum_alv ASSIGNING <fs_sum_alv> WITH KEY lifnr = ls_filter_read-lifnr.
  CLEAR: <fs_sum_alv>-odenecek_tutar,
         <fs_sum_alv>-belgeden_kalan.

  CLEAR: lv_tutar.
  lv_tutar = gs_scr_1903-category-odenecek_tutar.
  LOOP AT gs_scr_1903-filter INTO ls_filter WHERE kalan_odeme > 0.
    CLEAR: lv_index.
    lv_index = sy-tabix.
    IF lv_tutar EQ 0.
      EXIT.
    ENDIF.
    IF lv_tutar > ls_filter-kalan_odeme. " Burada tam ödeme senaryosu gerçekleşiyor.
*      ls_filter-ony_devlet_odeme = ls_filter-kdv_tut.
*      ls_filter-ony_satici_odeme = ls_filter-kdvsiz_tut.
      IF ls_filter-mwskz IS INITIAL.
        ls_filter-mwskz = 'V0'.
      ENDIF.
*      PERFORM get_tax_percentage USING ls_filter-mwskz ls_filter-kalan_odeme CHANGING ls_filter-ony_devlet_odeme ls_filter-ony_satici_odeme."ozans
      ls_filter-ony_satici_odeme = ls_filter-kalan_odeme."ozans
*      ls_filter-odenecek_tutar   = ls_filter-ony_devlet_odeme + ls_filter-ony_satici_odeme."ozans
      ls_filter-odenecek_tutar   =  ls_filter-ony_satici_odeme."ozans
*      ls_filter-belgeden_kalan   = ls_filter-belge_tutar - ( ls_filter-ony_devlet_odeme + ls_filter-ony_satici_odeme ).
      ls_filter-belgeden_kalan    = ls_filter-belge_tutar - ls_filter-kalan_odeme.
      lv_tutar = lv_tutar - ls_filter-kalan_odeme.
      LOOP AT gs_scr_1903-kismi ASSIGNING <fs_kismi> WHERE bukrs = ls_filter-bukrs
                                                       AND rebzg = ls_filter-belnr
                                                       AND rebzj = ls_filter-gjahr.

        ls_filter-belgeden_kalan = ls_filter-belgeden_kalan - <fs_kismi>-wrbtr.
      ENDLOOP.
    ELSEIF lv_tutar > 0. "burada kısmi ödeme olayı gerçekleşiyor.
      IF ls_filter-mwskz IS INITIAL.
        ls_filter-mwskz = 'V0'.
      ENDIF.
*      PERFORM get_tax_percentage USING ls_filter-mwskz lv_tutar CHANGING ls_filter-ony_devlet_odeme ls_filter-ony_satici_odeme."ozans
      ls_filter-ony_satici_odeme = lv_tutar ."ozans
      lv_tutar = 0.
*      ls_filter-odenecek_tutar   = ls_filter-ony_devlet_odeme + ls_filter-ony_satici_odeme."ozans
      ls_filter-odenecek_tutar   = ls_filter-ony_satici_odeme."ozans
*      ls_filter-belgeden_kalan   = ls_filter-belge_tutar - ( ls_filter-ony_devlet_odeme + ls_filter-ony_satici_odeme )."ozans
      ls_filter-belgeden_kalan   = ls_filter-belge_tutar -  ls_filter-ony_satici_odeme ."ozans
      LOOP AT gs_scr_1903-kismi ASSIGNING <fs_kismi> WHERE bukrs = ls_filter-bukrs
                                                       AND rebzg = ls_filter-belnr
                                                       AND rebzj = ls_filter-gjahr.

        ls_filter-belgeden_kalan = ls_filter-belgeden_kalan - <fs_kismi>-wrbtr.
      ENDLOOP.

    ENDIF.
    ls_filter-secim = 'X'.
    MODIFY gs_scr_1903-filter FROM ls_filter INDEX lv_index TRANSPORTING ony_satici_odeme ony_devlet_odeme secim odenecek_tutar belgeden_kalan.
  ENDLOOP.

ENDFORM.                    "approval_calculate
*&---------------------------------------------------------------------*
*&      Form  get_tax_percentage
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_MWSKZ   text
*      -->PV_TUTAR   text
*      -->PV_KDV     text
*      -->PV_NET     text
*----------------------------------------------------------------------*
FORM get_tax_percentage USING pv_mwskz TYPE bseg-mwskz
                              pv_tutar  TYPE bseg-wrbtr
                        CHANGING pv_kdv TYPE bseg-wrbtr
                                 pv_net TYPE bseg-wrbtr.

  DATA: lt_ftaxp TYPE TABLE OF ftaxp,
        ls_ftaxp LIKE LINE OF  lt_ftaxp,
        lv_kbetr TYPE ftaxp-kbetr.
  CALL FUNCTION 'GET_TAX_PERCENTAGE'
    EXPORTING
      aland   = 'TR'
      datab   = sy-datum
      mwskz   = pv_mwskz
      txjcd   = 'TAXTR'
    TABLES
      t_ftaxp = lt_ftaxp.
  IF lt_ftaxp IS INITIAL.
    RETURN.
  ENDIF.
*  DATA(ls_ftaxp) = lt_ftaxp[ 1 ].
  CLEAR: ls_ftaxp.
  READ TABLE lt_ftaxp INTO ls_ftaxp INDEX 1.
  IF ls_ftaxp-kbetr EQ 0.
    pv_kdv = 0.
    pv_net = pv_tutar.
  ELSE.
    lv_kbetr     = ls_ftaxp-kbetr / 1000.
    pv_kdv = pv_tutar - ( pv_tutar / ( 1 + lv_kbetr ) ).
    pv_net =  ( pv_tutar / ( 1 + lv_kbetr ) ).
  ENDIF.

ENDFORM.                    "get_tax_percentage
*&---------------------------------------------------------------------*
*&      Form  ONAY
*&---------------------------------------------------------------------*
FORM onay .
  DATA: lt_dummy  TYPE esp1_message_tab_type."inserted by ozans - 13.06.2020
  CASE gs_scr_1903-auth-statu.
    WHEN '01'.

    WHEN '02'.
      PERFORM category_approve USING '' CHANGING lt_dummy.
    WHEN '03'.
      PERFORM approver_approve.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "onay
*&---------------------------------------------------------------------*
*&      Form  category_approve
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM category_approve USING p_background
                   CHANGING et_tab TYPE esp1_message_tab_type.
  DATA: lt_tab      TYPE esp1_message_tab_type,
        ls_item_tab TYPE zsog_fi_018_t_04.

  FIELD-SYMBOLS: <fs_filter> LIKE LINE OF gs_scr_1903-filter.
  FIELD-SYMBOLS: <fs_sum>    LIKE LINE OF gs_scr_1903-sum_alv.

  LOOP AT gs_scr_1903-filter ASSIGNING <fs_filter> WHERE secim = 'X'.
    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE i009.
    RETURN.
  ENDIF.
  <fs_filter>-cat_onay = '@0V@'.
  MODIFY gs_scr_1903-filter FROM <fs_filter> TRANSPORTING cat_onay WHERE secim = 'X'.

  READ TABLE gs_scr_1903-sum_alv ASSIGNING <fs_sum> WITH KEY lifnr = <fs_filter>-lifnr.

  LOOP AT gs_scr_1903-filter ASSIGNING <fs_filter> WHERE secim = 'X'.

    MODIFY <fs_sum>-detail_alv FROM <fs_filter> TRANSPORTING cat_satici_odeme cat_devlet_odeme secim cat_onay odenecek_tutar  belgeden_kalan
                                                     WHERE laufd = <fs_filter>-laufd
                                                       AND laufi = <fs_filter>-laufi
                                                       AND bukrs = <fs_filter>-bukrs
                                                       AND lifnr = <fs_filter>-lifnr
                                                       AND belnr = <fs_filter>-belnr
                                                       AND gjahr = <fs_filter>-gjahr
                                                       AND buzei = <fs_filter>-buzei.

    MODIFY gs_scr_1903-detail FROM <fs_filter> TRANSPORTING cat_satici_odeme cat_devlet_odeme secim cat_onay odenecek_tutar  belgeden_kalan
                                                     WHERE laufd = <fs_filter>-laufd
                                                       AND laufi = <fs_filter>-laufi
                                                       AND bukrs = <fs_filter>-bukrs
                                                       AND lifnr = <fs_filter>-lifnr
                                                       AND belnr = <fs_filter>-belnr
                                                       AND gjahr = <fs_filter>-gjahr
                                                       AND buzei = <fs_filter>-buzei.

    ls_item_tab-secim            = <fs_filter>-secim.
    ls_item_tab-cat_satici_odeme = <fs_filter>-cat_satici_odeme.
    ls_item_tab-cat_devlet_odeme = <fs_filter>-cat_devlet_odeme.
    ls_item_tab-cat_onay         = <fs_filter>-cat_onay.
    ls_item_tab-odenecek_tutar   = <fs_filter>-odenecek_tutar.
    ls_item_tab-belgeden_kalan   = <fs_filter>-belgeden_kalan.
    MODIFY gs_scr_1903-item_tab FROM ls_item_tab TRANSPORTING secim cat_satici_odeme cat_devlet_odeme cat_onay odenecek_tutar belgeden_kalan
                               WHERE laufd = <fs_filter>-laufd
                                 AND laufi = <fs_filter>-laufi
                                 AND bukrs = <fs_filter>-bukrs
                                 AND lifnr = <fs_filter>-lifnr
                                 AND belnr = <fs_filter>-belnr
                                 AND gjahr = <fs_filter>-gjahr
                                 AND buzei = <fs_filter>-buzei.
    CLEAR: ls_item_tab.

  ENDLOOP.

  FIELD-SYMBOLS: <fs_detail> LIKE LINE OF <fs_sum>-detail_alv.
  LOOP AT gs_scr_1903-sum_alv ASSIGNING <fs_sum>.
    CLEAR: <fs_sum>-odenecek_tutar,
           <fs_sum>-belgeden_kalan.
    LOOP AT <fs_sum>-detail_alv ASSIGNING <fs_detail> .
      <fs_sum>-odenecek_tutar = <fs_sum>-odenecek_tutar + <fs_detail>-odenecek_tutar.
      <fs_sum>-belgeden_kalan = <fs_sum>-belgeden_kalan + <fs_detail>-belgeden_kalan.
*      LOOP AT gs_scr_1903-kismi ASSIGNING FIELD-SYMBOL(<fs_kismi>) WHERE bukrs = <fs_detail>-bukrs
*                                                       AND rebzg = <fs_detail>-belnr
*                                                       AND rebzj = <fs_detail>-gjahr.
*
*        <fs_sum>-belgeden_kalan = <fs_sum>-belgeden_kalan - <fs_kismi>-wrbtr.
*      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
  gs_scr_1903-r_alv->refresh( ).
  MODIFY zsog_fi_018_t_04 FROM TABLE gs_scr_1903-item_tab.
  IF sy-subrc EQ 0.
    PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'W' '000' text-025  '' '' ''.
  ENDIF.
  IF lt_tab IS NOT INITIAL.
*{   ->>> Inserted by Prodea Ozan Şahin - 13.06.2020 18:14:23
    IF p_background = 'X'.
      APPEND LINES OF lt_tab to et_tab.
    ELSE.
      PERFORM c14z_messages_show_as_popup TABLES lt_tab.
    ENDIF.
*}     <<<- End of   Inserted - 13.06.2020 18:14:23


  ENDIF.
ENDFORM.                    "category_approve
*&---------------------------------------------------------------------*
*&      Form  approver_approve
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM approver_approve.
  DATA: lt_tab      TYPE esp1_message_tab_type,
        ls_item_tab TYPE zsog_fi_018_t_04.

  FIELD-SYMBOLS: <fs_filter> LIKE LINE OF gs_scr_1903-filter.
  FIELD-SYMBOLS: <fs_sum>    LIKE LINE OF gs_scr_1903-sum_alv.

  LOOP AT gs_scr_1903-filter ASSIGNING <fs_filter> WHERE secim = 'X'.
    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE i009.
    RETURN.
  ENDIF.
  <fs_filter>-ony_onay = '@0V@'.
  MODIFY gs_scr_1903-filter FROM <fs_filter> TRANSPORTING ony_onay WHERE secim = 'X'.

  READ TABLE gs_scr_1903-sum_alv ASSIGNING <fs_sum> WITH KEY lifnr = <fs_filter>-lifnr.

  LOOP AT gs_scr_1903-filter ASSIGNING <fs_filter> WHERE secim = 'X'.

    MODIFY <fs_sum>-detail_alv FROM <fs_filter> TRANSPORTING ony_satici_odeme ony_devlet_odeme secim ony_onay odenecek_tutar  belgeden_kalan
                                                     WHERE laufd = <fs_filter>-laufd
                                                       AND laufi = <fs_filter>-laufi
                                                       AND bukrs = <fs_filter>-bukrs
                                                       AND lifnr = <fs_filter>-lifnr
                                                       AND belnr = <fs_filter>-belnr
                                                       AND gjahr = <fs_filter>-gjahr
                                                       AND buzei = <fs_filter>-buzei.

    MODIFY gs_scr_1903-detail FROM <fs_filter> TRANSPORTING ony_satici_odeme ony_devlet_odeme secim ony_onay odenecek_tutar  belgeden_kalan
                                                     WHERE laufd = <fs_filter>-laufd
                                                       AND laufi = <fs_filter>-laufi
                                                       AND bukrs = <fs_filter>-bukrs
                                                       AND lifnr = <fs_filter>-lifnr
                                                       AND belnr = <fs_filter>-belnr
                                                       AND gjahr = <fs_filter>-gjahr
                                                       AND buzei = <fs_filter>-buzei.

    ls_item_tab-secim            = <fs_filter>-secim.
    ls_item_tab-ony_satici_odeme = <fs_filter>-ony_satici_odeme.
    ls_item_tab-ony_devlet_odeme = <fs_filter>-ony_devlet_odeme.
    ls_item_tab-ony_onay         = <fs_filter>-cat_onay.
    ls_item_tab-odenecek_tutar   = <fs_filter>-odenecek_tutar.
    ls_item_tab-belgeden_kalan   = <fs_filter>-belgeden_kalan.

    MODIFY gs_scr_1903-item_tab FROM ls_item_tab TRANSPORTING secim ony_satici_odeme ony_devlet_odeme ony_onay odenecek_tutar  belgeden_kalan
                               WHERE laufd = <fs_filter>-laufd
                                 AND laufi = <fs_filter>-laufi
                                 AND bukrs = <fs_filter>-bukrs
                                 AND lifnr = <fs_filter>-lifnr
                                 AND belnr = <fs_filter>-belnr
                                 AND gjahr = <fs_filter>-gjahr
                                 AND buzei = <fs_filter>-buzei.
    CLEAR: ls_item_tab.

  ENDLOOP.

  FIELD-SYMBOLS: <fs_detail> LIKE LINE OF <fs_sum>-detail_alv.

  LOOP AT gs_scr_1903-sum_alv ASSIGNING <fs_sum>.
    CLEAR: <fs_sum>-odenecek_tutar,
           <fs_sum>-belgeden_kalan.
    LOOP AT <fs_sum>-detail_alv ASSIGNING <fs_detail> .
      <fs_sum>-odenecek_tutar = <fs_sum>-odenecek_tutar + <fs_detail>-odenecek_tutar.
      <fs_sum>-belgeden_kalan = <fs_sum>-belgeden_kalan + <fs_detail>-belgeden_kalan.
*      LOOP AT gs_scr_1903-kismi ASSIGNING FIELD-SYMBOL(<fs_kismi>) WHERE bukrs = <fs_detail>-bukrs
*                                                                     AND rebzg = <fs_detail>-belnr
*                                                                     AND rebzj = <fs_detail>-gjahr.
*
*        <fs_sum>-belgeden_kalan = <fs_sum>-belgeden_kalan - <fs_kismi>-wrbtr.
*      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
  gs_scr_1903-r_alv->refresh( ).
  MODIFY zsog_fi_018_t_04 FROM TABLE gs_scr_1903-item_tab.
  IF sy-subrc EQ 0.
    PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'W' '000' text-025  '' '' ''.
  ENDIF.
  IF lt_tab IS NOT INITIAL.
    PERFORM c14z_messages_show_as_popup TABLES lt_tab.
  ENDIF.

ENDFORM.                    "approver_approve
*&---------------------------------------------------------------------*
*&      Form  F_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM f_handle_toolbar  USING e_object TYPE REF TO cl_alv_event_toolbar_set
                             e_interactive TYPE any
                             sender        TYPE any.
  DATA: ls_toolbar TYPE stb_button.
  IF ( gs_scr_1903-auth-statu = '1' AND p_r1 EQ 'X' )
    or gs_scr_1903-auth-statu = '2'.
    CLEAR ls_toolbar.
    ls_toolbar-butn_type = 3.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function  = 'SATIRSIL'.
    ls_toolbar-icon      = icon_delete_row.
    ls_toolbar-butn_type = 0.
    ls_toolbar-text      = text-041.
    ls_toolbar-quickinfo = text-041.
    MOVE '' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDIF.

ENDFORM.                    " F_HANDLE_TOOLBAR
