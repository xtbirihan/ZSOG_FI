*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_006_CUST_CREATE_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_FILE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_FILE
*&---------------------------------------------------------------------*
FORM get_file CHANGING pv_file TYPE rlgrap-filename.
  DATA: lt_file_table  TYPE filetable,
        ls_file_table  TYPE file_table,
        lv_rc          TYPE i,
        lv_user_action TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Dosya Seçim'
      default_extension       = 'CSV'
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_rc
      user_action             = lv_user_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc NE 0.
    LEAVE LIST-PROCESSING.
  ELSE.
    READ TABLE lt_file_table INTO ls_file_table INDEX 1.
    pv_file = ls_file_table-filename.
  ENDIF.

ENDFORM.                    "get_file
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_PROGRAM
*&---------------------------------------------------------------------*
FORM initialize_program .
  CONCATENATE icon_create_text c_rmf_data_export-sample_file_sscr INTO sscrfields-functxt_01.
  CONCATENATE icon_biw_report  c_rlf_data_export-sample_file_sscr INTO sscrfields-functxt_02.
ENDFORM.                    " INITIALIZE_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM upload_file TABLES pt_datab
                  USING pv_file  TYPE rlgrap-filename
                        pv_type.


  DATA: lt_uptab LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.
  DATA: lv_titulo          TYPE string,
        lv_selected_folder TYPE rlgrap-filename,
        lv_filename        TYPE string.
*  TYPES: BEGIN OF ltt_datab,
*          fields TYPE string,
*        END OF ltt_datab.
*  DATA: lt_datab TYPE TABLE OF ltt_datab.

  lv_filename = pv_file.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename            = lv_filename
      filetype            = 'ASC'
      has_field_separator = 'X'
    CHANGING
      data_tab            = pt_datab[]
    EXCEPTIONS
      OTHERS              = 17.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
*    DELETE lt_datab INDEX 1.
  ENDIF.

  IF pt_datab[] IS INITIAL .
    MESSAGE e398(00) WITH 'Dosya İçerikleri Boş Olamaz'.
  ENDIF.


ENDFORM.                    " UPLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_SAMPLE_CSV
*&---------------------------------------------------------------------*
FORM download_sample_csv .
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
*      PERFORM export_structure_to_excel USING 'ZMM_S_SAYIM_SAMPLE'.
    WHEN 'FC02'.
*      PERFORM export_structure_to_csv.
  ENDCASE.

ENDFORM.                    " DOWNLOAD_SAMPLE_CSV
*&---------------------------------------------------------------------*
*&      Form  CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
FORM create_dynamic_table USING lv_structure_name TYPE dd02l-tabname
                          CHANGING co_tab TYPE REF TO data
                                   co_line TYPE any.

  DATA: lo_struct   TYPE REF TO cl_abap_structdescr,
        lo_new_type TYPE REF TO cl_abap_structdescr,
        lo_new_tab  TYPE REF TO cl_abap_tabledescr,
        lo_data     TYPE REF TO data,
        lt_comp     TYPE cl_abap_structdescr=>component_table,
        lt_tot_comp TYPE cl_abap_structdescr=>component_table,
        lo_dref     TYPE REF TO data.
*  DATA: co_tab      TYPE REF TO data ,
*        co_line     TYPE REF TO data.

* 1. Create Table Line
  CREATE DATA co_line TYPE (lv_structure_name).
*  ASSIGN lo_dref->* TO <f_line> .

* 2. Type Descr.
  lo_struct ?= cl_abap_typedescr=>describe_by_name( lv_structure_name ).
  lt_comp  = lo_struct->get_components( ).
  APPEND LINES OF lt_comp TO lt_tot_comp.

* 3. Create a New Type
  lo_new_type = cl_abap_structdescr=>create( lt_tot_comp ).

* 4. New Table type
  lo_new_tab = cl_abap_tabledescr=>create(
                  p_line_type  = lo_new_type
                  p_table_kind = cl_abap_tabledescr=>tablekind_std
                  p_unique     = abap_false ).

* 5. data to handle the new table type
  CREATE DATA co_tab  TYPE HANDLE lo_new_tab.
*  ASSIGN
*  ASSIGN co_tab->*  TO <tab>.
*  ASSIGN co_line->* TO <line>.
ENDFORM.                    " CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*&      Form  CREATE_TABLES
*&---------------------------------------------------------------------*
FORM create_tables .

  DATA: co_tab      TYPE REF TO data ,
        co_line     TYPE REF TO data.
  FIELD-SYMBOLS: <rmf_tab_d>   TYPE STANDARD TABLE ,
                 <rmf_line_d>  TYPE any,
                 <rmf_tab_h>   TYPE STANDARD TABLE ,
                 <rmf_line_h>  TYPE any,
                 <rlf_tab_d>   TYPE STANDARD TABLE ,
                 <rlf_line_d>  TYPE any,
                 <rlf_line_d2> TYPE any,
                 <fs_debtor_id> TYPE any,
                 <rlf_tab_h>   TYPE STANDARD TABLE ,
                 <rlf_line_h>  TYPE any.

  DATA: ls_h_type TYPE gtt_datab.
  DATA: lv_t      TYPE c,
        lv_comp   TYPE i,
        lv_str    TYPE string,
        lt_split  TYPE TABLE OF string,
        lt_datab  TYPE gtt_datab,
        ls_alv    TYPE zsog_fi_001_s_05,
        ls_rlf    TYPE zsog_fi_001_s_06,
        lt_rlf    TYPE TABLE OF zsog_fi_001_s_06.
  DATA: lv_cond TYPE string,
        lv_cond2 TYPE string.
  FIELD-SYMBOLS: <comp> TYPE any.


  PERFORM create_dynamic_table USING 'ZSOG_FI_001_S_02' CHANGING co_tab
                                                                 co_line.
  ASSIGN co_tab->*  TO <rmf_tab_d>.
  ASSIGN co_line->* TO <rmf_line_d>.

  PERFORM create_dynamic_table USING 'ZSOG_FI_001_S_01' CHANGING co_tab
                                                                 co_line.
  ASSIGN co_tab->*  TO <rmf_tab_h>.
  ASSIGN co_line->* TO <rmf_line_h>.

  PERFORM create_dynamic_table USING 'ZSOG_FI_001_S_04' CHANGING co_tab
                                                                  co_line.
  ASSIGN co_tab->*  TO <rlf_tab_d>.
  ASSIGN co_line->* TO <rlf_line_d>.

  PERFORM create_dynamic_table USING 'ZSOG_FI_001_S_03' CHANGING co_tab
                                                                 co_line.
  ASSIGN co_tab->*  TO <rlf_tab_h>.
  ASSIGN co_line->* TO <rlf_line_h>.

  READ TABLE gt_rmf INTO ls_h_type INDEX 1.
  DELETE gt_rmf INDEX 1.

  DESCRIBE FIELD <rmf_line_h> TYPE lv_t COMPONENTS lv_comp.
  SPLIT ls_h_type-fields AT ',' INTO TABLE lt_split.
  DO lv_comp TIMES.
    READ TABLE lt_split INTO lv_str INDEX sy-index.
    ASSIGN COMPONENT sy-index OF STRUCTURE <rmf_line_h> TO <comp>.
    <comp> = lv_str.
    CLEAR lv_str.
  ENDDO.
  CLEAR: lv_t, lv_comp, lv_str, lt_split, ls_h_type .

  READ TABLE gt_rlf INTO ls_h_type INDEX 1.
  DELETE gt_rlf INDEX 1.

  DESCRIBE FIELD <rlf_line_h> TYPE lv_t COMPONENTS lv_comp.
  SPLIT ls_h_type-fields AT ',' INTO TABLE lt_split.
  DO lv_comp TIMES.
    READ TABLE lt_split INTO lv_str INDEX sy-index.
    ASSIGN COMPONENT sy-index OF STRUCTURE <rlf_line_h> TO <comp>.
    <comp> = lv_str.
    CLEAR lv_str.
  ENDDO.
  CLEAR: lv_t, lv_comp, lv_str, lt_split .

  DESCRIBE FIELD <rmf_line_d> TYPE lv_t COMPONENTS lv_comp.
  LOOP AT gt_rmf INTO lt_datab.
    lv_str = lt_datab-fields.
    CLEAR: lt_split.
    SPLIT lv_str AT ',' INTO TABLE lt_split.
    DO lv_comp TIMES.
      READ TABLE lt_split INTO lv_str INDEX sy-index.
      ASSIGN COMPONENT sy-index OF STRUCTURE <rmf_line_d> TO <comp>.
      <comp> = lv_str.
      CLEAR lv_str.
    ENDDO.
    INSERT <rmf_line_d> INTO TABLE <rmf_tab_d>.
  ENDLOOP.
  CLEAR: lv_t, lv_comp, lv_str, lt_split .

  DESCRIBE FIELD <rlf_line_d> TYPE lv_t COMPONENTS lv_comp.
  LOOP AT gt_rlf INTO lt_datab.
    lv_str = lt_datab-fields.
    CLEAR: lt_split.
    SPLIT lv_str AT ',' INTO TABLE lt_split.
    DO lv_comp TIMES.
      READ TABLE lt_split INTO lv_str INDEX sy-index.
      ASSIGN COMPONENT sy-index OF STRUCTURE <rlf_line_d> TO <comp>.
      <comp> = lv_str.
      CLEAR lv_str.
    ENDDO.
    INSERT <rlf_line_d> INTO TABLE <rlf_tab_d>.
  ENDLOOP.


  LOOP AT <rmf_tab_d> ASSIGNING <rmf_line_d>.
    MOVE-CORRESPONDING <rmf_line_h> TO ls_alv.
    MOVE-CORRESPONDING <rmf_line_d> TO ls_alv.
*    CONCATENATE 'RETAILER_DEBTOR_ID'  'EQ' ls_alv-retailer_debtor_id INTO lv_cond SEPARATED BY space.
    CONCATENATE 'RETAIL_LOCATION_ID'  'EQ' ls_alv-retailer_debtor_id INTO lv_cond SEPARATED BY space.
*    READ TABLE <rlf_tab_d> ASSIGNING <rlf_line_d2> WITH KEY( lv_cond ).
    LOOP AT <rlf_tab_d> ASSIGNING <rlf_line_d2> WHERE (lv_cond).
      ASSIGN COMPONENT 'RETAILER_DEBTOR_ID' OF STRUCTURE <rlf_line_d2> TO <fs_debtor_id>.
      IF <fs_debtor_id> IS NOT INITIAL.
        CONCATENATE lv_cond 'AND' 'RETAILER_DEBTOR_ID' 'EQ' <fs_debtor_id> INTO lv_cond2 SEPARATED BY space.
        LOOP AT <rlf_tab_d> ASSIGNING <rlf_line_d> WHERE (lv_cond2).
          MOVE-CORRESPONDING <rlf_line_h> TO ls_rlf.
          MOVE-CORRESPONDING <rlf_line_d> TO ls_rlf.
          PERFORM customer_conv CHANGING ls_rlf-retailer_debtor_id.
          PERFORM customer_conv CHANGING ls_rlf-retail_location_id.

          APPEND ls_rlf TO lt_rlf.
          CLEAR: ls_rlf.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
    IF sy-subrc EQ 0.
      ls_alv-rlf_icon = icon_translation_show.
    ELSE.
      ls_alv-rlf_icon = icon_system_stop_recording.
    ENDIF.
    PERFORM customer_conv CHANGING ls_alv-retailer_debtor_id.
    ls_alv-rlf = lt_rlf.
    APPEND ls_alv TO gs_scr-1903-alv.
    CLEAR: ls_alv, lv_cond, lt_rlf, lv_cond2.
  ENDLOOP.

  PERFORM modify_data.
ENDFORM.                    " CREATE_TABLES
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_ALV
*&---------------------------------------------------------------------*
FORM initialize_alv .
  DATA message   TYPE REF TO cx_salv_msg.

  TRY.
      cl_salv_table=>factory(
      IMPORTING
        r_salv_table = gs_scr-1903-r_alv
      CHANGING
        t_table      = gs_scr-1903-alv ).

      gs_scr-1903-r_columns = gs_scr-1903-r_alv->get_columns( ).

      PERFORM enable_layout_settings.
      PERFORM optimize_column_width.
      PERFORM hide_client_column.
      PERFORM set_icon.
      PERFORM set_column_names.
      PERFORM set_toolbar.
      PERFORM display_settings TABLES gs_scr-1903-alv.
      PERFORM set_hotspot_click.

      " ...
      " PERFORM setting_n.
    CATCH cx_salv_msg INTO message.
      " error handling
  ENDTRY.
ENDFORM.                    "initialize_alv
*&---------------------------------------------------------------------*
*&      Form  enable_layout_settings
*&---------------------------------------------------------------------*
FORM enable_layout_settings.
*&---------------------------------------------------------------------*
  DATA layout_settings TYPE REF TO cl_salv_layout.
  DATA layout_key      TYPE salv_s_layout_key.


  layout_settings = gs_scr-1903-r_alv->get_layout( ).
  layout_key-report = sy-repid.
  layout_settings->set_key( layout_key ).
  layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).

  gs_scr-1903-r_selections = gs_scr-1903-r_alv->get_selections( ).
  gs_scr-1903-r_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

ENDFORM.                    "ENABLE_LAYOUT_SETTINGS

*&---------------------------------------------------------------------*
FORM optimize_column_width.
*&---------------------------------------------------------------------*
  gs_scr-1903-r_columns->set_optimize( ).
ENDFORM.                    "OPTIMIZE_COLUMN_WIDTH

*&---------------------------------------------------------------------*
FORM hide_client_column.
*&---------------------------------------------------------------------*
  DATA not_found TYPE REF TO cx_salv_not_found.

  TRY.
      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'T_MESSAGES' ).
      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO not_found.
      " error handling
  ENDTRY.

  TRY.
      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'RLF' ).
      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO not_found.
      " error handling
  ENDTRY.

  TRY.
      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'H_RECORD_TYPE' ).
      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO not_found.
      " error handling
  ENDTRY.

  TRY.
      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'H_VERSION_NO' ).
      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO not_found.
      " error handling
  ENDTRY.

  TRY.
      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'H_COMP_CODE' ).
      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO not_found.
      " error handling
  ENDTRY.

  TRY.
      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'H_CREATION_DATE_TIME' ).
      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO not_found.
      " error handling
  ENDTRY.

  TRY.
      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'H_FILE_TYPE' ).
      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO not_found.
      " error handling
  ENDTRY.

  TRY.
      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'H_NUMBER_OF_RECORDS' ).
      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO not_found.
      " error handling
  ENDTRY.

ENDFORM.                    " HIDE_CLIENT_COLUMN
*&---------------------------------------------------------------------*
*&      Form  set_icon
*&---------------------------------------------------------------------*
FORM set_icon.
  DATA: lr_columns TYPE REF TO cl_salv_columns_table,
       lr_column  TYPE REF TO cl_salv_column_table.
*
  lr_columns = gs_scr-1903-r_alv->get_columns( ).
  lr_column ?= lr_columns->get_column( 'CREATED' ).
  lr_column->set_icon( if_salv_c_bool_sap=>true ).
*
  lr_column ?= lr_columns->get_column( 'RLF_ICON' ).
  lr_column->set_icon( if_salv_c_bool_sap=>true ).

  lr_column ?= lr_columns->get_column( 'RLF_ICON' ).
  lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

  lr_column ?= lr_columns->get_column( 'STATUS' ).
  lr_column->set_icon( if_salv_c_bool_sap=>true ).

  lr_column ?= lr_columns->get_column( 'STATUS' ).
  lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

ENDFORM.                    "set_icon
*&---------------------------------------------------------------------*
FORM set_column_names.
*&---------------------------------------------------------------------*
*  DATA not_found TYPE REF TO cx_salv_not_found.
*
*  TRY.
*      gr_column = gr_columns->get_column( 'WAVWR' ).
*      gr_column->set_short_text( 'Maliyet' ).
*      gr_column->set_medium_text( 'Maliyet' ).
*      gr_column->set_long_text( 'Maliyet' ).
*    CATCH cx_salv_not_found INTO not_found.
*      " error handling
*  ENDTRY.
ENDFORM.                    " SET_DEPARTURE_COUNTRY_COLUMN

*&---------------------------------------------------------------------*
FORM set_toolbar.
*&---------------------------------------------------------------------*
  DATA functions TYPE REF TO cl_salv_functions_list.
  functions = gs_scr-1903-r_alv->get_functions( ).
  functions->set_all( ).

  gs_scr-1903-r_alv->set_screen_status(
    pfstatus      = 'STANDARD'
    report        = sy-repid
    set_functions = gs_scr-1903-r_alv->c_functions_all ).
ENDFORM.                    " SET_TOOLBAR
*&---------------------------------------------------------------------*
FORM display_settings TABLES lt_table.
*&---------------------------------------------------------------------*
  DATA display_settings TYPE REF TO cl_salv_display_settings.
  DATA: lv_tanim TYPE text70.
  DATA: lv_line TYPE i.
  lv_line  = lines( lt_table[] ).
  lv_tanim = |Müşteri Yaratma | && |--> | && |{ lv_line }| && | Kayıt Bulundu|.

  display_settings = gs_scr-1903-r_alv->get_display_settings( ).
  display_settings->set_striped_pattern( if_salv_c_bool_sap=>true ).
  display_settings->set_list_header( lv_tanim ).
ENDFORM.                    "display_settings
*&---------------------------------------------------------------------*
*&      Form  SET_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM set_hotspot_click .
*-- events
  gs_scr-1903-r_events = gs_scr-1903-r_alv->get_event( ).
  CREATE OBJECT event_handler.
  SET HANDLER event_handler->on_link_click   FOR gs_scr-1903-r_events.
  SET HANDLER event_handler->on_user_command FOR gs_scr-1903-r_events.
ENDFORM.                    "set_hotspot_click
*&---------------------------------------------------------------------*
FORM display_alv.
*&---------------------------------------------------------------------*
  gs_scr-1903-r_alv->display( ).
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  CUSTOMER_CONV
*&---------------------------------------------------------------------*
FORM customer_conv  CHANGING pv_id.
  DATA pv_input  TYPE char6.
  DATA pv_output TYPE char8.

  pv_input = pv_id.
  CALL FUNCTION 'ZSOG_SG_CUST_CONV'
    EXPORTING
      iv_kunnr = pv_input
    IMPORTING
      ev_kunnr = pv_output.
  pv_id = pv_output .

ENDFORM.                    " CUSTOMER_CONV
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  DATA: lv_tabix TYPE sy-tabix.
  TYPES: BEGIN OF ltt_kunnr,
         kunnr TYPE kna1-kunnr,
         END OF ltt_kunnr.
  DATA: lt_kunnr TYPE TABLE OF ltt_kunnr,
        ls_kunnr TYPE ltt_kunnr,
        ls_alv   TYPE zsog_fi_001_s_05.
  SORT gs_scr-1903-alv BY retailer_debtor_id.
  IF gs_scr-1903-alv IS NOT INITIAL.
    SELECT kunnr FROM kna1 INTO TABLE lt_kunnr
                           FOR ALL ENTRIES IN gs_scr-1903-alv
                           WHERE kunnr = gs_scr-1903-alv-retailer_debtor_id.
  ENDIF.
  LOOP AT lt_kunnr INTO ls_kunnr.
    READ TABLE gs_scr-1903-alv INTO ls_alv WITH KEY retailer_debtor_id = ls_kunnr-kunnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      lv_tabix = sy-tabix.
      ls_alv-process = 'U'.
      ls_alv-created     = icon_okay.
      MODIFY gs_scr-1903-alv FROM ls_alv INDEX lv_tabix TRANSPORTING process created.
    ENDIF.
    CLEAR: ls_alv, lv_tabix.
  ENDLOOP.

  ls_alv-process = 'I'.
  ls_alv-created = icon_create_position.

  MODIFY gs_scr-1903-alv FROM ls_alv TRANSPORTING  process created WHERE process IS INITIAL.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  ON_LINK_CLICK
*&---------------------------------------------------------------------*
FORM on_link_click  USING    p_row
                             p_column.
  DATA: lt_bapiret2 TYPE TABLE OF bapiret2.
  DATA: ls_alv      TYPE zsog_fi_001_s_05.
  DATA: lo_alv TYPE REF TO cl_salv_table.
  DATA: lt_rlf TYPE TABLE OF zsog_fi_001_s_06.

  CASE p_column.
    WHEN 'STATUS'.
      READ TABLE gs_scr-1903-alv INTO ls_alv INDEX p_row.
      IF sy-subrc EQ 0.
        lt_bapiret2 = ls_alv-t_messages.
        IF lt_bapiret2 IS NOT INITIAL.
          TRY.
              cl_salv_table=>factory(
                IMPORTING
                  r_salv_table = lo_alv
                CHANGING
                  t_table      = lt_bapiret2 ).

            CATCH cx_salv_msg.
          ENDTRY.

          DATA: lr_functions TYPE REF TO cl_salv_functions_list.

          lr_functions = lo_alv->get_functions( ).
          lr_functions->set_all( 'X' ).

          IF lo_alv IS BOUND.
            lo_alv->set_screen_popup(
              start_column = 5
              end_column   = 115
              start_line   = 5
              end_line     = 20 ).
            lo_alv->display( ).

          ENDIF.
        ENDIF.
      ENDIF.
    WHEN 'RLF_ICON'.
      READ TABLE gs_scr-1903-alv INTO ls_alv INDEX p_row.
      IF sy-subrc EQ 0.
        lt_rlf = ls_alv-rlf.
        PERFORM initialize_alv2 TABLES lt_rlf.
        PERFORM display_alv.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "on_link_click
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_ALV
*&---------------------------------------------------------------------*
FORM initialize_alv2 TABLES lt_rlf STRUCTURE zsog_fi_001_s_06.
  DATA message   TYPE REF TO cx_salv_msg.

  TRY.
      cl_salv_table=>factory(
      IMPORTING
        r_salv_table = gs_scr-1903-r_alv
      CHANGING
        t_table      = lt_rlf[] ).

      gs_scr-1903-r_columns = gs_scr-1903-r_alv->get_columns( ).

      PERFORM enable_layout_settings.
      PERFORM optimize_column_width.
      PERFORM hide_client_column.
*      PERFORM set_icon.
      PERFORM set_column_names.
      PERFORM set_toolbar2.
      PERFORM display_settings TABLES lt_rlf[].
      PERFORM set_hotspot_click.

      " ...
      " PERFORM setting_n.
    CATCH cx_salv_msg INTO message.
      " error handling
  ENDTRY.
ENDFORM.                    "initialize_alv
*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
FORM handle_user_command USING i_ucomm TYPE salv_de_function.
  CASE i_ucomm.
    WHEN '&RLF'.
      PERFORM display_all_rlf.
*      CALL SCREEN 1903.
    WHEN '&DEGISTIR'.
*      PERFORM kayit_degistir.
    WHEN '&CREATE'.
      PERFORM create_customer.
  ENDCASE.
ENDFORM.                    "handle_user_command
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALL_RLF
*&---------------------------------------------------------------------*
FORM display_all_rlf .
  DATA: ls_alv      TYPE zsog_fi_001_s_05.
  DATA: lt_rlf TYPE TABLE OF zsog_fi_001_s_06.
  LOOP AT gs_scr-1903-alv INTO ls_alv.
    IF ls_alv-rlf IS NOT INITIAL.
      APPEND LINES OF ls_alv-rlf TO lt_rlf.
    ENDIF.
  ENDLOOP.
  IF lt_rlf IS NOT INITIAL.
    PERFORM initialize_alv2 TABLES lt_rlf.
    PERFORM display_alv.
  ENDIF.
ENDFORM.                    " DISPLAY_ALL_RLF
*&---------------------------------------------------------------------*
FORM set_toolbar2.
*&---------------------------------------------------------------------*
  DATA functions TYPE REF TO cl_salv_functions_list.
  functions = gs_scr-1903-r_alv->get_functions( ).
  functions->set_all( ).

  gs_scr-1903-r_alv->set_screen_status(
    pfstatus      = 'STANDARD2'
    report        = sy-repid
    set_functions = gs_scr-1903-r_alv->c_functions_all ).
ENDFORM.                    "set_toolbar2
*&---------------------------------------------------------------------*
*&      Form  CREATE_CUSTOMER
*&---------------------------------------------------------------------*
FORM create_customer .
  DATA: lt_rows  TYPE salv_t_row,
        ls_rows  TYPE int4.

  lt_rows = gs_scr-1903-r_selections->get_selected_rows( ).
  IF lt_rows IS INITIAL.
    MESSAGE i000(zsg) WITH text-002.
    RETURN.
  ENDIF.

  DATA:  ls_data            TYPE cmds_ei_vmd_central_data,
         ls_datax           TYPE cmds_ei_vmd_central_data_xflag,
         ls_address         TYPE bapiad1vl,
         ls_addressx        TYPE bapiad1vlx,
         ls_tax_ind         TYPE cmds_ei_cmd_tax_ind,
         ls_company_code    TYPE cmds_ei_cmd_company,
         ls_customers       TYPE cmds_ei_main,
         lt_email           TYPE cvis_ei_smtp_t,               """"EMAIL CONTACT ---
         ls_contact         TYPE cvis_ei_smtp,
         lt_phone           TYPE cvis_ei_phone_t,                """PHONE  AND FAX
         ls_ph_contact      TYPE cvis_ei_phone,
         lt_fax             TYPE cvis_ei_fax_t,
         ls_fax_contact     TYPE cvis_ei_fax,
         ls_functions_t     TYPE cmds_ei_functions_t,
         ls_sales_data_st   TYPE cmds_ei_sales,
         lt_sales           TYPE cmds_ei_sales_t,
         lc_update          TYPE c VALUE 'M',
         lc_insert          TYPE c VALUE 'I',
         lv_tabix           TYPE sy-tabix ,
         lt_sales_dist      TYPE TABLE OF zsog_fi_001_t_03.

  DATA : ls_alv             TYPE zsog_fi_001_s_05.

  SELECT * FROM zsog_fi_001_t_03 INTO TABLE lt_sales_dist.
  SORT lt_sales_dist BY districts_number.

  LOOP AT lt_rows INTO ls_rows.
    READ TABLE gs_scr-1903-alv INTO ls_alv INDEX ls_rows.
    lv_tabix = sy-tabix.
    IF ls_alv-process EQ 'I'.
      lc_insert = 'I'.
    ELSEIF ls_alv-process EQ 'U'.
      lc_insert = 'M'.
    ENDIF.

    PERFORM fill_tax_data      USING ls_alv lc_insert CHANGING ls_tax_ind.
    PERFORM fill_adres_comcode TABLES lt_sales_dist USING ls_alv lc_insert CHANGING ls_company_code ls_address ls_addressx.
    PERFORM fill_central_data  USING ls_alv lc_insert CHANGING ls_data ls_datax.
    PERFORM fill_tel_fax_mail  USING ls_alv lc_insert CHANGING lt_phone lt_fax lt_email.
    PERFORM fill_sales_data    USING ls_alv lc_insert CHANGING ls_sales_data_st ls_functions_t .
    PERFORM fill_customer      USING ls_alv lc_insert ls_company_code ls_address ls_addressx  ls_data
                                     ls_datax ls_tax_ind lt_phone lt_fax lt_email
                               CHANGING ls_customers.

    PERFORM call_cmd_ei_api USING ls_customers lv_tabix CHANGING ls_alv.

    CLEAR: ls_alv, ls_tax_ind, ls_company_code, ls_address, ls_addressx,
           ls_data, ls_datax, lt_phone, lt_fax, lt_email, ls_sales_data_st, ls_functions_t,
           ls_customers.

  ENDLOOP.
  gs_scr-1903-r_alv->refresh( ).
ENDFORM.                    " CREATE_CUSTOMER
*&---------------------------------------------------------------------*
*&      Form  FILL_TAX_DATA
*&---------------------------------------------------------------------*
FORM fill_tax_data  USING    ps_alv     TYPE zsog_fi_001_s_05
                             lc_insert  TYPE char1
                    CHANGING ps_tax_ind TYPE cmds_ei_cmd_tax_ind.

  DATA: ls_tax_ind_st TYPE cmds_ei_tax_ind.

  ls_tax_ind_st-task = lc_insert.
  ls_tax_ind_st-data_key-aland = 'TR'.
  ls_tax_ind_st-data_key-tatyp = 'MWST'.
  ls_tax_ind_st-data-taxkd  = '1'.
  ls_tax_ind_st-datax-taxkd = 'X'.
  APPEND ls_tax_ind_st TO ps_tax_ind-tax_ind.

  ls_tax_ind_st-data_key-aland = 'TR'.
  ls_tax_ind_st-data_key-tatyp = 'ZHRD'.
  ls_tax_ind_st-data-taxkd  = '0'.
  ls_tax_ind_st-datax-taxkd = 'X'.
  APPEND ls_tax_ind_st TO ps_tax_ind-tax_ind.

  ls_tax_ind_st-data_key-aland = 'TR'.
  ls_tax_ind_st-data_key-tatyp = 'ZWIA'.
  ls_tax_ind_st-data-taxkd  = '1'.
  ls_tax_ind_st-datax-taxkd = 'X'.
  APPEND ls_tax_ind_st TO ps_tax_ind-tax_ind.

  ls_tax_ind_st-data_key-aland = 'TR'.
  ls_tax_ind_st-data_key-tatyp = 'ZWOT'.
  ls_tax_ind_st-data-taxkd  = '1'.
  ls_tax_ind_st-datax-taxkd = 'X'.
  APPEND ls_tax_ind_st TO ps_tax_ind-tax_ind.

  ls_tax_ind_st-data_key-aland = 'TR'.
  ls_tax_ind_st-data_key-tatyp = 'ZOIV'.
  ls_tax_ind_st-data-taxkd     = '0'.
  ls_tax_ind_st-datax-taxkd    = 'X'.
  APPEND ls_tax_ind_st TO ps_tax_ind-tax_ind.
ENDFORM.                    " FILL_TAX_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_ADRES_COMCODE
*&---------------------------------------------------------------------*
FORM fill_adres_comcode TABLES    pt_sales_dist   STRUCTURE zsog_fi_001_t_03
                         USING    ps_alv          TYPE zsog_fi_001_s_05
                                  pc_insert       TYPE char1
                         CHANGING ps_company_code TYPE cmds_ei_cmd_company
                                  ps_address      TYPE bapiad1vl
                                  ps_addressx     TYPE bapiad1vlx.

  DATA: ls_company_code_st TYPE cmds_ei_company.
  DATA: lv_char5           TYPE char5,
        ls_sales_dist      TYPE zsog_fi_001_t_03,
        ls_rlf             TYPE zsog_fi_001_s_06,
        lv_sale_dist       TYPE zsog_fi_001_s_06.

  IF ps_alv-rlf  IS NOT INITIAL.
    READ TABLE ps_alv-rlf INTO ls_rlf INDEX 1.
  ENDIF.
  IF ls_rlf-type EQ '1'.
    ps_address-title          = '0003'.
  ELSEIF ls_rlf-type EQ '2'.
    ps_address-title          = '0002'.
  ELSE.
    ps_address-title          = '0003'.
  ENDIF.

  ps_address-name           = ps_alv-business_name.
  ps_address-name_2         = ps_alv-owner_name.
  ps_address-city           = ps_alv-city.

  IF ps_alv-rlf  IS NOT INITIAL.
    READ TABLE ps_alv-rlf INTO ls_rlf INDEX 1.
  ENDIF.
  READ TABLE pt_sales_dist INTO ls_sales_dist WITH KEY districts_number = ls_rlf-sales_district BINARY SEARCH.
*  ps_address-district       = ps_alv-ort02.
  ps_address-district       = ls_sales_dist-districts.
  ps_address-country        = 'TR'.
  ps_address-langu          = 'TR'.
*  ps_address-region         = ps_alv-regio.         """COMNT
  ps_address-sort1          = ps_alv-business_name.
  ps_address-street         = ps_alv-street.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ps_alv-zip_code
    IMPORTING
      output = lv_char5.
  ps_address-po_box         = lv_char5.
  ps_address-postl_cod1     = lv_char5.
*
  ps_addressx-title          = 'X'.
  ps_addressx-name           = 'X'.
  ps_addressx-name_2         = 'X'.
  ps_addressx-city           = 'X'.
  ps_addressx-district       = 'X'.
  ps_addressx-country        = 'X'.
  ps_addressx-langu          = 'X'.
*  ps_addressx-region         = 'X'.
  ps_addressx-sort1          = 'X'.
  ps_addressx-street         = 'X'.
  ps_addressx-po_box         = 'X'.
  ps_addressx-postl_cod1     = 'X'.
*
  ls_company_code_st-task           = pc_insert.
  ls_company_code_st-data-akont     = '1200200001'. "aynı mutabakat hesabına mı gidecek
  ls_company_code_st-data-zterm     = 'Z000'.
  ls_company_code_st-datax-akont    = 'X'.
  ls_company_code_st-datax-zterm    = 'X'.
  ls_company_code_st-data_key-bukrs = '2425'.
*
  APPEND ls_company_code_st TO ps_company_code-company.

ENDFORM.                    " FILL_ADRES_COMCODE
*&---------------------------------------------------------------------*
*&      Form  FILL_CENTRAL_DATA
*&---------------------------------------------------------------------*
FORM fill_central_data  USING    ps_alv     TYPE zsog_fi_001_s_05
                                 pc_insert  TYPE char1
                        CHANGING ps_data    TYPE cmds_ei_vmd_central_data
                                 ps_datax   TYPE cmds_ei_vmd_central_data_xflag.

  DATA: ls_rlf TYPE zsog_fi_001_s_06.
  IF ps_alv-rlf  IS NOT INITIAL.
    READ TABLE ps_alv-rlf INTO ls_rlf INDEX 1.
  ENDIF.
  ps_data-ktokd = 'SG01'.
  ps_data-brsch = 'SG01'.
*  IF ps_alv-turkish_national_id IS NOT INITIAL.
*    ps_data-stcd1 = ps_alv-turkish_national_id.
*  ELSE.
*  ENDIF.
*  ps_data-stcd1 = ps_alv-house_number.
  ps_data-stcd1 = ls_rlf-tax_house.
  ps_data-stcd2 = ps_alv-tax_number.
  ps_data-stcd3 = 'X'.

  ps_data-katr2 = ls_rlf-action_code.
  ps_data-katr3 = ls_rlf-retailer_sub_object.
  ps_data-katr4 = ls_rlf-sales_territory_region.
  ps_data-katr5 = ls_rlf-commission_package_id.
  IF ls_rlf-is_lucky_retailer EQ '1'.
    ps_data-katr6 = 'Y'.
  ELSE.
    ps_data-katr6 = 'N'.
  ENDIF.

*
  ps_datax-ktokd = 'X'.
  ps_datax-brsch = 'X'.
  ps_datax-stcd1 = 'X'.
  ps_datax-stcd2 = 'X'.
  ps_datax-stcd3 = 'X'.
  ps_datax-katr2 = 'X'.
  ps_datax-katr3 = 'X'.
  ps_datax-katr4 = 'X'.
  ps_datax-katr5 = 'X'.
  ps_datax-katr6 = 'X'.

ENDFORM.                    " FILL_CENTRAL_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_TEL_FAX_MAIL
*&---------------------------------------------------------------------*
FORM fill_tel_fax_mail  USING    ps_alv     TYPE zsog_fi_001_s_05
                                 pc_insert  TYPE char1
                        CHANGING pt_phone   TYPE cvis_ei_phone_t
                                 pt_fax     TYPE cvis_ei_fax_t
                                 pt_email   TYPE cvis_ei_smtp_t.

  DATA: ls_phone    TYPE cvis_ei_phone_str,
        ls_fax      TYPE cvis_ei_fax_str,
        ls_email    TYPE cvis_ei_smtp_str.

  ls_phone-contact-data-countryiso = 'TR'.
  ls_phone-contact-data-telephone  = ps_alv-phone1.
  APPEND ls_phone TO pt_phone.


  ls_phone-contact-data-countryiso = 'TR'.
  ls_phone-contact-data-telephone  = ps_alv-mobile_phone.
  APPEND ls_phone TO pt_phone.


  IF ps_alv-fax IS NOT INITIAL.
    ls_fax-contact-data-countryiso   = 'TR'.
    ls_fax-contact-data-fax          = ps_alv-fax.
    APPEND ls_fax TO pt_fax.
  ENDIF.

  IF ps_alv-email IS NOT INITIAL.
    ls_email-contact-data-e_mail     = ps_alv-email.
    APPEND ls_email TO pt_email.
  ENDIF.

ENDFORM.                    " FILL_TEL_FAX_MAIL
*&---------------------------------------------------------------------*
*&      Form  FILL_SALES_DATA
*&---------------------------------------------------------------------*
FORM fill_sales_data  USING    ps_alv           TYPE zsog_fi_001_s_05
                               pc_insert        TYPE char1
                      CHANGING ps_sales_data_st TYPE cmds_ei_sales
                               ps_functions_t   TYPE cmds_ei_functions_t.

  DATA: ls_sales_data      TYPE cmds_ei_sales_data,
        ls_functions_st    TYPE cmds_ei_functions.
*
  ls_sales_data-zterm  = 'Z000'.
  ls_sales_data-kalks = '1'.
  ls_sales_data-versg = '1'.
  ls_sales_data-ktgrd = 'D2'.
  ls_sales_data-bzirk = 'GENEL'.
  ls_sales_data-waers = 'TRY'.
  ls_sales_data-lprio = '02'.
  ls_sales_data-inco1 = 'EXW'.
  ls_sales_data-konda = 'DG'.
  ls_sales_data-inco2 = '.'.
  ps_sales_data_st-data = ls_sales_data.

  ls_functions_st-task             = pc_insert.
  ls_functions_st-data_key-parvw   = 'AG'.
  ls_functions_st-data-defpa       = 'X'.
  ls_functions_st-data-partner     = ps_alv-retailer_debtor_id."'INTERNAL'.
  APPEND ls_functions_st TO  ps_functions_t.
  CLEAR: ls_functions_st.
*
  ls_functions_st-task = pc_insert.
  ls_functions_st-data_key-parvw  = 'WE'.
  ls_functions_st-data-defpa      = 'X'.
  ls_functions_st-data-partner    = ps_alv-retailer_debtor_id."'INTERNAL'.
  APPEND ls_functions_st TO  ps_functions_t.
  CLEAR: ls_functions_st.

  ls_functions_st-task             = pc_insert.
  ls_functions_st-data_key-parvw   = 'RG'.
  ls_functions_st-data-defpa       = 'X'.
  ls_functions_st-data-partner     = ps_alv-retailer_debtor_id."'INTERNAL'.
  APPEND ls_functions_st TO ps_functions_t.
  CLEAR: ls_functions_st.

  ls_functions_st-task = pc_insert.
  ls_functions_st-data_key-parvw  = 'RE'.
  ls_functions_st-data-defpa      = 'X'.
  ls_functions_st-data-partner    = ps_alv-retailer_debtor_id."'INTERNAL'.
  APPEND ls_functions_st TO ps_functions_t.
  CLEAR: ls_functions_st.

  ps_sales_data_st-functions-current_state = 'X'.
  ps_sales_data_st-functions-functions = ps_functions_t.


ENDFORM.                    " FILL_SALES_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_CUSTOMER
*&---------------------------------------------------------------------*
FORM fill_customer  USING    ps_alv           TYPE zsog_fi_001_s_05
                             pc_insert        TYPE char1
                             ps_company_code  TYPE cmds_ei_cmd_company
                             ps_address       TYPE bapiad1vl
                             ps_addressx      TYPE bapiad1vlx
                             ps_data          TYPE cmds_ei_vmd_central_data
                             ps_datax         TYPE cmds_ei_vmd_central_data_xflag
                             ps_tax_ind       TYPE cmds_ei_cmd_tax_ind
                             pt_phone         TYPE cvis_ei_phone_t
                             pt_fax           TYPE cvis_ei_fax_t
                             pt_email         TYPE cvis_ei_smtp_t
                    CHANGING ps_customers     TYPE cmds_ei_main.

  DATA:   ls_customer TYPE cmds_ei_extern.

  ls_customer-central_data-address-postal-data               = ps_address.
  ls_customer-central_data-address-postal-datax              = ps_addressx.
  ls_customer-central_data-address-task                      = pc_insert.
  ls_customer-central_data-central-data                      = ps_data.
  ls_customer-central_data-central-datax                     = ps_datax.
  ls_customer-central_data-tax_ind                           = ps_tax_ind.
  ls_customer-central_data-address-communication-phone-phone = pt_phone[].
  ls_customer-central_data-address-communication-fax-fax     = pt_fax[].
  ls_customer-central_data-address-communication-smtp-smtp   = pt_email[].
  ls_customer-sales_data-current_state                       = 'X'.
  ls_customer-header-object_instance-kunnr                   = ps_alv-retailer_debtor_id.
  ls_customer-central_data-central-data-ktokd                = 'SG01'.
  ls_customer-header-object_instance-kunnr                   = ps_alv-retailer_debtor_id.
  ls_customer-header-object_task                             = pc_insert.
  ls_customer-company_data                                   = ps_company_code.
  APPEND ls_customer TO ps_customers-customers.


ENDFORM.                    " FILL_CUSTOMER
*&---------------------------------------------------------------------*
*&      Form  CALL_CMD_EI_API
*&---------------------------------------------------------------------*
FORM call_cmd_ei_api  USING     ps_customers TYPE cmds_ei_main
                                pv_tabix     TYPE sy-tabix
                      CHANGING  ps_alv       TYPE zsog_fi_001_s_05.
*
  DATA:    ls_correct         TYPE cmds_ei_main,
           ls_defective       TYPE cmds_ei_main,
           ls_mes_correct     TYPE cvis_message,
           ls_mes_error       TYPE cvis_message,
           ls_bapiret2        TYPE bapiret2,
           lt_bapiret2        TYPE TABLE OF bapiret2.

  DATA: ls_rlf TYPE zsog_fi_001_t_01 ,
        ls_rmf TYPE zsog_fi_001_t_02 ,
        lt_rlf TYPE TABLE OF zsog_fi_001_t_01,
        lt_rmf TYPE TABLE OF zsog_fi_001_t_02 ,
        ls_alv_rlf  TYPE zsog_fi_001_s_06.

  CALL METHOD cmd_ei_api=>maintain_bapi
    EXPORTING
      iv_test_run              = ''
      iv_collect_messages      = 'X'
      is_master_data           = ps_customers
    IMPORTING
      es_master_data_correct   = ls_correct
      es_message_correct       = ls_mes_correct
      es_master_data_defective = ls_defective
      es_message_defective     = ls_mes_error.

  IF ls_mes_error-is_error IS INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    ls_bapiret2-type = 'S'.
    ls_bapiret2-id   = 'ZSG'.
    ls_bapiret2-number = '000'.
    ls_bapiret2-message    = ps_alv-retailer_debtor_id && | Müşterisi başarıyla yaratıldı/güncellendi!|.
    ls_bapiret2-message_v1 = ps_alv-retailer_debtor_id && | Müşterisi başarıyla yaratıldı/güncellendi!|.
    ls_bapiret2-message_v2 = ps_alv-retailer_debtor_id && | Müşterisi başarıyla yaratıldı/güncellendi!|.
    ls_bapiret2-message_v3 = ps_alv-retailer_debtor_id && | Müşterisi başarıyla yaratıldı/güncellendi!|.
    APPEND ls_bapiret2 TO lt_bapiret2.

    ps_alv-status = icon_action_success.
    ps_alv-t_messages = lt_bapiret2.
    MODIFY gs_scr-1903-alv FROM ps_alv INDEX pv_tabix TRANSPORTING status t_messages.

    MOVE-CORRESPONDING ps_alv TO ls_rmf.
    ls_rmf-mandt = sy-mandt.
    APPEND ls_rmf TO lt_rmf.
    LOOP AT ps_alv-rlf INTO ls_alv_rlf.
      MOVE-CORRESPONDING ls_alv_rlf TO ls_rlf.
      APPEND ls_rlf TO lt_rlf.
      CLEAR: ls_rlf.
    ENDLOOP.
    CLEAR: ls_rmf.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ps_alv-status = icon_action_fault.
    ps_alv-t_messages = ls_mes_error-messages.
    MODIFY gs_scr-1903-alv FROM ps_alv INDEX pv_tabix TRANSPORTING status t_messages.
*    APPEND LINES OF ls_mes_error-MESSAGES TO E_MESSAGES.
  ENDIF.
  IF lt_rlf IS NOT INITIAL.
    ls_rlf-mandt = sy-mandt.
    MODIFY lt_rlf FROM ls_rlf TRANSPORTING mandt WHERE mandt IS INITIAL.
    MODIFY zsog_fi_001_t_01  FROM TABLE lt_rlf.
    COMMIT WORK AND WAIT .
  ENDIF.
  IF  lt_rmf IS NOT INITIAL.
    MODIFY zsog_fi_001_t_02  FROM TABLE lt_rmf.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.                    " CALL_CMD_EI_API
