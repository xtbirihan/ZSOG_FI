*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_004_SALES_UPLOAD_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PROCESSING
*&---------------------------------------------------------------------*
FORM processing .
*  PERFORM upload_data.

*  DATA: lt_excel TYPE TABLE OF zsog_fi_004_s_001.
  DATA: ls_tarih TYPE  zsog_fi_004_t_02.

  SELECT SINGLE *  FROM zsog_fi_004_t_02 INTO  ls_tarih
                                              WHERE tarih IN s_tarih.
  IF sy-subrc EQ 0.
    MESSAGE i018(zsg) DISPLAY LIKE 'I' WITH ls_tarih-kayitno.
    LEAVE LIST-PROCESSING.
  ENDIF.


  SELECT retailer_no
         retailer_name
         gross_sales
         amount_to_be_refund
         paid_refunds
         gross_sales_refunds
         net_sales
         sov
         vat
         sales_comission
         pbc_comission
         comission_to_refund
         payout
         net_payout
         payout_comission
         income_tax
         inheritance_tax
         FROM zsg_t_021
         INTO CORRESPONDING FIELDS OF TABLE gs_scr-1903-alv
         WHERE file_date IN s_tarih.
  IF sy-subrc NE 0.
    MESSAGE i002(zsg) DISPLAY LIKE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*  PERFORM import_excel TABLES lt_excel USING p_file 'ZSOG_FI_004_S_001'.
*  PERFORM modify_data TABLES lt_excel.

ENDFORM.                    " PROCESSING
*----------------------------------------------------------------------*
FORM f4_filename CHANGING pv_file.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename = pv_file
      mask         = '*.*'
      mode         = 'O'
    IMPORTING
      filename     = pv_file
    EXCEPTIONS
      OTHERS       = 0.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE s020(rempt).
  ENDIF.
ENDFORM.                    " F4_FILENAME
*----------------------------------------------------------------------*
FORM initialize_program.
  CONCATENATE icon_xls c_texts-sample_file_sscr
    INTO sscrfields-functxt_01.
ENDFORM.                    "initialize_program
*&---------------------------------------------------------------------*
*&      Form  at_sscr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM at_sscr.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM export_structure_to_excel USING 'ZSOG_FI_004_S_001'.
    WHEN 'ONLI'.
      PERFORM get_date_list_between_dates.
  ENDCASE.
ENDFORM.                    "at_sscr
*&---------------------------------------------------------------------*
*&      Form  export_structure_to_excel
*&---------------------------------------------------------------------*
FORM export_structure_to_excel USING pv_structurename.

  DATA: lt_excel TYPE TABLE OF zsog_fi_004_s_001,
        ls_excel TYPE zsog_fi_004_s_001.
  DATA: lt_file     TYPE filetable WITH HEADER LINE,
        lv_rc       TYPE i,
        lv_xstring  TYPE xstring,
        lv_size     TYPE i,
        lt_bintab   TYPE solix_tab,
        lv_filename TYPE string,
        lv_path     TYPE string,
        lv_path2    TYPE string.

  DATA: lv_title             TYPE string,
        lv_default_extension TYPE string VALUE 'XLSX',
        lv_default_file_name TYPE string VALUE 'export.XLSX',
        lv_initial_directory TYPE string,
        lv_mask              TYPE string VALUE 'Excel (*.XLSX)|*.XLSX',
        lv_application       TYPE string,
        lv_loc_fn            TYPE string,
        lv_loc_dir           TYPE string,
        lv_user_action       TYPE i.

  zcl_sog_excel_download=>create_xls_from_itab(
    EXPORTING
    i_xlsx             = 'X'
    IMPORTING
      e_xstring         = lv_xstring
    CHANGING
      ct_data           =  lt_excel
         ).

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = lv_title
      default_extension    = lv_default_extension
      default_file_name    = lv_default_file_name           "#EC NOTEXT
      file_filter          = lv_mask
      initial_directory    = lv_initial_directory
    CHANGING
      filename             = lv_loc_fn
      path                 = lv_loc_dir
      fullpath             = lv_loc_dir
      user_action          = lv_user_action
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE e162(alvht).
    EXIT.
  ENDIF.

  CONCATENATE lv_loc_dir lv_loc_fn INTO lv_path2.

  IF lv_xstring IS NOT INITIAL.
    "save file
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer          = lv_xstring
*       APPEND_TO_TABLE = ' '
      IMPORTING
        output_length   = lv_size
      TABLES
        binary_tab      = lt_bintab.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = lv_size
        filename                  = lv_path2
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_bintab
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
           ).
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.
  ENDIF.

ENDFORM.                    "export_structure
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization .
  PERFORM restrict_datum.
ENDFORM.                    "initialization
*&---------------------------------------------------------------------*
*&      Form  RESTRIC_DATUM
*&---------------------------------------------------------------------*
FORM restrict_datum .
* Restrict the select options for S_DATE
* to just a date range
  DATA: selopt   TYPE sscr_ass,
        opt_list TYPE sscr_opt_list,
        restrict TYPE sscr_restrict.

  CLEAR opt_list.
  opt_list-name          = 'BT'.
  opt_list-options-bt    = 'X'.
  APPEND opt_list TO restrict-opt_list_tab.

  CLEAR selopt.
  selopt-kind            = 'S'.
  selopt-name            = 'S_TARIH'.
  selopt-sg_main         = 'I'.
  selopt-sg_addy         = ' '.
  selopt-op_main         = 'BT'.
  selopt-op_addy         = 'BT'.
  APPEND selopt  TO restrict-ass_tab.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction            = restrict
    EXCEPTIONS
      too_late               = 1
      repeated               = 2
      selopt_without_options = 5
      selopt_without_signs   = 6
      invalid_sign           = 7
      empty_option_list      = 9
      invalid_kind           = 10
      repeated_kind_a        = 11
      OTHERS                 = 12.

ENDFORM.                    "restrict_datum
*----------------------------------------------------------------------*
*FORM import_excel TABLES pt_excel STRUCTURE zsog_fi_004_s_001
*                   USING pv_filename pv_structurename.
*
*  DATA: lv_file_path LIKE rlgrap-filename.
*  DATA: it_file LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.
*  FIELD-SYMBOLS: <field> TYPE any.
*  DATA: lv_col_exists.
*  DATA: ls_data TYPE zsog_fi_004_s_001.
*  DATA: ls_excel  TYPE zsog_fi_004_s_001.
*
*  lv_file_path = pv_filename.
*
*  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
*    EXPORTING
*      filename                = lv_file_path
*      i_begin_col             = '1'
*      i_begin_row             = '1'
*      i_end_col               = '100'
*      i_end_row               = '100000'
*    TABLES
*      intern                  = it_file
*    EXCEPTIONS
*      inconsistent_parameters = 1
*      upload_ole              = 2
*      OTHERS                  = 3.
*  IF sy-subrc NE 0.
*  ENDIF.
*  IF it_file[] IS INITIAL.
*    MESSAGE i398(00) WITH c_texts-error_uploading_file.
*  ENDIF.
*
*  LOOP AT it_file.
*    UNASSIGN <field>.
*    ASSIGN COMPONENT it_file-col OF STRUCTURE ls_data
*      TO <field>.
*
*    CLEAR lv_col_exists.
*    PERFORM call_conversion USING it_file-col pv_structurename
*                            CHANGING it_file-value
*                                     lv_col_exists.
*
*    IF lv_col_exists IS NOT INITIAL.
*      PERFORM move_char_to_value
*                                 CHANGING it_file-value <field>.
*    ENDIF.
*
*    AT END OF row.
*      MOVE-CORRESPONDING ls_data TO ls_excel.
*      APPEND ls_excel TO pt_excel[].
*      CLEAR:  ls_excel, ls_data.
*    ENDAT.
*  ENDLOOP.
*
*ENDFORM.                    " GET_FILENAME_AND_IMPORT_EXCEL
*----------------------------------------------------------------------*
FORM call_conversion  USING pv_col pv_structurename
                      CHANGING pv_value
                               pv_col_exists.

  STATICS: lv_structurename_old LIKE dd02l-tabname.
  STATICS: lt_dd01l LIKE dd01l OCCURS 0 WITH HEADER LINE.
  STATICS: lt_dd03l LIKE dd03l OCCURS 0 WITH HEADER LINE.

  DATA:  ls_fcat TYPE lvc_s_fcat.
  DATA:  lt_fcat TYPE lvc_t_fcat.

  IF lt_fcat[] IS INITIAL OR
     pv_structurename NE lv_structurename_old.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = pv_structurename
        i_client_never_display = 'X'
        i_bypassing_buffer     = 'X'
      CHANGING
        ct_fieldcat            = lt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF lt_fcat[] IS NOT INITIAL.
      SELECT *
        FROM dd03l
        INTO TABLE lt_dd03l
        FOR ALL ENTRIES IN lt_fcat
        WHERE tabname = pv_structurename
          AND fieldname = lt_fcat-fieldname.
      IF lt_dd03l[] IS NOT INITIAL.
        SELECT *
          FROM dd01l
          INTO TABLE lt_dd01l
          FOR ALL ENTRIES IN lt_dd03l
          WHERE domname = lt_dd03l-domname.
      ENDIF.
    ENDIF.
    lv_structurename_old = pv_structurename.
  ENDIF.

  READ TABLE lt_fcat INTO ls_fcat INDEX pv_col.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.
  pv_col_exists = 'X'.
  CLEAR lt_dd03l.
  READ TABLE lt_dd03l WITH KEY tabname = pv_structurename
                               fieldname = ls_fcat-fieldname.
  CLEAR lt_dd01l.
  READ TABLE lt_dd01l WITH KEY domname = lt_dd03l-domname.
  CHECK sy-subrc EQ 0.
  CHECK lt_dd01l-convexit IS NOT INITIAL.

*  DATA: lv_conv_exit_func_name TYPE rs38l_fnam.

*  CONCATENATE 'CONVERSION_EXIT_' lt_dd01l-convexit '_INPUT'
*    INTO lv_conv_exit_func_name.
*
*  CALL FUNCTION lv_conv_exit_func_name
*    EXPORTING
*      input  = pv_value
*    IMPORTING
*      output = pv_value(ls_fcat-dd_outlen).

ENDFORM.                    " CALL_CONVERSION
*----------------------------------------------------------------------*
FORM move_char_to_value  CHANGING pv_char
                                  pv_value.
  DATA: lv_strlen TYPE i.
  DATA: lv_char TYPE string.
  DATA: lv_type.

  DESCRIBE FIELD pv_value TYPE lv_type.
  CASE lv_type.
    WHEN 'P'.
* Remove leading whitespace from a_string
      DO.
        lv_char = pv_char+0(1).

        FIND REGEX '[[:space:]]' IN lv_char.
        CASE sy-subrc.
          WHEN 0.
            SHIFT pv_char LEFT DELETING LEADING lv_char.
          WHEN OTHERS.
            EXIT.
        ENDCASE.
      ENDDO.

      SHIFT pv_char LEFT DELETING LEADING space.
      CONDENSE pv_char.
      REPLACE ALL OCCURRENCES OF '.' IN pv_char WITH ' '.

      CONDENSE pv_char.
      REPLACE ALL OCCURRENCES OF ',' IN pv_char WITH '.'.
      CONDENSE pv_char NO-GAPS.
      MOVE pv_char TO pv_value.
    WHEN 'F'.
    WHEN 'D'.
      PERFORM convert_date_char_to_sap USING pv_char
                                       CHANGING pv_value.
    WHEN OTHERS.
      MOVE pv_char TO pv_value.
  ENDCASE.
ENDFORM.                    "move_char_to_value
*----------------------------------------------------------------------*
FORM convert_date_char_to_sap  USING pv_char
                               CHANGING pv_datum.
  CONCATENATE pv_char+6(4) pv_char+3(2) pv_char(2) INTO pv_datum.
ENDFORM.                    " CONVERT_DATE_CHAR_TO_SAP
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data TABLES lt_excel STRUCTURE zsog_fi_004_s_001 .

*  DATA: ls_excel  TYPE zsog_fi_004_s_001.
*  DATA: ls_alv    TYPE zsog_fi_004_t_01.
*
*  LOOP AT lt_excel INTO ls_excel.
**    PERFORM customer_conv CHANGING ls_excel-retailer_no.
*    MOVE-CORRESPONDING ls_excel TO ls_alv.
*    APPEND ls_alv TO gs_scr-1903-alv.
*    CLEAR: ls_alv.
*  ENDLOOP.
ENDFORM.                    " MODIFY_DATA
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

ENDFORM.                    "customer_conv
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
      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'MANDT' ).
      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO not_found.
      " error handling
  ENDTRY.

*  TRY.
*      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'RMF' ).
*      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
*    CATCH cx_salv_not_found INTO not_found.
*      " error handling
*  ENDTRY.
*
*  TRY.
*      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'H_RECORD_TYPE' ).
*      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
*    CATCH cx_salv_not_found INTO not_found.
*      " error handling
*  ENDTRY.
*
*  TRY.
*      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'H_VERSION_NO' ).
*      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
*    CATCH cx_salv_not_found INTO not_found.
*      " error handling
*  ENDTRY.
*
*  TRY.
*      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'H_COMP_CODE' ).
*      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
*    CATCH cx_salv_not_found INTO not_found.
*      " error handling
*  ENDTRY.
*
*  TRY.
*      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'H_CREATION_DATE_TIME' ).
*      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
*    CATCH cx_salv_not_found INTO not_found.
*      " error handling
*  ENDTRY.
*
*  TRY.
*      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'H_FILE_TYPE' ).
*      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
*    CATCH cx_salv_not_found INTO not_found.
*      " error handling
*  ENDTRY.
*
*  TRY.
*      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'H_NUMBER_OF_RECORDS' ).
*      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
*    CATCH cx_salv_not_found INTO not_found.
*      " error handling
*  ENDTRY.

ENDFORM.                    " HIDE_CLIENT_COLUMN
*&---------------------------------------------------------------------*
*&      Form  set_icon
*&---------------------------------------------------------------------*
FORM set_icon.
*  DATA: lr_columns TYPE REF TO cl_salv_columns_table,
*       lr_column  TYPE REF TO cl_salv_column_table.
**
*  lr_columns = gs_scr-1903-r_alv->get_columns( ).
*  lr_column ?= lr_columns->get_column( 'CREATED' ).
*  lr_column->set_icon( if_salv_c_bool_sap=>true ).
**
*  lr_column ?= lr_columns->get_column( 'RMF_ICON' ).
*  lr_column->set_icon( if_salv_c_bool_sap=>true ).
*
*  lr_column ?= lr_columns->get_column( 'RMF_ICON' ).
*  lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*
*  lr_column ?= lr_columns->get_column( 'STATUS' ).
*  lr_column->set_icon( if_salv_c_bool_sap=>true ).
*
*  lr_column ?= lr_columns->get_column( 'STATUS' ).
*  lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

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
  lv_tanim = |SBS Satış | && |--> | && |{ lv_line }| && | Kayıt Bulundu|.

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
*&      Form  NUMBER_GET_NEXT
*&---------------------------------------------------------------------*
FORM number_get_next  CHANGING pv_kayitno.
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZSBS'
    IMPORTING
      number                  = pv_kayitno
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.                    "number_get_next
*&---------------------------------------------------------------------*
*&      Form  GET_DATE_LIST_BETWEEN_DATES
*&---------------------------------------------------------------------*
FORM get_date_list_between_dates .
  DATA:BEGIN OF daytab OCCURS 0.
          INCLUDE STRUCTURE scscp_period_str.
  DATA:END OF daytab.
  DATA: ls_dates TYPE zsog_fi_004_t_02.

  IF s_tarih-high NE s_tarih-low.
    s_tarih-high = s_tarih-high - 1.
  ENDIF.

  MODIFY s_tarih[] FROM s_tarih INDEX 1 TRANSPORTING high.

  CALL FUNCTION 'CSCP_PARA1_GET_PERIODS'
    EXPORTING
      i_datuv    = s_tarih-low
      i_datub    = s_tarih-high
      i_timeunit = 'D'
    TABLES
      et_dates   = daytab.
  DELETE daytab INDEX 1.
  LOOP AT daytab WHERE datuv IN s_tarih.
    ls_dates-tarih = daytab-datuv.
    ls_dates-mandt = sy-mandt.
    APPEND ls_dates TO gs_scr-1903-dates.
    CLEAR: ls_dates.
  ENDLOOP.
ENDFORM.                    " GET_DATE_LIST_BETWEEN_DATES
*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
FORM handle_user_command USING i_ucomm TYPE salv_de_function.
  CASE i_ucomm.
    WHEN '&KAYIT'.
      PERFORM kayit_1903.
*      CALL SCREEN 1903.
    WHEN '&DEGISTIR'.
*      PERFORM kayit_degistir.
  ENDCASE.
ENDFORM.                    "handle_user_command
*&------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  KAYIT_1903
*&---------------------------------------------------------------------*
FORM kayit_1903 .

  TYPES: BEGIN OF ltt_kna1,
         kunnr TYPE kna1-kunnr,
         katr3 TYPE kna1-katr3,
         END OF ltt_kna1.

  DATA: lt_kna1 TYPE TABLE OF ltt_kna1,
        ls_kna1 TYPE ltt_kna1,
        ls_alv  TYPE zsog_fi_004_t_01,
        lt_satis_fi TYPE TABLE OF zsog_fi_008_t_01,
        ls_satis_fi TYPE  zsog_fi_008_t_01,
        lt_mobil    TYPE TABLE OF zsog_fi_008_t_01,
        lt_mobil_sg TYPE TABLE OF zsog_fi_008_t_01,
        lt_lucky    TYPE TABLE OF zsog_fi_008_t_01,
        lt_islem    TYPE TABLE OF dd07t,
        ls_islem    TYPE dd07t,
        ls_mobil    TYPE zsog_fi_008_t_01,
        lt_colums   TYPE TABLE OF zsog_fi_008_t_02,
        ls_colums   TYPE  zsog_fi_008_t_02.

  DATA : ls_header        TYPE           bapiache09           ,
         lv_obj_type      TYPE           bapiache09-obj_type  ,
         lv_obj_key       TYPE           bapiache09-obj_key   ,
         lv_obj_sys       TYPE           bapiache09-obj_sys   ,
         lt_account       TYPE TABLE OF  bapiacgl09           ,
         lt_amount        TYPE TABLE OF  bapiaccr09           ,
         lt_payable       TYPE TABLE OF  bapiacap09           ,
         lt_extension2    TYPE TABLE OF  bapiparex           ,
         lt_tax           TYPE TABLE OF  bapiactx09           ,
         lt_receivable    TYPE TABLE OF  bapiacar09           .
  DATA:  lt_return  TYPE TABLE OF  bapiret2,
         lt_return2 TYPE TABLE OF  bapiret2,
         ls_return TYPE bapiret2.
  DATA: lv_titlebar(60)  TYPE c,
        lv_question(400) TYPE c.
  DATA: lv_answer        TYPE wfcst_char1.
  DATA: lv_item_no  TYPE posnr_acc.
  DATA: lv_subrc    TYPE sy-subrc.
  DATA: lv_kayitno  TYPE char10.
  DATA: lt_islem_belge TYPE TABLE OF zsog_fi_004_t_03 ,
        ls_islem_belge TYPE zsog_fi_004_t_03 ,
        ls_dates       TYPE zsog_fi_004_t_02.
  FIELD-SYMBOLS: <fs_amount> TYPE any.


  lv_titlebar = 'Muhasebe Kayıt'.
  lv_question =  'İşlemler kaydedilecektir. Emin misiniz?'.
  PERFORM pop_up_yes_no USING lv_titlebar lv_question CHANGING lv_answer.
  IF lv_answer NE '1'.
    RETURN.
  ENDIF.


  IF gs_scr-1903-alv IS NOT INITIAL.
    SELECT kunnr katr3 FROM kna1 INTO TABLE lt_kna1
                                 FOR ALL ENTRIES IN gs_scr-1903-alv
                                 WHERE kunnr = gs_scr-1903-alv-retailer_no.
    SORT  lt_kna1 BY kunnr.
  ENDIF.

  SELECT * FROM zsog_fi_008_t_01 INTO TABLE lt_satis_fi.
  SELECT * FROM zsog_fi_008_t_01 INTO TABLE lt_mobil    WHERE surec EQ '01' AND kunnr EQ ''.
  SELECT * FROM zsog_fi_008_t_01 INTO TABLE lt_mobil_sg WHERE surec EQ '01' AND kunnr NE ''.
  SELECT * FROM zsog_fi_008_t_01 INTO TABLE lt_lucky    WHERE surec EQ '02' AND kunnr NE ''.
  SELECT * FROM dd07t INTO TABLE lt_islem WHERE domname EQ 'ZSOG_FI_008_D_ISLEM'.

  SORT lt_islem BY domvalue_l.
  SORT lt_satis_fi BY surec kunnr.
  SELECT * FROM zsog_fi_008_t_02 INTO TABLE lt_colums.
  SORT lt_colums BY islem.

*  break xbgursel.
  LOOP AT gs_scr-1903-alv INTO ls_alv.
    IF lv_subrc IS NOT INITIAL.
      EXIT.
    ENDIF.
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_alv-retailer_no BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF ls_kna1-katr3 = '4'. "Mobil bayi için fi kayitları
        LOOP AT lt_colums INTO ls_colums.
          ASSIGN COMPONENT ls_colums-sutun OF STRUCTURE ls_alv TO <fs_amount>.
          IF <fs_amount> IS ASSIGNED.
            IF <fs_amount> IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
          READ TABLE lt_islem INTO ls_islem WITH KEY domvalue_l = ls_colums-islem.
          LOOP AT lt_mobil INTO ls_mobil WHERE islem = ls_colums-islem.
            lv_item_no = lv_item_no + 1.
            IF ls_mobil-lifnr IS NOT INITIAL. "spor toto satırı
              PERFORM add_extension2     TABLES lt_extension2 USING lv_item_no ls_alv  ls_mobil.
              PERFORM add_accountpayable TABLES lt_payable USING lv_item_no ls_alv  ls_mobil ls_islem.
              PERFORM add_currencyamount TABLES lt_amount  USING lv_item_no ls_alv ls_mobil ls_colums.
            ELSEIF ls_mobil-hkont IS NOT INITIAL. " ana hesap satırı
              PERFORM add_extension2     TABLES lt_extension2 USING lv_item_no ls_alv  ls_mobil.
              PERFORM add_accountgl      TABLES lt_account USING lv_item_no ls_alv ls_mobil ls_colums ls_islem.
              PERFORM add_currencyamount TABLES lt_amount  USING lv_item_no ls_alv ls_mobil ls_colums.
            ELSE. " bayi satırı
              PERFORM add_extension2     TABLES lt_extension2 USING lv_item_no ls_alv  ls_mobil.
              PERFORM add_receivable TABLES lt_receivable  USING lv_item_no ls_alv  ls_mobil ls_islem.
              PERFORM add_currencyamount TABLES lt_amount  USING lv_item_no ls_alv ls_mobil ls_colums.
            ENDIF.
          ENDLOOP.
          IF sy-subrc EQ 0.
            PERFORM fill_bapi_header USING  ls_colums-islem ls_islem CHANGING ls_header .

            PERFORM call_bapi TABLES lt_account lt_amount lt_payable lt_tax
                                     lt_receivable lt_return lt_extension2
                              USING  ls_header
                            CHANGING lv_obj_type
                                     lv_obj_key
                                     lv_obj_sys.
            IF lt_return IS NOT INITIAL.
              APPEND LINES OF lt_return TO lt_return2.
            ENDIF.
            LOOP AT lt_return INTO ls_return WHERE type CA 'EAX'.
              EXIT.
            ENDLOOP.
            IF sy-subrc EQ 0.
              lv_subrc = 4.
              EXIT.
            ELSE.
              ls_islem_belge-mandt = sy-mandt.
              ls_islem_belge-islem = ls_colums-islem.
              ls_islem_belge-kunnr = ls_alv-retailer_no.
              ls_islem_belge-belnr = lv_obj_key+0(10).
              ls_islem_belge-gjahr = lv_obj_key+14(4).
              APPEND ls_islem_belge TO lt_islem_belge.
*              PERFORM bapi_rollback_destination.
*            ELSE.
*              PERFORM bapi_commit_destination.
            ENDIF.
          ENDIF.
          CLEAR: ls_kna1, ls_satis_fi, ls_mobil, ls_islem_belge.
          CLEAR: lt_account,  lt_amount, lt_payable, lt_tax, lt_receivable, lt_return,
                 ls_header, lv_obj_type, lv_obj_key, lv_obj_sys, lv_item_no.
        ENDLOOP.
      ELSE.
        READ TABLE lt_satis_fi INTO ls_satis_fi WITH KEY surec = '01'
                                                         kunnr = ls_alv-retailer_no BINARY SEARCH.
        IF sy-subrc EQ 0. " Mobil Bayi Şans Girişim FI Kaydı

          LOOP AT lt_colums INTO ls_colums.
            ASSIGN COMPONENT ls_colums-sutun OF STRUCTURE ls_alv TO <fs_amount>.
            IF <fs_amount> IS ASSIGNED.
              IF <fs_amount> IS INITIAL.
                CONTINUE.
              ENDIF.
            ENDIF.
            READ TABLE lt_islem INTO ls_islem WITH KEY domvalue_l = ls_colums-islem.
            LOOP AT lt_mobil_sg INTO ls_mobil WHERE islem = ls_colums-islem.
              lv_item_no = lv_item_no + 1.
              IF ls_mobil-lifnr IS NOT INITIAL. "spor toto satırı
                PERFORM add_extension2     TABLES lt_extension2 USING lv_item_no ls_alv  ls_mobil.
                PERFORM add_accountpayable TABLES lt_payable USING lv_item_no ls_alv ls_mobil ls_islem.
                PERFORM add_currencyamount TABLES lt_amount  USING lv_item_no ls_alv ls_mobil ls_colums.
              ELSEIF ls_mobil-hkont IS NOT INITIAL. " ana hesap satırı
                PERFORM add_extension2     TABLES lt_extension2 USING lv_item_no ls_alv  ls_mobil.
                PERFORM add_accountgl      TABLES lt_account USING lv_item_no ls_alv ls_mobil ls_colums ls_islem.
                PERFORM add_currencyamount TABLES lt_amount  USING lv_item_no ls_alv ls_mobil ls_colums.
              ELSE. " bayi satırı
                PERFORM add_extension2     TABLES lt_extension2 USING lv_item_no ls_alv  ls_mobil.
                PERFORM add_receivable TABLES lt_receivable  USING lv_item_no ls_alv ls_mobil ls_islem.
                PERFORM add_currencyamount TABLES lt_amount  USING lv_item_no ls_alv ls_mobil ls_colums.
              ENDIF.
            ENDLOOP.
            IF sy-subrc EQ 0.

              PERFORM fill_bapi_header USING ls_colums-islem ls_islem CHANGING ls_header .

              PERFORM call_bapi TABLES lt_account lt_amount lt_payable lt_tax
                                 lt_receivable lt_return lt_extension2
                          USING  ls_header
                          CHANGING lv_obj_type
                                   lv_obj_key
                                   lv_obj_sys.
              IF lt_return IS NOT INITIAL.
                APPEND LINES OF lt_return TO lt_return2.
              ENDIF.
              LOOP AT lt_return INTO ls_return WHERE type CA 'EAX'.
                EXIT.
              ENDLOOP.
              IF sy-subrc EQ 0.
                lv_subrc = 4.
                EXIT.
              ELSE.
                ls_islem_belge-mandt = sy-mandt.
                ls_islem_belge-islem = ls_colums-islem.
                ls_islem_belge-kunnr = ls_alv-retailer_no.
                ls_islem_belge-belnr = lv_obj_key+0(10).
                ls_islem_belge-gjahr = lv_obj_key+14(4).
                APPEND ls_islem_belge TO lt_islem_belge.
*                PERFORM bapi_rollback_destination.
*              ELSE.
*                PERFORM bapi_commit_destination.
              ENDIF.
            ENDIF.
            CLEAR: ls_kna1, ls_satis_fi, ls_mobil, ls_islem_belge.
            CLEAR: lt_account,  lt_amount, lt_payable, lt_tax, lt_receivable, lt_return,
                   ls_header, lv_obj_type, lv_obj_key, lv_obj_sys, lv_item_no.
          ENDLOOP.
        ELSE.
          READ TABLE lt_satis_fi INTO ls_satis_fi WITH KEY surec = '02'
                                                           kunnr = ls_alv-retailer_no BINARY SEARCH.
          IF sy-subrc EQ 0. " Luck Bayi Şans Girişim Fi Kaydı

            LOOP AT lt_colums INTO ls_colums.

              ASSIGN COMPONENT ls_colums-sutun OF STRUCTURE ls_alv TO <fs_amount>.
              IF <fs_amount> IS ASSIGNED.
                IF <fs_amount> IS INITIAL.
                  CONTINUE.
                ENDIF.
              ENDIF.
              READ TABLE lt_islem INTO ls_islem WITH KEY domvalue_l = ls_colums-islem.
              LOOP AT lt_lucky INTO ls_mobil WHERE islem = ls_colums-islem.
                lv_item_no = lv_item_no + 1.
                IF ls_mobil-lifnr IS NOT INITIAL. "spor toto satırı
                  PERFORM add_extension2     TABLES lt_extension2 USING lv_item_no ls_alv  ls_mobil.
                  PERFORM add_accountpayable TABLES lt_payable USING lv_item_no ls_alv ls_mobil ls_islem.
                  PERFORM add_currencyamount TABLES lt_amount  USING lv_item_no ls_alv ls_mobil ls_colums.
                ELSEIF ls_mobil-hkont IS NOT INITIAL. " ana hesap satırı
                  PERFORM add_extension2     TABLES lt_extension2 USING lv_item_no ls_alv  ls_mobil.
                  PERFORM add_accountgl      TABLES lt_account USING lv_item_no ls_alv ls_mobil ls_colums ls_islem.
                  PERFORM add_currencyamount TABLES lt_amount  USING lv_item_no ls_alv ls_mobil ls_colums.
                ELSE. " bayi satırı
                  PERFORM add_extension2     TABLES lt_extension2 USING lv_item_no ls_alv  ls_mobil.
                  PERFORM add_receivable     TABLES lt_receivable  USING lv_item_no ls_alv ls_mobil ls_islem.
                  PERFORM add_currencyamount TABLES lt_amount  USING lv_item_no ls_alv ls_mobil ls_colums.
                ENDIF.
              ENDLOOP.
              IF sy-subrc EQ 0.

                PERFORM fill_bapi_header USING ls_colums-islem ls_islem CHANGING ls_header.

                PERFORM call_bapi TABLES lt_account lt_amount lt_payable lt_tax
                                   lt_receivable lt_return lt_extension2
                            USING  ls_header
                            CHANGING lv_obj_type
                                     lv_obj_key
                                     lv_obj_sys.
                IF lt_return IS NOT INITIAL.
                  APPEND LINES OF lt_return TO lt_return2.
                ENDIF.
                LOOP AT lt_return INTO ls_return WHERE type CA 'EAX'.
                  EXIT.
                ENDLOOP.
                IF sy-subrc EQ 0.
                  lv_subrc = 4.
                  EXIT.
                ELSE.

                  ls_islem_belge-mandt = sy-mandt.
                  ls_islem_belge-islem = ls_colums-islem.
                  ls_islem_belge-kunnr = ls_alv-retailer_no.
                  ls_islem_belge-belnr = lv_obj_key+0(10).
                  ls_islem_belge-gjahr = lv_obj_key+14(4).
                  APPEND ls_islem_belge TO lt_islem_belge.
*                  PERFORM bapi_rollback_destination.
*                ELSE.
*                  PERFORM bapi_commit_destination.
                ENDIF.
              ENDIF.
              CLEAR: ls_kna1, ls_satis_fi, ls_mobil, ls_islem_belge.
              CLEAR: lt_account,  lt_amount, lt_payable, lt_tax, lt_receivable, lt_return,
                     ls_header, lv_obj_type, lv_obj_key, lv_obj_sys, lv_item_no.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: ls_kna1, ls_satis_fi, ls_mobil, ls_islem_belge.
    CLEAR: lt_account,  lt_amount, lt_payable, lt_tax, lt_receivable, lt_return,
           ls_header, lv_obj_type, lv_obj_key, lv_obj_sys, lv_item_no.
  ENDLOOP.

  LOOP AT lt_return2 INTO ls_return WHERE type CA 'EAX'.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    PERFORM bapi_rollback_destination.
  ELSE.
    PERFORM bapi_commit_destination.

    PERFORM number_get_next CHANGING lv_kayitno.
    ls_alv-kayitno = lv_kayitno.
    MODIFY gs_scr-1903-alv FROM ls_alv TRANSPORTING kayitno WHERE kayitno IS INITIAL.
    ls_islem_belge-kayitno = lv_kayitno.
    MODIFY lt_islem_belge FROM ls_islem_belge TRANSPORTING kayitno WHERE kayitno IS INITIAL.
    ls_dates-kayitno = lv_kayitno.
*    ls_dates-mandt   = sy-mandt.
    MODIFY gs_scr-1903-dates FROM ls_dates TRANSPORTING mandt kayitno WHERE kayitno IS INITIAL.
    IF gs_scr-1903-alv IS  NOT INITIAL.
      MODIFY zsog_fi_004_t_01  FROM TABLE gs_scr-1903-alv.
      COMMIT WORK AND WAIT.
    ENDIF.
    IF lt_islem_belge IS NOT INITIAL.
      MODIFY zsog_fi_004_t_03  FROM TABLE lt_islem_belge.
      COMMIT WORK AND WAIT.
    ENDIF.

    IF gs_scr-1903-dates IS NOT INITIAL.
      MODIFY zsog_fi_004_t_02   FROM TABLE gs_scr-1903-dates.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  IF lt_return2 IS NOT INITIAL.
    PERFORM msg_display_error_table TABLES lt_return2.
  ENDIF.


ENDFORM.                    " KAYIT_1903
*&---------------------------------------------------------------------*
*&      Form  msg_display_error_table
*&---------------------------------------------------------------------*
FORM msg_display_error_table TABLES pt_return STRUCTURE bapiret2.
  CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
    EXPORTING
      it_message = pt_return[].
ENDFORM.                    "msg_display_error_table
*&---------------------------------------------------------------------*
*&      Form  BAPI_COMMIT_DESTINATION
*&---------------------------------------------------------------------*
FORM bapi_commit_destination .
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
ENDFORM.                    "bapi_commit_destination
*&---------------------------------------------------------------------*
*&      Form  BAPI_ROLLBACK_DESTINATION
*&---------------------------------------------------------------------*
FORM bapi_rollback_destination .
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ENDFORM.                    "bapi_rollback_destination
*&---------------------------------------------------------------------*
*&      Form  POP_UP_YES_NO
*&---------------------------------------------------------------------*
FORM pop_up_yes_no  USING    pv_titlebar
                             pv_question
                    CHANGING pv_answer.
  DATA ls_ui_popup_text TYPE wfcsr_ui_popup_text.
  ls_ui_popup_text-titlebar = pv_titlebar.
  ls_ui_popup_text-question = pv_question.

  CALL FUNCTION 'WFCS_POPUP_YES_NO'
    EXPORTING
      pi_ui_popup_text  = ls_ui_popup_text
    CHANGING
      pe_answer         = pv_answer
    EXCEPTIONS
      error_using_popup = 1
      OTHERS            = 2.


ENDFORM.                    " POP_UP_YES_NO
*&---------------------------------------------------------------------*
*&      Form  CALL_BAPI
*&---------------------------------------------------------------------*
FORM call_bapi  TABLES   pt_account    STRUCTURE bapiacgl09
                         pt_amount     STRUCTURE bapiaccr09
                         pt_payable    STRUCTURE bapiacap09
                         pt_tax        STRUCTURE bapiactx09
                         pt_receivable STRUCTURE bapiacar09
                         pt_return     STRUCTURE bapiret2
                         pt_extension2 STRUCTURE bapiparex
                USING    ps_header     STRUCTURE bapiache09
                CHANGING pv_obj_type   TYPE bapiache09-obj_type
                         pv_obj_key    TYPE bapiache09-obj_key
                         pv_obj_sys    TYPE bapiache09-obj_sys.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader    = ps_header
    IMPORTING
      obj_type          = pv_obj_type
      obj_key           = pv_obj_key
      obj_sys           = pv_obj_sys
    TABLES
      accountgl         = pt_account[]
      accountreceivable = pt_receivable[]
      currencyamount    = pt_amount[]
      accountpayable    = pt_payable[]
      accounttax        = pt_tax[]
      return            = pt_return[]
      extension2        = pt_extension2[].

ENDFORM.                    " CALL_BAPI
*&---------------------------------------------------------------------*
*&      Form  FILL_BAPI_HEADER
*&---------------------------------------------------------------------*
FORM fill_bapi_header  USING    pv_islem
                                pt_islem   TYPE dd07t
                       CHANGING ps_header STRUCTURE bapiache09.

  ps_header-bus_act         = 'RFBU'              .
  ps_header-username        = sy-uname            .
  ps_header-comp_code       = '2425'              .
  ps_header-doc_date        =  sy-datum           .
  ps_header-pstng_date      = sy-datum            .
*  ps_header-trans_date      = p_header-budat      .
  ps_header-fisc_year       = sy-datum+0(4)       .
  ps_header-fis_period      = sy-datum+4(2)       .
  ps_header-doc_type        = 'SD'                .
*  ps_header-header_txt      =  'Masraf Açıklama'  .
  ps_header-ref_doc_no      = pt_islem-ddtext      .
*  ps_header-acc_principle   = p_header-ldgrp      .
ENDFORM.                    " FILL_BAPI_HEADER
*&---------------------------------------------------------------------*
*&      Form  ADD_ACCOUNTPAYABLE
*&---------------------------------------------------------------------*
FORM add_accountpayable  TABLES pt_payable STRUCTURE bapiacap09
                          USING pv_item_no TYPE posnr_acc
                                ps_alv     TYPE zsog_fi_004_t_01
                                ls_mobil   TYPE zsog_fi_008_t_01
                                pt_islem   TYPE dd07t.

  pt_payable-itemno_acc =  pv_item_no  .
  pt_payable-vendor_no  =  ls_mobil-lifnr.
  pt_payable-item_text  =  pt_islem-ddtext.
*  pt_payable-gl_account =  ls_mobil-hkont.
  APPEND pt_payable.

ENDFORM.                    "add_accountpayable
*&---------------------------------------------------------------------*
*&      Form  ADD_RECEIVABLE
*&---------------------------------------------------------------------*
FORM add_receivable  TABLES pt_receivable  STRUCTURE  bapiacar09
                      USING pv_item_no TYPE posnr_acc
                            ps_alv     TYPE zsog_fi_004_t_01
                            ls_mobil   TYPE zsog_fi_008_t_01
                            pt_islem   TYPE dd07t.

  pt_receivable-itemno_acc  = pv_item_no.
  pt_receivable-customer  = ps_alv-retailer_no .
  pt_receivable-pmnttrms  = 'Z001'.
  pt_receivable-item_text  = pt_islem-ddtext.
  APPEND pt_receivable.

ENDFORM.                    "add_receivable
*&---------------------------------------------------------------------*
*&      Form  ADD_CURRENCYAMOUNT
*&---------------------------------------------------------------------*
FORM add_currencyamount  TABLES pt_amount STRUCTURE bapiaccr09
                          USING pv_item_no      TYPE posnr_acc
                                ps_alv     TYPE zsog_fi_004_t_01
                                ps_mobil   TYPE zsog_fi_008_t_01
                                ps_column  TYPE zsog_fi_008_t_02.

  FIELD-SYMBOLS: <fs_amount> TYPE any.

  ASSIGN COMPONENT ps_column-sutun OF STRUCTURE ps_alv TO <fs_amount>.
  IF <fs_amount> IS ASSIGNED.
    pt_amount-itemno_acc = pv_item_no    .
    pt_amount-curr_type  = '00'          . " Belge para birimi
    pt_amount-currency   = 'TRY'.
    IF ps_mobil-shkzg EQ 'H'.
      pt_amount-amt_doccur = <fs_amount> * -1 .
    ELSE.
      pt_amount-amt_doccur = <fs_amount>.
    ENDIF.

*  pt_amount-amt_base   = pv_amt_base.
    APPEND pt_amount.
  ENDIF.
ENDFORM.                    " ADD_CURRENCYAMOUNT
*&---------------------------------------------------------------------*
*&      Form  ADD_ACCOUNTGL
*&---------------------------------------------------------------------*
FORM add_accountgl  TABLES   pt_account STRUCTURE bapiacgl09
                    USING    pv_item_no   TYPE posnr_acc
                             ps_alv       TYPE zsog_fi_004_t_01
                             ls_mobil     TYPE zsog_fi_008_t_01
                             ps_column    TYPE zsog_fi_008_t_02
                             pt_islem     TYPE dd07t.

  pt_account-profit_ctr  = ls_mobil-prctr .
  pt_account-itemno_acc  = pv_item_no.
  pt_account-gl_account  = ls_mobil-hkont .
  pt_account-tax_code    = ls_mobil-mwskz.
  pt_account-item_text   = pt_islem-ddtext.
  APPEND pt_account.

ENDFORM.                    " ADD_ACCOUNTGL
*&---------------------------------------------------------------------*
*&      Form  ADD_EXTENSION2
*&---------------------------------------------------------------------*
FORM add_extension2  TABLES   pt_extension2 STRUCTURE bapiparex
                     USING    pv_item_no   TYPE posnr_acc
                              ps_alv       TYPE zsog_fi_004_t_01
                              ps_mobil     TYPE zsog_fi_008_t_01.

  pt_extension2-structure  = 'POSTING_KEY'.
  pt_extension2-valuepart1 = pv_item_no.
  pt_extension2-valuepart2 = ps_mobil-bschl.
  APPEND pt_extension2.

ENDFORM.                    " ADD_EXTENSION2
