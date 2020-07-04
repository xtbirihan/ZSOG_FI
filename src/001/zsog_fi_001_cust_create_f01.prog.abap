*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_001_CUST_CREATE_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM at_sscr.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM export_structure_to_excel USING 'ZSOG_FI_001_S_001'.
  ENDCASE.
ENDFORM.                    "at_sscr
*&---------------------------------------------------------------------*
*&      Form  export_structure_to_excel
*&---------------------------------------------------------------------*
FORM export_structure_to_excel USING pv_structurename.

  DATA: lt_excel TYPE TABLE OF zsog_fi_001_s_001,
        ls_excel TYPE zsog_fi_001_s_001.
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

  SELECT  k~kunnr
          k~land1
          k~name1
          k~name2
          k~ort01
          k~pstlz
          k~regio
          k~sortl
          k~stras
          k~telf1
          k~mcod1
          k~mcod3
          k~brsch
          k~ktokd
          k~ort02
          k~telf2
          k~stcd1
          k~stcd2
          k~stcd3
          k~konzs
          k~katr2
          k~katr3
          k~katr4
          k~katr5
          k~katr6
          b~akont
          b~zterm
          b~zsabe
          c~name1 AS adr_name
          c~name2 AS adr_name_2
          c~city1 AS adr_city
          c~city2 AS adr_district
          c~city_code AS adr_city_no
          c~post_code1 AS  adr_postl_cod1
          c~street AS adr_street
          c~house_num1 AS adr_house_no
          c~country AS adr_country
          c~langu AS adr_langu
          c~region AS adr_region
          c~sort1 AS adr_sort1
          c~sort2 AS adr_sort2
          c~tel_number AS adr_tel1_numbr
          c~fax_number AS adr_fax_number
          r~smtp_addr AS adr_e_mail
         FROM kna1 AS k
         INNER JOIN knb1 AS b ON k~kunnr EQ b~kunnr
                             AND b~bukrs EQ '2425'
         INNER JOIN adrc AS c ON c~addrnumber = k~adrnr
         INNER JOIN adr6 AS r ON c~addrnumber = r~addrnumber
         INTO TABLE lt_excel
         WHERE ( k~kunnr EQ 'SG901903' OR
                 k~kunnr EQ 'SG901923' ).

*          UP TO 5 ROWS

*         WHERE k~kunnr LIKE 'SG%'.


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
*----------------------------------------------------------------------*
FORM initialize_program.
  CONCATENATE icon_xls c_texts-sample_file_sscr
    INTO sscrfields-functxt_01.
ENDFORM.                    "initialize_program
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
FORM processing.
  PERFORM upload_data.
  PERFORM modify_data.
  PERFORM initialize_alv.
  PERFORM display_alv.

ENDFORM.                    "processing
*----------------------------------------------------------------------*
FORM upload_data.
  PERFORM import_excel USING p_file 'ZSOG_FI_001_S_001'.
ENDFORM.                    "upload_data
*----------------------------------------------------------------------*
FORM import_excel USING pv_filename pv_structurename.

  DATA: lv_file_path LIKE rlgrap-filename.
  DATA: it_file LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.
  FIELD-SYMBOLS: <field> TYPE any.
  DATA: lv_col_exists.
  DATA: ls_data TYPE zsog_fi_001_s_001.
  DATA: ls_alv  TYPE zsog_fi_001_s_002.

  lv_file_path = pv_filename.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = lv_file_path
      i_begin_col             = '1'
      i_begin_row             = '2'
      i_end_col               = '100'
      i_end_row               = '100000'
    TABLES
      intern                  = it_file
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc NE 0.
  ENDIF.
  IF it_file[] IS INITIAL.
    MESSAGE i398(00) WITH c_texts-error_uploading_file.
  ENDIF.

  LOOP AT it_file.
    UNASSIGN <field>.
    ASSIGN COMPONENT it_file-col OF STRUCTURE ls_data
      TO <field>.

    CLEAR lv_col_exists.
    PERFORM call_conversion USING it_file-col pv_structurename
                            CHANGING it_file-value
                                     lv_col_exists.

    IF lv_col_exists IS NOT INITIAL.
      PERFORM move_char_to_value USING it_file-value
                                 CHANGING <field>.
    ENDIF.

    AT END OF row.
      MOVE-CORRESPONDING ls_data TO ls_alv.
      APPEND ls_alv TO gs_scr-1903-alv.
      CLEAR:  ls_alv, ls_data.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " GET_FILENAME_AND_IMPORT_EXCEL
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
FORM move_char_to_value USING pv_char
                        CHANGING pv_value.
  DATA: lv_type.

  DESCRIBE FIELD pv_value TYPE lv_type.
  CASE lv_type.
    WHEN 'P'.
      REPLACE ALL OCCURRENCES OF ',' IN pv_char WITH '.'.
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
      PERFORM display_settings.
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
      gs_scr-1903-r_column = gs_scr-1903-r_columns->get_column( 'SIL' ).
      gs_scr-1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO not_found.
      " error handling
  ENDTRY.

ENDFORM.                    " HIDE_CLIENT_COLUMN
*&---------------------------------------------------------------------*
*&      Form  set_icon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_icon.
  DATA: lr_columns TYPE REF TO cl_salv_columns_table,
       lr_column  TYPE REF TO cl_salv_column_table.

  lr_columns = gs_scr-1903-r_alv->get_columns( ).
  lr_column ?= lr_columns->get_column( 'CREATED' ).
  lr_column->set_icon( if_salv_c_bool_sap=>true ).

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
FORM display_settings.
*&---------------------------------------------------------------------*
  DATA display_settings TYPE REF TO cl_salv_display_settings.
  DATA: lv_tanim TYPE text70.
  DATA: lv_line TYPE i.
  lv_line  = lines( gs_scr-1903-alv ).
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
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
FORM handle_user_command USING i_ucomm TYPE salv_de_function.
  CASE i_ucomm.
    WHEN '&CREATE'.
      PERFORM create_customer.
*      CALL SCREEN 1903.
    WHEN '&DEGISTIR'.
*      PERFORM kayit_degistir.
  ENDCASE.
ENDFORM.                    "handle_user_command
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
         lv_tabix           TYPE sy-tabix.

  DATA : ls_alv             TYPE zsog_fi_001_s_002.

  LOOP AT lt_rows INTO ls_rows.
    READ TABLE gs_scr-1903-alv INTO ls_alv INDEX ls_rows.
    lv_tabix = sy-tabix.
    IF ls_alv-process EQ 'I'.
      lc_insert = 'I'.
    ELSEIF ls_alv-process EQ 'U'.
      lc_insert = 'M'.
    ENDIF.

    PERFORM fill_tax_data      USING ls_alv lc_insert CHANGING ls_tax_ind.
    PERFORM fill_adres_comcode USING ls_alv lc_insert CHANGING ls_company_code ls_address ls_addressx.
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

*    PERFORM sd_customer_maintain_all USING ls_rows.
*    PERFORM customer_update USING ls_rows.
  ENDLOOP.
  gs_scr-1903-r_alv->refresh( ).
ENDFORM.                    " CREATE_CUSTOMER
*&---------------------------------------------------------------------*
*&      Form  SD_CUSTOMER_MAINTAIN_ALL
*&---------------------------------------------------------------------*
FORM sd_customer_maintain_all  USING ls_rows TYPE int4.

  DATA:   ls_kna1                TYPE kna1,
          ls_knb1                TYPE knb1,
          ls_adr                 TYPE bapiaddr1,
          lv_kunnr               TYPE kna1-kunnr,
          ls_o_kna1              TYPE kna1,
          lv_e_sd_cust_1321_done TYPE c,
          ls_return              TYPE  bapireturn1 ,
          lv_field               TYPE bapi_fld ,
          lt_bapiret2            TYPE bapiret2_t,
          ls_bapiret2            TYPE bapiret2,
          ls_alv                 TYPE zsog_fi_001_s_002,
          lv_tabix               TYPE sy-tabix.

  READ TABLE gs_scr-1903-alv INTO ls_alv INDEX ls_rows.
  lv_tabix = sy-tabix.
  IF ls_alv-process NE 'I'.
    RETURN.
  ENDIF.
  ls_kna1-mandt = sy-mandt.
  ls_kna1-kunnr = ls_alv-kunnr.
  ls_kna1-land1 = ls_alv-land1 .
*  ls_kna1-name1 = ls_alv-name1 .
*  ls_kna1-name2 = ls_alv-name2 .
*  ls_kna1-ort01 = ls_alv-ort01 .
*  ls_kna1-pstlz = ls_alv-pstlz .
*  ls_kna1-regio = ls_alv-regio .
*  ls_kna1-sortl = ls_alv-sortl .
*  ls_kna1-stras = ls_alv-stras .
*  ls_kna1-telf1 = ls_alv-telf1 .
*  ls_kna1-mcod1 = ls_alv-mcod1 .
*  ls_kna1-mcod3 = ls_alv-mcod3 .
  ls_kna1-brsch = ls_alv-brsch .
  ls_kna1-ktokd = ls_alv-ktokd .
*  ls_kna1-ort02 = ls_alv-ort02 .
  ls_kna1-telf2 = ls_alv-telf2 .
  ls_kna1-katr2 = ls_alv-katr2 .
  ls_kna1-katr3 = ls_alv-katr3 .
  ls_kna1-katr4 = ls_alv-katr4 .
  ls_kna1-katr5 = ls_alv-katr5 .
  ls_kna1-katr6 = ls_alv-katr6 .
  ls_kna1-stcd1 = ls_alv-stcd1 .
  ls_kna1-stcd2 = ls_alv-stcd2 .
  ls_kna1-stcd3 = ls_alv-stcd3 .
  ls_kna1-konzs = ls_alv-konzs .

  ls_knb1-mandt = sy-mandt.
  ls_knb1-kunnr = ls_alv-kunnr.
  ls_knb1-bukrs = '2425'.
  ls_knb1-akont = ls_alv-akont.
  ls_knb1-zterm = ls_alv-zterm.
  ls_knb1-zsabe = ls_alv-zsabe.

  ls_adr-name	        = ls_alv-adr_name        .
  ls_adr-name_2       = ls_alv-adr_name_2      .
  ls_adr-city	        = ls_alv-adr_city        .
  ls_adr-district	    = ls_alv-adr_district    .
  ls_adr-city_no      = ls_alv-adr_city_no     .
  ls_adr-postl_cod1	  = ls_alv-adr_postl_cod1  .
  ls_adr-street	      = ls_alv-adr_street      .
  ls_adr-house_no	    = ls_alv-adr_house_no    .
  ls_adr-country      = ls_alv-adr_country     .
  ls_adr-langu        = ls_alv-adr_langu       .
  ls_adr-region       = ls_alv-adr_region      .
  ls_adr-sort1        = ls_alv-adr_sort1       .
  ls_adr-sort2        = ls_alv-adr_sort2       .
  ls_adr-tel1_numbr	  = ls_alv-adr_tel1_numbr  .
  ls_adr-fax_number	  = ls_alv-adr_fax_number  .
  ls_adr-e_mail	      = ls_alv-adr_e_mail      .

  CALL FUNCTION 'SD_CUSTOMER_MAINTAIN_ALL'
    EXPORTING
      i_kna1                  = ls_kna1
      i_knb1                  = ls_knb1
      i_bapiaddr1             = ls_adr
      pi_postflag             = 'X'
    IMPORTING
      e_kunnr                 = lv_kunnr
      o_kna1                  = ls_o_kna1
      e_sd_cust_1321_done     = lv_e_sd_cust_1321_done
    EXCEPTIONS
      client_error            = 1
      kna1_incomplete         = 2
      knb1_incomplete         = 3
      knb5_incomplete         = 4
      knvv_incomplete         = 5
      kunnr_not_unique        = 6
      sales_area_not_unique   = 7
      sales_area_not_valid    = 8
      insert_update_conflict  = 9
      number_assignment_error = 10
      number_not_in_range     = 11
      number_range_not_extern = 12
      number_range_not_intern = 13
      account_group_not_valid = 14
      parnr_invalid           = 15
      bank_address_invalid    = 16
      tax_data_not_valid      = 17
      no_authority            = 18
      company_code_not_unique = 19
      dunning_data_not_valid  = 20
      knb1_reference_invalid  = 21
      cam_error               = 22
      OTHERS                  = 23.
  IF sy-subrc <> 0.
    ls_alv-status = icon_action_fault.
    CALL METHOD cl_msr_vrm_data=>convert_symessage2bapiret2(
      EXPORTING
        iv_field    = lv_field
      CHANGING
        ct_bapiret2 = lt_bapiret2 ).
    MODIFY gs_scr-1903-alv FROM ls_alv INDEX lv_tabix TRANSPORTING status t_messages.
  ELSE.

    ls_bapiret2-type = 'S'.
    ls_bapiret2-id   = 'ZSG'.
    ls_bapiret2-number = '000'.
    ls_bapiret2-message = ls_alv-kunnr && | Müşterisi başarıyla yaratıldı!|.
    ls_bapiret2-message_v1 = ls_alv-kunnr && | Müşterisi başarıyla yaratıldı!|.
    ls_bapiret2-message_v2 = ls_alv-kunnr && | Müşterisi başarıyla yaratıldı!|.
    ls_bapiret2-message_v3 = ls_alv-kunnr && | Müşterisi başarıyla yaratıldı!|.
    APPEND ls_bapiret2 TO lt_bapiret2.

    ls_alv-status = icon_action_success.
    ls_alv-t_messages = lt_bapiret2.
    ls_alv-process = 'U'.
    MODIFY gs_scr-1903-alv FROM ls_alv INDEX lv_tabix TRANSPORTING status t_messages process.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.


ENDFORM.                    " SD_CUSTOMER_MAINTAIN_ALL
*&---------------------------------------------------------------------*
*&      Form  CUSTOMER_UPDATE
*&---------------------------------------------------------------------*
FORM customer_update  USING ls_rows TYPE int4.
  DATA:   ls_kna1                TYPE kna1,
          ls_knb1                TYPE knb1,
          lv_kunnr               TYPE kna1-kunnr,
          ls_o_kna1              TYPE kna1,
          lv_e_sd_cust_1321_done TYPE c,
          ls_return              TYPE  bapireturn1 ,
          lv_field               TYPE bapi_fld ,
          lt_bapiret2            TYPE bapiret2_t,
          ls_bapiret2            TYPE bapiret2,
          ls_alv                 TYPE zsog_fi_001_s_002,
          lv_tabix               TYPE sy-tabix.

  DATA i_knvv  TYPE knvv.
  DATA i_ykna1 TYPE kna1.
  DATA i_yknb1 TYPE knb1.
  DATA t_xknas TYPE STANDARD TABLE OF fknas.
  DATA t_xknb5 TYPE STANDARD TABLE OF fknb5.
  DATA t_xknbk TYPE STANDARD TABLE OF fknbk.
  DATA t_xknva TYPE STANDARD TABLE OF fknva.
  DATA t_xknvd TYPE STANDARD TABLE OF fknvd.
  DATA t_xknvi TYPE STANDARD TABLE OF fknvi.
  DATA t_xknvk TYPE STANDARD TABLE OF fknvk.
  DATA t_xknvl TYPE STANDARD TABLE OF fknvl.
  DATA t_xknvp TYPE STANDARD TABLE OF fknvp.
  DATA t_xknvs TYPE STANDARD TABLE OF fknvs.
  DATA t_xknex TYPE STANDARD TABLE OF fknex.
  DATA t_xknza TYPE STANDARD TABLE OF fknza.
  DATA t_yknas TYPE STANDARD TABLE OF fknas.
  DATA t_yknb5 TYPE STANDARD TABLE OF fknb5.
  DATA t_yknbk TYPE STANDARD TABLE OF fknbk.
  DATA t_yknva TYPE STANDARD TABLE OF fknva.
  DATA t_yknvd TYPE STANDARD TABLE OF fknvd.
  DATA t_yknvi TYPE STANDARD TABLE OF fknvi.
  DATA t_yknvk TYPE STANDARD TABLE OF fknvk.
  DATA t_yknvl TYPE STANDARD TABLE OF fknvl.
  DATA t_yknvp TYPE STANDARD TABLE OF fknvp.
  DATA t_yknvs TYPE STANDARD TABLE OF fknvs.
  DATA t_yknex TYPE STANDARD TABLE OF fknex.
  DATA t_yknza TYPE STANDARD TABLE OF fknza.

  READ TABLE gs_scr-1903-alv INTO ls_alv INDEX ls_rows.
  lv_tabix = sy-tabix.
  IF ls_alv-process NE 'U'.
    RETURN.
  ENDIF.
  ls_kna1-mandt = sy-mandt.
  ls_kna1-kunnr = ls_alv-kunnr.
  ls_kna1-land1 = ls_alv-land1 .
  ls_kna1-name1 = ls_alv-name1 .
  ls_kna1-name2 = ls_alv-name2 .
  ls_kna1-ort01 = ls_alv-ort01 .
  ls_kna1-pstlz = ls_alv-pstlz .
  ls_kna1-regio = ls_alv-regio .
  ls_kna1-sortl = ls_alv-sortl .
  ls_kna1-stras = ls_alv-stras .
  ls_kna1-telf1 = ls_alv-telf1 .
  ls_kna1-mcod1 = ls_alv-mcod1 .
  ls_kna1-mcod3 = ls_alv-mcod3 .
  ls_kna1-brsch = ls_alv-brsch .
  ls_kna1-ktokd = ls_alv-ktokd .
  ls_kna1-ort02 = ls_alv-ort02 .
  ls_kna1-telf2 = ls_alv-telf2 .
  ls_kna1-katr2 = ls_alv-katr2 .
  ls_kna1-katr3 = ls_alv-katr3 .
  ls_kna1-katr4 = ls_alv-katr4 .
  ls_kna1-katr5 = ls_alv-katr5 .
  ls_kna1-katr6 = ls_alv-katr6 .
  ls_kna1-stcd1 = ls_alv-stcd1 .
  ls_kna1-stcd2 = ls_alv-stcd2 .
  ls_kna1-stcd3 = ls_alv-stcd3 .
  ls_kna1-konzs = ls_alv-konzs .

  ls_knb1-mandt = sy-mandt.
  ls_knb1-kunnr = ls_alv-kunnr.
  ls_knb1-bukrs = '2425'.
  ls_knb1-akont = ls_alv-akont.
  ls_knb1-zterm = ls_alv-zterm.
  ls_knb1-zsabe = ls_alv-zsabe.

  CALL FUNCTION 'CUSTOMER_UPDATE'
    EXPORTING
      i_kna1  = ls_kna1
      i_knb1  = ls_knb1
      i_knvv  = i_knvv
      i_ykna1 = i_ykna1
      i_yknb1 = i_yknb1
    TABLES
      t_xknas = t_xknas
      t_xknb5 = t_xknb5
      t_xknbk = t_xknbk
      t_xknva = t_xknva
      t_xknvd = t_xknvd
      t_xknvi = t_xknvi
      t_xknvk = t_xknvk
      t_xknvl = t_xknvl
      t_xknvp = t_xknvp
      t_xknvs = t_xknvs
      t_xknex = t_xknex
      t_xknza = t_xknza
      t_yknas = t_yknas
      t_yknb5 = t_yknb5
      t_yknbk = t_yknbk
      t_yknva = t_yknva
      t_yknvd = t_yknvd
      t_yknvi = t_yknvi
      t_yknvk = t_yknvk
      t_yknvl = t_yknvl
      t_yknvp = t_yknvp
      t_yknvs = t_yknvs
      t_yknex = t_yknex
      t_yknza = t_yknza
    EXCEPTIONS
      OTHERS  = 23.
  IF sy-subrc <> 0.
    CALL METHOD cl_msr_vrm_data=>convert_symessage2bapiret2(
      EXPORTING
        iv_field    = lv_field
      CHANGING
        ct_bapiret2 = lt_bapiret2 ).
    ls_alv-status = icon_action_fault.
    MODIFY gs_scr-1903-alv FROM ls_alv INDEX lv_tabix TRANSPORTING status t_messages.

  ELSE.
    ls_bapiret2-type = 'S'.
    ls_bapiret2-id   = 'ZSG'.
    ls_bapiret2-number = '000'.
    ls_bapiret2-message = ls_alv-kunnr && | Müşterisi başarıyla güncellendi!|.
    ls_bapiret2-message_v1 = ls_alv-kunnr && | Müşterisi başarıyla güncellendi!|.
    ls_bapiret2-message_v2 = ls_alv-kunnr && | Müşterisi başarıyla güncellendi!|.
    ls_bapiret2-message_v3 = ls_alv-kunnr && | Müşterisi başarıyla güncellendi!|.
    APPEND ls_bapiret2 TO lt_bapiret2.

    ls_alv-status = icon_action_success.
    ls_alv-t_messages = lt_bapiret2.
    MODIFY gs_scr-1903-alv FROM ls_alv INDEX lv_tabix TRANSPORTING status t_messages.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.
ENDFORM.                    " CUSTOMER_UPDATE
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
        ls_alv   TYPE zsog_fi_001_s_002.
  SORT gs_scr-1903-alv BY kunnr.
  IF gs_scr-1903-alv IS NOT INITIAL.
    SELECT kunnr FROM kna1 INTO TABLE lt_kunnr
                           FOR ALL ENTRIES IN gs_scr-1903-alv
                           WHERE kunnr = gs_scr-1903-alv-kunnr.
  ENDIF.
  LOOP AT lt_kunnr INTO ls_kunnr.
    READ TABLE gs_scr-1903-alv INTO ls_alv WITH KEY kunnr = ls_kunnr-kunnr BINARY SEARCH.
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
  DATA: ls_alv      TYPE zsog_fi_001_s_002.
  DATA: lo_alv TYPE REF TO cl_salv_table.

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
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " ON_LINK_CLICK
*&---------------------------------------------------------------------*
*&      Form  FILL_TAX_DATA
*&---------------------------------------------------------------------*
FORM fill_tax_data  USING    ps_alv     TYPE zsog_fi_001_s_002
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
FORM fill_adres_comcode  USING    ps_alv          TYPE zsog_fi_001_s_002
                                  pc_insert       TYPE char1
                         CHANGING ps_company_code TYPE cmds_ei_cmd_company
                                  ps_address      TYPE bapiad1vl
                                  ps_addressx     TYPE bapiad1vlx.

  DATA: ls_company_code_st TYPE cmds_ei_company.

  ps_address-title          = '0002.'.
  ps_address-name           = ps_alv-name1.
  ps_address-name_2         = ps_alv-name2.
  ps_address-city           = ps_alv-ort01.
  ps_address-district       = ps_alv-ort02.
  ps_address-country        = ps_alv-land1.
  ps_address-langu          = 'TR'.
  ps_address-region         = ps_alv-regio.         """COMNT
  ps_address-sort1          = ps_alv-name1.
  ps_address-street         = ps_alv-stras.
  ps_address-po_box         = ps_alv-pstlz.
  ps_address-postl_cod1     = ps_alv-pstlz.

  ps_addressx-title          = 'X'.
  ps_addressx-name           = 'X'.
  ps_addressx-name_2         = 'X'.
  ps_addressx-city           = 'X'.
  ps_addressx-district       = 'X'.
  ps_addressx-country        = 'X'.
  ps_addressx-langu          = 'X'.
  ps_addressx-region         = 'X'.
  ps_addressx-sort1          = 'X'.
  ps_addressx-street         = 'X'.
  ps_addressx-po_box         = 'X'.
  ps_addressx-postl_cod1     = 'X'.

  ls_company_code_st-task           = pc_insert.
  ls_company_code_st-data-akont     = ps_alv-akont.
  ls_company_code_st-data-zterm     = ps_alv-zterm.
  ls_company_code_st-datax-akont    = 'X'.
  ls_company_code_st-datax-zterm    = 'X'.
  ls_company_code_st-data_key-bukrs = '2425'.

*  WA_CUSTOMER-CENTRAL_DATA-ADDRESS-POSTAL-DATA  = WA_ADDRESS.
*  WA_CUSTOMER-CENTRAL_DATA-ADDRESS-POSTAL-DATAX = WA_ADDRESSX.
*  WA_COMPANY_CODE_ST-TASK                       = C_INSERT.
  APPEND ls_company_code_st TO ps_company_code-company.



ENDFORM.                    " FILL_ADRES_COMCODE
*&---------------------------------------------------------------------*
*&      Form  FILL_CENTRAL_DATA
*&---------------------------------------------------------------------*
FORM fill_central_data  USING    ps_alv     TYPE zsog_fi_001_s_002
                                 pc_insert  TYPE char1
                        CHANGING ps_data    TYPE cmds_ei_vmd_central_data
                                 ps_datax   TYPE cmds_ei_vmd_central_data_xflag.


  ps_data-ktokd = 'SG01'.
  ps_data-brsch = 'SG01'.
  ps_data-stcd1 = ps_alv-stcd1.
  ps_data-stcd2 = ps_alv-stcd2.
  ps_data-stcd3 = ps_alv-stcd3.
  ps_data-katr2 = ps_alv-katr2.
  ps_data-katr3 = ps_alv-katr3.
  ps_data-katr4 = ps_alv-katr4.
  ps_data-katr5 = ps_alv-katr5.
  ps_data-katr6 = ps_alv-katr6.

  ps_datax-ktokd = 'X'.
  ps_datax-brsch = 'X'.
  ps_datax-stcd1 = 'X'.
  ps_datax-stcd2 = 'X'.
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
FORM fill_tel_fax_mail  USING    ps_alv     TYPE zsog_fi_001_s_002
                                 pc_insert  TYPE char1
                        CHANGING pt_phone   TYPE cvis_ei_phone_t
                                 pt_fax     TYPE cvis_ei_fax_t
                                 pt_email   TYPE cvis_ei_smtp_t.

  DATA: ls_phone           TYPE cvis_ei_phone_str,
        ls_fax             TYPE cvis_ei_fax_str,
        ls_email           TYPE cvis_ei_smtp_str.

  ls_phone-contact-data-countryiso = 'TR'.
  ls_phone-contact-data-telephone  = ps_alv-adr_tel1_numbr.
  APPEND ls_phone TO pt_phone.

  ls_fax-contact-data-countryiso   = 'TR'.
  ls_fax-contact-data-fax          = ps_alv-adr_fax_number.
  APPEND ls_fax TO pt_fax.

  ls_email-contact-data-e_mail     = ps_alv-adr_e_mail.
  APPEND ls_email TO pt_email.


ENDFORM.                    " FILL_TEL_FAX_MAIL
*&---------------------------------------------------------------------*
*&      Form  FILL_SALES_DATA
*&---------------------------------------------------------------------*
FORM fill_sales_data  USING    ps_alv           TYPE zsog_fi_001_s_002
                               pc_insert        TYPE char1
                      CHANGING ps_sales_data_st TYPE cmds_ei_sales
                               ps_functions_t   TYPE cmds_ei_functions_t.

  DATA: ls_sales_data      TYPE cmds_ei_sales_data,
        ls_functions_st    TYPE cmds_ei_functions.

  ls_sales_data-zterm  = ps_alv-zterm.
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
  ls_functions_st-data-partner     = ps_alv-kunnr."'INTERNAL'.
  APPEND ls_functions_st TO  ps_functions_t.
  CLEAR: ls_functions_st.

  ls_functions_st-task = pc_insert.
  ls_functions_st-data_key-parvw  = 'WE'.
  ls_functions_st-data-defpa      = 'X'.
  ls_functions_st-data-partner    = ps_alv-kunnr."'INTERNAL'.
  APPEND ls_functions_st TO  ps_functions_t.
  CLEAR: ls_functions_st.

  ls_functions_st-task             = pc_insert.
  ls_functions_st-data_key-parvw   = 'RG'.
  ls_functions_st-data-defpa       = 'X'.
  ls_functions_st-data-partner     = ps_alv-kunnr."'INTERNAL'.
  APPEND ls_functions_st TO ps_functions_t.
  CLEAR: ls_functions_st.

  ls_functions_st-task = pc_insert.
  ls_functions_st-data_key-parvw  = 'RE'.
  ls_functions_st-data-defpa      = 'X'.
  ls_functions_st-data-partner    = ps_alv-kunnr."'INTERNAL'.
  APPEND ls_functions_st TO ps_functions_t.
  CLEAR: ls_functions_st.

  ps_sales_data_st-functions-current_state = 'X'.
  ps_sales_data_st-functions-functions = ps_functions_t.


ENDFORM.                    " FILL_SALES_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_CUSTOMER
*&---------------------------------------------------------------------*
FORM fill_customer  USING    ps_alv           TYPE zsog_fi_001_s_002
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
  ls_customer-header-object_instance-kunnr                   = ps_alv-kunnr.
  ls_customer-central_data-central-data-ktokd                = 'SG01'.
  ls_customer-header-object_instance-kunnr                   = ps_alv-kunnr.
  ls_customer-header-object_task                             = pc_insert.
  ls_customer-company_data                                   = ps_company_code.
  APPEND ls_customer TO ps_customers-customers.


ENDFORM.                    " FILL_CUSTOMER
*&---------------------------------------------------------------------*
*&      Form  CALL_CMD_EI_API
*&---------------------------------------------------------------------*
FORM call_cmd_ei_api  USING     ps_customers TYPE cmds_ei_main
                                pv_tabix     TYPE sy-tabix
                      CHANGING  ps_alv      TYPE zsog_fi_001_s_002.

  DATA:    ls_correct         TYPE cmds_ei_main,
           ls_defective       TYPE cmds_ei_main,
           ls_mes_correct     TYPE cvis_message,
           ls_mes_error       TYPE cvis_message,
           ls_bapiret2        TYPE bapiret2,
           lt_bapiret2        TYPE TABLE OF bapiret2.

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
    ls_bapiret2-message    = ps_alv-kunnr && | Müşterisi başarıyla yaratıldı/güncellendi!|.
    ls_bapiret2-message_v1 = ps_alv-kunnr && | Müşterisi başarıyla yaratıldı/güncellendi!|.
    ls_bapiret2-message_v2 = ps_alv-kunnr && | Müşterisi başarıyla yaratıldı/güncellendi!|.
    ls_bapiret2-message_v3 = ps_alv-kunnr && | Müşterisi başarıyla yaratıldı/güncellendi!|.
    APPEND ls_bapiret2 TO lt_bapiret2.

    ps_alv-status = icon_action_success.
    ps_alv-t_messages = lt_bapiret2.
    MODIFY gs_scr-1903-alv FROM ps_alv INDEX pv_tabix TRANSPORTING status t_messages.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ps_alv-status = icon_action_fault.
    ps_alv-t_messages = ls_mes_error-MESSAGES.
    MODIFY gs_scr-1903-alv FROM ps_alv INDEX pv_tabix TRANSPORTING status t_messages.
*    APPEND LINES OF ls_mes_error-MESSAGES TO E_MESSAGES.
  ENDIF.

ENDFORM.                    " CALL_CMD_EI_API
