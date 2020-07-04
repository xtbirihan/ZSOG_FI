*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_006_CUST_CREATE_TOP
*&---------------------------------------------------------------------*

TABLES: sscrfields.

CONSTANTS: BEGIN OF c_rmf_data_export,
             sample_file(20)          VALUE 'Örnek RMF Data Exp',
             sample_file_sscr(20)     VALUE 'Örnek RMF Data Exp',
             select_file_to_save(30)  VALUE 'Kaydedilecek dosyayı seçiniz',
             error_uploading_file(20) VALUE 'Yüklerken Hata oluştu',
           END OF c_rmf_data_export.

CONSTANTS: BEGIN OF c_rlf_data_export,
            sample_file(20)          VALUE 'Örnek RLF Data Export',
            sample_file_sscr(20)     VALUE 'Örnek RLF Data Export',
            select_file_to_save(30)  VALUE 'Kaydedilecek dosyayı seçiniz',
            error_uploading_file(20) VALUE 'Yüklerken Hata oluştu',
          END OF c_rlf_data_export.

DATA: BEGIN OF gs_scr.
DATA: BEGIN OF 1903.
DATA: ucomm            TYPE sy-ucomm.
DATA: error            TYPE char1.
DATA: alv              TYPE TABLE OF ZSOG_FI_001_S_05.
DATA: r_alv            TYPE REF TO cl_salv_table.
DATA: r_columns        TYPE REF TO cl_salv_columns_table.
DATA: r_column         TYPE REF TO cl_salv_column.
DATA: r_events         TYPE REF TO cl_salv_events_table.
DATA: r_selections     TYPE REF TO cl_salv_selections.
DATA: END OF 1903.
DATA: END OF gs_scr.

  TYPES: BEGIN OF gtt_datab,
          fields TYPE string,
        END OF gtt_datab.
  DATA: gt_rmf TYPE TABLE OF gtt_datab.
  DATA: gt_rlf TYPE TABLE OF gtt_datab.

*FIELD-SYMBOLS: <rmf_tab>   TYPE STANDARD TABLE ,
*               <rmf_line>  TYPE any,
*               <rlf_tab>   TYPE STANDARD TABLE ,
*               <rlf_line>  TYPE any.


*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS: on_link_click   FOR EVENT link_click
                  OF cl_salv_events_table
      IMPORTING row column,

      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.                    "lcl_handle_events DEFINITION

DATA: event_handler TYPE REF TO lcl_handle_events.
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_link_click.
    PERFORM on_link_click USING row column.
  ENDMETHOD.                    "on_link_click

  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.                    "on_user_command
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
