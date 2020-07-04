*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_001_CUST_CREATE_TOP
*&---------------------------------------------------------------------*
*<<-- GLOBAL DEF
CONSTANTS: BEGIN OF c_texts,
             sample_file(20)          VALUE 'Ornek Dosya',
             sample_file_sscr(20)     VALUE 'Ornek Dosya',
             select_file_to_save(30)
               VALUE 'Kaydedilecek dosyayı seçiniz',
             error_uploading_file(20) VALUE 'Yüklerken Hata oluştu',
           END OF c_texts.

CONSTANTS: BEGIN OF c_status,
             success TYPE icon-id VALUE icon_okay,
             error   TYPE icon-id VALUE icon_led_red,
           END OF c_status.

CONSTANTS: BEGIN OF c_created,
             yes TYPE icon-id VALUE icon_customer,
             no  TYPE icon-id VALUE '',
           END OF c_created.

TABLES: sscrfields.
TYPE-POOLS: icon, slis.

DATA: BEGIN OF gt_data OCCURS 0.
        INCLUDE STRUCTURE zsog_fi_001_s_001.
DATA: t_messages TYPE bapiret2_t.
DATA: status LIKE icon-id.
DATA: created LIKE icon-id.
DATA: END OF gt_data.


DATA: BEGIN OF gs_scr.
DATA: BEGIN OF 1903.
DATA: ucomm            TYPE sy-ucomm.
DATA: error            TYPE char1.
DATA: alv              TYPE TABLE OF ZSOG_FI_001_S_002.
DATA: r_alv            TYPE REF TO cl_salv_table.
DATA: r_columns        TYPE REF TO cl_salv_columns_table.
DATA: r_column         TYPE REF TO cl_salv_column.
DATA: r_events         TYPE REF TO cl_salv_events_table.
DATA: r_selections     TYPE REF TO cl_salv_selections.
DATA: END OF 1903.
DATA: END OF gs_scr.

*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*
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
*
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_link_click.
    PERFORM on_link_click USING row column.
  ENDMETHOD.                    "on_link_click

  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.                    "on_user_command
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
