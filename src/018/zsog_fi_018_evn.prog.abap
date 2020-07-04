*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_018_EVN
*&---------------------------------------------------------------------*
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
*----------------------------------------------------------------------*
*       CLASS lcl_gui_alv_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui_alv_event_receiver DEFINITION.
  PUBLIC SECTION.

    METHODS:
      "        Hotspot click control
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.    "this will be passed each time your click on the cell

    METHODS:
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
            e_object e_interactive sender,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

    METHODS:
      handle_data_changed
                    FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_ucomm.


    METHODS: check_changed_key
      IMPORTING
        ps_good         TYPE lvc_s_modi
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.


  PRIVATE SECTION.
    DATA: error_in_data TYPE c.

    METHODS: check_matnr
      IMPORTING
        ps_good         TYPE lvc_s_modi
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS: check_tutar
      IMPORTING
        ps_good         TYPE lvc_s_modi
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.
ENDCLASS.                    "lcl_gui_alv_event_receiver DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_gui_alv_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui_alv_event_receiver IMPLEMENTATION.

  METHOD handle_hotspot_click .

    "Budak
*    DATA(ls_alv)  = gs_scr_1903-filter[ es_row_no-row_id ].
*    IF sy-subrc EQ 0.
*      CASE e_column_id  .
*        WHEN 'BELNR'.
*          SET PARAMETER ID 'BLN' FIELD ls_alv-belnr  .
*          SET PARAMETER ID 'BUK' FIELD ls_alv-bukrs  .
*          SET PARAMETER ID 'GJR' FIELD ls_alv-gjahr  .
*          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
*        WHEN 'KISMI_ODEME_ICON'.
**          PERFORM call_popup_alv USING es_row_no-row_id.
*        WHEN OTHERS.
*      ENDCASE.
*
*    ENDIF.

    "ozans
    DATA: ls_alv LIKE LINE OF gs_scr_1903-filter.
    CLEAR ls_alv.
    READ TABLE gs_scr_1903-filter INTO ls_alv INDEX es_row_no-row_id.
    IF sy-subrc EQ 0.
      CASE e_column_id  .
        WHEN 'BELNR'.
          SET PARAMETER ID 'BLN' FIELD ls_alv-belnr  .
          SET PARAMETER ID 'BUK' FIELD ls_alv-bukrs  .
          SET PARAMETER ID 'GJR' FIELD ls_alv-gjahr  .
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
        WHEN 'KISMI_ODEME_ICON'.
*          PERFORM call_popup_alv USING es_row_no-row_id.
        WHEN OTHERS.
      ENDCASE.

    ENDIF.

  ENDMETHOD .                    "handle_hotspot_click

  METHOD handle_toolbar.
*{   ->>> Inserted by Prodea Ozan Şahin - 13.06.2020 13:36:49
    PERFORM f_handle_toolbar USING e_object e_interactive sender.
*}     <<<- End of   Inserted - 13.06.2020 13:36:49


  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'YENI'.
*        PERFORM yeni_kayit.
      WHEN 'KAYIT'.
*        CLEAR: gs_scr-1903-error.
      WHEN 'SIL'.
*        PERFORM sil.
      WHEN 'IMP_CSV'.
*{   ->>> Inserted by Prodea Ozan Şahin - 13.06.2020 13:39:54
      WHEN 'SATIRSIL'.
        PERFORM satici_satir_sil.
*}     <<<- End of   Inserted - 13.06.2020 13:39:54
    ENDCASE.
    IF gs_scr_1903-r_grid1 IS BOUND .
      PERFORM refresh_table_display.
    ENDIF.

  ENDMETHOD.                    "handle_user_command
  METHOD handle_data_changed.

    IF gs_scr_1903-r_grid1 IS BOUND .
      PERFORM refresh_table_display.
    ENDIF.
*    ENDLOOP.
  ENDMETHOD.                    "handle_data_changed
  METHOD check_changed_key.

  ENDMETHOD.                    "check_changed_key

  METHOD check_tutar.


  ENDMETHOD.                    "check_tutar

  METHOD check_matnr.

  ENDMETHOD.                    "check_matnr

ENDCLASS.                    "lcl_gui_alv_event_receiver IMPLEMENTATION

DATA: gr_alv_event_ref TYPE REF TO lcl_gui_alv_event_receiver.
