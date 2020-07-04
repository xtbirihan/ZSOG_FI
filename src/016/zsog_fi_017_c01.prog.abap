*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_017_C01
*&---------------------------------------------------------------------*

CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA: go_eventreceiver2    TYPE REF TO lcl_event_receiver.
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_data_changed
                    FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_ucomm sender,

               hotspot_click
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no ,

      handle_double_click
                    FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,
      handle_close
                    FOR EVENT close OF cl_gui_dialogbox_container
        IMPORTING sender.
  PRIVATE SECTION.
    DATA: dialogbox_status TYPE c. "'X': does exist, SPACE: does notex.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_data_changed.

    PERFORM f_handle_data_changed CHANGING er_data_changed.

  ENDMETHOD.                    "handle_data_changed
  METHOD handle_close.

    CALL METHOD sender->set_visible
      EXPORTING
        visible = space.

  ENDMETHOD.                    "handle_close
  METHOD hotspot_click.
    DATA: lv_sat TYPE char10.

    CHECK e_row_id IS NOT INITIAL."C
    CLEAR gs_out."C
    READ TABLE gt_out INTO gs_out INDEX e_row_id ."C
    IF sy-subrc EQ 0."C
      IF e_column_id = 'KUNNR' .
        SET PARAMETER ID 'KUN' FIELD gs_out-kunnr.
        SET PARAMETER ID 'BUK' FIELD '2425'.
        CALL TRANSACTION 'XD03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "hotspot_click

  METHOD handle_double_click.

    CLEAR: gt_hata_tab, gv_line, gs_out.
    READ TABLE gt_out INDEX e_row-index
    INTO gs_out TRANSPORTING message.

    SPLIT gs_out-message AT ' / ' INTO TABLE gt_hata_tab.
    DESCRIBE TABLE gt_hata_tab LINES gv_line.

    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY_OK'
      EXPORTING
        endpos_col   = 120
        endpos_row   = 20
        startpos_col = 5
        startpos_row = 2
        titletext    = 'Hata Detayları'
      TABLES
        valuetab     = gt_hata_tab
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE s123(zmm) WITH 'Detaylar görüntülenemiyor.'
      DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.                    "handle_double_click
ENDCLASS.                    "lcl_
