*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_016_TOP
*&---------------------------------------------------------------------*
TYPE-POOLS:
  slis.

CONSTANTS: gc_bukrs TYPE bkpf-bukrs VALUE '2425'.

TABLES: ZSOG_FI_009.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001      .
SELECT-OPTIONS: s_tarih  FOR sy-datum
                NO-EXTENSION NO INTERVALS MODIF ID m1,
                s_accper FOR ZSOG_FI_009-accounting_period_no
                NO-EXTENSION NO INTERVALS MODIF ID m2,
                s_week   FOR ZSOG_FI_009-week_identifier
                NO-EXTENSION NO INTERVALS MODIF ID m2.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN COMMENT /1(50) text-002.
SELECTION-SCREEN ULINE /1(50).

PARAMETERS: r1 RADIOBUTTON GROUP a1 DEFAULT 'X' USER-COMMAND blabla,
            r2 RADIOBUTTON GROUP a1,
            ra RADIOBUTTON GROUP a1,
            rb RADIOBUTTON GROUP a1,
            r3 RADIOBUTTON GROUP a1,
            r4 RADIOBUTTON GROUP a1.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN COMMENT /1(50) text-003.
SELECTION-SCREEN ULINE /1(50).

PARAMETERS: r5 RADIOBUTTON GROUP a1,
            r6 RADIOBUTTON GROUP a1,
            rc RADIOBUTTON GROUP a1,
            rd RADIOBUTTON GROUP a1,
            r7 RADIOBUTTON GROUP a1,
            r8 RADIOBUTTON GROUP a1.
SELECTION-SCREEN   END OF BLOCK bl1                                .


DATA : BEGIN OF gt_out OCCURS 0,
       color(4)             TYPE c,
       icon                 LIKE icon-id,
       accounting_period_no LIKE zsog_fi_009-accounting_period_no,
       week_identifier      LIKE zsog_fi_009-week_identifier,
       retailer_no          LIKE zsog_fi_011-retailer_no,
       file_date            LIKE zsg_t_021-file_date,
       date_from            LIKE zsog_fi_009-date_from,
       date_to              LIKE zsog_fi_009-date_to,
       gross_sales_refunds  LIKE zsg_t_021-gross_sales_refunds,
       vat                  LIKE zsg_t_021-vat,
       net_amount           LIKE zsg_t_021-gross_sales_refunds,
       doc_amount           LIKE zsg_t_021-gross_sales_refunds,
       gjahr                LIKE bkpf-gjahr,
       belnr                LIKE bkpf-belnr,
       bukrs                LIKE bkpf-bukrs,
       fatura               LIKE zsog_fi_011-fatura,
       th                   LIKE zsog_fi_011-th,
       celltab              TYPE lvc_t_styl,
       cellcolor            TYPE lvc_t_scol,
       END OF gt_out,

       gs_out    LIKE gt_out,
       gt_secili LIKE gt_out OCCURS 0 WITH HEADER LINE.

DATA: gs_010        TYPE zsog_fi_010,
      gv_surec_tipi TYPE zsog_fi_010-surec_tipi,
      gs_009        TYPE zsog_fi_009.

DATA: gt_011        TYPE ZSOG_FI_011 OCCURS 0 WITH HEADER LINE.

"OBJECT ORIENTED ALV TANIMLARI
DATA : g_container         TYPE scrfname VALUE 'GRID',
       grid                TYPE REF TO cl_gui_alv_grid,
       g_custom_container  TYPE REF TO cl_gui_custom_container,
       gs_vari100          TYPE disvariant,
       gs_layo100          TYPE lvc_s_layo.
DATA : gt_fcat             TYPE lvc_t_fcat WITH HEADER LINE,
       gs_fcat             TYPE lvc_s_fcat,
       gt_sort             TYPE lvc_t_sort WITH HEADER LINE,
       gs_sort             TYPE lvc_s_sort,
       it_rows             TYPE lvc_t_row ,
       gt_rows             LIKE lvc_s_row OCCURS 0 WITH HEADER LINE.

"ALV TANIMLARI
DATA: gt_flcat   TYPE slis_t_fieldcat_alv,
      gt_layout  TYPE slis_layout_alv,
      gt_event   TYPE slis_t_event,
      gs_variant TYPE disvariant,
      gx_variant LIKE disvariant,
      gv_save(1) TYPE c VALUE 'A' .

DATA :v_alv_variant        TYPE disvariant.
DATA :gt_toolbar_excluding TYPE ui_functions.

DATA: gwa_flcat LIKE LINE OF gt_flcat,
      gwa_event LIKE LINE OF gt_event,
      gwa_sort  LIKE LINE OF gt_sort.

DATA: gv_okcode  TYPE sy-ucomm.
DATA: gv_error   TYPE c.
DATA: gv_answer  TYPE c.
DATA: gv_quest   TYPE char100.
DATA: gv_dummy   TYPE c,
      gt_message TYPE TABLE OF bapiret2 WITH HEADER LINE.

CLASS: lcl_event_receiver_0100 DEFINITION DEFERRED.
DATA : event_receiver_0100     TYPE REF TO lcl_event_receiver_0100.

DEFINE editable.
  clear ls_celltab.
  ls_celltab-fieldname = &1 .
  if &2 eq 'E' .
    ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
  else.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  endif.

  insert ls_celltab into table gt_out-celltab.
END-OF-DEFINITION .

*&---------------------------------------------------------------------*
*& CLASS lcl_event_receiver_0100 DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_0100 DEFINITION.

  PUBLIC SECTION.

    METHODS handle_right_click                  " RIGHT_CLICK
      FOR EVENT right_click OF cl_gui_alv_grid.


    METHODS handle_left_click_design            " LEFT_CLICK_DESIGN
      FOR EVENT left_click_design OF cl_gui_alv_grid.


    METHODS handle_move_control                 " MOVE_CONTROL
      FOR EVENT move_control OF cl_gui_alv_grid.


    METHODS handle_size_control                 " SIZE_CONTROL
      FOR EVENT size_control OF cl_gui_alv_grid.


    METHODS handle_left_click_run               " LEFT_CLICK_RUN
      FOR EVENT left_click_run OF cl_gui_alv_grid.


    METHODS handle_onf1                                     " ONF1
      FOR EVENT onf1 OF cl_gui_alv_grid
        IMPORTING
          e_fieldname
          es_row_no
          er_event_data.


    METHODS handle_onf4                                     " ONF4
      FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING
          e_fieldname
          e_fieldvalue
          es_row_no
          er_event_data
          et_bad_cells
          e_display.


    METHODS handle_data_changed                 " DATA_CHANGED
      FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          er_data_changed
          e_onf4
          e_onf4_before
          e_onf4_after
          e_ucomm.

    METHODS handle_ondropgetflavor              " ONDROPGETFLAVOR
      FOR EVENT ondropgetflavor OF cl_gui_alv_grid
        IMPORTING
          e_row
          e_column
          es_row_no
          e_dragdropobj
          e_flavors.


    METHODS handle_ondrag                       " ONDRAG
      FOR EVENT ondrag OF cl_gui_alv_grid
        IMPORTING
          e_row
          e_column
          es_row_no
          e_dragdropobj.


    METHODS handle_ondrop                       " ONDROP
      FOR EVENT ondrop OF cl_gui_alv_grid
        IMPORTING
          e_row
          e_column
          es_row_no
          e_dragdropobj.


    METHODS handle_ondropcomplete               " ONDROPCOMPLETE
      FOR EVENT ondropcomplete OF cl_gui_alv_grid
        IMPORTING
          e_row
          e_column
          es_row_no
          e_dragdropobj.


    METHODS handle_subtotal_text                " SUBTOTAL_TEXT
      FOR EVENT subtotal_text OF cl_gui_alv_grid
        IMPORTING
          es_subtottxt_info
          ep_subtot_line
          e_event_data.


    METHODS handle_before_user_command          " BEFORE_USER_COMMAND
      FOR EVENT before_user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.


    METHODS handle_user_command                 " USER_COMMAND
      FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.


    METHODS handle_after_user_command           " AFTER_USER_COMMAND
      FOR EVENT after_user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm
          e_not_processed.


    METHODS handle_double_click                 " DOUBLE_CLICK
      FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING
          e_row
          e_column
          es_row_no.


    METHODS handle_delayed_callback             " DELAYED_CALLBACK
      FOR EVENT delayed_callback OF cl_gui_alv_grid.


    METHODS handle_delayed_changed_sel_cal " DELAYED_CHANGED_SEL_CALLBACK
      FOR EVENT delayed_changed_sel_callback OF cl_gui_alv_grid.


    METHODS handle_print_top_of_page            " PRINT_TOP_OF_PAGE
      FOR EVENT print_top_of_page OF cl_gui_alv_grid
        IMPORTING
          table_index.


    METHODS handle_print_top_of_list            " PRINT_TOP_OF_LIST
      FOR EVENT print_top_of_list OF cl_gui_alv_grid.


    METHODS handle_print_end_of_page            " PRINT_END_OF_PAGE
      FOR EVENT print_end_of_page OF cl_gui_alv_grid.


    METHODS handle_print_end_of_list            " PRINT_END_OF_LIST
      FOR EVENT print_end_of_list OF cl_gui_alv_grid.


    METHODS handle_top_of_page                  " TOP_OF_PAGE
      FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
          e_dyndoc_id
          table_index.


    METHODS handle_context_menu_request         " CONTEXT_MENU_REQUEST
      FOR EVENT context_menu_request OF cl_gui_alv_grid
        IMPORTING
          e_object.


    METHODS handle_menu_button                  " MENU_BUTTON
      FOR EVENT menu_button OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_ucomm.


    METHODS handle_toolbar                      " TOOLBAR
      FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive.


    METHODS handle_hotspot_click                " HOTSPOT_CLICK
      FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
          e_row_id
          e_column_id.


    METHODS handle_end_of_list                  " END_OF_LIST
      FOR EVENT end_of_list OF cl_gui_alv_grid
        IMPORTING
          e_dyndoc_id.


    METHODS handle_after_refresh                " AFTER_REFRESH
      FOR EVENT after_refresh OF cl_gui_alv_grid.


    METHODS handle_button_click                 " BUTTON_CLICK
      FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no.

    METHODS handle_data_changed_finished        " DATA_CHANGED_FINISHED
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING
          e_modified
          et_good_cells.

  PRIVATE SECTION.

ENDCLASS.                    "lcl_event_receiver_0100 DEFINITION

*&---------------------------------------------------------------------*
*& CLASS lcl_event_receiver_0100 IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver_0100 IMPLEMENTATION.


  METHOD handle_right_click.
    "break xebudak.
  ENDMETHOD.                    "handle_right_click

  METHOD handle_left_click_design.
    "break xebudak.
  ENDMETHOD.                    "handle_left_click_design

  METHOD handle_move_control.
    "break xebudak.
  ENDMETHOD.                    "handle_move_control

  METHOD handle_size_control.
    "break xebudak.
  ENDMETHOD.                    "handle_size_control

  METHOD handle_left_click_run.
    "break xebudak.
  ENDMETHOD.                    "handle_left_click_run

  METHOD handle_onf1.
    "break xebudak.
  ENDMETHOD.                    "handle_onf1

  METHOD handle_onf4.
    "break xebudak.
  ENDMETHOD.                    "handle_onf4

  METHOD handle_data_changed.
     PERFORM f_handle_data_changed USING er_data_changed.
  ENDMETHOD.                    "handle_data_changed

  METHOD handle_ondropgetflavor.
    "break xebudak.
  ENDMETHOD.                    "handle_ondropgetflavor

  METHOD handle_ondrag.
    "break xebudak.
  ENDMETHOD.                    "handle_ondrag

  METHOD handle_ondrop.
    "break xebudak.
  ENDMETHOD.                    "handle_ondrop

  METHOD handle_ondropcomplete.
    "break xebudak.
  ENDMETHOD.                    "handle_ondropcomplete

  METHOD handle_subtotal_text.
    "break xebudak.
  ENDMETHOD.                    "handle_subtotal_text

  METHOD handle_before_user_command.
    "break xebudak.
  ENDMETHOD.                    "handle_before_user_command

  METHOD handle_user_command.
    "break xebudak.
  ENDMETHOD.                    "handle_user_command

  METHOD handle_after_user_command.
    "break xebudak.
  ENDMETHOD.                    "handle_after_user_command

  METHOD handle_double_click.
    "break xebudak.
  ENDMETHOD.                    "handle_double_click

  METHOD handle_delayed_callback.
    "break xebudak.
  ENDMETHOD.                    "handle_delayed_callback

  METHOD handle_delayed_changed_sel_cal.
    "break xebudak.
  ENDMETHOD.                    "handle_delayed_changed_sel_cal

  METHOD handle_print_top_of_page.
    "break xebudak.
  ENDMETHOD.                    "handle_print_top_of_page

  METHOD handle_print_top_of_list.
    "break xebudak.
  ENDMETHOD.                    "handle_print_top_of_list

  METHOD handle_print_end_of_page.
    "break xebudak.
  ENDMETHOD.                    "handle_print_end_of_page

  METHOD handle_print_end_of_list.
    "break xebudak.
  ENDMETHOD.                    "handle_print_end_of_list

  METHOD handle_top_of_page.
    "break xebudak.
  ENDMETHOD.                    "handle_top_of_page

  METHOD handle_context_menu_request.
    "break xebudak.
  ENDMETHOD.                    "handle_context_menu_request

  METHOD handle_menu_button.
    "break xebudak.
  ENDMETHOD.                    "handle_menu_button

  METHOD handle_toolbar.
**    "break xebudak.
*    DATA: ls_toolbar  TYPE stb_button.
*
** append a separator to normal toolbar
*    CLEAR ls_toolbar.
*    MOVE 3 TO ls_toolbar-butn_type.
*    APPEND ls_toolbar TO e_object->mt_toolbar.
*
*    CLEAR ls_toolbar.
*    MOVE 'BB_GUNLUK' TO ls_toolbar-function.
*    MOVE icon_system_save TO ls_toolbar-icon.
*    MOVE 'Baş bayi günlük tahakkuk belge'
*      TO ls_toolbar-quickinfo.
*    MOVE 'Baş bayi günlük tahakkuk yarat' TO ls_toolbar-text.
*    MOVE ' ' TO ls_toolbar-disabled.
*    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.                    "handle_toolbar

  METHOD handle_hotspot_click.
    "break xebudak.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD handle_end_of_list.
    "break xebudak.
  ENDMETHOD.                    "handle_end_of_list

  METHOD handle_after_refresh.
    "break xebudak.
  ENDMETHOD.                    "handle_after_refresh

  METHOD handle_button_click.
    "break xebudak.
  ENDMETHOD.                    "handle_button_click

  METHOD handle_data_changed_finished.
    "break xebudak.
  ENDMETHOD.                    "handle_data_changed_finished


ENDCLASS.                    "LCL_EVENT_RECEIVER_0100 IMPLEMENTATION
