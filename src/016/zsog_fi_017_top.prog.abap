*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_017_TOP
*&---------------------------------------------------------------------*

TABLES: sscrfields.
DATA : gs_file  LIKE alsmex_tabline.
DATA : gt_file  TYPE TABLE OF alsmex_tabline.
DATA : gt_out  TYPE TABLE OF ZFI_S_VA7.
DATA : gs_out  TYPE ZFI_S_VA7.

DATA: gr_alvgrid             TYPE REF TO cl_gui_alv_grid,
      gc_custom_control_name TYPE scrfname VALUE 'SCREEN',
      gr_container           TYPE REF TO cl_gui_custom_container,
      gt_fieldcat            TYPE lvc_t_fcat,
      gs_fieldcat            TYPE lvc_s_fcat,
      gs_layout              TYPE lvc_s_layo,
      gs_row_no              TYPE lvc_s_roid,
      gt_row_no              TYPE lvc_t_roid,
      gt_exclude             TYPE ui_functions,
      ok_code                TYPE sy-ucomm,
      gt_exit(1),
      gv_onay(1) ,
      gv_hata(1).

TYPES: ty_line(2500) TYPE c.
DATA: gt_text  TYPE truxs_t_text_data.
DATA: lv_a TYPE c.
DATA: it_tab1 TYPE TABLE OF ty_line,
      it_tab2 TYPE TABLE OF ty_line,
      wa_tab  TYPE ty_line.

DATA: gt_hata_tab  TYPE truxs_t_text_data.
DATA: gv_line      TYPE i.


DATA: w_excel      TYPE ole2_object,
      w_workbook   TYPE ole2_object,
      w_worksheet  TYPE ole2_object,
      w_columns    TYPE ole2_object,
      w_column_ent TYPE ole2_object,
      w_cell       TYPE ole2_object,
      w_int        TYPE ole2_object,
      w_range      TYPE ole2_object.

DATA gv_error TYPE c."genel hata flag değişken.

DATA: BEGIN OF gs_hatalar,
        hata(500),
      END OF gs_hatalar,
      gt_hatalar LIKE TABLE OF gs_hatalar.

DATA: w_deli(1) TYPE c, "Delimiter
      w_hex     TYPE x,
      w_rc      TYPE i.
DATA : it_raw  TYPE truxs_t_text_data.
FIELD-SYMBOLS: <fs> .
CONSTANTS wl_c09(2) TYPE n VALUE 09.

CONSTANTS: c_ext_xls TYPE string VALUE '*.xlsx'.

SELECTION-SCREEN BEGIN OF BLOCK b102 WITH FRAME TITLE TEXT-002 .
SELECTION-SCREEN FUNCTION KEY 1.
PARAMETERS : p_file TYPE rlgrap-filename MODIF ID a.
SELECTION-SCREEN END OF BLOCK b102.
SELECTION-SCREEN BEGIN OF LINE .
SELECTION-SCREEN: PUSHBUTTON (20) TEXT-001 USER-COMMAND cl1 MODIF ID m1.
SELECTION-SCREEN END OF LINE .
