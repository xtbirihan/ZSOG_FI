*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_002_BAYI_VADE_TOP
*&---------------------------------------------------------------------*
TABLES: bsid.

*- Itab tanımları
DATA: BEGIN OF gs_blart,
      blart TYPE blart,
      END OF gs_blart,
      gt_blart LIKE TABLE OF gs_blart.

DATA: gt_rate TYPE TABLE OF zsog_fi_002_c002,
      gs_rate TYPE zsog_fi_002_c002.

DATA: gt_msg TYPE bal_t_msg,
      gs_msg TYPE bal_s_msg.

*- Var. tanımları
DATA: gv_error,
      gv_msg TYPE text200.

*- ALV tanımları
DATA: g_container          TYPE scrfname VALUE 'CONTAINER',
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      grid                 TYPE REF TO cl_gui_alv_grid,
      gs_layout            TYPE lvc_s_layo,
      gt_flcat             TYPE slis_t_fieldcat_alv,
      gs_flcat             LIKE LINE OF gt_flcat,
      gt_fcat              TYPE lvc_t_fcat WITH HEADER LINE,
      gs_fcat              TYPE lvc_s_fcat,
      gs_variant           TYPE disvariant,
      gx_variant           TYPE disvariant,
      gv_save              VALUE 'A',
      gs_toolbar           TYPE stb_button,
      gt_toolbar_excluding TYPE ui_functions,
      gs_stable            TYPE lvc_s_stbl VALUE 'XX'.

*- Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
SELECT-OPTIONS: s_kunnr FOR bsid-kunnr,
                s_blart FOR bsid-blart.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF BLOCK b1.
