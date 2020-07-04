*&---------------------------------------------------------------------*
*&  Include           ZSOG_SD_OBJECT_ALV
*&---------------------------------------------------------------------*

*- Include
INCLUDE <icon>.

*- Type-Pools
TYPE-POOLS : slis, icon.

*- Class
CLASS lcl_event_receiver DEFINITION DEFERRED.

*- Data
DATA: ok_code            LIKE sy-ucomm    ,
      gi_index_rows      TYPE lvc_t_row   ,
      g_selected_row     LIKE lvc_s_row   ,
      ld_count           TYPE i           ,
      g_container        TYPE scrfname VALUE 'C_CONT_100'     ,
      grid               TYPE REF TO cl_gui_alv_grid          ,
      grid3              TYPE REF TO cl_gui_alv_grid          ,
      g_custom_container TYPE REF TO cl_gui_custom_container  ,
      g_event_receiver   TYPE REF TO lcl_event_receiver       ,
      grid_popup         TYPE REF TO cl_gui_alv_grid          ,
      grid_mes           TYPE REF TO cl_gui_alv_grid          ,
      grid_file          TYPE REF TO cl_gui_alv_grid          ,
      g_p_container      TYPE scrfname VALUE 'C_CONT_101'     ,
      g_p_container2     TYPE scrfname VALUE 'C_CONT_102'     ,
      g_p_container3     TYPE scrfname VALUE 'C_CONT_103'     ,
      g_cust_con2        TYPE REF TO cl_gui_custom_container  ,
      g_cust_con1        TYPE REF TO cl_gui_custom_container  ,
      g_cust_con3        TYPE REF TO cl_gui_custom_container  ,
      gt_fieldcat        TYPE lvc_t_fcat  ,
      gt_fieldcat2       TYPE lvc_t_fcat  ,
      gt_fieldcat3       TYPE lvc_t_fcat  ,
      gt_fieldcat4       TYPE lvc_t_fcat  ,
      gs_fieldcat        TYPE lvc_s_fcat  ,
      gt_exclude         TYPE ui_functions,
      gs_layout          TYPE lvc_s_layo  ,
      gs_variant         TYPE disvariant  ,
      gx_variant         LIKE disvariant  ,
      gv_save(1)         TYPE c VALUE 'A' .

DATA : gs_toolbar  TYPE stb_button.
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS : handle_toolbar
                      FOR EVENT toolbar OF cl_gui_alv_grid
                          IMPORTING e_object e_interactive  ,

                    handle_user_command
                          FOR EVENT user_command OF cl_gui_alv_grid
                          IMPORTING e_ucomm                 ,

                    double_click
                          FOR EVENT double_click OF cl_gui_alv_grid
                          IMPORTING e_row
                                    e_column
                                    es_row_no               ,

                   handle_data_changed
                          FOR EVENT data_changed OF cl_gui_alv_grid
                          IMPORTING er_data_changed
                                    e_onf4
                                    e_onf4_before
                                    e_onf4_after
                                    e_ucomm ,
                  handle_hotspot_click
                        FOR EVENT hotspot_click OF cl_gui_alv_grid
                        IMPORTING e_row_id e_column_id,
                  handle_onf4
                        FOR EVENT onf4 OF cl_gui_alv_grid
                        IMPORTING e_fieldname
                                  e_fieldvalue
                                  es_row_no
                                  er_event_data
                                  et_bad_cells
                                  e_display.
  PRIVATE SECTION.

ENDCLASS.                    "lcl_event_receiver DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
*- Handle Toolbar
  METHOD handle_toolbar.
  ENDMETHOD.                    "handle_toolbar
*- Handle User Command
  METHOD handle_user_command .
  ENDMETHOD.                           "handle_user_command
*- Double Click
  METHOD double_click.
  ENDMETHOD.                    "double_click
*- Handle Data Changed
  METHOD handle_data_changed.
    PERFORM handle_data_changed USING er_data_changed.
  ENDMETHOD.                    "handle_data_changed
*- Handle hotspotclicl
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING e_row_id e_column_id.
  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK
*- Handle onf4
  METHOD handle_onf4.
    PERFORM on_f4 USING e_fieldname  e_fieldvalue es_row_no
er_event_data
                        et_bad_cells e_display.
    er_event_data->m_event_handled = 'X' . "disabling standard f4

  ENDMETHOD.                    "handle_onf4
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*                       Macro Field Catalog
*----------------------------------------------------------------------*
DEFINE create_fcat .
  gs_fieldcat-scrtext_l  = &1           .
  gs_fieldcat-scrtext_m  = &1           .
  gs_fieldcat-scrtext_s  = &1           .
  gs_fieldcat-fieldname  = &2           .
  gs_fieldcat-lowercase  = 'X'          .
  gs_fieldcat-outputlen  = &3           .
  gs_fieldcat-emphasize  = ''           .
  gs_fieldcat-fix_column = 'X'          .
  gs_fieldcat-no_zero    = 'X'          .
  gs_fieldcat-ref_table  = &4           .
  gs_fieldcat-ref_field  = &5           .
  gs_fieldcat-hotspot    = &6           .
  gs_fieldcat-no_out     = &7           .
  gs_fieldcat-col_pos    = &8           .
  gs_fieldcat-edit       = &9           .
  if gs_fieldcat-fieldname eq 'COLOR_LINE'.
    gs_fieldcat-icon     = 'X'.
  elseif gs_fieldcat-fieldname eq 'K_HKONT'.
    gs_fieldcat-f4availabl = 'X'.
  elseif gs_fieldcat-fieldname eq 'KOSTL'.
*    gs_fieldcat-f4availabl = 'X'.
  elseif gs_fieldcat-fieldname eq 'AUFNR'.
*    gs_fieldcat-f4availabl = 'X'.
  elseif gs_fieldcat-fieldname eq 'UMSKZ'.
*    gs_fieldcat-f4availabl = 'X'.
  elseif gs_fieldcat-fieldname eq 'MWSKZ'.
*    gs_fieldcat-f4availabl = 'X'.
  elseif gs_fieldcat-fieldname eq 'PASIF'.
    gs_fieldcat-checkbox   = 'X'.
  elseif gs_fieldcat-fieldname eq 'ZCHECK'.
    gs_fieldcat-checkbox   = 'X'.
  elseif gs_fieldcat-fieldname eq 'ZUONR'.
*    gs_fieldcat-lzero   = ' '.
*    gs_fieldcat-no_zero   = ' '.
  endif.
  append gs_fieldcat to gt_fieldcat     .
  clear  gs_fieldcat  .
END-OF-DEFINITION  .

DEFINE change_text_fcat               .
  loop at gt_fieldcat into gs_fieldcat
                  where fieldname eq &1 .
    gs_fieldcat-scrtext_l = &2 .
    gs_fieldcat-scrtext_m = &2 .
    gs_fieldcat-scrtext_s = &2 .
    gs_fieldcat-reptext   = &2 .
    modify gt_fieldcat from gs_fieldcat   .
  endloop.
END-OF-DEFINITION                     .

DEFINE add_buton .
  clear : gs_toolbar .
  gs_toolbar-butn_type = 3 .
  append gs_toolbar to e_object->mt_toolbar.
  clear : gs_toolbar .
  gs_toolbar-function  = &1 .
  gs_toolbar-icon      = &2 .
  gs_toolbar-quickinfo = &3 .
  gs_toolbar-text      = &3 .
  gs_toolbar-disabled  = &4 .
  gs_toolbar-butn_type = &5 .
  append gs_toolbar to e_object->mt_toolbar.
END-OF-DEFINITION .
*&---------------------------------------------------------------------*
*&      Form  fill_alv_layout
*&---------------------------------------------------------------------*
*       Çıktı ile İlgili Düzenlemeler
*----------------------------------------------------------------------*
FORM fill_alv_layout .
  CLEAR gs_layout.
  gs_layout-sel_mode    = 'D'  .
  gs_layout-zebra       = 'X'  .
  gs_layout-edit_mode   = 'X'  .
  gs_layout-stylefname  = 'EDIT_LINE'.
ENDFORM.                    " fill_alv_layout

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_fieldcat USING p_structure TYPE dd02l-tabname
                 CHANGING pt_fieldcat TYPE lvc_t_fcat .

  DATA : ls_fcat     TYPE lvc_s_fcat .

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name   = p_structure
      i_bypassing_buffer = 'X'
    CHANGING
      ct_fieldcat        = pt_fieldcat.

ENDFORM.                    " build_fieldcat

*----------------------------------------------------------------------*
*       FORM list_merge_fieldcat                                       *
*----------------------------------------------------------------------*
*       FieldCatalog Oluşturulması
*----------------------------------------------------------------------*
FORM merge_fieldcat USING p_tabname TYPE slis_tabname.


  DATA : lt_field TYPE slis_t_fieldcat_alv WITH HEADER LINE .

  CLEAR:gt_fieldcat,gt_fieldcat[].

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = p_tabname
      i_inclname             = sy-repid
      i_client_never_display = 'X'
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = lt_field[]
    EXCEPTIONS
      OTHERS                 = 3.

  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  LOOP AT lt_field .
    MOVE-CORRESPONDING lt_field TO gs_fieldcat .
    MOVE : lt_field-seltext_l     TO gs_fieldcat-scrtext_l ,
           lt_field-seltext_m     TO gs_fieldcat-scrtext_m ,
           lt_field-seltext_s     TO gs_fieldcat-scrtext_s ,
           lt_field-reptext_ddic  TO gs_fieldcat-seltext   ,
           lt_field-ref_fieldname TO gs_fieldcat-ref_field ,
           lt_field-ref_tabname   TO gs_fieldcat-ref_table .
    APPEND gs_fieldcat TO gt_fieldcat . CLEAR : gs_fieldcat .
  ENDLOOP.

ENDFORM.                    "list_merge_fieldcat
*&---------------------------------------------------------------------*
*&      Form  SET_TABLE_FOR_FIRST_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_table_for_first_display
                   TABLES it_outtab   TYPE STANDARD TABLE
                   USING  p_grid      TYPE REF TO  cl_gui_alv_grid
                          p_type      TYPE c.

  DATA: lt_f4 TYPE lvc_t_f4 WITH HEADER LINE,
        ls_f4 TYPE lvc_s_f4.


* F4 için eklenecek alan - field category - f4availabl X olmalı
  lt_f4-fieldname  = 'AUFNR'.
  lt_f4-register   = 'X' .
  lt_f4-getbefore  = 'X' .
  lt_f4-chngeafter = 'X' .
  APPEND lt_f4.CLEAR lt_f4.

  lt_f4-fieldname  = 'K_HKONT'.
  lt_f4-register   = 'X' .
  lt_f4-getbefore  = 'X' .
  lt_f4-chngeafter = 'X' .
  APPEND lt_f4.CLEAR lt_f4.


  DATA: it_fieldcat TYPE lvc_t_fcat.

  CLEAR:it_fieldcat[],it_fieldcat.
  IF p_type EQ 'P'.
    it_fieldcat[] = gt_fieldcat2[].
  ELSEIF p_type EQ 'K'.
    it_fieldcat[] = gt_fieldcat3[].
  ELSEIF p_type EQ 'S'.
    it_fieldcat[] = gt_fieldcat4[].
  ELSE.
    it_fieldcat[] = gt_fieldcat[].
  ENDIF.


  CALL METHOD p_grid->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].

* Register the method
  CALL METHOD p_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*  PERFORM build_data.
* Set editable cells to ready for input initially
  CALL METHOD p_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.
  gs_variant-report = sy-repid.

  CALL METHOD p_grid->set_table_for_first_display
    EXPORTING
      is_layout            = gs_layout
      it_toolbar_excluding = gt_exclude
      is_variant           = gs_variant
      i_bypassing_buffer   = 'X'
      i_save               = 'A'
    CHANGING
      it_fieldcatalog      = it_fieldcat
      it_outtab            = it_outtab[].

ENDFORM.                    "set_table_for_first_display
*&---------------------------------------------------------------------*
*&      Form  set_event_receiver
*&---------------------------------------------------------------------*
*       eventleri ata
*----------------------------------------------------------------------*
FORM set_event_receiver USING p_grid TYPE REF TO  cl_gui_alv_grid.
  IF p_grid IS NOT INITIAL.
    CREATE OBJECT g_event_receiver.
    SET HANDLER g_event_receiver->handle_hotspot_click FOR p_grid.
    SET HANDLER g_event_receiver->handle_onf4 FOR p_grid.
    SET HANDLER g_event_receiver->handle_data_changed FOR p_grid.

  ENDIF.

ENDFORM.                    "set_event_receiver
