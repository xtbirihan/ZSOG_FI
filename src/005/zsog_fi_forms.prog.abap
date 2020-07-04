*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ws_filename_open
*&---------------------------------------------------------------------*
*       Upload Open
*----------------------------------------------------------------------*
FORM ws_filename_open USING p_filename
                            p_default .

  DATA: file_table TYPE filetable,
        rc         TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title     = 'Select File'
      default_filename = p_default  " '*.xls'
      multiselection   = ''
    CHANGING
      file_table       = file_table
      rc               = rc.

  LOOP AT file_table INTO p_filename.  ENDLOOP.

ENDFORM.                    " ws_filename_open
*&---------------------------------------------------------------------*
*&      Form  SAP_GUI_MESSAGE
*&---------------------------------------------------------------------*
*       Status barda işlemlere dair mesaj verir.
*----------------------------------------------------------------------*
FORM sap_gui_message USING p_message
                           p_oran    TYPE i.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_oran
      text       = p_message.

ENDFORM.                    " SAP_GUI_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  SYS_ADD_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sys_add_bapiret2 TABLES pt_message STRUCTURE bapiret2 .

  DATA : ls_return TYPE bapiret2 .

  CALL FUNCTION 'FS_BAPI_BAPIRET2_FILL'
    EXPORTING
      type   = sy-msgty
      cl     = sy-msgid
      number = sy-msgno
      par1   = sy-msgv1
      par2   = sy-msgv2
      par3   = sy-msgv3
      par4   = sy-msgv4
    IMPORTING
      return = ls_return.
  APPEND ls_return TO pt_message.
ENDFORM.                    " SYS_ADD_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  SHOW_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM show_messages TABLES pt_message STRUCTURE bapiret2 .
  CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
    TABLES
      it_return = pt_message[].
ENDFORM.                    " SHOW_MESSAGES
*&---------------------------------------------------------------------*
*&      Form  BAPIRET_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bapiret_display  TABLES  pt_message STRUCTURE bapiret2 .

  CHECK pt_message[] IS NOT INITIAL .

  CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
    EXPORTING
      it_message = pt_message[].

ENDFORM.                    " BAPIRET_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_mail  TABLES   pt_objpack STRUCTURE sopcklsti1
                         pt_objhead STRUCTURE solisti1
                         pt_objtxt STRUCTURE solisti1
                         pt_reclist STRUCTURE somlreci1
                USING    ps_docdata TYPE sodocchgi1 .

  DATA : wa_docdata TYPE sodocchgi1,   " Document data
         wa_objbin  TYPE solisti1  ,   " Attachment data
         wa_objtxt  TYPE solisti1  ,   " Message body
         wa_objpack TYPE sopcklsti1,   " Packing list
         wa_reclist TYPE somlreci1 .   " Receipient list

  DATA : lv_lines TYPE i.

* Packing data
  DESCRIBE TABLE pt_objtxt LINES lv_lines.
  READ     TABLE pt_objtxt INTO pt_objtxt INDEX lv_lines.
  wa_docdata-doc_size = ( lv_lines - 1 ) * 255
                      + strlen( pt_objtxt ) .

  CLEAR wa_objpack-transf_bin.
  wa_objpack-head_start = 1.
  wa_objpack-head_num   = 0.
  wa_objpack-body_start = 1.
  wa_objpack-body_num   = lv_lines.
  wa_objpack-doc_type   = 'HTM'.
  APPEND wa_objpack TO pt_objpack.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = ps_docdata
      put_in_outbox              = 'X'
      commit_work                = 'X'     "used from rel.6.10
    TABLES
      packing_list               = pt_objpack
      object_header              = pt_objhead
      contents_txt               = pt_objtxt
      receivers                  = pt_reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

ENDFORM.                    " SEND_MAIL
*&---------------------------------------------------------------------*
*&      Form  GET_SSF_FUNCTION_MODULE_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_ssf_function_module_name  USING p_formname TYPE tdsfname
                                CHANGING p_fm_name  TYPE rs38l_fnam.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = p_formname
    IMPORTING
      fm_name            = p_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " GET_SSF_FUNCTION_MODULE_NAME
*&---------------------------------------------------------------------*
*&      Form  RS_TEXTPOOL_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM rs_textpool_read  USING pv_repid
                             pv_key
                    CHANGING pv_entry.
  DATA : lt_tpool TYPE TABLE OF textpool WITH HEADER LINE .

  CALL FUNCTION 'RS_TEXTPOOL_READ'
    EXPORTING
      objectname           = sy-repid
      action               = ' '
      authority_check      = ' '
      language             = sy-langu
    TABLES
      tpool                = lt_tpool
    EXCEPTIONS
      object_not_found     = 1
      permission_failure   = 2
      invalid_program_type = 3
      error_occured        = 4
      action_cancelled     = 5
      OTHERS               = 6.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE lt_tpool WITH KEY id  = 'S'
                               key = pv_key .
  IF sy-subrc EQ 0 .
    CONDENSE lt_tpool-entry .
    pv_entry = lt_tpool-entry .
  ENDIF.
ENDFORM.                    " RS_TEXTPOOL_READ
