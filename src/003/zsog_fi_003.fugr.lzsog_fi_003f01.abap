*----------------------------------------------------------------------*
***INCLUDE LZSOG_FI_003F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_MESSAGE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_RETURN_MESSAGE  text
*----------------------------------------------------------------------*
FORM get_message_text  CHANGING ev_message TYPE bapi_msg..
  CLEAR ev_message.
  CALL FUNCTION 'FORMAT_MESSAGE'
    EXPORTING
      id        = sy-msgid
      lang      = 'TR'
      no        = sy-msgno
      v1        = sy-msgv1
      v2        = sy-msgv2
      v3        = sy-msgv3
      v4        = sy-msgv4
    IMPORTING
      msg       = ev_message
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
ENDFORM.                    " GET_MESSAGE_TEXT
