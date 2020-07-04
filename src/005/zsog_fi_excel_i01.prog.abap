*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_EXCEL_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: lv_ucomm TYPE sy-ucomm.
  lv_ucomm = sy-ucomm.

  CASE lv_ucomm.
    WHEN '&BACK' OR '&CANC' OR '&ENDE'.
      LEAVE TO SCREEN 0.
    WHEN '&SAVE'.
      REFRESH gt_return.
      PERFORM save_document.
      IF gt_return[] IS NOT INITIAL.
        PERFORM show_logs .
      ENDIF.
      PERFORM refresh_grid.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
