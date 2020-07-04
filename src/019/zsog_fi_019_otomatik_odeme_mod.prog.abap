*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_019_OTOMATIK_ODEME_MOD
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '001'.

  PERFORM show_alv .
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code100.
    WHEN 'BACK' OR 'CANC' OR 'ENDE' .
      LEAVE TO SCREEN 0 .
    WHEN '&EXCEL'.
      PERFORM write_excel.
    WHEN '&TXT'.
    PERFORM download_txt.
  ENDCASE .
ENDMODULE.                 " USER_COMMAND_0100  INPUT
