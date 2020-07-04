*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_017_I01
*&---------------------------------------------------------------------*

MODULE user_command_0100 INPUT.

  DATA lv_okcode TYPE sy-ucomm.

  lv_okcode = ok_code.
  CLEAR ok_code.
  CASE lv_okcode.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'KAYDET'.

      PERFORM get_onay USING text-015 .
      CHECK gv_onay = '1' .
      PERFORM log_kaydet.
      PERFORM kaydet.
  ENDCASE.
  CLEAR lv_okcode.
ENDMODULE.                    "user_command_0100 INPUT
