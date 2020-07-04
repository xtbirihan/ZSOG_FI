*&---------------------------------------------------------------------*
*&  Include           ZFI_I_PESIN_ODENEN_GID_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: lv_value TYPE sval-value.

  CASE ok_code .
    WHEN 'BACK' OR '&F15' OR '&F12' .
      SET SCREEN 0 .
    WHEN 'RW' .
      LEAVE PROGRAM .
    WHEN 'SAVE' .
      PERFORM save .
    WHEN '&SIMUL' .
      PERFORM taksit_tablosu_olustur .
      PERFORM simulate .
    WHEN '&GUNLUK' .

      CALL METHOD alvgrid->check_changed_data
        IMPORTING
          e_valid = e_valid.

      CLEAR : lt_t_fieldcatalog , lt_t_fieldcatalog[] .
      v_default_recname = 'IT_LOG' .
      v_default_report_name = sy-repid .
      PERFORM set_report_fcat.
      PERFORM show_report_fcat_pop TABLES it_log
                          USING  '' "P_VARI
                                 gs_variant
                                 v_default_report_name
                                 v_default_recname.
      v_default_recname = 'GT_DATA' .
    WHEN '&TERS'.
      PERFORM popup_get_value CHANGING gv_belgeno.
      CHECK gv_belgeno IS NOT INITIAL.
      PERFORM get_header_items USING gv_belgeno.
      CHECK gt_200[] IS NOT INITIAL.
      CHECK gt_200_belge[] IS NOT INITIAL.
      LEAVE TO SCREEN 200.
*      IF sy-subrc = 0.
*        MESSAGE e029(zco) WITH gv_belgeno.
*
*      ENDIF.
  ENDCASE .

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_FILENAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*&      Module  GET_FILENAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_filename INPUT.

  PERFORM ws_filename_get USING filename .

ENDMODULE.                 " GET_FILENAME  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  DATA: lv_okcode TYPE sy-ucomm,
        lv_answer TYPE c,
        lv_error  TYPE c.

  CLEAR: lv_okcode.
  lv_okcode = gv_okcode200.

  CASE lv_okcode.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      PERFORM you_sure USING 'Ters kayıt ekranı'
                    CHANGING lv_answer.
      CHECK lv_answer = '1'.
      SELECT saknr txt20
        FROM skat
        INTO TABLE it_skat
        WHERE spras EQ sy-langu AND
              ktopl EQ 'SGHP' AND
              ( saknr LIKE '180%' OR
                saknr LIKE '280%' OR
* begin of insert Ali Y. Abbasgil 10.06.2013 15:53:43
                saknr LIKE '360%' OR
*    end of insert.
                saknr LIKE '191%' ).
      SORT it_skat BY saknr .

      CLEAR : it_style_kapali , wa_style .
      wa_style-fieldname = '' .
      wa_style-style = cl_gui_alv_grid=>mc_style_disabled."Kapat
      APPEND wa_style TO it_style_kapali .

      bkpf-blart = 'KR' .
      bkpf-bukrs = '2425' .
      LEAVE TO SCREEN 100.
    WHEN 'SAVE'.
      PERFORM check_changed_data USING grid2.
      PERFORM get_tarih CHANGING gv_tarih_200
                                 gv_tarih_200_post.
      CHECK gv_tarih_200 IS NOT INITIAL
        AND gv_tarih_200 NE SPACE.
      CHECK gv_tarih_200_post IS NOT INITIAL
        AND gv_tarih_200_post NE SPACE.
      PERFORM control_data CHANGING lv_error.
      CHECK lv_error EQ space.
      PERFORM ters_kayit CHANGING lv_error.
      PERFORM bapiret_display  TABLES gt_message .
      IF lv_error = space.
        SELECT saknr txt20
          FROM skat
          INTO TABLE it_skat
          WHERE spras EQ sy-langu AND
                ktopl EQ 'SGHP' AND
                ( saknr LIKE '180%' OR
                  saknr LIKE '280%' OR
* begin of insert Ali Y. Abbasgil 10.06.2013 15:53:43
                  saknr LIKE '360%' OR
*    end of insert.
                  saknr LIKE '191%' ).
        SORT it_skat BY saknr .

        CLEAR : it_style_kapali , wa_style .
        wa_style-fieldname = '' .
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled."Kapat
        APPEND wa_style TO it_style_kapali .

        bkpf-blart = 'KR' .
        bkpf-bukrs = '2425' .

        LEAVE TO SCREEN 100.
      ENDIF.
  ENDCASE.

ENDMODULE.
