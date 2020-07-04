*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_016_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: lv_okcode TYPE sy-ucomm,
        lv_quest(100) TYPE c.

  CLEAR: lv_okcode.

  lv_okcode = gv_okcode.

  CLEAR: gv_okcode.

  CASE lv_okcode.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'BB_GUNLUK' OR 'SB_GUNLUK'.

      PERFORM get_selected_rows TABLES gt_secili.

      PERFORM check_selected_rows TABLES gt_secili
                                   USING lv_okcode
                                CHANGING gv_error.

      CHECK gv_error = space.

      CLEAR: lv_quest.
      CONCATENATE 'Günlük tahakkuk belgesi yaratılacaktır,'
                  'emin misiniz?'
              INTO lv_quest SEPARATED BY space.
      PERFORM you_sure USING lv_quest
                    CHANGING gv_answer.

      CHECK gv_answer = '1'.

      PERFORM doc_create TABLES gt_secili
                          USING lv_okcode.

      PERFORM bapiret_display  TABLES gt_message.

    WHEN 'BB_GUN_REV' OR 'SB_GUN_REV'.

      PERFORM get_selected_rows TABLES gt_secili.

      PERFORM check_selected_rows TABLES gt_secili
                                   USING lv_okcode
                                CHANGING gv_error.

      CHECK gv_error = space.

      CLEAR: lv_quest.
      CONCATENATE 'Günlük tahakkuk belgesi iptal edilecektir,'
                  'emin misiniz?'
              INTO lv_quest SEPARATED BY space.
      PERFORM you_sure USING lv_quest
                    CHANGING gv_answer.

      CHECK gv_answer = '1'.

      PERFORM doc_reverse TABLES gt_secili
                           USING lv_okcode.

      PERFORM bapiret_display  TABLES gt_message.

    WHEN 'BB_TH' OR 'SB_TH'.


      PERFORM get_selected_rows TABLES gt_secili.

      PERFORM check_selected_rows TABLES gt_secili
                                   USING lv_okcode
                                CHANGING gv_error.

      CHECK gv_error = space.

      CLEAR: lv_quest.
      CONCATENATE
      'Haftalık tahakkuk ters hareket belgesi yaratılacaktır,'
                  'emin misiniz?'
              INTO lv_quest SEPARATED BY space.
      PERFORM you_sure USING lv_quest
                    CHANGING gv_answer.

      CHECK gv_answer = '1'.

*      PERFORM inv_doc_create TABLES gt_secili
*                              USING lv_okcode.

      PERFORM doc_create TABLES gt_secili
                          USING lv_okcode.

      PERFORM bapiret_display  TABLES gt_message.

    WHEN 'BB_TH_R' OR 'SB_TH_R'.

      PERFORM get_selected_rows TABLES gt_secili.

      PERFORM check_selected_rows TABLES gt_secili
                                   USING lv_okcode
                                CHANGING gv_error.

      CHECK gv_error = space.

      CLEAR: lv_quest.
      CONCATENATE
      'Haftalık tahakkuk ters hareket belgesi iptal edilecektir,'
                  'emin misiniz?'
              INTO lv_quest SEPARATED BY space.
      PERFORM you_sure USING lv_quest
                    CHANGING gv_answer.

      CHECK gv_answer = '1'.

      PERFORM doc_reverse TABLES gt_secili
                           USING lv_okcode.

      PERFORM bapiret_display  TABLES gt_message.

    WHEN 'BB_HAFTA' OR 'SB_HAFTA'.


      PERFORM get_selected_rows TABLES gt_secili.

      PERFORM check_selected_rows TABLES gt_secili
                                   USING lv_okcode
                                CHANGING gv_error.

      CHECK gv_error = space.

**{   ->>> Added by Prodea Ozan Şahin - 02.12.2019 16:42:44
      PERFORM check_inv_value TABLES gt_secili
                               USING lv_okcode
                            CHANGING gv_error.

      CHECK gv_error = space.
**}  	 <<<- End of  Added - 02.12.2019 16:42:44

      CLEAR: lv_quest.
      CONCATENATE 'Haftalık fatura belgesi yaratılacaktır,'
                  'emin misiniz?'
              INTO lv_quest SEPARATED BY space.
      PERFORM you_sure USING lv_quest
                    CHANGING gv_answer.

      CHECK gv_answer = '1'.

      PERFORM inv_doc_create TABLES gt_secili
                              USING lv_okcode.

      PERFORM bapiret_display  TABLES gt_message.

    WHEN 'BB_HAFTA_R' OR 'SB_HAFTA_R'.

      PERFORM get_selected_rows TABLES gt_secili.

      PERFORM check_selected_rows TABLES gt_secili
                                   USING lv_okcode
                                CHANGING gv_error.

      CHECK gv_error = space.

      CLEAR: lv_quest.
      CONCATENATE 'Haftalık fatura belgesi iptal edilecektir,'
                  'emin misiniz?'
              INTO lv_quest SEPARATED BY space.
      PERFORM you_sure USING lv_quest
                    CHANGING gv_answer.

      CHECK gv_answer = '1'.

      PERFORM doc_reverse TABLES gt_secili
                           USING lv_okcode.

      PERFORM bapiret_display  TABLES gt_message.

  ENDCASE.



  PERFORM check_changed_data    USING grid.
  PERFORM refresh_table_display USING grid.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
