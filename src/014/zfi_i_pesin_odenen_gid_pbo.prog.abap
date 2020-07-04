*&---------------------------------------------------------------------*
*&  Include           ZFI_I_PESIN_ODENEN_GID_PBO
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  CLEAR : extab , extab[] .
  IF it_log[] IS INITIAL .
    extab = 'GUNLUK' .
    APPEND extab .
  ENDIF .
  SET PF-STATUS 'STATUS' EXCLUDING extab .
  SET TITLEBAR 'TITLE'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_data OUTPUT.

  ON CHANGE OF filename .
    CLEAR : gt_data , gt_data[].
*  PERFORM UPLOAD USING GT_UPLOAD[] FILENAME .
    PERFORM upload_exc USING gt_upl_exc[] filename .
    LOOP AT gt_upl_exc .
      CLEAR gt_data .
      MOVE-CORRESPONDING gt_upl_exc TO gt_data .
      PERFORM addlz CHANGING gt_data-hkont .
      PERFORM addlz CHANGING gt_data-anahesap .
      PERFORM addlz CHANGING gt_data-aufnr .
      PERFORM addlz CHANGING gt_data-lifnr .
      PERFORM addlz CHANGING gt_data-kunnr.
      PERFORM addlz CHANGING gt_data-kostl .
      IF gt_data-taksit IS INITIAL .
        gt_data-taksit = 1 .
      ENDIF .
      IF gt_data-vade_bas IS NOT INITIAL AND
         gt_data-vade_bas NE '' AND
         gt_data-vade_bit IS NOT INITIAL AND
         gt_data-vade_bit NE '' .
        APPEND gt_data .
      ENDIF .
    ENDLOOP .
*  LOOP AT GT_UPLOAD .
*    clear gt_data .
*    split gt_upload-tarih at '.' into gt_data-tarih+6(2)
*                       gt_data-tarih+4(2) gt_data-tarih(4) .
*    split gt_upload-vade_bas at '.' into gt_data-vade_bas+6(2)
*                       gt_data-vade_bas+4(2) gt_data-vade_bas(4) .
*    split gt_upload-vade_bit at '.' into gt_data-vade_bit+6(2)
*                       gt_data-vade_bit+4(2) gt_data-vade_bit(4) .
*    gt_data-hkont = gt_upload-hkont .
*    gt_data-hkont_uzun  = gt_upload-hkont_uzun  .
*    gt_data-hkont_gider = gt_upload-hkont_gider .
*    gt_data-policeno = gt_upload-policeno .
*    gt_data-license_num = gt_upload-license_num .
*
*    PERFORM addlz CHANGING gt_data-hkont .
*    gt_data-sgtxt = gt_upload-sgtxt .
*    gt_data-bktxt = gt_upload-bktxt .
*    gt_data-aufnr = gt_upload-aufnr .
*    PERFORM addlz CHANGING gt_data-aufnr .
*    gt_data-mwskz = gt_upload-mwskz .
*    gt_data-lifnr = gt_upload-lifnr .
*    PERFORM addlz CHANGING gt_data-lifnr .
*    gt_data-kostl = gt_upload-kostl .
*    gt_data-gsber = gt_upload-gsber .
*    PERFORM addlz CHANGING gt_data-kostl .
*
*    CONDENSE : gt_upload-tutar .
*    REPLACE all  OCCURRENCES OF ',' : in gt_upload-tutar with '.' .
*    gt_data-tutar = gt_upload-tutar .
*    if gt_data-vade_bas is not INITIAL and
*       gt_data-vade_bas ne '' and
*       gt_data-vade_bit is not INITIAL and
*       gt_data-vade_bit ne '' .
*    append gt_data .
*    endif .
*  ENDLOOP .
  ENDON .
ENDMODULE.                 " GET_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_alv OUTPUT.

  IF container IS INITIAL .
    CREATE OBJECT container
      EXPORTING
        container_name = 'CONT'.
  ENDIF .

  IF alvgrid IS INITIAL .
    CREATE OBJECT alvgrid
      EXPORTING
        i_parent          = container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CLEAR : lt_t_fieldcatalog , lt_t_fieldcatalog[] , gs_layout .
    v_default_recname = 'GT_DATA' .
    v_default_report_name = sy-repid .
    PERFORM set_report_fcat.
    PERFORM layout_init USING gs_layout.
    PERFORM set_layout USING '' sy-repid.
    PERFORM convert_to_lvc USING lt_t_fieldcatalog[] gs_layout
                           CHANGING lvc_fieldcat[] lvc_layout .
    CALL METHOD alvgrid->set_table_for_first_display
      EXPORTING
        is_layout       = lvc_layout
        i_save          = g_save
        is_variant      = gs_variant
      CHANGING
        it_outtab       = gt_data[]
        it_fieldcatalog = lvc_fieldcat[].
    CREATE OBJECT o_event_handler .
    SET HANDLER o_event_handler->handle_double_click FOR alvgrid .
*    SET HANDLER o_event_handler->on_f4 FOR alvgrid .
  ELSE .
    CALL METHOD alvgrid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
  ENDIF .

*CALL METHOD alvgrid->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified .

ENDMODULE.                 " SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200'.
  PERFORM show_alv.
  PERFORM show_alv2.

ENDMODULE.                    "status_0200 OUTPUT
