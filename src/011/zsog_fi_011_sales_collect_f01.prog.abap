*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_011_SALES_COLLECT_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  COLLECT_DATA
*&---------------------------------------------------------------------*
FORM collect_data .

  DATA: lt_015 TYPE TABLE OF zsg_t_019,
        ls_015 TYPE zsg_t_019,
        lt_016 TYPE TABLE OF zsg_t_020,
        ls_016 TYPE zsg_t_020,
        lt_019 TYPE TABLE OF zsg_t_019,
        lt_020 TYPE TABLE OF zsg_t_020.

  SELECT mandt
         file_date
         gjahr
         monat
         retailer_no
         sales_amount
         cancelled_sales_amount
     FROM zsg_t_015 INTO TABLE lt_015
     WHERE file_date IN s_tarih.

  IF sy-subrc NE 0.
    WRITE : / 'ZSG_T_015 tablosunda kayıt bulunamadı'.
  ELSE.
    LOOP AT lt_015 INTO ls_015.
      COLLECT ls_015 INTO lt_019.
    ENDLOOP.

    IF lt_019 IS NOT INITIAL.
*      zsg_t_016 tablusu silinecek burda
      DELETE FROM zsg_t_019 WHERE file_date IN s_tarih.
      COMMIT WORK AND WAIT.
      MODIFY zsg_t_019 FROM TABLE lt_019.
      COMMIT WORK AND WAIT.
      WRITE : / 'ZSG_T_015 tablosu güncellendi'.
    ENDIF.
  ENDIF.

  SELECT mandt
         file_date
         gjahr
         monat
         retailer_no
         payout_amount
         cncld_payout_amount
         refund_amount
         cncld_refund_amount
         FROM zsg_t_016
         INTO TABLE lt_016
         WHERE file_date IN s_tarih.
  IF sy-subrc NE 0.
    WRITE : / 'ZSG_T_016 tablosunda kayıt bulunamadı'.
  ELSE.
    LOOP AT lt_016 INTO ls_016.
      COLLECT ls_016 INTO lt_020.
    ENDLOOP.
    IF lt_020 IS NOT INITIAL.
*      zsg_t_016 tablusu silinecek burda
      DELETE FROM zsg_t_020 WHERE file_date IN s_tarih.
      COMMIT WORK AND WAIT.
      MODIFY zsg_t_020 FROM TABLE lt_020.
      COMMIT WORK AND WAIT.
      WRITE : / 'ZSG_T_016 tablosu güncellendi'.
    ENDIF.
  ENDIF.

ENDFORM.                    " COLLECT_DATA
