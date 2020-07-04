FUNCTION zsog_sg_cust_conv.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_KUNNR) TYPE  CHAR6
*"  EXPORTING
*"     VALUE(EV_KUNNR) TYPE  CHAR8
*"----------------------------------------------------------------------

  DATA: lv_six TYPE char6,
        lv_int TYPE i.

  lv_int = strlen( iv_kunnr ).
  IF lv_int < 6.
    lv_six = iv_kunnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_six
      IMPORTING
        output = lv_six.
    CONCATENATE 'SG' lv_six INTO ev_kunnr .
  ELSEIF lv_int = 6.
    lv_six = iv_kunnr.
    CONCATENATE 'SG' lv_six INTO ev_kunnr .
  ELSE.
    RAISE not_expected_customer_lenght.
  ENDIF.



ENDFUNCTION.
