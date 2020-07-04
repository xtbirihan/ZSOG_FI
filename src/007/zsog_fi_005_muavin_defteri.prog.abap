*&---------------------------------------------------------------------*
*  ABAP Name     : ZSOG_FI_005_FI_KAYDI_EXCEL
*  Job-Name      :
*  Author        : Burcu Hilal Altunbaş
*  e-mail        : burcu.altunbas@prodea.com.tr
*  GMP relevant  : Mert Kaya
*  Date          : 31.07.2019
*  Description   : Excel ile FI Kaydı Yaratma Programı
*&---------------------------------------------------------------------*
REPORT zsog_fi_005_muavin_defteri NO STANDARD PAGE HEADING LINE-SIZE 202
.

include zsog_fi_005_muavin_deftri_t01.
include zsog_fi_005_muavin_defteri_f01.

*------------------------- INITIALIZATION   ---------------------------*
INITIALIZATION.

  PERFORM me_initialization.
*  PERFORM yetki_kontrol . " Tekrar açılacak

*at selection-screen on value-request for s_name3-low.
*  perform dynamic_sh_name3 using 'S_NAME3-LOW'.
*
*at selection-screen on value-request for s_name3-high.
*
*  perform dynamic_sh_name3 using 'S_NAME3-HIGH'.


*------------- AT SELECTION SCREEN OUTPUT    --------------------------*
AT SELECTION-SCREEN OUTPUT.

  DATA : lv_bukrs2 TYPE t001-bukrs.
  DATA : lv_ktopl  TYPE t001-ktopl.
  GET PARAMETER ID 'BUK' FIELD lv_bukrs2.
  SELECT SINGLE ktopl FROM t001 INTO lv_ktopl
                      WHERE bukrs = lv_bukrs2.
  SET PARAMETER ID 'KPL' FIELD lv_ktopl.

  PERFORM disable_fields .

  p_bukrs = '2425'.

*------------- AT SELECTION SCREEN ON VALUE REQUEST -------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
*  PERFORM reuse_alv_variant_sec USING    alv_repid
*                                CHANGING p_vari h_variant.
*------------------------- AT SELECTION-SCREEN -------------------*

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bukrs.

*SET PARAMETER ID 'KPL' FIELD 'INB'.
*
*  break alpaslang.

AT SELECTION-SCREEN.

  CASE sy-ucomm .
    WHEN 'BUT10' .
      PERFORM change_flg CHANGING gv_flg10 gv_but10 gv_flt10.
    WHEN 'RADIO'.
      DATA : lv_budat TYPE bkpf-budat.
      IF  p_sa = 'X' OR p_mu = 'X'.

        CLEAR : s_bldat,s_bldat[].
        SELECT MIN( budat ) FROM bkpf INTO lv_budat
                             WHERE bukrs = p_bukrs.
        s_bldat-sign = 'I'.
        s_bldat-option = 'BT'.
        s_bldat-high = sy-datum.
        s_bldat-low  = lv_budat.
        APPEND s_bldat.

      ENDIF.

    WHEN OTHERS.


*      perform  get_alv_variant  using p_vari
*                                         c_repid1
*                               changing  alv_repid
*                                         h_variant
*                                         def_variante
*                                         variant_save  .
  ENDCASE .

*------------------------- START-OF-SELECTION --------------------*
START-OF-SELECTION.

  DATA: lv_must TYPE zsog_must_sat_ana.

  IF p_mu = 'X'.
    lv_must = 'MUST'.
    AUTHORITY-CHECK OBJECT 'ZMUST'
             FOR USER sy-uname
             ID 'ZMUST'  FIELD lv_must.
*    if sy-subrc <> 0.
*      message 'Müşteri için Yetkiniz Yok' type 'S'.
*      exit.
*    endif.
  ENDIF.
  IF p_sa = 'X'.
    lv_must = 'SAT'.
    AUTHORITY-CHECK OBJECT 'ZMUST'
             FOR USER sy-uname
             ID 'ZMUST'  FIELD lv_must.
*    if sy-subrc <> 0.
*      message 'Satıcı için Yetkiniz Yok' type 'S'.
*      exit.
*    endif.
  ENDIF.
  IF p_gl = 'X'.
    lv_must = 'ANA'.
    AUTHORITY-CHECK OBJECT 'ZMUST'
             FOR USER sy-uname
             ID 'ZMUST'  FIELD lv_must.
*    if sy-subrc <> 0.
*      message 'Ana Hesap için Yetkiniz Yok' type 'S'.
*      exit.
*    endif.
  ENDIF.


  PERFORM set_initial_value.
  PERFORM header_for_alv USING text-h01 .
  PERFORM find_couples.

  IF p_gl = 'X'.
    PERFORM get_initial_gl.
    PERFORM get_data_gl.
    PERFORM get_data_gl_closed.
  ELSEIF p_mu = 'X'.
    PERFORM get_initial_mu.
    PERFORM get_data_mu.
    PERFORM get_data_mu_closed.
    PERFORM get_data_mu_satici.
    PERFORM satbol_yetki.
  ELSE.
    PERFORM get_initial_sa.
    PERFORM get_data_sa.
    PERFORM get_data_sa_closed.
    PERFORM get_data_sa_musteri.
  ENDIF.
  IF p_dvr = 'X'.
    PERFORM prepare_alv_table.
  ELSE.
    PERFORM prepare_alv_table2.
  ENDIF.


  IF p_dvr = 'X'.
** modify_devir performu devir satırlarında sonradan yapılan bir istek
** için yapıldı
    PERFORM modify_devir.
*    PERFORM modify_rtab .
  ENDIF.

  IF p_gl IS NOT INITIAL .
    LOOP AT rtab WHERE belnr NE 'DEVİR' .

      CLEAR: lv_ktopl.
      SELECT SINGLE kunnr lifnr FROM bseg
                         INTO (rtab-kunnr, rtab-lifnr)
                       WHERE bukrs = p_bukrs
                          AND belnr = rtab-belnr
                         AND gjahr = rtab-gjahr
                         AND buzei NE rtab-buzei
                         AND koart IN ('D', 'K' ).
      IF sy-subrc = 0 .
        IF rtab-kunnr IS NOT INITIAL .
          SELECT SINGLE name1 FROM kna1
                           INTO rtab-name3
                         WHERE kunnr = rtab-kunnr .
        ELSEIF rtab-lifnr IS NOT INITIAL .
          SELECT SINGLE name1 FROM lfa1
                           INTO rtab-name3
                         WHERE lifnr = rtab-lifnr .
        ELSE .

          SELECT SINGLE ktopl FROM t001
                              INTO lv_ktopl
                              WHERE bukrs = p_bukrs.
          SELECT SINGLE txt20 INTO rtab-name3
                FROM skat WHERE spras = sy-langu
                            AND saknr = rtab-gkont
                            AND ktopl = lv_ktopl.
        ENDIF .
        MODIFY rtab .
      ELSE .
        IF rtab-gkont IS NOT INITIAL.

          SELECT SINGLE ktopl FROM t001
                              INTO lv_ktopl
                             WHERE bukrs = p_bukrs.
          SELECT SINGLE txt20 INTO rtab-name3
                    FROM skat WHERE spras = sy-langu
                                AND saknr = rtab-gkont
                                AND ktopl = lv_ktopl.

          IF sy-subrc = 0 .
            MODIFY rtab.
          ENDIF.
        ENDIF .
      ENDIF .
    ENDLOOP .
  ENDIF .

*- xyigity 31.10.2014
* Ters kayıtları alınan HR ve HT belgelerinin silinmesi
  DATA : BEGIN OF lt_bktxt OCCURS 0,
           blart TYPE bkpf-blart,
           bktxt TYPE bkpf-bktxt,
         END OF lt_bktxt.
  CLEAR rtab.
  LOOP AT rtab.
    IF rtab-blart EQ 'HT' OR
       rtab-blart EQ 'AB'.
      IF rtab-bktxt IS NOT INITIAL.
        lt_bktxt-blart = rtab-blart.
        lt_bktxt-bktxt = rtab-bktxt.
        APPEND lt_bktxt.
      ENDIF.
    ENDIF.
    CLEAR rtab.
  ENDLOOP.

  CLEAR lt_bktxt.
  LOOP AT lt_bktxt.
    DELETE rtab WHERE bktxt EQ lt_bktxt-bktxt.
    CLEAR lt_bktxt.
  ENDLOOP.
*- xyigity

*------------------------- END-OF-SELECTION----------------------------*
END-OF-SELECTION.
  PERFORM show_alv.
