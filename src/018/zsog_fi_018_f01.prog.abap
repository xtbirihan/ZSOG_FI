*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_018_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CONTROL_FIELDS
*&---------------------------------------------------------------------*
FORM control_fields .
  CASE gs_scr_1903-auth-statu.
    WHEN '01'.
      IF sy-ucomm EQ 'ONLI'.
        IF p_r1 IS NOT INITIAL.
          IF s_waers-low IS INITIAL .
            SET CURSOR FIELD 'S_WAERS-LOW'.
            MESSAGE e002.
          ENDIF.
          IF s_tarih-low IS INITIAL.
            SET CURSOR FIELD 'S_TARIH-LOW'.
            MESSAGE e002.
          ENDIF.
          IF p_hbkid IS INITIAL.
            SET CURSOR FIELD 'P_HBKID'.
            MESSAGE e002.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '02'.
    WHEN '03'.
    WHEN OTHERS.
  ENDCASE.

  LOOP AT SCREEN.
    IF screen-group1 EQ 'M2'.
      screen-required = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
  IF sy-ucomm EQ 'ONLI'.
    IF s_bukrs-low IS INITIAL .
      SET CURSOR FIELD 'S_BUKRS-LOW'.
      MESSAGE e002.
    ENDIF.
    IF s_laufd-low IS INITIAL.
      SET CURSOR FIELD 'S_LAUFD-LOW'.
      MESSAGE e002.
    ENDIF.

    IF s_laufi-low IS INITIAL.
      SET CURSOR FIELD 'S_LAUFI-LOW'.
      MESSAGE e002.
    ENDIF.

*    IF s_sektr-low IS INITIAL.
*      SET CURSOR FIELD 'S_SEKTR-LOW'.
*      MESSAGE e002.
*    ENDIF.

  ENDIF.


ENDFORM.                    "control_fields
*&---------------------------------------------------------------------*
*& Form MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM modify_screen .

  CASE gs_scr_1903-auth-statu.
    WHEN '01'.
      IF p_r1 IS NOT INITIAL.
        LOOP AT SCREEN.
          CASE screen-group1.
            WHEN 'M1'.
              screen-required = '2'.
          ENDCASE.
          MODIFY SCREEN.
        ENDLOOP.
      ELSE.
      ENDIF.

      IF sy-ucomm EQ 'ONLI'.
        IF p_r1 IS NOT INITIAL.
          LOOP AT SCREEN.
            CASE screen-group1.
              WHEN 'M1'.
                screen-required = '1'.
            ENDCASE.
            MODIFY SCREEN.
          ENDLOOP.
        ELSE.
        ENDIF.
      ENDIF.
    WHEN '02'.
      LOOP AT SCREEN.
        CASE screen-group1.
          WHEN 'M1' OR 'C1' OR 'C2'.
            screen-invisible = 1.
            screen-active    = 0.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.

    WHEN '03'.
      LOOP AT SCREEN.
        CASE screen-group1.
          WHEN 'M1' OR 'C1' OR 'C2'.
            screen-invisible = 1.
            screen-active    = 0.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.
  ENDCASE.
  IF p_r2 IS NOT INITIAL.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'M1' OR 'C1'.
          screen-invisible = 1.
          screen-active    = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "modify_screen
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization .
  PERFORM: clear_all,
           get_auth,
           restrict_datum,
           restrict_fields.

ENDFORM.                    "initialization
*&---------------------------------------------------------------------*
*&      Form  CLEAR_ALL
*&---------------------------------------------------------------------*
FORM clear_all .
  CLEAR: gs_scr_1903,
         gt_bdcdata, gt_bdcdata[],
         gt_messtab, gt_messtab[].
ENDFORM.                    "clear_all
*&---------------------------------------------------------------------*
*&      Form  GET_AUTH
*&---------------------------------------------------------------------*
FORM get_auth .
  SELECT SINGLE * FROM zsog_fi_018_t_03
                  INTO gs_scr_1903-auth
                  WHERE uname = sy-uname.
  IF sy-subrc NE 0.
    MESSAGE i001.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.                    "get_auth
*&---------------------------------------------------------------------*
*&      Form  RESTRICT_FIELDS
*&---------------------------------------------------------------------*
FORM restrict_fields .

ENDFORM.                    "restrict_fields
*&---------------------------------------------------------------------*
*&      Form  restrict_datum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM restrict_datum.

  IF gs_scr_1903-auth-statu NE '01'.
    RETURN.
  ENDIF.
  DATA: selopt   TYPE sscr_ass,
        opt_list TYPE sscr_opt_list,
        restrict TYPE sscr_restrict.

  CLEAR opt_list.
  opt_list-name          = 'BT'.
  opt_list-options-bt    = 'X'.
  APPEND opt_list TO restrict-opt_list_tab.

  CLEAR selopt.
  selopt-kind            = 'S'.
  selopt-name            = 'S_TARIH'.
  selopt-sg_main         = 'I'.
  selopt-sg_addy         = ' '.
  selopt-op_main         = 'BT'.
  selopt-op_addy         = 'BT'.
  APPEND selopt  TO restrict-ass_tab.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction            = restrict
    EXCEPTIONS
      too_late               = 1
      repeated               = 2
      selopt_without_options = 5
      selopt_without_signs   = 6
      invalid_sign           = 7
      empty_option_list      = 9
      invalid_kind           = 10
      repeated_kind_a        = 11
      OTHERS                 = 12.

ENDFORM.                    "restrict_datum
*&---------------------------------------------------------------------*
*&      Form  header_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_data.

  SELECT * FROM zsog_fi_018_t_01 INTO TABLE gs_scr_1903-header_tab
                    WHERE laufd IN s_laufd
                      AND laufi IN s_laufi.

  SELECT SINGLE * FROM zsog_fi_018_t_01 INTO  gs_scr_1903-tanitici
                   WHERE  laufi IN s_laufi
                     AND  laufd IN s_laufd.
ENDFORM.                    "header_data
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data .
  DATA: lr_lifnr TYPE md_range_t_lifnr.

  CASE gs_scr_1903-auth-statu.
    WHEN '01'.
      PERFORM get_liste_hazirlayici_data TABLES lr_lifnr.
    WHEN '02'.
      PERFORM get_category_data  TABLES lr_lifnr.
    WHEN '03'.
      PERFORM get_appover_data TABLES lr_lifnr.
    WHEN OTHERS.
  ENDCASE.

  DATA: ls_sum LIKE LINE OF gs_scr_1903-sum_alv.

  LOOP AT gs_scr_1903-sum_alv INTO ls_sum.
*{ ->>> COMMENTED by Prodea araser - 27.02.2020 13:17:05
*    ls_sum-vadesi_gelen_tut = ls_sum-vadesi_gelen_tut * ( -1 ).
*} <<<- END of COMMENT - 27.02.2020 13:17:05
    MODIFY gs_scr_1903-sum_alv FROM ls_sum TRANSPORTING vadesi_gelen_tut.
    CLEAR: ls_sum.
  ENDLOOP.

  IF  gs_scr_1903-sum_alv IS INITIAL.
    MESSAGE i011.
    LEAVE LIST-PROCESSING.
  ENDIF.
  "ozans
  SORT  gs_scr_1903-sum_alv BY laufd laufi bukrs lifnr.
*  PERFORM get_stock TABLES lr_lifnr.
ENDFORM.                    "get_data
*&---------------------------------------------------------------------*
*&      Form  get_liste_hazirlayici_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LR_LIFNR   text
*----------------------------------------------------------------------*
FORM get_liste_hazirlayici_data TABLES lr_lifnr STRUCTURE mdrange_s_lifnr.

  DATA: ls_return       TYPE bapireturn,
        lt_keybalance   TYPE TABLE OF bapi3008_3,
        lt_keybalance_c TYPE TABLE OF bapi3007_3,
*        lt_header       TYPE TABLE OF zfi_so_dt_07, "Budak
        lv_answer(1),
*        lv_sektor(10),
        lv_toplam       TYPE bsik-wrbtr,
        lrs_lifnr       TYPE mdrange_s_lifnr.

*        z1~bukrs
*        z1~lifnr
*        z1~satici_adi
*        z1~land1
*        z1~laufd
*        z1~laufi
*        z1~kunnr
*        z1~satici_grubu
*        SUM( z1~belge_tutar )      AS wrbtr
*        SUM( z1~kdvli_tut )        AS kdvli_tut
*        SUM( z1~kdvsiz_tut )       AS kdvsiz_tut
*        SUM( z1~kdv_tut    )       AS kdv_tut

  DATA: BEGIN OF lt_header OCCURS 0,
          bukrs        LIKE bsik-bukrs,
          lifnr        LIKE bsik-lifnr,
          satici_adi   LIKE lfa1-name1,
          land1        LIKE lfa1-land1,
          laufd        LIKE zsog_fi_018_t_01-laufd,
          laufi        LIKE zsog_fi_018_t_01-laufi,
          kunnr        LIKE knb1-kunnr,
          satici_grubu LIKE lfa1-ktokk,
*          sektor       LIKE zsog_fi_018_t_01-sektor,
          wrbtr        LIKE bsik-wrbtr,
          kdvli_tut    LIKE bsik-wrbtr,
          kdvsiz_tut   LIKE bsik-wrbtr,
          kdv_tut      LIKE bsik-wrbtr,
        END OF lt_header.

  DATA: lt_header_data LIKE TABLE OF lt_header.


  DATA: lt_onay_durum TYPE zsog_fi_018_t_01 OCCURS 0.
  DATA: ls_onay_durum LIKE LINE OF lt_onay_durum.

*  DATA(lv_bkont) = s_waers-low+0(2). "Budak
  DATA: lv_bkont TYPE lfbk-bkont.
  lv_bkont = s_waers-low+0(2).

  TYPES : BEGIN OF ltt_tutar,
            odenecek_tutar TYPE bsik-wrbtr,
            belgeden_kalan TYPE bsik-wrbtr,
          END OF ltt_tutar.

  DATA: ls_tutar TYPE  ltt_tutar.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 10:54:38
  FIELD-SYMBOLS <fs_header> LIKE LINE OF lt_header.
  DATA: lt_kismi TYPE TABLE OF  zsog_fi_018_s_004.
  DATA: ls_kismi TYPE zsog_fi_018_s_004.


  DATA: BEGIN OF ls_temp1,
         bukrs            TYPE bsik-bukrs,
         lifnr            TYPE bsik-lifnr,
         satici_adi       TYPE lfa1-name1,"name1
         land1            TYPE lfa1-land1,
         gjahr            TYPE bsik-gjahr,
         belnr            TYPE bsik-belnr,
         buzei            TYPE bsik-buzei,
         waers            TYPE bsik-waers,
         blart            TYPE bsik-blart,
         shkzg            TYPE bsik-shkzg,
         budat            TYPE bsik-budat,
         bldat            TYPE bsik-bldat,
         wrbtr            TYPE bsik-wrbtr,
         zfbdt            TYPE bsik-zfbdt,
         zbd1t            TYPE bsik-zbd1t,
         hkont            TYPE bsik-hkont,
         zlspr            TYPE bsik-zlspr,
         zahls            TYPE lfb1-zahls,
         tam_odeme_bicimi TYPE bsik-zlsch,"zlsch
         satici_grubu     TYPE lfa1-ktokk,"ktokk
         mwskz            TYPE bsik-mwskz,
         l_kunnr          TYPE lfa1-kunnr,
         rebzg            TYPE bsik-rebzg,"denkleştirilmiş belge
         rebzj            TYPE bsik-rebzj,"denkleştirilmiş yıl
         rebzz            TYPE bsik-rebzz,"denkleştirilmiş kalem
         laufd            TYPE regup-laufd,
         laufi            TYPE zsog_fi_018_t_01-laufi,
         kunnr            TYPE knb1-kunnr,
         belge_tutar      TYPE bsik-wrbtr,
         net_vade_tarihi  TYPE bsik-zfbdt,
       END OF ls_temp1,
       lt_temp1 LIKE TABLE OF ls_temp1.
  FIELD-SYMBOLS: <fs_temp1> LIKE ls_temp1.

  DATA: BEGIN OF ls_temp2,
         bukrs            TYPE bsik-bukrs,
         lifnr            TYPE bsik-lifnr,
         satici_adi       TYPE lfa1-name1,"name1
         land1            TYPE lfa1-land1,
         gjahr            TYPE bsik-gjahr,
         belnr            TYPE bsik-belnr,
         buzei            TYPE bsik-buzei,
         waers            TYPE bsik-waers,
         blart            TYPE bsik-blart,
         shkzg            TYPE bsik-shkzg,
         budat            TYPE bsik-budat,
         bldat            TYPE bsik-bldat,
         wrbtr            TYPE bsik-wrbtr,
         zfbdt            TYPE bsik-zfbdt,
         zbd1t            TYPE bsik-zbd1t,
         hkont            TYPE bsik-hkont,
         zlspr            TYPE bsik-zlspr,
         zahls            TYPE lfb1-zahls,
         tam_odeme_bicimi TYPE bsik-zlsch,"zlsch
         satici_grubu     TYPE lfa1-ktokk,"ktokk
         mwskz            TYPE bsik-mwskz,
         l_kunnr          TYPE lfa1-kunnr,
         rebzg            TYPE bsik-rebzg,"denkleştirilmiş belge
         rebzj            TYPE bsik-rebzj,"denkleştirilmiş yıl
         rebzz            TYPE bsik-rebzz,"denkleştirilmiş kalem
         laufd            TYPE regup-laufd,
         laufi            TYPE zsog_fi_018_t_01-laufi,
         kunnr            TYPE knb1-kunnr,
         belge_tutar      TYPE bsik-wrbtr,
         net_vade_tarihi  TYPE bsik-zfbdt,
         kdvli_tut        LIKE bsik-wrbtr,
         kdvsiz_tut       LIKE bsik-wrbtr,
         kdv_tut          LIKE bsik-wrbtr,
       END OF ls_temp2,
       lt_temp2 LIKE TABLE OF ls_temp2.
  FIELD-SYMBOLS: <fs_temp2> LIKE ls_temp2.

  DATA: BEGIN OF ls_bset,
         bukrs TYPE bset-bukrs,
         belnr TYPE bset-belnr,
         gjahr TYPE bset-gjahr,
         buzei TYPE bset-buzei,
         hwbas TYPE bset-hwbas,
         hwste TYPE bset-hwste,
        END OF ls_bset,
        lt_bset LIKE TABLE OF ls_bset.

  DATA: BEGIN OF ls_knb1,
         bukrs TYPE knb1-bukrs,
         kunnr TYPE knb1-kunnr,
       END OF ls_knb1,
       lt_knb1 LIKE TABLE OF ls_knb1.

  DATA: ls_lismi_alv LIKE LINE OF gs_scr_1903-kismi.
  DATA: ls_detail LIKE LINE OF gs_scr_1903-detail.

  DATA: BEGIN OF ls_iban,
          lifnr TYPE lfa1-lifnr,
          banks TYPE lfbk-banks,
          bankl TYPE lfbk-bankl,
          bankn TYPE lfbk-bankn,
          iban  TYPE tiban-iban,
        END OF ls_iban,
        lt_iban LIKE TABLE OF ls_iban.

  DATA: BEGIN OF ls_banka,
          lifnr TYPE lfa1-lifnr,
          banks TYPE lfbk-banks,
          bankl TYPE lfbk-bankl,
          bankn TYPE lfbk-bankn,
          koinh TYPE lfbk-koinh,
          xezer TYPE lfbk-xezer,
          banka TYPE bnka-banka,
        END OF ls_banka,
        lt_banka LIKE TABLE OF ls_banka.

  " banke eşleştirme tablosu
  DATA: ls_banka_es TYPE zsog_fi_018_t_05,
        lt_banka_es TYPE TABLE OF zsog_fi_018_t_05.

  "ödemesi yapılan belgelerin ekrana gelmemesi için
  DATA: BEGIN OF ls_bseg,
         bukrs TYPE bseg-bukrs,
         belnr TYPE bseg-belnr,
         gjahr TYPE bseg-gjahr,
         buzei TYPE bseg-buzei,
         rebzg TYPE bseg-rebzg,
        END OF ls_bseg,
        lt_bseg LIKE TABLE OF ls_bseg.

*}     <<<- End of   Inserted - 11.03.2020 10:54:38

*  PERFORM domain_get_value USING s_sektr-low 'ZSOG_FI_018_SEKTOR_DM' CHANGING lv_sektor.

  SELECT * FROM zsog_fi_018_t_01 INTO CORRESPONDING FIELDS OF TABLE lt_onay_durum
                                  WHERE laufd  EQ s_laufd-low
                                    AND laufi  EQ s_laufi-low
                                    AND bukrs  EQ s_bukrs-low
                                    AND lifnr  IN s_lifnr
                                    ORDER BY laufd laufi bukrs lifnr.


  SELECT * FROM zsog_fi_018_t_04 INTO TABLE gs_scr_1903-item_tab
                                   WHERE laufd = s_laufd-low
                                     AND laufi = s_laufi-low
                                     AND bukrs = s_bukrs-low.

  "eğer 04 durumunda belge varsa onaylanmayı bekliyordur.
  LOOP AT lt_onay_durum INTO ls_onay_durum WHERE onay_durum = '04'.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    DATA: ls_oneri LIKE LINE OF lt_onay_durum.
    CLEAR: ls_oneri.
    READ TABLE lt_onay_durum INTO ls_oneri INDEX 1.
    p_oneri = ls_oneri-oneri_tutar.

    "Budak
*    SELECT * FROM zfi_so_dt_13
*             INTO CORRESPONDING FIELDS OF TABLE lt_header_data
*             WHERE laufd  IN s_laufd
*               AND laufi  IN s_laufi
*               AND bukrs  IN s_bukrs
*               AND sektor IN s_sektr.
*    lt_header = CORRESPONDING #( lt_header_data ).

*{   ->>> Inserted by Prodea Ozan Şahin - 10.03.2020 14:55:16
    SELECT  z4~bukrs
            z4~lifnr
            z4~satici_adi
            z4~land1
            z4~laufd
            z4~laufi
            z4~kunnr
            z4~satici_grubu
            SUM( z4~belge_tutar ) AS wrbtr
            SUM( z4~kdvli_tut )
            SUM( z4~kdvsiz_tut )
            SUM( z4~kdv_tut )
       FROM zsog_fi_018_t_04 AS z4
       INNER JOIN bsik AS bs ON bs~bukrs = z4~bukrs
                            AND bs~lifnr = z4~lifnr
                            AND bs~belnr = z4~belnr
                            AND bs~gjahr = z4~gjahr
                            AND bs~buzei = z4~buzei
                            AND bs~mandt = z4~mandt
*       INNER JOIN lfb1 AS l1 ON l1~bukrs = z4~bukrs
*                            AND l1~lifnr = z4~lifnr
*                            AND l1~mandt = z4~mandt
       INNER JOIN lfa1 AS l1 ON l1~lifnr = z4~lifnr
       INTO CORRESPONDING FIELDS OF TABLE lt_header
       WHERE z4~laufd  IN s_laufd
         AND z4~laufi  IN s_laufi
         AND z4~bukrs  IN s_bukrs
      GROUP BY z4~bukrs z4~lifnr z4~satici_adi
               z4~land1 z4~laufd z4~laufi
               z4~kunnr z4~satici_grubu.

    LOOP AT lt_header ASSIGNING <fs_header>.
      <fs_header>-laufd = s_laufd-low.
      <fs_header>-laufi = s_laufi-low.
    ENDLOOP.
*}     <<<- End of   Inserted - 10.03.2020 14:55:16

*
*    SELECT
*            bukrs               ,
*            lifnr               ,
*            satici_adi          ,
*            land1               ,
*            gjahr               ,
*            belnr               ,
*            buzei               ,
*            waers               ,
*            blart               ,
*            mwskz               ,
*            shkzg               ,
*            budat               ,
*            bldat               ,
*            zfbdt               ,
*            zbd1t               ,
*            zahls               ,
*            iban                ,
*            zlspr               ,
*            tam_odeme_bicimi    ,
*            satici_grubu        ,
*            net_vade_tarihi     ,
*            bankl               ,
*            bankn               ,
*            koinh               ,
*            xezer               ,
*            banka               ,
*            zlsch               ,
*            hbkid               ,
*            laufd               ,
*            laufi               ,
*            belge_tutar         ,
*            kdvli_tut           ,
*            kdvsiz_tut          ,
*            kdv_tut             ,
*            kalan_odeme         ,
*            cat_satici_odeme    ,
*            cat_devlet_odeme    ,
*            cat_onay            ,
*            ony_satici_odeme    ,
*            ony_devlet_odeme    ,
*            ony_onay            ,
*            tam_odeme           ,
*            odenecek_tutar      ,
*            belgeden_kalan
*       FROM zfi_so_dt_12
*        INTO CORRESPONDING FIELDS OF TABLE gs_scr_1903-detail
*             WHERE laufd  IN s_laufd
*               AND laufi  IN s_laufi
*               AND bukrs  IN s_bukrs
*               AND sektor IN s_sektr.

*{   ->>> Inserted by Prodea Ozan Şahin - 10.03.2020 15:23:11

    SELECT
     z4~mandt
     z4~laufd
     z4~laufi
     z4~bukrs
     z4~lifnr
     z4~belnr
     z4~gjahr
     z4~buzei
     z4~secim
     z4~satici_grubu
     z4~satici_adi
     z4~kunnr
     z4~land1
     z4~iban
     z4~waers
     z4~shkzg
     z4~blart
     z4~mwskz
     z4~budat
     z4~bldat
     z4~zfbdt
     z4~zbd1t
     z4~net_vade_tarihi
     z4~kismi_odeme_icon
     z4~zlsch
     z4~hbkid
     z4~bankl
     z4~bankn
     z4~banka
     z4~koinh
     z4~xezer
     z4~zahls
     z4~zlspr
     z4~tam_odeme_bicimi
     z4~belge_tutar
     z4~kdvli_tut
     z4~kdvsiz_tut
     z4~kdv_tut
     z4~kalan_odeme
     z4~cat_satici_odeme
     z4~cat_devlet_odeme
     z4~cat_onay
     z4~ony_satici_odeme
     z4~ony_devlet_odeme
     z4~ony_onay
     z4~uname
     z4~statu
     z4~onay_durum
     z4~icon_name
     z4~icon_text
     z4~datum
     z4~uzeit
     z4~tam_odeme
     z4~odenecek_tutar
     z4~belgeden_kalan
     FROM zsog_fi_018_t_04 AS z4
     INNER JOIN bsik AS bs ON bs~bukrs = z4~bukrs
                          AND bs~lifnr = z4~lifnr
                          AND bs~belnr = z4~belnr
                          AND bs~gjahr = z4~gjahr
                          AND bs~buzei = z4~buzei
                          AND bs~mandt = z4~mandt
*     INNER JOIN lfb1 AS l1 ON l1~bukrs = z4~bukrs
*                          AND l1~lifnr = z4~lifnr
*                          AND l1~mandt = z4~mandt
     INNER JOIN lfa1 AS l1 ON l1~lifnr = z4~lifnr
     INTO CORRESPONDING FIELDS OF TABLE gs_scr_1903-detail
     WHERE z4~laufd  IN s_laufd
       AND z4~laufi  IN s_laufi
       AND z4~bukrs  IN s_bukrs.

*}     <<<- End of   Inserted - 10.03.2020 15:23:11

*
*    "kismi ödeme belgeleri alınıyor.
*    SELECT  laufd,
*            laufi,
*            bukrs,
*            lifnr,
*            satici_grubu,
*            satici_adi,
*            waers,
*            belnr,
*            gjahr,
*            buzei,
*            shkzg,
*            blart,
*            budat,
*            bldat,
*            zfbdt,
*            zbd1t,
*            rebzg,
*            rebzj,
*            wrbtr,
*            ktokk,
*            net_vade_tarihi
*        FROM zfi_so_dt_14(
*                       p_bukrs = @s_bukrs-low ,
*                       p_laufd = @s_laufd-low ,
*                       p_laufi = @s_laufi-low   )
*         INTO TABLE @gs_scr_1903-kismi
*         WHERE lifnr IN @s_lifnr
*           AND belnr IN @s_belnr
*           AND satici_grubu IN @s_group
*           AND sektor IN @s_sektr.

*{   ->>> Inserted by Prodea Ozan Şahin - 10.03.2020 15:29:13

    CLEAR lt_kismi.
    SELECT
    b~bukrs
    b~lifnr
    l~name1 AS satici_adi
    b~waers
    b~belnr
    b~gjahr
    b~buzei
    b~rebzg
    b~rebzj
    b~shkzg
    b~blart
    b~budat
    b~bldat
    b~zfbdt
    b~zbd1t
    b~wrbtr
    l~ktokk
    FROM bsik AS b
    INNER JOIN lfa1 AS l ON b~lifnr = l~lifnr
    INNER JOIN lfb1 AS f ON b~bukrs = f~bukrs
                        AND b~lifnr = f~lifnr
    INNER JOIN zsog_fi_018_t_04 AS z  ON b~bukrs = z~bukrs
                                     AND b~rebzg = z~belnr
                                     AND b~rebzj = z~gjahr
    INNER JOIN zsog_fi_018_t_01 AS z1 ON z~laufd = z1~laufd
                                     AND z~laufi = z1~laufi
                                     AND z~bukrs = z1~bukrs
                                     AND z~lifnr = z1~lifnr
    INTO CORRESPONDING FIELDS OF TABLE lt_kismi
    WHERE b~lifnr IN s_lifnr
      AND b~belnr IN s_belnr
      AND z~satici_grubu IN s_group
*      AND z1~sektor IN s_sektr
      AND b~bukrs = s_bukrs-low
      AND ( b~blart = 'KS' OR b~blart = 'ZP' ).

    LOOP AT lt_kismi INTO ls_kismi.
      ls_kismi-net_vade_tarihi = ls_kismi-zbd1t + ls_kismi-zfbdt.
      ls_kismi-laufd = s_laufd-low.
      ls_kismi-laufi = s_laufi-low.
      APPEND ls_kismi TO gs_scr_1903-kismi.
    ENDLOOP.

*}     <<<- End of   Inserted - 10.03.2020 15:29:13

    DELETE ADJACENT DUPLICATES FROM gs_scr_1903-kismi COMPARING ALL FIELDS.
    DATA: ls_header       LIKE LINE OF lt_header.
    DATA: ls_keybalance   LIKE LINE OF lt_keybalance.
    DATA: ls_keybalance_c LIKE LINE OF lt_keybalance_c.
    DATA: lt_detail_alv   LIKE gs_scr_1903-detail.
    DATA: lt_kismi_alv    LIKE gs_scr_1903-kismi.
    FIELD-SYMBOLS : <fs_kismi_alv> LIKE LINE OF gs_scr_1903-kismi.
    FIELD-SYMBOLS : <fs_detail> LIKE LINE OF gs_scr_1903-detail.

    LOOP AT lt_header INTO ls_header.
      CLEAR: ls_keybalance, ls_keybalance_c.
      PERFORM bapi_ap_acc_getkeydatebalance TABLES lt_keybalance USING ls_header  CHANGING ls_return.
      IF lt_keybalance IS NOT INITIAL .
        READ TABLE lt_keybalance INTO ls_keybalance INDEX 1.
*        DATA(ls_keybalance) = lt_keybalance[ 1 ].
      ENDIF.

      IF ls_header-kunnr IS NOT INITIAL.
        PERFORM bapi_ar_acc_getkeydatebalance TABLES lt_keybalance_c USING ls_header CHANGING ls_return.

        IF lt_keybalance_c IS NOT INITIAL.
          READ TABLE lt_keybalance_c INTO ls_keybalance_c INDEX 1.
*          DATA(ls_keybalance_c) = lt_keybalance_c[ 1 ].
        ENDIF.
      ENDIF.


      CLEAR: lt_detail_alv, lt_detail_alv[], lt_kismi_alv, lt_kismi_alv[].


*      DATA(lt_detail_alv) =  FILTER #( gs_scr_1903-detail USING KEY lifnr
*                                         WHERE lifnr = ls_header-lifnr ).
      lt_detail_alv[] = gs_scr_1903-detail[].
      DELETE lt_detail_alv WHERE lifnr NE ls_header-lifnr.

*      DATA(lt_kismi_alv) =  FILTER #( gs_scr_1903-kismi USING KEY lifnr
*                             WHERE lifnr = ls_header-lifnr ).
      lt_kismi_alv[] = gs_scr_1903-kismi[].
      DELETE lt_kismi_alv WHERE lifnr NE ls_header-lifnr.

      READ TABLE lt_onay_durum INTO ls_onay_durum WITH KEY laufd = ls_header-laufd
                                                           laufi = ls_header-laufi
                                                           bukrs = ls_header-bukrs
                                                           lifnr = ls_header-lifnr
                                                           BINARY SEARCH.
      "eğer kısmi ödeme belgesi varsa kismi ödeme belge tutarı kadar belgeden düş
      UNASSIGN: <fs_kismi_alv>.
      LOOP AT lt_kismi_alv ASSIGNING <fs_kismi_alv>.
        ls_header-wrbtr = ls_header-wrbtr - <fs_kismi_alv>-wrbtr.
      ENDLOOP.

      UNASSIGN: <fs_detail>.
      LOOP AT lt_detail_alv ASSIGNING <fs_detail>.
        ls_tutar-odenecek_tutar = ls_tutar-odenecek_tutar + <fs_detail>-odenecek_tutar.
        ls_tutar-belgeden_kalan = ls_tutar-belgeden_kalan + <fs_detail>-belgeden_kalan.
      ENDLOOP.

      DATA: ls_s_01 TYPE zsog_fi_018_s_01.

      CLEAR: ls_s_01.
      ls_s_01-laufd               = ls_header-laufd.
      ls_s_01-laufi               = ls_header-laufi.
      ls_s_01-bukrs               = ls_header-bukrs.
      ls_s_01-lifnr               = ls_header-lifnr.
      ls_s_01-satici_adi          = ls_header-satici_adi.
      ls_s_01-kunnr               = ls_header-kunnr.
      ls_s_01-ktokk               = ls_header-satici_grubu.
      ls_s_01-land1               = ls_header-land1.
*      ls_s_01-sektor              = s_sektr-low.
*      ls_s_01-sektor_tanim        = lv_sektor.
      ls_s_01-toplam_tut          = ls_keybalance-lc_bal.
      ls_s_01-vadesi_gelen_tut    = ls_header-wrbtr.
      ls_s_01-musteri_borc        = ls_keybalance_c-lc_bal.
      ls_s_01-kdvli_tut           = ls_header-kdvli_tut.
      ls_s_01-kdvsiz_tut          = ls_header-kdvsiz_tut.
      ls_s_01-kdv_tut             = ls_header-kdv_tut.
      ls_s_01-odenecek_tutar      = ls_tutar-odenecek_tutar.
      ls_s_01-belgeden_kalan      = ls_tutar-belgeden_kalan.
      ls_s_01-detail_icon         = icon_select_detail.
      ls_s_01-kismi_icon          = icon_status_partly_booked.
      ls_s_01-detail_alv          = lt_detail_alv.
      ls_s_01-kismi_alv           = lt_kismi_alv.
      ls_s_01-onay_durum          = ls_onay_durum.

      APPEND ls_s_01 TO gs_scr_1903-sum_alv.

*      APPEND VALUE ZSOG_FI_018_S_01(
*                          laufd               = ls_header-laufd
*                          laufi               = ls_header-laufi
*                          bukrs               = ls_header-bukrs
*                          lifnr               = ls_header-lifnr
*                          satici_adi          = ls_header-satici_adi
*                          kunnr               = ls_header-kunnr
*                          ktokk               = ls_header-satici_grubu
*                          land1               = ls_header-land1
*                          sektor              = s_sektr-low
*                          sektor_tanim        = lv_sektor
*                          toplam_tut          = ls_keybalance-lc_bal
*                          vadesi_gelen_tut    = ls_header-wrbtr
*                          musteri_borc        = ls_keybalance_c-lc_bal
*                          kdvli_tut           = ls_header-kdvli_tut
*                          kdvsiz_tut          = ls_header-kdvsiz_tut
*                          kdv_tut             = ls_header-kdv_tut
*                          odenecek_tutar      = ls_tutar-odenecek_tutar
*                          belgeden_kalan      = ls_tutar-belgeden_kalan
*                          detail_icon         = icon_select_detail
*                          kismi_icon          = icon_status_partly_booked
*                          detail_alv          = lt_detail_alv
*                          kismi_alv           = lt_kismi_alv
*                          onay_durum          = ls_onay_durum
*                          ) TO gs_scr_1903-sum_alv.
      lv_toplam = ls_header-wrbtr + lv_toplam.

      CLEAR: lrs_lifnr.
      lrs_lifnr-sign   = 'I'.
      lrs_lifnr-option = 'EQ'.
      lrs_lifnr-low    = ls_header-lifnr.
      COLLECT lrs_lifnr INTO lr_lifnr.
*      COLLECT VALUE mdrange_s_lifnr(  sign   = 'I'
*                                      option = 'EQ'
*                                      low    = ls_header-lifnr ) INTO lr_lifnr.

      CLEAR: ls_keybalance, lt_keybalance,
             ls_return, ls_keybalance_c, lt_keybalance_c,
             lt_detail_alv, lt_kismi_alv, ls_tutar.


    ENDLOOP.
*{   ->>> Commented by Prodea Ozan Şahin - 03.07.2020 18:49:25
*    IF p_oneri > abs( lv_toplam ).
*      MESSAGE i024 WITH p_oneri lv_toplam .
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*}     <<<- End of  Commented - 03.07.2020 18:49:25
*{   ->>> Inserted by Prodea Ozan Şahin - 03.07.2020 18:48:11
    LOOP AT gs_scr_1903-detail  TRANSPORTING NO FIELDS WHERE ony_onay IS NOT INITIAL
                                                         AND cat_onay IS NOT INITIAL.
      EXIT.
    ENDLOOP.
    IF sy-subrc <> 0.
      IF p_oneri > abs( lv_toplam ).
        MESSAGE i024 WITH p_oneri lv_toplam .
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
*}     <<<- End of   Inserted - 03.07.2020 18:48:11


  ELSE. "eğer onay durumu 04 olan belge yok ise liste oluşturuluyordur veya listeyi tekrar görüntülüyordur.
    LOOP AT lt_onay_durum INTO ls_onay_durum       WHERE onay_durum NE '01' "02 den başka bir şey yok ise aynı listeyi tekrar görüntülüyordur
                                                     AND onay_durum NE '03'
                                                     AND onay_durum NE '04'
                                                     AND onay_durum NE '05'.
      EXIT.
    ENDLOOP.
    IF sy-subrc EQ 0.
      CLEAR: ls_oneri.
      READ TABLE lt_onay_durum INTO ls_oneri INDEX 1.
      p_oneri = ls_oneri-oneri_tutar.
*      SELECT * FROM zfi_so_dt_13
*               INTO TABLE @lt_header_data
*               WHERE laufd IN @s_laufd
*                 AND laufi IN @s_laufi
*                 AND bukrs IN @s_bukrs
*                 AND sektor IN @s_sektr.
*      lt_header = CORRESPONDING #( lt_header_data ).

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 10:22:44

      SELECT
        z1~bukrs
        z1~lifnr
        z1~satici_adi
        z1~land1
        z1~laufd
        z1~laufi
        z1~kunnr
        z1~satici_grubu
        SUM( z1~belge_tutar )      AS wrbtr
        SUM( z1~kdvli_tut )        AS kdvli_tut
        SUM( z1~kdvsiz_tut )       AS kdvsiz_tut
        SUM( z1~kdv_tut    )       AS kdv_tut
       FROM  zsog_fi_018_t_04 AS z1
       INNER JOIN bsik AS z ON z1~bukrs = z~bukrs
                           AND z1~lifnr = z~lifnr
                           AND z1~belnr = z~belnr
                           AND z1~gjahr = z~gjahr
                           AND z1~buzei = z~buzei
*       INNER JOIN lfb1 AS f ON f~bukrs = z1~bukrs
*                           AND f~lifnr = z1~lifnr
       INNER JOIN lfa1 AS f ON f~lifnr = z1~lifnr
       INTO TABLE lt_header
       WHERE z1~laufd IN s_laufd
         AND z1~laufi IN s_laufi
         AND z1~bukrs IN s_bukrs
        GROUP BY z1~bukrs z1~lifnr z1~satici_adi
                 z1~land1 z1~laufd z1~laufi
                 z1~kunnr z1~satici_grubu.

*}     <<<- End of   Inserted - 11.03.2020 10:22:44

*      SELECT
*              bukrs               ,
*              lifnr               ,
*              satici_adi          ,
*              land1               ,
*              gjahr               ,
*              belnr               ,
*              buzei               ,
*              waers               ,
*              blart               ,
*              mwskz               ,
*              shkzg               ,
*              budat               ,
*              bldat               ,
*              zfbdt               ,
*              zbd1t               ,
*              zahls               ,
*              iban                ,
*              zlspr               ,
*              tam_odeme_bicimi    ,
*              satici_grubu        ,
*              net_vade_tarihi     ,
*              bankl               ,
*              bankn               ,
*              koinh               ,
*              xezer               ,
*              banka               ,
*              zlsch               ,
*              hbkid               ,
*              laufd               ,
*              laufi               ,
*              belge_tutar         ,
*              kdvli_tut           ,
*              kdvsiz_tut          ,
*              kdv_tut             ,
*              kalan_odeme         ,
*              cat_satici_odeme    ,
*              cat_devlet_odeme    ,
*              cat_onay            ,
*              ony_satici_odeme    ,
*              ony_devlet_odeme    ,
*              ony_onay            ,
*              tam_odeme           ,
*              odenecek_tutar      ,
*              belgeden_kalan
*         FROM zfi_so_dt_12
*          INTO CORRESPONDING FIELDS OF TABLE @gs_scr_1903-detail
*               WHERE laufd IN @s_laufd
*                 AND laufi IN @s_laufi
*                 AND bukrs IN @s_bukrs
*                 AND sektor IN @s_sektr.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 10:51:32

      SELECT
       z4~mandt
       z4~laufd
       z4~laufi
       z4~bukrs
       z4~lifnr
       z4~belnr
       z4~gjahr
       z4~buzei
       z4~secim
       z4~satici_grubu
       z4~satici_adi
       z4~kunnr
       z4~land1
       z4~iban
       z4~waers
       z4~shkzg
       z4~blart
       z4~mwskz
       z4~budat
       z4~bldat
       z4~zfbdt
       z4~zbd1t
       z4~net_vade_tarihi
       z4~kismi_odeme_icon
       z4~zlsch
       z4~hbkid
       z4~bankl
       z4~bankn
       z4~banka
       z4~koinh
       z4~xezer
       z4~zahls
       z4~zlspr
       z4~tam_odeme_bicimi
       z4~belge_tutar
       z4~kdvli_tut
       z4~kdvsiz_tut
       z4~kdv_tut
       z4~kalan_odeme
       z4~cat_satici_odeme
       z4~cat_devlet_odeme
       z4~cat_onay
       z4~ony_satici_odeme
       z4~ony_devlet_odeme
       z4~ony_onay
       z4~uname
       z4~statu
       z4~onay_durum
       z4~icon_name
       z4~icon_text
       z4~datum
       z4~uzeit
       z4~tam_odeme
       z4~odenecek_tutar
       z4~belgeden_kalan
       FROM zsog_fi_018_t_04 AS z4
       INNER JOIN bsik AS bs ON bs~bukrs = z4~bukrs
                            AND bs~lifnr = z4~lifnr
                            AND bs~belnr = z4~belnr
                            AND bs~gjahr = z4~gjahr
                            AND bs~buzei = z4~buzei
                            AND bs~mandt = z4~mandt
*       INNER JOIN lfb1 AS l1 ON l1~bukrs = z4~bukrs
*                            AND l1~lifnr = z4~lifnr
*                            AND l1~mandt = z4~mandt
       INNER JOIN lfa1 AS l1 ON l1~lifnr = z4~lifnr
       INTO CORRESPONDING FIELDS OF TABLE gs_scr_1903-detail
       WHERE z4~laufd  IN s_laufd
         AND z4~laufi  IN s_laufi
         AND z4~bukrs  IN s_bukrs.


*}     <<<- End of   Inserted - 11.03.2020 10:51:32

*
*      "kismi ödeme belgeleri alınıyor.
*      SELECT  laufd,
*              laufi,
*              bukrs,
*              lifnr,
*              satici_grubu,
*              satici_adi,
*              waers,
*              belnr,
*              gjahr,
*              buzei,
*              shkzg,
*              blart,
*              budat,
*              bldat,
*              zfbdt,
*              zbd1t,
*              rebzg,
*              rebzj,
*              wrbtr,
*              ktokk,
*              net_vade_tarihi
*          FROM zfi_so_dt_14(
*                         p_bukrs = @s_bukrs-low ,
*                         p_laufd = @s_laufd-low ,
*                         p_laufi = @s_laufi-low   )
*           INTO TABLE @gs_scr_1903-kismi
*           WHERE lifnr IN @s_lifnr
*             AND belnr IN @s_belnr
*             AND satici_grubu IN @s_group
*             AND sektor IN @s_sektr.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 10:53:35

      CLEAR lt_kismi.
      SELECT
      b~bukrs
      b~lifnr
      l~name1 AS satici_adi
      b~waers
      b~belnr
      b~gjahr
      b~buzei
      b~rebzg
      b~rebzj
      b~shkzg
      b~blart
      b~budat
      b~bldat
      b~zfbdt
      b~zbd1t
      b~wrbtr
      l~ktokk
      FROM bsik AS b
      INNER JOIN lfa1 AS l ON b~lifnr = l~lifnr
      INNER JOIN lfb1 AS f ON b~bukrs = f~bukrs
                          AND b~lifnr = f~lifnr
      INNER JOIN zsog_fi_018_t_04 AS z  ON b~bukrs = z~bukrs
                                       AND b~rebzg = z~belnr
                                       AND b~rebzj = z~gjahr
      "?? iki tablo var, sorulacak
      INNER JOIN zsog_fi_018_t_01 AS z1 ON z~laufd = z1~laufd
                                       AND z~laufi = z1~laufi
                                       AND z~bukrs = z1~bukrs
                                       AND z~lifnr = z1~lifnr
      INTO CORRESPONDING FIELDS OF TABLE lt_kismi
      WHERE b~lifnr IN s_lifnr
        AND b~belnr IN s_belnr
        AND z~satici_grubu IN s_group
*        AND z1~sektor IN s_sektr
        AND b~bukrs = s_bukrs-low
        AND ( b~blart = 'KS' OR b~blart = 'ZP' ).

      LOOP AT lt_kismi INTO ls_kismi.
        ls_kismi-net_vade_tarihi = ls_kismi-zbd1t + ls_kismi-zfbdt.
        ls_kismi-laufd = s_laufd-low.
        ls_kismi-laufi = s_laufi-low.
        APPEND ls_kismi TO gs_scr_1903-kismi.
      ENDLOOP.

*}     <<<- End of   Inserted - 11.03.2020 10:53:35

      DELETE ADJACENT DUPLICATES FROM gs_scr_1903-kismi COMPARING ALL FIELDS.

      LOOP AT lt_header INTO ls_header.

        PERFORM bapi_ap_acc_getkeydatebalance TABLES lt_keybalance USING ls_header  CHANGING ls_return.
        IF lt_keybalance IS NOT INITIAL .
          READ TABLE lt_keybalance INTO ls_keybalance INDEX 1.
*          ls_keybalance = lt_keybalance[ 1 ].
        ENDIF.

        IF ls_header-kunnr IS NOT INITIAL.
          PERFORM bapi_ar_acc_getkeydatebalance TABLES lt_keybalance_c USING ls_header CHANGING ls_return.

          IF lt_keybalance_c IS NOT INITIAL.
            READ TABLE lt_keybalance_c INTO ls_keybalance_c INDEX 1.
*            ls_keybalance_c = lt_keybalance_c[ 1 ].
          ENDIF.
        ENDIF.

        CLEAR: lt_detail_alv, lt_detail_alv[],
               lt_kismi_alv , lt_kismi_alv[].

*        lt_detail_alv =  FILTER #( gs_scr_1903-detail USING KEY lifnr
*                                       WHERE lifnr = ls_header-lifnr ).

        lt_detail_alv[] = gs_scr_1903-detail[].
        DELETE lt_detail_alv WHERE lifnr NE ls_header-lifnr.

*        lt_kismi_alv =  FILTER #( gs_scr_1903-kismi USING KEY lifnr
*                         WHERE lifnr = ls_header-lifnr ).

        lt_kismi_alv[] = gs_scr_1903-kismi[].
        DELETE lt_kismi_alv WHERE lifnr NE ls_header-lifnr.

*        lt_kismi_alv =  filter #( gs_scr_1903-kismi USING KEY lifnr
*                         WHERE lifnr = ls_header-lifnr ).

        READ TABLE lt_onay_durum INTO ls_onay_durum WITH KEY laufd = ls_header-laufd
                                                             laufi = ls_header-laufi
                                                             bukrs = ls_header-bukrs
                                                             lifnr = ls_header-lifnr
                                                             BINARY SEARCH.

        "eğer kısmi ödeme belgesi varsa kismi ödeme belge tutarı kadar belgeden düş
        LOOP AT lt_kismi_alv ASSIGNING <fs_kismi_alv>.
          ls_header-wrbtr = ls_header-wrbtr - <fs_kismi_alv>-wrbtr.
        ENDLOOP.

        LOOP AT lt_detail_alv ASSIGNING <fs_detail>.
          ls_tutar-odenecek_tutar = ls_tutar-odenecek_tutar + <fs_detail>-odenecek_tutar.
          ls_tutar-belgeden_kalan = ls_tutar-belgeden_kalan + <fs_detail>-belgeden_kalan.
        ENDLOOP.

        CLEAR: ls_s_01.
        ls_s_01-laufd               = ls_header-laufd.
        ls_s_01-laufi               = ls_header-laufi.
        ls_s_01-bukrs               = ls_header-bukrs.
        ls_s_01-lifnr               = ls_header-lifnr.
        ls_s_01-satici_adi          = ls_header-satici_adi.
        ls_s_01-kunnr               = ls_header-kunnr.
        ls_s_01-ktokk               = ls_header-satici_grubu.
        ls_s_01-land1               = ls_header-land1.
        ls_s_01-toplam_tut          = ls_keybalance-lc_bal.
        ls_s_01-vadesi_gelen_tut    = ls_header-wrbtr.
        ls_s_01-musteri_borc        = ls_keybalance_c-lc_bal.
        ls_s_01-kdvli_tut           = ls_header-kdvli_tut.
        ls_s_01-kdvsiz_tut          = ls_header-kdvsiz_tut.
        ls_s_01-kdv_tut             = ls_header-kdv_tut.
        ls_s_01-odenecek_tutar      = ls_tutar-odenecek_tutar.
        ls_s_01-belgeden_kalan      = ls_tutar-belgeden_kalan.
        ls_s_01-detail_icon         = icon_select_detail.
        ls_s_01-kismi_icon          = icon_status_partly_booked.
        ls_s_01-detail_alv          = lt_detail_alv.
        ls_s_01-kismi_alv           = lt_kismi_alv.
        ls_s_01-onay_durum          = ls_onay_durum.

        APPEND ls_s_01 TO gs_scr_1903-sum_alv.

*        APPEND value zsog_fi_018_s_01(
*                            laufd               = ls_header-laufd
*                            laufi               = ls_header-laufi
*                            bukrs               = ls_header-bukrs
*                            lifnr               = ls_header-lifnr
*                            satici_adi          = ls_header-satici_adi
*                            kunnr               = ls_header-kunnr
*                            ktokk               = ls_header-satici_grubu
*                            land1               = ls_header-land1
*                            sektor              = s_sektr-low
*                            sektor_tanim        = lv_sektor
*                            toplam_tut          = ls_keybalance-lc_bal
*                            vadesi_gelen_tut    = ls_header-wrbtr
*                            musteri_borc        = ls_keybalance_c-lc_bal
*                            kdvli_tut           = ls_header-kdvli_tut
*                            kdvsiz_tut          = ls_header-kdvsiz_tut
*                            kdv_tut             = ls_header-kdv_tut
*                            odenecek_tutar      = ls_tutar-odenecek_tutar
*                            belgeden_kalan      = ls_tutar-belgeden_kalan
*                            detail_icon         = icon_select_detail
*                            kismi_icon          = icon_status_partly_booked
*                            detail_alv          = lt_detail_alv
*                            kismi_alv           = lt_kismi_alv
*                            onay_durum          = ls_onay_durum
*                            ) TO gs_scr_1903-sum_alv.
        lv_toplam = ls_header-wrbtr + lv_toplam.

        CLEAR: lrs_lifnr.
        lrs_lifnr-sign   = 'I'.
        lrs_lifnr-option = 'EQ'.
        lrs_lifnr-low    = ls_header-lifnr.
        COLLECT lrs_lifnr INTO lr_lifnr.

*        COLLECT value mdrange_s_lifnr(  SIGN   = 'I'
*                                 OPTION = 'EQ'
*                                 LOW    = ls_header-lifnr ) INTO lr_lifnr.

        CLEAR: ls_keybalance, lt_keybalance,
               ls_return, ls_keybalance_c, lt_keybalance_c,
               lt_detail_alv, lt_kismi_alv, ls_tutar.
      ENDLOOP.
      IF p_oneri > abs( lv_toplam ).
        MESSAGE i024 WITH p_oneri lv_toplam .
        LEAVE LIST-PROCESSING.
      ENDIF.


    ELSE.

      IF p_oneri IS INITIAL.
        MESSAGE i016.
        LEAVE LIST-PROCESSING.
      ENDIF.
      IF gs_scr_1903-tanitici IS NOT INITIAL.
        DATA: lv_date(10).
        lv_date = gs_scr_1903-tanitici-laufd+6(2) && |.| &&  gs_scr_1903-tanitici-laufd+4(2) && |.| &&
                   gs_scr_1903-tanitici-laufd+0(4).

        MESSAGE i005 WITH lv_date gs_scr_1903-tanitici-laufi.
        LEAVE LIST-PROCESSING.
      ENDIF.

*      SELECT * FROM zfi_so_dt_07(
*                        p_bkont = @lv_bkont ,
*                        p_bukrs = @s_bukrs-low ,
*                        p_waers = @s_waers-low ,
*                        p_blart = 'KS',
*                        p_hbkid = @p_hbkid ,
*                        p_laufd = @s_laufd-low ,
*                        p_laufi = @s_laufi-low ,
*                        p_tarih_low = @s_tarih-low,
*                        p_tarhi_high = @s_tarih-high  )
*               INTO TABLE @lt_header
*               WHERE lifnr IN @s_lifnr
*                 AND satici_grubu IN @s_group
*                 AND sektor IN @s_sektr.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 10:56:16
      "" Tekrar İncelenecek
*      Cds 01
      CLEAR lt_temp1.
      SELECT b~bukrs
             b~lifnr
             l~name1"satici_adi
             l~land1
             b~gjahr
             b~belnr
             b~buzei
             b~waers
             b~blart
             b~shkzg
             b~budat
             b~bldat
             b~wrbtr
             b~zfbdt
             b~zbd1t
             b~hkont
             b~zlspr
             f~zahls
             b~zlsch"tam_odeme_bicimi
             l~ktokk"satici_grubu
             b~mwskz
             l~kunnr"l_kunnr
             b~rebzg "denkleştirilmiş belge
             b~rebzj "denkleştirilmiş yıl
             b~rebzz "denkleştirilmiş kalem
       FROM bsik AS b
       INNER JOIN lfa1 AS l ON b~lifnr = l~lifnr
       LEFT OUTER JOIN lfb1 AS f ON f~lifnr = b~lifnr
                                 AND f~bukrs = b~bukrs
       INTO TABLE lt_temp1
       WHERE b~lifnr IN s_lifnr
         AND b~bukrs EQ s_bukrs-low
         AND b~waers EQ s_waers-low
         AND b~blart NE 'KS'.

      IF lt_temp1 IS NOT INITIAL.
        SELECT lb~lifnr lb~banks lb~bankl lb~bankn tb~iban
          FROM lfbk AS lb
          LEFT OUTER JOIN tiban AS tb ON tb~banks = lb~banks
                                AND tb~bankl = lb~bankl
                                AND tb~bankn = lb~bankn
          INTO TABLE lt_iban
          FOR ALL ENTRIES IN lt_temp1
          WHERE lb~lifnr = lt_temp1-lifnr.

        SELECT lb~lifnr lb~banks lb~bankl lb~bankn lb~koinh lb~xezer
               bn~banka
          FROM lfbk AS lb
          INNER JOIN bnka AS bn ON  bn~banks = lb~banks
                                AND bn~bankl = lb~bankl
          INTO TABLE lt_banka
          FOR ALL ENTRIES IN lt_temp1
          WHERE lb~lifnr = lt_temp1-lifnr
            AND lb~xezer = abap_true.
*            AND lb~bkont = lv_bkont.

        SELECT bukrs belnr gjahr buzei rebzg
          FROM bseg INTO TABLE lt_bseg
          FOR ALL ENTRIES IN lt_temp1
          WHERE bukrs = lt_temp1-bukrs
            AND belnr = lt_temp1-belnr
            AND gjahr = lt_temp1-gjahr
            AND buzei = lt_temp1-buzei
            AND rebzg <> space.

      ENDIF.
      SELECT * FROM zsog_fi_018_t_05
        INTO TABLE lt_banka_es
        WHERE hbkid = p_hbkid.

      SORT lt_iban     BY lifnr.
      SORT lt_banka    BY lifnr banks.
      SORT lt_banka_es BY waers.
      SORT lt_bseg     BY bukrs belnr gjahr buzei.

      LOOP AT lt_temp1 ASSIGNING <fs_temp1>.
        <fs_temp1>-belge_tutar = abs( <fs_temp1>-wrbtr )."ozans
        IF <fs_temp1>-shkzg = 'S'.
          <fs_temp1>-wrbtr       = -1 * <fs_temp1>-wrbtr.
          <fs_temp1>-belge_tutar = -1 * <fs_temp1>-belge_tutar.
        ENDIF.
        <fs_temp1>-net_vade_tarihi = <fs_temp1>-zfbdt + <fs_temp1>-zbd1t.
        <fs_temp1>-laufd           = s_laufd-low.
        <fs_temp1>-laufi           = s_laufi-low.
      ENDLOOP.

      "" Bset, knb1 data
      IF lt_temp1 IS NOT INITIAL.
        SELECT bukrs belnr gjahr
               buzei hwbas hwste
          FROM bset
          INTO TABLE lt_bset
          FOR ALL ENTRIES IN lt_temp1
          WHERE bukrs = lt_temp1-bukrs
            AND belnr = lt_temp1-belnr
            AND gjahr = lt_temp1-gjahr
            AND buzei = lt_temp1-buzei.

        SELECT bukrs kunnr
          FROM knb1
          INTO TABLE lt_knb1
          FOR ALL ENTRIES IN lt_temp1
          WHERE bukrs = lt_temp1-bukrs
            AND kunnr = lt_temp1-l_kunnr
           .
      ENDIF.
      SORT lt_bset BY bukrs belnr gjahr buzei.
      SORT lt_knb1 BY bukrs kunnr.

      LOOP AT lt_temp1 INTO ls_temp1 WHERE tam_odeme_bicimi <> 'H'
                                       AND net_vade_tarihi GE s_tarih-low
                                       AND net_vade_tarihi LE s_tarih-high.
        CLEAR: ls_temp2, ls_bset, ls_knb1, ls_bseg.
        MOVE-CORRESPONDING ls_temp1 TO ls_temp2.

*   "ödemesi yapılan belgelerin ekrana gelmemesi için
*        READ TABLE lt_bseg TRANSPORTING NO FIELDS with key bukrs = ls_temp1-bukrs
*                                                           belnr = ls_temp1-belnr
*                                                           gjahr = ls_temp1-gjahr
*                                                           buzei = ls_temp1-buzei
*                                                  BINARY SEARCH.
*        IF sy-subrc = 0.
*          CONTINUE.
*        ENDIF.

        READ TABLE lt_knb1 INTO ls_knb1 WITH KEY bukrs = ls_temp1-bukrs
                                                 kunnr = ls_temp1-l_kunnr
                                        BINARY SEARCH.
        IF sy-subrc = 0.
          ls_temp2-kunnr = ls_knb1-kunnr.
        ENDIF.

        READ TABLE lt_bset INTO ls_bset WITH KEY bukrs = ls_temp1-bukrs
                                                 belnr = ls_temp1-belnr
                                                 gjahr = ls_temp1-gjahr
                                                 buzei = ls_temp1-buzei
                                        BINARY SEARCH.

        ls_temp2-kdv_tut = ls_bset-hwste.

        IF ls_temp2-mwskz IS INITIAL.
          ls_temp2-kdvli_tut  = ls_temp2-belge_tutar.
          ls_temp2-kdvsiz_tut = ls_temp2-belge_tutar.
        ELSEIF ls_temp2-mwskz IS NOT INITIAL AND ls_temp2-shkzg = 'S'.
          ls_temp2-kdvli_tut  = ls_bset-hwbas + ls_bset-hwste.
          ls_temp2-kdvsiz_tut = ls_bset-hwbas.
        ELSEIF ls_temp2-mwskz IS NOT INITIAL AND ls_temp2-shkzg = 'H'.
          ls_temp2-kdvli_tut  = -1 * ( ls_bset-hwbas + ls_bset-hwste ) .
          ls_temp2-kdvsiz_tut = -1 * ls_bset-hwbas.
        ENDIF.

        APPEND ls_temp2 TO lt_temp2.

      ENDLOOP.

      LOOP AT lt_temp2 INTO ls_temp2.
        CLEAR lt_header.
        MOVE-CORRESPONDING ls_temp2 TO lt_header.
        COLLECT lt_header.
      ENDLOOP.

*}     <<<- End of   Inserted - 11.03.2020 10:56:16

*      FIELD-SYMBOLS: <fs_header> LIKE LINE OF lt_header.

*{ ->>> INSERTED by Prodea araser - 28.02.2020 14:53:00
      LOOP AT lt_header ASSIGNING <fs_header>.
        <fs_header>-wrbtr      = abs( <fs_header>-wrbtr ).
        <fs_header>-kdvli_tut  = abs( <fs_header>-kdvli_tut ).
        <fs_header>-kdvsiz_tut = abs( <fs_header>-kdvsiz_tut ).
        <fs_header>-kdv_tut    = abs( <fs_header>-kdv_tut ).
      ENDLOOP.
*} <<<- END of INSERT - 28.02.2020 14:53:00


*        SELECT
*                bukrs               ,
*                lifnr               ,
*                satici_adi          ,
*                land1               ,
*                gjahr               ,
*                belnr               ,
*                buzei               ,
*                waers               ,
*                blart               ,
*                mwskz               ,
*                shkzg               ,
*                budat               ,
*                bldat               ,
*                zfbdt               ,
*                zbd1t               ,
*                zahls               ,
*                iban_koinh   as iban,
*                zlspr               ,
*                tam_odeme_bicimi    ,
*                satici_grubu        ,
*                net_vade_tarihi     ,
*                iban_bankl as bankl ,
*                iban_bankn as bankn ,
*                iban_koinh as koinh ,
*                xezer               ,
*                iban_banka as banka ,
*                zlsch               ,
*                hbkid               ,
*                laufd               ,
*                laufi               ,
**{ ->>> EDITED by Prodea araser - 28.02.2020 14:55:13
**- Mutlak değer dznlm.
*                abs( belge_tutar ) AS belge_tutar        ,
*                abs( kdvli_tut ) AS kdvli_tut          ,
*                abs( kdvsiz_tut ) AS kdvsiz_tut         ,
*                abs( kdv_tut ) AS kdv_tut             ,
*                abs( kalan_odeme ) AS kalan_odeme
**} <<<- END of EDIT - 28.02.2020 14:55:13
*           FROM zfi_so_dt_06(
*                          p_bkont = @lv_bkont ,
*                          p_bukrs = @s_bukrs-low ,
*                          p_waers = @s_waers-low ,
*                          p_blart = 'KS',
*                          p_hbkid = @p_hbkid ,
*                          p_laufd = @s_laufd-low ,
*                          p_laufi = @s_laufi-low ,
*                          p_tarih_low = @s_tarih-low,
*                          p_tarhi_high = @s_tarih-high  )
*            INTO CORRESPONDING FIELDS of TABLE @gs_scr_1903-detail
*            WHERE lifnr IN @s_lifnr
*              AND belnr IN @s_belnr
*              AND satici_grubu IN @s_group
*              AND sektor IN @s_sektr.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 10:56:16
      "" Tekrar İncelenecek
      LOOP AT lt_temp2 INTO ls_temp2.
        CLEAR: ls_detail, ls_iban, ls_iban, ls_banka, ls_banka_es.
        MOVE-CORRESPONDING ls_temp2 TO ls_detail.

        READ TABLE lt_iban INTO ls_iban WITH KEY lifnr = ls_temp2-lifnr
                                        BINARY SEARCH.

        ls_detail-iban  = ls_iban-iban  .

        READ TABLE lt_banka INTO ls_banka WITH KEY lifnr = ls_temp2-lifnr
                                                   banks = ls_iban-banks.
        IF sy-subrc = 0.
*          ls_detail-banks = ls_banka-banks.
          ls_detail-bankl = ls_banka-bankl.
          ls_detail-bankn = ls_banka-bankn.
          ls_detail-koinh = ls_banka-koinh.
          ls_detail-xezer = ls_banka-xezer.
          ls_detail-banka = ls_banka-banka.
        ENDIF.

        "banka eşleştirme tablosu
        READ TABLE lt_banka_es INTO ls_banka_es WITH KEY waers = ls_detail-waers
                                                BINARY SEARCH.
        IF sy-subrc = 0 .
          ls_detail-zlsch =  ls_banka_es-zlsch.
          ls_detail-hbkid =  ls_banka_es-hbkid.
        ENDIF.

        APPEND ls_detail TO gs_scr_1903-detail.
      ENDLOOP.
*}     <<<- End of   Inserted - 11.03.2020 10:56:16

      "farklı bir tanıtıcı numarası ve tarihi ile kayıt var mı kontrol et.
      IF sy-subrc EQ 0.
        DATA: lt_item TYPE TABLE OF zsog_fi_018_t_04.

        SELECT  laufd
                laufi
                bukrs
                lifnr
                belnr
                gjahr
                buzei
           FROM zsog_fi_018_t_04
                 INTO CORRESPONDING FIELDS OF TABLE lt_item
                 FOR ALL ENTRIES IN gs_scr_1903-detail
                              WHERE bukrs = gs_scr_1903-detail-bukrs
                                AND lifnr = gs_scr_1903-detail-lifnr
                                AND belnr = gs_scr_1903-detail-belnr
                                AND gjahr = gs_scr_1903-detail-gjahr
                                AND buzei = gs_scr_1903-detail-buzei
                                AND onay_durum NE '05'.

        IF sy-subrc EQ 0.
          FIELD-SYMBOLS : <fs_item> LIKE LINE OF lt_item.
          LOOP AT lt_item ASSIGNING <fs_item>.
            READ TABLE gs_scr_1903-detail ASSIGNING <fs_detail> WITH KEY
                                                                 bukrs = <fs_item>-bukrs
                                                                 lifnr = <fs_item>-lifnr
                                                                 belnr = <fs_item>-belnr
                                                                 gjahr = <fs_item>-gjahr
                                                                 buzei = <fs_item>-buzei.
            IF sy-subrc EQ 0.
              <fs_detail>-secim = 'X'.
            ENDIF.

          ENDLOOP.
          DELETE gs_scr_1903-detail WHERE secim = 'X'.
*{   ->>> Commented by Prodea Ozan Şahin - 30.03.2020 16:07:09
*          PERFORM pop_up_confirm USING text-013 text-035  text-009 text-010 text-011
*                                       text-012 CHANGING lv_answer .
*          IF lv_answer NE '1'.
*
*          ELSE.
*            PERFORM call_popup_alv TABLES  lt_item.
*          ENDIF.
*}     <<<- End of  Commented - 30.03.2020 16:07:09


          IF gs_scr_1903-detail IS INITIAL OR <fs_item>-laufi EQ s_laufi-low.
            LEAVE LIST-PROCESSING.
          ENDIF.

        ENDIF.

      ENDIF.


*            "kismi ödeme belgeleri alınıyor.
*            SELECT  laufd,
*                    laufi,
*                    bukrs,
*                    lifnr,
*                    satici_grubu,
*                    satici_adi,
*                    waers,
*                    belnr,
*                    gjahr,
*                    buzei,
*                    shkzg,
*                    blart,
*                    budat,
*                    bldat,
*                    zfbdt,
*                    zbd1t,
*                    rebzg,
*                    rebzj,
*                    wrbtr,
*                    ktokk,
*                    net_vade_tarihi
*                from zfi_so_dt_10(
*                               p_bkont = @lv_bkont ,
*                               p_bukrs = @s_bukrs-low ,
*                               p_waers = @s_waers-low ,
*                               p_blart = 'KS',
*                               p_hbkid = @p_hbkid ,
*                               p_laufd = @s_laufd-low ,
*                               p_laufi = @s_laufi-low ,
*                               p_tarih_low = @s_tarih-low,
*                               p_tarhi_high = @s_tarih-high  )
*                 INTO TABLE @gs_scr_1903-kismi
*                 WHERE lifnr IN @s_lifnr
*                   AND belnr IN @s_belnr
*                   AND satici_grubu IN @s_group
*                   AND sektor IN @s_sektr.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 10:56:16
      "" Tekrar İncelenecek

      LOOP AT lt_temp2 INTO ls_temp2 WHERE blart = 'KS' OR blart = 'ZP'.
        CLEAR ls_lismi_alv.
        MOVE-CORRESPONDING ls_temp2 TO ls_lismi_alv.
        APPEND ls_lismi_alv TO gs_scr_1903-kismi.
      ENDLOOP.

*}     <<<- End of   Inserted - 11.03.2020 10:56:16

      LOOP AT lt_header INTO ls_header.

        PERFORM bapi_ap_acc_getkeydatebalance TABLES lt_keybalance USING ls_header  CHANGING ls_return.
        IF lt_keybalance IS NOT INITIAL .
          CLEAR: ls_keybalance.
          READ TABLE lt_keybalance INTO ls_keybalance INDEX 1.
*            ls_keybalance = lt_keybalance[ 1 ].
        ENDIF.

        IF ls_header-kunnr IS NOT INITIAL.
          PERFORM bapi_ar_acc_getkeydatebalance TABLES lt_keybalance_c USING ls_header CHANGING ls_return.

          IF lt_keybalance_c IS NOT INITIAL.
*              ls_keybalance_c = lt_keybalance_c[ 1 ].
            READ TABLE lt_keybalance_c INTO ls_keybalance_c INDEX 1.
          ENDIF.
        ENDIF.


*                lt_detail_alv =  filter #( gs_scr_1903-detail USING KEY lifnr
*                                  WHERE lifnr = ls_header-lifnr ).
*
*                lt_kismi_alv =  filter #( gs_scr_1903-kismi USING KEY lifnr
*                                 WHERE lifnr = ls_header-lifnr ).

        lt_detail_alv[] = gs_scr_1903-detail[].
        DELETE lt_detail_alv WHERE lifnr NE ls_header-lifnr.

        lt_kismi_alv[] = gs_scr_1903-kismi[].
        DELETE lt_kismi_alv WHERE lifnr NE ls_header-lifnr.

        READ TABLE lt_onay_durum INTO ls_onay_durum WITH KEY laufd = ls_header-laufd
                                                             laufi = ls_header-laufi
                                                             bukrs = ls_header-bukrs
                                                             lifnr = ls_header-lifnr
                                                             BINARY SEARCH.
        IF sy-subrc NE 0.

*                  ls_onay_durum = value zsog_fi_018_t_01(
*                                                          mandt      = sy-mandt
*                                                          laufd      = ls_header-laufd
*                                                          laufi      = ls_header-laufi
*                                                          bukrs      = ls_header-bukrs
*                                                          lifnr      = ls_header-lifnr
*                                                          onay_durum = '01'
*                                                          icon_name  = 'ICON_ICON_LIST'
*                                                          datum      = sy-datum
*                                                          uzeit      = sy-uzeit
*                                                          sektor     = s_sektr-low
*                                                        ) .
          CLEAR: ls_onay_durum.
          ls_onay_durum-mandt      = sy-mandt.
          ls_onay_durum-laufd      = ls_header-laufd.
          ls_onay_durum-laufi      = ls_header-laufi.
          ls_onay_durum-bukrs      = ls_header-bukrs.
          ls_onay_durum-lifnr      = ls_header-lifnr.
          ls_onay_durum-onay_durum = '01'.
          ls_onay_durum-icon_name  = 'ICON_ICON_LIST'.
          ls_onay_durum-datum      = sy-datum.
          ls_onay_durum-uzeit      = sy-uzeit.

          PERFORM domain_get_value USING '01' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING ls_onay_durum-icon_text.

        ENDIF.

        "eğer kısmi ödeme belgesi varsa kismi ödeme belge tutarı kadar belgeden düş
        LOOP AT lt_kismi_alv ASSIGNING <fs_kismi_alv>.
*          READ TABLE lt_item2 ASSIGNING FIELD-SYMBOL(<fs_item2>) WITH KEY bukrs = <fs_item>-bukrs
*                                                                          lifnr = <fs_item>-lifnr
*                                                                          belnr = <fs_item>-belnr
*                                                                          gjahr = <fs_item>-gjahr
*                                                                          buzei = <fs_item>-buzei.
*          IF sy-subrc ne 0.
          ls_header-wrbtr = ls_header-wrbtr - <fs_kismi_alv>-wrbtr.
*          ENDIF.

        ENDLOOP.
        LOOP AT lt_detail_alv ASSIGNING <fs_detail>.
          ls_tutar-odenecek_tutar = ls_tutar-odenecek_tutar + <fs_detail>-odenecek_tutar.
          ls_tutar-belgeden_kalan = ls_tutar-belgeden_kalan + <fs_detail>-belgeden_kalan.
        ENDLOOP.

        CLEAR: ls_s_01.
        ls_s_01-laufd               = ls_header-laufd.
        ls_s_01-laufi               = ls_header-laufi.
        ls_s_01-bukrs               = ls_header-bukrs.
        ls_s_01-lifnr               = ls_header-lifnr.
        ls_s_01-satici_adi          = ls_header-satici_adi.
        ls_s_01-kunnr               = ls_header-kunnr.
        ls_s_01-ktokk               = ls_header-satici_grubu.
        ls_s_01-land1               = ls_header-land1.
*        ls_s_01-sektor              = s_sektr-low.
*        ls_s_01-sektor_tanim        = lv_sektor.
        ls_s_01-toplam_tut          = ls_keybalance-lc_bal.
        ls_s_01-vadesi_gelen_tut    = ls_header-wrbtr.
        ls_s_01-musteri_borc        = ls_keybalance_c-lc_bal.
        ls_s_01-kdvli_tut           = ls_header-kdvli_tut.
        ls_s_01-kdvsiz_tut          = ls_header-kdvsiz_tut.
        ls_s_01-kdv_tut             = ls_header-kdv_tut.
        ls_s_01-odenecek_tutar      = ls_tutar-odenecek_tutar.
        ls_s_01-belgeden_kalan      = ls_tutar-belgeden_kalan.
        ls_s_01-detail_icon         = icon_select_detail.
        ls_s_01-kismi_icon          = icon_status_partly_booked.
        ls_s_01-detail_alv          = lt_detail_alv.
        ls_s_01-kismi_alv           = lt_kismi_alv.
        ls_s_01-onay_durum          = ls_onay_durum.

        APPEND ls_s_01 TO gs_scr_1903-sum_alv.

*          APPEND value zsog_fi_018_s_01(
*                              laufd               = ls_header-laufd
*                              laufi               = ls_header-laufi
*                              bukrs               = ls_header-bukrs
*                              lifnr               = ls_header-lifnr
*                              satici_adi          = ls_header-satici_adi
*                              kunnr               = ls_header-kunnr
*                              ktokk               = ls_header-satici_grubu
*                              land1               = ls_header-land1
*                              sektor              = s_sektr-low
*                              sektor_tanim        = lv_sektor
*                              toplam_tut          = ls_keybalance-lc_bal
*                              vadesi_gelen_tut    = ls_header-wrbtr
*                              musteri_borc        = ls_keybalance_c-lc_bal
*                              kdvli_tut           = ls_header-kdvli_tut
*                              kdvsiz_tut          = ls_header-kdvsiz_tut
*                              kdv_tut             = ls_header-kdv_tut
*                              odenecek_tutar      = ls_tutar-odenecek_tutar
*                              belgeden_kalan      = ls_tutar-belgeden_kalan
*                              detail_icon         = icon_select_detail
*                              kismi_icon          = icon_status_partly_booked
*                              detail_alv          = lt_detail_alv
*                              kismi_alv           = lt_kismi_alv
*                              onay_durum          = ls_onay_durum
*                              ) TO gs_scr_1903-sum_alv.

        lv_toplam = ls_header-wrbtr + lv_toplam.

        CLEAR: lrs_lifnr.
        lrs_lifnr-sign   = 'I'.
        lrs_lifnr-option = 'EQ'.
        lrs_lifnr-low    = ls_header-lifnr.
        COLLECT lrs_lifnr INTO lr_lifnr.


*          COLLECT value mdrange_s_lifnr(  SIGN   = 'I'
*                                   OPTION = 'EQ'
*                                   LOW    = ls_header-lifnr ) INTO lr_lifnr.

        CLEAR: ls_keybalance, lt_keybalance,
               ls_return, ls_keybalance_c, lt_keybalance_c,
               lt_detail_alv, lt_kismi_alv, ls_tutar.
      ENDLOOP.

      IF p_oneri > abs( lv_toplam ).
        MESSAGE i024 WITH p_oneri lv_toplam .
        LEAVE LIST-PROCESSING.
      ENDIF.

    ENDIF.
  ENDIF.


ENDFORM.                    "get_liste_hazirlayici_data
*&---------------------------------------------------------------------*
*&      Form  get_category_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LR_LIFNR   text
*----------------------------------------------------------------------*
FORM get_category_data TABLES lr_lifnr STRUCTURE mdrange_s_lifnr.
  DATA: ls_return       TYPE bapireturn,
        lt_keybalance   TYPE TABLE OF bapi3008_3,
        lt_keybalance_c TYPE TABLE OF bapi3007_3.
*        lt_header       TYPE TABLE OF zfi_so_dt_07.

  DATA: BEGIN OF lt_header OCCURS 0,
          bukrs        LIKE bsik-bukrs,
          lifnr        LIKE bsik-lifnr,
          satici_adi   LIKE lfa1-name1,
          land1        LIKE lfa1-land1,
          laufd        LIKE zsog_fi_018_t_01-laufd,
          laufi        LIKE zsog_fi_018_t_01-laufi,
          kunnr        LIKE knb1-kunnr,
          satici_grubu LIKE lfa1-ktokk,
*          sektor       LIKE zsog_fi_018_t_01-sektor,
          wrbtr        LIKE bsik-wrbtr,
          kdvli_tut    LIKE bsik-wrbtr,
          kdvsiz_tut   LIKE bsik-wrbtr,
          kdv_tut      LIKE bsik-wrbtr,
        END OF lt_header.

  TYPES : BEGIN OF ltt_tutar,
            odenecek_tutar TYPE bsik-wrbtr,
            belgeden_kalan TYPE bsik-wrbtr,
          END OF ltt_tutar.

  DATA: ls_tutar      TYPE ltt_tutar,
        lv_toplam     TYPE bsik-wrbtr.
*        lv_sektor(10)."ozans

  TYPES: BEGIN OF ltt_rate ,
           lifnr TYPE lfa1-lifnr,
           rate  TYPE decfloat34,
         END  OF ltt_rate .

  DATA: lt_rate TYPE TABLE OF ltt_rate.
  DATA: ls_rate LIKE LINE OF lt_rate.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 10:54:38
  DATA: lt_kismi TYPE TABLE OF  zsog_fi_018_s_004.
  DATA: ls_kismi TYPE zsog_fi_018_s_004.

*}     <<<- End of   Inserted - 11.03.2020 10:54:38

*  PERFORM domain_get_value USING s_sektr-low 'ZSOG_FI_018_SEKTOR_DM' CHANGING lv_sektor.

*  SELECT * FROM zfi_so_dt_13
*           INTO TABLE @data(lt_header_data)
*           WHERE laufd IN @s_laufd
*             AND laufi IN @s_laufi
*             AND bukrs IN @s_bukrs
*             AND sektor IN @s_sektr.
*  lt_header = corresponding #( lt_header_data ).

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 11:35:39

  SELECT
    z1~bukrs
    z1~lifnr
    z1~satici_adi
    z1~land1
    z1~laufd
    z1~laufi
    z1~kunnr
    z1~satici_grubu
    SUM( z1~belge_tutar )      AS wrbtr
    SUM( z1~kdvli_tut )        AS kdvli_tut
    SUM( z1~kdvsiz_tut )       AS kdvsiz_tut
    SUM( z1~kdv_tut    )       AS kdv_tut
   FROM  zsog_fi_018_t_04 AS z1
   INNER JOIN bsik AS z ON z1~bukrs = z~bukrs
                       AND z1~lifnr = z~lifnr
                       AND z1~belnr = z~belnr
                       AND z1~gjahr = z~gjahr
                       AND z1~buzei = z~buzei
*   INNER JOIN lfb1 AS f ON f~bukrs = z1~bukrs
*                       AND f~lifnr = z1~lifnr
   INNER JOIN lfa1 AS f ON f~lifnr = z1~lifnr
   INTO TABLE lt_header
   WHERE z1~laufd IN s_laufd
     AND z1~laufi IN s_laufi
     AND z1~bukrs IN s_bukrs
    GROUP BY z1~bukrs z1~lifnr z1~satici_adi
             z1~land1 z1~laufd z1~laufi
             z1~kunnr z1~satici_grubu.

*}     <<<- End of   Inserted - 11.03.2020 11:35:39

  DATA: lt_onay_durum TYPE zsog_fi_018_t_01 OCCURS 0.
  DATA: ls_onay_durum LIKE LINE OF lt_onay_durum.
  DATA: ls_oneri LIKE LINE OF lt_onay_durum.
  DATA: ls_keybalance   LIKE LINE OF lt_keybalance.
  DATA: ls_keybalance_c LIKE LINE OF lt_keybalance_c.
  DATA: lt_detail_alv   LIKE gs_scr_1903-detail.
  DATA: lt_kismi_alv    LIKE gs_scr_1903-kismi.
  FIELD-SYMBOLS : <fs_kismi_alv> LIKE LINE OF gs_scr_1903-kismi.
  FIELD-SYMBOLS : <fs_detail> LIKE LINE OF gs_scr_1903-detail.
  DATA: ls_s_01 TYPE zsog_fi_018_s_01.
  DATA: lrs_lifnr LIKE LINE OF lr_lifnr.


  SELECT * FROM zsog_fi_018_t_01 INTO CORRESPONDING FIELDS OF TABLE lt_onay_durum
                                  WHERE laufd = s_laufd-low
                                    AND laufi = s_laufi-low
                                    AND bukrs = s_bukrs-low
                                    AND lifnr IN s_lifnr
*                                    AND sektor IN s_sektr
                                    ORDER BY laufd laufi bukrs lifnr.

  SELECT * FROM zsog_fi_018_t_04 INTO CORRESPONDING FIELDS OF TABLE gs_scr_1903-item_tab
                                   WHERE laufd = s_laufd-low
                                     AND laufi = s_laufi-low
                                     AND bukrs = s_bukrs-low.

  CLEAR: ls_oneri.
  READ TABLE lt_onay_durum INTO ls_oneri INDEX 1.
  p_oneri = ls_oneri-oneri_tutar.

*  SELECT
*          bukrs               ,
*          lifnr               ,
*          satici_adi          ,
*          land1               ,
*          gjahr               ,
*          belnr               ,
*          buzei               ,
*          waers               ,
*          blart               ,
*          mwskz               ,
*          shkzg               ,
*          budat               ,
*          bldat               ,
*          zfbdt               ,
*          zbd1t               ,
*          zahls               ,
*          iban                ,
*          zlspr               ,
*          tam_odeme_bicimi    ,
*          satici_grubu        ,
*          net_vade_tarihi     ,
*          bankl               ,
*          bankn               ,
*          koinh               ,
*          xezer               ,
*          banka               ,
*          zlsch               ,
*          hbkid               ,
*          laufd               ,
*          laufi               ,
*          belge_tutar         ,
*          kdvli_tut           ,
*          kdvsiz_tut          ,
*          kdv_tut             ,
*          kalan_odeme         ,
*          cat_satici_odeme    ,
*          cat_devlet_odeme    ,
*          cat_onay            ,
*          ony_satici_odeme    ,
*          ony_devlet_odeme    ,
*          ony_onay            ,
*          tam_odeme           ,
*          odenecek_tutar      ,
*          belgeden_kalan
*     from zfi_so_dt_12
*      into corresponding fields of table @gs_scr_1903-detail
*      where laufd in @s_laufd
*        and laufi in @s_laufi
*        and bukrs in @s_bukrs
*        and sektor in @s_sektr.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 11:37:20

  SELECT
   z4~mandt
   z4~laufd
   z4~laufi
   z4~bukrs
   z4~lifnr
   z4~belnr
   z4~gjahr
   z4~buzei
   z4~secim
   z4~satici_grubu
   z4~satici_adi
   z4~kunnr
   z4~land1
   z4~iban
   z4~waers
   z4~shkzg
   z4~blart
   z4~mwskz
   z4~budat
   z4~bldat
   z4~zfbdt
   z4~zbd1t
   z4~net_vade_tarihi
   z4~kismi_odeme_icon
   z4~zlsch
   z4~hbkid
   z4~bankl
   z4~bankn
   z4~banka
   z4~koinh
   z4~xezer
   z4~zahls
   z4~zlspr
   z4~tam_odeme_bicimi
   z4~belge_tutar
   z4~kdvli_tut
   z4~kdvsiz_tut
   z4~kdv_tut
   z4~kalan_odeme
   z4~cat_satici_odeme
   z4~cat_devlet_odeme
   z4~cat_onay
   z4~ony_satici_odeme
   z4~ony_devlet_odeme
   z4~ony_onay
   z4~uname
   z4~statu
   z4~onay_durum
   z4~icon_name
   z4~icon_text
   z4~datum
   z4~uzeit
   z4~tam_odeme
   z4~odenecek_tutar
   z4~belgeden_kalan
   FROM zsog_fi_018_t_04 AS z4
   INNER JOIN bsik AS bs ON bs~bukrs = z4~bukrs
                        AND bs~lifnr = z4~lifnr
                        AND bs~belnr = z4~belnr
                        AND bs~gjahr = z4~gjahr
                        AND bs~buzei = z4~buzei
                        AND bs~mandt = z4~mandt
*   INNER JOIN lfb1 AS l1 ON l1~bukrs = z4~bukrs
*                        AND l1~lifnr = z4~lifnr
*                        AND l1~mandt = z4~mandt
   INNER JOIN lfa1 AS l1 ON l1~lifnr = z4~lifnr
   INTO CORRESPONDING FIELDS OF TABLE gs_scr_1903-detail
   WHERE z4~laufd  IN s_laufd
     AND z4~laufi  IN s_laufi
     AND z4~bukrs  IN s_bukrs.

*}     <<<- End of   Inserted - 11.03.2020 11:37:20


*      "kismi ödeme belgeleri alınıyor.
*      SELECT  laufd,
*              laufi,
*              bukrs,
*              lifnr,
*              satici_grubu,
*              satici_adi,
*              waers,
*              belnr,
*              gjahr,
*              buzei,
*              shkzg,
*              blart,
*              budat,
*              bldat,
*              zfbdt,
*              zbd1t,
*              rebzg,
*              rebzj,
*              wrbtr,
*              ktokk,
*              net_vade_tarihi
*          from zfi_so_dt_14(
*                         p_bukrs = @s_bukrs-low ,
*                         p_laufd = @s_laufd-low ,
*                         p_laufi = @s_laufi-low   )
*           INTO TABLE @gs_scr_1903-kismi
*           WHERE lifnr IN @s_lifnr
*             AND belnr IN @s_belnr
*             AND satici_grubu IN @s_group
*             AND sektor IN @s_sektr.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 11:41:10

  CLEAR lt_kismi.
  SELECT
  b~bukrs
  b~lifnr
  l~name1 AS satici_adi
  b~waers
  b~belnr
  b~gjahr
  b~buzei
  b~rebzg
  b~rebzj
  b~shkzg
  b~blart
  b~budat
  b~bldat
  b~zfbdt
  b~zbd1t
  b~wrbtr
  l~ktokk
  FROM bsik AS b
  INNER JOIN lfa1 AS l ON b~lifnr = l~lifnr
  INNER JOIN lfb1 AS f ON b~bukrs = f~bukrs
                      AND b~lifnr = f~lifnr
  INNER JOIN zsog_fi_018_t_04 AS z  ON b~bukrs = z~bukrs
                                   AND b~rebzg = z~belnr
                                   AND b~rebzj = z~gjahr
  INNER JOIN zsog_fi_018_t_01 AS z1 ON z~laufd = z1~laufd
                                   AND z~laufi = z1~laufi
                                   AND z~bukrs = z1~bukrs
                                   AND z~lifnr = z1~lifnr
  INTO CORRESPONDING FIELDS OF TABLE lt_kismi
  WHERE b~lifnr IN s_lifnr
    AND b~belnr IN s_belnr
    AND z~satici_grubu IN s_group
    AND b~bukrs = s_bukrs-low
    AND ( b~blart = 'KS' OR b~blart = 'ZP' ).

  LOOP AT lt_kismi INTO ls_kismi.
    ls_kismi-net_vade_tarihi = ls_kismi-zbd1t + ls_kismi-zfbdt.
    ls_kismi-laufd = s_laufd-low.
    ls_kismi-laufi = s_laufi-low.
    APPEND ls_kismi TO gs_scr_1903-kismi.
  ENDLOOP.

*}     <<<- End of   Inserted - 11.03.2020 11:41:10

  DELETE ADJACENT DUPLICATES FROM gs_scr_1903-kismi COMPARING ALL FIELDS.

  DATA: ls_header LIKE LINE OF lt_header.
  LOOP AT lt_header INTO ls_header.

    PERFORM bapi_ap_acc_getkeydatebalance TABLES lt_keybalance USING ls_header  CHANGING ls_return.
    IF lt_keybalance IS NOT INITIAL .
      CLEAR: ls_keybalance.
      READ TABLE lt_keybalance INTO ls_keybalance INDEX 1.
*          data(ls_keybalance) = lt_keybalance[ 1 ].
    ENDIF.

    IF ls_header-kunnr IS NOT INITIAL.
      PERFORM bapi_ar_acc_getkeydatebalance TABLES lt_keybalance_c USING ls_header CHANGING ls_return.

      IF lt_keybalance_c IS NOT INITIAL.
        CLEAR: ls_keybalance_c.
        READ TABLE lt_keybalance_c INTO ls_keybalance_c INDEX 1.
*            data(ls_keybalance_c) = lt_keybalance_c[ 1 ].
      ENDIF.
    ENDIF.
*        data(lt_detail_alv) =  filter #( gs_scr_1903-detail USING KEY lifnr
*                                           WHERE lifnr = ls_header-lifnr ).
*
*        data(lt_kismi_alv) =  filter #( gs_scr_1903-kismi USING KEY lifnr
*                                 WHERE lifnr = ls_header-lifnr ).

    lt_detail_alv[] = gs_scr_1903-detail[].
    DELETE lt_detail_alv WHERE lifnr NE ls_header-lifnr.

    lt_kismi_alv[] = gs_scr_1903-kismi[].
    DELETE lt_kismi_alv WHERE lifnr NE ls_header-lifnr.

    CLEAR: ls_onay_durum.
    READ TABLE lt_onay_durum INTO ls_onay_durum WITH KEY laufd = ls_header-laufd
                                                         laufi = ls_header-laufi
                                                         bukrs = ls_header-bukrs
                                                         lifnr = ls_header-lifnr
                                                         BINARY SEARCH.
    "eğer kısmi ödeme belgesi varsa kismi ödeme belge tutarı kadar belgeden düş
    LOOP AT lt_kismi_alv ASSIGNING <fs_kismi_alv>.
      ls_header-wrbtr = ls_header-wrbtr - <fs_kismi_alv>-wrbtr.
    ENDLOOP.

    LOOP AT lt_detail_alv ASSIGNING <fs_detail>.
      ls_tutar-odenecek_tutar = ls_tutar-odenecek_tutar + <fs_detail>-odenecek_tutar.
      ls_tutar-belgeden_kalan = ls_tutar-belgeden_kalan + <fs_detail>-belgeden_kalan.
    ENDLOOP.

    CLEAR: ls_s_01.
    ls_s_01-laufd               = ls_header-laufd.
    ls_s_01-laufi               = ls_header-laufi.
    ls_s_01-bukrs               = ls_header-bukrs.
    ls_s_01-lifnr               = ls_header-lifnr.
    ls_s_01-satici_adi          = ls_header-satici_adi.
    ls_s_01-kunnr               = ls_header-kunnr.
    ls_s_01-ktokk               = ls_header-satici_grubu.
    ls_s_01-land1               = ls_header-land1.
    ls_s_01-toplam_tut          = ls_keybalance-lc_bal.
*    ls_s_01-sektor              = s_sektr-low.
*    ls_s_01-sektor_tanim        = lv_sektor.
    ls_s_01-vadesi_gelen_tut    = ls_header-wrbtr.
    ls_s_01-musteri_borc        = ls_keybalance_c-lc_bal.
    ls_s_01-kdvli_tut           = ls_header-kdvli_tut.
    ls_s_01-kdvsiz_tut          = ls_header-kdvsiz_tut.
    ls_s_01-kdv_tut             = ls_header-kdv_tut.
    ls_s_01-odenecek_tutar      = ls_tutar-odenecek_tutar.
    ls_s_01-belgeden_kalan      = ls_tutar-belgeden_kalan.
    ls_s_01-detail_icon         = icon_select_detail.
    ls_s_01-kismi_icon          = icon_status_partly_booked.
    ls_s_01-detail_alv          = lt_detail_alv.
    ls_s_01-kismi_alv           = lt_kismi_alv.
    ls_s_01-onay_durum          = ls_onay_durum.

    APPEND ls_s_01 TO gs_scr_1903-sum_alv.

*        APPEND value zsog_fi_018_s_01(
*                            laufd               = ls_header-laufd
*                            laufi               = ls_header-laufi
*                            bukrs               = ls_header-bukrs
*                            lifnr               = ls_header-lifnr
*                            satici_adi          = ls_header-satici_adi
*                            kunnr               = ls_header-kunnr
*                            ktokk               = ls_header-satici_grubu
*                            land1               = ls_header-land1
*                            toplam_tut          = ls_keybalance-lc_bal
*                            sektor              = s_sektr-low
*                            sektor_tanim        = lv_sektor
*                            vadesi_gelen_tut    = ls_header-wrbtr
*                            musteri_borc        = ls_keybalance_c-lc_bal
*                            kdvli_tut           = ls_header-kdvli_tut
*                            kdvsiz_tut          = ls_header-kdvsiz_tut
*                            kdv_tut             = ls_header-kdv_tut
*                            odenecek_tutar      = ls_tutar-odenecek_tutar
*                            belgeden_kalan      = ls_tutar-belgeden_kalan
*                            detail_icon         = icon_select_detail
*                            kismi_icon          = icon_status_partly_booked
*                            detail_alv          = lt_detail_alv
*                            kismi_alv           = lt_kismi_alv
*                            onay_durum          = ls_onay_durum
*                            ) TO gs_scr_1903-sum_alv.

    lv_toplam = ls_header-wrbtr + lv_toplam.

*    LOOP AT gs_scr_1903-sum_alv INTO DATA(ls_sum).
*      ls_sum-vadesi_gelen_tut = ls_sum-vadesi_gelen_tut * ( -1 ).
*      MODIFY gs_scr_1903-sum_alv FROM ls_sum TRANSPORTING vadesi_gelen_tut.
*      CLEAR: ls_sum.
*    ENDLOOP.

    CLEAR: lrs_lifnr.
    lrs_lifnr-sign   = 'I'.
    lrs_lifnr-option = 'EQ'.
    lrs_lifnr-low    = ls_header-lifnr.
    COLLECT lrs_lifnr INTO lr_lifnr.

*        COLLECT value mdrange_s_lifnr(  SIGN   = 'I'
*                                        OPTION = 'EQ'
*                                        LOW    = ls_header-lifnr ) INTO lr_lifnr.
    CLEAR: ls_rate.
    ls_rate-lifnr = ls_header-lifnr.
    COLLECT ls_rate INTO lt_rate.

*        COLLECT value ltt_rate( lifnr = ls_header-lifnr ) INTO lt_rate.

    CLEAR: ls_keybalance, lt_keybalance,
           ls_return, ls_keybalance_c, lt_keybalance_c,
           lt_detail_alv, lt_kismi_alv, ls_tutar.
  ENDLOOP.

  FIELD-SYMBOLS : <fs_sum_alv> LIKE LINE OF gs_scr_1903-sum_alv,
                  <fs_rate>    LIKE LINE OF lt_rate.

  SORT gs_scr_1903-sum_alv BY lifnr.
  LOOP AT lt_rate ASSIGNING <fs_rate>.
    READ TABLE gs_scr_1903-sum_alv ASSIGNING <fs_sum_alv> WITH KEY lifnr = <fs_rate>-lifnr BINARY SEARCH .

    IF lv_toplam NE 0.
      <fs_rate>-rate = p_oneri / lv_toplam.
    ENDIF.

*        TRY.
*            <fs_rate>-rate = exact #( p_oneri / lv_toplam ).
*          CATCH cx_sy_conversion_rounding INTO data(exc).
*            <fs_rate>-rate = exc->value.
*        ENDTRY.

    <fs_sum_alv>-dagitim = <fs_rate>-rate * <fs_sum_alv>-vadesi_gelen_tut .
    <fs_sum_alv>-dagitim = floor( <fs_sum_alv>-dagitim ) .

  ENDLOOP.

ENDFORM.                    "get_category_data
*&---------------------------------------------------------------------*
*&      Form  get_appover_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LR_LIFNR   text
*----------------------------------------------------------------------*
FORM get_appover_data TABLES lr_lifnr STRUCTURE mdrange_s_lifnr.
  "aslında kategori ile arasında fark yok fakat değişiklik istenirse diye ayırıyorum.
  DATA: ls_return       TYPE bapireturn,
        lt_keybalance   TYPE TABLE OF bapi3008_3,
        lt_keybalance_c TYPE TABLE OF bapi3007_3.
*        lt_header       TYPE TABLE OF zfi_so_dt_07.

  DATA: BEGIN OF lt_header OCCURS 0,
          bukrs        LIKE bsik-bukrs,
          lifnr        LIKE bsik-lifnr,
          satici_adi   LIKE lfa1-name1,
          land1        LIKE lfa1-land1,
          laufd        LIKE zsog_fi_018_t_01-laufd,
          laufi        LIKE zsog_fi_018_t_01-laufi,
          kunnr        LIKE knb1-kunnr,
          satici_grubu LIKE lfa1-ktokk,
*          sektor       LIKE zsog_fi_018_t_01-sektor,
          wrbtr        LIKE bsik-wrbtr,
          kdvli_tut    LIKE bsik-wrbtr,
          kdvsiz_tut   LIKE bsik-wrbtr,
          kdv_tut      LIKE bsik-wrbtr,
        END OF lt_header.
  DATA: ls_header       LIKE LINE OF lt_header.

  TYPES : BEGIN OF ltt_tutar,
            odenecek_tutar TYPE bsik-wrbtr,
            belgeden_kalan TYPE bsik-wrbtr,
          END OF ltt_tutar.

  TYPES: BEGIN OF ltt_rate ,
           lifnr TYPE lfa1-lifnr,
           rate  TYPE decfloat34,
         END  OF ltt_rate .

  DATA: lt_rate TYPE TABLE OF ltt_rate.
  DATA: ls_rate LIKE LINE OF lt_rate.

  DATA: lt_onay_durum TYPE zsog_fi_018_t_01 OCCURS 0.
  DATA: ls_onay_durum LIKE LINE OF lt_onay_durum.
  DATA: ls_oneri LIKE LINE OF lt_onay_durum.
  DATA: ls_keybalance   LIKE LINE OF lt_keybalance.
  DATA: ls_keybalance_c LIKE LINE OF lt_keybalance_c.
  DATA: lt_detail_alv   LIKE gs_scr_1903-detail.
  DATA: lt_kismi_alv    LIKE gs_scr_1903-kismi.
  FIELD-SYMBOLS : <fs_kismi_alv> LIKE LINE OF gs_scr_1903-kismi.
  FIELD-SYMBOLS : <fs_detail> LIKE LINE OF gs_scr_1903-detail.
  DATA: ls_s_01 TYPE zsog_fi_018_s_01.
  DATA: lrs_lifnr LIKE LINE OF lr_lifnr.

  FIELD-SYMBOLS : <fs_sum_alv> LIKE LINE OF gs_scr_1903-sum_alv,
                  <fs_rate>    LIKE LINE OF lt_rate.

  DATA: ls_tutar      TYPE  ltt_tutar,
*        lv_sektor(10),"ozans
        lv_toplam     TYPE bsik-wrbtr.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 10:54:38
  DATA: lt_kismi TYPE TABLE OF  zsog_fi_018_s_004.
  DATA: ls_kismi TYPE zsog_fi_018_s_004.
*}     <<<- End of   Inserted - 11.03.2020 10:54:38

*  PERFORM domain_get_value USING s_sektr-low 'ZSOG_FI_018_SEKTOR_DM' CHANGING lv_sektor.

*  SELECT * FROM zfi_so_dt_13
*           INTO TABLE @data(lt_header_data)
*           WHERE laufd IN @s_laufd
*             AND laufi IN @s_laufi
*             AND bukrs IN @s_bukrs.
*  lt_header = corresponding #( lt_header_data ).

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 11:43:49

  SELECT
    z1~bukrs
    z1~lifnr
    z1~satici_adi
    z1~land1
    z1~laufd
    z1~laufi
    z1~kunnr
    z1~satici_grubu
    SUM( z1~belge_tutar )      AS wrbtr
    SUM( z1~kdvli_tut )        AS kdvli_tut
    SUM( z1~kdvsiz_tut )       AS kdvsiz_tut
    SUM( z1~kdv_tut    )       AS kdv_tut
   FROM  zsog_fi_018_t_04 AS z1
   INNER JOIN bsik AS z ON z1~bukrs = z~bukrs
                       AND z1~lifnr = z~lifnr
                       AND z1~belnr = z~belnr
                       AND z1~gjahr = z~gjahr
                       AND z1~buzei = z~buzei
*   INNER JOIN lfb1 AS f ON f~bukrs = z1~bukrs
*                       AND f~lifnr = z1~lifnr
   INNER JOIN lfa1 AS f ON f~lifnr = z1~lifnr
   INTO TABLE lt_header
   WHERE z1~laufd IN s_laufd
     AND z1~laufi IN s_laufi
     AND z1~bukrs IN s_bukrs
    GROUP BY z1~bukrs z1~lifnr z1~satici_adi
             z1~land1 z1~laufd z1~laufi
             z1~kunnr z1~satici_grubu .

*}     <<<- End of   Inserted - 11.03.2020 11:43:49

  SELECT * FROM zsog_fi_018_t_01 INTO CORRESPONDING FIELDS OF TABLE lt_onay_durum
                                  WHERE laufd EQ s_laufd-low
                                    AND laufi EQ s_laufi-low
                                    AND bukrs EQ s_bukrs-low
                                    AND lifnr IN s_lifnr
                                    ORDER BY laufd laufi bukrs lifnr.

  SELECT * FROM zsog_fi_018_t_04 INTO CORRESPONDING FIELDS OF TABLE gs_scr_1903-item_tab
                                  WHERE laufd EQ s_laufd-low
                                    AND laufi EQ s_laufi-low
                                    AND bukrs EQ s_bukrs-low.

  CLEAR: ls_oneri.
  READ TABLE lt_onay_durum INTO ls_oneri INDEX 1.
  p_oneri = ls_oneri-oneri_tutar.

*  SELECT
*          bukrs               ,
*          lifnr               ,
*          satici_adi          ,
*          land1               ,
*          gjahr               ,
*          belnr               ,
*          buzei               ,
*          waers               ,
*          blart               ,
*          mwskz               ,
*          shkzg               ,
*          budat               ,
*          bldat               ,
*          zfbdt               ,
*          zbd1t               ,
*          zahls               ,
*          iban                ,
*          zlspr               ,
*          tam_odeme_bicimi    ,
*          satici_grubu        ,
*          net_vade_tarihi     ,
*          bankl               ,
*          bankn               ,
*          koinh               ,
*          xezer               ,
*          banka               ,
*          zlsch               ,
*          hbkid               ,
*          laufd               ,
*          laufi               ,
*          belge_tutar         ,
*          kdvli_tut           ,
*          kdvsiz_tut          ,
*          kdv_tut             ,
*          kalan_odeme         ,
*          cat_satici_odeme    ,
*          cat_devlet_odeme    ,
*          cat_onay            ,
*          ony_satici_odeme    ,
*          ony_devlet_odeme    ,
*          ony_onay            ,
*          tam_odeme           ,
*          odenecek_tutar      ,
*          belgeden_kalan
*     from zfi_so_dt_12
*      into corresponding fields of table @gs_scr_1903-detail
*      where laufd in @s_laufd
*        and laufi in @s_laufi
*        and bukrs in @s_bukrs
*        and sektor in @s_sektr.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 11:46:46

  SELECT
   z4~mandt
   z4~laufd
   z4~laufi
   z4~bukrs
   z4~lifnr
   z4~belnr
   z4~gjahr
   z4~buzei
   z4~secim
   z4~satici_grubu
   z4~satici_adi
   z4~kunnr
   z4~land1
   z4~iban
   z4~waers
   z4~shkzg
   z4~blart
   z4~mwskz
   z4~budat
   z4~bldat
   z4~zfbdt
   z4~zbd1t
   z4~net_vade_tarihi
   z4~kismi_odeme_icon
   z4~zlsch
   z4~hbkid
   z4~bankl
   z4~bankn
   z4~banka
   z4~koinh
   z4~xezer
   z4~zahls
   z4~zlspr
   z4~tam_odeme_bicimi
   z4~belge_tutar
   z4~kdvli_tut
   z4~kdvsiz_tut
   z4~kdv_tut
   z4~kalan_odeme
   z4~cat_satici_odeme
   z4~cat_devlet_odeme
   z4~cat_onay
   z4~ony_satici_odeme
   z4~ony_devlet_odeme
   z4~ony_onay
   z4~uname
   z4~statu
   z4~onay_durum
   z4~icon_name
   z4~icon_text
   z4~datum
   z4~uzeit
   z4~tam_odeme
   z4~odenecek_tutar
   z4~belgeden_kalan
   FROM zsog_fi_018_t_04 AS z4
   INNER JOIN bsik AS bs ON bs~bukrs = z4~bukrs
                        AND bs~lifnr = z4~lifnr
                        AND bs~belnr = z4~belnr
                        AND bs~gjahr = z4~gjahr
                        AND bs~buzei = z4~buzei
                        AND bs~mandt = z4~mandt
*   INNER JOIN lfb1 AS l1 ON l1~bukrs = z4~bukrs
*                        AND l1~lifnr = z4~lifnr
*                        AND l1~mandt = z4~mandt
   INNER JOIN lfa1 AS l1 ON l1~lifnr = z4~lifnr
   INTO CORRESPONDING FIELDS OF TABLE gs_scr_1903-detail
   WHERE z4~laufd  IN s_laufd
     AND z4~laufi  IN s_laufi
     AND z4~bukrs  IN s_bukrs.

*}     <<<- End of   Inserted - 11.03.2020 11:46:46

*    "kismi ödeme belgeleri alınıyor.
*    SELECT  laufd,
*            laufi,
*            bukrs,
*            lifnr,
*            satici_grubu,
*            satici_adi,
*            waers,
*            belnr,
*            gjahr,
*            buzei,
*            shkzg,
*            blart,
*            budat,
*            bldat,
*            zfbdt,
*            zbd1t,
*            rebzg,
*            rebzj,
*            wrbtr,
*            ktokk,
*            net_vade_tarihi
*        from zfi_so_dt_14(
*                       p_bukrs = @s_bukrs-low ,
*                       p_laufd = @s_laufd-low ,
*                       p_laufi = @s_laufi-low   )
*         INTO TABLE @gs_scr_1903-kismi
*         WHERE lifnr IN @s_lifnr
*           AND belnr IN @s_belnr
*           AND satici_grubu IN @s_group
*           AND sektor IN @s_sektr.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 11:48:04

  CLEAR lt_kismi.
  SELECT
  b~bukrs
  b~lifnr
  l~name1 AS satici_adi
  b~waers
  b~belnr
  b~gjahr
  b~buzei
  b~rebzg
  b~rebzj
  b~shkzg
  b~blart
  b~budat
  b~bldat
  b~zfbdt
  b~zbd1t
  b~wrbtr
  l~ktokk
  FROM bsik AS b
  INNER JOIN lfa1 AS l ON b~lifnr = l~lifnr
  INNER JOIN lfb1 AS f ON b~bukrs = f~bukrs
                      AND b~lifnr = f~lifnr
  INNER JOIN zsog_fi_018_t_04 AS z  ON b~bukrs = z~bukrs
                                   AND b~rebzg = z~belnr
                                   AND b~rebzj = z~gjahr
  INNER JOIN zsog_fi_018_t_01 AS z1 ON z~laufd = z1~laufd
                                   AND z~laufi = z1~laufi
                                   AND z~bukrs = z1~bukrs
                                   AND z~lifnr = z1~lifnr
  INTO CORRESPONDING FIELDS OF TABLE lt_kismi
  WHERE b~lifnr IN s_lifnr
    AND b~belnr IN s_belnr
    AND z~satici_grubu IN s_group
*    AND z1~sektor IN s_sektr
    AND b~bukrs = s_bukrs-low
    AND ( b~blart = 'KS' OR b~blart = 'ZP' ).

  LOOP AT lt_kismi INTO ls_kismi.
    ls_kismi-net_vade_tarihi = ls_kismi-zbd1t + ls_kismi-zfbdt.
    ls_kismi-laufd = s_laufd-low.
    ls_kismi-laufi = s_laufi-low.
    APPEND ls_kismi TO gs_scr_1903-kismi.
  ENDLOOP.

*}     <<<- End of   Inserted - 11.03.2020 11:48:04

  DELETE ADJACENT DUPLICATES FROM gs_scr_1903-kismi COMPARING ALL FIELDS.
  LOOP AT lt_header INTO ls_header.

    PERFORM bapi_ap_acc_getkeydatebalance TABLES lt_keybalance USING ls_header  CHANGING ls_return.
    IF lt_keybalance IS NOT INITIAL .
      CLEAR: ls_keybalance.
      READ TABLE lt_keybalance INTO ls_keybalance INDEX 1.
*          data(ls_keybalance) = lt_keybalance[ 1 ].
    ENDIF.

    IF ls_header-kunnr IS NOT INITIAL.
      PERFORM bapi_ar_acc_getkeydatebalance TABLES lt_keybalance_c USING ls_header CHANGING ls_return.

      IF lt_keybalance_c IS NOT INITIAL.
        READ TABLE lt_keybalance_c INTO ls_keybalance_c INDEX 1.
*            data(ls_keybalance_c) = lt_keybalance_c[ 1 ].
      ENDIF.
    ENDIF.
*        data(lt_detail_alv) =  filter #( gs_scr_1903-detail USING KEY lifnr
*                                           WHERE lifnr = ls_header-lifnr ).
*
*
*        data(lt_kismi_alv) =  filter #( gs_scr_1903-kismi USING KEY lifnr
*                               WHERE lifnr = ls_header-lifnr ).

    lt_detail_alv[] = gs_scr_1903-detail[].
    DELETE lt_detail_alv WHERE lifnr NE ls_header-lifnr.

    lt_kismi_alv[] = gs_scr_1903-kismi[].
    DELETE lt_kismi_alv WHERE lifnr NE ls_header-lifnr.

    CLEAR: ls_onay_durum.
    READ TABLE lt_onay_durum INTO ls_onay_durum WITH KEY laufd = ls_header-laufd
                                                         laufi = ls_header-laufi
                                                         bukrs = ls_header-bukrs
                                                         lifnr = ls_header-lifnr
                                                         BINARY SEARCH.

    LOOP AT lt_kismi_alv ASSIGNING <fs_kismi_alv>.
      ls_header-wrbtr = ls_header-wrbtr - <fs_kismi_alv>-wrbtr.
    ENDLOOP.

    LOOP AT lt_detail_alv ASSIGNING <fs_detail>.
      ls_tutar-odenecek_tutar = ls_tutar-odenecek_tutar + <fs_detail>-odenecek_tutar.
      ls_tutar-belgeden_kalan = ls_tutar-belgeden_kalan + <fs_detail>-belgeden_kalan.
    ENDLOOP.

    CLEAR: ls_s_01.
    ls_s_01-laufd               = ls_header-laufd.
    ls_s_01-laufi               = ls_header-laufi.
    ls_s_01-bukrs               = ls_header-bukrs.
    ls_s_01-lifnr               = ls_header-lifnr.
    ls_s_01-satici_adi          = ls_header-satici_adi.
*    ls_s_01-sektor              = s_sektr-low.
*    ls_s_01-sektor_tanim        = lv_sektor.
    ls_s_01-kunnr               = ls_header-kunnr.
    ls_s_01-ktokk               = ls_header-satici_grubu.
    ls_s_01-land1               = ls_header-land1.
    ls_s_01-toplam_tut          = ls_keybalance-lc_bal.
    ls_s_01-vadesi_gelen_tut    = ls_header-wrbtr.
    ls_s_01-musteri_borc        = ls_keybalance_c-lc_bal.
    ls_s_01-kdvli_tut           = ls_header-kdvli_tut.
    ls_s_01-kdvsiz_tut          = ls_header-kdvsiz_tut.
    ls_s_01-kdv_tut             = ls_header-kdv_tut.
    ls_s_01-odenecek_tutar      = ls_tutar-odenecek_tutar.
    ls_s_01-belgeden_kalan      = ls_tutar-belgeden_kalan.
    ls_s_01-detail_icon         = icon_select_detail.
    ls_s_01-kismi_icon          = icon_status_partly_booked.
    ls_s_01-detail_alv          = lt_detail_alv.
    ls_s_01-kismi_alv           = lt_kismi_alv.
    ls_s_01-onay_durum          = ls_onay_durum.

    APPEND ls_s_01 TO gs_scr_1903-sum_alv.

*        APPEND value zsog_fi_018_s_01(
*                            laufd               = ls_header-laufd
*                            laufi               = ls_header-laufi
*                            bukrs               = ls_header-bukrs
*                            lifnr               = ls_header-lifnr
*                            satici_adi          = ls_header-satici_adi
*                            sektor              = s_sektr-low
*                            sektor_tanim        = lv_sektor
*                            kunnr               = ls_header-kunnr
*                            ktokk               = ls_header-satici_grubu
*                            land1               = ls_header-land1
*                            toplam_tut          = ls_keybalance-lc_bal
*                            vadesi_gelen_tut    = ls_header-wrbtr
*                            musteri_borc        = ls_keybalance_c-lc_bal
*                            kdvli_tut           = ls_header-kdvli_tut
*                            kdvsiz_tut          = ls_header-kdvsiz_tut
*                            kdv_tut             = ls_header-kdv_tut
*                            odenecek_tutar      = ls_tutar-odenecek_tutar
*                            belgeden_kalan      = ls_tutar-belgeden_kalan
*                            detail_icon         = icon_select_detail
*                            kismi_icon          = icon_status_partly_booked
*                            detail_alv          = lt_detail_alv
*                            kismi_alv           = lt_kismi_alv
*                            onay_durum          = ls_onay_durum
*                            ) TO gs_scr_1903-sum_alv.

    CLEAR: lrs_lifnr.
    lrs_lifnr-sign   = 'I'.
    lrs_lifnr-option = 'EQ'.
    lrs_lifnr-low    = ls_header-lifnr.
    COLLECT lrs_lifnr INTO lr_lifnr.

*        COLLECT value mdrange_s_lifnr(  SIGN   = 'I'
*                                        OPTION = 'EQ'
*                                        LOW    = ls_header-lifnr ) INTO lr_lifnr.

    lv_toplam = ls_header-wrbtr + lv_toplam.

    CLEAR: ls_keybalance, lt_keybalance,
           ls_return, ls_keybalance_c, lt_keybalance_c,
           lt_detail_alv, lt_kismi_alv, ls_tutar.
  ENDLOOP.

  SORT gs_scr_1903-sum_alv BY lifnr.
  LOOP AT lt_rate ASSIGNING <fs_rate>.
    READ TABLE gs_scr_1903-sum_alv ASSIGNING <fs_sum_alv> WITH KEY lifnr = <fs_rate>-lifnr BINARY SEARCH .

    IF lv_toplam NE 0.
      <fs_rate>-rate = p_oneri / lv_toplam.
    ENDIF.

*        TRY.
*            <fs_rate>-rate = exact #( p_oneri / lv_toplam ).
*          CATCH cx_sy_conversion_rounding INTO data(exc).
*            <fs_rate>-rate = exc->value.
*        ENDTRY.


    <fs_sum_alv>-dagitim = <fs_rate>-rate * <fs_sum_alv>-vadesi_gelen_tut .
    <fs_sum_alv>-dagitim = floor( <fs_sum_alv>-dagitim ) .

  ENDLOOP.

ENDFORM.                    "get_appover_data
*&---------------------------------------------------------------------*
*&      Form  get_list_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_list_report.
  DATA: ls_return       TYPE bapireturn,
        lt_keybalance   TYPE TABLE OF bapi3008_3,
        lt_keybalance_c TYPE TABLE OF bapi3007_3.
*        lt_header       TYPE TABLE OF zfi_so_dt_07,
*        lv_sektor(10).

  DATA: BEGIN OF lt_header OCCURS 0,
          bukrs        LIKE bsik-bukrs,
          lifnr        LIKE bsik-lifnr,
          satici_adi   LIKE lfa1-name1,
          land1        LIKE lfa1-land1,
          laufd        LIKE zsog_fi_018_t_01-laufd,
          laufi        LIKE zsog_fi_018_t_01-laufi,
          kunnr        LIKE knb1-kunnr,
          satici_grubu LIKE lfa1-ktokk,
*          sektor       LIKE zsog_fi_018_t_01-sektor,"ozans
          wrbtr        LIKE bsik-wrbtr,
          kdvli_tut    LIKE bsik-wrbtr,
          kdvsiz_tut   LIKE bsik-wrbtr,
          kdv_tut      LIKE bsik-wrbtr,
        END OF lt_header.
  DATA: ls_header       LIKE LINE OF lt_header.

  TYPES : BEGIN OF ltt_tutar,
            odenecek_tutar TYPE bsik-wrbtr,
            belgeden_kalan TYPE bsik-wrbtr,
          END OF ltt_tutar.

  DATA: ls_tutar      TYPE  ltt_tutar.


  DATA: lt_onay_durum TYPE zsog_fi_018_t_01 OCCURS 0.
  DATA: ls_onay_durum LIKE LINE OF lt_onay_durum.
  DATA: ls_oneri LIKE LINE OF lt_onay_durum.
  DATA: ls_keybalance   LIKE LINE OF lt_keybalance.
  DATA: ls_keybalance_c LIKE LINE OF lt_keybalance_c.
  DATA: lt_detail_alv   LIKE gs_scr_1903-detail.
  DATA: lt_kismi_alv    LIKE gs_scr_1903-kismi.
  DATA: lt_kismi_diger  LIKE gs_scr_1903-kismi_diger.
  DATA: ls_kismi_diger  LIKE LINE OF lt_kismi_diger.
  FIELD-SYMBOLS : <fs_kismi_alv> LIKE LINE OF gs_scr_1903-kismi.
  FIELD-SYMBOLS : <fs_detail> LIKE LINE OF gs_scr_1903-detail.
  DATA: ls_s_01 TYPE zsog_fi_018_s_01.
*    DATA: lrs_lifnr LIKE LINE OF lr_lifnr.

  FIELD-SYMBOLS : <fs_sum_alv> LIKE LINE OF gs_scr_1903-sum_alv.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 10:54:38
*  DATA: BEGIN OF ls_kismi.
*  DATA: zbd1t TYPE bsik-zbd1t,
*        zfbdt TYPE bsik-zfbdt,
*        kismi TYPE zsog_fi_018_s_004.
*  DATA: END OF ls_kismi.
*  DATA: lt_kismi LIKE TABLE OF ls_kismi.
  DATA: lt_kismi TYPE TABLE OF  zsog_fi_018_s_004.
  DATA: ls_kismi TYPE zsog_fi_018_s_004.
*}     <<<- End of   Inserted - 11.03.2020 10:54:38

*  PERFORM domain_get_value USING s_sektr-low 'ZSOG_FI_018_SEKTOR_DM' CHANGING lv_sektor.

  SELECT * FROM zsog_fi_018_t_01 INTO CORRESPONDING FIELDS OF TABLE lt_onay_durum
                                  WHERE laufd = s_laufd-low
                                    AND laufi = s_laufi-low
                                    AND bukrs = s_bukrs-low
                                    AND lifnr IN s_lifnr
                                    AND onay_durum EQ '05'
*                                    AND sektor IN s_sektr
                                    ORDER BY laufd laufi bukrs lifnr.
  IF  sy-subrc  NE 0 .
    MESSAGE i011.
    LEAVE LIST-PROCESSING.
  ENDIF.

*  SELECT * FROM zfi_so_dt_13
*           INTO TABLE @data(lt_header_data)
*           WHERE laufd IN @s_laufd
*             AND laufi IN @s_laufi
*             AND bukrs IN @s_bukrs
*             AND sektor IN @s_sektr.
*  lt_header = corresponding #( lt_header_data ).

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 11:50:19

  SELECT
    z1~bukrs
    z1~lifnr
    z1~satici_adi
    z1~land1
    z1~laufd
    z1~laufi
    z1~kunnr
    z1~satici_grubu
    SUM( z1~belge_tutar )      AS wrbtr
    SUM( z1~kdvli_tut )        AS kdvli_tut
    SUM( z1~kdvsiz_tut )       AS kdvsiz_tut
    SUM( z1~kdv_tut    )       AS kdv_tut
   FROM  zsog_fi_018_t_04 AS z1
   INNER JOIN bsik AS z ON z1~bukrs = z~bukrs
                       AND z1~lifnr = z~lifnr
                       AND z1~belnr = z~belnr
                       AND z1~gjahr = z~gjahr
                       AND z1~buzei = z~buzei
*   INNER JOIN lfb1 AS f ON f~bukrs = z1~bukrs
*                       AND f~lifnr = z1~lifnr
   INNER JOIN lfa1 AS f ON f~lifnr = z1~lifnr
   INTO TABLE lt_header
   WHERE z1~laufd IN s_laufd
     AND z1~laufi IN s_laufi
     AND z1~bukrs IN s_bukrs
    GROUP BY z1~bukrs z1~lifnr z1~satici_adi
             z1~land1 z1~laufd z1~laufi
             z1~kunnr z1~satici_grubu.

*}     <<<- End of   Inserted - 11.03.2020 11:50:19

  CLEAR: ls_oneri.
  READ TABLE lt_onay_durum INTO ls_oneri INDEX 1.
  p_oneri = ls_oneri-oneri_tutar.

*  SELECT
*          bukrs               ,
*          lifnr               ,
*          satici_adi          ,
*          land1               ,
*          gjahr               ,
*          belnr               ,
*          buzei               ,
*          waers               ,
*          blart               ,
*          mwskz               ,
*          shkzg               ,
*          budat               ,
*          bldat               ,
*          zfbdt               ,
*          zbd1t               ,
*          zahls               ,
*          iban                ,
*          zlspr               ,
*          tam_odeme_bicimi    ,
*          satici_grubu        ,
*          net_vade_tarihi     ,
*          bankl               ,
*          bankn               ,
*          koinh               ,
*          xezer               ,
*          banka               ,
*          zlsch               ,
*          hbkid               ,
*          laufd               ,
*          laufi               ,
*          belge_tutar         ,
*          kdvli_tut           ,
*          kdvsiz_tut          ,
*          kdv_tut             ,
*          kalan_odeme         ,
*          cat_satici_odeme    ,
*          cat_devlet_odeme    ,
*          cat_onay            ,
*          ony_satici_odeme    ,
*          ony_devlet_odeme    ,
*          ony_onay            ,
*          tam_odeme           ,
*          odenecek_tutar      ,
*          belgeden_kalan
*     from zfi_so_dt_12
*      into corresponding fields of table @gs_scr_1903-detail
*      where laufd in @s_laufd
*        and laufi in @s_laufi
*        and bukrs in @s_bukrs
*        and sektor in @s_sektr.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 11:55:07

  SELECT
   z4~mandt
   z4~laufd
   z4~laufi
   z4~bukrs
   z4~lifnr
   z4~belnr
   z4~gjahr
   z4~buzei
   z4~secim
   z4~satici_grubu
   z4~satici_adi
   z4~kunnr
   z4~land1
   z4~iban
   z4~waers
   z4~shkzg
   z4~blart
   z4~mwskz
   z4~budat
   z4~bldat
   z4~zfbdt
   z4~zbd1t
   z4~net_vade_tarihi
   z4~kismi_odeme_icon
   z4~zlsch
   z4~hbkid
   z4~bankl
   z4~bankn
   z4~banka
   z4~koinh
   z4~xezer
   z4~zahls
   z4~zlspr
   z4~tam_odeme_bicimi
   z4~belge_tutar
   z4~kdvli_tut
   z4~kdvsiz_tut
   z4~kdv_tut
   z4~kalan_odeme
   z4~cat_satici_odeme
   z4~cat_devlet_odeme
   z4~cat_onay
   z4~ony_satici_odeme
   z4~ony_devlet_odeme
   z4~ony_onay
   z4~uname
   z4~statu
   z4~onay_durum
   z4~icon_name
   z4~icon_text
   z4~datum
   z4~uzeit
   z4~tam_odeme
   z4~odenecek_tutar
   z4~belgeden_kalan
   FROM zsog_fi_018_t_04 AS z4
   INNER JOIN bsik AS bs ON bs~bukrs = z4~bukrs
                        AND bs~lifnr = z4~lifnr
                        AND bs~belnr = z4~belnr
                        AND bs~gjahr = z4~gjahr
                        AND bs~buzei = z4~buzei
                        AND bs~mandt = z4~mandt
*   INNER JOIN lfb1 AS l1 ON l1~bukrs = z4~bukrs
*                        AND l1~lifnr = z4~lifnr
*                        AND l1~mandt = z4~mandt
   INNER JOIN lfa1 AS l1 ON l1~lifnr = z4~lifnr
   INTO CORRESPONDING FIELDS OF TABLE gs_scr_1903-detail
   WHERE z4~laufd  IN s_laufd
     AND z4~laufi  IN s_laufi
     AND z4~bukrs  IN s_bukrs.

*}     <<<- End of   Inserted - 11.03.2020 11:55:07

*    "kismi ödeme belgeleri alınıyor.
*    SELECT  laufd,
*            laufi,
*            bukrs,
*            lifnr,
*            satici_grubu,
*            satici_adi,
*            waers,
*            belnr,
*            gjahr,
*            buzei,
*            shkzg,
*            blart,
*            budat,
*            bldat,
*            zfbdt,
*            zbd1t,
*            rebzg,
*            rebzj,
*            wrbtr,
*            ktokk,
*            net_vade_tarihi
*        from zfi_so_dt_16(
*                       p_bukrs = @s_bukrs-low ,
*                       p_laufd = @s_laufd-low ,
*                       p_laufi = @s_laufi-low   )
*         INTO TABLE @gs_scr_1903-kismi
*         WHERE lifnr IN @s_lifnr
*           AND belnr IN @s_belnr
*           AND satici_grubu IN @s_group
*           AND sektor IN @s_sektr.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 11:56:34

  CLEAR lt_kismi.
  SELECT
  b~bukrs
  b~lifnr
  l~name1 AS satici_adi
  b~waers
  b~belnr
  b~gjahr
  b~buzei
  b~rebzg
  b~rebzj
  b~shkzg
  b~blart
  b~budat
  b~bldat
  b~zfbdt
  b~zbd1t
  b~wrbtr
  l~ktokk
  FROM bsik AS b
  INNER JOIN lfa1 AS l ON b~lifnr = l~lifnr
  INNER JOIN lfb1 AS f ON b~bukrs = f~bukrs
                      AND b~lifnr = f~lifnr
  INNER JOIN zsog_fi_018_t_04 AS z  ON b~bukrs = z~bukrs
                                   AND b~rebzg = z~belnr
                                   AND b~rebzj = z~gjahr
  INNER JOIN zsog_fi_018_t_01 AS z1 ON z~laufd = z1~laufd
                                   AND z~laufi = z1~laufi
                                   AND z~bukrs = z1~bukrs
                                   AND z~lifnr = z1~lifnr
  INTO CORRESPONDING FIELDS OF TABLE lt_kismi
  WHERE b~lifnr IN s_lifnr
    AND b~belnr IN s_belnr
    AND z~satici_grubu IN s_group
*    AND z1~sektor IN s_sektr
    AND b~bukrs = s_bukrs-low
    AND z~laufd = s_laufd-low
    AND z~laufi = s_laufi-low
    AND ( b~blart = 'KS' ).
*  IF sy-subrc <> 0.
*    "ozans
*    SELECT
*    b~bukrs
*    b~lifnr
*    l~name1 AS satici_adi
*    b~waers
*    b~belnr
*    b~gjahr
*    b~buzei
*    b~rebzg
*    b~rebzj
*    b~shkzg
*    b~blart
*    b~budat
*    b~bldat
*    b~zfbdt
*    b~zbd1t
*    b~wrbtr
*    l~ktokk
*    FROM bsak AS b
*    INNER JOIN lfa1 AS l ON b~lifnr = l~lifnr
*    INNER JOIN lfb1 AS f ON b~bukrs = f~bukrs
*                        AND b~lifnr = f~lifnr
*    INNER JOIN zsog_fi_018_t_04 AS z  ON b~bukrs = z~bukrs
*                                     AND b~rebzg = z~belnr
*                                     AND b~rebzj = z~gjahr
*    INNER JOIN zsog_fi_018_t_01 AS z1 ON z~laufd = z1~laufd
*                                     AND z~laufi = z1~laufi
*                                     AND z~bukrs = z1~bukrs
*                                     AND z~lifnr = z1~lifnr
*    APPENDING CORRESPONDING FIELDS OF TABLE lt_kismi
*    WHERE b~lifnr IN s_lifnr
*      AND b~belnr IN s_belnr
*      AND z~satici_grubu IN s_group
**    AND z1~sektor IN s_sektr
*      AND b~bukrs = s_bukrs-low
*      AND z~laufd = s_laufd-low
*      AND z~laufi = s_laufi-low
*      AND ( b~blart = 'KS' ).
*  ENDIF.



  LOOP AT lt_kismi INTO ls_kismi.
    ls_kismi-net_vade_tarihi = ls_kismi-zbd1t + ls_kismi-zfbdt.
    ls_kismi-laufd = s_laufd-low.
    ls_kismi-laufi = s_laufi-low.
    APPEND ls_kismi TO gs_scr_1903-kismi.
  ENDLOOP.

*}     <<<- End of   Inserted - 11.03.2020 11:56:34

  DELETE ADJACENT DUPLICATES FROM gs_scr_1903-kismi COMPARING ALL FIELDS.


*      SELECT  laufd,
*              laufi,
*              bukrs,
*              lifnr,
*              satici_grubu,
*              satici_adi,
*              waers,
*              belnr,
*              gjahr,
*              buzei,
*              shkzg,
*              blart,
*              budat,
*              bldat,
*              zfbdt,
*              zbd1t,
*              rebzg,
*              rebzj,
*              wrbtr,
*              ktokk,
*              net_vade_tarihi
*          from zfi_so_dt_14(
*                         p_bukrs = @s_bukrs-low ,
*                         p_laufd = @s_laufd-low ,
*                         p_laufi = @s_laufi-low   )
*           INTO TABLE @gs_scr_1903-kismi_diger
*           WHERE lifnr IN @s_lifnr
*             AND belnr IN @s_belnr
*             AND satici_grubu IN @s_group
*             AND sektor IN @s_sektr.

*{   ->>> Inserted by Prodea Ozan Şahin - 11.03.2020 12:06:31

  CLEAR lt_kismi_diger.
  SELECT
  b~bukrs
  b~lifnr
  l~name1 AS satici_adi
  b~waers
  b~belnr
  b~gjahr
  b~buzei
  b~rebzg
  b~rebzj
  b~shkzg
  b~blart
  b~budat
  b~bldat
  b~zfbdt
  b~zbd1t
  b~wrbtr
  l~ktokk
  FROM bsik AS b
  INNER JOIN lfa1 AS l ON b~lifnr = l~lifnr
  INNER JOIN lfb1 AS f ON b~bukrs = f~bukrs
                      AND b~lifnr = f~lifnr
  INNER JOIN zsog_fi_018_t_04 AS z  ON b~bukrs = z~bukrs
                                   AND b~rebzg = z~belnr
                                   AND b~rebzj = z~gjahr
  INNER JOIN zsog_fi_018_t_01 AS z1 ON z~laufd = z1~laufd
                                   AND z~laufi = z1~laufi
                                   AND z~bukrs = z1~bukrs
                                   AND z~lifnr = z1~lifnr
  INTO CORRESPONDING FIELDS OF TABLE lt_kismi_diger
  WHERE b~lifnr IN s_lifnr
    AND b~belnr IN s_belnr
    AND z~satici_grubu IN s_group
*    AND z1~sektor IN s_sektr
    AND b~bukrs = s_bukrs-low
    AND ( b~blart = 'KS' OR b~blart = 'ZP' ).

  LOOP AT lt_kismi_diger INTO ls_kismi_diger.
    ls_kismi_diger-net_vade_tarihi = ls_kismi-zbd1t + ls_kismi-zfbdt.
    ls_kismi_diger-laufd = s_laufd-low.
    ls_kismi_diger-laufi = s_laufi-low.
    APPEND ls_kismi_diger TO gs_scr_1903-kismi_diger.
  ENDLOOP.

*}     <<<- End of   Inserted - 11.03.2020 12:06:31

  DELETE ADJACENT DUPLICATES FROM gs_scr_1903-kismi_diger COMPARING ALL FIELDS.

  LOOP AT lt_header INTO ls_header.

    PERFORM bapi_ap_acc_getkeydatebalance TABLES lt_keybalance USING ls_header  CHANGING ls_return.
    IF lt_keybalance IS NOT INITIAL .
      CLEAR: ls_keybalance.
      READ TABLE lt_keybalance INTO ls_keybalance INDEX 1.
*            data(ls_keybalance) = lt_keybalance[ 1 ].
    ENDIF.

    IF ls_header-kunnr IS NOT INITIAL.
      PERFORM bapi_ar_acc_getkeydatebalance TABLES lt_keybalance_c USING ls_header CHANGING ls_return.

      IF lt_keybalance_c IS NOT INITIAL.
*              data(ls_keybalance_c) = lt_keybalance_c[ 1 ].
        CLEAR: ls_keybalance_c.
        READ TABLE lt_keybalance_c INTO ls_keybalance_c INDEX 1.
      ENDIF.
    ENDIF.
*          data(lt_detail_alv) =  filter #( gs_scr_1903-detail USING KEY lifnr
*                                             WHERE lifnr = ls_header-lifnr ).
*
*          data(lt_kismi_alv) =  filter #( gs_scr_1903-kismi USING KEY lifnr
*                                   WHERE lifnr = ls_header-lifnr ).

    lt_detail_alv[] = gs_scr_1903-detail[].
    DELETE lt_detail_alv WHERE lifnr NE ls_header-lifnr.

    lt_kismi_alv[] = gs_scr_1903-kismi[].
    DELETE lt_kismi_alv WHERE lifnr NE ls_header-lifnr.


    READ TABLE lt_onay_durum INTO ls_onay_durum WITH KEY laufd = ls_header-laufd
                                                         laufi = ls_header-laufi
                                                         bukrs = ls_header-bukrs
                                                         lifnr = ls_header-lifnr
                                                         BINARY SEARCH.
    "eğer kısmi ödeme belgesi varsa kismi ödeme belge tutarı kadar belgeden düş
    LOOP AT lt_kismi_alv ASSIGNING <fs_kismi_alv>.
      ls_header-wrbtr = ls_header-wrbtr - <fs_kismi_alv>-wrbtr.
    ENDLOOP.

    lt_kismi_diger[] = gs_scr_1903-kismi_diger[].
    DELETE lt_kismi_diger WHERE lifnr NE ls_header-lifnr.
*
*          data(lt_kismi_diger) =  filter #( gs_scr_1903-kismi_diger USING KEY lifnr
*                                WHERE lifnr = ls_header-lifnr ).

    LOOP AT lt_detail_alv ASSIGNING <fs_detail>.
      ls_tutar-odenecek_tutar = ls_tutar-odenecek_tutar + <fs_detail>-odenecek_tutar.
      ls_tutar-belgeden_kalan = ls_tutar-belgeden_kalan + <fs_detail>-belgeden_kalan.
    ENDLOOP.

    CLEAR: ls_s_01.

    ls_s_01-laufd               = ls_header-laufd.
    ls_s_01-laufi               = ls_header-laufi.
    ls_s_01-bukrs               = ls_header-bukrs.
    ls_s_01-lifnr               = ls_header-lifnr.
    ls_s_01-satici_adi          = ls_header-satici_adi.
    ls_s_01-kunnr               = ls_header-kunnr.
*    ls_s_01-sektor              = s_sektr-low.
*    ls_s_01-sektor_tanim        = lv_sektor.
    ls_s_01-ktokk               = ls_header-satici_grubu.
    ls_s_01-land1               = ls_header-land1.
    ls_s_01-toplam_tut          = ls_keybalance-lc_bal.
    ls_s_01-vadesi_gelen_tut    = ls_header-wrbtr.
    ls_s_01-musteri_borc        = ls_keybalance_c-lc_bal.
    ls_s_01-kdvli_tut           = ls_header-kdvli_tut.
    ls_s_01-kdvsiz_tut          = ls_header-kdvsiz_tut.
    ls_s_01-kdv_tut             = ls_header-kdv_tut.
    ls_s_01-odenecek_tutar      = ls_tutar-odenecek_tutar.
    ls_s_01-belgeden_kalan      = ls_tutar-belgeden_kalan.
    ls_s_01-detail_icon         = icon_select_detail.
    ls_s_01-kismi_icon          = icon_status_partly_booked.
    ls_s_01-detail_alv          = lt_detail_alv.
    ls_s_01-kismi_alv           = lt_kismi_alv.
    ls_s_01-kismi_diger         = lt_kismi_diger.
    ls_s_01-onay_durum          = ls_onay_durum.

    APPEND ls_s_01 TO gs_scr_1903-sum_alv.

*          APPEND value zsog_fi_018_s_01(
*                              laufd               = ls_header-laufd
*                              laufi               = ls_header-laufi
*                              bukrs               = ls_header-bukrs
*                              lifnr               = ls_header-lifnr
*                              satici_adi          = ls_header-satici_adi
*                              kunnr               = ls_header-kunnr
*                              sektor              = s_sektr-low
*                              sektor_tanim        = lv_sektor
*                              ktokk               = ls_header-satici_grubu
*                              land1               = ls_header-land1
*                              toplam_tut          = ls_keybalance-lc_bal
*                              vadesi_gelen_tut    = ls_header-wrbtr
*                              musteri_borc        = ls_keybalance_c-lc_bal
*                              kdvli_tut           = ls_header-kdvli_tut
*                              kdvsiz_tut          = ls_header-kdvsiz_tut
*                              kdv_tut             = ls_header-kdv_tut
*                              odenecek_tutar      = ls_tutar-odenecek_tutar
*                              belgeden_kalan      = ls_tutar-belgeden_kalan
*                              detail_icon         = icon_select_detail
*                              kismi_icon          = icon_status_partly_booked
*                              detail_alv          = lt_detail_alv
*                              kismi_alv           = lt_kismi_alv
*                              kismi_diger         = lt_kismi_diger
*                              onay_durum          = ls_onay_durum
*                              ) TO gs_scr_1903-sum_alv.


    CLEAR: ls_keybalance, lt_keybalance,
           ls_return, ls_keybalance_c,  lt_keybalance_c,
           lt_detail_alv, lt_kismi_alv, lt_kismi_diger, ls_tutar .
  ENDLOOP.
  IF gs_scr_1903-sum_alv IS INITIAL.
    MESSAGE i011.
    LEAVE LIST-PROCESSING.
  ENDIF.

  "ozans
  SORT  gs_scr_1903-sum_alv BY laufd laufi bukrs lifnr.
ENDFORM.                    "get_list_report
*&---------------------------------------------------------------------*
*&      Form  get_stock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LR_LIFNR   text
*----------------------------------------------------------------------*
FORM get_stock TABLES lr_lifnr STRUCTURE mdrange_s_lifnr.

  DATA: lr_budat TYPE /gc1/tab_rng_date.
  DATA: lrs_budat LIKE LINE OF lr_budat.

  IF gs_scr_1903-sum_alv IS  NOT INITIAL.
    TYPES : BEGIN OF ltt_itab_alv ,
              bukrs   LIKE bsik-bukrs,
              lifnr   LIKE bsik-lifnr,
              name1   LIKE lfa1-name1,
              lifnr1  LIKE ekko-lifnr,
              zterm   LIKE lfb1-zterm,
              dmbtr   LIKE bsik-dmbtr,
              endwert TYPE dmbtr,
              wrbtr   LIKE bsik-wrbtr,
              aubet   LIKE kond-aubet,
              mwskz   LIKE eine-mwskz,
              stcd3   LIKE lfa1-stcd3,
              sperr   LIKE lfb1-sperr,
            END OF ltt_itab_alv.

    DATA: lt_itab_alv TYPE TABLE OF ltt_itab_alv.
    DATA: lo_data TYPE REF TO data.
    FIELD-SYMBOLS: <lt_outtab>  LIKE lt_itab_alv.
    FIELD-SYMBOLS: <fs_sum_alv> LIKE LINE OF gs_scr_1903-sum_alv.
    FIELD-SYMBOLS: <fs_outtab>  LIKE LINE OF lt_itab_alv.

    "Let know the model
    cl_salv_bs_runtime_info=>set(
     EXPORTING
       display  = abap_false
       metadata = abap_false
       data     = abap_true ).


*    APPEND value range_budat_s( SIGN = 'I'
*                                OPTION = 'EQ'
*                                LOW    = sy-datum ) TO lr_budat.

    CLEAR:lrs_budat.
    lrs_budat-sign   = 'I'  .
    lrs_budat-option = 'EQ'  .
    lrs_budat-low    = sy-datum.
    APPEND lrs_budat TO lr_budat.


*    SUBMIT zfi_r_satode WITH s_bukrs  IN s_bukrs
    SUBMIT zsog_fi_018_satode WITH s_bukrs  IN s_bukrs
                              WITH s_ekorg  IN s_bukrs
                              WITH s_lifnr1 IN lr_lifnr
                              WITH s_budat  IN lr_budat
                              WITH p_days   EQ '180' AND RETURN.

    TRY.
        " get data from SALV model
        cl_salv_bs_runtime_info=>get_data_ref(
              IMPORTING
                r_data = lo_data
        ).

        ASSIGN lo_data->* TO <lt_outtab>.
        cl_salv_bs_runtime_info=>clear( ).
      CATCH cx_salv_bs_sc_runtime_info.
    ENDTRY.
    IF <lt_outtab> IS ASSIGNED.
      SORT <lt_outtab> BY lifnr.
      LOOP AT gs_scr_1903-sum_alv ASSIGNING <fs_sum_alv>.
        READ TABLE <lt_outtab> ASSIGNING <fs_outtab> WITH KEY lifnr = <fs_sum_alv>-lifnr BINARY SEARCH.
        IF <fs_outtab> IS ASSIGNED.
          <fs_sum_alv>-toplam_stok = <fs_outtab>-endwert.
          UNASSIGN <fs_outtab>.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    "get_stock
*&---------------------------------------------------------------------*
*& Form CALL_POPUP_ALV
*&---------------------------------------------------------------------*
FORM call_popup_alv  TABLES pt_table.

  DATA: lo_popup_alv   TYPE REF TO cl_salv_table.
  DATA: lo_functions   TYPE REF TO cl_salv_functions_list.
  DATA: lr_columns     TYPE REF TO cl_salv_columns_table.
  DATA: lr_column      TYPE REF TO cl_salv_column_table.


  cl_salv_table=>factory(
     IMPORTING
       r_salv_table   = lo_popup_alv
    CHANGING
      t_table        = pt_table[] ).

  lo_functions = lo_popup_alv->get_functions( ).
  lo_functions->set_default( 'X' ).

  lr_columns = lo_popup_alv->get_columns( ).
  lr_columns->set_optimize( ).

* ALV as Popup
  lo_popup_alv->set_screen_popup(
    start_column = 10
    end_column   = 180
    start_line   = 1
    end_line     = 20 ).

* Display
  lo_popup_alv->display( ).

ENDFORM.                    "call_popup_alv
*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_data.
  DATA: lv_datum(10).
  DATA: lt_tab TYPE esp1_message_tab_type.
*  DATA: ls_tab TYPE esp1_message_wa_type.

  DATA: ls_sum_alv LIKE LINE OF gs_scr_1903-sum_alv.
  FIELD-SYMBOLS: <fs_header_tab> LIKE LINE OF gs_scr_1903-header_tab.
  FIELD-SYMBOLS: <fs_item_tab> LIKE LINE OF gs_scr_1903-item_tab.

  CASE gs_scr_1903-auth-statu.
    WHEN '01'.
      LOOP AT gs_scr_1903-sum_alv INTO ls_sum_alv WHERE onay_durum-onay_durum EQ '04'.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '01' .
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.
          WRITE s_laufd-low TO lv_datum DD/MM/YYYY.
          PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'W' '005' lv_datum s_laufi-low '' ''.
          PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                          text-015 '' '' ''.
        ENDIF.
        LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '01' AND  onay_durum NE '02'.
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.
          PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'W' '000' text-016
                                                          '' '' ''.

          PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                           text-017 '' '' ''.
        ENDIF.

        LOOP AT gs_scr_1903-item_tab ASSIGNING <fs_item_tab> WHERE laufd NE s_laufd-low OR laufi NE s_laufi-low.
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.
          WRITE <fs_item_tab>-laufd  TO lv_datum DD/MM/YYYY.
          PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                          text-018 '' '' ''.
          PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'W' '005' lv_datum <fs_item_tab>-laufi '' ''.

        ENDIF.
      ELSE.
        WRITE s_laufd-low TO lv_datum DD/MM/YYYY.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'W' '005' lv_datum s_laufi-low '' ''.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'W' '010' '' '' '' ''.
      ENDIF.
    WHEN '02'.

      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '02' .
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        WRITE s_laufd-low TO lv_datum DD/MM/YYYY.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'W' '005' lv_datum s_laufi-low '' ''.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                        text-031 '' '' ''.
      ENDIF.
      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '02' AND  onay_durum NE '03'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'W' '000' text-032
                                                        '' '' ''.

        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                         text-017 '' '' ''.
      ENDIF.

      LOOP AT gs_scr_1903-item_tab ASSIGNING <fs_item_tab> WHERE laufd NE s_laufd-low OR laufi NE s_laufi-low.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        WRITE <fs_item_tab>-laufd  TO lv_datum DD/MM/YYYY.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                        text-018 '' '' ''.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'W' '005' lv_datum <fs_item_tab>-laufi '' ''.
      ENDIF.

    WHEN '03'.
      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '03' .
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        WRITE s_laufd-low TO lv_datum DD/MM/YYYY.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'W' '005' lv_datum s_laufi-low '' ''.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                        text-033 '' '' ''.
      ENDIF.
      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '03' AND  onay_durum NE '04'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'W' '000' text-034
                                                        '' '' ''.

        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                         text-017 '' '' ''.
      ENDIF.

      LOOP AT gs_scr_1903-item_tab ASSIGNING <fs_item_tab> WHERE laufd NE s_laufd-low OR laufi NE s_laufi-low.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        WRITE <fs_item_tab>-laufd  TO lv_datum DD/MM/YYYY.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                        text-018 '' '' ''.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'W' '005' lv_datum <fs_item_tab>-laufi '' ''.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.


  IF lt_tab IS NOT INITIAL.
    PERFORM c14z_messages_show_as_popup TABLES lt_tab.
  ENDIF.

*  PERFORM msg_display_error_table TABLES lt_return.
ENDFORM.                    "check_data
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_ALV
*&---------------------------------------------------------------------*
FORM initialize_alv TABLES alv USING pfstatus TYPE sypfkey.
  TRY.
    cl_salv_table=>factory(
    IMPORTING
      r_salv_table = gs_scr_1903-r_alv
    CHANGING
      t_table      = alv[] ).

    gs_scr_1903-r_columns = gs_scr_1903-r_alv->get_columns( ).

    PERFORM enable_layout_settings.
    PERFORM optimize_column_width.
    PERFORM hide_client_column.
    PERFORM set_icon.
    PERFORM set_column_names.
    PERFORM set_toolbar USING pfstatus.
    PERFORM display_settings TABLES alv[].
    PERFORM set_hotspot_click.
    PERFORM hide_function.

    " ...
    " PERFORM setting_n.
*    CATCH cx_salv_msg INTO data(message).
    " error handling
  ENDTRY.
ENDFORM.                    "initialize_alv
*&---------------------------------------------------------------------*
*&      Form  enable_layout_settings
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM enable_layout_settings.
  DATA layout_key      TYPE salv_s_layout_key.
  DATA: layout_settings TYPE REF TO cl_salv_layout.

  layout_settings = gs_scr_1903-r_alv->get_layout( ).
  layout_key-report = sy-repid.
  layout_settings->set_key( layout_key ).
  layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).

ENDFORM.                    "ENABLE_LAYOUT_SETTINGS

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM optimize_column_width.

  gs_scr_1903-r_columns->set_optimize( ).
ENDFORM.                    "OPTIMIZE_COLUMN_WIDTH

*&---------------------------------------------------------------------*
*  DATA not_found TYPE REF TO cx_salv_not_found.
*&---------------------------------------------------------------------*
FORM hide_client_column.
  TRY.
      gs_scr_1903-r_column = gs_scr_1903-r_columns->get_column( 'DETAIL_ALV' ).
      gs_scr_1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
      " error handling
  ENDTRY.

  TRY.
      gs_scr_1903-r_column = gs_scr_1903-r_columns->get_column( 'KISMI_ALV' ).
      gs_scr_1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
      " error handling
  ENDTRY.

  TRY.
      gs_scr_1903-r_column = gs_scr_1903-r_columns->get_column( 'KISMI_DIGER' ).
      gs_scr_1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
      " error handling
  ENDTRY.


  TRY.
      gs_scr_1903-r_column = gs_scr_1903-r_columns->get_column( 'SECIM' ).
      gs_scr_1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
      " error handling
  ENDTRY.

  TRY.
      gs_scr_1903-r_column = gs_scr_1903-r_columns->get_column( 'ONAY_DURUM' ).
      gs_scr_1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found .
      " error handling
  ENDTRY.
**********************************************************************

  TRY.
      gs_scr_1903-r_column = gs_scr_1903-r_columns->get_column( 'KDVLI_TUT' ).
      gs_scr_1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found .
      " error handling
  ENDTRY.

  TRY.
      gs_scr_1903-r_column = gs_scr_1903-r_columns->get_column( 'KDVSIZ_TUT' ).
      gs_scr_1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found .
      " error handling
  ENDTRY.

  TRY.
      gs_scr_1903-r_column = gs_scr_1903-r_columns->get_column( 'KDV_TUT' ).
      gs_scr_1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found .
      " error handling
  ENDTRY.

  TRY.
      gs_scr_1903-r_column = gs_scr_1903-r_columns->get_column( 'SEKTOR' ).
      gs_scr_1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found .
      " error handling
  ENDTRY.

  TRY.
      gs_scr_1903-r_column = gs_scr_1903-r_columns->get_column( 'SEKTOR_TANIM' ).
      gs_scr_1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found .
      " error handling
  ENDTRY.

  TRY.
      gs_scr_1903-r_column = gs_scr_1903-r_columns->get_column( 'MWSKZ' ).
      gs_scr_1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found .
      " error handling
  ENDTRY.

  TRY.
      gs_scr_1903-r_column = gs_scr_1903-r_columns->get_column( 'TOPLAM_STOK' ).
      gs_scr_1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found .
      " error handling
  ENDTRY.

  TRY.
      gs_scr_1903-r_column = gs_scr_1903-r_columns->get_column( 'F_TOPLAM_STOK' ).
      gs_scr_1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found .
      " error handling
  ENDTRY.

  TRY.
      gs_scr_1903-r_column = gs_scr_1903-r_columns->get_column( 'DAGITIM' ).
      gs_scr_1903-r_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found .
      " error handling
  ENDTRY.

ENDFORM.                    " HIDE_CLIENT_COLUMN
*&---------------------------------------------------------------------*
*&      Form  set_icon
*&---------------------------------------------------------------------*
FORM set_icon.
  DATA: lr_columns TYPE REF TO cl_salv_columns_table,
        lr_column  TYPE REF TO cl_salv_column_table.
**
  TRY.
      lr_columns = gs_scr_1903-r_alv->get_columns( ).
    CATCH cx_salv_not_found .

  ENDTRY.
*  lr_column ?= lr_columns->get_column( 'LIFNR' ).
*  lr_column->set_icon( if_salv_c_bool_sap=>true ).
**

  TRY.
      lr_column ?= lr_columns->get_column( 'BELNR' ).
      lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    CATCH cx_salv_not_found .

  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'DETAIL_ICON' ).
      lr_column->set_icon( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found .

  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'DETAIL_ICON' ).
      lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    CATCH cx_salv_not_found .

  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'KISMI_ICON' ).
      lr_column->set_icon( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.

  ENDTRY.

  TRY.
      lr_column ?= lr_columns->get_column( 'KISMI_ICON' ).
      lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    CATCH cx_salv_not_found .

  ENDTRY.

  IF gs_scr_1903-name_of_alv IS INITIAL.
    TRY.
        lr_column ?= lr_columns->get_column( 'LIFNR' ).
        lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      CATCH cx_salv_not_found .

    ENDTRY.
  ENDIF.

  IF gs_scr_1903-name_of_alv EQ 'KISMI_ALV'.
    TRY.
        lr_column ?= lr_columns->get_column( 'REBZG' ).
        lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      CATCH cx_salv_not_found .

    ENDTRY.
  ENDIF.

ENDFORM.                    "set_icon
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM set_column_names.

*  DATA not_found TYPE REF TO cx_salv_not_found.
*
*  TRY.
*      gr_column = gr_columns->get_column( 'WAVWR' ).
*      gr_column->set_short_text( 'Maliyet' ).
*      gr_column->set_medium_text( 'Maliyet' ).
*      gr_column->set_long_text( 'Maliyet' ).
*    CATCH cx_salv_not_found INTO not_found.
*      " error handling
*  ENDTRY.
ENDFORM.                    " SET_DEPARTURE_COUNTRY_COLUMN
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM set_toolbar USING pfstatus.

  gs_scr_1903-r_alv->set_screen_status(
    pfstatus      = pfstatus
    report        = sy-repid
    set_functions = gs_scr_1903-r_alv->c_functions_all ).

ENDFORM.                    " SET_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  hide_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM hide_function.
  "eğer gui statuse yeni bir buton eklemek gerekirse,
  "ve yetkiye göre butonların kapatılması gerekiyorsa,
  "bu programdaki gui statüsün yeni bir gui statüse,
  "kopyalanması gerekiyor. Aksi halde halde yeni eklenen buton,
  "function list içerisine gelmiyor. Burada bir bug var.

  DATA: ls_sum_alv LIKE LINE OF gs_scr_1903-sum_alv.
  FIELD-SYMBOLS: <fs_header_tab> LIKE LINE OF gs_scr_1903-header_tab,
                 <fs_item_tab>   LIKE LINE OF gs_scr_1903-item_tab.


  DATA: lv_datum(10),
        lv_subrc TYPE sy-subrc VALUE 4.
  CASE gs_scr_1903-auth-statu.
    WHEN '01'.
      PERFORM hide_function_single USING '&KAYIT'.
      PERFORM hide_function_single USING '&HEPSDAGIT'."inserted by ozans - 13.06.2020
      PERFORM hide_function_single USING '&TEST'."inserted by ozans - 13.06.2020
      LOOP AT gs_scr_1903-sum_alv INTO ls_sum_alv WHERE onay_durum-onay_durum EQ '04'.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '01' .
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.
          PERFORM hide_function_single USING '&CATG'.
        ENDIF.
        LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '01' AND  onay_durum NE '02'.
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.
          PERFORM hide_function_single USING '&GERI'.
        ENDIF.

        LOOP AT gs_scr_1903-item_tab ASSIGNING <fs_item_tab> WHERE laufd NE s_laufd-low OR laufi NE s_laufi-low.
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.
          PERFORM hide_function_single USING '&CATG'.
        ENDIF.

        LOOP AT gs_scr_1903-item_tab ASSIGNING <fs_item_tab> WHERE onay_durum NE '01' AND  onay_durum NE '02'
                                                                             AND ( laufd NE s_laufd-low OR laufi NE s_laufi-low ).
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.
          PERFORM hide_function_single USING '&GERI'.
        ENDIF.

        LOOP AT gs_scr_1903-item_tab ASSIGNING <fs_item_tab> WHERE onay_durum NE '04'.
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.
          PERFORM hide_function_single USING '&KAYIT'.
        ENDIF.
        IF gs_scr_1903-item_tab IS INITIAL AND gs_scr_1903-header_tab IS INITIAL.
          PERFORM hide_function_single USING '&GERI'.
          PERFORM hide_function_single USING '&KAYIT'.
        ENDIF.

        PERFORM hide_function_single USING '&LISTEH'.
        PERFORM hide_function_single USING '&ONAYCI'.
      ELSE.  "eğer buraya geliyorsa onay durumu 04'tür ve sadece kaydet işlemi yapabilir.


        PERFORM hide_function_single USING '&CATG'.
        PERFORM hide_function_single USING '&GERI'.
        PERFORM hide_function_single USING '&LISTEH'.
        PERFORM hide_function_single USING '&ONAYCI'.
        PERFORM show_function_single USING '&KAYIT'.
      ENDIF.

    WHEN '02'.
      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '02' .
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM hide_function_single USING '&ONAYCI'.
*        PERFORM hide_function_single USING '&GERI'.
      ENDIF.
      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '02' AND  onay_durum NE '03'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM hide_function_single USING '&GERI'.
      ENDIF.

      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum EQ '02' .
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM hide_function_single USING '&GERI'.
      ENDIF.

      LOOP AT gs_scr_1903-item_tab ASSIGNING <fs_item_tab> WHERE laufd NE s_laufd-low OR laufi NE s_laufi-low.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM hide_function_single USING '&ONAYCI'.
      ENDIF.

      LOOP AT gs_scr_1903-item_tab ASSIGNING <fs_item_tab> WHERE onay_durum NE '02' AND  onay_durum NE '03'
                                                                           AND ( laufd NE s_laufd-low OR laufi NE s_laufi-low ).
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM hide_function_single USING '&GERI'.
      ENDIF.

      IF gs_scr_1903-item_tab IS INITIAL AND gs_scr_1903-header_tab IS INITIAL.
        PERFORM hide_function_single USING '&GERI'.
      ENDIF.

      "eğer kategori ekibi ise kategoriye gönder butonu ve kaydet butonu inaktif olacak.
      PERFORM hide_function_single USING '&CATG'.
      PERFORM hide_function_single USING '&KAYIT'.
      PERFORM hide_function_single USING '&LISTEH'.

    WHEN '03'.

      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '03' .
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM hide_function_single USING '&LISTEH'.
      ENDIF.
      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '03' AND  onay_durum NE '04'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM hide_function_single USING '&GERI'.
      ENDIF.

      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum EQ '03' .
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM hide_function_single USING '&GERI'.
      ENDIF.

      LOOP AT gs_scr_1903-item_tab ASSIGNING <fs_item_tab> WHERE laufd NE s_laufd-low OR laufi NE s_laufi-low.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM hide_function_single USING '&LISTEH'.
      ENDIF.

      LOOP AT gs_scr_1903-item_tab ASSIGNING <fs_item_tab> WHERE onay_durum NE '03' AND  onay_durum NE '04'
                                                                           AND ( laufd NE s_laufd-low OR laufi NE s_laufi-low ).
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM hide_function_single USING '&GERI'.
      ENDIF.

      IF gs_scr_1903-item_tab IS INITIAL AND gs_scr_1903-header_tab IS INITIAL.
        PERFORM hide_function_single USING '&GERI'.
      ENDIF.

      "eğer onaycıysa kategoriye gönder butonu inaktif olacak.
      PERFORM hide_function_single USING '&CATG'.
      PERFORM hide_function_single USING '&KAYIT'.
      PERFORM hide_function_single USING '&ONAYCI'.
      PERFORM hide_function_single USING '&HEPSDAGIT'."inserted by ozans - 13.06.2020

    WHEN OTHERS.
  ENDCASE.

*  DATA(functions) = gs_scr_1903-r_alv->get_functions( ).
*  DATA(lt_func_list) = functions->get_functions( ).
*  LOOP AT lt_func_list INTO DATA(la_func_list).
*    IF la_func_list-r_function->get_name( ) = '&CATG'.
*      la_func_list-r_function->set_visible( ' ' ).
*      EXIT.
*    ENDIF.
*ENDLOOP.
ENDFORM.                    "hide_function

*&---------------------------------------------------------------------*
FORM display_settings TABLES alv.
*&---------------------------------------------------------------------*
*  DATA display_settings TYPE REF TO cl_salv_display_settings.
  DATA: lv_tanim TYPE text70,
        lv_curr  TYPE c LENGTH 18.
*  DATA: lv_line TYPE i.
  DATA: lv_line TYPE sy-tabix.
  lv_line = lines( alv ).

  lv_tanim  = |Satıcı Ödeme | && |--> | && |{ lv_line }| && | Kayıt Bulundu|.
  IF p_oneri IS NOT INITIAL.
    WRITE p_oneri TO lv_curr.
    CONDENSE  lv_curr.
    lv_tanim =  |{ lv_tanim }| && |/|  && | Öneri Tutarı | && |= | && |{ lv_curr }|.
  ENDIF.

  DATA: display_settings TYPE REF TO cl_salv_display_settings.

  display_settings = gs_scr_1903-r_alv->get_display_settings( ).

  display_settings->set_striped_pattern( if_salv_c_bool_sap=>true ).
  display_settings->set_list_header( lv_tanim ).
ENDFORM.                    "display_settings
*&---------------------------------------------------------------------*
*&      Form  SET_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM set_hotspot_click .
*-- events
  gs_scr_1903-r_events = gs_scr_1903-r_alv->get_event( ).
  CREATE OBJECT event_handler.
  SET HANDLER event_handler->on_link_click   FOR gs_scr_1903-r_events.
  SET HANDLER event_handler->on_user_command FOR gs_scr_1903-r_events.
ENDFORM.                    "set_hotspot_click
*&---------------------------------------------------------------------*
FORM display_alv.
*&---------------------------------------------------------------------*
  gs_scr_1903-r_alv->display( ).
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  BAPI_AP_ACC_GETKEYDATEBALANCE
*&---------------------------------------------------------------------*
FORM bapi_ap_acc_getkeydatebalance  TABLES   pt_keybalance STRUCTURE bapi3008_3
                                    USING    ps_header LIKE gs_header
                                    CHANGING ps_return STRUCTURE bapireturn.

  CALL FUNCTION 'BAPI_AP_ACC_GETKEYDATEBALANCE'
    EXPORTING
      companycode = ps_header-bukrs
      vendor      = ps_header-lifnr
      keydate     = sy-datum
    IMPORTING
      return      = ps_return
    TABLES
      keybalance  = pt_keybalance.

ENDFORM.                    "bapi_ap_acc_getkeydatebalance
*&---------------------------------------------------------------------*
*&      Form  BAPI_AR_ACC_GETKEYDATEBALANCE
*&---------------------------------------------------------------------*
FORM bapi_ar_acc_getkeydatebalance  TABLES   pt_keybalance_c STRUCTURE bapi3007_3
                                    USING    ps_header LIKE gs_header
                                    CHANGING ps_return STRUCTURE bapireturn.

  CALL FUNCTION 'BAPI_AR_ACC_GETKEYDATEBALANCE'
    EXPORTING
      companycode = ps_header-bukrs
      customer    = ps_header-kunnr
      keydate     = sy-datum
    IMPORTING
      return      = ps_return
    TABLES
      keybalance  = pt_keybalance_c.
ENDFORM.                    "bapi_ar_acc_getkeydatebalance
*&---------------------------------------------------------------------*
*&      Form  ON_LINK_CLICK
*&---------------------------------------------------------------------*
FORM on_link_click  USING    p_row
                             p_column.
  CASE p_column.
    WHEN 'LIFNR'.
      PERFORM filter_detail_alv USING p_row p_column ''.
    WHEN 'BELNR' OR 'REBZG'.
      PERFORM click_handle USING p_row p_column.
    WHEN 'DETAIL_ICON'.
      PERFORM show_row_detail USING p_row p_column.
    WHEN 'KISMI_ICON'.
      PERFORM show_row_kismi USING p_row p_column.
  ENDCASE.

ENDFORM.                    "on_link_click
*&---------------------------------------------------------------------*
*&      Form  FILTER_DETAIL_ALV
*&---------------------------------------------------------------------*
FORM filter_detail_alv  USING    p_row
                                 p_column
                                 p_background."inserted by ozans- 13.06.2020
  CLEAR: gs_scr_1903-filter,
         gs_scr_1903-onay_durum .
  DATA: lv_tutar TYPE bseg-wrbtr.
  DATA: ls_sum LIKE LINE OF gs_scr_1903-sum_alv.
  FIELD-SYMBOLS: <fs_filter> LIKE LINE OF gs_scr_1903-filter,
                 <fs_kismi>  LIKE LINE OF ls_sum-kismi_alv.

  READ TABLE gs_scr_1903-sum_alv INTO ls_sum INDEX p_row.
*  data(ls_sum) = gs_scr_1903-sum_alv[ p_row ].

  gs_scr_1903-filter = ls_sum-detail_alv.
  IF gs_scr_1903-filter IS NOT INITIAL.
    CASE gs_scr_1903-auth-statu.
      WHEN '01'.
      WHEN '02'.
        PERFORM calculate_amount.
      WHEN '03'.
        PERFORM calculate_amount.
      WHEN OTHERS.
    ENDCASE.

*{   ->>> Inserted by Prodea Ozan Şahin - 31.03.2020 17:05:54
    "ödeme yapılan bir belgeye istinaden kalan ödeme tutarı hesaplaması
    DATA: lt_filter LIKE gs_scr_1903-filter.
    DATA: ls_filter LIKE LINE OF lt_filter.
    DATA: BEGIN OF ls_bseg,
            bukrs  LIKE bseg-bukrs ,
            belnr  LIKE bseg-belnr ,
            gjahr  LIKE bseg-gjahr ,
            buzei  LIKE bseg-buzei ,
            rebzg  LIKE bseg-rebzg ,
            rebzj  LIKE bseg-rebzj ,
            rebzz  LIKE bseg-rebzz ,
            dmbtr  LIKE bseg-dmbtr ,
            augbl  LIKE bseg-augbl ,
          END OF ls_bseg,
          lt_bseg LIKE TABLE OF ls_bseg.
    DATA: lv_dmbtr LIKE bseg-dmbtr.

    lt_filter = gs_scr_1903-filter.
    CLEAR gs_scr_1903-filter.
*** Selection screende seçilen belgeleri filtrele
    DELETE lt_filter WHERE NOT belnr IN s_belnr.

    IF lt_filter IS NOT INITIAL.
      SELECT bukrs belnr gjahr buzei
             rebzg rebzj rebzz dmbtr augbl
        FROM bseg
        INTO TABLE lt_bseg
        FOR ALL ENTRIES IN lt_filter
        WHERE bukrs = lt_filter-bukrs
          AND belnr = lt_filter-belnr
          AND gjahr = lt_filter-gjahr
          AND buzei = lt_filter-buzei.

      SORT lt_bseg BY bukrs belnr gjahr buzei.
    ENDIF.

    LOOP AT lt_filter INTO ls_filter.
      CLEAR ls_bseg.
      READ TABLE lt_bseg INTO ls_bseg WITH KEY bukrs = ls_filter-bukrs
                                               belnr = ls_filter-belnr
                                               gjahr = ls_filter-gjahr
                                               buzei = ls_filter-buzei
                                      BINARY SEARCH.
      IF sy-subrc = 0 AND ls_bseg-rebzg IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      APPEND ls_filter TO gs_scr_1903-filter.
    ENDLOOP.

*}     <<<- End of   Inserted - 31.03.2020 17:05:54

    LOOP AT gs_scr_1903-filter ASSIGNING <fs_filter>.

      LOOP AT ls_sum-kismi_alv ASSIGNING <fs_kismi> WHERE rebzg = <fs_filter>-belnr
                                                      AND rebzj = <fs_filter>-gjahr .
        lv_tutar = lv_tutar + <fs_kismi>-wrbtr.
      ENDLOOP.
      <fs_filter>-kalan_odeme =  <fs_filter>-belge_tutar - lv_tutar.

*{   ->>> Inserted by Prodea Ozan Şahin - 31.03.2020 17:28:13
      CLEAR lv_dmbtr.
*      SELECT SINGLE dmbtr FROM bseg INTO lv_dmbtr
*        WHERE bukrs = <fs_filter>-bukrs "ls_filter-bukrs "added by Prodea(ufukk)-06.04.2020 12:57:33
*          AND rebzg = <fs_filter>-belnr "ls_filter-belnr "added by Prodea(ufukk)-06.04.2020 12:57:35
*          AND rebzj = <fs_filter>-gjahr "ls_filter-gjahr "added by Prodea(ufukk)-06.04.2020 12:57:37
*          AND rebzz = <fs_filter>-buzei "ls_filter-buzei "added by Prodea(ufukk)-06.04.2020 12:57:39
*          AND rebzg <> space
*          AND augbl = space.
      LOOP AT lt_bseg INTO ls_bseg  WHERE bukrs = <fs_filter>-bukrs
                                      AND rebzg = <fs_filter>-belnr
                                      AND rebzj = <fs_filter>-gjahr
                                      AND rebzz = <fs_filter>-buzei
                                      AND rebzg <> space
                                      AND augbl = space.
        lv_dmbtr = lv_dmbtr + ls_bseg-dmbtr.
      ENDLOOP.
      IF sy-subrc = 0.
        <fs_filter>-kalan_odeme = <fs_filter>-kalan_odeme - lv_dmbtr.
      ENDIF.
*}     <<<- End of   Inserted - 31.03.2020 17:28:13

      CLEAR: lv_tutar.
    ENDLOOP.
    IF gs_scr_1903-onay_durum IS INITIAL.
      gs_scr_1903-onay_durum = ls_sum-onay_durum.
    ENDIF.

    SORT gs_scr_1903-filter BY net_vade_tarihi.
    IF p_background IS INITIAL."inserted by ozans - 13.06.2020
      CALL SCREEN 1903.
    ENDIF.
  ELSE.
    MESSAGE i003.
  ENDIF.


ENDFORM.                    "filter_detail_alv
*&---------------------------------------------------------------------*
*&      Form  calculate_amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM calculate_amount.
  "1903 ekranında toplam tutar hesaplanıyor. Kısmı ödeme belgeleri gelince
  "toplam tutardan kısmi ödeme belgelerinin tutarları çıkarılacaktır.

  DATA: ls_filter  LIKE LINE OF gs_scr_1903-filter,
        ls_sum_alv LIKE LINE OF gs_scr_1903-sum_alv.

  FIELD-SYMBOLS: <fs_filter> LIKE LINE OF gs_scr_1903-filter,
                 <fs_kismi>  LIKE LINE OF ls_sum_alv-kismi_alv.


  CLEAR:  gs_scr_1903-category.
  LOOP AT gs_scr_1903-filter INTO ls_filter.
    gs_scr_1903-category-odenecek_tutar =  0.
    gs_scr_1903-category-toplam_tutar = gs_scr_1903-category-toplam_tutar + ls_filter-belge_tutar.
  ENDLOOP.
  IF sy-subrc EQ 0.
    gs_scr_1903-category-bukrs =  ls_filter-bukrs.
    gs_scr_1903-category-lifnr =  ls_filter-lifnr.
    gs_scr_1903-category-waers =  ls_filter-waers.
    "satıcıya ait kısmi ödemelerin toplamı ilk ekrandan alınıyor.
*    data(ls_sum_alv) = gs_scr_1903-sum_alv[ lifnr  = gs_scr_1903-category-lifnr ].

    CLEAR: ls_sum_alv.
    READ TABLE gs_scr_1903-sum_alv INTO ls_sum_alv WITH KEY lifnr  = gs_scr_1903-category-lifnr.
    LOOP AT gs_scr_1903-filter ASSIGNING <fs_filter>.
      LOOP AT ls_sum_alv-kismi_alv ASSIGNING <fs_kismi> WHERE rebzg = <fs_filter>-belnr
                                                          AND rebzj = <fs_filter>-gjahr .

        ls_sum_alv-kismi_odeme = ls_sum_alv-kismi_odeme + <fs_kismi>-wrbtr.
      ENDLOOP.
    ENDLOOP.
    gs_scr_1903-category-toplam_tutar = gs_scr_1903-category-toplam_tutar - ls_sum_alv-kismi_odeme.
  ENDIF.

ENDFORM.                    "calculate_amount
*&---------------------------------------------------------------------*
*& Form CLICK_HANDLE
*&---------------------------------------------------------------------*
FORM click_handle  USING p_row
                         p_column.


  DATA: ls_detail LIKE LINE OF gs_scr_1903-detail,
        ls_sum    LIKE LINE OF gs_scr_1903-sum_alv,
        ls_kismi  LIKE LINE OF gs_scr_1903-kismi.

  FIELD-SYMBOLS: <fs_data> TYPE any.

  CASE p_column .
    WHEN 'BELNR'.
      IF gs_scr_1903-name_of_alv EQ 'ROW_ALV'.
*        READ TABLE gs_scr_1903-detail INTO ls_detail INDEX 1."commented by ozans
*        READ TABLE gs_scr_1903-sum_alv INTO ls_sum WITH KEY lifnr = ls_detail-lifnr."commented by ozans
        READ TABLE gs_scr_1903-sum_alv INTO ls_sum WITH KEY lifnr = gv_lifnr."added by ozans
        READ TABLE ls_sum-detail_alv INTO ls_detail INDEX p_row.
        ASSIGN COMPONENT p_column OF STRUCTURE ls_detail TO <fs_data>.

      ELSEIF gs_scr_1903-name_of_alv EQ 'ALL_ALV'.
        READ TABLE gs_scr_1903-detail INTO ls_detail INDEX p_row.
        ASSIGN COMPONENT p_column OF STRUCTURE ls_detail TO <fs_data>.

      ELSEIF gs_scr_1903-name_of_alv EQ 'KISMI_ALV'.
*        READ TABLE gs_scr_1903-kismi INTO ls_kismi INDEX 1."commented by ozans
*        READ TABLE gs_scr_1903-sum_alv INTO ls_sum WITH KEY lifnr = ls_kismi-lifnr."commented by ozans
        READ TABLE gs_scr_1903-sum_alv INTO ls_sum WITH KEY lifnr = gv_lifnr."added by ozans
        READ TABLE ls_sum-kismi_alv INTO ls_kismi INDEX p_row.
        ASSIGN COMPONENT p_column OF STRUCTURE ls_kismi TO <fs_data>.
        ls_detail-bukrs = ls_kismi-bukrs.
        ls_detail-gjahr = ls_kismi-gjahr.
      ENDIF.



      IF <fs_data> IS ASSIGNED.
        SET PARAMETER ID 'BLN' FIELD <fs_data>  .
        SET PARAMETER ID 'BUK' FIELD ls_detail-bukrs  .
        SET PARAMETER ID 'GJR' FIELD ls_detail-gjahr  .
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
      ENDIF.

    WHEN 'REBZG'.
      IF gs_scr_1903-name_of_alv EQ 'KISMI_ALV'.
*        READ TABLE gs_scr_1903-kismi INTO ls_kismi  INDEX 1."commetend by ozans
*        READ TABLE gs_scr_1903-sum_alv INTO ls_sum WITH KEY lifnr = ls_kismi-lifnr."commetend by ozans
        READ TABLE gs_scr_1903-sum_alv INTO ls_sum WITH KEY lifnr = gv_lifnr."added by ozans
        READ TABLE ls_sum-kismi_alv INTO ls_kismi INDEX p_row.
        ASSIGN COMPONENT p_column OF STRUCTURE ls_kismi TO <fs_data>.
        ls_detail-bukrs = ls_kismi-bukrs.
        ls_detail-gjahr = ls_kismi-rebzj.
      ENDIF.
      IF <fs_data> IS ASSIGNED.
        SET PARAMETER ID 'BLN' FIELD <fs_data>  .
        SET PARAMETER ID 'BUK' FIELD ls_detail-bukrs  .
        SET PARAMETER ID 'GJR' FIELD ls_detail-gjahr  .
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
      ENDIF.
  ENDCASE.


ENDFORM.                    "click_handle
*&---------------------------------------------------------------------*
*&      Form  SHOW_ROW_DETAIL
*&---------------------------------------------------------------------*
FORM show_row_detail USING p_row
                           p_column.

  DATA: ls_alv LIKE LINE OF gs_scr_1903-sum_alv.

  gs_scr_1903-name_of_alv = 'ROW_ALV'.
  READ TABLE gs_scr_1903-sum_alv INTO ls_alv INDEX p_row.
  IF sy-subrc EQ 0.
    PERFORM f_modify_detail_4_kismi TABLES ls_alv-kismi_alv ls_alv-detail_alv."added by Prodea(ufukk)-06.04.2020 13:57:28
    IF ls_alv-detail_alv IS NOT INITIAL.
*{   ->>> Inserted by Prodea Ozan Şahin - 08.04.2020 15:09:12
      " kısmi ve detay alv'nin hotspotunda satıcı tespit edilmesi için
      CLEAR gv_lifnr.
      gv_lifnr = ls_alv-lifnr.
*}     <<<- End of   Inserted - 08.04.2020 15:09:12
      PERFORM initialize_alv TABLES ls_alv-detail_alv USING 'STANDARD'.
      PERFORM display_alv.
    ENDIF.
  ENDIF.



ENDFORM.                    "show_row_detail
*&---------------------------------------------------------------------*
*&      Form  SHOW_ROW_DETAIL
*&---------------------------------------------------------------------*
FORM show_row_kismi USING p_row
                           p_column.

  DATA: ls_alv LIKE LINE OF gs_scr_1903-sum_alv.

  gs_scr_1903-name_of_alv = 'KISMI_ALV'.
  READ TABLE gs_scr_1903-sum_alv INTO ls_alv INDEX p_row.
  IF sy-subrc EQ 0.
    IF ls_alv-kismi_alv IS NOT INITIAL.
*{   ->>> Inserted by Prodea Ozan Şahin - 08.04.2020 15:09:12
      " kısmi ve detay alv'nin hotspotunda satıcı tespit edilmesi için
      CLEAR gv_lifnr.
      gv_lifnr = ls_alv-lifnr.
*}     <<<- End of   Inserted - 08.04.2020 15:09:12
      PERFORM initialize_alv TABLES ls_alv-kismi_alv USING 'STANDARD'.
      PERFORM display_alv.
    ENDIF.
  ENDIF.
  IF ls_alv-kismi_alv IS INITIAL.
    MESSAGE i019.
  ENDIF.

ENDFORM.                    "show_row_kismi

*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
FORM handle_user_command USING i_ucomm TYPE salv_de_function.
  CASE i_ucomm.
    WHEN '&CATG'.
      PERFORM send_category.
    WHEN '&ONAYCI'.
      PERFORM send_approver.
    WHEN '&LISTEH'.
      PERFORM send_to_list_maker.
    WHEN '&GERI'.
      PERFORM return_back.
    WHEN '&KAYIT'.
      PERFORM create_document.
    WHEN '&DETAY'.
      PERFORM show_all_detail.
    WHEN '&TERSKAYIT'.
*      PERFORM ters_kayit."commented by ozans
      PERFORM ters_kayit_2."added by ozans
*{   ->>> Inserted by Prodea Ozan Şahin - 10.04.2020 18:06:43
    WHEN '&ODEMELIST'.
      PERFORM odeme_listesi.
*}     <<<- End of   Inserted - 10.04.2020 18:06:43
*{   ->>> Inserted by Prodea Ozan Şahin - 13.06.2020 17:28:21
    WHEN '&HEPSDAGIT'.
      PERFORM hepsini_dagit.
*}     <<<- End of   Inserted - 13.06.2020 17:28:21
  ENDCASE.
  CLEAR: i_ucomm.
ENDFORM.                    "handle_user_command
*&---------------------------------------------------------------------*
*&      Form  send_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_category.
  DATA: lv_answer(1)  TYPE c,
        lt_onay_durum TYPE TABLE OF zsog_fi_018_t_01,
        ls_onay_durum TYPE zsog_fi_018_t_01,
        ls_item       TYPE zsog_fi_018_t_04,
        lt_item       TYPE TABLE OF zsog_fi_018_t_04.
  DATA: lt_tab TYPE esp1_message_tab_type.

  "Liste hazırlayıcısı kategoriye gönderme işlemi yaptığında,
  "0-Kullanıcıya sor göndermek istiyor muymuş

  PERFORM pop_up_confirm USING text-013 text-014  text-009 text-010 text-011
                               text-012 CHANGING lv_answer .
  IF lv_answer NE '1'.
    MESSAGE i004.
    RETURN.
  ENDIF.

  FIELD-SYMBOLS: <fs_sum_alv> LIKE LINE OF gs_scr_1903-sum_alv.
  DATA: ls_s_01 TYPE zsog_fi_018_t_01.

  "1-Header tablosunu güncelle  ZSOG_FI_018_T_01
  LOOP AT gs_scr_1903-sum_alv ASSIGNING <fs_sum_alv>.
    PERFORM domain_get_value USING '02' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING ls_onay_durum-icon_text.

    CLEAR: ls_s_01.
    ls_s_01-mandt       = sy-mandt.
    ls_s_01-laufd       = <fs_sum_alv>-laufd.
    ls_s_01-laufi       = <fs_sum_alv>-laufi.
    ls_s_01-bukrs       = <fs_sum_alv>-bukrs.
    ls_s_01-lifnr       = <fs_sum_alv>-lifnr.
    ls_s_01-onay_durum  = '02'.
    ls_s_01-icon_name   = 'ICON_TRANSPORTATION_MODE'.
    ls_s_01-icon_text   = ls_onay_durum-icon_text.
    ls_s_01-datum       = sy-datum.
    ls_s_01-uzeit       = sy-uzeit.
    ls_s_01-oneri_tutar = p_oneri.
*    ls_s_01-sektor      = s_sektr-low.

*    APPEND ls_s_01 TO lt_onay_durum.
    COLLECT ls_s_01 INTO lt_onay_durum.


*    COLLECT value zsog_fi_018_t_01(
*                               mandt       = sy-mandt
*                               laufd       = <fs_sum_alv>-laufd
*                               laufi       = <fs_sum_alv>-laufi
*                               bukrs       = <fs_sum_alv>-bukrs
*                               lifnr       = <fs_sum_alv>-lifnr
*                               onay_durum  = '02'
*                               icon_name   = 'ICON_TRANSPORTATION_MODE'
*                               icon_text   = ls_onay_durum-icon_text
*                               datum       = sy-datum
*                               uzeit       = sy-uzeit
*                               oneri_tutar = p_oneri
*                               sektor      = s_sektr-low
*                               ) INTO lt_onay_durum.
  ENDLOOP.

  FIELD-SYMBOLS: <fs_detail> LIKE LINE OF gs_scr_1903-detail.

  "2-Item tablosunu güncelle    ZSOG_FI_018_T_04
  LOOP AT gs_scr_1903-detail ASSIGNING <fs_detail>.

    UNASSIGN <fs_sum_alv>.

    MOVE-CORRESPONDING <fs_detail> TO ls_item.
*    ls_item = corresponding #( <fs_detail> ).
    READ TABLE gs_scr_1903-sum_alv ASSIGNING  <fs_sum_alv> WITH KEY lifnr = <fs_detail>-lifnr.
    ls_item-kunnr = <fs_sum_alv>-kunnr.
    ls_item-uname = gs_scr_1903-auth-uname.
    ls_item-statu = gs_scr_1903-auth-statu.
    ls_item-mandt = sy-mandt.
    PERFORM domain_get_value USING '02' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING ls_item-icon_text .
    ls_item-onay_durum = '02'.
    ls_item-icon_name  = 'ICON_TRANSPORTATION_MODE'.
    ls_item-datum      = sy-datum.
    ls_item-uzeit      = sy-uzeit.
    APPEND ls_item TO lt_item.
    CLEAR: ls_item.
  ENDLOOP.

  "3-Tabloları güncelle
  IF lt_onay_durum IS NOT INITIAL.
    MODIFY zsog_fi_018_t_01 FROM TABLE lt_onay_durum.
    COMMIT WORK AND WAIT .
  ENDIF.
  IF lt_item IS NOT INITIAL.
    MODIFY zsog_fi_018_t_04 FROM TABLE lt_item.
    COMMIT WORK AND WAIT .
  ENDIF.


  "4-Onay durumunu güncelle
  READ TABLE lt_onay_durum INTO gs_scr_1903-onay_durum INDEX 1.
*  gs_scr_1903-onay_durum = lt_onay_durum[ 1 ].

  "5-Kategori butonunu ekrandan kaldır.
  PERFORM hide_function_single USING '&CATG'.
  PERFORM show_function_single USING '&GERI'.

  "6-Mail gönder.
  PERFORM send_mail.

  DATA: lv_text(100) TYPE c.
  lv_text = |{ gs_scr_1903-onay_durum-laufi }| && | { text-023 }|.
  PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'S' '000'
                                                   lv_text '' '' ''.

  IF lt_tab IS NOT INITIAL.
    PERFORM c14z_messages_show_as_popup TABLES lt_tab.
  ENDIF.

ENDFORM.                    "send_category
*&---------------------------------------------------------------------*
*&      Form  send_approver
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_approver.
  DATA: lv_answer(1)  TYPE c,
        lt_onay_durum TYPE TABLE OF zsog_fi_018_t_01,
        ls_onay_durum TYPE zsog_fi_018_t_01,
        ls_item       TYPE zsog_fi_018_t_04,
        lt_item       TYPE TABLE OF zsog_fi_018_t_04.
  DATA: lt_tab TYPE esp1_message_tab_type.
  DATA: lv_oneri TYPE bsik-wrbtr.
  "Kategori onaycıya gönderme işlemi yaptığında,
  "0-Kullanıcıya sor göndermek istiyor muymuş

  PERFORM pop_up_confirm USING text-013 text-026  text-009 text-010 text-011
                               text-012 CHANGING lv_answer .
  IF lv_answer NE '1'.
    MESSAGE i004.
    RETURN.
  ENDIF.

  FIELD-SYMBOLS: <fs_detail> LIKE LINE OF gs_scr_1903-detail.

  LOOP AT gs_scr_1903-detail ASSIGNING <fs_detail> WHERE secim = 'X' OR cat_onay IS NOT INITIAL.
    lv_oneri = <fs_detail>-cat_devlet_odeme + <fs_detail>-cat_devlet_odeme + lv_oneri.
*    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE i009.
    RETURN.
  ENDIF.
  IF lv_oneri > p_oneri.
    MESSAGE i018.
    RETURN.
  ENDIF.

  FIELD-SYMBOLS: <fs_sum_alv> LIKE LINE OF gs_scr_1903-sum_alv.
  DATA: ls_s_01 TYPE zsog_fi_018_t_01.

  "1-Header tablosunu güncelle  ZSOG_FI_018_T_01
  LOOP AT gs_scr_1903-sum_alv ASSIGNING <fs_sum_alv>.
    PERFORM domain_get_value USING '03' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING ls_onay_durum-icon_text.

    CLEAR: ls_s_01.
    ls_s_01-mandt       = sy-mandt.
    ls_s_01-laufd       = <fs_sum_alv>-laufd.
    ls_s_01-laufi       = <fs_sum_alv>-laufi.
    ls_s_01-bukrs       = <fs_sum_alv>-bukrs.
    ls_s_01-lifnr       = <fs_sum_alv>-lifnr.
    ls_s_01-onay_durum  = '03'.
    ls_s_01-icon_name   = 'ICON_BIW_INFO_AREA'.
    ls_s_01-icon_text   = ls_onay_durum-icon_text.
    ls_s_01-datum       = sy-datum.
    ls_s_01-uzeit       = sy-uzeit.
    ls_s_01-oneri_tutar = p_oneri.
*    ls_s_01-sektor     = s_sektr-low.
*    APPEND ls_s_01 TO lt_onay_durum.
    COLLECT ls_s_01 INTO lt_onay_durum.


*    COLLECT value zsog_fi_018_t_01(
*                               mandt       = sy-mandt
*                               laufd       = <fs_sum_alv>-laufd
*                               laufi       = <fs_sum_alv>-laufi
*                               bukrs       = <fs_sum_alv>-bukrs
*                               lifnr       = <fs_sum_alv>-lifnr
*                               onay_durum  = '03'
*                               icon_name   = 'ICON_BIW_INFO_AREA'
*                               icon_text   = ls_onay_durum-icon_text
*                               datum       = sy-datum
*                               uzeit       = sy-uzeit
*                               oneri_tutar = p_oneri
*                               sektor     = s_sektr-low
*                               ) INTO lt_onay_durum.
  ENDLOOP.

  "2-Item tablosunu güncelle    ZSOG_FI_018_T_04
  LOOP AT gs_scr_1903-detail ASSIGNING <fs_detail>.

    UNASSIGN <fs_sum_alv>.

    MOVE-CORRESPONDING <fs_detail> TO ls_item.
*    ls_item = corresponding #( <fs_detail> ).
    READ TABLE gs_scr_1903-sum_alv ASSIGNING  <fs_sum_alv> WITH KEY lifnr = <fs_detail>-lifnr.
    ls_item-kunnr = <fs_sum_alv>-kunnr.
    ls_item-uname = gs_scr_1903-auth-uname.
    ls_item-statu = gs_scr_1903-auth-statu.
    ls_item-mandt = sy-mandt.
    PERFORM domain_get_value USING '03' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING ls_item-icon_text .
    ls_item-onay_durum = '03'.
    ls_item-icon_name  = 'ICON_BIW_INFO_AREA'.
    ls_item-datum      = sy-datum.
    ls_item-uzeit      = sy-uzeit.
    APPEND ls_item TO lt_item.
    CLEAR: ls_item.
  ENDLOOP.

  "3-Tabloları güncelle
  IF lt_onay_durum IS NOT INITIAL.
    MODIFY zsog_fi_018_t_01 FROM TABLE lt_onay_durum.
    COMMIT WORK AND WAIT .
  ENDIF.
  IF lt_item IS NOT INITIAL.
    MODIFY zsog_fi_018_t_04 FROM TABLE lt_item.
    COMMIT WORK AND WAIT .
  ENDIF.


  "4-Onay durumunu güncelle
  READ TABLE lt_onay_durum INTO gs_scr_1903-onay_durum INDEX 1.
*  gs_scr_1903-onay_durum = lt_onay_durum[ 1 ].

  "5-Kategori butonunu ekrandan kaldır.
  PERFORM hide_function_single USING '&ONAYCI'.
  PERFORM show_function_single USING '&GERI'.

  "6-Mail gönder.
  PERFORM send_mail.

  DATA: lv_text(100) TYPE c.
  lv_text = |{ gs_scr_1903-onay_durum-laufi }| && | { text-027 }|.
  PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'S' '000'
                                                lv_text '' '' ''.

  IF lt_tab IS NOT INITIAL.
    PERFORM c14z_messages_show_as_popup TABLES lt_tab.

  ENDIF.

ENDFORM.                    "send_approver
*&---------------------------------------------------------------------*
*&      Form  send_to_list_maker
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_to_list_maker.
  DATA: lv_answer(1)  TYPE c,
        lt_onay_durum TYPE TABLE OF zsog_fi_018_t_01,
        ls_onay_durum TYPE zsog_fi_018_t_01,
        ls_item       TYPE zsog_fi_018_t_04,
        lt_item       TYPE TABLE OF zsog_fi_018_t_04.
  DATA: lt_tab TYPE esp1_message_tab_type.
  DATA: lv_oneri TYPE bsik-wrbtr.
  "Onaycı Liste hazırlayıcısına gönderme işlemi yaptığında,
  "0-Kullanıcıya sor göndermek istiyor muymuş

  PERFORM pop_up_confirm USING text-013 text-028  text-009 text-010 text-011
                               text-012 CHANGING lv_answer .
  IF lv_answer NE '1'.
    MESSAGE i004.
    RETURN.
  ENDIF.

  FIELD-SYMBOLS: <fs_detail>  LIKE LINE OF gs_scr_1903-detail,
                 <fs_sum_alv> LIKE LINE OF gs_scr_1903-sum_alv.

  LOOP AT gs_scr_1903-detail ASSIGNING <fs_detail>.
    IF <fs_detail>-ony_satici_odeme IS INITIAL.
      lv_oneri = <fs_detail>-cat_devlet_odeme + <fs_detail>-cat_devlet_odeme + lv_oneri.
    ELSE.
      lv_oneri = <fs_detail>-ony_devlet_odeme + <fs_detail>-ony_devlet_odeme + lv_oneri.
    ENDIF.
  ENDLOOP.
  IF lv_oneri > p_oneri.
    MESSAGE i018.
    RETURN.
  ENDIF.

  DATA: ls_s_01 LIKE LINE OF lt_onay_durum.

  "1-Header tablosunu güncelle  ZSOG_FI_018_T_01
  LOOP AT gs_scr_1903-sum_alv ASSIGNING <fs_sum_alv>.
    PERFORM domain_get_value USING '04' 'ICON_MONEY' CHANGING ls_onay_durum-icon_text.
    CLEAR: ls_s_01.
    ls_s_01-mandt       = sy-mandt.
    ls_s_01-laufd       = <fs_sum_alv>-laufd.
    ls_s_01-laufi       = <fs_sum_alv>-laufi.
    ls_s_01-bukrs       = <fs_sum_alv>-bukrs.
    ls_s_01-lifnr       = <fs_sum_alv>-lifnr.
    ls_s_01-onay_durum  = '04'.
    ls_s_01-icon_name   = 'ICON_MONEY'.
    ls_s_01-icon_text   = ls_onay_durum-icon_text.
    ls_s_01-datum       = sy-datum.
    ls_s_01-uzeit       = sy-uzeit.
    ls_s_01-oneri_tutar = p_oneri.
*    ls_s_01-sektor      = s_sektr-low.

*    APPEND ls_s_01 TO lt_onay_durum.
    COLLECT ls_s_01 INTO lt_onay_durum.
*
*    COLLECT value zsog_fi_018_t_01(
*                               mandt       = sy-mandt
*                               laufd       = <fs_sum_alv>-laufd
*                               laufi       = <fs_sum_alv>-laufi
*                               bukrs       = <fs_sum_alv>-bukrs
*                               lifnr       = <fs_sum_alv>-lifnr
*                               onay_durum  = '04'
*                               icon_name   = 'ICON_MONEY'
*                               icon_text   = ls_onay_durum-icon_text
*                               datum       = sy-datum
*                               uzeit       = sy-uzeit
*                               oneri_tutar = p_oneri
*                               sektor      = s_sektr-low
*                               ) INTO lt_onay_durum.
  ENDLOOP.

  "2-Item tablosunu güncelle    ZSOG_FI_018_T_04
  LOOP AT gs_scr_1903-detail ASSIGNING <fs_detail>.
*    IF <fs_sum_alv> IS ASSIGNED .
    UNASSIGN <fs_sum_alv>.
*    ENDIF.

    MOVE-CORRESPONDING <fs_detail> TO ls_item.
*    ls_item = corresponding #( <fs_detail> ).
    READ TABLE gs_scr_1903-sum_alv ASSIGNING  <fs_sum_alv> WITH KEY lifnr = <fs_detail>-lifnr.
    ls_item-kunnr = <fs_sum_alv>-kunnr.
    ls_item-uname = gs_scr_1903-auth-uname.
    ls_item-statu = gs_scr_1903-auth-statu.
    ls_item-mandt = sy-mandt.
    PERFORM domain_get_value USING '04' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING ls_item-icon_text .
    ls_item-onay_durum = '04'.
    ls_item-icon_name  = 'ICON_MONEY'.
    ls_item-datum      = sy-datum.
    ls_item-uzeit      = sy-uzeit.
    IF <fs_detail>-ony_satici_odeme IS INITIAL.
      ls_item-ony_onay         = <fs_detail>-cat_onay.
      ls_item-ony_devlet_odeme = <fs_detail>-cat_devlet_odeme.
      ls_item-ony_satici_odeme = <fs_detail>-cat_satici_odeme.
      IF <fs_detail>-cat_onay IS NOT INITIAL.
        ls_item-secim = 'X'.
      ENDIF.
    ENDIF.
    APPEND ls_item TO lt_item.
    CLEAR: ls_item.
  ENDLOOP.

  "3-Tabloları güncelle
  IF lt_onay_durum IS NOT INITIAL.
    MODIFY zsog_fi_018_t_01 FROM TABLE lt_onay_durum.
    COMMIT WORK AND WAIT .
  ENDIF.
  IF lt_item IS NOT INITIAL.
    MODIFY zsog_fi_018_t_04 FROM TABLE lt_item.
    COMMIT WORK AND WAIT .
  ENDIF.

  "4-Onay durumunu güncelle
  READ TABLE lt_onay_durum INTO gs_scr_1903-onay_durum INDEX 1.
*  gs_scr_1903-onay_durum = lt_onay_durum[ 1 ].

  "5-Kategori butonunu ekrandan kaldır.
  PERFORM hide_function_single USING '&LISTEH'.
  PERFORM show_function_single USING '&GERI'.

  "6-Mail gönder.
  PERFORM send_mail.
  DATA:lv_text(100) TYPE c.
  lv_text = |{ gs_scr_1903-onay_durum-laufi }| && | { text-029 }|.
  PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'S' '000'
                                                lv_text '' '' ''.

  IF lt_tab IS NOT INITIAL.
    PERFORM c14z_messages_show_as_popup TABLES lt_tab.

  ENDIF.
ENDFORM.                    "send_to_list_maker
*&---------------------------------------------------------------------*
*&      Form  hide_function_single
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_NAME    text
*----------------------------------------------------------------------*
FORM hide_function_single USING pv_name.
  "eğer gui statuse yeni bir buton eklemek gerekirse,
  "ve yetkiye göre butonların kapatılması gerekiyorsa,
  "bu programdaki gui statüsün yeni bir gui statüse,
  "kopyalanması gerekiyor. Aksi halde halde yeni eklenen buton,
  "function list içerisine gelmiyor. Burada bir bug var.

  DATA: functions    TYPE REF TO cl_salv_functions_list,
        lt_func_list TYPE salv_t_ui_func,
        la_func_list LIKE LINE OF lt_func_list.

  functions = gs_scr_1903-r_alv->get_functions( ).
  lt_func_list = functions->get_functions( ).
  LOOP AT lt_func_list INTO la_func_list.
    IF la_func_list-r_function->get_name( ) = pv_name.
      la_func_list-r_function->set_visible( ' ' ).
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "hide_function_single
*&---------------------------------------------------------------------*
*&      Form  show_function_single
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_NAME    text
*----------------------------------------------------------------------*
FORM show_function_single USING pv_name.
  "eğer gui statuse yeni bir buton eklemek gerekirse,
  "ve yetkiye göre butonların kapatılması gerekiyorsa,
  "bu programdaki gui statüsün yeni bir gui statüse,
  "kopyalanması gerekiyor. Aksi halde halde yeni eklenen buton,
  "function list içerisine gelmiyor. Burada bir bug var.

  DATA: functions TYPE REF TO cl_salv_functions_list,
        lt_func_list TYPE salv_t_ui_func,
        la_func_list LIKE LINE OF lt_func_list.

  functions = gs_scr_1903-r_alv->get_functions( ).
  lt_func_list = functions->get_functions( ).
  LOOP AT lt_func_list INTO la_func_list.
    IF la_func_list-r_function->get_name( ) = pv_name.
      la_func_list-r_function->set_visible( 'X' ).
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "show_function_single
*&---------------------------------------------------------------------*
*&      Form  return_back
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM return_back.

  DATA: lt_tab        TYPE esp1_message_tab_type,
        lt_item       TYPE TABLE OF zsog_fi_018_t_04,
        ls_item       TYPE zsog_fi_018_t_04,
        ls_onay_durum TYPE zsog_fi_018_t_01.

  FIELD-SYMBOLS: <fs_header_tab> LIKE LINE OF gs_scr_1903-header_tab,
                 <fs_item_tab>   LIKE LINE OF gs_scr_1903-item_tab.

  DATA: ls_header_tab LIKE LINE OF gs_scr_1903-header_tab.

  CASE gs_scr_1903-auth-statu.
    WHEN '01'.
      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '01' AND
                                                                     onay_durum NE '02'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                        text-019 text-020 '' ''.
      ENDIF.
      LOOP AT gs_scr_1903-item_tab ASSIGNING <fs_item_tab> WHERE onay_durum NE '01' AND
                                                                 onay_durum NE '02'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                        text-019 text-020 '' ''.
      ENDIF.
      IF lt_tab IS NOT INITIAL.
        PERFORM c14z_messages_show_as_popup TABLES lt_tab.
        RETURN.
      ENDIF.
      DELETE zsog_fi_018_t_01 FROM TABLE gs_scr_1903-header_tab.
      COMMIT WORK AND WAIT.
      IF sy-subrc EQ 0.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'S' '000'
                                                        text-021 '' '' ''.
      ENDIF.

      CLEAR : ls_header_tab.
      READ TABLE gs_scr_1903-header_tab INTO ls_header_tab INDEX 1.
      IF ls_header_tab IS NOT INITIAL.
        DELETE FROM zsog_fi_018_t_04 WHERE laufd = ls_header_tab-laufd
                                       AND laufi = ls_header_tab-laufi
                                       AND bukrs = ls_header_tab-bukrs.
*                                   AND lifnr = ls_header_tab-lifnr.
        COMMIT WORK AND WAIT.
      ENDIF.
      ls_onay_durum-mandt       = sy-mandt.
      ls_onay_durum-laufd       = ls_header_tab-laufd.
      ls_onay_durum-laufi       = ls_header_tab-laufi.
      ls_onay_durum-bukrs       = ls_header_tab-bukrs.
      ls_onay_durum-lifnr       = ls_header_tab-lifnr.
      ls_onay_durum-onay_durum  = '01'.
      ls_onay_durum-icon_name   = 'ICON_ICON_LIST'.
      ls_onay_durum-datum       = sy-datum.
      ls_onay_durum-uzeit       = sy-uzeit.
      ls_onay_durum-oneri_tutar = p_oneri.
*      ls_onay_durum-sektor      = s_sektr-low.


*      ls_onay_durum = value zsog_fi_018_t_01(
*                                              mandt       = sy-mandt
*                                              laufd       = ls_header_tab-laufd
*                                              laufi       = ls_header_tab-laufi
*                                              bukrs       = ls_header_tab-bukrs
*                                              lifnr       = ls_header_tab-lifnr
*                                              onay_durum  = '01'
*                                              icon_name   = 'ICON_ICON_LIST'
*                                              datum       = sy-datum
*                                              uzeit       = sy-uzeit
*                                              oneri_tutar = p_oneri
*                                              sektor      = s_sektr-low
*                                            ) .

      PERFORM domain_get_value USING '01' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING ls_onay_durum-icon_text.
      gs_scr_1903-onay_durum = ls_onay_durum.

      IF sy-subrc EQ 0.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'S' '000'
                                                        text-022 '' '' ''.
      ENDIF.
      IF lt_tab IS NOT INITIAL.
        PERFORM c14z_messages_show_as_popup TABLES lt_tab.
        PERFORM hide_function_single USING '&GERI'.
        PERFORM show_function_single USING '&CATG'.
        RETURN.
      ENDIF.

    WHEN '02'.

      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '02' AND
                                                                     onay_durum NE '03'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                        text-030 text-020 '' ''.
      ENDIF.
      LOOP AT gs_scr_1903-item_tab ASSIGNING <fs_item_tab> WHERE onay_durum NE '02' AND
                                                                 onay_durum NE '03'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                        text-030 text-020 '' ''.
      ENDIF.
      IF lt_tab IS NOT INITIAL.
        PERFORM c14z_messages_show_as_popup TABLES lt_tab.
        RETURN.
      ENDIF.

      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab>.
        <fs_header_tab>-onay_durum  = '02'.
        <fs_header_tab>-icon_name   = 'ICON_TRANSPORTATION_MODE'.
        PERFORM domain_get_value USING '02' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING <fs_header_tab>-icon_text.
        <fs_header_tab>-datum = sy-datum.
        <fs_header_tab>-uzeit = sy-uzeit.
        <fs_header_tab>-oneri_tutar = p_oneri.
      ENDLOOP.

      FIELD-SYMBOLS: <fs_detail>  LIKE LINE OF gs_scr_1903-detail,
                     <fs_sum_alv> LIKE LINE OF gs_scr_1903-sum_alv.

      LOOP AT gs_scr_1903-detail ASSIGNING <fs_detail>.
*        ls_item = corresponding #( <fs_detail> ).
        MOVE-CORRESPONDING <fs_detail> TO ls_item.
        UNASSIGN: <fs_sum_alv>.

        READ TABLE gs_scr_1903-sum_alv ASSIGNING <fs_sum_alv> WITH KEY lifnr = <fs_detail>-lifnr.
        ls_item-kunnr = <fs_sum_alv>-kunnr.
        ls_item-uname = gs_scr_1903-auth-uname.
        ls_item-statu = gs_scr_1903-auth-statu.
        ls_item-mandt = sy-mandt.
        PERFORM domain_get_value USING '02' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING ls_item-icon_text .
        ls_item-onay_durum = '02'.
        ls_item-icon_name  = 'ICON_TRANSPORTATION_MODE'.
        ls_item-datum      = sy-datum.
        ls_item-uzeit      = sy-uzeit.
        <fs_detail>-cat_onay = ''.
        <fs_detail>-cat_devlet_odeme = 0.
        <fs_detail>-cat_satici_odeme = 0.
        ls_item-cat_onay   = ''.
        ls_item-cat_devlet_odeme     = 0.
        ls_item-cat_satici_odeme     = 0.
        ls_item-odenecek_tutar       = 0.
        ls_item-belgeden_kalan       = 0.
        ls_item-cat_satici_odeme     = 0.
        APPEND ls_item TO lt_item.
        CLEAR: ls_item.

      ENDLOOP.

      MODIFY zsog_fi_018_t_01 FROM TABLE gs_scr_1903-header_tab.
      COMMIT WORK AND WAIT.
      IF sy-subrc EQ 0.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'S' '000'
                                                        text-021 '' '' ''.
      ENDIF.
      MODIFY zsog_fi_018_t_04 FROM TABLE lt_item.
      COMMIT WORK AND WAIT.
      IF sy-subrc EQ 0.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'S' '000'
                                                        text-022 '' '' ''.
      ENDIF.
      IF lt_tab IS NOT INITIAL.
        PERFORM c14z_messages_show_as_popup TABLES lt_tab.
        PERFORM hide_function_single USING '&GERI'.
        PERFORM show_function_single USING '&ONAYCI'.
        RETURN.
      ENDIF.
    WHEN '03'.
      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab> WHERE onay_durum NE '03' AND
                                                                     onay_durum NE '04'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                        text-037 text-020 '' ''.
      ENDIF.
      LOOP AT gs_scr_1903-item_tab ASSIGNING <fs_item_tab> WHERE onay_durum NE '03' AND
                                                                 onay_durum NE '04'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'E' '000'
                                                        text-037 text-020 '' ''.
      ENDIF.
      IF lt_tab IS NOT INITIAL.
        PERFORM c14z_messages_show_as_popup TABLES lt_tab.
        RETURN.
      ENDIF.

      LOOP AT gs_scr_1903-header_tab ASSIGNING <fs_header_tab>.
        <fs_header_tab>-onay_durum  = '03'.
        <fs_header_tab>-icon_name   = 'ICON_BIW_INFO_AREA'.
        PERFORM domain_get_value USING '03' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING <fs_header_tab>-icon_text.
        <fs_header_tab>-datum = sy-datum.
        <fs_header_tab>-uzeit = sy-uzeit.
        <fs_header_tab>-oneri_tutar = p_oneri.
      ENDLOOP.

      LOOP AT gs_scr_1903-detail ASSIGNING <fs_detail>.
*        ls_item = corresponding #( <fs_detail> ).
        MOVE-CORRESPONDING <fs_detail> TO ls_item.
        UNASSIGN <fs_sum_alv>.
        READ TABLE gs_scr_1903-sum_alv ASSIGNING  <fs_sum_alv> WITH KEY lifnr = <fs_detail>-lifnr.
        ls_item-kunnr = <fs_sum_alv>-kunnr.
        ls_item-uname = gs_scr_1903-auth-uname.
        ls_item-statu = gs_scr_1903-auth-statu.
        ls_item-mandt = sy-mandt.
        PERFORM domain_get_value USING '03' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING ls_item-icon_text .
        ls_item-onay_durum = '03'.
        ls_item-icon_name  = 'ICON_BIW_INFO_AREA'.
        ls_item-datum      = sy-datum.
        ls_item-uzeit      = sy-uzeit.
        <fs_detail>-ony_onay = ''.
        <fs_detail>-ony_devlet_odeme = 0.
        <fs_detail>-ony_satici_odeme = 0.
        ls_item-ony_onay   = ''.
        ls_item-ony_devlet_odeme     = 0.
        ls_item-ony_satici_odeme     = 0.
        APPEND ls_item TO lt_item.
        CLEAR: ls_item.

      ENDLOOP.
      MODIFY zsog_fi_018_t_01 FROM TABLE gs_scr_1903-header_tab.
      COMMIT WORK AND WAIT.
      IF sy-subrc EQ 0.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'S' '000'
                                                        text-021 '' '' ''.
      ENDIF.
      MODIFY zsog_fi_018_t_04 FROM TABLE lt_item.
      COMMIT WORK AND WAIT.
      IF sy-subrc EQ 0.
        PERFORM fill_single_message TABLES lt_tab USING 'ZSOG_FI_018' 'S' '000'
                                                        text-022 '' '' ''.
      ENDIF.
      IF lt_tab IS NOT INITIAL.
        PERFORM c14z_messages_show_as_popup TABLES lt_tab.
        PERFORM hide_function_single USING '&GERI'.
        PERFORM show_function_single USING '&LISTEH'.
        RETURN.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "return_back
*&---------------------------------------------------------------------*
*&      Form  create_document
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_document.

  DATA: lt_vergisiz_tam   TYPE TABLE OF zsog_fi_018_s_02, "bunu flat structure yap kismiler gelince
        lt_saticiya_odeme TYPE TABLE OF zsog_fi_018_s_02,
        lt_devlete_odeme  TYPE TABLE OF zsog_fi_018_s_02,
        lt_vergisiz_kismi TYPE TABLE OF zsog_fi_018_s_02,
        lt_saticiya_kismi TYPE TABLE OF zsog_fi_018_s_02,
        lt_devlete_kismi  TYPE TABLE OF zsog_fi_018_s_02,
        lt_null           TYPE TABLE OF zsog_fi_018_s_02."added by Prodea(ufukk)-07.04.2020 10:09:54

  DATA: lt_return    TYPE TABLE OF bapiret2,
        lv_answer(1).

  DATA: lt_onay_durum TYPE TABLE OF zsog_fi_018_t_01,
        ls_onay_durum TYPE zsog_fi_018_t_01,
        lt_item       TYPE TABLE OF zsog_fi_018_t_04.

  DATA: lt_kismi      TYPE TABLE OF zsog_fi_018_t_06.


  CHECK gs_scr_1903-auth-statu EQ '01'.

  PERFORM pop_up_confirm USING text-013 text-036  text-009 text-010 text-011
                               text-012 CHANGING lv_answer .
  IF lv_answer NE '1'.
    MESSAGE i004.
    RETURN.
  ENDIF.

  DATA: ls_detail LIKE LINE OF gs_scr_1903-detail,
        ls_bapiret2 LIKE LINE OF lt_return.

  LOOP AT gs_scr_1903-detail INTO ls_detail WHERE iban IS INITIAL.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    CLEAR: ls_bapiret2.
    ls_bapiret2-type       = 'E'.
    ls_bapiret2-id         = 'ZSOG_FI_018'.
    ls_bapiret2-number     = 014.
    ls_bapiret2-message    = ls_detail-lifnr && | satıcısının iban bilgisi boştur|.
    ls_bapiret2-message_v1 = ls_detail-lifnr && | satıcısının iban bilgisi boştur|.
    APPEND ls_bapiret2 TO lt_return.


*    APPEND value bapiret2( type   = 'E'
*                           ID     = 'ZSOG_FI_018'
*                           NUMBER = 014
*                           message = ls_detail-lifnr && | satıcısının iban bilgisi boştur|
*                           message_v1 = ls_detail-lifnr && | satıcısının iban bilgisi boştur|
**                           message_v2 = ls_filter-lifnr && | satıcısının iban bilgisi boştur|
*                           ) TO lt_return.
  ENDIF.
  IF lt_return IS NOT INITIAL.
    PERFORM msg_display_error_table TABLES lt_return.
    RETURN.
  ENDIF.

  DATA: ls_sum_alv LIKE LINE OF gs_scr_1903-sum_alv.
  FIELD-SYMBOLS: <fs_detail> LIKE LINE OF gs_scr_1903-detail,
                 <fs_vergisiz_tam> LIKE LINE OF lt_vergisiz_tam,
                 <fs_saticiya_odeme> LIKE LINE OF lt_saticiya_odeme,
                 <fs_devlete_odeme>  LIKE LINE OF lt_devlete_odeme,
                 <fs_vergisiz_kismi> LIKE LINE OF lt_vergisiz_kismi,
                 <fs_saticiya_kismi> LIKE LINE OF lt_saticiya_kismi,
                 <fs_devlete_kismi>  LIKE LINE OF lt_devlete_kismi,
                 <lt_null>           LIKE LINE OF lt_null."added by Prodea(ufukk)-07.04.2020 10:11:08

  LOOP AT gs_scr_1903-sum_alv INTO ls_sum_alv WHERE onay_durum-onay_durum NE '04'.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    MESSAGE i013.
    RETURN.
  ENDIF.

*{ ->>> BEGIN of INSERT by Prodea (Ufuk Koyuncu) - 07.04.2020 13:56:28
  DATA lt_detail TYPE TABLE OF zsog_fi_018_s_02.
*  BREAK xcemreb.
  lt_detail[] = gs_scr_1903-detail[].

  PERFORM f_modify_detail_4_kismi TABLES gs_scr_1903-kismi lt_detail."added by Prodea(ufukk)-06.04.2020 13:57:28

*} <<<- END of INSERT by Prodea (Ufuk Koyuncu) - 07.04.2020 13:56:28

  LOOP AT lt_detail ASSIGNING <fs_detail> WHERE ony_satici_odeme IS NOT INITIAL.
    "1-Eğer onaycı satıcı ödeme ve onaycı devlet ödeme miktarı belge tutarına eşitse tam ödeme vardır.
    "1.1-Vergi göstergesi V0 ise satıcıya tam ödeme yapılarak belge kapatılmadır.
    "1.2- Vergi göstergesi V0 değilse Satıcıya ve devlete iki kısmı ödeme belgesi oluşturularak belge kapatılır.

    "2-Eğer onaycı satıcı ödeme ve onaycı devlet ödeme miktarı belge tutarına eşit değilse kısmi ödeme vardır.
    "2.1-Vergi göstergesi V0 ise satıcıya kısmı ödeme yapılır, belge bloke edilir.
    "2.2-Vergi göstergesi V0 değilse Satıcıya ve devlete iki kısmı ödeme belgesi oluşturularak belge bloke edilir.
    "2.3-Kalan tutar sıfırlanıyorsa bu belgenin de blokajı kalkmalı belge tamamlanmıştır. Daha önce kısmi belgeleri olabilir.

    "3-Belge oluşturma işlemleri sonucunda tanıtıcı numarası ve tanıtıcı tarihi 05 olarak güncellenir.

    "4-Z'li tablolar güncellenir.

    "5-Kısmi ödeme belgeleri toplanır.

    IF ( <fs_detail>-ony_satici_odeme +  <fs_detail>-ony_devlet_odeme ) = <fs_detail>-belge_tutar. "1
*{   ->>> Commented by Prodea Ozan Şahin - 20.03.2020 14:29:32
*      IF <fs_detail>-mwskz EQ 'V0'. "1.1
**        APPEND INITIAL LINE TO lt_vergisiz_tam ASSIGNING field-symbol(<fs_vergisiz_tam>).
**        MOVE-CORRESPONDING <fs_detail> TO <fs_vergisiz_tam>.
*        UNASSIGN <fs_vergisiz_tam>.
*        APPEND INITIAL LINE TO lt_vergisiz_tam ASSIGNING <fs_vergisiz_tam>.
*        IF <fs_vergisiz_tam> IS ASSIGNED .
*          MOVE-CORRESPONDING <fs_detail> TO <fs_vergisiz_tam>.
*        ENDIF.
*
*      ELSE. "1.2
*        DO 2 TIMES.
*          IF sy-index EQ 1.
*            APPEND INITIAL LINE TO lt_saticiya_odeme ASSIGNING field-symbol(<fs_saticiya_odeme>).
*            MOVE-CORRESPONDING <fs_detail> TO <fs_saticiya_odeme>.
      UNASSIGN: <fs_saticiya_odeme>.
      APPEND INITIAL LINE TO lt_saticiya_odeme ASSIGNING <fs_saticiya_odeme>.
      IF <fs_saticiya_odeme> IS ASSIGNED .
        MOVE-CORRESPONDING <fs_detail> TO <fs_saticiya_odeme>.
      ENDIF.
*          ELSE.
**            APPEND INITIAL LINE TO lt_devlete_odeme ASSIGNING field-symbol(<fs_devlete_odeme>).
**            MOVE-CORRESPONDING <fs_detail> TO <fs_devlete_odeme>.
*            UNASSIGN: <fs_devlete_odeme>.
*            APPEND INITIAL LINE TO lt_devlete_odeme ASSIGNING <fs_devlete_odeme>.
*            IF <fs_devlete_odeme> IS ASSIGNED .
*              MOVE-CORRESPONDING <fs_detail> TO <fs_devlete_odeme>.
*            ENDIF.
*          ENDIF.
*        ENDDO.
*      ENDIF.
*}     <<<- End of  Commented - 20.03.2020 14:29:32
    ELSE. "2
*{   ->>> Commented by Prodea Ozan Şahin - 20.03.2020 14:31:37
*      IF <fs_detail>-mwskz EQ 'V0'. "2.1
**        APPEND INITIAL LINE TO lt_vergisiz_kismi ASSIGNING field-symbol(<fs_vergisiz_kismi>).
*        UNASSIGN: <fs_vergisiz_kismi>.
*        APPEND INITIAL LINE TO lt_vergisiz_kismi ASSIGNING <fs_vergisiz_kismi>.
*        IF <fs_vergisiz_kismi> IS ASSIGNED.
*          MOVE-CORRESPONDING <fs_detail> TO <fs_vergisiz_kismi>.
*        ENDIF.
*      ELSE. "2.2
*        DO 2 TIMES.
*          IF sy-index EQ 1.
*            APPEND INITIAL LINE TO lt_saticiya_kismi ASSIGNING field-symbol(<fs_saticiya_kismi>).
      UNASSIGN: <fs_saticiya_kismi>.
      APPEND INITIAL LINE TO lt_saticiya_kismi ASSIGNING <fs_saticiya_kismi>.
      IF <fs_saticiya_kismi> IS ASSIGNED.
        MOVE-CORRESPONDING <fs_detail> TO <fs_saticiya_kismi>.
      ENDIF.
*          ELSE. "2.3
**            APPEND INITIAL LINE TO lt_devlete_kismi ASSIGNING field-symbol(<fs_devlete_kismi>).
*            UNASSIGN: <fs_devlete_kismi>.
*            APPEND INITIAL LINE TO lt_devlete_kismi ASSIGNING <fs_devlete_kismi>.
*            IF <fs_devlete_kismi> IS ASSIGNED.
*              MOVE-CORRESPONDING <fs_detail> TO <fs_devlete_kismi>.
*            ENDIF.
*          ENDIF.
*        ENDDO.
*      ENDIF.
*}     <<<- End of  Commented - 20.03.2020 14:31:37
    ENDIF.
  ENDLOOP.

*{ ->>> BEGIN of INSERT by Prodea (Ufuk Koyuncu) - 07.04.2020 10:14:03
  LOOP AT lt_detail ASSIGNING <fs_detail> WHERE ony_satici_odeme IS INITIAL.
    UNASSIGN: <lt_null>.
    APPEND INITIAL LINE TO lt_null ASSIGNING <lt_null>.
    IF <lt_null> IS ASSIGNED.
      MOVE-CORRESPONDING <fs_detail> TO <lt_null>.
    ENDIF.
  ENDLOOP.

  IF lt_null[] IS NOT INITIAL.
    PERFORM f_fill_item_4_null TABLES lt_null lt_item.
  ENDIF.
*} <<<- END of INSERT by Prodea (Ufuk Koyuncu) - 07.04.2020 10:14:03

  PERFORM unlock_object.

*{   ->>> Commented by Prodea Ozan Şahin - 20.03.2020 15:01:23
*  IF lt_vergisiz_tam IS NOT INITIAL.
*    "burada gerçekten tam ödeme gerçekleşiyor. Devlete ödeme yok hepsi satıcıya.
*    PERFORM vergisiz_tam_odeme TABLES lt_vergisiz_tam lt_return lt_item.
*  ENDIF.
*}     <<<- End of  Commented - 20.03.2020 15:01:23

  IF lt_saticiya_odeme IS NOT INITIAL.
    " ödemenin bir kısmı satıcı hesabına diğer kısmı devlet hesabına gidecek. Kısmi ödeme gerçekleşecek.
    " ödeme tamamlandığı için blokaja gerek yok.
*    PERFORM vergili_tam_odeme TABLES lt_saticiya_odeme lt_devlete_odeme lt_return.
    PERFORM vergili_tam_odeme_v2 TABLES lt_saticiya_odeme lt_devlete_odeme lt_return
                                        lt_item .
  ENDIF.

*  IF lt_devlete_odeme IS NOT INITIAL.
*    " ödemenin devlete yapılacak kısmı.
*
*  ENDIF.

*{   ->>> Commented by Prodea Ozan Şahin - 20.03.2020 15:01:35
*  IF lt_vergisiz_kismi IS NOT INITIAL.
*    " satıcıya kısmi ödeme yapılacak. Devlete ödeme yok. "Kısmi ödeme gerçekleşecek.
*    " kısmi ödeme olduğu için belgeye blokaj konulacak( şu an yok).
*    PERFORM vergisiz_kismi_odeme TABLES lt_vergisiz_kismi lt_return lt_item lt_kismi.
*  ENDIF.
*}     <<<- End of  Commented - 20.03.2020 15:01:35

  IF lt_saticiya_kismi IS NOT INITIAL.
    " burada kısmi ödeme gerçekleşiyor. Satıcıya ve devlete kısmi ödeme yapılacak.
    " kısmi ödeme olduğu için belgeye blokaj konulacak( şu an yok).
    PERFORM vergili_kismi_odeme TABLES lt_saticiya_kismi lt_devlete_kismi lt_return lt_item lt_kismi.
  ENDIF.

*  IF lt_devlete_kismi IS NOT INITIAL.
*
*  ENDIF.

  "kaydet butonunu kapat.
  PERFORM hide_function_single USING '&GERI'.
  PERFORM hide_function_single USING '&KAYIT'.

  FIELD-SYMBOLS: <fs_sum_alv> LIKE LINE OF gs_scr_1903-sum_alv.
  DATA: ls_s_01 LIKE LINE OF lt_onay_durum.

  "z'li tabloları güncelle
  "1-Header tablosunu güncelle  ZSOG_FI_018_T_01
  LOOP AT gs_scr_1903-sum_alv ASSIGNING <fs_sum_alv>.
    PERFORM domain_get_value USING '05' 'ICON_COMPLETE' CHANGING ls_onay_durum-icon_text.

    CLEAR: ls_s_01.
    ls_s_01-mandt       = sy-mandt.
    ls_s_01-laufd       = <fs_sum_alv>-laufd.
    ls_s_01-laufi       = <fs_sum_alv>-laufi.
    ls_s_01-bukrs       = <fs_sum_alv>-bukrs.
    ls_s_01-lifnr       = <fs_sum_alv>-lifnr.
    ls_s_01-onay_durum  = '05'.
    ls_s_01-icon_name   = 'ICON_COMPLETE'.
    ls_s_01-icon_text   = ls_onay_durum-icon_text.
    ls_s_01-datum       = sy-datum.
    ls_s_01-uzeit       = sy-uzeit.
    ls_s_01-oneri_tutar = p_oneri.
*    ls_s_01-sektor      = s_sektr-low.
*    APPEND ls_s_01 TO lt_onay_durum.
    COLLECT ls_s_01 INTO lt_onay_durum."ozans

*  COLLECT value zsog_fi_018_t_01(
*                             mandt       = sy-mandt
*                             laufd       = <fs_sum_alv>-laufd
*                             laufi       = <fs_sum_alv>-laufi
*                             bukrs       = <fs_sum_alv>-bukrs
*                             lifnr       = <fs_sum_alv>-lifnr
*                             onay_durum  = '05'
*                             icon_name   = 'ICON_COMPLETE'
*                             icon_text   = ls_onay_durum-icon_text
*                             datum       = sy-datum
*                             uzeit       = sy-uzeit
*                             oneri_tutar = p_oneri
*                             sektor      = s_sektr-low
*                             ) INTO lt_onay_durum.
  ENDLOOP.

  "2-Tabloları güncelle
  IF lt_onay_durum IS NOT INITIAL.
    MODIFY zsog_fi_018_t_01 FROM TABLE lt_onay_durum.
    COMMIT WORK AND WAIT .
  ENDIF.
  IF lt_item IS NOT INITIAL.
    MODIFY zsog_fi_018_t_04 FROM TABLE lt_item.
    COMMIT WORK AND WAIT .
  ENDIF.

  IF lt_return IS NOT INITIAL.
    PERFORM msg_display_error_table TABLES lt_return.
  ENDIF.
  PERFORM lock_object.

ENDFORM.                    "create_document
*&---------------------------------------------------------------------*
*&      Form  show_all_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM show_all_detail.
  gs_scr_1903-name_of_alv = 'ALL_ALV'.
  PERFORM f_modify_detail_4_kismi TABLES gs_scr_1903-kismi gs_scr_1903-detail."added by Prodea(ufukk)-06.04.2020 13:57:28
  IF gs_scr_1903-detail IS NOT INITIAL.
    PERFORM initialize_alv TABLES gs_scr_1903-detail USING 'STANDARD'.
    PERFORM display_alv.
  ENDIF.
ENDFORM.                    "show_all_detail
*&---------------------------------------------------------------------*
*&      Form  DOMAIN_GET_VALUE
*&---------------------------------------------------------------------*
FORM domain_get_value  USING    pv_value
                                pv_domain TYPE dd07l-domname
                       CHANGING pv_text.

  DATA: lt_idd07v TYPE TABLE OF  dd07v.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = pv_domain
      text           = 'X'
    TABLES
      dd07v_tab      = lt_idd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
  ENDIF.

  DATA: ls_idd07v LIKE LINE OF lt_idd07v.

  READ TABLE lt_idd07v INTO ls_idd07v WITH KEY domvalue_l = pv_value.
  IF sy-subrc EQ 0.
    pv_text = ls_idd07v-ddtext.
  ENDIF.

ENDFORM.                    "domain_get_value
*&---------------------------------------------------------------------*
*& Form POP_UP_CONFIRM
*&---------------------------------------------------------------------*
FORM pop_up_confirm  USING    p_text_002
                              p_text_003
                              p_text_004
                              p_text_005
                              p_text_006
                              p_text_007
                     CHANGING pv_answer.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = p_text_002
      text_question         = p_text_003
      text_button_1         = p_text_004
      icon_button_1         = p_text_006
      text_button_2         = p_text_005
      icon_button_2         = p_text_007
      default_button        = '2'
      display_cancel_button = ' '
    IMPORTING
      answer                = pv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

ENDFORM.                    "pop_up_confirm
*&---------------------------------------------------------------------*
*&      Form  msg_display_error_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_RETURN  text
*----------------------------------------------------------------------*
FORM msg_display_error_table TABLES pt_return STRUCTURE bapiret2.
  CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
    EXPORTING
      it_message = pt_return[].
ENDFORM.                    "msg_display_error_table
*&---------------------------------------------------------------------*
*&      Form  c14z_messages_show_as_popup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_TAB     text
*----------------------------------------------------------------------*
FORM c14z_messages_show_as_popup TABLES lt_tab.
  CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
    TABLES
      i_message_tab = lt_tab[].
ENDFORM.                    "c14z_messages_show_as_popup
*&---------------------------------------------------------------------*
*&      Form  fill_single_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_TAB     text
*      -->PV_MSGID   text
*      -->PV_MSGTY   text
*      -->PV_MSGNO   text
*      -->PV_MSGV1   text
*      -->PV_MSGV2   text
*      -->PV_MSGV3   text
*      -->PV_MSGV4   text
*----------------------------------------------------------------------*
FORM fill_single_message TABLES lt_tab USING pv_msgid pv_msgty pv_msgno pv_msgv1 pv_msgv2 pv_msgv3  pv_msgv4.
  DATA: ls_tab TYPE esp1_message_wa_type.

  ls_tab-msgid  = pv_msgid.
  ls_tab-msgty  = pv_msgty.
  ls_tab-msgno  = pv_msgno.
  ls_tab-msgv1  = pv_msgv1.
  ls_tab-msgv2  = pv_msgv2.
  ls_tab-msgv3  = pv_msgv3.
  ls_tab-msgv4  = pv_msgv4.

  APPEND ls_tab TO lt_tab.

ENDFORM.                    "fill_single_message
*&---------------------------------------------------------------------*
*&      Form  LOCK_OBJECT
*&---------------------------------------------------------------------*
FORM lock_object .
  "" fonksiyon yok, incelenecek
*  CALL FUNCTION 'ENQUEUE_EZ_SO_LOCK'
*    EXPORTING
*      mode_zfi_so_t_004 = 'E'
*      mandt             = sy-mandt
*      laufd             = s_laufd-low
*      laufi             = s_laufi-low
*    EXCEPTIONS
*      foreign_lock      = 1
*      system_failure    = 2
*      OTHERS            = 3.

*
*  IF sy-subrc <> 0.
*    MESSAGE i006 WITH sy-msgv1 s_laufi-low s_laufd-low.
*    LEAVE LIST-PROCESSING.
*  ENDIF.

ENDFORM.                    "lock_object
*&---------------------------------------------------------------------*
*&      Form  unlock_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM unlock_object .
  "" fonksiyon yok, incelenecek
*  CALL FUNCTION 'DEQUEUE_EZ_SO_LOCK'
*    EXPORTING
*      mode_zfi_so_t_004 = 'E'
*      mandt             = sy-mandt
*      laufd             = s_laufd-low
*      laufi             = s_laufi-low.

ENDFORM.                    "unlock_object
*&---------------------------------------------------------------------*
*&      Form  SEARCH_HELP_FOR_LAUFI
*&---------------------------------------------------------------------*
FORM search_help_for_laufi .
  DATA: lt_ret        TYPE TABLE OF ddshretval.
  DATA: mf_dynpfields TYPE TABLE OF dynpread.
  DATA: mf_hlp_repid  LIKE sy-repid.

  DATA: ls_dynp LIKE LINE OF mf_dynpfields.
  DATA: lt_search TYPE zsog_fi_018_t_01 OCCURS 0.

  mf_hlp_repid = sy-repid.

*  APPEND value #( fieldname  = 'S_LAUFI-LOW' ) TO mf_dynpfields.
*  APPEND value #( fieldname  = 'S_LAUFD-LOW' ) TO mf_dynpfields.

  CLEAR: ls_dynp.
  ls_dynp-fieldname = 'S_LAUFI-LOW'.
  APPEND ls_dynp TO mf_dynpfields.

  CLEAR: ls_dynp.
  ls_dynp-fieldname = 'S_LAUFD-LOW'.
  APPEND ls_dynp TO mf_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = mf_hlp_repid
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = mf_dynpfields[]
    EXCEPTIONS
      invalid_abapworkarea = 01
      invalid_dynprofield  = 02
      invalid_dynproname   = 03
      invalid_dynpronummer = 04
      invalid_request      = 05
      no_fielddescription  = 06
      undefind_error       = 07.

  FIELD-SYMBOLS :<fs_search>     LIKE LINE OF lt_search.
  FIELD-SYMBOLS :<fs_dynpfields> LIKE LINE OF mf_dynpfields.

  DATA: ls_ret LIKE LINE OF lt_ret.
  DATA: ls_search LIKE LINE OF lt_search.

  SELECT laufd
         laufi
         bukrs
         onay_durum
         icon_text
*         sektor
     FROM zsog_fi_018_t_01 INTO CORRESPONDING FIELDS OF TABLE lt_search
     GROUP BY laufd
              laufi
              bukrs
              onay_durum
              icon_text.
*              sektor    .
*   SORT lt_search by
  LOOP AT lt_search ASSIGNING <fs_search>.
    PERFORM domain_get_value USING <fs_search>-onay_durum 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING <fs_search>-icon_text .
*    PERFORM domain_get_value USING <fs_search>-sektor 'ZFI_SO_D_SEKTOR' CHANGING <fs_search>-text .
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'LAUFI'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'S_LAUFI-LOW'
      value_org   = 'S'
    TABLES
      value_tab   = lt_search
      return_tab  = lt_ret.
  IF lt_ret IS NOT INITIAL.
*      data(ls_ret) = lt_ret[ 1 ].
    CLEAR: ls_ret.
    READ TABLE lt_ret INTO ls_ret INDEX 1.
    IF lt_search IS NOT INITIAL.
      CLEAR: ls_search.
      READ TABLE lt_search INTO ls_search WITH KEY laufi = ls_ret-fieldval.
*        data(ls_search) = lt_search[ laufi = ls_ret-fieldval ].
      IF sy-subrc EQ 0.
        CLEAR: s_laufd, s_laufd[].
        UNASSIGN: <fs_dynpfields>.
        LOOP AT mf_dynpfields ASSIGNING <fs_dynpfields>.
          CASE <fs_dynpfields>-fieldname .
            WHEN 'S_LAUFD-LOW'.
              <fs_dynpfields>-fieldvalue = ls_search-laufd+6(2) && |.| && ls_search-laufd+4(2) && |.| && ls_search-laufd+0(4).
            WHEN 'S_LAUFI-LOW'.
              <fs_dynpfields>-fieldvalue = ls_ret-fieldval.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.
        CALL FUNCTION 'DYNP_VALUES_UPDATE'
          EXPORTING
            dyname               = mf_hlp_repid
            dynumb               = sy-dynnr
          TABLES
            dynpfields           = mf_dynpfields[]
          EXCEPTIONS
            invalid_abapworkarea = 01
            invalid_dynprofield  = 02
            invalid_dynproname   = 03
            invalid_dynpronummer = 04
            invalid_request      = 05
            no_fielddescription  = 06
            undefind_error       = 07. "<<== note 148804

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "search_help_for_laufi
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR: gt_bdcdata.
  gt_bdcdata-program  = program.
  gt_bdcdata-dynpro   = dynpro.
  gt_bdcdata-dynbegin = 'X'.
  APPEND gt_bdcdata.
ENDFORM.                    "BDC_DYNPRO
*----------------------------------------------------------------------*
*        Form  BDC_FIELD                                               *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR gt_bdcdata.
  gt_bdcdata-fnam = fnam.
  gt_bdcdata-fval = fval.
  APPEND gt_bdcdata.
ENDFORM.                    "bdc_field
*&---------------------------------------------------------------------*
*&      Form  vergisiz_tam_odeme
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_VERGISIZ_TAM  text
*      -->PT_RETURN        text
*      -->PT_ITEM          text
*----------------------------------------------------------------------*
FORM vergisiz_tam_odeme TABLES lt_vergisiz_tam STRUCTURE zsog_fi_018_s_02
                               pt_return       STRUCTURE bapiret2
                               pt_item         STRUCTURE zsog_fi_018_t_04.

  DATA: lt_return TYPE TABLE OF bapiret2.
  DATA: lv_subrc  TYPE sy-subrc.
  DATA: ls_item   TYPE zsog_fi_018_t_04.

  FIELD-SYMBOLS: <fs_vergisiz_tam> LIKE LINE OF lt_vergisiz_tam,
                 <fs_sum_alv>      LIKE LINE OF gs_scr_1903-sum_alv.

  LOOP AT lt_vergisiz_tam ASSIGNING <fs_vergisiz_tam>.
    PERFORM mass_change TABLES lt_return USING <fs_vergisiz_tam> 'ZLSCH' ''.
*    ls_item = corresponding #( <fs_vergisiz_tam> ).
    MOVE-CORRESPONDING <fs_vergisiz_tam> TO ls_item.

    UNASSIGN <fs_sum_alv>.
    READ TABLE gs_scr_1903-sum_alv ASSIGNING <fs_sum_alv> WITH KEY lifnr = <fs_vergisiz_tam>-lifnr.
    ls_item-kunnr = <fs_sum_alv>-kunnr.
    ls_item-uname = gs_scr_1903-auth-uname.
    ls_item-statu = gs_scr_1903-auth-statu.
    ls_item-mandt = sy-mandt.
    PERFORM domain_get_value USING '05' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING ls_item-icon_text .
    ls_item-onay_durum = '05'.
    ls_item-icon_name  = 'ICON_COMPLETE'.
    ls_item-datum      = sy-datum.
    ls_item-uzeit      = sy-uzeit.
    ls_item-tam_odeme  = 'X'.
    APPEND ls_item TO pt_item.
    CLEAR: ls_item.
*    IF <fs_sum_alv> IS ASSIGNED .
*      UNASSIGN <fs_sum_alv>.
*    ENDIF.
  ENDLOOP.
  IF lt_return IS NOT INITIAL.
    APPEND LINES OF lt_return TO pt_return.
  ENDIF.
ENDFORM.                    "vergisiz_tam_odeme
*&---------------------------------------------------------------------*
*& Form MASS_CHANGE
*&---------------------------------------------------------------------*
FORM mass_change TABLES pt_return STRUCTURE bapiret2
                  USING ps_odeme  STRUCTURE zsog_fi_018_s_02
                        pv_field
               CHANGING pv_subrc TYPE sy-subrc."xozans
  DATA: ls_bseg   TYPE bseg.
  DATA: lt_errtab   TYPE tpit_t_errdoc,
        ls_errtab   TYPE tpit_errdoc,
        lt_buztab   TYPE tpit_t_buztab,
        ls_buztab   TYPE tpit_buztab,
        lt_fldtab   TYPE tpit_t_fname,
        ls_fldtab   TYPE tpit_fname,
        lv_fiedl(5).


  FIELD-SYMBOLS: <fs_field>      TYPE any,
                 <fs_bseg_fiedl> TYPE any.

  lv_fiedl = pv_field.

  IF pv_field EQ 'ZLSCH'.
    ASSIGN COMPONENT pv_field OF STRUCTURE ps_odeme TO <fs_field>.
    IF <fs_field> IS ASSIGNED.
      ASSIGN COMPONENT pv_field OF STRUCTURE ls_bseg TO <fs_bseg_fiedl>.
      IF <fs_bseg_fiedl> IS ASSIGNED.
        <fs_bseg_fiedl> = <fs_field>.
      ENDIF.
    ENDIF.
  ELSEIF pv_field EQ 'ZLSPR'.
    ASSIGN COMPONENT pv_field OF STRUCTURE ls_bseg TO <fs_bseg_fiedl>.
    IF <fs_bseg_fiedl> IS ASSIGNED.
      <fs_bseg_fiedl> = 'A'.
    ENDIF.

  ELSEIF pv_field EQ  'REVRS'.
    lv_fiedl = 'ZLSCH'.

  ELSEIF pv_field EQ 'KISMI'.
    lv_fiedl = 'ZLSPR'.
  ENDIF.

*{ ->>> BEGIN of INSERT by Prodea (Ufuk Koyuncu) - 07.04.2020 13:59:28
  IF ps_odeme-zlspr = 'A'.
    IF ps_odeme-kalan_odeme = ps_odeme-odenecek_tutar.
      CLEAR ls_bseg-zlspr.
      ls_bseg-zlsch = 'M'.
      CLEAR ls_fldtab.

      ls_fldtab-fname  = 'ZLSPR'.
      ls_fldtab-aenkz  = 'X'.
      APPEND ls_fldtab TO lt_fldtab.

      ls_fldtab-fname  = 'ZLSCH'.
      ls_fldtab-aenkz  = 'X'.
      APPEND ls_fldtab TO lt_fldtab.
    ENDIF.
  ENDIF.
*} <<<- END of INSERT by Prodea (Ufuk Koyuncu) - 07.04.2020 13:59:28
  ls_bseg-mandt = sy-mandt.
  ls_bseg-bukrs = ps_odeme-bukrs.
  ls_bseg-belnr = ps_odeme-belnr.
  ls_bseg-gjahr = ps_odeme-gjahr.

  ls_buztab-bukrs = ps_odeme-bukrs.
  ls_buztab-belnr = ps_odeme-belnr.
  ls_buztab-gjahr = ps_odeme-gjahr.
  ls_buztab-buzei = ps_odeme-buzei.
  ls_buztab-bschl = '31'.
  APPEND ls_buztab TO lt_buztab.

  ls_fldtab-fname  = lv_fiedl. "pv_field.
  ls_fldtab-aenkz  = 'X'.
  APPEND ls_fldtab TO lt_fldtab.

  CALL FUNCTION 'FI_ITEMS_MASS_CHANGE'
    EXPORTING
      s_bseg     = ls_bseg
    IMPORTING
      errtab     = lt_errtab
    TABLES
      it_buztab  = lt_buztab
      it_fldtab  = lt_fldtab
    EXCEPTIONS
      bdc_errors = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  DATA: ls_bapiret2 TYPE bapiret2.

  LOOP AT lt_errtab INTO ls_errtab WHERE err-msgtyp CA 'EAX'.
    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
*    APPEND value bapiret2( type       = 'S'
*                           ID         = 'ZSOG_FI_018'
*                           NUMBER     = '014'
*                           message_v1 = ps_odeme-belnr
*                      ) TO pt_return.
*    commented by ozans 09.04.2020
*    CLEAR: ls_bapiret2.
*    ls_bapiret2-type       = 'S'.
*    ls_bapiret2-id         = 'ZSOG_FI_018'.
*    ls_bapiret2-number     = '031'."ls_bapiret2-number     = '014'.
*    ls_bapiret2-message_v1 = ps_odeme-belnr.
*    APPEND ls_bapiret2 TO pt_return.

    pv_subrc = 0."ozans
  ELSE.
    LOOP AT lt_errtab INTO ls_errtab.
*      APPEND value bapiret2( type       = ls_errtab-err-msgtyp
*                             ID         = ls_errtab-err-msgid
*                             NUMBER     = ls_errtab-err-msgnr
*                             message_v1 = ls_errtab-err-msgv1
*                       ) TO pt_return.
      CLEAR: ls_bapiret2.
      ls_bapiret2-type       = ls_errtab-err-msgtyp.
      ls_bapiret2-id         = ls_errtab-err-msgid.
      ls_bapiret2-number     = ls_errtab-err-msgnr.
      ls_bapiret2-message_v1 = ls_errtab-err-msgv1.
      ls_bapiret2-message_v2 = ls_errtab-err-msgv2.
      ls_bapiret2-message_v3 = ls_errtab-err-msgv3.
      ls_bapiret2-message_v4 = ls_errtab-err-msgv4.
      APPEND ls_bapiret2 TO pt_return.
      pv_subrc = 4."ozans
    ENDLOOP.
  ENDIF.
ENDFORM.                    "mass_change
*&---------------------------------------------------------------------*
*&      Form  mass_change_blokaj_payment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_RETURN  text
*      -->PS_ODEME   text
*----------------------------------------------------------------------*
FORM mass_change_blokaj_payment TABLES pt_return STRUCTURE bapiret2
                                 USING ps_odeme  STRUCTURE zsog_fi_018_s_02.

  DATA: ls_bseg   TYPE bseg.
  DATA: lt_errtab   TYPE tpit_t_errdoc,
        ls_errtab   TYPE tpit_errdoc,
        lt_buztab   TYPE tpit_t_buztab,
        ls_buztab   TYPE tpit_buztab,
        lt_fldtab   TYPE tpit_t_fname,
        ls_fldtab   TYPE tpit_fname,
        lv_fiedl(5).

  FIELD-SYMBOLS: <fs_field>      TYPE any,
                 <fs_bseg_fiedl> TYPE any.

  lv_fiedl = 'ZLSCH'.

  ASSIGN COMPONENT 'ZLSCH' OF STRUCTURE ps_odeme TO <fs_field>.
  IF <fs_field> IS ASSIGNED.
    ASSIGN COMPONENT 'ZLSCH' OF STRUCTURE ls_bseg TO <fs_bseg_fiedl>.
    IF <fs_bseg_fiedl> IS ASSIGNED.
      <fs_bseg_fiedl> = <fs_field>.
    ENDIF.
  ENDIF.

  ls_fldtab-fname  = lv_fiedl. "pv_field.
  ls_fldtab-aenkz  = 'X'.
  APPEND ls_fldtab TO lt_fldtab.

  CLEAR: ls_fldtab.

  lv_fiedl = 'ZLSPR'.
  ls_fldtab-fname  = lv_fiedl. "pv_field.
  ls_fldtab-aenkz  = 'X'.
  APPEND ls_fldtab TO lt_fldtab.

  ls_buztab-bukrs = ps_odeme-bukrs.
  ls_buztab-belnr = ps_odeme-belnr.
  ls_buztab-gjahr = ps_odeme-gjahr.
  ls_buztab-buzei = ps_odeme-buzei.
  ls_buztab-bschl = '31'.
  APPEND ls_buztab TO lt_buztab.


  ls_bseg-mandt = sy-mandt.
  ls_bseg-bukrs = ps_odeme-bukrs.
  ls_bseg-belnr = ps_odeme-belnr.
  ls_bseg-gjahr = ps_odeme-gjahr.


  CALL FUNCTION 'FI_ITEMS_MASS_CHANGE'
    EXPORTING
      s_bseg     = ls_bseg
    IMPORTING
      errtab     = lt_errtab
    TABLES
      it_buztab  = lt_buztab
      it_fldtab  = lt_fldtab
    EXCEPTIONS
      bdc_errors = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  DATA: ls_bapiret2 TYPE bapiret2.

  LOOP AT lt_errtab INTO ls_errtab WHERE err-msgtyp CA 'EAX'.
    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
*    APPEND value bapiret2( type       = 'S'
*                           ID         = 'ZSOG_FI_018'
*                           NUMBER     = '014'
*                           message_v1 = ps_odeme-belnr
*                      ) TO pt_return.

    CLEAR: ls_bapiret2.
    ls_bapiret2-type       = 'S'.
    ls_bapiret2-id         = 'ZSOG_FI_018'.
    ls_bapiret2-number     = '014'.
    ls_bapiret2-message_v1 = ps_odeme-belnr.
    APPEND ls_bapiret2 TO pt_return.

  ELSE.
    LOOP AT lt_errtab INTO ls_errtab.
*      APPEND value bapiret2( type       = ls_errtab-err-msgtyp
*                             ID         = ls_errtab-err-msgid
*                             NUMBER     = ls_errtab-err-msgnr
*                             message_v1 = ls_errtab-err-msgv1
*                       ) TO pt_return.

      CLEAR: ls_bapiret2.
      ls_bapiret2-type       = ls_errtab-err-msgtyp.
      ls_bapiret2-id         = ls_errtab-err-msgid.
      ls_bapiret2-number     = ls_errtab-err-msgnr.
      ls_bapiret2-message_v1 = ls_errtab-err-msgv1.
      ls_bapiret2-message_v2 = ls_errtab-err-msgv2.
      ls_bapiret2-message_v3 = ls_errtab-err-msgv3.
      ls_bapiret2-message_v4 = ls_errtab-err-msgv4.
      APPEND ls_bapiret2 TO pt_return.

    ENDLOOP.
  ENDIF.


ENDFORM.                    "mass_change_blokaj_payment
*&---------------------------------------------------------------------*
*&      Form  vergili_tam_odeme
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_SATICIYA_ODEME  text
*      -->LT_DEVLETE_ODEME   text
*      -->PT_RETURN          text
*----------------------------------------------------------------------*
FORM vergili_tam_odeme TABLES lt_saticiya_odeme STRUCTURE zsog_fi_018_s_02
                              lt_devlete_odeme  STRUCTURE zsog_fi_018_s_02
                              pt_return STRUCTURE bapiret2.
  DATA:  lt_return TYPE TABLE OF bapiret2.
  DATA : ls_option TYPE ctu_params,
         lv_datum  TYPE char10,
         lv_tutar  TYPE char15,
         lv_zbd1t  TYPE char3,
         lv_zlsch  TYPE char1.

  CLEAR: gt_bdcdata, gt_bdcdata[],
         gt_messtab, gt_messtab[].

  ls_option-dismode = p_mode.
  ls_option-updmode = 'S'.

  FIELD-SYMBOLS: <fs_saticiya_odeme> LIKE LINE OF lt_saticiya_odeme,
                 <fs_devlete_odeme>  LIKE LINE OF lt_devlete_odeme.

  LOOP AT lt_saticiya_odeme ASSIGNING <fs_saticiya_odeme>.
    PERFORM bdc_dynpro      USING 'SAPMF05A' '0132'.
    PERFORM bdc_field       USING 'RF05A-BELNR' <fs_saticiya_odeme>-belnr.
    PERFORM bdc_field       USING 'RF05A-BUKRS' <fs_saticiya_odeme>-bukrs.
    PERFORM bdc_field       USING 'RF05A-GJAHR' <fs_saticiya_odeme>-gjahr.
    PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
    PERFORM bdc_dynpro      USING 'SAPMF05A' '0133'.
    PERFORM bdc_field       USING 'BDC_CURSOR' 'BKPF-BLDAT'.
    lv_datum = |{ <fs_saticiya_odeme>-bldat+6(2) }| && |.| && |{ <fs_saticiya_odeme>-bldat+4(2) }| && |.| && |{ <fs_saticiya_odeme>-bldat+0(4) }|.
    PERFORM bdc_field       USING 'BKPF-BLDAT' lv_datum.
    PERFORM bdc_field       USING 'BKPF-BLART' 'KS'.
    CLEAR: lv_datum.
    lv_datum = |{ <fs_saticiya_odeme>-budat+6(2) }| && |.| && |{ <fs_saticiya_odeme>-budat+4(2) }| && |.| && |{ <fs_saticiya_odeme>-budat+0(4) }|.
    PERFORM bdc_field       USING 'BKPF-BUDAT' lv_datum.
    PERFORM bdc_dynpro      USING 'SAPMF05A' '0306'.
    PERFORM bdc_field       USING 'BDC_CURSOR' 'BSEG-ZLSCH'.
    PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
    lv_tutar = <fs_saticiya_odeme>-ony_satici_odeme.
    REPLACE FIRST OCCURRENCE OF '.' IN lv_tutar WITH ','.
    CONDENSE lv_tutar.
    PERFORM bdc_field       USING 'BSEG-WRBTR' lv_tutar.
    PERFORM bdc_field       USING 'BSEG-ZTERM' ''.
    lv_zbd1t = <fs_saticiya_odeme>-zbd1t.
    CONDENSE lv_zbd1t.
    IF lv_zbd1t < 100 AND lv_zbd1t NE '0'.
      lv_zbd1t = |0| && |{ lv_zbd1t }|.
    ENDIF.
    CONDENSE lv_zbd1t.
    PERFORM bdc_field       USING 'BSEG-ZBD1T' lv_zbd1t.
    lv_zlsch = <fs_saticiya_odeme>-zlsch.
    PERFORM bdc_field       USING 'BSEG-ZLSCH' lv_zlsch.  "ps_kismi_odeme-zlsch.
    PERFORM bdc_field       USING 'BDC_OKCODE' '=BU'.

    CALL TRANSACTION 'F-59' USING gt_bdcdata
        OPTIONS FROM ls_option
        MESSAGES INTO gt_messtab.
    LOOP AT gt_messtab WHERE msgtyp CA 'EAX'.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      COMMIT WORK AND WAIT.
      PERFORM mass_change TABLES lt_return USING <fs_saticiya_odeme> 'ZLSCH' ''.
*      PERFORM mass_change TABLES lt_return USING <fs_saticiya_odeme> 'ZLSPR'.
    ENDIF.

    DATA: ls_bapiret2 TYPE bapiret2.

    LOOP AT gt_messtab.

*      APPEND value bapiret2(
*        type          = gt_messtab-msgtyp
*        ID            = gt_messtab-msgid
*        NUMBER        = gt_messtab-msgnr
*        message       = gt_messtab-msgv1
*        message_v1    = gt_messtab-msgv1
*        message_v2    = gt_messtab-msgv2
*        message_v3    = gt_messtab-msgv3
*        message_v4    = gt_messtab-msgv4 ) TO lt_return.

      CLEAR: ls_bapiret2.
      ls_bapiret2-type       = gt_messtab-msgtyp.
      ls_bapiret2-id         = gt_messtab-msgid.
      ls_bapiret2-number     = gt_messtab-msgnr.
      ls_bapiret2-message_v1 = gt_messtab-msgv1.
      ls_bapiret2-message_v2 = gt_messtab-msgv2.
      ls_bapiret2-message_v3 = gt_messtab-msgv3.
      ls_bapiret2-message_v4 = gt_messtab-msgv4.
      APPEND ls_bapiret2 TO lt_return.

    ENDLOOP.
    CLEAR: gt_bdcdata, gt_bdcdata[], gt_messtab, gt_messtab[].

    LOOP AT lt_devlete_odeme ASSIGNING <fs_devlete_odeme> WHERE laufd  = <fs_saticiya_odeme>-laufd
                                                            AND laufi  = <fs_saticiya_odeme>-laufi
                                                            AND bukrs  = <fs_saticiya_odeme>-bukrs
                                                            AND lifnr  = <fs_saticiya_odeme>-lifnr
                                                            AND belnr  = <fs_saticiya_odeme>-belnr
                                                            AND gjahr  = <fs_saticiya_odeme>-gjahr
                                                            AND buzei  = <fs_saticiya_odeme>-buzei.
      CLEAR: lv_datum, lv_tutar, lv_zbd1t, lv_zlsch.


      PERFORM bdc_dynpro      USING 'SAPMF05A' '0132'.
      PERFORM bdc_field       USING 'RF05A-BELNR' <fs_devlete_odeme>-belnr.
      PERFORM bdc_field       USING 'RF05A-BUKRS' <fs_devlete_odeme>-bukrs.
      PERFORM bdc_field       USING 'RF05A-GJAHR' <fs_devlete_odeme>-gjahr.
      PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '0615'.
      PERFORM bdc_field       USING 'BDC_CURSOR' 'RF05A-PSBET'.
      PERFORM bdc_field       USING 'BDC_OKCODE' '=GO'.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '0133'.
      PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
      lv_datum = |{ sy-datum+6(2) }| && |.| && |{ sy-datum+4(2) }| && |.| && |{ sy-datum+0(4) }|.
      PERFORM bdc_field       USING 'BKPF-BLDAT' lv_datum.
      PERFORM bdc_field       USING 'BKPF-BLART' 'KS'.
      PERFORM bdc_field       USING 'BKPF-BUDAT' lv_datum.
      PERFORM bdc_field       USING 'BKPF-MONAT' sy-datum+4(2).
      CLEAR: lv_datum.
      lv_datum = |{ <fs_devlete_odeme>-budat+6(2) }| && |.| && |{ <fs_devlete_odeme>-budat+4(2) }| && |.| && |{ <fs_devlete_odeme>-budat+0(4) }|.
      PERFORM bdc_field       USING 'BKPF-WWERT' lv_datum.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '0306'.
      PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
      lv_tutar = <fs_devlete_odeme>-ony_devlet_odeme.
      REPLACE FIRST OCCURRENCE OF '.' IN lv_tutar WITH ','.
      CONDENSE lv_tutar.
      PERFORM bdc_field       USING 'BSEG-WRBTR' lv_tutar.
      PERFORM bdc_field       USING 'BDC_OKCODE' '=BU'.


*      PERFORM bdc_dynpro      USING 'SAPMF05A' '0132'.
*      PERFORM bdc_field       USING 'RF05A-BELNR' <fs_devlete_odeme>-belnr.
*      PERFORM bdc_field       USING 'RF05A-BUKRS' <fs_devlete_odeme>-bukrs.
*      PERFORM bdc_field       USING 'RF05A-GJAHR' <fs_devlete_odeme>-gjahr.
*      PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
*      PERFORM bdc_dynpro      USING 'SAPMF05A' '0133'.
*      "burada kısmi ödeme belgesi olduğu için bir popup açıyor açacak mı emin değilim. commiten sonra açıyor.
*      PERFORM bdc_dynpro      USING 'SAPMF05A' '0615'.
*      PERFORM bdc_field       USING 'BDC_OKCODE' '=GO'.
*
*      PERFORM bdc_field       USING 'BDC_CURSOR' 'BKPF-BLDAT'.
*      lv_datum = |{ <fs_devlete_odeme>-bldat+6(2) }| && |.| && |{ <fs_devlete_odeme>-bldat+4(2) }| && |.| && |{ <fs_devlete_odeme>-bldat+0(4) }|.
*      PERFORM bdc_field       USING 'BKPF-BLDAT' lv_datum.
*      PERFORM bdc_field       USING 'BKPF-BLART' 'KS'.
*      CLEAR: lv_datum.
*      lv_datum = |{ <fs_devlete_odeme>-budat+6(2) }| && |.| && |{ <fs_devlete_odeme>-budat+4(2) }| && |.| && |{ <fs_devlete_odeme>-budat+0(4) }|.
*      PERFORM bdc_field       USING 'BKPF-BUDAT' lv_datum.
*      PERFORM bdc_dynpro      USING 'SAPMF05A' '0306'.
*      PERFORM bdc_field       USING 'BDC_CURSOR' 'BSEG-ZLSCH'.
*      PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
*      lv_tutar = <fs_devlete_odeme>-ony_devlet_odeme.
*      REPLACE FIRST OCCURRENCE OF '.' IN lv_tutar WITH ','.
*      CONDENSE lv_tutar.
*      PERFORM bdc_field       USING 'BSEG-WRBTR' lv_tutar.
*      PERFORM bdc_field       USING 'BSEG-ZTERM' ''.
*      lv_zbd1t = <fs_devlete_odeme>-zbd1t.
*      CONDENSE lv_zbd1t.
*      IF lv_zbd1t < 100 AND lv_zbd1t NE '0'.
*        lv_zbd1t = |0| && |{ lv_zbd1t }|.
*      ENDIF.
*      CONDENSE lv_zbd1t.
*      PERFORM bdc_field       USING 'BSEG-ZBD1T' lv_zbd1t.
*      lv_zlsch = <fs_devlete_odeme>-zlsch.
*      PERFORM bdc_field       USING 'BSEG-ZLSCH' lv_zlsch.  "ps_kismi_odeme-zlsch.
*      PERFORM bdc_field       USING 'BDC_OKCODE' '=BU'.

    ENDLOOP.

    CALL TRANSACTION 'F-59' USING gt_bdcdata
        OPTIONS FROM ls_option
        MESSAGES INTO gt_messtab.
    LOOP AT gt_messtab WHERE msgtyp CA 'EAX'.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      COMMIT WORK AND WAIT.
      PERFORM mass_change TABLES lt_return USING <fs_devlete_odeme> 'ZLSCH' ''. "ödeme biçimi
*      PERFORM mass_change TABLES lt_return USING <fs_devlete_odeme> 'ZLSPR'. "ödeme blokajı burada kalan tutar sıfırlanırsa blokajı kaldır.
*      PERFORM mass_change TABLES lt_return USING <fs_devlete_odeme> 'KISMI'. "ödeme blokajı burada kaldır.

    ENDIF.


    LOOP AT gt_messtab.
*      APPEND value bapiret2(
*        type          = gt_messtab-msgtyp
*        ID            = gt_messtab-msgid
*        NUMBER        = gt_messtab-msgnr
*        message       = gt_messtab-msgv1
*        message_v1    = gt_messtab-msgv1
*        message_v2    = gt_messtab-msgv2
*        message_v3    = gt_messtab-msgv3
*        message_v4    = gt_messtab-msgv4 ) TO lt_return.
      CLEAR: ls_bapiret2.
      ls_bapiret2-type       = gt_messtab-msgtyp.
      ls_bapiret2-id         = gt_messtab-msgid.
      ls_bapiret2-number     = gt_messtab-msgnr.
      ls_bapiret2-message_v1 = gt_messtab-msgv1.
      ls_bapiret2-message_v2 = gt_messtab-msgv2.
      ls_bapiret2-message_v3 = gt_messtab-msgv3.
      ls_bapiret2-message_v4 = gt_messtab-msgv4.
      APPEND ls_bapiret2 TO lt_return.
    ENDLOOP.
    CLEAR: lv_datum, lv_tutar, lv_zbd1t, lv_zlsch, gt_messtab, gt_messtab[], gt_bdcdata, gt_bdcdata[] .
  ENDLOOP.

  IF lt_return IS NOT INITIAL.
    APPEND LINES OF lt_return TO pt_return.
  ENDIF.
ENDFORM.                    "vergili_tam_odeme
*&---------------------------------------------------------------------*
*&      Form  vergili_tam_odeme_v2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_SATICIYA_ODEME  text
*      -->LT_DEVLETE_ODEME   text
*      -->PT_RETURN          text
*      -->PT_ITEM            text
*----------------------------------------------------------------------*
FORM vergili_tam_odeme_v2 TABLES lt_saticiya_odeme STRUCTURE zsog_fi_018_s_02
                                 lt_devlete_odeme  STRUCTURE zsog_fi_018_s_02
                                 pt_return STRUCTURE bapiret2
                                 pt_item   STRUCTURE zsog_fi_018_t_04.


  DATA: lt_return TYPE TABLE OF bapiret2.
  DATA: lv_subrc  TYPE sy-subrc.
  DATA: ls_item   TYPE zsog_fi_018_t_04.

  FIELD-SYMBOLS: <fs_saticiya_odeme> LIKE LINE OF lt_saticiya_odeme,
                 <fs_sum_alv>        LIKE LINE OF gs_scr_1903-sum_alv.

  LOOP AT lt_saticiya_odeme ASSIGNING <fs_saticiya_odeme>.
    PERFORM mass_change TABLES lt_return USING <fs_saticiya_odeme> 'ZLSCH' ''.

*    ls_item = corresponding #( <fs_saticiya_odeme> ).
    MOVE-CORRESPONDING <fs_saticiya_odeme> TO ls_item.

    UNASSIGN <fs_sum_alv>.
    READ TABLE gs_scr_1903-sum_alv ASSIGNING <fs_sum_alv> WITH KEY lifnr = <fs_saticiya_odeme>-lifnr.
    ls_item-kunnr = <fs_sum_alv>-kunnr.
    ls_item-uname = gs_scr_1903-auth-uname.
    ls_item-statu = gs_scr_1903-auth-statu.
    ls_item-mandt = sy-mandt.
    PERFORM domain_get_value USING '05' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING ls_item-icon_text .
    ls_item-onay_durum = '05'.
    ls_item-icon_name  = 'ICON_COMPLETE'.
    ls_item-datum      = sy-datum.
    ls_item-uzeit      = sy-uzeit.
    ls_item-tam_odeme  = 'X'.
    APPEND ls_item TO pt_item.
    CLEAR: ls_item.

  ENDLOOP.
  IF lt_return IS NOT INITIAL.
    APPEND LINES OF lt_return TO pt_return.
  ENDIF.

ENDFORM.                    "vergili_tam_odeme_v2
*&---------------------------------------------------------------------*
*&      Form  vergisiz_kismi_odeme
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_VERGISIZ_KISMI  text
*      -->PT_RETURN          text
*      -->PT_ITEM            text
*      -->PT_KISMI           text
*----------------------------------------------------------------------*
FORM vergisiz_kismi_odeme TABLES lt_vergisiz_kismi STRUCTURE zsog_fi_018_s_02
                                 pt_return         STRUCTURE bapiret2
                                 pt_item           STRUCTURE zsog_fi_018_t_04
                                 pt_kismi          STRUCTURE zsog_fi_018_t_06.

  DATA:  lt_return TYPE TABLE OF bapiret2.
  DATA : ls_option      TYPE ctu_params,
         lv_datum       TYPE char10,
         lv_tutar       TYPE char15,
         lv_zbd1t       TYPE char3,
         lv_zlsch       TYPE char1,
         lv_cont(1),
         lv_kismi_belno TYPE bsik-belnr.

  CLEAR: gt_bdcdata, gt_bdcdata[],
         gt_messtab, gt_messtab[].

  DATA: ls_item         TYPE zsog_fi_018_t_04,
        lv_kismi_toplam TYPE bseg-wrbtr.

  ls_option-dismode = p_mode.
  ls_option-updmode = 'S'.

  DATA: ls_sum_alv LIKE LINE OF gs_scr_1903-sum_alv.
  FIELD-SYMBOLS: <fs_saticiya_kismi> LIKE LINE OF lt_vergisiz_kismi.
  FIELD-SYMBOLS: <fs_kismi>          LIKE LINE OF ls_sum_alv-kismi_alv.
  FIELD-SYMBOLS: <fs_sum_alv>        LIKE LINE OF gs_scr_1903-sum_alv.

  UNASSIGN: <fs_saticiya_kismi>.
  LOOP AT lt_vergisiz_kismi ASSIGNING <fs_saticiya_kismi>.
*    data(ls_sum_alv) = gs_scr_1903-sum_alv[ lifnr  = <fs_saticiya_kismi>-lifnr ].
    CLEAR: ls_sum_alv.
    READ TABLE gs_scr_1903-sum_alv INTO ls_sum_alv WITH KEY lifnr  = <fs_saticiya_kismi>-lifnr.

    UNASSIGN: <fs_kismi>.
    LOOP AT ls_sum_alv-kismi_alv ASSIGNING <fs_kismi> WHERE rebzg = <fs_saticiya_kismi>-belnr
                                                        AND rebzj = <fs_saticiya_kismi>-gjahr .

      lv_kismi_toplam = <fs_kismi>-wrbtr + lv_kismi_toplam.
    ENDLOOP.
    IF sy-subrc EQ 0. "önceden oluşturulmuş kısmi ödeme belgeesi vardır.

      lv_kismi_toplam = lv_kismi_toplam + <fs_saticiya_kismi>-ony_devlet_odeme + <fs_saticiya_kismi>-ony_satici_odeme.
      IF lv_kismi_toplam EQ <fs_saticiya_kismi>-belge_tutar.
*         PERFORM mass_change TABLES lt_return USING <fs_saticiya_kismi> 'KISMI'."blokaj kaldır.
*        PERFORM mass_change TABLES lt_return USING <fs_saticiya_kismi> 'KISMI'."blokaj kaldır.
*        PERFORM mass_change TABLES lt_return USING <fs_saticiya_kismi> 'ZLSCH'."ödeme biçimi
        PERFORM mass_change_blokaj_payment TABLES lt_return USING <fs_saticiya_kismi> .
        lv_cont = 'X'.
      ELSE.

        PERFORM bdc_dynpro      USING 'SAPMF05A' '0132'.
        PERFORM bdc_field       USING 'RF05A-BELNR' <fs_saticiya_kismi>-belnr.
        PERFORM bdc_field       USING 'RF05A-BUKRS' <fs_saticiya_kismi>-bukrs.
        PERFORM bdc_field       USING 'RF05A-GJAHR' <fs_saticiya_kismi>-gjahr.
        PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.

        PERFORM bdc_dynpro      USING 'SAPMF05A' '0615'.
        PERFORM bdc_field       USING 'BDC_CURSOR' 'RF05A-PSBET'.
        PERFORM bdc_field       USING 'BDC_OKCODE' '=GO'.

        PERFORM bdc_dynpro      USING 'SAPMF05A' '0133'.
        PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
        lv_datum = |{ sy-datum+6(2) }| && |.| && |{ sy-datum+4(2) }| && |.| && |{ sy-datum+0(4) }|.
        PERFORM bdc_field       USING 'BKPF-BLDAT' lv_datum.
        PERFORM bdc_field       USING 'BKPF-BLART' 'KS'.
        PERFORM bdc_field       USING 'BKPF-BUDAT' lv_datum.
        PERFORM bdc_field       USING 'BKPF-MONAT' sy-datum+4(2).
        CLEAR: lv_datum.
        lv_datum = |{ <fs_saticiya_kismi>-budat+6(2) }| && |.| && |{ <fs_saticiya_kismi>-budat+4(2) }| && |.| && |{ <fs_saticiya_kismi>-budat+0(4) }|.
        PERFORM bdc_field       USING 'BKPF-WWERT' lv_datum.

        PERFORM bdc_dynpro      USING 'SAPMF05A' '0306'.
        PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
        lv_tutar = <fs_saticiya_kismi>-ony_devlet_odeme + <fs_saticiya_kismi>-ony_satici_odeme.
        REPLACE FIRST OCCURRENCE OF '.' IN lv_tutar WITH ','.
        CONDENSE lv_tutar.
        PERFORM bdc_field       USING 'BSEG-WRBTR' lv_tutar.
        lv_zlsch = <fs_saticiya_kismi>-zlsch.
        PERFORM bdc_field       USING 'BSEG-ZLSCH' lv_zlsch.  "ps_kismi_odeme-zlsch.
        PERFORM bdc_field       USING 'BDC_OKCODE' '=BU'.
      ENDIF.

    ELSE.
      PERFORM bdc_dynpro      USING 'SAPMF05A' '0132'.
      PERFORM bdc_field       USING 'RF05A-BELNR' <fs_saticiya_kismi>-belnr.
      PERFORM bdc_field       USING 'RF05A-BUKRS' <fs_saticiya_kismi>-bukrs.
      PERFORM bdc_field       USING 'RF05A-GJAHR' <fs_saticiya_kismi>-gjahr.
      PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
      PERFORM bdc_dynpro      USING 'SAPMF05A' '0133'.
      PERFORM bdc_field       USING 'BDC_CURSOR' 'BKPF-BLDAT'.
      lv_datum = |{ <fs_saticiya_kismi>-bldat+6(2) }| && |.| && |{ <fs_saticiya_kismi>-bldat+4(2) }| && |.| && |{ <fs_saticiya_kismi>-bldat+0(4) }|.
      PERFORM bdc_field       USING 'BKPF-BLDAT' lv_datum.
      PERFORM bdc_field       USING 'BKPF-BLART' 'KS'.
      CLEAR: lv_datum.
      lv_datum = |{ <fs_saticiya_kismi>-budat+6(2) }| && |.| && |{ <fs_saticiya_kismi>-budat+4(2) }| && |.| && |{ <fs_saticiya_kismi>-budat+0(4) }|.
      PERFORM bdc_field       USING 'BKPF-BUDAT' lv_datum.
      PERFORM bdc_dynpro      USING 'SAPMF05A' '0306'.
      PERFORM bdc_field       USING 'BDC_CURSOR' 'BSEG-ZLSCH'.
      PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
      lv_tutar = <fs_saticiya_kismi>-ony_satici_odeme + <fs_saticiya_kismi>-ony_devlet_odeme.
      REPLACE FIRST OCCURRENCE OF '.' IN lv_tutar WITH ','.
      CONDENSE lv_tutar.
      PERFORM bdc_field       USING 'BSEG-WRBTR' lv_tutar.
      PERFORM bdc_field       USING 'BSEG-ZTERM' ''.
      lv_zbd1t = <fs_saticiya_kismi>-zbd1t.
      CONDENSE lv_zbd1t.
      IF lv_zbd1t < 100 AND lv_zbd1t NE '0'.
        lv_zbd1t = |0| && |{ lv_zbd1t }|.
      ENDIF.
      CONDENSE lv_zbd1t.
      PERFORM bdc_field       USING 'BSEG-ZBD1T' lv_zbd1t.
      lv_zlsch = <fs_saticiya_kismi>-zlsch.
      PERFORM bdc_field       USING 'BSEG-ZLSCH' lv_zlsch.  "ps_kismi_odeme-zlsch.
      PERFORM bdc_field       USING 'BDC_OKCODE' '=BU'.

    ENDIF.

    CALL TRANSACTION 'F-59' USING gt_bdcdata
             OPTIONS FROM ls_option
             MESSAGES INTO gt_messtab.
    LOOP AT gt_messtab WHERE msgtyp CA 'EAX'.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      COMMIT WORK AND WAIT.
      IF lv_cont IS INITIAL.
        PERFORM mass_change TABLES lt_return USING <fs_saticiya_kismi> 'ZLSPR' ''.
      ENDIF.

*      PERFORM mass_change TABLES lt_return USING <fs_devlete_odeme> 'ZLSPR'. "ödeme blokajı burada kalan tutar sıfırlanırsa blokajı kaldır.
*      PERFORM mass_change TABLES lt_return USING <fs_devlete_odeme> 'KISMI'. "ödeme blokajı burada kaldır.

*      ls_item = corresponding #( <fs_saticiya_kismi> ).
      MOVE-CORRESPONDING <fs_saticiya_kismi> TO ls_item.

      UNASSIGN: <fs_sum_alv>.
      READ TABLE gs_scr_1903-sum_alv ASSIGNING <fs_sum_alv> WITH KEY lifnr = <fs_saticiya_kismi>-lifnr.
      ls_item-kunnr = <fs_sum_alv>-kunnr.
      ls_item-uname = gs_scr_1903-auth-uname.
      ls_item-statu = gs_scr_1903-auth-statu.
      ls_item-mandt = sy-mandt.
      PERFORM domain_get_value USING '05' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING ls_item-icon_text .
      ls_item-onay_durum = '05'.
      ls_item-icon_name  = 'ICON_COMPLETE'.
      ls_item-datum      = sy-datum.
      ls_item-uzeit      = sy-uzeit.
      IF lv_cont EQ 'X'.
        ls_item-tam_odeme  = 'X'.
      ENDIF.
      APPEND ls_item TO pt_item.
      CLEAR: ls_item.

    ENDIF.

    DATA: ls_bapiret2 TYPE bapiret2.

    LOOP AT gt_messtab.
*      APPEND value bapiret2(
*        type          = gt_messtab-msgtyp
*        ID            = gt_messtab-msgid
*        NUMBER        = gt_messtab-msgnr
*        message       = gt_messtab-msgv1
*        message_v1    = gt_messtab-msgv1
*        message_v2    = gt_messtab-msgv2
*        message_v3    = gt_messtab-msgv3
*        message_v4    = gt_messtab-msgv4 ) TO lt_return.

      CLEAR: ls_bapiret2.
      ls_bapiret2-type       = gt_messtab-msgtyp.
      ls_bapiret2-id         = gt_messtab-msgid.
      ls_bapiret2-number     = gt_messtab-msgnr.
      ls_bapiret2-message    = gt_messtab-msgv1.
      ls_bapiret2-message_v1 = gt_messtab-msgv1.
      ls_bapiret2-message_v2 = gt_messtab-msgv2.
      ls_bapiret2-message_v3 = gt_messtab-msgv3.
      ls_bapiret2-message_v4 = gt_messtab-msgv4.
      APPEND ls_bapiret2 TO lt_return.

      IF gt_messtab-msgv1 CO '1234567890'.
        lv_kismi_belno = gt_messtab-msgv1.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input         = lv_kismi_belno
*         IMPORT>ING
           output        = lv_kismi_belno .

*        lv_kismi_belno = |{ lv_kismi_belno alpha = in  }|.

        DATA: BEGIN OF ls_odeme,
              bukrs      TYPE bsik-bukrs,
              lifnr      TYPE bsik-lifnr,
              satici_adi TYPE lfa1-name1,
              waers      TYPE bsik-waers,
              belnr      TYPE bsik-belnr,
              gjahr      TYPE bsik-gjahr,
              buzei      TYPE bsik-buzei,
              rebzg      TYPE bsik-rebzg,
              rebzj      TYPE bsik-rebzj,
              shkzg      TYPE bsik-shkzg,
              blart      TYPE bsik-blart,
              budat      TYPE bsik-budat,
              bldat      TYPE bsik-bldat,
              zfbdt      TYPE bsik-zfbdt,
              zbd1t      TYPE bsik-zbd1t,
              wrbtr      TYPE bsik-wrbtr,
              ktokk      TYPE lfa1-ktokk,
              END OF ls_odeme.

        DATA: ls_kismi TYPE zsog_fi_018_s_004.

        "belge numarası ile git kısmi ödeme belgesini al .
        CLEAR: ls_odeme, ls_kismi.
        SELECT SINGLE
                      b~bukrs
                      b~lifnr
                      l~name1 AS satici_adi
                      b~waers
                      b~belnr
                      b~gjahr
                      b~buzei
                      b~rebzg
                      b~rebzj
                      b~shkzg
                      b~blart
                      b~budat
                      b~bldat
                      b~zfbdt
                      b~zbd1t
                      b~wrbtr
                      l~ktokk
               INTO CORRESPONDING FIELDS OF ls_odeme
               FROM bsik AS b
               INNER JOIN lfa1 AS l ON b~lifnr = l~lifnr
               INNER JOIN lfb1 AS f ON b~bukrs = f~bukrs
                                   AND b~lifnr = f~lifnr
               WHERE b~bukrs = <fs_saticiya_kismi>-bukrs
                 AND b~lifnr = <fs_saticiya_kismi>-lifnr
                 AND b~belnr = lv_kismi_belno
                 AND b~gjahr = sy-datum+0(4).

*        data(ls_kismi) = corresponding zsog_fi_018_s_004( ls_odeme  ).
        MOVE-CORRESPONDING ls_odeme TO ls_kismi.
        ls_kismi-net_vade_tarihi = ls_kismi-zfbdt + ls_kismi-zbd1t.
        ls_kismi-laufd  = s_laufd-low.
        ls_kismi-laufi  = s_laufi-low.
        ls_kismi-satici_grubu = <fs_saticiya_kismi>-satici_grubu.

        "kismi ödeme belgesinin tutarı kadar vade tutarından çıkar.
        IF lv_cont EQ 'X'.
          ls_sum_alv-vadesi_gelen_tut = ls_sum_alv-vadesi_gelen_tut - <fs_saticiya_kismi>-ony_devlet_odeme - <fs_saticiya_kismi>-ony_satici_odeme..
        ELSE.
          ls_sum_alv-vadesi_gelen_tut = ls_sum_alv-vadesi_gelen_tut - lv_tutar.
        ENDIF.
*        pt_kismi = corresponding zsog_fi_018_t_06( ls_kismi ).
        MOVE-CORRESPONDING ls_kismi TO pt_kismi.
        pt_kismi-mandt = sy-mandt.
        APPEND pt_kismi.
        APPEND  ls_kismi TO ls_sum_alv-kismi_alv.
        APPEND  ls_kismi TO gs_scr_1903-kismi.
        MODIFY gs_scr_1903-sum_alv FROM ls_sum_alv.
        CLEAR: pt_kismi.
      ENDIF.
    ENDLOOP.


    CLEAR: ls_odeme, ls_kismi, lv_datum, lv_tutar, lv_zbd1t, lv_zlsch, gt_messtab, gt_messtab[], gt_bdcdata, gt_bdcdata[], ls_item, lv_cont, lv_kismi_belno,
           lv_kismi_toplam .
  ENDLOOP.

  IF lt_return IS NOT INITIAL.
    APPEND LINES OF lt_return TO pt_return.
  ENDIF.
ENDFORM.                    "vergisiz_kismi_odeme
*&---------------------------------------------------------------------*
*&      Form  vergili_kismi_odeme
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_SATICIYA_KISMI  text
*      -->LT_DEVLETE_KISMI   text
*      -->PT_RETURN          text
*      -->PT_ITEM            text
*      -->PT_KISMI           text
*----------------------------------------------------------------------*
FORM vergili_kismi_odeme TABLES lt_saticiya_kismi STRUCTURE zsog_fi_018_s_02
                                lt_devlete_kismi  STRUCTURE zsog_fi_018_s_02
                                pt_return         STRUCTURE bapiret2
                                pt_item           STRUCTURE zsog_fi_018_t_04
                                pt_kismi          STRUCTURE zsog_fi_018_t_06.

  DATA:  lt_return TYPE TABLE OF bapiret2.
  DATA : ls_option      TYPE ctu_params,
         lv_datum       TYPE char10,
         lv_tutar       TYPE char15,
         lv_zbd1t       TYPE char3,
         lv_zlsch       TYPE char1,
         lv_kismi_belno TYPE bsik-belnr.

  CLEAR: gt_bdcdata, gt_bdcdata[],
         gt_messtab, gt_messtab[].

  DATA: ls_item         TYPE zsog_fi_018_t_04,
        lv_kismi_toplam TYPE bseg-wrbtr,
        lv_cont(1).

  ls_option-dismode = p_mode.
  ls_option-updmode = 'S'.

  DATA: ls_sum_alv LIKE LINE OF gs_scr_1903-sum_alv.

  FIELD-SYMBOLS: <fs_saticiya_kismi> LIKE LINE OF lt_saticiya_kismi,
                 <fs_kismi>          LIKE LINE OF ls_sum_alv-kismi_alv,
                 <fs_sum_alv>        LIKE LINE OF gs_scr_1903-sum_alv.


  LOOP AT lt_saticiya_kismi ASSIGNING <fs_saticiya_kismi>.
*    data(ls_sum_alv) = gs_scr_1903-sum_alv[ lifnr  = <fs_saticiya_kismi>-lifnr ].
    CLEAR: ls_sum_alv.
    READ TABLE gs_scr_1903-sum_alv INTO ls_sum_alv WITH KEY lifnr  = <fs_saticiya_kismi>-lifnr.

    LOOP AT ls_sum_alv-kismi_alv ASSIGNING <fs_kismi> WHERE rebzg = <fs_saticiya_kismi>-belnr
                                                        AND rebzj = <fs_saticiya_kismi>-gjahr .

      lv_kismi_toplam = <fs_kismi>-wrbtr + lv_kismi_toplam.
    ENDLOOP.
    IF sy-subrc EQ 0. "önceden oluşturulmuş kısmi ödeme belgeesi vardır.

      lv_kismi_toplam = lv_kismi_toplam + <fs_saticiya_kismi>-ony_devlet_odeme + <fs_saticiya_kismi>-ony_satici_odeme.
      IF lv_kismi_toplam EQ <fs_saticiya_kismi>-belge_tutar.
        lv_cont = 'X'.
*         PERFORM mass_change TABLES lt_return USING <fs_saticiya_kismi> 'KISMI'."blokaj kaldır.
*        PERFORM mass_change TABLES lt_return USING <fs_saticiya_kismi> 'KISMI'."blokaj kaldır.
*        PERFORM mass_change TABLES lt_return USING <fs_saticiya_kismi> 'ZLSCH'."ödeme biçimi
        PERFORM mass_change_blokaj_payment TABLES lt_return USING <fs_saticiya_kismi> .

      ELSE.

        PERFORM bdc_dynpro      USING 'SAPMF05A' '0132'.
        PERFORM bdc_field       USING 'RF05A-BELNR' <fs_saticiya_kismi>-belnr.
        PERFORM bdc_field       USING 'RF05A-BUKRS' <fs_saticiya_kismi>-bukrs.
        PERFORM bdc_field       USING 'RF05A-GJAHR' <fs_saticiya_kismi>-gjahr.
        PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.

        PERFORM bdc_dynpro      USING 'SAPMF05A' '0615'.
        PERFORM bdc_field       USING 'BDC_CURSOR' 'RF05A-PSBET'.
        PERFORM bdc_field       USING 'BDC_OKCODE' '=GO'.

        PERFORM bdc_dynpro      USING 'SAPMF05A' '0133'.
        PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
        lv_datum = |{ sy-datum+6(2) }| && |.| && |{ sy-datum+4(2) }| && |.| && |{ sy-datum+0(4) }|.
        PERFORM bdc_field       USING 'BKPF-BLDAT' lv_datum.
        PERFORM bdc_field       USING 'BKPF-BLART' 'KS'.
        PERFORM bdc_field       USING 'BKPF-BUDAT' lv_datum.
        PERFORM bdc_field       USING 'BKPF-MONAT' sy-datum+4(2).
        CLEAR: lv_datum.
        lv_datum = |{ <fs_saticiya_kismi>-budat+6(2) }| && |.| && |{ <fs_saticiya_kismi>-budat+4(2) }| && |.| && |{ <fs_saticiya_kismi>-budat+0(4) }|.
        PERFORM bdc_field       USING 'BKPF-WWERT' lv_datum.

        PERFORM bdc_dynpro      USING 'SAPMF05A' '0306'.
        PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
        lv_tutar = <fs_saticiya_kismi>-ony_devlet_odeme + <fs_saticiya_kismi>-ony_satici_odeme.
        REPLACE FIRST OCCURRENCE OF '.' IN lv_tutar WITH ','.
        CONDENSE lv_tutar.
        PERFORM bdc_field       USING 'BSEG-WRBTR' lv_tutar.
        lv_zlsch = <fs_saticiya_kismi>-zlsch.
        PERFORM bdc_field       USING 'BSEG-ZLSCH' lv_zlsch.  "ps_kismi_odeme-zlsch.
        PERFORM bdc_field       USING 'BDC_OKCODE' '=BU'.
      ENDIF.

    ELSE.
      IF <fs_saticiya_kismi>-zlspr IS NOT INITIAL.
        PERFORM bdc_dynpro      USING 'SAPMF05A' '0132'.
        PERFORM bdc_field       USING 'RF05A-BELNR' <fs_saticiya_kismi>-belnr.
        PERFORM bdc_field       USING 'RF05A-BUKRS' <fs_saticiya_kismi>-bukrs.
        PERFORM bdc_field       USING 'RF05A-GJAHR' <fs_saticiya_kismi>-gjahr.
        PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.

        PERFORM bdc_dynpro      USING 'SAPMF05A' '0615'.
        PERFORM bdc_field       USING 'BDC_CURSOR' 'RF05A-PSBET'.
        PERFORM bdc_field       USING 'BDC_OKCODE' '=GO'.

        PERFORM bdc_dynpro      USING 'SAPMF05A' '0133'.
        PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
        lv_datum = |{ sy-datum+6(2) }| && |.| && |{ sy-datum+4(2) }| && |.| && |{ sy-datum+0(4) }|.
        PERFORM bdc_field       USING 'BKPF-BLDAT' lv_datum.
        PERFORM bdc_field       USING 'BKPF-BLART' 'KS'.
        PERFORM bdc_field       USING 'BKPF-BUDAT' lv_datum.
        PERFORM bdc_field       USING 'BKPF-MONAT' sy-datum+4(2).
        CLEAR: lv_datum.
        lv_datum = |{ <fs_saticiya_kismi>-budat+6(2) }| && |.| && |{ <fs_saticiya_kismi>-budat+4(2) }| && |.| && |{ <fs_saticiya_kismi>-budat+0(4) }|.
        PERFORM bdc_field       USING 'BKPF-WWERT' lv_datum.

        PERFORM bdc_dynpro      USING 'SAPMF05A' '0306'.
        PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
        lv_tutar = <fs_saticiya_kismi>-ony_devlet_odeme + <fs_saticiya_kismi>-ony_satici_odeme.
        REPLACE FIRST OCCURRENCE OF '.' IN lv_tutar WITH ','.
        CONDENSE lv_tutar.
        PERFORM bdc_field       USING 'BSEG-WRBTR' lv_tutar.
        lv_zlsch = <fs_saticiya_kismi>-zlsch.
        PERFORM bdc_field       USING 'BSEG-ZLSCH' lv_zlsch.  "ps_kismi_odeme-zlsch.
        PERFORM bdc_field       USING 'BDC_OKCODE' '=BU'.
      ELSE.
        PERFORM bdc_dynpro      USING 'SAPMF05A' '0132'.
        PERFORM bdc_field       USING 'RF05A-BELNR' <fs_saticiya_kismi>-belnr.
        PERFORM bdc_field       USING 'RF05A-BUKRS' <fs_saticiya_kismi>-bukrs.
        PERFORM bdc_field       USING 'RF05A-GJAHR' <fs_saticiya_kismi>-gjahr.
        PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
        PERFORM bdc_dynpro      USING 'SAPMF05A' '0133'.
        PERFORM bdc_field       USING 'BDC_CURSOR' 'BKPF-BLDAT'.
        lv_datum = |{ <fs_saticiya_kismi>-bldat+6(2) }| && |.| && |{ <fs_saticiya_kismi>-bldat+4(2) }| && |.| && |{ <fs_saticiya_kismi>-bldat+0(4) }|.
        PERFORM bdc_field       USING 'BKPF-BLDAT' lv_datum.
        PERFORM bdc_field       USING 'BKPF-BLART' 'KS'.
        CLEAR: lv_datum.
        lv_datum = |{ <fs_saticiya_kismi>-budat+6(2) }| && |.| && |{ <fs_saticiya_kismi>-budat+4(2) }| && |.| && |{ <fs_saticiya_kismi>-budat+0(4) }|.
        PERFORM bdc_field       USING 'BKPF-BUDAT' lv_datum.
        PERFORM bdc_dynpro      USING 'SAPMF05A' '0306'.
        PERFORM bdc_field       USING 'BDC_CURSOR' 'BSEG-ZLSCH'.
        PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
        lv_tutar = <fs_saticiya_kismi>-ony_satici_odeme + <fs_saticiya_kismi>-ony_devlet_odeme.
        REPLACE FIRST OCCURRENCE OF '.' IN lv_tutar WITH ','.
        CONDENSE lv_tutar.
        PERFORM bdc_field       USING 'BSEG-WRBTR' lv_tutar.
        PERFORM bdc_field       USING 'BSEG-ZTERM' ''.
        lv_zbd1t = <fs_saticiya_kismi>-zbd1t.
        CONDENSE lv_zbd1t.
        IF lv_zbd1t < 100 AND lv_zbd1t NE '0'.
          lv_zbd1t = |0| && |{ lv_zbd1t }|.
        ENDIF.
        CONDENSE lv_zbd1t.
        PERFORM bdc_field       USING 'BSEG-ZBD1T' lv_zbd1t.
        lv_zlsch = <fs_saticiya_kismi>-zlsch.
        PERFORM bdc_field       USING 'BSEG-ZLSCH' lv_zlsch.  "ps_kismi_odeme-zlsch.
        PERFORM bdc_field       USING 'BDC_OKCODE' '=BU'.
      ENDIF.
    ENDIF.

    CALL TRANSACTION 'F-59' USING gt_bdcdata
             OPTIONS FROM ls_option
             MESSAGES INTO gt_messtab.
    LOOP AT gt_messtab WHERE msgtyp CA 'EAX'.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      COMMIT WORK AND WAIT.
      IF lv_cont IS INITIAL.
        PERFORM mass_change TABLES lt_return USING <fs_saticiya_kismi> 'ZLSPR' ''.
      ENDIF.

*      PERFORM mass_change TABLES lt_return USING <fs_devlete_odeme> 'ZLSPR'. "ödeme blokajı burada kalan tutar sıfırlanırsa blokajı kaldır.
*      PERFORM mass_change TABLES lt_return USING <fs_devlete_odeme> 'KISMI'. "ödeme blokajı burada kaldır.

*      ls_item = corresponding #( <fs_saticiya_kismi> ).
      MOVE-CORRESPONDING <fs_saticiya_kismi> TO ls_item.
      UNASSIGN: <fs_sum_alv>.
      READ TABLE gs_scr_1903-sum_alv ASSIGNING  <fs_sum_alv> WITH KEY lifnr = <fs_saticiya_kismi>-lifnr.
      ls_item-kunnr = <fs_sum_alv>-kunnr.
      ls_item-uname = gs_scr_1903-auth-uname.
      ls_item-statu = gs_scr_1903-auth-statu.
      ls_item-mandt = sy-mandt.
      PERFORM domain_get_value USING '05' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING ls_item-icon_text .
      ls_item-onay_durum = '05'.
      ls_item-icon_name  = 'ICON_COMPLETE'.
      ls_item-datum      = sy-datum.
      ls_item-uzeit      = sy-uzeit.
      IF lv_cont EQ 'X'.
        ls_item-tam_odeme  = 'X'.
      ENDIF.
      APPEND ls_item TO pt_item.
      CLEAR: ls_item.

    ENDIF.

    DATA: ls_bapiret2 TYPE bapiret2.

    LOOP AT gt_messtab.
*      APPEND value bapiret2(
*        type          = gt_messtab-msgtyp
*        ID            = gt_messtab-msgid
*        NUMBER        = gt_messtab-msgnr
*        message       = gt_messtab-msgv1
*        message_v1    = gt_messtab-msgv1
*        message_v2    = gt_messtab-msgv2
*        message_v3    = gt_messtab-msgv3
*        message_v4    = gt_messtab-msgv4 ) TO lt_return.

      CLEAR: ls_bapiret2.
      ls_bapiret2-type       = gt_messtab-msgtyp.
      ls_bapiret2-id         = gt_messtab-msgid.
      ls_bapiret2-number     = gt_messtab-msgnr.
      ls_bapiret2-message    = gt_messtab-msgv1.
      ls_bapiret2-message_v1 = gt_messtab-msgv1.
      ls_bapiret2-message_v2 = gt_messtab-msgv2.
      ls_bapiret2-message_v3 = gt_messtab-msgv3.
      ls_bapiret2-message_v4 = gt_messtab-msgv4.
      APPEND ls_bapiret2 TO lt_return.

      IF gt_messtab-msgv1 CO '1234567890'.
        lv_kismi_belno = gt_messtab-msgv1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_kismi_belno
          IMPORTING
            output = lv_kismi_belno.

*        lv_kismi_belno = |{ lv_kismi_belno alpha = in  }|.

        DATA: BEGIN OF ls_odeme,
              bukrs      TYPE bsik-bukrs,
              lifnr      TYPE bsik-lifnr,
              satici_adi TYPE lfa1-name1,
              waers      TYPE bsik-waers,
              belnr      TYPE bsik-belnr,
              gjahr      TYPE bsik-gjahr,
              buzei      TYPE bsik-buzei,
              rebzg      TYPE bsik-rebzg,
              rebzj      TYPE bsik-rebzj,
              shkzg      TYPE bsik-shkzg,
              blart      TYPE bsik-blart,
              budat      TYPE bsik-budat,
              bldat      TYPE bsik-bldat,
              zfbdt      TYPE bsik-zfbdt,
              zbd1t      TYPE bsik-zbd1t,
              wrbtr      TYPE bsik-wrbtr,
              ktokk      TYPE lfa1-ktokk,
              END OF ls_odeme.

        DATA: ls_kismi TYPE zsog_fi_018_s_004.

        "belge numarası ile git kısmi ödeme belgesini al .
        SELECT SINGLE
                      b~bukrs
                      b~lifnr
                      l~name1 AS satici_adi
                      b~waers
                      b~belnr
                      b~gjahr
                      b~buzei
                      b~rebzg
                      b~rebzj
                      b~shkzg
                      b~blart
                      b~budat
                      b~bldat
                      b~zfbdt
                      b~zbd1t
                      b~wrbtr
                      l~ktokk
               INTO ls_odeme
               FROM bsik AS b
               INNER JOIN lfa1 AS l ON b~lifnr = l~lifnr
               INNER JOIN lfb1 AS f ON b~bukrs = f~bukrs
                                   AND b~lifnr = f~lifnr
               WHERE b~bukrs = <fs_saticiya_kismi>-bukrs
                 AND b~lifnr = <fs_saticiya_kismi>-lifnr
                 AND b~belnr = lv_kismi_belno
                 AND b~gjahr = sy-datum+0(4).

*        data(ls_kismi) = corresponding zsog_fi_018_s_004( ls_odeme  ).
        MOVE-CORRESPONDING ls_odeme TO ls_kismi.

        ls_kismi-net_vade_tarihi = ls_kismi-zfbdt + ls_kismi-zbd1t.
        ls_kismi-laufd  = s_laufd-low.
        ls_kismi-laufi  = s_laufi-low.
        ls_kismi-satici_grubu = <fs_saticiya_kismi>-satici_grubu.

        "kismi ödeme belgesinin tutarı kadar vade tutarından çıkar.
        IF lv_cont EQ 'X'.
          ls_sum_alv-vadesi_gelen_tut = ls_sum_alv-vadesi_gelen_tut - <fs_saticiya_kismi>-ony_devlet_odeme - <fs_saticiya_kismi>-ony_satici_odeme..
        ELSE.
          ls_sum_alv-vadesi_gelen_tut = ls_sum_alv-vadesi_gelen_tut - lv_tutar.
        ENDIF.
*        pt_kismi = corresponding zsog_fi_018_t_06( ls_kismi ).
        MOVE-CORRESPONDING ls_kismi TO pt_kismi.
        pt_kismi-mandt = sy-mandt.
        APPEND pt_kismi.
        APPEND  ls_kismi TO ls_sum_alv-kismi_alv.
        APPEND  ls_kismi TO gs_scr_1903-kismi.
        MODIFY gs_scr_1903-sum_alv FROM ls_sum_alv.
        CLEAR: pt_kismi.
      ENDIF.
    ENDLOOP.
    CLEAR: ls_kismi, lv_datum, lv_tutar, lv_zbd1t, lv_zlsch, gt_messtab, gt_messtab[], gt_bdcdata, gt_bdcdata[], ls_item, lv_cont, lv_kismi_belno,
           lv_kismi_toplam.
  ENDLOOP.

  IF lt_return IS NOT INITIAL.
    APPEND LINES OF lt_return TO pt_return.
  ENDIF.
ENDFORM.                    "vergili_kismi_odeme
*&---------------------------------------------------------------------*
*&      Form  send_mail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_mail .

  DATA: send_request       TYPE REF TO cl_bcs.
  DATA: text               TYPE bcsy_text.
  DATA: ls_text            TYPE soli.
  DATA: document           TYPE REF TO cl_document_bcs.
  DATA: sender             TYPE REF TO cl_sapuser_bcs.
  DATA: recipient          TYPE REF TO if_recipient_bcs.
  DATA: bcs_exception      TYPE REF TO cx_bcs.
  DATA: sent_to_all        TYPE os_boolean.

  DATA: lt_reciver TYPE TABLE OF zsog_fi_018_t_03.

  SELECT * FROM zsog_fi_018_t_03 INTO CORRESPONDING FIELDS OF TABLE lt_reciver
                                      WHERE statu = gs_scr_1903-onay_durum-onay_durum.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.
  CASE gs_scr_1903-onay_durum-onay_durum.
    WHEN '01'.


    WHEN '02'.
      ls_text-line =
              |{ gs_scr_1903-onay_durum-laufi }| && | tanıtıcı no'lu | &&
              |{ gs_scr_1903-onay_durum-laufd+6(2) }| && |.| && |{ gs_scr_1903-onay_durum-laufd+4(2) }| && |.| &&
              |{ gs_scr_1903-onay_durum-laufd+0(4) }| && | tarihli ödeme listesi hazırlanmıştır kontrol için ZPRD0001 işlem kodunu kullanınız.| .
      APPEND ls_text TO text.
    WHEN '03'.
      ls_text-line = |{ gs_scr_1903-onay_durum-laufi }| && | tanıtıcı no'lu | &&
       |{ gs_scr_1903-onay_durum-laufd+6(2) }| && |.| && |{ gs_scr_1903-onay_durum-laufd+4(2) }| && |.| &&
       |{ gs_scr_1903-onay_durum-laufd+0(4) }| && | tarihli ödeme listesi Kontrol ve Onayınıza sunulmuştur.|.
      APPEND ls_text TO text.
    WHEN '04'.
      ls_text-line = |{ gs_scr_1903-onay_durum-laufi }| && | tanıtıcı no'lu | &&
        |{ gs_scr_1903-onay_durum-laufd+6(2) }| && |.| && |{ gs_scr_1903-onay_durum-laufd+4(2) }| && |.| &&
        |{ gs_scr_1903-onay_durum-laufd+0(4) }| && | tarihli ödeme listesi listesi onayları tamamlanmış olup ödeme işlemine başlayabilirsiniz.|.
      APPEND ls_text TO text.
    WHEN OTHERS.
  ENDCASE.

  TRY.
*     -------- create persistent send request ------------------------
      send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document -------------------------------
*     create document from internal table with text
*      APPEND 'Hello world!' TO text.
      document = cl_document_bcs=>create_document(
                      i_type    = 'RAW'
                      i_text    = text
                      i_length  = '90'
                      i_subject = 'Otamatik Ödeme' ).

*     add document to send request
      CALL METHOD send_request->set_document( document ).

*     --------- set sender -------------------------------------------
*     note: this is necessary only if you want to set the sender
*           different from actual user (SY-UNAME). Otherwise sender is
*           set automatically with actual user.

      sender = cl_sapuser_bcs=>create( sy-uname ).
      CALL METHOD send_request->set_sender
        EXPORTING
          i_sender = sender.

*     --------- add recipient (e-mail address) -----------------------
*     create recipient - please replace e-mail address !!!

      DATA: ls_reciver LIKE LINE OF lt_reciver.
      LOOP AT lt_reciver INTO ls_reciver .
        recipient = cl_cam_address_bcs=>create_internet_address( ls_reciver-smtp_addr ).
*     add recipient with its respective attributes to send request
        CALL METHOD send_request->add_recipient
          EXPORTING
            i_recipient = recipient
            i_express   = 'X'.

      ENDLOOP.

*     ---------- send document ---------------------------------------
      CALL METHOD send_request->send(
        EXPORTING
          i_with_error_screen = 'X'
        RECEIVING
          result              = sent_to_all ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
* -----------------------------------------------------------
* *                     exception handling
* -----------------------------------------------------------
* * replace this very rudimentary exception handling
* * with your own one !!!
* -----------------------------------------------------------
    CATCH cx_bcs INTO bcs_exception.
*      WRITE: text-001.
*      WRITE: text-002, bcs_exception->error_type.
*      EXIT.

  ENDTRY.
ENDFORM.                    "send_mail
*&---------------------------------------------------------------------*
*&      Form  TERS_KAYIT
*&---------------------------------------------------------------------*
FORM ters_kayit .
  "1-Tam ödeme yapılmışsa, belgeden kalan sıfır ise belgenin ödeme biçimi ve blokajını kaldır.
  "2-Kısmi ödeme yapılmışsa kısmi ödeme belgesini iptal et.
  "3-Z'li tabloları sil tabloları sil.
  DATA: lt_return TYPE TABLE OF bapiret2.
  DATA: lv_subrc TYPE sy-subrc.
  DATA: lv_answer(1).
  PERFORM pop_up_confirm USING text-013 text-040  text-009 text-010 text-011
                               text-012 CHANGING lv_answer .
  IF lv_answer NE '1'.
    MESSAGE i004.
    RETURN.
  ENDIF.

  FIELD-SYMBOLS: <fs_sum_alv> LIKE LINE OF gs_scr_1903-sum_alv,
                 <fs_detail>  LIKE LINE OF <fs_sum_alv>-detail_alv,
                 <fs_kismi>   LIKE LINE OF <fs_sum_alv>-kismi_alv,
                 <fs_kismi_diger> LIKE LINE OF <fs_sum_alv>-kismi_diger.

  LOOP AT gs_scr_1903-sum_alv ASSIGNING <fs_sum_alv>.
    LOOP AT <fs_sum_alv>-detail_alv ASSIGNING <fs_detail>.
      "burada ilk çalıştırmada belge tutarı tamamlanmıştır. Tam ödeme gerçekleşmiştir.
      "Bu durumda belgeye ödeme biçimi 'H' olarak eklenmiştir. 'H' ödeme biçimini kaldır.
      IF <fs_detail>-belge_tutar = <fs_detail>-odenecek_tutar AND
         <fs_detail>-tam_odeme EQ 'X'.
*        PERFORM mass_change TABLES lt_return USING <fs_detail> 'REVRS'.

      ELSEIF <fs_sum_alv>-kismi_alv IS NOT INITIAL.
        "kısmi ödeme belgesinin ters kaydı alınıyor.
        LOOP AT <fs_sum_alv>-kismi_alv ASSIGNING <fs_kismi>.
*          PERFORM document_reverse TABLES lt_return
*                                    USING <fs_kismi>-bukrs
*                                          <fs_kismi>-belnr
*                                          <fs_kismi>-gjahr
*                                   CHANGING lv_subrc.
          IF lv_subrc IS INITIAL.
*            <fs_kismi>-shkzg = 'T'.
            "eğer bu belge ile ilgili başka bir kısmi belge varsa?
            LOOP AT <fs_sum_alv>-kismi_diger ASSIGNING <fs_kismi_diger>
                                             WHERE bukrs NE <fs_kismi>-bukrs
                                               AND belnr NE <fs_kismi>-belnr
                                               AND gjahr NE <fs_kismi>-gjahr.
              "eğer bu durum gerçekleşirse belgenin başka bir kısmi belgesi daha var anlamına gelir.
              "bu durumda orijinal belgeye blokaj koy ödeme biçimini kaldır.
              IF <fs_kismi_diger>-rebzg EQ <fs_kismi>-rebzg AND <fs_kismi_diger>-rebzj EQ <fs_kismi>-rebzj.
                IF <fs_detail>-tam_odeme EQ 'X'. "bu belge tamamlayan belgedir, blokajı silinmiştir blokaj ekle.

                ELSE. "bu belge öncesinde kismi belge oluşmuştur. Dolayısıyla blokaj ve ödeme biçimi kaldırımaya gerek yok.

                ENDIF.
              ELSE. "bu durumda belgenin başka kısmi ödemesi yoktur, blokajı ve ödeme biçimini kaldır.

              ENDIF.
            ENDLOOP.
            IF sy-subrc NE 0. "bu durumda belgenin başka kısmi ödemesi yoktur, blokajı ve ödeme biçimini kaldır.

            ENDIF.

          ENDIF.
          CLEAR: lv_subrc.
        ENDLOOP.
*        DELETE <fs_sum_alv>-kismi_alv WHERE shkzg EQ 'T'.
        "eğer kısmi ödeme belgesinin ters kaydı alınmışsa.
        IF sy-subrc EQ 0.
          "kismi ödeme belgesi orjinal belgeyi tamalayan belge mi?
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.


  gs_scr_1903-r_alv->refresh( ).
ENDFORM.                    "ters_kayit
*&---------------------------------------------------------------------*
*&      Form  document_reverse
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_RETURN  text
*      -->PV_BUKRS   text
*      -->PV_BELNR   text
*      -->PV_GJAHR   text
*      -->PV_SUBRC   text
*----------------------------------------------------------------------*
FORM document_reverse TABLES pt_return STRUCTURE bapiret2
                       USING pv_bukrs  TYPE bseg-bukrs
                             pv_belnr  TYPE bseg-belnr
                             pv_gjahr  TYPE bseg-gjahr
                      CHANGING pv_subrc TYPE sy-subrc.

  DATA: ls_return TYPE bapiret2.
  DATA: lv_budat TYPE bkpf-budat,
        lv_monat TYPE bkpf-monat,
        lv_xsofo TYPE xfeld.

  CALL FUNCTION 'CALL_FB08'
    EXPORTING
      i_bukrs      = pv_bukrs
      i_belnr      = pv_belnr
      i_gjahr      = pv_gjahr
      i_stgrd      = '01'
      i_update     = 'A'
      i_mode       = 'N'
    IMPORTING
      e_budat      = lv_budat
      e_monat      = lv_monat
      e_xsofo      = lv_xsofo
    EXCEPTIONS
      not_possible = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  "ozans
  IF lv_xsofo NE 'X'.
    ls_return-type         = 'E'.
    ls_return-id           = 'ZSOG_FI_018'.
    ls_return-number       = 022.
    CONCATENATE pv_belnr 'belgesinin ters kaydı alınamadı!' INTO  ls_return-message  SEPARATED BY space.
    ls_return-message_v1   = ls_return-message.
    APPEND ls_return TO pt_return.
    pv_subrc = 4.
  ELSE.
    ls_return-type         = 'S'.
    ls_return-id           = 'ZSOG_FI_018'.
    ls_return-number       = 023.
    CONCATENATE 'Ters kayıt alındı! Ters kayıt belge:'  pv_belnr INTO  ls_return-message  SEPARATED BY space.
    ls_return-message_v1   = ls_return-message.
    APPEND ls_return TO pt_return.
  ENDIF.
ENDFORM.                    "document_reverse
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM authority_check .

  DATA: BEGIN OF ls_bukrs,
        bukrs TYPE t001-bukrs,
        END OF ls_bukrs,
        lt_bukrs LIKE TABLE OF ls_bukrs.


  SELECT bukrs FROM t001 INTO TABLE lt_bukrs
                              WHERE bukrs IN s_bukrs.
  "xosahin
*  LOOP AT lt_bukrs INTO ls_bukrs.
*    AUTHORITY-CHECK OBJECT 'ZSO_BUKRS'
*      ID 'ACTVT' FIELD '03'
*      ID 'BUKRS' FIELD ls_bukrs-bukrs.
*    IF sy-subrc NE 0.
*      MESSAGE i000 WITH ls_bukrs-bukrs 'Mağazası için yetkiniz bulunmamaktadır!'.
*      STOP.
*    ENDIF.
*  ENDLOOP.


ENDFORM.                    "authority_check
*&---------------------------------------------------------------------*
*&      Form  TERS_KAYIT_2
*&---------------------------------------------------------------------*
FORM ters_kayit_2 .

  DATA: lv_answer(1) TYPE c.
*  DATA: ls_alv   TYPE zfi_s_satci_odem.
  DATA: lv_subrc TYPE sy-subrc.
  DATA: lt_return  TYPE TABLE OF bapiret2.
  DATA: lt_return2 TYPE TABLE OF bapiret2,
        ls_return  TYPE bapiret2.
  DATA: ls_kisi_odeme_alv TYPE zsog_fi_018_s_004.
  DATA: ls_odeme TYPE zsog_fi_018_s_02.

**********************************************************************
  DATA: lt_item TYPE TABLE OF zsog_fi_018_t_04,
        ls_item TYPE zsog_fi_018_t_04,
        lt_onay TYPE TABLE OF zsog_fi_018_t_01,
        ls_onay TYPE zsog_fi_018_t_01.

  DATA: ls_sum_alv     LIKE LINE OF gs_scr_1903-sum_alv,
        ls_detail      LIKE LINE OF ls_sum_alv-detail_alv,
        ls_kismi       LIKE LINE OF ls_sum_alv-kismi_alv,
        ls_kismi_diger LIKE LINE OF ls_sum_alv-kismi_diger.
  DATA: lv_flag TYPE c.

  PERFORM pop_up_confirm USING text-013 text-040  text-009 text-010 text-011
                               text-012 CHANGING lv_answer .
  IF lv_answer NE '1'.
    MESSAGE i004.
    RETURN.
  ENDIF.

  IF lt_return IS NOT INITIAL.
    PERFORM msg_display_error_table TABLES lt_return.
    RETURN.
  ENDIF.

  LOOP AT gs_scr_1903-sum_alv INTO ls_sum_alv.
    CLEAR lv_flag.
    LOOP AT ls_sum_alv-kismi_alv INTO ls_kismi.
      PERFORM document_reverse TABLES lt_return
                               USING  ls_kismi-bukrs
                                      ls_kismi-belnr
                                      ls_kismi-gjahr
                               CHANGING lv_subrc.
      IF lv_subrc IS INITIAL.
        CLEAR: ls_item, ls_onay.

        MOVE-CORRESPONDING ls_kismi TO ls_item.
        ls_item-mandt = sy-mandt.
        APPEND ls_item TO lt_item.

        MOVE-CORRESPONDING ls_kismi TO ls_onay.
        ls_onay-mandt = sy-mandt.
        APPEND ls_onay TO lt_onay.
      ENDIF.
    ENDLOOP.
*    IF sy-subrc EQ 0.
*      lv_flag = abap_true.
*    ENDIF.
    SORT ls_sum_alv-kismi_alv BY bukrs rebzg rebzj.

    LOOP AT ls_sum_alv-detail_alv INTO ls_detail.
**********************************************************************
      CLEAR lv_flag.
      READ TABLE ls_sum_alv-kismi_alv TRANSPORTING NO FIELDS
                                      WITH KEY bukrs = ls_detail-bukrs
                                               rebzg = ls_detail-belnr
                                               rebzj = ls_detail-gjahr
                                      BINARY SEARCH.
      IF sy-subrc = 0.
        lv_flag = abap_true.
      ENDIF.
**********************************************************************

      IF lv_flag = abap_true.

        MOVE-CORRESPONDING ls_detail TO ls_odeme.
        PERFORM mass_change TABLES lt_return
                       USING ls_odeme 'KISMI'
                       CHANGING  lv_subrc .
        IF lv_subrc EQ 0.
          CLEAR: ls_item, ls_onay.
          CLEAR: ls_detail-zlspr.
          MOVE-CORRESPONDING ls_detail TO ls_item.
          ls_item-mandt = sy-mandt.
          APPEND ls_item TO lt_item.

          MOVE-CORRESPONDING ls_detail TO ls_onay.
          ls_onay-mandt = sy-mandt.
          APPEND ls_onay TO lt_onay.

        ENDIF.
*        ls_detail-wrbtr       = ls_detail-belge_tutar."?
*        CLEAR: ls_detail-kismi_odeme."?
        ls_detail-odenecek_tutar = ls_detail-belge_tutar.
        WRITE '' AS ICON TO ls_detail-kismi_odeme_icon.
        MODIFY ls_sum_alv-detail_alv FROM ls_detail.
        CLEAR: ls_odeme.
*        IF ls_detail-tam_odeme_bicimi IS NOT INITIAL.
        MOVE-CORRESPONDING ls_detail TO ls_odeme.
        PERFORM mass_change TABLES lt_return
                             USING ls_odeme 'REVRS'
                             CHANGING  lv_subrc .
        IF lv_subrc EQ 0.
          CLEAR: ls_detail-tam_odeme_bicimi.
          MODIFY ls_sum_alv-detail_alv FROM ls_detail.
        ENDIF.
        CLEAR: ls_odeme.
*        ENDIF.
      ELSE.

*        IF ls_detail-tam_odeme_bicimi IS NOT INITIAL.
        MOVE-CORRESPONDING ls_detail TO ls_odeme.
        PERFORM mass_change TABLES lt_return
                             USING ls_odeme 'REVRS'
                             CHANGING  lv_subrc .
        IF lv_subrc EQ 0.
          CLEAR: ls_item, ls_onay , ls_detail-tam_odeme_bicimi.

          ls_item-mandt = sy-mandt.
          MOVE-CORRESPONDING ls_detail TO ls_item.
          APPEND ls_item TO lt_item.

          ls_onay-mandt = sy-mandt.
          MOVE-CORRESPONDING ls_detail TO ls_onay.
          APPEND ls_onay TO lt_onay.

          MODIFY ls_sum_alv-detail_alv FROM ls_detail.
        ENDIF.
        CLEAR: ls_odeme.
*        ENDIF
        .
      ENDIF.
    ENDLOOP.
    CLEAR: lv_subrc.
  ENDLOOP.
**********************************************************************
*  LOOP AT gs_scr_1903-sum_alv INTO ls_sum_alv.
*    CLEAR: lv_flag, lv_subrc.
*    LOOP AT ls_sum_alv-kismi_alv INTO ls_kismi.
*      CLEAR: lv_subrc.
*      PERFORM document_reverse TABLES lt_return
*                               USING  ls_kismi-bukrs
*                                      ls_kismi-belnr
*                                      ls_kismi-gjahr
*                               CHANGING lv_subrc.
*      IF lv_subrc IS INITIAL.
*        CLEAR: ls_item, ls_onay.
*
*        MOVE-CORRESPONDING ls_kismi TO ls_item.
*        ls_item-mandt = sy-mandt.
*        APPEND ls_item TO lt_item.
*
*        MOVE-CORRESPONDING ls_kismi TO ls_onay.
*        ls_onay-mandt = sy-mandt.
*        APPEND ls_onay TO lt_onay.
*
*
*        lv_flag = abap_true.
**        CLEAR ls_odeme.
**        MOVE-CORRESPONDING ls_kismi TO ls_odeme.
**        PERFORM mass_change TABLES lt_return
**                             USING ls_odeme 'KISMI'
**                          CHANGING lv_subrc .
*      ENDIF.
*
*    ENDLOOP.
**    SORT ls_sum_alv-kismi_alv BY bukrs belnr gjahr.
*    LOOP AT ls_sum_alv-detail_alv INTO ls_detail.
**      READ TABLE ls_sum_alv-kismi_alv TRANSPORTING NO FIELDS
**                                      WITH KEY bukrs = ls_detail-bukrs
**                                               belnr = ls_detail-belnr
**                                               gjahr = ls_detail-gjahr
**                                      BINARY SEARCH.
**      IF sy-subrc EQ 0.
*      IF lv_flag = abap_true.
*        MOVE-CORRESPONDING ls_detail TO ls_odeme.
*        CLEAR: lv_subrc.
*        PERFORM mass_change TABLES lt_return
*                       USING ls_odeme 'KISMI'
*                       CHANGING  lv_subrc .
*        IF lv_subrc EQ 0.
*          CLEAR: ls_item, ls_onay.
*          CLEAR: ls_detail-zlspr.
*          MOVE-CORRESPONDING ls_detail TO ls_item.
*          ls_item-mandt = sy-mandt.
*          APPEND ls_item TO lt_item.
*
*          MOVE-CORRESPONDING ls_detail TO ls_onay.
*          ls_onay-mandt = sy-mandt.
*          APPEND ls_onay TO lt_onay.
*
*        ENDIF.
**        ls_detail-wrbtr       = ls_detail-belge_tutar."?
**        CLEAR: ls_detail-kismi_odeme."?
*        ls_detail-odenecek_tutar = ls_detail-belge_tutar.
*        WRITE '' AS ICON TO ls_detail-kismi_odeme_icon.
*        MODIFY ls_sum_alv-detail_alv FROM ls_detail.
*        CLEAR: ls_odeme.
*
*        IF ls_detail-tam_odeme_bicimi IS NOT INITIAL.
*          CLEAR: lv_subrc.
*          MOVE-CORRESPONDING ls_detail TO ls_odeme.
*          PERFORM mass_change TABLES lt_return
*                               USING ls_odeme 'REVRS'
*                               CHANGING  lv_subrc .
*          IF lv_subrc EQ 0.
*            CLEAR: ls_detail-tam_odeme_bicimi.
*            MODIFY ls_sum_alv-detail_alv FROM ls_detail.
*          ENDIF.
*          CLEAR: ls_odeme.
*        ENDIF.
*      ELSE.
*
**        IF ls_detail-tam_odeme_bicimi IS NOT INITIAL.
*        MOVE-CORRESPONDING ls_detail TO ls_odeme.
*        CLEAR: lv_subrc.
*        PERFORM mass_change TABLES lt_return
*                             USING ls_odeme 'REVRS'
*                             CHANGING  lv_subrc .
*        IF lv_subrc EQ 0.
*          CLEAR: ls_item, ls_onay , ls_detail-tam_odeme_bicimi.
*
*          ls_item-mandt = sy-mandt.
*          MOVE-CORRESPONDING ls_detail TO ls_item.
*          APPEND ls_item TO lt_item.
*
*          ls_onay-mandt = sy-mandt.
*          MOVE-CORRESPONDING ls_detail TO ls_onay.
*          APPEND ls_onay TO lt_onay.
*
*          MODIFY ls_sum_alv-detail_alv FROM ls_detail.
*        ENDIF.
*        CLEAR: ls_odeme.
**        ENDIF.
*      ENDIF.
*    ENDLOOP.
*    CLEAR: lv_subrc.
*  ENDLOOP.

**********************************************************************
*  IF p_r1 IS NOT INITIAL.
*    ls_alv-secim = ''.
*    MODIFY gs_scr-1903-alv FROM ls_alv TRANSPORTING secim WHERE secim = 'X'.
*  ELSEIF p_r2 IS NOT INITIAL.
*    DELETE gs_scr-1903-alv  WHERE secim = 'X'.
*  ENDIF.

  SORT lt_item BY laufd laufi bukrs lifnr belnr gjahr buzei.
  SORT lt_onay BY laufd laufi bukrs lifnr.

  DELETE ADJACENT DUPLICATES FROM lt_item
                   COMPARING laufd laufi bukrs lifnr belnr gjahr buzei.
  DELETE ADJACENT DUPLICATES FROM lt_onay
                   COMPARING laufd laufi bukrs lifnr.

  IF lt_item IS NOT INITIAL.
    DELETE zsog_fi_018_t_04  FROM TABLE lt_item.
    COMMIT WORK AND WAIT.
  ENDIF.
  IF lt_onay IS NOT INITIAL.
    DELETE zsog_fi_018_t_01 FROM TABLE lt_onay.
    COMMIT WORK AND WAIT.
  ENDIF.

  IF lt_return IS NOT INITIAL.
    PERFORM msg_display_error_table TABLES lt_return.
  ENDIF.

ENDFORM.                    " TERS_KAYIT_2
*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_DETAIL_4_KISMI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_modify_detail_4_kismi TABLES kismi  STRUCTURE zsog_fi_018_s_004
                                    detail STRUCTURE zsog_fi_018_s_02.
  FIELD-SYMBOLS <detail> TYPE zsog_fi_018_s_02.
  FIELD-SYMBOLS <kismi>  TYPE zsog_fi_018_s_004.

  DATA: lv_tutar TYPE bseg-wrbtr.
  DATA: lv_dmbtr TYPE bseg-dmbtr.

  DATA: BEGIN OF ls_bseg,
          bukrs  LIKE bseg-bukrs ,
          belnr  LIKE bseg-belnr ,
          gjahr  LIKE bseg-gjahr ,
          buzei  LIKE bseg-buzei ,
          rebzg  LIKE bseg-rebzg ,
          rebzj  LIKE bseg-rebzj ,
          rebzz  LIKE bseg-rebzz ,
          dmbtr  LIKE bseg-dmbtr ,
          augbl  LIKE bseg-augbl ,
        END OF ls_bseg,
        lt_bseg LIKE TABLE OF ls_bseg.

  CHECK detail[] IS NOT INITIAL.

  SELECT bukrs belnr gjahr buzei
         rebzg rebzj rebzz dmbtr augbl
    FROM bseg
    INTO TABLE lt_bseg
    FOR ALL ENTRIES IN detail
    WHERE bukrs = detail-bukrs
      AND belnr = detail-belnr
      AND gjahr = detail-gjahr
      AND buzei = detail-buzei.

  SORT lt_bseg BY bukrs belnr gjahr buzei.

  LOOP AT detail ASSIGNING <detail>.
    CLEAR lv_tutar.
    LOOP AT kismi ASSIGNING <kismi> WHERE rebzg = <detail>-belnr
                                      AND rebzj = <detail>-gjahr .
      lv_tutar = lv_tutar + <kismi>-wrbtr.
    ENDLOOP.
    <detail>-kalan_odeme =  <detail>-belge_tutar - lv_tutar.

    CLEAR lv_dmbtr.
    LOOP AT lt_bseg INTO ls_bseg  WHERE bukrs = <detail>-bukrs
                                    AND rebzg = <detail>-belnr
                                    AND rebzj = <detail>-gjahr
                                    AND rebzz = <detail>-buzei
                                    AND rebzg <> space
                                    AND augbl = space.
      lv_dmbtr = lv_dmbtr + ls_bseg-dmbtr.
    ENDLOOP.
    IF sy-subrc = 0.
      <detail>-kalan_odeme = <detail>-kalan_odeme - lv_dmbtr.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " F_MODIFY_DETAIL_4_KISMI
*&---------------------------------------------------------------------*
*&      Form  F_FILL_ITEM_4_NULL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_NULL  text
*      -->P_LT_ITEM  text
*----------------------------------------------------------------------*
FORM f_fill_item_4_null  TABLES it_null STRUCTURE zsog_fi_018_s_02
                                et_item STRUCTURE zsog_fi_018_t_04.

  DATA ls_item TYPE zsog_fi_018_t_04.
  FIELD-SYMBOLS <it_null> TYPE zsog_fi_018_s_02.
  FIELD-SYMBOLS <fs_sum_alv>  LIKE LINE OF gs_scr_1903-sum_alv.

  CHECK it_null[] IS NOT INITIAL.

  LOOP AT it_null ASSIGNING <it_null>.
    MOVE-CORRESPONDING <it_null> TO ls_item.

    UNASSIGN <fs_sum_alv>.
    READ TABLE gs_scr_1903-sum_alv ASSIGNING <fs_sum_alv> WITH KEY lifnr = <it_null>-lifnr.
    ls_item-kunnr = <fs_sum_alv>-kunnr.
    ls_item-uname = gs_scr_1903-auth-uname.
    ls_item-statu = gs_scr_1903-auth-statu.
    ls_item-mandt = sy-mandt.
    PERFORM domain_get_value USING '05' 'ZSOG_FI_018_ONAY_DURUM_DM' CHANGING ls_item-icon_text .
    ls_item-onay_durum = '05'.
    ls_item-icon_name  = 'ICON_COMPLETE'.
    ls_item-datum      = sy-datum.
    ls_item-uzeit      = sy-uzeit.
    APPEND ls_item TO et_item.
    CLEAR: ls_item.
  ENDLOOP.
ENDFORM.                    " F_FILL_ITEM_4_NULL
*&---------------------------------------------------------------------*
*&      Form  ODEME_LISTESI
*&---------------------------------------------------------------------*
FORM odeme_listesi .
***** ----> Form Data
*  DATA: lv_formname TYPE tdsfname.
*  DATA: lv_func(30) TYPE c. "fonksiyon adı
*  DATA: ls_control_param   TYPE ssfctrlop,
*        ls_output_param    TYPE ssfcompop,
*        job_output_info    TYPE ssfcrescl,
*        job_output_options TYPE ssfcresop,
*        lt_otf             TYPE itcoo OCCURS 0 WITH HEADER LINE,
*        lv_tddest          TYPE rspopname,
*        pdf_tab            LIKE tline OCCURS 0 WITH HEADER LINE,
*        lv_bin_file        TYPE xstring,
*        lv_bin_filesize    TYPE i,
*        lt_binary_content  TYPE solix_tab.
****** <---- Form Data
*  DATA: lv_hata   TYPE c.
*  DATA: ls_header TYPE zsog_fi_018_s_005.
*  DATA: ls_table  TYPE zsog_fi_018_s_006.
*  DATA: lt_table  TYPE TABLE OF zsog_fi_018_s_006.
*  DATA: ls_footer TYPE zsog_fi_018_s_007.
*
*  DATA: lv_file_name TYPE string,
*        lv_file_path TYPE string,
*        lv_full_path TYPE string.
*
*  DATA: lv_hbkid  TYPE t012-hbkid.
*  DATA: ls_detail LIKE LINE OF gs_scr_1903-detail.
*  DATA: ls_sum    LIKE LINE OF gs_scr_1903-sum_alv.
*
*  DATA: BEGIN OF ls_temp,
*         lifnr TYPE lfa1-lifnr,
*         banka TYPE bnka-banka,
*         brnch TYPE bnka-brnch,
*         bankn TYPE lfbk-bankn,
*         name1 TYPE lfa1-name1,
*       END OF ls_temp,
*       lt_temp LIKE TABLE OF ls_temp.
***********************************************************************
*  READ TABLE gs_scr_1903-detail INTO ls_detail INDEX 1.
*  lv_hbkid = ls_detail-hbkid.
*
*  SELECT SINGLE
*    b~banka
*    b~brnch
*    d~iban
*    c~waers
*  FROM t012 AS a
*    INNER JOIN bnka  AS b  ON b~bankl = a~bankl
*                          AND b~banks = a~banks
*    INNER JOIN t012k AS c  ON c~hbkid = a~hbkid
*                          AND c~bukrs = a~bukrs
*    INNER JOIN tiban AS d  ON d~bankn = c~bankn
*  INTO ls_header
*  WHERE a~hbkid = lv_hbkid
*    AND a~bukrs = s_bukrs-low.
*
*  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
*    EXPORTING
*      input  = s_laufd-low
*    IMPORTING
*      output = ls_header-tanitici_tarih.
*
**    ls_header-tanitici_tarih  = s_laufd-low .
*
*  IF gs_scr_1903-sum_alv IS NOT INITIAL.
*    SELECT lf~lifnr bn~banka bn~brnch lf~bankn l1~name1
*      FROM lfbk AS lf
*      INNER JOIN bnka AS bn ON bn~bankl = lf~bankl
*                           AND bn~banks = lf~banks
*      INNER JOIN lfa1 AS l1 ON l1~lifnr = lf~lifnr
*      INTO TABLE lt_temp
*      FOR ALL ENTRIES IN gs_scr_1903-sum_alv
*      WHERE lf~lifnr = gs_scr_1903-sum_alv-lifnr.
*
*    SORT lt_temp BY lifnr.
*  ENDIF.
*
*  CLEAR ls_footer.
*  LOOP AT gs_scr_1903-sum_alv INTO ls_sum.
*    CLEAR: ls_table, ls_temp.
*    ls_table-lifnr = ls_sum-lifnr.
*    ls_table-tutar = ls_sum-kdvli_tut.
*    ls_table-tutar = abs( ls_table-tutar ).
*    ls_footer-tutar = ls_footer-tutar + ls_table-tutar.
*    READ TABLE lt_temp INTO ls_temp WITH KEY lifnr = ls_sum-lifnr
*                                    BINARY SEARCH.
*    IF sy-subrc = 0.
*      ls_table-banka = ls_temp-banka.
*      ls_table-brnch = ls_temp-brnch.
*      ls_table-bankn = ls_temp-bankn.
*      ls_table-name1 = ls_temp-name1.
*    ENDIF.
*
*    APPEND ls_table TO lt_table.
*
*  ENDLOOP.
*  ls_header-tutar = ls_footer-tutar.
*
*  lv_formname = 'ZFI_SF_ODM_LISTE'.
*
*  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*    EXPORTING
*      formname           = lv_formname
**     VARIANT            = ' '
**     DIRECT_CALL        = ' '
*    IMPORTING
*      fm_name            = lv_func
*    EXCEPTIONS
*      no_form            = 1
*      no_function_module = 2
*      OTHERS             = 3.
*  IF sy-subrc <> 0.
*    lv_hata = 'X'.
**    ent_retco = 1.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  ls_control_param-preview   = 'X'.
*  ls_control_param-no_dialog = 'X'.
*  ls_control_param-getotf    = 'X'.
*  ls_control_param-langu     = sy-langu.
*
*  ls_output_param-tdimmed    = 'X'.
*  ls_output_param-tddest     = 'LP01'.
*
*  CALL FUNCTION lv_func
*    EXPORTING
**     ARCHIVE_INDEX        =
**     ARCHIVE_INDEX_TAB    =
**     ARCHIVE_PARAMETERS   =
*      control_parameters   = ls_control_param
**     MAIL_APPL_OBJ        =
**     MAIL_RECIPIENT       =
**     MAIL_SENDER          =
*      output_options       = ls_output_param
*      user_settings        = abap_false
*      gs_header            = ls_header
*      gt_table             = lt_table
*      gs_footer            = ls_footer
*    IMPORTING
**     DOCUMENT_OUTPUT_INFO =
*      job_output_info      = job_output_info
*      job_output_options   = job_output_options
*    EXCEPTIONS
*      formatting_error     = 1
*      internal_error       = 2
*      send_error           = 3
*      user_canceled        = 4
*      OTHERS               = 5.
*  IF sy-subrc <> 0.
**    ent_retco = 2.
*    lv_hata = 'X'.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*
*  CHECK job_output_info-otfdata[] IS NOT INITIAL.
*  lt_otf[] = job_output_info-otfdata[].
*
*  CALL FUNCTION 'CONVERT_OTF'
*    EXPORTING
*      format                = 'PDF'
**     format                = 'BIN'
**     format                = 'DOCX'
**     max_linewidth         = 132
*    IMPORTING
*      bin_file              = lv_bin_file
*      bin_filesize          = lv_bin_filesize
*    TABLES
*      otf                   = lt_otf[]
*      lines                 = pdf_tab[]
*    EXCEPTIONS
*      err_max_linewidth     = 1
*      err_format            = 2
*      err_conv_not_possible = 3
*      err_bad_otf           = 4
*      OTHERS                = 5.
*
*  lt_binary_content[] = cl_document_bcs=>xstring_to_solix( lv_bin_file ).
**
*
*  CHECK lv_hata IS INITIAL.
*
*  CALL METHOD cl_gui_frontend_services=>file_save_dialog
*    EXPORTING
**     WINDOW_TITLE         =
*      default_extension    = 'PDF'
**     DEFAULT_FILE_NAME    =
**     FILE_FILTER          =
**     WITH_ENCODING        = 'X'
**     PROMPT_ON_OVERWRITE  = 'X'
**     INITIAL_DIRECTORY    =
*    CHANGING
*      filename             = lv_file_name
*      path                 = lv_file_path
*      fullpath             = lv_full_path
**     USER_ACTION          =
**     FILE_ENCODING        =
*    EXCEPTIONS
*      cntl_error           = 1
*      error_no_gui         = 2
*      not_supported_by_gui = 3
*      OTHERS               = 4.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
**************downloading the converted PDF data to your local PC********
*
*  CALL FUNCTION 'GUI_DOWNLOAD'
*    EXPORTING
*      bin_filesize            = lv_bin_filesize
*      filename                = lv_full_path
*      filetype                = 'BIN'
**     filetype                = 'ASC'
*    TABLES
*      data_tab                = lt_binary_content
*    EXCEPTIONS
*      file_write_error        = 1
*      no_batch                = 2
*      gui_refuse_filetransfer = 3
*      invalid_type            = 4
*      no_authority            = 5
*      unknown_error           = 6
*      header_not_allowed      = 7
*      separator_not_allowed   = 8
*      filesize_not_allowed    = 9
*      header_too_long         = 10
*      dp_error_create         = 11
*      dp_error_send           = 12
*      dp_error_write          = 13
*      unknown_dp_error        = 14
*      access_denied           = 15
*      dp_out_of_memory        = 16
*      disk_full               = 17
*      dp_timeout              = 18
*      file_not_found          = 19
*      dataprovider_exception  = 20
*      control_flush_error     = 21
*      OTHERS                  = 22.

*  DATA: lvi_file(70) TYPE c.
*  lvi_file = lv_full_path.
*  CALL FUNCTION 'EXECUTE_WINWORD'
*    EXPORTING
*      i_file = lvi_file.
ENDFORM.  " ODEME_LISTESI
*&---------------------------------------------------------------------*
*&      Form  SATICI_SATIR_SIL
*&---------------------------------------------------------------------*
FORM satici_satir_sil .
  DATA: lt_rows       TYPE lvc_t_row.
  DATA: ls_rows       LIKE LINE OF lt_rows.
  DATA: lt_filter     LIKE gs_scr_1903-filter.
  DATA: ls_filter     LIKE LINE OF gs_scr_1903-filter.
  DATA: ls_sum        LIKE LINE OF gs_scr_1903-sum_alv.
  DATA: ls_sum_temp   LIKE LINE OF gs_scr_1903-sum_alv.
  DATA: ls_sum_detail LIKE LINE OF ls_sum-detail_alv.
  DATA: lt_detail     LIKE gs_scr_1903-detail.
  DATA: ls_detail     LIKE LINE OF lt_detail.
  DATA: lv_answer     TYPE c.
  DATA: lv_question   TYPE char50.
  DATA: lv_lifnr      TYPE lfa1-lifnr.
  CALL METHOD gs_scr_1903-r_grid1->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  "satıcıyı almak için
  lt_filter = gs_scr_1903-filter.
  READ TABLE lt_filter INDEX 1 INTO ls_filter.
  lv_lifnr = ls_filter-lifnr.
  IF lt_rows IS NOT INITIAL.
    lv_question = text-042.
    PERFORM you_sure USING lv_question CHANGING lv_answer.
    IF lv_answer <> '1'.
      EXIT.
    ENDIF.


    READ TABLE gs_scr_1903-sum_alv INTO ls_sum WITH KEY lifnr = lv_lifnr.
    LOOP AT lt_rows INTO ls_rows.
      CLEAR ls_filter.
      READ TABLE gs_scr_1903-filter INTO ls_filter INDEX ls_rows-index.
      IF sy-subrc = 0.
        DELETE gs_scr_1903-detail WHERE lifnr = ls_filter-lifnr
                                    AND laufd = ls_filter-laufd
                                    AND laufi = ls_filter-laufi
                                    AND belnr = ls_filter-belnr.

        DELETE gs_scr_1903-filter WHERE lifnr = ls_filter-lifnr
                                    AND laufd = ls_filter-laufd
                                    AND laufi = ls_filter-laufi
                                    AND belnr = ls_filter-belnr.

*       READ TABLE ls_sum-detail_alv INTO ls_detail
        DELETE ls_sum-detail_alv  WHERE lifnr = ls_filter-lifnr
                                    AND laufd = ls_filter-laufd
                                    AND laufi = ls_filter-laufi
                                    AND belnr = ls_filter-belnr.
      ENDIF.
    ENDLOOP.
    LOOP AT gs_scr_1903-sum_alv INTO ls_sum_temp WHERE lifnr = lv_lifnr.
      ls_sum_temp = ls_sum.
      MODIFY gs_scr_1903-sum_alv FROM ls_sum TRANSPORTING detail_alv.
    ENDLOOP.
*    PERFORM refres
  ENDIF.
ENDFORM.                    " SATICI_SATIR_SIL
*&---------------------------------------------------------------------*
*&      Form  YOU_SURE
*&---------------------------------------------------------------------*
FORM you_sure  USING    ev_question
               CHANGING ev_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = ev_question
      default_button        = '2'
      display_cancel_button = space
    IMPORTING
      answer                = ev_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.


ENDFORM.                    " YOU_SURE
*&---------------------------------------------------------------------*
*&      Form  HEPSINI_DAGIT
*&---------------------------------------------------------------------*
FORM hepsini_dagit .
  DATA: ls_sum        LIKE LINE OF gs_scr_1903-sum_alv.
  DATA: lv_oneri      LIKE p_oneri.
  DATA: lv_index      TYPE i.
  DATA: lt_tab        TYPE esp1_message_tab_type.

  DATA: lo_selections TYPE REF TO cl_salv_selections.
  DATA lt_rows TYPE salv_t_row.
  DATA ls_row  LIKE LINE OF  lt_rows.
  lo_selections = gs_scr_1903-r_alv->get_selections( ).
  lt_rows = lo_selections->get_selected_rows( ).

  CHECK gs_scr_1903-auth-statu = '2'.

  IF lt_rows[] IS  INITIAL.
    MESSAGE s032 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF gs_dagit-flag IS INITIAL .
    gs_dagit-flag  = abap_true.
    gs_dagit-oneri = p_oneri.
  ENDIF.

  lv_oneri = gs_dagit-oneri.
  LOOP AT lt_rows INTO ls_row.
*    LOOP AT gs_scr_1903-sum_alv INTO ls_sum.
    CLEAR ls_sum.
    READ TABLE gs_scr_1903-sum_alv INTO ls_sum INDEX ls_row.
    IF sy-subrc = 0.
      PERFORM filter_detail_alv USING ls_row '' 'X'.
      IF lv_oneri > gs_scr_1903-category-toplam_tutar.
        gs_scr_1903-category-odenecek_tutar = gs_scr_1903-category-toplam_tutar.
        lv_oneri = lv_oneri - gs_scr_1903-category-odenecek_tutar.
      ELSE.
        gs_scr_1903-category-odenecek_tutar = lv_oneri.
        CLEAR lv_oneri.
      ENDIF.

      PERFORM category_calculate.

      PERFORM category_approve USING 'X' CHANGING lt_tab.

      IF lv_oneri IS INITIAL.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF lt_tab IS NOT INITIAL.
    PERFORM c14z_messages_show_as_popup TABLES lt_tab.
  ENDIF.
ENDFORM.                    " HEPSINI_DAGIT
