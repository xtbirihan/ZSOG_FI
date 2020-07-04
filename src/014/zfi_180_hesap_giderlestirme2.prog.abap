*&---------------------------------------------------------------------*
*& Report  ZFI_180_HESAP_GIDERLESTIRME2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZFI_180_HESAP_GIDERLESTIRME2.

INCLUDE zalv_global .

TABLES : zfi_pes_gid_bas , zfi_pes_gid_kal .

DATA : BEGIN OF it_report OCCURS 0 ,
        bukrs             LIKE zfi_pes_gid_bas-bukrs ,
        spmon             LIKE zfi_pes_gid_kal-spmon ,
        belgeno           LIKE zfi_pes_gid_bas-belgeno ,
        policeno          LIKE zfi_pes_gid_bas-policeno ,
        sgtxt             LIKE zfi_pes_gid_bas-sgtxt ,
        taksitno          LIKE zfi_pes_gid_kal-taksitno ,
        hkont             LIKE zfi_pes_gid_bas-hkont ,
        kisa_txt          LIKE skat-txt20 ,
        hkont_uzun        LIKE zfi_pes_gid_bas-hkont_uzun ,
        uzun_txt          LIKE skat-txt20 ,
        hkont_gider       LIKE zfi_pes_gid_bas-hkont_gider ,
        gider_txt         LIKE skat-txt20 ,
        kostl             LIKE zfi_pes_gid_bas-kostl ,
        gsber             LIKE zfi_pes_gid_bas-gsber ,
        aufnr             LIKE zfi_pes_gid_bas-aufnr ,
        tutar             LIKE zfi_pes_gid_kal-tutar ,
        siniflama_tutari  LIKE zfi_pes_gid_kal-tutar ,
        waers             LIKE zfi_pes_gid_bas-waers ,
        kursf             LIKE zfi_pes_gid_bas-kursf ,
        tutar_upb         LIKE bseg-dmbtr , "ergin
        awkey_vuk         LIKE zfi_pes_gid_kal-awkey_vuk ,
        message_vuk       LIKE zfi_pes_gid_kal-message_vuk ,
        awkey_tfrs        LIKE zfi_pes_gid_kal-awkey_tfrs ,
        message_tfrs      LIKE zfi_pes_gid_kal-message_tfrs ,
        awkey_vrm         LIKE zfi_pes_gid_kal-awkey_vrm ,
        message_vrm       LIKE zfi_pes_gid_kal-message_vrm ,
        selkz ,
       END OF it_report .

DATA : it_return  LIKE TABLE OF bapiret2 WITH HEADER LINE ,
       wa_bkpf    LIKE bkpf ,
       it_bseg    LIKE bseg OCCURS 0 WITH HEADER LINE ,
       tfrs_err ,
       vuk12_err ,
       vrm_tutar  LIKE zfi_pes_gid_kal-tutar ,
       next_spmon TYPE spmon ,
       beg_spmon  TYPE spmon ,
       end_spmon  TYPE spmon .

DATA : BEGIN OF it_skat OCCURS 0 ,
        saknr LIKE skat-saknr ,
        txt20 LIKE skat-txt20 ,
       END OF it_skat .


 DATA :  BEGIN OF it_oncekiay_gider OCCURS 0,
           spmon            TYPE zfi_pes_gid_kal-spmon ,
           belgeno          TYPE zfi_pes_gid_bas-belgeno ,
           kostl            TYPE zfi_pes_gid_bas-kostl ,
           tutar            TYPE zfi_pes_gid_kal-tutar ,
         END OF  it_oncekiay_gider.

DATA  it_temp_report LIKE TABLE OF it_report.

FIELD-SYMBOLS <fs_oncekiay_gider> LIKE LINE OF it_oncekiay_gider.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001 .
PARAMETERS : p_bukrs LIKE zfi_pes_gid_bas-bukrs
                      OBLIGATORY MEMORY ID buk ,
             p_spmon LIKE zfi_pes_gid_kal-spmon DEFAULT sy-datum(6).
SELECT-OPTIONS : "s_belge for zfi_pes_gid_bas-belgeno ,
                 s_police FOR zfi_pes_gid_bas-policeno ,
                 s_belge  FOR zfi_pes_gid_bas-belgeno ,
*                 s_licen  for zfi_pes_gid_bas-license_num ,
                 s_hkont FOR zfi_pes_gid_kal-hkont NO-DISPLAY .
SELECTION-SCREEN END OF BLOCK b1 .

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002 .
PARAMETERS : p_date   LIKE bkpf-budat OBLIGATORY ,
             p_blart  LIKE bkpf-blart OBLIGATORY DEFAULT 'SB' .
SELECTION-SCREEN END OF BLOCK b2 .

AT SELECTION-SCREEN OUTPUT .
  ON CHANGE OF p_spmon .
    p_date = p_spmon .
    p_date+6(2) = '01' .
    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = p_date
      IMPORTING
        last_day_of_month = p_date
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.

  ENDON .

START-OF-SELECTION .

  SELECT saknr txt20
    FROM skat
    INTO TABLE it_skat
    WHERE spras EQ sy-langu AND
          ktopl EQ 'YGHP' .
SORT it_skat BY saknr.

  SELECT zfi_pes_gid_bas~bukrs zfi_pes_gid_kal~spmon
         zfi_pes_gid_bas~belgeno zfi_pes_gid_bas~sgtxt
         zfi_pes_gid_kal~taksitno zfi_pes_gid_bas~hkont hkont_uzun
         zfi_pes_gid_bas~hkont_gider zfi_pes_gid_bas~kostl
         zfi_pes_gid_bas~gsber zfi_pes_gid_bas~aufnr
         zfi_pes_gid_bas~waers zfi_pes_gid_bas~kursf
         zfi_pes_gid_kal~tutar awkey_tfrs zfi_pes_gid_bas~policeno
         message_tfrs awkey_vuk message_vuk
      FROM zfi_pes_gid_bas JOIN zfi_pes_gid_kal
           ON zfi_pes_gid_bas~belgeno EQ zfi_pes_gid_kal~belgeno AND
              zfi_pes_gid_bas~kostl EQ zfi_pes_gid_kal~kostl
      INTO CORRESPONDING FIELDS OF TABLE it_report
      WHERE bukrs EQ p_bukrs AND
            spmon EQ p_spmon AND
*            zfi_pes_gid_bas~BELGENO in s_belge and
            zfi_pes_gid_bas~policeno IN s_police AND
            zfi_pes_gid_bas~belgeno  IN s_belge AND
*            zfi_pes_gid_bas~license_num in s_licen and
            zfi_pes_gid_bas~hkont IN s_hkont AND
            zfi_pes_gid_bas~awkey NE '' AND
            zfi_pes_gid_bas~durum NE 'I' . " iptal poliçeler gelmesin

   it_temp_report[] = it_report[].
   SORT it_temp_report BY belgeno.
   DELETE ADJACENT DUPLICATES FROM
   it_temp_report  COMPARING belgeno.


*Police numarasına göre
  IF it_temp_report[] IS NOT INITIAL.

    SELECT
      spmon
      belgeno
      kostl
      tutar
     INTO TABLE it_oncekiay_gider
     FROM zfi_pes_gid_kal
     FOR ALL ENTRIES IN it_temp_report
     WHERE belgeno    = it_temp_report-belgeno    AND
           spmon      < p_spmon                   AND
           awkey_vuk  = space                     AND
           awkey_tfrs = space.

      FREE it_temp_report.
  ENDIF.


  LOOP AT it_report .
    CLEAR it_skat .
    READ TABLE it_skat
          WITH KEY saknr = it_report-hkont  BINARY SEARCH .
    IF sy-subrc IS INITIAL .
      it_report-kisa_txt = it_skat-txt20 .
    ENDIF .

    CLEAR it_skat .
    READ TABLE it_skat
          WITH KEY saknr = it_report-hkont_uzun BINARY SEARCH .
    IF sy-subrc IS INITIAL .
      it_report-uzun_txt = it_skat-txt20 .
    ENDIF .

    CLEAR it_skat .
    READ TABLE it_skat
          WITH KEY saknr = it_report-hkont_gider BINARY SEARCH .
    IF sy-subrc IS INITIAL .
      it_report-gider_txt = it_skat-txt20 .
    ENDIF .

    CLEAR next_spmon .
    next_spmon = it_report-spmon .
    ADD 1 TO next_spmon(4) .
*    IF next_spmon+4(2) NE '12' .
*      ADD 1 TO next_spmon+4(2) .
*    ELSE .
*      ADD 1 TO next_spmon(4) .
*      next_spmon+4(2) = '01' .
*    ENDIF .
*    clear zfi_pes_gid_kal .
    SELECT SINGLE tutar FROM zfi_pes_gid_kal
      INTO it_report-siniflama_tutari
      WHERE belgeno EQ it_report-belgeno AND
            spmon EQ next_spmon .

     LOOP AT  it_oncekiay_gider ASSIGNING <fs_oncekiay_gider>
       WHERE belgeno = it_report-belgeno.
       ADD <fs_oncekiay_gider>-tutar TO it_report-tutar.
     ENDLOOP.

    "begin of insertion:20140514:fergins:
    IF it_report-waers EQ 'TRY'.
      it_report-tutar_upb  = it_report-tutar .
      it_report-kursf      = 1.
    ELSE.
      it_report-tutar_upb  = it_report-tutar * it_report-kursf .
    ENDIF.
    "end of insertion:20140514:fergins:

    MODIFY it_report .

  ENDLOOP .


END-OF-SELECTION .

  CLEAR : lt_t_fieldcatalog , lt_t_fieldcatalog[] .
  v_default_recname = 'IT_REPORT' .
  v_default_report_name = sy-repid .
  PERFORM set_report_fcat.
  PERFORM show_report_fcat_lvc TABLES it_report
                      USING  ''"P_VARI
                             gs_variant
                             v_default_report_name
                             v_default_recname.

*&---------------------------------------------------------------------*
*&      Form  save_document
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM save_document .

  LOOP AT it_report WHERE selkz IS NOT INITIAL AND
                          awkey_vuk IS INITIAL AND
                          awkey_tfrs IS INITIAL .

    """ VUK VİRMAN KAYDI
    PERFORM vuk_virman_kaydi .

*    IF it_report-awkey_vuk IS NOT INITIAL .
      " iki defter için virman başarıyla yapıldı şimdi tfrs defterinde
      " 13. ay sonrasının tutarını uzun vadeli hesaptan kısa vadeli
      " hesaba virman yap
*      PERFORM tfrs_virman_kaydi .
*    ENDIF .

    IF it_report-awkey_vuk IS NOT INITIAL AND tfrs_err IS INITIAL.

      PERFORM vuk_virman_280_180_12aylik .

      IF vuk12_err IS NOT INITIAL .

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .

        it_report-awkey_tfrs = '' .
        it_report-message_tfrs =
       '12 Aylık 280 -> 180 virmanı yapılamadığından geri alındı' .

        it_report-awkey_vuk = '' .
        it_report-message_vuk =
       '12 Aylık 280 -> 180 virmanı yapılamadığından geri alındı' .
      ELSE .
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
*        it_report-message_vuk = 'Kayıt Yaratıldı' .
*        it_report-message_tfrs = 'Kayıt Yaratıldı' .
      ENDIF .

    ELSE .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ENDIF .

    MODIFY it_report .

    UPDATE zfi_pes_gid_kal
      SET awkey_vuk = it_report-awkey_vuk
          message_vuk = it_report-message_vuk
          awkey_tfrs = it_report-awkey_tfrs
          message_tfrs = it_report-message_tfrs
          awkey_vrm = it_report-awkey_vrm
          message_vrm = it_report-message_vrm
      WHERE belgeno EQ it_report-belgeno AND
            kostl EQ it_report-kostl AND
            spmon   EQ it_report-spmon .
    COMMIT WORK AND WAIT .

     LOOP AT it_oncekiay_gider
       ASSIGNING <fs_oncekiay_gider>
       WHERE belgeno = it_report-belgeno.
       UPDATE zfi_pes_gid_kal
      SET awkey_vuk = it_report-awkey_vuk
          message_vuk = it_report-message_vuk
          awkey_tfrs = it_report-awkey_tfrs
          message_tfrs = it_report-message_tfrs
          awkey_vrm = it_report-awkey_vrm
          message_vrm = it_report-message_vrm
      WHERE belgeno EQ <fs_oncekiay_gider>-belgeno         AND
            kostl   EQ <fs_oncekiay_gider>-kostl           AND
            spmon   EQ <fs_oncekiay_gider>-spmon .
    COMMIT WORK AND WAIT .
    ENDLOOP.
  ENDLOOP .

ENDFORM .                    "save_document

*&---------------------------------------------------------------------*
*&      Form  vuk_virman_kaydi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM vuk_virman_kaydi .

  CLEAR : it_return[] , it_return , wa_bkpf , it_bseg , it_bseg[] ,
              tfrs_err.

  wa_bkpf-bukrs = it_report-bukrs .
  wa_bkpf-waers = it_report-waers .
  wa_bkpf-kursf = it_report-kursf .
*    wa_bkpf-ldgrp = 'ZX' . " defter vermiyoruz iki deftere de gidecek
  wa_bkpf-usnam = sy-uname .
  wa_bkpf-budat = p_date .
  wa_bkpf-bldat = p_date .
  wa_bkpf-wwert = p_date .
  wa_bkpf-blart = p_blart .

  it_bseg-hkont = it_report-hkont .
  it_bseg-wrbtr = it_report-tutar * -1 .
  it_bseg-gsber = it_report-gsber .
  it_bseg-valut = p_date .
  it_bseg-aufnr = it_report-aufnr.
  APPEND it_bseg .

  CLEAR it_bseg .
  it_bseg-hkont = it_report-hkont_gider .
  it_bseg-wrbtr = it_report-tutar .
  it_bseg-kostl = it_report-kostl .
  it_bseg-gsber = it_report-gsber .
  it_bseg-valut = p_date .
  it_bseg-aufnr = it_report-aufnr.
  APPEND it_bseg .


  CLEAR : it_report-message_vuk .

  CALL FUNCTION 'ZFI_DOCUMENT_POST'
    EXPORTING
      bkpf           = wa_bkpf
      commit         = ''
    IMPORTING
      e_key          = it_report-awkey_vuk
    TABLES
      bseg           = it_bseg[]
      return         = it_return[]
    EXCEPTIONS
      dogrudan_kayit = 1
      OTHERS         = 2.
  IF it_report-awkey_vuk IS INITIAL OR
     it_report-awkey_vuk EQ '$' .
    CLEAR it_report-awkey_vuk .
  ENDIF .

  LOOP AT it_return WHERE type EQ 'A' OR type EQ 'E' .
    CHECK NOT ( it_return-type EQ 'E' AND it_return-id EQ 'RW'
                AND it_return-number EQ '609' ) .
    CONCATENATE it_report-message_vuk it_return-message ','
        INTO it_report-message_vuk .
  ENDLOOP .
  IF sy-subrc IS NOT INITIAL .
    it_report-message_vuk = 'Kayıt yaratıldı' .
  ENDIF .

ENDFORM .                    "vuk_virman_kaydi

*&---------------------------------------------------------------------*
*&      Form  tfrs_virman_kaydi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tfrs_virman_kaydi .

  IF it_report-siniflama_tutari IS NOT INITIAL .
    CLEAR : it_return[] , it_return , wa_bkpf , it_bseg ,
            it_bseg[].

    wa_bkpf-bukrs = it_report-bukrs .
    wa_bkpf-waers = it_report-waers .
    wa_bkpf-kursf = it_report-kursf .
    wa_bkpf-ldgrp = 'Z1' . " tfrs defterine gidecek
    wa_bkpf-usnam = sy-uname .
    wa_bkpf-budat = p_date .
    wa_bkpf-bldat = p_date .
    wa_bkpf-wwert = p_date .
    wa_bkpf-blart = p_blart .

    it_bseg-hkont = it_report-hkont_uzun .
    it_bseg-wrbtr = it_report-siniflama_tutari * -1 .
    it_bseg-gsber = it_report-gsber .
    it_bseg-valut = p_date .
    it_bseg-aufnr = it_report-aufnr.
    APPEND it_bseg .

    CLEAR it_bseg .
    it_bseg-hkont = it_report-hkont .
    it_bseg-wrbtr = it_report-siniflama_tutari .
    it_bseg-kostl = it_report-kostl .
    it_bseg-gsber = it_report-gsber .
    it_bseg-valut = p_date .
    it_bseg-aufnr = it_report-aufnr.
    APPEND it_bseg .

    CLEAR : it_report-awkey_tfrs .

    CALL FUNCTION 'ZFI_DOCUMENT_POST'
      EXPORTING
        bkpf           = wa_bkpf
        commit         = ''
      IMPORTING
        e_key          = it_report-awkey_tfrs
      TABLES
        bseg           = it_bseg[]
        return         = it_return[]
      EXCEPTIONS
        dogrudan_kayit = 1
        OTHERS         = 2.

    IF it_report-awkey_tfrs IS INITIAL OR
       it_report-awkey_tfrs EQ '$' .
      CLEAR : it_report-awkey_tfrs , it_report-awkey_vuk .
      it_report-message_vuk =
      '280 --> 180 virmanı yapılamadığından geri alındı' .
      tfrs_err = 'X' .
    ENDIF .

    LOOP AT it_return WHERE type EQ 'A' OR type EQ 'E' .
      CHECK NOT ( it_return-type EQ 'E' AND it_return-id EQ 'RW'
                  AND it_return-number EQ '609' ) .
      CONCATENATE it_report-message_tfrs it_return-message ','
          INTO it_report-message_tfrs .
    ENDLOOP .
    IF sy-subrc IS NOT INITIAL .
      it_report-message_tfrs = 'Kayıt yaratıldı' .
    ENDIF .
  ELSE .
    CLEAR : tfrs_err , it_report-awkey_tfrs ,
            it_report-message_tfrs .
  ENDIF .

ENDFORM .                    "tfrs_virman_kaydi

*&---------------------------------------------------------------------*
*&      Form  vuk_virman_280_180_12aylik
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM vuk_virman_280_180_12aylik .

  CHECK p_spmon+4(2) EQ '12' .

  """ 12 aylık 280 --> 180 VİRMAN KAYDI HER İKİ DEFTERE
  CLEAR : it_return[] , it_return , wa_bkpf , it_bseg , it_bseg[] ,
          beg_spmon , end_spmon , vrm_tutar , vuk12_err .

  beg_spmon(4) = p_spmon(4) + 1 .
  beg_spmon+4(2) = '01' .

  end_spmon = beg_spmon .
  end_spmon+4(2) = '12' .

  SELECT SUM( tutar ) FROM zfi_pes_gid_kal
    INTO vrm_tutar
    WHERE belgeno EQ it_report-belgeno AND
          spmon BETWEEN beg_spmon AND end_spmon .

  CHECK vrm_tutar IS NOT INITIAL.

  wa_bkpf-ldgrp = '0L' .
  wa_bkpf-bukrs = it_report-bukrs .
  wa_bkpf-waers = it_report-waers .
  wa_bkpf-kursf = it_report-kursf .
  wa_bkpf-usnam = sy-uname .
  wa_bkpf-budat = p_date .
  wa_bkpf-bldat = p_date .
  wa_bkpf-wwert = p_date .
  wa_bkpf-blart = p_blart .

  it_bseg-hkont = it_report-hkont_uzun .
  it_bseg-wrbtr = vrm_tutar * -1 .
  it_bseg-gsber = it_report-gsber .
  it_bseg-valut = p_date .
  it_bseg-aufnr = it_report-aufnr.
  APPEND it_bseg .

  CLEAR it_bseg .
  it_bseg-hkont = it_report-hkont .
  it_bseg-wrbtr = vrm_tutar .
  it_bseg-gsber = it_report-gsber .
  it_bseg-valut = p_date .
  it_bseg-aufnr = it_report-aufnr.
  APPEND it_bseg .

  CLEAR : it_report-awkey_vrm , it_report-message_vrm  .

  CALL FUNCTION 'ZFI_DOCUMENT_POST'
    EXPORTING
      bkpf           = wa_bkpf
      commit         = ''
    IMPORTING
      e_key          = it_report-awkey_vrm
    TABLES
      bseg           = it_bseg[]
      return         = it_return[]
    EXCEPTIONS
      dogrudan_kayit = 1
      OTHERS         = 2.
  IF it_report-awkey_vrm IS INITIAL OR
     it_report-awkey_vrm EQ '$' .
    CLEAR it_report-awkey_vrm .
    vuk12_err = 'X' .
  ELSE .
    it_report-message_vrm = 'Kayıt Yaratıldı' .
  ENDIF .

  LOOP AT it_return WHERE type EQ 'A' OR type EQ 'E' .
    CHECK NOT ( it_return-type EQ 'E' AND it_return-id EQ 'RW'
                AND it_return-number EQ '609' ) .
    CONCATENATE it_report-message_vrm it_return-message ','
        INTO it_report-message_vrm .
  ENDLOOP .

ENDFORM .                    "vuk_virman_280_180_12aylik

*&---------------------------------------------------------------------*
*&      Form  ters_kayit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ters_kayit .

DATA : BEGIN OF lt_oncekiay_gider_ters  OCCURS 0,
         belgeno TYPE  zfi_pes_gid_kal-belgeno,
         spmon   TYPE  zfi_pes_gid_kal-spmon,
      END OF lt_oncekiay_gider_ters,
      lt_report LIKE TABLE OF it_report.


    FIELD-SYMBOLS <fs_oncekiay_gider_ters> LIKE LINE OF
                   lt_oncekiay_gider_ters.


      lt_report[] = it_report[].
      DELETE lt_report WHERE selkz <> 'X'.
      SORT lt_report BY belgeno awkey_vuk  awkey_tfrs.
      DELETE ADJACENT DUPLICATES FROM
      lt_report COMPARING belgeno awkey_vuk  awkey_tfrs.
   IF lt_report[] IS NOT INITIAL.
     SELECT
      belgeno
      spmon
     INTO TABLE lt_oncekiay_gider_ters
     FROM zfi_pes_gid_kal
       FOR ALL ENTRIES IN lt_report
     WHERE belgeno    = lt_report-belgeno        AND
           spmon      < p_spmon                  AND
           awkey_vuk  = lt_report-awkey_vuk      AND
           awkey_tfrs = lt_report-awkey_tfrs.
  ENDIF.


  LOOP AT it_report WHERE selkz IS NOT INITIAL .
    PERFORM reverse_batch USING ''
                          CHANGING it_report-awkey_vuk
                                   it_report-message_vuk .

    PERFORM reverse_batch USING 'TFRS'
                          CHANGING it_report-awkey_tfrs
                                   it_report-message_tfrs .

    PERFORM reverse_batch USING 'TFRS'
                          CHANGING it_report-awkey_vrm
                                   it_report-message_vrm .





    MODIFY it_report .

    UPDATE zfi_pes_gid_kal
    SET awkey_vuk = it_report-awkey_vuk
        message_vuk = it_report-message_vuk
        awkey_tfrs = it_report-awkey_tfrs
        message_tfrs = it_report-message_tfrs
        awkey_vrm = it_report-awkey_vrm
        message_vrm = it_report-message_vrm
    WHERE belgeno EQ it_report-belgeno AND
          spmon   EQ it_report-spmon .
    COMMIT WORK AND WAIT .
*önceki aylarda aynı policeno üzerinde aynı fi belgesine ait varsa
*güncelle(Bunun nedeni:Önceki aylara bakıp çalışmamassa son çalışan aya)
*ekleme yapılıp ona göre belgeyi o ay ve önceki ayada o belgeyi yaz
*kontrol ve günc.koyulduğundan ters kayıt içinde benzeri yazıldı.

    LOOP AT lt_oncekiay_gider_ters
      ASSIGNING <fs_oncekiay_gider_ters>
      WHERE belgeno = it_report-belgeno.
           UPDATE zfi_pes_gid_kal
           SET awkey_vuk    = it_report-awkey_vuk
               message_vuk  = it_report-message_vuk
               awkey_tfrs   = it_report-awkey_tfrs
               message_tfrs = it_report-message_tfrs
               awkey_vrm    = it_report-awkey_vrm
               message_vrm  = it_report-message_vrm
           WHERE belgeno   EQ <fs_oncekiay_gider_ters>-belgeno AND
                 spmon     EQ <fs_oncekiay_gider_ters>-spmon .
           COMMIT WORK AND WAIT .
   ENDLOOP.

  ENDLOOP .
ENDFORM .                    "ters_kayit

*&---------------------------------------------------------------------*
*&      Form  REVERSE_BATCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FV_PRINC   text
*      -->FV_AWKEY   text
*      -->FV_MESSAGE text
*----------------------------------------------------------------------*
FORM reverse_batch  USING fv_princ LIKE tacc_trgt_ldgr-acc_principle
                    CHANGING fv_awkey LIKE bkpf-awkey
                            fv_message .

  CHECK fv_awkey IS NOT INITIAL .

  DATA : reversal LIKE bapiacrev ,
         return LIKE bapiret2 OCCURS 0 WITH HEADER LINE ,
         objkey LIKE bapiacrev-obj_key .

  CLEAR : reversal , return , return[] , objkey .

  reversal-obj_type = 'BKPFF' .
  reversal-obj_key = fv_awkey .
  reversal-obj_sys = sy-mandt .
  reversal-obj_key_r = reversal-obj_key .
  reversal-reason_rev = '01' .
  reversal-acc_principle = fv_princ .

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
    EXPORTING
      reversal = reversal
      bus_act  = 'RFBU'
    IMPORTING
      obj_key  = objkey
    TABLES
      return   = return[].

  LOOP AT return WHERE type EQ 'A' OR type EQ 'E' .
    EXIT .
  ENDLOOP .
  IF sy-subrc IS INITIAL .
    fv_message = 'Ters Kayıt Alınamadı' .
  ELSE .
    fv_message = 'Ters Kayıt Alındı' .
    CLEAR fv_awkey .
  ENDIF .


ENDFORM .                    "REVERSE_BATCH

"""" alv forms here """"

*&---------------------------------------------------------------------*
*&      Form  SET_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_top_of_page.
*  PERFORM COMMENT_BUILD USING GT_LIST_TOP_OF_PAGE[].
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*       EXPORTING
**            i_logo             = 'ENJOYSAP_LOGO'
*            IT_LIST_COMMENTARY = GT_LIST_TOP_OF_PAGE.
ENDFORM.                    "set_top_of_page
*---------------------------------------------------------------------*
*       FORM COMMENT_BUILD                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  LT_TOP_OF_PAGE                                                *
*---------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE
                                        slis_t_listheader.
ENDFORM.                    "comment_build
*---------------------------------------------------------------------*
*  FORM f01_user_command
*---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_user_command USING r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield. "#EC CALLED

  CASE : r_ucomm .
    WHEN '&SAVE' .
      PERFORM save_document .
    WHEN '&IC1' .
      IF rs_selfield-fieldname EQ 'AWKEY_TFRS' OR
         rs_selfield-fieldname EQ 'AWKEY_VUK'.
        SET PARAMETER ID 'BUK' FIELD rs_selfield-value+10(4) .
        SET PARAMETER ID 'BLN' FIELD rs_selfield-value(10) .
        SET PARAMETER ID 'GJR' FIELD rs_selfield-value+14(4) .
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
      ENDIF .
    WHEN '&TERS' .
      PERFORM ters_kayit .
  ENDCASE .


  rs_selfield-refresh = 'X'.
ENDFORM.                    "f01_user_command


*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM set_pf_status_set USING rt_extab TYPE slis_t_extab .   "#EC CALLED
  PERFORM set_excluding_tab TABLES rt_extab.
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab[].
ENDFORM.                    "f01_set_status
*&---------------------------------------------------------------------*
*&      Form  excluding_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excluding_events.

  PERFORM exclude_events TABLES ex_events USING 'CALLER_EXIT'.
*  PERFORM EXCLUDE_EVENTS TABLES EX_EVENTS USING 'USER_COMMAND'.
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_PAGE'.
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_COVERPAGE'.
  PERFORM exclude_events TABLES ex_events USING 'END_OF_COVERPAGE'.
  PERFORM exclude_events TABLES ex_events USING 'FOREIGN_TOP_OF_PAGE'.
  PERFORM exclude_events TABLES ex_events USING 'FOREIGN_END_OF_PAGE'.
*  PERFORM EXCLUDE_EVENTS TABLES EX_EVENTS USING 'PF_STATUS_SET'.
  PERFORM exclude_events TABLES ex_events USING 'LIST_MODIFY'.
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_LIST'.
  PERFORM exclude_events TABLES ex_events USING 'END_OF_PAGE'.
  PERFORM exclude_events TABLES ex_events USING 'END_OF_LIST'.
  PERFORM exclude_events TABLES ex_events USING 'AFTER_LINE_OUTPUT'.
  PERFORM exclude_events TABLES ex_events USING 'BEFORE_LINE_OUTPUT'.
  PERFORM exclude_events TABLES ex_events USING 'REPREP_SEL_MODIFY'.
  PERFORM exclude_events TABLES ex_events USING 'SUBTOTAL_TEXT'.
  PERFORM exclude_events TABLES ex_events USING 'GROUPLEVEL_CHANGE'.

*  PERFORM APPEND_EVENTS  TABLES AP_EVENTS USING 'DATA_CHANGED'.
*  PERFORM APPEND_EVENTS  TABLES AP_EVENTS USING 'ITEM_DATA_EXPAND'.
*  PERFORM APPEND_EVENTS  TABLES AP_EVENTS USING 'GROUPLEVEL_CHANGE'.
ENDFORM.                    " excluding_events

*&---------------------------------------------------------------------*
*&      Form  SET_EXCLUDING_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->EXTAB      text
*----------------------------------------------------------------------*
FORM set_excluding_tab TABLES extab.
  REFRESH extab.
*  EXTAB = '&ABC'.      APPEND EXTAB.
*  extab = '&UMC'.      append extab.
*  extab = '%SL' .      append extab.
*  extab = '&SUM'.      append extab.
*  extab = '&OL0'.      append extab.
*  extab = '&OAD'.      append extab.
*  extab = '&AVE'.      append extab.
*  extab = '&ILT'.      append extab.
*  extab = '&ETA'.      append extab.
*  extab = '%PC' .      append extab.
*  extab = '&ALL'.      append extab.
*  extab = '&SAL'.      append extab.
*  EXTAB = '&EB9'.      APPEND EXTAB.
*  EXTAB = '&REFRESH'.  APPEND EXTAB.
*  extab = '&OUP'.      append extab.
*  extab = '&ODN'.      append extab.
*  extab = '&RNT_PREV'. append extab.
*  extab = '&VEXCEL'.   append extab.
*  extab = '&AOW'.      append extab.
*  EXTAB = '&GRAPH'.    APPEND EXTAB.
*  EXTAB = '&INFO'.     APPEND EXTAB.
*  EXTAB = '&DET'.     APPEND EXTAB.

ENDFORM.                    " set_excluding_tab

*&---------------------------------------------------------------------*
*&      Form  SET_REPORT_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_report_fcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = v_default_report_name
      i_internal_tabname     = v_default_recname
      i_inclname             = v_default_report_name
      i_client_never_display = 'X'
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = lt_t_fieldcatalog[]
    EXCEPTIONS
      OTHERS                 = 3.

  PERFORM set_field_cat_user_exit.

ENDFORM.                    " set_report_fcat

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CAT_USER_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_field_cat_user_exit .
  DATA: recname TYPE slis_tabname.
  DATA : v_title(42) TYPE c.
  MOVE: 'SELTEXT_L/SELTEXT_M/SELTEXT_S/REPTEXT_DDIC/F4AVAILABL' TO
v_title.
  recname = v_default_recname .

  PERFORM
   set_line_field_cat TABLES lt_t_fieldcatalog USING :
    recname 'KISA_TXT' v_title 'Kısa Vadeli Hesap Tanımı' ,
    recname 'UZUN_TXT' v_title 'Uzun Vadeli Hesap Tanımı' ,
    recname 'GIDER_TXT' v_title 'Gider Hesanı Tanımı' ,
    recname 'VIRMAN_VUK' v_title '180->700 virman belgesi' ,
    recname 'VIRMAN_TFRS' v_title '280->180 virman belgesi' ,
    recname 'SINIFLAMA_TUTARI' v_title 'Sınıflama Tutarı' .

  LOOP AT lt_t_fieldcatalog WHERE key IS NOT INITIAL .
    lt_t_fieldcatalog-key = '' .
    MODIFY lt_t_fieldcatalog .
  ENDLOOP .

  DELETE lt_t_fieldcatalog WHERE fieldname EQ 'SELKZ' OR
                                 fieldname EQ 'AWKEY_VRM' OR
                                 fieldname EQ 'MESSAGE_VRM' .


ENDFORM.                    " set_field_cat_user_exit

*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT_USER_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PVARI    text
*      -->P_DRNAME   text
*----------------------------------------------------------------------*
FORM set_layout_user_exit USING    p_pvari
                                   p_drname.

*  GS_GRID_SET-EDT_CLL_CB = 'X'.
*  GS_LAYOUT-GET_SELINFOS       = 'X'.
*  GS_LAYOUT-COLTAB_FIELDNAME   = 'COLOR'.
*  GS_LAYOUT-INFO_FIELDNAME     = 'COLOR'.
*  gs_layout-coltab_fieldname   = 'COLOR'.
*  gs_layout-expand_fieldname  = 'BUKRS'.
  gs_layout-zebra = 'X' .
  gs_layout-box_fieldname = 'SELKZ'.
  gs_layout-colwidth_optimize  = 'X'    .
ENDFORM.                    " set_layout_user_exit
