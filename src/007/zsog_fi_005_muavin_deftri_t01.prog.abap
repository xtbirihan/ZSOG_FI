*&---------------------------------------------------------------------*
*&  Include           ZSOG_FI_005_MUAVIN_DEFTRI_T01
*&---------------------------------------------------------------------*
TYPE-POOLS: tpit.
TABLES :  bkpf,adrc,"zallgugent002,
          bseg,  "Muhasebe belge parçası
          bsis,  "Muhasebe: Ana hesaplar için ikincil dizin
          bsas,
*          BSAS,  "Muhasebe: Ana hesaplar için ikincil endeks (dnklş.
          bsid,  "Muhasebe: Müşteriler için ikincil endeks
          bsad,  "Muhasebe: Müşteriler için ikincil dizin (dnklş.
          bsik,  "Muhasebe: Satıcılar için ikincil endeks
          bsak,  "Muhasebe: Satıcılar için ikincil endeks (dnklş.
          kna1,  "Müşteri ana verileri (genel veriler)
          lfa1,  "Satıcı ana verileri (genel bölüm)
          cobl,  "Hesap tayin blokuc
          t001, " Şirket Kodu
          faglflexa " defteri kebir görünümü
.
*          SKAT.  "Ana hesap ana verileri (Hesap planı: Tanımı)
DATA: g_first_top_of_page(1) TYPE c, "Flag for first top of page
      g_totpages(3)          TYPE n,
      g_pageno(3)            TYPE n,
      g_start_of_list(1)     TYPE c.

DATA   :  BEGIN OF itab OCCURS 0   ,
            prctr LIKE bsis-prctr  ,
            gsber LIKE bsis-gsber  ,
            hkont LIKE bsis-hkont  ,
            type                   ,  "Satici-Musteri ayrac
            saknr LIKE bseg-saknr  ,                        "YT20061218
            xref1_hd LIKE bkpf-xref1_hd,
            xref2_hd LIKE bkpf-xref2_hd,
            bfiyat LIKE bsik-dmbtr,
            lifnr LIKE bsik-lifnr  ,
            kunnr LIKE bsid-kunnr  ,
            name3 LIKE kna1-name3,
            dmbe2 LIKE bsik-dmbe2,
            dmbe3 LIKE bsik-dmbe3,
            augdt LIKE bsid-augdt  ,
            augbl LIKE bsid-augbl  ,
            bldat LIKE bsis-bldat  ,
            budat LIKE bsis-budat  ,  "10/10/2001 <**>p_date
            valut LIKE bsis-valut  ,
            valor(4) TYPE n        ,
            gecik(8) TYPE p DECIMALS 0 ,
***
            belnr_alt LIKE bkpf_addon-belnr_alt ,
            bukrs LIKE bkpf-bukrs  ,
            gjahr LIKE bsis-gjahr  ,
            belnr LIKE bsis-belnr  ,
            buzei LIKE bsis-buzei  ,
            awkey LIKE bkpf-awkey  ,
            xblnr LIKE bkpf-xblnr  ,
            usnam LIKE bkpf-usnam  ,

***
            zlspr LIKE bseg-zlspr  ,
            blart LIKE bsis-blart  ,
            shkzg LIKE bsis-shkzg  ,
            tutar(11) TYPE p DECIMALS 2  ,

            dmbtr LIKE bsis-dmbtr  ,
            wrbtr LIKE bsis-wrbtr  ,
            waers LIKE bsis-waers  ,
            bstat  LIKE bsis-bstat  ,
            aufnr LIKE bsas-aufnr  ,
            aufnr_text LIKE coas-ktext,

***
            zuonr LIKE bsis-zuonr  ,
            kostl LIKE bsis-kostl  ,
            sgtxt LIKE bsis-sgtxt  ,
            bktxt LIKE bkpf-bktxt ,
            umskz LIKE bsid-umskz  ,
            xref1 LIKE bsid-xref1,
            xref2 LIKE bsid-xref2,
            xref3 LIKE bsid-xref3,
***
            bdiff LIKE bseg-bdiff, "değerleme farkı
            zbd3t LIKE bsid-zbd3t,
            absbt LIKE bseg-absbt, "hedge edilen tutar
            stblg LIKE bkpf-stblg,
            stgrd LIKE bkpf-stgrd,
            matnr LIKE bseg-matnr,
*            bwshb3 LIKE rfpos-bwshb3,
*            bwshb4 LIKE rfpos-bwshb4,
*            bwshb5 LIKE rfpos-bwshb5,
            gkart LIKE rfpos-gkart,
            gkont LIKE rfpos-gkont,
            kursf LIKE bkpf-kursf,
            bwwrt LIKE rfposxext-bwwrt,
            menge LIKE bseg-menge,
            meins LIKE bseg-meins,
            maktx LIKE makt-maktx,
            projk LIKE bsis-projk,
            post1 LIKE prps-post1,
          END OF itab.

DATA   :  BEGIN OF devir OCCURS 0       ,
             mu_sa LIKE bsid-kunnr      , " Müşteri ve Satıcı
             hkont LIKE bsis-hkont      ,
             dmbtr(11) TYPE p DECIMALS 2,
             alck(11)  TYPE p DECIMALS 2,
             borc(11)  TYPE p DECIMALS 2,
             waers     TYPE waers       ,
             islendi(1)                 ,
          END OF devir.

DATA : BEGIN OF couples OCCURS 0,
         kunnr LIKE kna1-kunnr ,  "Musteri
         lifnr LIKE lfa1-lifnr ,  "Satici
       END OF couples.
***
DATA : BEGIN OF rtab OCCURS 0    ,
        tabix(8)                 ,
        prctr   LIKE bsis-prctr  ,
        lifnr LIKE bseg-lifnr,
*        name_lfa1 like lfa1-name1,"burcua
        kunnr LIKE bseg-kunnr,
*        name_kna1(70) type c ,"burcua
        name3 LIKE kna1-name3,
        gsber   LIKE bsis-gsber  ,
        hkont   LIKE bsis-hkont  ,
        saknr   LIKE bseg-saknr  ,                          "YT20061218
        name    LIKE kna1-name1  ,
        waers   LIKE bsis-waers  ,
        hwaer   LIKE bkpf-hwaer  ,
        xref1_hd LIKE bkpf-xref1_hd,
        xref2_hd LIKE bkpf-xref2_hd,
        dmbe2 LIKE bsik-dmbe2,
        dmbe3 LIKE bsik-dmbe3,
        bfiyat LIKE bsik-dmbtr,
        gjahr LIKE bsis-gjahr    ,
        belnr LIKE bsis-belnr    ,
        buzei LIKE bsis-buzei    ,
        zuonr LIKE bsis-zuonr    ,
        umskz LIKE bsid-umskz    ,
        bldat LIKE bsis-bldat    ,
        budat LIKE bsis-budat    ,
        dmbtr LIKE bsis-dmbtr    ,
        devir(11) TYPE p DECIMALS 2  ,
        dmbtr1(11) TYPE p DECIMALS 2 ,
        dmbtr2(11) TYPE p DECIMALS 2 ,
        dmbtr3(11) TYPE p DECIMALS 2 ,
        dmbtr4(11) TYPE p DECIMALS 2 ,
        dmbtr5(11) TYPE p DECIMALS 2 ,"add by yahyat 18.11.2005
        dmbtr6(11) TYPE p DECIMALS 2 ,"add by yahyat 18.11.2005
        sgtxt LIKE bsis-sgtxt        ,
        bktxt LIKE bkpf-bktxt ,
        augdt LIKE bsis-augdt ,
        augbl LIKE bsis-augbl ,
        valut LIKE bsis-valut ,
        valor LIKE itab-valor ,
        gecik(8) TYPE p DECIMALS 0  ,
        usnam LIKE bkpf-usnam  ,
*        BELNR_ALT LIKE BKPF_ADDON-BELNR_ALT ,
        awkey LIKE bkpf-awkey ,
        xblnr LIKE bkpf-xblnr ,
        blart LIKE bsis-blart ,
        bstat  LIKE bsis-bstat  ,
        kostl LIKE bsis-kostl ,
        aufnr LIKE bsis-aufnr ,
        aufnr_text LIKE coas-ktext,
        xref1 LIKE bsid-xref1        ,
        xref2 LIKE bsid-xref2        ,
        xref3 LIKE bsis-xref3        ,
        belnr_alt LIKE bkpf_addon-belnr_alt,
        line  LIKE tline-tdline ,
        type                  ,  "Satici-Musteri ayrac
""
        zlspr LIKE bseg-zlspr  ,
        bdiff LIKE bseg-bdiff, "değerleme farkı
        zbd3t LIKE bsid-zbd3t,
        wrbtr LIKE bseg-wrbtr,
        absbt LIKE bseg-absbt, "hedge edilen tutar
        stblg LIKE bkpf-stblg,
        stgrd LIKE bkpf-stgrd,
        matnr LIKE bseg-matnr,
*        bwshb3 LIKE rfpos-bwshb3,
*        bwshb4 LIKE rfpos-bwshb4,
*        bwshb5 LIKE rfpos-bwshb5,
        gkart LIKE rfpos-gkart,
        gkont LIKE rfpos-gkont,
        kursf LIKE bkpf-kursf,
        bwwrt LIKE rfposxext-bwwrt,
        menge LIKE bseg-menge ,
        meins LIKE bseg-meins ,
        maktx LIKE makt-maktx,
        ltext LIKE t074t-ltext,
        stcd2 LIKE lfa1-stcd2,
        projk LIKE bsis-projk,
        post1 LIKE prps-post1,
      line_color(4) TYPE c,
       END OF rtab.

DATA : BEGIN OF gs_rtab ,
        tabix(8)                 ,
        prctr   LIKE bsis-prctr  ,
        lifnr LIKE bseg-lifnr,
        kunnr LIKE bseg-kunnr,
        name3 LIKE kna1-name3,
        xref1_hd LIKE bkpf-xref1_hd,
        xref2_hd LIKE bkpf-xref2_hd,
        dmbe2 LIKE bsik-dmbe2,
        dmbe3 LIKE bsik-dmbe3,
        bfiyat LIKE bsik-dmbtr,
        gsber   LIKE bsis-gsber  ,
        hkont   LIKE bsis-hkont  ,
        saknr   LIKE bseg-saknr  ,                          "YT20061218
        name    LIKE kna1-name1  ,
        waers   LIKE bsis-waers  ,
        hwaer   LIKE bkpf-hwaer  ,
        gjahr LIKE bsis-gjahr    ,
        belnr LIKE bsis-belnr    ,
        buzei LIKE bsis-buzei    ,
        zuonr LIKE bsis-zuonr    ,
        umskz LIKE bsid-umskz    ,
        bldat LIKE bsis-bldat    ,
        budat LIKE bsis-budat    ,
        dmbtr LIKE bsis-dmbtr    ,
        devir(11) TYPE p DECIMALS 2  ,
        dmbtr1(11) TYPE p DECIMALS 2 ,
        dmbtr2(11) TYPE p DECIMALS 2 ,
        dmbtr3(11) TYPE p DECIMALS 2 ,
        dmbtr4(11) TYPE p DECIMALS 2 ,
        dmbtr5(11) TYPE p DECIMALS 2 ,"add by yahyat 18.11.2005
        dmbtr6(11) TYPE p DECIMALS 2 ,"add by yahyat 18.11.2005
        sgtxt LIKE bsis-sgtxt        ,
        bktxt LIKE bkpf-bktxt ,
        augdt LIKE bsis-augdt ,
        augbl LIKE bsis-augbl ,
        valut LIKE bsis-valut ,
        valor LIKE itab-valor ,
        gecik(8) TYPE p DECIMALS 0  ,
        usnam LIKE bkpf-usnam  ,
*        BELNR_ALT LIKE BKPF_ADDON-BELNR_ALT ,
        zlspr LIKE bseg-zlspr  ,
        awkey LIKE bkpf-awkey ,
        xblnr LIKE bkpf-xblnr ,
        blart LIKE bsis-blart ,
        bstat  LIKE bsis-bstat  ,
        kostl LIKE bsis-kostl ,
        aufnr LIKE bsis-aufnr ,
        aufnr_text LIKE coas-ktext,
        xref1 LIKE bsid-xref1        ,
        xref2 LIKE bsid-xref2        ,
        xref3 LIKE bsis-xref3        ,
        belnr_alt LIKE bkpf_addon-belnr_alt,

        type                  ,  "Satici-Musteri ayrac
        projk LIKE bsis-projk,
        post1 LIKE prps-post1,
       END OF gs_rtab.
DATA : BEGIN OF gs_rtab2 ,
        tabix(8)                 ,
        prctr   LIKE bsis-prctr  ,
        lifnr LIKE bseg-lifnr,
        kunnr LIKE bseg-kunnr,
        name3 LIKE kna1-name3,
        xref1_hd LIKE bkpf-xref1_hd,
        xref2_hd LIKE bkpf-xref2_hd,
        dmbe2 LIKE bsik-dmbe2,
        dmbe3 LIKE bsik-dmbe3,
        bfiyat LIKE bsik-dmbtr,
        gsber   LIKE bsis-gsber  ,
        hkont   LIKE bsis-hkont  ,
        saknr   LIKE bseg-saknr  ,                          "YT20061218
        name    LIKE kna1-name1  ,
        waers   LIKE bsis-waers  ,
        hwaer   LIKE bkpf-hwaer  ,
        gjahr LIKE bsis-gjahr    ,
        belnr LIKE bsis-belnr    ,
        buzei LIKE bsis-buzei    ,
        zuonr LIKE bsis-zuonr    ,
        umskz LIKE bsid-umskz    ,
        bldat LIKE bsis-bldat    ,
        budat LIKE bsis-budat    ,
        dmbtr LIKE bsis-dmbtr    ,
        devir(11) TYPE p DECIMALS 2  ,
        dmbtr1(11) TYPE p DECIMALS 2 ,
        dmbtr2(11) TYPE p DECIMALS 2 ,
        dmbtr3(11) TYPE p DECIMALS 2 ,
        dmbtr4(11) TYPE p DECIMALS 2 ,
        dmbtr5(11) TYPE p DECIMALS 2 ,"add by yahyat 18.11.2005
        dmbtr6(11) TYPE p DECIMALS 2 ,"add by yahyat 18.11.2005
        sgtxt LIKE bsis-sgtxt        ,
        augdt LIKE bsis-augdt ,
        augbl LIKE bsis-augbl ,
        valut LIKE bsis-valut ,
        valor LIKE itab-valor ,
        gecik(8) TYPE p DECIMALS 0  ,
        usnam LIKE bkpf-usnam  ,
*        BELNR_ALT LIKE BKPF_ADDON-BELNR_ALT ,
        zlspr LIKE bseg-zlspr  ,
        awkey LIKE bkpf-awkey ,
        xblnr LIKE bkpf-xblnr ,
        blart LIKE bsis-blart ,
        bstat  LIKE bsis-bstat  ,
        kostl LIKE bsis-kostl ,
        aufnr LIKE bsis-aufnr ,
        aufnr_text LIKE coas-ktext,
        xref1 LIKE bsid-xref1        ,
        xref2 LIKE bsid-xref2        ,
        xref3 LIKE bsis-xref3        ,
        belnr_alt LIKE bkpf_addon-belnr_alt,

        type                  ,  "Satici-Musteri ayrac
        projk LIKE bsis-projk,
        post1 LIKE prps-post1,
       END OF gs_rtab2.

DATA: bkpf_key LIKE bkpf_key.
DATA: bkpf_addon LIKE bkpf_addon.
DATA x LIKE bkpf_addon .
DATA: BEGIN OF gt_yvm OCCURS 0 ,
        bukrs TYPE bkpf-bukrs,
        belnr TYPE bkpf-belnr,
        gjahr TYPE bkpf-gjahr,
        belnr_alt LIKE bkpf_addon-belnr_alt,
      END OF gt_yvm.

DATA: it_h_t001      TYPE tpit_t_vt001 WITH HEADER LINE.
***
DATA : cumule(11) TYPE p DECIMALS 2,
       s_top(11) TYPE p DECIMALS 2,
       h_top(11) TYPE p DECIMALS 2,
       logic                 .

DATA: lv_ktopl TYPE t001-ktopl.

DATA : name(20).

*DATA: gs_liste_yetki TYPE zmuh_liste_yetki.
DATA: gv_sistem_yon(1).



* selection-screen begin of block run with frame .
PARAMETERS :
             p_gl RADIOBUTTON GROUP  rad1 USER-COMMAND radio ,
             p_sa RADIOBUTTON GROUP  rad1  ,
             p_mu RADIOBUTTON GROUP  rad1  .

SELECTION-SCREEN  BEGIN OF BLOCK b10 WITH FRAME TITLE text-b01.
SELECTION-SCREEN  BEGIN OF BLOCK b11 WITH FRAME TITLE text-b11 .
PARAMETERS : p_bukrs LIKE t001-bukrs MODIF ID buk MEMORY ID buk
OBLIGATORY.


SELECT-OPTIONS :
             p_hkont FOR bsis-hkont MODIF ID fgl, "GLedger
             s_belnr FOR bsis-belnr NO-DISPLAY,
             p_kunnr FOR bsid-kunnr MODIF ID fmu, "Musteri
             p_lifnr FOR bsik-lifnr MODIF ID fsa, "Satici
             s_bldat FOR bsis-bldat OBLIGATORY  ,
*             s_prctr FOR bsis-prctr             ,
             s_gsber FOR bsis-gsber             ,
             s_stcd2 FOR kna1-stcd2             ,
             s_name3 FOR kna1-name3,
             s_ktokd FOR kna1-ktokd,
             s_ktokk FOR lfa1-ktokk.
*PARAMETERS : p_rldnr TYPE faglflexa-rldnr DEFAULT '0L'.
SELECTION-SCREEN  END OF BLOCK b11.



*selection-screen end of block run.
SELECTION-SCREEN BEGIN OF BLOCK type WITH FRAME TITLE text-026.
PARAMETERS:
            x_shbv AS CHECKBOX MODIF ID fb,
            x_apar AS CHECKBOX MODIF ID fb.
SELECTION-SCREEN END OF BLOCK type.

PARAMETERS :
             p_cls AS CHECKBOX,
             p_dvr AS CHECKBOX DEFAULT 'X',
***             ters kayıt checkbox eklentileri
             p_trs AS CHECKBOX DEFAULT ' ',
             p_top AS CHECKBOX DEFAULT 'X'.
PARAMETERS : p_upb RADIOBUTTON GROUP curr DEFAULT 'X' ,
             p_bpb RADIOBUTTON GROUP curr .
SELECTION-SCREEN  END OF BLOCK b10.
SELECTION-SCREEN PUSHBUTTON /1(12) gv_but10 USER-COMMAND but10.

SELECTION-SCREEN  BEGIN OF BLOCK bs1 WITH FRAME TITLE text-bs1 .
SELECT-OPTIONS :
             s_zuonr FOR bsak-zuonr MODIF ID fb1,
             s_blart FOR bsis-blart MODIF ID fb1,
             s_werks FOR bseg-werks MODIF ID fb3,
             s_kostl FOR bsis-kostl MODIF ID fb1,
             s_xref1 FOR bsak-xref1 MODIF ID fb2,
             s_xref2 FOR bsak-xref2 MODIF ID fb2,
             s_xref3 FOR bsak-xref3 MODIF ID fb1,
             s_usnam FOR bkpf-usnam MODIF ID fb1,
             s_aufnr FOR cobl-aufnr MODIF ID fb1,
             s_xref1h FOR bkpf-xref1_hd MODIF ID fb1,
             s_xref2h FOR bkpf-xref2_hd MODIF ID fb1.

SELECTION-SCREEN  END OF BLOCK bs1.
PARAMETERS: p_vari LIKE disvariant-variant DEFAULT '/PRODEA' NO-DISPLAY.
DATA : p_chk .

DATA : gv_flg10 ,gv_flt10(10) TYPE c VALUE 'Detay'.
CONSTANTS : c_repid1 LIKE sy-repid VALUE sy-repid.

*------------------------- INCLUDE ------------------------------------*
INCLUDE  zkshfigen_alvforms .
INCLUDE <icon>.
*include zkshfigen_autority.
