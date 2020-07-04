TABLES: t001,
        t005,
        sscrfields,
        bkpf,
        bset,
        kna1,
        lfa1.

DATA: itabbsd TYPE TABLE OF /saptr/vatbbsd
         WITH HEADER LINE
         INITIAL SIZE 0.

DATA: itabbsh TYPE TABLE OF /saptr/vatbbsh
         WITH HEADER LINE
         INITIAL SIZE 0.

DATA: itabbad TYPE TABLE OF /saptr/vatbbad
         WITH HEADER LINE
         INITIAL SIZE 0.

DATA: itabbah TYPE TABLE OF /saptr/vatbbah
         WITH HEADER LINE
         INITIAL SIZE 0.

DATA: itabbsd_o TYPE TABLE OF /saptr/vatbbsd
         WITH HEADER LINE
         INITIAL SIZE 0.

DATA: itabbsh_o TYPE TABLE OF /saptr/vatbbsh
         WITH HEADER LINE
         INITIAL SIZE 0.

DATA: itabbad_o TYPE TABLE OF /saptr/vatbbad
         WITH HEADER LINE
         INITIAL SIZE 0.

DATA: itabbah_o TYPE TABLE OF /saptr/vatbbah
         WITH HEADER LINE
         INITIAL SIZE 0.

TYPE-POOLS: icon.
TYPE-POOLS: slis.
TYPE-POOLS: kkblo.
TYPE-POOLS: ixml.

TYPES: BEGIN OF xml_line,
         data(256) TYPE x,
       END OF xml_line.

TYPES: BEGIN OF tybset,
         bukrs LIKE bset-bukrs,
         belnr LIKE bset-belnr,
         gjahr LIKE bset-gjahr,
         buzei LIKE bset-buzei,
         mwskz LIKE bset-mwskz,
         shkzg LIKE bset-shkzg,
         hwbas LIKE bset-hwbas,
         hwste LIKE bset-hwste,
         fwste like bset-fwste,"burcua add 090819
         stblg LIKE bkpf-stblg,
         stjah LIKE bkpf-stjah,
         name1 LIKE kna1-name1,
         name2 LIKE kna1-name2,
         land1 LIKE kna1-land1,
         stcd1 LIKE kna1-stcd1,
         stcd2 LIKE kna1-stcd2,
         stcd3 LIKE kna1-stcd3,
         stkzn LIKE kna1-stkzn,
         lifnr LIKE bseg-lifnr,
         kunnr LIKE bseg-kunnr,
         xblnr LIKE bkpf-xblnr,
         budat LIKE bkpf-budat,
         bldat LIKE bkpf-bldat,
         awtyp LIKE bkpf-awtyp,
       END OF tybset.

TYPES: BEGIN OF tybkpf,
         bukrs LIKE bset-bukrs,
         belnr LIKE bset-belnr,
         gjahr LIKE bset-gjahr,
       END OF tybkpf.

TYPES: BEGIN OF tybsid,
         bukrs LIKE bsid-bukrs,
         belnr LIKE bsid-belnr,
         gjahr LIKE bsid-gjahr,
         buzei like bsid-buzei,
         kunnr LIKE bsid-kunnr,
       END OF tybsid.

TYPES: BEGIN OF tybsik,
         bukrs LIKE bsik-bukrs,
         belnr LIKE bsik-belnr,
         gjahr LIKE bsik-gjahr,
         buzei like bsik-buzei,
         lifnr LIKE bsik-lifnr,
       END OF tybsik.

DATA: BEGIN OF mtab OCCURS 0,
        mess(132),
      END OF mtab.

DATA: ibkpf TYPE TABLE OF tybkpf,
      ibsid TYPE SORTED TABLE OF tybsid with unique key bukrs belnr gjahr buzei,
      ibsik TYPE SORTED TABLE OF tybsik with unique key bukrs belnr gjahr buzei,
      wbkpf TYPE tybkpf,
      wbsid TYPE tybsid,
      wbsik TYPE tybsik.

DATA: ibset TYPE SORTED TABLE OF tybset
            WITH UNIQUE KEY bukrs belnr gjahr buzei
            INITIAL SIZE 0,
      ebset TYPE TABLE OF tybset,
      wbset TYPE tybset.

DATA: it007a TYPE TABLE OF t007a WITH HEADER LINE.

DATA: sktosl TYPE /saptr/sktosl,
      fzcust TYPE /saptr/fzcust,
      fzvend TYPE /saptr/fzvend,
      g_s_log TYPE bal_s_log,
      g_sscr_ucomm TYPE sscrfields-ucomm,
      ok-code(5) TYPE c.
DATA: fvalue LIKE ftpost-fval,
      default_tabname_master TYPE kkblo_tabname,
      default_tabname_slave TYPE kkblo_tabname,
      it_keyinfo      TYPE kkblo_keyinfo,
      it_sp_groups    TYPE kkblo_t_sp_group     WITH HEADER LINE,
      it_sort         TYPE kkblo_t_sortinfo     WITH HEADER LINE,
      it_extab        TYPE kkblo_t_extab        WITH HEADER LINE,
      it_filter       TYPE kkblo_t_filter,
      xline           LIKE sy-tabix.

DATA: gs_layout TYPE slis_layout_alv,
      g_exit_caused_by_caller,
      gs_exit_caused_by_user TYPE slis_exit_by_user,
*{   ->>> Commented by Prodea Ozan Şahin - 07.08.2019 15:52:48
*      g_incld LIKE sy-repid VALUE '/SAPTR/FI_I_KDV_B_RAPORU',
*      g_repid LIKE sy-repid VALUE '/SAPTR/FI_R_KDV_B_RAPORU'.
*}     <<<- End of  Commented - 07.08.2019 15:52:48
*{   ->>> Added by Prodea Ozan Şahin - 07.08.2019 15:53:02
      g_incld LIKE sy-repid VALUE 'ZSAPTR_FI_I_KDV_B_RAPORU',
      g_repid LIKE sy-repid VALUE 'ZSAPTR_FI_R_KDV_B_RAPORU'.
*     }  	 <<<- End of  Added - 07.08.2019 15:53:02

DATA: gt_list_top_of_page TYPE slis_t_listheader.

DATA: gs_selfield TYPE slis_selfield.

DATA: color TYPE kkblo_t_specialcol WITH HEADER LINE.

DATA: gs_variant LIKE disvariant.

DATA: BEGIN OF gt_alv OCCURS 0,
        itabbsd LIKE itabbsd OCCURS 0,
        itabbsh LIKE itabbsh OCCURS 0,
        itabbad LIKE itabbad OCCURS 0,
        itabbah LIKE itabbah OCCURS 0,
        itabbsd_o LIKE itabbsd_o OCCURS 0,
        itabbsh_o LIKE itabbsh_o OCCURS 0,
        itabbad_o LIKE itabbad_o OCCURS 0,
        itabbah_o LIKE itabbah_o OCCURS 0,
      END OF gt_alv.

DATA: flg_print_header(1) TYPE c VALUE 'X',
      g_refresh_round TYPE c,
      flg_summ        TYPE c,
      strxml TYPE xml_line,
      flg_statistikdruck TYPE slis_print_alv,
      lt_fieldcat TYPE slis_fieldcat_alv,
      gt_excluding TYPE slis_t_extab,
      flg_notp TYPE c.

DATA: var_avp1 LIKE rfums_alv-conlis,
      var_avp2 LIKE rfums_alv-conlis,
      var_avp3 LIKE rfums_alv-conlis,
      var_avp4 LIKE rfums_alv-conlis,
      var_avp5 LIKE rfums_alv-conlis,
      var_avp6 LIKE rfums_alv-conlis,
      var_avp7 LIKE rfums_alv-conlis,
      var_avp8 LIKE rfums_alv-conlis.

CONSTANTS: c_bsd  TYPE slis_handl VALUE 'BSD',
           c_bsh  TYPE slis_handl VALUE 'BSH',
           c_bad  TYPE slis_handl VALUE 'BAD',
           c_bah  TYPE slis_handl VALUE 'BAH',
           c_bsdo  TYPE slis_handl VALUE 'BSDO',
           c_bsho  TYPE slis_handl VALUE 'BSHO',
           c_bado  TYPE slis_handl VALUE 'BADO',
           c_baho  TYPE slis_handl VALUE 'BAHO'.


FIELD-SYMBOLS <gt_alv> LIKE LINE OF gt_alv.
DATA : wt_alv LIKE LINE OF gt_alv.

* BAdI Interface
* --------------
DATA: exit TYPE REF TO /saptr/if_ex_vat_b.

TYPES: BEGIN OF /saptr/rbukrs,
         sign(1),
         option(2),
         low  TYPE bukrs,
         high TYPE bukrs,
       END OF /saptr/rbukrs.

TYPES: BEGIN OF /saptr/rmonat,
         sign(1),
         option(2),
         low  TYPE monat,
         high TYPE monat,
       END OF /saptr/rmonat.

TYPES: BEGIN OF /saptr/rbudat,
         sign(1),
         option(2),
         low  TYPE budat,
         high TYPE budat,
       END OF /saptr/rbudat.

TYPES: BEGIN OF /saptr/rbldat,
         sign(1),
         option(2),
         low  TYPE monat,
         high TYPE monat,
       END OF /saptr/rbldat.

TYPES: BEGIN OF /saptr/rgjahr,
         sign(1),
         option(2),
         low  TYPE gjahr,
         high TYPE gjahr,
       END OF /saptr/rgjahr.

TYPES:
  /saptr/bukrs TYPE TABLE OF /saptr/rbukrs.
TYPES:
  /saptr/monat TYPE TABLE OF /saptr/rmonat.
TYPES:
  /saptr/budat TYPE TABLE OF /saptr/rbudat.
TYPES:
  /saptr/bldat TYPE TABLE OF /saptr/rbldat.
TYPES:
  /saptr/gjahr TYPE TABLE OF /saptr/rgjahr.
