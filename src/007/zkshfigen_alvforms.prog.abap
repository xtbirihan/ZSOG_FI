*&---------------------------------------------------------------------*
*&  Include           ZKSHFIGEN_ALVFORMS
*&---------------------------------------------------------------------*
type-pools: slis,
            kkblo.
data: alv_fieldcat  type slis_t_fieldcat_alv        with header line,
      alv_events    type slis_t_event               with header line,
      alv_sort      type slis_sortinfo_alv occurs 0 with header line.

data: alv_event_exit       type slis_t_event_exit                   ,
      alv_tabname_header   type slis_tabname                        ,
      alv_tabname          type slis_tabname                        ,
      alv_repid            like sy-repid                            ,
      alv_variant          like disvariant                          ,
      alv_list_top_of_page type slis_t_listheader                   ,
      alv_layout           type slis_layout_alv                     ,
      alv_sort_1           type slis_sortinfo_alv-fieldname         ,
      alv_colors           type kkblo_t_specialcol with header line ,
      gs_private           type slis_data_caller_exit               ,
      gs_selfield          type slis_selfield                       ,
      gs_grid              type lvc_title                           ,
      alv_logo(40) ,
      alv_info type slis_listheader-info  .
*- Varyant ...
data  : h_variant        like disvariant,
        def_variante    like disvariant,
        variant_exit(1) type c,
        variant_save(1) type c,
        variant_def(1)  type c.
field-symbols : <vout> type table ,
                <fcat> type table ,
                <oth>  type table .
field-symbols : <layout> type any.
data : begin of alv_list occurs 0,
         typ(1)  type c,
         key(20) type c,
         info    type slis_entry,
       end of alv_list.
define alv_list.
  alv_list-typ  = &1.
  alv_list-key  = &2.
  alv_list-info = &3.
  append alv_list.
end-of-definition.
*&---------------------------------------------------------------------*
*&      Form  alv_fieldcat_merge
*&---------------------------------------------------------------------*
form alv_fieldcat_merge using fcatname tabname .
  FIELD-SYMBOLS <fs_cat> type any.
  FIELD-SYMBOLS <fs_fieldname> TYPE any.
  FIELD-SYMBOLS <fs_noout> TYPE any.
  if alv_repid is initial.
    alv_repid = sy-repid .
  endif.
  alv_tabname = tabname .
  assign (fcatname) to <fcat> .
  refresh <fcat> .
  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name         = alv_repid
      i_internal_tabname     = alv_tabname
      i_inclname             = alv_repid
      i_client_never_display = 'X'
      i_bypassing_buffer     = 'X'
    changing
      ct_fieldcat            = <fcat>
    exceptions
      others                 = 3.

****** xdpolat 23.10.2019 da yazıldı.
  LOOP AT <fcat> ASSIGNING <fs_cat>.
   ASSIGN COMPONENT 'FIELDNAME' OF STRUCTURE <fs_cat> TO <fs_fieldname>.
   ASSIGN COMPONENT 'NO_OUT' OF STRUCTURE <fs_cat> TO <fs_noout>.
    CASE <fs_fieldname> .
      WHEN 'HKONT'.
        <fs_noout> = 'X'.
    ENDCASE.
  ENDLOOP.
****** xdpolat 23.10.2019 da yazıldı.

endform.                    " alv_fieldcat_merge
*&---------------------------------------------------------------------*
*&      Form  alv_set_events
*&---------------------------------------------------------------------*
form alv_get_events using  eventtab.
  assign (eventtab) to <oth> .
  refresh <oth> .
* Get All List Events
  call function 'REUSE_ALV_EVENTS_GET'
    exporting
      i_list_type     = 0
    importing
      et_events       = alv_events[]
    exceptions
      list_type_wrong = 1
      others          = 2.

  if sy-subrc ne 0.     exit.   endif.

*  READ TABLE alv_events WITH KEY name =  slis_ev_top_of_list.
*  IF sy-subrc = 0.
*    MOVE 'TOP_OF_LIST' TO alv_events-form.
*    MODIFY alv_events INDEX sy-tabix .
*  ENDIF.



*  READ TABLE alv_events WITH KEY name =  slis_ev_end_of_list.
*  IF sy-subrc = 0.
*    MOVE 'END_OF_LIST' TO alv_events-form.
*    MODIFY alv_events INDEX sy-tabix.
*  ENDIF.
endform.                    " alv_set_events
*&--------------------------------------------------------------------*
*&      Form  alv_set_event
*&--------------------------------------------------------------------*
form alv_set_event using    p_event.
*   Set Event Form Name
  alv_events-form = p_event.
  modify alv_events transporting form where name = p_event.
endform.                    " alv_set_event
*&---------------------------------------------------------------------*
*&      Form  initialization_for_alv
*&---------------------------------------------------------------------*
form initialization_for_alv using text .
  data:lt_rtab like rtab occurs 1 with header line.

  alv_repid   = sy-repid  .
*
  data: lv_hesap1(25)  ,
        lv_hesap2(25)  ,
        lv_hesap3(25)  ,
        lv_secim1(25)  ,
        lv_secim2(25)  ,
        lv_secim3(25)  ,
        lv_kayit1(25)  ,
        lv_kayit2(25)  ,
        lv_kayit3(25)  ,
        lv_is1(20)     ,
        lv_is2(20)     ,
        lv_is3(20)     .


  lt_rtab[] = rtab[].

  delete lt_rtab where tabix = 'DEVİR'.

  clear: lt_rtab.
  sort lt_rtab by hkont ascending.
  read table lt_rtab index 1.
  lv_hesap1 = lt_rtab-hkont.

  clear: lt_rtab.
  sort lt_rtab by hkont descending.
  read table lt_rtab index 1.
  lv_hesap2 = lt_rtab-hkont.

  concatenate lv_hesap1 '-' lv_hesap2 into lv_hesap3.


  clear: lt_rtab.
  sort lt_rtab by budat ascending.
  read table lt_rtab index 1.
  lv_kayit1 = lt_rtab-budat.

  clear: lt_rtab.
  sort lt_rtab by budat descending.
  read table lt_rtab index 1.
  lv_kayit2 = lt_rtab-budat.
  concatenate lv_kayit1+6(2) '.' lv_kayit1+4(2) '.' lv_kayit1+0(4) '-'
              lv_kayit2+6(2) '.' lv_kayit2+4(2) '.' lv_kayit2+0(4)
         into lv_kayit3.

  delete lt_rtab where gsber = ''.
  clear: lt_rtab.
  sort lt_rtab by gsber ascending.
  read table lt_rtab index 1.
  lv_is1 = lt_rtab-gsber.

  clear: lt_rtab.
  sort lt_rtab by gsber descending.
  read table lt_rtab index 1.
  lv_is2 = lt_rtab-gsber.

  concatenate lv_is1 '-' lv_is2 into lv_is3.

  alv_list 'S'  'Hesap Kodu    ' lv_hesap3       .
  if not p_hkont[] is initial.
    read table p_hkont index 1.
    if sy-subrc = 0.
      if p_hkont-sign = 'I'.
        if p_hkont-low eq ''.
          p_hkont-low = '0000000000'.
        elseif p_hkont-high eq ''.
          p_hkont-high = '9999999999'.
        endif.
        modify p_hkont index 1.
      endif.
      lv_secim1 = p_hkont-low.
      lv_secim2 = p_hkont-high.
      concatenate lv_secim1 '-' lv_secim2 into lv_secim3.
      alv_list 'S'  'Seçim Aralığı ' lv_secim3       .
    endif.
  endif.

  alv_list 'S'  'Kayıt Tarihi  ' lv_kayit3       .
  alv_list 'S'  'İş Alanı      ' lv_is3          .
  alv_layout-colwidth_optimize  = 'X'    .
  alv_layout-numc_sum           = 'X'    .
  alv_layout-no_subchoice       = 'X'    .
  alv_layout-zebra              = 'X'    .

endform.                    " initialization_for_alv
*&---------------------------------------------------------------------*
*&      Form  LIST_SET_ATTRIBUTE
*&---------------------------------------------------------------------*
form list_set_attribute tables   ti_fcat   structure alv_fieldcat
                        using    i_tabname type      slis_tabname
                                 i_fieldname
                                 i_target
                                 i_value.
  data: li_field  type standard table of slis_fieldname
                  with header line,
        li_target type standard table of slis_fieldname
                  with header line.
  data tabix like sy-tabix.
  field-symbols: <f1>.

  split i_fieldname at '/' into table li_field.
  split i_target    at '/' into table li_target.
  loop at li_field.
    read table ti_fcat with key tabname   = i_tabname
                                fieldname = li_field.
    tabix = sy-tabix.
    if sy-subrc = 0.
      loop at li_target.
        assign component li_target of structure ti_fcat to <f1>.
        <f1> = i_value.
        modify ti_fcat index tabix.
      endloop.
    endif.
  endloop.
endform.                               " LIST_SET_ATTRIBUTE

*&---------------------------------------------------------------------
*&      Form  top_of_page
*----------------------------------------------------------------------
form top_of_page .


****************************************************
  data: lv_pageno(5) type c..
  if sy-pagno = 0 .
    if sy-ucomm = 'PRIN'.
*      IF g_start_of_list = ' '.
*        g_totpages = g_totpages + 1.
*      ENDIF.


      clear: lv_pageno.
      move sy-pagno to lv_pageno.
*    lv_pageno = 1.
      condense lv_pageno.
      alv_list 'S'  'Sayfa No   ' lv_pageno       .
    else.
      clear: alv_list.
      read table alv_list with key key = 'Sayfa No   '.
      if sy-subrc = 4.
        move sy-pagno to lv_pageno.
        lv_pageno = 1.
        condense lv_pageno.
        alv_list 'S'  'Sayfa No   ' lv_pageno       .
      endif.
    endif.
  else.

*    IF g_start_of_list = ' '.
*      g_totpages = g_totpages + 1.
*    ENDIF.

    clear: alv_list.
    read table alv_list with key key = 'Sayfa No   '.
    if sy-subrc = 0.
      clear: lv_pageno.
      move sy-pagno to lv_pageno.
      lv_pageno = lv_pageno .
      condense lv_pageno.
      alv_list-info = lv_pageno.
      modify alv_list index sy-tabix.
    endif.
  endif.



  call function 'REUSE_ALV_COMMENTARY_WRITE'
  exporting
  i_logo             = alv_logo
  it_list_commentary = alv_list[].

endform.                    " top_of_page
*&---------------------------------------------------------------------
*&      Form  end_of_page
*----------------------------------------------------------------------
form end_of_page.
  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      i_logo             = alv_logo
      it_list_commentary = alv_list[].
endform.                    " END_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  FILL_T_SORT
*&---------------------------------------------------------------------*
form fill_t_sort tables  sorttab  structure alv_sort
                 using   tabname fieldname up down subtot .
  clear sorttab .
  move : tabname   to sorttab-tabname   ,
         fieldname to sorttab-fieldname ,
         up        to sorttab-up        ,
         down      to sorttab-down      ,
         subtot    to sorttab-subtot    .
  append sorttab .
endform.                               " FILL_T_SORT

*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_VARIANT_F4
*&---------------------------------------------------------------------*
form reuse_alv_variant_f4 using    pvari.
  data: es_variant like h_variant ,
        exit.

  h_variant-report     = sy-repid.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant    = h_variant
      i_save        = 'A'
    importing
      e_exit        = exit
      es_variant    = es_variant
    exceptions
      not_found     = 1
      program_error = 2
      others        = 3.
  if sy-subrc <> 2.
    if exit = space.
      pvari = es_variant-variant.
    endif.
  endif.
endform.                               " REUSE_ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_VARIANT_F4
*&---------------------------------------------------------------------*
form reuse_alv_variant_f42 using    p_vari
                                    p_variant like h_variant  .

  data: es_variant like h_variant ,
        exit.
  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant    = p_variant
      i_save        = 'A'
    importing
      e_exit        = exit
      es_variant    = es_variant
    exceptions
      not_found     = 1
      program_error = 2
      others        = 3.
  if sy-subrc <> 2.
    if exit = space.
      p_vari = es_variant-variant.
    endif.
  endif.
endform.                               " REUSE_ALV_VARIANT_F4
*&--------------------------------------------------------------------*
*&      Form  REUSE_ALV_VARIANT_F4
*&---------------------------------------------------------------------*
form reuse_alv_variant_sec using    p_repid
                           changing cp_vari
                                    cp_variant like h_variant  .

  data: es_variant like h_variant ,
        exit.
  cp_variant-report = p_repid.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant    = cp_variant
      i_save        = 'A'
    importing
      e_exit        = exit
      es_variant    = es_variant
    exceptions
      not_found     = 1
      program_error = 2
      others        = 3.
  if sy-subrc <> 2.
    if exit = space.
      cp_vari = cp_variant-variant = es_variant-variant.
    endif.
  endif.
endform.                               " REUSE_ALV_VARIANT_F4

**  At selection Screen Eventinin alt#nda
*AT SELECTION-SCREEN .
*  PERFORM  AT_SELSCREEN_VARYANT    .
*&---------------------------------------------------------------------*
*&      Form  AT_SELSCREEN_VARYANT
*&---------------------------------------------------------------------*
form at_selscreen_varyant .
*   P_vari parametre ismi olacak
  if not p_vari is initial.
    if h_variant is initial.
      h_variant-report = sy-repid.
    endif.

    move h_variant to def_variante.
    move p_vari to def_variante-variant.
    call function 'REUSE_ALV_VARIANT_EXISTENCE'
      exporting
        i_save     = variant_save
      changing
        cs_variant = def_variante.
    h_variant = def_variante.
  else.
    clear h_variant.
    h_variant-report = alv_repid.
  endif.

endform.                    " AT_SELSCREEN_VARYANT
**&---------------------------------------------------------------------
**
**&      Form  FILL_COLOR_COLUMNS
**&---------------------------------------------------------------------
**
*FORM FILL_COLOR_COLUMNS  USING  FIELDNAME  P_COLOR .
** alv de her field a farkl# bir renk vermek için kullan#l#r
**   Kullanabilmek için
**   data : color   like alv_colors occurs 0,
**   alv yard#m#yla gösterilecek internal tablo içine tan#mlanmal#d#r
**   alv_layout-coltab_fieldname   = 'COLOR'. alan# doldurulmal#d#r
**   amac# alv ye hangi alan#n renk alan# oldu#unu göstermektir
*
**  clear alv_colors-color.
**  MOVE: FIELDNAME TO alv_COLORs-FIELDNAME,
**        P_COLOR   TO alv_COLORs-COLOR-COL,
**        'X'       TO alv_colors-NOKEYCOL .
**  APPEND alv_COLORs TO temp-COLOR.
*ENDFORM.                               " FILL_COLOR_COLUMNS
*&---------------------------------------------------------------------*
*&      Form  at_user_command
*&      E#er alv de kendi menünüz kullanacaksan#z
*&      user command i#lemlerini burada de#erlendirirsiniz
*&---------------------------------------------------------------------*
*FORM user_command  USING p_ucomm
*                   p_f TYPE slis_selfield.
*  CASE p_ucomm.
*  ENDCASE.
*  p_f-refresh = 'X'.
*ENDFORM.
**---------------------------------------------------------------------*
**       FORM NETPR_PF_STATUS_SET                                      *
**       Alv'ye sizin olu#turdu#unuz menü ba#lan#r
**---------------------------------------------------------------------*
*FORM PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
*  SET PF-STATUS 'ALV_GUI'.
***MG
*ENDFORM.

**** Kullan#c#n#n alvde daha önceden kulland### bir layout ile program#
**** görebilmesi için kullan#lacak yap#
**** bir parametre tan#m# yar#m#yla kullan#c#dan layout seçmesi istenir
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI .
* PERFORM REUSE_ALV_VARIANT_F4 USING P_VARI .
*
**&---------------------------------------------------------------------
**
**&      Form  alv_lsa
**&---------------------------------------------------------------------
**
*FORM alv_lsa TABLES   fieldcat STRUCTURE alv_fieldcat
*             USING    p_fieldname TYPE slis_fieldname
*                      p_target
*                      p_value.
*  FIELD-SYMBOLS: <s>.
*
*  READ TABLE fieldcat WITH KEY tabname    = alv_tabname
*                                   fieldname  = p_fieldname.
*  IF sy-subrc = 0.
*    ASSIGN COMPONENT p_target OF STRUCTURE fieldcat TO <s>.
*    <s> = p_value.
*    MODIFY fieldcat INDEX sy-tabix.
*  ENDIF.
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  initialization_for_alv_MANUEL
*&---------------------------------------------------------------------*
form initialization_for_alv_manuel using kdate like sy-datum
                                         ptext .
  clear :alv_list[],alv_list.
  data date(10).
  write kdate to date dd/mm/yyyy .
*
  alv_list 'H'  'DGR.' ptext.
  alv_list 'S'  'Kullanıcı Adı' sy-uname       .
  alv_list 'S'  'Rapor Tarihi       ' date  .
*
  alv_layout-colwidth_optimize  = 'X'    .
  alv_layout-numc_sum           = 'X'    .
  alv_layout-no_subchoice       = 'X'    .
  alv_layout-zebra               = 'X'    .


  alv_layout-info_fieldname = 'COLOR'.

endform.                    " initialization_for_alv_MANUEL
*&---------------------------------------------------------------------*
*&      Form  GET_ALV_VARIANT
*&---------------------------------------------------------------------*
form get_alv_variant  using    pvari     like disvariant-variant
                               prepid    like sy-repid
                      changing palvrepid
                               pvariant  like disvariant
                               pdefvar like disvariant
                               pvarsave .
  palvrepid =  pvariant-report = prepid.

  if not pvari is initial.
    if pvariant is initial.
      pvariant-report = prepid.
    endif.
    move pvariant to pdefvar.
    move pvari   to pdefvar-variant.
    call function 'REUSE_ALV_VARIANT_EXISTENCE'
      exporting
        i_save     = pvarsave
      changing
        cs_variant = pdefvar.

    pvariant = pdefvar.
    palvrepid       = prepid .

  else.
    clear pvariant.
    palvrepid =  pvariant-report = prepid.
  endif.


endform.                    " GET_ALV_VARIANT
*&---------------------------------------------------------------------*
*&      Form  HEADER_FOR_ALV
*&---------------------------------------------------------------------*
form header_for_alv using text .
  ranges: r_hkont1   for p_hkont,
          r_kunnr1   for p_kunnr,
          r_lifnr1   for p_lifnr,
          r_hkont2  for p_hkont,
          r_kunnr2  for p_kunnr,
          r_lifnr2  for p_lifnr,
          r_hkont3  for p_hkont,
          r_kunnr3  for p_kunnr,
          r_lifnr3  for p_lifnr.
  data:date(20).
  data lv_info  like alv_list-info .
  data lv_infot like alv_list-info .

  data lv_line1 type i.
  data lv_line2 type i.
  data lv_line3 type i.
  data lv_line4 type i.
  data lv_line5 type i.
  data: lv_kt(30),
        lv_dk(10).

  concatenate ' / '
              sy-uzeit(2) ':'
              sy-uzeit+2(2)
         into lv_dk.


  concatenate s_bldat-low+6(2) '.'
              s_bldat-low+4(2) '.'
              s_bldat-low(4) '-'
              s_bldat-high+6(2) '.'
              s_bldat-high+4(2) '.'
              s_bldat-high(4)
              into lv_kt.
  describe table p_hkont lines lv_line1.
  describe table p_kunnr lines lv_line2.
  describe table p_lifnr lines lv_line3.
  describe table s_bldat lines lv_line4.
  describe table s_gsber lines lv_line5.

  write sy-datum to date dd/mm/yyyy .
  concatenate date lv_dk into date.

  alv_list 'H'  'DGR.' text                      .
  alv_list 'S'  'Kullanıcı Adı  ' sy-uname       .
  alv_list 'S'  'Tarih / Saat   ' date           .
  alv_list 'S'  'Seçim Tarih Aralığı' lv_kt.
  clear: r_hkont1,
         r_lifnr1,
         r_kunnr1,
         r_hkont2,
         r_lifnr2,
         r_kunnr2.
  read table p_hkont into r_hkont1 with key sign = 'E'.
  read table p_lifnr into r_lifnr1 with key sign = 'E'.
  read table p_kunnr into r_kunnr1 with key sign = 'E'.
  read table p_hkont into r_hkont2 with key option = 'NE'.
  read table p_lifnr into r_lifnr2 with key option = 'NE'.
  read table p_kunnr into r_kunnr2 with key option = 'NE'.
  read table p_hkont into r_hkont3 with key option = 'NB'.
  read table p_lifnr into r_lifnr3 with key option = 'NB'.
  read table p_kunnr into r_kunnr3 with key option = 'NB'.

  concatenate alv_info 'Muavin Defter - Çoklu Seçim'
  into lv_info separated by space .

  if     r_hkont1-sign = 'E'.
    alv_list 'H'  ''   lv_info .
  elseif r_lifnr1-sign = 'E'.
    alv_list 'H'  ''   lv_info .
  elseif r_kunnr1-sign = 'E'.
    alv_list 'H'  ''   lv_info .
  elseif r_hkont2-option = 'NE'.
    alv_list 'H'  ''   lv_info .
  elseif r_lifnr2-option = 'NE'.
    alv_list 'H'  ''  lv_info .
  elseif r_kunnr2-option = 'NE'.
    alv_list 'H'  ''   lv_info .
  elseif r_hkont3-option = 'NB'.
    alv_list 'H'  ''   lv_info .
  elseif r_lifnr3-option = 'NB'.
    alv_list 'H'  ''   lv_info .
  elseif r_kunnr3-option = 'NB'.
    alv_list 'H'  ''   lv_info .
  else.
    if  lv_line1 >= 2 or
        lv_line2 >= 2 or
        lv_line3 >= 2 or
        lv_line4 >= 2 or
        lv_line5 >= 2 .

      alv_list 'H'  ''   lv_info .
    else.
      concatenate alv_info 'Muavin Defter' into lv_info
      separated by space .
      alv_list 'H'  ''   lv_info .
    endif.
  endif.
endform.                    " HEADER_FOR_ALV
