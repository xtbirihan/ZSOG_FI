*&---------------------------------------------------------------------*
*& Report  ZFI_PESIN_ODENEN_GIDERLER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zfi_pesin_odenen_giderler.

INCLUDE zalv_global .
INCLUDE zfi_i_pesin_odenen_gid_top .
INCLUDE zfi_i_pesin_odenen_gid_pbo .
INCLUDE zfi_i_pesin_odenen_gid_pai .
INCLUDE zfi_i_pesin_odenen_gid_frm .

START-OF-SELECTION .

  SELECT saknr txt20
    FROM skat
    INTO TABLE it_skat
    WHERE spras EQ sy-langu AND
          ktopl EQ 'SGHP' AND
          ( saknr LIKE '180%' OR
            saknr LIKE '280%' OR
* begin of insert Ali Y. Abbasgil 10.06.2013 15:53:43
            saknr LIKE '360%' OR
*    end of insert.
            saknr LIKE '191%' ).
  SORT it_skat BY saknr .

  CLEAR : it_style_kapali , wa_style .
  wa_style-fieldname = '' .
  wa_style-style = cl_gui_alv_grid=>mc_style_disabled."Kapat
  APPEND wa_style TO it_style_kapali .

  bkpf-blart = 'KR' .
  bkpf-bukrs = '2425' .

  CALL SCREEN 0100 .

END-OF-SELECTION .
