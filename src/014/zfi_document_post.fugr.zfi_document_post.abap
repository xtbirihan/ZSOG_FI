FUNCTION ZFI_DOCUMENT_POST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(BKPF) TYPE  BKPF
*"     REFERENCE(ONKAYIT) TYPE  CHAR1 DEFAULT ''
*"     REFERENCE(COMMIT) TYPE  CHAR1 DEFAULT 'X'
*"     REFERENCE(LAND1) TYPE  LAND1 DEFAULT 'TR'
*"     REFERENCE(VERGI_HESAP_TIPI) TYPE  CHAR1 DEFAULT '1'
*"  EXPORTING
*"     REFERENCE(E_TYPE) TYPE  AWTYP
*"     REFERENCE(E_KEY) TYPE  AWKEY
*"     REFERENCE(E_SYS) TYPE  AWSYS
*"  TABLES
*"      BSEG STRUCTURE  BSEG
*"      CRITERIA STRUCTURE  BAPIACKEC9 OPTIONAL
*"      VALUEFIELD STRUCTURE  BAPIACKEV9 OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------



PERFORM clear_data.

gv_auto_commit = commit .
s_bkpf   = bkpf.
t_bseg[] = bseg[].
it_criteria[] = criteria[].
it_valuefield[] = valuefield[].
lv_land1 = land1.
gv_onkayit = onkayit .
gv_vergi_hesap_tipi = vergi_hesap_tipi .

PERFORM fill_header.
PERFORM fill_item.
PERFORM call_bapi.

return[] = it_return[].
e_type = l_type.
e_key  = l_key.
e_sys  = l_sys.


ENDFUNCTION.
