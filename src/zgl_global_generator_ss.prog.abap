*&---------------------------------------------------------------------*
*& Include          ZDM_GLOBAL_GENERATOR_SS
*&---------------------------------------------------------------------*

*SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
*PARAMETERS: p_rad3 RADIOBUTTON GROUP rad2 DEFAULT 'X',
*            p_rad4 RADIOBUTTON GROUP rad2.
*SELECTION-SCREEN END OF BLOCK b3.
*
*
*SELECTION-SCREEN BEGIN OF BLOCK b1  WITH FRAME TITLE text-001.
*PARAMETERS: p_bukrs TYPE bkpf-bukrs OBLIGATORY.
****SELECT-OPTIONS: p_bukrs FOR bkpf-bukrs.
*SELECT-OPTIONS: p_fy FOR bkpf-GJAHR OBLIGATORY NO INTERVALS NO-EXTENSION.
*SELECT-OPTIONS: p_date FOR bkpf-budat OBLIGATORY.
****SELECT-OPTIONS: p_date FOR bkpf-budat.
*SELECT-OPTIONS: p_belnr FOR  bkpf-belnr.
**SELECT-OPTIONS: p_bupla FOR  ygstoutward01-bupla.
*SELECTION-SCREEN END OF BLOCK b1.
*
*
*IF sy-subrc <> 0.
** Implement a suitable exception handling here
*ENDIF.
TABLES :   vbrp.
data : v_file TYPE string,
       ld_header type xstring.
parameter :  path TYPE rlgrap-filename .

v_file = path.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS : p_rad3 RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND ucomm,
             p_rad4 RADIOBUTTON GROUP rad1,
             p_rad1 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS : s_date  FOR sy-datum NO-EXTENSION OBLIGATORY.
SELECT-OPTIONS : s_bukrs FOR vbrk-bukrs .
SELECT-OPTIONS : s_bupla FOR vbrk-bupla  ."NO-EXTENSION. "OBLIGATORY.
SELECT-OPTIONS : s_plant FOR vbrp-werks.
*SELECT-OPTIONS : s_gjahr FOR vbrk-gjahr.
SELECT-OPTIONS : s_type  FOR vbrk-fkart.
SELECT-OPTIONS : s_vbeln FOR vbrk-vbeln." no INTERVALS.
**parameter : s_vebln TYPE vbrk-vbeln.
*SELECT-OPTIONS : s_werks FOR vbrp-werks.
SELECTION-SCREEN END OF BLOCK b2.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR path .
  CALL FUNCTION 'F4_FILENAME'
   EXPORTING
     PROGRAM_NAME        = SYST-CPROG
     DYNPRO_NUMBER       = SYST-DYNNR
     FIELD_NAME          = 'path'
   IMPORTING
     FILE_NAME           = path.
