*&---------------------------------------------------------------------*
*& Include          ZDM_INWARD_GENERATOR_SS
*&---------------------------------------------------------------------*
TABLES  : vbrk .

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_rad1 RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_rad3 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b1  WITH FRAME TITLE text-001.
*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS : s_date  FOR sy-datum NO-EXTENSION OBLIGATORY.
SELECT-OPTIONS : s_bukrs FOR vbrk-bukrs .
SELECT-OPTIONS : s_bupla FOR vbrk-bupla .
*SELECT-OPTIONS : s_plant FOR vbrp-werks.
SELECT-OPTIONS : s_gjahr FOR vbrk-gjahr.
*SELECT-OPTIONS : s_type  FOR vbrk-fkart.
*SELECT-OPTIONS : s_vbeln FOR vbrk-vbeln." no INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.
