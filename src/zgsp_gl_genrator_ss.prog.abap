*&---------------------------------------------------------------------*
*& Include          ZGSP_GL_GENRATOR_SS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZGSP_GL_GENRATOR_SS
*&---------------------------------------------------------------------*
*TABLES:BKPF, BSEG.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_rad1 RADIOBUTTON GROUP rad2 DEFAULT 'X',
            p_rad2 RADIOBUTTON GROUP rad2.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b1  WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS s_date FOR bkpf-budat.
SELECT-OPTIONS s_bukrs FOR bkpf-bukrs.
SELECT-OPTIONS s_bupla FOR bseg-bupla.
SELECT-OPTIONS s_gjahr FOR bkpf-gjahr.
*SELECT-OPTIONS S_BELNR FOR BKPF-BELNR.
SELECTION-SCREEN END OF BLOCK b1.
