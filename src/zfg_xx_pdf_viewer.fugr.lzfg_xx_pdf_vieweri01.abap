*----------------------------------------------------------------------*
***INCLUDE LZFG_XX_PDF_VIEWERI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pai_0100 input.
  if sy-ucomm = 'BACK'
    or sy-ucomm = 'EXIT'
    or sy-ucomm = 'CANC'.

    clear sy-ucomm.
    leave to screen 0.

  endif.

  clear sy-ucomm.
endmodule.
