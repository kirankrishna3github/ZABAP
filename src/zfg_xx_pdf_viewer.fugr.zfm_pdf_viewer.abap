function zfm_pdf_viewer.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PDF_BINARY) TYPE  XSTRING
*"----------------------------------------------------------------------
clear gv_pdf.
if iv_pdf_binary is not initial.
  gv_pdf = iv_pdf_binary.
endif.

call screen '0100'.
endfunction.
